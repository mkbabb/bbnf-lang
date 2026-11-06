use std::collections::BTreeSet;
use std::fs::{File, OpenOptions, TryLockError};
use std::io::{Read as _, Write as _};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use super::write::{ArtifactPublication, Generation, PublicationReceipt, failpoint};
use crate::architecture::schema::CanonicalText;
use crate::architecture::schema::Sha256Digest;

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
struct CurrentPointer {
    schema: CanonicalText,
    run_id: CanonicalText,
    publication_size: u64,
    publication_hash: Sha256Digest,
}

pub(super) fn atomic_publish(
    output: &Path,
    build: impl FnOnce(&Path) -> Result<Generation>,
) -> Result<Generation> {
    std::fs::create_dir_all(output)
        .with_context(|| format!("create evidence publication root `{}`", output.display()))?;
    let generations = output.join("generations");
    std::fs::create_dir_all(&generations)?;
    let counter = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let stage = generations.join(format!(".stage-{}-{counter}", std::process::id()));
    std::fs::create_dir(&stage)?;
    let mut stage_guard = Cleanup::directory(stage.clone());
    let generation = build(&stage)?;
    failpoint("staging-directory-fsync")?;
    File::open(&stage)?.sync_all()?;

    failpoint("publication-lock")?;
    let lock_path = output.join(".publication.lock");
    let _lock = LockGuard::acquire(&lock_path)?;
    let final_dir = generations.join(generation.run_id.as_str());
    let reuse = if final_dir.exists() {
        failpoint("existing-generation-verify")?;
        verify_generation(&final_dir, &generation).is_ok()
    } else {
        false
    };
    let previous_current = verified_current_bytes(output);
    let mut installation = None;
    if reuse {
        stage_guard.cleanup()?;
    } else {
        failpoint("generation-rename")?;
        if final_dir.exists() {
            let quarantine_root = output.join("quarantine");
            std::fs::create_dir_all(&quarantine_root)?;
            let path = quarantine_root.join(format!(
                "{}-{}-{counter}",
                generation.run_id,
                std::process::id()
            ));
            std::fs::rename(&final_dir, &path)?;
            if previous_current.is_none() {
                restore_current(output, None)?;
            }
        }
        if let Err(error) = std::fs::rename(&stage, &final_dir) {
            return Err(error.into());
        }
        stage_guard.disarm();
        installation = Some(Installation::new(generations.clone(), final_dir.clone()));
        failpoint("generations-directory-fsync")?;
        File::open(&generations)?.sync_all()?;
    }
    verify_generation(&final_dir, &generation)?;

    let publication = std::fs::read(final_dir.join("publication.json"))?;
    let pointer = CurrentPointer {
        schema: CanonicalText::new("bbnf.architecture.current.v3.1")?,
        run_id: generation.run_id.clone(),
        publication_size: publication.len() as u64,
        publication_hash: Sha256Digest::of(&publication),
    };
    let mut pointer_bytes = serde_json::to_vec(&pointer)?;
    pointer_bytes.push(b'\n');
    let current_temp = output.join(format!(".CURRENT-{}-{counter}", std::process::id()));
    let mut current_guard = Cleanup::file(current_temp.clone());
    failpoint("current-create")?;
    let mut current = File::create(&current_temp)?;
    failpoint("current-write")?;
    current.write_all(&pointer_bytes)?;
    failpoint("current-fsync")?;
    current.sync_all()?;
    drop(current);
    failpoint("current-replace")?;
    std::fs::rename(&current_temp, output.join("CURRENT"))?;
    current_guard.disarm();
    let root_sync = failpoint("root-directory-fsync")
        .and_then(|()| File::open(output)?.sync_all().map_err(Into::into));
    if let Err(error) = root_sync {
        restore_current(output, previous_current.as_deref())?;
        return Err(error);
    }
    if let Some(installation) = &mut installation {
        installation.commit();
    }
    Ok(generation)
}

fn restore_current(output: &Path, previous: Option<&[u8]>) -> Result<()> {
    let current = output.join("CURRENT");
    if let Some(bytes) = previous {
        let counter = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let temp = output.join(format!(
            ".CURRENT-rollback-{}-{counter}",
            std::process::id()
        ));
        let mut file = File::create(&temp)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        drop(file);
        std::fs::rename(temp, current)?;
        File::open(output)?.sync_all()?;
    } else if current.exists() {
        std::fs::remove_file(current)?;
        File::open(output)?.sync_all()?;
    }
    Ok(())
}

fn verify_generation(directory: &Path, generation: &Generation) -> Result<()> {
    let actual_names = std::fs::read_dir(directory)?
        .map(|entry| entry.map(|entry| entry.file_name().to_string_lossy().into_owned()))
        .collect::<std::io::Result<BTreeSet<_>>>()?;
    let expected_names = generation
        .expected_files
        .iter()
        .map(|name| name.as_str().to_string())
        .collect::<BTreeSet<_>>();
    if actual_names != expected_names {
        bail!("EVIDENCE_PUBLICATION_NONATOMIC: generation file set differs");
    }
    let publication = std::fs::read(directory.join("publication.json"))?;
    let decoded: PublicationReceipt = serde_json::from_slice(&publication)?;
    if decoded != generation.receipt()
        || decoded.source_binding != generation.source_binding
        || decoded.schema.as_str() != "bbnf.architecture.generation.v3.1"
        || generation.run_id.as_str()
            != format!("p31-{}", crate::architecture::hash::sha256(&publication))
    {
        bail!("EVIDENCE_PUBLICATION_NONATOMIC: generation receipt identity differs");
    }
    for (name, publication) in &decoded.artifacts {
        match publication {
            ArtifactPublication::Published { size, hash } => {
                let (actual_size, actual_hash) = file_identity(&directory.join(name.as_str()))?;
                if actual_size != *size || actual_hash != *hash {
                    bail!("EVIDENCE_PUBLICATION_NONATOMIC: publication member differs");
                }
            }
            ArtifactPublication::PublicationRoot if name.as_str() == "publication.json" => {}
            ArtifactPublication::Unavailable { .. } => {}
            ArtifactPublication::PublicationRoot => {
                bail!("EVIDENCE_PUBLICATION_NONATOMIC: invalid publication trust-root role")
            }
        }
    }
    Ok(())
}

fn verified_current_bytes(output: &Path) -> Option<Vec<u8>> {
    let bytes = std::fs::read(output.join("CURRENT")).ok()?;
    verify_current(output, &bytes).ok()?;
    Some(bytes)
}

fn verify_current(output: &Path, bytes: &[u8]) -> Result<()> {
    let pointer: CurrentPointer = serde_json::from_slice(bytes)
        .context("EVIDENCE_PUBLICATION_NONATOMIC: decode CURRENT pointer")?;
    if pointer.schema.as_str() != "bbnf.architecture.current.v3.1" {
        bail!("EVIDENCE_PUBLICATION_NONATOMIC: CURRENT schema differs");
    }
    let directory = output.join("generations").join(pointer.run_id.as_str());
    let publication = std::fs::read(directory.join("publication.json"))?;
    if publication.len() as u64 != pointer.publication_size
        || Sha256Digest::of(&publication) != pointer.publication_hash
        || pointer.run_id.as_str()
            != format!("p31-{}", crate::architecture::hash::sha256(&publication))
    {
        bail!("EVIDENCE_PUBLICATION_NONATOMIC: CURRENT trust root differs");
    }
    let receipt: PublicationReceipt = serde_json::from_slice(&publication)?;
    let actual_names = std::fs::read_dir(&directory)?
        .map(|entry| entry.map(|entry| entry.file_name().to_string_lossy().into_owned()))
        .collect::<std::io::Result<BTreeSet<_>>>()?;
    let expected_names = receipt
        .expected_files
        .iter()
        .map(|name| name.as_str().to_string())
        .collect::<BTreeSet<_>>();
    if actual_names != expected_names {
        bail!("EVIDENCE_PUBLICATION_NONATOMIC: CURRENT directory set differs");
    }
    for (name, artifact) in receipt.artifacts {
        if let ArtifactPublication::Published { size, hash } = artifact {
            let (member_size, member_hash) = file_identity(&directory.join(name.as_str()))?;
            if member_size != size || member_hash != hash {
                bail!("EVIDENCE_PUBLICATION_NONATOMIC: CURRENT member differs");
            }
        }
    }
    Ok(())
}

fn file_identity(path: &Path) -> Result<(u64, Sha256Digest)> {
    let mut file = File::open(path)?;
    let mut hash = crate::architecture::hash::Sha256::new();
    let mut size = 0u64;
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        size += read as u64;
        hash.update(&buffer[..read]);
    }
    Ok((size, Sha256Digest::new(hash.finalize())?))
}

pub(super) fn write_synced(path: &Path, bytes: &[u8]) -> Result<()> {
    failpoint("file-create")?;
    let mut file = File::create(path).with_context(|| format!("write `{}`", path.display()))?;
    failpoint("file-write")?;
    file.write_all(bytes)?;
    failpoint("file-fsync")?;
    file.sync_all()?;
    Ok(())
}

struct LockGuard {
    _file: File,
}

impl LockGuard {
    fn acquire(path: &Path) -> Result<Self> {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(path)
            .context("open persistent architecture publication lock")?;
        let started = std::time::Instant::now();
        loop {
            match file.try_lock() {
                Ok(()) => {
                    file.set_len(0)?;
                    writeln!(file, "{}", std::process::id())?;
                    file.sync_all()?;
                    return Ok(Self { _file: file });
                }
                Err(TryLockError::WouldBlock) => {
                    if started.elapsed() > std::time::Duration::from_secs(45) {
                        bail!("architecture publication lock remained held for 45 seconds");
                    }
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                Err(TryLockError::Error(error)) => {
                    return Err(error).context("acquire architecture publication lock");
                }
            }
        }
    }
}

struct Installation {
    generations: PathBuf,
    final_dir: PathBuf,
    committed: bool,
}

impl Installation {
    fn new(generations: PathBuf, final_dir: PathBuf) -> Self {
        Self {
            generations,
            final_dir,
            committed: false,
        }
    }

    fn commit(&mut self) {
        self.committed = true;
    }
}

impl Drop for Installation {
    fn drop(&mut self) {
        if self.committed {
            return;
        }
        if self.final_dir.exists() {
            let _ = std::fs::remove_dir_all(&self.final_dir);
        }
        let _ = File::open(&self.generations).and_then(|directory| directory.sync_all());
    }
}

enum CleanupKind {
    File,
    Directory,
}

struct Cleanup {
    path: PathBuf,
    kind: CleanupKind,
    armed: bool,
}

impl Cleanup {
    fn file(path: PathBuf) -> Self {
        Self {
            path,
            kind: CleanupKind::File,
            armed: true,
        }
    }

    fn directory(path: PathBuf) -> Self {
        Self {
            path,
            kind: CleanupKind::Directory,
            armed: true,
        }
    }

    fn cleanup(&mut self) -> Result<()> {
        if self.armed {
            match self.kind {
                CleanupKind::File => std::fs::remove_file(&self.path)?,
                CleanupKind::Directory => std::fs::remove_dir_all(&self.path)?,
            }
            self.armed = false;
        }
        Ok(())
    }

    fn disarm(&mut self) {
        self.armed = false;
    }
}

impl Drop for Cleanup {
    fn drop(&mut self) {
        if self.armed {
            match self.kind {
                CleanupKind::File => {
                    let _ = std::fs::remove_file(&self.path);
                }
                CleanupKind::Directory => {
                    let _ = std::fs::remove_dir_all(&self.path);
                }
            }
        }
    }
}
