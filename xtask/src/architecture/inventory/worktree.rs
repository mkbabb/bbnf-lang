use std::collections::BTreeSet;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::architecture::hash::Sha256;
use crate::architecture::manifest::{ConstellationManifest, ResolvedRepository, WorktreeSelector};
use crate::architecture::schema::{
    CanonicalText, ProductId, RepositoryId, Sha256Digest, WorktreeId,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct WorktreeRecord {
    pub identity: WorktreeId,
    pub repository: RepositoryId,
    pub administrative_key: CanonicalText,
    pub head: CanonicalText,
    pub branch: Option<CanonicalText>,
    pub detached: bool,
    pub bare: bool,
    pub dirty: bool,
    pub content_hash: Sha256Digest,
    pub administrative_config_hash: Sha256Digest,
    pub ignored_content_policy: CanonicalText,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ResolvedNomination {
    pub nomination: WorktreeId,
    pub repository: RepositoryId,
    pub product: ProductId,
    pub worktree: WorktreeId,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct WorktreeHashTranscript {
    pub workers: usize,
    pub buffer_bytes_per_worker: usize,
    pub jobs: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WorktreeCensus {
    pub records: Vec<WorktreeRecord>,
    pub hashing: WorktreeHashTranscript,
}

const MAX_HASH_WORKERS: usize = 18;
const HASH_BUFFER_BYTES: usize = 512 * 1024;

#[derive(Default)]
struct RawWorktree {
    path: Option<PathBuf>,
    head: String,
    branch: Option<String>,
    detached: bool,
    bare: bool,
}

pub fn collect_worktrees(repositories: &[ResolvedRepository]) -> Result<Vec<WorktreeRecord>> {
    collect_worktrees_with_git(repositories, Path::new("git"))
}

pub fn collect_worktrees_with_git(
    repositories: &[ResolvedRepository],
    git_executable: &Path,
) -> Result<Vec<WorktreeRecord>> {
    Ok(collect_worktree_census_with_git(repositories, git_executable)?.records)
}

pub fn collect_worktree_census_with_git(
    repositories: &[ResolvedRepository],
    git_executable: &Path,
) -> Result<WorktreeCensus> {
    let mut pending = Vec::new();
    for repository in repositories {
        pending.extend(collect_repository_inputs(repository, git_executable)?);
    }
    let (digests, hashing) = global_entry_digests(&pending)?;
    let mut records = pending
        .into_iter()
        .zip(digests)
        .map(|(pending, digests)| pending.finish(digests))
        .collect::<Result<Vec<_>>>()?;
    records.sort();
    let mut identities = BTreeSet::new();
    for record in &records {
        if !identities.insert((record.repository.clone(), record.identity.clone())) {
            bail!("WORKTREE_IDENTITY_COLLISION: duplicate portable worktree identity");
        }
    }
    Ok(WorktreeCensus { records, hashing })
}

fn collect_repository_inputs(
    repository: &ResolvedRepository,
    git_executable: &Path,
) -> Result<Vec<PendingWorktree>> {
    let output = git(
        git_executable,
        &repository.root,
        &["worktree", "list", "--porcelain"],
    )?;
    let text = std::str::from_utf8(&output).context("git worktree output is not UTF-8")?;
    let mut raw = Vec::new();
    let mut current = RawWorktree::default();
    for line in text.lines().chain([""]) {
        if line.is_empty() {
            if current.path.is_some() {
                raw.push(std::mem::take(&mut current));
            }
        } else if let Some(path) = line.strip_prefix("worktree ") {
            current.path = Some(PathBuf::from(path));
        } else if let Some(head) = line.strip_prefix("HEAD ") {
            current.head = head.into();
        } else if let Some(branch) = line.strip_prefix("branch ") {
            current.branch = Some(branch.trim_start_matches("refs/heads/").into());
        } else if line == "detached" {
            current.detached = true;
        } else if line == "bare" {
            current.bare = true;
        }
    }
    let declared_root = std::fs::canonicalize(&repository.root)
        .context("canonicalize declared repository worktree")?;
    let measured_primary = raw
        .first()
        .and_then(|worktree| worktree.path.as_ref())
        .context("Git omitted the primary worktree")?;
    if std::fs::canonicalize(measured_primary).context("canonicalize measured primary worktree")?
        != declared_root
    {
        bail!(
            "WORKTREE_ROOT_REDIRECTED: Git primary worktree differs from the declared repository root"
        );
    }
    raw.iter()
        .map(|item| PendingWorktree::capture(git_executable, &repository.id, item))
        .collect()
}

pub fn resolve_nominations(
    manifest: &ConstellationManifest,
    worktrees: &[WorktreeRecord],
) -> Result<Vec<ResolvedNomination>> {
    let mut resolved = Vec::new();
    for nomination in &manifest.worktree_nominations {
        let candidates = worktrees
            .iter()
            .filter(|record| record.repository == nomination.repository)
            .filter(|record| match nomination.selector {
                WorktreeSelector::ManifestRepositoryRoot => {
                    record.administrative_key.as_str() == "primary"
                }
            })
            .collect::<Vec<_>>();
        if candidates.len() != 1 {
            bail!(
                "PRODUCT_WORKTREE_UNNOMINATED: nomination `{}` resolved to {} measured worktrees",
                nomination.id,
                candidates.len()
            );
        }
        resolved.push(ResolvedNomination {
            nomination: nomination.id.clone(),
            repository: nomination.repository.clone(),
            product: nomination.product.clone(),
            worktree: candidates[0].identity.clone(),
        });
    }
    resolved.sort();
    Ok(resolved)
}

fn administrative_key(git_executable: &Path, path: &Path, bare: bool) -> Result<String> {
    if bare {
        return Ok("bare".into());
    }
    let git_dir = canonical_git_path(git_executable, path, "--git-dir")?;
    let common_dir = canonical_git_path(git_executable, path, "--git-common-dir")?;
    if git_dir == common_dir {
        return Ok("primary".into());
    }
    let relative = git_dir.strip_prefix(&common_dir).context(
        "WORKTREE_ADMINISTRATIVE_KEY_INVALID: Git directory is outside the canonical common directory",
    )?;
    let components = relative.components().collect::<Vec<_>>();
    if components.len() != 2
        || components[0].as_os_str() != "worktrees"
        || components[1].as_os_str().to_str().is_none()
    {
        bail!(
            "WORKTREE_ADMINISTRATIVE_KEY_INVALID: linked Git directory must be worktrees/<one-segment>"
        );
    }
    let name = components[1].as_os_str().to_str().expect("checked UTF-8");
    Ok(format!("worktrees/{name}"))
}

fn canonical_git_path(executable: &Path, root: &Path, argument: &str) -> Result<PathBuf> {
    let output = git(executable, root, &["rev-parse", argument])?;
    let text = std::str::from_utf8(&output).context("Git administrative path is not UTF-8")?;
    let path = PathBuf::from(text.trim());
    let absolute = if path.is_absolute() {
        path
    } else {
        root.join(path)
    };
    std::fs::canonicalize(absolute).context("canonicalize Git administrative path")
}

struct PendingWorktree {
    repository: RepositoryId,
    administrative_key: String,
    head: String,
    branch: Option<String>,
    detached: bool,
    bare: bool,
    root: PathBuf,
    tracked: Vec<u8>,
    untracked: Vec<u8>,
    changed: Vec<PathBuf>,
    administrative_config_hash: Sha256Digest,
}

impl PendingWorktree {
    fn capture(
        git_executable: &Path,
        repository: &RepositoryId,
        item: &RawWorktree,
    ) -> Result<Self> {
        let path = item.path.as_ref().context("worktree record omitted path")?;
        let administrative_key = administrative_key(git_executable, path, item.bare)?;
        let administrative_config_hash =
            administrative_config_hash(git_executable, path, item.bare)?;
        let tracked = git(
            git_executable,
            path,
            &[
                "diff",
                "--raw",
                "-z",
                "--no-abbrev",
                "--no-renames",
                "--ignore-submodules=all",
                "HEAD",
            ],
        )?;
        let mut changed = Vec::new();
        let mut tracked_rows = tracked
            .split(|byte| *byte == 0)
            .filter(|row| !row.is_empty());
        while let Some(header) = tracked_rows.next() {
            if !header.starts_with(b":") {
                bail!("Git raw diff record omitted metadata");
            }
            let relative = tracked_rows
                .next()
                .context("Git raw diff record omitted path")?;
            let relative = std::str::from_utf8(relative).context("non-UTF-8 tracked path")?;
            if relative.split('/').any(|part| part == "..") || Path::new(relative).is_absolute() {
                bail!("tracked path escaped worktree");
            }
            changed.push(PathBuf::from(relative));
        }
        let untracked = git(
            git_executable,
            path,
            &[
                "ls-files",
                "-z",
                "--others",
                "--exclude-per-directory=.gitignore",
            ],
        )?;
        for relative in untracked
            .split(|byte| *byte == 0)
            .filter(|item| !item.is_empty())
        {
            let relative = std::str::from_utf8(relative).context("non-UTF-8 untracked path")?;
            if relative.split('/').any(|part| part == "..") || Path::new(relative).is_absolute() {
                bail!("untracked path escaped worktree");
            }
            changed.push(PathBuf::from(relative));
        }
        Ok(Self {
            repository: repository.clone(),
            administrative_key,
            head: item.head.clone(),
            branch: item.branch.clone(),
            detached: item.detached,
            bare: item.bare,
            root: path.clone(),
            tracked,
            untracked,
            changed,
            administrative_config_hash,
        })
    }

    fn finish(self, digests: Vec<String>) -> Result<WorktreeRecord> {
        if digests.len() != self.changed.len() {
            bail!("WORKTREE_HASH_QUEUE_INCOMPLETE: changed-entry result count differs");
        }
        let mut hash = Sha256::new();
        hash.update(b"worktree-content-tree-v2\0");
        hash.update(self.head.as_bytes());
        hash.update(&[0xff]);
        hash.update(&self.tracked);
        hash.update(&[0xfe]);
        for digest in digests {
            hash.update(&[0xfd]);
            hash.update(digest.as_bytes());
        }
        let key_digest = crate::architecture::hash::sha256(self.administrative_key.as_bytes());
        Ok(WorktreeRecord {
            identity: WorktreeId::new(format!("{}-worktree-{key_digest}", self.repository))?,
            repository: self.repository,
            administrative_key: CanonicalText::new(self.administrative_key)?,
            head: CanonicalText::new(self.head)?,
            branch: self.branch.map(CanonicalText::new).transpose()?,
            detached: self.detached,
            bare: self.bare,
            dirty: !self.tracked.is_empty() || !self.untracked.is_empty(),
            content_hash: Sha256Digest::new(hash.finalize())?,
            administrative_config_hash: self.administrative_config_hash,
            ignored_content_policy: CanonicalText::new(
                "checked-in-.gitignore-only; info-global-system-excludes-disabled; ignored-derived-bytes-excluded; collector-readable-bytes-source-bound",
            )?,
        })
    }
}

fn global_entry_digests(
    worktrees: &[PendingWorktree],
) -> Result<(Vec<Vec<String>>, WorktreeHashTranscript)> {
    let jobs = worktrees
        .iter()
        .enumerate()
        .flat_map(|(worktree, pending)| {
            pending
                .changed
                .iter()
                .enumerate()
                .map(move |(entry, relative)| (worktree, entry, &pending.root, relative))
        })
        .collect::<Vec<_>>();
    let available = std::thread::available_parallelism()
        .map(|count| count.get())
        .unwrap_or(1);
    let requested = std::env::var("BBNF_ARCHITECTURE_WORKTREE_THREADS")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0);
    let workers = worker_policy(requested, jobs.len(), available);
    let mut indexed = if jobs.is_empty() {
        Vec::new()
    } else {
        let next = AtomicUsize::new(0);
        std::thread::scope(|scope| {
            (0..workers)
                .map(|_| {
                    let jobs = &jobs;
                    let next = &next;
                    scope.spawn(move || -> Result<Vec<_>> {
                        let mut output = Vec::new();
                        let mut buffer = vec![0u8; HASH_BUFFER_BYTES];
                        loop {
                            let index = next.fetch_add(1, Ordering::Relaxed);
                            let Some((worktree, entry, root, relative)) = jobs.get(index) else {
                                break;
                            };
                            output.push((
                                *worktree,
                                *entry,
                                changed_entry_digest(root, relative, &mut buffer)?,
                            ));
                        }
                        Ok(output)
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|thread| {
                    thread
                        .join()
                        .map_err(|_| anyhow::anyhow!("worktree entry hashing thread panicked"))?
                })
                .collect::<Result<Vec<_>>>()
                .map(|chunks| chunks.into_iter().flatten().collect::<Vec<_>>())
        })?
    };
    indexed.sort_by_key(|(worktree, entry, _)| (*worktree, *entry));
    let mut output = worktrees
        .iter()
        .map(|pending| Vec::with_capacity(pending.changed.len()))
        .collect::<Vec<_>>();
    for (worktree, _, digest) in indexed {
        output[worktree].push(digest);
    }
    Ok((
        output,
        WorktreeHashTranscript {
            workers,
            buffer_bytes_per_worker: HASH_BUFFER_BYTES,
            jobs: jobs.len(),
        },
    ))
}

fn worker_policy(requested: Option<usize>, jobs: usize, available: usize) -> usize {
    let hard_cap = available.clamp(1, MAX_HASH_WORKERS);
    requested
        .filter(|requested| *requested > 0)
        .unwrap_or(hard_cap)
        .min(hard_cap)
        .min(jobs.max(1))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn requested_worker_policy_for_test(requested: usize, jobs: usize, available: usize) -> usize {
    worker_policy(Some(requested), jobs, available)
}

fn changed_entry_digest(root: &Path, relative: &Path, buffer: &mut [u8]) -> Result<String> {
    let mut hash = Sha256::new();
    append_changed(root, relative, &mut hash, buffer)?;
    Ok(hash.finalize())
}

fn append_changed(
    root: &Path,
    relative: &Path,
    hash: &mut Sha256,
    buffer: &mut [u8],
) -> Result<()> {
    let absolute = root.join(relative);
    let metadata = match std::fs::symlink_metadata(&absolute) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            hash.update(&[0xf8]);
            hash.update(relative.to_string_lossy().as_bytes());
            return Ok(());
        }
        Err(error) => {
            return Err(error).with_context(|| format!("read changed `{}`", relative.display()));
        }
    };
    hash.update(&[0xfc]);
    hash.update(relative.to_string_lossy().as_bytes());
    if metadata.file_type().is_symlink() {
        hash.update(&[0xfa]);
        hash.update(std::fs::read_link(&absolute)?.to_string_lossy().as_bytes());
    } else if metadata.is_dir() {
        hash.update(&[0xf9]);
        let mut entries = std::fs::read_dir(&absolute)?.collect::<std::io::Result<Vec<_>>>()?;
        entries.sort_by_key(std::fs::DirEntry::file_name);
        for entry in entries {
            append_changed(root, &relative.join(entry.file_name()), hash, buffer)?;
        }
    } else {
        hash.update(&[0xfb]);
        let mut file = std::fs::File::open(&absolute)?;
        loop {
            let read = file.read(buffer)?;
            if read == 0 {
                break;
            }
            hash.update(&buffer[..read]);
        }
    }
    Ok(())
}

fn git(executable: &Path, root: &Path, args: &[&str]) -> Result<Vec<u8>> {
    crate::architecture::evidence::write::failpoint("git-command")?;
    let output = crate::architecture::canonical_child_command(executable)
        .env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_NOSYSTEM", "1")
        .env("GIT_CONFIG_SYSTEM", "/dev/null")
        .env("GIT_OPTIONAL_LOCKS", "0")
        .args(["-c", "core.fsmonitor=false"])
        .args(["-c", "core.untrackedCache=false"])
        .args(["-c", "core.ignorecase=false"])
        .args(["-c", "core.filemode=true"])
        .args(["-c", "core.symlinks=true"])
        .arg("-C")
        .arg(root)
        .arg("--work-tree")
        .arg(root)
        .args(args)
        .output()
        .context("execute git worktree truth command")?;
    if !output.status.success() {
        bail!(
            "git worktree truth command failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(output.stdout)
}

fn administrative_config_hash(
    git_executable: &Path,
    root: &Path,
    bare: bool,
) -> Result<Sha256Digest> {
    let mut hash = Sha256::new();
    hash.update(b"git-administrative-config-v3.1\0");
    if bare {
        hash.update(b"bare\0");
        return Sha256Digest::new(hash.finalize());
    }
    let git_dir = canonical_git_path(git_executable, root, "--git-dir")?;
    let common_dir = canonical_git_path(git_executable, root, "--git-common-dir")?;
    for (identity, path) in [
        ("worktree-dot-git", root.join(".git")),
        ("common-config", common_dir.join("config")),
        ("common-worktree-config", common_dir.join("config.worktree")),
        ("worktree-config", git_dir.join("config.worktree")),
    ] {
        hash.update(identity.as_bytes());
        hash.update(&[0]);
        match std::fs::symlink_metadata(&path) {
            Ok(metadata) if metadata.is_file() && !metadata.file_type().is_symlink() => {
                hash.update(b"file\0");
                let mut file = std::fs::File::open(path)?;
                let mut buffer = [0u8; 64 * 1024];
                loop {
                    let read = file.read(&mut buffer)?;
                    if read == 0 {
                        break;
                    }
                    hash.update(&buffer[..read]);
                }
            }
            Ok(metadata) if metadata.is_dir() && identity == "worktree-dot-git" => {
                hash.update(b"directory\0");
            }
            Ok(_) => bail!("WORKTREE_ADMIN_CONFIG_INVALID: unsupported administrative input"),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                hash.update(b"absent\0");
            }
            Err(error) => return Err(error).context("read Git administrative config"),
        }
        hash.update(&[0xff]);
    }
    Sha256Digest::new(hash.finalize())
}
