use std::collections::{BTreeMap, BTreeSet};
use std::io::Read as _;
use std::path::Path;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use crate::architecture::manifest::{
    ConstellationManifest, PackageFaceDecl, PackageFaceKind, ResolvedRepository,
};
use crate::architecture::schema::{
    ExclusionId, PackageFaceId, PhysicalIdentity, PhysicalKind, RepoPath, RepositoryId,
    Sha256Digest, validate_serialized_strings,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct InventoryRecord {
    pub identity: PhysicalIdentity,
    pub content_hash: Option<Sha256Digest>,
    pub semantic_faces: Vec<PackageFaceId>,
    pub exclusion: Option<ExclusionId>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Inventory {
    pub schema: String,
    pub records: Vec<InventoryRecord>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CapturedInput {
    pub identity: PhysicalIdentity,
    pub content_hash: Sha256Digest,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CapturedInputs {
    files: BTreeMap<(RepositoryId, RepoPath), CapturedInput>,
}

#[derive(Debug)]
pub struct InventorySnapshot {
    pub inventory: Inventory,
    pub inputs: CapturedInputs,
}

#[derive(Default)]
struct InventoryCollection {
    records: Vec<InventoryRecord>,
    inputs: CapturedInputs,
}

pub type CollectorInputBinding = (RepositoryId, RepoPath, Sha256Digest);

impl Inventory {
    pub fn collect(
        manifest: &ConstellationManifest,
        repositories: &[ResolvedRepository],
    ) -> Result<Self> {
        Self::collect_with_git(manifest, repositories, Path::new("git"))
    }

    pub fn collect_with_git(
        manifest: &ConstellationManifest,
        repositories: &[ResolvedRepository],
        git: &Path,
    ) -> Result<Self> {
        Ok(Self::collect_inner(manifest, repositories, git, false)?.inventory)
    }

    pub fn collect_snapshot(
        manifest: &ConstellationManifest,
        repositories: &[ResolvedRepository],
    ) -> Result<InventorySnapshot> {
        Self::collect_snapshot_with_git(manifest, repositories, Path::new("git"))
    }

    pub fn collect_snapshot_with_git(
        manifest: &ConstellationManifest,
        repositories: &[ResolvedRepository],
        git: &Path,
    ) -> Result<InventorySnapshot> {
        Self::collect_inner(manifest, repositories, git, true)
    }

    fn collect_inner(
        manifest: &ConstellationManifest,
        repositories: &[ResolvedRepository],
        git: &Path,
        capture_inputs: bool,
    ) -> Result<InventorySnapshot> {
        crate::architecture::evidence::write::failpoint("inventory-walk")?;
        let mut collection = InventoryCollection::default();
        for repository in repositories {
            let exclusions = manifest
                .exclusions
                .iter()
                .filter(|item| item.repository == repository.id)
                .collect::<Vec<_>>();
            walk_repository(
                &repository.id,
                &repository.root,
                &repository.root,
                &exclusions,
                manifest,
                capture_inputs,
                &mut collection,
            )?;
        }
        let InventoryCollection {
            mut records,
            inputs,
        } = collection;
        records.sort();
        let mut inventory = Self {
            schema: "bbnf.architecture.inventory.v3.1".into(),
            records,
        };
        inventory.attach_faces(manifest)?;
        inventory.validate(manifest)?;
        reconcile_tracked_exclusions_with_inventory(manifest, repositories, &inventory, git)?;
        inputs.validate(&inventory)?;
        Ok(InventorySnapshot { inventory, inputs })
    }

    pub fn collector_bindings(
        &self,
        manifest: &ConstellationManifest,
    ) -> Result<Vec<CollectorInputBinding>> {
        self.records
            .iter()
            .filter(|record| {
                capture_for_frontend(
                    manifest,
                    &record.identity.repository,
                    &record.identity.path,
                )
            })
            .map(|record| {
                if record.identity.kind != PhysicalKind::File || record.exclusion.is_some() {
                    bail!(
                        "COLLECTOR_INPUT_RECONCILIATION_FAILED: typed collector input is excluded or not a regular file"
                    );
                }
                Ok((
                    record.identity.repository.clone(),
                    record.identity.path.clone(),
                    record
                        .content_hash
                        .clone()
                        .context("captured collector input is unhashed")?,
                ))
            })
            .collect()
    }

    fn attach_faces(&mut self, manifest: &ConstellationManifest) -> Result<()> {
        for face in &manifest.package_faces {
            let mut linked = false;
            for record in &mut self.records {
                if record.identity.repository == face.repository
                    && within(&record.identity.path, &face.root)
                {
                    if record.identity.kind == PhysicalKind::Symlink {
                        if record.exclusion.is_none()
                            && semantic_symlink(face, &record.identity.path)
                        {
                            bail!(
                                "SEMANTIC_SYMLINK_REJECTED: package face `{}` contains typed-read symlink `{}`",
                                face.id,
                                record.identity.path
                            );
                        }
                        continue;
                    }
                    record.semantic_faces.push(face.id.clone());
                    record.semantic_faces.sort();
                    record.semantic_faces.dedup();
                    linked = true;
                }
            }
            if !linked {
                bail!(
                    "INVENTORY_SEMANTIC_FACE_UNLINKED: package face `{}` root `{}` is absent",
                    face.id,
                    face.root
                );
            }
        }
        Ok(())
    }

    pub fn validate(&self, manifest: &ConstellationManifest) -> Result<()> {
        let repositories = manifest
            .repositories
            .iter()
            .map(|item| item.id.clone())
            .collect::<BTreeSet<_>>();
        let mut triples = BTreeSet::new();
        let mut paths = BTreeMap::new();
        for record in &self.records {
            if !repositories.contains(&record.identity.repository) {
                bail!("INVENTORY_SCOPE_FACE_MISMATCH: unknown physical repository");
            }
            if !triples.insert(record.identity.clone()) {
                bail!("INVENTORY_PATH_AMBIGUOUS: duplicate physical triple");
            }
            let path_key = (
                record.identity.repository.clone(),
                record.identity.path.clone(),
            );
            if let Some(previous) = paths.insert(path_key, record.identity.kind.clone())
                && previous != record.identity.kind
            {
                bail!("INVENTORY_PATH_AMBIGUOUS: one path has multiple file kinds");
            }
            match record.identity.kind {
                PhysicalKind::File | PhysicalKind::Symlink if record.content_hash.is_none() => {
                    bail!("physical file lacks content identity")
                }
                PhysicalKind::Directory if record.content_hash.is_some() => {
                    bail!("INVENTORY_NODE_KIND_MISMATCH: directory has file content hash")
                }
                _ => {}
            }
        }
        for face in &manifest.package_faces {
            self.expect_kind(
                &face.repository,
                &face.root,
                PhysicalKind::Directory,
                "PACKAGE_FACE_ROOT_KIND_MISMATCH",
            )?;
            self.expect_kind(
                &face.repository,
                &face.manifest,
                PhysicalKind::File,
                "PACKAGE_FACE_MANIFEST_KIND_MISMATCH",
            )?;
        }
        for workspace in &manifest.workspaces {
            self.expect_kind(
                &workspace.repository,
                &workspace.manifest,
                PhysicalKind::File,
                "WORKSPACE_MANIFEST_KIND_MISMATCH",
            )?;
        }
        for context in &manifest.contexts {
            self.expect_kind(
                &context.repository,
                &context.manifest,
                PhysicalKind::File,
                "CONTEXT_MANIFEST_KIND_MISMATCH",
            )?;
            self.expect_kind(
                &context.repository,
                &context.lock,
                PhysicalKind::File,
                "CONTEXT_LOCK_KIND_MISMATCH",
            )?;
            for config in &context.config_inputs {
                self.expect_kind(
                    &context.repository,
                    config,
                    PhysicalKind::File,
                    "CONTEXT_CONFIG_KIND_MISMATCH",
                )?;
            }
        }
        for document in &manifest.documentation {
            self.expect_kind(
                &document.repository,
                &document.path,
                PhysicalKind::File,
                "DOCUMENT_PATH_KIND_MISMATCH",
            )?;
        }
        validate_serialized_strings(self)
    }

    pub fn find(&self, repository: &RepositoryId, path: &RepoPath) -> Option<&InventoryRecord> {
        self.records
            .iter()
            .find(|item| &item.identity.repository == repository && &item.identity.path == path)
    }

    pub fn repository_hash(&self, repository: &RepositoryId) -> Result<Sha256Digest> {
        let records = self
            .records
            .iter()
            .filter(|item| &item.identity.repository == repository)
            .collect::<Vec<_>>();
        Sha256Digest::new(crate::architecture::hash::hash_serialized(&records)?)
    }

    fn expect_kind(
        &self,
        repository: &RepositoryId,
        path: &RepoPath,
        expected: PhysicalKind,
        code: &str,
    ) -> Result<()> {
        let record = self
            .find(repository, path)
            .with_context(|| format!("{code}: required path `{repository}:{path}` is absent"))?;
        if record.identity.kind != expected {
            bail!("{code}: required path `{repository}:{path}` has the wrong physical kind");
        }
        Ok(())
    }
}

impl CapturedInputs {
    pub fn from_bytes(inputs: Vec<(PhysicalIdentity, Vec<u8>)>) -> Result<Self> {
        let mut files = BTreeMap::new();
        for (identity, bytes) in inputs {
            if identity.kind != PhysicalKind::File {
                bail!("collector typed reads require regular files");
            }
            let content_hash = Sha256Digest::of(&bytes);
            if files
                .insert(
                    (identity.repository.clone(), identity.path.clone()),
                    CapturedInput {
                        identity,
                        content_hash,
                        bytes,
                    },
                )
                .is_some()
            {
                bail!("duplicate captured collector input");
            }
        }
        Ok(Self { files })
    }

    pub fn frontend_face(&self, face: &PackageFaceDecl) -> Vec<&CapturedInput> {
        self.files
            .values()
            .filter(|input| input.identity.repository == face.repository)
            .filter(|input| within(&input.identity.path, &face.root))
            .collect()
    }

    pub fn find(&self, repository: &RepositoryId, path: &RepoPath) -> Option<&CapturedInput> {
        self.files.get(&(repository.clone(), path.clone()))
    }

    pub fn bindings(&self) -> Vec<CollectorInputBinding> {
        self.files
            .values()
            .map(|input| {
                (
                    input.identity.repository.clone(),
                    input.identity.path.clone(),
                    input.content_hash.clone(),
                )
            })
            .collect()
    }

    pub fn validate(&self, inventory: &Inventory) -> Result<()> {
        for input in self.files.values() {
            let record = inventory
                .find(&input.identity.repository, &input.identity.path)
                .context("captured collector input is absent from inventory")?;
            if record.identity.kind != PhysicalKind::File
                || record.exclusion.is_some()
                || record.content_hash.as_ref() != Some(&input.content_hash)
                || Sha256Digest::of(&input.bytes) != input.content_hash
            {
                bail!(
                    "COLLECTOR_INPUT_RECONCILIATION_FAILED: typed collector input is excluded, unhashed, or changed"
                );
            }
        }
        Ok(())
    }
}

fn within(path: &RepoPath, root: &RepoPath) -> bool {
    root.as_str() == "."
        || path.as_str() == root.as_str()
        || path
            .as_str()
            .strip_prefix(root.as_str())
            .is_some_and(|suffix| suffix.starts_with('/'))
}

fn semantic_symlink(face: &PackageFaceDecl, path: &RepoPath) -> bool {
    if path == &face.manifest {
        return true;
    }
    let extension = Path::new(path.as_str())
        .extension()
        .and_then(|value| value.to_str())
        .unwrap_or_default();
    match face.kind {
        PackageFaceKind::Rust => extension == "rs",
        PackageFaceKind::Frontend | PackageFaceKind::GeneratedFrontend => {
            matches!(
                extension,
                "js" | "mjs" | "cjs" | "ts" | "tsx" | "vue" | "css" | "scss" | "sass" | "less"
            )
        }
    }
}

fn capture_for_frontend(
    manifest: &ConstellationManifest,
    repository: &RepositoryId,
    path: &RepoPath,
) -> bool {
    manifest.package_faces.iter().any(|face| {
        face.repository == *repository
            && matches!(
                face.kind,
                PackageFaceKind::Frontend | PackageFaceKind::GeneratedFrontend
            )
            && within(path, &face.root)
            && (path == &face.manifest
                || matches!(
                    Path::new(path.as_str())
                        .extension()
                        .and_then(|value| value.to_str()),
                    Some("vue" | "css" | "scss" | "sass" | "less")
                ))
    })
}

pub fn reconcile_tracked_exclusions(
    manifest: &ConstellationManifest,
    repositories: &[ResolvedRepository],
) -> Result<()> {
    reconcile_tracked_exclusions_inner(manifest, repositories, None, Path::new("git"))
}

fn reconcile_tracked_exclusions_with_inventory(
    manifest: &ConstellationManifest,
    repositories: &[ResolvedRepository],
    inventory: &Inventory,
    git: &Path,
) -> Result<()> {
    reconcile_tracked_exclusions_inner(manifest, repositories, Some(inventory), git)
}

fn reconcile_tracked_exclusions_inner(
    manifest: &ConstellationManifest,
    repositories: &[ResolvedRepository],
    inventory: Option<&Inventory>,
    git: &Path,
) -> Result<()> {
    for repository in repositories {
        let output = crate::architecture::canonical_child_command(git)
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
            .arg(&repository.root)
            .arg("--work-tree")
            .arg(&repository.root)
            .args(["ls-files", "--stage", "-z"])
            .output()
            .context("reconcile exclusion policy with tracked files")?;
        if !output.status.success() {
            bail!("TRACKED_EXCLUSION_CENSUS_FAILED: git ls-files failed");
        }
        let exclusions = manifest
            .exclusions
            .iter()
            .filter(|item| item.repository == repository.id)
            .collect::<Vec<_>>();
        for row in output
            .stdout
            .split(|byte| *byte == 0)
            .filter(|item| !item.is_empty())
        {
            let row = std::str::from_utf8(row).context("tracked path is not UTF-8")?;
            let (metadata, path) = row
                .split_once('\t')
                .context("tracked stage row omitted path")?;
            let mode = metadata.split_whitespace().next().unwrap_or_default();
            if let Some(exclusion) = matching_exclusion(path, &exclusions) {
                let recorded_boundary_symlink = mode == "120000"
                    && exclusion_boundary(path, &exclusions)
                    && inventory.is_some_and(|inventory| {
                        inventory
                            .find(
                                &repository.id,
                                &RepoPath::new(path).expect("Git path is repository relative"),
                            )
                            .is_some_and(|record| {
                                record.identity.kind == PhysicalKind::Symlink
                                    && record.content_hash.is_some()
                                    && record.exclusion.as_ref() == Some(exclusion)
                            })
                    });
                if recorded_boundary_symlink {
                    continue;
                }
                bail!(
                    "TRACKED_INPUT_EXCLUDED: tracked path `{}` is hidden by exclusion `{}`",
                    path,
                    exclusion
                );
            }
        }
    }
    Ok(())
}

fn walk_repository(
    repository: &RepositoryId,
    root: &Path,
    path: &Path,
    exclusions: &[&crate::architecture::manifest::ExclusionDecl],
    manifest: &ConstellationManifest,
    capture_inputs: bool,
    collection: &mut InventoryCollection,
) -> Result<()> {
    let relative = relative_path(root, path)?;
    let exclusion = matching_exclusion(relative.as_str(), exclusions);
    let metadata = std::fs::symlink_metadata(path)
        .with_context(|| format!("inventory repository `{repository}`"))?;
    let kind = if metadata.file_type().is_symlink() {
        PhysicalKind::Symlink
    } else if metadata.is_dir() {
        PhysicalKind::Directory
    } else {
        PhysicalKind::File
    };
    let content_hash = match kind {
        PhysicalKind::File => {
            let capture = capture_inputs && capture_for_frontend(manifest, repository, &relative);
            let (hash, bytes) = read_file(path, capture)?;
            if let Some(bytes) = bytes {
                collection.inputs.files.insert(
                    (repository.clone(), relative.clone()),
                    CapturedInput {
                        identity: PhysicalIdentity {
                            repository: repository.clone(),
                            path: relative.clone(),
                            kind: PhysicalKind::File,
                        },
                        content_hash: hash.clone(),
                        bytes,
                    },
                );
            }
            Some(hash)
        }
        PhysicalKind::Symlink => {
            let target = std::fs::read_link(path)?;
            Some(Sha256Digest::of(
                &crate::architecture::hash::exact_os_identity_bytes(
                    b"inventory-symlink-target",
                    target.as_os_str(),
                ),
            ))
        }
        PhysicalKind::Directory => None,
    };
    collection.records.push(InventoryRecord {
        identity: PhysicalIdentity {
            repository: repository.clone(),
            path: relative,
            kind: kind.clone(),
        },
        content_hash,
        semantic_faces: Vec::new(),
        exclusion: exclusion.cloned(),
    });
    if kind != PhysicalKind::Directory || exclusion.is_some() {
        return Ok(());
    }
    let mut entries = std::fs::read_dir(path)
        .with_context(|| format!("inventory repository `{repository}`"))?
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        walk_repository(
            repository,
            root,
            &entry.path(),
            exclusions,
            manifest,
            capture_inputs,
            collection,
        )?;
    }
    Ok(())
}

fn read_file(path: &Path, capture: bool) -> Result<(Sha256Digest, Option<Vec<u8>>)> {
    crate::architecture::evidence::write::failpoint("inventory-read")?;
    let mut file = std::fs::File::open(path)?;
    let mut hash = crate::architecture::hash::Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    let mut bytes = capture.then(Vec::new);
    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hash.update(&buffer[..read]);
        if let Some(bytes) = &mut bytes {
            bytes.extend_from_slice(&buffer[..read]);
        }
    }
    Ok((Sha256Digest::new(hash.finalize())?, bytes))
}

fn relative_path(root: &Path, path: &Path) -> Result<RepoPath> {
    let relative = path
        .strip_prefix(root)
        .context("inventory path escaped repository")?;
    if relative.as_os_str().is_empty() {
        return Ok(RepoPath::root());
    }
    RepoPath::from_relative_path(relative).context("inventory repository path is not strict UTF-8")
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn relative_path_for_test(root: &Path, path: &Path) -> Result<RepoPath> {
    relative_path(root, path)
}

fn matching_exclusion<'a>(
    path: &str,
    exclusions: &'a [&crate::architecture::manifest::ExclusionDecl],
) -> Option<&'a ExclusionId> {
    exclusions.iter().find_map(|item| {
        let pattern = item.pattern.as_str();
        let prefix = pattern.strip_suffix("/**")?;
        if let Some(segment) = prefix.strip_prefix("**/") {
            return path
                .split('/')
                .any(|part| part == segment)
                .then_some(&item.id);
        }
        (path == prefix || path.starts_with(&format!("{prefix}/"))).then_some(&item.id)
    })
}

fn exclusion_boundary(
    path: &str,
    exclusions: &[&crate::architecture::manifest::ExclusionDecl],
) -> bool {
    exclusions.iter().any(|item| {
        let Some(prefix) = item.pattern.as_str().strip_suffix("/**") else {
            return false;
        };
        if let Some(segment) = prefix.strip_prefix("**/") {
            return path.rsplit('/').next() == Some(segment);
        }
        path == prefix
    })
}
