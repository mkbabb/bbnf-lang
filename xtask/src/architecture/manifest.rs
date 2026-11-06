use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use super::schema::{
    CanonicalText, ContextId, DocumentationId, ExclusionId, PackageFaceId, ProductId, RepoPath,
    RepositoryId, WorkspaceId, WorktreeId, validate_serialized_strings,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RepositoryDiscovery {
    WorkspaceRoot,
    Sibling,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct RepositoryDecl {
    pub id: RepositoryId,
    pub discovery: RepositoryDiscovery,
    pub sibling_name: Option<CanonicalText>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ProductDecl {
    pub id: ProductId,
    pub repository: RepositoryId,
    pub workspaces: Vec<WorkspaceId>,
    pub package_faces: Vec<PackageFaceId>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct WorkspaceDecl {
    pub id: WorkspaceId,
    pub repository: RepositoryId,
    pub manifest: RepoPath,
    pub package_faces: Vec<PackageFaceId>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ContextKind {
    Cargo,
    Frontend,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ContextDecl {
    pub id: ContextId,
    pub kind: ContextKind,
    pub repository: RepositoryId,
    pub workspace: WorkspaceId,
    pub package_faces: Vec<PackageFaceId>,
    pub manifest: RepoPath,
    pub lock: RepoPath,
    pub config_inputs: Vec<RepoPath>,
    pub target: CanonicalText,
    pub profile: CanonicalText,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PackageFaceKind {
    Rust,
    Frontend,
    GeneratedFrontend,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct PackageFaceDecl {
    pub id: PackageFaceId,
    pub repository: RepositoryId,
    pub product: ProductId,
    pub workspace: WorkspaceId,
    pub kind: PackageFaceKind,
    pub root: RepoPath,
    pub manifest: RepoPath,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DocumentationRole {
    ContractAuthority,
    VerdictAuthority,
    SupersededEvidence,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct DocumentationDecl {
    pub id: DocumentationId,
    pub repository: RepositoryId,
    pub path: RepoPath,
    pub role: DocumentationRole,
    pub superseded_by: Option<DocumentationId>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ExclusionDecl {
    pub id: ExclusionId,
    pub repository: RepositoryId,
    pub pattern: CanonicalText,
    pub reason: CanonicalText,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct WorktreeNomination {
    pub id: WorktreeId,
    pub repository: RepositoryId,
    pub product: ProductId,
    pub selector: WorktreeSelector,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WorktreeSelector {
    ManifestRepositoryRoot,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ConstellationManifest {
    pub schema: CanonicalText,
    pub repositories: Vec<RepositoryDecl>,
    pub products: Vec<ProductDecl>,
    pub workspaces: Vec<WorkspaceDecl>,
    pub contexts: Vec<ContextDecl>,
    pub package_faces: Vec<PackageFaceDecl>,
    pub documentation: Vec<DocumentationDecl>,
    pub exclusions: Vec<ExclusionDecl>,
    pub worktree_nominations: Vec<WorktreeNomination>,
}

#[derive(Clone, Debug)]
pub struct ResolvedRepository {
    pub id: RepositoryId,
    pub root: PathBuf,
}

pub struct LoadedManifest {
    pub manifest: ConstellationManifest,
    pub hash: crate::architecture::schema::Sha256Digest,
}

pub(crate) fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace parent")
        .to_path_buf()
}

impl ConstellationManifest {
    pub fn load(path: &Path) -> Result<Self> {
        Ok(Self::load_bound(path)?.manifest)
    }

    pub fn load_bound(path: &Path) -> Result<LoadedManifest> {
        crate::architecture::evidence::write::failpoint("manifest-read")?;
        let bytes =
            std::fs::read(path).with_context(|| format!("read manifest `{}`", path.display()))?;
        let text = std::str::from_utf8(&bytes).context("constellation manifest is not UTF-8")?;
        let mut manifest: Self = toml::from_str(text).context("parse constellation manifest")?;
        manifest.canonicalize();
        manifest.validate()?;
        Ok(LoadedManifest {
            manifest,
            hash: crate::architecture::schema::Sha256Digest::of(&bytes),
        })
    }

    pub fn canonicalize(&mut self) {
        self.repositories.sort();
        self.products.sort();
        self.workspaces.sort();
        self.contexts.sort();
        self.package_faces.sort();
        self.documentation.sort();
        self.exclusions.sort();
        self.worktree_nominations.sort();
        for product in &mut self.products {
            product.workspaces.sort();
            product.workspaces.dedup();
            product.package_faces.sort();
            product.package_faces.dedup();
        }
        for context in &mut self.contexts {
            context.package_faces.sort();
            context.package_faces.dedup();
            context.config_inputs.sort();
            context.config_inputs.dedup();
        }
        for workspace in &mut self.workspaces {
            workspace.package_faces.sort();
            workspace.package_faces.dedup();
        }
    }

    pub fn validate(&self) -> Result<()> {
        if self.schema.as_str() != "bbnf.architecture.constellation.v3.1" {
            bail!(
                "unsupported constellation manifest schema `{}`",
                self.schema
            );
        }
        let repositories = unique("repository", self.repositories.iter().map(|item| &item.id))?;
        let expected = BTreeSet::from([
            RepositoryId::new("bbnf")?,
            RepositoryId::new("parse-that")?,
            RepositoryId::new("pprint")?,
        ]);
        if repositories != expected {
            bail!("manifest must name exactly the three physical repositories");
        }
        let products = unique("product", self.products.iter().map(|item| &item.id))?;
        let workspaces = unique("workspace", self.workspaces.iter().map(|item| &item.id))?;
        let faces = unique(
            "package face",
            self.package_faces.iter().map(|item| &item.id),
        )?;
        unique("context", self.contexts.iter().map(|item| &item.id))?;
        let docs = unique(
            "documentation",
            self.documentation.iter().map(|item| &item.id),
        )?;
        unique("exclusion", self.exclusions.iter().map(|item| &item.id))?;
        unique(
            "worktree nomination",
            self.worktree_nominations.iter().map(|item| &item.id),
        )?;
        let expected_contexts = BTreeMap::from([
            (ContextId::new("cargo-bbnf-root")?, ContextKind::Cargo),
            (ContextId::new("cargo-bbnf-skinny")?, ContextKind::Cargo),
            (ContextId::new("cargo-bbnf-wasm")?, ContextKind::Cargo),
            (ContextId::new("cargo-parse-that-rust")?, ContextKind::Cargo),
            (ContextId::new("frontend-bbnf-root")?, ContextKind::Frontend),
            (
                ContextId::new("frontend-parse-that-typescript")?,
                ContextKind::Frontend,
            ),
        ]);
        let actual_contexts = self
            .contexts
            .iter()
            .map(|context| (context.id.clone(), context.kind))
            .collect::<BTreeMap<_, _>>();
        if actual_contexts != expected_contexts {
            bail!(
                "CONTEXT_ID_KIND_MISMATCH: manifest must name the exact four Cargo and two frontend context pairs"
            );
        }
        for workspace in &self.workspaces {
            require(&repositories, &workspace.repository, "workspace repository")?;
        }
        let workspace_map = self.workspace_map();
        let face_map = self
            .package_faces
            .iter()
            .map(|item| (item.id.clone(), item))
            .collect::<BTreeMap<_, _>>();
        let product_map = self
            .products
            .iter()
            .map(|item| (item.id.clone(), item))
            .collect::<BTreeMap<_, _>>();
        let mut workspace_owners = BTreeMap::<WorkspaceId, ProductId>::new();
        let mut face_owners = BTreeMap::<PackageFaceId, ProductId>::new();
        for product in &self.products {
            require(&repositories, &product.repository, "product repository")?;
            for workspace in &product.workspaces {
                require(&workspaces, workspace, "product workspace")?;
                let declaration = workspace_map[workspace];
                if declaration.repository != product.repository {
                    bail!(
                        "PRODUCT_WORKSPACE_REPOSITORY_MISMATCH: product `{}` crosses repository ownership",
                        product.id
                    );
                }
                if workspace_owners
                    .insert(workspace.clone(), product.id.clone())
                    .is_some()
                {
                    bail!(
                        "WORKSPACE_RECIPROCITY_MISMATCH: workspace `{workspace}` has multiple products"
                    );
                }
            }
            for face in &product.package_faces {
                require(&faces, face, "product package face")?;
                let declaration = face_map[face];
                if declaration.product != product.id || declaration.repository != product.repository
                {
                    bail!(
                        "FACE_RECIPROCITY_MISMATCH: product `{}` and face `{face}` disagree",
                        product.id
                    );
                }
                if face_owners
                    .insert(face.clone(), product.id.clone())
                    .is_some()
                {
                    bail!("FACE_RECIPROCITY_MISMATCH: face `{face}` has multiple products");
                }
            }
        }
        if workspace_owners.len() != self.workspaces.len() {
            bail!("WORKSPACE_RECIPROCITY_MISMATCH: every workspace must have exactly one product");
        }
        if face_owners.len() != self.package_faces.len() {
            bail!("FACE_RECIPROCITY_MISMATCH: every package face must have exactly one product");
        }
        let mut workspace_face_owners = BTreeMap::<PackageFaceId, WorkspaceId>::new();
        for workspace in &self.workspaces {
            for face_id in &workspace.package_faces {
                require(&faces, face_id, "workspace package face")?;
                let face = face_map[face_id];
                if face.workspace != workspace.id || face.repository != workspace.repository {
                    bail!(
                        "WORKSPACE_FACE_RECIPROCITY_MISMATCH: workspace `{}` and face `{face_id}` disagree",
                        workspace.id
                    );
                }
                if workspace_face_owners
                    .insert(face_id.clone(), workspace.id.clone())
                    .is_some()
                {
                    bail!(
                        "WORKSPACE_FACE_RECIPROCITY_MISMATCH: face `{face_id}` has multiple workspaces"
                    );
                }
            }
        }
        if workspace_face_owners.len() != self.package_faces.len() {
            bail!(
                "WORKSPACE_FACE_RECIPROCITY_MISMATCH: every package face must have exactly one workspace"
            );
        }
        for context in &self.contexts {
            require(&workspaces, &context.workspace, "context workspace")?;
            require(&repositories, &context.repository, "context repository")?;
            let workspace = workspace_map[&context.workspace];
            if workspace.repository != context.repository {
                bail!(
                    "CONTEXT_REPOSITORY_MISMATCH: context `{}` crosses repository ownership",
                    context.id
                );
            }
            if context.package_faces.is_empty() {
                bail!(
                    "CONTEXT_FACE_RECIPROCITY_MISMATCH: context `{}` has no package face",
                    context.id
                );
            }
            let mut owns_manifest = false;
            for face_id in &context.package_faces {
                require(&faces, face_id, "context package face")?;
                let face = face_map[face_id];
                if face.repository != context.repository || face.workspace != context.workspace {
                    bail!(
                        "CONTEXT_FACE_RECIPROCITY_MISMATCH: context `{}` crosses face ownership",
                        context.id
                    );
                }
                owns_manifest |= face.manifest == context.manifest;
                let expected_kind = match context.kind {
                    ContextKind::Cargo => PackageFaceKind::Rust,
                    ContextKind::Frontend => PackageFaceKind::Frontend,
                };
                if face.kind != expected_kind {
                    bail!(
                        "CONTEXT_FACE_KIND_MISMATCH: context `{}` names the wrong package-face kind",
                        context.id
                    );
                }
            }
            if !owns_manifest {
                bail!(
                    "CONTEXT_MANIFEST_ROLE_MISMATCH: context `{}` manifest is not one of its package faces",
                    context.id
                );
            }
        }
        for face in &self.package_faces {
            require(&repositories, &face.repository, "face repository")?;
            require(&products, &face.product, "face product")?;
            require(&workspaces, &face.workspace, "face workspace")?;
            let workspace = self
                .workspaces
                .iter()
                .find(|item| item.id == face.workspace)
                .context("face workspace disappeared")?;
            if workspace.repository != face.repository {
                bail!(
                    "INVENTORY_SCOPE_FACE_MISMATCH: face `{}` splits physical repository identity",
                    face.id
                );
            }
            if !workspace.package_faces.contains(&face.id) {
                bail!(
                    "WORKSPACE_FACE_RECIPROCITY_MISMATCH: face `{}` is absent from workspace `{}`",
                    face.id,
                    workspace.id
                );
            }
            let product = product_map[&face.product];
            if product.repository != face.repository
                || !product.workspaces.contains(&face.workspace)
                || !product.package_faces.contains(&face.id)
            {
                bail!(
                    "FACE_RECIPROCITY_MISMATCH: face `{}` disagrees with product ownership",
                    face.id
                );
            }
        }
        for document in &self.documentation {
            require(
                &repositories,
                &document.repository,
                "documentation repository",
            )?;
            if let Some(authority) = &document.superseded_by {
                require(&docs, authority, "documentation supersession")?;
                let authority = self
                    .documentation
                    .iter()
                    .find(|item| &item.id == authority)
                    .context("documentation authority disappeared")?;
                if authority.repository != document.repository
                    || authority.role == DocumentationRole::SupersededEvidence
                {
                    bail!(
                        "DOCUMENTATION_SUPERSESSION_INVALID: `{}` has an invalid authority",
                        document.id
                    );
                }
            } else if document.role == DocumentationRole::SupersededEvidence {
                bail!(
                    "DOCUMENTATION_SUPERSESSION_INVALID: superseded evidence `{}` has no authority",
                    document.id
                );
            }
        }
        for exclusion in &self.exclusions {
            require(&repositories, &exclusion.repository, "exclusion repository")?;
            validate_exclusion_pattern(exclusion.pattern.as_str())?;
        }
        let mut nominated_products = BTreeSet::new();
        for nomination in &self.worktree_nominations {
            require(&repositories, &nomination.repository, "worktree repository")?;
            require(&products, &nomination.product, "worktree product")?;
            if product_map[&nomination.product].repository != nomination.repository {
                bail!(
                    "NOMINATION_REPOSITORY_MISMATCH: nomination `{}` crosses product ownership",
                    nomination.id
                );
            }
            if !nominated_products.insert(nomination.product.clone()) {
                bail!(
                    "NOMINATION_CARDINALITY_MISMATCH: product `{}` has multiple nomination declarations",
                    nomination.product
                );
            }
        }
        if nominated_products != products {
            bail!(
                "NOMINATION_CARDINALITY_MISMATCH: every product requires exactly one nomination declaration"
            );
        }
        validate_serialized_strings(self)
    }

    pub fn resolve_repositories(&self, workspace_root: &Path) -> Result<Vec<ResolvedRepository>> {
        let parent = workspace_root
            .parent()
            .context("workspace root has no parent")?;
        let mut resolved = Vec::new();
        for repository in &self.repositories {
            let root = match repository.discovery {
                RepositoryDiscovery::WorkspaceRoot => workspace_root.to_path_buf(),
                RepositoryDiscovery::Sibling => parent.join(
                    repository
                        .sibling_name
                        .as_ref()
                        .context("sibling repository omitted sibling_name")?
                        .as_str(),
                ),
            };
            if !root.is_dir() {
                bail!("declared repository `{}` is unavailable", repository.id);
            }
            let root = std::fs::canonicalize(&root)
                .with_context(|| format!("canonicalize repository `{}`", repository.id))?;
            resolved.push(ResolvedRepository {
                id: repository.id.clone(),
                root,
            });
        }
        resolved.sort_by(|left, right| left.id.cmp(&right.id));
        let mut roots = BTreeSet::new();
        for repository in &resolved {
            if !roots.insert(repository.root.clone()) {
                bail!("REPOSITORY_ROOT_ALIAS: canonical repository roots must be distinct");
            }
        }
        Ok(resolved)
    }

    pub fn workspace_map(&self) -> BTreeMap<WorkspaceId, &WorkspaceDecl> {
        self.workspaces
            .iter()
            .map(|item| (item.id.clone(), item))
            .collect()
    }
}

fn validate_exclusion_pattern(pattern: &str) -> Result<()> {
    let prefix = pattern.strip_suffix("/**").ok_or_else(|| {
        anyhow::anyhow!("EXCLUSION_PATTERN_UNSUPPORTED: exclusions must end in `/**`")
    })?;
    let any_segment = prefix.strip_prefix("**/");
    let candidate = any_segment.unwrap_or(prefix);
    let invalid_segment = |segment: &str| {
        segment.is_empty()
            || segment == "."
            || segment == ".."
            || segment.contains('*')
            || segment.contains('?')
            || segment.contains('[')
            || segment.contains(']')
            || segment.contains('\\')
    };
    if candidate.split('/').any(invalid_segment)
        || (any_segment.is_some() && candidate.contains('/'))
    {
        bail!(
            "EXCLUSION_PATTERN_UNSUPPORTED: `{pattern}` is outside the directory exclusion grammar"
        );
    }
    Ok(())
}

fn unique<'a, T>(kind: &str, values: impl Iterator<Item = &'a T>) -> Result<BTreeSet<T>>
where
    T: Clone + Ord + std::fmt::Display + 'a,
{
    let mut set = BTreeSet::new();
    for value in values {
        if !set.insert(value.clone()) {
            bail!("duplicate {kind} identity `{value}`");
        }
    }
    Ok(set)
}

fn require<T: Ord + std::fmt::Display>(set: &BTreeSet<T>, value: &T, kind: &str) -> Result<()> {
    if !set.contains(value) {
        bail!("absent {kind} link `{value}`");
    }
    Ok(())
}
