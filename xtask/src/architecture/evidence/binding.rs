use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsString;
use std::io::{Read as _, Write as _};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::architecture::evidence::write::{RunEpoch, is_injected_failure};
use crate::architecture::inventory::{
    CollectorInputBinding, Inventory, ResolvedNomination, WorktreeHashTranscript, WorktreeRecord,
};
#[cfg(feature = "architecture-test-seams")]
use crate::architecture::inventory::{collect_worktree_census_with_git, resolve_nominations};
use crate::architecture::manifest::{ConstellationManifest, ContextKind, ResolvedRepository};
use crate::architecture::schema::{
    CanonicalText, ContextId, PackageFaceId, RepoPath, RepositoryId, Sha256Digest, WorkspaceId,
    validate_serialized_strings,
};

#[cfg(target_os = "macos")]
mod apple;

const BUILD_TOOL_NAMES: [&str; 6] = ["ar", "cargo", "cc", "ld", "rustc", "sccache"];
const LIVE_TOOL_NAMES: [&str; 8] = ["ar", "cargo", "cc", "git", "ld", "node", "rustc", "sccache"];
const REQUIRED_TOOL_PROBES: [(&str, &[&str]); 8] = [
    ("ar", &["--version"]),
    ("cargo", &["--version", "--verbose"]),
    ("rustc", &["--version", "--verbose"]),
    ("node", &["--version"]),
    ("git", &["--version"]),
    ("sccache", &["--version"]),
    ("cc", &["--version"]),
    ("ld", &["-v"]),
];

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RepositoryBinding {
    pub repository: RepositoryId,
    pub inventory_hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ContextBinding {
    pub context: ContextId,
    pub kind: ContextKind,
    pub repository: RepositoryId,
    pub workspace: WorkspaceId,
    pub package_faces: Vec<PackageFaceId>,
    pub manifest: RepoPath,
    pub manifest_hash: Option<Sha256Digest>,
    pub lock: RepoPath,
    pub lock_hash: Option<Sha256Digest>,
    pub config_inputs: Vec<ConfigBinding>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ConfigBinding {
    pub repository: RepositoryId,
    pub path: RepoPath,
    pub hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ToolBinding {
    pub tool: CanonicalText,
    pub invocation_name: CanonicalText,
    pub invocation_path_hash: Sha256Digest,
    pub proxy_hash: Sha256Digest,
    pub executable_hash: Sha256Digest,
    pub capability: CanonicalText,
    pub capability_hash: Sha256Digest,
    pub argv: Vec<CanonicalText>,
    #[serde(rename = "environment")]
    pub execution_contract: Vec<CanonicalText>,
    pub effective_path_hash: Sha256Digest,
    pub toolchain_selection: CanonicalText,
    pub toolchain_selection_hash: Sha256Digest,
    pub dynamic_runtime_hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceMember {
    pub path: RepoPath,
    pub size: u64,
    pub hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceCensus {
    pub schema: CanonicalText,
    pub members: Vec<SourceMember>,
    pub path_set_hash: Sha256Digest,
    pub ledger_hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BuildInputMember {
    pub repository: CanonicalText,
    pub path: RepoPath,
    pub size: u64,
    pub hash: Sha256Digest,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BuildInputCensus {
    pub schema: CanonicalText,
    pub members: Vec<BuildInputMember>,
    pub path_set_hash: Sha256Digest,
    pub ledger_hash: Sha256Digest,
}

impl Default for SourceCensus {
    fn default() -> Self {
        Self {
            schema: CanonicalText::new("bbnf.architecture.source-census.v3.1")
                .expect("static schema"),
            members: Vec::new(),
            path_set_hash: Sha256Digest::of(b""),
            ledger_hash: Sha256Digest::of(b""),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct BuildReceipt {
    pub schema: CanonicalText,
    pub exporter_hash: Sha256Digest,
    pub exporter_argv_hash: Sha256Digest,
    #[serde(rename = "exporter_environment_hash")]
    pub exporter_execution_contract_hash: Sha256Digest,
    pub source_path_set_hash: Sha256Digest,
    pub source_ledger_hash: Sha256Digest,
    pub embedded_source_ledger_hash: Sha256Digest,
    pub local_inputs: BuildInputCensus,
    pub embedded_local_input_ledger_hash: Sha256Digest,
    pub build_argv: Vec<CanonicalText>,
    pub build_feature_graph_hash: Sha256Digest,
    pub build_profile: CanonicalText,
    pub build_environment_hash: Sha256Digest,
    pub registry_source_policy: CanonicalText,
    pub build_tool_ledger_hash: Sha256Digest,
    pub embedded_build_tool_ledger_hash: Sha256Digest,
    pub cargo_payload_hash: Sha256Digest,
    pub rustc_payload_hash: Sha256Digest,
    pub wrapper_payload_hash: Sha256Digest,
    pub linker_driver_payload_hash: Sha256Digest,
    pub linker_payload_hash: Sha256Digest,
    pub archiver_payload_hash: Sha256Digest,
    pub toolchain_selection_hash: Sha256Digest,
    pub dynamic_runtime_hash: Sha256Digest,
    pub host_facts: Vec<CanonicalText>,
    pub host_hash: Sha256Digest,
}

#[derive(Clone, Debug)]
pub struct ToolSelection {
    bindings: Vec<ToolBinding>,
    paths: BTreeMap<String, PathBuf>,
    #[cfg(target_os = "macos")]
    apple: std::sync::Arc<apple::AppleArmCaptureContext>,
}

impl ToolSelection {
    pub fn capture() -> Result<Self> {
        match embedded_build_tool_paths()? {
            Some(paths) => Self::capture_embedded_build(&paths),
            None => Self::capture_ordinary(),
        }
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn capture_with_paths_for_test(paths: &BTreeMap<String, PathBuf>) -> Result<Self> {
        Self::capture_explicit_ordinary_for_test(paths)
    }

    fn capture_ordinary() -> Result<Self> {
        Self::capture_with_validated_paths(None)
    }

    fn capture_embedded_build(paths: &BTreeMap<String, PathBuf>) -> Result<Self> {
        validate_exact_build_tool_paths(paths)?;
        Self::capture_with_validated_paths(Some(paths))
    }

    #[cfg(feature = "architecture-test-seams")]
    fn capture_explicit_ordinary_for_test(paths: &BTreeMap<String, PathBuf>) -> Result<Self> {
        if !has_exact_names(paths, &LIVE_TOOL_NAMES) {
            anyhow::bail!("explicit ordinary capture received a different path set");
        }
        Self::capture_with_validated_paths(Some(paths))
    }

    fn capture_with_validated_paths(
        validated_paths: Option<&BTreeMap<String, PathBuf>>,
    ) -> Result<Self> {
        let mut bindings = Vec::new();
        let mut paths = BTreeMap::new();
        let epoch = RunEpoch::current()?;
        #[cfg(target_os = "macos")]
        let apple = apple::AppleArmCaptureContext::capture()?;
        let captured = run_required_tool_workers(&epoch, |tool, args| {
            let result = match validated_paths.and_then(|paths| paths.get(tool)) {
                Some(path) => tool_binding_at(
                    tool,
                    path,
                    args,
                    true,
                    #[cfg(target_os = "macos")]
                    &apple,
                ),
                None => tool_binding(
                    tool,
                    args,
                    true,
                    #[cfg(target_os = "macos")]
                    &apple,
                ),
            }?;
            Ok((tool, result))
        })?;
        #[cfg(target_os = "macos")]
        apple.verify_payloads_unchanged()?;
        for (tool, (binding, path)) in captured {
            bindings.push(binding);
            paths.insert(tool.to_string(), path);
        }
        paths.insert(
            "xtask-exporter".to_string(),
            std::env::current_exe().context("resolve executing exporter")?,
        );
        let mut selection = Self {
            bindings,
            paths,
            #[cfg(target_os = "macos")]
            apple,
        };
        let exporter = selection.capture_exporter_binding()?;
        selection.bindings.push(exporter);
        selection.bindings.sort();
        Ok(selection)
    }

    pub fn path(&self, tool: &str) -> Result<&Path> {
        self.paths
            .get(tool)
            .map(PathBuf::as_path)
            .with_context(|| format!("selected tool '{tool}' is unavailable"))
    }

    fn capture_exporter_binding(&self) -> Result<ToolBinding> {
        exporter_binding(
            self.path("xtask-exporter")?,
            #[cfg(target_os = "macos")]
            &self.apple,
        )
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn recapture_exporter(&self) -> Result<ToolBinding> {
        self.capture_exporter_binding()
    }

    fn build_tool_ledger_hash_inner(&self) -> Result<Sha256Digest> {
        build_tool_binding_hash(&self.bindings)
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn build_tool_ledger_hash(&self) -> Result<Sha256Digest> {
        self.build_tool_ledger_hash_inner()
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn replace_tool_for_counterexample(
        mut self,
        tool: &str,
        executable: &Path,
        args: &[&str],
    ) -> Result<Self> {
        let (binding, path) = tool_binding_at(
            tool,
            executable,
            args,
            true,
            #[cfg(target_os = "macos")]
            &self.apple,
        )?;
        let existing = self
            .bindings
            .iter_mut()
            .find(|candidate| candidate.tool.as_str() == tool)
            .with_context(|| format!("selected tool '{tool}' is unavailable"))?;
        *existing = binding;
        self.bindings.sort();
        self.paths.insert(tool.to_string(), path);
        Ok(self)
    }
}

/// Capture the binding-format hash for the exact build-tool path set used by
/// a hermetic build premise.
///
/// The map must contain exactly `ar`, `cargo`, `cc`, `ld`, `rustc`, and
/// `sccache`, each at an absolute path. Every [`ToolBinding`] field is
/// recaptured from those paths before the canonical ledger hash is computed.
pub fn capture_build_tool_binding_hash(paths: &BTreeMap<String, PathBuf>) -> Result<Sha256Digest> {
    let selection = capture_build_tool_selection(paths)?;
    Sha256Digest::new(crate::architecture::hash::hash_serialized(
        &selection.bindings,
    )?)
}

/// Recapture the exact three-repository build-input census using the selected
/// absolute build-tool paths.
pub fn capture_build_input_census(
    build_tool_paths: &BTreeMap<String, PathBuf>,
    repository_roots: &BTreeMap<String, PathBuf>,
) -> Result<BuildInputCensus> {
    validate_named_repository_roots(repository_roots)?;
    let selection = capture_build_tool_selection(build_tool_paths)?;
    build_input_census_at(
        &selection,
        &repository_roots["bbnf"],
        &repository_roots["parse-that"],
        &repository_roots["pprint"],
    )
}

/// Materialize a create-new immutable source snapshot from an exact typed
/// build-input census.
///
/// Every source member is read once, rejected unless it is a regular
/// non-symlink file with the retained size and hash, then written create-new
/// below exact `bbnf`, `parse-that`, and `pprint` sibling roots.
pub fn materialize_build_input_snapshot(
    census: &BuildInputCensus,
    repository_roots: &BTreeMap<String, PathBuf>,
    snapshot_parent: &Path,
) -> Result<BTreeMap<String, PathBuf>> {
    validate_named_repository_roots(repository_roots)?;
    let (path_set_hash, ledger_hash) = validate_build_input_census(census)?;
    if census.path_set_hash != path_set_hash || census.ledger_hash != ledger_hash {
        anyhow::bail!("BUILD_INPUT_SNAPSHOT_INVALID: census hashes differ");
    }
    if !snapshot_parent.is_absolute() || !snapshot_parent.is_dir() {
        anyhow::bail!("BUILD_INPUT_SNAPSHOT_INVALID: parent is not an absolute directory");
    }
    let snapshot_roots = ["bbnf", "parse-that", "pprint"]
        .into_iter()
        .map(|name| (name.to_owned(), snapshot_parent.join(name)))
        .collect::<BTreeMap<_, _>>();
    for root in snapshot_roots.values() {
        if root.exists() || std::fs::symlink_metadata(root).is_ok() {
            anyhow::bail!("BUILD_INPUT_SNAPSHOT_INVALID: destination already exists");
        }
        std::fs::create_dir(root)?;
    }
    for member in &census.members {
        let source_root = repository_roots
            .get(member.repository.as_str())
            .context("BUILD_INPUT_SNAPSHOT_INVALID: member repository is unavailable")?;
        let destination_root = snapshot_roots
            .get(member.repository.as_str())
            .context("BUILD_INPUT_SNAPSHOT_INVALID: destination repository is unavailable")?;
        let source = source_root.join(member.path.as_str());
        let metadata = std::fs::symlink_metadata(&source).with_context(|| {
            format!(
                "BUILD_INPUT_SNAPSHOT_INVALID: source `{}` is unavailable",
                source.display()
            )
        })?;
        if !metadata.is_file() || metadata.file_type().is_symlink() {
            anyhow::bail!("BUILD_INPUT_SNAPSHOT_INVALID: source member is not a regular file");
        }
        let bytes = std::fs::read(&source)?;
        if u64::try_from(bytes.len())? != member.size || Sha256Digest::of(&bytes) != member.hash {
            anyhow::bail!("BUILD_INPUT_SNAPSHOT_INVALID: source member bytes differ");
        }
        let destination = destination_root.join(member.path.as_str());
        let parent = destination
            .parent()
            .context("BUILD_INPUT_SNAPSHOT_INVALID: destination has no parent")?;
        std::fs::create_dir_all(parent)?;
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&destination)?;
        file.write_all(&bytes)?;
        file.sync_all()?;
    }
    Ok(snapshot_roots)
}

fn capture_build_tool_selection(paths: &BTreeMap<String, PathBuf>) -> Result<ToolSelection> {
    validate_exact_build_tool_paths(paths)?;
    capture_validated_tool_selection(paths)
}

fn validate_exact_build_tool_paths(paths: &BTreeMap<String, PathBuf>) -> Result<()> {
    if !has_exact_names(paths, &BUILD_TOOL_NAMES) || paths.values().any(|path| !path.is_absolute())
    {
        anyhow::bail!(
            "exact build-tool capture requires absolute ar/cargo/cc/ld/rustc/sccache paths"
        );
    }
    for (tool, path) in paths {
        validate_exact_rust_toolchain_path(tool, path)?;
    }
    Ok(())
}

fn capture_validated_tool_selection(paths: &BTreeMap<String, PathBuf>) -> Result<ToolSelection> {
    #[cfg(target_os = "macos")]
    let apple = apple::AppleArmCaptureContext::capture()?;
    let bindings = paths
        .iter()
        .map(|(tool, path)| {
            let (binding, _) = tool_binding_at(
                tool,
                path,
                required_tool_args(tool)?,
                false,
                #[cfg(target_os = "macos")]
                &apple,
            )?;
            Ok(binding)
        })
        .collect::<Result<Vec<_>>>()?;
    #[cfg(target_os = "macos")]
    apple.verify_payloads_unchanged()?;
    Ok(ToolSelection {
        bindings,
        paths: paths.clone(),
        #[cfg(target_os = "macos")]
        apple,
    })
}

fn embedded_build_tool_paths() -> Result<Option<BTreeMap<String, PathBuf>>> {
    let Some(json) = option_env!("BBNF_EXPORTER_BUILD_TOOL_PATHS_JSON") else {
        return Ok(None);
    };
    let encoded = serde_json::from_str::<BTreeMap<String, String>>(json)
        .context("EXPORTER_BUILD_RECEIPT_UNBOUND: decode exact build-tool paths")?;
    if serde_json::to_string(&encoded)? != json {
        anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: exact build-tool paths are not canonical JSON"
        );
    }
    let paths = encoded
        .into_iter()
        .map(|(name, path)| (name, PathBuf::from(path)))
        .collect::<BTreeMap<_, _>>();
    Ok(Some(paths))
}

fn required_tool_args(tool: &str) -> Result<&'static [&'static str]> {
    REQUIRED_TOOL_PROBES
        .iter()
        .find_map(|(name, args)| (*name == tool).then_some(*args))
        .with_context(|| format!("required tool '{tool}' has no capability probe"))
}

fn validate_exact_rust_toolchain_path(tool: &str, path: &Path) -> Result<()> {
    if !matches!(tool, "cargo" | "rustc") {
        return Ok(());
    }
    let rustup = resolve_executable("rustup")?;
    let output = crate::architecture::canonical_child_command(&rustup)
        .args(["which", tool])
        .output()
        .with_context(|| format!("resolve exact rustup-selected {tool}"))?;
    if !output.status.success() {
        anyhow::bail!("exact rustup-selected `{tool}` is unavailable");
    }
    let selected = std::str::from_utf8(&output.stdout)
        .context("rustup-selected build-tool path is not UTF-8")?
        .trim();
    let canonical_path = std::fs::canonicalize(path)?;
    let canonical_selected = std::fs::canonicalize(selected)?;
    if path.as_os_str() != canonical_path.as_os_str()
        || canonical_path.as_os_str() != canonical_selected.as_os_str()
    {
        anyhow::bail!(
            "required `{tool}` invocation is not the exact rustup-selected toolchain payload"
        );
    }
    Ok(())
}

fn run_required_tool_workers<T, F>(epoch: &RunEpoch, worker: F) -> Result<Vec<T>>
where
    T: Send,
    F: Fn(&'static str, &'static [&'static str]) -> Result<T> + Sync,
{
    std::thread::scope(|scope| -> Result<Vec<_>> {
        let mut slots = Vec::with_capacity(REQUIRED_TOOL_PROBES.len());
        for (tool, args) in REQUIRED_TOOL_PROBES {
            let epoch = epoch.clone();
            let worker = &worker;
            match std::thread::Builder::new().spawn_scoped(scope, move || {
                let _epoch = epoch.install()?;
                worker(tool, args)
            }) {
                Ok(handle) => slots.push(Ok(handle)),
                Err(error) => slots.push(Err((tool, error))),
            }
        }

        let mut outcomes = Vec::with_capacity(slots.len());
        for slot in slots {
            outcomes.push(match slot {
                Ok(handle) => Ok(handle.join()),
                Err((tool, error)) => Err(anyhow::Error::new(error)
                    .context(format!("spawn required tool '{tool}' binding worker"))),
            });
        }

        let mut first_panic = None;
        let mut injected_error = None;
        let mut multiple_injected_errors = false;
        let mut first_natural_or_spawn_error = None;
        let mut captured = Vec::with_capacity(outcomes.len());
        for outcome in outcomes {
            match outcome {
                Err(error) => {
                    if first_natural_or_spawn_error.is_none() {
                        first_natural_or_spawn_error = Some(error);
                    }
                }
                Ok(Err(panic)) => {
                    if first_panic.is_none() {
                        first_panic = Some(panic);
                    }
                }
                Ok(Ok(Err(error))) if is_injected_failure(&error) => {
                    if injected_error.is_some() {
                        multiple_injected_errors = true;
                    } else {
                        injected_error = Some(error);
                    }
                }
                Ok(Ok(Err(error))) => {
                    if first_natural_or_spawn_error.is_none() {
                        first_natural_or_spawn_error = Some(error);
                    }
                }
                Ok(Ok(Ok(result))) => captured.push(result),
            }
        }
        if let Some(panic) = first_panic {
            std::panic::resume_unwind(panic);
        }
        assert!(
            !multiple_injected_errors,
            "one run epoch produced multiple injected tool failures"
        );
        if let Some(error) = injected_error {
            return Err(error);
        }
        if let Some(error) = first_natural_or_spawn_error {
            return Err(error);
        }
        Ok(captured)
    })
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn exercise_required_tool_workers(
    epoch: &RunEpoch,
    worker: &(dyn Fn(&'static str) -> Result<()> + Sync),
) -> Result<Vec<&'static str>> {
    run_required_tool_workers(epoch, |tool, _args| {
        worker(tool)?;
        Ok(tool)
    })
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct HelperBinding {
    pub identity: CanonicalText,
    pub schema: CanonicalText,
    pub hash: Sha256Digest,
}

pub struct BindingCapture<'a> {
    pub workspace_root: &'a Path,
    pub manifest_hash: Sha256Digest,
    pub selection: ToolSelection,
    pub manifest: &'a ConstellationManifest,
    pub repositories: &'a [ResolvedRepository],
    pub inventory: &'a Inventory,
    pub collector_inputs: Vec<CollectorInputBinding>,
    pub worktrees: &'a [WorktreeRecord],
    pub worktree_hashing: WorktreeHashTranscript,
    pub nominations: &'a [ResolvedNomination],
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SourceBinding {
    pub schema: CanonicalText,
    pub complete: bool,
    pub unavailable: Vec<CanonicalText>,
    pub manifest_hash: Sha256Digest,
    pub repositories: Vec<RepositoryBinding>,
    pub contexts: Vec<ContextBinding>,
    pub collector_inputs: Vec<ConfigBinding>,
    pub tools: Vec<ToolBinding>,
    pub source: SourceCensus,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub build: Option<BuildReceipt>,
    pub helpers: Vec<HelperBinding>,
    pub worktrees: Vec<WorktreeRecord>,
    pub worktree_hashing: WorktreeHashTranscript,
    pub nominations: Vec<ResolvedNomination>,
    pub binding_hash: Sha256Digest,
    #[serde(skip, default)]
    helper_bytes: BTreeMap<String, Vec<u8>>,
    #[serde(skip, default)]
    tool_paths: BTreeMap<String, PathBuf>,
}

fn deserialize_required_option<'de, D, T>(
    deserializer: D,
) -> std::result::Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

impl SourceBinding {
    pub fn capture(capture: BindingCapture<'_>) -> Result<Self> {
        let BindingCapture {
            workspace_root,
            manifest_hash,
            selection,
            manifest,
            repositories,
            inventory,
            collector_inputs,
            worktrees,
            worktree_hashing,
            nominations,
        } = capture;
        let mut repository_bindings = repositories
            .iter()
            .map(|repository| {
                Ok(RepositoryBinding {
                    repository: repository.id.clone(),
                    inventory_hash: inventory.repository_hash(&repository.id)?,
                })
            })
            .collect::<Result<Vec<_>>>()?;
        repository_bindings.sort();
        let mut contexts = Vec::new();
        for context in &manifest.contexts {
            let manifest_record = inventory.find(&context.repository, &context.manifest);
            let lock_record = inventory.find(&context.repository, &context.lock);
            let mut config_inputs = context
                .config_inputs
                .iter()
                .filter_map(|path| {
                    inventory
                        .find(&context.repository, path)
                        .and_then(|record| record.content_hash.clone())
                        .map(|hash| ConfigBinding {
                            repository: context.repository.clone(),
                            path: path.clone(),
                            hash,
                        })
                })
                .collect::<Vec<_>>();
            config_inputs.sort();
            contexts.push(ContextBinding {
                context: context.id.clone(),
                kind: context.kind,
                repository: context.repository.clone(),
                workspace: context.workspace.clone(),
                package_faces: context.package_faces.clone(),
                manifest: context.manifest.clone(),
                manifest_hash: manifest_record.and_then(|record| record.content_hash.clone()),
                lock: context.lock.clone(),
                lock_hash: lock_record.and_then(|record| record.content_hash.clone()),
                config_inputs,
            });
        }
        contexts.sort();
        let collector_inputs = validate_collector_bindings(collector_inputs, inventory)?;
        let exporter_source_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .context("xtask source root")?;
        let source = source_census(exporter_source_root)?;
        let build = Some(build_receipt(&selection, &source)?);
        let tools = selection.bindings;
        let tool_paths = selection.paths;
        let mut helpers = Vec::new();
        let mut helper_bytes = BTreeMap::new();
        for (relative, schema) in [
            (
                "xtask/architecture/frontend/program.mjs",
                "bbnf.architecture.frontend-boundary.v3.1",
            ),
            (
                "xtask/architecture/frontend/vue.mjs",
                "bbnf.architecture.frontend-vue-helper.v3.1",
            ),
            (
                "xtask/architecture/frontend/css.mjs",
                "bbnf.architecture.frontend-css-helper.v3.1",
            ),
        ] {
            let bytes = std::fs::read(workspace_root.join(relative))?;
            helpers.push(HelperBinding {
                identity: CanonicalText::new(relative)?,
                schema: CanonicalText::new(schema)?,
                hash: Sha256Digest::of(&bytes),
            });
            helper_bytes.insert(relative.to_string(), bytes);
        }
        for (identity, version) in [
            ("resolver:cargo-metadata", "0.18"),
            ("resolver:syn", "2"),
            ("projection-kernel", "3"),
            ("sha256-kernel", "fips-180-4"),
        ] {
            helpers.push(HelperBinding {
                identity: CanonicalText::new(identity)?,
                schema: CanonicalText::new("bbnf.architecture.bound-kernel.v3.1")?,
                hash: Sha256Digest::of(version.as_bytes()),
            });
        }
        helpers.sort();
        let mut unavailable = Vec::new();
        for context in &contexts {
            if context.manifest_hash.is_none() {
                unavailable.push(CanonicalText::new(format!(
                    "context:{}:manifest",
                    context.context
                ))?);
            }
            if context.lock_hash.is_none() {
                unavailable.push(CanonicalText::new(format!(
                    "context:{}:lock",
                    context.context
                ))?);
            }
            let declared = manifest
                .contexts
                .iter()
                .find(|item| item.id == context.context)
                .expect("context declaration");
            if context.config_inputs.len() != declared.config_inputs.len() {
                unavailable.push(CanonicalText::new(format!(
                    "context:{}:config-inputs",
                    context.context
                ))?);
            }
        }
        for repository in repositories {
            if !worktrees
                .iter()
                .any(|worktree| worktree.repository == repository.id)
            {
                unavailable.push(CanonicalText::new(format!(
                    "repository:{}:worktrees",
                    repository.id
                ))?);
            }
        }
        if nominations.len() != manifest.worktree_nominations.len()
            || manifest.worktree_nominations.iter().any(|declaration| {
                !nominations
                    .iter()
                    .any(|resolved| resolved.nomination == declaration.id)
            })
        {
            unavailable.push(CanonicalText::new("worktree-nominations")?);
        }
        unavailable.sort();
        unavailable.dedup();
        let mut binding = Self {
            schema: CanonicalText::new("bbnf.architecture.source-binding.v3.1")?,
            complete: unavailable.is_empty(),
            unavailable,
            manifest_hash,
            repositories: repository_bindings,
            contexts,
            collector_inputs,
            tools,
            source,
            build,
            helpers,
            worktrees: worktrees.to_vec(),
            worktree_hashing,
            nominations: nominations.to_vec(),
            binding_hash: Sha256Digest::of(b"pending"),
            helper_bytes,
            tool_paths,
        };
        binding.worktrees.sort();
        binding.nominations.sort();
        binding.binding_hash = binding.recomputed_hash()?;
        validate_serialized_strings(&binding)?;
        Ok(binding)
    }

    pub fn minimal(manifest_path: &Path) -> Result<Self> {
        let manifest_hash = if manifest_path.is_file() {
            Sha256Digest::of(&std::fs::read(manifest_path)?)
        } else {
            Sha256Digest::of(b"manifest-unavailable")
        };
        let mut binding = Self {
            schema: CanonicalText::new("bbnf.architecture.source-binding.v3.1")?,
            complete: false,
            unavailable: vec![
                CanonicalText::new("repositories")?,
                CanonicalText::new("contexts")?,
                CanonicalText::new("collector-inputs")?,
                CanonicalText::new("tools")?,
                CanonicalText::new("helpers")?,
                CanonicalText::new("worktrees")?,
            ],
            manifest_hash,
            repositories: Vec::new(),
            contexts: Vec::new(),
            collector_inputs: Vec::new(),
            tools: Vec::new(),
            source: SourceCensus::default(),
            build: None,
            helpers: Vec::new(),
            worktrees: Vec::new(),
            worktree_hashing: WorktreeHashTranscript::default(),
            nominations: Vec::new(),
            binding_hash: Sha256Digest::of(b"pending"),
            helper_bytes: BTreeMap::new(),
            tool_paths: BTreeMap::new(),
        };
        binding.binding_hash = binding.recomputed_hash()?;
        Ok(binding)
    }

    pub(crate) fn recomputed_hash(&self) -> Result<Sha256Digest> {
        Sha256Digest::new(crate::architecture::hash::hash_serialized(&(
            &self.schema,
            self.complete,
            &self.unavailable,
            &self.manifest_hash,
            &self.repositories,
            &self.contexts,
            &self.collector_inputs,
            &self.tools,
            &self.source,
            &self.build,
            &self.helpers,
            &self.worktrees,
            &self.worktree_hashing,
            &self.nominations,
        ))?)
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn reseal_for_test(&mut self) -> Result<()> {
        self.binding_hash = self.recomputed_hash()?;
        Ok(())
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn validate_live_binding_planes_for_test(
        &self,
        workspace_root: &Path,
        repository_roots: &BTreeMap<String, PathBuf>,
        exporter_path: &Path,
    ) -> Result<()> {
        let tool_paths = retained_live_capture_inputs(&self.tool_paths)?;
        let live_selection = capture_live_tool_selection(&tool_paths)?;
        validate_live_binding_planes(
            self,
            workspace_root,
            repository_roots,
            &tool_paths,
            exporter_path,
            &live_selection,
            exact_tool(&self.tools, "xtask-exporter")?,
        )
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn validate_binding_planes_against_grounded_truth_for_test(
        &self,
        grounded_truth: &Self,
    ) -> Result<()> {
        validate_binding_plane_match(self, grounded_truth)
    }

    pub fn write_bound_helpers(&self, directory: &Path) -> Result<PathBuf> {
        std::fs::create_dir_all(directory)?;
        for (relative, name) in [
            ("xtask/architecture/frontend/program.mjs", "program.mjs"),
            ("xtask/architecture/frontend/vue.mjs", "vue.mjs"),
            ("xtask/architecture/frontend/css.mjs", "css.mjs"),
        ] {
            let bytes = self
                .helper_bytes
                .get(relative)
                .with_context(|| format!("bound helper '{relative}' is unavailable"))?;
            let binding = self
                .helpers
                .iter()
                .find(|binding| binding.identity.as_str() == relative)
                .context("helper binding is unavailable")?;
            if Sha256Digest::of(bytes) != binding.hash {
                anyhow::bail!("SOURCE_SNAPSHOT_DRIFT: bound helper bytes changed");
            }
            std::fs::write(directory.join(name), bytes)?;
        }
        Ok(directory.join("program.mjs"))
    }

    pub fn verified_tool_path(&self, tool: &str, args: &[&str]) -> Result<PathBuf> {
        let path = self
            .tool_paths
            .get(tool)
            .with_context(|| format!("bound tool '{tool}' is unavailable"))?;
        let expected = self
            .tools
            .iter()
            .find(|binding| binding.tool.as_str() == tool)
            .context("tool binding is unavailable")?;
        #[cfg(target_os = "macos")]
        let apple = apple::AppleArmCaptureContext::capture()?;
        let (actual, _) = tool_binding_at(
            tool,
            path,
            args,
            false,
            #[cfg(target_os = "macos")]
            &apple,
        )?;
        #[cfg(target_os = "macos")]
        apple.verify_payloads_unchanged()?;
        if &actual != expected {
            anyhow::bail!("SOURCE_SNAPSHOT_DRIFT: selected tool bytes or capabilities changed");
        }
        Ok(path.clone())
    }

    pub fn verify_recensus(&self, recensus: &Self) -> Result<()> {
        if recensus != self {
            anyhow::bail!("SOURCE_SNAPSHOT_DRIFT: source binding changed after capture");
        }
        Ok(())
    }
}

fn validate_named_repository_roots(roots: &BTreeMap<String, PathBuf>) -> Result<()> {
    let expected = ["bbnf", "parse-that", "pprint"]
        .into_iter()
        .collect::<BTreeSet<_>>();
    let actual = roots.keys().map(String::as_str).collect::<BTreeSet<_>>();
    if actual != expected || roots.len() != expected.len() {
        anyhow::bail!("repository roots must be exactly canonical bbnf/parse-that/pprint paths");
    }
    for (name, root) in roots {
        if !root.is_absolute() || std::fs::canonicalize(root)? != *root {
            anyhow::bail!("`{name}` repository root is not canonical");
        }
    }
    Ok(())
}

fn has_exact_names(paths: &BTreeMap<String, PathBuf>, names: &[&str]) -> bool {
    paths.len() == names.len() && paths.keys().map(String::as_str).eq(names.iter().copied())
}

#[cfg(feature = "architecture-test-seams")]
fn capture_live_tool_selection(paths: &BTreeMap<String, PathBuf>) -> Result<ToolSelection> {
    if !has_exact_names(paths, &LIVE_TOOL_NAMES) {
        anyhow::bail!("live tool capture received a different path set");
    }
    capture_validated_tool_selection(paths)
}

#[cfg(feature = "architecture-test-seams")]
fn retained_live_capture_inputs(
    retained: &BTreeMap<String, PathBuf>,
) -> Result<BTreeMap<String, PathBuf>> {
    let inputs = retained
        .iter()
        .filter(|(name, _)| name.as_str() != "xtask-exporter")
        .map(|(name, path)| (name.clone(), path.clone()))
        .collect::<BTreeMap<_, _>>();
    if !has_exact_names(&inputs, &LIVE_TOOL_NAMES) {
        anyhow::bail!("ASSAY_LIVE_CLOSURE_INVALID: retained capture-input map is incomplete");
    }
    Ok(inputs)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn live_capture_input_map_for_test(
    retained: &BTreeMap<String, PathBuf>,
    candidate: &BTreeMap<String, PathBuf>,
) -> Result<BTreeMap<String, PathBuf>> {
    let expected = retained_live_capture_inputs(retained)?;
    if candidate != &expected {
        anyhow::bail!("ASSAY_LIVE_CLOSURE_INVALID: capture-input substitution is forbidden");
    }
    Ok(expected)
}

#[cfg(feature = "architecture-test-seams")]
fn validate_live_binding_planes(
    retained: &SourceBinding,
    workspace_root: &Path,
    repository_roots: &BTreeMap<String, PathBuf>,
    tool_paths: &BTreeMap<String, PathBuf>,
    exporter_path: &Path,
    live_selection: &ToolSelection,
    retained_exporter: &ToolBinding,
) -> Result<()> {
    let manifest_path = workspace_root.join("xtask/architecture/constellation-v3.toml");
    let loaded = ConstellationManifest::load_bound(&manifest_path)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture constellation manifest")?;
    let repositories = loaded
        .manifest
        .resolve_repositories(workspace_root)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture repository roots")?;
    let actual_roots = repositories
        .iter()
        .map(|repository| (repository.id.as_str().to_owned(), repository.root.clone()))
        .collect::<BTreeMap<_, _>>();
    if actual_roots != *repository_roots {
        anyhow::bail!("ASSAY_LIVE_CLOSURE_INVALID: manifest-derived repository roots differ");
    }
    let git = &tool_paths["git"];
    let inventory = Inventory::collect_with_git(&loaded.manifest, &repositories, git)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture repository inventory")?;
    let collector_inputs = inventory
        .collector_bindings(&loaded.manifest)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture collector inputs")?;
    let worktree_census = collect_worktree_census_with_git(&repositories, git)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture worktrees")?;
    let nominations = resolve_nominations(&loaded.manifest, &worktree_census.records)
        .context("ASSAY_LIVE_CLOSURE_INVALID: recapture worktree nominations")?;
    let mut selection = live_selection.clone();
    selection.bindings.push(retained_exporter.clone());
    selection.bindings.sort();
    selection
        .paths
        .insert("xtask-exporter".to_owned(), exporter_path.to_path_buf());
    let recaptured = SourceBinding::capture(BindingCapture {
        workspace_root,
        manifest_hash: loaded.hash,
        selection,
        manifest: &loaded.manifest,
        repositories: &repositories,
        inventory: &inventory,
        collector_inputs,
        worktrees: &worktree_census.records,
        worktree_hashing: worktree_census.hashing,
        nominations: &nominations,
    })
    .context("ASSAY_LIVE_CLOSURE_INVALID: recapture binding planes")?;
    validate_binding_plane_match(retained, &recaptured)
}

#[cfg(feature = "architecture-test-seams")]
fn validate_binding_plane_match(
    retained: &SourceBinding,
    recaptured: &SourceBinding,
) -> Result<()> {
    let mismatched_planes = [
        (
            "manifest",
            retained.manifest_hash != recaptured.manifest_hash,
        ),
        (
            "repositories",
            retained.repositories != recaptured.repositories,
        ),
        ("contexts", retained.contexts != recaptured.contexts),
        (
            "collector-inputs",
            retained.collector_inputs != recaptured.collector_inputs,
        ),
        ("tools", retained.tools != recaptured.tools),
        ("source", retained.source != recaptured.source),
        ("build", retained.build != recaptured.build),
        ("helpers", retained.helpers != recaptured.helpers),
        ("worktrees", retained.worktrees != recaptured.worktrees),
        (
            "worktree-hashing",
            retained.worktree_hashing != recaptured.worktree_hashing,
        ),
        (
            "nominations",
            retained.nominations != recaptured.nominations,
        ),
        (
            "availability",
            retained.complete != recaptured.complete
                || retained.unavailable != recaptured.unavailable,
        ),
    ]
    .into_iter()
    .filter_map(|(name, differs)| differs.then_some(name))
    .collect::<Vec<_>>();
    if !mismatched_planes.is_empty() {
        anyhow::bail!(
            "ASSAY_LIVE_CLOSURE_INVALID: binding planes differ from live truth: {}",
            mismatched_planes.join(", ")
        );
    }
    Ok(())
}

fn build_tool_binding_hash(bindings: &[ToolBinding]) -> Result<Sha256Digest> {
    let selected = BUILD_TOOL_NAMES
        .into_iter()
        .map(|name| exact_tool(bindings, name))
        .collect::<Result<Vec<_>>>()?;
    Sha256Digest::new(crate::architecture::hash::hash_serialized(&selected)?)
}

fn validate_build_input_census(census: &BuildInputCensus) -> Result<(Sha256Digest, Sha256Digest)> {
    if census.schema.as_str() != "bbnf.architecture.local-build-input-census.v3.1" {
        anyhow::bail!("local build-input census has an unsupported schema");
    }
    if census.members.is_empty() {
        anyhow::bail!("local build-input census is empty");
    }
    if census.members.windows(2).any(|window| {
        (&window[0].repository, &window[0].path) >= (&window[1].repository, &window[1].path)
    }) {
        anyhow::bail!("local build-input members are not unique and strictly path-sorted");
    }
    Ok(build_input_census_hashes(&census.members))
}

fn build_input_census_hashes(members: &[BuildInputMember]) -> (Sha256Digest, Sha256Digest) {
    let mut path_rows = Vec::new();
    let mut ledger_rows = Vec::new();
    for member in members {
        path_rows.extend_from_slice(member.repository.as_str().as_bytes());
        path_rows.push(b'\t');
        path_rows.extend_from_slice(member.path.as_str().as_bytes());
        path_rows.push(b'\n');
        ledger_rows.extend_from_slice(member.hash.as_str().as_bytes());
        ledger_rows.push(b'\t');
        ledger_rows.extend_from_slice(member.size.to_string().as_bytes());
        ledger_rows.push(b'\t');
        ledger_rows.extend_from_slice(member.repository.as_str().as_bytes());
        ledger_rows.push(b'\t');
        ledger_rows.extend_from_slice(member.path.as_str().as_bytes());
        ledger_rows.push(b'\n');
    }
    (Sha256Digest::of(&path_rows), Sha256Digest::of(&ledger_rows))
}

fn exact_tool<'a>(tools: &'a [ToolBinding], name: &str) -> Result<&'a ToolBinding> {
    let matches = tools
        .iter()
        .filter(|binding| binding.tool.as_str() == name)
        .collect::<Vec<_>>();
    if matches.len() != 1 {
        anyhow::bail!("tool closure requires exactly one `{name}` binding");
    }
    Ok(matches[0])
}

impl BuildReceipt {
    fn verify_embedded_census_inner(&self) -> Result<()> {
        if self.embedded_source_ledger_hash != self.source_ledger_hash
            || self.embedded_local_input_ledger_hash != self.local_inputs.ledger_hash
            || self.embedded_build_tool_ledger_hash != self.build_tool_ledger_hash
        {
            anyhow::bail!(
                "EXPORTER_BUILD_INPUT_DRIFT: runtime source/build census differs from the ledger embedded before compilation"
            );
        }
        Ok(())
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn verify_embedded_census(&self) -> Result<()> {
        self.verify_embedded_census_inner()
    }
}

fn validate_collector_bindings(
    bindings: Vec<CollectorInputBinding>,
    inventory: &Inventory,
) -> Result<Vec<ConfigBinding>> {
    let mut bindings = bindings
        .into_iter()
        .map(|(repository, path, hash)| {
            let record = inventory
                .find(&repository, &path)
                .context("captured collector input is absent from inventory")?;
            if record.identity.kind != crate::architecture::schema::PhysicalKind::File
                || record.exclusion.is_some()
                || record.content_hash.as_ref() != Some(&hash)
            {
                anyhow::bail!(
                    "COLLECTOR_INPUT_RECONCILIATION_FAILED: typed collector input is excluded, unhashed, or changed"
                );
            }
            Ok(ConfigBinding {
                repository,
                path,
                hash,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    bindings.sort();
    if bindings.windows(2).any(|window| window[0] == window[1]) {
        anyhow::bail!("duplicate captured collector input");
    }
    Ok(bindings)
}

fn tool_binding(
    tool: &str,
    args: &[&str],
    inject: bool,
    #[cfg(target_os = "macos")] apple: &apple::AppleArmCaptureContext,
) -> Result<(ToolBinding, PathBuf)> {
    #[cfg(target_os = "macos")]
    if matches!(tool, "ar" | "cc" | "ld") {
        let path = apple.selected_path(tool)?;
        return tool_binding_at(tool, path, args, inject, apple);
    }
    let path = resolve_executable(tool)?;
    tool_binding_at(
        tool,
        &path,
        args,
        inject,
        #[cfg(target_os = "macos")]
        apple,
    )
}

fn tool_binding_at(
    tool: &str,
    path: &Path,
    args: &[&str],
    inject: bool,
    #[cfg(target_os = "macos")] apple: &apple::AppleArmCaptureContext,
) -> Result<(ToolBinding, PathBuf)> {
    if !path.is_absolute() {
        anyhow::bail!("required tool '{tool}' executable path is not absolute");
    }
    #[cfg(target_os = "macos")]
    if matches!(tool, "ar" | "cc" | "ld") {
        return apple_tool_binding(tool, path, args, inject, apple);
    }
    let direct_proxy_hash = hash_file(path)?;
    let (effective_path, toolchain_selection, toolchain_selection_hash) =
        effective_tool(tool, path)?;
    let executable_hash = hash_file(&effective_path)?;
    if inject {
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-spawn"))?;
    }
    let output = crate::architecture::canonical_child_command(path)
        .args(args)
        .output()
        .with_context(|| format!("execute required tool '{tool}'"))?;
    if inject {
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-read"))?;
    }
    if inject {
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-decode"))?;
    }
    let (capability, capability_hash) = capability_facts(tool, args, &output)?;
    let invocation_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .with_context(|| format!("required tool '{tool}' invocation name is not UTF-8"))?;
    let invocation_path_hash = hash_path(path);
    let proxy_hash = direct_proxy_hash;
    let argv = std::iter::once(tool)
        .chain(args.iter().copied())
        .map(CanonicalText::new)
        .collect::<Result<Vec<_>>>()?;
    let execution_contract = canonical_child_policy("required-tool-binding")?;
    Ok((
        ToolBinding {
            tool: CanonicalText::new(tool)?,
            invocation_name: CanonicalText::new(invocation_name)?,
            invocation_path_hash,
            proxy_hash,
            executable_hash: executable_hash.clone(),
            capability,
            capability_hash,
            argv,
            execution_contract,
            effective_path_hash: hash_path(&effective_path),
            toolchain_selection,
            toolchain_selection_hash,
            dynamic_runtime_hash: dynamic_runtime_hash(
                &effective_path,
                &executable_hash,
                #[cfg(target_os = "macos")]
                apple,
            ),
        },
        path.to_path_buf(),
    ))
}

#[cfg(target_os = "macos")]
fn apple_tool_binding(
    tool: &str,
    path: &Path,
    args: &[&str],
    inject: bool,
    apple: &apple::AppleArmCaptureContext,
) -> Result<(ToolBinding, PathBuf)> {
    apple.require_capture_input(tool, path)?;
    if inject {
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-spawn"))?;
    }
    let captured = apple.capture_tool(tool, args)?;
    if inject {
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-read"))?;
        crate::architecture::evidence::write::failpoint(&format!("tool-{tool}-decode"))?;
    }
    let (launcher, launcher_hash) = apple.launcher();
    let argv = std::iter::once(tool)
        .chain(args.iter().copied())
        .map(CanonicalText::new)
        .collect::<Result<Vec<_>>>()?;
    Ok((
        ToolBinding {
            tool: CanonicalText::new(tool)?,
            invocation_name: CanonicalText::new("arch")?,
            invocation_path_hash: hash_path(launcher),
            proxy_hash: launcher_hash.clone(),
            executable_hash: captured.selected_hash.clone(),
            capability: captured.capability,
            capability_hash: captured.capability_hash,
            argv,
            execution_contract: captured.execution_contract,
            effective_path_hash: hash_path(path),
            toolchain_selection: CanonicalText::new("xcode-developer-directory-selection")?,
            toolchain_selection_hash: captured.toolchain_selection_hash,
            dynamic_runtime_hash: captured.dynamic_runtime_hash,
        },
        path.to_path_buf(),
    ))
}

fn capability_facts(
    tool: &str,
    args: &[&str],
    output: &std::process::Output,
) -> Result<(CanonicalText, Sha256Digest)> {
    let archiver_usage = tool == "ar"
        && args == ["--version"]
        && !output.status.success()
        && output.stdout.is_empty()
        && !output.stderr.is_empty();
    if !output.status.success() && !archiver_usage {
        anyhow::bail!("required tool '{tool}' returned failure");
    }
    let bytes = if archiver_usage {
        output.stderr.as_slice()
    } else {
        output.stdout.as_slice()
    };
    let capability = std::str::from_utf8(bytes)
        .with_context(|| format!("required tool '{tool}' capability is not UTF-8"))?
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| {
            if line.starts_with("InstalledDir:") {
                "InstalledDir: <bound-path>"
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join(" | ");
    Ok((CanonicalText::new(capability)?, Sha256Digest::of(bytes)))
}

fn resolve_executable(tool: &str) -> Result<PathBuf> {
    let candidate = Path::new(tool);
    if candidate.components().count() > 1 {
        return absolute_without_symlink_resolution(candidate)
            .with_context(|| format!("resolve required tool '{tool}'"));
    }
    let path = std::env::var_os("PATH").context("PATH is unavailable")?;
    for directory in std::env::split_paths(&path) {
        let candidate = directory.join(tool);
        if candidate.is_file() {
            return absolute_without_symlink_resolution(&candidate)
                .with_context(|| format!("resolve required tool '{tool}'"));
        }
    }
    anyhow::bail!("required tool '{tool}' is unavailable")
}

fn absolute_without_symlink_resolution(path: &Path) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        Ok(std::env::current_dir()?.join(path))
    }
}

fn effective_tool(tool: &str, invocation: &Path) -> Result<(PathBuf, CanonicalText, Sha256Digest)> {
    let proxy = std::fs::canonicalize(invocation)
        .with_context(|| format!("resolve required tool '{tool}' payload"))?;
    if matches!(tool, "cargo" | "rustc")
        && proxy
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name.starts_with("rustup"))
    {
        let selected = crate::architecture::canonical_child_command(&proxy)
            .args(["which", tool])
            .output()
            .with_context(|| format!("resolve rustup-selected {tool} payload"))?;
        if !selected.status.success() {
            anyhow::bail!("required rustup-selected {tool} payload is unavailable");
        }
        let selected = std::str::from_utf8(&selected.stdout)
            .context("rustup-selected payload path is not UTF-8")?
            .trim();
        let effective = std::fs::canonicalize(selected)
            .with_context(|| format!("resolve rustup-selected {tool} bytes"))?;
        let active = crate::architecture::canonical_child_command(&proxy)
            .args(["show", "active-toolchain"])
            .output()
            .context("capture active rustup toolchain")?;
        if !active.status.success() {
            anyhow::bail!("active rustup toolchain is unavailable");
        }
        let raw = std::str::from_utf8(&active.stdout)
            .context("active rustup toolchain is not UTF-8")?
            .trim();
        let normalized = raw.split(" (").next().unwrap_or(raw);
        return Ok((
            effective,
            CanonicalText::new(normalized)?,
            Sha256Digest::of(raw.as_bytes()),
        ));
    }
    let selection = CanonicalText::new("direct-executable")?;
    let selection_hash = Sha256Digest::of(selection.as_str().as_bytes());
    Ok((proxy, selection, selection_hash))
}

fn exporter_binding(
    path: &Path,
    #[cfg(target_os = "macos")] apple: &apple::AppleArmCaptureContext,
) -> Result<ToolBinding> {
    let effective = std::fs::canonicalize(path).context("resolve executing exporter bytes")?;
    let executable_hash = hash_file(&effective)?;
    let argv = normalized_exporter_argv()?;
    let capability = CanonicalText::new(format!("xtask {}", env!("CARGO_PKG_VERSION")))?;
    let direct = CanonicalText::new("direct-executable")?;
    Ok(ToolBinding {
        tool: CanonicalText::new("xtask-exporter")?,
        invocation_name: CanonicalText::new(
            path.file_name()
                .and_then(|name| name.to_str())
                .context("executing exporter name is not UTF-8")?,
        )?,
        invocation_path_hash: hash_path(path),
        proxy_hash: executable_hash.clone(),
        executable_hash: executable_hash.clone(),
        capability_hash: Sha256Digest::of(capability.as_str().as_bytes()),
        capability,
        argv,
        execution_contract: exporter_execution_contract()?,
        effective_path_hash: hash_path(&effective),
        toolchain_selection_hash: Sha256Digest::of(direct.as_str().as_bytes()),
        toolchain_selection: direct,
        dynamic_runtime_hash: dynamic_runtime_hash(
            &effective,
            &executable_hash,
            #[cfg(target_os = "macos")]
            apple,
        ),
    })
}

#[cfg(feature = "architecture-test-seams")]
pub(crate) fn capture_current_exporter_for_test() -> Result<ToolBinding> {
    #[cfg(target_os = "macos")]
    let apple = apple::AppleArmCaptureContext::capture()?;
    exporter_binding(
        &std::env::current_exe().context("resolve executing test exporter")?,
        #[cfg(target_os = "macos")]
        &apple,
    )
}

fn normalized_exporter_argv() -> Result<Vec<CanonicalText>> {
    let mut normalized = Vec::new();
    let mut redact_next = false;
    for (index, argument) in std::env::args().enumerate() {
        let value = if index == 0 {
            "xtask-exporter".to_string()
        } else if redact_next {
            redact_next = false;
            "<bound-path>".to_string()
        } else {
            if matches!(argument.as_str(), "--root" | "--output") {
                redact_next = true;
            }
            argument
        };
        normalized.push(CanonicalText::new(value)?);
    }
    Ok(normalized)
}

fn canonical_child_policy(scope: &str) -> Result<Vec<CanonicalText>> {
    [
        format!("child-policy-scope={scope}"),
        "child-policy:env-clear=true".to_owned(),
        "child-policy:set:LC_ALL=C".to_owned(),
        "child-policy:set:TZ=UTC".to_owned(),
    ]
    .into_iter()
    .map(CanonicalText::new)
    .collect()
}

fn exporter_execution_contract() -> Result<Vec<CanonicalText>> {
    let epoch = RunEpoch::current()?;
    let runtime = [
        (
            "BBNF_ARCHITECTURE_FAILPOINT".to_owned(),
            epoch
                .injection_environment_value("BBNF_ARCHITECTURE_FAILPOINT")?
                .map(Into::into),
        ),
        (
            "BBNF_ARCHITECTURE_FAIL_STAGE".to_owned(),
            epoch
                .injection_environment_value("BBNF_ARCHITECTURE_FAIL_STAGE")?
                .map(Into::into),
        ),
        (
            "BBNF_ARCHITECTURE_WORKTREE_THREADS".to_owned(),
            std::env::var_os("BBNF_ARCHITECTURE_WORKTREE_THREADS"),
        ),
        ("PATH".to_owned(), std::env::var_os("PATH")),
    ]
    .into_iter()
    .collect::<BTreeMap<_, _>>();
    exporter_execution_contract_from_runtime(&runtime)
}

fn exporter_execution_contract_from_runtime(
    runtime: &BTreeMap<String, Option<OsString>>,
) -> Result<Vec<CanonicalText>> {
    let names = [
        "BBNF_ARCHITECTURE_FAILPOINT",
        "BBNF_ARCHITECTURE_FAIL_STAGE",
        "BBNF_ARCHITECTURE_WORKTREE_THREADS",
        "PATH",
    ]
    .into_iter()
    .collect::<BTreeSet<_>>();
    if runtime.keys().map(String::as_str).collect::<BTreeSet<_>>() != names
        || runtime.len() != names.len()
    {
        anyhow::bail!(
            "exporter runtime environment must contain exactly failpoint/fail-stage/worktree-threads/PATH"
        );
    }
    let mut facts = canonical_child_policy("all-production-architecture-children")?;
    for name in [
        "BBNF_ARCHITECTURE_FAILPOINT",
        "BBNF_ARCHITECTURE_FAIL_STAGE",
        "BBNF_ARCHITECTURE_WORKTREE_THREADS",
        "PATH",
    ] {
        let value = &runtime[name];
        let digest = value.as_ref().map_or_else(
            || Sha256Digest::of(b"<absent>"),
            |value| {
                Sha256Digest::of(&crate::architecture::hash::exact_os_identity_bytes(
                    b"exporter-runtime-input",
                    value,
                ))
            },
        );
        facts.push(CanonicalText::new(format!(
            "runtime-input:{name}:sha256:{digest}"
        ))?);
    }
    facts.sort();
    Ok(facts)
}

fn hash_path(path: &Path) -> Sha256Digest {
    Sha256Digest::of(&crate::architecture::hash::exact_os_identity_bytes(
        b"bound-tool-path",
        path.as_os_str(),
    ))
}

fn dynamic_runtime_hash(
    path: &Path,
    executable_hash: &Sha256Digest,
    #[cfg(target_os = "macos")] apple: &apple::AppleArmCaptureContext,
) -> Sha256Digest {
    #[cfg(target_os = "macos")]
    return apple.generic_runtime_hash(path, executable_hash);

    #[cfg(not(target_os = "macos"))]
    let _ = path;
    #[cfg(not(target_os = "macos"))]
    Sha256Digest::of(executable_hash.as_str().as_bytes())
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn exporter_execution_contract_for_test(
    runtime: &BTreeMap<String, Option<OsString>>,
) -> Result<Vec<CanonicalText>> {
    exporter_execution_contract_from_runtime(runtime)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn bound_path_hash_for_test(path: &Path) -> Sha256Digest {
    hash_path(path)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub use bound_path_hash_for_test as canonical_runtime_path_hash_for_test;

#[cfg(all(target_os = "macos", feature = "architecture-test-seams"))]
#[doc(hidden)]
pub use apple::{
    apple_binary_frame_for_test, apple_canonical_os_digest_for_test,
    apple_direct_dependencies_for_test, apple_physical_argv_for_test,
    apple_selection_inputs_for_test, apple_selector_record_for_test,
};

fn source_census(workspace_root: &Path) -> Result<SourceCensus> {
    let explicit = [
        ".cargo/config.toml",
        ".config/nextest.toml",
        ".github/workflows/ci.yml",
        ".rustfmt.toml",
        "Cargo.lock",
        "Cargo.toml",
        "Makefile",
        "restart/skinny/tranches/sk-v25/CAMPAIGN-STATE.md",
        "restart/skinny/tranches/sk-v25/HANDOFF.md",
        "restart/skinny/tranches/sk-v25/LOCK-CONSTITUTION.md",
        "restart/skinny/tranches/sk-v25/RUN-LEDGER.md",
        "restart/skinny/tranches/sk-v25/graph/P3-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3-EVIDENCE.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-GESTALT-2-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-ASSAY-PROTOCOL.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-S2-RETIREMENT.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-S2-LEDGER.tsv",
        "restart/skinny/tranches/sk-v25/graph/P3.1-ARM-SHA-SUCCESSOR-AUTHORITY.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-RESOURCE-SUCCESSOR-ADJUDICATION.md",
        "restart/skinny/tranches/sk-v25/graph/P3.1-RESOURCE-SUCCESSOR-LEDGER.tsv",
        "scripts/test-tier.sh",
        "xtask/Cargo.toml",
        "xtask/src/lib.rs",
        "xtask/src/main.rs",
        "xtask/src/regen_simple_runtime.rs",
        "xtask/tests/architecture.rs",
        "xtask/tests/architecture_feature_gate.rs",
        "xtask/tests/architecture_p3_counterexamples.rs",
    ];
    let recursive = [
        "xtask/architecture",
        "xtask/src/architecture",
        "xtask/tests/architecture",
        "xtask/tests/fixtures/architecture",
    ];
    let mut paths = explicit
        .iter()
        .map(|path| workspace_root.join(path))
        .collect::<Vec<_>>();
    for directory in recursive {
        collect_regular_files(
            &workspace_root.join(directory),
            &mut paths,
            "SOURCE_CENSUS_INCOMPLETE",
        )?;
    }
    paths.sort();
    paths.dedup();
    let mut members = Vec::with_capacity(paths.len());
    for path in paths {
        let metadata = std::fs::symlink_metadata(&path)
            .with_context(|| format!("SOURCE_CENSUS_INCOMPLETE: `{}`", path.display()))?;
        if !metadata.is_file() || metadata.file_type().is_symlink() {
            anyhow::bail!(
                "SOURCE_CENSUS_INCOMPLETE: subject member `{}` is not a regular file",
                path.display()
            );
        }
        members.push(SourceMember {
            path: source_member_repo_path(workspace_root, &path)?,
            size: metadata.len(),
            hash: hash_file(&path)?,
        });
    }
    members.sort_by(|left, right| left.path.cmp(&right.path));
    if members
        .windows(2)
        .any(|window| window[0].path == window[1].path)
    {
        anyhow::bail!("SOURCE_CENSUS_INCOMPLETE: duplicate subject path");
    }
    let mut path_rows = Vec::new();
    let mut ledger_rows = Vec::new();
    for member in &members {
        path_rows.extend_from_slice(member.path.as_str().as_bytes());
        path_rows.push(b'\n');
        ledger_rows.extend_from_slice(member.hash.as_str().as_bytes());
        ledger_rows.push(b'\t');
        ledger_rows.extend_from_slice(member.size.to_string().as_bytes());
        ledger_rows.push(b'\t');
        ledger_rows.extend_from_slice(member.path.as_str().as_bytes());
        ledger_rows.push(b'\n');
    }
    Ok(SourceCensus {
        schema: CanonicalText::new("bbnf.architecture.source-census.v3.1")?,
        members,
        path_set_hash: Sha256Digest::of(&path_rows),
        ledger_hash: Sha256Digest::of(&ledger_rows),
    })
}

fn collect_regular_files(directory: &Path, paths: &mut Vec<PathBuf>, failure: &str) -> Result<()> {
    let mut entries = std::fs::read_dir(directory)
        .with_context(|| format!("{failure}: `{}`", directory.display()))?
        .collect::<std::io::Result<Vec<_>>>()?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        let path = entry.path();
        let kind = entry.file_type()?;
        if kind.is_symlink() {
            anyhow::bail!("{failure}: symlink `{}` is unsupported", path.display());
        }
        if kind.is_dir() {
            collect_regular_files(&path, paths, failure)?;
        } else if kind.is_file() {
            paths.push(path);
        } else {
            anyhow::bail!("{failure}: non-file member");
        }
    }
    Ok(())
}

fn source_member_repo_path(workspace_root: &Path, path: &Path) -> Result<RepoPath> {
    RepoPath::from_relative_path(
        path.strip_prefix(workspace_root)
            .context("source census member escaped workspace")?,
    )
    .context("SOURCE_CENSUS_INCOMPLETE: source member path is not strict UTF-8")
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn source_member_repo_path_for_test(workspace_root: &Path, path: &Path) -> Result<RepoPath> {
    source_member_repo_path(workspace_root, path)
}

struct BuildInputLocation {
    repository: &'static str,
    root: PathBuf,
    path: PathBuf,
}

fn build_input_census(selection: &ToolSelection) -> Result<BuildInputCensus> {
    let bbnf = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .context("xtask build root")?
        .to_path_buf();
    let sibling_root = bbnf.parent().context("workspace sibling root")?;
    let parse_that = sibling_root.join("parse-that");
    let pprint = sibling_root.join("pprint");
    build_input_census_at(selection, &bbnf, &parse_that, &pprint)
}

fn build_input_census_at(
    selection: &ToolSelection,
    bbnf: &Path,
    parse_that: &Path,
    pprint: &Path,
) -> Result<BuildInputCensus> {
    verify_reachable_local_package_set(selection, bbnf, parse_that, pprint)?;
    let mut locations = Vec::new();
    for relative in [
        "Cargo.toml",
        "Cargo.lock",
        "rust-toolchain.toml",
        ".cargo/config.toml",
        "xtask/Cargo.toml",
        "crates/ir/Cargo.toml",
        "crates/ir/build.rs",
        "crates/csp-solver/Cargo.toml",
        "crates/egraph/Cargo.toml",
        "crates/egraph-derive/Cargo.toml",
    ] {
        locations.push(BuildInputLocation {
            repository: "bbnf",
            root: bbnf.to_path_buf(),
            path: bbnf.join(relative),
        });
    }
    for relative in [
        "xtask/src",
        "crates/ir/src",
        "crates/csp-solver/src",
        "crates/csp-solver/data/sudoku_puzzles",
        "crates/egraph/src",
        "crates/egraph-derive/src",
    ] {
        collect_build_input_tree("bbnf", bbnf, &bbnf.join(relative), &mut locations)?;
    }
    for relative in ["rust/parse_that/Cargo.toml", "rust/regex/Cargo.toml"] {
        locations.push(BuildInputLocation {
            repository: "parse-that",
            root: parse_that.to_path_buf(),
            path: parse_that.join(relative),
        });
    }
    for relative in ["rust/parse_that/src", "rust/regex/src"] {
        collect_build_input_tree(
            "parse-that",
            parse_that,
            &parse_that.join(relative),
            &mut locations,
        )?;
    }
    for relative in [
        "rust/Cargo.toml",
        "rust/derive/Cargo.toml",
        "rust/derive/lib.rs",
    ] {
        locations.push(BuildInputLocation {
            repository: "pprint",
            root: pprint.to_path_buf(),
            path: pprint.join(relative),
        });
    }
    collect_build_input_tree("pprint", pprint, &pprint.join("rust/src"), &mut locations)?;
    locations
        .sort_by(|left, right| (left.repository, &left.path).cmp(&(right.repository, &right.path)));
    locations
        .dedup_by(|left, right| left.repository == right.repository && left.path == right.path);
    let mut members = Vec::with_capacity(locations.len());
    for location in locations {
        let metadata = std::fs::symlink_metadata(&location.path).with_context(|| {
            format!(
                "BUILD_INPUT_CENSUS_INCOMPLETE: `{}`",
                location.path.display()
            )
        })?;
        if !metadata.is_file() || metadata.file_type().is_symlink() {
            anyhow::bail!(
                "BUILD_INPUT_CENSUS_INCOMPLETE: `{}` is not a regular file",
                location.path.display()
            );
        }
        members.push(BuildInputMember {
            repository: CanonicalText::new(location.repository)?,
            path: build_input_repo_path(&location.root, &location.path)?,
            size: metadata.len(),
            hash: hash_file(&location.path)?,
        });
    }
    members.sort();
    if members.windows(2).any(|window| {
        window[0].repository == window[1].repository && window[0].path == window[1].path
    }) {
        anyhow::bail!("BUILD_INPUT_CENSUS_INCOMPLETE: duplicate build input");
    }
    let (path_set_hash, ledger_hash) = build_input_census_hashes(&members);
    Ok(BuildInputCensus {
        schema: CanonicalText::new("bbnf.architecture.local-build-input-census.v3.1")?,
        members,
        path_set_hash,
        ledger_hash,
    })
}

fn build_input_repo_path(repository_root: &Path, path: &Path) -> Result<RepoPath> {
    RepoPath::from_relative_path(
        path.strip_prefix(repository_root)
            .context("build input escaped repository")?,
    )
    .context("BUILD_INPUT_CENSUS_INCOMPLETE: build input path is not strict UTF-8")
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn build_input_repo_path_for_test(repository_root: &Path, path: &Path) -> Result<RepoPath> {
    build_input_repo_path(repository_root, path)
}

fn verify_reachable_local_package_set(
    selection: &ToolSelection,
    bbnf: &Path,
    parse_that: &Path,
    pprint: &Path,
) -> Result<()> {
    let cargo = selection.path("cargo")?;
    let rustc = selection
        .bindings
        .iter()
        .find(|binding| binding.tool.as_str() == "rustc")
        .context("local package closure lacks rustc host")?;
    let host = rustc
        .capability
        .as_str()
        .split(" | ")
        .find_map(|line| line.strip_prefix("host: "))
        .context("local package closure lacks rustc host triple")?;
    let mut tool_directories = selection
        .paths
        .values()
        .filter_map(|path| path.parent().map(Path::to_path_buf))
        .collect::<Vec<_>>();
    tool_directories.sort();
    tool_directories.dedup();
    let path = std::env::join_paths(tool_directories).context("construct bound tool PATH")?;
    let output = crate::architecture::canonical_child_command(cargo)
        .args([
            "metadata",
            "--format-version",
            "1",
            "--locked",
            "--offline",
            "--filter-platform",
            host,
        ])
        .current_dir(bbnf)
        .env("CARGO_NET_OFFLINE", "true")
        .env("PATH", path)
        .output()
        .context("execute locked Cargo metadata for local build closure")?;
    if !output.status.success() {
        anyhow::bail!(
            "BUILD_INPUT_CENSUS_INCOMPLETE: Cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    let metadata: cargo_metadata::Metadata = serde_json::from_slice(&output.stdout)
        .context("decode locked Cargo metadata for local build closure")?;
    let resolve = metadata
        .resolve
        .as_ref()
        .context("BUILD_INPUT_CENSUS_INCOMPLETE: Cargo omitted its resolve graph")?;
    let xtask = metadata
        .packages
        .iter()
        .find(|package| {
            package.name.as_str() == "xtask"
                && Path::new(package.manifest_path.as_str()) == bbnf.join("xtask/Cargo.toml")
        })
        .context("BUILD_INPUT_CENSUS_INCOMPLETE: xtask package is absent")?;
    let nodes = resolve
        .nodes
        .iter()
        .map(|node| (&node.id, node))
        .collect::<BTreeMap<_, _>>();
    let packages = metadata
        .packages
        .iter()
        .map(|package| (&package.id, package))
        .collect::<BTreeMap<_, _>>();
    let mut pending = vec![xtask.id.clone()];
    let mut reachable = BTreeSet::new();
    while let Some(package) = pending.pop() {
        if !reachable.insert(package.clone()) {
            continue;
        }
        let node = nodes
            .get(&package)
            .with_context(|| format!("BUILD_INPUT_CENSUS_INCOMPLETE: missing node {package}"))?;
        pending.extend(node.dependencies.iter().cloned());
    }
    let mut actual = BTreeSet::new();
    for id in reachable {
        let package = packages
            .get(&id)
            .with_context(|| format!("BUILD_INPUT_CENSUS_INCOMPLETE: missing package {id}"))?;
        if package.source.is_some() {
            continue;
        }
        let manifest = Path::new(package.manifest_path.as_str());
        let identity = if let Ok(path) = manifest.strip_prefix(bbnf) {
            local_manifest_identity("bbnf", path)?
        } else if let Ok(path) = manifest.strip_prefix(parse_that) {
            local_manifest_identity("parse-that", path)?
        } else if let Ok(path) = manifest.strip_prefix(pprint) {
            local_manifest_identity("pprint", path)?
        } else {
            anyhow::bail!(
                "BUILD_INPUT_CENSUS_INCOMPLETE: reachable local package `{}` is outside declared repositories",
                package.name
            );
        };
        actual.insert(identity);
    }
    let expected = [
        "bbnf:crates/csp-solver/Cargo.toml",
        "bbnf:crates/egraph-derive/Cargo.toml",
        "bbnf:crates/egraph/Cargo.toml",
        "bbnf:crates/ir/Cargo.toml",
        "bbnf:xtask/Cargo.toml",
        "parse-that:rust/parse_that/Cargo.toml",
        "parse-that:rust/regex/Cargo.toml",
        "pprint:rust/Cargo.toml",
        "pprint:rust/derive/Cargo.toml",
    ]
    .into_iter()
    .map(str::to_string)
    .collect::<BTreeSet<_>>();
    if actual != expected {
        anyhow::bail!(
            "BUILD_INPUT_CENSUS_INCOMPLETE: locked reachable local package set differs; expected {expected:?}, actual {actual:?}"
        );
    }
    Ok(())
}

fn local_manifest_identity(repository: &str, path: &Path) -> Result<String> {
    path.to_str()
        .context("BUILD_INPUT_CENSUS_INCOMPLETE: local manifest path is not UTF-8")?;
    let path = RepoPath::from_relative_path(path)
        .context("BUILD_INPUT_CENSUS_INCOMPLETE: local manifest path is not canonical")?;
    Ok(format!("{repository}:{}", path.as_str()))
}

fn collect_build_input_tree(
    repository: &'static str,
    root: &Path,
    directory: &Path,
    locations: &mut Vec<BuildInputLocation>,
) -> Result<()> {
    let mut paths = Vec::new();
    collect_regular_files(directory, &mut paths, "BUILD_INPUT_CENSUS_INCOMPLETE")?;
    locations.extend(paths.into_iter().map(|path| BuildInputLocation {
        repository,
        root: root.to_path_buf(),
        path,
    }));
    Ok(())
}

fn build_receipt(selection: &ToolSelection, source: &SourceCensus) -> Result<BuildReceipt> {
    let exporter = exact_tool(&selection.bindings, "xtask-exporter")?;
    let archiver = exact_tool(&selection.bindings, "ar")?;
    let cargo = exact_tool(&selection.bindings, "cargo")?;
    let rustc = exact_tool(&selection.bindings, "rustc")?;
    let wrapper = exact_tool(&selection.bindings, "sccache")?;
    let linker_driver = exact_tool(&selection.bindings, "cc")?;
    let linker = exact_tool(&selection.bindings, "ld")?;
    let profile = match option_env!("BBNF_EXPORTER_BUILD_PROFILE") {
        Some("release") => "release",
        Some(other) => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: unsupported compile-time build profile `{other}`"
        ),
        None => {
            if [
                option_env!("BBNF_EXPORTER_BUILD_POLICY"),
                option_env!("BBNF_EXPORTER_BUILD_ENV_HASH"),
                option_env!("BBNF_EXPORTER_BUILD_ARGV_JSON"),
                option_env!("BBNF_EXPORTER_BUILD_FEATURE_GRAPH_HASH"),
                option_env!("BBNF_EXPORTER_SOURCE_LEDGER_HASH"),
                option_env!("BBNF_EXPORTER_BUILD_INPUT_LEDGER_HASH"),
                option_env!("BBNF_EXPORTER_BUILD_TOOL_LEDGER_HASH"),
                option_env!("BBNF_EXPORTER_BUILD_TOOL_PATHS_JSON"),
            ]
            .into_iter()
            .any(|receipt| receipt.is_some())
            {
                anyhow::bail!(
                    "EXPORTER_BUILD_RECEIPT_UNBOUND: embedded release inputs lack BBNF_EXPORTER_BUILD_PROFILE=release"
                );
            }
            "integration-test"
        }
    };
    if profile == "release" {
        let paths = embedded_build_tool_paths()?.context(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks exact build-tool paths",
        )?;
        for name in BUILD_TOOL_NAMES {
            if selection.path(name)? != paths[name] {
                anyhow::bail!(
                    "EXPORTER_BUILD_RECEIPT_UNBOUND: selected `{name}` input differs from its exact compile-time path"
                );
            }
        }
    }
    let build_policy =
        option_env!("BBNF_EXPORTER_BUILD_POLICY").unwrap_or("integration-test-unbound");
    let build_environment_hash = match option_env!("BBNF_EXPORTER_BUILD_ENV_HASH") {
        Some(hash) => Sha256Digest::new(hash)?,
        None if profile == "integration-test" => Sha256Digest::of(b"integration-test-unbound"),
        None => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks a build-environment hash"
        ),
    };
    if profile == "release" && build_policy != "bbnf-sanitized-build-v3.1" {
        anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks the sanitized build policy"
        );
    }
    let (build_argv, build_feature_graph_hash) = embedded_build_identity(
        profile,
        option_env!("BBNF_EXPORTER_BUILD_ARGV_JSON"),
        option_env!("BBNF_EXPORTER_BUILD_FEATURE_GRAPH_HASH"),
    )?;
    let host_facts = normalized_host_facts(
        rustc,
        #[cfg(target_os = "macos")]
        &selection.apple,
    )?;
    let host_hash = Sha256Digest::new(crate::architecture::hash::hash_serialized(&host_facts)?)?;
    let exporter_execution_contract = exporter_execution_contract()?;
    let local_inputs = build_input_census(selection)?;
    let build_tool_ledger_hash = selection.build_tool_ledger_hash_inner()?;
    let embedded_source_ledger_hash = match option_env!("BBNF_EXPORTER_SOURCE_LEDGER_HASH") {
        Some(hash) => Sha256Digest::new(hash)?,
        None if profile == "integration-test" => source.ledger_hash.clone(),
        None => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks its pre-build source ledger"
        ),
    };
    let embedded_local_input_ledger_hash = match option_env!(
        "BBNF_EXPORTER_BUILD_INPUT_LEDGER_HASH"
    ) {
        Some(hash) => Sha256Digest::new(hash)?,
        None if profile == "integration-test" => local_inputs.ledger_hash.clone(),
        None => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks its pre-build local-input ledger"
        ),
    };
    let embedded_build_tool_ledger_hash = match option_env!("BBNF_EXPORTER_BUILD_TOOL_LEDGER_HASH")
    {
        Some(hash) => Sha256Digest::new(hash)?,
        None if profile == "integration-test" => build_tool_ledger_hash.clone(),
        None => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: release exporter lacks its pre-build tool ledger"
        ),
    };
    let receipt = BuildReceipt {
        schema: CanonicalText::new("bbnf.architecture.exporter-build.v3.2")?,
        exporter_hash: exporter.executable_hash.clone(),
        // Publication destinations are transport, not source identity.  The
        // exporter binding retains every invocation argument while replacing
        // only the values of --root/--output with the typed <bound-path>
        // marker, so independent twin destinations bind the same operation.
        exporter_argv_hash: Sha256Digest::new(crate::architecture::hash::hash_serialized(
            &exporter.argv,
        )?)?,
        exporter_execution_contract_hash: Sha256Digest::new(
            crate::architecture::hash::hash_serialized(&exporter_execution_contract)?,
        )?,
        source_path_set_hash: source.path_set_hash.clone(),
        source_ledger_hash: source.ledger_hash.clone(),
        embedded_source_ledger_hash,
        local_inputs,
        embedded_local_input_ledger_hash,
        build_argv,
        build_feature_graph_hash,
        build_profile: CanonicalText::new(profile)?,
        build_environment_hash,
        registry_source_policy: CanonicalText::new(format!(
            "{build_policy}; locked registry archives are checksum-verified in an isolated Cargo home"
        ))?,
        build_tool_ledger_hash,
        embedded_build_tool_ledger_hash,
        cargo_payload_hash: cargo.executable_hash.clone(),
        rustc_payload_hash: rustc.executable_hash.clone(),
        wrapper_payload_hash: wrapper.executable_hash.clone(),
        linker_driver_payload_hash: linker_driver.executable_hash.clone(),
        linker_payload_hash: linker.executable_hash.clone(),
        archiver_payload_hash: archiver.executable_hash.clone(),
        toolchain_selection_hash: Sha256Digest::new(crate::architecture::hash::hash_serialized(
            &(
                &cargo.toolchain_selection_hash,
                &rustc.toolchain_selection_hash,
            ),
        )?)?,
        dynamic_runtime_hash: exporter.dynamic_runtime_hash.clone(),
        host_facts,
        host_hash,
    };
    receipt.verify_embedded_census_inner()?;
    Ok(receipt)
}

fn embedded_build_identity(
    profile: &str,
    argv_json: Option<&str>,
    feature_graph_hash: Option<&str>,
) -> Result<(Vec<CanonicalText>, Sha256Digest)> {
    match (argv_json, feature_graph_hash) {
        (Some(argv_json), Some(feature_graph_hash)) => {
            let argv = serde_json::from_str::<Vec<CanonicalText>>(argv_json)
                .context("EXPORTER_BUILD_RECEIPT_UNBOUND: malformed embedded build argv")?;
            if argv.is_empty() || serde_json::to_string(&argv)? != argv_json {
                anyhow::bail!(
                    "EXPORTER_BUILD_RECEIPT_UNBOUND: embedded build argv is empty or noncanonical"
                );
            }
            Ok((argv, Sha256Digest::new(feature_graph_hash)?))
        }
        (None, None) if profile == "integration-test" => Ok((
            [
                "cargo",
                "test",
                "--no-run",
                "--locked",
                "-p",
                "xtask",
                "--features",
                "architecture-test-seams",
                "--test",
                "architecture",
            ]
            .into_iter()
            .map(CanonicalText::new)
            .collect::<Result<Vec<_>>>()?,
            Sha256Digest::of(b"integration-test-unbound"),
        )),
        _ => anyhow::bail!(
            "EXPORTER_BUILD_RECEIPT_UNBOUND: build argv and feature-graph hash must be embedded together"
        ),
    }
}

fn normalized_host_facts(
    rustc: &ToolBinding,
    #[cfg(target_os = "macos")] apple: &apple::AppleArmCaptureContext,
) -> Result<Vec<CanonicalText>> {
    let mut facts = vec![
        CanonicalText::new(format!("arch={}", std::env::consts::ARCH))?,
        CanonicalText::new(format!("family={}", std::env::consts::FAMILY))?,
        CanonicalText::new(format!("os={}", std::env::consts::OS))?,
    ];
    for line in rustc.capability.as_str().split(" | ") {
        if line.starts_with("host:") || line.starts_with("release:") {
            facts.push(CanonicalText::new(line)?);
        }
    }
    #[cfg(target_os = "macos")]
    facts.extend(apple.host_facts()?);
    #[cfg(not(target_os = "macos"))]
    if let Ok(output) = crate::architecture::canonical_child_command("/usr/bin/uname")
        .arg("-srmv")
        .output()
        && output.status.success()
    {
        let uname = std::str::from_utf8(&output.stdout)?.trim();
        facts.push(CanonicalText::new(format!("kernel={uname}"))?);
    }
    facts.sort();
    Ok(facts)
}

fn hash_file(path: &Path) -> Result<Sha256Digest> {
    let mut file = std::fs::File::open(path)?;
    let mut hash = crate::architecture::hash::Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hash.update(&buffer[..read]);
    }
    Sha256Digest::new(hash.finalize())
}
