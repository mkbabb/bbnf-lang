use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use tempfile::tempdir;
use xtask::architecture::evidence::binding::{
    BindingCapture, ConfigBinding, SourceBinding, capture_build_tool_binding_hash,
};
use xtask::architecture::evidence::validate::validate_all;
use xtask::architecture::evidence::write::{
    ArtifactPublication, Diagnostic, EvidencePayload, GenerationStatus, MovementAuthorization,
    MovementAuthorizationStatus, publish,
};
use xtask::architecture::inventory::{
    Inventory, InventoryRecord, WorktreeRecord, collect_worktree_census_with_git,
    resolve_nominations,
};
use xtask::architecture::manifest::ConstellationManifest;
use xtask::architecture::project::project;
use xtask::architecture::schema::{
    CanonicalText, Condition, EdgeKind, GraphArtifact, Identity, NodeKind, PhysicalIdentity,
    PhysicalKind, PipelineStage, RepoPath, RepositoryId, ResolverId, Sha256Digest, SourceAnchor,
    WorktreeId, exact_edge, exact_node,
};

#[path = "evidence/binding.rs"]
mod binding;
#[path = "evidence/integration.rs"]
mod integration;
#[path = "evidence/transaction.rs"]
mod transaction;
#[path = "evidence/validate.rs"]
mod validate;
#[path = "evidence/write.rs"]
mod write;
use xtask::architecture::test_support::{
    RunEpoch, ToolSelection, canonical_child_command, exercise_final_recensus_workers,
    exercise_required_tool_workers, inject_operation as failpoint, run_at_with_epoch,
};

const ALL_TOOL_WORKERS: u8 = u8::MAX;
const GATE_TIMEOUT: Duration = Duration::from_secs(10);
const ARTIFACT_ROLES: [&str; 17] = [
    "basis.json",
    "binding.json",
    "candidate-graph.json",
    "condensation.json",
    "diagnostics.json",
    "generation.json",
    "graph.dot",
    "graph.json",
    "inventory.json",
    "manifest.json",
    "obligations.json",
    "projections.json",
    "publication.json",
    "quotient.json",
    "report.txt",
    "semantic-ledger.json",
    "shards.json",
];

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct CurrentPointer {
    schema: CanonicalText,
    run_id: CanonicalText,
    publication_size: u64,
    publication_hash: Sha256Digest,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct PublicationReceipt {
    schema: CanonicalText,
    status: GenerationStatus,
    stage: PipelineStage,
    source_binding: Sha256Digest,
    movement_authorization: MovementAuthorization,
    expected_files: Vec<CanonicalText>,
    artifacts: BTreeMap<CanonicalText, ArtifactPublication>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct GenerationSummary {
    schema: CanonicalText,
    status: GenerationStatus,
    stage: PipelineStage,
    source_binding: Sha256Digest,
    movement_authorization: MovementAuthorization,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(deny_unknown_fields)]
struct ExporterExecutionContractReceipt {
    execution_contract: Vec<CanonicalText>,
    execution_contract_hash: Sha256Digest,
    child_environment: Vec<CanonicalText>,
    child_environment_hash: Sha256Digest,
}

struct ExactBitGate {
    expected: u8,
    state: Mutex<ExactBitGateState>,
    wake: Condvar,
}

#[derive(Default)]
struct ExactBitGateState {
    arrived: u8,
    released: bool,
    failure: Option<String>,
}

impl ExactBitGate {
    fn new(expected: u8) -> Self {
        assert_ne!(expected, 0, "exact-bit gate requires at least one worker");
        Self {
            expected,
            state: Mutex::new(ExactBitGateState::default()),
            wake: Condvar::new(),
        }
    }

    fn arrive(&self, bit: u8) -> anyhow::Result<()> {
        let mut state = self.state.lock().expect("exact-bit gate lock");
        if bit.count_ones() != 1 || self.expected & bit == 0 {
            return Err(self.abort(&mut state, format!("unexpected worker bit {bit:#010b}")));
        }
        if state.arrived & bit != 0 {
            return Err(self.abort(&mut state, format!("duplicate worker bit {bit:#010b}")));
        }
        if state.released {
            return Err(self.abort(&mut state, "worker arrived after release".to_owned()));
        }
        state.arrived |= bit;
        if state.arrived == self.expected {
            state.released = true;
            self.wake.notify_all();
            return Ok(());
        }

        let deadline = Instant::now() + GATE_TIMEOUT;
        loop {
            if let Some(failure) = &state.failure {
                anyhow::bail!(failure.clone());
            }
            if state.released {
                if state.arrived == self.expected {
                    return Ok(());
                }
                anyhow::bail!("exact-bit gate released without its exact worker set");
            }
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                let arrived = state.arrived;
                return Err(self.abort(
                    &mut state,
                    format!(
                        "exact-bit gate timed out: arrived={arrived:#010b} expected={:#010b}",
                        self.expected
                    ),
                ));
            }
            let (next, timeout) = self
                .wake
                .wait_timeout(state, remaining)
                .expect("exact-bit gate wait");
            state = next;
            if timeout.timed_out() && !state.released {
                let arrived = state.arrived;
                return Err(self.abort(
                    &mut state,
                    format!(
                        "exact-bit gate timed out: arrived={arrived:#010b} expected={:#010b}",
                        self.expected
                    ),
                ));
            }
        }
    }

    fn arrived(&self) -> u8 {
        self.state.lock().expect("exact-bit gate lock").arrived
    }

    fn abort(&self, state: &mut ExactBitGateState, failure: String) -> anyhow::Error {
        state.failure.get_or_insert(failure);
        state.released = true;
        self.wake.notify_all();
        anyhow::anyhow!(
            state
                .failure
                .as_ref()
                .expect("exact-bit gate failure")
                .clone()
        )
    }
}

#[derive(Debug)]
struct NaturalToolFailure(&'static str);

impl std::fmt::Display for NaturalToolFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "natural {} failure", self.0)
    }
}

impl std::error::Error for NaturalToolFailure {}

#[derive(Debug)]
struct ToolWorkerPanic;

fn assert_one_named_child_passed(transcript: &str, name: &str, context: &str) {
    assert_eq!(
        transcript
            .lines()
            .filter(|line| *line == "running 1 test")
            .count(),
        1,
        "{context} did not run exactly one test: {transcript}"
    );
    let named_row = format!("test {name} ... ok");
    assert_eq!(
        transcript
            .lines()
            .filter(|line| *line == named_row.as_str())
            .count(),
        1,
        "{context} did not pass its named test exactly once: {transcript}"
    );
    let summaries = transcript
        .lines()
        .filter(|line| line.starts_with("test result:"))
        .collect::<Vec<_>>();
    assert_eq!(
        summaries.len(),
        1,
        "{context} emitted an ambiguous summary: {transcript}"
    );
    let fields = summaries[0].split("; ").collect::<Vec<_>>();
    assert!(
        fields.len() >= 4,
        "{context} emitted a malformed summary: {transcript}"
    );
    assert_eq!(
        &fields[..4],
        [
            "test result: ok. 1 passed",
            "0 failed",
            "0 ignored",
            "0 measured",
        ],
        "{context} success summary was not exact: {transcript}"
    );
}

fn tool_worker_bit(tool: &str) -> u8 {
    1 << match tool {
        "ar" => 0,
        "cargo" => 1,
        "rustc" => 2,
        "node" => 3,
        "git" => 4,
        "sccache" => 5,
        "cc" => 6,
        "ld" => 7,
        other => panic!("unexpected required tool {other}"),
    }
}

struct MaterializedRuntimeFixture {
    roots: tempfile::TempDir,
    bbnf: std::path::PathBuf,
}

fn materialized_runtime_fixture() -> MaterializedRuntimeFixture {
    let roots = tempdir().expect("roots");
    let manifest = super::manifest();
    let repositories = super::frontend::materialize(&manifest, roots.path());
    let bbnf = repositories
        .iter()
        .find(|repository| repository.id.as_str() == "bbnf")
        .expect("bbnf")
        .root
        .clone();
    let source = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace")
        .to_path_buf();
    let architecture = bbnf.join("xtask/architecture");
    fs::create_dir_all(architecture.join("frontend")).expect("architecture fixture");
    fs::copy(
        source.join("xtask/architecture/constellation-v3.toml"),
        architecture.join("constellation-v3.toml"),
    )
    .expect("manifest");
    for helper in ["program.mjs", "vue.mjs", "css.mjs"] {
        fs::copy(
            source.join("xtask/architecture/frontend").join(helper),
            architecture.join("frontend").join(helper),
        )
        .expect("helper");
    }
    for repository in &repositories {
        for (key, value) in [
            ("user.email", "fixture@example.invalid"),
            ("user.name", "Fixture"),
        ] {
            assert!(
                Command::new("git")
                    .arg("-C")
                    .arg(&repository.root)
                    .args(["config", key, value])
                    .status()
                    .expect("git config")
                    .success()
            );
        }
        for args in [
            &["add", "."][..],
            &["commit", "--allow-empty", "-m", "fixture"][..],
        ] {
            assert!(
                Command::new("git")
                    .arg("-C")
                    .arg(&repository.root)
                    .args(args)
                    .status()
                    .expect("Git fixture command")
                    .success()
            );
        }
    }
    MaterializedRuntimeFixture { roots, bbnf }
}

fn serial_runtime_binding(root: &std::path::Path) -> SourceBinding {
    let manifest_path = root.join("xtask/architecture/constellation-v3.toml");
    let tools = ToolSelection::capture().expect("tool selection");
    let loaded = ConstellationManifest::load_bound(&manifest_path).expect("bound manifest");
    let repositories = loaded
        .manifest
        .resolve_repositories(root)
        .expect("repositories");
    let git = tools.path("git").expect("Git tool");
    let inventory = Inventory::collect_with_git(&loaded.manifest, &repositories, git)
        .expect("serial inventory");
    let collector_inputs = inventory
        .collector_bindings(&loaded.manifest)
        .expect("collector bindings");
    let worktree_census =
        collect_worktree_census_with_git(&repositories, git).expect("serial worktree census");
    let nominations =
        resolve_nominations(&loaded.manifest, &worktree_census.records).expect("nominations");
    SourceBinding::capture(BindingCapture {
        workspace_root: root,
        manifest_hash: loaded.hash,
        selection: tools,
        manifest: &loaded.manifest,
        repositories: &repositories,
        inventory: &inventory,
        collector_inputs,
        worktrees: &worktree_census.records,
        worktree_hashing: worktree_census.hashing,
        nominations: &nominations,
    })
    .expect("serial binding")
}

fn current_run_id(output: &std::path::Path) -> String {
    let current: serde_json::Value =
        serde_json::from_slice(&fs::read(output.join("CURRENT")).expect("CURRENT pointer"))
            .expect("typed CURRENT pointer");
    current["run_id"]
        .as_str()
        .expect("CURRENT run_id")
        .to_string()
}

fn publish_minimal(
    output: &std::path::Path,
    binding: &SourceBinding,
) -> xtask::architecture::evidence::write::Generation {
    try_publish_minimal(output, binding).expect("publish")
}

fn try_publish_minimal(
    output: &std::path::Path,
    binding: &SourceBinding,
) -> anyhow::Result<xtask::architecture::evidence::write::Generation> {
    publish(
        output,
        EvidencePayload {
            binding,
            manifest: None,
            inventory: None,
            graph: None,
            graph_validated: false,
            projections: None,
            failures: None,
            diagnostics: Vec::new(),
            stage: PipelineStage::Manifest,
            status: GenerationStatus::Failure,
        },
    )
}

fn digest(value: &str) -> Sha256Digest {
    Sha256Digest::of(value.as_bytes())
}

fn capture_complete_binding(selection: ToolSelection) -> SourceBinding {
    let workspace = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("workspace");
    let manifest_path = workspace.join("xtask/architecture/constellation-v3.toml");
    let manifest = super::manifest();
    let repositories = manifest
        .resolve_repositories(workspace)
        .expect("repositories");
    let mut records = Vec::new();
    for context in &manifest.contexts {
        for path in std::iter::once(&context.manifest)
            .chain(std::iter::once(&context.lock))
            .chain(&context.config_inputs)
        {
            records.push(InventoryRecord {
                identity: PhysicalIdentity {
                    repository: context.repository.clone(),
                    path: path.clone(),
                    kind: PhysicalKind::File,
                },
                content_hash: Some(Sha256Digest::of(path.as_str().as_bytes())),
                semantic_faces: Vec::new(),
                exclusion: None,
            });
        }
    }
    records.sort();
    records.dedup_by(|left, right| left.identity == right.identity);
    let inventory = Inventory {
        schema: "bbnf.architecture.inventory.v3.1".into(),
        records,
    };
    let worktrees = vec![
        dummy_worktree("bbnf"),
        dummy_worktree("parse-that"),
        dummy_worktree("pprint"),
    ];
    let nominations = resolve_nominations(&manifest, &worktrees).expect("nominations");
    SourceBinding::capture(BindingCapture {
        workspace_root: workspace,
        manifest_hash: Sha256Digest::of(&fs::read(manifest_path).expect("manifest bytes")),
        selection,
        manifest: &manifest,
        repositories: &repositories,
        inventory: &inventory,
        collector_inputs: Vec::new(),
        worktrees: &worktrees,
        worktree_hashing: Default::default(),
        nominations: &nominations,
    })
    .expect("binding")
}

fn capture_grounded_binding(
    selection: ToolSelection,
) -> (
    SourceBinding,
    std::path::PathBuf,
    BTreeMap<String, std::path::PathBuf>,
) {
    let workspace = std::fs::canonicalize(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("workspace"),
    )
    .expect("canonical workspace");
    let manifest_path = workspace.join("xtask/architecture/constellation-v3.toml");
    let loaded = ConstellationManifest::load_bound(&manifest_path).expect("live manifest");
    let repositories = loaded
        .manifest
        .resolve_repositories(&workspace)
        .expect("live repositories");
    let git = selection.path("git").expect("bound Git");
    let inventory =
        Inventory::collect_with_git(&loaded.manifest, &repositories, git).expect("live inventory");
    let collector_inputs = inventory
        .collector_bindings(&loaded.manifest)
        .expect("live collector inputs");
    let worktree_census =
        collect_worktree_census_with_git(&repositories, git).expect("live worktrees");
    let nominations =
        resolve_nominations(&loaded.manifest, &worktree_census.records).expect("live nominations");
    let binding = SourceBinding::capture(BindingCapture {
        workspace_root: &workspace,
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
    .expect("grounded binding");
    let mut roots = repositories
        .iter()
        .map(|repository| (repository.id.as_str().to_owned(), repository.root.clone()))
        .collect::<BTreeMap<_, _>>();
    roots.insert(
        "pprint".to_owned(),
        std::fs::canonicalize(workspace.parent().expect("sibling root").join("pprint"))
            .expect("pprint root"),
    );
    (binding, workspace, roots)
}

fn dummy_worktree(repository: &str) -> WorktreeRecord {
    WorktreeRecord {
        identity: WorktreeId::new(format!("{repository}-primary")).expect("identity"),
        repository: RepositoryId::new(repository).expect("repository"),
        administrative_key: CanonicalText::new("primary").expect("key"),
        head: CanonicalText::new("0123456789012345678901234567890123456789").expect("head"),
        branch: None,
        detached: false,
        bare: false,
        dirty: false,
        content_hash: Sha256Digest::of(b"worktree"),
        administrative_config_hash: Sha256Digest::of(b"admin-config"),
        ignored_content_policy: CanonicalText::new(
            "ignored-derived-bytes-excluded; collector-readable-bytes-source-bound",
        )
        .expect("policy"),
    }
}

fn synthetic_graph() -> GraphArtifact {
    let identity = |key: &str| Identity::Virtual {
        namespace: CanonicalText::new("evidence").expect("namespace"),
        key: CanonicalText::new(key).expect("key"),
    };
    let mut graph = GraphArtifact::new();
    for key in ["a", "b"] {
        graph.exact(
            exact_node(identity(key), NodeKind::VirtualArtifact, key, Vec::new()).expect("node"),
        );
    }
    graph.exact(
        exact_edge(
            identity("a"),
            identity("b"),
            EdgeKind::RustUses,
            Condition::Always,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new("f20").expect("authority"),
            },
            ResolverId::new("f20").expect("resolver"),
            Vec::new(),
        )
        .expect("edge"),
    );
    graph.canonicalize().expect("graph");
    graph
}
