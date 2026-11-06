pub mod collect;
pub mod evidence;
mod hash;
pub mod inventory;
pub mod manifest;
pub mod project;
pub mod schema;

use std::ffi::OsStr;
use std::path::Path;
use std::process::Command;

use anyhow::Result;
use evidence::binding::{BindingCapture, SourceBinding, ToolSelection};
use evidence::validate::validate_all;
use evidence::write::{RunEpoch, failpoint, publish_complete, publish_failure, stage_injection};
use inventory::{CapturedInputs, Inventory, collect_worktree_census_with_git, resolve_nominations};
use manifest::ConstellationManifest;
use project::ProjectionSet;
use schema::{GraphArtifact, PipelineStage};

pub const MANIFEST_RELATIVE_PATH: &str = "xtask/architecture/constellation-v3.toml";

pub(crate) fn canonical_child_command(program: impl AsRef<OsStr>) -> Command {
    let mut command = Command::new(program);
    command.env_clear().env("LC_ALL", "C").env("TZ", "UTC");
    command
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub mod test_support {
    pub use super::evidence::binding::{
        ToolSelection, bound_path_hash_for_test, canonical_runtime_path_hash_for_test,
        exercise_required_tool_workers, exporter_execution_contract_for_test,
    };
    pub use super::evidence::validate::{
        ValidationOperation, ValidationPhase, validate_canonical_prefix_for_test,
        validate_projection_budget_with_observer_for_test, with_validation_observer_for_test,
    };
    pub use super::evidence::write::{RunEpoch, RunEpochReceipt};
    pub use super::hash::{
        JsonMapKeyTestCase, JsonOperation, JsonTestView, Sha256, budgeted_json_view_for_test,
        canonical_json, exact_os_identity_bytes_for_test, hash_serialized, serialized_len_capped,
        sha256, with_json_observer_for_test,
    };
    pub use super::inventory::worktree::requested_worker_policy_for_test;
    pub use super::project::cluster::{
        QuotientDomainOperation, with_quotient_domain_observer_for_test,
    };
    pub use super::project::cycle::{
        CycleStorageOperation, cycle_basis_fresh_prefix_for_test, validate_cycle_domain_for_test,
        with_cycle_storage_observer_for_test,
    };
    pub use super::project::scc::{
        validate_component_members_for_test, validate_scc_serialization_for_test,
    };
    pub use super::schema::{
        PercentDecodeOperation, percent_decode_allocation_bound_for_test, percent_decode_for_test,
        portable_violation_for_test, unchecked_canonical_text_for_test,
        validate_canonical_limits_for_test, with_percent_decode_observer_for_test,
    };
    pub use super::{exercise_final_recensus_workers, run_at_with_epoch};

    pub fn capture_current_exporter() -> anyhow::Result<super::evidence::binding::ToolBinding> {
        super::evidence::binding::capture_current_exporter_for_test()
    }

    pub fn canonical_child_command(program: &std::path::Path) -> std::process::Command {
        super::canonical_child_command(program)
    }

    pub fn inject_operation(name: &str) -> anyhow::Result<()> {
        super::evidence::write::failpoint(name)
    }
}

#[derive(Clone)]
pub(crate) struct RunState {
    pub(crate) binding: SourceBinding,
    pub(crate) manifest: Option<ConstellationManifest>,
    pub(crate) inventory: Option<Inventory>,
    pub(crate) captured: CapturedInputs,
    pub(crate) graph: Option<GraphArtifact>,
    pub(crate) graph_validated: bool,
    pub(crate) projections: Option<ProjectionSet>,
    pub(crate) tools: Option<ToolSelection>,
}

pub(crate) struct StageFailure {
    pub(crate) stage: PipelineStage,
    pub(crate) error: anyhow::Error,
    pub(crate) state: RunState,
}

pub fn run(output: &Path) -> Result<()> {
    let root = manifest::workspace_root();
    run_at(&root, output)
}

pub fn run_at(root: &Path, output: &Path) -> Result<()> {
    let epoch = RunEpoch::capture_environment()?;
    run_at_in_epoch(root, output, &epoch)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn run_at_with_epoch(root: &Path, output: &Path, epoch: &RunEpoch) -> Result<()> {
    run_at_in_epoch(root, output, epoch)
}

fn run_at_in_epoch(root: &Path, output: &Path, epoch: &RunEpoch) -> Result<()> {
    let _epoch = epoch.install()?;
    let manifest_path = root.join(MANIFEST_RELATIVE_PATH);
    let initial = RunState {
        binding: SourceBinding::minimal(&manifest_path)?,
        manifest: None,
        inventory: None,
        captured: CapturedInputs::default(),
        graph: None,
        graph_validated: false,
        projections: None,
        tools: None,
    };
    match execute(root, &manifest_path, initial) {
        Ok(state) => publish_complete(output, root, state),
        Err(failure) => publish_failure(output, root, *failure),
    }
}

fn execute(
    root: &Path,
    manifest_path: &Path,
    mut state: RunState,
) -> Result<RunState, Box<StageFailure>> {
    inject(&state, PipelineStage::Bootstrap)?;
    let tools = step(&state, PipelineStage::Bootstrap, ToolSelection::capture())?;
    let git = step(
        &state,
        PipelineStage::Bootstrap,
        tools.path("git").map(Path::to_path_buf),
    )?;
    inject(&state, PipelineStage::Manifest)?;
    let loaded = step(
        &state,
        PipelineStage::Manifest,
        ConstellationManifest::load_bound(manifest_path),
    )?;
    state.manifest = Some(loaded.manifest);
    let manifest_hash = loaded.hash;
    let manifest = state.manifest.as_ref().expect("manifest").clone();
    step(
        &state,
        PipelineStage::Manifest,
        failpoint("repository-resolution"),
    )?;
    let repositories = step(
        &state,
        PipelineStage::Manifest,
        manifest.resolve_repositories(root),
    )?;

    inject(&state, PipelineStage::Inventory)?;
    let snapshot = step(
        &state,
        PipelineStage::Inventory,
        Inventory::collect_snapshot_with_git(&manifest, &repositories, &git),
    )?;
    state.inventory = Some(snapshot.inventory);
    state.captured = snapshot.inputs;

    inject(&state, PipelineStage::Worktree)?;
    let worktree_census = step(
        &state,
        PipelineStage::Worktree,
        collect_worktree_census_with_git(&repositories, &git),
    )?;
    let worktrees = worktree_census.records;
    let nominations = step(
        &state,
        PipelineStage::Worktree,
        resolve_nominations(&manifest, &worktrees),
    )?;
    let selection = tools.clone();
    let collector_inputs = state.captured.bindings();
    inject(&state, PipelineStage::Foundation)?;
    state.binding = step(
        &state,
        PipelineStage::Foundation,
        SourceBinding::capture(BindingCapture {
            workspace_root: root,
            manifest_hash,
            selection: tools,
            manifest: &manifest,
            repositories: &repositories,
            inventory: state.inventory.as_ref().expect("inventory"),
            collector_inputs,
            worktrees: &worktrees,
            worktree_hashing: worktree_census.hashing,
            nominations: &nominations,
        }),
    )?;
    state.tools = Some(selection);

    state.graph = Some(GraphArtifact::new());
    let foundation = collect::foundation::collect(
        &manifest,
        state.inventory.as_ref().expect("inventory"),
        &worktrees,
        &nominations,
        state.graph.as_mut().expect("graph"),
    );
    step(&state, PipelineStage::Foundation, foundation)?;
    collect_boundaries(&manifest, &repositories, &mut state)?;

    inject(&state, PipelineStage::Validation)?;
    let canonical = state.graph.as_mut().expect("graph").canonicalize();
    step(&state, PipelineStage::Validation, canonical)?;
    inject(&state, PipelineStage::Projection)?;
    let projections = project::project(state.graph.as_ref().expect("graph"))
        .map_err(|error| failure(&state, project::failure_stage(&error), error))?;
    state.projections = Some(projections);
    let validation = validate_all(
        &manifest,
        state.inventory.as_ref().expect("inventory"),
        &state.binding,
        state.graph.as_mut().expect("graph"),
        state.projections.as_ref().expect("projections"),
    );
    step(&state, PipelineStage::Validation, validation)?;
    state.graph_validated = true;
    inject(&state, PipelineStage::Recensus)?;
    step(&state, PipelineStage::Recensus, failpoint("final-recensus"))?;
    let recensus = step(
        &state,
        PipelineStage::Recensus,
        recensus_binding(root, manifest_path),
    )?;
    step(
        &state,
        PipelineStage::Recensus,
        state.binding.verify_recensus(&recensus),
    )?;
    Ok(state)
}

fn recensus_binding(root: &Path, manifest_path: &Path) -> Result<SourceBinding> {
    let epoch = RunEpoch::current()?;
    let tools = ToolSelection::capture()?;
    let loaded = ConstellationManifest::load_bound(manifest_path)?;
    let repositories = loaded.manifest.resolve_repositories(root)?;
    let git = tools.path("git")?;
    let manifest = &loaded.manifest;
    let repository_slice = repositories.as_slice();
    let ((inventory, collector_inputs), worktree_census) = run_final_recensus_workers(
        &epoch,
        || {
            let inventory = Inventory::collect_with_git(manifest, repository_slice, git)?;
            let collector_inputs = inventory.collector_bindings(manifest)?;
            Ok((inventory, collector_inputs))
        },
        || collect_worktree_census_with_git(repository_slice, git),
    )?;
    let nominations = resolve_nominations(&loaded.manifest, &worktree_census.records)?;
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
}

enum FinalRecensusWorkerOutcome<T> {
    Spawn(std::io::Error),
    Joined(std::thread::Result<Result<T>>),
}

struct FinalRecensusWorkerResolution<T> {
    success: Option<T>,
    panic: Option<Box<dyn std::any::Any + Send>>,
    injection: Option<anyhow::Error>,
    natural: Option<anyhow::Error>,
}

fn run_final_recensus_workers<T, U, I, W>(
    epoch: &RunEpoch,
    inventory: I,
    worktree: W,
) -> Result<(T, U)>
where
    T: Send,
    U: Send,
    I: FnOnce() -> Result<T> + Send,
    W: FnOnce() -> Result<U> + Send,
{
    std::thread::scope(|scope| {
        let inventory_epoch = epoch.clone();
        let inventory = std::thread::Builder::new().spawn_scoped(scope, move || {
            let _epoch = inventory_epoch.install()?;
            inventory()
        });
        let worktree_epoch = epoch.clone();
        let worktree = std::thread::Builder::new().spawn_scoped(scope, move || {
            let _epoch = worktree_epoch.install()?;
            worktree()
        });

        let inventory = match inventory {
            Ok(handle) => FinalRecensusWorkerOutcome::Joined(handle.join()),
            Err(error) => FinalRecensusWorkerOutcome::Spawn(error),
        };
        let worktree = match worktree {
            Ok(handle) => FinalRecensusWorkerOutcome::Joined(handle.join()),
            Err(error) => FinalRecensusWorkerOutcome::Spawn(error),
        };

        let mut inventory = inspect_final_recensus_worker(inventory);
        let mut worktree = inspect_final_recensus_worker(worktree);

        if let Some(panic) = inventory.panic.take() {
            std::panic::resume_unwind(panic);
        }
        if let Some(panic) = worktree.panic.take() {
            std::panic::resume_unwind(panic);
        }
        match (inventory.injection.take(), worktree.injection.take()) {
            (Some(_), Some(_)) => {
                anyhow::bail!("one run epoch produced multiple final-recensus injections")
            }
            (Some(error), None) | (None, Some(error)) => return Err(error),
            (None, None) => {}
        }
        if let Some(error) = inventory.natural.take() {
            return Err(error);
        }
        if let Some(error) = worktree.natural.take() {
            return Err(error);
        }
        Ok((
            inventory
                .success
                .expect("inventory worker resolution is complete"),
            worktree
                .success
                .expect("worktree worker resolution is complete"),
        ))
    })
}

fn inspect_final_recensus_worker<T>(
    outcome: FinalRecensusWorkerOutcome<T>,
) -> FinalRecensusWorkerResolution<T> {
    let mut resolution = FinalRecensusWorkerResolution {
        success: None,
        panic: None,
        injection: None,
        natural: None,
    };
    match outcome {
        FinalRecensusWorkerOutcome::Spawn(error) => resolution.natural = Some(error.into()),
        FinalRecensusWorkerOutcome::Joined(Err(panic)) => resolution.panic = Some(panic),
        FinalRecensusWorkerOutcome::Joined(Ok(Err(error)))
            if evidence::write::is_injected_failure(&error) =>
        {
            resolution.injection = Some(error);
        }
        FinalRecensusWorkerOutcome::Joined(Ok(Err(error))) => resolution.natural = Some(error),
        FinalRecensusWorkerOutcome::Joined(Ok(Ok(success))) => resolution.success = Some(success),
    }
    resolution
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn exercise_final_recensus_workers<I, W>(
    epoch: &RunEpoch,
    inventory: I,
    worktree: W,
) -> Result<[&'static str; 2]>
where
    I: FnOnce() -> Result<()> + Send,
    W: FnOnce() -> Result<()> + Send,
{
    let ((), ()) = run_final_recensus_workers(epoch, inventory, worktree)?;
    Ok(["inventory", "worktree"])
}

fn collect_boundaries(
    manifest: &ConstellationManifest,
    repositories: &[manifest::ResolvedRepository],
    state: &mut RunState,
) -> Result<(), Box<StageFailure>> {
    for stage in [PipelineStage::Cargo, PipelineStage::Rust] {
        inject(state, stage)?;
        let result = if stage == PipelineStage::Cargo {
            collect::cargo::collect(manifest, repositories, state.graph.as_mut().expect("graph"))
        } else {
            collect::rust::collect(manifest, state.graph.as_mut().expect("graph"))
        };
        result.map_err(|error| failure(state, stage, error))?;
    }
    inject(state, PipelineStage::Node)?;
    inject(state, PipelineStage::Frontend)?;
    collect::frontend::collect(
        manifest,
        state.inventory.as_ref().expect("inventory"),
        &state.captured,
        &state.binding,
        state.graph.as_mut().expect("graph"),
    )
    .map_err(|error| failure(state, PipelineStage::Frontend, error))?;
    state.captured = CapturedInputs::default();
    inject(state, PipelineStage::Artifact)?;
    collect::artifact::collect(state.graph.as_mut().expect("graph"))
        .map_err(|error| failure(state, PipelineStage::Artifact, error))
}

fn step<T>(
    state: &RunState,
    stage: PipelineStage,
    result: Result<T>,
) -> Result<T, Box<StageFailure>> {
    result.map_err(|error| failure(state, stage, error))
}

fn inject(state: &RunState, stage: PipelineStage) -> Result<(), Box<StageFailure>> {
    step(state, stage, stage_injection(stage))
}

fn failure(
    state: &RunState,
    stage: PipelineStage,
    error: impl Into<anyhow::Error>,
) -> Box<StageFailure> {
    Box::new(StageFailure {
        stage,
        error: error.into(),
        state: state.clone(),
    })
}
