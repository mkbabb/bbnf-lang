use std::cell::RefCell;
use std::collections::BTreeMap;
use std::ffi::OsString;
use std::marker::PhantomData;
use std::path::Path;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use super::render::{dot, enforce_budgets, json, report};
use super::transaction::{atomic_publish, write_synced};
use crate::architecture::evidence::binding::SourceBinding;
use crate::architecture::inventory::Inventory;
use crate::architecture::manifest::ConstellationManifest;
use crate::architecture::project::ProjectionSet;
use crate::architecture::schema::{
    CanonicalText, GraphArtifact, PipelineStage, RequiredFailure, Sha256Digest,
    validate_serialized_strings,
};
use crate::architecture::{RunState, StageFailure};

const FAILPOINT_ENVIRONMENT: &str = "BBNF_ARCHITECTURE_FAILPOINT";
const FAIL_STAGE_ENVIRONMENT: &str = "BBNF_ARCHITECTURE_FAIL_STAGE";

#[derive(Clone, Debug)]
pub struct RunEpoch {
    state: Arc<Mutex<RunEpochState>>,
}

#[derive(Debug)]
struct RunEpochState {
    request: Option<InjectionRequest>,
    matched: bool,
    cancelled_by_primary_failure: bool,
}

#[derive(Clone, Debug)]
enum InjectionRequest {
    Operation(OperationPoint),
    Stage { stage: PipelineStage, value: String },
}

macro_rules! operation_points {
    ($($variant:ident => $value:literal),+ $(,)?) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum OperationPoint {
            $($variant),+
        }

        impl OperationPoint {
            fn parse(value: &str) -> Option<Self> {
                match value {
                    $($value => Some(Self::$variant),)+
                    _ => None,
                }
            }

            fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $value),+
                }
            }
        }
    };
}

operation_points! {
    ArtifactBoundary => "artifact-boundary",
    CargoBoundary => "cargo-boundary",
    CurrentCreate => "current-create",
    CurrentFsync => "current-fsync",
    CurrentReplace => "current-replace",
    CurrentWrite => "current-write",
    CycleBasis => "cycle-basis",
    DynamicCluster => "dynamic-cluster",
    ExistingGenerationVerify => "existing-generation-verify",
    FileCreate => "file-create",
    FileFsync => "file-fsync",
    FileWrite => "file-write",
    FinalRecensus => "final-recensus",
    FrontendBoundary => "frontend-boundary",
    FrontendNodeSpawn => "frontend-node-spawn",
    GenerationAssembly => "generation-assembly",
    GenerationRename => "generation-rename",
    GenerationsDirectoryFsync => "generations-directory-fsync",
    GitCommand => "git-command",
    GlobalValidation => "global-validation",
    HelperDecode => "helper-decode",
    InventoryRead => "inventory-read",
    InventoryWalk => "inventory-walk",
    ManifestRead => "manifest-read",
    ProjectionBuild => "projection-build",
    PublicationLock => "publication-lock",
    Quotient => "quotient",
    RepositoryResolution => "repository-resolution",
    RootDirectoryFsync => "root-directory-fsync",
    RustBoundary => "rust-boundary",
    StagingDirectoryFsync => "staging-directory-fsync",
    ToolArDecode => "tool-ar-decode",
    ToolArRead => "tool-ar-read",
    ToolArSpawn => "tool-ar-spawn",
    ToolCargoDecode => "tool-cargo-decode",
    ToolCargoRead => "tool-cargo-read",
    ToolCargoSpawn => "tool-cargo-spawn",
    ToolCcDecode => "tool-cc-decode",
    ToolCcRead => "tool-cc-read",
    ToolCcSpawn => "tool-cc-spawn",
    ToolGitDecode => "tool-git-decode",
    ToolGitRead => "tool-git-read",
    ToolGitSpawn => "tool-git-spawn",
    ToolLdDecode => "tool-ld-decode",
    ToolLdRead => "tool-ld-read",
    ToolLdSpawn => "tool-ld-spawn",
    ToolNodeDecode => "tool-node-decode",
    ToolNodeRead => "tool-node-read",
    ToolNodeSpawn => "tool-node-spawn",
    ToolRustcDecode => "tool-rustc-decode",
    ToolRustcRead => "tool-rustc-read",
    ToolRustcSpawn => "tool-rustc-spawn",
    ToolSccacheDecode => "tool-sccache-decode",
    ToolSccacheRead => "tool-sccache-read",
    ToolSccacheSpawn => "tool-sccache-spawn",
}

#[derive(Debug)]
enum InjectedFailure {
    Operation(OperationPoint),
    Stage(PipelineStage),
}

impl std::fmt::Display for InjectedFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operation(operation) => write!(
                formatter,
                "INJECTED_FAILURE at real operation `{}`",
                operation.as_str()
            ),
            Self::Stage(stage) => {
                write!(formatter, "INJECTED_FAILURE at typed stage {stage:?}")
            }
        }
    }
}

impl std::error::Error for InjectedFailure {}

pub(crate) fn is_injected_failure(error: &anyhow::Error) -> bool {
    error.downcast_ref::<InjectedFailure>().is_some()
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RunEpochReceipt {
    pub matched: u8,
    pub pending: bool,
    pub cancelled_by_primary_failure: bool,
}

thread_local! {
    static CURRENT_RUN_EPOCH: RefCell<Option<RunEpoch>> = const { RefCell::new(None) };
}

pub(crate) struct RunEpochGuard {
    _not_send_or_sync: PhantomData<Rc<()>>,
}

impl RunEpoch {
    fn disabled_epoch() -> Self {
        Self::new(None)
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn disabled() -> Self {
        Self::disabled_epoch()
    }

    pub(crate) fn capture_environment() -> Result<Self> {
        let failpoint = std::env::var_os(FAILPOINT_ENVIRONMENT);
        let stage = std::env::var_os(FAIL_STAGE_ENVIRONMENT);
        Self::from_os_values(failpoint, stage)
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn for_test(failpoint: Option<&str>, stage: Option<&str>) -> Result<Self> {
        Self::from_values(failpoint.map(str::to_owned), stage.map(str::to_owned))
    }

    pub(crate) fn install(&self) -> Result<RunEpochGuard> {
        CURRENT_RUN_EPOCH
            .try_with(|current| {
                let mut current = current
                    .try_borrow_mut()
                    .map_err(|_| anyhow::anyhow!("architecture run epoch is already borrowed"))?;
                if current.is_some() {
                    anyhow::bail!("nested architecture run epoch installation is forbidden");
                }
                *current = Some(self.clone());
                Ok(RunEpochGuard {
                    _not_send_or_sync: PhantomData,
                })
            })
            .map_err(|_| anyhow::anyhow!("architecture run epoch TLS is unavailable"))?
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn scope(&self) -> Result<impl Drop> {
        self.install()
    }

    #[cfg(feature = "architecture-test-seams")]
    #[doc(hidden)]
    pub fn receipt(&self) -> Result<RunEpochReceipt> {
        let state = self.lock()?;
        Ok(RunEpochReceipt {
            matched: u8::from(state.matched),
            pending: state.request.is_some()
                && !state.matched
                && !state.cancelled_by_primary_failure,
            cancelled_by_primary_failure: state.cancelled_by_primary_failure,
        })
    }

    pub(crate) fn current() -> Result<Self> {
        CURRENT_RUN_EPOCH
            .try_with(|current| {
                current
                    .try_borrow()
                    .map(|current| current.clone().unwrap_or_else(Self::disabled_epoch))
                    .map_err(|_| anyhow::anyhow!("architecture run epoch is already borrowed"))
            })
            .map_err(|_| anyhow::anyhow!("architecture run epoch TLS is unavailable"))?
    }

    pub(crate) fn cancel_unconsumed(&self) -> Result<()> {
        let mut state = self.lock()?;
        if state.request.is_some() && !state.matched {
            state.cancelled_by_primary_failure = true;
        }
        Ok(())
    }

    pub(crate) fn injection_environment_value(&self, name: &str) -> Result<Option<String>> {
        Ok(match (name, self.lock()?.request.as_ref()) {
            (FAILPOINT_ENVIRONMENT, Some(InjectionRequest::Operation(point))) => {
                Some(point.as_str().to_owned())
            }
            (FAIL_STAGE_ENVIRONMENT, Some(InjectionRequest::Stage { value, .. })) => {
                Some(value.clone())
            }
            _ => None,
        })
    }

    fn new(request: Option<InjectionRequest>) -> Self {
        Self {
            state: Arc::new(Mutex::new(RunEpochState {
                request,
                matched: false,
                cancelled_by_primary_failure: false,
            })),
        }
    }

    fn from_os_values(failpoint: Option<OsString>, stage: Option<OsString>) -> Result<Self> {
        if failpoint.is_some() && stage.is_some() {
            anyhow::bail!(
                "BBNF_ARCHITECTURE_FAILPOINT and BBNF_ARCHITECTURE_FAIL_STAGE cannot both be set"
            );
        }
        let failpoint = failpoint
            .map(|value| {
                value
                    .into_string()
                    .map_err(|_| anyhow::anyhow!("injection failpoint is not UTF-8"))
            })
            .transpose()?;
        let stage = stage
            .map(|value| {
                value
                    .into_string()
                    .map_err(|_| anyhow::anyhow!("injection stage is not UTF-8"))
            })
            .transpose()?;
        Self::from_values(failpoint, stage)
    }

    fn from_values(failpoint: Option<String>, stage: Option<String>) -> Result<Self> {
        if failpoint.is_some() && stage.is_some() {
            anyhow::bail!(
                "BBNF_ARCHITECTURE_FAILPOINT and BBNF_ARCHITECTURE_FAIL_STAGE cannot both be set"
            );
        }
        let request = if let Some(operation) = failpoint {
            let point = OperationPoint::parse(&operation).with_context(|| {
                format!("unknown architecture failure injection operation `{operation}`")
            })?;
            Some(InjectionRequest::Operation(point))
        } else if let Some(value) = stage {
            let stage = PipelineStage::parse(&value)
                .context("unknown architecture failure injection stage")?;
            Some(InjectionRequest::Stage { stage, value })
        } else {
            None
        };
        Ok(Self::new(request))
    }

    fn consume_operation(&self, operation: OperationPoint) -> Result<bool> {
        let mut state = self.lock()?;
        let matches = !state.matched
            && !state.cancelled_by_primary_failure
            && matches!(
                &state.request,
                Some(InjectionRequest::Operation(requested)) if *requested == operation
            );
        if matches {
            state.matched = true;
        }
        Ok(matches)
    }

    fn consume_stage(&self, stage: PipelineStage) -> Result<bool> {
        let mut state = self.lock()?;
        let matches = !state.matched
            && !state.cancelled_by_primary_failure
            && matches!(
                &state.request,
                Some(InjectionRequest::Stage { stage: requested, .. }) if *requested == stage
            );
        if matches {
            state.matched = true;
        }
        Ok(matches)
    }

    fn lock(&self) -> Result<std::sync::MutexGuard<'_, RunEpochState>> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("ARCHITECTURE_RUN_EPOCH_POISONED"))
    }
}

impl Drop for RunEpochGuard {
    fn drop(&mut self) {
        let _ = CURRENT_RUN_EPOCH.try_with(|current| {
            if let Ok(mut current) = current.try_borrow_mut() {
                *current = None;
            }
        });
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GenerationStatus {
    Red,
    Failure,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Diagnostic {
    pub stage: PipelineStage,
    pub message: CanonicalText,
    pub chain: Vec<CanonicalText>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MovementAuthorizationStatus {
    Denied,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct MovementAuthorization {
    pub schema: CanonicalText,
    pub status: MovementAuthorizationStatus,
    pub receipt: Option<CanonicalText>,
    pub reason: CanonicalText,
}

impl MovementAuthorization {
    fn denied() -> Result<Self> {
        Ok(Self {
            schema: CanonicalText::new("bbnf.architecture.movement-authorization.v3.1")?,
            status: MovementAuthorizationStatus::Denied,
            receipt: None,
            reason: CanonicalText::new(
                "global architecture is RED and no separate authorization receipt exists",
            )?,
        })
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum ArtifactPublication {
    Published {
        size: u64,
        hash: Sha256Digest,
    },
    Unavailable {
        stage: PipelineStage,
        reason: CanonicalText,
    },
    PublicationRoot,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Generation {
    pub schema: CanonicalText,
    pub run_id: CanonicalText,
    pub status: GenerationStatus,
    pub stage: PipelineStage,
    pub source_binding: Sha256Digest,
    pub movement_authorization: MovementAuthorization,
    pub expected_files: Vec<CanonicalText>,
    pub artifacts: BTreeMap<CanonicalText, ArtifactPublication>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub(crate) struct PublicationReceipt {
    pub(crate) schema: CanonicalText,
    pub(crate) status: GenerationStatus,
    pub(crate) stage: PipelineStage,
    pub(crate) source_binding: Sha256Digest,
    pub(crate) movement_authorization: MovementAuthorization,
    pub(crate) expected_files: Vec<CanonicalText>,
    pub(crate) artifacts: BTreeMap<CanonicalText, ArtifactPublication>,
    pub(crate) diagnostics: Vec<Diagnostic>,
}

impl Generation {
    pub(crate) fn receipt(&self) -> PublicationReceipt {
        PublicationReceipt {
            schema: self.schema.clone(),
            status: self.status,
            stage: self.stage,
            source_binding: self.source_binding.clone(),
            movement_authorization: self.movement_authorization.clone(),
            expected_files: self.expected_files.clone(),
            artifacts: self.artifacts.clone(),
            diagnostics: self.diagnostics.clone(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct GenerationSummary {
    schema: CanonicalText,
    status: GenerationStatus,
    stage: PipelineStage,
    source_binding: Sha256Digest,
    movement_authorization: MovementAuthorization,
    diagnostics: Vec<Diagnostic>,
}

pub struct EvidencePayload<'a> {
    pub binding: &'a SourceBinding,
    pub manifest: Option<&'a ConstellationManifest>,
    pub inventory: Option<&'a Inventory>,
    pub graph: Option<&'a GraphArtifact>,
    pub graph_validated: bool,
    pub projections: Option<&'a ProjectionSet>,
    pub failures: Option<&'a [RequiredFailure]>,
    pub diagnostics: Vec<Diagnostic>,
    pub stage: PipelineStage,
    pub status: GenerationStatus,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssembledGeneration {
    pub files: BTreeMap<String, Vec<u8>>,
    pub generation: Generation,
}

#[derive(Clone)]
struct MemberIdentity {
    size: usize,
    hash: Sha256Digest,
}

trait GenerationSink {
    fn insert(&mut self, name: &str, bytes: Vec<u8>) -> Result<()>;
    fn member(&self, name: &str) -> Option<&MemberIdentity>;
    fn names(&self) -> Vec<String>;
    fn total_bytes(&self) -> usize;
}

#[derive(Default)]
struct MemorySink {
    files: BTreeMap<String, Vec<u8>>,
    members: BTreeMap<String, MemberIdentity>,
    total_bytes: usize,
}

impl GenerationSink for MemorySink {
    fn insert(&mut self, name: &str, bytes: Vec<u8>) -> Result<()> {
        let member = member_identity(&bytes);
        if self.files.insert(name.into(), bytes).is_some()
            || self.members.insert(name.into(), member.clone()).is_some()
        {
            bail!("duplicate generation member `{name}`");
        }
        self.total_bytes += member.size;
        Ok(())
    }

    fn member(&self, name: &str) -> Option<&MemberIdentity> {
        self.members.get(name)
    }

    fn names(&self) -> Vec<String> {
        self.files.keys().cloned().collect()
    }

    fn total_bytes(&self) -> usize {
        self.total_bytes
    }
}

struct DirectorySink<'a> {
    directory: &'a Path,
    members: BTreeMap<String, MemberIdentity>,
    total_bytes: usize,
}

impl<'a> DirectorySink<'a> {
    fn new(directory: &'a Path) -> Self {
        Self {
            directory,
            members: BTreeMap::new(),
            total_bytes: 0,
        }
    }
}

impl GenerationSink for DirectorySink<'_> {
    fn insert(&mut self, name: &str, bytes: Vec<u8>) -> Result<()> {
        if self.members.contains_key(name) {
            bail!("duplicate generation member `{name}`");
        }
        let member = member_identity(&bytes);
        write_synced(&self.directory.join(name), &bytes)?;
        self.total_bytes += member.size;
        self.members.insert(name.into(), member);
        Ok(())
    }

    fn member(&self, name: &str) -> Option<&MemberIdentity> {
        self.members.get(name)
    }

    fn names(&self) -> Vec<String> {
        self.members.keys().cloned().collect()
    }

    fn total_bytes(&self) -> usize {
        self.total_bytes
    }
}

fn member_identity(bytes: &[u8]) -> MemberIdentity {
    MemberIdentity {
        size: bytes.len(),
        hash: Sha256Digest::of(bytes),
    }
}

pub(crate) fn publish_complete(output: &Path, root: &Path, state: RunState) -> Result<()> {
    if let Err(error) =
        stage_injection(PipelineStage::Generation).and_then(|()| failpoint("generation-assembly"))
    {
        return publish_failure(
            output,
            root,
            StageFailure {
                stage: PipelineStage::Generation,
                error,
                state,
            },
        );
    }
    let failures = state
        .graph
        .as_ref()
        .expect("complete graph")
        .required_failures();
    if let Err(error) = stage_injection(PipelineStage::Write) {
        return publish_failure(
            output,
            root,
            StageFailure {
                stage: PipelineStage::Write,
                error,
                state,
            },
        );
    }
    let generation = match publish(
        output,
        payload(
            &state,
            &failures,
            Vec::new(),
            PipelineStage::Complete,
            GenerationStatus::Red,
        ),
    ) {
        Ok(generation) => generation,
        Err(error) => {
            let stage = if error.to_string().contains("RESOURCE_BUDGET_EXCEEDED") {
                PipelineStage::Generation
            } else {
                PipelineStage::Write
            };
            return publish_failure(
                output,
                root,
                StageFailure {
                    stage,
                    error,
                    state,
                },
            );
        }
    };
    if let Err(error) = stage_injection(PipelineStage::Complete) {
        bail!(
            "architecture P3.1 completed safe RED publication {} before complete-stage injection failed: {error}",
            generation.run_id
        );
    }
    let graph = state.graph.as_ref().expect("complete graph");
    eprintln!(
        "architecture P3.1 generation {}: nodes={} edges={} required={}; global architecture RED, movement denied",
        generation.run_id,
        graph.nodes().len(),
        graph.edges().len(),
        failures.len()
    );
    bail!(
        "architecture P3.1 remains RED with {} required Unknowns; evidence generation {} published",
        failures.len(),
        generation.run_id
    )
}

pub(crate) fn publish_failure(output: &Path, root: &Path, failure: StageFailure) -> Result<()> {
    RunEpoch::current()?.cancel_unconsumed()?;
    let diagnostics = vec![diagnostic(failure.stage, &failure.error, root)?];
    let failures = failure
        .state
        .graph
        .as_ref()
        .map(GraphArtifact::required_failures)
        .unwrap_or_default();
    match publish(
        output,
        payload(
            &failure.state,
            &failures,
            diagnostics,
            failure.stage,
            GenerationStatus::Failure,
        ),
    ) {
        Ok(generation) => bail!(
            "architecture P3.1 failed at {:?}; stage-safe failure generation {} published",
            failure.stage,
            generation.run_id
        ),
        Err(publication) => bail!(
            "architecture P3.1 failed at {:?}: {}; failure generation was not published and previous CURRENT was preserved: {}",
            failure.stage,
            failure.error,
            publication
        ),
    }
}

fn payload<'a>(
    state: &'a RunState,
    failures: &'a [RequiredFailure],
    diagnostics: Vec<Diagnostic>,
    stage: PipelineStage,
    status: GenerationStatus,
) -> EvidencePayload<'a> {
    EvidencePayload {
        binding: &state.binding,
        manifest: state.manifest.as_ref(),
        inventory: state.inventory.as_ref(),
        graph: state.graph.as_ref(),
        graph_validated: state.graph_validated,
        projections: state.projections.as_ref(),
        failures: Some(failures),
        diagnostics,
        stage,
        status,
    }
}

fn diagnostic(stage: PipelineStage, error: &anyhow::Error, root: &Path) -> Result<Diagnostic> {
    let portable = |text: &str| {
        let output = text.replace(root.to_string_lossy().as_ref(), "bbnf:");
        root.parent().map_or(output.clone(), |parent| {
            output.replace(
                parent.join("parse-that").to_string_lossy().as_ref(),
                "parse-that:",
            )
        })
    };
    let safe = |text: String| {
        CanonicalText::new(text).or_else(|_| {
            CanonicalText::new("diagnostic redacted because it contained a private path")
        })
    };
    Ok(Diagnostic {
        stage,
        message: safe(portable(&error.to_string()))?,
        chain: error
            .chain()
            .skip(1)
            .map(|cause| safe(portable(&cause.to_string())))
            .collect::<Result<_>>()?,
    })
}

pub fn publish(output: &Path, payload: EvidencePayload<'_>) -> Result<Generation> {
    atomic_publish(output, |stage| {
        let mut sink = DirectorySink::new(stage);
        let generation = assemble_into(payload, &mut sink)?;
        enforce_budgets(
            sink.total_bytes(),
            sink.member("projections.json").map(|member| member.size),
            generation.status,
            generation.stage,
        )?;
        Ok(generation)
    })
}

pub fn assemble(payload: EvidencePayload<'_>) -> Result<AssembledGeneration> {
    let mut sink = MemorySink::default();
    let generation = assemble_into(payload, &mut sink)?;
    Ok(AssembledGeneration {
        files: sink.files,
        generation,
    })
}

fn emit_json<T: Serialize + ?Sized>(
    sink: &mut impl GenerationSink,
    name: &str,
    value: &T,
) -> Result<()> {
    sink.insert(name, json(value)?)
}

fn emit_derived(sink: &mut impl GenerationSink, projections: &ProjectionSet) -> Result<()> {
    let condensation = projections
        .projections
        .iter()
        .map(|projection| (&projection.spec.name, &projection.components))
        .collect::<Vec<_>>();
    emit_json(sink, "condensation.json", &condensation)?;
    let bases = projections
        .projections
        .iter()
        .map(|projection| (&projection.spec.name, &projection.component_bases))
        .collect::<Vec<_>>();
    emit_json(sink, "basis.json", &bases)?;
    let quotient = projections
        .projections
        .iter()
        .map(|projection| (&projection.spec.name, &projection.quotient))
        .collect::<Vec<_>>();
    emit_json(sink, "quotient.json", &quotient)?;
    let shards = projections
        .projections
        .iter()
        .map(|projection| (&projection.spec.name, &projection.clusters))
        .collect::<Vec<_>>();
    emit_json(sink, "shards.json", &shards)
}

fn assemble_into(
    payload: EvidencePayload<'_>,
    sink: &mut impl GenerationSink,
) -> Result<Generation> {
    emit_json(sink, "binding.json", payload.binding)?;
    emit_json(sink, "diagnostics.json", &payload.diagnostics)?;
    if let Some(manifest) = payload.manifest {
        emit_json(sink, "manifest.json", manifest)?;
    }
    if let Some(inventory) = payload.inventory {
        emit_json(sink, "inventory.json", inventory)?;
    }
    if let Some(graph) = payload.graph {
        let name = if payload.graph_validated {
            "graph.json"
        } else {
            "candidate-graph.json"
        };
        emit_json(sink, name, graph)?;
        emit_json(
            sink,
            "semantic-ledger.json",
            &crate::architecture::collect::artifact::required_red_ledger()?,
        )?;
    }
    if let Some(projections) = payload.projections {
        emit_json(sink, "projections.json", projections)?;
        emit_derived(sink, projections)?;
    }
    if let Some(failures) = payload.failures {
        emit_json(sink, "obligations.json", failures)?;
    }
    if payload.graph_validated
        && let Some(graph) = payload.graph
    {
        sink.insert("graph.dot", dot(graph)?)?;
        sink.insert(
            "report.txt",
            report(graph, payload.projections, payload.failures),
        )?;
    }

    let movement_authorization = MovementAuthorization::denied()?;
    let schema = CanonicalText::new("bbnf.architecture.generation.v3.1")?;
    emit_json(
        sink,
        "generation.json",
        &GenerationSummary {
            schema: schema.clone(),
            status: payload.status,
            stage: payload.stage,
            source_binding: payload.binding.binding_hash.clone(),
            movement_authorization: movement_authorization.clone(),
            diagnostics: payload.diagnostics.clone(),
        },
    )?;

    let roles = [
        "binding.json",
        "diagnostics.json",
        "manifest.json",
        "inventory.json",
        "graph.json",
        "candidate-graph.json",
        "projections.json",
        "obligations.json",
        "graph.dot",
        "report.txt",
        "condensation.json",
        "basis.json",
        "quotient.json",
        "shards.json",
        "semantic-ledger.json",
        "generation.json",
    ];
    let mut artifacts = BTreeMap::new();
    for name in roles {
        let key = CanonicalText::new(name)?;
        artifacts.insert(
            key,
            sink.member(name).map_or_else(
                || {
                    Ok::<_, anyhow::Error>(ArtifactPublication::Unavailable {
                        stage: payload.stage,
                        reason: CanonicalText::new("stage did not produce this typed artifact")?,
                    })
                },
                |member| {
                    Ok(ArtifactPublication::Published {
                        size: member.size as u64,
                        hash: member.hash.clone(),
                    })
                },
            )?,
        );
    }
    artifacts.insert(
        CanonicalText::new("publication.json")?,
        ArtifactPublication::PublicationRoot,
    );
    let mut expected_files = sink
        .names()
        .into_iter()
        .map(CanonicalText::new)
        .collect::<Result<Vec<_>>>()?;
    expected_files.push(CanonicalText::new("publication.json")?);
    expected_files.sort();
    let receipt = PublicationReceipt {
        schema: schema.clone(),
        status: payload.status,
        stage: payload.stage,
        source_binding: payload.binding.binding_hash.clone(),
        movement_authorization: movement_authorization.clone(),
        expected_files: expected_files.clone(),
        artifacts: artifacts.clone(),
        diagnostics: payload.diagnostics.clone(),
    };
    validate_serialized_strings(&receipt)?;
    emit_json(sink, "publication.json", &receipt)?;
    let run_id = CanonicalText::new(format!(
        "p31-{}",
        sink.member("publication.json")
            .context("publication trust root is absent")?
            .hash
    ))?;
    let generation = Generation {
        schema,
        run_id: run_id.clone(),
        status: payload.status,
        stage: payload.stage,
        source_binding: payload.binding.binding_hash.clone(),
        movement_authorization,
        expected_files,
        artifacts,
        diagnostics: payload.diagnostics,
    };
    Ok(generation)
}

pub(crate) fn failpoint(name: &str) -> Result<()> {
    let operation = OperationPoint::parse(name)
        .with_context(|| format!("unknown architecture failure injection operation `{name}`"))?;
    if RunEpoch::current()?.consume_operation(operation)? {
        return Err(InjectedFailure::Operation(operation).into());
    }
    Ok(())
}

pub(crate) fn stage_injection(stage: PipelineStage) -> Result<()> {
    if RunEpoch::current()?.consume_stage(stage)? {
        return Err(InjectedFailure::Stage(stage).into());
    }
    Ok(())
}
