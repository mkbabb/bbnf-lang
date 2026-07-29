use core::fmt;

use serde::Serialize;
use serde_json::Value;

use super::analysis::AnalysisFacts;
use super::dependency::DependencySolution;
use super::diagnostic::DiagnosticSet;
use super::emit::GeneratedModule;
use super::event::ScalarEventPlan;
use super::lower::LoweringPlan;
use super::source::{Revision, SourceMap};
use super::syntax::{NodeRemap, Program};
use super::typecheck::TypeSolution;
use super::{CompileOptions, CompiledModule, ModuleInput, StageResult, compile_module};

const SOURCE_MAP_REPLAY_SCHEMA: &str = "bbnf.source-map.v1";
const PROGRAM_REPLAY_SCHEMA: &str = "bbnf.program.v1";
const DEPENDENCY_REPLAY_SCHEMA: &str = "bbnf.dependency-solution.v1";
const TYPE_REPLAY_SCHEMA: &str = "bbnf.type-solution.v1";
const ANALYSIS_REPLAY_SCHEMA: &str = "bbnf.analysis-facts.v1";
const LOWERING_REPLAY_SCHEMA: &str = "bbnf.lowering-plan.v1";
const NODE_REMAP_REPLAY_SCHEMA: &str = "bbnf.node-remap.v1";
const GENERATED_REPLAY_SCHEMA: &str = "bbnf.generated-module.v1";
const COMPILATION_REPLAY_SCHEMA: &str = "bbnf.compiled-module.v1";

#[derive(Serialize)]
struct ReplayEnvelope<'a, T> {
    schema: &'a str,
    revision: String,
    value: &'a T,
}

fn serialize_stage<T: Serialize>(
    schema: &str,
    revision: Revision,
    value: &T,
) -> Result<Vec<u8>, serde_json::Error> {
    serde_json::to_vec(&ReplayEnvelope {
        schema,
        revision: revision.hex(),
        value,
    })
}

impl SourceMap {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(SOURCE_MAP_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl Program {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(PROGRAM_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl DependencySolution {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(DEPENDENCY_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl TypeSolution {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(TYPE_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl AnalysisFacts {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(ANALYSIS_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl LoweringPlan {
    /// Serialize this sealed lowering product with an explicit schema and
    /// revision. Field and collection order are deterministic.
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(LOWERING_REPLAY_SCHEMA, self.revision(), self)
    }
}

impl ScalarEventPlan {
    /// Serialize this sealed event product with its event schema and revision.
    /// The embedded lowering plan makes the replay record self-contained.
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(self.schema(), self.revision(), self)
    }
}

impl NodeRemap {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(NODE_REMAP_REPLAY_SCHEMA, self.to_revision(), self)
    }
}

#[derive(Serialize)]
struct GeneratedReplay<'a> {
    source: &'a str,
    manifest: &'a str,
}

impl GeneratedModule {
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(
            GENERATED_REPLAY_SCHEMA,
            self.revision(),
            &GeneratedReplay {
                source: self.source(),
                manifest: self.manifest(),
            },
        )
    }
}

#[derive(Serialize)]
struct CompilationReplay<'a> {
    source_map: &'a SourceMap,
    program: &'a Program,
    remap: &'a NodeRemap,
    dependency: &'a DependencySolution,
    types: &'a TypeSolution,
    facts: &'a AnalysisFacts,
    lowering: &'a LoweringPlan,
    events: &'a ScalarEventPlan,
    generated_source: &'a str,
    generated_manifest: &'a str,
}

impl CompiledModule {
    /// Serialize the complete immutable carrier as one revision-bound replay
    /// packet. The packet contains every stage and the exact generated product.
    pub fn replay_json(&self) -> Result<Vec<u8>, serde_json::Error> {
        serialize_stage(
            COMPILATION_REPLAY_SCHEMA,
            self.source_map.revision(),
            &CompilationReplay {
                source_map: &self.source_map,
                program: &self.program,
                remap: &self.remap,
                dependency: &self.dependency,
                types: &self.types,
                facts: &self.facts,
                lowering: &self.lowering,
                events: &self.events,
                generated_source: self.generated.source(),
                generated_manifest: self.generated.manifest(),
            },
        )
    }
}

/// A canonical field in the complete compilation replay packet.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ReplayComponent {
    SourceMap,
    Program,
    NodeRemap,
    Dependency,
    Types,
    Analysis,
    Lowering,
    Events,
    GeneratedSource,
    GeneratedManifest,
}

impl ReplayComponent {
    const fn field(self) -> &'static str {
        match self {
            Self::SourceMap => "source_map",
            Self::Program => "program",
            Self::NodeRemap => "remap",
            Self::Dependency => "dependency",
            Self::Types => "types",
            Self::Analysis => "facts",
            Self::Lowering => "lowering",
            Self::Events => "events",
            Self::GeneratedSource => "generated_source",
            Self::GeneratedManifest => "generated_manifest",
        }
    }
}

/// Why a complete compilation replay packet could not be validated.
///
/// Replay never trusts serialized compiler carriers. It extracts only the
/// logical source identity, recompiles that source under caller-supplied
/// options, and then compares every carrier and generated byte against the
/// canonical packet produced by that recompilation.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ReplayError {
    InvalidJson {
        message: String,
    },
    InvalidPacket {
        field: &'static str,
    },
    SchemaMismatch {
        expected: &'static str,
        found: Option<String>,
    },
    InvalidRevision {
        found: String,
    },
    RevisionMismatch {
        packet: String,
        rebuilt: String,
    },
    CompilationRejected {
        diagnostics: DiagnosticSet,
    },
    ValueMismatch {
        component: ReplayComponent,
    },
    NonCanonical,
    Serialization {
        message: String,
    },
}

impl fmt::Display for ReplayError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidJson { message } => write!(formatter, "invalid replay JSON: {message}"),
            Self::InvalidPacket { field } => {
                write!(formatter, "invalid or missing replay field `{field}`")
            }
            Self::SchemaMismatch { expected, found } => write!(
                formatter,
                "replay schema mismatch: expected `{expected}`, found {}",
                found.as_deref().unwrap_or("<non-string or missing>")
            ),
            Self::InvalidRevision { found } => {
                write!(formatter, "invalid canonical replay revision `{found}`")
            }
            Self::RevisionMismatch { packet, rebuilt } => write!(
                formatter,
                "replay revision mismatch: packet `{packet}`, rebuilt `{rebuilt}`"
            ),
            Self::CompilationRejected { diagnostics } => write!(
                formatter,
                "replayed source was rejected with {} diagnostic(s)",
                diagnostics.iter().len()
            ),
            Self::ValueMismatch { component } => {
                write!(
                    formatter,
                    "replay field `{}` is not canonical",
                    component.field()
                )
            }
            Self::NonCanonical => formatter.write_str("replay packet bytes are not canonical"),
            Self::Serialization { message } => {
                write!(formatter, "could not serialize rebuilt replay: {message}")
            }
        }
    }
}

impl std::error::Error for ReplayError {}

/// Validate and reconstruct a complete compilation replay packet.
///
/// This is the inverse of [`CompiledModule::replay_json`] for the complete
/// packet only. Reconstruction is deliberately by compilation rather than by
/// deserializing internal carriers, so stale or forged stage products cannot
/// cross the replay boundary.
pub fn replay_compilation(
    packet: &[u8],
    options: CompileOptions,
) -> Result<CompiledModule, ReplayError> {
    let supplied: Value =
        serde_json::from_slice(packet).map_err(|error| ReplayError::InvalidJson {
            message: error.to_string(),
        })?;
    let envelope = supplied.as_object().ok_or(ReplayError::InvalidPacket {
        field: "<envelope>",
    })?;

    let found_schema = envelope
        .get("schema")
        .and_then(Value::as_str)
        .map(str::to_owned);
    if found_schema.as_deref() != Some(COMPILATION_REPLAY_SCHEMA) {
        return Err(ReplayError::SchemaMismatch {
            expected: COMPILATION_REPLAY_SCHEMA,
            found: found_schema,
        });
    }

    let packet_revision = envelope
        .get("revision")
        .and_then(Value::as_str)
        .ok_or(ReplayError::InvalidPacket { field: "revision" })?;
    if packet_revision.len() != 64
        || !packet_revision
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
    {
        return Err(ReplayError::InvalidRevision {
            found: packet_revision.to_owned(),
        });
    }

    let value = envelope
        .get("value")
        .and_then(Value::as_object)
        .ok_or(ReplayError::InvalidPacket { field: "value" })?;
    let source_map =
        value
            .get("source_map")
            .and_then(Value::as_object)
            .ok_or(ReplayError::InvalidPacket {
                field: "value.source_map",
            })?;
    let modules =
        source_map
            .get("modules")
            .and_then(Value::as_array)
            .ok_or(ReplayError::InvalidPacket {
                field: "value.source_map.modules",
            })?;
    if modules.len() != 1 {
        return Err(ReplayError::InvalidPacket {
            field: "value.source_map.modules[0]",
        });
    }
    let source = modules[0].as_object().ok_or(ReplayError::InvalidPacket {
        field: "value.source_map.modules[0]",
    })?;
    let logical_path =
        source
            .get("logical_path")
            .and_then(Value::as_str)
            .ok_or(ReplayError::InvalidPacket {
                field: "value.source_map.modules[0].logical_path",
            })?;
    let encoded_bytes =
        source
            .get("bytes")
            .and_then(Value::as_array)
            .ok_or(ReplayError::InvalidPacket {
                field: "value.source_map.modules[0].bytes",
            })?;
    let source_bytes = encoded_bytes
        .iter()
        .enumerate()
        .map(|(index, value)| {
            value
                .as_u64()
                .and_then(|byte| u8::try_from(byte).ok())
                .ok_or(ReplayError::InvalidPacket {
                    field: if index == 0 {
                        "value.source_map.modules[0].bytes[0]"
                    } else {
                        "value.source_map.modules[0].bytes[*]"
                    },
                })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let rebuilt = match compile_module(ModuleInput::new(logical_path, &source_bytes), options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            return Err(ReplayError::CompilationRejected { diagnostics });
        }
    };
    let rebuilt_revision = rebuilt.source_map.revision().hex();
    if packet_revision != rebuilt_revision {
        return Err(ReplayError::RevisionMismatch {
            packet: packet_revision.to_owned(),
            rebuilt: rebuilt_revision,
        });
    }

    let canonical_packet = rebuilt
        .replay_json()
        .map_err(|error| ReplayError::Serialization {
            message: error.to_string(),
        })?;
    let canonical: Value =
        serde_json::from_slice(&canonical_packet).map_err(|error| ReplayError::Serialization {
            message: error.to_string(),
        })?;
    let canonical_value = canonical
        .get("value")
        .and_then(Value::as_object)
        .expect("compiler replay serialization always contains an object value");

    for component in [
        ReplayComponent::SourceMap,
        ReplayComponent::Program,
        ReplayComponent::NodeRemap,
        ReplayComponent::Dependency,
        ReplayComponent::Types,
        ReplayComponent::Analysis,
        ReplayComponent::Lowering,
        ReplayComponent::Events,
        ReplayComponent::GeneratedSource,
        ReplayComponent::GeneratedManifest,
    ] {
        if value.get(component.field()) != canonical_value.get(component.field()) {
            return Err(ReplayError::ValueMismatch { component });
        }
    }

    if packet != canonical_packet {
        return Err(ReplayError::NonCanonical);
    }

    Ok(rebuilt)
}

impl CompiledModule {
    /// Reconstruct this carrier only after source recompilation and complete
    /// canonical packet validation.
    pub fn from_replay_json(packet: &[u8], options: CompileOptions) -> Result<Self, ReplayError> {
        replay_compilation(packet, options)
    }
}
