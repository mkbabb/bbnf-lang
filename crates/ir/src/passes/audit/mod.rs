//! IR audit passes — read-only `&GrammarIR` walks that report coverage
//! and consistency facts about the substrate emitted by upstream passes
//! (`project_types`, `compute_payload_layouts`, `StructRegistry`).
//!
//! Audits never mutate IR. They produce structured reports the build
//! gate or downstream tooling consumes. The first member of this module
//! is `payload_coverage`: it enumerates every typed `->` marker
//! reachable from a grammar's rule graph and reports per-grammar
//! emitter coverage, distinguishing markers that are *pending*
//! `StructRegistry` closure (W0/W1 transition) from markers that are
//! *missing* a registry entry once closure should hold.
//!
//! The directory module shape (`audit/mod.rs` + `audit/<topic>.rs`)
//! follows the established convention for IR pass families
//! (`recognizers/`, `payload/`, `types/`). New audits land as
//! sibling modules, never as flat siblings of the wider `passes/`
//! directory.

pub mod payload_coverage;

pub use payload_coverage::{
    AbsentRegistryProbe, AuditCoverageReport, GrammarAuditTag, GrammarCoverage, MarkerStatus,
    MissingMarker, PayloadLayoutsProbe, PendingMarker, StructRegistryProbe,
    audit_payload_coverage, is_typed_arrow_fn, write_coverage_report,
};
