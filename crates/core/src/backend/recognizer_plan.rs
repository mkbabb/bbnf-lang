//! Scanner-planning unification bridge (Tranche X.8f).
//!
//! A small backend-facing record that exposes the authoritative
//! scanner plan computed in IR. The goal is singular: give every
//! emission site one struct to read from so `scanner_plan.rs`,
//! `dispatch.rs`, and `kernels::*` stop looking like loosely-coupled
//! subsystems whose agreement is enforced by grep gates rather than
//! by data flow.
//!
//! The record is a projection over data that already exists on
//! `GrammarIR` — `recognizer_decisions`, `node_facts` /
//! `Recognizer.peer_group`, and the delim-scan / key-dispatch
//! sidecars. Nothing new is computed; this is the single view the
//! three consumer sites read through.

use bbnf_ir::dag::NodeId;
use bbnf_ir::passes::csp_strategy::{AltMode, RegexEngine, WrapMode};
use bbnf_ir::passes::patterns::{GroupId, RecognizerShape};
use bbnf_ir::{GrammarIR, IrNode};

/// A classified family for a recognizer node. Unlike the raw
/// `RecognizerShape`, this enum is kernel-emitter-facing — it maps
/// one-to-one onto the per-family kernel modules the Rust backend
/// emits through (`kernels::*`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecognizerFamily {
    /// `"a" | "b" | ...` delimiter-balanced `{...}` / `[...]` / `(...)`.
    DelimiterBalanced,
    /// `element (sep element)*`.
    SeparatorList,
    /// Alt with disjoint leading tokens.
    TokenLedBranches,
    /// Literal-prefixed keyword dispatch.
    KeywordPrefix,
    /// Pure regex pattern.
    Regex,
    /// No recognized family — emission falls through to the generic
    /// driver path.
    Generic,
}

/// Light-weight hint about the kernel family's output shape. The
/// backend uses this to pick the right emission path without
/// re-classifying the recognizer shape.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitHint {
    /// Emit a delim-scan scanner body.
    DelimScan,
    /// Emit a key-dispatch jump table.
    KeyDispatch,
    /// Emit an alt byte-dispatch table.
    ByteDispatch,
    /// Emit a kernel fast-path scanner for a regex family.
    KernelScanner,
    /// Emit via the generic node dispatcher.
    Generic,
}

/// One backend-facing record per node. Projects `(family,
/// regex_engine, emit_hint, peer_group)` over the existing IR
/// substrate in one struct so consumer sites read through a single
/// view.
#[derive(Clone, Debug)]
pub struct ScannerPlanRecord {
    pub family: RecognizerFamily,
    pub regex_engine: Option<RegexEngine>,
    pub emit_hint: EmitHint,
    pub peer_group: Option<GroupId>,
}

impl ScannerPlanRecord {
    /// Default generic plan — no recognized family, backend falls
    /// through to the generic dispatcher.
    pub fn generic() -> Self {
        Self {
            family: RecognizerFamily::Generic,
            regex_engine: None,
            emit_hint: EmitHint::Generic,
            peer_group: None,
        }
    }
}

/// Look up the authoritative `ScannerPlanRecord` for a node.
///
/// Projects from `ir.recognizer_decisions` + `ir.node_facts` +
/// `ir.delim_scan_configs` / `ir.key_dispatch_configs`. The
/// projection is deterministic — the same `(ir, node)` pair always
/// returns the same record — and constant-time modulo the
/// `HashMap` lookups the IR sidecars already store.
pub fn scanner_plan_for(ir: &GrammarIR, node: &IrNode) -> ScannerPlanRecord {
    let Some(dag) = ir.dag.as_ref() else {
        return ScannerPlanRecord::generic();
    };
    let Some(node_id) = dag.node_for(node) else {
        return ScannerPlanRecord::generic();
    };
    plan_for_id(ir, node_id)
}

/// Same as [`scanner_plan_for`] but takes a pre-resolved `NodeId`.
pub fn plan_for_id(ir: &GrammarIR, node_id: NodeId) -> ScannerPlanRecord {
    let decision = ir.recognizer_decisions.get(&node_id);
    let recognizer = ir
        .node_facts
        .get(&node_id)
        .and_then(|f| f.recognizer.as_ref());

    let regex_engine = decision.and_then(|d| d.regex_engine.clone());

    let (family, emit_hint) = if ir.delim_scan_configs.contains_key(&node_id) {
        (RecognizerFamily::DelimiterBalanced, EmitHint::DelimScan)
    } else if ir.key_dispatch_configs.contains_key(&node_id) {
        (RecognizerFamily::KeywordPrefix, EmitHint::KeyDispatch)
    } else if let Some(mode) = decision.and_then(|d| d.alt_mode.as_ref()) {
        match mode {
            AltMode::ByteDispatch => (RecognizerFamily::TokenLedBranches, EmitHint::ByteDispatch),
            AltMode::KeyDispatch => (RecognizerFamily::KeywordPrefix, EmitHint::KeyDispatch),
            AltMode::TokenDispatch => (RecognizerFamily::TokenLedBranches, EmitHint::ByteDispatch),
            AltMode::Checkpoint | AltMode::SharedHelper(_) => {
                (RecognizerFamily::Generic, EmitHint::Generic)
            }
        }
    } else if let Some(mode) = decision.and_then(|d| d.wrap_mode.as_ref()) {
        match mode {
            WrapMode::DelimScan | WrapMode::BalancedScan => {
                (RecognizerFamily::DelimiterBalanced, EmitHint::DelimScan)
            }
            WrapMode::SepBy => (RecognizerFamily::SeparatorList, EmitHint::Generic),
            WrapMode::Generic | WrapMode::SharedHelper(_) => {
                (RecognizerFamily::Generic, EmitHint::Generic)
            }
        }
    } else if recognizer.is_some() {
        let shape_family = match &recognizer.unwrap().shape {
            RecognizerShape::DelimiterBalanced { .. } => RecognizerFamily::DelimiterBalanced,
            RecognizerShape::SeparatorList { .. } => RecognizerFamily::SeparatorList,
            RecognizerShape::TokenLedBranches { .. } => RecognizerFamily::TokenLedBranches,
            RecognizerShape::KeywordPrefix { .. } => RecognizerFamily::KeywordPrefix,
            RecognizerShape::Regex { .. } => RecognizerFamily::Regex,
            _ => RecognizerFamily::Generic,
        };
        let emit_hint = if regex_engine.is_some() {
            EmitHint::KernelScanner
        } else {
            EmitHint::Generic
        };
        (shape_family, emit_hint)
    } else if regex_engine.is_some() {
        (RecognizerFamily::Regex, EmitHint::KernelScanner)
    } else {
        (RecognizerFamily::Generic, EmitHint::Generic)
    };

    let peer_group = recognizer.and_then(|r| r.peer_group);

    ScannerPlanRecord {
        family,
        regex_engine,
        emit_hint,
        peer_group,
    }
}
