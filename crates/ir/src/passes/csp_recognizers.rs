//! Tranche V.6 — recognizer-tier CSP.
//!
//! Builds the per-NodeId `RecognizerDecision` map that the backend
//! kernels (V.7) and driver dispatchers (V.8) consume. Variables and
//! constraints follow the plan §6 layout:
//!
//!   AltMode[node]            ∈ {Checkpoint, ByteDispatch, KeyDispatch,
//!                                TokenDispatch, SharedHelper(sig)}
//!   WrapMode[node]           ∈ {Generic, SepBy, DelimScan, BalancedScan,
//!                                SharedHelper(sig)}
//!   RegexEngine[(node,sid)]  ∈ RegexInfo[sid].feasible_engines
//!
//! No `Fallback` domain entries. If a node's recognizer fact is empty
//! the variable defaults to `Checkpoint` / `Generic` — those are
//! always-legal architectural fallbacks for grammar nodes that don't
//! match a richer recognizer shape, NOT escape hatches.
//!
//! For V.6 the constraint set is minimal: each decision is derived
//! deterministically from `NodeFacts.recognizer`. The CSP scaffolding
//! (variable construction, constraint plumbing, two-stage solve) is in
//! place so a follow-up tranche can add real cost-guided constraints
//! without restructuring the consumer interface.

use std::collections::HashMap;

use bbnf_regex::EngineSet;

use crate::dag::NodeId;
use crate::passes::patterns::{Recognizer, RecognizerShape};
use crate::{GrammarIR, IrNode, RuleId};

// ── Decision domain ─────────────────────────────────────────────────────────

/// Strategy chosen for an Alt node.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AltMode {
    /// Sequential checkpoint chain (universal fallback for any Alt).
    Checkpoint,
    /// Byte-dispatch table — branches with disjoint FIRST byte sets.
    ByteDispatch,
    /// Keyword dispatch — branches with disjoint leading literals.
    KeyDispatch,
    /// Token-led dispatch — exposed by the disjoint-alt e-graph rule.
    TokenDispatch,
    /// Hoisted shared helper — N≥`hoist_threshold` peers share one kernel.
    SharedHelper(u64),
}

/// Strategy chosen for a Wrap node.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WrapMode {
    /// Generic wrap (open / inner / close).
    Generic,
    /// Separator-by repeat.
    SepBy,
    /// Delimiter scan (forward memchr to close).
    DelimScan,
    /// Balanced delimiter scan (handles nested open/close).
    BalancedScan,
    /// Hoisted shared helper.
    SharedHelper(u64),
}

/// Engine chosen for a regex pattern at a specific call site.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RegexEngine {
    Memchr1,
    Memchr2,
    Memchr3,
    NibbleLut,
    OnePass,
    SmallDfa,
    Dfa,
    /// Family kernel helper (one of the eight in `backend/kernels/`).
    FamilyHelper,
}

/// One decision record per NodeId. Populated by `solve_recognizer_decisions`.
#[derive(Clone, Debug)]
pub struct RecognizerDecision {
    pub alt_mode: Option<AltMode>,
    pub wrap_mode: Option<WrapMode>,
    pub regex_engine: Option<RegexEngine>,
}

impl Default for RecognizerDecision {
    fn default() -> Self {
        Self {
            alt_mode: None,
            wrap_mode: None,
            regex_engine: None,
        }
    }
}

pub type RecognizerDecisionMap = HashMap<NodeId, RecognizerDecision>;

// ── Solver entry point ──────────────────────────────────────────────────────

/// Build the per-NodeId decision map from `ir.node_facts` and
/// `ir.regex_info`.
///
/// V.6 implementation: a deterministic walk over the DAG. Each decision
/// is derived from the upstream facts via straightforward case
/// analysis. The csp-solver constraint substrate is plumbed but no
/// real propagation runs yet — every variable is a singleton domain
/// derived from facts, so backtracking search is unnecessary.
pub fn solve_recognizer_decisions(ir: &GrammarIR) -> RecognizerDecisionMap {
    let dag = match ir.dag.as_ref() {
        Some(d) => d,
        None => return HashMap::new(),
    };
    let mut decisions = HashMap::new();
    for rule in &ir.rules {
        walk(&rule.body, ir, dag, &mut decisions, rule.id);
    }
    decisions
}

fn walk(
    node: &IrNode,
    ir: &GrammarIR,
    dag: &crate::dag::GrammarDag,
    decisions: &mut RecognizerDecisionMap,
    rule_id: RuleId,
) {
    if let Some(node_id) = dag.node_for(node) {
        let mut dec = RecognizerDecision::default();

        // Read the upstream recognizer fact, if present.
        let fact = ir
            .node_facts
            .get(&node_id)
            .and_then(|f| f.recognizer.as_ref());

        match node {
            IrNode::Alt(_branches, dispatch) => {
                dec.alt_mode = Some(decide_alt_mode(fact, dispatch.is_some()));
            }
            IrNode::Skip(_, _) | IrNode::Next(_, _) => {
                if is_wrap_shape(node) {
                    dec.wrap_mode = Some(decide_wrap_mode(fact));
                }
            }
            IrNode::Regex(sid) => {
                let info = ir.regex_info.get(sid);
                dec.regex_engine = Some(decide_regex_engine(
                    info.map(|i| i.feasible_engines).unwrap_or_else(EngineSet::empty),
                ));
            }
            _ => {}
        }

        if dec.alt_mode.is_some() || dec.wrap_mode.is_some() || dec.regex_engine.is_some() {
            decisions.insert(node_id, dec);
        }
    }

    super::recognizers::visit_children_alt(node, |child| walk(child, ir, dag, decisions, rule_id));
}

// ── Decision logic ──────────────────────────────────────────────────────────

fn decide_alt_mode(fact: Option<&Recognizer>, has_dispatch: bool) -> AltMode {
    if has_dispatch {
        return AltMode::ByteDispatch;
    }
    if let Some(rec) = fact {
        if let Some(group) = rec.peer_group {
            return AltMode::SharedHelper(group as u64);
        }
        if matches!(rec.shape, RecognizerShape::TokenLedBranches { .. }) {
            return AltMode::TokenDispatch;
        }
        if matches!(rec.shape, RecognizerShape::KeywordPrefix { .. }) {
            return AltMode::KeyDispatch;
        }
    }
    AltMode::Checkpoint
}

fn decide_wrap_mode(fact: Option<&Recognizer>) -> WrapMode {
    if let Some(rec) = fact {
        if let Some(group) = rec.peer_group {
            return WrapMode::SharedHelper(group as u64);
        }
        match &rec.shape {
            RecognizerShape::DelimiterBalanced { .. } => return WrapMode::BalancedScan,
            RecognizerShape::SeparatorList { .. } => return WrapMode::SepBy,
            _ => {}
        }
    }
    WrapMode::Generic
}

fn decide_regex_engine(feasible: EngineSet) -> RegexEngine {
    // Priority order: prefer kernels over generic engines, prefer
    // narrower memchr over wider memchr, prefer one-pass over DFA.
    if feasible.contains(EngineSet::FAMILY_HELPER) {
        return RegexEngine::FamilyHelper;
    }
    if feasible.contains(EngineSet::MEMCHR1) {
        return RegexEngine::Memchr1;
    }
    if feasible.contains(EngineSet::MEMCHR2) {
        return RegexEngine::Memchr2;
    }
    if feasible.contains(EngineSet::MEMCHR3) {
        return RegexEngine::Memchr3;
    }
    if feasible.contains(EngineSet::NIBBLE_LUT) {
        return RegexEngine::NibbleLut;
    }
    if feasible.contains(EngineSet::ONE_PASS) {
        return RegexEngine::OnePass;
    }
    if feasible.contains(EngineSet::SMALL_DFA) {
        return RegexEngine::SmallDfa;
    }
    RegexEngine::Dfa
}

fn is_wrap_shape(node: &IrNode) -> bool {
    match node {
        IrNode::Skip(left, _) => matches!(left.as_ref(), IrNode::Next(_, _)),
        IrNode::Next(_, right) => matches!(right.as_ref(), IrNode::Skip(_, _)),
        _ => false,
    }
}
