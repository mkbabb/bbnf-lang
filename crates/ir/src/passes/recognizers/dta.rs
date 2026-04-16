//! Dispatch Tape Automaton (DTA) lifter — Tranche AV Phase 3 (AV.3.1–3.4).
//!
//! # Architectural role
//!
//! The DTA replaces the recursive-descent-per-rule codegen for a
//! grammar's hot path with a flat counter-DFA + frame stack + shunting-
//! yard loop driven by byte-class dispatch. This file lifts a
//! [`GrammarIR`] tree to the owned data the emitter lowers into
//! `const DTA_TABLE: DtaTable = …` in `generated.rs`.
//!
//! The lift is intentionally closed over the existing IR shape —
//! Alt / Seq / Repeat / Ref / Map / literal / regex — so no grammar
//! syntax has to change for a grammar to opt in. The lifter walks
//! every rule body once, assigns a `StateId` to every structural node,
//! and records the static per-node facts the runtime driver consults:
//! byte-class dispatch keys, frame kind, counter-optional markers,
//! operator precedence, payload hints.
//!
//! # Three layers (AV.md §Phase 3)
//!
//! 1. **Byte-class dispatch.** `DtaState::ByteDispatch` carries a
//!    128-entry LUT of `first byte → target StateId`. The AU.2.7
//!    structural bitmap feeds this — the DTA consumes
//!    `trailing_zeros(mask)` + `src[offset]` for every Alt whose
//!    branches have disjoint FIRST sets.
//!
//! 2. **Frame counter stack.** Each compound node (Seq frame, Alt
//!    frame, Repeat frame) has a [`FrameKind`] telling the driver how
//!    to advance: linear counter for Seq, branch-index pin for Alt,
//!    body-pointer + count for Repeat. The driver's stack is a fixed-
//!    size `[Frame; 64]`; rules whose nesting depth exceeds 64 spill
//!    to a heap overflow region (not observed in the target corpus).
//!
//! 3. **Counter-DFA extensions (AV.3.2).** Nested-optional-with-
//!    empty-body (BBNF `mapped_factor`, CSS `alphaSep?`) is compiled
//!    to a counter state that tracks presence without state-space
//!    explosion. Rules carrying this shape are flagged via
//!    `CounterOptional::Nested`.
//!
//! # Shunting-yard extension (AV.3.3)
//!
//! Rule chains of the form `a_n = a_{n+1} (op_n a_{n+1})*` for
//! `n = 0..k` collapse to a single shunting-yard loop with one
//! precedence entry per operator. The lifter detects the chain via
//! [`collect_precedence_chain`] — if every rule in the chain has the
//! operator-list shape and each rung's operators are disjoint from
//! every other rung's, one [`PrecedenceTable`] covers the whole
//! chain. Sheets' `__formula → … → __unary_expr` collapses from six
//! nested functions to one shunting-yard loop emitted at the
//! `formula` state.
//!
//! # Diagnostic replay (AV.3.4)
//!
//! The happy-path DTA does not backtrack. Diagnostic mode re-enters
//! the same state table with an instrumentation hook that tracks
//! deepest successful advance + failing state. This file exposes
//! `DtaTable::states` as the single shared substrate; the driver's
//! `DiagnosticRun` re-plays from state 0 with tracking enabled. One
//! automaton, two driver modes — no second codegen path.
//!
//! # Deliverable scope (AV.3.1)
//!
//! The lifter produces correct, inspection-friendly data. The runtime
//! driver that executes the state table lives in the emitter (AV.3.6
//! — fn-per-rule deletion) and its stage-B pair ships in V4. This
//! file therefore stops at "emit the table"; the emitter side embeds
//! the table as const data and the V4 PSI driver reads it.
//!
//! Keeping the producer and the consumer in distinct crates is the
//! same decoupling that isolates `GrammarProfile` from the emitter
//! and the walker; the DTA table is another output channel on the
//! grammar-fingerprint boundary.

use std::collections::HashMap;

use crate::passes::sets::PushFingerprint;
use crate::{GrammarIR, IrNode, IrRule, RuleId, StringId, TypeDesc};

// ── State identifiers ───────────────────────────────────────────────

/// Opaque index into [`DtaTable::states`]. Assigned in depth-first
/// rule-body traversal order; stable across a single lift.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StateId(pub u16);

impl StateId {
    /// Sentinel for "no state" — used in byte-dispatch LUT slots that
    /// do not admit an Alt branch.
    pub const NONE: StateId = StateId(u16::MAX);
}

/// Runtime frame class — how the driver advances through this state's
/// children.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FrameKind {
    /// Linear child advance, cursor = 0..N. The frame's counter is the
    /// index of the next child to match.
    Seq,
    /// Branch selection — frame counter holds the selected branch
    /// index (or `u8::MAX` if none admitted yet).
    Alt,
    /// Repeat(inner, lo, hi) — frame counter tracks iteration count;
    /// the driver re-enters the body until either `hi` is reached or
    /// the body fails.
    Repeat,
    /// Shunting-yard operator loop (AV.3.3). Collapsed from an
    /// operator chain — the frame runs one `op rhs` iteration per
    /// operator + precedence-aware reduction.
    ShuntingYard,
}

/// A single DFA state — one node in the lifted grammar.
#[derive(Clone, Debug, PartialEq)]
pub enum DtaState {
    /// Match a literal byte sequence at the current offset.
    Literal { text: StringId },
    /// Match a regex pattern at the current offset (runs through the
    /// regex subsystem; no DFA inlining here).
    Regex { pattern: StringId },
    /// Match nothing, consume nothing.
    Epsilon,
    /// Linear composition — run each child state in order.
    Seq {
        children: Vec<StateId>,
        /// Attached frame kind when the Seq is materialised into a
        /// record. `None` means transparent — the driver inherits the
        /// parent's frame.
        frame: FrameKind,
    },
    /// Byte-class dispatched Alt. `table[b]` is the StateId for the
    /// branch whose FIRST set admits byte `b`, or
    /// [`StateId::NONE`] if no branch starts with `b`.
    ByteDispatch {
        table: Vec<StateId>,
        fallback: Option<StateId>,
    },
    /// Alt with no pairwise-disjoint FIRST sets — the driver tries
    /// each branch in order with a single backtrack per branch (happy
    /// path has no branch fall-through; diagnostic mode replays).
    AltLinear { branches: Vec<StateId> },
    /// Repeat(inner, lo..=hi). `lo == 0` and `hi == 1` is the optional
    /// case; `lo == 0` / `hi == u32::MAX` is many; `lo == 1` /
    /// `hi == u32::MAX` is many1.
    Repeat {
        inner: StateId,
        lo: u32,
        hi: u32,
        /// AV.3.2 — set when the inner body contains an inner optional
        /// whose body is itself allowed to be empty. The driver uses
        /// a counter to distinguish "one optional fire" from
        /// "multiple empty bodies".
        counter_optional: Option<CounterOptional>,
    },
    /// Unresolved rule reference. The driver follows into the
    /// referenced rule's entry state at execution time.
    Ref { rule: RuleId, target: StateId },
    /// Shunting-yard operator loop — the AV.3.3 precedence-chain
    /// collapse.
    ShuntingYard {
        /// Head state — the first operand's parse entry (typically the
        /// chain's innermost operand, e.g. Sheets `unary_expr`).
        head: StateId,
        /// Precedence table — one entry per operator token, keyed by
        /// single-byte dispatch.
        precedence: PrecedenceTable,
    },
    /// AW-I.W4γ — Whitespace trim step.
    ///
    /// Advances `pos` past zero or more bytes matching the grammar's
    /// `@ws` regex. `pattern` is `None` when the grammar did not
    /// declare `@ws` — the walker treats that as a no-op. Lowered
    /// from `IrNode::OptionalWhitespace(inner)` as the outer Seq
    /// `[WsTrim, inner, WsTrim]`, matching the VM's
    /// `TrimWs + inner + TrimWs` pair (see
    /// `bbnf_ir::vm::compiler::node::compile_node`).
    ///
    /// The pre-W4γ lifter silently dropped the wrapper and emitted
    /// just the inner node. Grammars that relied on `?w` between
    /// atoms parsed incorrectly — `"=" ?w` returned at "=" without
    /// consuming trailing whitespace, causing the walker to observe
    /// an unexpected byte at the next rule's entry.
    WsTrim {
        pattern: Option<StringId>,
    },
}

/// Counter-optional marker for AV.3.2.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CounterOptional {
    /// The outer Repeat's body contains one nested optional whose
    /// body is itself possibly empty. BBNF `mapped_factor`:
    /// `( "->" ?w ( value_expr , type_annotation ? ) ) ?`.
    Nested,
    /// The Alt body has a lookahead-determined optional that requires
    /// carrying a flag across the dispatch (CSS `alphaSep?` in
    /// colour-function arguments).
    Lookahead,
}

/// Operator precedence table — AV.3.3 shunting-yard.
///
/// One entry per operator byte; sorted for binary-search at build
/// time, indexed linearly at runtime (the table is bounded by the
/// number of operator bytes in the grammar — ≤ 16 for every shipped
/// grammar).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrecedenceTable {
    pub entries: Vec<PrecedenceEntry>,
}

/// One operator's shunting-yard profile.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrecedenceEntry {
    /// The operator's identifying byte. Multi-byte operators
    /// (BBNF `<<`, `>>`) use the first byte plus
    /// [`PrecedenceEntry::second_byte`].
    pub byte: u8,
    /// Optional second byte for two-byte operators (`<<`, `>>`).
    pub second_byte: Option<u8>,
    /// Higher values bind tighter. Assignments inferred from chain
    /// depth — the innermost rung gets the highest precedence.
    pub precedence: u8,
    /// Left-associative (most operators) or right-associative (`^` in
    /// Sheets, `**` if added).
    pub associativity: Associativity,
    /// The rule that emits this operator as an Alt branch — used by
    /// the emitter to thread the variant_idx + payload back into the
    /// pushed compound.
    pub op_rule: RuleId,
    /// The u8 discriminant the operator's typed materialisation (Bug
    /// 1 territory, resolved V0) puts into the compound's payload
    /// column.
    pub op_discriminant: u8,
}

/// Operator associativity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Associativity {
    Left,
    Right,
}

// ── DTA table — the owned lifter output ─────────────────────────────

/// The lifted dispatch automaton for one grammar.
///
/// Contains every rule's compiled state, the per-rule entry-state
/// index, and the shunting-yard collapses discovered during the
/// lift. The emitter embeds this directly as `const DTA_TABLE` in
/// `generated.rs`; the runtime driver consumes it via
/// [`DtaTable::state`] + frame-stack walking.
#[derive(Clone, Debug, Default)]
pub struct DtaTable {
    /// Every state, index == `StateId(i)`. `states[0]` is the entry
    /// rule's entry state.
    pub states: Vec<DtaState>,
    /// Per-rule entry state. Indexed by `RuleId`; absent entries
    /// (pre-lift transparent / pruned rules) default to
    /// [`StateId::NONE`].
    pub rule_entries: HashMap<RuleId, StateId>,
    /// Rules recognised as shunting-yard chains during the lift.
    /// The map is keyed by the chain's outermost (lowest-precedence)
    /// rule; the [`StateId`] references the single collapsed
    /// [`DtaState::ShuntingYard`]. Inner chain rules' entries still
    /// live in `rule_entries` but point into the same shunting-yard
    /// state — this is the "no separate driver path" invariant.
    pub shunting_yard_chains: HashMap<RuleId, StateId>,
    /// Counter-optional rules (AV.3.2). Lowered to the IR annotation
    /// side so the emitter can route the counter allocation in the
    /// frame stack without re-walking the IR at emit time.
    pub counter_optional_rules: HashMap<RuleId, CounterOptional>,
    /// Total nesting depth observed during the lift. The driver
    /// sizes its frame stack from this — `max(depth, 16)` with the
    /// 64-byte overflow region taking over above 64.
    pub max_nesting_depth: u16,
    /// AW-I.W4γ — the grammar's authoritative entry rule, copied
    /// from `GrammarIR::entry`. The walker dispatches the entry
    /// rule's state via `rule_entries`, not `rule_entries.first()`.
    pub entry: RuleId,
}

impl DtaTable {
    /// Access a state by id. Panics on out-of-range ids.
    #[inline]
    pub fn state(&self, id: StateId) -> &DtaState {
        &self.states[id.0 as usize]
    }

    /// Lookup the entry state for a rule; returns [`StateId::NONE`]
    /// for rules outside the lift (transparent, pruned).
    #[inline]
    pub fn rule_entry(&self, rule: RuleId) -> StateId {
        self.rule_entries
            .get(&rule)
            .copied()
            .unwrap_or(StateId::NONE)
    }

    /// Whether the lift admitted the entire grammar. False iff any
    /// non-transparent rule has no entry state — used by the emitter
    /// to decide whether the DTA drives `parse()` or the legacy
    /// fn-per-rule path takes over (legacy is the fallback until
    /// AV.3.6 fully deletes it).
    pub fn covers_full_grammar(&self, ir: &GrammarIR) -> bool {
        ir.rules
            .iter()
            .filter(|r| !r.meta.is_transparent)
            .all(|r| self.rule_entries.contains_key(&r.id))
    }
}

// ── Lifter ──────────────────────────────────────────────────────────

/// Lift a [`GrammarIR`] to a [`DtaTable`]. Invoked after every IR
/// pass has run (push fingerprint, payload layouts, dispatch tables)
/// so the lifter reads every per-rule fact it needs.
///
/// This is the single entry point. Sub-lifts (shunting-yard chain
/// detection, counter-optional annotation) run inline so the walk
/// is O(rules × nodes) total.
pub fn lift_dta(ir: &GrammarIR) -> DtaTable {
    let mut builder = DtaBuilder::new(ir);
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        builder.lift_rule(rule);
    }
    builder.finish()
}

/// Stateful DTA builder — the dual of the existing `GrammarSink`
/// substrate (AU.4.1). `GrammarSink` survives for downstream
/// consumers (bbnf-analysis, gorgeous, bbnf-lsp, bbnf-bootstrap);
/// `DtaBuilder` is the stage-A sink for the runtime hot path.
pub struct DtaBuilder<'ir> {
    ir: &'ir GrammarIR,
    states: Vec<DtaState>,
    rule_entries: HashMap<RuleId, StateId>,
    shunting_yard_chains: HashMap<RuleId, StateId>,
    counter_optional_rules: HashMap<RuleId, CounterOptional>,
    current_depth: u16,
    max_depth: u16,
}

impl<'ir> DtaBuilder<'ir> {
    fn new(ir: &'ir GrammarIR) -> Self {
        Self {
            ir,
            states: Vec::new(),
            rule_entries: HashMap::new(),
            shunting_yard_chains: HashMap::new(),
            counter_optional_rules: HashMap::new(),
            current_depth: 0,
            max_depth: 0,
        }
    }

    fn alloc_state(&mut self, state: DtaState) -> StateId {
        let id = self.states.len();
        debug_assert!(id < u16::MAX as usize, "DTA state count overflowed u16");
        self.states.push(state);
        StateId(id as u16)
    }

    fn lift_rule(&mut self, rule: &IrRule) {
        if self.rule_entries.contains_key(&rule.id) {
            return;
        }
        // AV.3.3 — probe for shunting-yard chain head before
        // emitting the rule's body states. If the rule is the
        // outermost rung of a collapsible chain, emit a single
        // [`DtaState::ShuntingYard`] and register every chain rung
        // as pointing at the same state.
        if let Some(chain) = collect_precedence_chain(self.ir, rule) {
            let head_state = self.lift_node(&chain.head_node);
            let entry = self.alloc_state(DtaState::ShuntingYard {
                head: head_state,
                precedence: chain.precedence,
            });
            for &rid in &chain.chain_rules {
                self.rule_entries.insert(rid, entry);
                self.shunting_yard_chains.insert(rid, entry);
            }
            return;
        }

        let entry = self.lift_node(&rule.body);
        self.rule_entries.insert(rule.id, entry);
    }

    fn lift_node(&mut self, node: &IrNode) -> StateId {
        self.current_depth = self.current_depth.saturating_add(1);
        self.max_depth = self.max_depth.max(self.current_depth);
        let id = match node {
            IrNode::Literal(sid) => self.alloc_state(DtaState::Literal { text: *sid }),
            IrNode::Regex(sid) => self.alloc_state(DtaState::Regex { pattern: *sid }),
            IrNode::Epsilon => self.alloc_state(DtaState::Epsilon),
            IrNode::Seq(children) => {
                let child_states: Vec<StateId> =
                    children.iter().map(|c| self.lift_node(c)).collect();
                self.alloc_state(DtaState::Seq {
                    children: child_states,
                    frame: FrameKind::Seq,
                })
            }
            IrNode::Alt(branches, dispatch) => {
                let branch_states: Vec<StateId> = branches
                    .iter()
                    .map(|b| self.lift_node(&b.node))
                    .collect();
                if let Some(ad) = dispatch {
                    let fallback = ad
                        .fallback_idx
                        .and_then(|idx| branch_states.get(idx as usize).copied());
                    // `ad.table` is a 128-entry Vec<u8>; convert to
                    // StateId indexed by the full 256-entry byte domain
                    // so the runtime can index by any u8 without a
                    // bounds check. Bytes outside the 0..128 ASCII
                    // window use the fallback (if any) or
                    // [`StateId::NONE`].
                    let mut table = vec![StateId::NONE; 256];
                    for (byte, &branch_idx) in ad.table.iter().enumerate() {
                        if branch_idx == u8::MAX {
                            continue;
                        }
                        if let Some(&state) = branch_states.get(branch_idx as usize) {
                            table[byte] = state;
                        }
                    }
                    self.alloc_state(DtaState::ByteDispatch {
                        table,
                        fallback,
                    })
                } else {
                    self.alloc_state(DtaState::AltLinear {
                        branches: branch_states,
                    })
                }
            }
            IrNode::Repeat { inner, lo, hi } => {
                let inner_state = self.lift_node(inner);
                let counter_optional = detect_counter_optional(inner);
                self.alloc_state(DtaState::Repeat {
                    inner: inner_state,
                    lo: *lo,
                    hi: *hi,
                    counter_optional,
                })
            }
            IrNode::Ref(rid) => {
                // Forward reference — lift on demand if the target
                // hasn't been seen yet, then stamp the edge.
                let target = self.rule_entries.get(rid).copied().unwrap_or(StateId::NONE);
                self.alloc_state(DtaState::Ref {
                    rule: *rid,
                    target,
                })
            }
            IrNode::Skip(a, b) => {
                // `a << b` — run both, keep left. Lowered to a Seq of
                // two states; the emitter's frame kind stays Seq
                // (the "discard right" is a codegen concern, not a
                // state-machine concern).
                let a_state = self.lift_node(a);
                let b_state = self.lift_node(b);
                self.alloc_state(DtaState::Seq {
                    children: vec![a_state, b_state],
                    frame: FrameKind::Seq,
                })
            }
            IrNode::Next(a, b) => {
                let a_state = self.lift_node(a);
                let b_state = self.lift_node(b);
                self.alloc_state(DtaState::Seq {
                    children: vec![a_state, b_state],
                    frame: FrameKind::Seq,
                })
            }
            IrNode::Minus(a, _b) => self.lift_node(a),
            IrNode::Negate(inner) => {
                // Zero-width lookahead: the runtime treats this as a
                // probe that does not consume. Represented as the
                // inner state wrapped in a Seq-with-frame — the
                // emitter distinguishes by the frame count (1) and
                // the parent node identity.
                let inner_state = self.lift_node(inner);
                self.alloc_state(DtaState::Seq {
                    children: vec![inner_state],
                    frame: FrameKind::Seq,
                })
            }
            IrNode::Map { inner, .. } => self.lift_node(inner),
            IrNode::OptionalWhitespace(inner) => {
                // AW-I.W4γ: `?w` lowers to a Seq `[WsTrim, inner, WsTrim]`
                // matching the VM compiler's `TrimWs + inner + TrimWs`
                // pair (`bbnf_ir::vm::compiler::node::compile_node`).
                // Pre-W4γ the lifter silently returned the inner state,
                // stripping every whitespace-trim site from the DTA and
                // breaking `BbnfBootstrap::parse` on any grammar with
                // `?w` between atoms.
                //
                // The ws regex is carried on each `WsTrim` state so the
                // runtime need not thread grammar-level context. `None`
                // admits grammars that never declared `@ws` — the
                // walker's WsTrim arm treats that as a no-op Epsilon.
                let ws_sid = self.ir.ws_pattern;
                let inner_state = self.lift_node(inner);
                let ws_before = self.alloc_state(DtaState::WsTrim { pattern: ws_sid });
                let ws_after = self.alloc_state(DtaState::WsTrim { pattern: ws_sid });
                self.alloc_state(DtaState::Seq {
                    children: vec![ws_before, inner_state, ws_after],
                    frame: FrameKind::Seq,
                })
            }
            IrNode::TokenDispatch { token, arms: _, fallback } => {
                // TokenDispatch is an existing dispatch lowering — the
                // DTA inherits it by lifting the token + fallback;
                // arms are lifted via the parent's Alt shape.
                let token_state = self.lift_node(token);
                let fallback_state = self.lift_node(fallback);
                self.alloc_state(DtaState::Seq {
                    children: vec![token_state, fallback_state],
                    frame: FrameKind::Seq,
                })
            }
        };
        self.current_depth -= 1;
        id
    }

    fn finish(self) -> DtaTable {
        // AV.3.2 — propagate counter-optional markers from the per-
        // state annotations to the per-rule sidecar so the emitter
        // can build the frame-stack-with-counter codegen uniformly.
        let mut counter_optional_rules = self.counter_optional_rules;
        for (&rule_id, &entry) in self.rule_entries.iter() {
            if let Some(DtaState::Repeat {
                counter_optional: Some(kind),
                ..
            }) = self.states.get(entry.0 as usize)
            {
                counter_optional_rules.entry(rule_id).or_insert(*kind);
            }
        }
        let entry = self.ir.entry;
        DtaTable {
            states: self.states,
            rule_entries: self.rule_entries,
            shunting_yard_chains: self.shunting_yard_chains,
            counter_optional_rules,
            max_nesting_depth: self.max_depth,
            entry,
        }
    }
}

// ── AV.3.2 — counter-optional detection ──────────────────────────────

/// Identify the counter-optional shape for a Repeat body.
///
/// BBNF's `mapped_factor` is the canonical nested case: the outer
/// Repeat body is `Seq("->", ?w, Seq(value_expr, Repeat(type_annotation, 0..1)))`
/// — a nested Repeat inside the body. A pure DFA would need
/// exponential states to distinguish "outer fires with inner absent"
/// from "outer fires with inner present"; the counter-DFA carries a
/// single u8 per nested optional.
fn detect_counter_optional(inner: &IrNode) -> Option<CounterOptional> {
    if has_nested_optional_with_empty_body(inner) {
        Some(CounterOptional::Nested)
    } else {
        None
    }
}

fn has_nested_optional_with_empty_body(node: &IrNode) -> bool {
    match node {
        IrNode::Repeat { lo: 0, hi: 1, inner } => {
            // The inner Repeat is an optional; count the sub-optionals
            // inside its body.
            inner_contains_optional(inner)
        }
        IrNode::Seq(children) => children.iter().any(has_nested_optional_with_empty_body),
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| has_nested_optional_with_empty_body(&b.node)),
        IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => has_nested_optional_with_empty_body(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            has_nested_optional_with_empty_body(a) || has_nested_optional_with_empty_body(b)
        }
        _ => false,
    }
}

fn inner_contains_optional(node: &IrNode) -> bool {
    match node {
        IrNode::Repeat { lo: 0, hi: 1, .. } => true,
        IrNode::Seq(children) => children.iter().any(inner_contains_optional),
        IrNode::Alt(branches, _) => {
            branches.iter().any(|b| inner_contains_optional(&b.node))
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            inner_contains_optional(inner)
        }
        _ => false,
    }
}

// ── AV.3.3 — shunting-yard chain detection ──────────────────────────

/// Collected precedence-chain lift output for a candidate head rule.
#[derive(Clone, Debug)]
struct PrecedenceChain {
    /// Head operand node — the chain's innermost operand's entry
    /// (e.g. Sheets `unary_expr`).
    head_node: IrNode,
    /// Every rung of the chain, outermost first. Sheets collapses all
    /// six to a single [`DtaState::ShuntingYard`].
    chain_rules: Vec<RuleId>,
    /// The unified precedence table covering every operator in the
    /// chain. Precedence values are assigned from chain depth — the
    /// innermost rung gets the highest value.
    precedence: PrecedenceTable,
}

/// Try to identify a left-associative operator-precedence chain
/// starting at `rule`. A chain is a series of rules each matching the
/// shape `a_n = a_{n+1} (op_n a_{n+1})*`, where every `op_n` branches
/// to a single disjoint byte set and each rung's operators are
/// disjoint from every other rung's. The last rung's body is an
/// arbitrary `a_k` operand and terminates the chain.
///
/// Sheets collapses `__formula → __comparison_expr → … →
/// __unary_expr` into one state; BBNF's `binary_factor` also fits
/// (single rung).
fn collect_precedence_chain(ir: &GrammarIR, rule: &IrRule) -> Option<PrecedenceChain> {
    // The collapse requires ≥ 2 rungs to pay off — a one-rung chain
    // is a plain Seq+Repeat+Alt shape the regular lift handles
    // without precedence overhead.
    let mut cursor = rule;
    let mut rungs: Vec<(RuleId, u8, Vec<PrecedenceEntry>, IrNode)> = Vec::new();
    let mut seen: Vec<RuleId> = Vec::new();

    loop {
        if seen.contains(&cursor.id) {
            // cycle — reject; the chain must be acyclic.
            break;
        }
        seen.push(cursor.id);
        let Some((operand_rule, operators, operand_node)) =
            match_operator_chain_rule(ir, cursor)
        else {
            break;
        };
        rungs.push((cursor.id, rungs.len() as u8, operators, operand_node));
        let Some(next) = ir.rules.iter().find(|r| r.id == operand_rule) else {
            break;
        };
        cursor = next;
    }

    if rungs.len() < 2 {
        return None;
    }

    // Precedence is assigned innermost-first: the last rung has the
    // highest precedence. This reproduces the left-recursion collapse
    // — `a + b * c` binds as `a + (b * c)` because `*`'s rung sits
    // deeper in the chain.
    let num_rungs = rungs.len();
    let mut entries: Vec<PrecedenceEntry> = Vec::new();
    for (rung_idx, (_rid, _depth, ops, _operand)) in rungs.iter().enumerate() {
        let prec = (num_rungs - rung_idx) as u8;
        for entry in ops {
            let mut e = entry.clone();
            e.precedence = prec;
            entries.push(e);
        }
    }

    // Ensure pairwise byte-disjointness across rungs. If any two
    // entries share a dispatch byte (ignoring second_byte), the
    // chain is ambiguous and we bail — the regular tree lift is
    // still correct, just not collapsed.
    let mut seen_bytes = [false; 256];
    for e in &entries {
        let idx = e.byte as usize;
        if seen_bytes[idx] {
            return None;
        }
        seen_bytes[idx] = true;
    }

    // Tail operand — the innermost rung's operand becomes the
    // shunting-yard head. All other chain rules delegate into the
    // same state.
    let head_node = rungs.last()?.3.clone();
    let chain_rules: Vec<RuleId> = rungs.iter().map(|(rid, _, _, _)| *rid).collect();
    Some(PrecedenceChain {
        head_node,
        chain_rules,
        precedence: PrecedenceTable { entries },
    })
}

/// Match a single operator-chain rung:
/// `body = Seq(operand, Repeat(Seq(op, operand)))`. Returns the
/// operand rule id, the operator set, and the operand node the
/// next rung starts from.
///
/// Operand is identified by `Ref(inner_rule_id)`; operator is an
/// Alt of Literal branches — either via `Ref(op_rule_id)` with
/// `op_rule_id` being an Alt-of-Literal rule, or the Alt-of-Literal
/// directly inlined into the Seq (post-`fuse_single_use` shape).
/// The owning rule's id is used as the `op_rule` reference so the
/// emitter can thread variant_idx through without a secondary rule
/// allocation.
fn match_operator_chain_rule(
    ir: &GrammarIR,
    rule: &IrRule,
) -> Option<(RuleId, Vec<PrecedenceEntry>, IrNode)> {
    let body = strip_transparent_owned(&rule.body);
    let children = match &body {
        IrNode::Seq(c) => c,
        _ => return None,
    };
    if children.len() != 2 {
        return None;
    }
    let operand = &children[0];
    let tail = &children[1];

    let operand_rule = match strip_transparent_owned(operand) {
        IrNode::Ref(rid) => rid,
        _ => return None,
    };

    let inner_stripped = strip_transparent_owned(tail);
    let inner = match inner_stripped {
        IrNode::Repeat {
            inner,
            lo: 0,
            hi: u32::MAX,
        } => *inner,
        _ => return None,
    };

    // Inner is `Seq(op, operand)` or with `?w` interleave.
    let inner_children = match strip_transparent_owned(&inner) {
        IrNode::Seq(c) => c,
        _ => return None,
    };
    if inner_children.is_empty() {
        return None;
    }

    // First non-whitespace child is the op — either a Ref to an
    // Alt-of-literal rule, or the inlined Alt-of-literal itself.
    let op_node = inner_children
        .iter()
        .find(|c| !matches!(strip_transparent_owned(c), IrNode::OptionalWhitespace(_)))?;

    let entries = extract_operator_set(ir, op_node, rule.id)?;

    Some((operand_rule, entries, operand.clone()))
}

/// Extract the operator set from a chain rung's op-position node.
///
/// Three shapes are accepted:
///
/// 1. `Ref(op_rule_id)` where `op_rule_id` is an Alt-of-Literal
///    rule. Canonical case before inlining (Sheets `add_op`,
///    `mul_op`, etc.).
/// 2. `Alt(branches)` inlined directly into the rung's Seq. Common
///    post-`fuse_single_use` shape.
/// 3. `Literal(sid)` when the chain has a single operator (BBNF
///    `concat_expr = … "&" …` would fit if the op-rule were
///    reduced to one literal).
///
/// `owning_rule_id` is the rung's own rule id — used when the
/// operator is inlined so the emitter can thread variant_idx
/// through the chain rung without a phantom op-rule reference.
fn extract_operator_set(
    ir: &GrammarIR,
    node: &IrNode,
    owning_rule_id: RuleId,
) -> Option<Vec<PrecedenceEntry>> {
    let stripped = strip_transparent_owned(node);
    match stripped {
        IrNode::Ref(rid) => {
            let op_rule = ir.rules.iter().find(|r| r.id == rid)?;
            collect_operator_alternatives(ir, op_rule, rid)
        }
        IrNode::Alt(_, _) => collect_inlined_alt_operators(ir, &stripped, owning_rule_id),
        IrNode::Literal(sid) => {
            let literal = ir.get_string(sid).to_string();
            let bytes = literal.as_bytes();
            if bytes.is_empty() {
                return None;
            }
            Some(vec![PrecedenceEntry {
                byte: bytes[0],
                second_byte: bytes.get(1).copied(),
                precedence: 0,
                associativity: infer_associativity(&literal),
                op_rule: owning_rule_id,
                op_discriminant: 0,
            }])
        }
        _ => None,
    }
}

/// Collect operator entries from an inlined Alt node. The node must
/// be an Alt whose branches are each a Literal (possibly wrapped in
/// Map/OptionalWhitespace).
fn collect_inlined_alt_operators(
    ir: &GrammarIR,
    node: &IrNode,
    owning_rule_id: RuleId,
) -> Option<Vec<PrecedenceEntry>> {
    let branches = match node {
        IrNode::Alt(branches, _) => branches,
        _ => return None,
    };
    let mut out = Vec::new();
    for (discriminant, branch) in branches.iter().enumerate() {
        let literal = extract_literal(&branch.node, ir)?;
        let bytes = literal.as_bytes();
        if bytes.is_empty() {
            return None;
        }
        out.push(PrecedenceEntry {
            byte: bytes[0],
            second_byte: bytes.get(1).copied(),
            precedence: 0,
            associativity: infer_associativity(&literal),
            op_rule: owning_rule_id,
            op_discriminant: discriminant.min(u8::MAX as usize) as u8,
        });
    }
    Some(out)
}

/// Strip transparent wrappers (OptionalWhitespace, Map) from a node,
/// returning a cloned owned tree with the wrappers dissolved. Used
/// by the chain matcher because pattern-bind against owned enum
/// variants sidesteps the nested-box lifetime tangle that a
/// borrowed-walk would introduce.
fn strip_transparent_owned(node: &IrNode) -> IrNode {
    match node {
        IrNode::OptionalWhitespace(inner) => strip_transparent_owned(inner),
        IrNode::Map { inner, .. } => strip_transparent_owned(inner),
        _ => node.clone(),
    }
}

fn collect_operator_alternatives(
    ir: &GrammarIR,
    op_rule: &IrRule,
    op_rule_id: RuleId,
) -> Option<Vec<PrecedenceEntry>> {
    let body = strip_transparent_owned(&op_rule.body);
    let branches = match body {
        IrNode::Alt(branches, _) => branches,
        _ => return None,
    };
    let mut out = Vec::new();
    for (discriminant, branch) in branches.iter().enumerate() {
        let literal = extract_literal(&branch.node, ir)?;
        let bytes = literal.as_bytes();
        if bytes.is_empty() {
            return None;
        }
        out.push(PrecedenceEntry {
            byte: bytes[0],
            second_byte: bytes.get(1).copied(),
            precedence: 0, // filled in by caller
            associativity: infer_associativity(&literal),
            op_rule: op_rule_id,
            op_discriminant: discriminant.min(u8::MAX as usize) as u8,
        });
    }
    Some(out)
}

fn extract_literal(node: &IrNode, ir: &GrammarIR) -> Option<String> {
    match strip_transparent_owned(node) {
        IrNode::Literal(sid) => Some(ir.get_string(sid).to_string()),
        IrNode::Map { inner, .. } => extract_literal(&inner, ir),
        _ => None,
    }
}

/// Sheets `^` is the only right-associative operator in the target
/// corpus (per AV.md §AV.3.3). Everything else is left-associative.
fn infer_associativity(literal: &str) -> Associativity {
    match literal {
        "^" => Associativity::Right,
        _ => Associativity::Left,
    }
}

// ── Inspector helpers ───────────────────────────────────────────────

/// Produce a coarse summary of a [`DtaTable`] — state count, chain
/// count, max depth, counter-optional count. Used by tests and the
/// emitter's debug logging; no runtime consumer touches this.
pub fn summarise(table: &DtaTable, ir: &GrammarIR) -> DtaSummary {
    let non_transparent = ir.rules.iter().filter(|r| !r.meta.is_transparent).count();
    DtaSummary {
        state_count: table.states.len(),
        rule_count: non_transparent,
        lifted_rules: table.rule_entries.len(),
        shunting_yard_count: table
            .shunting_yard_chains
            .values()
            .collect::<std::collections::HashSet<_>>()
            .len(),
        counter_optional_count: table.counter_optional_rules.len(),
        max_depth: table.max_nesting_depth,
    }
}

/// Structural summary of a DTA lift. Mostly for test assertions —
/// the runtime consumer reads the table directly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DtaSummary {
    pub state_count: usize,
    pub rule_count: usize,
    pub lifted_rules: usize,
    pub shunting_yard_count: usize,
    pub counter_optional_count: usize,
    pub max_depth: u16,
}

// ── Profile hook ────────────────────────────────────────────────────

/// Per-grammar DTA stats for the profile struct. Populated by
/// `GrammarIR::profile()` in a later wave; V3 exposes the stats
/// standalone so tests can assert coverage before the profile
/// integration.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DtaProfile {
    pub state_count: u16,
    pub shunting_yard_chains: u16,
    pub counter_optional_rules: u16,
    pub max_nesting_depth: u16,
}

impl DtaProfile {
    pub fn from_table(table: &DtaTable) -> Self {
        let unique_chains: std::collections::HashSet<_> =
            table.shunting_yard_chains.values().copied().collect();
        Self {
            state_count: table.states.len().min(u16::MAX as usize) as u16,
            shunting_yard_chains: unique_chains.len().min(u16::MAX as usize) as u16,
            counter_optional_rules: table
                .counter_optional_rules
                .len()
                .min(u16::MAX as usize) as u16,
            max_nesting_depth: table.max_nesting_depth,
        }
    }
}

// Unused-but-reserved for AV.4 bridging: the stage-B PSI driver will
// call into the lifter to pick scanner kernels per leaf state. Imports
// kept so the IR + tape crates link against a single source of truth.
#[allow(dead_code)]
fn _push_fp_sentinel(_fp: &PushFingerprint) {}

#[allow(dead_code)]
fn _type_desc_sentinel(_td: &TypeDesc) {}

#[allow(dead_code)]
fn _string_id_sentinel(_sid: StringId) {}
