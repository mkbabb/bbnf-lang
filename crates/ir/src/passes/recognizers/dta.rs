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

use crate::{FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleId, StringId, TypeDesc};

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

/// AW-III.W1.6 — Seq emission promotion classification.
///
/// `Default` keeps the legacy structural Seq-compound emission (one
/// parent record + N children). `KvPair` instructs the walker to
/// emit the Seq as a flat `TapeKind::KvPair` leaf — the rule's
/// projection collapses to (key span, scalar payload) so the
/// compound + children layout is wasteful structural overhead.
/// Triggered by the lifter when the enclosing rule's
/// `payload_layouts` entry matches the KvPair shape.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SeqPromote {
    Default,
    KvPair,
}

/// AW-III.W1 — typed-leaf payload classification for `Map { Literal,
/// MapExpr::IntLit/BoolLit/FloatLit }` projections.
///
/// The lifter resolves the enclosing `Map { Literal,
/// MapExpr::IntLit/BoolLit/FloatLit }` into one of these variants;
/// the emitter lowers each variant 1:1 to the runtime payload kind.
/// `None` is the structural-only sentinel.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralPayload {
    /// No `->` annotation; emit the legacy structural Literal arm.
    None,
    /// `Map { Literal, IntLit }` projecting to `u8`.
    U8(u8),
    /// `Map { Literal, BoolLit }`.
    Bool(bool),
    /// `Map { Literal, IntLit }` projecting to `u32`.
    U32(u32),
    /// `Map { Literal, IntLit }` projecting to `i64`/`u64`.
    U64(u64),
    /// `Map { Literal, FloatLit }` projecting to `f64`.
    F64(f64),
}

/// AW-III.W1 — regex-decoder selector classifying which Stage-B
/// payload-decoding routine the runtime invokes for a Regex match.
///
/// The lifter resolves an enclosing `Map { Regex, FnDescriptor }` into
/// the matching decoder variant. `None` skips the Stage-B job.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegexPayloadKind {
    F64,
    U8,
    Bool,
    HexU32,
    I64,
    String,
    AggregateLarge,
}

/// A single DFA state — one node in the lifted grammar.
#[derive(Clone, Debug, PartialEq)]
pub enum DtaState {
    /// Match a literal byte sequence at the current offset.
    Literal {
        text: StringId,
        /// AW-III.W1 — typed-leaf payload threaded from the enclosing
        /// `Map { Literal, MapExpr::IntLit/BoolLit/FloatLit }`. The
        /// emitter lowers this 1:1 into the runtime literal-payload
        /// constant emitted at the leaf site.
        payload: LiteralPayload,
    },
    /// Match a regex pattern at the current offset (runs through the
    /// regex subsystem; no DFA inlining here).
    Regex {
        pattern: StringId,
        /// AW-III.W1 — decoder selector for the matched bytes; the
        /// emitter lowers `Some(_)` to the runtime payload-kind tag
        /// and the walker enqueues a `PayloadJob`. `None` keeps the
        /// payload-less Span emission.
        payload: Option<RegexPayloadKind>,
    },
    /// Match nothing, consume nothing.
    Epsilon,
    /// Linear composition — run each child state in order.
    Seq {
        children: Vec<StateId>,
        /// Attached frame kind when the Seq is materialised into a
        /// record. `None` means transparent — the driver inherits the
        /// parent's frame.
        frame: FrameKind,
        /// AW-III.W1.6 — when the enclosing rule's payload_layout is
        /// the KvPair shape (`Tuple([Span, scalar])`), the lifter
        /// stamps `KvPair` here so the walker emits the entire Seq's
        /// (key span, scalar bytes) projection as a single flat
        /// `TapeKind::KvPair` leaf instead of a structural compound.
        /// `Default` keeps the legacy Seq compound emission.
        promote: SeqPromote,
    },
    /// Byte-class dispatched Alt. `table[b]` is the StateId for the
    /// branch whose FIRST set admits byte `b`, or
    /// [`StateId::NONE`] if no branch starts with `b`.
    ByteDispatch {
        table: Vec<StateId>,
        fallback: Option<StateId>,
    },
    /// AW-III.W6.3 / AW-IV.W3.3 — ClassifyByte dispatched Alt — mined
    /// by `disjoint_first` pass when every branch's FIRST set is
    /// mutually-disjoint. Structurally equivalent to `ByteDispatch`
    /// but the IR discriminant preserves mining provenance so the
    /// emitter can specialise downstream (AW-IV.W3.3 lowers it to a
    /// single indexed load on a `&'static [DtaStateId; 256]` LUT —
    /// the tightest dispatch form, one load + branch per Alt entry).
    /// Per §6 the mechanism is grammar-agnostic. AW-IV.W3.3 promotes
    /// ClassifyByte ahead of ByteDispatch: when `disjoint_first` has
    /// mined a table, it replaces the upstream `compute_dispatch`
    /// admission so the walker arm sees the specialised LUT path.
    ClassifyByte {
        table: Vec<StateId>,
        fallback: Option<StateId>,
    },
    /// AW-III.W5-carry — ConsumeToNextStructural lift.
    ///
    /// Emitted when the [`pattern_alphabet`](super::pattern_alphabet)
    /// miner proves a regex pattern's matchable-byte set is disjoint
    /// from the grammar's structural alphabet. The walker collapses
    /// what would have been a byte-by-byte regex scan to an O(1)
    /// cursor jump: `cursor.pos = idx.positions[cursor.slot];
    /// cursor.slot += 1;`.
    ///
    /// Carries no payload — the matched bytes form a structural-hole
    /// span whose content is read back via `span_lo..span_hi` if the
    /// downstream emitter needs it. When the structural index is
    /// empty (cold-path replay), the walker falls back to the
    /// conventional regex scan via the interior `pattern` field.
    ConsumeToNextStructural {
        /// Original pattern — used as the cold-path fallback when the
        /// stage-1 structural index is empty.
        pattern: StringId,
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
    WsTrim { pattern: Option<StringId> },
    /// AW-II.W5b — Set-difference (`IrNode::Minus` lowering).
    ///
    /// Matches `primary` only if `excluded` does NOT match at the
    /// same starting offset. Mirrors the VM compiler's
    /// `compile_minus` semantic — savepoint, probe `excluded`; on
    /// success → fail; on failure → run `primary` and consume its
    /// match.
    ///
    /// The prior lifter silently discarded `excluded` and routed the
    /// left operand through unchanged. Every EBNF terminal
    /// (`character - "'"`, `character - '"'`) therefore accepted the
    /// quote byte it was meant to exclude, breaking the grammar.
    Minus { primary: StateId, excluded: StateId },
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
    /// pushed compound. May refer to a separate alphabet-providing
    /// rule (e.g. Sheets `add_op`) shared across rungs.
    pub op_rule: RuleId,
    /// The chain rung whose body literally introduces this
    /// operator. Distinct from `op_rule` when the rung delegates to
    /// a shared alphabet rule (e.g. `add_expr` rung, `op_rule =
    /// add_op`, `rung_rule = add_expr`); equal to `op_rule` for
    /// inlined Alt / Literal rungs that own their alphabet directly
    /// (Sheets `array_row` ` , `, `array_rows` ` ; `).
    /// Per-rule LUT projection in
    /// [`crate::passes::recognizers::operator_chain`] keys on this
    /// field so list-shaped rungs do not over-share separators.
    pub rung_rule: RuleId,
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
        // AW-III.W1.6 — Seq → KvPair promotion. When the enclosing
        // rule's `payload_layouts` entry exists AND the entry state
        // is a Seq, mark the Seq as KvPair-promoted so the walker
        // emits a flat `TapeKind::KvPair` leaf instead of a Seq
        // compound. The KvPair-shape gate ([`Tuple([Span, scalar])`])
        // is upstream — this just consumes the layout pass's
        // decision. CSS `dirPseudo` / `hex` and friends collapse via
        // this promotion.
        if self.ir.payload_layouts.contains_key(&rule.id) {
            self.maybe_promote_to_kv_pair(entry);
        }
        self.rule_entries.insert(rule.id, entry);
    }

    /// AW-III.W1.6 — promote a Seq state to KvPair when the
    /// enclosing rule has an aggregate payload_layout. Idempotent;
    /// non-Seq states are left alone.
    fn maybe_promote_to_kv_pair(&mut self, entry: StateId) {
        let idx = entry.0 as usize;
        if let Some(state) = self.states.get_mut(idx) {
            if let DtaState::Seq { promote, .. } = state {
                *promote = SeqPromote::KvPair;
            }
        }
    }

    fn lift_node(&mut self, node: &IrNode) -> StateId {
        self.lift_node_with_payload(node, LiteralPayload::None, None)
    }

    /// AW-III.W6.3 / AW-IV.W3.3 — look up the disjoint_first mining
    /// entry for an `IrNode::Alt`. Returns `None` when the Alt didn't
    /// admit ClassifyByte substitution (overlapping FIRST sets, empty
    /// branches, or the mining pass didn't run). The lifter consults
    /// this FIRST; when it returns `Some`, `ClassifyByte` supersedes
    /// any upstream `ByteDispatch` admission for the same Alt.
    fn lookup_disjoint_first(
        &self,
        node: &IrNode,
    ) -> Option<&'ir crate::passes::recognizers::disjoint_first::DisjointFirstTable> {
        let dag = self.ir.dag.as_ref()?;
        let node_id = dag.node_for(node)?;
        self.ir.disjoint_first_tables.get(&node_id)
    }

    /// AW-III.W5-carry — check whether an `IrNode::Regex` node has
    /// been admitted to `ConsumeToNextStructural` lifting.
    fn is_ctns_lifted(&self, node: &IrNode) -> bool {
        let Some(dag) = self.ir.dag.as_ref() else {
            return false;
        };
        let Some(node_id) = dag.node_for(node) else {
            return false;
        };
        self.ir.ctns_lifts.contains(&node_id)
    }

    /// AW-III.W1 — lift `node` while threading caller-supplied payload
    /// resolutions into a `Literal` / `Regex` leaf. The
    /// `IrNode::Map { inner, fn_id }` arm walks `fn_id` into a typed
    /// `LiteralPayload` (constants) or `RegexPayloadKind` (decoders)
    /// and recurses with the payload propagated; `Alt` propagates the
    /// caller-supplied payload to every branch so the per-branch
    /// `Map { Literal, IntLit }` shape (Sheets `add_op = "+" -> 0u8 |
    /// "-" -> 1u8`) lands correctly when reached through an outer
    /// per-branch lift.
    fn lift_node_with_payload(
        &mut self,
        node: &IrNode,
        literal_payload: LiteralPayload,
        regex_payload: Option<RegexPayloadKind>,
    ) -> StateId {
        self.current_depth = self.current_depth.saturating_add(1);
        self.max_depth = self.max_depth.max(self.current_depth);
        let id = match node {
            IrNode::Literal(sid) => self.alloc_state(DtaState::Literal {
                text: *sid,
                payload: literal_payload,
            }),
            IrNode::Regex(sid) => {
                // AW-III.W5-carry / AW-IV.W3.5b — consult the CTNS
                // lifter. When the pattern's matchable alphabet is
                // disjoint from the grammar's structural alphabet,
                // lift to `ConsumeToNextStructural` so the walker's
                // hot path collapses the byte-by-byte regex scan into
                // a single cursor jump via the stage-1 structural
                // index.
                //
                // The lift is gated on both:
                // 1. no payload decoder (CTNS emits structural-only
                //    spans; decoders need the scanned byte range).
                // 2. the lifter flagging this NodeId (the miner's
                //    admission check proves alphabet disjointness).
                //
                // AW-IV.W3.5b — the `ctns_enabled = false` gate cf691347
                // landed because the walker's Regex arm writes a Span
                // record and the CTNS arm wrote no record; downstream
                // grammar consumers fell off the data. W3.5b adds the
                // `TapeKind::Scanned` record variant for the CTNS arm
                // so the leaf-emit contract holds across both lifts;
                // un-gated here.
                if regex_payload.is_none() && self.is_ctns_lifted(node) {
                    self.alloc_state(DtaState::ConsumeToNextStructural { pattern: *sid })
                } else {
                    self.alloc_state(DtaState::Regex {
                        pattern: *sid,
                        payload: regex_payload,
                    })
                }
            }
            IrNode::Epsilon => self.alloc_state(DtaState::Epsilon),
            IrNode::Seq(children) => {
                let child_states: Vec<StateId> =
                    children.iter().map(|c| self.lift_node(c)).collect();
                self.alloc_state(DtaState::Seq {
                    children: child_states,
                    frame: FrameKind::Seq,
                    promote: SeqPromote::Default,
                })
            }
            IrNode::Alt(branches, dispatch) => {
                // AW-III.W1: caller-supplied payload propagates into
                // every branch so the per-branch `Map { Literal,
                // IntLit }` shape (Sheets `add_op = "+" -> 0u8 | "-"
                // -> 1u8`) inherits its discriminant when the Alt is
                // reached through an outer Map. Inner per-branch Maps
                // (the Sheets shape) override their parent's payload
                // because their own FnDescriptor specifies the value.
                let branch_states: Vec<StateId> = branches
                    .iter()
                    .map(|b| self.lift_node_with_payload(&b.node, literal_payload, regex_payload))
                    .collect();
                // AW-IV.W3.3 — reverse the dispatch gate.
                //
                // Pre-W3.3 `compute_dispatch` admitted every
                // disjoint-FIRST candidate first as `ByteDispatch`, so
                // the new `ClassifyByte` mining never observed any
                // admissible Alts: the miner ran, produced its table,
                // and the lifter always deferred to the upstream
                // dispatch because `dispatch.is_some()`. Reverse:
                // `ClassifyByte` runs FIRST. When `disjoint_first` has
                // mined a table, it REPLACES `ByteDispatch` — the
                // walker arm becomes a single indexed load on the
                // precomputed `[DtaStateId; 256]` LUT, the tightest
                // dispatch form. When `disjoint_first` has NOT mined
                // this Alt (overlapping FIRSTs, empty branches, or
                // regex-valued branches that preclude single-byte
                // classification), fall back to `ByteDispatch` from
                // the upstream `compute_dispatch` pass — the less-
                // optimised path that preserves fallback-branch
                // handling + 128-entry ASCII window semantics.
                // `AltLinear` remains the final fallback when neither
                // pass admitted the Alt.
                if let Some(disjoint) = self.lookup_disjoint_first(node) {
                    let mut table = vec![StateId::NONE; 256];
                    for (byte, branch_idx) in disjoint.table.iter().enumerate() {
                        if *branch_idx == u8::MAX {
                            continue;
                        }
                        if let Some(&state) = branch_states.get(*branch_idx as usize) {
                            table[byte] = state;
                        }
                    }
                    return self.alloc_state(DtaState::ClassifyByte {
                        table,
                        fallback: None,
                    });
                }
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
                    self.alloc_state(DtaState::ByteDispatch { table, fallback })
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
                self.alloc_state(DtaState::Ref { rule: *rid, target })
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
                    promote: SeqPromote::Default,
                })
            }
            IrNode::Next(a, b) => {
                let a_state = self.lift_node(a);
                let b_state = self.lift_node(b);
                self.alloc_state(DtaState::Seq {
                    children: vec![a_state, b_state],
                    frame: FrameKind::Seq,
                    promote: SeqPromote::Default,
                })
            }
            IrNode::Minus(primary, excluded) => {
                // AW-II.W5b — Set-difference: match `primary` only if
                // `excluded` does NOT match at the same start offset.
                // Mirrors the VM compiler's `compile_minus` (probe b;
                // if b succeeds, overall fails; else run a).
                //
                // Pre-W5b the lifter silently discarded the right
                // operand — `character - "'"` lowered to `character`
                // alone, so every EBNF terminal accepted the quote
                // byte it was meant to exclude.
                let primary_state = self.lift_node(primary);
                let excluded_state = self.lift_node(excluded);
                self.alloc_state(DtaState::Minus {
                    primary: primary_state,
                    excluded: excluded_state,
                })
            }
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
                    promote: SeqPromote::Default,
                })
            }
            IrNode::Map { inner, fn_id } => {
                // AW-III.W1: resolve the FnDescriptor into a typed
                // payload classification and propagate it into the
                // recursive lift. The pre-W1 lifter dropped the Map
                // wrapper wholesale (`self.lift_node(inner)`), losing
                // every typed-leaf annotation. The arms below mirror
                // the codegen routing for the Stage-B payload jobs:
                //
                //   - `IntLit` / `FloatLit` / `BoolLit` over a literal
                //     scanner → `LiteralPayload::*`. Walker writes the
                //     constant into the arena post-match.
                //   - `NumberConvert` over a regex → `RegexPayloadKind::F64`.
                //   - `HexConvert` over a regex → `RegexPayloadKind::HexU32`.
                //   - `SpanCapture` (input : Span) — no payload, stays
                //     structural.
                //   - `EnumWrap` / `BoxWrap` — no payload classification;
                //     they project compound type shape, not leaf data.
                let fn_desc = self.ir.fns.get(*fn_id as usize);
                let (lit_payload, mut rx_payload) = resolve_map_payload(fn_desc, inner);
                // AW-III.W1 universal-named arm: when the resolver
                // declined and the FnDescriptor is `Expr` with a
                // `Named` return type, project well-known names
                // ("String", "Bytes") to their decoder selectors.
                if rx_payload.is_none() && matches!(strip_to_leaf(inner), IrNode::Regex(_)) {
                    if let Some(FnDescriptor::Expr {
                        return_type: Some(TypeDesc::Named(sid)),
                        ..
                    }) = fn_desc
                    {
                        let name = self.ir.get_string(*sid);
                        if let Some(kind) = regex_payload_from_named(name) {
                            rx_payload = Some(kind);
                        }
                    }
                }
                let inherit_lit = if matches!(lit_payload, LiteralPayload::None) {
                    literal_payload
                } else {
                    lit_payload
                };
                let inherit_rx = rx_payload.or(regex_payload);
                self.lift_node_with_payload(inner, inherit_lit, inherit_rx)
            }
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
                    promote: SeqPromote::Default,
                })
            }
            IrNode::TokenDispatch {
                token,
                arms: _,
                fallback,
            } => {
                // TokenDispatch is an existing dispatch lowering — the
                // DTA inherits it by lifting the token + fallback;
                // arms are lifted via the parent's Alt shape.
                let token_state = self.lift_node(token);
                let fallback_state = self.lift_node(fallback);
                self.alloc_state(DtaState::Seq {
                    children: vec![token_state, fallback_state],
                    frame: FrameKind::Seq,
                    promote: SeqPromote::Default,
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
        IrNode::Repeat {
            lo: 0,
            hi: 1,
            inner,
        } => {
            // The inner Repeat is an optional; count the sub-optionals
            // inside its body.
            inner_contains_optional(inner)
        }
        IrNode::Seq(children) => children.iter().any(has_nested_optional_with_empty_body),
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| has_nested_optional_with_empty_body(&b.node)),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) | IrNode::Negate(inner) => {
            has_nested_optional_with_empty_body(inner)
        }
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
        IrNode::Alt(branches, _) => branches.iter().any(|b| inner_contains_optional(&b.node)),
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
        let Some((operand_rule, operators, operand_node)) = match_operator_chain_rule(ir, cursor)
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
///
/// AX.W0a.2.l — lifted to `pub(super)` so the sibling
/// [`super::operator_chain`] miner can reuse the structural matcher
/// for single-rung Pratt-classified rules (BBNF's `binary_factor`)
/// that the DTA `collect_precedence_chain` walker-path collapse
/// skips (it requires ≥ 2 rungs).
pub(super) fn match_operator_chain_rule(
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
/// `owning_rule_id` is the rung's own rule id — used as the
/// entries' `rung_rule` field for per-rung LUT scoping in the
/// downstream miner. When the operator is inlined (Alt / Literal
/// arms), `owning_rule_id` also serves as `op_rule`. When the
/// operator delegates to a shared alphabet rule (Ref arm), `op_rule`
/// is the referenced rule's id while `rung_rule` stays the rung's id.
fn extract_operator_set(
    ir: &GrammarIR,
    node: &IrNode,
    owning_rule_id: RuleId,
) -> Option<Vec<PrecedenceEntry>> {
    let stripped = strip_transparent_owned(node);
    match stripped {
        IrNode::Ref(rid) => {
            let op_rule = ir.rules.iter().find(|r| r.id == rid)?;
            collect_operator_alternatives(ir, op_rule, rid, owning_rule_id)
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
                rung_rule: owning_rule_id,
                op_discriminant: 0,
            }])
        }
        _ => None,
    }
}

/// Collect operator entries from an inlined Alt node. The node must
/// be an Alt whose branches are each a Literal, possibly wrapped in
/// Map/OptionalWhitespace, or prefix-factored as
/// `Seq(Literal(prefix), Alt(suffixes))` post-optimizer.
///
/// AX.W0a.2.o: Sheets `comparison_expr`'s inlined `compare_op`
/// surfaces a prefix-factored branch (`Seq(Literal("<"),
/// Alt(Literal(">") | Literal("=")))` → `["<>", "<="]`); handle it
/// via [`extract_literal_set`] so the admission sees every operator
/// literal the rule admits.
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
    let mut discriminant: usize = 0;
    for branch in branches.iter() {
        let literals = extract_literal_set(&branch.node, ir)?;
        for literal in literals {
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
                rung_rule: owning_rule_id,
                op_discriminant: discriminant.min(u8::MAX as usize) as u8,
            });
            discriminant += 1;
        }
    }
    Some(out)
}

/// AW-III.W1 — resolve a `Map { inner, fn_id }` into the typed-leaf
/// payload classification the lifter threads into the lifted leaf.
///
/// Returns `(LiteralPayload, Option<RegexPayloadKind>)` — at most one
/// is non-trivial depending on `inner`'s shape:
///
/// - `inner` matches `Literal(_)` (or `Map`-chained over one):
///   `MapExpr::IntLit(value)` / `BoolLit(value)` / `FloatLit(value)`
///   convert into `LiteralPayload::U8/U32/U64/Bool/F64` per the
///   destination width inferred from the value bounds.
/// - `inner` matches `Regex(_)`:
///   - `FnDescriptor::NumberConvert` → `RegexPayloadKind::F64`.
///   - `FnDescriptor::HexConvert` → `RegexPayloadKind::HexU32`.
///   - `FnDescriptor::Expr { return_type: Some(...) }` over a regex
///     with `MapExpr::Input` body → typed Span / String routing.
///
/// `SpanCapture`, `EnumWrap`, `BoxWrap`, and `Expr` arms whose return
/// shape is structural (Tuple / Enum / Boxed) yield no leaf payload —
/// the typed value lives in the parent's compound-payload aggregate.
fn resolve_map_payload(
    fn_desc: Option<&FnDescriptor>,
    inner: &IrNode,
) -> (LiteralPayload, Option<RegexPayloadKind>) {
    let Some(desc) = fn_desc else {
        return (LiteralPayload::None, None);
    };
    let inner_is_literal = matches!(strip_to_leaf(inner), IrNode::Literal(_));
    let inner_is_regex = matches!(strip_to_leaf(inner), IrNode::Regex(_));
    match desc {
        FnDescriptor::Expr { expr, return_type } => {
            if inner_is_literal {
                if let Some(payload) = literal_payload_from_expr(expr, return_type.as_ref()) {
                    return (payload, None);
                }
            }
            if inner_is_regex {
                if let Some(kind) = regex_payload_from_return(return_type.as_ref()) {
                    return (LiteralPayload::None, Some(kind));
                }
                // AW-III.W1 universal-named arm — `TypeDesc::Named`
                // routes via the IR string interner. Caller's IR is
                // not in scope here; the helper signature accepts an
                // already-resolved name string. The lifter's Map arm
                // resolves Named via `ir.get_string` before this
                // helper sees the input.
            }
            (LiteralPayload::None, None)
        }
        FnDescriptor::NumberConvert { .. } => {
            if inner_is_regex {
                (LiteralPayload::None, Some(RegexPayloadKind::F64))
            } else {
                (LiteralPayload::None, None)
            }
        }
        FnDescriptor::HexConvert { .. } => {
            if inner_is_regex {
                (LiteralPayload::None, Some(RegexPayloadKind::HexU32))
            } else {
                (LiteralPayload::None, None)
            }
        }
        // SpanCapture leaves the matched bytes as the canonical
        // payload — the runtime reads them via `span_lo` / `span_hi`
        // directly; no decoder slot needed.
        FnDescriptor::SpanCapture => (LiteralPayload::None, None),
        // EnumWrap / BoxWrap project compound type shape only — the
        // typed leaves are the inner node's own payloads.
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => (LiteralPayload::None, None),
    }
}

/// Strip Map / OptionalWhitespace wrappers down to the underlying
/// leaf node — used by [`resolve_map_payload`] to classify what
/// scanner the Map encloses.
fn strip_to_leaf(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => strip_to_leaf(inner),
        _ => node,
    }
}

/// Decode `MapExpr` constants into a [`LiteralPayload`].
///
/// The width is inferred from the explicit `return_type` annotation
/// when present (CSS `dirKeyword 'ltr' -> 0u8` carries return_type =
/// `Some(U8)`); falls back to value-bound inference when absent
/// (lifter has no type-context for a bare integer literal).
fn literal_payload_from_expr(
    expr: &MapExpr,
    return_type: Option<&TypeDesc>,
) -> Option<LiteralPayload> {
    match expr {
        MapExpr::IntLit(v) => Some(int_literal_payload(*v, return_type)),
        MapExpr::BoolLit(b) => Some(LiteralPayload::Bool(*b)),
        MapExpr::FloatLit(v) => Some(LiteralPayload::F64(*v)),
        // Constant-foldable subtree — fold then re-classify.
        _ => {
            let mut folded = expr.clone();
            folded.constant_fold();
            match folded {
                MapExpr::IntLit(v) => Some(int_literal_payload(v, return_type)),
                MapExpr::BoolLit(b) => Some(LiteralPayload::Bool(b)),
                MapExpr::FloatLit(v) => Some(LiteralPayload::F64(v)),
                _ => None,
            }
        }
    }
}

/// Map an `IntLit(i64)` to its narrow payload variant per the
/// declared `return_type`. When unannotated, the lifter chooses the
/// narrowest variant that holds the value.
fn int_literal_payload(value: i64, return_type: Option<&TypeDesc>) -> LiteralPayload {
    let by_type = return_type.and_then(|td| match td {
        TypeDesc::U8 | TypeDesc::I8 => Some(LiteralPayload::U8(value as u8)),
        TypeDesc::U16 | TypeDesc::I16 => Some(LiteralPayload::U32(value as u32)),
        TypeDesc::U32 | TypeDesc::I32 => Some(LiteralPayload::U32(value as u32)),
        TypeDesc::U64 | TypeDesc::I64 => Some(LiteralPayload::U64(value as u64)),
        TypeDesc::F64 => Some(LiteralPayload::F64(value as f64)),
        TypeDesc::Bool => Some(LiteralPayload::Bool(value != 0)),
        _ => None,
    });
    by_type.unwrap_or_else(|| {
        if (0..=255).contains(&value) {
            LiteralPayload::U8(value as u8)
        } else if (i32::MIN as i64..=u32::MAX as i64).contains(&value) {
            LiteralPayload::U32(value as u32)
        } else {
            LiteralPayload::U64(value as u64)
        }
    })
}

/// Map an explicit return-type annotation on `Map { Regex, Expr }`
/// into the matching `RegexPayloadKind`. Returns `None` for
/// non-decodable shapes (`Tuple`, `Enum`, etc.) — those flow through
/// the structural path.
///
/// `Bool` projects to its dedicated `Bool` decoder (case-insensitive
/// `true`/`false` discrimination) rather than `U8`. `U8`/`I8` would
/// reduce the matched bytes to `slice[0]`, which is the wrong byte
/// for case-insensitive scanners (`/TRUE/i` matches `T` or `t` —
/// neither is `1`). Sheets `boolean = /TRUE/i -> true` is the
/// canonical case.
///
/// `Named("String")` / `Named("Bytes")` route through the `String`
/// decoder (UTF-8 byte slice copied verbatim into the arena —
/// JSON's `decode_json_string_to_arena` host fn is the canonical
/// case; the runtime decoder writes the matched bytes into
/// `pay_agg` so downstream consumers slice them as a borrowed
/// `&str`).
fn regex_payload_from_return(return_type: Option<&TypeDesc>) -> Option<RegexPayloadKind> {
    let td = return_type?;
    Some(match td {
        TypeDesc::F64 => RegexPayloadKind::F64,
        TypeDesc::Bool => RegexPayloadKind::Bool,
        TypeDesc::U8 | TypeDesc::I8 => RegexPayloadKind::U8,
        TypeDesc::U32 | TypeDesc::I32 => RegexPayloadKind::HexU32,
        TypeDesc::I64 | TypeDesc::U64 => RegexPayloadKind::I64,
        TypeDesc::Span => return None, // Span lives in span_lo/hi — no decoder
        _ => return None,
    })
}

/// AW-III.W1 — extend `regex_payload_from_return` with the
/// universal-named arm. Called from the lifter's Map arm when the
/// regular return-type matcher declines.
fn regex_payload_from_named(name: &str) -> Option<RegexPayloadKind> {
    match name {
        "String" | "str" => Some(RegexPayloadKind::String),
        "Bytes" => Some(RegexPayloadKind::AggregateLarge),
        _ => None,
    }
}

/// Strip transparent wrappers (OptionalWhitespace, Map) from a node,
/// returning a cloned owned tree with the wrappers dissolved. Used
/// by the chain matcher because pattern-bind against owned enum
/// variants sidesteps the nested-box lifetime tangle that a
/// borrowed-walk would introduce.
///
/// AW-III.W1.7 — also peel `IrNode::Next(a, b)` so the chain matcher
/// recognises sequences shaped as `Next(operand, Repeat(...))` (CSS
/// `calc(2 * (3 + 4))` lowers `value `>>` operator chain` via
/// `Next`, not `Seq`). Without the peel `match_operator_chain_rule`
/// rejects every `Next`-shaped rule, leaving the lifter to emit a
/// long ByteDispatch chain instead of a single shunting-yard state.
/// The peel lifts both operands in order and wraps them in a
/// synthetic `Seq` so the chain detector's `Seq(operand, tail)`
/// pattern matches.
fn strip_transparent_owned(node: &IrNode) -> IrNode {
    match node {
        IrNode::OptionalWhitespace(inner) => strip_transparent_owned(inner),
        IrNode::Map { inner, .. } => strip_transparent_owned(inner),
        IrNode::Next(a, b) => {
            IrNode::Seq(vec![strip_transparent_owned(a), strip_transparent_owned(b)])
        }
        IrNode::Skip(a, b) => {
            IrNode::Seq(vec![strip_transparent_owned(a), strip_transparent_owned(b)])
        }
        _ => node.clone(),
    }
}

fn collect_operator_alternatives(
    ir: &GrammarIR,
    op_rule: &IrRule,
    op_rule_id: RuleId,
    rung_rule_id: RuleId,
) -> Option<Vec<PrecedenceEntry>> {
    let body = strip_transparent_owned(&op_rule.body);

    // Single-Literal op-rule body — `op = "+" -> Plus` and friends
    // — projects to a one-entry chain. The Alt-of-Literal path
    // below subsumes the n>=2 cases; the n=1 degenerate is the
    // sibling fall-through. Without this projection the Pratt
    // detector rejects every Ref-headed single-operator chain
    // (`w4_pratt_detector_admits_skip_based_operator_chain` fixture
    // and any grammar whose op-rule reduces to a single literal
    // post-Map-strip).
    let literals = match body {
        IrNode::Alt(branches, _) => {
            let mut out = Vec::new();
            for branch in branches.iter() {
                let lits = extract_literal_set(&branch.node, ir)?;
                out.extend(lits);
            }
            out
        }
        _ => extract_literal_set(&body, ir)?,
    };

    let mut out = Vec::with_capacity(literals.len());
    let mut discriminant: usize = 0;
    for literal in literals {
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
            rung_rule: rung_rule_id,
            op_discriminant: discriminant.min(u8::MAX as usize) as u8,
        });
        discriminant += 1;
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

/// Extract the set of literal strings an operator-branch node admits.
///
/// Extends [`extract_literal`] to handle prefix-factored Alt branches
/// produced by the optimizer's prefix-tree factoring on literal-led
/// Alts. The factoring rewrites
/// `Alt(Literal("<>") | Literal("<=") | Literal("<") | …)` as
/// `Alt(Seq(Literal("<"), Alt(Literal(">") | Literal("="))) | Literal("<") | …)`
/// — the Seq-factored branch expands to `["<>", "<="]` via the
/// Cartesian product of its prefix / suffix literal sets.
///
/// AX.W0a.2.o: Sheets `compare_op`'s post-factor body surfaces one
/// Seq-factored branch; single-literal `collect_operator_alternatives`
/// rejected the entire Alt when that branch failed `extract_literal`.
/// Factoring-aware expansion admits the miner's seven-literal chain.
///
/// Returns `None` when a branch is not literal-derivable (Seq with
/// non-literal head, Ref chain, Regex leaf, etc.) — those shapes are
/// not operator alternatives and the miner must not invent a
/// single-byte projection for them.
fn extract_literal_set(node: &IrNode, ir: &GrammarIR) -> Option<Vec<String>> {
    match strip_transparent_owned(node) {
        IrNode::Literal(sid) => Some(vec![ir.get_string(sid).to_string()]),
        IrNode::Map { inner, .. } => extract_literal_set(&inner, ir),
        IrNode::Seq(children) => {
            // Prefix-factored branch: `Seq([Literal(prefix), Alt(suffixes)])`
            // — expand the Cartesian product of the prefix with each
            // suffix literal. Chain with more than one non-trivial
            // child beyond the prefix-suffix split is not a valid
            // operator alternative and we reject.
            let substantive: Vec<&IrNode> = children
                .iter()
                .filter(|c| {
                    !matches!(
                        strip_transparent_owned(c),
                        IrNode::Epsilon | IrNode::OptionalWhitespace(_)
                    )
                })
                .collect();
            if substantive.len() != 2 {
                return None;
            }
            let prefix = extract_literal(substantive[0], ir)?;
            let suffix_alt = strip_transparent_owned(substantive[1]);
            let suffix_branches = match suffix_alt {
                IrNode::Alt(b, _) => b,
                _ => return None,
            };
            let mut out = Vec::new();
            for suffix_branch in &suffix_branches {
                let suffix_literals = extract_literal_set(&suffix_branch.node, ir)?;
                for suffix in suffix_literals {
                    let mut combined = prefix.clone();
                    combined.push_str(&suffix);
                    out.push(combined);
                }
            }
            if out.is_empty() {
                return None;
            }
            Some(out)
        }
        IrNode::Alt(branches, _) => {
            // Nested Alt — flatten each branch's literal set.
            let mut out = Vec::new();
            for branch in &branches {
                let literals = extract_literal_set(&branch.node, ir)?;
                out.extend(literals);
            }
            if out.is_empty() {
                return None;
            }
            Some(out)
        }
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

// AZ-II.cutover.A — `summarise` / `DtaSummary` / `DtaProfile` retired
// per `audit/AUDIT-3-DECAY-INVENTORY.md` §1 + AUDIT-6 §8.4. Verified
// zero non-doc consumers across the workspace pre-deletion. The
// remaining DTA surface (`DtaState`, `DtaTable`, `DtaBuilder`,
// `lift_dta`, `Associativity`, `CounterOptional`, `FrameKind`,
// `LiteralPayload`, `PrecedenceEntry`, `PrecedenceTable`,
// `RegexPayloadKind`, `SeqPromote`, `StateId`) is consumed by
// `core::backend::rust::emitter::dfa_codegen`,
// `recognizers::operator_chain`, and `core::backend::rust::emitter::grammar`
// — those entries stay live until cutover.C retires the tape-direct
// path entirely.
