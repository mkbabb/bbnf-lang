//! Dispatch Tape Automaton (DTA) runtime types — Tranche AV Phase 3.
//!
//! # Architectural role
//!
//! The DTA replaces the recursive-descent-per-rule codegen for a
//! grammar's hot path. The lifter in
//! `bbnf_ir::passes::recognizers::dta` produces the owned state table
//! at compile time; the emitter lowers it to a `const DTA_TABLE` in
//! each grammar's `generated.rs`. The types in this module are the
//! wire contract between the two — the emitter writes values of these
//! types, the runtime driver reads them.
//!
//! # Design
//!
//! Every field is `const`-constructible. Slices point at `static`
//! arrays emitted immediately before the `DTA_TABLE` literal. No
//! runtime allocation, no lazy initialisation.
//!
//! The runtime driver (lives in the emitter-side `parse()` function
//! post-AV.3.6) walks [`DtaTable::states`] starting from the entry
//! state, maintaining a fixed-size `[Frame; 64]` counter stack +
//! heap overflow region for grammars with nesting depth > 64.
//!
//! # V3 delivery
//!
//! The types live here; the emitter emits the `const` data; the
//! runtime driver is part of AV.3.6 (legacy fn-per-rule deletion).
//! Until the driver lands, `DTA_TABLE` is unused data — the emitter
//! still drives `parse()` through the legacy fn-per-rule path. This
//! is the "wave V3 between-wave failure is acceptable" framing: the
//! lifted table ships in this wave; the driver that consumes it
//! ships in V4's PSI pipeline.

/// Opaque state identifier in [`DtaTable::states`]. Stable for the
/// lifetime of one grammar compilation.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DtaStateId(pub u16);

impl DtaStateId {
    /// Sentinel — no state.
    pub const NONE: DtaStateId = DtaStateId(u16::MAX);
}

/// IR rule id, mirrored on the tape side to avoid a crate dependency
/// edge.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DtaRuleId(pub u32);

/// Runtime frame class — drives stack advance semantics.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DtaFrameKind {
    /// Linear Seq advance; counter is child index.
    Seq = 0,
    /// Alt branch selection; counter is selected branch index.
    Alt = 1,
    /// Repeat iteration; counter is iteration count.
    Repeat = 2,
    /// Shunting-yard operator loop (AV.3.3).
    ShuntingYard = 3,
}

/// Counter-optional annotation (AV.3.2).
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DtaCounterOptional {
    /// Body has a nested optional with an empty body.
    Nested = 1,
    /// Body has a lookahead-determined optional that carries across
    /// dispatch.
    Lookahead = 2,
}

/// Operator associativity for the shunting-yard loop.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DtaAssociativity {
    /// Left-associative: `a op b op c` binds as `(a op b) op c`.
    Left = 0,
    /// Right-associative: `a op b op c` binds as `a op (b op c)`. In
    /// the Sheets precedence chain this is only `^`.
    Right = 1,
}

/// Runtime DTA state — the emitter lowers one of these per IR node.
///
/// The enum is `#[repr(u8)]` with explicit discriminants so the
/// emitter's lowering is unambiguous. Each variant's payload is a
/// flat struct of `const`-safe primitives and static slice refs.
#[derive(Clone, Copy, Debug)]
pub enum DtaState {
    /// Literal byte sequence — the driver calls into the scanner at
    /// the referenced pattern.
    Literal {
        /// Byte sequence to match verbatim.
        text: &'static str,
    },
    /// Regex pattern — routes through the existing regex subsystem.
    Regex {
        /// Pattern string (static interned).
        pattern: &'static str,
    },
    /// Matches the empty string, consumes nothing.
    Epsilon,
    /// Linear Seq composition — run each child state in order.
    Seq {
        /// Child state ids; indexed sequentially by the driver.
        children: &'static [DtaStateId],
        /// Frame kind attached to this Seq when it materialises a
        /// record.
        frame: DtaFrameKind,
    },
    /// Byte-class dispatched Alt — `table[b]` is the branch state
    /// for first byte `b`, or [`DtaStateId::NONE`] if no branch
    /// starts with `b`.
    ByteDispatch {
        /// 256-entry dispatch table.
        table: &'static [DtaStateId; 256],
        /// Optional catch-all fallback tried when `table[b] == NONE`.
        fallback: DtaStateId,
    },
    /// Alt with no disjoint-FIRST-set dispatch — linear branch
    /// attempt.
    AltLinear {
        /// Branches tried in order.
        branches: &'static [DtaStateId],
    },
    /// Repeat(inner, lo..=hi). `(0, 1)` = optional; `(0, u32::MAX)` =
    /// many; `(1, u32::MAX)` = many1.
    Repeat {
        /// Inner body state.
        inner: DtaStateId,
        /// Lower bound (inclusive).
        lo: u32,
        /// Upper bound (inclusive).
        hi: u32,
        /// AV.3.2 marker — non-`None` when the body has nested
        /// optional-with-empty-body and the driver must track a
        /// per-iteration counter flag.
        counter_optional: Option<DtaCounterOptional>,
    },
    /// Rule reference — follow into the rule's entry state.
    Ref {
        /// The target rule (read from the lift's `rule_entries`
        /// map via `rule_entry_for`).
        rule: DtaRuleId,
        /// Pre-resolved entry state; set when the target has already
        /// been lifted at compile time, else [`DtaStateId::NONE`]
        /// and the driver looks up through `rule_entries`.
        target: DtaStateId,
    },
    /// Shunting-yard operator loop (AV.3.3).
    ShuntingYard {
        /// Operand head state.
        head: DtaStateId,
        /// Precedence table — one entry per operator byte.
        precedence: &'static [DtaPrecedenceEntry],
    },
}

/// One operator's shunting-yard profile.
#[derive(Clone, Copy, Debug)]
pub struct DtaPrecedenceEntry {
    /// Operator's identifying first byte.
    pub byte: u8,
    /// Optional second byte for two-byte operators (`<<`, `>>`).
    pub second_byte: Option<u8>,
    /// Higher values bind tighter.
    pub precedence: u8,
    /// Left or right associative.
    pub associativity: DtaAssociativity,
    /// Rule whose variant_idx the runtime threads into the pushed
    /// compound (`+` / `-` both share `add_op`'s rule id with
    /// different discriminants).
    pub op_rule: DtaRuleId,
    /// Which Alt branch index within the op rule — stored as the
    /// typed payload's u8 discriminant.
    pub op_discriminant: u8,
}

/// Per-rule entry mapping — one row per non-transparent rule.
#[derive(Clone, Copy, Debug)]
pub struct DtaRuleEntry {
    /// The rule's id.
    pub rule: DtaRuleId,
    /// The rule's entry state.
    pub state: DtaStateId,
}

/// The lifted dispatch automaton — one `const` per grammar.
///
/// Emitted at the top of `generated.rs` next to `GRAMMAR_PROFILE`.
/// The runtime driver consumes it to walk the input byte-by-byte,
/// maintaining the counter-frame stack.
#[derive(Clone, Copy, Debug)]
pub struct DtaTable {
    /// Every state; indexed by [`DtaStateId`]. Slice length is a
    /// per-grammar compile-time constant.
    pub states: &'static [DtaState],
    /// Per-rule entry-state map, sorted by rule id for binary
    /// search (the table is bounded by rule count — ≤ 256 for every
    /// grammar in the corpus).
    pub rule_entries: &'static [DtaRuleEntry],
    /// Rules whose entry state is a shunting-yard collapse, sorted
    /// by rule id. Used by the emitter's `__rule_kind` dispatch
    /// (stays separate from the parser hot path per AV.md §AV.3.6).
    pub shunting_yard_rules: &'static [DtaRuleId],
    /// Counter-optional rule set — AV.3.2 marker rules.
    pub counter_optional_rules: &'static [DtaRuleId],
    /// Maximum nesting depth observed during the lift. The driver
    /// sizes its `[Frame; 64]` stack from this; grammars with
    /// depth > 64 overflow onto the heap region.
    pub max_nesting_depth: u16,
}

impl DtaTable {
    /// Empty table — used as a `const` default for grammars whose
    /// DTA has not yet been lifted.
    pub const EMPTY: DtaTable = DtaTable {
        states: &[],
        rule_entries: &[],
        shunting_yard_rules: &[],
        counter_optional_rules: &[],
        max_nesting_depth: 0,
    };

    /// Look up a rule's entry state via binary search.
    pub fn rule_entry_for(&self, rule: DtaRuleId) -> DtaStateId {
        match self
            .rule_entries
            .binary_search_by_key(&rule.0, |e| e.rule.0)
        {
            Ok(idx) => self.rule_entries[idx].state,
            Err(_) => DtaStateId::NONE,
        }
    }

    /// Whether the grammar has any shunting-yard chain.
    pub const fn has_shunting_yard(&self) -> bool {
        !self.shunting_yard_rules.is_empty()
    }

    /// Whether the grammar has any counter-optional rule.
    pub const fn has_counter_optional(&self) -> bool {
        !self.counter_optional_rules.is_empty()
    }
}

// ── AV.3.4 — diagnostic replay ──────────────────────────────────────
//
// The happy-path driver does not backtrack. Diagnostic mode re-walks
// the same state machine with an instrumentation hook that tracks the
// deepest successful advance and the failing state. The trace data is
// all the error emitter needs to produce a useful "expected X at Y"
// diagnostic without a second codegen path.

/// Diagnostic trace — populated during a replay run.
///
/// The runtime driver writes `furthest_offset` each time it advances
/// past the previous maximum, and stamps `failing_state` when the
/// dispatch dead-ends. The caller's error emitter consults both.
#[derive(Clone, Copy, Debug)]
pub struct DtaDiagnostic {
    /// Deepest byte offset reached by the driver during the run.
    pub furthest_offset: u32,
    /// The state that dispatched to no successful child at the
    /// deepest advance. `DtaStateId::NONE` until the driver fails.
    pub failing_state: DtaStateId,
    /// Rule id of the rule that was active when the failure
    /// occurred. `DtaRuleId(u32::MAX)` until populated.
    pub failing_rule: DtaRuleId,
    /// Number of states visited — useful for diagnosing diagnostics-
    /// mode overhead vs. happy-path cost.
    pub states_visited: u32,
}

impl DtaDiagnostic {
    /// Empty diagnostic — the driver initialises from this.
    pub const EMPTY: Self = Self {
        furthest_offset: 0,
        failing_state: DtaStateId::NONE,
        failing_rule: DtaRuleId(u32::MAX),
        states_visited: 0,
    };

    /// Update `furthest_offset` with `state` if `offset` exceeds the
    /// previous best. The driver calls this at every state transition;
    /// the `if` is the single cost in happy-path mode.
    #[inline]
    pub fn observe(&mut self, offset: u32, state: DtaStateId, rule: DtaRuleId) {
        if offset > self.furthest_offset {
            self.furthest_offset = offset;
            self.failing_state = state;
            self.failing_rule = rule;
        }
    }

    /// Increment the visited counter. Cheap in Release.
    #[inline]
    pub fn tick(&mut self) {
        self.states_visited = self.states_visited.saturating_add(1);
    }
}
