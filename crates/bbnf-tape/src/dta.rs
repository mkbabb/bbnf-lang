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

use crate::psi::PayloadKind;

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

/// AW-III.W1.6 — Seq compound emission promotion.
///
/// The lifter stamps this on every `DtaState::Seq` to instruct the
/// walker which tape shape to emit. `Default` keeps the legacy
/// `TapeKind::Seq` compound + structural children; `KvPair`
/// collapses the Seq projection — known by the lifter to be
/// `Tuple([Span, scalar])` via the rule's payload_layout — into a
/// flat `TapeKind::KvPair` leaf whose `child_off` points at the
/// scalar bytes in the arena.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SeqPromote {
    Default = 0,
    KvPair = 1,
}

/// AW-III.W1 — Literal-leaf payload classification + lifted constant.
///
/// The lifter resolves the enclosing `Map { Literal, MapExpr::IntLit }`
/// (Sheets `add_op = "+" -> 0u8`, CSS `dirKeyword = "ltr" -> 0u8`,
/// CSS `percentageUnit = "%" -> 255u8`, JSON `null = "null" -> 0u8`,
/// etc.) into one of these variants. The walker writes the lifted
/// constant directly into the tape's arena at emit-leaf time — no
/// PSI round-trip is needed because the value is grammar-constant.
///
/// `None` is the absence sentinel — keeps the legacy structural-only
/// emission for grammar literals that carry no `->` annotation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralPayload {
    /// No `->` annotation — emit the legacy `TapeKind::Literal` row
    /// without payload bytes.
    None,
    /// `Map { Literal, IntLit(value) }` projecting to `u8`. The lifter
    /// truncates `IntLit(i64)` to `u8` during admission. Walker writes
    /// one byte at the arena offset; reader uses `payload_bytes(rec, 1)`.
    U8(u8),
    /// `Map { Literal, BoolLit(value) }`. Walker writes one byte
    /// (`0` / `1`). Reader uses `payload_bytes(rec, 1)`.
    Bool(bool),
    /// `Map { Literal, IntLit(value) }` projecting to `u32`. Walker
    /// writes 4 little-endian bytes. Reader uses
    /// `payload_bytes(rec, 4)` (or `payload_scalar::<u32>` over the
    /// arena view).
    U32(u32),
    /// `Map { Literal, IntLit(value) }` projecting to `i64`/`u64`.
    /// Walker writes 8 little-endian bytes. Reader uses
    /// `payload_bytes(rec, 8)`.
    U64(u64),
    /// `Map { Literal, FloatLit(value) }` projecting to `f64`. Walker
    /// writes 8 little-endian bytes from `value.to_bits()`. Reader
    /// uses `payload_bytes(rec, 8)`.
    F64(f64),
}

impl LiteralPayload {
    /// Number of bytes the walker writes into the arena for this
    /// variant. `0` for `None`.
    #[inline]
    pub const fn arena_byte_width(self) -> usize {
        match self {
            LiteralPayload::None => 0,
            LiteralPayload::U8(_) | LiteralPayload::Bool(_) => 1,
            LiteralPayload::U32(_) => 4,
            LiteralPayload::U64(_) | LiteralPayload::F64(_) => 8,
        }
    }

    /// Encode this payload's value as little-endian bytes into the
    /// caller-supplied 8-byte buffer; returns the number of bytes
    /// written. Used by the walker to stage the constant into the
    /// arena.
    #[inline]
    pub fn write_le(self, dst: &mut [u8; 8]) -> usize {
        match self {
            LiteralPayload::None => 0,
            LiteralPayload::U8(v) => {
                dst[0] = v;
                1
            }
            LiteralPayload::Bool(b) => {
                dst[0] = if b { 1 } else { 0 };
                1
            }
            LiteralPayload::U32(v) => {
                let bytes = v.to_le_bytes();
                dst[..4].copy_from_slice(&bytes);
                4
            }
            LiteralPayload::U64(v) => {
                let bytes = v.to_le_bytes();
                dst[..8].copy_from_slice(&bytes);
                8
            }
            LiteralPayload::F64(v) => {
                let bytes = v.to_bits().to_le_bytes();
                dst[..8].copy_from_slice(&bytes);
                8
            }
        }
    }

    /// `true` iff this variant carries a payload.
    #[inline]
    pub const fn is_some(self) -> bool {
        !matches!(self, LiteralPayload::None)
    }
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
    ///
    /// AW-III.W1: `payload` carries the typed-leaf classification
    /// (e.g. `Map { Literal "+", IntLit(0) }` lifts as `Literal {
    /// text: "+", payload: U8WithConst(0) }`). When `payload` is
    /// non-`None`, the walker's Literal arm writes the lifted constant
    /// bytes into the tape's arena (`pay_agg`) post-match and emits
    /// the leaf as `TapeKind::Span` with `child_off = arena offset`.
    /// `None` keeps the structural-only emission (`TapeKind::Literal`,
    /// no payload write).
    Literal {
        /// Byte sequence to match verbatim.
        text: &'static str,
        /// Per-state payload classification + lifted constant value.
        payload: LiteralPayload,
    },
    /// Regex pattern — routes through the existing regex subsystem.
    ///
    /// AW-III.W1: `payload` carries the runtime decoder selector. When
    /// non-`None`, the walker's Regex arm enqueues a `PayloadJob` into
    /// the PSI stream so the matched bytes decode into the arena at
    /// stage-B time (`PayloadKind::F64` for `number -> f64`,
    /// `HexU32` for hex-color regexes, etc.). `None` keeps the bare
    /// `TapeKind::Span` emission with no decoder dispatch.
    Regex {
        /// Pattern string (static interned).
        pattern: &'static str,
        /// Per-state decoder selector for the matched byte slice.
        /// `PayloadKind::None`-equivalent is the absence variant —
        /// represented here by an `Option` so the emitted const tables
        /// pay no enum-discriminant cost when the payload is absent.
        payload: Option<PayloadKind>,
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
        /// AW-III.W1.6 — Seq → KvPair promotion classification. The
        /// lifter sets `KvPair` when the enclosing rule's
        /// `payload_layouts` entry matches `Tuple([Span, scalar])`;
        /// the walker then emits a flat `TapeKind::KvPair` leaf
        /// instead of a Seq compound + children.
        promote: SeqPromote,
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
    /// AW-I.W4γ — Whitespace trim.
    ///
    /// Advances `pos` past the grammar's `@ws` pattern when set.
    /// Zero-byte trims are admitted (the `?` in `?w`). No column
    /// emission — whitespace is structural hole, not content.
    ///
    /// Lifted from `IrNode::OptionalWhitespace(inner)` as the tail
    /// element of a Seq `[inner_state, WsTrim]`. The prior lifter
    /// silently dropped the whitespace step and emitted the inner
    /// node alone, which caused every `?w`-bearing grammar to parse
    /// incorrectly at the whitespace boundary.
    ///
    /// The ws pattern is embedded on each state (rather than
    /// referenced from a shared scanner call) so the runtime
    /// dispatch does not depend on grammar-level state. An `Option`
    /// here admits grammars that never declared `@ws` — for those
    /// the walker simply treats the state as a no-op Epsilon.
    WsTrim {
        /// Whitespace regex pattern. `None` => act as Epsilon.
        pattern: Option<&'static str>,
    },
    /// AW-II.W5b — Set-difference state (IrNode::Minus semantic).
    ///
    /// Match `primary` only if `excluded` does NOT match at the same
    /// start offset. Mirrors the VM's `compile_minus` semantic:
    /// savepoint, probe excluded, restore on success → fail; on
    /// failure → run primary.
    ///
    /// The pre-W5b lifter silently discarded `excluded` and lowered
    /// `a - b` to `a` alone — every `character - "'"` site accepted
    /// the excluded byte, breaking EBNF terminal parsing (and every
    /// grammar that used `-` to carve a character class exclusion).
    Minus {
        /// The expression that must succeed and whose match is
        /// consumed by the driver.
        primary: DtaStateId,
        /// The expression that must NOT match at the same start
        /// offset. Probed with a savepoint; side effects are
        /// discarded before `primary` runs.
        excluded: DtaStateId,
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
    /// AW-I.W4γ — grammar entry rule.
    ///
    /// `dta_run` dispatches the entry state by looking up this
    /// rule id in `rule_entries`. The pre-W4γ driver read
    /// `rule_entries.first()` which returned the first-indexed
    /// rule — incorrect when the grammar entry isn't lowered as
    /// `RuleId(0)` (bbnf's entry is `grammar`, lifted last).
    pub entry: DtaRuleId,
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
        entry: DtaRuleId(0),
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
