//! Execution plans for recognizers.
//!
//! A `RecognizerInfo` describes *what* a recognizer matches. An
//! `ExecutionPlan` captures *how* the backend should drive it — inline
//! byte operations, a DFA lookup, a memchr scan, etc. Plans are derived
//! from facts + backend cost model.

/// Which execution shape to use for a recognizer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExecutionPlanKind {
    /// Direct byte comparison (literal prefix memcmp).
    Memcmp,
    /// Inline byte operations walked from the HIR.
    HirInline,
    /// Compiled DFA table (with state count).
    DfaTable,
    /// memchr scan driven by `accel_candidate`.
    MemchrScan,
    /// Generic fallback — state machine without acceleration.
    Generic,
}

/// Resolved execution plan for a specific recognizer.
#[derive(Clone, Debug)]
pub struct ExecutionPlan {
    pub kind: ExecutionPlanKind,
    /// For DFA plans: pre-computed state count.
    pub dfa_states: Option<usize>,
    /// For memchr plans: the acceleration byte.
    pub accel_byte: Option<u8>,
}
