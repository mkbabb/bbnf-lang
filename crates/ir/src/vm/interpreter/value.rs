//! Public types produced and consumed by the interpreter:
//!
//! - [`Value`] — a parse tree node (Span, Tagged, Array, Nil).
//! - [`ValueSlice`] — an owned boxed slice of values.
//! - [`ParseDiagnostic`] — a single failure-context message.
//! - [`ParseResult`] — the run result wrapper.

use std::{fmt, ops::Deref};

use crate::RuleId;

/// An owned slice of values.
#[derive(Clone)]
pub struct ValueSlice(Box<[Value]>);

impl ValueSlice {
    #[inline(always)]
    pub fn from_vec(values: Vec<Value>) -> Self {
        Self(values.into_boxed_slice())
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self(Box::new([]))
    }
}

impl Deref for ValueSlice {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for ValueSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl PartialEq for ValueSlice {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl Eq for ValueSlice {}

/// A generic parse tree node produced by the interpreter.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// A matched span of input: (start, end) byte offsets.
    Span(u32, u32),
    /// A tagged node (rule match).
    Tagged {
        tag: RuleId,
        span: (u32, u32),
        children: ValueSlice,
    },
    /// An array of values (from repeat/collect).
    Array(ValueSlice),
    /// Nothing matched (Epsilon).
    Nil,
}

/// A diagnostic message from the interpreter (parse error with context).
#[derive(Clone, Debug)]
pub struct ParseDiagnostic {
    /// Byte offset where the error occurred.
    pub offset: u32,
    /// Rule that was being parsed when the error occurred.
    pub rule_name: Option<String>,
    /// Characters that were expected at this position (from FOLLOW set).
    pub expected: Vec<u8>,
}

/// The run result wrapper.
pub struct ParseResult {
    pub value: Option<Value>,
    pub offset: u32,
    pub success: bool,
    /// Diagnostics collected during parsing (populated when FOLLOW sets are available).
    pub diagnostics: Vec<ParseDiagnostic>,
}
