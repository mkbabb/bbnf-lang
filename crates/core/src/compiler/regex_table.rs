//! Checked compile-time regex table plans over `parse-that-regex`.
//!
//! sk-v27 LW3.COMP-1b. The sk-v26 spine took this boundary from its own
//! `bbnf-regex::table_plan`. Under X-5 the engine is `parse-that-regex`, whose
//! determinizer is leftmost-first (LW3.RX-1a..c): the spine is born
//! leftmost-first. This module reads that engine's automaton into the
//! immutable, validated tables a compiler serializes or renders as
//! `parse_that::kernel::RegexPlan` statics. It contains no matching API:
//! execution belongs to the generated parser's kernel.
//!
//! The compile budget is the spine's `RegexBudget`, enforced on the engine:
//! group nesting while the pattern is parsed
//! (`parse_that_regex::parse_with_nesting_limit`), Thompson NFA states before
//! determinization (`Dfa::from_hir_with_nfa_limit`), and DFA states.

use std::fmt;

use parse_that_regex::{DEAD, Dfa, DfaError, DfaOptions, ParseError, ParseOptions};

/// Sentinel used for a missing transition.
pub(crate) const DEAD_STATE: u32 = DEAD;

/// Compilation controls for a table plan.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct TablePlanOptions {
    /// Maximum group nesting accepted while the pattern is parsed.
    pub(crate) nesting_limit: usize,
    /// Maximum number of Thompson NFA states given to the determinizer.
    pub(crate) nfa_state_limit: usize,
    /// Maximum number of DFA states.
    pub(crate) state_limit: usize,
    /// Whether the determinizer minimizes the DFA.
    pub(crate) minimize: bool,
}

/// Why a regex could not be lowered into an executable table.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub(crate) enum TablePlanCompileError {
    /// The regex text was not valid, or nested past the budget.
    Parse(ParseError),
    /// Raw non-ASCII pattern text is ambiguous at a byte-oriented table
    /// boundary; intended bytes or code points are spelled with escapes.
    NonAsciiPattern { offset: usize },
    /// Position assertions need look context, which a static byte table
    /// does not carry, so they are refused rather than erased.
    UnsupportedLook,
    /// The engine has no lowering for the pattern, or refused to build it.
    Unsupported(String),
    /// The Thompson NFA exceeded the configured state limit.
    NfaStateLimitExceeded { limit: usize },
    /// Determinization exceeded the configured state limit.
    StateLimitExceeded { limit: usize },
    /// The engine's automaton violated the table invariant.
    InvalidPlan(&'static str),
}

impl fmt::Display for TablePlanCompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(error) => error.fmt(f),
            Self::NonAsciiPattern { offset } => write!(
                f,
                "raw non-ASCII regex syntax at byte {offset}; use an explicit escape"
            ),
            Self::UnsupportedLook => write!(f, "a position assertion has no byte-table semantics"),
            Self::Unsupported(reason) => write!(f, "regex cannot be lowered to a DFA: {reason}"),
            Self::NfaStateLimitExceeded { limit } => {
                write!(f, "regex NFA exceeded the {limit}-state compilation limit")
            }
            Self::StateLimitExceeded { limit } => {
                write!(f, "regex DFA exceeded the {limit}-state compilation limit")
            }
            Self::InvalidPlan(reason) => {
                write!(f, "the regex engine produced an invalid table: {reason}")
            }
        }
    }
}

impl std::error::Error for TablePlanCompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Parse(error) => Some(error),
            _ => None,
        }
    }
}

/// Owned, validated DFA tables for compiler serialization and source emission.
///
/// Fields are private so every instance is born checked.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RegexTablePlan {
    byte_classes: [u8; 256],
    transitions: Vec<u32>,
    accepting: Vec<bool>,
    state_count: u32,
    class_count: u16,
    start_state: u32,
}

impl RegexTablePlan {
    /// Parse and compile a byte-oriented regex under the given budget.
    pub(crate) fn compile_with(
        pattern: &str,
        options: TablePlanOptions,
    ) -> Result<Self, TablePlanCompileError> {
        if let Some(offset) = pattern.bytes().position(|byte| !byte.is_ascii()) {
            return Err(TablePlanCompileError::NonAsciiPattern { offset });
        }
        // Runtime cursors consume bytes, so the table boundary has one
        // syntax policy: byte mode.
        let syntax = ParseOptions { unicode: false };
        let hir =
            parse_that_regex::parse_with_nesting_limit(pattern, &syntax, options.nesting_limit)
                .map_err(TablePlanCompileError::Parse)?;
        let engine = DfaOptions {
            state_limit: options.state_limit,
            minimize: options.minimize,
            unicode: false,
        };
        let dfa = Dfa::from_hir_with_nfa_limit(&hir, &engine, options.nfa_state_limit).map_err(
            |error| match error {
                DfaError::NfaStateLimit(_) => TablePlanCompileError::NfaStateLimitExceeded {
                    limit: options.nfa_state_limit,
                },
                DfaError::StateLimit(_) => TablePlanCompileError::StateLimitExceeded {
                    limit: options.state_limit,
                },
                DfaError::Lower(lower) => TablePlanCompileError::Unsupported(format!("{lower:?}")),
                DfaError::Build(reason) | DfaError::Parse(reason) => {
                    TablePlanCompileError::Unsupported(reason)
                }
            },
        )?;
        Self::from_dfa(dfa)
    }

    /// Read a look-free automaton into checked tables.
    fn from_dfa(dfa: Dfa) -> Result<Self, TablePlanCompileError> {
        if dfa.has_look {
            return Err(TablePlanCompileError::UnsupportedLook);
        }
        let class_count = dfa.num_classes;
        let state_count = dfa.states.len();
        if class_count == 0 || usize::from(class_count) > 256 {
            return Err(TablePlanCompileError::InvalidPlan("class count"));
        }
        let Ok(states) = u32::try_from(state_count) else {
            return Err(TablePlanCompileError::InvalidPlan("state count"));
        };
        if states == 0 || states == DEAD_STATE {
            return Err(TablePlanCompileError::InvalidPlan("state count"));
        }
        let start_state = dfa.starts[0];
        if start_state != 0 {
            return Err(TablePlanCompileError::InvalidPlan("start state"));
        }
        if state_count.checked_mul(usize::from(class_count)) != Some(dfa.flat_transitions.len()) {
            return Err(TablePlanCompileError::InvalidPlan("transition shape"));
        }
        if dfa.byte_classes.iter().any(|&class| u16::from(class) >= class_count) {
            return Err(TablePlanCompileError::InvalidPlan("byte class"));
        }
        if dfa
            .flat_transitions
            .iter()
            .any(|&target| target != DEAD_STATE && target >= states)
        {
            return Err(TablePlanCompileError::InvalidPlan("transition target"));
        }
        let accepting = dfa.states.iter().map(|state| state.is_accept).collect();
        Ok(Self {
            byte_classes: dfa.byte_classes,
            transitions: dfa.flat_transitions,
            accepting,
            state_count: states,
            class_count,
            start_state,
        })
    }

    pub(crate) const fn byte_classes(&self) -> &[u8; 256] {
        &self.byte_classes
    }

    pub(crate) fn transitions(&self) -> &[u32] {
        &self.transitions
    }

    pub(crate) fn accepting(&self) -> &[bool] {
        &self.accepting
    }

    pub(crate) const fn state_count(&self) -> u32 {
        self.state_count
    }

    pub(crate) const fn class_count(&self) -> u16 {
        self.class_count
    }

    pub(crate) const fn start_state(&self) -> u32 {
        self.start_state
    }
}
