use bbnf_regex::TablePlanOptions;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[non_exhaustive]
pub struct CompileOptions {
    pub instance_bytes: usize,
    pub source_bytes: usize,
    pub rules: usize,
    pub nodes: usize,
    pub syntax_nesting: usize,
    pub pratt_tables: usize,
    pub pratt_operators: usize,
    pub presentation_hints: usize,
    pub analysis_iterations: usize,
    pub analysis_work: usize,
    pub generated_bytes: usize,
    pub regex: RegexBudget,
    pub runtime: RuntimeBudget,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            instance_bytes: 1024 * 1024,
            source_bytes: 1024 * 1024,
            rules: 4_096,
            nodes: 65_536,
            syntax_nesting: 256,
            pratt_tables: 4_096,
            pratt_operators: 65_536,
            presentation_hints: 65_536,
            analysis_iterations: 65_536,
            analysis_work: 1_048_576,
            generated_bytes: 16 * 1024 * 1024,
            regex: RegexBudget::default(),
            runtime: RuntimeBudget::default(),
        }
    }
}

/// Default resource envelope baked into one generated parser.
///
/// These limits govern the emitted runtime only. They are deliberately
/// independent of the compiler's source, syntax, analysis, and generated-code
/// budgets above so relaxing a build-time bound cannot silently relax an
/// untrusted-input boundary.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[non_exhaustive]
pub struct RuntimeBudget {
    pub input_bytes: usize,
    pub nesting: usize,
    pub logical_events: usize,
    pub work: usize,
}

impl Default for RuntimeBudget {
    fn default() -> Self {
        Self {
            input_bytes: 1024 * 1024,
            nesting: 256,
            logical_events: 65_536,
            work: 1_048_576,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[non_exhaustive]
pub struct RegexBudget {
    pub nesting: usize,
    pub nfa_states: usize,
    pub dfa_states: usize,
}

impl Default for RegexBudget {
    fn default() -> Self {
        Self {
            nesting: 256,
            nfa_states: 65_536,
            dfa_states: 4_096,
        }
    }
}

impl RegexBudget {
    pub(crate) const fn table_options(self) -> TablePlanOptions {
        TablePlanOptions {
            nesting_limit: self.nesting,
            nfa_state_limit: self.nfa_states,
            state_limit: self.dfa_states,
            minimize: true,
        }
    }
}
