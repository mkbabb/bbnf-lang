use bbnf_regex::TablePlanOptions;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CompileOptions {
    pub source_bytes: usize,
    pub rules: usize,
    pub nodes: usize,
    pub syntax_nesting: usize,
    pub analysis_iterations: usize,
    pub generated_bytes: usize,
    pub regex: RegexBudget,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            source_bytes: 1024 * 1024,
            rules: 4_096,
            nodes: 65_536,
            syntax_nesting: 256,
            analysis_iterations: 65_536,
            generated_bytes: 16 * 1024 * 1024,
            regex: RegexBudget::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
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
