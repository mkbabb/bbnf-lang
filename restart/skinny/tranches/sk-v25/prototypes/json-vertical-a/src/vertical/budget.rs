use super::VerticalError;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Budget {
    pub source_bytes: u64,
    pub rules: u64,
    pub nodes: u64,
    pub graph_edges: u64,
    pub solver_operations: u64,
    pub plan_decisions: u64,
    pub code_instructions: u64,
    pub input_bytes: u64,
    pub nesting: u64,
    pub runtime_operations: u64,
    pub value_nodes: u64,
    pub artifact_bytes: u64,
}

impl Default for Budget {
    fn default() -> Self {
        Self {
            source_bytes: 65_536,
            rules: 256,
            nodes: 32_768,
            graph_edges: 131_072,
            solver_operations: 4_000_000,
            plan_decisions: 65_536,
            code_instructions: 262_144,
            input_bytes: 16 * 1024 * 1024,
            nesting: 512,
            runtime_operations: u64::MAX,
            value_nodes: u64::MAX,
            artifact_bytes: 64 * 1024 * 1024,
        }
    }
}

impl Budget {
    pub(crate) fn require(
        limit: u64,
        used: usize,
        resource: &'static str,
    ) -> Result<(), VerticalError> {
        let used = u64::try_from(used).unwrap_or(u64::MAX);
        if used > limit {
            return Err(VerticalError::Budget {
                resource,
                used,
                limit,
            });
        }
        Ok(())
    }
}
