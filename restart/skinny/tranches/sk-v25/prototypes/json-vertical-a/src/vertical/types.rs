use serde::{Deserialize, Serialize};

use super::{VerticalError, budget::Budget, graph::DependencyGraph, normalize::NormalizedProgram};

pub mod row;

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Type {
    Unit,
    Bool,
    F64,
    String,
    Json,
    Pair,
    List(Box<Type>),
    ExcludedPunctuation,
}

#[derive(Clone, Debug)]
pub struct TypeSolution;

impl TypeSolution {
    pub fn solve(
        program: &NormalizedProgram,
        graph: &DependencyGraph,
        _budget: Budget,
    ) -> Result<Self, VerticalError> {
        if graph.program_revision() != program.program().revision() {
            return Err(VerticalError::Identity { stage: "types" });
        }
        Err(VerticalError::Incomplete(
            "type solution requires types inferred solely from complete BBNF module semantics",
        ))
    }
}
