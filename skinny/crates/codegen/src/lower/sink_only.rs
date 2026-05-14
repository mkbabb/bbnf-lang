use ir::{BackendExpr, BackendIr, BackendRule};

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> sink_only", rule.name)
}

pub fn direct_builds_all(backend: &BackendIr, required_shapes: &[&str]) -> bool {
    required_shapes.iter().all(|shape| {
        backend
            .rules
            .iter()
            .any(|rule| direct_builds(&rule.expr, shape))
    })
}

fn direct_builds(expr: &BackendExpr, shape: &str) -> bool {
    match expr {
        BackendExpr::DirectBuild { shape: found } => found == shape,
        BackendExpr::Entry(inner)
        | BackendExpr::OptionalBranch(inner)
        | BackendExpr::RepeatLoop { body: inner, .. } => direct_builds(inner, shape),
        BackendExpr::Seq(children)
        | BackendExpr::Alt {
            branches: children, ..
        } => children.iter().any(|child| direct_builds(child, shape)),
        BackendExpr::ByteLiteral(_)
        | BackendExpr::RegexProgram { .. }
        | BackendExpr::CallRule { .. }
        | BackendExpr::SpanMark { .. }
        | BackendExpr::TapeEmit { .. }
        | BackendExpr::ValueProject { .. }
        | BackendExpr::Return => false,
    }
}
