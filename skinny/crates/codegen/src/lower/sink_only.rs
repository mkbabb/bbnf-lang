use ir::{BackendExpr, BackendIr, BackendRule};

const JSON_SINK_SHAPES: [&str; 7] = [
    "JsonObject",
    "JsonArray",
    "JsonPair",
    "JsonString",
    "JsonNumber",
    "JsonBool",
    "JsonNull",
];

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> sink_only", rule.name)
}

pub fn lower_json_direct_sink(backend: &BackendIr) -> Option<String> {
    if JSON_SINK_SHAPES.iter().all(|shape| {
        backend
            .rules
            .iter()
            .any(|rule| direct_builds(&rule.expr, shape))
    }) {
        Some(include_str!("../json_templates/sink_direct.rs").to_string())
    } else {
        None
    }
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
