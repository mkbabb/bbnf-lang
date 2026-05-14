use ir::BackendRule;

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> sink_only", rule.name)
}
