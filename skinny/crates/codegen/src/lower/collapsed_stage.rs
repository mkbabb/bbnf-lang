use ir::BackendRule;

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> collapsed_stage", rule.name)
}
