use ir::BackendRule;

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> eager_tape", rule.name)
}
