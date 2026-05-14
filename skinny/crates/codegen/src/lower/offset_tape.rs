use ir::BackendRule;

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> offset_tape", rule.name)
}
