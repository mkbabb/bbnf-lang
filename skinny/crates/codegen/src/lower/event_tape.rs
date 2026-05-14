use ir::BackendRule;

pub fn lower_rule(rule: &BackendRule) -> String {
    format!("rule {} -> event_tape", rule.name)
}
