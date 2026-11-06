#[test]
fn program_keeps_all_rules_and_explicit_entry() {
    let (_, program, ..) = super::stages();
    let names = program
        .rules()
        .iter()
        .map(|rule| rule.name())
        .collect::<Vec<_>>();
    assert_eq!(
        names,
        [
            "null", "bool", "number", "comma", "colon", "string", "array", "pair", "object",
            "value"
        ]
    );
    assert_eq!(
        program.rule(program.entry()).expect("entry").name(),
        "value"
    );
}
