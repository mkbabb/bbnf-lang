use bbnf::compiler::{NodeKind, SemanticType};
use bbnf::{CompileOptions, DiagnosticCode, ModuleInput, StageResult, compile_module};

fn compile(source: &str, options: CompileOptions) -> StageResult<bbnf::CompiledModule> {
    compile_module(
        ModuleInput::new("type-analysis-laws.bbnf", source.as_bytes()),
        options,
    )
}

fn accepted(source: &str) -> bbnf::CompiledModule {
    match compile(source, CompileOptions::default()) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("expected compilation to succeed:\n{rendered}");
        }
    }
}

fn rejected_code(source: &str, options: CompileOptions) -> DiagnosticCode {
    match compile(source, options) {
        StageResult::Accepted { .. } => {
            panic!("expected compilation to be rejected")
        }
        StageResult::Rejected { diagnostics } => {
            let mut diagnostics = diagnostics.iter();
            let first = diagnostics.next().expect("rejection has a diagnostic");
            assert!(diagnostics.next().is_none());
            first.code()
        }
    }
}

#[test]
fn product_sum_option_list_and_named_witnesses_are_derived_once() {
    let module = accepted(
        r#"
root = pair | maybe | many | empty;
pair = "a", /[0-9]+/;
maybe = "x"?;
many = "y"+;
empty = epsilon;
"#,
    );

    for rule in module.program.rules() {
        assert!(matches!(
            module.types.semantic_type(module.types.rule_type(rule.id())),
            SemanticType::Named(named) if *named == rule.id()
        ));
        let derivation = &module.types.rule_derivations()[rule.id().index()];
        assert_eq!(derivation.revision(), module.program.revision());
        assert_eq!(derivation.rule(), rule.id());
        assert_eq!(derivation.root_type(), module.types.node_type(rule.root()));
    }

    let mut saw_unit = false;
    let mut saw_product = false;
    let mut saw_sum = false;
    let mut saw_option = false;
    let mut saw_list = false;
    for node in module.program.nodes() {
        let derivation = &module.types.node_derivations()[node.id().index()];
        assert_eq!(derivation.revision(), module.program.revision());
        assert_eq!(derivation.node(), node.id());
        let semantic = module.types.semantic_type(derivation.type_id());
        match (node.kind(), semantic) {
            (NodeKind::Epsilon, SemanticType::Unit) => saw_unit = true,
            (NodeKind::Sequence(children), SemanticType::Product(members)) => {
                assert_eq!(members.len(), children.len());
                saw_product = true;
            }
            (NodeKind::Choice(children), SemanticType::Sum(members)) => {
                assert_eq!(members.len(), children.len());
                saw_sum = true;
            }
            (
                NodeKind::Repeat {
                    minimum: 0,
                    maximum: Some(1),
                    ..
                },
                SemanticType::Option(_),
            ) => saw_option = true,
            (NodeKind::Repeat { .. }, SemanticType::List(_)) => saw_list = true,
            (NodeKind::Literal(_) | NodeKind::Regex(_), SemanticType::Span) => {}
            (NodeKind::RuleRef(rule), SemanticType::Named(named)) => {
                assert_eq!(named, rule);
            }
            pair => panic!("node/type witness mismatch: {pair:?}"),
        }
    }
    assert!(saw_unit && saw_product && saw_sum && saw_option && saw_list);
}

#[test]
fn nullable_and_minimum_consumption_are_exact_across_references() {
    let module = accepted(
        r#"
root = pair | maybe | many | empty;
pair = "a", /[0-9]+/;
maybe = "x"?;
many = "y"+;
empty = epsilon;
"#,
    );
    let find_rule = |name: &str| {
        module
            .program
            .rules()
            .iter()
            .find(|rule| rule.name() == name)
            .expect("test rule exists")
            .id()
    };

    let root = module.facts.rule(find_rule("root"));
    assert!(root.nullable());
    assert_eq!(root.minimum_consumption(), Some(0));

    let pair = module.facts.rule(find_rule("pair"));
    assert!(!pair.nullable());
    assert_eq!(pair.minimum_consumption(), Some(2));

    let maybe = module.facts.rule(find_rule("maybe"));
    assert!(maybe.nullable());
    assert_eq!(maybe.minimum_consumption(), Some(0));

    let many = module.facts.rule(find_rule("many"));
    assert!(!many.nullable());
    assert_eq!(many.minimum_consumption(), Some(1));

    for fact in module.facts.nodes() {
        assert_eq!(fact.revision(), module.program.revision());
    }
    for fact in module.facts.rules() {
        assert_eq!(fact.revision(), module.program.revision());
    }
}

#[test]
fn repetition_of_nullable_child_is_rejected() {
    assert_eq!(
        rejected_code("root = epsilon*;", CompileOptions::default()),
        DiagnosticCode::NullableRepeat
    );
}

#[test]
fn bounded_analysis_exhaustion_is_a_typed_rejection() {
    let options = CompileOptions {
        analysis_iterations: 1,
        ..CompileOptions::default()
    };
    assert_eq!(
        rejected_code("root = \"x\";", options),
        DiagnosticCode::ResourceLimit
    );
}
