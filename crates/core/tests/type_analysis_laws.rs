mod instance_support;

use bbnf::compiler::{NodeKind, SemanticType};
use bbnf::{CompileOptions, DiagnosticCode, StageResult};
use std::fmt::Write as _;

fn compile(source: &str, options: CompileOptions) -> StageResult<bbnf::CompiledModule> {
    instance_support::compile("type-analysis-laws.bbnf", source.as_bytes(), options)
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

fn rejected_diagnostic(source: &str, options: CompileOptions) -> bbnf::Diagnostic {
    match compile(source, options) {
        StageResult::Accepted { .. } => panic!("expected compilation to be rejected"),
        StageResult::Rejected { diagnostics } => {
            let mut diagnostics = diagnostics.iter();
            let first = diagnostics
                .next()
                .expect("rejection has a diagnostic")
                .clone();
            assert!(diagnostics.next().is_none());
            first
        }
    }
}

fn doubling_grammar(depth: usize, entry: &str) -> String {
    let mut source = String::from("r0 = \"x\";\n");
    for index in 1..=depth {
        writeln!(source, "r{index} = r{}, r{};", index - 1, index - 1)
            .expect("writing to String is infallible");
    }
    writeln!(source, "root = {entry};").expect("writing to String is infallible");
    source
}

fn reference_chain(depth: usize) -> String {
    let mut source = String::from("r0 = \"x\";\n");
    for index in 1..=depth {
        writeln!(source, "r{index} = r{};", index - 1).expect("writing to String is infallible");
    }
    writeln!(source, "root = r{depth};").expect("writing to String is infallible");
    source
}

fn assert_exact_analysis_boundary(label: &str, source: &str) {
    let baseline = accepted(source);
    let work = baseline.facts().work_units();
    assert!(work > 0, "{label}: analysis performs logical work");

    let mut exact_options = CompileOptions::default();
    exact_options.analysis_work = work;
    let exact = match compile(source, exact_options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            panic!("{label}: exact analysis-work boundary rejected: {diagnostics:?}")
        }
    };
    assert_eq!(
        exact.facts(),
        baseline.facts(),
        "{label}: exact cap changes sealed facts"
    );

    let mut exhausted_options = exact_options;
    exhausted_options.analysis_work = work - 1;
    assert_eq!(
        rejected_code(source, exhausted_options),
        DiagnosticCode::ResourceLimit,
        "{label}: one-under analysis-work boundary does not reject"
    );
}

fn assert_semantic_rejection_boundary(label: &str, source: &str, expected: DiagnosticCode) {
    assert_eq!(
        rejected_code(source, CompileOptions::default()),
        expected,
        "{label}: high-cap semantic rejection changed"
    );

    let mut low = 0_usize;
    let mut high = CompileOptions::default().analysis_work;
    while low < high {
        let middle = low + (high - low) / 2;
        let mut options = CompileOptions::default();
        options.analysis_work = middle;
        match compile(source, options) {
            StageResult::Accepted { .. } => {
                panic!("{label}: semantically invalid grammar was accepted")
            }
            StageResult::Rejected { diagnostics } => {
                let code = diagnostics
                    .iter()
                    .next()
                    .expect("rejection has a diagnostic")
                    .code();
                if code == DiagnosticCode::ResourceLimit {
                    low = middle + 1;
                } else {
                    assert_eq!(code, expected, "{label}: rejection phase changed");
                    high = middle;
                }
            }
        }
    }
    assert!(
        low > 0,
        "{label}: semantic rejection requires no analysis work"
    );
    let mut exact = CompileOptions::default();
    exact.analysis_work = low;
    assert_eq!(rejected_code(source, exact), expected);
    let mut one_under = CompileOptions::default();
    one_under.analysis_work = low - 1;
    assert_eq!(
        rejected_code(source, one_under),
        DiagnosticCode::ResourceLimit
    );
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

    for rule in module.program().rules() {
        assert!(matches!(
            module
                .types()
                .rule_type(rule.id())
                .and_then(|type_id| module.types().semantic_type(type_id)),
            Some(SemanticType::Named(named)) if named == rule.id()
        ));
        let derivation = module
            .types()
            .rule_derivations()
            .get(rule.id().index())
            .expect("rule derivation");
        assert_eq!(derivation.revision(), module.program().revision());
        assert_eq!(derivation.rule(), rule.id());
        assert_eq!(
            Some(derivation.root_type()),
            module.types().node_type(rule.root())
        );
    }

    let mut saw_unit = false;
    let mut saw_product = false;
    let mut saw_sum = false;
    let mut saw_option = false;
    let mut saw_list = false;
    for node in module.program().nodes() {
        let derivation = module
            .types()
            .node_derivations()
            .get(node.id().index())
            .expect("node derivation");
        assert_eq!(derivation.revision(), module.program().revision());
        assert_eq!(derivation.node(), node.id());
        let semantic = module
            .types()
            .semantic_type(derivation.type_id())
            .expect("derivation type belongs to this type carrier");
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
            (NodeKind::Transform { child, .. }, semantic) => {
                assert_eq!(
                    semantic,
                    module
                        .types()
                        .node_type(child)
                        .and_then(|type_id| module.types().semantic_type(type_id))
                        .expect("child type belongs to this type carrier")
                );
            }
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
            .program()
            .rules()
            .iter()
            .find(|rule| rule.name() == name)
            .expect("test rule exists")
            .id()
    };

    let root = module
        .facts()
        .rule(find_rule("root"))
        .expect("rule facts belong to this carrier");
    assert!(root.nullable());
    assert_eq!(root.minimum_consumption(), Some(0));

    let pair = module
        .facts()
        .rule(find_rule("pair"))
        .expect("rule facts belong to this carrier");
    assert!(!pair.nullable());
    assert_eq!(pair.minimum_consumption(), Some(2));

    let maybe = module
        .facts()
        .rule(find_rule("maybe"))
        .expect("rule facts belong to this carrier");
    assert!(maybe.nullable());
    assert_eq!(maybe.minimum_consumption(), Some(0));

    let many = module
        .facts()
        .rule(find_rule("many"))
        .expect("rule facts belong to this carrier");
    assert!(!many.nullable());
    assert_eq!(many.minimum_consumption(), Some(1));

    for fact in module.facts().nodes() {
        assert_eq!(fact.revision(), module.program().revision());
    }
    for fact in module.facts().rules() {
        assert_eq!(fact.revision(), module.program().revision());
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
fn bounded_nullable_repetition_is_rejected_to_match_the_runtime_progress_law() {
    assert_eq!(
        rejected_code("root = epsilon?;", CompileOptions::default()),
        DiagnosticCode::NullableRepeat
    );
}

#[test]
fn json_like_recursive_sum_uses_finite_named_binders_and_converges() {
    let module = accepted(
        r#"
root = value;
value = object | array | string;
object = "{", (string, ":", value, (",", string, ":", value)*)?, "}";
array = "[", (value, (",", value)*)?, "]";
string = /[a-z]+/;
"#,
    );
    let find_rule = |name: &str| {
        module
            .program()
            .rules()
            .iter()
            .find(|rule| rule.name() == name)
            .expect("test rule exists")
            .id()
    };
    let value = find_rule("value");
    let object = find_rule("object");
    let array = find_rule("array");

    assert_eq!(
        module
            .facts()
            .rule(value)
            .expect("value facts")
            .minimum_consumption(),
        Some(1)
    );
    assert_eq!(
        module
            .facts()
            .rule(object)
            .expect("object facts")
            .minimum_consumption(),
        Some(2)
    );
    assert_eq!(
        module
            .facts()
            .rule(array)
            .expect("array facts")
            .minimum_consumption(),
        Some(2)
    );
    assert_eq!(
        module.dependency().component_of(value),
        module.dependency().component_of(object)
    );
    assert_eq!(
        module.dependency().component_of(value),
        module.dependency().component_of(array)
    );

    let named_references = module
        .program()
        .nodes()
        .iter()
        .filter_map(|node| match node.kind() {
            NodeKind::RuleRef(rule) => Some((node.id(), rule)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(!named_references.is_empty());
    for (node, rule) in named_references {
        assert!(matches!(
            module
                .types()
                .node_type(node)
                .and_then(|type_id| module.types().semantic_type(type_id)),
            Some(SemanticType::Named(bound)) if bound == rule
        ));
    }
    assert!(
        module.types().arena().len()
            <= module.program().nodes().len() + module.program().rules().len(),
        "recursive rules are represented by finite named binders, never unfolded"
    );
}

#[test]
fn consuming_self_and_mutual_recursion_have_exact_reachability_facts() {
    let module = accepted(
        r#"
root = "a", other | "z";
other = "b", root | "q";
"#,
    );
    for rule in module.program().rules() {
        let facts = module
            .facts()
            .rule(rule.id())
            .expect("rule facts belong to this carrier");
        assert!(!facts.nullable());
        assert_eq!(facts.minimum_consumption(), Some(1));
    }
}

#[test]
fn direct_and_mutual_zero_consumption_cycles_are_typed_rejections() {
    assert_eq!(
        rejected_code(r#"root = root | "x";"#, CompileOptions::default()),
        DiagnosticCode::RecursiveRule
    );
    assert_eq!(
        rejected_code(
            r#"
root = other;
other = root | "x";
"#,
            CompileOptions::default()
        ),
        DiagnosticCode::RecursiveRule
    );
}

#[test]
fn consuming_but_nonproductive_recursion_is_typed_rejection() {
    assert_eq!(
        rejected_code(r#"root = "x", root;"#, CompileOptions::default()),
        DiagnosticCode::RecursiveRule
    );
}

#[test]
fn nullable_prefix_recursion_is_rejected_before_generated_code() {
    assert_eq!(
        rejected_code(
            r#"root = (epsilon, root) | "x";"#,
            CompileOptions::default()
        ),
        DiagnosticCode::RecursiveRule
    );
}

#[test]
fn bounded_analysis_exhaustion_is_a_typed_rejection() {
    let source = "root = \"x\" | \"a\", root;";
    let mut options = CompileOptions::default();
    options.analysis_iterations = 1;
    assert_eq!(
        rejected_code(source, options),
        DiagnosticCode::ResourceLimit
    );

    options.analysis_iterations = 2;
    let module = match compile(source, options) {
        StageResult::Accepted { value, .. } => value,
        StageResult::Rejected { diagnostics } => {
            panic!("two exact recursive SCC passes must converge: {diagnostics:?}")
        }
    };
    assert_eq!(module.facts().iterations(), 2);
}

#[test]
fn minimum_consumption_overflow_is_a_typed_rejection_not_empty_language() {
    let source = doubling_grammar(65, "r65");
    let diagnostic = rejected_diagnostic(&source, CompileOptions::default());
    assert_eq!(
        diagnostic.code(),
        DiagnosticCode::MinimumConsumptionOverflow
    );
    assert_eq!(&source[diagnostic.start()..diagnostic.end()], "r63, r63");
}

#[test]
fn overflowing_descendant_rejects_even_when_entry_has_a_smaller_alternative() {
    let source = doubling_grammar(65, "r65 | \"z\"");
    assert_eq!(
        rejected_code(&source, CompileOptions::default()),
        DiagnosticCode::MinimumConsumptionOverflow
    );
}

#[test]
fn record_minimum_addition_overflow_is_a_typed_rejection() {
    let source = doubling_grammar(63, "%{ left: r63, right: r63 }");
    assert_eq!(
        rejected_code(&source, CompileOptions::default()),
        DiagnosticCode::MinimumConsumptionOverflow
    );
}

#[test]
fn dependency_first_acyclic_analysis_is_linear_and_exactly_budgeted() {
    for depth in [8, 64, 256] {
        let source = reference_chain(depth);
        let module = accepted(&source);
        let rules = module.program().rules().len();
        assert_eq!(module.facts().iterations(), rules);
        assert!(
            module.facts().work_units() <= rules * 128,
            "the conservative acyclic reference-chain bound remains linear"
        );
    }

    let source = reference_chain(64);
    assert_exact_analysis_boundary("acyclic-reference-chain", &source);
}

#[test]
fn analysis_work_exact_boundary_covers_distinct_topologies() {
    for (label, source) in [
        ("terminal", r#"root = "x";"#),
        (
            "wide-sequence-choice",
            r#"root = "a", ("b" | "c" | "d" | epsilon), "e", "f";"#,
        ),
        ("regex", r#"root = /[A-Za-z_][A-Za-z0-9_]*/;"#),
        (
            "shared-dag",
            r#"
root = left | right;
left = shared, "l";
right = shared, "r";
shared = "s" | "t";
"#,
        ),
        ("recursive-scc", r#"root = "(", root?, ")" | "x";"#),
        (
            "record",
            r#"root = %{ ~"{", left: /[a-z]+/, ~":", right: ("x" | "y"), ~"}" };"#,
        ),
        (
            "projection-difference",
            r#"
root = projected | difference;
projected = left >> right;
difference = left - right;
left = "a" | "b";
right = "b";
"#,
        ),
    ] {
        assert_exact_analysis_boundary(label, source);
    }
}

#[test]
fn analysis_work_one_under_precedes_semantic_diagnostic_aggregation() {
    for (label, source, code) in [
        (
            "nullable-repeat",
            r#"root = epsilon*;"#,
            DiagnosticCode::NullableRepeat,
        ),
        (
            "zero-consumption-cycle",
            r#"root = root | "x";"#,
            DiagnosticCode::RecursiveRule,
        ),
        (
            "nonproductive-recursion",
            r#"root = "x", root;"#,
            DiagnosticCode::RecursiveRule,
        ),
    ] {
        assert_semantic_rejection_boundary(label, source, code);
    }
}
