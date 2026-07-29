use bbnf::{CompileOptions, ModuleInput, StageResult, compile_module};

const PATH: &str = "contracts/w2-generated-control/grammar/control.bbnf";
const GRAMMAR: &[u8] =
    include_bytes!("../../../contracts/w2-generated-control/grammar/control.bbnf");

fn compile() -> bbnf::CompiledModule {
    match compile_module(ModuleInput::new(PATH, GRAMMAR), CompileOptions::default()) {
        StageResult::Accepted { value, diagnostics } => {
            assert!(diagnostics.is_empty());
            value
        }
        StageResult::Rejected { diagnostics } => {
            let rendered = diagnostics
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");
            panic!("contract grammar was rejected:\n{rendered}");
        }
    }
}

#[test]
fn emission_and_replay_are_byte_deterministic() {
    let first = compile();
    let second = compile();

    assert_eq!(first.generated().source(), second.generated().source());
    assert_eq!(first.generated().manifest(), second.generated().manifest());
    assert_eq!(
        first.lowering().replay_json().expect("serialize lowering"),
        second.lowering().replay_json().expect("serialize lowering")
    );
    assert_eq!(
        first.events().replay_json().expect("serialize events"),
        second.events().replay_json().expect("serialize events")
    );
}

#[test]
fn emitted_rust_is_direct_static_control() {
    let compilation = compile();
    let source = compilation.generated().source();

    for required in [
        "choice",
        "eof",
        "literal",
        "regex",
        "repeat",
        "sequence",
        "RegexPlan::new",
        "fn node_",
        "fn rule_",
        "parse_module",
        "pub fn check",
        "pub fn parse_events",
        "pub const TRANSFORMS",
    ] {
        assert!(source.contains(required), "missing `{required}`");
    }
    for forbidden in [
        "use parse_that::*",
        "dyn Parser",
        "enum RuntimeNode",
        "dispatch_node",
        "control.bbnf",
        "root = branch",
    ] {
        assert!(!source.contains(forbidden), "forbidden `{forbidden}`");
    }

    assert_eq!(
        source.matches("fn node_").count(),
        compilation.lowering().nodes().len()
    );
    assert_eq!(
        source.matches("fn rule_").count(),
        compilation.lowering().rules().len()
    );
}

#[test]
fn replay_envelopes_name_schema_and_revision() {
    let compilation = compile();
    let revision = compilation.lowering().revision().hex();
    let lowering = String::from_utf8(
        compilation
            .lowering()
            .replay_json()
            .expect("serialize lowering"),
    )
    .expect("JSON is UTF-8");
    let events = String::from_utf8(
        compilation
            .events()
            .replay_json()
            .expect("serialize events"),
    )
    .expect("JSON is UTF-8");

    assert!(lowering.starts_with("{\"schema\":\"bbnf.lowering-plan.v3\""));
    assert!(events.starts_with("{\"schema\":\"bbnf.scalar-events.v5\""));
    assert!(lowering.contains(&revision));
    assert!(events.contains(&revision));
    let event_packet: serde_json::Value =
        serde_json::from_str(&events).expect("event replay is JSON");
    assert!(
        event_packet["value"].get("lowering").is_none(),
        "event replay references lowering identities without duplicating its carrier"
    );
}

#[test]
fn wide_choice_hits_the_generated_source_budget_without_quadratic_growth() {
    let mut grammar = String::from("root = ");
    for index in 0..20_000 {
        if index != 0 {
            grammar.push_str(" | ");
        }
        grammar.push('"');
        grammar.push_str(&format!("branch-{index}"));
        grammar.push('"');
    }
    grammar.push_str(" ;");

    let mut options = CompileOptions::default();
    options.generated_bytes = 32 * 1024;
    let result = compile_module(
        ModuleInput::new("contracts/wide-choice.bbnf", grammar.as_bytes()),
        options,
    );
    let diagnostics = match result {
        StageResult::Rejected { diagnostics } => diagnostics,
        StageResult::Accepted { .. } => {
            panic!("adversarial wide choice unexpectedly fit its 32 KiB output budget")
        }
    };
    assert_eq!(
        diagnostics
            .iter()
            .next()
            .map(|diagnostic| diagnostic.code()),
        Some(bbnf::DiagnosticCode::GeneratedSourceTooLarge)
    );
}
