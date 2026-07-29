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

    assert_eq!(first.generated.source(), second.generated.source());
    assert_eq!(first.generated.manifest(), second.generated.manifest());
    assert_eq!(
        first.lowering.replay_json().expect("serialize lowering"),
        second.lowering.replay_json().expect("serialize lowering")
    );
    assert_eq!(
        first.events.replay_json().expect("serialize events"),
        second.events.replay_json().expect("serialize events")
    );
}

#[test]
fn emitted_rust_is_direct_static_control() {
    let compilation = compile();
    let source = compilation.generated.source();

    for required in [
        "use parse_that::{choice, eof, literal, regex, repeat, sequence,",
        "RegexPlan::new",
        "fn node_",
        "fn rule_",
        "parse_module",
        "pub fn check",
        "pub fn parse_events",
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
        compilation.lowering.nodes().len()
    );
    assert_eq!(
        source.matches("fn rule_").count(),
        compilation.lowering.rules().len()
    );
}

#[test]
fn replay_envelopes_name_schema_and_revision() {
    let compilation = compile();
    let revision = compilation.lowering.revision().hex();
    let lowering = String::from_utf8(
        compilation
            .lowering
            .replay_json()
            .expect("serialize lowering"),
    )
    .expect("JSON is UTF-8");
    let events = String::from_utf8(compilation.events.replay_json().expect("serialize events"))
        .expect("JSON is UTF-8");

    assert!(lowering.starts_with("{\"schema\":\"bbnf.lowering-plan.v1\""));
    assert!(events.starts_with("{\"schema\":\"bbnf.scalar-events.v1\""));
    assert!(lowering.contains(&revision));
    assert!(events.contains(&revision));
}
