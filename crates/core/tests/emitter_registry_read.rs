//! AZ-I.W1.B4 — wire-contract test: emitter consumes `StructRegistry`
//! on every compound-emission boundary.
//!
//! `emit_shapes_for_grammar` lives at the per-rule shape-dispatch
//! boundary inside the Rust backend. Each shape-classified
//! non-transparent rule corresponds to one compound-emission boundary
//! (the per-shape emitter writes a `parse_<shape>_<grammar>_<rule>`
//! function whose body opens and closes a compound record). At every
//! such boundary the emitter consults [`bbnf_ir::StructRegistry`] for
//! the rule's projected layout and records the read into a thread-
//! local diagnostic buffer (see
//! [`bbnf::backend::rust::emitter::shapes::registry_observer`]).
//!
//! This test confirms the consumer fires end-to-end:
//!
//! 1. JSON grammar source compiles through the full pipeline
//!    (`prepare_grammar` runs, which runs `project_types`, which
//!    populates `ir.struct_registry`).
//! 2. `generate_all` runs codegen against the prepared IR, traversing
//!    every shape-classified rule and triggering the registry-read.
//! 3. The drained log carries at least one event whose `had_layout`
//!    flag is `true` for a JSON Named rule — confirming the registry
//!    read fires AND lands a populated layout for at least one rule
//!    in production code.
//!
//! Per `feedback_substrate-with-consumer`: substrate (`StructRegistry`)
//! and consumer (the `emit_shapes_for_grammar` registry-read) are
//! exercised in one test; the gate fails if either is silent.

use bbnf::ParserAttributes;
use bbnf::backend::PreparedGrammar;
use bbnf::backend::rust::emitter::shapes::{
    clear_registry_read_log, drain_registry_read_log, RegistryReadEvent,
};
use bbnf::generate::generate_all;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

/// AZ-I.W1.B4 fixture grammar — the production JSON grammar source.
///
/// Inlined here so the test does not depend on filesystem layout. Mirrors
/// `grammar/json/json.bbnf`. Updates to the on-disk grammar do not need
/// to keep this in sync — the test only requires a JSON-shaped grammar
/// whose pipeline produces non-transparent shape-classified rules with
/// registered `StructLayout` entries.
const JSON_GRAMMAR: &str = r#"
null = "null" -> 0u8 ;
bool = "true" -> true | "false" -> false ;

number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ -> f64 ;

comma = "," ?w ;
colon = ":" ?w ;

string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/
       -> decode_json_string_to_arena(input) : String ;
array = "[" >> (( value << comma ? ) *)?w << "]" ;

pair = string, colon >> value ;
object = "{" >> (( pair << comma ? ) *)?w << "}" ;

value = object | array | string | number | bool | null ;
"#;

/// Compile the inline JSON grammar through the Rust pipeline; returns the
/// `PreparedGrammar` whose `ir.struct_registry` is populated by
/// `project_types`.
fn prepare_json_grammar() -> PreparedGrammar {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust {
            requested_prettify: false,
        },
    };
    match compile_grammar_request(JSON_GRAMMAR, &request)
        .expect("AZ-I.W1.B4: JSON grammar must compile")
    {
        CompileOutput::Rust(prepared) => prepared,
        other => panic!(
            "AZ-I.W1.B4: expected Rust output for JSON grammar, got {:?}",
            std::mem::discriminant(&other)
        ),
    }
}

/// AZ-I.W1.B4 hard gate.
///
/// Drives the full Rust codegen path (`generate_all`) over the JSON
/// grammar's prepared IR. The shape-emission pass (`emit_shapes_for_grammar`)
/// reads `ir.struct_registry.layout(rule.id)` for every shape-
/// classified non-transparent rule. The registry-observer captures the
/// reads; this test asserts the observer recorded at least one
/// populated `StructLayout` for a JSON Named rule.
///
/// Per `feedback_no-orthogonal-codepaths`: the read is unconditional —
/// it fires for every classified rule regardless of layout presence.
/// Per `feedback_substrate-with-consumer`: substrate (`StructRegistry`)
/// + consumer (`emit_shapes_for_grammar` registry-read) land in one
/// commit with a same-commit wire-contract test.
#[test]
fn emit_shapes_for_grammar_reads_struct_registry_on_json() {
    let prepared = prepare_json_grammar();

    // Sanity floor: the pipeline must have populated the registry. If
    // this fails, `project_types` did not run or the registry-population
    // phase regressed — the wire-contract test cannot observe a
    // consumer fire when the substrate itself is empty.
    assert!(
        !prepared.ir.struct_registry.is_empty(),
        "AZ-I.W1.B4 substrate-floor: ir.struct_registry must be populated \
         by project_types on the Rust path; len = 0 indicates the registry \
         population phase regressed"
    );

    let registered_count = prepared.ir.struct_registry.len();

    // Clear any reads recorded by upstream pipeline stages — this test
    // observes the codegen invocation only.
    clear_registry_read_log();

    let attrs = ParserAttributes::default();
    let ident = quote::format_ident!("JsonRegistryReadProbe");
    let _tokens = generate_all(&prepared, &attrs, &ident);

    let events: Vec<RegistryReadEvent> = drain_registry_read_log();

    assert!(
        !events.is_empty(),
        "AZ-I.W1.B4 hard gate: emit_shapes_for_grammar must fire at \
         least one registry-read on the JSON pipeline; observed 0 events \
         despite {registered_count} registered layouts"
    );

    let populated: Vec<&RegistryReadEvent> =
        events.iter().filter(|e| e.had_layout).collect();
    assert!(
        !populated.is_empty(),
        "AZ-I.W1.B4 hard gate: emit_shapes_for_grammar must observe at \
         least one populated StructLayout for a JSON Named rule; got \
         {} events, none with had_layout = true (registered_count = {})",
        events.len(),
        registered_count
    );

    // Cross-check: every event's rule_id must reference a real rule on
    // the IR — guards against off-by-one drift between the rule loop's
    // index space and the registry's keyspace.
    for ev in &events {
        let rule_exists = prepared
            .ir
            .rules
            .iter()
            .any(|r| r.id == ev.rule_id);
        assert!(
            rule_exists,
            "AZ-I.W1.B4: registry-read event references unknown rule_id {}",
            ev.rule_id
        );
    }
}

/// Companion-gate: every classified non-transparent rule that the
/// shape dispatcher emits a per-shape function for produces exactly one
/// registry-read event. Mirrors the shape-dispatcher's per-rule loop.
#[test]
fn registry_read_events_match_shape_classified_rule_count() {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let prepared = prepare_json_grammar();

    clear_registry_read_log();
    let attrs = ParserAttributes::default();
    let ident = quote::format_ident!("JsonRegistryCountProbe");
    let _tokens = generate_all(&prepared, &attrs, &ident);
    let events = drain_registry_read_log();

    // Expected count: one per non-transparent shape-classified rule.
    // The shape dispatcher's `if !tag.is_classified() { continue; }`
    // gate matches the registry-observer's reach: both run after the
    // transparency-skip and the classification-skip.
    let expected: usize = prepared
        .ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_transparent)
        .filter(|r| {
            !matches!(
                prepared.ir.shape_assignments.get(r.id),
                ShapeTag::None
            )
        })
        .count();

    assert_eq!(
        events.len(),
        expected,
        "AZ-I.W1.B4: expected {expected} registry-read events (one per \
         shape-classified non-transparent rule); got {}",
        events.len()
    );
}
