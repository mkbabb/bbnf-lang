//! The compile plan's grammar-level contract (sk-v27 LW1.COMP-3).
//!
//! - **Contradiction is refused (born-RED).** A rule that two
//!   directives force to opposite call modes is a contradictory
//!   grammar. `@token` forces `InlineBody` and `@pretty` forces
//!   `DirectCall`; today the inline plan's priority chain picks one
//!   (observed: `DirectCall`) and the grammar compiles. The cure
//!   (LW2.COMP-2) makes the conflict ⊤ and the compile an error.
//!
//! - **I7 generality.** No plan decision may read a rule's name. The
//!   same grammar under two spellings, one with names a heuristic might
//!   key on (`value`, `string`, `number`, `object`, `array`) and one
//!   opaque, must produce the same call strategies and the same rule
//!   metadata, rule for rule.

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar,
    compile_grammar_request, compute_call_strategies,
};

#[test]
#[ignore = "born-RED sk-v27 LW1.COMP-3: contradictory directives compile; cured by LW2.COMP-2"]
fn contradictory_call_mode_directives_are_refused() {
    let source = r#"
        word = /[a-z]+/ ;
        pair = word, ":" >> word ;
        @token word ;
        @pretty word group ;
    "#;

    let ir = compile_grammar(source, &PipelineOptions::default())
        .expect("the grammar is well-formed apart from the contradiction");
    let word = ir
        .rules
        .iter()
        .position(|rule| ir.get_string(rule.name) == "word")
        .expect("rule `word`");
    let decided = compute_call_strategies(&ir)[word];

    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Rust {
            requested_prettify: true,
        },
    };
    let output = compile_grammar_request(source, &request);
    assert!(
        output.is_err(),
        "`word` is forced InlineBody by @token and DirectCall by @pretty; the plan silently \
         decided {decided:?} and the compile returned {}",
        match output {
            Ok(CompileOutput::Rust(_)) => "Ok(Rust)",
            Ok(_) => "Ok(other)",
            Err(_) => unreachable!(),
        },
    );
}

/// One grammar, spelled with the eight names in `names`.
fn grammar(names: [&str; 8]) -> String {
    let [word, digits, sep, atom, items, list, pair, top] = names;
    format!(
        r#"
        {word} = /[a-z]+/ ;
        {digits} = /[0-9]+/ ;
        {sep} = "," ?w ;
        {atom} = {word} | {digits} | {list} ;
        {items} = ( {atom} << {sep} ? ) * ;
        {list} = "[" >> {items} ?w << "]" ;
        {pair} = {word}, ":" >> {atom} ;
        {top} = {pair} | {atom} ;
        @pretty {list} group ;
        "#
    )
}

fn plan_of(source: &str) -> Vec<String> {
    let ir = compile_grammar(source, &PipelineOptions::default()).expect("grammar compiles");
    let strategies = compute_call_strategies(&ir);
    ir.rules
        .iter()
        .zip(strategies)
        .map(|(rule, strategy)| format!("{strategy:?} {:?}", rule.meta))
        .collect()
}

#[test]
fn plan_decisions_do_not_read_rule_names() {
    let suggestive = plan_of(&grammar([
        "string", "number", "comma", "value", "members", "array", "pair", "object",
    ]));
    let opaque = plan_of(&grammar(["r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"]));
    assert_eq!(suggestive.len(), opaque.len());
    for (idx, (a, b)) in suggestive.iter().zip(&opaque).enumerate() {
        assert_eq!(a, b, "rule {idx} is planned differently under a renaming");
    }
}
