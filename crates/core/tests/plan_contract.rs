//! The compile plan's grammar-level contract (sk-v27 LW1.COMP-3).
//!
//! - **Contradiction is refused.** A rule that two directives force
//!   to opposite call modes is a contradictory grammar. `@token`
//!   forces `InlineBody` and `@pretty` forces `DirectCall`. Born RED in
//!   LW1.COMP-3 (the priority chain picked `DirectCall` and the grammar
//!   compiled); LW2.COMP-2 makes the conflict ⊤ and the compile an
//!   error that names both directives.
//!
//! - **I7 generality.** No plan decision may read a rule's name. The
//!   same grammar under two spellings, one with names a heuristic might
//!   key on (`value`, `string`, `number`, `object`, `array`) and one
//!   opaque, must produce the same call strategies and the same rule
//!   metadata, rule for rule.

use bbnf::pipeline::{
    CompileError, CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar,
    compile_grammar_request, compute_call_strategies,
};

#[test]
fn contradictory_call_mode_directives_are_refused() {
    let source = r#"
        word = /[a-z]+/ ;
        pair = word, ":" >> word ;
        @token word ;
        @pretty word group ;
    "#;

    let ir = compile_grammar(source, &PipelineOptions::default())
        .expect("the grammar is well-formed apart from the contradiction");
    let decided = compute_call_strategies(&ir);

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
        match &output {
            Ok(CompileOutput::Rust(_)) => "Ok(Rust)",
            Ok(_) => "Ok(other)",
            Err(_) => unreachable!(),
        },
    );
    let Err(err) = output else { unreachable!() };
    assert!(
        matches!(&err, CompileError::CallModeConflict { rule, .. } if rule == "word"),
        "expected a call-mode conflict on `word`, got {err:?}"
    );
    let message = err.to_string();
    assert!(
        message.contains("@token") && message.contains("@pretty"),
        "the diagnostic must name both directives: {message}"
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
    let strategies = compute_call_strategies(&ir).expect("the plan solves");
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
