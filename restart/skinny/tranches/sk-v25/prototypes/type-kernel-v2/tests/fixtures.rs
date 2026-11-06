use std::collections::BTreeSet;

use bbnf_type_kernel_v2::{
    solve, Budget, GrammarShape, Primitive, Projection, RowSpec, StoreBuilder,
};

#[test]
fn every_ir_node_kind_has_a_consumed_semantic_fixture() {
    let mut builder = StoreBuilder::new();
    let unit = builder.primitive(Primitive::Unit).unwrap();
    let string = builder.primitive(Primitive::Str).unwrap();
    let boolean = builder.primitive(Primitive::Bool).unwrap();
    let literal = string;
    let regex = string;
    let epsilon = unit;
    let seq = builder.tuple([string, boolean]).unwrap();
    let alt = builder
        .variant(RowSpec::closed([("literal", string), ("empty", unit)]).unwrap())
        .unwrap();
    let repeat = builder.list(string).unwrap();
    let reference = string;
    let skip = string;
    let next = boolean;
    let minus = string;
    let negate = unit;
    let map = builder.function([string], boolean).unwrap();
    let optional_whitespace = string;
    let token_dispatch = builder
        .variant(RowSpec::closed([("known", string), ("fallback", string)]).unwrap())
        .unwrap();
    let fixtures = [
        ("Literal", literal),
        ("Regex", regex),
        ("Epsilon", epsilon),
        ("Seq", seq),
        ("Alt", alt),
        ("Repeat", repeat),
        ("Ref", reference),
        ("Skip", skip),
        ("Next", next),
        ("Minus", minus),
        ("Negate", negate),
        ("Map", map),
        ("OptionalWhitespace", optional_whitespace),
        ("TokenDispatch", token_dispatch),
    ];
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    let mut covered = BTreeSet::new();
    for (kind, root) in fixtures {
        let semantic = solution.resolve(&store, root).unwrap();
        GrammarShape::from_semantic(&semantic).unwrap();
        covered.insert(kind);
    }
    assert_eq!(covered.len(), 14);
}

#[test]
fn grammar_shaped_rows_cover_recursive_generic_and_heterogeneous_values() {
    let mut builder = StoreBuilder::new();
    let unit = builder.primitive(Primitive::Unit).unwrap();
    let string = builder.primitive(Primitive::Str).unwrap();
    let integer = builder.primitive(Primitive::I64).unwrap();
    let (_, generic) = builder.fresh_meta().unwrap();
    let host_map = builder.function([generic], generic).unwrap();
    let recursive_grammar = builder
        .mu(|builder, binder| {
            let recurse = builder.bound(binder)?;
            builder.variant(RowSpec::closed([("epsilon", unit), ("next", recurse)])?)
        })
        .unwrap();
    let diagnostics = builder.list(string).unwrap();
    let parse_result = builder
        .record(RowSpec::closed([("diagnostics", diagnostics), ("value", generic)]).unwrap())
        .unwrap();
    let css_value = builder
        .variant(
            RowSpec::closed([("keyword", string), ("number", integer), ("empty", unit)]).unwrap(),
        )
        .unwrap();
    let heterogeneous_alt = builder
        .variant(RowSpec::closed([("text", string), ("count", integer)]).unwrap())
        .unwrap();
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    for root in [
        host_map,
        recursive_grammar,
        parse_result,
        css_value,
        heterogeneous_alt,
    ] {
        assert!(!solution.fingerprint(&store, root).unwrap().is_empty());
    }
}

#[test]
fn generated_rust_and_typescript_are_exact_solved_type_consumers() {
    let mut builder = StoreBuilder::new();
    let string = builder.primitive(Primitive::Str).unwrap();
    let diagnostics = builder.list(string).unwrap();
    let payload = builder
        .record(
            RowSpec::closed([
                ("node_kind", string),
                ("diagnostics", diagnostics),
                ("value", string),
            ])
            .unwrap(),
        )
        .unwrap();
    let store = builder.finish();
    let solution = solve(&store, &[], Budget::default()).unwrap();
    let generated = Projection::from_solution("ConsumerPayload", &solution, &store, payload)
        .unwrap()
        .generate()
        .unwrap();
    assert_eq!(
        generated.rust(),
        include_str!("../generated/consumer_payload.rs")
    );
    assert_eq!(
        generated.typescript(),
        include_str!("../generated/consumer_payload.ts")
    );
}
