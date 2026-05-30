pub mod tape;

/// Grammar-neutral bridge to the shared `bbnf-simd` structural primitives (the
/// CSS structural scan's NEON inner-skip + comment/bracket mask consumers;
/// alphabet/byte-set as caller data, Lock 14).
pub mod runtime_simd;

#[path = "grammars/json/mod.rs"]
pub mod generated_json;

#[path = "grammars/css_l4_declaration_values/mod.rs"]
pub mod generated_css_l4_declaration_values;

#[path = "grammars/css_l4_declaration_values_extended/mod.rs"]
pub mod generated_css_l4_declaration_values_extended;

#[path = "grammars/css_l4_stylesheet_selectors/mod.rs"]
pub mod generated_css_l4_stylesheet_selectors;

#[path = "grammars/css_l4_visual_functions/mod.rs"]
pub mod generated_css_l4_visual_functions;

#[path = "grammars/css_l4_at_rules_and_media/mod.rs"]
pub mod generated_css_l4_at_rules_and_media;

#[path = "grammars/css_l4_vendor_and_custom_atrules/mod.rs"]
pub mod generated_css_l4_vendor_and_custom_atrules;

#[path = "grammars/css_l4_nested_layout/mod.rs"]
pub mod generated_css_l4_nested_layout;

#[cfg(any(test, feature = "proof"))]
#[path = "grammars/json/event_grammar_witness.rs"]
pub mod json_event_grammar_witness;

#[cfg(any(test, feature = "proof"))]
#[path = "grammars/sheets_witness/mod.rs"]
pub mod sheets_witness;

pub mod grammars {
    pub use crate::generated_css_l4_at_rules_and_media as css_l4_at_rules_and_media;
    pub use crate::generated_css_l4_declaration_values as css_l4_declaration_values;
    pub use crate::generated_css_l4_declaration_values_extended as css_l4_declaration_values_extended;
    pub use crate::generated_css_l4_nested_layout as css_l4_nested_layout;
    pub use crate::generated_css_l4_stylesheet_selectors as css_l4_stylesheet_selectors;
    pub use crate::generated_css_l4_vendor_and_custom_atrules as css_l4_vendor_and_custom_atrules;
    pub use crate::generated_css_l4_visual_functions as css_l4_visual_functions;
    pub use crate::generated_json as json;
}

#[cfg(test)]
mod tests {
    use crate::grammars::json::{
        parse, parse_bytes, parse_direct, parse_only, parse_only_bytes, JsonNodeKind, JsonSink,
        JsonValue,
    };

    #[test]
    fn parses_and_projects_json() {
        let root = parse(r#"{"name":"bbnf","values":[1,true,null,"\u0041"]}"#).unwrap();
        assert_eq!(root.tape().payloads().len(), 0);
        assert_eq!(root.tape().payloads().write_count(), 0);
        assert_eq!(root.tape().payloads().allocation_count(), 0);

        let JsonValue::Object(object) = root.value() else {
            panic!("expected object");
        };
        assert_eq!(object.len(), 2);

        let JsonValue::Array(values) = object.get("values").unwrap() else {
            panic!("expected array");
        };
        let collected: Vec<_> = values
            .values()
            .map(|value| value.to_canonical_string())
            .collect();
        assert_eq!(collected, ["1", "true", "null", r#""A""#]);
    }

    // SK-V17 W1: the seven CSS Track-1 round-trip consumers no longer assert a
    // fact-stream `String`. They parse into the EXISTING skinny offset tape and
    // assert the tape is ACTIVATED (offsets emitted, zero `PayloadArena` writes
    // — the lazy-projection invariant) and that the 4-field structural summary,
    // reconstructed lazily from the tape via `ValueRef`, equals the hand-counted
    // reference for the fixture. No String round-trip assertion survives.
    // Tape-activation + lazy-projection assertion, generic over each grammar's
    // own `CssDocument` type (the seven modules emit isomorphic-but-distinct
    // generated types, so this is a macro rather than a typed fn).
    macro_rules! assert_css_summary {
        ($document:expr, $rules:expr, $at:expr, $qual:expr, $decls:expr) => {{
            let document = &$document;
            assert!(document.tape().offsets().len() > 0, "tape must be activated");
            assert_eq!(document.tape().payloads().write_count(), 0);
            assert_eq!(document.tape().payloads().allocation_count(), 0);
            let summary = document.summary();
            assert_eq!(summary.rules, $rules, "rules");
            assert_eq!(summary.at_rules, $at, "at_rules");
            assert_eq!(summary.qualified_rules, $qual, "qualified_rules");
            assert_eq!(summary.declarations, $decls, "declarations");
            assert_eq!(document.tape().offsets().len(), $rules + $decls);
        }};
    }

    #[test]
    fn css_l4_stylesheet_selectors_routes_into_tape() {
        let input = concat!(
            "main.card#hero > a[href^=\"https\"]:hover::before,\n",
            "#nav .item[data-state=\"open\"] + button:focus::after { color: red; }\n"
        );
        let document = crate::grammars::css_l4_stylesheet_selectors::parse(input).unwrap();
        // one qualified rule, one declaration.
        assert_css_summary!(document, 1, 0, 1, 1);
    }

    #[test]
    fn css_l4_declaration_values_extended_routes_into_tape() {
        let input = concat!(
            ":root { --brand-\\31: rgb(255 128 0 / 50%); --gap: calc(100% - 2rem); }\n",
            ".card { width: calc(var(--gap, 10px) + clamp(1rem, 2vw, 3rem)); ",
            "color: color-mix(in srgb, var(--brand-\\31) 80%, white); ",
            "background-image: url(\"/assets/bg\\\\ space.svg\"); ",
            "mask-image: url(/assets/mask.svg); content: \"escaped\\\\A line\"; }\n"
        );
        let document = crate::grammars::css_l4_declaration_values_extended::parse(input).unwrap();
        // two qualified rules; :root has 2 decls, .card has 5 decls.
        assert_css_summary!(document, 2, 0, 2, 7);
    }

    #[test]
    fn css_l4_visual_functions_routes_into_tape() {
        let input = concat!(
            ".hero { background-image: linear-gradient(45deg, #123456 0%, color-mix(in srgb, red 30%, blue) 100%); ",
            "transform: translate3d(10px, 20%, 0) rotate(12deg) scale(1.2); ",
            "filter: blur(2px) brightness(1.2) contrast(80%); ",
            "transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1); ",
            "animation-timing-function: steps(4, jump-end); }\n"
        );
        let document = crate::grammars::css_l4_visual_functions::parse(input).unwrap();
        // one qualified rule, five declarations.
        assert_css_summary!(document, 1, 0, 1, 5);
    }

    #[test]
    fn css_l4_at_rules_and_media_routes_into_tape() {
        let input = concat!(
            "@media screen and (min-width:1px){a{color:red}}\n",
            "@keyframes k{from,50%,to{opacity:1}}\n"
        );
        let document = crate::grammars::css_l4_at_rules_and_media::parse(input).unwrap();
        // @media (at) + nested a{} (qualified); @keyframes (at) + nested frame
        // block (qualified). rules=4 (2 at + 2 qualified), 2 declarations.
        let summary = document.summary();
        assert!(document.tape().offsets().len() > 0);
        assert_eq!(document.tape().payloads().write_count(), 0);
        assert_eq!(summary.at_rules, 2, "at_rules");
        assert_eq!(summary.declarations, 2, "declarations");
        assert_eq!(summary.rules, summary.at_rules + summary.qualified_rules);
    }

    #[test]
    fn css_l4_vendor_and_custom_atrules_routes_into_tape() {
        let input = concat!(
            "@custom-media --narrow (max-width:30em);\n",
            "@-webkit-keyframes fade{from{opacity:0}to{opacity:1}}\n",
            "a{-webkit-user-select:none;-moz-user-select:none;user-select:none}\n",
        );
        let document = crate::grammars::css_l4_vendor_and_custom_atrules::parse(input).unwrap();
        let summary = document.summary();
        assert!(document.tape().offsets().len() > 0);
        assert_eq!(document.tape().payloads().write_count(), 0);
        // @custom-media (at, ;-terminated) + @-webkit-keyframes (at) + a{} (qualified).
        assert_eq!(summary.at_rules, 2, "at_rules");
        assert!(summary.declarations >= 5, "declarations");
        assert_eq!(summary.rules, summary.at_rules + summary.qualified_rules);
    }

    #[test]
    fn css_l4_nested_layout_routes_into_tape() {
        let input = concat!(
            ".grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1rem;",
            "&>.item{margin-inline-start:1rem;inline-size:calc(100% - 2rem)}}\n",
            ".nav{display:flex;flex-direction:row;align-items:center;",
            "justify-content:space-between;padding-block:1rem;",
            "border-inline-start:2px solid #123456}\n",
            ".type{color:#123456;font-size:clamp(1rem,2vw,2rem);line-height:1.4}\n",
        );
        let document = crate::grammars::css_l4_nested_layout::parse(input).unwrap();
        let summary = document.summary();
        assert!(document.tape().offsets().len() > 0);
        assert_eq!(document.tape().payloads().write_count(), 0);
        assert_eq!(summary.at_rules, 0, "at_rules");
        // 14 declarations across .grid (+nested .item), .nav, .type.
        assert_eq!(summary.declarations, 14, "declarations");
        assert_eq!(summary.rules, summary.qualified_rules);
    }

    #[test]
    fn records_lazy_offset_tape() {
        let root = parse(r#"{"a":{"b":[]},"c":0}"#).unwrap();
        assert_eq!(
            root.tape()
                .offset_at(0)
                .map(|offset| root.source().as_bytes()[offset]),
            Some(b'{')
        );
        assert_eq!(root.tape().offset_bytes(), root.tape().offsets().len() * 4);
        assert!(root
            .token_stream()
            .any(|token| token.kind == JsonNodeKind::ObjectOpen));
    }

    #[test]
    fn rejects_malformed_json() {
        for invalid in [
            "",
            "[1,]",
            r#"{"a":}"#,
            r#"{"a" 1}"#,
            r#""bad\uZZZZ""#,
            r#""bad\uD800""#,
            r#""bad\uDD1E""#,
            "01",
            "true false",
        ] {
            assert!(parse(invalid).is_err(), "{invalid}");
        }
        assert!(parse_bytes(b"[\"\xC3\x28\"]").is_err());
    }

    #[test]
    fn generated_parse_only_accepts_and_rejects_json() {
        for valid in [
            "0",
            r#""plain""#,
            r#"{"name":"bbnf","values":[1,true,false,null,"\u0041"]}"#,
            "[[],{},-0,5e-324]",
        ] {
            parse_only(valid).unwrap();
        }

        for invalid in [
            "",
            "[1,]",
            r#"{"a":}"#,
            r#"{"a" 1}"#,
            r#""bad\uZZZZ""#,
            "01",
            "true false",
        ] {
            assert!(parse_only(invalid).is_err(), "{invalid}");
        }
        assert!(parse_only_bytes(b"[\"\xC3\x28\"]").is_err());
    }

    #[test]
    fn canonical_serialization_removes_layout() {
        let root = parse("{\n  \"b\" : [ false, null ], \"a\":\"x\\ny\" }").unwrap();
        assert_eq!(
            root.to_canonical_string(),
            r#"{"a":"x\ny","b":[false,null]}"#
        );
    }

    #[test]
    fn float_materialization_matches_serde_bits() {
        for literal in [
            "-0",
            "5e-324",
            "2.2250738585072014e-308",
            "1.7976931348623157e308",
            "43.474709000000125",
            "6.02214076e23",
        ] {
            let root = parse(literal).unwrap();
            let JsonValue::Number(number) = root.value() else {
                panic!("expected number");
            };
            let skinny = number.as_f64().unwrap().to_bits();
            let serde = serde_json::from_str::<serde_json::Number>(literal)
                .unwrap()
                .as_f64()
                .unwrap()
                .to_bits();
            assert_eq!(skinny, serde, "{literal}");
        }
    }

    #[test]
    fn generated_direct_parser_dispatches_context_sink_hooks() {
        #[derive(Default)]
        struct RecordingSink {
            events: Vec<String>,
        }

        impl RecordingSink {
            fn event(&mut self, event: impl Into<String>) {
                self.events.push(event.into());
            }
        }

        impl JsonSink for RecordingSink {
            fn begin_object(&mut self) {
                self.event("begin_object");
            }

            fn end_object(&mut self) {
                self.event("end_object");
            }

            fn begin_array(&mut self) {
                self.event("begin_array");
            }

            fn end_array(&mut self) {
                self.event("end_array");
            }

            fn key(&mut self, value: &str) {
                self.event(format!("key:{value}"));
            }

            fn string(&mut self, value: &str) {
                self.event(format!("root_string:{value}"));
            }

            fn i64(&mut self, value: i64) {
                self.event(format!("root_i64:{value}"));
            }

            fn u64(&mut self, value: u64) {
                self.event(format!("root_u64:{value}"));
            }

            fn f64(&mut self, value: f64) {
                self.event(format!("root_f64:{:016x}", value.to_bits()));
            }

            fn bool(&mut self, value: bool) {
                self.event(format!("root_bool:{value}"));
            }

            fn null(&mut self) {
                self.event("root_null");
            }

            fn array_string(&mut self, value: &str) {
                self.event(format!("array_string:{value}"));
            }

            fn array_i64(&mut self, value: i64) {
                self.event(format!("array_i64:{value}"));
            }

            fn array_u64(&mut self, value: u64) {
                self.event(format!("array_u64:{value}"));
            }

            fn array_f64(&mut self, value: f64) {
                self.event(format!("array_f64:{:016x}", value.to_bits()));
            }

            fn array_bool(&mut self, value: bool) {
                self.event(format!("array_bool:{value}"));
            }

            fn array_null(&mut self) {
                self.event("array_null");
            }

            fn object_string(&mut self, value: &str) {
                self.event(format!("object_string:{value}"));
            }

            fn object_i64(&mut self, value: i64) {
                self.event(format!("object_i64:{value}"));
            }

            fn object_u64(&mut self, value: u64) {
                self.event(format!("object_u64:{value}"));
            }

            fn object_f64(&mut self, value: f64) {
                self.event(format!("object_f64:{:016x}", value.to_bits()));
            }

            fn object_bool(&mut self, value: bool) {
                self.event(format!("object_bool:{value}"));
            }

            fn object_null(&mut self) {
                self.event("object_null");
            }
        }

        let mut root_scalar = RecordingSink::default();
        parse_direct("42", &mut root_scalar).unwrap();
        assert_eq!(root_scalar.events, ["root_u64:42"]);

        let mut sink = RecordingSink::default();
        parse_direct(
            r#"{"s":"x","i":-1,"u":2,"f":-0,"b":true,"n":null,"o":{"k":false},"a":["y",3,false,null,{"z":4},[5]]}"#,
            &mut sink,
        )
        .unwrap();

        assert_eq!(
            sink.events,
            [
                "begin_object",
                "key:s",
                "object_string:x",
                "key:i",
                "object_i64:-1",
                "key:u",
                "object_u64:2",
                "key:f",
                "object_f64:8000000000000000",
                "key:b",
                "object_bool:true",
                "key:n",
                "object_null",
                "key:o",
                "begin_object",
                "key:k",
                "object_bool:false",
                "end_object",
                "key:a",
                "begin_array",
                "array_string:y",
                "array_u64:3",
                "array_bool:false",
                "array_null",
                "begin_object",
                "key:z",
                "object_u64:4",
                "end_object",
                "begin_array",
                "array_u64:5",
                "end_array",
                "end_array",
                "end_object"
            ]
        );
    }

    #[test]
    fn css_l4_declaration_values_routes_into_tape() {
        let css = "a { color: #ff00ff; width: 50%; }\n";
        let document = crate::generated_css_l4_declaration_values::parse(css).unwrap();
        // one qualified rule, two declarations.
        assert_css_summary!(document, 1, 0, 1, 2);
    }

    #[test]
    fn css_l4_rich_projection_is_lazy_and_typed() {
        use crate::generated_css_l4_declaration_values as css;
        use css::CssTypedValue;

        // Two qualified rules (3 + 2 selectors) and a typed value mix:
        // dimension (10px), color (#abc), number (0.5), function (rgb(...)),
        // keyword (red), dimension (2rem).
        let input = concat!(
            "a, b.c, #d { margin: 10px; color: #abc; opacity: 0.5; }\n",
            ".x, .y { background: rgb(1 2 3); border: red; gap: 2rem; }\n"
        );
        let document = css::parse(input).unwrap();
        // The rich projection materializes NOTHING: zero payload writes/allocs.
        assert_eq!(document.tape().payloads().write_count(), 0, "lazy: no payload writes");
        assert_eq!(document.tape().payloads().allocation_count(), 0, "lazy: no payload allocs");

        let rich = document.rich_summary();
        // 4-field structural plane stays consistent with the lazy 4-field summary.
        let base = document.summary();
        assert_eq!(rich.rules, base.rules);
        assert_eq!(rich.at_rules, base.at_rules);
        assert_eq!(rich.qualified_rules, base.qualified_rules);
        assert_eq!(rich.declarations, base.declarations);
        // Rich value plane.
        assert_eq!(rich.rules, 2, "rules");
        assert_eq!(rich.qualified_rules, 2, "qualified_rules");
        assert_eq!(rich.declarations, 6, "declarations");
        assert_eq!(rich.selectors, 5, "selectors (3 + 2)");
        assert_eq!(rich.dimensions, 2, "dimensions (10px, 2rem)");
        assert_eq!(rich.numbers, 1, "numbers (0.5)");
        assert_eq!(rich.colors, 1, "colors (#abc)");
        assert_eq!(rich.functions, 1, "functions (rgb(...))");

        // The typed value-node decode is recovered from the source byte at the
        // value head — no stored tag.
        assert_eq!(CssTypedValue::classify(b"10px"), CssTypedValue::Dimension);
        assert_eq!(CssTypedValue::classify(b"#abc"), CssTypedValue::Color);
        assert_eq!(CssTypedValue::classify(b"0.5"), CssTypedValue::Number);
        assert_eq!(CssTypedValue::classify(b"rgb(1 2 3)"), CssTypedValue::Function);
        assert_eq!(CssTypedValue::classify(b"red"), CssTypedValue::Keyword);
        assert_eq!(CssTypedValue::classify(b"50%"), CssTypedValue::Dimension);
    }

    // ---- W3 NEON runtime-consumer parity guards ----------------------------
    //
    // The NEON inner-skip (`find_css_significant`), the L5 comment-close
    // consumer (`find_comment_close`), and the L6 top-level-comma consumer
    // (`count_top_level_commas`) accelerate the CSS scan; these guards pin them
    // to their scalar references so an acceleration regression is caught here,
    // independent of the corpus-scale equality oracle.

    fn significant_ref(bytes: &[u8], from: usize, delims: &[u8], fixed: &[u8; 9]) -> usize {
        let mut pos = from;
        while pos < bytes.len() {
            let byte = bytes[pos];
            if delims.contains(&byte) || fixed.contains(&byte) {
                return pos;
            }
            pos += 1;
        }
        pos
    }

    fn comment_close_ref(bytes: &[u8], start: usize) -> Option<usize> {
        let mut pos = start + 2;
        while pos + 1 < bytes.len() {
            if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
                return Some(pos + 2);
            }
            pos += 1;
        }
        None
    }

    fn count_top_level_commas_ref(bytes: &[u8]) -> usize {
        let mut depth: i32 = 0;
        let mut commas = 0usize;
        let mut pos = 0usize;
        while pos < bytes.len() {
            match bytes[pos] {
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth = depth.saturating_sub(1),
                b'"' | b'\'' => {
                    let quote = bytes[pos];
                    let mut c = pos + 1;
                    while c < bytes.len() {
                        match bytes[c] {
                            b'\\' => c += 2,
                            x if x == quote => {
                                c += 1;
                                break;
                            }
                            _ => c += 1,
                        }
                    }
                    pos = c;
                    continue;
                }
                b',' if depth == 0 => commas += 1,
                _ => {}
            }
            pos += 1;
        }
        commas
    }

    #[test]
    fn neon_significant_skip_matches_scalar() {
        const FIXED: &[u8; 9] = b"'\"/()[]{}";
        let corpora: [&[u8]; 5] = [
            b".a{color:red;background:url(x)}/* c */\n@media(x){.b,.c{d:e}}",
            b"a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9",
            br#"x:"quoted; with delims {}" ; y : 'single (paren)' }"#,
            b"",
            b"012345678901234567890123456789012345678901234567890123456789012345;{}()",
        ];
        for input in corpora {
            for &delims in &[&b";{}"[..], &b"{};"[..], &b":{};"[..], &b";}"[..]] {
                for from in 0..=input.len() {
                    let got = crate::runtime_simd::find_css_significant(input, from, delims, FIXED);
                    let want = significant_ref(input, from, delims, FIXED);
                    assert_eq!(
                        got, want,
                        "significant skip diverged: from={from} delims={delims:?} input={input:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn neon_comment_close_matches_scalar() {
        let cases: [&[u8]; 7] = [
            b"/* short */rest",
            b"/**/x",
            b"/* unterminated",
            b"/* exactly-64-bytes-of-comment-body-aaaaaaaaaaaaaaaaaaaaaaaaaa */z",
            b"/* a */ /* b */",
            b"/* contains ; { } : delimiters that must NOT leak */ .x{}",
            b"/*aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa*/tail",
        ];
        for input in cases {
            assert_eq!(
                crate::runtime_simd::find_comment_close(input, 0),
                comment_close_ref(input, 0),
                "comment-close diverged: input={input:?}"
            );
        }
        for pad in 55..70 {
            let mut input = b"/*".to_vec();
            input.resize(pad, b'a');
            input.extend_from_slice(b"*/tail");
            assert_eq!(
                crate::runtime_simd::find_comment_close(&input, 0),
                comment_close_ref(&input, 0),
                "boundary comment-close diverged: pad={pad}"
            );
        }
    }

    #[test]
    fn neon_top_level_commas_matches_scalar() {
        let cases: [&[u8]; 8] = [
            b".a, .b, .c",
            b"a:not(.x, .y), b[attr=\"v,w\"], c",
            b"",
            b".single",
            b"li:nth-child(2n+1), .grid > [data-col], main .a, .b, .c, .d, .e, .f, .g, .h, .i",
            b"x:matches(:is(a, b), :where(c, d)), y",
            br#".a[title="comma, inside"] , .b"#,
            b".aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, .b, .c",
        ];
        for input in cases {
            assert_eq!(
                crate::runtime_simd::count_top_level_commas(input),
                count_top_level_commas_ref(input),
                "top-level comma count diverged: input={input:?}"
            );
        }
        for pad in 58..70 {
            let mut input = vec![b'x'; pad];
            input.extend_from_slice(b"(a, b), c, d");
            assert_eq!(
                crate::runtime_simd::count_top_level_commas(&input),
                count_top_level_commas_ref(&input),
                "boundary comma count diverged: pad={pad}"
            );
        }
    }
}
