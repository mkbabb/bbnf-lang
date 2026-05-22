pub mod tape;

#[path = "grammars/json/mod.rs"]
pub mod generated_json;

#[path = "grammars/css_l4_declaration_values/mod.rs"]
pub mod generated_css_l4_declaration_values;

#[path = "grammars/css_l4_declaration_values_extended/mod.rs"]
pub mod generated_css_l4_declaration_values_extended;

#[path = "grammars/css_l4_stylesheet_selectors/mod.rs"]
pub mod generated_css_l4_stylesheet_selectors;

#[cfg(any(test, feature = "proof"))]
#[path = "grammars/json/event_grammar_witness.rs"]
pub mod json_event_grammar_witness;

#[cfg(any(test, feature = "proof"))]
#[path = "grammars/sheets_witness/mod.rs"]
pub mod sheets_witness;

pub mod grammars {
    pub use crate::generated_css_l4_declaration_values as css_l4_declaration_values;
    pub use crate::generated_css_l4_declaration_values_extended as css_l4_declaration_values_extended;
    pub use crate::generated_css_l4_stylesheet_selectors as css_l4_stylesheet_selectors;
    pub use crate::generated_json as json;
}

#[cfg(test)]
mod tests {
    use crate::grammars::json::{
        parse, parse_bytes, parse_direct, JsonNodeKind, JsonSink, JsonValue,
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

    #[test]
    fn css_l4_stylesheet_selectors_emit_fact_stream() {
        let input = concat!(
            "main.card#hero > a[href^=\"https\"]:hover::before,\n",
            "#nav .item[data-state=\"open\"] + button:focus::after { color: red; }\n"
        );
        let facts = crate::grammars::css_l4_stylesheet_selectors::parse(input).unwrap();
        assert!(facts.contains(
            "row\tid=css_l4/stylesheet_and_selectors/direct_to_struct/main\tplane=css_l4_stylesheet_selector_fact_stream"
        ));
        assert!(facts.contains(
            "end\trules=1\tselector_lists=1\tselectors=2\tselector_items=16\tdeclarations=1"
        ));
    }

    #[test]
    fn css_l4_declaration_values_extended_emit_fact_stream() {
        let input = concat!(
            ":root { --brand-\\31: rgb(255 128 0 / 50%); --gap: calc(100% - 2rem); }\n",
            ".card { width: calc(var(--gap, 10px) + clamp(1rem, 2vw, 3rem)); ",
            "color: color-mix(in srgb, var(--brand-\\31) 80%, white); ",
            "background-image: url(\"/assets/bg\\\\ space.svg\"); ",
            "mask-image: url(/assets/mask.svg); content: \"escaped\\\\A line\"; }\n"
        );
        let facts = crate::grammars::css_l4_declaration_values_extended::parse(input).unwrap();
        assert!(facts.contains(
            "row\tid=css_l4/declaration_values_extended/direct_to_struct/main\tplane=css_l4_declaration_value_extended_fact_stream"
        ));
        assert!(facts.contains("kind=function\tlexeme_hex=63616c63"));
        assert!(facts.contains("kind=url\tlexeme_hex=2f6173736574732f6d61736b2e737667"));
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
    fn css_l4_declaration_values_emit_fact_stream() {
        let css = "a { color: #ff00ff; width: 50%; }\n";
        let facts = crate::generated_css_l4_declaration_values::parse(css).unwrap();
        assert!(facts.contains("css-l4-declaration-value-facts-v1"));
        assert!(facts.contains("property_hex=636f6c6f72"));
        assert!(facts.contains("kind=hash\tlexeme_hex=666630306666"));
        assert!(facts.contains("property_hex=7769647468"));
        assert!(facts.contains("kind=percentage\tlexeme_hex=353025"));
    }
}
