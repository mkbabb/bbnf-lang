pub mod tape;

#[path = "grammars/json/mod.rs"]
pub mod generated_json;

pub mod grammars {
    pub use crate::generated_json as json;
}

#[cfg(test)]
mod tests {
    use crate::grammars::json::{parse, parse_bytes, JsonNodeKind, JsonValue};

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
}
