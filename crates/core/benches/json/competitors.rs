
//! JSON parsing competitor benchmarks — comprehensive shootout.
//!
//! 9 external parsers on the same 4 datasets. Each parser validates once
//! before the hot loop. Uses mimalloc for consistent allocation behavior.
//!
//! Competitors:
//!   serde_json       — hand-written, full owned parse to Value
//!   serde_json_borrow — hand-written, zero-copy borrowed parse
//!   sonic-rs         — SIMD, arena-allocated, full unescape
//!   simd-json        — SIMD, Rust port of simdjson (requires mutable input)
//!   jiter            — hand-written, Pydantic's iterable parser
//!   nom              — combinator, borrowed strings
//!   winnow           — combinator (nom successor), dispatch! macro
//!   pest             — PEG grammar-generated parser
//!   tree-sitter      — incremental, error-recovering parser generator (C-based)

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use divan::black_box;

fn load_json(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── serde_json (owned Value) ────────────────────────────────────────────────

macro_rules! bench_serde {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            serde_json::from_str::<serde_json::Value>(&input)
                .expect(concat!($file, ": serde_json parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                serde_json::from_str::<serde_json::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_serde!(serde_data, "data.json");
bench_serde!(serde_twitter, "twitter.json");
bench_serde!(serde_citm, "citm_catalog.json");
bench_serde!(serde_canada, "canada.json");
bench_serde!(serde_data_xl, "data_xl.json");

// ── serde_json_borrow (zero-copy borrowed Value) ────────────────────────────

macro_rules! bench_serde_borrow {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            serde_json::from_str::<serde_json_borrow::Value>(&input)
                .expect(concat!($file, ": serde_json_borrow parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                serde_json::from_str::<serde_json_borrow::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_serde_borrow!(serde_borrow_data, "data.json");
bench_serde_borrow!(serde_borrow_twitter, "twitter.json");
bench_serde_borrow!(serde_borrow_citm, "citm_catalog.json");
bench_serde_borrow!(serde_borrow_canada, "canada.json");
bench_serde_borrow!(serde_borrow_data_xl, "data_xl.json");

// ── sonic-rs (SIMD, arena-allocated) ────────────────────────────────────────

macro_rules! bench_sonic {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_sonic!(sonic_data, "data.json");
bench_sonic!(sonic_twitter, "twitter.json");
bench_sonic!(sonic_citm, "citm_catalog.json");
bench_sonic!(sonic_canada, "canada.json");
bench_sonic!(sonic_data_xl, "data_xl.json");

// ── simd-json (SIMD, requires mutable input — .to_vec() per iteration) ─────

macro_rules! bench_simd {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            {
                let mut bytes = input.as_bytes().to_vec();
                simd_json::to_owned_value(&mut bytes)
                    .expect(concat!($file, ": simd-json parse failed"));
            }
            b.with_inputs(|| input.clone()).bench_values(|input| {
                let mut bytes = input.as_bytes().to_vec();
                simd_json::to_owned_value(black_box(&mut bytes)).unwrap()
            });
        }
    };
}

bench_simd!(simd_data, "data.json");
bench_simd!(simd_twitter, "twitter.json");
bench_simd!(simd_citm, "citm_catalog.json");
bench_simd!(simd_canada, "canada.json");
bench_simd!(simd_data_xl, "data_xl.json");

// ── jiter (Pydantic's iterable parser, Cow<str> selective decode) ───────────

macro_rules! bench_jiter {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            jiter::JsonValue::parse(input.as_bytes(), false)
                .expect(concat!($file, ": jiter parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                jiter::JsonValue::parse(black_box(input.as_bytes()), false).unwrap()
            });
        }
    };
}

bench_jiter!(jiter_data, "data.json");
bench_jiter!(jiter_twitter, "twitter.json");
bench_jiter!(jiter_citm, "citm_catalog.json");
bench_jiter!(jiter_canada, "canada.json");
bench_jiter!(jiter_data_xl, "data_xl.json");

// ── nom (combinator, borrowed strings) ──────────────────────────────────────

mod nom_json {
    use std::collections::HashMap;

    use nom::{
        IResult,
        branch::alt,
        bytes::complete::{escaped, tag, take_while, take_while1},
        character::complete::{char, one_of},
        combinator::{cut, iterator, map, opt},
        multi::separated_list0,
        number::complete::double,
        sequence::{delimited, pair, preceded, separated_pair, terminated},
    };

    #[derive(Debug)]
    #[allow(dead_code)]
    pub enum JsonValue<'a> {
        Null,
        Str(&'a str),
        Boolean(bool),
        Num(f64),
        Array(Vec<JsonValue<'a>>),
        Object(HashMap<&'a str, JsonValue<'a>>),
    }

    fn is_string_character(c: char) -> bool {
        c != '"' && c != '\\'
    }

    fn is_space(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\r' || c == '\n'
    }

    fn sp(i: &str) -> IResult<&str, &str> {
        take_while(is_space)(i)
    }

    fn string(i: &str) -> IResult<&str, &str> {
        preceded(
            char('"'),
            cut(terminated(
                alt((
                    escaped(
                        take_while1(is_string_character),
                        '\\',
                        one_of("\"bfnrt\\/u"),
                    ),
                    tag(""),
                )),
                char('"'),
            )),
        )(i)
    }

    fn boolean(i: &str) -> IResult<&str, bool> {
        alt((map(tag("false"), |_| false), map(tag("true"), |_| true)))(i)
    }

    fn null(i: &str) -> IResult<&str, ()> {
        map(tag("null"), |_| ())(i)
    }

    fn array(i: &str) -> IResult<&str, Vec<JsonValue>> {
        preceded(
            char('['),
            cut(terminated(
                separated_list0(preceded(sp, char(',')), value),
                preceded(sp, char(']')),
            )),
        )(i)
    }

    fn key_value(i: &str) -> IResult<&str, (&str, JsonValue)> {
        separated_pair(preceded(sp, string), cut(preceded(sp, char(':'))), value)(i)
    }

    fn hash(i: &str) -> IResult<&str, HashMap<&str, JsonValue>> {
        let (i, _) = char('{')(i)?;
        let mut res = HashMap::default();

        match key_value(i) {
            Err(_) => preceded(sp, char('}'))(i).map(|(i, _)| (i, res)),
            Ok((i, first)) => {
                res.insert(first.0, first.1);
                let mut it = iterator(i, preceded(pair(sp, char(',')), key_value));
                res.extend(&mut it);
                let (i, _) = it.finish()?;
                preceded(sp, char('}'))(i).map(|(i, _)| (i, res))
            }
        }
    }

    fn value(i: &str) -> IResult<&str, JsonValue> {
        preceded(
            sp,
            alt((
                map(hash, JsonValue::Object),
                map(array, JsonValue::Array),
                map(string, JsonValue::Str),
                map(double, JsonValue::Num),
                map(boolean, JsonValue::Boolean),
                map(null, |_| JsonValue::Null),
            )),
        )(i)
    }

    pub fn root(i: &str) -> IResult<&str, JsonValue> {
        delimited(
            sp,
            alt((map(hash, JsonValue::Object), map(array, JsonValue::Array))),
            opt(sp),
        )(i)
    }
}

macro_rules! bench_nom {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            nom_json::root(&input).expect(concat!($file, ": nom parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                nom_json::root(black_box(&input)).unwrap();
            });
        }
    };
}

bench_nom!(nom_data, "data.json");
bench_nom!(nom_twitter, "twitter.json");
bench_nom!(nom_citm, "citm_catalog.json");
bench_nom!(nom_canada, "canada.json");
bench_nom!(nom_data_xl, "data_xl.json");

// ── winnow (combinator, dispatch! macro, borrowed strings) ──────────────────

mod winnow_json {
    use std::collections::HashMap;

    use winnow::ascii::float;
    use winnow::combinator::{
        delimited, dispatch, fail, peek, preceded, separated, separated_pair, terminated,
    };
    use winnow::prelude::*;
    use winnow::token::{any, take, take_while};

    #[derive(Debug, Clone)]
    #[allow(dead_code)]
    pub enum JsonValue<'a> {
        Null,
        Bool(bool),
        Num(f64),
        Str(&'a str),
        Array(Vec<JsonValue<'a>>),
        Object(HashMap<&'a str, JsonValue<'a>>),
    }

    pub fn json<'i>(input: &mut &'i str) -> ModalResult<JsonValue<'i>> {
        delimited(ws, json_value, ws).parse_next(input)
    }

    fn json_value<'i>(input: &mut &'i str) -> ModalResult<JsonValue<'i>> {
        dispatch!(peek(any);
            'n' => "null".value(JsonValue::Null),
            't' => "true".value(JsonValue::Bool(true)),
            'f' => "false".value(JsonValue::Bool(false)),
            '"' => string.map(JsonValue::Str),
            '+' => float.map(JsonValue::Num),
            '-' => float.map(JsonValue::Num),
            '0'..='9' => float.map(JsonValue::Num),
            '[' => array.map(JsonValue::Array),
            '{' => object.map(JsonValue::Object),
            _ => fail,
        )
        .parse_next(input)
    }

    fn string<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        preceded('"', terminated(string_content, '"')).parse_next(input)
    }

    fn string_content<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        let start = *input;
        loop {
            let _: &str = take_while(0.., |c: char| c != '"' && c != '\\').parse_next(input)?;

            if input.is_empty() || input.starts_with('"') {
                let consumed = start.len() - input.len();
                return Ok(&start[..consumed]);
            }

            // Backslash — skip escape sequence
            let _: char = any.parse_next(input)?;
            let esc: char = any.parse_next(input)?;
            match esc {
                '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => {}
                'u' => {
                    let _: &str = take(4usize).parse_next(input)?;
                }
                _ => {
                    return fail.parse_next(input);
                }
            }
        }
    }

    fn array<'i>(input: &mut &'i str) -> ModalResult<Vec<JsonValue<'i>>> {
        preceded(
            ('[', ws),
            terminated(separated(0.., json_value, (ws, ',', ws)), (ws, ']')),
        )
        .parse_next(input)
    }

    fn object<'i>(input: &mut &'i str) -> ModalResult<HashMap<&'i str, JsonValue<'i>>> {
        preceded(
            ('{', ws),
            terminated(separated(0.., key_value, (ws, ',', ws)), (ws, '}')),
        )
        .parse_next(input)
    }

    fn key_value<'i>(input: &mut &'i str) -> ModalResult<(&'i str, JsonValue<'i>)> {
        separated_pair(string, (ws, ':', ws), json_value).parse_next(input)
    }

    fn ws<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        take_while(0.., |c: char| " \t\r\n".contains(c)).parse_next(input)
    }
}

macro_rules! bench_winnow {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            use winnow::prelude::*;
            let input = load_json($file);
            winnow_json::json
                .parse(input.as_str())
                .expect(concat!($file, ": winnow parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                winnow_json::json.parse(black_box(input.as_str())).unwrap();
            });
        }
    };
}

bench_winnow!(winnow_data, "data.json");
bench_winnow!(winnow_twitter, "twitter.json");
bench_winnow!(winnow_citm, "citm_catalog.json");
bench_winnow!(winnow_canada, "canada.json");
bench_winnow!(winnow_data_xl, "data_xl.json");

// ── pest (PEG grammar-generated parser) ─────────────────────────────────────

mod pest_json {
    use std::collections::HashMap;

    use pest::iterators::Pair;
    use pest::{Parser, Span};

    use pest_grammars::json::*;

    #[allow(dead_code)]
    pub enum Json<'i> {
        Null,
        Bool(bool),
        Number(f64),
        String(Span<'i>),
        Array(Vec<Json<'i>>),
        Object(HashMap<Span<'i>, Json<'i>>),
    }

    fn consume_value(pair: Pair<Rule>) -> Json {
        // In pest_grammars 2.8+, `value` is a non-silent rule — unwrap it
        let pair = if pair.as_rule() == Rule::value {
            pair.into_inner().next().unwrap()
        } else {
            pair
        };

        match pair.as_rule() {
            Rule::null => Json::Null,
            Rule::bool => match pair.as_str() {
                "false" => Json::Bool(false),
                "true" => Json::Bool(true),
                _ => unreachable!(),
            },
            Rule::number => Json::Number(pair.as_str().parse().unwrap()),
            Rule::string => Json::String(pair.as_span()),
            Rule::array => Json::Array(pair.into_inner().map(consume_value).collect()),
            Rule::object => {
                let pairs = pair.into_inner().map(|pos| {
                    let mut pair = pos.into_inner();
                    let key = pair.next().unwrap().as_span();
                    let value = consume_value(pair.next().unwrap());
                    (key, value)
                });
                Json::Object(pairs.collect())
            }
            _ => unreachable!(),
        }
    }

    pub fn parse(data: &str) -> Json {
        let pair = JsonParser::parse(Rule::json, data).unwrap().next().unwrap();
        // json → value → actual type
        consume_value(pair.into_inner().next().unwrap())
    }
}

macro_rules! bench_pest {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            let _ = pest_json::parse(&input); // validate
            b.with_inputs(|| input.clone()).bench_values(|input| {
                pest_json::parse(black_box(&input));
            });
        }
    };
}

bench_pest!(pest_data, "data.json");
bench_pest!(pest_twitter, "twitter.json");
bench_pest!(pest_citm, "citm_catalog.json");
bench_pest!(pest_canada, "canada.json");
bench_pest!(pest_data_xl, "data_xl.json");

// ── tree-sitter (incremental, error-recovering, C-based) ─────────────────────

macro_rules! bench_tree_sitter {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load_json($file);
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&tree_sitter_json::LANGUAGE.into())
                .unwrap();
            assert!(
                parser.parse(&input, None).is_some(),
                concat!($file, ": tree-sitter parse failed")
            );
            b.with_inputs(|| input.clone()).bench_local(|input| {
                let mut parser = tree_sitter::Parser::new();
                parser
                    .set_language(&tree_sitter_json::LANGUAGE.into())
                    .unwrap();
                parser.parse(black_box(&input), None).unwrap()
            });
        }
    };
}

bench_tree_sitter!(tree_sitter_data, "data.json");
bench_tree_sitter!(tree_sitter_twitter, "twitter.json");
bench_tree_sitter!(tree_sitter_citm, "citm_catalog.json");
bench_tree_sitter!(tree_sitter_canada, "canada.json");
bench_tree_sitter!(tree_sitter_data_xl, "data_xl.json");

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
