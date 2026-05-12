use crate::track2::json;
use runtime::grammars::json::{JsonRoot, JsonToken};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: String,
    pub span: std::ops::Range<usize>,
    pub payload_class: u16,
}

#[derive(Debug, thiserror::Error)]
pub enum ParityError {
    #[error("track parse failed: {0}")]
    Parse(String),
    #[error("token stream divergence")]
    TokenStream,
    #[error("canonical serialization divergence: track1={track1}, track2={track2}")]
    Serialization { track1: String, track2: String },
    #[error("payload counters diverged from zero: writes={writes}, allocations={allocations}")]
    PayloadCounters { writes: u64, allocations: u64 },
}

pub fn assert_parity(input: &str) -> Result<(), ParityError> {
    let track1 = parse_track1(input)?;
    let track2 = json::parse(input).map_err(|error| ParityError::Parse(error.to_string()))?;

    let stream1 = token_stream_from_root(&track1);
    let stream2 = token_stream_from_root(&track2);
    if stream1 != stream2 {
        return Err(ParityError::TokenStream);
    }

    let track1_writes = track1.tape().payloads().write_count() as u64;
    let track1_allocations = track1.tape().payloads().allocation_count() as u64;
    if track1_writes != 0 || track1_allocations != 0 {
        return Err(ParityError::PayloadCounters {
            writes: track1_writes,
            allocations: track1_allocations,
        });
    }

    let track2_writes = track2.tape().payloads().write_count() as u64;
    let track2_allocations = track2.tape().payloads().allocation_count() as u64;
    if track2_writes != 0 || track2_allocations != 0 {
        return Err(ParityError::PayloadCounters {
            writes: track2_writes,
            allocations: track2_allocations,
        });
    }

    let s1 = track1.to_canonical_string();
    let s2 = track2.to_canonical_string();
    if s1 != s2 {
        return Err(ParityError::Serialization {
            track1: s1,
            track2: s2,
        });
    }

    Ok(())
}

pub fn serialize_canonical(input: &str) -> Result<String, ParityError> {
    Ok(parse_track1(input)?.to_canonical_string())
}

pub fn token_stream_from_root(root: &JsonRoot<'_>) -> Vec<Token> {
    root.token_stream().map(token_from_runtime).collect()
}

fn parse_track1<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParityError> {
    runtime::generated_json::parse(input).map_err(|error| ParityError::Parse(error.to_string()))
}

fn token_from_runtime(token: JsonToken) -> Token {
    Token {
        kind: token.kind.name().to_string(),
        span: token.start as usize..token.end as usize,
        payload_class: token.flags.payload_class(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_serialization_sorts_object_keys() {
        let a = serialize_canonical(r#"{"b":2,"a":1}"#).unwrap();
        let b = serialize_canonical(r#"{"a":1,"b":2}"#).unwrap();
        assert_eq!(a, b);
        assert_eq!(a, r#"{"a":1,"b":2}"#);
    }

    #[test]
    fn token_stream_tracks_spans_and_payload_classes() {
        let root = runtime::generated_json::parse(r#"{"a":[1,true]}"#).unwrap();
        let tokens = token_stream_from_root(&root);
        assert_eq!(tokens[0].kind, "Root");
        assert_eq!(tokens[2].kind, "Pair");
        assert_eq!(tokens[3].kind, "String");
        assert_eq!(tokens[3].span, 2..3);
    }

    #[test]
    fn parity_accepts_equivalent_tracks() {
        assert_parity(r#"{"b":2,"a":[true,null,"x"]}"#).unwrap();
    }
}
