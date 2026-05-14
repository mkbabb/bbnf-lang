use runtime::grammars::json::JsonSink;
use serde::de::{MapAccess, SeqAccess, Visitor};
use serde::Deserialize;
use std::borrow::Cow;
use std::fmt;

use parse_that_regex::{
    match_json_string_at_quote_trusted_utf8,
    number::{
        match_number_span_from_first, materialize_f64, materialize_i64, materialize_u64, NumberSpan,
    },
    skip_json_whitespace, unescape_json_string,
};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct JsonDirectDigest {
    pub objects: u64,
    pub arrays: u64,
    pub members: u64,
    pub elements: u64,
    pub strings: u64,
    pub string_bytes: u64,
    pub numbers: u64,
    pub bools: u64,
    pub trues: u64,
    pub nulls: u64,
    pub max_depth: u32,
    pub fingerprint: u64,
}

#[derive(Debug, thiserror::Error)]
pub enum DirectStructError {
    #[error("track parse failed: {0}")]
    Parse(String),
    #[error("serde_json direct struct failed: {0}")]
    Serde(String),
    #[error("sonic-rs direct struct failed: {0}")]
    Sonic(String),
    #[error("direct-to-struct digest mismatch: track1={track1:?} track2={track2:?} serde={serde:?} sonic={sonic:?}")]
    Mismatch {
        track1: JsonDirectDigest,
        track2: JsonDirectDigest,
        serde: JsonDirectDigest,
        sonic: JsonDirectDigest,
    },
}

impl JsonDirectDigest {
    fn scalar(tag: u64, fingerprint: u64) -> Self {
        Self {
            max_depth: 1,
            fingerprint: mix(tag, fingerprint),
            ..Self::default()
        }
    }

    fn string(value: &str) -> Self {
        let mut digest = Self::scalar(0x53, hash_bytes(value.as_bytes()));
        digest.strings = 1;
        digest.string_bytes = value.len() as u64;
        digest
    }

    fn number_i64(value: i64) -> Self {
        Self::number(0x4e49, value as u64)
    }

    fn number_u64(value: u64) -> Self {
        Self::number(0x4e55, value)
    }

    fn number_f64(value: f64) -> Self {
        Self::number(0x4e46, value.to_bits())
    }

    fn number(tag: u64, value: u64) -> Self {
        let mut digest = Self::scalar(tag, value);
        digest.numbers = 1;
        digest
    }

    fn number_raw_known(input: &[u8], span: &NumberSpan) -> Self {
        if span.is_integer {
            if span.negative {
                if &input[span.start..span.end] == b"-0" {
                    return Self::number_f64(-0.0);
                }
                if let Ok(value) = materialize_i64(input, span) {
                    return Self::number_i64(value);
                }
            } else if let Ok(value) = materialize_u64(input, span) {
                return Self::number_u64(value);
            }
        }
        materialize_f64(input, span)
            .map(Self::number_f64)
            .unwrap_or_else(|_| Self::number_f64(f64::NAN))
    }

    fn bool(value: bool) -> Self {
        let mut digest = Self::scalar(0x42, u64::from(value));
        digest.bools = 1;
        digest.trues = u64::from(value);
        digest
    }

    fn null() -> Self {
        let mut digest = Self::scalar(0x30, 0);
        digest.nulls = 1;
        digest
    }

    #[inline(always)]
    fn fold_string_scalar(&mut self, value: &str) {
        self.strings += 1;
        self.string_bytes += value.len() as u64;
        self.max_depth = self.max_depth.max(2);
        self.fingerprint = mix(self.fingerprint, mix(0x53, hash_bytes(value.as_bytes())));
    }

    #[inline(always)]
    fn fold_number_i64_scalar(&mut self, value: i64) {
        self.fold_number_scalar(0x4e49, value as u64);
    }

    #[inline(always)]
    fn fold_number_u64_scalar(&mut self, value: u64) {
        self.fold_number_scalar(0x4e55, value);
    }

    #[inline(always)]
    fn fold_number_f64_scalar(&mut self, value: f64) {
        self.fold_number_scalar(0x4e46, value.to_bits());
    }

    #[inline(always)]
    fn fold_number_scalar(&mut self, tag: u64, value: u64) {
        self.numbers += 1;
        self.max_depth = self.max_depth.max(2);
        self.fingerprint = mix(self.fingerprint, mix(tag, value));
    }

    #[inline(always)]
    fn fold_bool_scalar(&mut self, value: bool) {
        self.bools += 1;
        self.trues += u64::from(value);
        self.max_depth = self.max_depth.max(2);
        self.fingerprint = mix(self.fingerprint, mix(0x42, u64::from(value)));
    }

    #[inline(always)]
    fn fold_null_scalar(&mut self) {
        self.nulls += 1;
        self.max_depth = self.max_depth.max(2);
        self.fingerprint = mix(self.fingerprint, mix(0x30, 0));
    }

    fn fold_child(&mut self, child: Self) {
        self.objects += child.objects;
        self.arrays += child.arrays;
        self.members += child.members;
        self.elements += child.elements;
        self.strings += child.strings;
        self.string_bytes += child.string_bytes;
        self.numbers += child.numbers;
        self.bools += child.bools;
        self.trues += child.trues;
        self.nulls += child.nulls;
        self.max_depth = self.max_depth.max(child.max_depth + 1);
        self.fingerprint = mix(self.fingerprint, child.fingerprint);
    }

    fn fold_key(&mut self, key: &str) {
        self.string_bytes += key.len() as u64;
        self.fingerprint = mix(self.fingerprint, hash_bytes(key.as_bytes()));
    }

    fn same_shape_as(&self, other: &Self) -> bool {
        self.objects == other.objects
            && self.arrays == other.arrays
            && self.members == other.members
            && self.elements == other.elements
            && self.strings == other.strings
            && self.string_bytes == other.string_bytes
            && self.numbers == other.numbers
            && self.bools == other.bools
            && self.trues == other.trues
            && self.nulls == other.nulls
            && self.max_depth == other.max_depth
    }
}

#[derive(Debug, Default)]
struct JsonDigestSink {
    stack: Vec<DigestFrame>,
    root: Option<JsonDirectDigest>,
}

#[derive(Debug)]
enum DigestFrame {
    Object(JsonDirectDigest),
    Array(JsonDirectDigest),
}

impl JsonDigestSink {
    fn finish(self) -> Result<JsonDirectDigest, DirectStructError> {
        if !self.stack.is_empty() {
            return Err(DirectStructError::Parse(
                "direct sink finished with open containers".to_string(),
            ));
        }
        self.root
            .ok_or_else(|| DirectStructError::Parse("empty direct sink".to_string()))
    }

    fn push_value(&mut self, child: JsonDirectDigest) {
        match self.stack.last_mut() {
            Some(DigestFrame::Object(parent)) => parent.fold_child(child),
            Some(DigestFrame::Array(parent)) => {
                parent.elements += 1;
                parent.fold_child(child);
            }
            None => self.root = Some(child),
        }
    }

    #[inline(always)]
    fn with_object_parent(&mut self, f: impl FnOnce(&mut JsonDirectDigest)) {
        let Some(DigestFrame::Object(parent)) = self.stack.last_mut() else {
            unreachable!("object scalar outside object frame");
        };
        f(parent);
    }

    #[inline(always)]
    fn with_array_parent(&mut self, f: impl FnOnce(&mut JsonDirectDigest)) {
        let Some(DigestFrame::Array(parent)) = self.stack.last_mut() else {
            unreachable!("array scalar outside array frame");
        };
        parent.elements += 1;
        f(parent);
    }
}

impl JsonSink for JsonDigestSink {
    fn begin_object(&mut self) {
        self.stack.push(DigestFrame::Object(JsonDirectDigest {
            objects: 1,
            max_depth: 1,
            fingerprint: 0x7b,
            ..JsonDirectDigest::default()
        }));
    }

    fn end_object(&mut self) {
        let Some(DigestFrame::Object(mut digest)) = self.stack.pop() else {
            debug_assert!(false, "object end without object frame");
            return;
        };
        digest.fingerprint = mix(digest.fingerprint, digest.members);
        self.push_value(digest);
    }

    fn begin_array(&mut self) {
        self.stack.push(DigestFrame::Array(JsonDirectDigest {
            arrays: 1,
            max_depth: 1,
            fingerprint: 0x5b,
            ..JsonDirectDigest::default()
        }));
    }

    fn end_array(&mut self) {
        let Some(DigestFrame::Array(mut digest)) = self.stack.pop() else {
            debug_assert!(false, "array end without array frame");
            return;
        };
        digest.fingerprint = mix(digest.fingerprint, digest.elements);
        self.push_value(digest);
    }

    fn key(&mut self, value: &str) {
        let Some(DigestFrame::Object(object)) = self.stack.last_mut() else {
            debug_assert!(false, "key outside object frame");
            return;
        };
        object.members += 1;
        object.fold_key(value);
    }

    fn string(&mut self, value: &str) {
        self.push_value(JsonDirectDigest::string(value));
    }

    fn i64(&mut self, value: i64) {
        self.push_value(JsonDirectDigest::number_i64(value));
    }

    fn u64(&mut self, value: u64) {
        self.push_value(JsonDirectDigest::number_u64(value));
    }

    fn f64(&mut self, value: f64) {
        self.push_value(JsonDirectDigest::number_f64(value));
    }

    fn bool(&mut self, value: bool) {
        self.push_value(JsonDirectDigest::bool(value));
    }

    fn null(&mut self) {
        self.push_value(JsonDirectDigest::null());
    }

    fn array_string(&mut self, value: &str) {
        self.with_array_parent(|parent| parent.fold_string_scalar(value));
    }

    fn array_i64(&mut self, value: i64) {
        self.with_array_parent(|parent| parent.fold_number_i64_scalar(value));
    }

    fn array_u64(&mut self, value: u64) {
        self.with_array_parent(|parent| parent.fold_number_u64_scalar(value));
    }

    fn array_f64(&mut self, value: f64) {
        self.with_array_parent(|parent| parent.fold_number_f64_scalar(value));
    }

    fn array_bool(&mut self, value: bool) {
        self.with_array_parent(|parent| parent.fold_bool_scalar(value));
    }

    fn array_null(&mut self) {
        self.with_array_parent(JsonDirectDigest::fold_null_scalar);
    }

    fn object_string(&mut self, value: &str) {
        self.with_object_parent(|parent| parent.fold_string_scalar(value));
    }

    fn object_i64(&mut self, value: i64) {
        self.with_object_parent(|parent| parent.fold_number_i64_scalar(value));
    }

    fn object_u64(&mut self, value: u64) {
        self.with_object_parent(|parent| parent.fold_number_u64_scalar(value));
    }

    fn object_f64(&mut self, value: f64) {
        self.with_object_parent(|parent| parent.fold_number_f64_scalar(value));
    }

    fn object_bool(&mut self, value: bool) {
        self.with_object_parent(|parent| parent.fold_bool_scalar(value));
    }

    fn object_null(&mut self) {
        self.with_object_parent(JsonDirectDigest::fold_null_scalar);
    }
}

pub fn track1_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
    let mut sink = JsonDigestSink::default();
    runtime::generated_json::parse_direct(input, &mut sink)
        .map_err(|error| DirectStructError::Parse(error.to_string()))?;
    sink.finish()
}

pub fn track2_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
    hand::sink_digest(input)
}

pub fn serde_digest(bytes: &[u8]) -> Result<JsonDirectDigest, DirectStructError> {
    serde_json::from_slice(bytes).map_err(|error| DirectStructError::Serde(error.to_string()))
}

pub fn sonic_digest(bytes: &[u8]) -> Result<JsonDirectDigest, DirectStructError> {
    sonic_rs::from_slice(bytes).map_err(|error| DirectStructError::Sonic(error.to_string()))
}

pub fn assert_direct_struct_parity(input: &str, bytes: &[u8]) -> Result<(), DirectStructError> {
    let track1 = track1_digest(input)?;
    let track2 = track2_digest(input)?;
    let serde = serde_digest(bytes)?;
    let sonic = sonic_digest(bytes)?;
    if track1 == track2 && track1.same_shape_as(&serde) && track1.same_shape_as(&sonic) {
        Ok(())
    } else {
        Err(DirectStructError::Mismatch {
            track1,
            track2,
            serde,
            sonic,
        })
    }
}

mod hand {
    use super::*;

    pub(super) fn sink_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
        let mut parser = HandParser {
            bytes: input.as_bytes(),
            cursor: 0,
        };
        let digest = parser.value()?;
        parser.ws();
        if parser.cursor == parser.bytes.len() {
            Ok(digest)
        } else {
            Err(parser.error("trailing characters"))
        }
    }

    struct HandParser<'a> {
        bytes: &'a [u8],
        cursor: usize,
    }

    impl<'a> HandParser<'a> {
        fn value(&mut self) -> Result<JsonDirectDigest, DirectStructError> {
            self.ws();
            self.value_at()
        }

        fn value_at(&mut self) -> Result<JsonDirectDigest, DirectStructError> {
            let Some(byte) = self.bytes.get(self.cursor).copied() else {
                return Err(self.error("expected value"));
            };
            match byte {
                b'{' => self.object(),
                b'[' => self.array(),
                b'"' => self
                    .string()
                    .map(|value| JsonDirectDigest::string(value.as_ref())),
                b'-' | b'0'..=b'9' => self.number(byte),
                b't' => self.literal(b"true", JsonDirectDigest::bool(true)),
                b'f' => self.literal(b"false", JsonDirectDigest::bool(false)),
                b'n' => self.literal(b"null", JsonDirectDigest::null()),
                _ => Err(self.error("expected value")),
            }
        }

        fn object(&mut self) -> Result<JsonDirectDigest, DirectStructError> {
            self.expect(b'{')?;
            self.ws();
            let mut digest = JsonDirectDigest {
                objects: 1,
                max_depth: 1,
                fingerprint: 0x7b,
                ..JsonDirectDigest::default()
            };
            if self.take(b'}') {
                digest.fingerprint = mix(digest.fingerprint, digest.members);
                return Ok(digest);
            }
            loop {
                let key = self.string()?;
                digest.members += 1;
                digest.fold_key(key.as_ref());
                self.ws();
                self.expect(b':')?;
                digest.fold_child(self.value()?);
                self.ws();
                if self.take(b',') {
                    self.ws();
                    continue;
                }
                self.expect(b'}')?;
                digest.fingerprint = mix(digest.fingerprint, digest.members);
                return Ok(digest);
            }
        }

        fn array(&mut self) -> Result<JsonDirectDigest, DirectStructError> {
            self.expect(b'[')?;
            self.ws();
            let mut digest = JsonDirectDigest {
                arrays: 1,
                max_depth: 1,
                fingerprint: 0x5b,
                ..JsonDirectDigest::default()
            };
            if self.take(b']') {
                digest.fingerprint = mix(digest.fingerprint, digest.elements);
                return Ok(digest);
            }
            loop {
                digest.elements += 1;
                digest.fold_child(self.value()?);
                self.ws();
                if self.take(b',') {
                    self.ws();
                    continue;
                }
                self.expect(b']')?;
                digest.fingerprint = mix(digest.fingerprint, digest.elements);
                return Ok(digest);
            }
        }

        fn string(&mut self) -> Result<Cow<'a, str>, DirectStructError> {
            let span = match_json_string_at_quote_trusted_utf8(self.bytes, self.cursor)
                .map_err(|error| DirectStructError::Parse(error.to_string()))?;
            // SAFETY: DirectParser receives `&str`; the trusted matcher still
            // locates escapes and controls before this raw slice is borrowed.
            let raw = unsafe {
                std::str::from_utf8_unchecked(&self.bytes[span.content_start..span.content_end])
            };
            self.cursor = span.raw_end;
            if span.needs_unescape {
                unescape_json_string(raw)
                    .map_err(|error| DirectStructError::Parse(error.to_string()))
            } else {
                Ok(Cow::Borrowed(raw))
            }
        }

        fn number(&mut self, first: u8) -> Result<JsonDirectDigest, DirectStructError> {
            let number = match_number_span_from_first(self.bytes, self.cursor, first)
                .ok_or_else(|| self.error("invalid number"))?;
            self.cursor = number.end;
            Ok(JsonDirectDigest::number_raw_known(self.bytes, &number))
        }

        fn literal(
            &mut self,
            literal: &'static [u8],
            digest: JsonDirectDigest,
        ) -> Result<JsonDirectDigest, DirectStructError> {
            if self.bytes.get(self.cursor..self.cursor + literal.len()) != Some(literal) {
                return Err(self.error("invalid literal"));
            }
            self.cursor += literal.len();
            Ok(digest)
        }

        fn ws(&mut self) {
            self.cursor = skip_json_whitespace(self.bytes, self.cursor);
        }

        fn expect(&mut self, byte: u8) -> Result<(), DirectStructError> {
            if self.take(byte) {
                Ok(())
            } else {
                Err(self.error("unexpected delimiter"))
            }
        }

        fn take(&mut self, byte: u8) -> bool {
            if self.bytes.get(self.cursor) == Some(&byte) {
                self.cursor += 1;
                true
            } else {
                false
            }
        }

        fn error(&self, message: &'static str) -> DirectStructError {
            DirectStructError::Parse(format!("{message} at byte {}", self.cursor))
        }
    }
}

impl<'de> Deserialize<'de> for JsonDirectDigest {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(DigestVisitor)
    }
}

struct DigestVisitor;

impl<'de> Visitor<'de> for DigestVisitor {
    type Value = JsonDirectDigest;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("any JSON value")
    }

    fn visit_bool<E>(self, value: bool) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::bool(value))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::number_i64(value))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::number_u64(value))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::number_f64(value))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::string(value))
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::null())
    }

    fn visit_none<E>(self) -> Result<Self::Value, E> {
        Ok(JsonDirectDigest::null())
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        JsonDirectDigest::deserialize(deserializer)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut digest = JsonDirectDigest {
            arrays: 1,
            max_depth: 1,
            fingerprint: 0x5b,
            ..JsonDirectDigest::default()
        };
        while let Some(child) = seq.next_element::<JsonDirectDigest>()? {
            digest.elements += 1;
            digest.fold_child(child);
        }
        digest.fingerprint = mix(digest.fingerprint, digest.elements);
        Ok(digest)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut digest = JsonDirectDigest {
            objects: 1,
            max_depth: 1,
            fingerprint: 0x7b,
            ..JsonDirectDigest::default()
        };
        while let Some(key) = map.next_key::<Cow<'de, str>>()? {
            let child = map.next_value::<JsonDirectDigest>()?;
            digest.members += 1;
            digest.fold_key(key.as_ref());
            digest.fold_child(child);
        }
        digest.fingerprint = mix(digest.fingerprint, digest.members);
        Ok(digest)
    }
}

fn hash_bytes(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64 ^ bytes.len() as u64;
    let mut chunks = bytes.chunks_exact(8);
    for chunk in &mut chunks {
        let value = u64::from_le_bytes(chunk.try_into().expect("chunk size is 8"));
        hash = mix(hash, value);
    }
    let tail = chunks.remainder();
    if !tail.is_empty() {
        let mut value = 0_u64;
        for (index, byte) in tail.iter().copied().enumerate() {
            value |= (byte as u64) << (index * 8);
        }
        hash = mix(hash, value);
    }
    hash = mix(hash, bytes.len() as u64);
    hash
}

fn mix(seed: u64, value: u64) -> u64 {
    seed ^ value
        .wrapping_add(0x9e3779b97f4a7c15)
        .wrapping_add(seed << 6)
        .wrapping_add(seed >> 2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn direct_digest_matches_tracks_and_serde_baselines() {
        let input = br#"{"b":[1,true,null],"a":"x\u0020y"}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_direct_struct_parity(text, input).unwrap();
    }

    #[test]
    fn direct_digest_preserves_integer_width_class() {
        let input = br#"[9223372036854775807,9223372036854775808,1.5,-0]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_direct_struct_parity(text, input).unwrap();
    }

    #[test]
    fn exact_number_digest_matches_serde_number_classes() {
        for raw in [
            "-0",
            "0",
            "9223372036854775807",
            "9223372036854775808",
            "18446744073709551615",
            "18446744073709551616",
            "-9223372036854775808",
            "-9223372036854775809",
            "1.0",
            "1e0",
            "-0.0",
            "5e-324",
            "2.2250738585072014e-308",
            "1.7976931348623157e308",
            "43.474709000000125",
            "6.02214076e23",
        ] {
            let number = match_number_span_from_first(raw.as_bytes(), 0, raw.as_bytes()[0])
                .expect("test case must be a JSON number");
            let expected: JsonDirectDigest = serde_json::from_str(raw).unwrap();
            assert_eq!(
                JsonDirectDigest::number_raw_known(raw.as_bytes(), &number),
                expected,
                "{raw}"
            );
        }
    }
}
