pub mod tape {
    use std::marker::PhantomData;
    use std::sync::atomic::{AtomicU64, Ordering};

    use simd_scan::{
        scan_json_parse_index, scan_json_structurals, JsonParseIndex, StructuralIndex,
    };

    static NEXT_TAPE_ID: AtomicU64 = AtomicU64::new(1);

    #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
    #[repr(transparent)]
    pub struct NodeKindId(pub u16);

    #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
    #[repr(transparent)]
    pub struct TokenFlags(pub u16);

    impl TokenFlags {
        pub const PAYLOAD_CLASS_MASK: u16 = 0x000f;
        pub const INLINE_BOOL_NULL: u16 = 0;
        pub const INLINE_NUMBER_FAST: u16 = 1;
        pub const INLINE_STRING_BORROW: u16 = 2;
        pub const ARENA_OFFSET: u16 = 3;
        pub const SIBLING_SKIP: u16 = 4;

        pub const HAS_SCALAR_CACHE: u16 = 1 << 4;
        pub const STRING_NEEDS_UNESCAPE: u16 = 1 << 5;
        pub const STRING_BORROWS_SOURCE: u16 = 1 << 6;
        pub const IS_STRUCTURAL_OPEN: u16 = 1 << 7;
        pub const IS_STRUCTURAL_CLOSE: u16 = 1 << 8;
        pub const RECOVERY_KIND_SHIFT: u16 = 9;
        pub const RECOVERY_KIND_MASK: u16 = 0b11 << Self::RECOVERY_KIND_SHIFT;

        pub const fn new(payload_class: u16) -> Self {
            Self(payload_class & Self::PAYLOAD_CLASS_MASK)
        }

        pub const fn with(self, bit: u16) -> Self {
            Self(self.0 | bit)
        }

        pub const fn payload_class(self) -> u16 {
            self.0 & Self::PAYLOAD_CLASS_MASK
        }

        pub const fn contains(self, bit: u16) -> bool {
            self.0 & bit != 0
        }

        pub const fn bits(self) -> u16 {
            self.0
        }
    }

    #[repr(C, align(16))]
    #[derive(Copy, Clone, Eq, PartialEq, Debug)]
    pub struct TapeToken {
        pub kind: NodeKindId,
        pub flags: TokenFlags,
        pub start: u32,
        pub end: u32,
        pub payload_or_skip: u32,
    }

    impl TapeToken {
        pub fn new(
            kind: NodeKindId,
            flags: TokenFlags,
            start: usize,
            end: usize,
            payload_or_skip: u32,
        ) -> Self {
            Self {
                kind,
                flags,
                start: checked_u32(start),
                end: checked_u32(end),
                payload_or_skip,
            }
        }

        #[inline]
        pub fn span(&self) -> std::ops::Range<usize> {
            self.start as usize..self.end as usize
        }

        #[inline]
        pub fn subtree_skip(&self) -> usize {
            if self.flags.payload_class() == TokenFlags::SIBLING_SKIP {
                self.payload_or_skip as usize
            } else {
                1
            }
        }
    }

    pub struct PayloadArena {
        bytes: Vec<u8>,
        #[cfg(any(test, feature = "bench-counters"))]
        writes: u32,
        #[cfg(any(test, feature = "bench-counters"))]
        allocations: u32,
    }

    impl PayloadArena {
        pub fn empty() -> Self {
            Self {
                bytes: Vec::new(),
                #[cfg(any(test, feature = "bench-counters"))]
                writes: 0,
                #[cfg(any(test, feature = "bench-counters"))]
                allocations: 0,
            }
        }

        pub fn len(&self) -> usize {
            self.bytes.len()
        }

        pub fn is_empty(&self) -> bool {
            self.bytes.is_empty()
        }

        pub fn write_bytes(&mut self, bytes: &[u8]) -> u32 {
            let offset = checked_u32(self.bytes.len());
            #[cfg(any(test, feature = "bench-counters"))]
            let old_capacity = self.bytes.capacity();
            self.bytes.extend_from_slice(bytes);
            #[cfg(any(test, feature = "bench-counters"))]
            {
                self.writes += 1;
                if self.bytes.capacity() != old_capacity {
                    self.allocations += 1;
                }
            }
            offset
        }

        #[cfg(any(test, feature = "bench-counters"))]
        pub fn write_count(&self) -> u32 {
            self.writes
        }

        #[cfg(any(test, feature = "bench-counters"))]
        pub fn allocation_count(&self) -> u32 {
            self.allocations
        }
    }

    #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
    pub struct TapeId(pub u64);

    pub struct Tape<'input> {
        source: &'input [u8],
        tokens: Vec<TapeToken>,
        payloads: PayloadArena,
        id: TapeId,
    }

    impl<'input> Tape<'input> {
        pub fn source(&self) -> &'input [u8] {
            self.source
        }

        pub fn tokens(&self) -> &[TapeToken] {
            &self.tokens
        }

        pub fn token_capacity(&self) -> usize {
            self.tokens.capacity()
        }

        pub fn token(&self, index: u32) -> &TapeToken {
            &self.tokens[index as usize]
        }

        pub fn payloads(&self) -> &PayloadArena {
            &self.payloads
        }

        pub fn id(&self) -> TapeId {
            self.id
        }
    }

    pub struct TapeBuilder<'input> {
        source: &'input [u8],
        tokens: Vec<TapeToken>,
        payloads: PayloadArena,
    }

    impl<'input> TapeBuilder<'input> {
        pub fn new(source: &'input [u8]) -> Self {
            Self {
                source,
                tokens: Vec::new(),
                payloads: PayloadArena::empty(),
            }
        }

        pub fn with_capacity(source: &'input [u8], token_capacity: usize) -> Self {
            Self {
                source,
                tokens: Vec::with_capacity(token_capacity),
                payloads: PayloadArena::empty(),
            }
        }

        pub fn reserve_tokens(&mut self, additional: usize) {
            self.tokens.reserve(additional);
        }

        pub fn emit(
            &mut self,
            kind: NodeKindId,
            flags: TokenFlags,
            start: usize,
            end: usize,
            payload_or_skip: u32,
        ) -> u32 {
            let index = checked_u32(self.tokens.len());
            self.tokens
                .push(TapeToken::new(kind, flags, start, end, payload_or_skip));
            index
        }

        pub fn patch_end(&mut self, index: u32, end: usize) {
            self.tokens[index as usize].end = checked_u32(end);
        }

        pub fn patch_skip_to_current_len(&mut self, index: u32) {
            let skip = self.tokens.len() - index as usize;
            self.tokens[index as usize].payload_or_skip = checked_u32(skip);
        }

        pub fn patch_skip(&mut self, index: u32, skip: usize) {
            self.tokens[index as usize].payload_or_skip = checked_u32(skip);
        }

        pub fn token(&self, index: u32) -> &TapeToken {
            &self.tokens[index as usize]
        }

        pub fn finish(self) -> Tape<'input> {
            Tape {
                source: self.source,
                tokens: self.tokens,
                payloads: self.payloads,
                id: TapeId(NEXT_TAPE_ID.fetch_add(1, Ordering::Relaxed)),
            }
        }
    }

    pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind> {
        tape: &'doc Tape<'input>,
        index: u32,
        _kind: PhantomData<fn() -> K>,
        _input: PhantomData<&'input [u8]>,
    }

    impl<'doc, 'input: 'doc, K> Copy for ValueRef<'doc, 'input, K> {}

    impl<'doc, 'input: 'doc, K> Clone for ValueRef<'doc, 'input, K> {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl<'doc, 'input: 'doc, K> ValueRef<'doc, 'input, K> {
        pub fn new(tape: &'doc Tape<'input>, index: u32) -> Self {
            Self {
                tape,
                index,
                _kind: PhantomData,
                _input: PhantomData,
            }
        }

        pub fn erase(self) -> ValueRef<'doc, 'input, AnyKind> {
            ValueRef::new(self.tape, self.index)
        }

        pub fn tape(&self) -> &'doc Tape<'input> {
            self.tape
        }

        pub fn index(&self) -> u32 {
            self.index
        }

        pub fn token(&self) -> &'doc TapeToken {
            self.tape.token(self.index)
        }
    }

    pub enum AnyKind {}

    pub trait DocumentView<'a> {
        type Root: 'a;
        fn root_value(&'a self) -> Self::Root;
        fn tape_id(&self) -> TapeId;
        fn source(&'a self) -> &'a [u8];
    }

    pub fn scan_structurals(source: &[u8]) -> StructuralIndex {
        scan_json_structurals(source)
    }

    pub fn scan_parse_index(source: &[u8]) -> JsonParseIndex {
        scan_json_parse_index(source)
    }

    fn checked_u32(value: usize) -> u32 {
        u32::try_from(value).expect("skinny hot path supports inputs up to u32::MAX bytes")
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn tape_token_is_sixteen_bytes() {
            assert_eq!(std::mem::size_of::<TapeToken>(), 16);
            assert_eq!(std::mem::align_of::<TapeToken>(), 16);
        }
    }
}

#[path = "grammars/json/mod.rs"]
pub mod generated_json;

pub mod grammars {
    pub use crate::generated_json as json;
}

#[cfg(test)]
mod tests {
    use crate::grammars::json::{parse, JsonNodeKind, JsonValue};

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
    fn records_container_and_pair_skips() {
        let root = parse(r#"{"a":{"b":[]},"c":0}"#).unwrap();
        let tokens: Vec<_> = root.token_stream().collect();
        assert_eq!(tokens[0].kind, JsonNodeKind::Root);
        assert_eq!(tokens[1].kind, JsonNodeKind::ObjectOpen);
        assert!(tokens
            .iter()
            .any(|token| token.kind == JsonNodeKind::Pair && token.payload_or_skip > 1));
    }

    #[test]
    fn rejects_malformed_json() {
        for invalid in [
            "",
            "[1,]",
            r#"{"a":}"#,
            r#"{"a" 1}"#,
            r#""bad\uZZZZ""#,
            "01",
            "true false",
        ] {
            assert!(parse(invalid).is_err(), "{invalid}");
        }
    }

    #[test]
    fn canonical_serialization_removes_layout() {
        let root = parse("{\n  \"b\" : [ false, null ], \"a\":\"x\\ny\" }").unwrap();
        assert_eq!(
            root.to_canonical_string(),
            r#"{"a":"x\ny","b":[false,null]}"#
        );
    }
}
