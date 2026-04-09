//! [`SliceDeserializer`]: byte-slice deserializer over `&'a str`.

use crate::traits::Deserializer;

/// Byte-slice deserializer. Reads from a `&'a str` input with checkpoint/restore.
pub struct SliceDeserializer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> SliceDeserializer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.pos..]
    }
}

impl<'a> Deserializer<'a> for SliceDeserializer<'a> {
    type Checkpoint = usize;

    fn text_exact(&mut self, s: &str) -> bool {
        if self.remaining().starts_with(s) {
            self.pos += s.len();
            true
        } else {
            false
        }
    }

    fn text_span(&mut self) -> Option<&'a str> {
        let rest = self.remaining();
        if rest.is_empty() {
            return None;
        }
        let end = rest
            .bytes()
            .position(|b| matches!(b, b'{' | b'}' | b'[' | b']' | b',' | b':'))
            .unwrap_or(rest.len());
        if end == 0 {
            return None;
        }
        let span = &rest[..end];
        self.pos += end;
        Some(span)
    }

    fn char_exact(&mut self, c: u8) -> bool {
        if self.remaining().as_bytes().first() == Some(&c) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn skip_ws(&mut self) {
        let rest = self.remaining();
        let trimmed = rest.trim_ascii_start();
        self.pos += rest.len() - trimmed.len();
    }

    fn i64(&mut self) -> Option<i64> {
        let rest = self.remaining();
        let end = rest
            .bytes()
            .position(|b| !b.is_ascii_digit() && b != b'-')
            .unwrap_or(rest.len());
        let val: i64 = rest[..end].parse().ok()?;
        self.pos += end;
        Some(val)
    }

    fn u64(&mut self) -> Option<u64> {
        let rest = self.remaining();
        let end = rest
            .bytes()
            .position(|b| !b.is_ascii_digit())
            .unwrap_or(rest.len());
        let val: u64 = rest[..end].parse().ok()?;
        self.pos += end;
        Some(val)
    }

    fn f64(&mut self) -> Option<f64> {
        let rest = self.remaining();
        let end = rest
            .bytes()
            .position(|b| !matches!(b, b'0'..=b'9' | b'.' | b'-' | b'+' | b'e' | b'E'))
            .unwrap_or(rest.len());
        let val: f64 = rest[..end].parse().ok()?;
        self.pos += end;
        Some(val)
    }

    fn peek_byte(&self) -> Option<u8> {
        self.remaining().as_bytes().first().copied()
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn offset(&self) -> usize {
        self.pos
    }

    fn checkpoint(&mut self) -> Self::Checkpoint {
        self.pos
    }

    fn restore(&mut self, cp: Self::Checkpoint) {
        self.pos = cp;
    }
}
