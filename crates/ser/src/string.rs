//! [`StringSerializer`]: compact serializer into an owned [`String`].

use crate::traits::Serializer;

/// Compact serializer into an owned `String`. Supports checkpoint/restore
/// via truncation.
pub struct StringSerializer {
    buf: String,
}

impl StringSerializer {
    pub fn new() -> Self {
        Self { buf: String::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buf: String::with_capacity(cap),
        }
    }

    pub fn finish(self) -> String {
        self.buf
    }

    pub fn as_str(&self) -> &str {
        &self.buf
    }
}

impl Default for StringSerializer {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Serializer<'a> for StringSerializer {
    type Checkpoint = usize;

    #[inline]
    fn text(&mut self, s: &'a str) {
        self.buf.push_str(s);
    }

    #[inline]
    fn text_owned(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    #[inline]
    fn char(&mut self, c: u8) {
        self.buf.push(c as char);
    }

    #[inline]
    fn text_inline_ws(&mut self, s: &'a str) {
        self.buf.push_str(s);
    }

    #[inline]
    fn bool(&mut self, v: bool) {
        self.buf.push_str(if v { "true" } else { "false" });
    }

    #[inline]
    fn i64(&mut self, v: i64) {
        use std::fmt::Write;
        let _ = write!(self.buf, "{v}");
    }

    #[inline]
    fn u64(&mut self, v: u64) {
        use std::fmt::Write;
        let _ = write!(self.buf, "{v}");
    }

    #[inline]
    fn f64(&mut self, v: f64) {
        let mut b = ryu::Buffer::new();
        self.buf.push_str(b.format(v));
    }

    #[inline]
    fn i128(&mut self, v: i128) {
        use std::fmt::Write;
        let _ = write!(self.buf, "{v}");
    }

    #[inline]
    fn u128(&mut self, v: u128) {
        use std::fmt::Write;
        let _ = write!(self.buf, "{v}");
    }

    #[inline]
    fn hardline(&mut self) {}
    #[inline]
    fn softline(&mut self) {}
    #[inline]
    fn break_line(&mut self) {}
    #[inline]
    fn group_open(&mut self) {}
    #[inline]
    fn group_close(&mut self) {}
    #[inline]
    fn indent_open(&mut self) {}
    #[inline]
    fn indent_close(&mut self) {}

    #[inline]
    fn sep(&mut self, flat: &str, _brk: &str) {
        self.buf.push_str(flat);
    }

    #[inline]
    fn checkpoint(&mut self) -> Self::Checkpoint {
        self.buf.len()
    }

    #[inline]
    fn restore(&mut self, cp: Self::Checkpoint) {
        self.buf.truncate(cp);
    }
}
