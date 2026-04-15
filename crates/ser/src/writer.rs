//! [`WriterSerializer`]: minimal-bytes serializer over [`std::io::Write`].

use std::io::{self, Write};

use crate::traits::Serializer;

/// Minimal-bytes serializer over [`Write`]. Writes content directly,
/// ignores all formatting (groups, indentation, line breaks).
pub struct WriterSerializer<W: Write> {
    writer: W,
    error: Option<io::Error>,
}

impl<W: Write> WriterSerializer<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            error: None,
        }
    }

    pub fn finish(self) -> Result<W, io::Error> {
        match self.error {
            Some(e) => Err(e),
            None => Ok(self.writer),
        }
    }

    pub fn has_error(&self) -> bool {
        self.error.is_some()
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        if self.error.is_none() {
            if let Err(e) = self.writer.write_all(bytes) {
                self.error = Some(e);
            }
        }
    }
}

impl<'a, W: Write> Serializer<'a> for WriterSerializer<W> {
    type Checkpoint = u64;

    #[inline]
    fn text(&mut self, s: &'a str) {
        self.write(s.as_bytes());
    }

    #[inline]
    fn text_owned(&mut self, s: &str) {
        self.write(s.as_bytes());
    }

    #[inline]
    fn char(&mut self, c: u8) {
        self.write(&[c]);
    }

    #[inline]
    fn text_inline_ws(&mut self, s: &'a str) {
        self.write(s.as_bytes());
    }

    #[inline]
    fn bool(&mut self, v: bool) {
        self.write(if v { b"true" } else { b"false" });
    }

    #[inline]
    fn i64(&mut self, v: i64) {
        let mut buf = itoa::Buffer::new();
        self.write(buf.format(v).as_bytes());
    }

    #[inline]
    fn u64(&mut self, v: u64) {
        let mut buf = itoa::Buffer::new();
        self.write(buf.format(v).as_bytes());
    }

    #[inline]
    fn f64(&mut self, v: f64) {
        let mut buf = ryu::Buffer::new();
        self.write(buf.format(v).as_bytes());
    }

    #[inline]
    fn i128(&mut self, v: i128) {
        if self.error.is_none() {
            if let Err(e) = write!(self.writer, "{v}") {
                self.error = Some(e);
            }
        }
    }

    #[inline]
    fn u128(&mut self, v: u128) {
        if self.error.is_none() {
            if let Err(e) = write!(self.writer, "{v}") {
                self.error = Some(e);
            }
        }
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
        self.write(flat.as_bytes());
    }

    #[inline]
    fn checkpoint(&mut self) -> Self::Checkpoint {
        0
    }

    #[inline]
    fn restore(&mut self, _cp: Self::Checkpoint) {}
}
