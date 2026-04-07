//! Grammar-guided serialization and deserialization traits.
//!
//! `Serializer<'a>` is the inverse of parsing: where a grammar-derived parser
//! reads text into typed values, a serializer walks those values and writes
//! text back out. The trait is grammar-agnostic — codegen produces the
//! traversal, the serializer controls the output format.
//!
//! `Deserializer<'a>` is the inverse of serialization: type-guided reading
//! of structured text back into typed values.
//!
//! ## Implementations
//!
//! - [`WriterSerializer`]: minimal bytes to any `io::Write`
//! - [`StringSerializer`]: compact output into owned `String`
//! - `FmtBuilder` (in pprint): pretty-printed via Wadler-Lindig algorithm
//! - [`SliceDeserializer`]: reads from `&'a str` input

use std::io::{self, Write};

/// Strategy trait for grammar-guided serialization.
///
/// Two text paths: [`text()`](Serializer::text) for zero-copy borrowed input
/// (the 99% path), [`text_owned()`](Serializer::text_owned) for computed
/// values (hex, Display fallbacks) where the caller constructs a local string.
pub trait Serializer<'a> {
    /// Opaque checkpoint for speculative emission / backtracking.
    type Checkpoint: Copy;

    // ── Content primitives ───────────────────────────────────────────

    /// Emit a borrowed string slice from the input (zero-copy path).
    fn text(&mut self, s: &'a str);

    /// Emit a computed string (hex formatting, Display, etc.).
    /// The serializer copies/writes immediately — no borrowing.
    fn text_owned(&mut self, s: &str);

    /// Emit a single ASCII byte.
    fn char(&mut self, c: u8);

    /// Emit text that may contain inline whitespace (spaces/tabs).
    fn text_inline_ws(&mut self, s: &'a str);

    /// Emit a boolean value.
    fn bool(&mut self, v: bool);

    /// Emit a signed 64-bit integer.
    fn i64(&mut self, v: i64);

    /// Emit an unsigned 64-bit integer.
    fn u64(&mut self, v: u64);

    /// Emit a 64-bit float.
    fn f64(&mut self, v: f64);

    /// Emit a signed 128-bit integer.
    fn i128(&mut self, v: i128);

    /// Emit an unsigned 128-bit integer.
    fn u128(&mut self, v: u128);

    // ── Line control ─────────────────────────────────────────────────

    /// Unconditional line break + indent.
    fn hardline(&mut self);

    /// Line break + indent in break mode; space in flat mode.
    fn softline(&mut self);

    /// Line break + indent in break mode; nothing in flat mode.
    fn break_line(&mut self);

    // ── Structure ────────────────────────────────────────────────────

    /// Open a formatting group.
    fn group_open(&mut self);

    /// Close the current formatting group.
    fn group_close(&mut self);

    /// Increase indentation level.
    fn indent_open(&mut self);

    /// Decrease indentation level.
    fn indent_close(&mut self);

    // ── Separators ───────────────────────────────────────────────────

    /// Emit a separator: `flat` in flat mode, `brk` + newline in break mode.
    fn sep(&mut self, flat: &str, brk: &str);

    // ── Backtracking ─────────────────────────────────────────────────

    /// Save current emission state for potential rollback.
    fn checkpoint(&mut self) -> Self::Checkpoint;

    /// Restore emission state to a previous checkpoint.
    fn restore(&mut self, cp: Self::Checkpoint);

    // ── Composites (default decompositions) ──────────────────────────

    /// Open a delimiter-wrapped group: group + open char + indent + break.
    fn wrap_open(&mut self, open: u8) {
        self.group_open();
        self.char(open);
        self.indent_open();
        self.break_line();
    }

    /// Close a delimiter-wrapped group: dedent + break + close char + end group.
    fn wrap_close(&mut self, close: u8) {
        self.indent_close();
        self.break_line();
        self.char(close);
        self.group_close();
    }

    /// Emit standard comma separator.
    fn comma_sep(&mut self) {
        self.sep(", ", ",");
    }
}

/// Type-guided deserialization. Inverse of [`Serializer`].
pub trait Deserializer<'a> {
    /// Opaque checkpoint for speculative reads.
    type Checkpoint: Copy;

    fn text_exact(&mut self, s: &str) -> bool;
    fn text_span(&mut self) -> Option<&'a str>;
    fn char_exact(&mut self, c: u8) -> bool;
    fn skip_ws(&mut self);
    fn i64(&mut self) -> Option<i64>;
    fn u64(&mut self) -> Option<u64>;
    fn f64(&mut self) -> Option<f64>;
    fn peek_byte(&self) -> Option<u8>;
    fn at_eof(&self) -> bool;
    fn offset(&self) -> usize;
    fn checkpoint(&mut self) -> Self::Checkpoint;
    fn restore(&mut self, cp: Self::Checkpoint);
}

// ── WriterSerializer ───────────────────────────────────────────────────

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
        use std::io::Write as _;
        if self.error.is_none() {
            if let Err(e) = write!(self.writer, "{v}") {
                self.error = Some(e);
            }
        }
    }

    #[inline]
    fn u128(&mut self, v: u128) {
        use std::io::Write as _;
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

// ── StringSerializer ───────────────────────────────────────────────────

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

// ── SliceDeserializer ──────────────────────────────────────────────────

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
