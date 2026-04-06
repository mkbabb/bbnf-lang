//! Grammar-guided emission traits and compact sink implementation.
//!
//! `EmitSink` is the inverse of parsing: where a grammar-derived parser
//! reads text into typed values, an `EmitSink`-based emitter walks those
//! values and writes text back out. The trait is grammar-agnostic — codegen
//! produces the traversal, the sink determines the output format.

use std::io::{self, Write};

/// Strategy trait for grammar-guided emission.
///
/// Grammar-generated code calls these methods to emit structured output.
/// Implementations control the output format:
/// - [`CompactSink`]: minimal bytes, no formatting (serde_json::to_string equivalent)
/// - [`StringSink`]: compact emit into an owned `String` with checkpoint/restore
/// - `DocSink` (in pprint): pretty-printed via Wadler-Lindig algorithm
///
/// Primitive methods are the minimal vocabulary. Composite methods have default
/// implementations that decompose into primitives — specialized sinks (pprint)
/// override them with fused operations.
pub trait EmitSink<'a> {
    /// Opaque checkpoint for speculative emission / backtracking.
    type Checkpoint: Copy;

    // ── Content primitives ───────────────────────────────────────────

    /// Emit a borrowed string slice.
    fn text(&mut self, s: &'a str);

    /// Emit a single ASCII byte.
    fn char(&mut self, c: u8);

    /// Emit text that may contain inline whitespace (spaces/tabs).
    /// Sinks that track line width should count only the non-ws content.
    fn text_inline_ws(&mut self, s: &'a str);

    /// Emit a signed 64-bit integer.
    fn i64(&mut self, v: i64);

    /// Emit an unsigned 64-bit integer.
    fn u64(&mut self, v: u64);

    /// Emit a 64-bit float (ryu fast path recommended).
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

    /// Open a formatting group. Content inside may be laid out flat or broken.
    fn group_open(&mut self);

    /// Close the current formatting group.
    fn group_close(&mut self);

    /// Increase indentation level.
    fn indent_open(&mut self);

    /// Decrease indentation level.
    fn indent_close(&mut self);

    // ── Separators ───────────────────────────────────────────────────

    /// Emit a separator: `flat` variant in flat mode, `brk` variant + newline in break mode.
    fn sep(&mut self, flat: &str, brk: &str);

    // ── Backtracking ─────────────────────────────────────────────────

    /// Save current emission state for potential rollback.
    fn checkpoint(&mut self) -> Self::Checkpoint;

    /// Restore emission state to a previous checkpoint, discarding output since then.
    fn restore(&mut self, cp: Self::Checkpoint);

    // ── Composites (default decompositions) ──────────────────────────
    //
    // These have default implementations that decompose into primitives.
    // pprint overrides with fused 24-byte ops (WrapStart/WrapEnd/CommaSep).
    // CompactSink gets correct behavior via the defaults (char + no-ops).

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

    /// Emit standard comma separator: `", "` flat, `","` + newline when broken.
    fn comma_sep(&mut self) {
        self.sep(", ", ",");
    }
}

// ── CompactSink ──────────────────────────────────────────────────────────

/// Minimal-bytes emitter over [`Write`]. Writes content directly, ignores all
/// formatting (groups, indentation, line breaks).
pub struct CompactSink<W: Write> {
    writer: W,
    error: Option<io::Error>,
}

impl<W: Write> CompactSink<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            error: None,
        }
    }

    /// Consume the sink and return the underlying writer, or the first IO error.
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

impl<'a, W: Write> EmitSink<'a> for CompactSink<W> {
    type Checkpoint = u64;

    #[inline]
    fn text(&mut self, s: &'a str) {
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
        // itoa doesn't support i128; use fmt::Write.
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

// ── StringSink ───────────────────────────────────────────────────────────

/// Compact emitter into an owned `String`. Supports checkpoint/restore via
/// truncation. Use this for `emit_compact() -> String`.
pub struct StringSink {
    buf: String,
}

impl StringSink {
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

impl Default for StringSink {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> EmitSink<'a> for StringSink {
    type Checkpoint = usize;

    #[inline]
    fn text(&mut self, s: &'a str) {
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
