//! Core serialization/deserialization trait definitions.
//!
//! [`Serializer`] is the inverse of parsing: where a grammar-derived parser
//! reads text into typed values, a serializer walks those values and writes
//! text back out. [`Deserializer`] is the inverse of serialization:
//! type-guided reading of structured text back into typed values.

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
