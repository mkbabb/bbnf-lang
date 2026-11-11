//! The flat CSS driver loop — emitted as the `CssFullParser` calling the
//! `css_balanced_component_scan` runtime primitive with grammar-derived STOP
//! args. This is the >SOTA-preserving shape: a flat dispatch loop, NOT a
//! per-rule recursive descent. The structure (stylesheet → at-rule /
//! qualified-rule → block → declaration) is the grammar's
//! `stylesheet`/`ruleItem`/`ruleBlock`/`declaration` rule shape, behaviorally
//! equal to the retired hand-written recognizer.

use crate::lower::css_scan::CssScanProgram;

/// Emit the `CssFullParser` recognizer. The scan calls the primitive
/// (`css_balanced_component_scan(bytes, pos, STOP, &SKIP_ALPHABET)`); the
/// drivers dispatch on the returned delimiter. STOP sets and the dispatch
/// bytes are the grammar-derived constants rendered in `mod.rs`.
pub(super) fn render_full_parser(_program: &CssScanProgram, out: &mut String) {
    out.push_str(
        r#"struct CssFullParser<'i, 'b, 'input> {
    bytes: &'i [u8],
    pos: usize,
    tape: &'b mut TapeBuilder<'input>,
}

impl<'i, 'b, 'input> CssFullParser<'i, 'b, 'input> {
    fn new(input: &'i str, tape: &'b mut TapeBuilder<'input>) -> Self {
        Self {
            bytes: input.as_bytes(),
            pos: 0,
            tape,
        }
    }

    /// Append a rule opening at `offset`; the at-rule branch sets the sparse
    /// `GRAMMAR_BIT0` flag (a `BackendRule` branch-tag projection).
    #[inline]
    fn push_rule(&mut self, offset: usize, at_rule: bool) {
        let flags = if at_rule {
            OffsetFlags::NONE.with(AT_RULE_FLAG)
        } else {
            OffsetFlags::NONE
        };
        self.tape.push_offset(offset, flags);
    }

    /// Append a declaration at its `:` offset; the source byte (`:`) is the
    /// lazy kind tag — no flag, no payload.
    #[inline]
    fn push_declaration(&mut self, colon: usize) {
        self.tape.push_plain_offset(colon);
    }

    /// The grammar-derived scan: skip the balanced run to the next STOP-set
    /// delimiter at the top level via the named primitive.
    #[inline]
    fn scan(&self, pos: usize, stop: &[u8]) -> Result<Option<(u8, usize)>, CssFactError> {
        css_balanced_component_scan(self.bytes, pos, stop, &SKIP_ALPHABET)
            .map_err(scan_error_into)
    }

    fn parse_stylesheet(mut self) -> Result<(), CssFactError> {
        loop {
            self.skip_ws_comments()?;
            if self.skip_top_level_legacy_marker() {
                continue;
            }
            if self.pos >= self.bytes.len() {
                return Ok(());
            }
            if self.bytes[self.pos] == b'@' {
                self.parse_at_rule()?;
            } else if self.bytes[self.pos] == TERMINATOR {
                self.pos += 1;
            } else {
                self.parse_qualified_rule()?;
            }
        }
    }

    fn parse_at_rule(&mut self) -> Result<(), CssFactError> {
        self.pos += 1;
        let name_start = self.pos;
        while self.pos < self.bytes.len() && is_name_byte(self.bytes[self.pos]) {
            if self.bytes[self.pos] == b'\\' {
                self.pos = self.consume_escape_at(self.pos)?;
            } else {
                self.pos += 1;
            }
        }
        if self.pos == name_start {
            return Err(css_full_error(name_start, "expected at-rule name"));
        }

        match self.scan(self.pos, AT_RULE_STOP)? {
            Some((b, end)) if b == TERMINATOR => {
                self.push_rule(end, true);
                self.pos = end + 1;
                Ok(())
            }
            Some((b, end)) if b == BLOCK_OPEN => {
                self.push_rule(end, true);
                self.pos = end + 1;
                self.parse_block()
            }
            Some((b, end)) if b == BLOCK_CLOSE => {
                Err(css_full_error(end, "unexpected closing delimiter"))
            }
            Some((_, end)) => Err(css_full_error(end, "expected at-rule terminator")),
            None => Err(css_full_error(self.pos, "expected at-rule terminator")),
        }
    }

    fn parse_qualified_rule(&mut self) -> Result<(), CssFactError> {
        let start = self.pos;
        match self.scan(self.pos, QUALIFIED_STOP)? {
            Some((b, end)) if b == BLOCK_OPEN && has_non_ws(self.bytes, start, end) => {
                self.push_rule(end, false);
                self.pos = end + 1;
                self.parse_block()
            }
            Some((b, end)) if b == TERMINATOR && !has_non_ws(self.bytes, start, end) => {
                self.pos = end + 1;
                Ok(())
            }
            Some((b, end)) if b == BLOCK_CLOSE => {
                Err(css_full_error(end, "unexpected closing delimiter"))
            }
            Some((_, end)) => Err(css_full_error(end, "expected rule block")),
            None => Err(css_full_error(start, "expected rule block")),
        }
    }

    fn parse_block(&mut self) -> Result<(), CssFactError> {
        loop {
            self.skip_ws_comments()?;
            if self.pos >= self.bytes.len() {
                return Err(css_full_error(self.pos, "unclosed block"));
            }
            if self.bytes[self.pos] == BLOCK_CLOSE {
                self.pos += 1;
                return Ok(());
            }
            if self.bytes[self.pos] == b'@' {
                self.parse_at_rule()?;
            } else if self.bytes[self.pos] == TERMINATOR {
                self.pos += 1;
            } else {
                self.parse_block_item()?;
            }
        }
    }

    fn parse_block_item(&mut self) -> Result<(), CssFactError> {
        let start = self.pos;
        match self.scan(self.pos, QUALIFIED_STOP)? {
            Some((b, end)) if b == BLOCK_OPEN && has_non_ws(self.bytes, start, end) => {
                self.push_rule(end, false);
                self.pos = end + 1;
                self.parse_block()
            }
            Some((b, end)) if b == TERMINATOR => {
                if let Some(colon) = self.find_colon_before(start, end)? {
                    self.parse_declaration(start, colon)
                } else if !has_non_ws(self.bytes, start, end) {
                    self.pos = end + 1;
                    Ok(())
                } else {
                    Err(css_full_error(end, "expected declaration or nested rule"))
                }
            }
            Some((b, end)) if b == BLOCK_CLOSE => {
                if let Some(colon) = self.find_colon_before(start, end)? {
                    self.parse_declaration(start, colon)
                } else if !has_non_ws(self.bytes, start, end) {
                    Ok(())
                } else {
                    Err(css_full_error(end, "expected declaration or nested rule"))
                }
            }
            Some((_, end)) => Err(css_full_error(end, "expected declaration or nested rule")),
            None => Err(css_full_error(start, "unclosed block")),
        }
    }

    fn parse_declaration(&mut self, start: usize, colon: usize) -> Result<(), CssFactError> {
        if !has_non_ws(self.bytes, start, colon) {
            return Err(css_full_error(start, "expected declaration name"));
        }
        self.pos = colon + 1;
        match self.scan(self.pos, DECLARATION_STOP)? {
            Some((b, end)) if b == TERMINATOR => {
                self.push_declaration(colon);
                self.pos = end + 1;
                Ok(())
            }
            Some((b, end)) if b == BLOCK_CLOSE => {
                self.push_declaration(colon);
                self.pos = end;
                Ok(())
            }
            Some((_, end)) => Err(css_full_error(end, "expected declaration terminator")),
            None => Err(css_full_error(self.pos, "unclosed block")),
        }
    }

    fn skip_ws_comments(&mut self) -> Result<(), CssFactError> {
        loop {
            while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }
            if self.starts_with_at(self.pos, b"/*") {
                self.pos = self.consume_comment_at(self.pos)?;
                continue;
            }
            return Ok(());
        }
    }

    fn skip_top_level_legacy_marker(&mut self) -> bool {
        if self.starts_with_at(self.pos, b"<!--") {
            self.pos += 4;
            true
        } else if self.starts_with_at(self.pos, b"-->") {
            self.pos += 3;
            true
        } else if self.skip_charset_directive() {
            true
        } else {
            false
        }
    }

    /// `@charset "..."` is a tokenizer directive, not a counted at-rule. At the
    /// top level it is skipped so the structural counts match the reference
    /// CSSOM — the same class as the CDO/CDC markers above.
    #[inline]
    fn skip_charset_directive(&mut self) -> bool {
        if !self.starts_with_at(self.pos, b"@charset") {
            return false;
        }
        let after = self.pos + b"@charset".len();
        match self.bytes.get(after) {
            Some(byte) if byte.is_ascii_whitespace() => {}
            _ => return false,
        }
        let mut pos = after;
        while pos < self.bytes.len()
            && self.bytes[pos] != TERMINATOR
            && self.bytes[pos] != BLOCK_OPEN
        {
            pos += 1;
        }
        if pos < self.bytes.len() && self.bytes[pos] == TERMINATOR {
            self.pos = pos + 1;
            true
        } else {
            false
        }
    }

    /// Find the declaration colon before `end` via the primitive — the
    /// `:`-led STOP set with a `{`/`}`/`;` bound.
    fn find_colon_before(&self, start: usize, end: usize) -> Result<Option<usize>, CssFactError> {
        match self.scan(start, COLON_BEFORE_STOP)? {
            Some((b, colon)) if b == COLON && colon < end => Ok(Some(colon)),
            _ => Ok(None),
        }
    }

    fn consume_comment_at(&self, start: usize) -> Result<usize, CssFactError> {
        let mut pos = start + 2;
        while pos + 1 < self.bytes.len() {
            if self.bytes[pos] == b'*' && self.bytes[pos + 1] == b'/' {
                return Ok(pos + 2);
            }
            pos += 1;
        }
        Err(css_full_error(start, "unclosed comment"))
    }

    fn consume_escape_at(&self, start: usize) -> Result<usize, CssFactError> {
        if start + 1 >= self.bytes.len() {
            return Err(css_full_error(start, "unterminated escape"));
        }
        Ok(start + 2)
    }

    fn starts_with_at(&self, pos: usize, needle: &[u8]) -> bool {
        self.bytes
            .get(pos..pos.saturating_add(needle.len()))
            .is_some_and(|bytes| bytes == needle)
    }
}

fn has_non_ws(bytes: &[u8], start: usize, end: usize) -> bool {
    bytes[start..end].iter().any(|byte| !byte.is_ascii_whitespace())
}

fn is_name_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'\\') || byte >= 0x80
}

fn css_full_error(offset: usize, message: &'static str) -> CssFactError {
    CssFactError { offset, message }
}

fn fnv64(bytes: &[u8]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn push_hex64(out: &mut String, value: u64) {
    out.push_str(&format!("{value:016x}"));
}

"#,
    );
}
