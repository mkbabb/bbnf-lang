//! AZ-II.cutover.G — hand-written BBNF bootstrap parser.
//!
//! Breaks the chicken-and-egg between `bbnf::pipeline::compile_paths_request`
//! and `bbnf::grammar::generated::bbnf::BbnfBootstrap::parse`. The
//! generated `bbnf.rs` produced before cutover.F's emitter fixes
//! rejected every input at offset 0 (see `audit/cutover.F-PARTIAL.md`
//! Discovery 1); the cutover.F emitter fixes cannot land in the on-
//! disk file because regen requires a working `BbnfBootstrap::parse`.
//!
//! This module is the bypass: a hand-written recursive-descent
//! parser that consumes BBNF source and emits a [`BbnfDocument`]
//! directly via [`BbnfStructBuilder`]. With this parser routed at
//! `crate::grammar::parse` and `crate::pipeline::directives::parse_to_pipeline_inputs`
//! the regen pipeline runs to completion and the cutover.F-fixed
//! emitter produces a clean StructDirect `bbnf.rs`.
//!
//! The parser covers the full BBNF surface plus the `expressions.bbnf`
//! and `types.bbnf` imports — every rule the self-hosted grammar
//! uses. Keep-or-retire policy after cutover.G is deferred to
//! cutover.H or BA.W0.

use bbnf_ir::registry::{LayoutKind, StructLayout};
use bbnf_ir::TypeDesc;

use crate::runtime::ParseErr;
use crate::runtime::bbnf::{BbnfDocument, BbnfStructBuilder};
use crate::runtime::builder::StructBuilder;

/// Parser state. Owns a byte cursor over the input plus a builder
/// accumulating the typed document.
struct Parser<'p> {
    input: &'p [u8],
    src: &'p str,
    pos: usize,
    builder: BbnfStructBuilder<'p>,
}

impl<'p> Parser<'p> {
    fn new(src: &'p str) -> Self {
        Self {
            input: src.as_bytes(),
            src,
            pos: 0,
            builder: BbnfStructBuilder::new(),
        }
    }

    fn at(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn at_offset(&self, off: usize) -> Option<u8> {
        self.input.get(self.pos + off).copied()
    }

    fn peek_str(&self, s: &str) -> bool {
        let bytes = s.as_bytes();
        self.input.len() >= self.pos + bytes.len()
            && &self.input[self.pos..self.pos + bytes.len()] == bytes
    }

    fn eat_str(&mut self, s: &str) -> bool {
        if self.peek_str(s) {
            self.pos += s.len();
            true
        } else {
            false
        }
    }

    fn eat_byte(&mut self, b: u8) -> bool {
        if self.at() == Some(b) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn err(&self) -> ParseErr {
        ParseErr::Syntax {
            offset: self.pos as u32,
            rule: None,
        }
    }

    /// Skip whitespace + line comments. Block comments are NOT
    /// skipped here — they are real grammar items.
    fn skip_ws(&mut self) {
        loop {
            match self.at() {
                Some(b) if b.is_ascii_whitespace() => {
                    self.pos += 1;
                }
                Some(b'/') if self.at_offset(1) == Some(b'/') => {
                    self.pos += 2;
                    while let Some(b) = self.at() {
                        if b == b'\n' {
                            self.pos += 1;
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }

    /// Open a compound on the builder with the given rule name.
    fn begin(&mut self, rule_name: &str) -> crate::runtime::handle::CompoundHandle {
        let layout = StructLayout {
            rule_id: 0u32 as bbnf_ir::RuleId,
            rule_name: rule_name.to_string(),
            kind: LayoutKind::Struct,
            rule_type: TypeDesc::Span,
            fields: Vec::new(),
        };
        self.builder.begin_compound(&layout)
    }

    fn end(&mut self, handle: crate::runtime::handle::CompoundHandle) {
        self.builder.end_compound(handle);
    }

    fn push_span(&mut self, lo: usize, hi: usize) {
        let s = &self.src[lo..hi];
        self.builder.push_leaf_with_str(s);
    }

    /// `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span`
    /// Note BBNF identifiers admit '-' (kebab-case).
    fn parse_identifier_span(&mut self) -> Result<(usize, usize), ParseErr> {
        let lo = self.pos;
        match self.at() {
            Some(b) if b == b'_' || b.is_ascii_alphabetic() => self.pos += 1,
            _ => return Err(self.err()),
        }
        while let Some(b) = self.at() {
            if b == b'_' || b == b'-' || b.is_ascii_alphanumeric() {
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok((lo, self.pos))
    }

    fn parse_identifier(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("identifier");
        let (lo, hi) = self.parse_identifier_span()?;
        self.push_span(lo, hi);
        self.end(h);
        Ok(())
    }

    /// `value_ident = /[_a-zA-Z][_a-zA-Z0-9]*/` (NO '-' admitted here).
    fn parse_value_ident_span(&mut self) -> Result<(usize, usize), ParseErr> {
        let lo = self.pos;
        match self.at() {
            Some(b) if b == b'_' || b.is_ascii_alphabetic() => self.pos += 1,
            _ => return Err(self.err()),
        }
        while let Some(b) = self.at() {
            if b == b'_' || b.is_ascii_alphanumeric() {
                self.pos += 1;
            } else {
                break;
            }
        }
        Ok((lo, self.pos))
    }

    /// `literal = ( "\"" , /(\\.|[^"\\])*/ , "\"" | "'" , … | "`" , … ) -> Span`
    fn parse_literal(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("literal");
        let lo = self.pos;
        let quote = match self.at() {
            Some(b @ (b'"' | b'\'' | b'`')) => b,
            _ => {
                self.end(h);
                return Err(self.err());
            }
        };
        self.pos += 1;
        while let Some(b) = self.at() {
            if b == b'\\' {
                self.pos += 2;
                continue;
            }
            if b == quote {
                self.pos += 1;
                let hi = self.pos;
                self.push_span(lo, hi);
                self.end(h);
                return Ok(());
            }
            self.pos += 1;
        }
        self.end(h);
        Err(self.err())
    }

    /// `regex = ( "/" , /(\\.|[^\/])+/ , "/" ) -> Span`
    /// The body is at least one character.
    fn parse_regex(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("regex");
        let lo = self.pos;
        if !self.eat_byte(b'/') {
            self.end(h);
            return Err(self.err());
        }
        let body_start = self.pos;
        loop {
            match self.at() {
                Some(b'\\') => self.pos += 2,
                Some(b'/') => break,
                Some(_) => self.pos += 1,
                None => {
                    self.end(h);
                    return Err(self.err());
                }
            }
        }
        if self.pos == body_start {
            // Empty regex body — not valid BBNF.
            self.end(h);
            return Err(self.err());
        }
        if !self.eat_byte(b'/') {
            self.end(h);
            return Err(self.err());
        }
        let hi = self.pos;
        self.push_span(lo, hi);
        self.end(h);
        Ok(())
    }

    /// `big_comment = ( "/*" , /[^\*]*/ , "*/" ) ?w -> Span`
    fn parse_big_comment(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("big_comment");
        let lo = self.pos;
        if !self.peek_str("/*") {
            self.end(h);
            return Err(self.err());
        }
        self.pos += 2;
        loop {
            if self.peek_str("*/") {
                self.pos += 2;
                let hi = self.pos;
                self.push_span(lo, hi);
                self.skip_ws();
                self.end(h);
                return Ok(());
            }
            if self.at().is_none() {
                self.end(h);
                return Err(self.err());
            }
            self.pos += 1;
        }
    }

    /// `comment = ( "//" , /.*/ ) ?w -> Span`
    fn parse_comment(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("comment");
        let lo = self.pos;
        if !self.peek_str("//") {
            self.end(h);
            return Err(self.err());
        }
        self.pos += 2;
        while let Some(b) = self.at() {
            if b == b'\n' {
                break;
            }
            self.pos += 1;
        }
        let hi = self.pos;
        self.push_span(lo, hi);
        self.skip_ws();
        self.end(h);
        Ok(())
    }

    /// `lhs = identifier`
    fn parse_lhs(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("lhs");
        self.parse_identifier()?;
        self.end(h);
        Ok(())
    }

    /// `term = "ε" | "epsilon"
    ///       | identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?
    ///       | literal | regex
    ///       | "@{" , rhs ?w , "}"
    ///       | "(" , rhs ?w , ")"
    ///       | "[" , rhs ?w , "]"
    ///       | "{" , rhs ?w , "}"`
    fn parse_term(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("term");
        // ε literal — UTF-8 0xCE 0xB5
        if self.input.get(self.pos) == Some(&0xCE) && self.input.get(self.pos + 1) == Some(&0xB5) {
            let lo = self.pos;
            self.pos += 2;
            self.push_span(lo, self.pos);
            self.end(h);
            return Ok(());
        }
        if self.peek_str("epsilon") {
            let lo = self.pos;
            self.pos += "epsilon".len();
            self.push_span(lo, self.pos);
            self.end(h);
            return Ok(());
        }
        match self.at() {
            Some(b'"' | b'\'' | b'`') => {
                self.parse_literal()?;
                self.end(h);
                Ok(())
            }
            Some(b'/') => {
                self.parse_regex()?;
                self.end(h);
                Ok(())
            }
            Some(b'@') if self.peek_str("@{") => {
                self.pos += 2;
                self.skip_ws();
                self.parse_rhs()?;
                self.skip_ws();
                if !self.eat_byte(b'}') {
                    self.end(h);
                    return Err(self.err());
                }
                self.end(h);
                Ok(())
            }
            Some(b'(') => {
                self.pos += 1;
                self.skip_ws();
                self.parse_rhs()?;
                self.skip_ws();
                if !self.eat_byte(b')') {
                    self.end(h);
                    return Err(self.err());
                }
                self.end(h);
                Ok(())
            }
            Some(b'[') => {
                self.pos += 1;
                self.skip_ws();
                self.parse_rhs()?;
                self.skip_ws();
                if !self.eat_byte(b']') {
                    self.end(h);
                    return Err(self.err());
                }
                self.end(h);
                Ok(())
            }
            Some(b'{') => {
                self.pos += 1;
                self.skip_ws();
                self.parse_rhs()?;
                self.skip_ws();
                if !self.eat_byte(b'}') {
                    self.end(h);
                    return Err(self.err());
                }
                self.end(h);
                Ok(())
            }
            Some(b) if b == b'_' || b.is_ascii_alphabetic() => {
                self.parse_identifier()?;
                // Optional grammar function call: ( ... )
                if self.at() == Some(b'(') {
                    self.pos += 1;
                    self.skip_ws();
                    if self.at() != Some(b')') {
                        self.parse_call_arg()?;
                        self.skip_ws();
                        while self.eat_byte(b',') {
                            self.skip_ws();
                            self.parse_call_arg()?;
                            self.skip_ws();
                        }
                    }
                    if !self.eat_byte(b')') {
                        self.end(h);
                        return Err(self.err());
                    }
                }
                self.end(h);
                Ok(())
            }
            _ => {
                self.end(h);
                Err(self.err())
            }
        }
    }

    /// `modifier = "?w" | "?" | "*" | "+"`
    fn parse_modifier(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("modifier");
        let lo = self.pos;
        let ok = if self.peek_str("?w") {
            self.pos += 2;
            true
        } else if matches!(self.at(), Some(b'?' | b'*' | b'+')) {
            self.pos += 1;
            true
        } else {
            false
        };
        if !ok {
            self.end(h);
            return Err(self.err());
        }
        self.push_span(lo, self.pos);
        self.end(h);
        Ok(())
    }

    /// `factor = big_comment ? , term ?w , modifier ? , big_comment ?`
    fn parse_factor(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("factor");
        // Optional leading big_comment.
        if self.peek_str("/*") {
            self.parse_big_comment()?;
        }
        self.skip_ws();
        self.parse_term()?;
        self.skip_ws();
        // Optional modifier.
        if self.peek_str("?w") || matches!(self.at(), Some(b'?' | b'*' | b'+')) {
            // Disambiguate "?w" vs literal "?": both are modifiers
            // here.
            self.parse_modifier()?;
        }
        self.skip_ws();
        // Optional trailing big_comment.
        if self.peek_str("/*") {
            self.parse_big_comment()?;
        }
        self.end(h);
        Ok(())
    }

    /// `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ?`
    fn parse_mapped_factor(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("mapped_factor");
        self.parse_factor()?;
        self.skip_ws();
        if self.peek_str("->") {
            self.pos += 2;
            self.skip_ws();
            self.parse_value_expr()?;
            self.skip_ws();
            if self.at() == Some(b':') {
                self.parse_type_annotation()?;
            }
        }
        self.end(h);
        Ok(())
    }

    /// `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *`
    /// `binary_operators = "<<" | ">>" | "-"`
    fn parse_binary_factor(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("binary_factor");
        self.parse_mapped_factor()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            let op_ok = if self.peek_str("<<") || self.peek_str(">>") {
                self.pos += 2;
                true
            } else if self.at() == Some(b'-')
                // "-" must not be followed by ">" (that's the map arrow)
                && self.at_offset(1) != Some(b'>')
            {
                self.pos += 1;
                true
            } else {
                false
            };
            if !op_ok {
                self.pos = save;
                break;
            }
            self.skip_ws();
            self.parse_mapped_factor()?;
        }
        self.end(h);
        Ok(())
    }

    /// `concatenation = ( binary_factor ?w , "," ? ) +`
    fn parse_concatenation(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("concatenation");
        self.parse_binary_factor()?;
        self.skip_ws();
        let _ = self.eat_byte(b',');
        loop {
            let save = self.pos;
            self.skip_ws();
            // Stop if we see end-of-concatenation tokens.
            match self.at() {
                None | Some(b';' | b'.' | b'|' | b')' | b']' | b'}') => {
                    self.pos = save;
                    break;
                }
                _ => {}
            }
            // Try to parse another binary_factor.
            let attempt = self.pos;
            match self.parse_binary_factor() {
                Ok(()) => {
                    self.skip_ws();
                    let _ = self.eat_byte(b',');
                }
                Err(_) => {
                    self.pos = attempt;
                    break;
                }
            }
        }
        self.end(h);
        Ok(())
    }

    /// `alternation = ( concatenation ?w , "|" ? ) +`
    fn parse_alternation(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("alternation");
        self.parse_concatenation()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if self.at() != Some(b'|') {
                self.pos = save;
                break;
            }
            // "|" alone — separator. Followed by another concatenation.
            self.pos += 1;
            self.skip_ws();
            match self.at() {
                None | Some(b';' | b'.' | b')' | b']' | b'}') => {
                    // Trailing "|" allowed by grammar's ( "|" ?) ?
                    break;
                }
                _ => {
                    self.parse_concatenation()?;
                }
            }
        }
        self.end(h);
        Ok(())
    }

    /// `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`
    fn parse_closure(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("closure");
        if !self.eat_byte(b'|') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_identifier()?;
        loop {
            self.skip_ws();
            if !self.eat_byte(b',') {
                break;
            }
            self.skip_ws();
            self.parse_identifier()?;
        }
        self.skip_ws();
        if !self.eat_byte(b'|') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_rhs()?;
        self.end(h);
        Ok(())
    }

    /// `rhs = closure | alternation`
    fn parse_rhs(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("rhs");
        // closure begins with "|" as its first non-ws byte. But "|"
        // also appears in alternation as a separator. Disambiguate
        // by lookahead: closure starts with "|" followed by an
        // identifier-start-char.
        let save = self.pos;
        if self.at() == Some(b'|') {
            // Lookahead: skip "|" + ws, check if next is identifier.
            let try_p = self.pos + 1;
            let mut probe = try_p;
            while probe < self.input.len() && self.input[probe].is_ascii_whitespace() {
                probe += 1;
            }
            if probe < self.input.len()
                && (self.input[probe] == b'_' || self.input[probe].is_ascii_alphabetic())
            {
                // Looks like a closure.
                match self.parse_closure() {
                    Ok(()) => {
                        self.end(h);
                        return Ok(());
                    }
                    Err(_) => {
                        self.pos = save;
                    }
                }
            }
        }
        self.parse_alternation()?;
        self.end(h);
        Ok(())
    }

    /// `call_arg = ( binary_factor ?w , "|" ? ) +`
    fn parse_call_arg(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("call_arg");
        self.parse_binary_factor()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if !self.eat_byte(b'|') {
                self.pos = save;
                break;
            }
            self.skip_ws();
            match self.at() {
                None | Some(b',' | b')' | b';' | b'.') => break,
                _ => self.parse_binary_factor()?,
            }
        }
        self.end(h);
        Ok(())
    }

    /// `rule = lhs , "=" ?w , rhs ?w , ( ";" | "." )`
    fn parse_rule(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("rule");
        self.parse_lhs()?;
        self.skip_ws();
        if !self.eat_byte(b'=') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_rhs()?;
        self.skip_ws();
        if !(self.eat_byte(b';') || self.eat_byte(b'.')) {
            self.end(h);
            return Err(self.err());
        }
        self.end(h);
        Ok(())
    }

    /// `import_path = "\"" , /…/ , "\""`
    fn parse_import_path(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("import_path");
        let lo = self.pos;
        if !self.eat_byte(b'"') {
            self.end(h);
            return Err(self.err());
        }
        loop {
            match self.at() {
                Some(b'\\') => self.pos += 2,
                Some(b'"') => {
                    self.pos += 1;
                    break;
                }
                Some(_) => self.pos += 1,
                None => {
                    self.end(h);
                    return Err(self.err());
                }
            }
        }
        self.push_span(lo, self.pos);
        self.end(h);
        Ok(())
    }

    /// `import_items = "{" ?w , ( identifier , ( "," ?w , identifier ) * ) ?w , "}"`
    fn parse_import_items(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("import_items");
        if !self.eat_byte(b'{') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        if self.at() != Some(b'}') {
            self.parse_identifier()?;
            loop {
                self.skip_ws();
                if !self.eat_byte(b',') {
                    break;
                }
                self.skip_ws();
                self.parse_identifier()?;
            }
        }
        self.skip_ws();
        if !self.eat_byte(b'}') {
            self.end(h);
            return Err(self.err());
        }
        self.end(h);
        Ok(())
    }

    /// `import_directive = "@import" ?w , ( import_items ?w , "from" ?w , import_path | import_path ) ?w , ( ";" | "." ) ?`
    fn parse_import_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("import_directive");
        if !self.eat_str("@import") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        if self.at() == Some(b'{') {
            self.parse_import_items()?;
            self.skip_ws();
            if !self.eat_str("from") {
                self.end(h);
                return Err(self.err());
            }
            self.skip_ws();
            self.parse_import_path()?;
        } else {
            self.parse_import_path()?;
        }
        self.skip_ws();
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `recover_directive = "@recover" ?w , identifier ?w , rhs ?w , ( ";" | "." ) ?`
    fn parse_recover_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("recover_directive");
        if !self.eat_str("@recover") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_identifier()?;
        self.skip_ws();
        self.parse_rhs()?;
        self.skip_ws();
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `pretty_hint = identifier , ( "(" , /[^)]*/ , ")" ) ?`
    fn parse_pretty_hint(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("pretty_hint");
        self.parse_identifier()?;
        if self.at() == Some(b'(') {
            self.pos += 1;
            while let Some(b) = self.at() {
                if b == b')' {
                    self.pos += 1;
                    break;
                }
                self.pos += 1;
            }
        }
        self.end(h);
        Ok(())
    }

    /// `pretty_directive = "@pretty" ?w , ( "*" | identifier ) ?w , ( pretty_hint ?w ) + , ( ";" | "." ) ?`
    fn parse_pretty_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("pretty_directive");
        if !self.eat_str("@pretty") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        if self.at() == Some(b'*') {
            let lo = self.pos;
            self.pos += 1;
            // Push as a span so consumers see "*".
            let inner = self.begin("identifier");
            self.push_span(lo, self.pos);
            self.end(inner);
        } else {
            self.parse_identifier()?;
        }
        self.skip_ws();
        // (pretty_hint ?w) +
        self.parse_pretty_hint()?;
        loop {
            self.skip_ws();
            // Stop on terminator.
            if matches!(self.at(), None | Some(b';' | b'.')) {
                break;
            }
            // Stop on next directive / rule.
            if self.at() == Some(b'@') {
                break;
            }
            // Try another pretty_hint.
            let save = self.pos;
            match self.parse_pretty_hint() {
                Ok(()) => {}
                Err(_) => {
                    self.pos = save;
                    break;
                }
            }
        }
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `ws_directive = "@ws" ?w , regex ?w , ( ";" | "." ) ?`
    fn parse_ws_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("ws_directive");
        if !self.eat_str("@ws") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_regex()?;
        self.skip_ws();
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `token_directive = "@token" ?w , identifier ?w , ( ";" | "." ) ?`
    fn parse_token_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("token_directive");
        if !self.eat_str("@token") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_identifier()?;
        self.skip_ws();
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `debug_directive = "@debug" ?w , ( "*" | identifier ) ?w , ( ";" | "." ) ?`
    fn parse_debug_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("debug_directive");
        if !self.eat_str("@debug") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        if self.at() == Some(b'*') {
            let lo = self.pos;
            self.pos += 1;
            let inner = self.begin("identifier");
            self.push_span(lo, self.pos);
            self.end(inner);
        } else if matches!(self.at(), Some(b) if b == b'_' || b.is_ascii_alphabetic()) {
            self.parse_identifier()?;
        }
        self.skip_ws();
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `host_directive = "@host" ?w , identifier ?w , ( ":" ?w , type_name ?w ) ? , ( ";" | "." ) ?`
    fn parse_host_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("host_directive");
        if !self.eat_str("@host") {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_identifier()?;
        self.skip_ws();
        if self.eat_byte(b':') {
            self.skip_ws();
            self.parse_type_name()?;
            self.skip_ws();
        }
        let _ = self.eat_byte(b';') || self.eat_byte(b'.');
        self.end(h);
        Ok(())
    }

    /// `directive = import_directive | recover_directive | pretty_directive
    ///            | ws_directive | token_directive | debug_directive | host_directive`
    fn parse_directive(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("directive");
        let mut tag = 0u32;
        if self.peek_str("@import") {
            self.parse_import_directive()?;
        } else if self.peek_str("@recover") {
            tag = 1;
            self.parse_recover_directive()?;
        } else if self.peek_str("@pretty") {
            tag = 2;
            self.parse_pretty_directive()?;
        } else if self.peek_str("@ws") {
            tag = 3;
            self.parse_ws_directive()?;
        } else if self.peek_str("@token") {
            tag = 4;
            self.parse_token_directive()?;
        } else if self.peek_str("@debug") {
            tag = 5;
            self.parse_debug_directive()?;
        } else if self.peek_str("@host") {
            tag = 6;
            self.parse_host_directive()?;
        } else {
            self.end(h);
            return Err(self.err());
        }
        self.builder.push_branch_tag(tag);
        self.end(h);
        Ok(())
    }

    /// `grammar_item = comment | big_comment | directive | rule`
    fn parse_grammar_item(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("grammar_item");
        let mut tag = 0u32;
        if self.peek_str("//") {
            self.parse_comment()?;
        } else if self.peek_str("/*") {
            tag = 1;
            self.parse_big_comment()?;
        } else if self.at() == Some(b'@') {
            tag = 2;
            self.parse_directive()?;
        } else {
            tag = 3;
            self.parse_rule()?;
        }
        self.builder.push_branch_tag(tag);
        self.end(h);
        Ok(())
    }

    /// `grammar = ( grammar_item ?w ) *`
    fn parse_grammar(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("grammar");
        loop {
            self.skip_ws();
            if self.at().is_none() {
                break;
            }
            let save = self.pos;
            match self.parse_grammar_item() {
                Ok(()) => {
                    if self.pos == save {
                        // Zero-width iteration — bail to avoid infinite loop.
                        break;
                    }
                }
                Err(e) => {
                    self.end(h);
                    return Err(e);
                }
            }
        }
        self.end(h);
        Ok(())
    }

    // ─── Value-expression sub-grammar ───────────────────────────────────

    /// `value_expr = value_closure | value_or`
    fn parse_value_expr(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_expr");
        if self.at() == Some(b'|') {
            // Try closure.
            let save = self.pos;
            match self.parse_value_closure() {
                Ok(()) => {
                    self.end(h);
                    return Ok(());
                }
                Err(_) => {
                    self.pos = save;
                }
            }
        }
        self.parse_value_or()?;
        self.end(h);
        Ok(())
    }

    /// `value_closure = "|" , value_ident , ( "," ?w , value_ident ) * , "|" , value_expr`
    fn parse_value_closure(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_closure");
        if !self.eat_byte(b'|') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_value_ident()?;
        loop {
            self.skip_ws();
            if !self.eat_byte(b',') {
                break;
            }
            self.skip_ws();
            self.parse_value_ident()?;
        }
        self.skip_ws();
        if !self.eat_byte(b'|') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_value_expr()?;
        self.end(h);
        Ok(())
    }

    fn parse_value_ident(&mut self) -> Result<(), ParseErr> {
        let (lo, hi) = self.parse_value_ident_span()?;
        let h = self.begin("value_ident");
        self.push_span(lo, hi);
        self.end(h);
        Ok(())
    }

    /// `value_or = value_and , ( "||" ?w , value_and ) *`
    fn parse_value_or(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_or");
        self.parse_value_and()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if !self.peek_str("||") {
                self.pos = save;
                break;
            }
            self.pos += 2;
            self.skip_ws();
            self.parse_value_and()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_and = value_cmp , ( "&&" ?w , value_cmp ) *`
    fn parse_value_and(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_and");
        self.parse_value_cmp()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if !self.peek_str("&&") {
                self.pos = save;
                break;
            }
            self.pos += 2;
            self.skip_ws();
            self.parse_value_cmp()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_cmp = value_add , ( cmp_op ?w , value_add ) *`
    /// `cmp_op = "==" | "!=" | "<=" | ">=" | "<" | ">"`
    fn parse_value_cmp(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_cmp");
        self.parse_value_add()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            let op_ok = if self.peek_str("==") || self.peek_str("!=")
                || self.peek_str("<=") || self.peek_str(">=")
            {
                self.pos += 2;
                true
            } else if matches!(self.at(), Some(b'<' | b'>')) {
                self.pos += 1;
                true
            } else {
                false
            };
            if !op_ok {
                self.pos = save;
                break;
            }
            self.skip_ws();
            self.parse_value_add()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_add = value_mul , ( add_op ?w , value_mul ) *`
    /// `add_op = "+" | "-"`
    fn parse_value_add(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_add");
        self.parse_value_mul()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            // Don't confuse "-" with "->" arrow.
            if self.at() == Some(b'-') && self.at_offset(1) == Some(b'>') {
                self.pos = save;
                break;
            }
            if !matches!(self.at(), Some(b'+' | b'-')) {
                self.pos = save;
                break;
            }
            self.pos += 1;
            self.skip_ws();
            self.parse_value_mul()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_mul = value_unary , ( mul_op ?w , value_unary ) *`
    fn parse_value_mul(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_mul");
        self.parse_value_unary()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if !matches!(self.at(), Some(b'*' | b'/' | b'%')) {
                self.pos = save;
                break;
            }
            // "/" with following identifier-char might also be regex
            // boundary; but inside a value_expr we don't expect regex.
            self.pos += 1;
            self.skip_ws();
            self.parse_value_unary()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_unary = ( "!" | "-" ) , value_atom | value_atom`
    fn parse_value_unary(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_unary");
        if matches!(self.at(), Some(b'!')) {
            self.pos += 1;
            self.skip_ws();
            self.parse_value_atom()?;
        } else if self.at() == Some(b'-') && self.at_offset(1) != Some(b'>') {
            // Unary minus
            self.pos += 1;
            self.skip_ws();
            self.parse_value_atom()?;
        } else {
            self.parse_value_atom()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_atom = int_lit | float_lit | bool_lit | string_lit
    ///            | value_fn_call | value_input | value_path
    ///            | "(" , value_expr ?w , ")"`
    fn parse_value_atom(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_atom");
        match self.at() {
            Some(b'(') => {
                self.pos += 1;
                self.skip_ws();
                self.parse_value_expr()?;
                self.skip_ws();
                if !self.eat_byte(b')') {
                    self.end(h);
                    return Err(self.err());
                }
            }
            Some(b'"') => {
                self.parse_string_lit()?;
            }
            Some(b) if b.is_ascii_digit() || (b == b'.' && matches!(self.at_offset(1), Some(d) if d.is_ascii_digit())) => {
                self.parse_numeric_lit()?;
            }
            Some(b) if b == b'_' || b.is_ascii_alphabetic() => {
                // value_path / value_input / value_fn_call / bool_lit.
                if self.peek_str_kw("true") || self.peek_str_kw("false") {
                    self.parse_bool_lit()?;
                } else if self.peek_str_kw("input") {
                    self.parse_value_input()?;
                } else {
                    // value_path, possibly followed by "(" for fn call.
                    self.parse_value_path_or_call()?;
                }
            }
            _ => {
                self.end(h);
                return Err(self.err());
            }
        }
        self.end(h);
        Ok(())
    }

    /// Test if `s` matches and is followed by a non-identifier byte.
    fn peek_str_kw(&self, s: &str) -> bool {
        if !self.peek_str(s) {
            return false;
        }
        match self.input.get(self.pos + s.len()).copied() {
            Some(b) if b == b'_' || b.is_ascii_alphanumeric() => false,
            _ => true,
        }
    }

    fn parse_string_lit(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("string_lit");
        let lo = self.pos;
        if !self.eat_byte(b'"') {
            self.end(h);
            return Err(self.err());
        }
        loop {
            match self.at() {
                Some(b'\\') => self.pos += 2,
                Some(b'"') => {
                    self.pos += 1;
                    break;
                }
                Some(_) => self.pos += 1,
                None => {
                    self.end(h);
                    return Err(self.err());
                }
            }
        }
        self.push_span(lo, self.pos);
        self.end(h);
        Ok(())
    }

    fn parse_numeric_lit(&mut self) -> Result<(), ParseErr> {
        // Decide int vs float by looking for "." in the lookahead.
        let start = self.pos;
        let mut probe = start;
        // Hex prefix?
        if self.input.get(probe).copied() == Some(b'0')
            && matches!(self.input.get(probe + 1).copied(), Some(b'x' | b'X'))
        {
            probe += 2;
            while let Some(b) = self.input.get(probe).copied() {
                if b.is_ascii_hexdigit() {
                    probe += 1;
                } else {
                    break;
                }
            }
        } else {
            while let Some(b) = self.input.get(probe).copied() {
                if b.is_ascii_digit() {
                    probe += 1;
                } else {
                    break;
                }
            }
        }
        let mut is_float = false;
        if self.input.get(probe).copied() == Some(b'.')
            && matches!(self.input.get(probe + 1).copied(), Some(d) if d.is_ascii_digit())
        {
            is_float = true;
            probe += 1;
            while let Some(b) = self.input.get(probe).copied() {
                if b.is_ascii_digit() {
                    probe += 1;
                } else {
                    break;
                }
            }
            if matches!(self.input.get(probe).copied(), Some(b'e' | b'E')) {
                probe += 1;
                if matches!(self.input.get(probe).copied(), Some(b'+' | b'-')) {
                    probe += 1;
                }
                while let Some(b) = self.input.get(probe).copied() {
                    if b.is_ascii_digit() {
                        probe += 1;
                    } else {
                        break;
                    }
                }
            }
        }
        // Trailing word chars (per BBNF int_lit/float_lit allowing \w*).
        while let Some(b) = self.input.get(probe).copied() {
            if b == b'_' || b.is_ascii_alphanumeric() {
                probe += 1;
            } else {
                break;
            }
        }
        if probe == start {
            return Err(self.err());
        }
        let text = &self.src[start..probe];
        if is_float {
            let h = self.begin("float_lit");
            // Strip trailing word chars for parse.
            let parse_part: String = text.chars().take_while(|c| c.is_ascii_digit() || *c == '.' || *c == 'e' || *c == 'E' || *c == '+' || *c == '-').collect();
            let v: f64 = parse_part.parse().unwrap_or(0.0);
            self.builder.push_leaf_with_f64(v);
            self.end(h);
        } else {
            let h = self.begin("int_lit");
            let v: i64 = if text.starts_with("0x") || text.starts_with("0X") {
                let stripped: String = text[2..].chars().take_while(|c| c.is_ascii_hexdigit()).collect();
                i64::from_str_radix(&stripped, 16).unwrap_or(0)
            } else {
                let parse_part: String = text.chars().take_while(|c| c.is_ascii_digit()).collect();
                parse_part.parse().unwrap_or(0)
            };
            self.builder.push_leaf_with_i64(v);
            self.end(h);
        }
        self.pos = probe;
        Ok(())
    }

    fn parse_bool_lit(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("bool_lit");
        let value = if self.eat_str("true") {
            true
        } else if self.eat_str("false") {
            false
        } else {
            self.end(h);
            return Err(self.err());
        };
        self.builder.push_leaf_with_bool(value);
        self.end(h);
        Ok(())
    }

    /// `value_input = "input" , ( "." , value_ident ) *`
    fn parse_value_input(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("value_input");
        if !self.eat_str("input") {
            self.end(h);
            return Err(self.err());
        }
        loop {
            if self.at() != Some(b'.') {
                break;
            }
            self.pos += 1;
            self.parse_value_ident()?;
        }
        self.end(h);
        Ok(())
    }

    /// `value_path = value_ident , ( "::" , value_ident ) *`
    /// `value_fn_call = value_path , "(" , ( value_expr , ( "," ?w , value_expr ) * ) ? , ")"`
    fn parse_value_path_or_call(&mut self) -> Result<(), ParseErr> {
        let path_h = self.begin("value_path");
        self.parse_value_ident()?;
        loop {
            if !self.peek_str("::") {
                break;
            }
            self.pos += 2;
            self.parse_value_ident()?;
        }
        self.end(path_h);
        // Optional fn call.
        if self.at() == Some(b'(') {
            // Re-frame the path as a value_fn_call wrapper. Note: our
            // simpler approach: just consume the arg list inline.
            let call_h = self.begin("value_fn_call");
            self.pos += 1;
            self.skip_ws();
            if self.at() != Some(b')') {
                self.parse_value_expr()?;
                self.skip_ws();
                while self.eat_byte(b',') {
                    self.skip_ws();
                    self.parse_value_expr()?;
                    self.skip_ws();
                }
            }
            if !self.eat_byte(b')') {
                self.end(call_h);
                return Err(self.err());
            }
            self.end(call_h);
        }
        Ok(())
    }

    /// `type_annotation = ":" ?w , type_name`
    fn parse_type_annotation(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("type_annotation");
        if !self.eat_byte(b':') {
            self.end(h);
            return Err(self.err());
        }
        self.skip_ws();
        self.parse_type_name()?;
        self.end(h);
        Ok(())
    }

    /// `type_name = primitive | identifier`
    fn parse_type_name(&mut self) -> Result<(), ParseErr> {
        let h = self.begin("type_name");
        // Primitives are themselves valid identifiers, so just parse
        // an identifier; lowering disambiguates by string match.
        let (lo, hi) = self.parse_value_ident_span()?;
        self.push_span(lo, hi);
        self.end(h);
        Ok(())
    }
}

/// Hand-written BBNF parser entry point. Consumes `src` and emits
/// a [`BbnfDocument`] borrowing the input slice.
pub fn parse<'p>(src: &'p str) -> Result<BbnfDocument<'p>, ParseErr> {
    let mut parser = Parser::new(src);
    parser.parse_grammar()?;
    parser.skip_ws();
    if parser.pos != src.len() {
        return Err(ParseErr::Syntax {
            offset: parser.pos as u32,
            rule: None,
        });
    }
    Ok(parser.builder.finalise(src))
}
