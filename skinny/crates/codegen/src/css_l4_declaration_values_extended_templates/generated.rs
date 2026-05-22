use super::config;
use super::sink::{CssFactError, FactSink};

pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError> {
    Scanner::new(input).emit()
}

struct Scanner<'i> {
    input: &'i str,
    bytes: &'i [u8],
    pos: usize,
    decls: u32,
}

impl<'i> Scanner<'i> {
    fn new(input: &'i str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
            decls: 0,
        }
    }

    fn emit(mut self) -> Result<String, CssFactError> {
        let mut sink = FactSink::new(self.input);
        while self.pos < self.bytes.len() {
            if self.bytes[self.pos] == b'{' {
                self.pos += 1;
                self.scan_block(1, &mut sink)?;
            } else {
                self.pos += 1;
            }
        }
        Ok(sink.finish())
    }

    fn scan_block(&mut self, depth: u32, sink: &mut FactSink) -> Result<(), CssFactError> {
        let mut segment_start = self.pos;
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b'{' => {
                    self.pos += 1;
                    self.scan_block(depth + 1, sink)?;
                    segment_start = self.pos;
                }
                b';' => {
                    self.emit_declaration(segment_start, self.pos, depth, sink)?;
                    self.pos += 1;
                    segment_start = self.pos;
                }
                b'}' => {
                    self.emit_declaration(segment_start, self.pos, depth, sink)?;
                    self.pos += 1;
                    return Ok(());
                }
                byte if config::is_quote_byte(byte) => {
                    self.pos = consume_quoted(self.bytes, self.pos, self.bytes.len())?;
                }
                _ => self.pos += 1,
            }
        }
        Err(CssFactError {
            offset: self.pos,
            message: "unterminated CSS block",
        })
    }

    fn emit_declaration(
        &mut self,
        start: usize,
        end: usize,
        depth: u32,
        sink: &mut FactSink,
    ) -> Result<(), CssFactError> {
        let start = trim_start(self.bytes, start, end);
        let end = trim_end(self.bytes, start, end);
        if start == end {
            return Ok(());
        }
        let Some(colon) = find_colon(self.bytes, start, end) else {
            return Ok(());
        };
        let prop_start = trim_start(self.bytes, start, colon);
        let prop_end = trim_end(self.bytes, prop_start, colon);
        if prop_start == prop_end {
            return Err(CssFactError {
                offset: start,
                message: "missing CSS property name",
            });
        }
        let value_start = trim_start(self.bytes, colon + 1, end);
        let value_end = trim_end(self.bytes, value_start, end);
        let (value_end, important) = strip_important(self.bytes, value_start, value_end);
        let value_end = trim_end(self.bytes, value_start, value_end);
        let decl = self.decls;
        self.decls += 1;
        sink.declaration(
            decl,
            depth,
            &self.input[prop_start..prop_end],
            important,
            value_start,
            value_end,
        );
        self.emit_tokens(decl, value_start, value_end, 0, sink)?;
        Ok(())
    }

    fn emit_tokens(
        &self,
        decl: u32,
        mut pos: usize,
        end: usize,
        depth: u32,
        sink: &mut FactSink,
    ) -> Result<usize, CssFactError> {
        if depth > config::MAX_VALUE_RECURSION {
            return Err(CssFactError {
                offset: pos,
                message: "CSS value recursion limit exceeded",
            });
        }
        let mut idx = 0u32;
        while pos < end {
            pos = skip_ws_and_comments(self.bytes, pos, end);
            if pos >= end {
                break;
            }
            if self.bytes[pos] == b')' && depth > 0 {
                return Ok(pos + 1);
            }
            let start = pos;
            let b = self.bytes[pos];
            if b == b'#' {
                pos += 1;
                let mark = pos;
                while pos < end && is_ident_byte(self.bytes[pos]) {
                    pos = consume_ident_byte(self.bytes, pos, end);
                }
                sink.token(decl, idx, depth, config::TOKEN_HASH, &self.input[mark..pos]);
            } else if config::is_quote_byte(b) {
                let (next, normalized) = normalized_quoted(self.input, self.bytes, pos, end)?;
                pos = next;
                sink.token(decl, idx, depth, config::TOKEN_STRING, &normalized);
            } else if starts_number(self.bytes, pos, end) {
                pos = consume_number(self.bytes, pos, end);
                if pos < end && self.bytes[pos] == b'%' {
                    pos += 1;
                    sink.token(
                        decl,
                        idx,
                        depth,
                        config::TOKEN_PERCENTAGE,
                        &self.input[start..pos],
                    );
                } else if pos < end && is_ident_start(self.bytes[pos]) {
                    while pos < end && is_ident_byte(self.bytes[pos]) {
                        pos = consume_ident_byte(self.bytes, pos, end);
                    }
                    sink.token(
                        decl,
                        idx,
                        depth,
                        config::TOKEN_DIMENSION,
                        &self.input[start..pos],
                    );
                } else {
                    sink.token(
                        decl,
                        idx,
                        depth,
                        config::TOKEN_NUMBER,
                        &self.input[start..pos],
                    );
                }
            } else if is_ident_start(b) {
                while pos < end && is_ident_byte(self.bytes[pos]) {
                    pos = consume_ident_byte(self.bytes, pos, end);
                }
                let ident_end = pos;
                if pos < end && self.bytes[pos] == b'(' {
                    let name = &self.input[start..ident_end];
                    pos += 1;
                    let inner = skip_ws_and_comments(self.bytes, pos, end);
                    if config::is_url_function(name)
                        && inner < end
                        && !config::is_quote_byte(self.bytes[inner])
                    {
                        let close = find_unquoted_url_close(self.bytes, inner, end)?;
                        let lexeme_start = trim_start(self.bytes, inner, close);
                        let lexeme_end = trim_end(self.bytes, lexeme_start, close);
                        sink.token(
                            decl,
                            idx,
                            depth,
                            config::TOKEN_URL,
                            &self.input[lexeme_start..lexeme_end],
                        );
                        pos = close + 1;
                    } else {
                        sink.token(decl, idx, depth, config::TOKEN_FUNCTION, name);
                        idx += 1;
                        pos = self.emit_tokens(decl, pos, end, depth + 1, sink)?;
                        sink.token(decl, idx, depth, config::TOKEN_PAREN_CLOSE, ")");
                    }
                } else {
                    sink.token(
                        decl,
                        idx,
                        depth,
                        config::TOKEN_IDENT,
                        &self.input[start..pos],
                    );
                }
            } else {
                pos += 1;
                match b {
                    b',' => sink.token(decl, idx, depth, config::TOKEN_COMMA, ","),
                    b'(' => {
                        sink.token(decl, idx, depth, config::TOKEN_PAREN_OPEN, "(");
                        idx += 1;
                        pos = self.emit_tokens(decl, pos, end, depth + 1, sink)?;
                        sink.token(decl, idx, depth, config::TOKEN_PAREN_CLOSE, ")");
                    }
                    b')' => sink.token(decl, idx, depth, config::TOKEN_PAREN_CLOSE, ")"),
                    b'[' => sink.token(decl, idx, depth, config::TOKEN_BRACKET_OPEN, "["),
                    b']' => sink.token(decl, idx, depth, config::TOKEN_BRACKET_CLOSE, "]"),
                    _ => sink.token(
                        decl,
                        idx,
                        depth,
                        config::TOKEN_DELIM,
                        &self.input[start..pos],
                    ),
                }
            }
            idx += 1;
        }
        Ok(pos)
    }
}

fn trim_start(bytes: &[u8], mut start: usize, end: usize) -> usize {
    while start < end && config::is_trivia_byte(bytes[start]) {
        start += 1;
    }
    start
}

fn trim_end(bytes: &[u8], start: usize, mut end: usize) -> usize {
    while end > start && config::is_trivia_byte(bytes[end - 1]) {
        end -= 1;
    }
    end
}

fn find_colon(bytes: &[u8], start: usize, end: usize) -> Option<usize> {
    let mut depth = 0u32;
    let mut pos = start;
    while pos < end {
        match bytes[pos] {
            byte if config::is_quote_byte(byte) => pos = consume_quoted(bytes, pos, end).ok()?,
            b'(' | b'[' => {
                depth += 1;
                pos += 1;
            }
            b')' | b']' => {
                depth = depth.saturating_sub(1);
                pos += 1;
            }
            b':' if depth == 0 => return Some(pos),
            _ => pos += 1,
        }
    }
    None
}

fn strip_important(bytes: &[u8], start: usize, end: usize) -> (usize, bool) {
    let end = trim_end(bytes, start, end);
    let word = config::IMPORTANT_KEYWORD;
    if end < start + word.len() {
        return (end, false);
    }
    let word_start = end - word.len();
    if !bytes[word_start..end].eq_ignore_ascii_case(word) {
        return (end, false);
    }
    let bang_end = trim_end(bytes, start, word_start);
    if bang_end > start && bytes[bang_end - 1] == b'!' {
        (bang_end - 1, true)
    } else {
        (end, false)
    }
}

fn skip_ws_and_comments(bytes: &[u8], mut pos: usize, end: usize) -> usize {
    loop {
        while pos < end && config::is_trivia_byte(bytes[pos]) {
            pos += 1;
        }
        if config::starts_block_comment(bytes, pos, end) {
            pos += 2;
            while pos + 1 < end && !config::ends_block_comment(bytes, pos, end) {
                pos += 1;
            }
            pos = (pos + 2).min(end);
            continue;
        }
        return pos;
    }
}

fn starts_number(bytes: &[u8], pos: usize, end: usize) -> bool {
    let b = bytes[pos];
    b.is_ascii_digit()
        || (b == b'.' && pos + 1 < end && bytes[pos + 1].is_ascii_digit())
        || ((b == b'+' || b == b'-')
            && pos + 1 < end
            && (bytes[pos + 1].is_ascii_digit()
                || (bytes[pos + 1] == b'.' && pos + 2 < end && bytes[pos + 2].is_ascii_digit())))
}

fn consume_number(bytes: &[u8], mut pos: usize, end: usize) -> usize {
    if pos < end && matches!(bytes[pos], b'+' | b'-') {
        pos += 1;
    }
    while pos < end && bytes[pos].is_ascii_digit() {
        pos += 1;
    }
    if pos < end && bytes[pos] == b'.' {
        pos += 1;
        while pos < end && bytes[pos].is_ascii_digit() {
            pos += 1;
        }
    }
    pos
}

fn is_ident_start(byte: u8) -> bool {
    config::is_ident_start(byte)
}

fn is_ident_byte(byte: u8) -> bool {
    config::is_ident_byte(byte)
}

fn consume_ident_byte(bytes: &[u8], pos: usize, end: usize) -> usize {
    if !config::is_escape_byte(bytes[pos]) {
        return pos + 1;
    }
    let mut next = pos + 1;
    while next < end
        && next < pos + 1 + config::CSS_HEX_ESCAPE_MAX_DIGITS
        && bytes[next].is_ascii_hexdigit()
    {
        next += 1;
    }
    if next == pos + 1 && next < end {
        next += 1;
    }
    if next < end && config::is_trivia_byte(bytes[next]) {
        next += 1;
    }
    next
}

fn consume_quoted(bytes: &[u8], pos: usize, end: usize) -> Result<usize, CssFactError> {
    let quote = bytes[pos];
    let mut cursor = pos + 1;
    while cursor < end {
        match bytes[cursor] {
            byte if config::is_escape_byte(byte) => cursor = (cursor + 2).min(end),
            byte if byte == quote => return Ok(cursor + 1),
            _ => cursor += 1,
        }
    }
    Err(CssFactError {
        offset: pos,
        message: "unterminated CSS string",
    })
}

fn normalized_quoted(
    input: &str,
    bytes: &[u8],
    pos: usize,
    end: usize,
) -> Result<(usize, String), CssFactError> {
    let quote = bytes[pos];
    let mut cursor = pos + 1;
    let mut normalized = String::new();
    while cursor < end {
        match bytes[cursor] {
            byte if byte == quote => return Ok((cursor + 1, normalized)),
            byte if config::is_escape_byte(byte) && cursor + 1 < end => {
                if bytes[cursor + 1].is_ascii_hexdigit() {
                    let mut hex_end = cursor + 1;
                    while hex_end < end
                        && hex_end < cursor + 1 + config::CSS_HEX_ESCAPE_MAX_DIGITS
                        && bytes[hex_end].is_ascii_hexdigit()
                    {
                        hex_end += 1;
                    }
                    if let Ok(hex) = std::str::from_utf8(&bytes[cursor + 1..hex_end]) {
                        if let Ok(codepoint) = u32::from_str_radix(hex, 16) {
                            if let Some(ch) = char::from_u32(codepoint) {
                                normalized.push(ch);
                            }
                        }
                    }
                    cursor = hex_end;
                    if cursor < end && config::is_trivia_byte(bytes[cursor]) {
                        cursor += 1;
                    }
                } else {
                    let next = cursor + 2;
                    normalized.push_str(&input[cursor + 1..next]);
                    cursor = next;
                }
            }
            _ => {
                let next = cursor + 1;
                normalized.push_str(&input[cursor..next]);
                cursor = next;
            }
        }
    }
    Err(CssFactError {
        offset: pos,
        message: "unterminated CSS string",
    })
}

fn find_unquoted_url_close(
    bytes: &[u8],
    mut pos: usize,
    end: usize,
) -> Result<usize, CssFactError> {
    while pos < end {
        match bytes[pos] {
            b')' => return Ok(pos),
            byte if config::is_quote_byte(byte) => {
                return Err(CssFactError {
                    offset: pos,
                    message: "quoted URL must use function token form",
                });
            }
            _ => pos += 1,
        }
    }
    Err(CssFactError {
        offset: end,
        message: "unterminated CSS url",
    })
}
