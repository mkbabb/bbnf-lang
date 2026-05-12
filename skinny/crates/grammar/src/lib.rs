use ir::{AltMode, CaseSensitivity, ExprId, ExprKind, GrammarIr, SourceSpan, ValidationError};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum GrammarError {
    #[error("{code}: {message} at byte {offset}")]
    Parse {
        code: &'static str,
        message: String,
        offset: usize,
    },
    #[error(transparent)]
    Validation(#[from] ValidationError),
}

pub fn parse_json_grammar(source: &str) -> Result<GrammarIr, GrammarError> {
    parse_grammar("json", source)
}

pub fn load_json_grammar(path: impl AsRef<std::path::Path>) -> Result<GrammarIr, GrammarError> {
    let source = std::fs::read_to_string(path.as_ref()).map_err(|err| GrammarError::Parse {
        code: "BBNF-SOURCE-LOAD",
        message: err.to_string(),
        offset: 0,
    })?;
    parse_json_grammar(&source)
}

pub fn parse_grammar(name: &str, source: &str) -> Result<GrammarIr, GrammarError> {
    let hash = stable_hash(source);
    let mut parser = Parser::new(source, name, hash);
    parser.parse_rules()?;
    parser.grammar.resolve_refs()?;
    parser.grammar.validate()?;
    Ok(parser.grammar)
}

fn stable_hash(source: &str) -> String {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in source.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{hash:016x}")
}

struct Parser<'a> {
    source: &'a str,
    cursor: usize,
    grammar: GrammarIr,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, name: &str, hash: String) -> Self {
        Self {
            source,
            cursor: 0,
            grammar: GrammarIr::new(name, hash),
        }
    }

    fn parse_rules(&mut self) -> Result<(), GrammarError> {
        while self.skip_trivia() {
            if self.peek_char() == Some('@') {
                self.parse_directive()?;
                continue;
            }

            let start = self.cursor;
            let name = self.parse_ident()?;
            self.expect_char('=')?;
            let body = self.parse_expr()?;
            self.expect_char(';')?;
            self.grammar
                .add_rule(name, body, SourceSpan::new(start, self.cursor));
        }
        Ok(())
    }

    fn parse_directive(&mut self) -> Result<(), GrammarError> {
        let start = self.cursor;
        self.expect_char('@')?;
        let name = self.parse_ident()?;
        while let Some(ch) = self.peek_char() {
            self.bump_char();
            if ch == ';' || ch == '\n' {
                break;
            }
        }

        match name.as_str() {
            "import" | "token" => Ok(()),
            _ => Err(self.error_at(
                "BBNF-DIRECTIVE-NOT-IN-SKINNY",
                start,
                format!("directive @{name} is not available in the skinny compiler"),
            )),
        }
    }

    fn parse_expr(&mut self) -> Result<ExprId, GrammarError> {
        self.parse_alt()
    }

    fn parse_alt(&mut self) -> Result<ExprId, GrammarError> {
        let start = self.cursor;
        let mut branches = vec![self.parse_seq()?];
        loop {
            self.skip_inline_trivia();
            if self.peek_char() != Some('|') {
                break;
            }
            self.bump_char();
            branches.push(self.parse_seq()?);
        }

        if branches.len() == 1 {
            Ok(branches[0])
        } else {
            Ok(self.grammar.add_expr(
                ExprKind::Alt {
                    branches,
                    mode: AltMode::Dispatch,
                },
                SourceSpan::new(start, self.cursor),
            ))
        }
    }

    fn parse_seq(&mut self) -> Result<ExprId, GrammarError> {
        let start = self.cursor;
        let mut children = Vec::new();
        loop {
            self.skip_inline_trivia();
            match self.peek_char() {
                None | Some(';') | Some(')') | Some('|') => break,
                _ => children.push(self.parse_postfix()?),
            }
        }

        match children.len() {
            0 => Ok(self.grammar.add_expr(
                ExprKind::Seq(Vec::new()),
                SourceSpan::new(start, self.cursor),
            )),
            1 => Ok(children[0]),
            _ => Ok(self
                .grammar
                .add_expr(ExprKind::Seq(children), SourceSpan::new(start, self.cursor))),
        }
    }

    fn parse_postfix(&mut self) -> Result<ExprId, GrammarError> {
        let start = self.cursor;
        let mut expr = self.parse_atom()?;
        loop {
            self.skip_inline_trivia();
            let Some(ch) = self.peek_char() else {
                return Ok(expr);
            };
            match ch {
                '*' => {
                    self.bump_char();
                    expr = self.grammar.add_expr(
                        ExprKind::Repeat {
                            body: expr,
                            min: 0,
                            max: None,
                        },
                        SourceSpan::new(start, self.cursor),
                    );
                }
                '?' => {
                    self.bump_char();
                    expr = self.grammar.add_expr(
                        ExprKind::Optional(expr),
                        SourceSpan::new(start, self.cursor),
                    );
                }
                '+' => {
                    self.bump_char();
                    expr = self.grammar.add_expr(
                        ExprKind::Repeat {
                            body: expr,
                            min: 1,
                            max: None,
                        },
                        SourceSpan::new(start, self.cursor),
                    );
                }
                _ => return Ok(expr),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<ExprId, GrammarError> {
        self.skip_inline_trivia();
        let start = self.cursor;
        match self.peek_char() {
            Some('"') => {
                let bytes = self.parse_literal()?;
                Ok(self.grammar.add_expr(
                    ExprKind::Literal {
                        bytes,
                        case: CaseSensitivity::Sensitive,
                    },
                    SourceSpan::new(start, self.cursor),
                ))
            }
            Some('/') => {
                let pattern = self.parse_regex()?;
                Ok(self.grammar.add_expr(
                    ExprKind::Regex { pattern },
                    SourceSpan::new(start, self.cursor),
                ))
            }
            Some('(') => {
                self.bump_char();
                let expr = self.parse_expr()?;
                self.expect_char(')')?;
                Ok(expr)
            }
            Some(ch) if is_ident_start(ch) => {
                let name = self.parse_ident()?;
                Ok(self.grammar.add_expr(
                    ExprKind::Ref { name, target: None },
                    SourceSpan::new(start, self.cursor),
                ))
            }
            Some(ch) => Err(self.error(format!("unexpected token `{ch}`"))),
            None => Err(self.error("unexpected end of input".to_string())),
        }
    }

    fn parse_literal(&mut self) -> Result<Vec<u8>, GrammarError> {
        self.expect_char('"')?;
        let mut out = String::new();
        while let Some(ch) = self.peek_char() {
            self.bump_char();
            match ch {
                '"' => return Ok(out.into_bytes()),
                '\\' => {
                    let Some(escaped) = self.peek_char() else {
                        return Err(self.error("unterminated escape in literal".to_string()));
                    };
                    self.bump_char();
                    out.push(match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => out.push(other),
            }
        }
        Err(self.error("unterminated string literal".to_string()))
    }

    fn parse_regex(&mut self) -> Result<String, GrammarError> {
        self.expect_char('/')?;
        let mut pattern = String::new();
        let mut escaped = false;
        while let Some(ch) = self.peek_char() {
            self.bump_char();
            if escaped {
                pattern.push('\\');
                pattern.push(ch);
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == '/' {
                return Ok(pattern);
            } else {
                pattern.push(ch);
            }
        }
        Err(self.error("unterminated regex literal".to_string()))
    }

    fn parse_ident(&mut self) -> Result<String, GrammarError> {
        self.skip_inline_trivia();
        let start = self.cursor;
        let Some(ch) = self.peek_char() else {
            return Err(self.error("expected identifier".to_string()));
        };
        if !is_ident_start(ch) {
            return Err(self.error(format!("expected identifier, found `{ch}`")));
        }
        self.bump_char();
        while let Some(ch) = self.peek_char() {
            if !is_ident_continue(ch) {
                break;
            }
            self.bump_char();
        }
        Ok(self.source[start..self.cursor].to_string())
    }

    fn expect_char(&mut self, expected: char) -> Result<(), GrammarError> {
        self.skip_inline_trivia();
        match self.peek_char() {
            Some(ch) if ch == expected => {
                self.bump_char();
                Ok(())
            }
            Some(ch) => Err(self.error(format!("expected `{expected}`, found `{ch}`"))),
            None => Err(self.error(format!("expected `{expected}`, found end of input"))),
        }
    }

    fn skip_trivia(&mut self) -> bool {
        loop {
            self.skip_inline_trivia();
            if self.peek_char() == Some('\n') {
                self.bump_char();
                continue;
            }
            if self.source[self.cursor..].starts_with("//") {
                self.skip_line_comment();
                continue;
            }
            return self.cursor < self.source.len();
        }
    }

    fn skip_inline_trivia(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\r' | '\n') => {
                    self.bump_char();
                }
                _ if self.source[self.cursor..].starts_with("//") => self.skip_line_comment(),
                _ => return,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            self.bump_char();
            if ch == '\n' {
                break;
            }
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.cursor..].chars().next()
    }

    fn bump_char(&mut self) {
        if let Some(ch) = self.peek_char() {
            self.cursor += ch.len_utf8();
        }
    }

    fn error(&self, message: String) -> GrammarError {
        self.error_at("BBNF-PARSE", self.cursor, message)
    }

    fn error_at(&self, code: &'static str, offset: usize, message: String) -> GrammarError {
        GrammarError::Parse {
            code,
            message,
            offset,
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch == '-' || ch.is_ascii_alphanumeric()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ir::ExprKind;

    const JSON_GRAMMAR: &str = include_str!("../../../grammars/json.bbnf");

    #[test]
    fn parses_skinny_json_rules() {
        let grammar = parse_json_grammar(JSON_GRAMMAR).expect("json grammar parses");

        assert_eq!(grammar.rules.len(), 15);
        assert_eq!(grammar.rule_by_name("json").unwrap().name, "json");
        assert!(grammar.pretty().contains("value = ws"));
    }

    #[test]
    fn resolves_forward_and_cyclic_refs() {
        let grammar = parse_grammar("cycle", "a = b ;\nb = a | \"x\" ;").unwrap();
        let a = grammar.rule_by_name("a").unwrap();
        let ExprKind::Ref { target, .. } = grammar.expr(a.body).kind else {
            panic!("a body should be a ref");
        };
        assert_eq!(target, Some(grammar.rule_by_name("b").unwrap().id));
    }

    #[test]
    fn rejects_non_skinny_directives() {
        let err = parse_grammar("bad", "@layout(ws = ws)\njson = \"x\" ;").unwrap_err();
        assert!(matches!(
            err,
            GrammarError::Parse {
                code: "BBNF-DIRECTIVE-NOT-IN-SKINNY",
                ..
            }
        ));
    }

    #[test]
    fn rejects_unresolved_refs() {
        let err = parse_grammar("bad", "json = missing ;").unwrap_err();
        assert!(matches!(
            err,
            GrammarError::Validation(ValidationError::UnresolvedRef { .. })
        ));
    }
}
