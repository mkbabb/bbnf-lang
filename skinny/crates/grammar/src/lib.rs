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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeSource<'a> {
    pub path: &'a str,
    pub source: &'a str,
}

impl<'a> RuntimeSource<'a> {
    pub fn new(path: &'a str, source: &'a str) -> Self {
        Self { path, source }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeSourceFacts {
    pub source_hash: String,
    pub constructs: Vec<RuntimeConstruct>,
}

impl RuntimeSourceFacts {
    pub fn count(&self, kind: RuntimeConstructKind) -> usize {
        self.constructs
            .iter()
            .filter(|construct| construct.kind == kind)
            .count()
    }

    pub fn projection_count(&self) -> usize {
        self.count(RuntimeConstructKind::Projection)
            + self.count(RuntimeConstructKind::TypedProjection)
    }

    pub fn first_unsupported(&self) -> Option<UnsupportedRuntimeConstruct> {
        self.constructs
            .iter()
            .find_map(RuntimeConstruct::unsupported)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeConstruct {
    pub kind: RuntimeConstructKind,
    pub path: String,
    pub offset: usize,
    pub source_hash: String,
}

impl RuntimeConstruct {
    fn unsupported(&self) -> Option<UnsupportedRuntimeConstruct> {
        let code = match self.kind {
            RuntimeConstructKind::Import => "BBNF-UNSUPPORTED-IMPORT-RESOLUTION",
            RuntimeConstructKind::TokenDirective
            | RuntimeConstructKind::WhitespaceDirective
            | RuntimeConstructKind::PrettyDirective => "BBNF-UNSUPPORTED-DIRECTIVE",
            RuntimeConstructKind::WhitespaceModifier => "BBNF-UNSUPPORTED-WHITESPACE-MODIFIER",
            RuntimeConstructKind::Projection | RuntimeConstructKind::TypedProjection => {
                "BBNF-UNSUPPORTED-PROJECTION"
            }
            RuntimeConstructKind::HostCapture => "BBNF-UNSUPPORTED-HOST-CAPTURE",
            RuntimeConstructKind::Comma
            | RuntimeConstructKind::ShiftRight
            | RuntimeConstructKind::ShiftLeft => return None,
        };
        Some(UnsupportedRuntimeConstruct {
            code,
            path: self.path.clone(),
            offset: self.offset,
            source_hash: self.source_hash.clone(),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeConstructKind {
    Import,
    TokenDirective,
    WhitespaceDirective,
    PrettyDirective,
    Comma,
    WhitespaceModifier,
    ShiftRight,
    ShiftLeft,
    Projection,
    TypedProjection,
    HostCapture,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnsupportedRuntimeConstruct {
    pub code: &'static str,
    pub path: String,
    pub offset: usize,
    pub source_hash: String,
}

pub fn parse_runtime_source_facts(
    sources: &[RuntimeSource<'_>],
) -> Result<RuntimeSourceFacts, GrammarError> {
    if sources.is_empty() {
        return Err(GrammarError::Parse {
            code: "BBNF-RUNTIME-SOURCE-MISSING",
            message: "runtime generation requires at least one grammar source".to_string(),
            offset: 0,
        });
    }
    let mut digest = String::new();
    let mut constructs = Vec::new();
    for source in sources {
        let source_hash = stable_hash(source.source);
        digest.push_str(source.path);
        digest.push(':');
        digest.push_str(&source_hash);
        digest.push(';');
        scan_runtime_source(source, &source_hash, &mut constructs);
    }
    Ok(RuntimeSourceFacts {
        source_hash: stable_hash(&digest),
        constructs,
    })
}

fn scan_runtime_source(
    input: &RuntimeSource<'_>,
    source_hash: &str,
    constructs: &mut Vec<RuntimeConstruct>,
) {
    let bytes = input.source.as_bytes();
    let mut cursor = 0;
    while cursor < bytes.len() {
        let rest = &input.source[cursor..];
        if rest.starts_with("//") {
            cursor += rest.find('\n').map_or(rest.len(), |index| index + 1);
            continue;
        }
        if matches!(bytes[cursor], b'"' | b'\'') {
            cursor = skip_quoted(input.source, cursor);
            continue;
        }
        if bytes[cursor] == b'/' && !rest.starts_with("//") {
            cursor = skip_regex(input.source, cursor);
            continue;
        }
        let found = if rest.starts_with("@import") {
            Some((RuntimeConstructKind::Import, 7))
        } else if rest.starts_with("@token") {
            Some((RuntimeConstructKind::TokenDirective, 6))
        } else if rest.starts_with("@ws") {
            Some((RuntimeConstructKind::WhitespaceDirective, 3))
        } else if rest.starts_with("@pretty") {
            Some((RuntimeConstructKind::PrettyDirective, 7))
        } else if rest.starts_with("@{") {
            Some((RuntimeConstructKind::HostCapture, 2))
        } else if rest.starts_with("?w") {
            Some((RuntimeConstructKind::WhitespaceModifier, 2))
        } else if rest.starts_with(">>") {
            Some((RuntimeConstructKind::ShiftRight, 2))
        } else if rest.starts_with("<<") {
            Some((RuntimeConstructKind::ShiftLeft, 2))
        } else if rest.starts_with("->") {
            let kind = if projection_has_type_suffix(rest) {
                RuntimeConstructKind::TypedProjection
            } else {
                RuntimeConstructKind::Projection
            };
            Some((kind, 2))
        } else if bytes[cursor] == b',' {
            Some((RuntimeConstructKind::Comma, 1))
        } else {
            None
        };
        if let Some((kind, width)) = found {
            constructs.push(RuntimeConstruct {
                kind,
                path: input.path.to_string(),
                offset: cursor,
                source_hash: source_hash.to_string(),
            });
            cursor += width;
        } else {
            cursor += rest.chars().next().map(char::len_utf8).unwrap_or(1);
        }
    }
}

fn skip_quoted(source: &str, start: usize) -> usize {
    let quote = source.as_bytes()[start];
    let mut cursor = start + 1;
    let mut escaped = false;
    while cursor < source.len() {
        let byte = source.as_bytes()[cursor];
        cursor += 1;
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == quote {
            break;
        }
    }
    cursor
}

fn skip_regex(source: &str, start: usize) -> usize {
    let mut cursor = start + 1;
    let mut escaped = false;
    while cursor < source.len() {
        let byte = source.as_bytes()[cursor];
        cursor += 1;
        if escaped {
            escaped = false;
        } else if byte == b'\\' {
            escaped = true;
        } else if byte == b'/' {
            break;
        }
    }
    cursor
}

fn projection_has_type_suffix(rest: &str) -> bool {
    rest.split(|ch: char| ch == '|' || ch == ';' || ch == '\n' || ch == ',')
        .next()
        .and_then(|projection| projection.rsplit_once(':').map(|(_, ty)| ty.trim()))
        .is_some_and(|ty| {
            ["u8", "u16", "u32", "u64", "i64", "f64"]
                .iter()
                .any(|suffix| ty.starts_with(suffix))
        })
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

#[cfg(test)]
#[test]
fn w5a_css_l4_constructs_parse_as_source_facts() {
    let source = r#"
@import "tokens.bbnf" ;
@token ident ;
@ws /\s*/ ;
@pretty stylesheet block ;

root = @{ "url" , "(" >> ident ?w , "," ?w , ident << ")" } ;
tag = "from" -> 0u8 | "paint" -> crate::paint(input): u32 ;
"#;
    let facts = parse_runtime_source_facts(&[RuntimeSource::new("css-lite.bbnf", source)])
        .expect("source facts parse");

    assert_eq!(facts.count(RuntimeConstructKind::Import), 1);
    assert_eq!(facts.count(RuntimeConstructKind::TokenDirective), 1);
    assert_eq!(facts.count(RuntimeConstructKind::WhitespaceDirective), 1);
    assert_eq!(facts.count(RuntimeConstructKind::PrettyDirective), 1);
    assert_eq!(facts.count(RuntimeConstructKind::Comma), 3);
    assert_eq!(facts.count(RuntimeConstructKind::WhitespaceModifier), 2);
    assert_eq!(facts.count(RuntimeConstructKind::ShiftRight), 1);
    assert_eq!(facts.count(RuntimeConstructKind::ShiftLeft), 1);
    assert_eq!(facts.count(RuntimeConstructKind::Projection), 1);
    assert_eq!(facts.count(RuntimeConstructKind::TypedProjection), 1);
    assert_eq!(facts.count(RuntimeConstructKind::HostCapture), 1);
    assert!(!facts.source_hash.is_empty());
}

#[cfg(test)]
#[test]
fn w5a_named_unsupported_constructs_are_source_located() {
    let source = "root = \"x\" -> 0u8 ;";
    let facts = parse_runtime_source_facts(&[RuntimeSource::new("bbnf-self.bbnf", source)])
        .expect("source facts parse");
    let unsupported = facts.first_unsupported().expect("unsupported construct");

    assert_eq!(unsupported.code, "BBNF-UNSUPPORTED-PROJECTION");
    assert_eq!(unsupported.path, "bbnf-self.bbnf");
    assert_eq!(unsupported.offset, source.find("->").unwrap());
    assert!(!unsupported.source_hash.is_empty());
}
