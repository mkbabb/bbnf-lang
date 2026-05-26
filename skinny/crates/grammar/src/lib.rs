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
    pub frontend: RuntimeFrontendClosure,
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
pub struct RuntimeFrontendClosure {
    pub source_hash: String,
    pub sources: Vec<RuntimeFrontendSource>,
    pub imports: Vec<RuntimeFrontendImport>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFrontendSource {
    pub path: String,
    pub source_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFrontendImport {
    pub importer_path: String,
    pub importer_source_hash: String,
    pub offset: usize,
    pub specifier: String,
    pub resolved_path: String,
    pub resolved_source_hash: String,
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
            RuntimeConstructKind::TokenDirective
            | RuntimeConstructKind::WhitespaceDirective
            | RuntimeConstructKind::PrettyDirective => "BBNF-UNSUPPORTED-DIRECTIVE",
            RuntimeConstructKind::WhitespaceModifier => "BBNF-UNSUPPORTED-WHITESPACE-MODIFIER",
            RuntimeConstructKind::Projection | RuntimeConstructKind::TypedProjection => {
                "BBNF-UNSUPPORTED-PROJECTION"
            }
            RuntimeConstructKind::HostCapture => "BBNF-UNSUPPORTED-HOST-CAPTURE",
            RuntimeConstructKind::Comma
            | RuntimeConstructKind::Import
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
    let (source_hash, scans) = scan_runtime_sources(sources)?;
    let frontend = resolve_runtime_import_closure(source_hash.clone(), &scans)?;
    let constructs = scans
        .into_iter()
        .flat_map(|scan| scan.constructs)
        .collect::<Vec<_>>();
    Ok(RuntimeSourceFacts {
        source_hash,
        frontend,
        constructs,
    })
}

pub fn parse_frontend_closure(
    sources: &[RuntimeSource<'_>],
) -> Result<RuntimeFrontendClosure, GrammarError> {
    let (source_hash, scans) = scan_runtime_sources(sources)?;
    resolve_runtime_import_closure(source_hash, &scans)
}

fn scan_runtime_sources(
    sources: &[RuntimeSource<'_>],
) -> Result<(String, Vec<RuntimeSourceScan>), GrammarError> {
    if sources.is_empty() {
        return Err(GrammarError::Parse {
            code: "BBNF-RUNTIME-SOURCE-MISSING",
            message: "runtime generation requires at least one grammar source".to_string(),
            offset: 0,
        });
    }
    let mut digest = String::new();
    let mut scans = Vec::new();
    for source in sources {
        let source_hash = stable_hash(source.source);
        digest.push_str(source.path);
        digest.push(':');
        digest.push_str(&source_hash);
        digest.push(';');
        let mut constructs = Vec::new();
        let mut imports = Vec::new();
        scan_runtime_source(source, &source_hash, &mut constructs, &mut imports)?;
        scans.push(RuntimeSourceScan {
            path: source.path.to_string(),
            source_hash,
            constructs,
            imports,
        });
    }
    Ok((stable_hash(&digest), scans))
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeSourceScan {
    path: String,
    source_hash: String,
    constructs: Vec<RuntimeConstruct>,
    imports: Vec<RuntimeImportRef>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeImportRef {
    specifier: String,
    offset: usize,
}

fn scan_runtime_source(
    input: &RuntimeSource<'_>,
    source_hash: &str,
    constructs: &mut Vec<RuntimeConstruct>,
    imports: &mut Vec<RuntimeImportRef>,
) -> Result<(), GrammarError> {
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
        let found = if rest.starts_with("@import") && runtime_keyword_boundary(rest, 7) {
            let target = parse_runtime_import_target(input.source, cursor, 7)?;
            imports.push(target);
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
    Ok(())
}

fn runtime_keyword_boundary(rest: &str, width: usize) -> bool {
    match rest[width..].chars().next() {
        Some(ch) => !is_ident_continue(ch),
        None => true,
    }
}

fn parse_runtime_import_target(
    source: &str,
    directive_offset: usize,
    directive_width: usize,
) -> Result<RuntimeImportRef, GrammarError> {
    let mut cursor = directive_offset + directive_width;
    while cursor < source.len() {
        let byte = source.as_bytes()[cursor];
        if matches!(byte, b';' | b'\n') {
            return Err(runtime_import_error(
                directive_offset,
                "missing quoted import target",
            ));
        }
        if matches!(byte, b'"' | b'\'') {
            break;
        }
        cursor += 1;
    }
    if cursor >= source.len() {
        return Err(runtime_import_error(
            directive_offset,
            "missing quoted import target",
        ));
    }
    let quote = source.as_bytes()[cursor];
    cursor += 1;
    let target_start = cursor;
    let mut escaped = false;
    while cursor < source.len() {
        let byte = source.as_bytes()[cursor];
        if escaped {
            escaped = false;
            cursor += 1;
            continue;
        }
        if byte == b'\\' {
            escaped = true;
            cursor += 1;
            continue;
        }
        if byte == quote {
            let specifier = source[target_start..cursor].to_string();
            if specifier.trim().is_empty() {
                return Err(runtime_import_error(
                    directive_offset,
                    "runtime import target is empty",
                ));
            }
            return Ok(RuntimeImportRef {
                specifier,
                offset: directive_offset,
            });
        }
        cursor += 1;
    }
    Err(runtime_import_error(
        directive_offset,
        "unterminated runtime import target",
    ))
}

fn runtime_import_error(offset: usize, message: &str) -> GrammarError {
    GrammarError::Parse {
        code: "BBNF-RUNTIME-IMPORT",
        message: message.to_string(),
        offset,
    }
}

fn resolve_runtime_import_closure(
    source_hash: String,
    scans: &[RuntimeSourceScan],
) -> Result<RuntimeFrontendClosure, GrammarError> {
    let mut path_to_index = std::collections::BTreeMap::new();
    for (index, scan) in scans.iter().enumerate() {
        if path_to_index.insert(scan.path.as_str(), index).is_some() {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-SOURCE-DUPLICATE",
                message: format!("runtime source `{}` appears more than once", scan.path),
                offset: 0,
            });
        }
    }

    let mut adjacency = vec![Vec::new(); scans.len()];
    let mut imports = Vec::new();
    for (from_index, scan) in scans.iter().enumerate() {
        for import in &scan.imports {
            let resolved =
                resolve_runtime_import_path(&scan.path, &import.specifier, &path_to_index);
            let Some(&to_index) = path_to_index.get(resolved.as_str()) else {
                return Err(GrammarError::Parse {
                    code: "BBNF-RUNTIME-IMPORT-MISSING",
                    message: format!(
                        "runtime import `{}` from `{}` resolved to `{resolved}` but was not present in the request source map",
                        import.specifier, scan.path
                    ),
                    offset: import.offset,
                });
            };
            adjacency[from_index].push(RuntimeImportArc {
                to_index,
                offset: import.offset,
            });
            imports.push(RuntimeFrontendImport {
                importer_path: scan.path.clone(),
                importer_source_hash: scan.source_hash.clone(),
                offset: import.offset,
                specifier: import.specifier.clone(),
                resolved_path: resolved,
                resolved_source_hash: scans[to_index].source_hash.clone(),
            });
        }
    }

    let mut state = vec![RuntimeImportVisit::Unvisited; scans.len()];
    let mut stack = Vec::new();
    for index in 0..scans.len() {
        visit_runtime_imports(index, scans, &adjacency, &mut state, &mut stack)?;
    }
    let sources = scans
        .iter()
        .map(|scan| RuntimeFrontendSource {
            path: scan.path.clone(),
            source_hash: scan.source_hash.clone(),
        })
        .collect::<Vec<_>>();
    Ok(RuntimeFrontendClosure {
        source_hash,
        sources,
        imports,
    })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RuntimeImportArc {
    to_index: usize,
    offset: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeImportVisit {
    Unvisited,
    Visiting,
    Visited,
}

fn visit_runtime_imports(
    index: usize,
    scans: &[RuntimeSourceScan],
    adjacency: &[Vec<RuntimeImportArc>],
    state: &mut [RuntimeImportVisit],
    stack: &mut Vec<usize>,
) -> Result<(), GrammarError> {
    match state[index] {
        RuntimeImportVisit::Visited => return Ok(()),
        RuntimeImportVisit::Visiting => {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-IMPORT-CYCLE",
                message: format!("runtime import cycle entered at `{}`", scans[index].path),
                offset: 0,
            });
        }
        RuntimeImportVisit::Unvisited => {}
    }

    state[index] = RuntimeImportVisit::Visiting;
    stack.push(index);
    for edge in &adjacency[index] {
        if state[edge.to_index] == RuntimeImportVisit::Visiting {
            let cycle_start = stack
                .iter()
                .position(|stack_index| *stack_index == edge.to_index)
                .unwrap_or(0);
            let mut cycle = stack[cycle_start..]
                .iter()
                .map(|stack_index| scans[*stack_index].path.as_str())
                .collect::<Vec<_>>();
            cycle.push(scans[edge.to_index].path.as_str());
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-IMPORT-CYCLE",
                message: format!("runtime import cycle: {}", cycle.join(" -> ")),
                offset: edge.offset,
            });
        }
        visit_runtime_imports(edge.to_index, scans, adjacency, state, stack)?;
    }
    stack.pop();
    state[index] = RuntimeImportVisit::Visited;
    Ok(())
}

fn resolve_runtime_import_path(
    from_path: &str,
    target: &str,
    path_to_index: &std::collections::BTreeMap<&str, usize>,
) -> String {
    let mut parts = Vec::new();
    if !target.starts_with('/') {
        if let Some((base, _)) = from_path.rsplit_once('/') {
            parts.extend(base.split('/').filter(|part| !part.is_empty()));
        }
    }
    for part in target.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            _ => parts.push(part),
        }
    }
    let resolved = parts.join("/");
    if path_to_index.contains_key(resolved.as_str()) {
        return resolved;
    }
    if !resolved
        .rsplit('/')
        .next()
        .is_some_and(|segment| segment.contains('.'))
    {
        let with_bbnf = format!("{resolved}.bbnf");
        if path_to_index.contains_key(with_bbnf.as_str()) {
            return with_bbnf;
        }
    }
    resolved
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
    let facts = parse_runtime_source_facts(&[
        RuntimeSource::new("css-lite.bbnf", source),
        RuntimeSource::new("tokens.bbnf", "ident = /[a-z]+/ ;\n"),
    ])
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
fn w5b_frontend_import_graph_resolves_request_sources() {
    let stylesheet = r#"
@import "tokens.bbnf" ;
root = ident ;
"#;
    let tokens = r#"
@token ident ;
ident = /[a-z]+/ ;
"#;
    let facts = parse_runtime_source_facts(&[
        RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", stylesheet),
        RuntimeSource::new("grammar/css/l4/tokens.bbnf", tokens),
    ])
    .expect("request-local imports resolve");

    assert_eq!(facts.frontend.sources.len(), 2);
    assert!(facts.frontend.sources.iter().any(|hash| {
        hash.path == "grammar/css/l4/stylesheet.bbnf" && hash.source_hash == stable_hash(stylesheet)
    }));
    assert_eq!(
        facts.frontend.imports,
        vec![RuntimeFrontendImport {
            importer_path: "grammar/css/l4/stylesheet.bbnf".to_string(),
            importer_source_hash: stable_hash(stylesheet),
            offset: stylesheet.find("@import").unwrap(),
            specifier: "tokens.bbnf".to_string(),
            resolved_path: "grammar/css/l4/tokens.bbnf".to_string(),
            resolved_source_hash: stable_hash(tokens),
        }]
    );
}

#[cfg(test)]
#[test]
fn w5b_frontend_missing_import_fails_closed() {
    let source = "@import \"missing.bbnf\" ;\nroot = \"x\" ;\n";
    let err =
        parse_runtime_source_facts(&[RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", source)])
            .unwrap_err();

    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-IMPORT-MISSING",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_import_cycle_fails_closed() {
    let root = "@import \"tokens.bbnf\" ;\nroot = ident ;\n";
    let tokens = "@import \"stylesheet.bbnf\" ;\nident = /[a-z]+/ ;\n";
    let err = parse_runtime_source_facts(&[
        RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", root),
        RuntimeSource::new("grammar/css/l4/tokens.bbnf", tokens),
    ])
    .unwrap_err();

    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-IMPORT-CYCLE",
            ..
        }
    ));
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
