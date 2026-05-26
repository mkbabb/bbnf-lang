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
    pub layout: RuntimeFrontendLayout,
    pub pretty_directives: Vec<RuntimePrettyDirective>,
    pub host_captures: Vec<RuntimeHostCapture>,
    pub projections: Vec<RuntimeProjection>,
    pub typed_projections: Vec<RuntimeTypedProjection>,
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RuntimeFrontendLayout {
    pub whitespace_directives: Vec<RuntimeWhitespaceDirective>,
    pub whitespace_modifiers: Vec<RuntimeWhitespaceModifier>,
    pub discard_operators: Vec<RuntimeDiscardOperator>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeWhitespaceDirective {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
    pub pattern: String,
    pub pattern_offset: usize,
    pub pattern_end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeWhitespaceModifier {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeDiscardOperator {
    pub kind: RuntimeDiscardOperatorKind,
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeDiscardOperatorKind {
    Enter,
    Exit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimePrettyDirective {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
    pub target: String,
    pub target_offset: usize,
    pub target_end: usize,
    pub payload: String,
    pub payload_offset: usize,
    pub payload_end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeHostCapture {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
    pub body: String,
    pub body_offset: usize,
    pub body_end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeProjection {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
    pub target: String,
    pub target_offset: usize,
    pub target_end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeTypedProjection {
    pub path: String,
    pub source_hash: String,
    pub offset: usize,
    pub end: usize,
    pub target: String,
    pub target_offset: usize,
    pub target_end: usize,
    pub type_text: String,
    pub type_offset: usize,
    pub type_end: usize,
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
            RuntimeConstructKind::TokenDirective | RuntimeConstructKind::PrettyDirective => {
                "BBNF-UNSUPPORTED-DIRECTIVE"
            }
            RuntimeConstructKind::Projection | RuntimeConstructKind::TypedProjection => {
                "BBNF-UNSUPPORTED-PROJECTION"
            }
            RuntimeConstructKind::HostCapture => "BBNF-UNSUPPORTED-HOST-CAPTURE",
            RuntimeConstructKind::Comma
            | RuntimeConstructKind::Import
            | RuntimeConstructKind::WhitespaceDirective
            | RuntimeConstructKind::WhitespaceModifier
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
        let mut layout = RuntimeFrontendLayout::default();
        let mut pretty_directives = Vec::new();
        let mut host_captures = Vec::new();
        let mut projections = Vec::new();
        let mut typed_projections = Vec::new();
        scan_runtime_source(
            source,
            &source_hash,
            &mut constructs,
            &mut imports,
            &mut layout,
            &mut pretty_directives,
            &mut host_captures,
            &mut projections,
            &mut typed_projections,
        )?;
        scans.push(RuntimeSourceScan {
            path: source.path.to_string(),
            source_hash,
            constructs,
            imports,
            layout,
            pretty_directives,
            host_captures,
            projections,
            typed_projections,
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
    layout: RuntimeFrontendLayout,
    pretty_directives: Vec<RuntimePrettyDirective>,
    host_captures: Vec<RuntimeHostCapture>,
    projections: Vec<RuntimeProjection>,
    typed_projections: Vec<RuntimeTypedProjection>,
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
    layout: &mut RuntimeFrontendLayout,
    pretty_directives: &mut Vec<RuntimePrettyDirective>,
    host_captures: &mut Vec<RuntimeHostCapture>,
    projections: &mut Vec<RuntimeProjection>,
    typed_projections: &mut Vec<RuntimeTypedProjection>,
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
        } else if rest.starts_with("@ws") && runtime_keyword_boundary(rest, 3) {
            let directive = parse_runtime_ws_directive(input.source, cursor, 3)?;
            layout
                .whitespace_directives
                .push(RuntimeWhitespaceDirective {
                    path: input.path.to_string(),
                    source_hash: source_hash.to_string(),
                    offset: cursor,
                    end: directive.end,
                    pattern: directive.pattern,
                    pattern_offset: directive.pattern_offset,
                    pattern_end: directive.pattern_end,
                });
            Some((RuntimeConstructKind::WhitespaceDirective, 3))
        } else if rest.starts_with("@pretty") && runtime_keyword_boundary(rest, 7) {
            let directive = parse_runtime_pretty_directive(input.source, cursor, 7)?;
            pretty_directives.push(RuntimePrettyDirective {
                path: input.path.to_string(),
                source_hash: source_hash.to_string(),
                offset: cursor,
                end: directive.end,
                target: directive.target,
                target_offset: directive.target_offset,
                target_end: directive.target_end,
                payload: directive.payload,
                payload_offset: directive.payload_offset,
                payload_end: directive.payload_end,
            });
            Some((
                RuntimeConstructKind::PrettyDirective,
                directive.end - cursor,
            ))
        } else if rest.starts_with("@{") {
            let capture = parse_runtime_host_capture(input.source, cursor)?;
            host_captures.push(RuntimeHostCapture {
                path: input.path.to_string(),
                source_hash: source_hash.to_string(),
                offset: cursor,
                end: capture.end,
                body: capture.body,
                body_offset: capture.body_offset,
                body_end: capture.body_end,
            });
            Some((RuntimeConstructKind::HostCapture, 2))
        } else if rest.starts_with("?w")
            && runtime_keyword_boundary(rest, 2)
            && runtime_has_left_operand(input.source, cursor)
            && runtime_has_layout_boundary_after(input.source, cursor + 2)
        {
            layout.whitespace_modifiers.push(RuntimeWhitespaceModifier {
                path: input.path.to_string(),
                source_hash: source_hash.to_string(),
                offset: cursor,
                end: cursor + 2,
            });
            Some((RuntimeConstructKind::WhitespaceModifier, 2))
        } else if rest.starts_with("?w") {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-WHITESPACE-MODIFIER",
                message: "malformed whitespace modifier; expected standalone ?w".to_string(),
                offset: cursor,
            });
        } else if rest.starts_with(">>>") || rest.starts_with("<<<") {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-DISCARD-OPERATOR",
                message: "malformed discard operator; expected >> or <<".to_string(),
                offset: cursor,
            });
        } else if rest.starts_with(">>")
            && runtime_has_left_operand(input.source, cursor)
            && runtime_has_right_operand(input.source, cursor + 2)
        {
            layout.discard_operators.push(RuntimeDiscardOperator {
                kind: RuntimeDiscardOperatorKind::Enter,
                path: input.path.to_string(),
                source_hash: source_hash.to_string(),
                offset: cursor,
                end: cursor + 2,
            });
            Some((RuntimeConstructKind::ShiftRight, 2))
        } else if rest.starts_with(">>") {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-DISCARD-OPERATOR",
                message: "malformed discard operator; expected operands around >>".to_string(),
                offset: cursor,
            });
        } else if rest.starts_with("<<")
            && runtime_has_left_operand(input.source, cursor)
            && runtime_has_right_operand(input.source, cursor + 2)
        {
            layout.discard_operators.push(RuntimeDiscardOperator {
                kind: RuntimeDiscardOperatorKind::Exit,
                path: input.path.to_string(),
                source_hash: source_hash.to_string(),
                offset: cursor,
                end: cursor + 2,
            });
            Some((RuntimeConstructKind::ShiftLeft, 2))
        } else if rest.starts_with("<<") {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-DISCARD-OPERATOR",
                message: "malformed discard operator; expected operands around <<".to_string(),
                offset: cursor,
            });
        } else if rest.starts_with("->") {
            let projection = parse_runtime_projection(input.source, cursor)?;
            let (kind, width) = if let Some(type_text) = projection.type_text {
                typed_projections.push(RuntimeTypedProjection {
                    path: input.path.to_string(),
                    source_hash: source_hash.to_string(),
                    offset: cursor,
                    end: projection.end,
                    target: projection.target,
                    target_offset: projection.target_offset,
                    target_end: projection.target_end,
                    type_text,
                    type_offset: projection.type_offset.unwrap(),
                    type_end: projection.type_end.unwrap(),
                });
                (
                    RuntimeConstructKind::TypedProjection,
                    projection.end - cursor,
                )
            } else {
                projections.push(RuntimeProjection {
                    path: input.path.to_string(),
                    source_hash: source_hash.to_string(),
                    offset: cursor,
                    end: projection.end,
                    target: projection.target,
                    target_offset: projection.target_offset,
                    target_end: projection.target_end,
                });
                (RuntimeConstructKind::Projection, projection.end - cursor)
            };
            Some((kind, width))
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeWsDirective {
    pattern: String,
    pattern_offset: usize,
    pattern_end: usize,
    end: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimePrettyDirectiveScan {
    target: String,
    target_offset: usize,
    target_end: usize,
    payload: String,
    payload_offset: usize,
    payload_end: usize,
    end: usize,
}

fn parse_runtime_ws_directive(
    source: &str,
    directive_offset: usize,
    directive_width: usize,
) -> Result<RuntimeWsDirective, GrammarError> {
    let mut cursor = directive_offset + directive_width;
    while cursor < source.len() {
        let Some(ch) = source[cursor..].chars().next() else {
            break;
        };
        if matches!(ch, ' ' | '\t' | '\r' | '\n') {
            cursor += ch.len_utf8();
        } else {
            break;
        }
    }
    if source.as_bytes().get(cursor).copied() != Some(b'/') {
        return Err(GrammarError::Parse {
            code: "BBNF-RUNTIME-WHITESPACE-DIRECTIVE",
            message: "runtime @ws directive requires a regex literal".to_string(),
            offset: directive_offset,
        });
    }
    let pattern_start = cursor + 1;
    cursor = pattern_start;
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
        if byte == b'/' {
            let pattern_end = cursor;
            cursor += 1;
            while cursor < source.len() {
                let Some(ch) = source[cursor..].chars().next() else {
                    break;
                };
                if matches!(ch, ' ' | '\t' | '\r') {
                    cursor += ch.len_utf8();
                } else {
                    break;
                }
            }
            if source.as_bytes().get(cursor).copied() != Some(b';') {
                return Err(GrammarError::Parse {
                    code: "BBNF-RUNTIME-WHITESPACE-DIRECTIVE",
                    message: "runtime @ws directive requires a terminating semicolon".to_string(),
                    offset: directive_offset,
                });
            }
            return Ok(RuntimeWsDirective {
                pattern: source[pattern_start..pattern_end].to_string(),
                pattern_offset: pattern_start,
                pattern_end,
                end: cursor + 1,
            });
        }
        cursor += 1;
    }
    Err(GrammarError::Parse {
        code: "BBNF-RUNTIME-WHITESPACE-DIRECTIVE",
        message: "unterminated runtime @ws regex literal".to_string(),
        offset: directive_offset,
    })
}

fn parse_runtime_pretty_directive(
    source: &str,
    directive_offset: usize,
    directive_width: usize,
) -> Result<RuntimePrettyDirectiveScan, GrammarError> {
    let mut cursor = directive_offset + directive_width;
    runtime_skip_inline_whitespace(source, &mut cursor);
    let (target, target_offset, target_end) = runtime_parse_target_or_star(source, cursor)
        .ok_or_else(|| GrammarError::Parse {
            code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
            message: "runtime @pretty directive requires a target".to_string(),
            offset: directive_offset,
        })?;
    cursor = target_end;
    runtime_skip_inline_whitespace(source, &mut cursor);
    let payload_offset = cursor;
    let mut saw_hint = false;
    loop {
        runtime_skip_inline_whitespace(source, &mut cursor);
        match source.as_bytes().get(cursor).copied() {
            Some(b';') | Some(b'.') => {
                if !saw_hint {
                    return Err(GrammarError::Parse {
                        code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
                        message: "runtime @pretty directive requires at least one hint".to_string(),
                        offset: directive_offset,
                    });
                }
                let payload_end = source[..cursor].trim_end().len();
                return Ok(RuntimePrettyDirectiveScan {
                    target,
                    target_offset,
                    target_end,
                    payload: source[payload_offset..payload_end].to_string(),
                    payload_offset,
                    payload_end,
                    end: cursor + 1,
                });
            }
            Some(b'\n') | None => {
                return Err(GrammarError::Parse {
                    code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
                    message: "runtime @pretty directive requires a terminating semicolon"
                        .to_string(),
                    offset: directive_offset,
                });
            }
            _ => {}
        }

        let Some((hint, _, hint_end)) = runtime_parse_ident(source, cursor) else {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
                message: "runtime @pretty directive has malformed hint payload".to_string(),
                offset: directive_offset,
            });
        };
        if !runtime_pretty_hint_allowed(&hint) {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
                message: format!("unknown runtime @pretty hint `{hint}`"),
                offset: directive_offset,
            });
        }
        cursor = hint_end;
        if source.as_bytes().get(cursor).copied() == Some(b'(') {
            cursor = runtime_skip_balanced(source, cursor, b'(', b')').ok_or_else(|| {
                GrammarError::Parse {
                    code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
                    message: "unterminated runtime @pretty hint arguments".to_string(),
                    offset: directive_offset,
                }
            })?;
        }
        saw_hint = true;
    }
}

fn runtime_pretty_hint_allowed(hint: &str) -> bool {
    matches!(
        hint,
        "blankline" | "block" | "compact" | "group" | "hardbreak" | "indent" | "sep"
    )
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeHostCaptureScan {
    body: String,
    body_offset: usize,
    body_end: usize,
    end: usize,
}

fn parse_runtime_host_capture(
    source: &str,
    capture_offset: usize,
) -> Result<RuntimeHostCaptureScan, GrammarError> {
    let body_offset = capture_offset + 2;
    let Some(end) = runtime_skip_balanced(source, capture_offset + 1, b'{', b'}') else {
        return Err(GrammarError::Parse {
            code: "BBNF-RUNTIME-HOST-CAPTURE",
            message: "unterminated runtime host capture".to_string(),
            offset: capture_offset,
        });
    };
    let body_end = end - 1;
    Ok(RuntimeHostCaptureScan {
        body: source[body_offset..body_end].to_string(),
        body_offset,
        body_end,
        end,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeProjectionScan {
    target: String,
    target_offset: usize,
    target_end: usize,
    type_text: Option<String>,
    type_offset: Option<usize>,
    type_end: Option<usize>,
    end: usize,
}

fn parse_runtime_projection(
    source: &str,
    projection_offset: usize,
) -> Result<RuntimeProjectionScan, GrammarError> {
    let mut target_offset = projection_offset + 2;
    runtime_skip_inline_whitespace(source, &mut target_offset);
    let mut cursor = target_offset;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut type_separator = None;

    while cursor < source.len() {
        let rest = &source[cursor..];
        if rest.starts_with("//") {
            break;
        }
        let byte = source.as_bytes()[cursor];
        if matches!(byte, b'"' | b'\'') {
            cursor = skip_quoted(source, cursor);
            continue;
        }
        if byte == b'/' && !rest.starts_with("//") {
            cursor = skip_regex(source, cursor);
            continue;
        }
        if paren_depth == 0
            && bracket_depth == 0
            && brace_depth == 0
            && matches!(byte, b'|' | b';' | b'\n' | b',')
        {
            break;
        }
        match byte {
            b'(' => paren_depth += 1,
            b')' => paren_depth = paren_depth.saturating_sub(1),
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'{' => brace_depth += 1,
            b'}' => brace_depth = brace_depth.saturating_sub(1),
            b':' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                let prev_colon = cursor > 0 && source.as_bytes()[cursor - 1] == b':';
                let next_colon = source.as_bytes().get(cursor + 1).copied() == Some(b':');
                if !prev_colon && !next_colon {
                    type_separator = Some(cursor);
                }
            }
            _ => {}
        }
        cursor += 1;
    }

    let end = source[..cursor].trim_end().len();
    let target_end = type_separator.map_or(end, |separator| source[..separator].trim_end().len());
    if target_offset >= target_end {
        return Err(GrammarError::Parse {
            code: "BBNF-RUNTIME-PROJECTION",
            message: "runtime projection requires a target".to_string(),
            offset: projection_offset,
        });
    }

    if let Some(separator) = type_separator {
        let mut type_offset = separator + 1;
        runtime_skip_inline_whitespace(source, &mut type_offset);
        if type_offset >= end {
            return Err(GrammarError::Parse {
                code: "BBNF-RUNTIME-TYPED-PROJECTION",
                message: "runtime typed projection requires a type suffix".to_string(),
                offset: projection_offset,
            });
        }
        Ok(RuntimeProjectionScan {
            target: source[target_offset..target_end].to_string(),
            target_offset,
            target_end,
            type_text: Some(source[type_offset..end].to_string()),
            type_offset: Some(type_offset),
            type_end: Some(end),
            end,
        })
    } else {
        Ok(RuntimeProjectionScan {
            target: source[target_offset..target_end].to_string(),
            target_offset,
            target_end,
            type_text: None,
            type_offset: None,
            type_end: None,
            end,
        })
    }
}

fn runtime_has_left_operand(source: &str, cursor: usize) -> bool {
    source[..cursor]
        .chars()
        .rev()
        .find(|ch| !ch.is_whitespace())
        .is_some_and(|ch| {
            ch == '"' || ch == '\'' || ch == '/' || ch == ')' || ch == ']' || is_ident_continue(ch)
        })
}

fn runtime_has_right_operand(source: &str, cursor: usize) -> bool {
    source[cursor..]
        .chars()
        .find(|ch| !ch.is_whitespace())
        .is_some_and(|ch| matches!(ch, '"' | '\'' | '/' | '(') || is_ident_start(ch))
}

fn runtime_has_layout_boundary_after(source: &str, cursor: usize) -> bool {
    match source[cursor..].chars().next() {
        Some(ch) => ch.is_whitespace() || matches!(ch, ',' | ';' | '|' | ')' | ']' | '}'),
        None => true,
    }
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
    let mut layout = RuntimeFrontendLayout::default();
    let mut pretty_directives = Vec::new();
    let mut host_captures = Vec::new();
    let mut projections = Vec::new();
    let mut typed_projections = Vec::new();
    for scan in scans {
        layout
            .whitespace_directives
            .extend(scan.layout.whitespace_directives.clone());
        layout
            .whitespace_modifiers
            .extend(scan.layout.whitespace_modifiers.clone());
        layout
            .discard_operators
            .extend(scan.layout.discard_operators.clone());
        pretty_directives.extend(scan.pretty_directives.clone());
        host_captures.extend(scan.host_captures.clone());
        projections.extend(scan.projections.clone());
        typed_projections.extend(scan.typed_projections.clone());
    }
    Ok(RuntimeFrontendClosure {
        source_hash,
        sources,
        imports,
        layout,
        pretty_directives,
        host_captures,
        projections,
        typed_projections,
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

fn runtime_skip_inline_whitespace(source: &str, cursor: &mut usize) {
    while *cursor < source.len() {
        let Some(ch) = source[*cursor..].chars().next() else {
            break;
        };
        if matches!(ch, ' ' | '\t' | '\r') {
            *cursor += ch.len_utf8();
        } else {
            break;
        }
    }
}

fn runtime_parse_target_or_star(source: &str, cursor: usize) -> Option<(String, usize, usize)> {
    if source.as_bytes().get(cursor).copied() == Some(b'*') {
        return Some(("*".to_string(), cursor, cursor + 1));
    }
    runtime_parse_ident(source, cursor)
}

fn runtime_parse_ident(source: &str, cursor: usize) -> Option<(String, usize, usize)> {
    let mut chars = source[cursor..].char_indices();
    let (_, first) = chars.next()?;
    if !is_ident_start(first) {
        return None;
    }
    let mut end = cursor + first.len_utf8();
    for (relative, ch) in chars {
        if is_ident_continue(ch) {
            end = cursor + relative + ch.len_utf8();
        } else {
            break;
        }
    }
    Some((source[cursor..end].to_string(), cursor, end))
}

fn runtime_skip_balanced(
    source: &str,
    open: usize,
    open_byte: u8,
    close_byte: u8,
) -> Option<usize> {
    if source.as_bytes().get(open).copied() != Some(open_byte) {
        return None;
    }
    let mut cursor = open + 1;
    let mut depth = 1usize;
    while cursor < source.len() {
        let rest = &source[cursor..];
        if rest.starts_with("//") {
            cursor += rest.find('\n').map_or(rest.len(), |index| index + 1);
            continue;
        }
        let byte = source.as_bytes()[cursor];
        if matches!(byte, b'"' | b'\'') {
            cursor = skip_quoted(source, cursor);
            continue;
        }
        if byte == b'/' && !rest.starts_with("//") {
            cursor = skip_regex(source, cursor);
            continue;
        }
        if byte == open_byte {
            depth += 1;
        } else if byte == close_byte {
            depth -= 1;
            if depth == 0 {
                return Some(cursor + 1);
            }
        }
        cursor += 1;
    }
    None
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
                    if self.source[self.cursor..].starts_with("?w") {
                        return Err(self.error_at(
                            "BBNF-DIRECTIVE-NOT-IN-SKINNY",
                            self.cursor,
                            "whitespace modifier ?w is not available in the skinny compiler"
                                .to_string(),
                        ));
                    }
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
fn w5b_frontend_layout_contract_lowers_to_request_facts() {
    let source = "@ws /\\s*/ ;\nroot = ident ?w ;\n";
    let facts =
        parse_runtime_source_facts(&[RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", source)])
            .expect("layout facts lower");

    assert_eq!(facts.count(RuntimeConstructKind::WhitespaceDirective), 1);
    assert_eq!(facts.count(RuntimeConstructKind::WhitespaceModifier), 1);
    assert!(facts.first_unsupported().is_none());
    assert_eq!(facts.frontend.layout.whitespace_directives.len(), 1);
    let directive = &facts.frontend.layout.whitespace_directives[0];
    assert_eq!(directive.path, "grammar/css/l4/stylesheet.bbnf");
    assert_eq!(directive.source_hash, stable_hash(source));
    assert_eq!(directive.offset, source.find("@ws").unwrap());
    assert_eq!(directive.end, source.find(';').unwrap() + 1);
    assert_eq!(directive.pattern, "\\s*");
    assert_eq!(directive.pattern_offset, source.find("\\s*").unwrap());
    assert_eq!(directive.pattern_end, directive.pattern_offset + 3);

    assert_eq!(
        facts.frontend.layout.whitespace_modifiers,
        vec![RuntimeWhitespaceModifier {
            path: "grammar/css/l4/stylesheet.bbnf".to_string(),
            source_hash: stable_hash(source),
            offset: source.find("?w").unwrap(),
            end: source.find("?w").unwrap() + 2,
        }]
    );
}

#[cfg(test)]
#[test]
fn w5b_frontend_public_ws_remains_retired() {
    let err = parse_grammar("bad", "@ws /\\s*/ ;\nroot = \"x\" ;").unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-DIRECTIVE-NOT-IN-SKINNY",
            ..
        }
    ));

    let err = parse_grammar("bad", "w = \" \" ;\nroot = ident ?w ;\nident = \"x\" ;").unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-DIRECTIVE-NOT-IN-SKINNY",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_malformed_whitespace_modifier_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "root = ident ?word ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-WHITESPACE-MODIFIER",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_discard_operators_lower_to_request_facts() {
    let source = "root = \"(\" >> ident << \")\" ;\n";
    let facts =
        parse_runtime_source_facts(&[RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", source)])
            .expect("discard facts lower");

    assert_eq!(facts.count(RuntimeConstructKind::ShiftRight), 1);
    assert_eq!(facts.count(RuntimeConstructKind::ShiftLeft), 1);
    assert_eq!(
        facts.frontend.layout.discard_operators,
        vec![
            RuntimeDiscardOperator {
                kind: RuntimeDiscardOperatorKind::Enter,
                path: "grammar/css/l4/stylesheet.bbnf".to_string(),
                source_hash: stable_hash(source),
                offset: source.find(">>").unwrap(),
                end: source.find(">>").unwrap() + 2,
            },
            RuntimeDiscardOperator {
                kind: RuntimeDiscardOperatorKind::Exit,
                path: "grammar/css/l4/stylesheet.bbnf".to_string(),
                source_hash: stable_hash(source),
                offset: source.find("<<").unwrap(),
                end: source.find("<<").unwrap() + 2,
            },
        ]
    );
}

#[cfg(test)]
#[test]
fn w5b_frontend_malformed_discard_operator_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "root = \"(\" >>> ident ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-DISCARD-OPERATOR",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_pretty_span_projection_lower_to_request_facts() {
    let source = r#"
@pretty ruleList blankline ;
@pretty funcArgs group indent sep(", ") ;

root = @{ "url" , "(" >> ident ?w , "," ?w , ident << ")" } ;
tag = "from" -> 0u8 | "hex" -> crate::css_types::parse_hex_color(input) : u32 ;
"#;
    let facts =
        parse_runtime_source_facts(&[RuntimeSource::new("grammar/css/l4/stylesheet.bbnf", source)])
            .expect("pretty/span/projection facts lower");

    assert_eq!(facts.count(RuntimeConstructKind::PrettyDirective), 2);
    assert_eq!(facts.count(RuntimeConstructKind::HostCapture), 1);
    assert_eq!(facts.count(RuntimeConstructKind::Projection), 1);
    assert_eq!(facts.count(RuntimeConstructKind::TypedProjection), 1);
    assert_eq!(facts.count(RuntimeConstructKind::ShiftRight), 1);
    assert_eq!(facts.count(RuntimeConstructKind::ShiftLeft), 1);
    assert_eq!(facts.count(RuntimeConstructKind::WhitespaceModifier), 2);

    assert_eq!(facts.frontend.pretty_directives.len(), 2);
    assert_eq!(
        facts.frontend.pretty_directives[0],
        RuntimePrettyDirective {
            path: "grammar/css/l4/stylesheet.bbnf".to_string(),
            source_hash: stable_hash(source),
            offset: source.find("@pretty ruleList").unwrap(),
            end: source.find("@pretty funcArgs").unwrap() - 1,
            target: "ruleList".to_string(),
            target_offset: source.find("ruleList").unwrap(),
            target_end: source.find("ruleList").unwrap() + "ruleList".len(),
            payload: "blankline".to_string(),
            payload_offset: source.find("blankline").unwrap(),
            payload_end: source.find("blankline").unwrap() + "blankline".len(),
        }
    );
    assert_eq!(facts.frontend.pretty_directives[1].target, "funcArgs");
    assert_eq!(
        facts.frontend.pretty_directives[1].payload,
        "group indent sep(\", \")"
    );

    assert_eq!(facts.frontend.host_captures.len(), 1);
    let capture = &facts.frontend.host_captures[0];
    assert_eq!(capture.offset, source.find("@{").unwrap());
    assert_eq!(capture.end, source.find("} ;").unwrap() + 1);
    assert!(capture.body.contains(">> ident ?w"));
    assert!(capture.body.contains("ident <<"));

    assert_eq!(
        facts.frontend.projections,
        vec![RuntimeProjection {
            path: "grammar/css/l4/stylesheet.bbnf".to_string(),
            source_hash: stable_hash(source),
            offset: source.find("-> 0u8").unwrap(),
            end: source.find(" |").unwrap(),
            target: "0u8".to_string(),
            target_offset: source.find("0u8").unwrap(),
            target_end: source.find("0u8").unwrap() + "0u8".len(),
        }]
    );
    assert_eq!(
        facts.frontend.typed_projections,
        vec![RuntimeTypedProjection {
            path: "grammar/css/l4/stylesheet.bbnf".to_string(),
            source_hash: stable_hash(source),
            offset: source
                .find("-> crate::css_types::parse_hex_color(input)")
                .unwrap(),
            end: source.rfind(" ;").unwrap(),
            target: "crate::css_types::parse_hex_color(input)".to_string(),
            target_offset: source
                .find("crate::css_types::parse_hex_color(input)")
                .unwrap(),
            target_end: source.find(" : u32").unwrap(),
            type_text: "u32".to_string(),
            type_offset: source.find("u32").unwrap(),
            type_end: source.find("u32").unwrap() + "u32".len(),
        }]
    );
}

#[cfg(test)]
#[test]
fn w5b_frontend_unknown_pretty_payload_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "@pretty stylesheet vapor ;\nroot = \"x\" ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-PRETTY-DIRECTIVE",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_host_capture_unterminated_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "root = @{ \"url\" , \"(\" ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-HOST-CAPTURE",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_projection_malformed_target_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "root = \"x\" -> ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-PROJECTION",
            ..
        }
    ));
}

#[cfg(test)]
#[test]
fn w5b_frontend_typed_projection_malformed_type_fails_closed() {
    let err = parse_runtime_source_facts(&[RuntimeSource::new(
        "grammar/css/l4/stylesheet.bbnf",
        "root = \"x\" -> input : ;\n",
    )])
    .unwrap_err();
    assert!(matches!(
        err,
        GrammarError::Parse {
            code: "BBNF-RUNTIME-TYPED-PROJECTION",
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
