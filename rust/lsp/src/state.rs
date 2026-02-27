use std::collections::{HashMap, HashSet};

use bbnf::grammar::{BBNFGrammar, Expression, Token, AST};
use bbnf::analysis::{tarjan_scc, compute_first_sets, find_first_set_conflicts, find_aliases, CharSet};
use bbnf::generate::{calculate_ast_deps, get_nonterminal_name};

use self_cell::self_cell;

/// Extract the inner value from a TokenExpression (single expression).
fn get_inner_expression<'a, T>(tok: &'a Token<'a, T>) -> &'a T {
    &tok.value
}

use crate::analysis::LineIndex;

use tower_lsp_server::ls_types::*;

/// Information about a single production rule.
#[derive(Debug, Clone)]
pub struct RuleInfo {
    /// The rule name (LHS nonterminal).
    pub name: String,
    /// Byte offset range of the LHS name.
    pub name_span: (usize, usize),
    /// Byte offset range of the entire rule (LHS = RHS ;).
    pub full_span: (usize, usize),
    /// Pretty-printed RHS for hover display.
    pub rhs_text: String,
    /// All nonterminal references in the RHS.
    pub references: Vec<ReferenceInfo>,
}

/// A reference to a nonterminal in the RHS of a rule.
#[derive(Debug, Clone)]
pub struct ReferenceInfo {
    /// The referenced nonterminal name.
    pub name: String,
    /// Byte offset range of this reference.
    pub span: (usize, usize),
}

/// Semantic token data for a single token.
#[derive(Debug, Clone)]
pub struct SemanticTokenInfo {
    pub span: (usize, usize),
    pub token_type: u32,
}

/// Owned representation of an import directive.
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The path string from the import.
    pub path: String,
    /// Byte offset range of the entire import directive.
    pub span: (usize, usize),
    /// If `Some`, selective import; if `None`, glob import.
    pub items: Option<Vec<String>>,
}

/// Pre-analyzed document state — all data is owned (no lifetimes).
#[derive(Debug, Clone)]
pub struct DocumentInfo {
    pub rules: Vec<RuleInfo>,
    pub diagnostics: Vec<Diagnostic>,
    /// name → index into `rules`
    pub rule_index: HashMap<String, usize>,
    /// Semantic tokens in document order.
    pub semantic_tokens: Vec<SemanticTokenInfo>,
    /// FIRST set labels per rule name (formatted for display).
    pub first_set_labels: HashMap<String, String>,
    /// Rules that can derive the empty string.
    pub nullable_rules: HashSet<String>,
    /// Rules that participate in a cycle, with their cycle path.
    pub cyclic_rule_paths: HashMap<String, String>,
    /// Import directives parsed from the document.
    pub imports: Vec<ImportInfo>,
}

/// Diagnostic info extracted from the parser state (owned, no lifetimes).
#[derive(Debug, Clone)]
pub struct ParseDiagnostics {
    /// Parser offset after parsing (how far it consumed).
    pub offset: usize,
    /// Furthest offset reached during parsing (for error reporting).
    pub furthest_offset: usize,
    /// If parsing panicked, the panic message.
    pub panic_message: Option<String>,
}

// Self-referential struct: owns the source text and the parsed AST that borrows from it.
self_cell! {
    struct OwnedAst {
        owner: String,
        #[covariant]
        dependent: CachedAst,
    }
}

type CachedAst<'a> = Option<CachedParseResult<'a>>;

/// Holds both the parsed grammar and import directives (borrows from OwnedAst's owner).
struct CachedParseResult<'a> {
    ast: AST<'a>,
    imports: Vec<ImportInfo>,
}

/// Parse the source text once, returning the cached AST data and diagnostic info.
/// Both are extracted from a single parse call.
fn parse_once(src: &str) -> (Option<CachedParseResult<'_>>, ParseDiagnostics) {
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let parser = BBNFGrammar::grammar_with_imports();
        parser.parse_return_state(src)
    }));

    match parse_result {
        Ok((result, parser_state)) => {
            let diag = ParseDiagnostics {
                offset: parser_state.offset,
                furthest_offset: parser_state.furthest_offset,
                panic_message: None,
            };
            let cached = result.map(|pg| {
                let imports = pg.imports.iter().map(|imp| ImportInfo {
                    path: imp.path.to_string(),
                    span: (imp.span.start, imp.span.end),
                    items: imp.items.as_ref().map(|items| {
                        items.iter().map(|i| i.to_string()).collect()
                    }),
                }).collect();
                CachedParseResult {
                    ast: pg.rules,
                    imports,
                }
            });
            (cached, diag)
        }
        Err(panic_info) => {
            let msg = if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else {
                "Internal parser error".to_string()
            };
            let diag = ParseDiagnostics {
                offset: 0,
                furthest_offset: 0,
                panic_message: Some(msg),
            };
            (None, diag)
        }
    }
}

/// Stored per-document: raw text + analyzed info + precomputed line index + cached AST.
pub struct DocumentState {
    pub text: String,
    pub info: DocumentInfo,
    /// Precomputed line-start offsets for O(log n) position conversion.
    pub line_index: LineIndex,
    /// Cached parsed AST (self-referential: borrows from its own String).
    ast_cell: OwnedAst,
}

impl DocumentState {
    pub fn new(text: String) -> Self {
        let line_index = LineIndex::new(&text);
        // Parse once: the OwnedAst captures the AST, and we extract diagnostics
        // via a Cell to avoid double-parsing.
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();
        let info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );
        Self { text, info, line_index, ast_cell }
    }

    pub fn update(&mut self, text: String) {
        let line_index = LineIndex::new(&text);
        let parse_diag = std::cell::Cell::new(None::<ParseDiagnostics>);
        let ast_cell = OwnedAst::new(text.clone(), |src| {
            let (cached, diag) = parse_once(src);
            parse_diag.set(Some(diag));
            cached
        });
        let diag = parse_diag.into_inner().unwrap();
        self.info = analyze_from_cache(
            &text,
            &line_index,
            ast_cell.borrow_dependent().as_ref(),
            &diag,
        );
        self.line_index = line_index;
        self.ast_cell = ast_cell;
        self.text = text;
    }

    /// Access the cached AST (borrows from internally-owned text).
    pub fn ast(&self) -> Option<&AST<'_>> {
        self.ast_cell.borrow_dependent().as_ref().map(|c| &c.ast)
    }
}

/// Semantic token type indices matching our legend.
#[allow(dead_code)]
pub mod token_types {
    pub const RULE_DEFINITION: u32 = 0;
    pub const RULE_REFERENCE: u32 = 1;
    pub const STRING: u32 = 2;
    pub const REGEXP: u32 = 3;
    pub const OPERATOR: u32 = 4;
    pub const KEYWORD: u32 = 5;
    pub const COMMENT: u32 = 6;
}

/// Analyze a BBNF document using pre-parsed AST and diagnostics from `parse_once()`.
/// This avoids double-parsing: the OwnedAst parses once, and we reuse its results here.
fn analyze_from_cache(
    text: &str,
    line_index: &LineIndex,
    cached: Option<&CachedParseResult<'_>>,
    parse_diag: &ParseDiagnostics,
) -> DocumentInfo {
    let mut rules = Vec::new();
    let mut diagnostics = Vec::new();
    let mut rule_index = HashMap::new();
    let mut semantic_tokens = Vec::new();

    // Handle parser panic.
    if let Some(msg) = &parse_diag.panic_message {
        let pos = Position::new(0, 0);
        diagnostics.push(Diagnostic {
            range: Range::new(pos, pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: format!("Parse error: {}", msg),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: Vec::new(),
        };
    }

    let Some(parsed) = cached else {
        // Parse failure — report error at furthest offset.
        let offset = parse_diag.furthest_offset.max(parse_diag.offset);
        let pos = line_index.offset_to_position(offset);
        diagnostics.push(Diagnostic {
            range: Range::new(pos, pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: format!(
                "Parse error at offset {} (line {}, col {})",
                offset,
                pos.line + 1,
                pos.character + 1
            ),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: Vec::new(),
        };
    };

    // Check for incomplete parse (didn't consume all input).
    if parse_diag.offset < text.len() {
        let remaining = &text[parse_diag.offset..];
        if !remaining.trim().is_empty() {
            let pos = line_index.offset_to_position(parse_diag.offset);
            diagnostics.push(Diagnostic {
                range: Range::new(pos, pos),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("bbnf".into()),
                message: "Unexpected input after last rule".into(),
                ..Default::default()
            });
        }
    }

    let ast = &parsed.ast;
    let import_infos = parsed.imports.clone();

    // Check for empty AST on non-empty input — likely a parse failure not caught above.
    if ast.is_empty() && !text.trim().is_empty() && import_infos.is_empty() {
        let furthest = parse_diag.furthest_offset.max(parse_diag.offset);
        let pos = line_index.offset_to_position(furthest.min(text.len()));
        diagnostics.push(Diagnostic {
            range: Range::new(Position::new(0, 0), pos),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("bbnf".into()),
            message: "Failed to parse any rules. Check syntax (each rule needs: name = expression ;)".into(),
            ..Default::default()
        });
        return DocumentInfo {
            rules,
            diagnostics,
            rule_index,
            semantic_tokens,
            first_set_labels: HashMap::new(),
            nullable_rules: HashSet::new(),
            cyclic_rule_paths: HashMap::new(),
            imports: import_infos,
        };
    }

    // Extract rule info from AST.
    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, span: name_span, .. }) = lhs {
            let name_str = name.to_string();
            let name_byte_span = (name_span.start, name_span.end);

            // Compute full span (from LHS start to RHS end).
            let full_start = name_span.start;
            let full_end = compute_expression_end(rhs).unwrap_or(name_span.end);

            // Collect nonterminal references in RHS.
            let mut references = Vec::new();
            collect_references(rhs, &mut references);

            // Collect semantic tokens from RHS.
            collect_semantic_tokens(rhs, &mut semantic_tokens);

            // Semantic token for rule definition (LHS).
            semantic_tokens.push(SemanticTokenInfo {
                span: name_byte_span,
                token_type: token_types::RULE_DEFINITION,
            });

            // Pretty-print RHS for hover.
            let rhs_text = format_expression_short(rhs);

            // Check for duplicate rule.
            if let Some(&_existing_idx) = rule_index.get(&name_str) {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(name_byte_span.0, name_byte_span.1),
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("bbnf".into()),
                    message: format!("Duplicate rule: `{}`", name_str),
                    related_information: None, // TODO: populate with correct URI from caller
                    ..Default::default()
                });
            }

            let idx = rules.len();
            rule_index.insert(name_str.clone(), idx);

            rules.push(RuleInfo {
                name: name_str,
                name_span: name_byte_span,
                full_span: (full_start, full_end),
                rhs_text,
                references,
            });
        }
    }

    // Diagnostics: undefined nonterminals and unused rules.
    let defined: HashMap<&str, usize> = rules
        .iter()
        .enumerate()
        .map(|(i, r)| (r.name.as_str(), i))
        .collect();

    // Build set of names available via @import directives.
    let imported_names: HashSet<&str> = import_infos
        .iter()
        .flat_map(|imp| {
            imp.items
                .as_ref()
                .map(|items| items.iter().map(|s| s.as_str()).collect::<Vec<_>>())
                .unwrap_or_default()
        })
        .collect();

    let mut referenced_names: std::collections::HashSet<&str> =
        std::collections::HashSet::new();

    for rule in &rules {
        for refinfo in &rule.references {
            referenced_names.insert(&refinfo.name);
            if !defined.contains_key(refinfo.name.as_str())
                && !imported_names.contains(refinfo.name.as_str())
            {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(refinfo.span.0, refinfo.span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!("Undefined rule: `{}`", refinfo.name),
                    ..Default::default()
                });
            }
        }
    }

    for rule in &rules {
        if !referenced_names.contains(rule.name.as_str()) && rules.len() > 1 {
            // Don't flag the first rule — it's typically the entry point.
            if rule_index.get(rule.name.as_str()) != Some(&0) {
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("bbnf".into()),
                    message: format!("Unused rule: `{}`", rule.name),
                    tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                    ..Default::default()
                });
            }
        }
    }

    // Left recursion detection via dependency analysis.
    let deps = calculate_ast_deps(&ast);
    let scc = tarjan_scc(&deps);

    // Build cycle path strings and emit enhanced cycle diagnostics.
    let mut cyclic_rule_paths = HashMap::new();
    for scc_group in &scc.sccs {
        // An SCC with >1 member means all members are cyclic.
        // An SCC with 1 member is cyclic only if it self-references (checked via cyclic_rules).
        let is_multi = scc_group.len() > 1;
        let cyclic_members: Vec<&str> = scc_group
            .iter()
            .filter_map(|e| {
                let name = get_nonterminal_name(e)?;
                if is_multi || scc.cyclic_rules.contains(*e) {
                    Some(name)
                } else {
                    None
                }
            })
            .collect();

        if cyclic_members.is_empty() {
            continue;
        }

        for &member in &cyclic_members {
            let path = if cyclic_members.len() == 1 {
                // Self-recursive rule.
                format!("{} \u{2192} {}", member, member)
            } else {
                // Multi-member SCC — reconstruct a representative cycle path.
                build_cycle_path(member, &cyclic_members, &deps)
            };

            cyclic_rule_paths.insert(member.to_string(), path.clone());

            if let Some(&idx) = rule_index.get(member) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Rule `{}` participates in a cycle: {}",
                        member, path
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // FIRST set computation for inlay hints (SCC-ordered, O(n+E)).
    let first_sets = compute_first_sets(&ast, &deps, &scc);

    let mut first_set_labels = HashMap::new();
    let mut nullable_rules = HashSet::new();

    for (lhs, rhs) in ast.iter() {
        if let Expression::Nonterminal(Token { value: name, .. }) = lhs {
            let name_str = name.to_string();

            if let Some(cs) = first_sets.first.get(lhs) {
                first_set_labels.insert(name_str.clone(), format_charset(cs));
            }

            if first_sets.nullable.contains(lhs) {
                nullable_rules.insert(name_str.clone());
            }

            // Enhanced diagnostic: empty rule body detection.
            if is_empty_rhs(rhs) {
                if let Some(&idx) = rule_index.get(name.as_ref()) {
                    let rule = &rules[idx];
                    diagnostics.push(Diagnostic {
                        range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                        severity: Some(DiagnosticSeverity::WARNING),
                        source: Some("bbnf".into()),
                        message: format!("Rule `{}` has an empty body", name),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // FIRST set conflict detection for ambiguous alternations.
    let conflicts = find_first_set_conflicts(&ast, &first_sets);
    for (rule_name, rule_conflicts) in &conflicts {
        if let Some(&idx) = rule_index.get(rule_name.as_str()) {
            let rule = &rules[idx];
            for conflict in rule_conflicts {
                let overlap_str = format_charset(&conflict.overlap);
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::WARNING),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Alternation in `{}` has ambiguous FIRST sets: branches {} and {} both start with {}",
                        rule_name,
                        conflict.branch_a + 1,
                        conflict.branch_b + 1,
                        overlap_str
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // Alias detection: rules whose RHS is just a nonterminal reference.
    let aliases = find_aliases(&ast, &scc.cyclic_rules);
    for (alias_lhs, target) in &aliases {
        if let (Some(alias_name), Some(target_name)) = (
            get_nonterminal_name(alias_lhs),
            get_nonterminal_name(target),
        ) {
            // Skip aliases of imported rules (intentional re-exports).
            if imported_names.contains(alias_name) {
                continue;
            }
            if let Some(&idx) = rule_index.get(alias_name) {
                let rule = &rules[idx];
                diagnostics.push(Diagnostic {
                    range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("bbnf".into()),
                    message: format!(
                        "Rule `{}` is an alias of `{}` — consider using `{}` directly",
                        alias_name, target_name, target_name
                    ),
                    ..Default::default()
                });
            }
        }
    }

    // Unreachable rule detection via BFS from root rules.
    let reachable = compute_reachable_rules(&rules, &rule_index);
    for rule in &rules {
        // Skip the first rule (entry point) and already-unused rules.
        if rule_index.get(rule.name.as_str()) == Some(&0) {
            continue;
        }
        if !referenced_names.contains(rule.name.as_str()) {
            // Already flagged as unused — no need to also flag as unreachable.
            continue;
        }
        if !reachable.contains(rule.name.as_str()) {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(rule.name_span.0, rule.name_span.1),
                severity: Some(DiagnosticSeverity::HINT),
                source: Some("bbnf".into()),
                message: format!(
                    "Rule `{}` is unreachable from the entry rule",
                    rule.name
                ),
                tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                ..Default::default()
            });
        }
    }

    // Sort semantic tokens by offset for encoding.
    semantic_tokens.sort_by_key(|t| t.span.0);

    DocumentInfo {
        rules,
        diagnostics,
        rule_index,
        semantic_tokens,
        first_set_labels,
        nullable_rules,
        cyclic_rule_paths,
        imports: import_infos,
    }
}

/// Public convenience wrapper: parses the text and analyzes in one step.
/// Used by tests and any code that doesn't need the cached AST.
pub fn analyze(text: &str, line_index: &LineIndex) -> DocumentInfo {
    let (cached, diag) = parse_once(text);
    analyze_from_cache(text, line_index, cached.as_ref(), &diag)
}

/// Format a byte as a display-friendly character for inlay hints.
fn format_char(b: u8) -> String {
    match b {
        b'\t' => "\\t".into(),
        b'\n' => "\\n".into(),
        b'\r' => "\\r".into(),
        b' ' => "SP".into(),
        0x0b => "\\v".into(),
        0x0c => "\\f".into(),
        c if c.is_ascii_graphic() => format!("'{}'", c as char),
        c => format!("0x{:02x}", c),
    }
}

/// Format a CharSet as a human-readable string for inlay hints.
fn format_charset(cs: &CharSet) -> String {
    if cs.is_empty() {
        return "{}".into();
    }

    let chars: Vec<u8> = cs.iter().collect();

    // Try to detect ranges.
    let mut parts: Vec<String> = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        let start = chars[i];
        let mut end = start;
        while i + 1 < chars.len() && chars[i + 1] == end + 1 {
            end = chars[i + 1];
            i += 1;
        }
        if end - start >= 2 {
            parts.push(format!("{}..{}", format_char(start), format_char(end)));
        } else if end > start {
            parts.push(format_char(start));
            parts.push(format_char(end));
        } else {
            parts.push(format_char(start));
        }
        i += 1;
    }

    // Truncate if too many parts.
    if parts.len() > 6 {
        let truncated: Vec<&str> = parts.iter().take(5).map(|s| s.as_str()).collect();
        format!("{{{}, ...}}", truncated.join(", "))
    } else {
        format!("{{{}}}", parts.join(", "))
    }
}

/// Check if a rule RHS is effectively empty (epsilon only).
fn is_empty_rhs(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::Epsilon(_) => true,
        Expression::Rule(rhs, _) => is_empty_rhs(rhs),
        _ => false,
    }
}

/// Public wrapper for `compute_expression_end` (used by selection_range).
pub fn compute_expression_end_pub(expr: &Expression<'_>) -> Option<usize> {
    compute_expression_end(expr)
}

/// Recursively collect nonterminal references from an expression.
fn collect_references(expr: &Expression<'_>, refs: &mut Vec<ReferenceInfo>) {
    match expr {
        Expression::Nonterminal(tok) => {
            refs.push(ReferenceInfo {
                name: tok.value.to_string(),
                span: (tok.span.start, tok.span.end),
            });
        }
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            for child in get_inner_expression(inner) {
                collect_references(child, refs);
            }
        }
        Expression::Skip(l, r) | Expression::Next(l, r) | Expression::Minus(l, r) => {
            collect_references(get_inner_expression(l), refs);
            collect_references(get_inner_expression(r), refs);
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner) => {
            collect_references(get_inner_expression(inner), refs);
        }
        Expression::Rule(rhs, _) => {
            collect_references(rhs, refs);
        }
        Expression::MappedExpression((expr_tok, _)) => {
            collect_references(get_inner_expression(expr_tok), refs);
        }
        Expression::DebugExpression((expr_tok, _)) => {
            collect_references(get_inner_expression(expr_tok), refs);
        }
        _ => {}
    }
}

/// Collect semantic tokens from an expression tree.
fn collect_semantic_tokens(expr: &Expression<'_>, tokens: &mut Vec<SemanticTokenInfo>) {
    match expr {
        Expression::Nonterminal(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::RULE_REFERENCE,
            });
        }
        Expression::Literal(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::STRING,
            });
        }
        Expression::Regex(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::REGEXP,
            });
        }
        Expression::Epsilon(tok) => {
            tokens.push(SemanticTokenInfo {
                span: (tok.span.start, tok.span.end),
                token_type: token_types::KEYWORD,
            });
        }
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            for child in get_inner_expression(inner) {
                collect_semantic_tokens(child, tokens);
            }
        }
        Expression::Skip(l, r) | Expression::Next(l, r) | Expression::Minus(l, r) => {
            collect_semantic_tokens(get_inner_expression(l), tokens);
            collect_semantic_tokens(get_inner_expression(r), tokens);
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner) => {
            collect_semantic_tokens(get_inner_expression(inner), tokens);
        }
        Expression::Rule(rhs, _) => {
            collect_semantic_tokens(rhs, tokens);
        }
        Expression::MappedExpression((expr_tok, _)) => {
            collect_semantic_tokens(get_inner_expression(expr_tok), tokens);
        }
        Expression::DebugExpression((expr_tok, _)) => {
            collect_semantic_tokens(get_inner_expression(expr_tok), tokens);
        }
        _ => {}
    }
}

/// Compute the end byte offset of an expression.
fn compute_expression_end(expr: &Expression<'_>) -> Option<usize> {
    match expr {
        Expression::Literal(tok)
        | Expression::Nonterminal(tok)
        | Expression::Regex(tok) => Some(tok.span.end),
        Expression::MappingFn(tok) => Some(tok.span.end),
        Expression::Epsilon(tok) => Some(tok.span.end),
        Expression::Alternation(inner) | Expression::Concatenation(inner) => {
            get_inner_expression(inner)
                .last()
                .and_then(|e| compute_expression_end(e))
                .or(Some(inner.span.end))
        }
        Expression::Skip(_, r) | Expression::Next(_, r) | Expression::Minus(_, r) => {
            compute_expression_end(get_inner_expression(r))
                .or(Some(r.span.end))
        }
        Expression::Group(inner)
        | Expression::Optional(inner)
        | Expression::Many(inner)
        | Expression::Many1(inner)
        | Expression::OptionalWhitespace(inner) => {
            Some(inner.span.end)
        }
        Expression::Rule(rhs, mapping) => {
            if let Some(m) = mapping {
                compute_expression_end(m)
            } else {
                compute_expression_end(rhs)
            }
        }
        Expression::ProductionRule(_, rhs) => compute_expression_end(rhs),
        Expression::MappedExpression((_, mapping_tok)) => {
            Some(mapping_tok.span.end)
        }
        Expression::DebugExpression((expr_tok, _)) => {
            compute_expression_end(get_inner_expression(expr_tok))
        }
    }
}

/// Quick one-line formatting of an expression for hover text.
fn format_expression_short(expr: &Expression<'_>) -> String {
    match expr {
        Expression::Literal(tok) => format!("\"{}\"", tok.value),
        Expression::Nonterminal(tok) => tok.value.to_string(),
        Expression::Regex(tok) => format!("/{}/", tok.value),
        Expression::Epsilon(_) => "ε".into(),
        Expression::Group(inner) => {
            format!("({})", format_expression_short(get_inner_expression(inner)))
        }
        Expression::Optional(inner) => {
            format!("[{}]", format_expression_short(get_inner_expression(inner)))
        }
        Expression::OptionalWhitespace(inner) => {
            format!("{}?w", format_expression_short(get_inner_expression(inner)))
        }
        Expression::Many(inner) => {
            format!("{{{}}}",  format_expression_short(get_inner_expression(inner)))
        }
        Expression::Many1(inner) => {
            format!("{}+", format_expression_short(get_inner_expression(inner)))
        }
        Expression::Skip(l, r) => {
            format!(
                "{} << {}",
                format_expression_short(get_inner_expression(l)),
                format_expression_short(get_inner_expression(r))
            )
        }
        Expression::Next(l, r) => {
            format!(
                "{} >> {}",
                format_expression_short(get_inner_expression(l)),
                format_expression_short(get_inner_expression(r))
            )
        }
        Expression::Minus(l, r) => {
            format!(
                "{} - {}",
                format_expression_short(get_inner_expression(l)),
                format_expression_short(get_inner_expression(r))
            )
        }
        Expression::Concatenation(inner) => get_inner_expression(inner)
            .iter()
            .map(|e| format_expression_short(e))
            .collect::<Vec<_>>()
            .join(", "),
        Expression::Alternation(inner) => get_inner_expression(inner)
            .iter()
            .map(|e| format_expression_short(e))
            .collect::<Vec<_>>()
            .join(" | "),
        Expression::Rule(rhs, _) => format_expression_short(rhs),
        Expression::ProductionRule(lhs, rhs) => {
            format!(
                "{} = {}",
                format_expression_short(lhs),
                format_expression_short(rhs)
            )
        }
        Expression::MappedExpression((expr_tok, _)) => {
            format_expression_short(get_inner_expression(expr_tok))
        }
        Expression::DebugExpression((expr_tok, _)) => {
            format_expression_short(get_inner_expression(expr_tok))
        }
        Expression::MappingFn(tok) => format!("=> {}", tok.value),
    }
}

/// Build a representative cycle path string for a rule within its SCC.
///
/// Walks the dependency graph from `start` following only edges within the SCC members,
/// until it returns to `start`, producing a string like "expr → term → factor → expr".
fn build_cycle_path(start: &str, scc_members: &[&str], deps: &HashMap<Expression<'_>, HashSet<Expression<'_>>>) -> String {
    let member_set: HashSet<&str> = scc_members.iter().copied().collect();
    let mut path = vec![start];
    let mut visited = HashSet::new();
    visited.insert(start);
    let mut current = start;

    loop {
        // Find the dependency expression key for `current`.
        let mut found_next = false;
        for (key, dep_set) in deps {
            if let Some(name) = get_nonterminal_name(key) {
                if name == current {
                    // Walk deps looking for a member of the SCC.
                    for dep in dep_set {
                        if let Some(dep_name) = get_nonterminal_name(dep) {
                            if dep_name == start && path.len() > 1 {
                                // Completed the cycle.
                                path.push(start);
                                return path.join(" \u{2192} ");
                            }
                            if member_set.contains(dep_name) && !visited.contains(dep_name) {
                                visited.insert(dep_name);
                                path.push(dep_name);
                                current = dep_name;
                                found_next = true;
                                break;
                            }
                        }
                    }
                    break;
                }
            }
        }
        if !found_next {
            // Couldn't extend the path — close the cycle back to start.
            path.push(start);
            return path.join(" \u{2192} ");
        }
    }
}

/// Compute the set of rule names reachable from root rules via BFS.
///
/// Root rules are: the first rule in the grammar, plus any rule referenced by an import.
fn compute_reachable_rules(
    rules: &[RuleInfo],
    rule_index: &HashMap<String, usize>,
) -> HashSet<String> {
    let mut reachable = HashSet::new();

    if rules.is_empty() {
        return reachable;
    }

    // The first rule is the entry point / root.
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(rules[0].name.clone());
    reachable.insert(rules[0].name.clone());

    // BFS from root rules.
    while let Some(current) = queue.pop_front() {
        // Find all rules referenced by `current`.
        if let Some(&idx) = rule_index.get(&current) {
            let rule = &rules[idx];
            for refinfo in &rule.references {
                if !reachable.contains(&refinfo.name) {
                    reachable.insert(refinfo.name.clone());
                    queue.push_back(refinfo.name.clone());
                }
            }
        }
    }

    reachable
}
