//! Grammar extraction: BBNF struct-direct document → observational
//! `GrammarExtract` + pipeline-direct walkers.
//!
//! Walks the top-level `grammar` root view and assembles either the
//! observational [`crate::types::GrammarExtract`] (for LSP analysis /
//! gorgeous formatting / debug binaries) or the pipeline-direct
//! `(AST, DirectiveMaps)` pair (for `compile_grammar`). The `directive`
//! wrapper is peeled to expose the specific directive
//! [`crate::runtime::bbnf::BbnfCompoundKind`] (`ImportDirective`,
//! `PrettyDirective`, etc.), and each directive is extracted by
//! walking its data-bearing children.
//!
//! All structural CST traversal lives in the view-layer accessors
//! ([`BbnfView::span_text`], [`BbnfView::compound_kind`],
//! [`BbnfView::children`], [`BbnfView::child`]). This module only
//! handles the *typed*-grammar mapping (CST view → public
//! `crate::types::*` structs and pipeline-internal
//! [`DirectiveMaps`]).

use std::borrow::Cow;

use parse_that::Span;

use crate::lower::view_walk::{find_descendant_by_kind, find_rhs_expression_descendant};
use crate::pipeline::directives::DirectiveMaps;
use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfDocument, BbnfKind, BbnfView};
use crate::types::*;

/// Sink abstraction for absorbing a parsed grammar's rules and directives.
trait GrammarSink<'a> {
    fn insert_rule(&mut self, name: &'a str, entry: RuleEntry<'a>);
    fn push_import(&mut self, imp: ImportDirective<'a>);
    fn push_recover(&mut self, rec: RecoverDirective<'a>);
    fn push_pretty(&mut self, pretty: PrettyDirective<'a>);
    fn push_token_name(&mut self, name: Cow<'a, str>);
    fn push_debug_name(&mut self, name: Cow<'a, str>);
    fn set_ws_pattern(&mut self, pattern: Cow<'a, str>);
    fn push_host_fn(&mut self, decl: HostFnDecl<'a>);
}

/// Observational sink: accumulates every directive as a `Vec<_>` inside a
/// [`GrammarExtract`] for LSP analysis / gorgeous / debug callers.
struct ExtractSink<'a> {
    grammar: GrammarExtract<'a>,
}

impl<'a> GrammarSink<'a> for ExtractSink<'a> {
    fn insert_rule(&mut self, name: &'a str, entry: RuleEntry<'a>) {
        self.grammar.rules.insert(name, entry);
    }
    fn push_import(&mut self, imp: ImportDirective<'a>) {
        self.grammar.imports.push(imp);
    }
    fn push_recover(&mut self, rec: RecoverDirective<'a>) {
        self.grammar.recovers.push(rec);
    }
    fn push_pretty(&mut self, pretty: PrettyDirective<'a>) {
        self.grammar.pretties.push(pretty);
    }
    fn push_token_name(&mut self, name: Cow<'a, str>) {
        self.grammar.token_rules.push(name);
    }
    fn push_debug_name(&mut self, name: Cow<'a, str>) {
        self.grammar.debug_rules.push(name);
    }
    fn set_ws_pattern(&mut self, pattern: Cow<'a, str>) {
        self.grammar.ws_pattern = Some(pattern);
    }
    fn push_host_fn(&mut self, decl: HostFnDecl<'a>) {
        self.grammar.host_fns.push(decl);
    }
}

/// Pipeline sink: lands directives directly in the compile-shaped
/// containers.
struct PipelineSink<'a> {
    ast: AST<'a>,
    directives: DirectiveMaps<'a>,
    imports: Vec<ImportDirective<'a>>,
}

impl<'a> GrammarSink<'a> for PipelineSink<'a> {
    fn insert_rule(&mut self, name: &'a str, entry: RuleEntry<'a>) {
        self.ast.insert(name, entry);
    }
    fn push_import(&mut self, imp: ImportDirective<'a>) {
        self.imports.push(imp);
    }
    fn push_recover(&mut self, rec: RecoverDirective<'a>) {
        self.directives
            .recover_map_mut()
            .insert(rec.rule_name.to_string(), rec.sync_expr);
    }
    fn push_pretty(&mut self, pretty: PrettyDirective<'a>) {
        let hints: Vec<String> = pretty.hints.into_iter().map(|h| h.into_owned()).collect();
        self.directives
            .pretty_map_mut()
            .insert(pretty.rule_name.into_owned(), hints);
    }
    fn push_token_name(&mut self, name: Cow<'a, str>) {
        self.directives.token_set_mut().insert(name.into_owned());
    }
    fn push_debug_name(&mut self, name: Cow<'a, str>) {
        if name.as_ref() == "*" {
            self.directives.set_debug_all(true);
        } else {
            self.directives.debug_set_mut().insert(name.into_owned());
        }
    }
    fn set_ws_pattern(&mut self, pattern: Cow<'a, str>) {
        self.directives.set_ws_pattern(pattern.into_owned());
    }
    fn push_host_fn(&mut self, decl: HostFnDecl<'a>) {
        self.directives.host_map_mut().insert(
            decl.name.into_owned(),
            decl.return_type.map(|t| t.into_owned()),
        );
    }
}

/// Observational extraction: build a full [`GrammarExtract`] from a
/// finished struct-direct parse.
pub fn extract_observational<'a>(doc: &'a BbnfDocument<'a>) -> GrammarExtract<'a> {
    let mut sink = ExtractSink {
        grammar: GrammarExtract::empty(),
    };
    walk_document(doc, &mut sink);
    sink.grammar
}

/// Pipeline-direct extraction: build `(AST, DirectiveMaps, imports)`
/// straight from the BBNF document, skipping the [`GrammarExtract`]
/// intermediate.
pub(crate) fn extract_for_pipeline<'a>(
    doc: &'a BbnfDocument<'a>,
) -> (AST<'a>, DirectiveMaps<'a>, Vec<ImportDirective<'a>>) {
    let mut sink = PipelineSink {
        ast: indexmap::IndexMap::new(),
        directives: DirectiveMaps::default(),
        imports: Vec::new(),
    };
    walk_document(doc, &mut sink);
    (sink.ast, sink.directives, sink.imports)
}

/// Drive the single structural walk over the grammar root, dispatching
/// each peeled top-level item into the sink.
fn walk_document<'a, S: GrammarSink<'a>>(doc: &'a BbnfDocument<'a>, sink: &mut S) {
    let root = doc.view();

    // `grammar = ( grammar_item ?w ) *` — under structural emission
    // the per-iteration `?w` modifier expands into a Seq wrapping
    // `[WsTrim, X, WsTrim]`; the WsTrim states emit no records, so
    // the iteration compound has exactly one record-emitting direct
    // child — the X subtree. `peel_iter_wrapper` handles this
    // shape uniformly with the bare `grammar_item *` shape.
    for item in root.children() {
        let peeled = peel_iter_wrapper(item);
        let inner = peel_wrappers(peeled);
        absorb_item(inner, sink);
    }
}

/// Peel a single structural-invisible wrapper introduced by
/// `OptionalWhitespace(X)` lowering inside a Repeat body.
fn peel_iter_wrapper<'a>(item: BbnfView<'a, 'a>) -> BbnfView<'a, 'a> {
    // The wrapper's compound_kind is `Other` (anonymous structural
    // grouping) and its single substantive child is the X subtree.
    if !matches!(item.compound_kind(), Some(BbnfCompoundKind::Other)) {
        return item;
    }
    let Some(first) = item.child(0) else {
        return item;
    };
    if first.kind() != BbnfKind::Compound {
        return item;
    }
    let mut iter = item.children();
    iter.next();
    if iter.next().is_some() {
        return item;
    }
    first
}

/// Peel the transparent `grammar_item` and `directive` wrappers.
fn peel_wrappers<'a>(node: BbnfView<'a, 'a>) -> BbnfView<'a, 'a> {
    match node.compound_kind() {
        Some(BbnfCompoundKind::GrammarItem) | Some(BbnfCompoundKind::Directive) => {
            if let Some(child) = node.child(0) {
                peel_wrappers(child)
            } else {
                node
            }
        }
        _ => node,
    }
}

/// Route a single peeled grammar item — rule or directive — into the sink.
fn absorb_item<'a, S: GrammarSink<'a>>(item: BbnfView<'a, 'a>, sink: &mut S) {
    // Rules: `rule = identifier "=" rhs ";"`. The rule compound
    // carries the identifier as a Span leaf and the rhs as a
    // compound subtree.
    if matches!(item.compound_kind(), Some(BbnfCompoundKind::Rule)) {
        let rule_text = item.span_text();
        let (lo, _hi) = item.byte_span().unwrap_or((0, 0));
        // Identifier text: leading `[_a-zA-Z][\w]*` run from the
        // rule span start.
        let name_len = rule_text
            .bytes()
            .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
            .count();
        let name: &str = &rule_text[..name_len];
        let input = item.input();
        let name_span = ::parse_that::Span::new(lo as usize, lo as usize + name_len, input);
        let rhs =
            find_rhs_expression_descendant(item).expect("rule: missing rhs expression descendant");
        sink.insert_rule(name, RuleEntry { name_span, rhs });
        return;
    }

    // Directives: dispatch on compound kind.
    match item.compound_kind() {
        Some(BbnfCompoundKind::RecoverDirective) => {
            if let Some(rec) = decode_recover(item) {
                sink.push_recover(rec);
            }
        }
        Some(BbnfCompoundKind::PrettyDirective) => {
            if let Some(pretty) = decode_pretty(item) {
                sink.push_pretty(pretty);
            }
        }
        Some(BbnfCompoundKind::TokenDirective) => {
            if let Some(name) = decode_single_name(item, "@token") {
                sink.push_token_name(name);
            }
        }
        Some(BbnfCompoundKind::DebugDirective) => {
            if let Some(name) = decode_single_name(item, "@debug") {
                sink.push_debug_name(name);
            }
        }
        Some(BbnfCompoundKind::WsDirective) => {
            if let Some(pattern) = decode_ws(item) {
                sink.set_ws_pattern(pattern);
            }
        }
        Some(BbnfCompoundKind::HostDirective) => {
            if let Some(decl) = decode_host(item) {
                sink.push_host_fn(decl);
            }
        }
        Some(BbnfCompoundKind::ImportDirective) => {
            sink.push_import(decode_import(item));
        }
        _ => {}
    }
}

// ------------------------------------------------------------------
// Structural directive decoders.
// ------------------------------------------------------------------

/// Find the first non-empty Span-leaf descendant — the bbnf
/// `identifier` projection, when present. Used to recover the
/// rule-name target of `@token` / `@debug` / `@recover` / `@host`
/// directives.
fn find_first_identifier<'a>(view: BbnfView<'a, 'a>) -> Option<&'a str> {
    fn descend<'a>(view: BbnfView<'a, 'a>, out: &mut Option<&'a str>) {
        if out.is_some() {
            return;
        }
        if view.kind() == BbnfKind::Span {
            let s = view.span_text();
            let trimmed = s.trim();
            // Reject directive keyword leaves and the `*` wildcard
            // — `@token` / `@debug` / `@recover` / `@host` keyword
            // literals project to Span leaves under struct-direct,
            // and the wildcard branch surfaces as a `*` Span leaf.
            // Identifier characters are `[_a-zA-Z][_a-zA-Z0-9-]*`.
            if !trimmed.is_empty() && is_identifier(trimmed) {
                *out = Some(trimmed);
                return;
            }
        }
        for child in view.children() {
            descend(child, out);
            if out.is_some() {
                return;
            }
        }
    }
    let mut out = None;
    descend(view, &mut out);
    out
}

/// True iff `s` matches the bbnf `identifier` regex
/// `[_a-zA-Z][_a-zA-Z0-9-]*`.
fn is_identifier(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    if !(bytes[0].is_ascii_alphabetic() || bytes[0] == b'_') {
        return false;
    }
    bytes
        .iter()
        .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
}

/// `@recover ruleName syncExpr ;` — extract rule_name and sync_expr.
fn decode_recover<'a>(item: BbnfView<'a, 'a>) -> Option<RecoverDirective<'a>> {
    let (lo, hi) = item.byte_span().unwrap_or((0, 0));
    let input = item.input();
    let name = find_first_identifier(item)?;
    let sync_expr = find_rhs_expression_descendant(item).unwrap_or(item);
    Some(RecoverDirective {
        rule_name: name,
        sync_expr,
        span: Span::new(lo as usize, hi as usize, input),
    })
}

/// `@pretty target hint* ;` — first identifier is target, remaining
/// pretty_hint compound descendants are hints.
fn decode_pretty<'a>(item: BbnfView<'a, 'a>) -> Option<PrettyDirective<'a>> {
    let (lo, hi) = item.byte_span().unwrap_or((0, 0));
    let input = item.input();

    let target_text = find_first_identifier(item).unwrap_or("*");

    let mut hints: Vec<Cow<'a, str>> = Vec::new();
    collect_pretty_hint_descendants(item, &mut hints);

    Some(PrettyDirective {
        rule_name: Cow::Owned(target_text.to_string()),
        hints,
        span: Span::new(lo as usize, hi as usize, input),
    })
}

/// Collect every `pretty_hint` compound descendant's text into `out`.
fn collect_pretty_hint_descendants<'a>(view: BbnfView<'a, 'a>, out: &mut Vec<Cow<'a, str>>) {
    if matches!(view.compound_kind(), Some(BbnfCompoundKind::PrettyHint)) {
        out.push(pretty_hint_text(view));
        return;
    }
    for c in view.children() {
        collect_pretty_hint_descendants(c, out);
    }
}

/// Directives with the shape `@keyword (identifier | "*") ;` —
/// `@token` (identifier only) and `@debug` (identifier or `*`).
///
/// The `*` wildcard branch projects no identifier child; we scan
/// the directive's span text after stripping the leading keyword.
fn decode_single_name<'a>(item: BbnfView<'a, 'a>, keyword: &str) -> Option<Cow<'a, str>> {
    if let Some(name) = find_first_identifier(item) {
        if !name.is_empty() {
            return Some(Cow::Owned(name.to_string()));
        }
    }
    let text = item.span_text();
    let body = text.strip_prefix(keyword).unwrap_or(text);
    if body.trim_start().starts_with('*') {
        Some(Cow::Borrowed("*"))
    } else {
        None
    }
}

/// `@ws /regex/ ;` — find the regex leaf and strip the surrounding
/// `/` delimiters.
fn decode_ws<'a>(item: BbnfView<'a, 'a>) -> Option<Cow<'a, str>> {
    fn scan<'a>(node: BbnfView<'a, 'a>) -> Option<Cow<'a, str>> {
        let text = node.span_text();
        if let Some(stripped) = text.strip_prefix('/').and_then(|s| s.strip_suffix('/')) {
            return Some(Cow::Borrowed(stripped));
        }
        for c in node.children() {
            if let Some(s) = scan(c) {
                return Some(s);
            }
        }
        None
    }
    for child in item.children() {
        if let Some(s) = scan(child) {
            return Some(s);
        }
    }
    None
}

/// `@host name [: type] ;` — first identifier is name, optional
/// type annotation follows.
fn decode_host<'a>(item: BbnfView<'a, 'a>) -> Option<HostFnDecl<'a>> {
    let name = find_first_identifier(item)?;
    if name.is_empty() {
        return None;
    }
    // Optional type annotation: descend looking for any subtree
    // beginning with `:`.
    let return_type = find_type_annotation(item).map(|s| Cow::Owned(s.trim().to_string()));
    Some(HostFnDecl {
        name: Cow::Owned(name.to_string()),
        return_type,
    })
}

/// Find the type-annotation payload (everything after the `:` token)
/// for an `@host name : type ;` directive.
fn find_type_annotation<'a>(view: BbnfView<'a, 'a>) -> Option<&'a str> {
    fn descend<'a>(view: BbnfView<'a, 'a>) -> Option<&'a str> {
        let text = view.span_text();
        let trimmed = text.trim();
        if let Some(rest) = trimmed.strip_prefix(':') {
            let rest = rest.trim();
            // Drop a trailing `;` if it surfaces in the same span.
            let rest = rest.strip_suffix(';').unwrap_or(rest).trim();
            if !rest.is_empty() && is_type_name(rest) {
                return Some(rest);
            }
        }
        for child in view.children() {
            if let Some(found) = descend(child) {
                return Some(found);
            }
        }
        None
    }
    descend(view)
}

/// True iff `s` looks like a bbnf type-name (alphanumeric / `_`
/// / `-`).
fn is_type_name(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() {
        return false;
    }
    if !(bytes[0].is_ascii_alphabetic() || bytes[0] == b'_') {
        return false;
    }
    bytes
        .iter()
        .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
}

/// Decode an `import_directive` compound into its typed form.
fn decode_import<'a>(item: BbnfView<'a, 'a>) -> ImportDirective<'a> {
    let (lo, hi) = item.byte_span().unwrap_or((0, 0));
    let directive_span = Span::new(lo as usize, hi as usize, item.input());

    let path_view = find_descendant_by_kind(item, BbnfCompoundKind::ImportPath)
        .expect("import_directive: missing import_path descendant");

    let (path_lo, path_hi) = path_view.byte_span().unwrap_or((0, 0));
    let path_raw = &path_view.input()[path_lo as usize..path_hi as usize];
    let path_str = strip_quotes(path_raw);

    let items_view = find_descendant_by_kind(item, BbnfCompoundKind::ImportItems);

    let names: Option<Vec<ImportedName<'a>>> = items_view.map(|items| {
        let mut out = Vec::new();
        collect_identifier_spans(items, &mut out);
        out
    });

    ImportDirective {
        path: Cow::Borrowed(path_str),
        span: directive_span,
        items: names,
    }
}

/// Collect every Span-leaf descendant matching the bbnf `identifier`
/// regex into `out` as `ImportedName`. Used to extract names from an
/// `import_items` compound.
fn collect_identifier_spans<'a>(view: BbnfView<'a, 'a>, out: &mut Vec<ImportedName<'a>>) {
    if view.kind() == BbnfKind::Span {
        let text = view.span_text();
        let trimmed = text.trim();
        if !trimmed.is_empty() && is_identifier(trimmed) {
            // Recover byte-offset span for the trimmed identifier.
            if let Some((lo, hi)) = view.byte_span() {
                // The view's span is the leaf's full text; trimming
                // shifts the start. Find trimmed offset relative to
                // raw span text.
                let raw = &view.input()[lo as usize..hi as usize];
                let leading_ws = raw.len() - raw.trim_start().len();
                let trailing_ws = raw.len() - raw.trim_end().len();
                let span_lo = lo as usize + leading_ws;
                let span_hi = hi as usize - trailing_ws;
                let span = ::parse_that::Span::new(span_lo, span_hi, view.input());
                out.push(ImportedName {
                    name: Cow::Borrowed(trimmed),
                    span,
                });
            }
        }
        return;
    }
    for child in view.children() {
        collect_identifier_spans(child, out);
    }
}

fn strip_quotes(raw: &str) -> &str {
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        &raw[1..raw.len() - 1]
    } else {
        raw
    }
}

/// Format a single `pretty_hint` node as `name` or `name(arg)`.
///
/// `pretty_hint = identifier , ( "(" , /[^)]*/ , ")" ) ?`. The codegen
/// alt_dispatch / flat-shape parser pushes the identifier as a Span
/// child but doesn't surface the optional `( ... )` argument group as
/// a separate child compound — the `(`, regex content, and `)` are
/// consumed without being deposited on the open frame. Recover the
/// argument text by scanning forward in the input from the identifier
/// span's end when the dedicated arg-group child is absent.
fn pretty_hint_text<'a>(node: BbnfView<'a, 'a>) -> Cow<'a, str> {
    if matches!(node.compound_kind(), Some(BbnfCompoundKind::PrettyHint)) {
        let ident = node.child(0).expect("pretty_hint: missing identifier");
        let name = ident.span_text().trim();
        // Path A: the parser surfaced the optional arg group as a
        // dedicated child compound (legacy bootstrap_parser shape).
        if let Some(arg_group) = node.child(1) {
            if let Some((lo, hi)) = arg_group.byte_span() {
                if hi > lo {
                    let arg = &arg_group.input()[lo as usize..hi as usize];
                    return Cow::Owned(format!("{}{}", name, arg.trim()));
                }
            }
        }
        // Path B: the alt_dispatch codegen path consumed `( ... )`
        // without pushing a separate compound. Scan forward from the
        // identifier span's end to recover any trailing `(...)`.
        if let Some((_, ident_hi)) = ident.byte_span() {
            let input = ident.input();
            let after = &input[ident_hi as usize..];
            let trimmed = after.trim_start();
            if trimmed.starts_with('(') {
                if let Some(close) = trimmed.find(')') {
                    let arg = &trimmed[..=close];
                    return Cow::Owned(format!("{}{}", name, arg.trim()));
                }
            }
        }
        return Cow::Owned(name.to_string());
    }
    Cow::Owned(node.span_text().trim().to_string())
}
