//! Grammar extraction: bootstrap tape → observational `GrammarExtract` +
//! pipeline-direct tape walkers.
//!
//! Walks the top-level `grammar` root view and assembles either the
//! observational [`crate::types::GrammarExtract`] (for LSP analysis /
//! gorgeous JIT / debug binaries) or the pipeline-direct
//! `(AST, DirectiveMaps)` pair (for `compile_grammar`). The `directive`
//! wrapper is peeled to expose the specific directive `rule_kind()`
//! (`import_directive`, `pretty_directive`, etc.), and each directive
//! is extracted by walking its data-bearing children — the tape-first
//! emitter elides keyword literals, so only semantic nodes appear.
//!
//! All structural CST traversal lives in the view-layer cursor
//! accessors (`span_text`, `identifier_span`, `rule_kind`,
//! `children`, `child`). This module only handles the
//! *typed*-grammar mapping (CST view → public `crate::types::*`
//! structs and pipeline-internal [`DirectiveMaps`]).
//!
//! Tranche AU.4.1: the historical `extract_grammar` → `ParsedGrammar`
//! round-trip is gone. The compile pipeline now calls
//! [`extract_for_pipeline`], which walks the tape exactly once and
//! lands results straight in pipeline-shaped containers — no
//! intermediate grab-bag struct on the hot path. The observational
//! path ([`extract_observational`]) builds `GrammarExtract` for
//! tooling.

use std::borrow::Cow;

use parse_that::Span;

use super::generated::{BbnfBootstrap, BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::{find_descendant_by_kind, find_rhs_expression_descendant};
use crate::pipeline::directives::DirectiveMaps;
use crate::runtime::Parsed;
use crate::types::*;

/// Sink abstraction for absorbing a parsed grammar's rules and directives.
///
/// The tape walker ([`walk_tape`]) is shape-agnostic: it dispatches
/// each grammar item into a sink. Observational callers instantiate
/// [`ExtractSink`] to build a [`GrammarExtract`]; pipeline callers
/// instantiate [`PipelineSink`] to populate [`DirectiveMaps`] + `AST`
/// directly, skipping the intermediate vector allocations that
/// [`GrammarExtract`] demands.
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
/// containers. Imports are siphoned off because the loader / single-file
/// compile path treats them as a pre-merge side channel, not a map.
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
/// finished tape-first parse.
///
/// Consumed by LSP analysis, the gorgeous JIT driver, and the
/// `debug_parse` binary. The compile pipeline bypasses this function
/// in favour of [`extract_for_pipeline`] to avoid the intermediate
/// vector allocation.
pub fn extract_observational<'a>(
    parsed: &'a Parsed<'a, BbnfBootstrap>,
) -> GrammarExtract<'a> {
    let mut sink = ExtractSink {
        grammar: GrammarExtract::empty(),
    };
    walk_tape(parsed, &mut sink);
    sink.grammar
}

/// Pipeline-direct extraction: build `(AST, DirectiveMaps, imports)`
/// straight from the bootstrap tape, skipping the [`GrammarExtract`]
/// intermediate.
///
/// `imports` is returned separately because the single-file compile
/// path rejects any non-empty list (it does not resolve `@import`),
/// and the multi-file import loader needs the vector before per-module
/// merging. Keeping them on a side channel makes the flow explicit.
pub(crate) fn extract_for_pipeline<'a>(
    parsed: &'a Parsed<'a, BbnfBootstrap>,
) -> (AST<'a>, DirectiveMaps<'a>, Vec<ImportDirective<'a>>) {
    let mut sink = PipelineSink {
        ast: indexmap::IndexMap::new(),
        directives: DirectiveMaps::default(),
        imports: Vec::new(),
    };
    walk_tape(parsed, &mut sink);
    (sink.ast, sink.directives, sink.imports)
}

/// Drive the single structural walk over the grammar root, dispatching
/// each peeled top-level item into the sink.
fn walk_tape<'a, S: GrammarSink<'a>>(parsed: &'a Parsed<'a, BbnfBootstrap>, sink: &mut S) {
    let root = parsed.view();

    // `grammar = ( grammar_item ?w ) *` — under the DTA lifter the
    // per-iteration `?w` modifier expands `OptionalWhitespace(X)`
    // into a `Seq([WsTrim, X, WsTrim])`. The tape surface:
    //
    //     Rule compound (grammar rule entry; `parsed.view()`)
    //       └─ Seq compound (one per iteration wrapper; the
    //          OptionalWhitespace lowering)
    //            └─ X subtree (the grammar_item; WsTrim states emit
    //               no records)
    //
    // For grammars whose entry body is a bare `grammar_item *`
    // (no `?w`) the Seq wrapper is absent and iteration children
    // are grammar_items directly — `peel_iter_wrapper` handles both
    // shapes uniformly.
    //
    // TODO-postAW: collapse these structural-invisible Seqs at the
    // lifter (or mark them transparent) so consumers don't re-peel.
    // See the `iter_rep_children` helper in `crate::lower::tape_walk`
    // for the existing consumer-side mirror.
    use crate::runtime::tape::TapeKind;
    for item in root.children() {
        // An explicit `TapeKind::Repeat` wrapper — legacy fn-per-rule
        // emission retained this shape in a few places. Under the DTA
        // `frame_to_tape_kind(Repeat) == Rule`, so this branch is
        // effectively dead, but we keep it so the walker tolerates
        // shape shifts after regen.
        if item.kind() == TapeKind::Repeat {
            for grandchild in item.children() {
                let peeled = peel_iter_wrapper(grandchild);
                let inner = peel_wrappers(peeled);
                absorb_item(inner, sink);
            }
        } else {
            let peeled = peel_iter_wrapper(item);
            let inner = peel_wrappers(peeled);
            absorb_item(inner, sink);
        }
    }
}

/// Peel a single structural-invisible Seq wrapper introduced by
/// `OptionalWhitespace(X)` lowering inside a Repeat body.
///
/// The DTA lifter expands `OptionalWhitespace(X)` to
/// `Seq([WsTrim, X, WsTrim])`. `WsTrim` states emit no tape
/// records, so the iteration compound has exactly one record-
/// emitting direct child — the `X` subtree. When `item.kind()` is
/// `TapeKind::Seq` AND the compound has exactly one compound child,
/// peel to that child. Otherwise return `item` unchanged so
/// user-written Seq compounds (an `identifier "=" rhs ";"`-style
/// concatenation that reaches the walker directly) round-trip.
fn peel_iter_wrapper<'a>(item: BbnfBootstrapNodeView<'a>) -> BbnfBootstrapNodeView<'a> {
    use crate::runtime::tape::TapeKind;
    if item.kind() != TapeKind::Seq {
        return item;
    }
    // The Seq must have a compound first child and no siblings to
    // safely peel — multi-child Seqs are user-written concatenations.
    let Some(first) = item.child(0) else {
        return item;
    };
    if !matches!(
        first.kind(),
        TapeKind::Rule | TapeKind::Alt | TapeKind::Repeat | TapeKind::Seq,
    ) {
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
///
/// Both `grammar_item` and `directive` are alternation-wrappers whose
/// sole child carries the specific variant tag (`rule`, `import_directive`,
/// `pretty_directive`, etc.). Peeling both layers lets `absorb_item`
/// match on the concrete `rule_kind()` directly.
fn peel_wrappers<'a>(node: BbnfBootstrapNodeView<'a>) -> BbnfBootstrapNodeView<'a> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::grammar_item | BbnfBootstrapRuleKind::directive => {
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
fn absorb_item<'a, S: GrammarSink<'a>>(
    item: BbnfBootstrapNodeView<'a>,
    sink: &mut S,
) {
    // Rules: `rule = identifier "=" rhs ";"`. Under tape-first,
    // literal matches don't push records — and `identifier` may
    // also be elided at the tape level if it's a transparent /
    // fused rule whose body is inlined. So the rule compound can
    // have one or two direct children:
    //   - child(0) = identifier (when present)
    //   - child(0) = rhs (when identifier was elided)
    //   - child(0) = identifier, child(1) = rhs (both present)
    //
    // We identify the identifier by its `rule_kind`. The name
    // text comes from the leading token of the rule span.
    if item.rule_kind() == BbnfBootstrapRuleKind::rule {
        let rule_text = item.span_text();
        let (lo, _hi) = item.span();
        // Identifier text: leading `[_a-zA-Z][\w]*` run from the rule
        // span start. The bbnf grammar guarantees a rule starts with
        // its identifier.
        let name_len = rule_text
            .bytes()
            .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
            .count();
        let name: &str = &rule_text[..name_len];
        let input = item.input();
        let name_span = ::parse_that::Span::new(
            lo as usize,
            lo as usize + name_len,
            input,
        );
        // The RHS is the `rhs` expression-head descendant.
        // `rule = lhs , "=" ?w , rhs ?w , ( ";" | "." ) ;` — picking
        // the last non-identifier child erroneously lands on the
        // terminator Alt under DTA's structural wrapping, so we
        // descend explicitly.
        //
        // `rhs` is a transparent `closure | alternation` Wrap alias.
        // Walker tape preserves the named rhs compound; shape-
        // authoritative tape elides it (the Wrap shape emitter
        // dispatches directly to the chosen branch). `find_rhs_
        // expression_descendant` handles both — see tape_walk.rs.
        let rhs = find_rhs_expression_descendant(item)
            .expect("rule: missing rhs expression descendant");
        sink.insert_rule(
            name,
            RuleEntry {
                name_span,
                rhs,
            },
        );
        return;
    }

    // Directives: dispatch on `rule_kind()`. The `item` has been
    // peeled through `grammar_item` / `directive` wrappers, so it
    // points AT the specific directive compound. Each directive is
    // extracted structurally from its children — the tape-first
    // emitter elides literal slots (`@pretty`, `@token`, `;`, etc.),
    // so the child list contains only the semantically meaningful
    // data nodes (identifier, regex, hints compound, etc.).
    match item.rule_kind() {
        BbnfBootstrapRuleKind::recover_directive => {
            if let Some(rec) = decode_recover(item) {
                sink.push_recover(rec);
            }
        }
        BbnfBootstrapRuleKind::pretty_directive => {
            if let Some(pretty) = decode_pretty(item) {
                sink.push_pretty(pretty);
            }
        }
        BbnfBootstrapRuleKind::token_directive => {
            if let Some(name) = decode_single_name(item) {
                sink.push_token_name(name);
            }
        }
        BbnfBootstrapRuleKind::debug_directive => {
            if let Some(name) = decode_single_name(item) {
                sink.push_debug_name(name);
            }
        }
        BbnfBootstrapRuleKind::ws_directive => {
            if let Some(pattern) = decode_ws(item) {
                sink.set_ws_pattern(pattern);
            }
        }
        BbnfBootstrapRuleKind::host_directive => {
            if let Some(decl) = decode_host(item) {
                sink.push_host_fn(decl);
            }
        }
        BbnfBootstrapRuleKind::import_directive => {
            sink.push_import(decode_import(item));
        }
        _ => {}
    }
}

// ------------------------------------------------------------------
// Structural directive decoders — walk children by `rule_kind()`.
// Each returns the typed directive; the sink decides where it lands.
// ------------------------------------------------------------------

/// `@recover ruleName syncExpr ;` — extract rule_name (first
/// identifier descendant) and sync_expr (the rhs expression-head
/// descendant).
///
/// Under DTA, the identifier is nested inside the Seq wrappers the
/// lifter emits for the rule body; a descendant search handles that.
/// The sync expression is a full `rhs` body (the directive grammar
/// is `"@recover" ?w , identifier ?w , rhs ?w , ( ";" | "." ) ?`);
/// we pick it via `find_rhs_expression_descendant` so the
/// `metadata.rs::build_rule_meta` caller can hand it straight to
/// `lower_rhs` under either walker-routed tape (rhs compound
/// preserved) or shape-authoritative tape (rhs Wrap elided; the
/// closure / alternation branch is the direct descendant). Falls
/// back to the whole directive when no expression is present —
/// `dispatch_expression`'s directive arm returns Epsilon in that
/// case rather than panic.
fn decode_recover<'a>(item: BbnfBootstrapNodeView<'a>) -> Option<RecoverDirective<'a>> {
    let (lo, hi) = item.span();
    let input = item.input();
    let name_node = find_descendant_by_kind(item, BbnfBootstrapRuleKind::identifier)?;
    let sync_expr = find_rhs_expression_descendant(item).unwrap_or(item);
    Some(RecoverDirective {
        rule_name: name_node.span_text(),
        sync_expr,
        span: Span::new(lo as usize, hi as usize, input),
    })
}

/// `@pretty target hint* ;` — first identifier descendant is target,
/// remaining pretty_hint descendants are hints.
///
/// Under the DTA lowering, `pretty_directive` compound's children are
/// inner Seq/Alt/Repeat wrappers (one per component of the directive
/// grammar) — the target identifier and the pretty_hint compounds
/// sit two or three levels deep. `find_descendant_by_kind` + a
/// hint-collection descent walk handle both shapes.
fn decode_pretty<'a>(item: BbnfBootstrapNodeView<'a>) -> Option<PrettyDirective<'a>> {
    let (lo, hi) = item.span();
    let input = item.input();

    // Target: the first `identifier` descendant — the `(* | identifier)`
    // alternation's identifier branch when present. `*` produces no
    // identifier record, so `None` here means the wildcard form.
    let target_text = find_descendant_by_kind(item, BbnfBootstrapRuleKind::identifier)
        .map(|n| n.span_text())
        .unwrap_or("*");

    // Hints: every `pretty_hint` descendant. The `(pretty_hint ?w)+`
    // Repeat is a sibling of the target's Alt wrapper; walking the
    // whole directive tree collects them regardless of the intervening
    // structural nesting.
    let mut hints: Vec<Cow<'a, str>> = Vec::new();
    collect_pretty_hint_descendants(item, &mut hints);

    Some(PrettyDirective {
        rule_name: Cow::Owned(target_text.to_string()),
        hints,
        span: Span::new(lo as usize, hi as usize, input),
    })
}

/// Collect every `pretty_hint` descendant's text into `out`. Used by
/// `decode_pretty` to flatten the DTA-wrapped `(pretty_hint ?w)+`
/// Repeat structure.
fn collect_pretty_hint_descendants<'a>(
    view: BbnfBootstrapNodeView<'a>,
    out: &mut Vec<Cow<'a, str>>,
) {
    if view.rule_kind() == BbnfBootstrapRuleKind::pretty_hint {
        out.push(pretty_hint_text(view));
        return;
    }
    for c in view.children() {
        collect_pretty_hint_descendants(c, out);
    }
}

/// Directives with the shape `@keyword (identifier | "*") ;` —
/// `@token` (identifier only) and `@debug` (identifier or `*`
/// wildcard that marks every rule for instrumentation).
///
/// Under the tape-first emitter the `*` literal branch leaves no
/// record on the tape (the wildcard is elided alongside the keyword
/// and terminator), so there is no `BbnfBootstrapRuleKind::identifier`
/// child to find. The identifier branch does leave an
/// `identifier` record. We try the identifier child first; when
/// absent, we scan the directive's span text for a `*` wildcard.
fn decode_single_name<'a>(item: BbnfBootstrapNodeView<'a>) -> Option<Cow<'a, str>> {
    if let Some(name_node) = find_descendant_by_kind(item, BbnfBootstrapRuleKind::identifier) {
        let name = name_node.span_text();
        if !name.is_empty() {
            return Some(Cow::Owned(name.to_string()));
        }
    }
    // No identifier child — the `*` branch of the alt was taken.
    // The directive's span covers `@keyword ... <terminator>`; skip
    // the leading `@keyword` token and look for the wildcard.
    let text = item.span_text();
    let body = text
        .strip_prefix("@debug")
        .or_else(|| text.strip_prefix("@token"))
        .unwrap_or(text);
    if body.trim_start().starts_with('*') {
        Some(Cow::Borrowed("*"))
    } else {
        None
    }
}

/// `@ws /regex/ ;` — the regex descendant span text is wrapped in `/`.
/// Returns the pattern body without delimiters.
///
/// Uses a recursive descendant walk to see through DTA structural
/// wrappers (Seq compounds around the `regex` child).
fn decode_ws<'a>(item: BbnfBootstrapNodeView<'a>) -> Option<Cow<'a, str>> {
    fn scan<'a>(node: BbnfBootstrapNodeView<'a>) -> Option<Cow<'a, str>> {
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
fn decode_host<'a>(item: BbnfBootstrapNodeView<'a>) -> Option<HostFnDecl<'a>> {
    // Descendant walk so we see through DTA's structural Seq wrappers.
    let name_node = find_descendant_by_kind(item, BbnfBootstrapRuleKind::identifier)?;
    let name = name_node.span_text();
    if name.is_empty() {
        return None;
    }
    // Optional type annotation: the `type_name` descendant (comes
    // after the name identifier). `type_name` is imported from the
    // `types` sub-grammar so its rule kind is directly addressable.
    let return_type =
        find_descendant_by_kind(item, BbnfBootstrapRuleKind::type_name)
            .map(|c| Cow::Owned(c.span_text().trim().to_string()));
    Some(HostFnDecl {
        name: Cow::Owned(name.to_string()),
        return_type,
    })
}

/// Decode an `import_directive` compound into its typed form.
///
/// `import_directive = "@import" ?w , (
///       import_items ?w , "from" ?w , import_path
///     | import_path
/// ) ?w , ( ";" | "." ) ? ;`
///
/// The structural walk scans `item` and its descendants for two
/// semantic child kinds: `import_items` (the `{ a, b, c }` list —
/// present in the selective form only) and `import_path` (the
/// `"..."` string literal — present in both forms). Their presence
/// disambiguates the two Alt branches without depending on any
/// `import_directive_0` sub-variant identity that structural-mode
/// dedup may or may not collapse, and without reading positional
/// slots past the `@import` keyword.
///
/// The `@import` / `from` / `;` keyword literals are skipped
/// implicitly — they don't carry a rule_kind match.
fn decode_import<'a>(item: BbnfBootstrapNodeView<'a>) -> ImportDirective<'a> {
    let (lo, hi) = item.span();
    let directive_span = Span::new(lo as usize, hi as usize, item.input());

    let path_view =
        find_descendant_by_kind(item, BbnfBootstrapRuleKind::import_path)
            .expect("import_directive: missing import_path descendant");

    let (path_lo, path_hi) = path_view.span();
    let path_raw = &path_view.input()[path_lo as usize..path_hi as usize];
    let path_str = strip_quotes(path_raw);

    let items_view =
        find_descendant_by_kind(item, BbnfBootstrapRuleKind::import_items);

    let names: Option<Vec<ImportedName<'a>>> = items_view.map(|items| {
        let mut out = Vec::new();
        collect_identifier_descendants(items, &mut out);
        out
    });

    ImportDirective {
        path: Cow::Borrowed(path_str),
        span: directive_span,
        items: names,
    }
}

/// Collect every `identifier`-kind descendant of `view` into `out`.
/// Used to extract the names from an `import_items` compound whose
/// `{ first , rest , }` internal shape varies under structural
/// dedup.
fn collect_identifier_descendants<'a>(
    view: BbnfBootstrapNodeView<'a>,
    out: &mut Vec<ImportedName<'a>>,
) {
    if view.rule_kind() == BbnfBootstrapRuleKind::identifier {
        push_import_name(out, view);
        return;
    }
    for child in view.children() {
        collect_identifier_descendants(child, out);
    }
}

fn push_import_name<'a>(names: &mut Vec<ImportedName<'a>>, node: BbnfBootstrapNodeView<'a>) {
    let s = node.identifier_span();
    if !s.as_str().is_empty() {
        names.push(ImportedName {
            name: Cow::Borrowed(s.as_str()),
            span: s,
        });
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
/// `pretty_hint = identifier , ("(" , string_lit , ")")?` — the parsed
/// shape is `(identifier_node, paren_arg_opt)`. The arg slot's span
/// includes the parens if present; if the optional arg group is
/// absent we fall back to the bare identifier text.
fn pretty_hint_text<'a>(node: BbnfBootstrapNodeView<'a>) -> Cow<'a, str> {
    if node.rule_kind() == BbnfBootstrapRuleKind::pretty_hint {
        let ident = node.child(0).expect("pretty_hint: missing identifier");
        let name = ident.span_text().trim();
        // The optional `(string_lit)` group is child(1); its span
        // covers the parens + literal when present.
        if let Some(arg_group) = node.child(1) {
            let (lo, hi) = arg_group.span();
            if hi > lo {
                let arg = &arg_group.input()[lo as usize..hi as usize];
                return Cow::Owned(format!("{}{}", name, arg.trim()));
            }
        }
        return Cow::Owned(name.to_string());
    }
    Cow::Owned(node.span_text().trim().to_string())
}
