//! Grammar extraction: `BbnfBootstrapNodeView` → `ParsedGrammar`.
//!
//! Walks the top-level `grammar` root view and assembles the typed
//! `ParsedGrammar` via structural child traversal. The `directive`
//! wrapper is peeled to expose the specific directive `rule_kind()`
//! (`import_directive`, `pretty_directive`, etc.), and each directive
//! is extracted by walking its data-bearing children — the tape-first
//! emitter elides keyword literals, so only semantic nodes appear.
//!
//! All structural CST traversal lives in the view-layer cursor
//! accessors (`span_text`, `identifier_span`, `rule_kind`,
//! `children`, `child`). This module only handles the
//! *typed*-grammar mapping (CST view → public `crate::types::*`
//! structs).
//!
//! Tranche AC.2: replaced the `BbnfBootstrapEnum` enum pattern-match
//! walker with a cursor-backed view walker. The view API comes from
//! `crate::grammar::generated::BbnfBootstrapNodeView` (emitted by the
//! tape-first generator) and the `Parsed<BbnfBootstrap>` owning type
//! from `crate::runtime::Parsed`.

use std::borrow::Cow;

use parse_that::Span;

use super::generated::{BbnfBootstrap, BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::find_child_by_kind;
use crate::runtime::Parsed;
use crate::types::*;

/// Extract a `ParsedGrammar` from a finished tape-first parse.
///
/// The caller holds a `&'a Parsed<BbnfBootstrap>` whose internal
/// tape + owned input outlive the returned `ParsedGrammar<'a>`.
/// The root view is lent by `parsed.view()` (GAT-bound by the
/// `Root` impl on `BbnfBootstrap`).
pub fn extract_grammar<'a>(parsed: &'a Parsed<'a, BbnfBootstrap>) -> ParsedGrammar<'a> {
    let root = parsed.view();

    let mut grammar = ParsedGrammar::empty();

    // `grammar = grammar_item*` — the rule body is a single Repeat
    // compound whose direct children are the individual
    // `grammar_item` compounds. Flatten the Repeat wrapper, peel
    // transparent grammar_item / directive wrappers, and dispatch
    // each item to `absorb_item`.
    use ::bbnf::runtime::tape::TapeKind;
    for item in root.children() {
        if item.kind() == TapeKind::Repeat {
            for grandchild in item.children() {
                let inner = peel_wrappers(grandchild);
                absorb_item(inner, &mut grammar);
            }
        } else {
            let inner = peel_wrappers(item);
            absorb_item(inner, &mut grammar);
        }
    }

    grammar
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

/// Map a single grammar item (rule or directive) into the typed `ParsedGrammar`.
fn absorb_item<'a>(
    item: BbnfBootstrapNodeView<'a>,
    grammar: &mut ParsedGrammar<'a>,
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
        // Pick the rhs child: the last direct child that isn't the
        // identifier view (we look up by rule_kind match). For the
        // common case where identifier is elided, the single direct
        // child IS the rhs.
        let rhs = {
            let mut rhs: Option<BbnfBootstrapNodeView<'a>> = None;
            for c in item.children() {
                if c.rule_kind() != BbnfBootstrapRuleKind::identifier {
                    rhs = Some(c);
                }
            }
            rhs.expect("rule: missing rhs child")
        };
        grammar.rules.insert(
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
            absorb_recover_structural(item, &mut grammar.recovers);
        }
        BbnfBootstrapRuleKind::pretty_directive => {
            absorb_pretty_structural(item, &mut grammar.pretties);
        }
        BbnfBootstrapRuleKind::token_directive => {
            absorb_single_name_directive(item, &mut grammar.token_rules);
        }
        BbnfBootstrapRuleKind::debug_directive => {
            absorb_single_name_directive(item, &mut grammar.debug_rules);
        }
        BbnfBootstrapRuleKind::ws_directive => {
            absorb_ws_structural(item, &mut grammar.ws_pattern);
        }
        BbnfBootstrapRuleKind::host_directive => {
            absorb_host_structural(item, &mut grammar.host_fns);
        }
        BbnfBootstrapRuleKind::import_directive => {
            absorb_import_structural(item, &mut grammar.imports);
        }
        _ => {}
    }
}

// ------------------------------------------------------------------
// Structural directive extractors — walk children by `rule_kind()`.
// ------------------------------------------------------------------

/// `@recover ruleName syncExpr ;` — extract rule_name (first
/// identifier child) and sync_expr (remaining children).
fn absorb_recover_structural<'a>(
    item: BbnfBootstrapNodeView<'a>,
    recovers: &mut Vec<RecoverDirective<'a>>,
) {
    let (lo, hi) = item.span();
    let input = item.input();
    let mut children = item.children();
    let name_node = children
        .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::identifier)
        .expect("recover_directive: missing identifier child");
    let rule_name = name_node.span_text();
    recovers.push(RecoverDirective {
        rule_name,
        sync_expr: item,
        span: Span::new(lo as usize, hi as usize, input),
    });
}

/// `@pretty target hint* ;` — first identifier is target, remaining
/// children provide hints.
fn absorb_pretty_structural<'a>(
    item: BbnfBootstrapNodeView<'a>,
    pretties: &mut Vec<PrettyDirective<'a>>,
) {
    let (lo, hi) = item.span();
    let input = item.input();
    let mut children = item.children().peekable();

    // First identifier-kind child is the target rule name.
    let target = children
        .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::identifier)
        .expect("pretty_directive: missing target identifier");
    let target_text = target.span_text();

    // Remaining children carry hint tokens. Under the tape-first
    // layout, the hints may be wrapped in a Repeat compound.
    // Flatten any Repeat wrappers and extract each pretty_hint.
    let mut hints: Vec<Cow<'a, str>> = Vec::new();
    for child in children {
        use ::bbnf::runtime::tape::TapeKind;
        if child.kind() == TapeKind::Repeat {
            for hint in child.children() {
                hints.push(pretty_hint_text(hint));
            }
        } else {
            hints.push(pretty_hint_text(child));
        }
    }

    pretties.push(PrettyDirective {
        rule_name: Cow::Owned(target_text.to_string()),
        hints,
        span: Span::new(lo as usize, hi as usize, input),
    });
}

/// Directives with the shape `@keyword name ;` — `@token` and
/// `@debug`. The first identifier child provides the name.
fn absorb_single_name_directive<'a>(
    item: BbnfBootstrapNodeView<'a>,
    names: &mut Vec<Cow<'a, str>>,
) {
    let name_node = item
        .children()
        .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::identifier)
        .expect("single_name_directive: missing identifier child");
    let name = name_node.span_text();
    if !name.is_empty() {
        names.push(Cow::Owned(name.to_string()));
    }
}

/// `@ws /regex/ ;` — the regex child is the first child whose span
/// text starts with `/`.
fn absorb_ws_structural<'a>(
    item: BbnfBootstrapNodeView<'a>,
    ws_pattern: &mut Option<Cow<'a, str>>,
) {
    for child in item.children() {
        let text = child.span_text();
        if let Some(stripped) = text.strip_prefix('/').and_then(|s| s.strip_suffix('/')) {
            *ws_pattern = Some(Cow::Borrowed(stripped));
            return;
        }
    }
}

/// `@host name [: type] ;` — first identifier is name, optional
/// type annotation follows.
fn absorb_host_structural<'a>(
    item: BbnfBootstrapNodeView<'a>,
    host_fns: &mut Vec<HostFnDecl<'a>>,
) {
    let mut children = item.children();
    let name_node = children
        .find(|c| c.rule_kind() == BbnfBootstrapRuleKind::identifier)
        .expect("host_directive: missing identifier child");
    let name = name_node.span_text();
    if name.is_empty() {
        return;
    }
    // Optional type annotation: the next non-identifier child whose
    // span text is non-empty.
    let return_type = children
        .find(|c| {
            c.rule_kind() != BbnfBootstrapRuleKind::identifier && {
                let t = c.span_text().trim();
                !t.is_empty() && t != ":" && t != ";" && t != "."
            }
        })
        .map(|c| Cow::Owned(c.span_text().trim().to_string()));
    host_fns.push(HostFnDecl {
        name: Cow::Owned(name.to_string()),
        return_type,
    });
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
fn absorb_import_structural<'a>(
    item: BbnfBootstrapNodeView<'a>,
    imports: &mut Vec<ImportDirective<'a>>,
) {
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

    imports.push(ImportDirective {
        path: Cow::Borrowed(path_str),
        span: directive_span,
        items: names,
    });
}

/// Depth-first search for the first descendant whose `rule_kind()`
/// matches `target`. Checks the root first, then recurses through
/// children. Used to reach `import_items` / `import_path` compounds
/// that live underneath transparent / wrapper compounds that dedup
/// may or may not have collapsed.
fn find_descendant_by_kind<'a>(
    view: BbnfBootstrapNodeView<'a>,
    target: BbnfBootstrapRuleKind,
) -> Option<BbnfBootstrapNodeView<'a>> {
    if view.rule_kind() == target {
        return Some(view);
    }
    if let Some(direct) = find_child_by_kind(view, target) {
        return Some(direct);
    }
    for child in view.children() {
        if let Some(found) = find_descendant_by_kind(child, target) {
            return Some(found);
        }
    }
    None
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
