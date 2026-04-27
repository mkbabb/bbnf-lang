//! Lowering: BbnfBootstrapNodeView → IrNode.
//!
//! Layered descent through the grammar hierarchy:
//!   rhs → alternation → concatenation → binary_factor → mapped_factor →
//!   factor → term → leaf
//!
//! Produces IrNode directly from the tape-first bootstrap parse tree —
//! no intermediate Expression AST.
//!
//! Beta reduction is environment-driven, not walker-driven: when a grammar
//! closure is applied, we push a frame on `LowerCtx.env` mapping each param
//! to its argument CST view, lower the body recursively, and pop. Identifier
//! resolution (`resolve_name`) checks the env stack first before the rule
//! table. This eliminates the parallel `substitute_and_lower` walker.
//!
//! Tranche AE: shape-agnostic walking. Layer functions iterate
//! children via [`super::tape_walk::iter_rep_children`] (peels a
//! single `TapeKind::Repeat` wrapper produced by the `+` / `*`
//! quantifier under preserve_identity mode), select positional
//! children by `rule_kind()` rather than by index, and panic on
//! unhandled rule_kinds — silent `IrNode::Epsilon` fallbacks are
//! forbidden because they corrupt every downstream rule body
//! invisibly.
//!
//! Module layout (B5.W3):
//! - [`alt`]      — alternation / concatenation iteration
//! - [`repeat`]   — factor + quantifier modifier handling
//! - [`pratt`]    — binary_factor (operator-precedence climbing)
//! - [`wrap`]     — mapped_factor / grouped-term / map-arrow handling
//! - [`closures`] — grammar-call / env lookup beta-reduction

use bbnf_ir::IrNode;

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::LowerCtx;
use super::tape_walk::{collect_siblings_by_kind, find_sibling_by_kind, peel_transparent};

mod alt;
mod closures;
mod pratt;
mod repeat;
mod wrap;

pub(crate) use closures::resolve_name;
pub(crate) use wrap::GroupKind;

// ─── Top-level entry ──────────────────────────────────────────────────────────

/// Lower the RHS of a rule: `rhs = closure | alternation`.
///
/// The caller (`host.rs::extract_grammar` or `lower_to_ir`) hands
/// us whatever non-`identifier` child of the rule compound it
/// found. That child may be the `rhs` wrapper itself (preserved
/// under structural mode), one of the `grammar_item` / `directive`
/// transparent wrappers (peeled defensively), the rule's
/// `closure`, or directly the alternation/concatenation/factor
/// expression head (when the optimizer flattened intervening
/// wrappers in non-structural mode). Peel transparent wrappers,
/// then dispatch on the head's `rule_kind()`.
pub(crate) fn lower_rhs<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let node = peel_transparent(node);
    dispatch_expression(node, ctx)
}

/// Dispatch any expression view to the appropriate layer based on
/// its `rule_kind()`. The single source of truth for the layered
/// descent — every layer function calls back into this dispatcher
/// when it needs to lower a child whose role is "another
/// expression of unknown layer".
///
/// Unknown rule_kinds panic with a descriptive message; silent
/// `Epsilon` fallbacks would corrupt every rule body downstream
/// without any error. The bbnf.bbnf grammar is a closed schema —
/// every reachable rule_kind has an explicit handler.
pub(crate) fn dispatch_expression<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Peel named transparent wrappers (`grammar_item`, `directive`,
    // `lhs`) at the dispatch entry so layer functions can assume
    // their input is the semantic head.
    let node = peel_transparent(node);

    // Leaf fast-path: only when the node's span is a SINGLE closed
    // token — a bare identifier, an unquoted epsilon keyword, a
    // regex literal bounded by `/ ... /`, or a string literal
    // bounded by matching quotes with no interior punctuation that
    // would indicate a compound expression. The guard prevents a
    // multi-branch alternation whose full-source span happens to
    // start and end with the same quote byte (e.g. `literal`'s body)
    // from being swallowed as a single `Literal` IR node.
    if is_single_token_span(node) {
        if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
            return leaf;
        }
    }

    // Anonymous wrapper compounds — `Rule` / `Repeat` compounds
    // whose own `variant_idx` either isn't mapped in the current
    // `BbnfBootstrapRuleKind` enum (Unknown) or maps to the
    // sentinel `int_lit` because variant_idx=0 (the catch-all
    // Repeat / Optional sentinel) collides with `int_lit`'s
    // rule_id slot. Walk substantive Rule children (skipping
    // every Repeat / Optional wrapper child — separators,
    // optional placeholders, and the like) and re-dispatch:
    //
    // - Zero substantive Rule children → `Epsilon` (empty
    //   placeholder).
    // - Exactly one substantive Rule child → peel and recurse.
    // - Multiple substantive Rule children → treat as a `Seq`
    //   (the wrapper carries iteration content separated by
    //   non-pushing literals like `,` or `|`; under HEAD's
    //   hand-patched generated.rs the separator commas push
    //   single-byte Repeat compounds we ignore).
    //
    // After AE.4's clean regen these wrappers will have proper
    // enum entries and the peel becomes mostly unreachable.
    use crate::runtime::tape::TapeKind;
    let kind = node.rule_kind();
    let is_unknown_or_sentinel = matches!(
        kind,
        BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
    );
    let is_wrapper_kind = matches!(
        node.kind(),
        TapeKind::Rule | TapeKind::Repeat | TapeKind::Seq | TapeKind::Alt,
    );
    // Pratt-shape detection runs before the anonymous-wrapper branch
    // so that reducer compounds carrying op_discriminant in
    // `variant_idx` (∈ {0,1,2} → rule_kind aliases to int_lit /
    // float_lit / bool_lit) and flat-sequence wrappers both route
    // through `lower_binary_factor` rather than falling into the
    // wrapper-substantive branch (which drops the Span op-leaves) or
    // the catch-all `_ => lower_term` arm (which panics on
    // non-terminal spans).
    //
    // Two entry shapes:
    //  1. Direct reducer: `kind == Rule`, `variant_idx ∈ {0,1,2}`,
    //     three children `[LHS, op_leaf, RHS]`. The wrapper's buggy
    //     `first_child_root` backward walk sometimes surfaces the tail
    //     reducer in place of the true `binary_factor` outer.
    //  2. Flat wrapper: wrapper's `children()` surfaces `[operand,
    //     op_leaf, operand, …]` — the tail reducer's own children
    //     leaked through the wrapper's sib-skip walk. Detected via
    //     the presence of a `Span vi=0` child whose span text is one
    //     of the fixed operator tokens.
    //
    // Under the walker-era tape (pre-W0b) neither shape fires — the
    // outer `binary_factor` Rule compound was emitted with
    // `variant_idx = 34` (maps to `BbnfBootstrapRuleKind::binary_
    // factor`) and surfaced at the `BbnfBootstrapRuleKind::binary_
    // factor` arm below.
    if pratt::is_pratt_reducer(node) {
        return pratt::lower_binary_factor(node, ctx);
    }
    if is_wrapper_kind && pratt::looks_like_pratt_flat(node) {
        return pratt::lower_binary_factor(node, ctx);
    }

    if is_unknown_or_sentinel && is_wrapper_kind {
        let parent_offset = node.cursor().offset();
        let substantive: Vec<BbnfBootstrapNodeView<'a>> = node
            .children()
            .filter(|c| {
                matches!(
                    c.kind(),
                    TapeKind::Rule | TapeKind::Seq | TapeKind::Alt,
                )
            })
            // Cycle guard: drop any child whose tape offset
            // equals the parent's. A malformed compound whose
            // children include itself would otherwise re-enter
            // dispatch_expression at the same view and produce
            // an infinitely-nested IrNode tree, which the
            // codegen later SIGBUS-es while flattening.
            .filter(|c| c.cursor().offset() != parent_offset)
            .collect();
        match substantive.len() {
            0 => return IrNode::Epsilon,
            1 => return dispatch_expression(substantive[0], ctx),
            _ => {
                // Multiple substantive children — treat as a
                // concatenation (Seq) and lower each.
                let parts: Vec<IrNode> = substantive
                    .into_iter()
                    .map(|c| dispatch_expression(c, ctx))
                    .collect();
                return IrNode::Seq(parts);
            }
        }
    }

    match node.rule_kind() {
        BbnfBootstrapRuleKind::closure => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            // closure = "|", first_param, rest_params, "|", body
            let body = node
                .child(4)
                .expect("closure: missing body child");
            lower_rhs(body, ctx)
        }
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            alt::lower_alternation(node, ctx)
        }
        BbnfBootstrapRuleKind::concatenation => alt::lower_concatenation(node, ctx),
        BbnfBootstrapRuleKind::binary_factor => pratt::lower_binary_factor(node, ctx),
        BbnfBootstrapRuleKind::mapped_factor => wrap::lower_mapped_factor(node, ctx),
        BbnfBootstrapRuleKind::factor => repeat::lower_factor(node, ctx),

        // Term layer — `term` is the canonical rule_kind after the
        // AF substrate-break closure; `literal` / `regex` /
        // `identifier` surface directly when the optimizer inlines
        // the term wrapper. All four route through the same
        // content-dispatched `lower_term` entry.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex
        | BbnfBootstrapRuleKind::identifier => lower_term(node, ctx),

        // Comments and directives are grammar-level metadata — they
        // produce no IR contribution. Directives (@recover, @import,
        // @pretty, @ws, @token, @debug, @host, @no_collapse) are
        // consumed by host.rs during grammar extraction; expression
        // lowering treats them as Epsilon.
        //
        // Every directive variant must be listed: under DTA the
        // fallback `lower_term` descends into the compound's span
        // text and reports `unknown leading byte '@'` (or the
        // terminator `';'` when the sub-rule happens to carry its
        // terminator Alt as a direct descendant). `host_directive`
        // was dropped pre-AW-II; re-included here to close the
        // gap.
        //
        // AU.2.5: the old `_0` sub-variants (`import_directive_0`,
        // `pretty_directive_0`, `debug_directive_0`) vanished once
        // Ref-scalar projection collapsed their owning Alt to
        // homogeneous `Span`. AX.W0a.2.i.b: the same fate befell
        // `directive_0` + `grammar_item_0` once the shape-
        // authoritative Wrap emitter stamped Rule compounds with
        // the parent rule's id rather than walker-era sub-variant
        // ids. Dispatch is keyed on the canonical rule-kind; stale
        // `_0` sub-variant names are dropped from the match arm
        // because shape-auth regen no longer produces them.
        BbnfBootstrapRuleKind::comment
        | BbnfBootstrapRuleKind::big_comment
        | BbnfBootstrapRuleKind::recover_directive
        | BbnfBootstrapRuleKind::import_directive
        | BbnfBootstrapRuleKind::pretty_directive
        | BbnfBootstrapRuleKind::ws_directive
        | BbnfBootstrapRuleKind::token_directive
        | BbnfBootstrapRuleKind::debug_directive
        | BbnfBootstrapRuleKind::host_directive
        | BbnfBootstrapRuleKind::directive => IrNode::Epsilon,

        // Fallback: the bbnf grammar is closed at the expression
        // hierarchy layers above, so anything else is a term-shaped
        // node whose `rule_kind` was dropped by a sub-variant dedupe
        // pass in the generated schema. `lower_term` content-dispatches
        // by the span's leading byte and panics if it cannot classify
        // the shape — no silent `Epsilon` fallthrough.
        _ => lower_term(node, ctx),
    }
}

// ─── Term layer (leaf + grouped-term routing) ─────────────────────────────────

/// Whether `node`'s trimmed span is a single closed bbnf leaf
/// token — a bare identifier, `epsilon` / `ε`, a regex literal,
/// or a quoted string with no interior break into a compound
/// expression.
///
/// The gate stops the leaf fast-path in `dispatch_expression`
/// from swallowing a multi-branch alternation whose full-source
/// span happens to start and end with the same quote / bracket
/// byte (e.g. `literal`'s body, which begins with `"` and ends
/// with another `"` on the last branch after a run of `,` / `|`
/// compounds in between).
fn is_single_token_span(node: BbnfBootstrapNodeView<'_>) -> bool {
    let trimmed = node.span_text().trim();
    if trimmed.is_empty() {
        return false;
    }
    let bytes = trimmed.as_bytes();
    // Regex literal `/ ... /` — forbid a `/` inside the body that
    // would imply multiple regex literals concatenated.
    if bytes[0] == b'/' && bytes.len() >= 2 && bytes[bytes.len() - 1] == b'/' {
        let interior = &trimmed[1..trimmed.len() - 1];
        let mut escaped = false;
        for ch in interior.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '/' {
                return false;
            }
        }
        return true;
    }
    // String literal `"..."` / `'...'` / `` `...` `` — forbid
    // unescaped interior quotes.
    if let first @ (b'"' | b'\'' | b'`') = bytes[0] {
        if bytes.len() < 2 || bytes[bytes.len() - 1] != first {
            return false;
        }
        let quote = first as char;
        let interior = &trimmed[1..trimmed.len() - 1];
        let mut escaped = false;
        for ch in interior.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                return false;
            }
        }
        return true;
    }
    // Epsilon keyword.
    if trimmed == "epsilon" || trimmed == "ε" {
        return true;
    }
    // Bare identifier — matches the bbnf `identifier` regex.
    if (bytes[0].is_ascii_alphabetic() || bytes[0] == b'_')
        && bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return true;
    }
    false
}

/// Span-text variant of [`lower_leaf_by_span_text`] that operates
/// on a bare `&str` (rather than a view). Used by
/// [`wrap::lower_mapped_factor`] to recover an identifier / literal /
/// regex that was consumed by the parser without pushing its own
/// tape record.
pub(crate) fn lower_leaf_by_span_text_str<'a>(
    raw: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.len() >= 2
        && trimmed.starts_with('/')
        && trimmed.ends_with('/')
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        let id = ctx.strings.intern(inner);
        return Some(IrNode::Regex(id));
    }
    if trimmed.len() >= 2 {
        let bytes = trimmed.as_bytes();
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if first == last && (first == b'"' || first == b'\'' || first == b'`') {
            let inner = &trimmed[1..trimmed.len() - 1];
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            return Some(IrNode::Literal(id));
        }
    }
    if trimmed == "epsilon" || trimmed == "ε" {
        return Some(IrNode::Epsilon);
    }
    let id_bytes = trimmed.as_bytes();
    if !id_bytes.is_empty()
        && (id_bytes[0].is_ascii_alphabetic() || id_bytes[0] == b'_')
        && id_bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return Some(resolve_name(trimmed, ctx));
    }
    None
}

/// Search for `needle` in `haystack`, skipping over quoted strings
/// (`"..."`, `'...'`, `` `...` ``) and regex literals (`/.../`).
/// Returns the byte offset of the first unquoted occurrence, or `None`.
///
/// This prevents matching `->` inside a quoted literal like `"->"`.
pub(crate) fn find_unquoted(haystack: &str, needle: &str) -> Option<usize> {
    let bytes = haystack.as_bytes();
    let needle_bytes = needle.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        // Skip quoted strings and regex literals.
        if b == b'"' || b == b'\'' || b == b'`' || b == b'/' {
            let quote = b;
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' {
                    i += 2; // skip escape sequence
                    continue;
                }
                if bytes[i] == quote {
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }
        // Check for needle match at this position.
        if i + needle_bytes.len() <= bytes.len()
            && &bytes[i..i + needle_bytes.len()] == needle_bytes
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Span-text leaf classifier — the shape-agnostic fallback when
/// `dispatch_expression` can't route a view by its `rule_kind`.
///
/// Inspects the node's source slice (after trimming surrounding
/// whitespace) and matches against the closed bbnf leaf vocabulary:
///
/// - `/regex/` — regex literal (delimited by forward slashes,
///   matching the bbnf grammar's `regex` rule)
/// - `"text"` / `'text'` / `` `text` `` — string literal in any of
///   bbnf's three quote styles (matching the `literal` rule)
/// - `epsilon` / `ε` — epsilon
/// - bare identifier — nonterminal reference resolved against the
///   rule table or the closure environment
///
/// Returns `None` when the span text doesn't look like a leaf —
/// the caller falls through to its rule_kind-based dispatch (or
/// panics if there's no handler).
fn lower_leaf_by_span_text<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    use crate::runtime::tape::TapeKind;
    // Classify by the node's span text regardless of kind — under
    // DTA a `/regex/` or `"string"` leaf may be wrapped in a Seq/Alt
    // compound whose span still encodes the full token. The
    // `is_single_token_span` caller gate upstream guarantees we
    // only reach here when the span is a single closed bbnf token.
    match node.kind() {
        TapeKind::Rule
        | TapeKind::Span
        | TapeKind::Literal
        | TapeKind::Regex
        | TapeKind::Seq
        | TapeKind::Alt => {}
        _ => return None,
    }
    let raw = node.span_text();
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }

    // Regex literal: starts and ends with `/`, length ≥ 2.
    if trimmed.len() >= 2
        && trimmed.starts_with('/')
        && trimmed.ends_with('/')
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        let id = ctx.strings.intern(inner);
        return Some(IrNode::Regex(id));
    }

    // String literal in any of the three bbnf quote styles.
    if trimmed.len() >= 2 {
        let bytes = trimmed.as_bytes();
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if first == last && (first == b'"' || first == b'\'' || first == b'`') {
            let inner = &trimmed[1..trimmed.len() - 1];
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            return Some(IrNode::Literal(id));
        }
    }

    // Epsilon keyword.
    if trimmed == "epsilon" || trimmed == "ε" {
        return Some(IrNode::Epsilon);
    }

    // Bare identifier — must match `[_a-zA-Z][_a-zA-Z0-9-]*` per
    // the bbnf `identifier` rule. Trailing alphanumerics / `-` /
    // `_` only; bail on anything else.
    let id_bytes = trimmed.as_bytes();
    if !id_bytes.is_empty()
        && (id_bytes[0].is_ascii_alphabetic() || id_bytes[0] == b'_')
        && id_bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return Some(resolve_name(trimmed, ctx));
    }

    None
}

/// Lower a `term` compound.
///
/// The bbnf.bbnf `term` rule is a heterogeneous alternation:
///
/// ```bbnf
/// term = "ε" | "epsilon"
///      | identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?
///      | literal | regex
///      | "@{" , rhs ?w , "}"
///      | "(" , rhs ?w , ")"
///      | "[" , rhs ?w , "]"
///      | "{" , rhs ?w , "}" ;
/// ```
///
/// Every branch dedupes into the same `(Span, children, Span)` tape shape,
/// so the generated enum cannot express which branch hit — dispatch by
/// **content**, not by an enum sub-variant. The single source of truth is
/// the leading byte of the compound's source span (or the leading byte of
/// its first substantive child): `(` / `[` / `{` / `@` discriminate the
/// grouped forms, `"` / `'` / `` ` `` a literal, `/` a regex, `ε` / `e`
/// epsilon, anything else an identifier (possibly followed by grammar-call
/// argument parentheses).
///
/// This is the closed-schema entry point for the term layer. Every caller —
/// `dispatch_expression`, `lower_factor`, the implicit cascade under
/// `peel_transparent` — routes through here; there is no other
/// term-lowering path.
pub(crate) fn lower_term<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    {
        use std::io::Write as _;
        if let Ok(mut f) = std::fs::OpenOptions::new()
            .create(true).append(true)
            .open("/Users/mkbabb/Programming/bbnf-wt-ax-w0a-2m/axw0a2m-probe.log")
        {
            let _ = writeln!(f, "[lt] ENTRY span={:?} kind={:?} vi={} rk={:?}",
                node.span_text(), node.kind(), node.variant_idx(), node.rule_kind());
        }
    }
    // Plain leaf — literal, regex, identifier, epsilon — classified
    // directly from the span text. Covers every term branch whose
    // source span IS the leaf token (no inner expression to descend into).
    if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
        return leaf;
    }

    let raw = node.span_text();
    let trimmed = raw.trim_start();
    let bytes = trimmed.as_bytes();
    if bytes.is_empty() {
        panic!(
            "lower_term: empty span for rule_kind {:?} (full span = {:?})",
            node.rule_kind(),
            raw,
        );
    }

    // Grouped forms: `"(" rhs ")"`, `"[" rhs "]"`, `"{" rhs "}"`, `"@{" rhs "}"`.
    // The opening byte (plus a look-ahead for the two-byte `@{`) is the
    // only discriminator — the four forms all have the same child layout
    // `[open_delim, inner, close_delim]` at the tape level.
    match bytes[0] {
        b'(' => wrap::lower_grouped_term(node, GroupKind::Paren, ctx),
        b'[' => wrap::lower_grouped_term(node, GroupKind::Optional, ctx),
        b'{' => wrap::lower_grouped_term(node, GroupKind::Many, ctx),
        b'@' if bytes.len() >= 2 && bytes[1] == b'{' => {
            wrap::lower_grouped_term(node, GroupKind::SpanCapture, ctx)
        }
        // An identifier head with optional grammar-call argument
        // parentheses. The leaf classifier already handled the bare-identifier
        // case; reaching here means the span carries trailing `(...)` call args.
        b if b.is_ascii_alphabetic() || b == b'_' => {
            lower_identifier_with_optional_call(node, ctx)
        }
        other => panic!(
            "lower_term: unknown leading byte {:?} for rule_kind {:?} (span = {:?})",
            other as char,
            node.rule_kind(),
            raw,
        ),
    }
}

/// Lower an `identifier ( "(" call_arg ("," call_arg)* ")" )?` term.
///
/// The identifier is the first substantive child (either a `TapeKind::Rule`
/// compound for the `identifier` rule, or directly surfacing as a `Span`/
/// `Literal` leaf when the optimizer inlined the wrapper). Call arguments,
/// when present, are zero or more child compounds with `rule_kind = call_arg`.
fn lower_identifier_with_optional_call<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Under DTA the term's identifier branch body
    // `identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?`
    // is emitted inside anonymous Seq / Alt wrappers; the `identifier`
    // record and each `call_arg` surface as sibling body components
    // one or more wrapper levels below the term compound. Use the
    // sibling-scoped descent so the search doesn't step past a
    // sibling boundary into a nested expression's own identifier or
    // call_arg list. Fallback first-substantive-child handles the
    // optimizer-inlined shape where the identifier leaf surfaces
    // directly.
    let ident = find_sibling_by_kind(node, BbnfBootstrapRuleKind::identifier)
        .or_else(|| {
            node.children().find(|c| {
                let (lo, hi) = c.span();
                hi > lo
            })
        })
        .unwrap_or_else(|| {
            panic!(
                "lower_term (identifier): no identifier child in span {:?}",
                node.span_text(),
            )
        });
    let name = ident.span_text();
    // Positional `call_arg` siblings under the term compound, gathered
    // with the same scoping rule: no descent past the nested
    // expressions inside any individual arg's body.
    let mut call_args: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
    collect_siblings_by_kind(node, BbnfBootstrapRuleKind::call_arg, &mut call_args);
    if call_args.is_empty() {
        resolve_name(name, ctx)
    } else {
        closures::lower_grammar_call(name, &call_args, ctx)
    }
}
