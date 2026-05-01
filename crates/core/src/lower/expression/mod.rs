//! Lowering: BbnfView → IrNode.
//!
//! Layered descent through the grammar hierarchy:
//!   rhs → alternation → concatenation → binary_factor → mapped_factor →
//!   factor → term → leaf
//!
//! Produces IrNode directly from the struct-direct BBNF parse tree —
//! no intermediate Expression AST.
//!
//! Beta reduction is environment-driven, not walker-driven: when a grammar
//! closure is applied, we push a frame on `LowerCtx.env` mapping each param
//! to its argument CST view, lower the body recursively, and pop. Identifier
//! resolution (`resolve_name`) checks the env stack first before the rule
//! table. This eliminates the parallel `substitute_and_lower` walker.
//!
//! AZ-II.cutover.D1 — every layer function consumes [`BbnfView`],
//! iterates children via [`super::view_walk::iter_rep_children`],
//! selects positional children by
//! [`crate::runtime::bbnf::BbnfCompoundKind`] rather than by index,
//! and panics on unhandled compound kinds. Silent
//! `IrNode::Epsilon` fallbacks are forbidden because they corrupt
//! every downstream rule body invisibly.
//!
//! Module layout (B5.W3):
//! - [`alt`]      — alternation / concatenation iteration
//! - [`repeat`]   — factor + quantifier modifier handling
//! - [`pratt`]    — binary_factor (operator-precedence climbing)
//! - [`wrap`]     — mapped_factor / grouped-term / map-arrow handling
//! - [`closures`] — grammar-call / env lookup beta-reduction

use bbnf_ir::IrNode;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

use super::LowerCtx;
use super::view_walk::{collect_siblings_by_kind, peel_transparent};

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
/// The caller (`host.rs::extract_*` or `lower_to_ir`) hands us
/// whatever non-`identifier` child of the rule compound it found.
/// That child may be the `rhs` wrapper itself, one of the
/// `grammar_item` / `directive` transparent wrappers (peeled
/// defensively), the rule's `closure`, or directly the
/// alternation/concatenation/factor expression head. Peel
/// transparent wrappers, then dispatch on the head's compound kind.
pub(crate) fn lower_rhs<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let node = peel_transparent(node);
    dispatch_expression(node, ctx)
}

/// Dispatch any expression view to the appropriate layer based on
/// its compound kind. The single source of truth for the layered
/// descent — every layer function calls back into this dispatcher
/// when it needs to lower a child whose role is "another expression
/// of unknown layer".
///
/// Unknown compound kinds panic with a descriptive message; silent
/// `Epsilon` fallbacks would corrupt every rule body downstream
/// without any error. The bbnf.bbnf grammar is a closed schema —
/// every reachable compound kind has an explicit handler.
pub(crate) fn dispatch_expression<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Peel named transparent wrappers (`grammar_item`, `directive`,
    // `lhs`) at the dispatch entry so layer functions can assume
    // their input is the semantic head.
    let node = peel_transparent(node);

    // Leaf fast-path: only when the node's span is a SINGLE closed
    // bbnf leaf token — a bare identifier, an unquoted epsilon
    // keyword, a regex literal bounded by `/ ... /`, or a string
    // literal bounded by matching quotes with no interior punctuation
    // that would indicate a compound expression. The guard prevents
    // a multi-branch alternation whose full-source span happens to
    // start and end with the same quote byte (e.g. `literal`'s body)
    // from being swallowed as a single `Literal` IR node.
    if is_single_token_span(node) {
        if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
            return leaf;
        }
    }

    // Anonymous-wrapper compounds — compounds whose `BbnfCompoundKind`
    // is `Other` (catch-all for sub-grammar / structural groupings the
    // BBNF main alphabet doesn't enumerate). Walk substantive
    // compound children (skipping empty placeholders and span-only
    // separators) and re-dispatch:
    //
    // - Zero substantive children → `Epsilon` (empty placeholder).
    // - Exactly one substantive child → peel and recurse.
    // - Multiple substantive children → treat as a `Seq` (the
    //   wrapper carries iteration content separated by non-pushing
    //   literals like `,` or `|`).
    if matches!(node.compound_kind(), Some(BbnfCompoundKind::Other)) {
        // Pratt-shape detection runs before the anonymous-wrapper
        // branch so that flat-sequence wrappers carrying a binary-
        // operators op-leaf among their children route through
        // `lower_binary_factor` rather than the wrapper-substantive
        // branch.
        if pratt::is_pratt_reducer(node) {
            return pratt::lower_binary_factor(node, ctx);
        }
        if pratt::looks_like_pratt_flat(node) {
            return pratt::lower_binary_factor(node, ctx);
        }

        let parent_id = node.compound_identity();
        let substantive: Vec<BbnfView<'a, 'a>> = node
            .children()
            .filter(|c| c.kind() == BbnfKind::Compound)
            // Cycle guard: drop any child whose compound identity
            // equals the parent's. A malformed compound whose
            // children include itself would otherwise re-enter
            // dispatch_expression at the same view and produce an
            // infinitely-nested IrNode tree.
            .filter(|c| c.compound_identity() != parent_id)
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

    // Compound dispatch by structural kind.
    match node.compound_kind() {
        Some(BbnfCompoundKind::Closure) => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            // closure = "|", first_param, rest_params, "|", body
            let body = node.child(4).expect("closure: missing body child");
            lower_rhs(body, ctx)
        }
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            alt::lower_alternation(node, ctx)
        }
        Some(BbnfCompoundKind::Concatenation) => alt::lower_concatenation(node, ctx),
        Some(BbnfCompoundKind::BinaryFactor) => pratt::lower_binary_factor(node, ctx),
        Some(BbnfCompoundKind::MappedFactor) => wrap::lower_mapped_factor(node, ctx),
        Some(BbnfCompoundKind::Factor) => repeat::lower_factor(node, ctx),

        // Term layer — `term` is the canonical compound kind. Bare
        // identifier / literal / regex leaves arrive as Span leaves
        // (handled by the leaf fast-path upstream).
        Some(BbnfCompoundKind::Term) => lower_term(node, ctx),

        // Comments and directives are grammar-level metadata — they
        // produce no IR contribution. Directives (@recover, @import,
        // @pretty, @ws, @token, @debug, @host) are consumed by
        // host.rs during grammar extraction; expression lowering
        // treats them as Epsilon.
        Some(BbnfCompoundKind::RecoverDirective)
        | Some(BbnfCompoundKind::ImportDirective)
        | Some(BbnfCompoundKind::PrettyDirective)
        | Some(BbnfCompoundKind::WsDirective)
        | Some(BbnfCompoundKind::TokenDirective)
        | Some(BbnfCompoundKind::DebugDirective)
        | Some(BbnfCompoundKind::HostDirective)
        | Some(BbnfCompoundKind::Directive) => IrNode::Epsilon,

        // Span / leaf focus that survived `is_single_token_span` —
        // dispatch by content. Covers Span-projected leaves (raw
        // identifier, literal text, regex text) plus Other compounds
        // already handled above.
        None => lower_term(node, ctx),

        // Fallback: every other compound kind (Rule, Lhs, Rhs,
        // GrammarItem, Grammar, ImportPath, ImportItems, PrettyHint)
        // routes through `lower_term` which content-dispatches by
        // the span's leading byte and panics if it cannot classify
        // the shape — no silent `Epsilon` fallthrough.
        Some(_) => lower_term(node, ctx),
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
fn is_single_token_span(node: BbnfView<'_, '_>) -> bool {
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
/// source leaf.
pub(crate) fn lower_leaf_by_span_text_str<'a>(
    raw: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
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
/// `dispatch_expression` can't route a view by its compound kind.
///
/// Inspects the node's source slice (after trimming surrounding
/// whitespace) and matches against the closed bbnf leaf vocabulary:
///
/// - `/regex/` — regex literal
/// - `"text"` / `'text'` / `` `text` `` — string literal
/// - `epsilon` / `ε` — epsilon
/// - bare identifier — nonterminal reference resolved against the
///   rule table or the closure environment
///
/// Returns `None` when the span text doesn't look like a leaf —
/// the caller falls through to its compound-kind-based dispatch.
fn lower_leaf_by_span_text<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> Option<IrNode> {
    // Only Span / Compound focuses carry a recoverable span-text;
    // numeric / boolean / unit leaves don't represent a source token.
    match node.kind() {
        BbnfKind::Span | BbnfKind::Compound => {}
        _ => return None,
    }
    let raw = node.span_text();
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }

    // Regex literal: starts and ends with `/`, length ≥ 2.
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
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
    // the bbnf `identifier` rule.
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
/// Every branch dedupes into the same `BbnfCompoundKind::Term` shape,
/// so the compound kind alone cannot express which branch hit.
/// Dispatch precedence:
///
/// 1. **Structural `branch_tag`** — the codegen alt_dispatch emitter
///    records the chosen alt branch on the Term compound via
///    [`crate::runtime::builder::StructBuilder::push_branch_tag`].
///    Tags 0..=8 map directly to the eight alt branches above (with
///    branch 2 collapsing the optional grammar-call into the
///    identifier head). This is the canonical generated-parser path.
/// 2. **Content fallback** — when `branch_tag` is `None` (the
///    [`dispatch_expression`] catch-all routes non-Term compound kinds
///    here for content-based leaf classification), classify by the
///    leading byte of the trimmed source span: `(` / `[` / `{` / `@`
///    discriminate the grouped forms, `"` / `'` / `` ` `` a literal,
///    `/` a regex, `ε` / `e` epsilon, anything else an identifier
///    (possibly followed by grammar-call argument parentheses).
///
/// The structural path runs first because it is unambiguous: the
/// codegen-emitted Term carries no punctuator Span leaves around the
/// inner rhs, so the recovered `span_text()` begins with the inner
/// rhs's first byte rather than the grouping delimiter — content-
/// dispatch on that span would otherwise produce the wrong branch.
pub(crate) fn lower_term<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Structural dispatch path (codegen alt_dispatch): the emitter
    // records the chosen alt branch via `push_branch_tag`. When the
    // tag is set on a Term compound, route directly without inspecting
    // source bytes — the codegen path doesn't synthesise punctuator
    // spans for the grouped branches, so leading-byte content
    // classification would misclassify `( rhs )` / `[ rhs ]` /
    // `{ rhs }` / `@{ rhs }`.
    //
    // Gate the branch_tag dispatch on `compound_kind == Term`: the
    // `dispatch_expression` catch-all routes other compound kinds (Rule,
    // Lhs, Rhs, GrammarItem, ImportPath, ImportItems, PrettyHint, …)
    // through `lower_term` for span-content leaf classification, and
    // some of those compounds carry their own branch_tag for unrelated
    // alt-dispatch grammars (e.g. expressions.bbnf's value layer).
    if node.is_compound_kind(BbnfCompoundKind::Term) {
        match node.branch_tag() {
            Some(0) | Some(1) => return IrNode::Epsilon,
            Some(2) => return lower_identifier_with_optional_call(node, ctx),
            Some(3) => {
                // Literal: the literal's Span leaf surfaces as the only
                // substantive child; the existing leaf classifier picks
                // it up via span_text.
                if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
                    return leaf;
                }
            }
            Some(4) => {
                // Regex: same shape as Literal — Span leaf child carries
                // the `/.../ ` text.
                if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
                    return leaf;
                }
            }
            Some(5) => return wrap::lower_grouped_term(node, GroupKind::SpanCapture, ctx),
            Some(6) => return wrap::lower_grouped_term(node, GroupKind::Paren, ctx),
            Some(7) => return wrap::lower_grouped_term(node, GroupKind::Optional, ctx),
            Some(8) => return wrap::lower_grouped_term(node, GroupKind::Many, ctx),
            Some(other) => panic!(
                "lower_term: unknown branch_tag {} on Term compound (span = {:?})",
                other,
                node.span_text(),
            ),
            None => {
                // Term compound emitted without a branch tag — the
                // hand-written codepath (or an inlined alt branch).
                // Fall through to leading-byte content dispatch below;
                // the source span DOES include any synthetic punctuator
                // Spans pushed around the inner rhs.
            }
        }
    }

    // Plain leaf — literal, regex, identifier, epsilon — classified
    // directly from the span text. Covers every term branch whose
    // source span IS the leaf token.
    if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
        return leaf;
    }

    let raw = node.span_text();
    let trimmed = raw.trim_start();
    let bytes = trimmed.as_bytes();
    if bytes.is_empty() {
        panic!(
            "lower_term: empty span for compound_kind {:?} (full span = {:?})",
            node.compound_kind(),
            raw,
        );
    }

    // Grouped forms: `"(" rhs ")"`, `"[" rhs "]"`, `"{" rhs "}"`, `"@{" rhs "}"`.
    match bytes[0] {
        b'(' => wrap::lower_grouped_term(node, GroupKind::Paren, ctx),
        b'[' => wrap::lower_grouped_term(node, GroupKind::Optional, ctx),
        b'{' => wrap::lower_grouped_term(node, GroupKind::Many, ctx),
        b'@' if bytes.len() >= 2 && bytes[1] == b'{' => {
            wrap::lower_grouped_term(node, GroupKind::SpanCapture, ctx)
        }
        // An identifier head with optional grammar-call argument
        // parentheses. The leaf classifier already handled the
        // bare-identifier case; reaching here means the span carries
        // trailing `(...)` call args.
        b if b.is_ascii_alphabetic() || b == b'_' => lower_identifier_with_optional_call(node, ctx),
        other => panic!(
            "lower_term: unknown leading byte {:?} for compound_kind {:?} (span = {:?})",
            other as char,
            node.compound_kind(),
            raw,
        ),
    }
}

/// Lower an `identifier ( "(" call_arg ("," call_arg)* ")" )?` term.
///
/// The identifier is the first substantive child (a Span leaf for
/// the `identifier` rule's typed projection, or directly surfacing
/// when the grouping wrapper was inlined). Call arguments, when
/// present, are zero or more child compounds of kind `CallArg`.
fn lower_identifier_with_optional_call<'a>(
    node: BbnfView<'a, 'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // The term's identifier branch body
    // `identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?`
    // surfaces the `identifier` Span leaf as a child, and each
    // `call_arg` as a `CallArg`-kinded compound child. The
    // sibling-scoped descent skips through anonymous wrappers so
    // structural Seq groupings around the call_args don't hide them.
    //
    // Identifier surfaces as a Span leaf (the `identifier` rule
    // projects to `Span`). Find it via first non-empty Span child.
    let ident = node
        .children()
        .find(|c| c.kind() == BbnfKind::Span && !c.span_text().trim().is_empty())
        .or_else(|| {
            node.children().find(|c| {
                let trimmed = c.span_text().trim();
                !trimmed.is_empty()
            })
        })
        .unwrap_or_else(|| {
            panic!(
                "lower_term (identifier): no identifier child in span {:?}",
                node.span_text(),
            )
        });
    let name = ident.span_text();
    // Positional `call_arg` siblings under the term compound.
    let mut call_args: Vec<BbnfView<'a, 'a>> = Vec::new();
    collect_siblings_by_kind(node, BbnfCompoundKind::CallArg, &mut call_args);
    if call_args.is_empty() {
        resolve_name(name.trim(), ctx)
    } else {
        closures::lower_grammar_call(name.trim(), &call_args, ctx)
    }
}
