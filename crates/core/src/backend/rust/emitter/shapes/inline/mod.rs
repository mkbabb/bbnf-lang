//! Inline-position emitters — AX.W0a.2.d.
//!
//! # Role
//!
//! Every shape emitter (`flat`, `wrap`, `arglist`, `unordered`, `pratt`,
//! `array`, `object`, `scalar`) walks a rule body and encounters
//! structural positions — `IrNode::Literal`, `IrNode::Ref`,
//! `IrNode::Alt`, `IrNode::Regex`, `IrNode::Negate`, `IrNode::Minus`,
//! `IrNode::TokenDispatch`. Literal / Ref positions are directly
//! emitted: Literal byte-matches, Ref dispatches via
//! [`super::dispatcher::emit_ref_call_tape`] (per-Ref routing, W5.2).
//!
//! The remaining five — `Alt`, `Regex`, `Negate`, `Minus`,
//! `TokenDispatch` — historically fell back to the grammar's
//! `#dispatcher_ident` (aka `__value`). For JSON that dispatcher byte-
//! dispatches over value's Alt-of-Refs; for non-Alt-rooted grammars
//! (CSS `stylesheet`, Sheets `formula`, BBNF `grammar`) the dispatcher
//! IS the root's shape fn, so the fallback loops. Closing the W0a hard
//! gate #2 — "every grammar's `parse()` routes through the shape
//! dispatcher" — requires every inline position to emit node-specific
//! dispatch, no cross-rule recursion into the root.
//!
//! # Wire contract
//!
//! This module exports two entry points consumed by every shape
//! emitter's position-core walker:
//!
//! - [`emit_inline_position_tape`] — tape-path emission; produces the
//!   walker-identical record stream for the position (Alt compound +
//!   branch records for `IrNode::Alt`, `TapeKind::Span` leaf for
//!   `IrNode::Regex`, guard-only for `Negate` / `Minus`, TokenDispatch
//!   compound + matched-arm records for `IrNode::TokenDispatch`).
//! - [`emit_inline_position_visitor`] — visitor-path emission; mirrors
//!   the tape path structurally with visitor method calls replacing
//!   tape pushes.
//!
//! Both share per-branch first-byte computation, trivia stripping, and
//! the "try-each-branch" fallback (`'try_branches: loop { match first
//! { ... } }`) the AltDispatch emitter pioneered.
//!
//! # Walker parity
//!
//! Record shape per `IrNode`:
//!
//! - `Alt(branches, _)` — one `TapeKind::Alt` compound wrapping the
//!   winning branch's records. Branch selection via byte-dispatch over
//!   each branch's first-byte set (port of
//!   [`super::alt_dispatch::emit_dispatch_arms`]), falling back to
//!   linear retry on overlap. Ref branches call the target's shape fn
//!   via [`super::dispatcher::emit_ref_call_tape`]; Literal branches
//!   byte-match and push a `Literal` leaf; Regex branches scan the
//!   grammar's regex adapter and push a `Span` leaf; Seq branches
//!   match a flattened literal sequence. Mirrors the walker's
//!   `emit_alt_linear_arm`.
//!
//! - `Regex(pattern)` — one `TapeKind::Span` leaf covering the scan
//!   match. Scan via the per-grammar regex adapter (same entry point
//!   the HRegex emitter uses). Mirrors the walker's
//!   `emit_regex_arm` (sans PSI payload scheduling — inline positions
//!   don't carry host decoders; when they do, the rule carrying the
//!   Regex is classified as HRegex and doesn't hit this path).
//!
//! - `Negate(inner)` — guard-only. Try the inner's attempt; on success
//!   return `Err`. No tape record. Mirrors the walker's NotFollowedBy
//!   pattern.
//!
//! - `Minus(primary, excluded)` — first attempt the excluded pattern;
//!   on success fail with `Syntax`; otherwise attempt primary. Matches
//!   the walker's `emit_minus_arm`.
//!
//! - `TokenDispatch { token, arms, fallback }` — one
//!   `TapeKind::TokenDispatch` compound. Emit the token's Span leaf,
//!   then byte-dispatch over arm tokens with each arm emitting its
//!   `continuation`; on no match, emit the fallback.
//!
//! Module layout (B5.W3):
//! - [`alt`]             — Alt-position dispatch (tape + visitor)
//! - [`regex`]           — inline Regex Span emission
//! - [`guard`]           — Negate / Minus guard-only emission
//! - [`token_dispatch`]  — TokenDispatch compound emission
//! - [`branch_analysis`] — first-byte projection, trivia stripping,
//!   Seq flattening (shared by alt + token_dispatch)

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;

mod alt;
mod branch_analysis;
mod guard;
mod regex;
mod structural_branch;
mod token_dispatch;

pub(super) use structural_branch::{
    emit_seq_branch_structural_struct_direct, emit_seq_branch_structural_tape,
};

// ─────────────────────────────────────────────────────────────────────
// Tape-path entry point.
// ─────────────────────────────────────────────────────────────────────

/// Emit tape-path dispatch for an inline structural position (Alt /
/// Regex / Negate / Minus / TokenDispatch).
///
/// The emitted TokenStream is a block producing no expression value
/// (the caller threads it into a larger body). On error the block
/// propagates via `?` to the enclosing shape fn's Result signature.
///
/// `variant_idx` is inherited from the owning rule; every compound
/// push stamps it onto the outer record so downstream view accessors
/// see the owning rule's discriminant.
///
/// # AZ-I.W2.RE — strategy contract
///
/// Inline emission is shape-agnostic structural infrastructure: each
/// inline node emits Refs / scans / byte-matches via the dispatcher's
/// `emit_ref_call_tape` (which resolves to the strategy-aware
/// generated `parse_<shape>_<grammar>_<rule>` symbol at codegen time)
/// or via inline byte/regex emission. Strategy is therefore
/// committed at the per-shape entry boundary upstream of this fn —
/// reaching here implies the caller's per-shape entry resolved the
/// strategy and emitted its TapeDirect body. No strategy parameter
/// is needed in the inline interface.
pub(super) fn emit_inline_position_tape(
    node: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        // AX.W0a.2.g — walker-parity split. The IR's `Alt(branches,
        // Some(AltDispatch))` variant lowers to `IrState::ByteDispatch`
        // in the walker, which transitions to the chosen branch WITHOUT
        // pushing an Alt compound — the branch's own records are the
        // only tape emission. Inline emission must match: no Alt
        // compound push when a dispatch table is present. The
        // `AltLinear` variant (no dispatch table) continues to push
        // the Alt compound to preserve its walker parity.
        IrNode::Alt(branches, Some(_)) => alt::emit_alt_byte_dispatch_tape(
            branches, support_mod, grammar_suffix, ir,
        ),
        IrNode::Alt(branches, None) => {
            alt::emit_alt_tape(branches, variant_idx, support_mod, grammar_suffix, ir)
        }
        IrNode::Regex(sid) => regex::emit_regex_tape(*sid, variant_idx, grammar_suffix, ir),
        IrNode::Negate(inner) => guard::emit_negate_tape(inner, support_mod, grammar_suffix, ir),
        IrNode::Minus(primary, excluded) => {
            guard::emit_minus_tape(primary, excluded, variant_idx, support_mod, grammar_suffix, ir)
        }
        IrNode::TokenDispatch { token, arms, fallback } => token_dispatch::emit_token_dispatch_tape(
            token,
            arms,
            fallback,
            variant_idx,
            support_mod,
            grammar_suffix,
            ir,
        ),
        _ => unreachable!(
            "emit_inline_position_tape called on non-dispatch node: \
             {:?}",
            std::mem::discriminant(node),
        ),
    }
}

// ─────────────────────────────────────────────────────────────────────
// Visitor-path entry point.
// ─────────────────────────────────────────────────────────────────────

/// Visitor-path analog of [`emit_inline_position_tape`]. Emits the
/// same structural dispatch with visitor method calls replacing tape
/// pushes. Negate / Minus produce guard-only emission; Alt / Regex /
/// TokenDispatch emit matching dispatch that calls through to
/// visitor-path Ref calls.
///
/// # AZ-I.W2.RE — strategy contract
///
/// Mirrors [`emit_inline_position_tape`]: strategy is committed at
/// the per-shape entry boundary upstream of this fn; inline emission
/// itself is shape-agnostic structural infrastructure.
pub(super) fn emit_inline_position_visitor(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        IrNode::Alt(branches, _) => {
            alt::emit_alt_visitor(branches, support_mod, grammar_suffix, ir)
        }
        IrNode::Regex(sid) => regex::emit_regex_visitor(*sid, grammar_suffix, ir),
        IrNode::Negate(inner) => {
            guard::emit_negate_visitor(inner, support_mod, grammar_suffix, ir)
        }
        IrNode::Minus(primary, excluded) => guard::emit_minus_visitor(
            primary,
            excluded,
            support_mod,
            grammar_suffix,
            ir,
        ),
        IrNode::TokenDispatch { token, arms, fallback } => token_dispatch::emit_token_dispatch_visitor(
            token,
            arms,
            fallback,
            support_mod,
            grammar_suffix,
            ir,
        ),
        _ => unreachable!(
            "emit_inline_position_visitor called on non-dispatch node: \
             {:?}",
            std::mem::discriminant(node),
        ),
    }
}
