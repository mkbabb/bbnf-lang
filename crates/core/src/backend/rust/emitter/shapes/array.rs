//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2 / AX.W0a.2.a
//!
//! Emits the per-grammar Array-shape parse function with **walker-
//! identical tape emission**. The Array detector admits two structural
//! shapes:
//!
//! 1. **Shape 1 — wrapped homogeneous repeat** (JSON `array`):
//!
//!    ```text
//!    array = "[" >> ((value << comma?)*)?w << "]"
//!    ```
//!
//!    The body unwraps to `Wrap(open_byte, Repeat, close_byte)` where
//!    `open` and `close` are concrete single-byte literals.
//!    [`emit_parse_array_wrapped`] emits the nested Seq/Seq/Repeat/Seq
//!    compound tree with the bracket literals as Literal leaves.
//!
//! 2. **Shape 2 — entry-rule list** (CSS `stylesheet`, BBNF `grammar`):
//!
//!    ```text
//!    stylesheet = ruleList ?w          // OW(Repeat(...)) after inline
//!    grammar    = ( grammar_item ?w )* // direct Repeat
//!    ```
//!
//!    The body has no bracket wrap — the rule body is either a direct
//!    `Repeat` or an `OptionalWhitespace(Repeat(...))`. No close-
//!    delimiter sentinel exists; iteration terminates when the inner
//!    value's first-byte dispatch rejects (end-of-input or a byte not
//!    in the element's first set). [`emit_parse_array_list`] emits the
//!    matching Seq/Rule compound tree — outer Seq when an OW wrapper
//!    is present, otherwise the Repeat's Rule compound directly.
//!
//! Each structural IR production becomes a `push_compound` record.
//! Downstream view derives (`arrayView`, `valueView`, typed-field
//! projections) and the `tape_parity` golden fixtures navigate that
//! exact record sequence, so the shape emitter must reproduce it byte-
//! for-byte — only the **dispatch** is inlined (no `dispatch_one` /
//! `try_branch` / cross-crate helper chain), not the **records**.
//!
//! # Emitted tape shape — Shape 1 (for `[v1, v2]`)
//!
//! ```text
//! [ 0] Seq     variant=<array_id>  span=0..N   child=1  has_children=true
//! [ 1] Seq     variant=0           span=0..N-1 child=2  has_children=true   <- Next("[", rest)
//! [ 2] Literal variant=0           span=0..1                                 <- "["
//! [ 3] Seq     variant=0           span=1..N-1 child=4  has_children=true   <- OptionalWhitespace
//! [ 4] Rule    variant=0           span=1..N-1 child=5  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- Skip(value, Repeat(,?))
//!         ...value records...
//!         Rule  variant=0          has_children=true                        <- Repeat(,?)
//!           Seq variant=0                                                    <- OptionalWhitespace(",")
//!             Literal variant=0                                              <- ","
//! [ N] Literal variant=0           span=N-1..N                               <- "]"
//! ```
//!
//! # Emitted tape shape — Shape 2 (CSS stylesheet `OW(Repeat(OW(Ref)))`)
//!
//! ```text
//! [ 0] Seq     variant=<rule_id>  span=0..N   child=1  has_children=true   <- OW(Repeat)
//! [ 1] Rule    variant=0          span=L..R   child=2  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```
//!
//! # Emitted tape shape — Shape 2 (BBNF grammar, direct Repeat)
//!
//! ```text
//! [ 0] Rule    variant=<rule_id>  span=0..N   child=1  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```

use bbnf_ir::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Emit `pub fn parse_array_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
///
/// Dispatches on rule body structure:
///
/// - **Shape 1** — body unwraps to `Wrap(open, middle, close)` with
///   concrete single-byte open/close literals → [`emit_parse_array_wrapped`].
/// - **Shape 2** — body is a `Repeat` (direct) or `OptionalWhitespace(Repeat)`
///   with no delimiter wrap → [`emit_parse_array_list`].
///
/// The two variants share the function identity (`parse_array_<grammar>_<rule>`)
/// and the outer signature; only the body differs per shape.
pub fn emit_parse_array(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let body = unwrap_map_ow(&rule.body);
    if let Some((open, _middle, close)) = unwrap_wrap(body) {
        if single_byte_literal(open, ir).is_some()
            && single_byte_literal(close, ir).is_some()
        {
            return emit_parse_array_wrapped(grammar_suffix, rule, ir);
        }
    }
    emit_parse_array_list(grammar_suffix, rule, ir)
}

/// Emit the Shape 1 body — wrapped homogeneous repeat
/// (`"[" >> ((value << comma?)*)?w << "]"`, canonical JSON `array`).
///
/// Emission is walker-tape-identical: outer Seq compound (the rule
/// compound) wrapping a Next-Seq (open-literal + rest), wrapping an
/// OW-Seq (leading/trailing ws), wrapping the Repeat Rule compound,
/// wrapping per-iter Seqs with comma-repeat Rules.
fn emit_parse_array_wrapped(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Non-root dispatcher — the shape fn the value position recurses
    // into. Both root and non-root call sites share the same
    // dispatcher in the walker-parity emission (the walker's value
    // ByteDispatch is identical at root and nested positions).
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve the value-position Ref from the array body
    // (`"[" >> ((value << comma?)*)?w << "]"` → the `value` Ref).
    let value_ref = extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _value_off = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _value_off = #dispatcher_ident(input, p, state, builder)?;
            }
        });

    quote! {
        /// AW-V.W3.2 — per-grammar Array-shape parse function,
        /// **walker-tape-identical**.
        ///
        /// Emits the same nested Seq/Seq/Repeat/Seq compound tree the
        /// walker produces for the canonical JSON array rule. The
        /// record tree is navigated by every downstream view derive
        /// and the `tape_parity` golden fixtures; only dispatch is
        /// inlined relative to the walker, not the record stream.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale (the array ↔ value cycle).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let span_lo = *p as u32;
            if input.get(*p).copied() != Some(b'[') {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }

            // AY.W5.2 — pre-order emission via open_compound. Every
            // compound this shape emits lands write-time with
            // `close_compound` back-patching span_hi / child_off /
            // HAS_CHILDREN on the parent row and stamping each direct
            // child's SIB_SKIP_STAMPED_BIT. Value calls inside the loop
            // either emit leaves or nested pre-order subtrees; both
            // participate in the open-frame's push hook automatically.
            //
            // Outer compound: Skip(Next("[", rest), "]") — the array
            // rule itself. Variant_idx comes from the Ref's pending
            // stamp = rule.id & 0xFF.
            let outer_off = builder.open_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                #variant_idx,
                0,
            );

            // Inner Next compound: Next("[", OptionalWhitespace(Repeat(...))).
            let lbracket_open = *p as u32;
            let next_off = builder.open_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                lbracket_open,
                0,
                0,
            );

            // Leaf: "[" Literal (payload-less; TapeKind::Literal per
            // walker). Walker stamps variant_idx from the enclosing
            // rule's `pending_variant_idx` (set by the Ref landing in
            // this rule). Downstream `Leaf::Other` tests assert that
            // every structural literal inside a rule inherits the
            // owning rule's variant.
            *p += 1;
            let bracket_close = *p as u32;
            let _ = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Literal,
                lbracket_open,
                bracket_close,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );

            // OptionalWhitespace Seq compound — contains the Repeat.
            let opt_ws_open = *p as u32;
            let opt_ws_off = builder.open_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                opt_ws_open,
                0,
                0,
            );

            // Leading whitespace trim.
            let _ = #support_mod::skip_space(input, p, state);
            let repeat_open = *p as u32;

            // Repeat compound — one per rule invocation, regardless of iterations.
            let repeat_off = builder.open_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                repeat_open,
                0,
                0,
            );

            // Fast-empty check: `]` immediately closes everything.
            let maybe_close = input.get(*p).copied();
            if maybe_close == Some(b']') {
                // Close Repeat (no iterations), OptionalWhitespace, Next, outer.
                let repeat_close = *p as u32;
                builder.close_compound(repeat_off, repeat_close);
                let opt_ws_close = *p as u32;
                builder.close_compound(opt_ws_off, opt_ws_close);
                // Consume the ].
                *p += 1;
                let rbracket_lo = opt_ws_close;
                let rbracket_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    rbracket_lo,
                    rbracket_hi,
                    #variant_idx,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
                let next_close = rbracket_hi;
                builder.close_compound(next_off, next_close);
                let outer_close = *p as u32;
                builder.close_compound(outer_off, outer_close);
                return Ok(outer_off);
            }

            // Non-empty loop: parse iterations. Each iteration emits:
            //   Seq (iter) { value_records; Rule (comma_repeat) { maybe: Seq { Literal "," } } }
            //
            // Walker-parity decomposition of `(value << comma?) *`:
            //  - Iter body: `Seq[value, Repeat(comma, 0..1)]`. No OW
            //    between `value` and `Repeat(comma)`.
            //  - `comma = "," ?w` lowers to `OptionalWhitespace(Literal(","))`,
            //    which lowers to DTA `Seq[WsTrim, ",", WsTrim]` (leading
            //    + trailing ws absorbed into the Seq's span).
            //  - Repeat(comma, 0..1) fires at most once per iter; when the
            //    "," is absent, the comma iter Seq rolls back via
            //    `handle_repeat_failure` with `*pos = sp.pos` = iter entry.
            //  - Outer Repeat closes when the value Ref's ByteDispatch
            //    rejects `*pos` (e.g. `*pos` at `]` or at ws-before-`]`);
            //    on that absorb, `*pos = sp.pos` = position at which the
            //    last iter body ended (BEFORE the OW-Seq's trailing WsTrim).
            //  - Trailing ws between the last iter and `]` lives in the
            //    OW-Seq (outer OptionalWhitespace(Repeat)) trailing WsTrim,
            //    NOT in the Repeat compound span, NOT in any iter Seq span.
            loop {
                let iter_open = *p as u32;
                let iter_off = builder.open_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_open,
                    0,
                    0,
                );

                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call

                // comma_repeat Rule — span_lo captured AT `*p` immediately
                // after the value parse, BEFORE any leading ws the
                // comma rule's OW wrapper would consume. The walker's
                // Repeat arm captures `*pos` via `push_compound_fused`
                // at arm entry; there is no OW between `value` and
                // `Repeat(comma)` in the iter-body IR.
                let comma_repeat_open = *p as u32;
                let comma_repeat_off = builder.open_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    comma_repeat_open,
                    0,
                    0,
                );

                // Attempt one iter of comma_repeat. Iter body is
                // `Seq[leading WsTrim, Literal(","), trailing WsTrim]`.
                // On failure (no `,` after leading ws), handle_repeat_failure
                // rolls back `*pos` to the iter's saved entry.
                let comma_iter_save_p = *p;
                let _ = #support_mod::skip_space(input, p, state);
                let has_comma = input.get(*p).copied() == Some(b',');
                if has_comma {
                    // Comma iter Seq — span_lo at the iter entry (BEFORE
                    // the leading WsTrim), span_hi after the trailing
                    // WsTrim (same shape as the walker's Seq-wrapper for
                    // `OptionalWhitespace(Literal(","))`).
                    let comma_iter_open = comma_iter_save_p as u32;
                    let comma_iter_off = builder.open_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        comma_iter_open,
                        0,
                        0,
                    );
                    let comma_lo = *p as u32;
                    *p += 1;
                    let comma_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        comma_lo,
                        comma_hi,
                        #variant_idx,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                    // Trailing WsTrim inside the comma Seq.
                    let _ = #support_mod::skip_space(input, p, state);
                    let comma_iter_close = *p as u32;
                    builder.close_compound(comma_iter_off, comma_iter_close);
                } else {
                    // Walker-parity rollback: on comma-iter failure,
                    // handle_repeat_failure restores `*pos = sp.pos` = the
                    // iter's saved entry position. No records have been
                    // pushed — just reset `*p`.
                    *p = comma_iter_save_p;
                }
                let comma_repeat_close = *p as u32;
                builder.close_compound(comma_repeat_off, comma_repeat_close);

                let iter_close = *p as u32;
                builder.close_compound(iter_off, iter_close);

                // Decide continue vs close without consuming ws. The walker's
                // outer Repeat re-enters the value Ref; its ByteDispatch fails
                // when `*pos` is NOT a JSON value-start byte ({ / [ / " / -
                // / 0–9 / t / f / n), triggering handle_repeat_failure which
                // closes Repeat at `*pos = sp.pos` (= `iter_close` captured
                // above). Any trailing ws between the last iter and `]`
                // belongs to the OW-Seq's trailing WsTrim, NOT the Repeat.
                // Direct boolean form rather than `matches!`: nightly's
                // `matches!` expansion decorates the inner `match` with
                // `#[allow(non_exhaustive_omitted_patterns)]` — an
                // attribute on an expression (unstable, E0658) —
                // surfaced by the bootstrap's `cargo expand` step.
                let is_value_start = match input.get(*p).copied() {
                    Some(b'{') | Some(b'[') | Some(b'"') | Some(b'-')
                    | Some(b'0'..=b'9') | Some(b't') | Some(b'f') | Some(b'n') => true,
                    _ => false,
                };
                if !is_value_start {
                    // Close Repeat at current *p (BEFORE OW-Seq trailing ws).
                    let repeat_close = *p as u32;
                    builder.close_compound(repeat_off, repeat_close);
                    // OW-Seq trailing WsTrim: advance past ws.
                    let _ = #support_mod::skip_space(input, p, state);
                    let opt_ws_close = *p as u32;
                    builder.close_compound(opt_ws_off, opt_ws_close);
                    // Expect "]"; anything else (EOF, garbage) is a
                    // well-formed error identical to the walker's path
                    // (Skip's RHS literal mismatches).
                    if input.get(*p).copied() != Some(b']') {
                        return Err(match input.get(*p).copied() {
                            None => ::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            },
                            _ => ::bbnf::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        });
                    }
                    // Consume "]".
                    *p += 1;
                    let rbracket_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        opt_ws_close,
                        rbracket_hi,
                        #variant_idx,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                    let next_close = rbracket_hi;
                    builder.close_compound(next_off, next_close);
                    let outer_close = *p as u32;
                    builder.close_compound(outer_off, outer_close);
                    return Ok(outer_off);
                }
                // Continue: next iter's dispatcher call handles its own
                // leading ws-skip before byte-dispatching the value.
            }
        }
    }
}

/// Emit the Shape 2 body — entry-rule list with no bracket wrap
/// (CSS `stylesheet`, BBNF `grammar`).
///
/// Admitted bodies (after inlining) carry one of two structural shapes:
///
/// 1. **Direct Repeat** — `rule.body = Repeat { inner, lo, hi }`.
///    Walker pushes one Rule compound (the Repeat frame); emission
///    matches.
/// 2. **OW-wrapped Repeat** — `rule.body = OptionalWhitespace(Repeat)`.
///    Walker lowers OW to `Seq[WsTrim, Repeat state, WsTrim]` pushing
///    an outer Seq compound whose single meaningful child is the Rule
///    compound (WsTrim emits no record). Emission matches: outer Seq
///    compound with leading/trailing ws skips and the Repeat Rule
///    inside.
///
/// Per iteration: if the Repeat's inner is itself an
/// `OptionalWhitespace(value)` the walker pushes a per-iter Seq
/// compound (from the inner OW's Seq lowering) with a single
/// meaningful child from the value dispatch. Emission matches. If the
/// Repeat's inner is a bare value with no OW wrapper, no per-iter
/// compound is pushed — the iteration produces the value records
/// directly as children of the Rule compound.
///
/// Termination: the loop exits when `input.get(*p)` is out of the
/// Repeat-inner's first-set. The value's dispatcher fn rejects at
/// its own byte-dispatch (the same hook the Shape 1 inner uses),
/// which rolls back the iter's savepoint and closes the Rule
/// compound.
fn emit_parse_array_list(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Non-root dispatcher fallback — used when the value-position Ref
    // target can't be classified at emit time. Shape 2 entry rules
    // always have a classified target under the predicate that admits
    // them; the fallback closes the emission contract for completeness.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Pattern-match the body structure. The rule body is either a
    // `Repeat` directly (BBNF grammar) or an `OptionalWhitespace`
    // wrapping a `Repeat` (CSS stylesheet post-inline).
    let (has_outer_ow, repeat_inner) = match &rule.body {
        IrNode::OptionalWhitespace(inner) => match inner.as_ref() {
            IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
            _ => return quote! {},
        },
        IrNode::Repeat { inner, .. } => (false, inner.as_ref()),
        IrNode::Map { inner, .. } => match inner.as_ref() {
            IrNode::OptionalWhitespace(ow_inner) => match ow_inner.as_ref() {
                IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
                _ => return quote! {},
            },
            IrNode::Repeat { inner: r_inner, .. } => (false, r_inner.as_ref()),
            _ => return quote! {},
        },
        _ => return quote! {},
    };

    // Does the Repeat's inner element lower to an `IrState::Seq` (and
    // therefore push a per-iter Seq compound in the walker)? Walker's
    // Seq arm fires for `IrNode::Seq`, `IrNode::OptionalWhitespace`
    // (lowers to `Seq[WsTrim, inner, WsTrim]`), and `IrNode::Next` /
    // `IrNode::Skip` (lower to 2-child Seq). Bare `Ref` / `Literal` /
    // `Regex` inners don't lower to Seq — walker pushes only the
    // bare element's own records.
    //
    // BBNF / CSS use `OW(Ref)` inside the Repeat (→ per-iter Seq);
    // EBNF uses `Seq[Regex, Ref, Regex]` inside the Repeat (→ per-iter
    // Seq); pure `Repeat(Ref)` grammars (if any) would skip the
    // per-iter compound.
    let has_iter_ow = matches!(
        repeat_inner,
        IrNode::OptionalWhitespace(_)
            | IrNode::Seq(_)
            | IrNode::Next(_, _)
            | IrNode::Skip(_, _),
    );

    // AX.W0a.2.f — structural element emission. Walk `repeat_inner`
    // (or its OW-unwrapped inner when `has_iter_ow`) and emit position-
    // specific code: Ref → `emit_ref_call_tape`; Alt/Regex/Negate/
    // Minus/TokenDispatch → `inline::emit_inline_position_tape`; Seq /
    // Next / Skip → recurse; Literal → byte-match. Prior implementation
    // `extract_element_ref` collapsed the element to its first Ref and
    // fell back to `#dispatcher_ident` for Alts (BBNF
    // `Repeat(OW(Alt[Ref,…]))`) — the fallback called the root's
    // `__value` dispatcher, which on non-Alt-rooted grammars IS the
    // root shape fn and loops indefinitely. Direct structural emission
    // eliminates the recursive edge.
    let inner_to_emit = if has_iter_ow {
        match repeat_inner {
            IrNode::OptionalWhitespace(inner) => inner.as_ref(),
            _ => repeat_inner,
        }
    } else {
        repeat_inner
    };
    let _ = dispatcher_ident;
    let value_call = emit_element_position_tape(
        inner_to_emit,
        variant_idx,
        &support_mod,
        grammar_suffix,
        ir,
    );

    // First-set byte check for termination. When the dispatcher's byte
    // dispatch would reject the current `*p`, the Repeat closes. Today
    // this is a structural guard on end-of-input; the per-Ref
    // dispatcher carries its own byte check that rolls back the iter
    // savepoint and surfaces the failure. The outer loop intercepts
    // that failure via a try-iter pattern below.
    //
    // For robustness under walker parity: capture `*p` before each
    // iter, call the value, and on iter-body failure roll back `*p`
    // and exit the loop. The walker's `handle_repeat_failure` is
    // functionally identical.

    // Per-iter body. Two structural variants depending on whether the
    // Repeat's inner carries an OW wrapper:
    //
    // - `has_iter_ow == true` — push per-iter Seq compound; leading
    //   WsTrim (silent skip_space); value dispatch; trailing WsTrim;
    //   close Seq.
    // - `has_iter_ow == false` — no per-iter compound; just the value
    //   dispatch. The walker emits no Seq for a bare Ref inside
    //   Repeat.
    let iter_body = if has_iter_ow {
        quote! {
            let iter_open = *p as u32;
            let iter_child = builder.mark_children();

            // Leading WsTrim (silent — emits no record).
            let _ = #support_mod::skip_space(input, p, state);

            // Value dispatch. Failure surfaces through `?` and
            // unwinds to the caller; the outer loop treats the
            // partial-iter Seq compound as uncommitted (mark_children
            // is a read-only length marker) so the tape state rolls
            // back naturally on error.
            #value_call

            // Trailing WsTrim.
            let _ = #support_mod::skip_space(input, p, state);

            let iter_close = *p as u32;
            let _ = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                iter_child,
                iter_open,
                iter_close,
                0,
                0,
            );
        }
    } else {
        quote! {
            #value_call
        }
    };

    // Outer-OW wrap emission. When `rule.body` is `OW(Repeat)`, the
    // walker pushes an outer Seq compound (from OW's
    // `Seq[WsTrim, Repeat, WsTrim]` lowering) wrapping the Rule
    // compound. Emission matches: outer Seq open; leading ws skip;
    // Rule compound with the iterations; trailing ws skip; close
    // outer Seq.
    //
    // Variant_idx on the outer compound comes from the rule id — the
    // rule's variant stamp propagates through the OW's Seq (walker
    // parity).
    if has_outer_ow {
        quote! {
            /// AX.W0a.2.a — per-grammar Array-shape parse function
            /// (Shape 2 — OW-wrapped entry-rule list,
            /// **walker-tape-identical**).
            ///
            /// Emits `Seq[Rule]` matching the walker's lowering of
            /// `OptionalWhitespace(Repeat(...))` where the outer Seq
            /// carries the rule's variant stamp and the inner Rule
            /// compound carries the per-iteration children.
            ///
            /// AX.W0a.2.f — compound; plain `#[inline]`.
            #[inline]
            #[allow(non_snake_case, clippy::too_many_arguments)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut ::bbnf::runtime::tape::TapeBuilder,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                let span_lo = *p as u32;

                // Outer Seq compound (OW lowering: Seq[WsTrim, Repeat, WsTrim]).
                let outer_child = builder.mark_children();

                // Leading WsTrim (silent).
                let _ = #support_mod::skip_space(input, p, state);

                // Rule compound (the Repeat frame).
                let repeat_open = *p as u32;
                let repeat_child = builder.mark_children();

                // Iterate until the value dispatcher rejects or EOF.
                loop {
                    let iter_save_p = *p;
                    // Peek the current byte; EOF or a byte outside
                    // the dispatcher's first set terminates the loop.
                    // The dispatcher's own byte-dispatch performs the
                    // first-set check; on reject it returns Err which
                    // we intercept below.
                    if input.get(*p).is_none() {
                        break;
                    }
                    // Attempt one iteration. On success, continue;
                    // on failure, roll back `*p` and exit. The walker
                    // performs the same rollback via
                    // `handle_repeat_failure`.
                    let iter_result: ::core::result::Result<(), ::bbnf::runtime::tape::DtaError>
                        = (|| {
                            #iter_body
                            Ok(())
                        })();
                    match iter_result {
                        Ok(()) => {
                            // Guard against zero-width iteration
                            // (infinite loop protection): if `*p` did
                            // not advance, treat as terminator.
                            if *p == iter_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = iter_save_p;
                            break;
                        }
                    }
                }

                let repeat_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    repeat_child,
                    repeat_open,
                    repeat_close,
                    0,
                    0,
                );

                // Trailing WsTrim (silent).
                let _ = #support_mod::skip_space(input, p, state);

                let outer_close = *p as u32;
                let outer_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    outer_child,
                    span_lo,
                    outer_close,
                    // AX.W0a.2.g — entry-rule OW-Seq stamps variant=0
                    // for the same reason as the non-OW-outer Rule
                    // compound below: Shape 2 is only reached from the
                    // entry dispatcher, and walker's pending_variant_idx
                    // lowers to 0 on the top-level call.
                    0u8,
                    0,
                );
                Ok(outer_off)
            }
        }
    } else {
        quote! {
            /// AX.W0a.2.a — per-grammar Array-shape parse function
            /// (Shape 2 — direct-Repeat entry-rule list,
            /// **walker-tape-identical**).
            ///
            /// Emits a single Rule compound (the Repeat frame)
            /// carrying the per-iteration children. Matches the
            /// walker's direct lowering of a `Repeat { .. }` body
            /// where the rule's variant stamp lands on the Rule
            /// compound itself (no outer Seq wrapper).
            ///
            /// AX.W0a.2.f — compound; plain `#[inline]`.
            #[inline]
            #[allow(non_snake_case, clippy::too_many_arguments)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut ::bbnf::runtime::tape::TapeBuilder,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                let repeat_open = *p as u32;
                let repeat_child = builder.mark_children();

                loop {
                    let iter_save_p = *p;
                    if input.get(*p).is_none() {
                        break;
                    }
                    let iter_result: ::core::result::Result<(), ::bbnf::runtime::tape::DtaError>
                        = (|| {
                            #iter_body
                            Ok(())
                        })();
                    match iter_result {
                        Ok(()) => {
                            if *p == iter_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = iter_save_p;
                            break;
                        }
                    }
                }

                let repeat_close = *p as u32;
                let repeat_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    repeat_child,
                    repeat_open,
                    repeat_close,
                    // AX.W0a.2.g — entry-rule Repeat stamps variant=0.
                    // Walker's top-level call arrives with
                    // `pending_variant_idx == u8::MAX` (no Ref has
                    // stamped a variant yet), which lowers to 0 on the
                    // Repeat frame's push. Shape 2 (this emitter) is
                    // only reached from the entry dispatcher, so
                    // variant=0 preserves walker parity.
                    0u8,
                    0,
                );
                Ok(repeat_off)
            }
        }
    }
}

/// AX.W0a.2.f — emit tape-path code for a Repeat element body.
///
/// Walks the element structurally and emits per-position code:
///
/// - `Ref(rid)` → `emit_ref_call_tape` on the target's classified
///   shape fn. Classification is admission-guaranteed by
///   [`has_shape_dispatcher_entrypoint`]; unclassified targets would
///   have rejected the grammar before reaching this emission.
/// - `Alt(_, _)` / `Regex(_)` / `Negate(_)` / `Minus(_, _)` /
///   `TokenDispatch { .. }` → `inline::emit_inline_position_tape` —
///   byte-dispatch, regex-scan, guard, or TokenDispatch compound
///   directly, no recursion through `__value`.
/// - `Literal(sid)` → byte-match + `TapeKind::Literal` leaf push.
/// - `Seq(children)` / `Next(lhs, rhs)` / `Skip(lhs, rhs)` → emit each
///   child structurally in order.
/// - `OptionalWhitespace(inner)` / `Map { inner, .. }` → strip wrapper,
///   recurse on inner.
/// - `Repeat { inner, .. }` → emit the Repeat's inner positions inside
///   a bounded per-iter retry loop. Matches walker's `handle_repeat`
///   state machine: rollback `*p` on failure, terminate loop.
/// - `Epsilon` → emit nothing.
///
/// Note: this emitter is distinct from [`super::inline::
/// emit_inline_position_tape`] — the inline emitter focuses on the
/// five discrete positions (Alt / Regex / Negate / Minus /
/// TokenDispatch) a Flat/ArgList/Seq body might encounter at a
/// single position. The element emitter composes Seq / Next / Skip /
/// Literal / Ref handling around those positions so a Repeat can
/// iterate a composite element.
fn emit_element_position_tape(
    node: &bbnf_ir::IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Ref(rid) => match emit_ref_call_tape(grammar_suffix, *rid, ir) {
            Some(call) => quote! { let _value_off = (#call)?; },
            None => {
                // Admission guarantees this Ref's target is classified;
                // hitting None here indicates a detector/admission
                // disagreement. Emit a syntax error to surface the bug
                // rather than a silently-broken parse.
                quote! {
                    return ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    );
                }
            }
        },
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
            let var = variant_idx;
            quote! {
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return ::core::result::Result::Err(
                            ::bbnf::runtime::tape::DtaError::Syntax {
                                offset: at as u32,
                                failing_state:
                                    ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    }
                    *p = end;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        at as u32,
                        end as u32,
                        #var,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                }
            }
        }
        IrNode::Alt(_, _) | IrNode::Regex(_) | IrNode::Negate(_)
        | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. } => {
            super::inline::emit_inline_position_tape(
                node, variant_idx, support_mod, grammar_suffix, ir,
            )
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            emit_element_position_tape(
                inner, variant_idx, support_mod, grammar_suffix, ir,
            )
        }
        IrNode::Seq(children) => {
            let parts: Vec<TokenStream> = children
                .iter()
                .map(|c| emit_element_position_tape(
                    c, variant_idx, support_mod, grammar_suffix, ir,
                ))
                .collect();
            quote! { #(#parts)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_element_position_tape(
                lhs, variant_idx, support_mod, grammar_suffix, ir,
            );
            let r = emit_element_position_tape(
                rhs, variant_idx, support_mod, grammar_suffix, ir,
            );
            quote! { #l #r }
        }
        IrNode::Epsilon => quote! {},
        IrNode::Repeat { inner, .. } => {
            // Nested Repeat inside a Repeat element — uncommon but
            // legal (e.g. `((a b)* c)*`). Emit a bounded retry loop
            // whose body emits the inner positions. Iteration
            // terminates on body failure or zero-width progress.
            let body = emit_element_position_tape(
                inner, variant_idx, support_mod, grammar_suffix, ir,
            );
            quote! {
                loop {
                    let __inner_save_p = *p;
                    let __inner_result:
                        ::core::result::Result<(), ::bbnf::runtime::tape::DtaError>
                        = (|| {
                            #body
                            Ok(())
                        })();
                    match __inner_result {
                        Ok(()) => {
                            if *p == __inner_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = __inner_save_p;
                            break;
                        }
                    }
                }
            }
        }
    }
}

/// Extract the value-position Ref target from an array rule body.
///
/// AW-V.W5.2 — the canonical JSON array body is
/// `"[" >> ((value << comma?)*)?w << "]"`, which lowers to
/// `Skip(Next("[", OW(Repeat(Skip(value, Repeat(comma, 0..=1))))), "]")`.
/// The value Ref sits inside the outer Repeat. The list-rule entry
/// variant (CSS `stylesheet = ruleList ?w`, BBNF `grammar = (item ?w)*`)
/// has a simpler shape: `Repeat(ref_or_alt, lo, hi)` with OW wrappers.
///
/// Strategy: walk the body, find the outer `Repeat`, then find the
/// first value-position Ref inside the iteration body.
fn extract_array_value_ref(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    fn find_repeat_inner<'a>(n: &'a IrNode) -> Option<&'a IrNode> {
        match n {
            IrNode::Repeat { inner, .. } => Some(inner),
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                find_repeat_inner(inner)
            }
            IrNode::Seq(children) => children.iter().find_map(find_repeat_inner),
            IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
                find_repeat_inner(lhs).or_else(|| find_repeat_inner(rhs))
            }
            _ => None,
        }
    }
    fn first_value_ref(n: &IrNode, ir: &GrammarIR) -> Option<bbnf_ir::RuleId> {
        // Punctuation-rule predicate: a rule whose body is a single literal.
        fn is_punct(rid: bbnf_ir::RuleId, ir: &GrammarIR) -> bool {
            let rule = match ir.rules.iter().find(|r| r.id == rid) {
                Some(r) => r,
                None => return false,
            };
            fn unwrap<'a>(n: &'a IrNode) -> &'a IrNode {
                match n {
                    IrNode::OptionalWhitespace(i) | IrNode::Map { inner: i, .. } => {
                        unwrap(i)
                    }
                    _ => n,
                }
            }
            matches!(unwrap(&rule.body), IrNode::Literal(_))
        }
        match n {
            IrNode::Ref(rid) => {
                if is_punct(*rid, ir) {
                    None
                } else {
                    Some(*rid)
                }
            }
            IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
                first_value_ref(inner, ir)
            }
            IrNode::Seq(children) => children.iter().find_map(|c| first_value_ref(c, ir)),
            IrNode::Skip(lhs, _) => first_value_ref(lhs, ir),
            IrNode::Next(lhs, rhs) => {
                first_value_ref(lhs, ir).or_else(|| first_value_ref(rhs, ir))
            }
            IrNode::Alt(branches, _) => {
                // For Alt-of-Refs at the value position (uncommon but
                // legal), route through the dispatcher — return None.
                // A single-Ref Alt could be unwrapped, but that's not the
                // canonical shape.
                let _ = branches;
                None
            }
            _ => None,
        }
    }
    let repeat_inner = find_repeat_inner(node)?;
    first_value_ref(repeat_inner, ir)
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path Array emitter.
//
// Mirrors the prototype's `json_prototype::parse_array::<V>`
// (crates/json-prototype/src/lib.rs:308). Bypasses the tape;
// `visitor.begin_array()` / `visitor.end_array()` replace the compound
// + leaf record pushes the tape-path emits.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_array_visitor_<grammar>_<rule><V: JsonVisitor>(...)
/// -> Result<(), ParseErr>`.
pub fn emit_parse_array_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve value-position Ref for visitor path.
    let value_ref = extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, visitor)?;
            }
        });

    quote! {
        /// AW-V.W3-bench-fix — visitor-path Array-shape parse function.
        ///
        /// Mirrors `json_prototype::parse_array::<V>`. Bypasses
        /// the tape entirely.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            let begin_at = *p;
            if input.get(*p).copied() != Some(b'[') {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: begin_at as u32, rule: None,
                });
            }
            *p += 1;
            visitor.begin_array().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                offset: begin_at as u32, rule: None,
            })?;
            // Fast-empty check: `]` immediately closes.
            if let Some(b) = #support_mod::skip_space(input, p, state) {
                if b == b']' {
                    *p += 1;
                    return visitor.end_array().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    });
                }
            } else {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                });
            }
            loop {
                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call
                match #support_mod::skip_space(input, p, state) {
                    Some(b']') => {
                        *p += 1;
                        return visitor.end_array().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        });
                    }
                    Some(b',') => {
                        *p += 1;
                        let _ = #support_mod::skip_space(input, p, state);
                    }
                    _ => return Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    }),
                }
            }
        }
    }
}
