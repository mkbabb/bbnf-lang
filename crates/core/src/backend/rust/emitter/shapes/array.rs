//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Array-shape parse function with **walker-
//! identical tape emission**. The walker lowers the canonical JSON
//! array rule
//!
//! ```text
//! array = "[" >> ((value << comma?)*)?w << "]"
//! ```
//!
//! to a nested Seq/Seq/Repeat/Seq compound tree; each structural IR
//! production becomes a `push_compound` record. Downstream view
//! derives (`arrayView`, `valueView`, typed-field projections) and the
//! `tape_parity` golden fixtures navigate that exact record sequence,
//! so the shape emitter must reproduce it byte-for-byte — only the
//! **dispatch** is inlined (no `dispatch_one` / `try_branch` / cross-
//! crate helper chain), not the **records**.
//!
//! # Emitted tape shape (for `[v1, v2]`)
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

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Emit `pub fn parse_array_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_array(
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
        #[inline(always)]
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

            // Outer compound: Skip(Next("[", rest), "]") — the array rule itself.
            // Walker variant_idx comes from the Ref's pending stamp = rule.id & 0xFF.
            let outer_child = builder.mark_children();

            // Inner Next compound: Next("[", OptionalWhitespace(Repeat(...)))
            let lbracket_open = *p as u32;
            let next_child = builder.mark_children();

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
            let opt_ws_child = builder.mark_children();

            // Leading whitespace trim.
            let _ = #support_mod::skip_space(input, p, state);
            let repeat_open = *p as u32;

            // Repeat compound — one per rule invocation, regardless of iterations.
            let repeat_child = builder.mark_children();

            // Fast-empty check: `]` immediately closes everything.
            let maybe_close = input.get(*p).copied();
            if maybe_close == Some(b']') {
                // Close Repeat (no iterations), OptionalWhitespace, Next, outer.
                let repeat_close = *p as u32;
                let _repeat_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    repeat_child,
                    repeat_open,
                    repeat_close,
                    0,
                    0,
                );
                let opt_ws_close = *p as u32;
                let _opt_ws_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    opt_ws_child,
                    opt_ws_open,
                    opt_ws_close,
                    0,
                    0,
                );
                let next_close = *p as u32;
                let _next_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    next_child,
                    lbracket_open,
                    next_close,
                    0,
                    0,
                );
                // Consume the ].
                *p += 1;
                let rbracket_lo = next_close;
                let rbracket_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    rbracket_lo,
                    rbracket_hi,
                    #variant_idx,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
                let outer_close = *p as u32;
                let outer_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    outer_child,
                    span_lo,
                    outer_close,
                    #variant_idx,
                    0,
                );
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
                let iter_child = builder.mark_children();

                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call

                // comma_repeat Rule — span_lo captured AT `*p` immediately
                // after the value parse, BEFORE any leading ws the
                // comma rule's OW wrapper would consume. The walker's
                // Repeat arm captures `*pos` via `push_compound_fused`
                // at arm entry; there is no OW between `value` and
                // `Repeat(comma)` in the iter-body IR.
                let comma_repeat_open = *p as u32;
                let comma_repeat_child = builder.mark_children();

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
                    let comma_iter_child = builder.mark_children();
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
                    let _ = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        comma_iter_child,
                        comma_iter_open,
                        comma_iter_close,
                        0,
                        0,
                    );
                } else {
                    // Walker-parity rollback: on comma-iter failure,
                    // handle_repeat_failure restores `*pos = sp.pos` = the
                    // iter's saved entry position. No records have been
                    // pushed (mark_children is just a length marker, not
                    // a commit), so just reset `*p`.
                    *p = comma_iter_save_p;
                }
                let comma_repeat_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    comma_repeat_child,
                    comma_repeat_open,
                    comma_repeat_close,
                    0,
                    0,
                );

                let iter_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_child,
                    iter_open,
                    iter_close,
                    0,
                    0,
                );

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
                    let _ = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        repeat_child,
                        repeat_open,
                        repeat_close,
                        0,
                        0,
                    );
                    // OW-Seq trailing WsTrim: advance past ws.
                    let _ = #support_mod::skip_space(input, p, state);
                    let opt_ws_close = *p as u32;
                    let _ = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        opt_ws_child,
                        opt_ws_open,
                        opt_ws_close,
                        0,
                        0,
                    );
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
                    let next_close = *p as u32;
                    let _ = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        next_child,
                        lbracket_open,
                        next_close,
                        0,
                        0,
                    );
                    // Consume "]".
                    *p += 1;
                    let rbracket_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        next_close,
                        rbracket_hi,
                        #variant_idx,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                    let outer_close = *p as u32;
                    let outer_off = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        outer_child,
                        span_lo,
                        outer_close,
                        #variant_idx,
                        0,
                    );
                    return Ok(outer_off);
                }
                // Continue: next iter's dispatcher call handles its own
                // leading ws-skip before byte-dispatching the value.
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
// Mirrors the prototype's `bbnf_json_prototype::parse_array::<V>`
// (crates/bbnf-json-prototype/src/lib.rs:308). Bypasses the tape;
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
        /// Mirrors `bbnf_json_prototype::parse_array::<V>`. Bypasses
        /// the tape entirely.
        #[inline(always)]
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
