//! Tape-path Alt-dispatch emission for Wrap rules.
//!
//! Walks every Alt branch, partitions them into byte-dispatch arms
//! (Refs whose target carries a bounded first-byte set) and
//! linear-try fallback arms (Regex branches, Refs with unbounded
//! first sets, overlapping first sets), and emits the dispatcher
//! body. Honours `wrap_can_elide_compound` to skip the outer
//! Rule-compound when every branch already pushes its own tape
//! record (AY.W2.6).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::shape_fn_ident;
use super::{
    alt_branch_payload_value_for_wrap, shape_tag_name, unwrap_outer, wrap_rule_accepts_leading_ws,
};

/// Emit the Alt-dispatch body for the Wrap tape-path emitter.
///
/// Each branch is a Ref or Regex. For Ref branches we look up the
/// target rule's shape tag and emit a direct call to the matching
/// shape fn. The emission partitions branches into two sets:
///
/// 1. **Byte-dispatch branches** — Ref-to-classified whose target
///    carries a bounded first-byte set (≤ 16 bytes). These emit as
///    match arms keyed on the byte patterns.
///
/// 2. **Linear-try fallback branches** — Regex branches, Refs with
///    unbounded/large first sets, or overlapping first sets. These
///    emit as sequential attempts that roll back `*p` + tape columns
///    on failure, breaking out on first success.
///
/// The pre-AX.W0a.2.e emission delegated the fallback set to the
/// grammar's `#dispatcher_ident` (`__value`). For non-Alt-rooted
/// grammars that dispatcher IS the root shape fn, so the fallback
/// loops through the root unbounded. The linear-try rewrite
/// eliminates the cycle while preserving byte-dispatch performance
/// for the common disjoint-FIRST case (JSON `value`).
pub(super) fn emit_alt_tape_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
    rule_variant_idx: u8,
    rule: &IrRule,
    elide_compound: bool,
) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let _ = dispatcher_ident; // retained for trait-level compat; not emitted.
    // AX.W0a.2.s — Wrap rules whose branches accept whitespace as a
    // leading byte (CSS `combinator = /\s*>\s*/ | /\s*\+\s*/ |
    // /\s*~\s*/ | /\s+/`) cannot pre-skip at entry — the `/\s+/`
    // alternative needs the whitespace itself. Peek without advancing
    // when the rule is ws-leading; the first-byte arm match and the
    // linear-try branches both work fine on a non-advancing peek.
    let ws_leading = wrap_rule_accepts_leading_ws(rule, ir);
    // AX.W0a.2.q — rule-type-driven leaf kind; Wrap Alt branches
    // that carry typed `-> Nu8` / `-> bool` payloads push a leaf
    // whose kind matches the Keyword emitter's policy so walker-
    // parity readers see identical record shapes regardless of
    // whether the rule is Keyword-led (literal branches) or
    // Wrap-led (regex branches).
    let leaf_kind = super::wrap_rule_leaf_kind(rule, ir);

    // AX.W0a.2.j — Wrap emission must push a parent `Rule` compound
    // enclosing the chosen branch's compound so downstream IR-lowering
    // sees `Alt(Ref, Ref, …)` structural identity on re-parse (the
    // bootstrap self-host cycle). The compound's `variant_idx` carries
    // the **owning rule's id** so `TapeCursor::rule_kind()` resolves to
    // the Wrap rule's kind (`grammar_item`, `term`, `directive`,
    // `value_atom` for bbnf.bbnf). `meta_idx` carries the chosen
    // branch ordinal (0..3) so downstream sub-variant projection can
    // distinguish branches per-record.
    //
    // Walker-era parity pushed an analogous compound; the shape-
    // authoritative emission restores it here without reintroducing
    // walker code paths. The nested branch compound pushes its own
    // leaf/compound children into the Wrap's children run, so lowering
    // walks `Rule(grammar_item) → Rule(comment | big_comment | …)` the
    // way walker tape did.

    // Partition branches into byte-dispatch and linear-try sets. The
    // split mirrors `alt_dispatch.rs`'s treatment: a branch whose
    // first-byte set is bounded contributes a per-byte entry; every
    // other branch contributes an in-order linear-try body.
    //
    // Branches that share a first byte (e.g. BBNF's `comment` and
    // `big_comment` both starting with `/`) must collapse into ONE
    // match arm that linear-tries each in grammar order; otherwise
    // Rust's `match` would only take the first arm and later
    // same-byte branches become unreachable.
    let mut per_byte: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut linear_arms: Vec<TokenStream> = Vec::new();

    for (ord, branch) in branches.iter().enumerate() {
        let inner = unwrap_outer(&branch.node);
        // AX.W0a.2.q — pass the full branch so Regex-branch emission
        // can inspect the outer `Map { Regex, BoolLit | IntLit }`
        // annotation and write the typed-byte payload into the arena
        // (Sheets `boolean` canonical case; CSS `boolean`-like analogues).
        let body_call = emit_wrap_branch_call_tape(
            inner,
            branch,
            rule_variant_idx,
            &leaf_kind,
            grammar_suffix,
            ir,
        );
        let Some((call, first_bytes)) = body_call else {
            // Branch is not emitter-routable (unclassified or structurally
            // unsupported). Skip — under `has_shape_dispatcher_entrypoint`
            // admission every reachable Ref target is classified, so the
            // only remaining unroutable branches are structurally malformed
            // grammars which reject during classification.
            continue;
        };
        let branch_ord = (ord & 0x1F) as u8; // meta_idx is 5 bits (0..=31).
        let linear_body = emit_wrap_linear_body_tape(&call, branch_ord);
        if first_bytes.is_empty() {
            linear_arms.push(linear_body);
        } else {
            for b in first_bytes {
                per_byte.entry(b).or_default().push(linear_body.clone());
            }
        }
    }

    let byte_arms: Vec<TokenStream> = per_byte
        .into_iter()
        .map(|(byte, bodies)| {
            let byte_lit = byte;
            quote! {
                #byte_lit => {
                    #(#bodies)*
                }
            }
        })
        .collect();

    let first_peek = if ws_leading {
        // Peek without advancing: ws-leading branches need the
        // whitespace preserved at `*p`.
        quote! {
            let first = *input
                .get(*p)
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
        }
    } else {
        quote! {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
        }
    };
    if elide_compound {
        // AY.W2.6 — wrap-compound elision. Every branch's call already
        // pushes its own tape record; the outer Rule-compound would
        // merely double-wrap. Skip the `mark_children` bracket and
        // the `push_compound` emission; wrap is transparent at the
        // call site per AW-V.W4-fix and returns unit on success.
        let _ = (rule_variant_idx, quote! { __wrap_chosen_meta });
        quote! {
            let mut __wrap_chosen_meta: u8 = 0;
            #first_peek
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#linear_arms)*
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            let _ = __wrap_chosen_meta;
            Ok(())
        }
    } else {
        quote! {
            let __wrap_enter_p = *p as u32;
            // B5.W6 — open the post-order children scope. The bracket
            // bumps `current_depth` so child records pushed inside the
            // chosen branch stamp `frame_depth` at the correct
            // (parent + 1) value at push time. The matching
            // `end_compound_post_order` below absorbs the bump.
            let __wrap_enter_child = builder.enter_post_order_children();
            let mut __wrap_chosen_meta: u8 = 0;
            #first_peek
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#linear_arms)*
                // B5.W6 — every branch failed; close the bracket
                // explicitly so `current_depth` reflects the outer
                // frame before the error propagates. Rollback inside
                // each branch only rewinds the structural columns;
                // the bracket counter is the emitter's responsibility.
                builder.exit_post_order_children();
                return ::core::result::Result::Err(
                    crate::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            // AY-II.W0.b — unified compound emission via begin_compound_post /
            // end_compound_post_order. The Wrap rule's compound is walker-parity
            // post-order: it lands AFTER the chosen branch's records.
            // Post-W0.a the outer Rule row is allocated at the current
            // (post-branch) columns.len(); the matching
            // end_compound_post_order back-patches span_hi + child_off
            // + HAS_CHILDREN (child_off stamps to __wrap_enter_child,
            // the first branch record). `flags` carries the chosen
            // meta discriminant so sub-variant projection distinguishes
            // branches.
            //
            // B5.W6 — `begin_compound_post` stamps the outer row at the
            // outer-frame depth without bumping `current_depth`, the
            // bracket having already done so via
            // `enter_post_order_children`.
            let __wrap_exit_p = *p as u32;
            let __wrap_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Rule,
                __wrap_enter_p,
                #rule_variant_idx,
                __wrap_chosen_meta,
                0u16,
            );
            // AY-II.W0-fix — end_compound_post_order stamps span_hi +
            // child_off (backward to first branch record) + HAS_CHILDREN
            // atomically. Pre-fix the manual end_compound +
            // set_child_off_at pair never set HAS_CHILDREN_BIT for the
            // post-order layout (end_compound's heuristic requires
            // open_offset + 1 < cols.len(), false for post-order), so
            // readers saw has_children == false on a compound that DID
            // have children.
            builder.end_compound_post_order(
                __wrap_off,
                __wrap_exit_p,
                crate::runtime::tape::TapeOffset(__wrap_enter_child),
            );
            Ok(())
        }
    }
}

/// Emit the guts of a Wrap-branch attempt: save `*p` + column length,
/// invoke `call`, on failure restore both + fall through; on success
/// write branch ordinal + `break 'try_branches`.
fn emit_wrap_linear_body_tape(call: &TokenStream, branch_ord: u8) -> TokenStream {
    quote! {
        {
            let attempt_p = *p;
            // AY-II.W0.b — capture columns length as an explicit u32
            // open_offset; rollback_to (W0.a) replaces truncate to
            // signal checkpoint/restore intent to the fused pipeline.
            let attempt_len = builder.position();
            match #call {
                Ok(_) => {
                    __wrap_chosen_meta = #branch_ord;
                    break 'try_branches;
                }
                Err(_) => {
                    *p = attempt_p;
                    builder.rollback_to(attempt_len);
                }
            }
        }
    }
}

/// Resolve a Wrap-branch body to its shape-fn call + first-byte set.
/// Returns `(call_tokens, first_bytes)` or `None` when the branch is
/// structurally ineligible. An empty `first_bytes` denotes an
/// unbounded first set — the branch routes as a linear-try fallback.
fn emit_wrap_branch_call_tape(
    inner: &IrNode,
    branch: &bbnf_ir::AltBranch,
    rule_variant_idx: u8,
    leaf_kind: &TokenStream,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> Option<(TokenStream, Vec<u8>)> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match inner {
        IrNode::Ref(rid) => {
            let target = ir.rules.iter().find(|r| r.id == *rid)?;
            let tag = ir.shape_assignments.get(*rid);
            let shape_name = shape_tag_name(tag)?;
            let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
            // AX.W0a.2.g — Keyword fn signature extended with `state`.
            let call = match tag {
                ShapeTag::Number => quote! {
                    #target_fn(input, p, first, builder)
                },
                ShapeTag::Keyword => quote! {
                    #target_fn(input, p, first, state, builder)
                },
                _ => quote! {
                    #target_fn(input, p, state, builder)
                },
            };
            let first_bytes: Vec<u8> = target.meta.first_set.iter().collect();
            if first_bytes.len() > 16 {
                Some((call, Vec::new()))
            } else {
                Some((call, first_bytes))
            }
        }
        IrNode::Regex(sid) => {
            // AX.W0a.2.q — when the Regex branch carries a typed `Map {
            // Regex, BoolLit | IntLit }` annotation (Sheets
            // `boolean = /TRUE/i -> true | /FALSE/i -> false`, and
            // structurally-analogous u8-tagged Alt rules), emit the
            // scan + arena payload push with the same kind policy the
            // Keyword emitter uses for typed-Alt leaves. This closes
            // the `boolean_first_branch_fires_true_payload` parity test
            // without reintroducing walker code paths.
            //
            // Pre-W0a.2.q emission pushed `PayloadData::None` with a
            // `Literal`-kind leaf; the `typed_u8_payloads` reader
            // (Span or KvPair + `has_payload()` + `payload_bytes(rec,
            // 1)`) saw nothing, and the rule's `-> Nu8` discriminant
            // never reached the tape.
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident = super::super::super::dfa_codegen::regex_scan_adapter_ident(
                &super::super::sanitise_grammar(grammar_suffix),
            );
            let typed_payload = alt_branch_payload_value_for_wrap(branch, ir);
            let call = match typed_payload {
                Some(payload_u32) => {
                    // AX.W0a.2.q — typed-payload Regex branch. The
                    // `wrap_rule_leaf_kind`-derived `leaf_kind` sits in
                    // the caller's scope (bound via `quote!` ident).
                    // Emit the scan, commit `*p`, write 1 byte to the
                    // arena, push a leaf carrying the arena payload.
                    // `variant_idx = rule.id & 0xFF` so downstream
                    // readers (`typed_u8_payloads`) see a record whose
                    // `variant_idx()` matches the walker's Seq-
                    // promotion output.
                    quote! {
                        {
                            let span_lo = *p as u32;
                            match #regex_scan_ident(#pattern, input, *p) {
                                ::core::option::Option::Some(len) => {
                                    *p += len as usize;
                                    let __arena_off: u32 =
                                        builder.arena_mut().len() as u32;
                                    builder.arena_mut().push((#payload_u32) as u8);
                                    let _ = builder.push_leaf_with_arena_payload(
                                        #leaf_kind,
                                        span_lo,
                                        *p as u32,
                                        #rule_variant_idx,
                                        0u8,
                                        __arena_off,
                                        1u32,
                                    );
                                    ::core::result::Result::<
                                        crate::runtime::tape::TapeOffset,
                                        crate::runtime::tape::DtaError,
                                    >::Ok(())
                                }
                                ::core::option::Option::None => {
                                    ::core::result::Result::Err(
                                        crate::runtime::tape::DtaError::Syntax {
                                            offset: span_lo,
                                            failing_state:
                                                crate::runtime::tape::DtaStateId::NONE,
                                            failing_rule:
                                                crate::runtime::tape::DtaRuleId(u32::MAX),
                                        },
                                    )
                                }
                            }
                        }
                    }
                }
                None => {
                    // Untyped Regex branch — preserve the pre-W0a.2.q
                    // Span-leaf / no-payload emission.
                    quote! {
                        {
                            let span_lo = *p as u32;
                            match #regex_scan_ident(#pattern, input, *p) {
                                ::core::option::Option::Some(len) => {
                                    *p += len as usize;
                                    let _ = builder.push_leaf_with(
                                        crate::runtime::tape::TapeKind::Span,
                                        span_lo,
                                        *p as u32,
                                        0,
                                        0,
                                        crate::runtime::tape::PayloadData::None,
                                    );
                                    ::core::result::Result::<
                                        crate::runtime::tape::TapeOffset,
                                        crate::runtime::tape::DtaError,
                                    >::Ok(())
                                }
                                ::core::option::Option::None => {
                                    ::core::result::Result::Err(
                                        crate::runtime::tape::DtaError::Syntax {
                                            offset: span_lo,
                                            failing_state:
                                                crate::runtime::tape::DtaStateId::NONE,
                                            failing_rule:
                                                crate::runtime::tape::DtaRuleId(u32::MAX),
                                        },
                                    )
                                }
                            }
                        }
                    }
                }
            };
            // Regex branches have no bounded first-byte set at emit time
            // (pattern-first-byte analysis would require NFA inspection);
            // route as linear-try fallback.
            Some((call, Vec::new()))
        }
        _ => None,
    }
}
