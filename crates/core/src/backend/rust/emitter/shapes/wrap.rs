//! Wrap-shape emitter — `parse_wrap_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Wrap-shape parse functions for transparent
//! `Alt(Ref, Ref, …)` dispatchers. The Wrap-shape rule emits NO
//! compound of its own — it's a pass-through that dispatches to the
//! chosen branch rule's shape fn.
//!
//! Canonical:
//! - JSON `value = object | array | string | number | bool | null` —
//!   byte-dispatch onto the 6 branch shape fns.
//! - CSS `color = colorMix | colorFn | hex | colorFunction |
//!   namedColor` — each branch is a Ref.
//! - Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/` —
//!   mixed Ref + Regex branches.
//! - BBNF `rhs = closure | alternation`.
//!
//! # Emission shape
//!
//! The emitted function performs a byte-dispatch on the first
//! non-whitespace byte and directly delegates to the chosen branch's
//! shape fn. No outer compound is pushed — the branch's own compound
//! carries the final record (walker parity: the DTA's ByteDispatch
//! state emits no compound either).
//!
//! For branches where a byte-prefix-dispatch is possible (the Ref's
//! target rule's FIRST byte set is disjoint from siblings), the arm
//! is a direct Literal-byte match to the chosen shape fn. For Regex
//! branches or overlapping-FIRST branches, the arm falls through to
//! the grammar's value-dispatcher (which handles the per-grammar
//! Alt-dispatch table).
//!
//! # Wire contract
//!
//! Walker-tape parity: the chosen branch's shape fn emits the tape
//! record carrying the rule's semantic payload. Wrap itself emits
//! nothing. The dispatcher-fallback path hooks the walker for any
//! branch whose shape fn isn't in the emitter's reach.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, shape_fn_ident, visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Returns true when the Wrap rule's first-byte set includes an
/// ASCII whitespace byte — i.e. the rule's pattern matches significant
/// whitespace as its leading input. CSS `combinator` is the canonical
/// case (`/\s*>\s*/ | … | /\s+/`). When this returns `true` the
/// Wrap body must NOT pre-skip whitespace: the `/\s+/` branch relies
/// on the whitespace being present at `*p`. Walks `Alt`, `Regex`,
/// `Literal`, and transparent structural wrappers via `regex_info`
/// for the ASCII whitespace set.
fn wrap_rule_accepts_leading_ws(rule: &IrRule, ir: &GrammarIR) -> bool {
    accepts_leading_ws_bounded(&rule.body, ir, 3)
}

fn accepts_leading_ws_bounded(node: &IrNode, ir: &GrammarIR, budget: usize) -> bool {
    if budget == 0 {
        return false;
    }
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.strings[*sid as usize].as_bytes();
            bytes
                .first()
                .map(|&b| matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0C))
                .unwrap_or(false)
        }
        IrNode::Regex(sid) => {
            if let Some(info) = ir.regex_info.get(sid) {
                info.first_chars.has(b' ')
                    || info.first_chars.has(b'\t')
                    || info.first_chars.has(b'\n')
                    || info.first_chars.has(b'\r')
            } else {
                false
            }
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .any(|b| accepts_leading_ws_bounded(&b.node, ir, budget)),
        IrNode::Seq(children) => children
            .first()
            .map(|c| accepts_leading_ws_bounded(c, ir, budget))
            .unwrap_or(false),
        IrNode::Next(a, _) | IrNode::Skip(a, _) => accepts_leading_ws_bounded(a, ir, budget),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Repeat { inner, lo, .. } if *lo > 0 => {
            accepts_leading_ws_bounded(inner, ir, budget)
        }
        IrNode::Ref(rid) => {
            let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
                return false;
            };
            accepts_leading_ws_bounded(&target.body, ir, budget - 1)
        }
        _ => false,
    }
}

/// AX.W0a.2.q — rule-type-driven leaf-kind selection for Wrap's
/// typed-payload Alt branches. Mirrors
/// [`super::keyword::rule_keyword_leaf_kind`]'s policy so the
/// walker-parity readers see the same kind regardless of whether the
/// rule routes through Keyword (literal-led) or Wrap (regex-led).
///
/// - `TypeDesc::U8` rules → `TapeKind::KvPair` (CSS `dir_pseudo_*`,
///   Sheets `compare_op` / `add_op` / `mul_op` context match).
/// - `TypeDesc::Bool` rules → `TapeKind::Span` (JSON `bool`, Sheets
///   `boolean`).
/// - Other typed rules → `TapeKind::Span` (conservative default;
///   HRegex f64/u32 payloads follow this branch).
fn wrap_rule_leaf_kind(rule: &IrRule, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::TypeDesc;
    let ty = ir.types.iter().find_map(|(rid, t)| {
        if *rid == rule.id {
            Some(t.clone())
        } else {
            None
        }
    });
    match ty {
        Some(TypeDesc::U8) => quote! { ::bbnf::runtime::tape::TapeKind::KvPair },
        _ => quote! { ::bbnf::runtime::tape::TapeKind::Span },
    }
}

/// AX.W0a.2.q — extract the `u32` payload value from a Wrap Alt
/// branch's `Map { fn_id }` annotation, if present. Handles `IntLit`
/// and `BoolLit` MapExpr forms via the shared
/// [`super::keyword::alt_branch_payload_value`] helper (re-exported as
/// a module-local helper so wrap and keyword agree on encoding).
///
/// Returns `Some(token)` with the u32 literal when the branch is
/// Map-annotated with a scalar constant; `None` otherwise (structural
/// emission without typed payload).
fn alt_branch_payload_value_for_wrap(
    branch: &bbnf_ir::AltBranch,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = find_map_fn(&branch.node)?;
    let fd = ir.fns.get(fn_id as usize)?;
    let FnDescriptor::Expr { expr, .. } = fd else {
        return None;
    };
    match expr {
        MapExpr::BoolLit(b) => {
            let v = if *b { 1u32 } else { 0u32 };
            Some(quote! { #v })
        }
        MapExpr::IntLit(n) => {
            let v = *n as u32;
            Some(quote! { #v })
        }
        _ => None,
    }
}

/// AY.W2.6 — whether a Wrap rule's outer `push_compound` is safe to
/// elide. The elision is sound when every Alt branch's shape-fn call
/// already emits a self-contained tape record (either a compound via
/// `push_compound` or a leaf via `push_leaf_with`), so the outer
/// Rule-compound merely double-wraps the branch's own record.
///
/// # Criterion
///
/// The wrap body must be an `Alt` whose every branch is either:
///
/// - A `Ref(rid)` where the target rule carries a classified
///   `ShapeTag` (Object / Array / String / Number / Keyword /
///   Scalar / Pratt / Unordered / ArgList / Flat / HRegex / Wrap /
///   AltDispatch). Classified branches route through shape fns that
///   push their own record.
/// - A `Regex` branch — the `emit_wrap_branch_call_tape` path
///   already calls `push_leaf_with` on a successful scan.
///
/// Rules that don't satisfy this criterion (e.g. bare Alt of
/// Literal branches, or Alt branches whose target shape is
/// unclassified) retain the outer wrap for safety.
///
/// Per AY.md prop 2 / invariant 23 part 2: JSON's `value` rule
/// matches this criterion — cutting twitter tape record count from
/// ~158K to ~80K (sonic-rs-node-count parity).
fn wrap_can_elide_compound(rule: &IrRule, ir: &GrammarIR) -> bool {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    let body = unwrap_outer(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return false;
    };
    if branches.is_empty() {
        return false;
    }
    branches.iter().all(|b| {
        match unwrap_outer(&b.node) {
            IrNode::Ref(rid) => {
                let tag = ir.shape_assignments.get(*rid);
                !matches!(tag, ShapeTag::None)
            }
            IrNode::Regex(_) => true,
            _ => false,
        }
    })
}

/// Emit `pub fn parse_wrap_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_wrap(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_variant_idx = (rule.id & 0xFF) as u8;

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    // AY.W2.6 — wrap-compound elision: when every Alt branch's call
    // already emits its own tape record, the outer Rule-compound is a
    // no-op wrapper that doubles the record count. Per AY.md prop 2 /
    // invariant 23 part 2, skip the outer compound emission for such
    // rules (JSON `value` is the canonical case).
    let elide_compound = wrap_can_elide_compound(rule, ir);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_tape_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
            rule_variant_idx,
            rule,
            elide_compound,
        ),
        IrNode::Ref(rid) => {
            // Non-Alt Wrap body — single Ref transparent alias. Resolve
            // the target shape at emission time and call directly.
            // AX.W0a.2.e — never delegate to `#dispatcher_ident` which
            // for non-Alt-rooted grammars loops through the root.
            let _ = dispatcher_ident;
            match super::dispatcher::emit_ref_call_tape(grammar_suffix, *rid, ir) {
                Some(call) => quote! { #call },
                None => quote! {
                    ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state:
                                ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule:
                                ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    )
                },
            }
        }
        _ => {
            // Any other non-Alt Wrap body is structurally unsupported
            // under `has_shape_dispatcher_entrypoint` admission; emit a
            // syntax error rather than infinite-loop via the dispatcher.
            let _ = dispatcher_ident;
            quote! {
                ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state:
                            ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule:
                            ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                )
            }
        }
    };

    quote! {
        /// AW-V.W4-fix — per-grammar Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch
        /// to the chosen branch's shape fn, return that shape fn's
        /// offset unchanged. No outer compound emission; the
        /// branch's own shape fn owns the tape record.
        ///
        /// AX.W0a.2.f — compound; see `flat.rs` emission for the
        /// `#[inline]` downgrade rationale (LLVM inline-cycle
        /// collapse vs hard-requirement inliner abort).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::FusedBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            #dispatch
        }
    }
}

/// Peel Map / OptionalWhitespace wrappers to reach the structural Alt
/// / Ref body.
fn unwrap_outer(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            unwrap_outer(inner)
        }
        _ => node,
    }
}

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
fn emit_alt_tape_dispatch(
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
    let leaf_kind = wrap_rule_leaf_kind(rule, ir);

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
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
        }
    } else {
        quote! {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
        }
    };
    if elide_compound {
        // AY.W2.6 — wrap-compound elision. Every branch's call already
        // pushes its own tape record; the outer Rule-compound would
        // merely double-wrap. Skip the `mark_children` bracket, the
        // `push_compound` emission, and return the branch's own offset
        // (`TapeOffset::NONE` — wrap is transparent at the call site
        // per AW-V.W4-fix).
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
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            let _ = __wrap_chosen_meta;
            Ok(::bbnf::runtime::tape::TapeOffset::NONE)
        }
    } else {
        quote! {
            let __wrap_enter_p = *p as u32;
            let __wrap_enter_child = builder.columns_mut().len() as u32;
            let mut __wrap_chosen_meta: u8 = 0;
            #first_peek
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#linear_arms)*
                return ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            // AY-II.W0.b — unified compound emission via begin_compound /
            // end_compound. The Wrap rule's compound is walker-parity
            // post-order: it lands AFTER the chosen branch's records.
            // Post-W0.a the outer Rule row is allocated at the current
            // (post-branch) columns.len() via begin_compound; the
            // matching end_compound back-patches span_hi + child_off
            // + HAS_CHILDREN (child_off stamps to __wrap_enter_child,
            // the first branch record). `flags` carries the chosen
            // meta discriminant so sub-variant projection distinguishes
            // branches. `frame_depth` = variant_idx as before.
            let __wrap_exit_p = *p as u32;
            let __wrap_off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                __wrap_enter_p,
                #rule_variant_idx,
                __wrap_chosen_meta,
                0u8,
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
                ::bbnf::runtime::tape::TapeOffset(__wrap_enter_child),
            );
            Ok(::bbnf::runtime::tape::TapeOffset(__wrap_off))
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
            let attempt_len = builder.columns_mut().len() as u32;
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
            let target_fn =
                shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
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
            let regex_scan_ident = super::super::dfa_codegen::regex_scan_adapter_ident(
                &super::sanitise_grammar(grammar_suffix),
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
                                        ::bbnf::runtime::tape::TapeOffset,
                                        ::bbnf::runtime::tape::DtaError,
                                    >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                                }
                                ::core::option::Option::None => {
                                    ::core::result::Result::Err(
                                        ::bbnf::runtime::tape::DtaError::Syntax {
                                            offset: span_lo,
                                            failing_state:
                                                ::bbnf::runtime::tape::DtaStateId::NONE,
                                            failing_rule:
                                                ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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
                                        ::bbnf::runtime::tape::TapeKind::Span,
                                        span_lo,
                                        *p as u32,
                                        0,
                                        0,
                                        ::bbnf::runtime::tape::PayloadData::None,
                                    );
                                    ::core::result::Result::<
                                        ::bbnf::runtime::tape::TapeOffset,
                                        ::bbnf::runtime::tape::DtaError,
                                    >::Ok(::bbnf::runtime::tape::TapeOffset::NONE)
                                }
                                ::core::option::Option::None => {
                                    ::core::result::Result::Err(
                                        ::bbnf::runtime::tape::DtaError::Syntax {
                                            offset: span_lo,
                                            failing_state:
                                                ::bbnf::runtime::tape::DtaStateId::NONE,
                                            failing_rule:
                                                ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
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

/// Convert a [`ShapeTag`] into the shape-fn prefix. Returns `None`
/// when the tag is `None` (unclassified).
fn shape_tag_name(
    tag: bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag,
) -> Option<&'static str> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match tag {
        ShapeTag::Object => Some("object"),
        ShapeTag::Array => Some("array"),
        ShapeTag::String => Some("string"),
        ShapeTag::Number => Some("number"),
        ShapeTag::Keyword => Some("keyword"),
        ShapeTag::Scalar => Some("scalar"),
        ShapeTag::Pratt => Some("pratt"),
        ShapeTag::Unordered => Some("unordered"),
        ShapeTag::ArgList => Some("arglist"),
        ShapeTag::Flat => Some("flat"),
        ShapeTag::Wrap => Some("wrap"),
        ShapeTag::HRegex => Some("hregex"),
        ShapeTag::AltDispatch => Some("altdispatch"),
        ShapeTag::None => None,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path Wrap emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_wrap_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_wrap_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_visitor_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
        ),
        _ => quote! {
            #dispatcher_ident(input, p, state, visitor)
        },
    };

    quote! {
        /// AW-V.W4-fix — visitor-path Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch to
        /// the chosen branch's visitor-path shape fn. No visitor event
        /// fires here; the chosen branch's visitor fn owns the event
        /// emission.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
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
            #dispatch
        }
    }
}

/// Emit the visitor-path Alt-dispatch body.
fn emit_alt_visitor_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let mut ref_arms: Vec<TokenStream> = Vec::new();
    for branch in branches {
        let inner = unwrap_outer(&branch.node);
        let IrNode::Ref(rid) = inner else { continue };
        let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let tag = ir.shape_assignments.get(*rid);
        let shape_name = shape_tag_name(tag);
        let Some(shape_name) = shape_name else { continue };
        let target_fn =
            visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
        let first_bytes: Vec<u8> =
            target.meta.first_set.iter().collect();
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            continue;
        }
        let byte_pats: Vec<TokenStream> =
            first_bytes.iter().map(|b| quote! { #b }).collect();
        // AX.W0a.2.g — visitor-path Keyword signature extended with
        // `state` (see tape-path call).
        let call = match tag {
            ShapeTag::Number => quote! {
                #target_fn(input, p, first, visitor)
            },
            ShapeTag::Keyword => quote! {
                #target_fn(input, p, first, state, visitor)
            },
            ShapeTag::String => quote! {
                #target_fn(input, p, state, visitor, /*is_key=*/ false)
            },
            _ => quote! {
                #target_fn(input, p, state, visitor)
            },
        };
        ref_arms.push(quote! {
            #(#byte_pats)|* => #call,
        });
    }

    if ref_arms.is_empty() {
        return quote! {
            let _ = #support_mod::skip_space(input, p, state);
            #dispatcher_ident(input, p, state, visitor)
        };
    }

    let fallback = quote! {
        _ => #dispatcher_ident(input, p, state, visitor),
    };

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
        match first {
            #(#ref_arms)*
            #fallback
        }
    }
}
