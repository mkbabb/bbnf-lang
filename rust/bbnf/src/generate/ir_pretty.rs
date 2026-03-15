//! Pretty-printer code generation from IR.
//!
//! Generates `to_doc()` and `source_range()` impl blocks for the parser enum
//! by walking IR rule bodies and `PrettyHints` metadata.
//!
//! This module replaces the AST-based `prettify/` directory orchestrator.
//! It reuses the existing doc generation functions (`to_doc.rs`, `source_range.rs`,
//! `hints.rs`, `prettify_utils.rs`) which are AST-independent — they operate on
//! `syn::Type` + hint strings, not AST nodes.

use bbnf_ir::{GrammarIR, IrNode, IrRule, PrettyHints};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::{type_desc_to_syn, type_is_span, IrCodegenCtx};
use super::prettify::hints::{self, is_valid_hint};
use super::prettify::prettify_utils::*;
use super::prettify::source_range::generate_compound_range;
use super::prettify::to_doc::*;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Generate `to_doc()` and `source_range()` impl blocks from IR.
pub fn generate_prettify_ir(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let has_recovers = ctx.ir.rules.iter().any(|r| r.meta.recover.is_some())
        && !ctx.parser_attrs.skip_recover;

    let mut to_doc_arms = Vec::new();
    let mut source_range_arms = Vec::new();

    for rule in &ctx.ir.rules {
        // Skip transparent rules — they don't have enum variants.
        if rule.meta.is_transparent {
            continue;
        }

        let name = ctx.ir.get_string(rule.name);
        let variant = format_ident!("{}", name);

        // Get the inferred type for this rule.
        let ty = match ctx.rule_types.get(&rule.id) {
            Some(t) => t.clone(),
            None => ctx.boxed_enum_type.clone(),
        };

        // Unwrap Map wrapper to find the inner expression for pattern detection.
        let inner = unwrap_ir_map(&rule.body);

        // Get @pretty hints for this rule.
        let hints = resolve_ir_hints(rule, &ty, ctx);

        // Validate explicit hints.
        if let Some(ref ph) = rule.meta.pretty {
            let explicit = pretty_hints_to_strings(ph);
            for hint in &explicit {
                if !is_valid_hint(hint) {
                    let valid = hints::valid_hint_names();
                    panic!(
                        "@pretty directive for rule `{}` contains unknown hint `{}`. \
                         Valid hints are: {}",
                        name,
                        hint,
                        valid.join(", ")
                    );
                }
            }
        }

        // Determine type shape.
        let is_span = type_is_span(&ty);
        let is_vec = is_vec_type(&ty);

        // Check for wrapped pattern before type dispatch.
        let wrapped = detect_wrapped_pattern_ir(inner, ctx.ir)
            .or_else(|| resolve_and_detect_wrapped_ir(inner, ctx.ir));

        // Generate the to_doc match arm.
        let doc_body = if let Some((ref left, ref right)) = wrapped {
            if is_span {
                generate_wrapped_span_doc(&variant, left, right, &hints)
            } else {
                generate_wrapped_doc(&variant, left, right, &ty, &hints)
            }
        } else if is_span {
            generate_span_doc(&variant, &hints)
        } else if is_vec {
            generate_vec_doc_ir(&variant, &ty, &hints)
        } else {
            generate_compound_doc_ir(&variant, inner, &ty, &hints, ctx)
        };
        to_doc_arms.push(doc_body);

        // Generate source_range arm.
        let range_body = if is_span {
            quote! {
                Self::#variant(s) => Some((s.start, s.end)),
            }
        } else if is_vec {
            let item_source_range = generate_item_source_range(&ty);
            quote! {
                Self::#variant(items) => {
                    let mut _min_s = usize::MAX;
                    let mut _max_e = 0usize;
                    let mut _found = false;
                    for i in items.iter() {
                        if let Some((s, e)) = #item_source_range {
                            if s < _min_s { _min_s = s; }
                            if e > _max_e { _max_e = e; }
                            _found = true;
                        }
                    }
                    if _found { Some((_min_s, _max_e)) } else { None }
                }
            }
        } else {
            generate_compound_range(&variant, &ty)
        };
        source_range_arms.push(range_body);
    }

    // Add sub-variant arms for heterogeneous alternation branches.
    generate_sub_variant_arms(ctx, &mut to_doc_arms, &mut source_range_arms);

    // Recovered variant handling.
    if has_recovers {
        to_doc_arms.push(quote! {
            Self::Recovered => ::pprint::Doc::Null,
        });
        source_range_arms.push(quote! {
            Self::Recovered => None,
        });
    }

    quote! {
        impl<'a> #enum_ident<'a> {
            pub fn to_doc(&self) -> ::pprint::Doc<'a> {
                match self {
                    #(#to_doc_arms)*
                }
            }

            pub fn source_range(&self) -> Option<(usize, usize)> {
                match self {
                    #(#source_range_arms)*
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// IR pattern detectors
// ---------------------------------------------------------------------------

/// Detect `"L" >> middle << "R"` (wrapped repetition) in IR.
///
/// Matches `Skip(Next(Literal(L), inner), Literal(R))` or variants with
/// OptionalWhitespace wrappers.
fn detect_wrapped_pattern_ir(node: &IrNode, _ir: &GrammarIR) -> Option<(String, String)> {
    let node = unwrap_ir_whitespace(node);
    if let IrNode::Skip(left, right) = node {
        let left = unwrap_ir_whitespace(left);
        let right_node = unwrap_ir_whitespace(right);
        if let IrNode::Next(next_left, _) = left {
            let next_left_inner = unwrap_ir_whitespace(next_left);
            if let IrNode::Literal(l_sid) = next_left_inner {
                if let IrNode::Literal(r_sid) = right_node {
                    return Some((
                        _ir.get_string(*l_sid).to_string(),
                        _ir.get_string(*r_sid).to_string(),
                    ));
                }
            }
        }
    }
    None
}

/// Resolve a Ref and detect wrapped pattern in the referenced rule.
fn resolve_and_detect_wrapped_ir(node: &IrNode, ir: &GrammarIR) -> Option<(String, String)> {
    if let IrNode::Ref(rule_id) = node {
        let rule = &ir.rules[*rule_id as usize];
        let inner = unwrap_ir_map(&rule.body);
        detect_wrapped_pattern_ir(inner, ir)
    } else {
        None
    }
}

/// Detect `key, sep >> value` (key-value pair) pattern in IR.
///
/// Matches `Seq([elem, Next(Literal(sep), value)])`.
fn detect_key_value_pattern_ir(node: &IrNode, ir: &GrammarIR) -> Option<(String, String)> {
    if let IrNode::Seq(children) = node {
        if children.len() == 2 {
            if let IrNode::Next(sep, _) = &children[1] {
                let sep_inner = unwrap_ir_whitespace(sep);
                if let IrNode::Literal(sep_sid) = sep_inner {
                    return Some((
                        "key".to_string(),
                        ir.get_string(*sep_sid).to_string(),
                    ));
                }
                // sep can be a Ref to a rule that is a literal.
                if let IrNode::Ref(sep_rule_id) = sep_inner {
                    let sep_name = ir.get_string(ir.rules[*sep_rule_id as usize].name);
                    return Some(("key".to_string(), sep_name.to_string()));
                }
            }
        }
    }
    None
}

/// Resolve a rule name's body to find a literal value (for separator resolution).
fn resolve_separator_literal_ir(name: &str, ir: &GrammarIR) -> Option<String> {
    for rule in &ir.rules {
        if ir.get_string(rule.name) == name {
            let inner = unwrap_ir_map(&rule.body);
            let unwrapped = unwrap_ir_whitespace(inner);
            if let IrNode::Literal(sid) = unwrapped {
                return Some(ir.get_string(*sid).to_string());
            }
        }
    }
    None
}

// ---------------------------------------------------------------------------
// IR heuristics
// ---------------------------------------------------------------------------

/// Infer @pretty hints from rule shape when no explicit hints exist.
fn infer_hints_ir(rule: &IrRule, ty: &syn::Type, ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> Vec<String> {
    use super::prettify::heuristics::HeuristicMode;

    // Resolve mode from grammar-level @pretty * directive.
    let mode = resolve_heuristic_mode_ir(ir);
    match mode {
        HeuristicMode::Off | HeuristicMode::Minimal => Vec::new(),
        HeuristicMode::Auto => {
            let name = ir.get_string(rule.name);
            let inner = unwrap_ir_map(&rule.body);

            // 1. Top-level detection by name.
            const TOPLEVEL_NAMES: &[&str] = &[
                "grammar", "program", "stylesheet", "module", "document", "file", "root",
            ];
            if TOPLEVEL_NAMES.contains(&name) {
                return vec!["block".to_string()];
            }

            // Shape-based: Vec of nonterminals at root.
            if is_vec_type(ty) && is_nonterminal_repetition_ir(inner) {
                return vec!["block".to_string()];
            }

            // 2. Block-delimited detection.
            if contains_brace_wrapped_ir(inner, ir) {
                return vec!["group".to_string(), "indent".to_string()];
            }

            // 3. Large compound detection — only when the rule body contains
            //    nonterminal references. Rules that are purely terminal
            //    concatenations (literals, regexes, optional/repeated terminals)
            //    are opaque tokens and should not receive formatting hints.
            if let syn::Type::Tuple(tuple) = ty {
                if tuple.elems.len() > 3 && contains_structured_ref_ir(inner, ctx) {
                    return vec!["group".to_string()];
                }
            }
            if is_box_enum_type(ty) && !type_is_span(ty) {
                if let IrNode::Alt(branches, _) = inner {
                    if branches.len() > 2 {
                        return vec!["group".to_string()];
                    }
                }
            }

            Vec::new()
        }
    }
}

/// Resolve the heuristic mode from the IR grammar.
fn resolve_heuristic_mode_ir(ir: &GrammarIR) -> super::prettify::heuristics::HeuristicMode {
    use super::prettify::heuristics::HeuristicMode;

    // Look for a rule named "*" with pretty hints (grammar-level @pretty * mode).
    for rule in &ir.rules {
        if ir.get_string(rule.name) == "*" {
            if let Some(ref ph) = rule.meta.pretty {
                let hints = pretty_hints_to_strings(ph);
                if let Some(mode_str) = hints.first() {
                    return HeuristicMode::from_str(mode_str).unwrap_or(HeuristicMode::Auto);
                }
            }
        }
    }
    HeuristicMode::Auto
}

/// Check if a node is a repetition of nonterminals.
fn is_nonterminal_repetition_ir(node: &IrNode) -> bool {
    match node {
        IrNode::Repeat { inner, .. } => is_or_contains_nonterminal_ir(inner),
        IrNode::OptionalWhitespace(inner) => is_nonterminal_repetition_ir(inner),
        _ => false,
    }
}

/// Check if a node is or contains a nonterminal Ref.
fn is_or_contains_nonterminal_ir(node: &IrNode) -> bool {
    match node {
        IrNode::Ref(_) => true,
        IrNode::OptionalWhitespace(inner) => is_or_contains_nonterminal_ir(inner),
        IrNode::Seq(children) => children.iter().any(is_or_contains_nonterminal_ir),
        _ => false,
    }
}

/// Check if a node contains a brace-wrapped pattern `"{" >> ... << "}"`.
fn contains_brace_wrapped_ir(node: &IrNode, ir: &GrammarIR) -> bool {
    match node {
        IrNode::Skip(left, right) => {
            let left = unwrap_ir_whitespace(left);
            let right_node = unwrap_ir_whitespace(right);
            if let IrNode::Next(next_left, _) = left {
                let next_left_inner = unwrap_ir_whitespace(next_left);
                if let IrNode::Literal(l_sid) = next_left_inner {
                    if let IrNode::Literal(r_sid) = right_node {
                        return ir.get_string(*l_sid) == "{"
                            && ir.get_string(*r_sid) == "}";
                    }
                }
            }
            false
        }
        IrNode::Seq(children) => children.iter().any(|c| contains_brace_wrapped_ir(c, ir)),
        IrNode::OptionalWhitespace(inner) => contains_brace_wrapped_ir(inner, ir),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// IR-specific doc generation wrappers
// ---------------------------------------------------------------------------

/// Vec doc generation — same as AST version but without Expression parameter.
fn generate_vec_doc_ir(
    variant: &syn::Ident,
    ty: &syn::Type,
    hints: &[String],
) -> TokenStream {
    // Reuse generate_vec_doc by passing a dummy Expression.
    // The Expression parameter `_inner` is unused in generate_vec_doc.
    // We duplicate the Vec logic here to avoid the unused dep.

    use super::prettify::hints::extract_sep_string;

    let custom_sep = hints.iter().find_map(|h| extract_sep_string(h));

    let sep = if let Some(sep_str) = custom_sep {
        let has_group = hints.contains(&"group".to_string());
        if has_group {
            let sep_lit = proc_macro2::Literal::string(sep_str);
            let break_sep = sep_str.trim_end();
            let break_lit = proc_macro2::Literal::string(break_sep);
            quote! {
                ::pprint::Doc::IfBreak(
                    Box::new(
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                        + ::pprint::Doc::Hardline
                    ),
                    Box::new(
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit))
                    ),
                )
            }
        } else {
            let sep_lit = proc_macro2::Literal::string(sep_str);
            quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit)) }
        }
    } else if hints.contains(&"blankline".to_string()) {
        quote! { ::pprint::Doc::Hardline + ::pprint::Doc::Hardline }
    } else if hints.contains(&"block".to_string())
        || hints.contains(&"fast".to_string())
        || hints.contains(&"hardbreak".to_string())
    {
        quote! { ::pprint::Doc::Hardline }
    } else if hints.contains(&"nobreak".to_string())
        || hints.contains(&"compact".to_string())
    {
        quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) }
    } else if hints.contains(&"softbreak".to_string()) {
        quote! { ::pprint::Doc::Softline }
    } else if hints.contains(&"off".to_string()) {
        quote! { ::pprint::Doc::Null }
    } else {
        quote! { ::pprint::Doc::Softline }
    };

    let item_to_doc = generate_item_to_doc(ty);

    let has_indent = hints.contains(&"indent".to_string());
    let has_hard_sep = hints.contains(&"block".to_string())
        || hints.contains(&"blankline".to_string())
        || hints.contains(&"hardbreak".to_string());

    let base = if has_indent && has_hard_sep {
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| #item_to_doc).collect();
                if docs.is_empty() {
                    ::pprint::Doc::Null
                } else {
                    ::pprint::Doc::Indent(Box::new(
                        ::pprint::Doc::Hardline
                            + ::pprint::Doc::Join(Box::new((#sep, docs)))
                    ))
                    + ::pprint::Doc::Hardline
                }
            }
        }
    } else {
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| #item_to_doc).collect();
                if docs.is_empty() {
                    ::pprint::Doc::Null
                } else {
                    ::pprint::Doc::Join(Box::new((#sep, docs)))
                }
            }
        }
    };

    let outer_hints: Vec<String> = if has_indent && has_hard_sep {
        hints.iter()
            .filter(|h| h.as_str() != "indent")
            .cloned()
            .collect()
    } else {
        hints.to_vec()
    };
    let doc = apply_outer_hints(base, &outer_hints);
    quote! {
        Self::#variant(items) => { #doc }
    }
}

/// Compound doc generation from IR — replaces the AST-based version.
fn generate_compound_doc_ir(
    variant: &syn::Ident,
    inner: &IrNode,
    ty: &syn::Type,
    hints: &[String],
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    use super::prettify::hints::extract_sep_string;

    // Pattern 1: Wrapped repetition.
    if let Some((left_lit, right_lit)) = detect_wrapped_pattern_ir(inner, ctx.ir) {
        return generate_wrapped_doc(variant, &left_lit, &right_lit, ty, hints);
    }

    // Pattern 2: Key-value pair.
    if let Some((_key_name, sep_lit)) = detect_key_value_pattern_ir(inner, ctx.ir) {
        return generate_key_value_doc_ir(variant, &sep_lit, hints, ctx.ir);
    }

    // Box<Enum> — recurse.
    if is_box_enum_type(ty) {
        let doc = quote! { val.to_doc() };
        let doc = apply_hints(doc, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    // Option — unwrap.
    if is_option_type(ty) {
        let base = quote! {
            match val {
                Some(inner) => inner.to_doc(),
                None => ::pprint::Doc::Null,
            }
        };
        let doc = apply_hints(base, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    // Tuple type — concatenation.
    if let syn::Type::Tuple(tuple) = ty {
        let n = tuple.elems.len();
        let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
        let pattern = quote! { (#(#bindings),*) };

        let parts: Vec<TokenStream> = bindings
            .iter()
            .enumerate()
            .map(|(i, binding)| {
                let elem_ty = &tuple.elems[i];
                doc_for_binding(binding, elem_ty)
            })
            .collect();

        let combined = if parts.len() == 1 {
            parts[0].clone()
        } else {
            let custom_sep = hints.iter().find_map(|h| extract_sep_string(h));

            let sep: Option<TokenStream> = if let Some(sep_str) = custom_sep {
                let has_group = hints.contains(&"group".to_string());
                if has_group {
                    let sep_lit = proc_macro2::Literal::string(sep_str);
                    let break_sep = sep_str.trim_end();
                    let break_lit = proc_macro2::Literal::string(break_sep);
                    Some(quote! {
                        ::pprint::Doc::IfBreak(
                            Box::new(
                                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                                + ::pprint::Doc::Hardline
                            ),
                            Box::new(
                                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit))
                            ),
                        )
                    })
                } else {
                    let sep_lit = proc_macro2::Literal::string(sep_str);
                    Some(quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit)) })
                }
            } else if hints.contains(&"fast".to_string())
                || hints.contains(&"hardbreak".to_string())
                || hints.contains(&"block".to_string())
            {
                Some(quote! { ::pprint::Doc::Hardline })
            } else if hints.contains(&"blankline".to_string()) {
                Some(quote! { ::pprint::Doc::Hardline + ::pprint::Doc::Hardline })
            } else if hints.contains(&"nobreak".to_string())
                || hints.contains(&"compact".to_string())
            {
                Some(quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) })
            } else if hints.contains(&"softbreak".to_string()) {
                Some(quote! { ::pprint::Doc::Softline })
            } else if hints.contains(&"off".to_string()) {
                None // Null — skip interleaving entirely.
            } else {
                // No hints: raw concatenation — no separator needed.
                None
            };

            if let Some(sep) = sep {
                // Non-Null separator — interleave parts with separator.
                let mut interleaved: Vec<proc_macro2::TokenStream> = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        interleaved.push(sep.clone());
                    }
                    interleaved.push(part.clone());
                }
                quote! { ::pprint::concat(vec![#(#interleaved),*]) }
            } else {
                // No separator — direct `+` chaining (no Null interleaving).
                // Chains into a single Concat(Vec) with one heap allocation,
                // better cache locality than DoubleDoc/TripleDoc (3 separate Boxes).
                let mut acc = parts[0].clone();
                for part in &parts[1..] {
                    acc = quote! { (#acc) + (#part) };
                }
                acc
            }
        };

        let doc = apply_hints(combined, hints);
        return quote! {
            Self::#variant(#pattern) => { #doc }
        };
    }

    quote! {
        Self::#variant(_) => {
            panic!(
                "No @pretty doc-generation strategy registered for enum variant `{}`",
                stringify!(#variant)
            )
        }
    }
}

/// Key-value doc generation from IR.
fn generate_key_value_doc_ir(
    variant: &syn::Ident,
    sep: &str,
    hints: &[String],
    ir: &GrammarIR,
) -> TokenStream {
    let sep_str = resolve_separator_literal_ir(sep, ir).unwrap_or_else(|| {
        panic!(
            "Unable to resolve key-value separator `{}` to a literal for @pretty codegen",
            sep
        )
    });
    let sep_with_space = format!("{} ", sep_str.trim());
    let base = quote! {
        {
            let (key, val) = inner;
            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(key.as_str()))
                + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_with_space))
                + val.to_doc()
        }
    };
    let doc = apply_hints(base, hints);
    quote! {
        Self::#variant(inner) => { #doc }
    }
}

// ---------------------------------------------------------------------------
// Sub-variant arms
// ---------------------------------------------------------------------------

/// Generate to_doc and source_range arms for heterogeneous alternation sub-variants.
fn generate_sub_variant_arms(
    ctx: &IrCodegenCtx<'_>,
    to_doc_arms: &mut Vec<TokenStream>,
    source_range_arms: &mut Vec<TokenStream>,
) {
    let mut seen = std::collections::HashSet::new();

    for rule in &ctx.ir.rules {
        for sv in &rule.meta.sub_variants {
            let variant_name = ctx.ir.get_string(sv.variant_name);
            if !seen.insert(variant_name.to_string()) {
                continue;
            }
            let variant = format_ident!("{}", variant_name);
            let ty = type_desc_to_syn(&sv.ty, ctx);

            if let syn::Type::Tuple(tuple_ty) = &ty {
                let n = tuple_ty.elems.len();
                let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
                let pat = quote! { (#(#bindings),*) };

                let doc_parts: Vec<_> = tuple_ty
                    .elems
                    .iter()
                    .zip(bindings.iter())
                    .map(|(elem_ty, binding)| doc_for_binding(binding, elem_ty))
                    .collect();

                to_doc_arms.push(quote! {
                    Self::#variant(#pat) => {
                        ::pprint::concat(vec![#(#doc_parts),*])
                    }
                });

                let range_parts: Vec<_> = tuple_ty
                    .elems
                    .iter()
                    .zip(bindings.iter())
                    .map(|(elem_ty, binding)| range_for_binding(binding, elem_ty))
                    .collect();

                let fold_stmts: Vec<TokenStream> = range_parts
                    .iter()
                    .map(|rp| {
                        quote! {
                            if let Some((_s, _e)) = #rp {
                                if _s < _min_s { _min_s = _s; }
                                if _e > _max_e { _max_e = _e; }
                                _found = true;
                            }
                        }
                    })
                    .collect();

                source_range_arms.push(quote! {
                    Self::#variant(#pat) => {
                        let mut _min_s = usize::MAX;
                        let mut _max_e = 0usize;
                        let mut _found = false;
                        #(#fold_stmts)*
                        if _found { Some((_min_s, _max_e)) } else { None }
                    }
                });
            } else if type_is_span(&ty) {
                to_doc_arms.push(quote! {
                    Self::#variant(s) => ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str())),
                });
                source_range_arms.push(quote! {
                    Self::#variant(s) => Some((s.start, s.end)),
                });
            } else if is_box_enum_type(&ty) {
                to_doc_arms.push(quote! {
                    Self::#variant(val) => val.to_doc(),
                });
                source_range_arms.push(quote! {
                    Self::#variant(val) => val.source_range(),
                });
            } else {
                to_doc_arms.push(quote! {
                    Self::#variant(_) => ::pprint::Doc::Null,
                });
                source_range_arms.push(quote! {
                    Self::#variant(_) => None,
                });
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Helper utilities
// ---------------------------------------------------------------------------

/// Convert a `PrettyHints` struct to a `Vec<String>` for use with the existing
/// doc generation functions.
fn pretty_hints_to_strings(ph: &PrettyHints) -> Vec<String> {
    let mut hints = Vec::new();
    if ph.group {
        hints.push("group".to_string());
    }
    if ph.indent {
        hints.push("indent".to_string());
    }
    if ph.dedent {
        hints.push("dedent".to_string());
    }
    if ph.block {
        hints.push("block".to_string());
    }
    if ph.blankline {
        hints.push("blankline".to_string());
    }
    if ph.nobreak {
        hints.push("nobreak".to_string());
    }
    if ph.softbreak {
        hints.push("softbreak".to_string());
    }
    if ph.hardbreak {
        hints.push("hardbreak".to_string());
    }
    if ph.compact {
        hints.push("compact".to_string());
    }
    if ph.fast {
        hints.push("fast".to_string());
    }
    if ph.off {
        hints.push("off".to_string());
    }
    if let Some(ref s) = ph.sep {
        hints.push(format!("sep(\"{}\")", s));
    }
    if let Some(ref s) = ph.split {
        hints.push(format!("split(\"{}\")", s));
    }
    hints
}

/// Resolve hints for a rule: explicit @pretty > heuristic inference.
fn resolve_ir_hints(rule: &IrRule, ty: &syn::Type, ctx: &IrCodegenCtx<'_>) -> Vec<String> {
    if let Some(ref ph) = rule.meta.pretty {
        let hints = pretty_hints_to_strings(ph);
        if hints.iter().any(|h| h == "off") {
            return vec!["off".to_string()];
        }
        hints
    } else {
        infer_hints_ir(rule, ty, ctx.ir, ctx)
    }
}

/// Check if an IR node tree contains nonterminal references to structured
/// (non-span) rules. References to span-producing rules don't count as
/// "structural" content — they're effectively terminal patterns.
fn contains_structured_ref_ir(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> bool {
    match node {
        IrNode::Ref(rule_id) => {
            // Check if the referenced rule produces a Span type.
            if let Some(ty) = ctx.rule_types.get(rule_id) {
                !type_is_span(ty)
            } else {
                true // Unknown type → assume structured.
            }
        }
        IrNode::Seq(children) => children.iter().any(|c| contains_structured_ref_ir(c, ctx)),
        IrNode::Alt(branches, _) => {
            branches.iter().any(|b| contains_structured_ref_ir(&b.node, ctx))
        }
        IrNode::Repeat { inner, .. } => contains_structured_ref_ir(inner, ctx),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            contains_structured_ref_ir(a, ctx) || contains_structured_ref_ir(b, ctx)
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Negate(inner) | IrNode::Map { inner, .. } => {
            contains_structured_ref_ir(inner, ctx)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}

/// Unwrap Map wrappers to get to the inner expression.
fn unwrap_ir_map(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_ir_map(inner),
        other => other,
    }
}

/// Unwrap OptionalWhitespace wrappers.
fn unwrap_ir_whitespace(node: &IrNode) -> &IrNode {
    match node {
        IrNode::OptionalWhitespace(inner) => unwrap_ir_whitespace(inner),
        other => other,
    }
}
