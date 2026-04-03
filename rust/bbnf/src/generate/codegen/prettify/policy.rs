//! Pretty-policy planning for monolithic prettify codegen.

use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleId};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::ir_types::IrCodegenCtx;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum WrapperPolicy {
    None,
    Group,
    GroupIndent,
    Block,
    BlockIndent,
    Off,
}

impl Default for WrapperPolicy {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum SeparatorPolicy {
    None,
    Space,
    Softline,
    Hardline,
    Blankline,
    Custom(String),
}

impl Default for SeparatorPolicy {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Clone, Debug, Default)]
pub(super) struct PrettyPolicy {
    pub wrapper: WrapperPolicy,
    pub separator: SeparatorPolicy,
    pub is_ws_rule: bool,
    pub split: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub(super) struct PrettyRulePlan {
    pub inline: bool,
    pub policy: PrettyPolicy,
}

#[derive(Clone, Copy)]
pub(super) struct PrettifyCtx<'a> {
    pub ir: &'a GrammarIR,
    pub ctx: &'a IrCodegenCtx<'a>,
    pub plans: &'a [PrettyRulePlan],
}

impl<'a> PrettifyCtx<'a> {
    #[inline(always)]
    pub fn plan(&self, rule_id: RuleId) -> &PrettyRulePlan {
        &self.plans[rule_id as usize]
    }
}

pub(super) fn build_rule_plans(ir: &GrammarIR) -> Vec<PrettyRulePlan> {
    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    ir.rules
        .iter()
        .enumerate()
        .map(|(idx, rule)| {
            let policy = PrettyPolicy::from_rule(rule, ir);
            let inline = should_inline_rule(rule, &policy, ref_counts[idx]);
            PrettyRulePlan { inline, policy }
        })
        .collect()
}

impl PrettyPolicy {
    pub(super) fn from_rule(rule: &IrRule, ir: &GrammarIR) -> Self {
        let hints = rule.meta.directives.pretty.as_ref();
        let wrapper = match hints {
            Some(h) if h.off => WrapperPolicy::Off,
            Some(h) if h.block && h.indent => WrapperPolicy::BlockIndent,
            Some(h) if h.block => WrapperPolicy::Block,
            Some(h) if h.group && h.indent => WrapperPolicy::GroupIndent,
            Some(h) if h.group => WrapperPolicy::Group,
            _ => WrapperPolicy::None,
        };

        let separator = match hints {
            Some(h) if h.split.is_some() => SeparatorPolicy::None,
            Some(h) if h.blankline => SeparatorPolicy::Blankline,
            Some(h) if h.block || h.hardbreak || h.fast => SeparatorPolicy::Hardline,
            Some(h) if h.nobreak => SeparatorPolicy::Space,
            Some(h) if h.compact || h.off => SeparatorPolicy::None,
            Some(h) if h.softbreak => SeparatorPolicy::Softline,
            Some(h) => h
                .sep
                .as_ref()
                .map(|sep| SeparatorPolicy::Custom(sep.clone()))
                .unwrap_or(SeparatorPolicy::None),
            None => SeparatorPolicy::None,
        };

        let is_ws_rule = if let (IrNode::Regex(body_sid), Some(ws_sid)) = (&rule.body, ir.ws_pattern)
        {
            *body_sid == ws_sid
        } else {
            false
        };

        let split = hints.and_then(|h| h.split.clone());

        Self {
            wrapper,
            separator,
            is_ws_rule,
            split,
        }
    }
}

pub(super) fn emit_rule_wrapper(policy: &PrettyPolicy, body: TokenStream) -> TokenStream {
    match policy.wrapper {
        WrapperPolicy::GroupIndent => quote! {
            __builder.group_open();
            __builder.indent_open();
            let __pretty_ok = { #body };
            __builder.indent_close();
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::Group => quote! {
            __builder.group_open();
            let __pretty_ok = { #body };
            __builder.group_close();
            __pretty_ok
        },
        WrapperPolicy::BlockIndent => quote! {
            __builder.indent_open();
            __builder.hardline();
            let __pretty_ok = { #body };
            __builder.indent_close();
            __builder.hardline();
            __pretty_ok
        },
        WrapperPolicy::Block => quote! { #body },
        WrapperPolicy::Off | WrapperPolicy::None => quote! { #body },
    }
}

pub(super) fn emit_separator(policy: &PrettyPolicy) -> TokenStream {
    match &policy.separator {
        SeparatorPolicy::None => quote! {},
        SeparatorPolicy::Space => quote! { __builder.text(" "); },
        SeparatorPolicy::Softline => quote! { __builder.softline(); },
        SeparatorPolicy::Hardline => quote! { __builder.hardline(); },
        SeparatorPolicy::Blankline => quote! {
            __builder.hardline();
            __builder.hardline();
        },
        SeparatorPolicy::Custom(sep_str) => {
            let sep_lit = proc_macro2::Literal::string(sep_str);
            // Sep with empty brk: flat → emit flat bytes, break → newline.
            // Single op instead of IfBreak + Hardline + Text (3 ops).
            quote! { __builder.sep(#sep_lit, ""); }
        }
    }
}

pub(super) fn emit_whitespace_segment(start_var: &syn::Ident) -> TokenStream {
    quote! {
        __builder.text_inline_ws(&state.src[#start_var..state.offset]);
    }
}

pub(super) fn emit_ws_rule_body(body_expr: TokenStream, mctx: &mut super::super::MonoCtx) -> TokenStream {
    let ws_cp = mctx.fresh("ws_cp");
    // Light checkpoint: ws rule body never opens/closes groups.
    quote! {{
        let #ws_cp = __builder.light_checkpoint();
        let __ok = (|| -> bool { #body_expr; true })();
        __builder.light_restore(#ws_cp);
        if !__ok {
            return false;
        }
        true
    }}
}

pub(super) fn split_compile_error(rule_name: &str, split: &str) -> TokenStream {
    let msg = format!(
        "`split(\"{split}\")` is not supported by prettify codegen yet on rule `{rule_name}`"
    );
    syn::Error::new(proc_macro2::Span::call_site(), msg).to_compile_error()
}

fn should_inline_rule(rule: &IrRule, policy: &PrettyPolicy, ref_count: u32) -> bool {
    if policy.split.is_some() {
        return false;
    }
    if policy.is_ws_rule {
        return false;
    }
    if rule.meta.is_cyclic || rule.meta.directives.recover.is_some() {
        return false;
    }
    let cost = estimate_prettify_cost(&rule.body);
    // Always inline tiny rules (cost ≤ 3) even when referenced multiple times.
    // This covers patterns like `comma = "," ?w` and `colon = ":" ?w` which
    // are called thousands of times and benefit from avoiding function overhead.
    if ref_count > 1 && cost > 3 {
        return false;
    }

    cost <= 6 && max_alt_branches(&rule.body) <= 2
}

fn max_alt_branches(node: &IrNode) -> usize {
    match node {
        IrNode::Alt(branches, _) => {
            let inner_max = branches
                .iter()
                .map(|b| max_alt_branches(&b.node))
                .max()
                .unwrap_or(0);
            branches.len().max(inner_max)
        }
        IrNode::Seq(children) => children.iter().map(max_alt_branches).max().unwrap_or(0),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => max_alt_branches(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            max_alt_branches(a).max(max_alt_branches(b))
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let arm_max = arms
                .iter()
                .map(|arm| max_alt_branches(&arm.continuation))
                .max()
                .unwrap_or(0);
            max_alt_branches(token).max(arm_max).max(max_alt_branches(fallback))
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 0,
    }
}

fn estimate_prettify_cost(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => 1,
        IrNode::Ref(_) => 6,
        IrNode::Seq(children) => 1 + children.iter().map(estimate_prettify_cost).sum::<usize>(),
        IrNode::Alt(branches, _) => {
            4 + branches
                .iter()
                .map(|b| estimate_prettify_cost(&b.node))
                .sum::<usize>()
        }
        IrNode::Repeat { inner, .. } => 7 + estimate_prettify_cost(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            estimate_prettify_cost(a) + estimate_prettify_cost(b)
        }
        IrNode::Negate(inner) => 2 + estimate_prettify_cost(inner),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            1 + estimate_prettify_cost(inner)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            5 + estimate_prettify_cost(token)
                + arms
                    .iter()
                    .map(|arm| estimate_prettify_cost(&arm.continuation))
                    .sum::<usize>()
                + estimate_prettify_cost(fallback)
        }
    }
}

fn count_refs_vec(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(c) = counts.get_mut(*id as usize) {
                *c += 1;
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                count_refs_vec(child, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                count_refs_vec(&branch.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs_vec(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs_vec(a, counts);
            count_refs_vec(b, counts);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_refs_vec(token, counts);
            for arm in arms {
                count_refs_vec(&arm.continuation, counts);
            }
            count_refs_vec(fallback, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
