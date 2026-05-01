//! `CstSchema::from_ir()` — derive a CstSchema from a fully-lowered GrammarIR.
//!
//! Walks the IR rule set once and produces a self-contained schema:
//!
//! 1. Iterate non-transparent rules → `VariantDescriptor` for each.
//! 2. Iterate `RuleMeta::sub_variants` (deduped by name) → sub-variant entries.
//! 3. Append `Recovered` (if any `@recover` directive present + !skip_recover)
//!    and the trailing `__Phantom`.
//!
//! Field roles are inferred from `TypeDesc` shape and from the rule body's
//! `IrNode` structure (for top-level keyword/punctuation detection on
//! `Tuple` payloads). Identifier-carrier detection looks for `Ref(target)`
//! where `target` is the `identifier` rule.

use std::collections::HashSet;

use bbnf_ir::{GrammarIR, IrNode, RuleId, TypeDesc};

use super::model::*;

impl CstSchema {
    /// Build a `CstSchema` from a fully-lowered `GrammarIR`.
    ///
    /// `enum_name` is the target enum identifier — historically
    /// `BbnfBootstrapEnum` under the eager-AST emitter, now the
    /// per-rule `<Rule>View` family under the tape-first AC.2
    /// emitter. The schema is agnostic: it only names the target.
    /// `has_recovered` indicates whether the backend will emit a
    /// `Recovered` variant (any `@recover` directive present and
    /// `skip_recover` is off).
    pub fn from_ir(ir: &GrammarIR, enum_name: String, has_recovered: bool) -> Self {
        let directive_rules = discover_directive_rules(ir);
        let transparent_wrappers = discover_transparent_wrappers(ir);
        let identifier_rule_id = ir
            .rules
            .iter()
            .find(|r| ir.get_string(r.name) == "identifier")
            .map(|r| r.id);

        let mut variants = Vec::new();

        // ── Rule variants (skip transparent) ────────────────────────────
        for rule in &ir.rules {
            if rule.meta.is_transparent {
                continue;
            }
            let name = ir.get_string(rule.name).to_string();
            let type_desc = ir
                .types
                .iter()
                .find(|(rid, _)| *rid == rule.id)
                .map(|(_, td)| td.clone());

            let category = categorize_rule(&name, rule.id, type_desc.as_ref(), &directive_rules);

            let fields = type_desc
                .as_ref()
                .map(|td| derive_fields_from_type(td, Some(&rule.body), identifier_rule_id, ir))
                .unwrap_or_default();

            variants.push(VariantDescriptor {
                name,
                rule_id: Some(rule.id),
                category,
                type_desc,
                fields,
            });
        }

        // ── Sub-variants (heterogeneous Alt branches), deduped by name ──
        let mut seen_sub: HashSet<String> = HashSet::new();
        for rule in &ir.rules {
            for sv in &rule.meta.sub_variants {
                let sv_name = ir.get_string(sv.variant_name).to_string();
                if !seen_sub.insert(sv_name.clone()) {
                    continue;
                }
                let fields = derive_fields_from_type(&sv.ty, None, identifier_rule_id, ir);
                variants.push(VariantDescriptor {
                    name: sv_name,
                    rule_id: None,
                    category: VariantCategory::Composite,
                    type_desc: Some(sv.ty.clone()),
                    fields,
                });
            }
        }

        // ── Recovered + __Phantom ───────────────────────────────────────
        if has_recovered {
            variants.push(VariantDescriptor {
                name: "Recovered".to_string(),
                rule_id: None,
                category: VariantCategory::Recovered,
                type_desc: None,
                fields: Vec::new(),
            });
        }

        variants.push(VariantDescriptor {
            name: "__Phantom".to_string(),
            rule_id: None,
            category: VariantCategory::Phantom,
            type_desc: None,
            fields: Vec::new(),
        });

        Self {
            enum_name,
            variants,
            directive_rules,
            transparent_wrappers,
            has_recovered,
        }
    }
}

// ─── Discovery passes ────────────────────────────────────────────────────────

/// Discover directive rules by walking from the rule literally named `directive`.
fn discover_directive_rules(ir: &GrammarIR) -> HashSet<RuleId> {
    let mut refs = HashSet::new();
    if let Some(directive_rule) = ir
        .rules
        .iter()
        .find(|r| ir.get_string(r.name) == "directive")
    {
        collect_refs(&directive_rule.body, &mut refs);
    }
    refs
}

/// Discover transparent wrappers — rules whose body is a single `Ref`.
fn discover_transparent_wrappers(ir: &GrammarIR) -> HashSet<RuleId> {
    ir.rules
        .iter()
        .filter(|r| matches!(&r.body, IrNode::Ref(_)) || r.meta.is_transparent)
        .map(|r| r.id)
        .collect()
}

/// Recursively collect all `Ref(rule_id)` targets reachable from a node.
fn collect_refs(node: &IrNode, out: &mut HashSet<RuleId>) {
    match node {
        IrNode::Ref(rid) => {
            out.insert(*rid);
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_refs(&branch.node, out);
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                collect_refs(child, out);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => collect_refs(inner, out),
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            collect_refs(l, out);
            collect_refs(r, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_refs(token, out);
            for arm in arms {
                collect_refs(&arm.continuation, out);
            }
            collect_refs(fallback, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

// ─── Categorization ──────────────────────────────────────────────────────────

fn categorize_rule(
    name: &str,
    rule_id: RuleId,
    type_desc: Option<&TypeDesc>,
    directive_rules: &HashSet<RuleId>,
) -> VariantCategory {
    if directive_rules.contains(&rule_id) {
        return VariantCategory::Directive(directive_kind_from_name(name));
    }
    match type_desc {
        Some(TypeDesc::Span) => VariantCategory::Terminal,
        _ => VariantCategory::Composite,
    }
}

fn directive_kind_from_name(name: &str) -> DirectiveKind {
    match name {
        "import_directive" => DirectiveKind::Import,
        "recover_directive" => DirectiveKind::Recover,
        "pretty_directive" => DirectiveKind::Pretty,
        "ws_directive" => DirectiveKind::Ws,
        "token_directive" => DirectiveKind::Token,
        "debug_directive" => DirectiveKind::Debug,
        "host_directive" => DirectiveKind::Host,
        other => DirectiveKind::Other(other.to_string()),
    }
}

// ─── Field role derivation ───────────────────────────────────────────────────

/// Decompose a payload `TypeDesc` into top-level `FieldDescriptor`s.
///
/// `body_hint` is the rule's `IrNode` body, used to refine roles when the
/// type alone is ambiguous (e.g., distinguishing keywords from spans).
fn derive_fields_from_type(
    type_desc: &TypeDesc,
    body_hint: Option<&IrNode>,
    identifier_rule_id: Option<RuleId>,
    ir: &GrammarIR,
) -> Vec<FieldDescriptor> {
    match type_desc {
        TypeDesc::Tuple(elems) => {
            // Try to align tuple positions with `Seq` children of the body
            // for keyword/punctuation/identifier detection.
            let body_children = body_hint.and_then(seq_children);
            elems
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let body_node = body_children.and_then(|cs| cs.get(i));
                    FieldDescriptor {
                        index: i,
                        type_desc: elem.clone(),
                        role: derive_field_role(elem, body_node, identifier_rule_id, ir),
                    }
                })
                .collect()
        }
        // Single-payload variants: one field at index 0.
        _ => vec![FieldDescriptor {
            index: 0,
            type_desc: type_desc.clone(),
            role: derive_field_role(type_desc, body_hint, identifier_rule_id, ir),
        }],
    }
}

/// If a node is a `Seq`, return its children. Otherwise `None`.
fn seq_children(node: &IrNode) -> Option<&[IrNode]> {
    if let IrNode::Seq(children) = node {
        Some(children)
    } else {
        None
    }
}

/// Derive a `FieldRole` from the field's type and (optionally) the matching
/// IR body node.
///
/// Heuristics (in order):
/// 1. `Span` field whose body node is a `Literal` matching `[a-zA-Z]+`/`@\w+`
///    → `Keyword`.
/// 2. `Span` field whose body node is a `Literal` of a punctuation char
///    → `Punctuation`.
/// 3. `Span` field whose body node is `Ref(identifier_rule)` → `IdentifierCarrier`.
/// 4. `Span` field with no body hint → `Annotation`.
/// 5. `BoxedEnum` / `Enum` → `PrimaryChild` (or `IdentifierCarrier` if body
///    points at the identifier rule).
/// 6. `Option<...>` → `OptionalChild`.
/// 7. `Vec<...>` → `RepeatedChildren`.
/// 8. Others → `Annotation`.
fn unwrap_body_node(n: &IrNode) -> &IrNode {
    match n {
        IrNode::Repeat { inner, .. } => inner,
        IrNode::Map { inner, .. } => inner,
        IrNode::OptionalWhitespace(inner) => inner,
        other => other,
    }
}

fn derive_field_role(
    td: &TypeDesc,
    body_node: Option<&IrNode>,
    identifier_rule_id: Option<RuleId>,
    ir: &GrammarIR,
) -> FieldRole {
    match td {
        TypeDesc::Span => {
            if let Some(body) = body_node.map(unwrap_body_node) {
                match body {
                    IrNode::Literal(sid) => {
                        let s = ir.get_string(*sid);
                        if is_keyword_literal(s) {
                            FieldRole::Keyword
                        } else if is_punctuation_literal(s) {
                            FieldRole::Punctuation
                        } else {
                            FieldRole::Annotation
                        }
                    }
                    IrNode::Ref(rid) if Some(*rid) == identifier_rule_id => {
                        FieldRole::IdentifierCarrier
                    }
                    _ => FieldRole::Annotation,
                }
            } else {
                FieldRole::Annotation
            }
        }
        TypeDesc::BoxedEnum | TypeDesc::Enum | TypeDesc::HeterogeneousAltJoin(_) => {
            // AZ-III.W3a.3 — `HeterogeneousAltJoin` is the named-obligation
            // counterpart to `BoxedEnum`; both share the boxed-enum codegen
            // layout for downstream consumers.
            if let Some(IrNode::Ref(rid)) = body_node.map(unwrap_body_node) {
                if Some(*rid) == identifier_rule_id {
                    return FieldRole::IdentifierCarrier;
                }
            }
            FieldRole::PrimaryChild
        }
        TypeDesc::Option(_) => FieldRole::OptionalChild,
        TypeDesc::Vec(_) => FieldRole::RepeatedChildren,
        TypeDesc::Tuple(_) => FieldRole::PrimaryChild,
        TypeDesc::F64
        | TypeDesc::Bool
        | TypeDesc::I8
        | TypeDesc::U8
        | TypeDesc::I16
        | TypeDesc::U16
        | TypeDesc::I32
        | TypeDesc::U32
        | TypeDesc::I64
        | TypeDesc::U64
        | TypeDesc::Named(_) => FieldRole::Annotation,
    }
}

fn is_keyword_literal(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let bytes = s.as_bytes();
    // `@directive` style.
    if bytes[0] == b'@'
        && bytes[1..]
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_')
    {
        return true;
    }
    // Bare alphabetic keyword.
    bytes.iter().all(|b| b.is_ascii_alphabetic() || *b == b'_') && bytes.len() > 1
}

fn is_punctuation_literal(s: &str) -> bool {
    matches!(
        s,
        "=" | ";"
            | ","
            | "("
            | ")"
            | "{"
            | "}"
            | "["
            | "]"
            | "|"
            | ":"
            | "->"
            | "=>"
            | "::"
            | "<<"
            | ">>"
    )
}
