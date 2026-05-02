//! AZ-IV.W5 T4 — codegen-emitted production [`StructRegistry`] const.
//!
//! Per AUDIT-F transposition T4: the proc-macro `path!` validates path
//! segments against the per-grammar [`StructRegistry`]. Until W5 the macro
//! consumed a hand-authored synthetic fixture in
//! `crates/bbnf-path/src/registry.rs`; AUDIT-F mid-tranche §R1 names the
//! fixture as substrate-without-real-consumer in miniature. T4 routes the
//! macro through the production registry projected by `cargo xtask regen`.
//!
//! The registry's runtime shape (`BTreeMap<RuleId, StructLayout>` carrying
//! `String` field names) is not `const`-compatible in the strict sense; the
//! emission below produces a `LazyLock<StructRegistry>` static that
//! materialises the registry once at first access and reads byte-identical
//! to the registry the IR pipeline projected. The `LazyLock` lives at the
//! per-grammar generated module under the canonical name `REGISTRY`.
//!
//! `bbnf-path` reaches the static through a per-marker resolver that
//! returns `&'static StructRegistry` after dereferencing the lock; the
//! macro's lex / lower / validate stages stay untouched — only the source
//! of truth swaps.
//!
//! Emission walks the registry once and produces a constructor expression
//! per layout. Field names are `String::from(literal)` calls; type
//! descriptors recurse into themselves. The walk is grammar-general — no
//! rule-name pattern matching anywhere on the codegen surface.

use bbnf_ir::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Top-level entry. Emits the `pub static REGISTRY: LazyLock<StructRegistry>`
/// scoped to the per-grammar emit-impl module. Consumers reach the
/// dereferenced `&StructRegistry` via `&*REGISTRY` or
/// `REGISTRY.layout(...)` directly (the `Deref` impl on `LazyLock` makes
/// the registry's accessor surface available without explicit deref).
///
/// The `entry_rule` const carries the entry rule name extracted from the
/// IR's rule list — `bbnf-path` resolves the path's anchor through this
/// const so the macro and the runtime agree on the document root rule.
pub fn emit_registry(ir: &GrammarIR) -> TokenStream {
    let layout_constructors: Vec<TokenStream> = ir
        .struct_registry
        .iter()
        .map(emit_layout_constructor)
        .collect();

    // AZ-IV.W5 T4 — entry rule is the IR's `ir.entry` (the grammar's
    // declared start rule), NOT "first non-transparent rule." The two
    // diverge whenever the start rule is itself transparent or appears
    // mid-list after removed-left-recursion / inlining shuffles. The
    // sidecar writer at `xtask::regen::write_registry_sidecar` mirrors
    // this resolution exactly so the codegen const + the sidecar agree.
    let entry_rule_lit = ir
        .rules
        .iter()
        .find(|r| r.id == ir.entry)
        .map(|r| ir.get_string(r.name).to_string())
        .unwrap_or_default();

    quote! {
        /// AZ-IV.W5 T4 — codegen-emitted production registry.
        ///
        /// `LazyLock<StructRegistry>` projected from the IR's
        /// `project_types` closure at `cargo xtask regen` time. The
        /// `bbnf-path` proc-macro consumes this static through the
        /// per-marker resolver in `crates/bbnf-path/src/registry.rs`; the
        /// runtime audit pass + emitter consume the same registry shape
        /// from `GrammarIR::struct_registry`.
        ///
        /// `Deref` on `LazyLock` makes the registry's accessor surface
        /// available without explicit deref — `REGISTRY.layout(rule_id)`
        /// reads through the deref impl directly.
        pub static REGISTRY: ::std::sync::LazyLock<
            ::bbnf_ir::registry::StructRegistry,
        > = ::std::sync::LazyLock::new(|| {
            let mut __registry = ::bbnf_ir::registry::StructRegistry::new();
            #(__registry.insert(#layout_constructors);)*
            __registry
        });

        /// AZ-IV.W5 T4 — entry-rule name for the per-grammar registry.
        ///
        /// `bbnf-path` resolves the document-root rule through this const;
        /// the value is the first non-transparent rule's name (typically
        /// the start production of the grammar's top-level production).
        pub const REGISTRY_ENTRY_RULE: &str = #entry_rule_lit;
    }
}

/// Build a [`StructLayout`] constructor expression. Recurses into nested
/// types and field provenance.
fn emit_layout_constructor(layout: &StructLayout) -> TokenStream {
    let rule_id = layout.rule_id;
    let rule_name = layout.rule_name.as_str();
    let kind = emit_layout_kind(layout.kind);
    let rule_type = emit_type_desc(&layout.rule_type);
    let fields: Vec<TokenStream> = layout.fields.iter().map(emit_field).collect();

    quote! {
        ::bbnf_ir::registry::StructLayout {
            rule_id: #rule_id,
            rule_name: ::std::string::String::from(#rule_name),
            kind: #kind,
            rule_type: #rule_type,
            fields: ::std::vec![#(#fields),*],
        }
    }
}

fn emit_layout_kind(kind: LayoutKind) -> TokenStream {
    let variant = match kind {
        LayoutKind::Struct => format_ident!("Struct"),
        LayoutKind::TaggedEnum => format_ident!("TaggedEnum"),
        LayoutKind::UntaggedEnum => format_ident!("UntaggedEnum"),
        LayoutKind::NewtypeWrapper => format_ident!("NewtypeWrapper"),
    };
    quote! { ::bbnf_ir::registry::LayoutKind::#variant }
}

fn emit_field(field: &StructField) -> TokenStream {
    let name = field.name.as_str();
    let type_desc = emit_type_desc(&field.type_desc);
    let source = emit_field_source(field.source);
    quote! {
        ::bbnf_ir::registry::StructField {
            name: ::std::string::String::from(#name),
            type_desc: #type_desc,
            source: #source,
        }
    }
}

fn emit_field_source(source: FieldSource) -> TokenStream {
    match source {
        FieldSource::TypedLeaf => quote! {
            ::bbnf_ir::registry::FieldSource::TypedLeaf
        },
        FieldSource::BranchTag { branch_index } => quote! {
            ::bbnf_ir::registry::FieldSource::BranchTag {
                branch_index: #branch_index,
            }
        },
        FieldSource::SeqPosition { position } => quote! {
            ::bbnf_ir::registry::FieldSource::SeqPosition {
                position: #position,
            }
        },
        FieldSource::RepeatElement => quote! {
            ::bbnf_ir::registry::FieldSource::RepeatElement
        },
        FieldSource::RuleReference { target_rule } => quote! {
            ::bbnf_ir::registry::FieldSource::RuleReference {
                target_rule: #target_rule,
            }
        },
    }
}

fn emit_type_desc(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::Span => quote! { ::bbnf_ir::TypeDesc::Span },
        TypeDesc::F64 => quote! { ::bbnf_ir::TypeDesc::F64 },
        TypeDesc::Bool => quote! { ::bbnf_ir::TypeDesc::Bool },
        TypeDesc::I8 => quote! { ::bbnf_ir::TypeDesc::I8 },
        TypeDesc::U8 => quote! { ::bbnf_ir::TypeDesc::U8 },
        TypeDesc::I16 => quote! { ::bbnf_ir::TypeDesc::I16 },
        TypeDesc::U16 => quote! { ::bbnf_ir::TypeDesc::U16 },
        TypeDesc::I32 => quote! { ::bbnf_ir::TypeDesc::I32 },
        TypeDesc::U32 => quote! { ::bbnf_ir::TypeDesc::U32 },
        TypeDesc::I64 => quote! { ::bbnf_ir::TypeDesc::I64 },
        TypeDesc::U64 => quote! { ::bbnf_ir::TypeDesc::U64 },
        TypeDesc::BoxedEnum => quote! { ::bbnf_ir::TypeDesc::BoxedEnum },
        TypeDesc::Enum => quote! { ::bbnf_ir::TypeDesc::Enum },
        TypeDesc::Option(inner) => {
            let inner_ts = emit_type_desc(inner);
            quote! {
                ::bbnf_ir::TypeDesc::Option(::std::boxed::Box::new(#inner_ts))
            }
        }
        TypeDesc::Vec(inner) => {
            let inner_ts = emit_type_desc(inner);
            quote! {
                ::bbnf_ir::TypeDesc::Vec(::std::boxed::Box::new(#inner_ts))
            }
        }
        TypeDesc::Tuple(items) => {
            let item_ts: Vec<TokenStream> = items.iter().map(emit_type_desc).collect();
            quote! {
                ::bbnf_ir::TypeDesc::Tuple(::std::vec![#(#item_ts),*])
            }
        }
        TypeDesc::Named(string_id) => {
            quote! {
                ::bbnf_ir::TypeDesc::Named(#string_id)
            }
        }
        TypeDesc::HeterogeneousAltJoin(branches) => {
            let branch_ts: Vec<TokenStream> = branches.iter().map(emit_type_desc).collect();
            quote! {
                ::bbnf_ir::TypeDesc::HeterogeneousAltJoin(
                    ::std::vec![#(#branch_ts),*]
                )
            }
        }
    }
}
