//! Tape-backed directive value structs + extraction helpers.
//!
//! Emits:
//!
//! - `pub mod cst_directives { ... }` — one `#[derive(Clone, Copy)]`
//!   struct per directive variant. Struct fields are tape-native:
//!   pre-sliced `&'p str` for `Identifier` / `TextSpan`, raw span
//!   tuples for `SpanRaw`, and `TapeCursor<'p>` for compound
//!   (`Inner` / `OptionInner` / `Slice`) slots so callers can
//!   construct whatever typed view they need over the referenced
//!   record without the schema layer having to name the child
//!   rule's view type.
//! - Inside the same module, per-directive free helper functions
//!   `try_as_<rule>(cursor, input) -> Option<DirectiveStruct<'p>>`.
//!   These do all the field extraction work: match on
//!   `variant_idx`, read the compound rule's children in positional
//!   order, slice `input` for identifier / text-span fields, and
//!   build the directive's `span` from the keyword + terminator
//!   sub-records.
//!
//! Callers walking the grammar top-level iterate children manually
//! (after peeling `grammar_item` / `directive` wrappers) and dispatch
//! on `cursor.rule_kind()` before calling
//! `cst_directives::try_as_<rule>(cursor, input)` directly. There is
//! no view-anchored walking accessor: such an accessor would scan a
//! parent's children looking for the directive variant, which is
//! the wrong shape once the parent has been peeled to the directive
//! itself.
//!
//! The layout table (`DirectiveKind` → slot list) is preserved from
//! the pre-AC.2 emitter; only the extraction codegen is rewritten
//! to walk the tape instead of pattern-matching enum variants.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::model::{CstSchema, DirectiveKind, VariantCategory};
use super::shared::variant_idx_for;

/// Emit the `cst_directives` module containing directive structs +
/// their extraction helpers. Returns an empty `TokenStream` when the
/// schema declares no directive variants.
pub(super) fn generate_module(schema: &CstSchema) -> TokenStream {
    let mut structs = Vec::new();
    let mut helpers = Vec::new();
    let node_view_ident = format_ident!("{}NodeView", schema.enum_name);

    for variant in &schema.variants {
        let VariantCategory::Directive(ref kind) = variant.category else {
            continue;
        };
        let Some(layout) = directive_field_layout(kind) else {
            continue;
        };
        let Some(variant_idx) = variant_idx_for(schema, &variant.name) else {
            continue;
        };
        // `variant.type_desc` is consulted only as a heuristic: the
        // structural-mode pipeline preserves more wrappers (Optional
        // placeholders, comment slots) so the projected Tuple can
        // come out longer than the layout's slot count. The generated
        // `try_as_*_directive` helper walks the runtime tape via
        // `cursor.children()` and binds slots positionally from the
        // front, so an over-long Tuple is harmless — extra children
        // are simply not bound. Any non-empty payload is acceptable,
        // including the case where `type_desc` is missing entirely
        // (some directives lower to Unit because their body inlines
        // all literals).
        let _ = &variant.type_desc;

        let struct_ident = format_ident!("{}", layout.struct_name);
        let mut fields = Vec::new();
        for slot in layout.slots {
            let name = format_ident!("{}", slot.name);
            let ty: TokenStream = match slot.kind {
                DirectiveFieldKind::Identifier | DirectiveFieldKind::TextSpan => {
                    quote! { &'p str }
                }
                DirectiveFieldKind::SpanRaw => quote! { (u32, u32) },
                DirectiveFieldKind::Inner | DirectiveFieldKind::Slice => {
                    quote! { super::#node_view_ident<'p> }
                }
                DirectiveFieldKind::OptionInner => quote! {
                    ::core::option::Option<super::#node_view_ident<'p>>
                },
            };
            fields.push(quote! { pub #name: #ty });
        }
        // Full directive source span as a `parse_that::Span<'p>`
        // anchored to the owning `Parsed`'s input buffer. Consumers
        // that held the enum-era `parse_that::Span<'a>` field can
        // drop this in place; callers that want `(lo, hi)` byte
        // offsets call `.start` / `.end` on the Span.
        fields.push(quote! { pub span: ::parse_that::Span<'p> });

        structs.push(quote! {
            #[derive(Clone, Copy)]
            pub struct #struct_ident<'p> {
                #(#fields),*
            }
        });

        let helper = generate_helper(&struct_ident, &variant.name, variant_idx, layout, &node_view_ident);
        helpers.push(helper);
    }

    if structs.is_empty() {
        return TokenStream::new();
    }

    quote! {
        /// Schema-emitted directive value structs. Each is a thin
        /// typed projection over a tape record; the source span is
        /// always exposed via `.span`. Compound slots hand back raw
        /// `TapeCursor<'p>` handles so callers can construct
        /// whatever typed view they need without this module having
        /// to enumerate the target rule's view type.
        #[allow(dead_code, non_snake_case)]
        pub mod cst_directives {
            #(#structs)*

            #(#helpers)*
        }
    }
}

/// Emit the `try_as_<rule>(cursor, input)` helper that checks the
/// cursor's variant_idx and — if it matches — extracts the
/// directive struct by reading positional children in order.
///
/// Extraction convention (inherited from the pre-AC.2 layout):
///
/// - child[0] is the leading keyword span (`__kw`),
/// - child[last] is the trailing terminator span (`__term`),
/// - children `[1..len-1]` map positionally to `layout.slots`.
///
/// The returned struct's `span` is `(kw_lo, term_hi)`, matching the
/// enum-era `Span::new(__kw.start, __term.end, __kw.src)` shape.
fn generate_helper(
    struct_ident: &syn::Ident,
    variant_name: &str,
    variant_idx: u8,
    layout: &DirectiveLayout,
    node_view_ident: &syn::Ident,
) -> TokenStream {
    let try_ident = format_ident!("try_as_{}", variant_name);
    let idx_lit = variant_idx;

    // Build the list of positional child binds + the slot
    // assignments that consume them.
    //
    // The helper walks `cursor.children()` ONCE, storing each
    // child in a sequential local, so the whole scan is O(n) in
    // the compound's child count — `TapeCursor::child(i)` is
    // O(i), so repeated `child(k)` calls would be quadratic.
    let mut slot_assignments = Vec::new();
    let mut slot_binds = Vec::new();

    for (i, slot) in layout.slots.iter().enumerate() {
        let child_ident = format_ident!("__slot_{}", i);
        slot_binds.push(quote! {
            let #child_ident = __children.next()?;
        });

        let field_ident = format_ident!("{}", slot.name);
        let value: TokenStream = match slot.kind {
            DirectiveFieldKind::Identifier => quote! {
                super::cst_identifier_text(#child_ident, input)
            },
            DirectiveFieldKind::TextSpan => quote! {
                {
                    let (__lo, __hi) = #child_ident.span();
                    &input[__lo as usize..__hi as usize]
                }
            },
            DirectiveFieldKind::SpanRaw => quote! {
                #child_ident.span()
            },
            DirectiveFieldKind::Inner | DirectiveFieldKind::Slice => quote! {
                super::#node_view_ident::from_cursor(#child_ident, input)
            },
            DirectiveFieldKind::OptionInner => quote! {
                // OptionInner slots wrap their child in a single-
                // arm Alt/Optional record at the tape level. When
                // the optional is present, the compound carries
                // one grandchild cursor; when absent, zero
                // children. `next()` over the children iterator
                // yields `Some(child)` or `None`, wrapped into a
                // `NodeView` so consumers carry the input borrow.
                #child_ident
                    .children()
                    .next()
                    .map(|c| super::#node_view_ident::from_cursor(c, input))
            },
        };
        slot_assignments.push(quote! { #field_ident: #value });
    }

    quote! {
        /// Schema-emitted extraction helper. Returns the typed
        /// directive struct if `cursor` points at a record whose
        /// `variant_idx` matches this directive rule's codegen-
        /// assigned discriminator; `None` otherwise.
        #[inline]
        #[allow(dead_code)]
        pub fn #try_ident<'p>(
            cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
            input: &'p str,
        ) -> ::core::option::Option<#struct_ident<'p>> {
            if cursor.variant_idx() != #idx_lit {
                return ::core::option::Option::None;
            }
            // Positional child layout: [kw, slots..., terminator].
            // Walk once; each `next()` call is O(1) over the tape
            // iterator.
            let mut __children = cursor.children();
            let __kw = __children.next()?;
            #(#slot_binds)*
            let __term = __children.next()?;
            let (__kw_lo, _) = __kw.span();
            let (_, __term_hi) = __term.span();
            ::core::option::Option::Some(#struct_ident {
                #(#slot_assignments,)*
                span: ::parse_that::Span::new(__kw_lo as usize, __term_hi as usize, input),
            })
        }
    }
}

// ─── Directive layout table ──────────────────────────────────────────────────

/// Layout for a directive variant's semantic fields. Driven by `DirectiveKind`.
struct DirectiveLayout {
    struct_name: &'static str,
    slots: &'static [DirectiveSlot],
}

struct DirectiveSlot {
    name: &'static str,
    kind: DirectiveFieldKind,
}

#[derive(Clone, Copy)]
enum DirectiveFieldKind {
    /// Rule child that carries an identifier — extract via
    /// `cst_identifier_text(cursor, input)` into `&'p str`.
    Identifier,
    /// Rule child whose full span text is the slot's value — extract
    /// via `input[lo..hi]` into `&'p str`.
    TextSpan,
    /// Raw `(lo, hi)` span tuple of the rule child.
    SpanRaw,
    /// Rule child as a raw `TapeCursor<'p>`; callers wrap it in the
    /// appropriate view type.
    Inner,
    /// Optional rule child — `Option<TapeCursor<'p>>`.
    OptionInner,
    /// Repeated rule children — handed back as the parent cursor of
    /// the Repeat/Seq record. Callers iterate via
    /// `cursor.children()` to walk the individual items.
    Slice,
}

fn directive_field_layout(kind: &DirectiveKind) -> Option<&'static DirectiveLayout> {
    use DirectiveFieldKind as F;

    // One `static` per variant so the returned reference has a
    // lifetime independent of the enum match and we can return
    // `Option<&'static DirectiveLayout>`.
    static RECOVER: DirectiveLayout = DirectiveLayout {
        struct_name: "RecoverDirective",
        slots: &[
            DirectiveSlot { name: "rule_name", kind: F::Identifier },
            DirectiveSlot { name: "sync_expr", kind: F::Inner },
        ],
    };
    static PRETTY: DirectiveLayout = DirectiveLayout {
        struct_name: "PrettyDirective",
        slots: &[
            DirectiveSlot { name: "target", kind: F::TextSpan },
            DirectiveSlot { name: "hints", kind: F::Slice },
        ],
    };
    static IMPORT: DirectiveLayout = DirectiveLayout {
        struct_name: "ImportDirective",
        slots: &[DirectiveSlot { name: "inner", kind: F::Inner }],
    };
    static WS: DirectiveLayout = DirectiveLayout {
        struct_name: "WsDirective",
        slots: &[DirectiveSlot { name: "value", kind: F::Inner }],
    };
    static TOKEN: DirectiveLayout = DirectiveLayout {
        struct_name: "TokenDirective",
        slots: &[DirectiveSlot { name: "name", kind: F::Identifier }],
    };
    static DEBUG: DirectiveLayout = DirectiveLayout {
        struct_name: "DebugDirective",
        slots: &[DirectiveSlot { name: "target", kind: F::TextSpan }],
    };
    static HOST: DirectiveLayout = DirectiveLayout {
        struct_name: "HostDirective",
        slots: &[
            DirectiveSlot { name: "name", kind: F::Identifier },
            DirectiveSlot { name: "type_annotation", kind: F::OptionInner },
        ],
    };

    Some(match kind {
        DirectiveKind::Recover => &RECOVER,
        DirectiveKind::Pretty => &PRETTY,
        DirectiveKind::Import => &IMPORT,
        DirectiveKind::Ws => &WS,
        DirectiveKind::Token => &TOKEN,
        DirectiveKind::Debug => &DEBUG,
        DirectiveKind::Host => &HOST,
        DirectiveKind::Other(_) => return None,
    })
}

