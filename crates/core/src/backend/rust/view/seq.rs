//! View emission for `MustTape` Seq rules.
//!
//! A Seq rule's view wraps a compound `TapeCursor<'p>` whose
//! children are the individually-pushed sub-records. This module
//! generates typed positional child accessors (`.child_0()`,
//! `.child_1()`, ...) that return the appropriate view type for
//! each position, plus named accessors (`.identifier()`,
//! `.expression()`, ...) when the child is a `Ref` to a named rule.

use bbnf_ir::passes::{PayloadField, PayloadLayout};
use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed Seq child accessors for a rule whose body is a `Seq`.
///
/// Returns an `impl` block with `.child_N()` positional accessors
/// and, when available, named accessors derived from `Ref` targets.
pub fn emit_seq_accessors(
    rule: &IrRule,
    rule_name: &str,
    ir: &GrammarIR,
    grammar_name: &str,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);

    let children = match &rule.body {
        IrNode::Seq(children) => children,
        _ => return quote! {},
    };

    // Collect the effective children: Skip/Next expose the kept side,
    // Map exposes the inner, OptionalWhitespace is skipped.
    let effective: Vec<(usize, &IrNode)> = children
        .iter()
        .enumerate()
        .filter(|(_, child)| !matches!(child, IrNode::OptionalWhitespace(_)))
        .collect();

    if effective.is_empty() {
        return quote! {};
    }

    let mut methods = Vec::new();
    let mut seen_names: std::collections::HashSet<String> = Default::default();

    for (tape_idx, &(_, child)) in effective.iter().enumerate() {
        let idx_lit = tape_idx;
        let positional_ident = format_ident!("child_{}", tape_idx);

        // Determine child view type based on the child node shape.
        let (child_view_ty, named_ident) = resolve_child_view(child, ir, grammar_name);

        let doc_pos = format!("Child at position {} as a typed view.", tape_idx);
        methods.push(quote! {
            #[doc = #doc_pos]
            #[inline]
            pub fn #positional_ident(&self) -> ::core::option::Option<#child_view_ty<'p>> {
                self.cursor.child(#idx_lit).map(|c| #child_view_ty::from_cursor(c, self.input))
            }
        });

        // Named accessor when the child is a Ref to a named rule.
        if let Some(name) = named_ident {
            if seen_names.insert(name.clone()) {
                let name_ident = format_ident!("{}", name);
                let (named_view_ty, _) = resolve_child_view(child, ir, grammar_name);
                let doc_named = format!("The `{}` child as a typed view.", name);
                methods.push(quote! {
                    #[doc = #doc_named]
                    #[inline]
                    pub fn #name_ident(&self) -> ::core::option::Option<#named_view_ty<'p>> {
                        self.cursor.child(#idx_lit).map(|c| #named_view_ty::from_cursor(c, self.input))
                    }
                });
            }
        }
    }

    // `.num_children()` convenience.
    let count = effective.len();
    methods.push(quote! {
        /// The number of typed child positions in this Seq.
        #[inline]
        pub fn num_children(&self) -> usize {
            #count
        }
    });

    if methods.is_empty() {
        return quote! {};
    }

    quote! {
        #[allow(dead_code)]
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// Resolve the view type ident and optional named accessor for a
/// child node. Returns `(ViewTypeIdent, Option<accessor_name>)`.
///
/// - `Ref(rule_id)` → `<RuleName>View` + `Some("rule_name")`
/// - Everything else → `<Grammar>NodeView` + `None`
fn resolve_child_view(
    node: &IrNode,
    ir: &GrammarIR,
    grammar_name: &str,
) -> (proc_macro2::Ident, Option<String>) {
    // Peel through Map/OptionalWhitespace/Skip/Next to find the
    // innermost structurally meaningful node.
    let inner = peel_wrappers(node);

    match inner {
        IrNode::Ref(rule_id) => {
            let target_rule = &ir.rules[*rule_id as usize];
            if target_rule.meta.is_transparent {
                // Transparent rules don't have their own view type;
                // fall back to the generic NodeView.
                let nv = format_ident!("{}NodeView", grammar_name);
                (nv, None)
            } else {
                let name = ir.get_string(target_rule.name);
                let view_ident = format_ident!("{}View", name);
                (view_ident, Some(name.to_string()))
            }
        }
        _ => {
            let nv = format_ident!("{}NodeView", grammar_name);
            (nv, None)
        }
    }
}

/// Peel through Map, OptionalWhitespace, Skip (keep left), and
/// Next (keep right) wrappers to find the structurally meaningful
/// inner node.
fn peel_wrappers(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Next(_, inner)
        | IrNode::Negate(inner) => peel_wrappers(inner),
        IrNode::Skip(kept, _) => peel_wrappers(kept),
        other => other,
    }
}

// ── AR.9: KV-pair view accessors ────────────────────────────────

/// Emit typed accessors for a rule whose tape record is a flattened
/// KV-pair (`TapeKind::KvPair`).
///
/// The key Span is stored in the record's own `(span_lo, span_hi)`.
/// The scalar value is packed into the aggregate payload buffer.
/// This accessor surface exposes:
///
/// - `.key()` — the source text of the key Span.
/// - `.key_span()` — the `(lo, hi)` byte offsets of the key.
/// - `.value()` — the decoded scalar value from the payload.
/// - `.text()` — alias for `.key()` (backward compat with leaf views).
pub fn emit_kv_pair_accessors(
    rule: &IrRule,
    rule_name: &str,
    layout: &PayloadLayout,
    _type_desc: Option<&TypeDesc>,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);
    let total_bytes = layout.total_bytes as usize;

    // The layout contains only the scalar value field (the Span key
    // is implicit in the record's own span). There should be exactly
    // one field.
    assert!(
        layout.fields.len() == 1,
        "KV-pair layout must have exactly one scalar field, got {}",
        layout.fields.len()
    );
    let field = &layout.fields[0];
    let field_ty_ident = format_ident!(
        "{}",
        field.ty.rust_ident().expect("KV-pair value has rust_ident")
    );
    let field_read = kv_pair_field_read(field);
    let field_zero = kv_pair_field_zero(&field.ty);

    let mut methods = Vec::new();

    // `.key()` — the source text of the key Span.
    methods.push(quote! {
        /// The key as source text (the Span matched by the first
        /// child of the original Seq).
        #[inline]
        pub fn key(&self) -> &'p str {
            self.span_text()
        }
    });

    // `.text()` — alias for backward compatibility.
    methods.push(quote! {
        /// Alias for `.key()` — the source text of the key Span.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    });

    // `.key_span()` — byte offset pair of the key.
    methods.push(quote! {
        /// The key Span as `(lo, hi)` byte offsets.
        #[inline]
        pub fn key_span(&self) -> (u32, u32) {
            self.span()
        }
    });

    // `.value()` — the decoded scalar from the aggregate payload.
    methods.push(quote! {
        /// The value scalar decoded from the aggregate payload.
        ///
        /// Returns the zero-initialized value if no payload was
        /// written for this record.
        #[inline]
        pub fn value(&self) -> #field_ty_ident {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, #total_bytes) {
                Some(__bytes) => #field_read,
                None => #field_zero,
            }
        }
    });

    // `.byte_range()` convenience.
    if rule.meta.span_eligible {
        methods.push(quote! {
            /// The matched byte range as a `Range<usize>`.
            #[inline]
            pub fn byte_range(&self) -> ::core::ops::Range<usize> {
                let (lo, hi) = self.span();
                lo as usize..hi as usize
            }
        });
    }

    quote! {
        #[allow(dead_code)]
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// Emit the decoder expression for a KV-pair value field.
fn kv_pair_field_read(field: &PayloadField) -> TokenStream {
    let offset = field.offset as usize;
    let size = field
        .ty
        .payload_size_bytes()
        .expect("KV-pair value has payload_size") as usize;
    let end = offset + size;
    match field.ty {
        TypeDesc::Bool => quote! { __bytes[#offset] != 0 },
        TypeDesc::I8 => quote! { __bytes[#offset] as i8 },
        TypeDesc::U8 => quote! { __bytes[#offset] },
        TypeDesc::F64 => quote! {
            f64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 8 bytes"),
            )
        },
        TypeDesc::I16 => quote! {
            i16::from_le_bytes(
                <[u8; 2]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 2 bytes"),
            )
        },
        TypeDesc::U16 => quote! {
            u16::from_le_bytes(
                <[u8; 2]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 2 bytes"),
            )
        },
        TypeDesc::I32 => quote! {
            i32::from_le_bytes(
                <[u8; 4]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 4 bytes"),
            )
        },
        TypeDesc::U32 => quote! {
            u32::from_le_bytes(
                <[u8; 4]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 4 bytes"),
            )
        },
        TypeDesc::I64 => quote! {
            i64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 8 bytes"),
            )
        },
        TypeDesc::U64 => quote! {
            u64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("kv_pair slice is 8 bytes"),
            )
        },
        _ => unreachable!("KV-pair value must be a scalar TypeDesc"),
    }
}

/// Emit the zero value for a KV-pair value field.
fn kv_pair_field_zero(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::F64 => quote! { 0.0_f64 },
        TypeDesc::Bool => quote! { false },
        TypeDesc::I8 => quote! { 0_i8 },
        TypeDesc::U8 => quote! { 0_u8 },
        TypeDesc::I16 => quote! { 0_i16 },
        TypeDesc::U16 => quote! { 0_u16 },
        TypeDesc::I32 => quote! { 0_i32 },
        TypeDesc::U32 => quote! { 0_u32 },
        TypeDesc::I64 => quote! { 0_i64 },
        TypeDesc::U64 => quote! { 0_u64 },
        _ => quote! { ::core::default::Default::default() },
    }
}
