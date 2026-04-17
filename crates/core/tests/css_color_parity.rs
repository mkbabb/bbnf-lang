//! AW-III.W6.4 — CSS L4 color parity gate.
//!
//! Verifies the backend-resolved `Named("Color") → (U8, F64, F64, F64,
//! F64)` binding via [`bbnf::backend::rust::view::named_types`]. The
//! resolver table is data-driven (`BINDINGS`) so this test doubles as
//! the smoke check for the W6.4 extraction: the row lookup must
//! return the canonical 5-field tuple for every known colour name,
//! and the planner must admit that tuple at the `LARGE_PAYLOAD_MAX`
//! cap.
//!
//! The end-to-end `rgb(...)` → `Color` struct projection is already
//! covered by the decoder tests in `css_l4_color_view.rs`; this file
//! focuses on the resolver table contract.

use bbnf::backend::rust::view::named_types::{
    resolve_named_type, NamedTypeBinding, RustNamedTypes, BINDINGS,
};
use bbnf_ir::passes::{plan_layout_with_cap, NamedTypeResolver};
use bbnf_ir::{GrammarIR, StringId, TypeDesc};

// ─── Helpers ──────────────────────────────────────────────────────────

/// Build a minimal `GrammarIR` whose string table carries the given
/// names at their corresponding `StringId`s. Used so the resolver
/// can be exercised without a full grammar compile.
fn ir_with_names(names: &[&str]) -> GrammarIR {
    let mut ir = GrammarIR::default();
    for n in names {
        ir.strings.push((*n).to_string());
    }
    ir
}

// ─── Binding table invariants ─────────────────────────────────────────

#[test]
fn bindings_table_carries_color_and_colormix() {
    // W6.4 invariant: the universal binding table admits at least
    // the two AW.0.5 colour names. Future rows append below these.
    let names: Vec<&str> = BINDINGS.iter().map(|b| b.name).collect();
    assert!(
        names.contains(&"Color"),
        "BINDINGS must admit \"Color\"; got {:?}",
        names,
    );
    assert!(
        names.contains(&"ColorMix"),
        "BINDINGS must admit \"ColorMix\"; got {:?}",
        names,
    );
}

#[test]
fn bindings_color_fields_are_five_scalar_tuple() {
    let binding = BINDINGS
        .iter()
        .find(|b| b.name == "Color")
        .expect("Color binding present");
    assert_eq!(
        binding.fields.len(),
        5,
        "Color tuple arity: expected 5 (u8 space + 4 f64 channels), got {}",
        binding.fields.len(),
    );
    assert!(
        matches!(binding.fields[0], TypeDesc::U8),
        "Color field 0 must be U8 (space discriminant); got {:?}",
        binding.fields[0],
    );
    for (i, f) in binding.fields[1..].iter().enumerate() {
        assert!(
            matches!(f, TypeDesc::F64),
            "Color field {}: expected F64, got {:?}",
            i + 1,
            f,
        );
    }
}

#[test]
fn bindings_colormix_shape_matches_color() {
    // W6.4 constraint: ColorMix reuses the canonical colour tuple —
    // the mix-space u8 sits in the space slot, the four f64 slots
    // hold left/right colour refs + percentages. Shape equality
    // guarantees `aggregate_payload_ctor`'s 40 B routing fires for
    // both.
    let color = BINDINGS.iter().find(|b| b.name == "Color").unwrap();
    let mix = BINDINGS.iter().find(|b| b.name == "ColorMix").unwrap();
    assert_eq!(color.fields.len(), mix.fields.len(), "tuple arity parity");
    for (i, (c, m)) in color.fields.iter().zip(mix.fields.iter()).enumerate() {
        assert_eq!(c, m, "field {}: Color={:?} ColorMix={:?}", i, c, m);
    }
}

// ─── resolve_named_type contract ──────────────────────────────────────

#[test]
fn resolve_named_type_admits_color() {
    let ir = ir_with_names(&["Color"]);
    let binding: Option<&NamedTypeBinding> =
        resolve_named_type(&TypeDesc::Named(0), &ir.strings);
    let binding = binding.expect("Color must resolve via universal lookup");
    assert_eq!(binding.name, "Color");
    assert_eq!(binding.fields.len(), 5);
}

#[test]
fn resolve_named_type_admits_colormix() {
    let ir = ir_with_names(&["ColorMix"]);
    let binding = resolve_named_type(&TypeDesc::Named(0), &ir.strings)
        .expect("ColorMix must resolve via universal lookup");
    assert_eq!(binding.name, "ColorMix");
}

#[test]
fn resolve_named_type_rejects_unknown() {
    // W6.4 invariant: names that do not appear in the binding table
    // resolve to `None`. The layout pass then falls through to the
    // structural compound form — no panic, no accidental admission.
    let ir = ir_with_names(&["Widget", "Stylesheet", "UnknownNamed"]);
    for sid in 0..ir.strings.len() as StringId {
        assert!(
            resolve_named_type(&TypeDesc::Named(sid), &ir.strings).is_none(),
            "Unknown name {:?} must not resolve",
            ir.strings[sid as usize],
        );
    }
}

#[test]
fn resolve_named_type_rejects_non_named_typedesc() {
    // W6.4 invariant: a `TypeDesc` that is not `Named` returns `None`
    // regardless of the string table contents. The universal lookup
    // is a pass-through for unrelated variants.
    let ir = ir_with_names(&["Color"]);
    let non_named: &[TypeDesc] = &[
        TypeDesc::Span,
        TypeDesc::F64,
        TypeDesc::U8,
        TypeDesc::Bool,
        TypeDesc::Tuple(vec![TypeDesc::U8]),
        TypeDesc::Vec(Box::new(TypeDesc::F64)),
    ];
    for td in non_named {
        assert!(
            resolve_named_type(td, &ir.strings).is_none(),
            "non-Named TypeDesc {:?} must not resolve",
            td,
        );
    }
}

// ─── RustNamedTypes field-for-field parity ───────────────────────────

#[test]
fn rust_named_types_mirrors_binding_table() {
    // W6.4 core parity: every row in `BINDINGS` resolves via
    // `RustNamedTypes::resolve_named` to the tuple the row declares.
    // Proves the delegation fold removed the hardcoded match without
    // drifting.
    let names: Vec<&'static str> = BINDINGS.iter().map(|b| b.name).collect();
    let ir = ir_with_names(&names);
    let resolver = RustNamedTypes::from_ir(&ir);

    for (idx, binding) in BINDINGS.iter().enumerate() {
        let resolved = resolver
            .resolve_named(idx as StringId)
            .unwrap_or_else(|| panic!("{} must resolve", binding.name));
        match resolved {
            TypeDesc::Tuple(fields) => {
                assert_eq!(
                    fields.len(),
                    binding.fields.len(),
                    "{}: resolved tuple arity mismatch",
                    binding.name,
                );
                for (i, (res, decl)) in
                    fields.iter().zip(binding.fields.iter()).enumerate()
                {
                    assert_eq!(
                        res, decl,
                        "{}: field {}: resolved={:?} declared={:?}",
                        binding.name, i, res, decl,
                    );
                }
            }
            other => panic!("{} resolved to {:?}, expected Tuple", binding.name, other),
        }
    }
}

// ─── Planner admits the resolved tuple at LARGE_PAYLOAD_MAX ──────────

#[test]
fn color_tuple_plans_at_40_bytes_under_large_cap() {
    // W6.4 + AW.0.5: the 5-field colour tuple must plan to 40 B at
    // natural alignment under the `LARGE_PAYLOAD_MAX = 64` cap. This
    // is the final piece the planner relies on to route colour
    // records through `PayloadData::LargeAggregate`.
    let binding = BINDINGS.iter().find(|b| b.name == "Color").unwrap();
    let fields: Vec<TypeDesc> = binding.fields.to_vec();

    // 16 B cap rejects (40 > 16).
    assert!(
        plan_layout_with_cap(&fields, 16).is_none(),
        "Color must NOT fit under 16 B MAX_PAYLOAD_BYTES",
    );

    // 64 B cap admits and plans to 40 B.
    let layout = plan_layout_with_cap(&fields, 64)
        .expect("Color must fit under 64 B LARGE_PAYLOAD_MAX");
    assert_eq!(layout.total_bytes, 40, "total bytes");
    assert_eq!(layout.fields.len(), 5, "field count");
    assert_eq!(layout.fields[0].offset, 0, "u8 space @ 0");
    assert_eq!(layout.fields[1].offset, 8, "f64 c1 @ 8 (align-bump)");
    assert_eq!(layout.fields[2].offset, 16, "f64 c2 @ 16");
    assert_eq!(layout.fields[3].offset, 24, "f64 c3 @ 24");
    assert_eq!(layout.fields[4].offset, 32, "f64 alpha @ 32");
}
