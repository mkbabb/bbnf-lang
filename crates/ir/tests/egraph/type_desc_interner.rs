//! Tranche AA.1 — `TypeDescInterner` round-trip + idempotency.
//!
//! The interner is the load-bearing substrate for stable structural
//! type identity across the pipeline. These tests verify:
//!
//! 1. `intern` is idempotent — structurally equal inputs return the
//!    same id on every call.
//! 2. `resolve` returns the exact value that was interned.
//! 3. Distinct structural shapes produce distinct ids.
//! 4. Nested shapes (Tuple, Vec, Option) are canonicalized by
//!    structural equality, not pointer equality.
//! 5. Serde round-trip + `rebuild_lookup` preserves stable identity.

use bbnf_ir::{TypeDesc, TypeDescInterner};

#[test]
fn intern_idempotent_simple() {
    let mut interner = TypeDescInterner::new();
    let a = interner.intern(TypeDesc::Span);
    let b = interner.intern(TypeDesc::Span);
    assert_eq!(a, b, "idempotent intern of Span");
    assert_eq!(interner.len(), 1);
}

#[test]
fn intern_idempotent_nested() {
    let mut interner = TypeDescInterner::new();

    let ty = TypeDesc::Vec(Box::new(TypeDesc::Tuple(vec![
        TypeDesc::Span,
        TypeDesc::F64,
    ])));

    let a = interner.intern(ty.clone());
    let b = interner.intern(ty);
    assert_eq!(a, b, "idempotent intern of nested shape");
    assert_eq!(interner.len(), 1);
}

#[test]
fn distinct_shapes_distinct_ids() {
    let mut interner = TypeDescInterner::new();

    let a = interner.intern(TypeDesc::Span);
    let b = interner.intern(TypeDesc::F64);
    let c = interner.intern(TypeDesc::Enum);
    let d = interner.intern(TypeDesc::BoxedEnum);

    assert_ne!(a, b);
    assert_ne!(b, c);
    assert_ne!(c, d);
    assert_eq!(interner.len(), 4);
}

#[test]
fn resolve_round_trip() {
    let mut interner = TypeDescInterner::new();
    let ty = TypeDesc::Option(Box::new(TypeDesc::Vec(Box::new(TypeDesc::Span))));

    let id = interner.intern(ty.clone());
    assert_eq!(interner.resolve(id), &ty);
    assert_eq!(interner.get_cloned(id), ty);
}

#[test]
fn tuple_structural_equality() {
    let mut interner = TypeDescInterner::new();

    // Two tuples with identical element-wise shape should collapse.
    let t1 = TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::Span, TypeDesc::Enum]);
    let t2 = TypeDesc::Tuple(vec![TypeDesc::Span, TypeDesc::Span, TypeDesc::Enum]);

    let a = interner.intern(t1);
    let b = interner.intern(t2);
    assert_eq!(a, b);
    assert_eq!(interner.len(), 1);
}

#[test]
fn serde_round_trip_rebuilds_lookup() {
    let mut interner = TypeDescInterner::new();
    let id_span = interner.intern(TypeDesc::Span);
    let id_vec = interner.intern(TypeDesc::Vec(Box::new(TypeDesc::Span)));
    assert_eq!(interner.len(), 2);

    // Serialize + deserialize.
    let bytes = rmp_serde::to_vec(&interner).unwrap();
    let mut reloaded: TypeDescInterner = rmp_serde::from_slice(&bytes).unwrap();

    // Storage survives.
    assert_eq!(reloaded.resolve(id_span), &TypeDesc::Span);
    assert_eq!(
        reloaded.resolve(id_vec),
        &TypeDesc::Vec(Box::new(TypeDesc::Span))
    );

    // Lookup table must be rebuilt before `intern` is called on the
    // reloaded interner (the `#[serde(skip)]` attribute drops the
    // FxHashMap state across the wire).
    reloaded.rebuild_lookup();

    // Re-interning the same shape must return the original id.
    let id_span_again = reloaded.intern(TypeDesc::Span);
    assert_eq!(id_span, id_span_again);
    assert_eq!(
        reloaded.len(),
        2,
        "no new entry from re-intern after reload"
    );
}

#[test]
fn grammar_ir_intern_accessor() {
    use bbnf_ir::GrammarIR;

    let mut ir = GrammarIR::default();
    assert!(ir.type_desc_interner.is_empty());

    let id1 = ir.intern_type(TypeDesc::Span);
    let id2 = ir.intern_type(TypeDesc::Span);
    assert_eq!(id1, id2, "GrammarIR::intern_type is idempotent");
    assert_eq!(ir.resolve_type(id1), &TypeDesc::Span);

    let id3 = ir.intern_type(TypeDesc::Vec(Box::new(TypeDesc::F64)));
    assert_ne!(id1, id3);
    assert_eq!(ir.type_desc_interner.len(), 2);
}
