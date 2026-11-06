use bbnf_type_kernel_v4::*;

use super::super::{operations, primitive_solution, toolchain};

#[test]
fn semantic_profile_is_explicit_and_layout_provenance_is_toolchain_bound() {
    let solution = primitive_solution(Primitive::Span, "SpanValue");
    let mut operations = operations();
    let profile = TargetProfile {
        semantic: SemanticProfile::GrammarDerivedSchema,
        layout: LayoutProfile::AppleArm64Explicit,
    };
    let plan = lower(&solution, profile, &mut operations).expect("lower");
    assert_eq!(plan.profile(), profile);
    assert_eq!(plan.semantic_digest(), solution.semantic_digest());

    let first = measure_carrier(
        Carrier::Span,
        LayoutProfile::AppleArm64Explicit,
        toolchain(),
        &mut operations,
    )
    .expect("span layout");
    assert_eq!((first.size(), first.alignment()), (8, 4));
    assert_eq!(first.offsets(), &[0, 4]);
    let mut changed = toolchain();
    changed.rustc.push_str("-changed");
    let second = measure_carrier(
        Carrier::Span,
        LayoutProfile::AppleArm64Explicit,
        changed,
        &mut operations,
    )
    .expect("changed toolchain layout");
    assert_ne!(first.measurement_identity(), second.measurement_identity());
    assert_eq!(plan.semantic_digest(), solution.semantic_digest());
}

#[test]
fn explicit_carrier_matrix_and_opaque_target_rejection_are_narrow() {
    let mut operations = operations();
    let expected = [
        (Carrier::ThreeWordHeader, 24, 8),
        (Carrier::PaddedByteWord, 16, 8),
        (Carrier::ExplicitTaggedU64Option, 16, 8),
        (Carrier::NonNullPointerOption, 8, 8),
    ];
    for (carrier, size, alignment) in expected {
        let evidence = measure_carrier(
            carrier,
            LayoutProfile::AppleArm64Explicit,
            toolchain(),
            &mut operations,
        )
        .expect("carrier");
        assert_eq!((evidence.size(), evidence.alignment()), (size, alignment));
    }
    let error = measure_carrier(
        Carrier::Span,
        LayoutProfile::SemanticOnly,
        toolchain(),
        &mut operations,
    )
    .expect_err("semantic profile has no physical authority");
    assert!(matches!(
        error.kind(),
        TypeErrorKind::UnsupportedLayout { .. }
    ));
}
