use std::mem::{align_of, offset_of, size_of};

use bbnf_type_kernel_v3::type_kernel::OperationContext;
use bbnf_type_kernel_v3::type_kernel::layout::{
    LayoutClaim, LayoutErrorKind, LayoutErrorOrBudget, StorageField, StorageType,
    TagRepresentation, TargetAbi, lower_physical, measure_native_rust_layouts,
};

#[repr(C)]
struct KernelSpan {
    start: u32,
    end: u32,
}

#[repr(C)]
struct KernelHeader {
    ptr: usize,
    len: usize,
    cap: usize,
}

#[repr(C)]
struct Padded {
    byte: u8,
    word: u64,
}

#[test]
fn modeled_kernel_carriers_match_native_repr_c_probes() {
    let mut operations = OperationContext::unlimited();
    let span =
        lower_physical(&StorageType::Span, TargetAbi::AppleAarch64, &mut operations).unwrap();
    assert_eq!(span.size(), size_of::<KernelSpan>() as u64);
    assert_eq!(span.alignment(), align_of::<KernelSpan>() as u64);
    assert_eq!(
        span.fields()[0].offset(),
        offset_of!(KernelSpan, start) as u64
    );
    assert_eq!(
        span.fields()[1].offset(),
        offset_of!(KernelSpan, end) as u64
    );
    assert_eq!(span.provenance().claim(), LayoutClaim::Modeled);

    let header = lower_physical(
        &StorageType::OwnedString,
        TargetAbi::AppleAarch64,
        &mut operations,
    )
    .unwrap();
    assert_eq!(header.size(), size_of::<KernelHeader>() as u64);
    assert_eq!(header.alignment(), align_of::<KernelHeader>() as u64);
    assert_eq!(
        header.fields()[2].offset(),
        offset_of!(KernelHeader, cap) as u64
    );

    let padded = lower_physical(
        &StorageType::Struct(vec![
            StorageField::new("byte", StorageType::U8),
            StorageField::new("word", StorageType::U64),
        ]),
        TargetAbi::AppleAarch64,
        &mut operations,
    )
    .unwrap();
    assert_eq!(padded.size(), size_of::<Padded>() as u64);
    assert_eq!(padded.fields()[1].offset(), offset_of!(Padded, word) as u64);
    assert_eq!(padded.padding()[0].bytes(), 7);
}

#[test]
fn tags_and_niches_are_explicitly_distinguished() {
    let mut operations = OperationContext::unlimited();
    let tagged = lower_physical(
        &StorageType::TaggedOption(Box::new(StorageType::U64)),
        TargetAbi::AppleAarch64,
        &mut operations,
    )
    .unwrap();
    assert!(matches!(
        tagged.tag(),
        TagRepresentation::Explicit { bytes: 1, .. }
    ));
    assert_eq!(tagged.size(), 16);

    let niche = lower_physical(
        &StorageType::NullablePointer,
        TargetAbi::AppleAarch64,
        &mut operations,
    )
    .unwrap();
    assert_eq!(niche.tag(), &TagRepresentation::NullPointerNiche);
    assert_eq!(
        niche.size(),
        size_of::<Option<std::ptr::NonNull<u8>>>() as u64
    );
}

#[test]
fn rust_containers_are_measured_evidence_but_rejected_as_kernel_layouts() {
    let measurements = measure_native_rust_layouts();
    assert!(
        measurements
            .iter()
            .all(|item| item.claim() == LayoutClaim::Measured)
    );
    assert!(
        measurements
            .iter()
            .any(|item| item.name() == "std::string::String")
    );
    assert!(
        measurements
            .iter()
            .any(|item| item.name() == "Option<String>")
    );
    if cfg!(all(target_arch = "aarch64", target_os = "macos")) {
        for name in ["std::string::String", "std::vec::Vec<u8>", "Option<String>"] {
            let measurement = measurements
                .iter()
                .find(|item| item.name() == name)
                .unwrap();
            assert_eq!((measurement.size(), measurement.alignment()), (24, 8));
        }
        let niche = measurements
            .iter()
            .find(|item| item.name() == "Option<NonNull<u8>> null niche")
            .unwrap();
        assert_eq!((niche.size(), niche.alignment()), (8, 8));
    }

    let mut operations = OperationContext::unlimited();
    let error = lower_physical(
        &StorageType::OpaqueRustContainer("String".to_owned()),
        TargetAbi::AppleAarch64,
        &mut operations,
    )
    .unwrap_err();
    assert!(matches!(
        error,
        LayoutErrorOrBudget::Layout(ref layout)
            if layout.kind() == &LayoutErrorKind::OpaqueRustContainer("String".to_owned())
    ));
}
