//! Physical Apple arm64 layouts for kernel-owned representations only.

use std::fmt;
use std::mem::{align_of, size_of};
use std::ptr::NonNull;

use super::{OperationContext, Origin, TypeError};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetAbi {
    AppleAarch64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StorageType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I64,
    F64,
    /// Kernel-owned `#[repr(C)] { start: u32, end: u32 }`.
    Span,
    /// Kernel-owned `#[repr(C)] { ptr: usize, len: usize, cap: usize }`.
    OwnedString,
    /// Kernel-owned vector header with the same explicit three-word fields.
    OwnedVec,
    /// Kernel-owned non-null pointer whose optional form uses the null niche.
    NullablePointer,
    Struct(Vec<StorageField>),
    /// Explicit tagged representation; this does not assume a compiler niche.
    TaggedOption(Box<StorageType>),
    /// Rust library/compiler containers have no kernel-owned physical contract.
    OpaqueRustContainer(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StorageField {
    name: String,
    ty: StorageType,
}

impl StorageField {
    #[must_use]
    pub fn new(name: impl Into<String>, ty: StorageType) -> Self {
        Self {
            name: name.into(),
            ty,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FieldLayout {
    name: String,
    offset: u64,
    layout: Box<PhysicalLayout>,
}

impl FieldLayout {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn offset(&self) -> u64 {
        self.offset
    }

    #[must_use]
    pub fn layout(&self) -> &PhysicalLayout {
        &self.layout
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Padding {
    offset: u64,
    bytes: u64,
}

impl Padding {
    #[must_use]
    pub fn offset(&self) -> u64 {
        self.offset
    }

    #[must_use]
    pub fn bytes(&self) -> u64 {
        self.bytes
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TagRepresentation {
    None,
    Explicit {
        offset: u64,
        bytes: u8,
        none: u64,
        some: u64,
    },
    NullPointerNiche,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LayoutClaim {
    /// Deterministic target model derived from the explicit kernel carrier.
    Modeled,
    /// Runtime `size_of`/`align_of` observation on this exact host/toolchain.
    Measured,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutProvenance {
    target: String,
    representation: String,
    claim: LayoutClaim,
}

impl LayoutProvenance {
    #[must_use]
    pub fn target(&self) -> &str {
        &self.target
    }

    #[must_use]
    pub fn representation(&self) -> &str {
        &self.representation
    }

    #[must_use]
    pub fn claim(&self) -> LayoutClaim {
        self.claim
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PhysicalLayout {
    size: u64,
    alignment: u64,
    fields: Vec<FieldLayout>,
    padding: Vec<Padding>,
    tag: TagRepresentation,
    provenance: LayoutProvenance,
}

impl PhysicalLayout {
    #[must_use]
    pub fn size(&self) -> u64 {
        self.size
    }

    #[must_use]
    pub fn alignment(&self) -> u64 {
        self.alignment
    }

    #[must_use]
    pub fn fields(&self) -> &[FieldLayout] {
        &self.fields
    }

    #[must_use]
    pub fn padding(&self) -> &[Padding] {
        &self.padding
    }

    #[must_use]
    pub fn tag(&self) -> &TagRepresentation {
        &self.tag
    }

    #[must_use]
    pub fn provenance(&self) -> &LayoutProvenance {
        &self.provenance
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LayoutErrorKind {
    OpaqueRustContainer(String),
    ArithmeticOverflow,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutError {
    kind: LayoutErrorKind,
    site: String,
}

impl LayoutError {
    fn new(kind: LayoutErrorKind, site: impl Into<String>) -> Self {
        Self {
            kind,
            site: site.into(),
        }
    }

    #[must_use]
    pub fn kind(&self) -> &LayoutErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn site(&self) -> &str {
        &self.site
    }
}

impl fmt::Display for LayoutError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?} at {}", self.kind, self.site)
    }
}

impl std::error::Error for LayoutError {}

pub fn lower_physical(
    ty: &StorageType,
    target: TargetAbi,
    operations: &mut OperationContext,
) -> Result<PhysicalLayout, LayoutErrorOrBudget> {
    operations
        .step(
            "layout.lower",
            &[],
            [Origin::new("layout.apple_aarch64", format!("{ty:?}"))],
        )
        .map_err(LayoutErrorOrBudget::Budget)?;
    match target {
        TargetAbi::AppleAarch64 => lower_apple_aarch64(ty, operations),
    }
}

#[derive(Debug)]
pub enum LayoutErrorOrBudget {
    Layout(LayoutError),
    Budget(TypeError),
}

impl fmt::Display for LayoutErrorOrBudget {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Layout(error) => error.fmt(formatter),
            Self::Budget(error) => error.fmt(formatter),
        }
    }
}

impl std::error::Error for LayoutErrorOrBudget {}

fn lower_apple_aarch64(
    ty: &StorageType,
    operations: &mut OperationContext,
) -> Result<PhysicalLayout, LayoutErrorOrBudget> {
    operations
        .step(
            "layout.apple_aarch64.node",
            &[],
            [Origin::boundary("layout.apple_aarch64")],
        )
        .map_err(LayoutErrorOrBudget::Budget)?;
    let scalar = |size, alignment, representation: &str| PhysicalLayout {
        size,
        alignment,
        fields: Vec::new(),
        padding: Vec::new(),
        tag: TagRepresentation::None,
        provenance: modeled(representation),
    };
    match ty {
        StorageType::Bool | StorageType::U8 => Ok(scalar(1, 1, "kernel scalar")),
        StorageType::U16 => Ok(scalar(2, 2, "kernel scalar")),
        StorageType::U32 => Ok(scalar(4, 4, "kernel scalar")),
        StorageType::U64 | StorageType::I64 | StorageType::F64 => Ok(scalar(8, 8, "kernel scalar")),
        StorageType::Span => lower_struct(
            &[
                StorageField::new("start", StorageType::U32),
                StorageField::new("end", StorageType::U32),
            ],
            "kernel #[repr(C)] Span",
            operations,
        ),
        StorageType::OwnedString => lower_struct(
            &[
                StorageField::new("ptr", StorageType::U64),
                StorageField::new("len", StorageType::U64),
                StorageField::new("cap", StorageType::U64),
            ],
            "kernel #[repr(C)] OwnedString header",
            operations,
        ),
        StorageType::OwnedVec => lower_struct(
            &[
                StorageField::new("ptr", StorageType::U64),
                StorageField::new("len", StorageType::U64),
                StorageField::new("cap", StorageType::U64),
            ],
            "kernel #[repr(C)] OwnedVec header",
            operations,
        ),
        StorageType::NullablePointer => Ok(PhysicalLayout {
            size: 8,
            alignment: 8,
            fields: Vec::new(),
            padding: Vec::new(),
            tag: TagRepresentation::NullPointerNiche,
            provenance: modeled("kernel non-null pointer optional carrier"),
        }),
        StorageType::Struct(fields) => {
            lower_struct(fields, "kernel #[repr(C)] aggregate", operations)
        }
        StorageType::TaggedOption(payload) => {
            let payload = lower_apple_aarch64(payload, operations)?;
            let payload_offset =
                align_up(1, payload.alignment).ok_or_else(|| overflow("layout.tagged_option"))?;
            let unpadded = payload_offset
                .checked_add(payload.size)
                .ok_or_else(|| overflow("layout.tagged_option"))?;
            let size = align_up(unpadded, payload.alignment)
                .ok_or_else(|| overflow("layout.tagged_option"))?;
            let mut padding = Vec::new();
            if payload_offset > 1 {
                padding.push(Padding {
                    offset: 1,
                    bytes: payload_offset - 1,
                });
            }
            if size > unpadded {
                padding.push(Padding {
                    offset: unpadded,
                    bytes: size - unpadded,
                });
            }
            Ok(PhysicalLayout {
                size,
                alignment: payload.alignment,
                fields: vec![
                    FieldLayout {
                        name: "tag".to_owned(),
                        offset: 0,
                        layout: Box::new(scalar(1, 1, "explicit option tag")),
                    },
                    FieldLayout {
                        name: "payload".to_owned(),
                        offset: payload_offset,
                        layout: Box::new(payload),
                    },
                ],
                padding,
                tag: TagRepresentation::Explicit {
                    offset: 0,
                    bytes: 1,
                    none: 0,
                    some: 1,
                },
                provenance: modeled("kernel explicit tagged option"),
            })
        }
        StorageType::OpaqueRustContainer(name) => {
            Err(LayoutErrorOrBudget::Layout(LayoutError::new(
                LayoutErrorKind::OpaqueRustContainer(name.clone()),
                "layout.opaque",
            )))
        }
    }
}

fn lower_struct(
    fields: &[StorageField],
    representation: &str,
    operations: &mut OperationContext,
) -> Result<PhysicalLayout, LayoutErrorOrBudget> {
    let mut cursor = 0_u64;
    let mut alignment = 1_u64;
    let mut lowered = Vec::with_capacity(fields.len());
    let mut padding = Vec::new();
    for field in fields {
        let layout = lower_apple_aarch64(&field.ty, operations)?;
        alignment = alignment.max(layout.alignment);
        let offset =
            align_up(cursor, layout.alignment).ok_or_else(|| overflow("layout.struct.offset"))?;
        if offset > cursor {
            padding.push(Padding {
                offset: cursor,
                bytes: offset - cursor,
            });
        }
        cursor = offset
            .checked_add(layout.size)
            .ok_or_else(|| overflow("layout.struct.size"))?;
        lowered.push(FieldLayout {
            name: field.name.clone(),
            offset,
            layout: Box::new(layout),
        });
    }
    let size = align_up(cursor, alignment).ok_or_else(|| overflow("layout.struct.tail"))?;
    if size > cursor {
        padding.push(Padding {
            offset: cursor,
            bytes: size - cursor,
        });
    }
    Ok(PhysicalLayout {
        size,
        alignment,
        fields: lowered,
        padding,
        tag: TagRepresentation::None,
        provenance: modeled(representation),
    })
}

fn align_up(value: u64, alignment: u64) -> Option<u64> {
    let mask = alignment.checked_sub(1)?;
    value.checked_add(mask).map(|sum| sum & !mask)
}

fn overflow(site: &str) -> LayoutErrorOrBudget {
    LayoutErrorOrBudget::Layout(LayoutError::new(LayoutErrorKind::ArithmeticOverflow, site))
}

fn modeled(representation: &str) -> LayoutProvenance {
    LayoutProvenance {
        target: "aarch64-apple-darwin".to_owned(),
        representation: representation.to_owned(),
        claim: LayoutClaim::Modeled,
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeMeasurement {
    name: String,
    size: usize,
    alignment: usize,
    claim: LayoutClaim,
}

impl NativeMeasurement {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn size(&self) -> usize {
        self.size
    }

    #[must_use]
    pub fn alignment(&self) -> usize {
        self.alignment
    }

    #[must_use]
    pub fn claim(&self) -> LayoutClaim {
        self.claim
    }
}

/// Host observations are evidence only; they do not authorize lowering Rust's
/// library containers as kernel physical layouts.
#[must_use]
pub fn measure_native_rust_layouts() -> Vec<NativeMeasurement> {
    vec![
        measurement::<String>("std::string::String"),
        measurement::<Vec<u8>>("std::vec::Vec<u8>"),
        measurement::<Option<String>>("Option<String>"),
        measurement::<Option<NonNull<u8>>>("Option<NonNull<u8>> null niche"),
    ]
}

fn measurement<T>(name: &str) -> NativeMeasurement {
    NativeMeasurement {
        name: name.to_owned(),
        size: size_of::<T>(),
        alignment: align_of::<T>(),
        claim: LayoutClaim::Measured,
    }
}
