//! Explicit semantic/profile boundary and narrow physical carrier evidence.

use super::solve::{SolvedNode, TypeSolution};
use super::{
    OperationContext, Origin, SemanticDigest, TypeError, TypeErrorKind, charge_text, fnv1a64,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SemanticProfile {
    ExactValues,
    GrammarDerivedSchema,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LayoutProfile {
    SemanticOnly,
    AppleArm64Explicit,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TargetProfile {
    pub semantic: SemanticProfile,
    pub layout: LayoutProfile,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ToolchainIdentity {
    pub target: String,
    pub rustc: String,
    pub typescript: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LayoutClaim {
    Modeled,
    Measured,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Carrier {
    Span,
    ThreeWordHeader,
    PaddedByteWord,
    ExplicitTaggedU64Option,
    NonNullPointerOption,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutEvidence {
    carrier: Carrier,
    size: u64,
    alignment: u64,
    offsets: Vec<u64>,
    padding: Vec<(u64, u64)>,
    tag: String,
    profile: LayoutProfile,
    claim: LayoutClaim,
    toolchain: ToolchainIdentity,
    measurement_identity: String,
}

impl LayoutEvidence {
    #[must_use]
    pub fn carrier(&self) -> Carrier {
        self.carrier
    }

    #[must_use]
    pub fn size(&self) -> u64 {
        self.size
    }

    #[must_use]
    pub fn alignment(&self) -> u64 {
        self.alignment
    }

    #[must_use]
    pub fn offsets(&self) -> &[u64] {
        &self.offsets
    }

    #[must_use]
    pub fn padding(&self) -> &[(u64, u64)] {
        &self.padding
    }

    #[must_use]
    pub fn tag(&self) -> &str {
        &self.tag
    }

    #[must_use]
    pub fn claim(&self) -> LayoutClaim {
        self.claim
    }

    #[must_use]
    pub fn toolchain(&self) -> &ToolchainIdentity {
        &self.toolchain
    }

    #[must_use]
    pub fn measurement_identity(&self) -> &str {
        &self.measurement_identity
    }
}

#[derive(Clone, Debug)]
pub struct LoweringPlan {
    pub(crate) solution: TypeSolution,
    profile: TargetProfile,
    semantic_digest: SemanticDigest,
}

impl LoweringPlan {
    #[must_use]
    pub fn profile(&self) -> TargetProfile {
        self.profile
    }

    #[must_use]
    pub fn semantic_digest(&self) -> &SemanticDigest {
        &self.semantic_digest
    }
}

pub fn lower(
    solution: &TypeSolution,
    profile: TargetProfile,
    operations: &mut OperationContext,
) -> Result<LoweringPlan, TypeError> {
    for node in &solution.nodes {
        operations.charge(
            node_units(node),
            "layout.lower.semantic_node",
            &[],
            [Origin::boundary("layout.lower")],
        )?;
    }
    for row in &solution.rows {
        operations.charge(
            row.fields.len().saturating_mul(2).saturating_add(1),
            "layout.lower.semantic_row",
            &[],
            [Origin::boundary("layout.lower")],
        )?;
    }
    Ok(LoweringPlan {
        solution: solution.clone(),
        profile,
        semantic_digest: solution.semantic_digest().clone(),
    })
}

pub fn measure_carrier(
    carrier: Carrier,
    profile: LayoutProfile,
    toolchain: ToolchainIdentity,
    operations: &mut OperationContext,
) -> Result<LayoutEvidence, TypeError> {
    if profile != LayoutProfile::AppleArm64Explicit {
        return Err(operations.fail(
            TypeErrorKind::UnsupportedLayout {
                detail: "physical carriers require AppleArm64Explicit".to_owned(),
            },
            &[],
            "layout.carrier",
        ));
    }
    if toolchain.target != "aarch64-apple-darwin" {
        return Err(operations.fail(
            TypeErrorKind::UnsupportedLayout {
                detail: format!("unsupported physical target {}", toolchain.target),
            },
            &[],
            "layout.target",
        ));
    }
    for (field, value) in [
        ("target", toolchain.target.as_str()),
        ("rustc", toolchain.rustc.as_str()),
        ("typescript", toolchain.typescript.as_str()),
    ] {
        charge_text(
            operations,
            "layout.toolchain.bytes",
            value,
            &[],
            Origin::new("layout.toolchain", field),
        )?;
    }
    let (size, alignment, offsets, padding, tag, claim) = match carrier {
        Carrier::Span => (8, 4, vec![0, 4], Vec::new(), "none", LayoutClaim::Modeled),
        Carrier::ThreeWordHeader => (
            24,
            8,
            vec![0, 8, 16],
            Vec::new(),
            "none",
            LayoutClaim::Modeled,
        ),
        Carrier::PaddedByteWord => (
            16,
            8,
            vec![0, 8],
            vec![(1, 7)],
            "none",
            LayoutClaim::Modeled,
        ),
        Carrier::ExplicitTaggedU64Option => (
            16,
            8,
            vec![0, 8],
            vec![(1, 7)],
            "explicit:u8:none=0:some=1",
            LayoutClaim::Modeled,
        ),
        Carrier::NonNullPointerOption => (
            8,
            8,
            vec![0],
            Vec::new(),
            "null-pointer-niche",
            LayoutClaim::Measured,
        ),
    };
    operations.charge(
        offsets
            .len()
            .saturating_add(padding.len())
            .saturating_add(6),
        "layout.carrier.arithmetic",
        &[],
        [Origin::boundary("layout.carrier")],
    )?;
    let material = format!(
        "{carrier:?}|{size}|{alignment}|{offsets:?}|{padding:?}|{tag}|{}|{}|{}",
        toolchain.target, toolchain.rustc, toolchain.typescript
    );
    operations.charge(
        material.len().max(1),
        "layout.measurement_identity.bytes",
        &[],
        [Origin::boundary("layout.carrier")],
    )?;
    Ok(LayoutEvidence {
        carrier,
        size,
        alignment,
        offsets,
        padding,
        tag: tag.to_owned(),
        profile,
        claim,
        toolchain,
        measurement_identity: format!("fnv1a64:{:016x}", fnv1a64(material.as_bytes())),
    })
}

fn node_units(node: &SolvedNode) -> usize {
    match node {
        SolvedNode::Named(name) => name.len().saturating_add(name.chars().count()).max(1),
        SolvedNode::Tuple(items) => items.len().saturating_add(1),
        SolvedNode::Function { parameters, .. } => parameters.len().saturating_add(2),
        SolvedNode::Free { .. }
        | SolvedNode::Rec(_)
        | SolvedNode::Primitive(_)
        | SolvedNode::List(_)
        | SolvedNode::Option(_)
        | SolvedNode::Record(_)
        | SolvedNode::Variant(_)
        | SolvedNode::Mu { .. } => 1,
    }
}
