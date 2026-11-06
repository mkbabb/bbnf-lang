//! Structured value expressions used by the `->` map syntax.

use serde::{Deserialize, Serialize};

use super::{StringId, TypeDesc};

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NumericDomain {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumericExprError {
    DomainMismatch,
    OutOfRange,
    Overflow,
    DivisionByZero,
    InvalidOperator,
    InvalidNegation,
    NonFinite,
}

impl NumericExprError {
    pub const fn message(self) -> &'static str {
        match self {
            Self::DomainMismatch => "numeric domains do not match",
            Self::OutOfRange => "numeric literal is out of range",
            Self::Overflow => "numeric expression overflows its domain",
            Self::DivisionByZero => "numeric expression divides by zero",
            Self::InvalidOperator => "operator is invalid for the numeric domain",
            Self::InvalidNegation => "negation is invalid for the numeric domain",
            Self::NonFinite => "floating-point expression is not finite",
        }
    }
}

impl NumericDomain {
    pub fn from_type_desc(ty: &TypeDesc) -> Option<Self> {
        match ty {
            TypeDesc::I8 => Some(Self::I8),
            TypeDesc::U8 => Some(Self::U8),
            TypeDesc::I16 => Some(Self::I16),
            TypeDesc::U16 => Some(Self::U16),
            TypeDesc::I32 => Some(Self::I32),
            TypeDesc::U32 => Some(Self::U32),
            TypeDesc::I64 => Some(Self::I64),
            TypeDesc::U64 => Some(Self::U64),
            TypeDesc::F64 => Some(Self::F64),
            _ => None,
        }
    }

    pub fn type_desc(self) -> TypeDesc {
        match self {
            Self::I8 => TypeDesc::I8,
            Self::U8 => TypeDesc::U8,
            Self::I16 => TypeDesc::I16,
            Self::U16 => TypeDesc::U16,
            Self::I32 => TypeDesc::I32,
            Self::U32 => TypeDesc::U32,
            Self::I64 => TypeDesc::I64,
            Self::U64 => TypeDesc::U64,
            Self::F64 => TypeDesc::F64,
        }
    }

    fn signed_bounds(self) -> Option<(i128, i128)> {
        match self {
            Self::I8 => Some((i8::MIN.into(), i8::MAX.into())),
            Self::I16 => Some((i16::MIN.into(), i16::MAX.into())),
            Self::I32 => Some((i32::MIN.into(), i32::MAX.into())),
            Self::I64 => Some((i64::MIN.into(), i64::MAX.into())),
            _ => None,
        }
    }

    fn unsigned_max(self) -> Option<u128> {
        match self {
            Self::U8 => Some(u8::MAX.into()),
            Self::U16 => Some(u16::MAX.into()),
            Self::U32 => Some(u32::MAX.into()),
            Self::U64 => Some(u64::MAX.into()),
            _ => None,
        }
    }
}

/// A structured value expression used in `->` map syntax.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum MapExpr {
    IntLit(i64),
    UIntLit(u64),
    FloatLit(f64),
    BoolLit(bool),
    StringLit(StringId),
    Input,
    InputProp {
        prop: StringId,
    },
    FnCall {
        name: StringId,
        args: Vec<MapExpr>,
    },
    BinOp {
        op: MapBinOp,
        operand_domain: Option<NumericDomain>,
        lhs: Box<MapExpr>,
        rhs: Box<MapExpr>,
    },
    UnaryOp {
        op: MapUnaryOp,
        operand_domain: Option<NumericDomain>,
        inner: Box<MapExpr>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    Shl,
    Shr,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Copy)]
pub enum MapUnaryOp {
    Neg,
    Not,
}

fn inferred_domain(expr: &MapExpr) -> Option<NumericDomain> {
    match expr {
        MapExpr::IntLit(_) => Some(NumericDomain::I64),
        MapExpr::UIntLit(_) => Some(NumericDomain::U64),
        MapExpr::FloatLit(_) => Some(NumericDomain::F64),
        _ => None,
    }
}

fn signed_value(expr: &MapExpr, domain: NumericDomain) -> Result<i128, NumericExprError> {
    let (min, max) = domain
        .signed_bounds()
        .ok_or(NumericExprError::DomainMismatch)?;
    let value = match expr {
        MapExpr::IntLit(value) => i128::from(*value),
        MapExpr::UIntLit(value) => i128::from(*value),
        _ => return Err(NumericExprError::DomainMismatch),
    };
    (min..=max)
        .contains(&value)
        .then_some(value)
        .ok_or(NumericExprError::OutOfRange)
}

fn unsigned_value(expr: &MapExpr, domain: NumericDomain) -> Result<u128, NumericExprError> {
    let max = domain
        .unsigned_max()
        .ok_or(NumericExprError::DomainMismatch)?;
    let value = match expr {
        MapExpr::IntLit(value) => {
            u128::try_from(*value).map_err(|_| NumericExprError::OutOfRange)?
        }
        MapExpr::UIntLit(value) => u128::from(*value),
        _ => return Err(NumericExprError::DomainMismatch),
    };
    (value <= max)
        .then_some(value)
        .ok_or(NumericExprError::OutOfRange)
}

fn float_value(expr: &MapExpr, domain: NumericDomain) -> Result<f64, NumericExprError> {
    if domain != NumericDomain::F64 {
        return Err(NumericExprError::DomainMismatch);
    }
    match expr {
        MapExpr::FloatLit(value) if value.is_finite() => Ok(*value),
        MapExpr::FloatLit(_) => Err(NumericExprError::NonFinite),
        _ => Err(NumericExprError::DomainMismatch),
    }
}

fn checked_arithmetic(
    domain: NumericDomain,
    op: MapBinOp,
    lhs: &MapExpr,
    rhs: &MapExpr,
) -> Result<MapExpr, NumericExprError> {
    if let Some((min, max)) = domain.signed_bounds() {
        let (lhs, rhs) = (signed_value(lhs, domain)?, signed_value(rhs, domain)?);
        if rhs == 0 && matches!(op, MapBinOp::Div | MapBinOp::Mod) {
            return Err(NumericExprError::DivisionByZero);
        }
        let value = match op {
            MapBinOp::Add => lhs.checked_add(rhs),
            MapBinOp::Sub => lhs.checked_sub(rhs),
            MapBinOp::Mul => lhs.checked_mul(rhs),
            MapBinOp::Div => lhs.checked_div(rhs),
            MapBinOp::Mod => lhs.checked_rem(rhs),
            _ => return Err(NumericExprError::InvalidOperator),
        }
        .filter(|value| (min..=max).contains(value))
        .ok_or(NumericExprError::Overflow)?;
        return Ok(MapExpr::IntLit(
            i64::try_from(value).expect("signed domain fits i64"),
        ));
    }
    if let Some(max) = domain.unsigned_max() {
        let (lhs, rhs) = (unsigned_value(lhs, domain)?, unsigned_value(rhs, domain)?);
        if rhs == 0 && matches!(op, MapBinOp::Div | MapBinOp::Mod) {
            return Err(NumericExprError::DivisionByZero);
        }
        let value = match op {
            MapBinOp::Add => lhs.checked_add(rhs),
            MapBinOp::Sub => lhs.checked_sub(rhs),
            MapBinOp::Mul => lhs.checked_mul(rhs),
            MapBinOp::Div => lhs.checked_div(rhs),
            MapBinOp::Mod => lhs.checked_rem(rhs),
            _ => return Err(NumericExprError::InvalidOperator),
        }
        .filter(|value| *value <= max)
        .ok_or(NumericExprError::Overflow)?;
        return Ok(MapExpr::UIntLit(
            u64::try_from(value).expect("unsigned domain fits u64"),
        ));
    }
    let (lhs, rhs) = (float_value(lhs, domain)?, float_value(rhs, domain)?);
    if rhs == 0.0 && matches!(op, MapBinOp::Div | MapBinOp::Mod) {
        return Err(NumericExprError::DivisionByZero);
    }
    let value = match op {
        MapBinOp::Add => lhs + rhs,
        MapBinOp::Sub => lhs - rhs,
        MapBinOp::Mul => lhs * rhs,
        MapBinOp::Div => lhs / rhs,
        MapBinOp::Mod => lhs % rhs,
        _ => return Err(NumericExprError::InvalidOperator),
    };
    value
        .is_finite()
        .then_some(MapExpr::FloatLit(value))
        .ok_or(NumericExprError::NonFinite)
}

fn checked_comparison(
    domain: NumericDomain,
    op: MapBinOp,
    lhs: &MapExpr,
    rhs: &MapExpr,
) -> Result<MapExpr, NumericExprError> {
    let ordering = if domain.signed_bounds().is_some() {
        signed_value(lhs, domain)?.cmp(&signed_value(rhs, domain)?)
    } else if domain.unsigned_max().is_some() {
        unsigned_value(lhs, domain)?.cmp(&unsigned_value(rhs, domain)?)
    } else {
        float_value(lhs, domain)?
            .partial_cmp(&float_value(rhs, domain)?)
            .ok_or(NumericExprError::NonFinite)?
    };
    let value = match op {
        MapBinOp::Eq => ordering.is_eq(),
        MapBinOp::Ne => !ordering.is_eq(),
        MapBinOp::Lt => ordering.is_lt(),
        MapBinOp::Gt => ordering.is_gt(),
        MapBinOp::Le => !ordering.is_gt(),
        MapBinOp::Ge => !ordering.is_lt(),
        _ => return Err(NumericExprError::InvalidOperator),
    };
    Ok(MapExpr::BoolLit(value))
}

impl MapExpr {
    pub fn for_each_child(&self, f: &mut impl FnMut(&MapExpr)) {
        match self {
            Self::IntLit(_)
            | Self::UIntLit(_)
            | Self::FloatLit(_)
            | Self::BoolLit(_)
            | Self::StringLit(_)
            | Self::Input
            | Self::InputProp { .. } => {}
            Self::FnCall { args, .. } => args.iter().for_each(f),
            Self::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Self::UnaryOp { inner, .. } => f(inner),
        }
    }

    /// Validate and fold numeric constants in one exact domain.
    pub fn checked_fold(&mut self, domain: Option<NumericDomain>) -> Result<(), NumericExprError> {
        let mut candidate = self.clone();
        candidate.checked_fold_in_place(domain)?;
        *self = candidate;
        Ok(())
    }

    fn checked_fold_in_place(
        &mut self,
        domain: Option<NumericDomain>,
    ) -> Result<(), NumericExprError> {
        match self {
            Self::FnCall { args, .. } => {
                for arg in args {
                    arg.checked_fold_in_place(None)?;
                }
            }
            Self::BinOp {
                op,
                operand_domain,
                lhs,
                rhs,
            } => {
                let domain = match (domain, *operand_domain) {
                    (Some(expected), Some(actual)) if expected != actual => {
                        return Err(NumericExprError::DomainMismatch);
                    }
                    (Some(domain), _) | (None, Some(domain)) => Some(domain),
                    (None, None) => None,
                };
                if matches!(
                    op,
                    MapBinOp::BitAnd | MapBinOp::BitOr | MapBinOp::Shl | MapBinOp::Shr
                ) {
                    return Err(NumericExprError::InvalidOperator);
                }
                let numeric_surface = matches!(
                    op,
                    MapBinOp::Add
                        | MapBinOp::Sub
                        | MapBinOp::Mul
                        | MapBinOp::Div
                        | MapBinOp::Mod
                        | MapBinOp::Lt
                        | MapBinOp::Gt
                        | MapBinOp::Le
                        | MapBinOp::Ge
                );
                if numeric_surface && domain.is_none() && (!lhs.is_constant() || !rhs.is_constant())
                {
                    return Err(NumericExprError::DomainMismatch);
                }
                if domain.is_some()
                    && !matches!(
                        op,
                        MapBinOp::Add
                            | MapBinOp::Sub
                            | MapBinOp::Mul
                            | MapBinOp::Div
                            | MapBinOp::Mod
                            | MapBinOp::Eq
                            | MapBinOp::Ne
                            | MapBinOp::Lt
                            | MapBinOp::Gt
                            | MapBinOp::Le
                            | MapBinOp::Ge
                    )
                {
                    return Err(NumericExprError::InvalidOperator);
                }
                lhs.checked_fold_in_place(domain)?;
                rhs.checked_fold_in_place(domain)?;
                let folded = match (lhs.as_ref(), rhs.as_ref()) {
                    (Self::BoolLit(lhs), Self::BoolLit(rhs)) => match op {
                        MapBinOp::And => Some(Self::BoolLit(*lhs && *rhs)),
                        MapBinOp::Or => Some(Self::BoolLit(*lhs || *rhs)),
                        MapBinOp::Eq => Some(Self::BoolLit(lhs == rhs)),
                        MapBinOp::Ne => Some(Self::BoolLit(lhs != rhs)),
                        _ => return Err(NumericExprError::InvalidOperator),
                    },
                    (Self::StringLit(lhs), Self::StringLit(rhs)) => match op {
                        MapBinOp::Eq => Some(Self::BoolLit(lhs == rhs)),
                        MapBinOp::Ne => Some(Self::BoolLit(lhs != rhs)),
                        _ => return Err(NumericExprError::InvalidOperator),
                    },
                    (
                        Self::IntLit(_) | Self::UIntLit(_) | Self::FloatLit(_),
                        Self::IntLit(_) | Self::UIntLit(_) | Self::FloatLit(_),
                    ) => {
                        let inferred_lhs = inferred_domain(lhs).expect("matched numeric literal");
                        let inferred_rhs = inferred_domain(rhs).expect("matched numeric literal");
                        let domain = match domain {
                            Some(domain) => domain,
                            None if inferred_lhs == inferred_rhs => inferred_lhs,
                            None => return Err(NumericExprError::DomainMismatch),
                        };
                        Some(
                            if matches!(
                                op,
                                MapBinOp::Eq
                                    | MapBinOp::Ne
                                    | MapBinOp::Lt
                                    | MapBinOp::Gt
                                    | MapBinOp::Le
                                    | MapBinOp::Ge
                            ) {
                                checked_comparison(domain, *op, lhs, rhs)?
                            } else {
                                checked_arithmetic(domain, *op, lhs, rhs)?
                            },
                        )
                    }
                    _ => None,
                };
                if let Some(folded) = folded {
                    *self = folded;
                }
            }
            Self::UnaryOp {
                op,
                operand_domain,
                inner,
            } => {
                let domain = match (domain, *operand_domain) {
                    (Some(expected), Some(actual)) if expected != actual => {
                        return Err(NumericExprError::DomainMismatch);
                    }
                    (Some(domain), _) | (None, Some(domain)) => Some(domain),
                    (None, None) => None,
                };
                if matches!(op, MapUnaryOp::Neg) && domain.is_none() && !inner.is_constant() {
                    return Err(NumericExprError::DomainMismatch);
                }
                if matches!(
                    (*op, domain),
                    (MapUnaryOp::Not, Some(_))
                        | (
                            MapUnaryOp::Neg,
                            Some(
                                NumericDomain::U8
                                    | NumericDomain::U16
                                    | NumericDomain::U32
                                    | NumericDomain::U64
                            )
                        )
                ) {
                    return Err(if matches!(*op, MapUnaryOp::Neg) {
                        NumericExprError::InvalidNegation
                    } else {
                        NumericExprError::InvalidOperator
                    });
                }
                inner.checked_fold_in_place(domain)?;
                let folded = match (op, inner.as_ref()) {
                    (MapUnaryOp::Not, Self::BoolLit(value)) => Some(Self::BoolLit(!value)),
                    (MapUnaryOp::Neg, Self::IntLit(_) | Self::UIntLit(_) | Self::FloatLit(_)) => {
                        let domain = domain
                            .or_else(|| inferred_domain(inner))
                            .ok_or(NumericExprError::DomainMismatch)?;
                        if domain.unsigned_max().is_some() {
                            return Err(NumericExprError::InvalidNegation);
                        }
                        if let Some((min, max)) = domain.signed_bounds() {
                            let value = signed_value(inner, domain)?
                                .checked_neg()
                                .filter(|value| (min..=max).contains(value))
                                .ok_or(NumericExprError::InvalidNegation)?;
                            Some(Self::IntLit(
                                i64::try_from(value).expect("signed domain fits i64"),
                            ))
                        } else {
                            let value = -float_value(inner, domain)?;
                            Some(
                                value
                                    .is_finite()
                                    .then_some(Self::FloatLit(value))
                                    .ok_or(NumericExprError::NonFinite)?,
                            )
                        }
                    }
                    _ => None,
                };
                if let Some(folded) = folded {
                    *self = folded;
                }
            }
            Self::IntLit(_) | Self::UIntLit(_) | Self::FloatLit(_) => {
                if let Some(domain) = domain {
                    if domain.signed_bounds().is_some() {
                        signed_value(self, domain)?;
                    } else if domain.unsigned_max().is_some() {
                        unsigned_value(self, domain)?;
                    } else {
                        float_value(self, domain)?;
                    }
                } else if let Self::FloatLit(value) = self {
                    if !value.is_finite() {
                        return Err(NumericExprError::NonFinite);
                    }
                }
            }
            Self::BoolLit(_) | Self::StringLit(_) | Self::Input | Self::InputProp { .. } => {}
        }
        Ok(())
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Self::IntLit(_)
            | Self::UIntLit(_)
            | Self::FloatLit(_)
            | Self::BoolLit(_)
            | Self::StringLit(_) => true,
            Self::Input | Self::InputProp { .. } => false,
            Self::FnCall { args, .. } => args.iter().all(Self::is_constant),
            Self::BinOp { lhs, rhs, .. } => lhs.is_constant() && rhs.is_constant(),
            Self::UnaryOp { inner, .. } => inner.is_constant(),
        }
    }
}
