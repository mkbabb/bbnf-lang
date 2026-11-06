use std::collections::BTreeMap;

use crate::{SolvedTail, SolvedType};

/// A deliberately small grammar-result algebra. It tests whether equality and
/// rows are sufficient before adding subtyping or effects.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum GrammarShape {
    Scalar,
    Unit,
    Sequence(Vec<GrammarShape>),
    Repeated(Box<GrammarShape>),
    Optional(Box<GrammarShape>),
    Product(BTreeMap<String, GrammarShape>),
    Choice(BTreeMap<String, GrammarShape>),
    Recursive(Box<GrammarShape>),
    Function,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ShapeMismatch {
    pub semantic: String,
    pub expected: GrammarShape,
}

impl GrammarShape {
    pub fn from_semantic(ty: &SolvedType) -> Result<Self, ShapeMismatch> {
        Ok(match ty {
            SolvedType::Meta(_) | SolvedType::Bound(_) => {
                return Err(ShapeMismatch {
                    semantic: ty.render(),
                    expected: Self::Scalar,
                });
            }
            SolvedType::Primitive(crate::Primitive::Unit) => Self::Unit,
            SolvedType::Primitive(_) => Self::Scalar,
            SolvedType::Tuple(items) => Self::Sequence(
                items
                    .iter()
                    .map(Self::from_semantic)
                    .collect::<Result<_, _>>()?,
            ),
            SolvedType::List(item) => Self::Repeated(Box::new(Self::from_semantic(item)?)),
            SolvedType::Option(item) => Self::Optional(Box::new(Self::from_semantic(item)?)),
            SolvedType::Function { .. } => Self::Function,
            SolvedType::Record(row) => {
                if row.tail != SolvedTail::Closed {
                    return Err(ShapeMismatch {
                        semantic: ty.render(),
                        expected: Self::Product(BTreeMap::new()),
                    });
                }
                Self::Product(
                    row.fields
                        .iter()
                        .map(|(label, ty)| Ok((label.clone(), Self::from_semantic(ty)?)))
                        .collect::<Result<_, ShapeMismatch>>()?,
                )
            }
            SolvedType::Variant(row) => {
                if row.tail != SolvedTail::Closed {
                    return Err(ShapeMismatch {
                        semantic: ty.render(),
                        expected: Self::Choice(BTreeMap::new()),
                    });
                }
                Self::Choice(
                    row.fields
                        .iter()
                        .map(|(label, ty)| Ok((label.clone(), Self::from_semantic(ty)?)))
                        .collect::<Result<_, ShapeMismatch>>()?,
                )
            }
            SolvedType::Mu(body) => Self::Recursive(Box::new(Self::from_semantic(body)?)),
        })
    }

    pub fn check(ty: &SolvedType, expected: &Self) -> Result<(), ShapeMismatch> {
        let actual = Self::from_semantic(ty)?;
        if &actual == expected {
            Ok(())
        } else {
            Err(ShapeMismatch {
                semantic: ty.render(),
                expected: expected.clone(),
            })
        }
    }
}
