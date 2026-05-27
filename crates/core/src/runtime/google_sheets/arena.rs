use bbnf_ir::registry::StructLayout;
use crate::runtime::google_sheets::value::SheetsValue;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SheetsCompoundKind {
    Unknown,
    Formula,
    Expression,
    ComparisonExpr,
    ConcatExpr,
    AddExpr,
    MulExpr,
    ExpExpr,
    UnaryExpr,
    PostfixExpr,
    Primary,
    ParenExpr,
    FuncOpen,
    Arg,
    FuncArgs,
    FuncCall,
    LetBinding,
    LetArgs,
    LetCall,
    LambdaParams,
    LambdaCall,
    ArrayRow,
    ArrayRows,
    ArrayLiteral,
    Cell,
    RangeRef,
    RangeEnd,
    CellOrRange,
    ErrorLiteral,
    SheetPrefix,
    CompareOp,
    AddOp,
    MulOp,
    UnaryPrefix,
    Wrap,
}
impl SheetsCompoundKind {
    #[inline]
    pub fn is_transparent_wrap(self) -> bool {
        matches!(
            self, Self::Wrap | Self::Primary | Self::Expression | Self::RangeEnd |
            Self::CellOrRange
        )
    }
    #[inline]
    pub fn from_layout(layout: &StructLayout) -> Self {
        match layout.rule_id {
            3 => Self::ErrorLiteral,
            11 => Self::Cell,
            12 => Self::FuncOpen,
            13 => Self::RangeRef,
            14 => Self::CellOrRange,
            15 => Self::ComparisonExpr,
            16 => Self::MulExpr,
            17 => Self::UnaryExpr,
            18 => Self::ParenExpr,
            19 => Self::Arg,
            20 => Self::FuncArgs,
            21 => Self::LetBinding,
            22 => Self::LambdaParams,
            23 => Self::ArrayRow,
            24 => Self::ArrayRows,
            25 => Self::ArrayLiteral,
            26 => Self::ConcatExpr,
            27 => Self::AddExpr,
            28 => Self::ExpExpr,
            29 => Self::LambdaCall,
            31 => Self::FuncCall,
            32 => Self::LetArgs,
            33 => Self::LetCall,
            35 => Self::PostfixExpr,
            36 => Self::Formula,
            _ => Self::Wrap,
        }
    }
}
#[derive(Debug, Clone)]
pub struct SheetsCompound<'p> {
    pub kind: SheetsCompoundKind,
    pub children: Vec<SheetsValue<'p>>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SheetsCompoundId(u32);
impl SheetsCompoundId {
    pub const EMPTY: Self = Self(0);
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Default)]
pub struct SheetsArena<'p> {
    compounds: Vec<SheetsCompound<'p>>,
}
impl<'p> SheetsArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
        }
    }
    #[inline]
    pub fn push_compound(
        &mut self,
        kind: SheetsCompoundKind,
        children: Vec<SheetsValue<'p>>,
    ) -> SheetsCompoundId {
        self.compounds.push(SheetsCompound { kind, children });
        SheetsCompoundId(self.compounds.len() as u32)
    }
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        match id.slab_index() {
            None => {
                SheetsCompoundView {
                    kind: SheetsCompoundKind::Wrap,
                    children: &[],
                }
            }
            Some(i) => {
                let entry = &self.compounds[i];
                SheetsCompoundView {
                    kind: entry.kind,
                    children: entry.children.as_slice(),
                }
            }
        }
    }
    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
#[derive(Debug, Clone, Copy)]
pub struct SheetsCompoundView<'a, 'p: 'a> {
    pub kind: SheetsCompoundKind,
    pub children: &'a [SheetsValue<'p>],
}
