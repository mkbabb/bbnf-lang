/// The structural kind a tape cursor projects to, recovered lazily from the
/// sparse flag plane at the cursor's offset. One variant per grammar rule —
/// derived from the program's rule names, no shared structural vocabulary.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum GoogleSheetsKind {
    Number,
    String,
    Boolean,
    ErrorLiteral,
    SheetPrefix,
    CellRef,
    RangeEnd,
    Cell,
    RangeRef,
    CellOrRange,
    Identifier,
    CompareOp,
    ComparisonExpr,
    ConcatExpr,
    AddOp,
    AddExpr,
    MulOp,
    MulExpr,
    ExpExpr,
    UnaryPrefix,
    UnaryExpr,
    PostfixExpr,
    Primary,
    ParenExpr,
    FuncOpen,
    Arg,
    Comma,
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
    Expression,
    Formula,
    NamedRange,
}

/// The 1-based tag table: a recorded tape flag value decodes to its rule
/// kind by this table (`tag - 1` indexes the rule order).
const KIND_TABLE: &[GoogleSheetsKind] = &[
    GoogleSheetsKind::Number,
    GoogleSheetsKind::String,
    GoogleSheetsKind::Boolean,
    GoogleSheetsKind::ErrorLiteral,
    GoogleSheetsKind::SheetPrefix,
    GoogleSheetsKind::CellRef,
    GoogleSheetsKind::RangeEnd,
    GoogleSheetsKind::Cell,
    GoogleSheetsKind::RangeRef,
    GoogleSheetsKind::CellOrRange,
    GoogleSheetsKind::Identifier,
    GoogleSheetsKind::CompareOp,
    GoogleSheetsKind::ComparisonExpr,
    GoogleSheetsKind::ConcatExpr,
    GoogleSheetsKind::AddOp,
    GoogleSheetsKind::AddExpr,
    GoogleSheetsKind::MulOp,
    GoogleSheetsKind::MulExpr,
    GoogleSheetsKind::ExpExpr,
    GoogleSheetsKind::UnaryPrefix,
    GoogleSheetsKind::UnaryExpr,
    GoogleSheetsKind::PostfixExpr,
    GoogleSheetsKind::Primary,
    GoogleSheetsKind::ParenExpr,
    GoogleSheetsKind::FuncOpen,
    GoogleSheetsKind::Arg,
    GoogleSheetsKind::Comma,
    GoogleSheetsKind::FuncArgs,
    GoogleSheetsKind::FuncCall,
    GoogleSheetsKind::LetBinding,
    GoogleSheetsKind::LetArgs,
    GoogleSheetsKind::LetCall,
    GoogleSheetsKind::LambdaParams,
    GoogleSheetsKind::LambdaCall,
    GoogleSheetsKind::ArrayRow,
    GoogleSheetsKind::ArrayRows,
    GoogleSheetsKind::ArrayLiteral,
    GoogleSheetsKind::Expression,
    GoogleSheetsKind::Formula,
    GoogleSheetsKind::NamedRange,
];

impl GoogleSheetsKind {
    /// Decode the kind at a tape cursor from the sparse flag plane. A cursor with
    /// no flag (tag 0) — a bare structural offset — has no rule kind.
    #[inline]
    pub fn at_cursor(tape: &Tape<'_>, cursor: u32) -> Option<GoogleSheetsKind> {
        let tag = tape.flags_at(cursor)?.bits();
        if tag == 0 {
            return None;
        }
        KIND_TABLE.get((tag - 1) as usize).copied()
    }
}
