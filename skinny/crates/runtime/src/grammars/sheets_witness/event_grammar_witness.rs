use crate::tape::EventGrammar;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct SheetsEventGrammar;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct SheetsFactId(u8);

impl SheetsFactId {
    pub const FORMULA_PREFIX: Self = Self(0);
    pub const QUOTED_STRING: Self = Self(1);
    pub const ESCAPED_QUOTE: Self = Self(2);
}

impl EventGrammar for SheetsEventGrammar {
    const STRUCTURAL_CLASS_COUNT: u8 = 5;

    type FactId = SheetsFactId;

    fn admits_fact(id: Self::FactId) -> bool {
        matches!(id.0, 0..=2)
    }
}
