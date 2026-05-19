use crate::tape::EventGrammar;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct JsonEventGrammar;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct JsonFactId(u8);

impl JsonFactId {
    pub const STRING_ESCAPE_OR_CONTROL: Self = Self(0);
    pub const NUMBER_LITERAL: Self = Self(1);
    pub const LITERAL_TOKEN: Self = Self(2);
    pub const STRING_QUOTE_OWNERSHIP: Self = Self(3);
}

impl EventGrammar for JsonEventGrammar {
    const STRUCTURAL_CLASS_COUNT: u8 = 7;

    type FactId = JsonFactId;

    fn admits_fact(id: Self::FactId) -> bool {
        matches!(id.0, 0..=3)
    }
}
