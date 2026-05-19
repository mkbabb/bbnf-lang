#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum NoFactId {}

pub trait EventGrammar: 'static {
    const STRUCTURAL_CLASS_COUNT: u8;

    type FactId: Copy + Eq + Ord + 'static;

    fn admits_fact(id: Self::FactId) -> bool;

    fn admits_class(class: u8) -> bool {
        class != 0 && class <= Self::STRUCTURAL_CLASS_COUNT
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum AnyGrammar {}

impl EventGrammar for AnyGrammar {
    const STRUCTURAL_CLASS_COUNT: u8 = 0;

    type FactId = NoFactId;

    fn admits_fact(id: Self::FactId) -> bool {
        match id {}
    }

    fn admits_class(_class: u8) -> bool {
        false
    }
}
