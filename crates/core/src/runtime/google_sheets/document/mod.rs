pub mod canonical;
pub mod path_query;
pub mod view;
use crate::runtime::google_sheets::arena::{
    SheetsArena, SheetsCompoundId, SheetsCompoundView,
};
use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::path::Path;
pub use self::path_query::SheetsPathQuery;
pub use self::view::{SheetsKind, SheetsView};
#[derive(Debug)]
pub struct SheetsDocument<'p> {
    pub arena: SheetsArena<'p>,
    pub root: SheetsValue<'p>,
    pub input: &'p str,
}
impl<'p> SheetsDocument<'p> {
    #[inline]
    pub fn new(arena: SheetsArena<'p>, root: SheetsValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }
    #[inline]
    pub fn root(&self) -> &SheetsValue<'p> {
        &self.root
    }
    #[inline]
    pub fn arena(&self) -> &SheetsArena<'p> {
        &self.arena
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        self.arena.compound(id)
    }
    #[inline]
    pub fn view<'a>(&'a self) -> SheetsView<'a, 'p> {
        SheetsView::focused(self, self.root)
    }
    #[inline]
    pub fn to_value(&self) -> &SheetsValue<'p> {
        &self.root
    }
    #[inline]
    pub fn get<T: SheetsPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
    pub fn serialize_compact(&self) -> String {
        canonical::serialize_compact(self)
    }
}
