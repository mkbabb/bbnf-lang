use crate::runtime::google_sheets::arena::SheetsCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SheetsValue<'p> {
    Number(f64),
    String(&'p str),
    Bool(bool),
    Error(u8),
    CellRef(&'p str),
    Identifier(&'p str),
    SheetPrefix { tag: u8, text: &'p str },
    Tag(u8),
    Compound(SheetsCompoundId),
}
impl<'p> SheetsValue<'p> {
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, SheetsValue::Number(_))
    }
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self, SheetsValue::String(_))
    }
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self, SheetsValue::Compound(_))
    }
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            SheetsValue::Number(n) => Some(n),
            _ => None,
        }
    }
    #[inline]
    pub fn as_str(&self) -> Option<&'p str> {
        match *self {
            SheetsValue::String(s)
            | SheetsValue::CellRef(s)
            | SheetsValue::Identifier(s)
            | SheetsValue::SheetPrefix { text: s, .. } => Some(s),
            _ => None,
        }
    }
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match *self {
            SheetsValue::Bool(b) => Some(b),
            _ => None,
        }
    }
    #[inline]
    pub fn as_u8(&self) -> Option<u8> {
        match *self {
            SheetsValue::Tag(t) | SheetsValue::Error(t) => Some(t),
            SheetsValue::SheetPrefix { tag, .. } => Some(tag),
            _ => None,
        }
    }
}
