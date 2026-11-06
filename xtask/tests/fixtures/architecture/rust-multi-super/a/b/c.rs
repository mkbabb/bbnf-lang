use super::super::x::Thing;
pub use super::super::x::Thing as Reexport;

pub fn qualified(_: super::super::x::Thing) -> Thing {
    Thing
}

pub mod d {
    pub fn lexical() {
        use super::super::super::x::Thing;
        let _ = Thing;
    }
}
