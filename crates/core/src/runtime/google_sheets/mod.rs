//! AZ-I.W2-act.B2 — Google Sheets struct-direct runtime.
//!
//! `crates/core/src/runtime/google_sheets/` is the typed-struct
//! runtime for the Google Sheets formula grammar. The generated parser
//! writes directly into a [`SheetsDocument`] graph via the
//! [`SheetsStructBuilder`] consumer of the
//! [`crate::runtime::builder::StructBuilder`] trait — the tape
//! substrate is severed on the Sheets parse path post-orchestrator
//! regen; no `TapeBuilder` / `TapeRec` / `TapeCursor` symbol appears
//! in this module's transitive code.
//!
//! # Module layout
//!
//! - [`value`]    — typed [`SheetsValue`] sum closing every leaf and
//!   compound projection of `grammar/google-sheets/google-sheets.bbnf`.
//! - [`arena`]    — the [`SheetsArena`] owning slab for compound
//!   children plus the [`SheetsCompoundKind`] structural-kind
//!   discriminator.
//! - [`builder`]  — the [`SheetsStructBuilder`] concrete
//!   `StructBuilder` impl.
//! - [`document`] — the [`SheetsDocument`] root + the [`SheetsView`]
//!   newtype + [`SheetsPathQuery`] trait.
//!
//! # Wire contract
//!
//! `bbnf::grammar::generated::google_sheets::GoogleSheetsParser::parse(src)`
//! returns a [`SheetsDocument<'_>`] borrowing from `src`'s lifetime
//! once the orchestrator-owned regen flips the resolver's
//! `GoogleSheetsParser` arm to `EmitStrategy::StructDirect`. The
//! grammar's typed `->` annotations close as follows:
//!
//! - `number = /…/ -> f64` → [`SheetsValue::Number`]
//! - `string = /…/ -> input : Span` → [`SheetsValue::String`]
//! - `boolean = … -> true | … -> false` → [`SheetsValue::Bool`]
//! - `error_literal = … -> Nu8` → [`SheetsValue::Error`]
//! - `sheet_prefix = … -> Nu8` → [`SheetsValue::SheetPrefix`]
//! - `cell_ref = /…/ -> input : Span` → [`SheetsValue::CellRef`]
//! - `identifier = /…/ -> input : Span` → [`SheetsValue::Identifier`]
//! - `compare_op` / `add_op` / `mul_op` / `unary_prefix -> Nu8` →
//!   [`SheetsValue::Tag`]
//! - All compound rules → [`SheetsValue::Compound`]

pub mod arena;
pub mod builder;
pub mod document;
pub mod value;
pub mod view;

pub use arena::{SheetsArena, SheetsCompound, SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView};
pub use builder::SheetsStructBuilder;
pub use document::{SheetsDocument, SheetsKind, SheetsPathQuery, SheetsView};
pub use value::SheetsValue;
