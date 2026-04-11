//! `runtime` — types emitted directly into generated parser code.
//!
//! Tranche AB.2a introduced `Parsed<View>`; Tranche AC.1 adds
//! `ParseErr`. Together they form the public surface area of
//! every generated `Grammar::parse(input)` entry point:
//!
//! ```ignore
//! impl Grammar {
//!     pub fn parse(
//!         input: &str,
//!     ) -> Result<::bbnf::runtime::Parsed<Self>, ParseErr> { ... }
//! }
//! ```
//!
//! `Parsed<Grammar>` is marker-typed over the grammar struct itself;
//! the root view lifetime is lent by `&self` via the `Root` trait's
//! GAT `type View<'tape>` (landing in AC.2 alongside the emitter
//! rewrite).

pub mod error;
pub mod parsed;

pub use error::ParseErr;
pub use parsed::Parsed;
