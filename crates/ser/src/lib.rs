//! Grammar-guided serialization and deserialization traits.
//!
//! [`Serializer`] is the inverse of parsing: where a grammar-derived parser
//! reads text into typed values, a serializer walks those values and writes
//! text back out. The trait is grammar-agnostic — codegen produces the
//! traversal, the serializer controls the output format.
//!
//! [`Deserializer`] is the inverse of serialization: type-guided reading
//! of structured text back into typed values.
//!
//! ## Implementations
//!
//! - [`WriterSerializer`]: minimal bytes to any `io::Write`
//! - [`StringSerializer`]: compact output into owned `String`
//! - `FmtBuilder` (in pprint): pretty-printed via Wadler-Lindig algorithm
//! - [`SliceDeserializer`]: reads from `&'a str` input

pub mod slice;
pub mod string;
pub mod traits;
pub mod writer;

pub use slice::SliceDeserializer;
pub use string::StringSerializer;
pub use traits::{Deserializer, Serializer};
pub use writer::WriterSerializer;
