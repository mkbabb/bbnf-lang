//! TypeScript backend: emits self-contained recursive descent parser source code.
//!
//! Produces direct TS functions with zero runtime dependencies.
//! Uses discriminated unions for grammar enums, `T | null` for optionals,
//! `T[]` for vectors, `switch` for dispatch tables.

pub mod emitter;

pub use emitter::TsEmitter;
