//! TypeScript backend: generates self-contained TS parser source.
pub mod code;
pub mod emitter;
pub mod helpers;
pub use code::{TsCode, TsEmitCtx, TsEmitter};
