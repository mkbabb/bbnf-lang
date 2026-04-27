//! TypeScript backend: generates self-contained TS parser source.
mod alt;
pub mod code;
mod dispatch;
pub mod emitter;
pub mod projection;
mod repeat;
mod ws;
pub use code::{TsCode, TsEmitCtx, TsEmitter};
