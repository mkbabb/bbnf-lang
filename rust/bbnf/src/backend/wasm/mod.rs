//! WASM backend: generates WebAssembly Text (WAT) parser modules.
pub mod code;
pub mod emitter;
pub mod helpers;

mod alt;
mod dispatch;
mod repeat;
mod ws;

pub use code::{WasmEmitCtx, WasmEmitter};
