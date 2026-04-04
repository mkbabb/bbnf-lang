//! WASM backend: generates WebAssembly Text (WAT) parser modules.
pub mod code;
pub mod emitter;
pub mod helpers;
pub use code::{WasmEmitCtx, WasmEmitter};
