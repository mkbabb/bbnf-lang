//! WASM backend: emits WebAssembly Text (WAT) format.
//!
//! Produces a self-contained WASM module as WAT source.
//! Each grammar rule becomes a WASM function operating on
//! linear memory (input bytes) with i32 offset tracking.
//!
//! Execution model:
//! - Input bytes in linear memory at offset 0
//! - Each function takes (offset: i32, len: i32) → (new_offset: i32)
//! - Return -1 for failure
//! - `br_table` for dispatch-table alternations

pub mod emitter;

pub use emitter::WasmEmitter;
