//! WASM bindings for BBNF analysis, LSP features, gorgeous formatting, and bytecode VM.

#![allow(non_snake_case)]

mod analysis;
mod gorgeous;
mod lsp;
mod vm;

use serde::Serialize;
use wasm_bindgen::prelude::*;

/// Initialize panic hook for better error messages in WASM.
#[wasm_bindgen(start)]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

// ── Shared helpers ──────────────────────────────────────────────────────────

/// Helper: serialize to JsValue, returning JsValue::NULL on failure.
pub(crate) fn to_js_value<T: Serialize>(value: &T) -> JsValue {
    serde_wasm_bindgen::to_value(value).unwrap_or(JsValue::NULL)
}

// ── Shared conversion types ─────────────────────────────────────────────────

#[derive(Serialize, Clone)]
pub(crate) struct WasmRange {
    start_line: u32,
    start_character: u32,
    end_line: u32,
    end_character: u32,
}

pub(crate) fn range_to_wasm(r: &ls_types::Range) -> WasmRange {
    WasmRange {
        start_line: r.start.line,
        start_character: r.start.character,
        end_line: r.end.line,
        end_character: r.end.character,
    }
}
