//! Pre-built gorgeous formatters (compile-time codegen, no runtime grammar compilation).

use gorgeous::PrinterConfig;
use wasm_bindgen::prelude::*;

fn make_config(max_width: usize, indent: usize, use_tabs: bool) -> PrinterConfig {
    PrinterConfig {
        max_width,
        indent,
        use_tabs,
    }
}

#[wasm_bindgen]
pub fn format_json(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::json::prettify_json(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_css(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::css::prettify_css(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_bnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::bnf::prettify_bnf(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_ebnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::ebnf::prettify_ebnf(input, &make_config(max_width, indent, use_tabs))
}

#[wasm_bindgen]
pub fn format_bbnf(
    input: &str,
    max_width: usize,
    indent: usize,
    use_tabs: bool,
) -> Option<String> {
    gorgeous::bbnf::prettify_bbnf(input, &make_config(max_width, indent, use_tabs))
}
