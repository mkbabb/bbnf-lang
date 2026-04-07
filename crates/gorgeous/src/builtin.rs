use std::path::Path;

use gorgeous::PrinterConfig;

pub fn detect_language(path: &str) -> Option<&'static str> {
    match Path::new(path).extension()?.to_str()? {
        "json" | "jsonc" | "json5" => Some("json"),
        "css" | "scss" => Some("css"),
        "ebnf" => Some("ebnf"),
        "bnf" => Some("bnf"),
        "bbnf" => Some("bbnf"),
        _ => None,
    }
}

pub fn format_builtin(lang: &str, input: &str, config: &PrinterConfig) -> Result<String, String> {
    let result = match lang {
        "json" => gorgeous::json::prettify_json(input, config),
        "css" => gorgeous::css::prettify_css(input, config),
        "ebnf" => gorgeous::ebnf::prettify_ebnf(input, config),
        "bnf" => gorgeous::bnf::prettify_bnf(input, config),
        "bbnf" => gorgeous::bbnf::prettify_bbnf(input, config),
        _ => return Err(format!("unknown language: {lang}")),
    };
    result.ok_or_else(|| "parse error".to_string())
}
