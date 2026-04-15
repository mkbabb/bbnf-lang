use biome_css_parser::{CssParserOptions, parse_css};
use biome_css_formatter::{context::CssFormatOptions, format_node};

// AV.0.11 Category A — output-size dump test, not an assertion. Iterates
// over fixtures including `tailwind-output.css` and `app.css` which are
// not checked in. Forward-ticketed alongside `biome_compare` visualisation
// tests.
#[ignore = "AV.0.11 Category A: dump/visualisation test; relies on non-checked-in data/css/{tailwind-output,app}.css fixtures. Forward-ticketed to gorgeous visualisation-fixtures audit."]
#[test]
fn output_size_comparison() {
    for (name, path) in [
        ("bootstrap", "data/css/bootstrap.css"),
        ("tailwind", "data/css/tailwind-output.css"),
        ("app", "data/css/app.css"),
    ] {
        let input = std::fs::read_to_string(path).unwrap();

        let parsed = parse_css(&input, CssParserOptions::default());
        let biome_out = format_node(CssFormatOptions::default(), &parsed.syntax())
            .unwrap().print().unwrap().into_code();

        let gorg_out = gorgeous::css::prettify_css(&input, &gorgeous::PrinterConfig::default()).unwrap();

        eprintln!("=== {} ===", name);
        eprintln!("  Input:    {:>9} bytes, {:>6} lines", input.len(), input.lines().count());
        eprintln!("  Biome:    {:>9} bytes, {:>6} lines", biome_out.len(), biome_out.lines().count());
        eprintln!("  Gorgeous: {:>9} bytes, {:>6} lines", gorg_out.len(), gorg_out.lines().count());
        eprintln!("  Output ratio (gorg/biome): {:.2}x", gorg_out.len() as f64 / biome_out.len() as f64);
        eprintln!();
    }
}
