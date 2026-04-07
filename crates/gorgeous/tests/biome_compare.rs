use biome_css_parser::{CssParserOptions, parse_css};
use biome_css_formatter::{context::CssFormatOptions, format_node};

fn biome_format(input: &str) -> String {
    let parsed = parse_css(input, CssParserOptions::default());
    let options = CssFormatOptions::default();
    let formatted = format_node(options, &parsed.syntax()).unwrap();
    formatted.print().unwrap().into_code()
}

#[test]
fn dump_biome_vs_gorgeous() {
    let bootstrap = std::fs::read_to_string("data/css/bootstrap.css").unwrap();

    let biome_out = biome_format(&bootstrap);
    let gorg_out = gorgeous::css::prettify_css(&bootstrap, &gorgeous::PrinterConfig::default()).unwrap();

    // Print sizes
    eprintln!("=== SIZES ===");
    eprintln!("Input:    {} bytes, {} lines", bootstrap.len(), bootstrap.lines().count());
    eprintln!("Biome:    {} bytes, {} lines", biome_out.len(), biome_out.lines().count());
    eprintln!("Gorgeous: {} bytes, {} lines", gorg_out.len(), gorg_out.lines().count());

    // Print first 80 lines of each
    eprintln!("\n=== BIOME (first 60 lines) ===");
    for line in biome_out.lines().take(60) {
        eprintln!("{}", line);
    }
    eprintln!("\n=== GORGEOUS (first 60 lines) ===");
    for line in gorg_out.lines().take(60) {
        eprintln!("{}", line);
    }

    // Find a complex selector to compare
    eprintln!("\n=== BIOME (lines 200-250) ===");
    for line in biome_out.lines().skip(200).take(50) {
        eprintln!("{}", line);
    }
    eprintln!("\n=== GORGEOUS (lines 200-250) ===");
    for line in gorg_out.lines().skip(200).take(50) {
        eprintln!("{}", line);
    }
}

#[test]
fn dump_tailwind_comparison() {
    let tailwind = std::fs::read_to_string("data/css/tailwind-output.css").unwrap();

    // Check structure
    eprintln!("=== TAILWIND INPUT ===");
    eprintln!("Size: {} bytes, {} lines", tailwind.len(), tailwind.lines().count());

    // Count rule types
    let at_rules = tailwind.matches('@').count();
    let open_braces = tailwind.matches('{').count();
    let semicolons = tailwind.matches(';').count();
    eprintln!("@ rules: {}, braces: {}, semicolons: {}", at_rules, open_braces, semicolons);

    // Show some lines to understand structure
    eprintln!("\n=== TAILWIND (first 30 lines) ===");
    for line in tailwind.lines().take(30) {
        eprintln!("{}", line);
    }

    // Show middle section
    eprintln!("\n=== TAILWIND (lines 1000-1030) ===");
    for line in tailwind.lines().skip(1000).take(30) {
        eprintln!("{}", line);
    }

    // Average line length
    let total_chars: usize = tailwind.lines().map(|l| l.len()).sum();
    let line_count = tailwind.lines().count();
    eprintln!("\nAvg line length: {} chars", total_chars / line_count.max(1));

    // Count unique selectors vs repetitive
    let short_rules: usize = tailwind.lines().filter(|l| l.starts_with('.') && l.len() < 30).count();
    eprintln!("Short selector lines: {}", short_rules);
}
