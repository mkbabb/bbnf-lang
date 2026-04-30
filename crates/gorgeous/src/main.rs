mod builtin;

use std::io::{self, BufWriter, Read, Write};

use gorgeous::PrinterConfig;

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

fn usage() -> ! {
    eprintln!("Usage: gorg [OPTIONS] <FILE>");
    eprintln!();
    eprintln!("  BUILT-IN   gorg input.json");
    eprintln!("  STDIN      echo '{{}}' | gorg --lang json");
    eprintln!("  WRITE      gorg input.css -o output.css");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -l, --lang <LANG>       Language: json, css, ebnf, bnf, bbnf");
    eprintln!("  -w, --width <N>         Max line width (default: 80)");
    eprintln!("  -i, --indent <N>        Indent size (default: 4)");
    eprintln!("  --tabs                  Use tabs instead of spaces");
    eprintln!("  -o, --output <FILE>     Write to file instead of stdout");
    std::process::exit(1);
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.is_empty() {
        usage();
    }

    let mut lang: Option<String> = None;
    let mut width: usize = 80;
    let mut indent: usize = 4;
    let mut use_tabs = false;
    let mut output_path: Option<String> = None;
    let mut input_path: Option<String> = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-l" | "--lang" => {
                i += 1;
                lang = Some(args.get(i).unwrap_or_else(|| usage()).clone());
            }
            "-w" | "--width" => {
                i += 1;
                width = args
                    .get(i)
                    .unwrap_or_else(|| usage())
                    .parse()
                    .unwrap_or_else(|_| usage());
            }
            "-i" | "--indent" => {
                i += 1;
                indent = args
                    .get(i)
                    .unwrap_or_else(|| usage())
                    .parse()
                    .unwrap_or_else(|_| usage());
            }
            "--tabs" => use_tabs = true,
            "-o" | "--output" => {
                i += 1;
                output_path = Some(args.get(i).unwrap_or_else(|| usage()).clone());
            }
            "-h" | "--help" => usage(),
            s if s.starts_with('-') => {
                eprintln!("unknown option: {s}");
                usage();
            }
            _ => input_path = Some(args[i].clone()),
        }
        i += 1;
    }

    let config = PrinterConfig {
        max_width: width,
        indent,
        use_tabs,
    };

    // Built-in mode
    let input = match &input_path {
        Some(path) => std::fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("error reading {path}: {e}");
            std::process::exit(1);
        }),
        None => {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf).unwrap_or_else(|e| {
                eprintln!("error reading stdin: {e}");
                std::process::exit(1);
            });
            buf
        }
    };

    let lang = lang
        .as_deref()
        .or_else(|| input_path.as_deref().and_then(builtin::detect_language))
        .unwrap_or_else(|| {
            eprintln!("cannot detect language — use --lang");
            std::process::exit(1);
        });

    match builtin::format_builtin(lang, &input, &config) {
        Ok(output) => emit(&output, output_path.as_deref()),
        Err(e) => {
            eprintln!("gorg: {e}");
            std::process::exit(1);
        }
    }
}

fn emit(output: &str, path: Option<&str>) {
    if let Some(path) = path {
        std::fs::write(path, output).unwrap_or_else(|e| {
            eprintln!("error writing {path}: {e}");
            std::process::exit(1);
        });
    } else {
        let stdout = io::stdout().lock();
        let mut writer = BufWriter::new(stdout);
        writer.write_all(output.as_bytes()).unwrap();
    }
}
