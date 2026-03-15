//! LSP performance benchmarks.
//!
//! Run with: cargo test -p bbnf-lsp --test bench_lsp -- --nocapture
//!
//! These tests measure latency of each LSP action on grammars of varying sizes.

use std::io::{BufReader, Read, Write};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn send_lsp(stdin: &mut impl Write, msg: &str) {
    let header = format!("Content-Length: {}\r\n\r\n", msg.len());
    stdin.write_all(header.as_bytes()).unwrap();
    stdin.write_all(msg.as_bytes()).unwrap();
    stdin.flush().unwrap();
}

fn read_lsp(stdout: &mut BufReader<impl Read>) -> String {
    let mut header_buf = Vec::new();
    loop {
        let mut byte = [0u8; 1];
        stdout.read_exact(&mut byte).unwrap();
        header_buf.push(byte[0]);
        if header_buf.ends_with(b"\r\n\r\n") {
            break;
        }
    }
    let header = String::from_utf8(header_buf).unwrap();
    let len: usize = header
        .lines()
        .find(|l| l.starts_with("Content-Length:"))
        .unwrap()
        .split(": ")
        .nth(1)
        .unwrap()
        .trim()
        .parse()
        .unwrap();
    let mut body = vec![0u8; len];
    stdout.read_exact(&mut body).unwrap();
    String::from_utf8(body).unwrap()
}

fn read_until(
    stdout: &mut BufReader<impl Read>,
    predicate: impl Fn(&str) -> bool,
    timeout: Duration,
) -> Option<String> {
    let start = Instant::now();
    loop {
        if start.elapsed() > timeout {
            return None;
        }
        let msg = read_lsp(stdout);
        if predicate(&msg) {
            return Some(msg);
        }
    }
}

fn read_response(stdout: &mut BufReader<impl Read>, id: u32) -> String {
    let needle = format!("\"id\":{}", id);
    read_until(stdout, |msg| msg.contains(&needle), Duration::from_secs(30))
        .unwrap_or_else(|| panic!("Timed out waiting for response with id {}", id))
}

fn read_until_contains(stdout: &mut BufReader<impl Read>, needle: &str) -> String {
    read_until(stdout, |msg| msg.contains(needle), Duration::from_secs(30))
        .unwrap_or_else(|| panic!("Timed out waiting for message containing '{}'", needle))
}

fn start_server() -> (impl Write, BufReader<impl Read>, std::process::Child) {
    let binary = env!("CARGO_BIN_EXE_bbnf-lsp");
    let mut child = Command::new(binary)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start bbnf-lsp");
    let stdin = child.stdin.take().unwrap();
    let stdout = BufReader::new(child.stdout.take().unwrap());
    (stdin, stdout, child)
}

fn initialize(stdin: &mut impl Write, stdout: &mut BufReader<impl Read>) {
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":"file:///tmp","capabilities":{}}}"#,
    );
    let _ = read_response(stdout, 1);
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#,
    );
}

fn open_doc(stdin: &mut impl Write, uri: &str, text: &str) {
    let escaped = text
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t");
    let msg = format!(
        r#"{{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{{"textDocument":{{"uri":"{}","languageId":"bbnf","version":1,"text":"{}"}}}}}}"#,
        uri, escaped
    );
    send_lsp(stdin, &msg);
}

fn shutdown(
    stdin: &mut impl Write,
    stdout: &mut BufReader<impl Read>,
    mut child: std::process::Child,
) {
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","id":999,"method":"shutdown","params":null}"#,
    );
    let _ = read_response(stdout, 999);
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","method":"exit","params":null}"#,
    );
    let _ = child.wait();
}

// ---------------------------------------------------------------------------
// Grammar generators
// ---------------------------------------------------------------------------

/// Generate a grammar with N rules. Each rule references the previous one.
fn generate_chain_grammar(n: usize) -> String {
    let mut lines = Vec::with_capacity(n);
    lines.push(format!("rule_0 = \"token_0\";"));
    for i in 1..n {
        lines.push(format!("rule_{} = rule_{} | \"token_{}\";", i, i - 1, i));
    }
    lines.join("\n")
}

/// Generate a grammar with N rules and wide alternations.
fn generate_wide_grammar(n: usize) -> String {
    let mut lines = Vec::with_capacity(n + 1);
    for i in 0..n {
        lines.push(format!("token_{} = \"t{}\";", i, i));
    }
    // One root rule that references all tokens.
    let refs: Vec<String> = (0..n).map(|i| format!("token_{}", i)).collect();
    lines.push(format!("root = {};", refs.join(" | ")));
    lines.join("\n")
}

/// Generate a grammar with complex nested expressions.
fn generate_nested_grammar(n: usize) -> String {
    let mut lines = Vec::with_capacity(n);
    for i in 0..n {
        // Each rule has concatenation, alternation, groups, optionals, repetition.
        lines.push(format!(
            "rule_{i} = (\"a{i}\" | \"b{i}\"), [\"opt{i}\"], {{\"rep{i}\"}}, (\"g{i}\")+;",
            i = i
        ));
    }
    // Root that references all rules.
    let refs: Vec<String> = (0..n).map(|i| format!("rule_{}", i)).collect();
    lines.push(format!("root = {};", refs.join(" | ")));
    lines.join("\n")
}

// ---------------------------------------------------------------------------
// Benchmark helpers
// ---------------------------------------------------------------------------

fn timed<F: FnOnce() -> R, R>(f: F) -> (R, Duration) {
    let start = Instant::now();
    let r = f();
    (r, start.elapsed())
}

// ---------------------------------------------------------------------------
// The benchmark
// ---------------------------------------------------------------------------

#[test]
fn bench_lsp_actions() {
    let sizes = [10, 50, 100, 500, 1000];
    let generators: Vec<(&str, Box<dyn Fn(usize) -> String>)> = vec![
        ("chain", Box::new(|n| generate_chain_grammar(n))),
        ("wide", Box::new(|n| generate_wide_grammar(n))),
        ("nested", Box::new(|n| generate_nested_grammar(n))),
    ];

    println!("\n{}", "=".repeat(100));
    println!("LSP Performance Benchmark");
    println!("{}", "=".repeat(100));

    for (gen_name, gen_fn) in &generators {
        println!("\n--- Grammar type: {} ---", gen_name);
        println!(
            "{:<8} {:>8} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}",
            "Rules", "Bytes", "Open+Diag", "Hover", "Goto Def", "Refs", "Complete", "Format", "Inlay", "SelRange"
        );

        for &n in &sizes {
            let grammar = gen_fn(n);
            let byte_count = grammar.len();
            let rule_count = grammar.lines().count();

            let (mut stdin, mut stdout, child) = start_server();
            initialize(&mut stdin, &mut stdout);

            // 1. Open + diagnostics (measures parse + analysis).
            let (_, open_dur) = timed(|| {
                open_doc(&mut stdin, "file:///bench.bbnf", &grammar);
                read_until_contains(&mut stdout, "publishDiagnostics")
            });

            // 2. Hover (midpoint rule).
            let mid_line = rule_count / 2;
            let hover_msg = format!(
                r#"{{"jsonrpc":"2.0","id":10,"method":"textDocument/hover","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"position":{{"line":{},"character":3}}}}}}"#,
                mid_line
            );
            let (_, hover_dur) = timed(|| {
                send_lsp(&mut stdin, &hover_msg);
                read_response(&mut stdout, 10)
            });

            // 3. Go-to-definition (reference in last rule).
            let last_line = rule_count - 1;
            let goto_msg = format!(
                r#"{{"jsonrpc":"2.0","id":11,"method":"textDocument/definition","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"position":{{"line":{},"character":15}}}}}}"#,
                last_line
            );
            let (_, goto_dur) = timed(|| {
                send_lsp(&mut stdin, &goto_msg);
                read_response(&mut stdout, 11)
            });

            // 4. Find references (first rule).
            let refs_msg = format!(
                r#"{{"jsonrpc":"2.0","id":12,"method":"textDocument/references","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"position":{{"line":0,"character":3}},"context":{{"includeDeclaration":true}}}}}}"#,
            );
            let (_, refs_dur) = timed(|| {
                send_lsp(&mut stdin, &refs_msg);
                read_response(&mut stdout, 12)
            });

            // 5. Completion.
            let comp_msg = format!(
                r#"{{"jsonrpc":"2.0","id":13,"method":"textDocument/completion","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"position":{{"line":{},"character":10}}}}}}"#,
                mid_line
            );
            let (_, comp_dur) = timed(|| {
                send_lsp(&mut stdin, &comp_msg);
                read_response(&mut stdout, 13)
            });

            // 6. Formatting.
            let (_, fmt_dur) = timed(|| {
                send_lsp(
                    &mut stdin,
                    r#"{"jsonrpc":"2.0","id":14,"method":"textDocument/formatting","params":{"textDocument":{"uri":"file:///bench.bbnf"},"options":{"tabSize":4,"insertSpaces":true}}}"#,
                );
                read_response(&mut stdout, 14)
            });

            // 7. Inlay hints.
            let inlay_msg = format!(
                r#"{{"jsonrpc":"2.0","id":15,"method":"textDocument/inlayHint","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"range":{{"start":{{"line":0,"character":0}},"end":{{"line":{},"character":100}}}}}}}}"#,
                rule_count
            );
            let (_, inlay_dur) = timed(|| {
                send_lsp(&mut stdin, &inlay_msg);
                read_response(&mut stdout, 15)
            });

            // 8. Selection range.
            let sel_msg = format!(
                r#"{{"jsonrpc":"2.0","id":16,"method":"textDocument/selectionRange","params":{{"textDocument":{{"uri":"file:///bench.bbnf"}},"positions":[{{"line":{},"character":5}}]}}}}"#,
                mid_line
            );
            let (_, sel_dur) = timed(|| {
                send_lsp(&mut stdin, &sel_msg);
                read_response(&mut stdout, 16)
            });

            println!(
                "{:<8} {:>8} {:>9.2}ms {:>9.2}ms {:>9.2}ms {:>9.2}ms {:>9.2}ms {:>9.2}ms {:>9.2}ms {:>9.2}ms",
                n,
                byte_count,
                open_dur.as_secs_f64() * 1000.0,
                hover_dur.as_secs_f64() * 1000.0,
                goto_dur.as_secs_f64() * 1000.0,
                refs_dur.as_secs_f64() * 1000.0,
                comp_dur.as_secs_f64() * 1000.0,
                fmt_dur.as_secs_f64() * 1000.0,
                inlay_dur.as_secs_f64() * 1000.0,
                sel_dur.as_secs_f64() * 1000.0,
            );

            shutdown(&mut stdin, &mut stdout, child);
        }
    }

    println!("\n{}", "=".repeat(100));
}

/// Benchmark incremental edit performance.
#[test]
fn bench_incremental_edits() {
    println!("\n{}", "=".repeat(80));
    println!("Incremental Edit Performance");
    println!("{}", "=".repeat(80));

    let sizes = [100, 500, 1000];

    println!(
        "{:<8} {:>8} {:>12} {:>12} {:>12}",
        "Rules", "Bytes", "Full reopen", "Incr insert", "Incr replace"
    );

    for &n in &sizes {
        let grammar = generate_chain_grammar(n);
        let byte_count = grammar.len();

        let (mut stdin, mut stdout, child) = start_server();
        initialize(&mut stdin, &mut stdout);

        // Open initial document.
        open_doc(&mut stdin, "file:///bench.bbnf", &grammar);
        read_until_contains(&mut stdout, "publishDiagnostics");

        // 1. Full text change (simulates full reopen).
        let full_change = format!(
            r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"file:///bench.bbnf","version":2}},"contentChanges":[{{"text":"{}"}}]}}}}"#,
            grammar
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
        );
        let (_, full_dur) = timed(|| {
            send_lsp(&mut stdin, &full_change);
            read_until_contains(&mut stdout, "publishDiagnostics")
        });

        // 2. Incremental insert (add a new rule at the end).
        let insert_change = format!(
            r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"file:///bench.bbnf","version":3}},"contentChanges":[{{"range":{{"start":{{"line":{},"character":0}},"end":{{"line":{},"character":0}}}},"text":"new_rule = \"inserted\";\n"}}]}}}}"#,
            n, n
        );
        let (_, incr_insert_dur) = timed(|| {
            send_lsp(&mut stdin, &insert_change);
            read_until_contains(&mut stdout, "publishDiagnostics")
        });

        // 3. Incremental replace (change a token in the middle).
        let mid = n / 2;
        let replace_change = format!(
            r#"{{"jsonrpc":"2.0","method":"textDocument/didChange","params":{{"textDocument":{{"uri":"file:///bench.bbnf","version":4}},"contentChanges":[{{"range":{{"start":{{"line":{},"character":0}},"end":{{"line":{},"character":100}}}},"text":"rule_{} = \"replaced\";"}}]}}}}"#,
            mid, mid, mid
        );
        let (_, incr_replace_dur) = timed(|| {
            send_lsp(&mut stdin, &replace_change);
            read_until_contains(&mut stdout, "publishDiagnostics")
        });

        println!(
            "{:<8} {:>8} {:>11.2}ms {:>11.2}ms {:>11.2}ms",
            n,
            byte_count,
            full_dur.as_secs_f64() * 1000.0,
            incr_insert_dur.as_secs_f64() * 1000.0,
            incr_replace_dur.as_secs_f64() * 1000.0,
        );

        shutdown(&mut stdin, &mut stdout, child);
    }

    println!("{}", "=".repeat(80));
}
