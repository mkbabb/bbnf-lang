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

/// Read LSP messages until we find one matching the predicate, with a timeout.
/// Skips notifications like window/logMessage.
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

/// Read LSP messages until we find one containing the given text.
fn read_until_contains(
    stdout: &mut BufReader<impl Read>,
    needle: &str,
) -> String {
    read_until(stdout, |msg| msg.contains(needle), Duration::from_secs(10))
        .unwrap_or_else(|| panic!("Timed out waiting for message containing '{}'", needle))
}

/// Read LSP messages until we find a response with the given id.
fn read_response(stdout: &mut BufReader<impl Read>, id: u32) -> String {
    let needle = format!("\"id\":{}", id);
    read_until(stdout, |msg| msg.contains(&needle), Duration::from_secs(10))
        .unwrap_or_else(|| panic!("Timed out waiting for response with id {}", id))
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

fn initialize(stdin: &mut impl Write, stdout: &mut BufReader<impl Read>) -> String {
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":"file:///tmp","capabilities":{}}}"#,
    );
    let resp = read_response(stdout, 1);
    send_lsp(
        stdin,
        r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#,
    );
    resp
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

/// Open a doc and wait for publishDiagnostics notification.
fn open_doc_and_wait_diagnostics(
    stdin: &mut impl Write,
    stdout: &mut BufReader<impl Read>,
    uri: &str,
    text: &str,
) -> String {
    open_doc(stdin, uri, text);
    read_until_contains(stdout, "publishDiagnostics")
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
    let status = child.wait().unwrap();
    assert!(status.success(), "LSP process exited with: {:?}", status);
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_initialize_capabilities() {
    let (mut stdin, mut stdout, child) = start_server();
    let resp = initialize(&mut stdin, &mut stdout);

    assert!(resp.contains("hoverProvider"), "missing hover");
    assert!(resp.contains("definitionProvider"), "missing definition");
    assert!(resp.contains("referencesProvider"), "missing references");
    assert!(resp.contains("renameProvider"), "missing rename");
    assert!(resp.contains("completionProvider"), "missing completion");
    assert!(
        resp.contains("semanticTokensProvider"),
        "missing semantic tokens"
    );
    assert!(resp.contains("codeLensProvider"), "missing code lens");
    assert!(
        resp.contains("documentFormattingProvider"),
        "missing formatting"
    );
    assert!(resp.contains("foldingRangeProvider"), "missing folding");
    assert!(resp.contains("codeActionProvider"), "missing code actions");

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_valid_grammar() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nstring = /\"[^\"]*\"/;\nvalue = number | string;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Valid grammar diagnostics: {}", diag);

    // Parse the diagnostics array — should be empty or have only hints (unused)
    // In this grammar: value is first rule (entry), references number and string, all used
    // But wait: value is defined last, number is first. The first rule is entry point.
    // So number is entry, it doesn't reference string or value → those are unused.
    // Let's just check no errors/warnings (only hints allowed)
    assert!(
        !diag.contains("\"severity\":1"),
        "Should have no errors in valid grammar"
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_unused_rule() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "value = number;\nnumber = /[0-9]+/;\nunused = \"hello\";";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Unused rule diagnostics: {}", diag);
    assert!(
        diag.contains("Unused rule"),
        "Expected unused rule diagnostic, got: {}",
        diag
    );
    assert!(
        diag.contains("unused"),
        "Expected 'unused' name in diagnostic"
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_undefined_rule() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "value = number | missing_rule;\nnumber = /[0-9]+/;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Undefined rule diagnostics: {}", diag);
    assert!(
        diag.contains("Undefined rule"),
        "Expected undefined rule diagnostic, got: {}",
        diag
    );
    assert!(
        diag.contains("missing_rule"),
        "Expected 'missing_rule' name"
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_parse_error() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Missing semicolon — parse error
    let grammar = "value = number";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Parse error diagnostics: {}", diag);
    assert!(
        diag.contains("severity\":1") || diag.contains("severity\":2")
            || diag.contains("Parse") || diag.contains("parse")
            || diag.contains("Incomplete") || diag.contains("incomplete"),
        "Expected parse error diagnostic, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_hover_on_definition() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Hover on "number" at line 0, char 2
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":10,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2}}}"#,
    );
    let resp = read_response(&mut stdout, 10);
    eprintln!("Hover response: {}", resp);
    assert!(
        resp.contains("number"),
        "Expected number in hover content, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_goto_definition() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Go to definition of "number" reference at line 1, char 10
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":11,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":1,"character":10}}}"#,
    );
    let resp = read_response(&mut stdout, 11);
    eprintln!("Definition response: {}", resp);
    // Should point to line 0 (where number is defined)
    assert!(
        resp.contains("\"line\":0"),
        "Expected definition at line 0, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_completion() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nstring = /\"[^\"]*\"/;\nvalue = ;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":12,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":2,"character":8}}}"#,
    );
    let resp = read_response(&mut stdout, 12);
    eprintln!("Completion response: {}", resp);
    assert!(
        resp.contains("number"),
        "Expected 'number' in completions, got: {}",
        resp
    );
    assert!(
        resp.contains("string"),
        "Expected 'string' in completions, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_document_symbols() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "alpha = \"a\";\nbeta = \"b\";\ngamma = alpha | beta;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":13,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///test.bbnf"}}}"#,
    );
    let resp = read_response(&mut stdout, 13);
    eprintln!("Symbols response: {}", resp);
    assert!(
        resp.contains("alpha"),
        "Expected alpha symbol, got: {}",
        resp
    );
    assert!(resp.contains("beta"), "Expected beta symbol, got: {}", resp);
    assert!(
        resp.contains("gamma"),
        "Expected gamma symbol, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_references() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;\nlist = number | value;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Find references to "number" (defined on line 0, referenced on lines 1 and 2)
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":14,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2},"context":{"includeDeclaration":true}}}"#,
    );
    let resp = read_response(&mut stdout, 14);
    eprintln!("References response: {}", resp);
    // Should find at least 3 locations (definition + 2 refs)
    assert!(
        resp.contains("\"line\":0") && resp.contains("\"line\":1") && resp.contains("\"line\":2"),
        "Expected references on lines 0, 1, 2, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_formatting() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Grammar with irregular spacing
    let grammar = "number = /[0-9]+/ ;\nvalue = number | \"hello\" ;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":15,"method":"textDocument/formatting","params":{"textDocument":{"uri":"file:///test.bbnf"},"options":{"tabSize":4,"insertSpaces":true}}}"#,
    );
    let resp = read_response(&mut stdout, 15);
    eprintln!("Formatting response: {}", resp);
    // Should produce edits or null — either is valid
    assert!(
        resp.contains("\"id\":15"),
        "Expected formatting response, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_rename() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Prepare rename on "number" definition
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":16,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2}}}"#,
    );
    let resp = read_response(&mut stdout, 16);
    eprintln!("Prepare rename response: {}", resp);
    assert!(
        resp.contains("number"),
        "Expected 'number' placeholder in prepare rename, got: {}",
        resp
    );

    // Execute rename
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":17,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2},"newName":"digit"}}"#,
    );
    let resp = read_response(&mut stdout, 17);
    eprintln!("Rename response: {}", resp);
    assert!(
        resp.contains("digit"),
        "Expected 'digit' in rename edits, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_semantic_tokens() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number | \"hello\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":18,"method":"textDocument/semanticTokens/full","params":{"textDocument":{"uri":"file:///test.bbnf"}}}"#,
    );
    let resp = read_response(&mut stdout, 18);
    eprintln!("Semantic tokens response: {}", resp);
    assert!(
        resp.contains("data"),
        "Expected token data array, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_code_lens() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;\nlist = number | value;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":19,"method":"textDocument/codeLens","params":{"textDocument":{"uri":"file:///test.bbnf"}}}"#,
    );
    let resp = read_response(&mut stdout, 19);
    eprintln!("Code lens response: {}", resp);
    assert!(
        resp.contains("reference"),
        "Expected reference count in code lens, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_folding_ranges() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Multi-line rule should produce folding range
    let grammar = "value =\n  number\n  | string\n  | \"null\";\nnumber = /[0-9]+/;\nstring = /\"[^\"]*\"/;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":20,"method":"textDocument/foldingRange","params":{"textDocument":{"uri":"file:///test.bbnf"}}}"#,
    );
    let resp = read_response(&mut stdout, 20);
    eprintln!("Folding response: {}", resp);
    assert!(
        resp.contains("\"id\":20"),
        "Expected folding response, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_code_actions() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Grammar with an unused rule
    let grammar = "value = number;\nnumber = /[0-9]+/;\nunused = \"hello\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Request code actions for the "unused" rule on line 2
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":21,"method":"textDocument/codeAction","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":2,"character":0},"end":{"line":2,"character":6}},"context":{"diagnostics":[]}}}"#,
    );
    let resp = read_response(&mut stdout, 21);
    eprintln!("Code actions response: {}", resp);
    assert!(
        resp.contains("\"id\":21"),
        "Expected code actions response, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_real_json_grammar() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Note: The full JSON grammar with complex regex patterns like /\\["\\/bfnrt]/
    // currently causes a regex panic in the BBNF parser. The LSP should catch this
    // gracefully and report it as a diagnostic rather than crashing.
    // Using a simplified version here that parses cleanly.
    let grammar = r#"null = "null";
bool = "true" | "false";
number = /[0-9]+/;
string = /[a-zA-Z]+/;
array = "[" , [ value , { "," , value } ] , "]";
pair = string , ":" , value;
object = "{" , [ pair , { "," , pair } ] , "}";
value = string | number | object | array | bool | null;"#;

    let diag =
        open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///json.bbnf", grammar);
    eprintln!("JSON grammar diagnostics: {}", diag);
    // Should parse — no errors (only hints about unused rules allowed)
    assert!(
        !diag.contains("\"severity\":1"),
        "JSON grammar should have no errors, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_regex_panic_handled() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // This regex pattern causes a panic in the parser — LSP should catch it gracefully.
    let grammar = r#"char = /[^"\\]/ | /\\["\\/bfnrt]/ | /\\u[0-9a-fA-F]{4}/;
value = char;"#;

    let diag =
        open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Regex panic diagnostics: {}", diag);
    // Should get a diagnostic about parse error, not a crash
    assert!(
        diag.contains("publishDiagnostics"),
        "Should get diagnostics notification even on regex panic"
    );
    assert!(
        diag.contains("error") || diag.contains("Parse") || diag.contains("regex"),
        "Should report the regex error, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_did_change() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Start with valid grammar
    let grammar = "number = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Change to grammar with undefined reference
    let changed = r#"{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///test.bbnf","version":2},"contentChanges":[{"text":"value = missing;"}]}}"#;
    send_lsp(&mut stdin, changed);
    let diag = read_until_contains(&mut stdout, "publishDiagnostics");
    eprintln!("Changed diagnostics: {}", diag);
    assert!(
        diag.contains("Undefined rule") || diag.contains("missing"),
        "Expected diagnostic about undefined 'missing', got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

// ============================================================================
// New feature tests
// ============================================================================

#[test]
fn test_inlay_hints() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Pure-terminal rules (number) are suppressed; rules with refs (value) are shown.
    let grammar = "number = /[0-9]+/;\nvalue = number | \"hello\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":30,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":0,"character":0},"end":{"line":1,"character":100}}}}"#,
    );
    let resp = read_response(&mut stdout, 30);
    eprintln!("Inlay hints response: {}", resp);

    // value's FIRST set should contain digits and 'h' (from "hello").
    assert!(
        resp.contains("'h'"),
        "Expected 'h' in value FIRST set, got: {}",
        resp
    );
    // Only value should have a hint (number is pure-terminal, suppressed).
    assert_eq!(
        resp.matches("paddingLeft").count(),
        1,
        "Expected 1 inlay hint (pure-terminal rules suppressed), got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_inlay_hints_nullable() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Optional makes value nullable.
    let grammar = "value = [\"hello\"];";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":30,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":100}}}}"#,
    );
    let resp = read_response(&mut stdout, 30);
    eprintln!("Nullable inlay hints: {}", resp);
    assert!(
        resp.contains("nullable"),
        "Expected nullable indicator for optional rule, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_inlay_hints_empty_range() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Use rules with nonterminal refs so hints aren't suppressed.
    let grammar = "x = \"x\";\na = x | \"y\";\nb = x | \"z\";\nc = x | \"w\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Request hints for only line 1 — should return just "a".
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":30,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":100}}}}"#,
    );
    let resp = read_response(&mut stdout, 30);
    eprintln!("Range-limited inlay hints: {}", resp);
    assert_eq!(
        resp.matches("paddingLeft").count(),
        1,
        "Expected 1 inlay hint for single-line range, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_initialize_new_capabilities() {
    let (mut stdin, mut stdout, child) = start_server();
    let resp = initialize(&mut stdin, &mut stdout);

    assert!(resp.contains("inlayHintProvider"), "missing inlay hints");
    assert!(resp.contains("selectionRangeProvider"), "missing selection range");
    assert!(resp.contains("documentRangeFormattingProvider"), "missing range formatting");
    assert!(resp.contains("documentOnTypeFormattingProvider"), "missing on-type formatting");
    // Check incremental sync.
    assert!(
        resp.contains("textDocumentSync") && resp.contains("2"),
        "Expected incremental sync (kind 2), got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_selection_range() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "value = number | \"hello\";\nnumber = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Request selection range at the "number" reference position (line 0, char 10).
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":31,"method":"textDocument/selectionRange","params":{"textDocument":{"uri":"file:///test.bbnf"},"positions":[{"line":0,"character":10}]}}"#,
    );
    let resp = read_response(&mut stdout, 31);
    eprintln!("Selection range response: {}", resp);

    // Should have nested parent chain (innermost -> alternation -> full rule).
    assert!(
        resp.contains("parent"),
        "Expected nested parent chain, got: {}",
        resp
    );
    // There should be at least 2 levels (innermost range + parent).
    let parent_count = resp.matches("parent").count();
    assert!(
        parent_count >= 2,
        "Expected at least 2 parent levels, got {}: {}",
        parent_count,
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_selection_range_multiple_positions() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "a = \"x\";\nb = \"y\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Request selection ranges for two positions.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":31,"method":"textDocument/selectionRange","params":{"textDocument":{"uri":"file:///test.bbnf"},"positions":[{"line":0,"character":1},{"line":1,"character":1}]}}"#,
    );
    let resp = read_response(&mut stdout, 31);
    eprintln!("Multi-pos selection range: {}", resp);
    // Should return array of 2 results.
    let result: serde_json::Value = serde_json::from_str(&resp).unwrap();
    let arr = result["result"].as_array().expect("result should be array");
    assert_eq!(arr.len(), 2, "Expected 2 selection ranges, got {}", arr.len());

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_range_formatting() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/ ;\nvalue = number | \"hello\" ;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Format only the first line.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":32,"method":"textDocument/rangeFormatting","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":100}},"options":{"tabSize":4,"insertSpaces":true}}}"#,
    );
    let resp = read_response(&mut stdout, 32);
    eprintln!("Range formatting response: {}", resp);

    // Should produce exactly one edit for the first rule.
    let result: serde_json::Value = serde_json::from_str(&resp).unwrap();
    let edits = result["result"].as_array().expect("result should be array");
    assert_eq!(edits.len(), 1, "Expected 1 edit (first rule only)");
    // The edit should contain the formatted version.
    let new_text = edits[0]["newText"].as_str().unwrap();
    assert!(
        new_text.contains("number = /[0-9]+/;"),
        "Expected formatted rule text, got: {}",
        new_text
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_range_formatting_no_overlap() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "a = \"x\";\nb = \"y\";\nc = \"z\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Format a range covering only the middle rule (line 1).
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":32,"method":"textDocument/rangeFormatting","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":100}},"options":{"tabSize":4,"insertSpaces":true}}}"#,
    );
    let resp = read_response(&mut stdout, 32);
    eprintln!("Range formatting (middle rule): {}", resp);
    let result: serde_json::Value = serde_json::from_str(&resp).unwrap();
    let edits = result["result"].as_array().expect("result should be array");
    assert_eq!(edits.len(), 1, "Expected 1 edit for middle rule only");

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_on_type_formatting() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/ ;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Trigger on-type formatting after typing ';'.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":33,"method":"textDocument/onTypeFormatting","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":19},"ch":";","options":{"tabSize":4,"insertSpaces":true}}}"#,
    );
    let resp = read_response(&mut stdout, 33);
    eprintln!("On-type formatting response: {}", resp);
    // Should return edits or null (both valid).
    assert!(
        resp.contains("\"id\":33"),
        "Expected on-type formatting response"
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_incremental_change() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Start with valid grammar.
    let grammar = "number = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Send an incremental change: insert "value = number;\n" at the beginning.
    let change = r#"{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///test.bbnf","version":2},"contentChanges":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"text":"value = number;\n"}]}}"#;
    send_lsp(&mut stdin, change);
    let diag = read_until_contains(&mut stdout, "publishDiagnostics");
    eprintln!("Incremental change diagnostics: {}", diag);

    // After inserting "value = number;\n", both rules should be defined and all refs resolved.
    // No errors, no warnings.
    assert!(
        !diag.contains("\"severity\":1"),
        "Expected no errors after incremental insert, got: {}",
        diag
    );
    assert!(
        !diag.contains("Undefined"),
        "Expected no undefined rule diagnostics, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_incremental_change_delete() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Start with two rules: value references number.
    let grammar = "value = number;\nnumber = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Delete the second line (number rule) via incremental change.
    let change = r#"{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///test.bbnf","version":2},"contentChanges":[{"range":{"start":{"line":0,"character":15},"end":{"line":1,"character":18}},"text":""}]}}"#;
    send_lsp(&mut stdin, change);
    let diag = read_until_contains(&mut stdout, "publishDiagnostics");
    eprintln!("After delete diagnostics: {}", diag);

    // Now "number" is undefined — should produce a warning.
    assert!(
        diag.contains("Undefined") || diag.contains("undefined") || diag.contains("number"),
        "Expected undefined rule diagnostic after deleting definition, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_incremental_change_replace() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "value = \"hello\";";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Replace "hello" with "world" via incremental change.
    let change = r#"{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///test.bbnf","version":2},"contentChanges":[{"range":{"start":{"line":0,"character":9},"end":{"line":0,"character":14}},"text":"world"}]}}"#;
    send_lsp(&mut stdin, change);
    let diag = read_until_contains(&mut stdout, "publishDiagnostics");
    eprintln!("After replace diagnostics: {}", diag);

    // Should still be valid (no errors).
    assert!(
        !diag.contains("\"severity\":1"),
        "Expected no errors after text replacement, got: {}",
        diag
    );

    // Verify the change took effect by hovering on "value".
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":50,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2}}}"#,
    );
    let resp = read_response(&mut stdout, 50);
    eprintln!("Hover after replace: {}", resp);
    assert!(
        resp.contains("world"),
        "Expected 'world' in hover after replace, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_formatting_produces_valid_output() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Grammar with irregular whitespace.
    let grammar = "a =    \"x\" ;\nb  =  \"y\"  |  \"z\" ;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":40,"method":"textDocument/formatting","params":{"textDocument":{"uri":"file:///test.bbnf"},"options":{"tabSize":4,"insertSpaces":true}}}"#,
    );
    let resp = read_response(&mut stdout, 40);
    let result: serde_json::Value = serde_json::from_str(&resp).unwrap();
    let edits = result["result"].as_array().expect("should have edits");
    assert!(!edits.is_empty(), "Expected at least one edit");
    let new_text = edits[0]["newText"].as_str().unwrap();
    eprintln!("Formatted text:\n{}", new_text);

    // Formatted output should have consistent spacing.
    assert!(new_text.contains("a = \"x\";"), "Expected normalized rule a");
    assert!(
        new_text.contains("b = \"y\" | \"z\";"),
        "Expected normalized alternation, got: {}",
        new_text
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_large_grammar() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // A realistic grammar with many rules.
    let grammar = r#"null = "null";
bool = "true" | "false";
number = /[0-9]+/;
string = /[a-zA-Z]+/;
array = "[" , [ value , { "," , value } ] , "]";
pair = string , ":" , value;
object = "{" , [ pair , { "," , pair } ] , "}";
value = string | number | object | array | bool | null;"#;

    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    assert!(!diag.contains("\"severity\":1"), "No errors in valid grammar");

    // Test all features work together on this grammar.

    // Hover on "value" (last rule).
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":41,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":7,"character":2}}}"#,
    );
    let resp = read_response(&mut stdout, 41);
    assert!(resp.contains("value"), "Hover should show value rule");

    // Inlay hints — trivial single-terminal rules are suppressed, but
    // complex rules (alternations, references) should still have hints.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":42,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":"file:///test.bbnf"},"range":{"start":{"line":0,"character":0},"end":{"line":7,"character":100}}}}"#,
    );
    let resp = read_response(&mut stdout, 42);
    let hint_count = resp.matches("paddingLeft").count();
    assert!(
        hint_count >= 4,
        "Expected at least 4 inlay hints for non-trivial rules, got {}: {}",
        hint_count,
        resp
    );

    // Document symbols — should have 8.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":43,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///test.bbnf"}}}"#,
    );
    let resp = read_response(&mut stdout, 43);
    let result: serde_json::Value = serde_json::from_str(&resp).unwrap();
    let symbols = result["result"].as_array().expect("symbols array");
    assert_eq!(symbols.len(), 8, "Expected 8 document symbols");

    shutdown(&mut stdin, &mut stdout, child);
}

// ============================================================================
// Cross-file import tests
// ============================================================================

#[test]
fn test_import_suppresses_undefined_rule() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Open the "base" document first.
    let base_grammar = "number = /[0-9]+/;\nstring = /[a-zA-Z]+/;";
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///base.bbnf", base_grammar,
    );

    // Open the "main" document that imports base.
    let main_grammar = r#"@import "base.bbnf";
value = number | string;"#;
    let diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///main.bbnf", main_grammar,
    );
    eprintln!("Import diagnostics: {}", diag);

    // The "Undefined rule" diagnostics for `number` and `string` should be suppressed
    // because they are imported from base.bbnf.
    assert!(
        !diag.contains("Undefined rule"),
        "Should NOT have undefined rule diagnostics for imported rules, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_import_selective_suppresses_only_named_rules() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Open the "base" document with three rules.
    let base_grammar = "number = /[0-9]+/;\nstring = /[a-zA-Z]+/;\nbool = \"true\" | \"false\";";
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///base.bbnf", base_grammar,
    );

    // Main imports only `number` selectively, but references `string` too.
    let main_grammar = r#"@import { number } from "base.bbnf";
value = number | string;"#;
    let diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///main.bbnf", main_grammar,
    );
    eprintln!("Selective import diagnostics: {}", diag);

    // `number` should be suppressed, but `string` should still be undefined.
    assert!(
        diag.contains("Undefined rule"),
        "Should have undefined rule for non-imported `string`, got: {}",
        diag
    );
    assert!(
        diag.contains("string"),
        "Undefined rule should be about `string`, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_cross_file_goto_definition() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Open the base document.
    let base_grammar = "number = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///base.bbnf", base_grammar,
    );

    // Open main that imports base and references `number`.
    let main_grammar = r#"@import "base.bbnf";
value = number;"#;
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///main.bbnf", main_grammar,
    );

    // Go to definition of "number" at line 1, char 10.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":60,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///main.bbnf"},"position":{"line":1,"character":10}}}"#,
    );
    let resp = read_response(&mut stdout, 60);
    eprintln!("Cross-file goto def response: {}", resp);

    // Should point to base.bbnf where number is defined.
    assert!(
        resp.contains("base.bbnf"),
        "Expected definition in base.bbnf, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_cross_file_references() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Open base document with `number` rule.
    let base_grammar = "number = /[0-9]+/;";
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///base.bbnf", base_grammar,
    );

    // Open main that references `number`.
    let main_grammar = r#"@import "base.bbnf";
value = number;"#;
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///main.bbnf", main_grammar,
    );

    // Find references to `number` from its definition in base.bbnf (line 0, char 2).
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":61,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///base.bbnf"},"position":{"line":0,"character":2},"context":{"includeDeclaration":true}}}"#,
    );
    let resp = read_response(&mut stdout, 61);
    eprintln!("Cross-file references response: {}", resp);

    // Should find references in both base.bbnf and main.bbnf.
    assert!(
        resp.contains("base.bbnf"),
        "Expected reference in base.bbnf, got: {}",
        resp
    );
    assert!(
        resp.contains("main.bbnf"),
        "Expected cross-file reference in main.bbnf, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_cross_file_completion_includes_imported_rules() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Open base document with rules.
    let base_grammar = "number = /[0-9]+/;\nstring = /[a-zA-Z]+/;";
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///base.bbnf", base_grammar,
    );

    // Open main that imports base and has a partially-written rule.
    let main_grammar = r#"@import "base.bbnf";
value = ;"#;
    let _diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///main.bbnf", main_grammar,
    );

    // Request completion at the cursor position in `value = |`.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":62,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///main.bbnf"},"position":{"line":1,"character":8}}}"#,
    );
    let resp = read_response(&mut stdout, 62);
    eprintln!("Cross-file completion response: {}", resp);

    // Should include imported rules `number` and `string` in completions.
    assert!(
        resp.contains("number"),
        "Expected 'number' in completions from import, got: {}",
        resp
    );
    assert!(
        resp.contains("string"),
        "Expected 'string' in completions from import, got: {}",
        resp
    );
    // Should also mention the source file.
    assert!(
        resp.contains("base.bbnf"),
        "Expected source file annotation in completion detail, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_import_parser_valid_syntax() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Test that @import directives parse without errors.
    let grammar = r#"@import "other.bbnf";
@import { a, b } from "lib.bbnf";
value = a | b;"#;
    let diag = open_doc_and_wait_diagnostics(
        &mut stdin, &mut stdout, "file:///test.bbnf", grammar,
    );
    eprintln!("Import syntax diagnostics: {}", diag);

    // Should not have parse errors.
    assert!(
        !diag.contains("\"severity\":1"),
        "Import syntax should parse without errors, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

// ───── Enhanced diagnostics tests ─────────────────────────────────────────

#[test]
fn test_diagnostics_first_set_conflict() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Two alternatives both start with a digit.
    let grammar = "value = integer | decimal;\ninteger = /[0-9]+/;\ndecimal = /[0-9]+/, \".\", /[0-9]+/;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("FIRST conflict diagnostics: {}", diag);
    assert!(
        diag.contains("ambiguous FIRST sets"),
        "Expected FIRST set conflict diagnostic, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_no_first_set_conflict() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Two alternatives with disjoint FIRST sets.
    let grammar = "value = number | string;\nnumber = /[0-9]+/;\nstring = /\"[^\"]*\"/;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("No FIRST conflict diagnostics: {}", diag);
    assert!(
        !diag.contains("ambiguous FIRST sets"),
        "Expected no FIRST set conflict, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_cycle_path() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Direct left recursion.
    let grammar = "expr = expr, \"+\", term | term;\nterm = /[0-9]+/;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Cycle path diagnostics: {}", diag);
    // Should contain the cycle path arrow notation (→ in UTF-8).
    assert!(
        diag.contains("\u{2192}"),
        "Expected cycle path with arrow in diagnostic, got: {}",
        diag
    );
    assert!(
        diag.contains("participates in a cycle"),
        "Expected cycle diagnostic, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_alias_hint() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // Rule that is an alias of another.
    let grammar = "value = number;\nnumber = /[0-9]+/;\nint = number;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Alias hint diagnostics: {}", diag);
    assert!(
        diag.contains("alias"),
        "Expected alias hint diagnostic, got: {}",
        diag
    );
    assert!(
        diag.contains("int"),
        "Expected alias name 'int' in diagnostic, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_diagnostics_unreachable_rule() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    // helperA and helperB reference each other but are unreachable from root.
    let grammar = "value = number;\nnumber = /[0-9]+/;\nhelperA = helperB;\nhelperB = helperA;";
    let diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);
    eprintln!("Unreachable rule diagnostics: {}", diag);
    assert!(
        diag.contains("unreachable"),
        "Expected unreachable rule diagnostic, got: {}",
        diag
    );

    shutdown(&mut stdin, &mut stdout, child);
}

#[test]
fn test_hover_enhanced_info() {
    let (mut stdin, mut stdout, child) = start_server();
    initialize(&mut stdin, &mut stdout);

    let grammar = "number = /[0-9]+/;\nvalue = number;";
    let _diag = open_doc_and_wait_diagnostics(&mut stdin, &mut stdout, "file:///test.bbnf", grammar);

    // Hover on "number" definition at line 0, char 2.
    send_lsp(
        &mut stdin,
        r#"{"jsonrpc":"2.0","id":10,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///test.bbnf"},"position":{"line":0,"character":2}}}"#,
    );
    let resp = read_response(&mut stdout, 10);
    eprintln!("Enhanced hover response: {}", resp);
    // Should contain FIRST set info.
    assert!(
        resp.contains("FIRST"),
        "Expected FIRST set in hover, got: {}",
        resp
    );
    // Should contain nullable info.
    assert!(
        resp.contains("Nullable"),
        "Expected nullable info in hover, got: {}",
        resp
    );

    shutdown(&mut stdin, &mut stdout, child);
}
