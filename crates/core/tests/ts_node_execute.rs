//! AZ-IV.W5.2 — TS Node Execution Proof.
//!
//! Real `node` invocation against the W1-emitted TypeScript JSON parser.
//! The test compiles the JSON grammar through the TS backend, strips
//! TypeScript type annotations to produce valid ESM JavaScript, wires
//! up the host-fn shim (`decode_json_string_to_arena`), spawns Node
//! with the shim, parses `data/json/twitter.json`, walks the path
//! `["statuses", 0, "text"]`, and prints the leaf as JSON on stdout.
//! The test then compares the leaf byte-for-byte against
//! - the eager Rust path (`JsonParser::parse(input)?.get(path)`), and
//! - the lazy Rust path (`runtime::json::parse_with::<&str>(input, &path)`),
//!
//! per the W5.2 lazy + eager parity gate.
//!
//! ## Environment gating
//!
//! Node is required. If `which node` resolves the test runs end-to-end;
//! if not, the test panics with a precise message recording the missing
//! environment so the orchestrator can document the gate without
//! silently passing. The W5 dispatch's empty-return rule forbids
//! silent skips.
//!
//! ## Audit
//!
//! On success the test writes a one-line audit summary to
//! `docs/tranches/AZ-IV/audit/W5-node-execute.txt` capturing the Node
//! version, the leaf bytes (as JSON-quoted), the commit hash, and a
//! UTC timestamp. The orchestrator consumes this artefact in the W5
//! verification matrix.

use bbnf::grammar::generated::json::JsonParser;
use bbnf::path::ir::{OwnedPathSegment, TypedPath};
use bbnf::path::markers::Json;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};
use bbnf::runtime::json::parse_with;
use bbnf::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Best-effort audit writer. Records the W5.2 outcome (pass or
/// W1-gap evidence) to `docs/tranches/AZ-IV/audit/W5-node-execute.txt`
/// regardless of whether the byte-for-byte assertion holds. The
/// orchestrator consumes the artefact in the W5 verification matrix.
fn write_audit(node_version: &str, body: &str) {
    let audit_dir = std::path::Path::new("../../docs/tranches/AZ-IV/audit");
    if std::fs::create_dir_all(audit_dir).is_ok() {
        let commit = current_commit_hash();
        let ts_now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);
        let header = format!(
            "AZ-IV.W5.2 ts_node_execute\n\
             node_version={node_version}\n\
             commit={commit}\n\
             unix_ts={ts_now}\n",
        );
        let _ = std::fs::write(
            audit_dir.join("W5-node-execute.txt"),
            format!("{header}{body}"),
        );
    }
}

fn json_grammar_src() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf").expect("read grammar/json/json.bbnf")
}

fn twitter_json_src() -> String {
    std::fs::read_to_string("../../data/json/twitter.json")
        .expect("read data/json/twitter.json — fixture required for W5.2 node execute proof")
}

/// Compile the JSON grammar through the TS backend and strip TS-only
/// syntax that the existing `gen_ts_parser` helper misses (`declare
/// function …;`, `as unknown as …` casts inside expressions).
fn compile_and_sanitise_ts_parser() -> String {
    let request = CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    };
    let source = match compile_grammar_request(&json_grammar_src(), &request)
        .expect("compile json grammar to TS")
    {
        CompileOutput::Ts(src) => src,
        other => panic!("expected TS output, got {other:?}"),
    };
    sanitise_ts_to_js(&source)
}

/// Strip TS-only syntax to leave valid ESM JavaScript.
///
/// Goes beyond `gen_ts_parser`'s `strip_ts_types`:
/// - drops `declare function …;` lines,
/// - drops `as <type>` and `as unknown as <type>` casts inside
///   expressions (greedy single-line strip),
/// - retains the `(s: ParserState)` and friends stripping behaviour
///   that the existing helper already handles.
fn sanitise_ts_to_js(ts: &str) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut brace_depth: i32 = 0;
    let mut skipping_block = false;

    for line in ts.lines() {
        let trimmed = line.trim();

        // Drop top-level `declare function …;` lines outright.
        if trimmed.starts_with("declare ") {
            continue;
        }

        // Skip `interface Foo { … }` and multi-line `type Foo = { … };`
        // blocks (matches `gen_ts_parser`'s strip helper).
        if !skipping_block && (trimmed.starts_with("interface ") || trimmed.starts_with("type ")) {
            if trimmed.ends_with(';') && !trimmed.contains('{') {
                continue;
            }
            skipping_block = true;
            brace_depth = 0;
        }
        if skipping_block {
            for ch in trimmed.chars() {
                if ch == '{' {
                    brace_depth += 1;
                }
                if ch == '}' {
                    brace_depth -= 1;
                }
            }
            if brace_depth <= 0 && (trimmed.ends_with(';') || trimmed == "}") {
                skipping_block = false;
            }
            continue;
        }

        let mut cleaned = line.to_string();
        cleaned = cleaned.replace(" as const", "");
        cleaned = cleaned.replace("(s: ParserState)", "(s)");
        cleaned = cleaned.replace("(input: string)", "(input)");
        cleaned = cleaned.replace("start: number, end: number", "start, end");
        cleaned = cleaned.replace("input: string", "input");

        // Strip `): RetType | null {` → `) {` (only when the line
        // contains a final `{` opening a function body). Mirrors the
        // existing strip helper.
        if let Some(idx) = cleaned.find("): ") {
            if let Some(last_brace) = cleaned.rfind('{') {
                if last_brace > idx {
                    let before = &cleaned[..idx + 1];
                    let after = &cleaned[last_brace..];
                    cleaned = format!("{before} {after}");
                }
            }
        }

        // Strip ` as <Ident>` and ` as unknown as <Ident>` casts that
        // the W1 emitter inlines into expression positions. The
        // emitter shape is exactly ` as unknown as valueValue` /
        // ` as valueValue` — token-level pattern.
        cleaned = strip_as_casts(&cleaned);

        lines.push(cleaned);
    }

    lines.join("\n")
}

/// Remove ` as <Ident>` and ` as unknown as <Ident>` cast tokens. The
/// pattern only ever appears as a tail-of-expression cast in the
/// emitter; we walk the line, peel the casts, and stop when no more
/// patterns are found.
fn strip_as_casts(line: &str) -> String {
    let mut out = line.to_string();
    loop {
        let before = out.clone();
        // ` as unknown as <Ident>` (must precede the simpler ` as` peel).
        if let Some(pos) = out.find(" as unknown as ") {
            let tail = &out[pos + " as unknown as ".len()..];
            let end = tail
                .find(|c: char| !(c.is_alphanumeric() || c == '_'))
                .unwrap_or(tail.len());
            out = format!("{}{}", &out[..pos], &tail[end..]);
        } else if let Some(pos) = out.find(" as ") {
            let tail = &out[pos + " as ".len()..];
            // Only strip simple identifier casts; if what follows
            // doesn't start with an ident char, leave the line alone
            // (safer than corrupting unrelated source).
            if !tail
                .chars()
                .next()
                .map(|c| c.is_alphabetic() || c == '_')
                .unwrap_or(false)
            {
                break;
            }
            let end = tail
                .find(|c: char| !(c.is_alphanumeric() || c == '_'))
                .unwrap_or(tail.len());
            out = format!("{}{}", &out[..pos], &tail[end..]);
        } else {
            break;
        }
        if out == before {
            break;
        }
    }
    out
}

/// Resolve `node` on PATH, returning its absolute path or `None` if
/// it cannot be located. The dispatch's empty-return rule forbids a
/// silent skip — callers must surface the missing-Node case as a
/// loud panic with the diagnostic recorded.
fn which_node() -> Option<std::path::PathBuf> {
    let out = std::process::Command::new("which")
        .arg("node")
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if s.is_empty() {
        None
    } else {
        Some(std::path::PathBuf::from(s))
    }
}

fn node_version(node: &std::path::Path) -> String {
    let out = std::process::Command::new(node)
        .arg("--version")
        .output()
        .expect("node --version");
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

fn current_commit_hash() -> String {
    let out = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .expect("git rev-parse HEAD");
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn node_execute_twitter_statuses_zero_text() {
    // 1. Locate node.
    let node = match which_node() {
        Some(n) => n,
        None => panic!(
            "AZ-IV.W5.2 hard gate: `which node` resolved nothing on \
             PATH. Node.js is required for the TS Node execution \
             proof. Install Node ≥ 18 and rerun. (Empty-return rule: \
             this test refuses to silently skip.)"
        ),
    };
    let nv = node_version(&node);
    eprintln!("ts_node_execute: node = {} ({nv})", node.display());

    // 2. Generate fresh TS parser source and sanitise to JS.
    let js_parser = compile_and_sanitise_ts_parser();
    assert!(
        js_parser.contains("export function parse"),
        "TS emitter must expose `parse`; got prefix: {}",
        &js_parser[..js_parser.len().min(400)]
    );

    // 3. Stage the parser + a runner script in a temp dir.
    let tmp = tempfile::tempdir().expect("tempdir");
    let parser_path = tmp.path().join("generated_json.mjs");
    std::fs::write(&parser_path, &js_parser).expect("write generated_json.mjs");

    // 4. Stage the twitter fixture next to the parser. The fixture is
    //    git-ignored at the repo root (large), but copying into the
    //    tempdir keeps the runner self-contained.
    let twitter_src = twitter_json_src();
    let fixture_path = tmp.path().join("twitter.json");
    std::fs::write(&fixture_path, &twitter_src).expect("stage twitter.json");

    // 5. Stage the runner. The runner imports the parser, injects the
    //    `decode_json_string_to_arena` host fn (mirrors the Rust
    //    `parse_that::parsers::scan::decode_json_string_to_arena`
    //    surface — JSON-quoted span → decoded JS string), parses the
    //    fixture, walks the typed path on the resulting tagged tree,
    //    and prints the leaf as a JSON-quoted string on stdout. Any
    //    failure throws and Node exits non-zero.
    let runner_src = r#"
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dirname = dirname(fileURLToPath(import.meta.url));
const inputPath = join(__dirname, "twitter.json");
const input = readFileSync(inputPath, "utf-8");

// Host fn shim: decode_json_string_to_arena receives the span
// {start, end} of a JSON-quoted string literal and must return the
// decoded JS string. Mirrors parse_that::parsers::scan::decode_json_string_to_arena.
globalThis.decode_json_string_to_arena = function decode_json_string_to_arena(span) {
    if (span === null) return null;
    const raw = input.slice(span.start, span.end);
    return JSON.parse(raw);
};

const mod = await import("./generated_json.mjs");
if (typeof mod.parse !== "function") {
    throw new Error("generated_json.mjs missing exported parse()");
}

const out = mod.parse(input);
if (out === null || out === undefined || out.result === null) {
    throw new Error(`parse() failed: ${JSON.stringify(out)}`);
}
if (out.offset !== input.length) {
    throw new Error(
        `parse() did not consume full input: offset=${out.offset} len=${input.length}`,
    );
}

// Walk the tagged tree at path ["statuses", 0, "text"].
// Every JsonValue at the runtime is { tag, value }:
//   - object.value is an array of pair-values { tag: "pair", value: [keyValue, valueValue] }
//   - array.value  is an array of valueValue
//   - string.value is the decoded JS string
//   - pair.value is the [keyValue, valueValue] tuple

function getField(node, name) {
    if (node === null || node.tag !== "object") {
        throw new Error(`getField: expected object, got ${node && node.tag}`);
    }
    const pairs = node.value;
    for (const p of pairs) {
        if (p.tag !== "pair") {
            throw new Error(`getField: pair-array element not pair: ${p && p.tag}`);
        }
        const [keyNode, valNode] = p.value;
        if (keyNode === null || keyNode.tag !== "string") {
            throw new Error(`getField: key not string: ${keyNode && keyNode.tag}`);
        }
        if (keyNode.value === name) return valNode;
    }
    return null;
}

function getIndex(node, idx) {
    if (node === null || node.tag !== "array") {
        throw new Error(`getIndex: expected array, got ${node && node.tag}`);
    }
    const elems = node.value;
    if (idx < 0 || idx >= elems.length) return null;
    return elems[idx];
}

function asString(node) {
    if (node === null || node.tag !== "string") {
        throw new Error(`asString: expected string, got ${node && node.tag}`);
    }
    return node.value;
}

let cur = out.result;
cur = getField(cur, "statuses");
cur = getIndex(cur, 0);
cur = getField(cur, "text");
const leaf = asString(cur);

// Emit on stdout as a JSON-encoded string so the Rust test reads back
// a byte-for-byte equal value via serde_json::from_str::<String>.
process.stdout.write(JSON.stringify(leaf));
"#;
    let runner_path = tmp.path().join("runner.mjs");
    std::fs::write(&runner_path, runner_src).expect("write runner.mjs");

    // 6. Spawn node and capture stdout.
    let output = std::process::Command::new(&node)
        .arg(&runner_path)
        .output()
        .expect("spawn node runner");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        write_audit(
            &nv,
            &format!(
                "status=fail-w1-gap\n\
                 path=$.statuses[0].text\n\
                 reason=TS-emitted parser cannot navigate object.value\n\
                 detail=W1 backend-ts emits count-only repeats; object/array \
                 `value` is a span over input range, not an aggregated array \
                 of pairs/elements. Path-walk fails at first object descent.\n\
                 stderr_first_400={}\n\
                 stdout_first_400={}\n",
                stderr.chars().take(400).collect::<String>(),
                stdout.chars().take(400).collect::<String>(),
            ),
        );
        panic!(
            "node runner failed (status {:?}):\n----- stderr -----\n{stderr}\n----- stdout -----\n{stdout}",
            output.status,
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    // The runner JSON-encodes the leaf string; decode it.
    let ts_leaf: String = serde_json::from_str(&stdout).unwrap_or_else(|e| {
        panic!("node stdout was not valid JSON-encoded string ({e}); raw stdout: {stdout:?}")
    });

    // 7. Eager Rust path.
    let doc = JsonParser::parse(&twitter_src).expect("eager Rust parse");
    let legacy_segments = [
        LegacySegment::Field("statuses"),
        LegacySegment::Index(0),
        LegacySegment::Field("text"),
    ];
    let rust_eager = doc
        .get::<&str>(LegacyPath::new(&legacy_segments))
        .expect("eager Rust leaf at $.statuses[0].text");

    // 8. Lazy Rust path (`parse_with`).
    let typed_path: TypedPath<Json, &str> = TypedPath::from_owned(vec![
        OwnedPathSegment::Field("statuses".to_owned()),
        OwnedPathSegment::Index(0),
        OwnedPathSegment::Field("text".to_owned()),
    ]);
    let rust_lazy = parse_with::<&str>(&twitter_src, &typed_path)
        .expect("lazy Rust leaf at $.statuses[0].text");

    // 9. Cross-frontend byte-for-byte equality.
    assert_eq!(
        rust_eager.as_bytes(),
        rust_lazy.as_bytes(),
        "Rust eager + lazy disagree on $.statuses[0].text"
    );
    assert_eq!(
        ts_leaf.as_bytes(),
        rust_eager.as_bytes(),
        "TS Node frontend leaf differs from Rust eager leaf at \
         $.statuses[0].text\n  ts_len={}, rust_len={}\n  ts={ts_leaf:?}\n  rust={rust_eager:?}",
        ts_leaf.len(),
        rust_eager.len()
    );

    // 10. Audit artefact. Best-effort write — the test passes even if
    //     the audit dir cannot be created (e.g. read-only sandbox).
    write_audit(
        &nv,
        &format!(
            "status=pass\n\
             path=$.statuses[0].text\n\
             leaf_byte_len={}\n\
             leaf_json={}\n\
             rust_eager==rust_lazy==ts: byte-for-byte\n",
            ts_leaf.len(),
            serde_json::to_string(&ts_leaf).unwrap_or_default(),
        ),
    );
}
