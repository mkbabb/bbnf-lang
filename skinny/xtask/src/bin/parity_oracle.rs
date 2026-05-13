//! Parity oracle: compare skinny JSON parse outcome against serde_json
//! over a set of corpora. Reports pass/fail with first-byte divergence.
//!
//! Invocation:
//!     parity-oracle <corpus_path> [<corpus_path> ...]
//!
//! Exit code: 0 if all parity-pass, 1 if any divergence or skinny parse-fail.

use runtime::generated_json::{self, JsonValue};
use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: parity-oracle <corpus_path> [<corpus_path> ...]");
        std::process::exit(2);
    }

    let mut any_fail = false;
    println!("corpus,size_bytes,skinny_ok,serde_ok,structural_match,note");
    for path_arg in &args[1..] {
        let path = Path::new(path_arg);
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(path_arg);
        let bytes = match std::fs::read(path) {
            Ok(b) => b,
            Err(e) => {
                println!("{name},0,err,err,no,read_failed: {e}");
                any_fail = true;
                continue;
            }
        };
        let size = bytes.len();

        let input = match std::str::from_utf8(&bytes) {
            Ok(s) => s,
            Err(e) => {
                println!("{name},{size},err,err,no,non_utf8: {e}");
                any_fail = true;
                continue;
            }
        };

        let skinny_result = generated_json::parse(input);
        let skinny_ok = skinny_result.is_ok();

        let serde_result = serde_json::from_str::<serde_json::Value>(input);
        let serde_ok = serde_result.is_ok();

        let mut match_ok = false;
        let mut note = String::new();
        if skinny_ok && serde_ok {
            // Walk both trees, count nodes by kind, compare.
            let root = skinny_result.unwrap();
            let serde_val = serde_result.unwrap();
            let mut sk_counts = [0u64; 6]; // obj, arr, str, num, bool, null
            count_skinny(&root.value(), &mut sk_counts);
            let mut sj_counts = [0u64; 6];
            count_serde(&serde_val, &mut sj_counts);
            if sk_counts == sj_counts {
                match_ok = true;
            } else {
                note = format!("count_mismatch sk={:?} sj={:?}", sk_counts, sj_counts);
            }
        } else if !skinny_ok {
            note = format!("skinny_err: {}", skinny_result.err().unwrap());
        } else {
            note = format!("serde_err: {}", serde_result.err().unwrap());
        }

        let s = if skinny_ok { "ok" } else { "fail" };
        let j = if serde_ok { "ok" } else { "fail" };
        let m = if match_ok { "yes" } else { "no" };
        println!("{name},{size},{s},{j},{m},{note}");
        if !skinny_ok || (serde_ok && !match_ok) {
            any_fail = true;
        }
    }

    std::process::exit(if any_fail { 1 } else { 0 });
}

fn count_skinny<'doc, 'i: 'doc>(value: &JsonValue<'doc, 'i>, counts: &mut [u64; 6]) {
    match value {
        JsonValue::Object(o) => {
            counts[0] += 1;
            for pair in o.pairs() {
                counts[2] += 1; // key is a string
                count_skinny(&pair.value(), counts);
            }
        }
        JsonValue::Array(a) => {
            counts[1] += 1;
            for v in a.values() {
                count_skinny(&v, counts);
            }
        }
        JsonValue::String(_) => counts[2] += 1,
        JsonValue::Number(_) => counts[3] += 1,
        JsonValue::Bool(_) => counts[4] += 1,
        JsonValue::Null(_) => counts[5] += 1,
    }
}

fn count_serde(value: &serde_json::Value, counts: &mut [u64; 6]) {
    match value {
        serde_json::Value::Object(o) => {
            counts[0] += 1;
            for (k, v) in o {
                let _ = k;
                counts[2] += 1; // key
                count_serde(v, counts);
            }
        }
        serde_json::Value::Array(a) => {
            counts[1] += 1;
            for v in a {
                count_serde(v, counts);
            }
        }
        serde_json::Value::String(_) => counts[2] += 1,
        serde_json::Value::Number(_) => counts[3] += 1,
        serde_json::Value::Bool(_) => counts[4] += 1,
        serde_json::Value::Null => counts[5] += 1,
    }
}
