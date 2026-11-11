//! mirror-lint — LOCK-6 6A MIRROR-TREE structural linter.
//!
//! Verifies the isomorphism plane between the canonical path spine
//! (`crates/core/src/path/{ir,error}.rs` + `markers.rs`) and the
//! TS/wasm bridge (`crates/bbnf-path-ts/{src/schema.rs, src/lib.rs,
//! src/fixture.rs, ts/path.ts}`).
//!
//! It syn-extracts every bridge-side declaration, classifies each
//! against the I=5 idiom classes (idiom-classes.toml), and resolves
//! each to a spine pre-image (mirror.toml [[pair]] rows for the serde
//! twins; structural rules for the rest). A bridge declaration that is
//! neither an identity mirror nor a covered idiom exemption fails RED.
//!
//!   cap        = Σ_c |sites matching predicate[c] with a spine pre-image|
//!   exemptions = |non-identity bridge declarations|
//!   GREEN iff exemptions == cap  (and cap == expected 13)
//!
//! Usage:
//!   mirror-lint --spine A,B --markers M --schema S --lib L \
//!               --fixture F --ts T --mirror mirror.toml

use std::collections::BTreeSet;
use std::fs;
use std::process::exit;

struct Args {
    spine: Vec<String>,
    markers: String,
    schema: String,
    lib: String,
    fixture: String,
    ts: String,
    mirror: String,
}

fn arg(args: &[String], key: &str) -> String {
    let i = args.iter().position(|a| a == key).unwrap_or_else(|| {
        eprintln!("missing arg {key}");
        exit(2);
    });
    args[i + 1].clone()
}

fn read(path: &str) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("cannot read {path}: {e}");
        exit(2);
    })
}

/// syn-extract pub struct/enum/trait idents from a Rust file.
fn rust_type_idents(src: &str) -> BTreeSet<String> {
    let file = syn::parse_file(src).expect("parse rust file");
    let mut out = BTreeSet::new();
    for item in file.items {
        match item {
            syn::Item::Struct(s) => {
                out.insert(s.ident.to_string());
            }
            syn::Item::Enum(e) => {
                out.insert(e.ident.to_string());
            }
            syn::Item::Trait(t) => {
                out.insert(t.ident.to_string());
            }
            _ => {}
        }
    }
    out
}

/// serde-transport sites: struct/enum in schema.rs deriving Serialize
/// whose ident ends in "Payload". Returns the idents.
fn serde_payload_types(src: &str) -> Vec<String> {
    let file = syn::parse_file(src).expect("parse schema");
    let mut out = Vec::new();
    let mut consider = |ident: String, attrs: &[syn::Attribute]| {
        if !ident.ends_with("Payload") {
            return;
        }
        let derives_serialize = attrs.iter().any(|a| {
            a.path().is_ident("derive")
                && matches!(&a.meta, syn::Meta::List(l) if l.tokens.to_string().contains("Serialize"))
        });
        if derives_serialize {
            out.push(ident);
        }
    };
    for item in file.items {
        match item {
            syn::Item::Struct(s) => consider(s.ident.to_string(), &s.attrs),
            syn::Item::Enum(e) => consider(e.ident.to_string(), &e.attrs),
            _ => {}
        }
    }
    out
}

/// native-shim sites: pub fn *_native + whether a #[wasm_bindgen] pub fn
/// of the same stem exists in the same file.
fn native_shims(src: &str) -> Vec<(String, bool)> {
    let file = syn::parse_file(src).expect("parse lib");
    let mut wasm_exports = BTreeSet::new();
    let mut shims = Vec::new();
    for item in &file.items {
        if let syn::Item::Fn(f) = item {
            let name = f.sig.ident.to_string();
            let has_wasm = f.attrs.iter().any(|a| {
                let p = a.path();
                p.is_ident("wasm_bindgen")
                    || p.segments.last().map(|s| s.ident == "wasm_bindgen").unwrap_or(false)
            });
            if has_wasm {
                wasm_exports.insert(name.clone());
            }
        }
    }
    for item in &file.items {
        if let syn::Item::Fn(f) = item {
            let name = f.sig.ident.to_string();
            if let Some(stem) = name.strip_suffix("_native") {
                shims.push((name.clone(), wasm_exports.contains(stem)));
            }
        }
    }
    shims
}

/// constructor sites in schema.rs: `impl X { pub fn new }`.
fn rust_constructors(src: &str) -> Vec<String> {
    let file = syn::parse_file(src).expect("parse for ctor");
    let mut out = Vec::new();
    for item in file.items {
        if let syn::Item::Impl(im) = item {
            let ty = if let syn::Type::Path(tp) = &*im.self_ty {
                tp.path.segments.last().map(|s| s.ident.to_string()).unwrap_or_default()
            } else {
                String::new()
            };
            for it in im.items {
                if let syn::ImplItem::Fn(f) = it {
                    if f.sig.ident == "new" {
                        out.push(format!("{ty}::new"));
                    }
                }
            }
        }
    }
    out
}

/// TS census: (interfaces_and_types, error_classes, has_constructor).
fn ts_census(src: &str) -> (Vec<String>, Vec<String>, bool) {
    let mut ifaces = Vec::new();
    let mut classes = Vec::new();
    let mut has_ctor = false;
    for line in src.lines() {
        let t = line.trim();
        if let Some(rest) = t.strip_prefix("export interface ") {
            ifaces.push(ident_head(rest));
        } else if let Some(rest) = t.strip_prefix("export type ") {
            ifaces.push(ident_head(rest));
        } else if let Some(rest) = t.strip_prefix("export class ") {
            let name = ident_head(rest);
            if t.contains("extends Error") {
                classes.push(name);
            } else {
                // non-error class still an exemption candidate; record as
                // "class:Name" so it must find a pre-image.
                classes.push(name);
            }
        }
        if t.starts_with("constructor(") {
            has_ctor = true;
        }
    }
    (ifaces, classes, has_ctor)
}

fn ident_head(s: &str) -> String {
    s.chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect()
}

/// marker keys in fixture.rs fixture_for_marker: `"Json" => Some(...)`.
fn marker_keys(src: &str) -> Vec<String> {
    let mut out = Vec::new();
    for line in src.lines() {
        let t = line.trim();
        // pattern:  "Json" => Some(json_fixture()),
        if t.starts_with('"') && t.contains("=> Some(") {
            if let Some(end) = t[1..].find('"') {
                out.push(t[1..1 + end].to_string());
            }
        }
    }
    out
}

/// spine marker ZSTs: `pub struct Json;` in markers.rs.
fn spine_markers(src: &str) -> BTreeSet<String> {
    rust_type_idents(src)
}

/// mirror.toml [[pair]] rows: twin -> preimage.
fn mirror_pairs(src: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let mut twin = None;
    for line in src.lines() {
        let t = line.trim();
        if t == "[[pair]]" {
            twin = None;
        } else if let Some(v) = t.strip_prefix("twin = ") {
            twin = Some(v.trim().trim_matches('"').to_string());
        } else if let Some(v) = t.strip_prefix("preimage = ") {
            if let Some(tw) = twin.take() {
                out.push((tw, v.trim().trim_matches('"').to_string()));
            }
        }
    }
    out
}

fn main() {
    let raw: Vec<String> = std::env::args().collect();
    let args = Args {
        spine: arg(&raw, "--spine").split(',').map(String::from).collect(),
        markers: arg(&raw, "--markers"),
        schema: arg(&raw, "--schema"),
        lib: arg(&raw, "--lib"),
        fixture: arg(&raw, "--fixture"),
        ts: arg(&raw, "--ts"),
        mirror: arg(&raw, "--mirror"),
    };

    // 1. Canonical spine universe.
    let mut spine_types: BTreeSet<String> = BTreeSet::new();
    for p in &args.spine {
        spine_types.extend(rust_type_idents(&read(p)));
    }
    let spine_mk = spine_markers(&read(&args.markers));
    let pairs = mirror_pairs(&read(&args.mirror));

    // 2. Bridge census.
    let schema_src = read(&args.schema);
    let lib_src = read(&args.lib);
    let fixture_src = read(&args.fixture);
    let ts_src = read(&args.ts);

    let serde_sites = serde_payload_types(&schema_src);
    let shim_sites = native_shims(&lib_src);
    let (ts_ifaces, ts_classes, ts_has_ctor) = ts_census(&ts_src);
    let rust_ctors = rust_constructors(&schema_src);
    let markers = marker_keys(&fixture_src);

    // 3. Classify every NON-identity bridge declaration.
    // Each entry: (site, class, legit)
    let mut entries: Vec<(String, &str, bool)> = Vec::new();

    // serde-transport: Payload types. legit iff a mirror pair maps it to
    // a spine type that exists.
    for s in &serde_sites {
        let legit = pairs
            .iter()
            .find(|(tw, _)| tw == s)
            .map(|(_, pre)| spine_types.contains(pre))
            .unwrap_or(false);
        entries.push((format!("schema.rs:{s}"), "serde-transport", legit));
    }

    // native-shim: *_native fns. legit iff matching wasm export exists.
    for (name, has_export) in &shim_sites {
        entries.push((format!("lib.rs:{name}"), "native-shim", *has_export));
    }

    // native-error: TS error classes. legit iff spine PathError exists.
    for c in &ts_classes {
        // an error-class is legit iff its identity pre-image (PathError)
        // is in the spine.
        let legit = spine_types.contains(c);
        entries.push((format!("path.ts:class {c}"), "native-error", legit));
    }

    // constructor: schema Payload::new + ts constructor. legit iff spine
    // PathError exists (canonical constructor pre-image).
    let ctor_preimage_ok = spine_types.contains("PathError");
    for c in &rust_ctors {
        entries.push((format!("schema.rs:{c}"), "constructor", ctor_preimage_ok));
    }
    if ts_has_ctor {
        entries.push((
            "path.ts:PathError#constructor".to_string(),
            "constructor",
            ctor_preimage_ok,
        ));
    }

    // marker: fixture marker keys. legit iff spine marker ZST exists.
    for m in &markers {
        entries.push((format!("fixture.rs:\"{m}\""), "marker", spine_mk.contains(m)));
    }

    // TS interfaces/types that are NOT identity mirrors and NOT serde
    // pair twins are UNACCOUNTED exemptions (class none, legit false).
    let pair_twins: BTreeSet<&String> = pairs.iter().map(|(t, _)| t).collect();
    for i in &ts_ifaces {
        let is_identity = spine_types.contains(i);
        let is_pair = pair_twins.contains(i);
        if !is_identity && !is_pair {
            entries.push((format!("path.ts:interface {i}"), "UNACCOUNTED", false));
        }
    }

    // 4. Tally.
    let exemptions = entries.len();
    let cap = entries.iter().filter(|(_, _, ok)| *ok).count();
    let expected = 13usize;

    // per-class breakdown
    let classes = ["serde-transport", "native-shim", "native-error", "constructor", "marker"];
    println!("== 6A MIRROR-TREE linter ==");
    for c in classes {
        let n = entries
            .iter()
            .filter(|(_, cl, ok)| *cl == c && *ok)
            .count();
        println!("  {c:<16} cap+= {n}");
    }
    let unaccounted: Vec<&(String, &str, bool)> =
        entries.iter().filter(|(_, _, ok)| !*ok).collect();
    if !unaccounted.is_empty() {
        println!("  -- illegitimate / unaccounted:");
        for (site, cl, _) in &unaccounted {
            println!("     RED  {site}  [{cl}]");
        }
    }
    println!("cap        = {cap}   (Σ predicate sites with spine pre-image)");
    println!("exemptions = {exemptions}   (non-identity bridge declarations)");
    println!("expected   = {expected}");

    let green = exemptions == cap && cap == expected;
    if green {
        println!("VERDICT: GREEN  (exemptions == cap == {expected})");
        exit(0);
    } else {
        println!(
            "VERDICT: RED    (exemptions {exemptions} != cap {cap}  or cap != {expected})"
        );
        exit(1);
    }
}
