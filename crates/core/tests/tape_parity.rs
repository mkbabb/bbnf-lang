//! Tape-first parity tests — AC.2 acceptance gate.
//!
//! For every production grammar (JSON, CSS L4, BBNF, Google Sheets,
//! EBNF) this suite parses a corpus of sample inputs through the
//! new `Grammar::parse(input)` → `Parsed<Grammar>` → `.view()` API
//! and asserts:
//!
//! - `parse()` succeeds on valid inputs and the root view's `kind()`
//!   is a `Rule` or compound discriminator (never `None`).
//! - The structural walk yields a stable child count.
//! - The total record count written to the tape matches the golden
//!   snapshot stored under `tests/fixtures/tape_golden/<grammar>/<input>.json`.
//!
//! Golden snapshots are generated on first run if absent and
//! compared byte-for-byte thereafter. When the grammar or the
//! tape layout changes intentionally, delete the golden directory
//! and re-run the suite to regenerate.
//!
//! # Why tape parity matters
//!
//! Pre-AC.2 the parser emitted an eager enum tree (`<Name>Enum`) and
//! callers matched against enum variants directly. Post-AC.2 the
//! parser writes to a linear tape and callers read through
//! `TapeCursor` accessors. Parity between the two shapes is measured
//! by the structural summary below: total records in the tape, root
//! kind, and the depth-one child count. Any divergence against the
//! golden is a codegen regression.

#![allow(dead_code)]

use std::fs;
use std::path::PathBuf;

use bbnf::runtime::Root;
use bbnf::runtime::tape::{TapeCursor, TapeKind};
use bbnf_derive::Parser;

// ─── Grammar fixtures ────────────────────────────────────────────────
//
// Each `#[derive(Parser)]` below pulls in the production grammar the
// corresponding benchmark corpus targets. The `skip_recover` flag on
// the CSS and Google Sheets grammars matches the existing slab
// integration tests — the benchmark corpus is well-formed, so
// recovery arms would never fire.

/// AU.2.4: the CSS L4 grammar's `hex` rule references
/// `crate::css_types::parse_hex_color` through the HexConvert route;
/// every #[derive(Parser)] site that includes CSS L4 must expose this
/// module with a compatible signature so the generated parser compiles.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonGrammar;

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Grammar;

#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
struct BbnfGrammar;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsGrammar;

#[derive(Parser)]
#[parser(path = "../../grammar/ebnf/ebnf.bbnf")]
struct EbnfGrammar;

// ─── Corpus helpers ──────────────────────────────────────────────────

/// Search a small candidate list for an input file. Matches the
/// existing slab-test helper so the tests can be run from either
/// the workspace root (`cargo test -p bbnf`) or the crate directory.
fn load(relpath: &str) -> String {
    let candidates = [
        format!("../../data/{}", relpath),
        format!("../data/{}", relpath),
        format!("data/{}", relpath),
        relpath.to_string(),
    ];
    for path in &candidates {
        if let Ok(contents) = fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find data file '{}'; tried: {:?}",
        relpath, candidates
    );
}

/// Load a grammar sample file from the `grammar/` tree — used for
/// the BBNF self-hosted test case.
fn load_grammar_sample(relpath: &str) -> String {
    let candidates = [
        format!("../../grammar/{}", relpath),
        format!("../grammar/{}", relpath),
        format!("grammar/{}", relpath),
    ];
    for path in &candidates {
        if let Ok(contents) = fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find grammar file '{}'; tried: {:?}",
        relpath, candidates
    );
}

// ─── Bridge traits ───────────────────────────────────────────────────

/// Bridge trait that lifts every generated view's inherent
/// `.cursor()` accessor into a generic-compatible surface.
///
/// The universal accessor set defined in `generate_views` includes
/// `fn cursor(&self) -> TapeCursor<'tape>` as an inherent method on
/// each `<Name>View<'tape>` struct. Rust doesn't let us call inherent
/// methods through a generic `V` bound, so we bounce through this
/// trait. The method is named `cursor_of` (not `cursor`) so the
/// trait dispatch can't accidentally recurse into itself when the
/// view type also carries an inherent `cursor()` method.
trait HasCursor<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape>;
}

/// Bridge trait that lifts every grammar's inherent `Grammar::parse`
/// entry point into a generic surface. Generated parsers emit
/// `pub fn parse(input: &str) -> Result<Parsed<Self>, ParseErr>` as
/// an inherent method on the grammar marker struct; this trait
/// forwards to it so the harness can be generic over grammars
/// without forcing `parse` onto `Root`.
trait ParseGrammar: Root + Sized {
    fn parse_input(input: &str) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr>;
}

// ─── Per-grammar bridge impls ────────────────────────────────────────
//
// Each grammar's root view type gets a `HasCursor` impl that
// forwards to the view's inherent `.cursor()` method; each grammar
// marker struct gets a `ParseGrammar` impl that forwards to the
// generated `Grammar::parse` inherent. One macro covers both.

macro_rules! impl_grammar_bridges {
    ($grammar:ty) => {
        impl<'tape> HasCursor<'tape> for <$grammar as Root>::View<'tape> {
            fn cursor_of(&self) -> TapeCursor<'tape> {
                // Fully-qualified inherent method call on the view
                // type — unqualified `self.cursor()` would recurse
                // into the trait method on type-alias resolution.
                <<$grammar as Root>::View<'tape>>::cursor(self)
            }
        }
        impl ParseGrammar for $grammar {
            fn parse_input(
                input: &str,
            ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
                <$grammar>::parse(input)
            }
        }
    };
}

impl_grammar_bridges!(JsonGrammar);
impl_grammar_bridges!(CssL4Grammar);
impl_grammar_bridges!(BbnfGrammar);
impl_grammar_bridges!(SheetsGrammar);
impl_grammar_bridges!(EbnfGrammar);

// ─── Golden-snapshot machinery ───────────────────────────────────────

/// Map a [`TapeKind`] to its canonical name string used in golden
/// files. Kept separate from the `Debug` impl so the kind string
/// isn't dependent on upstream formatting changes.
fn kind_name(k: TapeKind) -> &'static str {
    match k {
        TapeKind::None => "None",
        TapeKind::Span => "Span",
        TapeKind::Epsilon => "Epsilon",
        TapeKind::Literal => "Literal",
        TapeKind::Regex => "Regex",
        TapeKind::Seq => "Seq",
        TapeKind::Alt => "Alt",
        TapeKind::Repeat => "Repeat",
        TapeKind::Rule => "Rule",
        TapeKind::VariantTag => "VariantTag",
        TapeKind::MapValue => "MapValue",
        TapeKind::TokenDispatch => "TokenDispatch",
        TapeKind::Recovered => "Recovered",
        TapeKind::KvPair => "KvPair",
        TapeKind::Reserved => "Reserved",
    }
}

/// Minimal structural summary of a parsed tape. Compared against a
/// checked-in JSON golden to detect codegen regressions.
#[derive(Debug, PartialEq, Eq)]
struct TapeSummary {
    root_kind: &'static str,
    root_variant_idx: u8,
    root_children_count: usize,
    total_records: usize,
}

impl TapeSummary {
    /// Compute the summary by walking the parsed tape once from the
    /// root view's cursor. `root_kind` stores the enum variant's
    /// canonical debug name (via a hand-rolled match — we don't use
    /// `Debug::fmt` so the string stays a `&'static str`).
    fn from_cursor(cursor: TapeCursor<'_>, total_records: usize) -> Self {
        let root_kind = kind_name(cursor.kind());
        let root_variant_idx = cursor.variant_idx();
        let root_children_count = cursor.children().count();
        TapeSummary {
            root_kind,
            root_variant_idx,
            root_children_count,
            total_records,
        }
    }

    /// Serialize to the JSON shape we check in under
    /// `tests/fixtures/tape_golden/`. Hand-rolled — we don't want to
    /// pull in serde for a four-field struct.
    fn to_json(&self) -> String {
        format!(
            "{{\n  \"root_kind\": \"{}\",\n  \"root_variant_idx\": {},\n  \"root_children_count\": {},\n  \"total_records\": {}\n}}\n",
            self.root_kind, self.root_variant_idx, self.root_children_count, self.total_records,
        )
    }

    /// Inverse of `to_json` — extracts the four fields from a
    /// checked-in golden file. Tolerates extra whitespace but not
    /// field reordering (the fields are always written in the order
    /// above).
    fn from_json(raw: &str) -> Option<Self> {
        fn extract_str<'a>(s: &'a str, key: &str) -> Option<&'a str> {
            let needle = format!("\"{}\": \"", key);
            let start = s.find(&needle)? + needle.len();
            let end = start + s[start..].find('"')?;
            Some(&s[start..end])
        }
        fn extract_num(s: &str, key: &str) -> Option<u64> {
            let needle = format!("\"{}\":", key);
            let start = s.find(&needle)? + needle.len();
            let rest = s[start..].trim_start();
            let end = rest.find(|c: char| !c.is_ascii_digit())?;
            rest[..end].parse().ok()
        }
        let root_kind_str = extract_str(raw, "root_kind")?;
        // Interned static slices so the comparison in
        // assert_eq matches the live-computed `&'static str`.
        let root_kind: &'static str = match root_kind_str {
            "None" => "None",
            "Span" => "Span",
            "Epsilon" => "Epsilon",
            "Literal" => "Literal",
            "Regex" => "Regex",
            "Seq" => "Seq",
            "Alt" => "Alt",
            "Repeat" => "Repeat",
            "Rule" => "Rule",
            "VariantTag" => "VariantTag",
            "MapValue" => "MapValue",
            "TokenDispatch" => "TokenDispatch",
            "Recovered" => "Recovered",
            "KvPair" => "KvPair",
            "Reserved" => "Reserved",
            _ => return None,
        };
        Some(TapeSummary {
            root_kind,
            root_variant_idx: extract_num(raw, "root_variant_idx")? as u8,
            root_children_count: extract_num(raw, "root_children_count")? as usize,
            total_records: extract_num(raw, "total_records")? as usize,
        })
    }
}

/// Resolve the on-disk path for a given grammar + input pair's
/// golden snapshot. The fixture directory is created lazily on
/// first write.
fn golden_path(grammar: &str, input: &str) -> PathBuf {
    // Tests run from `crates/core`. Fall back to the workspace root
    // layout in case a caller invokes via `cargo test --workspace`.
    let base_candidates = [
        PathBuf::from("tests/fixtures/tape_golden"),
        PathBuf::from("crates/core/tests/fixtures/tape_golden"),
    ];
    for base in &base_candidates {
        if base.exists() {
            return base.join(grammar).join(format!("{}.json", input));
        }
    }
    // First run — fall back to the crate-local path and let the
    // write step create the directory.
    PathBuf::from("tests/fixtures/tape_golden")
        .join(grammar)
        .join(format!("{}.json", input))
}

/// Core assertion — parse `input` through the given grammar, walk
/// the tape, compute a structural summary, and diff against the
/// golden. Generates the golden on first run.
fn assert_tape_parity<G>(grammar: &'static str, input_name: &str, input: &str)
where
    G: ParseGrammar,
    for<'tape> <G as Root>::View<'tape>: HasCursor<'tape>,
{
    let parsed = match G::parse_input(input) {
        Ok(p) => p,
        Err(e) => panic!(
            "{}/{}: parse failed with {:?}",
            grammar, input_name, e
        ),
    };
    let total_records = parsed.tape().len();
    let view = parsed.view();
    // Views are cursor-backed; every generated view type exposes a
    // `.cursor()` accessor for raw-tape inspection. The universal
    // accessor set is defined by `generate_views` in
    // `backend/rust/view/mod.rs`. We lift the accessor into
    // `HasCursor` so the harness stays generic over grammars.
    let cursor: TapeCursor<'_> = view.cursor_of();
    assert_ne!(
        cursor.kind(),
        TapeKind::None,
        "{}/{}: root cursor reported TapeKind::None",
        grammar,
        input_name
    );
    let summary = TapeSummary::from_cursor(cursor, total_records);

    let path = golden_path(grammar, input_name);
    if let Some(parent) = path.parent() {
        if !parent.exists() {
            fs::create_dir_all(parent).expect("failed to create golden dir");
        }
    }
    match fs::read_to_string(&path) {
        Ok(raw) => {
            let golden = TapeSummary::from_json(&raw).unwrap_or_else(|| {
                panic!(
                    "{}/{}: corrupt golden at {} — delete it to regenerate",
                    grammar,
                    input_name,
                    path.display()
                )
            });
            assert_eq!(
                summary, golden,
                "{}/{}: tape summary diverged from golden at {}",
                grammar,
                input_name,
                path.display()
            );
        }
        Err(_) => {
            // First run — write the golden.
            fs::write(&path, summary.to_json()).expect("failed to write golden");
            eprintln!(
                "[tape_parity] wrote new golden {} for {}/{}",
                path.display(),
                grammar,
                input_name
            );
        }
    }
}

// ─── Malformed-input test ────────────────────────────────────────────

/// `parse()` must return `Err` on obviously broken input. Uses
/// unbalanced JSON as the canonical check — every grammar rejects
/// this with a syntax error, but we only exercise the JSON path
/// because constructing a "universal broken input" is ill-defined.
#[test]
fn json_rejects_malformed() {
    let input = "{ \"key\": [1, 2, ";
    let parsed = JsonGrammar::parse(input);
    assert!(
        parsed.is_err(),
        "expected parse failure on truncated JSON, got Ok"
    );
}

// ─── JSON test cases (5) ─────────────────────────────────────────────

#[test]
fn json_canada_tape_parity() {
    let input = load("json/canada.json");
    assert_tape_parity::<JsonGrammar>("json", "canada", &input);
}

#[test]
fn json_twitter_tape_parity() {
    let input = load("json/twitter.json");
    assert_tape_parity::<JsonGrammar>("json", "twitter", &input);
}

#[test]
fn json_citm_tape_parity() {
    let input = load("json/citm_catalog.json");
    assert_tape_parity::<JsonGrammar>("json", "citm_catalog", &input);
}

#[test]
fn json_data_tape_parity() {
    let input = load("json/data.json");
    assert_tape_parity::<JsonGrammar>("json", "data", &input);
}

#[test]
fn json_data_xl_tape_parity() {
    let input = load("json/data_xl.json");
    assert_tape_parity::<JsonGrammar>("json", "data_xl", &input);
}

// ─── CSS L4 test cases (4) ───────────────────────────────────────────

#[test]
fn css_bootstrap_tape_parity() {
    let input = load("css/bootstrap.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "bootstrap", &input);
}

#[test]
fn css_normalize_tape_parity() {
    let input = load("css/normalize.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "normalize", &input);
}

#[test]
fn css_tailwind_tape_parity() {
    let input = load("css/tailwind.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "tailwind", &input);
}

#[test]
fn css_test_import_tape_parity() {
    let input = load("css/test_import.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "test_import", &input);
}

// ─── BBNF self-hosted test cases (3) ─────────────────────────────────

#[test]
fn bbnf_self_hosted_bbnf_tape_parity() {
    let input = load_grammar_sample("bbnf/bbnf.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "bbnf", &input);
}

#[test]
fn bbnf_expressions_tape_parity() {
    let input = load_grammar_sample("bbnf/expressions.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "expressions", &input);
}

#[test]
fn bbnf_types_tape_parity() {
    let input = load_grammar_sample("bbnf/types.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "types", &input);
}

// ─── Google Sheets test cases (3) ────────────────────────────────────

#[test]
fn sheets_simple_formula_tape_parity() {
    // Google Sheets corpus is derived from the benchmark suite; a
    // small hand-written formula exercises the expression path.
    let input = "=SUM(A1:A10) + AVERAGE(B1:B10)";
    assert_tape_parity::<SheetsGrammar>("sheets", "simple", input);
}

#[test]
fn sheets_nested_if_tape_parity() {
    let input = "=IF(A1>10, IF(B1<5, \"low\", \"mid\"), \"high\")";
    assert_tape_parity::<SheetsGrammar>("sheets", "nested_if", input);
}

#[test]
fn sheets_arithmetic_tape_parity() {
    let input = "=(1+2)*3-4/5";
    assert_tape_parity::<SheetsGrammar>("sheets", "arithmetic", input);
}

// ─── EBNF test cases (3) ─────────────────────────────────────────────

#[test]
fn ebnf_minimal_tape_parity() {
    let input = "digit = \"0\" | \"1\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "minimal", input);
}

#[test]
fn ebnf_expr_grammar_tape_parity() {
    let input = "expr = term , { \"+\" , term } ; term = factor , { \"*\" , factor } ; factor = digit ; digit = \"0\" | \"1\" | \"2\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "expr", input);
}

#[test]
fn ebnf_recursive_list_tape_parity() {
    let input = "list = \"[\" , [ item , { \",\" , item } ] , \"]\" ; item = \"a\" | \"b\" | \"c\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "recursive_list", input);
}

// ─── View smoke tests — root kind + children ────────────────────────
//
// These are tighter, golden-free sanity checks that do not depend on
// the structural summary. They fail fast if the view layer is
// fundamentally miswired (e.g., the root cursor points at a leaf
// instead of a Rule compound) without waiting for a corpus diff.

#[test]
fn json_root_is_compound() {
    let input = "{\"key\": [1, 2, 3]}";
    let parsed = JsonGrammar::parse(input).expect("parse");
    let view = parsed.view();
    let cursor = view.cursor_of();
    assert!(
        matches!(
            cursor.kind(),
            TapeKind::Rule | TapeKind::Seq | TapeKind::Alt
        ),
        "json root cursor kind = {:?}, expected a compound",
        cursor.kind()
    );
    assert!(cursor.children().count() >= 1);
}

#[test]
fn ebnf_root_has_at_least_one_rule() {
    let input = "a = \"x\" ;";
    let parsed = EbnfGrammar::parse(input).expect("parse");
    let view = parsed.view();
    let cursor = view.cursor_of();
    assert!(cursor.children().count() >= 1);
}

#[test]
fn bbnf_total_records_nonzero() {
    let input = "start = \"a\" | \"b\" ;";
    let parsed = BbnfGrammar::parse(input).expect("parse");
    assert!(parsed.tape().len() > 0, "bbnf parse produced empty tape");
}
