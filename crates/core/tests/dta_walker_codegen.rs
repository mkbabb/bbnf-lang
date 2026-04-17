//! AW-III.W4.b — Codegen tests for the specialised DTA walker emitter.
//!
//! These tests exercise the emitter pass directly (no compile cycle).
//! They consume `bbnf_ir`'s lifted DTA tables for representative
//! grammars and confirm:
//!
//! 1. The emitter produces a `pub fn dta_run_<grammar>` declaration.
//! 2. The outer dispatch loop has one match arm per state id.
//! 3. ByteDispatch states inline their LUT verbatim as match arms
//!    over byte literals (LLVM jump-table input).
//! 4. The emitter has no behavioural branch on grammar identity per
//!    the §6 invariant.
//!
//! Tests run on the IR-side `lift_dta` output for fabricated grammars
//! plus the bootstrap-emitted `BbnfBootstrap` to keep the dependency
//! graph minimal.

use bbnf::backend::rust::emitter::dta_walker::{
    emit_specialised_walker, walker_fn_ident, HotColdPartition, HOT_BUDGET,
};
use bbnf_ir::passes::profile::GrammarProfile;
use bbnf_ir::passes::recognizers::dta::{
    DtaState, DtaTable, FrameKind, LiteralPayload, SeqPromote, StateId,
};
use bbnf_ir::passes::sets::StructuralAlphabet;
use bbnf_ir::GrammarIR;
// `RuleId` / `StringId` are u32 aliases — used inline below for clarity.

/// Construct a small synthetic table covering Literal + ByteDispatch.
/// The table mirrors a JSON-like entry shape — one byte-dispatched
/// entry that selects between `null` / `true` / `false` literal arms.
fn synth_byte_dispatch_table() -> DtaTable {
    let lit_null = StateId(0);
    let lit_true = StateId(1);
    let lit_false = StateId(2);
    let dispatch_state = StateId(3);

    // ByteDispatch table: 256 NONE entries except for 'n' / 't' / 'f'.
    let mut dispatch = vec![StateId::NONE; 256];
    dispatch[b'n' as usize] = lit_null;
    dispatch[b't' as usize] = lit_true;
    dispatch[b'f' as usize] = lit_false;

    let mut rule_entries = std::collections::HashMap::new();
    rule_entries.insert(0u32, dispatch_state);

    DtaTable {
        states: vec![
            // 0: Literal "null"
            DtaState::Literal {
                text: 0u32,
                payload: LiteralPayload::None,
            },
            // 1: Literal "true"
            DtaState::Literal {
                text: 1u32,
                payload: LiteralPayload::None,
            },
            // 2: Literal "false"
            DtaState::Literal {
                text: 2u32,
                payload: LiteralPayload::None,
            },
            // 3: ByteDispatch over the three first-bytes
            DtaState::ByteDispatch {
                table: dispatch,
                fallback: None,
            },
        ],
        rule_entries,
        shunting_yard_chains: std::collections::HashMap::new(),
        counter_optional_rules: std::collections::HashMap::new(),
        max_nesting_depth: 1,
        entry: 0u32,
    }
}

/// Construct a table with > HOT_BUDGET states to exercise the cold-
/// sibling emission path. Each state is a no-op `Epsilon` so the
/// partition's structural decisions are the only output the test
/// observes.
fn synth_oversized_table() -> DtaTable {
    let n = HOT_BUDGET + 8;
    let states: Vec<DtaState> = (0..n).map(|_| DtaState::Epsilon).collect();
    let mut rule_entries = std::collections::HashMap::new();
    rule_entries.insert(0u32, StateId(0));
    DtaTable {
        states,
        rule_entries,
        shunting_yard_chains: std::collections::HashMap::new(),
        counter_optional_rules: std::collections::HashMap::new(),
        max_nesting_depth: 1,
        entry: 0u32,
    }
}

/// Confirm the walker function ident sanitises grammar identifiers
/// that contain non-Rust-ident characters.
#[test]
fn walker_fn_ident_sanitises_grammar_name() {
    let id = walker_fn_ident("css/l4");
    assert_eq!(id.to_string(), "dta_run_css_l4");
    let id = walker_fn_ident("BbnfBootstrap");
    assert_eq!(id.to_string(), "dta_run_BbnfBootstrap");
    let id = walker_fn_ident("some-grammar");
    assert_eq!(id.to_string(), "dta_run_some_grammar");
}

/// Confirm the emitter produces a `pub fn dta_run_<grammar>`
/// declaration for a non-empty table. The function is the W4 hard-
/// gate's structural deliverable.
#[test]
fn emits_pub_fn_dta_run_per_grammar() {
    let table = synth_byte_dispatch_table();
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("synth", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    assert!(
        s.contains("pub fn dta_run_synth"),
        "emitted code lacks `pub fn dta_run_synth`:\n{s}",
    );
    assert!(
        s.contains("__dta_walker_inline"),
        "emitted code lacks the inner walker module",
    );
}

/// Confirm every state id in the table reaches an arm in the outer
/// dispatch `match cur`. The `Literal::u16_unsuffixed(N)` entries
/// emit as bare `N` integer literals; we grep for the expected match-
/// arm structure.
#[test]
fn outer_match_has_arm_per_state() {
    let table = synth_byte_dispatch_table();
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("synth", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    // Every state id (0..table.states.len()) appears as a match arm
    // ahead of the wildcard. Searching for "X => " is a structural
    // proxy — the emitter format keeps each arm on its own logical
    // production.
    for idx in 0..table.states.len() {
        let needle = format!("{idx}usize ");
        // proc_macro2 emits unsuffixed u16 literals as bare digits,
        // so search for the bare integer followed by `=>`.
        let bare_arm = format!("{idx} =>");
        assert!(
            s.contains(&bare_arm) || s.contains(&needle),
            "state id {idx} has no dispatch arm (looked for `{bare_arm}` / `{needle}`)",
        );
    }
}

/// Confirm ByteDispatch states inline their LUT as a `match input[pos]`
/// over byte literals. The const-folded inlining is the W4 jump-table
/// win — every byte that maps to a non-NONE state appears as an arm
/// `<byte_lit> => __StepOutcome::Next(<state_lit>)`.
#[test]
fn byte_dispatch_inlined_as_match_arms() {
    let table = synth_byte_dispatch_table();
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("synth", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    // The inlined ByteDispatch arm reads `input[pos]` and matches on
    // the byte. Every dispatch byte the test seeded ('n', 't', 'f')
    // appears as a numeric literal in the emitted code. AW-III.W4.d
    // threads `pos: &mut u32` through the walker so the byte-peek
    // dereferences the pointer (`*pos`).
    assert!(
        s.contains("input . get (* pos as usize)")
            || s.contains("input.get(*pos as usize)"),
        "ByteDispatch arm lacks `input[*pos]` peek:\n{s}",
    );
    for byte in [b'n', b't', b'f'] {
        let needle = format!("{byte}u8");
        let bare = format!("{byte} =>");
        assert!(
            s.contains(&needle) || s.contains(&bare),
            "ByteDispatch arm missing byte literal {byte} (looked for `{needle}` / `{bare}`)",
        );
    }
    // The kind tag string identifies the inlined ByteDispatch arm.
    assert!(
        s.contains("byte_dispatch"),
        "ByteDispatch arm lacks the kind-tag annotation",
    );
}

/// Confirm hot/cold partitioning splits states above HOT_BUDGET into
/// `#[cold] #[inline(never)]` siblings; below the budget the entire
/// table inlines.
#[test]
fn hot_cold_partition_inlines_under_budget() {
    let table = synth_byte_dispatch_table();
    let partition = HotColdPartition::for_table(&table);
    assert!(
        partition.cold.is_empty(),
        "table fits within HOT_BUDGET; no cold siblings expected (got {} cold states)",
        partition.cold.len(),
    );
    assert_eq!(partition.hot.len(), table.states.len());
}

/// Confirm hot/cold partitioning emits cold siblings for tables
/// exceeding HOT_BUDGET. The emitted code includes
/// `#[cold] #[inline(never)] fn __cold_state_<id>(...)` declarations
/// for every cold state.
#[test]
fn hot_cold_partition_emits_cold_siblings_above_budget() {
    let table = synth_oversized_table();
    let partition = HotColdPartition::for_table(&table);
    assert!(
        !partition.cold.is_empty(),
        "table exceeds HOT_BUDGET; cold siblings expected",
    );
    assert_eq!(
        partition.hot.len() + partition.cold.len(),
        table.states.len(),
        "hot ∪ cold must equal the full state set",
    );
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("oversized", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    assert!(
        s.contains("# [cold]") || s.contains("#[cold]"),
        "emitted code lacks `#[cold]` cold-sibling annotation",
    );
    assert!(
        s.contains("__cold_state_"),
        "emitted code lacks `__cold_state_*` sibling fn declarations",
    );
}

/// Confirm the empty-table branch produces a callable `pub fn` that
/// returns `InvalidState`. Empty-table grammars exist because the
/// lifter may emit no states for transparent-only grammars; the
/// walker surface stays available so the surrounding `parse()` can
/// route uniformly.
#[test]
fn empty_table_emits_callable_stub() {
    let mut rule_entries = std::collections::HashMap::new();
    rule_entries.insert(0u32, StateId::NONE);
    let table = DtaTable {
        states: Vec::new(),
        rule_entries,
        shunting_yard_chains: std::collections::HashMap::new(),
        counter_optional_rules: std::collections::HashMap::new(),
        max_nesting_depth: 0,
        entry: 0u32,
    };
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("empty", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    assert!(
        s.contains("pub fn dta_run_empty"),
        "empty-table walker lacks `pub fn dta_run_empty`",
    );
    assert!(
        s.contains("InvalidState"),
        "empty-table walker should return `InvalidState`",
    );
}

/// §6 invariant proof: the emitter produces structurally-identical
/// scaffolding regardless of the grammar name. Two different grammar
/// names with identical IR produce token streams that differ ONLY in
/// the `dta_run_<name>` symbol. Every other line is byte-identical.
///
/// This is the test the hard gate calls out as a §6 generalisation
/// proof: the pass body never branches on grammar identity.
#[test]
fn emit_is_grammar_name_agnostic() {
    let table = synth_byte_dispatch_table();
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens_a = emit_specialised_walker("alpha", &GrammarIR::default(), &table, &alphabet, &profile);
    let tokens_b = emit_specialised_walker("beta", &GrammarIR::default(), &table, &alphabet, &profile);
    let s_a = tokens_a.to_string();
    let s_b = tokens_b.to_string();
    // Every difference between the two outputs must be a substitution
    // of the grammar name. Strip the grammar-specific symbol
    // identifiers and compare.
    // Strip every per-grammar symbol the W1.α / W1.β emitters mint —
    // `dta_run_<g>`, `__regex_scan_<g>`, `__dfa_match_<g>_<state_idx>` —
    // so the §6 proof reduces to "the rest of the token stream is
    // byte-identical between grammars."
    let stripped_a = s_a
        .replace("dta_run_alpha", "__GRAMMAR_RUN__")
        .replace("__regex_scan_alpha", "__GRAMMAR_REGEX_SCAN__")
        .replace("__dfa_match_alpha", "__GRAMMAR_DFA__");
    let stripped_b = s_b
        .replace("dta_run_beta", "__GRAMMAR_RUN__")
        .replace("__regex_scan_beta", "__GRAMMAR_REGEX_SCAN__")
        .replace("__dfa_match_beta", "__GRAMMAR_DFA__");
    assert_eq!(
        stripped_a, stripped_b,
        "emitter output differs beyond the grammar-name substitution; §6 violation"
    );
}

/// AW-III.W4.d — confirm the bridge collapsed. Earlier waves routed
/// every non-`ByteDispatch` arm through `__dispatch_via_cold` /
/// `dta_run_cold`; the W4.d inline-lowering closes that bridge so the
/// hot path never crosses into `dispatch_one`. The emitted module
/// references `StepResult` (the runtime outcome enum) directly; the
/// per-arm bodies advance `cur` via `StepResult::Next(<id>)`.
///
/// The check strips doc-comment tokens (`# [doc = ...]`) before
/// scanning so prose mentions of the cold-path symbols in the
/// emitter's own documentation do not trigger false positives.
#[test]
fn no_cold_path_bridge_in_emitted_code() {
    let table = synth_byte_dispatch_table();
    let alphabet = StructuralAlphabet::default();
    let profile = GrammarProfile::default();
    let tokens = emit_specialised_walker("synth", &GrammarIR::default(), &table, &alphabet, &profile);
    let s = tokens.to_string();
    let stripped = strip_doc_comments(&s);
    assert!(
        !stripped.contains("__dispatch_via_cold"),
        "emitted code still references the cold-path bridge — W4.d collapse incomplete",
    );
    assert!(
        !stripped.contains("dta_run_cold"),
        "emitted code still references `dta_run_cold` — hot path leaks into the cold dispatcher",
    );
    assert!(
        !stripped.contains("dispatch_one"),
        "emitted code still references `dispatch_one` — interpreter floor not removed",
    );
    assert!(
        s.contains("StepResult"),
        "emitted code lacks the runtime `StepResult` outcome enum reference",
    );
}

/// Strip `# [doc = r" ..."]` token sequences from the proc_macro2
/// `to_string()` output so the `no_cold_path_bridge_in_emitted_code`
/// check can scan for actual code mentions of the cold-path symbols
/// without tripping on prose in the emitter's own documentation.
fn strip_doc_comments(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        // Match the literal token sequence `# [doc = ` produced by
        // proc_macro2's `to_string()` for `#[doc = "..."]` attrs.
        let needle = b"# [doc = ";
        if i + needle.len() <= bytes.len() && &bytes[i..i + needle.len()] == needle {
            // Skip until the matching `]`. The doc literal is a Rust
            // string literal that proc_macro2 prints verbatim; the
            // closing `]` may be preceded by `"`. Walk until the
            // first `]` after a `"`.
            let mut j = i + needle.len();
            // Skip the opening `r"` or `"`.
            while j < bytes.len() && bytes[j] != b']' {
                j += 1;
            }
            // Move past the `]`.
            if j < bytes.len() {
                j += 1;
            }
            i = j;
        } else {
            out.push(bytes[i] as char);
            i += 1;
        }
    }
    out
}

// Stub so SeqPromote / FrameKind aren't unused — the synthetic table
// builders consume them for completeness.
#[test]
fn import_smoke() {
    let _ = (FrameKind::Seq, SeqPromote::Default);
}
