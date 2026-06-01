---
agent: CH6
pass: T-P1-TOTALITY-EXCAVATION
cycle: V4
campaign: SK-V18
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
audited_head: 4e4aa0648
inputs_audited:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/ARCHITECTURE.md
  - restart/MASTER-PLAN.md
  - restart/locks/LOCKS.md
spot_verified_paths:
  - skinny/crates/runtime/src/tape/mod.rs:175 (phantom <G> census)
  - skinny/crates/runtime/src/grammars/json/event_grammar_witness.rs:4
  - skinny/crates/runtime/src/grammars/sheets_witness/event_grammar_witness.rs:4
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/config.rs:1-9 (zero W7)
  - skinny/crates/runtime/src/grammars/json/config.rs:22-26 (triad)
  - skinny/crates/runtime/src/grammars/json/generated.rs:12-15 (attach_structural_index NO-OP)
  - skinny/crates/runtime/src/grammars/json/scan.rs:22,47
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:2,9,257,393,394,899
  - skinny/crates/runtime/src/grammars/css_l4_declaration_values/parser.rs:42
  - skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:131,306,345
  - skinny/crates/codegen/src/lower/mod.rs:18-26
  - skinny/crates/codegen/src/lower/rust.rs:32,112
  - skinny/crates/codegen/src/lower/sink_only.rs:122
  - skinny/crates/codegen/src/lower/tape_plan.rs:58
  - skinny/crates/codegen/src/lower/eager_tape.rs:16
  - crates/core/src/grammar/generated/{json,css_l4,math}.rs:1 (AUTO-GENERATED header)
  - crates/core/src/grammar/generated/json.rs:701,719,732 (OnceCell probe)
  - crates/core/src/grammar/generated/math.rs (inert ScanState, 0 ensure_structural_index)
  - crates/core/src/runtime/json/value.rs:1 (@generated marker)
  - crates/ir/src/registry/strategy.rs:134-185,216 (9 idents + consumer)
  - skinny/RESULTS.md:5-25 (SOTA bench rows)
---

# CH6 Anti-Paper-Close — SK-V18 T-P1 V4 (cycle V4)

## Verdict

REVISE.

Score: 8 ACCEPT / 2 REVISE / 0 REJECT.

The six live inventories (1A-1F + the two 1F auxiliaries) are, in the main,
genuinely anti-paper-close: SOTA closure is split bench-backed (JSON) vs
directional-not-re-locked (CSS); the totality-tree `OnceCell` probe substrate is
held OPEN as an SK-V19 reconcile burden rather than laundered into a clean "NONE";
the CSS substrate-target row OPENS a gap (1A-DIV-005) instead of crediting the
removed mislabel as a resolution; every UNKNOWN carries a `verify_action`; and no
divergence is deferred to "a later inventory". I found NO "resolved/wired" self-
report lacking a live citation — the three `resolved` usages are all either a
spec-claim column, an explicitly-downgraded closure word, or a "fact, not a
resolution" framing.

Two closure surfaces exceed their cited live evidence and must be corrected:

1. `1C` C12 grants `IMPLEMENTED (IMPL-EXCEEDS-SPEC)` ("real xtask generator
   output") to the 9 totality grammar parsers resting ONLY on the
   `//! AUTO-GENERATED` header comment plus LOC/fn counts — the EXACT provenance
   fallacy the same inventory (C2/D4) disclaims ("markers do not equal generator
   output"). Internal contradiction; the closure word exceeds evidence.
2. `1D` G-5 and `1F-anti-pattern` cite the load-bearing "MEASURED Track-1 plane"
   FNV closure-support row at `css_cold_harness.rs:130 track1_full`, but the file
   is at `src/bin/css_cold_harness.rs` and `fn track1_full` is at `:131` (`:130`
   is the comment line). The substantive fence-opening claim is correct; the
   load-bearing measured-plane citation resolves to the wrong path and line.

## Live Verification (run from /Users/mkbabb/Programming/bbnf-lang at HEAD 4e4aa0648)

Phantom `<G>` non-test census (1A-SUB-023 / D5 / G-8 / COH18-008): every
`EventGrammar`/`AnyGrammar` site is the trait def, the `ValueRef<…>` field/impls
(`tape/mod.rs:175,183,185,191,202`), or the two witness DEFS
(`json/event_grammar_witness.rs:4`, `sheets_witness/event_grammar_witness.rs:4`).
NO non-test `ValueRef::<…Grammar>` instantiation on disk. Census EMPTY — verified.

CSS config W7 absence (1A-SUB-016 / 1A-DIV-005 / C-1): `rg -c
'W7_|BackendShape|substrate_target|admitted_fact_output'
css_l4_declaration_values/config.rs` = 0; head emits only ROW_ID / REQUEST_PROFILE
/ ENTRY_RULE / hashes / counts. JSON `config.rs:22-26` carries the full triad
(`W7_DIRECT_BACKEND_SHAPE="SinkOnly"` … `W7_SAME_SUBSTRATE_UNION="pass"`).
Verified — the gap is OPENED, not the impl exceeding spec.

JSON `attach_structural_index` NO-OP (1A-SUB-017): `generated.rs:12-15` is
`debug_assert_eq!(config::STRUCTURAL_BYTES, b"{}[],:\"")` then `let _ = state;`;
`scan.rs:22 scan_structurals`, `:47 structural_capacity_for` are the real scan.
Verified — no retained sidecar; the `impl_exceeds_spec` direction holds.

`select_lowering` 5-shape (1B / 1E / 1D G-3): `lower/mod.rs:18` matches
`cost.chosen` on exactly Eager/Offset/Event/SinkOnly/Collapsed → static lowerings.
`lower/rust.rs:32 lower_to_rust`, `:112 validate_policy_facts`,
`sink_only.rs:122 lower_program` (real AST walk), `tape_plan.rs:58 render_rule`
(marker string `enter_rule(...)`), `eager_tape.rs:16 lower_rule → render_rule(Eager)`,
`lower/collapsed_stage.rs` exists (not `collapsed_tape.rs`). Verified.

`OnceCell` probe breadth 8/9 (1F-anti / 1E No-candidates / CH5-V3-003): all 9
`grammar/generated/*.rs` declare the `OnceCell<…StructuralIndex>` field, but
`ensure_structural_index` count is 0 for `math.rs`, ≥2 for the other 8;
`google_sheets.rs` has 3. `math.rs` carries the documented inert `ScanState` shell.
`json.rs:701/719/732` confirmed. Verified — 8/9 active is exact.

`@generated` / provenance census (C2/D4): `rg -ln '@generated'
crates/core/src/runtime/` = 67; `json/value.rs:1` = "// @generated by xtask
regen-json…". `grammar/generated/{json,css_l4,math}.rs:1` =
"//! AUTO-GENERATED from `[workspace.metadata.bbnf.grammars]`". Pattern H = 71
total / 67 per-grammar. Runtime leak scan = 12 sites / 4 files. All verified.

9-grammar idents table (COH18-005 / COH18-012 / 1F-anti): `strategy.rs` `idents`
rows at `:137,143,149,155,161,167,173,179,185` are the nine grammar-named arrays;
consumer `for_grammar_with_manifest(…, PRODUCTION_MANIFEST_TABLE)` at `:216`.
Verified.

SOTA bench rows (D-9 / J-1 / L08 / COH18-013): `RESULTS.md` rows back the cited
numbers exactly — twitter parse_only Track1 `8349.290 > sonic 4913.095`; citm
`9079.838 > 8335.772`; canada `16709.901 > 12970.929`; all `GO`/`strict`/per-iter
equality PASS. CSS half is cited only to a synthesis-doc assertion and is graded
DIRECTIONAL not re-locked across 1D/1E/1F. Verified — the closure word is split.

FNV production path (G-5 / 1F-anti): `css_l4_…/generated.rs:393` push `input_fnv64`,
`:394 fnv64(input.as_bytes())`, `:899 fn fnv64`; `parser.rs:42 parse_full →
generated::emit_full_parse`; `bin/css_cold_harness.rs:131 fn track1_full → parse_full`,
dispatched at `:306`/`:345`. Substantive claim verified; CITATION DEFECTS noted below.

x86 = 28 files; 7 css_l4 `generated.rs` share single md5 `b654562c…`. Verified.

## Findings

| id | disposition | target | finding | evidence | required fold |
|---|---|---|---|---|---|
| CH6-V4-001 | ACCEPT | 1E L05 | Lock 5 closure is scoped `implemented (partial / Rust-only)` and explicitly states "the generator DOES NOT EXIST … two forked hand-written parsers + 7 replicas. The per-backend lower contract is honoured in shape, not in grammar-derivation." No broad honour claim; grammar-derivation kept open. | `1E-locks-evidence.md:85`; live `select_lowering` 5-shape at `skinny/crates/codegen/src/lower/mod.rs:18-26`. | None. |
| CH6-V4-002 | ACCEPT | 1A-SUB-016 / 1A-DIV-005 / 1C C-1 | The removed `W7_POLICY_BACKEND_SHAPE` mislabel is treated as "a fact, not a resolution"; it OPENS an opposite-direction gap (CSS substrate-target has ZERO config evidence) routed to 1A-DIV-005 + 1A-UNK-004 — NOT credited as impl_exceeds_spec. | `1A-substrate-evidence.md:88`; live `rg -c 'W7_|BackendShape|substrate_target' css_l4_declaration_values/config.rs` = 0; JSON triad at `json/config.rs:22-26`. | None. |
| CH6-V4-003 | ACCEPT | C2/D4, 1A-SUB-015/022, COH18-007 | `@generated` headers are treated as file-state evidence only — "markers do not equal generator output" (C2/D4) — never as provenance closure; round-trip regen kept open at 1A-UNK-003, 1D U-1, COH18-007. | `1C-runtime-evidence.md:31,55`; live 67 markers, `json/value.rs:1`. | None. |
| CH6-V4-004 | ACCEPT | UNKNOWN routing | Every UNKNOWN carries a concrete `verify_action`: 1A-UNK-001..005, 1B U1-U3, 1C U1-U4, 1D U-1..U-5, 1E-V5-U1..U3, U-COH18-001..002. No future-inventory deferral; 1F-past-corpora has zero UNKNOWN (it is a do-not-re-derive ledger), correctly carrying no verify_action. | `1A:184-190`; `1B:147-164`; `1C:77-82`; `1D:211-248`; `1E:130-136`; `1F-coherence-scan.md:112-118`. | None. |
| CH6-V4-005 | REVISE | 1D G-5 + 1F-anti-pattern (FNV row) | The substantive "production telemetry, NOT bench-quarantined, fence-obligation" claim is CORRECT and anti-paper-close (it OPENS a fence, does not close one). BUT the load-bearing "MEASURED Track-1 plane" closure-support citation `css_cold_harness.rs:130 track1_full` is mis-pathed and mis-lined: the file is `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs` (under `bin/`), and `fn track1_full` is at `:131` — `:130` is the `// ---- track1_full` comment. The dispatch is at `:306`/`:345`. A load-bearing measured-plane anchor must resolve to the function, not the comment, and must carry the `bin/` segment. | live `grep -n 'fn track1_full' skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs` = `:131`; dispatch `:306`,`:345`; the bare-path `src/css_cold_harness.rs` does not exist. | In `1D-skinny-lessons.md:201` (G-5) and `1F-anti-pattern.md:41`, re-anchor the measured-plane cite to `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:131` (`fn track1_full`), and cite the bench dispatch `:306`/`:345` as the plane proof. |
| CH6-V4-006 | ACCEPT | Phantom `<G>` (1A-SUB-023, D5, G-8, COH18-008) | The "test-only / census EMPTY" basis for the DELETE is grounded with live evidence; closure words are scoped to enum/field def + witness DEFS, never to a production instantiation. The K-axis is explicitly PRESERVED. | live census (no non-test `ValueRef::<…Grammar>`); `1A-substrate-evidence.md:95`. | None. |
| CH6-V4-007 | REVISE | 1C C12 | C12 grants `IMPLEMENTED (IMPL-EXCEEDS-SPEC)` with the note "All 9 grammar PARSERS are real xtask generator output from `.bbnf`", but the ONLY live evidence cited is the `//! AUTO-GENERATED from [workspace.metadata.bbnf.grammars]` header comment plus LOC/fn counts — NO regen round-trip, generator manifest, or byte-equivalence proof. This is the EXACT provenance fallacy C2/D4 (SAME inventory) rejects ("markers do not equal generator output"; "marker present but FILES are still the hand-roster"). The round-trip is held open at 1A-UNK-003 and 1D U-1, yet C12 closes it. A closure verdict resting on a `@generated`/`AUTO-GENERATED` header alone exceeds live evidence. | `1C-runtime-evidence.md:41` (C12) vs `:31,55` (C2/D4 disclaimer); the only live witness is the header; cf. open route `1A-substrate-evidence.md:188` (1A-UNK-003), `1D-skinny-lessons.md:213-225` (U-1). | In `1C-runtime-evidence.md:41`, downgrade the verdict from `IMPLEMENTED (IMPL-EXCEEDS-SPEC)` to a header-scoped partial — e.g. `partial / @generated-header present; generator round-trip provenance UNKNOWN` — and route the provenance closure to 1A-UNK-003 / 1D U-1, consistent with C2/D4's own marker-disclaimer. The "ahead of skinny's runtime plane" framing may stay as a comparative note, not a closure verdict. |
| CH6-V4-008 | ACCEPT | OnceCell totality-tree carry (1E No-candidates, 1F-anti, CH5-V3-003) | The `crates/core` `OnceCell<StructuralIndex>` probe is NOT laundered into the skinny "NONE" universality: 1E explicitly scopes "NONE in the SKINNY benched tree" and carries the totality probe as an OPEN SK-V19 Lock-1 reconcile burden ("do NOT close substrate-union 'BOTH trees' while this is unclassified"). Breadth "8 of 9" verified exact. | `1E-locks-evidence.md:158`; live `ensure_structural_index` 0 for math, ≥2 for the other 8; skinny `bbnf-simd` carries 0 `OnceCell<StructuralIndex>`. | None. |
| CH6-V4-009 | ACCEPT | L08 / D-9 / G-3 / J-1 SOTA + decision-spine closure | The SOTA closure word is split: JSON is bench-row-backed (`RESULTS.md:5-25` verified) and admitted `impl_exceeds_spec`; CSS is DIRECTIONAL-not-re-locked with the H1 `css_canon_bench` re-lock gate named. The decision-spine "load-bearing" verdict is graded "PROVED at admitted scope; selection DEPTH pending" with the Sheets precedence-tower stressor held open (1B U1 independently keeps the vacuity question open). No over-close. | live RESULTS rows (twitter/citm/canada `>` sonic verified); `1D-skinny-lessons.md:67,199`; `1E-locks-evidence.md:88`; `1B-codegen-evidence.md:149` (U1). | None. |
| CH6-V4-010 | ACCEPT | 1A-SUB-024 / C11 substrate-neutral closure | The `implemented (substrate-neutral confirmed)` / `no second substrate` closure rests on a STRUCTURAL fact (`struct CssDocument { tape: Tape<'input> }` holding the existing `Tape` type, `use crate::tape::{…}` at `:2`, reuse of the existing sparse flag pair at `:9`), not on the doc comment alone; and `tape/cursor.rs`/G4b are explicitly kept unimplemented. Bounded closure, distinct from C12's header-only basis. | live `css_l4_declaration_values/generated.rs:2,9,257-258`; `1A-substrate-evidence.md:96`. | None. |

## Note on the REVISE rate

This lens returned 8/10 ACCEPT (80%). The packet's anti-paper-close discipline is
substantively strong — closure words are split, gaps are opened not credited, the
totality `OnceCell` carry is held open, and no UNKNOWN lacks a `verify_action`. The
two REVISEs are not rubber-stamp dissent: CH6-V4-007 is a genuine internal
contradiction (a closure verdict resting on the exact header the same inventory
disclaims), and CH6-V4-005 is a load-bearing measured-plane anchor that resolves to
the wrong path and line. Both were surfaced by direct disk verification, not recall.

TALLY accept=8 revise=2 reject=0
