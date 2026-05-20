# SK-V11 W1b R3: Codegen / json_provider Boundary

Pass: W1b Phase 1 research.
Date: 2026-05-20.
Scope: minimal generated non-JSON baseline path that proves no generic JSON policy leaks.
Output: this file only.

## Read Set

- `restart/skinny/tranches/sk-v11/SPEC.md` §5 W1b and §6 W2.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md` W1b/W2 sections.
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/direct_schema.rs`
- `skinny/crates/codegen/src/json_provider.rs`
- JSON generated/template paths under `skinny/crates/codegen/src/json_templates/` and `skinny/crates/runtime/src/grammars/json/`
- Existing generated typed path `skinny/crates/bbnf-bench/src/generated_real_typed.rs`

## Boundary Finding

W1b must create one generated non-JSON baseline plus an independent oracle, but it must not land a behavior intervention or claim row admission. SPEC §5 names the owner paths and requires: generated Track 1 non-JSON baseline, independent oracle/Track 2, strict equality, throughput metadata, no generic JSON policy, and no JSON `RESULTS.md` movement (`SPEC.md:327-378`). W2 is separate: it consumes W1b and is the first place where a CSS L4 intervention may admit, with a `ceil(W1b_css_baseline_mbps * 1.01)` threshold (`SPEC.md:380-432`).

The live codegen path is not currently grammar-neutral runtime generation. Both runtime emission and typed emission call `json_provider::ensure_runtime_profile` before lowering (`skinny/crates/codegen/src/lib.rs:102-168`). `json_provider` accepts only `backend.grammar_name == "json"` and otherwise errors, then emits JSON-named modules/types/templates (`skinny/crates/codegen/src/json_provider.rs:4-73`). Therefore W1b cannot treat `json_provider` emission as a generality proof.

There is a narrower usable boundary:

- `lower::sink_only` is mostly grammar-data lowering. It records rule names, direct shapes, span kinds, literals, and dispatch alt count from BIR without JSON-named runtime policy (`skinny/crates/codegen/src/lower/sink_only.rs:19-130`).
- `lower::schema_direct` is likewise a thin `SinkOnlyProgram + DirectSchemaSet` adapter. It validates host/API schema facts and requires DirectBuild shapes/literals (`skinny/crates/codegen/src/lower/schema_direct.rs:5-27`).
- `direct_schema.rs` is a host/API schema contract surface, not JSON policy by itself (`skinny/crates/codegen/src/direct_schema.rs:3-120`).
- `sink_direct.rs` is not a W1b-safe generic renderer today because it emits `JsonSink`, JSON parse errors, and JSON value dispatch (`skinny/crates/codegen/src/sink_direct.rs:72-120`).
- `typed_direct.rs` is less JSON-named at the type API level, but its parser runtime still hardcodes JSON string, bool/null, number, whitespace, and skip semantics via `match_string_at_quote_trusted_utf8`, `unescape_string`, `true`/`false`/`null`, and JSON number helpers (`skinny/crates/codegen/src/typed_direct.rs:20-29`, `skinny/crates/codegen/src/typed_direct.rs:480-510`). It is acceptable as inventory, not as a non-JSON generated parser without either per-grammar template facts or an explicit "left untouched / bypassed" proof.

## Minimal W1b Baseline Path

The minimal baseline should be CSS L4 declaration values unless CHALLENGE selects Sheets or BBNF-self. CSS is preferred by SPEC and by the preblocked ledger because W2 is explicitly CSS L4 intervention work. The baseline path should be baseline-only:

1. Select exactly one target: `grammar_id=css_l4`, `corpus=declaration_values`, `workload=typed` or `direct`, one output plane.
2. Generate Track 1 through a new non-JSON baseline harness that bypasses `json_provider` entirely. The proof should demonstrate the selected parser path does not call `emit_from_source`, `emit_typed_from_source`, `emit_with_layout`, `emit_typed_with_layout`, or any `json_provider::*` function.
3. Keep the live JSON provider untouched in W1b unless the implementation replaces it with a grammar-neutral template and proves JSON generated output remains isolated. The smaller W1b path is "bypassed and left untouched."
4. Use `lower::sink_only` / `lower::schema_direct` only if the selected non-JSON generated baseline can consume BIR/source facts without introducing JSON-shaped renderer policy. If this is too large, create a selected-target generated baseline harness under bench/runtime owner paths and record that generic codegen remains untouched.
5. Add or name an independent oracle/Track 2 that does not call generated Track 1, generated SinkOnly helpers, generated typed helpers, or benchmark-private parser code. Digest output is allowed only as an oracle/output plane, not as parser substrate.
6. Feed the W1a non-JSON gate/report lane with run id, host, flags, sample count, output plane, oracle status, generated Track 1 Mbps, and oracle/Track 2 Mbps.
7. Assert no behavior admission: W1b row is baseline authority only; `skinny/RESULTS.md` JSON rows do not move; no C1-C7 primitive lands.

This proves "no generic JSON policy leaks" by negative and positive evidence:

- Negative: the selected non-JSON baseline does not transit `json_provider`, `json_templates`, `sink_direct`, generated JSON runtime, or JSON bench Track 1.
- Positive: the gate consumes a same-run non-JSON generated Track 1 and an independent oracle/Track 2 for one selected output plane.

## Exact Edit Surface For Implementation

Must be allowed to edit in W1b implementation if needed:

- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/benches/` for the one selected baseline benchmark.
- One selected generated non-JSON runtime/bench module under `skinny/crates/runtime/src/grammars/` or `skinny/crates/bbnf-bench/src/`, named for the selected target.
- One selected grammar input family only: `grammar/css/l4/` if CSS is selected; otherwise `grammar/google-sheets/google-sheets.bbnf` or `grammar/bbnf/`.
- A selected oracle/Track 2 module or fixture/report file, if not already present.
- `restart/skinny/tranches/sk-v11/research/w1b/` for research/challenge notes.

Conditionally editable only if the implementation genuinely uses codegen source:

- `skinny/crates/codegen/src/lib.rs`: only to add a grammar-neutral bypass API or proof hook; do not route non-JSON through `json_provider`.
- `skinny/crates/codegen/src/lower/`: only for BIR-to-neutral metadata/lowering needed by the selected baseline.
- `skinny/crates/codegen/src/direct_schema.rs`: only for host/API schema facts needed by the selected typed output plane.

Must not be edited for the minimal W1b baseline:

- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/codegen/src/typed_direct.rs` unless CHALLENGE explicitly accepts a per-grammar typed-template change; current JSON-shaped parser runtime makes this a W2-or-later risk, not the minimal W1b baseline.
- `skinny/crates/parse-that-regex/src/`
- `skinny/crates/bbnf-simd/src/` and `skinny/crates/bbnf-simd/tests/`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` except on a failed implementation/redress path, not research.

## W2 Hand-off Constraint

W2 must not create the first measurable non-JSON baseline. It consumes W1b's selected CSS baseline, then wires exactly one C1-C6 primitive family with C7 support if needed. Any W2 generic/codegen/runtime-outside-JSON edit must prove: generated non-JSON Track 1, independent Track 2/oracle, strict equality, improvement over W1b, and no JSON policy in generic crates or generated runtime templates. If W1b cannot produce the baseline without crossing `json_provider`, W2 should be REVISE/BLOCKED rather than combining baseline and intervention.
