# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V11

Verdict: ACCEPT.

Confidence: 96%.

Target reviewed: `61d5cc3b` (`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Scope

CH2 reviewed the V11 fold after the V10 CH4 rejection for post-V6 cost cap
overflow and empty build metadata acceptance. The review focused on Lock 14,
grammar neutrality, no new directive/BIR/substrate/API/`BackendShape`/
`UnionTape` surface, report/gate-only telemetry, behavior-surface freeze, and
whether CSS L4 / Sheets / BBNF-self proof obligations were weakened.

## Evidence

- The CH2 standard is Lock 14 generality: no grammar-name leak and no JSON-only
  intervention that fails CSS L4 / Sheets / BBNF-self
  (`restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:88`).
  The convergence protocol also requires folded hardening before advancing
  (`restart/prompts/ORCHESTRATOR.md:104`-`restart/prompts/ORCHESTRATOR.md:121`).
- SPEC makes W0 report/gate telemetry mandatory and gate-consumed: every
  emitted SK-V8 field must be consumed by `gate-json`, and Lock 14 leaks reject
  the wave (`restart/skinny/tranches/sk-v8/SPEC.md:103`-`restart/skinny/tranches/sk-v8/SPEC.md:146`).
  The W0 exit gate remains no parser/scanner/SIMD/asm/codegen/product behavior
  or generated parser output change, with `gate-json` as the same-wave consumer
  (`restart/skinny/tranches/sk-v8/SPEC.md:346`-`restart/skinny/tranches/sk-v8/SPEC.md:361`).
- The generality gate still forbids public JSON API in generic crates, JSON-name
  grammar branches, generic JSON structural policy, and template/provider
  boundary leaks; non-JSON proof is required for generic CostFacts/codegen/
  runtime/SIMD/parser-template edits
  (`restart/skinny/tranches/sk-v8/SPEC.md:261`-`restart/skinny/tranches/sk-v8/SPEC.md:286`).
  SPEC's non-negotiables still forbid new directives, BIR variants,
  `BackendShape` variants, `UnionTape`, new substrate surfaces, public substrate
  APIs, sidecar/parallel substrate, and JSON policy in generic crates
  (`restart/skinny/tranches/sk-v8/SPEC.md:191`-`restart/skinny/tranches/sk-v8/SPEC.md:205`).
- V10's only blockers were CH4 cost/reproducibility issues: source fold over
  `<=120` and empty `arch`/`cpu`/`os`/`simd` metadata acceptance
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:29`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:45`;
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:92`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:118`).
  V10 CH2 had already accepted the generality surface
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:20`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:27`).
- The V11 source fold is one telemetry/report file only. `git diff --name-status
  e74d480d..61d5cc3b` reported only
  `M skinny/crates/bbnf-bench/src/report.rs`, and `git diff --shortstat
  00c3485a..61d5cc3b -- skinny/crates/bbnf-bench/src/report.rs` reported
  `1 file changed, 118 insertions(+), 13 deletions(-)`, satisfying the live
  post-V6 `<=120` report/gate/test/doc cap. No generic crate is in the V11 diff.
- The grammar-id JSON check remains inside `bbnf-bench` W0 report validation,
  not a generic crate branch (`skinny/crates/bbnf-bench/src/report.rs:322`).
  `gate-json` still validates schema-v3 and then `validate_sk_v8_w0()` before
  accepting or writing the report (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`-`skinny/crates/bbnf-bench/src/bin/gate.rs:328`).
- V11 closes the empty metadata acceptance without adding a generic policy
  surface. `validate_w0_manifest_semantics()` now requires exact W0 sentinels,
  exact build flags, non-empty host `arch`/`cpu`, non-empty feature `arch`/`os`/
  `simd`, exact `target_cpu=native`, and workload-local substrate telemetry
  tuples (`skinny/crates/bbnf-bench/src/report.rs:1007`-`skinny/crates/bbnf-bench/src/report.rs:1081`).
  The tuple helper is telemetry classification only: parse rows remain
  `borrowed_view_over_offset_tape / discarded_after_capacity / one`; direct rows
  remain `sink_only_digest / n/a / zero_or_inert`; typed rows remain
  `typed_direct_projection / n/a / zero_or_inert`
  (`skinny/crates/bbnf-bench/src/report.rs:1083`-`skinny/crates/bbnf-bench/src/report.rs:1094`).
- W0 still cannot become strict admission. Rows must remain
  `strictness=deferred`, validation path `view-boundary`, `parse_utf8=view-boundary`,
  and `escape_complete=yes` (`skinny/crates/bbnf-bench/src/report.rs:1096`-`skinny/crates/bbnf-bench/src/report.rs:1122`).
  The strict admission helper still rejects non-GO outcomes, non-strict rows,
  stale/historical/absent comparator freshness, and plane/validation mismatches
  (`skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`).
- Lock 14's executable baseline classifies `report.rs`, `gate.rs`,
  `bin/gate.rs`, and `lock14_baseline.rs` as `bench_gate_schema` /
  `telemetry_only`, while generic surfaces remain read-only
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:267`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:295`).
  Its validator checks the allowlist, frozen git roots, and `BackendShape`
  surface (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:340`).
  Frozen roots cover grammar/runtime/IR/passes/codegen/grammar/BBNF/SIMD/
  parser/generation/product surfaces
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:397`).
- `BackendShape` remains the five existing variants only
  (`skinny/crates/ir/src/lib.rs:401`-`skinny/crates/ir/src/lib.rs:408`), and the
  Lock 14 validator rejects a changed variant count or `UnionTape`
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:491`).
- Current `RESULTS.md` remains W0 telemetry/gate posture: rows are deferred and
  view-boundary (`skinny/RESULTS.md:3`-`skinny/RESULTS.md:5`), the manifest is
  present (`skinny/RESULTS.md:44`-`skinny/RESULTS.md:48`), and the note states
  `gate-json` consumes the manifest while native comparators are same-run and
  C++ sidecars are historical or absent, never W0 strict anchors
  (`skinny/RESULTS.md:138`-`skinny/RESULTS.md:141`).

## Commands

- `git status --short` was clean before creating this CH2 file.
- `git diff --name-status e74d480d..61d5cc3b` returned only
  `M skinny/crates/bbnf-bench/src/report.rs`.
- `git diff --name-only 0bd16f6d..61d5cc3b -- <Lock14 frozen roots>` returned
  no paths.
- `git diff --name-only e74d480d..61d5cc3b -- skinny/crates/runtime/src
  skinny/crates/ir/src skinny/crates/passes/src skinny/crates/codegen/src
  skinny/crates/grammar/src skinny/crates/bbnf/src skinny/crates/bbnf-simd/src
  skinny/crates/parse-that-regex/src` returned no paths.
- `git diff e74d480d..61d5cc3b -- . | rg
  '^\+.*(pub enum BackendShape|UnionTape|union_tape|\bBIR\b|directive|StructuralAlphabet::json|StrictJson|skip_json|match_json|unescape_json|JsonPolicy|json_policy|public substrate|sidecar substrate|parallel substrate|BackendShape::)'`
  returned no matches.
- `git diff e74d480d..61d5cc3b -- <generic crate roots> | rg
  '^\+.*(json|Json|JSON|UnionTape|union_tape|BackendShape|directive|\bBIR\b|substrate|Tape|StructuralAlphabet)'`
  returned no matches.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and
  8 gate-binary W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` passed 5 focused strict tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed the
  sidecar same-run rejection test.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline
  -- --nocapture` passed, including the new empty host/feature metadata negative
  mutations (`skinny/crates/bbnf-bench/src/report.rs:2053`-`skinny/crates/bbnf-bench/src/report.rs:2069`).
- `cargo test -p bbnf-bench lock14 -- --nocapture` passed all 7 Lock 14 tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` passed and replayed the current
  W0 report validation.
- `cargo xtask check-conformance` passed:
  `conformance: 21 valid fixtures accepted; 7 invalid fixtures rejected`.
- `git diff --check e74d480d..61d5cc3b` returned clean.

## Blockers

None.

## Required Fold

None for CH2. V11 remains a report/gate telemetry fold, closes the V10 empty
metadata hole, stays under the live W0 source cap, and does not introduce JSON
policy into generic crates or any new directive/BIR/substrate/API/
`BackendShape`/`UnionTape` surface.

## Residual Risk

- This acceptance is limited to the V11 CH2 generality lens. W0 still needs the
  full V11 six-lens consolidation and then the orchestrator's two consecutive
  qualifying ACCEPT cycles before W1-W6 can dispatch.
- The CSS L4 / Sheets / BBNF-self proof remains an unchanged-output/frozen-root
  proof here because V11 did not edit generic CostFacts, codegen, runtime, SIMD,
  or parser-template paths. Any later generic edit must rerun the full SPEC
  Section 2.1 non-JSON proof rather than inheriting this W0 report-only result.
