# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V12

Verdict: ACCEPT.

Confidence: 97%.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Scope

CH2 re-challenged the unchanged V11-accepted W0 target for Lock 14 and
grammar-neutrality. The review focused on the required no-JSON-policy boundary
in generic crates, no new directive/BIR/substrate/API/`BackendShape`/`UnionTape`
surface, W0 report/gate-only telemetry, no behavior-surface drift since V11, and
whether CSS L4 / Sheets / BBNF-self proof obligations remain intact.

## Evidence

- The CH2 lens is explicitly Lock 14 generality: no grammar-name leak, and every
  proposed intervention must work for CSS L4, Sheets, and BBNF-self rather than
  only JSON (`restart/prompts/ORCHESTRATOR.md:74`-`restart/prompts/ORCHESTRATOR.md:88`).
  The convergence rule requires two consecutive qualifying ACCEPT cycles
  (`restart/prompts/ORCHESTRATOR.md:104`-`restart/prompts/ORCHESTRATOR.md:121`).
- SPEC keeps W0 in report/gate telemetry: every SK-V8 field must be consumed by
  `gate-json`, and Lock 14 generic leaks reject the wave
  (`restart/skinny/tranches/sk-v8/SPEC.md:103`-`restart/skinny/tranches/sk-v8/SPEC.md:146`).
  The W0 exit gate still forbids parser/scanner/SIMD/asm/codegen/product behavior
  or generated parser output drift, with `gate-json` as the same-wave consumer
  (`restart/skinny/tranches/sk-v8/SPEC.md:346`-`restart/skinny/tranches/sk-v8/SPEC.md:361`).
- The generality gate forbids public JSON APIs, grammar-name branches, generic
  JSON structural policy, and template/provider leaks; non-JSON proof is
  required for generic CostFacts/codegen/runtime/SIMD/parser-template edits
  (`restart/skinny/tranches/sk-v8/SPEC.md:261`-`restart/skinny/tranches/sk-v8/SPEC.md:286`).
  The non-negotiables also forbid new directives, BIR variants, `BackendShape`
  variants, `UnionTape`, new substrate surfaces, public substrate APIs,
  sidecar/parallel substrate, and JSON policy in generic crates
  (`restart/skinny/tranches/sk-v8/SPEC.md:191`-`restart/skinny/tranches/sk-v8/SPEC.md:205`).
- W0 is still telemetry-only in the dispatch authority; W3's later structural
  work remains blocked from adding `UnionTape`, new `BackendShape`, directive,
  BIR, or public substrate API (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:63`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:95`;
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:134`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:147`).
  HANDOFF records the same W0-only posture and frozen behavior-surface
  requirement (`restart/skinny/tranches/sk-v8/HANDOFF.md:127`-`restart/skinny/tranches/sk-v8/HANDOFF.md:156`).
- V10 rejected only on CH4 cost/reproducibility blockers: post-V6 cap overflow
  and empty `arch`/`cpu`/`os`/`simd` metadata acceptance
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:20`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:29`;
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:92`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:118`).
  V11 then accepted 6/6 and established the first qualifying cycle; its CH2
  disposition specifically accepted that no generic crate, directive, BIR,
  substrate, public API, `BackendShape`, or `UnionTape` surface moved
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:28`).
- The target fold remains one report/gate telemetry source file. `git diff
  --name-status e74d480d..61d5cc3b4312883e026060174e876a0c18b34703` returned
  only `M skinny/crates/bbnf-bench/src/report.rs`; `git diff --shortstat
  00c3485a..61d5cc3b4312883e026060174e876a0c18b34703 --
  skinny/crates/bbnf-bench/src/report.rs` returned `1 file changed, 118
  insertions(+), 13 deletions(-)`. `git show --stat --oneline --no-renames
  61d5cc3b4312883e026060174e876a0c18b34703 --
  skinny/crates/bbnf-bench/src/report.rs` reported `58 insertions / 109
  deletions`, i.e. the final commit narrowed the V10 fold rather than widening
  it.
- There is no behavior-surface drift since the V11 target. Before writing this
  V12 file, `git diff --name-only
  61d5cc3b4312883e026060174e876a0c18b34703..HEAD -- skinny` returned no paths,
  and the same command over `skinny/crates/bbnf-bench/src/report.rs`, `gate.rs`,
  `bin/gate.rs`, `lock14_baseline.rs`, `skinny/xtask/src/main.rs`, and
  `skinny/RESULTS.md` returned no paths. `git diff --name-status
  61d5cc3b4312883e026060174e876a0c18b34703..HEAD --
  restart/skinny/tranches/sk-v8/research/wave-0-hardening` returned only the
  seven V11 hardening report files.
- The frozen Lock 14 roots still have no target drift: `git diff --name-only
  0bd16f6d..HEAD -- skinny/grammars skinny/test_data
  skinny/crates/test-fixtures skinny/crates/runtime/src skinny/crates/ir/src
  skinny/crates/passes/src skinny/crates/codegen/src skinny/crates/grammar/src
  skinny/crates/bbnf/src skinny/crates/bbnf-simd/src
  skinny/crates/bbnf-simd/build.rs skinny/crates/bbnf-simd/ext
  skinny/crates/parse-that-regex/src skinny/crates/bbnf-bench/src/direct_struct.rs
  skinny/crates/bbnf-bench/src/real_typed_struct.rs
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/track2 skinny/crates/bbnf-bench/src/parity.rs
  skinny/crates/bbnf-bench/src/scan.rs
  skinny/crates/bbnf-bench/src/materialization.rs
  skinny/xtask/src/real_typed_schema.rs` returned no paths.
- The diff scans found no added forbidden surface. `git diff
  e74d480d..61d5cc3b4312883e026060174e876a0c18b34703 -- . | rg
  '^\\+.*(pub enum BackendShape|UnionTape|union_tape|\\bBIR\\b|directive|StructuralAlphabet::json|StrictJson|skip_json|match_json|unescape_json|JsonPolicy|json_policy|public substrate|sidecar substrate|parallel substrate|BackendShape::)'`
  returned no matches, and the same generic-root diff scan for added JSON/tape/
  substrate/API tokens returned no matches.
- The grammar id check remains bench report validation, not generic runtime
  policy (`skinny/crates/bbnf-bench/src/report.rs:322`-`skinny/crates/bbnf-bench/src/report.rs:327`).
  `gate-json` calls Lock 14 validation before report construction, then requires
  schema-v3 plus `validate_sk_v8_w0()` before accepting or writing the report
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:37`-`skinny/crates/bbnf-bench/src/bin/gate.rs:43`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs:315`-`skinny/crates/bbnf-bench/src/bin/gate.rs:328`).
- W0 manifest semantics are still workload-local telemetry, not a substrate API.
  The validator requires exact pre-W1 sentinels, non-empty host/feature metadata,
  exact `target_cpu=native`, and exact substrate tuples
  (`skinny/crates/bbnf-bench/src/report.rs:1007`-`skinny/crates/bbnf-bench/src/report.rs:1081`).
  The tuple set remains `borrowed_view_over_offset_tape /
  discarded_after_capacity / one`, `sink_only_digest / n/a / zero_or_inert`, and
  `typed_direct_projection / n/a / zero_or_inert`
  (`skinny/crates/bbnf-bench/src/report.rs:1083`-`skinny/crates/bbnf-bench/src/report.rs:1094`;
  `skinny/crates/bbnf-bench/src/bin/gate.rs:603`-`skinny/crates/bbnf-bench/src/bin/gate.rs:614`).
- W0 still cannot become strict admission. Report validation pins
  `strictness=deferred`, `measured_validation_path=view-boundary`,
  `parse_utf8=view-boundary`, and `escape_complete=yes`
  (`skinny/crates/bbnf-bench/src/report.rs:1096`-`skinny/crates/bbnf-bench/src/report.rs:1122`).
  The strict-admission helper rejects non-GO outcomes, deferred rows, non-strict
  comparators, stale/historical/absent freshness, and plane/validation mismatches
  (`skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`).
- The executable Lock 14 baseline classifies JSON inputs/templates/outputs as
  allowed surfaces while keeping generic surfaces read-only and bench report/gate
  files telemetry-only (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:13`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:320`).
  Its validator checks entries, frozen git roots, and the `BackendShape` surface
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:340`);
  the frozen roots cover grammar, runtime, IR, passes, codegen, SIMD, generated,
  Track 2, parity, scan, materialization, and host/API schema surfaces
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:397`).
  The `BackendShape` validator rejects variant-count drift and `UnionTape`
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:491`),
  while the IR still has only `EagerTape`, `OffsetTape`, `EventTape`,
  `SinkOnly`, and `CollapsedStage` (`skinny/crates/ir/src/lib.rs:401`-`skinny/crates/ir/src/lib.rs:408`).
- Current report validation was replayed. `CARGO_TARGET_DIR=/tmp/skv8-w0-target
  RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory
  --check-results` exited 0 and rendered the current W0 report. The report stays
  W0 telemetry posture: rows are deferred/view-boundary
  (`skinny/RESULTS.md:3`-`skinny/RESULTS.md:5`), the SK-V8 manifest is present
  (`skinny/RESULTS.md:44`-`skinny/RESULTS.md:48`), and the report states
  `gate-json` consumes that manifest while C++ sidecars are historical or absent,
  never W0 strict anchors (`skinny/RESULTS.md:138`-`skinny/RESULTS.md:141`).
- Focused executable checks passed: `cargo test -p bbnf-bench lock14 --
  --nocapture` passed all 7 Lock 14 tests, including frozen-root and
  `BackendShape`/`UnionTape` drift tests
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:520`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:610`).
  `cargo test -p bbnf-bench
  report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture` passed,
  including empty host/feature metadata negative mutations
  (`skinny/crates/bbnf-bench/src/report.rs:2053`-`skinny/crates/bbnf-bench/src/report.rs:2069`).

## Blockers

None for CH2.

## Required Fold

None. The V12 CH2 re-challenge found no JSON policy in generic crates, no new
directive/BIR/substrate/API/`BackendShape`/`UnionTape` surface, no source or
report drift since the V11 target, and no weakening of the CSS L4 / Sheets /
BBNF-self proof obligations for future generic edits.

## Residual Risk

- This verdict is limited to the CH2 generality lens. W0 closes only if the full
  V12 challenge cycle also returns the required qualifying ACCEPT state with zero
  critical defects and no unresolved REVISE.
- The non-JSON proof is unchanged-root proof because V12 did not edit generic
  CostFacts, codegen, runtime, SIMD, parser-template, IR, grammar, or generated
  surfaces. Any later generic edit must rerun the full SPEC Section 2.1 proof
  for CSS L4, Sheets, and BBNF-self rather than inheriting this W0 report/gate
  result.
