# SK-V12 W1a CHALLENGE CH4 - Cost And Size

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Lens: CH4 cost, size, benchmark cost, generated roster, and redress cap fit.
Disposition: REVISE.

## Authorities Read

- `restart/prompts/ORCHESTRATOR.md:74-88`: CH4 checks whether LOC budget, risk class, wave alignment, hard cap, and same-wave consumer are realistic.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112-125`: skinny CHALLENGE is adversarial review; CH4 asks whether the LOC budget and hard cap are realistic.
- `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:69-78`, `:88-130`: W1a is high risk, CHALLENGE-mandatory, `<=360` hand LOC, 20/15/30 minute phase caps, and redress edits only SPEC owner paths.
- `restart/skinny/tranches/sk-v12/SPEC.md:237-275`: W1a source/edit cap is `<=360` hand LOC, generated output is named separately, generated LOC/module bytes/grammar size/O(N) growth are tracked, and cap misses halt.
- `restart/skinny/tranches/sk-v12/SPEC.md:314-349`: W1a must legalize CSS emission through `GrammarConfig` or equivalent metadata, add Lock 14 scan consumption, preserve JSON parity/floors, claim no CSS row, add no directive/BIR/`BackendShape`/public substrate API, and save `/tmp/skv12-waveW1a-rejected.patch` on fail.
- `restart/skinny/tranches/sk-v12/SPEC.md:174-211`: current CSS L4 has no admitted row; JSON direct/typed guard floors remain active; JSON-producing path movement requires guard rerun or no-touch proof.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35`, `:80-106`, `:141-150`: CSS L4 is authoritative, close requires `lightningcss_mbps + 1`, and the seven Lock 14 leaks must be resolved before CSS L4 emission is legal.
- W1a research and plan: `CONSOLIDATED.md`, `PLAN.md`, `PLAN-P1-grammar-profile.md`, `PLAN-P2-lock14-gate.md`, and `A1` through `A6` under `restart/skinny/tranches/sk-v12/research/w1a/`.

## Cost Finding

The W1a plan is directionally right but not CH4-clean as written. It combines four cost classes inside one redress:

1. Codegen/provider refactor: private `GrammarProfile`, provider selection, JSON config generation, and JSON template imports.
2. Generated roster change: new JSON `config.rs`, changed JSON generated modules, stale-file/exact-roster enforcement, and maybe generated real-typed output.
3. Lock 14 gate implementation: generic-root scan, positive/negative tests, owner-delta handling, and same-command gate consumption.
4. Measurement/report surface: SPEC Section 0.5 floor enforcement, native JSON guard refresh, `RESULTS.md` exactness, and REDRESS 121 accounting.

That is too much for `<=360` hand LOC and `<=30 min` redress without scope reduction. The plan also requires `cargo run -p xtask -- lint-loc`, but the current baseline already fails: `crates/bbnf-bench: 12143/3300 LOC` and `xtask: 1402/650 LOC`. Because this failure exists before W1a source work, the current verification list cannot be a W1a PASS gate.

## LOC Risk

Current owner-surface sizes are large enough that "small additive patch" assumptions are unsafe:

- `codegen/src/lib.rs`: 498 lines; `json_provider.rs`: 96; `sink_direct.rs`: 514; `typed_direct.rs`: 678.
- JSON templates: `generated.rs` 393, `value.rs` 172, `view.rs` 459, plus parser/visitor.
- `lock14_baseline.rs`: 806; `bin/gate.rs`: 2515; `report.rs`: 3229; `xtask/src/main.rs`: 741.

The cap can fit only a narrow slice: one codegen-private profile boundary, one generated JSON config template, minimal imports in JSON templates, one exact JSON-roster check, and a focused Lock 14 scan in `lock14_baseline.rs`. It cannot also fit typed renderer containment, report-floor plumbing, xtask command changes, broad owner-delta logic, and full benchmark refresh work.

CH4 estimate after reductions: `~300-360` hand LOC if `report.rs`, `bin/gate.rs`, `xtask`, IR, runtime tape, CSS, and typed renderer splitting stay out. As written, plausible cost is `430-650+` hand LOC before REDRESS text.

## Generated Size Risk

Generated-size budget has headroom but roster risk is high:

- Current planned runtime JSON roster is 1,994 lines / 63,452 bytes, under the 4,000 generated-runtime LOC ceiling.
- `generated_real_typed.rs` is 1,846 lines / 62,599 bytes.
- `skinny/grammars/json.bbnf` is 492 bytes.

A generated `config.rs` is size-safe by itself. The risk is not raw generated LOC; it is uncontrolled fanout. `scan.rs` and `sink.rs` are both generated outputs and template inputs today, so byte-clean regen does not prove independent template provenance until W1a makes ownership exact. The plan must name the final roster and reject stale extras only for `crates/runtime/src/grammars/json`, not globally for directories like `crates/bbnf-bench/src`.

## Exact Roster Risk

REVISE because the plan rosters are not exact enough:

- `PLAN.md` names optional `json_sink_direct.rs` and `json_typed_direct.rs`; `PLAN-P2` instead names `grammar_profiles/mod.rs` and `grammar_profiles/json.rs`.
- `generated_real_typed.rs` is "only if" output in one plan and part of checks in another.
- `report.rs`, `bin/gate.rs`, and `xtask/src/main.rs` are optional owner paths, but the verification list assumes some of that plumbing may exist.
- The revert slices in the plan variants range from narrow paths to broad `skinny/crates/runtime`, `skinny/crates/ir`, and `skinny/crates/bbnf-bench`.

Before source work, reduce to one exact roster. Anything outside it returns to CHALLENGE instead of being treated as "if needed".

## Benchmark Cost

As written, benchmark cost does not fit the wave cap. `bench-json` runs `cargo bench -p bbnf-bench`; the bench crate currently registers `json_parity` and `simd_scan`, and `json_parity` covers the JSON row family. The command then updates or checks `RESULTS.md`. That is not a bounded 30 minute redress step, especially after codegen/runtime movement and rebuild cost.

W1a can fit only if the plan either:

- proves generated JSON and generated typed bytes are unchanged and uses no-touch guard state, or
- records W1a as BLOCKED/REJECTED at cap if the required native guard refresh has not completed.

If generated JSON output intentionally changes, the fresh guard refresh is real cost. CH4 does not accept pretending that cost is "verification overhead" outside the redress cap.

## Required Scope Reductions

To become ACCEPT, revise W1a to this cap-fit scope:

1. Source owner roster:
   - `skinny/crates/codegen/src/lib.rs`
   - `skinny/crates/codegen/src/grammar_profile.rs`
   - `skinny/crates/codegen/src/json_provider.rs`
   - `skinny/crates/codegen/src/json_templates/config.rs`
   - only the JSON templates needed to import `super::config`
   - `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
   - `skinny/REDRESS.md`
2. Generated roster:
   - `skinny/crates/runtime/src/grammars/json/config.rs`
   - `generated.rs`, `mod.rs`, `parser.rs`, `scan.rs`, `sink.rs`, `value.rs`, `view.rs`, `visitor.rs`, `host.rs` only if regen changes them
   - `generated_real_typed.rs` only if `check-real-typed` proves a necessary typed regen diff
3. Remove from W1a unless a new CHALLENGE accepts them:
   - `report.rs` floor checker
   - `bin/gate.rs` argument changes
   - `xtask/src/main.rs` command changes
   - IR edits
   - runtime tape/public API edits
   - typed renderer splitting
   - CSS runtime/generated files
4. Replace the baseline-failing `lint-loc` gate with W1a-local generated-size accounting: hand LOC delta, generated LOC delta, generated module bytes, `json.bbnf` bytes, and an O(N) growth statement in REDRESS 121.
5. Keep floor verification as either existing `gate-json --check-results` plus explicit REDRESS floor table, or a separate later gate task. Do not add report schema/outcome churn in W1a.
6. Use the cap honestly: if coding plus byte/parity/gate checks cannot finish by 30 minutes, save `/tmp/skv12-waveW1a-rejected.patch`, revert only the W1a slice, and record REDRESS 121 as BLOCKED/REJECTED.

## Final Disposition

REVISE.

Redress can fit the W1a cap only after the reductions above. As written, the plan exceeds CH4 tolerance through ambiguous rosters, an already-failing `lint-loc` gate, likely over-360 hand LOC, and an unbounded native benchmark refresh. Do not dispatch source redress until the plan has one exact owner roster, one generated roster, no optional broad plumbing, and an explicit cap-stop rule.
