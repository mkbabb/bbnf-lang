# Alpha-E — Candidate Shortlist — SK-V15 V1

Pass: Pass Alpha. Cycle: SK-V14 -> SK-V15.
Date: 2026-05-27.
Scope: <=5 candidate packages for downstream S-P0/S-P3; S-P3 may split
packages into exact waves only with the dependency/cap gates below.
Output: this file.

The current worktree has pre-existing dirty source files in several owner
paths, especially `crates/core/src/runtime/css_l4/*` and
`skinny/crates/bbnf-bench/src/generated_real_typed.rs`. SK-V15 waves must
isolate those edits before redress and must not stage unrelated work.

| Package | Candidate | Owner paths | Same-wave consumer | LOC budget | Hard cap | Revert/block protocol | Falsifiability gate | Risk |
|---|---|---|---|---:|---|---|---|---|
| A | CSS admission honesty plus typed rebuild. First demote the 24 CSS rows to one diagnostic aggregate; then build typed CSS value output before retiring `CSS_GENERATED_RS`, fact-stream `parse()`, or `CssFullParseSummary`. | `skinny/crates/bbnf-bench/src/css_l4_w8.rs`, `skinny/xtask/src/main.rs`, `skinny/crates/codegen/src/runtime_generator.rs`, `skinny/crates/runtime/src/grammars/css_l4_*`, `grammar/css/l4`, `crates/core/src/runtime/css_l4/*`, `skinny/RESULTS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md` | generic grammar-id regen/check path, CSS typed benchmark, and one non-JSON/non-CSS smoke target from the existing template cohort (`math` or `csv`) | <=900 net LOC split across sub-waves | research <=20m, plan <=15m, redress <=30m per sub-wave | old CSS rows stay demoted; old parser cannot be removed from live admission until typed path proof lands | no admitted CSS row shares a measurement tuple unless aggregate; `rg "CSS_GENERATED_RS|CssFullParseSummary|Result<String, CssFactError>"` finds no live generated parser contract after typed proof; cssparser same-workload delta recorded on Apple M5 Max / aarch64 | high |
| B | Restore Lock 14 / Lock 16 gate coverage and make exclusions findings. | `skinny/crates/bbnf-bench/src/lock14_baseline.rs` at HEAD; Lock 16 owner surfaces `skinny/xtask/src/main.rs` `primitive-checkasm`, `skinny/crates/bbnf-simd/tests/checkasm_*`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`, `skinny/crates/bbnf-bench/src/report.rs` `lock16_status` / `checkasm_or_parity_status`, and `skinny/crates/bbnf-bench/src/bin/gate.rs`; plus surfaced leak owners | Lock 14/16 baseline gate, `primitive-checkasm`, and exclusion-report transcript | <=250 net LOC | research <=20m, plan <=15m, redress <=30m | failing gate blocks all later generic-clean claims | scan includes `runtime_generator.rs`, `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`, `json_templates/`; Lock 16 rows include `lock16_status`, `checkasm_or_parity_status`, and every exclusion is emitted as evidence, not silently skipped | medium |
| C | Collapse codegen fanout and Pattern H provenance together. | `skinny/xtask/src/main.rs`, `xtask/src/regen_simple_runtime.rs`, `skinny/crates/codegen/src/grammar_profile.rs`, `skinny/crates/codegen/src/grammar_provider.rs`, `skinny/crates/passes/src/lib.rs`, `crates/core/src/runtime/**` | generic grammar-id regen/check path with JSON, CSS, and `math` or `csv` smoke; root runtime regen + header census | <=700 net LOC split across sub-waves | research <=20m, plan <=15m, redress <=30m per sub-wave | header-only changes do not close; generated source must round-trip from the generator or block downstream Pattern H claims | no per-grammar regen enum/match fanout, no 4-style `RuntimeStyle`, no JSON/CSS runtime mode split, Pattern H count remains 67, and all 67 first lines carry generator provenance | high |
| D | Make Decision Engine load-bearing. | `skinny/crates/passes/src/backend_egraph.rs`, `skinny/crates/passes/src/decision_csp.rs`, `skinny/crates/ir/src/cost.rs`, `skinny/crates/codegen/src/lower/*.rs` | `codegen::lower::rust::lower_to_rust` + generated runtime diff | <=800 net LOC split across lowerer sub-waves | research <=20m, plan <=15m, redress <=30m per lowerer sub-wave | a zero-rule or scaffold lowerer blocks all Decision Engine admits | rewrite count >= 1; no scaffold block id; no grammar-named CSP facts; all five lowerers real and >=50 LOC | high |
| E | Quarantine closed-enum/FNV bench products. | `skinny/xtask/src/real_typed_schema.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, `skinny/crates/codegen/src/json_typed_direct.rs` | strict-product comparator + adversarial collision fixtures | <=300 net LOC | research <=20m, plan <=15m, redress <=30m | any production migration blocks close and routes to REDRESS | no FNV-keyed arbiter in production runtime; sidecars independent of Track 1 closed enums; adversarial fixture catches hidden collision/equivalence | medium |

Pre-blocks: no broadcast re-admit, no static centralisation of generated
runtime bodies, no self-exempting grep gate, no header-only Pattern H
paper close, no no-op Decision Engine, no FNV product migration into
`crates/core/src/runtime`.
