# Alpha-E - Candidate Shortlist - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: candidate packages for downstream S-P0/S-P3; S-P3 authors the exact
wave plan after S-P0/S-P1/S-P2 convergence.
Output: this file.

| Package | Candidate | Owner paths | Same-wave consumer | LOC budget | Falsifiability gate | Risk |
|---|---|---|---|---:|---|---|
| A | Grammar-derived CSS L4 provider and typed CSS document/value output. | `grammar/css/l4/**`, `crates/core/src/runtime/css_l4/**`, `skinny/crates/codegen/src/**`, `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`, `skinny/xtask/src/**` | CSS typed parser/view/visitor plus same-workload cssparser comparator | <=1200 split by S-P3 | delete or bypass no live proof until grammar-derived provider emits typed document/value nodes; no `CSS_GENERATED_RS` live proof | high |
| B | CSS same-workload equality and >SOTA retime. | CSS bench/report/gate surfaces, typed CSS runtime, `restart/skinny/ROLLING-SOTA-DELTA.md`, `skinny/RESULTS.md` | cold M5 Max retime gate with typed summary equality | <=500 | Track 1 `4/4`, cssparser `4/4`, typed summaries equal, Track 1 > cssparser + threshold; otherwise REJECT/REDRESS | high |
| C | Dirty generated CSS state retirement and broad reproducibility restoration. | `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, xtask regen/check commands | `cargo test -p codegen`, `cargo xtask check-real-typed`, CSS check commands | <=400 | broad checks pass from clean tree or each remaining dirty input has explicit intrinsic-block proof | high |
| D | Pattern H generator-owned collapse beyond provenance. | `crates/core/src/runtime/**`, root/skinny regen tooling, grammar metadata | root runtime regen/check plus 67-file census | <=900 | count stays 67, every file round-trips from one grammar-id parameterized template family, no header-only close | high |
| E | Native aarch64 SIMD hot-leaf implementation. | only hot files identified by S-P1 profile, `crates/core/src/**`, `skinny/crates/bbnf-simd/**`, checkasm/parity tests | scalar reference, checkasm/parity, same-wave parser consumer, cold per-parse measurement | <=600 per primitive | Apple M5 Max / aarch64 only; no x86; no SIMD admit without scalar and parity | medium |

## Pre-Blocks

- No CSS broadcast re-admit.
- No string-literal generated CSS provider.
- No fact-stream, summary, or brace-counter CSS SOTA proof.
- No full-codegen close while pre-existing dirty generated files remain.
- No FNV production migration.
- No x86 or AVX-512 implementation scope.
