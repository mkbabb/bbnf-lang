# Alpha-E - Candidate Shortlist - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: candidate packages for downstream S-P0/S-P3; S-P3 authors the exact
wave plan after S-P0/S-P1/S-P2 convergence.
Output: this file.

| Package | Candidate | Owner paths | Same-wave consumer | LOC budget | Falsifiability gate | Risk |
|---|---|---|---|---:|---|---|
| A | Grammar-derived CSS L4 provider and typed CSS document/value output. | writable: `grammar/css/l4/**`, grammar-neutral generator/template code, generated provider manifests, generated runtime outputs; read/delete/replace-only: `crates/core/src/runtime/css_l4/**` | CSS typed parser/view/visitor plus same-workload cssparser comparator | <=1200 split by S-P3 | delete or bypass no live proof until grammar-derived provider emits typed document/value nodes and byte-equivalent regen evidence; no `CSS_GENERATED_RS` live proof | high |
| B | CSS same-workload equality and >SOTA retime. | CSS bench/report/gate surfaces, typed CSS runtime, `restart/skinny/ROLLING-SOTA-DELTA.md`, `skinny/RESULTS.md` | cold M5 Max retime gate with typed summary equality | <=500 | Track 1 `4/4`, cssparser `4/4`, typed summaries equal, Track 1 > cssparser + threshold; otherwise REJECT/REDRESS | high |
| C | Dirty generated CSS state retirement and broad reproducibility restoration. | `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, xtask regen/check commands | `(cd skinny && cargo test -p codegen)`, `(cd skinny && cargo xtask check-real-typed)`, CSS check commands | <=400 | broad checks pass from clean tree or each remaining dirty input has explicit intrinsic-block proof | high |
| D | Pattern H generator-owned collapse beyond provenance. | `crates/core/src/runtime/**`, root/skinny regen tooling, grammar metadata | root runtime regen/check plus 67-file census | <=900 | count stays 67, every file round-trips from one grammar-id parameterized template family, no header-only close | high |
| E | Conditional native aarch64 SIMD hot-leaf discovery. | no source owner until S-P1 names a fresh hot leaf; later scope may include `crates/core/src/**` or `skinny/crates/bbnf-simd/**` only by S-P3 contract | scalar reference, checkasm/parity, same-wave parser consumer, cold per-parse measurement | <=600 manual LOC per primitive after S-P3 split | Apple M5 Max / aarch64 only; no x86; no SIMD admit without scalar and parity; no PMULL/CSSC-from-ISA or REDRESS-247 string64 retry | medium |

## Pre-Blocks

- No CSS broadcast re-admit.
- No string-literal generated CSS provider.
- No fact-stream, summary, or brace-counter CSS SOTA proof.
- No full-codegen close while pre-existing dirty generated files remain.
- No FNV production migration.
- No x86 or AVX-512 implementation scope.
- The full inherited REDRESS semantics in `SYNTHESIS.md` and
  `alpha-C-redress-digest.md` are binding. In particular, no
  tiny-string/StringBlock replay, retained parse shortcuts, retained cursor/list,
  retained sidecar tables, cursor streams, parser-owned structural streams,
  aux density/projection tables, retained class columns, Track 1 == Track 2
  sidecars, wrong-plane comparator admission, global direct/Track 2 cap changes,
  numeric/digit route without fresh P1 BBNF-side hot leaf, one-quartet
  promotion, PMULL/CSSC promotion from ISA alone, decoded-string retry,
  structural-stream retry, fixed-shape Unicode retry, or old string64 framing.

## Budget Discipline

S-P3 must split generated-heavy packages with explicit budget columns:
manual source/test LOC, generated-output status, docs/ledger LOC, phase hard
cap, split trigger, and same-commit consumer callsite. Generated output cannot
hide manual scope. If a package cannot fit the cap, S-P3 must split inside the
legal wave graph, record row-level intrinsic block, or route a wave-graph
amendment before redress.
