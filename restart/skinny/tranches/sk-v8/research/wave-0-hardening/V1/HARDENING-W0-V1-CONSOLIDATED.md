# SK-V8 W0 Hardening V1 Consolidated

Date: 2026-05-18.

Scope: adversarial review of W0 commit `6d8cb701` against the SK-V8
telemetry gate, strict-vs-strict comparator discipline, Lock 14 freeze, report
determinism, sidecar provenance, and test/redress sufficiency.

## Decision

REJECT.

V1 result: 1/6 ACCEPT, 5/6 REJECT.

| Review | Lens | Decision | Acceptance probability | Disposition |
|---|---|---:|---:|---|
| CH1 | strict admission semantics | REJECT | 58% | Fold required |
| CH2 | telemetry manifest and profile provenance | REJECT | 40% | Fold required |
| CH3 | Lock 14 grammar-neutral freeze | REJECT | 22% | Fold required |
| CH4 | report determinism and reproducibility | REJECT | 61% | Fold required |
| CH5 | comparator evidence and sidecar provenance | ACCEPT | 93% | Preserve accepted boundaries |
| CH6 | tests and redress sufficiency | REJECT | 58% | Fold required |

V1 is below the required acceptance threshold and cannot close W0. W0 must
advance to a v+1 fold and re-challenge.

## Blocking Fold Set

1. Wire strict-admission refusal into the live W0 gate path, not only helper
   tests. The live report validator must parse every outcome, reject unknown
   and out-of-SPEC outcomes, and reject any strict admission claim unless
   row strictness, comparator strictness, output plane, comparator freshness,
   sidecar freshness, measured validation path, UTF-8 validation path, and
   escape completeness all satisfy the strict-vs-strict contract.
2. Extend strict-admission evidence with `parse_utf8` and `escape_complete`.
   `parse_utf8=view-boundary` and any non-`yes` escape status must fail strict
   admission.
3. Preserve hard parse failures when W0 demotes parse scoreboard rows. `I`,
   `J`, `K`, `L`, and `M` must remain their own hard-failure outcomes; only
   admission-capable parse outcomes may become reserved `S`.
4. Correct native comparator provenance by workload. Parse rows may cite
   `sonic_rs_anchor` and `serde_json`; direct rows must cite
   `sonic_rs_direct_to_struct` and `serde_json_direct_to_struct`; typed rows
   must cite `sonic_rs_real_typed_struct` and
   `serde_json_real_typed_struct`. W0 validation must reject native source
   artifacts that do not match row workload.
5. Replace profile/hot-leaf placeholder acceptance with executable provenance
   validation. If W0 uses Criterion slope as the hot-leaf proxy, the manifest
   must say so explicitly and the validator must require the matching
   Criterion estimate artifact, row id, sample cost, sample count, and source
   path shape rather than accepting arbitrary non-`unprofiled` strings.
6. Convert Lock 14 from path-existence allowlist to a real freeze gate for W0.
   It must compare frozen roots against the parent W0 baseline or current git
   cleanliness, include runtime/tape, runtime exports, IR/BIR/BackendShape,
   passes, codegen generic/lower/template surfaces, SIMD/asm scanner surfaces,
   grammars, fixtures, generated JSON/typed output, and reject unlisted
   additions under frozen roots.
7. Stabilize W0 report generation. `gate-json` must not stamp committed rows
   with path-volatile or current-HEAD run ids that change after the commit. It
   must support a check-only validation path and require an explicit update
   flag to mutate `RESULTS.md`; validation failure must not rewrite the
   committed report.
8. Gate or de-render volatile probe/RSS sections. Any rendered probe/RSS
   evidence must either be baseline-validated or clearly excluded from W0
   admission evidence. It must not churn the committed report while W0
   validation passes.
9. Add integrated W0 negative tests for unsupported outcomes, deferred or
   view-boundary strict claims, plane mismatch admission, stale/historical
   sidecar strict claims, malformed native source paths, placeholder
   profile/hot-leaf evidence, hard-failure demotion, and Lock 14 frozen-root
   changes/additions.
10. Preserve CH5's accepted boundaries: C++ sidecars remain historical or
    absent planning signals only; sonic-rs lossy remains permissive; neither
    may support strict admission.

## Required V2 Evidence

- `cargo test -p bbnf-bench`
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- `cargo xtask gate-json --advisory --check-results` or equivalent check-only
  W0 gate
- one explicit report update command when `RESULTS.md` is intentionally
  regenerated
- an immediate second check-only gate run producing no `RESULTS.md` diff
- Lock 14 frozen-root negative tests and frozen-surface diff proof

## Disposition

W0 remains open. Fold the blockers into W0 v+1, commit the fold, then dispatch
a V2 challenge. W1-W6 remain blocked until W0 receives two consecutive
qualifying ACCEPT cycles or an explicit governance override.
