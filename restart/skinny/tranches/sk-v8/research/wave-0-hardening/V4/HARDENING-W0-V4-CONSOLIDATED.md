# SK-V8 W0 Hardening V4 Consolidated

Date: 2026-05-18.

Target: `077aadad8aacf95e3250ec157f30ba6ab873bf6b`
(`fix(sk-v8-wave0): fold hardening V3 gate blockers`).

## Verdict

REJECT.

Accept rate: 4/6 = 66.7%.

Convergence: not achieved. V4 is not a qualifying ACCEPT cycle under
`restart/prompts/ORCHESTRATOR.md` Section 3Z because CH1 and CH4 found material
W0 blockers. W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | REJECT | 97% | Deferred rows do not semantically consume `parse_utf8` and `escape_complete`; an isolated mutation to `none` / `n/a` still passed the W0 baseline acceptance test. |
| CH2 | ACCEPT | 96% | Comparator allowlist, native strict-only admission, sidecar-same-run rejection, and flaw-probe isolation held under focused tests. |
| CH3 | ACCEPT | 96% | Lock 14, frozen-root diffs, no directive/BIR/substrate drift, and grammar-neutral W0 boundary held under tests and gate replay. |
| CH4 | REJECT | 96% | `run_id` excludes volatile probes but still fingerprints unvalidated W0-shaped `json_*` Criterion groups outside the validated fixture/row set. |
| CH5 | ACCEPT | 96% | Live SPEC/SYNTHESIS/HANDOFF/DISPATCH-PROMPT and W0-rendered `RESULTS.md` are consistent; W1-W6 remain blocked correctly. |
| CH6 | ACCEPT | 96% | End-to-end replay, copied-root replay, SIMD metadata mutation failure, sidecar contract wording, and anti-paper-close evidence held. |

## Required V5 Fold

1. Gate `parse_utf8` and `escape_complete` for all W0 rows, not only strict
   admission rows. For the current W0 opening surface, deferred rows must keep
   `parse_utf8=view-boundary` and `escape_complete=yes`; otherwise
   `Report::validate_sk_v8_w0()` must reject.
2. Add a negative test mutating an otherwise valid W0 row to
   `parse_utf8=none` and/or `escape_complete=n/a` while leaving
   `strictness=deferred` and `measured_validation_path=view-boundary`.
3. Scope `criterion_fingerprint` to the validated W0 fixture/row manifest
   rather than any W0-shaped recursive Criterion path. An unrelated
   `json_unvalidated_future/track1_generated/new/estimates.json` must either
   leave the committed `run_id` unchanged or fail with an explicit
   unvalidated-group error before any update path.
4. Preserve the accepted V4 folds: strict comparator id allowlisting, sidecar
   same-run rejection, flaw-probe isolation, volatile probe exclusion, mandatory
   SIMD metadata validation, Lock 14/frozen roots, packet/RESULTS consistency,
   and copied-root replay evidence.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- focused negative for deferred-row `parse_utf8` / `escape_complete` mutation
- focused negative for unrelated W0-shaped Criterion group
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- `git diff --check`

## Governance

V4 rejection resets the consecutive ACCEPT counter. After the V5 fold, W0 must
receive two consecutive challenge cycles at at least 95% ACCEPT, with no open
critical defects, before W0 can close and W1-W6 can dispatch.
