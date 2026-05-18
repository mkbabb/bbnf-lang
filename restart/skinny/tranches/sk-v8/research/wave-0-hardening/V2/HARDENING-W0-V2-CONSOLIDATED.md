# SK-V8 W0 Hardening V2 Consolidated

Date: 2026-05-18.

Reviewed commit: `cb0fdba0 fix(sk-v8-wave0): fold hardening V1 gate blockers`.

## Verdict

REJECT.

V2 returned two ACCEPT reviews and four REJECT reviews:

| Challenge | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 96% | Strict-admission live wiring, outcome validation, `parse_utf8` / `escape_complete`, and hard parse-failure preservation are folded. |
| CH2 | REJECT | 76% | Native same-run evidence and sidecar source/freshness are still under-validated. |
| CH3 | REJECT | 86% | Lock 14 freeze omits directive admission and SIMD asm/build surfaces. |
| CH4 | REJECT | 84% | `--update-results` can still promote stale or mixed Criterion metadata into `SK-V8-open`. |
| CH5 | REJECT | 94% | `sk_v8.row_id` is not bound to rendered `corpus` / `workload`, and native comparator plane is not validated for deferred rows. |
| CH6 | ACCEPT | 96% | Anti-paper-close blockers from V1 are folded for mutation semantics, check-only reproducibility, and hard parse outcomes. |

The minimum ACCEPT confidence is 96%, but V2 is not a qualifying convergence
cycle because four challenge lenses rejected. W0 remains open. W1-W6 remain
blocked.

## Accepted Folds

- `gate-json` is check-only by default and writes only with
  `--update-results` / `--write-results`.
- Validation failures no longer rewrite `skinny/RESULTS.md`.
- Committed run ids no longer include HEAD or local Criterion root paths.
- Volatile masking probes and peak-RSS notes are not rendered by default.
- Strict-admission validation is called from W0 report validation and includes
  row strictness, output plane, comparator strictness/freshness, measured-path,
  `parse_utf8`, and `escape_complete`.
- Unsupported outcome strings and legacy outcomes are rejected by the W0 row
  validator; hard parse failures `I/J/K/L/M` are preserved instead of being
  collapsed into `S`.
- Criterion profile/hot-leaf placeholders are rejected by exact
  `criterion-slope-profile` row paths.
- Native comparator source paths are workload-specific for parse, direct, and
  real typed rows.
- Lock 14 now performs Git-backed content enforcement for the roots it covers
  and checks the `BackendShape` surface against the existing five variants.

## Blocking Fold Items

1. Bind row identity.
   `TelemetryRow::validate_sk_v8_w0` must parse `sk_v8.row_id` and require it
   to match the rendered `corpus` and `workload` before baseline, parse-guard,
   profile, hot-leaf, outcome, or comparator validation.

2. Tighten native comparator evidence.
   For `sonic_rs_strict` and `serde_json`, W0 must require the workload-matched
   source path, expected comparator plane, `comparator_strictness=strict`,
   `comparator_freshness=same-run-native`, `sidecar_freshness=n/a`, and a
   finite `value_mbps`.

3. Tighten sidecar evidence.
   Populated historical sidecars must use source artifacts tied to row corpus
   and comparator id. Absent sidecars must use
   `absence:w0:{corpus}:{workload}:{id}`. Any `sidecar-same-run` claim must
   reject until a structured same-run sidecar manifest supplies corpus,
   comparator id, binary/build identity, plane, strictness, freshness, and run
   id.

4. Expand Lock 14 frozen roots.
   Include directive admission and asm wiring at minimum:
   `crates/grammar/src`, `crates/bbnf/src` if it exposes grammar/directive
   entry behavior, `crates/bbnf-simd/build.rs`, and `crates/bbnf-simd/ext`.
   Either include `crates/parse-that-regex/src` or explicitly classify why it
   is outside W0's scanner freeze.

5. Validate Criterion capture coherence before writing.
   W0 must reject missing/malformed metadata and mixed roots before
   `--update-results` can promote a report. The validator must bind fixture
   `input_sha256`, `input_bytes`, profile, rustflags, target CPU, host identity,
   sample policy/count, measurement time, workload/track tags, and capture id
   coherently across all loaded rows. The criterion fingerprint is an immutable
   identifier only after semantic metadata validation passes.

6. Prevent volatile probe commits.
   Make `--include-volatile-probes` incompatible with `--update-results`, or
   add explicit non-admission/baseline validation before those sections can be
   written.

## Required Negative Tests

- Row id mismatch against rendered corpus/workload.
- Native comparator plane, freshness, sidecar freshness, strictness, and missing
  Mbps mutation while keeping the expected source path.
- Sidecar source-artifact and freshness mismatch, including rejected
  `sidecar-same-run` without structured same-run manifest.
- Lock 14 rejection for changed directive parser/admitted directive name,
  changed SIMD build script, changed asm include, and newly added asm
  source/include under the frozen surface.
- Stale or mixed Criterion metadata: changed `bbnf_commit` / capture id,
  mismatched fixture hash/bytes, mixed host/profile/rustflags/target CPU/sample
  policy, and malformed or missing comparator metadata.

## Evidence Reviewed

Challenge agents collectively ran:

- `cargo test -p bbnf-bench`
- `cargo test -p bbnf-bench lock14_baseline::tests`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --advisory --check-results`
- Frozen-root diff checks for the current V2 frozen-root set and exploratory
  diff checks over the omitted directive/asm candidate roots.

Those commands prove the happy path is stable after V1 folding. They do not
close the V2 rejection items above.

## Next Step

Fold the V2 blockers into W0 v+1, rerun the full W0 evidence suite, commit the
fold, then re-challenge. The next qualifying cycle must return at least 95%
ACCEPT across the cohort, and W0 still needs two consecutive qualifying ACCEPT
cycles before W1-W6 can dispatch.
