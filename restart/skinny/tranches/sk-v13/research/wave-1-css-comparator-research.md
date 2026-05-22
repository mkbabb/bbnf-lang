# SK-V13 W1 Research - CSS Comparator/Oracle Harness

Wave: W1. Phase: Research. Date: 2026-05-21.

## Scope

Owner paths are the SPEC Section 4 paths: `skinny/crates/bbnf-bench/`,
`skinny/xtask/src/`, W1 research artifacts, `skinny/RESULTS.md`, and
`skinny/REDRESS.md` only on reject. W0 is admitted at `99a6123fc`, so W1 entry
is open.

Six read-only sidecars audited the W1 surface:

- Schema/API: `nonjson_css_l4.rs` and `report.rs`.
- Gate/xtask: `gate.rs` and `xtask/src/main.rs`.
- CSS matrix: SK-V13 parity scoping plus SPEC Section 4.
- Verification: report-only Mbps, stale Criterion bytes, missing coverage, and
  old SK-V12-only report tests.
- Rolling delta: `RESULTS.md` and `ROLLING-SOTA-DELTA.md`.
- Pre-blocks: P2/P3 CSS gates and anti-paper-close hardening.

## Findings

1. The current SK-V12 CSS harness is single-row by construction. It hard-codes
   `css_l4/declaration_values/direct_to_struct/main`, the 187-byte fixture,
   the three Criterion lane names, and the retained fact-stream needles.

2. `SkV12CssL4SotaReport::validate_gate()` proves internal report shape, but
   the external proof lives in `gate.rs::validate_skv12_css_l4_sota_report`,
   which reads Criterion lanes and retained fact artifacts. W1 must continue to
   consume that external proof so report-only Mbps cannot admit.

3. The SK-V13 rolling delta already requires exactly 24 CSS rows. Only
   `declaration_values` may be numeric and `ADMITTED`; the other 23 rows must
   remain explicit `OPEN`/`absent:not-yet-generated` placeholders until their
   own waves produce same-plane facts.

4. W1 needs a new SK-V13 coverage report instead of widening the SK-V12 SOTA
   report. The report should carry the 24-feature matrix, distinguish
   `measured` from `absent_until_planned_wave`, and count admissions separately
   from harness coverage.

5. Same-plane facts for future CSS waves must include the feature id, row id,
   output plane, fixture/corpus id, grammar and input checksums, coverage
   artifact, Track 1 fact artifact, lightningcss fact artifact, independent
   cssparser/golden artifact, strict equality artifact, fact-stream hash, run
   id, host/build metadata, and JSON guard state.

6. W1 must not close on CSS `PARTIAL`, diagnostic source/comment/whitespace
   rows, lightningcss recovery mode, support-only SIMD, or a one-CSS-row close.
   The declaration-values row is a maintain guard, not full CSS close.

## Selected Research Conclusion

The smallest admissible W1 implementation is a SK-V13 CSS comparator/oracle
matrix report consumed by `gate-json --check-results`. It must:

- require all 24 CSS features;
- preserve the existing declaration-values row via the Criterion-backed
  SK-V12 lightningcss/cssparser proof;
- keep all ungenerated rows `OPEN-ABSENT` without fake Mbps;
- reject report-only, stale, missing-coverage, or old SK-V12-only evidence;
- keep JSON guards on the same command invocation.
