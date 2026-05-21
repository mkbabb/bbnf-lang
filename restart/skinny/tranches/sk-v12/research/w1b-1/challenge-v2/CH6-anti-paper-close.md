# SK-V12 W1b-1 CHALLENGE V2 CH6 - Anti-Paper-Close

Date: 2026-05-20.
Scope: W1b-1 CHALLENGE V2 lens CH6, anti-paper-close / measurable scaffold
integrity.
Output: this file only.

## Verdict

ACCEPT FOR REDRESS, fail-closed.

Plan V2 can close as a real measurable scaffold, not a paper close, if redress
implements the plan's gate-consumed fields exactly. The V1 CH6 shape was already
directionally admissible; V2 repairs the two paper-close risks that mattered to
this lens:

- `lock14_baseline.rs` is now an owned W1b-1 path, so `lock14_status = pass`
  can be produced by the executable gate rather than self-reported.
- The implementation is narrowed to a scalar-only CSS L4 generated Track 1,
  independent `cssparser` oracle, retained fact streams, companion non-JSON
  report validation, and JSON guard accounting. W1b-2 still owns the
  `lightningcss_mbps + 1` admission bar.

This verdict is not CSS L4 SOTA admission. W1b-1 may only close as
`C / GO` scaffold/equality for
`css_l4/declaration_values/direct_to_struct/main` on
`css_l4_declaration_value_fact_stream`, with finite Track 1 and oracle Mbps.

## Evidence

- V1 CHALLENGE consolidated to `REVISE before redress` only because CH2 lacked
  `lock14_baseline.rs` owner authority and CH4 found the initial slice too wide
  for the pinned cap. V1 CH6 accepted the anti-paper-close shape with hard
  redress preconditions.
- PLAN V2 now owns `skinny/crates/bbnf-bench/src/lock14_baseline.rs` and
  requires a W1b-1 frozen-root / parent-diff authorization keyed to the
  redress commit. The authorization is limited to the Section 6 CSS scaffold
  owner slice and keeps substrate, IR, grammar-crate, pass-crate, SIMD,
  directives, BIR variants, and `BackendShape` outside the allowance.
- SPEC Section 6 now includes the same Lock 14 owner path and requires the
  companion non-JSON gate to consume generated size, grammar/input checksums,
  strictness, validation/profile artifacts, Lock 14/16 status,
  scalar-reference status, and parity status in the same wave.
- PLAN V2 fixes the row and plane exactly:
  `row_id = css_l4/declaration_values/direct_to_struct/main` and
  `output_plane = css_l4_declaration_value_fact_stream`.
- The generated Track 1 proof is falsifiable: redress must add a
  CSS-owned profile/provider/templates surface that emits `mod.rs`, `config.rs`,
  `parser.rs`, and `generated.rs`, then run a reproducibility test that
  byte-compares rendered output with the committed runtime files. A hand parser
  under a generated directory cannot satisfy this.
- The oracle route remains independent: PLAN V2 selects `cssparser` in
  `bbnf-bench` and forbids calls into generated Track 1,
  `runtime::generated_json`, root CSS runtime, `lightningcss`,
  `parse_that_regex`, or `bbnf-simd`.
- The gate-consumption surface is explicit. W1b-1 PASS requires strictness,
  grammar checksum, input checksum, input bytes, measured validation path,
  profile artifact, generated LOC, generated module bytes, grammar-size guard,
  Lock 14/16 status, scalar-reference status, parity status, finite Track 1 and
  oracle Mbps, `sample_count >= 30`, strict equality, independence, and the
  same-wave consumer class.
- Current `skinny/RESULTS.md` has only JSON rows and remains overall
  `N-direct / NoGo`; W1b-1 must not move those rows or add a main JSON table
  CSS/SOTA column. REDRESS 121 and 122 are prerequisites only: W1a repaired the
  GrammarConfig/Lock 14 legality surface, and W2 repaired the
  `escape_mask_64` correctness prerequisite. Neither admitted CSS L4.

## Required Redress Preconditions

1. Generate Track 1 for real. The CSS runtime files must be emitted by the
   CSS-owned provider/profile and reproduced byte-for-byte by a test equivalent
   to `css_l4_declaration_values_generated_runtime_reproducible`.
2. Keep W1b-1 scalar-only. Do not touch `bbnf-simd`, aarch64 modules, ASM-backed
   helpers, `lightningcss`, public substrate APIs, directives, BIR variants, or
   `BackendShape`.
3. Preserve oracle independence. The oracle/bench must not call generated
   Track 1, `runtime::generated_json`, root CSS runtime, `json_provider`,
   `parse_that_regex`, `bbnf-simd`, or `lightningcss`.
4. Prove equality by retained bytes. PASS requires byte equality of retained
   Track 1 and oracle fact streams, plus first-diff artifacts on failure; parse
   success, declaration counts, pretty CSS equality, or digest-only equality are
   not enough.
5. Harden the companion non-JSON gate before PASS. The gate must reject missing
   or invalid strictness, grammar/input checksums, input bytes, measured
   validation path, profile artifact, generated LOC, generated module bytes,
   grammar-size guard, Lock 14/16 status, scalar-reference status, parity
   status, retained artifact paths/hashes, equality, independence, and sample
   count.
6. `lock14_status = pass` must be executable evidence from the same
   `gate-json --skv12-non-json-report ... --check-results` run, not a report
   string accepted on trust.
7. The W1b-1 report row must be exact:
   `outcome_id = C`, `verdict = GO`,
   `track1_mbps >= 1.0`, `track2_or_oracle_mbps >= 1.0`,
   `sample_count >= 30`, `strict_output_equality = pass`,
   `track2_independence_status = independent_verified`,
   `same_wave_consumer_class = companion_gate_generated_baseline`,
   `lock16_status = not_applicable:scalar_only`,
   `scalar_reference_status = generated_scalar_track1`, and
   `checkasm_or_parity_status = parity_pass`.
8. Do not claim CSS SOTA, do not add `lightningcss_mbps`, do not add a new
   outcome variant, and do not add CSS columns to the main JSON `RESULTS.md`
   table in W1b-1.
9. Run the full JSON guard path because W1b-1 moves codegen selection, runtime
   exports, report/gate validation, and bench dependencies. If generated JSON
   output or JSON behavior changes unexpectedly, `check-json`,
   `check-real-typed`, and `check-conformance` must pass before measurement.
10. On any failed precondition after source edits, save
    `/tmp/skv12-waveW1b-1-rejected.patch`, revert only the W1b-1 owner slice,
    and record measured `BLOCKED/FAIL` in REDRESS. Do not substitute Sheets,
    BBNF-self, JSON rows, root CSS runtime, `complex-errors.css`, or a
    report-only close.

## Blockers

- No CH6 blocker remains before redress. V2 repairs the V1 owner-authority gap
  and narrows the slice enough for anti-paper-close review to proceed.
- Redress must still fail closed if the current companion gate is not extended
  to consume every required W1b-1 field. Producer-only report fields would make
  the close paper.
- Redress must fail closed if the CSS runtime is generated only by path/name,
  if the oracle shares Track 1 internals, if retained fact artifacts are absent,
  or if W1b-1 tries to claim the W1b-2 lightningcss admission gate.
