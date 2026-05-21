# SK-V12 W1b-1 CHALLENGE V2 CH4: Cost / LOC / Generated Size / Deps

Date: 2026-05-20.
Scope: W1b-1 CHALLENGE V2 lens CH4 only.
Output: this file.

## Verdict

ACCEPT-CONDITIONAL.

Plan V2 repairs the CH4 V1 blocker enough to authorize redress. It does not
prove the implementation will PASS inside the pinned `<=30 min` wall, but it
now makes the cap falsifiable: the redress agent must stop at the cap, save the
patch, and record BLOCKED/FAIL instead of silently widening W1b-1 or rolling the
work into W1b-2.

The source budget is still tight. The revised plan budgets exactly 360 hand
source LOC, including a 15 LOC contingency, so CH4 accepts only if redress
counts the implementation slice before PASS and rejects any over-budget patch.
Generated runtime output remains outside the hand cap, but generated LOC and
module bytes must be measured and gate-consumed.

## Evidence

- SPEC Section 2 keeps W1b-1 at `<=360 hand; generated output named
  separately`, high risk, `<=30 min`; it also binds the 0.9x-cap commit/blocking
  state and cap halt rule. Plan V2 preserves that cap instead of amending it.
- SPEC Section 6 now owns the full redress surface CH4 needs, including
  `skinny/Cargo.toml`, `bbnf-bench/Cargo.toml`, the CSS provider/profile paths,
  runtime export/generated runtime paths, `report.rs`, `gate.rs` if needed,
  `src/bin/gate.rs`, and `lock14_baseline.rs`. The earlier hidden owner-cost
  blocker is gone.
- Plan V2 narrows the source allocation from the V1 overrun shape to exactly
  360 hand LOC: provider/profile/tests `<=85`, runtime sink/export `<=55`,
  oracle/equality/bench `<=90`, report/gate validation `<=95`, Lock 14 delta
  `<=20`, contingency `<=15`.
- The revised budget explicitly excludes fixture bytes, report JSON, retained
  artifacts, generated runtime output, and REDRESS/docs from hand source LOC.
  That is acceptable because SPEC names generated output separately, but redress
  must report those non-source sizes rather than hiding them.
- The report/gate work remains the highest CH4 risk. Current
  `SkV12NonJsonRow` is already a consumed schema with `deny_unknown_fields`, and
  current validation maps `direct_to_struct` to `direct_sink`. W1b-1 must add
  the CSS-specific `css_l4_declaration_value_fact_stream` plane and the V2
  generated-size/Lock14/Lock16/scalar/parity fields without turning the report
  patch into a broad telemetry refactor.
- The codegen roster is still JSON-only today:
  `runtime_profiles()` returns only `json_provider::runtime_profile()`. Adding
  a CSS provider/profile is feasible and in-scope, but any generic
  `if grammar_name == "css_l4"` policy branch would spend LOC while failing
  Lock 14.
- The dependency shape is acceptable if narrow: `cssparser` belongs in
  `skinny/Cargo.toml` workspace dependencies and is consumed only by
  `bbnf-bench`. It must not enter `runtime`, `codegen`, generated Track 1,
  or any generic parser/runtime path.
- The measurement suite is still heavy for 30 minutes: four cargo tests,
  Criterion, companion gate, JSON bench/gate rerun, cost-facts gate, and floor
  verification. V2 makes this acceptable only by making overrun a measured
  REDRESS result, not a license to continue past the cap.

## Required Redress Preconditions

1. Count hand source LOC for the W1b-1 implementation slice before PASS. The
   count must include Rust source and Rust tests in codegen, runtime,
   `bbnf-bench`, and Lock 14 touched by W1b-1. It must exclude generated
   runtime output, fixture bytes, report JSON, artifacts, and REDRESS/docs.
2. Reject the wave if the hand source slice exceeds 360 LOC. Do not move
   excess implementation into generated files, report JSON, or docs to evade
   the cap.
3. Record generated runtime LOC and module bytes in the companion report, with
   the V2 `<=300 generated LOC` and `<=14000 module bytes` target or a
   gate-consumed O(N) explanation against the CSS grammar-input baseline.
4. Keep `cssparser` bench-only: workspace dependency plus `bbnf-bench`
   dependency is acceptable; runtime/codegen/generated use is a CH4 reject.
5. Keep W1b-1 scalar-only. No `bbnf-simd`, aarch64 helper, ASM helper,
   `lightningcss`, root CSS runtime, JSON runtime reuse, or `parse_that_regex`
   dependency can appear in this wave.
6. Enforce the wall-clock boundary: at 0.9x cap, if generated runtime
   reproducibility and Track 1/oracle equality are not green, redress saves
   `/tmp/skv12-waveW1b-1-rejected.patch` and records BLOCKED/FAIL.
7. Treat Criterion, the companion non-JSON gate, JSON guard, cost-facts gate,
   and floor verification as mandatory measurement. If they cannot run inside
   the cap, record a measured overrun in REDRESS; do not defer them to W1b-2.
8. Keep report/gate validation targeted to W1b-1 fields and negative tests.
   Broad telemetry-schema cleanup, main JSON `RESULTS.md` columns, new outcome
   variants, or lightningcss placeholders are out of scope.

## Blockers

No CH4 plan-time blocker remains after V2.

The remaining risks are redress-time fail-closed conditions:

- exceeding 360 counted hand source LOC;
- failing to produce consumed generated LOC/module-byte telemetry;
- needing `cssparser` outside `bbnf-bench`;
- using generic CSS policy in codegen/runtime instead of a CSS-owned
  provider/profile;
- treating the mandatory measurement suite as after-cap work;
- attempting any SOTA/lightningcss admission in W1b-1.

Any one of those conditions rejects the wave, but none requires another plan
revision before redress.
