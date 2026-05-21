# SK-V12 W1b-2b Challenge V2 - CH2 Generality / Lock 14

Lens: CH2 generality / Lock 14.
Verdict: ACCEPT.

## Scope

Reviewed `SKINNY-TRIUMVIRATE.md` Section 4, `SPEC.md` Sections 2.1 and 7.2,
`PLAN-V2.md`, and the SK-V12 value/API Lock-14 audit. This review covers only
the revised W1b-2b report/gate plan. It does not authorize source edits,
directive/BIR/substrate expansion, fallback grammar resequencing, or SIMD/ASM
admission.

## Findings

1. ACCEPT - PLAN-V2 proves generality by executable CSS evidence, not prose.

   SPEC Section 2.1 requires CSS L4 benchmark/equality evidence instead of a
   prose Lock-14 assertion. PLAN-V2 binds the single CSS row
   `css_l4/declaration_values/direct_to_struct/main`, the output plane
   `css_l4_declaration_value_fact_stream`, fixture SHA, `input_bytes == 187`,
   strict three-way equality, independent cssparser oracle status, and
   lightningcss sequence status. The companion gate then consumes the three
   Criterion lanes and recomputes throughput, threshold, and margin from
   `new/` artifacts. That is an executable CSS L4 proof for this wave.

2. ACCEPT - Lock 14 authority remains process-owned.

   PLAN-V2 allows the report to carry
   `lock14_status == pass:lock14_baseline::validate`, but the gate command
   still runs through `gate.rs`, whose existing entry path invokes the Lock 14
   baseline validator before companion report acceptance. This preserves the
   important distinction: the JSON report string is evidence, while the gate
   process is authority. Implementation must not add a CSS-only bypass around
   the baseline validator.

3. ACCEPT - No generic JSON policy leak is introduced.

   The value/API audit's seven leaks are codegen/runtime policy leaks. PLAN-V2
   does not touch those surfaces. It explicitly refuses to widen
   `sk-v12-nonjson-generated-v1`, adds a dedicated
   `sk-v12-css-l4-sota-v1` report schema, and limits owner paths to
   `bbnf-bench` report/gate code plus the measured report artifact, REDRESS,
   and conditional RESULTS movement. This avoids encoding CSS or JSON parser
   policy into a generic runtime or codegen surface.

4. ACCEPT - The companion gate preserves no-substrate/no-directive boundaries.

   PLAN-V2 does not authorize any new directive, BIR variant, `BackendShape`
   variant, public substrate API, parser-owned sidecar, SIMD/ASM claim, or
   public output-plane expansion. `lock16_status ==
   n/a:no_simd_or_asm_claim` is admissible for W1b-2b because this wave is
   report/gate consumption of a scalar comparator row, not a primitive admit.

5. ACCEPT - JSON guard coupling is fail-closed.

   PLAN-V2 requires CSS gate validation before optional JSON guard continuation
   and states that CSS-only Criterion roots are rejected by the existing JSON
   path. Write/update/probe flags and mixed companion reports are rejected by
   the shared companion parser. RESULTS movement is limited to
   `PASS-ADMIT-CANDIDATE` or accepted JSON guard demotion; a measured baseline
   must leave RESULTS byte-identical.

## Required Implementation Invariants

- Keep `--skv12-css-l4-sota-report` on the existing `gate.rs` entry path so
  `lock14_baseline::validate` runs before companion acceptance.
- Reject mixed companion reports, write/update flags, volatile probes, and
  CSS-only Criterion roots used as JSON guard proof.
- Recompute `threshold_mbps = lightningcss_mbps + 1` and
  `admission_margin_mbps` from Criterion `new/` telemetry; never trust report
  math as throughput authority.
- Keep W1b-2b to the single CSS L4 declaration-values row and the dedicated
  `sk-v12-css-l4-sota-v1` schema.
- Do not add directive, BIR, `BackendShape`, public substrate API, sidecar
  substrate, parser-owned generic CSS/JSON branch, or SIMD/ASM admission.

## Blocking Findings

None.

## CH2 Verdict

ACCEPT. PLAN-V2 is sufficiently grammar-neutral for W1b-2b because it proves
the CSS L4 row by executable equality and Criterion evidence, while preserving
Lock 14, no-substrate, no-directive, JSON guard, and no-write boundaries.
