# SK-V12 W1b-2b Challenge V3 - CH2 Generality / Lock 14

Lens: CH2 generality / Lock 14.
Verdict: ACCEPT.

## Scope

Reviewed `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` Section 4,
`restart/skinny/tranches/sk-v12/SPEC.md` Sections 2.1 and 7.2,
`restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`, and the
SK-V12 value/API Lock-14 audit. This review covers only the W1b-2b PLAN-V3
report/gate disposition. It does not authorize source outside the named owner
paths, any directive/BIR/`BackendShape` expansion, public substrate API, SIMD or
ASM admission, or fallback grammar resequencing.

## Findings

1. ACCEPT - PLAN-V3 proves the non-JSON row by executable CSS evidence.

   SPEC Section 2.1 requires CSS L4 to be exercised by benchmark/equality, not
   prose. PLAN-V3 binds W1b-2b to the single row
   `css_l4/declaration_values/direct_to_struct/main`, the plane
   `css_l4_declaration_value_fact_stream`, fixture SHA, `input_bytes == 187`,
   strict three-way equality, cssparser oracle independence, lightningcss
   sequence status, retained fact-stream hashes, and Criterion `new/` telemetry.
   The planned gate recomputes `threshold_mbps = lightningcss_mbps + 1` and the
   margin from artifacts, so the CSS L4 proof is executable.

2. ACCEPT - CSS policy stays out of generic runtime and codegen surfaces.

   The value/API audit's Lock-14 leaks are generic runtime/codegen policy leaks.
   PLAN-V3 does not touch those surfaces. Its owner paths are limited to
   `bbnf-bench` report/gate code, the CSS SOTA report artifact, REDRESS, and the
   SPEC budget correction. The dedicated `sk-v12-css-l4-sota-v1` schema is
   explicitly a companion evidence schema, not an extension of the non-JSON
   generated baseline schema and not a parser policy surface.

3. ACCEPT - Lock 14 process authority remains executable.

   PLAN-V3 requires `lock14_status == pass:lock14_baseline::validate` in the
   report and keeps the CSS report on the existing gate path. Implementation
   must preserve that ordering: `lock14_baseline::validate` is the authority,
   while the report field is only recorded evidence. There is no planned bypass
   that would let a CSS-specific report string substitute for the Lock-14 gate.

4. ACCEPT - No substrate, directive, or output-plane expansion is authorized.

   PLAN-V3 does not add a directive, BIR variant, `BackendShape` variant, public
   substrate API, parser-owned sidecar substrate, or generic CSS/JSON branch.
   `lock16_status == n/a:no_simd_or_asm_claim` is correct for this wave because
   W1b-2b consumes scalar comparator artifacts and makes no SIMD/ASM primitive
   admission.

5. ACCEPT - Split CSS and JSON evidence roots reduce grammar coupling.

   PLAN-V3 fixes the V2 coupling risk by requiring separate CSS SOTA and JSON
   guard commands. CSS validation reads only the three `nonjson_css_l4` lanes
   from the CSS Criterion root; JSON guard/stale checking runs separately
   against `/tmp/skv12-w1a-json-guard-criterion` with no CSS report flag. That
   keeps CSS row disposition from becoming a generic stale-results or JSON guard
   policy change.

6. ACCEPT - RESULTS routing is bounded for Lock 14.

   PLAN-V3 routes `skinny/RESULTS.md` reconciliation to W5 even on
   `PASS-ADMIT-CANDIDATE`. For CH2 this is the safer grammar-neutral boundary:
   W1b-2b can measure and admit the CSS row candidate without broadening the
   JSON RESULTS renderer or stale-check semantics in a companion-gate wave.

## Required Implementation Invariants

- Keep `--skv12-css-l4-sota-report` on the existing `gate.rs` entry path so
  `lock14_baseline::validate` runs before companion report acceptance.
- Reject mixed companion reports, write/update flags, volatile probes, missing
  paths, flag-as-path values, and unrelated extra arguments.
- Keep the CSS report validator row-specific and schema-specific; do not widen
  `sk-v12-nonjson-generated-v1` or generic RESULTS rendering in W1b-2b.
- Verify the retained Track 1, cssparser, and lightningcss fact streams as
  separate artifacts; do not let `lightningcss_facts` call the cssparser oracle
  path.
- Do not add directive, BIR, `BackendShape`, public substrate API, sidecar
  substrate, parser-owned generic CSS/JSON branch, or SIMD/ASM admission.

## Blocking Findings

None.

## CH2 Verdict

ACCEPT. PLAN-V3 is sufficiently grammar-neutral for W1b-2b because it keeps CSS
specificity inside a measured companion report/gate, preserves executable
Lock-14 authority, and avoids generic runtime/codegen, directive, substrate, and
ASM/SIMD expansion.
