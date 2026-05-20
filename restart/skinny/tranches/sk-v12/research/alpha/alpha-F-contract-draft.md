# SK-V12 Alpha-F Contract Draft

Pass: Pass Alpha re-bracket. Cycle: USER-PIN V1.
Date: 2026-05-20.
Agent: alpha-F.
Scope: SK-V11 -> SK-V12 contract draft under
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
Output: this file, plus `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
and `restart/skinny/tranches/sk-v12/HANDOFF.md`.

This lane does not create or edit `SPEC.md` or `DISPATCH-PROMPT.md`.
The existing SK-V12 implementation packet predates the user pin wherever it
still treats CSS/Sheets/BBNF-self as preflight-equivalent or keeps the
union/ASM-gen categories blocked. S-P3 must re-derive the W1 plan and later
packet text under this contract.

## 1. Authorities Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/SPEC.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS 120
- `restart/skinny/tranches/sk-v12/research/skv12-W1-A7-sheets-execution-scout.md`
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-profile-truth-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`
- User dispatch for this alpha-F lane.

## 2. Pin Delta

The user pin is contract authority for SK-V12. It supersedes the earlier Alpha
contract at the points below:

1. CSS L4 is authoritative. W1 attempts the generated CSS L4 parser redress
   first. Sheets and BBNF-self are fallbacks only after a CSS L4 redress
   attempt records measured BLOCKED or REJECTED evidence in REDRESS. A CSS
   preflight-only miss is not sufficient to skip to Sheets.
2. The close bar is >SOTA against lightningcss, not a 1% lift over bbnf's own
   non-JSON baseline. The CSS L4 row admits only when generated Track 1 Mbps is
   greater than `lightningcss_mbps + 1` on the same corpus and output plane,
   with strict equality and same-host provenance.
3. The Rust union substrate category is unblocked. REDRESS 96, 97, and 98
   remain historical measured-rejected implementations, but the architectural
   category may dispatch again if the new plan cites them, names the material
   differential, passes CHALLENGE, and satisfies scalar/reference, parity, and
   same-wave consumer gates.
4. The ASM-gen category is unblocked. REDRESS 88, 89, and 90 remain historical
   measured-rejected implementations, but PMULL, CSSC CTZ, EOR3/BCAX, UDOT,
   TBL/TBX, or other ARMv9.2-A candidates may dispatch after micro-proof,
   scalar reference, checkasm/parity, strict equality, and same-wave consumer.
5. SIMD utilization is a close concern. The five orphan aarch64 primitives
   named by the coverage audit must be removed, demoted as inventory-only with
   evidence, or wired to same-wave consumers before close.
6. Parse time and >SOTA are first priority. JSON guard floors are second.
   `parse_only` remains diagnostic-only and cannot admit a SOTA row.

## 3. Section 0 Close Contract

SK-V12 closes by exactly one of two routes.

### ADMIT

ADMIT requires all of the following:

1. A generated CSS L4 row admits on the CSS L4 output plane selected by S-P3.
2. Generated Track 1 throughput is strictly greater than
   `lightningcss_mbps + 1` on the same corpus, same output plane, same host,
   and same strictness semantics.
3. Strict equality passes against an independent oracle or Track 2, and the
   oracle path is not generated Track 1 in disguise.
4. The gate consumes provenance for grammar id, corpus/workload, generated
   source path, generated runtime path, fixture/input checksum, oracle path,
   lightningcss comparator command/artifact, run id, host triple, build flags,
   feature mask, sample count, Track 1 Mbps, oracle Mbps, strict equality, and
   JSON guard state.
5. Lock 14 Section 2.1 passes. The seven generic-crate JSON leaks identified
   in `skv12-value-api-audit.md` are resolved through a `GrammarConfig`
   surface or an equivalent grammar-derived metadata surface before CSS L4
   emission is legal.
6. Lock 16 passes. No new SIMD admission occurs until the `escape_mask_64`
   NEON correctness bug identified by the totality fold scout is verified and
   resolved, and every admitted primitive has scalar reference, checkasm/parity,
   and same-wave consumer evidence.
7. The JSON guard floors from the opening result surface hold or record
   measured-disposition demotions in REDRESS.

### FIXPOINT

FIXPOINT is legal only if ADMIT is measured uncloseable across a full Pass
Alpha bracket and every condition below is met:

1. The CSS L4 route has at least one measured redress attempt, not only a
   preflight refusal. The attempt records the selected corpus, lightningcss
   comparator, equality oracle, Track 1 evidence, failure mode, and rejected
   or blocked patch path.
2. Sheets and BBNF-self were considered only after the CSS L4 redress attempt.
   They may support routed remainder, but they do not replace the CSS close bar.
3. The closing tranche records at least one new measured union-substrate
   implementation attempt with material differential against REDRESS 96/97/98.
4. The closing tranche records at least one new measured ASM-gen attempt with
   material differential against REDRESS 88/89/90 or another ARMv9.2-A route
   from the aarch64 audit.
5. The union and ASM-gen attempts both carry fresh profile evidence,
   microbench evidence, strict equality/parity evidence, and same-wave
   consumer evidence. Orphan primitives at close invalidate FIXPOINT.
6. REDRESS names every measured miss, the close docs agree, and the routed
   remainder is explicit enough for SK-V13 if the campaign continues.

## 4. Result Surface Carried Into The Re-Bracket

`skinny/RESULTS.md` remains the SK-V11 close surface until S-P1/S-P3 refreshes
it under the pin:

| Family | Current state | Pin-aware SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | JSON guard and routed ledger |
| `real_typed_struct` | 7 `A / GO` | JSON typed guard surface |
| CSS L4 generated parser | no admitted row | authoritative first target |
| Sheets / BBNF-self | no admitted row | fallback only after CSS redress attempt |
| Overall | `N-direct / NoGo` | seed outcome, not close |

The REDRESS 119 direct residual table remains historical evidence. The user pin
does not make JSON direct rows first priority; it makes parse-time >SOTA first
priority through the CSS L4 generated row and permits new union/ASM-gen routes
where they have a real CSS or guard-row consumer.

## 5. Required W1 Contract For S-P3

S-P3 must re-derive W1 under the pin. The W1 plan must:

1. Select CSS L4 first and name the exact declaration-value corpus/workload,
   generated Track 1 path, generated runtime module, fixture/input source,
   independent oracle/Track 2 path, lightningcss comparator command, equality
   command, benchmark command, gate command, and rollback slice.
2. Treat Sheets and BBNF-self as post-redress fallbacks only. A CSS preflight
   limitation becomes a CSS W1 redress attempt or an S-P3 manifest revision,
   not an immediate fallback.
3. Land the `GrammarConfig` or equivalent grammar-derived metadata surface
   needed to remove JSON policy from generic generated templates before CSS L4
   emission can compile.
4. Verify and resolve the `escape_mask_64` NEON bug before any SIMD admission.
   If W1 contains no SIMD body, it must still record the SIMD-admission block
   so W2+ cannot route around it.
5. Re-evaluate union and ASM-gen candidates as admissible categories, not as
   globally pre-blocked families. Any candidate must cite prior REDRESS and
   name material differential.
6. Preserve W0 as revalidated, not redone. Commit `f788eb97` remains the W0
   telemetry/gate lock unless the revalidation command shows drift.

## 6. Telemetry Binding

The CSS L4 row may enter `skinny/RESULTS.md` or a gate-consumed companion
report. Either path must fail closed on missing:

- row id, grammar id, domain, workload, output plane, strictness;
- generated Track 1 source path and generated runtime path;
- grammar source or generated metadata checksum;
- fixture/input provenance and byte count;
- independent oracle or Track 2 source path;
- lightningcss comparator Mbps and artifact path;
- strict equality result and measured validation path;
- Track 1 Mbps, oracle Mbps, sample count, sample cost, run id, host triple,
  feature mask, build flags, profile artifact, and benchmark artifact;
- Lock 14 scan result, Lock 16 result where applicable, same-wave consumer
  class, JSON guard state, gate status, wave id, and REDRESS id.

Producer-only fields, stale run ids, oracle coupling, grammar-name branches in
generic crates, parse-only admission, missing lightningcss comparator evidence,
or orphan SIMD primitives reject the wave.

## 7. Refusal Conditions

Refuse or return REVISE for any SK-V12 dispatch that:

- asks Alpha-F to edit `SPEC.md` or `DISPATCH-PROMPT.md`;
- skips CSS L4 in favor of Sheets or BBNF-self before a CSS redress attempt;
- uses `ceil(baseline_mbps * 1.01)` as the CSS close bar instead of
  `lightningcss_mbps + 1`;
- treats REDRESS 96/97/98 or 88/89/90 as category-level blockers after the
  user pin, rather than historical implementation evidence;
- admits a SIMD/ASM route before the `escape_mask_64` bug is verified and
  resolved;
- leaves an admitted or inventory-visible aarch64 primitive orphaned at close;
- claims grammar generalization by prose or by hand-only witness modules;
- adds a new directive, BIR variant, BackendShape variant, public substrate
  API, parser-owned sidecar, or x86 implementation route.

## 8. G-Alpha Presentation Seed

Pin-aware SK-V12 asks G-Alpha to authorize this revised contract:

- Target row: generated CSS L4 direct or typed row selected by S-P3.
- Admission floor: generated Track 1 > `lightningcss_mbps + 1`.
- Fallback: Sheets/BBNF-self only after CSS L4 redress attempt records
  measured disposition.
- W0: revalidate `f788eb97`; do not redo unless drift is measured.
- W1 next move: S-P3 re-derives W1 under the pin.
- Campaign fixpoint: allowed only after CSS, union-substrate, and ASM-gen
  categories each have new measured evidence in the closing tranche.

This Alpha-F draft deliberately leaves `SPEC.md` and `DISPATCH-PROMPT.md`
unchanged. They are downstream S-P3 products and remain stale where they
contradict the user pin.
