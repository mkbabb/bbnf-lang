# Handoff SK-V12

Date: 2026-05-20.

Status: Pass Alpha SK-V11 -> SK-V12 is re-bracketed under
`USER-PIN-W1-CSS-L4-SOTA.md`. The earlier SK-V12 packet remains useful
historical context, but any clause that treats CSS/Sheets/BBNF-self as
preflight-equivalent, sets the CSS close bar to a bbnf baseline lift, or blocks
union/ASM-gen categories at the category level is superseded by the user pin
and the updated `SYNTHESIS.md`.

## 1. Read First

1. `restart/prompts/ORCHESTRATOR.md`
2. `restart/prompts/pass-contracts/PASS-ALPHA.md`
3. `restart/prompts/skinny/PASS-1-PROFILE.md`
4. `restart/prompts/skinny/PASS-2-RESEARCH.md`
5. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
6. `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
7. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
8. `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
9. `restart/skinny/tranches/sk-v12/SPEC.md` as pre-pin implementation context
   only where it does not conflict with the user pin
10. `skinny/RESULTS.md`
11. `skinny/REDRESS.md` through REDRESS 120
12. The six 2026-05-20 audits in `restart/skinny/tranches/sk-v12/research/`.

## 2. Current State

SK-V11 close remains the seed result surface:

| Family | State | SK-V12 role |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | diagnostic only |
| `direct_to_struct` | 4 `A / GO`, 13 `N-direct / NO-GO` | JSON guard and routed ledger |
| `real_typed_struct` | 7 `A / GO` | JSON typed guard surface |
| CSS L4 generated parser | no admitted row | authoritative first target |
| Sheets / BBNF-self | no admitted row | fallback only after CSS redress attempt |
| Overall | `N-direct / NoGo` | seed outcome |

The direct residual rows remain recorded by REDRESS 119/120, but the pin moves
the campaign target to generated CSS L4 > lightningcss and reopens union plus
ASM-gen categories where a new material-differential plan can pass CHALLENGE.
Local citations: `skinny/RESULTS.md:5-45` is the live row table,
`skinny/RESULTS.md:143-146` records the unchanged overall outcome and Track 2
independence notes, REDRESS 119 is `skinny/REDRESS.md:3495-3527`, and REDRESS
120 is `skinny/REDRESS.md:3531-3553`.

## 3. SK-V12 Goalset

SK-V12 priority order is binding:

1. Admit a generated CSS L4 row with Track 1 throughput greater than
   `lightningcss_mbps + 1` on the same corpus, same output plane, same host,
   and strict equality semantics.
   The selected output plane is one canonical CSS fact stream shared by
   generated Track 1, independent Track 2/oracle, and lightningcss.
2. Preserve JSON direct and typed guard rows, or record measured demotions in
   REDRESS. JSON guards are second priority after CSS L4 >SOTA.
   Any generic runtime, codegen, generated-output, benchmark, report, or gate
   change that can produce JSON requires a refreshed JSON guard run unless
   `skinny/RESULTS.md` is proven unchanged.
3. Keep `parse_only` diagnostic. No parse-only row can count as SOTA admission.
4. Treat Sheets and BBNF-self as fallbacks only after a CSS L4 redress attempt
   records measured BLOCKED or REJECTED evidence.
5. Treat Rust union substrate and ASM-gen categories as unblocked at the
   category level. Specific prior REDRESS entries remain measured historical
   implementations that any new plan must cite and materially differentiate.

## 4. Campaign Close

SK-V12 closes by ADMIT or FIXPOINT.

ADMIT:

- generated CSS L4 Track 1 > `lightningcss_mbps + 1`;
- independent oracle or Track 2 strict equality;
- gate-consumed provenance and lightningcss comparator evidence;
- Lock 14 clean after resolving the generic JSON leaks needed for CSS L4
  emission;
- Lock 16 clean for any SIMD admission;
- the carried orphan set is zero by admission, removal, or inventory demotion
  with evidence: `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_context`, and `cache_hints`;
- JSON guards held or measured-disposition demoted.

FIXPOINT:

- CSS L4 has at least one measured redress attempt;
- Sheets/BBNF-self were not used before the CSS redress attempt;
- a new measured union-substrate attempt exists in the closing tranche;
- a new measured ASM-gen attempt exists in the closing tranche;
- orphan production SIMD primitives are zero by admission, removal, or
  explicit inventory demotion;
- REDRESS records every measured miss and all close docs agree.

## 5. W0 Handling

W0 telemetry/gate lock at commit `f788eb97` is revalidated, not redone. Treat
it as valid unless revalidation shows drift in the gate/report surface, source
baseline, or JSON result surface. Alpha-F does not authorize a W0 rewrite.

## 6. Required Pass Re-Derivation

Next move after Alpha hardening converges: present G-Alpha, then run SK-V12
S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan under the user pin. The
pre-pin S-P1/S-P2 artifacts may be cited only after fresh measured
revalidation; they are not substitute pass convergence.

The new S-P3 plan must:

- select CSS L4 first;
- name the exact CSS L4 row, output plane, generated Track 1 path, runtime
  module, fixture/input source, independent oracle/Track 2, lightningcss
  comparator, equality command, benchmark command, gate command, and rollback
  slice;
- use strict `generated_track1_mbps > lightningcss_mbps + 1` as the admission
  floor; equality at `+1` is a miss;
- land `GrammarConfig` or equivalent generated metadata before CSS L4 emission
  can legally leave JSON-only templates;
- verify and resolve the `escape_mask_64` NEON correctness bug before any new
  SIMD admission;
- carry Sheets and BBNF-self only as post-CSS-redress fallbacks;
- re-evaluate union and ASM-gen routes under the user pin, with REDRESS
  citation and material differential.

Seed wave split for S-P3 to adjudicate:

| Seed | Role | Cap | Failure path |
|---|---|---|---|
| W0 | revalidate `f788eb97` telemetry/gate lock | docs-only, 20/15/30 | return to S-P3 on drift |
| W1a | `GrammarConfig` legality / JSON parity | <=360 hand LOC | `/tmp/skv12-waveW1a-rejected.patch` |
| W1b | CSS L4 generated baseline + lightningcss comparator | <=620 hand LOC | `/tmp/skv12-waveW1b-rejected.patch` |
| W2 | `escape_mask_64` correctness | <=180 hand LOC | `/tmp/skv12-waveW2-rejected.patch`; SIMD remains blocked |
| W3 | CSS-local same-tape union attempt | <=420 hand LOC | `/tmp/skv12-waveW3-rejected.patch` |
| W4 | ARMv9.2 ASM-gen consumer + orphan disposition | <=430 hand LOC | `/tmp/skv12-waveW4-rejected.patch` |
| W5 | close / Alpha feedback | docs-only | synthesize SK-V13 if close unmet |

## 7. Telemetry Binding

The CSS L4 gate or companion report must consume grammar id, row id, output
plane, strictness, generated source/runtime paths, grammar/input checksums,
oracle path, lightningcss comparator artifact and Mbps, strict equality,
Track 1 Mbps, oracle Mbps, sample count, sample cost, run id, host triple,
feature mask, build flags, profile artifact, benchmark artifact, Lock 14
status, Lock 16 status where applicable, same-wave consumer class, JSON guard
state, gate status, wave id, and REDRESS id.

Producer-only fields, stale run ids, oracle coupling, grammar-name branches in
generic crates, missing lightningcss evidence, parse-only admission, and orphan
SIMD primitives fail closed.

## 8. Refusal Conditions

Refuse or return REVISE for any dispatch that:

- asks this Alpha-F lane to edit `SPEC.md` or `DISPATCH-PROMPT.md`;
- skips CSS L4 before a CSS redress attempt;
- uses `ceil(baseline_mbps * 1.01)` as the CSS close bar;
- treats REDRESS 96/97/98 or 88/89/90 as category-level blockers after the
  user pin;
- admits SIMD before the `escape_mask_64` bug is verified and resolved;
- leaves production SIMD primitives orphaned at close;
- claims grammar generalization by prose or stale hand-only witness modules;
- adds a directive, BIR variant, BackendShape variant, public substrate API,
  parser-owned sidecar, or x86 implementation target.

## 9. Dispatch Boundary

This handoff authorizes no source work. It authorizes G-Alpha presentation and
then the pin-aware S-P1 -> S-P2 -> S-P3 pass sequence. Downstream S-P3 packet
text may update `SPEC.md` and `DISPATCH-PROMPT.md` in its own lane only after
those passes converge.
