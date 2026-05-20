# Handoff SK-V12

Date: 2026-05-20.

Status: ready-for-wave-W0. Pass Alpha SK-V11 -> SK-V12 has been re-bracketed
under `USER-PIN-W1-CSS-L4-SOTA.md`; pin-aware S-P1 Profile, S-P2 Research, and
S-P3 Synthesis-Plan have converged. The earlier Sheets-first SK-V12 packet is
historical context only where it does not conflict with the user pin or the
current `SPEC.md`.

## 1. Read First

1. `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
2. `restart/prompts/ORCHESTRATOR.md`
3. `restart/prompts/pass-contracts/PASS-ALPHA.md`
4. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
5. `restart/prompts/skinny/PASS-1-PROFILE.md`
6. `restart/prompts/skinny/PASS-2-RESEARCH.md`
7. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
8. `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
9. `restart/skinny/tranches/sk-v12/SPEC.md`
10. `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md`
11. `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
12. `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
13. `restart/skinny/tranches/sk-v12/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`
14. The six accepted S-P2 reports under `restart/skinny/tranches/sk-v12/research/p2/`
15. The six 2026-05-20 audits under `restart/skinny/tranches/sk-v12/research/`
16. `skinny/RESULTS.md`
17. `skinny/REDRESS.md`

## 2. Current State

SK-V11 close remains the seed result surface:

| Family | State | SK-V12 role |
|---|---|---|
| `parse_only` | diagnostic-only concession | no SOTA admission target |
| `direct_to_struct` | JSON guard plus routed ledger | guard second after CSS L4 |
| `real_typed_struct` | JSON typed guard surface | guard second after CSS L4 |
| CSS L4 generated parser | no admitted row | authoritative first target |
| Sheets / BBNF-self | no admitted row | fallback only after measured CSS redress |
| Overall | campaign still open | close by ADMIT or FIXPOINT |

The pin moves the campaign target to generated CSS L4 > lightningcss and
reopens union plus ASM-gen categories at category level. REDRESS 96/97/98 and
88/89/90 remain measured historical implementations; any new adjacent wave
must cite them, name the material differential, and pass CHALLENGE before
redress.

## 3. SK-V12 Goalset

SK-V12 priority order is binding:

1. Admit a generated CSS L4 row with Track 1 throughput strictly greater than
   `lightningcss_mbps + 1` on the same corpus, same output plane, same host,
   and strict equality semantics.
2. Preserve JSON direct and typed guard rows, or record measured demotions in
   REDRESS. JSON guards are second priority after CSS L4 >SOTA.
3. Keep `parse_only` diagnostic. No parse-only row can count as SOTA admission.
4. Treat Sheets and BBNF-self as fallbacks only after W1b-2 records a measured
   CSS L4 lightningcss comparator/admission redress.
5. Treat Rust union substrate and ASM-gen categories as unblocked at category
   level, with material-differential and CHALLENGE discipline intact.

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

## 5. Wave Order

| Wave | SPEC section | Title | Dispatch status |
|---|---|---|---|
| W0 | Section 3 | Pin Telemetry And Gate Revalidation | Dispatch now |
| W1a | Section 4 | GrammarConfig + Lock 14 Legality Gate | After W0 close |
| W2 | Section 5 | `escape_mask_64` Correctness Prerequisite | After W1a close |
| W1b-1 | Section 6 | CSS L4 Generated Track 1 + Independent Oracle Scaffold | After W1a close; scalar-only unless W2 passed |
| W1b-2 | Section 7 | CSS L4 Lightningcss Comparator + Admission Gate | After W1b-1 close |
| W3 | Section 8 | CSS-Local Same-Tape Union Attempt | After W1b-2 measured CSS row plus CHALLENGE |
| W4 | Section 9 | ASM-Gen CSS Consumer + AArch64 Orphan Disposition | After W1b-2 close, W2 close, and CHALLENGE |
| W5 | Section 10 | Close And Alpha Feedback | After W0/W1a/W2/W1b-1/W1b-2/W4 and conditional W3 disposition |

## 6. W0 Dispatch

W0 telemetry/gate lock at commit `f788eb97` is revalidated, not redone. Treat
it as valid unless revalidation shows drift in the gate/report surface, source
baseline, or JSON result surface.

W0 owner paths:

- `restart/skinny/tranches/sk-v12/research/`
- `restart/skinny/tranches/sk-v12/research/p1/`
- `skinny/RESULTS.md` only if gate records unchanged state or measured
  disposition
- `skinny/REDRESS.md`

W0 does not authorize parser/scanner/SIMD/codegen behavior edits, generated
runtime output changes, or benchmark behavior changes. Dispatch W0 through the
research -> plan -> redress triumvirate with distinct commits.

## 7. Telemetry Binding

The CSS L4 gate or companion report must consume grammar id, row id, output
plane, strictness, generated source/runtime paths, grammar/input checksums,
oracle path, lightningcss comparator artifact and Mbps, strict equality,
Track 1 Mbps, oracle Mbps, sample count, sample cost, run id, host triple,
feature mask, build flags, profile artifact, benchmark artifact, Lock 14
status, Lock 16 status where applicable, generated LOC, generated module byte
size, O(N) grammar-size status, same-wave consumer class, JSON guard state,
gate status, wave id, and REDRESS id.

Producer-only fields, stale run ids, oracle coupling, grammar-name branches in
generic crates, missing lightningcss evidence, parse-only admission, and orphan
SIMD primitives fail closed.

## 8. Refusal Conditions

Refuse or return REVISE for any dispatch that:

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

This handoff authorizes W0. Downstream behavior work is authorized only by the
current `SPEC.md` wave entry gates and `DISPATCH-PROMPT.md` triumvirate
protocol.
