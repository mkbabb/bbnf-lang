# Handoff SK-V12

Date: 2026-05-20.

Status: CLOSED PASS-ADMIT under `G-W5-CLOSE` / REDRESS-127. Pass Alpha
SK-V11 -> SK-V12 was re-bracketed under `USER-PIN-W1-CSS-L4-SOTA.md`;
pin-aware S-P1 Profile, S-P2 Research, S-P3 Synthesis-Plan, and W0/W1a/W2/
W1b-1/W1b-2a/W1b-2b/W4/W5 have closed. No further SK-V12 wave dispatch is
authorized.

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

SK-V12 close result:

| Family | State | SK-V12 role |
|---|---|---|
| `parse_only` | diagnostic-only concession | no SOTA admission target |
| `direct_to_struct` | JSON guard plus routed ledger | guard second after CSS L4 |
| `real_typed_struct` | JSON typed guard surface | guard second after CSS L4 |
| CSS L4 generated parser | `A / GO` via `css_l4/declaration_values/direct_to_struct/main` | admitted close row |
| Sheets / BBNF-self | no admitted row | fallback history only; not needed for close |
| Overall | `A / Go` | PASS-ADMIT close |

The admitted CSS row records generated Track 1 `429.34420791225705 Mbps`,
lightningcss threshold `169.92962215656692 Mbps`, strict equality
`pass:track1=cssparser=lightningcss`, JSON guards held, and W4 final orphan
count zero. REDRESS 96/97/98 and 88/89/90 remain measured historical
implementations; the categories are still unblocked for future materially
differentiated attempts, but no fresh union attempt is required for this ADMIT
close.

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

SK-V12 closed by ADMIT, not FIXPOINT.

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

Close evidence:

- CSS L4 row: `css_l4/declaration_values/direct_to_struct/main`;
- Track 1: `429.34420791225705 Mbps`;
- lightningcss: `168.92962215656692 Mbps`;
- threshold: `169.92962215656692 Mbps`;
- margin: `259.41458575569015 Mbps`;
- REDRESS: 125 for the report gate, 126 for W4 zero-orphan/ASM-gen route,
  127 for W5 close reconciliation;
- campaign close file:
  `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`.

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
| W0 | Section 3 | Pin Telemetry And Gate Revalidation | Closed |
| W1a | Section 4 | GrammarConfig + Lock 14 Legality Gate | Closed REDRESS-121 |
| W2 | Section 5 | `escape_mask_64` Correctness Prerequisite | Closed REDRESS-122 |
| W1b-1 | Section 6 | CSS L4 Generated Track 1 + Independent Oracle Scaffold | Closed REDRESS-123 |
| W1b-2 | Section 7 | CSS L4 Lightningcss Comparator + Admission Gate | Closed REDRESS-124 / REDRESS-125 |
| W3 | Section 8 | CSS-Local Same-Tape Union Attempt | Not required for ADMIT close |
| W4 | Section 9 | ASM-Gen CSS Consumer + AArch64 Orphan Disposition | Closed REDRESS-126 |
| W5 | Section 10 | Close And Alpha Feedback | Closed PASS-ADMIT REDRESS-127 |

All SK-V12 wave dispatch is now historical. W3 is not required because W5
closed by ADMIT on an already-admitted CSS path; W4 supplied zero-orphan and
ASM-gen route evidence before close.

## 6. Historical W0 Dispatch

W0 telemetry/gate lock at commit `f788eb97` was revalidated, not redone.

W0 owner paths:

- `restart/skinny/tranches/sk-v12/research/`
- `restart/skinny/tranches/sk-v12/research/p1/`
- `skinny/RESULTS.md` only if gate records unchanged state or measured
  disposition
- `skinny/REDRESS.md`

W0 did not authorize parser/scanner/SIMD/codegen behavior edits, generated
runtime output changes, or benchmark behavior changes.

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

This handoff no longer authorizes downstream SK-V12 behavior work. The next
campaign action is external to SK-V12 unless a future user pin opens a new
tranche from the recorded routed remainder.
