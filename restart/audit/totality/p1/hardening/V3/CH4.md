# SK-V15 T-P1 V3 CH4 Cost Hardening

Verdict: REVISE

Score: 82/100

Scope: CH4 cost/risk/wave/hard-cap review over the SK-V15 T-P1 V3 packet. This
file replaces stale prior-cycle CH4 content in place. No source files,
inventories, staging, or commits were changed by this lens.

## Evidence

Authority checked:

- `restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` requires realistic
  LOC-delta/risk for every divergence and wave-alignment evidence for amendment
  candidates.
- `restart/prompts/ORCHESTRATOR.md:86` additionally requires LOC budget, risk
  class, wave alignment, hard cap, and same-wave consumer per kernel/primitive.
- `restart/audit/totality/p1/hardening/V3/CHALLENGE-CONTEXT.md:61-63` narrows
  this V3 CH4 lens to carrier keying and bounded receiver splits.
- `restart/audit/totality/p1/hardening/V2/CH4.md:17-20` and
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V2-CONSOLIDATED.md:40-42`
  define the V3 fold targets: re-key 1E carriers, add LAC wave/cost alignment,
  split broad buckets, and add primitive/kernel receiver proof rows.

Material commands run:

```bash
rg -n "^\\| D-1E-V1-" restart/audit/totality/p1/1E-locks-evidence.md
rg -n "^\\| LAC-1E-" restart/audit/totality/p1/1E-locks-evidence.md
rg -n "10,000\\+|1,500-8,000|per receiver|TBD|unbounded|paper-only|paper only|later wave|future phase|\\bper accepted primitive\\b|primitive same-wave consumers 80-350 LOC each|plus per-runtime projection subwaves" restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1C-runtime-evidence.md restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md
nl -ba restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md | sed -n '24,58p'
nl -ba restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md | sed -n '27,38p;229,234p'
```

The old literal unbounded shapes `10,000+`, `1,500-8,000`, and `per receiver`
are gone from the six live inventories. The same grep still finds residual
class-level cost text at `1D-skinny-lessons.md:164` and `1D-skinny-lessons.md:166`:
`plus per-runtime projection subwaves`, `per accepted primitive`, and
`primitive same-wave consumers 80-350 LOC each`.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH4-V3-001 | ACCEPT | 1E's divergence carrier is now keyed to the same divergence IDs it budgets. | The divergence rows `D-1E-V1-01` through `D-1E-V1-14` are defined at `1E-locks-evidence.md:110-123`; the V3 cost/wave carrier repeats the same IDs at `1E-locks-evidence.md:149-162` with LOC, risk, wave hint, hard cap, and matching evidence notes. The V2 mismatch from `V2/CH4.md:17` is not present. | None for 1E divergence keying. |
| CH4-V3-002 | ACCEPT | Every 1E LAC candidate now has a keyed wave/cost carrier. | LAC candidates run `LAC-1E-V1-01` through `LAC-1E-V2-15` at `1E-locks-evidence.md:129-143`; the V3 LAC carrier repeats every LAC at `1E-locks-evidence.md:168-182` with LOC, risk, wave hint, hard cap, and evidence note. This satisfies `T-P1-V3-F07` and the LAC half of `T-P1-V3-F09`. | None for 1E LAC alignment. |
| CH4-V3-003 | ACCEPT | The largest 1C Pattern H bucket is no longer open-ended. | `1C-runtime-evidence.md:123` budgets Pattern H as `1,500-3,000 generator/provenance gate + seven 700-1,200 runtime projection subwaves + 600-1,200 close transcript`; `1C-runtime-evidence.md:130` explicitly records the CH4-V2-F08 split. This is bounded enough for 1C's runtime receiver. | Preserve this split in any later fold. |
| CH4-V3-004 | REVISE | 1D still carries broad implementation buckets without per-bucket wave/hard-cap receivers. The text is improved from V2, but it remains a class-level cost envelope rather than a cap-valid implementation route. | `1D-skinny-lessons.md:164` groups ten unimplemented claims under one divergence row and budgets several subfamilies, including Pattern H `plus per-runtime projection subwaves`, but does not bind each subfamily to a hard cap, owner path, exit gate, or route/revert row. `1D-skinny-lessons.md:166` repeats the problem for eight unknown surfaces and keeps `parse-that vocabulary 250-700 LOC per accepted primitive` plus `primitive same-wave consumers 80-350 LOC each`. V2 explicitly required avoiding aggregate or "per receiver" style rows without enumerating receivers (`V2/CH4.md:19`; consolidated fold `HARDENING-T-P1-V2-CONSOLIDATED.md:41`). | Split `1D` rows 164 and 166 into bounded receiver rows or add an adjacent carrier table keyed to each sub-bucket: owner path/row, LOC range, risk, wave, hard cap, same-wave consumer/proof, and route/revert disposition. Remove open `per accepted primitive` / `each` language unless the accepted primitives are enumerated. |
| CH4-V3-005 | REVISE | The primitive/kernel receiver table is still class-level, not per source-present primitive/kernel. | `1D-skinny-lessons.md:180-186` adds five receiver classes, but V2 required a table for each source-present primitive/kernel (`V2/CH4.md:20`; consolidated fold `HARDENING-T-P1-V2-CONSOLIDATED.md:42`). The source research already names concrete candidates and process units: `p2b-dav1d-process.md:47-58` lists `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT` + `BULK_EMIT_POSITIONS_64`, `EOB_PAD_CLAMP`, `escape_mask_64`, UTF-8 validation, unicode escape SIMD, long-string scanner, direct cursor/whitespace, tape/allocation pressure, and rejected product-builder/hash rows. `p2e-parse-that-gaps.md:229-234` separately lists `skip_byte_set_run`, `classify_local_block_64`, `bounded_plain_literal_span`, `validate_utf8_run`, `digit_run_span_accumulate`, and `escaped_literal_segments`. V3 does not map those named rows to LOC/risk/wave/hard-cap/consumer dispositions. | Add a primitive/kernel receiver table in `1D`, `1E`, or `1F` that enumerates each source-present or shortlisted primitive by name. Each row must state wave, consumer path/row, proof command, LOC range, risk, hard cap, and final absent-consumer disposition (`wired`, `deleted`, `scalar-delegate-non-ASM`, `architecture-blocked-with-REDRESS`, or `research-only/rejected`). |

## Required Fold

V3 cannot be accepted under CH4 until the remaining class-level cost carriers are
made receiver-bounded:

1. Split `1D-skinny-lessons.md:164` and `1D-skinny-lessons.md:166` into keyed
   rows or a keyed carrier table. The receiver keys should cover CSS
   broadcast/value, Lock 14/16 gates, Pattern H, Decision Engine, codegen leaks,
   FNV quarantine, JSON c/B research, CSS typed API/re-timing, parse-that
   vocabulary, and primitive same-wave consumers.
2. Enumerate primitive/kernel rows from the existing P2-B and P2-E source lists
   instead of using only class rows. For each named primitive, state LOC, risk,
   wave, hard cap, consumer/proof, and absent-consumer disposition.
3. Keep the 1E V3 carrier structure as-is; it fixes the V2 keying defects and
   should not be reopened except to cross-link any new receiver rows.

CH4 can move to ACCEPT once those two 1D receiver gaps are folded. Until then,
I cannot confirm that no unbounded or paper-only cost bucket survived V3.
