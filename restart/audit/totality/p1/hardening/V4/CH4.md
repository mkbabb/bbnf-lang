# SK-V15 T-P1 V4 CH4 Cost Hardening

Verdict: ACCEPT

Scope: CH4 cost/risk/wave/hard-cap review over the SK-V15 T-P1 V4 packet,
with the V4 dispatch focus on the 1D receiver cost carrier and
primitive/kernel receiver table. This replaces stale prior-cycle content in
place. No inventories, spec surfaces, source files, staging, or commits were
changed by this lens.

## Evidence

Authority checked:

- `restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` requires every
  divergence to carry realistic LOC/risk and every 1E amendment candidate to
  carry wave-alignment evidence.
- `restart/prompts/ORCHESTRATOR.md:81-88` defines CH4 COST as LOC budget,
  risk class, wave alignment, hard cap, and same-wave consumer per
  kernel/primitive.
- `restart/prompts/ORCHESTRATOR.md:110-120` requires CHALLENGE results to fold
  before the next cycle and uses two consecutive >=95% cycles for convergence.
- `restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:42-50` assigns
  `T-P1-V4-F02` and `T-P1-V4-F03` to `1D-skinny-lessons.md`.
- `restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:62-64` narrows
  this lens to whether the receiver cost carrier and primitive/kernel receiver
  table remove the V3 class-level cost gaps, with no unbounded, paper-only, or
  non-enumerated primitive route surviving.
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:28`
  records the V3 CH4 REVISE fold target;
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:38-39`
  define the required V4 receiver carrier and primitive/kernel enumeration.
- `restart/audit/totality/p1/hardening/V3/CH4.md:50-51` names the two V3 CH4
  blockers, and `restart/audit/totality/p1/hardening/V3/CH4.md:58-65` states
  the required fold.

Live packet evidence:

- `restart/audit/totality/p1/1D-skinny-lessons.md:48-51` records the V4 fold
  of CH4-V2 and CH4-V3 cost/primitive requirements.
- `restart/audit/totality/p1/1D-skinny-lessons.md:166` routes the old
  unimplemented class bucket to the V4 receiver carrier, and
  `restart/audit/totality/p1/1D-skinny-lessons.md:168` routes unknown primitive
  surfaces to the receiver carrier plus primitive/kernel table.
- `restart/audit/totality/p1/1D-skinny-lessons.md:170-184` contains
  `RC-01` through `RC-11`, each with owner path/row, LOC range, risk, wave,
  hard cap, consumer/proof, and route/revert disposition.
- `restart/audit/totality/p1/1D-skinny-lessons.md:196-217` enumerates the
  primitive/kernel rows required by the V3 fold.
- `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:24-32`
  defines scalar oracle, SIMD/ASM path, checkasm, same-wave consumer, and
  manifest/locks as mandatory admission stages;
  `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:41` states
  every source-present primitive must close as `wired`, `deleted`,
  `scalar-delegate-non-ASM`, or architecture-blocked with REDRESS, and orphan
  intrinsic/ASM files do not close Lock 16.
- `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:47-58`
  lists the P2-B source primitive/process rows that V4 must route.
- `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:31-55`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:57-75`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:90-123`,
  `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:154-217`,
  and `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223-236`
  list the P2-E parse-that vocabulary gaps and grammar-neutrality constraints.
- `restart/audit/totality/p1/1E-locks-evidence.md:195-200` independently
  requires Lock 16 gate output to classify each source-present primitive as
  wired, deleted, scalar-delegate non-ASM, or architecture-blocked with REDRESS.
- `restart/audit/totality/p1/1F-coherence-scan.md:128` extends the sidecar
  close grep guard to include FNV/hash surfaces, and
  `restart/audit/totality/p1/1F-coherence-scan.md:142` routes CSS generated FNV
  hashes to W10 quarantine rather than substrate/equality proof.

Material grep check:

- A targeted scan over the six live inventories found no residual old V3
  unbounded phrases `10,000+`, `1,500-8,000`, `per accepted primitive`, or
  `primitive same-wave consumers 80-350 LOC each`.
- The remaining `source-present primitive` language is gate/manifest language,
  not an admit route: `restart/audit/totality/p1/1D-skinny-lessons.md:177`,
  `restart/audit/totality/p1/1D-skinny-lessons.md:225`,
  `restart/audit/totality/p1/1D-skinny-lessons.md:229`;
  `restart/audit/totality/p1/1E-locks-evidence.md:200`;
  `restart/audit/totality/p1/1F-coherence-scan.md:77`.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH4-V4-001 | ACCEPT | The V4 receiver cost carrier discharges the V3 class-level 1D cost bucket. | V3 required splitting the class rows into keyed receiver rows with owner, LOC, risk, wave, hard cap, same-wave proof, and route/revert disposition (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:38`; `restart/audit/totality/p1/hardening/V3/CH4.md:50`, `restart/audit/totality/p1/hardening/V3/CH4.md:58-62`). V4 routes the former aggregate rows to `RC-01` through `RC-11` at `restart/audit/totality/p1/1D-skinny-lessons.md:166`, `restart/audit/totality/p1/1D-skinny-lessons.md:168`, and defines those receivers at `restart/audit/totality/p1/1D-skinny-lessons.md:170-184`. The required buckets are present: CSS broadcast/value/re-time (`RC-01`..`RC-03`), Lock 14/16 gates (`RC-04`), Pattern H (`RC-05`), Decision Engine (`RC-06`), codegen leaks (`RC-07`), FNV quarantine (`RC-08`), JSON c/B research (`RC-09`), parse-that vocabulary (`RC-10`), and primitive same-wave consumers (`RC-11`). |
| CH4-V4-002 | ACCEPT | No receiver row is unbounded or paper-only. | Each receiver row at `restart/audit/totality/p1/1D-skinny-lessons.md:174-184` has a bounded LOC range and hard cap plus proof/disposition text. The largest Pattern H row is no longer open-ended: `RC-05` sets generator/provenance, named projection, and close-transcript ranges plus a 3,600 LOC cap and requires explicit per-runtime owner rows when projection changes (`restart/audit/totality/p1/1D-skinny-lessons.md:178`). FNV is bounded and quarantined by production scan plus adversarial fixtures (`restart/audit/totality/p1/1D-skinny-lessons.md:181`), not accepted as an equality arbiter. JSON c/B remains docs/research unless a primitive row owns code (`restart/audit/totality/p1/1D-skinny-lessons.md:182`). |
| CH4-V4-003 | ACCEPT | The primitive/kernel receiver table enumerates every V3-required named route. | V3 required source-present primitive/kernel rows instead of class rows (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md:39`; `restart/audit/totality/p1/hardening/V3/CH4.md:51`, `restart/audit/totality/p1/hardening/V3/CH4.md:63-65`). V4 lists `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT` / `BULK_EMIT_POSITIONS_64`, `EOB_PAD_CLAMP`, `escape_mask_64`, UTF-8 validation, Unicode escape SIMD, long-string scanner, direct cursor/whitespace, tape/allocation pressure, product-builder/hash rows, and the six parse-that gaps `skip_byte_set_run`, `classify_local_block_64`, `bounded_plain_literal_span`, `validate_utf8_run`, `digit_run_span_accumulate`, and `escaped_literal_segments` at `restart/audit/totality/p1/1D-skinny-lessons.md:200-217`. |
| CH4-V4-004 | ACCEPT | Primitive rows carry the CH4 fields and block non-enumerated admit routes. | The primitive table header requires source row, owning wave, consumer/proof, LOC/risk/hard cap, and absent-consumer disposition (`restart/audit/totality/p1/1D-skinny-lessons.md:198`). Rows `restart/audit/totality/p1/1D-skinny-lessons.md:200-217` populate those fields. The table does not admit by declaration: PMULL/CSSC rows are architecture-blocked or scalar-delegate unless fresh consumer proof lands (`restart/audit/totality/p1/1D-skinny-lessons.md:202-203`); `EOB_PAD_CLAMP` is support inventory only (`restart/audit/totality/p1/1D-skinny-lessons.md:204`); `escape_mask_64`, UTF-8, Unicode escape, long-string, and parse-that rows remain research-only without consumer proof (`restart/audit/totality/p1/1D-skinny-lessons.md:205-208`, `restart/audit/totality/p1/1D-skinny-lessons.md:212-217`); product-builder/hash rows are delete or bench-only quarantine and never production equality proof (`restart/audit/totality/p1/1D-skinny-lessons.md:211`). |
| CH4-V4-005 | ACCEPT | The packet keeps primitive work as required/UNKNOWN until proof exists, which is the correct CH4 cost posture rather than paper-close. | 1D states candidate primitives are research gaps, not admits (`restart/audit/totality/p1/1D-skinny-lessons.md:117`), and records G-8/G-9 as pending with scalar oracle, strict checkasm, same-wave consumer, and grammar-neutral vocabulary requirements (`restart/audit/totality/p1/1D-skinny-lessons.md:145-146`). The Gaps table preserves Lock 14/16 restoration, parse-that vocabulary, and SIMD/ASM manifest as UNKNOWN/required inputs (`restart/audit/totality/p1/1D-skinny-lessons.md:225`, `restart/audit/totality/p1/1D-skinny-lessons.md:228-229`). P2-B says checkasm commands are necessary but not sufficient and source-present primitives must receive an explicit final disposition (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:41`). 1E's Lock 16 gate carrier repeats the same disposition requirement (`restart/audit/totality/p1/1E-locks-evidence.md:195-200`). |
| CH4-V4-006 | ACCEPT | FNV/hash and sidecar-adjacent primitive risks are cost-bounded and fenced, not laundered into a primitive close. | 1D's FNV receiver is `RC-08`, bounded at 80-220 LOC / 320 cap, with production scan plus adversarial fixtures and bench-only quarantine/delete disposition (`restart/audit/totality/p1/1D-skinny-lessons.md:181`). Product-builder/hash rows in the primitive table route to W10 quarantine and explicitly forbid production equality proof (`restart/audit/totality/p1/1D-skinny-lessons.md:211`). 1F identifies generated CSS runtime FNV hashes as hash-sidecar/telemetry coupling and says they are not CSS Value API proof, retained identity, same-substrate evidence, or a production equality arbiter (`restart/audit/totality/p1/1F-coherence-scan.md:89`, `restart/audit/totality/p1/1F-coherence-scan.md:142`, `restart/audit/totality/p1/1F-coherence-scan.md:165`). |

## Required Fold

None. The V4 receiver carrier and primitive/kernel receiver table remove the V3
CH4 class-level cost gaps. No unbounded, paper-only, or non-enumerated
primitive route survives in the live V4 packet.
