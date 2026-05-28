# SK-V15 T-P1 V5 CH4 Cost Hardening

Verdict: ACCEPT

Scope: CH4 COST review for the SK-V15 T-P1 V5 packet. The V5 dispatch focus is
whether the V4 receiver cost carrier and primitive/kernel receiver table remain
bounded, and whether V5 introduced any unbounded, paper-only, or non-enumerated
primitive route. No inventories, spec surfaces, implementation files, staging, or
commits were changed by this lens.

## Evidence

Authority checked:

- `restart/prompts/totality/PASS-1-EXCAVATION.md:121-123` requires each
  divergence to carry realistic LOC/risk and each 1E amendment candidate to
  carry wave-alignment evidence.
- `restart/prompts/ORCHESTRATOR.md:81-88` defines CH4 COST as LOC budget, risk
  class, wave alignment, hard cap, and same-wave consumer per kernel/primitive.
- `restart/prompts/ORCHESTRATOR.md:110-120` requires each CHALLENGE cycle to
  fold before advance; `restart/prompts/ORCHESTRATOR.md:125-126` makes V5 the
  hard ceiling if convergence is not reached.
- `restart/prompts/ORCHESTRATOR.md:205-206` requires scalar reference and
  checkasm parity before SIMD/ASM wiring, plus same-wave consumer so no orphan
  kernel closes.
- `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:54-61` requires
  V5 to preserve the V4 accepted surfaces: `RC-01` through `RC-11`, non-admitting
  primitive/kernel rows, FNV/hash telemetry quarantine, and Pattern H evidence
  fences.
- `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:74-76` assigns
  CH4 to verify the V4 receiver cost carrier and primitive/kernel receiver table
  remain bounded, with no unbounded, paper-only, or non-enumerated primitive
  route introduced by V5.
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:27-32`
  records V4 CH4 as ACCEPT, and
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V4-CONSOLIDATED.md:47-52`
  states the receiver carrier and primitive/kernel table preservation contract.
- `restart/audit/totality/p1/hardening/V4/CH4.md:89-94` accepted the V4 receiver
  carrier, primitive/kernel enumeration, non-admitting primitive posture, and
  FNV/hash quarantine; `restart/audit/totality/p1/hardening/V4/CH4.md:98-100`
  found no required fold.

Live packet evidence:

- `restart/audit/totality/p1/1D-skinny-lessons.md:48-51` records the CH4 V2/V3
  fold of bounded receiver classes and primitive/kernel receiver proof table.
- `restart/audit/totality/p1/1D-skinny-lessons.md:166` routes the unimplemented
  class bucket to the V4 receiver carrier, and
  `restart/audit/totality/p1/1D-skinny-lessons.md:168` routes unknown close
  surfaces to `RC-01` through `RC-11` plus the primitive/kernel table.
- `restart/audit/totality/p1/1D-skinny-lessons.md:170-184` contains the V4
  receiver cost carrier with owner path/row, LOC range, risk, wave, hard cap,
  consumer/proof, and route/revert disposition.
- `restart/audit/totality/p1/1D-skinny-lessons.md:196-217` contains the
  primitive/kernel receiver table with source row, owning wave, consumer/proof,
  LOC/risk/hard cap, and absent-consumer disposition for each named row.
- `restart/audit/totality/p1/1D-skinny-lessons.md:225-229` keeps Lock 14/16,
  parse-that vocabulary, and SIMD/ASM manifest work as UNKNOWN/required inputs,
  including the no-orphan-kernel rule.
- `restart/audit/totality/p1/1E-locks-evidence.md:105`,
  `restart/audit/totality/p1/1E-locks-evidence.md:143`, and
  `restart/audit/totality/p1/1E-locks-evidence.md:200` independently require
  strict SIMD evidence, primitive traceability, and source-present primitive
  disposition.
- `restart/audit/totality/p1/1F-coherence-scan.md:89` classifies generated CSS
  runtime FNV hashes as telemetry-only/hash-sidecar coupling, not CSS Value API
  proof, retained identity, same-substrate proof, or a production equality
  arbiter.
- `restart/audit/totality/p1/1F-coherence-scan.md:91-101` lists all seven CSS
  generated runtime FNV sites with root-resolving path:line evidence, while
  `restart/audit/totality/p1/1F-coherence-scan.md:154` and
  `restart/audit/totality/p1/1F-coherence-scan.md:177` route the issue to W10
  FNV quarantine.

V5 drift check:

- `git show --numstat --oneline af809cf27` reports the V5 fold touched only
  `1A-substrate-evidence.md`, `1B-codegen-evidence.md`, and
  `1F-coherence-scan.md`; the V5 fold did not edit `1D-skinny-lessons.md` or
  `1E-locks-evidence.md`, where the cost carrier and primitive manifest carrier
  live.
- `git diff af809cf27^ af809cf27 -- restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1F-coherence-scan.md`
  shows 1A/1B stale-cycle prose and citation expansion, plus the 1F FNV
  line-position transcript. None of those edits changes a receiver LOC range,
  hard cap, consumer/proof requirement, or primitive disposition.
- Targeted scan over the six live inventories for `10,000+`, `1,500-8,000`,
  `per accepted primitive`, `primitive same-wave consumers 80-350 LOC each`,
  `unbounded`, `paper-only`, and `non-enumerated` returned no hits.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH4-V5-001 | ACCEPT | V5 did not alter the V4 receiver cost carrier. | The V5 fold changed only 1A, 1B, and 1F; `1D-skinny-lessons.md` is the carrier for `RC-01` through `RC-11` at `restart/audit/totality/p1/1D-skinny-lessons.md:170-184`, and that file was not touched by the V5 fold. V5 context requires these rows to remain bounded at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:54-59` and makes this CH4's focus at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:74-76`. |
| CH4-V5-002 | ACCEPT | `RC-01` through `RC-11` remain bounded and non-paper. | The receiver header at `restart/audit/totality/p1/1D-skinny-lessons.md:172` requires owner, LOC range, risk, wave, hard cap, consumer/proof, and disposition. Rows `restart/audit/totality/p1/1D-skinny-lessons.md:174-184` populate those fields. The largest Pattern H row remains capped at 3,600 LOC with explicit provenance and owner-row proof (`restart/audit/totality/p1/1D-skinny-lessons.md:178`). FNV remains bounded at 80-220 LOC / 320 cap and quarantined or deleted, never an equality arbiter (`restart/audit/totality/p1/1D-skinny-lessons.md:181`). |
| CH4-V5-003 | ACCEPT | The primitive/kernel receiver table remains enumerated and non-admitting. | The table header at `restart/audit/totality/p1/1D-skinny-lessons.md:198` requires source row, owning wave, consumer/proof, LOC/risk/hard cap, and absent-consumer disposition. The named rows at `restart/audit/totality/p1/1D-skinny-lessons.md:200-217` enumerate byte-class, bitmap, EOB, escape, UTF-8, Unicode, long-string, cursor/whitespace, tape/allocation, product/hash, and parse-that routes. PMULL and CSSC remain blocked unless fresh consumer proof lands (`restart/audit/totality/p1/1D-skinny-lessons.md:202-203`); product/hash rows route to delete or bench-only quarantine (`restart/audit/totality/p1/1D-skinny-lessons.md:211`). |
| CH4-V5-004 | ACCEPT | V5's FNV transcript is evidence-only and did not create a primitive admit route. | `restart/audit/totality/p1/1F-coherence-scan.md:89` says the generated CSS runtime FNV hashes are telemetry-only unless W10 proves otherwise, and are not CSS Value API proof, retained identity, same-substrate evidence, or a production equality arbiter. The transcript at `restart/audit/totality/p1/1F-coherence-scan.md:91-101` expands root-resolving citations only. The downstream receiver remains W10 quarantine at `restart/audit/totality/p1/1F-coherence-scan.md:154` and `restart/audit/totality/p1/1F-coherence-scan.md:177`, matching `RC-08` and the product/hash row at `restart/audit/totality/p1/1D-skinny-lessons.md:181` and `restart/audit/totality/p1/1D-skinny-lessons.md:211`. |
| CH4-V5-005 | ACCEPT | V5 preserves the no-orphan-kernel / no-source-present-shortcut rule. | CH4 authority requires same-wave consumer per primitive at `restart/prompts/ORCHESTRATOR.md:86` and forbids orphan kernels at `restart/prompts/ORCHESTRATOR.md:205-206`. 1D keeps candidate primitives as research gaps, not admits (`restart/audit/totality/p1/1D-skinny-lessons.md:117`), and keeps SIMD/ASM admits UNKNOWN/required until scalar oracle, target path, checkasm, same-wave consumer, and manifest evidence exist (`restart/audit/totality/p1/1D-skinny-lessons.md:229`). 1E repeats that every source-present primitive must close as wired, deleted, scalar-delegate non-ASM, or architecture-blocked with REDRESS (`restart/audit/totality/p1/1E-locks-evidence.md:200`). |
| CH4-V5-006 | ACCEPT | No old unbounded or non-enumerated primitive language survives in the live V5 packet. | Targeted `rg` over `1A` through `1F-coherence-scan.md` found no hits for the old unbounded phrases `10,000+`, `1,500-8,000`, `per accepted primitive`, `primitive same-wave consumers 80-350 LOC each`, `unbounded`, `paper-only`, or `non-enumerated`. The remaining primitive language is bounded table or gate-manifest language at `restart/audit/totality/p1/1D-skinny-lessons.md:198-217`, `restart/audit/totality/p1/1D-skinny-lessons.md:225-229`, and `restart/audit/totality/p1/1E-locks-evidence.md:200`. |

## Required Fold

None. The V4 receiver cost carrier and primitive/kernel receiver table remain
bounded in the live V5 packet. V5 introduced no unbounded, paper-only, or
non-enumerated primitive route.
