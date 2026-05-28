# CH7 Overfit-Prune / Gate-Exclusion - SK-V15 T-P1 V4

Verdict: ACCEPT.

Scope checked: SK-V15 T-P1 V4 live inventories after fold commit `0c79c2b43`,
the V4 dispatch context, the V3 CH7 baseline, and the locked S-P3 V4
hardening packet. This lens treats `1F-anti-pattern.md` and
`1F-past-corpora.md` as historical auxiliaries only; `1F-coherence-scan.md` is
the live 1F authority for this cycle.

CH7 standard: the packet must not convert stale W8R CSS proof, x86/AVX-512
diagnostics, PMULL/CSSC availability, FNV bench or runtime hashes, retained
sidecars, self-exempting gates, candidate primitives, or generated-header-only
status into close evidence. Open risks may remain only if they are surfaced as
receiver work with proof obligations.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH7-V4-001 | ACCEPT | The S-P3 CH7 posture still propagates into T-P1. S-P3 V4 locked only after CH7 confirmed W8R positive proof, x86/AVX-512 anchors, PMULL/CSSC-only promotion, retained sidecars, public `UnionTape`, density tables, second tapes, and stale numeric/digit routes remain blocked. | `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:19-29`; V4 dispatch context at `restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:47-65`. | None. |
| CH7-V4-002 | ACCEPT | CSS L4 remains audit-demoted. The packet records the broadcast admit, string-literal generator, workload mismatch, and missing typed value API as disproved or unimplemented, not as evidence for SOTA close. | `restart/audit/totality/p1/1D-skinny-lessons.md:108-110`, `restart/audit/totality/p1/1D-skinny-lessons.md:153-159`; `restart/audit/totality/p1/1E-locks-evidence.md:111-113`; `restart/audit/totality/p1/1F-coherence-scan.md:75`, `restart/audit/totality/p1/1F-coherence-scan.md:82`, `restart/audit/totality/p1/1F-coherence-scan.md:150-155`, `restart/audit/totality/p1/1F-coherence-scan.md:162`. | None. |
| CH7-V4-003 | ACCEPT | Gate-exclusion risks are not paper-closed. The V4 fold records Lock 14/16 gate holes, self-exempting gate risk, explicit exclusion reporting, and primitive manifest requirements. | `restart/audit/totality/p1/1D-skinny-lessons.md:114`, `restart/audit/totality/p1/1D-skinny-lessons.md:177`, `restart/audit/totality/p1/1D-skinny-lessons.md:225`, `restart/audit/totality/p1/1D-skinny-lessons.md:229`; `restart/audit/totality/p1/1E-locks-evidence.md:103`, `restart/audit/totality/p1/1E-locks-evidence.md:117`, `restart/audit/totality/p1/1E-locks-evidence.md:140`, `restart/audit/totality/p1/1E-locks-evidence.md:195-200`; `restart/audit/totality/p1/1F-coherence-scan.md:77-78`, `restart/audit/totality/p1/1F-coherence-scan.md:98-104`, `restart/audit/totality/p1/1F-coherence-scan.md:128-130`, `restart/audit/totality/p1/1F-coherence-scan.md:143`, `restart/audit/totality/p1/1F-coherence-scan.md:161`. | None. |
| CH7-V4-004 | ACCEPT | x86/AVX-512 remains diagnostic-only, while PMULL/CSSC candidate rows are blocked unless a fresh consumer and strict parity proof land. | `restart/audit/totality/p1/1D-skinny-lessons.md:113`, `restart/audit/totality/p1/1D-skinny-lessons.md:139`, `restart/audit/totality/p1/1D-skinny-lessons.md:145`, `restart/audit/totality/p1/1D-skinny-lessons.md:200-203`; `restart/audit/totality/p1/1B-codegen-evidence.md:47`, `restart/audit/totality/p1/1B-codegen-evidence.md:64`, `restart/audit/totality/p1/1B-codegen-evidence.md:103`; `restart/audit/totality/p1/1E-locks-evidence.md:105`, `restart/audit/totality/p1/1E-locks-evidence.md:143`, `restart/audit/totality/p1/1E-locks-evidence.md:200-221`. | None. |
| CH7-V4-005 | ACCEPT | FNV and hash surfaces are fenced as bench-only, telemetry-only, or unknown pending W10 quarantine. They are not credited as CSS Value API proof, retained identity, same-substrate evidence, or production equality arbiters. | `restart/audit/totality/p1/1D-skinny-lessons.md:129`, `restart/audit/totality/p1/1D-skinny-lessons.md:181`, `restart/audit/totality/p1/1D-skinny-lessons.md:211`, `restart/audit/totality/p1/1D-skinny-lessons.md:230`; `restart/audit/totality/p1/1F-coherence-scan.md:89`, `restart/audit/totality/p1/1F-coherence-scan.md:128`, `restart/audit/totality/p1/1F-coherence-scan.md:142`, `restart/audit/totality/p1/1F-coherence-scan.md:165`. | None. |
| CH7-V4-006 | ACCEPT | Sidecars and retained substrates remain blocked or comparator-only. The V4 packet distinguishes same-call masks/tape projection from retained structural sidecars, CSS comparator sidecars, and source-sidecar/hash telemetry. | `restart/audit/totality/p1/1D-skinny-lessons.md:111-112`, `restart/audit/totality/p1/1D-skinny-lessons.md:144`, `restart/audit/totality/p1/1D-skinny-lessons.md:194`, `restart/audit/totality/p1/1D-skinny-lessons.md:208-210`; `restart/audit/totality/p1/1F-coherence-scan.md:87-88`, `restart/audit/totality/p1/1F-coherence-scan.md:128`, `restart/audit/totality/p1/1F-coherence-scan.md:140-142`, `restart/audit/totality/p1/1F-coherence-scan.md:173`. | None. |
| CH7-V4-007 | ACCEPT | Pattern H generated status is not closed by headers. The packet preserves the `67` current-count invariant and `0/67` generated-header state, then routes closure to PRUNE-WAVE-D with generated ownership and round-trip proof. | `restart/audit/totality/p1/1C-runtime-evidence.md:123`, `restart/audit/totality/p1/1C-runtime-evidence.md:131-141`, `restart/audit/totality/p1/1C-runtime-evidence.md:156`; `restart/audit/totality/p1/1D-skinny-lessons.md:115`, `restart/audit/totality/p1/1D-skinny-lessons.md:178`, `restart/audit/totality/p1/1D-skinny-lessons.md:226`; `restart/audit/totality/p1/1E-locks-evidence.md:84`, `restart/audit/totality/p1/1E-locks-evidence.md:116`, `restart/audit/totality/p1/1E-locks-evidence.md:134`, `restart/audit/totality/p1/1E-locks-evidence.md:141`, `restart/audit/totality/p1/1E-locks-evidence.md:173-180`; `restart/audit/totality/p1/1F-coherence-scan.md:79`, `restart/audit/totality/p1/1F-coherence-scan.md:140`, `restart/audit/totality/p1/1F-coherence-scan.md:153`, `restart/audit/totality/p1/1F-coherence-scan.md:163`. | None. |
| CH7-V4-008 | ACCEPT | Candidate primitives are treated as research gaps until scalar oracle, aarch64 or scalar-delegate implementation, strict parity, and same-wave consumer proof exist. No primitive row is admitted by declaration alone. | `restart/audit/totality/p1/1D-skinny-lessons.md:117`, `restart/audit/totality/p1/1D-skinny-lessons.md:184`, `restart/audit/totality/p1/1D-skinny-lessons.md:196-217`, `restart/audit/totality/p1/1D-skinny-lessons.md:228-229`; `restart/audit/totality/p1/1E-locks-evidence.md:105`, `restart/audit/totality/p1/1E-locks-evidence.md:143`, `restart/audit/totality/p1/1E-locks-evidence.md:200`, `restart/audit/totality/p1/1E-locks-evidence.md:221`. | None. |

## Executed Checks

```sh
rg -n "W8R|x86|PMULL|CSSC|FNV|fnv|sidecar|gate-exclusion|gate exclusion|header-only|header only|primitive|generated header|@generated|UNKNOWN|unimplemented|partial|honoured|honored|Implemented" \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md

rg -n "x86|PMULL|CSSC|NEON|aarch64|M5|Apple|simd|SIMD" \
  restart/audit/totality/p1/1A-substrate-evidence.md \
  restart/audit/totality/p1/1B-codegen-evidence.md \
  restart/audit/totality/p1/1C-runtime-evidence.md \
  restart/audit/totality/p1/1D-skinny-lessons.md \
  restart/audit/totality/p1/1E-locks-evidence.md \
  restart/audit/totality/p1/1F-coherence-scan.md
```

Observed: all CH7-sensitive classes are carried as diagnostic history, open
receiver work, UNKNOWN/required proof, or explicit rejection. No stale W8R,
x86-only, PMULL/CSSC-only, FNV/hash-sidecar, retained-sidecar, gate-exclusion,
primitive-declaration, or header-only close route is introduced by the V4 fold.

## Orphan-REVISE Check

No CH7 orphan REVISE remains from V3. V4 preserves the S-P3 gate-exclusion
posture and adds no new CH7 fold requirement.
