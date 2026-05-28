# CH7 Overfit-Prune / Gate-Exclusion - SK-V15 T-P1 V5

Verdict: ACCEPT.

Scope checked: SK-V15 T-P1 V5 live inventories after fold commit `af809cf27`,
the V5 dispatch context, the V4 CH7 baseline, and the locked S-P3 V4
hardening packet. This lens treats `1F-anti-pattern.md` and
`1F-past-corpora.md` as historical auxiliaries only; `1F-coherence-scan.md` is
the live 1F authority for this cycle.

CH7 standard: the V5 citation/FNV transcript fold must not convert stale W8R
CSS proof, x86/AVX-512 diagnostics, PMULL/CSSC availability, FNV bench or
runtime hashes, retained sidecars, self-exempting gates, candidate primitives,
or generated-header-only status into close evidence. Open risks may remain
only if they are surfaced as receiver work with proof obligations.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH7-V5-001 | ACCEPT | The V5 context preserves the S-P3 CH7 posture and explicitly forbids a silent hard-ceiling lock. | `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:25-31`, `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:63-66`, `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md:86-89`; S-P3 CH7 lock posture at `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:19-29`. | None. |
| CH7-V5-002 | ACCEPT | The seven-profile FNV transcript strengthens source coverage without upgrading FNV to CSS Value API, retained identity, same-substrate proof, or equality proof. | COH-016 remains `hash-sidecar coupling / unknown` at `restart/audit/totality/p1/1F-coherence-scan.md:89`; all seven CSS runtime path:line entries are evidence rows at `restart/audit/totality/p1/1F-coherence-scan.md:91-101`; owner/receiver row still routes CSS generated FNV hashes to W10 quarantine at `restart/audit/totality/p1/1F-coherence-scan.md:154`; the gap row still says no production quarantine proof exists at `restart/audit/totality/p1/1F-coherence-scan.md:177`. | None. |
| CH7-V5-003 | ACCEPT | CSS L4 remains audit-demoted, not re-admitted through the FNV/citation fold. | CSS broadcast, string-literal generation, workload mismatch, and missing typed value API remain disproved or unimplemented at `restart/audit/totality/p1/1D-skinny-lessons.md:107-110`, `restart/audit/totality/p1/1D-skinny-lessons.md:153-159`; CSS Value API remains missing at `restart/audit/totality/p1/1F-coherence-scan.md:174`. | None. |
| CH7-V5-004 | ACCEPT | x86/AVX-512 remains diagnostic-only; PMULL/CSSC rows remain blocked without fresh consumer and strict parity proof. | Native aarch64 discipline is preserved at `restart/audit/totality/p1/1D-skinny-lessons.md:113`, `restart/audit/totality/p1/1D-skinny-lessons.md:139`; PMULL/CSSC candidate rows remain architecture-blocked or scalar-delegate at `restart/audit/totality/p1/1D-skinny-lessons.md:202-203`; Lock 16 manifest/strictness remains open at `restart/audit/totality/p1/1E-locks-evidence.md:105`, `restart/audit/totality/p1/1E-locks-evidence.md:200-221`. | None. |
| CH7-V5-005 | ACCEPT | Gate-exclusion risks remain explicit live receivers, not clean proof. | Lock 14/16 gate holes remain unimplemented at `restart/audit/totality/p1/1F-coherence-scan.md:77-78`; gate-exclusion carrier requires included roots and exclusion reporting at `restart/audit/totality/p1/1F-coherence-scan.md:110-115`; self-exempting gate risk remains open at `restart/audit/totality/p1/1F-coherence-scan.md:128-130`; 1E repeats exclusion-report and primitive-source status obligations at `restart/audit/totality/p1/1E-locks-evidence.md:195-200`. | None. |
| CH7-V5-006 | ACCEPT | Sidecars and retained substrates remain blocked or comparator-only. | Root structural sidecars and CSS comparator sidecars remain live coupling rows at `restart/audit/totality/p1/1F-coherence-scan.md:87-88`; the sidecar/hash grep guard still includes source-sidecar and hash surfaces at `restart/audit/totality/p1/1F-coherence-scan.md:128`; retained structural sidecars and streaming cursors remain blocked at `restart/audit/totality/p1/1D-skinny-lessons.md:111-112`. | None. |
| CH7-V5-007 | ACCEPT | Pattern H generated status is not closed by headers or citation repair. | Pattern H remains `67` current baseline and `0/67` generated-header state at `restart/audit/totality/p1/1E-locks-evidence.md:84`; 1C still records the root runtime count and zero generated-header state at `restart/audit/totality/p1/1C-runtime-evidence.md:123`, `restart/audit/totality/p1/1C-runtime-evidence.md:156`; 1D routes generated ownership to PRUNE-WAVE-D at `restart/audit/totality/p1/1D-skinny-lessons.md:115`, `restart/audit/totality/p1/1D-skinny-lessons.md:178`, `restart/audit/totality/p1/1D-skinny-lessons.md:226`. | None. |
| CH7-V5-008 | ACCEPT | Candidate primitives remain research gaps until scalar oracle, aarch64 or scalar-delegate implementation, strict parity, and same-wave consumer proof exist. | Primitive discipline remains pending at `restart/audit/totality/p1/1D-skinny-lessons.md:117`, `restart/audit/totality/p1/1D-skinny-lessons.md:184`, `restart/audit/totality/p1/1D-skinny-lessons.md:196-217`, `restart/audit/totality/p1/1D-skinny-lessons.md:228-229`; Lock 16 manifest evidence remains open at `restart/audit/totality/p1/1E-locks-evidence.md:143`, `restart/audit/totality/p1/1E-locks-evidence.md:200-221`. | None. |

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
receiver work, UNKNOWN/required proof, or explicit rejection. The V5 fold only
improves citation resolvability and current-source FNV coverage. It adds no
stale W8R, x86-only, PMULL/CSSC-only, FNV/hash-sidecar, retained-sidecar,
gate-exclusion, primitive-declaration, or header-only close route.

## Required Fold

None. CH7 has no V5-required fold.
