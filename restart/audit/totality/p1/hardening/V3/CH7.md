# CH7 Overfit-Prune / Gate-Exclusion - SK-V15 T-P1 V3

Verdict: ACCEPT.

Scope checked: SK-V15 T-P1 V3 live inventories after fold commit
`7e32eddaa`, V2 CH7, the SK-V15 S-P3 V4 lock packet, and active SK-V15
SYNTHESIS/SPEC/DISPATCH guard language. This review treats
`1F-anti-pattern.md` and `1F-past-corpora.md` as historical auxiliaries only;
`1F-coherence-scan.md` is the live 1F authority for V3.

CH7 standard: the packet must not convert stale W8R CSS proof, x86/AVX-512
diagnostics, PMULL/CSSC ISA availability, FNV bench scaffolding, sidecars,
self-exempting gates, or generated-header-only status into close evidence.
Open risks may remain only if they are surfaced as receiver work with a
proof obligation.

## Findings

| id | disposition | finding | evidence | required fold |
|---|---|---|---|---|
| CH7-V3-001 | ACCEPT | S-P3 gate-exclusion discipline propagates into T-P1. The V3 packet carries `NEW-CH7-V5-03` through the close condition and explicitly requires exclusion reports for Lock 14 / Lock 16 gates. | `restart/skinny/tranches/sk-v15/SYNTHESIS.md:44`, `:75`, `:109-110`, `:121-127`; `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:31`. | None. Preserve the PRUNE-WAVE-B receiver. |
| CH7-V3-002 | ACCEPT | The live T-P1 inventories do not paper-close gate exclusions. They name omitted roots and self-exempting gate paths as live risk, then require included-root and exclusion-report proof. | `restart/audit/totality/p1/1E-locks-evidence.md:116`, `:139`, `:195-198`; `restart/audit/totality/p1/1F-coherence-scan.md:75`, `:97-101`, `:139`, `:157`; A3 evidence at `restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md:29`, `:38`, `:104`. | None. |
| CH7-V3-003 | ACCEPT | W8R CSS evidence is fenced as diagnostic or negative-fixture input, not admitted proof. The T-P1 packet carries anti-broadcast telemetry and collapse requirements rather than laundering old CSS metrics. | `restart/audit/totality/p1/1D-skinny-lessons.md:175`, `:193`; `restart/audit/totality/p1/1F-coherence-scan.md:101`, `:146`; `restart/skinny/tranches/sk-v15/SPEC.md:175`, `:194`, `:259`, `:277`; S-P3 V4 confirms W8R positive proof remains blocked at `restart/skinny/tranches/sk-v15/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md:31`. | None. |
| CH7-V3-004 | ACCEPT | x86 and AVX-512 remain diagnostic only; PMULL and CSSC production promotion remain blocked without fresh consumer proof. | `restart/audit/totality/p1/1D-skinny-lessons.md:111`, `:137`; `restart/audit/totality/p1/1B-codegen-evidence.md:46`, `:63`, `:101`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:43`; `restart/skinny/tranches/sk-v15/SPEC.md:136`, `:480`; `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:50`, `:290-291`. | None. |
| CH7-V3-005 | ACCEPT | FNV closed-enum products remain bench-only pending a production guard; the packet does not turn them into equality or runtime proof. | `restart/audit/totality/p1/1D-skinny-lessons.md:102`, `:127`, `:186`, `:199`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:48`; `restart/audit/totality/p1/1F-coherence-scan.md:81-82`. | None. |
| CH7-V3-006 | ACCEPT | Sidecar and EventTape risks are fenced rather than reopened. Root structural sidecars and CSS comparator sidecars are recorded as live coupling/open proof rows; EventTape work is constrained to generated same-substrate consumers. | `restart/audit/totality/p1/1A-substrate-evidence.md:75-76`, `:80`, `:92`, `:94`; `restart/audit/totality/p1/1B-codegen-evidence.md:69`; `restart/audit/totality/p1/1C-runtime-evidence.md:64`, `:101-102`; `restart/audit/totality/p1/1F-coherence-scan.md:85-86`, `:125`, `:168`. | None. CH5/substrate close must still re-run the sidecar grep before any close claim. |
| CH7-V3-007 | ACCEPT | Pattern H generated status is not paper-closed by headers. V3 records 67 root runtime files and 0 generated headers, then routes closure to PRUNE-WAVE-D with delete/regenerate or check proof. | `restart/audit/totality/p1/1C-runtime-evidence.md:123`, `:137`, `:139`, `:154`; `restart/audit/totality/p1/1D-skinny-lessons.md:113`; `restart/audit/totality/p1/1F-coherence-scan.md:77`, `:112`, `:137`, `:149`; `restart/skinny/tranches/sk-v15/SYNTHESIS.md:77`. | None. |

## Executed Checks

```sh
rg -n "(Lock 14|Lock 16|exclu|W8R|x86|AVX|PMULL|CSSC|sidecar|header|FNV|broadcast|UNKNOWN)" restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1C-runtime-evidence.md restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md
rg -n "W8R" restart/audit/totality/p1 restart/skinny/tranches/sk-v15
rg -n "PMULL|CSSC|x86|AVX|AVX-512|AVX512" restart/audit/totality/p1 restart/skinny/tranches/sk-v15
rg -n "exclusion|self-exempt|gate_exclusion|excluded roots|exclusions" restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md restart/skinny/tranches/sk-v15/SYNTHESIS.md restart/skinny/tranches/sk-v15/audit-overfit/sk-v15-audit-overfit-A3-lock14-lock16-generic-scan.md
```

Observed: the V3 inventories report the known overfit/gate-exclusion classes
as open receivers, not as clean close evidence. No CH7-specific REVISE remains.

## Orphan-REVISE Check

No orphan CH7 REVISE remains from V2. The V3 fold preserves the CH7 blocker
taxonomy and carries the S-P3 V4 gate-exclusion posture into T-P1 without
introducing a new self-exempting, stale-W8R, x86-only, PMULL/CSSC-only,
sidecar, FNV, or header-only close route.
