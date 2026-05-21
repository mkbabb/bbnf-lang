---
agent: 3C
pass: T-P3-synthesis
cycle: V1
generated_at: 2026-05-21T19:14:19Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: LOCKS.md
proposed_deltas_count: 12
delta_summary:
  carried_from_prior_cycle: []
  removed: []
  answered: [LAC-1E-01, LAC-1E-02, LAC-1E-03, LAC-1E-04, LAC-1E-05, LAC-1E-06, LAC-1E-07, LAC-1E-08, LAC-1E-09, LAC-1E-10, LAC-1E-11, T2A-LAC-01, T2A-LAC-02, T2A-LAC-03, T2A-LAC-04, T2A-LAC-05, LAC-2B-01, LAC-2B-02, LAC-2B-03, LAC-2B-04, LAC-2B-05, LAC-2B-06, LAC-2B-07, LAC-2C-01, LAC-2C-02, LAC-2C-03, LAC-2C-04, LAC-2C-05, LAC-2D-01, LAC-2D-02, LAC-2D-03, LAC-2D-04, LAC-2D-05, LAC-2E-01, LAC-2E-02, LAC-2E-03, LAC-2E-04, LAC-2F-01, LAC-2F-02, LAC-2F-03, LAC-2F-04]
  newly_added: [3C-L01-substrate-ceiling-history, 3C-L08-non-json-bench-feed, 3C-L14-per-wave-gate, 3C-L16-checkasm-escape-mask, 3C-GOMEGA-boundaries]
prior_cycle_dispositions_folded:
  accepted: [G-T-P1-EXCAVATION-CONVERGED, G-T-P2-RESEARCH-CONVERGED]
  rejected: []
  revised: []
---

## Executive Summary

3C consolidates 41 LOCKS amendment candidates into 12 proposed v+1 hunks. The diff preserves the 16-lock numbering, supersedes the stale SK-V9 allowance without erasing it as history, and adds no directive, BIR variant, `BackendShape`, public substrate API, or new lock. The main changes are: Lock 1 gets substrate-ceiling history, fact-stream/output-plane fencing, and material-differential rules for union reopen; Lock 8 gets row-plane accounting plus the non-JSON telemetry feed into `BENCH.md` Section 8; Lock 10 gets active cost evidence and regex/HIR fact requirements without expanding the five-shape canon; Lock 14 gets generated-output allowance plus per-wave name/shape leak gates; Lock 16 gets strict checkasm, `escape_mask_64` prerequisite-only wording, primitive manifests, zero-orphan close states, and hardware-gate clarification. G3/G-Omega boundaries remain explicit: T-P3 proposes only, Pass Omega edits only after G-Omega. Evidence: `PASS-3-SYNTHESIS.md:21`-`24`, `PASS-3-SYNTHESIS.md:189`-`198`, `PASS-3-SYNTHESIS.md:210`, `restart/HANDOFF.md:44`-`47`.

## V1 Delta Summary

| delta id | affected LOCKS.md line(s) | disposition summary | source candidates |
|---|---|---|---|
| 3C-L01-substrate-ceiling-history | `restart/locks/LOCKS.md:52` | Modify Lock 1 with scoped JSON lazy-offset evidence, Track 2 substrate-ceiling history, fact-stream/output-plane fencing, transient-mask rules, and REDRESS 96/97/98 material-differential constraints. | LAC-1E-01, T2A-LAC-01, LAC-2B-05, LAC-2B-06, LAC-2D-05, LAC-2E-04 |
| 3C-L02-layout-live-first | `restart/locks/LOCKS.md:54` | Accept live `LayoutFacts.backend_shape` first; retain `Layout` / `LayoutSink` as V1 API-freeze obligations or remove them in Pass Omega. | LAC-1E-02 |
| 3C-L03-path-cursor-proof | `restart/locks/LOCKS.md:56` | Add a verification clause for empty-path cursor elision instead of claiming closure from absent evidence. | LAC-1E-03 |
| 3C-L08-row-plane-bench-feed | `restart/locks/LOCKS.md:66` | Replace blanket SOTA language with row-plane accounting, same-plane comparator provenance, and non-JSON telemetry feed through `BENCH.md` Section 8 or dedicated companion gates. | LAC-1E-04, T2A-LAC-02, LAC-2F-04 |
| 3C-L09-runtime-api-obligations | `restart/locks/LOCKS.md:68` | State that `parse_in(input, &bump)` and generated owned documents remain V1 obligations, not skinny closure. | LAC-1E-05 |
| 3C-L10-decision-engine-cost | `restart/locks/LOCKS.md:70` | Preserve the five-shape canon but require active cost evidence, generated grammar facts, regex/HIR facts where relevant, and fail-closed abrogate gates. | T2A-LAC-05, LAC-2C-04, LAC-2D-01, LAC-2D-02, LAC-2D-03, LAC-2F-03 |
| 3C-L11-L12-workspace-drift | `restart/locks/LOCKS.md:72`-`74` | Distinguish root legacy workspace drift from skinny truth and require A.W0/A.W1 archive/removal proof. | LAC-1E-06 |
| 3C-L13-loc-exceptions | `restart/locks/LOCKS.md:76` | Allow generated and gate/report files only under explicit budgets and transcripts; keep 500 LOC for non-generated production files. | LAC-1E-07 |
| 3C-L14-generated-output-and-per-wave-gate | `restart/locks/LOCKS.md:78` plus superseded allowance block `restart/locks/LOCKS.md:1`-`17` | Add generated-output allowance, generated provider manifest, grammar-name plus grammar-shape leak census, per-wave gate enforcement, and CSS/Sheets/BBNF-self transfer proof. | LAC-1E-08, LAC-1E-11, T2A-LAC-04, LAC-2B-03, LAC-2C-01, LAC-2C-02, LAC-2C-03, LAC-2C-05, LAC-2F-01 |
| 3C-L15-profile-scope | `restart/locks/LOCKS.md:80`-`85` | Split skinny enforcement from root thin-LTO drift and keep JSON i-cache evidence scoped. | LAC-1E-09 |
| 3C-L16-manifest-checkasm-orphans | `restart/locks/LOCKS.md:87`-`112` | Add primitive manifest, strict checkasm, `escape_mask_64` prerequisite-only state, grammar-policy gate, zero-orphan states, `svmatch_u8` hardware clarification, and same-wave consumer rule. | LAC-1E-10, T2A-LAC-03, LAC-2B-01, LAC-2B-02, LAC-2B-04, LAC-2B-07, LAC-2D-04, LAC-2E-01, LAC-2E-02, LAC-2E-03, LAC-2F-02 |
| 3C-GOMEGA-boundaries | document preamble / no lock renumbering | Add governance note in the proposed diff only: actual edits are Pass Omega CRUD after G3 and G-Omega; new locks or shape/substrate expansions are user-gated. | PASS-3 / HANDOFF / T-P2 convergence evidence |

## Disposition Counts

| disposition | count |
|---|---:|
| ACCEPT | 30 |
| MODIFY | 11 |
| REJECT | 0 |
| DEFER | 0 |
| Total | 41 |

## Disposition Matrix

| candidate | proposer | affected lock(s) | disposition | evidence path:line | rationale / v+1 routing |
|---|---|---|---|---|---|
| LAC-1E-01 | 1E | Lock 1 | MODIFY | `restart/audit/totality/p1/1E-locks-evidence.md:100`; `skinny/REDRESS.md:246`-`256`; `restart/skinny/BENCH.md:121`-`136`; `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:45`-`46` | Fold into Lock 1 as scoped JSON lazy-offset and substrate-ceiling evidence, not as universal Lock 1 closure. |
| LAC-1E-02 | 1E | Lock 2 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:101`; `restart/locks/LOCKS.md:54`; `restart/audit/totality/p1/1E-locks-evidence.md:64` | Add live-first wording for `LayoutFacts.backend_shape` while keeping `Layout` / `LayoutSink` as API-freeze obligations. |
| LAC-1E-03 | 1E | Lock 3 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:102`; `restart/locks/LOCKS.md:56`; `restart/audit/totality/p1/1E-locks-evidence.md:65`; `restart/audit/totality/p1/1E-locks-evidence.md:125` | Add an executable verification requirement for empty-path elision; no closure claim is made. |
| LAC-1E-04 | 1E | Lock 8 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:103`; `skinny/RESULTS.md:5`-`35`; `skinny/RESULTS.md:94`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`-`110` | Fold into row-plane accounting: JSON parse/direct/typed and CSS fact-stream rows stay separate. |
| LAC-1E-05 | 1E | Lock 9 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:104`; `restart/locks/LOCKS.md:68`; `restart/audit/totality/p1/1E-locks-evidence.md:71` | Add skinny-scoped caveat that bump arena and generated owned documents remain V1 obligations. |
| LAC-1E-06 | 1E | Locks 11, 12 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:105`; `restart/locks/LOCKS.md:72`-`74`; `restart/audit/totality/p1/1E-locks-evidence.md:73`-`74` | Fold root workspace drift into Locks 11/12 verification without weakening archive requirements. |
| LAC-1E-07 | 1E | Lock 13 | MODIFY | `restart/audit/totality/p1/1E-locks-evidence.md:106`; `restart/locks/LOCKS.md:76`; `skinny/REDRESS.md:299`-`312`; `restart/audit/totality/p1/1F-anti-pattern.md:31` | Accept exceptions only for generated and gate/report files with budgets and transcripts; keep non-generated production ceiling. |
| LAC-1E-08 | 1E | Lock 14 | MODIFY | `restart/audit/totality/p1/1E-locks-evidence.md:107`; `restart/locks/LOCKS.md:78`; `skinny/RESULTS.md:94`; `skinny/REDRESS.md:3824`-`3840`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65` | Codify generated non-JSON allowance but add per-wave gate and grammar-shape leak criteria. |
| LAC-1E-09 | 1E | Lock 15 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:108`; `restart/locks/LOCKS.md:80`-`85`; `skinny/REDRESS.md:258`-`264`; `restart/audit/totality/p1/1E-locks-evidence.md:77` | Split skinny fat-LTO enforcement from root thin-LTO drift and keep i-cache evidence scoped. |
| LAC-1E-10 | 1E | Lock 16 | ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:109`; `restart/locks/LOCKS.md:112`; `skinny/REDRESS.md:3603`-`3632`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101` | Add manifest and consumer requirements; Lock 16 remains partial until traceability is gate-consumed. |
| LAC-1E-11 | 1E | Scoped SK-V9 allowance, Lock 14 | MODIFY | `restart/audit/totality/p1/1E-locks-evidence.md:110`; `restart/locks/LOCKS.md:1`-`17`; `restart/locks/LOCKS.md:78`; `skinny/RESULTS.md:94` | Supersede the SK-V9 block as historical scoped allowance; do not delete evidence. |
| T2A-LAC-01 | 2A | Lock 1 | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:145`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:77`-`98`; `skinny/REDRESS.md:2910`-`2940`; `restart/audit/totality/p1/1A-substrate-evidence.md:45` | Add transient-mask and retained-sidecar prohibition to Lock 1. |
| T2A-LAC-02 | 2A | Lock 8 / BENCH | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:146`; `skinny/RESULTS.md:3`; `skinny/RESULTS.md:145`-`149`; `restart/skinny/BENCH.md:678`-`684`; `restart/skinny/BENCH.md:1498`-`1512` | Add comparator-plane provenance and route non-JSON telemetry through BENCH Section 8 gate/companion report consumers. |
| T2A-LAC-03 | 2A | Lock 16 | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:147`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:150`-`180`; `skinny/REDRESS.md:3603`-`3632`; `skinny/REDRESS.md:3766`-`3820` | Fold into primitive manifest and strict admission ledger. |
| T2A-LAC-04 | 2A | Lock 14 | ACCEPT | `restart/audit/totality/p2/2A-sota-landscape.md:148`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65`; `restart/audit/totality/p1/1A-substrate-evidence.md:46`; `skinny/RESULTS.md:94` | Add transfer only through generated data/policy traits; CSS fact streams are evidence, not closure. |
| T2A-LAC-05 | 2A | Lock 10 | MODIFY | `restart/audit/totality/p2/2A-sota-landscape.md:149`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:208`-`219`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115` | Fold as Lock 10 cost/abrogate precondition; do not make S-P3 wave scope language part of LOCKS. |
| LAC-2B-01 | 2B | Lock 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:349`; `restart/audit/totality/p1/1E-locks-evidence.md:109`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101` | Same as LAC-1E-10/T2A-LAC-03; manifest is mandatory. |
| LAC-2B-02 | 2B | Lock 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:350`; `skinny/REDRESS.md:3621`-`3625`; `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`50` | Add `BBNF_SIMD_STRICT=1` as admission command requirement; non-strict is exploratory. |
| LAC-2B-03 | 2B | Locks 14, 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:351`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:226`-`230`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:57`-`64` | Add `G-SIMD-GRAMMAR-POLICY` to shared primitive consumers. |
| LAC-2B-04 | 2B | Lock 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:352`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:194`-`206`; `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:54`; `skinny/REDRESS.md:3806`-`3812` | Add zero-orphan close states and demoted-history limitation. |
| LAC-2B-05 | 2B | Locks 1, 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:353`; `skinny/REDRESS.md:2910`-`2940`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137` | Fold retained-mask/structural replay block into Lock 1 and Lock 16. |
| LAC-2B-06 | 2B | Locks 1, 16 | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:354`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:77`-`98` | Add `substrate_target`, `retention_lifetime`, and `policy_owner` to primitive/scanner/union rows. |
| LAC-2B-07 | 2B | Lock 16 / REDRESS | ACCEPT | `restart/audit/totality/p2/2B-primitive-vocabulary.md:355`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137`; `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:74`-`78` | Fold material-differential checklist for reopen labels into Lock 16 and Lock 1. |
| LAC-2C-01 | 2C | Locks 14, 6 | ACCEPT | `restart/audit/totality/p2/2C-grammar-neutrality.md:184`; `restart/locks/LOCKS.md:78`; `restart/audit/totality/p1/1B-codegen-evidence.md:58`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:57`-`63` | Add generated provider manifest rule; do not hand-code provider enums. |
| LAC-2C-02 | 2C | Locks 14, 10 | ACCEPT | `restart/audit/totality/p2/2C-grammar-neutrality.md:185`; `restart/audit/totality/p1/1B-codegen-evidence.md:59`-`60`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:60` | Add grammar-shape leak scan to Lock 14 verification. |
| LAC-2C-03 | 2C | Locks 1, 9, 14 | MODIFY | `restart/audit/totality/p2/2C-grammar-neutrality.md:186`; `restart/audit/totality/p1/1C-runtime-evidence.md:71`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:61`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:96`-`98` | Accept grammar-owned sink/fact/flag semantics, but keep generic tape bit storage grammar-neutral and fact streams as output planes. |
| LAC-2C-04 | 2C | Locks 10, 14 | ACCEPT | `restart/audit/totality/p2/2C-grammar-neutrality.md:187`; `restart/HANDOFF.md:195`-`245`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:68`-`75` | Add onboarding proof over existing five shapes and generated facts. |
| LAC-2C-05 | 2C | Locks 16, 14 | ACCEPT | `restart/audit/totality/p2/2C-grammar-neutrality.md:188`; `skinny/REDRESS.md:3603`-`3632`; `skinny/REDRESS.md:3766`-`3820`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:246`-`247` | Add non-JSON exercise requirement when a primitive is claimed grammar-neutral. |
| LAC-2D-01 | 2D | Lock 10 | MODIFY | `restart/audit/totality/p2/2D-cost-model.md:188`; `restart/audit/totality/p1/1B-codegen-evidence.md:38`-`39`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115` | Accept active cost resolver direction; final equality-saturation schema belongs in ARCH/3A, so LOCKS gets the non-closure and fail-closed rule. |
| LAC-2D-02 | 2D | Lock 10 | MODIFY | `restart/audit/totality/p2/2D-cost-model.md:189`; `restart/audit/totality/p1/1B-codegen-evidence.md:38`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115` | Accept objective evidence requirement; defer exact `CostFacts` field list to ARCH but make stale/static fallback non-admitting in Lock 10. |
| LAC-2D-03 | 2D | Lock 14 | ACCEPT | `restart/audit/totality/p2/2D-cost-model.md:190`; `restart/audit/totality/p1/1B-codegen-evidence.md:49`-`50`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65` | Fold generated grammar metadata requirement into Lock 14 and Lock 10. |
| LAC-2D-04 | 2D | Lock 16 / CollapsedStage | ACCEPT | `restart/audit/totality/p2/2D-cost-model.md:191`; `restart/locks/LOCKS.md:87`-`112`; `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`55`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:223`-`230` | Add transient emitted-strategy constraint and x86-only literature boundary. |
| LAC-2D-05 | 2D | Lock 1 | ACCEPT | `restart/audit/totality/p2/2D-cost-model.md:192`; `skinny/REDRESS.md:2910`-`2940`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137` | Fold union-substrate history into Lock 1. |
| LAC-2E-01 | 2E | Locks 16, 1 | ACCEPT | `restart/audit/totality/p2/2E-host-arch-esoterica.md:268`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:150`-`180` | Same manifest rule as 2A/2B; include hardware gate and substrate fields. |
| LAC-2E-02 | 2E | Lock 16 | ACCEPT | `restart/audit/totality/p2/2E-host-arch-esoterica.md:269`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:194`-`206`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84`-`93` | Add zero-orphan state machine. |
| LAC-2E-03 | 2E | Lock 16 | MODIFY | `restart/audit/totality/p2/2E-host-arch-esoterica.md:270`; `restart/locks/LOCKS.md:95` | Do not erase the existing NEON-port claim; clarify that native `svmatch_u8` is SVE2-only and the NEON reduction-tree port must be separately manifested and gated. |
| LAC-2E-04 | 2E | Locks 1, 16 | ACCEPT | `restart/audit/totality/p2/2E-host-arch-esoterica.md:271`; `skinny/REDRESS.md:2910`-`2940`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:120`-`137` | Fold PMULL/CSSC reopen material-differential requirement into Locks 1/16. |
| LAC-2F-01 | 2F | Lock 14 | ACCEPT | `restart/audit/totality/p2/2F-parse-that-gaps.md:249`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65`; `restart/audit/totality/p1/1E-locks-evidence.md:57` | Add grammar-neutral parse-that/regex API rule. |
| LAC-2F-02 | 2F | Lock 16 | ACCEPT | `restart/audit/totality/p2/2F-parse-that-gaps.md:250`; `skinny/REDRESS.md:3603`-`3632`; `skinny/REDRESS.md:3766`-`3820`; `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101` | A facade cannot hide undocumented SIMD; fold into manifest/checkasm wording. |
| LAC-2F-03 | 2F | Lock 10 | MODIFY | `restart/audit/totality/p2/2F-parse-that-gaps.md:251`; `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:191`-`192`; `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:79` | Accept regex/HIR facts as required decision-engine inputs; exact fact schema belongs in ARCH/3A. |
| LAC-2F-04 | 2F | Lock 8 / direct gate | MODIFY | `restart/audit/totality/p2/2F-parse-that-gaps.md:252`; `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`-`110`; `restart/audit/totality/p1/1D-skinny-lessons.md:89`-`90`; `restart/audit/totality/p1/1D-skinny-lessons.md:107` | Fold semantic-output comparator rule into Lock 8 row-plane wording; do not add digest-hash details to LOCKS. |

## Proposed Delta Table

| proposed delta | source finding-id cited | affected V1 surface section | rationale |
|---|---|---|---|
| Add scoped substrate-ceiling and fact-output fencing to Lock 1. | 1A-SUB-014, 1A-LOCK1-AMEND-001, LAC-1E-01, T2A-LAC-01, LAC-2B-05/06, LAC-2D-05, LAC-2E-04 | `LOCKS.md` Lock 1 at `restart/locks/LOCKS.md:52` | Track 2 substrate-ceiling evidence answers substrate viability but not universal closure; REDRESS 96/97/98 retire old union replay; fact streams are output planes, not retained substrate. Evidence: `restart/audit/totality/p1/1A-substrate-evidence.md:80`-`84`, `skinny/REDRESS.md:2910`-`2940`, `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:77`-`98`. |
| Add Lock 8 row-plane, comparator-plane, and `BENCH.md` Section 8 feed. | LAC-1E-04, T2A-LAC-02, LAC-2F-04, 1D row-plane lesson | `LOCKS.md` Lock 8 at `restart/locks/LOCKS.md:66` | `skinny/RESULTS.md` carries JSON NO-GO rows and a CSS row; REDRESS says the CSS row uses dedicated W1b-2b gate, not JSON renderer. Evidence: `skinny/RESULTS.md:94`, `skinny/RESULTS.md:145`-`149`, `skinny/REDRESS.md:3836`-`3840`, `restart/skinny/BENCH.md:1498`-`1512`. |
| Add Lock 10 active cost/decision-engine evidence without shape expansion. | T2A-LAC-05, LAC-2C-04, LAC-2D-01/02/03, LAC-2F-03 | `LOCKS.md` Lock 10 at `restart/locks/LOCKS.md:70` | Five-shape canon remains, but P1-P8 cascade and thin `CostFacts` cannot be closure. Evidence: `restart/audit/totality/p1/1B-codegen-evidence.md:36`-`39`, `restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115`. |
| Add generated provider/output allowance and per-wave Lock 14 gate. | LAC-1E-08/11, T2A-LAC-04, LAC-2B-03, LAC-2C-01/02/03/05, LAC-2F-01 | `LOCKS.md` Lock 14 at `restart/locks/LOCKS.md:78` and scoped allowance block at `restart/locks/LOCKS.md:1`-`17` | Generated grammar-owned code can carry names only as rostered output; generic crates cannot hand-code provider enums or grammar-shape policy. Every wave touching generic code needs a Lock 14 baseline plus name/shape scan. Evidence: `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:53`-`65`, `restart/audit/totality/p1/1F-anti-pattern.md:33`-`40`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:226`-`230`. |
| Add strict checkasm, `escape_mask_64`, primitive manifest, zero-orphan, and hardware-gate rules to Lock 16. | LAC-1E-10, T2A-LAC-03, LAC-2B-01/02/04/07, LAC-2D-04, LAC-2E-01/02/03, LAC-2F-02 | `LOCKS.md` Lock 16 at `restart/locks/LOCKS.md:87`-`112` | Correctness/microbench proof is prerequisite only. `escape_mask_64` is checkasm-backed but not production admission until consumed; every primitive requires strict checkasm, scalar reference, same-wave consumer, row movement/rejection, and orphan close state. Evidence: `skinny/REDRESS.md:3603`-`3632`, `skinny/REDRESS.md:3766`-`3820`, `restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`58`. |
| Add G-Omega boundary note in the diff document. | PASS-3 Sections 6/8, HANDOFF, T-P2 convergence | T-P3 artifact boundary | T-P3 must not edit `LOCKS.md`; Pass Omega CRUD applies ratified changes after G3/G-Omega. Evidence: `restart/prompts/totality/PASS-3-SYNTHESIS.md:21`-`24`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:189`-`198`, `restart/prompts/ORCHESTRATOR.md:165`-`170`, `restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md:57`-`58`. |

## Consequences

Positive: the v+1 diff converts repeated T-P2 manifest/checkasm/grammar-neutrality rows into falsifiable lock clauses, keeps CSS L4 as admitted non-JSON evidence, and prevents producer-only SIMD or JSON-only generality close. The Lock 1 substrate-ceiling history makes REDRESS 96/97/98 a pre-block against replay while preserving fresh materially differentiated union attempts.

Cost: the highest-cost consequences route to implementation waves, not this artifact: generated provider manifest plus Lock 14 scan is 700-2,000 LOC per 1E (`restart/audit/totality/p1/1E-locks-evidence.md:107`); Lock 16 manifest/checkasm closure is 200-600 LOC per 1E (`restart/audit/totality/p1/1E-locks-evidence.md:109`); cost-model/decision-engine fold is high risk under T-P2 abrogate caps (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:103`-`115`).

Propagation: 12 LOCKS hunks touch at least five other surfaces after G-Omega: `ARCHITECTURE.md` for cost/fact schema, `MASTER-PLAN.md` for wave gates, `BENCH.md` for non-JSON telemetry feed, `HANDOFF.md` for G-Omega status, and `MIGRATION.md` for workspace/archive drift. T-P3 does not perform those edits; `PASS-3-SYNTHESIS.md:197`-`198` assigns actual spec edits to Pass Omega.

## Open Questions

| lens | question | receiving gate |
|---|---|---|
| CH1 correctness | Does the proposed line-level diff remain patchable after other 3X agents edit adjacent proposed surfaces? | G3 review validates `3C-locks-v+1-diff.md` against current `restart/locks/LOCKS.md`; T-P3 itself does not edit LOCKS. Evidence boundary: `PASS-3-SYNTHESIS.md:21`-`24`. |
| CH2 generality | Which exact generated provider manifest layout replaces `RuntimeProvider` without adding generic grammar branches? | 3E grammar-generalisation and Pass Omega ARCH/LOCKS CRUD; candidate evidence at `restart/audit/totality/p2/2C-grammar-neutrality.md:184`-`188`. |
| CH4 cost | How much of the detailed `CostFacts` schema belongs in LOCKS versus ARCH? | 3A should own detailed ARCH schema; 3C keeps only non-closure and abrogate gates in Lock 10. Evidence: `restart/audit/totality/p2/2D-cost-model.md:188`-`192`. |
| CH6 anti-paper-close | Are any accepted Lock 16 primitives row-admitted by the lock text alone? | No. The diff says `escape_mask_64` and microbench-only rows remain prerequisite/proof-only until same-wave consumer and row movement/rejection. Evidence: `skinny/REDRESS.md:3603`-`3632`, `skinny/REDRESS.md:3766`-`3820`. |
