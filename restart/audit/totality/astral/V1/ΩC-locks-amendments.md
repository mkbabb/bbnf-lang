# Ω-C Locks Amendments

Pass: Pass Omega.
Cycle: V1 substantive dispatch.
Date: 2026-05-21.
Scope: Audit the 16 locks against recent skinny `REDRESS.md` / `RESULTS.md` evidence and converged T-P3 hardening, preserving the 16-lock count unless user/G-Omega explicitly amends it.
Output: `restart/audit/totality/astral/V1/ΩC-locks-amendments.md` and proposed gated diff `restart/audit/totality/astral/V1/locks-diff.md`.

## Verdict

Ω-C carries the T-P3 3C lock amendment packet forward as a G-Omega-gated Pass Omega V1 proposal. The 16-lock count stays fixed. No new lock, lock retirement, directive, BIR variant, `BackendShape`, public substrate API, or retained sidecar is authorized by this artifact.

The required amendment set is: scoped Lock 14 allowance history; Lock 1 substrate-ceiling/fact-output/union-history rules; Lock 2 live-state clarification; Lock 3 empty-path verification; Lock 8 row-plane/comparator/non-JSON bench feed; Lock 9 runtime API obligations; Lock 10 active cost/regex-HIR facts; Locks 11 and 12 workspace/archive verification; Lock 13 exception discipline; Lock 14 generated-output/per-wave gate; Lock 15 skinny-vs-root profile scope; Lock 16 manifest/strict checkasm/escape-mask/orphan/hardware gates; and a governance boundary footer. This matches the G3 packet's 13 proposed hunks (`restart/audit/totality/p3/G3-PRESENTATION.md:25`).

No lock is no longer load-bearing. Locks 4, 5, 6, and 7 receive no Ω-C text amendment in V1 because the recent skinny/T-P3 evidence does not require direct lock-text surgery, but they remain active governance constraints.

## Evidence Base

| evidence surface | relevant finding |
|---|---|
| `restart/prompts/pass-contracts/PASS-OMEGA.md:30` | Ω-C must audit the 16 locks against recent skinny REDRESS and T-P3 hardening, identifying amendments, additions, and retirements. |
| `restart/prompts/pass-contracts/PASS-OMEGA.md:67` | CRUD-3 owns `restart/locks/LOCKS.md`; amendments live in `locks-diff.md` until G-Omega sign-off. |
| `restart/prompts/pass-contracts/PASS-OMEGA.md:96`-`108` | G-Omega is mandatory before any locks amendment merges. |
| `restart/prompts/pass-contracts/PASS-OMEGA.md:166`-`172` | Pass Omega is the authority for lock amendment/count accuracy, and the skinny fold remains monotonic. |
| `restart/audit/totality/p3/hardening/HARDENING-T-P3-CONVERGED.md:11`-`18` | T-P3 converged with two accepted challenge cycles. |
| `restart/audit/totality/p3/hardening/HARDENING-T-P3-CONVERGED.md:26`-`30` | 3C locks crystallisation and proposed diff are part of the converged packet. |
| `restart/audit/totality/p3/hardening/HARDENING-T-P3-CONVERGED.md:51`-`56` | T-P3 authorizes intake only, not direct edits to `LOCKS.md`, source, gates, REDRESS, RESULTS, or SK-V13 W0. |
| `restart/audit/totality/p3/3C-locks-crystallisation.md:23` | 3C identifies the same high-load amendments: Locks 1, 8, 10, 14, 16 and G-Omega boundaries. |
| `restart/audit/totality/p3/3C-locks-crystallisation.md:42`-`50` | 41 candidates resolved as 30 ACCEPT, 11 MODIFY, 0 REJECT, 0 DEFER. |
| `restart/audit/totality/p3/3C-locks-crystallisation.md:121`-`134` | CH4 cost/disposition ledger states propagation and gates per family. |
| `skinny/REDRESS.md:3555`-`3601` | SK-V12 W1a supplies concrete Lock 14 generated-config legality evidence without admitting CSS/SIMD rows. |
| `skinny/REDRESS.md:3603`-`3632` | `escape_mask_64` is an admitted correctness prerequisite only, with strict checkasm and no row movement. |
| `skinny/REDRESS.md:3718`-`3764` | CSS L4 same-plane lightningcss SOTA candidate is gate-consumed but not final until close reconciliation. |
| `skinny/REDRESS.md:3766`-`3820` | CSS delimiter ASM route is microbench-only, routes production split separately, and closes orphan inventory to zero. |
| `skinny/REDRESS.md:3822`-`3879` | SK-V12 closes by `PASS-ADMIT` through the CSS L4 declaration-values fact-stream row and records zero aarch64 orphan state. |
| `skinny/RESULTS.md:94` | `css_l4/declaration_values/direct_to_struct/main` row records the fact-stream output plane, Lock 14 pass, Lock 16 `n/a`, strict equality, and companion gate. |
| `skinny/RESULTS.md:145`-`149` | Current close notes record overall `A / Go`, SK-V12 `PASS-ADMIT`, Track 1/Track 2 definitions, companion report, and comparator provenance. |

Measured drift recorded, not invented: the historical "Current Bench Fact" at `skinny/REDRESS.md:12`-`15` still says overall `N-direct / NoGo`, while later close authority says `PASS-ADMIT` and `A / Go` (`skinny/REDRESS.md:3824`-`3840`, `skinny/RESULTS.md:145`-`149`). This is an Ω-B/Ω-E corpus-alignment finding, not a blocker to Ω-C's proposed lock amendments.

## Sixteen-Lock Audit

| lock | Ω-C disposition | amendment required | evidence |
|---|---|---|---|
| 1 | Load-bearing; amend. | Add substrate-ceiling history, fact-stream output-plane fence, declared substrate/retention/policy fields, and REDRESS 96/97/98 material-differential gate. | `restart/locks/LOCKS.md:52`; `restart/audit/totality/p3/3C-locks-crystallisation.md:29`; `skinny/REDRESS.md:3860`-`3863`. |
| 2 | Load-bearing; amend. | Add live-state clarification that `LayoutFacts.backend_shape` is live evidence while `Layout` / `LayoutSink` remain V1 obligations unless Pass Omega removes those names. | `restart/locks/LOCKS.md:54`; `restart/audit/totality/p3/3C-locks-crystallisation.md:30`. |
| 3 | Load-bearing; amend. | Add executable empty-path elision verification requirement. | `restart/locks/LOCKS.md:56`; `restart/audit/totality/p3/3C-locks-crystallisation.md:31`. |
| 4 | Load-bearing; no direct Ω-C text amendment. | None in V1; no recent skinny/T-P3 evidence requires direct Lock 4 surgery. | `restart/locks/LOCKS.md:58`; no 3C family targets Lock 4 in `restart/audit/totality/p3/3C-locks-crystallisation.md:27`-`40`. |
| 5 | Load-bearing; no direct Ω-C text amendment. | None in V1; IR/backend lower boundary remains active. | `restart/locks/LOCKS.md:60`; no 3C family targets Lock 5 in `restart/audit/totality/p3/3C-locks-crystallisation.md:27`-`40`. |
| 6 | Load-bearing; no standalone Ω-C hunk. | No direct hunk; Lock 14 generated-output language preserves the committed-source/generator boundary. | `restart/locks/LOCKS.md:62`; `restart/audit/totality/p3/3C-locks-crystallisation.md:79`. |
| 7 | Load-bearing; no direct Ω-C text amendment. | None in V1; path crate naming remains active. | `restart/locks/LOCKS.md:64`; no 3C family targets Lock 7 in `restart/audit/totality/p3/3C-locks-crystallisation.md:27`-`40`. |
| 8 | Load-bearing; amend. | Add row-plane accounting, comparator provenance, non-JSON `BENCH.md` Section 8/companion-gate feed, and semantic-output direct digest rule. | `restart/locks/LOCKS.md:66`; `restart/audit/totality/p3/3C-locks-crystallisation.md:32`; `skinny/RESULTS.md:94`; `skinny/REDRESS.md:3836`-`3840`. |
| 9 | Load-bearing; amend. | Clarify skinny facade does not close bump arena and generated-owned document obligations. | `restart/locks/LOCKS.md:68`; `restart/audit/totality/p3/3C-locks-crystallisation.md:33`. |
| 10 | Load-bearing; amend. | Preserve five-shape canon while requiring active cost evidence, generated facts, regex/HIR facts, and fail-closed gates. | `restart/locks/LOCKS.md:70`; `restart/audit/totality/p3/3C-locks-crystallisation.md:34`; `restart/audit/totality/p3/3C-locks-crystallisation.md:128`. |
| 11 | Load-bearing; amend. | Require root workspace/archive proof, not skinny truth. | `restart/locks/LOCKS.md:72`; `restart/audit/totality/p3/3C-locks-crystallisation.md:35`. |
| 12 | Load-bearing; amend. | Require archive ceremony proof via metadata transcript or Pass Omega-equivalent evidence. | `restart/locks/LOCKS.md:74`; `restart/audit/totality/p3/3C-locks-crystallisation.md:35`. |
| 13 | Load-bearing; amend. | Add explicit generated and gate/report exception discipline while preserving 500 LOC production ceiling. | `restart/locks/LOCKS.md:76`; `restart/audit/totality/p3/3C-locks-crystallisation.md:36`. |
| 14 | Load-bearing; amend. | Supersede SK-V9 allowance as history; add generated-output allowance, generated provider manifests, grammar-name/shape leak census, per-wave gate enforcement, and both Sheets and BBNF-self fail-closed/generated-role controls before fleet-wide grammar-neutral claims. | `restart/locks/LOCKS.md:1`-`17`; `restart/locks/LOCKS.md:78`; `restart/audit/totality/p3/3C-locks-crystallisation.md:37`; `skinny/REDRESS.md:3555`-`3601`. |
| 15 | Load-bearing; amend. | Clarify skinny release-profile evidence does not close root thin-LTO/profile drift or non-JSON i-cache closure. | `restart/locks/LOCKS.md:80`-`85`; `restart/audit/totality/p3/3C-locks-crystallisation.md:38`. |
| 16 | Load-bearing; amend. | Add primitive manifest, strict checkasm, `escape_mask_64` prerequisite-only state, zero-orphan close states, `CollapsedStage` transient constraints, hardware-gate clarification, and material-differential route requirements. | `restart/locks/LOCKS.md:87`-`112`; `restart/audit/totality/p3/3C-locks-crystallisation.md:39`; `skinny/REDRESS.md:3603`-`3632`; `skinny/REDRESS.md:3766`-`3820`. |

## T-P3 3C Candidate Family Disposition

| T-P3 3C family | locks / surface | T-P3 disposition | final Ω-C action | rationale |
|---|---|---|---|---|
| Scoped SK-V9 allowance history | Lock 14 preamble | MODIFY | Carry to `locks-diff.md` Hunk 1. | SK-V12 CSS L4 evidence supersedes the active allowance surface while retaining SK-V9 as scoped history (`restart/audit/totality/p3/3C-locks-v+1-diff.md:18`-`54`; `skinny/RESULTS.md:94`). |
| `3C-L01-substrate-ceiling-history` | Lock 1 | MODIFY | Carry to Hunk 2. | Repeated union/sidecar REDRESS and W5 close require material-differential reopen terms (`restart/audit/totality/p3/3C-locks-crystallisation.md:123`; `skinny/REDRESS.md:3860`-`3863`). |
| `3C-L02-layout-live-first` | Lock 2 | ACCEPT | Carry to Hunk 3. | Live `LayoutFacts.backend_shape` wording prevents paper closure while retaining V1 API obligations (`restart/audit/totality/p3/3C-locks-crystallisation.md:124`). |
| `3C-L03-path-cursor-proof` | Lock 3 | ACCEPT | Carry to Hunk 4. | Empty-path elision needs executable proof (`restart/audit/totality/p3/3C-locks-crystallisation.md:125`). |
| `3C-L08-row-plane-bench-feed` | Lock 8 / BENCH feed | ACCEPT/MODIFY | Carry to Hunk 5. | Current results mix JSON NO-GO rows with CSS fact-stream PASS-ADMIT; row-plane and companion-gate provenance must be lock text (`restart/audit/totality/p3/3C-locks-crystallisation.md:126`; `skinny/RESULTS.md:94`; `skinny/RESULTS.md:145`-`149`). |
| `3C-L09-runtime-api-obligations` | Lock 9 | ACCEPT | Carry to Hunk 6. | Skinny API does not close bump/owned V1 obligations (`restart/audit/totality/p3/3C-locks-crystallisation.md:127`). |
| `3C-L10-decision-engine-cost` | Lock 10 | MODIFY | Carry to Hunk 7. | Five-shape canon remains fixed while cost evidence, regex/HIR facts, and fail-closed gates become required (`restart/audit/totality/p3/3C-locks-crystallisation.md:128`). |
| `3C-L11-L12-workspace-drift` | Locks 11, 12 | ACCEPT | Carry to Hunk 8. | Root workspace/archive drift must be proven, not inferred from skinny (`restart/audit/totality/p3/3C-locks-crystallisation.md:129`). |
| `3C-L13-loc-exceptions` | Lock 13 | MODIFY | Carry to Hunk 9. | Generated/report exceptions require budget and transcript, with production ceiling preserved (`restart/audit/totality/p3/3C-locks-crystallisation.md:130`). |
| `3C-L14-generated-output-and-per-wave-gate` | Lock 14 | MODIFY | Carry to Hunk 10. | Repeated generated-provider, grammar-shape, and non-JSON evidence requires generated-output allowance plus per-wave leak census; fleet-wide grammar-neutral claims also require both Sheets and BBNF-self fail-closed controls or admitted generated-role fact rows, while one of those witnesses scopes claims to witnessed grammars only (`restart/audit/totality/p3/3C-locks-crystallisation.md:131`; `skinny/REDRESS.md:3557`-`3567`). |
| `3C-L15-profile-scope` | Lock 15 | ACCEPT | Carry to Hunk 11. | JSON profile facts remain scoped; root fat-LTO and non-JSON proof remain open (`restart/audit/totality/p3/3C-locks-crystallisation.md:132`). |
| `3C-L16-manifest-checkasm-orphans` | Lock 16 | ACCEPT/MODIFY | Carry to Hunk 12. | `escape_mask_64`, W4 microbench, and zero-orphan evidence require strict admission and no producer-only primitive close (`restart/audit/totality/p3/3C-locks-crystallisation.md:133`; `skinny/REDRESS.md:3605`-`3632`; `skinny/REDRESS.md:3864`-`3872`). |
| `3C-GOMEGA-boundaries` | Governance footer | ACCEPT | Carry to Hunk 13. | T-P3 and Pass Omega remain proposal-only until CHALLENGE convergence and G-Omega (`restart/audit/totality/p3/3C-locks-crystallisation.md:134`; `restart/prompts/ORCHESTRATOR.md:165`-`172`). |

## CH4 Hunk Cost Ledger

This ledger folds the T-P3 3C cost basis into Ω-C. LOC budgets are CRUD-3 document-edit budgets for the proposed `LOCKS.md` amendment unless a row names a later implementation receiver; those later receivers remain blocked until Pass Omega CHALLENGE convergence and user G-Omega sign-off.

| hunk | Ω-C family | LOC budget | propagation surfaces | risk class | wave alignment | same-wave gate | hard cap / receiver split |
|---|---|---:|---|---|---|---|---|
| Hunk 1 | Scoped SK-V9 allowance history | 40-90 docs | 3: `LOCKS.md`, G-Omega status text, skinny evidence references | High | Lock 14 history cleanup | SK-V9 remains historical and scoped; SK-V12 CSS L4 remains one strict positive row, not universal grammar closure. | Hard cap 90 doc LOC; CRUD-3 doc receiver only. |
| Hunk 2 | `3C-L01-substrate-ceiling-history` | 90-180 docs | 5: `LOCKS.md`, `ARCHITECTURE.md`, `MASTER-PLAN.md`, `HANDOFF.md`, `MIGRATION.md` | High | Lock 1 + union material-differential wave | No union replay without material differential and measured row consumer/block. | Hard cap 180 doc LOC; any union/source work routes to a later implementation wave. |
| Hunk 3 | `3C-L02-layout-live-first` | 20-50 docs | 2: `LOCKS.md`, `ARCHITECTURE.md` | Low-medium | Lock 2 API-freeze cleanup | Live `LayoutFacts.backend_shape` text must not claim full `LayoutSink` closure. | Hard cap 50 doc LOC; CRUD-3 doc receiver only. |
| Hunk 4 | `3C-L03-path-cursor-proof` | 20-50 docs | 2: `LOCKS.md`, generator verification docs/tests | Medium | Lock 3 verification wording | Empty-path elision needs executable proof before close. | Hard cap 50 doc LOC; implementation/test proof routes to the owning generator wave. |
| Hunk 5 | `3C-L08-row-plane-bench-feed` | 120-260 docs | 5: `LOCKS.md`, `BENCH.md`, companion gate report, `MASTER-PLAN.md`, skinny corpus sync | High | Lock 8 + BENCH non-JSON telemetry | Row-plane tables must carry comparator provenance and strict equality. | Hard cap 260 doc LOC; BENCH/gate production is a later receiver, not CRUD-3. |
| Hunk 6 | `3C-L09-runtime-api-obligations` | 20-50 docs | 2: `LOCKS.md`, runtime API obligation docs | Low-medium | Lock 9 V1 obligation note | Skinny API cannot close bump/generator-owned document obligations. | Hard cap 50 doc LOC; runtime source/API tests route to a later implementation wave. |
| Hunk 7 | `3C-L10-decision-engine-cost` | 140-320 docs | 5: `LOCKS.md`, `ARCHITECTURE.md`, `MASTER-PLAN.md`, `HANDOFF.md`, decision-engine receiver docs | High | Lock 10 + decision-engine fold | Active CostFacts and regex/HIR facts are required; P1-P8 fallback is non-admitting. | Hard cap 320 doc LOC; decision-engine implementation routes to a later generated-facts wave. |
| Hunk 8 | `3C-L11-L12-workspace-drift` | 30-80 docs | 3: `LOCKS.md`, `HANDOFF.md`, workspace/archive metadata transcript | Medium | Workspace/archive Omega CRUD | Archive/remove proof required for root drift closure. | Hard cap 80 doc LOC; workspace mutation requires separate authorized CRUD. |
| Hunk 9 | `3C-L13-loc-exceptions` | 30-80 docs | 3: `LOCKS.md`, gate/report LOC transcript, generated artifact roster | Medium | LOC budget gate | Generated/report exceptions require budgets and transcripts. | Hard cap 80 doc LOC; generated/report growth is not admitted by this doc edit. |
| Hunk 10 | `3C-L14-generated-output-and-per-wave-gate` | 180-420 docs | 6: `LOCKS.md`, `ARCHITECTURE.md`, `MASTER-PLAN.md`, `BENCH.md`, generated-runtime registry docs, skinny corpus sync | High | Lock 14 registry/runtime wave | Generated names allowed only through rostered output; grammar-shape leak scan required per wave; fleet-wide transfer requires CSS L4 plus both Sheets and BBNF-self controls. | Hard cap 420 doc LOC; provider registry/runtime/source work routes to a later implementation wave. |
| Hunk 11 | `3C-L15-profile-scope` | 20-50 docs | 2: `LOCKS.md`, profile/build-status docs | Medium | Lock 15 scope clarification | Skinny profile evidence cannot close root thin-LTO drift. | Hard cap 50 doc LOC; build/profile changes route to a later implementation wave. |
| Hunk 12 | `3C-L16-manifest-checkasm-orphans` | 180-420 docs | 6: `LOCKS.md`, `ARCHITECTURE.md`, `BENCH.md`, `HANDOFF.md`, primitive manifest docs, hardware-gate docs | High | Lock 16 primitive/SIMD waves | Strict checkasm, scalar reference, first consumer, row movement/rejection, and zero-orphan disposition. | Hard cap 420 doc LOC; SIMD/ASM/primitive implementation routes to later primitive waves. |
| Hunk 13 | `3C-GOMEGA-boundaries` | 40-100 docs | 4: `LOCKS.md`, G-Omega presentation, `HANDOFF.md`, orchestrator/pass-contract status | High process | G3/G-Omega | T-P3 proposes only; authoritative edits merge only through Omega/G-Omega flow. | Hard cap 100 doc LOC; no implementation receiver. |

## Additions And Retirements

| question | Ω-C finding |
|---|---|
| Lock additions justified by repeated REDRESS evidence? | No new numbered lock is justified in V1. Repeated evidence is better expressed as clauses under Locks 1, 8, 10, 14, and 16 while preserving the 16-lock count (`restart/prompts/pass-contracts/PASS-OMEGA.md:171`; `restart/audit/totality/p3/G3-PRESENTATION.md:33`-`36`). |
| Locks no longer load-bearing? | None. All 16 locks remain load-bearing. |
| Amendment count? | 13 proposed hunks: scoped allowance history, 11 lock hunks touching Locks 1/2/3/8/9/10/11/12/13/14/15/16, and one governance footer (`restart/audit/totality/p3/G3-PRESENTATION.md:25`). |
| Missing evidence? | The stale top `REDRESS.md` current-bench paragraph conflicts with later close authority; record as Ω-B/Ω-E measurable corpus drift (`skinny/REDRESS.md:12`-`15`; `skinny/RESULTS.md:145`-`149`). |

## G-Omega Blockers And CRUD-3 Receiver

| blocker | exact condition | receiver |
|---|---|---|
| CHALLENGE convergence | Ω-C cannot merge by itself; Pass Omega CHALLENGE must converge before CRUD executes (`restart/prompts/pass-contracts/PASS-OMEGA.md:86`-`94`). | Orchestrator / Pass Omega hardening. |
| User G-Omega | User must close G-Omega before any locks amendment merges (`restart/prompts/pass-contracts/PASS-OMEGA.md:96`-`108`; `restart/prompts/ORCHESTRATOR.md:165`-`172`). | User gate. |
| CRUD authorization | CRUD-3 applies only the challenge-authorized locks diff; no CRUD agent edits beyond CHALLENGE CONSOLIDATED authorization (`restart/prompts/pass-contracts/PASS-OMEGA.md:57`-`74`). | **CRUD-3 LOCKS**, target `restart/locks/LOCKS.md` (+ new lock files only if user/G-Omega amends count), source diff `restart/audit/totality/astral/V1/locks-diff.md`. |
| Scope freeze | No implementation wave may use proposed text to edit source, gate output, `RESULTS.md`, `REDRESS.md`, or dispatch SK-V13 W0 before G-Omega (`restart/audit/totality/p3/G3-PRESENTATION.md:64`-`68`). | Orchestrator / skinny dispatch owner. |
