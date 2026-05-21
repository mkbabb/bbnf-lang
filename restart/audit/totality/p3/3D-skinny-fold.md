---
agent: 3D
pass: T-P3-synthesis
cycle: V1
generated_at: 2026-05-21T19:13:04Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: "V1.1 proposed-delta packet only; no direct spec or lock edit"
proposed_deltas_count: 10
delta_summary:
  carried_from_prior_cycle: []
  removed: []
  answered: []
  newly_added:
    - FOLD-3D-001
    - FOLD-3D-002
    - FOLD-3D-003
    - FOLD-3D-004
    - FOLD-3D-005
    - FOLD-3D-006
    - FOLD-3D-007
    - FOLD-3D-008
    - FOLD-3D-009
    - FOLD-3D-010
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
---

## Executive Summary

T-P3 3D folds skinny evidence into totality only as proposed V1 amendments: the
pass is explicitly a synthesis pass, not a direct spec edit, and 3D is scoped to
make skinny wins V1-authoritative while using skinny rejections as lock evidence
for 3C rather than live skinny requirements
(`restart/prompts/totality/PASS-3-SYNTHESIS.md:3`,
`restart/prompts/totality/PASS-3-SYNTHESIS.md:52`,
`restart/prompts/totality/PASS-3-SYNTHESIS.md:213`). The durable skinny wins
are: single-substrate JSON tape plus direct projection, cold payload arenas,
typed/direct rows only when generated artifacts and oracle parity prove them,
one admitted CSS L4 generated row, GrammarConfig/Lock14 partial legality,
resolved escape-mask correctness prerequisite, and zero-orphan SIMD cleanup
(`skinny/REDRESS.md:110`, `skinny/REDRESS.md:126`,
`skinny/REDRESS.md:134`, `skinny/REDRESS.md:3106`,
`skinny/RESULTS.md:94`, `skinny/REDRESS.md:3557`,
`restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:50`). The durable rejections are
equally important: parse-only admission, direct residual rows, pair fusion,
dispatch-table substitution, skipless token substitution, union class-column
replay, support-only SIMD, and one-row CSS closure cannot silently graduate
(`skinny/REDRESS.md:209`, `skinny/REDRESS.md:216`,
`skinny/REDRESS.md:226`, `skinny/REDRESS.md:2910`,
`restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`). Proposed fold count: 10.

## V1 Delta Summary

| disposition | count | notes |
| --- | ---: | --- |
| Carried from prior T-P3 cycle | 0 | This is the first 3D skinny-fold packet for the current T-P3 cycle; T-P3 requires proposed amendments only, not direct spec edits (`restart/prompts/totality/PASS-3-SYNTHESIS.md:21`). |
| Removed | 0 | No prior 3D deltas exist to remove in this packet. |
| Answered | 0 | Open questions are routed to 3C/3E/3F rather than marked closed in 3D (`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`). |
| Newly added | 10 | The folds below distill 1D skinny lessons, T-P2 external dossiers, S-P1/S-P2 hardening, and SK-V13 scoping into monotonic totality amendments (`restart/audit/totality/p1/1D-skinny-lessons.md:1`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:112`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md:86`). |

## Proposed Delta Table

| proposed delta | source T-P1/T-P2 finding-id cited | affected V1 surface section | rationale and expected 3C/3E impact |
| --- | --- | --- | --- |
| FOLD-3D-001: Add a single-substrate/output-plane taxonomy that treats JSON offset tape, direct SinkOnly projection, and CSS fact-stream rows as one substrate family with fenced output planes. | 1A records direct shared tape as partial, CSS fact-stream as admitted evidence with a substrate-category gap, and scanner sidecars as fenced (`restart/audit/totality/p1/1A-substrate-evidence.md:32`, `restart/audit/totality/p1/1A-substrate-evidence.md:46`, `restart/audit/totality/p1/1A-substrate-evidence.md:58`). Skinny proves one-buffer tape and direct projection over tape spans, not a parallel tree (`skinny/REDRESS.md:110`, `skinny/REDRESS.md:126`). | Lock 1, substrate catalog, direct row spec, CSS row spec. | This should become a 3C Lock 1 MODIFY candidate: admit fact-stream rows as an output-plane of the same substrate discipline while preserving the ban on parallel substrates. 3E should reuse this taxonomy when checking non-JSON import boundaries and BackendShape consumers. |
| FOLD-3D-002: Fold cold/lazy materialization into the totality substrate contract, including generated grammar-owned flags and sinks. | Skinny payload counters remain cold across admitted JSON/CSS results (`skinny/REDRESS.md:134`, `skinny/RESULTS.md:99`). T-P1 finds Lock14 root leaks around sink/flags, and T-P2 says generated per-grammar config/sink is legal while public JSON-shaped traits are not (`restart/audit/totality/p1/1C-runtime-evidence.md:79`, `restart/audit/totality/p1/1C-runtime-evidence.md:83`, `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:56`, `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:61`). | Lock 1, Lock 14, runtime consumption, generated sink schema. | 3C should bind cold payload/lazy materialization as a lock-strengthening rule rather than a JSON-only optimization. 3E should verify generated flag/sink surfaces are grammar-owned and not public generic runtime traits. |
| FOLD-3D-003: Replace parse-only tolerance with row-plane SOTA accounting for parse, direct, typed, and non-JSON rows. | Skinny records parse-only rows as NO-GO, direct residual rows as NO-GO, and typed rows as GO only when generated products and oracle parity land (`skinny/RESULTS.md:5`, `skinny/RESULTS.md:6`, `skinny/RESULTS.md:7`, `skinny/REDRESS.md:2980`, `skinny/REDRESS.md:3040`, `skinny/REDRESS.md:3106`). 1E already proposes row-plane accounting (`restart/audit/totality/p1/1E-locks-evidence.md:103`). | Lock 8, BENCH, skinny-results import contract. | 3C should MODIFY Lock 8 so every result row carries corpus, plane, comparator, generated-artifact, row-predicate, and routed-remainder fields. 3E should apply the same row-plane ledger to CSS and future grammar corpora. |
| FOLD-3D-004: Admit the SK-V12 CSS L4 row as durable evidence but explicitly reject it as full CSS parity or campaign closure under SK-V13. | The CSS row is admitted with strict equality artifacts and SOTA movement (`skinny/RESULTS.md:94`, `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md:12`). SK-V13 reopens full CSS parity: one admitted feature family, 23 remaining non-OOS families, and feature matrices still missing or partial (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:38`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:90`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:94`). | Lock 8, Lock 14, CSS parity matrix, non-JSON benchmark gate. | 3C should record the CSS row as positive Lock 8/14 evidence, not as a closure shortcut. 3E should require full feature-family coverage before any non-JSON totality claim can be promoted. |
| FOLD-3D-005: Generalize Lock14 through generated provider registries while preserving the name-versus-shape distinction for grammar-specific code. | REDRESS admits GrammarConfig legality only as partial Lock14 evidence (`skinny/REDRESS.md:3557`). 1E says Lock14 must distinguish grammar-specific names from grammar-specific generic-runtime shapes (`restart/audit/totality/p1/1E-locks-evidence.md:112`). T-P2 defines the transfer contract: generated modules from one template are legal, hard-coded grammar arms/names/types/flags in generic crates are not (`restart/audit/totality/p2/2C-grammar-neutrality.md:82`). | Lock 14, Lock 10, directive lowering, generated provider registry, grammar-onboarding test. | 3C should MODIFY Lock14 with an explicit generated-provider exception and reject public GrammarConfig/JsonSink-style generic abstractions. 3E should require Sheets or BBNF-self negative controls before declaring a primitive grammar-neutral. |
| FOLD-3D-006: Promote SIMD/ASM only through a Lock16 manifest with scalar parity, checkasm-style validation, row movement, same-wave production consumer, and zero-orphan disposition. | Skinny resolves escape-mask correctness as a prerequisite but keeps W4 microbench production wiring split out and demotes five SIMD orphans (`skinny/REDRESS.md:3603`, `skinny/REDRESS.md:3766`, `skinny/REDRESS.md:3869`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:12`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:21`). T-P2 defines admitted/conditional/refuted primitive states and manifest fields (`restart/audit/totality/p2/2B-primitive-vocabulary.md:126`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:186`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:115`). | Lock 16, SIMD primitive ledger, host-arch gate, BENCH row admission. | 3C should MODIFY Lock16 so support-only SIMD never counts as totality evidence. 3E should reject primitive-only imports unless they include scalar fallbacks, corpus parity, feature gating, and a same-wave production consumer. |
| FOLD-3D-007: Add a rejected-route ledger that prevents replay of skinny failures without material differentials. | Pair fusion, dispatch tables, and skipless 12-byte tokens were rejected (`skinny/REDRESS.md:209`, `skinny/REDRESS.md:216`, `skinny/REDRESS.md:226`). Union class-column and streaming cursor attempts failed and were retired unless a fresh material differential exists (`skinny/REDRESS.md:2795`, `skinny/REDRESS.md:2850`, `skinny/REDRESS.md:2910`). T-P2 repeats the material-differential gate for future union work (`restart/audit/totality/p2/2E-host-arch-esoterica.md:184`). | Lock 1, Lock 4, Lock 10, Lock 14, Lock 16, rejected-route appendix. | 3C should strengthen locks by naming historical preblocks as non-admissive unless a candidate changes data movement, consumer shape, or measured row outcome. 3E should use the ledger as a replay filter, not as a ban on fresh evidence. |
| FOLD-3D-008: Fold the SK-V13 decision-engine route as replacement of the P1-P8 cascade, not as a new directive, BIR, BackendShape, or substrate. | SK-V13 requires bbnf-regex, egraph, active cost, and CSP replacement while deleting or gating P1-P8 (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:59`). The scoping audit finds passive cost, absent/stubbed CSP, absent egraph, and hard-coded recognizers (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:13`). T-P2 says the exact cascade is partial/refuted and active-cost abrogation gates are needed (`restart/audit/totality/p2/2D-cost-model.md:55`, `restart/audit/totality/p2/2D-cost-model.md:139`). | Lock 4, Lock 10, Lock 14, cost model, recognizer import boundary. | 3C should route this to Lock 4/10/14 MODIFY candidates with an explicit no-new-surface clause. 3E should verify the optimizer consumes existing five BackendShape values and generated providers only. |
| FOLD-3D-009: Add a full-SOTA/anti-demotion handoff gate for G-Omega before any W0 implementation packet. | SK-V13 requires full ADMIT or architectural block for every remaining row/feature, 51 JSON rows, and full CSS parity; G-Omega must run before W0 (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:30`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:112`). S-P1 and S-P2 converged as research/hardening, not admissions (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:53`, `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:46`). | Lock 8, BENCH, HANDOFF, implementation packet gate. | 3C should ensure no lock wording allows S-P1/S-P2 profile facts or one-row CSS evidence to close the campaign. 3F should receive a gate that names the remaining JSON/CSS rows and requires G-Omega before W0 dispatch. |
| FOLD-3D-010: Preserve the monotonic skinny boundary: skinny evidence informs totality, totality does not reopen or rewrite live skinny artifacts. | T-P3 says proposed deltas feed Pass Omega and do not edit V1 directly (`restart/prompts/totality/PASS-3-SYNTHESIS.md:21`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:228`). The pass also says 3C must disposition every lock candidate and no skinny fold may silently reverse direction (`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:213`). The handoff forbids governance surface edits from this packet (`restart/HANDOFF.md:44`). | T-P3 packet boundary, 3C lock-disposition checklist, Pass Omega intake. | 3C should treat this file as evidence, not authority. 3E/3F should cite skinny only as historical evidence unless a future skinny tranche independently admits or rejects new rows. |

## Expected 3C Lock Impact Index

| 3C target | expected disposition pressure | contributing folds |
| --- | --- | --- |
| Lock 1 substrate | MODIFY: add output-plane taxonomy, cold/lazy materialization, and rejected-route replay filter while preserving one-substrate discipline (`restart/locks/LOCKS.md:52`). | FOLD-3D-001, FOLD-3D-002, FOLD-3D-007 |
| Lock 4 output-piping | MODIFY: replace hard-coded decision cascade with active cost/CSP/egraph gates without fused generic substrate or post-hoc output piping (`restart/locks/LOCKS.md:58`). | FOLD-3D-008 |
| Lock 8 SOTA | MODIFY: row-plane SOTA ledger, CSS partial-admit rule, full-SOTA handoff gate, and no parse-only demotion loophole (`restart/locks/LOCKS.md:66`). | FOLD-3D-003, FOLD-3D-004, FOLD-3D-009 |
| Lock 10 directives | MODIFY: keep BackendShape side-table/directive surface closed; generated provider selection may specialize, but no new public directive or BackendShape follows from skinny (`restart/locks/LOCKS.md:70`). | FOLD-3D-005, FOLD-3D-008 |
| Lock 14 grammar generalization | MODIFY: legal generated per-grammar modules from one template; illegal generic runtime grammar arms, names, public JSON-shaped sinks, or JSON-specific flags (`restart/locks/LOCKS.md:78`). | FOLD-3D-002, FOLD-3D-004, FOLD-3D-005, FOLD-3D-008 |
| Lock 16 SIMD | MODIFY: require scalar/checkasm/corpus parity, feature gates, same-wave production consumer, and zero-orphan disposition (`restart/locks/LOCKS.md:87`). | FOLD-3D-006, FOLD-3D-007 |
| 5-shape BackendShape coherence | VERIFY in 3C/3E: decision engine and non-JSON import must consume the existing five shapes, not create hidden shape variants (`restart/audit/totality/p1/1B-codegen-evidence.md:36`, `restart/prompts/totality/PASS-3-SYNTHESIS.md:211`). | FOLD-3D-005, FOLD-3D-008 |

## Consequences

| delta group | positive effect | cost / risk / wave | propagation |
| --- | --- | --- | --- |
| FOLD-3D-001 and FOLD-3D-002 | Converts skinny substrate wins into a grammar-neutral substrate taxonomy without reopening live skinny rows. | Documentation delta is small, but implementation follow-through is medium because runtime sinks, flags, and output planes cross substrate and runtime ownership; T-P1 already flags shared scheduling and CSS category gaps (`restart/audit/totality/p1/1A-substrate-evidence.md:32`, `restart/audit/totality/p1/1A-substrate-evidence.md:46`). | 3C Lock 1/14 wording, 3E non-JSON import checks, future generated sink audits. |
| FOLD-3D-003 and FOLD-3D-004 | Stops parse-only and one-row CSS evidence from masquerading as campaign closure while preserving the admitted CSS row. | High verification cost because SK-V13 reopens 51 JSON rows and 23 remaining CSS feature families (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:95`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md:94`). | 3C Lock 8, BENCH schema, 3F handoff gate, future skinny result ledgers. |
| FOLD-3D-005 | Makes Lock14 actionable: generated per-grammar specialization is legal only when generic crates remain grammar-shape neutral. | High risk if totality blurs grammar names with grammar shapes; T-P1 found current drift and T-P2 requires negative controls (`restart/audit/totality/p1/1E-locks-evidence.md:76`, `restart/audit/totality/p2/2C-grammar-neutrality.md:157`). | 3C Lock 14, 3E generated-provider registry, Sheets/BBNF-self onboarding tests. |
| FOLD-3D-006 and FOLD-3D-007 | Converts SIMD, ASM, and union history into admission gates instead of repeated experiments. | Medium-to-high implementation risk because primitives need scalar parity, host feature gates, production consumers, and material differentials before admission (`restart/audit/totality/p2/2B-primitive-vocabulary.md:186`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:184`). | 3C Lock 16, rejected-route appendix, SIMD manifest, future union packet checklist. |
| FOLD-3D-008 | Gives the decision-engine work a bounded totality receiving surface and prevents optimizer work from inventing new directives or shapes. | High design risk because the current engine is hard-coded and missing egraph/CSP pieces (`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md:13`). | 3C Lock 4/10/14, 3E BackendShape coherence, implementation SPEC/DISPATCH after G-Omega. |
| FOLD-3D-009 and FOLD-3D-010 | Keeps S-P1/S-P2 as evidence and makes G-Omega the pre-W0 gate, preserving monotonic skinny-to-totality direction. | Low documentation risk, high process risk if future packets treat research profiles as admissions (`restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md:53`, `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:46`). | 3C no-silent-drop checklist, 3F handoff, Pass Omega intake. |

## Open Questions

| id | question | routed receiver |
| --- | --- | --- |
| CH1 | Does 3C disposition every 1E plus 2X lock candidate touched by these folds, including Lock 1, 4, 8, 10, 14, and 16, with no silent drops (`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`)? | 3C |
| CH2 | Should the generated-provider Lock14 amendment require Sheets and BBNF-self negative controls in the first V1.1 lock text, or may that remain a 3E onboarding gate (`restart/audit/totality/p2/2C-grammar-neutrality.md:157`)? | 3C/3E |
| CH3 | What exact evidence distinguishes a fresh union material differential from a replay of REDRESS 96/97/98 (`skinny/REDRESS.md:2910`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:73`)? | 3C |
| CH4 | Should CSS source-sidecar/lightningcss comparator code be named in Lock 1/14 text as comparator-only, so it cannot be mistaken for runtime substrate (`restart/audit/totality/p1/1A-substrate-evidence.md:58`, `skinny/REDRESS.md:3681`)? | 3C/3E |
| CH5 | Where should the G-Omega before W0 gate live: HANDOFF only, BENCH only, or both (`restart/skinny/tranches/sk-v13/HANDOFF.md:54`, `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md:60`)? | 3F |
| CH6 | Should row-plane SOTA ledgers include profile-method freshness as a required column, given S-P1 found profile facts are not gate admissions (`restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:13`, `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md:11`)? | 3C/3F |
