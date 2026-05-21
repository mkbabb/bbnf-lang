# Ω-D Master-Plan Reconciliation

Pass: Pass Omega V1 substantive dispatch, agent Ω-D.
Cycle: V1.
Date: 2026-05-21.
Scope: Reconcile `restart/MASTER-PLAN.md` §H and the current A-J wave ledger against landed skinny work, T-P1/T-P2/T-P3 findings, `skinny/REDRESS.md`, `skinny/RESULTS.md`, and the SK-V13 full-SOTA contract. This is proposal-only: no governance surface is amended here.
Output: `restart/audit/totality/astral/V1/ΩD-master-plan-reconciliation.md` plus the companion proposed diff at `restart/audit/totality/astral/V1/master-plan-diff.md`.

## Boundary And Authority

Ω-D owns MASTER reconciliation: audit §H against actual landed work, identify landed/refuted/pending waves, update allocations/references, and identify new waves implied by skinny REDRESS plus totality findings (`restart/prompts/pass-contracts/PASS-OMEGA.md:31`). Pass Omega is above the T-P1/T-P2/T-P3 cycle and consumes skinny REDRESS/RESULTS into V1 spec surfaces, but CRUD edits occur only after convergence (`restart/prompts/pass-contracts/PASS-OMEGA.md:3-11`, `restart/prompts/pass-contracts/PASS-OMEGA.md:76-95`). G-Omega is mandatory before proposed master-plan and locks diffs merge (`restart/prompts/pass-contracts/PASS-OMEGA.md:96-110`).

T-P3 is converged, but it authorizes intake only. The G3 packet says T-P3 is proposal-only, retains the 16-lock count, keeps SK-V13 W0 blocked until G-Omega, and authorizes no governance/source/generated/RESULTS/REDRESS edits before Omega convergence and G-Omega (`restart/audit/totality/p3/G3-PRESENTATION.md:31-47`, `restart/audit/totality/p3/G3-PRESENTATION.md:62-68`). The converged record repeats that no `MASTER-PLAN.md` edit, source edit, generated-runtime edit, gate output, `skinny/RESULTS.md`, `skinny/REDRESS.md`, or SK-V13 W0 is authorized by T-P3 (`restart/audit/totality/p3/hardening/HARDENING-T-P3-CONVERGED.md:49-56`).

## Current MASTER Census

| census item | current state | citation |
|---|---|---|
| Tranche set | A-J exists, with stub counts that are explicitly planning stubs. | `restart/MASTER-PLAN.md:181-202` |
| A | 5 concrete wave rows, A.W0-A.W4. | `restart/MASTER-PLAN.md:255-277` |
| B | 5 concrete wave rows, B.W0-B.W4. | `restart/MASTER-PLAN.md:290-310` |
| C | 6 concrete wave rows, C.W0-C.W5. | `restart/MASTER-PLAN.md:323-346` |
| D | 6 concrete wave rows, D.W0-D.W5. | `restart/MASTER-PLAN.md:359-380` |
| E | 5 concrete wave rows, E.W0-E.W4. | `restart/MASTER-PLAN.md:393-414` |
| F | 6 concrete wave rows, F.W0-F.W5. | `restart/MASTER-PLAN.md:424-446` |
| G | 5 concrete wave rows, G.W0-G.W4. | `restart/MASTER-PLAN.md:457-478` |
| H | The summary table still says 5 stub waves, but §13 contains 10 concrete rows: H.W0, H.W1, H.W2, H.W2.5, H.W3, H.W4, H.W4.LOCK14, H.W5, H.W6, H.W7. | `restart/MASTER-PLAN.md:198`, `restart/MASTER-PLAN.md:524-535` |
| I | 5 concrete wave rows, I.W0-I.W4. | `restart/MASTER-PLAN.md:629-637` |
| J | 6 concrete wave rows, J.W0-J.W5. | `restart/MASTER-PLAN.md:660-669` |
| Total current concrete rows | 59 concrete rows when §H's 10 current rows are counted. T-P3 3B independently classified these as 4 landed, 1 refuted, and 54 pending, then proposed 12 new waves. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:34-41` |

## Landed / Refuted / Pending Ledger

| wave | Ω-D status | reconciliation | evidence |
|---|---|---|---|
| B.W0 | landed-scoped | Tape tokens/append substrate exist for skinny JSON, but checkpoint/rollback and V1 root substrate closure remain pending. | `restart/MASTER-PLAN.md:306`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:52` |
| B.W4 | landed-scoped | One generated grammar parses through tape/direct skinny shells; this is not full V1 generated-runtime closure. | `restart/MASTER-PLAN.md:310`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:56`, `skinny/RESULTS.md:145-148` |
| H.W0 | landed-scoped | Preflight/build profile, capacity, escape-mask correctness prerequisites are landed; they do not admit a throughput row by themselves. | `restart/MASTER-PLAN.md:526`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:85`, `skinny/REDRESS.md:3603-3632` |
| H.W1 | partial/pending-throughput | Rust-state substrate and backend-shape derivation landed, but throughput recovery remains open. Proposed V1.1 text should mark this as `PARTIAL: landed substrate, pending row movement`, not plain landed. | `restart/MASTER-PLAN.md:527` |
| H.W2 | partial/pending-consumer | Several aarch64 primitives have scalar references, parity, and admitted hot consumers, but new primitives require same-wave row movement. | `restart/MASTER-PLAN.md:528`, `restart/audit/totality/p3/3D-skinny-fold.md:72-73` |
| H.W2.5 | partial/pending-state-machine | Primitive vocabulary exists, but contract-level macros remain inadmissible without same-wave structural-tape, bracket-stack, or CollapsedStage consumers. | `restart/MASTER-PLAN.md:529` |
| H.W3 | split: number landed; UTF-8/string fusion refuted-as-close | Integer/number materialization landed; the UTF-8/string close route is refuted as a SOTA close and must not be reintroduced under another name. | `restart/MASTER-PLAN.md:530`, `skinny/REDRESS.md:1686-1887` |
| H.W4 | partial/pending | SinkOnly/direct rows are correctness-green and some rows admitted, but the direct matrix remains open and SK-V13 reopens all JSON planes under strict sonic-rs. | `restart/MASTER-PLAN.md:531`, `skinny/RESULTS.md:5-45`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-110` |
| H.W4.LOCK14 | partial/pending | GrammarConfig legality is concrete evidence, but it is not full Lock 14 repair for CSS/Sheets/BBNF-self or generic-crate grammar-shape neutrality. | `restart/MASTER-PLAN.md:532`, `skinny/REDRESS.md:3555-3601` |
| H.W5 | landed-scoped | Consumed arm64/generic primitive set is landed; x86 CollapsedStage successor is optional/background and no orphan macro can count without a consumer. | `restart/MASTER-PLAN.md:533`, `skinny/REDRESS.md:3822-3872` |
| H.W6 | pending and stale | MASTER still describes the SK-V6 strict matrix before CSS gates; SK-V13 supersedes this with full CSS parity plus all JSON rows under strict comparators. | `restart/MASTER-PLAN.md:534`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-110` |
| H.W7 | pending | Pratt recognizer facts and `PrattSpine` remain pending and still depend on C/E/BIR fact closure. | `restart/MASTER-PLAN.md:535`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:94` |
| Non-H existing waves | mostly pending | T-P3's whole-MASTER census remains valid for the non-H rows: 54 pending overall across the 59 existing rows after four scoped landings and one refuted close route are separated. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:34-41`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:47-105` |

## Proposed V1.1 Renames / Status Changes

| target | proposed V1.1 change | cost / risk | receiver |
|---|---|---|---|
| §5 Tranche Set H row | Change H stub count from `5` to `10 current rows; V1.1 status ledger required`, and add a note that A-J counts are planning stubs but §H has already expanded through skinny fold-back. | 20-50 doc LOC; medium risk if counts are silently renumbered. | CRUD-2 MASTER census note. |
| H.W1 | Rename status to `PARTIAL - substrate landed, throughput pending`. Preserve the `603308b3` Rust-state landing but forbid treating it as row admission. | 20-60 doc LOC; medium process risk. | H/J benchmark row gate plus S-P3 dispatch guard. |
| H.W2 + H.W2.5 | Merge their close criteria under a primitive state-machine note: `admitted`, `conditional`, `refuted`, `demoted/deleted`, or `architectural-block`, each with scalar parity and same-wave consumer evidence. | 80-180 doc LOC; high hidden-coupling risk. | Lock 16 manifest, BENCH primitive ledger, S-P3 wave plans. |
| H.W3 | Split into `H.W3a number materialization landed` and `H.W3b string/Unicode exactness pending; UTF-8 fusion refuted-as-close`. | 40-120 doc LOC; high regression risk if REDRESS 66-69 routes are replayed. | D/H regex/HIR fact boundary and JSON/CSS row gates. |
| H.W4 | Rename to `JSON row-plane direct/typed matrix`, with parse_only/direct_to_struct/real_typed_struct all visible. | 60-160 doc/report LOC; high gate risk. | BENCH row-plane ledger and SK-V13 G5. |
| H.W4.LOCK14 | Rename to `Lock 14 provider/config/sink repair`, scoped as partial until generated non-JSON plus negative controls pass. | 60-160 doc LOC; high generality risk. | LOCKS diff, generated-provider registry, Sheets/BBNF-self witnesses. |
| H.W5 | Rename to `consumed arm64/generic primitive set landed; x86 successor optional; zero-orphan gate mandatory`. | 30-80 doc LOC; medium risk. | Lock 16 manifest and S-P3 G4. |
| H.W6 | Replace SK-V6 strict-matrix wording with `SK-V13 full-SOTA receiver map`: G1 CSS parity, G2 decision engine, G3 union, G4 zero aarch64 orphans, G5 51 JSON rows, G6 Totality V1.1/G-Omega, G7 no-demotion. | 120-260 doc LOC; high risk if it narrows the user pin. | MASTER §13, BENCH, HANDOFF, S-P3 SPEC. |
| H.W7 | Keep pending, but add dependency on C.W3/C.W4/E.W0/E.W1 fact/BIR closure and the decision-engine fold. | 20-80 doc LOC; medium dependency risk. | C/E/H recognizer fact rows. |

## Proposed V1.1 Wave Additions

| wave | allocation | cost / risk | receiver |
|---|---|---|---|
| MP.NW0 | G-Omega and Totality V1.1 ratification before SK-V13 W0/source/RESULTS/REDRESS wave. | 250-700 doc LOC; high process risk. | G-Omega packet, CRUD-2/4, S-P3 pre-W0 refusal gate. |
| MP.NW1 | Current-state authority and row-plane telemetry fold for SK-V12 CSS admission, JSON parse/direct/typed planes, companion reports, and stale-current surfaces. | 180-420 doc/report LOC; high gate risk. | BENCH/HANDOFF/MASTER current-state ledger. |
| MP.NW2 | CSS stylesheet root and selector framework under full lightningcss parity, not one-row closure. | 350-500 impl/doc LOC; high parity risk. | SK-V13 G1 CSS feature rows. |
| MP.NW3 | CSS declaration-values expansion: declarations, `var()`, `calc()`, colors, custom-property/value facts. | 600-840 impl/doc LOC; medium-high risk. | SK-V13 G1 feature gates. |
| MP.NW4 | CSS visual/rule expansion: gradients, transforms, filters, easing, at-rules, nesting as feature rows. | 700-950 impl/doc LOC; medium risk; split if generated LOC exceeds cap. | SK-V13 G1 feature matrix. |
| MP.NW5 | JSON 51-row strict sonic matrix: 17 corpora x parse_only/direct_to_struct/real_typed_struct. | 350-900 bench/gate/report LOC; high risk. | SK-V13 G5 and J.W1. |
| MP.NW6 | Lock 14 generated provider/config/sink/fact/flag/schema repair with CSS plus Sheets/BBNF-self negative controls. | 700-2000 LOC, cap 2600; high generality risk. | Lock 14, generated registry, non-JSON witnesses. |
| MP.NW7 | Regex/HIR fact extraction import boundary through `parse-that-regex` or equivalent facts. | 300-700 LOC; high integration risk. | D/H regex fact consumer and generated parser/resolver row. |
| MP.NW8 | Decision-engine replacement: bbnf-regex extraction, egraph language, guarded rewrites, active cost, CSP feasibility, and P1-P8 retirement/fail-closed compatibility. | 900-2200 LOC, cap 2800; high design risk. | SK-V13 G2, C.W4/C.W5, backend-shape rows. |
| MP.NW9 | AArch64 ASCII run-skip production split and zero-orphan disposition. | 120-220 narrow impl LOC; high if generalized. | SK-V13 G4, CSS scan-block consumer or measured rejection. |
| MP.NW10 | Fresh union-substrate variant or architectural block with material differential beyond REDRESS 96/97/98. | 700-1600 LOC, cap 2000; high risk. | SK-V13 G3, substrate/runtime/codegen/bench wave. |
| MP.NW11 | Sheets and BBNF-self negative-control/generalization witnesses for generated role facts and future grammar onboarding. | 250-800 LOC; medium-high risk. | Lock 14 and future grammar onboarding gates. |
| MP.NW12 | Rolling SOTA delta and no-demotion close gate: one row for every JSON row/plane and every CSS feature, with regression failure under G7 unless architectural-block/user re-pin is recorded. | 150-350 doc/gate LOC; high anti-paper-close risk. | BENCH rolling table, HANDOFF close gate, MASTER J.W1. |

MP.NW0-MP.NW11 are the T-P3 3B new-wave family, preserved with cost/risk/receiver routing (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:107-123`). MP.NW12 is added by Ω-D because the SK-V13 contract requires a rolling SOTA delta table and an indefatigable no-demotion close rule; those are not visible enough as a MASTER wave in the T-P3 3B allocation (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:124-130`, `restart/skinny/tranches/sk-v13/SYNTHESIS.md:187-200`).

## SK-V13 Full-SOTA Bar To Surface In MASTER

| SK-V13 gate | MASTER receiver | evidence |
|---|---|---|
| G1 CSS L4 parity | MP.NW2-MP.NW4 plus H.W6/J.W1. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:38-57`, `skinny/RESULTS.md:94` |
| G2 decision-engine fold | MP.NW8 plus C.W4/C.W5. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:59-71` |
| G3 union variant/block | MP.NW10 plus B/C/H substrate rows. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:73-82`, `skinny/REDRESS.md:2795-2925` |
| G4 zero aarch64 orphans | MP.NW9 plus H.W2/H.W2.5/H.W5. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:84-93`, `skinny/REDRESS.md:3766-3820` |
| G5 every JSON row above strict sonic | MP.NW5 plus H.W4/J.W1. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-110`, `skinny/RESULTS.md:5-45` |
| G6 Totality V1.1 and G-Omega | MP.NW0 plus Pass Omega CRUD/G-Omega. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:112-122` |
| G7 no demotion | MP.NW12 plus J.W1/J.W5. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:124-130` |

## G-Omega Blockers

| blocker | measurable condition before merge | citation |
|---|---|---|
| Omega convergence not complete | Ω-A through Ω-F must exist, CH1-CH6 must converge, and no unresolved critical/REVISE may remain before CRUD. | `restart/prompts/pass-contracts/PASS-OMEGA.md:76-95`, `restart/prompts/ORCHESTRATOR.md:104-123` |
| MASTER diff is proposal-only | This file and `master-plan-diff.md` are not edits to `restart/MASTER-PLAN.md`; CRUD-2 may apply only after challenge convergence and G-Omega. | `restart/prompts/pass-contracts/PASS-OMEGA.md:57-75`, `restart/prompts/pass-contracts/PASS-OMEGA.md:174-180` |
| Lock amendments remain gated | Any Lock 14/16/1 changes implied by MASTER waves require Ω-C output, challenge acceptance, and user G-Omega. | `restart/prompts/pass-contracts/PASS-OMEGA.md:96-110`, `restart/audit/totality/p3/G3-PRESENTATION.md:33-36` |
| SK-V13 W0 remains blocked | No implementation Wave 0, source edit wave, or RESULTS/REDRESS-writing wave may start until Totality V1.1 is ratified and G-Omega closes. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md:112-122`, `restart/audit/totality/p3/G3-PRESENTATION.md:46-47` |
| Witness cardinality unresolved | G-Omega must decide whether Lock 14 negative controls require both Sheets and BBNF-self or one negative-control witness plus CSS before implementation. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:164-173`, `restart/audit/totality/p3/3D-skinny-fold.md:119-128` |
| Scoped landings need labels | CRUD-2 must prevent B.W0/B.W4/H.W0/H.W5 and H.W1/H.W2 partials from reading as V1/root/campaign close. | `restart/audit/totality/p3/3B-master-plan-reconciliation.md:164-170`, `restart/MASTER-PLAN.md:526-533` |
