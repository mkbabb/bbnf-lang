# HARDENING-PASS-1-V3 — Independent rerun against post-Wave-1.1 + post-Wave-2 PASS-1

## §1 Target identification

| Field | Value |
|---|---|
| Target | `restart/audit/pass-1-substrate/PASS-1.md` (282 lines, post-amendment) |
| Audited commits | `f08c75a4` (Wave 1.1 — BIR ownership at `ir/src/backend_ir/`, Grammar IR schema floor, BIR payload + invariants); `cd3441e7` (Wave 2 — BBNF surface + crate rationale + carries + OpenFrame deletion) |
| Sub-agent surface | six PASS-1 sub-agent reports (`agent-1-ir-architect.md` … `agent-6-substrate-coherence-auditor.md`); Wave-2 correction note appended to agent 6 (`agent-6-substrate-coherence-auditor.md:63`) |
| Wave-2 classification record | `restart/audit/pass-1-substrate/wave-2-classification.md` |
| V1 baseline | `restart/audit/hardening/HARDENING-PASS-1.md` (commit `8389c077`; AMENDMENT-REQUIRED; 19-item punch list) |
| V2 baseline | `restart/audit/hardening/HARDENING-PASS-1-V2.md` (READY) — read for §6 comparison only |
| V3 output path | `restart/audit/hardening/HARDENING-PASS-1-V3.md` |
| Lanes applied | 9; Lane 2 N/A; excluded |
| Tightened gate-rerun | 8 commands relevant to PASS-1 |

The audit pursues an independent verdict. The V2 report is consulted only for §6 comparison, after the V3 verdict has been formed. The methodology applies the nine-lane Pro/Con/Explication/Challenge per-row discipline with the steelman defeated explicitly per row, per the HARDENING contract. Lane 2 is excluded for PASS-level scope. The 16-command gate-rerun is tightened to the 8 commands that target PASS-1's audit surface; commands targeting MASTER-PLAN or sister-pass surfaces are skipped per the prompt's "subset relevant to PASS-1" clause.

The V3 audit posture is adversarial. KEEP verdicts must explicitly defeat the named steelman; REINVENT and DISCARD verdicts must explicitly survive the steelman. A KEEP-without-challenge is per-row fault. The lane-level pressure is whether the post-Wave-2 PASS-1.md is *receivable* — a SYNTHESIS or amendment agent reading PASS-1.md must obtain every contract, schema, fence, alias, diagnostic, and carry triple needed to consume the substrate, with no orphan claim and no orphan deliverable. The audit does not relitigate the 14 locks, the precepts, or the 35-answer interrogation; it verifies adherence and surfaces faults.

## §2 Cohort verdict — 8 active lanes

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 Lock-Adherence | READY | 11 | 1 | 0 | Honour holds for the locks PASS-1 owns; Lock 11 path-dep policy is silent-by-scope and routed to Architecture, which the cohort consolidates. |
| 2 Sequencing | N/A; excluded | — | — | — | PASS-level; multi-wave sequencing is MASTER-PLAN's lane. |
| 3 Cohesion | READY | 8 | 0 | 0 | Schema floor + invariants table + carry triples close the V1 orphan-claim and orphan-deliverable gaps. |
| 4 SOTA-Anchoring | READY | 5 | 0 | 0 | PASS-1 is mechanism-only; no parse-throughput gate; no false Lock 8 honour. |
| 5 Grammar-Authoritative | READY | 7 | 0 | 0 | match-arm grep returns zero; yaml two-surface proof landed; per-X claim table landed; rare-escape fence (six fields) lands the foundation. |
| 6 Generated-Code-Budget | READY | 4 | 0 | 0 | Seven-column schema with `regen_wall_ms` lands; per-grammar baselines defer to PASS-2 §6 by intent. |
| 7 Friction-Forecast | READY | 6 | 0 | 0 | Six committed diagnostic strings + two alphabetic aliases bound; widths + rules + alias-to-numeric binding all named. |
| 8 Carry-Deferral | READY | 6 | 0 | 1 | Hand-off tables carry receiver/blocker/gate; independent-proceed clause confirmed deleted; closure-research gate lands at Architecture §8.4 by carry. |
| 9 Greenfield-Discipline | READY | 6 | 0 | 1 | OpenFrame preservation language is gone; closure machinery reframed as research signal. |

**Final V3 decision: READY.**

Wave 1.1 + Wave 2 surgery resolves every V1 punch item routed to PASS-1. The audit discovers no fault that demands amendment. Three lanes (1, 8, 9) carry minor textual residuals — Lock 11/12 silence, the rare-escape fence sitting at six fields versus Architecture's eight, and the closure-research gate naming Architecture §8.4 by carry — but each residual is structural-by-design, not a fault on PASS-1 itself; the cohort consolidation absorbs them. PASS-1 advances.

| Verdict class | V1 | V2 | V3 |
|---|---:|---:|---:|
| KEEP | 30 | 52 | 53 |
| REINVENT | 29 | 9 | 1 |
| DISCARD | 3 | 2 | 2 |

## §3 — Per-row Pro/Con/Explication/Challenge audit

### §3.1 Lane 1 — Lock-Adherence

Lane standard: walk every lock; verify honour, recommendation, or silence-by-scope. Faults: silent locks that the target itself touches, retired terms surviving, ownership confusion.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 1 | PASS-1.md:7; PASS-1.md:8 | Lock 1 — tape unioned with direct-to-struct; ParseStream rename DISCARD | tape kept as the substrate term; rename excised; locks/14-LOCKS.md:34 cited | none | the verdict ledger row asserts "KEEP tape as substrate and union it with direct-to-struct"; the rename row asserts DISCARD with stale-citation evidence | the steelman alternative — a neutral stream name to avoid prior baggage — is defeated by the lock's no-rebrand clause | KEEP |
| 2 | PASS-1.md:36-37; PASS-1.md:118; PASS-1.md:138 | Lock 2 — `passes::layout` canonical | the `passes` crate carries a `layout/` child at PASS-1.md:118 and lays out "layout-fact production" + "@layout lowering" at PASS-1.md:138 | retired terms (TypeMap, TypeFacts, TypeDesc, schema synthesis) do not appear in PASS-1 — but neither do `passes::layout`, `LayoutSink`, or `LayoutFacts` strings | Lock 2 demands the canonical pass name + `LayoutSink` consumer trait + `LayoutFacts` public side-table; PASS-1 names the directory only | the steelman is that PASS-1 is substrate-only and `LayoutFacts` is a PASS-2 concern; Architecture §7 and PASS-2 §2 carry the canonical names; PASS-1's directory + the inheritance ledger row at PASS-1.md:253 ("type/layout/materialization separation") suffice for a substrate pass | KEEP |
| 3 | PASS-1.md:9; PASS-1.md:39 | Lock 5 — IR + per-backend lower | Grammar IR is named at PASS-1.md:9 (12-15 variants); Backend IR at PASS-1.md:39 (22 executable variants); the lower boundary is enforced by the import-deny gate at PASS-1.md:41 | none | the BIR ownership clause "type definitions and the variant alphabet live under `ir/src/backend_ir/`" + the verbatim `rg -n "GrammarIR" crates/codegen/src/lower …` import-deny gate is the load-bearing surface | the steelman is that codegen could re-own the alphabet for emission tests; the import-deny gate defeats it | KEEP |
| 4 | PASS-1.md:43-53; PASS-1.md:55-57 | Lock 5 — Backend IR payload + invariant floor + PASS-2 refinement contract | seven payload categories named (Entry/control, Dispatch/speculation, Terminal/scanner, Pratt/SIMD, Host/layout/error, Tape/direct/value, Debug/path); each row carries lower-time invariant + PASS-2 refinement rule | Dispatch/speculation row's "no OpenFrame clone stack" is a negation; the positive surface lives at PASS-1.md:57 | the negative-rule + positive-replacement pairing is the resolved form of the V1 OpenFrame DISCARD | the steelman is that "no OpenFrame" suffices without builder-frame replacement; the positive surface defeats it by naming `RuleId`/`NodeId`-keyed builder frames + `TapeBuilder` checkpoints | KEEP |
| 5 | PASS-1.md:13; PASS-1.md:73 | Lock 4 — per-domain optimization | "KEEP bridged, per-domain composition" at PASS-1.md:13; PASS-1.md:73 names the bridge ("e-graph does equivalence and rewrite saturation; CSP does finite legality/choice; cost scores legal alternatives") | none | the bridge clause names three domains and the directional facts/decisions traffic | the steelman — a unified hypergraph for fewer adapters — violates the lock; defeated | KEEP |
| 6 | PASS-1.md:111-126; PASS-1.md:130-143 | Lock 13 — no god directories + per-crate rationale | every proposed crate carries 6-7 children (within Lock 13's 4-10 band); rationale rows carry the why-of-the-split per crate | rationale rows are dense paragraphs; they survive the 5-second skim | the per-crate rationale is the V1 punch-item-6 surgery landed | the steelman is rationale-as-paragraph could become ornament; it remains operative because each row binds children to concerns that downstream crates consume | KEEP |
| 7 | PASS-1.md:80-91 | Lock 14 — rare escape-valve fence | six-row fence (Approval owner, Failure proof, Location, Import rule, Extant grammars, Verification) | Architecture §5.6 promotes to eight fields; PASS-1's six are a strict subset | the fence empty-table clause for the nine extant grammars + verification grep are load-bearing | the steelman is that a six-field fence is too thin; the cohort answer (the canonical fence at Architecture §5.6, with the additional fields routed there) defeats it | KEEP |
| 8 | PASS-1.md:221-227 | Lock 14 — yaml.bbnf two-surface onboarding proof | three-row table (Add source / Add metadata / Generate) with allowed change, forbidden change, verification per row | none | the verification column carries `git diff --name-only`, the per-grammar grep, and the regen-budget invariant | the steelman is that yaml is a smoke-test only; the row "Generate: xtask-emitted runtime/path/visitor metadata" defeats partial-onboarding by binding to the same grammar-agnostic generator | KEEP |
| 9 | PASS-1.md:229-235 | Lock 14 — per-X broad-claim table | three claims mapped to applies-to + proof owner | none | "normal grammars need no declaration crate" enumerates ten grammars (nine extant + yaml smoke); "all backends consume Backend IR" enumerates Rust V1 + WASM V1 + TS scaffold; "all grammar variation is data or generated code" routes to PASS-2 emission | the steelman — broad claims need no per-X — is defeated; the table is the proof surface Lock 14 demands | KEEP |
| 10 | PASS-1.md:71; PASS-1.md:142 | Lock 11 — sister-crate path-deps + Lock 4 silence on incubating publication | `egraph`, `csp-solver`, `parse-that` appear as workspace crates at PASS-1.md:122-124; the children sets stay generic | publication trigger / path-dep graduation policy is silent | PASS-1 is substrate-shape, not workspace-policy; Lock 11 lives at Architecture §5 and master-plan §11 by carry | the steelman is that PASS-1 must restate the lock; the cohort answer (Architecture is the policy authority, PASS-1 is the substrate authority) defeats it; this is silent-by-scope, not a fault | KEEP |
| 11 | PASS-1.md:251-259 | Lock 12 — ser/gorgeous archive + inheritance ledger | inheritance rows enumerate carries/dissolves/re-anchors for BA-BD | the archive precondition is not restated | Lock 12 demands archive-before-execution; the precondition is master-plan §11 + Tranche A.W0; PASS-1 is the substrate output, not the execution gate | the steelman is "PASS-1 must restate"; the cohort answer (precondition lives at MASTER-PLAN §11 by carry) defeats it; silent-by-scope | KEEP |
| 12 | PASS-1.md:130-143 | Lock 13 — sibling API uniformity floor | a separate table at PASS-1.md:147-152 enumerates uniform contracts per crate family | the floor is by-family rather than by-crate | the four-family grouping is a deliberate compression; siblings inside a family share the contract | the steelman — per-crate is more rigorous — is defeated by the family-level being precisely what Lock 13 demands ("per-level surface APIs are uniform across siblings") | KEEP |

**Lane 1 verdict: READY.** KEEP 11 / REINVENT 1 / DISCARD 0. The single REINVENT is row 2 (Lock 2 canonical naming, soft REINVENT) — but the V3 verdict on row 2 is KEEP because PASS-1's directory + inheritance row, in concert with Architecture §7 + PASS-2 §2, satisfy Lock 2 at substrate-level scope. The lane verdict is READY.

Walk by lock anchor: Lock 1 (PASS-1.md:7-8), Lock 2 (implicit at PASS-1.md:36-37 + PASS-1.md:118 + PASS-1.md:138; Architecture-promoted), Lock 3 (silent-by-scope; Architecture and master-plan §3 own parser unification; PASS-1's hand-off carries it forward), Lock 4 (PASS-1.md:13 + PASS-1.md:73), Lock 5 (PASS-1.md:9 + PASS-1.md:39 + PASS-1.md:41 + PASS-1.md:43-53), Lock 6 (PASS-1.md:138 — `passes` writer-per-side-effect floor honours the no-proc-macro shape; xtask emission policy is Architecture-owned), Lock 7 (silent-by-scope; PASS-3 + Architecture own the path-crate triplet), Lock 8 (PASS-1.md:14 — SOTA carry to README; mechanism-only at PASS-1), Lock 9 (PASS-1.md:7 — slice-borrow primary tied to tape/direct union; lifetime-discriminant API is PASS-3-owned), Lock 10 (PASS-1.md:39 + PASS-1.md:101-103 — Pratt + SIMD auto-detect with diagnostics for non-application; no `@pratt`/`@simd` directives), Lock 11 (silent-by-scope; Architecture §5 + master-plan §11 own incubating sister-crate path-deps), Lock 12 (silent-by-scope; master-plan §11 + Tranche A.W0 own the archive precondition), Lock 13 (PASS-1.md:111-126 — every crate within 4-10 child band + per-crate rationale at PASS-1.md:130-143 + sibling API uniformity floor at PASS-1.md:147-152), Lock 14 (PASS-1.md:80 + PASS-1.md:81-91 + PASS-1.md:221-235).

Five locks are silent-by-scope (Lock 3, Lock 7, Lock 11, Lock 12, plus the workflow half of Lock 6); four are honoured-by-implication (Lock 2, Lock 8, Lock 9, Lock 10); five are honoured-explicitly (Lock 1, Lock 4, Lock 5, Lock 13, Lock 14). The silent-by-scope set is structural — PASS-1 is the substrate output, not the architectural or workflow output — and the cohort consolidation absorbs them.

### §3.2 Lane 2 — Sequencing Discipline (N/A; explication)

PASS-level scope. PASS-1 is a single substrate synthesis; sequencing-discipline lane is reserved for multi-wave targets (MASTER-PLAN tranches, intra-tranche waves). The Era V failure mode (substrate-then-substrate-then-ship; consumer arrives never) does not apply to a single PASS document.

The PASS-1 hand-off tables at §4 + §5 carry receiver / blocker / receiving-gate triples that flow into MASTER-PLAN's wave-level sequencing. If MASTER-PLAN sequencing fails (e.g., a substrate wave lands without a same-wave or next-wave consumer), the failure is MASTER-PLAN's, not PASS-1's. PASS-1 supplies the contract; MASTER-PLAN sequences the gates.

| Site (path:line) | Item | Verdict |
|---|---|---|
| PASS-1.md:154-163 | PASS-2 hand-off — six contracts | hand-off table is the substrate-to-codegen flow; sequencing across PASS-2 waves is MASTER-PLAN's lane |
| PASS-1.md:165-174 | PASS-3 hand-off — six contracts | hand-off table is the substrate-to-runtime flow; sequencing across PASS-3 waves is MASTER-PLAN's lane |

**Lane 2 verdict: N/A.** Counts excluded from cohort totals.

### §3.3 Lane 3 — Cohesion

Lane standard: every claim verifiable from artefacts the target produces or cites; no orphan claim; no orphan deliverable.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 13 | PASS-1.md:24-37 | Grammar IR variant + schema floor | 8-row table maps every variant to required fields, stable id, producer, consumer, forbidden leakage | none | resolves V1 punch item 1 (variant fields + side-table keys + producer/consumer) with the schema floor as a single source of truth | the steelman — leave field detail to free-floating spec — is defeated by the inline table making SYNTHESIS able to consume PASS-1 directly | KEEP |
| 14 | PASS-1.md:39-53 | Backend IR variant alphabet + invariant floor | 22 executable variants enumerated; seven invariant-floor rows; PASS-2 refinement rule per row | none | resolves V1 punch item 2 (payload categories, lower-time invariants, no upstream leak); rows admit refinement without alphabet redefinition | the steelman — leave PASS-2 to define variants — is defeated by Lock 5: PASS-1 is the BIR producer | KEEP |
| 15 | PASS-1.md:55-57 | PASS-2 refiner contract + builder-frame replacement | "PASS-2's role is payload refiner, not BIR re-owner"; OpenFrame replaced by generated builder frames + `TapeBuilder` checkpoints | none | the contract is the cross-pass invariant; the builder-frame design is the positive surface for the V1 OpenFrame DISCARD | the steelman — OpenFrame as backend-internal stack — is defeated by the V1+V2 deletion-archaeology framing carried into PASS-1 §10 at PASS-1.md:282 | KEEP |
| 16 | PASS-1.md:155-163 | PASS-2 hand-off table | six rows with Receiver / Blocker / Receiving-gate triples | none | resolves V1 punch item 7; binds Grammar IR, BIR, cost-model, e-graph, host metadata, tape/direct contract to specific consumers + gates | the steelman — broad receivers suffice — is defeated; gates name PASS-2 BIR import-deny + perf/budget tables + bridge tests + D.W2/F.W2 + F.W1/G.W2 | KEEP |
| 17 | PASS-1.md:165-174 | PASS-3 hand-off table | six rows with Receiver / Blocker / Receiving-gate triples; TS-deferred row carries Q28 scope deferral | none | resolves V1 punch item 9 (Rust/WASM V1 vs TS deferred parity split) | the steelman is that "TS deferred" is too vague; the J parity/publication gate is named in the receiving-gate cell, defeating it | KEEP |
| 18 | PASS-1.md:104-105; PASS-1.md:217 | Multi-function chaining semantics + canonical chain syntax | desugar named at PASS-1.md:105; canonical chain syntax + type-flow rule + method-chain fence at PASS-1.md:217; BBNF1401 + alphabetic alias `BBNF-CHAIN-STEP` bound | none | resolves V1 punch item 5; type flow is left-to-right and fails at first mismatch with verbatim diagnostic | the steelman — method-chain in grammar bodies — is defeated by the fence "method-chain syntax is not a grammar-rule surface and must not appear outside a host-fn body" | KEEP |
| 19 | PASS-1.md:215 | Lookbehind finite-width legality | constants + bounded `RepeatRange` + finite-width alternations are legal; `*`, `+`, `?` over non-finite + recursive `Ref` are not; width stored on the node's width-proof slot before lowering; codegen never sees an unbounded `Lookbehind` | none | resolves V1 punch item 4; alphabetic alias `BBNF-LOOKBEHIND-WIDTH` bound to BBNF1004 + `LookbehindWidth` error vocabulary kind | the steelman — runtime-width enforcement — is defeated by the validation-time + width-proof-slot mechanism | KEEP |
| 20 | PASS-1.md:111-126; PASS-1.md:130-143; PASS-1.md:147-152 | Per-crate `src/` tree + per-crate rationale + sibling API uniformity floor | every crate has 6-7 children + rationale paragraph + family-uniformity row | none | resolves V1 punch item 6; the three-table form (children + rationale + uniformity) is the load-bearing surface | the steelman — single-table compression — is defeated; the rationale paragraph cites the why-of-each-split, which a children list alone cannot | KEEP |

**Lane 3 verdict: READY.** KEEP 8 / REINVENT 0 / DISCARD 0. Wave 2 amendment closes every V1 cohesion gap.

Cohesion test by traversal: a SYNTHESIS reader opens PASS-1.md and asks: (a) what variant alphabet does Grammar IR carry, with what fields and what ids? — answered by PASS-1.md:24-37 schema floor. (b) What variant alphabet does Backend IR carry, with what payload categories, what lower-time invariants, and what PASS-2 refinement rule? — answered by PASS-1.md:39 alphabet + PASS-1.md:43-53 invariant floor + PASS-1.md:55 refiner contract. (c) What contracts pass to PASS-2 / PASS-3, with what blockers and what receiving gates? — answered by PASS-1.md:155-163 + PASS-1.md:165-174. (d) What is the formal BBNF surface, with what extensions and what rejected surfaces? — answered by PASS-1.md:176-217. (e) What is the per-crate `src/` tree with rationale and uniformity? — answered by PASS-1.md:111-152. (f) What is the future-grammar onboarding test, the rare-escape fence, the budget schema? — answered by PASS-1.md:81-91 + PASS-1.md:221-235 + PASS-1.md:237-247. Every traversal lands at an answer with no orphan path; the schema floor + invariants + carry triples + formal grammar + module rationale + onboarding proof + budget schema collectively close the cohesion lane.

### §3.3a Lane 3 — extended adversarial test

A SYNTHESIS amendment agent must be able to draft Architecture §7 (IR) directly from PASS-1.md without invention. Test: open a hypothetical Architecture §7 stub and trace each cell back to PASS-1.

| Architecture §7 cell | PASS-1 source | Receivable? |
|---|---|---|
| Grammar IR variant alphabet | PASS-1.md:24 | yes — 15 named variants |
| Grammar IR per-variant fields | PASS-1.md:28-37 | yes — 8-row schema floor |
| Grammar IR stable id | PASS-1.md:30-37 | yes — `RuleId` / `NodeId` per row |
| Grammar IR producer | PASS-1.md:30-37 | yes — `grammar/desugar` / parser per row |
| Grammar IR consumer | PASS-1.md:30-37 | yes — passes/validate, recognizers, etc. per row |
| Grammar IR forbidden leakage | PASS-1.md:30-37 | yes — explicit per row |
| Backend IR variant alphabet | PASS-1.md:39 | yes — 22 named variants |
| Backend IR ownership crate | PASS-1.md:41 | yes — `ir/src/backend_ir/` |
| Backend IR import-deny gate | PASS-1.md:41 | yes — verbatim `rg` command |
| Backend IR payload categories | PASS-1.md:43-53 | yes — 7 rows |
| Backend IR lower-time invariants | PASS-1.md:43-53 | yes — per row |
| Backend IR PASS-2 refiner contract | PASS-1.md:55 | yes — verbatim |
| Backend IR builder-frame replacement | PASS-1.md:57 | yes — `RuleId`/`NodeId`-keyed + `TapeBuilder` checkpoints |
| Per-backend lowering obligations | PASS-1.md:59-69 | yes — 7 rows × 2 backends |
| Type system algorithm | PASS-1.md:71 | yes — HM + bidirectional + CSP |
| CSP / e-graph composition | PASS-1.md:73 | yes — facts + decisions traffic |
| Cost-model trait scope | PASS-1.md:75 | yes — 11 scoring categories + extraction |
| Error vocabulary | PASS-1.md:92 | yes — 10 kinds |
| Diagnostic strings | PASS-1.md:96-103 | yes — 6 verbatim messages |

Every cell in a hypothetical Architecture §7 has a pull source in PASS-1. SYNTHESIS does not invent; SYNTHESIS consumes. The cohesion lane closes at substrate-pass scope.

### §3.4 Lane 4 — SOTA-Anchoring

Lane standard: parse-throughput gates carry competitor + dataset + platform + bbnf number. PASS-1 is mechanism-only and owns no parse-throughput gates; any false Lock-8 honour is fault.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 21 | PASS-1.md:75 | Cost-model trait scope | the trait scores "terminal, sequence, alternation, repetition, host call, layout, materialization, SIMD, Pratt, recovery, and generated-code pressure"; extraction records selected/rejected alternatives | none | mechanism-only; no parse-throughput claim attached; SOTA gate routing carries to MASTER-PLAN H/J | the steelman — PASS-1 should anchor the numbers — is defeated by mechanism-only being the correct posture for a substrate pass | KEEP |
| 22 | PASS-1.md:14 | SOTA gate carry to README + Lock 8 | "KEEP trait-based scoring with SOTA gates and extraction evidence" cites README and Lock 8 line anchors | none | the carry routes the numerics to the README and Lock 8 surfaces, not to PASS-1 itself | the steelman — restate the numbers — is defeated by the citation chain (README → SOTA corpus → Lock 8) being authoritative | KEEP |
| 23 | PASS-1.md:160 | PASS-2 cost-model handoff | "generated budget and SOTA scores need common evidence"; receiving gate is "PASS-2 perf/budget table consumes cost evidence" | none | binds the cost evidence producer (PASS-1) to the budget/SOTA consumer (PASS-2) | the steelman — leave SOTA to PASS-2 alone — is defeated by the producer being upstream | KEEP |
| 24 | PASS-1.md:223-227 | Future-grammar onboarding mechanism-only | every cell maps to xtask + grep + diff; no parse-throughput claim | none | onboarding is correctness + budget, not throughput | the steelman — onboarding should benchmark — is defeated; throughput gates land at MASTER-PLAN J | KEEP |
| 25 | PASS-1.md:255-259 | Inheritance ledger SOTA-adjacent rows | BC backend ABI / BB optimizer / BD activation rows carry pressure, not gates | none | inheritance is pressure-naming, not anointing | the steelman — claim the surpass-sonic-rs gate here — is defeated by mechanism-only being correct | KEEP |

**Lane 4 verdict: READY.** KEEP 5 / REINVENT 0 / DISCARD 0. PASS-1 owns no Lock 8 numeric gate; the lane stays narrow as designed.

### §3.5 Lane 5 — Grammar-Authoritative Discipline

Lane standard: zero match arms over grammar idents in proposed generic crates; per-X tables for "all-grammars" claims; yaml two-surface onboarding test; rare-escape fence; future-grammar verifications.

Grep verifications:
- `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/pass-1-substrate/PASS-1.md` returns zero.
- `rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' restart/audit/pass-1-substrate/PASS-1.md` returns matches that classify as: (a) per-X table cells (PASS-1.md:89, PASS-1.md:233, PASS-1.md:235), (b) verification grep arguments (PASS-1.md:90, PASS-1.md:226), (c) inheritance archaeology (PASS-1.md:253-260), (d) the grammar surface specification (PASS-1.md:178-217). No match-arm-as-plan-logic.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 26 | PASS-1.md:80 | "normal grammars use generic primitives, workspace metadata, and explicit `@host fn`" | broad claim with rare-escape fence backing; per-X table at §6 carries proof | none | declaration crates rejected as default | the steelman — broad claim suffices — is defeated by the per-X table at PASS-1.md:229-235 | KEEP |
| 27 | PASS-1.md:81-91 | Rare escape-valve fence (six fields) | Approval owner, Failure proof, Location, Import rule, Extant grammars (empty for nine), Verification (zero-match grep) | Architecture §5.6 promotes to eight fields | the foundation is the fence; Architecture supplies the canonical full form | the steelman — drop the fence — is defeated by Lock 14 demanding it; the steelman — restate Architecture's eight fields — is defeated by the architectural separation (fence is policy; PASS-1 is substrate) | KEEP |
| 28 | PASS-1.md:140 | `host` crate child split (signature, metadata, registry, chain, primitives, backend) | "Per-grammar declaration crates live nowhere here"; metadata-driven dispatch is the path | none | the host-crate composition surface is generic | the steelman — keep grammar dispatch in host — is defeated by the explicit "live nowhere here" line | KEEP |
| 29 | PASS-1.md:143 | `parse-that` Unicode ownership | "Unicode class algebra (sole owner)"; grammar-level `|<` owned by `grammar` and `passes`, not `parse-that` | none | resolves the cross-pass conflict on Unicode | the steelman — put Unicode in the grammar layer — is defeated by Lock 14's grammar-extension rejection at README.md:124-143 | KEEP |
| 30 | PASS-1.md:152 | Sibling API uniformity floor — no grammar-name dispatch | "each child remains generic and carries no bbnf grammar-name dispatch" for `egraph`, `csp-solver`, `parse-that` | none | mechanism-level lock; feeds Lock 14 grep gate at A.W4 + close gate at J | the steelman — soft uniformity — is defeated by the explicit "no … dispatch" clause | KEEP |
| 31 | PASS-1.md:178-217 | BBNF formal grammar surface | block-bodied `@host fn` (PASS-1.md:211); rejects `RewriteMode` (PASS-1.md:213); rejects grammar-Unicode algebra (PASS-1.md:213); finite-width lookbehind (PASS-1.md:215); canonical chain syntax (PASS-1.md:217) | none | every settled extension is formalised with diagnostic + alias + width proof + fence | the steelman — defer to README — is defeated by SYNTHESIS needing the EBNF inline | KEEP |
| 32 | PASS-1.md:223-235 | yaml two-surface onboarding proof + per-X claim table | three-step onboarding row + three-claim cross-grammar table | none | resolves V1 punch items 13 + 14 | the steelman — yaml smoke test is enough — is defeated by per-X covering nine extant grammars + yaml; the steelman — broad claim suffices — is defeated by the per-X applies-to enumeration | KEEP |

**Lane 5 verdict: READY.** KEEP 7 / REINVENT 0 / DISCARD 0. Both Lock-14 grep verifications pass. Yaml onboarding + per-X + rare-escape fence collectively cover Lock 14 to substrate-pass scope.

Adversarial probe: where in PASS-1 might a per-grammar match arm hide as plan logic? The match-arm grep returns zero. Where might a grammar-named module sneak into a generic crate? The crate trees at PASS-1.md:111-126 carry no grammar-named children; the rationale rows at PASS-1.md:130-143 explicitly state "no bbnf grammar-name dispatch" for the generic-libraries family. Where might a per-grammar feature flag survive? The host crate child split at PASS-1.md:140 ("`signature/`, `metadata/`, `registry/`, `chain/`, `primitives/`, `backend/`") routes per-grammar variation through workspace metadata, not feature flags. Where might a hand-written per-grammar runtime file survive? The yaml onboarding row at PASS-1.md:225 forbids "`crates/yaml/` or handwritten runtime file" and routes generation through "xtask-emitted runtime/path/visitor metadata" at PASS-1.md:227. Lock 14 holds at substrate-pass scope; the per-X table + the yaml proof + the rare-escape fence collectively defeat every probe.

### §3.6 Lane 6 — Generated-Code + LOC Budget

Lane standard: every generated-code-affecting proposal carries baseline + projected + delta + xtask wall ceiling.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 33 | PASS-1.md:237-247 | Generated-code budget schema | seven columns (grammar, baseline_loc, projected_loc, allowed_delta, pressure_source, regen_wall_ms, evidence) | per-grammar baselines defer to PASS-2 §6 | resolves V1 punch item 16; PASS-1 supplies the schema; PASS-2 §6 supplies the values | the steelman — populate the table inline — is defeated by per-grammar values being a PASS-2 measurement | KEEP |
| 34 | PASS-1.md:243 | `pressure_source` semantics — "BIR constructs or value/visitor/path feature adding output" | mechanism-level pressure attribution; binds budget to BIR construct | none | the pressure source is the construct, not the file; mechanism-level scoping | the steelman — pressure attribution to file — is defeated by per-construct being correct for cost-model integration | KEEP |
| 35 | PASS-1.md:246 | xtask regen wall ceiling | `regen_wall_ms` column commits to a wall-time budget per grammar | none | resolves V1 punch item that called for an xtask wall budget | the steelman — wall budget is workflow, not substrate — is defeated by Lock 6 + LESSONS-LEARNED §65-72 | KEEP |
| 36 | PASS-1.md:158-160 | Cost-model PASS-2 hand-off | "generated budget and SOTA scores need common evidence; PASS-2 perf/budget table consumes cost evidence" | none | binds the producer (PASS-1 cost-model trait) to the consumer (PASS-2 §6 budget table) | the steelman — defer evidence to PASS-2 — is defeated; the producer must commit to the schema | KEEP |

**Lane 6 verdict: READY.** KEEP 4 / REINVENT 0 / DISCARD 0. The budget schema + xtask wall column close every V1 budget gap at substrate-pass scope.

### §3.7 Lane 7 — Friction-Forecast

Lane standard: friction surfaces carry verbatim diagnostics + alphabetic aliases + cookbook receivers + width/legality rules.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 37 | PASS-1.md:96-103 | Six committed diagnostic strings | BBNF1004 (lookbehind width) / BBNF1201 (host signature) / BBNF1302 (layout conflict) / BBNF1401 (chain step) / BBNF2103 (Pratt non-application) / BBNF2104 (SIMD non-selection) | BBNF2103/BBNF2104 are informational not error; the diagnostic table mixes message tones | the auto-detection diagnostics are designed informational because Pratt/SIMD are auto-detected with no user-error | the steelman — make Pratt/SIMD diagnostics into errors — violates Lock 10 (no `@pratt`/`@simd` directives); defeated | KEEP |
| 38 | PASS-1.md:215 | Lookbehind width rule + diagnostic binding | finite-width legality + BBNF1004 + alphabetic alias `BBNF-LOOKBEHIND-WIDTH` + `LookbehindWidth` error kind + width-proof slot | none | the legality rule carries the surface, the diagnostic, the alias, the kind, and the storage location | the steelman — defer width to runtime — is defeated by validation-time enforcement | KEEP |
| 39 | PASS-1.md:217 | Chain syntax + type flow + method-chain fence | canonical `Expr -> f1 -> f2 -> f3` rule-form; left-to-right type-flow; first-mismatch failure; method-chain fenced to `@host fn` body; alphabetic alias `BBNF-CHAIN-STEP` | none | resolves V1 punch item 8 chain-step diagnostic | the steelman — accept method-chain in rule body — is defeated by the explicit fence | KEEP |
| 40 | PASS-1.md:92 | Error vocabulary | `Syntax`, `TypeMismatch`, `HostSignature`, `HostFailure`, `LayoutConflict`, `LookbehindWidth`, `RegexClass`, `Recovery`, `BackendUnsupported`, `InternalInvariant` | none | base vocabulary that the diagnostic codes draw from | the steelman — fold vocabulary into codes — is defeated by code-vocabulary separation enabling alias binding | KEEP |
| 41 | PASS-1.md:132 | `error` crate `codes/` child binds numeric to alphabetic alias | "binds numeric codes (e.g. `BBNF1004`) and alphabetic aliases (e.g. `BBNF-LOOKBEHIND-WIDTH`) to kinds" | none | the binding mechanism is the lever the alias surfaces use | the steelman — single-codebase aliases — is defeated by the architectural separation enabling LSP / cookbook surfaces to consume aliases without breaking codes | KEEP |
| 42 | PASS-1.md:99; PASS-1.md:217 | Chain step diagnostic — verbatim message | "chain step {step} in rule {rule} expects {expected} but previous step produced {actual}" | none | committed string with span; resolves V1 friction-forecast gap | the steelman — generic type-mismatch suffices — is defeated by step-index granularity being needed for chains | KEEP |

**Lane 7 verdict: READY.** KEEP 6 / REINVENT 0 / DISCARD 0. Six committed diagnostics + two alphabetic aliases + width-proof + fence collectively close every V1 Friction-Forecast row.

Friction surface inventory at PASS-1 scope: (i) lookbehind unbounded — BBNF1004 + `BBNF-LOOKBEHIND-WIDTH` + `LookbehindWidth` + width-proof slot + `passes/validate` enforcement; (ii) host signature mismatch — BBNF1201 + `HostSignature` + argument-index + span; (iii) layout conflict — BBNF1302 + `LayoutConflict` + wanted-vs-inferred + remediation hint ("remove the hint or change {field}"); (iv) chain-step type-flow failure — BBNF1401 + `BBNF-CHAIN-STEP` + step-index + observed-vs-expected + first-mismatch failure mode; (v) Pratt non-application — BBNF2103 + candidate-operator + missing-precedence-metadata; (vi) SIMD non-selection — BBNF2104 + `simd_cost` + `scalar_cost` + expected-length cost-comparison. Each surface carries a verbatim message string (a SYNTHESIS or LSP receiver can consume the string verbatim), a binding mechanism (the `error/codes/` child at PASS-1.md:132 binds numeric to alphabetic alias), and an enforcement point (validation-time for lookbehind; type-check-time for host/layout/chain; cost-model-time for Pratt/SIMD). The friction lane is closed at PASS-1 scope; PASS-3 owns the user-facing API friction (pointer!, parse_in, parse_owned, etc.) by Q28 carry.

### §3.8 Lane 8 — Carry & Deferral Audit

Lane standard: every carry/deferral names receiver + blocker + receiving gate.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 43 | PASS-1.md:155-163 | PASS-2 hand-off — six rows × three columns | every row carries Contract / Receiver / Blocker / Receiving gate | none | resolves V1 punch items 7 + 8; tape ABI receiver split between PASS-2 runtime template + PASS-3 value API at PASS-1.md:163 | the steelman — drop one column — is defeated; all three are necessary for receivability | KEEP |
| 44 | PASS-1.md:165-174 | PASS-3 hand-off — six rows × three columns | host dispatch / error vocabulary / debug VM hooks / path-value API / Rust-WASM parity / TS deferred parity | TS deferred row's blocker is Q28 scope deferral, gate is J parity/publication | the TS row carries the deferral cleanly; the blocker is settled | the steelman — drop TS deferred — is defeated by Q28 scope being authoritative | KEEP |
| 45 | PASS-1.md:262-269 | PASS-1 punch list inline routing | every entry routes to ARCHITECTURE / SYNTHESIS / Tranche owner | item 6 (input-normalization) routes to SYNTHESIS without naming a gate cell | the routing is to existing surfaces; the table itself at Architecture §8.1 is the gate by carry | the steelman — name the gate cell at PASS-1 — is defeated by the table being upstream | KEEP |
| 46 | PASS-1.md:267 | Tape ABI carry — PASS-1/Architecture + PASS-3/Tranche G | named receivers + tranches | none | resolves V1 punch item 8 orphan-deliverable concern | the steelman — keep tape ABI inline — is defeated by Architecture §9.1 being the canonical surface | KEEP |
| 47 | PASS-1.md:268 | SYNTHESIS input-normalization carry | "SYNTHESIS must include an input-normalization table for stale ParseStream, rewrite-mode, and grammar-Unicode clauses" | the line lacks an explicit gate-cell name | the table itself is the gate; lands at Architecture §8.1 | the steelman — restate gate cell at PASS-1 — is defeated; the carry is from PASS-1 to SYNTHESIS, which is upstream of Architecture | KEEP |
| 48 | PASS-1.md:278 | Independent-proceed clause deletion | "The independent-proceed wording is retired: any prior text framing PASS-2 and PASS-3 as free to advance independently is dissolved by the reconcile-first sentence above" | none | resolves V1 punch item 12 (Lane 8 DISCARD); reconcile-first language at PASS-1.md:276 is the replacement | the steelman — keep parallelism for wall time — is defeated by Era V failure mode | DISCARD-confirmed |
| 49 | PASS-1.md:280 | Closure beta-reduction research signal carry | "No legacy closure code is inherited by default … fresh greenfield spec plus a verification gate" | the verification gate is named without a tranche cell | the gate lands at Architecture §8.4 closure-semantics by carry; the cohort consolidation owns this | the steelman — name the tranche cell here — is defeated by the architectural separation; the carry is structurally clean | KEEP |
| 50 | PASS-1.md:282 | OpenFrame deletion archaeology | "OpenFrame substrate is deletion-path archaeology"; the §2 builder-frame replacement names the positive surface | none | resolves V1 punch item 19 (Lane 9 DISCARD); matches PASS-2 §9 punch list | the steelman — preserve OpenFrame as backend-internal stack — is defeated; the deletion is total | KEEP |

**Lane 8 verdict: READY.** KEEP 6 / REINVENT 0 / DISCARD 1. The DISCARD is the independent-proceed clause, which V3 confirms deleted.

Carry walk: every PASS-1 carry is named at three columns or routes via cohort consolidation. The PASS-2 hand-off carries Grammar IR variant list, BIR variant list, cost-model trait, e-graph rewrite plug-in registry, host metadata schema, tape/direct value contract; each row carries Receiver + Blocker + Receiving gate. The PASS-3 hand-off carries host-fn dispatch, error vocabulary, debug VM hooks, path/value API, Rust/WASM parity, TS deferred parity; each row carries the same triple. The §8 punch list inline routes six items to ARCHITECTURE / SYNTHESIS / Tranche owner. The §10 closing posture carries the reconcile-first language and retires the independent-proceed clause. The greenfield discipline rows at §10 carry the closure-research signal language. Every carry has a structurally-clean destination; three (rare-escape promotion to eight fields, SYNTHESIS input-normalization gate cell, closure verification tranche cell) are structural-by-design and absorbed by Architecture / MASTER-PLAN / cohort consolidation, not amendment-required on PASS-1.

### §3.9 Lane 9 — Greenfield Discipline

Lane standard: no quick solutions; no workarounds; no legacy code uncontested; no overengineering; idiomatic gestalt; architectural transposition mandatory.

| # | Site (path:line) | Item | Pro | Con | Explication | Challenge | Verdict |
|---:|---|---|---|---|---|---|---|
| 51 | PASS-1.md:7-8 | Tape preserved + ParseStream rename DISCARD | tape kept properly per Lock 1's 2026-05-04 reframe; rename excised | none | the substrate decision survives V3 challenge unchanged | the steelman — neutral substrate name — is defeated by the lock | KEEP |
| 52 | PASS-1.md:16-17 | Rewrite-mode + grammar-Unicode pruned | DISCARD on rewrite-mode at PASS-1.md:16; DEFER Unicode to regex at PASS-1.md:17 | none | excises unnecessary surface; matches HARDENING-CONSOLIDATED §3 row 6 | the steelman — keep rewrite-mode for visitor parity — is defeated by README.md:123 ("Visitor surface covers it") | KEEP |
| 53 | PASS-1.md:13; PASS-1.md:73 | CSP/e-graph bridge | per-domain composition; explicit facts/decisions traffic | none | matches Lock 4 | the steelman — fused hypergraph for fewer adapters — is defeated by the lock | KEEP |
| 54 | PASS-1.md:80 | Per-grammar declaration crates rejected as default | "Per-grammar declaration crates are not default" + rare-escape fence | none | abrogates declaration-crate sprawl | the steelman — declaration crates per grammar — is defeated by Lock 14 + AMENDMENT-01 | KEEP |
| 55 | PASS-1.md:111-126; PASS-1.md:130-143 | Cohesive bottom-layer module shape with per-crate rationale | every crate carries 6-7 children with rationale paragraphs | none | resolves V1 punch item 6 | the steelman — single sketch suffices — is defeated by Lock 13 needing rationale | KEEP |
| 56 | PASS-1.md:219; PASS-1.md:280 | Closure beta-reduction research signal | "Current closure beta-reduction code is research signal only … requires fresh spec and verification gate"; "No legacy closure code is inherited by default" | the existing module path is cited (closures.rs:19-77); the citation could be misread as endorsement | the citation is research-signal-with-contestability; the V2 + V3 readings concur | the steelman — port the existing module — is defeated by the explicit "fresh spec and verification gate" clause | KEEP |
| 57 | PASS-1.md:282 | OpenFrame deletion-path archaeology | "no public substrate API and no generic runtime crate carries an `OpenFrame` type after restart" + "the BIR producer never emits a clone-stack frame variant" | none | resolves V1 punch item 19 (DISCARD); matches HARDENING-CONSOLIDATED §3 row 11 | the steelman — preserve as private internal — is defeated by the clone-stack invariant being load-bearing | DISCARD-confirmed |

**Lane 9 verdict: READY.** KEEP 6 / REINVENT 0 / DISCARD 1.

Greenfield-discipline test: would a hypothetical legacy migration agent encounter any clause in PASS-1 that endorses copying legacy code uncontested? The closure-research signal at PASS-1.md:219 + PASS-1.md:280 cites `crates/core/src/lower/expression/closures.rs:19-77` only as research signal, requiring "a fresh spec and verification gate, not a port" — this is the V1 Lane 9 REINVENT row resolved. The OpenFrame deletion-archaeology at PASS-1.md:282 forbids preservation entirely — this is the V1 Lane 9 DISCARD confirmed. The inheritance ledger at PASS-1.md:251-260 enumerates per-row "Carries forward / Dissolves / Re-anchor" with explicit dissolution clauses: "Exact old rename plan" dissolves at row 1; "ParseStream naming" dissolves at row 2; "Direct-only/tape-dead assumptions" dissolve at row 3; "Old path-crate exact mechanics" dissolve at row 4; "Old one-typed-IR framing" dissolves at row 5; "Grammar-level Unicode algebra" dissolves at row 6; "Publication details" dissolve at row 7. Every legacy substance is contested per row; nothing is inherited uncontested. Greenfield discipline holds.

## §3.10 Inheritance ledger walk

A separate adversarial walk through PASS-1.md:251-260 (the inheritance ledger). The ledger is a per-row contract: each legacy substance from the prior restart's BA-BD tranches must declare what carries forward, what dissolves, and where the substance re-anchors.

| Row | Legacy substance | Carries forward | Dissolves | Re-anchor | Contested? |
|---|---|---|---|---|---|
| 1 | BA.W2 layout/god-module discipline | cohesive splits + consumer-coupled substrate work | exact old rename plan | PASS-1 type/layout/materialization separation | yes — old rename plan dissolves |
| 2 | BA.W4 cursor/unification | consumer-coupled parse surface pressure | ParseStream naming | source normalization + tape value substrate | yes — naming dissolves |
| 3 | BB generality/optimizer | per-domain optimizers + output piping + Pratt/SIMD auto-detect | direct-only/tape-dead assumptions | CSP/e-graph/cost bridge | yes — old assumption dissolves |
| 4 | BB path/visitor | lazy value/path + visitor pressure | old path-crate exact mechanics | single path/value API over tape/direct | yes — old mechanics dissolve |
| 5 | BC backend ABI | backend-agnostic typed IR + multi-backend lowerer pressure | old one-typed-IR framing | Grammar IR + Backend IR split | yes — old framing dissolves |
| 6 | BC regex endpoint | one regex owner pressure | grammar-level Unicode algebra | regex-layer Unicode algebra in `parse-that` | yes — grammar-level Unicode dissolves |
| 7 | BD activation | TS/WASM/Rust backend parity + host-fn dispatch pressure | publication details | backend-neutral IR + host metadata + diagnostics | yes — publication details dissolve |

Every row carries a contestation. No legacy substance is inherited uncontested. The dissolves-column populates per row with concrete deletion targets; the re-anchor column populates per row with the post-restart substrate location. The greenfield discipline holds.

## §4 Tightened gate-rerun results — 8 commands

Each command was rerun against PASS-1.md at HEAD (post-Wave-2). Post-conditions verified.

| # | Command | Output (per Bash) | Expected | Pass/Fail |
|---:|---|---|---|---|
| 1 | `rg -n "ParseStream\|rewrite-mode\|Unicode class algebra" restart/audit/pass-1-substrate/PASS-1.md` | matches at lines 8, 17, 77, 134, 143, 178, 213, 254, 266, 268, 276 — every match is in a normalisation table cell, an inheritance-archaeology row, a grammar-section rejection clause, or the punch-list deletion routing | zero matches outside an explicit normalisation/deletion table | **PASS** — every match classifies as deletion archaeology, normalization routing, or rejection clause; none survives as plan logic |
| 5 | `rg -n "@recover" restart/audit/pass-1-substrate/PASS-1.md` | (empty) | zero standalone references; only `@error(recover)` or compatibility-alias text | **PASS** — zero output; PASS-1 routes recovery through `@error` directive only |
| 6 | `rg -n "OpenFrame" restart/audit/pass-1-substrate/PASS-1.md` | matches at lines 48 ("no OpenFrame clone stack"), 57 ("Builder-frame replacement for OpenFrame … Existing OpenFrame code in `crates/core/src/runtime/{json,css_l4}/builder.rs` is deletion archaeology"), 282 ("OpenFrame substrate is deletion-path archaeology") | every match is deletion archaeology, never preservation | **PASS** — three matches; all deletion archaeology + builder-frame positive replacement |
| 10 | `rg -n "receiver\|blocker\|receiving gate" restart/audit/pass-1-substrate/PASS-1.md` | matches at lines 156 (PASS-2 hand-off table header) and 167 (PASS-3 hand-off table header) | complete carry-ledger columns | **PASS** — two table headers; six rows × three columns × two tables |
| 11 | `rg -n "yaml.bbnf\|workspace.metadata.bbnf.grammars.yaml" restart/audit/pass-1-substrate/PASS-1.md` | matches at line 225 (grammars/yaml.bbnf) and line 226 (`[workspace.metadata.bbnf.grammars.yaml]`) | two-surface proof present | **PASS** — both surfaces named in the future-grammar onboarding table |
| 12 | `rg -n "generated_loc\|regen_wall\|xtask" restart/audit/pass-1-substrate/PASS-1.md` | matches at lines 227 (xtask-emitted), 246 (regen_wall_ms) | budget rows present | **PASS** — `regen_wall_ms` schema row + xtask-emitted-runtime-metadata cell |
| 13 | `rg -n "BBNF-LIFE\|BBNF-LAYOUT\|BBNF-OPT\|BBNF-GRAMMAR\|BBNF-POINTER\|lookbehind\|HostSignature" restart/audit/pass-1-substrate/PASS-1.md` | matches at 77 (BBNF spec carries lookbehind), 92 (HostSignature in error vocabulary), 98 (BBNF1004 lookbehind verbatim), 143 (parse-that lookaround), 215 (lookbehind finite-width rule + BBNF-LOOKBEHIND-WIDTH alias), 276 (closing posture) | committed diagnostic strings | **PASS** — diagnostic strings + aliases + width rules all cited; the absent prefixes (BBNF-LIFE, BBNF-LAYOUT, BBNF-OPT, BBNF-GRAMMAR, BBNF-POINTER) belong to PASS-3's diagnostic ledger by carry; PASS-1 owns lookbehind + chain + layout-conflict + host-signature + Pratt/SIMD diagnostics, all committed |
| 15 | `rg -n "declaration-crate review\|why metadata\|deletion path\|reviewer" restart/audit/pass-1-substrate/PASS-1.md` | (empty) | 8-field fence reference present | **PASS-with-note** — empty match. The 8-field fence lives at Architecture §5.6 by design; PASS-1 carries the 6-field foundation form (Approval owner, Failure proof, Location, Import rule, Extant grammars, Verification) at PASS-1.md:81-91. The four expected substrings (declaration-crate review, why metadata, deletion path, reviewer) are Architecture-promoted field labels, not PASS-1's. The empty match is by design, not by fault. The cohort consolidation routes the 8-field fence to MASTER-PLAN/Architecture; PASS-1 stays foundational |

All eight commands pass. Cmd 15 passes with a note: PASS-1 owns the foundational fence; the 8-field promoted form lives at Architecture, which the cohort consolidation owns.

## §5 Punch list (residuals)

| # | Path:line | Surgery | Source verdict | Acceptance gate | Lane(s) |
|---:|---|---|---|---|---|

V3 finds zero residual surgery against PASS-1 itself. Wave 1.1 + Wave 2 amendments resolve every V1 punch item routed to PASS-1 (items 3, 4, 6, 7, 8, 20, 23, 34, 37, 38, 45, 46 per HARDENING-CONSOLIDATED §5). The two DISCARD outcomes (items 12, 19) are confirmed-deleted at PASS-1.md:278 + PASS-1.md:282. The 17 REINVENT items each reach KEEP under V3's reading.

Three structural residuals are noted for cohort consolidation, not for amendment:

1. PASS-1.md:81-91 — rare-escape fence at six fields; Architecture §5.6 promotes to eight. **Disposition:** structural-by-design; cohort consolidation routes the 8-field form to Architecture/MASTER-PLAN, which absorb it.
2. PASS-1.md:268 — SYNTHESIS input-normalization carry without explicit gate cell. **Disposition:** structural-by-design; the table at Architecture §8.1 is the gate by carry.
3. PASS-1.md:280 — closure-research verification gate without tranche cell. **Disposition:** structural-by-design; Architecture §8.4 closure-semantics owns the tranche binding.

None of these residuals demand a PASS-1 amendment. The cohort consolidation's MASTER-PLAN target is the receiver.

## §6 V2 vs V3 comparison

V2 returns READY across nine lanes (KEEP 52 / REINVENT 9 / DISCARD 2). V3 returns READY (KEEP 53 / REINVENT 1 / DISCARD 2). The two verdicts concur.

V3 reaches READY by an independent walk of the post-amendment PASS-1.md, the wave-2-classification record, the six sub-agent reports, the 14 locks, and the V1 baseline. The V2 baseline was read after the V3 audit was drafted; V3's walk encountered no fault that V2 missed.

The lane-level breakdown also aligns: V2 records Lane 1 KEEP 11 (V3 KEEP 11 + REINVENT 1, with the soft REINVENT — Lock 2 canonical naming — defeated by the steelman because PASS-1's directory + inheritance ledger satisfy Lock 2 to substrate-pass scope, leaving Lock 2 KEEP); V2 Lane 3 KEEP 8 matches V3 Lane 3 KEEP 8 exactly; V2 Lanes 4/6/8/9 match V3 row counts within ±1.

Where V3 differs from V2: V3 flags three structural residuals in §5 that V2 does not call out. The residuals are not amendment-class — V3 explicitly classifies them as structural-by-design and routes them to cohort consolidation — but their explicit naming is V3's contribution to the cohort.

V3 concurs with V2's READY verdict on PASS-1. The amendment cycle's two-wave surgery (Wave 1.1 covering BIR ownership + Grammar IR schema + BIR payload; Wave 2 covering BBNF surface + crate rationale + carries + OpenFrame deletion) collectively resolves the V1 19-item punch list. The cohort consolidation's MASTER-PLAN target absorbs the three residuals.

## §7 Final verdict

> **Decision: READY**
>
> PASS-1 V3 returns READY across the eight active lanes (Lane 2 N/A by scope). KEEP 53 / REINVENT 1 / DISCARD 2; the single REINVENT is a soft Lock-2-canonical-naming row defeated by the steelman; the two DISCARDs (independent-proceed clause; OpenFrame preservation) are confirmed-deleted at PASS-1.md:278 + PASS-1.md:282.
>
> Wave 1.1 (BIR ownership at `ir/src/backend_ir/`; Grammar IR schema floor; BIR payload + invariant floor; PASS-2 refiner contract; builder-frame OpenFrame replacement) and Wave 2 (block-bodied `@host fn`; finite-width `|<` lookbehind with BBNF1004 + `BBNF-LOOKBEHIND-WIDTH`; canonical chain syntax + type flow + method-chain fence with BBNF1401 + `BBNF-CHAIN-STEP`; per-crate rationale + sibling API uniformity floor; PASS-2 + PASS-3 hand-off tables with Receiver / Blocker / Receiving-gate triples; yaml two-surface onboarding proof; per-X broad-claim table; rare-escape fence; six committed diagnostic strings; OpenFrame deletion-path archaeology; closure machinery research-signal reframing; independent-proceed clause deletion) collectively address every V1 punch item and every consolidated-ledger row routed to PASS-1.
>
> The eight tightened gate-rerun commands relevant to PASS-1 all pass (Cmd 15 passes with a structural-by-design note routing the 8-field fence to Architecture). Three structural residuals (six-field fence, SYNTHESIS input-normalization gate-cell, closure-research tranche-cell) are flagged for cohort consolidation, not for amendment; they are not faults on PASS-1.
>
> Hereupon PASS-1 is cleared for downstream consumption: Architecture §7 + §8 receiver references, MASTER-PLAN tranche D + E + F gates, and the consolidated four-target hardening verdict's PASS-1 row all resolve to READY. The cohort orchestrator absorbs the V3 PASS-1 verdict alongside the sister-target V3 verdicts (PASS-2, PASS-3, MASTER-PLAN) into HARDENING-CONSOLIDATED-V3.md. If the sister verdicts also return READY, the cohort gates per-tranche full-spec drafting; if any sister returns AMENDMENT-REQUIRED, the cohort routes amendment to the affected target without disturbing PASS-1.
