# CORPUS-AUDIT-3 — `restart/audit/hardening/` directory

## §1 Audit scope

| Field | Value |
|---|---:|
| Files | 48 |
| Total bytes | ~1.8 MB (1,843 KB) |
| Total lines | 14,171 |
| Cycle range | V1 (May 4 14:15 unsuffixed) through V8 (May 7 01:49 V8 suffix) |
| Calendar span | May 4 → May 7 (4 days; 8 main cycles + 1 reviewer pass + 2 V5.1 intermediates) |
| Largest file | `HARDENING-CONSOLIDATED.md` (V1) — 619 lines / 45 KB |
| Smallest file | `HARDENING-PASS-1-PASS-2-V5.1A.md` — 75 lines / 5 KB |

Composition:

| Class | Count | Total lines |
|---|---:|---:|
| CONSOLIDATED reports (cohort-roll-ups) | 9 (V1, V2, V3, V4, V5, V6, V7, V7.1, V8) | 2,405 |
| Per-target hardening reports | 31 (PASS-1 ×7, PASS-2 ×8, PASS-3 ×8, MASTER-PLAN ×7, SYNTHESIS ×2 (V5.1+V6), V5.1 intermediates ×2) | 9,419 |
| REVIEW-A/B/C/D reviewer reports | 4 | 1,647 |
| **Total** | **48** | **14,171** |

(`HARDENING-PASS-1-PASS-2-V5.1.md` + `…V5.1A.md` count under per-target row above; PASS-1 has 7 cycles because V4 is absent — V3-READY carried through.)

## §2 Per-cycle history table

| Cycle | Date (May) | Trigger | Outputs | Cohort verdict | Notes |
|---|---|---|---|---|---|
| **V1** | 04 14:15-16:17 | Phase 6 first hardening over Wave-2 corpus | `HARDENING-PASS-1.md` (206), `…PASS-2.md` (294), `…PASS-3.md` (219), `…MASTER-PLAN.md` (227), `…CONSOLIDATED.md` (619) | **AMENDMENT-REQUIRED** (KEEP 117 / REINVENT 127 / DISCARD 8 / 47 punch items across 9 letter-cats) | Largest cohort by reinvention count; established the 9-lane (later 10) audit shape. |
| **V1 reviewer pass** | 04 17:45-17:49 | Adversarial review of V1 CONSOLIDATED | `REVIEW-A` (314), `…-B` (450), `…-C` (314), `…-D` (569) | (informational; no verdict change) | Four parallel reviewers hit consolidation fidelity, architectural integrity, Lock 14/greenfield, punch-list executability. Cited only once downstream — V2 §86 names "Reviewer D §6" gate-rerun checklist. |
| **V2** | 04 18:52-19:04 | Wave-2 amendment dispatch landed | `…-PASS-1-V2.md` (192), `-PASS-2-V2.md` (179), `-PASS-3-V2.md` (185), `-MASTER-PLAN-V2.md` (189), `-CONSOLIDATED-V2.md` (161) | **READY** (KEEP 244 / cumulative 92% KEEP rate / 4 non-blocking phrasing residuals) | First cycle to return READY across all four targets via single-author serial verification. |
| **V3** | 05 02:08-02:34 | Independent parallel re-audit (4 fresh auditors) | `-PASS-1-V3.md` (305), `-PASS-2-V3.md` (264), `-PASS-3-V3.md` (407), `-MASTER-PLAN-V3.md` (425), `-CONSOLIDATED-V3.md` (84) | **AMENDMENT-REQUIRED** (PASS-1 READY; PASS-2/PASS-3/MASTER-PLAN AMEND; 24 punch items) | Adversarial four-agent parallelism surfaced cross-document conflicts that V2 single-author missed. |
| **V4** | 05 02:24-02:34 | Wave-4.1 narrow amendment closure | `-PASS-2-V4.md` (192), `-PASS-3-V4.md` (329), `-MASTER-PLAN-V4.md` (410), `-CONSOLIDATED-V4.md` (124); PASS-1 carried V3-READY | **READY** (KEEP 241 / REINVENT 1 / DISCARD 2; all 24 V3 punch items closed) | No PASS-1-V4 file — V3-READY explicitly carried per V4 §1. |
| **V5** | 05 14:47-14:59 | Phase 0 V5 metahardening (carry-aware) over V4-READY | `-PASS-1-V5.md` (471), `-PASS-2-V5.md` (419), `-PASS-3-V5.md` (432), `-MASTER-PLAN-V5.md` (419), `-CONSOLIDATED-V5.md` (498) | **AMENDMENT-REQUIRED** (all 4 targets) | Reopened V4 finding ARCH §8.1 vs PASS-1 §6 grammar drift, `@pratt`/`@simd` diagnostic resurrection, citation drift. |
| **V5.1** | 05 15:18-15:22 | Phase 0.5 narrow amendment verification (3 routes) | `-PASS-1-PASS-2-V5.1.md` (236), `-PASS-1-PASS-2-V5.1A.md` (75), `-SYNTHESIS-V5.1.md` (215), `-PASS-3-V5.1.md` (198) | **READY** (after V5.1A citation hygiene) | Intermediate verification reports — no CONSOLIDATED-V5.1 written. V5.1A is a residue micro-pass over four shifted local line citations from V5.1. |
| **V6** | 06 10:51 (research fold + verification) | Research-fold orchestration (8 topic agents); fold to receivers | `-PASS-1-V6.md` (255), `-PASS-2-V6.md` (428), `-PASS-3-V6.md` (424), `-SYNTHESIS-V6.md` (246), `-CONSOLIDATED-V6.md` (391) | **READY** (all 4; KEEP 96 / zero blocking) | Topics 1-8 (HM, bidirectional, CSP/GADTs, e-graphs, cost models, tape, green/red incremental, SIMD/DFA) folded across PASS + SYNTHESIS trio. SYNTHESIS-V6 replaces MASTER-PLAN-V6 (filename change). |
| **V7** | 06 16:56-17:05 | Phase 7 fold absorption (V1-FOLD-CANDIDATES.md, 30 items × 4 tiers) | `-PASS-1-V7.md` (251), `-PASS-2-V7.md` (353), `-PASS-3-V7.md` (251), `-MASTER-PLAN-V7.md` (387), `-CONSOLIDATED-V7.md` (177) | **AMENDMENT-REQUIRED** (3 of 4 READY; MASTER-PLAN trio AMEND with 10 cite-hygiene + cross-doc coherence faults) | DK13/GADT/row-poly/schema-miner/function-values/closures/Backend trait/parse-that-regex anchored. `pointer!` → `path!` rename incomplete. |
| **V7.1** | 06 17:26 | Phase 7.5A+B narrow amendment closure verification | `-CONSOLIDATED-V7.1.md` (186) — verification-only, no per-target reports | **READY** (13 of 14 V7 punch items CLOSED; 1 friction residue R4 BBNF-PATTERN-NONEXHAUSTIVE rolls forward) | Single verification agent; trio rename cluster CLOSED (`pointer!` → `path!` + `BBNF-POINTER-*` → `BBNF-PATH-*` + regex-automata oracle removed). |
| **V8** | 07 01:41-01:49 | Phase-8.1-amendment simplification audit (lenses I/J/K) | `-PASS-1-V8.md` (139), `-PASS-2-V8.md` (400), `-PASS-3-V8.md` (173), `-MASTER-PLAN-V8.md` (322), `-CONSOLIDATED-V8.md` (167) | **SIMPLIFY-AVAILABLE** (V7.1 READY survives; 41 simplification candidates surfaced across 5 tiers α/β/γ/δ/ε) | Adversarial pressure on contrivance / host-leverage / meta-grammar discipline; no architectural axiom contested. |

Aggregate cycle-level KEEP / READY: V1 amend → V2 ready → V3 amend → V4 ready → V5 amend → V5.1 ready → V6 ready → V7 amend → V7.1 ready → V8 simplify-available. Five amendment cycles + five readiness gates. Verify-then-rerun rhythm preserved throughout.

## §3 Per-file classification

KEY: **KSH** = KEEP-SEALED-HISTORY (audit-trail evidence; preserve unchanged); **EXP** = EXPLICATE (citation/cross-reference target for downstream); **PRU** = PRUNE-CANDIDATE (cycle internal-only; superseded by later consolidation that does not cite it).

| # | File | Cycle | Type | Lines | Classification | Rationale |
|---:|---|---|---|---:|---|---|
| 1 | `HARDENING-CONSOLIDATED.md` | V1 | CONSOLIDATED | 619 | **KSH-EXP** | Origin baseline; V2-V8 cite "V1" verdicts via §10 history tables; deletion erases the start-of-trail. |
| 2 | `HARDENING-PASS-1.md` | V1 | per-target | 206 | **KSH** | V1 cohort source; cited by REVIEW-A/B/C/D + V2 §1. |
| 3 | `HARDENING-PASS-2.md` | V1 | per-target | 294 | **KSH** | Same. |
| 4 | `HARDENING-PASS-3.md` | V1 | per-target | 219 | **KSH** | Same. |
| 5 | `HARDENING-MASTER-PLAN.md` | V1 | per-target | 227 | **KSH** | Same. |
| 6 | `REVIEW-A-CONSOLIDATION-FIDELITY.md` | V1-reviewer | reviewer | 314 | **KSH-PRU?** | Cited downstream only via "Reviewer D §6" reference in V2; otherwise dormant. See §4. |
| 7 | `REVIEW-B-ARCHITECTURAL-INTEGRITY.md` | V1-reviewer | reviewer | 450 | **KSH-PRU?** | Same — never cited by filename in V2-V8. |
| 8 | `REVIEW-C-LOCK-14-GREENFIELD.md` | V1-reviewer | reviewer | 314 | **KSH-PRU?** | Same. |
| 9 | `REVIEW-D-PUNCH-LIST-EXECUTABILITY.md` | V1-reviewer | reviewer | 569 | **KSH** | Cited by V2:86 ("Reviewer D §6 gate-rerun checklist"). Heaviest reviewer; the others lean on it for harness language. |
| 10 | `HARDENING-CONSOLIDATED-V2.md` | V2 | CONSOLIDATED | 161 | **KSH-EXP** | V5 §10 + V6 §10 cite as cohort-trajectory rows. |
| 11 | `HARDENING-PASS-1-V2.md` | V2 | per-target | 192 | **KSH-PRU?** | V3 §1 cites via commit hash `4670773d`; V4+ does not cite by filename. Superseded by V3-PASS-1. |
| 12 | `HARDENING-PASS-2-V2.md` | V2 | per-target | 179 | **KSH-PRU?** | Same. |
| 13 | `HARDENING-PASS-3-V2.md` | V2 | per-target | 185 | **KSH-PRU?** | Same. |
| 14 | `HARDENING-MASTER-PLAN-V2.md` | V2 | per-target | 189 | **KSH-PRU?** | Same. |
| 15 | `HARDENING-CONSOLIDATED-V3.md` | V3 | CONSOLIDATED | 84 | **KSH-EXP** | V4 §3 punch-closure summary anchors here. Smallest CONSOLIDATED — V3 was a closure-verification flavor. |
| 16 | `HARDENING-PASS-1-V3.md` | V3 | per-target | 305 | **KSH-EXP** | V4 §1 explicitly notes "PASS-1 carries V3-READY through V4 cohort tally without rerun". Load-bearing. |
| 17 | `HARDENING-PASS-2-V3.md` | V3 | per-target | 264 | **KSH-PRU?** | Superseded by PASS-2-V4. |
| 18 | `HARDENING-PASS-3-V3.md` | V3 | per-target | 407 | **KSH-PRU?** | Superseded by PASS-3-V4. |
| 19 | `HARDENING-MASTER-PLAN-V3.md` | V3 | per-target | 425 | **KSH-PRU?** | Superseded by MASTER-PLAN-V4. |
| 20 | `HARDENING-CONSOLIDATED-V4.md` | V4 | CONSOLIDATED | 124 | **KSH-EXP** | V5 §1 + V5 §10 cohort baseline; V6 §10. |
| 21 | `HARDENING-PASS-2-V4.md` | V4 | per-target | 192 | **KSH-PRU?** | Cohort row in V4 §1; superseded by V5/V6/V7. |
| 22 | `HARDENING-PASS-3-V4.md` | V4 | per-target | 329 | **KSH-PRU?** | Same. |
| 23 | `HARDENING-MASTER-PLAN-V4.md` | V4 | per-target | 410 | **KSH-PRU?** | Same. |
| 24 | `HARDENING-CONSOLIDATED-V5.md` | V5 | CONSOLIDATED | 498 | **KSH-EXP** | V5.1 verification reports cite V5 §156-165 bundle map + §280-294 success criteria. V6 §10 history. Heaviest CONSOLIDATED. |
| 25 | `HARDENING-PASS-1-V5.md` | V5 | per-target | 471 | **KSH-PRU?** | V5.1 narrow amendment ran against this; superseded by V6+. |
| 26 | `HARDENING-PASS-2-V5.md` | V5 | per-target | 419 | **KSH-PRU?** | Same. |
| 27 | `HARDENING-PASS-3-V5.md` | V5 | per-target | 432 | **KSH-PRU?** | Same. |
| 28 | `HARDENING-MASTER-PLAN-V5.md` | V5 | per-target | 419 | **KSH-PRU?** | Same. |
| 29 | `HARDENING-PASS-1-PASS-2-V5.1.md` | V5.1 | intermediate | 236 | **KSH-PRU** | Cited only by V5.1A; verdict landed AMENDMENT-REQUIRED, superseded by V5.1A closure. See §5. |
| 30 | `HARDENING-PASS-1-PASS-2-V5.1A.md` | V5.1A | intermediate | 75 | **KSH-PRU** | Cited only by V6 §10 history table generically. Smallest file; closure of V5.1 residue. See §5. |
| 31 | `HARDENING-SYNTHESIS-V5.1.md` | V5.1 | intermediate | 215 | **KSH-PRU** | Cited only by V6 §10; superseded by V6/V7/V7.1 SYNTHESIS treatment. See §5. |
| 32 | `HARDENING-PASS-3-V5.1.md` | V5.1 | intermediate | 198 | **KSH-PRU** | Same. See §5. |
| 33 | `HARDENING-CONSOLIDATED-V6.md` | V6 | CONSOLIDATED | 391 | **KSH-EXP** | V7 §10 + V7.1 §1 explicitly anchor "V6 carry-baseline of record"; the research-fold receiver-binding ledger. |
| 34 | `HARDENING-PASS-1-V6.md` | V6 | per-target | 255 | **KSH-EXP** | V7 §1 cites commit `a745f12e` as audit baseline. |
| 35 | `HARDENING-PASS-2-V6.md` | V6 | per-target | 428 | **KSH-EXP** | V7 §1 cites commit `5ea41850`. |
| 36 | `HARDENING-PASS-3-V6.md` | V6 | per-target | 424 | **KSH-EXP** | V7 §1 cites commit `c5e3aab7`. |
| 37 | `HARDENING-SYNTHESIS-V6.md` | V6 | per-target | 246 | **KSH-EXP** | V7 §1 cites commit `4fe06344`. Note: V6 used `SYNTHESIS-V6` as filename rather than `MASTER-PLAN-V6` — single-cycle nomenclature anomaly. |
| 38 | `HARDENING-CONSOLIDATED-V7.md` | V7 | CONSOLIDATED | 177 | **KSH-EXP** | V7.1 §1 explicitly anchors "V7 carry-baseline of record" `822bed18`. |
| 39 | `HARDENING-PASS-1-V7.md` | V7 | per-target | 251 | **KSH-EXP** | V7.1 verifies R1-R3 closure against this. |
| 40 | `HARDENING-PASS-2-V7.md` | V7 | per-target | 353 | **KSH-PRU?** | V8 cites V7.1 only; PASS-2-V7 superseded by PASS-2-V8. |
| 41 | `HARDENING-PASS-3-V7.md` | V7 | per-target | 251 | **KSH-PRU?** | Same. |
| 42 | `HARDENING-MASTER-PLAN-V7.md` | V7 | per-target | 387 | **KSH-PRU?** | Same. |
| 43 | `HARDENING-CONSOLIDATED-V7.1.md` | V7.1 | CONSOLIDATED | 186 | **KSH-EXP** | V8 §1 baseline ("V7.1 READY survives V8 lens scrutiny"). The current operating baseline. |
| 44 | `HARDENING-CONSOLIDATED-V8.md` | V8 | CONSOLIDATED | 167 | **KSH-EXP** | Most-recent simplification audit; 41-candidate surface. **Live operating document.** |
| 45 | `HARDENING-PASS-1-V8.md` | V8 | per-target | 139 | **KSH-EXP** | V8 cohort source; tier α/γ/δ candidates anchor here. |
| 46 | `HARDENING-PASS-2-V8.md` | V8 | per-target | 400 | **KSH-EXP** | Same. |
| 47 | `HARDENING-PASS-3-V8.md` | V8 | per-target | 173 | **KSH-EXP** | Same. |
| 48 | `HARDENING-MASTER-PLAN-V8.md` | V8 | per-target | 322 | **KSH-EXP** | Same. |

Tally: **17 KSH-EXP** (live citation targets) / **27 KSH-PRU?** (sealed audit-trail; aggressive prune candidates) / **4 KSH-PRU** (V5.1 intermediates; minimal prune candidates).

## §4 Reviewer reports disposition (REVIEW-A/B/C/D)

Cycle binding: V1-reviewer (May 4 17:45-17:49 timestamps; one wall-hour after the V1 cohort consolidated at 16:17). All four target `HARDENING-CONSOLIDATED.md` (V1) and the four V1 per-target reports.

Citation footprint:

| Reviewer | Cited downstream? | Form | Disposition |
|---|---|---|---|
| REVIEW-A (consolidation fidelity) | No filename cite anywhere in V2-V8 | V2 §86 names "Reviewer D §6 + AMENDMENT-DISPATCH §3 Wave 4" gate-rerun checklist (D-only) | **KSH** — sealed evidence; pruning loses the V1-era fidelity audit. |
| REVIEW-B (architectural integrity) | No filename cite | Heaviest pre-V2 architectural pressure-test (450 lines); prose subsumed by V2-V8 trio audits | **KSH** — sealed evidence. |
| REVIEW-C (Lock 14 + greenfield) | No filename cite | V1 reviewer was the load-bearing review for Lock 14 (yaml two-surface, declaration-crate fence, OpenFrame retiral); language entered the trio | **KSH** — sealed evidence. |
| REVIEW-D (punch-list executability) | **YES**, V2:86 | "Reviewer D §6 gate-rerun checklist" — the 16-command harness that V2-V6 ran | **KSH-EXP** — load-bearing for V2-V6 audit harness. |

Verdict: The reviewer cohort is intentional adversarial cross-check after V1 CONSOLIDATED.md returned AMENDMENT-REQUIRED. They predate V2 verification. Three of four are dormant after V2 (REVIEW-A/B/C); REVIEW-D's gate-rerun checklist still anchors V2-V6 verification harness language. None should be pruned: the four reviewers are the only adversarial-pass artefact for Phase 6, and the V8 simplification audit references the lens-discipline they introduced (§7 V8 LLM-pathology summary acknowledges F/G/H lenses).

Recommendation: **all four KSH**. Aggressive prune leaves them in.

## §5 V5.1 + V5.1A intermediate reports disposition

Files in scope:

| File | Lines | Cited where? |
|---|---:|---|
| `HARDENING-PASS-1-PASS-2-V5.1.md` | 236 | Cited only by `HARDENING-PASS-1-PASS-2-V5.1A.md:5-13` (residue input). V6 §10 history mentions "V5.1" generically. |
| `HARDENING-PASS-1-PASS-2-V5.1A.md` | 75 | Cited only by V6 §10 history table generically ("V5.1 narrow amendments closed the substantive V5 defects"). |
| `HARDENING-SYNTHESIS-V5.1.md` | 215 | Cited only by V6 §10 history generically. |
| `HARDENING-PASS-3-V5.1.md` | 198 | Cited only by V6 §10 history generically. |

Total: 724 lines / ~60 KB. None of V6, V7, V7.1, V8 reference these files by filename — only the cycle-name "V5.1" appears, and only in the V5.1-to-V6 history row of `HARDENING-CONSOLIDATED-V6.md:358-366`.

The V5.1 cohort is **the cleanest prune-candidate set** in the directory:
- They are intermediate verification reports (Phase 0.5 narrow-amendment verification of V5 amendment dispatch).
- No CONSOLIDATED-V5.1 was authored — the verdict reached the corpus only via V6 §10 generic history.
- The 75-line V5.1A residue micro-pass is deeply internal (four shifted local line-citations).
- Their architectural content is fully absorbed by V6 PASS-1/PASS-2/PASS-3/SYNTHESIS reports.

Disposition: **KSH-PRU** for all four. Audit-trail value is the cycle-name reference; the file content is dispatch-internal and not cited by content. If pruned, V6 §10 history continues to read coherently.

## §6 V2-deferral occurrence ledger

User mandate per `restart/prompts/HARDENING.md` post-Phase-8.1 amendment: V2 deferrals are retired. V1 is the surface that ships; aspirational items route to per-tranche bodies, not "V2 amendment".

Surface scan across consolidations + per-target reports:

| File | Hits | Verdict | Rewrite class |
|---|---:|---|---|
| `HARDENING-CONSOLIDATED.md` (V1) | 0 | clean | — |
| `HARDENING-CONSOLIDATED-V2.md` | 1 | acceptable-historical (`:117` "deferring zero substantive surgery" — describes V2 amendment dispatch behavior, not a V2-version deferral) | acceptable |
| `HARDENING-CONSOLIDATED-V3.md` / V4 / V5 / V5.1 cohort / V6 | 0 | clean | — |
| `HARDENING-CONSOLIDATED-V7.md:154` | 1 | "V2 deferral routes mechanical-expansion via trait pre-existence" — Backend trait V2 deferral (Wasm/Ts backends) | acceptable-historical (sealed verdict; not an editable directive) |
| `HARDENING-CONSOLIDATED-V7.1.md:44 + :88` | 2 | `bbnf-path-ts` row narrows to V2-deferral language — explicit V2-deferral closure verdict | acceptable-historical (sealed verdict citing MIGRATION row that itself was narrowed) |
| `HARDENING-CONSOLIDATED-V8.md` | 11 | mixed: §2 cohort table + §3 Tier δ items 3, 4, 9, 10 + §6 final readiness verdict + §7 LLM-pathology summary | mostly-acceptable; **§3 Tier δ candidates (CHR-improvement, GADT V2, function composition library) are live V2-amendment receivers — under user mandate these would route to per-tranche bodies instead** |
| `HARDENING-MASTER-PLAN-V8.md` | 22 | densest occurrence | live policy carrier — must reframe under mandate |
| `HARDENING-PASS-1-V8.md` | 13 | Lens K aspirational rows | live policy carrier |
| `HARDENING-PASS-3-V7.md / V8` | 13 / 3 | tranche-body deferral language for DAP/LSP body | mixed |
| `HARDENING-PASS-2-V7.md` | 9 (`:62 :108 :109` etc.) | "WasmBackend: Backend deferred post-V1; V2 amendment alongside Lock 11" | sealed-historical (V7-era verdict) |
| `HARDENING-PASS-1-V7.md` | 5 | DK13/GADT V2 deferral | sealed-historical |

**Classification**:
- **must-rewrite-V1**: V8 Tier δ items in `HARDENING-CONSOLIDATED-V8.md:71-79` + `HARDENING-MASTER-PLAN-V8.md` carry-ledger rows. These are the live-policy artefacts the mandate touches; they propose "V2 amendment" as the receiver for items the mandate now retires.
- **acceptable-as-historical-record**: every occurrence in V1-V7 + V7.1 sealed verdicts. They are the audit-trail of what V1-V7-era discipline considered V2-deferrable; rewriting them retroactively erases evidence of the user mandate's effect. Per the mandate's calibrated reading, these stay sealed; only forward-facing surfaces (V8 + the trio) update.

Concrete must-rewrite locations under the mandate:
1. `HARDENING-CONSOLIDATED-V8.md:65-79` Tier δ table — δ3 (CHR-improvement layer), δ4 (GADT V2), δ9 (function composition library), δ10 (CHR-improvement) currently carry "V2 amendment" receiver. Under mandate, route to tranche bodies (specific tranche: D body for DK13/GADT; pluggable-component tranches for CHR-improvement; library tranches for function composition).
2. `HARDENING-CONSOLIDATED-V8.md:122` — "**V2 amendment** (5 items): CHR-improvement, function composition, GADT surface, etc." needs reroute.
3. `HARDENING-CONSOLIDATED-V8.md:124` — "Every aspirational item has a tranche body or V2 amendment receiver" — reword to "tranche body receiver" only.
4. `HARDENING-CONSOLIDATED-V8.md:165` — "all route to Phase 8.4 fold or to per-tranche bodies / V2 amendments" — drop "/V2 amendments".
5. `HARDENING-MASTER-PLAN-V8.md` — 22 occurrences; densest reroute target. Per audit-1/audit-2 cohort (parallel agents on type-system + function-value-system) the carry-ledger here is the live consolidation point.

The V7-era language at `HARDENING-PASS-2-V7.md:62/:108-109` is sealed; rewriting it would erase the audit-trail of when WasmBackend/TsBackend V2-deferral was the operating verdict. Mandate retires that policy going forward, not retroactively.

## §7 Pruning recommendation

Three options sized:

### Option A — AGGRESSIVE prune (~12 files / ~390 KB)

Drop V2 + V3 per-target reports (8 files) + V5 per-target reports (4 files) + V5.1 cohort (4 files):
- `HARDENING-PASS-{1,2,3}-V2.md` + `HARDENING-MASTER-PLAN-V2.md` (4 files / 745 lines)
- `HARDENING-PASS-{2,3}-V3.md` + `HARDENING-MASTER-PLAN-V3.md` (3 files; PASS-1-V3 retained — V4 cites it as carried-READY) (1097 lines)
- `HARDENING-PASS-{1,2,3}-V5.md` + `HARDENING-MASTER-PLAN-V5.md` (4 files / 1741 lines)
- `HARDENING-PASS-1-PASS-2-V5.1{,A}.md` + `HARDENING-{SYNTHESIS,PASS-3}-V5.1.md` (4 files / 724 lines)
- Total: 15 files / 4307 lines / ~140 KB

(Pruning expands the table from "12 files" to 15 files because V5 + V5.1 both qualify as superseded.)

Risks: REVIEW-D §6 gate-rerun checklist references V2-PASS surface for command-rerun harness language. V4 §3 punch-closure summary anchors against V3 per-target reports. Pruning V3 per-target erases the only adversarial four-agent parallel cycle's evidence (V3 was the cycle that converted single-author V2-READY into AMENDMENT-REQUIRED via independent re-audit — the cycle that proved adversarial parallelism was load-bearing).

### Option B — MINIMAL prune (~4 files / ~60 KB)

Drop only V5.1 cohort:
- `HARDENING-PASS-1-PASS-2-V5.1.md` (236 lines)
- `HARDENING-PASS-1-PASS-2-V5.1A.md` (75 lines)
- `HARDENING-SYNTHESIS-V5.1.md` (215 lines)
- `HARDENING-PASS-3-V5.1.md` (198 lines)
- Total: 4 files / 724 lines / ~60 KB

Risks: V6 §10 history continues to read coherently after pruning ("V5.1 narrow amendments closed the substantive V5 defects" remains a true claim by cycle name). The pruned files are intermediate verification reports — the deepest internal cycle of the audit trail. Only V5.1A even names V5.1 by filename, and that's a residue micro-pass. Aggregate audit-trail loss: the verify-then-patch closure evidence between V5 (AMEND) and V6 (READY).

### Option C — NONE (retain all 48)

Total disk: 1.8 MB. Total cognitive overhead: 14,171 lines. Total citation paths preserved. Audit-trail integrity 100%.

### Recommendation: **MINIMAL prune (Option B)**.

Rationale: The hardening directory is the audit trail. The mandate "the user may want to prune older cycles if the audit-trail value drops" (§2.5) only applies if audit-trail value provably dropped. Per cycle:
- **V1** (CONSOLIDATED + 4 per-target + 4 reviewer = 9 files): trail-origin. Cite anchor for every later cycle's "V1 baseline" claim. KEEP all.
- **V2-V7 per-target** (24 files): each cycle's verdict letter is sealed by commit hash citations in the next-cycle CONSOLIDATED. Aggressive prune erases adversarial-cycle evidence (V3 four-agent re-audit; V5 Phase-0 metahardening reopening; V7 V1-FOLD absorption). KEEP all.
- **V5.1 + V5.1A** (4 files): intermediate verification reports between V5 amendment dispatch and V6 research-fold. Never CONSOLIDATED into a V5.1 cohort report. Architectural content fully absorbed by V6. The only downstream reference is V6 §10 generic history. Audit-trail value is the cycle-name reference, not file content. **Pruneable without information loss.**

Net: prune 4 files / 60 KB / 724 lines (~5% of directory bytes; ~5% of line count). Audit-trail intact; V6 §10 history continues coherent.

Re-classification under recommendation:
- 44 files KSH (preserved sealed history).
- 4 files PRUNE (V5.1 cohort).

## §8 Open questions

1. **V5.1 prune timing.** The recommended prune presumes V8 verdict (SIMPLIFY-AVAILABLE) holds. If V8.4 fold runs and V8.1 READY supersedes V7.1, does V5.1 audit-trail evidence become re-load-bearing for V5 → V8 trajectory reconstruction? Audit unable to determine without V8.4 dispatch decision. **Recommend defer V5.1 prune until V8.1 lands or V8.4 is explicitly waived.**
2. **REVIEW-A/B/C reviewer files (1078 lines combined; never cited by filename).** Per §4, classified KSH on adversarial-pass-evidence grounds. If "adversarial-pass-evidence" is not a load-bearing classification under user discipline, these are aggressive-prune candidates. **Awaiting user adjudication.**
3. **HARDENING-MASTER-PLAN-V6 absence.** V6 used `HARDENING-SYNTHESIS-V6.md` as the trio audit filename rather than `HARDENING-MASTER-PLAN-V6.md`. V7 returned to MASTER-PLAN nomenclature. The single-cycle nomenclature anomaly is sealed-by-commit but produces a search-discovery friction. **No prune — note for SYNTHESIS-V6 explicate: cross-reference filename in V7 history table for downstream-grep stability.**
4. **V8 Tier δ V2-amendment receivers.** §6 surfaces 5 must-rewrite locations in V8 CONSOLIDATED + MASTER-PLAN where "V2 amendment" remains the routing receiver for CHR-improvement / function composition / GADT surface. Under the user mandate retiring V2 deferrals, these need reroute to per-tranche bodies. **Recommend Wave-4.1 amendment-class redress agent re-anchors V8 Tier δ to tranche-D body / library-tranche / CSP-solver crate.** This is forward-policy work, not history-rewrite.
5. **Cross-directory dependence.** The hardening directory cites `restart/research/V1-FOLD-CANDIDATES.md`, `restart/research/INDEX.md`, `restart/research/fold-{pass,synthesis}-N.md`, `restart/locks/14-LOCKS.md`, `restart/prompts/HARDENING.md`, `restart/prompts/AMENDMENT-DISPATCH.md`. Audit-3 scope is hardening directory only; cross-directory pruning would need parallel audit on those receivers. **Out of scope.**

---

**Closing posture.** The hardening directory is an 8-cycle adversarial audit trail covering 4 days of greenfield restart pressure. 48 files / 14,171 lines / 1.8 MB. 17 files are live citation targets (current operating baseline V7.1+V8 + their immediate predecessors V6/V7); 27 files are sealed audit-trail (older per-target reports superseded by later consolidations); 4 files (V5.1 cohort) are intermediate verification reports never consolidated into a CONSOLIDATED-V5.1 and only generically referenced by V6 §10 history. The minimal prune (Option B; 4 files / 60 KB) is judicious — pruning loses no cited content; the audit-trail-by-cycle-name remains coherent. Aggressive prune (Option A) erases adversarial-cycle evidence (V3 four-agent re-audit; V5 Phase-0 metahardening) that has near-zero current citation but heavy historical weight; reject. Option C (retain all) is the safest under "calibrated, no premature optimization" discipline.

**Recommendation: Option B — prune 4 V5.1 files; retain 44.** Defer until V8.4 fold decision lands or is explicitly waived (Open Q1).
