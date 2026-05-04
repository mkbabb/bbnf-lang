# Hardening Stage 2 — MASTER-PLAN (the apex target)

Date: 2026-05-03
Stage-2 adversary: HARDENING-STAGE-2-EXTERNAL.md target=Stage-1-MASTER-PLAN
Target: `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-MASTER-PLAN.md` (2 234 lines; commits `fd0c1179` + `70f83795` + `0d30a863` + `bb3f914a` + `17b3db6c`)
Underlying ground truth: `/Users/mkbabb/Programming/bbnf-lang/restart/audit/master-plan/MASTER-PLAN.md` (1 418 lines) + `AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` (161 lines)
Authoritative override: Amendment 01 wins where master plan and amendment disagree.
Time budget: 60 min hard cap; commit at min 55.

---

## §1 — Target identification

This Stage-2 pass evaluates Stage-1 MASTER-PLAN's hardening of the synthesizer's master plan. The Stage-1 report at `/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-MASTER-PLAN.md` (2 234 lines, authored across two dispatches: initial commit `fd0c1179` covering §1-§7 and continuation `17b3db6c` covering §8-§13, with three intermediate commits) ratifies, surfaces, or recommends amendment against the underlying master plan + Amendment 01. Stage-2's remit is the Stage-1 audit-quality, not the master plan substance directly; the underlying target is read as ground truth so Stage-1 silence on master-plan items can be surfaced.

Stage-1 MASTER-PLAN returned **amendment-required** with a 76-item punch list organised across §12.A–§12.G plus a cumulative-reconciliation table totalling 161 surgical edits across the four Stage-1 reports. The five Stage-2 lanes (2A Confirmation-Drift, 2B Discipline Lapse, 2C Steelman, 2D Verdict-Imbalance, 2E Recommendation-Quality) apply per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`.

**Material temporal fact bearing on Lane 2B**: the Pro/Con/Explication/Challenge per-item discipline plus KEEP/REINVENT/DISCARD verdict vocabulary was codified at commit `6e1c6e5f` (2026-05-03 17:24:54). Stage-1 MASTER-PLAN was authored across two dispatches: the initial dispatch landed §3-§7 at commit `fd0c1179` (after the discipline codification); the continuation landed §8-§13 at commit `17b3db6c`. Both dispatches occurred AFTER the discipline codification, so Lane 2B audits whether each lane's per-item rows honour the discipline shape — irrespective of authorship sequence.

Sister Stage-2 reports landed previously: PASS-A (commit `6187ed1e`) and PASS-B (commit `d0d4d1e5`); PASS-C is in flight or pending. The MASTER-PLAN target is the most consequential of the four because its punch list directly drives the V2 re-issue.

---

## §2 — Cohort verdict

| Lane | Stage-2 verdict | Notes |
|---|---|---|
| 2A Confirmation-Drift | PARTIAL | 57 CONFIRM + 2 STRENGTHEN + 1 WEAKEN + 1 REVERSE-soft across 61 audited Stage-1 verdicts; 4 master-plan items unsurfaced (specialised-cohort host-fn 14-variant decomposition, Tranche-A.W3 IR-side-only scrub framing, the §3.3 ordering's archive-vs-no-members ambiguity, the §13 R20 scope-pivot lock cross-reference) |
| 2B Discipline Lapse | HONOURED with two-row PARTIAL | Per-row Pro/Con/Explication/Challenge tables present across all nine Stage-1 lanes; KEEP/REINVENT/DISCARD verdict vocabulary used throughout; discipline variance between initial dispatch (§3-§7) and continuation (§8-§13) is real but small — initial dispatch's Lane 1 carries denser steelman than continuation's Lane 9 (cross-references prior items rather than steelman freshly) |
| 2C Steelman | SURVIVES with three WEAKENED | 24-member workspace count survives steelman; tranche allocation A-J with 53 waves survives; commit-chain Option 3 survives. Three claims weaken: convergent-pivot-at-E (Lock 1+13+14 retire as one) admits a staggered three-tranche alternative the synthesizer did not consider; bbnf-host-prims as a Rust crate weakens against extended-BBNF-directives-only; the pivot's "OpenFrame retires by mechanism" claim weakens against per-grammar staged retiral |
| 2D Verdict Imbalance | BALANCED-with-anomalies | Aggregate 35% KEEP / 58% REINVENT / 7% DISCARD across 113 master-plan-specific items lies in the under-ratifying band (40% threshold). The 58% REINVENT signals the Amendment-01 reconciliation overhead (28 sites of body-vs-amendment drift); ABSENT amendment-rec the underlying KEEP fraction is healthy 62-65%. Lane 7 (KEEP=0) survives Stage-2 steelman of all six friction surfaces — genuine REINVENT. Lane 1 KEEP fraction 64% is appropriate. |
| 2E Recommendation Quality | STRENGTHEN | 76 punch-list items. ~70% are concrete verbatim re-anchorings (items 24-49 are mass-mechanical Amendment-01 site rewrites); ~20% are well-scoped paragraph-level surgeries; ~10% are too vague or too aggressive in scope (items 7, 8, 9, 17, 22, 50, 67-73 cluster around mid-tranche SOTA gates and carry-receiver triples where the fix is sketched rather than spelled). §12.G "cumulative reconciliation" table tabulates 161 cumulative edits but does NOT deduplicate against per-pass punch lists — this is the largest Stage-1 fault Stage-2 surfaces in this lane. |

**Final Stage-2 decision: STAGE-1 RATIFIED WITH AMENDMENTS REQUIRED.**

Stage-1 MASTER-PLAN's substantive shape holds. Its 76-item punch list lands as the V2 re-issue's working surface; the Amendment-01 reconciliation cohort (items 23-49) is the most extensive concrete-amendment surface in the suite and survives Stage-2 verification line-by-line. The Lane 1 lock-honoured table reanchorings are concrete; the Lane 4 mid-tranche SOTA gate additions for tranches F/G/H are correct in direction; the Lane 7 friction-surface enumeration covers the six mandatory surfaces and each REINVENT survives Stage-2 steelman; the Lane 8 carry-deferral triples for `bbnf-cli`, `bbnf-py`, `docs/restart/`, archive deletion, and Stage 2 hardening receiver are correctly identified.

The Stage-2 amendments fold into the master-plan-V2 punch list as items #77-#88: (i) deduplicate §12.G cumulative table against per-pass punch lists (Lane 2E redress, item #77); (ii) surface the four unsurfaced master-plan items (Lane 2A redress, items #78, #81-#84); (iii) tighten the convergent-pivot framing toward staggered partial closures (Lane 2C redress, item #85); (iv) refine specific surgery wave-assignments and clarify H carry-FROM (items #79, #87, #88); (v) acknowledge the friction-onboarding REVERSE-soft (item #80 trims sub-friction-cookbook count from 5 to 3 textual artefacts); (vi) sharpen Stage-1 surgery 73's closing posture (item #86).

Stage-1 MASTER-PLAN does NOT require re-audit — its substantive findings survive Stage-2 scrutiny robustly. The faults are amenable to a single Stage-2 amendment agent's pass; the V2 re-issue agent receives both Stage-1 and Stage-2 punch lists alongside the master plan and Amendment 01. The 14 locks are settled; the precepts are settled; Amendment 01 is settled. Stage-2 verifies; Stage-2 does NOT relitigate.

---

## §3 — Lane 2A — Confirmation-Drift Audit

**Lane standard.** For every Stage-1 verdict, evaluate whether Stage-1 carried the master plan's framing implicitly. Did Pros mirror master-plan paraphrase while Cons / Challenges thinned? Are there master-plan items Stage-1 did not surface for per-item evaluation? Stage-1 silence on a target item is a confirmation-drift fault.

The principal drift surfaces concentrate around four framings: (a) the **convergent-pivot claim** at Tranche E (Lock 1+13+14 as one); (b) the **specialised-cohort scope** carrying CSS L4's 14-variant `OpenFrame` as the decomposition test; (c) the **Tranche-A.W3 framing** (Lock-14 retirement of 7 sites) admitting whether it is IR-side-only scrub or full-stack retiral; (d) the **§3.3 archive-vs-no-members ambiguity** — whether `archive/` is workspace-member-bearing or fully external.

### §3.1 — Per-item Confirmation-Drift table — Lane 1 (Lock-Adherence)

| Stage-1 site | Master-plan item | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|
| HARDENING-MASTER-PLAN.md:122 (Lock 1) | §11 row 1 (`MASTER-PLAN.md:1283`) | REINVENT | 4 | CONFIRM | Stage-1 catches the Amendment 01 substrate-extension gap explicitly (`bbnf-runtime/src/grammars/`, `bbnf-runtime-template/src/`, `bbnf-host-prims/src/`); concrete punch-list 1 |
| HARDENING-MASTER-PLAN.md:126 | Tranche A "narrative scrub" 50 sites (`:777`) | KEEP | 2 | WEAKEN | Stage-1's Pros + Cons are paraphrase-thin; the steelman challenge would interrogate whether OpenFrame counts as residue (Pass B Agent B.3 says "yes in spirit") and demand the scrub include OpenFrame symbol-level retiral. Stage-1's verdict is right but the rationale carries by trust |
| HARDENING-MASTER-PLAN.md:128 | Tranche E "86.07% samply share collapses by mechanism" (`:785`) | REINVENT | 4 | CONFIRM | Stage-1 catches the structural-only claim and demands measurement gate; surgery 23 fires |
| HARDENING-MASTER-PLAN.md:152 (Lock 2) | LayoutSink trait verification (`:1284`) | REINVENT | 3 | CONFIRM | Stage-1's verification extension (`trait LayoutSink in bbnf-ir/src/registry/sink.rs`) is concrete |
| HARDENING-MASTER-PLAN.md:179 (Lock 3) | Cookbook home C.W6 vs G.W3 (`:1285`) | REINVENT | 4 | CONFIRM | Stage-1 catches the sequencing inconsistency cleanly; surgery 3 relocates to G.W3 |
| HARDENING-MASTER-PLAN.md:202 (Lock 4) | Negative claim no fused solver (`:1286`) | KEEP | 2 | CONFIRM | Stage-1's surgery 4 is minor + correct |
| HARDENING-MASTER-PLAN.md:225 (Lock 5) | WASM uncovered in trait method count (`:1287`) | REINVENT | 4 | CONFIRM | Stage-1's three-way post-tranche-H verification is concrete; surgery 5 |
| HARDENING-MASTER-PLAN.md:250 (Lock 6) | Path naming per-grammar declaration crate (`:1288`) | REINVENT | 5 | CONFIRM | Stage-1 catches Amendment-01 path conflict; surgery 6 reanchors |
| HARDENING-MASTER-PLAN.md:269 (Lock 7) | Path triplet honoured (`:1289`) | KEEP | 2 | CONFIRM | Honoured by construction; no surgery beyond cross-reference |
| HARDENING-MASTER-PLAN.md:296 (Lock 8) | Per-tranche-wave SOTA enumeration | REINVENT | 4 | CONFIRM | Stage-1's per-tranche enumeration (F/G/H mid-tranche missing) is concrete; surgery 7-9 |
| HARDENING-MASTER-PLAN.md:328 (Lock 9) | Aggregator vs impl verification (`:1291`) | KEEP | 3 | STRENGTHEN | Stage-1's surgery 10 is correct but understates: re-export discipline can mask missing impl entirely; the surgery should also verify the aggregator's `pub use` actually compiles after impl removal (a regression test) |
| HARDENING-MASTER-PLAN.md:357 (Lock 10) | Metadata escape vs directive (`:1292`) | KEEP | 4 | CONFIRM | Stage-1's challenge is precise: metadata is workspace data not grammar source; Lock 10's verbatim text grants the distinction. Honoured. |
| HARDENING-MASTER-PLAN.md:385 (Lock 11) | Submodule operations in §7.2 prelude | REINVENT | 4 | CONFIRM | Stage-1 catches the operational-vs-ratified gap; surgery 11 |
| HARDENING-MASTER-PLAN.md:402 (Lock 12) | Honoured by construction | KEEP | 2 | CONFIRM | Filesystem-greppable; no surgery |
| HARDENING-MASTER-PLAN.md:431 (Lock 13) | grammars/ subdirectory exemption (`:1295`) | REINVENT | 4 | CONFIRM | Stage-1 catches Amendment-01 footnote; surgery 12 |
| HARDENING-MASTER-PLAN.md:469 (Lock 14 ×3) | Three-step ceremony, generic-crate enumeration, Tranche-E gate | REINVENT (×3) + DISCARD (×1) | 5 | CONFIRM | Stage-1's Lock-14 reckoning is the strongest discipline application in the report; surgery 13-16 + multiple cross-refs to Lane 5 |

### §3.2 — Per-item Confirmation-Drift table — Lanes 2-9

| Stage-1 site | Master-plan item | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|
| HARDENING-MASTER-PLAN.md:586 (Lane 2 Tranche C) | path-core scope ambiguity (path/, path-ts/ shells empty during B-F) | REINVENT | 4 | CONFIRM | Stage-1 catches genuine substrate-without-consumer Era-V hazard; surgery 17 reduces shell scope |
| HARDENING-MASTER-PLAN.md:648 (Lane 2 Tranche E) | "9 declaration crates" reanchor | DISCARD + REINVENT (×2) | 5 | CONFIRM | Stage-1's strongest sequencing finding; surgery 18-20 correct |
| HARDENING-MASTER-PLAN.md:702 (Lane 2 Tranche G) | path/ shell empty for 5 tranches | REINVENT | 4 | CONFIRM | Stage-1 catches a real Era-V class fault; surgery 21 |
| HARDENING-MASTER-PLAN.md:770 (Lane 2 H carry-FROM) | DAG diagram disagrees with §5.1 H | REINVENT | 5 | STRENGTHEN | Stage-1 catches the inconsistency cleanly; surgery 22 corrects to "D, E only". Stage-2's strengthen: H may also carry from F (F's optimiser-emitted runtime is consumed by H per master plan §5.2 line 791 "post-tranche-D Emitter trait collapse means TS + WASM share the per-shape walking pattern", and tranche F's auto-detection lives in bbnf-passes which H consumes via the IR boundary). The H carry-FROM should be "D, E, F" not "D, E"; Stage-2 amendment item #79 below. |
| HARDENING-MASTER-PLAN.md:818 (Lane 3 86.07% samply) | structural retirement no measurement | REINVENT | 5 | CONFIRM | Surgery 23 lands per Pass B hardening adjudication |
| HARDENING-MASTER-PLAN.md:836 (Lane 3 168 750 vs 168 020 LOC) | 730-LOC drift | REINVENT | 4 | CONFIRM | Surgery 24 reconciles to CENSUS as authoritative |
| HARDENING-MASTER-PLAN.md:856 (Lane 3 24-vs-33 workspace) | 25-site Amendment 01 surgery | REINVENT (×25) | 5 | CONFIRM | Stage-1's largest concrete surgery cohort; surgery 25-49 are the V2's body work |
| HARDENING-MASTER-PLAN.md:872 (Lane 3 Tranche E close gate) | gate not specified | REINVENT | 5 | CONFIRM | Surgery 50 names three close-gate criteria |
| HARDENING-MASTER-PLAN.md:889 (Lane 3 specialised cohort retiral) | `specialised/` module | REINVENT | 4 | CONFIRM | Surgery 51 reanchors to host-fn composition. Stage-2 deep-dive on this in §5.3 below — the steelman question is whether CSS L4 14-variant OpenFrame is composable from primitives at all. |
| HARDENING-MASTER-PLAN.md:925 (Lane 3 pub use bbnf::*) | glob re-exports | REINVENT | 5 | CONFIRM | Stage-1 catches Lock-14's prior-art critique applying recursively; surgery 53 |
| HARDENING-MASTER-PLAN.md:934 (Lane 3 grammar-agnosticism gate) | bbnf-runtime-template gate | REINVENT | 4 | CONFIRM | Surgery 54 |
| HARDENING-MASTER-PLAN.md:976 (Lane 4 Tranche F SOTA) | mid-tranche perf gate absent | REINVENT | 4 | CONFIRM | Surgery 55 names verbatim ±5% gate |
| HARDENING-MASTER-PLAN.md:984 (Lane 4 Tranche G SOTA) | mid-tranche three-API gate absent | REINVENT | 4 | CONFIRM | Surgery 56 names slice-borrow ≤ 436 µs target |
| HARDENING-MASTER-PLAN.md:993 (Lane 4 Tranche H parity) | parity-numbers gate absent | REINVENT | 4 | CONFIRM | Surgery 57 names byte-for-byte equivalence gate |
| HARDENING-MASTER-PLAN.md:1052 (Lane 5 zero match-arms grep) | confirmed | KEEP | 2 | CONFIRM | Honoured by abstention |
| HARDENING-MASTER-PLAN.md:1077 (Lane 5 Per-X 16-row table) | verification reformat | REINVENT | 4 | CONFIRM | Surgery 58 |
| HARDENING-MASTER-PLAN.md:1124 (Lane 5 future-grammar onboarding) | three-step → two-surface | DISCARD + REINVENT | 5 | CONFIRM | Stage-1's strongest Lock-14 closure invariant catch; surgery 59 |
| HARDENING-MASTER-PLAN.md:1167 (Lane 5 28 site reanchoring) | mass surgery enumeration | REINVENT (×23) + DISCARD (×5) | 5 | CONFIRM | The 28-site count matches Amendment 01's enumeration; surgery is mechanical |
| HARDENING-MASTER-PLAN.md:1219 (Lane 6 730-LOC drift) | §3.1 vs §12.1 reconciliation | REINVENT | 3 | CONFIRM | Surgery 24 cross-ref |
| HARDENING-MASTER-PLAN.md:1238 (Lane 6 trajectory under Amendment 01) | substrate-name reanchor preserves arithmetic | REINVENT | 3 | CONFIRM | Stage-1 verifies the +173 750 end-state survives Amendment 01 |
| HARDENING-MASTER-PLAN.md:1264 (Lane 6 generator window per-wave gates) | window cited but not gated | REINVENT | 4 | CONFIRM | Surgery extends per-wave gate cells; matches `feedback_generated-size-budget` |
| HARDENING-MASTER-PLAN.md:1287 (Lane 6 xtask regen wall) | wall-clock budget absent | REINVENT | 4 | CONFIRM | Surgery extends regen ≤ 5 s; matches `feedback_iter-profile-always` |
| HARDENING-MASTER-PLAN.md:1339 (Lane 6 doc-LOC budget table) | per-cookbook LOC budget | REINVENT | 3 | CONFIRM | Surgery extends §8.2.5 with LOC column |
| HARDENING-MASTER-PLAN.md:1408 (Lane 7 pointer! macro friction) | error verbatim required | REINVENT | 5 | CONFIRM | Surgery 60 lands three error verbatims |
| HARDENING-MASTER-PLAN.md:1452 (Lane 7 lifetime API friction) | when-to-use cookbook | REINVENT | 5 | CONFIRM | Surgery 61 lands lifetime-surfaces cookbook |
| HARDENING-MASTER-PLAN.md:1485 (Lane 7 layout-lowering errors) | error verbatim required | REINVENT | 5 | CONFIRM | Surgery 62 lands three error verbatims for layout-lowering |
| HARDENING-MASTER-PLAN.md:1521 (Lane 7 Pratt + SIMD misfire) | diagnostic CLI | REINVENT | 5 | CONFIRM | Surgery 63 lands cargo xtask diag |
| HARDENING-MASTER-PLAN.md:1561 (Lane 7 crate-split migration) | sed-recipe content | REINVENT | 4 | CONFIRM | Surgery 64 lands verbatim sed-recipes |
| HARDENING-MASTER-PLAN.md:1607 (Lane 7 future-grammar onboarding) | five sub-friction cookbooks | REINVENT | 5 | CONFIRM | Surgery 65-66 land host-fn-composition + test-fixtures cookbooks. **STAGE-2 SOFT-REVERSE candidate**: the metadata schema doc (`docs/spec/codegen.md`) named in Stage-1's reference column may discharge sub-friction 1+2 (metadata schema; source-path conventions) without a separate cookbook. Stage-2 amendment item #80 below queries whether all 5 sub-friction surfaces require cookbooks or whether 3 cookbooks suffice. |
| HARDENING-MASTER-PLAN.md:1677 (Lane 8 bbnf-cli defer) | post-J tranche K | REINVENT | 4 | CONFIRM | Surgery 67 names triple |
| HARDENING-MASTER-PLAN.md:1693 (Lane 8 bbnf-py defer) | triggering condition | REINVENT | 4 | CONFIRM | Surgery 68 names triggering condition |
| HARDENING-MASTER-PLAN.md:1709 (Lane 8 sister-crate submodule) | resolves inline at line 109 | KEEP (cross-ref) | 3 | CONFIRM | Honoured; cross-references Lane 1 punch 11 |
| HARDENING-MASTER-PLAN.md:1725 (Lane 8 cutover Option A vs B) | user adjudication triple complete | KEEP | 3 | CONFIRM | Receiver-blocker-gate triple is named per HARDENING.md §Methodology |
| HARDENING-MASTER-PLAN.md:1741 (Lane 8 docs/restart/ OR-disposition) | adjudicate per Lock 13 | REINVENT | 4 | CONFIRM | Surgery 69 commits to relocation |
| HARDENING-MASTER-PLAN.md:1757 (Lane 8 archive at 1.0) | post-J 1.0-release tranche W0 gate | REINVENT | 3 | CONFIRM | Surgery 70 |
| HARDENING-MASTER-PLAN.md:1815 (Lane 8 Stage-2 hardening receiver) | path stale + Stage-2 silent | REINVENT | 4 | CONFIRM | Surgery 72-73 |
| HARDENING-MASTER-PLAN.md:1869 (Lane 9 §11.1.1 9-crate proliferation) | quick-solution disguise | REINVENT | 5 | CONFIRM | Stage-1's strongest greenfield-discipline finding; cross-ref to Lane 5 |
| HARDENING-MASTER-PLAN.md:1898 (Lane 9 §11.1.3 OR-disposition language) | quick-solution-shaped offering | REINVENT | 4 | CONFIRM | Cross-ref to Lane 8 surgery 69 |
| HARDENING-MASTER-PLAN.md:1908 (Lane 9 §11.1.4 pub use glob) | aggregator footprint | REINVENT | 4 | CONFIRM | Cross-ref to Lane 3 surgery 53 |
| HARDENING-MASTER-PLAN.md:1920 (Lane 9 §11.2.1 Box::leak retiral) | tranche A.W4 silent | REINVENT | 4 | CONFIRM | Surgery 74 |
| HARDENING-MASTER-PLAN.md:1928 (Lane 9 §11.2.2 wildcard @debug) | tranche C.W6 silent | REINVENT | 4 | CONFIRM | Surgery 75 |
| HARDENING-MASTER-PLAN.md:1936 (Lane 9 §11.2.3 defensive fallback) | tranche C.W6 silent | REINVENT | 4 | CONFIRM | Surgery 76 |
| HARDENING-MASTER-PLAN.md:1959 (Lane 9 §11.3 file-level verdicts) | five-bucket ledger | KEEP | 3 | CONFIRM | Master plan composes pass-level findings; appropriate composition |
| HARDENING-MASTER-PLAN.md:2003 (Lane 9 §11.5 architectural transpositions) | six transpositions enumerated | KEEP | 3 | CONFIRM | Honoured |

### §3.3 — Items Stage-1 silenced (potential confirmation-drift faults)

The walk surfaces four master-plan items present in MASTER-PLAN.md that Stage-1 did not explicitly evaluate per-row:

1. **§3.3 archive-vs-no-members ambiguity** (master plan line 173: "`archive/` is NOT a workspace member (per Lock 12)"). Stage-1 ratifies Lock 12 as honoured-by-construction. But the archive ceremony's *consequences* on Cargo.toml `[workspace] exclude = [...]` or absent declaration aren't gated. If `archive/{ser, gorgeous}/` carry their own `Cargo.toml` (preserving build-ability of the archived state), do they need to be in `[workspace] exclude` or merely *not* in `[workspace] members`? Master plan §3.3 line 173 says NOT a member; it does NOT say they're excluded. Stage-1 silence on this is a confirmation-drift fault. Stage-2 amendment item #81 below.

2. **§5.2 Tranche-A.W3 framing for IR Lock-14 retirement** (line 777: "The IR Lock-14 retirement (the seven sites; per Pass A §7 W1) lands here because they block the IR fracture"). Stage-1's Lane 1 Lock 14 catches the Amendment-01 conflict but does not interrogate whether A.W3 retirement is *IR-side scrub only* (scrubbing Pass A's 7 sites) or full-stack retiral (the Pass B 18-site reanchor). The master plan's §11 row 14 verification command extends across 14-15 generic crates including bbnf-codegen (Pass B scope), suggesting A.W3 carries beyond Pass A's seven sites. Stage-1 cohort verdict §2 line 48 says "Tranche A.W3 Lock-14 retirement before Tranche E template-emit is a same-wave-substrate fault unless A.W3 is restated as IR-side scrub only" — so Stage-1 *does* surface this — but the per-item table at line 487 doesn't carry a separate row. The §3 Tranche A entry at line 542 says "All same-tranche consumers; honoured" without specifying scope. Stage-2 amendment item #82 below.

3. **§13 R20 scope-pivot lock cross-reference**: master plan §13 R20 (line 1382) names "Tranche execution agent (per-tranche) introduces scope-pivot mid-tranche" with mitigation "Per `tranche/SPEC.md` §Scope Reveal: open `{LETTER}-II.md` if absorption ceiling exceeded; never absorb silently". Stage-1 silences this — Lane 8 (carry & deferral) does not surface it; Lane 9 doesn't reference it. Per memory item `new-tranche-new-doc` and `2026-04-30 - Scope Pivots Open A New Letter`, this risk is real and the mitigation is project-canonical. Stage-1 should at minimum cross-reference. Stage-2 amendment item #83 below.

4. **CSS L4 14-variant OpenFrame composability under Amendment 01** (cross-pass concern surfaced in HARDENING-PASS-B.md:188-201 fault C.3). The master plan §5.2 line 785 says "specialised cohort retain `specialised/` for extensions only"; Stage-1 surgery 51 reanchors to host-fn composition. But Stage-1 does NOT verify that the 14-variant OpenFrame is decomposable into the 8-primitive `bbnf-host-prims` library. Pass B's hardening report at line 192 explicitly raises this as unresolved: "the 14-variant OpenFrame is more complex than hex-color — it carries 14 typed value variants per CSS construct (Color, Length, Selector, Declaration, etc.). The question whether each variant's runtime construction *composes from primitives* or requires bespoke Rust is unresolved." Master plan Stage-1 surgery 51 reanchors the language but doesn't gate the *feasibility*. Stage-2 amendment item #84 below.

### §3.4 — Lane 2A verdict

| Sub-area | Items audited | CONFIRM | STRENGTHEN | WEAKEN | REVERSE-soft | Stage-2 verdict |
|---|---:|---:|---:|---:|---:|---|
| Lane 1 Lock-Adherence | 16 | 14 | 1 | 1 | 0 | confirmation-drift PARTIAL — Lock 9 verification understates regression test |
| Lane 2 Sequencing | 4 | 3 | 1 | 0 | 0 | confirmation-drift PARTIAL — H carry-FROM amendment to D+E+F not D+E |
| Lane 3 Cohesion | 8 | 8 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Lane 4 SOTA | 3 | 3 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Lane 5 Grammar-Authoritative | 4 | 4 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Lane 6 Generated-LOC + xtask | 5 | 5 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Lane 7 Friction Forecast | 6 | 5 | 0 | 0 | 1 | confirmation-drift PARTIAL — onboarding 5 sub-friction → 3 cookbook check |
| Lane 8 Carry & Deferral | 7 | 7 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Lane 9 Greenfield Discipline | 8 | 8 | 0 | 0 | 0 | confirmation-drift HONOURED |
| Items Stage-1 silenced | 4 unsurfaced | — | — | — | — | confirmation-drift FAULT — surface as amendment items #81-#84 |
| Aggregate | 61 + 4 unsurfaced = 65 | 57 | 2 | 1 | 1 | **PARTIAL** |

**Lane 2A verdict: PARTIAL CONFIRMATION-DRIFT.** Stage-1 honoured the discipline across nine lanes with mechanical thoroughness on Lock 14 + Amendment 01 reconciliation (Lane 5 + Lane 1 Lock 14); the four unsurfaced items + two STRENGTHEN/WEAKEN/REVERSE-soft items are the surface. Stage-2 amendments #79-#84 below address the drift faults; the underlying audit holds.

---

## §4 — Lane 2B — Discipline Lapse Audit

**Lane standard.** Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2B: did Stage-1 honour its own Pro/Con/Explication/Challenge per-item discipline? Are Explication columns paragraph-shaped (good) or one-line (suspicious)? Do Pros and Cons mirror in weight, or does one column dominate? Does the Challenge column carry the steelman counter-argument? Are KEEP verdicts justified explicitly defeating the Challenge, or assumed?

The discipline shape was codified at commit `6e1c6e5f` (2026-05-03 17:24:54). Stage-1 MASTER-PLAN's initial dispatch (commit `fd0c1179`, covering §1-§7) and continuation (commit `17b3db6c`, covering §8-§13, with intermediate commits `70f83795` + `0d30a863` + `bb3f914a`) both occurred AFTER the codification — so per-row Pro/Con/Challenge tables and KEEP/REINVENT/DISCARD verdicts are mandatory throughout.

### §4.1 — Per-lane discipline table

| Stage-1 lane | Per-item rows | Avg challenge strength (1-5) | Discipline verdict | Stage-2 redress |
|---|---:|---:|---|---|
| §3 Lane 1 Lock-Adherence | 16 (one per lock + sub-rows) | 3.2 | HONOURED | Per-row table at lines 124-126, 154-156, 181-183, 204-206, 228-230, 253-255, 272-274, 302-307, 331-333, 360-362, 387-389, 404-406, 435-437, 487-491. Each row carries Site / Item / Explication / Pros / Cons / Challenge / Verdict. Discipline application is mechanical. The §11.5 architectural-transposition row does carry a thinner Challenge ("retraction is naming, not substance") but it cross-references Lane 5's mass surgery for substantive coverage. |
| §4 Lane 2 Sequencing | 10 tranche-level rows + cross-tranche DAG | 3.5 | HONOURED | Per-tranche audit walks A through J; each tranche row gives substantive deliverables, same-wave-or-next-wave consumers, and verdict. Tranche E's DISCARD ("9 declaration crates") is justified explicitly defeating the master-plan body's framing via Amendment 01. |
| §5 Lane 3 Cohesion | 8 orphan claims + 1 orphan deliverable | 3.8 | HONOURED | Each orphan-claim row gives Site / Pros / Cons / Surgery; the per-row table at lines 942-953 summarises with Type + Severity + Surgery columns. Severity column is a Stage-2-friendly addition. |
| §6 Lane 4 SOTA | 4 perf-tranche rows + 1 close-tranche row | 4.0 | HONOURED | Each row's surgery column is concrete and verbatim ("F.W6 close gate adds: 'JSON twitter parse measured before + after Pratt + SIMD auto-detection lands; expected within ±5% of pre-F or improvement; no regression > 5%'"). Verbatim discipline is exemplary. |
| §7 Lane 5 Grammar-Authoritative | 28 reanchoring rows + 5 sub-classification rows + 1 future-grammar onboarding row | 4.5 | HONOURED | The 28-site reanchoring table at lines 1148-1166 is the strongest discipline application; per-X table at lines 1130-1135 is exemplary; the future-grammar test walk-through at lines 1097-1138 is the most mechanically-audit-worthy section in the report. |
| §8 Lane 6 Generated-LOC | 9 rows across 6 sub-lanes | 3.0 | PARTIAL | Per-tranche projection table at lines 1234-1244 carries Tranche / Net delta / Substantive faults columns but the Pro/Con/Challenge structure thins on tranches A-D and I-J (where the verdict is "honoured" or "honoured by abstention"). The §8.4 xtask regen-cycle budget surgery is paragraph-form rather than per-row table; HONOURED structurally but not formatted per-row. Stage-2 redress: minor — the xtask regen surgery should be a single-row table for symmetric reading. |
| §9 Lane 7 Friction Forecast | 6 friction-surface sections with verbatim error tables | 4.7 | HONOURED | Each sub-section carries API surface / Friction enumeration / Required artefacts table / Master plan coverage / Surgery. The verbatim error message commitments (15 across 6 surfaces) are the strongest discipline application in this lane. |
| §10 Lane 8 Carry & Deferral | 6 substantive defers + 18 tranche-level carries + cross-tranche tables | 3.5 | HONOURED with one-row PARTIAL | Per-defer table at lines 1671-1675, 1689, 1715, 1729, 1747 carries Receiver / Blocker / Receiving Gate columns. Defer 3 (sister-crate submodule) is honoured but the row at line 1707-1709 carries a cross-ref-only Challenge ("already in punch-list 11") rather than a fresh steelman; this is the one-row PARTIAL. |
| §11 Lane 9 Greenfield | 9 rows across 6 sub-lanes | 2.8 | PARTIAL | Lane 9 over-relies on cross-references to prior-lane punch-list items (8 of 9 REINVENT rows say "Already in Lane X punch-list item Y"). The cross-ref discipline is correct architecturally but per-row Challenges are thin. Items #74-#76 (Pass-A-inherited workaround retirals) carry fresh Pro/Con/Challenge but items #11.1.1, #11.1.2, #11.1.3, #11.1.4, #11.4 lean on the cross-ref. Stage-2 redress: minor — Lane 9 acknowledges its role is meta-cohort and cross-ref is honest discipline application; thin Challenge is acceptable when the Challenge has been steelmanned in the source lane. |

### §4.2 — Discipline variance between initial dispatch (§3-§7) and continuation (§8-§13)

The orchestrator brief noted the possibility of variance. Stage-2 verifies:

- **Initial dispatch (§3-§7)**: covers Lock-Adherence, Sequencing, Cohesion, SOTA, Grammar-Authoritative. Average challenge strength = 3.8. Per-row Pro/Con/Challenge tables present throughout. The Lane 5 28-site reanchoring is the apex application.
- **Continuation (§8-§13)**: covers Generated-LOC, Friction, Carry & Deferral, Greenfield, Punch list, Final readiness. Average challenge strength = 3.6. Per-row tables present but Lane 9's cross-reference reliance lowers the apparent challenge density.
- **Variance magnitude**: small. The continuation honours the discipline with the same per-row table shape as the initial dispatch. The minor differences (Lane 9's cross-ref leaning; Lane 6's paragraph-form xtask regen surgery; Lane 7's higher challenge strength than Lane 9's) are within the "PARTIAL" band rather than indicating a discipline lapse.

The variance is REAL but SMALL. It is not a structural fault — both dispatches honour the discipline shape; the continuation simply faces a more cross-cohesion-heavy workload (Lane 9 by definition cross-references prior lanes; Lane 8 by definition tabulates carry-receivers many of which are addressed in Lane 5).

### §4.3 — Lane 2B verdict

| Lane | Discipline | Verdict |
|---|---|---|
| 1 Lock-Adherence | Pro/Con/Challenge table per lock + sub-rows; verdicts explicit | HONOURED |
| 2 Sequencing | Per-tranche table + DAG cross-check; verdicts explicit | HONOURED |
| 3 Cohesion | Orphan-claim rows + Severity column; verdicts explicit | HONOURED |
| 4 SOTA | Per-tranche perf-gate rows + verbatim surgery | HONOURED |
| 5 Grammar-Authoritative | 28-site mass-reanchoring + per-X table + future-grammar walk-through | HONOURED (apex) |
| 6 Generated-LOC | Per-tranche projection table; xtask regen surgery in paragraph form | PARTIAL (xtask format) |
| 7 Friction Forecast | Per-surface API/Friction/Artefacts/Surgery sections + 15 verbatim errors | HONOURED |
| 8 Carry & Deferral | Per-defer Receiver/Blocker/Gate triples + tranche-level table | HONOURED with one-row partial |
| 9 Greenfield | Cross-reference to prior-lane surgeries + 3 fresh Pass-A-inherited rows | PARTIAL (cross-ref leaning) |

**Lane 2B verdict: HONOURED with two-row PARTIAL.** Stage-1 MASTER-PLAN applied the Pro/Con/Explication/Challenge per-item discipline + KEEP/REINVENT/DISCARD verdicts mechanically across the nine lanes, modulo two minor discipline lapses: (i) §8.4 xtask regen-cycle budget surgery in paragraph form rather than per-row table (low severity; format-only); (ii) Lane 9's cross-reference-heavy Pro/Con/Challenge density (low severity; honest meta-cohort acknowledgment). Stage-2 redress is minor and additive rather than corrective.

---

## §5 — Lane 2C — Steelman Audit

**Lane standard.** Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2C: for every Stage-1 KEEP verdict, construct the strongest counter-argument the audit could have made. If Stage-1's Challenge column is weaker than the Stage-2 steelman, the KEEP verdict is suspect. Five named architectural decisions per orchestrator brief:

1. The 24-member workspace count under Amendment 01
2. Tranche allocation A-J (10 tranches; 53 waves; 7-9 month calendar)
3. Convergent pivot at Tranche E (Lock 1+13+14 retiring as one)
4. Commit-chain Option 3 (keep verbatim + branch reset)
5. `bbnf-host-prims` as the host-fn home

### §5.1 — Decision 1: 24-member workspace count

**Stage-1 verdict**: KEEP (cross-ref to Lane 5 mass surgery) per `HARDENING-MASTER-PLAN.md:1980` ("The 33-member shape (master plan body, pre-Amendment-01) was *overfit* — per Amendment 01 §'Premise', the 9 per-grammar declaration crates are escape-valve overfitting. 33 → 24 brings the workspace closer to sonic-rs cohesion *while honouring* Lock 13").

**Stage-1 challenge**: cites sonic-rs (1 crate), lightning-css (1 crate), simdjson (1 repo) as cohesion exemplars; argues 24-member shape honours Lock 13's per-directory cohesion at workspace level. Challenge strength: 3.

**Stage-2 steelman alternatives**:

- **Alternative A: 18-member workspace**. Could `bbnf-grammar` + `bbnf-parse` collapse into one crate (sonic-rs has one parser crate, not two)? Could `bbnf-vm` collapse into `bbnf-passes` (the VM is the bytecode pass; arguably part of the pass ensemble)? Could `bbnf-runtime` + `bbnf-runtime-template` collapse (the runtime *is* the template's output)? Could `path` + `path-ts` collapse into `path-core` (proc-macro shells are required separate by Rust; but `path-ts` is a *cdylib* not a proc-macro, so could collapse into `path-core` with feature-gating)? Could `bbnf-error` + `bbnf-pipeline` + `bbnf-host` collapse (each is small; mechanism-only crates)?

- **Alternative B: 30-member workspace**. Could the optimiser sub-crates split further (egraph + egraph-derive + egraph-rules + egraph-cost-model)? Could the runtime split per concern (runtime-builder + runtime-handle + runtime-view)? Could the bench harness split (bench-harness + bench-sota-anchors + bench-vitest-shim)?

**Stage-2 evaluation of steelman**:

- *Alternative A defeats*: `bbnf-grammar` + `bbnf-parse` collapse fails because grammar-source AST is consumed independently of lower-passes (per master plan §3.2 item 1: "the two crates path-dep on each other in a clean direction"). VM collapse fails because Lock 13 forbids files >500 LOC and `bbnf-passes` already approaches budget. Runtime + template collapse fails because the template *emits* runtime output as separate concern (Lock 5 IR-vs-output split). path-ts cdylib collapse into path-core feature-gates breaks Lock 7's settled triplet. The mechanism-crate trio can collapse but at cost of cohesion (one crate with three concerns vs three crates with one concern each — Lock 13 favours the latter).

- *Alternative B defeats*: optimiser sub-split is over-engineering per `feedback_kiss-perf-bias`. Runtime split is over-engineering per Lock 13's "4-10 children at the next level" — bbnf-runtime/{builder, handle, view, error} = 4 children, optimal. Bench split is over-engineering — `feedback_vitest-bench` says "no separate harness, KISS".

- **Steelman verdict**: 24 SURVIVES against both A and B. The 24-member shape is calibrated; Alternative A would force concern-mixing in `bbnf-grammar` + `bbnf-parse`; Alternative B would over-split mechanism crates.

**Stage-1 verdict survives steelman.**

### §5.2 — Decision 2: Tranche allocation A-J (10 tranches; 53 waves; 7-9 month calendar)

**Stage-1 verdict**: KEEP (Lane 2 ratification cross-tranche; per `HARDENING-MASTER-PLAN.md:776-792` "Sequencing-discipline summary"). Stage-1's Lane 2 walks each tranche, identifies 2 REINVENT items (path-core scope; H carry-FROM) but ratifies the 10-tranche allocation in shape.

**Stage-1 challenge**: per-tranche substrate-consumer pattern audit; identifies Tranche E as the substrate centerpiece (the largest single-tranche surface). Challenge strength: 3.

**Stage-2 steelman alternatives**:

- **Alternative A: 8 tranches**. Could B (bbnf-error + bbnf-pipeline) fold into A (workspace genesis)? Could D (codegen IR) fold into C (parse + IR)? Could G (slice-borrow API + pointer macro) fold into E (per-grammar runtime template)? Could I (sister-crate publication) fold into J (close)?

- **Alternative B: 12 tranches**. Could C (parse + IR foundation; 7 waves) split into C-parse + C-IR? Could E (8 waves) split into E-template + E-runtime + E-host-prims? Could H (TS + WASM) split into H-TS + H-WASM?

**Stage-2 evaluation of steelman**:

- *Alternative A defeats*: B-into-A fails because `bbnf-error` + `bbnf-pipeline` foundation must precede `bbnf-grammar` + `bbnf-parse` consumption (per master plan §3.3 dependency depth ordering). D-into-C fails because the codegen IR contract (Phase-4 BC.W0 substrate) is independently architectured from the parse-front IR; merging them dilutes Lock 5. G-into-E fails because slice-borrow API consumes runtime template output (Lock 9 vs Lock 14 are different lock honours; sequenced not merged). I-into-J fails because sister-crate publication needs API-freeze stability before close ceremony measurements.

- *Alternative B defeats*: C-split fails because `bbnf-grammar` and `bbnf-parse` are paired (parse-front consumes grammar AST same wave); 7 waves is reasonable for the 13 god-module SPLITs + Lock 2 fold + bbnf-vm extraction. E-split into 3 fails because the convergent pivot's identity is the *unity* of Lock 1+13+14 retiring together; splitting reintroduces Era-V substrate-then-substrate-then-ship hazard. H-split fails because TS + WASM share the per-shape walking pattern (post-D Emitter trait collapse); splitting duplicates the shared substrate.

- **Steelman verdict**: 10-tranche SURVIVES against both A and B. The 53-wave count is calibrated. The 7-9 month calendar with 10-15% redress slack lands within the suite's 6-12 month band.

**Stage-1 verdict survives steelman.**

### §5.3 — Decision 3: Convergent pivot at Tranche E (Lock 1+13+14 retire as one)

**Stage-1 verdict**: KEEP (architectural-transposition framing per `HARDENING-MASTER-PLAN.md:2003`). Stage-1 cross-references Pass B's claim that the three locks retire through one substrate (template-emit + direct-projection + Emitter coarsening).

**Stage-1 challenge**: §11.5 line 2003 row 1: "retraction is naming, not substance"; the substrate identity is preserved under Amendment 01 (template-emitted subdirs replace per-grammar declaration crates; the convergence holds). Challenge strength: 2.

**Stage-2 steelman alternative**: Could the three locks retire in *staggered* tranches without sacrificing greenfield discipline?

- **Lock 13 first (no god directories)**: A.W4 already does the IR-side cleanup; C.W6 does the final SPLITs. Lock 13 retires AT TRANCHE C, NOT TRANCHE E.
- **Lock 14 second (full grammar generalisation)**: A.W3 retires the 7 IR-side sites; the deeper Lock-14 closure (template-emit zero-grammar-code) needs the runtime template to land. Lock 14 *partial* retires AT TRANCHE A; full closure needs Tranche E's `bbnf-host-prims` + template-emit.
- **Lock 1 third (tape + columnar dead)**: A.W4 does the narrative scrub; the deeper Lock-1 retiral (OpenFrame as the substantive question per Pass B Agent B.3) needs direct-projection emit. Lock 1 *partial* retires AT TRANCHE A; full closure needs Tranche E's direct-projection.

If Lock 13 fully retires at C, Lock 14 fully retires at E, and Lock 1 fully retires at E (alongside Lock 14's E.W6 template-emit gate), the locks DO stagger across tranches — they DO NOT retire as one tranche-bound movement.

**Stage-2 finding**: the "convergent pivot at E" claim is partly mythological. Locks 13 + 14 + 1 *touch* multiple tranches each:
- Lock 1: A.W4 (narrative scrub) + E.W6 (direct-projection emit completes OpenFrame retiral)
- Lock 13: A.W4 (initial cleanup) + C.W6 (final SPLITs)
- Lock 14: A.W3 (7 IR sites) + E.W6 (template-emit zero-grammar-code closure)

The locks share E.W6 as the *final closure point* but they each have *partial closure* in earlier tranches. The convergent-pivot claim conflates "all three reach final closure at E.W6" with "all three retire only at E". The latter is false.

**Stage-1 challenge weakens**: the verdict survives because the master plan's §5.2 line 785 is precise ("the convergent pivot — Lock 1 + Lock 13 + Lock 14 retire as one architectural movement"; *movement*, not *single tranche*). But Stage-1's KEEP at §11.5 reads "the pivot at tranche E" without acknowledging the staggered partial closures. Stage-2 surgery: §11.5 row 1 Pro column extends to "retiral movement spans tranches A.W3 + A.W4 + C.W6 + E.W6; the *closure* converges at E.W6 even if partial closures stagger". Punch-list item #85.

**Steelman verdict: WEAKENED.** The KEEP holds but the framing requires sharpening. The three-lock-as-one claim survives if "as one" means "at one closure point", not "in one tranche".

### §5.4 — Decision 4: Commit-chain Option 3 (keep verbatim + branch reset)

**Stage-1 verdict**: ratifies Pass C's Option 3 outright per `MASTER-PLAN.md:1040`. Stage-1 does not separately interrogate Options 1 (squash to one commit), 2 (squash by tranche), or 4 (multiple feature branches).

**Stage-1 challenge**: provenance preservation; commits-as-lessons; operational cost; reversibility via tag. Challenge strength: 3 (paraphrase from Pass C; not original steelman).

**Stage-2 steelman**:

- **Option 1 (squash)**: a single squash commit at the prelude would make `git log master` carry one commit ("greenfield restart 2026-05-03"). Provenance lives in the tag pre-restart-2026-05-03. Steelman: simpler `git log` post-restart; the prior-chain archaeology is *only* via tag, not via main-branch log. **Defeats**: per `feedback_accurate-perf-narrative` and `perf-breakthrough-accuracy`, specific commit SHAs are cited in memory items; squashing breaks attribution. Defeat: Option 1 fails the project's own attribution discipline.

- **Option 2 (squash by tranche)**: 30 tranche-letter commits (Y, Z, AA, ..., BD). Steelman: tranches are the project's organisational unit; squashing by tranche preserves per-tranche archaeology. **Defeats**: per `era-V-dta-psi-rut.md` archaeology, the substrate-then-substrate-then-ship anti-pattern is COMMIT-LEVEL; squashing by tranche aggregates 7-substrate-failure cohorts into one commit, erasing the failure-anatomy. Defeat: Option 2 fails Era V's lesson preservation.

- **Option 4 (multiple feature branches)**: keep `master` at pre-restart; create `bbnf-greenfield`, `bbnf-tranche-A`, ... feature branches; each tranche a separate branch. Steelman: clean separation; per-tranche review surfaces. **Defeats**: split-brain governance; branch coordination overhead; PR-merge ceremony for every tranche-close. Defeat: Option 4 fails the suite's governance shape (one master plan + sequential tranche execution).

- **Option 3 SURVIVES**: the only option that preserves attribution + lessons + simple operational shape. Tag-based reversibility absorbs the rollback risk.

**Steelman verdict: SURVIVES.** Option 3 wins against all three alternatives. Stage-1's verdict holds; Stage-2 strengthens by enumerating the three steelman defeats above. Punch-list item #86 (optional; sharpens Stage-1's challenge column without changing verdict).

### §5.5 — Decision 5: bbnf-host-prims as the host-fn home

**Stage-1 verdict**: KEEP per Amendment 01 ratification (bbnf-host-prims is the generic primitive library replacing per-grammar host fns).

**Stage-1 challenge**: cites Amendment 01's 8 primitives table (parse_int_radix, parse_float, parse_enum, parse_hex_pair, slice_borrow, cow_unescape, regex_captures, validate_predicate); argues primitives compose to per-grammar host fns via metadata or @host directives. Challenge strength: 3.

**Stage-2 steelman alternative**: could host-fns live in extended-BBNF directives only, with no Rust crate?

- The `@host` directive in extended BBNF (per Amendment 01 §"Host-fn implementations" lines 50-54) shows: `@host parse_hex_color: regex("#[0-9a-fA-F]{6}") -> Color { Color::Rgb(parse_hex_pair($1[1..3]), parse_hex_pair($1[3..5]), parse_hex_pair($1[5..7])) }`. The directive expression carries Rust syntax in `{ ... }` body.
- If the body must be Rust-syntactic, then Rust *primitives* must be defined somewhere. The `parse_hex_pair` reference in the directive body resolves to... what? If `bbnf-host-prims` doesn't exist, the primitives must live inline in each grammar source file or in some other Rust crate.

**Stage-2 evaluation**:

- *Without `bbnf-host-prims`*: each grammar source file would need to define `parse_hex_pair`, `parse_int_radix`, etc. inline (via `@host` blocks) or some other generic Rust crate would absorb the primitives. Either way: the primitives need a *Rust home*. They cannot live exclusively as BBNF directives.

- *With `bbnf-host-prims` as Rust crate*: the 8 primitives are a single `composition_table.rs` module + dispatch. Per-grammar `@host` directives reference the primitive names. The primitives are tested in one location, versioned in one location, used across all grammars.

- **Steelman verdict: SURVIVES.** The "directives only, no crate" alternative fails because Rust primitives need a Rust home. The minimal Rust home for cross-grammar primitives is `bbnf-host-prims`. Honoured by construction.

**Stage-1 verdict survives steelman.**

However, Stage-2 surfaces a sub-question (cross-ref Lane 2A §3.3 item 4): are the 8 primitives sufficient to compose CSS L4's 14-variant OpenFrame? If not, `bbnf-host-prims` needs more primitives, OR the per-grammar `@host` body carries bespoke Rust beyond primitive composition. Stage-2 amendment item #84 covers this.

### §5.6 — Decision 6: Docs re-do six-wave plan (auxiliary architectural decision)

**Stage-1 verdict**: KEEP (per `HARDENING-MASTER-PLAN.md:75` — "the docs re-do plan" survives the lanes; `MASTER-PLAN.md:1187` distributes docs work into A.W3 + A.W6 + C.W3 + C.W4 + C.W7 + J.W4).

**Stage-1 challenge**: Stage-1 surfaces individual fault items (e.g., Lane 8 OR-disposition for `docs/restart/`) but does not interrogate the *six-wave allocation* itself. Challenge strength: 2 (paraphrase ratification of Pass C's plan).

**Stage-2 steelman alternatives**:

- **Alternative A: Three-wave plan**. Could the docs work compress into A.W3 (relocate) + C.W7 (rewrite + new spec) + J.W4 (final synthesis)? Six waves seem high for ~10 KB of docs.

- **Alternative B: Tranche-distributed approach**. Could each tranche author its own per-tranche docs (e.g., Tranche F authors `docs/optimizer/`, Tranche G authors `docs/howto/cookbook/path-macro.md`, Tranche H authors `docs/spec/codegen.md` updates for TS+WASM)? This ensures docs land alongside the substrate they describe.

**Stage-2 evaluation**:

- *Alternative A defeats*: three-wave compression fails because the docs corpus has different concerns (relocation = mechanical = A.W3; rewrite = semantic = C.W3-W4; new specs = synthesis = C.W7; new cookbooks = consumer-aware = G.W2-W3). Each concern needs its own wave. The six-wave plan honours `feedback_doc-style` and `feedback_doc-alongside-code`.

- *Alternative B defeats*: tranche-distributed approach is the actual HARDENING-MASTER-PLAN.md recommendation. Stage-1 surgery 60 (G.W3 path-macro cookbook), surgery 61 (G.W2 lifetime cookbook), surgery 62 (C.W2 layout-lowering page), surgery 63 (F.W3 pratt-simd cookbook), surgery 64 (C.W4 migration page extends), surgery 65-66 (E.W6 host-fn-composition + C.W3 test-fixtures cookbooks per Stage-2 #88) ARE the tranche-distributed approach. The master plan §8.3 "Sequencing per tranche" already names "tranche A.W3 (mechanical) + A.W6 + C.W3 + C.W4 + C.W7 + J.W4" but each cookbook lands at the consuming wave. Honoured.

- **Steelman verdict**: SURVIVES. The six-wave docs allocation maps to distinct concerns (relocation, rewrite, new spec, new cookbook, new migration, final synthesis); tranche-distributed cookbook landings layer on top per Stage-1 surgery 60-66. Honoured.

**Stage-1 verdict survives steelman.**

### §5.7 — Per-decision steelman table

| Decision | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 steelman | Survives steelman? | Stage-2 verdict |
|---|---|---:|---|---|---|
| 24-member workspace count | KEEP | 3 | 18 (collapse) vs 30 (split) — both defeat; Lock 13 + sonic-rs cohesion calibrated | yes | SURVIVES |
| Tranche allocation A-J | KEEP | 3 | 8 (B+A, D+C, G+E, I+J) vs 12 (C-split, E-split, H-split) — both defeat | yes | SURVIVES |
| Convergent pivot at E | KEEP | 2 | staggered closures across A.W3 + A.W4 + C.W6 + E.W6 — partial weakness in framing | yes (with sharpening) | WEAKENED |
| Commit-chain Option 3 | KEEP (paraphrase) | 3 | Options 1 (squash all) + 2 (squash per tranche) + 4 (per-tranche branches) — all defeat | yes | SURVIVES (sharpened) |
| bbnf-host-prims as host-fn home | KEEP | 3 | directives-only no-crate alternative — defeats; Rust primitives need Rust home | yes | SURVIVES |
| Docs re-do six-wave plan | KEEP (paraphrase) | 2 | three-wave (compress) vs tranche-distributed (already implemented) — both defeat | yes | SURVIVES |

### §5.8 — Lane 2C verdict

**Lane 2C verdict: SURVIVES with two sharpenings (one WEAKENED, one strengthened paraphrase).**

The 24-member workspace count survives steelman; the 10-tranche A-J allocation survives steelman; the commit-chain Option 3 survives steelman (Stage-2 strengthens by enumerating the three rejected alternatives); bbnf-host-prims as host-fn home survives steelman (Stage-2 surfaces the CSS L4 14-variant sub-question for Lane 2A amendment).

The "convergent pivot at Tranche E" claim is the one weakness Stage-2 surfaces: the three locks (1, 13, 14) reach *closure* at E.W6 but have *partial closures* staggered across tranches A.W3 + A.W4 + C.W6 + E.W6. The Stage-1 KEEP holds because the master-plan body language ("retire as one architectural movement") is precise; but Stage-1's Pro/Con/Challenge column at §11.5 row 1 understates the staggering. Stage-2 punch-list item #85 sharpens the framing.

No Stage-1 KEEP overturns to REINVENT or DISCARD against Stage-2 steelman.

---

## §6 — Lane 2D — Verdict-Imbalance Audit

**Lane standard.** Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2D: evaluate Stage-1's cohort verdict balance. KEEP / REINVENT / DISCARD distribution across lanes. Pattern of distribution. Threshold flags: BALANCED 60-80% KEEP healthy; OVER-RATIFYING >85% KEEP suggests challenge failure; UNDER-RATIFYING <40% KEEP suggests over-rejection.

### §6.1 — Cohort distribution table

The Stage-1 MASTER-PLAN cohort verdict at §2 line 47-55 enumerates per-lane KEEP / REINVENT / DISCARD counts:

| Lane | KEEP | REINVENT | DISCARD | Total | KEEP fraction | Stage-2 verdict |
|---|---:|---:|---:|---:|---:|---|
| 1 Lock-Adherence | 9 | 4 | 1 | 14 | 64% | BALANCED |
| 2 Sequencing | 8 | 2 | 0 | 10 | 80% | BALANCED (upper-end) |
| 3 Cohesion | 0 | 7 | 1 | 8 | 0% | UNDER-RATIFYING (orphan-claim cohort) |
| 4 SOTA Anchoring | 4 | 3 | 0 | 7 | 57% | BALANCED |
| 5 Grammar-Authoritative | 2 | 28 | 5 | 35 | 6% | UNDER-RATIFYING (Amendment-01 reconciliation cohort; expected) |
| 6 Generated-Code Budget | 6 | 4 | 0 | 10 | 60% | BALANCED |
| 7 Friction Forecast | 0 | 6 | 0 | 6 | 0% | UNDER-RATIFYING (orchestrator-flagged; Stage-2 audits below) |
| 8 Carry & Deferral | 5 | 8 | 1 | 14 | 36% | UNDER-RATIFYING (Amendment-01 cross-cohort) |
| 9 Greenfield Discipline | 6 | 3 | 1 | 10 | 60% | BALANCED |
| **Aggregate** | **40** | **65** | **8** | **113** | **35%** | **UNDER-RATIFYING** |

**Aggregate KEEP fraction = 35%**. Per HARDENING-STAGE-2-EXTERNAL.md threshold: <40% KEEP signals over-rejection. The aggregate signal lands in the UNDER-RATIFYING band.

### §6.2 — Per-lane variance interpretation

- **Lane 1 (64% KEEP)**: balanced. The four REINVENT + one DISCARD cluster around Amendment-01-driven verification command extensions (Locks 1, 6, 13, 14) — appropriate given the master plan body's pre-Amendment-01 language. **NOT over-ratifying** as orchestrator concern suggested; the 64% KEEP is appropriate.

- **Lane 2 (80% KEEP)**: balanced upper-end. Eight tranches ratify by sequencing; two REINVENT (Tranche C path-core scope; Tranche E "9 declaration crates") are concrete. The DAG cross-tranche audit catches the H carry-FROM ambiguity. Stage-2 amendment item #79 amplifies (H carries from D, E, F not D, E only). 80% KEEP is sequencing-honest because tranche allocation is a settled architectural shape.

- **Lane 3 (0% KEEP, 87% REINVENT, 13% DISCARD)**: under-ratifying. Stage-1 surfaces 8 orphan claims; all 8 require surgery. Why no KEEP? Because the lane's standard is "orphan claims" — the per-row table by definition surfaces faults; the *honoured* claims are not on the table. This is a methodological artefact, not a fault. The 0% KEEP is appropriate to the lane's role.

- **Lane 4 (57% KEEP)**: balanced. Four tranche perf-gate slots are n/a + 1 ratified J close + 4 REINVENT (mid-tranche absences). The 57% KEEP treats the n/a slots as KEEP (correct).

- **Lane 5 (6% KEEP, 80% REINVENT, 14% DISCARD)**: under-ratifying. The 28-site Amendment-01 mass-reconciliation drives the under-ratification. **NOT a fault** — the under-ratification reflects the master plan body's pre-Amendment-01 authoring; every reanchoring is an Amendment-01-driven mechanical surgery. Absent Amendment 01, the master plan body's per-grammar-crate language was internally consistent; Amendment 01 retracted it; Stage-1 mechanically reanchors. The 6% KEEP fraction is *exactly* what discipline should produce.

- **Lane 6 (60% KEEP)**: balanced. Per-tranche window cells survive (KEEP); Amendment-01 substrate-name reanchorings + per-wave gate cell additions REINVENT. 60% lower-edge of healthy.

- **Lane 7 (0% KEEP, 100% REINVENT)**: orchestrator-flagged. Stage-2 deep-dive in §6.3.

- **Lane 8 (36% KEEP)**: under-ratifying. Five carries are honoured (defer 3 sister-crate, defer 4 cutover, plus three tranche-level cross-references); eight REINVENT (the substantive defers + Stage-2 hardening receiver + commit prelude); one DISCARD (bbnf-py speculative framing). The under-ratification reflects: (a) Amendment-01-driven reanchorings; (b) Pass-A/B/C-inherited workaround retirals; (c) Stage-2 hardening receiver was silent in master plan body. Concrete + appropriate.

- **Lane 9 (60% KEEP)**: balanced. Five-bucket ledger + architectural-transposition framing survive; quick-solution surfaces + Pass-A-inherited workarounds REINVENT.

### §6.3 — Lane 7 deep-dive: KEEP=0 over-rejection check

The orchestrator brief flags Lane 7 specifically: "Lane 7 has zero KEEP (over-rejecting?)".

Lane 7 covers six friction surfaces (per HARDENING.md §Lane 7 mandatory list):
1. `pointer!` macro syntax
2. `parse / parse_in / parse_owned` lifetime API
3. Layout-lowering errors
4. Pratt + SIMD auto-detection misfire
5. Crate-split migration
6. Future-grammar onboarding (Lock 14 user perspective)

Stage-1 verdict for each: REINVENT (each requires a cookbook + verbatim error commitment). The 100% REINVENT signal is genuine — the master plan body is silent on every named surface.

**Stage-2 steelman of Stage-1's REINVENT verdicts**:

- **Surface 1 (`pointer!` macro)**: Stage-1 surgery 60 commits three error verbatims. Could the macro friction be discharged by `docs/spec/codegen.md` (named in Stage-1's own §8.6 Lane 6 doc table) without a separate cookbook? **Stage-2 finding**: NO. The codegen.md spec describes the IR contract; the `pointer!` macro is a USER surface (sonic-rs convention adoption). Cookbook is required. KEEP=0 is appropriate; REINVENT 1 holds.

- **Surface 2 (lifetime API)**: Stage-1 surgery 61 commits a when-to-use cookbook. Could the friction be discharged by Lock 9's verbatim text alone (the locks are settled; the API is named at master plan §11 row 9)? **Stage-2 finding**: NO. The locks are project-internal; the cookbook is user-facing. The decision tree (slice-borrow vs arena vs owned) is a USER mental model, not a project-substrate concern. Cookbook is required. REINVENT 2 holds.

- **Surface 3 (layout-lowering errors)**: Stage-1 surgery 62 commits three error verbatims. Could the friction be discharged by the rustc-rendered errors (i.e., the layout-lowering pass panic stack trace IS the error)? **Stage-2 finding**: NO. The rustc traceback is internal-substrate-shaped; user-facing error messages MUST be authored to guide the grammar author to the fix (per `feedback_doc-style` and `2026-04-30 - Triumvirate Auto-Triggers` lesson). REINVENT 3 holds.

- **Surface 4 (Pratt + SIMD misfire)**: Stage-1 surgery 63 commits a `cargo xtask diag` subcommand. Could the friction be discharged by the metadata escape valves alone (`pratt_eligibility = "skip"`)? **Stage-2 finding**: NO. The metadata escape requires the user to *know* the misfire happened. Without diagnostic surface, the user has no signal. The diagnostic surface + escape valve are dual mechanisms. REINVENT 4 holds.

- **Surface 5 (crate-split migration)**: Stage-1 surgery 64 commits sed-recipes + dep map. Could the friction be discharged by `cargo doc` rendering the new crate structure? **Stage-2 finding**: NO. Existing consumers' code breaks at compile-time before `cargo doc` is consulted. The migration page must precede the breakage. REINVENT 5 holds.

- **Surface 6 (future-grammar onboarding)**: Stage-1 surgery 65-66 commits two cookbooks (host-fn-composition + test-fixtures). Could the friction be discharged by metadata schema documentation alone (in `docs/spec/codegen.md`)? **Stage-2 finding**: PARTIAL. The schema doc covers metadata schema (sub-friction 1) and source-path conventions (sub-friction 2). The test-fixture conventions (sub-friction 4) and host-fn declaration (sub-friction 3) need separate cookbook. Diagnostic output (sub-friction 5) is CLI surface. **5 sub-frictions → 3 artefacts (codegen.md schema + host-fn-composition cookbook + test-fixtures cookbook) + CLI output**. Stage-1's surgery enumerates 2 cookbooks; Stage-2 affirms 2 cookbooks ARE the right shape (the metadata schema lives in codegen.md, not in a separate cookbook). REINVENT 6 holds with refined cookbook count.

**Lane 7 KEEP=0 verdict: APPROPRIATE.** Each REINVENT survives Stage-2 steelman; the friction surfaces genuinely require artefacts the master plan body lacks. The 0% KEEP is not over-rejection; it is honest discipline-application.

### §6.4 — Aggregate UNDER-RATIFYING signal interpretation

**Stage-2 evaluation**: the 35% aggregate KEEP fraction signals UNDER-RATIFYING per the literal threshold. But the signal decomposes:

- Lane 5 (28 reanchoring sites) drives 28 of the 65 REINVENT items (43%). These are Amendment-01-driven mechanical surgeries; without Amendment 01 they would be KEEP. Adjusting Lane 5 from 6% KEEP to a hypothetical 80% KEEP (pre-Amendment) shifts the aggregate from 35% to ~60%. The under-ratification is *Amendment-01-driven*, not *audit-discipline-driven*.

- Lane 3 (8 orphan claims) drives 8 of the 65 REINVENT items (12%). These are by-definition orphan-claim surfaces; the lane methodology produces 100% REINVENT because the lane's per-row table only carries faults. Adjusting Lane 3's accounting from 0% KEEP to a hypothetical 60% KEEP (counting honoured claims that don't surface) shifts the aggregate further.

- Lane 7 (6 friction surfaces) drives 6 of the 65 REINVENT items (9%). These are genuine — the master plan body is silent.

- Lane 8 (8 carry items) drives 8 of the 65 REINVENT items (12%). Includes Amendment-01 cross-references (3) + Stage-2 hardening receiver (2) + Pass-A-inherited workarounds (3 cross-refs). The Stage-2-receiver REINVENTs are themselves Stage-2-emergent.

**Adjusted KEEP fraction**: removing the Amendment-01-driven Lane 5 + Lane 3's methodological 0% KEEP + Lane 8's Stage-2-cross-references, the underlying KEEP fraction is approximately 62-65%, lying in the BALANCED band.

**Stage-2 verdict**: BALANCED-with-anomalies. The literal aggregate 35% is below the 40% under-ratifying threshold, but the under-ratification is structurally driven (Amendment 01 reconciliation, methodological orphan-claim accounting, Stage-2-emergent receivers). The audit discipline is not over-rejecting; the rejection cluster is concentrated in Amendment-01-mandatory sites.

### §6.5 — Cross-target verdict-imbalance comparison (sister Stage-2 reports)

Per orchestrator brief: "If Stage 2 against PASS-A/B/C complete before you, their Stage-2 punch lists become useful comparison points for verdict-imbalance Lane 2D." The three sister Stage-2 reports landed prior:

| Report | Stage-2 verdict | KEEP fraction (aggregate) | Lane 2D verdict |
|---|---|---:|---|
| HARDENING-STAGE-2-PASS-A | PARTIAL (CONFIRM 12/STRENGTHEN 7/WEAKEN 3) | 58% | BALANCED |
| HARDENING-STAGE-2-PASS-B | RATIFIED with minor amendments | ~60% (estimated) | BALANCED |
| HARDENING-STAGE-2-PASS-C | RATIFIED-pending-amendment | mixed (1 honoured-mostly / 6 partial / 1 violated / 2 n/a) | BALANCED (Lane 9 honoured-mostly justified by periphery scope) |
| **HARDENING-STAGE-2-MASTER-PLAN** (this) | RATIFIED with amendments | 35% literal / 62-65% adjusted | BALANCED-with-anomalies |

The MASTER-PLAN target's 35% literal aggregate KEEP is the lowest of the four — but uniquely this is driven by Amendment-01 mass reconciliation (28 sites of REINVENT). Sister Stage-2 reports verify their target Stage-1 reports also carry Amendment-01 reconciliation cohorts (PASS-A 10 sites; PASS-B 18 sites; PASS-C minimal — periphery scope). The MASTER-PLAN aggregates all 28 master-plan-body sites + 23 cross-pass reanchorings + Lane-7 friction + Lane-8 carry redress. Per cross-comparison: MASTER-PLAN's aggregate REINVENT density is 1.5-2× higher than sister reports because its scope INCLUDES sister-pass-inherited reanchorings PLUS master-plan-body sites PLUS Stage-2-emergent receivers (Stage-2 hardening receiver, V2 reissue gate). The under-ratifying signal is real but justified.

**Cross-target signal**: no over-ratification across any of the four targets; no single Stage-1 over-rejected; the four Stage-1 audits + four Stage-2 verifications uniformly hold. The MASTER-PLAN is the apex: its 76 entries are the V2 surface; its under-ratifying signal is structural; its convergent-pivot WEAKENED is the only steelman finding that touches substantive framing (versus mechanical Amendment-01 reanchoring).

### §6.6 — Lane 2D verdict

**Lane 2D verdict: BALANCED-with-anomalies.**

Stage-1 MASTER-PLAN's verdict distribution lies superficially in the UNDER-RATIFYING band (35% aggregate KEEP, threshold 40%) but the under-ratification is structurally explainable: Amendment-01 reconciliation drives 28 sites of REINVENT (Lane 5); methodological orphan-claim accounting drives 8 (Lane 3); Lane 7 friction surfaces are silently-omitted in master plan body (legitimate REINVENT); Lane 8 Amendment-01 + Stage-2 cross-references add 3-5 emergent REINVENTs. Adjusted underlying KEEP fraction is ~62-65%, BALANCED.

Lane 7's KEEP=0 survives Stage-2 deep-dive — each of the six friction surfaces genuinely requires an artefact the master plan lacks. The over-rejection concern is unfounded.

Lane 1's KEEP=64% lands in the appropriate band given Amendment-01-driven verification command extensions.

No lane signals over-ratification (>85% KEEP); no lane signals discipline failure beyond the Amendment-01-anchored expected reanchorings.

---

## §7 — Lane 2E — Recommendation-Quality Audit

**Lane standard.** Per HARDENING-STAGE-2-EXTERNAL.md §Lane 2E: for every Stage-1 punch-list entry (REINVENT and DISCARD verdicts), evaluate (a) concreteness — verbatim text or hand-wavy; (b) applicability — can a downstream agent execute; (c) scope-correctness — single-line / paragraph / multi-section / re-draft labelled accurately.

Stage-1 MASTER-PLAN's punch list contains 76 entries organised across §12.A through §12.G. The orchestrator brief specifically calls out §12.G as a "cumulative reconciliation table (161 edits across 4 reports)" — does it actually consolidate or just total?

### §7.1 — Per-surgery audit, §12.A Lock-Adherence reconciliation (items 1-22)

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 1 | Extend Lock-1 verification to grep Amendment-01 substrate paths | 5 | 5 | single-line OK | none |
| 2 | Extend Lock-2 verification to verify LayoutSink trait + consumer count ≥ 2 | 5 | 5 | single-line OK | none |
| 3 | Reanchor Lock-3 cookbook from C.W6 to G.W3 | 5 | 5 | single-line OK | none |
| 4 | Extend Lock-4 verification with negative-claim no-fused-solver workspace member | 5 | 5 | single-line OK | none |
| 5 | Extend Lock-5 verification post-tranche-H to three-way Rust+TS+WASM | 5 | 5 | single-line OK | none |
| 6 | Reanchor Lock-6 verification regen targets per Amendment 01 | 5 | 5 | single-line OK | none |
| 7 | Extend Lock-8 row + tranche F gestalt: F.W6 close gate cites verbatim ±5% target | 4 | 5 | paragraph OK | minor: clarify "before + after" baseline measurement is from F.W2 not F.W1 |
| 8 | Extend Lock-8 row + tranche G gestalt: G.W5 close gate cites verbatim three-API target | 4 | 5 | paragraph OK | minor: clarify which lifetime variant gates the close vs measures alongside |
| 9 | Extend Lock-8 row + tranche H gestalt: H.W4 close gate cites byte-for-byte parity | 4 | 5 | paragraph OK | minor: byte-for-byte equivalence is strict; should clarify whitespace/comment normalisation policy |
| 10 | Extend Lock-9 verification to grep bbnf-parse impl alongside aggregator re-export | 5 | 5 | single-line OK | none |
| 11 | Extend §7.2 prelude commits with `git submodule add` for parse-that + bbnf-regex | 5 | 5 | paragraph OK | none |
| 12 | Reanchor Lock-13 verification with Amendment-01 grammars/ exemption footnote | 5 | 5 | single-line OK | none |
| 13 | Strike "creating its declaration crate" step from Lock-14 verification | 5 | 5 | single-line OK | none |
| 14 | Extend Lock-14 generic-crate enumeration with bbnf-host-prims | 5 | 5 | single-line OK | none |
| 15 | Reformat Lock-14 verification as 16-row per-crate table | 4 | 4 | multi-section OK | minor: per-crate table needs a "expected hits" column (zero) to be machine-greppable; add cell |
| 16 | Extend tranche E gestalt with future-grammar onboarding test as close gate | 4 | 5 | single-line OK | minor: which wave (E.W7 or E.W8) carries the onboarding test? Stage-2 specifies E.W8 close per amendment item 19 |
| 17 | Tighten tranche C path-core scope to C.W6 only | 4 | 5 | paragraph OK | minor: ensure G.W3 + H.W2 receivers are explicitly tagged |
| 18 | Reanchor tranche E "9 declaration crates" to template-emitted subdirs | 5 | 5 | paragraph OK | none |
| 19 | Tranche E gestalt names future-grammar onboarding as close gate | 5 | 5 | paragraph OK | none |
| 20 | Tranche E 8-wave breakdown enumerates substrate-consumer sequencing | 3 | 3 | paragraph (vague) | tighten: specify which waves land template (W1-W3) and which exercise consumer (W4-W8) — Stage-2 amendment item #87 |
| 21 | Tranche-A.W2 path/ + path-ts/ shell registration narrows | 4 | 5 | paragraph OK | minor: specify lib.rs stub language ("tranche G implements" / "tranche H implements") |
| 22 | Reconcile §9.2 dependency-DAG with §5.1 H carry-FROM | 4 | 4 | paragraph OK | tighten: H carries from D, E, AND F (Stage-2 §3.2 amendment) — Stage-2 amendment item #79 |

§12.A summary: 22 surgeries; aggregate concreteness 4.5; aggregate applicability 4.7; scope-correctness OK throughout; 5 minor refinements + 2 substantive amendments (#79, #87).

### §7.2 — Per-surgery audit, §12.B Cohesion + Amendment 01 reconciliation (items 23-49)

This is the largest cohort — 27 mass-mechanical Amendment-01 site reanchorings.

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 23 | Tranche E (E.W6) samply gate post-direct-projection | 5 | 5 | single-line OK | none |
| 24 | Reconcile §3.1 LOC sums (168 020) with §12.1 baseline (168 750) | 4 | 4 | paragraph OK | minor: prefer §3.1 update to CENSUS-exact figures |
| 25-49 | 25 mass-mechanical Amendment-01 reanchoring entries | 5 (each) | 5 (each) | single-line/paragraph (each) | none individually; aggregate redress: ensure batch-application order (strike rows before adding new rows) per Amendment 01 §"Master-plan sections superseded" enumeration |

§12.B summary: 27 surgeries; aggregate concreteness 4.9; aggregate applicability 4.9; scope-correctness exemplary; 1 minor refinement; the cohort is the punch list's mechanical apex.

### §7.3 — Per-surgery audit, §12.C Tranche-E close gate + future-grammar test (items 50-59)

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 50 | Tranche E close gate (E.W8) names four close-gate criteria | 4 | 5 | paragraph OK | minor: the four criteria should be a numbered list in the gate spec, not prose |
| 51 | Strike specialised cohort `specialised/` retain language | 5 | 5 | paragraph OK | none |
| 52 | Strike §3.2 reconciliation item 9 (cross-ref 30) | 5 | 5 | paragraph OK | none |
| 53 | Replace `pub use ...::*;` with explicit re-exports (10-15 symbols per crate) | 4 | 4 | multi-section OK | minor: enumerate the canonical 10-15 public symbols per crate (presently un-enumerated; downstream agent must derive) |
| 54 | Tranche E close gate adds bbnf-runtime-template grammar-agnosticism verification | 5 | 5 | single-line OK | none |
| 55 | Tranche F (F.W6) close gate adds JSON twitter parse verbatim ±5% | 5 | 5 | single-line OK | none |
| 56 | Tranche G (G.W5) close gate adds three-lifetime API target | 5 | 5 | single-line OK | none |
| 57 | Tranche H (H.W4) close gate adds byte-for-byte parity | 5 | 5 | single-line OK | none |
| 58 | Reformat Lock-14 verification as 16-row per-crate table (cross-ref 15) | 4 | 4 | multi-section OK | minor: cross-ref scope per item 15 |
| 59 | Future-grammar ceremony three steps → two surfaces (cross-ref 13) | 5 | 5 | single-line OK | none |

§12.C summary: 10 surgeries; aggregate concreteness 4.7; aggregate applicability 4.8; 2 minor refinements (numbered-list + enumerated-symbols).

### §7.4 — Per-surgery audit, §12.D Generated-LOC budget + xtask wall (items 60-66)

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 60 | Tranche G.W3 deliverable list extends with path-macro cookbook + 3 errors | 5 | 5 | paragraph OK | none |
| 61 | Tranche G.W2 deliverable list extends with lifetime-surfaces cookbook + 3 errors | 5 | 5 | paragraph OK | none |
| 62 | Tranche C.W2 deliverable list extends with layout-lowering page + 3 errors | 5 | 5 | paragraph OK | none |
| 63 | Tranche F.W3 deliverable list extends with pratt-simd cookbook + cargo xtask diag | 5 | 5 | paragraph OK | none |
| 64 | Tranche C.W4 migration page extends with verbatim sed-recipes + dep map | 5 | 5 | paragraph OK | none |
| 65 | New cookbook `host-fn-composition.md` lands at C.W3 or E.W6 | 3 | 3 | single-line (vague) | tighten: pick one wave (E.W6 lands `bbnf-host-prims` per Amendment 01 — co-locate cookbook here) — Stage-2 amendment item #88 |
| 66 | New cookbook `test-fixtures.md` lands at C.W3 or E.W6 | 3 | 3 | single-line (vague) | tighten: pick one wave (C.W3 lands bbnf-test-fixtures per master plan §3.1 row 14 — co-locate cookbook here) — Stage-2 amendment item #88 |

§12.D summary: 7 surgeries; aggregate concreteness 4.4; aggregate applicability 4.4; 2 surgeries vague on wave assignment (Stage-2 amendment #88 specifies).

### §7.5 — Per-surgery audit, §12.E Carry & deferral fixes (items 67-73)

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 67 | bbnf-cli defer Notes column extends with post-J tranche K + footnote | 5 | 5 | paragraph OK | none |
| 68 | bbnf-py §3.2 item 7 reads triggering-condition-receiver-gate triple | 5 | 5 | paragraph OK | none |
| 69 | Strike OR-disposition for `docs/restart/`; commit to `docs/process/restart/` | 5 | 5 | single-line OK | none |
| 70 | Archive at 1.0 disposition: post-J 1.0-release tranche W0 with reproducibility test | 4 | 4 | paragraph OK | minor: "reproducible from sister-repo provenance" needs a verification command (gh release tag list?) |
| 71 | §7.2 prelude commit 8 names Amendment 01 | 5 | 5 | single-line OK | none |
| 72 | Update path reference `docs/restart/HARDENING.md` → `restart/prompts/HARDENING.md` | 5 | 5 | single-line OK | none |
| 73 | §15 closing posture extends to name Stage 2 hardening invocation | 5 | 5 | paragraph OK | none |

§12.E summary: 7 surgeries; aggregate concreteness 4.9; aggregate applicability 4.9; 1 minor refinement.

### §7.6 — Per-surgery audit, §12.F Greenfield-discipline workaround retirements (items 74-76)

| # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 74 | Tranche A.W4 names Box::leak retiral at grammar/mod.rs:57 | 5 | 5 | paragraph OK | none |
| 75 | Tranche C.W6 names wildcard @debug retiral at grammar/host.rs:387 | 5 | 5 | paragraph OK | none |
| 76 | Tranche C.W6 names defensive fallback retiral at lower/value_expr/simple_kinds.rs:185 | 5 | 5 | paragraph OK | none |

§12.F summary: 3 surgeries; aggregate concreteness 5.0; aggregate applicability 5.0; exemplary.

### §7.7 — §12.G cumulative reconciliation audit (the orchestrator-flagged section)

The orchestrator brief: *"Particular focus: §12.G's 'cumulative reconciliation table (161 edits across 4 reports)' — does Stage-1 actually consolidate or just total? If the cumulative table doesn't deduplicate against Pass A/B/C punch lists, that is a fault."*

The §12.G table at `HARDENING-MASTER-PLAN.md:2155-2161` reads:

| Report | Entries | Apex faults |
|---|---:|---|
| HARDENING-PASS-A.md | 25 | per-grammar declaration crate sites (10), residue gates (4), friction surfaces (5), cohesion + budget (4), future-grammar test (1), carry triple (1) |
| HARDENING-PASS-B.md | 30 | per-grammar declaration crate reanchorings (18), SOTA gates (5), LOC budget (4), friction surface (1), greenfield posture (2) |
| HARDENING-PASS-C.md | 30 | bbnf-language-server framing (4), cohesion paths (7), LOC budget (4), friction (3), carry triples (3), defer scope (9) |
| **HARDENING-MASTER-PLAN.md** (this) | 76 | Lock reconciliations (22), Amendment 01 reanchorings (27), tranche-E close + future-grammar (10), generated-LOC + cookbook (7), carry & deferral (7), workaround retirals (3) |
| **Cumulative** | **161** | |

**Stage-2 deduplication check**: walking the four reports' apex-fault columns:

- **per-grammar declaration crate sites**: PASS-A (10) + PASS-B (18) + this MASTER-PLAN (27) = 55 sites of Amendment-01 reanchoring across the four reports. Per Amendment 01 §"Master-plan sections superseded" the master plan body has ~28 sites total. The 55 cumulative reanchorings do NOT correspond to 55 distinct master-plan-body sites; they correspond to a smaller number of physical sites with multiple report-level surgery entries. Cumulative count is INFLATED by report duplication.
- **SOTA gates**: PASS-B (5) + this MASTER-PLAN (Lane 4 4 + Lock 8 cell ×3) = ~9 surgery entries, but per the underlying master plan body there are only 4 perf-tranches missing SOTA (F, G, H + J close ratifies) — so 4 actual sites. Cumulative count INFLATED.
- **friction surfaces**: PASS-A (5) + PASS-B (1) + PASS-C (3) + this MASTER-PLAN (Lane 7 6) = 15 surgery entries. Per HARDENING.md §Lane 7 there are 6 mandatory surfaces. Cumulative count INFLATED.
- **carry triples**: PASS-A (1) + PASS-C (3) + this MASTER-PLAN (Lane 8 5+) = 9+ entries; underlying carries are 6 substantive defers + Stage-2 receiver. Cumulative count INFLATED.

**Stage-2 finding**: §12.G is a **TOTAL, not a CONSOLIDATION**. The cumulative 161 is the sum of per-report punch-list lengths; it does NOT deduplicate against the underlying physical sites. The actual physical-site count (deduplicated against master-plan-body line numbers + master-plan close gate cells) is approximately:

- Amendment-01 reanchoring: 28 master-plan-body sites (Amendment 01 enumeration)
- SOTA gates: 4 perf-tranche sites (F, G, H, J close)
- friction surfaces: 6 surfaces
- carry triples: 6 substantive defers + Stage-2 receiver = 7
- Pass-A inherited workarounds: 3 (Box::leak, wildcard @debug, defensive fallback)
- Lock-Adherence verification cell extensions: 14 (one per lock, mostly minor)
- LOC trajectory + xtask wall: 6
- Tranche-E close gate criteria: 4
- Tranche-A.W3 IR-side scrub framing: 1
- Tranche-G path/ shell narrowing: 1
- DAG vs §5.1 H carry-FROM: 1
- §3.2 reconciliation item 9 strike: 1
- pub use glob → explicit re-exports: 1 (with multiple per-crate iterations)
- bbnf-runtime-template grammar-agnosticism gate: 1

Approximate deduplicated physical-site count: ~75-80 distinct physical-site surgeries across the four reports' cumulative work.

**The 161 cumulative count overstates the work by ~2× due to report-duplication.**

**Stage-2 redress**: §12.G needs a deduplicated table organized by underlying physical site, with each report's surgery entry as a column showing the per-report citation. Stage-2 amendment item #77 below.

### §7.8 — Lane 2E verdict

| Sub-cohort | Surgeries | Avg concreteness (1-5) | Avg applicability (1-5) | Scope correctness | Verdict |
|---|---:|---:|---:|---|---|
| §12.A Lock-Adherence | 22 | 4.5 | 4.7 | OK | STRENGTHEN with 5 minor refinements + 2 substantive amendments (#79, #87) |
| §12.B Amendment 01 reconciliation | 27 | 4.9 | 4.9 | exemplary | HONOURED |
| §12.C Tranche-E close + future-grammar | 10 | 4.7 | 4.8 | OK | STRENGTHEN with 2 minor refinements |
| §12.D Generated-LOC + cookbook | 7 | 4.4 | 4.4 | OK | STRENGTHEN with #88 (vague wave assignments) |
| §12.E Carry & deferral | 7 | 4.9 | 4.9 | OK | HONOURED |
| §12.F Workaround retirals | 3 | 5.0 | 5.0 | exemplary | HONOURED |
| §12.G Cumulative reconciliation | (totalling, not surgery) | — | — | TOTAL not CONSOLIDATION | FAULT (Stage-2 amendment #77) |

**Lane 2E verdict: STRENGTHEN.** Stage-1's 76 individual punch-list entries are largely concrete, applicable, scope-correct. The §12.A through §12.F cohorts apply the discipline well. The §12.G "cumulative reconciliation" claim is a TOTAL, not a CONSOLIDATION — the 161 cumulative figure overstates the work by approximately 2× because per-report punch-list entries are not deduplicated against underlying physical sites. Stage-2 amendment #77 reformats §12.G as a per-physical-site cross-reference table.

The 9 minor refinements (items 7-9, 15, 16, 17, 21, 24, 53) are additive sharpenings that the V2 re-issue agent absorbs as part of its single pass; they do not require dedicated amendment-agent dispatch. The 2 substantive amendments (#79 H carry-FROM extends to F; #87 Tranche E 8-wave allocation specifies template-vs-consumer waves) become Stage-2 punch-list items.

---

## §8 — Stage-2 Punch List

Ordered amendments to Stage-1 verdicts and recommendations. Owner: V2 re-issue agent throughout (a single agent applies the punch list alongside Stage-1's 76 entries). Each amendment cross-references its source Stage-2 lane.

### §8.A — Items #77-#80 — Lane 2E + Lane 2A redress

| # | Target | Stage-1 verdict to amend | Stage-2 amended verdict | Reason | Lane |
|---:|---|---|---|---|---|
| 77 | `HARDENING-MASTER-PLAN.md:2155-2161` (§12.G cumulative reconciliation table) | "Cumulative 161 entries" | Reformat as per-physical-site cross-reference table; deduplicate against per-pass entries; specify ~75-80 distinct physical-site surgeries with each report's surgery citation as a column | The §12.G is a TOTAL of report-internal punch-list lengths, not a CONSOLIDATION against underlying physical sites; 161 overstates work by ~2× | 2E |
| 78 | `HARDENING-MASTER-PLAN.md` (introduce new §3.4 cell or extend §3.1) | (no Stage-1 row for this) | Add row for the §3.3 archive-vs-no-members ambiguity: master plan §3.3 line 173 says `archive/` is NOT a workspace member but does NOT specify `[workspace] exclude`; surgery: master plan §3.3 commit explicit `exclude = ["archive/*"]` if `archive/{ser, gorgeous}` carry their own Cargo.toml; otherwise commit explicit assertion that they don't | Lane 2A surfaces unsurfaced master-plan item | 2A |
| 79 | `HARDENING-MASTER-PLAN.md:773` (surgery 22) | "H carries FROM D + E only (G dependency is wrong)" | Amend: H carries FROM D + E + F (F's optimiser pipeline lands cost-model output-piping that bbnf-passes consumes; H's TS+WASM emit consumes through bbnf-passes the optimised IR, so F is in H's transitive carry chain even though G's slice-borrow API surface is NOT) | Lane 2A STRENGTHEN: master plan §5.2 line 791 names "post-tranche-D Emitter trait collapse" as the consumed substrate; F's optimiser also flows through bbnf-passes which H reads | 2A |
| 80 | `HARDENING-MASTER-PLAN.md:1607-1612` (surgery 65-66 + Lane 7 §9.6) | "Five sub-friction surfaces require 2 cookbooks (host-fn-composition + test-fixtures)" | Refine: 5 sub-frictions discharge through (1) `docs/spec/codegen.md` for metadata schema + source-path conventions; (2) `docs/howto/cookbook/host-fn-composition.md` for host-fn declaration; (3) `docs/howto/cookbook/test-fixtures.md` for fixture conventions; (4) `cargo xtask regen` CLI output for diagnostic — total 3 textual artefacts + CLI output | Lane 2A REVERSE-soft: metadata schema lives in spec doc (already in §8.2.5), not separate cookbook | 2A |

### §8.B — Items #81-#84 — Lane 2A unsurfaced item redress

| # | Target | Stage-1 verdict to amend | Stage-2 amended verdict | Reason | Lane |
|---:|---|---|---|---|---|
| 81 | `MASTER-PLAN.md:173` (`archive/` is NOT a workspace member) | (silent) | Master plan §3.3 commits explicit `[workspace] exclude = ["archive/**"]` declaration if archived crates retain Cargo.toml for build-ability of the historical state; otherwise explicit assertion that archived crates carry no Cargo.toml | Lane 2A unsurfaced item: workspace-member-vs-excluded ambiguity | 2A |
| 82 | `MASTER-PLAN.md:777` (Tranche A.W3 IR Lock-14 retirement framing) | (silent on scope) | Tranche A.W3 surgery clarifies: A.W3 retires the 7 Pass-A IR-side sites (per Pass A §7 W1) — IR-side scrub. The deeper Lock-14 closure (template-emit zero-grammar-code; the Pass B 18-site reanchor) lands at E.W6 with full-stack template-emit. A.W3 is NOT Pass-B-scope retiral. | Lane 2A unsurfaced item: scope of A.W3 framed unclearly | 2A |
| 83 | `MASTER-PLAN.md:1382` (R20 scope-pivot risk) | (silent on cross-references) | §13 R20 mitigation extends to cross-reference `tranche/SPEC.md` §Scope Reveal AND memory item `feedback_new-tranche-new-doc` AND `2026-04-30 - Scope Pivots Open A New Letter` lesson; specify absorption ceiling = 2 paths + unchanged hard gate | Lane 2A unsurfaced item: R20 lacks operational cross-references | 2A |
| 84 | `MASTER-PLAN.md:785, 1167` (specialised cohort + 14-variant OpenFrame) | Stage-1 surgery 51 reanchors language but doesn't gate feasibility | Tranche E close gate adds CSS L4 14-variant OpenFrame composability proof: post-template-emit, all 14 variants either (a) compose from `bbnf-host-prims` 8 primitives expressed in metadata, OR (b) compose from `@host` directive bodies in `grammar/css-l4/css-l4.bbnf` referencing primitives. If neither, surface a Tranche-E.W4 design-block triumvirate: extend `bbnf-host-prims` primitive count, OR ratify per-grammar `@host` body bespoke Rust as Lock-14 escape valve. Cross-reference Pass B Agent B.4 §Q2 / hardening-PASS-B fault C.3 | Lane 2A unsurfaced item: feasibility silence | 2A |

### §8.C — Items #85-#86 — Lane 2C steelman redress

| # | Target | Stage-1 verdict to amend | Stage-2 amended verdict | Reason | Lane |
|---:|---|---|---|---|---|
| 85 | `HARDENING-MASTER-PLAN.md:2003` (§11.5 architectural-transposition row 1 Pro/Con/Challenge) | "retraction is naming, not substance" | Pro column extends to: "retiral movement spans tranches A.W3 + A.W4 + C.W6 + E.W6; the *closure* converges at E.W6 even if partial closures stagger. Lock 1 partial closes at A.W4 (narrative scrub) + full closes at E.W6 (direct-projection emit retires OpenFrame); Lock 13 partial closes at A.W4 + full closes at C.W6 (final SPLITs); Lock 14 partial closes at A.W3 (7 IR sites) + full closes at E.W6 (template-emit). The 'as one' framing means 'reach final closure at E.W6', not 'retire only at E'." | Lane 2C WEAKENED: convergent pivot framing understates staggered partial closures | 2C |
| 86 | `HARDENING-MASTER-PLAN.md:2138` (surgery 73 closing posture) | "§15 closing posture extends to name Stage 2 hardening invocation" | Stage-1 surgery is correct; Stage-2 strengthens by suggesting §15 closing posture additionally enumerate the three rejected Options (1, 2, 4) with one-sentence-each defeat — making Stage-1's challenge column thicker and self-documenting against future re-litigation | Lane 2C SURVIVES (sharpened) | 2C |

### §8.D — Items #87-#88 — Lane 2E surgery refinement

| # | Target | Stage-1 verdict to amend | Stage-2 amended verdict | Reason | Lane |
|---:|---|---|---|---|---|
| 87 | `HARDENING-MASTER-PLAN.md:2065` (surgery 20 Tranche E 8-wave breakdown) | "enumerate substrate-consumer sequencing" (vague) | Tighten: E.W1 lands `bbnf-host-prims` skeleton (the 8 primitives + composition table); E.W2 lands `bbnf-runtime-template` skeleton (template emitter shell); E.W3 lands per-grammar metadata composition declarations + `@host` directive parser; E.W4 lands direct-projection emit substrate (one grammar — JSON — round-trips); E.W5 extends template-emit to specialised cohort (bbnf, css-l4, sheets); E.W6 extends to trivial cohort (bnf, csv, ebnf, css-pretty, math); E.W7 retires `crates/core/src/runtime/<grammar>/` hand-written modules; E.W8 close gate fires (future-grammar onboarding test, samply gate, grammar-agnosticism gate, samply <1% checkpoint share) | Lane 2E STRENGTHEN: Stage-1 surgery 20 was vague at paragraph level | 2E |
| 88 | `HARDENING-MASTER-PLAN.md:2125-2126` (surgery 65-66 wave assignment vagueness) | "lands at C.W3 or E.W6" | Specify: `host-fn-composition.md` lands at E.W6 (co-locates with `bbnf-host-prims` substrate completion); `test-fixtures.md` lands at C.W3 (co-locates with `bbnf-test-fixtures` workspace member creation per master plan §3.1 row 14) | Lane 2E STRENGTHEN: Stage-1 surgery 65-66 OR-disposition mirrors the quick-solution language Stage-1 itself catches in Lane 9 | 2E |

### §8.E — Stage-2 punch-list summary

The 12 Stage-2 amendments (#77-#88) integrate with Stage-1's 76-entry punch list. Categorised:

| Lane source | Items | Substantive impact |
|---|---|---|
| 2A Confirmation-Drift | #78, #79, #80, #81, #82, #83, #84 (seven) | Surface 4 unsurfaced master-plan items + STRENGTHEN H carry-FROM + REVERSE-soft sub-friction-cookbook count |
| 2B Discipline Lapse | (no items; HONOURED with two-row PARTIAL absorbed in V2 reformatting) | n/a |
| 2C Steelman | #85, #86 (two) | Sharpen convergent-pivot framing + closing-posture enumerate-rejected-options |
| 2D Verdict Imbalance | (no items; BALANCED-with-anomalies; structural explanation suffices) | n/a |
| 2E Recommendation Quality | #77, #87, #88 (three) | Reformat §12.G cumulative table + tighten Tranche-E wave breakdown + tighten cookbook wave assignments |

**Stage-2 punch-list aggregate**: 12 amendments. The V2 re-issue agent absorbs them alongside Stage-1's 76 entries to produce MASTER-PLAN-V2.md. Of the 12, only 7 are user-visible amendments to the punch list itself (the rest are re-grouping of existing surgeries or extensions to existing rows).

---

## §9 — Final readiness

> **Stage-2 Decision: STAGE-1 RATIFIED with amendments required.**
>
> Stage-1 MASTER-PLAN's audit-quality holds. The 76-item punch list is the V2 re-issue agent's working surface; the Amendment-01 mass reconciliation cohort (items 23-49) survives Stage-2 verification line-by-line; the per-tranche-wave SOTA gates for F+G+H mid-tranche are correctly demanded; the friction-surface enumeration is exhaustive against the six mandatory surfaces; the carry-deferral triples for `bbnf-cli`, `bbnf-py`, `docs/restart/`, archive deletion, and Stage-2 hardening receiver are correctly identified; the Pass-A-inherited workaround retirals (Box::leak, wildcard @debug, defensive fallback) are correctly named with tranche-wave receivers. The substantive shape — 24-member workspace, 10-tranche A-J allocation, convergent-pivot at E (with Stage-2 framing sharpening), commit-chain Option 3, bbnf-host-prims as host-fn home — survives Lane 2C steelman against the strongest counter-arguments.
>
> Where Stage-1 falls short is mechanical, not structural: §12.G "cumulative reconciliation" tabulates 161 cumulative edits without deduplicating against per-pass punch lists (Stage-2 amendment #77 reformats); 4 master-plan items go unsurfaced for per-row evaluation (items #81-#84 surface them); Lane 9's cross-reference-heavy Pro/Con/Challenge density is honest discipline acknowledgment rather than fault but Lane 2B logs it; the convergent-pivot Lane 2C verdict WEAKENS against staggered-closure steelman (item #85 sharpens the framing); H carry-FROM extends from D+E to D+E+F (item #79). The Lane 2D under-ratifying signal is structurally explainable (Amendment-01 reconciliation drives 28 of 65 REINVENTs; methodological orphan-claim accounting drives 8; Lane 7 friction surfaces genuinely require artefacts) — the audit is not over-rejecting; the rejection cluster is concentrated in Amendment-01-mandatory sites.
>
> The 12 Stage-2 amendments (#77-#88) integrate with Stage-1's 76-entry punch list. Cumulatively, the V2 re-issue agent receives 88 punch-list items (76 from Stage-1 MASTER-PLAN + 12 from this Stage-2) plus 25 from sister Stage-1 PASS-A + 30 from sister Stage-1 PASS-B + 30 from sister Stage-1 PASS-C + adjusted Stage-2-PASS-A + Stage-2-PASS-B + Stage-2-PASS-C amendments. After deduplication against underlying physical sites (Stage-2 amendment #77's reformatted §12.G table), the actual physical-site surgery count is approximately 90-110 distinct surgeries. The 161 + 12 cumulative figure overstates the V2 work by approximately 1.5-2×.
>
> Stage-1 MASTER-PLAN does NOT require re-audit. Its substantive findings survive Stage-2 scrutiny robustly. The 14 locks remain settled; the precepts remain settled; Amendment 01 remains authoritative. Stage-2 verifies; Stage-2 does not relitigate. The greenfield mandate carries through; the convergent-pivot identity (with Stage-2 sharpening of "as one closure point" rather than "in one tranche") is preserved; the workspace shape (24 members) is calibrated against sonic-rs / lightning-css / simdjson cohesion exemplars; the tranche allocation (A-J; 53 waves; 7-9 month calendar) survives both 8-tranche and 12-tranche steelmen; the commit-chain Option 3 wins against Options 1, 2, 4 on attribution-preservation + lessons-preservation + operational-simplicity grounds.
>
> Hereupon the V2 re-issue agent applies the cumulative punch list (Stage-1's 76 + Stage-2's 12 + sister Stage-1 reports' 85 + sister Stage-2 reports' amendments) to produce `MASTER-PLAN-V2.md`. Amendment 01 absorbs into the V2 body wholesale; the 28 retracted-substrate sites reanchor; the future-grammar onboarding test crystallises as a Tranche-E close gate; the Lock-14 verification command extends to 16 generic crates including `bbnf-host-prims`; the per-tranche perf gates land at F.W6 + G.W5 + H.W4 (mid-tranche) plus J close (terminal); the friction surfaces gate at C.W2 + C.W4 + E.W6 + F.W3 + G.W2 + G.W3; the carry triples (bbnf-cli + bbnf-py + archive disposition) name receivers + blockers + gates; the Pass-A-inherited workaround retirals land at A.W4 + C.W6 (×2). With V2 + Amendment 01 committed and the four Stage-2 reports landed, tranche drafting opens — the 10 tranche-drafting agents read V2 + Amendment 01 + their tranche-specific stubs at `restart/tranches/{X}/{X}.md`; the per-tranche execution agents draft full waves under each stub; tranche execution is out of suite scope. The plan from here is the V2 re-issue agent's; the substrate is settled; Stage 1 + Stage 2 hardening have closed; the greenfield mandate remains the disposition.




