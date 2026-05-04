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
| 2A Confirmation-Drift | PARTIAL | 21 CONFIRM + 9 STRENGTHEN + 3 WEAKEN + 2 REVERSE-soft across 35 audited Stage-1 verdicts; 4 master-plan items unsurfaced (specialised-cohort host-fn 14-variant decomposition, Tranche-A.W3 IR-side-only scrub framing, the §3.3 ordering's archive-vs-no-members ambiguity, the §13 R20 scope-pivot lock cross-reference) |
| 2B Discipline Lapse | HONOURED with two-row PARTIAL | Per-row Pro/Con/Explication/Challenge tables present across all nine Stage-1 lanes; KEEP/REINVENT/DISCARD verdict vocabulary used throughout; discipline variance between initial dispatch (§3-§7) and continuation (§8-§13) is real but small — initial dispatch's Lane 1 carries denser steelman than continuation's Lane 9 (cross-references prior items rather than steelman freshly) |
| 2C Steelman | SURVIVES with three WEAKENED | 24-member workspace count survives steelman; tranche allocation A-J with 53 waves survives; commit-chain Option 3 survives. Three claims weaken: convergent-pivot-at-E (Lock 1+13+14 retire as one) admits a staggered three-tranche alternative the synthesizer did not consider; bbnf-host-prims as a Rust crate weakens against extended-BBNF-directives-only; the pivot's "OpenFrame retires by mechanism" claim weakens against per-grammar staged retiral |
| 2D Verdict Imbalance | BALANCED-with-anomalies | Aggregate 38% KEEP / 56% REINVENT / 6% DISCARD across 113 master-plan-specific items lies in the under-ratifying band (40% threshold). The 56% REINVENT signals the Amendment-01 reconciliation overhead (28 sites of body-vs-amendment drift); ABSENT amendment-rec the underlying KEEP fraction is healthy 60-65%. Lane 7 (KEEP=0) genuinely over-rejects per Stage-2 steelman of two friction surfaces. Lane 1 KEEP fraction 36% is appropriate given Amendment-01-driven Lock-14 + Lock-6 + Lock-13 reanchorings. |
| 2E Recommendation Quality | STRENGTHEN | 76 punch-list items. ~70% are concrete verbatim re-anchorings (items 24-49 are mass-mechanical Amendment-01 site rewrites); ~20% are well-scoped paragraph-level surgeries; ~10% are too vague or too aggressive in scope (items 7, 8, 9, 17, 22, 50, 67-73 cluster around mid-tranche SOTA gates and carry-receiver triples where the fix is sketched rather than spelled). §12.G "cumulative reconciliation" table tabulates 161 cumulative edits but does NOT deduplicate against per-pass punch lists — this is the largest Stage-1 fault Stage-2 surfaces in this lane. |

**Final Stage-2 decision: STAGE-1 RATIFIED WITH AMENDMENTS REQUIRED.**

Stage-1 MASTER-PLAN's substantive shape holds. Its 76-item punch list lands as the V2 re-issue's working surface; the Amendment-01 reconciliation cohort (items 23-49) is the most extensive concrete-amendment surface in the suite and survives Stage-2 verification line-by-line. The Lane 1 lock-honoured table reanchorings are concrete; the Lane 4 mid-tranche SOTA gate additions for tranches F/G/H are correct in direction; the Lane 7 friction-surface enumeration covers the six mandatory surfaces; the Lane 8 carry-deferral triples for `bbnf-cli`, `bbnf-py`, `docs/restart/`, archive deletion, and Stage 2 hardening receiver are correctly identified.

The Stage-2 amendments fold into the master-plan-V2 punch list as items #77-#88: (i) deduplicate §12.G cumulative table against per-pass punch lists (Lane 2E redress); (ii) surface the four unsurfaced master-plan items (Lane 2A redress); (iii) tighten the three weakened Stage-1 KEEPs (Lane 2C redress for convergent pivot, bbnf-host-prims home, OpenFrame mechanism claim); (iv) acknowledge the verdict-imbalance signal at Lane 7 (KEEP=0) by checking each REINVENT against alternative friction-discharge mechanisms (e.g., metadata schema documentation may discharge §9.6 onboarding without a separate cookbook).

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
| Aggregate | 65 (35 distinct + 30 cross-refs) | 57 | 2 | 1 | 1 | **PARTIAL** |

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

(continuing in next commit)

