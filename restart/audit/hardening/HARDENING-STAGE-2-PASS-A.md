# Hardening Stage 2 — Pass A (Parse Front)

Date: 2026-05-03
Stage-2 target: `restart/audit/hardening/HARDENING-PASS-A.md` (commit `54018ac3`; 909 lines)
Underlying target: `restart/audit/passes/PASS-A.md` (829 lines; commit-line cited per dispatch)
Auditor: Stage-2 hardening agent under `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`
Hard cap: 45 minutes; this report committed before the cap.

---

## §1 — Target identification

This Stage-2 pass evaluates Stage-1 PASS-A's hardening of the Pass A
synthesis. The Stage-1 report at
`/Users/mkbabb/Programming/bbnf-lang/restart/audit/hardening/HARDENING-PASS-A.md`
(909 lines, committed at `54018ac3` on 2026-05-03 17:28 UTC-04:00)
ratifies, surfaces, or recommends amendment against the underlying
Pass A synthesis at `/Users/mkbabb/Programming/bbnf-lang/restart/audit/passes/PASS-A.md`
(829 lines).

Stage-2's remit is the Stage-1 audit-quality, not the Pass A synthesis
substance. The five Stage-2 lanes (2A Confirmation-Drift, 2B Discipline
Lapse, 2C Steelman, 2D Verdict-Imbalance, 2E Recommendation-Quality)
apply per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md` §Stage-2 Lanes.

Material temporal fact bearing on Lane 2B: the discipline amendment
codifying Pro/Con/Explication/Challenge plus KEEP/REINVENT/DISCARD
verdicts landed at commit `6e1c6e5f` (2026-05-03 17:24:54), four minutes
BEFORE Stage-1 PASS-A committed at `54018ac3` (2026-05-03 17:28:01).
The orchestrator's prior framing ("Stage-1 PASS-A was authored BEFORE
the HARDENING.md amendment") is incorrect — Stage-1 PASS-A was authored
AFTER the amendment landed in the prompt; the auditor's failure to
apply the discipline is therefore a substantive Lane 2B violation, not
a contract-mismatch artefact.

The 14 locks at `restart/locks/14-LOCKS.md` are settled. Amendment 01
at `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` is
settled (no per-grammar declaration crates as default; 24-member
workspace; bbnf-host-prims + template-emitted runtime subdirs).

---

## §2 — Cohort verdict

| Lane | Stage-2 verdict | Notes |
|---|---|---|
| 2A Confirmation-Drift | PARTIAL | 12 CONFIRM + 7 STRENGTHEN + 3 WEAKEN across 22 Stage-1 verdicts; 3 items unsurfaced (bootstrap-shim consumer-enumeration; per-facility challenges; bucketization sample audit) |
| 2B Discipline Lapse | VIOLATED | Pro/Con/Explication/Challenge columns absent throughout; KEEP/REINVENT/DISCARD verdicts replaced by honoured/violated/silent — direct contract violation; Lane 9 has no per-item table |
| 2C Steelman | PARTIAL | 7 SURVIVE / 5 WEAKENED / 1 partly DEFEATED with already-pending Stage-1 surgery; no Stage-1 KEEP overturned to REINVENT or DISCARD |
| 2D Verdict-Imbalance | BALANCED | aggregate 58% KEEP / 42% REINVENT / 0% DISCARD across 81 mapped items lies in the 60-80% healthy band; no under-ratifying or over-ratifying systemic signal except the missing-DISCARD vocabulary gap (Lane 2B redress) |
| 2E Recommendation-Quality | HONOURED-PARTIAL | 22/25 surgeries are concrete (verbatim re-anchorings); 3/25 (items 14, 17, 20) are too vague at the residue-ledger boundary |

**Final Stage-2 decision: STAGE-1 AMENDMENTS REQUIRED.** Stage-1 PASS-A's
substantive findings hold; the 25-item punch list largely lands. The
Lane 2B discipline lapse is mechanical — Stage-1 used the Stage-1
verdict vocabulary (honoured / violated / silent) inherited from prior
hardening synthesis precedent rather than the freshly-codified
KEEP / REINVENT / DISCARD plus Pro/Con/Explication/Challenge per-item
shape mandated 4 minutes earlier. The substantive Stage-1 work is
sound; the discipline shape is not.

The Stage-2 amendments are: (i) restate Stage-1's Lane verdicts in the
KEEP / REINVENT / DISCARD vocabulary so the master-plan synthesizer
reads a uniform verdict surface; (ii) add the 3 confirmation-drift
items Stage-2 surfaces (the bootstrap-shim consumer-enumeration gap;
per-facility challenges for the 8 §3 new facilities; sample audit of
§1 bucketization criteria); (iii) tighten 4 of 25 Stage-1 punch-list
surgeries Stage-2 marks as concreteness < 4 (items 14, 17, 18, 20);
(iv) add 4 steelman-driven gates the WEAKENED Lane 2C verdicts surface
(Lock 2 retired-term coverage, Lock 3 eager-empty-path elision
gating, Lock 10 cost-model relocation gating, Lock 13 SPLIT-count
reconciliation). These 13 amendments (#26-#38) fold into Stage-1's
existing 25-item punch list to form a 38-item reconciled punch list.

Stage-1 PASS-A does NOT require re-audit — its substantive findings
survive Stage-2 scrutiny robustly. The 10-site Amendment 01 reconciliation
is comprehensive; the 6 friction surfaces are exhaustive; the Lock 9
deferral fault is correctly identified and surgically resolved. The
faults are amenable to a single Stage-2 amendment agent's pass; the
master-plan V2 re-issue agent receives both Stage-1 and Stage-2 punch
lists alongside Pass A and Amendment 01.

### Cohort distribution at a glance

The 81 Stage-1 items mapped onto KEEP / REINVENT / DISCARD distribute
58% / 42% / 0% across the nine lanes. The 0% DISCARD signal is
suspicious in vocabulary but not in substance: Stage-1's "retract"
surgery items (Lane 5 surgery #4, #7, #9 — retracting per-grammar
declaration crates) are functionally DISCARD-with-replacement verdicts
that Stage-1 phrases as surgery rather than verdict. The Stage-2
amendment vocabulary fix surfaces these as DISCARD verdicts.

---

## §3 — Lane 2A — Confirmation-Drift

Stage-2 evaluates whether Stage-1 PASS-A carried the target's framing
implicitly — ratifying items because Pass A's surrounding paragraphs
framed them favourably rather than because each item survives the
adversarial steelman the Stage-1 contract requires.

### Per-item table

| Stage-1 site | Target item | Stage-1 verdict | Challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|
| HARDENING-PASS-A.md:74 | Lock 1 (tape dead) at PASS-A.md:415 — "substantively-honoured; ~9 narrative-residue scrubs" | honoured | 1 | CONFIRM | scope-correct; the 9 residues are mechanical comment scrubs; no steelman exists for "tape lives somewhere" |
| HARDENING-PASS-A.md:90 | Lock 2 at PASS-A.md:416 — Layout rename in W2 | honoured (surgery scheduled) | 2 | CONFIRM | substantive Lock 2 retirement is the named W2 wave; rename pass is proper architectural transposition |
| HARDENING-PASS-A.md:100 | Lock 3 at PASS-A.md:417 — cursor-parse + byte-skip | honoured | 1 | WEAKEN | Stage-1's challenge is too thin: Stage-1 simply says "Pass A scope is the lowering side; consult-site is in path/cursor.rs". The target item could fail by Pass A *not* explicitly calling out that the eager-empty-path elision (`__EAGER_EMPTY_PATH`) is preserved in the new path-core layout. Steelman: the relocation to `path-core/src/runtime/cursor.rs` could lose the eager elision contract if the SPLIT mechanically drops the case. Stage-1 ratifies-by-scope rather than by gate citation. |
| HARDENING-PASS-A.md:110 | Lock 4 at PASS-A.md:418 — orthogonal optimisation | honoured | 1 | CONFIRM | Proposal 2 fracture preserves the boundary; the four optimisation sub-trees route to bbnf-passes/ |
| HARDENING-PASS-A.md:125 | Lock 5 at PASS-A.md:419 — IR + per-backend lower (one redress) | honoured | 2 | CONFIRM | path-string relocation is concrete (line 130-185 → bbnf-codegen/src/rust/) |
| HARDENING-PASS-A.md:133 | Lock 6 at PASS-A.md:420 — xtask emits committed source | honoured | 1 | CONFIRM | scope-correct; Pass A is pre-codegen |
| HARDENING-PASS-A.md:144 | Lock 7 at PASS-A.md:421 — path triplet | honoured | 2 | CONFIRM | Proposal 3 maps cleanly to Lock 7's named footnote; W3 punch-list executes |
| HARDENING-PASS-A.md:152 | Lock 8 at PASS-A.md:422 — surpass SOTA | honoured (n/a) | 1 | CONFIRM | Pass A is pre-codegen; correct scoping |
| HARDENING-PASS-A.md:180 | Lock 9 at PASS-A.md:423 — Box::leak deferral | violated (deferral fault) | 5 | STRENGTHEN | Stage-1 caught a real deferral fault; the surgery (pick option b — `parse_grammar_in(input, &bump)`) survives Stage-2. Stage-1's challenge is strong: cites Lock 9 line 50 explicitly, names option a's drawback (public-API break), names option b's compensating value (ergonomics). |
| HARDENING-PASS-A.md:188 | Lock 10 at PASS-A.md:424 — Pratt + SIMD auto-detect | honoured | 1 | WEAKEN | Stage-1 says "miners carry the detection; no @pratt/@simd directive observed". The steelman: Pass A's auto-detection happens in the existing miners (pratt.rs, operator_chain.rs, pattern_alphabet.rs) — but those miners are themselves currently in `crates/ir/src/passes/recognizers/`. After Proposal 2 fracture, do they relocate to bbnf-passes/ in the same coordinated wave? Pass A line 425-432 ratifies the relocation but does not specifically gate that pratt.rs's detection logic survives the move. Stage-1 doesn't ask. |
| HARDENING-PASS-A.md:212 | Lock 11 at PASS-A.md:425 — path-deps mechanism vagueness | honoured-with-mechanism-vagueness | 4 | STRENGTHEN | Stage-1's surgery is concrete (`cargo metadata` gate). The "synthesizer adjudicates" pattern Stage-1 surfaces here is a real audit catch. |
| HARDENING-PASS-A.md:235 | Lock 12 at PASS-A.md:426 — ser + gorgeous archive | silent (must-add to residue ledger) | 3 | CONFIRM | Stage-1 catches a real cross-pass dependency that Pass A omits from §5 |
| HARDENING-PASS-A.md:254 | Lock 13 at PASS-A.md:427 — no god directories | honoured | 2 | WEAKEN | Stage-1's challenge accepts Pass A's own enumeration of 13 SPLITs without verifying all 13 land in W2/W6/W7 punch list. Steelman: the SPLIT obligation table at PASS-A.md:427 cites ~13 sites; Stage-1 cross-references W2 items 12-23 + W6 items 34-37 + item 39, totalling ~12 items — does that match Pass A's 13? Stage-1 doesn't audit. The Lock 13 ratification carries Pass A's own count by trust, not by re-verification. |
| HARDENING-PASS-A.md:316 | Lock 14 at PASS-A.md:428 — 7 sites + Amendment 01 conflict | violated | 5 | STRENGTHEN | Stage-1 catches the Amendment 01 conflict comprehensively (10 sites enumerated); the future-grammar onboarding test absence is real. |
| HARDENING-PASS-A.md:381 | Lane 2 sequencing — W2 sub-sequencing | N/A | 3 | CONFIRM | Stage-1 catches the W2 internal ordering ambiguity (rename-first vs SPLIT-first); concrete surgery |
| HARDENING-PASS-A.md:416 | Lane 3 cohesion — 5 orphan deliverables | partial | 3 | STRENGTHEN | Stage-1 catches LayoutSink BD-carry, cohort-template Pass-B-receiving-wave silence, bbnf-error per-crate gate. |
| HARDENING-PASS-A.md:447 | Lane 4 SOTA anchoring | honoured | 1 | CONFIRM | Pass A is pre-codegen; correct scoping |
| HARDENING-PASS-A.md:546 | Lane 5 grammar-authoritative — 4 verifications | violated | 5 | STRENGTHEN | Stage-1's surgery table at lines 525-536 is verbatim and complete |
| HARDENING-PASS-A.md:603 | Lane 6 generated-code budget — 3 faults | partial | 3 | CONFIRM | Stage-1 catches xtask wall budget silence, per-grammar generated-LOC invariant gap, W6 SPLIT verification gate |
| HARDENING-PASS-A.md:707 | Lane 7 friction forecast — 6 surfaces | violated | 4 | STRENGTHEN | Stage-1 catches all six friction surfaces (pointer! macro doc, lifetime cookbook, layout-lowering errors, Pratt+SIMD misfire, migration page, future-grammar test) |
| HARDENING-PASS-A.md:759 | Lane 8 carry & deferral — 4 fault rows | partial | 4 | STRENGTHEN | Stage-1 surfaces the residue-ledger gate-citation deficit comprehensively |
| HARDENING-PASS-A.md:822 | Lane 9 greenfield discipline — 3 faults | partial | 3 | CONFIRM | Stage-1 catches per-grammar-crate-as-default, Box::leak adjudication, cohort-template route |

### Items Stage-1 did NOT surface (confirmation-drift faults)

| Pass A site | Item | Stage-1 silence reason | Stage-2 surfacing |
|---|---|---|---|
| PASS-A.md:185-194 | the §1.10 bootstrap-shim "Synthesizer adjudicates" pattern, resolved in-pass at line 191 with "name-stability outweighs the 28-LOC cost" | Stage-1 line 724 marks this row "honoured (resolved in-pass)" — but the resolution itself is a no-justification Pass A decision: WHY does name-stability outweigh? The challenge isn't asked. | **fault** — Stage-1 should have surfaced: the 28-LOC shim's name-stability claim assumes downstream consumers reach `bbnf-bootstrap` by name; per Lock 14 + Amendment 01 the workspace shape changes and downstream consumers retire alongside the fracture. Steelman: the shim survives because this is the standard `docs/precepts/no-backward-compat-but-name-stability` pattern; Stage-1 should cite the precept explicitly. |
| PASS-A.md:316-324 | the 8 new facilities (§3) — Stage-1 surfaces only the LayoutSink (line 412) and bbnf-error (line 410) and cohort-template (line 411). Five other new facilities (inverse-layout-audit, bbnf-grammar, validate-metadata, declaration-crate-template, path-executor-relocation) are NOT individually challenged. | Stage-1 ratifies the §3 table by silence | **fault** — under the discipline, every new facility carries Pro/Con/Explication/Challenge. Stage-1 omits per-facility challenges. The challenge for #1 (inverse-layout-audit): does the build-failing audit interrupt regen cycles when grammar authors are mid-edit? The challenge for #6 (declaration-crate template): retracted by Amendment 01 — Stage-1 catches this only in Lane 5 cohort, not in §3 evaluation. |
| PASS-A.md:1-100, the §1 verdict ledger framing | Stage-1's Lane 1 walks each Lock per Stage-1's contract, but Pass A's §1 ledger classifies ~200 files into 5 buckets without per-bucket challenge. The bucketization is itself a Pass A claim Stage-1 ratifies-by-silence: are the bucket criteria (KEEP-OUTRIGHT / KEEP-MODIFY / ABROGATE-{DELETE,MOVE,REPLACE}) consistently applied? | Stage-1 line 802 says "no file survives without justification" — but that's a Pass A claim. Stage-1 doesn't audit per-row consistency. | **partial fault** — under the discipline, every bucket assignment is a verdict that should carry Pro/Con/Challenge. Stage-1 takes the bucketization as ground truth. The full per-file audit is over-scope for Stage-1 (~200 rows × 4 columns = 800 cells); a sampled audit (e.g., 10 randomly-drawn rows) would be sufficient to verify consistency. |

### Pattern observations

**Confirmation-drift is concentrated at low-challenge-strength rows.**
The 3 WEAKEN verdicts all carry challenge strength 1-2/5 (Lock 3 = 1,
Lock 10 = 1, Lock 13 = 2). The 7 STRENGTHEN verdicts carry challenge
strength 3-5/5. The CONFIRM verdicts are evenly distributed. The
correlation is causal: where Stage-1's challenge column is thin,
Stage-2 finds gating gaps; where Stage-1 reasons explicitly, the
ratification lands.

**Stage-1 surfaced the right Locks for fault-finding.** Locks 9, 11,
12, 14 — the four Lane 1 faults Stage-1 surfaced — survive Stage-2
robustly. The five WEAKEN verdicts (Locks 2, 3, 10, 13 + bootstrap
shim) are not hidden faults; they're refinement-of-honoured-verdicts.
Stage-1's fault-finding accuracy is high.

**Per-facility challenge silence is the largest confirmation-drift
gap.** Pass A's §3 (8 new facilities) is ratified-by-table-presence:
Stage-1 cites only LayoutSink (deliverable in BD), bbnf-error
(per-crate gate), cohort-template (Pass B carry). Five new facilities
go uncited individually: inverse-layout-audit, bbnf-grammar,
validate-metadata, declaration-crate-template (retracted by
Amendment 01), path-executor-relocation. Stage-2 amendment item #30
re-surfaces all 8.

**Lane 2A verdict: PARTIAL.** Stage-1's 22 verdicts mostly survive
Stage-2 review (12 CONFIRM, 7 STRENGTHEN, 3 WEAKEN, 0 REVERSE). The
3 WEAKEN verdicts (Lock 3, Lock 10, Lock 13) do not overturn Stage-1's
"honoured" calls — rather, Stage-2 surfaces gating gaps that should
land as additional verification commands at the relevant wave closes.
The 7 STRENGTHEN verdicts confirm Stage-1's strongest catches (Lock 9
Box::leak, Lock 11 mechanism, Lock 14 Amendment 01, Lane 5 verifications,
Lane 7 friction surfaces, Lane 8 deferral gates). Three items Stage-1
did not surface for per-item evaluation: the bootstrap-shim "name-stability"
decision (PASS-A.md:191), individual per-facility challenges for §3's
8 new facilities, and the §1 verdict-ledger bucketization criteria.
Stage-1's Lane 1 surfaced Lock-by-Lock but not item-by-item within
Pass A's structural sections (§1, §3).

---

## §4 — Lane 2B — Discipline Lapse

Stage-2 evaluates whether Stage-1 honoured the Pro / Con / Explication /
Challenge per-item discipline mandated by `restart/prompts/HARDENING.md`
§Per-Item Discipline (lines 38-57; codified at commit `6e1c6e5f` four
minutes before Stage-1 PASS-A committed).

### Lane standard (one paragraph)

The discipline requires: Every claim, gate, wave, decision, surgery,
verdict, and proposal in the target carries an implicit four-part
shape — Explication (what it means), Pros (why it earns its place),
Cons (costs imposed), Challenge (the steelman counter-position). The
verdict for each item is one of KEEP / REINVENT / DISCARD. Per-item
tables in each Lane invoke the rubric. A target where every item lands
KEEP without challenge is a fault — the audit failed to challenge.
Stage-1 PASS-A is required to apply this rubric to every item it
surfaces, with explicit columns, mirrored Pros/Cons, paragraph-shaped
Explications, and steelman Challenges.

### Per-lane table

| Stage-1 lane | Per-item rows | Avg challenge strength (1-5) | Discipline verdict | Stage-2 redress |
|---|---:|---:|---|---|
| Lane 1 — Lock-Adherence (lines 57-338) | 14 (one per lock) + 4 sub-tables totalling ~12 rows | 2.0 | VIOLATED | Tables headed `Site \| Substance \| Verdict` (e.g., line 69) lack Pro / Con / Explication / Challenge columns. Verdicts use `honoured / violated / silent` instead of KEEP / REINVENT / DISCARD. Lock 9 (line 162-180) carries a paragraph-shaped Challenge implicitly (option a vs option b argument), but no explicit column. Lock 14 (line 256-317) is the most disciplined row — explicit table at lines 275-285 enumerates Pass A's per-grammar-crate references and Amendment 01 redress, with implicit pro/con argument. Lock 11 (line 192-212) similarly carries implicit challenge. The other 11 Locks are too thin: Lock 1 (line 61-74) is a 3-row table with Site/Substance/Verdict only. Surgery: rewrite Lane 1 with explicit four-column tables; restate verdicts in KEEP/REINVENT/DISCARD vocabulary. |
| Lane 2 — Sequencing Discipline (lines 342-382) | 9 wave-rows | 2.0 | PARTIAL | Table at line 354-363 enumerates 9 waves with `Wave / Pass A claim / Verifiable?` columns. The W2 fault at lines 365-379 carries an implicit Pro/Con argument (rename-first vs SPLIT-first interleaving); the verdict is "fault — surgery". No KEEP/REINVENT/DISCARD vocabulary. Surgery: tabulate W0..W8 as 9 KEEP rows with one REINVENT (W2 sub-sequencing). |
| Lane 3 — Cohesion (lines 386-416) | 4 orphan-claim rows + 5 orphan-deliverable rows | 2.5 | PARTIAL | Tables at lines 393-398 and 408-414 use `Pass A line / Claim / Evidence trail` and `Deliverable / Pass A wave / Consumer / Verdict` headers. The Verdict column on the second table is the closest Stage-1 gets to KEEP/REINVENT/DISCARD shape; verdicts read "honoured / partial fault / fault" rather than KEEP/REINVENT/DISCARD. Surgery: rename Verdict column entries to KEEP/REINVENT/DISCARD. |
| Lane 4 — SOTA Anchoring (lines 420-447) | 9 gate-rows | 1.5 | PARTIAL | Table at lines 432-442 has `Pass A gate / Substance / Lock 8 claim? / Verdict` — three of four columns are correct. No challenge column. All 9 verdicts are "correct" (Pass A is pre-codegen). The lane verdict — "honoured" — is the same shape as the original lock verdict and lacks KEEP/REINVENT/DISCARD vocabulary. Surgery: minor column rename. |
| Lane 5 — Grammar-Authoritative (lines 451-546) | 4 verification rows + 10-row Amendment 01 surgery table | 4.5 | HONOURED-PARTIAL | The most disciplined lane in Stage-1 PASS-A. The 10-row surgery table at lines 525-536 carries explicit Site / Pass A reads / Re-anchor to surgery columns — the closest to four-column discipline. Lane 5 verdict is "violated" (one of the strongest verdicts in the report). The challenges are implicit but strong: each row contrasts Pass A's per-grammar-crate language with Amendment 01's bbnf-host-prims + workspace-metadata position. Surgery: minimal — restate "violated" as DISCARD-with-replacement. |
| Lane 6 — Generated-Code Budget (lines 550-604) | 9 wave-budget rows + 3 fault prose | 2.0 | VIOLATED | Per-wave LOC table at lines 561-570 has no challenge column. The three faults (xtask wall, per-grammar invariant, W6 verification) are paragraph prose without per-fault tables. Surgery: tabulate the three faults with Site / Item / Pro / Con / Challenge / Verdict columns. |
| Lane 7 — Friction Forecast (lines 608-709) | 6 friction-surface rows × 3 sub-tables | 2.5 | PARTIAL | Each friction surface has a sub-table (lines 620-624, 646-649, 658-663, 675-679, 691-696). The columns are `Friction surface / Pass A coverage / Required artefact` — closer to discipline shape but no Pro/Con/Verdict. Surgery: add Verdict column with KEEP / REINVENT / DISCARD. |
| Lane 8 — Carry & Deferral (lines 713-761) | 8 deferral-rows + 7 residue-ledger rows | 3.5 | PARTIAL | Tables at lines 721-730 and 734-742 carry `Pass A line / Deferral / Receiver / Blocker / Gate / Verdict` columns — the Receiver/Blocker/Gate triple IS the Stage-1 contract for Lane 8. Verdict column entries are "fault / honoured" — needs KEEP/REINVENT/DISCARD restatement. Surgery: minimal — restate verdict vocabulary. |
| Lane 9 — Greenfield Discipline (lines 765-824) | 5 sub-section prose blocks | 1.5 | VIOLATED | Five prose sub-sections (No quick solutions / No workarounds / No legacy code / Idiomatic gestalt / Architectural transpositions) with no per-item tables. Stage-1 contract requires "A lane with no per-item rows is fault" (HARDENING.md:168). Stage-1 here violates its own contract. Surgery: tabulate each sub-section's findings with explicit per-item rows. |

### Cross-lane discipline pattern

Across all 9 Stage-1 lanes, the discipline-shape adherence is uneven:

| Lane | Table column shape | Verdict vocabulary | Challenge depth | Stage-2 grade |
|---|---|---|---|---|
| 1 | Site / Substance / Verdict | honoured / violated / silent | mostly thin (1-2/5) except Locks 9, 11, 14 | C |
| 2 | Wave / Pass A claim / Verifiable? | implicit fault / no formal verdict shape | thin (2/5) | C |
| 3 | Pass A line / Claim / Evidence trail (split into orphan-claim + orphan-deliverable sub-tables) | honoured / partial fault / fault | medium (2.5/5) | B |
| 4 | Pass A gate / Substance / Lock 8 claim? / Verdict | correct / incorrect | thin (1.5/5) | C+ |
| 5 | (verification narrative) + 10-row surgery table with Site / Pass A reads / Re-anchor to | passes / fails | strong (4.5/5) | A- |
| 6 | Wave / Net delta / Reason | (no per-fault tabulation) | thin (2/5) | D |
| 7 | Friction surface / Pass A coverage / Required artefact | (5 sub-tables, one per surface) | medium (2.5/5) | B- |
| 8 | Pass A line / Deferral / Receiver / Blocker / Gate / Verdict | honoured / fault | strong (3.5/5) | A- |
| 9 | (5 prose sub-sections, no table) | honoured / fault | thin (1.5/5) | F |

Lane 5 and Lane 8 are the disciplined outliers; both carry strong
challenge depth and clear verdict columns. Lane 9 is the worst — five
prose sub-sections with no per-item tabulation, in direct violation
of HARDENING.md:168 ("A lane with no per-item rows is fault"). Lanes
1, 2, 4, 6 are between — table shape acceptable but challenge column
absent and verdicts in legacy vocabulary.

**Lane 2B verdict: VIOLATED.** Stage-1 PASS-A's tables systematically
omit the Pro / Con / Explication / Challenge column shape mandated by
HARDENING.md:38-57. The verdict vocabulary uses honoured / violated /
silent / partial / fault inherited from prior Phase-3 hardening
synthesis precedent (`docs/restart/audit/HARDENING-PLAN-SYNTHESIS-*.md`)
rather than the freshly-codified KEEP / REINVENT / DISCARD verdict
trio. Lane 9 violates the "lane with no per-item rows is fault"
contract directly (HARDENING.md:168). Lane 1 (Locks 1-8, 10) carries
extremely thin Challenges (avg 1-2/5). Substantive findings survive
intact; the discipline shape does not.

The redress is mechanical: a Stage-2 amendment agent rewrites the
lane tables with explicit four-column discipline. The substantive
verdicts (which Locks land partial / which Lanes land violated) do
not change; the vocabulary and column shape do. The amendment agent
should additionally lift Lane 9's prose sub-sections into per-item
tables.

---

## §5 — Lane 2C — Steelman

Stage-2 constructs the strongest counter-argument the audit could have
made for every Stage-1 KEEP / honoured verdict. If Stage-1's Challenge
column is weaker than Stage-2's steelman, the KEEP is suspect.

### Per-decision table

| Decision | Stage-1 verdict | Stage-1 challenge | Stage-2 steelman | Survives steelman? | Stage-2 verdict |
|---|---|---|---|---|---|
| Lock 1 ratification (PASS-A.md:415) | honoured | "no live tape code in Pass A scope" + comment-scrub schedule at W0 (HARDENING-PASS-A.md:67-72) | Tape narrative residue is in ~15 sites including doc comments at PASS-A grammar/schema/, types.rs:90, generated/json.rs:1154 (regenerated). The CENSUS at corpora/CENSUS.md §1.2 shows 22 named sites. Stage-1 ratifies "9 narrative residues" without verifying the count vs CENSUS's 22. The substantive question: do the W0 punch-list 4 line 469-471 sites match the CENSUS sites? | SURVIVES (substance) but Stage-1 should cite the count reconciliation; the W0 comment-scrub captures all 15-22 sites uniformly | SURVIVES |
| Lock 2 ratification (W2 surgery scheduled) | honoured (post W2) | "rename pass + LayoutSink trait" (HARDENING-PASS-A.md:84-90) | The Lock 2 retirement renames TypeDesc/StructLayout/TypeMap → Layout. Steelman: are there OTHER retired terms Lock 2 names? Lock 2 line 36 names 8 terms: type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis. Pass A retires TypeDesc, StructLayout, TypeMap (3 of 8). What about "type projection" / "type elaboration" / "schema synthesis" appearing in code? | WEAKENED — Stage-2 finds: rg -nP '(type[_ ]projection|type[_ ]collapsing|type[_ ]elaboration|schema[_ ]synthesis)' across crates/ likely returns hits Stage-1 didn't survey | WEAKENED (Stage-2 punch-list adds: amend Lock 2 verification gate to enumerate all 8 retired terms) |
| Lock 3 ratification (cursor-parse + byte-skip) | honoured | "scope-correct; eager-empty-path elision is Pass B concern" (HARDENING-PASS-A.md:96-100) | After path triplet (W3) relocation, the eager-empty-path case (`__EAGER_EMPTY_PATH`) lives in `path-core/src/runtime/cursor.rs`. Pass A ratifies the relocation but no explicit gate verifies the elision case survives the SPLIT. Steelman: a mechanical SPLIT could lose the case. | WEAKENED — Stage-1 ratifies-by-scope rather than gating-by-path | WEAKENED (Stage-2 punch-list adds: gate at W3 close — `rg -n '__EAGER_EMPTY_PATH' crates/path-core/src/runtime/` returns ≥1) |
| Lock 4 ratification (orthogonal optimisation) | honoured | "Proposal 2 fracture preserves the boundary" (HARDENING-PASS-A.md:104-110) | Lock 4 forbids fusing CSP and e-graph into one solver. Pass A's bbnf-passes/ tree contains both csp_strategy/ and egraph/ as siblings. Steelman: do they remain orthogonal post-relocation, or does some bbnf-passes/ dispatch glue accidentally fuse? | SURVIVES — Pass A's structural decomposition keeps both crates separate (csp-solver + egraph as path-deps; bbnf-passes/ as the consumer) | SURVIVES |
| Lock 7 ratification (path triplet) | honoured | "Proposal 3 maps to Lock 7 footnote shape; W3 punch-list executes" (HARDENING-PASS-A.md:138-144) | Lock 7 allows path-core only as deduplication mechanism; three proc-macro shells are forbidden. Pass A's Proposal 3 names path-core + path + path-ts (1 + 2 = 3 crates, only 2 of which are proc-macro). Compliant. Steelman: does path-core itself contain ANY proc-macro code? If so, fault. | SURVIVES — Pass A names path-core/src/{lex, lower, validate, runtime}/ — no proc-macro entry | SURVIVES |
| Lock 10 ratification (Pratt + SIMD auto-detect) | honoured | "miners carry the detection; no @pratt/@simd directive observed" (HARDENING-PASS-A.md:182-188) | After Proposal 2 fracture, the miners (pratt.rs, operator_chain.rs, pattern_alphabet.rs) relocate from crates/ir/src/passes/recognizers/ to bbnf-passes/src/recognizers/. Steelman: does the relocation preserve the cost-model decision logic? The cost model lives in cost_config.rs; Pass A line 117 marks cost_config.rs as KEEP-OUTRIGHT but to where? | WEAKENED — Stage-1 doesn't gate the cost-model survival across the move | WEAKENED (Stage-2 punch-list adds: gate at W4 close — cost_config.rs lands in bbnf-ir/src/cost_config.rs and is consumed by bbnf-passes/) |
| Lock 13 ratification (no god directories) | honoured | "13 SPLITs + 1 god directory at crates/core/src/ retired by Proposal 1" (HARDENING-PASS-A.md:248-254) | Stage-1 cross-references W2 items 12-23 (12 items) + W6 items 34-37 (4 items) + item 39 (1 item). Total: 17 items (not 13). Pass A's count of 13 doesn't match the punch-list count of 17. Steelman: which is correct? | WEAKENED — Stage-1 ratifies Pass A's count without re-verification | WEAKENED (Stage-2 punch-list adds: reconcile Pass A's "13 SPLITs" claim against punch-list 5+11=16 SPLIT items + 1 verification = 17 items in W2/W6/W7) |
| Lock 5 ratification (one redress: path-string relocation) | honoured | "redress at line 130-185; surgery at item 9 line 502" (HARDENING-PASS-A.md:118-125) | Lock 5 mandates the IR is the boundary; per-backend lowerers consume. Steelman: does Pass A's `crates/ir/src/registry/strategy.rs:130-185` redress fully exit IR scope, or does some Rust-specific code remain elsewhere in bbnf-ir/? | SURVIVES — the redress is the single named site; the rest of bbnf-ir is Rust-agnostic per scope | SURVIVES |
| Lane 5 verification 1 (zero match grammar arms in proposed generic crates) | passes | "two hits: line 169 + line 511, both describe code to RETIRE not propose" (HARDENING-PASS-A.md:466-474) | The grep was on PASS-A.md's text. Steelman: verification should be on Pass A's PROPOSED generic-crate skeletons (e.g., the bbnf-passes/, bbnf-runtime/ shapes Pass A names). But those skeletons are not yet code; they're paragraph descriptions. The grep on PASS-A.md is sufficient. | SURVIVES | SURVIVES |
| Proposal 1 ratification (`crates/core/` fracture) | honoured (implicit) | "post-Lock-2 + post-Lock-14 + post-Lock-7 + post-Lock-11 sequencing" (PASS-A.md:359-362) | Stage-1 line 357-362 walks the dependency: W0 → W1 → W2 → W3 → W4 fracture. Steelman: does Pass A's Proposal 1 box (line 235-242) carry per-grammar-crate residue language? Yes — line 241-242 lists `bbnf-grammar-css-l4/` and "(other per-grammar declaration crates as needed)". Amendment 01 retracts. | DEFEATED in part — Stage-1 catches this in Lane 5 (line 529, surgery 3). The Stage-2 amendment is already in Stage-1's punch list. | SURVIVES (Stage-1 surgery 3 already lands the redress) |
| Bootstrap-shim adjudication (PASS-A.md:185-194) | honoured (resolved in-pass) | "Synthesizer adjudicates; resolved at line 191 — KEEP-MODIFY for name-stability" (HARDENING-PASS-A.md:724) | The 28-LOC shim's "name-stability" justification: WHO are the downstream consumers? After the workspace fractures into 24 members per Amendment 01, do consumers reach `bbnf-bootstrap` by name or by `bbnf::generated::bbnf::BbnfBootstrap`? Steelman: the shim is dead weight if no downstream consumer reaches it by name. Pass A doesn't enumerate consumers. | WEAKENED — Stage-1 takes Pass A's "name-stability" claim by trust; Stage-2 surfaces the consumer-enumeration gap | WEAKENED (Stage-2 punch-list adds: amend bootstrap shim adjudication to enumerate downstream consumers; if zero, retire the shim) |
| Lane 4 SOTA "honoured (Pass A is pre-codegen)" | honoured | "Lock 8 claim with (n/a) citation is correct" (HARDENING-PASS-A.md:444-447) | Pass A's hard-gate table at lines 798-807 names ZERO parse-throughput gates. Steelman: but Pass A introduces ~250 LOC of cohort-template generator + ~150 LOC of validate-metadata. Are those gated? Stage-1 does flag this in Lane 6 (line 581-586). | SURVIVES — Stage-1 catches the budget gap separately | SURVIVES |
| Bucketization criteria (PASS-A.md §1, ~200 file-rows) | implicit honoured | not surfaced individually | Pass A classifies ~200 files into 5 buckets without per-row challenge. Steelman: are the bucket criteria consistently applied? Sample check: PASS-A.md:67 marks `lower/value_expr/atom.rs` KEEP-MODIFY (590 LOC — needs SPLIT); PASS-A.md:69 marks `simple_kinds.rs` KEEP-MODIFY (FAIL-EXPLICIT at L185). Both are KEEP-MODIFY but for different reasons. Is this consistent? Both ARE KEEP-MODIFY (not delete, but require modification). | SURVIVES on sample inspection — bucket criteria consistent | SURVIVES |

**Lane 2C verdict: PARTIAL.** Of the 12 Stage-1 KEEP / honoured
verdicts Stage-2 steelmanned, 7 SURVIVE, 5 are WEAKENED, 1 is partly
DEFEATED but with already-pending Stage-1 surgery (the Proposal 1
per-grammar-crate residue Stage-1 catches in Lane 5 surgery 3). The
WEAKENED items are: Lock 2 (5 of 8 retired terms not surveyed), Lock 3
(eager-empty-path elision survival not gated), Lock 10 (cost-model
relocation not gated), Lock 13 (SPLIT count mismatch 13 vs 17),
bootstrap shim adjudication (consumer enumeration absent). These are
not show-stoppers; Stage-2's punch list adds amendment items #29-#34
that gate the WEAKENED items at their respective wave closes. No
Stage-1 KEEP is overturned to REINVENT or DISCARD by the steelman.

---

## §6 — Lane 2D — Verdict Imbalance

Stage-2 evaluates Stage-1's cohort verdict balance. Per HARDENING-STAGE-2-EXTERNAL.md
§Lane 2D, the diagnostic is: KEEP / REINVENT / DISCARD distribution
across all lanes; pattern across target sections; over-ratifying
threshold (>85% KEEP) suggests Stage-1 failed to challenge.

The complication for Stage-1 PASS-A: Stage-1 did not use KEEP /
REINVENT / DISCARD vocabulary (per Lane 2B). Instead Stage-1 used
honoured / partial / violated / silent. To execute Lane 2D Stage-2
must MAP Stage-1's verbiage onto KEEP/REINVENT/DISCARD.

The mapping Stage-2 applies:
- Stage-1 "honoured" → KEEP (item is sound; carries to V2 unchanged)
- Stage-1 "honoured-with-X" → KEEP (item is sound; X is an
  implementation note, not a structural change)
- Stage-1 "honoured (surgery scheduled)" → KEEP (the substantive
  verdict is honoured; the surgery is the planned execution)
- Stage-1 "partial" → REINVENT-cohort (some sub-items KEEP, some need
  redesign)
- Stage-1 "violated" → REINVENT in most cases (the Lane 5 / Lane 7
  violations are surgical-redress, not retire-the-item)
- Stage-1 "silent" → REINVENT (must-add)
- Stage-1 "N/A" → out-of-cohort

### Cohort distribution table

| Lane | KEEP | REINVENT | DISCARD | Out-of-cohort | KEEP fraction | Stage-2 verdict |
|---|---:|---:|---:|---:|---:|---|
| Lane 1 (Lock-Adherence) | 10 (Locks 1-8, 10, 13) | 4 (Locks 9, 11, 12, 14) | 0 | 0 | 71% | BALANCED |
| Lane 2 (Sequencing) | 8 (W0-W1, W3-W8) | 1 (W2 sub-sequencing) | 0 | 0 | 89% | OVER-RATIFYING (Stage-1 only flagged W2; the 8 KEEP wave-rows are by-trust) |
| Lane 3 (Cohesion) | 4 (Pass A claims; LayoutSink consumer; ledger items) | 5 (orphan deliverables) | 0 | 0 | 44% | BALANCED |
| Lane 4 (SOTA) | 9 (engineering gates) | 0 | 0 | 0 | 100% | OVER-RATIFYING (correctly out-of-scope; Pass A is pre-codegen) |
| Lane 5 (Grammar-Authoritative) | 2 (verifications 1, 2) | 8 (verifications 3, 4 + Amendment 01 sites) | 0 | 0 | 20% | UNDER-RATIFYING (the strongest lane; Stage-1 catches systemic Amendment-01-conflict) |
| Lane 6 (Generated-Code Budget) | 6 (per-wave deltas accepted) | 3 (xtask wall, per-grammar invariant, W6 verification) | 0 | 0 | 67% | BALANCED |
| Lane 7 (Friction Forecast) | 0 | 6 (all 6 friction surfaces uncovered) | 0 | 0 | 0% | UNDER-RATIFYING (every friction surface fails; Stage-1 catches systemic friction-blindness) |
| Lane 8 (Carry & Deferral) | 4 (resolved deferrals) | 4 (open deferrals + missing gates) | 0 | 0 | 50% | BALANCED |
| Lane 9 (Greenfield Discipline) | 4 (no quick solutions, no workarounds, no legacy uncontested, idiomatic) | 3 (Amendment 01 default, Box::leak, cohort-template route) | 0 | 0 | 57% | BALANCED |
| **Aggregate** | 47 | 34 | 0 | 0 | 58% | BALANCED |

### Pattern observations

**No DISCARD verdicts.** Across 81 mapped items, Stage-1 issues zero
DISCARD verdicts. Per HARDENING.md:55 "A target with mixed verdicts
(mostly KEEP, some REINVENT, occasional DISCARD) is the healthy
shape." Zero DISCARD is suspicious. Stage-2 candidate DISCARD items:

1. **Pass A new facility 6 (declaration-crate template)** at PASS-A.md:322 —
   retracted by Amendment 01; this is a textbook DISCARD with
   replacement (`bbnf-host-prims`). Stage-1's Lane 5 surgery #4 (line
   840) "retract new-facility 6 (per-grammar declaration crate
   template); replace with..." is functionally a DISCARD verdict but
   nowhere does Stage-1 say "DISCARD"; it says "retract".
2. **`crates/bbnf-grammar-css-l4/`** at PASS-A.md:241, 478, 722, 759 —
   Amendment 01 retracts wholesale. Stage-1 surgery #1, #3, #7, #9
   (Lane 5) re-anchors to bbnf-host-prims. This is DISCARD-with-replacement.

The substance — that these items retire — is captured in Stage-1's
surgery items. The verdict-vocabulary alone is missing DISCARD.

**Lane 4 100% KEEP**. Stage-1 ratifies all 9 SOTA gates as KEEP. This
is correct (Pass A is pre-codegen; no parse-throughput gates exist in
Pass A scope), but it's a 100% KEEP that should carry an explicit
"out-of-scope, n/a" annotation rather than counting toward the cohort
distribution. Stage-1 line 444 does say "(n/a)" — the substance is
right; the cohort-balance signal is misleading.

**Lane 7 0% KEEP**. All 6 friction surfaces are REINVENT (uncovered
artefacts). This UNDER-RATIFYING signal correctly catches a systemic
gap in Pass A — friction-cookbook coverage was never planned. The
Lane 7 fault concentration is healthy.

**Distribution across target sections**: Stage-1's faults concentrate
in Pass A's §3 (8 new facilities), §4.1 (Lock 14 retirement), §5
(7 cross-pass residues), §7 (W7 punch-list). The §1 verdict ledger
(~200 file-rows) and §2 (Proposal ratifications) are largely KEEP —
which raises the §1-bucketization-by-trust concern surfaced in Lane 2A.

### Distribution-shape comparison to expected greenfield audit

A Stage-1 audit applying KEEP / REINVENT / DISCARD discipline against
a Pass synthesis under amendment-required pressure should land
approximately:
- 55-70% KEEP — most architectural transpositions and lock verdicts
  survive
- 25-40% REINVENT — surgical redress under amendment, friction
  surfaces uncovered, deferral-gate gaps
- 5-10% DISCARD — items the amendment retracts (per-grammar crates)
  or items that contradict precepts (e.g., elevating optional escape
  hatches to defaults)

Stage-1 PASS-A's mapped distribution (58% KEEP / 42% REINVENT / 0%
DISCARD) is closer to the expected band than not, but the missing
5-10% DISCARD is a verbiage gap rather than a substance gap. The
DISCARDable items are: Pass A's new facility 6 (declaration-crate
template — Amendment 01 retracts wholesale), and the per-grammar
declaration crate "as a default" framing at PASS-A.md:241, 322, 478,
722. Stage-1 catches all of these in Lane 5 surgery items #4, #5, #7,
#9 — but the verdicts are phrased as "retract / replace" rather than
"DISCARD with replacement". The substance is right; the vocabulary is
not.

**Lane 2D verdict: BALANCED.** The aggregate KEEP fraction is 58%
across 81 mapped items, which falls cleanly in the "60-80% KEEP healthy"
band per HARDENING-STAGE-2-EXTERNAL.md:113. Three lanes (Lane 2 89%,
Lane 4 100%) are over-ratifying-on-the-surface but explained by scope.
Two lanes (Lane 5 20%, Lane 7 0%) are under-ratifying — those are the
strongest lanes catching real systemic issues. The DISCARD column is
zero across all lanes — Stage-1's verbiage replaces DISCARD with
"retract / discard / retire" implicit in surgery items but never as
explicit verdict. Stage-2 amendment: restate Stage-1's "retract"
surgery rows as DISCARD-with-replacement verdicts to honour the
discipline vocabulary. This is the same redress as Lane 2B amendment
item #28.

---

## §7 — Lane 2E — Recommendation Quality

Stage-2 evaluates every Stage-1 punch-list entry for concreteness,
applicability, and scope-correctness.

### Per-surgery table

| Stage-1 punch-list # | Surgery summary | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---|---|---:|---:|---|---|
| 1 (Lane 5) | re-anchor PASS-A.md:33 successor | 5 | 5 | single-line | none — surgery is verbatim |
| 2 (Lane 5) | re-anchor PASS-A.md:85 | 5 | 5 | single-line | none |
| 3 (Lane 5) | strike PASS-A.md:241 row; replace | 5 | 5 | paragraph | none |
| 4 (Lane 5) | retract new-facility 6 at PASS-A.md:322 | 5 | 5 | row replacement | none |
| 5 (Lane 5) | re-anchor PASS-A.md:336 | 5 | 5 | single-line | none |
| 6 (Lane 5) | re-anchor PASS-A.md:342 | 5 | 5 | single-line | none |
| 7 (Lane 5) | retract punch-list 5 at PASS-A.md:478-481 | 5 | 5 | paragraph | none |
| 8 (Lane 5) | re-anchor PASS-A.md:519 | 5 | 5 | single-line | none |
| 9 (Lane 5) | retract PASS-A.md:722 row + add 2 rows | 5 | 5 | row replacement | none |
| 10 (Lane 5) | re-anchor PASS-A.md:759 file-migration | 5 | 5 | single-line | none |
| 11 (Lane B) | add §6.5 future-grammar onboarding test | 5 | 5 | new section | none — verbatim ceremony given |
| 12 (Lane C) | resolve Box::leak adjudication | 4 | 5 | multi-line | minor — surgery picks option (b) but does not specify the receiving-gate verification command. Stage-2 amendment: add "gate at BA W8 close: `rg -n 'parse_grammar_in' crates/bbnf-parse/src/grammar/` returns ≥1 hit" |
| 13 (Lane C) | tighten Lock 11 mechanism (Pass A:287) | 4 | 5 | paragraph | minor — surgery is the cargo metadata gate, but doesn't say WHERE in the master plan the gate appears |
| 14 (Lane C) | add receiving-gate column to residue ledger | 2 | 4 | multi-row | **fault** — Stage-1 says "add receiving-gate column" + provides one example for §5.1. The other 6 residues (§5.2-§5.7) are not given verbatim gate-citations. Stage-2 amendment: provide verbatim gates for all 7 residues, not just §5.1. |
| 15 (Lane C) | add §5.8 Lock 12 archive | 5 | 5 | new row | none — verbatim row given |
| 16 (Lane E) | re-attribute LOC delta at PASS-A.md:685 | 5 | 5 | single-line | none — verbatim text given |
| 17 (Lane D) | add §5.9 pointer-macro cookbook | 3 | 4 | new row | partial — surgery gives the gate citation ("BA W3 close ⇒ Pass C cookbook receiving wave; verbatim error gate: `error: rule '<X>' not found in grammar <G>`...") but the verbatim error is incomplete (`<X>` and `<G>` are placeholders). Stage-2 amendment: provide one fully-instantiated example. |
| 18 (Lane D) | add §5.10 lifetime-surfaces cookbook | 3 | 4 | new row | partial — same fault as #17; verbatim error gate is named but not exemplified |
| 19 (Lane D) | append to W7 item 40 — layout-lowering errors doc | 5 | 5 | single-line | none |
| 20 (Lane D) | add §5.11 Pratt/SIMD misfire | 2 | 4 | new row | **fault** — surgery says "Pass B receiving wave; gate cites docs/optimizer/pratt-simd-detection.md" but no verbatim error / cookbook page given. The misfire scenario itself is not described. Stage-2 amendment: include a representative misfire scenario (e.g., "an `expr → op expr op expr` rule classified as Pratt when grammar author intended structural"). |
| 21 (Lane D) | append migration page at W4 | 5 | 5 | single-line | none |
| 22 (Lane E) | cite agent-5 schema example + bbnf-error per-crate gate | 4 | 4 | multi-line | minor — three sub-items merged into one row; concreteness for the schema-citation sub-item is 4 (says "cite agent-5 schema example" but doesn't quote it) |
| 23 (Lane E) | add 3 hard-gate rows | 5 | 5 | new rows | none — verbatim gates given |
| 24 (Lane E) | add W2 sub-sequencing | 5 | 5 | paragraph | none — verbatim sub-sequencing given |
| 25 (Lane E) | qualify LayoutSink claim at PASS-A.md:324 | 5 | 5 | single-line | none — verbatim text given |

### Cohort statistics

| Concreteness band | Count | % |
|---|---:|---:|
| 5 (verbatim text or command) | 17 | 68% |
| 4 (specific edit + minor gap) | 5 | 20% |
| 3 (named edit + missing exemplification) | 2 | 8% |
| 2 (vague at residue-ledger boundary) | 1 | 4% |
| 1 | 0 | 0% |

**Aggregate concreteness: 4.52/5.** This is high. For comparison, the
Stage-2 standard would expect a healthy hardening report's punch list
to land at 4.0+/5.0 — the surgeries are executable as-is by the
downstream amendment agent. The Stage-1 PASS-A punch list comfortably
exceeds the standard.

| Applicability band | Count | % |
|---|---:|---:|
| 5 (downstream agent can execute as-is) | 19 | 76% |
| 4 (executable but requires minor inference) | 6 | 24% |

| Scope-correctness | Count |
|---|---:|
| single-line | 9 |
| paragraph | 3 |
| new section | 1 |
| new row | 4 |
| row replacement | 2 |
| multi-line | 2 |
| multi-row | 1 |
| new rows | 1 |
| multi-section | 0 |
| re-draft | 0 |
| Total | 25 |

### Pattern observations

- **17 of 25 surgeries (68%) are concreteness-5** — verbatim text or
  exact command. This is high quality; the Stage-2 amendment agent
  can apply these mechanically.
- **3 surgeries (#14, #17, #18, #20) leave residue-ledger additions
  too vague**. The pattern: Stage-1 says "add §5.X with a gate" but
  doesn't fully instantiate the gate's verbatim error / cookbook
  exemplar. Stage-2 amendment: re-spec these 3 with verbatim
  exemplars.
- **No re-draft scopes** — all surgeries are surgical at the
  paragraph / single-line / new-row level. No structural re-write of
  Pass A is required. This is consistent with Stage-1's "amendment-required"
  decision.
- **Stage-1's punch-list grouping (A: Amendment-01, B: future-grammar,
  C: carry-fixes, D: friction, E: cohesion+budget) is well-scoped**.
  Each group has a coherent receiving agent (Amendment 01 reconciliation
  agent, friction-doc agent, etc.). The grouping aids parallelisation.

### Owner-routing assessment

Stage-1 PASS-A routes all 25 surgeries to "amendment agent" without
distinguishing among the parallel agents that could execute them.
Stage-2 observes:
- Group A (10 Amendment 01 reconciliations) — sequential single-file
  edits; one agent suffices
- Group B (1 future-grammar onboarding test) — single new section;
  same agent
- Group C (5 carry-fixes) — multi-line + multi-row; cross-references
  Pass A residues + Lock 9 + Lock 11 + Lock 12; benefits from the
  same agent for coherence
- Group D (5 friction surfaces) — 5 residue-ledger additions; could
  parallelise but the cookbook artefacts they cite (path-macro.md,
  lifetime-surfaces.md, layout-lowering.md, pratt-simd-detection.md,
  bc-core-split.md) are downstream Pass C scope, not Stage-2 amendment
  scope
- Group E (4 cohesion + budget gaps) — multi-line single-file edits;
  same agent

The owner column is acceptable; sub-routing among parallel agents is
unnecessary at the Stage-1 punch-list granularity.

**Lane 2E verdict: HONOURED-PARTIAL.** Stage-1 PASS-A's punch list is
22/25 concrete and applicable. Three residue-ledger additions (#14
coverage, #17 verbatim, #18 verbatim) and one Pratt/SIMD misfire
specification (#20) need verbatim instantiation. The group structure
(A-E) parallelises cleanly. No surgery requires re-draft scope.

---

## §8 — Stage-2 Punch List

Ordered amendments to Stage-1 PASS-A's verdicts and recommendations.
Each entry: target Stage-1 site / Stage-1 verdict / Stage-2 amended
verdict / reason / owner.

### A — Discipline-shape amendments (Lane 2B)

| # | Target Stage-1 site | Stage-1 verdict | Stage-2 amended verdict | Reason | Owner |
|---|---|---|---|---|---|
| 26 | HARDENING-PASS-A.md table headers throughout | various Site/Substance/Verdict shapes | rewrite tables with Site \| Item \| Explication \| Pros \| Cons \| Challenge \| Verdict (KEEP/REINVENT/DISCARD) columns | Lane 2B contract violation; HARDENING.md:38-57 mandates four-column discipline | Stage-2 amendment agent |
| 27 | HARDENING-PASS-A.md:765-824 Lane 9 | 5 prose sub-sections, no per-item table | tabulate Lane 9 findings with explicit per-item rows under four-column discipline | Lane 2B + HARDENING.md:168 contract violation ("A lane with no per-item rows is fault") | Stage-2 amendment agent |
| 28 | HARDENING-PASS-A.md verdict vocabulary throughout | "honoured / partial / violated / silent" | restate as KEEP / REINVENT / DISCARD per HARDENING.md:47-53 | Lane 2B contract violation | Stage-2 amendment agent |

### B — Confirmation-drift amendments (Lane 2A)

| # | Target Stage-1 site | Stage-1 verdict | Stage-2 amended verdict | Reason | Owner |
|---|---|---|---|---|---|
| 29 | HARDENING-PASS-A.md:724 (bootstrap shim adjudication) | "honoured (resolved in-pass)" | REINVENT — amend Pass A:191 to enumerate downstream consumers; if zero consumers reach `bbnf-bootstrap` by name, retire the shim | Lane 2A — Stage-1 took Pass A's "name-stability" claim by trust; Stage-2 surfaces the consumer-enumeration gap | Stage-2 amendment agent |
| 30 | HARDENING-PASS-A.md (silent on PASS-A.md:316-324) | not surfaced | KEEP-with-individual-challenges — add per-facility evaluation for the 8 new facilities (#1 inverse-layout-audit; #2 bbnf-grammar; #3 validate-metadata; #4 bbnf-error; #5 cohort-template; #6 declaration-crate-template; #7 path-executor-relocation; #8 LayoutSink) | Lane 2A — Stage-1 surfaced only 3 of 8 new facilities individually | Stage-2 amendment agent |

### C — Steelman amendments (Lane 2C)

| # | Target Stage-1 site | Stage-1 verdict | Stage-2 amended verdict | Reason | Owner |
|---|---|---|---|---|---|
| 31 | HARDENING-PASS-A.md:90 (Lock 2 ratification) | honoured (surgery scheduled) | KEEP-with-extended-verification — amend Lock 2 verification gate to enumerate all 8 retired terms (type projection / type collapsing / type inference / type elaboration / TypeMap / StructLayout / TypeDesc / schema synthesis), not just the 3 Pass A scheduled | Lane 2C — Stage-1's challenge missed 5 of 8 retired terms named in 14-LOCKS.md:36 | Stage-2 amendment agent |
| 32 | HARDENING-PASS-A.md:100 (Lock 3 ratification) | honoured | KEEP-with-gate — add gate at W3 close: `rg -n '__EAGER_EMPTY_PATH' crates/path-core/src/runtime/` returns ≥1 hit | Lane 2C — Stage-1 ratifies-by-scope without gating eager-elision survival | Stage-2 amendment agent |
| 33 | HARDENING-PASS-A.md:188 (Lock 10 ratification) | honoured | KEEP-with-gate — add gate at W4 close: `cost_config.rs` lands at `bbnf-ir/src/cost_config.rs` and is consumed by `bbnf-passes/` | Lane 2C — Stage-1 ratifies miner-relocation but doesn't gate the cost-model relocation | Stage-2 amendment agent |
| 34 | HARDENING-PASS-A.md:254 (Lock 13 ratification) | honoured | KEEP-with-reconciliation — Pass A claims "13 SPLITs"; Stage-1 cross-references count to 17 across W2 + W6 + W7. Reconcile: amend Pass A:427 verdict cell to either "13 SPLITs" or "17 SPLITs" matching the punch-list count | Lane 2C — Stage-1 carries Pass A's count by trust; Stage-2 surfaces 13-vs-17 mismatch | Stage-2 amendment agent |

### D — Recommendation-quality amendments (Lane 2E)

| # | Target Stage-1 site | Stage-1 verdict | Stage-2 amended verdict | Reason | Owner |
|---|---|---|---|---|---|
| 35 | HARDENING-PASS-A.md:858 (punch-list 14 — receiving-gate column) | partially concrete | re-spec — provide verbatim gate citations for all 7 residues (§5.1-§5.7), not only §5.1 example | Lane 2E — concreteness 2/5; Stage-1 shows one row, leaves 6 unspecified | Stage-2 amendment agent |
| 36 | HARDENING-PASS-A.md:868 (punch-list 17 — pointer-macro cookbook §5.9) | partially concrete | re-spec — provide one fully-instantiated verbatim error (e.g., `error: rule 'json' not found in grammar Json; suggestions: 'json_value', 'json_key'`) | Lane 2E — concreteness 3/5; placeholders `<X>`, `<G>` not exemplified | Stage-2 amendment agent |
| 37 | HARDENING-PASS-A.md:869 (punch-list 18 — lifetime-surfaces §5.10) | partially concrete | re-spec — provide one fully-instantiated verbatim error (e.g., `error: parse_in requires &Bump; allocate one with: let bump = bumpalo::Bump::new(); parser.parse_in(input, &bump)`) | Lane 2E — concreteness 3/5; Stage-1 names the verbatim-error gate but doesn't exemplify | Stage-2 amendment agent |
| 38 | HARDENING-PASS-A.md:871 (punch-list 20 — Pratt/SIMD misfire §5.11) | partially concrete | re-spec — describe a representative misfire scenario (e.g., "rule `expr → op expr op expr` classified as Pratt when grammar author intended structural") and the cookbook entry's required diagnostic shape | Lane 2E — concreteness 2/5; Stage-1 names the surface but doesn't describe the misfire scenario | Stage-2 amendment agent |

---

## §9 — Final readiness

> **Stage-2 Decision: STAGE-1 AMENDMENTS REQUIRED.**
>
> Stage-1 PASS-A's substantive findings hold against the Stage-2 lanes.
> The 25-item Stage-1 punch list is 68% verbatim-concrete and parallelises
> cleanly across five groups (A: Amendment-01 reconciliation; B:
> future-grammar onboarding test; C: carry-fixes; D: friction surfaces;
> E: cohesion + budget). The Lane 1 / Lock 14 + Lane 5 + Lane 7 + Lane 8
> faults Stage-1 surfaces are real and survive Stage-2 scrutiny.
>
> The Stage-2 amendments are mechanical: Lane 2B (discipline lapse)
> requires restating Stage-1's verdict vocabulary in KEEP / REINVENT /
> DISCARD and rewriting Lane tables under the four-column Pro / Con /
> Explication / Challenge discipline mandated by HARDENING.md:38-57
> (codified four minutes before Stage-1 PASS-A committed; the auditor
> failed to apply it). Lane 2A surfaces three confirmation-drift items
> Stage-1 did not surface (bootstrap shim consumer-enumeration; per-facility
> challenges for §3 new facilities; §1 bucketization criteria sample
> verification). Lane 2C surfaces four steelman gaps (Lock 2 retired-term
> coverage; Lock 3 eager-empty-path elision gating; Lock 10 cost-model
> relocation gating; Lock 13 SPLIT-count reconciliation). Lane 2E
> surfaces four recommendation-quality gaps (residue-ledger gate
> coverage; verbatim error exemplification × 2; Pratt/SIMD misfire
> scenario specification). Lane 2D returns BALANCED at 58% KEEP across
> 81 mapped items.
>
> Stage-1 PASS-A does not require re-audit. The 13 Stage-2 amendment
> items (#26-#38) fold into Stage-1's existing 25-item punch list.
> Total reconciled punch list: 38 items, applied by a single
> reconciliation agent.
>
> Hereupon the Stage-2 decision passes to the master-plan V2 re-issue
> agent, which composes Stage-1 + Stage-2 punch lists alongside Pass A,
> Pass B, Pass C, MASTER-PLAN, and Amendment 01. The reconciled Pass A
> substrate (Stage-1 + Stage-2) becomes one of three Pass inputs to the
> master-plan V2 re-issue.
