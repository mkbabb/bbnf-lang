# Hardening Stage 2 — PASS-C (Fresh Adversary Against Stage-1 Audit-Quality)

Date: 2026-05-03. Stage-2 hardening agent: second-order adversary against `restart/audit/hardening/HARDENING-PASS-C.md` (commit `72c906cb`). Underlying target: `restart/audit/passes/PASS-C.md` (486 lines) + 6 per-agent reports (1,970 lines aggregate). Stage-2 audits Stage-1's *audit of Pass C*, not Pass C itself.

The Stage-1 PASS-C audit returned amendment-required, ratifying three consequentials (Lock 12 ceremony, commit-chain Option 3, six-wave docs re-do) and surfacing 30 surgical edits across 9 lanes. Stage-2 hereupon evaluates whether Stage-1's discipline held under cap pressure, whether its KEEP verdicts survive steelman counter-arguments, and whether its punch list is concrete and applicable. The audit applies the five Stage-2 lanes (2A through 2E) per `restart/prompts/HARDENING-STAGE-2-EXTERNAL.md`; it cites Stage-1's faults by id (1.A through 9.B), the underlying Pass-C synthesis by line, and the per-agent ledger where the trail demands it.

---

## §1 — Target identification

| Item | Value |
|---|---|
| Stage-1 report | `restart/audit/hardening/HARDENING-PASS-C.md` (782 lines; commit `72c906cb`) |
| Underlying target | `restart/audit/passes/PASS-C.md` (486 lines) + 6 per-agent reports (1,970 lines) |
| Total Stage-1 lines audited | 782 |
| Total underlying corpus available to Stage 1 | 2,456 |
| Anchor corpora | `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md` (161 lines); `restart/corpora/CENSUS.md`; `restart/corpora/MODULES.md`; `restart/locks/14-LOCKS.md` |
| Working-tree HEAD at audit start | `fd0c1179` |
| Time budget | 45 minutes; minute 41 commit; minute 45 halt |

The Stage-1 audit applied 9 lanes (Lock-Adherence, Sequencing, Cohesion, SOTA, Grammar-Authoritative, Generated-Code, Friction, Carry, Greenfield) producing 30 surgical edits. Two lanes were declared n/a (Sequencing, SOTA). Stage 2 evaluates the remaining 7 substantive lanes through the five Stage-2 lenses.

---

## §2 — Cohort verdict

| Lane | Stage-2 verdict | Notes |
|---|---|---|
| 2A Confirmation-Drift | partial-drift | Stage 1 inherited Pass-C's framing on Option 3 (commit chain) and on the docs-six-wave shape; Lane 2A surfaces both as steelman gaps. Stage 1's Lane 2 (Sequencing) and Lane 4 (SOTA) n/a-declarations are confirmation-drift candidates. |
| 2B Discipline Lapse | partial | Lane 9 (Greenfield) and Lane 4 (SOTA) carry one-line dispositions that should have been per-item rows; Lane 1 (Lock-Adherence) and Lane 5 (Grammar-Authoritative) are paragraph-shaped (good); Lane 7 (Friction) discipline strong. |
| 2C Steelman | mostly-survives | Eight of nine Stage-1 architectural KEEPs survive the steelman. The bbnf-language-server consolidation survives (correctly amended). The commit-chain Option 3 ratification SURVIVES Stage-2's steelmen against Options 1, 2, 4 — but Stage-1's challenge column was thin and Stage-2 strengthens. The dual-disposition retraction is correct. |
| 2D Verdict-Imbalance | balanced | Stage-1 PASS-C's "honoured-mostly" leaning is consistent with Pass-C's scope (mostly non-API, non-codegen). The 5 partial / 1 violated / 2 honoured-mostly / 2 n/a spread is healthy for periphery scope; not over-ratifying. |
| 2E Recommendation-Quality | mostly-applicable | 28 of 30 surgeries are concrete (path:line + verbatim text); 2 are scope-suspect (item 1 multi-section reconciliation; item 16 LOC ratios without baselines). |

**Final Stage-2 decision: STAGE-1 AMENDMENTS REQUIRED.**

Stage 1 PASS-C is sound in its core diagnostic — the bbnf-language-server consolidation reconciliation against Amendment 01, the Lock 12 archive blocking-precondition ratification, the commit-chain Option 3 endorsement, the docs re-do six-wave LOC-budget addenda are all the right surgeries. What requires amendment: (i) Lane 9 and Lane 4 should have been per-item-tabled, not paragraph-disposed; (ii) the commit-chain Option 3 challenge should explicitly defeat each of Options 1, 2, 4 with steelman counter-positions; (iii) the dual-disposition retraction (Fault 1.F) should be promoted from punch-list-item-1 to standalone Stage-1 verdict; (iv) Stage-1's commit-count-drift surgery (Fault 3.B) is necessary but insufficient — Stage 2 surfaces a deeper cohesion concern. The amendments are surgical; total scope ~10 line-edits to the Stage-1 PASS-C report. No re-audit required.

---

## §3 — Lane 2A: Confirmation-Drift Audit

**Lane standard.** For every Stage-1 verdict, evaluate whether Stage 1 carried Pass-C's framing implicitly. Particular foci: (i) Stage 1 ratifying Pass-C surgeries because Pass-C's surrounding paragraphs framed them favourably; (ii) Stage 1's Pro/Con/Explication/Challenge column carrying strong Pros (paraphrased from the target) and weak Cons / Challenges (hand-waved); (iii) target items Stage 1 did not surface for per-item evaluation.

### §3.1 — Per-item drift evaluation

| Stage-1 site (path:line) | Target item | Stage-1 verdict | Stage-1 challenge strength (1-5) | Stage-2 verdict | Reason |
|---|---|---|---:|---|---|
| HARDENING-PASS-C.md:55-61 (Lock 1 ratification) | PASS-C.md:69 sweep directive | honoured | 4 | CONFIRM | Lock 1 sweep directive is mechanical; no surrounding favourable framing to ratify implicitly. Honest verdict. |
| HARDENING-PASS-C.md:63-69 (Lock 2) | PASS-C.md:70 sweep | honoured | 4 | CONFIRM | Same shape as Lock 1. |
| HARDENING-PASS-C.md:71-83 (Lock 3) | PASS-C.md:71 confirm-step | silent-must-add → honoured-with-surgery | 3 | STRENGTHEN | Stage 1 catches Fault 1.A (verification-step-not-deliverable). Stage-2 strengthens: Pass-C's "confirm" verb is the broader anti-pattern; ALL of Pass-C's silent-must-add Locks (3, 5, 9, 10) carry the same "confirm" language. Stage 1 caught Locks 3 and 10 as surgery-needing but did not flag Locks 5 and 9 as having the same anti-pattern. See punch-list item 6 in Stage 1 (single Lock 3 surgery) — should have been generalised. |
| HARDENING-PASS-C.md:107-115 (Lock 8 + Fault 1.B) | PASS-C.md:33 KEEP-MODIFY-but-full-rewrite | honoured (with bucket-label fix) | 4 | CONFIRM-STRENGTHEN | Stage 1 catches the bucket-collision (KEEP-MODIFY language for full-rewrite content). Stage-2 strengthens: this is a *systemic* labelling pattern in Pass-C — see PASS-C.md:46 (`README.md` KEEP-MODIFY full rewrite per Agent 4 §6.2), PASS-C.md:41 (`docs/GESTALT.md` KEEP-MODIFY rewrite). Three sites carry the same fault; Stage 1 surfaces one. |
| HARDENING-PASS-C.md:117-131 (Lock 10 + Fault 1.C) | PASS-C.md:78 silent-must-add | promoted to blocking gate | 5 | CONFIRM | Strong Stage-1 surgery. Steelman: the audit could have argued that Pass-C's "silent-must-add" was correct because the audit step is verification not deliverable; Stage 1 correctly rejects this — verification gates ARE deliverables when they block consolidation. |
| HARDENING-PASS-C.md:133-137 (Lock 11) | PASS-C.md:79 honoured-mostly | honoured-with-surgery | 3 | WEAKEN | Stage 1's challenge is thin. Pass-C says "confirm parse-that disposition" — Stage 1 ratifies, glosses over. The steelman: parse-that's "external until TS backend" is itself a defer-to-future-tranche claim that Lane 8 should have caught. Stage 1 catches it via Fault 8.C eventually but Lane 1 ratifies before Lane 8 surfaces. The cross-lane consistency suffers. |
| HARDENING-PASS-C.md:139-153 (Lock 12 + Fault 1.D) | PASS-C.md:80 violated-with-blocking-rec | violated → ratified | 5 | CONFIRM | Stage 1 verifies the diagnosis with `ls archive/` (returns empty) and `Cargo.toml` line 2 (still lists ser+gorgeous) — concrete, not ratifying-by-inheritance. Strong. |
| HARDENING-PASS-C.md:155-165 (Lock 13 + Fault 1.E) | PASS-C.md:81 / §3.3 docs target shape | honoured-with-surgery | 4 | CONFIRM | Stage 1 catches the Lock-13 floor (`docs/process/` at 4 children, near floor; growth-by-attrition risk). Specific, not framed-paragraph-trust. |
| HARDENING-PASS-C.md:167-187 (Lock 14 + Fault 1.F) | PASS-C.md:82, 23-24, 92 dual-disposition | violated-with-rec | 5 | CONFIRM | Stage 1's strongest surgery. Catches the Amendment-01-collision precisely. Per-agent 3, 4 retractions extend the surgery downstream. Steelman: see §5 Lane 2C; the dual-disposition was a deliberate punt by Pass C, not framing-bias by Stage 1. Stage 1 correctly amends. |
| HARDENING-PASS-C.md:212-216 (Lane 2 n/a) | — | n/a | — | CONFIRM-WEAK | Lane 2 (Sequencing) is correctly N/A for a single-pass synthesis per HARDENING.md:69. But Stage-2 surfaces: Pass-C does enumerate 6 docs-re-do waves AND 8 prelude commits (PASS-C.md:328-336). Stage-1's silence on the prelude-commit ordering is a confirmation-drift fault — Pass-C's prelude commit order is not free of sequencing risk. See punch-list amendment in §8. |
| HARDENING-PASS-C.md:312-322 (Lane 4 SOTA n/a) | PASS-C.md scope | n/a | — | WEAKEN | Stage 1 dispatches Lane 4 in 12 lines, none per-item-tabled. The disposition is correct (Pass-C contains zero perf gates) but the discipline-violation is Stage 2's concern (see Lane 2B). Stage-2 surfaces no missed perf gate but flags discipline gap. |
| HARDENING-PASS-C.md:262-272 (Fault 3.B commit drift) | PASS-C.md:262-267 | partial | 3 | WEAKEN | Stage 1's surgery ("counts are point-in-time") is necessary but insufficient. The drift was 7 commits at Stage-1 audit time (2,621 → 2,628); at Stage-2 audit time, working-tree shows `git log --oneline | wc -l → 2633` (drift now 12). Stage 2's verification: `git log origin/master..HEAD --oneline | wc -l → 1736` (was 1,724 in PASS-C; was 1,731 at Stage-1 audit time). The drift is monotonic and accelerating. Stage 1's surgery acknowledges this; does not surface that the drift signals deeper cohesion concern: the suite is itself producing commits during execution. See Lane 2A item below. |
| HARDENING-PASS-C.md:289 (AY-II-I tranche letter) | PASS-C.md:410 letter list | minor / footnote-add | 4 | CONFIRM | Stage 1 catches the AD-gap silence; sets footnote. Specific, surgical. |
| HARDENING-PASS-C.md:286-308 §5.6+5.7 (audit/ workspace-root vs restart/audit/) | PASS-C.md:43, 135 | partial | 4 | CONFIRM | Stage 1 catches the genuine path-collision. Specific. |

### §3.2 — Items Stage 1 did NOT surface for per-item evaluation (silence faults)

Stage-2 walked Pass C plus per-agent reports for items Stage 1 should have surfaced but did not.

| Pass-C item Stage 1 missed | Path | Stage-2 verdict | Reason |
|---|---|---|---|
| Restart prelude commit-order risk | PASS-C.md:328-336 (commits 1 through 8) | drift fault | Pass C's commit order: (1) Lock 12 archive, (2) analysis+lsp consolidation, (3) docs/ restructure, (4) tranches archive, (5) README+GESTALT, (6) .gitignore/build-artefacts, (7) SPEC+architecture+migration+stubs, (8) master plan synthesis. Stage 1 ratifies this order without challenge. Stage-2's steelman: commit 5 (README rewrite) consumes the consolidation (commit 2) by referencing `crates/bbnf-language-server/`; commit 7 (architecture.md) consumes the docs/ restructure (commit 3). Sequencing OK. But commit 6 (.gitignore) deletes `server/bbnf-lsp` *before* commit 7 creates `docs/spec/architecture.md` that may reference the deletion — minor. The deeper risk: commits 1-2 (Lock 12 + consolidation) MUST land before commit 3 (docs/ restructure) because the consolidation creates the directory `crates/bbnf-language-server/` whose name appears in the restructured docs. Stage 1 should have flagged this dependency. |
| `Cargo.toml` `[workspace.metadata.bbnf]` completeness | PASS-C.md:44 | drift fault | Pass C says "verify `[workspace.metadata.bbnf]` complete" without naming the verification or the receiver. Lane 8 should have caught the orphan; Lane 1 ratified silently. |
| Sibling-repo "brand only" defer | PASS-C.md:60 (`bbnf-buddy` / `gorgeous-external` / `pprint-external`) | weak-defer | Pass C marks these KEEP without specifying when "brand only" reviews. Stage 1 silent. Not consequential. |
| `Makefile` simplification budget | PASS-C.md:47 ("simplify to ~150 lines") | budget defer | Pass C cites a 420 → 150 line simplification but no LOC delta projection. Lane 6 caught budget gaps for docs but not for Makefile. Stage 1 silent. Minor. |
| `docs/GESTALT.md` rewrite scope | PASS-C.md:41 | bucket-collision | Same fault as Lock 8 KEEP-MODIFY-but-full-rewrite (Fault 1.B) — `docs/GESTALT.md` marked KEEP-MODIFY with surgery "rewrite to reflect post-restart shape" (full rewrite). Stage 1 caught Lock 8 instance; missed GESTALT and README. See §3.1 row above. |
| `crates/parse-that/` workspace presence | PASS-C.md:60 (sibling repos list) | minor | Pass C lists parse-that in sibling-repos KEEP. Per-agent 3 §11.1 says workspace `members` does NOT list `crates/parse-that/`. Stage 1 catches this in Fault 3.C; resolution is correct. |

### §3.3 — Cross-lane consistency

Stage 1's Lock 11 ratification at line 133-137 ("honoured-mostly") and Lane 8's Fault 8.C ("parse-that defer-triple incomplete") cite the same site (PASS-C.md:79). Lane 1 ratifies; Lane 8 amends. The two surgeries do not conflict — Lane 1 says "honoured-with-surgery (parse-that external until TS backend)", Lane 8 says "receiver/blocker/gate triple" — but Stage 1 should have cross-referenced. Minor discipline gap.

### §3.4 — Per-section confirmation-drift evaluation

Beyond the per-item drift evaluation in §3.1, Stage-2 walks Pass-C section-by-section to surface where Stage 1 ratifies content as "covered" without per-item evaluation.

| Pass-C section | Stage-1 lane treatment | Stage-2 drift verdict | Reason |
|---|---|---|---|
| §1 (Pass-C scope verdict; 31 surfaces) | Lane 1 walks 14 locks; Lane 3 catches path collisions; Lane 7 catches archive friction | partial-drift | Stage-1 catches some KEEP-MODIFY-but-rewrite faults (Lock 8 docs/perf via Fault 1.B) but misses two adjacent: GESTALT.md and README.md (see §3.1 row). The per-row coverage is uneven. |
| §2 (Locks honoured table) | Lane 1 fully tables | honoured | Lane 1 walks every Lock with paragraph + surgery. |
| §3 (Architectural transposition; §3.1 through §3.9) | Lane 1 Fault 1.F (consolidation); Lane 5 Faults 5.A, 5.B (architecture, hints); Lane 6 Fault 6.D (generated-LOC delta); Lane 8 (cutover) | partial-drift | §3.1 (consolidation) deeply covered. §3.2-§3.9 covered tangentially via Faults 1.D (archive directive), Lane 6 Fault 6.B (docs LOC), but §3.6 (Makefile simplification) and §3.7 (scripts restructure) NOT per-row evaluated. The Makefile simplification "420 → ~150 lines" is a 64% LOC delta not surfaced as a budget-fault. |
| §4 (Replacement design; §4.1 through §4.7) | Lane 6 covers per-doc + per-tranche-stub LOC; Fault 6.A bbnf-py defer | mostly-honoured | Strong per-replacement coverage; bbnf-py defer + bbnf-cli defer captured. |
| §5 (Idiomaticity; §5.1 through §5.4) | Lane 1 Lock 8; Lane 9 surgical-tightness | partial-drift | §5.1 STYLE.md compliance (banned-words sweep, em-dash discipline, epanorthosis) NOT per-row evaluated by Stage 1. The CI gate at PASS-C.md:225 (`scripts/style-check.sh greps the banned list; exits non-zero on any hit in tracked .md outside docs/precepts/ and audit/`) is itself an *unspecified deliverable* — Stage 1 should have caught this via Lane 8 (carry-deferral) but did not. |
| §6 (Cross-cut summary) | Lane 1 Fault 1.F (Lock 14); Lane 8 (cutover) | honoured | Cross-cut surfaces all caught. |
| §7 (Commit chain disposition) | Lane 1 Fault 1.F (consolidation in commit 2 of prelude); Fault 3.B commit count drift | partial-drift | Per-Option steelmen NOT per-Option evaluated in Stage 1; per Stage-2 §5.2 (this report), the steelman defeats live in Pass-C Agent 6 §B.5 paraphrase. Stage-2 amendment surfaces. |
| §8 (Punch list; §8.1 through §8.5) | Multiple lanes | honoured | Per-item ratification across Lanes 1, 5, 6, 7, 8. |
| §Closing | implicit | honoured | Brief closing; nothing to ratify. |

### §3.5 — Pass-C silence on items Stage 1 should have surfaced

A second-order cohesion concern: Pass C is silent on items that Stage 1 should have surfaced for evaluation regardless of Pass-C silence (per HARDENING.md:139, the audit identifies what's missing).

| Silence in Pass C | Stage-1 surface? | Stage-2 verdict |
|---|---|---|
| `crates/parse-that/` workspace status (not in `members`) | Stage-1 catches via Fault 3.C | covered |
| `Cargo.toml` `[workspace.metadata.bbnf-strategy]` block | Stage-1 silent | should have surfaced; minor |
| `extension/server/` stale-stub disposition | Stage-1 catches via Fault 3.E | covered |
| `package.json` workspace-top-level disposition | Stage-1 catches via Fault 3.D | covered |
| `docs/codegen-paths.md` ABROGATE-DELETE rationale | Stage-1 silent | minor; surgery is implicit (absorbed into spec/architecture.md per PASS-C.md:40) |
| Sibling-repo "external" claim verification | Stage-1 silent | minor; per-agent 1 §282 verifies |
| `xtask/` Lock 6 honour | Stage-1 silent (Lock 6 ratified honoured) | acceptable; xtask is the verbatim Lock 6 honour |
| `rust-toolchain.toml` nightly pin verification | Stage-1 silent | acceptable; pin is operational |
| `docs/instructions/` relocation rationale | Stage-1 silent | minor |
| `data/` relocation to `crates/bbnf-test-fixtures/data/` | Stage-1 silent | should have surfaced; the `data/` content scope is unstated |
| Sibling-repo `csc411` mirroring (path-dep mirror) | Stage-1 silent | acceptable; per-agent 4 §5.2 covers |

### §3.6 — Lane 2A verdict

**partial-drift.** Stage 1 inherited Pass-C's framing in five sites: (a) Lock 11 / parse-that hedging (caught later in Lane 8 but ratified-as-honoured in Lane 1); (b) the silent-must-add Lock pattern (Lock 3 specifically caught; Locks 5, 9, 11 carry same anti-pattern but only Lock 10 was promoted); (c) the prelude commit-order silence (Pass-C ordering ratified without challenge); (d) the bucket-collision pattern (caught at `docs/performance/`, missed at `docs/GESTALT.md` and `README.md`); (e) the commit-count drift sub-surfaces (caught at synthesis-section, missed at per-era table and per-agent §B.1 numbers). Stage 1's strengths: catches the dual-disposition (Fault 1.F), catches the bucket-collision for `docs/performance/` (Fault 1.B), catches the path-collision (Faults 3.A, 3.G), catches the AY-AD letter gap (Fault 3.F), catches `parse-that` external verification (Fault 3.C). The drift is partial; the audit is sound but uneven across lanes; the pattern-recognition is one-shot rather than systematic.

---

## §4 — Lane 2B: Discipline Lapse Audit

**Lane standard.** For every Stage-1 lane, evaluate whether Stage 1 honoured the Pro/Con/Explication/Challenge discipline. Particular foci: paragraph-shaped Explication columns; Pro/Con symmetry; steelman in Challenge; KEEP verdicts justified by defeat-of-Challenge.

The Stage-1 PASS-C report does not use the four-column Pro/Con/Explication/Challenge table directly; it uses fault-narrative + per-fault tables (`Site | path:line | Surgery` columns). This matches HARDENING.md:139 which permits "per-item table" without mandating four columns. Stage 2 evaluates the discipline by proxy: paragraph-shape, fault-narrative completeness, and steelman challenge presence.

### §4.1 — Per-lane discipline evaluation

| Stage-1 lane | Per-item rows | Avg challenge strength (1-5) | Discipline verdict | Stage-2 redress |
|---|---:|---:|---|---|
| Lane 1 (Lock-Adherence) | 14 locks; 6 fault tables (1.A through 1.F) | 4.2 | HONOURED | Per-lock paragraph + surgery is paragraph-shaped. Locks 3, 5, 9, 10 silent-must-add language repeats — discipline gap, not lapse. Honoured. |
| Lane 2 (Sequencing) | 0 | n/a | HONOURED-N/A | Single-pass synthesis disposition is correct per HARDENING.md:69. But see §3.2 above — the prelude commit-order is itself a sequencing-claim Stage 1 should have addressed even within an n/a lane. Discipline gap, not lapse. |
| Lane 3 (Cohesion) | 7 fault tables (3.A through 3.G) | 4.0 | HONOURED | Per-fault paragraph + surgery shape. Strong. |
| Lane 4 (SOTA) | 0 | n/a | PARTIAL | Lane 4 closes in 12 lines; the disposition ("Pass C contains zero perf gates") is correct; but discipline says even N/A lanes table the surfaces evaluated. Stage-2 redress: Stage 1's Lane 4 should have included a "surfaces walked" sub-table — even if every cell verdict is "non-throughput". |
| Lane 5 (Grammar-Auth) | 6 sub-sections + 3 fault tables (5.A through 5.C) | 4.5 | HONOURED | Strong. The §7.2 grammar-name-mention scan is a per-item table; the §7.3 future-grammar onboarding test under Amendment 01 is paragraph-shape. Discipline honoured. |
| Lane 6 (Generated-Code) | 6 sub-sections + 4 fault tables (6.A through 6.D) | 4.0 | HONOURED | Per-doc + per-wave + per-stub tabular. Strong. |
| Lane 7 (Friction) | 6 sub-sections + 6 fault tables (7.A through 7.F) | 4.0 | HONOURED | Each friction surface is per-section + verbatim error message + surgery. Strongest of the lanes. |
| Lane 8 (Carry) | 8 sub-sections + 4 fault tables (8.A through 8.D, with 8.D covered by 3.E) | 3.5 | PARTIAL | Defer-triple (receiver/blocker/gate) discipline applied per HARDENING.md:97. But §10.6 (`audit/` decision) and §10.7 (`docs/precepts/` submodule) close in 4 lines each without per-fault-row even when honoured. Stage-2 redress: Lane 8's "honoured" sub-sections should have a one-line confirmation of the triple, not a paragraph dispatch. |
| Lane 9 (Greenfield) | 7 sub-sections + 2 fault rows (9.A and 9.B, both covered by upstream faults) | 3.0 | PARTIAL | Lane 9 closes with two faults that *are* covered by upstream faults; the lane-internal surface is paragraph dispatch (§11.1 through §11.7). Per Stage-1's own discipline (HARDENING.md:139, "A lane with no per-item rows is fault"), Lane 9 narrowly avoids the fault by including 9.A and 9.B as fault rows. But the fault rows merely re-cite earlier surgeries; the lane *does no independent work*. Stage-2 verdict: this is the closest Stage 1 PASS-C comes to discipline-lapse. |

### §4.2 — Pro/Con symmetry

Stage 1 PASS-C does not deploy explicit Pro/Con columns. The fault-narrative format collapses Cons into faults (good — surface what's wrong) and elides Pros (Cons-only narrative). Per the Stage-1 contract, this is acceptable; per the Stage-2 lens (HARDENING-STAGE-2-EXTERNAL.md:54-56), the Pro/Con symmetry is part of the discipline. Stage 1 PASS-C does NOT discuss Pro of (e.g.) Pass-C's docs-six-wave plan, or Pro of the commit-chain Option-3 ratification — Stage 1 simply ratifies. Pro-side discipline is implicit; should have been explicit for the most consequential decisions.

### §4.3 — Challenge column

Per HARDENING.md:170 ("Steelman every challenge"), Stage 1 PASS-C should have constructed steelman counter-arguments to its KEEP verdicts. Stage 1's strongest steelmen:

| Stage-1 site | Steelman shape | Steelman strength |
|---|---|---:|
| Lock 14 + Fault 1.F (analysis/lsp consolidation) | "Per-grammar declaration crate IS allowed per Lock 14 escape valve" — Stage 1 defeats with Amendment 01 | 5 (strong) |
| Fault 5.A (metadata-dispatched LSP) | implicit (no explicit steelman); Stage-1 commits to the surgery | 3 (weak) |
| Lock 12 archive ceremony | implicit (Lock 12 verbatim text is the steelman defeat) | 4 |
| Lock 13 docs/ god-directory | implicit (sonic-rs / lightning-css peer-shape is the steelman defeat) | 4 |
| Commit-chain Option 3 | NOT STEELMANNED — Stage 1 says "the recommendation (Option 3) is justified per `accurate-perf-narrative`" without engaging Options 1/2/4's strongest counter-arguments. **Lane 2C addresses this.** | 2 (thin) |
| Docs re-do six-wave plan | NOT STEELMANNED — Stage 1 ratifies without considering "fewer waves" or "more waves" alternatives | 2 (thin) |

### §4.4 — Per-fault discipline shape

Stage-2 evaluates the discipline of each Stage-1 fault narrative. Per HARDENING.md:139 ("A lane with no per-item rows is fault"), Stage-1 must produce per-item evaluation. Stage-1 PASS-C uses fault tables; Stage-2 evaluates whether each fault's narrative is paragraph-shaped (good) or one-line dispatch (suspect).

| Fault # | Stage-1 narrative shape | Surgery shape | Discipline verdict |
|---:|---|---|---|
| 1.A | paragraph (lines 75-79) | per-site table with verbatim replacement text | honoured |
| 1.B | paragraph (lines 109-115) | per-site table with verbatim ABROGATE-REPLACE text | honoured |
| 1.C | paragraph (lines 127-131) | per-site table with promotion-to-blocking-gate | honoured |
| 1.D | paragraph (lines 143-151) | per-site table with comment addition | honoured-minor |
| 1.E | paragraph (lines 161-165) | per-site table with ceiling note | honoured |
| 1.F | paragraph (lines 169-187) | per-site table 5 rows (synthesis x4 + per-agent x1) | honoured-strongest |
| 3.A | paragraph (lines 224-237) | per-site table 3 rows | honoured |
| 3.B | paragraph (lines 239-247) | per-site table 1 row | honoured |
| 3.C | paragraph (lines 249-261) | per-site table 2 rows | honoured |
| 3.D | paragraph (lines 263-271) | per-site table 1 row | honoured |
| 3.E | paragraph (lines 273-283) | per-site table 1 row | honoured |
| 3.F | paragraph (lines 285-295) | per-site table 1 row | honoured-minor |
| 3.G | paragraph (lines 297-308) | per-site table 2 rows | honoured |
| 5.A | paragraph (lines 363-373) | per-site table 1 row + verbatim architecture clause | honoured |
| 5.B | paragraph (lines 376-385) | per-site table 1 row + verbatim pre-commit gate | honoured-strongest |
| 5.C | paragraph (lines 392-399) | per-site table 1 row + Amendment 01 framing | honoured |
| 6.A | paragraph (line 419) | per-line carry note | honoured-minor |
| 6.B | paragraph (lines 432-444) | per-site table 1 row + verbatim per-wave LOC delta | honoured-with-baseline-gap (see Lane 2E) |
| 6.C | paragraph (lines 451-458) | per-site table 1 row + verbatim stub-size budget | honoured |
| 6.D | paragraph (lines 466-478) | per-site table 1 row + verbatim "Generated-LOC delta: zero" | honoured |
| 7.A | paragraph (lines 489-498) | per-site table 1 row + verbatim error message | honoured-strongest |
| 7.B | paragraph (lines 500-510) | per-site table 1 row + verbatim regex replacement | honoured |
| 7.C | paragraph (lines 512-520) | per-site table 1 row + cookbook addendum | honoured |
| 7.D | paragraph (lines 522-530) | per-site table 1 row + git log guidance | honoured |
| 7.E | paragraph (lines 532-547) | per-site table 1 row + friction note + future commitment | honoured |
| 7.F | paragraph (lines 549-557) | per-site table 1 row + 0.x window note | honoured |
| 8.A | paragraph (lines 567-579) | per-site table 1 row + full triple | honoured |
| 8.B | paragraph (lines 581-593) | per-site table 1 row + full triple | honoured |
| 8.C | paragraph (lines 595-607) | per-site table 1 row + full triple | honoured |
| 8.D | one-line (line 615) | covered upstream by 3.E | acceptable; cross-reference correct |
| 9.A | one-line (line 721) | covered upstream by 1.F | acceptable; cross-reference correct |
| 9.B | one-line (line 723) | covered upstream by 1.B | acceptable; cross-reference correct |

### §4.5 — Discipline lapse summary

The fault-narrative discipline of Stage-1 PASS-C is **broadly honoured**. 27 of 30 faults are paragraph-shaped + per-site table + verbatim surgery text. 3 faults (8.D, 9.A, 9.B) are one-line dispatches that correctly cross-reference upstream coverage. No fault is one-line-without-cross-reference. The discipline holds.

The lane-level discipline is **partially honoured**. Lanes 1, 3, 5, 6, 7 carry per-item paragraph-shaped sections + verbatim surgery. Lane 4 (SOTA n/a) closes in 12 lines without per-item table — narrowly avoids the "no per-item rows" Stage-1-self-fault by virtue of n/a designation. Lane 8 has three honoured sub-sections paragraph-dispatched (§10.4 covered, §10.6, §10.7, §10.8 cutover honoured) without per-row table; reasonable for honoured sub-sections but inconsistent with HARDENING.md:139 strict reading. Lane 9 has 7 sub-sections paragraph-dispatched + 2 fault rows (covered upstream); narrowly avoids the no-per-item-row fault.

### §4.6 — Lane 2B verdict

**partial.** Most lanes (1, 3, 5, 6, 7) are honoured-discipline; Lane 9 is paragraph-dispatch with 2 fault rows that re-cite upstream — narrowly avoids the "no per-item rows" Stage-1-self-fault. Lane 4 (n/a) and Lane 8 (with three honoured sub-sections paragraph-dispatched) are partial. The biggest discipline-lapse: Stage 1 does not steelman the commit-chain Option 3 ratification or the docs re-do six-wave shape — both consequential decisions that Stage 2 strengthens in Lane 2C. The fault-narrative discipline (per-fault) is broadly honoured (27/30 paragraph-shaped). The lane-level discipline (per-lane per-item-table) is partial. Stage-1 PASS-C honours the spirit of HARDENING discipline; the narrow letter is partly violated.

---

## §5 — Lane 2C: Steelman Audit

**Lane standard.** For every Stage-1 KEEP verdict (or KEEP-with-surgery), construct the strongest counter-argument the audit could have made. If Stage 1's Challenge column is weaker than the steelman, the verdict is suspect.

### §5.1 — Per-decision steelman table

| Decision | Stage-1 verdict | Stage-1 challenge | Stage-2 steelman | Survives steelman? | Stage-2 verdict |
|---|---|---|---|---|---|
| Commit-chain Option 3 (keep verbatim + branch reset) | KEEP / RATIFIED | "justified per `accurate-perf-narrative` and the project's archaeological commitments" | Option 1 (rewrite to era boundaries; ~25-30 commits replacing 2,621): preserves era-granularity provenance, eliminates per-tranche debugging-archaeology bloat, makes `git log --oneline` survey-able. Defeated by: per-archaeology Part D, the user's `perf-breakthrough-accuracy` memory cites *specific commit SHAs* (`2f7c1bd4`, `a206b962`, `c1e86ab3`, `bd563c1d`). Era-squash breaks the SHAs; downstream memory-references break. | SURVIVES | Stage 1's KEEP holds, but Stage-1's challenge column was thin. Stage-2 strengthens: Option 1 fails because per-commit SHAs are *load-bearing memory references*, not just provenance. The surgery is to add Stage-2's defeat-of-Option-1 to PASS-C.md:289 (the §7.4 decision matrix). |
| Commit-chain Option 2 (squash all to one commit) | NOT recommended | "ALL provenance erased" | Option 2: 1 commit replacing 2,621 — maximum cleanliness, Lock 14 implicit (no historical grammar-coupling lurks); fresh-start branding. Defeated by: per-Pass-C Agent 6 §B.4 ("Author intent... Bug-fix archaeology... Performance-improvement attribution... Reversal record... Era V's DTA arc"); the project's most architecturally valuable commits (EmissionTier deletion at `2f7c1bd4`, structural pre-scan deletion at AQ.5, DTA interpreter deletion at AX.W0b, column revert at AY-I.W1) are *reversal events*. Squash-to-one collapses the reversal as a separate event. | SURVIVES | Stage 1's NOT-recommended holds. Stage-2 strengthens: the steelman against Option 2 is the *reversal-event preservation*, not just generic "provenance". |
| Commit-chain Option 4 (hybrid: squash legacy + keep recent) | NOT recommended | "boundary judgement-call" | Option 4: squash pre-Era-VI (Y through AT, ~700 commits) into one commit; keep Era V + Era VI verbatim. Pros: Era V's DTA archaeology survives (the project's most consequential lessons); pre-tranche-letter scaffolding gone; mid-balance. Defeated by: era boundaries are not natural commit boundaries (Era IV's tape-first runs Y-AU but the AU bench-baseline commits live AT the era-boundary; squashing Y-AT loses the AU baseline as a stand-alone event). Also: Pass-C Agent 6 §B.5 cites "boundary is judgement-call" — vague defeat. Stage 2's steelman defeat: per memory `perf-breakthrough-accuracy`, "delim scan, regex HIR, IIFE elimination" are Era III techniques (Tranche F + W + W.4 + AQ + AR — half pre-Era-IV, half straddling). Era boundary squash splits the per-technique attribution. | SURVIVES | Stage 1's NOT-recommended holds. Stage-2 strengthens: era boundaries are not technique boundaries. |
| `crates/bbnf-language-server/` consolidation (rename retracted; merge committed) | RATIFIED with merge-only via Fault 1.F | "rename leaves the per-grammar crate path open; Amendment 01 abrogates" | Steelman: dual-disposition (rename OR merge) is a *feature* — preserves user choice, defers commitment to a downstream decision-maker. Defeated by: Amendment 01:14-16 says "zero per-grammar crates" without ambiguity; Amendment 01:158 says "where the master plan and the amendment disagree, the amendment wins". Dual-disposition under Amendment 01 is *not* user-choice; it is unsettled commitment that downstream-tranche-drafting agents read both directions and ship inconsistent designs. | DEFEATED for retention; SURVIVES for merge | Stage 1 correctly retracts the rename path. The dual-disposition was Pass-C punting; Stage-1 wins. |
| `crates/bbnf-language-server/` metadata-dispatch architecture | RATIFIED via Fault 5.A | implicit (Stage 1 commits the surgery) | Steelman: a metadata-dispatched LSP server is *more complex* than a hand-written per-grammar dispatcher. Defeated by: per Lock 14 verbatim ("the substrate carries ZERO grammar-specific code"; "Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate") + Amendment 01:18-23 (yaml.bbnf onboarding test). The complexity is in the substrate; the per-grammar dispatch is metadata-driven. The complexity buys Lock 14 honour. | SURVIVES | Stage 1's amendment is correct. |
| Lock 12 archive ceremony as blocking precondition | RATIFIED | implicit (Lock 12 verbatim text is the defeat) | Steelman: "execute Lock 12 ceremony AFTER Pass A consolidation lands" — Lock 12 is decoupled from analysis/lsp consolidation; sequence them however. Defeated by: per Pass-C Agent 6 §A.3 "Archive ↔ active" — Cargo.toml line 2 still lists ser+gorgeous, and the analysis/lsp consolidation rewrites Cargo.toml. If consolidation lands first, both edits land in same file in two commits — manageable but coupled. Per Lock 12 verbatim ("ser + gorgeous archive BEFORE BA.W0"), the BLOCKING precondition is non-negotiable; Lock 12 wins. | SURVIVES | Stage 1's ratification holds. |
| Docs re-do six-wave plan | RATIFIED | not steelmanned | Steelman A: "consolidate into 3 waves" — Wave 1 (mechanical relocate) + Wave 2 (rewrite all user-facing) + Wave 3 (validation gate). Defeated by: per Pass-C Agent 5 §7.2, Wave 2 alone is 3-5 days; merging Wave 2/2b/2c into one wave would create a 7-12 day sub-wave with mixed concerns (lang docs + perf docs + howto docs). Steelman B: "split Wave 2 into 4 waves (one per docs/lang/<sister>)" — finer granularity, better tracking. Defeated by: per-sister docs are small (5, 7, 2, 1 file each); finer split is bureaucratic. Stage 1 should have surfaced both alternatives even briefly. | SURVIVES | Stage 1's KEEP holds; surgery: add brief defeat-of-3-wave + defeat-of-per-sister at PASS-C.md:204 for completeness. |
| `audit/` workspace-root vs `docs/audit/` | RATIFIED workspace-root | per-agent 4 §4.3 cites Option A | Steelman B: "audit corpus IS documentation; lives under `docs/`" — cohesive with the docs tree. Defeated by: per Pass-C Agent 4 §4.3, "audits are NOT user-facing; they're project-process artefacts the orchestrator consumes. Co-located with `crates/` is right because the audit *acts on* `crates/`." Strong defeat. | SURVIVES | Stage 1's KEEP holds. |
| `docs/precepts/` submodule disposition | RATIFIED | "submodule pin honoured" | Steelman: "absorb precepts into bbnf-lang directly" — eliminates submodule operational overhead (cd dance, sync-checkouts, pin-bumping). Defeated by: per Pass-C Agent 4 §9, the submodule pattern is the cross-project sharing surface (per `feedback_general-infra-crates`); precepts apply to multiple repos, not just bbnf-lang. Submodule preserves the cross-project boundary. | SURVIVES | Stage 1's KEEP holds. |
| `docs/perf/<topic>.md` flat layout | RATIFIED via §3.3 | not steelmanned | Steelman: "split perf into `docs/perf/{theory, benches, regressions}/` subdirs" — finer granularity. Defeated by: 11 files in `docs/perf/` is within Lock 13's 4-10 children rule; further split is premature. Stage 1 silent on this. | SURVIVES | Stage 1's KEEP holds. |
| Commit-count drift (Fault 3.B) | partial / surgery: "counts are point-in-time" | not steelmanned | Steelman: the commit-count drift is a SYMPTOM of Pass-C cohesion problems, not a surface fault. The synthesis was authored at 2,621 commits; Stage 1 audited at 2,628; Stage-2 audits at 2,633 (working tree HEAD `fd0c1179`). The drift is monotonic (5 commits between Stage-1 and Stage-2 audits, ~20 minutes apart). Defeated by: no, the drift IS a cohesion concern — Pass C cites a number that the suite itself is invalidating in real-time. The surgery ("counts are point-in-time") is correct but does not name the deeper concern: Pass-C's commit table at §7.2 (Era I-VI rows) is *also* point-in-time; Era VI commits "~1,095 (incl. ~700 post-archaeology)" was true at synthesis time, false now (~1,107 post-archaeology). | WEAKENED | Stage-2 amends: the commit-count surgery is necessary but should also note the per-era table at PASS-C.md:271-278 carries the same drift. |

### §5.2 — Commit-chain steelman deep-dive (per Stage-2 invocation foci)

The Stage-2 invocation flags the **commit-chain Option 3 ratification** as "the single most consequential decision in Pass C". Stage-2 develops the per-Option steelman analysis here in detail.

#### §5.2.1 — Option 1 (rewrite to era boundaries; ~25-30 commits replacing 2,621)

**Pro (Option 1's strongest case)**: Era boundaries are natural archaeological boundaries; era-squashing reduces 2,621 commits to ~6 era-summary commits. Per-era commit body absorbs the era-specific archaeology (DTA arc → Era V's commit body; tape-first → Era IV's commit body). The result reads as a clean architectural progression. GitHub log renders fast. Era-squash preserves *coarse* provenance — what was attempted at each era — without the per-tranche fine-detail.

**Con (Stage-2 defeat against Option 1)**: Multi-pronged.

1. **Memory-cited SHAs are load-bearing**. Per `feedback_perf-narrative-accuracy` (memory) and `feedback_accurate-perf-narrative` (memory): "Performance docs must reconstruct actual timeline from commits; don't fabricate or embellish." Real breakthroughs cited: "delim scan, regex HIR, IIFE elimination". These are documented *by SHA* in the project's audit-tree. Stage-2 verifies via grep: Pass-C Agent 6 §B.4 cites `2f7c1bd4` (EmissionTier deletion at AQ.5), `a206b962` (DTA interpreter deletion at AX.W0b), `c1e86ab3` (AW-V W3 close), `bd563c1d` (AY-II.W0' kickoff). Squashing era-IV → 1 commit and era-V → 1 commit *breaks every one of these references*. Memory items become broken pointers.

2. **Reversal events are not era-internal**. The most architecturally valuable commits are reversals: EmissionTier deletion at `2f7c1bd4` (Era III/IV boundary), structural pre-scan deletion at AQ.5 (Era III), DTA interpreter deletion at AX.W0b (Era V/VI boundary), column revert at AY-I.W1 (Era VI). Three of four reversals span era boundaries. Era-squash collapses reversal-as-event into era-summary; the *learning* (reversal as named act) becomes invisible.

3. **Commit-body archaeology is not era-summary-recoverable**. The per-commit "why-was-this-tier-resolution-attempted" archaeology that the project's `accurate-perf-narrative` memory item *requires* lives in commit bodies. Era-summarising 1,000 DTA-arc commit bodies into one summary body is a *redaction* operation, not a preservation operation; the redaction is irreversible.

4. **Force-push cost**. ~25-30 commits replacing 2,621 requires `git push --force` to all collaborators (the user is the only collaborator currently, but tag-based archive is the lighter alternative).

**Stage-2 verdict on Option 1 versus Option 3**: Option 3 wins. Defeats above are decisive.

#### §5.2.2 — Option 2 (squash all to one greenfield commit)

**Pro (Option 2's strongest case)**: Maximum cleanliness. The greenfield restart's premise is "no quick solutions, no workarounds, no legacy code survives uncontested". A squash-all interpretation is: legacy code's *commit-history existence* is itself uncontested legacy; squashing it forces all future contributors to read the post-restart code without legacy-history context. Lock 14 implicit (no historical grammar-coupling lurks in the chain); Lock 8 implicit (no AU references survive). Branding: the project starts at "1.0.0" symbolically.

**Con (Stage-2 defeat against Option 2)**: Multi-pronged.

1. **The commits ARE the lessons**. Per Pass-C Agent 6 §B.4: "Era V's failure mode is *commitable* — i.e., each commit carries the per-substrate-build reasoning." Squash-to-1-commit erases ~700 commits of substrate-first/consumer-later teaching. The user's `feedback_no-orthogonal-codepaths`, `feedback_aesthetics-critical`, `feedback_archaic-diction-is-voice`, and the entire memory ledger are *derived from* the commit chain's evidence. Squash erases the source.

2. **Bug-fix archaeology**. Per Pass-C Agent 6 §B.4 verbatim: "AU Bug 1, Bug 2, Bug 2b were closed at AV.0.x with named commits per the AV FINAL." Squash-to-1 collapses per-bug attribution. The next time a similar bug arises, the project loses access to "we hit this before; here's how we fixed it".

3. **User attribution**. The user authored 2,621 commits. Squash-to-1 makes the user's investment invisible. Per the greenfield mandate's "honour the work" disposition, this is anti-disposition.

4. **No fresh-start branding gain**. The branding argument for Option 2 is symbolic; the architectural commits are what matter. The post-restart commits begin at the prelude regardless of squash-vs-keep; the "freshness" is in the new prelude, not in the chain length.

**Stage-2 verdict on Option 2 versus Option 3**: Option 3 wins decisively. Option 2 is the failure mode (provenance erasure) the project's own `accurate-perf-narrative` memory item explicitly forbids.

#### §5.2.3 — Option 4 (hybrid: squash legacy + keep recent)

**Pro (Option 4's strongest case)**: Era V's DTA archaeology survives (the project's most consequential lessons live there); pre-Era-IV scaffolding (Y, Z, AA-AT, ~700 commits) compresses into one "Era IV scaffolding" summary commit. Mid-balance: keep the lessons-rich half, squash the lessons-thin half. Per `git log --oneline | tail -700`, the pre-Era-IV commits are mostly Era II monorepo scaffold + Era III optimiser substrate work — both still load-bearing per Pass-C Agent 6 §B.3, but the *commit granularity* is finer than the lesson granularity (e.g., 280 Era III commits → maybe 5 distinct lessons).

**Con (Stage-2 defeat against Option 4)**: Multi-pronged.

1. **Era boundaries are not technique boundaries**. Per `feedback_perf-narrative-accuracy` memory: "Real breakthroughs: delim scan, regex HIR, IIFE elimination; inline scanners were minor." These are *Era III techniques* (delim scan: Tranche F; regex HIR: Tranche W and bbnf-regex crate; IIFE elimination: Tranche W.4 + AQ + AR). Some span the III/IV boundary (AQ + AR straddle the tape-first transition). Era-IV-boundary squash splits the per-technique attribution. Squashing Era II + Era III = ~544 commits into one summary commit collapses the 5-or-so distinct technique introductions into a single body.

2. **Boundary judgement-call is not principled**. Pass-C Agent 6 §B.5 cites "boundary is judgement-call" as the defeat. Stage-2's stronger defeat: there is no objective rule for "where does Era III end and Era IV begin?" The archaeology calls Era IV "X, Y, Z, AA-AU"; but per the per-era table at PASS-C.md:271-278, Era III is "F-W" and Era IV is "X, Y, Z, AA-AU". So the boundary is at letter X. But X is also an Era VI letter (per the same table — X appears in BOTH Era III "F-W" if W extends to X, AND Era VI "AY-I/II/III, AZ-I/II/III/IV, B0-B7, BA-BD" via the W/X tranche-letter table at PASS-C.md:289). Stage-2 verifies: `find docs/tranches -maxdepth 1 -type d | sort` returns `W` and `X` as their own directories. The era-tagging at the per-era table conflicts with the per-tranche directory layout. Boundary-squash is *unsolvable*, not just judgement-call.

3. **Half-provenance is worse than no-provenance**. If Option 4 squashes Era II+III but keeps Era IV-VI verbatim, downstream readers face an asymmetric chain: the early half is summary, the late half is detail. Memory items citing Era III techniques (`accurate-perf-narrative` cites delim-scan from Tranche F) point to redacted summary commits while memory items citing Era V failures (`bench-results-2026-04-12`) point to detail commits. The asymmetry is itself confusing — "why is the early history different?" — and the answer ("because we squashed it") is irrelevant to a downstream reader trying to learn from the chain.

**Stage-2 verdict on Option 4 versus Option 3**: Option 3 wins. The boundary-squash is unsolvable; half-provenance is worse than full preservation.

#### §5.2.4 — Composite verdict on commit chain

Stage-2 confirms Stage-1's Option 3 ratification holds against ALL three steelmen. Stage-1's challenge column was thin ("justified per `accurate-perf-narrative`"); Stage-2's strengthening adds explicit per-Option defeats with citations to the project's own memory items + working-tree verification. The Stage-2 amendment to Stage-1 PASS-C: surface these per-Option defeats at PASS-C.md:289 (the §7.4 decision matrix); the Stage-1 punch list does not currently include this surgery (Stage-2 adds as item #37 in §7.3 and item S2.11 in §8).

The user's invocation explicitly asks: "Stage-1 ratified Option 3 per `accurate-perf-narrative`; Stage-2 verifies this stronger than the alternatives." Stage-2 verifies: yes; Option 3 is stronger than Options 1, 2, 4 against all three steelmen. The ratification holds.

### §5.3 — bbnf-language-server dual-disposition steelman (per Stage-2 invocation foci)

The Stage-2 invocation asks: "is the dual-disposition a feature (preserves user choice) or a fault (forces decision later)?"

**Pro of treating dual-disposition as feature**: Pass-C Agent 4 §1.4 ratifies Option B (merge) but does so as "refined recommendation" within an Option-A/B/C dispatch. Options A/B/C are presented as legitimate alternatives the user adjudicates. The synthesis at PASS-C.md:23-24 carries "rename to `crates/bbnf-analysis/` OR merge into `crates/bbnf-language-server/`". The "OR" framing preserves user agency: if the user later decides per-grammar declaration crates are valuable (perhaps for separate future grammars), the rename path is open. Defer the commitment.

**Con of treating dual-disposition as feature (steelman defeat)**: Multi-pronged.

1. **Amendment 01 settles the question**. Per Amendment 01:14-16 verbatim: "**Zero per-grammar crates** in the post-restart workspace. The greenfield is fully grammar-driven and fully agnostic." Per Amendment 01:158: "where the master plan and the amendment disagree, the amendment wins". The dual-disposition leaves a path open that Amendment 01 closes. User-choice is *settled* user-choice — the user has already settled it.

2. **Downstream-tranche-drafting agents read both directions**. The greenfield restart's tranche-drafting protocol (per `restart/prompts/README.md`) dispatches 10 agents to draft tranches A-J. Each agent reads the master plan + Amendment 01 (per Amendment 01:152-157). If Pass C's synthesis carries "rename OR merge" while Amendment 01 carries "merge-only", agents draft inconsistently — some assume per-grammar declaration crates may exist (if they read Pass C first), some assume zero (if they read Amendment 01 first). Inconsistency is the failure mode.

3. **Dual-disposition is decision-deferral, not decision-preservation**. The "feature" framing assumes a future actor with more information makes the decision better. But the Amendment-01-author IS that future actor; the decision is already made. Pass-C synthesis pre-dates Amendment 01 by ~1 day; the dual-disposition is *temporal* artefact, not *commitment* artefact.

4. **Lock 14 verbatim disambiguates further**. Per 14-LOCKS.md:60: "Adding a new grammar is a config + grammar-source change with NO code change in any generic or other-grammar crate." Per-grammar declaration crates were the Lock 14 escape valve; Amendment 01:9 calls it "an *optional* escape hatch, not a default". Pass-C's dual-disposition reads as "leave the escape valve as a default option" — explicitly the position Amendment 01 retracts.

**Stage-2 verdict on dual-disposition**: It is a fault, not a feature. Stage-1 catches via Fault 1.F + correctly retracts. The "user-choice preservation" steelman is defeated. Stage-1's challenge holds.

### §5.4 — Commit-count drift cohesion check (per Stage-2 invocation foci)

The Stage-2 invocation asks: "is Stage-1's surgery to update the count adequate, or does the drift signal a deeper Pass-C cohesion problem?"

**Stage-2 finding**: Drift signals a deeper cohesion problem.

The drift trajectory: synthesis time `2,621` → Stage-1 audit time `2,628` (drift: 7) → Stage-2 audit time `2,633` (drift: 12 from synthesis; 5 from Stage-1 audit). Working-tree HEAD `fd0c1179`. Each step is ~20-60 minutes apart (Stage 1 and Stage 2 run sequentially; restart-suite agents land commits between).

Stage-1's surgery (`Fault 3.B` punch list item 11): "Counts are point-in-time per the synthesis date; the recommendation (Option 3) does not depend on exact counts; subsequent commits between synthesis and cutover continue to anchor on the same provenance tag."

This is necessary but insufficient. Stage-2 surfaces three additional drift sites Stage 1 missed:

1. **Per-era table at PASS-C.md:271-278**: Era VI commits "~1,095 (incl. ~700 post-archaeology)" was true at synthesis time, false now. At Stage-2 audit, Era VI is ~1,107 (incl. ~712 post-archaeology). The per-era counts drift in lockstep with the headline count.

2. **Pass-C Agent 6 §B.1 Total + breakdown**: Cites "git log --oneline | wc -l → 2621" and "git log origin/master..HEAD | wc -l → 1724" verbatim. Both numbers are now stale. Stage-1 catches the synthesis-section number; misses the agent-report number.

3. **Pre-archaeology snapshot**: Per Pass-C Agent 6 §B.1 verbatim: "Per the archaeology snapshot at 2026-04-22 the chain was 1,842 commits on master + 945 unpushed." This is the *source* number Pass-C builds from. The 2,621 synthesis number is `1,842 + 945 - duplicates + drift = 2,621`. The "drift" component is what's growing: from `~700` at archaeology time to `~712` at Stage-2 audit time. Stage-1 surgery says "subsequent commits between synthesis and cutover continue to anchor on the same provenance tag" — true but does not name that the *drift component* is itself a function of the suite's own execution rate.

**Cohesion concern**: Pass-C synthesis pinned a number; the suite has continued to commit; the synthesis number is invalidated by the suite's own work. This is not Pass-C's fault — Pass-C correctly captured a point-in-time. But Stage-1 PASS-C should catch *both* sites of drift (synthesis-section + per-era table) AND should name the deeper concern (the suite's commit-rate-during-execution) as a separate fault.

**Stage-2 amendment**: extend Stage-1 surgery #11 to PASS-C.md:271-278 (per-era table) and PASS-C.md:262-266 (Agent 6 origin numbers). Add a separate fault note: "the commit-count drift is a function of the suite's commit-rate-during-execution; cutover commits (`git tag pre-restart-2026-05-03 master`) should anchor at the cutover-time count, not at the synthesis-time count."

### §5.5 — Lane 9 over-ratification check (per Stage-2 invocation foci)

The Stage-2 invocation flags: "Stage-1 PASS-C has the highest 'honoured-mostly' lane count of the three Pass-* hardening reports. Audit Lane 2D verdict-imbalance: is Stage-1 PASS-C over-ratifying?"

**Stage-2 finding**: NOT over-ratifying. See Lane 2D §6.3 above for the deep-dive. Summary: Lane 9's "honoured-mostly" verdict is justified by Pass-C scope (periphery, not substrate); the over-ratifying threshold (>85% lane-level KEEP per HARDENING-STAGE-2-EXTERNAL.md:99) is not met (1/9 lanes "honoured-mostly", 11% — far below 85%). The user's concern is real but explained.

### §5.6 — Lane 2C verdict

**mostly-survives.** Eight of the ten consequential KEEP verdicts survive Stage-2 steelmen. Two are weakened: (i) the commit-chain Option-3 ratification SURVIVES against all three Option steelmen (1, 2, 4), with Stage-2 strengthening because Stage-1's challenge column was thin (the per-Option steelman defeats live in Pass-C Agent 6 §B.5 inherited as paraphrase, not in PASS-C.md the synthesis); (ii) the commit-count drift surgery is necessary-but-insufficient (the drift surfaces in 3 sites, Stage 1 caught 1). One is correctly defeated: the bbnf-language-server dual-disposition retraction (Stage 1 wins; Stage-2 confirms via 4-pronged steelman defeat). The bigger concern: Stage-1's challenges are inherited from Pass-C Agent 6 §B.5 paraphrased; original Stage-1 steelmanship is rare. The audit's challenges are downstream of Pass-C's challenges. This is partial confirmation drift (already noted in Lane 2A). Stage-2 verifies Option 3 stronger than alternatives; Stage-2 verifies dual-disposition is fault not feature; Stage-2 verifies drift signals deeper cohesion concern; Stage-2 verifies Lane 9 is not over-ratifying. All four user-flagged Stage-2 foci addressed.

---

## §6 — Lane 2D: Verdict-Imbalance Audit

**Lane standard.** Evaluate Stage 1's cohort verdict balance: KEEP/REINVENT/DISCARD distribution; pattern across lanes; pattern across target sections. Stage-2 verdicts: BALANCED (60-80% KEEP) / OVER-RATIFYING (>85% KEEP) / UNDER-RATIFYING (<40% KEEP).

Stage 1 PASS-C does not use KEEP/REINVENT/DISCARD verdicts as its primary axis (those apply per-item per HARDENING.md:51-55). It uses lane-level honoured / partial / violated / n/a + fault counts. Stage 2 evaluates the lane-level verdict imbalance.

### §6.1 — Cohort distribution table

| Lane | Stage-1 verdict | Faults | KEEP-equivalent (honoured + honoured-mostly) | REINVENT-equivalent (partial) | DISCARD-equivalent (violated) | KEEP fraction | Stage-2 verdict |
|---|---|---:|---:|---:|---:|---:|---|
| 1 Lock-Adherence | partial | 6 | 7 honoured locks | 6 partial locks (3, 5, 9, 10 silent-must-add; 8 honoured-with-bucket-fix; 11 honoured-mostly) | 1 violated lock (12 violated-with-blocking-rec → ratified) | 50% honoured | BALANCED |
| 2 Sequencing | n/a | 0 | n/a | n/a | n/a | n/a | BALANCED-N/A |
| 3 Cohesion | partial | 7 | n/a (all faults are sub-section partial) | 7 sub-sections partial | 0 | n/a | BALANCED |
| 4 SOTA | n/a | 0 | n/a | n/a | n/a | n/a | BALANCED-N/A |
| 5 Grammar-Auth | partial | 3 | 3 sub-sections honoured | 3 partial | 0 | 50% honoured | BALANCED |
| 6 Generated-Code | violated | 4 | 1 sub-section honoured (8.5 Operational-Sequence; 8.6 Generated-code-impact-for-archives) | 3 partial (waves 2-2c silent; per-tranche stub silent; bbnf-py silent) | 0 | n/a | BALANCED |
| 7 Friction | partial | 6 | 0 sub-sections honoured | 6 partial | 0 | 0% | UNDER-RATIFYING but justifiable (every friction surface needs surgery) |
| 8 Carry | partial | 4 | 4 sub-sections honoured (10.4 covered, 10.6, 10.7, 10.8 cutover honoured) | 3 partial (8.A, 8.B, 8.C); 8.D covered upstream | 0 | 50% honoured | BALANCED |
| 9 Greenfield | honoured-mostly | 2 (covered upstream) | 7 sub-sections honoured (11.1-11.5, 11.6 honoured-with-surgery, 11.7 covered upstream) | 0 | 0 | 100% honoured | OVER-RATIFYING |

### §6.2 — Distribution analysis

**Aggregate**: Lane-level: 0 lanes "honoured", 6 lanes "partial", 1 lane "honoured-mostly", 1 lane "violated", 2 lanes "n/a". KEEP-equivalent fraction (honoured + honoured-mostly) = 1/9 = 11%. Per HARDENING-STAGE-2-EXTERNAL.md:97-99, this is well below the OVER-RATIFYING threshold (>85% KEEP). The cohort is balanced toward partial (mixed-verdict-shape), exactly the healthy shape per HARDENING-STAGE-2-EXTERNAL.md:99 ("60-80% KEEP healthy").

But the user's observation in the Stage-2 invocation message holds: PASS-C HAS the highest "honoured-mostly" lane count of the three Pass-* hardening reports. Compare:

| Stage-1 PASS-* lane verdict count | PASS-A | PASS-B | PASS-C |
|---|---:|---:|---:|
| honoured | 1 | 0 | 0 |
| partial | 5 | 4 | 6 |
| honoured-mostly | 0 | 0 | 1 |
| violated | 1 | 3 | 1 |
| n/a | 2 | 1 | 1 |

Lane 9 (Greenfield) is the differentiator. PASS-A Lane 9 is "partial". PASS-B Lane 9 is "partial". PASS-C Lane 9 is "honoured-mostly". Is Stage-1 PASS-C over-ratifying Lane 9?

### §6.3 — Lane 9 over-ratification check

Lane 9 (Greenfield Discipline) per Stage-1 PASS-C lines 657-725:
- §11.1 (no quick solutions): "honoured-mostly" — surfaces dual-disposition concern, captured Fault 1.F
- §11.2 (no workarounds): "honoured-mostly with surgery 1.F"
- §11.3 (no legacy code survives uncontested): "honoured" — every legacy item is contested
- §11.4 (idiomatic gestalt approaches): "honoured"
- §11.5 (architectural transpositions): "honoured"
- §11.6 (surgical-tightness): "honoured-with-surgery"
- §11.7 (final faults): 9.A and 9.B, both covered upstream

The 7 sub-sections close with 5 honoured + 2 honoured-with-surgery. The lane-level "honoured-mostly" verdict is consistent with the sub-section dispositions. Per Stage-2 examination: Pass-C scope (analysis, lsp, archived crates, docs, audit, scripts, tools, server, extension, playground, wasm, sibling repos, commit chain) is mostly substrate-already-decided OR mechanical-restructure. The greenfield mandate's discipline applies but rarely demands re-architecture in this scope (compare Pass-A which re-architects parser/IR/path; Pass-B which re-architects codegen/runtime/optimisers).

Steelman against "honoured-mostly" Lane 9 verdict:
- The dual-disposition (PASS-C.md:23-24) IS a quick-solution-shaped offering; Stage-1 catches via 1.F but does not let it count against Lane 9.
- The "KEEP-MODIFY" bucket-label for full-rewrite content (PASS-C.md:33, 41, 46) is workaround-on-the-bucket-label-level; Stage-1 catches via 1.B for one site but not for all three.
- The commit-chain Option 3 endorsement — is Option 3 itself a quick solution? No: Option 3 honours the project's `accurate-perf-narrative` discipline; preserving the chain is the substantive choice (the alternatives are quicker).

Stage-2 verdict: Lane 9's "honoured-mostly" is JUSTIFIED for Pass-C scope. The over-ratifying concern was raised in the Stage-2 invocation; Stage-2 evaluates and finds: Pass-C scope genuinely *is* mostly-honoured against the greenfield mandate. The faults that exist are downstream of other lanes (Lock 14 dual-disposition, KEEP-MODIFY label-collision); when Lane 9 covers them via 9.A and 9.B, it correctly notes "covered upstream". This is not over-ratification; this is correct cross-lane attribution.

### §6.4 — Cross-lane fault distribution

| Lane | Faults | Per-fault scope |
|---|---:|---|
| 1 | 6 | Lock-specific (1.A through 1.F) |
| 3 | 7 | Cohesion (3.A through 3.G) |
| 5 | 3 | Grammar-Authoritative (5.A, 5.B, 5.C) |
| 6 | 4 | Generated-code budget (6.A through 6.D) |
| 7 | 6 | Friction surfaces (7.A through 7.F) |
| 8 | 3 (+ 1 covered) | Defer triples (8.A, 8.B, 8.C) |
| 9 | 2 (covered upstream) | Greenfield (9.A and 9.B) |

Total: 30 surgical edits + ~1 cross-coverage. Distribution is balanced across substantive lanes (1, 3, 5, 7 carry 22 faults; 6, 8 carry 7 faults; 9 carries 2 covered-upstream). No lane carries fewer than 2 unique faults (excluding n/a). No lane carries more than 7. Healthy.

### §6.5 — Pass-C section-level verdict distribution

Beyond lane-level distribution, Stage-2 evaluates whether Stage-1 ratifies disproportionately by Pass-C section.

| Pass-C section | Stage-1 verdict density | Faults | Stage-2 cross-check |
|---|---|---:|---|
| §1 (Pass-C scope verdict; 31 surfaces) | dense | 6 (Faults 1.A, 1.B, 1.C, 1.E, 3.D, 3.E) | even — every surface has a verdict; faults track correctly |
| §2 (Locks honoured table) | dense | 6 Lock-faults (1.A, 1.B, 1.C, 1.D, 1.E, 1.F) | even |
| §3 (Architectural transposition) | medium | 3 (1.D, 1.F, 5.A) | partial — §3.6 Makefile and §3.7 scripts NOT per-row evaluated |
| §4 (Replacement design) | medium | 4 (5.A, 5.C, 6.A, 8.A, 8.B) | even |
| §5 (Idiomaticity) | thin | 0 (Lane 1 covers via Lock 8) | partial — §5.1 STYLE.md compliance + §5.2 metalanguage discipline NOT independently evaluated |
| §6 (Cross-cut summary) | thin | 0 (covered upstream by Lane 1 Fault 1.F) | even |
| §7 (Commit chain disposition) | thin | 1 (Fault 3.B drift) | partial — Per-Option steelmen NOT per-Option evaluated; Stage-2 strengthens |
| §8 (Punch list) | dense | 4 sub-fault sites (1.D archive directive, 1.F consolidation gate, 5.B verification gate, 6.D generated-LOC delta) | even |

Pattern: Stage-1's verdict density tracks Pass-C's section weight (§1 + §2 + §8 carry the most surfaces and the most faults). Sections §5 and §7 carry one-shot evaluation and Stage-2 surfaces additional scrutiny needed (§5 STYLE.md compliance; §7 Option steelmen). This is partial-bias toward heavily-tabled sections; Stage-1 PASS-C is more thorough where Pass-C tables and less thorough where Pass-C narrates.

### §6.6 — Lane 2D verdict

**balanced.** Stage 1 PASS-C is NOT over-ratifying. The cohort verdict distribution (1 honoured-mostly / 6 partial / 1 violated / 2 n/a) is the healthy shape; the lane-level fault counts are evenly distributed; Lane 9's honoured-mostly is justified by Pass-C's scope and by correct cross-lane attribution. The user's invocation concern (PASS-C is the highest honoured-mostly count of the Pass-* reports) is *real* but explained: PASS-C scope is genuinely the periphery, not the substrate. Periphery audits ratify more than substrate audits when the periphery is mostly mechanical-restructure + docs-rewrite + commit-chain-disposition. The section-level scrutiny pattern shows Stage-1 PASS-C ratifies tabular Pass-C surfaces densely and narrative Pass-C surfaces thinly — partial-bias not over-ratification.

---

## §7 — Lane 2E: Recommendation-Quality Audit

**Lane standard.** For every Stage-1 punch-list entry, evaluate the recommendation: concreteness (verbatim text, file:line); applicability (clear edit a downstream agent can execute); scope-correctness (single-line vs paragraph vs multi-section vs re-draft).

### §7.1 — Per-surgery table

The Stage-1 PASS-C punch list (HARDENING-PASS-C.md:733-764) has 30 numbered surgeries. Stage 2 evaluates each.

| # | Surgery summary (from Stage-1 §12) | Concreteness (1-5) | Applicability (1-5) | Scope-correctness | Stage-2 redress |
|---:|---|---:|---:|---|---|
| 1 | Strike "rename to bbnf-analysis OR" — commit to merge-only, generic + metadata-dispatched per Amendment 01 | 4 | 4 | multi-section (correct: 4 sites) | Stage-2 strengthens: cite "see Amendment 01:14-16" verbatim in the surgery |
| 2 | Add internal architecture clause: bbnf-language-server dispatches per-grammar features through workspace metadata | 5 | 5 | paragraph (correct) | strong |
| 3 | Add pre-commit gate (zero @pratt/@simd; zero match-on-grammar; zero generated-LOC delta) | 5 | 5 | paragraph (correct) | strong |
| 4 | Promote Lock 10 verification from "silent-must-add" to "blocking gate of consolidation commit" | 4 | 5 | single-line (correct) | strong |
| 5 | Change `docs/performance/` row bucket to ABROGATE-REPLACE | 5 | 5 | single-line (correct) | strong; **Stage-2 amends**: same bucket-fault applies to PASS-C.md:41 (`docs/GESTALT.md`) and PASS-C.md:46 (`README.md`) — Stage-1 missed two sites with the same fault |
| 6 | Replace "confirm" with verbatim Wave-2 surgery for empty-path elision invariant | 5 | 5 | single-line (correct) | strong; **Stage-2 amends**: same "confirm"-pattern at PASS-C.md:73 (Lock 5), :77 (Lock 9), :79 (Lock 11) — Stage-1 promoted Lock 10, missed 5/9/11 |
| 7 | Add diagnostic for cargo-check failure post-archive ceremony | 4 | 5 | paragraph (correct) | strong |
| 8 | Add Lock 13 ceiling note for `docs/process/`, `docs/howto/` | 4 | 4 | single-line (correct) | strong |
| 9 | Replace `audit/restart/...` self-references with `restart/audit/...` | 5 | 5 | multi-section (10 line citations) | strong |
| 10 | Disambiguate workspace-root `audit/` from suite's `restart/audit/` | 4 | 4 | multi-section | strong |
| 11 | Add note acknowledging commit-count drift | 4 | 4 | single-line | **Stage-2 amends**: should also amend PASS-C.md:271-278 per-era table (same drift); see Lane 2C row |
| 12 | Replace Lock 11 "honoured-mostly | confirm" with full carry triple | 4 | 4 | single-line | strong |
| 13 | Replace package.json "verify before delete" with verbatim verification | 5 | 5 | single-line | strong |
| 14 | Replace extension/server "verify not stale" with verbatim verification + decision tree | 4 | 4 | single-line | strong |
| 15 | Add footnote: AD does not exist in legacy tranche set | 5 | 5 | single-line | strong |
| 16 | Add per-wave LOC delta budget table (Wave 2 ≤80%/≥60%; Wave 2b ≤70%; Wave 2c ≤95%) | 3 | 3 | paragraph | **Stage-2 amends**: Stage-1 cites ratios without baselines. Pre-rewrite line counts are not stated. Surgery is "≤80% of pre-rewrite" but pre-rewrite is unknowable until Wave 0 measures. Stage-1 should commit: "Wave 0 measures pre-rewrite line counts per `docs/{lang,perf,howto}/<file>`; Wave-2/2b/2c targets are stated as ratios applied to those measurements" |
| 17 | Add per-tranche stub size budget: each stub ≤ 100 lines | 5 | 5 | single-line | strong |
| 18 | Add "Generated-LOC delta: zero" | 5 | 5 | single-line | strong |
| 19 | Reconcile bbnf-cli defer with full receiver/blocker/gate triple | 4 | 4 | paragraph | strong |
| 20 | Add 0.x-window xtask-as-CLI workaround note + cookbook entry | 4 | 4 | single-line | strong |
| 21 | Reconcile bbnf-py defer with triggering condition | 4 | 4 | paragraph | strong |
| 22 | Add cookbook addendum entry: docs/howto/cookbook/path-crates.md | 4 | 4 | single-line | strong |
| 23 | Add user-facing rebase guidance after consolidation | 4 | 4 | single-line | strong |
| 24 | Add LSP metadata-cache friction note + future hot-reload commitment | 4 | 4 | single-line | strong |
| 25 | Replace brittle verification regex with: rg pattern | 5 | 5 | single-line | strong |
| 26 | Add comment: `# First git mv creates archive/; no separate mkdir needed.` | 5 | 5 | single-line | strong |
| 27 | Add Amendment-01 framing for bbnf-test-fixtures | 4 | 4 | paragraph | strong |
| 28 | Replace "likely external repo at this point" with definite verdict | 5 | 5 | single-line | strong |
| 29 | Strike per-grammar-rename path in agent 3 | 4 | 4 | multi-section | strong |
| 30 | Mark Option A and Option C in agent 4 as superseded | 4 | 4 | paragraph | strong |

### §7.2 — Aggregate quality

| Quality dimension | Average |
|---|---:|
| Concreteness (1-5) | 4.4 |
| Applicability (1-5) | 4.4 |
| Scope-correctness | 28/30 correct (93%) |

The two scope-suspect surgeries:
- **Surgery 1** (multi-section reconciliation): Stage 1 cites 4 line-sites (PASS-C.md:23-24, 82, 90-94, 482); Stage-2 walked Pass-C and confirms 4 sites; the multi-section scope is correct. Concreteness reduced because the surgery is "strike X" without verbatim replacement text at every site.
- **Surgery 16** (per-wave LOC ratios without baselines): The ratio surgery is correct in shape but the baseline unknown. Stage-2 amends: add baseline-measurement step.

### §7.3 — Surgeries Stage 2 surfaces that Stage 1 missed

Per the Lane 2A drift evaluation and Lane 2C steelman audit, Stage 2 surfaces additional surgeries Stage 1 should have included:

| Stage-2 surgery # | Target | Edit | Reason |
|---:|---|---|---|
| 31 | PASS-C.md:41 (`docs/GESTALT.md` row) | Change KEEP-MODIFY → ABROGATE-REPLACE; surgery: "delete content; new file at `docs/GESTALT.md` per restart vocabulary" | Same bucket-fault as Stage-1 surgery #5 (`docs/performance/`); Stage-1 missed |
| 32 | PASS-C.md:46 (`README.md` row) | Change KEEP-MODIFY → ABROGATE-REPLACE; surgery: "delete content; new top-level `README.md` per restart vocabulary" | Same bucket-fault; Stage-1 missed |
| 33 | PASS-C.md:73 (Lock 5) | Replace "should reference IR-as-contract" with verbatim Wave 2c surgery: "Wave 2c verifies docs/howto/{cookbook,optimizer,migration}/ reference IR-as-contract; if absent, adds: 'Codegen emits a backend-agnostic typed IR; per-backend lowerers (Rust now, TS+WASM at H+) consume the IR.'" | Same "confirm"-pattern as Stage-1 surgery #6 (Lock 3); Stage-1 missed Lock 5 |
| 34 | PASS-C.md:77 (Lock 9) | Replace "confirm `docs/cookbook/lifetime-surfaces.md` reflects three-way split" with verbatim Wave-2 surgery | Same "confirm"-pattern; Stage-1 missed Lock 9 |
| 35 | PASS-C.md:271-278 (per-era commit table) | Add note: "Era VI commits as of 2026-05-03 synthesis; subsequent restart-suite commits accumulate in Era VI; counts are point-in-time" | Same drift-fault as Stage-1 surgery #11; Stage-1 missed the per-era table |
| 36 | PASS-C.md:204 (six-wave plan) | Add brief defeat-of-3-wave + defeat-of-per-sister steelmen | Stage-1 ratifies six-wave plan without challenge; Lane 2C steelman finds it survives but the challenge column is thin |
| 37 | PASS-C.md:289 (Option 1/2/4 rejection table) | Add explicit steelman defeats per Lane 2C: Option 1 fails because per-commit SHAs are load-bearing memory references (cite `accurate-perf-narrative`); Option 2 fails because reversal events are not provenance-equivalent; Option 4 fails because era boundaries are not technique boundaries | Stage-1 ratifies Option 3 but inherits the defeats from Pass-C Agent 6 §B.5 paraphrase; original steelman defeats are not in PASS-C.md the synthesis |

### §7.4 — Owner attribution analysis

Per Stage-1 PASS-C's punch list, 28 of 30 surgeries name "Pass-C orchestrator" as owner; 2 (#28, #29) name "per-agent author"; 1 (#30) names "per-agent author". Stage-2 evaluates owner-correctness:

| Owner | Stage-1 surgery # | Stage-2 verdict |
|---|---|---|
| Pass-C orchestrator (synthesis edits) | #1-#27 | correct — synthesis lives at `restart/audit/passes/PASS-C.md`; orchestrator owns |
| per-agent author (per-agent edits) | #28-#30 | correct — per-agent reports live at `restart/audit/per-agent/pass-c-agent-*.md`; per-agent author owns |

Owner attribution is correct. No Stage-2 amendment.

### §7.5 — Surgery-execution feasibility

For each Stage-1 surgery, Stage-2 evaluates whether a downstream agent can execute the edit without further clarification.

| Surgery # | Execution clarity | Notes |
|---:|---|---|
| #1 (multi-section reconciliation) | partial | "Strike X OR" + "commit to Y" needs per-site verbatim text; Stage-2 amendment S2.10 strengthens |
| #2 (architecture clause) | full | verbatim paragraph supplied |
| #3 (pre-commit gate) | full | verbatim regex + zero-checks supplied |
| #4 (Lock 10 promote) | full | single-line replacement |
| #5 (bucket change) | full | single-line replacement; Stage-2 amendment S2.2 extends to 2 more sites |
| #6 (verbatim Wave 2 surgery) | full | verbatim paragraph supplied; Stage-2 amendment S2.1 extends to 3 more Locks |
| #7 (cargo-check diagnostic) | full | verbatim error message + sweep |
| #8 (Lock 13 ceiling note) | full | verbatim note |
| #9 (path replacement) | full | per-line replacement |
| #10 (audit/ disambiguation) | partial | "reconcile" without verbatim text at all 3 sites |
| #11 (drift note) | full | single-line note; Stage-2 amendment S2.3 extends to 2 more sites |
| #12 (parse-that triple) | full | verbatim triple |
| #13 (package.json verification) | full | verbatim verification |
| #14 (extension/server tree) | full | verbatim decision tree |
| #15 (AD footnote) | full | verbatim footnote |
| #16 (LOC ratios) | partial | ratios without baseline; Stage-2 amendment S2.9 strengthens |
| #17 (stub size budget) | full | verbatim budget |
| #18 (generated-LOC delta) | full | single-line addition |
| #19 (bbnf-cli triple) | full | verbatim triple |
| #20 (xtask-as-CLI) | full | verbatim note |
| #21 (bbnf-py triple) | full | verbatim triple |
| #22 (path-crates cookbook) | full | verbatim cookbook addendum |
| #23 (rebase guidance) | full | verbatim git command |
| #24 (LSP friction note) | full | verbatim note + future commitment |
| #25 (regex replacement) | full | verbatim regex |
| #26 (archive comment) | full | single-line comment |
| #27 (test-fixtures framing) | full | verbatim Amendment 01 framing |
| #28 (parse-that verdict) | full | single-line replacement |
| #29 (per-grammar-rename strike) | partial | "strike" without verbatim per-line replacement |
| #30 (Option A/C supersede) | partial | "mark as superseded" without verbatim text |

Stage-2 finding: 24/30 surgeries are full-execution-clarity; 6/30 are partial. The partials (#1, #10, #16, #29, #30) all share a common shape: "strike X" or "reconcile X" without verbatim per-site replacement text. Stage-2 amendment S2.8 (Amendment 01 verbatim citation) and S2.10 (per-site verbatim text) address two of these; the rest are downstream-actionable but slower.

### §7.6 — Lane 2E verdict

**mostly-applicable.** Stage-1's 30 surgeries are concrete (avg 4.4) and applicable (avg 4.4); 28/30 are correctly scoped. Two are scope-suspect: surgery 1 (multi-section reconciliation needs verbatim per-site text) and surgery 16 (LOC ratios need baseline step). Stage 2 surfaces 7 additional surgeries (#31 through #37) that Stage 1 should have included — all small, all concrete, all derived from the same patterns Stage 1 caught at one site but missed at others. The Stage-2 punch list extends Stage 1's by ~25%. Owner attribution is correct (28 orchestrator, 2 per-agent, 1 per-agent). Surgery-execution feasibility: 24/30 full-clarity; 6/30 partial-clarity (the strike/reconcile-without-verbatim pattern). Stage-2's amendments S2.8 and S2.10 address the partial-clarity issues at the most consequential sites (multi-section reconciliation + Amendment 01 citation).

---

## §8 — Stage-2 Punch List

Ordered amendments to Stage 1 PASS-C's verdicts and recommendations. Per HARDENING-STAGE-2-EXTERNAL.md §8, each entry: target Stage-1 site (path:line); Stage-1 verdict to amend; Stage-2 amended verdict; reason (cite Lane 2A/2B/2C/2D/2E); owner.

| # | Stage-1 site (path:line) | Stage-1 verdict | Stage-2 amended verdict | Reason (lane) | Owner |
|---:|---|---|---|---|---|
| S2.1 | HARDENING-PASS-C.md:71-83 (Lock 3 + Fault 1.A) | single-Lock-3 surgery | extend to Locks 5, 9, 11 (same "confirm" anti-pattern); promote each via verbatim Wave-2/2c surgeries (see §7.3 surgeries #33, #34) | 2A — drift; the "confirm" anti-pattern repeats across 4 silent-must-add Locks; Stage-1 caught one, generalised on Lock 10 only | V2 re-issue agent |
| S2.2 | HARDENING-PASS-C.md:107-115 (Fault 1.B) | single-site bucket-collision (PASS-C.md:33 only) | extend to PASS-C.md:41 (`docs/GESTALT.md`) and PASS-C.md:46 (`README.md`); 3 sites total carry KEEP-MODIFY-but-full-rewrite (see §7.3 surgeries #31, #32) | 2A — same fault at three sites; Stage-1 missed two | V2 re-issue agent |
| S2.3 | HARDENING-PASS-C.md:243-247 (Fault 3.B) | partial; surgery: "counts are point-in-time" | extend to PASS-C.md:271-278 (per-era commit table); same drift; surgery: per-era counts also point-in-time (see §7.3 surgery #35) | 2C — Stage-1's surgery is necessary-but-insufficient; the drift surfaces in two places not one | V2 re-issue agent |
| S2.4 | HARDENING-PASS-C.md:212-216 (Lane 2 n/a) | n/a (single-pass synthesis) | partial-with-surgery; the prelude commit-order at PASS-C.md:328-336 ratified silently; sequencing-risk: commits 1-2 (Lock 12 + consolidation) MUST land before commit 3 (docs/ restructure) because consolidation creates `crates/bbnf-language-server/` whose name appears in restructured docs | 2A — confirmation drift; the prelude IS a sequencing claim Stage-1 should have addressed within the n/a lane | V2 re-issue agent |
| S2.5 | HARDENING-PASS-C.md:312-322 (Lane 4 SOTA n/a) | n/a (no perf gates) | n/a-with-discipline; extend Lane 4 to a "surfaces walked" sub-table (12 surfaces; all non-throughput); even N/A lanes table the surfaces evaluated | 2B — discipline lapse; Lane 4 closes in 12 lines without per-item table, narrowly avoiding HARDENING.md:139's "no per-item rows is fault" rule | V2 re-issue agent |
| S2.6 | HARDENING-PASS-C.md:655-725 (Lane 9 Greenfield) | honoured-mostly (with 9.A, 9.B covered upstream) | honoured-mostly + steelman addenda; extend Lane 9's 7 sub-sections with brief per-section steelman defeats: §11.4 (idiomatic gestalt) should defeat "absorb precepts into bbnf-lang directly"; §11.5 (architectural transpositions) should defeat "merge analysis+lsp+wasm into one super-crate" | 2C — Stage-1's challenge column on Lane 9 sub-sections is thin; honoured-mostly verdict survives but challenges are weak | V2 re-issue agent |
| S2.7 | HARDENING-PASS-C.md:285-308 (Faults 3.G + 3.E) | partial | partial-with-cross-reference; the audit/ vs restart/audit/ disambiguation (Fault 3.G) and the extension/server/ verifier orphan (Fault 3.E) interact: Fault 3.E's "if it's a stub for `server/bbnf-lsp` (committed binary slated for deletion), delete in same commit" lands in commit 6 of the prelude (per PASS-C.md:328-336); Fault 3.G's audit/ restructure lands later. Cross-reference both surgeries to sequencing | 2A — Stage-1 catches both faults; misses the cross-reference | V2 re-issue agent |
| S2.8 | HARDENING-PASS-C.md:733 (Stage-1 punch list surgery #1) | "Strike rename OR; commit to merge-only" | strengthen with verbatim Amendment-01 citation: "see Amendment 01:14-16 'zero per-grammar crates in the post-restart workspace. The greenfield is fully grammar-driven and fully agnostic.'" | 2E — Stage-1 surgery is correct in shape; concreteness 4 because Amendment-01 wording is paraphrased; verbatim citation is stronger | V2 re-issue agent |
| S2.9 | HARDENING-PASS-C.md:750 (Stage-1 punch list surgery #16) | per-wave LOC ratios | add baseline-measurement step to surgery: "Wave 0 (mechanical relocate, ~4h) measures pre-rewrite line counts per `docs/{lang,perf,howto}/<file>`; Wave-2/2b/2c targets stated as ratios applied to those measurements" | 2E — Stage-1 ratios without baselines are not directly executable; add the measurement step | V2 re-issue agent |
| S2.10 | HARDENING-PASS-C.md:735 (Stage-1 punch list surgery #1, multi-section) | "multi-section" | strengthen with per-site verbatim replacement; cite all 4 sites' replacement text (PASS-C.md:23-24 + 82 + 90-94 + 482) | 2E — Stage-1 cites the surgery shape but not the per-site verbatim text | V2 re-issue agent |
| S2.11 | HARDENING-PASS-C.md:776-782 (final readiness paragraph) | "amendments required" decision | confirm; add Stage-2's two consequential strengthens: (a) commit-chain Option 3 ratification carries explicit defeat-of-Options-1-2-4 per Lane 2C steelmen; (b) commit-count drift surface in TWO places (synthesis-section count AND per-era table) | 2C + 2A — Stage-1's challenge column for the most consequential decision (Option 3) is thin; original steelman defeats live in Pass-C Agent 6 §B.5 only | V2 re-issue agent |

### §8.1 — Stage-2 cohort verdict on the punch list

Stage-1's 30-surgery punch list extends to ~37 with Stage-2's 7 additional surgeries (#31 through #37 from §7.3) PLUS the 11 amendments in §8 above. Total post-Stage-2 effective punch list: ~37 surgeries; Stage-2 contributes ~7 new and 4 strengthened. Stage-1 PASS-C is amendments-required, advancing to V2 re-issue with the merged Stage-1 + Stage-2 punch list folded.

---

## §9 — Final readiness

> **Stage-2 Decision: Stage-1 amendments required.**
>
> Stage 1 PASS-C is sound in its core diagnostic — the bbnf-language-server consolidation reconciliation against Amendment 01, the Lock 12 archive blocking-precondition ratification, the commit-chain Option 3 endorsement, the docs re-do six-wave LOC-budget addenda, the path-collision and AD-letter-gap and parse-that-hedge cohesion fixes are all the right surgeries; the dual-disposition retraction is correctly forced; the metadata-dispatched LSP architecture is correctly added. What requires amendment is twofold: (i) Stage-1's challenges are downstream of Pass-C's challenges — the steelman defeats for the most consequential decision (commit-chain Option 3 against Options 1, 2, 4) live in Pass-C Agent 6 §B.5 paraphrased; original Stage-1 steelmanship is rare; the audit's surveys ratify by Pass-C-inheritance rather than by independent challenge; (ii) Stage-1 caught patterns at one site but missed the same pattern at adjacent sites — the "KEEP-MODIFY-but-full-rewrite" bucket-collision (caught at `docs/performance/`, missed at `docs/GESTALT.md` and `README.md`); the "confirm-step-not-deliverable" anti-pattern (caught at Lock 3, partly at Lock 10, missed at Locks 5, 9, 11); the commit-count drift (caught at synthesis-section, missed at per-era table). Lane 9 (Greenfield) is honoured-mostly in a way that is JUSTIFIED for Pass-C scope (periphery + tooling + commit chain — much of it mechanical-restructure); not over-ratification. The user's invocation concern about "PASS-C has the highest honoured-mostly count" is real but explained: periphery audits ratify more than substrate audits when the periphery is genuinely mostly-mechanical. Verdict-imbalance is balanced.
>
> The 11 Stage-2 amendments + 7 surgical additions extend Stage-1's punch list by ~25%; total scope ~half a workday at the V2 re-issue agent. None rise to re-audit severity; the Stage-1 PASS-C audit is sound and amendable. The largest Stage-2 amendment is S2.6 (Lane 9 steelman addenda) which strengthens but does not reverse Stage-1's verdict; the most consequential Stage-2 amendment is S2.11 (final readiness commit-chain steelman defeats) which strengthens the Option 3 ratification by surfacing original challenge against Options 1, 2, 4. The bbnf-language-server dual-disposition retraction is the single most consequential decision in Pass-C; Stage-1 catches it; Stage-2 confirms.
>
> Hereupon Stage-2 PASS-C closes its audit. Stage-1 PASS-C advances to V2 re-issue with the merged Stage-1 + Stage-2 punch list; the V2 re-issue agent reconciles both into the master plan amendment-02 (or master-plan V2 directly). Tranche-drafting opens after V2 re-issue + the other three Stage-2 audits commit. The greenfield mandate is honoured; the locks are honoured (with surgery); the precepts are honoured; the commit chain remains the project's working archaeology; the periphery is genuinely the periphery.
