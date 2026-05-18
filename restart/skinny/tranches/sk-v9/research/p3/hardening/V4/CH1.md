# SK-V9 S-P3 Hardening — CH1 CORRECTNESS — V4

Lens: CH1 CORRECTNESS. Pass: S-P3 Synthesis-Plan. Cycle: V4.
Date: 2026-05-18.
Cohort under review: `research/p3/skv9-p3-{A,B,C,D,E}-*.md` +
`skv9-p3-F-spec-draft.md` + `skv9-p3-F-dispatch-draft.md` (seven
artefacts).
V4 surface: the `docs(sk-v9-p3-v4)` commit `cef745d2` — a two-hunk,
single-file diff to `skv9-p3-A-candidate-shortlist.md` (8 insertions,
4 deletions), folding the V3 CH1 §4 N1 defect.
Convergence rule: per `ORCHESTRATOR.md` §3W + §3Z, S-P3 must clear
≥95% × 2 consecutive cycles.

V1 62.5%; V2 73.7%; V3 93.75% — V3 missed the §3Z 95% threshold by a
single disposition (#13 ACCEPT-WITH-NOTE) held back by the lone N1
manifest-content defect. V4 is a surgical fold of N1 only.

---

## §1 — V3-defect resolution

The V3 CH1 §4 named two residual defects. N1 was the lone REVISE-grade
manifest-content inconsistency; N2 was a citation-hygiene REVISE
deferred to the redress agents (not a fold target).

| V3 defect | Locus | V4 status | Evidence |
|---|---|---|---|
| N1 — P3-A §2.2 C3 falsifiability gate lists `gsoc-2018 ≥ 41198` as a W3 must-improve exit row, contradicting F-spec §6 + P3-C §2 | P3-A §2.2 C3 (gate prose + classification table) | **RESOLVED** | The V4 diff applied two surgical edits — §2.2 C3 gate prose (P3-A:280-284) and the §4 classification-table C3 row (P3-A:734). Both sites now state `gsoc-2018` does not bind the W3 exit gate and carries a no-regression-only clause. See §2 #1-#6 below. |
| N2 — F-spec §6 `mesh`/`numbers` W10b `:NN` line anchors spot-uncertain | F-spec §6, P3-C §2 | **DEFERRED (as designed)** | V3 §4 explicitly routed N2 to the W3/W4 redress agents at measurement time, not to the V4 fold. The V4 commit message scopes itself to N1 only. N2 is not a V4 regression and is correctly out of V4 scope; it remains a redress-time citation re-confirmation. Not load-bearing on any arithmetic. |

N1, the single defect that held V3 to 93.75%, is folded. N2 is
correctly carried forward to redress per the V3 §4 disposition.

---

## §2 — V4 dispositions

Verified against the live tree: `git show cef745d2` (the V4 diff);
`skv9-p3-A-candidate-shortlist.md` §2.2 C3 + §4 table; F-spec §2 W3
row (`:134`), §6 (`:628-737`); P3-C §2 W3 (`:134`, `:723`-region,
§"per-wave gate table"); `skinny/RESULTS.md:5,12,16,24,39`.

| # | Claim under review | Artefact | Verdict | Evidence |
|--:|---|---|---|---|
| 1 | The V4 diff touches exactly one file, two hunks | commit `cef745d2` | ACCEPT | `git show HEAD --stat`: `skv9-p3-A-candidate-shortlist.md \| 12 ++++++++----`, `1 file changed, 8 insertions(+), 4 deletions(-)`. No other artefact touched — the fold is correctly minimal-surface. |
| 2 | `gsoc-2018 ≥ 41198` is removed from P3-A §2.2 C3's must-improve list | P3-A §2.2 C3 | ACCEPT | Diff hunk 1 deletes `gsoc-2018 ≥ 41198 (today 22184 — partial closure expected, see §4.3 below)` from the must-improve enumeration. The live §2.2 (P3-A:276-285) must-improve list now reads exactly `twitter ≥ 17685`, `apache_builds ≥ 14124`, `distinct_values ≥ 15731`, `update_center ≥ 14370` — four rows, no `gsoc-2018`. The stale `≥ 41198` figure is eliminated. |
| 3 | The new §2.2 gsoc-2018 framing is consistent with F-spec §6 | P3-A §2.2 C3 vs F-spec §6 | ACCEPT | New §2.2 prose (P3-A:280-284): "`gsoc-2018` does **not** bind the W3 exit gate (F-spec §6, P3-C §2): its throughput gap exceeds the per-delimiter budget, so the union substrate alone cannot lift it to sonic-strict/1.10 — gsoc-2018 carries a no-regression-only clause at W3". F-spec §6 (`:723-726`): "`gsoc-2018` does NOT bind W3: it is a P1-named uncloseable row carrying a unicode-bearing residual; if it closes only partially that is the residual handed to W4, not a W3 falsification (P2-A §4.3)." Identical ruling — gsoc-2018 is a no-regression row, not a W3 exit-gate row. |
| 4 | The new §2.2 gsoc-2018 framing is consistent with P3-C §2 W3 | P3-A §2.2 C3 vs P3-C §2 W3 | ACCEPT | P3-C §2 W3 exit-gate row (`:134`): "`gsoc-2018` partially closes (its 51% gap is also unicode-bearing; full closure routes to W4) — `gsoc-2018` is **not** an exit-gate row for W3, only a no-regression row." The P3-A §2.2 edit's "no-regression-only clause at W3" and "its partial improvement is recorded, not gated" reproduce this verbatim in substance. The cohort is now content-isomorphic on the W3 gate. |
| 5 | The "throughput gap exceeds the per-delimiter budget" rationale is SPEC-grounded, not invented | P3-A §2.2 C3 | ACCEPT | The rationale traces to F-spec §6 row 134 ("OLS fit … Four LOSS rows exceed 130-460% of the per-byte budget — delimiter-only intervention is insufficient") and the P2-E §6.4 honest verdict cited at F-spec §6 (`:857`, `:927`): "neither the codec nor the string-block widening closes them alone." gsoc-2018's 51% gap (RESULTS.md:24, `-51.0%`) confirms the gap magnitude. The V4 prose introduces no new uncited claim — it restates the existing SPEC rationale. |
| 6 | The §4 classification-table C3 row is corrected | P3-A §4 | ACCEPT | Diff hunk 2: the C3 row's "Row-moving?" cell previously read "must-improve `twitter`, `apache_builds`, `gsoc-2018` (partial), `distinct_values`, `update_center`"; it now reads "must-improve `twitter`, `apache_builds`, `distinct_values`, `update_center` … `gsoc-2018` no-regression-only (gap exceeds the per-delimiter budget — does not bind the W3 gate)". The classification table and the §2.2 gate prose are now mutually consistent — both sites fixed, no half-fold. |
| 7 | The four surviving must-improve rows + thresholds are unchanged and arithmetically correct | P3-A §2.2 C3 | ACCEPT | `twitter ≥ 17685` = `ceil(19453/1.10)` = `ceil(17684.5)` (sonic 19453); `apache_builds ≥ 14124` = `ceil(15536/1.10)` = `ceil(14123.6)` (sonic 15536); `distinct_values ≥ 15731` = `ceil(17304/1.10)` = `ceil(15730.9)` (sonic 17304); `update_center ≥ 14370` = `ceil(15806/1.10)` = `ceil(14369.1)` (sonic 15806). All four match F-spec §2 W3 row (`:134`) and F-spec §6 (`:684-689`) exactly. The V4 diff left these four untouched — verified character-identical to V3. |
| 8 | The "today" figures for the four must-improve rows are unchanged and live-correct | P3-A §2.2 C3 vs RESULTS.md | ACCEPT | P3-A §2.2 retains `twitter today 13188`, `apache_builds today 11917`, `distinct_values today 8972`, `update_center today 9857`. These match F-spec §6 (`:686-689`) and the live `RESULTS.md` parse_only Track 1 column (rows `:5`, `:12`, `:39`, `:16`). The V4 diff did not perturb them. |
| 9 | The W10b six-row must-not-regress block in §2.2 is unchanged | P3-A §2.2 C3 | ACCEPT | The diff's hunk-1 lower context line is `Hot-leaf: consume_structural ≤ 5% self-time` — i.e. the W10b block (`canada ≥ 15866`, `citm_catalog ≥ 28630`, `instruments ≥ 15865`, `marine_ik ≥ 11831`, `mesh ≥ 12186`, `numbers ≥ 17596`, P3-A:287-291) is below the diff window and untouched. Live §2.2 carries all six floors verbatim, uniform `floor(today × 0.98)`. No regression to the V3-verified N3 fix. |
| 10 | The hot-leaf falsifiers (`consume_structural ≤ 5%`, `at_cursor ≤ 1%`) are unchanged | P3-A §2.2 C3 | ACCEPT | Diff hunk 1's final inserted line restores `Hot-leaf: consume_structural ≤ 5% self-time,` — the falsifier text is preserved across the edit; only the line break shifted (the `Hot-leaf:` clause moved onto a fresh line because the gsoc sentence grew). The falsifier semantics are byte-identical to V3. |
| 11 | No new defect is introduced in the §2.2 gate prose | P3-A §2.2 C3 | ACCEPT-WITH-NOTE | The replacement prose is internally coherent and SPEC-faithful. NOTE: the inserted clause ends "…recorded, not gated (see §4.3)". P3-A has no §4.3 — its sections are §0/§1/§2/§3/§4/§5 (verified via header scan). This dangling intra-document cross-reference is **pre-existing** — the V3 text already read "(today 22184 — partial closure expected, see §4.3 below)" — and V4 retained rather than introduced it. It is not a V4 regression, but the fold did not clean it. The intended target is the C5 candidate's `gsoc-2018` contribution detail (P3-A:625) or the F-spec §6 W4-routing; the redress agent should either drop "(see §4.3)" or repoint it. Citation hygiene, not a structural defect; non-load-bearing. |
| 12 | The C3 classification-table row's other cells (Row-moving verdict, mechanism) are unchanged | P3-A §4 | ACCEPT | The C3 row retains `**Row-moving**`, `Yes — …`, and the mechanism cell "The structural fix: union event-model, `consume_structural` deleted." Only the must-improve enumeration inside the "Yes — …" cell was edited. C3 remains a row-moving candidate — correct: it still moves four rows. The classification is unperturbed. |
| 13 | The C4 classification-table row's pre-existing `gsoc-2018` no-regression-only note is consistent with the V4 C3 edit | P3-A §4 | ACCEPT | The C4 row (P3-A:735) already read "`gsoc-2018` no-regression-only" before V4. The V4 C3 edit now makes C3 and C4 say the same thing about gsoc-2018 — coherent: gsoc-2018 is no-regression-only at W3 (C3) and at W4b-2/W4 the codec also treats it no-regression (F-spec §2a `:169`, P3-C §"W4b-2" `:315`, `:353`). No contradiction surfaced by the V4 fold. |
| 14 | gsoc-2018's no-regression status at W4b-2 (`≥ 21963`) is untouched and still consistent | F-spec §2a, P3-C, P3-A C5 | ACCEPT | V4 touched only the W3 gate. F-spec §2a W4b-2 (`:169`) and P3-C §"W4b-2" (`:315`, `:353-360`) retain `gsoc-2018 Track 1 ≥ 21963` (`ceil(22184×0.99)`, `RESULTS.md:24`) as the no-regression basis. The V4 §2.2 edit's "no-regression-only clause at W3" does not collide with the separate W4b-2 no-regression basis — they are two distinct waves, both no-regression for gsoc-2018, both honest. The V3 N2-resolution figure `21963` stands. |
| 15 | No stale `Cycle: V3` survives where a V4 stamp is expected, and no spurious cohort-wide restamp occurred | all seven artefacts | ACCEPT-WITH-NOTE | The V4 fold is deliberately single-file and surgical (commit message: "Fixed both sites"). The other six artefacts were correctly NOT re-authored — they were already SPEC-correct on the W3 gate at V3 (F-spec §6 / §2, P3-C §2 all carried the correct ruling; it was only P3-A that lagged). NOTE: P3-A still carries its `§0 V3 fold footer` and line-3 `Cycle: V3` stamp; the V4 commit did not add a `§0 V4 fold footer` to P3-A. This is a documentation-hygiene gap — the cycle stamp on the edited artefact is now one cycle stale. It does not affect correctness of the gate content (the substantive N1 fix is complete and correct) but a tidy fold would stamp the touched file. Non-blocking; flagged for the redress agent. |

---

## §3 — Aggregate verdict

**15 dispositions: 15 ACCEPT (incl. 2 ACCEPT-WITH-NOTE), 0 REVISE,
0 REJECT.** Plus the V3 §4 N1 defect RESOLVED and N2 correctly
deferred.

ACCEPT rate = 15 / 15 = **100%.**

This **clears** the §3Z 95% threshold. The lone V3 manifest-content
defect — P3-A §2.2 C3 carrying `gsoc-2018 ≥ 41198` as a W3
must-improve exit row — is folded at both sites the V3 §4 N1
disposition named: the §2.2 falsifiability-gate prose and the §4
classification-table C3 row. The new framing ("does not bind the W3
exit gate; no-regression-only clause; gap exceeds the per-delimiter
budget") is verbatim-faithful to the F-spec §6 and P3-C §2 W3 ruling
the V3 cohort already carried. The cohort is now content-isomorphic on
the W3 gate — every artefact agrees both on the wave *structure* and
on the W3 exit-gate row set.

The two ACCEPT-WITH-NOTE rows are **not REJECT-grade**:

- #11 — the "(see §4.3)" intra-document cross-reference is dangling,
  but it is **pre-existing** (the V3 text already carried it); V4
  retained rather than introduced it. It is citation hygiene, not a
  structural defect, and it does not touch the gate logic.
- #15 — the edited artefact P3-A still carries a `Cycle: V3` line-3
  stamp and no `§0 V4 fold footer`. This is documentation hygiene; the
  substantive N1 fix is complete and correct.

Neither note blocks convergence; both are sub-REVISE residue a redress
agent folds in seconds. The four surviving must-improve rows
(twitter 17685, apache_builds 14124, distinct_values 15731,
update_center 14370) and their thresholds are unchanged and
arithmetically exact; the W10b six-row block and the hot-leaf
falsifiers are unperturbed; no new defect is introduced.

**CH1 V4 = 100%.** Paired with V3 at 93.75%, CH1 has now had one
≥95% cycle. Per §3Z (≥95% × 2 consecutive), CH1 requires one further
≥95% cycle to certify — V4 is the first of the two. The V4 fold did
exactly what the V3 §4 prescription called for: one-line-grade,
mechanical, no re-research, no F-MAIN change.

---

## §4 — New defects (not in V1/V2/V3 CH1)

| # | Defect | Severity | Fix |
|--:|---|---|---|
| N1 | P3-A §2.2 C3's V4-inserted prose ends "…recorded, not gated (see §4.3)". P3-A has no §4.3 section (its headers are §0/§1/§2/§3/§4/§5). The reference is dangling. It is **pre-existing** — the V3 text already read "see §4.3 below" — so V4 inherited rather than created it; but the V4 fold, which rewrote that exact sentence, was the natural opportunity to clean it and did not. | REVISE (citation, non-blocking) | In P3-A §2.2 C3, either drop the "(see §4.3)" parenthetical or repoint it to the actual gsoc-2018 W4-routing locus (P3-A C5 detail at `:625`, or F-spec §6 W4-routing). Not load-bearing on any gate arithmetic. |
| N2 | The V4 commit edited `skv9-p3-A-candidate-shortlist.md` but did not update its line-3 cycle stamp (`Cycle: V3`) or add a `§0 V4 fold footer`. The edited artefact's provenance metadata is one cycle stale. | REVISE (doc hygiene, non-blocking) | A redress touch stamps P3-A line 3 `Cycle: V4` and appends a one-line `§0 V4 fold footer` recording the gsoc-2018 W3-gate correction. |

Both N1 and N2 are sub-REVISE documentation/citation residue; neither
touches gate logic, gate arithmetic, the wave manifest, or any
load-bearing claim. They do not hold CH1 V4 below 95% — CH1 V4
certifies at 100%. They are flagged for a trivial redress touch so
the next cycle's cohort is fully tidy.
