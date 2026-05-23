# CH3 REGRESSION (REDRESS) — Pass Alpha V2 Disposition

Date: 2026-05-23. Lens: CH3 per `ORCHESTRATOR.md §3W` row CH3, the dispatch context's §CH-3 at `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:114-127`, and the V2 addendum's §1 fold-verification overlay at `restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md:19-25`. Scope: re-apply CH3 over the V2 α-redispatched artefacts (commit `958406257`), verify that V1's two CH3 REVISEs (routed via F-10 and E-10) landed, and scan for fresh REDRESS-recurrence regressions introduced during the V2 fold cycle.

## §0 — Disposition summary

- ACCEPT-rate: 30 / 31 per-§ rows = **96.8 %** (above the §3Z 95 % convergence floor).
- REJECT count: 0.
- REVISE count: 1 (FRESH-FINDING — HANDOFF §7 carry-over guard count desynced from V2 F-1 reconciliation).
- Critical findings: 0 reopens of audit-closed routes; 0 silent regressions of admitted rows; both V1 CH3 REVISEs FOLD-LANDED verbatim per their dispatch-context text; the one new finding is a count-arithmetic desync between two HANDOFF sections that the V2 F-1 fold partially refreshed.

Overall: V2 converges the CH3 lens. Both V1 REVISEs landed cleanly — F-10 lifted the W10.3 nested_layout round-trip-rule trigger into SYNTHESIS §0.4 P-1 and HANDOFF §7 verbatim; E-10 disambiguated C-5's REDRESS scribe contract to 29 row-keyed entries (5 W14 + 23 SK-V13 CSS + 1 SK-V12 W1b) in both α-E §7 owner-paths and the §7 falsifiability gate. The one new REVISE is a residue of the V2 F-1 fold (CH6-binding numeric divergence reconciliation): SYNTHESIS §1.2 + §1.3 and HANDOFF §3 honestly reconcile to the wider 6 + 11 ROLLING-SOTA-DELTA population, but HANDOFF §7's audit-falsified carry-over guard at line 193 still cites the narrower "4 direct + 7 typed = 41 rows" total. The 6-row gap (2 direct + 4 typed) could permit a future S-P3 wave to admit a row under the §7 guard without invoking the wider PRUNE-1 revert — a soft regression vector the V1 lens did not see because the V1 numbers matched then.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `alpha-C-redress-digest.md` | §0 Binding Interpretation | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged on the binding-interpretation block; REDRESS-119/120 HISTORY clause still echoes the addendum verbatim. |
| `alpha-C-redress-digest.md` | §1 CSS L4 admits (131-135) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged; the SK-V12 W1b carry-over annotation still resolves through α-A authority + α-E E-10. |
| `alpha-C-redress-digest.md` | §1 Decision-engine fold (136-140) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §1 JSON direct admits (141-143, 159) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §1 JSON typed admits (145-148, 160) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §1 JSON typed measured rejects + correctness reject (149-153) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §1 JSON parse-only admits (154-158) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §1 Disposition summary table | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §2 P-1 through P-7 | ACCEPT | V1 ACCEPT; the V2 C-1 fold (CH5 §2 P-7 buffer-ownership triple-check at `alpha-C-redress-digest.md:362-370`) is CH3-orthogonal but strengthens P-7's load-bearing surface — no CH3 regression. |
| `alpha-C-redress-digest.md` | §3 Pattern-level summary table | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `alpha-C-redress-digest.md` | §4 Reopen obligations | ACCEPT (V1-STAND) | V1 ACCEPT; the round-trip-rule trigger on W10.3 nested_layout (the V1 REVISE-1 source) is now ALSO carried in SYNTHESIS + HANDOFF per F-10 — α-C is no longer the sole carrier. |
| `alpha-C-redress-digest.md` | §5 Closing posture | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | V1 ACCEPT; V2 added the CH6 F-1 numeric-reconciliation paragraph at lines 73-89 — the goalset enumeration "0 ADMITTED per plane × 4 surfaces" still binds at line 35-37 + 56-65; the wider 6+11 population is correctly captured. CH3-clean. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R1-R10) | ACCEPT | V1 ACCEPT; V2 added the F-11 `regen-{grammar}` family annotation at R4 (line 96) — does not alter R3/R7/R8's REDRESS routing. |
| `SYNTHESIS.md` | §0.4 P-1 through P-7 pre-blocks | **ACCEPT (FOLD-LANDED)** | V1 REVISE-1 (F-10) **FOLD-LANDED VERBATIM** at `SYNTHESIS.md:115-120`: "Per α-C §4, W10.3 nested_layout (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence; any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the same trigger." Matches the V1 fold text. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT (V1-STAND) | V1 ACCEPT; contracted deferral per PASS-ALPHA §4.4. |
| `SYNTHESIS.md` | §1 Corrected diagnosis (§1.1 - §1.3) | ACCEPT | V1 ACCEPT; V2 added the F-1 reconciliation paragraph at §1.3 lines 200-209 (4+7 dispatch vs 6+11 wider ledger) — both populations bound for PRUNE-1 revert; carries the +2 direct (marine_ik, instruments) and +4 typed (random, instruments, numbers, unicode_basic via W13.1-.4 + update_center W15.1) row-keyed enumeration. No row silently regressed. |
| `SYNTHESIS.md` | §2 Telemetry binding (new audit_overlay_verdict + track2_entry_point columns) | ACCEPT | V1 ACCEPT; V2 added `track2_entry_point` per CH5 F-15 — CH3-orthogonal; no REDRESS-recurrence vector enabled. |
| `SYNTHESIS.md` | §3 Candidate shortlist (C-1 to C-5) | ACCEPT | V1 ACCEPT; V2 added LOC + same-wave-consumer + risk columns per CH4 F-3/F-4/F-5 + CH7 F-17 (the C-3 round-trip dual-tree gate is now visible at line 273; C-5 row at line 275 carries "29 new row-keyed entries" matching E-10). C-5 still binds R3 PRUNE-1 + PRUNE-2 — same REDRESS scribe scope as V1. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT | V1 ACCEPT; V2 added F-6 per-wave LOC ceiling, F-9 triumvirate clause, F-12 forward invariant inheritance, F-13 two-grammar exercise, F-14 G-SIMD-GRAMMAR-POLICY triad. None re-opens a REDRESS-closed route. |
| `SYNTHESIS.md` | §5 Pre-blocked + unblocked routes | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged at lines 346-380; REDRESS-119/120 LIFTED clause at lines 355-356 still binds HISTORY-only with fresh-material-differential requirement. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT (V1-STAND) | V1 ACCEPT; V2 unchanged. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | V1 ACCEPT; V2 added the F-1 reconciliation paragraph at lines 79-89 carrying the 6+11 wider population — direct +2 (marine_ik, instruments) and typed +4 (random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + update_center W15.1) enumerated. CH3-clean as a row-by-row revert binding. |
| `HANDOFF.md` | §6 Next-move | ACCEPT | V1 ACCEPT; V2 added F-7 hard-cap echo and F-8 G-Omega restoration — CH3-orthogonal. |
| `HANDOFF.md` | §7 Refusal conditions | **REVISE (FRESH-FINDING)** | V1 REVISE-1 (F-10) **FOLD-LANDED VERBATIM** at lines 225-229 (the W10.3 nested_layout / ≥ 50× round-trip-trigger refusal bullet). HOWEVER, the audit-falsified-row carry-over guard at lines 192-194 still cites "25 CSS + 5 parse_only + 4 direct + 7 typed = **41 rows**" — the NARROWER dispatch §1 population. The V2 F-1 fold in HANDOFF §3 lines 79-89 + SYNTHESIS §1.2 + §1.3 reconciles to the WIDER 6+11 population (47 rows total: 25+5+6+11). This creates a 6-row gap (2 direct + 4 typed extras) the §7 guard fails to bind. See §2 FRESH-FINDING below. |
| `alpha-A-results-extraction.md` | §"ROLLING-SOTA-DELTA reconciliation" (V2 A-1 fold; lines 129-131 + 161-169) | ACCEPT | V2 fold A-1 + A-2 landed (the 2-line direct-reconciliation table at line 130 names "+2: marine_ik, instruments"; the typed +4 extension-row annotation lifts random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + update_center W15.1 at lines 192-197). The SK-V12 W1b CSS row carry-over (`declaration_values`) is named at line 233. CH3-clean. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 owner paths + REDRESS scribe contract | **ACCEPT (FOLD-LANDED)** | V1 REVISE-2 (E-10) **FOLD-LANDED VERBATIM** at `alpha-E-candidate-shortlist.md:601-614` (owner-paths block: "append **29 row-keyed REDRESS entries** (one entry per reverted row, naming the row key + the validation §reference), partitioned as 5 W14 row keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key") and at line 656-658 (falsifiability gate: "`skinny/REDRESS.md` carries 29 new row-keyed entries (5 W14 row keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key)"). SK-V12 W1b (`declaration_values`) cites `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` per line 614. Matches the V1 fold text verbatim. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table (C-5 row falsifiability gate) | ACCEPT | V2 C-5 row at line 87 reads "`skinny/REDRESS.md` carries 29 new row-keyed entries" — consistent with §7 expansion; no ambiguity. |
| `alpha-E-candidate-shortlist.md` | §10 cap discipline (E-2 fold) | ACCEPT | V2 cap table at lines 741-745 correctly defaults C-1/C-2/C-3/C-5 to 30-min redress; only C-4 inherits 45-min per-CSP-shape. CH3-orthogonal but verified — no REDRESS-redress-cap regression. |

## §2 — Critical findings

### V1 fold verification (FOLD-LANDED)

#### V1 REVISE-1 (F-10) — W10.3 nested_layout round-trip-rule trigger

**Status: FOLD-LANDED (verbatim, in both target artefacts).**

SYNTHESIS §0.4 P-1 (`SYNTHESIS.md:113-120`) now carries the round-trip-rule trigger paragraph the V1 disposition prescribed:

> SK-V14 generated files post-PRUNE must round-trip through `cargo xtask regen-css` (R4) — hand-patching is forbidden per `[clean-regen-discipline]`. Per α-C §4, W10.3 nested_layout (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence; any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the same trigger.

HANDOFF §7 (`HANDOFF.md:225-229`) carries the matching refusal-condition bullet:

> - reopens W10.3 nested_layout a second time within a tranche without user re-pin + intrinsic-block evidence; equivalently, admits any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× without the same round-trip-rule trigger (per `SYNTHESIS.md §0.4 P-1` + α-C §4);

Both texts match the V1 prescription at `V1/CH3.md §3 Fold F-1`. The round-trip-trigger pre-condition no longer lives only in α-C §4 — S-P3 authoring SPEC.md from SYNTHESIS + HANDOFF will inherit it directly. **V1 REVISE-1 is closed.**

#### V1 REVISE-2 (E-10) — α-E §7 C-5 REDRESS scribe contract count disambiguation

**Status: FOLD-LANDED (verbatim, in both target locations within α-E).**

α-E §7 owner-paths (`alpha-E-candidate-shortlist.md:601-614`):

> - `skinny/REDRESS.md` — append **29 row-keyed REDRESS entries** (one entry per reverted row, naming the row key + the validation §reference), partitioned as 5 W14 row keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key:
>   - W14.1 (numbers) cites `v2-json-validation.md §1 W14.1`.
>   - W14.2 (citm_catalog) cites `v2-json-validation.md §1 W14.2`.
>   - W14.3 (canada) cites `v2-json-validation.md §1 W14.3`.
>   - W14.4 (marine_ik) cites `v2-json-validation.md §1 W14.4`.
>   - W14.5 (mesh) cites `v2-json-validation.md §1 W14.5`.
>   - 23 SK-V13 CSS row keys cite `v1-css-l4-validation.md §§1-6` as appropriate per row (declaration_values + 22 others across the sk-v13 CSS admit set).
>   - 1 SK-V12 W1b row key (`declaration_values` cross-tranche stability revert) cites `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` and original REDRESS-123.

α-E §7 falsifiability gate (`alpha-E-candidate-shortlist.md:656-658`):

> - `skinny/REDRESS.md` carries 29 new row-keyed entries (5 W14 row keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key) with validation §refs per the owner-paths block above.

The SK-V12 W1b citation explicitly names `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` — the audit-trail surface the dispatch context §CH-3 prescribed. The dispatch context's "5 parse_only + 24 CSS = 29 REDRESS entries minimum" count is satisfied under the row-keyed reading (29 = 5 W14 + 23 SK-V13 CSS + 1 SK-V12 W1b). C-5's SYNTHESIS §3 row (`SYNTHESIS.md:275`) inherits the disambiguation: "`skinny/REDRESS.md` carries 29 new row-keyed entries." **V1 REVISE-2 is closed.**

### FRESH-FINDING — HANDOFF §7 carry-over-guard count desync against V2 F-1 reconciliation

**Source:**
- `HANDOFF.md:192-194` — the audit-falsified carry-over guard reads "inherits any of the audit-falsified admit rows (**25 CSS + 5 parse_only + 4 direct + 7 typed = 41 rows**) as carry-over without fresh material differential under rebound comparator".
- `HANDOFF.md:79-89` — the V2 F-1 fold reconciles direct + typed admits to "dispatch §1 cites 4 + 7; α-A / α-D measure 6 + 11 under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger ... PRUNE-1's ledger revert binds the wider 6+11 population (direct +2: marine_ik, instruments; typed +4: random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 plus update_center W15.1 adjusted), not the narrower 4+7 the dispatch summarises."
- `SYNTHESIS.md:200-209` — mirror reconciliation; "PRUNE-1 binds the wider 6+11 population so the revert covers every comparator-misbinding row, not just the dispatch-narrowed 11."

**Defect:** The CH6-binding F-1 fold correctly broadened SYNTHESIS §1.2 / §1.3 + HANDOFF §3 to the wider 6+11 population, but HANDOFF §7's carry-over guard at line 193 retained the V1-era narrower "4 direct + 7 typed = 41 rows" total. The two HANDOFF sections now disagree about scope:

- §3 honest-baseline summary: 25 CSS + 5 parse_only + 6 direct + 11 typed = **47 audit-falsified rows** (the binding scope).
- §7 refusal conditions: 25 CSS + 5 parse_only + 4 direct + 7 typed = **41 audit-falsified rows** (the dispatch-narrowed scope).

The 6-row delta is `direct +2: marine_ik, instruments` and `typed +4: random, instruments, numbers, unicode_basic` (the rows F-1 explicitly named as needing PRUNE-1 revert). A downstream S-P3 wave reading HANDOFF §7 alone — which is the load-bearing refusal-conditions list S-P3 must consult before admitting any row — could conclude these 6 rows are NOT under the carry-over-guard and admit them under the rebound comparator without binding the F-1 prescription. The §7 refusal list is the canonical pre-admit gate; a 6-row scope gap is a regression vector the V1 lens did not see (V1's narrower numbers matched HANDOFF §7's narrower numbers; V2 broadened §3 but not §7).

**Severity:** REVISE (not REJECT). No row is silently regressed yet — both SYNTHESIS §1.3 and HANDOFF §3 carry the wider scope and SYNTHESIS §3 C-5 row binds the PRUNE-1 + PRUNE-2 revert at the wider scope through E-10's 29-row scribe contract (which counts JSON parse_only at 5, not the 6+11 direct/typed populations, so the direct/typed broadening is parallel and orthogonal to the C-5 scribe count). The defect is that the §7 line-193 enumeration tells a future S-P3 wave a smaller-than-binding carry-over set.

**Fold (V3 prescription):** Edit `HANDOFF.md:192-194` to broaden the count and enumerate the wider population. Suggested text:

> - inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows** under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md §1.3` reconciliation) as carry-over without fresh material differential under rebound comparator;

This single-line edit closes the desync without altering the §7 structure. The SYNTHESIS §1.3 reconciliation paragraph already names the +6 rows (direct +2 marine_ik + instruments; typed +4 random + instruments + numbers + unicode_basic via W13.1-.4 + update_center W15.1), so HANDOFF §7 needs only the count update; the enumeration sits one citation away.

### Non-findings worth recording (V2-confirmed)

- **REDRESS-119/120 LIFTED handling stable.** V2 unchanged at α-C §0, SYNTHESIS §5 (lines 355-356), HANDOFF authority list — all carry the HISTORY-only + fresh-material-differential constraint.
- **No candidate re-uses `sonic_rs::from_slice::<Value>` as comparator.** P-2 pre-block carries forward; C-2 explicitly rebinds three plane-correct strict comparators; HANDOFF §7 (line 189-190) refuses any plan that admits a row under the misbound API.
- **No candidate silently reverses a prior REJECT.** Items 149-153 (W13.5-.9), 142+159 (W11.2/W11.4), and the W8/W9 SCAFFOLD demotion all carry forward as PRE-BLOCK in V2.
- **C-5 owner paths correctly fold REDRESS scribe at 29 row-keyed entries.** E-10 verified; SYNTHESIS §3 row at line 275 inherits the count.
- **W10.3 nested_layout round-trip-rule trigger now in three artefacts.** α-C §4 (original), SYNTHESIS §0.4 P-1 (F-10), HANDOFF §7 (F-10) — triple redundancy across the load-bearing chain.
- **The dual-tree round-trip gate at SYNTHESIS §3 C-3 (line 273)** is CH7-territory but CH3-relevant: it bars the recurrence vector (hand-written CSS with fake `@generated` header) the round-trip-rule trigger was prescribed for. F-17 + E-1 / E-14 strengthen the P-1 + W10.3 chain through the C-3 candidate gate.
- **The α-E §7 SK-V12 W1b carry-over now explicitly cites REDRESS-123** at line 614 ("`v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` and original REDRESS-123"). The V1 dispatch context §CH-3 spot-check (REDRESS-123-127 for W1b at `REDRESS.md:3636-3825`) is satisfied; α-E's enumeration is row-precise.

## §3 — Recommended folds for V3

One surgical doc edit, within the α-F HANDOFF scope. No new candidates; no new R-targets; no architectural changes.

### Fold F-V3-1 (re-dispatch α-F HANDOFF author)

Edit `restart/skinny/tranches/sk-v14/HANDOFF.md:192-194` to broaden the audit-falsified carry-over count from 41 to 47 (mirroring the F-1 reconciliation in §3 + the SYNTHESIS §1.3 broadening):

> - inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows** under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md §1.3` reconciliation) as carry-over without fresh material differential under rebound comparator;

This is editorial precision — the V2 F-1 fold correctly broadened §3 honest-baseline and SYNTHESIS §1.2 / §1.3 / §3 but missed the §7 line-193 mirror. The §7 refusal list is the canonical pre-admit gate S-P3 will consult; the desync must close before G-Alpha.

### Convergence note

V2 CH3 ACCEPT-rate of 96.8 % already clears the §3Z 95 % single-cycle floor — V2 converges on this lens. V3 with F-V3-1 landed forecasts 100 % CH3 ACCEPT. The fresh finding is a downstream residue of the V2 CH6 F-1 fold (which itself remediated a V1 BINDING REJECT) — the broadening propagated to §3 but stopped at §7. Single-line fix; no escalation flag.

No CH3-level escalation flag.
