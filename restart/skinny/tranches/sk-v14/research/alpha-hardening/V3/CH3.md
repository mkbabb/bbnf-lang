# CH3 REGRESSION (REDRESS) — Pass Alpha V3 Disposition

Date: 2026-05-23. Lens: CH3 per `ORCHESTRATOR.md §3W` row CH3, the dispatch-context lens scope at `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:114-127`, and the V3 confirming-pass cycle bind that micro-folded F-V3-α-F-1 (CH3 V2 F-V3-1) under commit `5e2ae78b4`. Scope: verify the V2 fresh-finding fold landed verbatim, re-confirm the V2 96.8 % baseline holds across the remaining CH3 § dispositions, and run a fresh scan over the V3 artefacts (V2 baseline + commit `5e2ae78b4` micro-fold deltas) for any regression the V2 lens did not see.

## §0 — Disposition summary

- ACCEPT-rate: 31 / 31 per-§ rows = **100 %** (above the §3Z 95 % convergence floor; second consecutive ≥ 95 % cycle closes the two-consecutive-cycle convergence chain per `ORCHESTRATOR.md §3Z`).
- REJECT count: 0.
- REVISE count: 0.
- Critical findings: 0 — V2's lone fresh finding (HANDOFF §7 carry-over guard desync) is folded verbatim; the V2 baseline 30 / 31 ACCEPT dispositions stand unchanged; the fresh-finding sweep surfaced no new REDRESS-recurrence regression introduced by the V3 micro-fold.

Overall: V3 confirms the CH3 lens. F-V3-1 fold (commit `5e2ae78b4` packet F-V3-α-F-1) landed verbatim at `HANDOFF.md:192-197` — the §7 audit-falsified carry-over guard now reads "25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows** under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md §1.3` reconciliation", matching the V2 §3 Fold F-V3-1 prescription character-for-character. The desync between HANDOFF §3 (47-row wider scope) and HANDOFF §7 (formerly 41-row narrower scope) is closed; both sections now bind the same audit-falsified carry-over set, and the §7 refusal list will correctly bar S-P3 from admitting any of the six F-1-named extension rows (direct +2: marine_ik, instruments; typed +4: random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + update_center W15.1 adjusted) without fresh-material differential under rebound comparator. The other CH3 V2 dispositions (REVISE-1 F-10 W10.3 round-trip-rule trigger; REVISE-2 E-10 C-5 REDRESS-scribe count disambiguation) STAND landed per V2 verification — no regression. The V3 micro-fold's other packet (F-V3-α-E-1 against α-E:362-365) is CH2 territory and CH3-orthogonal; no REDRESS surface touched.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `alpha-C-redress-digest.md` | §0 Binding Interpretation | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched (file STANDS per commit `5e2ae78b4` message); REDRESS-119/120 HISTORY clause echoes addendum verbatim. |
| `alpha-C-redress-digest.md` | §1 CSS L4 admits (131-135) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; SK-V12 W1b carry-over annotation resolves through α-A + E-10. |
| `alpha-C-redress-digest.md` | §1 Decision-engine fold (136-140) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §1 JSON direct admits (141-143, 159) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §1 JSON typed admits (145-148, 160) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §1 JSON typed measured rejects + correctness reject (149-153) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §1 JSON parse-only admits (154-158) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §1 Disposition summary table | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §2 P-1 through P-7 | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; CH5 §2 P-7 buffer-ownership triple-check remains the CH3-orthogonal strengthener. |
| `alpha-C-redress-digest.md` | §3 Pattern-level summary table | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `alpha-C-redress-digest.md` | §4 Reopen obligations | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. The round-trip-rule trigger on W10.3 nested_layout (the V1 REVISE-1 source) remains triple-carried (α-C §4 + SYNTHESIS §0.4 P-1 + HANDOFF §7) per F-10. |
| `alpha-C-redress-digest.md` | §5 Closing posture | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; goalset enumeration "0 ADMITTED per plane × 4 surfaces" still binds at line 35-37 + 56-65; F-1 reconciliation paragraph at lines 73-89 carries the wider 6+11 population. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R1-R10) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. F-11 `regen-{grammar}` family annotation at R4 (line 96) does not alter R3/R7/R8's REDRESS routing. |
| `SYNTHESIS.md` | §0.4 P-1 through P-7 pre-blocks | ACCEPT (V2-STAND, FOLD-LANDED carried) | V1 REVISE-1 (F-10) STILL FOLD-LANDED at `SYNTHESIS.md:115-120`. V3 untouched. The W10.3 nested_layout round-trip-rule trigger paragraph remains verbatim ("Per α-C §4, W10.3 nested_layout (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence; any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the same trigger."). |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; contracted deferral per PASS-ALPHA §4.4. |
| `SYNTHESIS.md` | §1 Corrected diagnosis (§1.1 - §1.3) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. F-1 reconciliation paragraph at §1.3 lines 200-209 (4+7 dispatch vs 6+11 wider ledger) STANDS; PRUNE-1 binds the wider population per `SYNTHESIS.md:204` and row enumeration sits at `SYNTHESIS.md:211` (§1.3 header). |
| `SYNTHESIS.md` | §2 Telemetry binding (new audit_overlay_verdict + track2_entry_point columns) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `SYNTHESIS.md` | §3 Candidate shortlist (C-1 to C-5) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. C-5 row at line 275 still binds R3 PRUNE-1 + PRUNE-2 at the 29 row-keyed entries; C-3 row at line 273 still binds the round-trip dual-tree gate. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `SYNTHESIS.md` | §5 Pre-blocked + unblocked routes | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; REDRESS-119/120 LIFTED clause at lines 355-356 still binds HISTORY-only. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. F-1 reconciliation paragraph at lines 79-89 carries the wider 6+11 population; row-by-row binding for PRUNE-1 revert holds. |
| `HANDOFF.md` | §6 Next-move | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. |
| `HANDOFF.md` | §7 Refusal conditions | **ACCEPT (FOLD-LANDED — V3 F-V3-α-F-1)** | V1 REVISE-1 (F-10) STILL landed at lines 228-232 (W10.3 round-trip-trigger refusal bullet); V2 fresh-finding (F-V3-1) fold **FOLD-LANDED VERBATIM** at lines 192-197. Diff vs V2 baseline (`850a29256` → `5e2ae78b4`) confined to lines 192-197: "inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows** under the broader `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md §1.3` reconciliation) as carry-over without fresh material differential under rebound comparator;" — character-identical to the V2 §3 Fold F-V3-1 prescription. The §3 ↔ §7 scope desync is closed; the six F-1-named extension rows (direct +2: marine_ik, instruments; typed +4: random, instruments, numbers, unicode_basic via W13.1-.4 + update_center W15.1) are now bound by the §7 refusal gate. |
| `alpha-A-results-extraction.md` | §"ROLLING-SOTA-DELTA reconciliation" (V2 A-1 fold; lines 129-131 + 161-169) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched (alpha-A STANDS per commit `5e2ae78b4` message). |
| `alpha-E-candidate-shortlist.md` | §7 C-5 owner paths + REDRESS scribe contract | ACCEPT (V2-STAND, FOLD-LANDED carried) | V1 REVISE-2 (E-10) STILL FOLD-LANDED at `alpha-E-candidate-shortlist.md:616` (owner-paths: "append **29 row-keyed REDRESS entries**" partitioned as 5 W14 + 23 SK-V13 CSS + 1 SK-V12 W1b) and at line 671 (falsifiability gate: "`skinny/REDRESS.md` carries 29 new row-keyed entries"). V3 micro-fold F-V3-α-E-1 touched only §C-1 owner paths (CH2 territory at lines 362-365) — no REDRESS-scribe contract modification. SK-V12 W1b citation `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` + REDRESS-123 still present. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table (C-5 row falsifiability gate) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched. C-5 row at line 87 still reads "`skinny/REDRESS.md` carries 29 new row-keyed entries"; consistent with §7. |
| `alpha-E-candidate-shortlist.md` | §10 cap discipline (E-2 fold) | ACCEPT (V2-STAND) | V2 ACCEPT; V3 untouched; cap table still defaults C-1/C-2/C-3/C-5 to 30-min redress. |

## §2 — Critical findings

### V2 fresh-finding F-V3-1 fold verification (FOLD-LANDED — closes the V2 → V3 cycle)

**Status: FOLD-LANDED (verbatim).**

V3 micro-fold packet F-V3-α-F-1 (per commit `5e2ae78b4` body) implements the V2 CH3 §3 Fold F-V3-1 prescription character-for-character. The git diff between V2 consolidated baseline (`850a29256`) and V3 micro-fold (`5e2ae78b4`) confined to `HANDOFF.md:192-197` reads:

```
 - inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only
-  + 4 direct + 7 typed = 41 rows) as carry-over without fresh material
+  + 6 direct + 11 typed = **47 rows** under the broader
+  `ROLLING-SOTA-DELTA.md:13-93` ledger; the V1 dispatch §1 narrower
+  bind of 4 direct + 7 typed is a strict subset per `SYNTHESIS.md
+  §1.3` reconciliation) as carry-over without fresh material
   differential under rebound comparator;
```

The fold text matches the V2 prescription at `V2/CH3.md:127-129` character-for-character (modulo the markdown-list line-break wrap). The §1.3 citation lands the reader at `SYNTHESIS.md:211` (the "1.3 Honest rolling delta" header), and the SYNTHESIS §1.3 body at lines 200-209 enumerates the six extension rows (direct +2: marine_ik, instruments; typed +4: random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + update_center W15.1 adjusted) so a downstream S-P3 wave consulting HANDOFF §7 has the §1.3 row enumeration one cite away.

**The §3 ↔ §7 scope desync is fully closed.** HANDOFF §3 (lines 79-89, carrying the wider 6+11 population per F-1) and HANDOFF §7 (lines 192-197, V3 broadened to 47 rows) now bind the same audit-falsified carry-over set. S-P3 reading the load-bearing §7 refusal list will correctly REVISE any plan that admits one of the six extension rows without fresh-material differential under rebound comparator. **V2 fresh finding is closed.**

### V1 REVISE-1 (F-10) — W10.3 nested_layout round-trip-rule trigger

**Status: FOLD-LANDED (still carried in V3 artefacts; no regression).**

SYNTHESIS §0.4 P-1 (`SYNTHESIS.md:115-120`) and HANDOFF §7 (`HANDOFF.md:228-232`) both retain the round-trip-rule trigger text verbatim per the V1 fold prescription. The HANDOFF §7 bullet reads:

> - reopens W10.3 nested_layout a second time within a tranche without user re-pin + intrinsic-block evidence; equivalently, admits any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× without the same round-trip-rule trigger (per `SYNTHESIS.md §0.4 P-1` + α-C §4);

V3 untouched. Triple-redundancy across α-C §4 (original carrier), SYNTHESIS §0.4 P-1 (F-10), HANDOFF §7 (F-10) holds. **V1 REVISE-1 stays closed.**

### V1 REVISE-2 (E-10) — α-E §7 C-5 REDRESS scribe contract count

**Status: FOLD-LANDED (still carried in V3 artefacts; no regression).**

α-E §7 owner-paths (`alpha-E-candidate-shortlist.md:616`) still binds the "29 row-keyed REDRESS entries" partition (5 W14 row keys + 23 SK-V13 CSS row keys + 1 SK-V12 W1b row key), with the SK-V12 W1b citation explicitly naming `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` and REDRESS-123. The §7 falsifiability gate at line 671 mirrors. SYNTHESIS §3 C-5 row at line 275 inherits. V3 micro-fold F-V3-α-E-1 touched only the §C-1 owner-paths shell-loop literal at α-E:362-365 (CH2 territory — grammar enumeration via cargo metadata); the §7 C-5 REDRESS-scribe contract is untouched. **V1 REVISE-2 stays closed.**

### Fresh-finding scan (V3 artefacts)

V3 ALL ARTEFACTS scanned end-to-end for new REDRESS-recurrence regressions introduced by commit `5e2ae78b4`. Six non-findings worth recording:

- **F-V3-α-F-1 edit does not over-reach.** The diff is confined to the §7 carry-over guard's count and citation; no adjacent refusal bullet, no §3 mirror, no §1.3 row enumeration was altered. The fold is surgical per V2 prescription.
- **F-V3-α-E-1 edit does not touch CH3 surface.** The CH2 fold replaces a shell-loop grammar enumeration literal at α-E:362-365 (the C-1 forward-invariant gate) with a `cargo metadata` derivation. No REDRESS scribe contract, no candidate gate, no row enumeration touched.
- **REDRESS-119/120 LIFTED handling stable.** α-C §0, SYNTHESIS §5 (lines 355-356), HANDOFF authority list all carry the HISTORY-only + fresh-material-differential constraint unchanged.
- **No new candidate re-uses `sonic_rs::from_slice::<Value>`.** P-2 pre-block carries forward; C-2 binds three plane-correct strict comparators; HANDOFF §7 (lines 189-190) refuses any plan that admits a row under the misbound API. The V3 broadened §7 line 193 inherits this gate.
- **No silent REJECT reversal.** The W13.5-.9 measured rejects, W11.2/W11.4 correctness rejects, and the W8/W9 SCAFFOLD demotions all carry forward as PRE-BLOCK in V3.
- **The HANDOFF §7 ↔ §3 ↔ SYNTHESIS §1.3 cite-chain is now consistent.** §3 (47 rows binding scope per F-1), §7 (47 rows broadened per F-V3-α-F-1, citing §1.3), §1.3 (rolling-delta with row enumeration). The three load-bearing sections agree on scope; S-P3 cannot accidentally admit a §1.3-named extension row through §7's prior narrower scope.

### Non-finding: REDRESS.md sample (grep only)

Per dispatch context CH-3 spot-check protocol (`grep -n 'REDRESS' skinny/REDRESS.md | head -50`), the REDRESS.md ledger is at 5041 lines / 107 top-level sections; the W14.1–W14.5 admission entries are present at lines 4765/4800/4836/4872/4915, and W15.1 UpdateCenter at line 4992 — confirming the wider 6+11 population's W13.x + W14.x + W15.1 sources are tracked surface (so the F-V3-α-F-1 broadened guard is a binding gate, not an empty one). REDRESS-123 is cited at line 3640 per the SK-V12 W1b carry-over chain α-E §7 binds. No new REDRESS entries are needed for V3 — the C-5 PRUNE wave is the scribe trigger and lands at S-P3 sequence, not before.

## §3 — Recommended folds for V4 (if any)

**None.** V3 confirming pass converges the CH3 lens at 100 % ACCEPT. The V2 fresh finding is closed; the V2 baseline 30 / 31 ACCEPT dispositions stand unchanged; the fresh-finding sweep over the V3 micro-fold surfaced no new REDRESS-recurrence regression.

### Convergence note

V3 CH3 ACCEPT-rate of 100 % is the second consecutive ≥ 95 % cycle for the CH3 lens (V2 = 96.8 %; V3 = 100 %). Per `ORCHESTRATOR.md §3Z` two-consecutive-cycle convergence rule, the CH3 lens locks at V3. The SK-V14 contract on CH3-bound REDRESS surface (round-trip-rule trigger; 47-row audit-falsified carry-over guard; 29 row-keyed REDRESS scribe contract; REDRESS-119/120 HISTORY-only with fresh-material-differential constraint) is binding at G-Alpha sign-off.

No CH3-level escalation flag. No CH3-level redress-revert recommendation. The CH3 surface is ready for S-P3 SPEC.md authoring inheritance.
