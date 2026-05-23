# CH3 REGRESSION (REDRESS) — Pass Alpha V1 Disposition

Date: 2026-05-22. Lens: CH3 per `ORCHESTRATOR.md §3W` row CH3 and the dispatch context's §CH-3 at `restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:114-127`. Scope: every artefact in §1 of the dispatch context, audited solely for REDRESS-recurrence vectors, pre-block coverage, silent reopen of audit-closed routes, and REDRESS-119/120 LIFTED handling.

## §0 — Disposition summary

- ACCEPT-rate: 28 / 30 per-§ rows = 93.3% (just below the §3Z 95% convergence floor).
- REJECT count: 0.
- REVISE count: 2.
- ACCEPT-with-note count: 1 (no functional defect; clarification only).
- Critical findings: 0 reopens of audit-closed routes; 0 silent regressions; 0 missing REDRESS scribes within the candidate's owner contract. Two REVISEs flag editorial-precision gaps that fall well short of REJECT — they harden the campaign-wide REDRESS scribe contract for SK-V14's PRUNE waves.

Overall: the Pass Alpha α-cycle has accurately classified every SK-V13 REDRESS entry against the audit pack, lifted P-1 through P-7 verbatim into SYNTHESIS §0.4, refused to re-open any route the audit closed, and threaded the REDRESS-119/120 HISTORY constraint through both SYNTHESIS §5 and HANDOFF §7. The two REVISEs ask α-C and α-E to add the SK-V12 W1b REDRESS-123 citation explicitly to C-5's per-row revert ledger, and ask SYNTHESIS §0.4 to lift α-C's "round-trip rule trigger" flag on W10.3 nested_layout (REDRESS 135) since that flag is currently load-bearing only in α-C §1 and does not echo into SYNTHESIS / HANDOFF.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `alpha-C-redress-digest.md` | §0 Binding Interpretation | ACCEPT | Honest binding of audit-wins-over-REDRESS rule; cites `b24232776` as the audit-pack pin. REDRESS-119/120 HISTORY clause echoes the addendum at `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:60-63` verbatim. |
| `alpha-C-redress-digest.md` | §1 CSS L4 admits (131-135) | ACCEPT | Items 131-135 verified at `skinny/REDRESS.md:3881-4077`; covered features per row match the wave's "covers N features" claim; ADMIT-FAKE verdicts cite `v1 §5` rows 2-24 correctly; the 24-row collapse + SK-V12 W1b carry is internally consistent. |
| `alpha-C-redress-digest.md` | §1 Decision-engine fold (136-140) | ACCEPT | LOAD-BEARING / SCAFFOLD-ONLY split matches v4 §1-6; W8+W9 demotion to SCAFFOLD is sourced; W9's +3.88 Mbps row-strengthening claim is correctly downgraded because the underlying W3 row is P-1 fake. |
| `alpha-C-redress-digest.md` | §1 JSON direct admits (141-143, 159) | ACCEPT | Verified item 141 at `REDRESS.md:4290-4346` (numbers W11.1 PASS-ADMIT @ 13,875 / 12,918) and item 159 at `REDRESS.md:4960-4990` (W11.4 REJECT); v6 comparator-misbinding reclassification routed through R7 correctly; HONEST measured reject 142+159 carry PRE-BLOCK on same dispatch-envelope / byte-fetch shape. |
| `alpha-C-redress-digest.md` | §1 JSON typed admits (145-148, 160) | ACCEPT | All five flagged REOPEN-AUDIT under R7 with the v6 §7 caveat that the typed-plane comparator is closer-to-correct-but-not-yet-verified; W15.1 disposition matches `REDRESS.md:4992-5041`. |
| `alpha-C-redress-digest.md` | §1 JSON typed measured rejects + correctness reject (149-153) | ACCEPT | All five carry HONEST PRE-BLOCK on the same shape; the round-trip rule for second-in-tranche reopen carries forward correctly under v5's "honest patterns left clean" disposition. |
| `alpha-C-redress-digest.md` | §1 JSON parse-only admits (154-158) | ACCEPT | Items 154-158 verified at `REDRESS.md:4765-4958`; W14.1 source delta literally matches v2 §1 (gate.rs / report.rs / lock14_baseline.rs + main.rs only) per the REDRESS bullet "It supplies the missing gate-consumed strict DOM output-plane contract... does not change parser runtime". The DEMOTE-AUDIT, REVERT per R3 PRUNE-1 disposition is binding. |
| `alpha-C-redress-digest.md` | §1 Disposition summary table | ACCEPT | Aggregate 30 entries (3 SURVIVE + 5 DELETE + 5 REVERT + 3 SCAFFOLD + 7 REOPEN + 7 HONEST-REJECT) = 30 = 160-131+1. Arithmetic holds. |
| `alpha-C-redress-digest.md` | §2 P-1 through P-7 | ACCEPT | Each pre-block cites its validation §, names its production technique, and binds an R-target lift route. Falsifiability commands are concrete (`grep -l '@generated'`, `grep -n 'sonic_rs::from_slice::<sonic_rs::Value>'`, byte-count gate, parser-touch diff, runtime-divergence row, `find ... -mindepth 1 -maxdepth 1`, per-iter equality column). |
| `alpha-C-redress-digest.md` | §3 Pattern-level summary table | ACCEPT | Each pre-block's "Lifted by" R-target column lines up with the SYNTHESIS §0.4 + α-E §2 candidate matrix. |
| `alpha-C-redress-digest.md` | §4 Reopen obligations | ACCEPT | PRUNE-first ordering binds. R1+R2 comparator + per-iter equality binds before any new admit. The round-trip rule trigger for W10.3 nested_layout (124× anomaly) is correctly flagged. |
| `alpha-C-redress-digest.md` | §5 Closing posture | ACCEPT | The three-step posture (PRUNE → REBIND → REBUILD) and the 30-violation Lock 14 count match the audit pack at the level α-C is responsible for. |
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | The "implementation-limited misses are reopens, not closes" clause locks the REDRESS recurrence vector through the close gate. |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | 0 ADMITTED per plane × 4 surfaces; the obligation language ("all 17 reopen") forbids any silent regression. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R1-R10) | ACCEPT | R3 PRUNE-1 explicitly names W14.1-W14.5 revert + 24 CSS row revert; R7 routes the 4 direct + 7 typed REOPEN-AUDIT entries; R8 binds the distinct parse_only path before re-admit. |
| `SYNTHESIS.md` | §0.4 P-1 through P-7 pre-blocks | REVISE | Inherits α-C §2 verbatim and is binding, BUT does not lift α-C §1's per-entry "round-trip rule trigger" flag on REDRESS 135 (W10.3 nested_layout, 124× anomaly). α-C §1 explicitly calls this out as "flag as round-trip rule trigger under future reopen — wave failed in spirit even if header read ADMIT"; SYNTHESIS §0.4 should add a parallel per-row round-trip trigger list (currently round-trip is bound only at P-1 generic). Fold: SYNTHESIS §0.4 P-1 paragraph adds a closing sentence naming W10.3 nested_layout (and any future CSS feature whose claimed Mbps exceeds lightningcss by ≥ 50×) as a permanent round-trip trigger requiring user re-pin on any second-in-tranche reopen. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | The deferral to S-P3 is contracted per PASS-ALPHA §4.4 — not a paper-close. |
| `SYNTHESIS.md` | §1 Corrected diagnosis (§1.1 - §1.3) | ACCEPT | Falsified rows enumerated; honest rolling delta matches α-A's audit-corrected baseline; "campaign at zero on numbers; non-zero on architecture" is the right framing. |
| `SYNTHESIS.md` | §2 Telemetry binding (new audit_overlay_verdict column) | ACCEPT | The new column gate-enforces per-row AUDIT-FALSIFIED / SUSTAINED / PENDING enum; "any row currently AUDIT-FALSIFIED requires fresh material differential evidence to re-admit" (§4 last bullet) is the load-bearing prophylactic against silent re-admit. |
| `SYNTHESIS.md` | §3 Candidate shortlist (C-1 to C-5) | ACCEPT | All five candidates carry pre-block lift mappings; C-5's revert scope correctly enumerates "PRUNE-1: revert W14.1-W14.5 ... REDRESS per row cites `v2 §1-4`. PRUNE-2: delete 7 CSS hand-written template files + their `include_str!`'d `generated.rs`; revert 24 CSS L4 admitted rows; REDRESS per row cites `v1 §1-6`." Citation routing is precise. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT | "no SPEC clause may inherit weaker scoping labels..." closes the deferral loophole; "the §1 audit overlay column is gate-enforced per row" closes the silent-regression loophole; the G-SIMD-GRAMMAR-POLICY clause covers the SIMD substrate-union route that REDRESS-119/120 history could otherwise re-open under cover. |
| `SYNTHESIS.md` | §5 Pre-blocked + unblocked routes | ACCEPT | Explicit prohibition on "closing a JSON row through REDRESS-119 / REDRESS-120 history without fresh SK-V14 evidence (both LIFTED per addendum; HISTORY only)" at lines 298-299. Pre-block list folds in P-1 through P-7 by reference. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | No silent reopen; indefatigability clause carries. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | SK-V13 audit reversal binding; SK-V14 opens prune-then-rebuild. |
| `HANDOFF.md` | §3 Honest baseline summary (3 sub-sections) | ACCEPT | "Does not survive" enumerates 25 CSS + 5 parse_only + 11 direct+typed + W8+W9 cleanly; the recurrence vector (8 per-grammar provider modules) is named. |
| `HANDOFF.md` | §6 Next-move | ACCEPT | CH3 review scoped explicitly ("CH3 reviews the pre-block list against REDRESS") confirms the orchestrator placed this lens correctly. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | Lines 200-204 enumerate "introduces any of patterns P-1 through P-7 (`SYNTHESIS.md §0.4`)" verbatim; the audit-falsified admit row count (41) is named; all seven recurrence vectors covered. |
| `alpha-A-results-extraction.md` | §"Routing into α-C/α-D/α-E/α-F" (lines 306-329) | ACCEPT-WITH-NOTE | α-A routes "REDRESS-123-127 for W1b" CSS reverts to PRUNE-2 — verified at `REDRESS.md:3636-3825`, items 123-127 are SK-V12 W1b-1/2a/2b/W4/W5 entries. Note: α-C's §1 covers items 131-160 (SK-V13 cycle only, by explicit dispatch scope at line 14-15). α-C and α-A are mutually consistent; the SK-V12 W1b row revert is α-A's authority and α-C carries it forward as one sentence ("does not appear as a SK-V13 REDRESS item but is reverted in the same DELETE pass"). No defect; clarification only. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 owner paths + REDRESS scribe contract | REVISE | C-5's REDRESS scribe contract enumerates 5 W14 entries (lines 498-502) + "24 CSS rows cite `v1-css-l4-validation.md §§1-6`" (lines 503-506). The 24-row scribe contract conflates 23 SK-V13 + 1 SK-V12 W1b without naming REDRESS-123 explicitly as the SK-V12 carry-over revert source. C-5 §"Falsifiability gate" claims "29 new entries (5 W14 + 24 CSS)" but the 24 CSS row reverts only span 5 SK-V13 wave REDRESS entries (131-135) — the per-ROW REDRESS scribe contract collapses into 5 wave REDRESS entries + 1 SK-V12 carry-over (REDRESS-123) for a total of 6 REDRESS entries, not 29. If the C-5 contract intends 29 NEW REDRESS entries (one per reverted row), the scribe contract should explicitly state "append 29 row-keyed REDRESS entries" and enumerate the row keys; if the contract intends 6 wave-keyed REDRESS entries, it should restate the count. Fold: α-E §7 clarifies "29 row-keyed REDRESS entries (5 W14 row keys + 23 SK-V13 CSS feature row keys + 1 SK-V12 W1b CSS feature row key)" OR "6 wave-keyed REDRESS entries (131-135 + 123)" and align the C-5 falsifiability gate line accordingly. The dispatch context §CH-3 explicitly asks "verify the scope: 5 parse_only + 24 CSS = 29 REDRESS entries minimum" — that count holds ONLY under the row-keyed interpretation, which is the more conservative and audit-trail-friendly choice. |

## §2 — Critical findings

### REVISE-1 — SYNTHESIS §0.4 P-1 paragraph does not lift α-C's per-entry round-trip rule trigger flag on W10.3 nested_layout

**Source:** `alpha-C-redress-digest.md:56` ("flag as round-trip rule trigger under future reopen — wave failed in spirit even if header read ADMIT"); `alpha-C-redress-digest.md:395-397` ("**Round-trip rule.** W10.3 nested_layout (124× anomaly) triggers the round-trip rule preemptively under v1 §1 Claim 5. Any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence.").

**Defect:** SYNTHESIS §0.4 P-1 (lines 96-102) names the regen-css pipeline as the generic round-trip enforcer but does not echo α-C §4's specific round-trip-trigger flag on W10.3 nested_layout. A future SK-V14 wave dispatching a "real" CSS nested_layout admit attempt could read SYNTHESIS / HANDOFF and miss the round-trip-trigger pre-condition that α-C carried only in its own digest.

**Severity:** REVISE (not REJECT). The pre-condition IS in α-C, which IS bound by SYNTHESIS §"Authority" at line 17 ("audit-overfit/validation/v{1..6}-*.md") and α-C's §"Authority read" — but α-C is not in SYNTHESIS's authority chain. The round-trip trigger thus lives only in α-C's digest and may be lost when S-P3 authors SPEC.md from SYNTHESIS + HANDOFF.

**Fold:** SYNTHESIS §0.4 P-1 closing sentence adds: "Per α-C §4, W10.3 nested_layout (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence. Any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the same trigger." HANDOFF §7 adds a matching refusal-condition bullet for the same trigger.

### REVISE-2 — α-E §7 C-5 REDRESS scribe contract count ambiguity

**Source:** `alpha-E-candidate-shortlist.md:497-506` ("Append per-row REDRESS entries: ... 24 CSS rows cite `v1-css-l4-validation.md §§1-6` as appropriate per row (declaration_values + 23 others; the SK-V12 `declaration_values` row also reverts per `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL`)"); `alpha-E-candidate-shortlist.md:548-549` ("`skinny/REDRESS.md` carries 29 new entries (5 W14 + 24 CSS) with validation §refs").

**Defect:** The "29 new entries" count is ambiguous — it could read either as 29 row-keyed REDRESS entries (the more conservative, row-by-row audit-trail style) or as 6 wave-keyed REDRESS entries (one revert entry per SK-V13 wave REDRESS 131-135 + one for SK-V12 REDRESS-123). The α-C §1 dispositions are wave-keyed (5 wave entries for the 24 CSS rows). Per dispatch context §CH-3 the prescribed scope is "5 parse_only + 24 CSS = 29 REDRESS entries minimum" — the row-keyed reading. The two readings differ by a factor of 4.8× in scribe surface; downstream S-P3 should not be left guessing.

**Severity:** REVISE (not REJECT). No row is silently regressed by either reading. The defect is an audit-trail surface-area question, not a regression risk.

**Fold:** α-E §7 owner-paths clarifies the scribe contract as "29 row-keyed REDRESS entries" — one entry per reverted row, naming the row key + the validation §reference. This preserves the per-row REDRESS-119-style row authority that SK-V13 W11+ already uses. C-5's falsifiability gate then reads "`skinny/REDRESS.md` carries 29 new row-keyed entries" rather than the current ambiguous wording.

### Non-findings worth recording

- **REDRESS-119/120 LIFTED handling is correctly flagged** across all artefacts. α-C §0 (lines 24-29), SYNTHESIS §5 pre-block 4 (lines 298-299), and HANDOFF authority list item 4 (line 117) all carry the addendum's HISTORY-only constraint with the fresh-material-differential rebind requirement. No candidate re-opens REDRESS-119 or REDRESS-120 silently.
- **No candidate re-uses `sonic_rs::from_slice::<Value>`.** C-2 explicitly forbids the misbound API and binds three plane-correct strict comparators per R1. α-C §2 P-2 carries the pattern-level pre-block; SYNTHESIS §0.4 P-2 inherits it; HANDOFF §7 refuses any plan that "counts a row as admitted under the misbound `sonic_rs::from_slice::<Value>` comparator (P-2)" (line 169).
- **No candidate silently reverses a prior REJECT decision.** Items 149-153 (W13.5-W13.9 HONEST measured rejects) and 142+159 (W11.2 / W11.4 HONEST measured rejects) all carry forward as PRE-BLOCK on same-shape reopens. R7 re-admit waves cite "REOPEN-AUDIT" not "reverse reject" — semantically distinct and audit-correct.
- **HANDOFF §7 refusal conditions cover the recurrence-vector list** verbatim (lines 200-204 lift P-1 through P-7) and add the 41-row audit-falsified carry-over guard (line 173: "inherits any of the audit-falsified admit rows (25 CSS + 5 parse_only + 4 direct + 7 typed = 41 rows) as carry-over without fresh material differential under rebound comparator"). 41 is the right count for the row-keyed audit-falsified set; the additional 7 typed + 4 direct beyond the SK-V13 falsified set follow from v6 §7 comparator-misbinding scope.
- **The decision-engine fold's W8+W9 demotion to SCAFFOLD is correctly routed to C-4 (PRUNE-5).** α-C §1 items 139+140 + α-E §6 C-4 + SYNTHESIS §3 row C-4 all bind "no row admit cites W8/W9 without measured runtime consumption". This closes the P-5 pattern-level pre-block at the candidate level.

## §3 — Recommended folds for V2

Two surgical doc edits, both within the existing α-cycle scope. No new candidates; no new R-targets; no architectural changes.

### Fold F-1 (re-dispatch α-F SYNTHESIS author)

Edit `restart/skinny/tranches/sk-v14/SYNTHESIS.md` §0.4 P-1 paragraph (current lines 96-102) to add a closing sentence flagging the W10.3 nested_layout round-trip trigger:

> Per α-C §4, W10.3 nested_layout (124× anomaly) carries a preemptive round-trip-rule trigger: any second-in-tranche reopen of nested_layout requires user re-pin with intrinsic-block evidence. Any future CSS feature whose claimed Mbps exceeds the same-plane SOTA comparator by ≥ 50× inherits the same trigger.

Additionally edit `restart/skinny/tranches/sk-v14/HANDOFF.md` §7 (current lines 200-204) to add a matching refusal-condition bullet:

> - re-opens W10.3 nested_layout (or any CSS feature whose prior claimed Mbps exceeded the same-plane SOTA comparator by ≥ 50×) without explicit user re-pin carrying intrinsic-block evidence per the SYNTHESIS §0.4 P-1 round-trip-rule trigger.

### Fold F-2 (re-dispatch α-E candidate-shortlist author)

Edit `restart/skinny/tranches/sk-v14/research/alpha/alpha-E-candidate-shortlist.md` §7 C-5 owner-paths block (current lines 497-506) and §7 falsifiability gate (lines 548-549) to disambiguate the scribe contract to row-keyed:

> `skinny/REDRESS.md` carries 29 row-keyed entries — one entry per reverted row, naming the row key + the validation §reference. Enumerated: 5 W14 row keys (`json/{numbers,citm_catalog,canada,marine_ik,mesh}/parse_only/main`) + 23 SK-V13 CSS feature row keys (`declarations`, `css_variables`, `calc_expressions`, `var_url_functions`, `color_functions`, `gradients`, `transforms`, `filters`, `easing_functions`, `at_rules_keyframes`, `media_queries`, `vendor_prefixes`, `custom_at_rules`, `nested_rules`, `logical_properties`, `grid`, `flexbox`, `typed_property_groups`, `stylesheet_root`, plus the 4 remaining SK-V13-covered features) + 1 SK-V12 W1b CSS feature row key (`css_l4/declaration_values/direct_to_struct/main` per `v5-cross-tranche-stability.md §1 SK-V12 PARTIAL` and original REDRESS-123). The per-row REDRESS scribe preserves the row-authority pattern SK-V13 W11+ established for REDRESS-119-style reopen accounting.

### Convergence note

Both folds are editorial precision improvements, not substantive regressions. The ACCEPT-rate of 93.3% is one fold below the §3Z 95% convergence floor — V2 can converge in a single fold cycle iff F-1 + F-2 land cleanly. No other lens-CH3 finding requires a redress beyond these two docs.

No CH3-level escalation flag.
