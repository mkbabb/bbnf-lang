# CH1 CORRECTNESS — Pass Alpha V1 Disposition

Lens: every claim cites file:line, commit SHA, RESULTS row, or REDRESS
entry that resolves. Falsifiability gates are measurable. Comparator
deltas match the strictness plane. Audit-overlay verdicts cite the
correct validation §reference. R-target acceptance criteria are
empirically verifiable.

## §0 — Disposition summary

- Sections audited: 53 (7 SYNTHESIS, 8 HANDOFF, 7 α-A, 8 α-B, 5 α-C, 6 α-D, 12 α-E).
- ACCEPT-rate: **94.3 %** (50 ACCEPT / 53).
- REJECT count: **0**.
- REVISE count: **3** (α-A §2 direct-admit row-count drift; α-A §3 typed-admit row-count drift; α-E §6 hot-leaf falsifiability vague on "pre-wave" baseline).
- Critical findings: 0 (no audit-falsified citation; no unmeasurable gate; no honest-baseline contradiction).
- Escalation flag: **NONE**. Three REVISE items are accountancy refinements; none reopens a route the audit closed; none calls the V1 verdict into question.

The campaign's honest-baseline bind (0 / 51 JSON × 3 planes + 0 / 24 CSS L4) is preserved verbatim across all eight artefacts; the eight architectural pillars (W5 / W6 / W7 / bbnf-simd / OffsetFlags + Tape / `generated_json::parse_direct` / `generated_real_typed::parse_*` / 15 CSS `.bbnf`) carry forward without dilution. Every R1–R10 acceptance criterion in SYNTHESIS §0.3 names a concrete xtask command or grep predicate that resolves to a binary pass/fail; every C-1..C-5 falsifiability gate names a binary predicate.

## §1 — Per-artefact disposition table

### SYNTHESIS.md

| § | Disposition | Reason |
|---|---|---|
| §0.1 close condition R10 verbatim | ACCEPT | Lines 38-46 quote the R10 bar verbatim from `ORCHESTRATOR-PROMPT.md:154`; every JSON cell + CSS feature; intrinsic-block proof escape; full ADMIT or bracket SK-V15. |
| §0.2 goalset enumeration (75-row population) | ACCEPT | 17×3+24=75 matches `ROLLING-SOTA-DELTA.md:13-93` row table; nominal SK-V13 admit counts (5/4/7/24=40) are corrected to the audit bind (0/0/0/0). |
| §0.3 R-target table (R1–R10 acceptance) | ACCEPT | All 10 R-targets carry concrete acceptance commands: R1 names three plane-correct comparators; R2 names per-iter equality column; R3 enumerates 5 PRUNE sub-waves; R4 names `cargo xtask regen-css` + round-trip diff; R5 names corpus paths + ~960 KB; R6/R7/R8 each cite downstream binding. Empirically verifiable. |
| §0.4 pre-blocks P-1..P-7 | ACCEPT | All seven P-X carry validation-pack §refs (P-1 → v1 §1 Claim 1; P-2 → v6 §1 + §3; P-3 → v1 §1 Claim 4 + §3; P-4 → v2 §1; P-5 → v4 §4+§5; P-6 → v3 §1; P-7 → cross-ref Lock 1 + CH5). Every claim resolves to the cited line range. |
| §0.5 wave-by-wave gate deferral | ACCEPT | Deferral is contracted per `PASS-ALPHA.md §4.4`; SYNTHESIS §0.5 explicitly states the boundary + forward pointer to S-P3. Not paper-close. |
| §1 corrected diagnosis (1.1 + 1.2 + 1.3) | ACCEPT | Per-pillar citation table at §1.1 cites `v4 §1` (W5/W6/W7), `v3 §4` (bbnf-simd), `v3 §2` (OffsetFlags+Tape), `v2 §3.1` (parse_direct), `v2 §4.1` (parse_real_typed); each resolves. §1.2 row-by-row falsification cites v1/v2/v6/v3 by section. §1.3 audit-zero rolling delta is the single source of truth carried in α-A/B/C/D/E. |
| §2 telemetry binding (extended schema) | ACCEPT | `comparator_plane` + `per_iter_equality` columns are concrete; `audit_overlay_verdict` enum has three values per row; `xtask gate-json` is the enforcement point. Schema extends `sk-v13/SYNTHESIS.md §2` cleanly. |
| §3 candidate shortlist C-1..C-5 | ACCEPT | All five candidates carry concrete falsifiability gates (grep predicates, round-trip diff, byte-count thresholds, equality-column presence); risk classes match scope; same-wave consumer per candidate. |
| §4 S-P3 constraints | ACCEPT | 11 constraints all bind to specific gates; G-SIMD-GRAMMAR-POLICY clause (lines 276-281) closes the SIMD substrate-union hole correctly per CH5. |
| §5 pre-blocked / unblocked routes | ACCEPT | Both lists cite addendum + audit + Lock 14; no orphan claim. |
| §6 close posture | ACCEPT | Restates honest baseline; reads as standalone prose; no metalanguage. |

### HANDOFF.md

| § | Disposition | Reason |
|---|---|---|
| §1 bracket verdict | ACCEPT | "0 / 43 admitted rows survive strict-vs-strict" matches the audit-pack synthesis verbatim; pillar list matches §1.1 of SYNTHESIS. |
| §2 authority list | ACCEPT | 14-item ordered read list aligns with dispatch §0 + SYNTHESIS authority. |
| §3 honest baseline summary | ACCEPT | Eight pillars enumerated with v-pack citations; four falsifications enumerated with v-pack citations; 30 Lock 14 violation breakdown matches `v3 §1 + §8` (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW). |
| §4 pre-S-P0 readiness | ACCEPT | Working-tree status + seeded commits (`496a81417`, `6ab711d77`) resolve to git history. |
| §5 pass sequence (10 steps) | ACCEPT | Each step cites the binding doc (ORCHESTRATOR §3W, PASS-ALPHA §3, PASS-0-OVERFIT §CH7, etc.); no skipped gate. |
| §6 next-move chain | ACCEPT | `ready-for-CHALLENGE-V1 → G-Alpha → S-P0` matches ORCHESTRATOR §6 sign-off gates. |
| §7 refusal conditions (16 items) | ACCEPT | Covers P-1..P-7 + Lock 14 + G-SIMD-GRAMMAR-POLICY + parser-touch requirement; the refusal set is the empirical contract for downstream REVISE returns. |
| §8 V1 disposition (PENDING) | ACCEPT | Honest pending state; binds the contract until G-Alpha close per `ORCHESTRATOR.md §6`. |

### α-A — Results extraction

| § | Disposition | Reason |
|---|---|---|
| §0 preamble + conventions | ACCEPT | RESULTS.md + ROLLING-SOTA-DELTA.md line citations (`:3-49`, `:51-131`, `:13-93`) match wc -l verified files (RESULTS 185 lines, ROLLING-SOTA-DELTA 99 lines). |
| §1 parse_only table (17 rows) | ACCEPT | Every Mbps figure in the table reconciles to `ROLLING-SOTA-DELTA.md:14-31` and `RESULTS.md` per `Track 1 Mbps` column; audit overlay verdict per row cites v2 §1-4 + v6 §3 correctly; five W14 admits AUDIT-FALSIFIED with the right validation §ref. Δ vs SK-V12 numbers preserved from the prior SK-V13 alpha-A. |
| §2 direct_to_struct table (17 rows) | REVISE | The dispatch context §1 enumerates "4 JSON direct admits"; α-A §2 enumerates 6 ADMITTED rows (citm_catalog, apache_builds, marine_ik, instruments, numbers, unicode_basic). α-A acknowledges the discrepancy at lines 117-122 and binds the larger set as the comparator-misbinding pattern; the verdict (0/17 ADMITTED post-overlay) is correct. **Revise:** add an explicit reconciliation table mapping the 6 ROLLING-SOTA-DELTA "ADMITTED" rows to the dispatch §1 "4" — the +2 delta (marine_ik, instruments) needs a one-line audit-overlay justification (carry under same v6 §1 row 3 pattern) lest a downstream redress dispute the prune scope. |
| §3 real_typed_struct table (17 rows; 11 ADMITTED + 6 MISSING) | REVISE | Same shape as §2: dispatch §1 binds "7 JSON typed admits"; α-A §3 enumerates 11 ADMITTED rows (twitter, citm_catalog, apache_builds, github_events, update_center, mesh, random, marine_ik, instruments, numbers, unicode_basic). The narrative at lines 161-169 reconciles ("11 vs 7"; the 7 v2-traced rows + 4 newer W13/W15 typed admits inherit the same v6 §1 row 4 pattern) but the table itself does not flag which 4 rows extend dispatch §1's count. **Revise:** annotate the 4 extension rows (random, instruments, numbers, unicode_basic — the W13.1/.2/.3/.4 + the W15.1-update_center adjusted) so PRUNE-1 scope is unambiguous. |
| §4 CSS L4 table (24 rows) | ACCEPT | All 24 ADMITTED row Mbps + lightningcss Mbps + fixture-byte counts reconcile to `ROLLING-SOTA-DELTA.md:70-93`; SK-V12 W1b carry-over at +434 Mbps preserved; every row cites v1 §5 for AUDIT-FALSIFIED; the W10.3 124× anomaly cites v1 §1 Claim 5 + SYNTHESIS §CSS L4. |
| §5 c/B + telemetry | ACCEPT | `ns_per_byte=0.514090` for twitter parse_only resolves to RESULTS:55; `0.265339` for citm_catalog resolves to RESULTS:58; host triple matches RESULTS:55; c/B schema-debt acknowledgement is correctly framed. |
| §6 audit verdict summary | ACCEPT | 0/75 ADMITTED is the §1 audit bind; per-plane counts (5+6+11+24=46 nominal admits, all AUDIT-FALSIFIED) accurately tally the table data, with the 11 vs 7 typed discrepancy already disclosed at §3. |
| §7 forward pointers | ACCEPT | Each α-B/C/D/E/F forward pointer names the right downstream consumer; α-E pointer enumerates C-1..C-5 by ID + R-target. |

### α-B — Competitor deltas

| § | Disposition | Reason |
|---|---|---|
| §0 bound baseline | ACCEPT | Baseline citation `ORCHESTRATOR-PROMPT.md:71-77` resolves (lines 71-78 are the honest delta block); audit-bound 0/75 stands. |
| §1.1 per-plane comparator binding | ACCEPT | Three rows cite `benches/json_parity.rs:87-102`, `bbnf-bench/src/direct_struct.rs:427-429`, `bbnf-bench/src/real_typed_struct.rs:690-731`; each binding traced to v6 §1 / v2 §3.2 / §4.2. The framing distinction — typed already plane-correct, direct + parse_only misbound — is correct per v6 §1 + §2. |
| §1.2 comparator availability ledger | ACCEPT | Per-comparator coverage counts (sonic 17/17, simdjson DOM 13/17, yyjson 6/17, etc.) reconcile to RESULTS.md `simdjson DOM` / `yyjson` columns; non-sonic comparators correctly flagged as HONEST today and as ceasing to be the binding gate post-R1. |
| §2 parse_only rebound overlay (17 rows) | ACCEPT | Every Mbps figure matches `ROLLING-SOTA-DELTA.md:14-31`; COMPARATOR-PENDING-R1 verdict applies correctly per v6 §3 + the v6 §3 sonic-rs Skipper absence finding (v0.5.8). Projection prose ("0-2 of 5 survive R1 rebind") is calibrated; no overclaim. |
| §3 direct rebound overlay (17 rows) | ACCEPT | Margins reconcile; the 6 historic direct admits (citm_catalog, apache_builds, marine_ik, instruments, numbers, unicode_basic) reclassify COMPARATOR-PENDING-R1; projection "1-3 of 6 survive R1" is honest. |
| §4 typed rebound overlay (10 + 7 MISSING) | ACCEPT | The 6 MISSING corpora (canada, gsoc-2018, unicode_mixed, unicode_escapes, distinct_values, y_string_unicode) match `ROLLING-SOTA-DELTA.md:22,40,52,55,61,64`; ORACLE-PENDING-R2 verdict per row is the v6 §6 framing (binding is plane-correct; oracle is not per-iter). Distinction between "comparator misnaming" (R1) and "per-iter equality" (R2) is preserved. |
| §5 CSS rebound overlay (24 rows) | ACCEPT | Per-row T1 + lightningcss + fixture bytes reconcile to ROLLING-SOTA-DELTA + α-A §4; PIPELINE-PENDING-R4 + CORPUS-PENDING-R5 dispositions are mutually exclusive with admit until R4 + R5 land. Three-condition pending state (R4 + R5 + R6 work-equivalent comparator) is accurate. |
| §6 SK-V14 telemetry debt | ACCEPT | Per-plane comparator gap rows (10 entries) all carry concrete R-pointer (R1+R8, R1+R7, R2, R7, R4, R5, R6, R2+R6); coverage-at-HEAD numbers match §1.2; required coverage at admit gate is the R-target verbatim. |
| §7 roll-up | ACCEPT | 0/75 HONEST / 45 SUSPECT / 34 COMPARATOR-PENDING-R1 / 10 ORACLE-PENDING-R2 / 31 CORPUS-PENDING-R4+R5+R6 tally is internally consistent (45 SUSPECT = sum of historic admits per plane: 5 parse_only + 6 direct + 10 typed + 24 CSS); pending-condition multiplicity correctly disclosed in narrative. |
| §8 escalations | ACCEPT | The sonic v0.5.8 Skipper absence flagged as architectural-block risk for R1 parse_only matches v6 §3 finding; mitigation (~80 LOC in-tree wrapper) is concrete; 10 typed-MISSING corpora flagged as R7 scope expansion. |

### α-C — REDRESS digest

| § | Disposition | Reason |
|---|---|---|
| Binding interpretation | ACCEPT | Audit wins where it falsifies; REDRESS 119/120 LIFTED per addendum; Lock 14 count corrected to 30 per v3 §8 (11 CRITICAL + 7 HIGH + 5 MED + 7 LOW). |
| §1 per-entry SK-V13 dispositions (items 131-160, 30 entries) | ACCEPT | Sample-verified that REDRESS items 154-158 (W14.1-W14.5 = ADMIT for json/numbers, citm_catalog, canada, marine_ik, mesh parse_only) exist in `skinny/REDRESS.md` per `grep -nE "W14\.[1-5]"` (line 4767 onward); all five reclassify DEMOTE-AUDIT per v2 §1 + v6 §3. Items 131-135 (CSS W3/W4/W10.1-3) → 24 CSS rows correctly traced. Items 136-140 (W5-W9) → 3 SURVIVE + 2 SCAFFOLD per v4 §1+§4+§5. Items 141-148 + 159-160 (W11/W13/W15 direct/typed) → REOPEN-AUDIT per v6 §1 + §6. Disposition table at end (3 SURVIVE / 5 DELETE / 5 REVERT / 3 SCAFFOLD / 7 REOPEN / 7 HONEST-REJECT = 30) reconciles. |
| §2 pattern-level pre-blocks P-1..P-7 | ACCEPT | All seven P-X carry citation + pattern + SK-V14 binding + falsifiability + lift path; P-6 falsifiability gate (`find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json|JsonGrammar|parse_json_grammar'`) is the same predicate as SYNTHESIS §3 C-1; P-1 round-trip falsifiability and P-4 parser-touch indicator are concrete xtask checks. |
| §3 pattern-level summary table | ACCEPT | 7-row table maps each pre-block to its bar + lift + round-trip eligibility; consistent with §2. |
| §4 reopen obligations for S-P3 | ACCEPT | Six obligations (PRUNE-first, comparator rebind first, CSS only after R3+R4+R5, JSON re-baseline after R1+R2, parse_only distinct path before re-admit, round-trip rule, indefatigability) match the R-target dependency chain. |
| §5 closing posture | ACCEPT | One substantive architectural advance (W5-W7) + one substantive scaffold debt (W8+W9); 25 CSS + 5 parse_only collapse; 4 direct + 7 typed PARSERS hold + COMPARATORS lose; SK-V14 posture (PRUNE → REBIND → REBUILD) matches SYNTHESIS §3 + §6. |

### α-D — Validated / invalidated / demoted / still-open

| § | Disposition | Reason |
|---|---|---|
| §0 contract boundary | ACCEPT | Honest baseline restated; audit-pack commit `b24232776` + synthesis `084d83ecf` cited. |
| §1 source map | ACCEPT | Full read list including all six v-pack files + SK-V13 alpha-D antecedent. |
| §2 VALIDATED (V-1..V-8, 8 pillars) | ACCEPT | Each pillar carries source-file:line citation + audit §ref (V-1 → `passes/lib.rs:1` + v4 §1; V-3 → `passes/lib.rs:476-478` + v4 §1+§2; V-6 → `runtime/src/grammars/json/generated.rs:407-421` + v2 §3.1; V-7 → `bbnf-bench/src/generated_real_typed.rs` (1600+ lines) + v2 §4.1; V-8 → 15 .bbnf files at `grammar/css/l4/`, file list verified end-to-end). |
| §3 INVALIDATED (I-1..I-4) | ACCEPT | I-1 25-row CSS table fully enumerated with v1 §5 per row; I-2 5 parse_only with W14 commit SHAs reconciled to v2 §1; I-3 4 direct rows (note: α-D acknowledges the 6 vs 4 dispatch-discrepancy at lines 282-291 — handles it by taking dispatch §1 4 as authoritative, treating marine_ik as carry, excluding instruments at -672 OPEN); I-4 7 typed rows with comparator misbinding per v6 §2 row 4. |
| §4 DEMOTED (D-1 W8, D-2 W9, D-3 SK-V12 W1b) | ACCEPT | All three demote correctly; D-3 explicitly folds into I-1's 25-row bundle. |
| §5 STILL-OPEN (S-1..S-6) | ACCEPT | Six cohort enumeration; per-cohort reopen path traces R-target binding; S-2 24-feature list reconciles with sk-v13 scoping doc (verified via the 24 named features in α-D §5 S-2). |
| §6 net ledger | ACCEPT | 8 VALIDATED + 4 invalidated row-groups + 3 DEMOTED + 6 STILL-OPEN cohorts; audit-zero rolling delta restated. |

### α-E — Candidate shortlist

| § | Disposition | Reason |
|---|---|---|
| §0 authority + binding posture | ACCEPT | Eight authority docs with line refs; honest baseline bind cites ORCHESTRATOR-PROMPT.md:71-77 + DISPATCH-CONTEXT.md:55-58 (verified). |
| §1 why prune-first | ACCEPT | Reasoning is sound: candidate slots spent on prerequisites because admit thresholds against a misbound comparator on a non-existent corpus through a hand-curated generator are not measurable. R6/R7/R8 correctly deferred to SK-V14 wave program. |
| §2 shortlist table (5 candidates) | ACCEPT | All five candidates carry: ID + R-target + same-wave consumer + falsifiability + LOC + risk. LOC envelope (4.95k-8.3k) is realistic; risk classes (very high / high / medium) match scope per CH4. |
| §3 C-1 Lock-14 refactor | ACCEPT | Owner paths (10 paths) all resolve; `decision_csp.rs:235` hardcoded `"json"` rule string verified by grep; trait-based dispatch architectural intent is non-Lock-14-violating per CH2; falsifiability gates are concrete; dependency on C-3 + C-5 correctly stated. |
| §4 C-2 comparator rebind + per-iter oracle | ACCEPT | Owner paths resolve; fallback comparator selection (if no strict parse_only in sonic-rs, fall back to simdjson On Demand skip or yyjson structural counter) handles the sonic v0.5.8 Skipper-absence escalation per α-B §8; falsifiability rejects rows with empty equality column. |
| §5 C-3 regen-css + corpora | ACCEPT | xtask shape mirrors existing `regen-json` at `skinny/xtask/src/main.rs:121-127`; the 15 .bbnf grammars list verified at `grammar/css/l4/` (15 files confirmed); corpora vendoring plan is concrete; round-trip falsifiability gate is xtask + git diff. |
| §6 C-4 W8+W9 scaffold → load-bearing | REVISE | The falsifiability gate at line 442-444 requires a "named pre-wave row's hot leaf attribution differs from its pre-wave value." α-E nominates `json/numbers/direct_to_struct/main` (pre-wave hot leaf `parse_value_at` per "Lock 15 evidence"). **Revise:** the citation to "Lock 15 evidence" is not anchored to a v-pack §ref or RESULTS row; either bind it to the `RESULTS.md` hot-leaf column directly, or reference v2 §3.1 + W11.1's numeric-array dispatch trace explicitly, lest a downstream agent cannot reproduce the pre-wave baseline. The candidate itself is sound; the gate citation is under-specified. |
| §7 C-5 clean revert | ACCEPT | Owner paths fully enumerated (RESULTS.md + ROLLING-SOTA-DELTA.md + REDRESS.md + 7 template dirs + 7 provider files + 7 generated.rs + runtime/src/lib.rs); 29 REDRESS entries (5 W14 + 24 CSS) is the right count; falsifiability gates are git grep / ls / cargo build predicates. |
| §8 consolidated pre-blocks | ACCEPT | Carries α-C P-1..P-7 verbatim with per-candidate binding. |
| §9 concurrency + serialisation matrix | ACCEPT | Wave Zero (C-5 || C-2 || C-3), then C-1 after C-5+C-2, then C-4 after C-1+C-2; ledger single-writer discipline preserved per SK-V13 alpha-E concurrency matrix. |
| §10 cost + caps + telemetry | ACCEPT | Hard caps (20 / 15 / 30-45) match memory `[dispatch-hard-cap]` + addendum amendment; per-iter equality column + hot-leaf attribution column required. |
| §11 convergence + escalation | ACCEPT | Four escalation paths (sonic Skipper absence; .bbnf round-trip failure; C-4 hot-leaf invariance; C-1 sub-wave regression) each map to a concrete recovery mechanism (user re-pin, architectural-block proof per affected feature, abrogate-before-patch, per-grammar carve-out under no-workarounds-arch). |

## §2 — Critical findings

No REJECT-class findings. Three REVISE items detailed below.

### REVISE-1 — α-A §2 direct-admit row-count drift

**Citation:** α-A §2 lines 76-122; SYNTHESIS §0.2 lines 56-58 (binds 4 direct admits); DISPATCH-CONTEXT §1 line 47 (binds 4 direct admits).

**Issue:** α-A §2 enumerates 6 ADMITTED direct rows (citm_catalog, apache_builds, marine_ik, instruments, numbers, unicode_basic) where the dispatch §1 honest-baseline bind says "4 JSON direct admits." α-A discloses the discrepancy at lines 117-122 and binds the larger set under the same comparator-misbinding pattern. The 0/17 post-overlay verdict is correct. The audit (v6 §1 row 3 + §7) covers all 6 rows under the same misbinding pattern, so the discrepancy is accountancy, not correctness.

**Fold for V2:** add a 2-line reconciliation table in α-A §2 mapping ROLLING-SOTA-DELTA's 6 ADMITTED rows to the dispatch §1 "4" — name the +2 (marine_ik, instruments) explicitly with their v6 §1 row 3 binding so PRUNE-1's revert scope is unambiguous. Same fold flows through C-5's REDRESS-entry count: §7 currently says "29 entries (5 W14 + 24 CSS)"; the 6-vs-4 direct discrepancy does not affect that count because the direct admits are not separately reverted in PRUNE-1 (they reopen under R7), but the audit-trail prose should disclose the 4-vs-6 distinction once and bind it.

### REVISE-2 — α-A §3 typed-admit row-count drift

**Citation:** α-A §3 lines 126-176; SYNTHESIS §0.2 line 58 (binds 7 typed admits); DISPATCH-CONTEXT §1 line 48 (binds 7 typed admits).

**Issue:** α-A §3 enumerates 11 ADMITTED typed rows where the dispatch §1 bind says "7." α-A discloses at lines 161-169: the 7 v2 §4-traced rows (twitter, citm_catalog, apache_builds, github_events, update_center, mesh, marine_ik) are the SK-V12 carries; the +4 newer typed admits (random, instruments, numbers, unicode_basic via W13.1/.2/.3/.4 + W15.1 update_center adjusted) inherit the same v6 §1 row 4 pattern. All 11 reclassify AUDIT-FALSIFIED. Net 0/17 is correct.

**Fold for V2:** annotate the +4 extension rows in α-A §3's table (a footnote or "wave id" column would make it explicit). The typed-admit count drift cascades into α-C §1's W13.1-.4 + W15.1 disposition (correctly REOPEN-AUDIT under R7) and into α-D §3 I-4 (where the ROLLING-SOTA-DELTA-wider count of 10 + the dispatch §1 7 are reconciled correctly at lines 363-367). The fold here is a consistent labelling of the extension rows across A, C, D so a future S-P3 wave dispatching PRUNE-1's revert scope reads one number.

### REVISE-3 — α-E §6 C-4 falsifiability gate under-specified

**Citation:** α-E §6 lines 432-445.

**Issue:** The C-4 falsifiability gate requires a "named pre-wave row's hot leaf attribution differs from its pre-wave value." α-E nominates `json/numbers/direct_to_struct/main` with pre-wave hot leaf `parse_value_at` "per Lock 15 evidence." The Lock 15 evidence is not bound to a v-pack §ref, RESULTS.md row, or commit SHA in this artefact, leaving a downstream agent unable to reproduce the pre-wave baseline. The candidate itself is sound (CSP shape choice → runtime divergence → hot-leaf attribution change is the right gate); the citation is the gap.

**Fold for V2:** rebind the pre-wave hot-leaf citation to one of: (a) the `RESULTS.md` Hot-leaf column for the named row at HEAD (per the SYNTHESIS §2 telemetry schema, the column is required), or (b) v2 §3.1's numeric-array dispatch trace + W11.1 commit SHA. Either anchors the pre-wave baseline so the post-wave hot-leaf assertion is binary.

## §3 — Recommended folds for V2 (if any)

Three V2 dispatch instructions, all minor accountancy refinements; none requires α-agent re-dispatch beyond a single targeted line edit per finding. Aggregator may roll these into the V2 directive verbatim:

1. **α-A V2 dispatch:** add a 2-line reconciliation footnote at §2 disclosing the 6-vs-4 direct-admit drift (REVISE-1); add a "wave id" or footnote column at §3 marking the +4 typed extension rows (REVISE-2). No re-extraction needed; both reconciliations are already disclosed in α-A's narrative; the fold lifts them into the table proper. Hard cap: 10 min redress.

2. **α-E V2 dispatch:** rebind the C-4 pre-wave hot-leaf citation to a concrete RESULTS.md column or v2 §3.1 reference (REVISE-3). No re-research needed; the rebinding is a 3-line edit. Hard cap: 5 min redress.

3. **No-op for SYNTHESIS / HANDOFF / α-B / α-C / α-D:** none of these artefacts carry a finding that needs V2 fold. The 3-of-53-section REVISE rate is below the §3Z reject threshold; CH1 verdict is therefore convergence-bearing.

If V2 lands the three folds above and CH2-CH7 carry comparable ACCEPT-rates, the cycle converges per `ORCHESTRATOR.md §3Z` (≥95 % ACCEPT × two consecutive cycles) and the bracket advances to G-Alpha sign-off.
