# CH1 CORRECTNESS — Pass Alpha SK-V17 (cycle V4)

Lens: CH1 Correctness (PASS-ALPHA §3 + ORCHESTRATOR §3W). Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
+ `SYNTHESIS.md` + `HANDOFF.md` (cycle V4, folding the V3 CHALLENGE dispositions).

CH1 focus: (a) every claim cites a RESULTS.md row / REDRESS entry / commit SHA / measurement
file; (b) falsifiability gates are measurable; (c) competitor deltas computed on the correct
(materializing) plane vs lightningcss. CH1/CH6 REJECT any uncited claim.

Host discipline: HEAD `1c5bd7a25` re-verified (`git log --oneline -1` →
`feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`) — matches the contract anchor
and every artefact `master_head` field. Reviewer re-ran every load-bearing path/line/grep claim
directly against the worktree this cycle (not trusting the V3 ledger).

NOTE — scope (carried from V3, unchanged): PASS-ALPHA §6 names six alpha artefacts (αA..αF).
Only αA..αE exist on disk; **αF (`αF-contract-draft.md`) is ABSENT** — its deliverable
(SYNTHESIS.md + HANDOFF.md) was authored directly per the §2 row ("α-F … `SYNTHESIS.md` +
`HANDOFF.md`"). Contract-compliant; not a CH1 defect.

---

## Verification performed (reviewer evidence, this cycle — re-run, not inherited)

1. `git log --oneline -1` → `1c5bd7a25`. ✓ Contract anchor + all artefact HEAD fields correct.
2. **24-row broadcast ground truth — RE-GREPPED:** `grep -c '^| css_l4/' skinny/RESULTS.md` =
   **24**; `grep -c 'css_l4/.*direct_to_struct/main'` = **25** (substring); `grep -c '2319.041'`
   = **24** (broadcast tuple on exactly 24 lines). Table-row range first **112** / last **135**
   (`grep -n '^| css_l4/'`). Line **154** = `` `css_l4/declaration_values/direct_to_struct/main`;
   REDRESS-127; companion `` — a prose REDRESS-127 companion reference, NOT a table row.
   `grep -nE 'W6.*css|tape.*direct_to_struct' skinny/RESULTS.md` = **EMPTY**: no admitted/distinct
   W6 typed CSS row. ✓ All five numeric ground-truth claims hold EXACT.
3. **V3-CH1-a (αA §2 stale reconciliation note): FOLDED.** αA:141-147 now reads "All cohort
   artefacts state 24 / lines 112-135 as of V3 (αC §4/§7, αD §0/§5, SYNTHESIS §0.2, HANDOFF
   Current-State); the V2 '6' undercount is resolved across the cohort." The self-contradictory
   "ONLY one … states the correct count" sentence is gone. Ground-truth at :141-143 retained.
   `v4_fold_dispositions` (αA:18-19) records the fix. ✓
4. **V3-CH1-b (αC §4 mislabelled 25th grep hit): FOLDED.** αC:229-232 now reads "= 25 substring
   matches, of which 24 are `^| css_l4/` table rows (112-135) and the 25th (:154) is a prose
   REDRESS-127 companion reference, not a row; there is NO admitted/distinct W6 typed CSS row in
   RESULTS.md (`grep 'W6.*css|tape.*direct_to_struct'` = EMPTY)." The "+ 1 distinct W6 typed row"
   mislabel is gone. ✓ SYNTHESIS changelog (:11-15) records both V4 folds verbatim.
5. **`assert_lightningcss_strict_equality` (SYNTHESIS:110 / αE / αF):** defined
   `nonjson_css_l4.rs:776`; call sites `:1057`, `:3460`. ✓ EXACT.
6. **`W8_SELECTED_CSS_ROWS: usize = 24`** at `css_l4_w8.rs:17` (consumed :160, asserted :440).
   ✓ EXACT — backs αC §4 / αC §7 row-4 / SYNTHESIS §0.4 "one tuple ×24 row-ids".
7. **Tape substrate (UNWIRED):** `skinny/crates/core/src/runtime/tape/` exists (assembler.rs,
   event_grammar.rs, offsets.rs, mod.rs). `grep -rc TapeStructBuilder skinny/` = EMPTY → UNWIRED
   disclosed truthfully (αA §6 "dead code", αD V6). ✓
8. **αE candidate anchors RE-VERIFIED:** udot orphan `parse_4_digits_dotprod` at
   `bbnf-simd/src/aarch64/digit_mac.rs:27`, `udot` instruction `:40`, scalar twin
   `parse_4_digits` `:5` (C4a). `grep -rln i8mm skinny/crates` = EMPTY → C4b "net-new i8mm,
   grep-clean-absent" EXACT. `W5C_REQUEST_FACT_PROFILES` defined `codegen/src/lib.rs:336`
   (consumed 299/567/611) → C0 retire-list target EXACT. ✓
9. **Competitor plane:** every css row 112-135 carries `lightningcss_strict[plane=
   css_l4_full_parse,strictness=strict,…,mbps=929.281,source=lightningcss-1.0.0-alpha.71:
   StyleSheet::parse]` (full-CSSOM materializing) and `cssparser_oracle[…,mbps=2362.037]`
   (token/full-parse probe). `grep -oE 'lightningcss_mbps=…'` = single value `929.281`,
   consistent with the broadcast (NOT a per-corpus baseline). The 793/833/974 dispersion the
   artefacts cite is attributed to the canonical/A-series reports, not to these falsified rows.
   αB carries 23 `[INF]`/`[AGG]`/`UNMEASURED-PENDING` markings → inference discipline present. ✓
10. **JSON guard:** twitter/parse_only Track1 `8349.290` > sonic-strict `4913.095` = +69.9%
    (αB:268, EXACT); citm row present. ✓

---

## §1 — alphaA (results extraction)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 standing + benched-surface disambiguation | ACCEPT | 0/24 admitted; skinny-tree translation intact; 24-count stated correctly. |
| §1 canonical bench table | ACCEPT | 70/974/2539 disclosed as contract canonical; single-sample vs build-run dispersion flagged; N≥50-median binding stated. |
| §2 per-corpus structure | ACCEPT | **V3-CH1-a FOLDED.** :141-147 reconciliation note rewritten ("all cohort artefacts state 24/112-135 as of V3; V2 '6' undercount resolved"); stale self-contradictory sentence removed; ground-truth :141-143 (24, lines 112-135, grep-verified at `1c5bd7a25`) retained + re-verified EXACT. |
| §3 8-field equality | ACCEPT | 10136/9561/9561/20043, errors=0, `shared_summary_equal=true`. |
| §4 20x checkpoint | ACCEPT | 14.2x/15.6x/20x cited; SHA `8153236e8`. |
| §5 sub-wave ledger | ACCEPT | Each row SHA + report line; single-sample caveat honest. |
| §6 banked wins | ACCEPT | Provenance per row; V6 substrate re-cited skinny `runtime/src/tape/`; `grep TapeStructBuilder skinny/ = EMPTY` re-verified. |
| §7 goalset seed | ACCEPT | Lever sequence on benched surface; neutrality vehicle `dispatch.rs:42,101`; 24-row broadcast pre-block cited. |
| §8 citation ledger | ACCEPT | Comprehensive; normalize-A-series-only note present. |

alphaA: 8 ACCEPT, 0 REVISE, 0 REJECT.

---

## §2 — alphaB (competitor deltas)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 plane taxonomy | ACCEPT | Materializing-vs-token-scan = correct CSS analogue of sonic-rs `utf8_lossy`; lightningcss=fair bar, cssparser=flaw probe; 24-count cited. |
| §0.1 benched-substrate disclosure | ACCEPT | Benched Track 1 = fact-stream String today; "typed CSSOM" = intended SK-V17 subject; gap arithmetic uses parse-throughput canonicals. |
| §1 canonical baseline | ACCEPT | HEAD verified; corpus bytes cited; dispersion = harness inadequacy; `normalize` ABSENT disclosed. |
| §2 per-corpus delta vs lightningcss | ACCEPT | `[INF]`/`[RNG][INF]`/`[AGG]` inline marking + legend (23 markings re-counted); only aggregate `[AGG cited]`; UNMEASURED-PENDING + §6 wave-gate prohibition. Correct-plane delta. |
| §3 per-corpus delta vs cssparser | ACCEPT | `[INF]` inline; cssparser plane-mismatched, ~36x aggregate, NOT a SOTA bar. |
| §4 inter-comparator relation | ACCEPT | lightningcss ~38% of cssparser; ratios cited not inferred; materialization-tax framing correct. |
| §5 JSON guard | ACCEPT | twitter +69.9% (8349.290/4913.095) re-verified EXACT against RESULTS; citm +62.0% present. |
| §6 findings feed | ACCEPT | Each finding restates a cited result; UNMEASURED-PENDING + no-inferred-endpoint-gate mandate binds downstream. |
| Verification ledger | ACCEPT | INFERRED items enumerated; 24-count restated; benched-tree provenance cited. |

alphaB: 9 ACCEPT, 0 REVISE, 0 REJECT.

---

## §3 — alphaC (REDRESS digest)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 framing + tree-disambiguation table | ACCEPT | Core-tree-vs-skinny mapping with verified paths; Lock 2 retirement. |
| §1 AZ-IV eager value tree | ACCEPT | 118x cited `cb14970f` + `sk-v16-arch:46-66`; re-open test skinny-keyed. |
| §2 StructRegistry/Arena/Builder + §2b layout | ACCEPT | 28-65x/983x/10583x WATCHDOG cited; SPLIT re-keyed to skinny surface + Lock 2. |
| §3 fact-stream String | ACCEPT | `emit_fact_stream` `generated.rs:5,61`; ~34% self-time; retirement-clause re-open test = correct anti-second-substrate gate. |
| §4 24-row broadcast | ACCEPT | **V3-CH1-b FOLDED.** :229-232 now "25 substring matches, of which 24 are `^| css_l4/` table rows (112-135), 25th (:154) prose REDRESS-127 companion, NO W6 typed CSS row (`grep 'W6.*css|tape…'` EMPTY)." Re-greped EXACT: 24 `^|`, 25 substring, :154 = REDRESS-127 prose, W6-grep empty. Header :215 + :220/:228/:233 still state 24. PERMANENT-PRE-BLOCK + basis note unaffected. |
| §5 FNV/fixture | ACCEPT | 148-fn correction applied; both contrivances cited. |
| §6 x86/AVX | ACCEPT | aarch64-only; x86 sites `REDRESS.md:465-468`; Lock 16. |
| §7 consolidated ledger | ACCEPT | Row-4 says 24 / lines 112-135 / `W8_SELECTED_CSS_ROWS=24` (css_l4_w8.rs:17 re-verified); re-open tests skinny-keyed. |
| §8 single distinction + lock anchors | ACCEPT | Admit/pre-block line = correct CH3 anchor; Lock 1/2/8/14/16 cited. |

alphaC: 9 ACCEPT, 0 REVISE, 0 REJECT.

---

## §4 — alphaD (validated/invalidated ledger)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 benched-surface disambiguation + grammar-witness reality | ACCEPT | Doc→skinny path table; JSON-witnessed-only generality disclosure; 24-count + lines 112-135 grep-verified. |
| §1 validated (V1-V6) | ACCEPT | Each SHA (`ea8138056`,`4de419f5e`,`2a85bf240`,`8153236e8`,`1c5bd7a25`) + measured evidence; V6 UNWIRED with grep evidence. |
| §2 invalidated (I1-I7) | ACCEPT | Each cites measured refutation; I7 N≥50 mandate. |
| §3 still-open (O1-O5) | ACCEPT | Owner paths skinny-keyed; O3-NEON hot-leaf %% tagged S-P1-re-confirm; O5 relabel to grammar-derivation-not-TOML-LOC (V3 F1 fold); falsifiable NO-GO thresholds bound. |
| §0/§5 CSS-row governing fact | ACCEPT | Lines 128-130 + 183-185 say "24 … AUDIT-FALSIFIED, `skinny/RESULTS.md:112-135`, grep-verified count = 24." Conclusion (zero ADMITTED typed CSS rows) survives. |
| §4 demoted | ACCEPT | Pattern H / FNV / Decision Engine cited to `sk-v16/SYNTHESIS.md`. |

alphaD: 6 ACCEPT, 0 REVISE, 0 REJECT.

---

## §5 — alphaE (candidate shortlist)

Candidate content converged at V3 (11 ACCEPT/0/0); V4 advances only changelog + cycle stamp
(αE:12-23). The CH4-V2 `resolve_builder_routes` fabrication was struck at V3 (grep-clean) and is
NOT carried into V4. All anchors re-verified this cycle (item 8).

| Section | Disposition | Rationale + fix |
|---|---|---|
| V2/V3/V4 changelog | ACCEPT | C4 split, W5C retire-list, JSON-witnessed generality, 187→148, same-run-lightningcss-median, V3 fold-confirm accurate. |
| §0 ground-truth anchors | ACCEPT | Every anchor reviewer-verified (digit_mac udot `:27`/`:40`, dispatch.rs:42/101, i8mm=0, 148-fn, `W5C_REQUEST_FACT_PROFILES` lib.rs:336). |
| C0 de-fact-stream | ACCEPT | Skinny paths; retire-list `W5C_REQUEST_FACT_PROFILES`; gate measurable (8-field EXACT + typed-not-String + N≥50 + profile-const-deleted grep). |
| C1 tape wiring | ACCEPT | Skinny paths; ≥30 PASS / <20 NO-GO measurable; generality EXIT gate = Lock-14 binding. |
| C2 NEON pre-scan | ACCEPT | `select_classifier`/`lo6_table_admissible` cited; scalar-ref + checkasm present; ≥80 gate measurable. |
| C3 commit-by-construction | ACCEPT | ≥300 / >same-run-lightningcss / <200 NO-GO measurable. |
| C4a udot orphan | ACCEPT | `digit_mac.rs:27,40` orphan + scalar twin `:5` re-verified; checkasm REQUIRED; admits unconditionally. |
| C4b net-new i8mm (GATED) | ACCEPT | i8mm grep-clean-absent re-verified; hard ENTRY gate + honest-residual EXIT = no-paper-close; scalar-ref + checkasm REQUIRED. |
| §2 dependency order | ACCEPT | DAG measurable; C0+C1 coupling explicit. |
| §3 cross-cutting discipline | ACCEPT | N≥50-median, comparator-plane, 8-field-EXACT, witnessed-generality, no-paper-close — each measurable. |
| §4 escalation note | ACCEPT | C0-unmeasurable → §8 BLOCKED; hot-leaf-%% S-P1-re-confirm. Correct PASS-ALPHA §8. |

alphaE: 11 ACCEPT, 0 REVISE, 0 REJECT.

---

## §6 — SYNTHESIS.md (αF contract)

| Section | Disposition | Rationale + fix |
|---|---|---|
| header + changelog | ACCEPT | Both V4 folds (V3-CH1-a, V3-CH1-b) recorded verbatim at :8-15; V3 substantive folds carried; skinny owner paths named; totality SK-V18 fold target. |
| Authority | ACCEPT | Files cited; G-Omega-only gate posture. |
| §0.1 close condition | ACCEPT | Every surface citation skinny-tree; Tape/Layout/preserve-rich-ast/equality/>SOTA/tailwind/telemetry gates grep-verifiable + measurable. |
| §0.2 starting state | ACCEPT | "24 `css_l4/*/direct_to_struct/main` rows (lines 112-135, grep-verified = 24) … falsified broadcast diagnostics" — re-verified EXACT. lightningcss run-dependence bound to same-run N≥50 median. |
| §0.3 receiver goalset | ACCEPT | Receiver obligations target skinny paths; `W5C_REQUEST_FACT_PROFILES` DELETE replaces non-existent template. |
| §0.4 pre-blocks | ACCEPT | Faithful to αC; 24-row broadcast pre-block 24 / lines 112-135. |
| §0.5 per-corpus close | ACCEPT | Corpus set `{bootstrap,tailwindcss,material-components-web,animate}`; `normalize`-absent; inferred αB endpoints UNMEASURED-PENDING; "animate OR bootstrap". Correct §4.1 form. |
| §0.6 strict comparator gate | ACCEPT | `assert_lightningcss_strict_equality` `nonjson_css_l4.rs:776` (sites :1057,:3460) re-verified EXACT; disambiguates harness `benches/nonjson_css_l4.rs:8`. Plane mapping correct. |
| Section 1 ledger | ACCEPT | A-series 454/735/496 cited `3b8b757d` with normalize-A-series-only caveat. |
| Section 2 telemetry | ACCEPT | CSS schema (sample_count≥50, median, cold, full-cssom, equality-before-speed, rich-ast, `tape_activated` not-by-core-grep, `w5c_profile_array_retired`) measurable; broadcast tripwire specified. |
| Section 3 trajectory | ACCEPT | Four-lever route skinny-translated; hot-leaf %% S-P1-re-confirm; "animate OR bootstrap" consistent with §0.5. |

SYNTHESIS: 11 ACCEPT, 0 REVISE, 0 REJECT.

---

## §7 — HANDOFF.md (αF packet)

| Section | Disposition | Rationale + fix |
|---|---|---|
| Benched-substrate disclosure | ACCEPT | Skinny tape tree + fact-stream String + `W5C_REQUEST_FACT_PROFILES`; core symbols grep-clean-absent SK-V18 fold target. |
| Current State | ACCEPT | "24 `css_l4/*/direct_to_struct/main` rows (lines 112-135, grep-verified = 24) … falsified broadcast diagnostics"; consistent with :158-160. Re-verified EXACT. |
| What SK-V17 Opens | ACCEPT | Gating artefact + four-lever route name skinny codegen surface; hot-leaf %% S-P1-re-confirm. |
| Authority / Gate Posture | ACCEPT | Files cited; G-Omega-only; CH7 disclosed as pass-added extension lens. |
| Pre-Blocked Routes | ACCEPT | Faithful to αC + SYNTHESIS §0.4; 24-row broadcast 24; no second-substrate/Lock-1 escape carried. |
| Next Move | ACCEPT | S-P1 re-profile obligation; skinny wave-surface naming; C4a-unconditional/C4b-gated; "animate OR bootstrap". |
| Close criterion / revert-deferral | ACCEPT | "animate OR bootstrap"; revert/hard-cap/triumvirate sanctioned-deferred to S-P3 per §4.4. |

HANDOFF: 7 ACCEPT, 0 REVISE, 0 REJECT.

---

## Consolidated CH1 disposition (cycle V4)

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| alphaA | 8 | 0 | 0 |
| alphaB | 9 | 0 | 0 |
| alphaC | 9 | 0 | 0 |
| alphaD | 6 | 0 | 0 |
| alphaE | 11 | 0 | 0 |
| SYNTHESIS | 11 | 0 | 0 |
| HANDOFF | 7 | 0 | 0 |
| **TOTAL** | **61** | **0** | **0** |

ACCEPT rate = 61/61 = **100%**. Above the §3Z ≥95% convergence bar. Both V3 REVISEs
(V3-CH1-a stale reconciliation note in αA §2; V3-CH1-b mislabelled 25th grep hit in αC §4) are
fully folded and now ACCEPT, re-verified directly against the worktree. **Zero open REVISE,
zero orphan REVISE, zero REJECT.** No new substantive uncited claim surfaced in the V4 fold;
the candidate content (αE) and all four ground-truth conclusions (zero admitted typed CSS rows;
24-row broadcast PERMANENT PRE-BLOCK lines 112-135; lightningcss fair full-CSSOM comparator;
cssparser plane-disclosed flaw probe) are stable across V3→V4.

This is CH1's **second consecutive ≥95% cycle** (V3 = 96.7%, V4 = 100%) with zero orphan REVISE
— satisfying the ORCHESTRATOR §3Z convergence law for this lens.

## Plane / measurability / citation verdicts (CH1's three mandates)

**Competitor-plane verdict: PASS.** Every artefact computes the >SOTA delta against lightningcss
full-CSSOM (`plane=css_l4_full_parse,strictness=strict,source=lightningcss-1.0.0-alpha.71:
StyleSheet::parse`, re-verified on RESULTS rows 112-135) and demotes cssparser (`2362.037`,
re-verified) to a plane-disclosed flaw probe. αB's 23 `[INF]`/`[AGG]`/`UNMEASURED-PENDING`
in-table markings + §6 prohibition on keying wave gates to inferred per-corpus endpoints is the
model. The lightningcss baseline carried in the CSS rows is the single broadcast `929.281`
(consistent with the 24-row broadcast pre-block, NOT a per-corpus baseline); the 793/833/974
dispersion the artefacts cite is correctly attributed to the canonical/A-series reports, and the
wave gate is bound to a same-run N≥50 median rather than any frozen literal. No plane dishonesty.

**Falsifiability-gate verdict: PASS.** Every αE candidate (C0-C4b) carries a measurable PASS/NO-GO
threshold (8-field EXACT, ≥30/≥80/≥300 Mbps, grep-verifiable retirement of
`W5C_REQUEST_FACT_PROFILES` at `codegen/src/lib.rs:336`, `tape_activated`-not-by-core-grep).
SYNTHESIS §0.1 gates are grep-verifiable on `skinny/crates/`; the telemetry schema is emitted
verbatim with the broadcast tripwire.

**Citation verdict: PASS.** Every substantive claim cites a RESULTS row / REDRESS entry / commit
SHA / measurement file, reviewer-RE-VERIFIED at HEAD `1c5bd7a25` (not inherited from V3). The two
V3 REVISEs were localized metadata defects, both now folded. Zero uncited substantive claims
remain.
