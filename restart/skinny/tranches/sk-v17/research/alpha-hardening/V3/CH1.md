# CH1 CORRECTNESS — Pass Alpha SK-V17 (cycle V3)

Lens: CH1 Correctness (PASS-ALPHA §3 + ORCHESTRATOR §3W). Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
+ `SYNTHESIS.md` + `HANDOFF.md` (cycle V3, folding the V2 CHALLENGE dispositions).

CH1 focus: (a) every claim cites a RESULTS row / REDRESS entry / commit SHA / measurement
file; (b) falsifiability gates are measurable; (c) competitor deltas computed on the correct
(materializing) plane vs lightningcss. CH1/CH6 REJECT any uncited claim.

Host discipline verified: HEAD `1c5bd7a25` (`git log --oneline -1` →
`feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`). Reviewer re-verified every
load-bearing path/line/grep claim directly against the worktree this cycle.

NOTE — scope: PASS-ALPHA §6 names six alpha artefacts (αA..αF). Only αA..αE exist on disk;
**αF (`αF-contract-draft.md`) is ABSENT** — its output (SYNTHESIS.md + HANDOFF.md) was
authored directly per the §2 row ("α-F … `SYNTHESIS.md` + `HANDOFF.md`"). That is contract-
compliant (αF's *deliverable* is the contract draft, not a separate research file); flagged
for the consumer but not a CH1 defect.

---

## Verification performed (reviewer evidence, this cycle)

1. `git log --oneline -1` → `1c5bd7a25`. ✓ All artefact HEAD anchors correct.
2. **The V2 CH1-R1 count defect (6→24): FOLDED.** `grep -c '^| css_l4/.*/direct_to_struct/main '
   skinny/RESULTS.md` = **24**; rows span **112-135** (first three 112/113/114, last 135); each
   carries the identical broadcast tuple `track1_mbps=2319.041;cssparser_mbps=2362.037;
   lightningcss_mbps=929.281`. Every artefact now states 24 / lines 112-135 / grep-verified:
   αA (:124,:127,:131,:140,:360), αB (:34,:339), αC (:215,:220,:228,:230,:350), αD
   (:128,:130,:183,:185), SYNTHESIS (:139-146,:202-204), HANDOFF (:48-52,:158-160). The only
   surviving "6" strings live in `alpha-hardening/V1/CH1.md` (historical, out of review scope).
   ✓ The propagated undercount is corrected everywhere it was live.
3. **The V2 CH1-R2 line defect (:8→:776): FOLDED.** `assert_lightningcss_strict_equality` is
   defined at `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:776` (call sites 1057, 3460 —
   verified). SYNTHESIS:110 now cites `nonjson_css_l4.rs:776 … call sites :1057,:3460` and
   correctly disambiguates the *harness* file `benches/nonjson_css_l4.rs:8` (a distinct file —
   `find skinny -name nonjson_css_l4.rs` returns both `src/` and `benches/`). SYNTHESIS:305
   carries the substantive claim without a stale line. ✓
4. `W8_SELECTED_CSS_ROWS: usize = 24` at `skinny/crates/bbnf-bench/src/css_l4_w8.rs:17`
   (consumed :160, asserted :440). ✓ αC §7 row-4 / SYNTHESIS §0.4 "one tuple ×24 row-ids"
   binding EXACT.
5. **The 25th grep hit (αC §4 defect — see V3-CH1-b):** `grep -c 'css_l4/.*/direct_to_struct/
   main' skinny/RESULTS.md` (αC's exact pattern, no leading `| `) = **25**, but
   `grep -c '^| css_l4/'` = **24**. The 25th match is `skinny/RESULTS.md:154`, a *prose
   companion reference* inside a REDRESS annotation ("``css_l4/declaration_values/
   direct_to_struct/main``; REDRESS-127; companion …"), NOT a table row and NOT a W6 typed
   row. `grep -n 'W6.*css\|tape.*direct_to_struct' skinny/RESULTS.md` = EMPTY: there is **no
   admitted/distinct W6 typed CSS row** in RESULTS.md. αC:229 mislabels :154 as "1 distinct W6
   typed row" — wrong. (See V3-CH1-b.)
6. Competitor plane: lightningcss `929.281` Mbps carried with `plane=css_l4_full_parse,
   strictness=strict,source=lightningcss-1.0.0-alpha.71:StyleSheet::parse` in every RESULTS
   row 112-135; cssparser `2362.037` is the `cssparser_oracle` token/full-parse probe. The
   artefacts compute the >SOTA delta against lightningcss (materializing) and demote cssparser
   to plane-disclosed flaw probe. ✓ Correct-plane mandate satisfied (see plane verdict).
7. αA:142-146 cross-artefact reconciliation note (see V3-CH1-a): asserts the four sibling
   artefacts "all undercount … as '6' … MUST be corrected to 24 … in the V3 fold." Verified
   against current files: those four ALREADY say 24 (item 2). The note is now stale + self-
   contradictory ("This artefact is the ONLY one … that states the correct count" — false at
   V3). REVISE.

---

## §1 — alphaA (results extraction)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 standing + benched-surface disambiguation | ACCEPT | 0/24 admitted; skinny-tree translation intact (`RequestFacts`, `track1_facts`, `runtime/src/tape/`); 24 falsified-row count now stated correctly (lines 12,19). |
| §1 canonical bench table | ACCEPT | 70/974/2539 disclosed as contract canonical; single-sample vs build-run dispersion honestly flagged; N≥50-median binding stated. |
| §2 per-corpus structure | **REVISE** | **V3-CH1-a.** Lines 142-146 carry a now-STALE cross-artefact reconciliation note: it claims "αC §4/§7, αD §0/§5, SYNTHESIS §0.2, HANDOFF Current-State all undercount the same rows as '6' … MUST be corrected to 24, lines 112-135 in the V3 fold" and "This artefact is the ONLY one in the V2 alpha cohort that states the correct count." The V3 fold has landed — all four now state 24 (verified item 2). The note is factually wrong at V3 (others no longer say "6") and self-contradictory. **Fix: delete lines 142-146 (or rewrite as "All cohort artefacts state 24 / lines 112-135 as of V3; the V2 undercount is resolved").** The grep-verified ground truth at lines 140-141 (24, lines 112-135) is correct and stays. Substantive conclusion (zero admitted typed CSS rows) unaffected. |
| §3 8-field equality | ACCEPT | 10136/9561/9561/20043, errors=0, `shared_summary_equal=true`; core-tree-witness vs skinny-benched framing correct. |
| §4 20x checkpoint | ACCEPT | 14.2x/15.6x/20x cited; SHA `8153236e8`; benched-surface note folded. |
| §5 sub-wave ledger | ACCEPT | Each row SHA + report line; single-sample caveat honest. |
| §6 banked wins | ACCEPT | Provenance per row; V6 substrate re-cited skinny `runtime/src/tape/`; `grep TapeStructBuilder skinny/ = EMPTY` disclosed. |
| §7 goalset seed | ACCEPT | Lever sequence on benched surface; neutrality vehicle `dispatch.rs:42,101`; 24-row broadcast pre-block cited (:360). |
| §8 citation ledger | ACCEPT | Comprehensive; normalize-A-series-only note present. |

alphaA: 7 ACCEPT, 1 REVISE, 0 REJECT.

---

## §2 — alphaB (competitor deltas)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 plane taxonomy | ACCEPT | Materializing-vs-token-scan = correct CSS analogue of sonic-rs `utf8_lossy`; lightningcss=fair bar, cssparser=flaw probe; 24-count cited (:34) with lines 112-135. |
| §0.1 benched-substrate disclosure | ACCEPT | Benched Track 1 = fact-stream String today; "typed CSSOM" = intended SK-V17 subject; gap arithmetic uses parse-throughput canonicals. Honest. |
| §1 canonical baseline | ACCEPT | HEAD verified; corpus bytes cited; dispersion = harness inadequacy; `normalize` ABSENT disclosed. |
| §2 per-corpus delta vs lightningcss | ACCEPT | `[INF]`/`[RNG][INF]`/`[AGG]` inline marking + legend; only aggregate `[AGG cited]`; UNMEASURED-PENDING + §6 wave-gate prohibition present. Correct-plane delta. |
| §3 per-corpus delta vs cssparser | ACCEPT | `[INF]` inline; cssparser plane-mismatched, ~36x aggregate, NOT a SOTA bar — correct. |
| §4 inter-comparator relation | ACCEPT | lightningcss ~38% of cssparser; 793/2529=0.314, 833/2476=0.336 cited not inferred; materialization tax framing correct. |
| §5 JSON guard | ACCEPT | twitter +69.9% / citm +62.0% verified EXACT against RESULTS.md (8349.290/4913.095; 20512.601/12662.292). |
| §6 findings feed | ACCEPT | Each finding restates a cited result; UNMEASURED-PENDING + no-inferred-endpoint-gate mandate binds downstream waves. |
| Verification ledger | ACCEPT | INFERRED items enumerated; 24-count restated (:339); benched-tree provenance cited. |

alphaB: 9 ACCEPT, 0 REVISE, 0 REJECT.

---

## §3 — alphaC (REDRESS digest)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 framing + tree-disambiguation table | ACCEPT | Core-tree-vs-skinny mapping with verified paths (`W5C_REQUEST_FACT_PROFILES` lib.rs:336, 7 `RequestFacts` regen_css.rs registrations, Lock 2 retirement). Re-verified. |
| §1 AZ-IV eager value tree | ACCEPT | 118x (canada 1.83ms→215.7ms) cited `cb14970f` + `sk-v16-arch:46-66`; re-open test skinny-keyed; PayloadArena-counter telemetry binding. |
| §2 StructRegistry/Arena/Builder + §2b layout | ACCEPT | 28-65x/983x/10583x WATCHDOG cited; SPLIT re-keyed to skinny surface + Lock 2; `W5C_REQUEST_FACT_PROFILES` relocation trap named. |
| §3 fact-stream String | ACCEPT | `emit_fact_stream` `generated.rs:5,61`; ~34% self-time; `track1_facts` String `:596`; retirement-clause re-open test = correct CH5 anti-second-substrate gate. |
| §4 24-row broadcast | **REVISE** | **V3-CH1-b.** The header (:215), :220, :228, :230, :233 all correctly state **24** (V2 CH1-R1 folded — good). BUT line 229 says: "`grep -c 'css_l4/.*/direct_to_struct/main'` = 24 broadcast rows **+ 1 distinct W6 typed row at :154**". This is a misreading of the 25th grep hit: `skinny/RESULTS.md:154` is a **prose companion reference inside a REDRESS-127 annotation** (verified: "``css_l4/declaration_values/direct_to_struct/main``; REDRESS-127; companion …"), NOT a table row and NOT a W6 typed row. `grep -n 'W6.*css\|tape.*direct_to_struct' skinny/RESULTS.md` = EMPTY — there is no admitted/distinct W6 typed CSS row. Asserting one contradicts this artefact's own load-bearing conclusion (zero admitted typed CSS rows) and αD §0 (:128). **Fix at :229: replace "= 24 broadcast rows + 1 distinct W6 typed row at :154" with "= 25 substring matches, of which 24 are `^| css_l4/` table rows (lines 112-135) and the 25th (:154) is a prose REDRESS-127 companion reference, not a row; there is NO admitted/distinct W6 typed CSS row in RESULTS.md."** The PERMANENT-PRE-BLOCK classification + 24-row basis note (:233-235) are unaffected. |
| §5 FNV/fixture | ACCEPT | 148-fn correction applied (187 stale); both contrivances cited. |
| §6 x86/AVX | ACCEPT | aarch64-only; no-SVE; x86 sites `REDRESS.md:465-468`; Lock 16 cited. |
| §7 consolidated ledger | ACCEPT | Row-4 (:350) correctly says 24 / lines 112-135 / `W8_SELECTED_CSS_ROWS=24` (verified css_l4_w8.rs:17); each row maps to its § with measured refutation; re-open tests skinny-keyed. |
| §8 single distinction + lock anchors | ACCEPT | Admit/pre-block line = correct CH3 anchor; Lock 1/2/8/14/16 anchors cited. |

alphaC: 8 ACCEPT, 1 REVISE, 0 REJECT.

---

## §4 — alphaD (validated/invalidated ledger)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 benched-surface disambiguation + grammar-witness reality | ACCEPT | Doc→skinny path table; JSON-witnessed-only generality disclosure; `normalize`-absent disclosed. 24-count + lines 112-135 grep-verified (:128,:130). |
| §1 validated (V1-V6) | ACCEPT | Each SHA (`ea8138056`,`4de419f5e`,`2a85bf240`,`8153236e8`,`1c5bd7a25`) + measured evidence; V6 UNWIRED with grep evidence. |
| §2 invalidated (I1-I7) | ACCEPT | Each cites measured refutation; I1 dual core/skinny analogue; I7 N≥50 mandate. |
| §3 still-open (O1-O5) | ACCEPT | Owner paths skinny-keyed; O3-NEON hot-leaf %% tagged S-P1-re-confirm; O5 names `W5C_REQUEST_FACT_PROFILES` + 148-fn; falsifiable NO-GO thresholds bound. |
| §0/§5 CSS-row governing fact | ACCEPT | **V2 CH1-R1 FOLDED.** Lines 128-130 + 183-185 now say "24 `css_l4/*/direct_to_struct/main` … AUDIT-FALSIFIED, `skinny/RESULTS.md:112-135`, grep-verified count = 24." Conclusion (zero ADMITTED typed CSS rows) survives. |
| §4 demoted | ACCEPT | Pattern H / FNV / Decision Engine cited to `sk-v16/SYNTHESIS.md`. |

alphaD: 6 ACCEPT, 0 REVISE, 0 REJECT.

---

## §5 — alphaE (candidate shortlist)

| Section | Disposition | Rationale + fix |
|---|---|---|
| V2/V3 changelog | ACCEPT | C4 split, W5C retire-list, JSON-witnessed generality, 187→148, same-run-lightningcss-median folds accurate. |
| §0 ground-truth anchors | ACCEPT | Every anchor reviewer-verified (sheets_witness 25 LOC, digit_mac orphan `udot`, dispatch.rs:42/101, i8mm=0, 148-fn). Architecture-doc translation correction load-bearing + EXACT. |
| C0 de-fact-stream | ACCEPT | Skinny paths; retire-list `W5C_REQUEST_FACT_PROFILES`; gate measurable (8-field EXACT + typed-not-String + N≥50 + profile-const-deleted grep); scalar-ref N/A justified. |
| C1 tape wiring | ACCEPT | Skinny paths; ≥30 PASS / <20 NO-GO measurable; borrowed-slice-vs-lazy ENTRY gate; generality EXIT gate = Lock-14 binding. |
| C2 NEON pre-scan | ACCEPT | `select_classifier`/`PrimitiveKernels`/`lo6_table_admissible` cited; scalar-ref + checkasm present; S-P1-re-confirm folded; ≥80 gate measurable. |
| C3 commit-by-construction | ACCEPT | Skinny paths; ≥300 / >same-run-lightningcss / <200 NO-GO measurable; non-deposition-PROVEN-at-codegen gate = correct CH5 framing. |
| C4a udot orphan | ACCEPT | `digit_mac.rs:27,40` orphan + scalar twin `:15-22`; checkasm REQUIRED; admits unconditionally; same-wave consumer = CSS number leaf. |
| C4b net-new i8mm (GATED) | ACCEPT | i8mm grep-clean-absent verified; hard ENTRY gate + honest-residual EXIT = correct no-paper-close; scalar-ref + checkasm REQUIRED. |
| §2 dependency order | ACCEPT | DAG measurable; C0+C1 coupling explicit. |
| §3 cross-cutting discipline | ACCEPT | N≥50-median, benched-corpus-set, comparator-plane, 8-field-EXACT, witnessed-generality, no-relocated-overfit, no-paper-close — each measurable. |
| §4 escalation note | ACCEPT | C0-unmeasurable → §8 BLOCKED; borrowed-slice-vs-lazy elevated; hot-leaf-%% S-P1-re-confirm stated. Correct PASS-ALPHA §8. |

alphaE: 11 ACCEPT, 0 REVISE, 0 REJECT.

---

## §6 — SYNTHESIS.md (αF contract)

| Section | Disposition | Rationale + fix |
|---|---|---|
| header + benched-surface note | ACCEPT | Skinny owner paths named; core symbols grep-clean-absent + verified-binding; totality tree marked SK-V18 fold target. Changelog (:8) records the V2 6→24 + `:776` folds. |
| Authority | ACCEPT | Files cited; G-Omega-only gate posture. |
| §0.1 close condition | ACCEPT | Every surface citation skinny-tree; Tape/Layout/preserve-rich-ast/equality/>SOTA/tailwind/telemetry gates individually grep-verifiable + measurable. |
| §0.2 starting state | ACCEPT | **V2 CH1-R1 FOLDED.** Lines 139-146: "The 24 `css_l4/*/direct_to_struct/main` rows present in `skinny/RESULTS.md` (lines 112-135, grep-verified … = 24) … these 24 falsified broadcast diagnostics." Count + range now correct (verified). lightningcss run-dependence disclosed + bound to same-run N≥50 median. Conclusion (zero admitted typed CSS rows) survives. |
| §0.3 receiver goalset | ACCEPT | Receiver obligations target skinny paths; totality paths marked SK-V18 fold (no-edit); `W5C_REQUEST_FACT_PROFILES` DELETE replaces the non-existent template. |
| §0.4 pre-blocks | ACCEPT | Faithful to αC; 24-row broadcast pre-block (:202-204) says 24 / lines 112-135; generality clause witness-honest. |
| §0.5 per-corpus close | ACCEPT | Corpus set `{bootstrap,tailwindcss,material-components-web,animate}`; `normalize`-absent; inferred αB endpoints UNMEASURED-PENDING + no-wave-gate-key; "animate OR bootstrap" criterion. Correct §4.1 form. |
| §0.6 strict comparator gate | ACCEPT | **V2 CH1-R2 FOLDED.** Line 110 cites `assert_lightningcss_strict_equality` defined `nonjson_css_l4.rs:776` (call sites `:1057,:3460`) + disambiguates harness `benches/nonjson_css_l4.rs:8`. Verified EXACT. Plane mapping (lightningcss=fair, cssparser=flaw-probe) correct. |
| Section 1 ledger | ACCEPT | A-series 454/735/496 cited `3b8b757d` with normalize-A-series-only caveat. |
| Section 2 telemetry | ACCEPT | CSS schema (sample_count≥50, median, cold, full-cssom plane, equality-before-speed, rich-ast, `tape_activated` "NOT satisfiable by a grep in crates/core/", `w5c_profile_array_retired`, `simd_non_json_exercise`) measurable + gate-bindable; `--skv17-css-sota-report` consumer + broadcast tripwire specified. |
| Section 3 trajectory | ACCEPT | Four-lever route skinny-translated; hot-leaf %% S-P1-re-confirm folded; "animate OR bootstrap" consistent with §0.5. |

SYNTHESIS: 11 ACCEPT, 0 REVISE, 0 REJECT. Both V2 REVISEs resolved.

---

## §7 — HANDOFF.md (αF packet)

| Section | Disposition | Rationale + fix |
|---|---|---|
| Benched-substrate disclosure | ACCEPT | Skinny tape tree + fact-stream String + `W5C_REQUEST_FACT_PROFILES`; core symbols grep-clean-absent SK-V18 fold target. |
| Current State | ACCEPT | **V2 CH1-R1 FOLDED.** Lines 46-52: "It also holds 24 `css_l4/*/direct_to_struct/main` rows (lines 112-135, grep-verified … = 24) … these 24 falsified broadcast diagnostics." Internally consistent with :158-160 ("24-row broadcast … the 24 falsified … RESULTS rows, lines 112-135"). Count + range correct (verified). lightningcss run-dependence disclosed. |
| What SK-V17 Opens | ACCEPT | Gating artefact + four-lever route name skinny codegen surface; hot-leaf %% S-P1-re-confirm folded. |
| Authority / Gate Posture | ACCEPT | Files cited; G-Omega-only; CH7 disclosed as pass-added extension lens beyond §3W canon. |
| Pre-Blocked Routes | ACCEPT | Faithful to αC + SYNTHESIS §0.4; 24-row broadcast (:158-160) says 24; no second-substrate / Lock-1 escapes carried. |
| Next Move | ACCEPT | S-P1 re-profile obligation; skinny wave-surface naming; `tape_activated`-not-by-core-grep clause; C4a-unconditional/C4b-gated; "animate OR bootstrap" close criterion. |
| Close criterion / revert-deferral | ACCEPT | "animate OR bootstrap"; revert/hard-cap/triumvirate sanctioned-deferred to S-P3 per §4.4. |

HANDOFF: 7 ACCEPT, 0 REVISE, 0 REJECT.

---

## Consolidated CH1 disposition (cycle V3)

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| alphaA | 7 | 1 | 0 |
| alphaB | 9 | 0 | 0 |
| alphaC | 8 | 1 | 0 |
| alphaD | 6 | 0 | 0 |
| alphaE | 11 | 0 | 0 |
| SYNTHESIS | 11 | 0 | 0 |
| HANDOFF | 7 | 0 | 0 |
| **TOTAL** | **59** | **2** | **0** |

ACCEPT rate = 59/61 = **96.7%**. Above the §3Z ≥95% convergence bar. Both V2 REVISEs
(CH1-R1 count 6→24, CH1-R2 line :8→:776) are fully folded and ACCEPT. Two NEW minor REVISE
defects surfaced in the V3 fold (V3-CH1-a stale reconciliation note in αA, V3-CH1-b
mislabelled 25th grep hit in αC). Both are residue of the count-correction fold itself, are
count-accuracy / metadata REVISEs (not structural), and do not touch any load-bearing
conclusion (zero admitted typed CSS rows; 24-row broadcast PERMANENT PRE-BLOCK; lightningcss
fair-comparator plane). Neither is an orphan REVISE: both name path:line + concrete fix and
fold trivially in V4.

## The load-bearing CH1 findings (REVISE for V4)

- **V3-CH1-a (REVISE × 1 — αA §2 / lines 142-146):** the cross-artefact reconciliation note
  is now STALE. It claims αC/αD/SYNTHESIS/HANDOFF "all undercount … as '6' … MUST be corrected
  to 24 … in the V3 fold" and that αA "is the ONLY one … that states the correct count" — both
  false at V3 (verified: all four now state 24, lines 112-135). Fix: delete lines 142-146 or
  rewrite as "All cohort artefacts state 24 / lines 112-135 as of V3; the V2 undercount is
  resolved." Ground-truth at :140-141 stays.

- **V3-CH1-b (REVISE × 1 — αC §4 / line 229):** "`grep -c 'css_l4/.*/direct_to_struct/main'`
  = 24 broadcast rows **+ 1 distinct W6 typed row at :154**" mislabels the 25th grep substring
  hit. `skinny/RESULTS.md:154` is a prose REDRESS-127 companion reference, not a table row and
  not a W6 typed row; `grep 'W6.*css\|tape.*direct_to_struct' skinny/RESULTS.md` = EMPTY (no
  admitted/distinct W6 typed CSS row exists). The mislabel weakly contradicts the artefact's own
  conclusion. Fix at :229: "= 25 substring matches, of which 24 are `^| css_l4/` table rows
  (lines 112-135) and the 25th (:154) is a prose REDRESS-127 companion reference, not a row;
  there is NO admitted/distinct W6 typed CSS row in RESULTS.md." Classification + basis note
  unaffected.

## Plane / measurability / citation verdicts (CH1's three mandates)

**Competitor-plane verdict: PASS.** Every artefact computes the >SOTA delta against
lightningcss full-CSSOM (the materializing comparator; `plane=css_l4_full_parse,strictness=
strict,source=lightningcss-1.0.0-alpha.71:StyleSheet::parse`, verified on RESULTS rows 112-135)
and demotes cssparser (`2362.037`) to a plane-disclosed flaw probe (αB §0/§3/§4, SYNTHESIS
§0.6, αE §3). αB's `[INF]` in-table marking + UNMEASURED-PENDING + §6.2 prohibition on keying
wave gates to inferred per-corpus endpoints is the model CH1 wants. lightningcss baseline-NUMBER
dispersion (793/833/929/974) disclosed and resolved by binding the gate to the same-run N≥50
median rather than any frozen literal. No plane dishonesty.

**Falsifiability-gate verdict: PASS.** Every αE candidate (C0-C4b) carries a measurable
PASS/NO-GO threshold (8-field EXACT, ≥30/≥80/≥300 Mbps, grep-verifiable retirement of
`W5C_REQUEST_FACT_PROFILES`, `tape_activated`-not-by-core-grep). SYNTHESIS §0.1 gates are
grep-verifiable on `skinny/crates/`; the telemetry schema (§4.3) is emitted verbatim with the
broadcast tripwire.

**Citation verdict: PASS (with 2 minor REVISEs).** Every substantive claim cites a RESULTS row
/ REDRESS entry / commit SHA / measurement file, reviewer-re-verified at HEAD `1c5bd7a25`. The
two REVISEs are localized metadata defects (a stale meta-note; a grep-substring mislabel), not
uncited substantive claims — neither triggers a CH1 REJECT.
