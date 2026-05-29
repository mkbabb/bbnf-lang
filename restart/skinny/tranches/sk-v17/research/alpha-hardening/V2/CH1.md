# CH1 CORRECTNESS — Pass Alpha SK-V17 (cycle V2)

Lens: CH1 Correctness (PASS-ALPHA §3 + ORCHESTRATOR §3W). Adversarial review of
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA,alphaB,alphaC,alphaD,alphaE}.md`
+ `SYNTHESIS.md` + `HANDOFF.md` (cycle V2, folding the V1 CHALLENGE dispositions).

CH1 focus: (a) every claim cites a RESULTS row / REDRESS entry / commit SHA / measurement
file; (b) falsifiability gates are measurable; (c) competitor deltas computed on the correct
(materializing) plane vs lightningcss. CH1/CH6 REJECT any uncited claim.

Host discipline verified: HEAD `1c5bd7a25` (`git log --oneline -1` confirmed →
`feat(sk-v16-W6-tape): add shared flat-tape runtime substrate`). Reviewer verified every
load-bearing path/line/grep claim directly against the worktree.

---

## Verification performed (reviewer evidence, this cycle)

1. `git log --oneline -1` → `1c5bd7a25`. ✓ All artefact HEAD anchors correct.
2. Wrong-tree symbol grep over `skinny/crates/`: `StructLayout` = **0**, `OpenFrame` = **0**,
   `CssArena` = **0**, `TapeStructBuilder` = **0**. ✓ The V1 CH1-R1 translation correction is
   now load-bearing-honoured by every V2 artefact (αE §0, αA §0 disambiguation, αC §0 table,
   αD §0 table, SYNTHESIS benched-surface note, HANDOFF benched-substrate disclosure).
3. Benched tape tree: `skinny/crates/runtime/src/tape/` = `assembler.rs` (`TapeBuilder` :42,
   `push_plain_offset` :71), `mod.rs`, `event_grammar.rs`, `offsets.rs`. Core tape tree
   `crates/core/src/runtime/tape/` = `record/arena/cursor/mod`. ✓ Two distinct trees; the V2
   artefacts cite the skinny one as the owner path and the core one as the SK-V18 fold target.
4. `RuntimeEmitterKind` enum `grammar_provider.rs:40-42` `{CompiledLowering, RequestFacts}`;
   selection `lib.rs:282` (`CompiledLowering`, JSON) / `lib.rs:291` (`RequestFacts`, CSS). ✓
5. `track1_facts(input) -> Result<String,String>` at `nonjson_css_l4.rs:596`. ✓ Benched CSS
   Track 1 is a String, not typed (αE C0).
6. `W5C_REQUEST_FACT_PROFILES` const at `codegen/src/lib.rs:336` (declared :299, iterated
   :567, :611); 7 `RuntimeEmitterKind::RequestFacts` registrations in `regen_css.rs` (lines
   45,63,81,99,117,135,153). ✓ The new V2 retire-target citations are EXACT.
7. Fixture count: `grep -c 'fn parse_' generated_real_typed.rs` = **148** (NOT 187). ✓ The
   V2 187→148 correction is correct and uniformly applied.
8. `BackendShape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` at
   `ir/cost.rs:119-131,258-269`; `lower/{tape_plan,offset_tape,event_tape,eager_tape,
   collapsed_stage,sink_only}.rs` all exist. ✓ The skinny layout-equivalent citations hold.
9. Lock 2 RETIRES `StructLayout` (LOCKS.md:160, "the IR record is `Layout`"). ✓ αC §0/§2b
   citation EXACT.
10. `sheets_witness/` = 25 LOC total (`event_grammar_witness.rs` 24 + `mod.rs` 1); only `json`
    and `sheets_witness` carry an `event_grammar_witness.rs`. ✓ The V2 "JSON-witnessed-only /
    Sheets is a 25-line stub / BBNF-self absent" generality scoping is EXACT and verified.
11. `digit_mac.rs`: `parse_4_digits_dotprod` :27 with `udot` :40, `dot4_i8` :53 with `sdot`
    :63; `i8mm` grep over `skinny/crates/` = **0**. ✓ C4a orphan + C4b net-new framing correct.
12. `dispatch.rs`: `select_classifier` :42, `lo6_table_admissible` :101, `PrimitiveKernels`
    :50, `scalar::classify_chunk` :21. ✓ Neutrality-vehicle citations EXACT.
13. CSS corpus `css_l4_corpus.rs:23,31,39,47` = `{bootstrap, tailwindcss,
    material-components-web, animate}`; `normalize` grep over the bench crate = **0**. ✓ The
    V1 CH1-R6 normalize-corpus defect is corrected everywhere; tranche-success now reads
    "animate OR bootstrap" (SYNTHESIS §0.5:256, HANDOFF :203).
14. JSON guard deltas: twitter/parse_only `8349.290`/sonic `4913.095` = **+69.9%** EXACT;
    citm/real_typed `20512.601`/sonic `12662.292` = **+62.0%** EXACT (each tuple appears 5×
    in RESULTS.md). ✓ αA/αB/αD JSON-guard figures verified.
15. **RESULTS.md CSS row count (the V2 defect — see CH1-R1):** `grep -c '^| css_l4/.*/
    direct_to_struct/main '` = **24** (24 distinct grammar features: at_rules_keyframes …
    vendor_prefixes), spanning lines **112-135**, each carrying the identical broadcast tuple
    `track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281`. This is the
    "24-row broadcast" (one tuple × 24 conceptual row-ids) the audits falsified. **It is NOT
    "6 rows"** — several artefacts (and V1 CH1 itself) say "6"; that is wrong.
16. `assert_lightningcss_strict_equality` is defined at `nonjson_css_l4.rs:776` (call sites
    1057, 3460), NOT `:8` (SYNTHESIS §0.6 / αE cite `:8` — wrong line, see CH1-R2).

---

## §1 — alphaA (results extraction)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 standing + benched-surface disambiguation | ACCEPT | 0/24 admitted; benched-surface translation (lines 46-60) names the skinny tree correctly (`RequestFacts`, `track1_facts`, `runtime/src/tape/`); grep-clean core symbols disclosed. The V1 CH1-R3 lever-seed note is folded. |
| §1 canonical bench table | ACCEPT | 70/974/2539 disclosed as contract canonical; committed single-sample 793/2529/69.668 vs 60.96 build-run dispersion honestly flagged (`w6tape-report.md:42-47`); N≥50-median binding stated. Same-run-median gate keying correctly resolved (lines 99-101). |
| §2 per-corpus structure (CH1-R2 fold) | **REVISE** | **CH1-R1 (count).** The V1 CH1-R2 "zero CSS rows" defect is correctly fixed — but the replacement text says "**24** `css_l4/*/direct_to_struct/main` rows" (lines 124, 126), which is CORRECT and verified. αA is the ONLY artefact that gets the count right (24). However line 132 then narrows to "the 24-row broadcast that the V1/V2 overfit audits FALSIFIED (one wave's number broadcast across 24 rows)" — consistent. **αA itself is correct on the count; the REVISE here is only to add a one-line cross-artefact reconciliation note** flagging that αC/αD/SYNTHESIS/HANDOFF undercount as "6" so the divergence is visible to the consumer. (Strictly αA §2 is ACCEPT-grade; tagged REVISE to carry the reconciliation obligation.) |
| §3 8-field equality | ACCEPT | 10136/9561/9561/20043, errors=0, `shared_summary_equal=true`, cited to three reports + test path. The core-tree-witness vs skinny-benched-equivalent distinction (lines 154-159) is the correct CH5-honest framing. |
| §4 20x checkpoint | ACCEPT | 14.2x/15.6x/20x-vs-3.1-fragment all cited `:83-89`; SHA `8153236e8`; soundness + watermark-divergence argument cited. Benched-surface note (lines 210-211: core-tree builder, skinny retarget to `TapeBuilder` marker) folded. |
| §5 sub-wave ledger | ACCEPT | Each row carries SHA + report line; single-sample-cold caveat honest; skinny-tree substrate note in the W6-tape row (lines 271-277). |
| §6 banked wins + V6 substrate citation | ACCEPT | Provenance per row; V6 substrate re-cited to skinny `runtime/src/tape/` (`TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena`), with `grep TapeStructBuilder skinny/ = EMPTY` disclosed (lines 292-301). V1 CH1-R3 fully folded. |
| §7 goalset seed | ACCEPT | Lever sequence framed against the skinny benched surface (lines 326-348); same-run-median close threshold (lines 318-324); neutrality vehicle = `select_classifier`/`lo6_table_admissible` (`dispatch.rs:42,101`). V1 CH1-R3 folded. |
| §8 citation ledger | ACCEPT | Comprehensive; the normalize-A-series-only note (lines 380-381) and benched-surface translation citation present. |

alphaA: 7 ACCEPT, 1 REVISE, 0 REJECT.

---

## §2 — alphaB (competitor deltas)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 plane taxonomy | ACCEPT | Materializing-vs-token-scan = correct CSS analogue of sonic-rs `utf8_lossy`; lightningcss=fair bar, cssparser=flaw probe. Cited `w6-speed-report.md:55-60`. CH1 "correct plane" mandate satisfied. |
| §0.1 benched-substrate disclosure | ACCEPT | New V2 section; correctly states the benched Track 1 is a fact-stream String today, the "typed CSSOM" plane is the *intended* SK-V17 subject, and the gap arithmetic uses parse-throughput canonicals not String-serialization cost. Honest plane separation. |
| §1 canonical baseline | ACCEPT | HEAD verified; corpus bytes cited; the 3.09/70/13-15 dispersion = harness inadequacy (not architecture contradiction) cited `nonjson_css_l4.rs:1134`-equivalent + `:60`. `normalize` ABSENT disclosed. |
| §2 per-corpus delta vs lightningcss (CH1-R4 fold) | ACCEPT | The V1 CH1-R4 in-table inferred-marking defect is FIXED: every inferred cell carries `[INF]`/`[RNG][INF assign]`/`[AGG]` inline (lines 147-153), the marker legend (lines 144-146) defines them, and only the aggregate row is `[AGG cited]`. material `~60 [INF est. mid-low]` is explicitly marked doubly-inferred. UNMEASURED-PENDING + §6 wave-gate prohibition present. Exactly the CH1-required visual marking. |
| §3 per-corpus delta vs cssparser (CH1-R4 fold) | ACCEPT | Same `[INF]` inline marking applied (lines 184-187); reasoning (cssparser plane-mismatched, ~36x aggregate cited, NOT a SOTA bar) correct and `w6-speed-report.md:102/58`-cited. |
| §4 inter-comparator relation | ACCEPT | lightningcss ~38% of cssparser (~2.6-3x) = materialization tax; cross-checked 793/2529=0.314, 833/2476=0.336 — cited, not inferred. Correctly framed as why lightningcss is the fair bar. |
| §5 JSON guard | ACCEPT | twitter +69.9% / citm +62.0% verified EXACT against RESULTS.md. |
| §6 findings feed | ACCEPT | Each finding restates a cited result; the per-corpus UNMEASURED-PENDING + no-inferred-endpoint-gate mandate (§6.2) is the correct CH1 posture and binds downstream waves. |
| Verification ledger | ACCEPT | INFERRED items enumerated (lines 318-323); benched-tree provenance cited. |

alphaB: 9 ACCEPT, 0 REVISE, 0 REJECT. The V1 CH1-R4 inferred-marking defect is fully resolved.

---

## §3 — alphaC (REDRESS digest)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 framing + tree-disambiguation table | ACCEPT | The core-tree-vs-skinny mapping table (lines 27-34) is the strongest translation artefact in the cohort: each concept maps doc-symbol → skinny benched surface with verified paths (`W5C_REQUEST_FACT_PROFILES` lib.rs:336, the 7 `RequestFacts` regen_css.rs registrations, `BackendShape` ir/cost.rs, Lock 2 retirement). Reviewer re-verified each. |
| §1 AZ-IV eager value tree | ACCEPT | 118x (canada 1.83ms→215.7ms) cited `cb14970f` + `sk-v16-arch:46-66`; re-open test skinny-keyed (`runtime/src/grammars/css_l4_*/` + `nonjson_css_l4.rs:596-624`); telemetry binding = PayloadArena counters (REDRESS 8). |
| §2 StructRegistry/Arena/Builder + §2b layout | ACCEPT | 28-65x/983x/10583x WATCHDOG cited; SPLIT (indirection=permanent, layout=admit-under-framing) re-keyed to skinny `BackendRule`/`LayoutFacts` + Lock 2 canonical-name note. Re-open test names the `W5C_REQUEST_FACT_PROFILES` relocation trap. |
| §3 fact-stream String | ACCEPT | `emit_fact_stream` `generated.rs:5,61`; ~34% self-time; `track1_facts` String `:596`; the retirement-clause re-open test (the 7 `RequestFacts` registrations + `W5C_REQUEST_FACT_PROFILES` must retire) is the correct CH5 anti-second-substrate gate. |
| §4 24-row broadcast (CH1-R1 count) | **REVISE** | **CH1-R1.** The §4 header (line 211) and the row-4 ledger (line 342) both say "**24**-row broadcast … one tuple ×24 row-ids … `W8_SELECTED_CSS_ROWS=24`" — CORRECT and verified. BUT line 224 says "These **6** falsified tuples are still present in `skinny/RESULTS.md`" and line 342 says "**6** falsified rows still present in `skinny/RESULTS.md`". This is internally self-contradictory (24-row broadcast but "6 falsified rows") and **factually wrong**: there are **24** `css_l4/*/direct_to_struct/main` rows in RESULTS.md (lines 112-135), each carrying the identical tuple — verified `grep -c` = 24. Fix: replace both "6" with "24" (e.g. "These 24 falsified tuples are still present … 24 falsified rows still present"). The classification (PERMANENT PRE-BLOCK) and re-open test are unaffected. |
| §5 FNV/fixture | ACCEPT | 148-fn correction applied (line 268, "the architecture doc's 187 … is stale"); both contrivances cited; classification correct. |
| §6 x86/AVX | ACCEPT | aarch64-only; no-SVE cited `:265-266`; x86 sites `REDRESS.md:465-468`; Lock 16 cited. |
| §7 consolidated ledger | ACCEPT (except row-4 "6" — folded into CH1-R1) | Each row maps to its § with measured refutation; all re-open tests skinny-keyed. Row-4 carries the same "6" defect (CH1-R1). |
| §8 single distinction + lock anchors | ACCEPT | The admit/pre-block line is the correct CH3 anchor; Lock 1/2/8/14/16 anchors cited with LOCKS.md lines. |

alphaC: 8 ACCEPT, 1 REVISE, 0 REJECT.

---

## §4 — alphaD (validated/invalidated ledger)

| Section | Disposition | Rationale + fix |
|---|---|---|
| §0 benched-surface disambiguation + grammar-witness reality | ACCEPT | The V1 CH1-R5 defect is FIXED: the §0 table (lines 30-39) maps every doc symbol to its verified skinny path; the grammar-witness disclosure (lines 53-59, "tape-generality demonstrated today is JSON-witnessed only; Sheets/BBNF generality is by-construction, not by-exercise") is a NEW honesty improvement that strengthens CH1/CH2. `normalize`-absent disclosed (lines 46-51). |
| §1 validated (V1-V6) | ACCEPT | Each carries SHA (`ea8138056`, `4de419f5e`, `2a85bf240`, `8153236e8`, `1c5bd7a25`) + measured evidence; V6 flags UNWIRED with grep evidence + skinny module-name verification. |
| §2 invalidated (I1-I7) | ACCEPT | I1-I7 each cite measured refutation; I1 reframed for both core (eager) and skinny (fact-stream) analogues; I6 timeline correction; I7 N≥50 mandate. |
| §3 still-open (O1-O5, CH1-R5 fold) | ACCEPT | All owner paths re-keyed to the skinny benched surface (the V1 CH1-R5 defect); O3-NEON hot-leaf %% correctly tagged "core-tree profile … MUST be re-confirmed on the benched skinny path (S-P1 re-profile)"; O5 names `W5C_REQUEST_FACT_PROFILES` + 148-fn + the `css_l4.toml` as TOTALITY-fold-only. Falsifiable NO-GO thresholds bound (lines 136-140). |
| §0 CSS-row governing fact | **REVISE** | **CH1-R1.** Lines 122-124 and 177 say "the only CSS rows present are **6** `css_l4/*/direct_to_struct/main` W8R broadcast diagnostics … `skinny/RESULTS.md:112`". Same undercount as αC: the actual count is **24** (verified). Fix: replace "6" with "24" at lines 122-124 and 177. (Section 5 line 177 likewise.) Conclusion — zero ADMITTED typed CSS rows — survives. |
| §4 demoted | ACCEPT | Pattern H / FNV / Decision Engine cited to `sk-v16/SYNTHESIS.md`. |
| §5 ledger text | ACCEPT (except "6" — folded into CH1-R1) | Ordered spine O5→O1+O2→O3→O4 correct; carries the same "6" at line 177-178 (CH1-R1). |

alphaD: 5 ACCEPT, 1 REVISE, 0 REJECT.

---

## §5 — alphaE (candidate shortlist)

| Section | Disposition | Rationale + fix |
|---|---|---|
| V2 changelog | ACCEPT | The C4 split, W5C retire-list, JSON-witnessed generality, 187→148, same-run-lightningcss-median folds are each accurate restatements of the V1 dispositions. |
| §0 ground-truth anchors | ACCEPT | Every anchor cited + reviewer-verified (sheets_witness 25 LOC, digit_mac orphan udot, dispatch.rs:42/101, i8mm=0, 148-fn). The architecture-doc translation correction (lines 71-95) is the load-bearing CH1-R1 anchor the whole cohort inherits — verified EXACT. |
| C0 de-fact-stream | ACCEPT | Skinny paths; retire-list names `W5C_REQUEST_FACT_PROFILES`; falsifiability gate measurable (8-field EXACT + typed-not-String + N≥50 + profile-const-deleted grep); scalar-ref N/A correctly justified. |
| C1 tape wiring | ACCEPT | Skinny paths verified; ≥30 PASS / <20 NO-GO measurable; entry-gate (borrowed-slice-vs-lazy decision, `w6tape-conversion-report.md:67`) correctly elevated to ENTRY gate; generality EXIT gate (emit `sheets_witness` view OR scope to JSON+CSS) is the correct Lock-14 binding. |
| C2 NEON pre-scan | ACCEPT | `select_classifier`/`PrimitiveKernels`/`lo6_table_admissible` cited; scalar-ref present; checkasm tests named + verified present; the core-tree-profile-%% S-P1-re-confirm obligation is folded; ≥80 gate measurable. |
| C3 commit-by-construction | ACCEPT | Skinny paths; ≥300 / >same-run-lightningcss-plausible / <200 NO-GO measurable; the "non-deposition PROVEN at codegen, not heuristic" gate is the correct CH5 framing; S-P1-re-confirm tag on the ~31% figure. |
| C4a udot orphan | ACCEPT | `digit_mac.rs:27,40` orphan + scalar twin `:15-22` cited; checkasm REQUIRED; admits unconditionally; same-wave consumer = CSS number leaf. |
| C4b net-new i8mm (GATED) | ACCEPT | i8mm grep-clean-absent verified; hard ENTRY gate (Wave-5 re-profile proves digit leaf top-N tailwind) + honest-residual EXIT (no fabricated cross) = correct CH6/no-paper-close posture; scalar-ref + checkasm REQUIRED. |
| §2 dependency order | ACCEPT | DAG measurable; C0+C1 coupling + entry gates explicit. |
| §3 cross-cutting discipline | ACCEPT | N≥50-median, benched-corpus-set, comparator-plane, 8-field-EXACT, witnessed-generality, no-relocated-overfit, no-paper-close — each is a measurable binding. |
| §4 escalation note | ACCEPT | C0-unmeasurable → §8 BLOCKED; borrowed-slice-vs-lazy decision elevated to goalset-authoring time; hot-leaf-%% S-P1-re-confirm obligation stated. Correct PASS-ALPHA §8 application. |

alphaE: 11 ACCEPT, 0 REVISE, 0 REJECT. The shortlist is correctly bracketed, fully measurable, and the cohort's translation anchor.

---

## §6 — SYNTHESIS.md (αF contract)

| Section | Disposition | Rationale + fix |
|---|---|---|
| header + benched-surface note | ACCEPT | **CH1-R1 (the V1 central REJECT) is RESOLVED.** The benched-surface note (lines 21-58) names every skinny owner path, declares the core symbols grep-clean-absent + verified-binding, and explicitly marks the totality tree the SK-V18 fold target. Reviewer re-verified grep=0. |
| Authority | ACCEPT | Files cited; G-Omega-only gate posture stated per the active pin. |
| §0.1 close condition (V1 REJECT ×2 fold) | ACCEPT | **The V1 §0.1 REJECT is RESOLVED.** Every surface citation is now skinny-tree: Tape activation gates on `skinny/crates/runtime/src/tape/` + a non-zero grep over `runtime/src/grammars/css_l4_*/` + PayloadArena counters; Layout-driven projection gates on `skinny/crates/codegen/` + `lower/{tape_plan,offset_tape,event_tape}.rs` + the `W5C_REQUEST_FACT_PROFILES` retirement; preserve-rich-ast cites skinny value types. Each gate is grep-verifiable on `skinny/crates/`. Equality/rich-ast/>SOTA/tailwind/telemetry gates individually measurable. |
| §0.2 starting state | **REVISE** | **CH1-R1 (count).** Lines 135-140: "The **6** `css_l4/*/direct_to_struct/main` rows present in `skinny/RESULTS.md` (lines **112-118**) … these **6** falsified broadcast diagnostics." Both the count and the line range are wrong: there are **24** such rows spanning **112-135** (verified `grep -c` = 24). Fix: "The 24 `css_l4/*/direct_to_struct/main` rows present in `skinny/RESULTS.md` (lines 112-135) … these 24 falsified broadcast diagnostics." The lightningcss-baseline reconciliation (lines 114-121, V1 REVISE) is correctly resolved — the run-dependent dispersion is disclosed and the gate bound to the same-run N≥50 median. Conclusion (zero admitted typed CSS rows) survives. |
| §0.3 receiver goalset (V1 REJECT fold) | ACCEPT | **The V1 §0.3 REJECT is RESOLVED.** Every receiver obligation targets skinny paths (`skinny/crates/codegen/`, `grammar_provider.rs`, `lower/`, `assembler.rs:42,71`, `runtime_generator.rs:17-25`); the preamble (lines 145-149) explicitly marks the totality paths as SK-V18 fold target a receiver must NOT edit. The `W5C_REQUEST_FACT_PROFILES` DELETE replaces the non-existent `OpenFrame` template the V1 draft wrongly directed. |
| §0.4 pre-blocks | ACCEPT | Faithful to αC; all six families + the `W5C_REQUEST_FACT_PROFILES` retirement + hidden-coupling escapes + the generality clause (witness-honest, JSON+CSS-only unless sheets_witness exercised) carried. The "24-row broadcast" pre-block (line 184) correctly says 24. |
| §0.5 per-corpus close (V1 CH1-R6 fold) | ACCEPT | **The V1 CH1-R6 normalize defect is RESOLVED.** The corpus set is fixed to `{bootstrap, tailwindcss, material-components-web, animate}`, `normalize`-absent disclosed, the per-corpus table names only benched corpora, the inferred αB endpoints are tagged UNMEASURED-PENDING with no-wave-gate-may-key (lines 241-247), and the tranche-success criterion reads "animate OR bootstrap" (line 256). Correct PASS-ALPHA §4.1 form. |
| §0.6 strict comparator gate | **REVISE** | **CH1-R2.** Line 106 (Telemetry-honesty gate) and the §0.6 prose cite "`nonjson_css_l4.rs:8` `assert_lightningcss_strict_equality`". The function is defined at `nonjson_css_l4.rs:776` (call sites 1057, 3460), NOT `:8` (verified). Fix: change `:8` to `:776` (or drop the line number). The substantive claim (the fact-stream-equality comparator is retired; the comparator must build CSSOM) is correct; only the line citation is wrong. The plane mapping (lightningcss=fair, cssparser=flaw-probe) is correct. |
| Section 1 ledger | ACCEPT | Consistent with αD; A-series 454/735/496 cited `3b8b757d` with the normalize-A-series-only caveat (line 301-303) so it does not leak into the per-corpus gate. |
| Section 2 telemetry | ACCEPT | The CSS schema columns (sample_count≥50, median, cold, full-cssom plane, equality-before-speed, rich-ast, tape_activated with the explicit "NOT satisfiable by a grep in `crates/core/`" clause, `w5c_profile_array_retired`, `simd_non_json_exercise` naming a grammar not a bare bool) are measurable + gate-bindable. The `--skv17-css-sota-report` consumer + the broadcast/single-tuple tripwire are specified. |
| Section 3 trajectory (V1 CH1-R6 fold) | ACCEPT | The four-lever route is skinny-tree-translated; the hot-leaf %% S-P1-re-confirm obligation is folded; the "animate OR bootstrap" criterion is consistent with §0.5. |

SYNTHESIS: 8 ACCEPT, 2 REVISE, 0 REJECT. Both V1 REJECTs resolved.

---

## §7 — HANDOFF.md (αF packet)

| Section | Disposition | Rationale + fix |
|---|---|---|
| Benched-substrate disclosure | ACCEPT | The V1 Current-State wrong-tree-provenance REVISE is RESOLVED: lines 7-18 name the skinny tape tree + the fact-stream String + `W5C_REQUEST_FACT_PROFILES`, and mark the core symbols grep-clean-absent SK-V18 fold target. |
| Current State | **REVISE** | **CH1-R1 (count).** Lines 46-49: "It also holds **6** `css_l4/*/direct_to_struct/main` rows (lines **112-118**) … these **6** falsified broadcast diagnostics." Count and range wrong (24 rows, lines 112-135). Internally inconsistent with this same file's line 139 "The **24**-row broadcast measurement (one timing tuple → N conceptual admits; the source of the **6** falsified RESULTS rows)" — 24-row broadcast but "6 falsified rows". Fix: replace "6" with "24" at lines 46-49 and 140; the line range to 112-135. Otherwise Current State is correctly skinny-tree-keyed; the lightningcss run-dependence disclosure (lines 39-42) is correct. |
| What SK-V17 Opens | ACCEPT | The V1 REVISE is RESOLVED: the gating artefact + four-lever route name the skinny codegen surface (`skinny/crates/codegen/`, `assembler.rs:42,71`, `codegen/src/lib.rs:336`, `bbnf-simd/src/dispatch.rs`); the hot-leaf %% S-P1-re-confirm tag (lines 72-76) folds the V1 actual-profiling REVISE. |
| Authority / Gate Posture | ACCEPT | Files cited; G-Omega-only posture stated; CH7 correctly disclosed as a pass-added extension lens beyond the CH1-CH6 §3W canon (not elevated to the mandatory set). |
| Pre-Blocked Routes | ACCEPT | Faithful to αC + SYNTHESIS §0.4; the "24-row broadcast" pre-block (line 139) correctly says 24 (but then "6 falsified RESULTS rows" — the CH1-R1 inconsistency, folded above). No second substrate / Lock-1 escapes carried. |
| Next Move | ACCEPT | The V1 REVISE is RESOLVED: the S-P1 re-profile obligation (lines 170-174), the skinny wave-surface naming (W1-W5, lines 177-188), the `tape_activated`-not-by-core-grep clause (lines 193-196), C4a-unconditional/C4b-gated, and the "animate OR bootstrap" close criterion (lines 203-205) are all correct and skinny-keyed. |
| Close criterion / revert-deferral | ACCEPT | "animate OR bootstrap" (CH1-R6 fold); revert/hard-cap/triumvirate sanctioned-deferred to S-P3 per PASS-ALPHA §4.4. |

HANDOFF: 6 ACCEPT, 1 REVISE, 0 REJECT.

---

## Consolidated CH1 disposition (cycle V2)

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| alphaA | 7 | 1 | 0 |
| alphaB | 9 | 0 | 0 |
| alphaC | 8 | 1 | 0 |
| alphaD | 5 | 1 | 0 |
| alphaE | 11 | 0 | 0 |
| SYNTHESIS | 8 | 2 | 0 |
| HANDOFF | 6 | 1 | 0 |
| **TOTAL** | **54** | **6** | **0** |

ACCEPT rate = 54/60 = **90.0%**. Below the §3Z ≥95% convergence bar; one more fold (V3) required for the count defect. Both V1 REJECTs are resolved; all five V1 REVISE families (CH1-R2 zero-CSS-rows, CH1-R3/R5 tree-citation, CH1-R4 inferred-marking, CH1-R6 normalize) are folded and ACCEPT.

## The load-bearing CH1 findings (orphan-REVISE blockers for V3)

- **CH1-R1 (REVISE × 5 — the single propagated defect: αA-reconcile, αC §4/§7, αD §0/§5, SYNTHESIS §0.2, HANDOFF Current-State):** the count of falsified `css_l4/*/direct_to_struct/main` broadcast rows in `skinny/RESULTS.md` is **24** (24 distinct grammar features, lines 112-135, each carrying `2319.041/2362.037/929.281`), NOT "6". Reviewer-verified `grep -c '^| css_l4/.*/direct_to_struct/main '` = 24. The "6" is wrong wherever it appears as the present-row count; it traces to V1 CH1's own erroneous "6" (V1 CH1 lines 25,36,73) and propagated into the V2 fold. It is internally self-contradictory in αC (line 211 "24-row" vs 224 "6 falsified") and HANDOFF (line 139 "24-row broadcast" vs 140 "6 falsified RESULTS rows"). αA gets it right (24); αB/αE are correct (αE does not state a present-row count). The SYNTHESIS/HANDOFF line range "112-118" is also wrong (correct: 112-135). **Fix: replace "6" with "24" and "112-118" with "112-135" at every cited site.** The downstream conclusion (zero ADMITTED typed CSS rows; the 24 are diagnostic-only PRE-BLOCK, never a baseline) is UNAFFECTED — this is a count-accuracy REVISE, not a structural REJECT. But it is a CH1 correctness defect because the broadcast count is the load-bearing falsification figure (saying "6" understates the contrivance being pre-blocked), so it blocks the ≥95% bar.

- **CH1-R2 (REVISE × 1 — SYNTHESIS §0.6 / line 106):** `assert_lightningcss_strict_equality` is at `nonjson_css_l4.rs:776`, not `:8` (verified; call sites 1057, 3460). Fix the line citation to `:776` or drop it. Substantive claim correct.

Competitor-plane verdict (CH1's third mandate): **PASS.** Every artefact computes the >SOTA delta against lightningcss full-CSSOM (the materializing comparator) and explicitly demotes cssparser to a plane-disclosed flaw probe (αB §0/§4, SYNTHESIS §0.6, αE §3). αB's V2 in-table `[INF]` marking + UNMEASURED-PENDING + the §6.2 prohibition on keying wave gates to inferred per-corpus endpoints is the model CH1 wants. The lightningcss baseline-NUMBER dispersion (793/833/929/974) is disclosed everywhere and correctly resolved by binding the gate to the same-run N≥50 median rather than any frozen literal (SYNTHESIS §0.2/§0.5, αA §1, αE §0). No plane dishonesty remains.

Benched-plane verdict (CH1-R1 from V1): **PASS.** The wrong-tree REJECTs that dominated V1 are fully resolved — every owner path, close-condition gate, receiver obligation, and pre-block in SYNTHESIS/HANDOFF cites the skinny benched tree (grep-verifiable on `skinny/crates/`), with the totality tree explicitly demoted to the SK-V18 fold target. The `tape_activated`-not-by-core-grep telemetry clause (SYNTHESIS Section 2, HANDOFF Next-Move) closes the wrong-tree-satisfiability loophole at the gate level.
