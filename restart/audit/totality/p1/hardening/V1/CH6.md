---
agent: CH6
pass: T-P1-totality-excavation
cycle: V1 (SK-V18 challenge cycle; inventories are the V5/SK-V18 fold)
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
inputs_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 CH6 (:130-:135)
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/ARCHITECTURE.md (read-only spec surface) §9/§9.1/§9.2/§7.3/§10.1
  - restart/MASTER-PLAN.md (read-only spec surface)
  - restart/locks/LOCKS.md (read-only spec surface) 16 locks
spot_verified_live_paths:
  - skinny/crates/codegen/src/runtime_generator.rs:701 (CSS_GENERATED_RS) — CONFIRMED
  - skinny/crates/codegen/src/grammar_provider.rs:40-42,110 (RuntimeEmitterKind) — CONFIRMED
  - skinny/crates/runtime/src/tape/mod.rs:175 (phantom <G>) — CONFIRMED
  - md5 css_l4_*/generated.rs ×7 = b654562c… — CONFIRMED distinct=1
  - crates/ir/src/registry/strategy.rs:134-155 (PRODUCTION_MANIFEST_TABLE idents) — CONFIRMED
  - find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' = 71; per-grammar 67 — CONFIRMED
  - rg @generated crates/core/src/runtime = 67 — CONFIRMED
  - LOCKS.md numbered locks 1..16 at :75,:160,:170,:179,:181,:183,:200,:202,:260,:269,:319,:328,:336,:349,:436,:453 — CONFIRMED
  - bbnf-simd src/x86_64 (24) + ext/x86 (4) = 28 — CONFIRMED
  - json/generated.rs parse_w11_1_number ×7 — CONFIRMED
  - json_sink_direct.rs render :4 + dispatch bytes :138-163 + render fns :124/:251/:326/:497 (&mut String) — CONFIRMED
  - lower/tape_plan.rs:58 render_rule + :65 runtime_plan:: marker — CONFIRMED
  - RESULTS.md:5,8,11 JSON twitter/citm/canada cold > sonic, simd=Scalar aarch64 — CONFIRMED
  - lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS / :2442 SKV15_W2_EXTRA / :2463 diagnostic-x86 — CONFIRMED
---

## Lens Contract

CH6 rejects paper close: no inventory self-reports a divergence "resolved",
"wired", "implemented", "honoured", "proved", or "measurement-valid" without a
live-evidence citation (cargo-asm symbol, bench row, checkasm pass, REDRESS
admit, or a re-grounded path:line); no divergence is deferred to "a later
inventory"; every UNKNOWN carries a `verify_action`
(`restart/prompts/totality/PASS-1-EXCAVATION.md:130`-`:135`). Cycle V1 expects
≥30% REVISE — an all-ACCEPT wave without close reading is itself paper-close.

## Verdict

REVISE. This is a strong, evidence-dense inventory set: I spot-verified the
~20 most load-bearing cited rows against live code and **every one resolved
exactly** — the CSS const courier (`runtime_generator.rs:701`), the
`RuntimeEmitterKind` fork (`grammar_provider.rs:40-42`), the phantom `<G>`
(`tape/mod.rs:175`), the seven byte-identical css_l4 replicas (md5
`b654562c…`, distinct count = 1), the totality `ir` grammar-named table
(`crates/ir/src/registry/strategy.rs:134-155`), Pattern-H 71/67, the 16-lock
roster, x86 28-file count, the `parse_w11_1_number ×7` leak, the fixed-literal
`json_sink_direct.rs` dispatch, the marker-string lowerers, and the JSON cold
RESULTS rows. Citation accuracy is ~95%+. The blockers under MY lens are
narrower paper-close defects: one mis-cited line repeated three times, one
"RESOLVED-BY-REMOVAL"/`impl_exceeds_spec` grading that credits an *opened gap*
as the impl exceeding spec, one closure word ("MEASUREMENT-VALID"/"PROVED") on
the CSS headline that rests on a synthesis-doc assertion and is contradicted by
the same inventory's own UNKNOWN, one UNKNOWN-conjecture that contradicts the
spec scope it cites, and heterogeneous cycle frontmatter. No cross-inventory
"deferred to a later inventory" violation was found.

## Findings

| # | disposition | target | evidence | fold directive |
|---|---|---|---|---|
| F1 | REVISE | 1D `diagnostic-x86` line mis-cited ×3 | 1D cites `lock14_baseline.rs:2456` for the `diagnostic-x86` exclusion in THREE places — the spec table row (`restart/audit/totality/p1/1D-skinny-lessons.md:63`), D-7 (`:101`), and G-7 (`:170`). Live `:2456` is `("crates/bbnf-simd/tests/checkasm_parity.rs", "strict-checkasm-admitted")`; the `diagnostic-x86` token is at `:2463`. The substantive claim (green-by-exclusion) is TRUE and 1E cites it correctly at `:2463` (`restart/audit/totality/p1/1E-locks-evidence.md:104`). This is a recalled/wrong LOC, not a false claim. | Repoint all three 1D citations from `:2456` to `:2463`; reconcile against 1E's correct `:2409`/`:2442`/`:2463` triad. |
| F2 | REVISE | 1A-SUB-016 closure word masks an opened gap | The row is graded `impl_exceeds_spec` with note "RESOLVED-BY-REMOVAL — but the substrate-target classification for the CSS fact stream now has ZERO config evidence (opposite-direction gap)" (`restart/audit/totality/p1/1A-substrate-evidence.md:79`). Live `rg -c 'W7_|BackendShape|substrate_target' css_l4_declaration_values/config.rs` = 0 (confirmed). An *absence of required classification* graded as the impl EXCEEDING spec is a paper-close inversion: the same note routes it to 1A-DIV-005 (`:99`), G5 (`:112`), and 1A-UNK-004 (`:177`) as a GAP. "RESOLVED-BY-REMOVAL" is a closure word applied to a removal that opened a gap. | Downgrade 1A-SUB-016 verdict from `impl_exceeds_spec` to `partial / gap-routed` matching 1A-DIV-005 + 1A-UNK-004. Do not count this row in `impl_exceeds_spec`. Keep "removed the mislabel" as a fact, not "resolved". |
| F3 | REVISE | CSS "MEASUREMENT-VALID/PROVED" rests on a synthesis-doc assertion + contradicted by 1D's own U-4 | 1D J-3 grades CSS "PROVED (newly measurement-valid) … beats lightningcss 1.9–3.3×" citing only `SYNTHESIS-AUDIT-OVERFIT.md:36-37` (`restart/audit/totality/p1/1D-skinny-lessons.md:154`), and G-9/exec-summary repeat it (`:42`,`:172`). The cited line is the synthesis doc *asserting* the number — NOT a bench-row table like RESULTS.md gives JSON (J-1 cites RESULTS.md:5-25, which I confirmed carries `simd=Scalar; cpu=Apple M5 Max` measured rows). 1D's OWN U-4 (`:199`-`:203`) then says the CSS ratios "ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked." 1E L08 (`restart/audit/totality/p1/1E-locks-evidence.md:87`) and COH18-013 (`restart/audit/totality/p1/1F-coherence-scan.md:83`) carry the same un-caveated "MEASUREMENT-VALID". CH6: a "proved/measurement-valid" perf claim needs a cited bench row, not the synthesis doc that states it. | In J-3/G-9/L08/COH18-013, attach the U-4 directional caveat INLINE and cite the actual `css_canon_bench` row (or the S-P1 ratio source), not `SYNTHESIS-AUDIT-OVERFIT.md:36`. Until a quiet re-capture row is cited, grade CSS >SOTA `directional / not-re-locked`, not `PROVED`. |
| F4 | REVISE | 1C U2 conjecture contradicts the spec scope it cites | 1C C3 (`restart/audit/totality/p1/1C-runtime-evidence.md:31`) + U2 (`:79`) explain the ARCH "30 sites across 15 files" vs the live runtime-only 12/4 (I confirmed: 12 sites in 4 `parse_with.rs`) by conjecturing "the 30/15 likely includes `__shape_support_*` emit sites in the generated grammar plane" / "a wider `crates/` scan." But ARCH §9:2217-2219 — the very text — scopes the figure to `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/` (the SAME 4 dirs) and cites back "per 1C-runtime-evidence:125" (a self-referential spec↔inventory loop). The conjecture is inconsistent with its own cited scope. | Reword U2: the 30/15 figure is scoped to the same 4 dirs in the cited ARCH line, so the gap is a stale spec count (or a circular self-citation), NOT a wider-scan artefact. Keep the verify_action; drop the contradicted "wider crates/ scan" hypothesis or label it explicitly unverified. |
| F5 | REVISE | Heterogeneous cycle frontmatter across the 8 inventories | The eight artifacts self-label with five different cycle strings: 1A `SK-V18`, 1B `SK-V18-TOTALITY-EXCAVATION`, 1C `TOTALITY-EXCAVATION (post SK-V18 …)`, 1D `SK-V18-totality`, 1E `V5`, 1F×3 `V5-SKV18-totality` (`restart/audit/totality/p1/1A-substrate-evidence.md:4`; `1B:6`; `1C:4`; `1D:4`; `1E:4`; `1F-anti-pattern.md:4`). A V1 CHALLENGE aggregator cannot key dispositions to a single cycle label. Not a paper-close on a claim, but a discipline defect that risks the fold mis-attributing rows. | Normalize all eight `cycle:` frontmatter to one agreed SK-V18 cycle tag before V2 consolidation, or have the aggregator pin the canonical cycle label and note the inputs' self-labels. |
| F6 | ACCEPT | CSS const courier / emitter fork / phantom `<G>` (1B/1C/1D/1E/1F) | Spot-verified live: `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`; `grammar_provider.rs:40-42 enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` + `:110` CSS exemption + `runtime_generator.rs:16-26` match dispatch; `tape/mod.rs:175 ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` with `_grammar: PhantomData<fn()->G>`, and the non-test instantiation census IS empty (`event_grammar_tests.rs:18,20 _proof_compiles::<JsonEventGrammar/SheetsEventGrammar>` are the only real-type sites). These are not paper-close: each carries a live path:line and a named receiver wave (G1/G2/G3/G4). | Preserve. These are the campaign-anchor divergences; the citations are exact. |
| F7 | ACCEPT | 7 css_l4 replicas md5 + ir grammar-named table | `md5 css_l4_*/generated.rs` = `b654562c…` ×7, distinct=1 (confirmed). `crates/ir/src/registry/strategy.rs:134 PRODUCTION_MANIFEST_TABLE` with `idents:&["JsonParser","JsonGrammar"]` (:137), `["GoogleSheetsParser","GoogleSheetsGrammar"]` (:143), `["CssL4Parser"]` (:149), `["BbnfBootstrap","BbnfParser"]` (:155) — exact. COH18-005 (`1F-coherence-scan.md:75`) / 1F-anti-pattern row (`:58`) correctly flag this as the relocated-seam analog the codegen-scoped leak scan misses. | Preserve. The relocated-seam finding is live-grounded and the gate-scope gap (COH18-012) is real. |
| F8 | ACCEPT | Pattern-H census + 16-lock count + x86 28-file | `find … runtime -mindepth 2 = 71`; per-grammar (excl. `tape/`) = 67; `rg @generated = 67`; `json/value.rs:1` "@generated by xtask regen-json" — all exact (1E D-1E-V5-06 `:106`, COH18-007 `:77`). The 16 numbered locks resolve at every cited LOCKS line (1E `:71`). x86 = 24 (`src/x86_64`) + 4 (`ext/x86`) = 28 (1E D-1E-V5-04 `:104`, 1D D-4 `:92`). | Preserve. The 71-vs-67 drift carries a verify_action (1E-V5-U3 `:132`, COH18-007 gap `:108`) per CH6. |
| F9 | ACCEPT | 1C C2 "markers do not equal generator output" | 1C correctly refuses the paper-close that 67 `@generated` headers prove generator provenance: C2 (`1C-runtime-evidence.md:30`) and D4 (`:53`) state "Marker present but FILES are still the hand-roster (markers do not make them generator output)" and route round-trip provenance to U1 (`:78`). This is the anti-paper-close discipline CH6 wants — a header is not executable close evidence. | Preserve verbatim. This is the exemplary CH6-aligned row in the set. |
| F10 | ACCEPT | Marker-string lowerers + fixed-literal sink (1B D2/D3) | `lower/tape_plan.rs:58 render_rule` emits the `runtime_plan::… ops=N` marker string at `:65`; `json_sink_direct.rs:4 render` then `render_value_dispatch/_container_rules/_string_rule/_utility_rules` at `:124/:251/:326/:497` all take `&mut String` only, with hardcoded `{[",-tfn` dispatch at `:138-163` — all confirmed (the `parse_w11_1_number_direct` leak even appears at `:147` here). 1B does not over-claim: the 5-shape canon is "whole as a discriminator" but bodies are scaffolds, and U1/U3 (`:142`,`:152`) flag the vacuity risk with verify_actions. | Preserve. The "canon whole / bodies scaffold" split is honest and live-grounded. |
| F11 | ACCEPT | JSON >SOTA backed by a real bench table | J-1 (`1D-skinny-lessons.md:152`) cites RESULTS.md:5-25; confirmed live: twitter 8349.290 > sonic 4913.095 (:5), citm 9079.838 > 8335.772 (:8), canada 16709.901 > 12970.929 (:11), each a `measured-row` with `simd=Scalar; arch=aarch64; cpu=Apple M5 Max` and `per-iter equality PASS`. This is genuine bench evidence, not a doc self-report — the CH6 standard the CSS row (F3) fails to meet. | Preserve. J-1 is the model the CSS rows should follow. |
| F12 | ACCEPT | UNKNOWN rows carry verify_action across all 8 inventories | 1A 1A-UNK-001..005 (`:174`-`:178`) each carry an `rg`/manifest verify_action; 1B U1-U3 (`:142`-`:157`); 1C U1-U4 (`:78`-`:81`); 1D U-1..U-5 (`:180`-`:208`); 1E 1E-V5-U1..U3 (`:130`-`:132`); 1F-anti-pattern UNKNOWN rows (`:43`); 1F-coherence U-COH18-001/002 (`:115`-`:116`). No bare UNKNOWN without an action. | Preserve every verify_action row; do not collapse into generic "future work". |
| F13 | ACCEPT | No cross-inventory "deferred to a later inventory" violation | A scan for `later inventory` / `deferred to 1[A-F]` / `covered by 1X` returned only the benign intra-file pointer "see 1A-DIV-005". Divergences route to named WAVES (P1-P5/G1-G6/PROVE/H1) and to T-P3/Pass-Omega disposition, which is the sanctioned escalation, not a paper-close deferral. | Preserve. The wave/disposition routing is CH6-compliant. |

## Fold Directives

1. Fold `CH6-V1-1D-XINTEL-LINE`: repoint 1D's three `lock14_baseline.rs:2456` citations to `:2463` (the live `diagnostic-x86` line); reconcile against 1E's correct triad.
2. Fold `CH6-V1-1A-SUB-016-GRADE`: downgrade 1A-SUB-016 from `impl_exceeds_spec`/"RESOLVED-BY-REMOVAL" to `partial / gap-routed`; do not credit an opened ZERO-evidence gap as the impl exceeding spec.
3. Fold `CH6-V1-CSS-MEASUREMENT-CAVEAT`: attach 1D's U-4 directional caveat inline to every CSS "MEASUREMENT-VALID/PROVED" row (J-3, G-9, 1E L08, COH18-013) and cite the actual `css_canon_bench` row, not `SYNTHESIS-AUDIT-OVERFIT.md:36`.
4. Fold `CH6-V1-1C-U2-SCOPE`: reword 1C U2 so its conjecture matches the spec scope it cites (the ARCH 30/15 figure is scoped to the same 4 runtime dirs and self-cites the inventory) rather than a contradicted "wider crates/ scan".
5. Fold `CH6-V1-CYCLE-FRONTMATTER`: normalize the five divergent `cycle:` labels across the 8 inventories (or have the aggregator pin the canonical SK-V18 cycle tag) so V2 dispositions key to one cycle.

No REJECT is warranted: I found no recalled/false/uncited claim — every load-bearing cited path:line resolved on live code. The defects are bounded wording/grading/line-reference/freshness issues, not evidence that the T-P1 inventory set is structurally unusable.

TALLY accept=8 revise=5 reject=0
