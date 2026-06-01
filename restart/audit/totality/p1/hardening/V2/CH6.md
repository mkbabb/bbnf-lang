---
agent: CH6
pass: T-P1-totality-excavation
cycle: V2 (SK-V18 challenge cycle; inventories are the V5/SK-V18 fold, post-V1-CHALLENGE micro-fold)
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
verification_head: 4e4aa0648 (dirty tree; matches 1E self-claimed point)
inputs_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 CH6 (:130-:135)
  - restart/audit/totality/p1/hardening/V1/CH6.md (V1 lens carry-forward + 5 fold directives)
  - restart/audit/totality/p1/1A-substrate-evidence.md (V5; 1A-SUB-016 CH6-F2 downgrade folded)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V5)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V5; U2 CH6-F4 rewording folded)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V5; F1 :2463 repoint + J-3/G-9 CH6-F3 caveat folded)
  - restart/audit/totality/p1/1E-locks-evidence.md (V5; L08 CH6-F3 caveat folded)
  - restart/audit/totality/p1/1F-anti-pattern.md (V5)
  - restart/audit/totality/p1/1F-coherence-scan.md (V5)
  - restart/audit/totality/p1/1F-past-corpora.md (V5)
  - restart/ARCHITECTURE.md (read-only spec surface) §9/§9.1/§9.2/§7.3/§12
  - restart/MASTER-PLAN.md (read-only spec surface)
  - restart/locks/LOCKS.md (read-only spec surface) 16 locks + Lock-14 :620 clause
spot_verified_live_paths:
  - runtime_generator.rs:701 CSS_GENERATED_RS / :1611 close / :16-26 emitter dispatch — CONFIRMED
  - grammar_provider.rs:40-42 RuntimeEmitterKind{CompiledLowering,RequestFacts} + :110 CSS-exempt — CONFIRMED
  - tape/mod.rs:175 ValueRef<…K=AnyKind,G:EventGrammar=AnyGrammar> + :178 _kind/:179 _grammar PhantomData + :227 DocumentView — CONFIRMED
  - md5 css_l4_*/generated.rs ×7 = b654562c… distinct=1 — CONFIRMED
  - crates/ir/src/registry/strategy.rs:134-155 PRODUCTION_MANIFEST_TABLE grammar-named idents — CONFIRMED
  - lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS (strict, no leak surfaces) / :2442 SKV15_W2_EXTRA (holds runtime_generator/grammar_provider/json_sink_direct/json_typed_direct/json_templates) / :2463 ("crates/bbnf-simd/src/x86_64","diagnostic-x86") — CONFIRMED (F1 repoint correct)
  - css_l4_declaration_values/config.rs rg W7_|BackendShape|substrate_target = 0 (F2 gap) — CONFIRMED
  - json/config.rs:22-26 full W7 triad — CONFIRMED
  - json/generated.rs:12-15 attach_structural_index NO-OP (let _ = state) / :760-767 parse_direct W7 debug-asserts / parse_w11_1_number ×7 — CONFIRMED
  - json_sink_direct.rs:138-150 fixed-literal {[",-tfn dispatch incl. parse_w11_1_number_direct at :147 — CONFIRMED
  - ARCH §9:2215-2219 wide leak-scan command but "30 sites/15 files" attributed to 4 runtime dirs + self-cite "per 1C-runtime-evidence:125" — CONFIRMED (F4 circular-cite real)
  - live runtime-only leak scan = 12 sites in 4 parse_with.rs (json/css_l4/bbnf/google_sheets) — CONFIRMED
  - find crates/core/src/runtime -mindepth 2 -type f .rs = 71; @generated = 67; +4 = tape/{mod,cursor,arena,record}.rs — CONFIRMED
  - css_types.rs = 66 LOC, :1 "Host shims for the CSS L4 grammar's -> parse_hex_color(...)" — CONFIRMED
  - skinny x86 = 28 files / 4401 LOC (src/x86_64 + ext/x86) — CONFIRMED
  - crates/core/.../json.rs:701 OnceCell<StructuralIndex> / :719 ensure_structural_index / :732 scan_structural / support.rs:67 "The probe substrate" — CONFIRMED; skinny bbnf-simd next_structural_at_or_after = 0 — CONFIRMED
  - simd-scan/src/lib.rs:68 pub use next_structural_at_or_after — CONFIRMED
  - LOCKS.md 16 numbered lock headings at :75,:160,:170,:179,:181,:183,:200,:202,:260,:269,:319,:328,:336,:349,:436,:453 — CONFIRMED; :620 "G:EventGrammar … is the generality vehicle" — CONFIRMED
  - SYNTHESIS-AUDIT-OVERFIT.md:36 is prose assertion not a bench table; RESULTS.md:5/8/11 measured-rows twitter/citm/canada > sonic, per-iter PASS — CONFIRMED
  - HEAD = 4e4aa0648 (exists; matches 1E claim) — CONFIRMED
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

REVISE. The V2 inventory set is the post-V1-CHALLENGE micro-fold, and four of the
five V1 CH6 fold directives landed cleanly and are live-verified: F1 (the
`diagnostic-x86` line repoint — 1D now cites `:2463`, which on disk is exactly
`("crates/bbnf-simd/src/x86_64", "diagnostic-x86")`); F2 (1A-SUB-016 downgraded
to `partial / gap-routed`, `rg W7_|BackendShape css_l4 config = 0` confirmed,
routed to 1A-DIV-005 + 1A-UNK-004 + G5); F4 (1C U2 reworded — the ARCH 30/15
figure is now correctly framed as stale-within-scope + circular self-cite `per
1C-runtime-evidence:125`, with the verify_action kept and the "wider crates/
scan" hypothesis dropped); F5 (all eight inventories now key `cycle:
V5-SKV18-totality` with divergent self-labels carried as `cycle_self_label`). I
re-spot-verified ~25 of the most load-bearing rows against live code at HEAD
`4e4aa0648` and **every one resolved exactly** — the CSS const courier, the
emitter fork, the phantom `<G>` (with the K-axis preserved), the seven
byte-identical css_l4 md5 replicas, the totality `ir` grammar-named manifest
table, Pattern-H 71/67 (+4 = `tape/`), x86 28-file/4401-LOC, the
`parse_w11_1_number` ×7 leak, the fixed-literal sink dispatch, the green-by-
exclusion root partition (leak surfaces in `SKV15_W2_EXTRA`, NOT strict
`GENERIC_SCAN_ROOTS`), the OnceCell probe substrate, and the JSON cold RESULTS
rows. Citation accuracy remains ~95%+.

The blockers under MY lens are the **incomplete propagation of the V1 CH6 F3
fold** and one **surviving closure-word grade inversion of the same class F2
caught**. The F3 directive named four CSS "MEASUREMENT-VALID/PROVED" mirror
rows by ID (J-3, G-9, 1E L08, COH18-013); the caveat reached J-3/G-9/L08 but
COH18-013 was MISSED, and two sibling mirror sites (1D Spec-Claim table row 62,
1F-past-corpora A-table row 27) carry the same un-caveated doc-assertion-backed
closure word. And 1A-SUB-018 still grades a hand-written, explicitly-NOT-
generated JSON sink courier as `impl_exceeds_spec` against a "template-emitted"
spec claim its own evidence shows VIOLATED. No cross-inventory "deferred to a
later inventory" violation; no recalled/false/uncited claim → no REJECT.

## Findings

| # | disposition | target | evidence | fold directive |
|---|---|---|---|---|
| F1 | REVISE | F3 fold MISSED COH18-013 — the row it named by ID | V1 CH6 F3 directive (`restart/audit/totality/p1/hardening/V1/CH6.md:90`) reads "attach 1D's U-4 directional caveat inline to every CSS 'MEASUREMENT-VALID/PROVED' row (J-3, G-9, **1E L08, COH18-013**)". J-3 (`1D-skinny-lessons.md:181`), G-9 (`:199`), and L08 (`1E-locks-evidence.md:88`) all now carry the `(CH6-F3)` inline directional caveat — verified. But COH18-013 (`1F-coherence-scan.md:83`) still reads "CSS canonical cold N≥200 1.9-3.3× MEASUREMENT-VALID" with verdict `implemented (coherent)` / class `implemented` and **zero** directional/CH6-F3 caveat (grep for `CH6-F3|DIRECTIONAL|not re-locked|loadavg` in `:83` = empty). The cited source `SYNTHESIS-AUDIT-OVERFIT.md:36` is a prose assertion, NOT a bench table (live-verified: `:36` = "cold N=200 1.9–3.3×) — the residual is NOT a fake admit…"). A row F3 named by ID, still graded as a coherent closure on a doc-assertion CSS ratio, is a residual paper-close. | In COH18-013, attach the U-4 directional caveat inline (mirror L08's `(CH6-F3)` phrasing), cite the H1 `css_canon_bench` re-lock gate, and re-grade the CSS half `directional / not re-locked` while keeping the JSON 51/51 half `implemented (coherent)`. Split the two halves so the JSON coherence survives and the CSS ratio stops being a coherent close. |
| F2 | REVISE | 1D Spec-Claim table row 62 — un-caveated `impl_exceeds_spec` MEASUREMENT-VALID | The `>SOTA preserved HONESTLY` row (`1D-skinny-lessons.md:62`) cites `RESULTS.md:5-25` (JSON, real bench rows — confirmed twitter 8349>4913, citm 9079>8335, canada 16709>12970, per-iter PASS) AND `SYNTHESIS-AUDIT-OVERFIT.md:36-37` (CSS — a prose assertion), grades the WHOLE row `impl_exceeds_spec`, and the note says "CSS >SOTA is MEASUREMENT-VALID (NOT fake like SK-V13)" with NO directional caveat (grep `:62` for `CH6-F3|DIRECTIONAL|loadavg` = empty). This is the Spec-Claim-table mirror of the J-3 digest row that DID get the F3 fold; the un-caveated "MEASUREMENT-VALID" closure word on the CSS half survives here. The JSON half legitimately exceeds spec; the CSS half rests on a doc assertion the row's own G-9/U-4 siblings now flag as directional. | Split row 62: keep JSON `impl_exceeds_spec` (RESULTS.md bench-backed); re-grade the CSS half `directional / not re-locked` with the inline U-4 caveat (loadavg 4.35, H1 re-lock gate), and cite the `css_canon_bench` row rather than `SYNTHESIS-AUDIT-OVERFIT.md:36`. Do not let one un-folded mirror carry the closure word the fold retired elsewhere. |
| F3 | REVISE | 1A-SUB-018 credits a hand-written non-generated courier as `impl_exceeds_spec` | 1A-SUB-018 (`1A-substrate-evidence.md:83`) grades JSON `sink.rs` `impl_exceeds_spec` against ARCH:1944 "grammar runtime files are template-emitted." Live `sink.rs:1` = "JSON-owned direct sink source; **not part of the generated JSON roster**" and `generated.rs:748 use super::sink::JsonSink` — confirmed. The evidence shows the spec claim is VIOLATED (the file is hand-written, NOT template-emitted), yet the verdict is `impl_exceeds_spec`. This is the same closure-word inversion F2 caught at 1A-SUB-016: an opened/standing gap (hand-written courier R-A targets for retirement, feeding 1A-DIV-007) credited as the impl EXCEEDING spec. The note honestly routes it to R-A/G1, so it is milder than F2's, but the grade still papers a violation as an exceedance. | Downgrade 1A-SUB-018 from `impl_exceeds_spec` to `partial / gap-routed` matching 1A-DIV-007 (generated-runtime claim not closed). Keep "useful SinkOnly evidence" as a fact, not "exceeds spec." Do not count this row in `impl_exceeds_spec` (frontmatter `impl_exceeds_spec: 2` would drop to 1 once 1A-SUB-017 — the legitimate no-retained-sidecar exceedance — is the sole survivor). |
| F4 | REVISE | 1F-past-corpora A-table row 27 fixes the CSS headline as a settled fact, no directional caveat | Row 27 (`1F-past-corpora.md:27`) is titled "**SK-V18 headline >SOTA is MEASUREMENT-VALID**" under "## A — Already-Validated Facts (do NOT re-litigate as open)", binding implication "Do NOT re-derive CSS as 'audit-demoted/contrived'." It quotes `SYNTHESIS-AUDIT-OVERFIT.md:36` (the prose assertion) and carries NO directional caveat for the CSS half (grep `:27` = empty). Placing the CSS ratio in a do-NOT-re-litigate ledger row, sourced only to the synthesis doc that asserts it, is precisely the paper-close F3 targets: it forbids re-opening a CSS perf number that 1D's own U-4 says is "DIRECTIONAL, NOT re-locked." The JSON 51/51 half (row 28) IS bench-backed and correctly settled. | In row 27, split JSON (settled, `SPEC.md:184`/RESULTS.md-backed) from CSS (directional). For the CSS half add: "the 1.9–3.3× ratio is directional (loadavg 4.35), re-lock gate H1 `css_canon_bench` per 1D U-4 — do NOT re-derive as fake, but it is NOT yet a re-locked bench fact." A do-not-re-litigate ledger may not freeze a not-yet-re-locked number as validated. |
| F5 | ACCEPT | V1 F1/F2/F4/F5 folds correctly applied + live-verified | F1: 1D now cites `:2463` (live = `("crates/bbnf-simd/src/x86_64","diagnostic-x86")`) at the table row, D-7, and G-7; reconciled against 1E's `:2409`/`:2442`/`:2463` triad. F2: 1A-SUB-016 = `partial / gap-routed`, `rg W7_|BackendShape css_l4 config = 0` confirmed, routed to 1A-DIV-005/1A-UNK-004/G5. F4: 1C C3/U2 reworded to stale-within-scope + circular self-cite (ARCH `per 1C-runtime-evidence:125` live-confirmed), verify_action kept, "wider scan" hypothesis dropped. F5: all 8 inventories key `cycle: V5-SKV18-totality`, divergent self-labels carried as `cycle_self_label`. | Preserve all four folds. They discharge the V1 CH6 REVISE quartet at HEAD. |
| F6 | ACCEPT | CSS const courier / emitter fork / phantom `<G>` campaign anchors | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"` … `:1611 "#;`; `:16-26` match dispatch (`CompiledLowering => emit_from_source` / `RequestFacts => emit_request_facts`); `grammar_provider.rs:40-42 enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` + `:110` CSS-exempt; `tape/mod.rs:175 ValueRef<…K=AnyKind,G:EventGrammar=AnyGrammar>` with `:178 _kind` (REAL K axis) + `:179 _grammar: PhantomData<fn()->G>` (decorative). Each carries a live path:line and a named receiver wave (G1/G2/G3/G4). | Preserve. The campaign-anchor citations are exact; not paper-close. |
| F7 | ACCEPT | 7 css_l4 md5 replicas + ir grammar-named manifest table | `md5 css_l4_*/generated.rs` = `b654562c…` ×7, distinct=1. `crates/ir/src/registry/strategy.rs:134 PRODUCTION_MANIFEST_TABLE` with `idents:&["JsonParser","JsonGrammar"]` (:137), `["GoogleSheetsParser","GoogleSheetsGrammar"]` (:143), `["CssL4Parser"]` (:149), `["BbnfBootstrap","BbnfParser"]` (:155). COH18-005 / 1F-anti row / COH18-012 correctly flag this as the relocated-seam the codegen-scoped `:1643` leak scan misses (and that the wide `:2215`/`LOCKS.md:349` scan DOES catch). | Preserve. The relocated-seam finding and the §12 vs §9 gate-scope distinction are live-grounded and CH6-honest. |
| F8 | ACCEPT | Pattern-H 71/67 + 16-lock count + x86 28-file/4401-LOC | `find … runtime -mindepth 2 = 71`; `@generated = 67`; +4 = `tape/{mod,cursor,arena,record}.rs` (live-listed). 16 numbered locks resolve at every cited LOCKS line (1..16 headings confirmed; the "Lock 17" grep hit is "SK-V17" text, not a 17th lock). x86 = 28 files / 4401 LOC. The 71-vs-67 drift carries a verify_action (1E-V5-U3 `:136`, COH18-007 `:109`) and a +N-must-trace routing — CH6-compliant. | Preserve. The drift is routed, not closed. |
| F9 | ACCEPT | F4 1C U2 stale-spec rewording is a routed UNKNOWN, not a paper-close | 1C C3 (`:32`) + U2 (`:80`) now say the 30/15 is stale-within-scope (ARCH `:2217-2219` scopes it to the SAME 4 dirs the live 12/4 scan covers, AND self-cites `1C-runtime-evidence:125` — both live-confirmed). U2 keeps the verify_action: "re-run `rg … crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/` and reconcile … if they still disagree at the same scope, the §9 figure is stale." This is the anti-paper-close discipline: a routed UNKNOWN with a falsifier, not a closed conjecture. | Preserve verbatim. This is the exemplary discharge of a V1 CH6 REVISE. |
| F10 | ACCEPT | OnceCell probe "now CLASSIFIED, not UNKNOWN" does NOT paper-close substrate-union | 1F-anti `:43` + 1E `:158` classify the totality `OnceCell<StructuralIndex>` probe as `generated_function`/`per-parse &mut ScanState, NOT cross-call` (live-verified: `json.rs:711 OnceCell::new()` per `ScanState::new`, threaded `&mut`), the ADMISSIBLE class per `LOCKS.md:139-149`. Crucially it does NOT close: it carries an explicit fence "do NOT close substrate-union 'BOTH trees' while this is unclassified" + an SK-V19 verify_action scanning `OnceCell<StructuralIndex>|ensure_structural_index|scan_structural|next_structural_at_or_after` over `crates/core/src`, and confirms skinny `bbnf-simd` = 0. The classification is bounded ("per-parse scratch, NOT a proven violation"), not a closure word. | Preserve. The classification + SK-V19 carry + fence is CH6-aligned: a bounded grade with a live falsifier, never a "BOTH trees clean" close. |
| F11 | ACCEPT | UNKNOWN rows carry verify_action across all 8 inventories | 1A 1A-UNK-001..005 (`:179-:183`) each carry an `rg`/manifest action; 1B U1-U3 (`:145-:160`); 1C U1-U4 (`:79-:82`); 1D U-1..U-5 (`:207-:237`); 1E 1E-V5-U1..U3 (`:134-:136`); 1F-coherence U-COH18-001/002 (`:116-:117`). No bare UNKNOWN without a falsifier. | Preserve every verify_action; do not collapse into generic "future work". |
| F12 | ACCEPT | No cross-inventory "deferred to a later inventory" violation | A scan for `later inventory` / `deferred to 1[A-F]` / `covered by 1X` returned only benign intra-file pointers and sanctioned wave/disposition routing (P1-P5 / G1-G6 / PROVE / H1 / T-P3 / Pass Omega / SK-V19 adoption). No divergence is parked on a sibling inventory; every divergence routes to a named WAVE or an explicit T-P3/Omega disposition. | Preserve. The wave/disposition routing is CH6-compliant escalation, not a paper-close deferral. |
| F13 | ACCEPT | CSS headline source-nature honesty (where folded) + JSON bench-row model | Where the F3 fold landed (J-3 `:181`, L08 `1E:88`), the inventory now correctly distinguishes the CSS doc-assertion source (`SYNTHESIS-AUDIT-OVERFIT.md:36`, live-confirmed prose) from the JSON bench-row model (`RESULTS.md:5/8/11`, live-confirmed `measured-row` + `per-iter equality PASS`). J-1 remains the standard the CSS rows should meet. The defect is propagation completeness (F1/F2/F4), not the model itself. | Preserve J-3/L08/G-9 as the template; propagate to the three un-folded mirrors per F1/F2/F4. |

## Fold Directives

1. Fold `CH6-V2-COH18-013-CAVEAT`: attach the U-4 directional caveat inline to COH18-013 (`1F-coherence-scan.md:83`) — the F3-named row the V1 fold missed; split the JSON-coherent half from the CSS-directional half; cite the H1 `css_canon_bench` re-lock gate, not `SYNTHESIS-AUDIT-OVERFIT.md:36`.
2. Fold `CH6-V2-1D-ROW62-CAVEAT`: split 1D Spec-Claim row 62 (`1D-skinny-lessons.md:62`) — keep JSON `impl_exceeds_spec` (RESULTS.md-backed), re-grade the CSS half `directional / not re-locked` with the inline U-4 caveat.
3. Fold `CH6-V2-1A-SUB-018-GRADE`: downgrade 1A-SUB-018 (`1A-substrate-evidence.md:83`) from `impl_exceeds_spec` to `partial / gap-routed` matching 1A-DIV-007; do not credit a hand-written non-generated courier as the impl exceeding a "template-emitted" claim; drop the frontmatter `impl_exceeds_spec` count from 2 to 1.
4. Fold `CH6-V2-PASTCORPORA-ROW27-CAVEAT`: in 1F-past-corpora A-table row 27 (`1F-past-corpora.md:27`), split JSON (settled) from CSS (directional); a do-not-re-litigate ledger may not freeze the not-yet-re-locked 1.9–3.3× CSS ratio as a validated fact.

No REJECT is warranted: every load-bearing cited path:line resolved on live code at
HEAD `4e4aa0648`, the V1 CH6 F1/F2/F4/F5 folds are correctly discharged, and the
residual defects are an incompletely-propagated F3 caveat (three mirror sites) plus
one surviving closure-word grade inversion of the F2 class — bounded wording/grading
issues, not evidence that the T-P1 inventory set is structurally unusable.

TALLY accept=9 revise=4 reject=0
