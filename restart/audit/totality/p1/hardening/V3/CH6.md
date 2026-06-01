---
agent: CH6
pass: T-P1-totality-excavation
cycle: V3 (SK-V18 challenge cycle; inventories are the V5/SK-V18 fold, post-V1+V2-CHALLENGE micro-folds)
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-06-01T00:00:00Z
verification_head: 4e4aa0648 (dirty tree; matches V2 CH6 self-claimed point; inventories dirty per V1/V2 fold)
inputs_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3 CH6 (:130-:135)
  - restart/audit/totality/p1/hardening/V1/CH6.md (V1 lens; 5 fold directives)
  - restart/audit/totality/p1/hardening/V2/CH6.md (V2 lens; 4 fold directives)
  - restart/audit/totality/p1/1A-substrate-evidence.md (V5-SKV18; 1A-SUB-016 + 1A-SUB-018 downgrades folded)
  - restart/audit/totality/p1/1B-codegen-evidence.md (V5-SKV18)
  - restart/audit/totality/p1/1C-runtime-evidence.md (V5-SKV18; U2 rewording folded)
  - restart/audit/totality/p1/1D-skinny-lessons.md (V5-SKV18; J-3/G-9/row62 caveats folded)
  - restart/audit/totality/p1/1E-locks-evidence.md (V5-SKV18; L08 caveat folded)
  - restart/audit/totality/p1/1F-anti-pattern.md (V5-SKV18)
  - restart/audit/totality/p1/1F-coherence-scan.md (V5-SKV18; COH18-013 split folded)
  - restart/audit/totality/p1/1F-past-corpora.md (V5-SKV18; row27 split folded)
  - restart/ARCHITECTURE.md (read-only spec surface)
  - restart/MASTER-PLAN.md (read-only spec surface)
  - restart/locks/LOCKS.md (read-only spec surface) 16 locks
spot_verified_live_paths:
  - runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#" + :16-26 emitter dispatch (CompiledLowering=>emit_from_source / RequestFacts=>emit_request_facts) — CONFIRMED
  - grammar_provider.rs:40-42 enum RuntimeEmitterKind{CompiledLowering,RequestFacts} + :110 CSS-exempt (!= RequestFacts) — CONFIRMED
  - tape/mod.rs:175 ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar> + :178 _kind (real K) / :179 _grammar PhantomData (decorative G); :94 pub struct Tape<'input> — CONFIRMED
  - md5 css_l4_*/generated.rs ×7 = b654562ccff46ed62dd48e9ace325830 distinct=1 — CONFIRMED
  - crates/ir/src/registry/strategy.rs:134 PRODUCTION_MANIFEST_TABLE grammar-named idents (JsonParser/JsonGrammar :137, GoogleSheets… :143, CssL4Parser :149, BbnfBootstrap/BbnfParser :155) — CONFIRMED
  - skinny/crates/bbnf-bench/src/lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS / :2442 SKV15_W2_EXTRA_COVERAGE_ROOTS / :2463 ("crates/bbnf-simd/src/x86_64","diagnostic-x86") — CONFIRMED (V1 :2456 mis-cite fully reconciled to :2463; bare path corrected to skinny/crates/bbnf-bench/src/ per CH1-V2-F4)
  - json/generated.rs:12-15 attach_structural_index NO-OP (debug_assert_eq! + let _ = state) — CONFIRMED; json/sink.rs:1 "JSON-owned direct sink source; not part of the generated JSON roster" — CONFIRMED
  - RESULTS.md:5/8/11 measured-row twitter 8349.290>4913.095 / citm 9079.838>8335.772 / canada 16709.901>12970.929, per-iter equality PASS, simd Scalar aarch64 M5 Max — CONFIRMED
  - SYNTHESIS-AUDIT-OVERFIT.md:36 = prose assertion ("cold N=200 1.9–3.3×) — the residual is NOT a fake admit…"), NOT a bench table; :138 = A1 prose row — CONFIRMED
  - ir/src/lib.rs:340-346 enum BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage} (5 variants) — CONFIRMED
  - passes/src/lib.rs:329 mod recognizers / :392 fn derive_backend_shape — CONFIRMED; lower/mod.rs:18-26 select_lowering 5-arm match cost.chosen, zero grammar names — CONFIRMED
  - lower/tape_plan.rs:58 render_rule marker-string emitter — CONFIRMED (4 of 5 lowerers route to it; only SinkOnly real)
  - nonjson_css_l4.rs:3091 fn measure_mbps (warm micro-fixture path that did NOT produce the numbers) — CONFIRMED
  - crates/core/src/runtime/css_l4/builder.rs = 817 LOC, :16 enum OpenFrame (L13 >500 cap) — CONFIRMED
  - REDRESS.md:6356 "SK-V15 W8 EagerTape OffsetTape Lowerer Admit" / :6382 "SK-V15 W9 Remaining Lowerer All-Five Gate Admit" — CONFIRMED
  - sheets_witness/event_grammar_witness.rs = 24 LOC / mod.rs = 1 LOC (COH18-010) — CONFIRMED; codegen/src/json_templates/ exists (config/generated/parser/value/view/visitor.rs) (COH18-011) — CONFIRMED
  - find runtime -mindepth 2 .rs = 71; @generated = 67 (Pattern-H) — CONFIRMED
  - find bbnf-simd/src/x86_64 + ext/x86 = 28 files; bbnf-simd/src/lib.rs:5 pub mod x86_64 — CONFIRMED
  - 1D Spec-table row62 SPLIT (JSON impl_exceeds_spec / CSS directional); 1F-coherence COH18-013 SPLIT; 1A-SUB-018 downgraded to partial/gap-routed, frontmatter impl_exceeds_spec 2->1; 1F-past-corpora row27 SPLIT — CONFIRMED (V2 F1-F4 folds landed)
  - 1D Exec Summary :30-32 grep DIRECTIONAL|loadavg|not re-locked|CH6 = 0 (NO directional caveat) — CONFIRMED (NEW residual)
  - 1B frontmatter lacks generated_at; 1C generated_at = 2026-06-01 (date-only) vs six siblings 2026-06-01T00:00:00Z — CONFIRMED
---

## Lens Contract

CH6 rejects paper close: no inventory self-reports a divergence "resolved",
"wired", "implemented", "honoured", "proved", or "measurement-valid" without a
live-evidence citation (cargo-asm symbol, bench row, checkasm pass, REDRESS
admit, or a re-grounded path:line); no divergence is deferred to "a later
inventory"; every UNKNOWN carries a `verify_action`
(`restart/prompts/totality/PASS-1-EXCAVATION.md:130`-`:135`). The cycle expects
≥30% REVISE — an all-ACCEPT wave without close reading is itself paper-close.

## Verdict

REVISE. The inventory set is the post-V1+V2-CHALLENGE micro-fold at HEAD
`4e4aa0648` (inventories dirty with the applied folds). I re-spot-verified ~22 of
the most load-bearing cited rows against live code and **every single one
resolved exactly** — the CSS const courier (`runtime_generator.rs:701`), the
`RuntimeEmitterKind` fork (`grammar_provider.rs:40-42`), the phantom `<G>` with
the real K-axis preserved (`tape/mod.rs:175-179`), the seven byte-identical
css_l4 md5 replicas (`b654562c…`, distinct=1), the totality `ir` grammar-named
manifest table (`strategy.rs:134-155`), the lock14 triad (`:2409`/`:2442`/`:2463`
with the V1 `:2456` mis-cite now fully reconciled and the bare path corrected),
the 5-shape enum + `select_lowering` discriminator (`ir/lib.rs:340-346`,
`lower/mod.rs:18-26`) honestly split from the four marker-string lowering bodies,
the warm micro-fixture `measure_mbps` (`nonjson_css_l4.rs:3091`), the 817-LOC CSS
builder, Pattern-H 71/67, x86 28-file, and the JSON cold RESULTS rows. Citation
accuracy is ~95%+. All four V2 CH6 fold directives (COH18-013 split, 1D row62
split, 1A-SUB-018 downgrade with frontmatter `impl_exceeds_spec` 2→1, past-corpora
row27 split) **landed and are live-verified**, and all five V1 directives stay
discharged.

The blocker under MY lens is a **NEW, un-folded mirror** of exactly the closure
word the V1/V2 folds retired in six other sites: the **1D Executive Summary**
(`1D-skinny-lessons.md:30`) — the most-read paragraph of the inventory — still
states the CSS half flat as "the >SOTA is now MEASUREMENT-VALID (… CSS canonical
cold N≥200 beats lightningcss 1.9–3.3×)" with **zero** directional caveat (grep of
`:30-32` for `DIRECTIONAL|loadavg|not re-locked|CH6` = 0, live-confirmed). V1 F3
named J-3/G-9/L08/COH18-013; V2 caught row62 and past-corpora row27; **neither
cycle caught the headline summary line**. Three further residuals: a "RESOLVED"
closure word backed only by a "prior V4 packet" pointer (no live citation), a
verdict-token over-reach on a coherence-with-an-unexecuted-plan, and `generated_at`
frontmatter heterogeneity the V1 F5 `cycle:`-normalization left behind. No
cross-inventory "deferred to a later inventory" violation; every UNKNOWN carries
a verify_action; no recalled/false/uncited claim → **no REJECT**.

## Findings

| # | disposition | target | evidence | fold directive |
|---|---|---|---|---|
| F1 | REVISE | 1D Executive Summary :30 — un-folded un-caveated CSS "MEASUREMENT-VALID" headline mirror | `1D-skinny-lessons.md:30-31` states "the >SOTA is now MEASUREMENT-VALID (JSON 51/51 strict cold beats sonic-rs; CSS canonical cold N≥200 beats lightningcss 1.9–3.3×)" with NO directional caveat (`grep :30-32 DIRECTIONAL\|loadavg\|not re-locked\|CH6` = 0, live-confirmed). This is the SAME closure word the V1 F3 fold retired at J-3 (`:182`), G-9 (`:200`), L08 (`1E:88`), COH18-013 (`1F-coh:83`) and the V2 F2/F4 folds retired at Spec-table row62 (`:62`) and past-corpora row27 (`1F-past:27`). The headline Executive Summary — the most-read paragraph — is the only mirror in the inventory's own thesis statement; both prior cycles named the table/digest rows by ID but missed the summary prose. The CSS half rests on `SYNTHESIS-AUDIT-OVERFIT.md:36` (a prose assertion, live-confirmed NOT a bench table), and the same inventory's U-4 (`:228-237`) says the CSS ratios "ran under loadavg 4.35 and are DIRECTIONAL, NOT re-locked." A headline that asserts the CSS ratio flat while the body caveats it is residual paper-close. | In `1D-skinny-lessons.md:30-31`, split the parenthetical: keep "JSON 51/51 strict cold beats sonic-rs" (RESULTS.md-backed), and re-state the CSS half "CSS canonical cold beats lightningcss 1.9–3.3× — newly measurement-valid (NOT fake) but DIRECTIONAL, NOT yet re-locked (loadavg 4.35; H1 `css_canon_bench` re-lock gate per U-4)". Do not let the inventory's headline carry the un-caveated closure word its own body retired. |
| F2 | REVISE | 1F-past-corpora :76 — "RESOLVED" closure word backed only by "prior V4 packet" | The D-table row (`1F-past-corpora.md:76`) reads "SK-V15: CSS audit-demoted … | **prior V4 packet** | SK-V15 **RESOLVED** the measurement/comparator issues; SK-V18 inherits a measurement-valid CSS…". The closure word "RESOLVED" rests entirely on the pointer "prior V4 packet" — NOT a live path:line, bench row, or REDRESS admit. CH6 forbids "resolved" without a live-evidence citation; a cross-tranche history table may state the lesson, but "RESOLVED the measurement/comparator issues" asserts a closed state on a pointer-to-a-superseded-doc, and it is the load-bearing predicate the "inherits a measurement-valid CSS" claim (which F1 shows is still directional) depends on. Milder than F1 (a historical lesson, not an active divergence), but the word over-closes. | In `1F-past-corpora.md:76`, downgrade "RESOLVED" to a cited, bounded form: "SK-V15 ADDRESSED the broadcast/comparator contrivances (prior V4 COH-002/009); the CSS measurement is now directional-not-fake but NOT yet re-locked (per 1D U-4)". Cite the specific prior-packet COH IDs the claim rests on, not a bare "prior V4 packet", and do not carry "resolved" into the SK-V18 inheritance predicate. |
| F3 | REVISE | COH18-011 verdict token "implemented (coherent)" applied to an UNEXECUTED plan gate | `1F-coherence-scan.md:81` grades the ARCH single-plane pre-gate (`ARCHITECTURE.md:2007-2009`) "implemented (coherent)" / class "implemented", with the prose "SK-V18 G1 binds exactly this … the certified PLAN honors this lock." Live evidence is only that `codegen/src/json_templates/` exists (confirmed) — the G1 byte-equivalence gate is a PLANNED wave, NOT executed. The PROSE is honest ("certified plan honors"), but the verdict TOKEN "implemented" on a coherence-with-an-unexecuted-plan invites a reader to score G1 as run. Contrast COH18-010 (`:80`), where "implemented (coherent)" is correct because the live stub (24-LOC + 1-LOC, confirmed) ACTUALLY matches the ARCH figure. COH18-011 has no executed gate to anchor "implemented". | In `1F-coherence-scan.md:81`, change the COH18-011 verdict token from "implemented (coherent)" / "implemented" to "coherent (plan-bound)" / "plan-coherent", keeping the prose. The spec↔plan coherence is real; the GATE is not implemented. Reserve the "implemented" token for rows with an executed live surface (cf. COH18-010's live stub). |
| F4 | REVISE | `generated_at` frontmatter heterogeneity the V1 F5 normalization left behind | V1 F5 normalized `cycle:` to `V5-SKV18-totality` across all 8 inventories (confirmed) but `generated_at` remains divergent: **1B has NO `generated_at` field at all** (`1B-codegen-evidence.md:1-16` jumps `cycle_self_label` → `spec_surfaces_audited`); **1C carries `generated_at: 2026-06-01`** (date-only) while the other six carry `2026-06-01T00:00:00Z` (full ISO). A consolidator keying provenance/freshness to `generated_at` cannot uniformly order the set; an absent field on 1B is a silent provenance gap. Not a paper-close on a claim, but a frontmatter discipline defect of the same family V1 F5 began and did not finish. | Add `generated_at: 2026-06-01T00:00:00Z` to 1B's frontmatter; normalize 1C's to the full ISO timestamp. Have the V3 aggregator verify all 8 inventories carry an identical-format `generated_at`, closing the V1 F5 normalization that stopped at `cycle:`. |
| F5 | ACCEPT | V2 F1-F4 folds + V1 F1-F5 folds all landed and live-verified | V2 F1: COH18-013 (`1F-coh:83`) SPLIT — JSON 51/51 "implemented (coherent)" / CSS "directional, not re-locked", inline U-4 caveat + H1 `css_canon_bench` gate. V2 F2: 1D row62 (`:62`) SPLIT — JSON `impl_exceeds_spec` (RESULTS.md) / CSS directional. V2 F3: 1A-SUB-018 (`:90`) = `partial / gap-routed`, frontmatter `impl_exceeds_spec` 1 (dropped 2→1, `:28`). V2 F4: past-corpora row27 (`:27`) SPLIT — JSON settled / CSS directional. V1 F1 `:2463` repoint + bare-path correction, F2 1A-SUB-016 downgrade (`rg W7_\|BackendShape css_l4 config = 0` confirmed), F4 1C U2 stale-within-scope rewording, F5 `cycle:` normalization — all confirmed. | Preserve all nine folds. They discharge the V1+V2 CH6 REVISE roster at HEAD; F1-F4 above are the residuals those folds did not reach. |
| F6 | ACCEPT | CSS const courier / emitter fork / phantom `<G>` campaign anchors | `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`; `:16-26` match dispatch (`CompiledLowering => emit_from_source` / `RequestFacts => emit_request_facts`); `grammar_provider.rs:40-42 enum RuntimeEmitterKind{CompiledLowering,RequestFacts}` + `:110` CSS-exempt; `tape/mod.rs:175 ValueRef<…K=AnyKind,G:EventGrammar=AnyGrammar>` with `:178 _kind` (REAL K axis) + `:179 _grammar: PhantomData<fn()->G>` (decorative). Each carries a live path:line and a named receiver wave (G1/G2/G3/G4). | Preserve. The campaign-anchor citations are exact; not paper-close. |
| F7 | ACCEPT | 7 css_l4 md5 replicas + ir grammar-named manifest table | `md5 css_l4_*/generated.rs` = `b654562ccff46ed62dd48e9ace325830` ×7, distinct=1. `crates/ir/src/registry/strategy.rs:134 PRODUCTION_MANIFEST_TABLE` grammar-named idents at `:137`/`:143`/`:149`/`:155`. COH18-005 / 1F-anti / COH18-012 correctly flag this as the relocated-seam the codegen-scoped leak scan misses. | Preserve. Relocated-seam finding is live-grounded and CH6-honest. |
| F8 | ACCEPT | 1B 5-shape "canon whole / bodies scaffold" split is honest | `ir/lib.rs:340-346` = exactly 5 variants → graded IMPLEMENTED; `select_lowering` (`lower/mod.rs:18-26`) = 5-arm `match cost.chosen`, zero grammar names → IMPLEMENTED; but `eager/offset/event_tape.rs` + `collapsed_stage.rs` each route to the `tape_plan::render_rule` marker-string emitter (`lower/tape_plan.rs:58`) → graded UNIMPLEMENTED ("emits MARKER STRING, NOT Rust"), and the "5 scaffolds" row is `IMPL_EXCEEDS_SPEC` ONLY because "Spec NAMES them as scaffolds". This is the exemplary CH6 split: the discriminator is whole, the bodies are not closed, and the inventory refuses to credit the marker strings as real lowerers. | Preserve. The "canon whole / bodies scaffold" split is the model anti-paper-close grade. |
| F9 | ACCEPT | 1C C2 "markers ≠ generator output" refuses the @generated paper-close | 1C states "Marker present but FILES are still the hand-roster" and routes provenance to U1; 1A-SUB-015 (`:87`) grades the template-emitted claim `unimplemented` with `scan.rs`/`sink.rs` "not part of the generated JSON roster" (live-confirmed). A 67-count `@generated` header is correctly refused as executable closure evidence. | Preserve verbatim. The header-is-not-provenance discipline is exactly CH6-aligned. |
| F10 | ACCEPT | Pattern-H 71/67 + 16-lock + x86 28-file census | `find runtime -mindepth 2 .rs = 71`; `@generated = 67`; x86 = 28 (`src/x86_64` + `ext/x86`); `bbnf-simd/src/lib.rs:5 pub mod x86_64`. The 71-vs-67 drift carries a verify_action (1E-V5-U3, COH18-007). 16 numbered locks resolve at every cited LOCKS line. | Preserve. The drift is routed, not closed. |
| F11 | ACCEPT | UNKNOWN rows carry verify_action across all 8 inventories | 1A 1A-UNK-001..005 each carry an `rg`/manifest action (`:186-:188` confirmed); 1C U1-U4 (`:79-:82`) each carry a falsifier; 1B U1-U3, 1D U-1..U-5, 1E 1E-V5-U1..U3, 1F-anti / 1F-coherence UNKNOWN rows all carry verify_actions. No bare UNKNOWN. The OnceCell probe is CLASSIFIED (not UNKNOWN) but still fenced with an SK-V19 verify_action, not closed. | Preserve every verify_action; do not collapse into "future work". |
| F12 | ACCEPT | No cross-inventory "deferred to a later inventory" violation | A scan for `later inventory` / `deferred to 1[A-F]` / `covered by 1X` returned only benign intra-file pointers and sanctioned wave/disposition routing (P1-P5 / G1-G6 / PROVE / H1 / T-P3 / Pass Omega / SK-V19 adoption). No divergence is parked on a sibling inventory. The HANDOFF SK-V17→V18 boundary note (`1F-past:78`) explicitly routes the totality `crates/core/` adoption to SK-V19 as a NAMED tranche, not a paper-close deferral. | Preserve. The wave/disposition routing is CH6-compliant escalation. |
| F13 | ACCEPT | JSON >SOTA backed by a real bench table — the model the CSS rows must meet | J-1 / row62-JSON / COH18-013-JSON cite `RESULTS.md:5-25`; live-confirmed twitter 8349.290>4913.095, citm 9079.838>8335.772, canada 16709.901>12970.929, each a `measured-row` with `simd=Scalar; arch=aarch64; cpu=Apple M5 Max` and `per-iter equality PASS`. This is genuine bench evidence, NOT a doc self-report — the CH6 standard `SYNTHESIS-AUDIT-OVERFIT.md:36` (a prose assertion) fails to meet for the CSS half (F1). | Preserve. J-1 / the JSON halves are the bench-row template the CSS headline (F1) must be downgraded to match until `css_canon_bench` is re-locked. |

## Fold Directives

1. Fold `CH6-V3-1D-EXECSUMMARY-CAVEAT`: in `1D-skinny-lessons.md:30-31`, split the headline parenthetical — keep the JSON 51/51 half flat (RESULTS.md-backed), re-state the CSS half as "newly measurement-valid (NOT fake) but DIRECTIONAL, NOT yet re-locked (loadavg 4.35; H1 `css_canon_bench` gate per U-4)". This is the headline/thesis-statement mirror the V1 F3 + V2 F1/F2/F4 folds missed; the inventory's own Executive Summary may not carry the un-caveated closure word its body retired.
2. Fold `CH6-V3-PASTCORPORA-RESOLVED-DOWNGRADE`: in `1F-past-corpora.md:76`, downgrade "SK-V15 RESOLVED" to "SK-V15 ADDRESSED (prior V4 COH-002/009)"; cite the specific superseded-packet COH IDs, not a bare "prior V4 packet", and do not carry "resolved" into the "inherits a measurement-valid CSS" predicate F1 shows is still directional.
3. Fold `CH6-V3-COH18-011-TOKEN`: in `1F-coherence-scan.md:81`, change the COH18-011 verdict token from "implemented (coherent)" to "coherent (plan-bound)" — the spec↔plan coherence is real but the G1 gate is unexecuted; reserve "implemented" for rows with an executed live surface (cf. COH18-010's live 24/1-LOC stub).
4. Fold `CH6-V3-GENERATED-AT-NORMALIZE`: add `generated_at: 2026-06-01T00:00:00Z` to 1B's frontmatter and normalize 1C's date-only value to the full ISO timestamp; the V3 aggregator verifies all 8 inventories carry an identical-format `generated_at`, finishing the V1 F5 frontmatter normalization that stopped at `cycle:`.

No REJECT is warranted: every load-bearing cited path:line resolved on live code at
HEAD `4e4aa0648`, the V1 F1-F5 and V2 F1-F4 CH6 folds are correctly discharged, and
the residual defects are an un-caveated headline closure word, one pointer-backed
"RESOLVED", one verdict-token over-reach on an unexecuted plan gate, and
`generated_at` frontmatter heterogeneity — bounded wording/grading/freshness issues,
not evidence that the T-P1 inventory set is structurally unusable.

TALLY accept=9 revise=4 reject=0
