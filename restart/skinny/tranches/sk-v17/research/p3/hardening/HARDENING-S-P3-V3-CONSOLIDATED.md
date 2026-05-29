# SK-V17 S-P3 SYNTHESIS-PLAN — CHALLENGE CONSOLIDATION (V3)

Pass: S-P3 Synthesis-Plan. Cycle: V3 (final). Date: 2026-05-29.
Aggregator over `p3/hardening/V3/{CH1..CH7}.md`. Authority:
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §3 (CH1–CH6 + CH7) + §4 convergence;
`ORCHESTRATOR.md` §3W (six-lens CHALLENGE, monotonically extensible) / §3Z (≥95% × 2,
zero orphan REVISE, V ≤ 5, wave count ≤ 12).
Subject: `restart/skinny/tranches/sk-v17/research/p3/{p3a-candidate-shortlist,
p3b-wave-sequencing, p3c-falsifiability-gates, p3d-telemetry-schema, p3e-preblocked-ledger,
p3f-spec-draft}.md` + `restart/skinny/tranches/sk-v17/SPEC.md` (THE wave plan) +
`restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md`.
Input ground truth (LOCKED): S-P1 profile commit `0ae1caa52`
(`research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md §3`); S-P2 candidate pool commit
`f87ee713a` (`research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 L1–L9, §4
REJECTed, §6 binding conditions); Pass Alpha goalset commit `6496fecae`
(`SYNTHESIS.md` §0 + `research/alpha/{alphaE,alphaF}.md`).
Master HEAD `f87ee713a` (`git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §0 — Convergence verdict

**CONVERGED. S-P3 advances to the wave triumvirate.** The CHALLENGE wave returned ≥95%
ACCEPT for two consecutive cycles (V2 95.8%, V3 97.9%), with ZERO REJECT across all seven
lenses every cycle and no orphan disposition-flipping REVISE outstanding. The SK-V17 SPEC
is correctness-sound (every shortlist candidate traces to a LOCKED S-P2 survivor and through
it to a named S-P1 hot leaf), grammar-neutral by construction (Lock 14 — the shared
`select_classifier(alphabet)` + the generic `ValueRef<…,G:EventGrammar>` + the single
`BackendRule`-walking projection generator), regression-clean (re-opens no `skinny/REDRESS.md`
route; PRUNE-before-rebuild holds), cost-disciplined (every wave carries an LOC budget, a
≤90-min cap, a 20/15/30 phase breakdown, a same-wave consumer, and a revert protocol),
substrate-honest (Lock 1 union holds, no parallel substrate / sidecar / retained cursor / aux
table / Track1≡Track2 dishonesty), paper-close-free (every wave closes on a measured bench
row / grep / counter / checkasm, N≥50), and contrivance-free (lightningcss full-CSSOM the
fair materializing bar; no fixture / FNV / broadcast / per-corpus-literal re-entry).

| Cycle | ACCEPT rate | REJECT | Open REVISE | Gating lens |
|---|---|---|---|---|
| V1 | 79.8% | 0 | several (CH6 four anti-paper-close items; CH4 L7→W1 placement; CH1 D1/D2; CH2 R-CH2-1/2/3; CH5 wave-numbering) | CH6 / CH4 (placement + paper-close family) |
| V2 | 95.8% | 0 | 3 (CH4-6 L4-placement; CH5-2 L8-W1-vs-W2; CH6 W2 0%-floor → -2.0% band) all named with verbatim fixes | CH4/CH5/CH6 (cohort re-key orphans) |
| **V3** | **97.9%** | **0** | **2 (one root defect: CH1 D1(V3) SPEC-line citation drift in P3-A/P3-C; cosmetic, non-flipping)** | — none gating |

Per-lens V3: CH1 24/26 = 92.3%; CH2 16/16 = 100%; CH3 12/12 = 100%; CH4 1/1 = 100%
(45.8%→79.2%→100% across V1→V2→V3 phase-shape folds); CH5 16/16 = 100%; CH6 17/17 = 100%;
CH7 7/7 = 100%. Pass-wide disposition-unit aggregate: **93 ACCEPT / 95 total = 97.9%**. The
two non-ACCEPTs are a SINGLE underlying cosmetic defect surfaced on the CH1 lens (the V3
SPEC-line-citation re-key that landed in P3-B but was left orphaned in P3-A/P3-C — R1 §2
below). They flip no candidate, alter no wave placement, name a deterministic three-line fix,
and do not block the §3Z test (V2+V3 are two consecutive ≥95% cycles; the substance under the
flag — candidate→wave placement, gate predicate, threshold — is independently re-verified
correct against the SPEC content, not the drifted anchors). Carried as a V4-fold-on-first-touch
obligation into the W0 dispatch, NOT a gating defect, NOT a re-cycle trigger.

## §1 — §3Z verdict (the orchestrator convergence test, applied)

**(a) ≥95% ACCEPT for two consecutive cycles — MET.** V2 = 95.8%, V3 = 97.9%. Every lens
cleared ≥95% on V3 except CH1 at 92.3% — and CH1's shortfall is two facets of a single
cosmetic citation-drift defect (not a correctness, generality, regression, cost, coupling,
paper-close, or overfit failure), with the substance under both flags re-verified clean at
source. Five of seven lenses at 100%; CH4 lifted from its V1 45.8% floor to 100% on the
P3-cohort L4→W2 / L7→W1 reconciliation.

**(b) Zero open critical defects — MET.** Zero REJECT across all seven lenses, all three
cycles. No shortlist candidate is a speculative kernel without an S-P1 antecedent; no wave's
>SOTA close rests on a permissive comparator (the strict full-CSSOM plane is enforced in code
at `css_canon_bench.rs:113-115,126`); no wave re-opens a REDRESS route (second-substrate grep
ZERO over `skinny/crates/` at HEAD); no wave introduces a second substrate or sidecar; no wave
closes on a future-phase promise.

**(c) Zero orphan unresolved REVISE — MET WITH ONE NAMED RESIDUAL (non-gating).** The single
V3 REVISE family (CH1 D1(V3)) is a SPEC-line-citation drift in P3-A/P3-C, not a
disposition-flipping orphan: every gate predicate and candidate→wave placement it touches
(L4→W2, L7→W1, L9→W4-post-W1, W1 no-speed-admission) is ACCEPT on substance and
independently re-verified against the SPEC content this cycle. Under §3W this is a
wording/anchor-fold item handed to the S-P3 first-touch of P3-A/P3-C (the SPEC itself carries
the CORRECT anchors), not a re-cycle trigger. **V ≤ 5 — MET (this is V3).** Wave count = 6
(W0–W5) ≤ 12; active shortlist = 8 (L1–L8; L9 conditional, not counted) ≤ 8 — both §3Z ceilings MET.

**§3Z verdict: CONVERGED. Advance to the SK-V17 wave triumvirate.** S-P3 has produced the
SK-V17 SPEC — the wave-sequenced, falsifiability-gated, telemetry-bound, pre-block-aware
implementation contract whose waves the per-wave triumvirate executes (research → plan →
redress, distinct commits, CHALLENGE interposed for W1/W2/W3 first-of-class).

## §2 — The §3Z verdict's single residual REVISE (R1, carried into W0 first-touch)

**R1 — CH1 D1(V3): SPEC-line citation drift in P3-A and P3-C (cosmetic, non-flipping,
deterministic 3-line fix).** When the V3 SPEC grew (the W1/W2 consumer enumeration + the
R-CH2-1 byte-equal check folded in), every `SPEC.md:<line>` anchor across the P3 cohort
shifted. P3-B's §0 fold note declares it re-keyed its own anchors and routed the reconciliation
to P3-A/P3-C; the fix landed in P3-B (and the P3-A W2/L4 cite) but three citation families in
P3-A/P3-C still carry the pre-grow (stale) anchors. The candidate→wave PLACEMENT and the gate
PREDICATES are correct and cohort-consistent everywhere; only the line numbers drifted:

1. `p3c-falsifiability-gates.md:83,88,155,168` — cite `SPEC.md:447` for "NO speed admission
   this wave." The unique "NO speed admission this wave" line is **`SPEC.md:475`**; `:447` is
   the unrelated L7-gated-behind-tape sizing clause. **Fix:** `:447` → `:475` (4 sites).
2. `p3a-candidate-shortlist.md:156,160,218` — cite `SPEC.md:616,637` for the W4 post-W1 L9
   admission gate; `:613-618` is the W3 ENTRY gate. The actual W4 L9 gate is **§7
   `:670-672,695`**. **Fix:** `:616,637` → `:670-672,695`.
3. `p3a-candidate-shortlist.md:131,135` — cite `SPEC.md:388,391` for "L7 lands in W1"; `:388`
   is the W0-downstream line, `:390` the W1 head, `:446-448` the L7 W1-default-body task.
   **Fix:** `:388,391` → `:390,396,446-448`.

This is the V3 twin of the V2 orphan-REVISE class (one artefact's re-key not propagating to
the cohort). Folding it makes every gate's SPEC anchor resolve to its own clause and returns
CH1 to 100%. It is a cosmetic anchor-reconciliation, applied on first touch of P3-A/P3-C at W0
dispatch; the binding SPEC carries the correct anchors already, so no executing triumvirate is
misled. **NON-GATING.**

Also carried (non-blocking, NOT a REVISE): CH6 §3.1 INFORMATIONAL — the SPEC W2 section does
not restate a `track1_full_parse ≥ -2.0%` recognizer-maintain line that P3-C §2.2 carries; a
P3-C↔SPEC completeness nicety, not a measurability defect (the typed gate + the JSON ±1.0%
tripwire already bound W2's close). Recorded; does not gate.

## §3 — Per-lens V3 dispositions

| Lens | ACCEPT | REVISE | REJECT | Rate | Verdict |
|---|---:|---:|---:|---:|---|
| **CH1 CORRECTNESS** | 24 | 2 | 0 | 92.3% | Every shortlist candidate traces to an S-P2 survivor + S-P1 hot leaf; no REJECTed candidate; every gate measurable; behaviour-wave thresholds denominate on `SK-V17-open`; strict full-CSSOM plane. Two REVISE = one D1(V3) citation-drift root defect (R1). |
| **CH2 GENERALITY** | 16 | 0 | 0 | 100% | Grammar-neutral end-to-end; all three V1 CH2 REVISEs (R-CH2-1/2/3) held + intact; the V3 W2 -2.0% band delta is CH2-neutral. Three load-bearing CH2 vehicles ground-truth-verified at HEAD. Lock 14 §2.1 gate present; non-JSON CSS L4 proof required. |
| **CH3 REGRESSION** | 12 | 0 | 0 | 100% | No wave re-opens a pre-block; PRUNE-before-rebuild holds; fact-stream/W5C retirement strands NO consumer (every site tree-verified at HEAD + same-commit migrate/delete obligation + greppable gate); JSON 51/51 guarded ±1.0%; full inherited family list present; V2 citation-drift residual folded. |
| **CH4 COST** | 1 | 0 | 0 | 100% | Every wave: LOC budget + ≤90-min cap + 20/15/30 phase breakdown + same-wave consumer + revert. Wave count 6 ≤ 12; shortlist 8 ≤ 8. Net-new L5/L6 carry scalar-ref + checkasm-new (verified absent at HEAD) BEFORE wiring. V2 CH4-6 L4→W2 folded clean. |
| **CH5 HIDDEN-COUPLING** | 16 | 0 | 0 | 100% | One substrate (Lock 1); L8 flag = `BackendRule` branch-tag over the existing sparse pair (verified `OffsetFlags(u8)`/`GRAMMAR_BIT0,1`/sparse pair at HEAD), NOT an aux table; no renamed scanner / retained cursor / sidecar / Track1≡Track2 dishonesty. V2 CH5-2 L8→W2 folded across all five P3-C sites. |
| **CH6 ANTI-PAPER-CLOSE** | 17 | 0 | 0 | 100% | Every wave closes on measurement (bench row / grep / counter / checkasm, N≥50); every deferral has a named receiver + gate; L9 carries its post-W1 re-profile admission gate AND its +5% exit lift concretely. V2 W2 0%-floor → -2.0% band folded at `SPEC.md:5,564-568`. All four V1 folds held. |
| **CH7 OVERFIT-PRUNE** | 7 | 0 | 0 | 100% | lightningcss full-CSSOM the fair bar (verified materialized CSSOM rule count, not token scan); success = real per-corpus median crossing, not a broadcast (W8R single-tuple tripwire `gate-json`-enforced); tailwind honest; no fixture/FNV re-entry; grammar-derived projections. All-ACCEPT carry from V2. |
| **Aggregate** | **93** | **2** | **0** | **97.9%** | CONVERGED (≥95% × 2: V2 95.8%, V3 97.9%; zero REJECT; zero orphan flipping REVISE; V3 ≤ 5; waves 6 ≤ 12). |

## §4 — The LOCKED SK-V17 wave plan (SPEC.md — PASS-ALPHA §4.4, PRUNE-before-rebuild)

The SPEC is the contract. It folds Pass Alpha's goalset (`SYNTHESIS.md §0`, 6496fecae), the
S-P1 N=200 profile (recognizer beats lightningcss 2–3×; the gap is MATERIALIZATION, not
recognition), and the LOCKED L1–L9 candidate pool into a six-wave **PRUNE-before-rebuild**
sequence. Substrate before consumer; guard rows before risk rows; W0 baseline first; behaviour
waves conditional on W0 close. Each wave carries owner paths, a measurable exit gate (named
corpus rows + Mbps thresholds vs lightningcss N≥50), a ≤90-min hard cap with a 20/15/30
research/plan/redress phase breakdown (+ CHALLENGE 90 first-of-class on W1/W2/W3), a revert
protocol, a same-wave consumer, and a pre-blocked-route list.

### Subject (the bracket close-condition, §0.1)
CSS L4 typed Track 1 BEATS lightningcss full-CSSOM on ≥1 structurally-regular benched corpus
(animate OR bootstrap) at N≥50 cold median — via unified tape/layout/projection + aarch64 NEON,
benched on the SKINNY tree (`skinny/crates/`, NOT `crates/core/`), grammar-neutral (Lock 14),
preserve-rich-ast (lazy `ValueRef`, never flattened), no contrivance, foldable to TOTALITY
(SK-V18). Recognition-only A (4-field `track1_full_parse`) does NOT discharge the typed gate.

### Wave manifest (SPEC §2 `:262-269`)

| Wave | §  | Name | Owner-path family | LOC budget | Cap | Pre-blocks (the key barred routes) | Same-wave consumer | Measurable exit gate (the falsifiable close) |
|---|---|---|---|---|---:|---|---|---|
| **W0** | §3 | Baseline + telemetry lock + lightningcss CSSOM re-baseline | `css_canon_bench.rs`, `css_l4_corpus.rs`, `gate-json` (harness/gate/report/test only) | 0 behavior + ≤300 harness/test | ≤90m | 24-row broadcast 215; fact-stream comparator; warm/single-sample; **no behaviour / no generated-output change** | `gate-json` consumes every emitted telemetry field + rejects malformed/missing | `SK-V17-open` captured; per-corpus lightningcss full-CSSOM **median N≥50 cold** emitted, all 4 corpora; JSON 51/51 within ±1.0% of `SK-V17-open`; `gate-json` rejects 4 malformed-row classes (fixture-proven); NO behaviour change. (`SPEC.md:368-375`) |
| **W1** | §4 | **PRUNE**: retire fact-stream + W5C array → tape activation (L2 + L3-minimal + L7) | `codegen/src/lib.rs` (W5C delete), `lower/{tape_plan,offset_tape,event_tape}.rs`, `runtime/src/tape/`, `grammars/css_l4_*/parser.rs`, `runtime/src/lib.rs` (7 round-trip tests) | ≤450 src/test (gen named separately) | ≤90m + CHALLENGE 90 | union-substrate 96/97/98 (admissible ONLY via REDRESS-140 differential, cardinality one, index==offsets); AZ-IV eager; StructRegistry indirection; W5C relocated-not-retired; fact-stream as admission plane; `split_off`/`Vec<Vec>` | L3's `ValueRef` cursor read IS L2's consumer (same commit); L7 sized by L2 | `tape_activated=true` (grep `Tape`/`ValueRef`/`TapeBuilder` in CSS path non-zero + `PayloadArena` write/alloc counters); `w5c_profile_array_retired=true` (grep ZERO); NO dangling `emit_fact_stream` round-trip assert (grep over `runtime/`+`codegen/` ZERO); **EXACT 8-field equality** (`rules=10136/style=9561/sel=9561/decls=20043`); JSON 51/51 ±1.0%; **NO speed admission this wave** (equality before speed). (`SPEC.md:461-476`) |
| **W2** | §5 | **REBUILD**: layout-driven lazy projection generator (L3-full + L8 + L4 + W5C retire) | ONE `BackendRule`-walking generator in `codegen/`; generated `document/value/view/visitor` per grammar (JSON + CSS riders) | ≤450 default / ≤650 with accepted pre-redress fit proof (§5) | ≤90m + CHALLENGE 90 | L8 flag as hand-curated per-rule catalogue (relocated-W5C overfit); L1/L4 index as parallel retained vector (REDRESS-53); fake-generated-template 213; eager/preserve-rich-ast breach; second substrate via `StructLayout`/`TapeCursor` | generated CSS projection (`value_from_ref`-isomorphic) reads the W1 tape; L8 flags read by L3; L4 reuses the index ONCE | `lazy_view_generated=true`; **R-CH2-1 (load-bearing): JSON rider re-emits byte-equal THROUGH the new generator** (a CSS-only generator does NOT pass); `css_rich_ast_preserved=true` (value-plane population parity); typed plane **no worse than -2.0% median vs the W1 typed-tape baseline** (`track1_typed@W2(c) ≥ -2.0%`, N≥50, all 4 corpora — bench-falsifiable band, a bare 0% floor does not bind); JSON 51/51 ±1.0%; `regen --check`. (`SPEC.md:547-571`) |
| **W3** | §6 | NEON structural index, RE-PROFILED (L1 + L5 + L6) | `bbnf-simd/src/aarch64/{comment_body_mask_64,bracket_depth_mask_64}.rs` + scalar twins + `tests/checkasm_*`; `dispatch.rs select_classifier` | ≤450 src/test (gen SIMD named) | ≤90m + CHALLENGE 90 | lo6/`classify_tbl4` on CSS (`;`0x3b/`{`0x7b → slot-59 `& 0x3f` collision; eq-set fan instead); PMULL on hot path (REDRESS-88; L5 uses `escape_mask_64` carry idiom); CTZ as default body (REDRESS-89; L6 default = scalar running balance); orphan kernel / net-new without checkasm + same-wave consumer; udot/i8mm/FNV-as-primitive | the tape's structural decode consumes the `Vec<u32>` index in the SAME commit; scan + tape land together or neither | **THE >SOTA gate: ≥1 regular corpus (animate OR bootstrap) crosses `delta_vs_lightningcss > 1.0×` at N≥50 cold median on the typed plane** (`css_comparator_plane=full-cssom`), `css_rich_ast_preserved=true` + `css_typed_summary_equal=true` re-proven; tailwind ADMIT-or-honest-REDRESS; material per-corpus median reported; `native_simd_status ∈ {parity-pass,checkasm-pass}` per primitive; `simd_non_json_exercise=css_l4`; JSON 51/51 ±1.0%; index IS the tape. Entry-gate orphan-kernel tripwire: if no scan leaf survives top-N on the benched tape path, W3 lands NO kernel and the >SOTA gate is evaluated on the W2 plane. (`SPEC.md:637-648,616-619`) |
| **W4** | §7 | Commit-by-construction Alt-mode (L9, **CONDITIONAL** on post-W1 re-profile) | `lower/tape_plan.rs` Alt-mode + generated output | ≤300 src/test | ≤90m | `split_off`/`Vec<Vec>`; value-discard; rollback antecedent fabrication (S-P1 measured ZERO speculative-rollback self-time; 28.87%+2.45% recognition-control is NOT a rollback antecedent) | the post-W1 CSS recognizer spine (the live consumer on the post-W1 profile, not a promised future consumer) | Dispatches ONLY if the post-W1 typed-tape re-profile (N≥50) surfaces a top-N rollback/recognition-control leaf; ELSE W4 does NOT dispatch (recorded not-needed, NOT a failure). On dispatch: byte-identical tape (checkasm) + measured **≥ +5% N≥50 cold median lift** vs the W3 plane on the gated corpus (`track1_typed@W4 ≥ +5%`); a lift below +5% disposes L9 NOT-WARRANTED (measured, not paper-closed). (`SPEC.md:668-680,708-716`) |
| **W5** | §8 | Close, clean regen, Lock-14 audit, Alpha feedback | docs/RESULTS/REDRESS/HANDOFF reconciliation; ≤150 named Lock-14 cleanup | 0 default / ≤150 cleanup | ≤90m | dirty-generated close / hand-patched generated (SK-V15 `DifferentFile("generated.rs")`); paper close; legacy-shim deletion before replacement proof; brace-counter as close surrogate; corpus-average substituting per-corpus medians | the close checklist + the `regen --check` gate + document reconciliation | `dirty_generated_state=clean` (`regen --check` 9/9, exit 0); Lock-14 audit passes (no CSS/JSON policy in generic crates, no renamed residue, no relocated W5C; CSS L4 non-JSON proof); every wave admitted/rejected/routed; JSON 51/51 + preserve-rich-ast + equality re-proven; **the tranche success criterion (≥1 regular corpus crosses) recorded TRUE, or honest residual recorded + escalated per PASS-ALPHA §8 (WARN)** — NOT paper-closed. (`SPEC.md:755-766`) |

### PRUNE-before-rebuild discipline (the load-bearing ordering)
W1 is the PRUNE wave: it DELETES the fact-stream String plane and the hand-coded
`W5C_REQUEST_FACT_PROFILES` routing array, THEN routes CSS into the EXISTING tape — it does
not add a parallel tape path beside the String (`SPEC.md:392-394`). W2 is the REBUILD: the
full rich projection generator generalizing W1's minimal cursor. W3 (NEON) is gated behind W1
tape activation (no structural index to scan into until the tape decodes CSS). Topological:
telemetry (W0) → prune (W1) → rebuild (W2) → NEON (W3) → L9 (W4, conditional) → close (W5).
No wave rebuilds on an un-pruned base.

## §5 — Falsifiability thresholds (per-corpus Mbps vs lightningcss, N≥50)

Every wave's exit gate resolves to a number from the bench, a grep over `skinny/crates/`, or
an equality/counter assertion — never prose. The strict admission predicate is executable:
`gate-json` rejects any CSS >SOTA admission unless `css_comparator_plane=full-cssom`, the
comparator is the same-run re-baselined lightningcss median, `css_typed_summary_equal=true`
(gate before speed), and `css_rich_ast_preserved=true` (`SPEC.md:127-131`).

- **The comparator (the fair bar).** lightningcss full-CSSOM (`StyleSheet::parse` →
  materialized CSSOM rule count, `css_canon_bench.rs:113-115,126`), re-baselined same-run at
  N≥50 — NOT cssparser token-scan (flaw-probe / planning-only, never strict admission). The
  harness asserts N≥50 in code (`css_canon_bench.rs:250`).
- **The corpora (sha256-pinned, `css_l4_corpus.rs:22-54`).** bootstrap@5.3.3, tailwindcss@0.2.0,
  material-components-web@14.0.0, animate.css@4.1.1 — genuine deployed stylesheets, not
  synthetic fixtures.
- **THE >SOTA gate (W3 exit, `SPEC.md:639-641`).** ≥1 regular corpus (**animate OR bootstrap**)
  crosses `delta_vs_lightningcss > 1.0×` at N≥50 cold MEDIAN on the typed plane
  (`track1_typed ÷ lightningcss_full_cssom`, same-run), with preserve-rich-ast + EXACT 8-field
  equality re-proven.
- **Per-corpus Mbps endpoints are UNMEASURED-PENDING (`SPEC.md:202-209`).** The prior numbers
  (793/833/929/974, run-dependent) are NOT the gate; no wave exit-gate may key on an inferred
  per-corpus endpoint until W0's N≥50 harness emits the per-corpus split. The S-P1 V4 band
  (1237/1110/1261/833 lightningcss) and the alphaB inferred endpoints (164/51/60) are SIZING
  references only — explicitly barred as gate denominators (inferred-endpoint trap closed,
  CH1 §2.4). The denominator for every behaviour-wave threshold is the **W0-recorded
  `lcss(corpus)@W0` / `SK-V17-open` median**.
- **tailwind (hardest hold-out).** Benched cold N≥50: ADMIT only if `delta_vs_lightningcss >
  1.0×`, ELSE honest residual gap + hot leaf in REDRESS — NOT tranche-blocking provided ≥1
  regular corpus crosses; NOT hidden behind a corpus average (a corpus-average substitution is
  an explicit W5 paper-close FAIL, `SPEC.md:774,828`).
- **JSON guard floor (every behaviour wave).** All 51 JSON rows maintain A/GO strict same-plane,
  throughput within ±1.0% of `SK-V17-open` — distinct from the W2 CSS-typed -2.0% maintain band.
- **Tranche-level criterion (W5).** `max(track1_typed/lcss@W0) > 1.0` at N≥50 median, with
  equality + preserve-rich-ast + JSON 51/51. If false at W5 close → BLOCKED, residual gap + hot
  leaf recorded in REDRESS, NOT paper-closed.

## §6 — Residual REVISE (carried into the W0 dispatch)

ONE residual, **non-gating**: R1 (CH1 D1(V3), §2 above) — the SPEC-line citation drift in
P3-A (`:131,135,156,160,218`) and P3-C (`:83,88,155,168`). Deterministic three-line fix
(`:447`→`:475`; `:616,637`→`:670-672,695`; `:388,391`→`:390,396,446-448`). Cosmetic
anchor-reconciliation; the binding SPEC carries the correct anchors; no candidate/placement/
predicate/threshold flips; applied on first touch of P3-A/P3-C at W0 dispatch. ZERO REJECT,
ZERO orphan disposition-flipping REVISE.

Also carried (INFORMATIONAL, not a REVISE): the CH6 §3.1 SPEC-W2-vs-P3-C `track1_full_parse
≥ -2.0%` recognizer-maintain-line completeness nicety — does not gate.

## §7 — HANDOFF

- **next-move = ready-for-T-P1 (totality fold) then W0 dispatch.** S-P3 converged; the SK-V17
  SPEC is the contract. The next move is the T-P1 totality fold (the greater-spec totality
  pass consuming the converged skinny SPEC), then dispatch of the SK-V17 **Wave 0** triumvirate
  per `pass-contracts/SKINNY-TRIUMVIRATE.md` (research → plan → redress, distinct commits). W0
  is dispatchable now (baseline + telemetry lock + lightningcss CSSOM re-baseline, 0 behaviour
  LOC); W1–W5 remain conditionally gated, each blocked until its prior wave closes and its
  entry gate holds. W4 (L9) is doubly-conditional on the post-W1 re-profile firing the L9 gate.
- **Update `restart/skinny/tranches/sk-v17/HANDOFF.md`** next-move line to
  `ready-for-T-P1 (totality fold) then W0 dispatch`.
- **First-touch obligation at W0:** fold R1 (the three-line P3-A/P3-C SPEC-line citation
  re-key) before or at the W0 redress commit. Also fold the inherited S-P2 residual R1 cosmetic
  fixes on first touch of p2a/p2c if cited (the `Cycle:` stamp / "deferred to P2-F" phrasing),
  per the S-P2 consolidation §5.
- **Binding carry-forward conditions (self-bound in the V3 artefacts + SPEC §9 `:837-850`):**
  L1/L4 index == tape-offsets identity; L8 flag = `BackendRule` branch-tag (no hand-curated
  per-rule catalogue, no relocated W5C); L2/L3 retire `W5C_REQUEST_FACT_PROFILES` + route from
  grammar; L6 scalar-balance default (CTZ consumer-only, parity-gated); L9 gated on the hard
  post-W1 typed-tape re-profile.

## §8 — Sources

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (scope matrix), §3 (CH1–CH7 charges),
  §4 (convergence), §8 (bbnf-lang axes).
- `restart/skinny/tranches/sk-v17/SPEC.md` (THE wave plan; verified anchors §0.1 close
  condition `:35-116`, §0.2 comparator `:118-131`, §0.4 telemetry `:151-198`, §0.5 goalset
  `:200-225`, §1 non-negotiables `:227-258`, §2 manifest `:262-269`, §2.1 generality+Lock-14
  `:305-335`, W0 `:368-375`, W1 `:461-476`, W2 `:547-571`, W3 >SOTA gate `:637-648` + orphan-
  kernel tripwire `:616-619`, W4 L9 `:668-680,708-716`, W5 `:755-766`, §9 route ledger
  `:782-854`, §10 dispatch scope `:856+`).
- `restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md` (per-wave dispatch contract).
- `restart/skinny/tranches/sk-v17/research/p3/{p3a..p3f}.md` (the V3 cohort, re-authored
  17:01-17:07).
- `restart/skinny/tranches/sk-v17/research/p3/hardening/V3/{CH1..CH7}.md` (the V3 lens
  reports; per-lens counts §3).
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 (L1–L9 LOCKED pool), §4 (REJECTed set barred), §5 (residual R1 cosmetic fold), §6
  (binding shortlist conditions). Commit `f87ee713a`.
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  §3.1/§3.3/§3.4 (bench medians, hot leaves, lever order; recognizer beats lightningcss 2-3×,
  gap is materialization). Commit `0ae1caa52`.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0 close-conditions + goalset;
  `research/alpha/{alphaE,alphaF}.md`. Commit `6496fecae`.
- Live source at HEAD `f87ee713a`: `bbnf-bench/src/bin/css_canon_bench.rs:113-115,126,250,260,266`;
  `bbnf-bench/src/css_l4_corpus.rs:22-54`; `bbnf-simd/src/dispatch.rs:42,101`;
  `runtime/src/tape/{mod.rs:18,22-23,144-150,175, assembler.rs:42,45-46,71,98-111}`;
  `codegen/src/lib.rs:299,336,567,611`; `runtime/src/lib.rs:76,91,108,126,143,162,434`;
  `grammars/css_l4_*/parser.rs:6` (×7); `runtime/src/grammars/css_l4_declaration_values/
  generated.rs:5`; `bbnf-bench/src/nonjson_css_l4.rs:596,624,2725`; second-substrate symbols
  (`StructLayout`/`TapeCursor`/`UnionTape`/`TapeStructBuilder`/`StructRegistry`) grep-clean of
  constructs (guard machinery only).
- `restart/skinny/tranches/sk-v8/SPEC.md` (the SPEC shape mirrored — per-wave LOC-budget column
  + global phase-cap table + ≤650 fit-proof escape on the highest-LOC wave).
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` (the per-wave triumvirate contract);
  `restart/prompts/pass-contracts/PASS-ALPHA.md` §4.4 (the wave template), §8 (WARN escalation).
- `skinny/REDRESS.md` (96/97/98 retirement; 140 SK-V16 W9 differential `:4245-4252`; JSON floors
  twitter 17685 / citm_catalog 28630 `:2828`).
