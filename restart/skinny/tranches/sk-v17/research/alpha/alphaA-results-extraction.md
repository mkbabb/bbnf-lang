---
artefact: αA SK-V16 results extraction (CSS focus)
pass: PASS-ALPHA cycle V4
agent: alphaA
tranche: sk-v17
subject: CSS L4 typed parsing >SOTA via unified tape/layout/projection + aarch64 NEON
master_head: 1c5bd7a25250640f3a6fcfc00abed11f556f674f
host: aarch64 Apple M5 Max, RUSTFLAGS=-C target-cpu=native, --profile bench, cold (warmup_iters=0)
canonical_close_condition: CSS Track 1 typed CSSOM must BEAT lightningcss (full-CSSOM comparator) on every metric
sk_v16_css_admission: 0/24 admitted — CSS Track 1 typed below cssparser gate; >SOTA bar UNMET
v2_fold_dispositions:
  - CH1-R2 (§2 per-corpus) — RESULTS.md does NOT have "zero CSS rows"; it carries 24 falsified W8R broadcast rows
  - CH1-R3 (§7 lever seed) — add benched-surface note per alphaE §0
  - tree-citation (§3,§4,§6,§8) — re-cite all benched-surface symbols to skinny tree per alphaE §0 lines 37-51
  - §6 V6 / §8 — re-cite tape substrate to skinny runtime/src/tape/ (TapeBuilder/Tape/ValueRef/PayloadArena)
  - §7:250-251 — close threshold is same-run re-baselined lightningcss median (Wave 0), not fixed >974
  - §7:260-261 — lever neutrality vehicle is skinny select_classifier(alphabet)/lo6_table_admissible (dispatch.rs:42,101)
v4_fold_dispositions:
  - V3-CH1-a (reconciliation note :139-146) — stale/self-contradictory meta-note deleted; rewritten as "all cohort artefacts state 24/112-135 as of V3; V2 undercount resolved" (ground-truth at :139-141 retained)
sources:
  - skinny/RESULTS.md (51 JSON rows ADMITTED + 24 css_l4/*/direct_to_struct/main rows, all AUDIT-FALSIFIED/not_admitted)
  - restart/audit/skinny-impl-overfit/sk-v16-w6-speed-report.md (f2fe49bbc, LazyLock hoist)
  - restart/audit/skinny-impl-overfit/sk-v16-w6p1-dimension-dispatch-report.md (85b4edd88, lever-1 suffix dispatch)
  - restart/audit/skinny-impl-overfit/sk-v16-w6p2-o1-checkpoint-report.md (8153236e8, O(1) checkpoint)
  - restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md (1c5bd7a25, flat-tape substrate)
  - restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md (A-series archaeology + design)
  - restart/skinny/tranches/sk-v16/HANDOFF.md
  - skinny/crates/bbnf-bench/src/css_l4_corpus.rs (corpus byte sizes)
  - skinny/crates/bbnf-simd/src/dispatch.rs (grammar-neutral SIMD entry — neutrality vehicle)
---

# αA — SK-V16 CSS Results Extraction

## 0. One-paragraph standing

SK-V16 closed with CSS L4 **0/24 admitted**. The W6 SOTA gate (Track 1 typed CSSOM must
≥ cssparser before any admit) is correctly and honestly **REJECTED** at every W6 sub-wave.
The banked SK-V16 CSS wins are CORRECTNESS + ALGORITHMIC, not throughput: (a) a
grammar-derived CSS L4 provider; (b) 8-field structural equality with cssparser
(rules=10136 / style=9561 / sel=9561 / decls=20043, errors=0, `shared_summary_equal=true`,
4/4 corpora); (c) a sound O(1)-amortised speculative checkpoint (the ~20x on single-sheet
bootstrap relative to the design's fragment baseline; ~14-16x measured on bootstrap
single-sheet directly); (d) the flat-tape SUBSTRATE landed but **UNWIRED** (dead code w.r.t.
every parse path — no grammar rides the tape). CSS Track 1 in the *benched skinny tree* is the
`RuntimeEmitterKind::RequestFacts` fact-stream String path at **~70 Mbps cold corpus**,
reproducibly ~14x slower than lightningcss. The SK-V17 goalset is born here.

**Benched-surface disambiguation (binding, per αE §0 lines 37-51).** Every throughput claim
below pertains to the **skinny benched tree** (`skinny/crates/`), NOT the totality core tree.
In skinny there is **no `StructLayout`, no `OpenFrame`, no `CssArena`, no `TapeStructBuilder`**
(all grep-clean across `skinny/crates/`). The benched CSS "Track 1" is the fact-stream String
emitter (`RuntimeEmitterKind::RequestFacts`, `track1_facts(input) -> Result<String,String>` at
`skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596`, fed by `emit_fact_stream`,
`skinny/crates/runtime/src/grammars/css_l4_*/generated.rs:5`). The landed tape substrate is
`skinny/crates/runtime/src/tape/` (`TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena`,
`mod.rs:94,175,38`; `assembler.rs`). The skinny layout-equivalent is the codegen lowering
(`BackendRule` + `skinny/crates/codegen/src/lower/{tape_plan,offset_tape,event_tape}.rs`). The
architecture doc's `crates/core/...`/`StructLayout`/`OpenFrame`/`CssArena` symbols are the
TOTALITY fold target, not SK-V17 owner paths. The 8-field typed-equality retime
(`css_l4_w6_typed_retime`) and the OpenFrame eager path are core-tree artefacts; their throughput
verdicts are cited as evidence of the *model's* cost, but the SK-V17 surface to move is the
skinny benched path.

---

## 1. Canonical cold bench (the SK-V17 re-baseline ground truth)

**Canonical figures (N=100 cold per-parse, per-corpus medians, master `1c5bd7a25`):**

| Engine | Full-corpus throughput | Output plane | Strictness |
|---|---|---|---|
| **CSS typed Track 1 (eager OpenFrame CSSOM, core-tree retime model)** | **~70 Mbps** (per-corpus 51-164) | typed CSSOM (`CssTypedValue` rich tree) | strict, rich-AST |
| **lightningcss** | **~974 Mbps** | full L2 CSSOM (`StyleSheet::parse`) | strict, full-CSSOM materialising |
| **cssparser** | **~2539 Mbps** | token stream | token-scan only (no CSSOM) |

- **Reproducible verdict: CSS typed Track 1 is ~14x slower than lightningcss** and ~36x
  slower than cssparser on the full 979,638-byte corpus. lightningcss is the FAIR comparator
  (it materialises a full CSSOM, as Track 1 does); cssparser does no CSSOM construction and
  is the SPEC admission floor only.
- **STATISTICAL CAVEAT (binding for SK-V17 telemetry):** the committed W6 sub-wave reports
  ALL use `W6_SAMPLE_COUNT=1` / single `Instant`-elapsed around one corpus parse
  (`sk-v16-w6-speed-report.md:13`; `sk-v16-w6p1-dimension-dispatch-report.md:13`). This
  single-sample harness is **statistically inadequate** — it explains the wide run-to-run
  spread observed below. The SK-V17 telemetry MUST use **N≥50 cold samples + per-corpus
  median** (the canonical N=100 convention) and emit the §4.3 PASS-ALPHA schema verbatim.

**Committed single-sample evidence (the spread that motivates N≥50):** the W6tape report
(`sk-v16-w6tape-report.md:42-47`, master `1c5bd7a25`) records two independent cold runs that
agree on the verdict but diverge ~5x in absolute scale due to single-sample thermal/build noise:

| Source (single-sample) | Track 1 Mbps | cssparser Mbps | lightningcss Mbps | Track 1 / lightningcss |
|---|---|---:|---:|---:|
| Scrutineer run | 69.668 | 2529.390 | 793.326 | 0.0878 (~11.4x slower) |
| Build run | 13.416 (first-cold 14.884) | 150.715 | 60.96 | 0.220 (~4.5x slower) |

The scrutineer's **69.668 Mbps Track 1 ≈ the canonical ~70 Mbps** and corresponds to the
brief's stated 63.9 baseline; the build run is a colder/lower-clock machine state. The
canonical ~974 lightningcss / ~2539 cssparser figures are the re-baselined per-corpus-median
numbers the SK-V17 harness must reproduce — they sit above the scrutineer's single-sample
793/2529 (median > single-cold-sample, as expected once N≥50 smooths the cold-start tail).
**Because the absolute lightningcss number is run-state-dependent (793 single-sample vs ~974
median vs 60.96 build-run), the SK-V17 close gate is keyed to a same-run re-baselined
lightningcss median, NOT a frozen literal** (see §7).

---

## 2. Per-corpus structure (the 4-corpus CSS L4 SK-V14 set)

Corpus definition: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54`. Four minified
real-world stylesheets, total **979,638 B** (the figure used in every W6 ratio):

| Corpus | Bytes | Character | Expected difficulty |
|---|---:|---|---|
| animate-4.1.1.min.css | 71,750 | keyframe-heavy, regular | easiest — regular structure |
| bootstrap-5.3.3.min.css | 232,803 | broad property mix, regular | regular; the O(N²) checkpoint stress case |
| tailwindcss-0.2.0.min.css | 179,631 | deeply nested, many short rules | HARDEST — short-rule density; bench adversarially |
| material-components-web-14.0.0.min.css | 495,454 | large, mixed selectors+values | large; dominates corpus weight |

- The contract's **per-corpus median range 51-164 Mbps** maps across these 4: the regular
  corpora (animate/bootstrap) sit at the high end (~120-164), the irregular/short-rule
  corpus (tailwind) at the low end (~51-70), material in between. SK-V17 telemetry must emit
  one row PER corpus PER workload, not a single corpus-aggregate.

**Per-corpus deltas (SK-V16 CSS row state — CORRECTED, CH1-R2):** the prior cycle-V1 claim
that `skinny/RESULTS.md` holds "zero CSS rows" is **FALSE**. RESULTS.md carries **51 ADMITTED
JSON rows AND 24 `css_l4/*/direct_to_struct/main` rows** — but the 24 CSS rows are **all
`not_admitted:SK-V15-W0-broadcast-diagnostic`, audit overlay `AUDIT-FALSIFIED`** (verified:
`grep -c '^| css_l4/'` = 24; `grep 'not_admitted:'` = 24× `SK-V15-W0-broadcast-diagnostic`).
The accurate statement is: **zero ADMITTED typed CSS rows; only 24 falsified W8R broadcast
diagnostics present.** Their CostFacts tuple is the broadcast measurement (e.g.
`css_l4/declaration_values/direct_to_struct/main`):
`track1_mbps=2319.041; cssparser_mbps=2362.037; lightningcss_mbps=929.281` —
the **24-row broadcast** that the V1/V2 overfit audits FALSIFIED (one wave's number broadcast
across 24 rows; `value_plane=full_parse_summary`, NOT a typed CSSOM; `simd=Scalar`). These rows
are a PRE-BLOCK, not a baseline: they are the very fact-stream/full-parse-summary measurement
SPEC #3/#9 declares diagnostic-only and forbids admitting. **SK-V17 must establish the
per-corpus TYPED-CSS baseline as its first telemetry act** — there is no SK-V16 *admitted*
per-corpus typed-CSS row to delta against, and the 24 falsified broadcast rows must NOT be
lifted as a baseline.

**Cross-artefact reconciliation note (V4 fold, binding):** this artefact's **24** is the
grep-verified ground truth — `grep -c '^| css_l4/' skinny/RESULTS.md` = 24, lines **112-135**,
every row `not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED` (re-verified at
master `1c5bd7a25`). All cohort artefacts state 24 / lines 112-135 as of V3 (αC §4/§7, αD §0/§5,
SYNTHESIS §0.2, HANDOFF Current-State); the V2 "6" undercount is resolved across the cohort. The
substantive conclusions (zero ADMITTED typed CSS rows; PERMANENT-PRE-BLOCK of the broadcast
measurement) are unchanged.

A-series historical high-water (recognition-only, no typed CSSOM; commit `3b8b757d`,
`crates/bbnf-tape`, `sk-v16-css-sota-tape-architecture.md:41-44`): bootstrap **454 MB/s**,
normalize **735 MB/s**, tailwind **496 MB/s**. These are the proof-of-concept the unwired
flat-tape substrate is meant to reconstitute generally — recognition only, NOT parity-complete
typed CSSOM, so not a direct comparator, but they prove the tape substrate is not the bottleneck.
(Note: `normalize` is an A-series recognition corpus only; it is NOT in the benched SK-V14 set
`{animate, bootstrap, tailwindcss, material-components-web}` — any SK-V17 close condition must
key on the benched four, not on `normalize`.)

---

## 3. The 8-field structural-equality state (the load-bearing correctness gate)

The W5/W6 structural-equality gate is the banked SK-V16 correctness win and the load-bearing
guard that any SK-V17 speed intervention must preserve. The 8-field equality is exercised by the
core-tree typed retime (`crates/core/tests/css_l4_w6_typed_retime.rs`, which builds the FULL
typed document via `CssL4Parser::parse + visit_document` over `CssTypedValue` variants), then a
shared 8-field summary is compared field-by-field against an independent cssparser walk. **This
is the model the benched skinny path must reach: SK-V17's de-fact-stream wave (αE C0) must re-run
exactly this 8-field equality on the NEW skinny benched typed path — the equality is the bridge
between the core-tree retime witness and the skinny benched surface.**

**The 8 gate fields (all EQUAL, `shared_summary_equal=true`, 4/4 corpora — verified at every
W6 sub-wave including master `1c5bd7a25`):**

| # | Field | Track 1 | cssparser | Equal |
|---|---|---:|---:|:---:|
| 1 | rules | 10136 | 10136 | ✓ |
| 2 | style_rules | 9561 | 9561 | ✓ |
| 3 | selectors | 9561 | 9561 | ✓ |
| 4 | declarations | 20043 | 20043 | ✓ |
| 5 | track1_errors | 0 | — | ✓ |
| 6 | cssparser_errors | — | 0 | ✓ |
| 7 | shared_summary_equal | true | — | ✓ |
| 8 | (per-corpus pass) | 4/4 corpora | 4/4 | ✓ |

- Source: `sk-v16-w6p2-o1-checkpoint-report.md:54-60`; `sk-v16-w6-speed-report.md:102`;
  `sk-v16-w6tape-report.md:30`; test `crates/core/tests/css_l4_w6_typed_retime.rs` (core-tree
  witness; skinny benched equivalent is the `css_l4_w8` / typed-summary gate per αE C0).
- **preserve-rich-ast holds exactly.** The value plane is richly populated, NOT flattened to
  Span: dimensions=2963, colors=1169, functions=883, lists=6754
  (`sk-v16-w6tape-report.md:34`). `CssColor/CssDimension/CssFunction/Selector/CssRule/
  CssTypedValue` all defined and fully traversed.
- **Invariant for SK-V17 (CH5 honesty):** this 8-field equality is the structural-honesty
  gate proving Track 1 produces a real typed CSSOM equal to cssparser's parse — NOT a
  flattened summary, NOT a constant. EVERY SK-V17 speed intervention must re-prove these 8
  fields EXACT before admit. This is the Track 2 ≠ Track 1 honesty anchor for CSS.

---

## 4. The 20x O(1)-checkpoint win (sound, generic, banked)

**Commit `8153236e8`** — `perf(sk-v16-W6): O(1) speculative-checkpoint via scratch-stack hoist`.
Scrutineer verdict **ACCEPT** (rollback_sound / parity_real / equality_real / speedup_real all true).

**What it is (generic mechanism, all grammars — Lock 14 clean):** every growing per-frame
container was hoisted out of the core-tree `OpenFrame` into builder-owned append-only **scratch
stacks**; each container-owning frame now holds only a `*_base: usize` cursor. `begin_compound`
records `base = scratch.len()`; interior deposits append to scratch top; `end_compound` drains
`[base..]` as one contiguous list. Frames became `Copy` (scalars + cursors), so the checkpoint
`stack.clone()` collapsed from an **O(document) deep clone** of the growing root-compound
`rules` Vec at every checkpoint (the **O(N²)** cause) to an **O(stack-depth) memcpy**. Rollback
truncates each scratch to its checkpoint length-marker — the precise inverse of append.

- Sources edited (mechanism, not generated; CORE-TREE — these are the totality builder, NOT
  skinny benched surface): `crates/core/src/runtime/builder.rs` (trait doc),
  `xtask/src/regen_css.rs`, `xtask/src/regen_simple_runtime.rs`,
  `crates/core/src/runtime/builder_template.rs`. All 9 grammars regenerated; `regen --check`
  clean 9/9. Containers covered: CSS pending_rules/decls/selectors/values/blocks/components;
  JSON pending_items/pending_pairs; bbnf+sheets+simple-cohort pending_children — **generic
  across all grammars, no CSS special-case** (`sk-v16-w6p2-o1-checkpoint-report.md:25-37`).
  (Benched-surface note: this win lives in the core-tree OpenFrame builder; SK-V17's skinny
  retarget folds checkpoint into the skinny `TapeBuilder` `offsets.len()` marker per αE C1.)

**The measured win (cold, warmup=0, release):**

| Measurement | Before | After | Speedup |
|---|---:|---:|---:|
| `data/css/bootstrap.css` (280,311 B) single-sheet | 0.617 Mbps | 8.741 Mbps | **14.2x** |
| corpus `bootstrap-5.3.3.min.css` (232,803 B) | 0.509 Mbps | 7.957 Mbps | **15.6x** |
| Full-corpus track1 (release) | — | 63.904 Mbps | — |
| vs design's ~3.1 Mbps fragment-corpus baseline | 3.1 | 63.9 | **~20x** |

- The "**20x**" is full-corpus track1 (63.9 Mbps) relative to the design's ~3.1 Mbps
  fragment-corpus baseline (`sk-v16-w6p2-o1-checkpoint-report.md:89`). The direct
  single-sheet bootstrap win is **14.2x / 15.6x** (`:83-84`). Scrutineer independent re-run:
  10.007 MB/s and 8.543 MB/s — same order, honest variance (`:87`).
- **Soundness (the gate, not speed):** truncate is the EXACT inverse of speculative append
  because the checkpoint-placing emitter wraps each speculative sub-expression in a
  self-contained IIFE between `checkpoint()`/`rollback()`, and `begin/end_compound` are a
  matched lexical pair inside one rule body — so every `split_off(base)` has `base >= marker`
  and no lower-frame deposit escapes truncate (`:103-107`). The emitter was NOT modified.
- **The implementer deliberately DIVERGED** from the design's deferral-journal/watermark
  prescription because the watermark scheme is **unsound here**: profiling showed checkpoints
  are routinely DROPPED without commit (commit count 916 << checkpoint count 1452 in
  generated CSS), so a push-on-checkpoint/pop-on-commit watermark stack leaks and the
  "min watermark" discriminator is wrong (must be max). The scratch-stack hoist achieves the
  identical O(N²)→O(N) elimination with strictly simpler provably-sound machinery
  (`:42-47`). **SK-V17 must NOT re-open the watermark route** (see αC redress digest).
- **Residual cost deliberately deferred:** the per-checkpoint `stack.clone()` (one small
  `Vec<OpenFrame>` alloc of Copy frames). Eliminating it soundly is pure speed work with
  soundness risk; the unsound stub hit 62x by eliminating ALL checkpoint work (below-marker
  scalar writes would survive a bare `stack_len` truncate). The sound version is the honest
  ~14-20x ceiling. Parity was NOT traded for speed.

---

## 5. The W6 sub-wave throughput ledger (per-sub-wave, all single-sample cold)

| Sub-wave | Commit | Intervention | Track 1 Mbps | cssparser Mbps | lightningcss Mbps | Δ vs prior | Verdict |
|---|---|---|---:|---:|---:|---|---|
| W6 LazyLock hoist | `f2fe49bbc` | hoist flat StructLayout to `static LazyLock` (core-tree) | 3.093 (claim 3.157) | 2476.472 | 833.199 | baseline | REJECTED (gate) |
| W6.1 lever-1 | `85b4edd88` | dimension suffix-class dispatch (kill 8-way Alt re-scan) | 3.178 (claim 3.153) | 2498.579 | 832.285 | +1.02x (noise) | REJECTED (gate) |
| W6.2 O(1) checkpoint | `8153236e8` | scratch-stack hoist, O(N²)→O(N) | 63.904 (full corpus) | 2239.519 | 754.692 | ~20x (vs 3.1 frag) | REJECTED (gate); ACCEPT (mechanism) |
| W6-tape substrate | `1c5bd7a25` | flat-tape substrate (UNWIRED, skinny `runtime/src/tape/`) | ~70 (69.668) | 2529.390 | 793.326 | none (dead code) | REJECTED as speedup; merge as scaffold |

**Reading the ledger (the honest SK-V16 CSS perf timeline):**

1. **W6 / W6.1 prove the dimension speculation was NOT the throughput floor.** The W6
   profiler attributed >68% self-time to the per-branch number re-parse and named lever (a)
   "the dominant lever that must land first." W6.1 implemented EXACTLY that
   (`grammar/css/l4/value-unit.bbnf:77-79`, suffix dispatch, generic
   `set_alt_branch_class`) — and throughput did NOT move (3.09 → 3.18, noise). **This
   refutes the dimension-speculation hypothesis by measurement** (`sk-v16-w6p1-dimension-dispatch-report.md:96-102`).
   The cost lives in **typed-value materialization / arena+builder indirection** (the
   post-AZ-IV 28-118x retention regression), not dimension dispatch.

2. **W6.2 is the only real throughput move (~20x), and it is ALGORITHMIC, not micro-opt.**
   It eliminated the O(N²) root-compound deep-clone-per-checkpoint. Even after it, Track 1 is
   ~11.8x under lightningcss — the gap is the materialization floor, not checkpoint cost.

3. **W6-tape did nothing measurable** — the substrate is correct, clean, additive, and the
   RIGHT seam (the skinny `runtime/src/tape/` `TapeBuilder`/`Tape`/`ValueRef`/`PayloadArena`,
   a single non-generic layout-driven builder, NO `Arena<G>` indirection, NO `StructRegistry`
   reintroduction), but it is **unwired dead code**: grep shows ZERO usage of the skinny
   `Tape`/`TapeBuilder`/`ValueRef` types in any CSS parse path. CSS still rides the
   `RuntimeEmitterKind::RequestFacts` fact-stream String path byte-for-byte
   (`sk-v16-w6tape-report.md:61`). The lazy-view-over-tape accessor generator (DESIGN steps
   3-10) does NOT exist; this is where SK-V17 begins.

---

## 6. Banked SK-V16 wins carried into SK-V17 (with provenance)

| Win | Commit | Status | SK-V17 disposition |
|---|---|---|---|
| Grammar-derived CSS L4 provider | W4/W5 (`ec12016c8` lineage) | banked, parity-green | foundation; do not re-derive |
| 8-field structural equality with cssparser | W5, held through `1c5bd7a25` | banked correctness gate (10136/9561/9561/20043, errors=0) | LOAD-BEARING guard; re-prove EXACT before every admit |
| Cross-grammar PEG codegen fix | W5 (corrected PEG-branch-order emitter, `ec12016c8`) | banked | foundation |
| O(1) speculative checkpoint (~20x bootstrap; sound, generic) | `8153236e8` | banked algorithmic win (core-tree OpenFrame builder) | foundation; do NOT re-open watermark route; retarget marker to skinny `TapeBuilder` |
| Flat-tape SUBSTRATE | `1c5bd7a25` | landed but **UNWIRED** dead code | SK-V17 must wire it: lazy-view generator + CSS-on-tape conversion |
| dimension suffix-class dispatch (lever-1) | `85b4edd88` | banked (clean left-factoring; throughput-flat) | foundation; refuted as throughput lever |

**V6 substrate citation (CORRECTED — skinny benched tree).** The flat-tape substrate banked at
`1c5bd7a25` is **`skinny/crates/runtime/src/tape/`** — types `TapeBuilder`/`Tape`/`ValueRef`/
`PayloadArena` (`tape/mod.rs:94,175,38`; `assembler.rs` `TapeBuilder`, `push_plain_offset` =
one branchless `u32` write + `set_len`). The architecture doc's `TapeStructBuilder`/`TapeCursor`/
`TapeRef` and `crates/core/src/runtime/tape/` are TOTALITY symbols (`grep TapeStructBuilder
skinny/` = EMPTY); they are the fold target, NOT the SK-V17 owner path. SK-V17 wires the
*skinny* `TapeBuilder` into the benched CSS path: parser emits Open/Close/Leaf offset records;
the lazy `ValueRef` cursor (isomorphic to JSON `value_from_ref`, `json/value.rs:143`) projects
the typed CSSOM at view time. No new cursor/builder type is introduced — the existing skinny
`Tape`/`ValueRef` is reused; this is the Lock-1 anti-second-substrate posture.

---

## 7. The SK-V17 goalset seed (what αA hands to αE/αF)

The CSS state is unambiguous and telemetry-bound:

- **Current state:** CSS typed Track 1 ~70 Mbps (per-corpus 51-164); ~7.2% of lightningcss
  (~974); ~2.8% of cssparser (~2539); **~14x slower than lightningcss**; 0/24 admitted; 8-field
  equality EXACT; rich-AST preserved. Benched surface = skinny
  `RuntimeEmitterKind::RequestFacts` fact-stream String (`nonjson_css_l4.rs:596`), NOT a typed
  CSSOM on the benched row.
- **Target state (the SK-V17 close condition):** CSS Track 1 typed CSSOM must **BEAT
  lightningcss** on the regular corpora first (animate/bootstrap/material), with tailwind
  benched adversarially, while holding the 8-field equality EXACT and preserve-rich-ast. **The
  close threshold is the same-run re-baselined lightningcss full-CSSOM median measured in the
  SK-V17 Wave 0 re-baseline at N≥50 — NOT a frozen literal.** The prior-run ~974 Mbps is a
  PRIOR-RUN REFERENCE only (the same single-sample harness reported 793 and 60.96 on adjacent
  runs, §1); the gate is `Track 1 median > same-run measured lightningcss full-CSSOM median,
  N≥50`. The honest first-cross band is **300-600 Mbps** per the architecture synthesis
  (`sk-v16-css-sota-tape-architecture.md:347-355`); crossing the lightningcss median is
  plausible on structurally-regular corpora once the model becomes isomorphic to the
  already-SOTA JSON tape, with tailwind the hardest and last.
- **The named lever sequence** (from the unwired-substrate gap), framed against the **skinny
  benched surface** per αE §0:
  1. **De-fact-stream + wire the flat-tape lazy-view generator + CSS-on-tape conversion** — the
     dominant materialization-floor lever. Skinny surface: extend/retarget
     `RuntimeEmitterKind` (`codegen/src/grammar_provider.rs`) so CSS emits into skinny
     `TapeBuilder` (`runtime/src/tape/`) instead of `emit_fact_stream`
     (`css_l4_*/generated.rs:5`); generate the lazy `ValueRef` typed view via skinny lowering
     (`codegen/src/lower/{tape_plan,offset_tape}.rs` over `BackendRule`). Removes the
     materialization-floor cost (~3-4x in the core-tree analog).
  2. **NEON structural pre-scan** (`byte_class_index_64` over the ~56% `find_component_delim`
     leaf + movemask cascade) — **but the hot-leaf %% must be re-confirmed on the benched
     skinny path (S-P1 re-profile), not inherited from the core-tree profile** (actual-profiling
     discipline).
  3. **commit-by-construction structural spine** (no speculative rollback on the structural
     backbone, driven by the NEON structural index).
- **Lever neutrality vehicle (the Lock-14 mechanism, CORRECTED).** The claim that these levers
  are "ALL grammar-neutral" is grounded NOT in architecture-doc names but in the benched skinny
  SIMD dispatch surface: `select_classifier(alphabet: &'static [u8; 64])`
  (`skinny/crates/bbnf-simd/src/dispatch.rs:42`) keys every scanner on the grammar's runtime
  alphabet, and `lo6_table_admissible(alphabet)` (`dispatch.rs:101`) is the
  grammar-general/scalar-fallback gate. JSON/CSS/Sheets/BBNF share this one scanner vocabulary;
  CSS-specific behaviour enters only as the alphabet/delimiter set passed to
  `select_classifier`, never as a CSS branch in the kernel (αE C2 neutrality vehicle). All
  aarch64-only, NO x86, NO SVE (Apple cores have none).
- **Pre-blocked (do NOT re-open — see αC):** AZ-IV eager value-tree materialization (118x),
  StructRegistry/Arena<G>/Builder<G> hot-path indirection (28-65x), CSS fact-stream String
  serialization, the **24-row W8R broadcast measurement** (`not_admitted:SK-V15-W0-broadcast-
  diagnostic`, RESULTS.md), watermark/deferral-journal checkpoint scheme (unsound here),
  unsound bare stack_len-truncate (62x stub), x86/AVX paths.
- **Telemetry binding (mandatory):** SK-V17 harness must emit N≥50 cold samples + per-corpus
  median (the canonical N=100 convention), one row PER corpus PER workload, NOT the W6
  single-sample / corpus-aggregate / broadcast form. The §4.3 PASS-ALPHA schema verbatim;
  lightningcss + cssparser are permanently wired into the W6 harness already.

---

## 8. Citation ledger (every claim above)

- Canonical cold bench ~70/~974/~2539: contract ground truth (N=100 per-corpus median, master
  `1c5bd7a25`); closest committed evidence `sk-v16-w6tape-report.md:42-47` (scrutineer single-sample
  69.668/2529.390/793.326). Same-run-median caveat: build-run 13.4/150.7/60.96 (`:42-47`).
- Corpus sizes + 979,638 B total: `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54`.
- 24 falsified CSS rows + broadcast tuple 2319.041/2362.037/929.281: `skinny/RESULTS.md`
  (`grep -c '^| css_l4/'`=24, all `not_admitted:SK-V15-W0-broadcast-diagnostic`/`AUDIT-FALSIFIED`,
  `value_plane=full_parse_summary`, `simd=Scalar`); SPEC #3/#9 diagnostic-only forbiddance.
- 8-field equality 10136/9561/9561/20043: `sk-v16-w6p2-o1-checkpoint-report.md:54-60`,
  `sk-v16-w6-speed-report.md:102`, `sk-v16-w6tape-report.md:30`; core-tree witness
  `crates/core/tests/css_l4_w6_typed_retime.rs`.
- 20x / 14.2x / 15.6x checkpoint win: `sk-v16-w6p2-o1-checkpoint-report.md:83-89`, commit `8153236e8`.
- dimension-dispatch refutation: `sk-v16-w6p1-dimension-dispatch-report.md:96-102`, commit `85b4edd88`.
- LazyLock hoist 3.09 Mbps: `sk-v16-w6-speed-report.md:55-60`, commit `f2fe49bbc`.
- Flat-tape substrate (skinny tree, UNWIRED): `skinny/crates/runtime/src/tape/mod.rs:94,175,38`,
  `assembler.rs` (`TapeBuilder`); `sk-v16-w6tape-report.md:5-7,61`, commit `1c5bd7a25`.
  (`grep TapeStructBuilder skinny/` = EMPTY — core-tree symbol, not benched.)
- rich-AST value plane (2963/1169/883/6754): `sk-v16-w6tape-report.md:34`.
- A-series recognition marks (454/735/496): `sk-v16-css-sota-tape-architecture.md:41-44`, commit `3b8b757d`.
  (`normalize` is A-series-only, NOT in benched SK-V14 set.)
- single-sample harness inadequacy: `sk-v16-w6-speed-report.md:13`, `sk-v16-w6p1-dimension-dispatch-report.md:13`.
- benched CSS Track 1 = fact-stream String: `nonjson_css_l4.rs:596` (`track1_facts -> Result<String,String>`),
  `css_l4_*/generated.rs:5` (`emit_fact_stream`), `RuntimeEmitterKind::RequestFacts`
  (`codegen/src/grammar_provider.rs`).
- lever-neutrality vehicle: `skinny/crates/bbnf-simd/src/dispatch.rs:42` (`select_classifier`),
  `:101` (`lo6_table_admissible`).
- benched-surface translation correction: αE §0 (`alphaE-candidate-shortlist.md:37-51`);
  `StructLayout`/`OpenFrame`/`CssArena` grep-clean across `skinny/crates/`.
