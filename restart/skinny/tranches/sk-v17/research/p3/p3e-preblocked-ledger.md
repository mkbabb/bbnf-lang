# SK-V17 P3-E: Pre-blocked-route ledger

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-29.
Scope: Walk `skinny/REDRESS.md` (~253 items / 6465 lines); produce the per-wave
pre-block list — the REDRESS entries each SK-V17 wave must NOT re-open — and identify
routes that may admit only under a different framing with fresh S-P1 evidence.
Output: this file.
Pass Alpha goalset: SYNTHESIS.md §0 close-condition — CSS L4 typed Track 1 beats
lightningcss full-CSSOM on ≥1 regular corpus (animate OR bootstrap) at N≥50 median,
via unified tape/layout/projection + aarch64 NEON, JSON 51/51 held, preserve-rich-ast,
EXACT 8-field cssparser equality re-proven. §0.4 pre-block families:
`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247,
FNV closed-enum production migration`.
Candidate pool: research/p2/ post-CHALLENGE survivors L1-L9 (HARDENING-S-P2-V3 §3).

Master HEAD `f87ee713a` (`git rev-parse HEAD = f87ee713a7cf82e6d2cc82738dde313940c49121`).

## §1 — Synthesis (concrete; cites REDRESS item, P2 candidate, S-P1 row, goalset line)

The SK-V17 candidate pool (L1-L9) is dominated by **substrate/tape/projection** routes
(L2 tape append, L3 lazy `ValueRef`, L4 tokenize-once, L7 reserve, L8 flag side-table)
and **NEON mask** routes (L1 byte-class, L5 comment mask, L6 bracket mask). Both
families have dense, recently-walked REDRESS antecedents. The danger is not that an
SK-V17 wave proposes a *new* substrate (the S-P2 CHALLENGE already barred D6 second
substrate, §4 below); it is that a wave **silently re-enters a falsified route under a
new name** — and the most dangerous antecedents are not the JSON micro-kernel rejects
but two load-bearing *retirements*:

1. **REDRESS 96/97/98 — `G-W3-UNION-SUBSTRATE` RETIRED (not blocked).** Item 98
   (`REDRESS.md:2908-2950`): the thesis that "the parser discards a SIMD structural
   index, re-discovers structural bytes scalar, and would gain throughput if the index
   became the retained union substrate" was made measurable twice (96 = full
   move-consumed `scan_structurals` vector inside `parse`; 97 = allocation-free
   `JsonStructuralCursor` over the aarch64 scanner) and **both regressed uniformly on
   all 10 JSON rows** (96: twitter 9284 vs 17685 floor; 97: twitter 7520 vs 17685
   floor — `:2828`,`:2886`). The empirical finding (`:2928-2933`): on the M5 Max
   wide-issue core the scalar delimiter loop is *cheaper* than materializing or
   streaming a SIMD structural cursor through retained parsing, because consuming the
   index adds memory traffic + cursor indirection a branch-predictable cache-hot scalar
   loop does not pay. **This is the central pre-block for L1/L4/L7** and the
   single greatest correctness risk in the SK-V17 plan.

2. **SK-V15 W6 / W11 (items 248, 253; `REDRESS.md:6294-6313`, `:6453-6456`) — the CSS
   typed plane is the *measured starting state*, not a pre-block per se,** but its
   exclusion list IS a pre-block set: W6 live admission sources EXCLUDE `W8R`,
   `CSS_GENERATED_RS`, fact-stream output, `CssFullParseSummary`, `parse_full`,
   brace-counter proof, and `lightningcss` (as a Track-1 surrogate). SK-V17 inherits
   that exclusion verbatim (§0.4 final bullet).

The material differential that makes the SK-V17 tape route ADMISSIBLE despite REDRESS
96/97/98 is established by precedent — **REDRESS 140 / SK-V16 W9** (`REDRESS.md:4245-4252`):
"The material differential from REDRESS 96/97/98 is that **no public `UnionTape`, public
substrate API, `BackendShape`, BIR/directive, class column, retained structural index,
sidecar vector, parser-owned cursor, second scan, or `bbnf-simd` edit is introduced.
Substrate cardinality remains one.**" SK-V17 rides exactly this seam: the structural
index IS the tape's `offsets` (S-P2 §6 condition 1: index == tape-offsets identity), the
classifier emits a *transient* `Vec<u32>` consumed in the same wave by the tape build
(L1→L2, L4), reset per parse, never retained across calls. The pre-block holds when —
and only when — the SK-V17 producer is transient-and-consumed-in-place, not a retained
parallel substrate. Any wave that retains the index as a vector parallel to a retained
parse collapses straight into REDRESS 53/96/97/98.

The wave→candidate mapping is the **canonical SPEC 6-wave W0-W5 manifest**
(`SPEC.md:264-267`) — the binding map every CHALLENGE lens checks pre-blocks against.
P3-B's prior 5-wave numbering (NEON@W2, L9@W3, close@W4) is REVISEd to this map; this
ledger keys every pre-block to the SPEC wave ordinal so a wave triumvirate dispatched by
wave label checks each gate at the right wave (no double/skip). The L8 side-table lands in
**W2** (`SPEC.md:515-516,542-543,576-577`), NOT W1; the dirty-regen / `regen --check` 9/9 close
gate lands in **W5** (`SPEC.md:748,757`), NOT W0 (W0 lands no generated change,
`SPEC.md:375`):

| Wave | Lever / receiver | Candidates landed | Owner-path family |
|---|---|---|---|
| **W0** | telemetry + lightningcss full-CSSOM re-baseline | none (baseline; NO generated change, `SPEC.md:375`) | `bbnf-bench` harness, `gate-json`, `nonjson_css_l4.rs` |
| **W1** | PRUNE: kill fact-stream String; activate tape; retire `W5C_REQUEST_FACT_PROFILES` | **L2** `push_plain_offset`, **L7** one-shot reserve, **L3** minimal `ValueRef` cursor (same-wave consumer) | `codegen/src/lower/`, `runtime_generator.rs`, `assembler.rs`, `lib.rs:336` + emit_fact_stream consumers, `regen_css.rs:45-153` |
| **W2** | lazy-view projection generator (the gating artefact) | **L3** full rich `ValueRef` rider, **L8** sparse-flag side-table, **L4** tokenize-once reuse | `codegen/grammar_provider.rs`, `lower/{tape_plan,offset_tape,event_tape}.rs`, `runtime/src/tape/{mod,assembler}.rs:93-150` |
| **W3** | NEON structural pre-scan (re-profiled first) | **L1** byte-class eq-set, **L5** comment mask, **L6** bracket mask | `bbnf-simd/src/dispatch.rs`, `aarch64/byte_class_from_eq_set_64.rs`, net-new `comment*`/`bracket*` |
| **W4** | commit-by-construction spine (CONDITIONAL on post-W1 re-profile) | **L9** Alt-mode codegen (gated) | `codegen/src/lower/tape_plan.rs` |
| **W5** | close, clean regen (`regen --check` 9/9), Lock-14 audit, Alpha feedback | none (close; ≤150 named Lock-14 cleanup LOC only) | `HANDOFF.md`, `REDRESS.md`, `RESULTS.md`, the 8 dirty generated files, generic-crate Lock-14 grep surface |

## §2 — Deliverable: the per-wave pre-blocked ledger

Every wave inherits the full §0.4 family plus the hidden-coupling escape list. The
table below names the entries each wave is *most at risk of re-opening* (the
proximate pre-blocks) — the entries a CH3 reviewer checks that wave's owner paths
against. "Inherited (all waves)" entries below the per-wave tables apply to every row.

### W0 — Baseline / telemetry-lock / lightningcss full-CSSOM re-baseline

| Pre-block | REDRESS | Why W0 is at risk | Forbidden in W0 |
|---|---|---|---|
| 24-row broadcast | **215** (`:5316-5350`), §0.2/§0.4 | W0 wires the lightningcss comparator + emits the per-corpus row table; the temptation is to re-project ONE timing tuple across the 4/24 corpus rows (the W8R `track1=2319.041/lightningcss=929.281/cssparser=2362.037` broadcast). | `css_sample_count == 1`; one tuple across >1 corpus row; any row sourced from `SK-V14-W8R-css-full-parse-profile-cold-8`. gate rejects (`SPEC.md:374,380`). |
| Fact-stream as comparator | **SK-V15 W6** (item 248, `:6294-6324`); §0.4 | W0 retires `assert_lightningcss_strict_equality` (`nonjson_css_l4.rs:776`, asserts vs a fact-stream today, `:1057,:3460`). Re-wiring the comparator against the fact-stream String instead of a built CSSOM repeats W6's `Track 1 4.317 vs cssparser 2051.911` flaw plane. | lightningcss compared against a fact-stream; `css_comparator_plane != full-cssom` for the lightningcss bar; cssparser token-scan presented as a >SOTA bar (it is a flaw probe, §0.6). |
| Warm/single-sample telemetry | **SK-V15 W6/W11** (248/253) — `W6_SAMPLE_COUNT=1` retired; `no-warm-benches` | W0 sets N. Re-using single-sample or warm/cached medians repeats the W6 statistical inadequacy. | `css_sample_count < 50`; `css_sample_statistic != median`; `css_sample_mode != cold`. |
| Behaviour / generated change at W0 | §8 axis 3 (`build-infra-first` — W0 is baseline only); `SPEC.md:375` ("NO parser/scanner/SIMD/codegen behavior or generated parser output change lands") | W0 is pure baseline + gate. Landing ANY behaviour primitive, SIMD kernel, codegen change, or generated parser output regen at W0 pre-empts the wave-gated PRUNE/rebuild sequence and double-counts a change the later wave owns. | any `L*` primitive; any generated `generated.rs` regen (the dirty-regen close is a **W5** gate, not W0 — see W5 table); any source edit not required for telemetry/gate/comparator wiring. |

W0 lands no behaviour primitive (no L*) and **no generated change** (`SPEC.md:375`); it is
pure baseline + gate. CH4 same-wave-consumer is the W0 `gate-json` row consumer (it rejects
malformed/missing telemetry in the same slice, `SPEC.md:377`); no kernel ships at W0.
The dirty-generated-close `regen --check` 9/9 gate is **re-keyed to W5** (`SPEC.md:748,757`)
— W0 lands no generated edit to clean.

### W1 — PRUNE / Tape activation: kill fact-stream, `push_plain_offset` (L2), one-shot reserve (L7), minimal `ValueRef` consumer (L3), retire `W5C_REQUEST_FACT_PROFILES`

| Pre-block | REDRESS | Why W1 is at risk | Forbidden in W1 |
|---|---|---|---|
| Union-substrate retirement | **96/97/98** (`:2795-2950`) — RETIRED, not blocked | W1 makes the tape the live CSS parse substrate. If the tape append is implemented as a retained structural-index vector / move-consumed scan vector inside `parse` (96) or a streaming cursor through retained parsing (97), it re-opens the *retired* union-substrate thesis and is REJECT-on-sight without a new Alpha/S-P3 contract (`:2934-2936`). Admissible only via the REDRESS-140 differential: substrate cardinality stays **one**, the tape's `offsets` IS the index, no public `UnionTape` / class column / sidecar / parser-owned cursor / second scan. | a co-indexed class-byte column in `runtime::tape`; a move-consumed `scan_structurals` vector in `parse`; an allocation-free streaming cursor *added alongside* the scalar path; any `UnionTape`/public substrate API. |
| AZ-IV eager value tree | §0.4 bullet 1 (118x regression); REDRESS **70** (`:1890` first `real_typed_struct` rejected as eager) | L2 tape append must stay lazy-by-default; if W1 eagerly materializes typed values on append (per-leaf `Box::new`, f64-alloc-per-number, per-color `Box<CssColor>`) it is the AZ-IV overfit. | eager per-leaf payload at append time; eager value tree; per-leaf `Box::new`. |
| StructRegistry / Arena<G> hot-path indirection | §0.4 bullet 2 (28-983x regression); REDRESS 96 class-column | W1's `TapeBuilder` (`assembler.rs:42`) must stay a single non-generic layout-driven sink. A registry lookup per leaf re-enters the StructRegistry regression. | `StructRegistry`/`Arena<G>`/`Builder<G>` per-leaf lookup; generic-over-grammar `TapeBuilder`. |
| `W5C_REQUEST_FACT_PROFILES` relocated, not retired | §0.4 bullet 4; S-P2 §6 condition 3; SK-V14 W7 (item 214, `:5297-5312` — fact-stream consumed the policy triad without retiring the array) | W1 must DELETE the array (`lib.rs:336`, iterated `:567,:611`, selected `:299/:291`) and derive routing from `BackendRule`. Relocating its per-rule branching into projection DATA / flag form is the Lock-14-phrase-#1 re-entry seam (`SPEC.md:323` "RETIRED, not relocated"). The L8 flag-form re-entry of this branching is the W2 proximate (see W2 table); W1's obligation is the array's DELETION + `BackendRule`-derived routing. | `W5C_REQUEST_FACT_PROFILES` surviving; per-rule-id match arms in skinny generic crates JSON does not need; any residual CSS routing entry not naming its `.bbnf` rule; the array's branching relocated into projection DATA. |
| Fact-stream as live admission plane | §0.4 bullet 3; **215**; SK-V15 W6 exclusions | W1 retires `emit_fact_stream` (`generated.rs:5`). Keeping a `parse_full`/`emit_full_parse`/`CssFullParseSummary` route as an admission surface re-enters the W8R/W6 exclusion set. | `emit_fact_stream`/`emit_full_parse`/`CSS_GENERATED_RS`/`CssFullParseSummary`/`parse_full` as an admission output plane; brace-counter proof. |
| Dangling `emit_fact_stream` round-trip assertion (stranded consumer) | §8 axis 5 (`no-deferrals`/same-wave consumer); `SPEC.md:416-423,453-458`; the W5C-array iteration sites | W1 DELETES the `W5C_REQUEST_FACT_PROFILES` array AND retires `emit_fact_stream` as the live plane. The binding SPEC §4 owner-paths (`SPEC.md:416-423`) now enumerate the full stranded set spanning BOTH crates — the runtime-side round-trip test fns (`runtime/src/lib.rs:76,91,108,126,143,162,434`), the seven live `grammars/css_l4_*/parser.rs:6` entries, the three `codegen/src/lib.rs:581,1001,1035` `.contains("emit_fact_stream")` assertions, and the `runtime_generator.rs:621,666,694` template. The codegen-side `w5c_*` round-trip + W5C-array iteration set verified this cycle against the live file is **`lib.rs:299,336,567,569,581,597,611,613,1001,1035,1109,1113`** (299 selected; 336 array def; 567/611 W5C-array iteration; 581/1001/1035 `.contains("emit_fact_stream")` assertions; 597/1109/1113 the `w5c_*` CSS-request round-trip + source-hash consumers). Per the same-wave-consumer non-negotiable (`SPEC.md:453-458`), EVERY one of these must be migrated to the tape-equality assertion or DELETED in the W1 commit; none may dangle past the retirement. A surviving `emit_fact_stream`/`W5C_REQUEST_FACT_PROFILES` assertion is a compile-broken orphan or a live re-entry of the retired plane. | any `emit_fact_stream` round-trip assertion surviving in `lib.rs` (581/1001/1035) or `runtime/src/lib.rs` (76/91/108/126/143/162/434); any `W5C_REQUEST_FACT_PROFILES` iteration surviving (567/611); any `w5c_*` request-round-trip consumer surviving without migration (597/1109/1113); a dangling reference to the deleted array/plane. |
| `split_off` / `Vec<Vec>` checkpoint arena | §0.3 ("checkpoint = `offsets.len()` O(1); rollback = truncate. No `split_off`, no `Vec<Vec>` arena"); S-P2 §3 L9/D3 | W1's O(1) checkpoint must be `offsets.len()` marker + truncate on the one offset vector. | `split_off`; `Vec<Vec>` arena; per-leaf eager payload on rollback. |

W1 same-wave consumer: L2 produces the tape; per SPEC §4 (`SPEC.md:444-451`) L2's
same-wave consumer is **L3's minimal `ValueRef` cursor read** — the minimal cursor
sufficient to re-prove the 8-field equality — landed in the SAME W1 commit (they land
together or neither, `no-deferrals`/SK-V5 orphan-kernel). L7 reserve is gated behind L2
(its consumer is the tape it sizes; if W3 has not landed the NEON index, L7 sizes from a
conservative byte-proportional bound — never a per-corpus literal, `SPEC.md:448`).
**L8 is NOT a W1 candidate** — it lands in W2 (`SPEC.md:542-543`), where its reader (L3's
full rider) is co-resident; this ledger keys the L8 anti-sidecar pre-block to the W2 table
below. W1 also carries the **stranded-consumer obligation** above: the full stranded
`emit_fact_stream`/`W5C_REQUEST_FACT_PROFILES` assertion set (SPEC §4 owner-paths
`SPEC.md:416-423`, codegen-side `lib.rs:299-1113` enumerated above) is migrated/deleted in
the same W1 commit so no dangling consumer survives the retirement.

### W2 — Lazy-view projection generator: L3 full rich `ValueRef` rider, L8 sparse-flag side-table, L4 tokenize-once reuse

| Pre-block | REDRESS | Why W2 is at risk | Forbidden in W2 |
|---|---|---|---|
| JSON `value_from_ref` rider not re-emitted byte-equal (generic-named CSS-only generator) | §8 axis 2 (`typed-materialization-invariant`); `SPEC.md:534-536,550-556`; the CH2 generic-named-generator failure mode | W2 is the projection generator — its CH2 load-bearing proof is that the SAME `BackendRule`-walking generator emits BOTH riders. A CSS-only generator that leaves JSON's hand-written `value_from_ref` (`json/value.rs:143`) path untouched satisfies `projection_generality_exercise ∈ {json, css_l4}` vacuously (the JSON rider is exercised, not re-generated) — the generic-named-but-CSS-pinned failure class (REDRESS 213). The gate: the JSON `value_from_ref` rider must RE-EMIT byte-equal THROUGH the new generator; if the regenerated JSON rider differs from the committed `json/value.rs:143`, W2 FAILS (the generator is CSS-pinned, not generic). | a CSS-only generator that does not re-emit the JSON `value_from_ref` rider; a regenerated JSON rider that is NOT byte-equal to `json/value.rs:143`; `projection_generality_exercise` satisfied by an un-regenerated JSON path. |
| Parser-local structural-mask cursor | **51, 53** (`:1334-1336` byte-class whitespace cursor / parser-local structural-mask cursor); SK-V8 W3 (item 92, `:2673` "does not reopen REDRESS 51 or 53 cursor routes") | L3's `ValueRef` is a read cursor over the SAME tape — admissible. A parser-OWNED structural cursor / retained cursor / cursor stream is the 51/53 route + the §0.4 hidden-coupling list. L4 reuse must be over the neutral L1 index (= tape offsets), with NO parser-local second cursor (S-P2 §6 condition 1). | parser-owned structural cursor; retained cursor/list; cursor stream; a parser-local second cursor for L4 reuse; aux density/projection table. |
| Sparse-flag → sidecar/dense column (L8) | §0.4 hidden-coupling list; REDRESS 96 (class column); SK-V9 W9 anti-sidecar (`:6407-6411`); `SPEC.md:515-516,542-543,576-577` | L8 lands in **W2** (re-keyed from W1): it stores kind-disambiguation flags in the EXISTING `flag_cursors`/`flag_values` sparse pair (`assembler.rs:93-113`, `flags_at` `:144-150`), each bit a `BackendRule` branch-tag projection (S-P2 §6 condition 2), paid only where non-zero. Widening to a dense parallel column or a new vector re-enters the class-column substrate (96) and the EventTape anti-sidecar gate. The flag MUST be a branch-tag projection, NOT a hand-curated per-rule catalogue (the relocated-W5C overfit, `SPEC.md:526-527,576-577`). | a dense parallel flag column; a widened per-position record; a new flag vector; any non-`absent` EventTape anti-sidecar field; L8 flag = hand-curated per-rule catalogue (relocated W5C). |
| Fake-generated-template / static centralization | **213** (`:5276-5293`) — the `G-SK-V14-W6.0-CSS-L4-ROOT-RUNTIME-COLLAPSE` reject: a structural registry that "does not carry the CSS domain projection data required to regenerate `CssColor` constructors, unit enums, ... recursive color references", and static centralization "would reintroduce the fake-generated-template failure class" | W2 IS the projection generator. It must walk the `BackendRule` shape and EMIT real accessors (`document/value/view/visitor`), not hand-author a static CSS runtime template or lose the projection data. The CSS routing (selector/aggregate/numeric/function/color rule sets, value-list collapse, hex packing, color-component order — §0.1 layout row) must be DERIVED as DATA from the grammar, not re-hardcoded. | a static hand-authored CSS runtime template; projection data lost rather than carried as tape-plan data; per-rule-id branching re-hardcoded into the generator. |
| Eager projection / preserve-rich-ast violation | §0.4 bullet 1 (AZ-IV); SK-V9 W9 ("alternate document projection" forbidden, `:6409`) | L3 must reconstruct the typed CSSOM ON DEMAND via lazy `ValueRef` reads — NOT a flattened span tree (loses rich AST) and NOT eager (per-leaf `Box::new`). `preserve-rich-ast` is non-negotiable (§0.1). | flattening typed rules to spans for speed; eager value-tree projection; an alternate/second document projection. |
| Second substrate via `StructLayout`/`TapeCursor` | §0.4 "No second substrate"; D6 (§4) | If W2 introduces a skinny `StructLayout`/`TapeStructBuilder`/`TapeCursor`, those become a SECOND substrate alongside the landed `Tape`/`ValueRef` (Lock 1 type-ambivalence). The generator emits accessors over the EXISTING `Tape`/`ValueRef`. | new `StructLayout`/`TapeStructBuilder`/`TapeCursor` type; new cursor/builder type; sixth `BackendShape`. |

W2 same-wave consumer: the generated CSS projection (`value_from_ref`-isomorphic) is the
production reader of the W1 tape; L3's full rich rider is L2's W2-resident consumer (the
W1 minimal cursor generalizes here, S-P2 §3 L2/L3). **L8's flag bits are read by L3 in the
same W2 commit** (`SPEC.md:543`) — L8 does not ship as an orphan producer. L4 is the
consumer half of L1's producer — but L1 lands in W3, so W2's L4 "tokenize-once" reuse is
over the W1 tape offsets (not yet the NEON index); the NEON-fed reuse arrives W3. P3-C
must gate L4's W2 form on the tape-offsets reuse, not a pre-NEON parser cursor.

### W3 — NEON structural pre-scan: L1 byte-class eq-set classifier, L5 comment mask, L6 bracket mask

| Pre-block | REDRESS | Why W3 is at risk | Forbidden in W3 |
|---|---|---|---|
| Union-substrate (the SIMD index re-emerges) | **96/97/98** (RETIRED) | W3 emits the `Vec<u32>` structural index. This is the EXACT artefact 96/97/98 retired. Admissible ONLY because the index IS the tape offsets (transient producer, consumed in place by W1/W2's tape, reset per parse — S-P2 §6 condition 1; §0.4 "if structural offsets are retained, the structural projection IS the tape, `LOCKS.md:75`"). A retained parallel index, a cross-call classifier-state carry, or a SIMD cursor streamed through retained parsing re-opens the retirement. | retained parallel structural index; cross-call classifier-state retention (REJECT under Lock 1 v+1); SIMD cursor through retained parsing; a second source pass / second scan. |
| PMULL on the hot path | **88** (`:2510-2540`) — PMULL `bitmap_prefix_xor_64` falsified escape-heavy/narrow JSON rows (-12.66% unicode_escapes); SK-V15 W6 host-feature note | L5's comment mask must use the `escape_mask_64` `overflowing_add` carry idiom (`lib.rs:188`), explicitly NOT PMULL (S-P2 §3 L5). Routing the mask fill through PMULL re-opens item 88. | PMULL as a default hot body (`bitmap_prefix_xor_64` or any mask fill); any `pmull.1q` on the production hot path without a narrowly-gated same-row non-regression proof. |
| CTZ as unconditional default body | **89** (`:2542-2585`) — CTZ bulk consumer falsified 6 JSON parse_only rows (-8.07% mesh); S-P2 §6 condition 4 | L6's SHIPPED/DEFAULT body is a SCALAR running balance over the two precomputed masks (i32 `depth_carry` threaded within one `scan_components_to_index` call, never retained). The CTZ-ranges path is consumer-only + parity-gated + REVISE-back-conditioned, NOT the default body (REDRESS-89 bound, S-P2 §6 condition 4). | CTZ-ranges as L6's unconditional/default body; `bitmap_next_set_bit` CTZ promoted to the production hot path without same-row maintain proof. |
| lo6/table classifier on CSS | §4 (route-eliminated) — `lo6 classify_tbl4` on the CSS alphabet; SYNTHESIS benched-surface note (`;{`→slot-59 `& 0x3f` collision; table-NEON scalar passthrough) | W3's CSS classifier MUST be the eq-set fan `byte_class_from_eq_set_64_neon` (`aarch64/byte_class_from_eq_set_64.rs:33`), NOT the JSON lo6 `classify_tbl4` (`dispatch.rs:106`) — the CSS `;`(0x3b)/`{`(0x7b) pair collides under `& 0x3f` (both → slot 59), and `byte_class_from_table_64_neon` is a scalar passthrough today (would claim a SIMD win it runs scalar). | routing CSS through `lo6_table_admissible`/`classify_tbl4`; presenting the table path's scalar passthrough as a SIMD win. |
| Orphan kernel / net-new without consumer | REDRESS **88/89** (orphan-kernel lesson); SK-V5 W5 (`:1255-1267` "all remaining bbnf.asm primitive bodies" blocked — no consumer); §4 udot/i8mm | L5/L6 are NET-NEW (`comment_body_mask_64`/`bracket_depth_mask_64`); each REQUIRES a new checkasm gate (ABSENT today, S-P2 §3) AND a same-wave consumer (G3 = the L1 composition). A net-new mask kernel landed without its checkasm + same-wave consumer is an orphan kernel. The udot/i8mm digit family is BARRED (§4). | a net-new mask kernel without `checkasm_comment_body_mask_64`/`checkasm_bracket_depth_mask_64`; a net-new kernel without a same-wave structural-index consumer; wiring the `udot`/`i8mm` digit kernel (§4). |
| Micro-kernel without same-row gate | **80, 82, 83, 84** (`:2217`,`:2287`,`:2320`,`:2360`) — mantissa-widen / single-quartet unicode classifier / StringBlock16 / object-pair value-byte, all rejected for no measured same-row improvement / no same-wave consumer | W3 kernels must be profile-first (RE-PROFILED on the benched tape path per §0.1 NEON gate — the ~56%/~10% figures are inherited from the core-tree profile and must be re-confirmed) with scalar-ref + checkasm + same-wave consumer + same-row maintain. A kernel landed on a stale/inherited profile is the 80/82-84 class. | any NEON kernel grounded on the inherited (not re-profiled) `find_component_delim ~56%`/`consume_balanced_at ~10%` figure; a kernel without scalar-ref + checkasm + same-row maintain. |

W3 same-wave consumer: L1 produces the index, L2's tape (W1) consumes it; L5/L6 feed L1
(G3 composition). The NEON leaf must exercise a non-JSON grammar (`css_l4`, the
`simd_non_json_exercise` column) — Lock 14. NEON is gated behind tape activation (no
index to pre-scan into until the tape decodes CSS, §0.1 NEON gate).

### W4 — Commit-by-construction spine (L9, CONDITIONAL)

| Pre-block | REDRESS | Why W4 is at risk | Forbidden in W4 |
|---|---|---|---|
| L9 admitted without the post-W1 re-profile | S-P2 §3 L9 + §6 (HARD blocking S-P1-re-confirm); §6 ("28.87%+2.45% recognition-control figures are NOT a measured rollback antecedent") | L9 (commit-by-construction Alt-mode) is CONDITIONAL: admit as active ONLY if a post-W1 typed-`Tape`/`ValueRef` re-profile (N≥50) surfaces the recognition-control loop (un-masked by the retired alloc floor) or a speculative-rollback leaf as top-N self-time. P1-E measured ZERO speculative checkpoint/rollback self-time on either benched plane. Shipping L9 without that re-profile is a speculative kernel (CH1). | L9 landed without a post-W1 re-profile naming a top-N rollback/recognition-control leaf; treating the 28.87%/2.45% recognition-control figure as a rollback antecedent. |
| `split_off`/`Vec<Vec>` rollback | §0.3; S-P2 §3 L9/D3 | L9 rides D3's O(1) `offsets.len()` checkpoint / `truncate` rollback on the ONE offset vector. | `split_off`; `Vec<Vec>` arena for checkpoint/rollback. |
| Value-discard on the Alt-mode pass | `no-value-discard` discipline; `typed-materialization-invariant` | The Alt-mode codegen property must not drop computed structural emits (`.map(|_| ())`); every `->` reaches the tape emitter. | discarding computed values in the Alt-mode lowering; an Alt that deposits structural but the spine commits without emitting it. |

W4 same-wave consumer: the post-W1 CSS recognizer spine — GATED on the re-profile as the
ADMISSION gate, not a live consumer on the locked profile. If the re-profile does not
surface a rollback/recognition-control leaf, W4 does NOT dispatch L9 (it is not a
deferral — it is a measured non-admission, recorded per `abrogate-before-patch`).

### W5 — Close, clean regen (`regen --check` 9/9), Lock-14 audit, Alpha feedback

W5 lands no behaviour primitive (no L*); it is the close wave (`SPEC.md:730-779`). It is
the wave that owns the dirty-generated-regen gate (re-keyed here from W0) and the
anti-paper-close family. Its pre-blocks are the close-time re-entries — a CH3 reviewer
checks the W5 owner paths (the 8 dirty generated files, `RESULTS.md`/`REDRESS.md`/
`HANDOFF.md`, the generic-crate Lock-14 grep surface) against them:

| Pre-block | REDRESS | Why W5 is at risk | Forbidden in W5 |
|---|---|---|---|
| Dirty-generated close claim / hand-patched generated | §0.4 last bullet ("full-codegen close claims while dirty generated CSS files remain"); SK-V15 W7-W11 all blocked on `DifferentFile("generated.rs")` (`:6350-6354`, `:6378-6380`, `:6412-6414`); `SPEC.md:748,757,772-773` | W5 must clean-regen the 8 dirty files via `cargo xtask regen --check` 9/9 exit 0 (`SPEC.md:748`). Closing W5 with the files still dirty, or HAND-PATCHING them to pass, re-enters the SK-V15 dirty-state block + Lock 6/14 + `clean-regen-discipline`. The gate is `dirty_generated_state=clean` (`SPEC.md:757`). | hand-patched generated files (`clean-regen-discipline`); `regen --check` < 9/9; `dirty_generated_state != clean`; a full-codegen close claim while any of the 8 files remains dirty. |
| Paper close (close on promise, not measurement) | §3 CH6 anti-paper-close; §8 axis 4 (`no-orphan-redress`); `SPEC.md:763-764,771` | W5 must close on the recorded measurement: ≥1 regular corpus crosses recorded TRUE, OR the honest residual recorded + escalated per PASS-ALPHA §8 (WARN). Closing on "wired"/"integrated"/a future-phase promise, or substituting an architecture analogy for row data, is the paper-close failure (CH6). | a close that asserts success without the per-corpus N≥50 median row; "wired"/"integrated" as a close criterion; architecture analogy without row data; deferring the >SOTA proof to a future phase. |
| Legacy-shim deletion BEFORE replacement proof | §0.4 / SK-V15 PRUNE-before-rebuild; `SPEC.md:771-772` ("deleting legacy CSS generated/runtime shims before replacement proof landed") | W5 reconciles dispositions; deleting the legacy CSS generated/runtime shims (the fact-stream/`W5C` residue) BEFORE the tape/projection replacement proof has landed strands the live path. Deletion follows the proof, never precedes it (PRUNE-before-rebuild is W1's order; W5 enforces the residue is gone only after W1-W3 admitted). | deleting a legacy CSS generated or runtime shim at W5 before its W1-W3 replacement is proven admitted; removing the fact-stream residue without the tape path proven live. |
| Brace-counter proof as a close surrogate | SK-V15 W6 exclusions (item 248, `:6294-6324`); §0.4; `SPEC.md:773-774` (dropping falsifier rows) | The CSS close must re-prove EXACT 8-field cssparser equality (`rules=10136, style=9561, sel=9561, decls=20043`); a brace-counter / balance-count proof presented in place of the 8-field structural equality re-enters the W6 surrogate-proof exclusion. Dropping a falsifier row to make the close pass is barred. | a brace-counter / balance-count proof presented as the close equality proof; dropping any falsifier/guard row; `css_typed_summary_equal != true` at close. |
| Corpus-average substituting for per-corpus medians | §0.5 per-corpus close; SYNTHESIS §0.5; `SPEC.md:774` ("corpus-average claim substituting for per-corpus medians") | The tranche success criterion is per-corpus (≥1 REGULAR corpus, animate OR bootstrap, crosses at its OWN N≥50 median). A corpus-average / harmonic-mean / blended figure that papers a non-crossing per-corpus row with a favourable aggregate is barred (grep-rejected at `SPEC.md:774`). | a corpus-average / blended / harmonic-mean throughput substituting for the per-corpus animate/bootstrap median; an aggregate crossing the bar where no single regular corpus crosses. |

W5 same-wave consumer: the close checklist + the `regen --check` 9/9 gate + the document
reconciliation (`RESULTS`/`REDRESS`/`HANDOFF` zero-drift) — no kernel ships at W5
(`SPEC.md:768`). Revert is documentary: reopen the producing wave or mark close blocked
with a mismatch list naming file paths, rows, and missing evidence (`SPEC.md:776-777`).

### Inherited by ALL waves (the §0.4 family + hidden-coupling escapes — verbatim)

These bind every wave; a wave that touches them in any owner path fails CH3:

- **REDRESS 28+33** — active `match_tiny_plain_string` as a retained parse-G fix
  (`:1303`,`:2004`): the tiny-string scalar kernel is direct-only, never a retained-G
  SOTA primitive. No SK-V17 wave wires it into the typed path.
- **REDRESS 50-55** — retained projection side tables (50), byte-class whitespace cursor
  (51), parser-local structural-mask cursor (53), exact decoded-string stats sink (54),
  quote-source fused streaming materializer (55) (`:1331-1336`). The retained-sidecar /
  parser-local-cursor family.
- **REDRESS 60-72** — the direct-string allocation / receiver / byte-writing /
  source-hook / parser-owned-scratch / DirectBuild-semantic-string-fact family
  (`:1346-2006`); 70 = first eager `real_typed_struct` reject; 71/72 = the bounded
  admits (host/API output schema typed DirectBuild; widened tiny-probe). The
  direct-materialization experiments family — `no SK-V7 wave reopens REDRESS 60-72`
  (`:2426`,`:2461`).
- **REDRESS 80, 82-84** — mantissa-widen, single-quartet unicode classifier,
  StringBlock16 tiny probe, object-pair value-byte compaction (`:2217`,`:2287`,`:2320`,
  `:2360`): JSON micro-kernels rejected for no measured same-row improvement.
- **REDRESS 88, 89** — PMULL prefix-XOR (88), CTZ bulk consumer (89) on the production
  hot path (`:2510`,`:2544`): both falsified JSON rows. (W3 proximate.)
- **REDRESS 96-98** — `G-W3-UNION-SUBSTRATE` RETIRED (`:2795-2950`). The SIMD-index-as-
  retained-substrate thesis; admissible only via the REDRESS-140 differential
  (cardinality one, index == tape offsets, no public substrate). (W1/W3 proximate.)
- **REDRESS 183/184/209-213** — the SK-V14 PRUNE rejects (`:5092`,`:5105`,`:5173-5293`);
  213 = the fake-generated-template / static-centralization failure class. (W2 proximate.)
- **REDRESS 215** — the 24-row CSS broadcast (one tuple → 24 rows, `:5316-5350`). (W0
  proximate.)
- **REDRESS 242-247** — the SK-V14 JSON parse_only micro-optimization rejects (indexed
  strings 244, structural stream 246, string64 247; `:6034-6262`): each rejected for
  regression / no same-row improvement on the parse-only plane.
- **FNV closed-enum production migration** — FNV stays bench-only (SK-V15 W10, item 252,
  `:6416-6444`); no production FNV selector/arbiter/correctness/migration. The
  `fnv64`/`push_ascii_lower_hex` 8.98-9.11% leaf retires WHOLESALE with the fact-stream
  String (S-P2 §4), never a primitive.
- **Hidden-coupling escapes** (§0.4): retained sidecars / sidecar tables / sidecar event
  vectors / retained cursor-or-list / cursor streams / aux density-or-projection tables /
  parser-owned structural projections / parallel source passes / second tapes / public
  `UnionTape` / new substrate APIs / sixth `BackendShape` / production FNV arbiters /
  production hash-correctness proof / Track 1 == Track 2 sidecars / wrong-plane comparator
  admission / cross-call classifier-state retention. **No second substrate** (Lock 1).

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

This is a pre-block ledger; its falsifiability is the *negative* gate (the routes a wave
may not take), which is checked against owner-path greps + the same bench the positive
gates use (P3-C). The named corpus rows that any pre-block violation would falsify:

- **JSON guard tripwire (every wave).** The SK-V17 tape activation must move NO JSON row
  out of A/GO: **51/51 JSON rows** strict, same-plane, cold, Apple M5 Max / aarch64
  (SYNTHESIS §0.1 JSON guard). A pre-block re-entry (e.g. W1 re-introducing the 96/97/98
  union substrate) is FALSIFIED by the exact failure signature REDRESS 96/97 recorded:
  any JSON parse_only/direct/typed row dropping below its locked floor (e.g. twitter
  17685, citm_catalog 28630 Mbps — `:2828`). The W10b-style per-row maintain block
  (`>2% drop vs pre-wave = fail`, REDRESS 89 `:2573-2579`) is the tripwire.
- **CSS subject rows (W0 → W5).** The four benched corpora `{bootstrap, tailwindcss,
  material-components-web, animate}` (`css_l4_corpus.rs:22-54`). Tranche success: ≥1
  regular corpus (animate OR bootstrap) median Track 1 typed > median lightningcss
  full-CSSOM at N≥50 (§0.5). A pre-block violation that produces a number is FALSIFIED
  when (a) `css_comparator_plane != full-cssom` (fact-stream comparator re-entry, W6
  flaw plane), (b) `css_sample_count < 50` or not median or not cold, (c) one tuple
  across >1 corpus row (broadcast 215), or (d) `css_typed_summary_equal != true` before
  any speed admission (EXACT 8-field: `rules=10136, style=9561, sel=9561, decls=20043`,
  `track1_errors=0`, `cssparser_errors=0`, 4/4 — SYNTHESIS §0.1 equality gate). The
  lightningcss bar is the same-run re-baselined median (Wave 0), NOT the prior
  run-dependent 793/833/929/974 figures (§0.2).
- **Pre-block grep gates (per wave, owner-path-scoped).** Each wave's pre-block is
  bench-measurable OR grep-measurable: `W5C_REQUEST_FACT_PROFILES` retired = `grep` over
  `codegen/src/lib.rs` returns zero (W1); **no dangling `emit_fact_stream` round-trip
  assertion** = `grep -n 'emit_fact_stream\|W5C_REQUEST_FACT_PROFILES'` over BOTH
  `codegen/src/lib.rs` AND `runtime/src/lib.rs` returns zero after the W1 commit (the full
  stranded set — codegen `lib.rs:299,336,567,569,581,597,611,613,1001,1035,1109,1113`,
  runtime `lib.rs:76,91,108,126,143,162,434`, `parser.rs:6` × 7, `runtime_generator.rs:621,
  666,694` — all migrated/deleted, SPEC §4 owner-paths `SPEC.md:416-423,453-458` — W1); no second substrate = `grep` for
  `StructLayout|TapeStructBuilder|TapeCursor|UnionTape` over `skinny/crates/` returns
  zero (all waves); **JSON `value_from_ref` rider re-emits byte-equal** = the regenerated
  JSON rider `diff`s clean against the committed `json/value.rs:143` THROUGH the new
  generator (W2 — the CH2 anti-CSS-pinned gate); CSS classifier = eq-set not lo6 =
  `byte_class_from_eq_set_64` in the CSS scan path, `classify_tbl4` absent from it (W3);
  L6 default body scalar = no CTZ in the L6 default body (W3); PMULL absent from
  prefix-XOR/mask-fill (W3); L9 re-profile artefact present before L9 lands (W4);
  **`regen --check` 9/9 exit 0** = `dirty_generated_state=clean`, no hand-patched
  generated file (W5). An ungreppable / unmeasurable pre-block is not a gate — it is
  rejected from this ledger (none here are ungreppable).

## §4 — Pre-blocked routes (the REJECTed-class, barred from the SK-V17 shortlist entirely)

These are not per-wave risks; they are barred from the SK-V17 *shortlist* (HARDENING-S-P2-V3
§4) — a wave that admits any of them as ACTIVE fails CH1 (no benched CSS S-P1 antecedent →
speculative kernel) and CH3 (re-opens a REJECTed route). Recorded here so no wave re-frames
one under a new name:

- **orphan `udot` 4-digit decode** (CF-4a/C5/C-B3/G4; `digit_mac.rs:5,27`, scalar twin
  `:15-22`): ZERO benched CSS digit/number hot leaf (P1-E §4.4(a); CSS counts, does not
  decode dimensions). Checkasm REQUIRED-NEW (absent). Same-wave consumer NONE. **Re-admission
  gate:** a post-W1 typed-`ValueRef` dimension-decode re-profile naming a digit leaf top-N.
- **net-new `i8mm` digit/dimension kernel** (CF-4b/C6): no P1 antecedent + net-new kernel
  + net-new scalar-ref + net-new checkasm; DOUBLY orphan-blocked. i8mm grep-clean-absent
  from `bbnf-simd/src/` (SYNTHESIS benched-surface note: i8mm appears only as a CPU feature,
  not a kernel). Barred; gated contingency only.
- **FNV / `push_ascii_lower_hex`** (the 8.98-9.11% leaf): retires WHOLESALE with the
  fact-stream String, never a primitive. Any NEON hex/FNV kernel is pre-emptively REJECTed
  pass-wide (SK-V15 W10 quarantine, item 252).
- **asmjson collapsed-stage FSM**: x86 AVX-512-only (`ARCHITECTURE.md:1206,1284`); dead on
  the aarch64 host. Inventoried, not a candidate.
- **lo6 `classify_tbl4` reuse on the CSS alphabet**: route-eliminated (`;{` `& 0x3f`
  collision → slot 59; table-NEON scalar passthrough). The CSS answer is L1's eq-set fan.
- **D6 second substrate**: the Lock-1 no-go anchor — `StructLayout`/`TapeStructBuilder`/
  `TapeCursor`/retained class column/sidecar event vector/aux density table/retained
  cursor/parallel source pass/public `UnionTape`/cross-call classifier carry. Proposes
  nothing; REJECT-on-sight.

### Routes that may admit ONLY under a different framing with fresh S-P1 evidence

| Route | Current disposition | Re-admission framing (fresh S-P1 required) |
|---|---|---|
| L9 commit-by-construction Alt-mode | CONDITIONAL (not yet active) | a post-W1 typed-`Tape`/`ValueRef` re-profile (N≥50) surfacing the recognition-control loop (un-masked by the retired alloc floor) or a speculative-rollback leaf as top-N self-time. The 28.87%+2.45% recognition-control figures are NOT a measured rollback antecedent (S-P2 §6). |
| udot 4-digit decode (CF-4a) | REJECT on current evidence; HARD-GATED | a post-W1 typed-`ValueRef` dimension-decode re-profile naming a digit/number leaf top-N self-time on a benched CSS corpus (esp. tailwind). Per §0.3 the udot orphan-wiring (C4a) "admits unconditionally" ONLY in the SYNTHESIS receiver framing where it has a live decode consumer; until the typed dimension-decode path exists, it has no consumer (S-P2 §4). |
| i8mm digit/dimension kernel (CF-4b) | REJECT (doubly orphan-blocked) | the C4a re-profile result AND a measured i8mm same-row win vs the scalar twin, with net-new scalar-ref + net-new checkasm landed same-wave (§0.5 tailwind row: "i8mm kernel (C4b, GATED behind re-profile)"). |
| Union substrate (96/97/98 thesis) | RETIRED (not merely blocked) | cannot re-admit without a NEW Alpha/S-P3 contract (`:2934-2936`). SK-V17 does NOT re-admit it; it rides the orthogonal REDRESS-140 differential (cardinality one, index == tape offsets) which is NOT the union-substrate thesis. |

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (P3-E row), §3 CH3, §8 axis 6.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions), §0.2 (starting
  state + broadcast 215), §0.3 (receiver goalset), §0.4 (pre-blocks + generality clause +
  hidden-coupling list), §0.5 (per-corpus close), §0.6 (strict comparator), Section 2
  (telemetry + gate rejection rules), Section 3 (four-lever trajectory), benched-surface note.
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
  §3 (LOCKED pool L1-L9), §4 (REJECTed set), §6 (binding shortlist conditions 1-4 + L9 gate).
- `skinny/REDRESS.md`: items 28/33 (`:1303`,`:2004`), 50-55 (`:1331-1336`), 60-72
  (`:1346-2006`, esp. 70 `:1890`, 71 `:1944`, 72 `:1996`), 80 (`:2217`), 82-84
  (`:2287`,`:2320`,`:2360`), 88 (`:2510-2540`), 89 (`:2542-2585`), 90 (`:2589`), 91-93
  (`:2620-2729`, esp. 92 `:2661-2690`), 94-98 (`:2731-2950`, esp. 96 `:2795`, 97 `:2850`,
  98 `:2908-2950`), 140 SK-V16 W9 differential (`:4245-4252`), 144 SIMD-ASM production
  (`:4420`), 183/184 (`:5092`,`:5105`), 209-213 (`:5173-5293`, esp. 213 `:5276-5293`),
  214 SK-V14 W7 (`:5297-5312`), 215 broadcast (`:5316-5350`), 242-247 (`:6034-6262`),
  248 SK-V15 W6 CSS retime reject (`:6294-6324`), 250-252 SK-V15 W8/W9/W10 (`:6356-6444`),
  253 SK-V15 W11 close (`:6446-6465`).
- `restart/skinny/tranches/sk-v17/SPEC.md` (the canonical 6-wave W0-W5 manifest, the
  binding map this ledger keys every pre-block to): wave manifest `:264-267`; W0 baseline
  no-generated-change `:375`; W1 PRUNE owner paths + stranded-consumer obligation
  `:390-492` (emit_fact_stream assertion set named `:416-423,453-458`); W2 projection generator
  + JSON `value_from_ref` rider re-emit + L8 sparse pair `:494-583` (esp. `:534-536,550-556`
  the CH2 anti-CSS-pinned gate, `:515-516,542-543,576-577` the L8 sidecar block); W3 NEON
  `:588-666`; W4 L9 conditional `:668-723`; W5 close + `regen --check` 9/9 + paper-close
  family `:730-779` (esp. `:748,757` dirty-regen gate, `:771-774` the close-time
  pre-blocks); §2.1 generality `:297-327`; §9 route ledger `:782+`.
- `restart/skinny/tranches/sk-v8/SPEC.md` (SPEC shape; pre-block section style `:535`,
  `:577`, `:640`).
- `skinny/crates/codegen/src/lib.rs` (the stranded `emit_fact_stream`/
  `W5C_REQUEST_FACT_PROFILES` consumer enumeration verified this cycle): `:299` selected,
  `:336` array def, `:567,:611` array iteration, `:581,:1001,:1035` `.contains("emit_fact_stream")`
  assertions, `:597,:1109,:1113` the `w5c_*` CSS-request round-trip + source-hash consumers.
- `restart/locks/LOCKS.md` (Lock 1 substrate-union `:75`; Lock 14 grammar-neutrality
  `:386-397`) — cited via SYNTHESIS.
