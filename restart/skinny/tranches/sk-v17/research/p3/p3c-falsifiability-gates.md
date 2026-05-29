# SK-V17 P3-C: Falsifiability Gates

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-29.
Scope: For every wave W0–W5 in the canonical SPEC 6-wave manifest (`SPEC.md:257-267`: W0 baseline / W1 tape / W2 projection / W3 NEON / W4 L9-conditional / W5 close), author the falsifiability gate — named corpus rows + concrete Mbps thresholds vs lightningcss (N>=50 median, same-run), the full-table maintain budget, the measurable exit gate, and the revert protocol. Every gate is greppable/benchable from `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs`; an unmeasurable (prose-only) gate is rejected. P3-B is re-sequenced to this same 6-wave map (V2 fold); the wave ordinals below are the SPEC's, not P3-B's.
Output: this file (`p3/p3c-falsifiability-gates.md`).
Pass Alpha goalset: SYNTHESIS §0.1 close conditions + §0.5 per-corpus close conditions. Tranche-level criterion: **>=1 regular corpus (animate OR bootstrap) crosses the lightningcss full-CSSOM bar at N>=50 median**, preserve-rich-ast intact, EXACT 8-field cssparser equality re-proven, JSON 51/51 held.
Candidate pool: `research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 LOCKED L1–L9 (post-CHALLENGE survivors).

---

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, goalset line)

### 1.1 — What a falsifiability gate is, in this tranche

S-P1 fixed the bar empirically (HARDENING-S-P1-V4 §3.1, fresh V4 run, `/tmp/skv17-p1/css_canon_n200_v4.txt`):

- The **recognition plane already BEATS lightningcss** 2.01–3.09× every corpus — but it is 4-field recognition-only (`CssFullParseSummary`), NOT preserve-rich-ast, so it does NOT discharge the SK-V17 typed gate (S-P1 §2, outcome **A**, §3.2).
- The **typed fact-stream plane LOSES** at 0.60–0.77× (outcome **L**), bound by the ~64% `String`-alloc floor + 25% `emit_fact_stream` self-time (S-P1 §3.3 fact-stream table; instr/byte 214.56–364.51 vs full_parse 46.46–57.72 → the quantified **4.4× instr/byte gap** is the target, S-P1 §3.4 item 3).
- The **eager-typed plane is pre-blocked** (outcome **K**, AZ-IV, SYNTHESIS §0.4).

So the SK-V17 task is precisely: land a **typed (preserve-rich-ast) plane at full-parse-like cost** via the tape, without the eager regression. The falsifiability gate for the tranche is therefore not "is the parser fast" — the recognizer already is — but **"does the typed plane reach the recognition plane's cost class (close the 4.4× gap) far enough to cross the lightningcss full-CSSOM bar on >=1 regular corpus, with 8-field equality re-proven?"**

Every wave below carries a gate that is **measured from one harness** — `css_canon_bench.rs` (`assert!(n >= 50)` `:250`; cold per-parse `fn sample :146`; emits `ROW corpus=… workload=… median_mbps=… min_mbps=… max_mbps=… stddev_mbps=…` `:260,:266`). The lightningcss comparator is the **same-run re-baselined full-CSSOM median** (`StyleSheet::parse`, S-P1 §3.1 lightningcss column), NOT a prior fixed number (SYNTHESIS §0.5: "the prior numbers 793/833/929/974 are NOT the gate"). The `cssparser` token-scan column is a **flaw probe only** (materializes nothing; S-P1 §3.2) and is NEVER a strict-admission anchor.

### 1.2 — Bar definition (the only load-bearing comparison)

Per SYNTHESIS §0.5 and S-P1 §3.1, the >SOTA bar is **per-corpus, same-run, N>=50 cold median, vs lightningcss full-CSSOM**, expressed as a within-harness ratio `track1_typed_median ÷ lightningcss_median > 1.0`. The wave-0 re-baselined lightningcss median per corpus is the gate denominator. The S-P1 V4 lightningcss medians are the **prior-run reference band** (NOT the gate, restated per wave for sizing only):

| Corpus | bytes | lightningcss full-CSSOM median (S-P1 V4 ref, Mbps) | typed track1 fact-stream now (Mbps) | current ratio (L) |
|---|---:|---:|---:|---:|
| animate | 71750 | 1237.346 | 741.702 | 0.60× |
| bootstrap | 232803 | 1110.169 | 851.021 | 0.77× |
| material-components-web | 495454 | 1261.148 (min 160.300 cold outlier) | 874.902 | 0.69× |
| tailwindcss | 179631 | 833.786 | 559.480 | 0.67× |

These are restated as the **W0 re-baseline target** (W0 re-emits them same-run; the live gate is the W0-emitted median, denoted `lcss(corpus)@W0`). Recognition plane (full_parse) for context: 2493/2273/2590/2577 Mbps — that is the cost ceiling the typed plane is being lifted toward, NOT a gate.

### 1.3 — The four-lever stack → waves (topological: substrate before consumer; guard rows before risk rows)

SYNTHESIS §0.5 names the intervention stack: **tape activation (W1) + layout projection (W2) + NEON structural index (W3) + commit-by-construction spine (W4)** — this is the canonical SPEC 6-wave split (`SPEC.md:257-267`), NOT a 5-wave merge of tape+projection. S-P1 §3.4 item 2 binds the lever ORDER: **tape FIRST, then NEON on the surviving scan** — "S-P2 must not invert it (the scan is masked by the String floor on the typed plane)." This is the load-bearing sequencing constraint my gates enforce: the NEON wave (W3) carries a **substrate-precondition entry gate** (tape must be live, else there is no structural index to pre-scan into — SYNTHESIS §0.1 NEON gate "gated behind tape activation").

Wave→candidate map (drawn ONLY from LOCKED L1–L9; consumes P3-A shortlist; ordinals = SPEC `:257-267`):

| Wave | Candidates landed | Same-wave consumer (per kernel) | Goalset close-row served |
|---|---|---|---|
| W0 baseline + telemetry-lock | (none — infra) | `gate-json` consumes every required column | Telemetry honesty (N>=50); JSON-guard baseline |
| W1 tape activation | L2 `push_plain_offset` · L3 minimal `ValueRef` cursor read · L7 one-shot SIMD reserve | L3 consumes L2's tape; L7 sizes L2's `offsets` | Tape activation (not dead code); preserve-rich-ast; CSS typed equality |
| W2 layout-driven projection | L3 generalization (codegen full lazy-view rider) · L8 sparse-flag side-table · L4 tokenize-once reuse · W5C_REQUEST_FACT_PROFILES retire (L2/L3 cond. §6.3) | the generated `document/value/view/visitor` riders (JSON + CSS); L8 feeds L3 kind-disambig; L4 reuses the (W3) structural index, falling back to the W1 single-walk if W3 has not landed | Layout-driven projection; Generated-state cleanliness |
| W3 NEON structural index | L1 eq-set classifier · L5 `comment_body_mask_64` · L6 `bracket_depth_mask_64` (L7 single-valued to W1, `SPEC.md:396,446`; L4 single-valued to W2, `SPEC.md:497-499` — W3 produces the `Vec<u32>` index that W2's L4 consumes) | the tape consumes the `Vec<u32>` index == offsets (L1/L4 identity §6.1) | NEON hot-leaf union |
| W4 commit-by-construction spine | L9 (CONDITIONAL — admits only on the **post-W1** typed-tape re-profile, §6 L9 gate; antecedent = the retired alloc floor, NOT the W3 scan collapse) | the post-tape CSS recognizer spine | (conditional; recognition-control loop) |
| W5 corpus close + honest tailwind | (no kernel — integration/bench) | the bench rows themselves | CSS >SOTA on regular corpora; Honest tailwind; Foldable to TOTALITY |

Wave count = **6 (W0–W5) <= 12** (CH4 ceiling). W4 is conditional and may collapse into "rejected-pending-re-profile" without blocking the tranche (its gate is the re-profile, §3.5).

---

## §2 — Deliverable: the per-wave falsifiability gate set

Every gate below has five mandatory parts (SK-V8 SPEC shape, §4): **named corpus rows · Mbps thresholds (vs `lcss(corpus)@W0`) · full-table maintain budget · exit gate (measurable) · revert protocol**. All thresholds are N>=50 cold medians from `css_canon_bench.rs`. "lcss(corpus)@W0" = the Wave-0 re-baselined same-run lightningcss full-CSSOM median for that corpus. The strict plane is `track1_typed` (preserve-rich-ast) ÷ `lightningcss` full-CSSOM, same-run (CH1: "comparator deltas use the strict plane").

### §2.0 — W0: baseline + telemetry lock (the `SK-V17-open` anchor)

**Purpose.** Build the `SK-V17-open` baseline, lock the telemetry gate, re-baseline lightningcss same-run. No behaviour wave dispatches until W0 closes (SYNTHESIS §0.5; PASS-3 §8.3 "W0 is always baseline + telemetry"). PRUNE-before-rebuild: W0 retires the `W6_SAMPLE_COUNT=1` single-sample harness and the 24-row broadcast diagnostic (RESULTS.md lines 112-135) as live planes BEFORE any rebuild.

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | All four: `animate`, `bootstrap`, `material-components-web`, `tailwindcss`, each emitting `track1_full_parse`, `track1_fact_stream`, `lightningcss`, `cssparser` rows. Plus the 51 JSON guard rows in `skinny/RESULTS.md`. |
| Mbps thresholds | **Establish, not cross.** W0 emits `lcss(corpus)@W0` for all four corpora (the live denominator for W1–W5). Sanity floor: each `lcss(corpus)@W0` reproduces the S-P1 V4 reference band (§1.2) within ±15% across the 3-run stability check (S-P1 §3.1 stability table 2.12/2.25/2.05 etc.); a corpus whose W0 lightningcss median falls outside ±15% of the S-P1 band halts W0 (measurement drift, not a behaviour result). |
| Full-table maintain budget | **JSON 51/51 admitted, strict, same-plane** (SYNTHESIS §0.1 JSON-guard row). W0 touches no JSON row; the 51 JSON rows must read identically to HEAD `f87ee713a`. |
| Exit gate (measurable) | (a) `css_canon_bench.rs` asserts `n >= 50` (`:250`, present at HEAD) and emits the 8-column schema row (`:260`). (b) `gate-json` REJECTS any RESULTS row missing a required SK-V17 column (P3-D schema). (c) The 24-row broadcast diagnostic (RESULTS lines 112-135) is marked retired / not a live admission plane. (d) `lcss(corpus)@W0` recorded for all four corpora, same-run, full-CSSOM (`StyleSheet::parse`, NOT a fact-stream — SYNTHESIS §0.1 telemetry-honesty row, `assert_lightningcss_strict_equality` `nonjson_css_l4.rs:776` retired-against-fact-stream replaced by CSSOM build). (e) JSON 51/51 byte-identical to HEAD. |
| Revert protocol | Revert the W0 harness/gate commit slice as one unit; restore `skinny/RESULTS.md` + `skinny/REDRESS.md` to HEAD `f87ee713a`. W0 touches no `runtime/`/`codegen/`/`bbnf-simd/` source, so a W0 revert is a pure bench/gate/docs slice. If `lcss@W0` cannot be re-baselined same-run, W0 returns BLOCKED (not a paper-close). |

**Falsifiability:** if `gate-json` accepts a row missing a required column, OR a JSON row changes, OR no same-run lightningcss CSSOM median emits, W0 FAILS. This wave closes on a measured baseline + a passing gate-reject test, NOT on "harness wired."

### §2.1 — W1: tape activation (L2 + L3-minimal + L7)

**Purpose.** Flip benched CSS Track 1 from `emit_fact_stream` String (`generated.rs:5`) to `TapeBuilder::push_plain_offset` (`assembler.rs:71`) over the EXISTING skinny `Tape`/`ValueRef` (Lock 1, no second substrate); reconstruct the typed CSSOM via lazy `ValueRef` projection (L3, isomorphic to JSON `value_from_ref` `json/value.rs:143`). PRUNE: retire `emit_fact_stream` as the LIVE plane in the same slice the tape goes live. This wave targets the **4.4× instr/byte gap** (S-P1 §3.4 item 3): killing the ~64% alloc floor + 25% `emit_fact_stream` self-time.

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | All four corpora, `track1_typed` (the NEW lazy-`ValueRef` plane that replaces fact_stream) + `lightningcss` rows; `cssparser` flaw-probe row for the equality check. JSON 51/51 guard. |
| Mbps thresholds | **NO speed admission this wave** (`SPEC.md:447`: "equality is the gate before speed"). W1 closes on substrate truth — equality + the tape-activation greps + the lazy-by-default counters — NOT on any Mbps threshold. The gate is: EXACT 8-field structural equality re-proves on the NEW typed path: `rules=10136, style=9561, sel=9561, decls=20043`, `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora (banked `1c5bd7a25`). **DIAGNOSTIC (non-gating) sizing signal only:** the W1 commit RECORDS `track1_typed@W1(c) ÷ fs@W0(c)` per corpus as a sizing telemetry value so W3's lift has a denominator; a large improvement (the alloc-floor kill: fact-stream is 215–365 i/B vs full_parse 46–58 i/B) is the EXPECTED shape but is NOT a W1 admission threshold and CANNOT fail the wave. No +40% (or any %) denominator is author-invented as a gate — it has no S-P1/S-P2 trace and is demoted to diagnostic per CHALLENGE V1 D2/REVISE-2. **W1 is NOT gated on crossing lightningcss** (that is W3/W5) and NOT gated on any fact-stream-delta; W1's falsifiable claim is "equality holds on the tape path and the String is gone" — proven by grep + counters, not by a speed number. |
| Full-table maintain budget | JSON 51/51 maintain GO, strict, same-plane (tape activation moves NO JSON row out of A/GO — SYNTHESIS §0.1 JSON guard). `track1_full_parse` recognition rows no worse than **-2.0%** median vs W0 (the recognizer must not regress while the typed plane is rebuilt). `PayloadArena.write_count == 0` on source-re-readable leaves (alphaC counter; proves lazy-by-default, not eager — AZ-IV pre-block, SYNTHESIS §0.4). |
| Exit gate (measurable) | (a) grep `TapeBuilder|ValueRef|PayloadArena|crate::tape` over `skinny/crates/runtime/src/grammars/css_l4_*/` returns **non-zero** (S-P1 §3.3 confirms it returns ZERO at HEAD — this is the tape-activation proof). (b) Benched Track 1 stops returning `String` (`emit_fact_stream` retired as live plane). (c) 8-field equality re-proven on the new path (the four counts above, exact). (d) The `track1_typed@W1 ÷ fs@W0` ratio is RECORDED per corpus as diagnostic sizing telemetry (non-gating — no threshold, cannot fail the wave; see Mbps row). (e) `PayloadArena` write/alloc counters confirm tape-not-String emission. (f) NO new cursor/builder type introduced (grep: no `StructLayout`/`TapeStructBuilder`/`TapeCursor` in `skinny/crates/` — Lock 1, SYNTHESIS §0.4). (L8 does NOT land in W1 — it is single-valued to W2, `SPEC.md:497`; its `BackendRule` branch-tag guard is the W2 exit gate, §2.2.) |
| Revert protocol | Revert the W1 source slice (`runtime/src/grammars/css_l4_*/generated.rs` regen + `lower/` seam + bench wiring) as one commit slice; the fact-stream `emit_fact_stream` path is restored as the live plane; generated files re-emitted from the prior generator state (Lock 6/14 clean regen). If equality fails to re-prove, W1 HALTS at the equality gate (speed is never read) and records the failing field count in REDRESS — NOT paper-closed. |

**Falsifiability:** if 8-field equality does not re-prove EXACTLY, W1 FAILS (gate before speed). If grep still returns ZERO tape symbols in `css_l4_*/`, the tape is not activated → FAIL (W6 "unwired dead code" re-instated). If `PayloadArena.write_count != 0` on source-re-readable leaves, the lazy-by-default claim is FALSIFIED → FAIL (eager regression, AZ-IV). The diagnostic `track1_typed ÷ fs@W0` ratio CANNOT fail W1 — it is a sizing signal for W3, not an admission gate (no speed admission this wave, `SPEC.md:447`). The alloc-floor-kill is proven structurally (the `String`-returning `emit_fact_stream` is retired as the live plane + `PayloadArena.write_count==0`), not by a throughput delta.

### §2.2 — W2: layout-driven projection (L3 generalization + L8 + L4 + W5C retire)

**Purpose.** Build the codegen lazy-view accessor generator in `skinny/crates/codegen/` that emits `document/value/view/visitor` for CSS by walking the SAME `BackendRule` shape (SYNTHESIS §0.1 layout-driven-projection row), land **L8** (the sparse-flag side-table — the kind-disambiguation mechanism read by L3, `SPEC.md:497,574`) and **L4** (tokenize-once reuse — the full rider consumes W3's structural index ONCE; if W3 has not landed, L4 reuses the W1 single-walk, `SPEC.md:497-499`), and RETIRE the hand-coded `W5C_REQUEST_FACT_PROFILES` array (`codegen/src/lib.rs:336`) — derive routing from the grammar, do NOT relocate it into projection data (Lock 14 phrase #1; §6.3 condition). PRUNE: delete `W5C_REQUEST_FACT_PROFILES` and the seven `RuntimeEmitterKind::RequestFacts` literals (`regen_css.rs:45,63,81,99,117,135,153`) BEFORE re-deriving.

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | All four corpora `track1_typed` + `lightningcss`; the two projection riders exercised are **JSON (existing `value_from_ref`) + CSS (new rich rider)** (SYNTHESIS §0.4 generality clause — Sheets/BBNF-self deferred to SK-V18). JSON 51/51 guard. |
| Mbps thresholds | `track1_typed@W2` no worse than **-2.0%** vs `track1_typed@W1` on all four corpora (W2 is a codegen-generality refactor, not a speedup wave — it must not regress the W1 tape gain). The projection-generator correctness is the gate, not a new throughput number; the cross-bar threshold is deferred to W3/W5. |
| Full-table maintain budget | JSON 51/51 maintain GO (the JSON `value_from_ref` rider is re-emitted by the SAME generator and must stay byte-equal in behaviour — strict). CSS 8-field equality maintained from W1. `track1_full_parse` no worse than -2.0%. |
| Exit gate (measurable) | (a) `grep 'W5C_REQUEST_FACT_PROFILES' skinny/crates/codegen/src/lib.rs` returns **empty** (retired; was `:336` at HEAD). (b) No per-rule-id match arms in the skinny generic crates that JSON does not need (grep for CSS-keyed rule-id match arms in `codegen/src/lower/` → empty; every residual routing entry names its `.bbnf` rule). (c) **JSON rider re-emits BYTE-EQUAL THROUGH the new generator** (the load-bearing CH2 anti-overfit check, R-CH2-1): the JSON `value_from_ref` accessor file produced by the ONE `BackendRule`-walking generator is byte-identical to the hand-written/prior-generated JSON path at HEAD — `git diff` of the regenerated JSON accessor returns EMPTY. If the JSON projection output changes by even one byte, W2 FAILS (the generator did not subsume JSON's path; a CSS-only generator that leaves JSON's hand-written path untouched is the forbidden generic-named-CSS-generator failure mode). AND the CSS rider produces the 8-field-equal typed CSSOM from the SAME generator. Both riders ride ONE generator — JSON the witness, CSS the first-mover. (d) `cargo xtask regen --check` 9/9 exit 0 (Generated-state cleanliness, SYNTHESIS §0.1). (e) preserve-rich-ast: typed values are lazy `ValueRef` reads, no per-leaf `Box::new`, no eager tree (grep `Box::new` in the generated CSS value path → only the bounded `PayloadArena` escape hatch for irreducible scalars). (f) **L8 flag bits are `BackendRule` branch-tag projections, NOT a hand-curated per-rule catalogue** (§6.2 / S-P2 §6 condition; `SPEC.md:526-527,571-572`): grep for a relocated `W5C_REQUEST_FACT_PROFILES`-shaped array or per-rule-id flag catalogue in the L8 side-table → REJECT; the flag value MUST derive from the `BackendRule` branch tag (L8 lands in W2, read by L3 the same wave, `SPEC.md:574`). (g) L4 consumes the structural index ONCE — the index IS the tape `offsets` (L1/L4 identity, §6.1); no parser-local second index retained parallel to a retained parse (REDRESS-53 → REJECT). |
| Revert protocol | Revert the W2 codegen slice; restore `W5C_REQUEST_FACT_PROFILES` + the seven `RequestFacts` literals; re-emit generated files from the restored generator. If the generator cannot derive a CSS routing entry from the grammar (i.e. a residual entry is intrinsically hand-data), record it in REDRESS as a Lock-14 residual and route to a different framing — do NOT relocate it into projection data (forbidden, §6.3). |

**Falsifiability:** if `W5C_REQUEST_FACT_PROFILES` survives (grep non-empty), OR a CSS-keyed per-rule match arm appears in a generic crate JSON does not need, W2 FAILS CH2 (Lock 14). **If the JSON rider does not re-emit byte-equal THROUGH the new generator (its accessor changes by even one byte), W2 FAILS (R-CH2-1):** this is what forbids a CSS-only generator that leaves JSON's hand-written path untouched — `projection_generality_exercise ∈ {json, css_l4}` is only satisfied when BOTH riders flow from the ONE generator, proven by the JSON byte-equal diff. If `regen --check` is not 9/9 exit 0, W2 FAILS (dirty generated state). This W2(c)/falsifiability language is promoted into the SPEC W2 exit gate (P3-F fold, `SPEC.md:507-517`).

### §2.3 — W3: NEON structural index (L1 + L5 + L6)

**Purpose.** Land the NEON hot-leaf union on the surviving scan AFTER the tape is live (S-P1 §3.4 item 2 lever-order; gated behind tape activation per SYNTHESIS §0.1 NEON gate). L1 eq-set classifier (`byte_class_from_eq_set_64_neon`, `aarch64/byte_class_from_eq_set_64.rs:33`) routed via `select_classifier(alphabet)` (`dispatch.rs:42`) produces a `Vec<u32>` structural index == the tape `offsets` (L1/L4 identity, §6.1); L5 `comment_body_mask_64` + L6 `bracket_depth_mask_64` are the net-new suppressor/balance masks (AND-NOTed / running-balance into the index). **L4 and L7 do NOT land here:** L4 (tokenize-once reuse, the 2–3× re-walk kill, S-P1 §3.3 P1-D note) is single-valued to W2 (`SPEC.md:497-499`) where it CONSUMES this wave's `Vec<u32>` index ONCE; L7 (one-shot reserve sizing `offsets` from the scan count) is single-valued to W1 (`SPEC.md:396,446`) and sizes against this wave's scan count once it lands. This is the wave that CLOSES the gap onto the recognition cost class — it consumes ~69% of recognition self-time (S-P1 §3.3: `find_component_delim` 59.24% + `consume_balanced_at` 10.31%).

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | All four corpora `track1_typed` + `lightningcss`. **NEON non-JSON exercise = CSS** (the `simd_non_json_exercise` column, SYNTHESIS §0.4 NEON clause — `css_l4` is a real rider through `select_classifier(alphabet)`). JSON 51/51 guard. |
| Mbps thresholds | **The cross-bar wave.** Tranche criterion: **>=1 regular corpus (animate OR bootstrap) `track1_typed@W3 median > lcss(corpus)@W0` (ratio > 1.0×, N>=50).** Concretely the strict-plane gate: `track1_typed@W3(animate) / lcss(animate)@W0 > 1.0` OR `track1_typed@W3(bootstrap) / lcss(bootstrap)@W0 > 1.0`. Sizing reference (S-P1 V4 lightningcss band, NOT the gate): the regular-corpus typed plane must reach >1237 Mbps (animate) or >1110 Mbps (bootstrap) at the W0 re-baseline. material reports its per-corpus median delta (integration check, not a single-corpus gate — SYNTHESIS §0.5 material row). |
| Full-table maintain budget | JSON 51/51 maintain GO, strict (the shared `select_classifier(alphabet)` kernel must not move a JSON row — JSON rides the same primitive at `json/scan.rs:219`). CSS 8-field equality maintained (output-invariant under L4 reuse — S-P1: cssparser equality holds). `track1_typed` non-regular corpora no worse than -2.0% vs W1 unless they cross (they may improve). |
| Exit gate (measurable) | (a) >=1 of {animate, bootstrap} crosses (ratio > 1.0× vs `lcss@W0`, N>=50 median). (b) Every NEON kernel has a scalar reference (L1: `src/scalar/byte_class_from_eq_set_64.rs` PRESENT; L5/L6: new `src/scalar/comment*`/`bracket*`, ABSENT today → REQUIRED-NEW in this wave) AND a checkasm differential (L1: `tests/checkasm_byte_class_from_eq_set_64.rs` PRESENT; L5: `checkasm_comment_body_mask_64` REQUIRED-NEW; L6: `checkasm_bracket_depth_mask_64` REQUIRED-NEW) — every primitive lands scalar-ref + checkasm + same-wave consumer or it does not land. (c) L1/L4 identity: the produced `Vec<u32>` IS the tape `offsets`; carry/depth threads WITHIN a single `scan_components_to_index` call, reset per parse (grep: no retained index parallel to a retained parse — REDRESS-53 collapse → REJECT). (d) L6 SHIPPED body is the scalar running balance (CTZ-ranges is consumer-only + parity-gated, NOT the default — §6.4; promoting CTZ to default re-opens REDRESS-89 → REJECT). (e) L5 uses the `escape_mask_64` `overflowing_add` carry idiom (`lib.rs:188`), NOT PMULL (stays clear of REDRESS-88). (f) aarch64-only — grep `bbnf-simd/src/` for `x86`/`avx`/`sve` → empty (SYNTHESIS §0.4 x86 pre-block). |
| Revert protocol | Revert the W3 SIMD + codegen-index slice as one commit; the scalar scan path (the W1 tape build over scalar `find_component_delim`) is restored as live. Each kernel is individually scalar-referenced, so a single failing kernel reverts to its scalar twin without reverting the whole wave (per-primitive revert). If NO regular corpus crosses after W3, the row is REJECTed with the residual gap + hot leaf recorded in REDRESS (SYNTHESIS §0.5 fallback "if < 1.0× lightningcss after W4: REJECT row, record residual gap + hot leaf; do NOT paper-close") — but the tranche only blocks if NEITHER W3 NOR W4 crosses (§3.6). |

**Falsifiability:** if NO regular corpus crosses 1.0× and W4 cannot lift it, the tranche-level criterion FAILS → BLOCKED with honest REDRESS residual. If any kernel ships without its scalar twin OR checkasm, CH4 FAILS (orphan kernel). If the structural index is retained parallel to a retained parse, CH5 FAILS (REDRESS-53 second substrate). If a JSON row moves, CH3 FAILS (the shared kernel regressed the witness).

### §2.4 — W4: commit-by-construction spine (L9 — CONDITIONAL)

**Purpose.** L9 commit-by-construction Alt-mode codegen (CF-3, owner `codegen/src/lower/tape_plan.rs`), riding the SK-V16-banked O(1) `offsets.len()` checkpoint / `truncate` rollback (no `split_off`, no `Vec<Vec>` arena). **CONDITIONAL ADMISSION** (S-P2 §3 L9 gate; HARDENING-S-P2-V3 §6 L9 obligation): L9 admits to W4 ONLY if a **post-W1 typed-tape re-profile (N>=50)** surfaces the recognition-control loop OR a speculative-rollback leaf as top-N self-time. **Antecedent = the retired alloc floor (W1), NOT the W3 scan collapse** (HARDENING-S-P2-V3 §3 L9, `:241,:243-244`: "the typed-tape path AFTER the alloc floor falls (post-CF-1) … un-masked by the retired alloc floor"; CF-1 = W1 tape activation). The re-profile fires post-W1 because the control loop is masked by the String alloc floor on the fact-stream plane and is unmasked the moment W1 retires it — W3's NEON scan collapse is irrelevant to whether the recognition-control loop is hot. The LOCKED 28.87%+2.45% recognition-control figures (S-P1 §3.3) are NOT a measured rollback antecedent — P1-E measured ZERO speculative checkpoint/rollback self-time on either benched plane.

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | The L9-gate re-profile rows: `track1_typed` on all four corpora, profiled with samply at N>=50 on the **post-W1** typed-tape plane (the gate-firing measurement; `SPEC.md:637`). Then, IF the gate fired and W4 dispatches (after W0–W3 close), the same four `track1_typed` + `lightningcss` rows for the +5% speedup gate vs the W3 plane. |
| Mbps thresholds | **Admission gate (measured, falsifiable):** the **post-W1** typed-tape samply re-profile (N>=50) must name the recognition-control loop (`parse_stylesheet`/`parse_block`/`parse_block_item`) OR a speculative-rollback leaf as a **top-N self-time leaf** on the typed plane — fired by the alloc floor falling at W1, NOT by the W3 scan collapse (antecedent per HARDENING-S-P2-V3 §3 L9 `:241,:243-244`). If it does NOT, L9 is NOT admitted — W4 does not dispatch; L9 recorded "rejected-pending: no post-tape rollback antecedent" (NOT a failure; the conditional is correctly disposed). If admitted: `track1_typed@W4 median > track1_typed@W3 median` by **>= +5%** (N>=50 cold median) on the corpus where the recognition-control loop is hot, AND any regular corpus that crossed at W3 stays crossed. |
| Full-table maintain budget | JSON 51/51 maintain GO. CSS 8-field equality maintained (L9 checkasm: recognizer-output equality with/without the Alt-mode pass — byte-identical tape). Any W3-crossed regular corpus stays >1.0×. |
| Exit gate (measurable) | (a) The **post-W1** admission re-profile artefact exists and names the top-N leaf (or explicitly records ZERO rollback/control-loop self-time → L9 disposed-rejected, measurably). (b) IF admitted: byte-identical tape with/without the Alt-mode pass (checkasm), AND the +5% speedup (N>=50 median) vs the W3 plane on the hot corpus. (c) L9 rides the SK-V16-banked O(1) `offsets.len()` checkpoint / `truncate` rollback — no `split_off`, no `Vec<Vec>` arena (grep `codegen/src/lower/tape_plan.rs` → no second offset vector). |
| Revert protocol | Revert the `tape_plan.rs` Alt-mode slice; the speculative-checkpoint-everywhere emit is restored (byte-identical tape guaranteed by the checkasm). If the re-profile shows no antecedent, there is nothing to revert — W4 disposes L9 as rejected-pending-SK-V18 and records the re-profile in REDRESS. |

**Falsifiability:** L9's admission is itself a falsifiable measurement — it admits ONLY on a measured top-N rollback/control leaf on the **post-W1** typed-tape plane (antecedent = the retired alloc floor). A wave that lands L9 without that re-profile FAILS CH1 (speculative kernel, no antecedent). A "wired Alt-mode" claim with no +5% measured speedup on a named corpus is a paper-close → CH6 FAIL.

### §2.5 — W5: corpus close + honest tailwind report

**Purpose.** The integration/close wave: confirm the tranche-level criterion (>=1 regular corpus crossed), report tailwind honestly, record material delta, prove foldability to TOTALITY. No new kernel — this wave closes on measurement.

| Gate part | Specification (measurable) |
|---|---|
| Named corpus rows | All four corpora `track1_typed` + `lightningcss`, N>=50 cold median, same-run, final. |
| Mbps thresholds | **Tranche close criterion (SYNTHESIS §0.5 success criterion):** at least one of {animate, bootstrap} `track1_typed@W5 / lcss@W0 > 1.0×` (carried from W3/W4, re-confirmed at close on the final build). **tailwind:** benched cold N>=50; ADMIT if `track1_typed@W5(tailwind) / lcss(tailwind)@W0 > 1.0`; ELSE the residual gap is REPORTED with the hot-leaf attribution and recorded in REDRESS — NOT paper-closed, NOT hidden behind a corpus average (SYNTHESIS §0.1 honest-tailwind row; §0.5 tailwind row). **material:** per-corpus median delta reported (integration check). |
| Full-table maintain budget | JSON 51/51 maintain GO, strict, same-plane (the close tripwire). CSS 8-field equality holds on the final build. `track1_full_parse` recognition plane no worse than -2.0% vs W0 (the recognizer that already beat lightningcss must not regress). |
| Exit gate (measurable) | (a) >=1 regular corpus crosses at N>=50 median (re-confirmed). (b) preserve-rich-ast intact: value-plane population parity (dimensions/colors/functions/lists counts match the eager-tree baseline — SYNTHESIS §0.1 preserve-rich-ast row). (c) 8-field equality re-proven on the close build. (d) tailwind disposition is admit-or-honest-REDRESS, NO corpus-average substitution (grep RESULTS for a corpus-average admit claim → empty). (e) Foldability: the tape/projection/NEON model is structured so `crates/core/src/runtime/tape/` can adopt it in SK-V18 (the generality riders are JSON+CSS; Sheets/BBNF-self deferred — SYNTHESIS §0.4). (f) `skinny/RESULTS.md`, `skinny/REDRESS.md`, `HANDOFF.md` agree at close. |
| Revert protocol | W5 changes only bench wiring + RESULTS/REDRESS/docs. Revert the close slice; the prior wave's measured rows stand. If NEITHER regular corpus crosses (W3 and W4 both short), W5 records BLOCKED with the per-corpus residual gap + hot-leaf attribution in REDRESS — the tranche does NOT paper-close (SYNTHESIS §0.5 fallback). |

**Falsifiability:** the tranche FAILS its close criterion if neither animate nor bootstrap crosses 1.0× at N>=50 — and that failure is recorded honestly in REDRESS, not masked. A tailwind "admit" without a per-corpus median > lcss FAILS (corpus-average dishonesty, CH6). A close claim with regressed JSON FAILS the guard tripwire.

---

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds, per wave, consolidated)

The single binding table the SPEC folds. Denominators: `lcss(c)@W0` = Wave-0 re-baselined same-run lightningcss full-CSSOM median; `fs@W0` = Wave-0 re-baselined fact_stream median. All medians N>=50 cold from `css_canon_bench.rs`.

| Wave | Lift rows (the gate) | Threshold (strict plane) | Maintain rows | Maintain budget |
|---|---|---|---|---|
| **W0** | (none) | `lcss(c)@W0` recorded all 4 corpora, ±15% of S-P1 V4 band; `gate-json` rejects missing column | JSON 51 | byte-identical to HEAD `f87ee713a` |
| **W1** | (no speed lift — substrate truth) | equality re-proven (4 exact counts) + tape-symbol grep non-zero + `PayloadArena.write_count==0`. **NO speed admission** (`SPEC.md:447`); `track1_typed@W1(c) ÷ fs@W0(c)` RECORDED as diagnostic sizing telemetry only (non-gating, no threshold) | JSON 51; `track1_full_parse` 4 | JSON GO strict; full_parse >= -2.0%; `PayloadArena.write_count==0` on re-readable leaves |
| **W2** | (refactor, no lift) | `track1_typed@W2(c) >= -2.0%` vs W1 all 4; JSON rider byte-equal | JSON 51; CSS equality | JSON GO; equality held; `regen --check` 9/9 exit 0; `W5C_REQUEST_FACT_PROFILES` grep empty; L8 flag = `BackendRule` branch-tag (no per-rule catalogue); L4 index == tape offsets (no parallel retained index) |
| **W3** | animate OR bootstrap `track1_typed` | **`track1_typed@W3(c) / lcss(c)@W0 > 1.0×`** for c ∈ {animate, bootstrap}, >=1 corpus, N>=50 | JSON 51; CSS equality; full_parse 4 | JSON GO strict; equality held; full_parse >= -2.0%; non-crossing typed >= -2.0% vs W1 |
| **W4** | the corpus where control-loop is hot (conditional) | **post-W1** re-profile names top-N rollback/control leaf (antecedent = retired alloc floor) → THEN `track1_typed@W4 >= +5%` vs W3; ELSE L9 disposed-rejected (W4 does not dispatch) | JSON 51; CSS equality; W3-crossed corpus | JSON GO; byte-identical tape (checkasm); crossed corpus stays >1.0× |
| **W5** | animate OR bootstrap (re-confirm); tailwind (admit-or-REDRESS) | regular corpus >1.0× re-confirmed at close; tailwind admit iff >1.0× else honest REDRESS | JSON 51; CSS equality; full_parse 4 | JSON GO strict; equality re-proven; full_parse >= -2.0%; value-plane population parity |

**Tranche-level falsifiability (the one criterion that gates the whole bracket):** `max(track1_typed@close(animate)/lcss(animate)@W0, track1_typed@close(bootstrap)/lcss(bootstrap)@W0) > 1.0` at N>=50 median, with 8-field equality re-proven AND JSON 51/51 held AND preserve-rich-ast intact. If false at W5 close → **BLOCKED**, residual gap + hot leaf recorded in REDRESS, NOT paper-closed.

### 3.x — Why each gate is measurable (CH1/CH6 self-audit)

- Every threshold is a number read from `css_canon_bench.rs` `median_mbps` (`:266`) or a grep over `skinny/crates/` or a checkasm/equality assertion — none is prose ("wired"/"integrated" are not exit gates anywhere above).
- Every comparator delta uses the **strict plane** (`track1_typed` preserve-rich-ast ÷ lightningcss full-CSSOM), never the cssparser flaw probe, never recognition-only full_parse (which is outcome A, not the typed gate).
- Every wave's exit gate compares against the `SK-V17-open` baseline (W0-recorded `lcss@W0`/`fs@W0`), per CH1.
- W1/W2 close on equality + grep + counters (substrate truth) — **W1 reads NO Mbps threshold** (`SPEC.md:447`, "equality is the gate before speed"); its `track1_typed ÷ fs@W0` ratio is diagnostic sizing telemetry, not an admission gate, and was demoted from a V1 author-invented +40% threshold (CHALLENGE V1 D2/REVISE-2, no S-P1/S-P2 trace for the 1.40× denominator). W3/W4/W5 close on a crossed Mbps ratio (speed truth) — no wave closes on a future-phase promise (CH6).

---

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

Per SYNTHESIS §0.4 + S-P2 §4. Each wave's gate must NOT admit any of these; a gate that would require one is rejected and routed to P3-E for the per-wave pre-block list.

| Wave | Pre-blocked routes the gate must not admit |
|---|---|
| W0 | The 24-row broadcast (RESULTS 112-135, one tuple → N rows) — W0 RETIRES it, never re-emits it. `W6_SAMPLE_COUNT=1` single-sample — retired. Fixture/FNV capacity constants. |
| W1 | AZ-IV eager-value-tree (per-leaf `Box::new`, f64-alloc-per-number, `Box<CssColor>`) — `PayloadArena.write_count==0` gate enforces lazy. StructRegistry/`Arena<G>`/`Builder<G>` hot-path indirection (REDRESS 28+33). Second substrate: `StructLayout`/`TapeStructBuilder`/`TapeCursor` (Lock 1). `emit_fact_stream` String as live admission plane (it retires; diagnostic-only). |
| W2 | `W5C_REQUEST_FACT_PROFILES` relocation into projection DATA (Lock 14 phrase #1, §6.3) — the retire must be a derivation, not a move. L8 flag as a hand-curated per-rule catalogue (the relocated-W5C overfit; L8 lands here, `SPEC.md:820`). L1/L4 index as a parallel retained vector (REDRESS-53 — L4 consumes the index here). Per-rule-id match arms in generic crates JSON does not need. Deleting legacy CSS generated/runtime shims before replacement proof lands. |
| W3 | REDRESS-53 (structural index retained parallel to a retained parse — the index IS the tape, §6.1). REDRESS-88 (PMULL — L5 uses `overflowing_add` carry instead). REDRESS-89 (CTZ-ranges as L6 default body, §6.4 — CTZ is consumer-only + parity-gated). x86/AVX/SVE (aarch64-only). Orphan udot/i8mm digit kernel (no benched CSS antecedent, S-P2 §4 — barred; the digit re-admission is a SEPARATE post-W3 re-profile, NOT this wave). Retained sidecar / sidecar event vector / aux density table / parallel source pass (Lock 1). |
| W4 | L9 as a speculative kernel without the post-tape re-profile antecedent (S-P2 §6 L9 gate). `split_off`/`Vec<Vec>` arena rollback (rides D3 `offsets.len()`/`truncate` only). |
| W5 | Corpus-average admit substituting for per-corpus medians (honest-tailwind, SYNTHESIS §0.1). Paper-close on a future-phase promise. lightningcss-vs-fact-stream comparator (must be full-CSSOM; the `assert_lightningcss_strict_equality` `nonjson_css_l4.rs:776` fact-stream assert is retired in W0). Wrong-tree close keyed on `crates/core/` totality symbols (`StructLayout`/`OpenFrame`/`CssArena` — grep-clean-absent from `skinny/crates/`; SYNTHESIS benched-surface note). |

Inherited REDRESS pre-block families (semantics, all waves): `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration` (SYNTHESIS §0.4). The full per-wave enumeration is P3-E's deliverable; my gates name the routes each wave's *gate* must not admit.

### 4.1 — S-P2 §6 binding shortlist conditions enforced in my gates

1. **L1/L4 index == tape-offsets identity** → W3 exit gate (c) (the L1 index is produced here: `Vec<u32>` IS the tape `offsets`, carry/depth within one `scan_components_to_index` call, reset per parse) AND W2 exit gate (g) (L4 — single-valued to W2 — consumes that index ONCE, no parallel retained index); a retained parallel index collapses to REDRESS-53 → REJECT.
2. **L8 flag = `BackendRule` branch-tag** → W2 exit gate (f): L8 is single-valued to W2 (`SPEC.md:497,574`); no hand-curated per-rule catalogue (relocated `W5C_REQUEST_FACT_PROFILES` → CH2 REVISE).
3. **L2/L3 routing derived-from-grammar** → W2 exit gate (a)(b): `W5C_REQUEST_FACT_PROFILES` retired, every residual entry names its `.bbnf` rule.
4. **L6 scalar-balance default** → W3 exit gate (d): CTZ-ranges consumer-only + parity-gated, NOT default body (REDRESS-89).
5. **L9 hard post-CF-1 re-profile** → W4 admission gate (§2.4): admit only on a measured top-N rollback/control leaf on the **post-W1** typed-tape plane (CF-1 = W1 tape activation; antecedent = the retired alloc floor, NOT the W3 scan collapse — HARDENING-S-P2-V3 §3 L9 `:241,:243-244`).

---

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` (§2 P3-C row, §3 CH1/CH6, §8 wave-gate discipline) — the pass contract.
- `restart/skinny/tranches/sk-v17/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` §3 (LOCKED L1–L9), §4 (REJECTed set), §6 (binding shortlist conditions) — the candidate pool.
- `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` §3.1 (canonical N>=200 bench, lightningcss medians, ratio band), §3.2 (outcome classes), §3.3 (hot leaves: `find_component_delim` 59.24%, `consume_balanced_at` 10.31%, `emit_fact_stream` 25.01%, alloc floor ~64%), §3.4 (lever order tape-first, 4.4× instr/byte target) — the profile/bar.
- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (close conditions: tape activation, layout projection, typed equality, preserve-rich-ast, >SOTA, honest tailwind, telemetry, NEON, foldable), §0.4 (pre-blocks + generality clause), §0.5 (per-corpus close conditions + tranche success criterion) — the Pass-Alpha goalset.
- `restart/skinny/tranches/sk-v17/SPEC.md` — the canonical 6-wave manifest these gates bind to: `:257-267` (W0–W5 wave manifest, ordinals), `:447` (W1 "NO speed admission this wave"), `:507-517` (W2 exit gate, target of the R-CH2-1 JSON-byte-equal promotion), `:616-617,:637,:653` (W4 post-W1 re-profile + measured lift), `:396,:446-447` (L7 single-valued to W1), `:497-499` (L8 + L4 single-valued to W2; L4 consumes the W3 index ONCE), `:526-527,:571-574` (L8 `BackendRule` branch-tag guard + L1/L4 identity guard as W2 pre-blocks), `:820` (W2 pre-blocked routes). The load-bearing wave plan.
- `restart/skinny/tranches/sk-v8/SPEC.md` §0 + Sections 4–8 (entry/exit/revert wave-gate shape mirrored) — the SPEC shape.
- `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` (`:146` `fn sample`, `:250` `assert!(n>=50)`, `:260/:266` schema row `median_mbps`) — the measurement harness.
- `skinny/crates/bbnf-bench/src/css_l4_corpus.rs:22-54` (the four sha256-pinned corpora: bootstrap 232803, tailwindcss 179631, material-components-web 495454, animate 71750) — the corpus set.
- `skinny/crates/bbnf-simd/src/dispatch.rs:42` (`select_classifier`), `aarch64/byte_class_from_eq_set_64.rs:33` (L1 kernel) — NEON owner paths.
- `skinny/crates/runtime/src/tape/assembler.rs:71` (`push_plain_offset`), `mod.rs:175` (`ValueRef`), `json/value.rs:143` (`value_from_ref`) — tape/projection owner paths.
- `skinny/crates/codegen/src/lib.rs:336` (`W5C_REQUEST_FACT_PROFILES`), `lower/tape_plan.rs` (L9 owner), `xtask/src/regen_css.rs:45-153` (seam-flip site) — codegen owner paths.
- `skinny/REDRESS.md` (pre-block families 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98) — the regression ledger.
- Master HEAD `f87ee713a` (`git rev-parse HEAD` = `f87ee713a7cf82e6d2cc82738dde313940c49121`).
