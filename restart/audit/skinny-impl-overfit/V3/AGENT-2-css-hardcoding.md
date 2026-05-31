# AGENT-2 — CSS-specific hardcoding + inflection-point readiness (PASS-IMPL-OVERFIT-AUDIT V3, SK-V17 close)

**Audit target:** master HEAD `f6a38445b` (SK-V17 CLOSED). Benched tree `skinny/crates/`.
**Axis:** CSS-specific hardcoding; the inflection-point readiness of the CSS path.
**Posture:** WRITE-ONLY. Read-only on source. Hand-craft is admissible during the >SOTA proof; contrivance is not. This report distinguishes the two.

---

## Headline verdict

**MIXED, leaning PRUNE-REQUIRED at the inflection gate.**

- The CSS parser is **100% hand-written, NOT grammar-derived.** It is a 646-LOC recursive-descent scanner emitted verbatim as a Rust `const &str` (`CSS_GENERATED_RS`). The grammar files `grammar/css/l4/*.bbnf` do **not** feed it. This is honest, *declared* hand-craft — but it means the CSS path has made **zero progress toward grammar-driven generalization** since SK-V16 flagged the identical pattern.
- The W2 rich CSSOM (`CssTypedValue`, `selector_count`, etc.) is **hand-authored classification logic in the same const string**, byte-dispatch heuristics — not grammar-projected types. Honest hand-craft, but special-cased to CSS.
- The W3 NEON acceleration is **largely UNWIRED from the hot path.** Only `count_top_level_commas` is called by the generated parser, and only inside the *rich-summary projection* (cold), not the recognizer scan. `find_css_significant` and `find_comment_close` exist, are checkasm/scalar-parity tested, but are **dead at the admission path** — referenced only from `#[cfg(test)]` in `lib.rs`. The SK-V17 W3 commit title ("NEON structural-index acceleration of the CSS scan") **overstates** what landed in the benched recognizer.
- The **>SOTA timing comparison is a contrivance**, on two counts: (a) the timed corpus is the 85–357-byte SHA256-pinned micro-fixtures, **not** the real 71KB–495KB corpus (which is parity-checked but never timed); (b) the timed `lightningcss_facts` function does **strictly more work than the skinny path** — full lightningcss `StyleSheet::parse` + a projection walk + SHA256 + **a second full `cssparser` re-parse** — so the "skinny vs lightningcss" ratio is not a like-for-like parse race. Also `measure_mbps` is a **warm** benchmark (16 warmup + 2000 hot iters on one tiny buffer), violating the no-warm-benches discipline.

The >SOTA *parity* (9-field rich equality vs cssparser AND vs lightningcss-parseability) is genuine on the real corpus. The >SOTA *speed claim* as currently measured is not a fair comparison.

---

## Findings (severity + verdict, path:line)

### F1 — CSS parser is a hand-written const-string scanner, NOT grammar-derived. [HIGH / honest-hand-craft-but-overfit-at-inflection]

`skinny/crates/codegen/src/runtime_generator.rs:701` — `const CSS_GENERATED_RS: &str = r#"..."#;` spans **lines 701–1610** (~910 lines of the file; ~646 LOC of actual provider). Inside it is a complete recursive-descent CSS scanner: `CssFullParser` with `parse_stylesheet` (1160), `parse_at_rule` (1179), `parse_qualified_rule` (1210), `parse_block` (1228), `parse_block_item` (1248), `parse_declaration` (1280), `find_component_delim` (1357), `consume_balanced_at` (1393), `consume_string_at` (1426), `consume_comment_at` (1415).

The emit path proves the grammar never feeds the parser:
- `runtime_generator.rs:91` — `("generated.rs".to_string(), normalize(CSS_GENERATED_RS))` — the parser is the **verbatim const**, normalized only for indentation (`normalize`, 172).
- `runtime_generator.rs:76-103` — `emit_request_facts` is the CSS emit path (`RuntimeEmitterKind::RequestFacts`, dispatched at `:25`). The `facts: &grammar::RuntimeSourceFacts` argument feeds **only `config.rs`** (`render_request_facts_config`, `:105-134`) — request-identity constants (ROW_ID, ENTRY_RULE, import/layout/discard *counts*). The grammar contributes **no parser structure**.
- Contrast JSON: `RuntimeEmitterKind::CompiledLowering` (`:17-24`) calls `crate::emit_from_source(&request.grammar_name, &source.source)` — JSON genuinely lowers the grammar source. CSS does not route through this at all.

This is **declared** in the comment (`:683-700`): "the only per-grammar datum is which positions are pushed … never a hand-curated per-rule routing table." That declaration is accurate *about the tape*, but the recognizer *itself* is wholly hand-written. **This is exactly the SK-V16 finding ("CSS_GENERATED_RS was a 646-LOC hand-written tokeniser embedded as a const string") un-remediated.** Per user latitude this is acceptable *as a >SOTA proof*, but the file name `generated.rs` and the `@generated` header (`normalize` prepends `crate::GENERATED_HEADER`, `:174`) make a hand-written artefact masquerade as codegen output — a Lock-14/Pattern-H smell for AUDIT-3/4 to corroborate.

### F2 — The rich value API is hand-authored byte-heuristic classification, special-cased to CSS. [MEDIUM / honest-hand-craft]

`runtime_generator.rs:909-953` — `CssTypedValue` enum + `classify()` is a hand-written leading-byte dispatch: `b'#' => Color`, numeric-head → `Dimension`/`Number` via `number_has_unit` (`:1557`), ident-head → `Function`/`Keyword` via `leading_ident_is_function` (`:1583`). `selector_count` (`:849`) = `1 + count_top_level_commas(prelude)`. None of this is projected from `grammar/css/l4/color.bbnf`, `value-unit.bbnf`, `selectors.bbnf` etc. — the grammar's rich named types (the SPEC's `CssColor`/`CssDimension`/`CssFunction`/`Selector`) **do not exist**; W2 honestly substituted a "tape-supportable" coarse 6-way head-classifier. The W2 agent's own deviation note ("the SPEC's named rich types … did NOT exist in the benched tree") is corroborated here. Honest, but it is a **richer-summary count, not a grammar-derived typed CSSOM**.

### F3 — W3 NEON acceleration is UNWIRED from the hot recognizer; only the cold projection uses one primitive. [HIGH / claim-overstatement]

The hot scan `find_component_delim` (`runtime_generator.rs:1357-1380`) is **purely scalar** — a byte-at-a-time `while` loop. It does **not** call the NEON `find_css_significant`.

`grep` of all non-test callers (`skinny/crates/runtime/src/lib.rs`, generated CSS modules):
- `find_css_significant` — defined `runtime_simd.rs:169`; called **only** from `lib.rs:574` (`#[cfg(test)] fn neon_significant_skip_matches_scalar`). **Dead at admission.**
- `find_comment_close` — defined `runtime_simd.rs:112`; called **only** from `lib.rs:598,608` (test). **Dead at admission.** (The generated parser uses scalar `consume_comment_at`, `runtime_generator.rs:1415.)
- `count_top_level_commas` — defined `runtime_simd.rs:29`; the **only** NEON fn wired into a generated CSS module: `runtime_generator.rs:1509-1511` calls `crate::runtime_simd::count_top_level_commas`, reached from `selector_count` (`:857`) → reached only from `rich_summary` (`:1016`). The structural rich summary is **not the hot recognizer** — it's a post-parse projection over already-built tape nodes.

So the SK-V17 W3 commit `6bb4b2a6c` "NEON structural-index acceleration of the CSS scan" landed: a grammar-neutral, checkasm-validated SIMD library (`bbnf-simd` with `scalar/`+`aarch64/` dispatch and parity tests — genuinely good, see F6), **but did not wire it into the CSS recognizer's hot loop.** The acceleration of "the CSS scan" is, in the benched tree, one comma-count in the cold summary. This is a **scope-vs-claim gap**, not a contrivance, but it means the CSS hot path is **still scalar** and the >SOTA does **not** currently depend on NEON.

### F4 — The timed >SOTA comparison is not like-for-like, and runs on micro-fixtures, not the real corpus. [HIGH / CONTRIVANCE]

Two independent defects in the speed claim:

**(a) Comparator-plane mismatch.** The criterion bench (`skinny/crates/bbnf-bench/benches/nonjson_css_l4.rs:16,30`) times `track1_facts` vs `lightningcss_facts`. But `lightningcss_facts` (`src/nonjson_css_l4.rs:528-544`) does, per call: `validate_fixture_shape` (SHA256 of input, `:529`→`:1988-2007`) + `StyleSheet::parse` full lightningcss CSSOM (`:530`) + `collect_lightningcss_declarations` recursive projection walk (`:534`) + `expected_fixture_projection` compare (`:532`) + then **returns `cssparser_summary_facts`** (`:543`) which does **a whole second parse with cssparser** (`:512-521`). Meanwhile `track1_facts` (`:480-482`) does one tape summary. The timed "lightningcss" number is therefore `lightningcss-parse + cssparser-parse + sha256 + walk` — strictly more work than skinny's plane. **The 2–3.3x ratio is inflated by the comparator doing extra work**, not purely by skinny being faster.

**(b) Micro-fixture, not real corpus.** `measure_mbps` (`:3091-3114`) is invoked at `:962-965` on `input` = the **85–357-byte** SHA256-pinned fixtures (`EXPECTED_FIXTURE_BYTES = 187`, `:66`; stylesheet/selectors `117`, `:75`; at-rules `85`, `:102`). The real-world corpus — bootstrap 232KB, tailwind 179KB, material 495KB, animate 71KB (`css_l4_corpus.rs:21-51`) — is loaded via `load_all()` **only** in the rich-parity test (`:3547`), and is **never timed**. A 187-byte buffer fits in L1 and is dominated by call overhead; throughput on it is not a SOTA-representative figure.

**(c) Warm bench.** `measure_mbps` runs 16 warmup + 2000 hot iterations on the same buffer (`:3095-3102`), reporting steady-state warm Mbps. This violates the `no-warm-benches` discipline (cold per-parse only) that the JSON track honors.

### F5 — Per-fixture SHA256 + hardcoded expected-projection scaffolding embedded in the production bench crate. [MEDIUM / contrivance-scaffolding]

`src/nonjson_css_l4.rs:59-203` — seven fixtures each carry a hardcoded `*_FIXTURE_SHA256`, `*_FIXTURE_BYTES`, and `FIXTURE_TOKENS_0..6` / `FIXTURE_DECLS` spec tables; `validate_*_fixture_shape` (`:1988`, `:2009`, `:2030`, …) reject any input whose length/sha differs. `expected_fixture_projection` (`:2502`) is a hand-written expected `Vec<LightningDeclaration>`. This is **fixture-keyed admission**: the bench accepts exactly the captured fixtures and nothing else. It is test-fixture pinning, defensible as a regression guard, but it lives in the timed path (`lightningcss_facts` calls `validate_fixture_shape`) and couples the >SOTA measurement to specific captured bytes — the `CANONICAL_FIXTURE`/`CAPTURED_W2_INPUT` smell that PASS-IMPL §2 AUDIT-5 names. Cross-reference AUDIT-5.

### F6 — bbnf-simd primitives ARE grammar-neutral and validated (the one clean part). [POSITIVE]

`runtime_simd.rs:1-10` doc + impl: every entry takes the alphabet as **caller data** (`BRACKET_OPENS`/`CLOSES` `:17-18`, `delimiters`/`fixed` args `:169`). The kernel `bbnf_simd::prim::byte_class_from_eq_set_64` is shared with JSON's `scan_structurals`. `bbnf-simd/src/` has parallel `scalar/` and `aarch64/` implementations with a `dispatch.rs`, and checkasm parity tests (`bbnf-simd/tests/checkasm_*.rs` — comment_body_mask, byte_class, escape_mask, etc.). So the SIMD substrate is **not** aarch64-only-fragile and **not** CSS-special-cased. The defect is purely **non-wiring** (F3), not the primitives.

---

## Direct answers to the charged questions

**Is the CSS parser hand-written-template or grammar-derived?**
**Hand-written template.** A const `&str` (`runtime_generator.rs:701`) emitted verbatim. The `.bbnf` grammar is never consumed by the CSS emit path (`emit_request_facts`, `:76`). Identical to the SK-V16 finding; un-remediated.

**Rich value API origin — hand or grammar?**
**Hand.** `CssTypedValue::classify` byte-heuristics (`:929`), `number_has_unit` (`:1557`), `leading_ident_is_function` (`:1583`), `selector_count` (`:849`). The SPEC's named rich types do not exist; W2 honestly substituted a coarse head-classifier.

**CSS value types — hand-curated toml or grammar→projection?**
The 594-line `xtask/runtime-projections/css_l4.toml` exists and is referenced only by the Lock-14 baseline manifest (`lock14_baseline.rs:740,1180`) — it is **not** consumed to drive the parser or value API (no `.rs` outside the Lock-14 inventory reads it). The value classification is hand-coded in F2, not toml-driven. The toml is inert provenance, not a live projection source.

**The 9-field equality — genuine or hand-counted summary?**
**Genuine population-parity counts, but a coarse summary, not a rich CSSOM.** `CssRichSummary` (`:1046-1057`) = 4 structural + selectors + dimensions/numbers/colors/functions. `assert_rich_strict_equality` (`:451`) checks `track1_rich == cssparser_rich` on the real corpus (`:3547-3550`) — a real equality, independently verified against cssparser. But it is **9 integers**, not a materialized typed tree; lightningcss is only checked for *parseability* + a projection compare, never field-equal to skinny's 9 fields. Fair as a *parity* gate; the *speed* comparison riding alongside it is the contrivance (F4).

**NEON status?**
Grammar-neutral, checkasm-validated, scalar+aarch64 dispatch (F6) — but **unwired from the CSS hot recognizer** (F3). Only `count_top_level_commas` reaches a generated module, and only in the cold rich-summary. The hot scan is scalar.

**Inflection-point assessment for CSS — ready? does >SOTA depend on hand-shaping?**
**Not ready; PRUNE first.** The CSS path is the *most* contrived axis of the two grammars: a hand-written const-string parser with `@generated` headers, a NEON claim that didn't reach the hot loop, and a speed comparison that is both apples-to-oranges and micro-fixture-warm. Crucially: **the current >SOTA does NOT depend on hand-shaping for speed** — the hot path is scalar and runs on 187-byte buffers, so the measured ratio is comparator-overhead + cache-resident-warm, not a hand-tuned-inner-loop advantage. That is actually *good news for the backtrack*: there is no fragile hand-tuned kernel to preserve. The gap to grammar-driven is the recognizer *structure* (F1), which a real `.bbnf`→recognizer generator must now produce, plus *re-measuring* on the real corpus cold vs a same-plane lightningcss (F4).

---

## Prune / course-correct recommendations for SK-V18

1. **PRUNE-WAVE-CSS-A (measurement truthing) — do this BEFORE any speed claim.**
   - Time on the **real corpus** (`css_l4_corpus::load_all()`, the 71KB–495KB minified sheets), not the 85–357B fixtures. `measure_mbps` (`nonjson_css_l4.rs:3091`) must take a corpus iterator, not a single pinned buffer.
   - Make the comparator **same-plane**: time lightningcss `StyleSheet::parse` *alone* against skinny's recognizer+summary *alone*. Strip the `sha256 + cssparser-re-parse + projection-walk` from the timed `lightningcss_facts` (`:528`) — those belong in the equality assertion, not the timed fn.
   - Make it **cold** per the no-warm-benches discipline: drop the 2000-iter warm loop for a cold-per-parse harness mirroring JSON's.
   - Expect the 2–3.3x ratio to move materially once the comparator stops doing double work; the consolidated report must re-state CSS >SOTA against the corrected number.

2. **REBUILD-WAVE-CSS-B (grammar-driven recognizer) — the actual inflection backtrack.**
   - Route CSS through the same `emit_from_source(grammar_name, source)` path JSON uses (`runtime_generator.rs:23`), or a successor generator, so the recognizer is **derived from `grammar/css/l4/*.bbnf`**. Delete `CSS_GENERATED_RS` as a const (`:701`) once the generator emits an equivalent scanner.
   - Because the hot path is currently scalar and runs cache-resident, there is **no hand-tuned kernel to lose** — the backtrack risk is low. Validate the grammar-derived recognizer against the *existing* 9-field rich-parity oracle (`assert_rich_strict_equality`, `:451`) on the real corpus; that oracle is the one honest artefact to keep.

3. **WIRE-OR-RETIRE-WAVE-CSS-C (NEON honesty).**
   - Either wire `find_css_significant` / `find_comment_close` into the recognizer's hot `find_component_delim` (`:1357`) and `consume_comment_at` (`:1415`) so the W3 claim is true, OR retire them and correct the W3 ledger to say "NEON wired only in the cold comma-count projection." Do not leave checkasm-tested-but-dead acceleration in the tree while the commit title claims hot-scan acceleration.

4. **DE-SCAFFOLD-WAVE-CSS-D.** Move the per-fixture SHA256/byte-len/expected-projection pins (`:59-203`, `:1988+`, `:2502`) out of the timed path. Fixture-shape regression guards are fine in `#[test]`; they must not gate or sit inside `measure_mbps`-reached functions.

---

## Forward-lens note (for SK-V18 S-P0)

S-P0 should add a CHALLENGE addendum that this cycle's spec audit missed:
- **NEW-CH-V3-CSS-01 (generated-header truth):** any file whose name is `generated.rs` or carries `GENERATED_HEADER` MUST be reachable from a grammar-source→emit lowering; a hand-written const-string body normalized into a `generated.rs` is a fake-generated artefact. Grep gate: `CSS_GENERATED_RS`-style `const … &str = r#"…parse…"#` blocks in `codegen/`.
- **NEW-CH-V3-CSS-02 (timed-plane symmetry):** the timed comparator function and the timed skinny function MUST do equal work (parse-only vs parse-only). Any `validate_*`/`sha256`/second-parser call inside a criterion-timed fn is a comparator-inflation contrivance.
- **NEW-CH-V3-CSS-03 (corpus-in-the-timer):** the corpus that is *parity-checked* (`load_all`) MUST be the corpus that is *timed*. A real corpus checked for equality but a micro-fixture used for Mbps is a broadcast/representativeness contrivance.
- **NEW-CH-V3-CSS-04 (acceleration-wiring):** a SIMD/NEON wave's commit claim ("acceleration of the X scan") MUST be backed by a non-test caller of the primitive on the hot path; checkasm-tested-but-`#[cfg(test)]`-only callers do not satisfy an acceleration claim.

**One-line trajectory:** CSS is >SOTA on parity and plausibly on speed, but the SK-V17 implementation proved it with a hand-written parser, an unwired NEON claim, and an apples-to-oranges warm micro-bench — SK-V18 must PRUNE the measurement contrivance and REBUILD the recognizer grammar-driven *before* the CSS axis can claim the inflection point.
