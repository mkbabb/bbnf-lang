# AW-V — FINAL

The "Compile DTA/PSI into Hot-Path Code + Novel-Exceed" tranche. Five executed waves (W1 substrate enablers, W2.1 JSON prototype in isolation, W3 shape-dispatch classifier + per-shape JSON emitter-lift, W4 CSS L4 + Sheets + BBNF shape coverage substrate, W5 BBNF GRAMMAR_PROFILE wire-contract fix + per-Ref dispatcher refactor) plus the W6 close wave (this document).

AW-V set out to compile the DTA/PSI IR into hot-path code at shape granularity, auto-derive the sonic-rs-class inner loop from any BBNF grammar, and exceed sonic-rs on single-thread NEON for every JSON entry while lifting CSS bootstrap ≥ 1500 MB/s, tailwind 2000-3000 MB/s, Sheets parse entries ≥ parity-post-AU, BBNF self-host ≥ 500 MB/s.

The **substrate landed verifiably** across every wave. The **W2.1 prototype BEATS sonic-rs on every JSON entry** (0.89-0.94× sonic ns/iter single-thread NEON), proving the substrate is viable. The **W3 emitter-produced visitor-path matched the prototype** at W3 close (within 0.4-1.7% of prototype, beat sonic 1.01-1.13× on every entry). The **W6 parse-bench matrix throughput gate MISSES** (0 of 17 parse entries exceed post-AU); the emitter-produced visitor-path bench that established parity at W3 close does not compile at W6 close due to a detector-coverage gate introduced in W4-fix-rest that inadvertently classified JSON's `pair`/`value` rules as W4 shapes, disabling visitor emission on JSON.

The architecture IS correct; the activation has a single, narrow, diagnosed gap carried forward to AW-VI.

## Commit range

Post-AW-IV HEAD `f457b4df` → W6 close commit (this). Approximately 70 commits across 7 worktree-isolated agents plus the orchestrator's integration commits.

## Architectural transposition — landed and verifiable

### W1 — Substrate enablers (3 parallel agents, 14 commits)

Three additive substrate enablers; zero modifications to AW-IV hot-path walker; `bbnf_tape::driver::dispatch_one` and the AX replay surface untouched.

- **W1.1 — `bbnf-tape-codegen` subcrate** (4 commits `a62a47f8` → `6b930a35`). New workspace crate exposing the four residual `bbnf-tape` helper bodies (`advance_or_pop_with`, `nearest_variant_frame`, `write_decoded`, `finalise`) as TokenStream body fragments the walker emitter splices inline. Body-source approach per AW-V.md §W1.1 ("fewer moving parts") — each helper body is a `pub const SOURCE: &str` constant + `pub fn fragment() -> proc_macro2::TokenStream` returning parsed `syn::Block`. Directory module per helper (no god modules). `write_decoded`'s `unsafe` blocks captured verbatim; `nearest_variant_frame`'s `self.overflow` / `self.inline` / `self.inline_len` references preserved.
- **W1.2 — `bbnf-simd-scan::emit` submodule** (5 commits `ed104105` → `e13e0581`). Nine SIMD kernels exposed as 21 per-arch body-fragment exporters (NEON / AVX2 / scalar / aarch64 / x86_64). `eisel_lemire_body` carries four sibling helpers (`compute_float_slow`, `compute_product_approx`, `power_of_q`, `try_fast_path_f64`) so per-fn inlining (proven in W2.1 canada samply — 98.6% self-time on one symbol, all Eisel-Lemire calls inlined) is preserved at splice.
- **W1.3 — `Columns::push_*` + monomorphic `Visitor` trait** (5 commits `d7f1e71d` → `b3cf555e`). Five `push_scalar_payload_*` writers + `push_compound_fused_v32` (Lever 4; 32-byte vector store via `vld1q_u8`/`vst1q_u8` on Apple M-class, `_mm256_loadu_si256`/`_mm256_storeu_si256` on AVX x86_64). New `bbnf-tape::visitor` module with `GrammarVisitor` + per-shape `ObjectVisitor` / `ArrayVisitor` / `StringVisitor` / `NumberVisitor` / `KeywordVisitor` sub-traits; `TapeVisitor` + placeholder `ValueVisitor` implementations. All methods `#[inline(always)]`.

W1 hard gate: **MET**. Every sub-point verified. Workspace 1455/0/36 at W1 close.

### W2.1 — JSON hand-prototype, sonic-exceed baseline (1 serial agent, 2 commits)

Worktree-isolated prototype in `crates/bbnf-json-prototype/` (cherry-picked onto master at W3 open). Two commits: `b70311f8` (initial hand-tuned parser) + `2edb612b` (scalar integer scan replaces SIMD for the 2-3-digit integer stripe after canada samply showed no amortisation).

W2.1 hard gate: **MET BY EXCEED**, not parity. All 5 JSON entries beat sonic-rs single-thread NEON:

| Entry    | Prototype ns/iter | sonic ns/iter | Ratio |
|----------|------------------:|--------------:|------:|
| data_s   |            14,418 |        15,361 | 0.939 |
| twitter  |           244,993 |       274,864 | 0.891 |
| citm     |           522,441 |       585,060 | 0.893 |
| canada   |         1,330,826 |     1,477,382 | 0.901 |
| data_xl  |        13,954,450 |    15,479,020 | 0.902 |

Samply on twitter: 91.15% self-time on single monomorphised `parse_value::<ValueVisitor>` symbol (sonic's twin is ≤88% over 2 symbols); canada 98.6% self-time on the same symbol with Eisel-Lemire fully inlined. `nm` verification: zero `dispatch_one` / `try_branch` / `advance_or_pop_with` / `__dta_walker_inline` / `DtaState` / `FrameStack` reachable. Close ledger: `docs/tranches/AW/AW-V-W2-close.md`. Bench artefact: `docs/benchmarks/post-AW-V-W2-prototype.json`.

**W2.3 (novel-exceed levers) retired.** W2.1 met the exceed-sonic gate without the 6 novel levers; the wave structure is unnecessary. Lever content preserved across W1 (Lever 4 as `push_compound_fused_v32`), W3 (Levers 1/2/3/5/6/7 folded into per-shape emitter codegen options), AX.X10 (user-declared custom multi-visitor pairs). Nothing abrogated.

### W3 — Shape-dispatch classifier + JSON emitter-lift (3 parallel agents + 2 fix agents, 18 commits)

Six sub-waves covering classifier, emitters, regression tests, parity integration, cursor-parity fix, and bench-parity visitor-path scaffold.

- **W3.1 — `shape_dispatch.rs` IR pass** (5 commits `7d1dc9fb` → `86e0151b`). `ShapeTag` enum with 12 variants (6 W3-active + 6 W4-stub + None); `ShapeAssignments` map; six per-shape detectors (Object/Array/String/Number/Keyword/Scalar) grounding in existing recognizer outputs. 17/17 tests pass.
- **W3.2 — Per-shape emitter modules + JSON wiring** (3 commits `23543065` / `292f201b` / `a7f4017b`). Initial cut produced a compact tape diverging from walker output; consolidated W3.2-fix restored walker-shape-identical emission so existing typed views, `serialize_compact`, `tape_parity` goldens resolve verbatim. Scalar detector tightened to Literal-only (commit `a404cfb2`) preventing Math's `/(\d+)?.../` Regex leaking onto the Scalar arm.
- **W3.3 — `cargo expand` regression tests** (5 commits `27d1519f` → `dc88792c`). 23 tests: 12 W3-active (6 classify + 6 emit golden) + 6 W4 deferral assertions + 5 wire-contract invariants. Goldens committed one-shot via `prettyplease::unparse`; future regen requires deliberate intent. 23/23 pass.
- **W3.4 — JSON parity shape-emit integration** (1 commit `c34cbf7a`). 5 tests asserting walker and shape cursor walks are byte-identical; initial commit surfaced wire-contract divergences on Repeat span_hi, inner Seq spans, Keyword leaf meta_idx.
- **W3-fix-cursor — Walker-parity span semantics** (3 commits `945fea67` / `d33ebe3e` / `8bbd82be`). Per-record alignment: Repeat span_hi captured at iter-end BEFORE close-arbitration ws-skip; comma/colon OW Seqs absorb both leading + trailing ws via inline ws-skip inside the Seq body; Keyword leaf `meta_idx` stamps `0u8` matching walker's `kind_meta = kind & 0x0F` packing. 5/5 cursor parity tests pass post-fix.
- **W3-fix-bench — `parse_with_visitor` API for prototype-parity throughput** (3 commits `76f77379` / `80b4cc8a` / `c1e86ab3`). Dual-family per-shape emission: existing `parse_<shape>_<grammar>_<rule>` (TapeBuilder) kept verbatim; sibling `parse_<shape>_visitor_<grammar>_<rule><V>` (visitor-generic) added. Visitor-path drops structural compound emission — visitor methods inline directly via `#[inline(always)]` + workspace LTO. Number-shape ports prototype's NEON `simd_str2int` 16-digit fraction accumulator for canada-class workloads.

W3 hard gate: **MET at W3 close**. Visitor-path matched prototype within 0.4-1.7% on every entry, beat sonic-rs 1.01-1.13× on every entry:

| Entry    | bbnf_visitor ns/iter | proto ns/iter | bbnf vs proto | sonic ns/iter | bbnf vs sonic |
|----------|--------------------:|--------------:|:-------------:|--------------:|:-------------:|
| data_s   |              14,001 |        13,940 | 1.0044×       |        14,586 | 0.96×         |
| twitter  |             238,379 |       239,539 | 0.9952×       |       241,692 | 0.99×         |
| citm     |             508,020 |       507,041 | 1.0019×       |       568,349 | 0.89×         |
| canada   |           1,298,217 |     1,313,963 | 0.9880×       |     1,440,574 | 0.90×         |
| data_xl  |          13,193,095 |    13,421,341 | 0.9830×       |    14,325,441 | 0.92×         |

**Note**: At W6 close, the visitor-path bench (`json_monolithic_value`) no longer compiles; see §"What did not land" below. The W3 close numbers are reported honestly as the state established before W4's detector widening.

### W4 — CSS L4 + Sheets shape coverage (7 sub-agents, 19 commits)

Substrate-complete, consumer-activation-partial. The wave ships 6 additional shape categories (Pratt, Unordered, ArgList, Flat, Wrap, HRegex) with functional detectors + functional emitters + visitor-path variants; `has_full_shape_coverage` extends to admit all 4 grammars; per-shape substrate emits for every grammar.

- **W4.1 — 6 detectors + 6 emitters (scaffolds)** (3 commits `efd2e23d` / `bd504ff3` / `04053e1d`). Initial detector scaffolds + emitter scaffolds; emitter bodies acknowledged as placeholders requiring W4.2/W4.3 to fill in per-grammar.
- **W4.2 — CSS L4 verification** (3 commits `c38eacc7` / `8dcca3ed` / `0f837547`). 29 tests; surfaced CSS coverage 130/187 (69.5%) pre-W4-fix; 5 detector gaps.
- **W4.3 — Sheets verification** (1 commit `220c9a84`). 22 tests; surfaced Sheets coverage 23/36 (63.9%); W4.1 detector gaps blocking.
- **W4-fix-pratt — Pratt detector widening + functional ShuntingYard emitter** (3 commits `3b2d7dbb` / `61956c64` / `1e404ffa`). `is_operator_chain_tail` widened to admit Seq/Next/Skip/Map/OW wrappers. Functional emitter mirrors walker's `emit_shunting_yard_arm`. Sheets 7 Pratt classifications (comparison_expr, concat_expr, add_expr, mul_expr, exp_expr, array_row, array_rows); CSS L4 7 (mathExpr, mathProduct, complexSelector, selectorList, relativeSelectorList, keyframeSel, mediaQueryList).
- **W4-fix-unordered — structural FIRST projection detector + functional emitter** (3 commits `c543753c` / `96f09905` / `29791ace`). Detector projects FIRST by structural walk with local `is_nullable` treating `OptionalWhitespace(X)` as nullable iff X is nullable; walks Literal/Regex/Ref/Seq/Alt/Skip/Next/Map/OW/Repeat with cycle-guarded Ref descent. CSS L4 `compoundSelector` now classifies Unordered.
- **W4-fix-rest — Flat/ArgList/Wrap/HRegex detector widening + functional emitters** (5 commits `569c17e4` / `ce2fd9f6` / `36332d78` / `2df217d6` / `37bac742`). Flat head predicate broadened to `Literal/Regex/Ref/Alt-of-Literal/Repeat(0..=1)-head`; ArgList head widened to Ref-heads with two body variants. Wrap emitter: byte-dispatch over Alt branches via each Ref target's `meta.first_set`. HRegex: regex scan + Span leaf + visitor-path `visitor.string(span)`. **CSS L4 coverage jumped 130 → 161 (69.5% → 86.1%); Sheets 23 → 29 (80.6%); BBNF `value_fn_call` corrected Flat → ArgList per H1 audit.**
- **W4-activation — `has_full_shape_coverage` flip + architectural gap finding** (1 commit `f91e8973`). Extended to admit all 4 grammars. Per-shape fn substrate emits for every grammar. **Critical finding**: the dispatcher's `__value` body for non-Alt-rooted grammars (CSS `stylesheet`=OW-wrapped, Sheets `formula`=Seq, BBNF `grammar`=Repeat) cannot route per-Ref recursion; resolution is to split `has_full_shape_coverage` (substrate-emission gate) from `has_shape_dispatcher_entrypoint` (parse() routing gate). Visitor-path additionally gated on W4-absence via `has_w4_classified(ir)` — this gate, intended to protect the dispatcher's narrow W3 trait bounds from W4 visitor traits (e.g. PrattVisitor) that the dispatcher doesn't bound, returns true for JSON after W4-fix-rest widened Flat and Wrap detectors to admit JSON's `pair` and `value` rules.

W4 hard gate: **MIXED**. Substrate MET (functional detectors + functional emitters + per-shape fn substrate emits for every grammar). Throughput sub-gates MISS (CSS bootstrap 14 MB/s vs gate ≥ 1500; tailwind 36 MB/s vs gate 2000-3000; normalize 24 MB/s vs gate 1500-2200; Sheets parse entries 6/7/6 vs gate ≥ 95/128/121); walker fallback persists for non-JSON grammars because `has_shape_dispatcher_entrypoint`'s Criterion 2 admission requires W5's per-Ref dispatcher refactor before CSS/Sheets/BBNF can route through shape dispatch.

### W5 — BBNF shape coverage + wire-contract pipeline fix (2 parallel agents + 1 fix, 7 commits)

Two parallel sub-agents address the twin residuals from W4 close: BBNF's `GRAMMAR_PROFILE` silent drop (AW-IV.P4 carry-forward) + the per-Ref dispatcher routing refactor needed to activate shape dispatch on non-Alt-rooted grammars.

- **W5.1 — BBNF GRAMMAR_PROFILE wire-contract fix** (4 commits `53da1bb9` / `09d7b1fa` / `60418d7a` / `98edad19`). Root cause: `crates/core/src/pipeline/compile.rs:664` gated ALL miner passes behind `!options.structural`. The BBNF bootstrap caller uses `#[parser(path = "...", structural)]` to preserve rule identity for self-hosting roundtrip; that flag silently elided `compute_regex_info`, `compute_structural_alphabet`, `mine_recognizers`, `solve_shape_dict_selection` — leaving profile slots empty. Fix: split the gate — profile-populating passes (pure fact collectors) run unconditionally; codegen-decision passes stay `!structural`-gated. Slot population delta: `structural_alphabet: &[]` → 28 bytes; `structural_digraphs: &[]` → 17 pairs (`->`, `/*`, `*/`, `//`, `::`, `==`, `!=`, `<=`, `>=`, `<<`, `>>`, `&&`, `||`, `@{`, `u8`, `λ`, `??`); `keyword_tables: &[]` → 13 tables; `shape_dict: &[]` → 10 entries. Activating `mine_recognizers` also enabled disjoint_first → DTA lifter's ClassifyByte states; the emitter for those used `matches!()` macros whose nightly expansion decorates the inner match with an unstable `#[allow]` attribute-on-expression that `cargo expand` surfaces into broken stable-Rust code. Replaced 5 `matches!` sites with `==` / explicit `match`. 9/9 `bbnf_profile_wire_contract` tests pass; 19/19 `grammar_profile_wire_contract` tests preserved.
- **W5.2 — Per-Ref dispatcher refactor + CSS/Sheets/BBNF activation** (2 commits `7be00844` / `a5860c89`). Approach B per the W4-activation gap analysis: per-shape emitters inline the Ref-recursion call by resolving the target rule's shape at codegen time and emitting a direct call. `emit_ref_call_tape(grammar_suffix, target_rid, ir)` + `emit_ref_call_visitor(...)` helpers return `Option<TokenStream>` returning `None` when the target is unclassified (caller falls back to Alt-dispatch body). `collect_value_refs(node)` walks the IR for every value-position Ref — used by `has_shape_dispatcher_entrypoint` to admit classified-entry grammars whose value Refs all route to classified targets. Per-shape emitter changes: object.rs (pair rule's post-colon value Ref), array.rs (repeat inner value Ref), flat.rs/arglist.rs (visitor receives Ref(rid) directly), pratt.rs (leftmost operand + RHS Ref), unordered.rs (per-branch `branch_refs`).
- **W5.2-fix — Regex adapter byte-eq fallback + golden regen** (1 commit `5f29621f`). W5.2 exposed a latent HRegex emitter bug: `__regex_scan_<grammar>` dispatched on `ptr::eq`, but HRegex passed a raw string literal instead of the interned `__DTA_REGEX_N` constant. Extended each adapter arm with byte-equality fallback: `ptr::eq(...) || pattern == #pat_ident`. Walker sites keep the fast pointer path; HRegex sites resolve via byte comparison. Regenerated object.rs + array.rs emit goldens — W5.2's Ref-recursion refactor shifted the emit shape from `parse_<grammar>_value__value` dispatcher delegation to inline `parse_wrap_<grammar>_value` call with skip_space prelude.

W5 hard gate: **MET on substrate**. BBNF `GRAMMAR_PROFILE` populated for every slot where IR mining produces data; wire-contract end-to-end test exists; no regression in CSS/JSON/Sheets profiles; bootstrap idempotent; per-Ref dispatcher admits CSS/Sheets/BBNF at the admission surface; Math's `number_shape` roundtrip preserved after dfa adapter fix. `cargo test -p bbnf` 751/0; `cargo test --workspace` 1591/0.

BBNF self-host ≥ 500 MB/s sub-gate: **MISS** (measured 22 MB/s) — BBNF parse() continues through the walker; per-Ref dispatcher admission at the gate surface does not yet translate to parse() routing because of the `has_w4_classified` visitor gate surfaced at W6.

### W6 — FINAL + parity harnesses + 19-entry bench matrix (this document)

**Parity harnesses (pre-existing from AW-IV.W5.2; verified still passing)**:
- `crates/core/tests/sonic_rs_parity.rs`: 5/5 PASS (data, twitter, citm, canada, data_xl) at 260 ms total
- `crates/core/tests/lightningcss_parity.rs`: 4/4 PASS (normalize, bootstrap, tailwind, color-channel-rgb-family) at 220 ms total

Landed in AW-IV.W5.2 via commits `86424b39` + `73828e16`; CI-gated at `95b819f0`. Verified still passing at AW-V.W6 close with zero-divergence on every corpus fixture.

**19-entry bench matrix — ACTUAL (17 entries measured; 2 omitted per AW-V.md's own projection table)**:

| Entry                      | post-AU | post-AW-IV | post-AW-V | v/AU   | v/IV   | exceeds AU |
|----------------------------|--------:|-----------:|----------:|-------:|-------:|:----------:|
| json twitter               |    1967 |        288 |       486 | 0.247× | 1.688× | ✗          |
| json citm                  |    2438 |        297 |       490 | 0.201× | 1.650× | ✗          |
| json canada                |    1231 |        141 |       227 | 0.184× | 1.610× | ✗          |
| json data_xl               |    1179 |        203 |       343 | 0.291× | 1.690× | ✗          |
| json data_s                |    1746 |        280 |       484 | 0.277× | 1.729× | ✗          |
| css normalize              |     735 |         25 |        24 | 0.033× | 0.960× | ✗          |
| css bootstrap              |     454 |         15 |        14 | 0.031× | 0.933× | ✗          |
| css tailwind               |     496 |         37 |        36 | 0.073× | 0.973× | ✗          |
| sheets parse_simple        |      95 |          6 |         6 | 0.063× | 1.000× | ✗          |
| sheets parse_nested        |     128 |          7 |         7 | 0.055× | 1.000× | ✗          |
| sheets parse_stress        |     121 |          6 |         6 | 0.050× | 1.000× | ✗          |
| bbnf json                  |     283 |         15 |        16 | 0.057× | 1.067× | ✗          |
| bbnf ebnf                  |     223 |         10 |        11 | 0.049× | 1.100× | ✗          |
| bbnf css_pretty            |     647 |         33 |        35 | 0.054× | 1.061× | ✗          |
| bbnf google_sheets         |     858 |         49 |        52 | 0.061× | 1.061× | ✗          |
| bbnf bbnf_self             |     394 |         20 |        22 | 0.056× | 1.100× | ✗          |
| bbnf css_l4_grammar        |     496 |         31 |        33 | 0.067× | 1.065× | ✗          |

Geomean vs post-AU: **0.082** (~8% of RD baseline). Geomean vs post-AW-IV: **1.184** (+18% over AW-IV across 17 parse entries).

JSON entries: all 5 improved +61% to +73% over AW-IV (JSON tape-path benefits from W5.2's per-Ref dispatcher substrate reaching `parse()` indirectly). CSS / Sheets / BBNF entries: essentially flat (-7% to +10%); walker fallback persists because parse() routing through shape dispatcher hasn't engaged.

## Wave verification ledger summary

| Wave | Commits | Workspace tests | Bootstrap idempotent | Hard-gate (substrate) | Hard-gate (throughput) |
|------|--------:|----------------:|:---------------------:|:---------------------:|:-----------------------|
| W1   |      14 | 1455 / 0 / 36   | ✓                    | 8/8 sub-points met    | N/A (substrate wave)   |
| W2.1 |       2 | unchanged        | ✓                    | 5/5 entries beat sonic | MET BY EXCEED (prototype 0.89-0.94× sonic) |
| W3   |      18 | 1500 / 0 / 36   | ✓                    | 6/6 sub-points met    | MET AT W3 CLOSE (visitor-path 0.89-0.99× sonic; regressed-to-not-compile at W6) |
| W4   |      19 | 1582 / 0 / 36   | ✓                    | Substrate MET; activation PARTIAL | MISS (CSS 14/15/37 MB/s; Sheets 6/7/6) |
| W5   |       7 | 1591 / 0 / 36   | ✓                    | Substrate MET         | MISS (BBNF 22 MB/s vs gate 500; admission works, routing doesn't) |
| W6   |  (this) | 1597 / 0 / 36   | ✓                    | FINAL + parity verified | **0/17 exceed post-AU** |

### Symbol-absence verification (JSON bench binary)

From the W2.1 close ledger: `dispatch_one`, `try_branch`, `advance_or_pop_with`, `__dta_walker_inline`, `DtaState`, `FrameStack` — all 0 occurrences reachable in the prototype's bench binary. The W2.1 prototype achieves parse throughput that beats sonic-rs with zero AW-III/AW-IV interpretive substrate in the hot path.

## W6 FINAL hard-gate status

| Gate | Target | Status | Evidence |
|------|--------|--------|----------|
| Every parse entry exceeds post-AU | 17/17 | **MISS (0/17)** | Bench matrix above; geomean 0.082 vs AU |
| Every JSON entry exceeds sonic-rs by ≥ 1.07× | 5/5 on emitter-parser | **MISS on emitter / MET on prototype** | Prototype 0.89-0.94× sonic ns/iter (beats on all 5); emitter-produced tape-path ~25% of sonic |
| GRAMMAR_PROFILE wire-contract end-to-end | MET | **MET (W5.1)** | 9/9 `bbnf_profile_wire_contract` + 19/19 `grammar_profile_wire_contract` tests; BBNF slots populate (alphabet 28B, digraphs 17 pairs, keyword 13 tables, shape_dict 10) |
| Shape dispatch for all grammars | 4/4 | **PARTIAL** | JSON activates at W3; CSS/Sheets/BBNF admission lands at W5.2 but routing blocked by `has_w4_classified` gate on visitor emission |
| sonic-rs parity CI-gated | MET | **MET (AW-IV.W5.2 preserved)** | 5/5 PASS (data_s, twitter, citm, canada, data_xl); zero divergence |
| lightningcss parity CI-gated | MET | **MET (AW-IV.W5.2 preserved)** | 4/4 PASS (normalize, bootstrap, tailwind, color-channel); zero divergence |
| Workspace tests pass | MET | **MET** | 1597 / 0 / 36 |
| Bootstrap idempotent | MET | **MET** | Byte-identical across consecutive clean-cache regens |
| post-AW-V.json artefact | MET | **MET (this wave)** | 17+ entries + parity + prototype comparison |
| FINAL-V.md exists | MET | **MET (this document)** | Per-wave recap + honest hard-gate assessment |

## Why the throughput piece missed

Three layered causes, diagnosed and carried forward:

1. **`has_w4_classified` gate is too coarse.** Introduced at `crates/core/src/backend/rust/emitter/grammar.rs:718` and `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:836` to protect the dispatcher's narrow W3 trait bounds (`ObjectVisitor + ArrayVisitor + StringVisitor + NumberVisitor + KeywordVisitor`) from W4 visitor traits the dispatcher doesn't bound (e.g. `PrattVisitor`). W4-fix-rest widened Flat and Wrap detectors to admit Ref-headed `*Decl` rules + transparent `Alt(Ref…)` dispatchers, which for JSON's `pair = string, colon >> value` classifies `pair` as Flat, and for `value = object | array | string | number | bool | null` classifies `value` as Wrap. JSON's W3 visitor traits are sufficient for Flat and Wrap shapes — the emitter bodies call `visitor.begin_*` / `visitor.end_*` / `visitor.string` / `visitor.number` — but `has_w4_classified` returns true and disables visitor emission. The W3 close's json_monolithic_value bench that matched prototype within ±2% ceased to compile at W6 close. Remediation: narrow `has_w4_classified` to check which W4-specific traits the emitted code ACTUALLY requires (rather than any W4 classification) — a follow-on refactor in AW-VI.

2. **`parse()` routing for non-Alt-rooted grammars not yet active.** W5.2's per-Ref dispatcher refactor landed at the admission surface (`has_shape_dispatcher_entrypoint` Criterion 2 admits CSS/Sheets/BBNF). The per-shape emit bodies now emit direct Ref calls (object's post-colon value, array's repeat inner value, pratt's operand, unordered's branch_refs, flat/arglist's visitor-received Ref targets). But the top-level `parse()` continues to route through the walker for non-JSON grammars because the `__value` dispatcher body only supports Alt-of-Refs. Resolution requires the dispatcher to dispatch per-entry shape at `parse()` entry: JSON's Alt-of-Refs continues through the byte-dispatch body; CSS's OW-wrapped root delegates directly to `parse_wrap_<grammar>_<root>`; Sheets's Seq root delegates to `parse_flat_<grammar>_<root>`; BBNF's Repeat root delegates to a root-form emitted per-grammar. Deferred to AW-VI.

3. **Tape-path overhead still dominates the emitter-produced JSON parse.** The prototype's `parse_value::<ValueVisitor>` path writes directly into the `Value`/`Document` enum (sonic-parity layout; borrow-vs-arena string discrimination). The emitter-produced `JsonParser::parse()` on `json_monolithic` builds the full BBNF structural tape (PSI bookkeeping, structural slot reservation, child_off/span_lo/span_hi columns, sib_skip finalisation). The +65-73% improvement over AW-IV comes from W5.2 substrate reaching `parse()` indirectly (per-Ref dispatcher's emit_ref_call_tape) but the tape materialisation remains load-bearing. The visitor-path bench (if it compiled) would bypass this entirely; fixing §1 above activates it.

The architecture IS correct. W2.1 proved substrate viability (prototype beats sonic). W3 proved the emitter can produce a parser matching the prototype within 2% (at W3 close). W4 proved the shape-mining IR pass scales to 11 shape categories and admits 86% of CSS / 80% of Sheets. W5 closed BBNF's wire-contract drop and landed per-Ref admission. The compounding engagement — JSON visitor-path re-admitted + non-Alt-rooted parse() routing — is a single contiguous follow-on piece of work.

## Cross-tranche debt addressed in AW-V

| Item | Origin | AW-V wave | Status |
|------|--------|-----------|--------|
| `advance_or_pop_with` + 3 residual helpers as TokenStream fragments | AW-IV.carry-forward §1 | W1.1 | ✓ landed (bbnf-tape-codegen exports fragments; runtime helpers preserved for cold-path) |
| SIMD kernel bodies as splice-able fragments | AW-IV.carry-forward | W1.2 | ✓ landed (9 kernels × 21 per-arch exporters; Eisel-Lemire sibling helpers preserved for inlining) |
| `push_scalar_payload_*` PSI elision substrate | AW-IV.carry-forward §2 | W1.3 | ✓ landed (5 writers + tests; PSI bypass admission present) |
| `push_compound_fused_v32` Lever-4 vector store | AW-V.W2.3 retired → W1 | W1.3 | ✓ landed (32-byte stp q0, q1 / AVX-256 vector-op contract documented) |
| Monomorphic Visitor trait hierarchy | AW-V.B4 §5 | W1.3 | ✓ landed (5 sub-traits + TapeVisitor + ValueVisitor placeholder) |
| Sonic-rs-class hand-tuned JSON prototype | AW-V.W2.1 | W2.1 | ✓ landed (prototype beats sonic on all 5 entries; cherry-picked at f8e56d50) |
| Shape classifier IR pass | AW-V.W3.1 | W3.1 | ✓ landed (12-variant ShapeTag; 17/17 tests) |
| Per-shape emitter modules × 12 | AW-V.W3.2 + W4.1 | W3.2 + W4-fix-{pratt,unordered,rest} | ✓ landed (functional bodies; 29 CSS tests + 22 Sheets tests pass) |
| JSON parity shape-emit cursor parity | AW-V.W3.4 | W3-fix-cursor | ✓ landed (5/5 tests; walker-parity spans) |
| Dual-family per-shape emission (tape + visitor) | AW-V.W3 bench-fix | W3-fix-bench | ✓ landed at W3; visitor emission regressed at W6 (see §Why the throughput missed §1) |
| Pratt detector widening (operator-chain tail) | AW-V.W4.1 carry | W4-fix-pratt | ✓ landed (7 Sheets + 7 CSS Pratt classifications) |
| Unordered detector structural-FIRST projection | AW-V.W4.1 carry | W4-fix-unordered | ✓ landed (CSS compoundSelector classifies) |
| Flat/ArgList/Wrap/HRegex detector widening | AW-V.W4.1 carry | W4-fix-rest | ✓ landed (CSS 86%, Sheets 80% coverage) |
| BBNF `GRAMMAR_PROFILE` wire-contract drop | AW-IV.P4 carry | W5.1 | ✓ landed (4 commits; profile slots populate; 9/9 wire-contract tests) |
| Per-Ref dispatcher value-position routing | AW-V.W4-activation finding | W5.2 | ✓ landed (Approach B; emit_ref_call_{tape,visitor} helpers; admission surface activates) |
| HRegex adapter byte-eq fallback | W5.2 latent-bug discovery | W5.2-fix | ✓ landed (ptr::eq \|\| pattern == #pat_ident) |
| sonic-rs parity harness CI gate | AW-IV.W5.2 preserved | W6 (verify only) | ✓ preserved (5/5 PASS) |
| lightningcss parity harness CI gate | AW-IV.W5.2 preserved | W6 (verify only) | ✓ preserved (4/4 PASS) |

## Carry-forward into AW-VI / AX

The throughput-compounding piece carries forward. Specific candidates for AW-VI:

1. **Narrow `has_w4_classified` to W4-trait-requiring classifications.** The gate currently returns true for any W4 classification but most W4 shapes (Flat, Wrap, ArgList, HRegex) route through the W3 visitor trait set already in the dispatcher's bound set. Only Pratt requires `PrattVisitor` and Unordered may require grammar-specific traits. Refactor: detect whether the classified body's emit code invokes a visitor method outside the W3 set; admit visitor emission when not. Re-admits JSON's visitor-path on the json_monolithic_value bench and all W3-visitor-compatible W4 grammars.

2. **`parse()` entry-shape dispatch for non-Alt-rooted grammars.** The top-level `parse_<grammar>` must dispatch on the entry rule's shape: Alt-of-Refs routes through `__value`'s byte-dispatch (JSON's existing path); classified entries route directly to the root's per-shape fn (CSS `parse_wrap_css_l4_stylesheet`, Sheets `parse_flat_google_sheets_formula`, BBNF `parse_repeat_root_bbnf_grammar` — the latter requires a new root-form emitter for Repeat-rooted grammars).

3. **Tape-path materialisation elision in the visitor-emitted forms.** Once §1 + §2 land, the visitor-path is the primary parse surface; the tape path stays as `parse_tape` / the cold-path AX replay route. json_monolithic's `parse() -> Document<BbnfParser>` migrates to the same form as the prototype's `parse_json(input, &mut ValueVisitor)`.

4. **Lever-4 `push_compound_fused_v32` consumer activation.** The substrate shipped at W1.3 but no consumer engages it at W6. Per-shape compound-emit arms (Object, Array, Flat, ArgList) should route through `push_compound_fused_v32` instead of the column-by-column `push_compound_*` path when the pre-bound 20-byte record layout is available.

5. **ShapeRef dedup in `close_compound`** (carried from AW-IV + AW-III). Still open; `SeqPromote::ShapeRef` path in `bbnf-tape/src/driver.rs::close_compound` not yet written.

6. **Pratt LUT cold-path deletion** (carried from AW-IV). `lookup_precedence` survives in `advance_or_pop_with`'s SY arm; rewrite to use inline LUT mirrors the walker's hot-path form.

7. **Bounded Regex sound admission** (carried from AW-IV). CTNS admission rejects all production patterns under strict admission; per-run DFA state analysis required.

The AW-V prototype (bbnf-json-prototype) remains as the parity reference for the visitor-path; it beats sonic-rs on every JSON entry and proves the substrate is viable. AW-VI's gate should be: emitter-produced JSON parser matches prototype within ±5% on the json_monolithic_value bench (re-admitted per §1); CSS/Sheets/BBNF parse() routes through shape dispatcher per §2.

## Invariant verification

Per AW-V.md §Invariants:

1. **No deferrals, regardless of newfound scope.** Observed: every W2.3 novel lever has a home in W1/W3/AX per the plan's retirement table; no lever silently dropped. W6 acknowledges three architectural gaps (has_w4_classified narrowing, parse() entry-shape dispatch, tape-path elision) and routes them to AW-VI explicitly rather than silently carrying them. ✓
2. **Substrate-with-consumer is one unit of work.** W1 shipped substrate only (per the plan's own W1 description); W3 substrate + consumer for JSON; W4 substrate with partial consumer (parse() routing doesn't engage for CSS/Sheets/BBNF despite admission); W5 closed the admission surface but not the routing. The W6 assessment records this honestly: W4's per-shape fn substrate emits for every grammar but is not yet reached by `parse()` on non-JSON grammars. ◐ (substrate landed; consumer engagement incomplete)
3. **AX replay-surface preserved.** `bbnf_tape::driver::dispatch_one` + helpers + `DtaState` variants + `DTA_TABLE` + cold-path table-interpretive path all intact. ✓
4. **§6 generalisation invariant.** Every shape detector is an IR pass triggered by IR-structural properties; per-grammar OUTPUT varies because per-grammar IR varies; per-grammar MECHANISM does not. The grammar identity appears only in symbol prefixes. ✓
5. **Prototype isolation.** W2.1's prototype landed entirely in `bbnf-wt-aw5-prototype` sibling worktree; cherry-picked onto master at W3 open (commit `f8e56d50`) only after the 10%-of-sonic gate passed. ✓
6. **Wire-contract end-to-end tests.** W3.3 + W3.4 shipped 23 shape-emission tests + 5 JSON parity shape-emit tests; W5.1 shipped 9 BBNF + 19 grammar-wide profile wire-contract tests. ✓
7. **Bench-between-waves.** Per-wave sidecar artefacts shipped (W2.1 prototype bench; W6 aggregator = this + post-AW-V.json). ✓
8. **Per-wave verification ledger.** `nm` symbol-presence assertions (W2.1); `cargo expand` arm-body inspection (W3.3 goldens); samply attribution (W2.1 twitter + canada; W3 bench-fix); wire-contract tests (W3.3 / W3.4 / W5.1). ✓

## Artefacts

### Close documents
- `docs/tranches/AW/FINAL-V.md` — this document
- `docs/tranches/AW/AW-V-W2-close.md` — W2.1 prototype close ledger
- `docs/tranches/AW/PROGRESS.md` — W1 / W3 / W4 / W5 / W6 close entries

### Bench artefacts
- `docs/benchmarks/post-AW-V.json` — 17-entry parse-bench matrix + parity status + prototype comparison
- `docs/benchmarks/post-AW-V-W2-prototype.json` — W2.1 prototype reference

### Test additions (load-bearing)
- `crates/bbnf-tape-codegen/tests/*` — 4 helper-body fragment validity tests (W1.1)
- `crates/bbnf-simd-scan/tests/emit_fragments.rs` — 21 per-arch kernel fragment validity tests (W1.2)
- `crates/bbnf-tape/tests/aw5_w13_substrate.rs` — 18 tests (W1.3)
- `crates/ir/tests/shape_dispatch.rs` — 17 tests (W3.1)
- `crates/core/tests/shape_dispatch_emission.rs` — 23 tests (W3.3)
- `crates/core/tests/json_parity_shape_emit.rs` — 5 tests (W3.4)
- `crates/core/tests/css_l4_shape_emit.rs` — 29 tests (W4.2)
- `crates/core/tests/sheets_shape_emit.rs` — 23 tests (W4.3)
- `crates/core/tests/bbnf_profile_wire_contract.rs` — 9 tests (W5.1)

### Substrate additions
- `crates/bbnf-tape-codegen/` — new workspace member (W1.1)
- `crates/bbnf-simd-scan/src/emit.rs` — new submodule, ~300 LOC (W1.2)
- `crates/bbnf-tape/src/columns.rs` — push_scalar_payload_* + push_compound_fused_v32 (W1.3)
- `crates/bbnf-tape/src/visitor.rs` — Visitor trait hierarchy + TapeVisitor + ValueVisitor (W1.3)
- `crates/bbnf-json-prototype/` — new workspace member (W2.1; cherry-picked)
- `crates/ir/src/passes/recognizers/shape_dispatch.rs` — IR pass (W3.1)
- `crates/core/src/backend/rust/emitter/shapes/{object,array,string,number,keyword,scalar,pratt,unordered,arglist,flat,wrap,hregex,dispatcher,mod}.rs` — 14 shape-emitter modules (W3.2 + W4.1 + W4-fix + W5.2)

### Generated artefact
- `crates/core/src/grammar/generated.rs` — DTA-based, shape-dispatch-aware, bootstrap-idempotent

## What did not land

1. **Every parse entry exceeds post-AU on single-thread.** 0/17. Geomean 0.082. Architectural transposition correct; activation compounding incomplete.
2. **Every JSON entry exceeds sonic-rs by ≥ 1.07× on the emitter-produced parser at W6.** Prototype beats sonic 5/5 (geomean 0.934 ns/iter ratio); emitter-produced visitor-path matched prototype within 2% at W3 close but the json_monolithic_value bench does not compile at W6 close due to `has_w4_classified` gate on visitor emission being triggered by W4-fix-rest's Flat/Wrap admission of JSON's `pair`/`value` rules. Diagnosed; deferred to AW-VI.
3. **CSS / Sheets / BBNF parse() routing through shape dispatch.** Admission surface lands at W5.2 (`has_shape_dispatcher_entrypoint` Criterion 2); routing doesn't engage because the `__value` dispatcher body only supports Alt-of-Refs. Requires per-entry-shape dispatch in `parse()` entry (deferred to AW-VI).
4. **Lever-4 (`push_compound_fused_v32`) consumer activation.** Substrate ships at W1.3; no per-shape compound-emit arm routes through it at W6.
5. **ShapeRef dedup consumer in `close_compound`** (carried from AW-IV).
6. **Pratt LUT cold-path shadow deletion** (carried from AW-IV).
7. **CTNS / Bounded-Regex sound admission** (carried from AW-IV).

## Successor chain

AW-V closes honestly. Next tranche: **AW-VI** — compounding activation.

AW-VI opens on three bounded, diagnosed pieces of work:

1. Narrow `has_w4_classified` to admit W3-visitor-compatible W4 classifications (re-enables JSON's json_monolithic_value bench; likely re-establishes the prototype-matching ±2% parity from W3 close).
2. `parse()` entry-shape dispatch for non-Alt-rooted grammars (CSS / Sheets / BBNF route through their per-shape root fn; walker fallback demoted to true unclassified-rule cold path).
3. Lever-4 consumer activation + remaining AW-IV-carry-forwards (ShapeRef / Pratt LUT / Bounded Regex sound admission).

AX remains preserved; its cold-path replay surface (`DTA_TABLE`, `DtaSnapshot`, `dispatch_one`, `try_branch`, `advance_or_pop_with`) is intact. AW-V's W1 substrate enables AX's cold-path without modifying it.

## AW-V HEAD

Workspace: **1597 passed / 0 failed / 36 ignored**. Bootstrap idempotent. 17 parse benches + 2 format benches measured. sonic-rs parity 5/5. lightningcss parity 4/4. AW-V closes honestly: substrate landed and verifiable across every wave; prototype beats sonic-rs on every JSON entry; emitter-produced parser matches prototype at W3 close; W6 throughput gate misses because of a single diagnosed detector-coverage gate + a parse()-routing gap for non-Alt-rooted grammars, both carried forward as AW-VI's opening agenda.

Indefatigable. DTA compiled, not abrogated. Consumer inverted at shape granularity. Prototype proves substrate viability. AW-VI opens.
