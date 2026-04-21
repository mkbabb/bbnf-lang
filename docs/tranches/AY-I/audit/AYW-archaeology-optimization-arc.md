# AYW — Archaeology of the Optimisation Arc (AK → AY)

Read-only archaeology of eighteen tranches spanning AK (2026-04-11
flat-tape substrate) through in-flight AY (2026-04-20, W1 close). The
arc inverts at AU: the AK→AU ascent traded architectural cleanup for
substrate simplification; the AV→AW-V descent landed load-bearing
substrate without consumer activation and lost every bytes/cyc gain
that AK-AU had banked. AX was substrate-and-API closure; AY is
restoration-in-progress.

This document covers seven sections: per-tranche lever inventory (§1),
recurring anti-patterns (§2), approach categorisation (§3), AU
root-cause (§4), scope-reveal pattern map (§5), successor-tranche move
candidates
(§6), dev-expedite seeds (§7).

All numbers cite either `docs/benchmarks/post-{LETTER}.json` or named
samply self-times. Every gate status citation routes to the tranche's
FINAL.md or retrospective. No speculation without citation.

---

## 1. Per-tranche optimisation lever inventory

### AK — Flat Vec tape + per-branch variant discriminator

- **Lever**: Collapsed `ChunkedArena<TapeRec>` (`Vec<Vec<TapeRec>>`)
  to flat `Vec<TapeRec>`; added per-Alt `__branch_idx: u8` threaded
  through the epilogue as the tape's variant discriminator.
- **BEAT-predecessor gate**: **PASSED**. +10-14% across every JSON
  entry (citm 1,880 → 2,008; canada 1,332 → 1,467; twitter
  1,543 → 1,661 MB/s).
- **Substrate without consumer**: EmissionTier infrastructure left
  standing for later activation — AM.1 demolished it (-2,306 LOC).
- **Deferred silently**: clean bootstrap regen (still hand-patched,
  chronic since AC).
- **Gate-off commits**: none.
- **Orchestration**: plan (`AK.md`) authored AFTER implementation
  commits landed — post-hoc close-out as plan (ghost tranche; retro
  notes plan `c62ad389` lands 17:15 after AK.0 at 05:04 and AK.1 at
  05:12).

### AL — Research only (no execution)

- **Lever**: Four competing research prototypes (direct-projection
  unified ABI, e-graph→CSP feedback bridge, TaggedUnion boxing,
  profile-guided cost calibration) explored but not executed.
- **BEAT-predecessor gate**: N/A — no execution tranche. AL subsumed
  into AM/AN/AO/AP as input rather than shipping its own substrate.
- **Substrate without consumer**: all four prototypes. Global CSP
  solve, cost-model grid sweep, unified direct-projection ABI, and
  e-graph→CSP bridge all entered chronic-deferral status.
- **Gate-off commits**: none (no commits).

### AM — EmissionTier deletion + payload buffer + per-branch surgery

- **Lever**: Deleted the EmissionTier axis (-2,306 LOC), introduced
  `TapeRec::payload_idx: u16` + `Vec<u8>` payloads for `f64` (direct
  projection via side-channel), per-branch `push_leaf` vs
  `push_compound` dispatch on Alt-of-leaves.
- **BEAT-predecessor gate**: **PASSED**. Canada 1,453 → 1,689 MB/s
  (+16%), citm +7%, twitter +1%; canada BEAT sonic-rs by +12%.
- **Substrate without consumer**: AM.4 SIMD escape-parity string
  scanner shipped in parse-that with no codegen consumers; AM.5
  structural-bitmap miner + kernel shipped with no routing.
- **Deferred silently**: AM.5.3 structural-bitmap-in-codegen, AM.6
  cost-calibration sweep.
- **Gate-off commits**: none.
- **Orchestration**: no PROGRESS / FINAL / research; plan edited
  retroactively; AM.0+ CSP soft-index (269× compile speedup) was
  undeclared scope folded post-hoc.

### AN — Correctness + @ws SIMD + f64 payload activation

- **Lever**: Fixed the `__has_children` / `__branch_idx` correctness
  bugs from AM.3; routed CSS `@ws` through the SIMD
  `scan_ws_block_comments` kernel (replaced 15 × 147-line inline HIR
  with kernel calls, −2,190 LOC expanded); activated `F64`
  PayloadKind end-to-end; 3-tier whitespace bitmap caching
  (`ws_bitmap` on ParserState).
- **BEAT-predecessor gate**: **PARTIAL**. Canada 1,689 → 1,768
  (+5%), data 1,613 → 1,718 (+7%); citm 2,138 → 2,052 (−4%),
  twitter 1,671 → 1,650 (−1%). CSS normalize +230%, bootstrap +234%,
  tailwind +215% vs cssparser.
- **Substrate without consumer**: single-pass string scanning
  (AN.3), 32-byte AVX2 SIMD (AN.5), release-build instrumentation
  (AN.6) all declared; none landed.
- **Deferred silently**: AN.2 scanner-generalisation cluster (the
  eight AR.6/AS.5 items' origin), AN.3, AN.5, AN.6.
- **Gate-off commits**: none.

### AO — Structural dispatch (code-complete, NEVER ACTIVATED)

- **Lever**: `compute_structural_bytes` IR pass + parse-that cursor
  + `filter_quote_parity` + pre-scan codegen. Designed as the +50%
  lever on JSON citm (WS elision).
- **BEAT-predecessor gate**: **FAILED**. The `STATUS: OPEN (code
  complete for Phase 0, never exercised end-to-end)` header on AO.md
  is AP-era post-hoc retconning. Zero `scan_structural` calls in
  cargo-expand of JSON or CSS L4. Bench impact: **0%**.
- **Substrate without consumer**: the flagship feature of the
  tranche. All six sub-phases (IR pass, cursor, quote-parity,
  pre-scan, structural-aware dispatch, WS elision) landed as source;
  zero runtime activation.
- **Gate-off commits**: **YES** — emitter's `structural_mode` flag
  hardcoded false in `crates/core/src/generate/mod.rs:61`; commit
  history shows the gate introduced in-tranche with no same-tranche
  flip.
- **Orchestration**: no PROGRESS / FINAL / bench. Four of six phases
  (SIMD widening, cost calibration, self-hosting, global CSP) also
  silently dropped.

### AP — Activation + enrichment + WS bitmap

- **Lever**: Ground-truth measurement (12-agent audit); AP.0.2 `@ws`
  kernel universality fix (unblocked CSS L4 tailwind parse); AP.3.1
  SIMD WS bitmap (replaced scalar 8-byte loop with `u8x16::simd_eq`);
  AP.3.3 wired `scan_quoted_string_simd` into JSON production path;
  AP.4.1 CSS L4 `__declaration` key-dispatch (restructured 28-branch
  sequential backtrack).
- **BEAT-predecessor gate**: **PASSED**. JSON citm 1,874 → 2,712
  (+45%), twitter 1,540 → 2,173 (+41%). CSS bootstrap 337 → 505
  (+50%), normalize 623 → 978 (+57%). WS self-time on citm dropped
  from 50.4% to 11.9% per samply.
- **Substrate without consumer**: AP.1b peek-only redesign of
  structural dispatch (shipped `structural_mode = false`; see AQ.5
  deletion). AP.5.1 NibbleLut DFA codegen, AP.5.4 UTF-8 deferral,
  AP.5.5 TapeBuilder default pre-alloc — all declared, all deferred.
- **Gate-off commits**: **YES** — structural dispatch gated off a
  second time.
- **Deferred silently**: AP.3.2, AP.3.4, AP.4.2, AP.4.3, AP.4.4,
  AP.5.4, AP.5.5, AP.6.4, AP.6.5 — ten sub-phases silently dropped
  despite plan's self-promise "No deferrals."

### AQ — Generalisation + TypeDesc + Structural delete

- **Lever**: Deleted `PayloadKind` enum (TypeDesc consumed directly);
  deoverfit `RegexClass` (5 language-named variants → structural with
  parameters); deleted structural dispatch infrastructure entirely
  (~400 LOC). Aggregate payload layout planner + Alt-typed-enum view
  codegen shipped.
- **BEAT-predecessor gate**: **FLAT (within noise)**. Citm 2,712 →
  2,700 (−0.5%), twitter 2,173 → 2,086 (−4%). The Phase 7 CSS
  `__compoundSelector` routing + length-bucketed PHF was reverted
  (`64a2cf9` reverts `04bd0421`) on `var(--x)` regression.
- **Substrate without consumer**: Phase 6 typed-payload path
  end-to-end as source; **zero rules in any production grammar
  receive an aggregate layout**; scalar path on JSON `value` falls
  silent post-`fuse_single_use`. Code-completeness masqueraded as
  activation.
- **Gate-off commits**: none.
- **Deferred silently**: AQ.1 ostensibly "HIGHEST PRIORITY end the
  deferral chain" (bootstrap regen) shipped as `5b06096` "restore
  structural attribute + span-text fallback" — the opposite of the
  plan's hard gate. `grammar_roundtrip` remained `#[ignore]`-gated.

### AR — Discriminator split + payload activation + self-hosting close

- **Lever**: `meta: Vec<u8>` side-channel (AR.1.1) separating Alt
  branch index from rule ID; fixed `lower_map_arrow` type-suffix
  detection (AR.2.1); added `allows_escapes: bool` to `Identifier`
  RegexClass variant; `input.len() / 2 + 2` tape capacity heuristic;
  NEON + SSE4.2 fractional SIMD scan; `TapeKind::KvPair`.
- **BEAT-predecessor gate**: **FAILED on JSON**. Canada 1,797 →
  1,089 (−39%), data_xl 1,348 → 1,046 (−22%). But: genuine
  closure of the ten-tranche bootstrap deferral (`0c6e011` "recover
  modifiers from Repeat(vi=0)").
- **Substrate without consumer**: AR.6.1/6.2/6.4/6.5/6.6 scanner
  generalisation rolled to AS.5 where they became "Not applicable"
  / "Not implemented" (defer-then-retire).
- **Gate-off commits**: none.
- **Orchestration**: duplicate commits `b0e4534`/`6c889d5` + `677a801`
  /`6074a4b` from worktree racing (commit-before-parallelise
  edict violation).

### AS — CSS L4 activation + Span admission + scanner truth

- **Lever**: CSS L4 tailwind parse unblocked (scan_ident sub-flag
  dispatch); Span admitted as scalar payload (AS.2.1); six
  hand-rolled regex parsers in codegen replaced by cached
  `RegexInfo` (−263 LOC).
- **BEAT-predecessor gate**: **PARTIAL**. Twitter 2,069 → 2,003
  (−3%); CSS bootstrap reactivated from BROKEN to 525 MB/s; canada
  1,097 → 1,089 still deep in the AR regression.
- **Substrate without consumer**: AS.2.3 `StructRegistry` field
  added, never populated (AT marks as dead code, AU.4.2 deletes).
- **Gate-off commits**: none.
- **Deferred silently**: Phase 4 (fresh samply + post-AS.json +
  hot-path optimisation) silently dropped; gate #7 (twitter ≥ 2,000
  MB/s) dropped without plan rationale.
- **Orchestration**: 5-phase × 9-gate plan executed solo in ~1.5h;
  no `FINAL.md`, no worktrees, no research.

### AT — Projection truth + regression redress + bench parity

- **Lever**: General `resolve_branch_type` for inlined nodes
  (AT.1.1); multi-type `__payload_tag`; SIMD NEON scalar guard (AT
  audit surfaced: SIMD 16-byte load for 2-digit runs cost more than
  scalar SWAR).
- **BEAT-predecessor gate**: **PARTIAL**. Canada 1,089 → 1,464
  MB/s (Phase 2 redress gate met); JSON throughput still below AQ.
- **Substrate without consumer**: AT.1 Phase-1 passed its
  `push_leaf_with_f64` grep gate but `branch_pushes_children` in
  `driver/alt.rs` (file ABSENT from AT.md's critical-files table)
  mis-classified every `value` branch as compound; **every typed
  payload capture was a dead store until AU.1.1 (`83357e4`)**.
- **Gate-off commits**: none.
- **Silent deferrals**: string-decode codegen (3.2-3.5),
  `post-AT.json`, semantic parity audit, CSS/Sheets/BBNF deep tests,
  gate-5 "resolver handles Constant" (only Map/FnDescriptor
  exercised).

### AU — Projection activation + unified arena (historical peak)

- **Lever**: `branch_pushes_children` fix (AU.1.1 `83357e4`) —
  the real activation patch that AT-Phase-1's grep gate missed; CSS
  `-> f64`/`->` hex dispatch (AU.2.3/2.4/2.5); structural bitmap v2
  (AU.2.7, subsumed memchr1/2/3 + nibble_lut); `ParsedGrammar`
  deletion (AU.4.1, 11-tranche deferral resolved); `.map(|_| ())`
  elimination at codegen (309 sites → 0); **unified arena on AoS
  (AU.6.7)** — one `push_leaf_with(kind, PayloadData)` method
  replacing ten `push_leaf_with_*`; per-grammar push_fingerprint
  (AU.6.2, canada +18%).
- **BEAT-predecessor gate**: **PARTIAL (with documented misses)**.
  10 MET, 2 MET\* (qualified), 5 PARTIAL, 5 MISSED. Twitter 1,967
  MB/s = **0.615 bytes/cyc = 76% of sonic-rs**; citm 2,438 MB/s =
  **0.762 bytes/cyc = 94% of sonic-rs** — highest ever.
- **Substrate without consumer**: Bug 1 (Alt-payload first-branch
  loss) + Bug 2 (`-> Span` → push_compound) + Bug 2b
  (scanner-to-payload threading) documented in
  `typed-parity-audit.md` and routed to AV. AU.6.9 BBNF comment
  fast path blocked by Bug 2.
- **Gate-off commits**: none.
- **Orchestration**: archetypal tranche. Seven-wave schedule
  dispatched; samply trio (`prepare-profile-wave.sh` +
  `CARGO_TARGET_DIR` + `wave.tsv`) enabled 27-entry profiling
  fan-out cited by every downstream tranche. Mid-tranche Phase 2
  re-plan (CSS scanner activation → CSS typed-AST parity) without
  breaking wave cadence.

### AV — DTA substrate + SoA columnar + ShapeRef (substrate-only)

- **Lever**: DTA (Deterministic Tape Automaton) synthesis; SoA
  columnar substrate (`Columns { kinds, flags, extra, span_lo,
  span_hi, sib_skip, child_off: Vec<_>}` — 7 Vec pushes per record);
  `GrammarProfile` codegen channel per-grammar `pub const`;
  PayloadStream (PSI); ShapeDictionary (`TapeKind::ShapeRef`);
  parse-that Eisel-Lemire + simdjson SIMD decode.
- **BEAT-predecessor gate**: **CATASTROPHIC REGRESSION**. Canada
  1,231 → 455 (−63%); twitter 1,967 → 481 (−76%); bootstrap
  454 → 182 (−60%). 2.5–4.5× across every entry.
- **Substrate without consumer**: V0–V5 landed the substrate; V6–V9
  (parallel parse, SIMD keyword, bloom dedup, walker migration)
  cut at V5 boundary "per user direction" — entire 83-commit
  tranche substrate-only. AV.3.6 fn-per-rule deletion never fired;
  `Tape::reduce_column` API never written.
- **Gate-off commits**: none by flag, but DTA runtime driver was
  the consumer and it was routed wholesale to AW.
- **Orchestration**: first bench at V10 — 2.5-4.5× regression
  invisible until tranche close. `research/` held six "April 2026"
  deliverables with headers identifying them as AU-era (did not
  re-fan-out per RESEARCH.md six-agent edict).

### AW-I — DTA activation + fn-per-rule deletion

- **Lever**: Walker completion (AltLinear / Repeat / ShuntingYard
  arms); `parse()` swap to direct `dta_run_into` dispatch;
  `parse_dta()` retired; W2.5 snapshot migration absorbed from W4.5
  under scope-reveal.
- **BEAT-predecessor gate**: **FAILED on throughput**. Twitter 481
  → 123 MB/s (−74%) — universal −91% regression. Bootstrap 182 →
  1,436 MB/s called out as "correctness regression disguised as a
  throughput win — parse-failure artefact (9 records vs 92,228
  golden)".
- **Substrate without consumer**: none at close; every architectural
  precept activated. Twelve substrate extensions the plan did not
  enumerate landed.
- **Gate-off commits**: `has_inline_frame_depth: bool = false`
  flipped in-tranche; not a gate-off violation.
- **Scope-reveal**: W4ζ lowering-pipeline migration too broad —
  cleanly opened AW-II.

### AW-II — Lowering migration + consumer absorption

- **Lever**: `find_child_by_kind` → `find_descendant_by_kind`
  migration in `lower/**`, `graph/**`, `types.rs` (W1–W4);
  `DtaState::Minus` producer-side fix (W5b); universal named-type
  projection (W5c).
- **BEAT-predecessor gate**: **FAILED**. 0/17 parse entries exceed
  post-AU. DTA viability question raised at W5.7 (5-40× regression
  measured).
- **Substrate without consumer**: 36 Cluster-C payload activation
  residuals; DTA lifter strips `IrNode::Map { inner, .. }`
  wholesale (`crates/ir/src/passes/recognizers/dta.rs:525`); walker
  hardcodes `PayloadKind::F64` (`crates/bbnf-tape/src/driver.rs:912`).
- **Scope-reveal**: tripartite residual (50 correctness failures +
  67 unaudited ignores + 5–40× bench regression) — new letter
  (AW-III) inserted to audit DTA viability before layering
  optimisation.

### AW-III — DTA correctness closure + architectural transposition

- **Lever**: `dispatch_one` at **0% self-time** (transposition
  complete); stage-1 SIMD active at 12.13%; push_compound_fused
  replaces reserve_compound; bbnf-simd-scan NEON + AVX2 + AVX-512 +
  WASM + scalar kernels.
- **BEAT-predecessor gate**: **FAILED on throughput (gate 12)**.
  **0 of 17 parse entries strictly better than post-AU**; geomean
  0.08× of baseline. 11 of 12 structural gates MET.
- **Substrate without consumer**: SHAPE_DICT empty across grammars;
  0 PHF tables; 0 ClassifyByte tables; CTNS gated off; bounded-Regex
  defeated by dense alphabets; Pratt LUT emitted but walker's
  ShuntingYard arm still uses linear scan.
- **Gate-off commits**: none.

### AW-IV — Interpreter abrogation core

- **Lever**: W1 hoisted every `match table.states[N]` indirection
  into literal `let` bindings; W2 inlined helper bodies + fat LTO;
  W3 five emitter-mined consumer activations (ShapeRef, PHF,
  ClassifyByte, Pratt LUT, direct-to-struct); W4.4 document-parallel
  fork on tailwind (+131% at 4 threads); W5.1 `reduce_column` API
  (canada f64 6.57× microbench).
- **BEAT-predecessor gate**: **FAILED on throughput (W6)**. **0/17
  parse entries exceed post-AU**; geomean 0.071 (~7%). Geomean vs
  AW-III: 1.83× (+83% recovery but still an order of magnitude
  below RD baseline).
- **Substrate without consumer**: `push_compound_fused_v32` 32-byte
  store ships with no consumer; ShapeRef dedup in `close_compound`
  deferred; Pratt LUT cold-path shadow in `advance_or_pop_with`'s
  SY arm; CTNS / bounded-Regex sound admission. Cost-grid sweep
  (AM.6 chronic) closed via null-result escape clause —
  `CALIBRATED_WEIGHTS == CostWeights::default()`.
- **Gate-off commits**: none.
- **W4.4 breakthrough**: tailwind 16 → 37 MB/s at 4 threads (2.24×).
  The only throughput-moving lever across the AW-I..V arc.

### AW-V — Compile DTA/PSI into hot-path + novel-exceed

- **Lever**: `bbnf-tape-codegen` subcrate (helper body splice
  fragments); `bbnf-simd-scan::emit` 21 per-arch body exporters;
  11-shape classifier (6 W3-active + 6 W4-stub); `parse_with_visitor`
  API; W2.1 hand-prototype in `bbnf-json-prototype/` crate.
- **BEAT-predecessor gate**: **W2.1 BEATS sonic-rs** (0.89-0.94×
  ns/iter across 5 fixtures — the substrate-viability proof). **W3
  emitter-produced visitor-path matched prototype at W3 close**
  (±2%). **W6 throughput gate MISSES** — 0/17 parse entries exceed
  post-AU; visitor-path bench no longer compiles due to W4
  detector-widening reclassifying JSON's `pair`/`value` rules,
  tripping `has_w4_classified` gate.
- **Substrate without consumer**: W5.2 per-Ref dispatcher admission
  lands but parse() doesn't route; BBNF GRAMMAR_PROFILE populated
  (28 bytes structural_alphabet, 17 digraphs, 13 keyword tables, 10
  shape_dict) — wire-contract tests pass but consumer paths don't
  fire.
- **Gate-off commits**: `has_w4_classified` gate is gate-off by
  calibration, not by flag — the gate admits JSON then blocks
  emission.

### AX — Substrate-and-API closure (interpreter deletion + view surface)

- **Lever**: W0a narrowed `has_w4_classified` to `Pratt | Unordered`;
  W0a.2 cascade closed 5 emitter defects + bootstrap idempotency;
  W0b deleted the interpreter (~85K LOC); W1r.1-W1r.7 delivered
  grammar-derived NodeView + typed accessors + canonical-serialise
  byte-parity against sonic-rs and lightningcss.
- **BEAT-predecessor gate**: **CSS/Sheets/BBNF recovered 2.7-8.5×
  at W0a.close**; JSON held flat at AW-V level (twitter 486 → 448
  MB/s). W1r delta vs W0a-close is slightly negative on JSON
  (4-13%) due to NodeView + canonical-serialisation harnesses
  adding work without compensating optimisation.
- **Substrate without consumer**: 5 stale wire-contract test files
  (predicates retired W0a.2.j; fields carved W0b.A); `TypeDesc::Named`
  collapse before Rust emit (diag routed to AY.W2). Block B
  (W2-W14 optimisation arc) did not execute — honest scope-reveal
  absorption into AY (see §5).
- **Gate-off commits**: none.
- **Scope-reveal (W1 absorb)**: hand-coded `bbnf::json::Value` /
  `bbnf::css::StyleSheet` duplicates (W1.A/W1.B) reverted as
  invariant-4/11/18 violations; W1 re-scoped in place without
  letter pivot.

### AY — AU restoration + BEAT-sonic (in flight)

- **Lever (W0)**: ~2,300 LOC legacy prune (dta.rs 550 → 80,
  shape_dict.rs delete, classify_byte.rs delete, 5 stale
  wire-contract tests retired); ebnf_prettify deferred to AY.W2.
- **Lever (W1)**: **AU AoS substrate revert** — `Columns` 7
  structural Vec columns → 1 `Vec<TapeRec>` + parallel `sib_skip`
  column (columns.rs 1,618 → 1,119 LOC, −31%); finaliser
  stack-buffer scratch; `#[inline(always)]` cross-crate hot helpers;
  Pratt Option C inline + `[LocalOpEntry; 16]` stack hoist.
- **BEAT-predecessor gate (W1)**: **PARTIAL**. Twitter 437 → 688
  MB/s Phase 1 sanity (+57%); **SOFT-MISS on gate 1 (≥ 0.45 bytes/
  cyc; measured 0.215)**. CSS L4 tailwind +35.4% (PASS). W1-fix
  retired eager `scan_structural` (substrate-with-consumer cycle
  re-opened: consumer lands in W4).
- **Substrate without consumer (current)**: `scan_structural` +
  `StructuralIndex` substrate retained for AY.W4 CTNS-style
  consumers; absorbed under SPEC §Transitional fallback.
- **Gate-off commits**: none.
- **Scope-reveal (W1-fix)**: samply-driven regression re-plan —
  eager `scan_structural` retired same-wave via Absorb.

---

## 2. Recurring anti-patterns (the optimisation anti-corpus)

Count + most-egregious instance per pattern. All citations from
`docs/tranches/AW/audit/SYNTHESIS.md` + per-tranche retros + AX
audit.

### 2.1 Substrate-without-consumer — 14 instances across 18 tranches

The #1 chronic pattern. Emission lands; consumer doesn't; hard gate
closes on "code exists"; runtime never fires.

- **AK** EmissionTier scaffolding left after `__branch_idx` obsoleted it.
- **AM.2/AM.5** payload buffer + structural bitmap in parse-that; no
  codegen consumer.
- **AO (Phase 0 full chain)** structural IR pass + cursor + quote-
  parity + pre-scan + WS elision — NEVER ACTIVATED; AP recap:
  parse impact **0%**.
- **AP.1/AP.1b** `structural_mode = false` gate-off (see §2.2).
- **AQ Phase 6** PayloadKind deletion + TypeDesc expansion + layout
  planner + Alt typed enum view — **zero payload writes in six
  production grammars**.
- **AS.2.3** `StructRegistry` field populated never; deleted at
  AU.4.2.
- **AT.1** `resolve_branch_type` emits `push_leaf_with_{f64,bool,u8}`;
  `driver/alt.rs::branch_pushes_children` mis-classifies inlined-Ref
  leaf branches — **every typed capture a dead store until AU.1.1**.
- **AV.0.5** Color admission inert through V5 (layout pass never
  admits `TypeDesc::Named("Color")`).
- **AV.2.5** reordered-unrolling kernels landed; `Tape::reduce_column`
  API never written (landed AW-IV.W5.1 as 6.57× microbench-only).
- **AV V0–V5** entire DTA substrate: DTA_TABLE + GRAMMAR_PROFILE +
  SHAPE_DICT + PayloadStream + ShapeRef cursor — runtime driver
  deferred to AW.
- **AW-III.W6** SHAPE_DICT empty; 0 PHF tables; 0 ClassifyByte
  tables; CTNS gated off; Pratt LUT emitted but walker uses linear
  scan.
- **AW-IV.W3** 4 of 5 sub-waves: ShapeRef, Pratt LUT, CTNS, bounded
  Regex — substrate without consumer.
- **AW-V W4/W5** per-Ref dispatcher + per-shape emitters for CSS /
  Sheets / BBNF; substrate emits for every grammar; parse() still
  routes through walker.
- **AX.1r.1** static `BINDINGS` slice for named-type resolution —
  dead code on every grammar; refactor's value is code hygiene +
  readiness.

**Most egregious**: **AO structural dispatch**. The tranche's
headline lever, built across 6 sub-phases, landed with the
activation flag hardcoded false in the emitter and no same-tranche
flip. `AO.md` header was retconned post-hoc: `STATUS: OPEN (code
complete for Phase 0, never exercised end-to-end)`. Zero
`scan_structural` calls in cargo-expand of JSON or CSS L4 at close.

### 2.2 Gate-off commits — 3 instances

- **AP.1** `structural_mode = false` in `generate/mod.rs:61`; three
  specific bugs surfaced only in AQ's post-hoc audit.
- **AP.1b peek-only redesign** re-introduced the same gate.
- **AW-V.W6** `has_w4_classified` gate — not a Boolean flag but a
  calibrated detector that over-admits JSON and blocks visitor-path
  emission.

**Most egregious**: **AP.1**. Flagship structural-dispatch
committed with `false` default in a tranche that audit AQ then
deleted outright.

### 2.3 Hard-gate-via-grep — 2 instances

- **AT.1 Phase-1** passed grep gate `push_leaf_with_f64` appears;
  `branch_pushes_children` in `driver/alt.rs` (file ABSENT from
  AT.md's critical-files table) still mis-classified. Every typed
  payload capture a dead store until AU.1.1.
- **AQ.1** "end the deferral chain" shipped as
  `5b06096 restore structural attribute + span-text fallback` —
  passed source-level gate, did the opposite of the declared hard
  gate (`grammar_roundtrip` un-ignored).

**Most egregious**: **AT.1**. Plan's critical-files table didn't
include `driver/alt.rs`; agent never opened it; bug lived in the
unlisted file.

### 2.4 Ghost tranches (commits before plan) — 2 instances

- **AK** `AK.md` committed at `c62ad389` AFTER AK.0 (05:04), AK.1
  (05:12) — plan authored 17:15 as post-hoc close-out.
- **AL/AN** label collision — AN plan (`acaa1898`) + three AN.0 fix
  commits landed BEFORE AL.1 committed; AN Phase-0 doc-fold
  (`17728fd7`) retroactively absorbs AL.1 as "AN Phase 1.1".

### 2.5 Label collisions (mid-tranche pivot under same letter) — 3 instances

- **AR** audit-driven replan kept under AR/ instead of promoted to
  a new letter per SPEC `new-tranche-new-doc`.
- **AS** mid-stream re-plan of AR-audit leftovers — no new letter.
- **AW** the arc-internal split (AW-I → AW-II inserting into the
  original AW schedule, then AW-III → AW-IV) is borderline —
  SYNTHESIS counts AW-I/AW-II split as correct (W4ζ lowering too
  broad → new letter).

**Most egregious**: **AR**. Audit phase expanded the 5-phase plan
to 9; AS later marked "AS Phase 2 done — bootstrap loop closed in
AR audit" — the genuine closure of the chronic self-hosting debt
landed under AR's banner in AR's post-audit cleanup window, scope
neither AR.md nor critique.md forecast.

### 2.6 Cost-model miscalibration — 2 instances

- **AU.7.1** SoA prototype at 1.94× vs gate ≥ 5× — gate miscalibrated
  against naive f64-sum read side; AoS+arena was not terminal, but
  naive SoA didn't pay. Later AW-IV.W5.1 closed the gate at 6.57×
  via 4-lane reordered unrolling (4 accumulators breaking strict-
  IEEE left-fold).
- **AW-IV.W5.3** cost-grid sweep 648 measurements over 54 configs;
  `CALIBRATED_WEIGHTS == CostWeights::default()` — null result,
  hard-gate closed via plan's escape clause.

**Most egregious**: **AU.7**. The SoA columnar pivot became the
load-bearing architecture of AV+AW-I on the strength of a
misspecified microbench.

### 2.7 Golden-drift — 3 instances

- **AW-II.W5.B** 10 tape_parity goldens regenerated (record-count
  ratios 1.04–2.50 — shape mismatches, not truncation); required
  deliberate regen.
- **AW-V W3.2** shape-emit goldens required `cargo expand` regen
  via `prettyplease::unparse`.
- **AY.W0-D** (`24d18f42`) `bbnf_json_prototype` → `json_prototype`
  for crate rename (Class B golden refresh); separate W0-D commit
  `a7aded47` hex_color round-trip tests rewrote to search-by-target
  across all KvPair records (golden-drift).

### 2.8 Flaky perf threshold tests — 1 instance

- **AY.W1** `tape::tests::packed_cache::packed_cache_read_beats_soa_materialise`
  asserts a 1.3× perf threshold near system noise floor;
  intermittent (passes ~3 of 5 runs). Pre-existing at AY open; not
  introduced by AY.

### 2.9 Worktree orphan accumulation — 1 major cleanup

- **AY.W0 close** — `git worktree` 50 → 13 (−37 orphans: 23
  manually-named bbnf-wt-\* + 17 .claude/worktrees/agent-\* + 2
  /private/tmp + 4 active-now W0 worktrees of which 3 already
  removed). 21 stale `.profiles/` files (>5d) purged.

### 2.10 Bench-omission + silent deferrals — 11 instances

Counting silent-deferral chains per retro:
- **AN** 5 items (AN.0.5, AN.2, AN.3, AN.5, AN.6).
- **AO** 4 phases silent-deferred (only re-surface in post-hoc
  STATUS: OPEN block).
- **AP** ~10 sub-phases.
- **AS** 4 sub-items under "PARTIAL" label.
- **AT** 5 gates silent-dropped (decode codegen, post-AT.json,
  semantic parity audit, CSS/Sheets/BBNF deep tests, gate-5
  resolver).
- **AV** V6-V9 (declared at plan time, cut at V5 close); AV.3.6
  fn-per-rule delete slipped silently within V3 → V4-close →
  never.

**Bench omission specifically**: AV V10 was the first bench;
2.5-4.5× regression invisible until tranche close. AN functional
gate passed while shipping −39% canada / −20% data_xl.

---

## 3. Optimisation approaches — what works vs what doesn't

Each tranche categorised by the dominant input signal that shaped
the plan:

| Tranche | Approach | Outcome |
|--------|----------|---------|
| AK | Spec-driven (flat-tape invariant) | BEAT (+10-14%) |
| AL | Research-only | N/A (no ship) |
| AM | Spec-driven (tier demolition + payload buffer) | BEAT (+16% canada) |
| AN | Reactive (AM bug triage) + grammar-driven (@ws kernel) | PARTIAL |
| AO | Spec-driven (structural-index model from sonic-rs) | FAIL (0% — never activated) |
| AP | Profile-driven (12-agent samply audit) | BEAT (+45% citm, +41% twitter) |
| AQ | Spec-driven (TypeDesc consolidation) | FLAT |
| AR | Reactive (post-AQ payload dormancy) + spec-driven (discriminator split) | FAIL on JSON (−39% canada) |
| AS | Reactive (AR CSS regression) | PARTIAL |
| AT | Reactive (AS regression + projection truth) | PARTIAL |
| AU | **Profile-driven (samply trio; 27-entry fan-out)** | **PARTIAL but historical peak** |
| AV | Spec-driven (SoA columnar + DTA synthesis) | CATASTROPHIC (−60-76%) |
| AW-I | Spec-driven (DTA activation) | FAIL (−74% twitter) |
| AW-II | Reactive (W4ζ lowering migration) | FAIL |
| AW-III | Spec-driven (architectural transposition, 0% dispatch_one) | FAIL on throughput; PASS on 11/12 structural gates |
| AW-IV | Spec-driven (interpreter abrogation) + bench-driven (W4.4 parallel fork) | FAIL on 0/17; BREAKTHROUGH on tailwind (+131%) |
| AW-V | Spec-driven (compile DTA to hot-path) + proof-driven (W2.1 prototype) | W2.1 BEAT sonic 0.89-0.94×; W6 0/17 |
| AX | Reactive (AW-V close-state) + grammar-driven (view surface) | CSS/Sheets/BBNF +2.7-8.5×; JSON flat |

### Approach ratios

**Profile-driven (2 tranches: AP, AU)** — 100% BEAT-predecessor or
historical-peak ratio. Zero substrate-without-consumer landings
(AP.1/AP.1b gated structural dispatch off precisely because profile
showed pre-scan cost > WS savings post-AP.3.1; AU routed every
missed gate to named destinations). Profile-driven is the only
approach that consistently moved bytes/cyc.

**Bench-driven (1 lever: AW-IV.W4.4)** — Single-lever breakthrough
(tailwind +131% at 4 threads). Only throughput-moving lever across
the entire AW-I..V arc.

**Spec-driven (10 tranches: AK, AM, AO, AQ, AU.7-rejected, AV, AW-I,
AW-III, AW-IV, AW-V)** — Mixed. AK/AM beat; AO/AQ/AV/AW-I..AW-V
accumulated the bulk of substrate-without-consumer debt (14 of 14
instances in §2.1). Spec-driven tranches are where architectural
vision outran profile attribution.

**Grammar-driven (3 tranches: AN, AP.0.2, AX.W1r)** — Localised
correctness wins; never the headline lever.

**Reactive (4 tranches: AR, AS, AT, AW-II)** — Correct response to
regressions; never the lever that produces net-positive delta.
Usually preserves forward motion.

### Verdict

- **Profile-driven** produced the highest BEAT-predecessor ratio
  (2/2 = 100%).
- **Spec-driven** accumulated the most substrate-without-consumer
  (12/14 instances).
- **Bench-driven** is the single-lever breakthrough pattern when
  profile-attributed microbench gates hold.

The AV → AW-V descent is a 5-tranche spec-driven cascade where each
tranche's substrate assumed the next would activate it. By AX close,
exactly one lever (AW-IV.W4.4 document-parallel) had produced
measured throughput movement; every other substrate was either
deleted (AX.W0b ~85K LOC interpreter) or inert.

---

## 4. AU-era optimisation — why did AU succeed?

AU is the historical peak: twitter **1,967 MB/s = 0.615 bytes/cyc =
76% of sonic-rs**; citm **2,438 MB/s = 0.762 bytes/cyc = 94% of
sonic-rs**. Post-AV/AW-\* master at twitter 437 MB/s = 0.137
bytes/cyc = 17% of sonic-rs — a 4.5× regression on the most-tested
fixture.

### Load-bearing AU primitives (per `docs/tranches/AU/FINAL.md`)

1. **Flat AoS `Vec<TapeRec>` write path**. One 16-byte store, one
   bounds check, one possible realloc per push. `Tape { records:
   Vec<TapeRec>, arena: Vec<u8> }`.
2. **Unified `push_leaf_with(kind, PayloadData)`** (AU.6.7 commits
   `3b75463` + `9a1186e` + `7fc0adf`). Ten `push_leaf_with_*`
   methods collapsed into one; `PayloadData` enum covers `None |
   InlineScalar(u32) | WideScalar(u64) | Aggregate(&[u8]) |
   Bytes(&[u8])`; `TapeRec::payload_idx` removed in favour of arena
   offset in `child_off` (u32 range).
3. **Per-grammar `push_fingerprint` divisor** (AU.6.2 `ff32c0b` +
   `c2664f3`). Compile-time fingerprint generates a per-grammar
   (numer, denom) divisor for `TapeBuilder::with_capacity`. JSON
   canada +18% (989 → 1,169); BBNF json.bbnf +49%.
4. **`.map(|_| ())` elimination at codegen**. 309 sites → 0
   (`4e4a75e`). No-value-discard invariant enforced uniformly.
5. **Classifier scope correct**. `RegexClass::Numeric {
   allow_leading_dot }` dispatch (`240535b`) selects
   `scan_number_f64` (generic) vs `scan_number_strict_f64` (JSON).
6. **Regression-to-spec discipline**. Every missed gate routed to
   AV with a surgical fix sketch (`typed-parity-audit.md`). 10
   MET, 2 MET\* (qualified), 5 PARTIAL, 5 MISSED, 1 DEFERRED.
   Every miss named. No silent deferrals.
7. **Samply trio** — `prepare-profile-wave.sh` + `CARGO_TARGET_DIR`
   + `wave.tsv` (27-entry fan-out). Three pre-wave friction fixes
   unlocked the profiling infrastructure every subsequent tranche
   cites.

### Top-3 AU-era invariants eroded between AU and current master

1. **Flat AoS `Vec<TapeRec>` write path → SoA 7-column pivot (AV
   AW-I)**. 7 Vec pushes per structural record vs 1; 7 heterogeneous
   type stores LLVM cannot fuse. Per the A12 audit (`docs/tranches/
   AX/audit/next-tranche/12-au-archaeology-beat-sonic.md` §5.1),
   this is the single largest regression driver — 2× recovery on
   twitter just from the W1 revert. **AY.W1 restored `Vec<TapeRec>`
   primary + optional `PackedRecord` sidecar as read-side cache**
   (landed 2026-04-20 at HEAD `fb34e008`).

2. **`.map(|_| ())` elimination invariant (AU.6.5) → Bug 1 per-branch
   payload-write loss (AV.0.1 partial fix) → compound-wrap emission
   on scalar-leaf rules (AW-V.W4 detector widening)**. AU fought the
   value-discard pattern at codegen; AW-V.W4 reintroduced the same
   class of dead work by wrapping every `value` rule in a compound
   record. Per AY.md §Architectural thesis: "for JSON 100K scalars
   → ~200K tape records (100K leaves + 100K wraps); sonic-rs has no
   wrap — `Value::Number(f)` IS the node." **AY.W2 G3 wrap-elision
   targets restoration** (not yet landed at audit time).

3. **Unified `push_leaf_with(kind, PayloadData)` (AU.6.7) →
   multi-column `push_structural` cross-crate call (AV/AW-III
   `Columns::push_structural` inlined unreliably despite `#[inline]`
   hint)**. Per A1-A8 fresh profile (`AX/audit/next-tranche/A1`,
   `A8`): `Columns::push_structural` 23-43% of every grammar's
   self-time; `tape::finaliser::finalise` 12-27%. These two symbols
   = 50-70% of hot-path. Both are post-AU artefacts. **AY.W1.5
   `#[inline(always)]` cross-crate hot helpers + `Tape::get` landed
   (commit `b6ff6fe0`/`b649d794`)** — NM gate met; samply shift
   from `tape` crate into per-rule `parse_*` functions validated.

**The AU orchestration discipline** — bench-between-every-wave
(absent from AV through V10), profile-driven plan authoring
(`profiling-{1,2}.md` co-located with plan), samply trio
infrastructure, re-plan-on-scope-reveal instead of defer — is what
separates AU from the AV–AW-V cascade. Restoring the substrate
without restoring the discipline is a necessary but not sufficient
condition.

---

## 5. Scope-reveal pattern map

Per SPEC §Scope-reveal protocol, each tranche's response mode to
revealed work:

| Tranche | Mode | Reveal | Cleanness |
|---------|------|--------|-----------|
| AK | n/a | no reveal documented | — |
| AL | n/a | no execution | — |
| AM | silent | AM.0+ CSP soft-index folded post-hoc; AM.5.3 dropped | messy |
| AN | silent | 5 items dropped to AO without rationale | messy |
| AO | silent | 4 phases "STATUS: OPEN" retrocon | very messy |
| AP | silent | ~10 sub-phases dropped | messy |
| AQ | silent | AQ.1 shipped opposite of plan's gate; Phase 7 reverted | messy |
| AR | label collision | audit expanded 5 → 9 phases under AR/ | messy |
| AS | label collision | mid-stream replan of AR leftovers under AS | messy |
| AT | silent | 5 gates dropped including the critical file `driver/alt.rs` | very messy |
| AU | **Absorb (re-plan)** | Phase 2 rewritten Session 2 from CSS-scanner-activation to CSS-typed-AST-parity-with-lightningcss | **clean — exemplar** |
| AV | tranche-boundary cut | V6-V9 cut at V5 boundary "per user direction" | declared but late |
| AW-I | **New letter** | W4ζ lowering-pipeline migration too broad → opened AW-II | clean |
| AW-II | **New letter** | tripartite residual (50 failures + 67 ignores + 5-40× regression) → opened AW-III | clean |
| AW-III | declared scope | transposition complete; gate 12 missed; routed to AW-IV W1 | clean |
| AW-IV | declared scope | W1.4-aggressive binding-rule revision mid-execution; W2.3 rescope | clean |
| AW-V | declared scope | W2.3 retired (W2.1 met exceed-gate); W4 detector widening gated out visitor path | clean |
| AX | **Absorb (W1 in place)** | W1.A/W1.B hand-coded value duplicates reverted; W1r re-scoped without letter pivot | clean |
| AX | **New letter** | Block B (W2-W14) → AY | clean |
| AY | **Absorb (W1-fix)** | eager `scan_structural` retired same-wave via samply-driven regression | clean |

### Cleanest scope-reveal — exemplar

**AU Session 2 Phase-2 rewrite**. Three things made it clean:
1. The rewrite was driven by a profiling artefact (`profiling-1.md`)
   whose conclusions made the existing plan framing undeniable.
2. It happened without breaking wave cadence — Phase 2 gained
   AU.2.0 (grammar audit), AU.2.5 (typed dimensions), AU.2.6 (typed
   colours); no other wave slipped.
3. No letter pivot; the tranche closed under AU with the rewrite
   visible in-plan.

### Messiest scope-reveal — anti-exemplar

**AO structural dispatch**. The flagship lever of the tranche
landed with `structural_mode = false` and no same-tranche flip. The
`STATUS: OPEN (code complete for Phase 0, never exercised end-to-end)`
header is AP-era post-hoc retrocon; at tranche close there was
no FINAL.md, no PROGRESS.md, no bench, no explicit deferral. Four
of six phases silently dropped. AQ post-audit diagnosed 3 specific
bugs in the never-exercised code paths; AQ.5 deleted ~400 LOC.

**Cleanness-of-handling correlates with documentation discipline**.
AU/AW-I/AW-II/AX all produce FINAL.md + bench + routed-ledger at
close. AM/AO/AP/AR/AS/AT all lack at least one of those artefacts.

---

## 6. The optimisation trajectory — what should BA do?

Given 18 tranches of evidence, the single highest-ROI architectural
change BA should make:

### Candidate ranking

**#1 — Unified decision surface: fuse classifier + e-graph cost
model + shape dispatcher + regex DFA extraction into one cost-guided
extraction pass** (PRIMARY RECOMMENDATION).

Evidence:
- **Substrate-without-consumer count**: 14 of 14 instances have the
  same shape — an analysis pass emits structural data into IR
  sidecars that no consumer reads at runtime (§2.1). The AW-IV.W3
  five consumer activations (ShapeRef, PHF, ClassifyByte, Pratt
  LUT, direct-to-struct) each ran on a separately-mined sidecar
  with its own guardrail. Four of five didn't activate.
- **Detector LOC budget** (AX.W12 scope) — ~1,676 LOC of shape
  detectors to retire in favour of e-graph canonical forms.
- **Cost-model miscalibration count** (§2.6) — 2 of 2 were
  consequences of running an optimisation gate against a substrate
  the cost model didn't see: AU.7.1 SoA gate on naive f64-sum,
  AW-IV.W5.3 null-result sweep on an invariant-DTA-state config
  space.
- **The A7 audit verdict** (`docs/tranches/AX/audit/next-tranche/
  A7-ax-unfinished-absorption.md`) marks W10 (e-graph G1-G4 universal
  rewrites) + W11 (G5-G9 per-shape rewrites) as **POST-BA** "ledger-
  only until a consumer cites measurable samply shift."

The architectural transposition: replace the current pipeline's
eight separate analyses (recognizers, regex_info, structural_
alphabet, dispatch, fuse, inline, shape_dispatch, payload_layout)
with one cost-guided e-graph extraction. Each analysis becomes a
rewrite rule; costs arbitrate; extraction produces the IR the
shape emitter consumes. This retires the detector LOC budget AND
makes every future optimisation lever an e-graph rewrite rather
than a new orthogonal sidecar.

**Expected gain**: eliminates the substrate-without-consumer class
structurally (you cannot mine data an extraction pass doesn't
read). Cost: 2-3 tranches of substrate work; BA scope.

**Evidence it's the right call**:
- AV/AW-I/II/III/IV/V all landed substrate-without-consumer in the
  same shape. No amount of per-tranche discipline prevented this.
- AU.7.1 SoA gate on a misspecified microbench is the same failure
  pattern at cost-model layer — the cost model didn't know about
  write-side cost.
- AW-V.W2.1 prototype matched sonic-rs; emitter matched prototype
  at W3 close; W4 detector widening broke it. One unified cost
  surface (prototype's per-shape monomorphised fn) makes every
  later rewrite an e-graph rewrite-rule that gets costed against
  the prototype invariant.

**#2 — Payload-layout CSP rewrite: admit all Named types into 16B
hot-path**.

Evidence: §A6 `named-preservation-design.md` — `TypeDesc::Named`
collapses pre-emit on every Rust-target grammar; `prune_unreachable`
is the empirically-identified culprit. **AY.W2 scopes this but
hasn't landed at audit time**. 16B aggregate slot admits all
scalar-struct Named types; larger (33B colour-function) need
arena-backed widening AU.2.6 punted on. Post-BA if AY.W2 lands.

**#3 — Tape substrate rethink: columnar SIMD reducer wiring**.

Evidence: AU.7.1's 1.94× SoA prototype failed vs gate ≥ 5×;
AW-IV.W5.1 closed the gate at 6.57× via 4-lane reordered unrolling
(4 accumulators breaking strict-IEEE left-fold). **The emitter-
side reordering pattern is the missing lever** (AU.7's load-bearing
finding). AY.W1 restored flat AoS write path; AY.W8 adds
document-parallel. BA could restart the SoA pivot with an
emitter-side reducer codegen (`Tape::reduce_column<C, R>` actually
consumed per-grammar at emit time). Read-side cache only, write
side stays AoS.

### Candidates rejected

- **Parse-that / bbnf-regex unified DFA emit** — high-LOC, medium
  gain. Per A1/A2: JSON `__regex_scan_*` is ~0% of self-time; CSS
  regex_scan is 26% but CSS already BEATS lightningcss. Attacking
  CSS regex-scan attacks a metric bbnf already wins. Routed to
  AY.W4 (low priority).
- **Fused parse + value materialisation single-walk** — would
  subsume the view→value hop and potentially match sonic-rs's
  `Value::Number(f) IS the node` pattern. But AY.W3 already ships
  per-shape inline fn pattern via prototype; adding a second walker
  is orthogonal substrate. Post-BA.

**The recommended BA charter**: unified cost-guided e-graph
extraction as the single decision surface. Retires the substrate-
without-consumer class of failure mode at root.

---

## 7. Expedite-dev seeds

### Fastest observed test-suite time

**AY.W0 close** — 1491 passed / 0 failed / 40 ignored under the
`ax-iter` profile. This profile inherits `dev`, sets `debug=0`,
`codegen-units=16`, with per-package `opt-level=1` on `bbnf-ir`,
`csp-solver`, `parse_that`. AX's prepare-profile-wave.sh honed the
setup; AY.W0 preserved it.

**Shared `CARGO_TARGET_DIR`** discipline from AU propagated: cache
bytes per grammar (per A4 audit):

| Grammar | Cache bytes | Cache lines | Cold wall |
|---------|------------:|------------:|----------:|
| json | 202,245 | 3,902 | 1.02 s |
| ebnf | 443,023 | 8,649 | 0.39 s |
| sheets | 781,031 | 14,036 | 0.51 s |
| bbnf | 435,805 | 7,531 | 0.53 s |
| **css_l4** | **13,003,357** | **196,760** | **1.81 s** |

**CSS L4 is 85.8% of workspace cache bytes**. Every sub-agent
rebuilding CSS L4 pays the 1.81s / 636 MB RSS tax.

### Slowest bench wall — and why

**A4 finding**: parse_that 107s cold build. AR audit identified
`egraph/{egraph,extract}.rs` clone churn (11 clones reduced to 5
in AR.3.1); AR.3.2 interned `cost_model.rs` string lookups (O(n)
→ O(1)). AT audit continued; AU.6.2 push_fingerprint reduced
per-grammar codegen cost.

### Regen cycle-time floor

**Bootstrap regen via `scripts/bootstrap-bbnf.sh`** — idempotent
at AU close across three consecutive clean-cache runs; at AY.W0
close `generated.rs` stabilised at 29,593 LOC. Cold regen:
5-10 minutes per cycle. AX audit: ≥ 14 regen cycles across
W0a.2 alone, each 5-10 min = 70-140 min of regen per wave.

### Samply setup friction + template commits

**AU Session 2** is the template. Three pre-wave fixes landed
before dispatch:
1. `prepare-profile-wave.sh` — fixed one-entry-only bug.
2. ripgrep → grep — silent-timeout elimination.
3. Bencher substring contamination — filter pattern fix.

Template commits: AU's `profiling-{1,2}.md` authoring pattern with
27 (bench, entry) pairs profiled cleanly on one shared
`CARGO_TARGET_DIR`; `wave.tsv` consumed verbatim; every claim
artefact-cited.

### Known flaky tests

- **AY.W1 `packed_cache_read_beats_soa_materialise`** — 1.3× perf
  threshold near system noise floor; passes ~3 of 5 runs.
  Pre-existing on master at AY open; standalone retry green.
- **Pre-AU `test_selective_transitive_unfurling`** (imports.rs) —
  carry-over orthogonal scope, still failing at AU close under
  --no-fail-fast.
- AW-V.W6 **visitor-path bench (`json_monolithic_value`) no longer
  compiles** due to `has_w4_classified` gate — AY.W2 scope.
- Total real-regression flakiness count: ~4-5 across the arc;
  noise count (perf thresholds): ~1.

### What bleeds the most developer time per tranche

1. **CSS L4 codegen cache** (1.81s, 636 MB RSS per cold build;
   85.8% of workspace cache) — every sub-agent pays. AX.W1r.3a
   `@pretty` refactor reduced CSS L4 rustc 5.81s → 1.81s (−69%),
   RSS 877 MB → 636 MB (−27%). This is AX's largest dev-time
   recovery.
2. **Bootstrap regen cycle time** — 5-10 min × 14 cycles = 70-140
   min per W0a.2-scale wave. Not linear with tranche complexity.
3. **`.bbnf-cache` invalidation** — proc-macro cache is sneaky.
   AQ operational directive: "ALWAYS clear `crates/target/.bbnf-cache`
   + touch `crates/derive/src/lib.rs` before any bench after code
   changes to codegen OR the build will use cached generated code
   and changes won't be tested." Schema version bumps (AY.W0-D
   `26239370` bumps 13 → 14) are the clean escape.
4. **Samply profile rebuild** between waves — mitigated by AU's
   shared `CARGO_TARGET_DIR` + `wave.tsv` contract.
5. **Worktree orphan accumulation** — AY.W0 pruned 50 → 13; 37
   orphans across .claude/worktrees/agent-\* + manually-named
   bbnf-wt-\* directories.

### Top-3 dev-expedite seeds for BB

1. **CSS L4 bench cache ceiling**. AX.W1r.3a dropped 69% already;
   a further pass should target the remaining 1.81s / 636 MB RSS.
   The A4 audit identifies this as the "16.6× larger than next
   per-grammar entry" outlier.
2. **Bootstrap regen idempotency integration in CI**. AV close seed
   names it: "AW should consider a CI step that diffs `generated.rs`
   against a fresh regen on every PR." Not yet landed; would
   eliminate the hand-patched-generated.rs chronic that survived
   11 tranches.
3. **Shared scripts for `.profile-target`**. AU's samply trio is
   the template; AY inherited it clean; BB should cement it as a
   workspace-level `xtask` or `cargo-profile` sub-command. Half the
   worktree orphan count trace to abandoned `.profile-target`
   divergences.

---

## Closing posture

Eighteen tranches produced one historical peak (AU at 0.615 bytes/
cyc twitter / 0.762 citm) and one in-flight restoration (AY.W1 at
0.215 bytes/cyc twitter post-fix). The intervening 10 tranches
(AV through AX) landed load-bearing substrate; **the AY arc now
carries the burden of activating it**.

The AK→AU ascent is the expedite-dev template: profile-driven
plans, samply trio infrastructure, re-plan-on-reveal discipline,
no silent deferrals. The AV→AW-V cascade is the anti-template:
spec-driven plans, substrate-first cadence, bench at close, silent
within-wave deferrals.

AY is the correction-under-contact; BA is the next test of whether
the arc has internalised the AU-era orchestration discipline at
substrate-architecture scale. The top-ranked BA move — unified
e-graph extraction as single decision surface — is the transposition
that retires the substrate-without-consumer class of failure at
root. Two tranches of work; retires ~1,676 LOC of detector cruft;
makes every future lever an e-graph rewrite rule.

---

Document HEAD: worktree `bbnf-wt-ay-audit5` at `a91633e3`
(AY.W3c.2 Value API bench matrix + ratios). Audit authored
2026-04-20 post-AY.W1-fix close. Eighteen tranches covered.
Read-only; no source touched.
