# Tranche AP — Structural Activation, Payload Truthing, Hot-Path Demolition

## Ground Truth (Post-AO Audit, 12 agents)

This tranche is informed by three rounds of parallel agent audits:
AP-prototype-1 (4 agents: samply, cargo expand, tranche recap, structural
failure analysis), AP-prototype-2 (4 agents: ground-truth bench measurements,
expand truth, profile truth, consolidated recap), and the present audit
(8 agents: profiling, codegen inspection, hot-path audit, SIMD gaps,
tranche recapitulation, EmissionTier deletion history, payload truthing,
benchmark infrastructure).

All findings below are grounded in measured artifacts. No claim is
speculative.

---

## Measured Baseline (cached binaries, aarch64-apple-darwin M-series)

### JSON Monolithic (tape-first AOT)

| Dataset    | ns/iter     | MB/s  | sonic-rs | Gap     |
|------------|-------------|-------|----------|---------|
| canada     | 1,283,117   | 1,754 | 1,540    | **+14% BEAT** |
| citm       | 921,574     | 1,874 | 3,097    | -39%    |
| twitter    | 409,851     | 1,540 | 2,736    | -44%    |
| data       | 22,319      | 1,590 | 2,450    | -35%    |
| data_xl    | 21,734,708  | 979   | 1,520    | -36%    |

### CSS L4 Typed (tape-first AOT, `stylesheet.bbnf`)

| Dataset    | ns/iter   | MB/s | Status |
|------------|-----------|------|--------|
| normalize  | 9,841     | 623  | OK     |
| bootstrap  | 830,098   | 337  | OK     |
| tailwind   | —         | —    | **FAIL: offset 387594** |

### Compile Pipeline

| Grammar    | ns/iter     |
|------------|-------------|
| json       | 134,562     |
| ebnf       | 497,831     |
| css_mono   | 308,444     |
| bbnf       | 1,987,523   |
| sheets     | 2,077,262   |
| css_l4     | 10,350,583  |

---

## Profiling Truth (samply, release, arm64)

### JSON citm (1.7 MB, 548 samples)

| Self% | Function | Notes |
|-------|----------|-------|
| **50.4%** | `trim_leading_whitespace_scan_and_cache` | WS bitmap cold path |
| 28.8% | `<JsonParser>::__value` | Dispatch + number/string scan |
| 10.2% | `<JsonParser>::__pair` | Key string scan + memchr |
| 1.6% | `memchr::memchr` | String delimiter |

### JSON twitter (632 KB, 466 samples)

| Self% | Function | Notes |
|-------|----------|-------|
| **39.9%** | `trim_leading_whitespace_scan_and_cache` | Same WS bottleneck |
| 29.4% | `<JsonParser>::__value` | |
| 13.7% | `<JsonParser>::__pair` | |
| 6.4% | `memchr::memchr` | |
| 3.9% | `core::str::from_utf8` | UTF-8 validation |

### JSON canada (2.3 MB, 131 samples)

| Self% | Function | Notes |
|-------|----------|-------|
| **68.7%** | `<JsonParser>::__value` | Number scanning dominates |

### CSS L4 bootstrap (280 KB)

| Self% | Function | Notes |
|-------|----------|-------|
| **29.5%** | `__declaration` | 28-way sequential backtrack, no dispatch |
| **23.9%** | `__compoundSelector` | 4,245 instructions |
| 6.7% | `__value` | |
| 6.3% | `scan_ws_block_comments_slow` | Comment-aware WS |
| 3.4% | `__alignDecl` | |

### compile_css_l4 (10.35 ms)

| Self% | Function |
|-------|----------|
| 5.3% | `quicksort<u32>` |
| 4.6% | `SipHasher::write` |
| 2.2% | `compute_facts_for_node` |
| 2.1% | `solve_component` |
| 1.9% | `bb_recurse` |
| 1.4% | `TapeCursor::children` |
| 1.1% | `TypeDesc::clone` |

---

## Cargo Expand Truth

### JSON (2,859 lines expanded)

- **Structural dispatch: NOT ACTIVE.** Zero `advance_to_structural`,
  zero `scan_structural`, zero `filter_quote_parity` in emitted code.
  The `structural_mode` flag exists in the emitter but never flips to
  `true` for the JSON bench target. The `compute_structural_bytes` IR
  pass populates `ir.structural_bytes` at compile time, but the emitted
  `parse()` entry point for the proc-macro derive path does not wire
  the pre-scan. The bench binary's parser struct is generated via
  `#[derive(Parser)]` which goes through a different entry point than
  the `generate::emit_grammar` path that has the structural wiring.

- **Payload projection: EFFECTIVELY DEAD for JSON.** The number branch
  in `__value` calls `scan_json_number_span` (span-only), not the f64
  capture path. Root cause: AO Phase 2.2 renamed the kernel function
  but the payload wiring in `emit_regex_match_impl` still references
  the old name. Additionally, `__has_payload` is declared but never
  set to `true` in the JSON expanded output.

- **24 `trim_leading_whitespace_mut` calls** in the parse path. These
  are the source of the 40-54% WS overhead seen in samply.

### CSS L4 (103,378 lines expanded)

- **222 `scan_ws_block_comments` calls** — @ws kernel correctly routed.
- **113 `is_ascii_whitespace` uses** in inline separator patterns that
  BYPASS the @ws kernel — correctness bug for CSS comments in separator
  positions.
- **`__declaration`: 28-way sequential backtrack, NO dispatch table.**
  Each branch tries a property name, backtracks on failure. This
  function alone is 29.5% of CSS L4 runtime.
- **43 copies** of inline `ws + ':' + ws` scanner pattern.
- **42 copies** of `!important` pattern.
- **24 functions** declare `__payload_f64` but never use it.

---

## Consolidated Tranche Recap (AA → AO)

### What Actually Landed

| Tranche | Deliverable | Parse Impact |
|---------|-------------|--------------|
| AA | E-graph substrate (TypeDescInterner, GrammarAnalysis, Lattice, csp-solver) | — |
| AB | MaterializationClass lattice (MustTape/TapeSpanOnly/TransparentElide) | — |
| AC | Atomic tape transposition: every rule → `Option<TapeOffset>` | Break-even |
| AE | Shape-agnostic lowering, panic-never-Epsilon | — |
| AF | Universal CostWeights, per-component CSP, EmissionTier lattice | — |
| AG | Build parallelism (33→4 binaries), CSP tier variables activated | — |
| AI | EmissionTier wired into emitter, eligibility widened, view stubs→real | — |
| AJ | Zero-alloc TapeCursor, zero-copy Parsed, canada fix | +10% canada |
| AK | Flat Vec tape, per-branch variant discriminator | **+10-14% all** |
| AM | EmissionTier deleted (-2,306 LOC), payload buffer, per-branch surgery | **+4-17% all** |
| AN | PayloadKind, Serialize rewrite, @ws SIMD routing, WS bitmap cache | +5-8% |
| AO | Structural bytes IR pass, structural mode codegen (NOT ACTIVE) | 0% |

### What Was Designed But Never Activated

1. **Structural dispatch** — Infrastructure built across AO.0.1–0.6
   (IR pass, ParserState cursor, quote parity filter, pre-scan codegen,
   structural-aware dispatch, WS elision). All code exists. Never
   exercised end-to-end. The proc-macro derive path does not call the
   structural pre-scan entry point.

2. **Direct-to-struct projection (Tier B)** — Designed in AF-prototype,
   implemented in AI.1-AI.4 with a three-function triad ABI, deleted in
   AM.1 because the ABI split made Direct-tier functions unable to call
   Tape-tier children, causing reconciliation to widen everything back
   to Tape. Net: -2,306 LOC, zero performance impact.

3. **Lazy AST evaluation (Tier C)** — Never built. The tape IS the
   lazy substrate; the view layer projects on demand. No formal
   DirectSlot/LazyView infrastructure was created.

4. **Global CSP solve** — Still per-component. Cross-component
   optimization impossible. CSS L4 particularly impacted.

### What Is Broken

1. **Core crate does not compile.** 6 errors: `BbnfBootstrapRuleKind::rhs`
   (4 sites) and `::value_expr` (2 sites) reference variants that no
   longer exist in `generated.rs`. Bench binaries work only from cached
   artifacts.

2. **CSS L4 tailwind FAIL at offset 387594.** The grammar is correct:
   `/[^;!}]*/` properly excludes `!` for `!important` detection. The
   bug is in codegen: `/*!*/` is a CSS comment that the `@ws`
   comment-aware kernel should consume BEFORE the value regex sees
   the `!`. Tailwind uses `var(--tw-empty,/*!*/ /*!*/)` — 278
   occurrences in tailwind.css. The `@ws` kernel is not active in
   value-scanning positions. This is the same root cause as item 4.

3. **Payload projection dead for JSON.** Kernel rename broke the wiring.

4. **CSS L4 `@ws` kernel not universally active.** 113 `?w` separator
   positions AND all value-scanning positions use `is_ascii_whitespace`
   instead of the `@ws` comment-aware kernel. CSS comments can appear
   in separator and value positions — this is a codegen bug, not a
   grammar bug. The `@ws` directive declares the comment-aware scanner;
   the emitter must use it everywhere whitespace is consumed, not just
   in explicit `?w` calls.

5. **CSS L4 typed grammar ~5 type errors.** Seq child type projection,
   Repeat Span compression, Wrap/delim-scan mismatches.

---

## Honest Naming: What the System Actually Is

Stop calling the current state "direct-to-struct". The system is:

**Tape + f64 side-channel payloads + lazy cursor views.**

- The parser writes 16-byte `TapeRec` records to a flat `Vec<TapeRec>`.
- For `f64` values only (via `FnDescriptor::NumberConvert`), the parser
  ALSO writes 8 bytes to a separate `Vec<u8>` payload buffer, indexed
  by `TapeRec::payload_idx`.
- `PayloadKind::Bool` and `PayloadKind::U8` are defined but **never
  assigned** by codegen. Zero uses in emitted code.
- The view layer wraps `TapeCursor` pointing into the tape. All
  accessors return either scalars or other `View<'p>` cursor wrappers.
  No accessor returns a typed Rust struct or data-carrying enum variant.
- **1 of 7** JSON rules uses payloads (~14%). **~1 of 265** CSS L4
  rules uses payloads (<0.4%). Even with Bool/U8 wired, coverage would
  reach ~8.7% of CSS L4 rules.

Real direct-to-struct projection would mean: the parser returns typed
Rust values (structs, enums) directly, without writing tape records for
those values. The deleted Tier B system attempted this with a separate
ABI and failed. The correct approach — identified in AL-prototype-1 —
is to keep the unified tape ABI but enrich it with typed payloads that
cover ALL leaf values, not just f64.

---

## AP Plan

### Design Principles

1. **Fix what's broken before optimizing.** Build errors, parse
   failures, dead wiring — all resolved in Phase 0.
2. **Activate what's built.** Structural mode, payload projection —
   the code exists, it just isn't running.
3. **Profile-driven.** The samply data says WS is 50% of citm. That's
   the #1 target.
4. **Honest naming.** No "direct-to-struct" until we have actual struct
   projection. Payload enrichment is the path.
5. **No deferrals.** Everything in this plan ships within the tranche.

---

## Phase 0: Build Restoration + Correctness (BLOCKING)

### AP.0.1 Fix bootstrap enum references

Remove `BbnfBootstrapRuleKind::rhs` from match arms in 4 files (the
current grammar no longer produces an `rhs` rule). Remove
`BbnfBootstrapRuleKind::value_expr` from 2 files (grammar inlined this
wrapper; `value_or` is the replacement). Do NOT regenerate
`generated.rs` — that requires resolving two deeper structural-mode
codegen deficits. Just fix the consumers.

**Files:**
- `crates/core/src/graph/deps.rs:105`
- `crates/core/src/graph/metadata.rs:48,76`
- `crates/core/src/lower/tape_walk.rs:116`
- `crates/core/src/lower/expression.rs:551`
- `crates/core/src/lower/value_expr.rs:104`

### AP.0.2 Fix CSS L4 `@ws` kernel universality

The `@ws` comment-aware kernel (`scan_ws_block_comments`) is correctly
routed for explicit `?w` calls (222 sites), but the emitter ALSO
generates bare `is_ascii_whitespace` checks in two codegen paths that
bypass the `@ws` directive:

1. **Inline separator patterns** — 113 sites where `?w` around `:`,
   `,`, and other separators emits plain ASCII whitespace instead of
   the comment-aware kernel.

2. **Value-scanning positions** — the value regex `/[^;!}]*/` in
   `customPropertyDecl` and `genericDecl` is correct (it excludes `!`
   for `!important` detection), but CSS comments like `/*!*/` contain
   `!` and must be consumed by the `@ws` kernel BEFORE the value regex
   runs. The emitter must interleave `@ws` consumption within or around
   value-scanning regex spans so comments are transparent.

The grammar is NOT wrong. The grammar correctly declares `@ws` with
the comment-aware pattern. The bug is that the emitter doesn't honor
`@ws` universally — it uses the kernel for some positions and bare
ASCII for others. Every position where whitespace or comments can
appear in CSS must route through the `@ws` kernel.

**Root cause:** The emitter's `emit_ws_trim_impl` respects `@ws` but
the inline separator codegen in `seq.rs`/`repeat.rs` and the regex
value-span codegen don't consult `ir.ws_pattern`. Fix both paths.

**Files:**
- `crates/core/src/backend/rust/emitter/seq.rs` (separator WS)
- `crates/core/src/backend/rust/emitter/repeat.rs` (separator WS)
- `crates/core/src/backend/rust/emitter/leaves.rs` (value-span WS)

This single fix resolves both the 113 separator bypass AND the
tailwind parse failure (offset 387594).

### AP.0.3 Fix payload wiring for JSON

AO Phase 2.2 renamed `scan_number_f64` → `scan_json_number_span` in
the kernel, but the payload path in `emit_regex_match_impl` still emits
the old function name. Restore the f64 capture path so `push_leaf_with_f64`
actually fires for numeric leaves.

**File:** `crates/core/src/backend/rust/emitter/leaves.rs`

### AP.0.4 Delete CSS monolithic benchmark

Per directive. CSS L4 is the only CSS bench target. Delete
`crates/core/benches/css/monolithic.rs`. Update `Cargo.toml` if needed.

### AP.0.5 Verify `cargo test --workspace`

All fixes above, then full workspace compile and test pass.

### Verification
```bash
cargo test --workspace
cargo bench -p bbnf --bench css_l4 --no-run  # tailwind compiles
```

---

## Phase 1: Structural Dispatch Activation (the +50% lever)

Samply shows WS scanning is 50% of citm and 40% of twitter. The
structural dispatch codegen already exists and eliminates WS trim
entirely (WS → no-op when structural index is active). Phase 1
activates it.

### AP.1.1 Diagnose why structural mode doesn't activate

The `compute_structural_bytes` IR pass runs (AO.0.1). The emitter
checks `ir.structural_bytes.is_some() && ir.ws_pattern.is_none()`.
But the expanded JSON code has zero `scan_structural` calls. Trace
the activation chain:

1. Does `compute_structural_bytes` populate `ir.structural_bytes`?
2. Does `generate::emit_grammar` read the flag?
3. Does the proc-macro derive path call `generate::emit_grammar` or
   a different entry point that skips the structural wiring?

**Files:** `crates/ir/src/passes/structural_bytes.rs`,
`crates/core/src/generate/mod.rs:56`,
`crates/core/src/backend/rust/emitter/grammar.rs:462-483`

### AP.1.2 Wire structural pre-scan into derive path

If the proc-macro derive path skips `emit_grammar_impl` (which has
the structural pre-scan codegen), wire it in. The structural pre-scan
must execute in the generated `parse()` function regardless of entry
point.

### AP.1.3 End-to-end validation

`cargo expand` the JSON bench target. Require:
- `scan_structural` call present
- `filter_quote_parity` call present
- `advance_to_structural` in dispatch arms
- Zero `trim_leading_whitespace_mut` calls (WS elided)

### AP.1.4 Fix structural mode bugs

The codegen has never been exercised. Expect correctness bugs in:
- `advance_to_structural()` cursor management on backtrack
- Quote parity edge cases (escaped quotes, nested strings)
- Checkpoint save/restore of `structural_cursor`
- Structural byte set must include `"` for quote parity

### AP.1.5 Size gating

Pre-scan overhead is amortized for large inputs but dominates for
small inputs. Gate on `input.len() > 4096`. Below threshold, fall
back to byte-at-a-time dispatch.

### AP.1.6 Profile validation

samply profile citm/twitter with structural mode active. Target:
`trim_leading_whitespace_scan_and_cache` drops below 5% (from 50%).

### Verification
```bash
cargo expand -p bbnf --bench json_monolithic 2>&1 | grep scan_structural  # present
cargo bench -p bbnf --bench json_monolithic  # citm ≥ 2,800 MB/s
samply record ... json_citm  # WS < 5%
```

---

## Phase 2: Payload Enrichment (honest "direct projection")

The current payload system covers only f64 via `NumberConvert`. Bool
and U8 are defined but never wired. Real direct projection means ALL
deterministic leaf values store typed payloads, not just numbers.

### AP.2.1 Wire PayloadKind::Bool

`"true" -> true | "false" -> false` maps should emit
`push_leaf_with_bool`. Detection: when `FnDescriptor::Constant` has
a boolean value and the parent is a `TapeSpanOnly` Alt.

**Files:** `crates/core/src/backend/rust/emitter/mod.rs` (payload
detection), `crates/core/src/backend/rust/emitter/tape_prelude.rs`
(epilogue), `crates/core/src/backend/rust/view/leaves.rs` (accessor)

### AP.2.2 Wire PayloadKind::U8

`"px" -> 0u8 | "em" -> 1u8` maps (86 branches across CSS L4 keywords
alone) should emit `push_leaf_with_u8`. Detection: when
`FnDescriptor::Constant` has a `u8` value.

### AP.2.3 Extend PayloadKind to U16/U32

For CSS L4 hex color values (`FnDescriptor::HexConvert`) and larger
enum discriminants. Add `push_leaf_with_u32` to builder, `payload_u32`
to tape reader, `PayloadKind::U32` to enum.

### AP.2.4 View accessor typed returns

Generate `.as_bool() -> bool`, `.as_u8() -> u8`, `.as_u32() -> u32`
on view types with the corresponding `PayloadKind`. These read the
payload buffer in O(1) and never touch source text.

### AP.2.5 Payload coverage audit

After 2.1-2.4, measure payload coverage:
- JSON: number (f64) + bool → 2/7 substantive rules (~29%)
- CSS L4: number (f64) + keywords (u8) + hex colors (u32) → ~23/265
  rules (~9%)

### Verification
```bash
cargo expand -p bbnf --bench json_monolithic | grep push_leaf_with_bool  # present
cargo expand -p bbnf --bench css_l4 | grep push_leaf_with_u8  # present
cargo test --workspace
```

---

## Phase 3: WS Hot-Path Surgery

Even with structural dispatch active for large inputs, small inputs
and non-structural grammars (CSS L4 with `@ws`) still hit the WS
path. Attack the remaining WS overhead.

### AP.3.1 SIMD bitmap construction in scan_and_cache

The cold-path `trim_leading_whitespace_scan_and_cache` builds a
64-bit bitmap with a scalar 8-bytes-at-a-time loop. Replace with
NEON `cmeq.16b` / portable_simd `u8x16::simd_eq` — 16 bytes per
iteration. The existing Tier 1 fast path already uses SIMD; extend
to Tier 3.

**File:** `parse-that/rust/parse_that/src/scanners.rs:202-252`
**Expected:** -50% of scan_and_cache time → -20-27% total JSON time
on non-structural paths.

### AP.3.2 Reduce redundant trim calls

24 trim calls in JSON parse path. Sep_by loops call trim after comma
AND before next value — redundant when the comma already consumed
trailing WS. Elide consecutive trims via a `last_trim_offset` guard
or by combining `literal + ?w` into a fused scan.

**File:** `crates/core/src/backend/rust/emitter/repeat.rs`
**Expected:** -30% of trim calls → -5-10% on non-structural paths.

### AP.3.3 Wire `scan_quoted_string_simd` into production

`scan_string_quoted` (general kernel, used by CSS) uses `memchr2`.
`quoted_string_scan_full` (JSON-specific) uses the SIMD escape-parity
scanner. Route ALL double-quoted string scanning through the SIMD
path, falling back to `memchr2` for single-quoted strings.

**Files:** `parse-that/rust/parse_that/src/parsers/scan/quoted.rs`,
`crates/core/src/backend/kernels/quoted_string.rs`

### AP.3.4 SIMD `filter_quote_parity`

The structural scanner's quote-parity filter is entirely scalar
(backward linear scan for backslashes per quote position). Rewrite
to operate on bitmasks using the `escaped_mask` technique from
`quoted_simd.rs` — bulk parity computation per 16-byte chunk.

**File:** `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`

---

## Phase 4: CSS L4 Hot-Path Demolition

CSS L4 is dominated by dispatch overhead (92%), not scanning.
`__declaration` alone is 29.5% of runtime — a 28-branch sequential
checkpoint cascade.

### AP.4.1 Key dispatch for `__declaration`

The 28 property-name branches share first bytes (`m` for margin/
max-width, `f` for font-*/flex-*, `p` for padding/position), so
byte dispatch is insufficient. Use key dispatch: scan identifier →
compare against known keys. The `KeyDispatch` pattern detector
already exists in `crates/core/src/backend/patterns/key_dispatch.rs`.

Restructure `grammar/css/l4/properties.bbnf` to present the
declaration as `ident ?w ':' ?w Alt(typed_branches) | genericDecl`
instead of `Alt(28 sequential branches)`.

**Expected:** 28 sequential backtracks → 1 ident scan + hash lookup
→ -20% CSS L4 time.

### AP.4.2 Hoist duplicated patterns

43 copies of `ws + ':' + ws` and 42 copies of `!important` in
expanded output. Factor into shared inline helper functions or
grammar-level shared rules marked `@no_collapse`.

### AP.4.3 CSS L4 type error fixes

Fix the ~5 remaining type errors in the CSS L4 typed grammar:
- Seq child type projection with `collapse_simple_spans`
- Repeat Span compression
- Wrap/delim-scan type mismatches

### AP.4.4 CSS L4 structural mode investigation

CSS L4 has `@ws /.../ ;` (custom whitespace pattern), which gates
structural mode OFF. Investigate: can CSS structural dispatch work
alongside comment-aware WS? Options:
1. Extend structural mode to support custom WS that is pure
   comment-skip (treat comment openings as structural).
2. Accept CSS L4 stays non-structural but attack WS via kernel
   optimization instead.

---

## Phase 5: Scanner Surgery + View Layer

### AP.5.1 NibbleLut emission in DFA codegen

`try_emit_accel_scan` at `dfa/helpers.rs:218` returns `None` for
`NibbleLut` strategy. Wire it to emit `::parse_that::find_first_of_nibble_lut`
for DFA self-loop states with 4-8 exit bytes.

### AP.5.2 SIMD delimiter scan inner loop

`balanced_wrap.rs:69-89` scans byte-at-a-time for pivot/close/open.
Replace with `find_first_of_3(haystack, pivot, close, open)` — 16
bytes per iteration instead of 1.

### AP.5.3 `Tape::get` unchecked access

Add `Tape::get_unchecked(offset)` with `debug_assert!` guard. Wire
view-layer `TapeCursor` to use it. Eliminates a branch per record
access.

### AP.5.4 Deferred UTF-8 validation

`from_utf8` is 3.9% of twitter. `Span::new` validates UTF-8 at
construction. Defer validation to `.as_str()` on the view layer.
Store raw byte offsets during parse — validation happens only when
the user actually reads the string content.

### AP.5.5 `TapeBuilder` default pre-allocation

`TapeBuilder::new()` starts empty. Change to `input.len() / 4`
default capacity (same heuristic as `with_capacity`).

---

## Phase 6: Calibration + Benchmarks

### AP.6.1 Full benchmark sweep

Single-invocation bench sweep for JSON (5 datasets) + CSS L4 (3
datasets) + compile pipeline (6 grammars). Per-bench subprocess
invocations are FORBIDDEN (Tranche Z bench-sweep invariant).

### AP.6.2 samply profile comparison

Save post-AP profiles to `docs/benchmarks/profiles/post-AP/`.
Diff against post-AO profiles. Every "+X%" claim must cite a
symbol name + self-time delta.

### AP.6.3 Create `docs/benchmarks/post-AP.json`

Standard schema matching post-Z.json format:
- `tag`, `description`, `arch`, `hardware`
- `tests` (workspace pass counts)
- `benches` (json_monolithic, css_l4, compile_pipeline with
  ns_per_iter, mb_per_sec, delta_vs_post_z_pct)
- `gates` (hard: citm ≥ 2,800, tailwind parses; soft: twitter ≥
  2,200, css_l4 bootstrap ≥ 500)
- `gates_vs_post_z` (per-bench target vs observed)
- `phase_summary` (per-phase status + impact)

### AP.6.4 Cost model grid sweep

Sweep `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`
across JSON + CSS L4. No individual bench regresses >1%.

### AP.6.5 Global CSP solve for CSS L4

Currently per-component. CSS L4 has many import modules = many
components. Cross-component optimization opportunities are missed.
Implement optional global solve.

---

## Performance Targets

### JSON (target: WS < 5% of profile with structural mode)

| Dataset | Current | AP Target | sonic-rs | Goal |
|---------|---------|-----------|----------|------|
| canada  | 1,754   | 2,000+    | 1,540    | **BEAT** (maintain) |
| citm    | 1,874   | **3,200+** | 3,097   | **BEAT** |
| twitter | 1,540   | **2,500+** | 2,736   | CLOSE (10%) |
| data    | 1,590   | 2,200+    | 2,450    | CLOSE |
| data_xl | 979     | 1,500+    | 1,520    | PARITY |

### CSS L4 (target: `__declaration` < 10% of profile)

| Dataset   | Current | AP Target | Key Lever |
|-----------|---------|-----------|-----------|
| normalize | 623     | 900+      | Key dispatch + payload |
| bootstrap | 337     | 600+      | Key dispatch + pattern dedup |
| tailwind  | FAIL    | 500+      | @ws universality + dispatch |

### Hard Gates

- `cargo test --workspace` passes
- CSS L4 tailwind parses without error
- JSON citm ≥ 2,800 MB/s
- `docs/benchmarks/post-AP.json` exists with all fields populated
- Every "+X%" claim cites a samply symbol + self-time delta

### Soft Gates

- JSON twitter ≥ 2,200 MB/s
- CSS L4 bootstrap ≥ 500 MB/s
- WS self-time < 10% on JSON citm (from 50%)
- PayloadKind::Bool and ::U8 have nonzero uses in expanded code

---

## Execution Waves (4 agents per wave)

### Wave 1: Foundation (AP.0)
- **Agent A:** Fix bootstrap enum refs (0.1) + verify compilation (0.5)
- **Agent B:** Fix `@ws` kernel universality — separator + value positions (0.2)
- **Agent C:** Fix payload wiring (0.3)
- **Agent D:** Delete CSS monolithic bench (0.4) + CSS L4 type errors (4.3)

### Wave 2: Structural Activation (AP.1)
- **Agent A:** Diagnose + fix structural mode activation (1.1-1.2)
- **Agent B:** End-to-end validation + bug fixes (1.3-1.4)
- **Agent C:** Size gating (1.5) + SIMD quote parity filter (3.4)
- **Agent D:** Payload Bool/U8 wiring (2.1-2.2)

### Wave 3: Hot-Path + CSS (AP.2-4)
- **Agent A:** Key dispatch for CSS `__declaration` (4.1)
- **Agent B:** SIMD WS bitmap + trim reduction (3.1-3.2)
- **Agent C:** SIMD string scanner wiring (3.3) + NibbleLut (5.1)
- **Agent D:** CSS pattern dedup (4.2) + structural investigation (4.4)

### Wave 4: Surgery + Calibration (AP.5-6)
- **Agent A:** Tape unchecked access + deferred UTF-8 (5.3-5.4)
- **Agent B:** SIMD delimiter scan (5.2) + TapeBuilder prealloc (5.5)
- **Agent C:** Full bench sweep + samply profiles (6.1-6.2)
- **Agent D:** Create post-AP.json + cost model sweep + global CSP (6.3-6.5)

---

## Critical Files

| File | Issue | Phase |
|------|-------|-------|
| `crates/core/src/graph/deps.rs` | `rhs` not in enum | 0.1 |
| `crates/core/src/graph/metadata.rs` | `rhs` not in enum | 0.1 |
| `crates/core/src/lower/tape_walk.rs` | `rhs` not in enum | 0.1 |
| `crates/core/src/lower/expression.rs` | `value_expr` not in enum | 0.1 |
| `crates/core/src/lower/value_expr.rs` | `value_expr` not in enum | 0.1 |
| `crates/core/src/backend/rust/emitter/seq.rs` | @ws kernel not universal | 0.2 |
| `crates/core/src/backend/rust/emitter/repeat.rs` | @ws kernel not universal | 0.2 |
| `crates/core/src/backend/rust/emitter/leaves.rs` | @ws in value spans + payload fn name | 0.2, 0.3 |
| `crates/core/benches/css/monolithic.rs` | DELETE | 0.4 |
| `crates/ir/src/passes/structural_bytes.rs` | Activation trace | 1.1 |
| `crates/core/src/generate/mod.rs` | structural_mode flag | 1.1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | Pre-scan entry | 1.2 |
| `crates/core/src/backend/rust/emitter/alt.rs` | Structural dispatch | 1.3 |
| `crates/core/src/backend/rust/emitter/ws.rs` | WS elision | 1.3 |
| `crates/core/src/backend/rust/emitter/mod.rs` | Payload detection | 2.1 |
| `crates/core/src/backend/rust/emitter/tape_prelude.rs` | Payload epilogue | 2.1 |
| `crates/core/src/backend/rust/view/leaves.rs` | Typed accessors | 2.4 |
| `parse-that/src/scanners.rs` | SIMD bitmap cold path | 3.1 |
| `parse-that/src/parsers/scan/quoted.rs` | SIMD string wiring | 3.3 |
| `parse-that/src/parsers/scan/quote_parity.rs` | SIMD parity filter | 3.4 |
| `grammar/css/l4/properties.bbnf` | Declaration dispatch | 4.1 |
| `crates/core/src/generate/regex/emit/dfa/helpers.rs` | NibbleLut | 5.1 |
| `crates/core/src/backend/kernels/balanced_wrap.rs` | SIMD delimiter | 5.2 |
| `crates/bbnf-tape/src/tape.rs` | Unchecked access | 5.3 |
| `crates/bbnf-tape/src/builder.rs` | Default prealloc | 5.5 |
| `docs/benchmarks/post-AP.json` | CREATE | 6.3 |

---

## Deferred Items Inventory (carried from all prior tranches)

Every item below was deferred across AA–AO. AP explicitly addresses
some; the rest are declared OUT OF SCOPE with rationale.

### Addressed by AP

| Item | Origin | AP Phase |
|------|--------|----------|
| CSS L4 tailwind parse failure (codegen @ws gap) | AN.0.5 | AP.0.2 |
| Payload Bool/U8 wiring | AN Phase 0 | AP.2.1-2.2 |
| Structural dispatch activation | AO Phase 0 | AP.1 |
| NibbleLut DFA codegen | AO Phase 3 | AP.5.1 |
| Bootstrap compile error | AO dirty worktree | AP.0.1 |
| Cost model calibration | AM.6 → AO Phase 4 | AP.6.4 |
| Global CSP solve | AL.4 → AO Phase 4 | AP.6.5 |
| CSS declaration dispatch | new finding | AP.4.1 |
| SIMD quote parity | AO Phase 0.3 | AP.3.4 |
| SIMD delimiter scan | new finding | AP.5.2 |
| post-AA benchmark gap | AP-prototype-2 | AP.6.3 |

### Out of Scope (with rationale)

| Item | Origin | Rationale |
|------|--------|-----------|
| Clean bootstrap regen | AC.2 | Blocked by two structural-mode codegen deficits; fixing these is a tranche-sized effort. The 6-line consumer fix (AP.0.1) is sufficient. |
| 32-byte AVX2 SIMD widening | AO Phase 3 | Premature until structural mode is proven. 16-byte NEON/SSE2 is sufficient for current targets. |
| Padded buffer mode | AO Phase 1 | Marginal gain (+3-5%) vs complexity. Defer until structural mode gains plateau. |
| SIMD digit-to-integer | AO Phase 3.2 | Canada already beats sonic-rs. Numeric SIMD is not the bottleneck. |
| TaggedUnion boxing | AL.6 | Insufficient profile evidence. TapeSpanOnly already eliminates most boxing. |
| TS/WASM backend tape migration | various | Out of scope — Rust-only for performance work. |
| E-graph→CSP feedback bridge | AL.5 | Research-grade; no production grammar has demonstrated need. |
| Profile-guided cost calibration | AC.16 | Grid sweep (AP.6.4) is the practical alternative. |
| Real struct projection ABI | AF/AI | The deleted Tier B approach (separate ABI) is architecturally wrong. Payload enrichment (AP.2) is the correct path. If payload coverage exceeds 50% of rules, revisit in a future tranche with arena-based struct projection using the same tape ABI. |

---

## Profiling Methodology

Unchanged from Tranche Z. Pre-AP baselines are the measurements in
this document's "Measured Baseline" section. Post-AP profiles land at
`docs/benchmarks/profiles/post-AP/`.

### Build + resolve + record

```bash
cargo bench -p bbnf --bench json_monolithic --no-run
cargo bench -p bbnf --bench css_l4 --no-run
cargo bench -p bbnf --bench compile_pipeline --no-run

JSON_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 \
  -name 'json_monolithic-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
CSS_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 \
  -name 'css_l4-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)
PIPE_BIN=$(find target/release/deps -maxdepth 1 -type f -perm -111 \
  -name 'compile_pipeline-*' ! -name '*.d' ! -name '*.dSYM' | xargs ls -t | head -1)

mkdir -p docs/benchmarks/profiles/post-AP

(cd crates/core && samply record --save-only --unstable-presymbolicate \
  -o ../../docs/benchmarks/profiles/post-AP/json_citm.samply \
  -- "$JSON_BIN" citm_catalog --bench)

(cd crates/core && samply record --save-only --unstable-presymbolicate \
  -o ../../docs/benchmarks/profiles/post-AP/json_twitter.samply \
  -- "$JSON_BIN" twitter --bench)

(cd crates/core && samply record --save-only --unstable-presymbolicate \
  -o ../../docs/benchmarks/profiles/post-AP/css_l4_bootstrap.samply \
  -- "$CSS_BIN" bootstrap --bench)

(cd crates/core && samply record --save-only --unstable-presymbolicate \
  -o ../../docs/benchmarks/profiles/post-AP/css_l4_tailwind.samply \
  -- "$CSS_BIN" tailwind --bench)
```

### Bench sweep invariant

Parse benches MUST run in a single `"$BIN" --bench` invocation. No
per-bench subprocess. Tranche Z post-mortem: individual `"$BIN" citm
--bench` runs produce ~5-12% slower numbers than the sweep — this is
process-startup artifact, not real regression.
