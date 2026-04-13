# Tranche AQ — Generalization, Direct-to-Struct, Structural Dispatch Activation

## Ground Truth (Post-AP Deep Audit, 6 parallel agents)

This tranche is informed by a 6-agent parallel audit documenting:
- Complete prior-tranche status (AA→AP): `docs/tranches/AQ-audit.md`
- JSON/CSS/BBNF/Sheets parse + compile profiling: `docs/benchmarks/profiles/post-AP/`
- Structural dispatch root-cause analysis
- sonic-rs architectural comparison
- Generalization debt inventory (5 nominal RegexClass variants, 7 sp_*/sp_*, 6 scan_json_*, hardcoded byte sets)

All findings are grounded in measured profile data and source reads. No claim is speculative.

---

## Measured Baseline (post-AP, aarch64-apple-darwin M4 Max)

### JSON Monolithic
| Dataset | MB/s | sonic-rs | Gap |
|---------|------|----------|-----|
| canada | 1,797 | 1,540 | **+17% BEAT** |
| citm | 2,712 | 3,000 | -10% |
| twitter | 2,173 | 2,643 | -18% |
| data | 1,900 | 2,346 | -19% |
| data_xl | 1,341 | 1,460 | -8% |

### CSS L4 Typed
| Dataset | MB/s | Status |
|---------|------|--------|
| normalize | 978 | OK |
| bootstrap | 505 | OK |
| tailwind | 534 | OK (fixed in AP) |

### Compile Pipeline
| Grammar | ns/iter |
|---------|---------|
| json | 121,787 |
| ebnf | 378,709 |
| bbnf | 1,610,534 |
| sheets | 2,133,446 |
| css_l4 | 9,558,664 |

### Profile Truth (samply, citm 3382 samples)

| Self% | Function | Notes |
|-------|----------|-------|
| **56.4%** | `<JsonParser>::__value` | 5132-byte function body |
| **28.2%** | `<JsonParser>::__pair` | 2364 bytes |
| **11.9%** | `trim_leading_whitespace_scan_and_cache` | Cold path, no longer dominant |
| 1.6% | memchr | |

### CSS L4 Hot Paths
| Dataset | Function | Self% |
|---------|----------|-------|
| bootstrap | `__declaration` | 33.2% |
| normalize | `__compoundSelector` | 40.2% |
| tailwind | `__compoundSelector` | 36.6% |
| tailwind | `scan_ws_block_comments_slow` | 9.2% |

## Architectural State

### What Works
- Tape-first zero-copy parse (AC)
- Materialization tripartite: MustTape/TapeSpanOnly/TransparentElide (AB)
- Per-branch tape surgery with `__branch_idx` (AK)
- F64/Bool/U8 payload projection (AN/AP)
- Key dispatch for declaration-style Alts (AP.4)
- @ws kernel universality (AP.0.2)
- SIMD WS bitmap, SIMD string scanner, NibbleLut DFA, SIMD delim scan (AP.3/AP.5)

### What Does NOT Work
1. **Structural dispatch is dead code.** `structural_mode=false` hard-coded. Infrastructure (IR pass, parse-that helpers, alt.rs hybrid codegen, pre-scan emission) is complete but never activates. The claim "pre-scan overhead > savings" is TRUE for current implementation, FALSE as general architectural conclusion. Fixable.

2. **Direct-to-struct projection does not exist.** What we call "direct projection" is tape + side-channel f64/bool/u8 payloads + cursor-wrapping views. No accessor returns a typed Rust struct or data-carrying enum variant. Payload coverage: 14% JSON, <1% CSS L4.

3. **Language-specific overfitting.** 5 `RegexClass` variants named after languages. 6 scanner functions prefixed `scan_json_`. 7 `sp_json_*`/`sp_css_*` wrappers. Hardcoded `STRUCTURAL_PUNCTS = b",:{}[]"` JSON byte set. `emit_json_call` kernel entry.

4. **Clean bootstrap regen still deferred.** `generated.rs` hand-patched. `grammar_roundtrip` tests `#[ignore]`-gated with `usize::MAX` sentinels.

5. **Generalization debt in IR walkers.** `single_byte_literal` duplicated 3×, `unwrap_wrap` duplicated 2×, `extract_leading_literals`/`resolve_to_seq` live as private key_dispatch helpers.

---

## Design Principles

1. **Gestalt architectural transpositions** — if a system is fundamentally broken (separate-ABI direct projection, overfit RegexClass taxonomy), re-architect it, don't patch.
2. **No legacy code** — every deferred item either ships or is explicitly deleted with rationale.
3. **Structural before nominal** — regex classification parameterizes on shape, not language. Scanner names describe what they scan, not where they were first used.
4. **Profile-driven** — every optimization must be measurable via samply.
5. **Single path** — one codegen path, one regex system, one scanner-dispatch enum.

---

## AQ Plan — 9 phases

## Phase 0: IR Inspection Module (Foundation)

Duplication in recognizer miners and key_dispatch prevents clean refactoring. Phase 0 consolidates.

### AQ.0.1 Create `bbnf_ir::passes::inspect` module
```
crates/ir/src/passes/inspect/
    mod.rs
    walk.rs      // visit_children_alt (moved from recognizers::mod)
    unwrap.rs    // unwrap_wrap, unwrap_map_ow
    resolve.rs   // resolve_to_seq, unwrap_to_alt, unwrap_to_repeat
    literal.rs   // single_byte_literal (unified from 3 copies)
    leading.rs   // extract_leading_literals, extract_leading_regex_pattern
```

### AQ.0.2 Migrate recognizer miners to use inspect
All miners in `crates/ir/src/passes/recognizers/` import from `passes::inspect`. Delete duplicates.

### AQ.0.3 Migrate key_dispatch
`key_dispatch.rs` stops being a closed universe. Its helpers move to inspect; it only owns dispatch-specific logic.

**Files:**
- NEW: `crates/ir/src/passes/inspect/` (5 files)
- MODIFY: all files in `crates/ir/src/passes/recognizers/`

## Phase 1: Deoverfit RegexClass

### AQ.1.1 Expand structural variants with parameters

Add to `bbnf_regex::classify::RegexClass`:
- `Numeric { allows_sign, allows_fraction, allows_exponent, reject_leading_zero, allow_leading_dot }`
- `QuotedString { quote_char, allows_escapes, allows_u_escapes }`
- `Identifier { allows_leading_dash, allows_double_dash_prefix }`

Rename `WsBlockComment` → `WhitespaceWithBlockComment` (structural name).

### AQ.1.2 Delete nominal variants
Delete `JsonString`, `JsonNumber`, `CssIdent`, `CssQuotedString` variants.

### AQ.1.3 Delete exact-string dictionary
Delete `classify_known_pattern` function and all `*_PATTERNS` constants from `regex/src/classify/mod.rs`.

### AQ.1.4 Update structural classifiers
`try_classify_numeric` / `try_classify_quoted_string` / `try_classify_identifier` populate new fields from HIR structure.

### AQ.1.5 Add `RegexClass::canonical_pattern(&self)` helper
Returns the canonical regex string for a parameterized variant. Eliminates the need for IR types like `key_class_regex_pattern()` to hardcode pattern strings.

### AQ.1.6 Migrate all consumers
Every `match RegexClass::JsonString` becomes `match RegexClass::QuotedString { allows_u_escapes: true, .. }` etc.

Sites (~60):
- `core/src/lower/expression.rs:1342`
- `core/src/generate/regex/emit/mod.rs` (5 match sites)
- `core/src/generate/regex/emit/scanner_plan.rs:131`
- `core/src/backend/kernels/punct_ws_region.rs:30`
- `core/src/backend/rust/emitter/{grammar,leaves,ws}.rs`
- `ir/src/passes/recognizers/{comment_ws,identifier,quoted_string}.rs`
- `ir/src/types/recognizer_configs.rs:51` → use `canonical_pattern()`
- Tests: `crates/core/tests/regex_classify.rs` etc.

**Files:**
- MODIFY: `parse-that/rust/regex/src/classify/mod.rs`
- MODIFY: `parse-that/rust/regex/src/classify/structural.rs`
- MODIFY: `parse-that/rust/regex/src/info/mod.rs`
- MODIFY: ~20 consumer files

## Phase 2: Deoverfit Scanners and Kernels

### AQ.2.1 Rename `scan_json_number_*` → `scan_number_strict_*`
Move `JSON_NUMBER_CONFIG` → `STRICT_NUMBER_CONFIG`. The "JSON-ness" is a NumberConfig value, not a separate function family.

### AQ.2.2 Move `quoted_string_scan_full` out of `parsers/json.rs`
Move to `scan/quoted.rs`, rename → `scan_quoted_string_strict`. Move `validate_json_escapes` → `validate_strict_escapes`.

### AQ.2.3 Delete language-prefixed SpanParser wrappers
Delete from `parse-that/src/span_parser/constructors.rs:88-124`:
- `sp_json_number`, `sp_json_string`, `sp_json_string_quoted`
- `sp_css_ident`, `sp_css_ws_comment`, `sp_css_string`, `sp_css_block_comment`

Provide structural `sp_number(config)`, `sp_quoted_string(config)`, `sp_ident(config)`, `sp_ws_comment`, `sp_block_comment` factories.

### AQ.2.4 Rename `SpanScanner` enum variants
In `parse-that/src/span_parser/span_scanner.rs`:
- `JsonNumber` → `NumberStrict`
- `JsonString` / `JsonStringQuoted` → `QuotedStringStrict{Content?}`
- `CssIdent` / `CssWsComment` / `CssString` / `CssBlockComment` → generic forms

### AQ.2.5 Rename `is_css_ws` → `is_ascii_ws_no_vtab`
In `parse-that/src/parsers/scan/ws_comment.rs:28`. The name was nominal; the predicate is structural.

### AQ.2.6 Delete `scan_balanced_end` wrapper
In `parse-that/src/parsers/scan/balanced.rs:114`. Hardcodes CSS config. Callers build their own `BalancedScanConfig`.

### AQ.2.7 Parameterize `scan_ident`
Accept `IdentConfig { allow_leading_dash, allow_double_dash_prefix }` or split into variants.

### AQ.2.8 Kernel renames
- `kernels::quoted_string::emit_json_call` → `emit_call_strict`
- `kernels::number::emit_call_span` / `emit_call_fused` — add relaxed-config overloads
- `kernels::identifier::emit_call` — add variants per config
- Drop "JSON" from `punct_ws_region.rs:1` module doc

### AQ.2.9 Parameterize `STRUCTURAL_PUNCTS`
`ir/src/passes/recognizers/punct_ws_region.rs:30` hardcodes `b",:{}[]"`. Derive from `ir.structural_bytes` (same source as `compute_structural_bytes`).

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/{number,number_f64,ws_comment,ident,balanced,quoted}.rs`
- MODIFY: `parse-that/rust/parse_that/src/parsers/json.rs` (split quoted helpers out)
- MODIFY: `parse-that/rust/parse_that/src/span_parser/{constructors,span_scanner}.rs`
- MODIFY: `crates/core/src/backend/kernels/{quoted_string,number,identifier,punct_ws_region}.rs`
- MODIFY: `crates/ir/src/passes/recognizers/punct_ws_region.rs`

## Phase 3: Structural Dispatch — Complete the Feature

This is the highest-impact phase. WS scanning was 50% of citm runtime pre-AP (dropped to 12% with SIMD WS bitmap). Structural dispatch properly implemented can eliminate it entirely.

### AQ.3.1 Fuse `filter_quote_parity` into `scan_structural`

Current: `filter_quote_parity` is a scalar backwards-backslash scan (O(quotes × backslashes)). On citm this is ~4ms — 4× parse budget.

Fix: use simdjson's technique. Compute quote bitmap via SIMD. Compute `in_string` carry via XOR-prefix-sum. AND structural bitmap with `!in_string` mask. Emit only real structural positions. All in one SIMD pass.

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/structural.rs`
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`

### AQ.3.2 WS elision between structural positions

In `crates/core/src/backend/rust/emitter/ws.rs`:
- When `structural_mode && ws_pattern.is_none()` AND we're between two structural positions, emit `state.offset = state.structural_index[state.structural_cursor]` — jump directly to next structural byte.
- The comment at `ws.rs:41-45` saying "whitespace trim is still required" is wrong; between retained structural positions, `filter_quote_parity` (fused above) guarantees no in-string bytes and FIRST-set guarantees no dispatch bytes, so non-structural bytes are necessarily whitespace (or non-dispatch pattern content which is handled by inner regex scanners).

### AQ.3.3 Collapse hybrid dispatch in alt.rs

Current `alt.rs:121-150` emits both structural peek AND full byte-load fallback. Duplicate match arms. Always calls `sync_structural_cursor_to_offset`.

Fix: when `structural_mode` is active, emit pure `advance_to_structural(state)` path. No fallback. Non-structural positions don't reach Alt dispatch (between-position bytes are consumed by inner scanners).

### AQ.3.4 Checkpoint save/restore of structural_cursor

`alt.rs:195` saves `state.offset` but NOT `state.structural_cursor`. On backtrack cursor desyncs. Fix: save/restore both in checkpoint mode.

### AQ.3.5 Size gating
Pre-scan overhead still bounded. Gate on `input.len() > 4096`.

### AQ.3.6 Re-enable structural_mode
`crates/core/src/generate/mod.rs:60`:
```rust
emitter.structural_mode = ir.structural_bytes.is_some() && ir.ws_pattern.is_none();
```

**Expected impact: citm 2,712 → 3,400+ MB/s (BEAT sonic-rs). twitter 2,173 → 2,800+.**

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/structural.rs`
- DELETE: `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs`
- MODIFY: `crates/core/src/backend/rust/emitter/{alt,ws,grammar}.rs`
- MODIFY: `crates/core/src/generate/mod.rs`

## Phase 4: Real Direct-to-Struct Projection

Per AL-prototype-2's diagnosis, the original Tier B failed because of separate ABIs. The correct approach: unified `(state, tape) -> Option<TapeOffset>` ABI with leaf rules emitting `push_leaf` (no mark_children) while still passing tape through to children.

But for TRUE direct-to-struct projection, we need more than side-channel scalar payloads. We need typed view accessors returning user-defined structs and enums.

### AQ.4.1 Extend PayloadKind to U16, U32, I16, I32, I64

Simple numeric extensions. Covers CSS hex colors, larger enum discriminants, signed numerics.

### AQ.4.2 Generic aggregate payload

A rule whose body is `Seq(scalar_fields...)` should be able to store all fields in a contiguous payload region. The view accessor constructs a typed struct from the payload bytes.

This requires:
- Compile-time layout planning (`PayloadLayout` per rule)
- Parser emits multi-field payload writes
- View accessor generates `pub fn value(&self) -> Rule { Rule { field_0: payload.read_f64(0), field_1: payload.read_u8(8), ... } }`

### AQ.4.3 Alt → enum payload

A rule whose body is `Alt(branches)` where all branches are TapeSpanOnly (leaf) should enable the view to return a typed Rust enum variant directly.

The discriminant is already in `TapeRec.flags` (variant_idx). For variants with payloads, combine with the aggregate payload mechanism.

### AQ.4.4 Rename honestly

Replace "direct projection" terminology in docs and code with "typed view accessors" or "scalar/aggregate payload projection". The mechanism is enhanced tape + payload, not a separate ABI.

**Files:**
- MODIFY: `crates/core/src/backend/rust/emitter_types.rs` (PayloadKind expansion)
- MODIFY: `crates/bbnf-tape/src/{builder,tape}.rs` (push_leaf_with_{u16,u32,i16,i32,i64}, aggregate payloads)
- MODIFY: `crates/core/src/backend/rust/emitter/{grammar,map_value,tape_prelude}.rs`
- MODIFY: `crates/core/src/backend/rust/view/{leaves,alt,seq}.rs`
- NEW: `crates/ir/src/passes/payload/layout.rs` — compile-time layout planner

## Phase 5: CSS L4 Hot Path — `__compoundSelector`

AP.4 landed key dispatch for `__declaration` (+35-50%). The remaining hottest function is `__compoundSelector` (40.2% normalize, 36.6% tailwind). It does hand-rolled byte-by-byte CSS identifier parsing inline instead of calling the `scan_ident` kernel.

### AQ.5.1 Classify CSS identifier regex as `RegexClass::Identifier`
With `allows_leading_dash: true, allows_double_dash_prefix: true`. Update `try_classify_identifier` in bbnf-regex.

### AQ.5.2 Emit `scan_ident` kernel call instead of inline loop
In `core/src/generate/regex/emit/` — when emission detects `RegexClass::Identifier`, emit `::parse_that::scan_ident(state, &IDENT_CONFIG_CSS)` instead of unrolling char-class loops.

### AQ.5.3 Length-bucketed perfect hash for __declaration key dispatch
AP.4 emits a linear array-equality ladder. Replace with:
- Group by byte length
- Within each length, perfect-hash on first/last byte
- Single SIMD compare for 16-byte-aligned names

**Expected impact: CSS bootstrap 505 → 800+, normalize 978 → 1,400+.**

**Files:**
- MODIFY: `parse-that/rust/regex/src/classify/structural.rs` (expand Identifier detection)
- MODIFY: `crates/core/src/generate/regex/emit/` (emit scan_ident for Identifier variant)
- MODIFY: `crates/ir/src/passes/recognizers/key_dispatch.rs` (length-bucketed hash)
- MODIFY: `crates/core/src/backend/driver/alt.rs` (hash-based dispatch codegen)

## Phase 6: Port sonic-rs Techniques (Portable Only)

### AQ.6.1 `skip_space` bitmap caching
Add `nospace_bits: u64, nospace_start: isize` to ParserState. After first SIMD WS scan, cache non-space bitmap. Next `trim_leading_whitespace` within 64 bytes of previous scan reuses via `trailing_zeros()`.

### AQ.6.2 Pre-size output Vec with grammar-derived constant
Compile-time analysis of grammar: maximum-node-per-byte bound. Parser emits `TapeBuilder::with_capacity(input.len() * MAX_NODE_PER_BYTE)`. Removes per-container Vec::grow.

### AQ.6.3 TLS-recycled scratch for TapeBuilder
Optional codegen flag. Thread-local recycled Vec<TapeRec>. Repeat-parse scenarios (LSP, gorgeous) benefit.

### AQ.6.4 Port `simd_str2int` to NEON
sonic-rs has x86_64 SIMD digit-parse but no NEON. Implement NEON version: `vsub` by `'0'`, check range via `vmax/vmin`, bitmask via `vmovmaskq`, pairwise adds via `vmull`+`vpaddl`. Direct win on canada/data_xl.

**Expected impact: canada 1,797 → 2,100+, data_xl 1,341 → 1,700+.**

**Files:**
- MODIFY: `parse-that/rust/parse_that/src/state.rs` (bitmap cache fields)
- MODIFY: `parse-that/rust/parse_that/src/scanners.rs` (skip_space cache)
- MODIFY: `parse-that/rust/parse_that/src/parsers/scan/number.rs` (NEON simd_str2int)
- MODIFY: `crates/bbnf-tape/src/builder.rs` (TLS scratch option)
- MODIFY: `crates/core/src/backend/rust/emitter/grammar.rs` (Vec sizing)

## Phase 7: Clean Bootstrap Regen

### AQ.7.1 Resolve structural-mode codegen deficits
AE/AF/AG/AI/AM/AN/AP all deferred this. Identify the two blockers:
- Schema emitter's `cst_directives` layout check under structural mode
- Heterogeneous Alt sub-variant coercion under structural mode

### AQ.7.2 Regenerate `generated.rs` from scratch
Run `scripts/bootstrap-bbnf.sh` (or equivalent). Verify parity with hand-patched version.

### AQ.7.3 Freeze grammar_roundtrip constants
`crates/core/tests/grammar_roundtrip.rs:47-52` — replace `usize::MAX` sentinels with exact rule counts. Un-ignore tests.

### AQ.7.4 Delete legacy bridges
- Span-text fallback in `core/src/grammar/host.rs:206-423`
- Any "AE-era" compatibility shims still present

**Files:**
- REGEN: `crates/core/src/grammar/generated.rs`
- MODIFY: `crates/core/tests/grammar_roundtrip.rs`
- DELETE: `crates/core/src/grammar/host.rs` bridge logic
- MODIFY: schema emitter to handle structural mode

## Phase 8: Benchmark Validation + Instrumentation

### AQ.8.1 Full bench sweep
Single-invocation sweep for JSON (5) + CSS L4 (3) + sheets (3) + compile pipeline (5). Write to `docs/benchmarks/post-AQ.json`.

### AQ.8.2 samply diff against post-AP
Save post-AQ profiles to `docs/benchmarks/profiles/post-AQ/`. Every +X% claim cites symbol + self-time delta.

### AQ.8.3 `BBNF_EGRAPH_REPORT=1` and `BBNF_CSP_REPORT=1`
Release-build instrumentation. AN.6, AO.4.3, AP.6.4 all deferred this.

### AQ.8.4 Cost model grid sweep
Sweep `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`, `tape_push` across JSON + CSS L4. Select defaults maximizing geomean. No individual bench regresses >1%.

### AQ.8.5 Global CSP solve
Currently per-component. CSS L4 hits budget fallback. Implement `solve_grammar_global` with 10M node budget. Accept compile-time cost if parse throughput wins.

---

## Performance Targets

### JSON (target: BEAT sonic-rs on citm/twitter)
| Dataset | Current | AQ Target | sonic-rs | Goal |
|---------|---------|-----------|----------|------|
| canada | 1,797 | **2,100+** | 1,540 | **BEAT (extend)** |
| citm | 2,712 | **3,400+** | 3,000 | **BEAT** |
| twitter | 2,173 | **2,800+** | 2,643 | **BEAT** |
| data | 1,900 | 2,400+ | 2,346 | PARITY |
| data_xl | 1,341 | **1,700+** | 1,460 | BEAT |

### CSS L4 (key dispatch + scan_ident)
| Dataset | Current | AQ Target |
|---------|---------|-----------|
| normalize | 978 | **1,400+** |
| bootstrap | 505 | **800+** |
| tailwind | 534 | **800+** |

### Hard Gates
- `cargo test --workspace` passes (minus the 2 known-failing google_sheets tests)
- Structural mode activates end-to-end (`cargo expand` shows `scan_structural` calls)
- WS self-time < 5% on citm (from 12% post-AP)
- Zero `JsonString`/`JsonNumber`/`CssIdent`/`CssQuotedString` in `RegexClass`
- Zero `scan_json_*` function names in parse-that
- Zero `sp_json_*`/`sp_css_*` SpanParser constructors
- `crates/core/src/grammar/generated.rs` reproducible from bootstrap script
- `grammar_roundtrip` tests un-ignored and passing
- `docs/benchmarks/post-AQ.json` exists

### Soft Gates
- JSON citm ≥ 3,400 MB/s (BEAT sonic-rs)
- JSON twitter ≥ 2,800 MB/s (BEAT sonic-rs)
- CSS L4 bootstrap ≥ 800 MB/s
- NEON `simd_str2int` shipped
- `skip_space` bitmap cache active
- Global CSP solve shipped OR documented as unnecessary

---

## Execution Waves (6 agents per wave)

### Wave 1: Foundation (AQ.0 + AQ.1)
- Agent A: IR inspect module (AQ.0)
- Agent B: RegexClass variant expansion (AQ.1.1-1.5)
- Agent C: RegexClass consumer migration (AQ.1.6)
- Agent D: Scanner renames (AQ.2.1-2.2, parse-that side)
- Agent E: SpanParser constructor renames (AQ.2.3-2.5)
- Agent F: Kernel renames + STRUCTURAL_PUNCTS parameterization (AQ.2.8-2.9)

### Wave 2: Structural Dispatch Activation (AQ.3)
- Agent A: Fuse filter_quote_parity into scan_structural (AQ.3.1)
- Agent B: WS elision between structural positions (AQ.3.2)
- Agent C: Collapse hybrid dispatch + checkpoint save (AQ.3.3-3.4)
- Agent D: Size gating + re-enable structural_mode (AQ.3.5-3.6)
- Agent E: CSS L4 identifier regex classification (AQ.5.1-5.2)
- Agent F: Length-bucketed key dispatch hash (AQ.5.3)

### Wave 3: Direct-to-Struct + sonic-rs techniques (AQ.4 + AQ.6)
- Agent A: PayloadKind expansion (U16/U32/I16/I32/I64) (AQ.4.1)
- Agent B: Aggregate payload layout planner (AQ.4.2)
- Agent C: Alt→enum payload (AQ.4.3)
- Agent D: skip_space bitmap caching (AQ.6.1)
- Agent E: NEON simd_str2int (AQ.6.4)
- Agent F: Vec pre-sizing + TLS scratch (AQ.6.2-6.3)

### Wave 4: Bootstrap + Instrumentation + Validation (AQ.7 + AQ.8)
- Agent A: Clean bootstrap regen (AQ.7)
- Agent B: Release instrumentation (AQ.8.3)
- Agent C: Cost model grid sweep (AQ.8.4)
- Agent D: Global CSP solve (AQ.8.5)
- Agent E: Full bench sweep + samply diff (AQ.8.1-8.2)
- Agent F: post-AQ.json + final report

---

## Critical Files

| File | Phase |
|------|-------|
| `parse-that/rust/regex/src/classify/mod.rs` | 1 |
| `parse-that/rust/regex/src/classify/structural.rs` | 1, 5 |
| `parse-that/rust/parse_that/src/parsers/scan/structural.rs` | 3 |
| `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs` | 3 (DELETE) |
| `parse-that/rust/parse_that/src/parsers/scan/{number,number_f64,ident,ws_comment,balanced}.rs` | 2 |
| `parse-that/rust/parse_that/src/parsers/json.rs` | 2 (split) |
| `parse-that/rust/parse_that/src/span_parser/{constructors,span_scanner}.rs` | 2 |
| `parse-that/rust/parse_that/src/state.rs` | 3, 6 |
| `parse-that/rust/parse_that/src/scanners.rs` | 6 |
| `crates/ir/src/passes/inspect/` | 0 (NEW) |
| `crates/ir/src/passes/recognizers/*.rs` | 0 |
| `crates/ir/src/passes/recognizers/punct_ws_region.rs` | 2 |
| `crates/ir/src/types/recognizer_configs.rs` | 1 |
| `crates/core/src/backend/kernels/*.rs` | 2 |
| `crates/core/src/backend/rust/emitter/{alt,ws,grammar,leaves,map_value,tape_prelude}.rs` | 3, 4 |
| `crates/core/src/backend/rust/view/{leaves,alt,seq}.rs` | 4 |
| `crates/core/src/backend/rust/emitter_types.rs` | 4 |
| `crates/bbnf-tape/src/{builder,tape}.rs` | 4 |
| `crates/core/src/generate/mod.rs` | 3 |
| `crates/core/src/generate/regex/emit/*.rs` | 1, 5 |
| `crates/core/src/grammar/generated.rs` | 7 (REGEN) |
| `crates/core/tests/grammar_roundtrip.rs` | 7 |
| `docs/benchmarks/post-AQ.json` | 8 (NEW) |
| `docs/benchmarks/profiles/post-AQ/` | 8 (NEW) |
