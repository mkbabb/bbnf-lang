# Tranche AL -- Maximal -O3 Optimization Pass

## Context

Post-AK, BBNF achieves parity with sonic-rs on numeric workloads
(canada: -2%) and beats simd-json on every dataset (+9% to +94%).
The remaining 23-35% gap to sonic-rs is concentrated on
**string-heavy workloads** (citm -34%, twitter -34%, data -35%,
data_xl -23%). The gap is attributable to three techniques:

1. **Structural bitmap pre-scan** — SIMD pre-pass builds compressed
   index of structural byte positions, eliminating per-byte dispatch
   and implicit whitespace skip
2. **SIMD string interior scanning** — branchless escape-parity
   via carry-less multiply processes 64 bytes/cycle vs memchr2's
   find-one-at-a-time loop
3. **Implicit whitespace elimination** — bitmap subsumes all `?w`
   call sites (a no-op under bitmap dispatch)

All four optimization systems (grammar e-graph, regex HIR e-graph,
CSP solver, pattern matching) are verified wired and activating.
Cost model is unified at `ir/src/cost_config.rs`. This tranche
targets the parse-time gap, not the optimizer infrastructure.

### Post-AK baseline

| File    | BBNF MB/s | sonic-rs | Gap    |
|---------|-----------|----------|--------|
| citm    | 2,008     | 3,031    | -34%   |
| canada  | 1,467     | 1,499    | -2%    |
| twitter | 1,661     | 2,522    | -34%   |
| data    | 1,491     | 2,293    | -35%   |
| data_xl | 1,117     | 1,445    | -23%   |

### Relationship to AC

AC (docs/tranches/AA-prototype-3.md) was written pre-tape-first.
Post-AK reality supersedes several phases:

- **Superseded**: AC.7 TaggedUnion (BoxedEnum runtime cost is zero
  under tape-first), AC.9 direct-to-slab, AC.15 incremental cache,
  AC.16 profile-guided infra
- **Already landed**: AC.1 TypeDescId, AC.3 unified CostConfig
- **Absorbed into AL**: AC.0 observability (AL.1), AC.11 structural
  bitmap (AL.3)
- **Deferred**: AC.2 Analysis<N> (not needed for sonic-rs gap),
  AC.8 IIFE->labeled blocks (CSS, not JSON), AC.10 ClassMask SIMD
  (CSS), AC.12 perfect-hash dispatch (CSS), AC.13 RefMode CSP (AL.6)

---

## Phase DAG

```
  AL.0 Dead Code Purge ──────┐
         │                    │
  AL.1 Profiling Baseline ◄──┘
         │
  AL.2 Substrate Micro-opts ◄── AL.0
         │
    ┌────┴────┐
    ▼         ▼
  AL.3      AL.4          (parallel: bitmap + SIMD strings)
  Bitmap    SIMD Strings
    │         │
    └────┬────┘
         ▼
  AL.5 Cost Calibration
         │
  AL.6 CSP Unification
         │
  AL.7 Verification
```

---

## AL.0 — Dead Code Purge

Remove identified legacy code/workarounds. Clean substrate for
subsequent phases.

### AL.0.1 — BoxedEnum driver fallback

Under tape-first, `ValuePlacement::Alloc` for `BoxedEnum` in the
Rust backend is dead code. The Rust emitter never allocates per-Alt
value — all values are tape records.

**Files:**
- `crates/core/src/backend/types/decisions.rs` — audit
  `ValuePlacement::Alloc` arm for BoxedEnum
- `crates/core/src/backend/driver/mod.rs:239-251` — remove
  `BoxedEnum → Enum` fallback conversion
- `crates/core/src/backend/driver/repeat.rs:86,141` — remove
  `RepeatElemKind::BoxedEnum` conversions
- Verify with `cargo expand` on json bench that zero slab alloc
  calls exist in generated code

### AL.0.2 — Remove `collapse_simple_spans`

Superseded by `MaterializationClass::TapeSpanOnly`. The boolean
controlled slab-era Seq-to-Span collapse. Under tape-first, the
materialization classifier makes this decision.

**Files:**
- `crates/ir/src/types/grammar.rs` — delete field
- `crates/ir/src/passes/recognizers/node_facts.rs` — remove reads
- `crates/ir/src/passes/types/constraint/seq.rs` — remove reads
- `crates/ir/src/passes/types/generate.rs` — remove reads
- `crates/core/src/backend/driver/analysis.rs` — remove setter
- `crates/core/src/lower/mod.rs` — remove initialization
- All test fixtures that set it — update

### AL.0.3 — Gate `compute_sp_method_rules` behind target

The pass produces `has_sp_method` consumed only by TS/WASM
backends. The Rust tape-first backend uses materialization instead.

**Files:**
- `crates/core/src/pipeline/compile.rs` — wrap call in
  `if matches!(target, Ts | Wasm)`
- `crates/ir/src/passes/span.rs:168` — no change to pass itself

### AL.0.4 — String interner double allocation

`StringInterner::intern` allocates twice: once for the map key,
once for the vec entry.

**File:** `crates/core/src/lower/string_interner.rs`
**Fix:** Single `owned = s.to_string()`, clone for map insertion.

**Gate:** All tests pass. `cargo clippy -D warnings` clean.

---

## AL.1 — Profiling Baseline

Capture the post-AK reference for every subsequent phase.

### AL.1.1 — samply profile capture

Run `samply record` on all five JSON benchmarks under release.
Store as `docs/benchmarks/profiles/post-AK/`. Identify top-10
symbols per benchmark.

### AL.1.2 — Baseline verification

Reproduce the AK numbers within +/-3%. Record as
`docs/benchmarks/post-AK.json`.

**Gate:** Profiles captured. Numbers reproducible.

---

## AL.2 — Substrate Micro-optimizations

### AL.2.1 — Eliminate double IrNode cloning

`inline_acyclic` and `fuse_single_use` clone bodies into a Vec,
then clone again into the lookup table.

**Files:**
- `crates/ir/src/passes/transform/inline.rs:25-48`
- `crates/ir/src/passes/transform/fuse.rs:43-65`

**Fix:** Build lookup table directly. Use `for (id, body) in vec`
(by-value destructure) instead of `for (id, body) in &vec` + clone.

### AL.2.2 — Borrow config maps instead of cloning

`install_pattern_caches` clones three HashMaps from IR into
DriverState. Change DriverState to borrow from IR via lifetime.

**Files:**
- `crates/core/src/backend/driver/mod.rs` — add `'ir` lifetime
- `crates/core/src/backend/patterns/cache.rs` — change owned → ref
- `crates/core/src/pipeline/compile.rs:226-236` — pass borrows

### AL.2.3 — Normalizer dirty tracking

Add per-rule dirty bitmap to the structural normalizer loop.
Each pass returns which rules changed. Next iteration only
processes dirty rules + their callers.

**Files:**
- `crates/core/src/pipeline/compile.rs:466-488`
- Each pass in `crates/ir/src/passes/transform/` — return
  `Changed<BitVec>` or equivalent

**Gate:** All tests. Compile_css_l4 bench neutral or improved.
Zero parse-time regression.

---

## AL.3 — Structural Bitmap Pre-scan

The single highest-leverage parse-time optimization. Grammar-
agnostic when parameterized by structural byte set.

### AL.3.1 — Structural byte set miner

Compute the set of structural bytes from the grammar's entry rule:
dispatch-eligible Alt first bytes + literal delimiters. If the
union fits a SIMD classification strategy, the grammar qualifies.

For JSON: `{ } [ ] : , "` = 7 bytes.
For CSS: `{ } ; : , " '` = 7 bytes.

**Files (new):**
- `crates/ir/src/passes/recognizers/structural_bitmap.rs`
**Files (modified):**
- `crates/ir/src/passes/recognizers/mod.rs`

### AL.3.2 — SIMD structural scanner

Runtime scanner in parse-that. Takes `&[u8]` + structural byte
set, produces flat `Vec<u32>` of structural positions.

- **aarch64 NEON**: `vceqq_u8` per class, accumulate, compress
- **x86_64 SSE2/AVX2**: `_mm256_cmpeq_epi8` + movemask + pdep
- **Scalar**: plain byte loop

**Files (new):**
- `parse-that/rust/parse_that/src/parsers/scan/structural.rs`

### AL.3.3 — Generated code: bitmap-dispatch mode

Under bitmap dispatch, the generated `__value` function pops the
next structural position from the index and dispatches on the
structural byte, NOT on the raw input byte. `?w` becomes a no-op
(the cursor already points past whitespace).

**Files (new):**
- `crates/core/src/backend/kernels/structural_bitmap.rs`
**Files (modified):**
- `crates/core/src/backend/rust/emitter/dispatch.rs`
- `crates/core/src/backend/rust/emitter/ws.rs` — no-op under bitmap
- `crates/ir/src/passes/csp_strategy/mod.rs` — BitmapDispatch mode

### AL.3.4 — CSP integration

Add cost knobs for bitmap construction amortization. The solver
prefers bitmap when `construction_cost < per_site_savings * sites`.

**Gate:**
- JSON citm +10%, twitter +10%, canada +5% minimum
- samply: `__value` self-time drops 30%+
- Full test suite passes

---

## AL.4 — SIMD String Interior Scanning

Close the remaining string-heavy gap. Runs in parallel with AL.3
since it modifies only parse-that runtime, not codegen.

### AL.4.1 — Branchless escape-parity scanner

The simdjson/sonic-rs technique:
1. Load 32-64 bytes, compare against `"` and `\` → bitmasks
2. Carry-less multiply on `\` mask → escape parity
3. AND quote mask with even-parity mask → true quotes
4. First set bit = closing quote

- **x86_64**: `_mm256_cmpeq_epi8`, `_mm_clmulepi64_si128`,
  `_tzcnt_u64`
- **aarch64**: `vceqq_u8`, `vmull_p64`, `__rbit` + `__clz`
- **Scalar**: existing memchr2 loop (fallback, already works)

**Files (new):**
- `parse-that/rust/parse_that/src/parsers/scan/quoted_simd.rs`
**Files (modified):**
- `parse-that/rust/parse_that/src/parsers/scan/quoted.rs` —
  dispatch to SIMD when available

### AL.4.2 — Escape-free fast path

After SIMD scan, if backslash mask was all-zero, the string is
escape-free — return span immediately without validation. This is
the common case for JSON keys.

**Gate:**
- twitter +8%, data +8%, citm +5% minimum
- canada neutral (no strings)
- samply: memchr2 exits top-10

---

## AL.5 — Cost Model Empirical Calibration

### AL.5.1 — Sweep harness

Script that iterates over a grid of cost weight combinations via
`BBNF_COST_*` env vars, compiles + benches, records tuples.

Weights to sweep:
- `dispatch_bonus` (range: -4.0 to -0.5)
- `call_overhead` (range: 2.0 to 8.0)
- `inline_body_size_penalty` (range: 0.1 to 2.0)
- `emission_tier_bonus` (range: -3.0 to -0.5)

### AL.5.2 — Calibrate defaults

Select weight config maximizing geometric mean across JSON +
CSS tailwind. Update `CostWeights::default()`.

**Gate:** Geo-mean +2% or more. No individual bench regresses >1%.

---

## AL.6 — CSP Unification

Merge the backend-local inline CSP into the IR-level strategy CSP
for globally optimal inline + strategy decisions.

### AL.6.1 — Lift RefMode into strategy CSP

Add `RefMode { DirectCall, InlineBody }` variable class to
`csp_strategy/mod.rs`. One variable per `IrNode::Ref(id)` in DAG.

**Files:**
- `crates/ir/src/passes/csp_strategy/mod.rs`

### AL.6.2 — Cross-variable constraints

ImplicationConstraints between RefMode and AltMode:
- Inlined body FIRST set replaces rule FIRST set at call site
- Inline + overlapping FIRST → ByteDispatch infeasible
- Cyclic rules → forced DirectCall

### AL.6.3 — Delete backend-local inline CSP

Replace `crates/core/src/backend/rust/analysis/inline.rs` with
reads from `ir.recognizer_decisions`.

**Gate:** All tests. Parse perf neutral or improved. Single CSP
solve replaces two independent solves.

---

## AL.7 — Verification

### AL.7.1 — Full benchmark + profile capture

Run all JSON + CSS benchmarks. Capture samply profiles. Record
as `docs/benchmarks/post-AL.json`.

### AL.7.2 — Target table

| File    | Post-AK | AL Target | sonic-rs | Gap   |
|---------|---------|-----------|----------|-------|
| citm    | 2,008   | 2,400+    | 3,031    | <-22% |
| canada  | 1,467   | 1,550+    | 1,499    | >+3%  |
| twitter | 1,661   | 2,000+    | 2,522    | <-22% |
| data    | 1,491   | 1,800+    | 2,293    | <-22% |
| data_xl | 1,117   | 1,350+    | 1,445    | <-7%  |

Conservative: +15-25% on string-heavy, closing to 7-22% gap.
Aggressive: SIMD bitmap + string scanner → 10-15% gap.

### AL.7.3 — Documentation

Write `docs/tranches/AL.md` with per-phase results, profile
attributions, updated competitor table.

---

## Execution Notes

- Commit after each sub-phase (AL.0.1, AL.0.2, ...) — not per phase
- samply profiles required before declaring any phase gate met
- AL.3 and AL.4 are the critical path; AL.0/AL.1/AL.2 are warmup
- AL.5 and AL.6 are polish; the big wins come from AL.3+AL.4
- Use `BBNF_PIPELINE_REPORT=1` to verify no compile-time regression
  exceeds 15% on any single pass

## Critical Files

| Purpose | Path |
|---------|------|
| Pipeline | `crates/core/src/pipeline/compile.rs` |
| Cost config | `crates/ir/src/cost_config.rs` |
| Cost weights | `crates/egraph/src/cost_weights.rs` |
| Grammar e-graph | `crates/ir/src/egraph/` |
| CSP strategy | `crates/ir/src/passes/csp_strategy/mod.rs` |
| Inline CSP | `crates/core/src/backend/rust/analysis/inline.rs` |
| Recognizers | `crates/ir/src/passes/recognizers/` |
| Type desc | `crates/ir/src/types/type_desc.rs` |
| String interner | `crates/core/src/lower/string_interner.rs` |
| Driver | `crates/core/src/backend/driver/` |
| Regex emit | `crates/core/src/generate/regex/emit/` |
| String scanner | `parse-that/rust/parse_that/src/parsers/scan/quoted.rs` |
| Structural scan | `parse-that/rust/parse_that/src/parsers/scan/` (new) |
| Emitter dispatch | `crates/core/src/backend/rust/emitter/dispatch.rs` |
| Emitter ws | `crates/core/src/backend/rust/emitter/ws.rs` |
