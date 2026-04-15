# Tranche AM — Tape Purity + SIMD Parity

Post-AK baseline: JSON cold parse at 1121-2001 MB/s, 3-38% behind
sonic-rs warm (1477-3045 MB/s). BBNF already beats simd-json on
every dataset (+20% to +91%). The gap is string-scanning dominated
(canada is at parity; citm/twitter/data lag 34-38%). Two
architectural liabilities: broken EmissionTier axis (~2000 LOC
dead code), residual BumpSlab in VM. The tape substrate (flat
`Vec<TapeRec>`, 16 bytes/record) is sound.

Target: **beat sonic-rs warm with BBNF cold on every dataset.**

## Pre-AM baseline

### JSON cold (MB/s)

| File      | BBNF   | sonic-rs | simd-json | serde  |
|-----------|--------|----------|-----------|--------|
| canada    | 1,453  | 1,503    | 761       | 553    |
| citm      | 2,001  | 3,045    | 1,662     | 932    |
| twitter   | 1,672  | 2,693    | 1,338     | 801    |
| data      | 1,502  | 2,401    | 1,331     | 743    |
| data_xl   | 1,121  | 1,477    | 962       | 529    |

### CSS competitors (warm, MB/s)

| File       | cssparser | lightningcss |
|------------|-----------|--------------|
| normalize  | 732       | 284          |
| bootstrap  | 476       | 134          |
| tailwind   | 446       | 99           |

CSS monolithic panicked on bootstrap.css offset 6437 (regression).

## AM.0 — Fix regressions

Three pre-existing failures block the workspace:

1. `crates/lsp/tests/analyze.rs` — stale `parse_with_state` tuple
   destructuring; function now returns `Option<ParsedGrammar>`.
2. `crates/bootstrap/src/lib.rs` — derive macro panic in
   `mapped_factor`: mapped `"->"` literal with `?w` modifier
   unresolvable as tape term child.
3. CSS parser bootstrap.css offset 6437 — parse regression.

## AM.1 — Abrogation: EmissionTier + BumpSlab

Delete EmissionTier axis (~2000 LOC). The Direct tier emits
`__rule_direct(state) -> Option<()>` (no tape param); reconciliation
monotonically widens every Direct rule to Tape; net runtime effect
is zero. 45 files reference EmissionTier.

Delete BumpSlab from VM interpreter (2 files, 5 refs). Already
gone from parse-that (zero matches). Delete `slab_alloc` from
CostWeights.

## AM.2 — Tape payload for direct projection

Repurpose `TapeRec._reserved: [u8; 2]` as `payload_idx: u16`.
Add `payloads: Vec<u8>` to `Tape`/`TapeBuilder`. Leaf rules with
typed mappings (`-> f64`, `-> true`, `-> 0u8`) store values in the
payload buffer at parse time. View layer reads payloads directly.
TapeRec stays 16 bytes.

## AM.3 — Per-branch tape surgery

Alt-bodied MustTape rules call `mark_children` unconditionally.
Reform: `branch_pushes_children(ir, node) -> bool` classifies each
Alt branch; leaf branches emit `push_leaf`, compound branches emit
`mark_children` + `push_compound`. Eliminates compound-record
overhead for ~60% of JSON `value` branches. AL.1 samply estimate:
~9% overhead on citm from this pattern.

## AM.4 — SIMD string scanner

Replace scalar `quoted_string_scan_full` with vectorized
escape-parity scanning via carry-less multiply (proven in simdjson,
sonic-rs). aarch64 NEON + x86_64 AVX2 + scalar fallback.
Escape-free fast path returns immediately when `\` mask is
all-zero. Target: +50% on string-heavy workloads.

## AM.5 — Structural bitmap pre-scan

Grammar-mined structural byte set -> SIMD pre-scan -> flat
`Vec<u32>` position index. Dispatch on structural positions
eliminates per-byte branching and subsumes `?w` whitespace calls.
Target: +15-20% via dispatch elimination.

## AM.6 — Cost model calibration

Grid sweep over CostWeights: dispatch_bonus, call_overhead,
inline_body_size_penalty, tape_push. Optimize geometric mean
across json_citm + json_twitter + css_tailwind. No individual
bench regresses >1%.

## Post-AM.3 results (per-branch tape surgery)

| File      | Pre-AM | Post-AM.3 | Delta  | sonic-rs |
|-----------|--------|-----------|--------|----------|
| canada    | 1,453  | 1,704     | +17%   | 1,503    |
| citm      | 2,001  | 2,182     | +9%    | 3,045    |
| data      | 1,491  | 1,644     | +10%   | 2,401    |
| twitter   | 1,661  | 1,733     | +4%    | 2,693    |

canada now BEATS sonic-rs by +12%. AM.3 contributed +4-16% across
all datasets by emitting `push_leaf` for leaf Alt branches instead
of unconditional `push_compound`.

## Post-AM.3 consolidated results

| File      | Pre-AM | Post-AM | Delta  | sonic-rs | vs sonic |
|-----------|--------|---------|--------|----------|----------|
| canada    | 1,453  | 1,689   | +16%   | 1,503    | +12% BEAT|
| citm      | 2,001  | 2,138   | +7%    | 3,045    | -30%     |
| data      | 1,491  | 1,613   | +8%    | 2,401    | -33%     |
| data_xl   | 1,121  | 1,153   | +3%    | 1,477    | -22%     |
| twitter   | 1,661  | 1,671   | +1%    | 2,693    | -38%     |

### CSS Monolithic (cold, MB/s)
| File       | MB/s  |
|------------|-------|
| bootstrap  | 1,754 |
| normalize  | 2,247 |
| tailwind   | 1,533 |

### Compile Pipeline (all pass)
| Grammar  | Time    |
|----------|---------|
| json     | 131 us  |
| ebnf     | 393 us  |
| css mono | 871 us  |
| sheets   | 2.6 ms  |
| bbnf     | 26.9 ms |
| css l4   | 46.2 ms |

AM.4 (SIMD escape-parity string scanner via `portable_simd`) was
implemented but shows ~neutral impact: memchr2 was already
SIMD-accelerated and the iterative run-enumeration approach does
not achieve true O(1) carry-less multiply per chunk.

## Compile pipeline (post-AM)

| Grammar  | Pre-AM  | Post-AM  | Delta  |
|----------|---------|----------|--------|
| json     | 131 us  | 122 us   | -7%    |
| bbnf     | 566 ms  | 1.58 ms  | -99.7% |
| css l4   | 11.3 s  | 9.46 ms  | -99.9% |
| sheets   | panic   | 2.07 ms  | fixed  |

## What landed

| Phase | Summary | LOC delta |
|-------|---------|-----------|
| AM.0 | Fix 4 regressions + sep hint parser + CSS char_class guard | +240 |
| AM.0+ | CSP solver soft-index + incremental bound (269x speedup) | +27 / -52 |
| AM.1 | Delete EmissionTier axis + BumpSlab residue | -2,306 |
| AM.2 | Tape payload buffer (TapeRec._reserved → payload_idx) | +240 |
| AM.3 | Per-branch push_leaf/push_compound for Alt rules | +150 |
| AM.4 | SIMD escape-parity string scanner (parse-that) | +425 |
| AM.5 | Structural bitmap scanner (parse-that, infrastructure) | +300 |

## What was investigated but not merged

- **FamilyHelper CSP routing**: Exempting FamilyHelper from engine
  propagation caused a -10% regression on citm — the inline HIR per-byte
  loops benefit from LLVM cross-function optimization that outweighs
  the SIMD function-call path.
- **Whitespace trim guard**: Near-zero impact — LLVM already inlines
  trim_leading_whitespace_mut with an identical fast-path check.

## Remaining gap analysis

The string-heavy gap to sonic-rs (22-38%) is fundamentally
architectural: sonic-rs pre-scans the entire buffer with SIMD to
build a structural bitmap, then dispatches on pre-located positions.
BBNF uses recursive descent with per-byte dispatch. The inline HIR
loops are already well-optimized by LLVM — switching to function-call
SIMD scanners actually REGRESSED performance. Closing the remaining
gap requires integrating the AM.5 structural bitmap into the codegen
(AM.5.3), which changes the parse dispatch model from per-byte to
per-structural-position.
