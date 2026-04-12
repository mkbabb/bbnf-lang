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

## Post-AM targets (MB/s, cold)

| File      | Pre-AM | Target | sonic-rs |
|-----------|--------|--------|----------|
| canada    | 1,453  | 1,550+ | 1,503    |
| citm      | 2,001  | 3,100+ | 3,045    |
| twitter   | 1,672  | 2,750+ | 2,693    |
| data      | 1,502  | 2,450+ | 2,401    |
| data_xl   | 1,121  | 1,500+ | 1,477    |
