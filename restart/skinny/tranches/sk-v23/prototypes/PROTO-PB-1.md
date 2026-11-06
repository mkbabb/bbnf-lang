# PROTO-PB-1 — NEON shufti/truffle prefilter on the DFA search loop

Analysis instrument (never landed). Target: parse-that `rust/regex`. Date 2026-07-10.
Question: does wiring the orphaned `detect_accel` table (plus a truffle tier) into the
DFA find loop yield >=2x cold on skip-heavy inputs with full match-position parity on real
grammar patterns?

**Verdict: GO (with a mandatory gate).** Skip-heavy speedups are 18-67x (>=2x met by an
order of magnitude), match-position parity is exact (0 mismatches, exhaustive per-offset),
the truffle tier activates on wide exit sets the old ≤8 shufti path abandoned, and the one
failing sub-criterion — dense-match regression — is fully recovered by a fused warm-up
trigger. Absolutes are directional (host loadavg 7-14 throughout); ratios are the certified
quantity per the profiling standing law.

## Setup (non-clobber honoured)

Copied only the compile→`find_at` subtree of `parse-that/rust/regex` to scratch
(`hir/ automata/ sets/ unicode.rs utf8.rs`); egraph/classify/info/first/algebra/path_lexer
are off the search path and were dropped. The vendored crate builds with `smallvec` alone
and uses the **real** HIR parser + NFA→DFA + `detect_accel` — nothing about the automaton
was mocked. parse-that's working tree was never touched.

Instrument at `…/scratchpad/pb1/`: `src/automata/accel.rs` (+truffle),
`src/automata/search_accel.rs` (NEON kernels), `src/automata/dfa.rs`
(`find_at_accel`, `find_at_accel_gated`), `src/bin/pb1_bench.rs`.

## What was wired

`detect_accel` was already computed but consumed by no find loop (orphaned since the ScalarLut
deletion). Two consumers added to `dfa.rs`:

- `find_at_accel` — ungated: in any accelerable self-loop state, `scan_to_exit` skips the whole
  run to the next exit byte.
- `find_at_accel_gated` — fused trigger: the scalar table-walk runs unchanged but counts
  consecutive self-transitions; only after `GATE=16` self-loop bytes in an accelerable state
  does it hand the remainder to the SIMD scanner.

Truffle tier (`accel.rs`), extending the enum past the old `_ => None` at >8 exit bytes:

```rust
Truffle { hi_clear: [u8;16], hi_set: [u8;16] },   // 9..=256 exit bytes
// build: for v in exit set { h=v>>4; l=v&0xF;
//   if h<8 { hi_clear[l]|=1<<h } else { hi_set[l]|=1<<(h-8) } }   // exact 256-bit membership
```

NEON kernels (`search_accel.rs`, aarch64, `vshrn_n_u16(_,4)` first-match idiom — the blessed
vshrn family). Memchr1/2/3 → `vceqq`+`vorrq`; NibbleLut → shufti `vqtbl1q`×2 + `vtstq`;
Truffle → `vqtbl1q(clear)`,`vqtbl1q(set)`, `vbslq` on `hi>=8`, bit = `vqtbl1q(pow2,hi)`,
`vtstq`. A scalar `is_exit_byte` oracle backs every kernel; scalar tail for <16 bytes.

## Parity (correctness gate)

Exhaustive per-offset `find_at == find_at_accel == find_at_accel_gated` over a 256 KiB corpus
for every pattern, plus whole-document scan-equivalence over 8 MiB. **0 mismatches, eq=true,
across all 4 patterns × 3 corpora × 2 variants.** Miss record is negative, as required.

## Measurements — paired-interleaved, cold, 8 MiB (>LLC), 9 rounds, median ns/byte

Real grammar patterns; `accel-hist` = per-state strategy census. base column is ns/byte;
accel/gated columns are SPEEDUP vs base (higher = faster).

| pattern | tier | corpus | base | accel(ungated) | gated(fused) |
|---|---|---|---|---|---|
| `[^"\\]+` (JSON body) | Memchr2 | skip-4096 | 0.90 | **39.7x** | 35.7x |
| | | skip-256 | 1.78 | **38.4x** | 25.7x |
| | | dense-6 | 0.85 | 0.71-0.86x | **1.15x** |
| `[^\n]+` (line) | Memchr1 | skip-4096 | 0.98 | **45-50x** | 43.6x |
| | | skip-512 | 1.89 | **63-67x** | 43.5x |
| | | dense-5 | 0.91 | 0.74-0.90x | **1.07x** |
| `[ \t\r\n]+` (ws-skip) | **Truffle** | skip-4096 | 0.85 | **21-27x** | 18.4x |
| | | skip-512 | 1.77 | **34x** | 27.3x |
| | | dense-5 | 0.99 | 0.59-0.76x | **1.14x** |
| `[a-zA-Z0-9_]+` (ident) | **Truffle** | skip-4096 | 1.00 | **24-26x** | 21.7x |
| | | skip-512 | 1.85 | **36x** | 27.0x |
| | | dense-6 | 0.74 | 0.53-0.64x | 0.95x |

Skip-heavy: 18-67x everywhere. Truffle carries the two wide-exit-set patterns (whitespace,
identifier) that the ≤8 shufti tier could not accelerate at all — net-new coverage.

## The dense caveat and its resolution

Ungated acceleration **regresses 1.1-1.9x** on pathologically dense inputs (exit every 5-6
bytes): when mean run length << SIMD width, the per-run dispatch (LUT vector loads + the
accel-vs-scalar branch) is pure overhead over the tight table-walk. This is inherent, not a
bug. A warm-up gate that still tested membership per byte (`is_exit_byte`) did **not** fix it
— that test is heavier than a flat-table index. The **fused trigger** does: it counts
`next==state` self-transitions inside the ordinary scalar loop and only arms the scanner after
GATE, so dense inputs run the baseline loop plus one counter → **0.95-1.15x (regression
gone)**, while skip-heavy still gets 18-63x. The gate costs ~5-20% of the ungated skip-heavy
peak — a correct trade. This gate is therefore a **required design element**, not optional.

## Implications for SK-V23 (PT-T track)

- The DFA-accel prefilter is a real, large cold win on skip-heavy grammar tokens (strings,
  comments, whitespace runs, identifiers) — the common BBNF/JSON/CSS lexer shapes.
- Land order: (1) `AccelStrategy::Truffle` + `build_truffle_luts`; (2) NEON scanner module
  with the vshrn first-match idiom (aarch64-only, matching the x86-abrogation axis — no fallback
  intrinsics owed); (3) consume via the **fused-trigger** loop only (never the ungated form).
- Open tuning: GATE=16 was not swept; an adaptive give-up counter (rust/regex-style) may beat a
  fixed gate on mixed corpora. Non-blocking.
- Scope note: measured through a tokenizer-style repeated-`find_at` scan (the realistic lexer
  usage); a single anchored `find_at` accelerates exactly one run, so the win is per self-loop
  run, not per call.
