# LOCK-9 · pass-3 · PROTO-1 — Short Brief 1 (9A vs 9B): author the canonical DENSE-64 producer + purge the double-`vaddv`, on the real substrate

**Probe 1, family 9A-vs-9B.** The dispatched string was the placeholder `"Short brief one"`; the probe
title maps to FAMILY-SPECS **BRIEF 1** — "the evolved-home axis re-measurement." Brief 1 has two
PASS/FAIL halves: (A) the **seam** — a cross-crate `#[inline]` producer inlines free at fat-LTO cgu=1;
(B) the **evolved-home construction** — *author the two canonical producers into a shared home AND purge
the four double-`vaddv` sites*, then measure residual divergence. Half (A) was CONFIRMED in machine code
by the prior probe (`PROTO-1-consumer.disasm`: `scan_inline_seam` = 0 in-loop indirect; OnceLock =
1 `blr`). It is not re-litigated. **This probe runs half (B)** — the part explicitly marked NOT-run —
on the **real bbnf-simd substrate** in scratch, plus the LOCK-6 divergence table neither pass produced.

Parse-that and the canonical bbnf-simd were **not mutated** (READ-ONLY, P23): all work is in a scratch
copy of `skinny/crates/bbnf-simd`. Cold paired CSS timing (Brief 1's other measurement) was **not run** —
loadavg sat 5–9 the whole window (recorded below); a certified ≥0.98× ratio needs a quiet machine, and
the axis is a *perf wash by law* (§reconciliation 1), so the honest datum here is the maintenance-cost
divergence, not a contended ratio. Scope stated plainly at the end.

## Question

Reconciliation 3 asserts the two lawful movemask producers (LEAN nibble-`vshrn`, DENSE-64 `vpaddq`
tree) are **absent from bbnf-simd** and must be authored **once into any home regardless of ownership** —
so adopting 9A is *not* "inherit good kernels for free," it is "author the canonical producers AND purge
the four refuted double-`vaddv` sites first." Two questions, both zero-measured for two passes:
1. Can the canonical DENSE-64 `vpaddq` producer be authored and shown **bit-identical** to the refuted
   form yet **free of horizontal reduction**, and does the purge leave the real crate **building green**
   with the gate `rg 'vaddv_u8|vaddvq_u8'` clean?
2. What is the **residual LOC/substrate divergence** between the one-home (9A) and two-home (9B) layouts
   once the producers are authored — the actual LOCK-6 maintenance-cost input?

## Method

**(1) Self-contained producer probe** (`PROTO-1-dense64-probe.rs`, no deps, `lto="fat" cgu=1`): three
producers side by side — the refuted `pack64_double_vaddv` (verbatim from `structural_classify_64.rs`),
the canonical `pack64_dense_vpaddq` (simdjson `neon_movemask_bulk`), a byte-order-preserving single-stripe
`movemask_u8x16_vpaddq` (3-fold `vpaddq`), and `firstset_lean_vshrn`. Differential oracle: each vs a
scalar reference over **N=200,000** `black_box` xorshift blocks (64 lanes, each 0x00/0xFF); INJECT_BUG
flips one lane. `otool -tvV` per `#[no_mangle]` frame → reduction-op census.

**(2) Real-substrate purge** (scratch copy of `skinny/crates/bbnf-simd`, isolated `CARGO_TARGET_DIR`,
standalone `Cargo.toml`, `blake3` from crates.io): apply the verified producers to **all four**
double-`vaddv` sites — `structural_classify_64` (4-stripe → `pack4` routed through the DENSE-64 tree),
`byte_class_from_eq_set_64` / `comment_body_mask_64` / `bracket_depth_mask_64` (single-stripe → the 3-fold
`vpaddq` drop-in). `cargo build --release --lib`; then the gate census. Diff → `PROTO-1-vaddv-purge.patch`.

Env: `aarch64-apple-darwin`, arm64, rustc 1.96.0-nightly (02c7f9bec 2026-04-10), 18 cores. **No x86.**
Substrate live this session: 4 double-`vaddv` sites (8 lines); `vpaddq` in **zero** files (reconciliation
3 confirmed by grep); `lo6_table_admissible` at `bbnf-simd/src/dispatch.rs:101`.

## What ran

Both built and executed. Probe: clean build 2.4s, parity run exit 0. Scratch bbnf-simd: clean lib build
(3.08s cold incl. blake3; 0.69s after the 3-site patch), gate census clean. loadavg recorded per step
(3.3 → 8.9 across the window — **contended, no timing claimed**).

## The numbers

**Producer reduction-op census** (`otool`, `PROTO-1-producers.disasm`):

| producer | insns | `addv` (horiz. reduce) | `uzp`+`orr` | `shrn` |
|---|---|---|---|---|
| `pack64_double_vaddv` (REFUTED) | 39 | **8** | 0 | 0 |
| `pack64_dense_vpaddq` (CANONICAL DENSE-64) | 22 | **0** | 7 | 0 |
| `movemask_u8x16_vpaddq` (single-stripe drop-in) | — | **0** | via `addp` fold | 0 |
| `firstset_lean_vshrn` (LEAN) | 7 | 0 | 0 | **1** |

LLVM lowered the DENSE-64 `vpaddq` disjoint-bit tree to **7 `uzp1/uzp2`+`orr.16b`, zero horizontal
reduction** — exactly the "uzp/orr, zero reduction ops" the FAMILY-SPECS §DENSE predicts. The refuted
form carries **8 `addv.8b`** (2 per stripe × 4) plus `bfi`/`ext`/`fmov` spills.

**Differential parity oracle (N=200,000):**

```
dense_vpaddq          mismatches vs scalar = 0
double_vaddv          mismatches vs scalar = 0     (refuted form is CORRECT — purge is perf-only)
single_stripe_vpaddq  mismatches vs scalar = 0
INJECT_BUG (flip lane5) bites dense = true
LEAN vshrn first-set of lane5 = 5 (want 5)
PARITY_PASS = true
```

The canonical producer is **bit-identical** to the refuted one (both match scalar over 200k), so the
purge preserves checkasm parity by construction; the oracle has teeth (INJECT_BUG bites).

**Real-substrate purge — GATE GREEN:**

```
$ rg -n 'vaddv_u8|vaddvq_u8' scratch/bbnf-simd/src/aarch64
  (none — zero vaddv reductions remain in aarch64/)
$ rg -c 'vpaddq_u8' src/aarch64/*.rs   # the evolved home now carries the canonical producer
  bracket_depth_mask_64.rs:3   byte_class_from_eq_set_64.rs:3
  comment_body_mask_64.rs:3    structural_classify_64.rs:4
$ cargo build --release --lib   →  Finished in 0.69s   (BUILD GREEN, all 4 sites purged)
```

Diff: **4 files, +38/−25** (`PROTO-1-vaddv-purge.patch`). The DENSE-64 producer is ~15 LOC authored
**once** and reused across all four sites — the "author once regardless of home" cost made concrete.

**The evolved-home divergence table (the LOCK-6 datum — static census of the live trees):**

| axis | 9B parse-that (own kernels) | 9A bbnf-simd (shared home) | drift / maintenance basis |
|---|---|---|---|
| SIMD dialect | `portable_simd` `u8x16`/`to_bitmask` — **NIGHTLY** | `core::arch::aarch64` intrinsics — **STABLE** | a shared home forces ONE dialect; bbnf-simd is already stable, 9B's off-nightly headline is UNVERIFIED (§9B Q3) |
| LEAN movemask | `to_bitmask().trailing_zeros()` (compiler-lowered; 3 sites) | hand `vshrn` (`movemask.rs`, currently the refuted scalar-repack) | both must reach canonical LEAN; portable_simd already lowers right, bbnf-simd's hand form needs authoring |
| DENSE movemask | `to_bitmask() as u64` per-16, shift-combined (6 sites) | 4× local **double-`vaddv`** (refuted) | **NEITHER** had `vpaddq` → authored once here (recon 3, now built) |
| padding | eager `PaddedView`, `INPUT_PAD_BYTES=64` whole-input | `eob_pad_clamp` (6-line scalar clamp) | divergent invariant; a shared home must pick one (eager-pad vs clamp) |
| table shuffle | **none** (no `vqtbl` in scan/) | `vqtbl4q_u8` lo6 4-table (`classify_tbl4`) | bbnf-simd is a superset; shared home = 9B *gains* the table classifier |
| block granularity | per-16 `u8x16`, shift-combine to u64 | native 64-byte (`*_64`, 4-stripe) | convention gap in the pack path |
| x86 | live SSE4.2 in **9** scan files (dead-by-law §0) | none (aarch64-only) | 9A is already x86-clean; 9B carries dead x86 to purge either way |
| correctness oracle | absent (tests assert "parsed w/o panic") | full checkasm scalar-parity (16 `checkasm_*`) | 9A owns the oracle 9B lacks — a maintenance asset only in the shared home |
| LOC (scan surface) | ~2956 (`scan/` only) + `scanners`/`span_parser` | ~1351 (`aarch64/`) + scalar refs | one-home authors the producer **once** (~+15 LOC); two-home duplicates it |

**Reading (LOCK-6's input, not LOCK-9's verdict):** the divergence is **real and structural** — dialect
(nightly vs stable), padding (eager vs clamp), granularity, and the oracle differ. This is *not* two
near-identical kernels; a shared home forces a genuine convergence (one dialect, one padding, one pack
path) but also *delivers* the stable toolchain + `vqtbl4q` + checkasm oracle that 9B lacks. The perf axis
washes (producers author-once, bit-identical, seam-free per prior probe); the decision is which side pays
the convergence cost — precisely the "null hypothesis defeated by measured drift/maintenance cost" that
LOCK-6 renders, now furnished with data.

## Verdict

**RAN — MEASURED.** Brief 1 half (B) is delivered on the real substrate: the canonical DENSE-64 `vpaddq`
producer is **authored, bit-identical to the refuted form (0/200k), and reduction-free (0 `addv` → 7
uzp/orr)**; all **four** double-`vaddv` sites are **purged** in a scratch bbnf-simd that **builds green**
with the gate `rg vaddv` **clean** and `vpaddq` now present in all four (the evolved home constructed).
The LOC/substrate divergence table is emitted as data — the LOCK-6 maintenance-cost datum neither pass
produced. Reconciliation 3 is confirmed by construction: neither home had `vpaddq`; it costs ~15 LOC
authored once.

**Scope honesty (what did NOT run):** (i) the cold paired divan `parse_that_css` N≥200 two-windows ratio
— loadavg 5–9 all window; a certified ≥0.98× needs a quiet machine and the axis is a perf wash by law, so
no ratio is claimed (SPEC_ONLY for that sub-gate, not measured). (ii) the checkasm harness was not
executed against the patched crate (needs `test-fixtures`, not standalone-buildable in cap) — parity is
instead proven by the standalone differential oracle (200k + INJECT_BUG), which is the same bit-parity
contract. (iii) the full scratch parse-that build + the OnceLock de-face of the 5 `prim::` kernels + the
one-line `[patch]` seam rewire — the seam mechanism they rest on is the prior probe's CONFIRMED result
(`PROTO-1-consumer.disasm`), not re-run here.

**VERDICT LINE:** canonical DENSE-64 `vpaddq` producer authored → **bit-identical to double-`vaddv`
(0/200k) but 0 horizontal reductions (7 uzp/orr vs 8 `addv`)**; all four refuted sites purged, scratch
bbnf-simd **builds green**, gate `rg vaddv` **clean**, `vpaddq` present in all four (evolved home built);
divergence table emitted (nightly↔stable, PaddedView↔clamp, per-16↔64B, oracle-present↔absent) as LOCK-6's
maintenance-cost input. Half (B) PASS; the perf-half ratio is SPEC_ONLY under load (axis is a wash by law).
Artifacts: `PROTO-1-vaddv-purge.patch`, `PROTO-1-dense64-probe.rs`, `PROTO-1-producers.disasm` (this dir).
