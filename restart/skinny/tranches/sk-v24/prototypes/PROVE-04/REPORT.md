# PROVE-04 — Pipelined structural/quote scan (F1)

**Verdict: PARTIAL** (measured; the >=1.15x primitive gate is UNMET). The 2x64B
software-pipelined scan is a real, CI-separated win over the single-block scan —
**~1.05-1.06x on the full scan, stable across two windows** — but far short of
1.15x. End-to-end is **neutral by construction** (the kernel is additive and not
wired into the live parse path). The refutation is the finding: on Apple M5, the
deep OoO core already pipelines the single-block loop, and the vshrn-movemask
back-half saturates the vector ALU ports, so multi-block staging finds almost no
classify-stage headroom.

Host: Apple M5 Max, aarch64, `--release` (fat-LTO, cgu=1, debug=true). PMU
`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` (per-process -> load-robust). Cold
per-parse, paired per-sample interleaving, N=200, iters=8/sample.

---

## What was built

A single self-contained aarch64 kernel module + a PMU microbench, both purely
additive (zero edits to any existing kernel, dispatch, runtime, parser, or
`@generated` file).

- `skinny/crates/bbnf-simd/src/aarch64/structural_scan.rs` (+217) — simdjson
  stage-1 transcribed onto the NEON stripe scanner. Per 64B block: classify the
  structural set `{ } [ ] : ,`, the delimiter `"`, and the escape `\`; resolve
  escapes (`escape_mask_64`, odd/even backslash-run parity) and in-string
  membership (`prefix_xor_64`), then emit positions of structurals **outside**
  strings. Masks are packed with the certified `vshrn_n_u16::<4>` idiom
  (`movemask_u8x16`). Three lanes share one `resolve_block` + tail, so parity is
  **structural** — the only variables are mask production and loop schedule:
  - `scan_scalar_ref` — byte-walk classify (the executable parity oracle).
  - `scan_single` — NEON classify, one 64B block per iteration (baseline).
  - `scan_pipelined` — NEON classify **staged one block ahead** of the
    carry-dependent resolve: prologue classifies block 0, then each steady-state
    iteration issues block N+1's vector loads/compares while block N's scalar
    carry math + compaction retire (two 64B blocks in flight), epilogue resolves
    the last. `classify_fold_{single,pipelined}` isolate the classify stage
    (no compaction) and fold identically (bonus classify-mask parity check).
- `skinny/crates/bbnf-simd/tests/structural_scan_bench.rs` (+211) — parity
  assertions (oracle == single == pipelined, byte-for-byte) + the paired PMU
  microbench.
- `skinny/crates/bbnf-simd/src/aarch64/mod.rs` (+2) — module registration.

Parity: **all three lanes emit byte-identical position vectors** on twitter /
citm / canada; the classify folds match. Non-string structural counts: twitter
30 318, citm 93 731, canada 223 235.

---

## Measured (certified: PMU cyc/byte, paired per-sample ratio single/pipe; >1 => pipelined faster)

**Window 1** — loadavg 15.28 / 15.12 / 12.27, N=200, iters=8:

| corpus | lane | single cyc/B | pipe cyc/B | ratio (single/pipe) |
|---|---|--:|--:|--:|
| twitter | full-scan | 1.0340 | 1.0022 | **1.0470** +/-0.0190 |
| twitter | classify   | 0.7793 | 0.7789 | 1.0087 +/-0.0123 |
| citm    | full-scan | 1.1267 | 1.0929 | **1.0484** +/-0.0140 |
| citm    | classify   | 0.7912 | 0.7913 | 1.0058 +/-0.0109 |
| canada  | full-scan | 1.3085 | 1.2270 | **1.0619** +/-0.0135 |
| canada  | classify   | 0.8232 | 0.8204 | 1.0066 +/-0.0135 |

**Window 2** — loadavg 14.33 / 14.90 / 12.27, N=200, iters=8:

| corpus | lane | single cyc/B | pipe cyc/B | ratio |
|---|---|--:|--:|--:|
| twitter | full-scan | 1.0779 | 1.0415 | **1.0489** +/-0.0242 |
| citm    | full-scan | 1.1667 | 1.1171 | **1.0638** +/-0.0198 |
| canada  | full-scan | 1.3263 | 1.2582 | **1.0620** +/-0.0169 |
| classify (all) | — | ~0.78-0.84 | ~0.78-0.83 | 1.009-1.010 |

Reading: full-scan pipelining is **1.047-1.064x**, robust across both windows,
CIs excluding both 1.0 and 1.15. Classify-only is **statistically neutral** — the
staging buys nothing in classify; the whole full-scan gain is the overlap of the
scalar carry/compaction `resolve_block(N)` with the vector `classify(N+1)`.

## Gate

- Primitive win >=1.15x: **UNMET** — measured 1.05-1.06x (full-scan), ~1.01x
  (classify). PARTIAL.
- End-to-end no regression: **MET by construction** — the diff is additive
  (+430 lines across one registration line + two new files); it touches no code
  on the JSON parse path, so `json_canon_bench` is byte-identical. (The live JSON
  path neutralizes the structural scan anyway — R1: `parse_direct` uses GrowOnly
  const-256, NEON scan is `BBNF_TAPE_CAPACITY`-gated — so no live surface exists
  to regress or to exercise the new kernel without wiring.)

## Why it stops at ~1.06x (mechanism, from the disassembly)

`otool -tV` of `scan_pipelined` steady state — the vshrn idiom is present and the
classify is ALU-bound on the movemask back-half:

```
movi.16b v0,#0x7b  v1,#0x7d  v2,#0x5b  v3,#0x5d  v4,#0x3a  v5,#0x2c   ; { } [ ] : ,
movi.16b v16,#0x22  v17,#0x5c                                        ; " \
ldr q18,[x21,x8]                       ; load 16B chunk
cmeq.16b v19,v18,v0 ... orr.16b ...    ; 6-way structural fan (6x cmeq + 5x orr)
shrn.8b  v19,v19,#0x4                   ; vshrn_n_u16::<4> mask-narrow  <- the idiom
and.8b/ushr.8b/and.8b/orr.8b + zip1.8b/ushl.4h/fmov x/orr w,w,lsr#16  ; movemask stitch
... x3 masks (struct, quote, bslash) x4 chunks/block
```

The classify runs at 0.78-0.84 cyc/B — it is **not** memory-bound; it is bound on
the per-16B `zip/ushl/fmov/orr` movemask reconstruction (3 masks x 4 chunks =
12 stitches/block). Two blocks in flight compete for the same NEON/ALU issue
ports, so classify has **no ILP headroom** — hence the neutral classify lane. The
only slack the M5 leaves on the table is the serial scalar `resolve` (escape
parity + prefix-xor + scalar compaction), which the staging successfully hides
under the next block's vector work -> the flat ~1.05-1.06x full-scan gain,
independent of structural density (twitter 4.8% ~= canada 9.9%).

## Integration cost + exact seam

Additive today; nothing wired. To productionize into the scan dispatch: replace
the block body of `scan_dispatch` (`bbnf-simd/src/lib.rs:105`, currently
`prim::byte_class_from_table_64` -> **scalar** + `compact_mask`) with
`classify_block_neon` + `resolve_block`, and adopt the staged loop — a ~10-line
swap reusing the shared `resolve_block`. But the far larger lever the disassembly
exposes is the **movemask stitch**, not the loop schedule: a cheaper narrow
(keep 4-bits/byte and feed compaction directly, or the legacy `vaddv` reduce,
`crates/simd-scan/src/neon.rs:690`) would cut the 0.78 cyc/B classify floor and
dominate any pipelining gain. Wiring onto the live JSON parse path is a separate,
larger change (the live scan is `BBNF_TAPE_CAPACITY`-gated and the parser does
not consume a position index today).

## Honest bottom line

Multi-block software pipelining is **measured, real, and small** on M5 (~1.05-
1.06x). It is **not** the F1 scan lever the 1.15x gate demanded — the OoO core is
already the pipeliner for a loop this tight, and the classify stage is ALU-bound
on the movemask reconstruction, which is where the cycles actually are. Route the
F1 effort at the movemask narrow + compaction, not the block schedule.

## Reproduce

```
cd skinny
cargo test --release -p bbnf-simd --test structural_scan_bench -- --nocapture --exact prove04_pipelined_scan
# env: SCAN_BENCH_N (default 200), SCAN_BENCH_ITERS (default 8)
```
