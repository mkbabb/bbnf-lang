# LOCK-9 · pass-3 · PROTO-1 — Brief 1 (9A vs 9B): the cross-crate `#[inline]` seam

**Probe 1, family 9A vs 9B.** The dispatched brief string was the placeholder `"Test brief one."`
The probe *title* maps to FAMILY-SPECS **BRIEF 1** — "the evolved-home axis re-measurement + 9A's perf
half on CSS." The full brief (scratch parse-that + scratch bbnf-simd, purge the 4 double-`vaddv` sites,
de-face the OnceLock kernels, wire a `[patch]` path-dep, paired cold divan `parse_that_css` N≥200 two
windows) does not fit a 50-min cap under load ~5.1. Per the RULES ("a prototype either RUNS or is marked
SPEC_ONLY; prefer running") and parsimony, I ran **the smallest probe that answers Brief 1's decisive
sub-question** — gate (3)/(4), reconciliation 2: *does a shared-home `#[inline]` byte-kernel inline across
an **rlib crate boundary** under fat-LTO cgu=1, leaving zero in-loop indirect, or does a cross-crate
`bl`/`blr` survive?* This is the mechanism behind the "9A⊗9B perf axis WASHES" finding the whole lock
turns on. parse-that and bbnf-simd were **not touched** (READ-ONLY, P23); the probe is a self-contained
two-crate reproduction in scratch.

## Question

Reconciliation 2 asserts the 9A/9B perf axis washes because (i) a direct `#[inline]` producer crosses the
rlib seam free at cgu=1, and only (ii) an *intentional* OnceLock runtime-dispatch pointer — **present in
either home** — costs an indirect. Two passes never disassembled the seam itself. Is the claim true on
*this* aarch64 toolchain, as machine code?

## Method

A minimal cargo workspace reproducing the parse-that↔bbnf-simd seam (`brief1-seam/`, full source in
`PROTO-1.patch`), `[profile.release] lto="fat", codegen-units=1, panic="abort", opt-level=3`:

- **`kernel`** (separate **rlib**) = stand-in for the shared 9A home. Exposes `#[inline]`
  `movemask_lean` (LEAN `vshrn_n_u16::<4>`) + `classify_chunk` (`vceqq_u8`→lean), plus a
  `movemask_double_vaddv` counterexample (the refuted idiom). Pure `core::arch::aarch64`, no nightly
  features, no OnceLock.
- **`consumer`** (bin, path-dep on `kernel`) = stand-in for parse-that's scan sites. Three
  `#[inline(never)] #[no_mangle]` frames so `otool` can find them (this does not affect whether the
  *kernel* inlines *into* them):
  - `scan_inline_seam` — hot loop calls `kernel::classify_chunk` across the rlib boundary. **PRIMARY.**
  - `scan_via_oncelock` — dispatches through a `OnceLock<fn(...)->u64>` pointer. **SECONDARY** (recon 2's
    named boundary).
  - `scan_double_vaddv` — calls the refuted producer. **COUNTEREXAMPLE.**
- Build: `cargo build --release` (kernel + consumer both compiled fat-LTO cgu=1). Disasm: `otool -tvV`,
  then per-frame `bl`/`blr`/`shrn`/`addv` census + hot-loop-only indirect count.
- Timing (secondary, load-contended): built-in `bench` mode, 65536-byte scan (4096 chunks), 4000
  iters/scan, 6 windows alternating arm order (odd = LEAN-first, even = vaddv-first), loadavg recorded.

Env: `aarch64-apple-darwin`, arm64, rustc 1.96.0-nightly (02c7f9bec), 18 cores. **No x86.**
Substrate re-verified live this session: 4 double-`vaddv` sites present
(`bracket_depth_mask_64`/`comment_body_mask_64`/`structural_classify_64`/`byte_class_from_eq_set_64`);
`lo6_table_admissible` at `skinny/crates/bbnf-simd/src/dispatch.rs:101`.

## What ran

Built clean (2.06s), consumer executes, guard `0`. `otool -tvV` produced 55882 lines
(`PROTO-1-consumer.disasm`, saved beside this doc). Bench ran 6 windows.

## The numbers

Per-frame machine-code census (fat-LTO cgu=1):

| frame | total insns | `bl`/`blr` | **in-loop indirect** | `shrn` | `addv` |
|---|---|---|---|---|---|
| `scan_inline_seam` (cross-crate `#[inline]`) | 19 | **0** | **0** | 1 | 0 |
| `scan_via_oncelock` (runtime dispatch) | 45 | 2 | **1** | 0 | 0 |
| `scan_double_vaddv` (refuted idiom) | 26 | 0 | 0 | 0 | 2 |

`scan_inline_seam` hot loop (verbatim) — the LEAN producer is **inlined across the rlib seam**, no reduction op, no call:

```
ldr   q1, [x0], #0x10      ; load 16B chunk
cmeq.16b v1, v1, v0        ; vceqq_u8  (classify_chunk, from kernel rlib)
shrn.8b  v1, v1, #0x4      ; vshrn_n_u16::<4>  — LEAN producer, INLINED
fmov  x10, d1              ; vget_lane_u64
eor   x8, x8, x10          ; acc ^=
add   x9, x9, #0x10 / cmp x9,x1 / b.ls  <loop>
```

`scan_via_oncelock` hot loop retains **`blr x22`** (+ arg spill `stp q1,q0,[sp,#0x10]` — the vector
calling convention cannot ride an indirect, so it round-trips memory). The OnceLock fn-pointer does **not**
devirtualise: the runtime-dispatch boundary is real, and by construction it is the **same** boundary in
9A or 9B.

`scan_double_vaddv` inlines fully (0 `bl`) but carries **2 `addv.8b`** reduction ops per chunk (`addv b3` +
`ext.16b` + `addv b2` + `bfi`) — the refuted idiom, live counterexample to §0 iv.

Paired timing (65536 B/scan, 4000 iters, loadavg 5.12 before/after — **contended, secondary**):

| window (order) | LEAN inline ns/scan | double-`vaddv` ns/scan | LEAN/vaddv |
|---|---|---|---|
| 1 (LEAN first) | 1386.8 | 2429.4 | 0.571 |
| 2 (vaddv first) | 1550.6 | 2461.2 | 0.630 |
| 3 (LEAN first) | 1497.7 | 2458.9 | 0.609 |
| 4 (vaddv first) | 1460.1 | 2455.7 | 0.595 |
| 5 (LEAN first) | 1379.1 | 2443.4 | 0.564 |
| 6 (vaddv first) | 1504.8 | 2507.6 | 0.600 |

Mean LEAN/vaddv ≈ **0.595** (LEAN ~1.7× faster than the refuted idiom) — order-insensitive, corroborates
the "`shrn` narrow, never double-`vaddv`" law. NB this is the **producer-form** margin, not the 9A-vs-9B
axis; the axis is the seam, which the disasm shows costs **nothing** for a direct `#[inline]` producer.

## Verdict

**RAN — MEASURED.** The seam half of Brief 1 gate (3)/(4) is CONFIRMED in machine code: a shared-home
`#[inline]` byte-kernel inlines across the **rlib crate boundary** at fat-LTO cgu=1 with **zero in-loop
indirect** (`scan_inline_seam`: 0 `bl`/`blr`, LEAN `shrn.8b` present inline). The only surviving in-loop
indirect is the **intentional OnceLock runtime-dispatch pointer** (`scan_via_oncelock`: 1 `blr` + arg
spill) — reconciliation 2's named boundary, **identical in either kernel home**. This is the mechanism of
the "9A⊗9B perf axis WASHES" verdict, now shown as code rather than asserted: **the choice of home does
not change the in-loop instruction stream of a direct producer**, so the axis is a maintenance-cost call
(LOCK-6), exactly as the synthesis holds.

**Scope honesty (what did NOT run):** this is a *minimal reproduction of the seam*, not the full Brief 1.
NOT run: the actual scratch parse-that + scratch bbnf-simd build; the purge of the 4 real double-`vaddv`
sites; the OnceLock de-face of the 5 `prim::` kernels; the paired cold divan `parse_that_css` N≥200 two
windows (the CSS ≥0.98× no-regression floor — Brief 1's *other* PASS/FAIL half); and the evolved-home
LOC/`PaddedView`/`vqtbl1q`-vs-`vqtbl4q` divergence table (the LOCK-6 datum). Those remain owed — this
probe de-risks the seam claim they rest on. The dispatched brief string was the placeholder `"Test brief
one"`; I mapped it to FAMILY-SPECS Brief 1 by the probe title and ran its decisive slice.

**VERDICT LINE:** cross-crate `#[inline]` producer → **0 in-loop indirect** at fat-LTO cgu=1 (rlib seam
crossed free); intentional OnceLock pointer → **1 in-loop `blr`**, same in either home; refuted double-
`vaddv` → 2 reduction ops, ~1.7× slower. Seam half PASS; 9A⊗9B perf axis wash-mechanism confirmed in
machine code. Artifacts: `PROTO-1.patch`, `PROTO-1-consumer.disasm` (this dir).
