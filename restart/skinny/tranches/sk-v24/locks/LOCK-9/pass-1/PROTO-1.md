# PROTO-1 — LOCK-9 pass-1, probe 1: 9A/9B kernel-home adjudication

**Family:** 9A (cross-crate kernel inline) + paired no-regression.
**Question that decides the axis:** Can parse-that consume a `bbnf-simd` kernel
through the crate boundary **without an indirect call in the hot loop** (fat-LTO),
and pass no-regression? PASS ⇒ 9A(shared) is viable; FAIL ⇒ 9B(own home) forced.

Kernel under probe (session sharpening): `bitmap_prefix_xor_64` — the one that
routes through the `dispatch.rs` `OnceLock` fn-pointer table
(`bbnf-simd/src/lib.rs:261-262` → `dispatch::primitive_kernels().bitmap_prefix_xor_64`),
**not** a direct-cfg primitive. Consumer in parse-that:
`parse_that/src/parsers/scan/quote_parity.rs:41` `prefix_xor`, called per 64-byte
stripe inside `compute_in_string_bitmap` (lines 139, 165).

**Verdict line:** **Arm A FAILS, Arm B PASSES.** The `OnceLock` fn-pointer facade
survives fat-LTO as an indirect `blr` **inside the loop body** — 9A **through the
current `prim` facade is blocked**. A direct `#[cfg]` entry (Arm B) inlines to
zero indirect branches and is byte-identical to parse-that's existing inline body.
**9A(shared) is viable *only* with the direct-cfg refactor of the hot-primitive
facade; the OnceLock dispatch layer must be bypassed for hot kernels.**

---

## Method (what actually ran)

Parse-that is READ-ONLY. Per R3 §8 the working tree was rsync-copied to
`scratchpad/pt-scratch` with external patch paths repointed to originals and an
isolated `CARGO_TARGET_DIR`. `bbnf-simd` was copied to `scratchpad/bbnf-simd-scratch`
(standalone manifest, `edition=2021`, `blake3=1`; dev-deps/tests dropped).

Two facts made a **minimal isolating probe** the parsimonious decisive test rather
than a full parse-that rebuild:

1. **`bbnf_simd::prefix_xor_64(mask, false)` is bit-identical to parse-that's
   `prefix_xor(mask)`.** The aarch64 "NEON" body
   (`aarch64/bitmap_prefix_xor_64.rs`) *delegates to the scalar bit-twiddle*; with
   `carry_in=false` the scalar body is the same five-shift/xor chain parse-that
   already inlines. Parity was asserted at runtime (see below).
2. **The kernel's only consumers — `compute_in_string_bitmap` / `filter_quote_parity`
   — are NOT on the `parse_that_combinator` bench hot path.** `json_parser()` scans
   strings via `sp_quoted_string` (`span_parser/constructors.rs:120`); nothing in
   `src/` calls `filter_quote_parity` except the re-export. So the specified
   no-regression bench (6 JSON corpora via `json_parser`) **cannot execute
   `prefix_xor`** — a paired run would measure pure noise and "pass" ≥0.98× trivially
   while saying nothing about the kernel swap. The honest measurement surface is the
   kernel call itself.

Probe crate (`scratchpad/lock9-probe`, profile `bench-like` = `lto="fat"`,
`codegen-units=1`, `opt-level=3`, toolchain `nightly-2026-04-11`) emits five
`#[no_mangle] #[inline(never)]` functions across the same crate boundary parse-that
would cross:

| fn | body |
|---|---|
| `probe_baseline` | parse-that's inline five-shift/xor (the status quo) |
| `probe_arm_a` | `bbnf_simd::prefix_xor_64(m,false)` → `prim` → **OnceLock** table |
| `probe_arm_b` | `bbnf_simd::prefix_xor_64_direct(m,false)` → direct `#[cfg]`, **no OnceLock** |
| `probe_loop_arm_a` / `_b` | the same calls **inside a per-stripe loop** |

`prefix_xor_64_direct` is the counterfactual added to `bbnf-simd`
(`armB-bbnf-simd-lib.patch`): `#[cfg(target_arch="aarch64")]` → call the NEON body
directly, bypassing `dispatch::primitive_kernels()`.

Build → run (parity) → `otool -tvV` → grep `blr`/`br`/`bl→kernel` per function.

---

## The numbers

### Correctness / parity (runtime)
```
baseline=0xffaaffaaf1ec3228  arm_a=0xffaaffaaf1ec3228  arm_b=0xffaaffaaf1ec3228  parity=true
```
All three arms agree bit-for-bit (`carry_in=false`). Semantic swap is sound.

### Criterion (a) — indirect calls in the hot loop (fat-LTO disassembly, aarch64)

| function | instrs | indirect `blr`/`br` | `bl`→OnceLock/kernel |
|---|---:|---:|---:|
| `probe_baseline` | 7 | **0** | 0 |
| `probe_arm_a` (OnceLock facade) | 19 | **1** | 1 |
| `probe_arm_b` (direct cfg) | — | **0** | 0 *(ICF-folded onto `probe_baseline`, same address)* |
| `probe_loop_arm_a` | 33 | **1 — inside the loop body** | 1 |
| `probe_loop_arm_b` | 16 | **0** | 0 |

`probe_arm_a` hot core (the indirect call the criterion forbids):
```
adrp x8, 0x100044000 ; add x8,#0x218 ; ldapr x8,[x8] ; cbnz ...   ; OnceLock state acquire-load + init check
ldr  x8, [x8, #0x228]                                             ; load kernel fn-pointer
blr  x8                                                           ; <-- INDIRECT CALL to the kernel
```
In `probe_loop_arm_a`, LICM hoists the *address* of the OnceLock cell out of the
loop, but the `ldapr` acquire-load, the `cbnz` init-branch, the fn-pointer `ldr`,
and the **`blr x8` indirect call remain in the loop body every iteration.**

`probe_loop_arm_b` loop body is a pure `eor` chain — zero `blr`, zero OnceLock:
```
ldr x10,[x0],#8 ; eor x10,x10,x8 ; eor..lsl#1 ; ..#2 ; ..#4 ; ..#8 ; ..#16 ; eor x8,x8,x10,lsl#32 ; eor x8,x8,x10 ; subs; b.ne
```
`nm` shows `_probe_arm_b` and `_probe_baseline` at the **same address** — the linker
identical-code-folded them, i.e. the direct-cfg path is provably the same machine
code as parse-that's existing inline body.

### Criterion (c) — no-regression (kernel-level micro-timing, the only surface that runs the swap)

16 384 masks/rep (≈ a 1 MiB input's stripe count), 2000 reps, best-of-9, three
process runs. **loadavg during runs: 13–16 (recorded, high — treat absolutes as
noisy; the ratio and the sign are robust and corroborated by the disassembly).**

| run | arm_a (OnceLock) ns/call | arm_b (direct) ns/call | arm_b / arm_a |
|---|---:|---:|---:|
| 1 | 3.787 | 3.453 | 0.912 |
| 2 | 3.653 | 3.405 | 0.932 |
| 3 | 3.608 | 3.366 | 0.933 |

- **arm_b vs baseline: 1.00×** (identical machine code — ICF proof, not a measurement).
- **arm_a vs baseline: ≈0.91–0.93×** — the OnceLock acquire-load + init-branch +
  indirect call costs ~0.25–0.35 ns/stripe; **fails the ≥0.98× threshold even at the
  kernel micro level.**

### Criterion (b) — `vshrn`/`to_bitmask` reduction count: **N/A to this kernel.**
`to_bitmask`/`vshrn` is the NEON movemask reduction in `classify_stripe_64`, a
**different** kernel. The sharpened probe targets `bitmap_prefix_xor_64`, which has
no `to_bitmask` reduction and was not rerouted. This criterion belongs to a
classify-stripe probe, not the prefix-xor probe; nothing here regressed it because
nothing here touched it.

---

## Verdict

| arm | criterion (a) zero indirect-in-loop | (c) ≥0.98× | result |
|---|---|---|---|
| **A** — 9A via current `prim`/OnceLock facade | ✗ `blr x8` survives in loop | ✗ ~0.92× | **FAIL** |
| **B** — direct `#[cfg]` entry, no OnceLock | ✓ 0 indirect (ICF ≡ baseline) | ✓ 1.00× | **PASS** |

**Adjudication for the 9A/9B axis:** The shared kernel home (9A) is **not
disqualified** — parse-that *can* consume a `bbnf-simd` kernel with zero hot-loop
indirect calls and zero regression — **but only through a direct `#[cfg]`-selected
entry point.** The current `dispatch.rs` `OnceLock` fn-pointer facade
(`prim::bitmap_prefix_xor_64`) is a hard devirtualization barrier that fat-LTO
cannot pierce, so consuming *that* API in a hot loop **fails**. Per the brief this
"proves the fn-pointer facade blocks 9A and mandates the direct-cfg refactor."
**Passing arm: B.** Actionable consequence for LOCK-9: if 9A(shared) is chosen,
hot primitives must expose a direct-cfg surface alongside (or instead of) the
runtime-dispatched table; 9B(own home) would inherit the identical constraint, so
the OnceLock-vs-direct-cfg decision — not shared-vs-own — is the real lever.

---

## Provenance / honesty notes
- **Ran, measured.** aarch64 (arm64), toolchain `nightly-2026-04-11`, profile
  `bench-like` (`lto="fat"`, `cu=1`, `O3`). loadavg 12→16 across the session
  (recorded per run). No x86.
- **Parsimony deviation from the letter of the brief:** the decisive test isolates
  the *kernel call* in a minimal probe crate rather than rebuilding full parse-that
  and disassembling `filter_quote_parity`. Justification: (i) the call site is a
  byte-identical one-line swap (`prefix_xor(x)` → `bbnf_simd::prefix_xor_64(x,false)`)
  across the *same* parse_that↔bbnf-simd LTO boundary, so the codegen generalizes;
  (ii) the specified `parse_that_combinator` bench provably never executes this
  kernel (dead relative to `json_parser`), so a full-bench "pass" would be
  disingenuous. The intended one-line `quote_parity.rs` patch (Arm A) is provided as
  a diff for the eventual real integration.
- **Artifacts** (`PROTO-1-artifacts/`): `armA-quote_parity.patch` (intended
  integration diff), `armB-bbnf-simd-lib.patch` (the direct-cfg wrapper actually
  built), `lock9-probe-main.rs` + `lock9-probe-Cargo.toml` (the probe),
  `disasm-probe-functions.txt` (full fat-LTO disassembly of all five functions).
