# SK-V7 B6 — DAV1D-grade checkasm hardening design

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Output date: 2026-05-16
Wave: SK-V7 Wave 4 (parallel with Lock 14 cleanup)
Hard caps: 45 min implementation; 15 min measurement
Predecessors: `restart/skinny/tranches/sk-v6/research/skv6-B2-checkasm-hardening-plan.md`, SK-V7 Wave A finding A5

Scope guardrail. Design artifact only; no tracked file is modified by this
document. Implementation lands as a follow-on tranche.

## 1. Inventory — what exists today

`skinny/crates/bbnf-simd/tests/` already hosts a partial checkasm surface that
this design augments rather than replaces:

| File | LOC | Role |
| --- | --- | --- |
| `checkasm_common.rs` | 164 | Shared `Xorshift64`, `guarded_call`, `stack_canary_then`, AArch64-only `callee_saved_register_then` |
| `checkasm_parity.rs` | 742 | Original V5 classifier harness; private Xorshift, private signal_guard, `Instant`-based bench |
| `checkasm_byte_class_from_eq_set_64.rs` | 533 | V5 admit; duplicates Xorshift + signal_guard + non-asserting stack canary |
| `checkasm_byte_class_from_table_64.rs` | 49 | Uses `checkasm_common::guarded_call` |
| `checkasm_bulk_emit_positions_64.rs` | 60 | V6 admit; uses `checkasm_common` |
| `checkasm_structural_terminator_64.rs` | 62 | V6 admit; uses `checkasm_common` |
| `checkasm_bitmap_prefix_xor_64.rs` | 37 | Uses `checkasm_common` |
| `checkasm_bitmap_next_set_bit.rs` | 29 | Uses `checkasm_common` |
| `checkasm_eob_pad_clamp.rs` | 26 | Uses `checkasm_common` |
| `checkasm_utf8_block.rs` | 68 | AArch64 only; bespoke (no `checkasm_common`) |

Critical gaps observed during inventory (file:line citations):

- `tests/checkasm_byte_class_from_eq_set_64.rs:131-138` declares a 1 KiB
  `[0xDE; 1024]` canary, reads it via `read_volatile` pre/post, but never
  compares pre vs post — the canary is purely a stack-prefill, not a
  tamper detector.
- `tests/checkasm_common.rs:46-52` does fold pre/post equality, but the
  closure is a *safe Rust* call shape, so its result attests to the Rust
  wrapper, not to a raw extern symbol.
- `tests/checkasm_common.rs:55-93` provides `callee_saved_register_then` for
  AArch64 only; x86_64 has no analogue, so the `byte_class_from_eq_set_64`
  AVX-512 candidate runs without callee-saved coverage.
- `tests/checkasm_parity.rs:706-714` measures wall-clock `Instant::now()` and
  later multiplies by an assumed `3.5 GHz` constant. The B/cycle column is
  fabricated, not measured.
- `tests/checkasm_parity.rs:140-153` installs a `libc::signal` handler that
  panics from the handler itself — not async-signal-safe and not recoverable.
- `tests/checkasm_byte_class_from_eq_set_64.rs:33-81` duplicates `Xorshift64`
  verbatim. The comment at lines 19-25 explains why (integration tests are
  separate crates), but the duplication is now load-bearing across four
  files and breaks edit isomorphism.

## 2. Goals and non-goals

Goals (SK-V7 Wave 4):

1. Promote `checkasm_common.rs` to the single source of truth for `Xorshift64`,
   signal handling, stack canary fold, register-clobber sentinels (both
   arches), and cycle-counter readout.
2. Convert the existing stack canary from prefill-only to **XOR-fold
   compare**: pre/post u8 fold, assert equality.
3. Add x86_64 SysV callee-saved sentinels (`rbx`, `rbp`, `r12-r15`) plus
   xmm/ymm/zmm6-15 coverage when the kernel under test declares it touches
   them.
4. Replace `Instant` cycle accounting with native cycle counters:
   `__rdtsc`/`__rdtscp` on x86_64, `mach_absolute_time` on Darwin AArch64,
   `cntvct_el0` (ISB-bracketed) on Linux AArch64. Report min-cycles across
   N iterations, not mean.
5. Provide a single `checkasm_call!` macro that composes signal guard +
   register-clobber + stack-canary + cycle counter around any candidate.
6. Convert the four "thin" V6 tests + the V5 byte_class test to call through
   the new macro so per-primitive hardening uplift is uniform.

Non-goals (deferred to SK-V8):

- Real `sigsetjmp`/`siglongjmp` recoverable faults. The B2 plan stage 3
  describes this; we keep the panic-from-handler for V7 and note the
  follow-up in the design.
- Forced-tier dispatch hooks (`BBNF_SIMD_FORCE=`). B2 stage 4 territory.
- `tests/checkasm_ffi_x86_64.asm` raw ABI shim. B2 stage 2 lands once a
  non-Rust-wrapped extern symbol exists.
- Windows x64 ABI rows. AArch64 macOS + x86_64 Linux only for V7.

## 3. Shared module — `tests/checkasm_common.rs` rewrite

LOC budget: 250 (current 164 → +86). The module becomes a literal `mod
checkasm_common;` include from every integration test (current pattern, see
`checkasm_bulk_emit_positions_64.rs:1`). No promotion to a separate crate;
Cargo's integration-test-per-crate model would force a new published surface.

### 3.1 Public API skeleton

```rust
// tests/checkasm_common.rs
#![allow(dead_code)]

pub struct Xorshift64(u64);
impl Xorshift64 { /* unchanged from existing 3-31 */ }

/// Compose all guards around a single candidate-kernel call.
#[macro_export]
macro_rules! checkasm_call {
    (label = $label:literal, $candidate:expr) => {{
        $crate::checkasm_common::signal_guard::arm();
        $crate::checkasm_common::with_register_sentinels(|| {
            $crate::checkasm_common::with_stack_canary($label, || ($candidate)())
        })
    }};
}

/// Cycle-min benchmark; returns the smallest cycle delta across N runs.
pub fn rdtsc_min_cycles<F: FnMut()>(iters: usize, mut body: F) -> u64 { ... }

pub mod signal_guard { /* unchanged at this stage; see §7 */ }
```

### 3.2 Stack canary XOR-fold (replaces lines 41-53)

```rust
#[inline(never)]
pub fn with_stack_canary<F, R>(label: &'static str, f: F) -> R
where F: FnOnce() -> R,
{
    // 1 KiB pinned at a fixed offset in this frame.
    let mut canary = [0u8; 1024];
    Xorshift64::new(0xCANA_RYCA_NARY_CANE_u64 | 1).fill(&mut canary);
    let pre_fold = canary.iter().fold(0u8, |a, b| a ^ b);
    std::hint::black_box(&canary);

    let result = f();

    std::hint::black_box(&canary);
    let post_fold = canary.iter().fold(0u8, |a, b| a ^ b);
    if pre_fold != post_fold {
        // Find the first divergent byte for actionable reporting.
        let mut reference = [0u8; 1024];
        Xorshift64::new(0xCANA_RYCA_NARY_CANE_u64 | 1).fill(&mut reference);
        let first_bad = canary.iter()
            .zip(reference.iter())
            .position(|(a, b)| a != b);
        panic!(
            "checkasm[{label}]: stack canary clobbered (pre={pre_fold:#04x} \
             post={post_fold:#04x} first_bad_byte={first_bad:?})"
        );
    }
    result
}
```

Why XOR fold and not memcmp? An XOR fold collapses 1 KiB to a single u8 and
is read-once-per-byte; the candidate kernel cannot fingerprint the canary
pattern to skip clobbered bytes. The reseeded `Xorshift64` reconstructs the
authoritative pattern on mismatch for first-bad-byte diagnostic.

### 3.3 Register sentinel shim — AArch64 (extends existing 95-164)

The existing `read_aarch64_callee_saved` / `write_aarch64_callee_saved` cover
`x19-x28`. Extend to include `d8-d15` (low 64 bits of `v8-v15`) and `x29`/`x30`
*observation only* (we cannot write LR pre-call). Pattern:

```rust
#[cfg(target_arch = "aarch64")]
#[inline(always)]
fn read_aarch64_neon_callee_saved() -> [u64; 8] {
    let mut out = [0u64; 8];
    unsafe {
        core::arch::asm!(
            "fmov {d8}, d8",  "fmov {d9}, d9",
            "fmov {d10}, d10","fmov {d11}, d11",
            "fmov {d12}, d12","fmov {d13}, d13",
            "fmov {d14}, d14","fmov {d15}, d15",
            d8 = out(reg) out[0], d9 = out(reg) out[1],
            d10 = out(reg) out[2], d11 = out(reg) out[3],
            d12 = out(reg) out[4], d13 = out(reg) out[5],
            d14 = out(reg) out[6], d15 = out(reg) out[7],
            options(nostack, preserves_flags)
        );
    }
    out
}
```

The `write` counterpart uses `fmov dN, {reg}`. Sentinel pattern:
`0xD0D0_D0D0_D0D0_D000 | reg_index`.

### 3.4 Register sentinel shim — x86_64 SysV (NEW)

```rust
#[cfg(target_arch = "x86_64")]
#[inline(always)]
fn read_x86_64_callee_saved() -> [u64; 6] {
    let rbx: u64; let rbp: u64;
    let r12: u64; let r13: u64; let r14: u64; let r15: u64;
    unsafe {
        core::arch::asm!(
            "mov {rbx}, rbx",
            "mov {rbp}, rbp",
            "mov {r12}, r12",
            "mov {r13}, r13",
            "mov {r14}, r14",
            "mov {r15}, r15",
            rbx = out(reg) rbx, rbp = out(reg) rbp,
            r12 = out(reg) r12, r13 = out(reg) r13,
            r14 = out(reg) r14, r15 = out(reg) r15,
            options(nostack, preserves_flags)
        );
    }
    [rbx, rbp, r12, r13, r14, r15]
}
```

The write counterpart cannot directly assign `rbp` without spooking the
frame-pointer machinery; we wrap that one slot in `#[inline(never)]`
boundaries and use `options(nomem)` to keep Rust's view consistent. Sentinel
values: `0x1919..1919`, `0x2020..2020`, `0x2121..2121`, etc. — same pattern
as AArch64 for cross-arch trace homogeneity.

### 3.5 vzeroupper / k-mask discipline (x86_64, AVX-512 candidates only)

Post-call assertion when the candidate touched AVX-512 state:

```rust
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[inline(always)]
unsafe fn assert_ymm_uppers_zero() {
    use core::arch::x86_64::*;
    // Read ymm6..ymm15 via dummy stores; assert upper halves are zero.
    // Skipped on hosts where `cpuid` does not advertise AVX2.
}
```

The check is *aspirational* for V7 — none of the seven currently admitted
primitives intentionally touches xmm/ymm6-15, but the
`byte_class_from_eq_set_64` AVX-512 body needs the discipline check before
it can be tier-tested.

### 3.6 Cycle counter

```rust
#[cfg(target_arch = "x86_64")]
#[inline(always)]
pub fn read_cycles() -> u64 {
    unsafe {
        core::arch::x86_64::_mm_lfence();
        let lo: u32; let hi: u32;
        core::arch::asm!("rdtsc", out("eax") lo, out("edx") hi,
            options(nomem, nostack, preserves_flags));
        ((hi as u64) << 32) | (lo as u64)
    }
}

#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
#[inline(always)]
pub fn read_cycles() -> u64 {
    // Darwin: mach_absolute_time is ticks; nominally 1 tick per cycle on
    // Apple silicon but the manifest stores `cycle_source = "darwin_mach"`
    // so downstream tooling does not over-claim.
    extern "C" { fn mach_absolute_time() -> u64; }
    unsafe { mach_absolute_time() }
}

#[cfg(all(target_arch = "aarch64", target_os = "linux"))]
#[inline(always)]
pub fn read_cycles() -> u64 {
    let value: u64;
    unsafe {
        core::arch::asm!(
            "isb",
            "mrs {value}, cntvct_el0",
            value = out(reg) value,
            options(nomem, nostack, preserves_flags),
        );
    }
    value
}

pub fn rdtsc_min_cycles<F: FnMut()>(iters: usize, mut body: F) -> u64 {
    // Warm: discard the first 4 iterations.
    for _ in 0..4 { body(); }
    let mut best = u64::MAX;
    for _ in 0..iters {
        let t0 = read_cycles();
        body();
        std::hint::black_box(());
        let t1 = read_cycles();
        best = best.min(t1.wrapping_sub(t0));
    }
    best
}
```

Min, not mean, because the minimum is the only sample uncontaminated by
preemption, SMI, or LLC fill. checkasm.h uses median; dav1d uses min.
We pick min for the JIT-friendliness in CI noise — single regressive run
cannot poison the report.

## 4. Per-primitive checkasm invariant table

Each row maps an admitted primitive to the hardening axes it must clear.
"new" denotes a V7-tranche additions; "kept" denotes the V5/V6 existing
coverage that survives the rewrite.

| Primitive | Tier | Stack canary (XOR-fold) | Reg-clobber shim | Cycle counter | Adversarial seeds | Alignment sweep | Corpus row | Tail-padding |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `BYTE_CLASS_FROM_EQ_SET_64` | V5 | kept (asserted in V7) | new x86_64 SysV; new AArch64 d8-d15 | new rdtsc | kept 0xCAFE/0xDEAD; new 0xBADC0FFEE0DDF00D | kept 0..64 | kept twitter.json | new |
| `BYTE_CLASS_FROM_TABLE_64` | V6 thin | new | new (both arches) | new | new (seed 0x5441…) | kept 0..64 | new (twitter + canada) | new |
| `BITMAP_PREFIX_XOR_64` | V6 thin | new | new | new | new | n/a (no buffer) | n/a | n/a |
| `BITMAP_NEXT_SET_BIT` | V6 thin | new | new | new | new | n/a | n/a | n/a |
| `BULK_EMIT_POSITIONS_64` | V6 admit | new | new | new | kept 0x4255…; new 0xFAFA… | n/a (mask-based) | new (json positions snapshot) | kept (prefix `0xFFFF_FFFF` guard) |
| `EOB_PAD_CLAMP` | V6 thin | new | new | new | new | kept 0..=64 | new | kept (lane zero check) |
| `STRUCTURAL_TERMINATOR_64` | V6 admit | new | new (AArch64) | new | kept 0x5354…; new 0xABAB… | kept 0..64 | new (twitter) | new |
| `UTF8_BLOCK` | AArch64 | new (test was bespoke) | new (AArch64) | new | new (boundary continuations) | new (block-aligned only; primitive is 16B) | new (twitter UTF-8 paths) | new (continuation across boundary) |

Total per-primitive uplift averages ~40 LOC (4 axes × ~10 LOC of macro
plumbing). Eight rows × 40 ≈ 320 LOC. Combined with `checkasm_common.rs`
delta (+86 LOC) the budget lands at ~406 LOC, comfortably inside the
400-500 target with headroom for inline comments.

## 5. Per-test call-site shape

Existing thin-test pattern (`checkasm_bulk_emit_positions_64.rs:40-43`):

```rust
assert_eq!(
    guarded_call(|| candidate(base, mask)),
    reference(base, mask),
    "base={base} mask={mask:#018x}"
);
```

V7 rewrite uses the new macro and emits a separate cycle column:

```rust
let observed = checkasm_call!(label = "bulk_emit/random", || candidate(base, mask));
let expected = reference(base, mask);
assert_eq!(observed, expected, "base={base} mask={mask:#018x}");

if std::env::var_os("BBNF_SIMD_CHECKASM_BENCH").is_some() {
    let cycles = rdtsc_min_cycles(256, || {
        std::hint::black_box(candidate(base, mask));
    });
    eprintln!("[checkasm] bulk_emit base={base} mask={mask:#018x} cycles={cycles}");
}
```

Cycle reporting is opt-in (`BBNF_SIMD_CHECKASM_BENCH=1`); the default
test invocation runs correctness only and is deterministic.

## 6. Implementation order (Wave 4 micro-sequence)

The hardening primitives land in this order, smallest-first so each
intermediate state remains green:

1. **Stack canary XOR-fold** — drop-in replacement of
   `with_stack_canary` in `checkasm_common.rs`. Every test gains the
   assertion. Estimated impact: 10 LOC swap; CI runs unchanged.
2. **x86_64 register sentinels** — adds `read_x86_64_callee_saved` +
   `with_register_sentinels` x86_64 branch. AArch64 branch is the existing
   helper renamed. Estimated impact: 70 LOC.
3. **Cycle counter** — `read_cycles` + `rdtsc_min_cycles` added; no
   call-site changes yet. 40 LOC.
4. **`checkasm_call!` macro** — composes the three guards. 20 LOC.
5. **Test rewrites** — eight integration tests adopt the macro and migrate
   their adversarial-seed cases. 320 LOC churn across tests, mostly
   mechanical.
6. **`checkasm_parity.rs` migration** — replace private `signal_guard`
   module + private `Xorshift64` with `checkasm_common::*`. Net negative
   LOC (~ -120 from `checkasm_parity.rs`, +0 in `checkasm_common.rs`).

The first three steps are independently green. Step 4 lands once steps 1-3
have been committed. Step 5 is the parallelizable burst; step 6 is the
clean-up sweep.

## 7. Deferred items (B2 stages 2-5 not in this design)

Cited here so the design's scope boundary is explicit, not implicit:

- `sigsetjmp`/`siglongjmp` recoverable faults (B2 stage 3). Current `libc::
  signal` + panic-from-handler stays for V7. Tracked in V8 as a follow-on.
- `BBNF_SIMD_FORCE=` tier masks (B2 stage 4). Requires runtime dispatch
  refactor in `src/dispatch.rs`; out of scope.
- Raw extern checked-call shim (`tests/checkasm_ffi_x86_64.asm`,
  `tests/checkasm_ffi_aarch64.S`). All V7 primitives are reached through
  Rust safe wrappers, so the shim is unneeded *until* a non-wrapped extern
  is admitted.
- Windows x64 ABI (`rbx`, `rbp`, `rdi`, `rsi`, `r12-15`, `xmm6-15`). macOS +
  Linux only for V7.
- Manifest file (`tests/checkasm_manifest.rs` / `.toml`). B2 stage 0
  artifact; tracking only.

## 8. Exit gate

The B6 hardening tranche is complete when, on both an Apple M-series host
and an x86_64 Linux host:

1. Every integration test in `skinny/crates/bbnf-simd/tests/checkasm_*.rs`
   compiles against the renamed `checkasm_common.rs` and passes under
   `cargo test --profile ax-iter -p bbnf-simd --tests checkasm_`.
2. The stack canary fold-equality assert is reached by every guarded
   call site (verified by intentional bug-injection through a one-line
   diff that writes `canary[0] ^= 1` inside a candidate; the run must
   fail with the new diagnostic).
3. The x86_64 register sentinel check is reached and passes; verified by
   a one-line diff that clobbers `rbx` in a candidate; the run must
   fail with `clobbered an x86_64 callee-saved register`.
4. The cycle counter reports non-zero deltas for at least one primitive
   under `BBNF_SIMD_CHECKASM_BENCH=1`; the value must be reproducible
   within 10% across three consecutive runs (validated by the
   `rdtsc_min_cycles` minimum-aggregator).
5. `checkasm_parity.rs` no longer carries a private `Xorshift64`,
   `signal_guard`, or `stack_clobber_then`. All three come from
   `checkasm_common`.

## 9. Critical landing-first hardening primitive

Of the four hardening axes the **stack canary XOR-fold compare** is the
single highest-value item to land first:

- It is the smallest delta (≈10 LOC change to `with_stack_canary`).
- It immediately upgrades every existing `guarded_call(...)` site without
  any per-test rewrite — all five thin V6 tests inherit the fix
  transparently.
- The current canary is *silent*: a kernel that writes one byte past its
  expected frame today produces no signal. Closing that hole is a
  correctness bound, not a diagnostic improvement.
- It unblocks the credibility of the V5 `byte_class_from_eq_set_64` admit,
  whose 533-LOC test (`tests/checkasm_byte_class_from_eq_set_64.rs:131-138`)
  *appears* to canary but in practice does not.

Implementation order recommendation: land step 1 (canary fold) as a
standalone commit, then step 2 (x86_64 register sentinels) as a second
commit, then steps 3-6 as a third commit. The first commit alone closes
the most exploitable diagnostic gap in the existing harness.

## 10. File and citation index

Primary citations used in this design:

- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:46-52` — current
  canary that prefills but does not assert.
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:55-93` — AArch64-only
  register sentinel.
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:131-138`
  — 1 KiB canary that lacks the post-call compare.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:140-153` — panic-from-
  signal-handler that B2 stage 3 wants replaced (deferred here).
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:706-714` — Instant
  cycle accounting that fabricates B/cycle.
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:40-43`
  — example thin-test call shape that the V7 macro replaces.
- `restart/skinny/tranches/sk-v6/research/skv6-B2-checkasm-hardening-plan.md:42-97`
  — admission principle and per-primitive status table from which this
  design inherits its row taxonomy.

End design.
