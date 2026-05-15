# SK-V6 B2: DAV1D/checkasm hardening plan

Workspace read: `/Users/mkbabb/Programming/bbnf-lang`

Output date: 2026-05-15

Scope: planning artifact only. No repository file should be edited by this
cohort step. This plan is grounded in `/tmp/skv6-A2-dav1d-asm-process.md` and
the local `skinny/crates/bbnf-simd` test surface.

## Local facts that drive the plan

- `tests/checkasm_common.rs` has reusable `Xorshift64`, `guarded_call`, and a
  stack canary that is asserted after the Rust closure returns.
- `tests/checkasm_common.rs` also has `callee_saved_register_then` for AArch64
  Rust closures. Per A2 and `skinny/REDRESS.md`, this is not a valid ABI
  boundary proof for raw ASM and must not be used as admission evidence.
- `tests/checkasm_parity.rs` duplicates RNG, signal, stack clobber, robust mean
  bench, and strict-mode logic. Its signal handler uses process-global
  `libc::signal` and panics from the handler; that is not FFmpeg-equivalent
  recoverable fault handling.
- `tests/checkasm_parity.rs` reports bytes/cycle from `Instant` plus an assumed
  3.5 GHz cycle rate. That is diagnostic only, not an RDTSC/cycle gate.
- The primitive tests cover many good scalar-oracle rows:
  `checkasm_byte_class_from_eq_set_64`, `checkasm_byte_class_from_table_64`,
  `checkasm_bitmap_prefix_xor_64`, `checkasm_bitmap_next_set_bit`,
  `checkasm_bulk_emit_positions_64`, `checkasm_eob_pad_clamp`,
  `checkasm_structural_terminator_64`, and `checkasm_utf8_block`.
- Most primitive tests still call safe Rust wrappers through `guarded_call`.
  That proves wrapper parity, not raw ABI preservation.
- `src/dispatch.rs` already has a `PrimitiveKernels` `OnceLock`, but AArch64
  primitive selection is unconditional NEON and optional feature families are
  not represented as forced tiers.
- `src/lib.rs::prim::byte_class_from_eq_set_64` bypasses the table and selects
  the AVX-512 body through compile-time `target_feature = "avx512bw"`.
- `skinny/REDRESS.md` makes same-wave consumers non-negotiable. Orphan
  primitives cannot be admitted or credited on benchmark potential alone.

## Admission principle

Treat checkasm as an admission system, not as a loose differential test. A raw
SIMD/ASM primitive becomes `admitted` only when all of these are true:

1. It has a scalar executable oracle.
2. It has a primitive-specific parity matrix with deterministic seeds,
   boundary rows, alignment rows, and input/output guard checks.
3. Every host-supported CPU tier can be forced through the same matrix.
4. Raw extern candidates are called through an ABI checked-call shim.
5. Register clobbers, stack canaries, recoverable faults, and memory guards are
   reported as structured checkasm failures.
6. Cycle numbers are collected only after correctness passes and are tagged
   with the exact feature tier and counter source.
7. A real same-wave runtime/codegen consumer exists, or the primitive is
   `candidate` / `blocked_no_consumer`, not `admitted`.

## Stage 0 - Freeze the manifest before new ASM admission

Add a test/doc manifest, for example `tests/checkasm_manifest.rs` or
`checkasm_primitives.toml`, before landing new bodies.

Each row must contain:

- `primitive`
- `scalar_oracle`
- `safe_wrapper`
- `raw_symbols`
- `required_features`
- `forced_tiers`
- `parity_tests`
- `checked_call`
- `stack_canary`
- `register_clobber`
- `fault_recovery`
- `cycle_key`
- `same_wave_consumer`
- `status = candidate | admitted | blocked_no_consumer | retired`

Initial honest statuses:

| Primitive | Current status | Required status before admission |
| --- | --- | --- |
| `BYTE_CLASS_FROM_EQ_SET_64` | strong wrapper parity, raw AVX-512 symbol exists | `candidate` until table dispatch, forced AVX-512 row, checked-call row, and consumer evidence are present |
| `BYTE_CLASS_FROM_TABLE_64` | consumed by generic scan, wrapper parity | `admitted` only after forced-tier and raw checked-call rows for non-Rust bodies |
| `BITMAP_PREFIX_XOR_64` | consumed by JSON string-region scan, wrapper parity | `admitted` after carry-boundary rows plus forced-tier rows |
| `BITMAP_NEXT_SET_BIT` | consumed by `compact_mask`, wrapper parity | `admitted` after forced-tier rows |
| `BULK_EMIT_POSITIONS_64` | consumed by `compact_mask`, wrapper parity | `admitted` after destination suffix guard and capacity-contract rows |
| `EOB_PAD_CLAMP` | consumed by JSON tail scan, wrapper parity | `admitted` after dead-lane and alias/guard rows |
| `STRUCTURAL_TERMINATOR_64` | consumed by AArch64 no-quote fast path | `admitted` after forced-tier rows and structural/terminator overlap rows |
| `UTF8_BLOCK` / `UNESCAPE_UXXXX` | AArch64 parity exists | `candidate` until stateful consumer and boundary-state oracle rows are complete |
| `BULK_EMIT_COMPRESSED` | no same-wave consumer | `blocked_no_consumer` |
| `FRAME_PUSH_BOUNDED` | no same-wave consumer | `blocked_no_consumer` |
| `FRAME_POP_BOUNDED` | no same-wave consumer | `blocked_no_consumer` |
| `FSM_DISPATCH_THREADED` | no generated per-grammar ASM consumer | `blocked_no_consumer` |

Gate: CI fails if any row marked `admitted` lacks scalar oracle, parity test,
forced-tier matrix, ABI checked-call coverage for raw ASM, and same-wave
consumer evidence.

## Stage 1 - Normalize scalar-oracle call shape

Centralize the current duplicated harness logic in `tests/checkasm_common.rs`.

Required helpers:

- deterministic RNG and seed logging
- `call_ref` for scalar oracle
- `call_new_safe` for Rust wrappers
- `call_new_ffi` for raw extern candidates
- source guard allocation
- destination prefix/suffix guard allocation
- alignment-window generation
- strict failure reporting
- recoverable fault wrapper
- feature-tier runner
- timing sampler

Rules:

- Scalar oracle calls never go through register-clobber shims.
- Rust safe wrappers can continue to use `guarded_call`.
- Raw ASM/FFI candidates must use `call_new_ffi`.
- Every parity failure reports primitive, tier, seed, alignment, row label,
  candidate value, reference value, and first divergent lane/offset when known.
- `BBNF_SIMD_STRICT=1` becomes the CI default for all checkasm tests. Logging
  without failure remains available only for local diagnosis.

Primitive row additions:

- `BYTE_CLASS_FROM_EQ_SET_64`: keep empty set, duplicate entries, set sizes
  1..=8, alignment sweep, adversarial seeds, corpus parity, constant fills,
  and tail-padding contract. Add explicit `set.len() > 8` safe-wrapper behavior:
  either return scalar fallback by construction or reject before raw dispatch.
- `BYTE_CLASS_FROM_TABLE_64`: keep density and alignment rows. Add exact rows
  for all-zero table, all-one table, singleton byte, every 2nd/3rd/5th/17th
  byte, low-half-only, high-half-only, randomized sparse, and randomized dense.
- `BITMAP_PREFIX_XOR_64`: keep fixed cases and random sweep. Add single-bit
  rows for every bit 0..63 under both carry states and chunk handoff rows from
  string/escape scanning.
- `BITMAP_NEXT_SET_BIT`: keep cursor 0..=64 rows. Add cursor exactly on set
  bit, cursor immediately after last set bit, and masks around 31/32/63/64.
- `BULK_EMIT_POSITIONS_64`: keep prefix guard. Add suffix guard, capacity equal
  to popcount, one-less-than-popcount if bounded output is a supported
  contract, zero mask, all-ones mask, and high base rows near `u32::MAX`.
- `EOB_PAD_CLAMP`: keep length 0..=64 and dead-lane zeroing. Add sentinel input
  rows proving inactive lanes do not leak, plus alias rows if aliasing is
  contract-permitted.
- `STRUCTURAL_TERMINATOR_64`: keep alignment and random JSONish rows. Add
  terminator also structural, terminator in/out of quoted regions, byte 0,
  byte 63, and no-terminator rows.
- `UTF8_BLOCK`: keep ASCII, complete multibyte, boundary continuation,
  overlong, surrogate, and `uXXXX` rows. Add truncated continuation tails,
  split 2/3/4-byte sequences across block boundaries, noncharacters according
  to language policy, and state/consumed-byte comparison.

Gate: every primitive test imports common helpers. No test-local copy of signal
or stack wrappers remains except for target-specific checked-call shims.

## Stage 2 - Add real ABI checked-call shims

Add test-only checked-call shims for raw extern candidates:

- `tests/checkasm_ffi_x86_64.asm`
- `tests/checkasm_ffi_aarch64.S`
- optional small C support file if `sigsetjmp` / `siglongjmp` integration is
  simpler there than in Rust

The shim ABI should be narrow and explicit:

```text
checkasm_call_<primitive>_<arch>(
    fn_ptr,
    marshalled_args,
    out_result,
    out_failure
) -> i32
```

Return codes:

- `0 = ok`
- `1 = register_clobber`
- `2 = stack_canary_clobber`
- `3 = fault_recovered`
- `4 = bad_stack_alignment`
- `5 = unsupported_abi_shape`

x86_64 SysV requirements:

- Seed and verify callee-saved GPRs: `rbx`, `rbp`, `r12`, `r13`, `r14`, `r15`.
- Preserve normal stack alignment before calling the candidate.
- Put canaries in the shim-owned frame and around stack-passed argument space,
  outside any area the ABI permits the callee to use.
- Verify the stack pointer returns to the expected value.
- Windows x64 is not admitted until a separate row checks `rbx`, `rbp`, `rdi`,
  `rsi`, `r12`..`r15`, and `xmm6`..`xmm15`.

AArch64 AAPCS64 requirements:

- Seed and verify GPRs `x19`..`x28`.
- Seed and verify the callee-saved low 64 bits of `v8`..`v15` / `d8`..`d15`.
- Preserve 16-byte stack alignment.
- Verify stack canaries around shim-owned stack and stack-passed argument
  slots.
- Do not treat Rust closure sentinels as evidence for this row.

Rules:

- `callee_saved_register_then` is retired from admission evidence or renamed
  to make clear it is a Rust-closure stress helper only.
- Checked-call rows are required only for raw `extern "C"`/ASM symbols.
- Safe Rust wrappers still run parity tests, but they do not satisfy raw ABI
  admission.

Gate: the first raw symbol, `byte_class_from_eq_set_64_avx512`, cannot be
marked `admitted` until it has a passing x86_64 checked-call row.

## Stage 3 - Replace panic signal handlers with recoverable faults

Current `libc::signal` plus panic from handler must be replaced.

Required behavior:

- Install handlers with `sigaction`.
- Cover `SIGSEGV`, `SIGBUS`, `SIGILL`, and `SIGFPE`.
- Handler body is async-signal-safe only.
- Use a test-only recovery trampoline based on `sigsetjmp` / `siglongjmp` or
  an equivalent native shim.
- Restore prior handlers after the guarded region or isolate handler state with
  thread-local active test context.
- Report primitive, tier, seed, alignment, and row label after recovery.

Rules:

- A candidate fault fails only that checkasm row, not the whole binary.
- A second nested fault during recovery terminates the process.
- Fault handling is correctness infrastructure, not a way to accept OOB loads.

Gate: no new unsafe ASM body is admitted while the active crash path is a Rust
panic from a signal handler.

## Stage 4 - Make CPU dispatch force-mask testable

Refactor dispatch into a dav1d-style monotonic table.

Add a feature tier enum:

```text
scalar
swar
aarch64_neon
aarch64_dotprod
aarch64_i8mm
aarch64_sve
aarch64_sve2
x86_avx2
x86_avx512icl
```

Feature detection rules:

- Start every `PrimitiveKernels` table with scalar functions.
- Override only the entries whose exact feature group is available and whose
  manifest row is not blocked.
- AArch64 baseline NEON may be normal for the target, but DOTPROD, I8MM, SVE,
  and SVE2 must be separate detected tiers.
- x86 AVX-512 rows must include OS-enabled vector/opmask state, not just CPUID
  instruction bits.
- AVX-512 rows relying on Ice Lake-class bundles should be named
  `x86_avx512icl`, not generic `avx512`.

Test controls:

```text
BBNF_SIMD_FORCE=scalar|swar|aarch64_neon|aarch64_dotprod|aarch64_i8mm|aarch64_sve|aarch64_sve2|x86_avx2|x86_avx512icl
BBNF_SIMD_MASK=<comma-separated disabled tiers>
```

Per primitive test matrix:

1. scalar oracle vs scalar candidate
2. scalar oracle vs each host-supported forced tier
3. scalar oracle vs default dispatch table
4. raw symbol checked-call row for every raw extern implementation

Immediate dispatch fix:

- Move `prim::byte_class_from_eq_set_64` into `PrimitiveKernels`.
- Remove compile-time-only public selection from `src/lib.rs`.
- Keep compile-time `cfg` only as a symbol availability guard; final selection
  must go through runtime feature detection plus test force masks.

Gate: a primitive cannot be `admitted` if checkasm cannot force its lower tier
after a faster tier is available on the host.

## Stage 5 - Enforce RDTSC/cycle discipline

Current `Instant`-based robust mean stays as local timing only. It must not
claim true bytes/cycle or gate admission.

Cycle measurement rules:

- Benchmark only after the same row passed scalar parity, memory guards,
  recoverable fault handling, and checked-call checks.
- Record cycle numbers under an explicit `cycle_source`:
  `x86_rdtsc`, `aarch64_cntvct`, `instant_ns`, or `external_perf`.
- `instant_ns` may report ns/byte only. It must not convert to bytes/cycle
  using an assumed GHz value.
- x86 cycle reads use serialized counters:
  start with `lfence; rdtsc`, stop with `rdtscp; lfence`, or an equivalent
  checkasm-approved serialization sequence.
- AArch64 cycle reads use `cntvct_el0`/`cntfrq_el0` only when available to
  EL0, bracketed by `isb`; otherwise fall back to `instant_ns`.
- Warm up before sampling.
- Alternate equivalent buffers during bench loops.
- Increase loop count until measurement overhead is amortized.
- Use the existing robust outlier rejection idea, but tag every sample with
  primitive, tier, CPU model, target triple, compiler flags, and force mask.
- Cycle regressions are reported separately from correctness. They do not
  turn a failing correctness row green or admit a primitive without a consumer.

Gate: no checkasm report may publish B/cycle without a real cycle counter or
external perf source. `classifier_bench_robust_mean` should rename its
existing B/cycle field or remove it until this lands.

## Stage 6 - Per-primitive exact admission gates

### BYTE_CLASS_FROM_EQ_SET_64

Required before admission:

- Scalar oracle:
  `src/scalar/byte_class_from_eq_set_64.rs`.
- Safe wrapper parity:
  current `checkasm_byte_class_from_eq_set_64` rows plus `set.len() > 8`
  wrapper-contract row.
- Raw x86 symbol row:
  `byte_class_from_eq_set_64_avx512(src, set_ptr, set_len)`.
- Checked-call:
  x86_64 shim verifies GPR preservation and stack canary.
- Dispatch:
  scalar, default, and `x86_avx512icl` forced rows. If AVX-512 BW-only remains
  supported separately, it gets a separate row; otherwise do not name it.
- Memory:
  source prefix/suffix guard around a 64-byte window and no read past exactly
  64 bytes; set buffer guard around `set_len`.
- Consumer:
  must name the runtime/generated path that selects eq-set classification. If
  no such path exists in the same wave, status remains `candidate`.

### BYTE_CLASS_FROM_TABLE_64

Required before admission:

- Existing density and alignment rows.
- Exact density rows listed in Stage 1.
- Forced rows for scalar, AArch64 NEON/TBL when present, and any future x86
  table-lookup tier.
- Checked-call rows for any raw symbol once it exists.
- Consumer remains generic `scan_dispatch`.

### BITMAP_PREFIX_XOR_64

Required before admission:

- Existing fixed and random carry rows.
- Exhaustive single-bit rows under both carry states.
- Handoff rows from quote/escape scanning.
- Forced rows for scalar, AArch64 NEON when present, and future x86 VPCLMUL
  tiers.
- Consumer remains JSON string-region scan.

### BITMAP_NEXT_SET_BIT

Required before admission:

- Existing cursor 0..=64 rows.
- Boundary rows around 0, 31, 32, 63, and 64.
- Forced rows for scalar, AArch64 NEON when present, and future BMI1/x86 tier.
- Consumer remains `compact_mask`.

### BULK_EMIT_POSITIONS_64

Required before admission:

- Existing prefix guard and base rows.
- Suffix guard after maximum 64 writes.
- Capacity contract rows: exact capacity and one-less-than-popcount if bounded
  output is part of the API.
- High-base overflow behavior specified and tested.
- Forced rows for every emitted backend.
- Consumer remains `compact_mask`.

### EOB_PAD_CLAMP

Required before admission:

- Existing length 0..=64 and dead-lane rows.
- Sentinel inactive-lane rows.
- Alias row if aliasing is legal.
- Forced rows for scalar and AArch64 NEON when present.
- Consumer remains JSON tail scan.

### STRUCTURAL_TERMINATOR_64

Required before admission:

- Existing AArch64 alignment and JSONish rows.
- Terminator/structural overlap rows.
- Byte 0, byte 63, and no-terminator rows.
- Quote-region rows when the primitive participates in string scanning.
- Forced rows for scalar and AArch64 table tier.
- Consumer remains JSON no-quote fast path.

### UTF8_BLOCK / UNESCAPE_UXXXX

Required before admission:

- Existing scalar and NEON parity rows.
- Stateful boundary rows that compare accept/reject status and consumed-byte
  state.
- Invalid hex, high surrogate without low surrogate, low surrogate without
  high surrogate, non-contiguous escape runs, and split block rows.
- Same-wave consumer inside string materialization or validation. Without that
  consumer, status is `candidate`.

## Stage 7 - Same-wave consumer gate

Same-wave consumer evidence is mandatory and mechanical.

A consumer row must include:

- repository path
- function/module name
- primitive call site
- feature tier selected
- scalar fallback path
- checkasm test proving the primitive contract
- integration test or bench gate proving the consumer path executes

Admission rule:

- `admitted` means correctness plus a consumer.
- `candidate` means correctness work exists but consumer or forced-tier proof is
  incomplete.
- `blocked_no_consumer` means no current runtime/codegen path uses the body.
- Benchmark-only use does not count as a consumer.
- Microbench speedups do not lift status.

Blocked until matching consumers exist:

- `BULK_EMIT_COMPRESSED` needs a structural-tape compressed sink consumer.
- `FRAME_PUSH_BOUNDED` needs a bracket-stack CollapsedStage consumer.
- `FRAME_POP_BOUNDED` needs a bracket-stack CollapsedStage consumer.
- `FSM_DISPATCH_THREADED` needs generated per-grammar `.asm` CollapsedStage
  consumers.

Gate: any implementation packet that includes one of the blocked primitives
must either land the same-wave consumer and all checkasm rows or remove the
primitive body from the packet.

## Stage 8 - CI order

Run order after implementation:

1. `cargo fmt`
2. scalar-oracle compile rows
3. wrapper parity rows under `BBNF_SIMD_FORCE=scalar`
4. forced-tier rows for every host-supported tier
5. raw checked-call rows
6. recoverable fault smoke rows
7. corpus parity
8. cycle diagnostics, correctness-passing rows only
9. same-wave consumer tests

Required environment rows:

```text
BBNF_SIMD_STRICT=1 BBNF_SIMD_FORCE=scalar cargo test -p bbnf-simd --profile ax-iter
BBNF_SIMD_STRICT=1 BBNF_SIMD_FORCE=<host tier> cargo test -p bbnf-simd --profile ax-iter
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --profile ax-iter
```

Optional but expected on x86_64 AVX-512 hosts:

```text
BBNF_SIMD_STRICT=1 BBNF_SIMD_FORCE=x86_avx512icl cargo test -p bbnf-simd --profile ax-iter --test checkasm_byte_class_from_eq_set_64
```

Optional but expected on AArch64 hosts:

```text
BBNF_SIMD_STRICT=1 BBNF_SIMD_FORCE=aarch64_neon cargo test -p bbnf-simd --profile ax-iter
```

## Stop conditions

- Do not admit raw ASM while register preservation is tested only around Rust
  closures.
- Do not admit raw ASM while stack canaries are only touched, not asserted.
- Do not admit raw ASM while signal recovery panics from the handler.
- Do not publish B/cycle from assumed clock rates.
- Do not let compile-time `target_feature` pick a public primitive without a
  force-maskable runtime dispatch row.
- Do not credit a primitive without a same-wave consumer.

## Bottom line

The next checkasm hardening wave should land infrastructure before more clever
kernels: centralized scalar-oracle calls, recoverable faults, real ABI
checked-call shims, force-maskable dispatch, disciplined cycle reporting, and
manifest-enforced same-wave consumers. That upgrades `bbnf-simd` from useful
Rust-level parity tests to dav1d/FFmpeg-style primitive admission.
