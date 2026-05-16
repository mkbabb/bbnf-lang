# SK-V6 A2: dav1d / FFmpeg / VLC ASM Process Research

Workspace: `/Users/mkbabb/Programming/bbnf-lang`

Output date: 2026-05-15

This report is research-only. No repository files were edited.

## Primary Sources Used

Local bbnf-lang sources:

- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/dispatch.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/lib.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/build.rs`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/ext/x86/bbnf.asm`
- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v5/research/skv5-A2-dav1d-process.md`
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v5/research/skv5-A6-research-ledger.md`

Upstream source snapshots:

- dav1d official git clone: `/tmp/skv6-dav1d`, commit `1cfad6dbca24fa5b4ab4853617e275281c5b8f78`
- FFmpeg official git clone: `/tmp/skv6-ffmpeg`, commit `b2867481d95b855356865632c57cd74702489b74`
- x264 official git clone: `/tmp/skv6-x264`, commit `0480cb05fa188d37ae87e8f4fd8f1aea3711f7ee`

Stable source URLs:

- dav1d checkasm driver: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/tests/checkasm/checkasm.c
- dav1d CPU feature mask: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/cpu.c
- dav1d x86 CPU detection: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/x86/cpu.c
- dav1d x86 dispatch example: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/x86/mc.h
- dav1d AArch64 CPU detection: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/arm/cpu.c
- dav1d AArch64 dispatch example: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/arm/mc.h
- dav1d AArch64 asm macros: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/arm/asm.S
- dav1d SVE2 asm example: https://code.videolan.org/videolan/dav1d/-/blob/1cfad6dbca24fa5b4ab4853617e275281c5b8f78/src/arm/64/mc16_sve.S
- FFmpeg checkasm API: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/tests/checkasm/checkasm.h
- FFmpeg checkasm driver: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/tests/checkasm/checkasm.c
- FFmpeg x86 checked-call shim: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/tests/checkasm/x86/checkasm.asm
- FFmpeg AArch64 checked-call shim: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/tests/checkasm/aarch64/checkasm.S
- FFmpeg x86inc: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/libavutil/x86/x86inc.asm
- FFmpeg AArch64 asm macros: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/libavutil/aarch64/asm.S
- FFmpeg AArch64 CPU detection: https://git.ffmpeg.org/gitweb/ffmpeg.git/blob/b2867481d95b855356865632c57cd74702489b74:/libavutil/aarch64/cpu.c
- x264 VideoLAN x86inc lineage: https://code.videolan.org/videolan/x264/-/blob/0480cb05fa188d37ae87e8f4fd8f1aea3711f7ee/common/x86/x86inc.asm

## Executive Takeaways

1. Upstream checkasm is an admission system, not a loose differential test. It combines scalar reference calls, candidate calls, forced CPU-mask runs, crash recovery, ABI register preservation checks, stack clobber probes, padded/aligned memory comparison, and controlled benchmark sampling.

2. Assembly dispatch is table-oriented. dav1d initializes a function-pointer context once, then progressively overrides scalar or lower-tier entries with SSSE3/SSE4.1/AVX2/AVX-512ICL, or NEON/DOTPROD/I8MM/SVE2 entries. The runtime path should not repeatedly test ISA features at each primitive call.

3. Register-clobber testing must happen at the real ABI boundary. FFmpeg's checkasm shims call raw function pointers after seeding callee-saved registers and stack sentinels. bbnf-simd's current AArch64 register sentinel around a Rust closure is not a sound equivalent and should be removed from Rust-closure testing.

4. AArch64 extension discipline is stricter than "put NEON in `.S`". dav1d and FFmpeg explicitly disable optional extensions by default and enable DOTPROD, I8MM, SVE, or SVE2 only inside the files/regions that need those instructions.

5. x86inc discipline is the right local foundation. bbnf-simd already vendors x86inc and has `ext/x86/bbnf.asm`; the next step is to make every raw ASM body declare its ABI shape, feature suffix, exact register counts, and checkasm admission row before a consumer can select it.

6. No new BBNF directives are needed. SIMD/ASM eligibility should remain an internal lowering and cost-model consequence of existing layout facts, backend shape, primitive contracts, and target feature masks.

## Upstream Process Learnings

### dav1d checkasm

dav1d's checkasm entry point has a fixed test registry and a CPU flag table that includes x86 SSE2/SSSE3/SSE4.1/AVX2/AVX-512ICL and AArch64 NEON/DOTPROD/I8MM/SVE/SVE2. Before running tests it initializes CPU flags, applies the optional mask through `dav1d_set_cpu_flags_mask`, and passes the resulting feature state into `checkasm_main`.

The important process point is that candidate coverage is exercised under feature masks. A host that supports AVX-512ICL can still run lower-tier code by forcing masks. This catches cases where a lower implementation silently rots after the fastest path starts passing.

Representative dav1d test style:

- Clone mutable inputs before `call_ref` and `call_new`.
- Compare return values and full mutated state, not just the direct return.
- Fill source padding and edge regions deliberately when the primitive may legally read beyond visible pixels.
- Bench only after correctness passes.
- Alternate buffers during bench loops to avoid measuring cache artifacts as correctness.

For bbnf-simd, the direct translation is: every primitive check must name the scalar executable spec, run the selected candidate under forced dispatch masks, compare all mutated outputs and guard regions, and benchmark only admitted candidates.

### FFmpeg checkasm

FFmpeg checkasm is the strongest model for ABI safety.

The public checkasm macros (`check_func`, `declare_func`, `call_ref`, `call_new`, `bench`) hide a hard boundary: on x86 and AArch64, `call_new` routes through architecture-specific checked-call assembly. These shims seed callee-saved registers with sentinels, poison temporary argument registers, create stack canaries, call the raw function pointer, then verify register and stack preservation before returning.

FFmpeg also has signal recovery in the checkasm driver. Faulting candidate code is reported as a failed test instead of aborting the entire test binary. The local bbnf-simd signal handler currently panics from a signal handler in one path; that is not equivalent to FFmpeg's recovery design and should not be the final gate for unsafe ASM.

FFmpeg's benchmark macro also avoids naive timing. It repeats calls, rejects outliers through a simple robust filter, and records bench results only for functions that passed correctness.

### x86inc / VideoLAN macro discipline

x86inc exists to make raw assembly behave like a constrained ABI DSL:

- `cglobal` defines exported symbol naming, argument count, GPR use, vector register use, and stack reservation.
- `INIT_XMM`, `INIT_YMM`, and `INIT_ZMM` select the vector width and feature suffix for the following body.
- `RET`/`REP_RET` centralize function epilogue behavior.
- cpuflag hierarchy is encoded once, so an AVX2 or AVX-512 body carries a consistent name and feature identity.

bbnf-simd should keep using x86inc, but treat the macro header as an enforceable contract:

- Every NASM primitive body declares exact `nargs`, `gpregs`, vector register count, and stack bytes.
- Every function has a suffix that matches dispatch features, such as `_avx2` or `_avx512icl`.
- Every body terminates through x86inc `RET`.
- Untrusted source loads use unaligned-safe forms unless the primitive contract proves alignment.
- Jump tables belong inside a specific admitted primitive body, not in grammar-level dispatch.

### CPU dispatch

dav1d dispatch uses monotonic function-table override:

1. Initialize a context to scalar or baseline functions.
2. If SSSE3/NEON exists, replace eligible entries.
3. If a stronger feature exists, replace only the entries that benefit and are safe.
4. Keep exceptions local, such as dav1d's slow-gather policy for AVX-512 gather-sensitive functions.

This is different from scattering `is_x86_feature_detected!` inside public wrapper functions. bbnf-simd should select a `PrimitiveKernels` table once, then call function pointers.

### AArch64 NEON / DOTPROD / I8MM / SVE / SVE2 authoring

dav1d and FFmpeg both define AArch64 macro layers that disable optional extensions by default. A file or region that needs SVE/SVE2 explicitly enables it. This catches accidental instruction creep at assembly time.

Recommended bbnf-simd pattern:

- Keep NEON, DOTPROD/I8MM, and SVE/SVE2 in distinct files or clearly delimited regions.
- Use suffixes such as `_neon`, `_neon_dotprod`, `_neon_i8mm`, and `_sve2`.
- Gate compilation with target-feature configuration and gate dispatch with runtime HWCAP feature detection.
- If SVE vector length matters, query it explicitly, as FFmpeg does with `cntb`; do not assume a fixed scalable width.
- If fixed 128-bit behavior is intended inside SVE2 code, make that explicit with predication such as fixed `ptrue` patterns and test it under the SVE2 forced mask.

### x86 AVX2 / AVX-512 authoring

dav1d keeps AVX2 and AVX-512 implementations in separate x86 assembly files with feature-specific `INIT_*` setup and symbol suffixes. AVX-512 is not treated as a blanket replacement for every AVX2 routine; some entries are overridden only when the CPU feature set and microarchitecture policy are acceptable.

For bbnf-simd:

- AVX2 bodies should use `_avx2` suffixes and only require the feature bits actually used.
- AVX-512 bodies should name the concrete tier, probably `_avx512icl` when relying on the Ice Lake feature bundle rather than plain AVX-512F.
- Feature groups should include OS-enabled XMM/YMM/ZMM/opmask state, not only CPUID instruction bits. Rust's detection helpers cover OS enablement, but the local dispatch table still needs a named feature group and test matrix.
- Gather-using or latency-sensitive AVX-512 code needs an explicit microarchitecture policy before admission.

## Local bbnf-simd State

### Strong existing pieces

bbnf-simd already has several important pieces in place:

- `CHECKASM-REPORT.md` documents a parity harness modeled on FFmpeg checkasm.
- Primitive tests clone source buffers, compare reference/candidate outputs, and include alignment and adversarial sweeps for several primitives.
- `checkasm_byte_class_from_eq_set_64.rs` is the strongest current gate: it includes signal guarding, alignment sweeps, set-size sweeps, adversarial seeds, corpus parity, empty sets, duplicate entries, and tail padding.
- `ext/x86/bbnf.asm` defines a grammar-neutral x86 macro contract layer for current and planned primitives.
- `build.rs` compiles NASM sources on x86_64 and can disable ASM through `BBNF_SIMD_DISABLE_ASM`.
- `REDRESS.md` already records the same-wave consumer rule and blocks orphan primitives such as `BULK_EMIT_COMPRESSED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`, and `FSM_DISPATCH_THREADED`.

### Current gaps

1. `tests/checkasm_parity.rs` installs a signal handler that panics from the handler. That is not async-signal-safe and does not match FFmpeg's checked failure recovery.

2. `tests/checkasm_parity.rs` has a local `stack_clobber_then` that touches a canary with volatile operations but does not assert it after the candidate. `tests/checkasm_common.rs` has a stronger `guarded_call`, but neither one performs FFmpeg-style below-stack clobbering or ABI-boundary register validation.

3. `tests/checkasm_common.rs` includes `callee_saved_register_then` around a Rust closure on AArch64. That should not be used as evidence that arbitrary Rust code preserved the raw ASM ABI. It must be reserved for explicit extern function-pointer calls through a shim.

4. `src/dispatch.rs` currently selects NEON kernels unconditionally on AArch64 targets. Baseline NEON is normal for AArch64, but the pattern will not scale to DOTPROD, I8MM, SVE, or SVE2. Runtime feature tables should be introduced before adding optional AArch64 variants.

5. `src/lib.rs` selects the AVX-512 byte-class primitive through compile-time `target_feature=avx512bw`. That bypasses the upstream-style runtime feature table and forced-mask testing model.

6. Checkasm helpers are not centralized enough. Signal guards, stack guards, buffer guard setup, robust timing, and dispatch forcing should live in one test support module so every primitive gate has the same safety envelope.

7. Some primitives have good randomized tests but still need exhaustive boundary rows and destination guard checks that match their memory-write contracts.

## Exact Checkasm Hardening Changes

### 1. Add real checked-call shims

Add test-only checked-call assembly shims, modeled on FFmpeg:

- `tests/checkasm_ffi_x86_64.asm`
- `tests/checkasm_ffi_aarch64.S`

Required behavior:

- Accept a raw `extern "C"` candidate function pointer and marshalled arguments.
- Seed callee-saved registers with sentinel values before calling the candidate.
- On System V x86_64, verify callee-saved GPRs. If Windows x64 is supported later, verify XMM6-XMM15 there.
- On AArch64, verify x19-x28 and d8-d15.
- Install a stack canary covering stack-passed arguments and verify it after return.
- Return an error code or structured failure record; do not panic inside the shim.

Use these shims only for raw ASM/FFI candidates. Keep Rust scalar references on ordinary Rust calls.

### 2. Retire register-sentinel Rust closures

Replace `callee_saved_register_then` with an `asm_candidate_call` helper that only accepts explicit extern function pointers. The local redress note already identifies the closure-based sentinel as unsound; the fix is to stop treating it as checkasm evidence.

### 3. Replace signal panic with recoverable fault handling

Replace panic-from-signal-handler paths with a Unix recovery trampoline:

- Use `sigaction`, not process-global `signal`, for SIGSEGV, SIGBUS, SIGILL, and SIGFPE.
- Keep the handler async-signal-safe.
- Use a small C/ASM `sigsetjmp` / `siglongjmp` wrapper or equivalent test-only native shim to return control to the harness.
- Report the failing primitive, forced CPU mask, seed, and alignment case.

Until this exists, mark crash isolation as partial, not FFmpeg-equivalent.

### 4. Centralize common checkasm helpers

Move all shared harness behavior into `tests/checkasm_common.rs`:

- deterministic RNG and seed logging
- source/destination guard allocation
- alignment-window generation
- stack guard wrapper
- recoverable signal wrapper
- checked-call shim wrappers
- robust timing sampler
- forced feature-mask runner

Primitive test files should then contain only primitive-specific input generation and invariants.

### 5. Add forced dispatch matrix

Introduce a test-only feature mask API equivalent to dav1d's `dav1d_set_cpu_flags_mask`:

- `scalar`
- `neon`
- `dotprod`
- `i8mm`
- `sve`
- `sve2`
- `swar`
- `avx2`
- `avx512icl`

Suggested environment controls:

- `BBNF_SIMD_FORCE=scalar|neon|dotprod|i8mm|sve|sve2|swar|avx2|avx512icl`
- `BBNF_SIMD_MASK=...` for disabling a subset on capable hosts

Every primitive checkasm test should run:

1. scalar reference against scalar candidate
2. scalar reference against each host-supported forced tier
3. scalar reference against the default dispatch table

The scalar executable spec remains the oracle for every row.

### 6. Make dispatch table-first

Refactor `src/dispatch.rs` and public wrappers toward an upstream-style table:

- Build `PrimitiveKernels` once with `OnceLock`.
- Start with scalar functions.
- Override eligible entries when runtime feature groups are present.
- Keep optional feature families separate from baseline AArch64 NEON.
- Avoid compile-time-only AVX-512 selection in public wrappers.
- Add a test-only setter or mask hook so checkasm can force lower tiers.

This applies immediately to `byte_class_from_eq_set_64`; it should not select AVX-512 only because the crate was compiled with `target_feature=avx512bw`.

### 7. Use admission rows for every raw primitive

Add a test/doc-only manifest row for each primitive. This is not a BBNF directive; it is a local admission record.

Fields:

- primitive name
- scalar reference path
- raw ASM symbol names
- required CPU features
- checkasm test modules
- forced-mask cases
- ABI checked-call coverage
- same-wave runtime consumer
- benchmark key, if admitted for performance reporting
- status: `candidate`, `admitted`, `blocked_no_consumer`, or `retired`

The manifest should fail CI for any `admitted` raw ASM primitive without scalar reference, checkasm gate, feature mask row, and consumer evidence.

## Exact Primitive Hardening Changes

### BYTE_CLASS_FROM_EQ_SET_64

Current status: strongest local primitive gate.

Keep:

- empty set
- duplicate set entries
- set-size sweep
- alignment sweep
- adversarial seeds
- corpus parity
- tail padding checks

Add:

- checked-call coverage for the AVX-512 extern symbol once linked in tests
- forced scalar/default/AVX-512ICL dispatch rows
- explicit `set_len > 8` behavior at the safe wrapper boundary, either reject or document unreachable construction
- destination pre/post guard sentinels around the 64-byte output window

### BYTE_CLASS_FROM_TABLE_64

Add exact density rows:

- all zero table
- all one table
- singleton byte
- every 2nd, 3rd, 5th, and 17th byte
- high-half-only and low-half-only sets
- randomized dense and sparse tables

Run all rows under scalar, NEON/TBL if present, and any future x86 table-lookup tier.

### BITMAP_PREFIX_XOR_64

Add exhaustive or sampled carry-state rows:

- mask `0`
- mask `u64::MAX`
- alternating bit patterns
- single-bit masks for every bit index 0..63
- incoming carry false/true
- tail handoff cases from string/escape scanning

This is the class that previously exposed NEON escape-mask/tail-handoff risk, so it should stay adversarial rather than only random.

### BITMAP_NEXT_SET_BIT

Add exhaustive cursor/mask boundary rows:

- cursor 0..64 against mask `0`
- cursor 0..64 against mask `u64::MAX`
- single low bit
- single high bit
- cursor exactly on a set bit
- cursor immediately after the last set bit
- random masks with cursor near 0, 31, 32, 63, and 64

### BULK_EMIT_POSITIONS_64

Current status: later admitted with structural+terminator work.

Add:

- destination prefix/suffix sentinels
- capacity exactly equal to popcount
- capacity one less than popcount, if the contract supports bounded output
- zero mask
- all-ones mask
- high base values near `u32::MAX`, with overflow behavior specified and tested
- forced dispatch rows for every emitted backend

### EOB_PAD_CLAMP

Keep dead-lane zeroing checks.

Add:

- exhaustive length rows at 0, 1, 2, 15, 16, 31, 32, 63, 64, and any contract-permitted boundary above 64
- input sentinel lanes that prove inactive/dead output lanes are not leaked
- aliasing row if source and destination may overlap

### STRUCTURAL_TERMINATOR_64

Add:

- terminator byte also classified as structural
- terminator byte inside and outside quoted regions
- no-quote fast-path corpus rows
- terminator at byte 0 and byte 63
- no terminator present
- forced dispatch rows for scalar and SIMD variants

### UTF8_BLOCK / unescape-related primitives

Before admitting more UTF-8 or `\uXXXX` SIMD:

- test split multi-byte sequences across 64-byte boundaries
- test overlong encodings
- test surrogate halves and surrogate pairs
- test noncharacters according to the language policy
- test truncated continuation tails
- compare both accept/reject result and consumed-byte state

### Blocked orphan primitives

Keep blocked until real same-wave consumers exist:

- `BULK_EMIT_COMPRESSED`
- `FRAME_PUSH_BOUNDED`
- `FRAME_POP_BOUNDED`
- `FSM_DISPATCH_THREADED`

Do not admit them on benchmark potential alone.

## Assembly Authoring Rules To Adopt

### x86

1. Include the vendored x86inc layer and `ext/x86/bbnf.asm`.
2. Use exactly one `INIT_XMM`, `INIT_YMM`, or `INIT_ZMM` region per function family unless the file intentionally emits multiple variants.
3. Declare every public body with `cglobal name, nargs, gpregs, xmmregs, stack`.
4. Use feature suffixes that match dispatch: `_swar`, `_ssse3`, `_avx2`, `_avx512icl`.
5. Use `RET`, not a bare epilogue.
6. Prefer unaligned-safe loads for caller-provided bytes.
7. Keep grammar-specific tables in generated data, not in the macro library.
8. Require a checked-call row before any raw x86 ASM body can be marked admitted.

### AArch64

1. Add a local AArch64 asm macro layer equivalent in spirit to dav1d/FFmpeg `asm.S`.
2. Disable optional extensions by default.
3. Enable DOTPROD, I8MM, SVE, and SVE2 only around the code that uses them.
4. Use symbol suffixes that encode the feature family.
5. Keep scalable-vector code separate from fixed-width NEON code.
6. Add PAC/BTI-compatible entry macros if any indirect-call targets are emitted.
7. Require checked-call coverage for x19-x28, d8-d15, and stack sentinels before admission.

## Fold Into skinny/global Specs Without New Directives

No `@runtime`, `@simd`, `@backend`, `@shape`, or `@asm` directive is needed. SIMD and ASM remain internal lowering consequences.

### `restart/skinny/WORKSPACE.md`

Add a gate stating that any raw ASM primitive must have:

- scalar executable spec
- per-ISA symbol names
- runtime feature mask row
- forced-mask checkasm run
- ABI checked-call coverage
- same-wave runtime consumer

This is a workspace engineering gate, not user-facing grammar.

### `restart/skinny/COMPILER.md`

Describe ASM selection as part of the existing backend shape and cost model:

- `LayoutFacts.backend_shape` determines whether a primitive family is legal.
- The cost model selects from admitted primitive families only.
- If no admitted body exists for the selected target, lowering falls back to scalar or emits the existing not-viable diagnostic path.
- Per-call feature detection is forbidden in generated hot loops; lowering calls a selected table entry.

### `restart/skinny/SUBSTRATE.md`

Extend primitive admission language:

- A primitive is admissible only with a scalar reference, checkasm gate, feature-mask matrix, and consumer.
- Structural projection remains a tape/direct projection leaf, not a sidecar representation.
- Assembly cannot introduce hidden state outside declared primitive inputs/outputs.

### `restart/skinny/BENCH.md`

Require benchmark rows to disclose:

- CPU model
- OS/target triple
- selected feature tier
- forced feature mask, if any
- scalar parity/checkasm status
- primitive admission status

Checkasm bench numbers should be treated as admission diagnostics. End-to-end benchmark credit should come only after the primitive is wired through a real consumer.

### `restart/locks/LOCKS.md`

Extend Lock 16 with three concrete requirements:

- raw ASM must pass an ABI checked-call shim
- CPU dispatch must be force-mask testable
- optional AArch64 and x86 feature tiers must be extension-gated and recorded in the primitive admission row

### Global architecture / master plan

Fold this into `CollapsedStage` and backend planning:

- A collapsed stage may choose a SIMD/ASM primitive only from admitted primitive rows.
- ISA selection is internal target planning derived from layout facts and feature masks.
- Missing ASM is never a grammar semantic difference; it is either scalar fallback or a backend viability diagnostic.
- Same-wave consumer evidence remains mandatory so the project does not accumulate orphan intrinsics.

## Proposed Immediate Work Order

1. Introduce test-only feature masks and refactor bbnf-simd dispatch to table-first scalar/default selection.
2. Replace compile-time-only AVX-512 selection for `BYTE_CLASS_FROM_EQ_SET_64` with runtime table dispatch.
3. Add recoverable signal handling and centralize checkasm helpers.
4. Add x86_64 and AArch64 checked-call shims for raw extern candidates.
5. Move AArch64 register-sentinel coverage out of Rust closure wrappers.
6. Create the primitive admission manifest and mark current rows honestly.
7. Harden `BULK_EMIT_POSITIONS_64`, `BITMAP_PREFIX_XOR_64`, and `STRUCTURAL_TERMINATOR_64` first because they are closest to structural scanning consumers.

## Bottom Line

The dav1d/FFmpeg/VLC process is not "write clever assembly, then benchmark it." It is:

1. write scalar executable truth,
2. expose raw candidates through narrow ABI contracts,
3. force every CPU tier through checkasm,
4. verify memory, stack, registers, and crash behavior,
5. dispatch through initialized tables,
6. admit only primitives with real consumers.

bbnf-simd is already close in structure, especially on x86inc adoption and primitive parity tests. The highest-value SK-V6 change is to upgrade the harness from Rust-level parity checks to upstream-style ABI and feature-mask admission, while keeping all SIMD/ASM decisions inside existing skinny/backend specs rather than adding new grammar directives.
