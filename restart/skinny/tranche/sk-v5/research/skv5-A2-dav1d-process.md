; SK-V5 A2 — DAV1D / FFmpeg / VLC ASM SIMD process discipline as it applies to bbnf-simd
;
; Scope: the audit-grade walk-through of x86inc.asm (Layer 0, vendored), x86util.asm
; (the video-specific reusable patterns), checkasm (the differential gate), and the
; per-codec data-vs-code separation that lets dav1d ship one set of primitives across
; eight bit-depth × chroma × prediction-mode permutations without code duplication.
; The closing section maps every discipline onto the eight unlanded bbnf.asm
; primitives so the next wave can land their bodies under a single, codified shape.
;
; Sources: skinny/crates/bbnf-simd/ext/x86/x86inc.asm (1,978 LOC), x86util.asm
; (1,036 LOC), bbnf.asm (485 LOC, contract-only); checkasm_byte_class_from_eq_set_64.rs
; (533 LOC, the working per-primitive harness); checkasm_parity.rs (775 LOC, the
; whole-classifier harness); CHECKASM-REPORT.md; FFmpeg checkasm.h reference
; (https://www.ffmpeg.org/doxygen/7.1/checkasm_8h.html). All file:line citations
; resolve under skinny/crates/bbnf-simd/ unless otherwise stated.

# 1. Layer 0 — x86inc.asm full inventory

x86inc.asm is the x264 / x265 / dav1d / VLC shared ASM-abstraction layer. It is
2005-vintage, BSD-2, and the single most reused .asm file in the open-source SIMD
world. It does seven things that no hand-written .asm primitive should ever try
to re-invent. The vendored copy at `ext/x86/x86inc.asm` is verbatim (cf. the
license header at `ext/x86/x86inc.asm:1-22`).

## 1.1 Width macros: INIT_XMM / INIT_YMM / INIT_ZMM (`x86inc.asm:1097-1145`)

Each width macro re-binds the meta-register `m0..m31` to the corresponding
physical width, swaps the width-aware aliases `mova`, `movu`, `movh`, `movnta`,
and writes the broadcast suffixes `bcstw`, `bcstd`, `bcstq` so the same source
text expands to xmm/ymm/zmm bodies via three different headers. `INIT_ZMM` at
`x86inc.asm:1131` is the one bbnf-simd will use most: it sets `mmsize=64`,
binds `m0..m31` to `zmm0..zmm31`, and pre-declares `bcstq 1to8` for the
512-bit broadcast suffix.

The discipline is: every primitive body opens with one `INIT_ZMM avx512` (or
the appropriate width × cpuflag combination) before `cglobal`. The
`avx512icl` cpuflag string (`x86inc.asm:967`) covers AVX-512 F+CD+BW+DQ+VL
plus VNNI/IFMA/VBMI/VBMI2/VPOPCNTDQ/BITALG/VAES/VPCLMULQDQ — exactly the
Ice-Lake-X+ / Zen-4+ baseline bbnf primitives target. Use `avx512` for the
sub-icl rump (BW-only fallbacks).

## 1.2 Function entry: cglobal / cextern / cvisible (`x86inc.asm:820-888`)

`cglobal name, n_args, n_regs, n_xmm, [stack_size,] arg_names...` is the
function header. It:

  - applies private_prefix mangling via `mangle()` (`x86inc.asm:79-83`);
  - emits `global name:function hidden` on ELF (so the symbol does not
    participate in dynamic-linker rebinding — every bbnf primitive is a leaf);
  - resets `stack_offset = 0`, `xmm_regs_used = 0`, `rstk = rsp`;
  - dispatches into `PROLOGUE` (`x86inc.asm:500-521` WIN64, `:641-659` SysV64,
    `:699-723` x86_32) which pushes the callee-saved registers required by
    the ABI, performs `WIN64_SPILL_XMM` on Windows when `n_xmm > 6`, allocates
    `stack_size` aligned to `required_stack_alignment`, and loads argument N
    into `rN` if it lives in memory.

bbnf-simd already uses this verbatim — `src/x86_64/byte_class_from_eq_set_64.asm:49-50`
opens with `INIT_ZMM avx512` and `cglobal byte_class_from_eq_set_64_avx512, 3, 6, 9, src, set_ptr, set_len`,
which after PROLOGUE expansion gives us `srcq=rdi`, `set_ptrq=rsi`, `set_lenq=rdx`,
six GPRs claimed (none beyond the three SysV arg regs really needed, so this
is conservative — see §6 for the tightening discussion), nine ZMMs claimed
(zmm0 source + zmm1..zmm8 set-member broadcasts), no stack allocated.

`cextern foo` (`x86inc.asm:875-879`) declares an externally-defined symbol
with private_prefix mangling. Used for cross-file LUT references; bbnf-simd
will use this from codegen-emitted per-grammar `.data` tables that need to
reference grammar-neutral helper LUTs in `bbnf.asm`.

`cvisible name, …` (`x86inc.asm:823-825`) is identical to cglobal but
without the `hidden` ELF visibility — exported across the dylib boundary.
bbnf-simd's Rust FFI wrappers go through `cglobal` (hidden) plus a Rust
`extern "C"` declaration that picks up the mangled symbol; `cvisible` is not
expected to be needed.

## 1.3 ABI handling: WIN64 / SysV64 / x86_32 (`x86inc.asm:482-742`)

x86inc factors the three calling conventions through a single `PROLOGUE`
macro. The DECLARE_REG sequences at `x86inc.asm:484-498` (WIN64) and
`:625-639` (SysV64) bind the arg-N → physical-register mapping; the meta
registers `r0..r14` are stable across the two ABIs even though the underlying
physical registers differ. WIN64 uses rcx/rdx/r8/r9 for the first four args
plus shadow space (`x86inc.asm:425`, the `%%pad + 32` term); SysV64 uses
rdi/rsi/rdx/rcx/r8/r9 with no shadow space.

The PROLOGUE asserts `regs_used >= num_args` (`x86inc.asm:503` WIN64,
`:644` SysV) so the macro never silently truncates the arg list. The
`PUSH_IF_USED` / `POP_IF_USED` chains (`x86inc.asm:275-291`) handle the
callee-saved subset — WIN64 saves r7..r14 (which after DECLARE_REG resolves to
rdi/rsi/rbx/rbp/r14/r15/r12/r13), SysV64 saves r9..r14 (rbx/rbp/r14/r15/r12/r13).

## 1.4 WIN64_SPILL_XMM and xmm_regs_used tracking (`x86inc.asm:525-579`)

On Windows the callee must save xmm6..xmm15 if it uses them. `WIN64_SPILL_XMM
n_xmm` allocates `(n_xmm - 8) * 16 + 32` bytes of stack (the +32 is shadow
space) and pushes xmm6..xmm15 as appropriate via `movaps`. On SysV the macro
is a no-op stub (`x86inc.asm:744-758`). `xmm_regs_used` is an `%assign`-tracked
counter — every primitive must declare its peak XMM usage so the stub correctly
sizes the stack frame on Windows. bbnf-simd's `cglobal …, n_xmm=9` is the
declaration; on SysV the 9 is informational.

## 1.5 Width-aware move aliases: mova / movu / movh / movnta

These rebind on every INIT_*:

| INIT  | mova    | movu    | movh    | movnta  |
|-------|---------|---------|---------|---------|
| MMX   | movq    | movq    | movd    | movntq  |
| XMM   | movdqa  | movdqu  | movq    | movntdq |
| YMM   | movdqa  | movdqu  | (undef) | movntdq |
| ZMM   | movdqa  | movdqu  | (undef) | movntdq |

`movh` is the low-half load — used only at xmm/mmx widths because at ymm/zmm
the half-load instruction class is different. bbnf primitives should prefer
`movu` for inputs (over-allocation upstream guarantees alignment-tolerance) and
`mova` only when an alignment invariant is statically provable; the cost of a
single misaligned `vmovdqu` on AVX-512 hosts is zero per-cycle on Skylake-X+
when the address actually is 64-byte aligned.

## 1.6 RET — context-aware return (`x86inc.asm:614-621` WIN64, `:663-676` SysV64)

`RET` is the single most under-appreciated macro in x86inc. It:

  1. emits `WIN64_RESTORE_XMM_INTERNAL` on Windows (pops every spilled xmm6+);
  2. emits `POP_IF_USED` to mirror the PROLOGUE pushes;
  3. emits `vzeroupper` if `vzeroupper_required` (`x86inc.asm:374`) — true when
     `mmsize > 16` AND the host either isn't 64-bit or uses xmm_regs_used > 16
     or lacks AVX-512 (which fixes the avoidable SSE/AVX transition penalty);
  4. emits `AUTO_REP_RET` (`x86inc.asm:775-781`) — on K10-class AMD CPUs, a bare
     `ret` immediately following a branch target costs cycles; the macro emits
     `rep ret` only when needed.

The discipline is: every bbnf primitive ends with a single `RET` and zero
hand-rolled stack/restore logic. The current `byte_class_from_eq_set_64.asm:105`
follows this exactly.

## 1.7 PIC / non-PIC, SECTION_RODATA, SECTION_TEXT

PIC is forced on at `x86inc.asm:107-114` (`%define PIC 1; default rel` on
x86_64 — RIP-relative addressing is universally cheaper). The `LEA` macro at
`x86inc.asm:239-249` emits `lea reg, [addr]` on x86_64 and the call/pop trick
on 32-bit non-PIC builds.

`SECTION_RODATA [alignment]` (`x86inc.asm:93-105`) emits the correct section
directive per output format (`.rdata` on Windows, `.rodata` on ELF, `.text`
on aout/coff which lack rodata). bbnf primitives' constant LUTs (the
`BYTE_CLASS_FROM_TABLE_64` 256-byte tables, the `FSM_DISPATCH_THREADED`
state-target arrays) live in SECTION_RODATA — but only the *grammar-neutral*
constants; per-grammar LUTs live in codegen-emitted .data, not in bbnf.asm
itself (Lock 14, §4).

## 1.8 The rest: REPX, PUSH/POP, LOAD_IF_USED, ASSERT, DEFINE_ARGS, BRANCH_INSTR

These are convenience macros. `REPX {psrlw x, 8}, m0, m1, m2, m3` (`x86inc.asm:253`)
expands the same instruction across N register arguments — useful for the
fan-out shape of `BYTE_CLASS_FROM_EQ_SET_64` and `BULK_EMIT_COMPRESSED`'s
prep stages. `BRANCH_INSTR` (`x86inc.asm:783-796`) hooks AUTO_REP_RET so the
right ret form is emitted after any of the standard jump mnemonics.
`DEFINE_ARGS new_name1, new_name2, …` (`x86inc.asm:338-371`) rebinds the
arg-name aliases after a primitive has consumed its initial args and wants to
reuse the same registers for different semantic roles — bbnf primitives that
loop over input chunks should DEFINE_ARGS to relabel rdi as the source cursor
once the original `src` semantics are done.

# 2. The dav1d x86util.asm layer

x86util.asm (`ext/x86/x86util.asm`, 1,036 LOC) is the video-specific
companion to x86inc. It is *not* part of the abstraction layer per se — it's a
library of higher-level patterns the dav1d (and historically the x264) kernels
share. Concretely:

  - **Transposes**: TRANSPOSE4x4B/W/D, TRANSPOSE8x8W, TRANSPOSE16x16W
    (`x86util.asm:80-279`) — every IDCT / DCT8/16/32 pass goes through one of
    these. Not relevant to bbnf-simd (parsers do not transpose), BUT the
    pattern of "small, named, macro-bodied building blocks that compose into
    larger kernels" is exactly the discipline bbnf.asm imitates.
  - **Absolute-value / sign / abs2 / abs4**: PABSW, PSIGNW, ABS1, ABS2, ABS4
    (`x86util.asm:297-399`) — DCT inverse-coefficient handling. Not relevant.
  - **PALIGNR emulation**: PALIGNR macro at `x86util.asm:471-499`. The SSSE3
    instruction emulated on SSE2 via shuffle-OR. THE PATTERN: when the target
    ISA admits a native instruction, the macro dispatches to it; on lower
    baselines it composes a multi-instruction equivalent. bbnf.asm should
    use this pattern for AVX-512-VBMI's `vpermb` vs AVX-512-BW-only's
    `vpshufb + vpor` fallback (the `BYTE_CLASS_FROM_TABLE_64` macro
    documents this at `bbnf.asm:84-92`).
  - **HADD pseudo-instructions**: HADDD, HADDW, HADDPS (`x86util.asm:422-470`)
    — horizontal reductions. bbnf primitives generally avoid horizontal
    reductions in the hot path; the reduction targets are kmask registers
    (`korq`) or population counts (`popcnt`), neither of which is HADD-shaped.
  - **SPLATB / SPLATW / SPLATD** (`x86util.asm:400-792`) — broadcast from a
    GPR or memory slot into a vector. bbnf.asm's `BYTE_CLASS_FROM_EQ_SET_64`
    uses `vpbroadcastb` directly (it's AVX-512 BW); the SPLATB macro is for
    SSE2/SSSE3 fallback construction.
  - **LOAD_DIFF / STORE_DIFF / DIFFx2** (`x86util.asm:673-746`) — diff-store
    patterns for residual-coefficient handling. Not relevant.
  - **VBROADCASTSS / VBROADCASTI128** (`x86util.asm:841-896`) — VEX-encoded
    broadcasts emulated as load+shuffle on pre-AVX hosts. Same fall-back
    pattern as PALIGNR.

The reusable pattern across all of x86util.asm: **named macros with
documented input/output register contracts, fall-back bodies for lower-ISA
baselines, no global state, no implicit clobbers beyond what the documentation
declares**. bbnf.asm is the parsing-domain equivalent (`bbnf.asm:1-45`),
following exactly this shape with nine macros instead of x86util's ~60. The
narrower count is correct: parsing has many fewer primitive operations than
video.

# 3. checkasm process — full inventory

## 3.1 The FFmpeg / dav1d checkasm.h core (per https://www.ffmpeg.org/doxygen/7.1/checkasm_8h.html)

The checkasm framework declares ten essential macros that define the per-test
shape. Below, signature first, semantics second:

  - `declare_func(ret_type, args…)` — declares a typedef `func_type` of the
    callee shape; stores the reference and candidate pointers as
    `(func_type *)func_ref`, `(func_type *)func_new`. Every per-primitive test
    expands one `declare_func` near the top.
  - `declare_func_emms(cpu_flags, ret, …)` — variant for MMX functions that
    require an EMMS issue between ref and new calls (legacy; not needed for
    AVX-512 primitives).
  - `declare_func_float(ret, …)` and `_float_emms` — variant for FP-returning
    primitives. Used by audio codec checks.
  - `check_func(func_ptr, "test_name_format", args…)` — registers `func_ptr`
    as the candidate, captures sigsetjmp context via `checkasm_save_context()`,
    sets up SIGSEGV/SIGBUS/SIGILL/SIGFPE handlers. Returns nonzero if the
    function was registered for this run (the `--test=name` filter).
  - `call_ref(args…)` — invokes the reference function with the signal handler
    armed. The call site is wrapped in `checkasm_set_signal_handler_state(1)`
    pre-call, `(0)` post-call so a crash in the *reference* implementation is
    distinguishable from a crash in the candidate.
  - `call_new(args…)` — invokes the candidate. The wrapper additionally checks
    for ABI violations: it sets known scratch values in caller-saved registers
    pre-call and verifies them post-call (the *register clobber* check) and it
    fills a stack canary pre-call and verifies it post-call (the *stack
    clobber* check). dav1d's call_new on x86_64 uses a tiny ASM shim
    (`checkasm.asm` in dav1d's tests dir) that performs this dance.
  - `bench_new(args…)` — runs the candidate inside a measurement loop with
    `rdtscp` / `mach_absolute_time` / per-OS-equivalent cycle counters, applies
    the median-of-N + 4×-outlier-rejection robust-mean filter that the dav1d
    paper calls `count*4 <= sum` (see checkasm.c).
  - `report` / `report()` — flushes the per-test pass/fail count and prints
    summary lines.
  - `fail()` — registers a divergence with file:line for the current test.
  - `randomize_buffers(dst0, dst1, size)` / `randomize_buffers2(…)` —
    fills both destination buffers with the same xorshift-derived bytes via
    `av_lfg_get(&checkasm_lfg)`, so a post-call memcmp on the destinations
    detects nondeterminism, and a separate memcmp on the *unread* tail of the
    source buffers detects scribble-back.

## 3.2 Test registration (tests[][])

Per-codec checks live in `tests/checkasm/<codec>.c` and expose a single entry
point `check_<codec>(void)`. The master table `tests[]` in
`tests/checkasm/checkasm.c` is an array of `{ "name", check_fn, REQUEST_VERSION }`
entries. `main()` walks the table, invokes each `check_<codec>`, which in turn
opens one or more `check_func()` blocks per dispatched function. The `--test=`
flag filters by `name`; `--bench=` runs the bench loop on matching primitives.

## 3.3 Signal trap (SIGSEGV / SIGBUS / SIGILL / SIGFPE)

`checkasm_save_context()` is `sigsetjmp(checkasm_context, 1)` storing the
signal mask; the signal handler `checkasm_signal_handler()` does
`siglongjmp(checkasm_context, signum)`. The trap surfaces three classes of
candidate bug:

  1. Wild pointer (SIGSEGV) — uninitialised register used as a base address;
  2. Misaligned access on a strict-alignment ISA (SIGBUS — relevant for
     AArch64 ldp with `_acq` if the target lacks unaligned-acquire);
  3. Unsupported instruction (SIGILL) — the dispatch picked a kernel whose
     ISA gate is misconfigured; the host actually lacks the feature.

bbnf-simd already implements this in `tests/checkasm_byte_class_from_eq_set_64.rs:87-114`
and `tests/checkasm_parity.rs:133-160` with the same SIGSEGV+SIGBUS+SIGILL
trio routed through `libc::signal`. The Rust pattern is structurally
equivalent — a `Once::call_once` arms the handler, the handler restores
`SIG_DFL` and panics, the panic unwinds back to the test runner.

## 3.4 Stack-clobber detection

dav1d's checkasm.asm pre-fills the stack frame below rsp with 0xDEADBEEFCAFEBABE
patterns, calls the candidate, then verifies the canaries post-call. bbnf-simd
implements the same idea in `stack_clobber_then` at
`tests/checkasm_byte_class_from_eq_set_64.rs:126-138` via a 1 KiB
`[0xDE; 1024]` array probed with `read_volatile` on entry and exit. The Rust
form is slightly weaker (it doesn't currently *verify* the canary value, only
forces the read so any segfault triggers the handler) — see §8 for the
hardening recommendation.

## 3.5 Register-clobber detection

This is the one piece of dav1d's harness that bbnf-simd currently lacks
entirely. dav1d's `call_new` shim on x86_64 sets rbx/rbp/r12/r13/r14/r15 to
known sentinel values pre-call and verifies them post-call; on AArch64 the
shim does the same for x19..x28 and d8..d15 (the AAPCS64 callee-saved set). If
the kernel violates the ABI by clobbering a callee-saved register, the
post-call comparison flags it. **bbnf-simd's Rust-side `call_new` is just a
Rust function call**, so Rust's calling convention enforces the ABI mechanically
on entry and exit — but it does not detect *intra-kernel* register-save
violations (where the ASM clobbers and then restores), which dav1d's shim
catches.

## 3.6 Bench loop discipline

dav1d's `bench_new` loop: warmup of 8 iterations, then 1024 (configurable) timed
runs, recording each `__rdtscp` delta. Outlier rejection: sort the deltas,
strip the top 1/16, take the mean. The "count×4 ≤ sum" check
(`tests/checkasm/checkasm.c` in upstream dav1d) iteratively pops the tail
violator until the sum-of-rest dominates four times the count of remaining
samples — eliminating noise from preemption/interrupts.

bbnf-simd's `classifier_bench_robust_mean` at `tests/checkasm_parity.rs`
implements the equivalent at `robust_mean_ns` — same algorithm, different
clock source (Rust's `Instant`). The substitution is acceptable for parity
gating; for absolute-cycle reporting, `__rdtsc` via `core::arch::x86_64::_rdtsc`
or `mach_absolute_time` should be wired in (§8).

## 3.7 Per-architecture test harness layout

dav1d places per-codec tests in `tests/checkasm/{<codec>.c, mc_avx2.c, mc_sse2.c}`
where the suffix names the ISA the test was authored against. The dispatch
inside the test compares the scalar reference against every dispatched ASM
variant — so on a Skylake host `mc_avx2.c` runs SSSE3+AVX+AVX2; on Ice-Lake-X
it additionally exercises the AVX-512 paths.

bbnf-simd's layout is per-architecture-first (`src/x86_64/avx2/`,
`src/x86_64/avx512_vbmi2/`, `src/aarch64/`) which is the more granular form.
The dav1d shape works because the codec is one — there are only a handful of
top-level functions. bbnf has nine primitives × multiple ISA tiers; the
per-primitive directory under per-ISA root is the right factoring.

## 3.8 Comparison: bbnf-simd current state vs the full discipline

| checkasm feature                          | bbnf-simd state                                    | Gap                                              |
|-------------------------------------------|----------------------------------------------------|--------------------------------------------------|
| `declare_func` typedef                    | not present — Rust uses direct fn pointers         | none functionally; cosmetic                      |
| `call_ref` / `call_new` symmetric         | yes; `byte_class_from_eq_set_64_scalar` vs dispatch | none                                             |
| Signal-trap SIGSEGV/SIGBUS/SIGILL         | yes (`signal_guard::arm()`)                        | none                                             |
| Stack-clobber pre-fill                    | yes (`stack_clobber_then` 1 KiB 0xDE)              | canary not verified post-call (see §8)           |
| Register-clobber check                    | **no**                                             | needs ASM shim with sentinel-pre / verify-post   |
| `randomize_buffers` identical src0/src1   | yes (`Xorshift64::fill` on two backings)           | none                                             |
| Bench cycle counter                       | wall-clock via `Instant`                           | want `__rdtsc` / `mach_absolute_time`            |
| `count*4 ≤ sum` outlier filter            | yes (`robust_mean_ns`)                             | none                                             |
| `--test=` / `--bench=` filter             | no — `cargo test` provides `--test name` only      | env-var dispatch present (`BBNF_SIMD_STRICT`)    |
| Multi-test `tests[]` registry             | implicit — one `#[test]` per primitive             | none for now; codify if test count > 20          |

# 4. Per-codec data tables vs grammar-neutral kernels

This is the load-bearing dav1d discipline. The relevant exhibit is
`src/x86/filmgrain_common.asm` (upstream dav1d): per the file-content audit
above, the file declares one `struc FGData` and one `cextern gaussian_sequence`.
**It contains zero executable code.** The per-bit-depth kernels
(`filmgrain8.asm`, `filmgrain16.asm`) `%include` `filmgrain_common.asm` to
pick up the struct layout and the external LUT pointer; the kernel code
proper, with its bit-depth-specific instruction selection, lives in the
per-depth file.

Generalising the pattern across dav1d:

  1. **Per-codec / per-format kernels** are *split* by every dimension of
     specialisation: bit-depth × chroma-subsampling × prediction-mode ×
     ISA-tier. dav1d's MC (motion compensation) directory has separate files
     for 8-bit / 16-bit × SSSE3 / AVX2 / AVX-512 × 4:2:0 / 4:2:2 / 4:4:4 ×
     PUT / PREP / W_AVG. The total file count is in the dozens; each file
     handles exactly one cell of the matrix.
  2. **The shared spine** — the macroblock dispatcher and the FGData struct
     and `gaussian_sequence` LUT — lives in *common* files. The dispatcher
     is C (not ASM); the ASM cells are leaf kernels.
  3. **The variation** is split between code (per-cell .asm bodies) and data
     (per-format LUTs in .rodata). dav1d puts the data alongside the code in
     the per-cell file when it's small (per-shift coefficient tables);
     larger or universally-shared tables go in common .asm or .rodata.

Mapped to bbnf:

  - The dav1d-equivalent of "per-codec kernel" is "per-grammar
    CollapsedStage kernel" — one .asm per grammar's collapsed DFA, emitted by
    codegen. These contain (a) the FSM-state-target table, (b) the per-state
    class predicate dispatch, (c) the per-class action emit.
  - The dav1d-equivalent of "shared spine" is bbnf.asm's nine grammar-neutral
    macros plus x86inc.asm — the primitive vocabulary every kernel reuses.
  - The dav1d-equivalent of `filmgrain_common.asm`'s `struc FGData` is the
    `OpenFrames` / `FramesBuf` stack-layout convention. This is the
    place to put a `struc BBNFParserContext` if bbnf-simd grows enough
    per-grammar variation that the context layout needs a common declaration.

The discipline that bbnf must therefore enforce, per Lock 14:

  - **NO grammar-specific code in `ext/x86/bbnf.asm`.** The file is macro
    declarations; macro bodies are grammar-neutral. JSON-isms (`{` `}` `[` `]`
    bracket-pair check) belong in the codegen-emitted per-grammar kernel
    file, NOT in a `BRACKET_VALIDATE_JSON` macro.
  - **NO grammar-specific code in `crates/bbnf-simd/src/`.** The Rust FFI shims
    in `src/x86_64/<primitive>.rs` and `src/aarch64/<primitive>.rs` are
    grammar-neutral wrappers around grammar-neutral kernels. Grammar specifics
    (the actual class predicate, the actual FSM state table) arrive as `.rodata`
    pointers passed in at call time.
  - **Codegen emits per-grammar `.asm` and `.rodata`** into the build artifact
    directory; these are linked alongside bbnf-simd's static archive. Each
    emitted file `%include`s `bbnf.asm` and `x86inc.asm` to pick up the
    Layer-0+1 vocabulary, then defines its per-grammar dispatch.

# 5. The no-orphan-kernel rule

Per `docs/precepts/instructions/LESSONS-LEARNED.md:17-26` (the same-wave
producer-consumer pairing rule, restated by the V9.5 amendment Lock 1
"substrate union" lesson): **a primitive that has no consumer is removed**.
The dav1d corollary is that `filmgrain_common.asm`'s `struc FGData` exists
because `filmgrain8.asm` and `filmgrain16.asm` use it; an orphan struct
declaration would be dead weight in the .rodata segment and worse, a vector
for confusing future contributors.

The bbnf restatement (`restart/ARCHITECTURE.md:1095`):

  > A primitive lands in bbnf.asm only when at least one shape consumes it
  > through codegen at the same wave.

For the eight unlanded primitives, the consumer pairing is:

| Primitive                  | Same-wave consumer                                     |
|----------------------------|--------------------------------------------------------|
| BYTE_CLASS_FROM_TABLE_64   | per-grammar classifier with class predicates >8 chars  |
| BITMAP_PREFIX_XOR_64       | string-region detection (JSON / CSS string literals)   |
| BITMAP_NEXT_SET_BIT        | dispatch driver (every grammar with scan-emit shape)   |
| BULK_EMIT_COMPRESSED       | tape builder (structural offsets, string body extract) |
| EOB_PAD_CLAMP              | every grammar's last-stripe handler                    |
| FSM_DISPATCH_THREADED      | CollapsedStage kernels (FSM-collapsing grammars only)  |
| FRAME_PUSH_BOUNDED         | bracket-stack grammars (JSON object/array, CSS blocks) |
| FRAME_POP_BOUNDED          | same as FRAME_PUSH_BOUNDED                             |

Every primitive on this list has a real consumer. The wave-by-wave landing
plan (§9) constrains each primitive's body to land in the same wave as its
first consumer.

# 6. ABI / register / clobber discipline

What every bbnf-simd primitive must declare in its `bbnf.asm` macro
contract block, and what its body must enforce:

## 6.1 Inputs

SysV64 (the bbnf-simd target ABI; Windows is a follow-on tier):

  - `rdi` = arg 0 (pointer-typed args first: source buffer, LUT pointer)
  - `rsi` = arg 1 (often the second pointer, or a length)
  - `rdx` = arg 2 (length, or the third pointer)
  - `rcx, r8, r9` = args 3, 4, 5 (counts, sentinels, mode bits)
  - Args ≥ 7 spill onto the stack at `[rsp + 8 + 8*N]` — bbnf primitives
    should accept ≤ 6 args to avoid stack-arg complexity.

x86inc binds these to `r0q..r5q` (`x86inc.asm:625-639`); the named-arg form
via `cglobal name, n, regs, xmm, arg_names` is preferred for readability —
`srcq`, `set_ptrq`, `set_lenq` per `byte_class_from_eq_set_64.asm:50`.

## 6.2 Outputs

  - `rax` = primary scalar return (popcounts, offsets, kmask materialisations
    via `kmovq`). Bbnf's `byte_class_from_eq_set_64` returns via `rax`.
  - `k0..k7` = AVX-512 mask outputs — but k0 is reserved as the all-ones mask
    in some instruction encodings; use `k1..k7`. The `BYTE_CLASS_FROM_TABLE_64`
    and `BYTE_CLASS_FROM_EQ_SET_64` macros at `bbnf.asm:76,113` declare `k1` as
    the output by convention.
  - `zmm0..zmm31` = vector returns — for primitives that emit a vector
    (e.g. `EOB_PAD_CLAMP` which loads a padded 64B chunk). Bbnf convention:
    output ZMMs are zmm0+ in sequence; `bbnf.asm:278` shows the EOB shape.

## 6.3 Clobbers

Every primitive's `bbnf.asm` block enumerates its clobbers explicitly. The
inventory at `bbnf.asm:79-82, 117-119, 161-162, 198-200, 237-240, 285-289`
already does this; extend the same pattern for the eight remaining bodies. The
discipline is:

  - **Caller-saved scratch is fair game.** rax, rcx, rdx, rsi, rdi (when not
    arg), r8..r11, xmm0..xmm5 on SysV; rax, rcx, rdx, r8..r11, xmm0..xmm5 on
    WIN64. Use freely; document in the contract.
  - **Callee-saved is forbidden in leaf primitives.** rbx, rbp, r12..r15,
    xmm6..xmm15 on WIN64. If a primitive *must* use them, declare via cglobal's
    `n_regs` parameter so PROLOGUE pushes them and RET pops them.
  - **Mask registers k1..k7 are caller-saved.** Document which the primitive
    writes (the output) and which it scratches (intermediates).
  - **Flags are clobbered.** Every cmp/test/inc/dec writes EFLAGS; do not pass
    a flag-conditional state across a primitive boundary.

## 6.4 Stack alignment

`x86inc.asm:373` defines `required_stack_alignment = ((mmsize + 15) & ~15)`
— 16 for XMM, 16 for YMM (! — AVX256 doesn't need 32B-aligned stack since
load/store can be unaligned), 64 for ZMM (! — vmovdqa64 requires 64B
alignment when used with a register operand). bbnf primitives that use
`vmovdqa64` on a stack temporary need to declare an aligned stack frame via
`cglobal …, stack_size=64`. Easier discipline: **don't use `vmovdqa` on the
stack**; use `vmovdqu` everywhere, the cycle penalty on actually-aligned
addresses is zero.

## 6.5 Vector zeroing discipline (vzeroupper)

`x86inc.asm:374` defines `vzeroupper_required = (mmsize > 16 && (ARCH_X86_64 == 0
|| xmm_regs_used > 16 || notcpuflag(avx512)))`. **On AVX-512 hosts, vzeroupper
is NOT emitted automatically** — because the AVX/SSE transition penalty is
absent on AVX-512 microarchitectures (Skylake-X+, Ice-Lake, Zen 4+).

The implication for bbnf primitives:

  - On the AVX-512 baseline (the bbnf-simd primary target), no vzeroupper at
    RET — the macro elides it correctly.
  - On AVX2/AVX baselines where the primitive is also assembled (e.g. a
    backwards-compat AVX2 fork of `BYTE_CLASS_FROM_EQ_SET_64`), `INIT_YMM avx2`
    triggers vzeroupper emission at RET because cpuflag(avx512) is false.

## 6.6 cglobal entry / RET exit (mandatory pattern)

Every primitive body file follows this shape (the byte_class_from_eq_set_64
template, generalised):

```
; ext/x86/<primitive>.asm — body for <PRIMITIVE_NAME>
;
; Contract declared in ext/x86/bbnf.asm. Scalar reference at
; src/scalar/<primitive>.rs. checkasm row at tests/checkasm_<primitive>.rs.

%include "x86inc.asm"

SECTION_RODATA
; (any grammar-neutral LUTs the body needs go here)

SECTION .text

INIT_ZMM avx512    ; or INIT_YMM avx2, etc., per ISA tier
cglobal <name>_<isa>, <nargs>, <nregs>, <nxmm>, arg1, arg2, ...
    ; body
    RET
```

The current `byte_class_from_eq_set_64.asm` follows this exactly.

## 6.7 Per-ISA file naming convention

dav1d uses `<primitive>_<isa>.asm` (`mc_avx2.asm`, `mc_sse2.asm`). bbnf-simd's
current per-primitive directory under per-ISA root
(`src/x86_64/byte_class_from_eq_set_64.asm`) is more granular and is the right
choice given the primitive-first factoring. Codify:

  - `src/x86_64/<primitive>.asm` — primary body for the highest ISA tier this
    primitive admits (AVX-512 BW + VBMI for byte-class, VPCLMULQDQ for
    bitmap_prefix_xor, etc.).
  - `src/x86_64/<primitive>_<isa-fallback>.asm` — additional body file for a
    lower ISA tier when the primitive admits one (e.g. an AVX2 fork that
    operates on 32B at a time). The dispatch lives in
    `src/x86_64/<primitive>.rs`.
  - `src/aarch64/<primitive>.rs` — NEON / SVE2 intrinsic body (no .asm; the
    AArch64 toolchain prefers intrinsics, and `cglobal` is x86-only).
  - `src/scalar/<primitive>.rs` — the scalar reference; the spec.
  - `tests/checkasm_<primitive>.rs` — the differential gate.

# 7. FFmpeg's process beyond dav1d

FFmpeg adds three layers on top of dav1d's discipline.

## 7.1 Multi-target runtime dispatch (CPUID → kernel table)

FFmpeg's `libavutil/cpu.c` runs cpuid once at startup, caches the result in a
global `av_get_cpu_flags()` accessor. Each codec's init function inspects the
flags and patches a function-pointer table:

```c
av_cold void ff_<codec>_init_<arch>(<codec>Context *c, int bit_depth) {
    int cpu_flags = av_get_cpu_flags();
    if (EXTERNAL_AVX512ICL(cpu_flags))     c->fn = ff_<codec>_avx512icl;
    else if (EXTERNAL_AVX2(cpu_flags))     c->fn = ff_<codec>_avx2;
    else if (EXTERNAL_SSSE3(cpu_flags))    c->fn = ff_<codec>_ssse3;
}
```

bbnf-simd's `src/dispatch.rs` SelectedClassifier enum is the equivalent at the
Rust level. The full landing pattern (`bbnf.asm`'s nine macros × multiple ISA
tiers) needs:

  1. A `runtime_dispatch` module that runs cpuid once via
     `std::arch::is_x86_feature_detected!` (or `cpuid`-direct for finer
     granularity) and stores the result in a `OnceLock<CpuFeatures>`.
  2. A `KernelTable` struct holding fn-pointers for each primitive × ISA tier.
  3. Init-time table population: for each primitive, walk ISA tiers
     high-to-low, pick the first the host supports, store the pointer.
  4. The Rust FFI wrapper in `src/x86_64/<primitive>.rs` reads from the table
     via a single indirect call.

The cost is one indirect call per primitive invocation — negligible against
the SIMD work done inside. The benefit is single-binary deploys across the
microarchitectural spread.

## 7.2 SIMD intrinsics fallback vs ASM (cost trade-off)

FFmpeg keeps two parallel implementations of many primitives: a hand-written
.asm version (the fast path) and a Rust/C intrinsics version (the
maintainability backstop). For new primitives, FFmpeg sometimes lands the
intrinsics version first to prove correctness, then the .asm version when
the hot path warrants it.

bbnf-simd should do the same for the eight unlanded primitives — each lands
with:

  1. A scalar reference in `src/scalar/<primitive>.rs` (the spec);
  2. An intrinsics implementation in `src/x86_64/<primitive>.rs` for AVX-512,
     `src/aarch64/<primitive>.rs` for NEON (the cross-platform backstop);
  3. A hand-written .asm body in `src/x86_64/<primitive>.asm` (the fast
     path, when the intrinsics version's codegen quality is provably bested).

Steps 1 and 2 are mandatory for every primitive. Step 3 is conditional on the
hot path actually mattering — for `BITMAP_PREFIX_XOR_64`, the single
VPCLMULQDQ instruction is identical between intrinsics and hand-written ASM,
so step 3 is redundant. For `BYTE_CLASS_FROM_EQ_SET_64`, the intrinsics
version's register-allocator can spill k-registers in unpredictable patterns;
the hand-written version gets the register file exactly right and is the
preferred shipping form. The `cargo asm` discipline
(`feedback_inspect_generated.md` in MEMORY) is how the choice is made.

## 7.3 "The scalar reference IS the spec" rule

This is the rule that makes the whole edifice tractable. Every ASM kernel
ships with a scalar twin that:

  1. is short enough to audit by inspection (≤ 50 LOC for primitive-level);
  2. uses only baseline integer / float ops — no SIMD intrinsics;
  3. is bench'd against the ASM in the same checkasm row (so any
     specification ambiguity is caught at parity-check time);
  4. is committed to the repo as part of the primitive's landing PR.

bbnf-simd's `src/scalar/byte_class_from_eq_set_64.rs` is the working
example. Every one of the eight remaining primitives needs the same form
landed in the same wave as its ASM body.

# 8. Concrete amendments to bbnf-simd

## 8.1 checkasm gaps

  - **Register-clobber detection**: add a tiny ASM shim
    `tests/checkasm_call_new.asm` (or use inline ASM via Rust's
    `core::arch::asm!`) that, around every `call_new`, writes sentinels into
    rbx/rbp/r12/r13/r14/r15 pre-call and verifies them post-call. Same for
    AArch64 x19..x28, d8..d15. Without this, an ASM kernel that clobbers a
    callee-saved without restoring it is detected only when its caller
    happens to depend on that register value — late and randomly.
  - **Stack canary verification post-call**: the current
    `stack_clobber_then` at `tests/checkasm_byte_class_from_eq_set_64.rs:126`
    reads but does not compare the canary value. Add an XOR-fold of the
    1 KiB buffer pre and post; assert equality.
  - **call_ref / call_new arity macros**: codify a Rust macro
    `bbnf_checkasm_call!(fn_ptr, args…)` that wraps the candidate call in
    the signal-guard arm / disarm dance so per-primitive tests don't repeat
    the boilerplate. The pattern is:
    ```rust
    macro_rules! call_new {
        ($f:expr, $($arg:expr),*) => {{
            signal_guard::arm();
            let result = stack_clobber_then(|| unsafe { $f($($arg),*) });
            signal_guard::disarm();
            result
        }};
    }
    ```
  - **randomize_buffers**: lift `Xorshift64::fill` to a shared module
    `tests/common/mod.rs` (or `tests/checkasm_common.rs`) so per-primitive
    tests stop redefining it (the duplication at
    `tests/checkasm_byte_class_from_eq_set_64.rs:36-81` will compound as
    primitive count grows).
  - **Cycle counter**: replace `Instant`-based timing with `__rdtsc`
    via `core::arch::x86_64::_rdtsc` on x86_64 and
    `core::arch::aarch64::__rdtsc` (or `mach_absolute_time`) on AArch64.
    Wall-clock has ~20ns jitter; rdtsc has ~1 cycle.

## 8.2 x86inc patterns bbnf hasn't yet used

  - **DEFINE_ARGS for arg-relabeling**: when a primitive consumes its inputs
    and reuses the same registers for loop-local state, `DEFINE_ARGS cursor,
    limit, mask` rebinds for readability. None of the future bodies need
    this for AVX-512 fan-out shape, but `BULK_EMIT_COMPRESSED`'s rdi cursor
    advance does — see §9.5.
  - **REPX for instruction fan-out**: `REPX {vpbroadcastb zmm%1, byte
    [set_ptrq + %1]}, 0, 1, 2, 3, 4, 5, 6, 7` would compress the 8-fold
    fan-out in `byte_class_from_eq_set_64.asm:56-99` into one macro
    invocation. Worth applying as cleanup.
  - **SECTION_RODATA**: currently declared but unused in the byte_class
    body. The `BYTE_CLASS_FROM_TABLE_64` body will use it for the
    256-byte LUT (§9.1).
  - **cextern for codegen-emitted LUTs**: as per §4, codegen-emitted
    per-grammar `.data` tables get `cextern <table_name>` references in the
    consuming kernel.

## 8.3 Per-ISA file naming + per-primitive directory codification

Make this a Lock-15 amendment: `src/<arch>/<primitive>/{mod.rs, body.asm,
fallback.asm}` is the directory shape. The current
`src/x86_64/byte_class_from_eq_set_64.rs` + `.asm` flat-sibling pair is fine
when there is exactly one body file; promote to directory when a second body
arrives.

## 8.4 Runtime dispatch discipline (CPUID + kernel table)

Land a `src/dispatch.rs` extension that:

  1. Reads CPU features once via `is_x86_feature_detected!` (or `cpuid_count`
     for granularity beyond Rust's whitelist) into a `OnceLock<CpuFeatures>`.
  2. Exposes `pub fn dispatch_<primitive>() -> fn(&[u8; 64], …) -> u64` that
     consults the OnceLock and returns the highest-tier kernel pointer.
  3. The Rust FFI wrapper in `src/x86_64/<primitive>.rs` calls
     `dispatch_<primitive>()()` rather than an `#[cfg(target_feature)]`
     gated direct call — so a single build artifact runs on a Skylake host
     (AVX2 path) AND an Ice-Lake-X host (AVX-512 path).

# 9. The eight remaining primitive bodies, dav1d-style

For each unlanded primitive, the spec is: macro contract already in bbnf.asm
(citations below), scalar reference and intrinsics+ASM body land together,
checkasm row mirrors the byte_class_from_eq_set_64 template.

## 9.1 BYTE_CLASS_FROM_TABLE_64 (`bbnf.asm:65-101`)

**Body pattern**: load 64 bytes into zmm0 (`vmovdqu64 zmm0, [rdi]`), broadcast
the LUT base into zmm1 via four `vmovdqu64` from `[rsi+0/16/32/48]` into
zmm1..zmm4, perform `vpermb` to gather LUT[src[i]] into zmm5, `vptestmb k1,
zmm5, zmm5` to materialise the membership mask.

**ISA fork**: on AVX-512 BW without VBMI, replace `vpermb` with two
`vpshufb` over the LUT halves plus a `vpor` (the asmjson per-state mask
shape). On GFNI hosts where the predicate is expressible as a GF(2⁸) affine,
use `vgf2p8affineqb` with an 8-byte affine constant — 5× fewer µops than the
full table. Codegen flags admissibility per ARCH §7.3.

**Scalar reference**: `for i in 0..64 { mask |= (lut[src[i]] != 0) as u64 << i }`.

**checkasm shape**: alignment sweep 0..64, set-size sweep across LUT
densities 1..256, adversarial seeds, corpus parity. The LUT itself is an
input — pass `&[u8; 256]` from the harness.

**call_ref / call_new signature**: `fn(src: &[u8; 64], lut: &[u8; 256]) -> u64`.

## 9.2 BITMAP_PREFIX_XOR_64 (`bbnf.asm:144-181`)

**Body pattern**: `vpxor xmm0, xmm0, xmm0; vpinsrq xmm1, xmm1, rdi, 0;
vpclmulqdq xmm2, xmm1, [all_ones_q], 0x00; vmovq rax, xmm2`. Single
VPCLMULQDQ at 128-bit width is sufficient since the input is 64 bits.

**ISA fork**: VPCLMULQDQ-VEX256 vs EVEX-512 — same instruction, different
encoding; the EVEX form admits zmm operands but the 64-bit input doesn't
benefit. Use the VEX-128 form for max compatibility.

**Scalar reference**: the `prefix_xor_64` already at
`src/lib.rs:352-364` — the bit-parallel doubling formula
`mask ^= mask<<1; <<2; <<4; <<8; <<16; <<32`. Already serves as spec.

**checkasm shape**: full 64-bit input space is 2^64; sample via Xorshift
plus boundary cases (`u64::MAX`, `0x5555…`, `0xAAAA…`, single-bit walking).

**call_ref / call_new signature**: `fn(mask: u64) -> u64`.

## 9.3 BITMAP_NEXT_SET_BIT (`bbnf.asm:183-223`)

**Body pattern**: two instructions — `shrx rcx, rdi, rsi; tzcnt rax, rcx`.
`shrx` shifts without affecting flags (BMI2); `tzcnt` returns 64 on zero
input. The result is the offset of the next set bit ≥ rsi.

**ISA fork**: BMI1 (tzcnt) is universal on AVX-512 hosts; no fallback needed.

**Scalar reference**: `let shifted = mask >> cursor; if shifted == 0 { 64 }
else { cursor + shifted.trailing_zeros() as u64 }`.

**checkasm shape**: dense bitmap with cursor sweep 0..64; sparse bitmap;
all-zero bitmap; all-ones bitmap; single-bit walking.

**call_ref / call_new signature**: `fn(mask: u64, cursor: u64) -> u64`.

## 9.4 BULK_EMIT_COMPRESSED (`bbnf.asm:225-267`)

**Body pattern**: `vpcompressb zmm4{k1}{z}, zmm0; vmovdqu64 [rdi], zmm4;
kmovq rax, k1; popcnt rax, rax; add rdi, rax`. Sink-cursor maintenance is
baked into the macro.

**ISA fork**: AVX-512 VBMI2 required. On hosts without VBMI2, the primitive
returns an "unsupported" sentinel and the dispatcher routes to a scalar
tape builder.

**Scalar reference**: `for i in 0..64 { if mask & (1<<i) != 0 { sink.push(src[i]) } }`,
returning the count.

**checkasm shape**: every mask popcount 0..64 over random src; corpus parity
over the JSON tape-build path.

**call_ref / call_new signature**: `fn(src: &[u8; 64], mask: u64, sink: &mut [u8]) -> usize`.
The Rust wrapper handles the sink slice; the ASM body operates on the raw
pointer.

## 9.5 EOB_PAD_CLAMP (`bbnf.asm:269-315`)

**Body pattern**: `mov rax, -1; bzhi rax, rax, rsi; kmovq k7, rax;
vmovdqu8 zmm0{k7}{z}, [rdi]`. Four instructions; zero-extending masked load
materialises a 64-byte vector with the live-byte mask in k7.

**ISA fork**: AVX-512 BW (masked load) + BMI2 (bzhi). Universal on the
bbnf-simd target.

**Scalar reference**: copy `rsi` bytes from `&input[start..start+rsi]` into
a zero-initialised `[u8; 64]`; mask is `(1u64 << rsi).wrapping_sub(1)`.

**checkasm shape**: rsi sweep 0..=64; ensure over-allocation invariant (the
input buffer must have 64B of slack past `start + rsi`); adversarial:
rsi = 64, rsi = 0.

**call_ref / call_new signature**: `fn(input: *const u8, rsi: u64) -> (Zmm, u64)`.
The Rust wrapper returns the ZMM as `[u8; 64]` + the k7 mask as `u64`.

## 9.6 FSM_DISPATCH_THREADED (`bbnf.asm:317-368`)

**Body pattern**: single instruction — `jmp [r11 + r10*8]`. No return; control
flows to the next state's body, which ends in another FSM_DISPATCH_THREADED
expansion. The "loop" is the threaded chain itself.

**ISA fork**: baseline x86_64; no SIMD.

**Scalar reference**: a Rust match-on-state in `src/scalar/fsm_dispatch.rs`.
Per the V9.5 amendment, FSM_DISPATCH_THREADED is consumed only by
codegen-emitted CollapsedStage kernels, so the scalar twin lives in codegen
output too. The bbnf.asm contract declares the macro; the reference
implementation is the Rust state machine in codegen's `CollapsedStage` lowering.

**checkasm shape**: differential — emit a small grammar's CollapsedStage as
both ASM and Rust; compare state-traversal traces over a corpus. This is
the only primitive where the parity check operates over a *trace* rather
than a single output value.

**call signature**: tail-call; no Rust FFI wrapper. The dispatch arrives from
the per-grammar kernel's entry point; the kernel itself is the FFI surface.

## 9.7 FRAME_PUSH_BOUNDED (`bbnf.asm:370-422`)

**Body pattern**: three instructions — `mov [rdi+rcx], al; inc rcx; cmp rcx,
r9; ja %1` where `%1` is the overflow error label supplied at macro
expansion. Plain GPR ops; no SIMD.

**ISA fork**: baseline x86_64.

**Scalar reference**: a Rust `fn push(stack: &mut Vec<u8>, kind: u8, max: usize)
-> Result<(), Overflow>` doing `if stack.len() == max { Err } else { stack.push(kind); Ok }`.
For a stack-only (no-heap) primitive, the reference works on `&mut [u8; 64]`
plus a `&mut usize` depth cursor.

**checkasm shape**: push sequence up to MAX_DEPTH, push that overflows MAX_DEPTH
(verifies the error-label branch fires), random push sequences from Xorshift.

**call_ref / call_new signature**: `fn(stack: &mut [u8; 64], depth: &mut u64,
max: u64, kind: u8) -> Result<(), ()>`.

## 9.8 FRAME_POP_BOUNDED (`bbnf.asm:424-473`)

**Body pattern**: four instructions — `dec rcx; mov al, [rdi+rcx]; cmp al,
%1; jne %2` where %1 is the expected FrameKind imm and %2 the mismatch label.

**ISA fork**: baseline x86_64.

**Scalar reference**: `if depth == 0 { panic } let kind = stack[depth-1];
if kind != expected { Err(Mismatch) } else { Ok }`.

**checkasm shape**: push-pop round-trip parity; push-pop with intermediate
mismatched expectation (must fire mismatch label); empty-stack pop
(precondition violation — see contract note at `bbnf.asm:460-464` —
not exercised by harness because the contract forbids it).

**call_ref / call_new signature**: `fn(stack: &[u8; 64], depth: &mut u64,
expected: u8) -> Result<u8, ()>`.

# 10. Closing — the wave plan

Per the same-wave-consumer rule (§5), these eight primitives do not all land
together. The ordering implied by §9 and ARCHITECTURE §7.4:

  - **Wave A** — BITMAP_PREFIX_XOR_64, BITMAP_NEXT_SET_BIT, EOB_PAD_CLAMP.
    Three small, universal primitives; consumer is the JSON CollapsedStage
    kernel which is already prototyped. Land in the same PR as the kernel
    that consumes them.
  - **Wave B** — BYTE_CLASS_FROM_TABLE_64. Consumer is the
    CSS / arbitrary-grammar classifier (predicates >8 chars). Land with
    the CSS L4 typed grammar's CollapsedStage emission.
  - **Wave C** — BULK_EMIT_COMPRESSED. Consumer is the structural-tape
    builder; land when the tape becomes a separate codegen output.
  - **Wave D** — FRAME_PUSH_BOUNDED, FRAME_POP_BOUNDED. Consumer is any
    bracket-stack grammar's CollapsedStage. Land with the first such kernel.
  - **Wave E** — FSM_DISPATCH_THREADED. Consumer is the CollapsedStage
    backend itself. Land when the codegen emits its first .asm output.

Every wave's PR carries: macro contract already in bbnf.asm; body in
`src/x86_64/<primitive>.asm`; intrinsics fallback in
`src/x86_64/<primitive>.rs`; AArch64 NEON equivalent in
`src/aarch64/<primitive>.rs`; scalar reference in
`src/scalar/<primitive>.rs`; checkasm row in
`tests/checkasm_<primitive>.rs` modelled verbatim on
`tests/checkasm_byte_class_from_eq_set_64.rs`; CHECKASM-REPORT.md updated
with the new primitive's parity result.

The dav1d discipline this report has codified is that NO new primitive lands
without (a) a same-wave consumer, (b) a scalar twin that IS the spec, (c) a
checkasm row that asserts parity over random + adversarial + corpus inputs,
and (d) a contract block in `bbnf.asm` whose macro signature has not been
violated. The eight remaining primitives each pass through this gate
individually; the gate is the deliverable.
