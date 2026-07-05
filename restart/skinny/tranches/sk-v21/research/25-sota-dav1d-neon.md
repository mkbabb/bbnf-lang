# SOTA Frontier: the dav1d/rav1d asm discipline and Apple-M NEON microarchitecture (2026)

Scope: what the dav1d project's assembly discipline actually is, what survived when it was
ported to Rust as rav1d, the microarchitectural facts of NEON on Apple M1–M4 silicon, the
honest bound on what hand-written assembly can still win over compiler intrinsics on that
silicon in 2026, and the "checkasm bound to liveness" pattern that keeps optimized kernels
from rotting behind green tests. Written to inform a parser generator whose emitter targets a
hybrid of backend-agnostic Rust and hand-rolled aarch64 kernels dispatched dav1d-style.

The single load-bearing conclusion: the dav1d discipline is transplantable to a parser
generator almost verbatim as an *engineering process* — a function-pointer kernel table plus a
correctness-and-benchmark harness that iterates every registered variant — but the *headroom*
that discipline buys on Apple silicon in 2026 is narrow. A modern LLVM emitting NEON intrinsics
from well-shaped Rust lands within a few percent of hand asm on straight-line SIMD; the durable
wins are confined to (1) idioms the compiler provably cannot synthesize (the `shrn` movemask,
integer↔SIMD-transfer avoidance), (2) whole-loop instruction scheduling and register-pressure
control the register allocator mismanages, and (3) elimination of language-level tax (bounds
checks, panic paths, zero-init) that is a Rust problem, not an asm problem. Bounding our dav1d
ambition to those three is the honest posture.

---

## 1. The dav1d asm discipline

### 1.1 The shape: pure C + hand asm, function-pointer kernel tables

dav1d is VideoLAN's AV1 decoder, deliberately "pure C and asm," architected for speed and
correctness and now production-battle-tested (shipping in VLC, Chrome, Firefox, Android). The
governing engineering loop is stated plainly by its own contributors: *write assembly, test it
with checkasm, and if it is faster it is likely to get merged*
(https://medium.com/@ewoutterhoeven/how-arms-neon-enables-efficient-av1-decoding-on-mobile-5fcb3a4f6e7f).
The decoder is not "C with a few asm helpers"; the performance-critical DSP (loop filters, CDEF,
inverse transforms, motion compensation, film grain, intra prediction) is essentially *all*
hand asm, with the C serving as the readable reference and the fallback for un-accelerated ISAs.

Dispatch is by **function pointer, selected once at init from runtime CPU-feature detection**.
The decoder holds DSP context structs whose members are function pointers; at initialization it
detects CPU flags (SSSE3/AVX2/AVX-512 on x86; NEON/DOTPROD/I8MM/SVE2 on aarch64) and overwrites
the C defaults with the best available specialized kernel. There is no per-call branch on CPU
features on the hot path — the branch was paid once, at setup, and the hot loop is an indirect
call through a resolved pointer. This is the canonical "kernel table" pattern and it is exactly
what a code generator can emit mechanically.

### 1.2 Per-ISA specialization, and when they hand-write vs trust the compiler

dav1d does not write one "NEON version." It writes **per-microarchitecture-tier kernels**:
baseline NEON, then variants exploiting DOTPROD (`sdot`/`udot`), I8MM, and — on ARMv9 parts —
SVE2. Each is a separate registered function pointer, each separately validated and benchmarked.
The decision to hand-write is *empirical, not dogmatic*: a kernel earns its place only if
checkasm shows it beats the C compiled at `-O3` (and beats the next-cheapest specialization).
Where the compiler already emits optimal code — simple element-wise passes with no awkward
horizontal reduction, shuffle, or saturation — dav1d leaves the C. The asm is spent where the
compiler is known to fail: horizontal reductions, byte-permute/table-lookup classification,
saturating narrowing, and tight-schedule inner loops where register allocation and software
pipelining matter. This is the load-bearing cultural fact: **hand asm is a targeted instrument,
applied only where a measurement proves the compiler left cycles on the table.**

### 1.3 What survived the Rust port (rav1d): the empirical answer on "does asm translate"

rav1d (ISRG/Prossimo's memory-safe port) is the strongest available evidence on how much of the
dav1d discipline is language-independent. It was built by (1) running `c2rust` over dav1d, (2)
**reusing dav1d's asm-optimized functions unchanged**, and (3) idiomatizing the transpiled C
(https://ohadravid.github.io/posts/2025-05-rav1d-faster/,
https://www.memorysafety.org/blog/rav1d-performance-optimization/). Load-bearing findings:

- **The assembly was kept verbatim, not translated.** "Many of the performance critical
  operations in dav1d are implemented in native assembly, so we reused the dav1d assembly code
  for these low-level, highly-tuned routines" — kept as-is
  (https://www.memorysafety.org/blog/rav1d-performance-optimization/). dav1d and rav1d *share the
  exact same low-level asm*; it is not modifiable by the port. The asm layer is a black box the
  host language calls into.
- **Function pointers beat enum dispatch — measured, not assumed.** The team tried a Rust-idiomatic
  `match` on a `CpuFlags` enum to select kernels and found it *slower* than raw function pointers
  when profiled, and reverted: "when profiling, we found this was actually slower than the
  function pointers so we returned to them"
  (https://www.memorysafety.org/blog/rav1d-performance-optimization/). The dav1d dispatch model is
  not a C wart to be idiomatized away — it is the optimum, in Rust too.
- **The calling-convention trick:** Rust passes extra arguments the asm ignores. "The assembly
  functions remain exactly the same and just ignore the extra arguments. This is safe to do
  according to the calling convention for our architectures." A stable ABI seam lets one asm body
  serve multiple host-side signatures.
- **The residual gap is a *language* tax, not an asm gap.** Untouched transpiled C-as-Rust ran
  ~3.8% slower than the C, executing ~7% more instructions, of which ~2% were bounds checks; the
  rest was panic/branch-prediction and codegen-shape overhead. Optimizing in earnest cut overhead
  from a peak ~11% to under 6% on x86_64; on aarch64 (M3) the gap was ~9% (≈6 s on a 68 s decode)
  before targeted fixes (https://ohadravid.github.io/posts/2025-05-rav1d-faster/).
- **The two concrete aarch64 wins are instructive** because *neither touched asm* — both removed
  Rust-imposed overhead around the asm:
  - Eliminating an unnecessary zero-init of a scratch buffer in the NEON CDEF-filter dispatch
    wrapper (`cdef_filter_neon_erased`), converting to `MaybeUninit`: 670→274 profiler samples,
    ~1.5% total (≈1.2 s).
  - Replacing a derived `PartialEq` on a two-`i16` `Mv` struct (which LLVM compiled to a poor
    field-wise compare) with a byte-wise compare: ~0.7% total. (Rust issue #140167 is the
    underlying LLVM missed-optimization.)

The rav1d verdict for us: **the asm is portable and the dispatch model is the optimum in any
host language; the fight is entirely on the host side — bounds checks, panic paths, zero-init,
and codegen shape around the kernel calls.** A parser generator emitting Rust wrappers around
hand kernels inherits *exactly* rav1d's problem surface, and rav1d already mapped the fixes:
`MaybeUninit` for scratch, hand-shaped equality/hashing, and function-pointer (not enum) dispatch.

The $20,000 Prossimo bounty to close rav1d to within 5% of dav1d ran 2025-05-14 through
2025-12-31 (https://www.memorysafety.org/blog/rav1d-perf-bounty/); the starting gap it named was
"about 5% slower … differs a bit depending on the benchmark, input, and platform." No final
winner announcement surfaced in this pass — the takeaway that matters is the *magnitude*: after
heavy optimization, safe Rust wrapping identical asm sits a low-single-digit percentage behind C,
and closing the last few percent required LLVM-level and host-shape surgery, not more asm.

---

## 2. Apple M-series NEON microarchitecture (M1–M4), 2026 state

### 2.1 The macro facts: 128-bit only, four SIMD pipes, no SVE on P-cores until M4

- **128-bit vectors, four parallel SIMD/FP pipelines.** The M1/M2 Firestorm-class P-core has
  **four 128-bit NEON/FP pipes**, each capable of an independent FMAC per cycle; M4 keeps four
  128-bit SIMD units doing FP+integer vector work in parallel
  (https://arxiv.org/html/2502.05317v1, https://en.wikipedia.org/wiki/Apple_M1). Apple's answer to
  wide vectors is *not* width — it is **replication**: four 128-bit ops/cycle deliver ~512
  bit-lanes/cycle of throughput (e.g. 16 FP32 FMACs/cycle), matching an AVX-512 unit's work but
  issued as four separate NEON instructions. The implication for a code generator: the way to
  saturate an Apple P-core is **four independent 128-bit dependency chains in flight**, not one
  wide vector. Unrolling to expose ILP matters more than lane width.
- **No SVE on P-cores through M3; M4 adds ARMv9.2-A but SVE remains effectively unavailable to
  general code.** M1/M2/M3 are ARMv8.5/8.6-A — NEON only, no SVE/SVE2. M4 moves to ARMv9.2-A, but
  reports through 2026 are that **SVE2 is not exposed to developers** on M4 as a practical vector
  target (https://arxiv.org/html/2502.05317v1). The 2026 planning assumption for Apple silicon is:
  **write 128-bit NEON; do not design around scalable vectors.** This is the same NEON-only floor
  dav1d's aarch64 kernels target, which is convenient — the dav1d NEON kernel corpus is directly
  representative of what Apple silicon runs.
- **Pipeline width 8 uops/cycle** (Firestorm), so SIMD shares an 8-wide backend with scalar/load/
  store — SIMD-bound loops still need scalar-op and address-generation headroom
  (https://dougallj.github.io/applecpu/firestorm.html).

### 2.2 Load/store bandwidth

Firestorm has **four load/store units**: two load-only, one store-only, one load-or-store — max
sustained **3 loads + 2 stores per cycle**, each unit a full **128-bit datapath** shared by
scalar and SIMD/FP domains (https://dougallj.github.io/applecpu/firestorm.html,
https://eclecticlight.co/2021/12/22/anomalies-in-base-performance-of-m1-cores/). Vector `LD1`
(one reg) measures ~5 cycles latency, 3/cycle throughput; `ST1` ~2/cycle. Paired `LDP`/`STP` are
specifically optimized and can be up to ~40% faster than separate accesses in access-heavy code,
which is why every modern aarch64 compiler pairs aggressively
(https://www.wasilzafar.com/pages/series/arm-assembly/arm-assembly-16-apple-silicon.html). For a
byte-streaming parser, the takeaway: **load bandwidth (3×128b/cyc = 48 bytes/cyc peak) is not the
bottleneck for a 16-byte-per-iteration scanner** — the classification/reduction work is.

### 2.3 The measured NEON instruction table (M1 Firestorm; latency / recip-throughput, cycles)

From ocxtal's `insn_bench_aarch64` on Firestorm
(https://github.com/ocxtal/insn_bench_aarch64/blob/master/results/apple_m1_firestorm.md):

| Instruction | Latency | Throughput (ops/cyc) | Note |
|---|---|---|---|
| `ADD` (vec int) | 2 | 4+ | cheap, all four pipes |
| `CMEQ`/`CMHI` (compare) | 2 | 4 | produces all-ones/all-zero lanes |
| `SHRN`/`SHRN2` | 3 | 4 | the movemask engine (see §2.4) |
| `ADDV`/`UADDLV` (horiz reduce) | 3 | 4 | cheaper than folklore; still a reduction |
| `TBL` len=1 | 2 | 4 | 16-byte LUT |
| `TBL` len=2 | 2 | 4 | 32-byte LUT — same cost as len=1 |
| `TBL` len=3 | 4 | 2 | 48-byte LUT |
| `TBL` len=4 (`vqtbl4q`) | 4 | 1.33 | 64-byte LUT — 3× the reciprocal cost of len≤2 |
| `TBX` len=4 | 8 | ~1 | merging variant is far worse |
| `FADD` (vec) | 3 | 4 | |
| `FMLA` (vec) | 4 | 4 | fully pipelined |
| `LD1` (1 reg) | 5 | 3 | |
| `ST1` (1 reg) | — | 2 | |
| `UMOV`/`SMOV`/`FMOV` (SIMD↔GPR) | **12** | — | the killer — see §2.5 |
| `CNT` (popcount) roundtrip to GPR | ~15 | — | int→SIMD→int, 4 instr (simdjson) |

### 2.4 The `shrn` movemask idiom vs `vaddv` — the central NEON parsing primitive

NEON has **no `PMOVMSKB`**. Extracting a compact bitmask from a 128-bit comparison result is the
defining primitive of a SIMD byte parser (simdjson-class classification, delimiter scanning), and
there are three known techniques, in ascending quality:

1. **Naive PMOVMSKB emulation** (the sse2neon/SIMDe 1:1 port): long dependency chains, "at least 4
   instructions each with ≥2 cycles latency," commonly 1.5–2× slower than the alternatives
   (https://branchfree.org/2019/04/01/..., search corpus). **Never emit this.**
2. **`vaddv`/horizontal-add with a per-lane power-of-two bitmask** (`{0x0101,0x0202,…}` then
   `vaddvq_u16`): correct, but funnels through a horizontal reduction and, in the general case, is
   longer than the shift-narrow path.
3. **The `shrn`-by-4 idiom** (Arm's recommended replacement, Danila Kutenin / glibc `strlen`):
   ```c
   uint16x8_t eq = vreinterpretq_u16_u8(vceqq_u8(chunk, vdupq_n_u8(tag)));
   uint8x8_t   r = vshrn_n_u16(eq, 4);          // narrow 8×16b → 8×8b, 4 bits/lane
   uint64_t  m  = vget_lane_u64(vreinterpret_u64_u8(r), 0);
   ```
   **3–4 instructions total**, producing a **4-bits-per-input-byte** bitmask (a matching byte
   shows as nibble `0xF`; the mask for an all-match input is `0x8888…` after masking). This
   replaced glibc's older `addp`-based approach for a documented **10–15% strlen improvement** on
   SPEC CPU 2017
   (https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon).
   On Firestorm the critical `SHRN` is 3-cycle latency / 4-per-cycle throughput.

**The subtlety a code generator MUST encode:** the `shrn`-4 mask is *4 bits per byte*, not 1. All
downstream bit arithmetic (finding the next delimiter, popcount of matches, indexing) must operate
in the nibble domain — `tzcount(m) >> 2` for the byte index, mask `& 0x1111…` conventions, etc.
This is the single most common correctness trap when porting an x86 simdjson-style scanner to NEON;
it is not optional and it is not the compiler's job. This is a canonical case where the *generator*
carries architecture knowledge the compiler does not.

Arm's own note also flags that `shrn` uses a **single vector pipe** on some Neoverse cores (V1),
so for very long runs the multi-pipe `vaddv` variant can win by spreading across pipes. On Apple
Firestorm, with four SIMD pipes and 3c/4-per-cycle `SHRN`, the shift-narrow path is the default;
the caveat is worth encoding as a pluggable choice, not a hardcoded one.

### 2.5 `vqtbl` LUT dispatch and the integer↔SIMD transfer cliff

Two microarchitectural facts dominate parser kernel design on Apple silicon:

- **`vqtbl` table lookup is cheap up to 32 bytes, expensive past it.** A 16- or 32-byte table
  (`vqtbl1q`/`vqtbl2q`, TBL len 1–2) is **2c latency, 4/cycle** — essentially free classification.
  A 64-byte table (`vqtbl4q`, len 4) is **4c latency but only 1.33/cycle** — 3× the reciprocal
  cost. **Design classifiers to fit a 32-byte (two-register) table.** simdjson's M1 tuning did
  exactly this: it replaced whitespace matching with "a subtract and a 32-byte TBL" via
  `vqtbl2q_u8` (https://github.com/simdjson/simdjson/discussions/1658). The nibble-LUT
  classification technique (split byte into hi/lo nibble, two 16-byte `vqtbl1q` lookups, AND the
  results) is the canonical simdjson-class shape and lands squarely in the cheap regime.
- **Moving data between SIMD and GPR registers costs ~12 cycles each way** (`UMOV`/`FMOV`), and a
  NEON popcount that roundtrips int→SIMD→int is ~15c/4-instr — "much worse than x86"
  (https://github.com/simdjson/simdjson/discussions/1658). This is *the* Apple-NEON design
  constraint: **keep the computation in the vector domain; every crossing to GPR is a ~12-cycle
  bubble.** A parser that classifies in SIMD, extracts a mask (one `shrn` + one `vget_lane` = one
  crossing), then does index arithmetic in GPR pays *one* crossing per 16-byte block — acceptable.
  A parser that crosses per byte, or popcounts naively per block, is pathological. This constraint
  is invisible to the compiler and must live in the generator's cost model.

---

## 3. What hand-written asm can STILL win on Apple silicon in 2026 — the honest bound

This is the question that bounds our dav1d ambition. The honest 2026 answer, from the rav1d
evidence and the microarchitecture:

**Where hand asm does NOT reliably win anymore** (trust intrinsics + LLVM):
- Straight-line element-wise vector math (adds, FMLAs, simple shuffles). Modern LLVM schedules
  these near-optimally on the four symmetric Firestorm pipes; the rav1d asm was *kept* here only
  because it already existed, not because intrinsics would have lost. Intrinsics give "almost
  one-to-one mapping to assembly … without register allocation, instruction scheduling, type
  checking" (search corpus) and the compiler pairs `LDP`/`STP` aggressively on its own.
- Anything where the win would be < ~2–3%: the rav1d experience shows that at that margin the
  bottleneck has moved to *host-language shape* (bounds checks, panics, zero-init), which asm
  cannot fix and which is where the real percentage lives.

**Where hand asm (or, equivalently, generator-emitted intrinsic sequences the compiler cannot
synthesize) STILL wins:**
1. **Idioms the compiler cannot invent.** The `shrn`-4 movemask, the nibble-LUT classifier, the
   `vqtbl2q` 32-byte-table trick, saturating-narrow packing — LLVM will not auto-vectorize a
   scalar byte loop into these. The win here is not "asm vs intrinsics," it is "SIMD algorithm vs
   scalar" — often 4–10×. The generator supplies the *algorithm*; whether it is spelled in asm or
   intrinsics is secondary.
2. **Whole-loop instruction scheduling and register-pressure control.** When a hot loop needs four
   independent dependency chains in flight to saturate the four SIMD pipes, LLVM's register
   allocator sometimes serializes or spills. Hand scheduling (or careful intrinsic structuring
   with explicit unroll) recovers ILP. This is dav1d's genuine residual asm win: tight, deep loops
   where the register file (32× 128-bit) must be hand-budgeted. simdjson's M1 tuning shaved 8c→7c
   on a movemask by reordering into a `vqtbl` + multiply — a scheduling/idiom win, ~1%.
3. **Eliminating the host-language tax at the ABI seam** — technically not asm, but the same
   discipline: `MaybeUninit` scratch, hand-shaped equality, function-pointer dispatch. This is
   where rav1d found its largest single gains and is fully within a code generator's control.

**The bound stated plainly:** on Apple silicon in 2026, hand-written asm buys, over
*well-structured intrinsics from well-shaped host code*, roughly **0–5%** on straight-line SIMD and
**up to low-tens-of-percent** only on loops where register allocation / software pipelining is
mismanaged or where an idiom must be forced. The *large* wins (multiples) come from the SIMD
*algorithm choice*, which is architecture-neutral at the source level and belongs in the
generator's kernel library, not from the asm-vs-intrinsics spelling. **For bbnf-lang: emit NEON
intrinsics from generated Rust as the default; reserve literal hand-asm for the handful of hot
kernels where a checkasm-style measurement proves the compiler serialized the pipes.** This is
precisely dav1d's own empirical rule, and it is the disciplined, non-romantic posture.

---

## 4. The checkasm-bound-to-liveness pattern — preventing dead kernels behind green tests

checkasm (originating in x264, now shared by FFmpeg and dav1d;
https://checkasm.videolan.me/, https://deepwiki.com/FFmpeg/FFmpeg/10.1-assembly-testing-with-checkasm)
is the mechanism that makes the "measure, then merge" discipline safe. Its four load-bearing
properties, each directly transplantable to a parser generator's kernel harness:

1. **One test, both correctness and benchmark, per kernel.** The macro vocabulary:
   `declare_func` (register the function-pointer signature under test), `call_ref` (invoke the C
   reference), `call_new` (invoke the optimized variant), `bench_new`/`BENCH` (cycle-count the
   optimized variant via platform performance counters). The *same* test body that proves
   bit-exactness also produces the benchmark — correctness and speed are never measured on
   divergent code paths.

2. **Randomized inputs + bit-exact output comparison.** `rnd()` generates pseudo-random test data
   across multiple seeds/patterns; kernels run on aligned randomized buffers (`PIXEL_RECT`); the
   reference and optimized outputs are compared byte-exact (`checkasm_check_*`). A kernel that is
   fast but wrong on some input pattern fails here — you cannot ship a green-but-incorrect
   optimization.

3. **ABI-violation detection via register clobber + stack canaries.** Architecture-specific
   wrappers (x86/aarch64/riscv) intentionally clobber caller/callee registers before the call and
   verify callee-saved registers (and VFP/AVX state) are restored afterward, and plant stack
   canaries to catch stack corruption. On aarch64 the guarding lives in `asm.S` macros preserving
   callee-saved regs. This catches the class of asm bug that passes a naive output-equality test
   but corrupts the caller — the bugs that make hand asm terrifying.

4. **THE LIVENESS PROPERTY — iterate every registered CPU-flag variant.** checkasm "first tests
   the C reference (null flag) and then **iterates through all detected flags to ensure every SIMD
   implementation is validated**" (https://deepwiki.com/FFmpeg/FFmpeg/10.1-assembly-testing-with-checkasm).
   This is the answer to "how do you prevent a kernel going dead behind green tests": the harness
   does not test *the kernel the dispatcher happened to pick on this machine* — it enumerates
   **every** registered specialization and forces each to be exercised against the reference and
   benchmarked. A kernel that no longer gets dispatched, or that a refactor orphaned, still shows
   up in the checkasm iteration; if it is missing or unbenched, that is visible. Liveness is a
   property of *binding the test loop to the dispatch table itself*, not to any single runtime
   selection.

**The transplant for bbnf-lang.** A parser generator emitting a function-pointer kernel table
should co-emit a checkasm-shaped harness that: (a) for every generated grammar, registers the
scalar reference kernel and every SIMD specialization behind the same signature; (b) drives each
with randomized inputs (random valid documents + adversarial/edge inputs) and asserts byte-exact
output parity against the scalar reference; (c) benchmarks each variant with a cold, single-shot,
cycle-accurate timer (the memory's `no-warm-benches`, `bench-single-run`, `iter-profile-always`
discipline is the local analog of checkasm's counters); and (d) **iterates the whole dispatch
table**, so a specialization that stops being selected, or that a regeneration silently dropped,
fails the harness rather than rotting. This is the concrete mechanism by which "parse speed is
first-order" becomes *enforceable* rather than aspirational: no kernel is merged without a
measurement, and no kernel survives without a live, iterated correctness+bench binding. The
generated harness is the missing enforcement layer that makes the dav1d ambition self-policing.

---

## 5. Synthesis: implications for the SK-V21 emitter

- **Adopt the dav1d dispatch model verbatim.** Function-pointer kernel table, resolved once at
  init from runtime CPU-feature detection, no per-call feature branch. rav1d proves this beats
  enum dispatch *even in Rust* — do not idiomatize it away.
- **Default to NEON intrinsics from well-shaped generated Rust; hand-asm only where measured.**
  The 2026 asm-vs-intrinsics headroom on Apple silicon is 0–5% except at register-pressure/
  scheduling failures. The large wins are algorithmic (SIMD vs scalar) and belong in a
  backend-agnostic kernel library. This aligns with the memory precepts against orthogonal code
  paths and quick asm hacks — asm is the targeted exception, not the substrate.
- **Encode the two Apple-NEON invariants in the cost model** (pluggable, per the memory's
  pluggable-components precept): (1) the `shrn`-4 movemask produces 4-bits-per-byte — all mask
  arithmetic is in the nibble domain; (2) keep computation in the vector domain — every
  SIMD↔GPR crossing is ~12 cycles, so classify-in-SIMD / extract-one-mask / index-in-GPR, one
  crossing per 16-byte block. Size all classification LUTs to ≤32 bytes (`vqtbl2q`, 2c/4-per-cyc)
  and never spill to a 64-byte `vqtbl4q` (4c/1.33-per-cyc) without proving it pays.
- **Co-emit a checkasm-shaped, liveness-bound harness** that iterates every registered kernel
  variant with randomized + adversarial inputs, asserts byte-exact parity against the scalar
  reference, and cold-benchmarks each with cycle-accurate counters. This is the enforcement
  layer that keeps "parse speed first-order" honest and prevents dead kernels behind green tests.
- **Budget the host-language tax deliberately.** rav1d's real gains were `MaybeUninit` scratch,
  hand-shaped equality, and killing panic paths in hot functions — all host-side, all within a
  generator's control. On aarch64 specifically the low-hanging fruit was Rust overhead *around*
  the asm, not the asm. Generated Rust wrappers should be `MaybeUninit`-scratch, panic-free on the
  hot path, and shaped so LLVM's bounds-check elision fires (asserted-length slices, chunked
  iterators) — the generator can guarantee these by construction where hand-written Rust cannot.

## Sources

- https://ohadravid.github.io/posts/2025-05-rav1d-faster/ — rav1d 1% post: asm reuse, CDEF
  zero-init, `Mv` PartialEq, samply 1000 Hz methodology, M3 9% gap.
- https://www.memorysafety.org/blog/rav1d-performance-optimization/ — asm kept verbatim, function
  pointers beat enum dispatch (measured), calling-convention extra-args trick, 11%→<6% x86_64,
  3.8% transpile / 7% instr / 2% bounds-check breakdown.
- https://www.memorysafety.org/blog/rav1d-perf-bounty/ — $20k bounty, ~5% starting gap,
  2025-05-14 → 2025-12-31.
- https://medium.com/@ewoutterhoeven/how-arms-neon-enables-efficient-av1-decoding-on-mobile-5fcb3a4f6e7f
  — "write asm, test with checkasm, merge if faster"; pure C + asm; per-ISA NEON/DOTPROD/SVE2.
- https://checkasm.videolan.me/ and https://deepwiki.com/FFmpeg/FFmpeg/10.1-assembly-testing-with-checkasm
  — declare_func/call_ref/call_new/bench_new, rnd(), bit-exact compare, ABI clobber + stack
  canaries, iterate ALL CPU-flag variants (the liveness property).
- https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
  — `shrn`-4 movemask (3–4 instr, 4-bits/lane), 10–15% strlen SPEC gain, single-pipe caveat.
- https://github.com/simdjson/simdjson/discussions/1658 — M1 tuning: `vqtbl2q` 32-byte table
  classification, 8c→7c movemask, popcount int→SIMD→int 15c/4-instr, integer↔SIMD cost.
- https://github.com/ocxtal/insn_bench_aarch64/blob/master/results/apple_m1_firestorm.md — measured
  Firestorm NEON latency/throughput table (TBL len 1–4, SHRN 3c, ADDV 3c, CMEQ 2c, UMOV/FMOV 12c).
- https://dougallj.github.io/applecpu/firestorm.html — 4 SIMD/FP units, 4 load/store units (128-bit),
  8 uops/cycle.
- https://arxiv.org/html/2502.05317v1 and https://en.wikipedia.org/wiki/Apple_M1 — four 128-bit SIMD
  pipes, replication-not-width, no SVE on P-cores through M3, M4 ARMv9.2-A but SVE2 not exposed.
- https://www.wasilzafar.com/pages/series/arm-assembly/arm-assembly-16-apple-silicon.html — LDP/STP
  ~40% faster, compilers pair aggressively.
