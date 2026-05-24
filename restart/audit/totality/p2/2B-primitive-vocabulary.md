---
agent: 2B
pass: T-P2-research
cycle: V1
generated_at: 2026-05-23T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 18
counted_source_ids: [SRC-DAV1D-HEAD, SRC-DAV1D-X86INC, SRC-X264-X86INC, SRC-FFMPEG-X86UTIL, SRC-FFMPEG-CHECKASM-C, SRC-FFMPEG-CHECKASM-H, SRC-VIDEOLAN-CHECKASM-PAGE, SRC-ARM-ACLE, SRC-ARM-NEON-INTR, SRC-SIMDJSON-PAPER, SRC-LOCKS-16, SRC-BBNF-ASM, SRC-BBNF-X86INC, SRC-BBNF-X86UTIL, SRC-BBNF-LICENSE-VENDOR, SRC-BBNF-SCALAR, SRC-BBNF-CHECKASM, SRC-S-P2-V3]
techniques_grounded: 11
techniques_refuted: 4
techniques_partial: 3
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions:
    - 2B-layer0-vendored-corpus-pin
    - 2B-layer1-9-primitive-vocabulary
    - 2B-one-directional-dependency
    - 2B-admission-disciplne-per-lock-16
    - 2B-fsm-frame-scalar-checkasm-gap
locks_amendment_candidates: 5
---

# T-P2 2B — Two-Layer SIMD/ASM Primitive Vocabulary

## Executive Summary

Ground the two-layer SIMD/ASM primitive vocabulary the V1 spec assumes.
**Layer 0** is the vendored verbatim x86 macro corpus — `x86inc.asm` (72
macros, x264 origin, also vendored by FFmpeg and dav1d at HEAD
`1718ff9aded99f0a89f5c7940d6afb8948301e33`) plus `x86util.asm` (66 macros,
FFmpeg origin). Layer 0 is build-time ABI / register / SIMD instruction
abstraction only; it carries no grammar policy and no runtime state.
**Layer 1** is `bbnf.asm`, bbnf-authored, grammar-neutral, declaring nine
primitive contracts (`BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
`EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`,
`FRAME_POP_BOUNDED`). Layer 1 `%include`s Layer 0; dependency is strictly
one-directional. Each Layer 1 primitive requires (a) a scalar reference in
`crates/bbnf-simd/src/scalar/`, (b) a checkasm parity test in
`crates/bbnf-simd/tests/checkasm_<name>.rs`, and (c) a named same-wave
consumer per Lock 16. Audit at HEAD: 6/9 Layer-1 primitives have scalar
references, 6/9 have checkasm tests; **3/9 (`FSM_DISPATCH_THREADED`,
`FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`) are skeleton contracts only**
and are non-admissible until scalar oracle + checkasm parity land. P3-A's
8-candidate shortlist consumes Layer 1 through the S-P2 V3 P2-B 5-stage
admission process.

## Technique Grounding Table

| spec-claim / T-P1-divergence | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 corpus may be vendored verbatim from upstream rather than re-authored. | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-31` (vendor attribution: x264 / FFmpeg, ISC + LGPL-2.1-or-later, build-time include only); upstream dav1d HEAD pin `1718ff9aded99f0a89f5c7940d6afb8948301e33` per `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:29`; upstream dav1d source at `https://code.videolan.org/videolan/dav1d/-/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/src/x86/x86inc.asm`. | grounded | The vendored corpus is build-time macro substrate only — calling conventions, ABI mechanics, register naming, SIMD instruction macros. Layer 0 carries zero grammar policy. |
| `x86inc.asm` is the x264-origin / FFmpeg / dav1d-shared calling-convention abstraction. | `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:1-22` (x264 copyright 2005-2024, ISC license), `:24-28` ("NASM/YASM syntax combined with a large number of macros to provide easy abstraction between different calling conventions (x86_32, win64, linux64). It also has various other useful features to simplify writing the kind of DSP functions that are most often used."). | grounded | 72 macros local count via `grep -cE "^%macro " ext/x86/x86inc.asm`; covers `cglobal`, `PROLOGUE`, `RET`, `DECLARE_REG`, `PUSH_IF_USED`, `INIT_XMM/YMM/ZMM`, `AVX_INSTR`, `EVEX_INSTR`, `WIN64_SPILL_XMM`, threadsafe RIP-relative addressing, and the AVX-512 mask-register helpers. |
| `x86util.asm` is the FFmpeg DSP macro toolbox (transposes, butterflies, PALIGNR, splat). | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:34-43` (FFmpeg copyright 2008-2024, LGPL-2.1-or-later, header-only-include exception); `skinny/crates/bbnf-simd/ext/x86/x86util.asm` macro inventory (66 macros local count). | grounded | 66 macros: `SBUTTERFLY`, `TRANSPOSE4x4B..TRANSPOSE16x16W`, `PALIGNR`, `PSHUFLW`, `PABSW`, `HADDD`, `SPLATB_LOAD`, `VBROADCASTSS`, `PBLENDVB`, etc. Lock 14 risk: these are pixel/DSP-domain helpers; only the calling-convention subset is universally applicable to bbnf. |
| Vendored macro library remains unmodified; bbnf does not patch Layer 0. | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:48-53` ("Vendoring keeps the build deterministic and avoids a runtime dependency on a system FFmpeg / x264"); no local diffs vs upstream recorded in `bbnf-simd` build configuration. | grounded | Lock 16 admissibility requires unmodified Layer 0; any local patch would invalidate the upstream-citation provenance and require an explicit lock amendment. |
| Layer 1 is bbnf-authored, grammar-neutral, declares 9 primitive contracts. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-12` (file purpose), `:30-44` (9-primitive macro inventory: BYTE_CLASS_FROM_TABLE_64, BYTE_CLASS_FROM_EQ_SET_64, BITMAP_PREFIX_XOR_64, BITMAP_NEXT_SET_BIT, BULK_EMIT_COMPRESSED, EOB_PAD_CLAMP, FSM_DISPATCH_THREADED, FRAME_PUSH_BOUNDED, FRAME_POP_BOUNDED), `:55-60` (per-grammar LUT data lives outside the macro library). | grounded | Local count via `grep -cE "^%macro " ext/x86/bbnf.asm` returns 9. Each contract names inputs / outputs / clobbers / ISA admissibility / citation; bodies live in `src/x86_64/*.asm` per `bbnf.asm:9-12`. |
| Layer 1 → Layer 0 dependency is one-directional (`%include "x86inc.asm" ; %include "x86util.asm"`). | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:47-48` (`%include "x86inc.asm" ; %include "x86util.asm"`); Layer 0 files contain no reference back to `bbnf.asm`. | grounded | Cross-grep confirms: `grep -n "bbnf\|BYTE_CLASS_FROM\|BITMAP_PREFIX" ext/x86/x86inc.asm ext/x86/x86util.asm` returns zero hits. Layer 0 is unaware of Layer 1. |
| Admissible Layer 1 primitive requires a scalar reference in `src/scalar/`. | Lock 16 closing clause at `restart/locks/LOCKS.md:307` ("Every SIMD primitive carries a unit-parity test against the scalar reference and a corpus-parity test against the expanded skinny corpus"); `bbnf.asm:9-12` ("Scalar reference implementations live in src/scalar/*.rs and serve as the executable specification per the checkasm admission gate"); local refs at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:2`, `byte_class_from_eq_set_64.rs:26`, `bitmap_prefix_xor_64.rs:2`, `bitmap_next_set_bit.rs:2`, `bulk_emit_positions_64.rs:2`, `eob_pad_clamp.rs:8`. | grounded | Scalar reference is the executable specification — checkasm differential parity compares optimized output to scalar output at every randomized input. |
| Admissible Layer 1 primitive requires a checkasm parity test in `tests/checkasm_<name>.rs`. | Lock 16 closing clause at `restart/locks/LOCKS.md:307`; FFmpeg `checkasm_check_func` mechanism at `tests/checkasm/checkasm.c` (https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html); VideoLAN checkasm project page (https://www-test.videolan.org/projects/checkasm/); local tests at `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`, `checkasm_byte_class_from_eq_set_64.rs`, `checkasm_bitmap_prefix_xor_64.rs`, `checkasm_bitmap_next_set_bit.rs`, `checkasm_bulk_emit_positions_64.rs`, `checkasm_eob_pad_clamp.rs`. | grounded | Sibling-template path is `bbnf-simd/tests/checkasm_<name>.rs`. Strict mode `BBNF_SIMD_STRICT=1` is mandatory for admission per `restart/locks/LOCKS.md:320-322`. |
| Lock 16 admission requires a same-wave production consumer (no orphan kernels). | Lock 16 ext at `restart/locks/LOCKS.md:335-342` ("At close, every source-present primitive is exactly one of `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`. … Support-only hint modules, unconsumed prefix/next bitmap bodies, cache hints without exact caller placement, and orphan `asm!`/intrinsic files do not close Lock 16"); S-P2 V3 P2-B 5-stage admission at `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:62-118` (Stage D = same-wave consumer). | grounded | `[no-deferrals]` memory entry confirms: no admission split across waves. The consumer commits in the same wave as the kernel. |
| `BITMAP_PREFIX_XOR_64` lifts simdjson's quote-mask primitive at 512-bit width. | simdjson "Parsing Gigabytes of JSON per Second" §3.1 (Langdale & Lemire, Software: Practice & Experience 49(8), 2019) cited at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:174-175`; Lock 16 admissibility row at `restart/locks/LOCKS.md:294` (VPCLMULQDQ 512-bit; "Linux kernel CRC-32C reaches 45-60 GB/s vs ~7-8 GB/s SSE4.2 with this primitive — same multiplier on prefix-XOR"). | grounded | Lock 14 holds: prefix-XOR is a pure bit-parallel carry, valid for any toggle-based region (CSS string literals, BBNF rule-string content, JSON quote pairs). |
| `BITMAP_NEXT_SET_BIT` is the dispatch-driver primitive. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:206-217` ("Per asmjson src/lib.rs the inner classifier loop is dominated by ~18× tzcnt calls per chunk"); Intel/AMD ISA reference for `tzcnt` zero-operand behaviour returning operand width. | grounded | Two-instruction hot path: `shrx rcx, rdi, rsi ; tzcnt rax, rcx`. Universal on AVX-512 hosts (BMI1 baseline). |
| `BULK_EMIT_COMPRESSED` uses VBMI2 `vpcompressb`. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:241-261`; Lock 16 row at `restart/locks/LOCKS.md:292` ("`_mm512_mask_compressstoreu_epi8` (Lemire 2022; simdjson `icelake/simd.h:157` explicitly leaves unused for portability)"). | grounded | VBMI2 hardware gate (Ice Lake+ Intel, Zen 4+ AMD); without VBMI2 callers fall back to scalar tape-builder. |
| `EOB_PAD_CLAMP` borrows dav1d msac tail-handling. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:296-309`; dav1d msac at upstream HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` `src/x86/msac.asm:80-220` (cross-chunk refill pattern cited in Lock 16 v+1 at `restart/locks/LOCKS.md:305`); local scalar ref at `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:8`. | grounded | The msac `cnt/buf/end` cross-chunk refill is the one genuinely transferable algorithmic insight beyond what simdjson/sonic-rs/yyjson demonstrate. |
| `FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED` are asmjson's bounded frame stack. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:404-418` (asmjson `frames_buf` / `open_buf` bounded stack); `:454-468` (companion pop with bracket validate); upstream asmjson `src/lib.rs` referenced. | partial | Contract declared but **no scalar reference and no checkasm test exist at HEAD** for either macro. `grep frame_push ext/x86/x86util.asm src/scalar/*.rs` returns zero hits. Admission requires building the scalar oracle (push/pop semantics with bounds check) + `checkasm_frame_push_bounded.rs` + `checkasm_frame_pop_bounded.rs` before any consumer can wire. |
| `FSM_DISPATCH_THREADED` is the asmjson r10-PC threaded dispatch primitive. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:355-363` ("asmjson src/lib.rs (jmp r10 dispatch core); 'Threaded Code' (Bell, 1973) — the originating ISA-level pattern"); baseline x86_64 indirect jump (no SIMD). | partial | Contract declared but **no scalar reference and no checkasm test exist at HEAD**. The "scalar" reference for a threaded dispatch is a switch-statement equivalent; the checkasm differential would test target-table traversal under random state sequences. Admission requires both artifacts. |
| Layer 1 macros are grammar-neutral; per-grammar LUTs live in codegen-emitted `.data`. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:55-60` ("Per-grammar .data sections — class LUTs, FSM transition tables, frame close-bracket maps — live in the corresponding per-grammar kernel .asm files emitted by codegen, NOT in this macro library"); Lock 14 zero-overfitting at `restart/locks/LOCKS.md:282` ("primitives are grammar-neutral; per-grammar variation lives in codegen-emitted .data tables"). | grounded | The dav1d primitive-lift discipline in full force: shared primitives + per-instance LUT data + shared dispatch spine. |
| Layer 1 may encode JSON quote/escape/control constants directly. | Dispatch hardcodes JSON quote/backslash/control values at `skinny/crates/bbnf-simd/src/dispatch.rs:22-33` (Tbl4 selection); SK-V13 SYNTHESIS pre-block at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:239-263`. | refuted | Lock 14 leak: shared `bbnf-simd` consumers cannot inherit JSON quote/escape/control constants into CSS, union, parse-only, or shared generated code. Per-grammar policy must come from generated grammar code or caller data. |
| Source-present primitive can close as `inventory_demoted_with_evidence`. | Lock 16 v+1 at `restart/locks/LOCKS.md:335-342` ("`inventory_demoted_with_evidence` is historical evidence only"); SK-V12 close pattern at `skinny/REDRESS.md:3806-3812`. | refuted | Only four close states: `wired`, `deleted`, `scalar-delegate-non-ASM`, `architectural-block-with-REDRESS`. Inventory is not a close state under SK-V13/14. |
| Vendoring the FFmpeg/dav1d *pixel-domain* kernels is admissible Layer 0. | dav1d motion compensation / IDCT / loop filter / film grain (T14-T17 of the catalog at `restart/locks/LOCKS.md:305`); they are pixel-domain. | refuted | Lock 16 v+1: "dav1d's pixel-arithmetic kernels do not translate to JSON. … But the *primitive operations* underneath them DO translate." Layer 0 vendors only the macro/ABI substrate (x86inc.asm, x86util.asm), not the kernel bodies. |
| AVX-512 stubs in `bbnf.asm` are M5 Max admission-ready. | `bbnf.asm` declares only x86 macro contracts; SK-V13 implementation scope is aarch64 / Apple Silicon only. | refuted | Layer 1 x86 contracts inform the totality vocabulary but cannot drive SK-V14 wave selection or M5 Max benchmark claims. Per Lock 16 v+1 at `restart/locks/LOCKS.md:346-349`: "AVX-512 literature is x86 architecture pressure and cannot close M5/aarch64 rows." |

## Architectural Assertions Defended

### A1 — Layer 0 is vendored verbatim macro infrastructure, build-time only

The Layer 0 corpus is `x86inc.asm` (72 `%macro` declarations, x264 origin
2005-2024, ISC license) plus `x86util.asm` (66 `%macro` declarations,
FFmpeg origin 2008-2024, LGPL-2.1-or-later, header-only-include
exception). Vendor attribution lives at
`skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-43`. The upstream pin
is dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` recorded in
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:29`. The macros
provide calling-convention abstraction across `x86_32 / win64 / linux64`,
register naming, stack alignment helpers, `cglobal` / `PROLOGUE` / `RET`
function entry, `INIT_XMM` / `INIT_YMM` / `INIT_ZMM` SIMD-register
initialization, `AVX_INSTR` and `EVEX_INSTR` instruction emission
helpers, the AVX-512 mask-register permutation helpers, and the
DSP-oriented butterflies / transposes / shuffles in `x86util.asm`. This
is exactly the right Layer 0 boundary: ABI mechanics and instruction
ergonomics, nothing more. Layer 0 must not carry JSON, CSS, or any
grammar policy.

### A2 — Layer 1 is a 9-primitive grammar-neutral contract vocabulary

`bbnf.asm` declares exactly nine primitive contracts; the file is
include-only and emits no code itself (`bbnf.asm:50-51`). The bodies live
in `src/x86_64/*.asm` per the contract declarations. The nine primitives
partition by function:

| # | primitive | function | hardware gate | ISA citation |
|---|---|---|---|---|
| 1 | `BYTE_CLASS_FROM_TABLE_64` | 64B → k1 via 256-byte LUT (`vpermb` / GFNI) | AVX-512 BW + (VBMI \| BW-only fallback) + GFNI optional | asmjson per-state byte mask; dav1d film-grain classifier (`bbnf.asm:93-96`) |
| 2 | `BYTE_CLASS_FROM_EQ_SET_64` | 64B → k1 via ≤8 char fan-out (`vpcmpeqb` + `korq`) | AVX-512 BW | asmjson `classify_chunk` inner loop (`bbnf.asm:134-137`) |
| 3 | `BITMAP_PREFIX_XOR_64` | 64b bitmap → ripple-XOR via VPCLMULQDQ | VPCLMULQDQ (PCLMULQDQ-VEX256 fallback) | simdjson "Parsing Gigabytes of JSON per Second" §3.1 (`bbnf.asm:174-175`) |
| 4 | `BITMAP_NEXT_SET_BIT` | 64b bitmap + cursor → next-set offset | BMI1 (universal on AVX-512 hosts) | asmjson `classify_chunk` dispatch loop (`bbnf.asm:215-217`) |
| 5 | `BULK_EMIT_COMPRESSED` | 64B + k1 → compressed sink (`vpcompressb`) | AVX-512 VBMI2 | simdjson tape-builder VBMI2 fork (`bbnf.asm:256-261`) |
| 6 | `EOB_PAD_CLAMP` | tail bytes → zero-padded 64B vector | AVX-512 BW + BMI2 | dav1d msac tail-handling; simdjson padded-buffer convention (`bbnf.asm:304-309`) |
| 7 | `FSM_DISPATCH_THREADED` | state-as-PC threaded dispatch (`jmp [tbl+r10*8]`) | baseline x86_64 | asmjson r10-PC dispatch core; Bell 1973 "Threaded Code" (`bbnf.asm:357-363`) |
| 8 | `FRAME_PUSH_BOUNDED` | push FrameKind onto open_buf with bounds check | baseline x86_64 | asmjson `frames_buf` / `open_buf` bounded stack (`bbnf.asm:411-417`) |
| 9 | `FRAME_POP_BOUNDED` | pop FrameKind + close-bracket validate | baseline x86_64 | asmjson `open_buf` pop + bracket validate (`bbnf.asm:464-468`) |

Per `bbnf.asm:41-44`: primitive 7 is the sole FSM macro; it is consumed
only by per-grammar `CollapsedStage` kernels emitted by codegen. The
other eight are recursive-descent / scan-emit leaf primitives shared
across all grammars. Per-grammar LUTs live in codegen-emitted `.data`
sections; the macro library itself is grammar-neutral
(`bbnf.asm:55-60`). This is the dav1d primitive-lift discipline: shared
primitives + per-instance LUT data + shared dispatch spine.

### A3 — Dependency is one-directional (Layer 1 → Layer 0)

`bbnf.asm:47-48` issues the dependency edge:
```
%include "x86inc.asm"
%include "x86util.asm"
```
Layer 0 has no symbol or macro reference back to Layer 1
(`grep -n "bbnf\|BYTE_CLASS_FROM\|BITMAP_PREFIX\|FRAME_PUSH" ext/x86/x86inc.asm ext/x86/x86util.asm` returns
zero hits). The dependency is therefore unambiguously one-directional;
Layer 1 may consume Layer 0 macros at expansion time but Layer 0 cannot
reference any Layer 1 symbol. This protects the upstream-vendor
provenance: any local Layer 0 patch would break the verbatim-copy
attribution at `LICENSE-VENDOR:5-43`.

### A4 — Lock 16 admission discipline per primitive

Per Lock 16 at `restart/locks/LOCKS.md:282-360` and the S-P2 V3 P2-B
5-stage admission process at
`restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:60-118`,
each Layer 1 primitive must carry:

1. **Scalar reference** in `crates/bbnf-simd/src/scalar/<name>.rs` —
   non-SIMD oracle, executable specification per `bbnf.asm:9-12`.
2. **Checkasm parity test** in `crates/bbnf-simd/tests/checkasm_<name>.rs`
   — differential parity vs scalar oracle for randomized inputs; strict
   mode `BBNF_SIMD_STRICT=1` mandatory per `LOCKS.md:320-322`.
3. **Same-wave consumer NAMED** — a runtime/codegen path that consumes
   the primitive in production within the same commit/wave that admits
   the kernel. Orphan kernels do not close Lock 16
   (`LOCKS.md:335-342`).
4. **Hardware gate** — explicit `target_feature` per primitive (BMI1,
   VPCLMULQDQ, VBMI, VBMI2, GFNI, BW, etc.) with a scalar-delegate
   fallback below the gate.
5. **Published citation** — paper / library source path:line / named
   technique. Confabulated citation = CH1 REJECT.

The 5-stage S-P2 process (Stage A scalar oracle, Stage B checkasm cell,
Stage C microbench, Stage D same-wave consumer, Stage E wave-close
disposition) gates every primitive admission. The P3-A 8-candidate
shortlist at
`restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:57-118`
consumes Layer 1 primitives by binding the canonical primitive names to
the 5-stage process — each shortlist row carries explicit (scalar-ref
status / checkasm-parity expectation / same-wave-consumer NAMED) cells.

### A5 — Audit at HEAD: 6/9 admitted shape vs. 3/9 skeleton-only

Cross-grep of `bbnf.asm` macro names against `src/scalar/` and
`tests/checkasm_*.rs` at HEAD:

| Layer 1 primitive | scalar ref at HEAD | checkasm test at HEAD | admission state |
|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | `src/scalar/byte_class_from_table_64.rs:2` | `tests/checkasm_byte_class_from_table_64.rs` | scalar + checkasm present; same-wave consumer required for full admit |
| `BYTE_CLASS_FROM_EQ_SET_64` | `src/scalar/byte_class_from_eq_set_64.rs:26` | `tests/checkasm_byte_class_from_eq_set_64.rs` | scalar + checkasm present; same-wave consumer required for full admit |
| `BITMAP_PREFIX_XOR_64` | `src/scalar/bitmap_prefix_xor_64.rs:2` | `tests/checkasm_bitmap_prefix_xor_64.rs` | scalar + checkasm present; aarch64 dispatch delegates to scalar per `src/aarch64/bitmap_prefix_xor_64.rs:1-4` — `scalar-delegate-non-ASM` is its current close state |
| `BITMAP_NEXT_SET_BIT` | `src/scalar/bitmap_next_set_bit.rs:2` | `tests/checkasm_bitmap_next_set_bit.rs` | scalar + checkasm present; aarch64 dispatch delegates to scalar per `src/aarch64/bitmap_next_set_bit.rs:1-4` — `scalar-delegate-non-ASM` is its current close state |
| `BULK_EMIT_COMPRESSED` | `src/scalar/bulk_emit_positions_64.rs:2` (named `bulk_emit_positions_64_scalar`) | `tests/checkasm_bulk_emit_positions_64.rs` | scalar + checkasm present; aarch64 delegates to scalar per `src/aarch64/bulk_emit_positions_64.rs:1-4` — `scalar-delegate-non-ASM` is its current close state |
| `EOB_PAD_CLAMP` | `src/scalar/eob_pad_clamp.rs:8` | `tests/checkasm_eob_pad_clamp.rs` | scalar + checkasm present; same-wave consumer required for full admit |
| `FSM_DISPATCH_THREADED` | **absent** | **absent** | **skeleton-contract only** — non-admissible until scalar oracle (switch-equivalent) + checkasm cell (state-sequence differential) exist |
| `FRAME_PUSH_BOUNDED` | **absent** | **absent** | **skeleton-contract only** — non-admissible until scalar oracle + `checkasm_frame_push_bounded.rs` exist |
| `FRAME_POP_BOUNDED` | **absent** | **absent** | **skeleton-contract only** — non-admissible until scalar oracle + `checkasm_frame_pop_bounded.rs` exist |

**Layer 0 count**: 138 macros total (72 in `x86inc.asm` + 66 in
`x86util.asm`). **Layer 1 count**: 9 contracts; 6/9 carry both
scalar reference and checkasm test; 3/9 are skeleton declarations
pending admission artefacts.

## Architectural Assertions Refuted

### R1 — Layer 0 includes pixel-domain kernels

The dossier defends Layer 0 as `x86inc.asm` + `x86util.asm` only.
Vendoring dav1d's motion-compensation, IDCT, loop-filter, or
film-grain `.asm` files into Layer 0 would be a Lock 14 violation: these
are pixel-domain kernels that do not transfer to byte-stream parsing.
Per Lock 16 v+1 at `restart/locks/LOCKS.md:305`: "dav1d's
pixel-arithmetic kernels do not translate to JSON (T14-T17 of the
catalog: motion compensation, IDCT, loop filter, film grain — all are
pixel-domain). But the *primitive operations* underneath them DO
translate." The lifted primitives belong in Layer 1 (e.g.
`BITMAP_PREFIX_XOR_64` is the VPCLMULQDQ primitive abstracted from
simdjson's quote-mask shape, not the simdjson source verbatim).

### R2 — Layer 1 may encode grammar-specific constants

`byte_class_from_eq_set_64` is correctly neutral because it accepts a
caller-supplied set and returns a mask. However, `classify_tbl4`'s
dispatch hardcodes JSON quote / backslash / control values at
`skinny/crates/bbnf-simd/src/dispatch.rs:22-33`. This is acceptable for
the current JSON caller but not for shared CSS or arbitrary-grammar use.
The SK-V13 `G-SIMD-GRAMMAR-POLICY` pre-block at
`restart/skinny/tranches/sk-v13/SYNTHESIS.md:239-263` is binding: shared
`bbnf-simd` consumers cannot inherit JSON constants into CSS, union,
parse-only, or shared generated code. T-P3 must require generated
grammar or caller-data policy at every non-JSON / shared call site, and
the Lock 16 manifest must record `policy_owner`.

### R3 — Skeleton macro presence closes Lock 16

`bbnf.asm` declares 9 primitive contracts. Three of those nine
(`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`)
have no scalar reference and no checkasm test at HEAD. The presence of
a macro contract in `bbnf.asm` is *not* admission. Per Lock 16
closing-state vocabulary at `LOCKS.md:335-342`: support-only modules,
unconsumed bodies, and orphan `asm!`/intrinsic files do not close Lock
16. The three skeleton contracts must either ship scalar oracle +
checkasm cell + same-wave consumer in a single wave, or be deleted from
`bbnf.asm` if no consumer is named within the SK-V14 horizon.

### R4 — x86-only Layer 1 contracts satisfy SK-V14 admission

`bbnf.asm` declares only x86 macro contracts; SK-V14 implementation
scope is aarch64 / Apple M5 Max. The x86 contracts inform the totality
primitive vocabulary but cannot close M5 Max admission rows. Per Lock
16 v+1 at `LOCKS.md:346-349`: "AVX-512 literature is x86 architecture
pressure and cannot close M5/aarch64 rows." The aarch64-counterpart
implementation lives at `crates/bbnf-simd/src/aarch64/*.rs`; admission
on aarch64 requires a NEON body for each Layer 1 primitive plus the
same checkasm/scalar discipline. The aarch64 Layer 1 vocabulary inherits
the same 9-primitive contract surface but is implementation-distinct.

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Does FSM_DISPATCH_THREADED admit a meaningful scalar oracle? | A switch-statement equivalent would behave identically to the indirect jump under random state-sequence inputs; build the oracle and the checkasm differential, then decide whether `FSM_DISPATCH_THREADED` admits or is replaced by codegen-emitted switch ladder. |
| Are FRAME_PUSH_BOUNDED / FRAME_POP_BOUNDED still required given current scalar Rust open_buf? | Audit `skinny/crates/runtime/src/` for an open-frames stack consumer; if no consumer exists, mark both macros `deleted` rather than ship admission artifacts for a contract no production caller uses. |
| Does the Layer 0 vendoring need a periodic upstream-resync gate? | Add a CI job that fetches dav1d HEAD and diffs `src/x86/x86inc.asm` against `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`; failure surfaces a lock-amendment candidate to bump the pinned SHA. |
| Are there `x86util.asm` macros bbnf will never use that should be deleted from Layer 0? | Static analysis of bbnf-bodies against `x86util.asm` macro inventory would identify dead helpers (e.g. `IDCT4_1D`, `STORE_DCT`); however Lock 16 vendor-verbatim discipline likely forbids selective subsetting. Confirm with T-P3. |
| Should the aarch64 Layer 1 vocabulary have its own `.S` macro file equivalent to `bbnf.asm`? | The aarch64 path currently uses Rust `core::arch::aarch64::*` intrinsics rather than a NEON macro file; evaluate whether an `aarch64/bbnf_neon.S` macro library would improve discipline parity with the x86 path. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2B-01 | Lock 16 | Pin Layer 0 vendoring SHA explicitly in `LICENSE-VENDOR`: add a footer recording dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (per `T-P2-V2-FOLD-ADDENDUM.md:29`) so the verbatim-copy claim is traceable without grepping fold addenda. | `LICENSE-VENDOR:9-13` (x264/FFmpeg attribution); fold addendum SHA pin. |
| LAC-2B-02 | Lock 16 | Require Layer 1 contracts to ship scalar reference + checkasm cell in the same commit they are declared in `bbnf.asm`. A contract with no scalar oracle and no checkasm test is non-admissible regardless of how complete the macro contract appears. | `bbnf.asm:30-44` (9 contracts); `src/scalar/` and `tests/checkasm_*` have only 6/9 backings; LOCKS.md:307. |
| LAC-2B-03 | Lock 14 / Lock 16 | Require `policy_owner` field on every Layer 1 consumer call site: `generated_grammar` (codegen emits the LUT/constants), `caller_data` (consumer supplies at runtime), or `none` (truly grammar-neutral, e.g. `BITMAP_PREFIX_XOR_64`). Reject shared call sites with hardcoded JSON constants. | `dispatch.rs:22-33` (JSON-hardcoded Tbl4 dispatch); SYNTHESIS.md:239-263 (`G-SIMD-GRAMMAR-POLICY`). |
| LAC-2B-04 | Lock 16 | Forbid Layer 0 modifications: any local diff vs upstream `x86inc.asm` / `x86util.asm` invalidates the verbatim-vendor provenance and requires an explicit lock amendment recording (a) the diff, (b) the upstream pin SHA, (c) the bbnf-specific reason. | LICENSE-VENDOR:48-53 (build determinism rationale); upstream pin in V2 addendum. |
| LAC-2B-05 | Lock 16 | Require an aarch64 Layer 1 vocabulary parity manifest: each of the 9 Layer-1 contracts must list its aarch64 body (`src/aarch64/<name>.rs`) and admission state, since SK-V14 implementation scope is aarch64 / M5 Max while `bbnf.asm` declares only x86 contracts. | bbnf.asm is x86-only; LOCKS.md:346-349 forbids x86 literature closing aarch64 rows. |

## Sources

Primary external sources:
- `SRC-DAV1D-HEAD`: dav1d upstream HEAD pin `1718ff9aded99f0a89f5c7940d6afb8948301e33` per `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:29`.
- `SRC-DAV1D-X86INC`: `https://code.videolan.org/videolan/dav1d/-/blob/1718ff9aded99f0a89f5c7940d6afb8948301e33/src/x86/x86inc.asm` (vendored verbatim into bbnf-simd at `ext/x86/x86inc.asm`).
- `SRC-X264-X86INC`: x264 project origin of `x86inc.asm` (https://www.videolan.org/developers/x264.html), ISC.
- `SRC-FFMPEG-X86UTIL`: FFmpeg `libavutil/x86/x86util.asm` (LGPL-2.1-or-later, header-only-include exception).
- `SRC-FFMPEG-CHECKASM-C`: https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html (`checkasm_check_func` / `checkasm_bench_func`).
- `SRC-FFMPEG-CHECKASM-H`: https://www.ffmpeg.org/doxygen/8.0/checkasm_8h_source.html.
- `SRC-VIDEOLAN-CHECKASM-PAGE`: https://www-test.videolan.org/projects/checkasm/.
- `SRC-ARM-ACLE`: Arm C Language Extensions 2026Q1 — https://arm-software.github.io/acle/main/acle.html.
- `SRC-ARM-NEON-INTR`: Arm NEON Intrinsics Reference — https://arm-software.github.io/acle/neon_intrinsics/advsimd.html.
- `SRC-SIMDJSON-PAPER`: Langdale & Lemire, "Parsing Gigabytes of JSON per Second", Software: Practice & Experience 49(8):1109-1135 (2019), §3.1 quote-mask construction.

Primary local sources:
- `SRC-LOCKS-16`: `restart/locks/LOCKS.md:282-360` (Lock 16 SIMD/ASM admissibility allowlist).
- `SRC-BBNF-ASM`: `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-486` (Layer 1 contract declarations).
- `SRC-BBNF-X86INC`: `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:1-2030` (Layer 0, x264-origin).
- `SRC-BBNF-X86UTIL`: `skinny/crates/bbnf-simd/ext/x86/x86util.asm:1-1100` (Layer 0, FFmpeg-origin).
- `SRC-BBNF-LICENSE-VENDOR`: `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:1-55` (vendor attribution).
- `SRC-BBNF-SCALAR`: `skinny/crates/bbnf-simd/src/scalar/*.rs` (six scalar references at HEAD).
- `SRC-BBNF-CHECKASM`: `skinny/crates/bbnf-simd/tests/checkasm_*.rs` (six checkasm parity tests at HEAD).
- `SRC-S-P2-V3`: `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:60-118` (5-stage admission process); `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:57-118` (8-candidate shortlist consuming Layer 1).
