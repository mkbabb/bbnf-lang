---
agent: 2B
pass: T-P2-research
cycle: V3
generated_at: 2026-05-23T00:00:00Z
v3_fold_packet_consumed:
  - F-CH5-V2-01 (per-primitive substrate_target + retention_lifetime columns added to §A5 audit table + §A6 cost ledger; 6 admitted primitives + 3 SKELETON N/A rows; Lock 1 v+1 + LAC-2F-V5-02 binding)
v2_fold_packet_consumed:
  - CH4-F1 (SKELETON triple binary disposition — DELETE recommended; no consumer)
  - CH4-F2 (marker-string lowerers documented as candidate refutation R5)
  - CH4-F4 (per-candidate adoption-cost ledger populated for 9 Layer-1 contracts)
  - CH2-V1-item-5 (6 aarch64 scalar-delegate-non-ASM close states classified atomically)
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 18
counted_source_ids: [SRC-DAV1D-HEAD, SRC-DAV1D-X86INC, SRC-X264-X86INC, SRC-FFMPEG-X86UTIL, SRC-FFMPEG-CHECKASM-C, SRC-FFMPEG-CHECKASM-H, SRC-VIDEOLAN-CHECKASM-PAGE, SRC-ARM-ACLE, SRC-ARM-NEON-INTR, SRC-SIMDJSON-PAPER, SRC-LOCKS-16, SRC-BBNF-ASM, SRC-BBNF-X86INC, SRC-BBNF-X86UTIL, SRC-BBNF-LICENSE-VENDOR, SRC-BBNF-SCALAR, SRC-BBNF-CHECKASM, SRC-S-P2-V3]
techniques_grounded: 11
techniques_refuted: 5
techniques_partial: 3
prior_cycle_dispositions_folded:
  accepted:
    - 2B-layer0-vendored-corpus-pin
    - 2B-layer1-9-primitive-vocabulary
    - 2B-one-directional-dependency
    - 2B-admission-disciplne-per-lock-16
    - 2B-fsm-frame-scalar-checkasm-gap
  rejected: []
  revised:
    - 2B-SKELETON-triple-binary-disposition-delete (CH4-F1, V1 REJECT-IN-PART → V2 DELETE-RECOMMENDED)
    - 2B-marker-string-lowerers-as-candidate-refutation (CH4-F2, V1 silent → V2 §R5 explicit)
    - 2B-aarch64-close-state-atomic-classification (CH2-V1-item-5, V1 implicit → V2 §A5 per-primitive cells)
  first_cycle_additions:
    - 2B-layer1-adoption-cost-ledger (CH4-F4, 9 rows, 8-cell manifest schema per T2A-LAC-V1-03)
locks_amendment_candidates: 7
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
and are non-admissible. **V2 disposition (CH4-F1): DELETE the SKELETON
triple from `bbnf.asm`.** No production consumer for any of the three
exists at HEAD: `grep -rn "frame_push\|frame_pop\|open_buf\|FRAME_PUSH\|
FRAME_POP" skinny/crates/runtime/src/` returns zero hits beyond fixture
strings, and `grep -rn "FSM_DISPATCH_THREADED\|fsm_dispatch_threaded"
skinny/crates/codegen/src/` returns zero hits. With no SK-V14-horizon
consumer named, OQ-2's verify-action collapses to the deletion branch
per Lock 16 v+1 close-state vocabulary at `LOCKS.md:335-342` (`deleted`
is one of the four admissible close states; `skeleton-contract-only` is
not). The deletion stanza ships in the same V2 wave that lands the
adoption-cost ledger; if a same-wave consumer is named after V2 close,
the contract may be re-introduced under the standard 5-stage admission
process. P3-A's 8-candidate shortlist consumes Layer 1 through the S-P2
V3 P2-B 5-stage admission process and does NOT reference the SKELETON
triple — the shortlist is closed against the 6-primitive admitted
surface only.

**V2 fold packet items folded:** (CH4-F1) SKELETON triple binary
disposition resolved to DELETE at §R3 + §A6; (CH4-F2) 4-of-5 marker-string
BackendShape lowerers documented as candidate refutation at §R5;
(CH4-F4) per-candidate adoption-cost ledger populated at §A6 for all 9
Layer-1 contracts using T2A-LAC-V1-03's eight-cell manifest schema;
(CH2-V1 item 5) the 6 aarch64 scalar-delegate-non-ASM close states
classified atomically at §A5 per-primitive cells.

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

### A5 — Audit at HEAD: 6/9 admitted shape vs. 3/9 skeleton-only; atomic per-primitive aarch64 close state

Cross-grep of `bbnf.asm` macro names against `src/scalar/`,
`src/aarch64/`, `src/x86_64/`, and `tests/checkasm_*.rs` at HEAD.
Verification commands: `grep -cE "^%macro " skinny/crates/bbnf-simd/
ext/x86/bbnf.asm` returns 9; `ls skinny/crates/bbnf-simd/src/scalar/
*.rs | wc -l` returns 6 primitive bodies + `mod.rs` + `swar_8byte.rs`;
`ls skinny/crates/bbnf-simd/tests/checkasm_*.rs | wc -l` returns 9
(6 Layer-1 cells + `checkasm_ascii_set_member_find_64.rs` +
`checkasm_escape_mask_64.rs` + `checkasm_structural_terminator_64.rs`).

The **aarch64 close state column** is the atomic classification CH2 V1
item 5 required. Per Lock 16 v+1 close-state vocabulary at
`LOCKS.md:335-342` ("`wired`, `deleted`, `scalar-delegate-non-ASM`, or
`architectural-block-with-REDRESS`"), each primitive's aarch64 row is
exactly one of `ASM-admitted` (NEON intrinsic body present),
`scalar-delegate-non-ASM` (aarch64 file forwards to scalar reference),
or `pending-aarch64-port` (no aarch64 body at all). The
`scalar-delegate-non-ASM` state is admissible per Lock 16 v+1 ONLY when
the scalar reference itself is the executable specification AND the
delegation is intentional (e.g. the primitive's optimal NEON shape
matches the scalar shape, or NEON does not improve on scalar
throughput for this primitive's data dependency graph).

The **substrate_target** and **retention_lifetime** columns (V3 fold
F-CH5-V2-01) bind the Lock 1 v+1 substrate-union manifest per primitive,
making the per-row substrate disposition self-contained rather than
relying on the cohort-wide V2-FOLD §Lock 1 contract
(`T-P2-V2-FOLD-ADDENDUM.md:77-97`) being read in conjunction. Values:
`substrate_target ∈ {local_temp_only, existing_tape, direct_sink,
admitted_fact_output}`; `retention_lifetime ∈
{transient-single-call, transient-multi-call-bounded,
retained-across-call-boundary}` per LAC-2F-V5-02 (the third value is
REJECT class — "no cross-call retained classifier state … prefix-XOR
carry word"; carry MUST stay within a single chunk-call boundary).
All six admitted Layer-1 primitives close at `transient-single-call`;
4 of 6 (BYTE_CLASS_FROM_TABLE_64, BYTE_CLASS_FROM_EQ_SET_64,
BITMAP_PREFIX_XOR_64, BITMAP_NEXT_SET_BIT) carry `local_temp_only`
substrate; 2 of 6 (BULK_EMIT_COMPRESSED, EOB_PAD_CLAMP) carry
`direct_sink` because the consumer's output buffer IS the sink. The
3 SKELETON contracts carry `N/A` (DELETED per CH4-F1; had any
survived, FRAME_PUSH_BOUNDED / FRAME_POP_BOUNDED would themselves be
substrate-union violations as parser-owned cursor/list state).

| Layer 1 primitive | scalar ref at HEAD | checkasm test at HEAD | x86_64 close state | aarch64 close state | substrate_target (Lock 1 v+1) | retention_lifetime (LAC-2F-V5-02) | atomic V2 disposition |
|---|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | `src/scalar/byte_class_from_table_64.rs:2` | `tests/checkasm_byte_class_from_table_64.rs` | `pending-x86_64-port` (no body at `src/x86_64/byte_class_from_table_64.rs`; dispatch falls through to scalar via `src/dispatch.rs:79`) | **`scalar-delegate-non-ASM`** (`src/aarch64/byte_class_from_table_64.rs:1-4` forwards verbatim to scalar) | `local_temp_only` (256-byte LUT consumed in-loop; output mask is per-chunk scalar/SIMD temporary, never retained across the chunk-call boundary) | `transient-single-call` (mask + LUT lookup live within one classifier inner-loop call; LAC-2F-V5-02 forbids cross-call carry) | scalar-backed + checkasm-backed; same-wave consumer NAMED (`bbnf_simd::scan_dispatch` at `crates/bbnf-simd/src/lib.rs:114`); awaits NEON `vqtbl4q_u8` port to reach `wired` state on aarch64 |
| `BYTE_CLASS_FROM_EQ_SET_64` | `src/scalar/byte_class_from_eq_set_64.rs:26` | `tests/checkasm_byte_class_from_eq_set_64.rs` | `ASM-admitted` (`src/x86_64/byte_class_from_eq_set_64.rs` + `byte_class_from_eq_set_64.asm` body present; admitted via AVX-512BW gate at `crates/bbnf-simd/src/lib.rs:286`) | **`ASM-admitted`** (genuine NEON intrinsic body at `src/aarch64/byte_class_from_eq_set_64.rs:33-90` using `vceqq_u8` + `vorrq_u8` reduction tree; not a delegate stub) | `local_temp_only` (≤8-byte set fan-out and reduction-tree mask are register-resident inside the call; output mask is consumed by the in-loop next-set-bit driver, never retained) | `transient-single-call` (set + intermediate `vceqq_u8` lanes + reduced mask all bounded by one classifier-loop call; LAC-2F-V5-02 bind) | scalar-backed + checkasm-backed + ASM-backed on both ISAs; same-wave consumer NAMED (`bbnf_simd::find_ascii_set_member64` at `crates/bbnf-simd/src/lib.rs:209-226`); `wired` close state achieved on aarch64 |
| `BITMAP_PREFIX_XOR_64` | `src/scalar/bitmap_prefix_xor_64.rs:2` | `tests/checkasm_bitmap_prefix_xor_64.rs` | `pending-x86_64-port` (VPCLMULQDQ body specified by `bbnf.asm:174-175` not yet emitted; dispatch falls through scalar at `src/dispatch.rs`) | **`scalar-delegate-non-ASM`** (`src/aarch64/bitmap_prefix_xor_64.rs:1-4` forwards to scalar; the PMULL/CSSC NEON port is a REDRESS-88-bounded route per `LOCKS.md:282-360` + the aarch64 close state is partial pending S-P2 V3 C-P2C-2 admission) | `local_temp_only` (ripple-XOR carry word + 64-bit toggled mask are register-resident temporaries inside the chunk-call; output mask is consumed by the same-loop string/escape classifier, never published as a sidecar) | `transient-single-call` (per LAC-2F-V5-02 binding contract: "no cross-call retained classifier state … prefix-XOR carry word"; carry MUST stay within a single chunk-call boundary) | scalar-backed + checkasm-backed; same-wave consumer NAMED (`bbnf_simd::escape_mask_64` callers + `runtime::grammars::json::scan` at `crates/runtime/src/grammars/json/scan.rs:239`); aarch64 promotion gated on REDRESS-88 PMULL hot-body admission |
| `BITMAP_NEXT_SET_BIT` | `src/scalar/bitmap_next_set_bit.rs:2` | `tests/checkasm_bitmap_next_set_bit.rs` | `pending-x86_64-port` (BMI1 `tzcnt` body not yet emitted at `src/x86_64/`; dispatch falls to scalar `u64::trailing_zeros` equivalent) | **`scalar-delegate-non-ASM`** (`src/aarch64/bitmap_next_set_bit.rs:1-4` forwards to scalar; the aarch64 `RBIT + CLZ` two-instruction equivalent or CSSC `CTZ` per REDRESS-89 is a S-P2 V3 C-P2C-2 admission row, not yet wired) | `local_temp_only` (cursor + 64-bit mask + `tzcnt` output offset are register-resident temporaries inside the dispatch loop; bitmap is consumed-and-cleared in-loop, never retained as a sidecar across calls) | `transient-single-call` (cursor advancement + next-set offset bounded by one dispatch-loop call; LAC-2F-V5-02 bind on classifier-state retention) | scalar-backed + checkasm-backed; same-wave consumer NAMED (`bbnf_simd` internal dispatch + downstream readers via `prim::bitmap_next_set_bit` at `lib.rs:265`); aarch64 promotion gated on REDRESS-89 CSSC CTZ bulk admission |
| `BULK_EMIT_COMPRESSED` | `src/scalar/bulk_emit_positions_64.rs:2` (named `bulk_emit_positions_64_scalar`) | `tests/checkasm_bulk_emit_positions_64.rs` | `pending-x86_64-port` (VBMI2 `vpcompressb` body specified by `bbnf.asm:241-261` not yet emitted; dispatch falls to scalar) | **`scalar-delegate-non-ASM`** (`src/aarch64/bulk_emit_positions_64.rs:1-4` forwards to scalar; there is no clean NEON equivalent for VBMI2 mask-compress-store at 64-bit lane — the aarch64 close state is `scalar-delegate-non-ASM` BY DESIGN, not pending-port; the scalar `bulk_emit_positions_64_scalar` is the executable spec on aarch64) | `direct_sink` (compressed bytes/positions are written directly to the caller-owned sink — `compact_mask` consumer's output buffer — not buffered through any intermediate parallel substrate) | `transient-single-call` (mask + 64B source vector live within one compaction call; sink-store completes before return; no retained state) | scalar-backed + checkasm-backed; same-wave consumer NAMED (`bbnf_simd::compact_mask` at `crates/bbnf-simd/src/lib.rs:228-243`); aarch64 close state is **terminal `scalar-delegate-non-ASM`** (no NEON 1-op equivalent to VBMI2) |
| `EOB_PAD_CLAMP` | `src/scalar/eob_pad_clamp.rs:8` | `tests/checkasm_eob_pad_clamp.rs` | `pending-x86_64-port` (AVX-512BW + BMI2 body specified by `bbnf.asm:296-309` not yet emitted) | **`scalar-delegate-non-ASM`** (`src/aarch64/eob_pad_clamp.rs:1-7` re-exports `EobBlock` from scalar and forwards `eob_pad_clamp_neon` to scalar; the dav1d msac tail-handling shape per `bbnf.asm:296-309` is byte-copy + zero-pad, which scalar implements optimally on aarch64; NEON memcpy alignment helpers add no measurable win for this primitive's data-dependency graph) | `direct_sink` (tail bytes are byte-copied + zero-padded into the caller-supplied 64B `EobBlock` buffer; no parallel substrate or sidecar — the padded buffer IS the sink for tail-block consumers) | `transient-single-call` (pad-clamp is invoked once per chunk-tail; output buffer lifetime is owned by the cursor-tail handler at `bbnf_simd::scan_dispatch lib.rs:118-122`; no cross-call carry) | scalar-backed + checkasm-backed; same-wave consumer NAMED (`bbnf_simd::prim::eob_pad_clamp` at `lib.rs:275`); aarch64 close state is **terminal `scalar-delegate-non-ASM`** by design (scalar is optimal for byte-copy + zero-pad on aarch64) |
| `FSM_DISPATCH_THREADED` | **absent** | **absent** | `skeleton-contract only` — V2 disposition: **DELETE** from `bbnf.asm` (no codegen-emitted CollapsedStage consumer at HEAD per `grep -rn "FSM_DISPATCH_THREADED" skinny/crates/codegen/src/` returning zero hits; CH4-F1 binary disposition resolves to deletion under Lock 16 v+1 close-state vocabulary) | `skeleton-contract only` — V2 disposition: **DELETE** | **N/A** (contract DELETED per CH4-F1; no substrate to declare) | **N/A** (contract DELETED per CH4-F1; no retention scope) | scalar absent + checkasm absent + same-wave consumer absent; **V2 ACTION: delete contract declaration from `bbnf.asm:355-363`** per CH4-F1 |
| `FRAME_PUSH_BOUNDED` | **absent** | **absent** | `skeleton-contract only` — V2 disposition: **DELETE** from `bbnf.asm` (no `open_buf` / `frames_buf` consumer in `skinny/crates/runtime/src/` per `grep -rn "frame_push\|open_buf\|frames_buf" skinny/crates/runtime/src/` returning zero hits beyond CSS keyframe fixture strings; OQ-2 verify-action collapses to deletion under Lock 16 v+1) | `skeleton-contract only` — V2 disposition: **DELETE** | **N/A** (contract DELETED per CH4-F1; note: had the contract survived, a parser-owned `open_buf` frame stack would itself be a Lock 1 substrate-union violation per LAC-2F-V5-02 — "parser-owned cursor/list state" is REJECT) | **N/A** (contract DELETED per CH4-F1; a frame stack would have required `retained-across-call-boundary`, the REJECT class under Lock 1 v+1) | scalar absent + checkasm absent + same-wave consumer absent; **V2 ACTION: delete contract declaration from `bbnf.asm:404-418`** per CH4-F1 |
| `FRAME_POP_BOUNDED` | **absent** | **absent** | `skeleton-contract only` — V2 disposition: **DELETE** from `bbnf.asm` (companion to FRAME_PUSH_BOUNDED; no `open_buf` pop consumer in `skinny/crates/runtime/src/`; deletion is per-pair binary disposition) | `skeleton-contract only` — V2 disposition: **DELETE** | **N/A** (contract DELETED per CH4-F1; same parser-owned-stack substrate violation as FRAME_PUSH_BOUNDED applies had the contract survived) | **N/A** (contract DELETED per CH4-F1; companion to FRAME_PUSH_BOUNDED) | scalar absent + checkasm absent + same-wave consumer absent; **V2 ACTION: delete contract declaration from `bbnf.asm:454-468`** per CH4-F1 |

**Aarch64 close-state census (CH2 V1 item 5 atomic resolution):**

- **1 of 6 admitted primitives reaches `ASM-admitted` on aarch64**:
  `BYTE_CLASS_FROM_EQ_SET_64` (genuine NEON intrinsic body at
  `src/aarch64/byte_class_from_eq_set_64.rs:33-90`).
- **2 of 6 admitted primitives are TERMINAL `scalar-delegate-non-ASM`
  by design on aarch64** (no NEON improvement is available or
  desirable): `BULK_EMIT_COMPRESSED` (no NEON 1-op equivalent for
  VBMI2 mask-compress-store at 64-bit lane), `EOB_PAD_CLAMP` (scalar
  is optimal for byte-copy + zero-pad).
- **2 of 6 admitted primitives are `pending-aarch64-port` gated on
  REDRESS-88/89 admission**: `BITMAP_PREFIX_XOR_64` (PMULL hot body
  per REDRESS-88; S-P2 V3 C-P2C-2 candidate), `BITMAP_NEXT_SET_BIT`
  (CSSC CTZ bulk per REDRESS-89; S-P2 V3 C-P2C-2 candidate).
- **1 of 6 admitted primitives is `pending-aarch64-port` on standard
  primitive-lift route** (no REDRESS gate): `BYTE_CLASS_FROM_TABLE_64`
  (awaits NEON `vqtbl4q_u8` 4-table lookup port; Lemire 2019 lineage
  per `LOCKS.md:283`).
- **3 of 9 contracts are `skeleton-contract only`** (V2: DELETE);
  irrelevant for aarch64 close-state census.

The atomic classification eliminates the V1 imprecision that read 6/9
as uniformly `scalar-delegate-non-ASM`. The V2 truth is heterogeneous:
1 aarch64 ASM-admitted (`BYTE_CLASS_FROM_EQ_SET_64`), 2 terminal
scalar-delegate (`BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`), 2 REDRESS-
gated pending-port (`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`), 1
standard pending-port (`BYTE_CLASS_FROM_TABLE_64`), 3 DELETE
(SKELETON triple).

**Layer 0 count**: 138 macros total (72 in `x86inc.asm` + 66 in
`x86util.asm`). **Layer 1 count post-V2**: 6 contracts after SKELETON
triple deletion; 6/6 carry both scalar reference and checkasm test;
1/6 reaches `ASM-admitted` on aarch64; 2/6 terminal scalar-delegate
on aarch64; 3/6 pending-port on aarch64 (2 REDRESS-gated, 1 standard).

### A6 — Per-candidate adoption-cost ledger (CH4-F4 V2 fold)

Populates the eight-cell manifest schema from T2A-LAC-V1-03
(`restart/audit/totality/p2/2A-sota-landscape.md:132`) extended at V3
fold F-CH5-V2-01 with the Lock 1 v+1 substrate-union manifest pair
(`T-P2-V2-FOLD-ADDENDUM.md:77-97`) for all 9 Layer-1 contracts. Each
row carries: (a) abstract primitive name + ISA citation, (b)
published citation, (c) hardware gate, (d) scalar reference path:line,
(e) checkasm differential cell path:line, (f) corpus parity test,
(g) same-wave production consumer at `crate::module::function`
granularity (per CH4-F6), (h) `substrate_target ∈ {local_temp_only,
existing_tape, direct_sink, admitted_fact_output}` per Lock 1 v+1, (i)
`retention_lifetime ∈ {transient-single-call,
transient-multi-call-bounded, retained-across-call-boundary}` per
LAC-2F-V5-02 (the third value is REJECT class under Lock 1 v+1), (j)
row admission or measured rejection state. Strict-mode flag
`BBNF_SIMD_STRICT=1` is mandatory cohort-wide per `LOCKS.md:320-322`
(CH4-F5).

The ledger uses the V2 normalised admission-state vocabulary per
CH4-F8 (the eight states the cohort-wide normalisation defines):
`source-present-only`, `scalar-backed`, `checkasm-backed`,
`micro-proven`, `wave-admitted`, `row-admitted`, `measured-rejected`,
`architectural-block-with-REDRESS`. Lock 16 v+1's four close states
(`wired`, `deleted`, `scalar-delegate-non-ASM`,
`architectural-block-with-REDRESS`) map cleanly onto the last four
admitted plus `deleted`.

| # | abstract primitive + ISA citation | published citation | hardware gate | scalar reference (path:line) | checkasm cell (path:line) | corpus parity | same-wave consumer (crate::module::function) | substrate_target (Lock 1 v+1) | retention_lifetime (LAC-2F-V5-02) | row admission / measured rejection |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `BYTE_CLASS_FROM_TABLE_64` — 64B → mask via 256-byte LUT (`vpermb`/GFNI on x86; `vqtbl4q_u8` on aarch64) | asmjson per-state byte mask; dav1d film-grain classifier per `bbnf.asm:93-96`; Lemire 2019 NEON 4-table lookup per `LOCKS.md:283` | AVX-512BW (+ VBMI \| GFNI optional) on x86_64; NEON `vqtbl4q_u8` on aarch64 (Armv8 baseline) | `crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:2` | `crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs` | `corpus_parity.rs` + `classifier_parity.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::scan_dispatch` at `crates/bbnf-simd/src/lib.rs:114` (downstream: `bbnf_simd::structural_index::*`) | `local_temp_only` (256-byte LUT + per-chunk mask consumed in-loop; no parallel substrate) | `transient-single-call` (mask + LUT lookup bounded by one classifier-loop call; LAC-2F-V5-02 bind) | scalar-backed + checkasm-backed; x86 ASM body pending; aarch64 `scalar-delegate-non-ASM` (V2 close state); awaits NEON `vqtbl4q_u8` body to reach `row-admitted` on aarch64 |
| 2 | `BYTE_CLASS_FROM_EQ_SET_64` — 64B → mask via ≤8-byte set fan-out (`vpcmpeqb`+`korq` on x86; `vceqq_u8`+`vorrq_u8` on aarch64) | asmjson `classify_chunk` inner loop per `bbnf.asm:134-137`; Validark byte-class-fan + dav1d byte-stripe lineage per `aarch64/byte_class_from_eq_set_64.rs:8-19` | AVX-512BW on x86_64; NEON baseline on aarch64 (Armv8) | `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:26` | `crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs` | `corpus_parity.rs` + `classifier_parity.rs` + `aarch64_primitives.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::find_ascii_set_member64` at `crates/bbnf-simd/src/lib.rs:209-226` (downstream: scanner inner-loop consumers in `runtime::grammars::*::scan`) | `local_temp_only` (set fan-out lanes + reduction-tree mask register-resident; consumed by in-loop next-set-bit driver) | `transient-single-call` (set + `vceqq_u8` intermediates + reduced mask all bounded by one classifier-loop call) | scalar-backed + checkasm-backed + ASM-backed both ISAs; `wave-admitted` x86 + aarch64; `row-admitted` pending S-P2 V3 P3-A wave consumer measurement |
| 3 | `BITMAP_PREFIX_XOR_64` — 64b bitmap → ripple-XOR via VPCLMULQDQ (x86) / PMULL (aarch64) / scalar carry-chain (fallback) | simdjson "Parsing Gigabytes of JSON per Second" §3.1 (Langdale & Lemire 2019) per `bbnf.asm:174-175`; Intel CLMUL Whitepaper; REDRESS-88 PMULL hot-body per `LOCKS.md` | VPCLMULQDQ on x86_64 (Ice Lake+); PMULL on aarch64 (Armv8 + AES feature; REDRESS-88-gated); scalar fallback | `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2` | `crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs` | `corpus_parity.rs` + `classifier_parity.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::prefix_xor_64` at `crates/bbnf-simd/src/lib.rs:170-172` (downstream consumer: `runtime::grammars::json::scan::*` at `crates/runtime/src/grammars/json/scan.rs:239`) | `local_temp_only` (carry word + toggled mask register-resident; consumed by same-loop string/escape classifier) | `transient-single-call` (per LAC-2F-V5-02: "no cross-call retained classifier state … prefix-XOR carry word"; carry MUST stay within a single chunk-call boundary) | scalar-backed + checkasm-backed + `row-admitted` (consumer at `scan.rs:239` already wired); x86 VPCLMULQDQ body pending; aarch64 `scalar-delegate-non-ASM` pending REDRESS-88 PMULL admission (S-P2 V3 C-P2C-2) |
| 4 | `BITMAP_NEXT_SET_BIT` — 64b bitmap + cursor → next-set offset via `tzcnt` (x86) / `RBIT`+`CLZ` or CSSC `CTZ` (aarch64) | asmjson `classify_chunk` dispatch per `bbnf.asm:215-217`; Intel BMI1 ISA reference; REDRESS-89 CSSC CTZ bulk per `LOCKS.md` | BMI1 on x86_64 (universal on AVX-512 hosts); aarch64 baseline (RBIT+CLZ on Armv8) or CSSC (Armv9.4-A) | `crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:2` | `crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs` | `corpus_parity.rs` + `aarch64_primitives.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::prim::bitmap_next_set_bit` at `crates/bbnf-simd/src/lib.rs:265-267` (downstream consumer: classifier dispatch loops in `bbnf_simd::scan_dispatch` + `runtime::grammars::*::scan`) | `local_temp_only` (cursor + 64-bit mask + offset register-resident; bitmap consumed-and-cleared in-loop) | `transient-single-call` (cursor advancement + offset bounded by one dispatch-loop call; LAC-2F-V5-02 bind on classifier-state retention) | scalar-backed + checkasm-backed; x86 BMI1 body pending; aarch64 `scalar-delegate-non-ASM` pending REDRESS-89 CSSC admission (S-P2 V3 C-P2C-2) |
| 5 | `BULK_EMIT_COMPRESSED` — 64B + mask → compressed sink via `vpcompressb` (x86 VBMI2) / scalar gather-store (aarch64; no NEON 1-op equivalent at 64-bit lane) | simdjson tape-builder VBMI2 fork per `bbnf.asm:256-261`; Lemire 2022 VBMI2 per `LOCKS.md:292` | AVX-512 VBMI2 on x86_64 (Ice Lake+ Intel, Zen 4+ AMD); scalar fallback on aarch64 (terminal) | `crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:2` (named `bulk_emit_positions_64_scalar`) | `crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs` | `corpus_parity.rs` + `classifier_parity.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::compact_mask` at `crates/bbnf-simd/src/lib.rs:228-243` (downstream consumer: `bbnf_simd::scan_dispatch` at `lib.rs:115`) | `direct_sink` (compressed bytes/positions written directly to caller-owned `compact_mask` output buffer; no intermediate parallel substrate) | `transient-single-call` (mask + 64B source live within one compaction call; sink-store completes before return) | scalar-backed + checkasm-backed + `row-admitted` (compact_mask consumer at `lib.rs:228-243` wired); x86 VBMI2 body pending; aarch64 terminal `scalar-delegate-non-ASM` by design (no NEON 1-op equivalent) |
| 6 | `EOB_PAD_CLAMP` — tail bytes → zero-padded 64B vector via masked load + `vpxorq` zero-fill (x86) / byte-copy + zero-pad (aarch64; scalar optimal) | dav1d msac tail-handling per `bbnf.asm:296-309`; simdjson padded-buffer convention; dav1d `src/x86/msac.asm:80-220` cross-chunk refill per `LOCKS.md:305` | AVX-512BW + BMI2 on x86_64; scalar fallback on aarch64 (terminal; scalar is optimal for byte-copy + zero-pad) | `crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:8` | `crates/bbnf-simd/tests/checkasm_eob_pad_clamp.rs` | `corpus_parity.rs` under `BBNF_SIMD_STRICT=1` | `bbnf_simd::prim::eob_pad_clamp` at `crates/bbnf-simd/src/lib.rs:275-277` (downstream consumer: tail-block handlers in `bbnf_simd::scan_dispatch` cursor-tail logic at `lib.rs:118-122`) | `direct_sink` (tail bytes byte-copied + zero-padded into caller-supplied 64B `EobBlock` buffer; padded buffer IS the sink for tail-block consumers) | `transient-single-call` (pad-clamp invoked once per chunk-tail; output buffer lifetime owned by cursor-tail handler; no cross-call carry) | scalar-backed + checkasm-backed; x86 AVX-512BW + BMI2 body pending; aarch64 terminal `scalar-delegate-non-ASM` by design |
| 7 | `FSM_DISPATCH_THREADED` — state-as-PC threaded dispatch (`jmp [tbl+r10*8]`); baseline x86_64 | asmjson r10-PC dispatch core per `bbnf.asm:357-363`; Bell 1973 "Threaded Code" (originating ISA-level pattern) | baseline x86_64 (no SIMD); no aarch64 equivalent in current bbnf scope | **absent** (no scalar oracle at HEAD) | **absent** (no checkasm cell at HEAD) | n/a (cannot run parity without scalar oracle) | **none named** at HEAD (no codegen-emitted CollapsedStage consumer per `grep -rn "FSM_DISPATCH_THREADED" skinny/crates/codegen/src/` zero hits) | **N/A** (contract DELETED per CH4-F1) | **N/A** (contract DELETED per CH4-F1) | `deleted` (V2 disposition: DELETE per CH4-F1; no SK-V14-horizon consumer named) |
| 8 | `FRAME_PUSH_BOUNDED` — push FrameKind onto open_buf with bounds check; baseline x86_64 | asmjson `frames_buf` / `open_buf` bounded stack per `bbnf.asm:411-417` | baseline x86_64; no aarch64 equivalent declared | **absent** | **absent** | n/a | **none named** at HEAD (no `open_buf` / `frames_buf` consumer in `skinny/crates/runtime/src/` per `grep -rn "open_buf\|frames_buf" skinny/crates/runtime/src/` zero hits beyond CSS keyframe fixture strings) | **N/A** (contract DELETED per CH4-F1; a surviving parser-owned `open_buf` frame stack would itself be a Lock 1 substrate-union violation — REJECT class) | **N/A** (contract DELETED per CH4-F1; a frame stack would have required `retained-across-call-boundary`, the REJECT class under Lock 1 v+1 per LAC-2F-V5-02) | `deleted` (V2 disposition: DELETE per CH4-F1; OQ-2 verify-action collapses to deletion branch) |
| 9 | `FRAME_POP_BOUNDED` — pop FrameKind + close-bracket validate; baseline x86_64 | asmjson `open_buf` pop + bracket validate per `bbnf.asm:464-468` | baseline x86_64; no aarch64 equivalent declared | **absent** | **absent** | n/a | **none named** at HEAD (companion to FRAME_PUSH_BOUNDED; deletion is per-pair binary disposition) | **N/A** (contract DELETED per CH4-F1; companion to FRAME_PUSH_BOUNDED) | **N/A** (contract DELETED per CH4-F1; companion to FRAME_PUSH_BOUNDED) | `deleted` (V2 disposition: DELETE per CH4-F1) |

**Adoption-cost LOC envelopes (CH4-F7) and rollback paths**: rows 1-6
are scalar-implementation-anchored at HEAD; LOC envelopes for missing
ASM bodies are: row 1 x86 VPERMB body ≈ 30-50 LOC + GFNI alt ≈ 40-60
LOC; row 1 aarch64 `vqtbl4q_u8` body ≈ 25-40 LOC; row 3 x86
VPCLMULQDQ body ≈ 40-60 LOC; row 3 aarch64 PMULL body (REDRESS-88
gated) ≈ 50-80 LOC; row 4 x86 BMI1 body ≈ 15-25 LOC; row 4 aarch64
RBIT+CLZ body ≈ 15-25 LOC, CSSC CTZ ≈ 10-15 LOC (REDRESS-89 gated);
row 5 x86 VBMI2 body ≈ 30-50 LOC; row 6 x86 AVX-512BW masked-load body
≈ 50-80 LOC. Rollback path per row: feature-gate-off via
`cfg(target_feature = ...)` block; the scalar reference is the
zero-cost rollback target. Touched crates per row admission: bbnf-simd
exclusively for kernel admission; downstream consumer rows touch
runtime + codegen.

**Cohort-wide cells normalised per CH4-F5/F6/F8**: every row carries
`BBNF_SIMD_STRICT=1` precondition (CH4-F5), every consumer named at
`crate::module::function` granularity (CH4-F6), every admission state
drawn from the 8-state V2-normalised vocabulary (CH4-F8). Lock 16 v+1
close-state mapping: 1 `wave-admitted` (row 2 BYTE_CLASS_FROM_EQ_SET_64
aarch64), 3 `row-admitted` (rows 2, 3, 5 on consumer-wired basis), 6
`scalar-backed + checkasm-backed` shared baseline, 3 `deleted` (rows
7-9 SKELETON triple V2 disposition).

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

### R3 — Skeleton macro presence closes Lock 16 (V2: DELETE enacted)

`bbnf.asm` declares 9 primitive contracts. Three of those nine
(`FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`)
have no scalar reference and no checkasm test at HEAD. The presence of
a macro contract in `bbnf.asm` is *not* admission. Per Lock 16
closing-state vocabulary at `LOCKS.md:335-342`: support-only modules,
unconsumed bodies, and orphan `asm!`/intrinsic files do not close Lock
16. The three skeleton contracts must either ship scalar oracle +
checkasm cell + same-wave consumer in a single wave, or be deleted from
`bbnf.asm` if no consumer is named within the SK-V14 horizon.

**V2 binary disposition (CH4-F1 enaction): DELETE all three.** The
three SKELETON contracts at HEAD have:

- **No SK-V14-horizon consumer** in any production crate:
  - `grep -rn "FSM_DISPATCH_THREADED\|fsm_dispatch_threaded" skinny/
    crates/codegen/src/ skinny/crates/runtime/src/ skinny/crates/
    passes/src/` returns zero hits.
  - `grep -rn "FRAME_PUSH_BOUNDED\|FRAME_POP_BOUNDED\|frame_push\|
    frame_pop\|open_buf\|frames_buf" skinny/crates/runtime/src/`
    returns zero hits beyond CSS keyframe fixture strings
    (`runtime/src/lib.rs:128,156` reference `@keyframes` CSS
    selector text, not the `open_buf` frame-stack semantics).
- **No scalar oracle** that would satisfy CH4 admission discipline:
  the FSM oracle would be a switch-statement equivalent; the frame
  oracle would be Vec-backed push/pop semantics — neither is
  authored at HEAD.
- **No checkasm cell**: `ls skinny/crates/bbnf-simd/tests/
  checkasm_fsm_*.rs skinny/crates/bbnf-simd/tests/checkasm_frame_
  *.rs` returns zero matches.

OQ-1 (FSM scalar oracle as switch-statement equivalent) and OQ-2
(`skinny/crates/runtime/src/` open-frames consumer audit) both
collapse to the deletion branch: OQ-2 verify-action returns no
consumer in `runtime/src/`, and OQ-1 cannot proceed without a named
downstream codegen-emitted CollapsedStage consumer (none exists).
Per Lock 16 v+1's four close states (`wired`, `deleted`,
`scalar-delegate-non-ASM`, `architectural-block-with-REDRESS`),
`deleted` is the V2 close state for all three contracts. The
deletion stanza removes `FSM_DISPATCH_THREADED` declaration at
`bbnf.asm:355-363`, `FRAME_PUSH_BOUNDED` at `bbnf.asm:404-418`, and
`FRAME_POP_BOUNDED` at `bbnf.asm:454-468`, ships in the same V2 wave
as the adoption-cost ledger publication, and reduces the Layer-1
contract count from 9 to 6.

If a same-wave consumer surfaces post-V2 (e.g. SK-V14 codegen lands
a CollapsedStage shape that legitimately needs r10-PC threaded
dispatch, or a runtime open-frames stack lands as part of a CSS L4
nested-layout consumer), the contract may be re-introduced under
the standard 5-stage admission process (Stage A scalar oracle →
Stage B checkasm cell → Stage C microbench → Stage D same-wave
consumer → Stage E wave-close disposition) in a new wave that
provides ALL admission artefacts in a single commit.

### R4 — x86-only Layer 1 contracts satisfy SK-V14 admission

`bbnf.asm` declares only x86 macro contracts; SK-V14 implementation
scope is aarch64 / Apple M5 Max. The x86 contracts inform the totality
primitive vocabulary but cannot close M5 Max admission rows. Per Lock
16 v+1 at `LOCKS.md:346-349`: "AVX-512 literature is x86 architecture
pressure and cannot close M5/aarch64 rows." The aarch64-counterpart
implementation lives at `crates/bbnf-simd/src/aarch64/*.rs`; admission
on aarch64 requires a NEON body for each Layer 1 primitive plus the
same checkasm/scalar discipline. The aarch64 Layer 1 vocabulary inherits
the same 9-primitive contract surface (6 post-V2 deletion of SKELETON
triple) but is implementation-distinct.

### R5 — `BackendShape` lowerers shipping as marker strings constitute candidate admission (V2 fold CH4-F2)

The five-shape `BackendShape` enum in `crates/codegen/src/lower/` is
the V1 backend-shape candidate set for the cost-model decision engine
that 2D's `T2D-FIVE-SHAPE-FINITE-SET` defends. Audit at HEAD: four of
five lowerers emit literal marker strings instead of generating real
backend code. The exact bodies (verified verbatim):

- `crates/codegen/src/lower/eager_tape.rs:15-17`:
  `format!("rule {} -> eager_tape", rule.name)`
- `crates/codegen/src/lower/offset_tape.rs:15-17`:
  `format!("rule {} -> offset_tape", rule.name)`
- `crates/codegen/src/lower/event_tape.rs:15-17`:
  `format!("rule {} -> event_tape", rule.name)`
- `crates/codegen/src/lower/collapsed_stage.rs:15-17`:
  `format!("rule {} -> collapsed_stage", rule.name)`

The only lowerer with actual generated-code emission is
`crates/codegen/src/lower/sink_only.rs:1-300+` (genuine
`SinkOnlyProgram` IR-to-code projection).

**Refutation.** The 4-of-5 marker-string lowerers are NOT "candidate
set members under evaluation" — they are unimplemented placeholders
that the cost extractor cannot extract meaningful costs from. Per
2B's own §A4 admission contract, a backend shape requires (a) scalar
oracle, (b) checkasm cell, (c) named same-wave consumer, (d) hardware
gate, (e) published citation. None of `EagerTape`, `OffsetTape`,
`EventTape`, `CollapsedStage` meets any of (a)-(c) at HEAD; the
codegen path emits a debug string, not a runtime artifact. Searching
over a five-shape candidate set whose four members emit
`format!("rule {} -> <shape>", ...)` is paper-architecture: the cost
model's extraction over `BackendShape::*` is searching a domain that
has no real candidates for 80% of its surface.

**V2 fold disposition (CH4-F2 candidate refutation, not silent
admission).** The marker-string state is documented here as a
refutation row rather than tolerated as a code-quality observation
because the same Lock 16 admission contract that bars SKELETON
contracts in `bbnf.asm` (§R3) bars marker-string lowerers in
`codegen/src/lower/`: in both cases, a contract surface is declared
but no admission artefacts back it. The disposition for the four
marker-string lowerers is delegated to 2D V2 (per the V1 → V2 fold
packet, item 12): each of `EagerTape`, `OffsetTape`, `EventTape`,
`CollapsedStage` must EITHER (a) ship concrete IR-to-code emission
+ scalar oracle + checkasm cell + same-wave runtime consumer in the
SK-V14 horizon, OR (b) be retired from the V1 candidate set via Lock
10 amendment, reducing `BackendShape` to `SinkOnly` as the only
admitted shape. 2D's `LAC-2D-04` already pins this admission
condition for `CollapsedStage`; V2 fold extends the same condition
to the other three marker-string shapes.

This refutation is the codegen-layer analogue of 2B's §R3 SKELETON
refutation at the macro-layer: both refute "contract presence without
implementation = admission" as a paper-close pattern. The V2 fold
packet (item 12) names 2D V2 as the disposition author; 2B V2
documents the refutation here so the CH4-F2 finding is logged in
this dossier's refutation table for cross-dossier consistency.

## Open Research Questions

| UNKNOWN | verify_action | wave/pass anchor |
|---|---|---|
| ~~Does FSM_DISPATCH_THREADED admit a meaningful scalar oracle?~~ | **DISCHARGED V2 via §R3 DELETE.** Verify-action collapsed to deletion branch: no SK-V14-horizon codegen-emitted CollapsedStage consumer exists at HEAD per `grep -rn "FSM_DISPATCH_THREADED" skinny/crates/codegen/src/` zero hits. Contract scheduled for deletion from `bbnf.asm:355-363` in V2 wave. | V2 wave (same-wave with V2 dispatch) |
| ~~Are FRAME_PUSH_BOUNDED / FRAME_POP_BOUNDED still required given current scalar Rust open_buf?~~ | **DISCHARGED V2 via §R3 DELETE.** `skinny/crates/runtime/src/` audit returns no open-frames stack consumer; OQ-2 verify-action collapses to deletion branch. Both contracts scheduled for deletion from `bbnf.asm:404-418,454-468` in V2 wave. | V2 wave (same-wave with V2 dispatch) |
| Does the Layer 0 vendoring need a periodic upstream-resync gate? | Add a CI job that fetches dav1d HEAD and diffs `src/x86/x86inc.asm` against `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`; failure surfaces a lock-amendment candidate to bump the pinned SHA. | Deferred to S-P3 W{TBD} (CI infrastructure wave) |
| Are there `x86util.asm` macros bbnf will never use that should be deleted from Layer 0? | Static analysis of bbnf-bodies against `x86util.asm` macro inventory would identify dead helpers (e.g. `IDCT4_1D`, `STORE_DCT`); however Lock 16 vendor-verbatim discipline likely forbids selective subsetting. Confirm with T-P3 §3C governance disposition. | Discharged at T-P3 §3C amendment authoring |
| Should the aarch64 Layer 1 vocabulary have its own `.S` macro file equivalent to `bbnf.asm`? | The aarch64 path currently uses Rust `core::arch::aarch64::*` intrinsics rather than a NEON macro file; evaluate whether an `aarch64/bbnf_neon.S` macro library would improve discipline parity with the x86 path. Per LAC-2B-05, the aarch64 vocabulary parity manifest is the immediate need; a macro file is a future-wave consideration. | Discharged at T-P3 §3C amendment authoring (LAC-2B-05 disposition) |
| Will REDRESS-88 (PMULL hot body) admission unlock `BITMAP_PREFIX_XOR_64` aarch64 promotion from `scalar-delegate-non-ASM` to `ASM-admitted`? | Track S-P2 V3 C-P2C-2 candidate admission timeline; the aarch64 `BITMAP_PREFIX_XOR_64` row in §A5 is gated on REDRESS-88 closure. | Deferred to S-P3 P3-A wave consuming C-P2C-2 admission |
| Will REDRESS-89 (CSSC CTZ bulk) admission unlock `BITMAP_NEXT_SET_BIT` aarch64 promotion? | Same as above, tracking S-P2 V3 C-P2C-2 admission for CSSC CTZ specifically. | Deferred to S-P3 P3-A wave consuming C-P2C-2 admission |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2B-01 | Lock 16 | Pin Layer 0 vendoring SHA explicitly in `LICENSE-VENDOR`: add a footer recording dav1d HEAD `1718ff9aded99f0a89f5c7940d6afb8948301e33` (per `T-P2-V2-FOLD-ADDENDUM.md:29`) so the verbatim-copy claim is traceable without grepping fold addenda. | `LICENSE-VENDOR:9-13` (x264/FFmpeg attribution); fold addendum SHA pin. |
| LAC-2B-02 | Lock 16 | Require Layer 1 contracts to ship scalar reference + checkasm cell in the same commit they are declared in `bbnf.asm`. A contract with no scalar oracle and no checkasm test is non-admissible regardless of how complete the macro contract appears. | `bbnf.asm:30-44` (9 contracts); `src/scalar/` and `tests/checkasm_*` have only 6/9 backings; LOCKS.md:307. |
| LAC-2B-03 | Lock 14 / Lock 16 | Require `policy_owner` field on every Layer 1 consumer call site: `generated_grammar` (codegen emits the LUT/constants), `caller_data` (consumer supplies at runtime), or `none` (truly grammar-neutral, e.g. `BITMAP_PREFIX_XOR_64`). Reject shared call sites with hardcoded JSON constants. | `dispatch.rs:22-33` (JSON-hardcoded Tbl4 dispatch); SYNTHESIS.md:239-263 (`G-SIMD-GRAMMAR-POLICY`). |
| LAC-2B-04 | Lock 16 | Forbid Layer 0 modifications: any local diff vs upstream `x86inc.asm` / `x86util.asm` invalidates the verbatim-vendor provenance and requires an explicit lock amendment recording (a) the diff, (b) the upstream pin SHA, (c) the bbnf-specific reason. | LICENSE-VENDOR:48-53 (build determinism rationale); upstream pin in V2 addendum. |
| LAC-2B-05 | Lock 16 | Require an aarch64 Layer 1 vocabulary parity manifest: each of the 6 (post-V2 deletion) Layer-1 contracts must list its aarch64 body (`src/aarch64/<name>.rs`) and admission state from {`ASM-admitted`, `scalar-delegate-non-ASM`, `pending-aarch64-port`, `pending-aarch64-port-REDRESS-gated`} per §A5 atomic classification, since SK-V14 implementation scope is aarch64 / M5 Max while `bbnf.asm` declares only x86 contracts. | bbnf.asm is x86-only; LOCKS.md:346-349 forbids x86 literature closing aarch64 rows; §A5 atomic close-state classification provides per-primitive evidence. |
| LAC-2B-06 (NEW V2) | Lock 10 (cost model candidate set) / Lock 16 | Forbid marker-string `BackendShape` lowerers in the V1 candidate set: every `BackendShape::*` variant whose lowerer in `crates/codegen/src/lower/<shape>.rs` returns `format!("rule {} -> <shape>", ...)` instead of a concrete IR-to-code emission MUST EITHER (a) ship concrete codegen + scalar oracle + checkasm cell + same-wave runtime consumer in the SK-V14 horizon, OR (b) be retired from the V1 candidate set via Lock 10 amendment. The same admission-presence rule that bars SKELETON contracts in `bbnf.asm` (§R3) bars marker-string lowerers in `codegen/src/lower/`. | `crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` (4-of-5 marker-string lowerers); `crates/codegen/src/lower/sink_only.rs:1-300+` (the only genuine lowerer); 2D's `LAC-2D-04` (CollapsedStage admission); §R5 V2 refutation. |
| LAC-2B-07 (NEW V2) | Lock 16 (close-state vocabulary) | Encode the atomic per-primitive close-state vocabulary surfaced by §A5: {`ASM-admitted` (NEON intrinsic body present and not a scalar delegate), `scalar-delegate-non-ASM-by-design` (terminal close state; scalar is optimal or no ISA equivalent exists), `scalar-delegate-non-ASM-pending-port` (delegate is interim; port is targeted), `pending-aarch64-port-REDRESS-gated` (port gated on REDRESS-88/89 closure), `pending-port-standard` (no REDRESS gate; standard primitive-lift route), `deleted` (V2 disposition: contract removed)}. The current Lock 16 v+1 close-state vocabulary at `LOCKS.md:335-342` collapses these into 4 states; the atomic V2 vocabulary preserves the disposition information (`by-design` vs `pending-port` is load-bearing for SK-V14 wave planning). | §A5 atomic per-primitive close-state classification; Lock 16 v+1 4-state vocabulary at `LOCKS.md:335-342`; aarch64 close-state census (1 ASM-admitted / 2 terminal scalar-delegate / 2 REDRESS-gated pending-port / 1 standard pending-port / 3 deleted). |

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
