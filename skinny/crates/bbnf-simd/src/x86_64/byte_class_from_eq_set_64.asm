; ============================================================================
; BYTE_CLASS_FROM_EQ_SET_64 — AVX-512 BW body.
;
; Contract (declared in ext/x86/bbnf.asm, body below):
;   Input :
;     rdi = src      (pointer to 64 contiguous source bytes)
;     rsi = set_ptr  (pointer to up to 8 byte values)
;     rdx = set_len  (1..=8)
;   Output:
;     rax = 64-bit mask where bit i is set iff src[i] ∈ set
;
; Citations:
;   * asmjson (Lemire et al.) `classify_chunk` inner loop: fan
;     `vpcmpeqb` against each broadcast set member, `korq`-reduce into a
;     single `k` register, materialise via `kmovq`.  At 64-byte width on
;     AVX-512 BW, this is the strict additive lift over asmjson's 32-byte
;     AVX2 `vpcmpeqb`/`vpmovmskb`/`or` chain — one zmm load, eight
;     broadcast/compare pairs, seven kor reductions, one kmovq spill.
;   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): admitted only
;     if checkasm parity holds against the scalar reference in
;     src/scalar/byte_class_from_eq_set_64.rs across every (src, set)
;     pair the harness sweeps.
;
; Register file:
;   zmm0          — source chunk (loaded once)
;   zmm1..zmm8    — broadcast set members (lazy-loaded by set_len)
;   k1..k8        — per-member equality masks; k1 is the accumulator
; ============================================================================

%include "x86inc.asm"

SECTION_RODATA

SECTION .text

; ----------------------------------------------------------------------------
; byte_class_from_eq_set_64_avx512(src, set_ptr, set_len) -> u64
;
;   Argument registers (SysV / x86inc cglobal):
;     src       = r0 (rdi)
;     set_ptr   = r1 (rsi)
;     set_len   = r2 (rdx)
;
;   x86inc cglobal arguments:
;     name=byte_class_from_eq_set_64_avx512
;     nargs=3   gpregs=6   xmmregs=8   args=src, set_ptr, set_len
; ----------------------------------------------------------------------------

INIT_ZMM avx512
cglobal byte_class_from_eq_set_64_avx512, 3, 6, 9, src, set_ptr, set_len
    ; --- 1. Load the 64-byte source chunk into zmm0 (unaligned-safe) -------
    vmovdqu64       zmm0, [srcq]

    ; --- 2. Broadcast set[0] and produce the first equality mask in k1 ----
    ;       set_len >= 1 by contract, so this dispatch is unconditional.
    vpbroadcastb    zmm1, byte [set_ptrq + 0]
    vpcmpeqb        k1,   zmm0, zmm1

    cmp             set_lenq, 1
    je              .done

    vpbroadcastb    zmm2, byte [set_ptrq + 1]
    vpcmpeqb        k2,   zmm0, zmm2
    korq            k1,   k1, k2
    cmp             set_lenq, 2
    je              .done

    vpbroadcastb    zmm3, byte [set_ptrq + 2]
    vpcmpeqb        k3,   zmm0, zmm3
    korq            k1,   k1, k3
    cmp             set_lenq, 3
    je              .done

    vpbroadcastb    zmm4, byte [set_ptrq + 3]
    vpcmpeqb        k4,   zmm0, zmm4
    korq            k1,   k1, k4
    cmp             set_lenq, 4
    je              .done

    vpbroadcastb    zmm5, byte [set_ptrq + 4]
    vpcmpeqb        k5,   zmm0, zmm5
    korq            k1,   k1, k5
    cmp             set_lenq, 5
    je              .done

    vpbroadcastb    zmm6, byte [set_ptrq + 5]
    vpcmpeqb        k6,   zmm0, zmm6
    korq            k1,   k1, k6
    cmp             set_lenq, 6
    je              .done

    vpbroadcastb    zmm7, byte [set_ptrq + 6]
    vpcmpeqb        k7,   zmm0, zmm7
    korq            k1,   k1, k7
    cmp             set_lenq, 7
    je              .done

    vpbroadcastb    zmm8, byte [set_ptrq + 7]
    vpcmpeqb        k2,   zmm0, zmm8     ; k2 reused — k1 still the accumulator
    korq            k1,   k1, k2

.done:
    ; --- 3. Materialise the 64-bit result -----------------------------------
    kmovq           rax, k1
    RET
