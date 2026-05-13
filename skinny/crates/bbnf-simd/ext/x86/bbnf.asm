; ============================================================================
; bbnf.asm — bbnf-lang primitive macro vocabulary (Layer 1)
;
; Companion to dav1d's vendored x86inc.asm (Layer 0). This file declares the
; grammar-neutral primitive macros that bbnf-simd kernels use as their
; reusable vocabulary. Together with x86inc.asm it forms the complete macro
; layer (analog of dav1d's x86inc.asm + x86util.asm pair).
;
; Macros declared here are CONTRACTS. Bodies live in per-primitive .asm
; files under src/x86_64/*.asm. The Rust FFI shims live in src/x86_64/*.rs.
; Scalar reference implementations live in src/scalar/*.rs and serve as the
; executable specification per the checkasm admission gate.
;
; Source attribution:
;   - x86inc.asm vendoring from dav1d (BSD-2). See ext/x86/LICENSE-VENDOR.
;   - asmjson primitive shapes (vpcmpeqb fan-out + kor reduce, jmp r10
;     dispatch, frames_buf/open_buf bounded stack): see upstream src/lib.rs
;     and the V9.5 cohort digest at restart/skinny/audit/WAVE-1-2-COHORT-DIGEST.md.
;   - simdjson VPCLMULQDQ prefix-XOR pattern; scaled here to 512-bit.
;   - dav1d primitive-lift discipline (per-ISA primitives + per-grammar LUT
;     data + shared dispatch spine): see filmgrain_common.asm pattern.
;
; Locks governing this file:
;   - Lock 1  (substrate union — tape ≡ structural projection)
;   - Lock 14 (zero overfitting — primitives are grammar-neutral; per-grammar
;              variation lives in codegen-emitted .data tables)
;   - Lock 15 (i-cache budget — per-primitive bodies under 20 KiB hot)
;   - Lock 16 (SIMD admissibility allowlist)
;
; Macro inventory (9 grammar-neutral primitives):
;   1. BYTE_CLASS_FROM_TABLE_64    — 64B → k1 via 256-byte LUT (vpermb/GFNI)
;   2. BYTE_CLASS_FROM_EQ_SET_64   — 64B → k1 via ≤8 char fan-out (vpcmpeqb+kor)
;   3. BITMAP_PREFIX_XOR_64        — 64b bitmap → ripple-XOR (VPCLMULQDQ-512)
;   4. BITMAP_NEXT_SET_BIT         — 64b bitmap + cursor → next-set offset (tzcnt)
;   5. BULK_EMIT_COMPRESSED        — 64B + k1 → compressed sink (vpcompressb)
;   6. EOB_PAD_CLAMP               — tail bytes → zero-padded 64B vector
;   7. FSM_DISPATCH_THREADED       — state-as-PC threaded dispatch (jmp [tbl+r10*8])
;   8. FRAME_PUSH_BOUNDED          — push FrameKind onto open_buf with bounds check
;   9. FRAME_POP_BOUNDED           — pop FrameKind from open_buf + close-bracket validate
;
; Primitive 7 (FSM_DISPATCH_THREADED) is the SOLE FSM macro; it is consumed
; only by per-grammar CollapsedStage kernels emitted by codegen. The other
; eight are recursive-descent / scan-emit leaf primitives shared across all
; grammars (JSON, CSS, BBNF-self, Sheets, arbitrary user grammars).
; ============================================================================

%include "x86inc.asm"
%include "x86util.asm"

; section .text default — macros expand at use sites in per-primitive .asm
; files. This file is include-only; it emits no code of its own.

SECTION_RODATA 64

; (Per-grammar .data sections — class LUTs, FSM transition tables, frame
; close-bracket maps — live in the corresponding per-grammar kernel .asm
; files emitted by codegen, NOT in this macro library. The macro vocabulary
; is grammar-neutral; only the .data is grammar-specialized. This is the
; dav1d primitive-lift discipline in full force: shared primitives +
; per-instance LUT data + shared dispatch spine.)

SECTION .text

; ----------------------------------------------------------------------------
; BYTE_CLASS_FROM_TABLE_64 — 64B → k-mask via table lookup
;
; Inputs:
;   zmm0..zmm3       64 contiguous source bytes (caller loads via 4×vmovdqu64
;                    from [src + 0/16/32/48], or one vmovdqu64 zmm0,[src] on
;                    AVX-512F-capable hosts where the full ZMM load is legal)
;   %1 (general reg) 256-byte class-membership LUT pointer (RIP-relative ok).
;                    Encoded as a vpermb table: LUT[i] = 0xFF if byte i is in
;                    the class, 0x00 otherwise. Codegen emits one LUT per
;                    distinct class predicate.
;
; Outputs:
;   k1               64-bit class-membership mask, bit i = (LUT[src[i]] != 0)
;
; Clobbers:
;   zmm4, zmm5       scratch (LUT broadcast + permuted result)
;   k2               scratch (intermediate vptestmb destination, freed at exit)
;
; ISA admissibility:
;   AVX-512 BW required (vpermb).
;   AVX-512 VBMI required for the vpermb fast path (Ice Lake+ on Intel, Zen 4+
;   on AMD). On AVX-512 BW only, fall back to vpshufb + carry-merge (slower).
;
;   GFNI variant (vgf2p8affineqb) admits when the predicate is expressible as
;   a 64-bit affine transform over GF(2⁸); this replaces the 256-byte table
;   with an 8-byte affine constant and runs ~5× fewer µops. Codegen flags the
;   admissibility at LayoutFacts derivation time (ARCH §7.3).
;
; Citation:
;   asmjson per-state byte mask (src/lib.rs FSM transitions).
;   dav1d film-grain classifier (filmgrain_common.asm).
;   Lock 16 row: AVX-512 BW + (VBMI | BW-only fallback) + GFNI optional.
; ----------------------------------------------------------------------------
%macro BYTE_CLASS_FROM_TABLE_64 1 ; %1 = LUT pointer (general reg)
    ; Body lives in src/x86_64/byte_class_from_table_64.asm.
    ; This macro is intentionally a contract declaration in this skeleton.
%endmacro

; ----------------------------------------------------------------------------
; BYTE_CLASS_FROM_EQ_SET_64 — 64B → k-mask via equality fan-out
;
; Inputs:
;   zmm0..zmm3       64 source bytes (4×vmovdqu64, or one zmm load)
;   %1..%N (imm8)    N ≤ 8 byte constants to match against (compile-time
;                    parameters supplied at macro-expansion site)
;
; Outputs:
;   k1               64-bit equality-set membership mask (OR over per-constant
;                    vpcmpeqb results)
;
; Clobbers:
;   zmm4             scratch (broadcasted comparand)
;   k2..k_(N+1)      scratch (per-constant vpcmpeqb destinations; freed by
;                    the korq reduce into k1)
;
; ISA admissibility:
;   AVX-512 BW. Each char is broadcast via vpbroadcastb to zmm4, then compared
;   against the 64-byte source register with vpcmpeqb writing to kN; the
;   accumulator k1 is folded forward with korq kN, k1, kN. The reduce tree
;   admits straight-line scheduling (no loop overhead for N ≤ 8).
;
; Notes:
;   This macro implements asmjson's ACTUAL hot shape. Per upstream src/lib.rs
;   analysis the ONLY AVX-512 primitives asmjson uses are:
;       - 10× vpcmpeqb   (the per-state classifier fan-out)
;       -  6× korq       (the OR reduce across class members)
;       - 18× tzcnt      (the next-set-bit dispatch driver, see primitive 4)
;   No esoterica. The 64-byte width is the strict-additive lift over asmjson's
;   32-byte AVX2 path (1.7× wider data per amortized FSM-step).
;
; Citation:
;   asmjson src/lib.rs (classify_chunk inner loop).
;   Lock 16 admissibility: AVX-512 BW.
; ----------------------------------------------------------------------------
%macro BYTE_CLASS_FROM_EQ_SET_64 1-8 ; %1..%N = imm8 char constants (N ≤ 8)
    ; Body lives in src/x86_64/byte_class_from_eq_set_64.asm.
    ; Variadic macro: codegen emits exactly N vpcmpeqb + (N-1) korq pairs.
%endmacro

; ----------------------------------------------------------------------------
; BITMAP_PREFIX_XOR_64 — 64-bit bitmap → string-region carry mask
;
; Inputs:
;   rdi              64-bit structural-quote bitmap (1 = unescaped quote pos)
;
; Outputs:
;   rax              64-bit string-region bitmap (1 = position is inside a
;                    string literal). Each 1-bit in rdi toggles the carry;
;                    rax[i] is the parity of rdi[0..i].
;
; Clobbers:
;   xmm0, xmm1       scratch (operand staging for VPCLMULQDQ)
;
; ISA admissibility:
;   VPCLMULQDQ required. Multiplying the bitmap by the all-ones constant
;   under carryless multiplication yields the prefix-XOR in a single µop.
;   On EVEX-512 hosts (Ice Lake-X+, Zen 4+) the 512-bit form (vpclmulqdq with
;   ZMM operands) processes the whole 64-bit lane in one instruction; on
;   VEX-only hosts (Zen 3, Tiger Lake client) the 128-bit form suffices since
;   the input is already 64 bits wide.
;
; Notes:
;   simdjson uses this exact primitive at 128-bit width for its quote-mask
;   construction. We scale to 512-bit only to keep the dispatch loop's
;   data-path uniformly ZMM-wide; the actual carry computation is 64→64.
;   Lock 14 forbids overfitting this primitive to JSON: it is a pure
;   bit-parallel carry, valid for any toggle-based region (CSS string
;   literals, BBNF rule-string content, etc.).
;
; Citation:
;   simdjson "Parsing Gigabytes of JSON per Second" §3.1 (quote mask).
;   Lock 16 admissibility: VPCLMULQDQ (PCLMULQDQ-VEX256 acceptable fallback).
; ----------------------------------------------------------------------------
%macro BITMAP_PREFIX_XOR_64 0
    ; Body lives in src/x86_64/bitmap_prefix_xor_64.asm.
    ; Single-instruction hot path on EVEX-512 hosts; 2-µop on VEX-only.
%endmacro

; ----------------------------------------------------------------------------
; BITMAP_NEXT_SET_BIT — locate the next set bit at or after a cursor
;
; Inputs:
;   rdi              64-bit bitmap (typically a structural-position mask
;                    derived from BYTE_CLASS_FROM_* primitives)
;   rsi              cursor offset within the chunk, 0 ≤ rsi ≤ 63
;
; Outputs:
;   rax              offset of the next set bit ≥ rsi, in range [rsi, 64].
;                    Returns 64 when no further set bit exists in the chunk
;                    (caller's overflow signal; the chunk is exhausted).
;   ZF               set when rax == 64 (no more bits); cleared otherwise.
;
; Clobbers:
;   rcx              scratch (shifted bitmap holding bits ≥ rsi)
;
; ISA admissibility:
;   BMI1 required (tzcnt). tzcnt on a zero operand returns 64 (its operand
;   width) per Intel/AMD ISA reference, which is exactly the overflow sentinel
;   we want — no branch needed in the hot loop.
;
; Notes:
;   This is THE dispatch-driver primitive. Per asmjson src/lib.rs the inner
;   classifier loop is dominated by ~18× tzcnt calls per chunk — once for each
;   structural transition. The macro is consumed by:
;       - the recursive-descent leaf scanners (find-next-delim shape)
;       - the FSM dispatch spine (advance-cursor-to-next-event shape)
;
;   Lock 1 substrate union: the bitmap produced by BYTE_CLASS_FROM_* IS the
;   structural projection. BITMAP_NEXT_SET_BIT closes the loop from class
;   detection to dispatch with zero materialization.
;
; Citation:
;   asmjson src/lib.rs (classify_chunk dispatch loop).
;   Lock 16 admissibility: BMI1 (universal on AVX-512 hosts).
; ----------------------------------------------------------------------------
%macro BITMAP_NEXT_SET_BIT 0
    ; Body lives in src/x86_64/bitmap_next_set_bit.asm.
    ; Two-instruction hot path: shrx rcx, rdi, rsi ; tzcnt rax, rcx.
%endmacro

; ----------------------------------------------------------------------------
; BULK_EMIT_COMPRESSED — gather selected bytes via vpcompressb into sink
;
; Inputs:
;   zmm0..zmm3       64 source bytes (4×vmovdqu64, or one zmm load)
;   k1               64-bit selection mask (bit i = emit src[i] into sink)
;   rdi              output sink pointer (caller-owned, must have ≥64B room)
;
; Outputs:
;   [rdi]            popcnt(k1) compressed bytes written contiguously
;   rdi              advanced by popcnt(k1) (sink cursor maintained by macro)
;
; Clobbers:
;   zmm4             scratch (vpcompressb destination)
;   rax              scratch (popcnt(k1) for cursor advance)
;
; ISA admissibility:
;   AVX-512 VBMI2 required (vpcompressb). VBMI2 is Ice Lake+ on Intel and
;   Zen 4+ on AMD. Without VBMI2, callers must take the scalar tape-builder
;   path; Lock 16 admissibility gates this primitive on the VBMI2 feature
;   flag at LayoutFacts derivation (ARCH §7.3 backend_shape).
;
; Notes:
;   Used at scan/emit boundaries: structural tape construction, string-body
;   extraction, identifier accumulation. The macro maintains the rdi cursor
;   invariant so callers can chain emits without interleaving popcnt
;   bookkeeping in their hot loop.
;
;   Caller responsibility: the sink buffer must be over-allocated by ≥64B
;   beyond its semantic capacity (EOB_PAD_CLAMP companion). This eliminates
;   the scalar tail-loop entirely — every chunk emits a full 64-byte store
;   regardless of trailing bytes.
;
; Citation:
;   simdjson tape-builder stage (string-extraction pattern, AVX-512 VBMI2 fork).
;   asmjson omits this primitive (no compressed emit; pure dispatch-driven).
;   Lock 16 admissibility: AVX-512 VBMI2.
; ----------------------------------------------------------------------------
%macro BULK_EMIT_COMPRESSED 0
    ; Body lives in src/x86_64/bulk_emit_compressed.asm.
    ; Hot path: vpcompressb zmm4{k1}{z}, zmm0 ; vmovdqu64 [rdi], zmm4
    ;           ; popcnt rax, k1 ; add rdi, rax.
%endmacro

; ----------------------------------------------------------------------------
; EOB_PAD_CLAMP — load a partial tail into a zero-padded 64-byte vector
;
; Inputs:
;   rdi              byte pointer to the partial tail (must be readable for
;                    at least rsi bytes; over-allocation handles the rest)
;   rsi              remaining-byte count, 0 ≤ rsi ≤ 64 (caller invariant)
;
; Outputs:
;   zmm0..zmm3       64 bytes: rsi available bytes followed by (64-rsi) zero
;                    bytes. The zero-padding is semantically inert under every
;                    class predicate we use (no class accepts 0x00 except by
;                    grammar opt-in, which codegen flags).
;   k7               64-bit "live-byte" mask (bit i set ⇔ i < rsi). Callers
;                    that DO accept 0x00 as a class member must AND their
;                    class mask with k7 to discard padding.
;
; Clobbers:
;   rax              scratch (mask construction)
;   k6               scratch (intermediate mask before k7)
;
; ISA admissibility:
;   AVX-512 BW (mask-zero-merge load). The hot path uses a single
;   vmovdqu8 zmm0{k7}{z}, [rdi] with k7 derived from rsi via bzhi-style
;   construction (mov rax, -1 ; bzhi rax, rax, rsi ; kmovq k7, rax).
;
; Notes:
;   This primitive eliminates the scalar tail loop that traditionally
;   bookends SIMD parsers. It is the msac-style pattern: the buffer is
;   over-allocated by 64 bytes upstream, so reading past rsi is safe;
;   the mask discards the over-read.
;
;   Caller responsibility: the source buffer must be over-allocated by 64
;   bytes. bbnf-simd's input-staging layer (src/staging.rs) enforces this
;   invariant at every entry point — Lock 1 substrate union demands a
;   single allocation discipline across the parser.
;
; Citation:
;   dav1d msac (Multi-Symbol Arithmetic Coding) tail-handling pattern.
;   simdjson padded-buffer convention (32-byte pad → 64-byte pad on AVX-512).
;   Lock 16 admissibility: AVX-512 BW + BMI2 (bzhi).
; ----------------------------------------------------------------------------
%macro EOB_PAD_CLAMP 0
    ; Body lives in src/x86_64/eob_pad_clamp.asm.
    ; Hot path: mov rax,-1 ; bzhi rax,rax,rsi ; kmovq k7,rax
    ;           ; vmovdqu8 zmm0{k7}{z},[rdi]   (plus zmm1..zmm3 if rsi>16/32/48).
%endmacro

; ----------------------------------------------------------------------------
; FSM_DISPATCH_THREADED — state-as-PC threaded dispatch
;
; Inputs:
;   r10              state value (treated as an unsigned index into the
;                    per-grammar state-target table). The caller must
;                    guarantee r10 < table_length; codegen emits a debug-mode
;                    bounds check, production-mode raw jump.
;   r11              base address of the state-target table (8-byte slots,
;                    each holding a code address). The table is emitted into
;                    a per-grammar .data section by codegen — one table per
;                    CollapsedStage kernel, never shared across grammars.
;
; Outputs:
;   (control flow)   indirect jump to [r11 + r10*8]; no return to the macro
;                    expansion site. Each target is responsible for advancing
;                    state and re-entering dispatch via another expansion of
;                    this macro (the threaded-code pattern).
;
; Clobbers:
;   none (the jump target inherits the full register file as-is; this is the
;   r10 PC-as-state convention asmjson uses)
;
; ISA admissibility:
;   Baseline x86_64 (indirect jump). No SIMD or AVX requirement.
;
; Notes:
;   This is the ONLY macro in this file that participates in FSM dispatch.
;   The other eight are recursive-descent leaf primitives — they execute
;   under a single FSM state and return to the dispatch driver via fall-
;   through. The state-target table is per-grammar (Lock 14: zero
;   overfitting in primitives; per-grammar specialization lives in data).
;
;   Macro consumers: only the CollapsedStage kernels emitted by codegen for
;   each grammar (JSON/CSS/BBNF-self/Sheets/...). Hand-written kernels in
;   src/x86_64/*.asm that implement individual leaves DO NOT use this macro;
;   they are dispatched-into, not dispatching.
;
;   Threading discipline: each state body ends with a jump-to-next via
;   another FSM_DISPATCH_THREADED expansion. There is no central loop; the
;   "loop" is the threaded chain itself. This is the asmjson r10-PC pattern,
;   the SOTA-beat dispatch primitive.
;
; Citation:
;   asmjson src/lib.rs (jmp r10 dispatch core).
;   "Threaded Code" (Bell, 1973) — the originating ISA-level pattern.
;   Lock 16 admissibility: baseline x86_64.
; ----------------------------------------------------------------------------
%macro FSM_DISPATCH_THREADED 0
    ; Body lives in src/x86_64/fsm_dispatch_threaded.asm.
    ; Single-instruction hot path: jmp [r11 + r10*8].
%endmacro

; ----------------------------------------------------------------------------
; FRAME_PUSH_BOUNDED — push a FrameKind onto the open-frames stack
;
; Inputs:
;   al               frame value (FrameKind enum: 0 = Object, 1 = Array,
;                    additional kinds reserved for grammar-specific frames
;                    such as CSS at-rule blocks, BBNF rule-bodies, etc.)
;   rdi              open_buf base pointer (caller-owned 64-byte buffer,
;                    asmjson frames_buf/open_buf convention)
;   rcx              current depth, 0 ≤ rcx ≤ MAX_DEPTH (caller invariant
;                    on entry; macro establishes rcx ≤ MAX_DEPTH on exit)
;   r9               MAX_DEPTH constant (typically 64, matching open_buf
;                    capacity; grammar-tunable via codegen)
;
; Outputs:
;   [rdi + rcx]      stores al (the new frame on top of stack)
;   rcx              incremented by 1 (new depth)
;   (control flow)   on overflow (post-increment rcx > r9), control branches
;                    to the label supplied as %1. The error label is the
;                    grammar-specific deep-nesting fault handler (codegen
;                    emits one per kernel; typically returns ParseError).
;
; Clobbers:
;   none (rcx is updated in place; al is read-only; rdi is preserved)
;
; ISA admissibility:
;   Baseline x86_64. Three-instruction hot path: mov [rdi+rcx], al ; inc rcx
;   ; cmp rcx, r9 ; ja %1.
;
; Notes:
;   asmjson's bounded frame stack lives in two parallel 64-byte buffers:
;       - open_buf[64]   : FrameKind (Object|Array) at each depth
;       - frames_buf[64] : auxiliary per-frame metadata (asmjson uses this
;                          for the comma/colon expectation state machine)
;   This macro maintains the open_buf half; frames_buf maintenance lives in
;   per-grammar code because the metadata schema is grammar-specific.
;
;   MAX_DEPTH = 64 by default. Grammars with deeper structural nesting
;   (rare: a 65-level-nested JSON document) fall back to the scalar path;
;   Lock 16 admissibility gates the SIMD path on rcx ≤ MAX_DEPTH at every
;   PUSH. The 64-byte bound is the natural ZMM register width — extending
;   it requires a second ZMM and a second macro variant.
;
; Citation:
;   asmjson src/lib.rs (frames_buf / open_buf bounded stack).
;   simdjson uses a heap-allocated stack with no inline bound (different
;   trade-off; bbnf-simd follows asmjson here for stack-only allocation).
;   Lock 16 admissibility: baseline x86_64.
; ----------------------------------------------------------------------------
%macro FRAME_PUSH_BOUNDED 1 ; %1 = overflow-error label
    ; Body lives in src/x86_64/frame_push_bounded.asm.
    ; Hot path: mov [rdi+rcx], al ; inc rcx ; cmp rcx, r9 ; ja %1.
%endmacro

; ----------------------------------------------------------------------------
; FRAME_POP_BOUNDED — pop a FrameKind from the open-frames stack
;
; Inputs:
;   rdi              open_buf base pointer
;   rcx              current depth, MUST be ≥ 1 on entry (caller invariant;
;                    underflow is a parse error caught before this macro)
;   %1 (imm8)        expected FrameKind for the close-bracket the caller
;                    just observed (0 for '}', 1 for ']', grammar-specific
;                    for other close-markers)
;   %2 (label)       mismatch-error label — close-bracket type does not
;                    match the top-of-stack FrameKind. Codegen emits one
;                    per kernel; typically returns BracketMismatch.
;
; Outputs:
;   al               popped frame value ([rdi + rcx - 1] before decrement)
;   rcx              decremented by 1 (new depth, may be 0 = empty stack)
;   (control flow)   on mismatch (al != %1), control branches to %2.
;
; Clobbers:
;   none (rcx is updated in place; rdi is preserved; al is the return value)
;
; ISA admissibility:
;   Baseline x86_64. Four-instruction hot path: dec rcx ; mov al,[rdi+rcx]
;   ; cmp al, %1 ; jne %2.
;
; Notes:
;   Companion to FRAME_PUSH_BOUNDED. The two macros maintain the open_buf
;   invariant: at every quiescent point, [rdi + 0 .. rdi + rcx - 1] holds
;   the live frame stack and rcx is its depth.
;
;   The close-bracket validation IS the macro's correctness guarantee.
;   asmjson's correctness for matched brackets falls out of this single
;   cmp+jne pair — there is no separate bracket-matching pass. The
;   structural-tape invariant is maintained inline.
;
;   Caller responsibility: ensure rcx ≥ 1 BEFORE expanding this macro
;   (i.e. the close-bracket dispatcher must have observed an open-bracket
;   first). The macro does NOT defensively check; underflow is a precondition
;   violation, not a recoverable parse error. Codegen emits an empty-stack
;   guard at the dispatcher entry instead.
;
; Citation:
;   asmjson src/lib.rs (open_buf pop + bracket validate).
;   Lock 16 admissibility: baseline x86_64.
; ----------------------------------------------------------------------------
%macro FRAME_POP_BOUNDED 2 ; %1 = expected FrameKind imm8 ; %2 = mismatch label
    ; Body lives in src/x86_64/frame_pop_bounded.asm.
    ; Hot path: dec rcx ; mov al,[rdi+rcx] ; cmp al, %1 ; jne %2.
%endmacro

; ============================================================================
; End of bbnf.asm. Per-primitive bodies live in src/x86_64/*.asm. See also
; the scalar references in src/scalar/*.rs and the checkasm differential
; harness in tests/checkasm_parity.rs + tests/checkasm_*.rs.
;
; Admission gate: every body lands in src/x86_64/<primitive>.asm together
; with its scalar twin in src/scalar/<primitive>.rs and a checkasm row in
; tests/checkasm_<primitive>.rs. The differential harness compares scalar
; and SIMD outputs over the bbnf-bench corpus; mismatch is a CI hard fail
; per Lock 16 SIMD admissibility.
; ============================================================================
