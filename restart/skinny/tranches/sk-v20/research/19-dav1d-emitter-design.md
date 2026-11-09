# 19 — The dav1d-Lineage Hybrid Emitter: Architecture, Kernel Inventory, and the Honest asm-vs-Intrinsics Verdict

## Thesis

The dav1d discipline is: a portable **reference** path; per-ISA **leaf kernels** as pure
flat-array functions; **runtime dispatch** by CPU-feature function pointer; and a
**checkasm** differential+microbench harness that admits every kernel against the
reference under adversarial inputs, signal traps, and clobber guards. The skinny tree has
already imported the *vocabulary* of this discipline — `bbnf-simd` with `scalar/`
references, `aarch64/` NEON bodies, a `dispatch.rs` table, and a `checkasm_parity.rs`
harness modelled explicitly on `checkasm.h`. What it has **not** imported is the binding
chain of consequence: the kernels are not selected by a cost model, are not composed by
the emitter, and several are dead on the live path. The proposal below is the
transposition that closes that gap, with an honest verdict on where hand asm earns its
maintenance cost on Apple-M and where it is a trap.

---

## Part 1 — What exists, and where the discipline breaks

### 1.1 The kernel layer is real but orphaned from the emitter

`bbnf-simd` carries fourteen aarch64 kernels (`structural_classify_64`,
`byte_class_from_eq_set_64`, `byte_class_from_table_64`, `comment_body_mask_64`,
`bracket_depth_mask_64`, `unescape_uxxxx`, `match_tiny_plain_string`, `classify_tbl4`,
`bitmap_prefix_xor_64`, `bulk_emit_positions_64`, `digit_mac`, `cache_hints`, the utf8
validator, plus scalar mirrors). Twelve are NEON intrinsics; only two
(`aarch64/cache_hints.rs`, `aarch64/digit_mac.rs`) use inline `asm!`. Each has a scalar
reference and a checkasm parity test. This part is faithful to dav1d.

The break is upstream. **No cost model selects these kernels.** The only "cost model" in
the skinny tree is the egraph extractor in `passes/src/backend_egraph.rs` (`Decisionln`),
which chooses BIR decision nodes, not SIMD kernels. Kernel choice is instead hand-wired
into per-grammar runtime modules: `runtime/src/grammars/json/scan.rs:215-240` hard-codes
`classify_tbl4` + `escape_mask_64`; `runtime/src/runtime_simd.rs:17` hard-codes
`bracket_depth_mask_64` + `byte_class_from_eq_set_64` for CSS. The emitter
(`codegen/src/lower/`) does not compose kernels — `codegen/src/css_scan_direct/projection.rs:1`
reaches a kernel exactly once, through a runtime shim. **The emitter is not the
specializer.** It emits descent/tape code; the SIMD substrate is bolted on by hand-written
grammar modules living *in `runtime/`* and therefore, by construction, grammar-specific
code in a generic crate — a Lock-14 smell the HEAD manifest-driven CSS relocation only
partially drained.

### 1.2 Dead kernels admitted behind a green harness

The most serious honesty defect: **admitted, parity-green kernels with zero live
consumers.**

- `runtime/src/grammars/json/scan.rs:1-9` documents itself: *"this module is NEUTRALIZED
  on the live JSON path … the heavy entries (`scan_structurals` + its aarch64
  `neon::scan` …) are reachable ONLY under the explicit"* capacity override. Under the
  default `CapacityPlan::GrowOnly` the entire SIMD structural scanner is dead. The
  checkasm sweep in `checkasm_parity.rs` still runs against `scan_dispatch` and reports
  green — so the harness certifies code the live parser never executes.
- `unescape_uxxxx_neon` (`aarch64/unescape_uxxxx.rs:74`) is a complete, correct intrinsics
  `\uXXXX` decoder with a 200-line parity test (`checkasm_parity.rs:591-610`). A grep for
  live callers in `runtime/src` returns **nothing**. The kernel exists, is tested, is
  benchmarked, and is never called.
- `digit_mac.rs` (`parse_4_digits`, the `udot` inline-asm number kernel) and the entire
  `cache_hints.rs` (`prfm`/`stnp`) are called from **nowhere** outside their own module
  and tests. These are the *only two inline-`asm!` kernels in the tree*, and both are dead.

This inverts the dav1d guarantee. In dav1d, checkasm-green ⇒ the kernel is dispatched and
carries load. Here, checkasm-green is decoupled from liveness; the green wall certifies a
museum. The campaign's own `restart/HANDOFF.md` registers this as a known wound ("CSS NEON
dead at admission | G5/G6"), but it remains open.

### 1.3 The backend label lies

`scan_dispatch` (`lib.rs:105-123`) calls the NEON `byte_class_from_table_64` kernel
(line 113), then stamps the result `ScanBackend::Scalar` (line 122). `active_backend()`
(line 139-141) unconditionally returns `Scalar`. The dispatch *enum* exists but the
dispatch *truth* is hard-coded — a reader auditing "which backend ran" is actively misled.

### 1.4 A documented artifact that is not on disk

Lock 16 (`restart/locks/LOCKS.md:474`) asserts *"ffmpeg `x86inc.asm` macro corpus vendored
verbatim at `crates/bbnf-simd/ext/x86/x86inc.asm`."* No such path exists in either tree
(`find` over `bbnf-simd` returns no `.asm`/`.S` files; no `ext/` dir). The lock cites a
vendored corpus as the justification for an x86 macro discipline, but the corpus is absent
— and per the aarch64-only scope (precept 4), x86 asm *should* be absent. The lock text is
stale scope-creep that must be struck, not honored.

---

## Part 2 — The hybrid-emitter architecture proposal

The fix is to build the chain dav1d actually has: **cost model → kernel selection →
emitter composition → checkasm admission**, with the emitter as the single specializer.

### 2.1 Three layers, one contract

```
  KernelSpec (grammar-agnostic primitive ABI: &[u8;64] → mask/positions)
      ├─ reference   : scalar Rust  (the executable spec; always present)
      ├─ neon        : intrinsics   (default aarch64 body)
      └─ neon_asm    : global_asm!  (opt-in, ONLY where §3 verdict says "yes")
```

A `KernelSpec` is a flat-array contract: fixed-width block in, bitmask/positions out, no
grammar identifiers in the signature. `structural_classify_64`'s signature
(`src/aarch64/structural_classify_64.rs:98`) is already the model — its `KindPartition` is
*caller data*, not baked grammar. The `CostModel` — promoted out of the egraph extractor
into a first-class `codegen::cost::KernelCostModel` — reads Grammar-IR facts (alphabet size
and lo6-collision structure, presence of number/string/comment/nesting tokens, max
chunk-spanning token width) and emits a `KernelPlan`: an ordered list of `KernelSpec` ids
with their caller-data partitions. Lock 16 line 476 already names these IR facts
("alphabet size, number-token presence, string-token presence, chunk-spanning-token
presence") — they are specified but unwired; this proposal wires them.

### 2.2 The emitter composes; it does not contain

The emitter consumes the `KernelPlan` and emits a generated scanner that *calls* kernels
by their stable ABI. Crucially the **per-grammar scanner becomes generated output of the
emitter**, deleting `runtime/src/grammars/json/scan.rs` and `runtime/src/runtime_simd.rs`
as hand-written modules. The kernels stay grammar-agnostic in `bbnf-simd`; the
*composition* (which kernel, what partition, what fallback) is the emitter's job and lands
in `runtime/src/grammars/<g>/generated.rs` with the `@generated` banner the CSS module
already carries (`runtime/src/grammars/css_l4_declaration_values/generated.rs:1`). This is
the direct cure for §1.1: afterward, the only grammar-specific bytes are generated, and
`bbnf-simd`/`codegen` carry zero.

### 2.3 Dispatch is one table, selected once, truthfully

Collapse the two parallel selectors (`select_classifier` + `primitive_kernels`, both in
`dispatch.rs`) into one `KernelTable` resolved in a single `OnceLock`, keyed by detected
CPU features (`is_aarch64_feature_detected!` for `dotprod`/`i8mm` where a kernel has an asm
variant). `StructuralIndex.backend` must report the variant that *actually ran*
(`Neon` / `NeonAsm` / `Scalar`), ending the `lib.rs:122` lie. This mirrors dav1d's single
`cpu_flags`-gated function-pointer table.

### 2.4 The harness is generated per grammar, not authored once

Today `checkasm_parity.rs` is an 838-line hand-authored file pinned to JSON
(`CHECKASM_ALPHABET = b"{}[],:\""`, line 33) and four CSS corpora (lines 702-731). In the
dav1d transposition the emitter generates a `checkasm_<grammar>.rs` per grammar from the
same `KernelPlan`: reference body = scalar kernel, candidate = selected variant,
caller-data partitions = the grammar's, corpus = the grammar's fixtures. The alignment
sweep (`0..64`), the signal trampoline (`signal_guard`, lines 137-166), the stack canary
(`with_stack_canary_xor_fold`, already extracted into `checkasm_common.rs:137`), the
bug-injection falsifier (`BBNF_SIMD_INJECT_BUG`), and the robust-mean microbench
(lines 387-410) are *reused machinery*. **Liveness becomes an admission gate**: the
generated harness asserts that every kernel in the `KernelPlan` is reached by the generated
scanner on at least one corpus byte, which would have caught all of §1.2 at codegen time.

A second harness honesty fix: `checkasm_parity.rs:16-20` ships with `BBNF_SIMD_STRICT`
**off by default** — random/misaligned parity divergences are *logged, not failed*. dav1d
checkasm has no such escape hatch; a divergence is a build failure. The generated harness
must default to strict; the non-strict mode is a per-kernel admission-debt marker, never
the steady state.

---

## Part 3 — The honest asm-vs-intrinsics verdict, per kernel class

On Apple-M with LLVM, NEON intrinsics map 1:1 to instructions and LLVM's aarch64 scheduler
is strong; hand asm wins **only** where LLVM provably mis-schedules, refuses an instruction
it has no intrinsic for, or spills across a state boundary. The honest split:

| Kernel class | Verdict | Reasoning |
|---|---|---|
| **Structural classify** (`vceqq` fan / `vqtbl` two-LUT) | **Intrinsics. asm is a trap.** | LLVM schedules the 4-stripe `vceqq`/`vorrq`/movemask tree near-optimally; the loop is throughput-bound on lookup ports, not scheduling. Hand asm freezes register allocation the inliner wants to vary per call site. |
| **Delimiter / set-member find** (`byte_class_from_eq_set_64`) | **Intrinsics.** | Same shape; the `trailing_zeros` early-out is a scalar `rbit`+`clz` LLVM emits cleanly. |
| **Number parse** (`udot` digit MAC) | **Asm, narrowly justified — and temporary.** | `digit_mac.rs:39` uses `asm!` for `udot` because it had *no stable `core::arch` intrinsic* when written. As `vdotq_u32` stabilizes this should **revert to the intrinsic**; the asm is a missing-intrinsic stopgap, not a scheduling win. |
| **Whitespace skip / SWAR byte-class** | **Plain Rust / intrinsics.** | The SWAR floor (`word - 0x2020…`) is scalar; LLVM emits it perfectly. No asm. |
| **String unescape `\uXXXX`** (`vqtbl1q_u8` nibble LUT) | **Intrinsics.** | `unescape_uxxxx_neon` is already pure intrinsics and bit-exact; the per-lane `vgetq_lane` extract tail is the only soft spot, and asm would not beat LLVM's `umov` there. |
| **UTF-8 validate** (Lemire 3-table) | **Intrinsics.** | Canonical aarch64 validators (simdjson, simdutf) are intrinsics; no shipped validator uses hand asm — the table-AND-fold has no un-intrinsic'd op. |
| **Cache hints / streaming store** (`prfm`, `stnp`) | **Asm, mandatory.** | `prfm pldl2strm`, `stnp` (non-temporal pair store) have **no `core::arch` intrinsic** — `cache_hints.rs` is the legitimate floor of hand asm, exactly Lock 16's "admissible only when the intrinsic is absent." |
| **LDP/STP bulk position emit** (`bulk_emit_positions_64`) | **Conditional asm, diff-gated.** | LLVM *usually* fuses adjacent loads into `ldp` on Apple-M, but the compaction store loop sometimes emits singleton `str`s. If `cargo asm` shows un-paired stores, a `global_asm!` body forcing `stp` pairing is justified — **only with a checked-in disassembly diff proving the miss.** Speculative asm here is the trap. |

**The general law:** on Apple-M, hand asm earns its maintenance cost in exactly three
situations — (1) the operation has no `core::arch` intrinsic (`prfm`, `stnp`, pre-stable
`udot`); (2) `cargo asm` *proves* LLVM mis-schedules or fails to pair (`ldp`/`stp`); (3) a
mask-register state machine must persist a value across a chunk boundary LLVM insists on
spilling. Everywhere else intrinsics win, because they inline into the per-grammar
generated scanner and let LLVM co-schedule kernel and caller — a freedom hand asm destroys.
The current tree is *correctly conservative*: 12 of 14 kernels are intrinsics, and the 2
asm kernels are both missing-intrinsic cases. The defect is not asm overuse — it is asm
(and intrinsics) **underuse on the live path**: dead code, not wrong code.

### 3.1 global_asm! vs naked-fn vs `asm!`

For the conditional `ldp`/`stp` cases, prefer **`global_asm!` with a flat leaf** over
`#[naked]` fns: `global_asm!` gives a stable symbol the dispatch table can take a pointer
to (the dav1d model) and survives LLVM inlining, whereas `#[naked]` is rough on stable Rust
and tempts ABI mistakes. Inline `asm!` (as in `digit_mac`/`cache_hints`) is correct for a
*single-instruction* missing-intrinsic insert where you *want* LLVM to schedule around the
one opaque op. The rule: single missing op → inline `asm!`; whole hand-scheduled leaf →
`global_asm!`; never `#[naked]` for kernels.

---

## Part 4 — The kernel inventory worth hand-tuning for parsing

Grammar-agnostic primitives, priority-ordered, verdict folded in:

1. **structural_classify_64** — block→significant+per-kind masks. *Intrinsics.* The
   existing `vceqq` fan is correct; widen to the `vld4q_u8`+per-channel `vqtbl1q_u8`
   LD4-interleaved classifier (Lock 16 line 459; ~10% c/B on Apple-M per Validark) when the
   alphabet exceeds the fan break-even (~16 members).
2. **delimiter/set-member find** — *Intrinsics.* Present (`byte_class_from_eq_set_64`).
3. **ident/number scan boundary** — block→first-non-class position. *Intrinsics.*
4. **number parse** (Eisel-Lemire mantissa + `udot` digit MAC) — *Asm only for `udot`
   until `vdotq` stabilizes; mantissa mul is intrinsics.*
5. **string unescape** (`\uXXXX` + backslash prefix-XOR) — *Intrinsics.* `vqtbl` nibble LUT
   present; prefix-XOR (`escape_mask_64`, `lib.rs:174`) is SWAR, scalar.
6. **UTF-8 validate** — *Intrinsics.* Lemire 3-table; present under `aarch64/utf8/`.
7. **whitespace skip** — *Scalar SWAR floor + NEON for long runs.*
8. **bulk position emit / compaction** — *Conditional `stp` asm, diff-gated.*
9. **cache hints / non-temporal store** — *Asm, mandatory (no intrinsic).*

The msac cross-chunk refill pattern (Lock 16 line 476) is the one genuinely novel
algorithmic lift beyond simdjson and remains unbuilt; it is the only item where a
*state-machine* (not a leaf) justifies hand asm, and it should be the flagship `global_asm!`
target if and only if a profiled chunk-boundary spill is demonstrated.

---

## Recommendations (gestalt, not patches)

1. **Promote the cost model to a first-class `KernelCostModel`** in `codegen/src/cost/`
   driven by Grammar-IR facts (Lock 16 line 476 names them), emitting a `KernelPlan`.
   Delete the implicit hand-wiring in `runtime/grammars/*/scan.rs` and `runtime_simd.rs`.
2. **Make the emitter the specializer.** Generate per-grammar scanners that compose kernels
   by ABI; the only grammar-specific bytes are generated output. This drains the Lock-14
   leak in `runtime/`.
3. **Bind checkasm to liveness and default to strict.** Generate `checkasm_<grammar>.rs`
   from the `KernelPlan`, reuse the sweep/canary/injection machinery, add a liveness
   assertion that every planned kernel is reached on real corpus bytes, and flip
   `BBNF_SIMD_STRICT` on by default. This kills the §1.2 museum kernels.
4. **Tell the truth in the dispatch label.** `StructuralIndex.backend` reports the variant
   that ran; collapse the two selectors in `dispatch.rs` into one feature-gated
   `KernelTable`.
5. **Strike the stale x86 lock text.** Lock 16 line 474 cites a vendored `ext/x86/` corpus
   that does not exist and contradicts the aarch64-only scope; remove it.
6. **Hold the asm line.** Keep intrinsics as default; admit `global_asm!`/`asm!` only for
   the three proven cases (missing intrinsic, disassembly-proven mis-schedule, cross-chunk
   spill), each with a checked-in `cargo asm` diff — never speculatively.
