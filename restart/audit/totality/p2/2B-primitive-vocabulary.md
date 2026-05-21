---
agent: 2B
pass: T-P2-research
cycle: V1
generated_at: 2026-05-21T08:38:19Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 24
techniques_grounded: 13
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [2B-L0-vendored-macro-boundary, 2B-L1-primitive-contracts, 2B-checkasm-admission-loop, 2B-lock16-traceability-manifest, 2B-no-orphan-primitive-rule]
locks_amendment_candidates: 5
---

# T-P2 2B Primitive Vocabulary Research

## Executive Summary

The defensible primitive layer is two-layered and narrower than the V1 prose
implies. Layer 0 is a vendored macro/process substrate: `x86inc.asm` and
`x86util.asm` give build-time calling-convention and macro mechanics, while the
FFmpeg/VideoLAN checkasm process gives the scalar-reference, differential
parity, register/stack/signal, and benchmark-after-correctness loop. Layer 1 is
the bbnf-authored vocabulary: byte-class, bitmap, escape, string, digit, tail,
and context primitives whose inputs are byte windows, masks, carries, tables,
and caller-supplied grammar policy.

The local code has much of the skeleton: scalar references, dedicated
`checkasm_*` tests, a primitive dispatch table, and the `bbnf.asm` contract
file. It is not yet a closed Lock 16 system. Several aarch64 entries are
scalar delegates or support-only inventory, `BBNF_SIMD_STRICT` is opt-in in
the broad parity harness, and no manifest proves every intrinsic or `asm!`
site maps to an allowlist row, scalar oracle, checkasm cell, corpus parity, and
same-wave row consumer. T-P3 should amend Lock 16 to require that manifest and
make support-only SIMD landings impossible.

## Technique Grounding Table

| spec-claim or T-P1-divergence-id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 may vendor an established macro substrate instead of inventing ABI macros. | Local vendor attribution: `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5`-`13`; `x86inc.asm` header and calling-convention macro purpose at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:1`-`29`. | grounded | Layer 0 is build-time x86 macro machinery only. It must not become grammar policy or a runtime substrate. |
| Layer 1 is bbnf-authored grammar-neutral primitive vocabulary. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1`-`12`, `:30`-`:44`, `:55`-`:60`. | grounded | `bbnf.asm` already names nine grammar-neutral contracts and states that per-grammar data lives outside the macro library. |
| Scalar-reference plus optimized implementation plus result comparison before bench is the transferable checkasm process. | FFmpeg `checkasm_check_func` selects/refers to prior reference versions before benchmark eligibility: https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html lines 1046-1093; VideoLAN checkasm example calls reference, new implementation, compares, then benches: https://www-test.videolan.org/projects/checkasm/ lines 104-126. | grounded | bbnf's admission loop must treat checkasm as a gate before Criterion row claims, not as an after-the-fact test. |
| Register, stack, illegal-instruction, and failure diagnostics are part of process, not decoration. | VideoLAN supported-platform and failure examples: https://www-test.videolan.org/projects/checkasm/ lines 78-87 and 164-188; local canary/register helpers at `skinny/crates/bbnf-simd/tests/checkasm_common.rs:49`-`71` and `:83`-`:111`. | grounded | Lock 16 should require stack/register/fault coverage for handwritten ASM and unsafe intrinsic kernels where applicable. |
| bbnf local checkasm mirrors FFmpeg-style differential parity. | `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`20`, `:57`-`:115`. | partial | The harness is strong, but broad parity is strict only when `BBNF_SIMD_STRICT=1`; admission commands must set it. |
| `byte_class_from_eq_set_64` is a valid Layer 1 primitive. | Contract and scalar executable specification at `skinny/crates/bbnf-simd/src/lib.rs:259`-`271`, scalar/checkasm assertion at `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1`-`17`, NEON body at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:27`-`72`. | grounded | Admissible only with caller-supplied byte set. A JSON structural alphabet embedded in shared code is a Lock 14 leak. |
| TBL lookup is an admissible byte-class and escape-decode basis. | Arm Neon Intrinsics Reference maps `vqtbl4q_u8` / table lookup: https://arm-software.github.io/acle/neon_intrinsics/advsimd.html; local TBL classifier at `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:16`-`43`; local hex decode at `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:58`-`166`. | grounded | The TBL core is neutral; JSON `\uXXXX` and CSS variable-length escapes need separate grammar-owned wrappers. |
| ASCII set run-skip is micro-proven but not admitted as production. | Local checkasm/microbench at `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs:20`-`40`, `:103`-`:190`; REDRESS-126 at `skinny/REDRESS.md:3768`-`3820`. | partial | The primitive is a top production candidate only after a CSS scanner consumer lands in the same wave. |
| Escape-mask correctness is a prerequisite, not a throughput primitive. | `skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs:6`-`33`, `:55`-`:123`; REDRESS-122 at `skinny/REDRESS.md:3603`-`3632`. | grounded | It should remain in the vocabulary as a correctness guard; it did not move CSS or JSON rows by itself. |
| Structural scan speed implies a retained union substrate should be consumed. | P1 mode-III structural SIMD ratios at `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:71`-`87`; REDRESS 96/97/98 at `skinny/REDRESS.md:2795`-`2950`. | refuted for tested retained forms | Structural masks are valid transient primitives. Retained class-column and streaming-cursor substrates are measured failures unless a new route names a material differential. |
| PMULL prefix-XOR and CSSC CTZ are automatically admissible once correct. | Arm ACLE feature gates PMULL through the AES/PMULL feature family, DOTPROD through `__ARM_FEATURE_DOTPROD`, SHA3 through `__ARM_FEATURE_SHA3`, and CSSC through `__ARM_FEATURE_CSSC`: https://arm-software.github.io/acle/main/acle.html; REDRESS 88/89 at `skinny/REDRESS.md:2510`-`2585`. | refuted as default hot-body substitutions | They are category-unblocked only with a new same-wave consumer and material differential; local replacement of scalar delegates is already measured as regressive. |
| UDOT digit MAC is an admissible primitive family. | Arm ACLE dot-product availability: https://arm-software.github.io/acle/main/acle.html lines 4193-4199; local scalar and `udot` body at `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4`-`49`. | partial | The primitive has a scalar oracle and ISA name but no row consumer. It needs numeric-row attribution and checkasm expansion before shortlist. |
| `byte_context` and `cache_hints` can stay as support primitives. | Local `vextq_u8` wrappers at `skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1`-`10`; local PRFM/STNP wrappers at `skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1`-`32`; REDRESS-126 orphan disposition at `skinny/REDRESS.md:3806`-`3812`, `:3869`-`:3872`. | refuted for admission | They are inventory until consumed by a string/context or store/prefetch row, or deleted/demoted with evidence. |
| `bbnf-simd` dispatch already gives one canonical primitive path. | Primitive table at `skinny/crates/bbnf-simd/src/dispatch.rs:49`-`74`; `prim::*` wrappers at `skinny/crates/bbnf-simd/src/lib.rs:231`-`271`. | partial | Dispatch exists, but several selected aarch64 entries are scalar delegates, and x86 `.asm`/intrinsic/direct module paths are not yet manifest-normalized. |
| Producer-only SIMD, resolver, union, or codegen artifacts can close support waves. | SK-V13 pre-blocks producer-only artifacts at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:239`-`263`; handoff REVISE rule at `restart/skinny/tranches/sk-v13/HANDOFF.md:153`-`165`. | refuted | Every primitive needs a same-wave measured consumer, or the wave is proof-only/reject. |

## Architectural Assertions Defended

### A1 - Layer 0 Is Process And Macro Infrastructure

Layer 0 should stay small and boring: vendored x86 macro headers plus the
checkasm discipline. Local vendor metadata says `x86inc.asm` originates from
x264 and is also vendored by FFmpeg, while `x86util.asm` originates from
FFmpeg (`skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:9`-`43`). The
`x86inc.asm` header defines itself as a NASM/YASM abstraction layer for calling
conventions and DSP-oriented macro ergonomics
(`skinny/crates/bbnf-simd/ext/x86/x86inc.asm:24`-`28`). That is exactly the
right Layer 0 boundary: it supplies ABI mechanics, register naming, stack
rules, macro expansion, and deterministic assembly builds. It must not carry
JSON, CSS, or Sheets policy.

The process half of Layer 0 is FFmpeg/VideoLAN checkasm. VideoLAN's public page
states checkasm verifies assembly correctness and benchmarks performance, lists
register/stack/state checks for ARM64 and x86, and shows the reference/new
call, compare, then benchmark sequence. FFmpeg's `checkasm_check_func` records
function versions and returns a reference implementation for comparison before
`checkasm_bench_func` allows benchmarking. bbnf's local parity harness already
mirrors that shape with bit-identical inputs, source-mutation checks,
misalignment sweeps, signal trapping, a stack canary, and strict corpus parity
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`20`).

### A2 - Layer 1 Is A Grammar-Neutral Contract Vocabulary

`bbnf.asm` is the current Layer 1 contract. It declares nine primitives:
`BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
`BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
`EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, and
`FRAME_POP_BOUNDED` (`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30`-`44`).
The same file states scalar references live under `src/scalar/*.rs` and are
the executable specification for checkasm (`:9`-`:12`), while per-grammar LUTs
and transition data live in generated per-grammar data, not in the macro
library (`:55`-`:60`).

That contract is sound if T-P3 keeps the vocabulary at the byte/mask/carry
level. The primitive owns a byte-window operation; the grammar owns quote,
escape, control, delimiter, number, and tuple policy. This aligns with SK-V13's
`G-SIMD-GRAMMAR-POLICY` pre-block: shared `bbnf-simd` consumers cannot inherit
JSON quote/escape/control constants into CSS, union, parse-only, or shared
generated code (`restart/skinny/tranches/sk-v13/SYNTHESIS.md:246`-`261`).

### A3 - The Existing Vocabulary Splits Into Admit, Candidate, And Inventory

Admitted or near-admitted shapes:

| primitive family | local status | T-P3 disposition |
|---|---|---|
| Byte-set classify / member find | Scalar reference, NEON body, checkasm, and W4 microbench exist; production CSS wiring deferred. | Shortlist only with CSS scan-block consumer in same wave. |
| Escape mask | Independent byte-walk scalar checkasm and historical falsifier coverage landed as correctness prerequisite. | Keep as mandatory gate; not row movement by itself. |
| String block / TBL escape decode | Scalar and NEON bodies exist for JSON string special blocks and `\uXXXX` decode. | Select only when JSON/CSS string row names a consumer and grammar policy. |
| Structural scan masks | `scan_structurals`/`scan_tail` are measured scanner probes; local dispatch table uses class/table and bulk emit. | Valid transient primitive evidence; not retained union proof. |

Candidate shapes that need missing evidence:

| primitive family | missing before shortlist | admission route |
|---|---|---|
| UDOT digit MAC | Dedicated checkasm matrix and numeric hot-leaf consumer. | JSON/CSS number row; no proof-only landing. |
| PMULL + CSSC CTZ union emit | New material differential vs REDRESS 88/89/96/97/98 and checkasm for carry/order. | SIMD-first union consumer, not scalar-delegate replacement. |
| 64-byte string context | 64-byte scalar oracle, cross-chunk policy, and row profile. | JSON/CSS string or escaped-ident row. |

Inventory that must not admit alone:

| primitive | local evidence | disposition |
|---|---|---|
| `bitmap_prefix_xor_64` aarch64 | Selected by dispatch but delegates to scalar (`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`-`4`). | Consumer-gated rewrite or keep scalar. |
| `bitmap_next_set_bit` aarch64 | Scalar delegate (`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`-`4`). | Consumer-gated CSSC/CTZ route only. |
| `bulk_emit_positions_64` aarch64 | Scalar delegate behind unsafe wrapper (`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`-`4`). | Consumer-gated rewrite only. |
| `byte_context` | `vextq_u8` helpers, no production caller. | Fold into string-context consumer or delete/demote. |
| `cache_hints` | PRFM/STNP wrappers, no production caller. | Only row-measured prefetch/store wave or delete/demote. |

### A4 - Lock 16 Needs Mechanical Traceability

T-P1 1E already recorded Lock 16 as partial because no artifact proves every
`core::arch::*` and `asm!` site maps to an allowlist row plus scalar parity,
corpus parity, and same-wave consumer evidence. This 2B pass confirms that
finding. A grep over `skinny/crates/bbnf-simd/src` shows `asm!` in
`digit_mac.rs` and `cache_hints.rs`, unsafe Neon intrinsics in classifier,
unicode, movemask, string, and context modules, and x86 feature-gated stubs.
Some are active, some are scalar delegates, and some are inventory.

T-P3 should require a generated or hand-audited primitive manifest with one row
per primitive and per intrinsic/ASM use site:

| field | purpose |
|---|---|
| `primitive_id` | Stable Layer 1 contract name, e.g. `BYTE_CLASS_FROM_EQ_SET_64`. |
| `source_paths` | Scalar, candidate, dispatch, and checkasm files. |
| `isa_allowlist` | Lock 16 row and primary ISA/source citation. |
| `scalar_reference` | Executable reference function. |
| `checkasm_cell` | Strict command and adversarial classes. |
| `corpus_parity` | Corpus/equality guard where applicable. |
| `same_wave_consumer` | Runtime/codegen path that consumes it in production. |
| `row_gate` | JSON/CSS/Sheets row moved, measured reject, or architectural block. |
| `orphan_status` | `wired`, `deleted`, `demoted_with_evidence`, or `blocked`. |

Without this manifest, Lock 16 remains an intention rather than an enforceable
interface.

## Architectural Assertions Refuted

### R1 - Primitive Parity Alone Is Not Admission

REDRESS-122 fixed `escape_mask_64` and its historical falsifier, but explicitly
made no production scanner, SIMD body, gate, `RESULTS.md`, or row admission
change (`skinny/REDRESS.md:3603`-`3632`). REDRESS-126 proved
`a64_ascii_set_run_skip` at 4.718x in microbench and still routed production
wiring separately (`skinny/REDRESS.md:3768`-`3820`). Therefore T-P3 must reject
any support-only or proof-only primitive wave unless it records a measured
rejection or architectural block for the touched row family.

### R2 - Retained Structural Union Is Not Proven By Scanner Speed

P1-C measured structural SIMD faster than scalar scans on every JSON corpus,
but REDRESS 96 and 97 show two correctness-green retained-consumer
implementations missed every target/guard row. REDRESS 98 names the empirical
finding: the retained class/cursor substrate adds memory traffic and cursor
indirection that the scalar loop does not pay on the host
(`skinny/REDRESS.md:2910`-`2934`). Structural masks stay in the vocabulary as
transient producer facts; they do not justify replaying class-column,
streaming-cursor, parser-owned sidecar, or parallel `UnionTape` shapes.

### R3 - PMULL And CSSC Are Not Drop-In Fixes

REDRESS 88 rejected PMULL as a default `bitmap_prefix_xor_64` production body
after JSON row regressions; REDRESS 89 rejected a narrowed CTZ/bulk consumer
despite correctness and asm proof (`skinny/REDRESS.md:2510`-`2585`). The user
pin unblocks the category, not the measured implementation. A future PMULL or
CSSC wave needs a new body/consumer contract, e.g. SIMD-first union emission
that deletes scalar structural consumption in the measured row, with the old
REDRESS cited as material differential.

### R4 - Layer 1 Cannot Encode Grammar Policy

`byte_class_from_eq_set_64` is neutral because it accepts a caller-supplied set
and returns a mask. `classify_tbl4` currently accepts `terminator`, `escape`,
and `control_limit` parameters in its low-level function, but dispatch hardcodes
JSON quote/backslash/control values when selecting `NeonTbl4`
(`skinny/crates/bbnf-simd/src/dispatch.rs:22`-`33`). That is acceptable for
the current JSON caller but not for shared CSS or arbitrary grammar use.
T-P3 must require GrammarConfig policy at every non-JSON/shared call site.

### R5 - Orphan Demotion Is Not Close By Deletion Of The Word "Orphan"

SK-V12 closed five aarch64 orphans as `inventory_demoted_with_evidence`, and
the files still exist. REDRESS-126 accounts them as final orphan count zero
only because the close disposition demoted the inventory and separated
`a64_ascii_set_run_skip` as a routed production split (`skinny/REDRESS.md:3806`
-`3812`, `:3869`-`:3872`). Under the SK-V13 addendum, this is not enough for
perfection: each inventory primitive must be wired same-wave, deleted, or
carried only with architectural-block evidence.

### R6 - x86 Layer 0 Is Not SK-V13 Implementation Scope

Layer 0 x86 macro vendoring is valid totality architecture, but SK-V13
implementation is aarch64 / Apple Silicon only. x86 AVX-512 stubs and
`bbnf.asm` x86 contracts should inform the primitive vocabulary, not drive
SK-V13 wave selection or benchmark claims.

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Exact dav1d source-file anchors for the old internal checkasm copy and the macro corpus lineage. | If T-P3 wants dav1d file-level claims, fetch exact `code.videolan.org/videolan/dav1d` raw source anchors. Until then, cite VideoLAN checkasm lineage only at project level and use FFmpeg source for call/check/bench mechanics. |
| Complete intrinsic/ASM manifest coverage. | Run `rg -n "core::arch|asm!|target_feature|cfg\\(target_arch" skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/tests`, then materialize the Lock 16 manifest row-by-row. |
| Whether `a64_ascii_set_run_skip` moves a real CSS row after production wiring. | Wire the CSS `scan_block` delimiter loop or `skip_ws_and_comments` consumer in the same wave, then run strict lightningcss/cssparser equality plus Criterion on the CSS row. |
| Whether UDOT digit MAC moves any JSON/CSS number row. | Add strict checkasm for signs, decimal/exponent boundaries, invalid lanes, and overflow; profile numeric row leaves after decision-engine changes; wire only with row consumer. |
| Whether PMULL+CSSC can win as a SIMD-first union route. | Micro-prove a new matrix+rank-order emitter against scalar carry/order, then consume it in a generated row. Do not replace scalar delegates globally. |
| Whether byte-context and cache-hint inventory should be deleted. | If no W13+ row names a string-context or store/prefetch hot leaf, remove the files or mark architectural-block in REDRESS rather than retaining support-only APIs. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment candidate | supporting evidence |
|---|---|---|---|
| LAC-2B-01 | Lock 16 | Add a primitive traceability manifest requirement: every intrinsic/`asm!` site maps to a stable primitive id, primary ISA/library citation, scalar reference, strict checkasm cell, corpus/equality parity where applicable, same-wave consumer, and row gate. | T-P1 1E Lock 16 partial verdict; local `asm!` in `digit_mac.rs` and `cache_hints.rs`; dispatch selecting scalar delegates. |
| LAC-2B-02 | Lock 16 | Make `BBNF_SIMD_STRICT=1` mandatory for all admission checkasm commands; non-strict parity may be exploratory only. | `checkasm_parity.rs:16`-`20` says broad divergences are logged but non-failing unless strict is set; REDRESS-122 commands used strict mode. |
| LAC-2B-03 | Lock 16 / Lock 14 | Add `G-SIMD-GRAMMAR-POLICY`: any shared `bbnf-simd` consumer must name quote, escape, control, delimiter, number, and no-string/no-number policy from GrammarConfig or generated grammar code. | SK-V13 SYNTHESIS pre-block for shared classifier dispatch; dispatch currently passes JSON quote/backslash/control constants. |
| LAC-2B-04 | Lock 16 | Encode zero-orphan close as `wired`, `deleted`, or `architectural-block`; `inventory_demoted_with_evidence` is not enough for a perfected tranche unless the file is non-production dead inventory with explicit REDRESS. | REDRESS-126 demoted five aarch64 files while leaving them in tree; user addendum requires zero orphans at close. |
| LAC-2B-05 | Lock 1 / Lock 16 | Distinguish transient mask primitives from retained substrate attempts: byte masks/positions may be consumed within a row, but any retained class column/cursor/vector must cite REDRESS 96/97/98 and name a new material differential. | REDRESS 96/97/98 falsified retained structural variants; P1-C still shows scanner micro-speed as useful antecedent. |

## Sources

Primary external sources:

- FFmpeg `tests/checkasm/checkasm.c` source, `checkasm_check_func`,
  `checkasm_bench_func`, and failure handling:
  https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html.
- FFmpeg `tests/checkasm/checkasm.h` source:
  https://www.ffmpeg.org/doxygen/8.0/checkasm_8h_source.html.
- VideoLAN checkasm project page, API example, supported platforms, benchmark
  and failure examples, dav1d lineage note:
  https://www-test.videolan.org/projects/checkasm/.
- Arm C Language Extensions 2026Q1:
  https://arm-software.github.io/acle/main/acle.html.
- Arm Neon Intrinsics Reference:
  https://arm-software.github.io/acle/neon_intrinsics/advsimd.html.

Primary local sources:

- `restart/prompts/totality/PASS-2-RESEARCH.md`.
- `restart/audit/totality/p1/1D-skinny-lessons.md`.
- `restart/audit/totality/p1/1E-locks-evidence.md`.
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`.
- `restart/locks/LOCKS.md`.
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.
- `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md`.
- `restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
- `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR`.
- `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`.
- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`.
- `skinny/crates/bbnf-simd/src/lib.rs`.
- `skinny/crates/bbnf-simd/src/dispatch.rs`.
- `skinny/crates/bbnf-simd/src/aarch64/*.rs`.
- `skinny/crates/bbnf-simd/src/scalar/*.rs`.
- `skinny/crates/bbnf-simd/tests/checkasm_*.rs`.
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`.
