---
agent: 2B
pass: T-P2-research
cycle: V4
generated_at: 2026-05-21T12:10:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 24
counted_source_ids: [T2B-SRC-FFMPEG-C, T2B-SRC-FFMPEG-H, T2B-SRC-VIDEOLAN-CHECKASM, T2B-SRC-ARM-ACLE, T2B-SRC-ARM-NEON, T2B-SRC-P2-PROMPT, T2B-SRC-T-P1-1D, T2B-SRC-T-P1-1E, T2B-SRC-T-P1-HARDENING, T2B-SRC-V2-FOLD, T2B-SRC-V3-FOLD, T2B-SRC-LOCKS, T2B-SRC-SK-V13-SYNTHESIS, T2B-SRC-SK-V13-HANDOFF, T2B-SRC-SIMD-SCOPING, T2B-SRC-P1-B, T2B-SRC-P1-C, T2B-SRC-P1-E, T2B-SRC-P1-LEDGER, T2B-SRC-P2-B, T2B-SRC-P2-C, T2B-SRC-RESULTS, T2B-SRC-REDRESS, T2B-SRC-BBNF-SIMD]
techniques_grounded: 13
techniques_refuted: 6
v2_fold_authority: restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
v3_fold_authority: restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
v4_fold_authority: restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md
v1_ch_lenses_folded: [CH1, CH2, CH3, CH4, CH5, CH6]
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: [CH1-provenance, CH2-lock14-transfer, CH3-redress-material-differentials, CH4-cost-and-admission-ledger, CH5-substrate-kind-fields, CH6-anti-paper-close-state-machine]
  first_cycle_additions: [2B-L0-vendored-macro-boundary, 2B-L1-primitive-contracts, 2B-checkasm-admission-loop, 2B-lock16-traceability-manifest, 2B-no-orphan-primitive-rule]
  v2_additions: [2B-per-technique-admission-ledger, 2B-source-present-orphan-enum, 2B-lock16-manifest-retention-fields, 2B-redress-121-127-taxonomy, 2B-union-asm-material-differential-checklist]
  v3_additions: [2B-executable-shared-ledger, 2B-normalized-admission-state, 2B-counted-source-ids]
  v4_additions: [2B-non-shortlist-blockers, 2B-redress-slice-ownership]
locks_amendment_candidates: 7
---

# T-P2 2B Primitive Vocabulary Research

## Executive Summary

V2 preserves the V1 layer split but makes it gate-consumable. Layer 0 remains
vendored macro/process infrastructure plus the FFmpeg/VideoLAN checkasm loop:
scalar reference, optimized implementation, differential parity, register /
stack / signal discipline, and benchmark-after-correctness. Layer 1 remains the
bbnf-authored vocabulary of byte-window, mask, carry, table, string, escape,
digit, tail, and context primitives whose policy comes from generated grammar
data or caller data.

The V1 research correctly refused Lock 16 closure from skeleton presence. V2
folds the hardening revise set into mechanical requirements: every candidate
has an admission-ledger row, `BBNF_SIMD_STRICT=1` is mandatory for admission
checkasm, source-present primitives close only as `wired`, `deleted`,
`scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`, and every
manifest row carries `substrate_target`, `retention_lifetime`, and
`policy_owner`. Proof-only and inventory routes are explicitly downgraded until
they wire a same-wave consumer that moves a JSON/CSS/Sheets/BBNF-self row or
records measured rejection / architectural block.

## V2 Fold Authority

This dossier folds the V1 CH1-CH6 hardening set through
`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.

| lens | V2 fold into 2B |
|---|---|
| CH1 correctness / provenance | Moving-source evidence is provenance only when pinned in the addendum register; the Sneller repository citation is removed from 2B authority; architecture-pressure sources do not admit primitives. |
| CH2 generality / Lock 14 | Shared SIMD primitives must receive byte alphabets, delimiters, quote / escape / control, number, string, and no-string policy from generated grammar data or caller data. JSON/CSS policy embedded in Layer 1 is a blocker. |
| CH3 regression / REDRESS | REDRESS 88/89/96/97/98/119/121-127 are folded as route facts; PMULL/CSSC/union labels require the material-differential checklist below before shortlist use. |
| CH4 cost / implementation realism | Every candidate gets an admission-ledger row with LOC/risk/rollback/abrogate fields; proof-only primitives are not S-P3-eligible. |
| CH5 hidden coupling / Lock 1 | Every primitive, union route, imported scanner plan, and consumer declares `substrate_target`, `retention_lifetime`, and `policy_owner`; retained class/mask sidecars remain blocked. |
| CH6 anti-paper-close | "Grounded" means source-backed only. Admission requires scalar reference, strict checkasm/parity, same-wave production consumer, strict equality/oracle, and measured row movement or architectural-block evidence. |

## Technique Grounding Table

| spec-claim or T-P1-divergence-id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 may vendor an established macro substrate instead of inventing ABI macros. | Local vendor attribution: `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5`-`13`; `x86inc.asm` header and calling-convention macro purpose at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:1`-`29`. | grounded | Layer 0 is build-time x86 macro machinery only. It must not become grammar policy or a runtime substrate. |
| Layer 1 is bbnf-authored grammar-neutral primitive vocabulary. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1`-`12`, `:30`-`:44`, `:55`-`:60`. | grounded | `bbnf.asm` already names nine grammar-neutral contracts and states that per-grammar data lives outside the macro library. |
| Scalar-reference plus optimized implementation plus result comparison before bench is the transferable checkasm process. | FFmpeg HEAD `085714182302333dd83dcb9c36cf828dc4eba929` per the V2 addendum, `tests/checkasm/checkasm.c` / `tests/checkasm/checkasm.h`, plus the VideoLAN checkasm process description at `https://www-test.videolan.org/projects/checkasm/`. | grounded | bbnf's admission loop must treat checkasm as a gate before Criterion row claims, not as an after-the-fact test. |
| Register, stack, illegal-instruction, and failure diagnostics are part of process, not decoration. | VideoLAN checkasm supported-platform and failure-mode documentation at `https://www-test.videolan.org/projects/checkasm/`; local canary/register helpers at `skinny/crates/bbnf-simd/tests/checkasm_common.rs:49`-`71` and `:83`-`:111`. | grounded | Lock 16 should require stack/register/fault coverage for handwritten ASM and unsafe intrinsic kernels where applicable. |
| bbnf local checkasm mirrors FFmpeg-style differential parity. | `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`-`20`, `:57`-`:115`. | partial | The harness is strong, but broad parity is strict only when `BBNF_SIMD_STRICT=1`; admission commands must set it. |
| `byte_class_from_eq_set_64` is a valid Layer 1 primitive. | Contract and scalar executable specification at `skinny/crates/bbnf-simd/src/lib.rs:259`-`271`, scalar/checkasm assertion at `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1`-`17`, NEON body at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:27`-`72`. | grounded | Admissible only with caller-supplied byte set. A JSON structural alphabet embedded in shared code is a Lock 14 leak. |
| TBL lookup is an admissible byte-class and escape-decode basis. | Arm Neon Intrinsics Reference maps `vqtbl4q_u8` / table lookup: https://arm-software.github.io/acle/neon_intrinsics/advsimd.html; local TBL classifier at `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:16`-`43`; local hex decode at `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:58`-`166`. | grounded | The TBL core is neutral; JSON `\uXXXX` and CSS variable-length escapes need separate grammar-owned wrappers. |
| ASCII set run-skip is micro-proven but not admitted as production. | Local checkasm/microbench at `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs:20`-`40`, `:103`-`:190`; REDRESS-126 at `skinny/REDRESS.md:3768`-`3820`. | partial | The primitive is a top production candidate only after a CSS scanner consumer lands in the same wave. |
| Escape-mask correctness is a prerequisite, not a throughput primitive. | `skinny/crates/bbnf-simd/tests/checkasm_escape_mask_64.rs:6`-`33`, `:55`-`:123`; REDRESS-122 at `skinny/REDRESS.md:3603`-`3632`. | grounded | It should remain in the vocabulary as a correctness guard; it did not move CSS or JSON rows by itself. |
| Structural scan speed implies a retained union substrate should be consumed. | P1 mode-III structural SIMD ratios at `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:71`-`87`; REDRESS 96/97/98 at `skinny/REDRESS.md:2795`-`2950`. | refuted for tested retained forms | Structural masks are valid transient primitives. Retained class-column and streaming-cursor substrates are measured failures unless a new route names a material differential. |
| PMULL prefix-XOR and CSSC CTZ are automatically admissible once correct. | Arm ACLE feature gates PMULL through the AES/PMULL feature family, DOTPROD through `__ARM_FEATURE_DOTPROD`, SHA3 through `__ARM_FEATURE_SHA3`, and CSSC through `__ARM_FEATURE_CSSC`: https://arm-software.github.io/acle/main/acle.html; REDRESS 88/89 at `skinny/REDRESS.md:2510`-`2585`. | refuted as default hot-body substitutions | They are category-unblocked only with a new same-wave consumer and material differential; local replacement of scalar delegates is already measured as regressive. |
| UDOT digit MAC is an admissible primitive family. | Arm ACLE documents dot-product availability through `__ARM_FEATURE_DOTPROD` at `https://arm-software.github.io/acle/main/acle.html`; local scalar and `udot` body at `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4`-`49`. | partial | The primitive has a scalar oracle and ISA name but no row consumer. It needs numeric-row attribution and checkasm expansion before shortlist. |
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

### A3 - The Existing Vocabulary Splits Into Admission States

V2 replaces the V1 "admitted / candidate / inventory" shorthand with the
shared state machine:

```text
source_backed -> scalar_backed -> checkasm_backed -> micro_proven ->
production_wired -> row_admitted | measured_rejected | architectural_block
```

`source_backed`, `scalar_backed`, `checkasm_backed`, and `micro_proven` are
non-admitting states. A primitive can enter S-P3 only if the wave names the
missing transition and the same-wave consumer that reaches
`production_wired` plus row movement or measured rejection.

#### Per-Technique Admission Ledger

V4 fold note: the executable ledger with LOC, risk, rollback, abrogate
thresholds, normalized `admissibility_state`, separate `disposition_or_blocker`
fields, non-shortlist blockers, and REDRESS-slice ownership lives in
`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md` plus
`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md`. The local table below is
retained as a compact owner summary and must not be used as the gate-consumed
ledger if it disagrees with the V3/V4 addenda.

| candidate_id | source / local status | BBNF_SIMD_STRICT_status | consumer and row/feature gate | substrate / lifetime / policy | V3 state / disposition |
|---|---|---|---|---|---|
| `ascii_set_member64_css_delimiter` | Scalar, NEON/checkasm, and 4.718x microbench in REDRESS-126; production CSS wiring deferred. | Mandatory for admission command; non-strict runs are exploratory. | CSS `scan_block` delimiter or `skip_ws_and_comments` consumer; strict lightningcss/cssparser equality and > lightningcss+1 row movement. | `local_temp_only` / `local_loop` / `generated_grammar`. | `micro_proven`; not admitted until CSS consumer lands. |
| `escape_mask_64` | Scalar/checkasm correctness prerequisite; REDRESS-122 fixed the xorshift falsifier. | Mandatory; historical strict checkasm is prerequisite evidence only. | JSON/CSS string or escape row must consume it in production. | `local_temp_only` / `local_loop` / `generated_grammar` or `caller_data`. | `checkasm_backed`; prerequisite only. |
| `tbl_tbx_escape_decode_batch` | TBL and `unescape_uxxxx` bodies exist; grammar policy split is incomplete for CSS escapes. | Mandatory once selected; checkasm must cover JSON fixed and CSS variable-width escapes separately. | JSON unicode direct/parse/typed row or CSS escaped identifier/string row. | `direct_sink` or `existing_tape` / `generated_function` / `generated_grammar`. | `scalar_backed`; CSS policy/checkasm blocker. |
| `digit_run_accumulate` / UDOT | Local `digit_mac.rs` source and ISA support exist; row consumer and strict matrix are missing. | Missing before admission; must set strict mode in new checkasm. | JSON numeric direct/parse row or CSS number/dimension row. | `direct_sink` or `existing_tape` / `generated_function` / `generated_grammar`. | `source_backed`; scalar/checkasm/consumer blockers. |
| `pmull_cssc_structural_union_emit64` | PMULL/CSSC categories are unblocked, but REDRESS 88/89/96/97/98 block replay. | Missing before shortlist; scalar carry/order oracle plus strict checkasm required. | Row-local union/structural consumer that deletes or bypasses old scalar cost in that row. | `existing_tape`, `direct_sink`, or `admitted_fact_output` / `local_loop` or `generated_function` / `generated_grammar`. | `source_backed`; high-risk REDRESS-differential blocker. |
| `string_context_64` / `byte_context` | `vextq_u8` context helpers exist with no production caller. | Missing. | JSON/CSS string-context consumer with cross-chunk policy and row gate. | `local_temp_only` / `local_loop` / `generated_grammar`. | `source_backed`; source-present inventory blocker. |
| `cache_hints` | PRFM/STNP wrappers exist with no row consumer. | Missing and likely not a parity primitive unless wrapped in measurable store/prefetch row. | Store/prefetch hot leaf with strict no-regression guards, or deletion. | `local_temp_only` / `local_loop` / `none` unless generated code owns policy. | `source_backed`; source-present inventory blocker. |
| `bbnf-regex` extraction | Conditional import authority only; parse-that sibling worktree is pinned in the addendum but dirty. | Not a SIMD primitive; use parity/equality gates for scanner facts. | HIR-to-bbnf mapping consumed by resolver and generated parser rows. | `local_temp_only` compile-time facts / `generated_function` / `generated_grammar`. | `source_backed`; import/snapshot blocker. |
| e-graph / CSP / cost resolver | Architecture candidate; not a primitive admission. | Not applicable. | JSON/CSS equality, bounded saturation/CSP report, and cascade fail-closed row guards. | extracted plan must declare `substrate_target` per node. | `source_backed`; abrogate caps in V3 addendum. |

#### Source-Present SIMD / ASM Orphan State

At close every source-present primitive is exactly one of:

```text
wired
deleted
scalar-delegate-non-ASM
architectural-block-with-REDRESS
```

`inventory_demoted_with_evidence` is historical REDRESS evidence only. It is
not a V2 close state under the full-SOTA addendum.

| primitive | local evidence | allowed V2 disposition |
|---|---|---|
| `bitmap_prefix_xor_64` aarch64 | Selected by dispatch but delegates to scalar (`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`-`4`). | `scalar-delegate-non-ASM` unless a row-local PMULL route wires and admits/rejects. |
| `bitmap_next_set_bit` aarch64 | Scalar delegate (`skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`-`4`). | `scalar-delegate-non-ASM` unless a row-local CSSC/CTZ route wires and admits/rejects. |
| `bulk_emit_positions_64` aarch64 | Scalar delegate behind unsafe wrapper (`skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`-`4`). | `scalar-delegate-non-ASM` unless a row-local bulk consumer wires and admits/rejects. |
| `byte_context` | `vextq_u8` helpers, no production caller. | `wired`, `deleted`, or `architectural-block-with-REDRESS`; no inventory close. |
| `cache_hints` | PRFM/STNP wrappers, no production caller. | `wired`, `deleted`, or `architectural-block-with-REDRESS`; no inventory close. |
| x86 `bbnf.asm` contracts | Valid Layer 1 vocabulary and macro contract, but SK-V13 implementation is aarch64 only. | totality vocabulary only; not SK-V13 production admission. |

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
| `candidate_id` / `primitive_id` | Stable Layer 1 contract name, e.g. `BYTE_CLASS_FROM_EQ_SET_64`. |
| `owner_dossier` | Owning T-P2 dossier or downstream wave plan. |
| `source_paths_or_external_source` | Scalar, candidate, dispatch, checkasm, and pinned external source files. |
| `isa_allowlist` / `hardware_gate` | Lock 16 row, target feature, and primary ISA/source citation. |
| `scalar_reference` | Executable reference function. |
| `checkasm_or_parity_command` | Strict command, adversarial classes, and parity corpus. |
| `BBNF_SIMD_STRICT_status` | `required`, `not_applicable`, or `missing_before_admission`; broad non-strict parity may not admit. |
| `corpus_or_equality_oracle` | CSS/lightningcss/cssparser, JSON/sonic strict/Track 2, or grammar-specific oracle. |
| `same_wave_consumer_path` | Runtime/codegen path that consumes it in production. |
| `expected_row_or_feature_gate` | JSON/CSS/Sheets/BBNF-self row moved, measured reject, or architectural block. |
| `loc_budget` / `risk_class` | Implementation envelope and review risk. |
| `rollback_path` / `abrogate_threshold` | Revert plan and fail-closed criteria. |
| `admissibility_state` | One of the V2 state-machine values above. |
| `substrate_target` | `local_temp_only`, `existing_tape`, `direct_sink`, or `admitted_fact_output`. |
| `retention_lifetime` | `local_loop`, `generated_function`, or `output_row`. |
| `policy_owner` | `generated_grammar`, `caller_data`, or `none`; shared primitive crates cannot own grammar policy. |
| `source_present_orphan_state` | `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`. |

Without this manifest, Lock 16 remains an intention rather than an enforceable
interface.

### A5 - REDRESS 121-127 Taxonomy

V2 treats SK-V12 close entries as admission taxonomy, not shorthand for future
closure.

| REDRESS entry | V2 state | downstream meaning for 2B |
|---|---|---|
| 121 GrammarConfig legality | prerequisite / partial Lock 14 repair | does not close grammar-policy transfer for primitives alone. |
| 122 escape-mask correctness | prerequisite only | no production scanner, no row admission, no SIMD throughput claim. |
| 123 CSS scaffold | generated baseline/oracle scaffold | no SOTA admission. |
| 124 lightningcss comparator | comparator/equality infrastructure | no SOTA admission. |
| 125 CSS SOTA candidate | measured candidate | admitted only through 127 close record. |
| 126 ASCII run-skip | microbench route-production split | not production SIMD admission. |
| 127 SK-V12 close | CSS declaration-values row admit; union/ASM still routed | not full CSS parity, not zero-orphan perfection, not a future-work waiver. |

### A6 - Union / ASM Material-Differential Checklist

`Union-C`, `PMULL+CSSC`, `SIMD-first union`, `UDOT`, `TBL/TBX`, or any other
ASM-gen label is not shortlist-safe unless the wave plan proves the checklist:

1. Prior REDRESS route(s) are cited: 88, 89, 96, 97, 98, 119, 122, and/or 126
   as applicable.
2. The route does not globally replace `bitmap_prefix_xor_64` or
   `bitmap_next_set_bit` default bodies.
3. The route does not replay retained class-column, streaming cursor,
   class-lane-only, parser-owned sidecar, or `UnionTape` shapes.
4. The old scalar cost source is deleted or bypassed inside the named row
   consumer, not merely renamed.
5. The consumer owns masks/positions in one loop and emits into
   `existing_tape`, `direct_sink`, or `admitted_fact_output`.
6. Scalar reference, `BBNF_SIMD_STRICT=1` checkasm/parity command, feature gate,
   and isolated microbench are named before S-P3 scoping.
7. Strict row gate, guard rows, rollback path, and abort criteria are named.

### A7 - Abrogate Gates For Primitive Routes

Primitive, union, and resolver routes fail closed when:

| gate | threshold |
|---|---|
| parity/checkasm | Any scalar/checkasm/equality failure. |
| row regression | Any previously admitted JSON/CSS row silently demotes. |
| generated LOC growth | Exceeds the candidate ledger `loc_budget` upper bound, or a stricter SPEC wave budget if S-P3 names one, without traced O(N) reason. |
| stale cost evidence | More than 30 percent of candidate expressions use stale/static fallback. |
| e-graph saturation | Node/iteration cap exceeded without equality-preserving extraction. |
| CSP solve | More than 1 s per grammar or unresolved UNSAT cause. |

## Architectural Assertions Refuted

### R1 - Primitive Parity Alone Is Not Admission

REDRESS-122 fixed `escape_mask_64` and its historical falsifier, but explicitly
made no production scanner, SIMD body, gate, `RESULTS.md`, or row admission
change (`skinny/REDRESS.md:3603`-`3632`). REDRESS-126 proved
`a64_ascii_set_run_skip` at 4.718x in microbench and still routed production
wiring separately (`skinny/REDRESS.md:3768`-`3820`). Therefore T-P3 must reject
any support-only or proof-only primitive wave unless it records a measured
rejection or architectural block for the touched row family. "Grounded" in 2B
means source-backed only; it is not an admission state.

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
REDRESS cited as material differential. If it cannot satisfy A6, it remains an
inventory route no matter how strong the instruction-level source evidence is.

### R4 - Layer 1 Cannot Encode Grammar Policy

`byte_class_from_eq_set_64` is neutral because it accepts a caller-supplied set
and returns a mask. `classify_tbl4` currently accepts `terminator`, `escape`,
and `control_limit` parameters in its low-level function, but dispatch hardcodes
JSON quote/backslash/control values when selecting `NeonTbl4`
(`skinny/crates/bbnf-simd/src/dispatch.rs:22`-`33`). That is acceptable for
the current JSON caller but not for shared CSS or arbitrary grammar use.
T-P3 must require generated grammar or caller-data policy at every
non-JSON/shared call site, and the manifest must record `policy_owner`.

### R5 - Orphan Demotion Is Not Close By Deletion Of The Word "Orphan"

SK-V12 closed five aarch64 orphans as `inventory_demoted_with_evidence`, and
the files still exist. REDRESS-126 accounts them as final orphan count zero
only because the close disposition demoted the inventory and separated
`a64_ascii_set_run_skip` as a routed production split (`skinny/REDRESS.md:3806`
-`3812`, `:3869`-`:3872`). Under the SK-V13 addendum, this is not enough for
perfection: each inventory primitive must be wired same-wave, deleted, or
carried only as `scalar-delegate-non-ASM` or
`architectural-block-with-REDRESS`. Historical
`inventory_demoted_with_evidence` cannot satisfy the SK-V13 zero-orphan bar.

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
| Whether byte-context and cache-hint inventory should be deleted. | If no W13+ row names a string-context or store/prefetch hot leaf, remove the files or mark `architectural-block-with-REDRESS` rather than retaining support-only APIs. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | lock(s) | proposed amendment candidate | supporting evidence |
|---|---|---|---|
| LAC-2B-01 | Lock 16 | Add a primitive traceability manifest requirement: every intrinsic/`asm!` site maps to a stable primitive id, primary ISA/library citation, scalar reference, strict checkasm cell, corpus/equality parity where applicable, same-wave consumer, row gate, LOC/risk, rollback, and abrogate threshold. | T-P1 1E Lock 16 partial verdict; local `asm!` in `digit_mac.rs` and `cache_hints.rs`; dispatch selecting scalar delegates; CH4 V1 fold requirement. |
| LAC-2B-02 | Lock 16 | Make `BBNF_SIMD_STRICT=1` mandatory for all admission checkasm commands; non-strict parity may be exploratory only. | `checkasm_parity.rs:16`-`20` says broad divergences are logged but non-failing unless strict is set; REDRESS-122 commands used strict mode. |
| LAC-2B-03 | Lock 16 / Lock 14 | Add `G-SIMD-GRAMMAR-POLICY`: any shared `bbnf-simd` consumer must name quote, escape, control, delimiter, number, and no-string/no-number policy from GrammarConfig or generated grammar code. | SK-V13 SYNTHESIS pre-block for shared classifier dispatch; dispatch currently passes JSON quote/backslash/control constants. |
| LAC-2B-04 | Lock 16 | Encode zero-orphan close as `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`; `inventory_demoted_with_evidence` is not a close state. | REDRESS-126 demoted five aarch64 files while leaving them in tree; user addendum requires zero orphans at close; CH6 requires terminology reconciliation. |
| LAC-2B-05 | Lock 1 / Lock 16 | Distinguish transient mask primitives from retained substrate attempts: byte masks/positions may be consumed within a row, but any retained class column/cursor/vector must cite REDRESS 96/97/98 and name a new material differential. | REDRESS 96/97/98 falsified retained structural variants; P1-C still shows scanner micro-speed as useful antecedent. |
| LAC-2B-06 | Lock 1 / Lock 16 | Add `substrate_target`, `retention_lifetime`, and `policy_owner` to every primitive / scanner / union manifest row; reject retained masks/classes outside `local_loop`, `generated_function`, or admitted output rows. | CH5 V1 fold requirement; V2 addendum Lock 1 substrate-kind contract. |
| LAC-2B-07 | Lock 16 / REDRESS | Require the A6 material-differential checklist for PMULL/CSSC/union/ASM-gen reopen routes before S-P3 shortlist use. | CH3 and CH6 warned that broad "union" or "ASM" labels can replay REDRESS 88/89/96/97/98 without row-local consumer evidence. |

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
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/hardening/V1/CH1.md`.
- `restart/audit/totality/p2/hardening/V1/CH2.md`.
- `restart/audit/totality/p2/hardening/V1/CH3.md`.
- `restart/audit/totality/p2/hardening/V1/CH4.md`.
- `restart/audit/totality/p2/hardening/V1/CH5.md`.
- `restart/audit/totality/p2/hardening/V1/CH6.md`.
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
