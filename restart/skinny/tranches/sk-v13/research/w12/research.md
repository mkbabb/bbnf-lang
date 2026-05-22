# SK-V13 W12 Research - SIMD/ASM Production Wiring + Zero Orphans

Date: 2026-05-21.
Scope: W12 under SPEC Section 16.

## Authority

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 16 authorizes W12 to
  complete `a64_ascii_set_run_skip` production wiring or record measured
  rejection, and to audit aarch64 primitive orphan status.
- `skinny/REDRESS.md` item 126 records the SK-V12 W4 delimiter route as
  `ROUTE-PRODUCTION-SPLIT`: checkasm and microbench passed, but no CSS
  production consumer shipped.
- The SK-V13 pre-block ledger keeps REDRESS 88/89/90/122/126 binding:
  checkasm-only and microbench-only admissions reject, PMULL/CSSC structural
  replays need a fresh union/tape consumer, and final orphan count must be zero.

## Cohort Findings

1. The narrow production consumer is the CSS declaration-values `scan_block`
   delimiter loop. Runtime
   `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
   and template
   `skinny/crates/codegen/src/css_l4_declaration_values_templates/generated.rs`
   scan for exactly `b'{'`, `b';'`, and `b'}'`, matching the W4 delimiter
   microbench set `b"{};"`.
2. The existing test-local `find_candidate` in
   `skinny/crates/bbnf-simd/tests/checkasm_ascii_set_member_find_64.rs`
   already uses the admissible primitive shape: 64-byte windows through
   `bbnf_simd::prim::byte_class_from_eq_set_64`, then scalar tail handling.
   W12 must promote that shape into a production API and consume it in the CSS
   scanner.
3. Declaration-values-extended and visual-functions scanners also have
   delimiter loops, but they include quote handling. They require a wider
   delimiter set and additional quote/comment parity cases. W12 should not
   widen into those call sites before the narrow `{};` production split has
   measured.
4. JSON PMULL/CSSC structural SIMD should not be attempted in W12 mainline.
   Clearing REDRESS 88/89/96/97/98 would require a new SIMD-first union/tape
   writer with generated JSON parse consumption in the same wave. Local bitmap
   primitive body swaps would replay historical failures.
5. `xtask primitive-checkasm` currently omits
   `checkasm_ascii_set_member_find_64`; W12 should include it so the caller
   parity/microbench gate is part of the standard primitive checkasm command.
6. No existing W12 companion gate exists. If W12 admits or rejects the
   production split, the report/gate surface should consume scalar reference,
   checkasm, production consumer, CSS equality, measurement, REDRESS-126
   citation, and final orphan count evidence.

## Current Orphan State

REDRESS-126 demotes the five historical aarch64 orphan rows as
`inventory_demoted_with_evidence`: `bitmap_prefix_xor_64`,
`bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, and
`cache_hints`. The files remain in tree, but they are not production
admissions. W12 must avoid creating another orphan and must preserve final
orphan count zero.

## Candidate

Promote a production `bbnf_simd::find_ascii_set_member64(bytes, cursor, end,
set)` wrapper over the existing `byte_class_from_eq_set_64` primitive, then
use it in CSS declaration-values `scan_block` to jump over non-delimiter spans.

This is an ASM/SIMD production wiring attempt, not a CSS feature expansion:

- no new grammar feature row;
- no comparator relaxation;
- no JSON parser, union, tape, or digest change;
- no x86 scope;
- no support-only primitive.

## Falsifiability

W12 admits only if the production-wired CSS row preserves strict fact-stream
equality and the W12 gate consumes the primitive/checkasm/consumer evidence.
If Criterion shows no row movement or regression, the CSS consumer patch is
reverted or recorded as measured rejection per the accepted W12 plan.

## Pre-Blocked Routes

- No second production split after REDRESS-126.
- No checkasm-only or microbench-only close.
- No PMULL/CSSC JSON structural route without a same-wave generated JSON parse
  consumer and material differential against REDRESS 88/89/96/97/98.
- No retained aarch64 orphan at close.
