# SK-V12 S-P2 CHALLENGE V2 — CH4 Cost

Disposition: ACCEPT.

Lens: CH4 COST.
Date: 2026-05-20.
Scope: verify that every current S-P2 candidate carries scalar-reference
status or sketch, checkasm/parity expectation or N/A, and same-wave
consumer/proof note; verify that non-candidates are clearly ineligible.

## Basis

PASS-2 makes the CH4 rule explicit: each candidate must carry a
scalar-reference status, a checkasm-parity expectation, and a same-wave
consumer note; a candidate missing any one of the three fails this lens
(`restart/prompts/skinny/PASS-2-RESEARCH.md:119`-`:124`). ORCHESTRATOR §8
makes scalar/checkasm parity and same-wave consumers non-negotiable
(`restart/prompts/ORCHESTRATOR.md:197`-`:206`). Lock 1 keeps SIMD masks
transient and rejects retained sidecars (`restart/locks/LOCKS.md:52`), Lock
14 keeps grammar policy out of generic crates (`restart/locks/LOCKS.md:78`),
and Lock 16 requires unit parity plus corpus parity for SIMD primitives
(`restart/locks/LOCKS.md:112`).

The V1 consolidation required the V2 fold to add triad accounting to P2-A,
P2-C, P2-D, and P2-F; to demote LD4/SHA3 speculative ISA entries; and to mark
P2-D diagnostics as ineligible under current S-P1 (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:27`-`:43`).
V2 resolves those CH4 defects.

## Findings

1. P2-A now satisfies CH4 for C1-C7. Its candidate table directly carries
   `Scalar-reference status`, `Checkasm/parity expectation`, and
   `Same-wave consumer note` columns (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:29`-`:37`).
   It also adds a scalar-sketch floor for all seven candidates and requires
   S-P3 to replace sketches with executable scalar references before SIMD or
   native wiring (`restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md:39`-`:49`).

2. P2-B remains CH4-complete. Its common admission process orders scalar
   oracle, differential checkasm, feature/fallback, caller micro-proof, and
   same-wave consumer before admission (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:27`-`:34`).
   Its twelve admission gates carry scalar-ref status, strict parity/checkasm,
   same-wave consumer rules, and admission boundaries (`restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md:36`-`:51`).

3. P2-C now separates current candidates from inventory. It declares six
   current candidates and two non-selectable inventory/support entries, with
   LD4 and SHA3 demoted because current S-P1 lacks an interleaved stream or a
   concrete three-input boolean fold (`restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md:38`-`:42`).
   Each current candidate has scalar-ref status, checkasm expectation, and a
   same-wave consumer: C1 (`:44`-`:53`), C3 (`:67`-`:76`), C4 (`:78`-`:87`),
   C5 (`:89`-`:98`), C6 (`:100`-`:109`), and C7 (`:111`-`:120`). The
   non-candidates I1 and I2 are explicitly non-selectable until fresh profile
   evidence, scalar oracles, parity, and consumers exist (`:55`-`:65`,
   `:122`-`:133`).

4. P2-D no longer overcounts tape diagnostics as selectable candidates. It
   states current selectable candidate count is zero, same-tape diagnostic
   count is three, and the parallel-substrate route is rejected
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:67`-`:72`).
   The table now includes scalar-reference status, checkasm/parity status, and
   same-wave proof/consumer notes for each diagnostic, while marking all three
   diagnostic/ineligible under current S-P1; `structural_class_lane_union` is
   rejected with no legal scalar reference or consumer (`:74`-`:83`).

5. P2-E remains CH4-complete for its five parse-that gaps. Each candidate has
   an executable scalar sketch or status, explicit parity/checkasm
   expectations, and a same-wave consumer note: `pt_byte_set_run_skip`
   (`restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md:60`-`:89`),
   `pt_bounded_plain_string_end` (`:110`-`:147`),
   `pt_digit_run_span_accumulate` (`:175`-`:216`), `pt_hex_quad_decode`
   (`:237`-`:278`), and `pt_escaped_string_segments` (`:307`-`:354`).

6. P2-F now carries the triad at the family level and marks non-parser
   families ineligible. The table includes scalar-ref status, checkasm/parity
   status, same-wave consumer/proof note, P1 antecedent, and eligibility for
   all F1-F8 (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:31`-`:40`).
   F7 is oracle-only and parser-candidate-ineligible; F8 is accounting-only
   and parser-candidate-ineligible (`:39`-`:40`).

7. The bbnf-simd harness evidence supports the CH4 process without waiving
   consumer duties. `CHECKASM-REPORT.md` maps reference-vs-candidate calls,
   source mutation checks, alignment sweeps, stack canaries, signal guards,
   and outlier filtering into the local Rust harness
   (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41`-`:51`), and still says
   no-orphan bodies require real codegen/runtime consumers
   (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:251`-`:254`). The generic
   harness records or promotes random/misaligned divergences under
   `BBNF_SIMD_STRICT=1`, while corpus parity always asserts
   (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`-`:20`,
   `:345`-`:370`). Local parity examples compare scalar and candidate for
   eq-set masks (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:156`-`:180`),
   table classification (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13`-`:49`),
   bitmap helpers (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs:5`-`:37`;
   `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5`-`:29`),
   bulk position emission (`skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:5`-`:60`),
   EOB tail clamping (`skinny/crates/bbnf-simd/tests/checkasm_eob_pad_clamp.rs:5`-`:26`),
   structural terminator classification (`skinny/crates/bbnf-simd/tests/checkasm_structural_terminator_64.rs:7`-`:62`),
   and the current smoke-level x4 Unicode escape proof
   (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58`-`:68`).

8. RESULTS and REDRESS keep the cost interpretation honest. The live result
   surface is still `N-direct / NoGo`, with Track 1 as generated JSON and
   Track 2 as the independent hand-coded tape parser
   (`skinny/RESULTS.md:143`-`:145`). REDRESS 119/120 close SK-V11 direct
   residuals as a measured fixpoint and route SK-V12 to generated non-JSON
   baseline work first (`skinny/REDRESS.md:3495`-`:3553`). The V2 S-P2
   artifacts do not claim row movement from primitive parity alone.

## Disposition

ACCEPT. The V2 S-P2 research corpus is CH4-complete. Current candidates carry
the scalar-reference, checkasm/parity, and same-wave-consumer/proof triad;
entries lacking current S-P1 movement authority are marked diagnostic,
inventory-only, oracle-only, accounting-only, or rejected. S-P3 may reuse the
candidate pool only under the same constraints: executable scalar reference
before native wiring, strict parity/checkasm before production, and a same-wave
generated/runtime/report consumer before any behavior admission.

No source, RESULTS, REDRESS, lock, or sibling CH file change is requested by
this lens.
