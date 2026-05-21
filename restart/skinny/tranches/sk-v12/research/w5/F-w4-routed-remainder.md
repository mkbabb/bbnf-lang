# SK-V12 W5 Research F: W4 Routed Remainder

## Verdict

W4 is closed route evidence, not an ADMIT blocker. It satisfies USER PIN D5 by
ending with zero orphan inventory and satisfies the ASM-gen attempt requirement
for the closing tranche record. Because SK-V12 closes by ADMIT, it does not
need a FIXPOINT clause with a production SIMD admission.

## Evidence

- REDRESS-126 records W4 as `ROUTE-PRODUCTION-SPLIT`, not CSS ADMIT and not
  production SIMD/ASM admission.
- The caller-level checkasm/parity test for `find_ascii_set_member64(bytes,
  cursor, end, b"{};")` passed with the scalar byte-walk reference.
- The microbench artifact records parity `pass`, scalar
  `18.510497846 ns/iter`, candidate `3.923145814 ns/iter`, speedup ratio
  `4.718279341`, threshold `1.01`, and decision `pass`.
- The accepted PLAN-V4 default branch required W4 to halt after a passing
  pre-production microbench and route production wiring separately.
- `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md` records
  final `orphan_count=0` and demotes all five orphan rows with evidence:
  `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`,
  `byte_context`, and `cache_hints`.
- The default-branch no-touch proof for JSON, report, gate, Lock 14, and
  `RESULTS.md` roots printed no paths.

## Routed Remainder

After campaign close, the optional production split is a new wave/tranche
candidate: CSS `scan_block` wiring for `find_ascii_set_member64`, a W4-current
report/gate, Lock 14 authorization, fresh Criterion/equality artifacts, and a
W2 prerequisite rerun if the production path depends on the escape-mask proof.
It is not a retroactive blocker for W5.
