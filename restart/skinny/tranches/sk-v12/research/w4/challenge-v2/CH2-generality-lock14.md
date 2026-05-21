# SK-V12 W4 CHALLENGE V2 - CH2 Generality And Lock 14

Verdict: REVISE.

PLAN-V2 is mostly grammar-neutral. It selects the CSS-generated `scan_block`
delimiter member-find caller, keeps delimiter policy CSS-local, consumes the
existing grammar-neutral `byte_class_from_eq_set_64`, and rejects JSON
templates, `parse-that`, generic runtime, x86, directives, BIR,
`BackendShape`, public substrate, and sidecars.

The blocker is owner-path durability under Lock 14. W4 production edits
`crates/codegen/src/...` and `crates/runtime/src/...`, both frozen roots. The
current Lock 14 owner authorization only recognizes SK-V12 W1a and W1b-1
parent diffs, not `sk-v12-waveW4`. PLAN-V3 must either:

- add a W4-specific, narrow Lock 14 owner authorization path, including
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs` in W4 ownership with tests;
  or
- make W4 redress reject-only before frozen-root production edits.

Any W4 Lock 14 authorization must admit only the selected CSS template/runtime
slice and must continue excluding `json_templates`, generic runtime/codegen,
IR, passes, SIMD source, directives, BIR, `BackendShape`, and public substrate.
