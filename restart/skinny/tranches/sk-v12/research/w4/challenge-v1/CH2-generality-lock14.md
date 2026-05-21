# SK-V12 W4 CHALLENGE V1 - CH2 Generality And Lock 14

Verdict: REVISE.

The plan is directionally legal and does not add JSON policy leakage, a
directive, BIR variant, `BackendShape`, or public substrate API. It still has
concrete blockers before CH2 can accept it.

## Findings

1. The selected consumer contract is internally inconsistent. PLAN-V1 targets
   `scan_block` delimiter search and defines a scalar "find set member" loop.
   A5's Lock 16 protocol instead selects `skip_ws_and_comments` / layout
   run-skip and defines the opposite scalar contract, "skip set members." Pick
   one exact generated consumer and make the scalar reference, checkasm,
   microbench, and production wiring match it.

2. The SIMD consumer is not yet benchmark-proven in a gate-consumed way.
   PLAN-V1 names a general `nonjson_css_l4` Criterion run, but A5 requires a
   named scalar-vs-candidate generated-caller microbench with checksum,
   synthetic windows, sample count, and a threshold. PLAN-V2 must add the exact
   W4 benchmark lane and gate/report consumer.

3. Generated CSS template/runtime consistency is under-specified. Editing both
   `css_l4_declaration_values_templates/` and generated runtime output is
   legal, but PLAN-V2 must require the codegen reproducibility test and runtime
   CSS fact-stream test before redress can pass.

4. Lock 16 verification is too thin. Extending only
   `checkasm_byte_class_from_eq_set_64.rs` is not enough for a generated
   caller API. PLAN-V2 must require a dedicated caller-level checkasm/parity
   cell or scoped equivalent for the selected API, plus reruns of the W2 /
   byte-class parity gates.

The owner-path correction is legal, but after the SPEC edit it should be stated
as "do not select `json_templates/`" rather than "amend owner list."
