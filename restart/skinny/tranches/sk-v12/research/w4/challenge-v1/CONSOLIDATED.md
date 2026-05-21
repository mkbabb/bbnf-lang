# SK-V12 W4 CHALLENGE V1 - Consolidated Disposition

Verdict: REVISE.

All six lenses rejected PLAN-V1 for redress readiness. The selected direction
can still become admissible, but only after PLAN-V2 resolves the common
blocking issues below.

## Required PLAN-V2 Changes

1. Bind one exact generated caller contract. PLAN-V1 mixes delimiter
   member-find in `scan_block` with A5's layout run-skip framing. PLAN-V2 must
   choose one API, name it consistently, and align scalar reference, checkasm,
   microbench, production wiring, and REDRESS language to that API.

2. Add W4-current report/gate consumption. W4 cannot admit on the W1b-2b
   REDRESS-125 report because that validator is hard-bound to W1b-2b and
   `lock16_status=n/a:no_simd_or_asm_claim`. W4 needs a W4-specific report or
   report mode that consumes selected primitive, scalar reference,
   checkasm/caller parity, microbench evidence, same-wave consumer, Lock 16
   status, post-W4 CSS Criterion numbers, JSON guard state, and W4 REDRESS id.

3. Add caller-level parity. Primitive `byte_class_from_eq_set_64` checkasm is
   necessary but not sufficient. The selected generated caller must prove
   cursor/end/tail behavior against a scalar reference.

4. Make micro-prove-first gate-consumed. PLAN-V2 must name the exact
   scalar-vs-candidate generated-caller microbench lane, retained artifact, and
   threshold. If the microbench misses, redress stops at `MEASURED-REJECT` and
   does not wire production.

5. Harden generated CSS reproducibility commands. PLAN-V2 must require the
   codegen CSS reproducibility test and runtime CSS fact-stream test whenever
   generated template/runtime output moves.

6. Make orphan zero evidence-backed. PLAN-V2 must make
   `restart/skinny/tranches/sk-v12/research/w4/orphan-disposition.md` a hard
   output with A4's per-orphan evidence fields and final `orphan_count=0`.

7. State W3/W5 coupling. W3 is not required on the current ADMIT path because
   REDRESS-125 already has a CSS ADMIT candidate, but W3 remains mandatory for
   FIXPOINT. W4 redress must not run concurrently with W3 shared-file edits.

No CHALLENGE lens found a blocker in grammar neutrality, JSON policy leakage,
directive/BIR/`BackendShape` expansion, public substrate expansion, or the
truthfulness of the five candidate orphan dispositions.
