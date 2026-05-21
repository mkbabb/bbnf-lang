# SK-V12 W4 CHALLENGE V1 - CH1 Correctness

Verdict: REVISE.

The route can be made semantically correct, but PLAN-V1 is not clean enough to
redress.

## Findings

1. Scalar contract is ambiguous. PLAN-V1 selects `a64_ascii_set_run_skip` but
   defines a delimiter finder over `{`, `;`, and `}`. That finder is correct
   for `scan_block`, but it conflicts with A5's layout run-skip scalar, which
   advances while bytes are members of the CSS layout set. PLAN-V2 must pick
   one exact API. If the selected route is the CSS block scanner, name it
   `find_ascii_set_member` / delimiter scan, not layout `run_skip`.

2. W4 cannot admit on the retained W1b-2b report. The existing
   `SkV12CssL4SotaReport` validator is hard-bound to W1b-2b, REDRESS-125, and
   `lock16_status=n/a:no_simd_or_asm_claim`. A SIMD-backed W4 production route
   needs W4-owned report/gate consumption for selected primitive, scalar
   reference, checkasm/caller parity, microbench evidence, Lock 16 status, and
   W4 REDRESS id.

3. Caller parity is under-specified. The existing `byte_class_from_eq_set_64`
   scalar and NEON bodies are acceptable anchors, but W4 also needs
   caller-level parity for cursor/end/tail behavior around the generated CSS
   delimiter finder.

## Correctness Read

`scan_block` is a valid narrow consumer if PLAN-V2 binds the selected route to
that caller. The current generated CSS scanner treats every `{`, `;`, and `}`
as structural in `scan_block`; replacing the non-member byte increment with a
jump to the next delimiter is semantically equivalent when caller parity proves
cursor and tail behavior.

W2 is satisfied as the `escape_mask_64` prerequisite via REDRESS-122, but W4
must rerun the relevant SIMD/checkasm gates for its own admission.

Required before redress: bind the exact caller contract, add caller-level
scalar/SIMD parity, and add a W4-specific report/gate consumer.
