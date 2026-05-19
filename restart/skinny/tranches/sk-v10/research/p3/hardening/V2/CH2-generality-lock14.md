# SK-V10 S-P3 V2 CH2 Generality / Lock 14

Verdict: ACCEPT
Acceptance: 96%

Scope: V2 folded contract audit for SPEC Section 2.1, W5/W7/W8/W9 exits,
DISPATCH CHALLENGE trigger, P3-B/P3-E/P3-F alignment, and V1 hardening
consolidation. Focus question: whether every generic/codegen/runtime-outside-JSON
edit requires named non-JSON proof or a valid no-behavior-edit proof.

## Findings

1. ACCEPT: SPEC Section 2.1 now supplies the missing generality/Lock 14 gate.
   It binds every wave that touches a generic crate, codegen, `bbnf-simd`,
   `parse-that-regex`, or runtime outside the generated JSON template
   (`restart/skinny/tranches/sk-v10/SPEC.md:204`). The gate requires the plan to
   name owner path class and whether generic behavior changes
   (`restart/skinny/tranches/sk-v10/SPEC.md:210`), keeps JSON quote, slash,
   `\u`, surrogate, number, whitespace, output, and row semantics in generated
   per-grammar templates (`restart/skinny/tranches/sk-v10/SPEC.md:211`), and
   requires a named CSS L4, Sheets, or BBNF-self proof for any
   generic/codegen/runtime-outside-JSON behavior edit
   (`restart/skinny/tranches/sk-v10/SPEC.md:215`). The no-behavior-edit escape is
   valid only when the diff shows no generic, codegen, or runtime-outside-JSON
   behavior edit, such as docs or fixture-only proof work
   (`restart/skinny/tranches/sk-v10/SPEC.md:216`). This directly closes the V1
   CH2 gap.

2. ACCEPT: W5 now hardens root-type typed generalization against JSON-policy
   leakage. W5 requires root models to represent array and map-entry roots
   without hard-coded JSON policy in generic crates
   (`restart/skinny/tranches/sk-v10/SPEC.md:467`) and explicitly routes edits to
   `direct_schema.rs`, `typed_direct.rs`, or other generic/codegen/runtime
   behavior through Section 2.1 with named CSS L4, Sheets, or BBNF-self proof
   (`restart/skinny/tranches/sk-v10/SPEC.md:471`). It also restates that
   no-generic-behavior-changed proof is valid only when the diff contains no
   generic/codegen/runtime-outside-JSON behavior edit
   (`restart/skinny/tranches/sk-v10/SPEC.md:474`). This aligns with the V1
   consolidated fold requiring W5 named non-JSON proof and limiting the
   no-behavior claim (`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V1-CONSOLIDATED.md:36`).

3. ACCEPT: W7 and W8 now inherit the Section 2.1 gate and preserve generated JSON
   ownership. W7 treats generated artifacts as read-only evidence unless the wave
   owns generator/schema input and regeneration
   (`restart/skinny/tranches/sk-v10/SPEC.md:537`,
   `restart/skinny/tranches/sk-v10/SPEC.md:553`) and exits only if any
   generic/codegen/runtime-outside-JSON behavior edit passes Section 2.1
   (`restart/skinny/tranches/sk-v10/SPEC.md:564`). W8 carries the same generated
   evidence restriction (`restart/skinny/tranches/sk-v10/SPEC.md:585`,
   `restart/skinny/tranches/sk-v10/SPEC.md:603`) and its exit requires Section
   2.1 while preserving JSON policy in generated templates
   (`restart/skinny/tranches/sk-v10/SPEC.md:614`). Because Section 2.1 itself
   contains the named-proof versus valid-no-behavior-proof distinction, W7/W8 do
   not need to duplicate the wording.

4. ACCEPT: W9 production is narrowed to the exact W7/W8 proof and carries the
   same Lock 14 behavior-edit gate. The wave can consume only a relevant accepted
   W7 or W8 `C4`-`C7` primitive (`restart/skinny/tranches/sk-v10/SPEC.md:629`)
   for the exact primitive and caller (`restart/skinny/tranches/sk-v10/SPEC.md:643`).
   Its tasks limit dispatch to one proven primitive, one existing production
   caller, one consumer plane, and one row-moving target set
   (`restart/skinny/tranches/sk-v10/SPEC.md:654`), and its exit requires any
   generic/codegen/runtime-outside-JSON behavior edit to pass Section 2.1
   (`restart/skinny/tranches/sk-v10/SPEC.md:663`). This prevents W9 from using a
   micro-proof as a broad generic kernel authorization.

5. ACCEPT: DISPATCH now triggers CHALLENGE for the relevant generality risk
   classes. Phase 2.5 makes CHALLENGE mandatory for W5, W7, W8, and W9, and also
   for any first-of-class source edit or generic-crate, codegen, `bbnf-simd`,
   `parse-that-regex`, or runtime-outside-JSON edit
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:90`). The load-bearing
   facts repeat that generated artifacts are read-only evidence unless generator
   ownership and regeneration are present (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:169`)
   and that generic/codegen/runtime-outside-JSON behavior edits must pass SPEC
   Section 2.1 with named CSS L4, Sheets, or BBNF-self proof
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:172`). This is sufficient
   because Section 2.1 defines the valid no-behavior-edit proof.

6. ACCEPT: P3-B, P3-E, and P3-F now align with the folded V2 posture. P3-B
   refuses generic crates or runtime outside JSON without Lock 14 non-JSON proof
   (`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:115`) and
   limits W9 to the relevant accepted W7/W8 proof for exact `C4`-`C7` primitive
   and caller (`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:65`).
   P3-E remains negative authority and permits later SPEC/DISPATCH tightening
   without loosening (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:21`);
   its checklist requires each route to state whether generic/codegen/runtime-
   outside-JSON edits need CSS L4, Sheets, or BBNF-self proof
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:280`).
   P3-F already specified that generic-crate, codegen, or runtime-outside-JSON
   edits require named non-JSON proof
   (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:76`) and that
   CHALLENGE is mandatory for W7-W9 primitive/kernel proof or production and
   generic-crate edits (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:283`).

## Required Fixes

None.

Residual risk: P3-E's checklist asks whether a generic/codegen/runtime-outside-
JSON edit needs non-JSON proof, while SPEC Section 2.1 now requires that proof
for behavior edits and permits a no-behavior proof only when the diff contains no
such behavior edit. This is acceptable because P3-E is a refusal ledger and
explicitly allows later SPEC/DISPATCH tightening without loosening.
