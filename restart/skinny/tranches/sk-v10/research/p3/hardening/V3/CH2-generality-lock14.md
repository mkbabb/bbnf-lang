# SK-V10 S-P3 V3 CH2 Generality / Lock 14

Verdict: ACCEPT
Acceptance: 97%

Scope: V3 confirmation for CH2 generality/Lock 14 over the hygiene-folded S-P3
contract. This checks for regression from V2 acceptance in SPEC Section 2.1,
W5/W7/W8/W9 exits, DISPATCH CHALLENGE triggers, generated-boundary language,
and support artifacts.

## Findings

1. ACCEPT: No regression from V2 CH2 acceptance. V2 CH2 accepted the folded
   contract at 96% with no required fixes
   (`restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH2-generality-lock14.md:3`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH2-generality-lock14.md:4`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/V2/CH2-generality-lock14.md:95`).
   The V2 consolidation records the same CH2 ACCEPT/96% outcome and requires V3
   as the consecutive confirmation cycle, not as a reopened defect cycle
   (`restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:13`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:16`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:37`,
   `restart/skinny/tranches/sk-v10/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:41`).

2. ACCEPT: SPEC Section 2.1 still provides the Lock 14 generality gate. Every
   wave touching a generic crate, codegen, `bbnf-simd`, `parse-that-regex`, or
   runtime outside the generated JSON template must pass the gate before redress
   can close (`restart/skinny/tranches/sk-v10/SPEC.md:204`,
   `restart/skinny/tranches/sk-v10/SPEC.md:206`). The plan must name owner path
   class and whether generic behavior changes
   (`restart/skinny/tranches/sk-v10/SPEC.md:210`), JSON quote/slash/`\u`/
   surrogate/number/whitespace/output/row semantics remain generated-template
   owned (`restart/skinny/tranches/sk-v10/SPEC.md:211`,
   `restart/skinny/tranches/sk-v10/SPEC.md:214`), and any
   generic/codegen/runtime-outside-JSON behavior edit requires a named CSS L4,
   Sheets, or BBNF-self proof (`restart/skinny/tranches/sk-v10/SPEC.md:215`).
   The no-behavior escape remains narrowed to diffs with no such behavior edit
   (`restart/skinny/tranches/sk-v10/SPEC.md:216`).

3. ACCEPT: Generated-boundary language remains explicit and enforceable. SPEC
   Section 2.1 says generated files are read-only evidence unless the same wave
   owns generator or schema input plus the regeneration command, and generated
   output may be committed only as regenerated output
   (`restart/skinny/tranches/sk-v10/SPEC.md:219`,
   `restart/skinny/tranches/sk-v10/SPEC.md:221`). DISPATCH repeats the same
   load-bearing fact and forbids hand-patched generated output
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:171`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:173`). Proof failure
   reverts the generic/codegen/runtime edit and records the missing non-JSON
   proof or generated-boundary violation
   (`restart/skinny/tranches/sk-v10/SPEC.md:222`,
   `restart/skinny/tranches/sk-v10/SPEC.md:223`).

4. ACCEPT: W5 has not regressed. The manifest keeps W5 proof-only unless W6
   consumes the proof (`restart/skinny/tranches/sk-v10/SPEC.md:169`,
   `restart/skinny/tranches/sk-v10/SPEC.md:184`). Its exit requires array and
   map-entry roots without hard-coded JSON policy in generic crates
   (`restart/skinny/tranches/sk-v10/SPEC.md:467`) and sends edits to
   `direct_schema.rs`, `typed_direct.rs`, or other generic/codegen/runtime
   behavior through Section 2.1 with named CSS L4, Sheets, or BBNF-self proof
   (`restart/skinny/tranches/sk-v10/SPEC.md:471`,
   `restart/skinny/tranches/sk-v10/SPEC.md:473`). The no-generic-behavior proof
   remains valid only when the diff contains no generic/codegen/runtime-outside-
   JSON behavior edit (`restart/skinny/tranches/sk-v10/SPEC.md:474`).

5. ACCEPT: W7 and W8 remain proof-only micro waves and preserve the generated
   boundary. The manifest states that W7/W8 are deliberately proof-only and that
   production caller wiring lands only in W9
   (`restart/skinny/tranches/sk-v10/SPEC.md:186`,
   `restart/skinny/tranches/sk-v10/SPEC.md:187`). W7 treats generated artifacts
   as read-only evidence unless generator/schema input and regeneration are owned
   (`restart/skinny/tranches/sk-v10/SPEC.md:537`,
   `restart/skinny/tranches/sk-v10/SPEC.md:553`) and exits only if any
   generic/codegen/runtime-outside-JSON behavior edit passes Section 2.1
   (`restart/skinny/tranches/sk-v10/SPEC.md:564`). W8 carries the same
   read-only generated evidence rule (`restart/skinny/tranches/sk-v10/SPEC.md:585`,
   `restart/skinny/tranches/sk-v10/SPEC.md:603`) and requires Section 2.1 while
   keeping JSON policy in generated templates
   (`restart/skinny/tranches/sk-v10/SPEC.md:614`,
   `restart/skinny/tranches/sk-v10/SPEC.md:615`).

6. ACCEPT: W9 remains constrained to one exact accepted W7/W8 proof and does not
   broaden Lock 14 authorization. The manifest says W9 consumes exactly one
   relevant accepted W7 or W8 proof for the exact primitive and caller, while
   `C8` and `C9` cannot feed W9 without future SPEC/CHALLENGE amendment
   (`restart/skinny/tranches/sk-v10/SPEC.md:188`,
   `restart/skinny/tranches/sk-v10/SPEC.md:191`). W9 candidates are only a
   relevant accepted W7/W8 `C4`-`C7` primitive
   (`restart/skinny/tranches/sk-v10/SPEC.md:629`), entry requires the exact
   primitive and caller (`restart/skinny/tranches/sk-v10/SPEC.md:643`), tasks
   limit the dispatch to one primitive, one caller, one consumer plane, and one
   row-moving target set (`restart/skinny/tranches/sk-v10/SPEC.md:654`), and the
   exit keeps Section 2.1 binding on generic/codegen/runtime-outside-JSON edits
   (`restart/skinny/tranches/sk-v10/SPEC.md:663`).

7. ACCEPT: DISPATCH CHALLENGE triggers are still strong enough for CH2. W5, W7,
   W8, and W9 are listed with CHALLENGE-dependent dispatch status where relevant
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:51`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:55`). Phase 2.5 makes
   CHALLENGE mandatory for W5/W7/W8/W9 and for any first-of-class source edit or
   generic-crate, codegen, `bbnf-simd`, `parse-that-regex`, or runtime-outside-
   JSON edit (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:90`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:94`). CHALLENGE must
   reject W7/W8 plans missing scalar oracle, differential/checkasm parity,
   identified existing caller, or threshold-bearing caller microbench, and must
   reject W9 plans missing same-commit production consumer wiring
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:107`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:112`). DISPATCH also
   repeats the Section 2.1 named-proof requirement
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:174`,
   `restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:175`).

8. ACCEPT: Support artifacts remain aligned and do not loosen the live
   SPEC/DISPATCH contract. P3-B keeps W5 proof-only, W7/W8 proof-only, and W9
   limited to the relevant accepted proof for the exact primitive and caller
   (`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:61`,
   `restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:65`);
   its refusal conditions block generic crates or runtime outside JSON without
   Lock 14 non-JSON proof
   (`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:115`).
   P3-E is negative authority that later SPEC/DISPATCH may tighten but not
   loosen without fresh CHALLENGE
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:21`,
   `restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:23`),
   and its material-differential checklist asks whether generic/codegen/runtime-
   outside-JSON edits need CSS L4, Sheets, or BBNF-self proof
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:280`).
   P3-F states Lock 14 and the named non-JSON proof requirement
   (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:76`,
   `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:79`) and makes
   CHALLENGE mandatory for W7-W9 primitive/kernel proof or production and
   generic-crate edits
   (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:283`,
   `restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:285`).

## Required Fixes

None.

Residual note: P3-E's checklist uses "whether" for non-JSON proof need, while
SPEC Section 2.1 now directly requires named non-JSON proof for behavior edits
and permits "no generic behavior changed" only when there is no generic/codegen/
runtime-outside-JSON behavior edit. This is not a V3 regression because P3-E is
negative authority and explicitly allows later SPEC/DISPATCH tightening without
loosening.
