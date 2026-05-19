# SK-V10 S-P3 V1 CH2: Generality And Lock 14

Verdict: REVISE.

Acceptance percentage: 84%.

Scope: audit whether the SK-V10 S-P3 SPEC keeps JSON policy out of generic
crates, distinguishes PASS-3 tape/direct identity from the rejected skinny W3
route, preserves Lock 1 and Lock 14, and routes CSS L4 / Sheets / BBNF-self
proof correctly.

## Standard

CH2 requires the S-P3 SPEC to carry a visible Section 2.1 generality and Lock 14
gate. That gate must require every generic-crate edit to carry non-JSON proof
from CSS L4, Sheets, or BBNF-self; a wave that lets JSON policy into a generic
crate fails CH2 (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116-120`).

S-P2 already folded this requirement into the canonical candidate ledger:
generic kernels may expose byte sets, class tables, masks, offsets, digit
accumulators, and policy structs, while JSON quote, slash, `\u`, surrogate,
number, whitespace, output, and row semantics belong in generated per-grammar
templates (`restart/skinny/tranches/sk-v10/research/p2/hardening/V1/CH2-generality-lock14.md:95-110`,
`restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:31-37`).

## Findings

1. REVISE: the SPEC does not contain the required Section 2.1 generality gate.
   It has strong global Lock 14 language in the close condition and
   non-negotiables: generic crate, codegen, or runtime-outside-JSON edits require
   named CSS L4, Sheets, or BBNF-self proof, and JSON quote/slash/`\u`/surrogate/
   number/whitespace/output/row semantics belong in generated per-grammar
   templates (`restart/skinny/tranches/sk-v10/SPEC.md:49-51`,
   `restart/skinny/tranches/sk-v10/SPEC.md:143-145`). However the S-P3 plan
   explicitly calls for a SPEC Section 2.1 generality + Lock 14 gate
   (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:94-99`), while the current
   SPEC jumps from Section 2 Wave Manifest to Section 3 W0
   (`restart/skinny/tranches/sk-v10/SPEC.md:160-195`). This is a structural
   CH2 defect even though the policy intent is mostly present.

2. ACCEPT: the SPEC keeps the rejected W3 implementation route distinct from
   PASS-3 tape/direct runtime identity. P3-E states the distinction directly:
   PASS-3 tape/direct runtime union is the user-surface identity contract, while
   SK-V10 W3 union/event substrate is a retired skinny implementation route
   (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:25-34`).
   The SPEC carries that into W3: W3 is a firewall, audits aliases and
   parse-only SOTA claims, and explicitly keeps PASS-3 tape/direct runtime
   identity distinct from the rejected skinny W3 implementation route
   (`restart/skinny/tranches/sk-v10/SPEC.md:316-337`). Its exit gate refuses live
   dispatch through W3 union/event substrate, class column, streaming cursor,
   `UnionTape`, or W4 cascade-lock (`restart/skinny/tranches/sk-v10/SPEC.md:339-344`).

3. ACCEPT: Lock 1 is preserved at the governing level. The SPEC blocks W3
   union/event substrate, retained class columns, structural cursors,
   `UnionTape`, class-lane-only routes, W4-through-W3 cascade-locks, parser
   sidecar producers, public substrate APIs, and parallel retained tapes
   (`restart/skinny/tranches/sk-v10/SPEC.md:125-127`,
   `restart/skinny/tranches/sk-v10/SPEC.md:141-142`). W3, W9, W10, and the
   global pre-block ledger keep W3, sidecar, scratch, parser-owned projection,
   and parallel substrate routes blocked (`restart/skinny/tranches/sk-v10/SPEC.md:349-350`,
   `restart/skinny/tranches/sk-v10/SPEC.md:600-601`,
   `restart/skinny/tranches/sk-v10/SPEC.md:632-640`,
   `restart/skinny/tranches/sk-v10/SPEC.md:678-683`).

4. REVISE: W7-W9 do not consistently make Lock 14 proof an exit condition even
   though they name generic/codegen owners. W7 can edit `bbnf-simd`,
   `parse-that-regex`, `runtime/src/grammars/json/generated.rs`, and
   `codegen/src/typed_direct.rs` (`restart/skinny/tranches/sk-v10/SPEC.md:484-491`),
   but its exit gate only requires scalar oracle, differential harness,
   microbench, caller identification, and no row movement
   (`restart/skinny/tranches/sk-v10/SPEC.md:504-509`). W8 has generic owners and
   does say JSON slash/`\u`/surrogate policy stays in generated templates, but
   the exit gate only says "No generic JSON policy leaks" instead of requiring a
   named non-JSON proof or a no-generic-behavior-changed proof
   (`restart/skinny/tranches/sk-v10/SPEC.md:523-549`). W9 inherits the W7/W8
   primitive owner paths and wires an existing JSON direct or typed caller, but
   its exit gate has no Lock 14 proof predicate
   (`restart/skinny/tranches/sk-v10/SPEC.md:565-595`). This is weaker than P3-B,
   which requires grammar-neutral proof plus named CSS L4 / Sheets / BBNF-self
   evidence for generic, codegen, or runtime-outside-JSON edits
   (`restart/skinny/tranches/sk-v10/research/p3/p3b-wave-sequencing.md:31-33`).

5. ACCEPT with one caveat: W5 root-type typed generalization is correctly routed
   as proof-only and grammar-neutral. The tasks require `Vec<T>` and map-entry
   roots without JSON policy in generic code; the exit gate requires the root
   model to avoid hard-coded JSON policy, preserve checksum parity, and either
   name CSS L4 / Sheets / BBNF-self impact or prove no generic behavior changed
   (`restart/skinny/tranches/sk-v10/SPEC.md:413-428`). This is the model the
   missing Section 2.1 gate should generalize to all generic/codegen/runtime-
   outside-JSON waves.

6. ACCEPT: CSS L4 / Sheets / BBNF-self and Omega routing are present at close.
   The close gate sends the substrate-ceiling lock amendment route to Omega and
   sends CSS L4, Sheets, and BBNF-self generalization risk to Totality
   (`restart/skinny/tranches/sk-v10/SPEC.md:659-667`). This matches the SK-V10
   synthesis route that sends W3 substrate-ceiling falsification to Pass Omega
   and non-JSON generalization risk to Totality
   (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:162-170`).

7. REVISE: the DISPATCH prompt weakens P3-F's first-of-class/generic edit
   challenge trigger. P3-F says CHALLENGE is mandatory for any first-of-class
   source or generic-crate edit (`restart/skinny/tranches/sk-v10/research/p3/p3f-spec-draft.md:281-284`).
   The integrated DISPATCH prompt makes CHALLENGE mandatory for named waves and
   optional for W0, W2, W3, and Close unless gate semantics change
   (`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:89-93`), but it does not
   restate the "any first-of-class source or generic-crate edit" trigger. This
   creates an avoidable dispatch ambiguity for Lock 14.

## Required Fixes

1. Add `## 2.1 Generality And Lock 14 Gate` to the SPEC before Section 3. It
   should require every wave touching a generic crate, codegen, or runtime outside
   JSON to declare: owner path class, whether generic behavior changes, the
   grammar-neutral abstraction exposed, named CSS L4 / Sheets / BBNF-self proof
   or a no-generic-behavior-changed proof, generated per-grammar confinement for
   JSON policy, and the REDRESS/revert action on proof failure.

2. Amend W7, W8, and W9 exit gates to require the Section 2.1 proof whenever
   `bbnf-simd`, `parse-that-regex`, `codegen`, or runtime outside the JSON
   generated template is edited. W8's "No generic JSON policy leaks" should be
   upgraded to the named proof requirement.

3. Amend DISPATCH Phase 2.5 so CHALLENGE is mandatory for any first-of-class
   source edit or any generic-crate/codegen/runtime-outside-JSON edit, matching
   P3-F.

4. Preserve the existing W3 firewall language and close routing. Those portions
   already satisfy CH2 and should not be loosened while fixing the Section 2.1
   gap.

## Result

REVISE. The SPEC is directionally strong and does not appear to intentionally
admit JSON policy into generic crates. It nevertheless misses the required
Section 2.1 gate and leaves W7-W9 with weaker per-wave Lock 14 exits than the
P2/P3 authorities require. This is fixable without changing the SK-V10 topology.
