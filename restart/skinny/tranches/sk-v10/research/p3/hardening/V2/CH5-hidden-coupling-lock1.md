# SK-V10 S-P3 V2 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 96%

Scope audited: V2 fold coverage for the V1 CH5 hidden-coupling blockers:
generated artifact ownership, Track 2 forbidden-dependency proof, W5 Lock 14
generic/codegen loophole, P3-E stale wave aliases, and W3 alias blocking.

## Findings

### 1. Generated artifacts are now read-only unless generator/schema plus regen are owned

Status: fixed.

The V2 SPEC adds a global generated-boundary rule: generated files are read-only
evidence unless the same wave owns the generator or schema input and the
regeneration command, and generated output may be committed only as regenerated
output, not as a hand patch (`restart/skinny/tranches/sk-v10/SPEC.md:219`-`221`).
Dispatch repeats the same load-bearing fact for implementation agents
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:169`-`171`).

The specific V1 CH5 W7/W8 drift route is also closed locally. W7 marks
`skinny/crates/runtime/src/grammars/json/generated.rs` and
`skinny/crates/bbnf-bench/src/generated_real_typed.rs` as read-only evidence
unless the same wave owns generator input and regeneration
(`restart/skinny/tranches/sk-v10/SPEC.md:537`-`541`), and its tasks repeat the
read-only call-site rule (`restart/skinny/tranches/sk-v10/SPEC.md:553`-`555`).
W8 carries the same treatment for its generated JSON and typed artifacts
(`restart/skinny/tranches/sk-v10/SPEC.md:585`-`589`) and repeats the regeneration
condition in tasks (`restart/skinny/tranches/sk-v10/SPEC.md:603`-`605`).

Required fix: none.

### 2. Track 2 forbidden-dependency proof is folded into Section 2.2 and row gates

Status: fixed.

Section 2.2 now states the forbidden dependency list directly: Track 2 may not
call generated Track 1, generated SinkOnly helpers, generated typed helpers, or
benchmark-private shared parser code (`restart/skinny/tranches/sk-v10/SPEC.md:225`-`229`).
If `gate-json` cannot mechanically prove that boundary, the wave must carry an
audit artifact naming the Track 2 source path, checked generated/helper paths,
and absent forbidden dependencies (`restart/skinny/tranches/sk-v10/SPEC.md:229`-`232`).
Dispatch repeats the same rule in load-bearing facts
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:165`-`168`).

The rule is also attached to row-moving gates. W1 requires Section 2.2
independence and the fallback audit artifact if mechanical proof is unavailable
(`restart/skinny/tranches/sk-v10/SPEC.md:304`-`307`). W2, W4, W6, W9, and W10
all require Track 2/oracle independence through Section 2.2 for moved rows
(`restart/skinny/tranches/sk-v10/SPEC.md:349`,
`restart/skinny/tranches/sk-v10/SPEC.md:430`,
`restart/skinny/tranches/sk-v10/SPEC.md:518`,
`restart/skinny/tranches/sk-v10/SPEC.md:666`,
`restart/skinny/tranches/sk-v10/SPEC.md:705`).

Required fix: none.

### 3. W5 Lock 14 no-behavior loophole is narrowed to actual diff content

Status: fixed.

The global Lock 14 gate now allows "no generic behavior changed" only when the
diff shows no generic, codegen, or runtime-outside-JSON behavior edit
(`restart/skinny/tranches/sk-v10/SPEC.md:215`-`218`). W5 repeats this exact
constraint against the root-model/codegen paths: edits to
`skinny/crates/codegen/src/direct_schema.rs`,
`skinny/crates/codegen/src/typed_direct.rs`, or other generic/codegen/runtime
behavior must pass Section 2.1 with a named CSS L4, Sheets, or BBNF-self proof
(`restart/skinny/tranches/sk-v10/SPEC.md:471`-`473`). Its no-generic-behavior
claim is valid only when the diff contains no such behavior edit
(`restart/skinny/tranches/sk-v10/SPEC.md:474`-`475`).

That closes the V1 escape hatch where W5 could own codegen root-model files and
then assert no generic behavior changed without tying the assertion to the
actual diff.

Required fix: none.

### 4. P3-E stale aliases are resolved against the final SPEC manifest

Status: fixed.

The V2 P3-E ledger now states that final dispatch numbering is the top-level
SPEC W0-W10 manifest, that scoped sections are aligned to final SPEC wave
numbers and candidate families, and that older compressed draft aliases are not
dispatch identifiers (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:64`-`67`).
The ledger sections now align W3 to the firewall
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:126`),
W4 to `instruments` typed product admission
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:144`),
and W5/W6 to root-type typed generalization and row admission
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:166`).

The only residual issue is a stale phrase, "V1 CHALLENGE fold," in the alignment
note (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:64`).
It does not reopen stale aliases because the following sentence binds final
SPEC W0-W10 numbering and rejects older aliases, but it should be cleaned up in
the next documentation pass.

Required fix: none for ACCEPT. Optional cleanup: rename "V1 CHALLENGE fold" to
"V2 CHALLENGE fold" in P3-E.

### 5. W3 aliases are blocked, including renamed substrate and W4-through-W3 routes

Status: fixed.

The V2 non-negotiables ban W3 union/event substrate, retained class column,
structural cursor, `UnionTape`, class-lane-only route, W4-through-W3
cascade-lock, and renamed equivalents (`restart/skinny/tranches/sk-v10/SPEC.md:123`-`127`).
The W3 firewall requires an audit for W3 aliases, parse-only SOTA claims, and
W4-through-W3 dependencies (`restart/skinny/tranches/sk-v10/SPEC.md:376`-`379`),
and its exit gate requires `rg` plus plan audit proof that no live dispatch route
uses W3 union/event substrate, class column, streaming cursor, `UnionTape`, or
W4 cascade-lock (`restart/skinny/tranches/sk-v10/SPEC.md:383`-`388`).

Dispatch also fails renamed W3 reopening at the top of the implementation
contract: no agent may reopen REDRESS 96-98 through a renamed W3, structural
cursor, `UnionTape`, retained class column, sidecar producer, or W4 cascade-lock
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:12`-`16`). P3-E binds the
same global pre-blocks and says W3 cannot be the consumer or entry gate for W4
cascade work (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:51`-`52`).

Required fix: none.

## Required Fixes

None required for ACCEPT.

Optional cleanup only: update the stale "V1 CHALLENGE fold" phrase in
`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:64` to
"V2 CHALLENGE fold" so the label matches the current audit cycle.
