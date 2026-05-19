# SK-V10 S-P3 V3 CH5 - Hidden Coupling / Lock 1

Verdict: ACCEPT

Acceptance percentage: 97%

Scope audited: V3 confirmation over the V2-accepted CH5 hidden-coupling surface:
generated artifact ownership, Track 2 forbidden-dependency proof, W5 Lock 14
no-behavior constraint, P3-E wave aliases, W3 alias blocking, and the V2
hygiene fold.

## Findings

### 1. Generated files remain read-only unless generator/schema plus regen are owned

Status: confirmed; no regression from V2.

The global SPEC rule still treats generated files as read-only evidence unless
the same wave owns the generator or schema input and the regeneration command,
and it still permits committing generated output only as regenerated output, not
as a hand patch (`restart/skinny/tranches/sk-v10/SPEC.md:219`-`221`). Dispatch
keeps the same rule in the implementation-agent load-bearing facts
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:171`-`173`).

The concrete W7/W8 generated paths remain locally guarded. W7 marks
`skinny/crates/runtime/src/grammars/json/generated.rs` and
`skinny/crates/bbnf-bench/src/generated_real_typed.rs` as read-only evidence
unless generator input and regeneration are owned
(`restart/skinny/tranches/sk-v10/SPEC.md:537`-`541`) and repeats the same
task-level rule (`restart/skinny/tranches/sk-v10/SPEC.md:553`-`555`). W8 keeps
the matching generated-artifact boundary for its JSON and typed generated files
(`restart/skinny/tranches/sk-v10/SPEC.md:585`-`589`) and repeats the
generator/schema plus regeneration condition (`restart/skinny/tranches/sk-v10/SPEC.md:603`-`605`).

Required fix: none.

### 2. Track 2 forbidden-dependency proof remains explicit and enforceable

Status: confirmed; no regression from V2.

Section 2.2 still states the forbidden dependency list directly: Track 2 may not
call generated Track 1, generated SinkOnly helpers, generated typed helpers, or
benchmark-private shared parser code (`restart/skinny/tranches/sk-v10/SPEC.md:225`-`232`).
If mechanical proof is unavailable, the wave plan must carry an audit artifact
naming the Track 2 source path, checked generated/helper paths, and the absent
forbidden dependencies (`restart/skinny/tranches/sk-v10/SPEC.md:229`-`232`).
Dispatch repeats that Track 2 boundary and fallback audit requirement
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:167`-`170`).

The underlying P3-C gate remains aligned: row-moving waves require generated
Track 1 and independent Track 2/oracle under the same run id, and Track 2 may
not call generated Track 1, generated SinkOnly helpers, generated typed helpers,
or benchmark-private shared parser code
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:33`-`41`).

Required fix: none.

### 3. W5 Lock 14 no-behavior constraint is still tied to actual diff content

Status: confirmed; no regression from V2.

The global Lock 14 gate still allows a "no generic behavior changed" claim only
when the diff shows no generic, codegen, or runtime-outside-JSON behavior edit,
such as docs or fixture-only proof work
(`restart/skinny/tranches/sk-v10/SPEC.md:204`-`218`). That prevents a codegen or
generic-runtime behavior edit from bypassing the CSS L4, Sheets, or BBNF-self
proof requirement by assertion.

P3-E keeps the same boundary for root-type work: root arrays and map-entry roots
must be represented by the typed schema model, JSON-specific root policy may not
leak into generic codegen or runtime, and valid root proof must preserve
full-fixture generated/serde/sonic checksum parity
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:166`-`187`).

Required fix: none.

### 4. P3-E wave aliases remain resolved to the final SPEC manifest

Status: confirmed; V2 hygiene cleanup landed.

The V2 CH5 residual was only a stale "V1 CHALLENGE fold" label. That label has
been corrected: P3-E now says "V2 CHALLENGE fold" and binds final dispatch
numbering to the top-level SPEC W0-W10 manifest
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:64`-`67`).
P3-C carries the same alignment rule and says earlier compressed aliases are not
dispatch identifiers
(`restart/skinny/tranches/sk-v10/research/p3/p3c-falsifiability-gates.md:128`-`147`).

The final SPEC manifest still defines W3 as the W3/parse-only firewall, W4 as
`instruments` typed product admission, W5 as root-type typed generalization
proof, W6 as root typed row admission, W7/W8 as proof-only primitive waves, and
W9 as existing-call-site kernel production
(`restart/skinny/tranches/sk-v10/SPEC.md:160`-`175`). P3-E's scoped sections now
match those meanings: W3 is firewall-only (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:126`-`142`),
W4 is `instruments` typed admission (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:144`-`164`),
and W5/W6 are root-type proof and row admission (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:166`-`187`).

Required fix: none.

### 5. W3 alias blocking remains closed, including W4-through-W3 routes

Status: confirmed; no regression from V2.

The top-level non-negotiables still ban W3 union/event substrate, retained class
column, structural cursor, `UnionTape`, class-lane-only route, W4-through-W3
cascade-lock, and renamed equivalents (`restart/skinny/tranches/sk-v10/SPEC.md:123`-`127`).
The W3 firewall still requires an audit for W3 aliases, parse-only SOTA claims,
and W4-through-W3 dependencies (`restart/skinny/tranches/sk-v10/SPEC.md:376`-`379`),
and its exit gate still requires `rg` plus plan-audit proof of no live dispatch
route through W3 union/event substrate, class column, streaming cursor,
`UnionTape`, or W4 cascade-lock (`restart/skinny/tranches/sk-v10/SPEC.md:383`-`388`).

Dispatch keeps REDRESS 96-98 closed against renamed W3, structural cursor,
`UnionTape`, retained class column, sidecar producer, and W4 cascade-lock
(`restart/skinny/tranches/sk-v10/DISPATCH-PROMPT.md:12`-`16`). The binding
pre-block ledger separately states that W3 cannot be the consumer or entry gate
for W4 cascade work (`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:47`-`52`)
and that W7/W8/W9 existing-substrate primitive work may target only current
string/unescape call sites, with W3 forbidden as a caller
(`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md:189`-`195`).

Required fix: none.

## Required Fixes

None required for CH5 / Lock 1 acceptance.

V3 confirms the V2 CH5 acceptance with no reopened hidden coupling. The only V2
CH5 hygiene item, the stale P3-E "V1 CHALLENGE fold" label, has been corrected
in the current contract.
