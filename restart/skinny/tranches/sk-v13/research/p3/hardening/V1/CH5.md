# SK-V13 S-P3 V1 CH5: Hidden Coupling

Verdict: REVISE

Lens: hidden sidecar substrate, parser-owned structural projection, renamed
scanner routes, Track 1 / Track 2 comparator honesty, stale SOTA tables, public
substrate/API expansion, JSON-specific Lock 14 leaks, generated/runtime
coupling, planned parallel source overlap, and material distinction from prior
REDRESS history.

## Findings

### CH5-1 - Fleet-wide Lock 14 witness cardinality still permits hidden JSON/CSS coupling

SPEC and DISPATCH currently allow a generic-crate proof with "CSS L4 and at
least one of Sheets or BBNF-self" (`restart/skinny/tranches/sk-v13/SPEC.md:338`,
`restart/skinny/tranches/sk-v13/SPEC.md:339`,
`restart/skinny/tranches/sk-v13/SPEC.md:340`,
`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:132`,
`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:133`). That is too weak for
fleet-wide grammar-neutral claims because the live value/API audit shows only
1/7 Lock 14 leaks fully resolved, 4/7 partial, and 2/7 unresolved, including
JSON dispatch, JSON quote/escape string policy, JSON key-colon structure,
`OffsetFlags` semantics, and `JsonSink` coupling
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:63`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:64`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:65`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:66`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:67`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:68`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:69`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:70`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:71`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:113`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:114`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:115`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:116`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:117`).

Required fold: SPEC Section 2.1 and DISPATCH Lock 14 must distinguish scoped
proof from fleet-wide proof. Fleet-wide generic-crate claims need CSS L4 plus
both Sheets and BBNF-self fail-closed/generated-role witnesses. CSS plus only
one of those witnesses is admissible only as a scoped non-JSON proof for the
named touched surface.

### CH5-2 - SIMD-first union can bypass same-wave zero-orphan accountability

SPEC W9 may touch `skinny/crates/bbnf-simd/` when the SIMD-first union route is
selected (`restart/skinny/tranches/sk-v13/SPEC.md:656`,
`restart/skinny/tranches/sk-v13/SPEC.md:657`,
`restart/skinny/tranches/sk-v13/SPEC.md:658`,
`restart/skinny/tranches/sk-v13/SPEC.md:659`,
`restart/skinny/tranches/sk-v13/SPEC.md:660`,
`restart/skinny/tranches/sk-v13/SPEC.md:661`,
`restart/skinny/tranches/sk-v13/SPEC.md:662`), but the W9 exit gate names union
correctness and guard maintenance without explicitly restating
`orphan_count_after = 0` for that same wave
(`restart/skinny/tranches/sk-v13/SPEC.md:678`,
`restart/skinny/tranches/sk-v13/SPEC.md:679`,
`restart/skinny/tranches/sk-v13/SPEC.md:680`,
`restart/skinny/tranches/sk-v13/SPEC.md:681`,
`restart/skinny/tranches/sk-v13/SPEC.md:682`,
`restart/skinny/tranches/sk-v13/SPEC.md:683`,
`restart/skinny/tranches/sk-v13/SPEC.md:684`). W12 later carries the explicit
zero-orphan gate (`restart/skinny/tranches/sk-v13/SPEC.md:773`,
`restart/skinny/tranches/sk-v13/SPEC.md:774`,
`restart/skinny/tranches/sk-v13/SPEC.md:775`,
`restart/skinny/tranches/sk-v13/SPEC.md:776`,
`restart/skinny/tranches/sk-v13/SPEC.md:777`,
`restart/skinny/tranches/sk-v13/SPEC.md:778`), but CH5 cannot accept a later
cleanup dependency for a wave that may itself create or reclassify SIMD
primitives. P3-C already states the binding rule: `orphan_count_after = 0`, the
five REDRESS-126 demoted primitives remain history only, and `a64_ascii_set_run_skip`
has no second production-split deferral
(`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:321`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:322`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:323`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:324`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:325`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:326`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:327`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:328`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:329`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:330`,
`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:331`).

The risk is concrete, not theoretical: the scoping inventory records five
source-present aarch64 primitives demoted as inventory and one microbench-passed
`a64_ascii_set_run_skip` route with production wiring deferred
(`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:12`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:13`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:14`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:15`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:16`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:17`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:18`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:19`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:20`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:21`,
`restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:23`).

Required fold: every SPEC section that authorizes `skinny/crates/bbnf-simd/` or
SIMD-selected generated consumers must include the same-wave zero-orphan exit
predicate, revert/delete/demote protocol, strict checkasm status, and production
consumer row evidence. W9/C3 cannot rely on W12 for orphan cleanup.

### CH5-3 - P3-B/P3-D/P3-E hidden-coupling controls are not yet binding in DISPATCH

The V1 DISPATCH authority still says P3-B through P3-E are absent
(`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:26`,
`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:27`,
`restart/skinny/tranches/sk-v13/DISPATCH-PROMPT.md:28`), and P3-F records the
same draft-time absence (`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:27`,
`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:28`,
`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:29`,
`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:30`,
`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:31`,
`restart/skinny/tranches/sk-v13/research/p3/p3f-spec-draft.md:32`). Those
artifacts now exist and carry load-bearing hidden-coupling rules that an
implementation dispatch must not miss: P3-B serializes overlapping owner paths
and ledger writers (`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:72`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:73`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:74`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:75`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:76`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:77`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:78`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:79`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:80`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:81`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:82`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:83`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:84`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:207`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:208`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:209`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:210`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:211`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:212`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:213`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:214`,
`restart/skinny/tranches/sk-v13/research/p3/p3b-wave-sequencing.md:215`), and
P3-D makes comparator, sidecar, substrate, union, SIMD, and Track 2 fields
gate-consumed rather than report prose
(`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:57`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:58`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:67`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:68`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:69`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:70`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:71`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:72`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:73`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:74`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:80`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:81`,
`restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:82`).

Required fold: DISPATCH and P3-F must name P3-B/P3-C/P3-D/P3-E as required
reading, then fold their owner-overlap, ledger-serialization, telemetry, and
pre-block rules into the final SPEC/DISPATCH source map. Without that, a wave
packet can miss a hidden coupling constraint even while reading the dispatch
contract.

## Accepted Checks

- The current SPEC/DISPATCH do not authorize the old sidecar substrate directly.
  SPEC Section 1 blocks new directives, BIR variants, `BackendShape` variants,
  public `UnionTape`, parallel substrate, parser-owned structural cursor, aux
  density table, retained class side vector, sidecar event vector, and second
  source scanner (`restart/skinny/tranches/sk-v13/SPEC.md:249`,
  `restart/skinny/tranches/sk-v13/SPEC.md:250`,
  `restart/skinny/tranches/sk-v13/SPEC.md:251`,
  `restart/skinny/tranches/sk-v13/SPEC.md:252`,
  `restart/skinny/tranches/sk-v13/SPEC.md:253`,
  `restart/skinny/tranches/sk-v13/SPEC.md:254`).
- Track 1 / Track 2 comparator honesty is correctly stated in P3-C and P3-D:
  Track 2 cannot be the SOTA anchor, cannot call Track 1, and cannot hide Track
  1 demotion (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:35`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:36`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:37`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:38`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:39`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:156`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:157`,
  `restart/skinny/tranches/sk-v13/research/p3/p3d-telemetry-schema.md:158`).
- The union/ASM reopen routes are materially named rather than generic
  "try SIMD" language: P3-C distinguishes C1 same-tape projection, C2 e-graph
  shape, and C3 SIMD mask-to-tape from REDRESS 96/97/98 and REDRESS 88/89
  (`restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:262`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:263`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:264`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:265`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:266`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:267`,
  `restart/skinny/tranches/sk-v13/research/p3/p3c-falsifiability-gates.md:268`).

## Required Fold Items

1. Replace the Lock 14 proof rule in SPEC/DISPATCH with scoped vs fleet-wide
   witness cardinality: CSS L4 plus both Sheets and BBNF-self for fleet-wide
   generic-crate closure.
2. Add same-wave `orphan_count_after = 0` and delete/demote/revert protocol to
   every SPEC section that may touch `bbnf-simd`, including W9/C3 union.
3. Update DISPATCH and P3-F to require P3-B/P3-C/P3-D/P3-E, then reconcile the
   wave/source-overlap map so implementation packets cannot ignore P3-B
   serialization or P3-D gate-consumed telemetry.

## Evidence

- Read and cross-checked PASS-3, SKINNY-TRIUMVIRATE, P3-A through P3-F,
  SPEC, DISPATCH, S-P1/S-P2 scoping audits, `skinny/RESULTS.md`, and relevant
  REDRESS-derived route ledgers.
- Verification command to run after this file is written:
  `git diff --check -- restart/skinny/tranches/sk-v13/research/p3/hardening/V1/CH5.md`.
