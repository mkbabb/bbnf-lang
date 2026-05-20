# SK-V11 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V11. It binds to the SK-V11 packet at
`restart/skinny/tranches/sk-v11/` and to the wave plan in
`restart/skinny/tranches/sk-v11/SPEC.md`.

Status: DRAFT. S-P3 V4 has not yet converged; no behavior source work is
authorized from this prompt until S-P3 CHALLENGE accepts the packet.

SK-V11 is not a W3 retry. REDRESS 96/97/98 retired the union/event/class-column
substrate thesis, and REDRESS 102 firewalled parse-only SOTA claims. The live
targets are product-plane direct closure or measured direct fixpoint, plus one
admitted benchmarked non-JSON generated direct/typed parser intervention.

## Required Reading

Read in order:

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/skinny/tranches/sk-v11/SYNTHESIS.md`.
6. `restart/skinny/tranches/sk-v11/HANDOFF.md`.
7. `restart/skinny/tranches/sk-v11/SPEC.md`.
8. `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
9. `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
10. `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`.
11. The S-P2 V3 research cohort:
    - `research/p2/p2a-sota-teardown.md`.
    - `research/p2/p2b-dav1d-process.md`.
    - `research/p2/p2c-arch-esoterica.md`.
    - `research/p2/p2d-substrate-tape.md`.
    - `research/p2/p2e-parse-that-gaps.md`.
    - `research/p2/p2f-grammar-neutral.md`.
12. The S-P3 cohort for the current accepted cycle, when present:
    - `research/p3/p3a-candidate-shortlist.md`.
    - `research/p3/p3b-wave-sequencing.md`.
    - `research/p3/p3c-falsifiability-gates.md`.
    - `research/p3/p3d-telemetry-schema.md`.
    - `research/p3/p3e-preblocked-ledger.md`.
    - `research/p3/p3f-spec-draft.md`.
13. `restart/skinny/tranches/sk-v10/research/close/close-redress.md`.
14. `restart/skinny/tranches/sk-v9/research/p3/hardening/HARDENING-S-P3-CONVERGED.md`.
15. `skinny/RESULTS.md`.
16. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Candidate surface | Dispatch status | Budget | Redress cap |
|---|---|---|---|---|---:|---:|
| W0 | Section 3 | SK-V11-open Telemetry Lock | W0 profile authority | Closed by S-P1/W0 | 0 behavior LOC | n/a |
| W1a | Section 4 | Non-JSON Gate/Report Schema Lane | C9 + Lock 14 gate | Conditional on S-P3 convergence + CHALLENGE | <=260 handwritten LOC; 0 generated LOC unless fixtures are named | <=90 min |
| W1b | Section 5 | Generated Non-JSON Baseline And Oracle Lane | C9 + generated baseline harness | Conditional on W1a close + CHALLENGE | <=360 handwritten LOC; generated output capped to selected inputs | <=90 min |
| W2 | Section 6 | CSS L4 Generated Direct/Typed Intervention Proof | C1/C2/C4/C5/C6 with C7 support | Conditional on W1b close + CHALLENGE | <=430 handwritten LOC; generated output capped to named inputs | <=90 min |
| W3 | Section 7 | Numeric Direct Closure Slice | C4 + D4 | Conditional on W2 disposition + CHALLENGE | <=360 handwritten LOC; generated output capped to named callers | <=90 min |
| W4 | Section 8 | Generated Dispatch And Byte-Set Control Slice | C1/C5/C6 + D1/D2 | Conditional on W3 disposition + CHALLENGE | <=430 handwritten LOC; generated output capped to named inputs | <=90 min |
| W5 | Section 9 | Bounded String Span And Special-Byte Scan | C2 + D3 | Conditional on W4 disposition + CHALLENGE | <=360 handwritten LOC; generated output capped to named callers | <=90 min |
| W6 | Section 10 | Escaped Segment And Hex Decode Slice | C3 | Conditional on W5 disposition + CHALLENGE | <=360 handwritten LOC; generated output capped to named callers | <=90 min |
| W7 | Section 11 | Output Digest/Hash Host Sink | C8 only | Conditional on W3-W6 dispositions + CHALLENGE | <=350 handwritten LOC | <=90 min |
| W8 | Section 12 | Direct Residual Fixpoint And Row Reclamation | remaining measured C1-C8 routes | Conditional on W3-W7 dispositions | <=250 docs/gate/result LOC; source requires W8a split and spare bracket slot | <=90 min |
| W9 | Section 13 | Close And Alpha Feedback | docs/gate | Conditional on W8 close | 80-180 LOC | <=90 min |

The dependency order is firm. W1a lands first because non-JSON telemetry must
be gate-consumed before any baseline can become authority. W1b creates exactly
one generated non-JSON baseline plus independent oracle. W2 is the preferred
non-JSON admitted intervention and consumes the W1b baseline. W3-W7 are
row-moving or proof/consumer waves and require CHALLENGE before redress. W8
reconciles every remaining direct residual as admitted or measured
uncloseable. W9 closes. The bracket is 11 waves, leaving one spare split before
the skinny `> 12` escalation rule.

## Per-Wave Triumvirate Protocol

Every wave follows `SKINNY-TRIUMVIRATE.md`: Research, Plan, optional or
mandatory CHALLENGE, and Redress are distinct artifacts and commits.

### Phase 1 - Research

- Up to six parallel research agents on disjoint scope rows, 30 min cap each.
- The S-P1, S-P2, and S-P3 artifacts are archived research. New research lands
  only if a wave blocker is discovered.
- Research writes under `restart/skinny/tranches/sk-v11/research/` and edits
  no source.
- Commit: `docs(sk-v11-wave{W}-research): archive {scope} cohort`.

### Phase 2 - Plan

- One or two plan agents, 30 min cap.
- The plan selects one SPEC intervention and names owner paths, entry gate,
  exit gate, scalar reference/oracle, parity/checkasm, micro-proof, row
  thresholds, LOC budget, risk class, revert protocol, same-wave consumer, and
  pre-blocked routes.
- Plan agents edit no source.
- Commit: `docs(sk-v11-wave{W}-plan): select {intervention}`.

### Phase 2.5 - CHALLENGE

CHALLENGE is mandatory for W1a, W1b, W2, W3, W4, W5, W6, and W7. It is
mandatory for W8 if W8 touches behavior source or any
generic/codegen/runtime-outside-JSON path. It is optional for W0 and W9 Close
only when they stay in already accepted gate semantics.

Six lenses apply:

- CH1 correctness and measurable row gates.
- CH2 generality and Lock 14.
- CH3 REDRESS regression/pre-blocks.
- CH4 cost and micro-proof adequacy.
- CH5 hidden coupling and Lock 1.
- CH6 anti-paper-close and same-wave consumer.

A REJECT disposition returns the wave to plan. Redress does not start until
CHALLENGE accepts.

### Phase 3 - Redress

- One redress agent. Single implementation thread per wave.
- Redress implements only the SPEC section's owner paths.
- Fresh Criterion uses `RUSTFLAGS="-C target-cpu=native"` unless the SPEC
  section states a stricter flag.
- Any AArch64 SIMD/ASM body must pass strict scalar differential/checkasm
  before row measurements count.
- Row movement must satisfy SPEC thresholds and the relevant gate consumer.
- Commit on PASS: `feat(sk-v11-wave{W}): admit {intervention}`.
- Commit on FAIL: `docs(sk-v11-wave{W}-redress): reject {intervention}` and
  preserve the rejected patch route named in REDRESS.

A failed wave still produces research, plan, challenge when required, and
redress evidence. The next wave starts fresh.

## Falsifiability Gates

Each gate is stated in the SPEC and is not redefined here:

- `G-W0-SK-V11-OPEN-LOCK`
- `G-W1a-NONJSON-GATE`
- `G-W1b-NONJSON-BASELINE`
- `G-W2-CSS-GENERATED-INTERVENTION`
- `G-W3-NUMERIC-SEQUENCE-DIRECT`
- `G-W4-DISPATCH-BYTESET-DIRECT`
- `G-W5-STRING-SPAN-DIRECT`
- `G-W6-ESCAPE-SEGMENT-DIRECT`
- `G-W7-DIGEST-SINK`
- `G-W8-DIRECT-FIXPOINT`
- `G-W9-CLOSE-SK-V11`

Load-bearing facts:

- Direct target floors are in SPEC §0.4.
- Direct and typed guard floors are in SPEC §0.5.
- W0-clamped direct rows are not admitted by opening throughput alone.
- Non-JSON close requires generated Track 1, independent oracle/Track 2, strict
  output equality, measured throughput, and gate consumption.
- Micro-prove-first is an entry gate for every kernel/SIMD/generic behavior
  intervention.
- Track 2/oracle may not call generated Track 1 or hidden shared parser code.
- Generated output may be committed only as regenerated output from named
  inputs.
- Parse-only rows cannot close SK-V11 SOTA.
- W3 reopen claims fail closed.

## Pre-Blocked Routes

Hard pre-blocks:

1. W3 union/event substrate, class column, structural-position vector,
   streaming cursor, class lane, sidecar producer, `UnionTape`, W4 cascade-lock
   through W3.
2. Parse-only SOTA close.
3. Parallel substrate, aux side table, whitespace cursor, structural cursor,
   event cursor, retained position vector, parser-owned projection.
4. Direct-vs-typed relabeling.
5. JSON policy in generic crates or runtime outside generated modules.
6. x86 implementation work.
7. PMULL prefix-XOR and CSSC CTZ/bulk emission default hot paths.
8. String materialization replay, retained wide scans, `StringBlock16`
   retained wrappers, decoded scratch/facts, x4 proof-to-production, existing
   `unescape_string` reuse as same-wave production.
9. Numeric fallback/mantissa/f64 policy rewrites.
10. Object next-key carry and value-byte compaction outside same-loop generated
    product consumers.
11. PMU/cycles/structural-scan/masking/lazy materialization as behavior
    producers.

Any REDRESS-adjacent route must state a material differential before
implementation. A rename is not a material differential.

## Telemetry And Outcome Discipline

SK-V11 inherits the schema-v3 required identifier set and outcome enum
`A C G I J K L M N-direct S`. A wave that emits a new field or non-JSON
companion report must update every report, fixture, gate, and consumer in the
same commit. The default is no new columns in `skinny/RESULTS.md`.

`gate-json` and any non-JSON companion gate must reject missing required
fields, unsupported outcomes, non-uniform run ids, stale strict anchors, strict
plane mismatch, deferred validation admission, direct digest as typed proof,
parse-only SOTA claims, W3 reopen claims, Track 2 coupling, and producer-only
telemetry.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc processes,
artifact mtimes, and dirty worktree state. Keep research, plan, CHALLENGE,
redress, and close artifacts in distinct commits. Stage only the intended
slice and preserve unrelated worktree state.

Every dispatch carries the redress minute cap from the manifest. At 0.9x the
cap the agent commits, records a REDRESS rejection, or surfaces an extension
decision. At the cap it halts.

## Convergence And Escalation

SK-V11 converges when W1a-W8 and W9 Close have admitted, proof-closed, or rejected
with measurement; SPEC §0 close holds; and the close documents agree. Close may
declare direct `GO` or measured direct fixpoint, but it cannot waive the
non-JSON benchmarked-intervention axis without a `BLOCKED` verdict.

If a wave's falsifiability gate cannot be made measurable, or a goalset row no
surviving candidate can meet, escalate as `BLOCKED` naming the unresolved gate.
