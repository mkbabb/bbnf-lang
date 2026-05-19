# SK-V10 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V10. It binds to the SK-V10 packet at
`restart/skinny/tranches/sk-v10/` and to the wave plan in
`restart/skinny/tranches/sk-v10/SPEC.md`.

G-Alpha is closed. S-P1 Profile, S-P2 Research, and S-P3 Synthesis-Plan are
closed. W0 telemetry freeze is closed under REDRESS 99. W1 direct contract is
closed under REDRESS 100. W2 direct row-table reclamation is closed under
REDRESS 101. W3 parse-only firewall is closed under REDRESS 102. W4
`instruments` typed product admission is rejected under REDRESS 103. W5
root-type typed generalization proof is closed under REDRESS 104. W6
`github_events` root typed row admission is closed under REDRESS 105. W7
full-string primitive micro-proof is rejected under REDRESS 106. W8 hex escape
micro-proof is closed under REDRESS 107. Source work is authorized only
wave-by-wave after the requested wave's SPEC entry gate passes; W9 is the next
live dispatch for the exact W8 C6 `unescape_string` proof, subject to
CHALLENGE.

REDRESS 96, 97, and 98 retire the W3 union/event/class-column substrate
thesis. No implementation agent may reopen it through a renamed W3, a
structural cursor, `UnionTape`, retained class column, sidecar producer, or W4
cascade-lock. Parse-only is diagnostic `S / NO-GO`; direct and typed product
planes are the SK-V10 row-moving surfaces.

## Required Reading

Read in order:

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/skinny/tranches/sk-v10/SYNTHESIS.md`.
6. `restart/skinny/tranches/sk-v10/HANDOFF.md`.
7. `restart/skinny/tranches/sk-v10/SPEC.md`.
8. `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`.
9. `restart/skinny/tranches/sk-v10/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`.
10. `restart/skinny/tranches/sk-v10/research/p2/p2g-candidate-ledger.md`.
11. The S-P3 cohort:
    - `research/p3/p3a-candidate-shortlist.md`.
    - `research/p3/p3b-wave-sequencing.md`.
    - `research/p3/p3c-falsifiability-gates.md`.
    - `research/p3/p3d-telemetry-schema.md`.
    - `research/p3/p3e-preblocked-ledger.md`.
    - `research/p3/p3f-spec-draft.md`.
12. The W0 close artefacts:
    - `research/w0/w0-research.md`.
    - `research/w0/w0-plan.md`.
    - `research/w0/w0-redress.md`.
13. The W1 close artefacts:
    - `research/w1/w1-research.md`.
    - `research/w1/w1-plan.md`.
    - `research/w1/hardening/CHALLENGE-W1-CONSOLIDATED.md`.
    - `research/w1/w1-redress.md`.
14. The W2 close artefacts:
    - `research/w2/w2-research.md`.
    - `research/w2/w2-plan.md`.
    - `research/w2/w2-redress.md`.
15. The W3 close artefacts:
    - `research/w3/w3-research.md`.
    - `research/w3/w3-plan.md`.
    - `research/w3/w3-redress.md`.
16. The W4-W7 close artefacts:
    - `research/w4/w4-redress.md`.
    - `research/w5/w5-redress.md`.
    - `research/w6/w6-redress.md`.
    - `research/w7/w7-redress.md`.
    - `research/w8/w8-redress.md`.
17. `skinny/RESULTS.md`.
18. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Candidate ids | Dispatch status | Budget | Redress cap |
|---|---|---|---|---|---:|---:|
| W0 | Section 3 | SK-V10-open Telemetry Freeze | `C12` | Closed - REDRESS 99 | 120-240 gate/report LOC | <=90 min |
| W1 | Section 4 | Direct Output/Control-Path Contract | `C1` | Closed - REDRESS 100 | 180-320 docs/gate LOC | <=90 min |
| W2 | Section 5 | Direct Row-Table Reclamation | `C1` | Closed - REDRESS 101 | 120-240 gate/report LOC | <=90 min |
| W3 | Section 6 | W3 And Parse-Only Firewall | firewall | Closed - REDRESS 102 | 80-160 docs/gate LOC | <=90 min |
| W4 | Section 7 | `instruments` Typed Product Admission | `C2` | Rejected - REDRESS 103 | 160-260 source/generated + 40-80 gate LOC | <=90 min |
| W5 | Section 8 | Root-Type Typed Generalization Proof | `C3` | Closed - REDRESS 104 | 220-420 source/generated + 60-120 test/gate LOC | <=90 min |
| W6 | Section 9 | Root Typed Row Admission | `C3` | Closed - REDRESS 105 | 160-260 source/generated + 40-80 gate LOC per corpus | <=90 min |
| W7 | Section 10 | String Primitive Micro-Proof | `C4` or `C5` | Rejected - REDRESS 106 | 90-260 proof LOC | <=90 min |
| W8 | Section 11 | Escape/Segment Micro-Proof | `C6` | Closed - REDRESS 107 | 90-260 proof LOC | <=90 min |
| W9 | Section 12 | Existing-Call-Site Kernel Production | proven `C6` | Dispatchable for W8 C6 `unescape_string` proof + CHALLENGE | 220-420 source/bench/gate LOC | <=90 min |
| W10 | Section 13 | Direct Residual Behavior Tranche | `C1` follow-on | Conditional on W2 + W3 + CHALLENGE | 320 source/gate LOC; 420 only with CHALLENGE | <=90 min |
| Close | Section 14 | SK-V10 Close Accounting | `C11`, docs | Conditional on W0-W10 dispositions | 80-160 docs/gate LOC | <=90 min |

The dependency order is firm. W1 must precede W2 and W10. W3 is a firewall
only and never dispatches a W3 substrate. W4 rejected with measurement and no
typed row movement. W5 proved array and map-entry typed roots but moved no
row. W6 admitted `github_events/real_typed_struct`. W7 rejected the
`C5-full-string-proof` route under REDRESS 106 and cannot feed W9. W8 remains a
proof-only micro wave and accepted C6 for `unescape_uxxxx_x4_neon` in
`unescape_string` under REDRESS 107. W9 is the only kernel production wave and
may consume only that exact accepted W8 proof, or a future SPEC/CHALLENGE
accepted replacement proof, for the exact `C4`-`C7` primitive and existing call
site. `C8` and `C9` cannot feed W9 without a future SPEC/CHALLENGE amendment. A
wave whose SPEC entry gate is not PASS must be refused with the reason recorded.

## Per-Wave Triumvirate Protocol

Every wave follows `SKINNY-TRIUMVIRATE.md`: Research, Plan, optional or
mandatory CHALLENGE, and Redress each land as distinct artefacts and commits.

### Phase 1 - Research

- Up to six parallel research agents on disjoint scope rows, 30 min cap each.
- The S-P1, S-P2, and S-P3 artefacts are the archived research. New research
  lands only if a wave blocker is discovered.
- Research writes under `restart/skinny/tranches/sk-v10/research/` and edits
  no source.
- Commit: `docs(sk-v10-wave{W}-research): archive {scope} cohort`.

### Phase 2 - Plan

- One or two plan agents, 30 min cap.
- The plan selects the single SPEC intervention and names exact owner paths,
  entry gate, exit gate, falsifiability threshold, LOC budget, risk class,
  revert protocol, same-wave consumer, scalar reference or oracle,
  checkasm/differential requirement, and pre-blocked routes.
- Plan agents edit no source.
- Commit: `docs(sk-v10-wave{W}-plan): select {intervention}`.

### Phase 2.5 - CHALLENGE

CHALLENGE is mandatory for W1, W4, W5, W6, W7, W8, W9, and W10. It is also
mandatory for any first-of-class source edit and for any generic-crate,
codegen, `bbnf-simd`, `parse-that-regex`, or runtime-outside-JSON edit. It is
optional for W0, W2, W3, and Close only when the plan stays inside the already
accepted gate semantics and avoids those source classes.

Six lenses apply:

- CH1 correctness.
- CH2 generality and Lock 14.
- CH3 regression and REDRESS adjacency.
- CH4 cost and micro-proof adequacy.
- CH5 hidden coupling and Lock 1.
- CH6 anti-paper-close.

A REJECT disposition returns the wave to plan. No redress starts until
CHALLENGE accepts. For W7 and W8, CHALLENGE must reject any plan that combines
multiple primitive families, misses scalar oracle, misses checkasm or
differential parity, lacks an identified existing caller, or lacks a
threshold-bearing caller microbench. For W9, CHALLENGE must also reject any
plan lacking same-commit production consumer wiring.

### Phase 3 - Redress

- One redress agent. Single implementation thread per wave.
- Redress implements only the SPEC section's owner paths. Any other source
  path returns REVISE before editing.
- Fresh Criterion capture uses `RUSTFLAGS="-C target-cpu=native"` unless the
  SPEC section states a stricter host flag.
- Row movement must satisfy the SPEC gate and `gate-json`.
- Same-wave consumer proof is mandatory for behavior source changes.
- Commit on PASS: `feat(sk-v10-wave{W}): admit {intervention}`.
- Commit on FAIL: `docs(sk-v10-wave{W}-redress): reject {intervention}` and
  preserve the rejected patch route named in REDRESS.

A failed wave still produces the research, plan, and redress records required
by the triumvirate contract. The next wave starts fresh.

## Falsifiability Gates

Each gate is stated in the SPEC and is not redefined here:

- `G-W0-TELEMETRY-FREEZE`
- `G-W1-DIRECT-CONTRACT`
- `G-W2-DIRECT-RECLAMATION`
- `G-W3-PARSE-FIREWALL`
- `G-W4-INSTRUMENTS-TYPED`
- `G-W5-ROOT-TYPED-PROOF`
- `G-W6-ROOT-TYPED-ROW`
- `G-W7-STRING-MICROPROOF`
- `G-W8-ESCAPE-SEGMENT-MICROPROOF`
- `G-W9-KERNEL-PRODUCTION`
- `G-W10-DIRECT-RESIDUAL`
- `G-CLOSE-SK-V10`

Load-bearing facts:

- Direct row floors and direct maintain floors are in SPEC Section 0.2.
- Existing typed maintain floors are in SPEC Section 0.2.
- The W10b maintain block is in SPEC Section 0.2 and binds any aarch64 SIMD,
  string, unescape, number, whitespace, byte-class, movemask, or parse-loop
  production wiring.
- New typed rows require generated Track 1, independent Track 2/oracle,
  serde_json typed, sonic-rs typed, full-fixture checksum parity, same-run
  Criterion metadata, and `ceil(same-run sonic_typed / 1.10)` for Track 1 and
  Track 2/oracle.
- W7 and W8 proof-only closure requires a threshold-clearing caller microbench
  artifact with observed value, threshold, run id, host triple, build flags,
  feature gate, representative corpus slices, sample count, scalar oracle
  identity, and differential harness identity. A miss records observed value
  versus threshold in REDRESS.
- W9 is limited to exactly one proven primitive, exactly one existing
  production caller, exactly one consumer plane, and one row-moving target set.
  Split the wave if both direct and typed rows would move or if gate/report
  updates exceed the manifest budget.
- Track 2 may not call generated Track 1, generated SinkOnly helpers, generated
  typed helpers, or benchmark-private shared parser code. If `gate-json` cannot
  prove that boundary, the wave must carry an audit artifact naming the checked
  paths.
- Generated artifacts are read-only evidence unless the same wave owns the
  generator/schema input and regeneration command. Generated output may be
  committed only as regenerated output, never as a hand patch.
- Generic/codegen/runtime-outside-JSON behavior edits must pass SPEC Section
  2.1 with named CSS L4, Sheets, or BBNF-self proof.
- Parse-only rows never close SK-V10 SOTA while `S / NO-GO`.
- W3 reopen claims fail closed under REDRESS 98.

## Pre-Blocked Routes

The binding ledger is
`restart/skinny/tranches/sk-v10/research/p3/p3e-preblocked-ledger.md`.
The hard pre-blocks are:

1. W3/union/event substrate, retained class column, `UnionTape`, structural
   index, streaming structural cursor, parser-owned structural projection.
2. W4 cascade-lock through W3.
3. Parse-only SOTA close.
4. Sidecar or parallel substrate producer.
5. Generic JSON policy leaks.
6. Direct-vs-typed relabeling.
7. Canada typed shortcut.
8. PMULL/VPCLMUL prefix-XOR as default and CSSC/CTZ bulk emission as default.
9. Eager scratch or decoded direct materialization replay.
10. Capacity pre-scan as product evidence.

Any REDRESS-adjacent route must state the material differential before
implementation. A rename, narrower prose, or helper reshuffle is not a
material differential.

## Telemetry And Outcome Discipline

SK-V10 inherits the SK-V9 36-field telemetry schema and outcome enum:
`A C G I J K L M N-direct S`. A wave that emits a new field or outcome must
update every report, fixture, gate, and RESULTS consumer in the same commit.
The default is no new columns.

`gate-json` must reject missing required fields, unsupported outcomes,
non-uniform run ids, stale strict anchors, strict plane mismatch, deferred
validation admission, direct digest as typed proof, parse-only SOTA claims, W3
reopen claims, Track 2 coupling, and producer-only telemetry.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc
processes, artefact mtimes, and dirty worktree state. Keep research, plan,
CHALLENGE, redress, and close artefacts in distinct commits. Stage only the
intended slice and preserve unrelated worktree state.

Every dispatch carries an explicit redress minute cap from the manifest.
Research is capped at 30 minutes per agent, plan at 30 minutes per agent, and
CHALLENGE at 60-90 minutes when required. At 0.9x the redress cap the agent
commits or records a REDRESS rejection; at the cap it halts and surfaces the
extension decision.

## Convergence And Escalation

The SK-V10 bracket converges when W0-W10 and Close are admitted, proof-closed,
or REDRESS-rejected with measurement; Section 0 close condition holds; and the
close documents agree. The close route sends the REDRESS 98 substrate-ceiling
lock amendment to Pass Omega and sends non-JSON generalization risk to the
totality track.

If a wave's falsifiability gate cannot be made measurable, or a goalset row no
shortlist candidate can meet, escalate as `BLOCKED` naming the unresolved gate.
If the bracket exceeds its W0-W10 plus Close envelope without convergence,
escalate as `BLOCKED: skinny bracket V10 exceeded planned wave envelope; user
adjudicate scope or abandon`.
