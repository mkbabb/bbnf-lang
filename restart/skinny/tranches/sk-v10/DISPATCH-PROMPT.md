# SK-V10 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V10. It binds to the SK-V10 packet at
`restart/skinny/tranches/sk-v10/` and to the wave plan in
`restart/skinny/tranches/sk-v10/SPEC.md`.

G-Alpha is closed. S-P1 Profile and S-P2 Research are closed. S-P3 is the
authority for this dispatch contract. Source work is authorized only
wave-by-wave after the requested wave's SPEC entry gate passes.

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
12. `skinny/RESULTS.md`.
13. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Candidate ids | Dispatch status | Hard cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | SK-V10-open Telemetry Freeze | `C12` | Initial gate-only wave | <=90 min |
| W1 | Section 4 | Direct Output/Control-Path Contract | `C1` | Conditional on W0 close | <=90 min |
| W2 | Section 5 | Direct Row-Table Reclamation | `C1` | Conditional on W1 close | <=90 min |
| W3 | Section 6 | W3 And Parse-Only Firewall | firewall | Conditional on W2 close | <=90 min |
| W4 | Section 7 | `instruments` Typed Product Admission | `C2` | Conditional on W3 close | <=90 min |
| W5 | Section 8 | Root-Type Typed Generalization Proof | `C3` | Conditional on W4 disposition | <=90 min |
| W6 | Section 9 | Root Typed Row Admission | `C3` | Conditional on W5 proof | <=90 min |
| W7 | Section 10 | String Primitive Micro-Proof | `C4` or `C5` | Conditional on W3 close + CHALLENGE | <=90 min |
| W8 | Section 11 | Escape/Segment Micro-Proof | `C6` or `C7` | Conditional on W7 as needed + CHALLENGE | <=90 min |
| W9 | Section 12 | Existing-Call-Site Kernel Production | proven `C4`-`C9` | Conditional on W7/W8 proof + CHALLENGE | <=90 min |
| W10 | Section 13 | Direct Residual Behavior Tranche | `C1` follow-on | Conditional on W2 + W3 + CHALLENGE | <=90 min |
| Close | Section 14 | SK-V10 Close Accounting | `C11`, docs | Conditional on W0-W10 dispositions | <=90 min |

The dependency order is firm. W1 must precede W2 and W10. W3 is a firewall
only and never dispatches a W3 substrate. W7 and W8 are proof-only micro waves.
W9 is the only kernel production wave and may consume only a W7/W8-proven
primitive at an existing call site. A wave whose SPEC entry gate is not PASS
must be refused with the reason recorded.

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

CHALLENGE is mandatory for W1, W4, W5, W6, W7, W8, W9, and W10. It is optional
for W0, W2, W3, and Close unless the plan touches gate semantics outside the
SPEC text.

Six lenses apply:

- CH1 correctness.
- CH2 generality and Lock 14.
- CH3 regression and REDRESS adjacency.
- CH4 cost and micro-proof adequacy.
- CH5 hidden coupling and Lock 1.
- CH6 anti-paper-close.

A REJECT disposition returns the wave to plan. No redress starts until
CHALLENGE accepts. For W7-W9, CHALLENGE must reject any plan that combines
multiple primitive families, missing scalar oracle, missing checkasm or
differential parity, absent caller microbench, or absent production consumer.

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

Every dispatch carries an explicit minute cap. At 0.9x the cap the agent
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
