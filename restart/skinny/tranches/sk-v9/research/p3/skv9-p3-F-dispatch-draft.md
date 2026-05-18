# SK-V9 P3-F: DISPATCH-PROMPT Draft

Pass: S-P3 Synthesis-Plan. Cycle: V9.
Date: 2026-05-18.
Scope: Draft the next SK-V9 DISPATCH-PROMPT — the implementation-agent
per-wave dispatch contract that binds to the SK-V9 SPEC wave plan.
Output: this file (a DRAFT; the orchestrator promotes it to
`sk-v9/DISPATCH-PROMPT.md` after S-P3 CHALLENGE converges).

Integration note: composed alongside `skv9-p3-F-spec-draft.md`. Where
this prompt references a falsifiability gate or a pre-blocked route, it
references the SPEC section by number and does not restate the gate —
the SPEC draft is the single source. `[INTEGRATE P3-x]` marks where a
sibling P3 artefact supersedes a referenced section.

---

# SK-V9 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration
SK-V9. It binds to the SK-V9 packet at
`restart/skinny/tranches/sk-v9/`. Each wave of the SK-V9 SPEC is
executed by one triumvirate per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

G-Alpha is closed. W0 telemetry-lock is closed under
`sk-v9-open:criterion-fnv64-cd1673844eeea12f`. The S-P1 rerun converged
six-of-six lenses against the fresh PMU table at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv`; `G-S-P1-RERUN-CONVERGED` is
recorded PASS. S-P2 Research converged. The six behavior interventions
are dispatchable in the SPEC Section 2 dependency order. A wave is
dispatch authority only after its own SPEC entry gate passes.

## Required Reading

Read in order:

1. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
2. `restart/prompts/skinny/PASS-1-PROFILE.md`.
3. `restart/prompts/skinny/PASS-2-RESEARCH.md`.
4. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
5. `restart/skinny/tranches/sk-v9/SYNTHESIS.md`.
6. `restart/skinny/tranches/sk-v9/SPEC.md`.
7. `restart/skinny/tranches/sk-v9/HANDOFF.md`.
8. `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.
9. `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`.
10. The S-P2 report owning the dispatched wave:
    - W1 → `research/p2/skv9-p2-C-apache-citm-admission.md`.
    - W2 → `research/p2/skv9-p2-B-retained-grammar-proof.md`.
    - W3 → `research/p2/skv9-p2-A-union-event-model.md`.
    - W4 → `research/p2/skv9-p2-D-aarch64-asm-opportunities.md` §4.
    - W5 → `research/p2/skv9-p2-E-unicode-escape-codec.md` +
      `skv9-p2-D-aarch64-asm-opportunities.md` §3.
11. `restart/skinny/tranches/sk-v9/research/skv9-W0-close.md`.
12. `skinny/RESULTS.md`.
13. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | Hard cap |
|---|---|---|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | Closed | <=90 min |
| W1 | Section 4 | Apache/CITM Measured Typed-Row Admission | Dispatchable — independent | <=90 min |
| W2 | Section 5 | Retained Class/Event Grammar Proof | Conditional on W0 + proof CHALLENGE | <=90 min |
| W3 | Section 6 | Union Event-Model — Class-Column Substrate | Conditional on W2 proof acceptance | <=90 min |
| W4 | Section 7 | aarch64 ASM Consumers — String-Block Widening | Conditional on W3 close | <=90 min |
| W5 | Section 8 | Unicode-Escape Codec — Conditional Admission | Conditional on W4 close | <=90 min |
| W6 | Section 9 | Close And Alpha Feedback | Conditional on W1-W5 dispositions | <=90 min |

The dependency order is firm (`HARDENING-S-P2-CONVERGED.md`): W1 is
fully independent and lands first among behavior waves; W2 proof
unblocks W3; W3 union substrate is the consumer base for W4; W5 codec
is paired with the W4 string-scanner widening. If a requested wave's
SPEC entry gate is not PASS, refuse dispatch and record why. A
conditional section is not dispatch authority until its entry gate
passes. `[INTEGRATE P3-B]`.

## Per-Wave Triumvirate Protocol

Every wave is one triumvirate per `SKINNY-TRIUMVIRATE.md` §1 — three
phases, each its own commit:

### Phase 1 — Research (read-only)

- Up to six parallel research agents on disjoint scope rows; 30 min cap
  each. For W1-W5 the S-P2 report owning the wave is the archived
  research cohort — new research lands only if a wave blocker is
  discovered.
- Research writes one artefact per agent under
  `restart/skinny/tranches/sk-v9/research/` and edits no source.
- Commit: `docs(sk-v9-wave{W}-research): archive {scope} cohort`.

### Phase 2 — Plan (synthesis)

- One or two plan agents; 30 min cap. The plan selects the single SPEC
  intervention and names: owner paths (exactly the SPEC Section's owner
  table), the falsifiability gate (the SPEC Section's exit gate), the
  hard cap, the revert protocol, the same-wave consumer, the
  per-primitive scalar-reference + checkasm requirement, and the
  pre-blocked routes.
- Plan agents edit no source.
- Commit: `docs(sk-v9-wave{W}-plan): select {intervention}`.

### Phase 2.5 — CHALLENGE (mandatory for W2, W3, W4, W5)

- Six-lens adversarial review of the plan per `SKINNY-TRIUMVIRATE.md`
  §4 (CH1 correctness, CH2 generality/Lock 14, CH3 regression/REDRESS,
  CH4 cost, CH5 hidden coupling, CH6 next-tranche/revert). 90 min wall.
- CHALLENGE is mandatory for W2 (first-of-class proof surface), W3
  (substrate-touching), W4 (SIMD primitive), W5 (SIMD primitive).
  CHALLENGE is optional for W1 (mechanical baseline-whitelist
  expansion) and skipped for W6 (docs reconciliation).
- A REJECT disposition routes the wave back to plan; the wave does not
  reach redress until CHALLENGE accepts.

### Phase 3 — Redress (implementation + measurement)

- One redress agent — single implementation thread per wave; no
  shared-file races. 75 min cap (60 impl + 15 measure).
- The redress agent implements only the SPEC Section's owner paths;
  any other source path returns REVISE before editing. It produces a
  fresh Criterion capture with `RUSTFLAGS="-C target-cpu=native"`,
  measures against the SPEC Section's exit gate, and per
  `SKINNY-TRIUMVIRATE.md` §8 confirms the same-wave consumer call
  shows in the `samply` symbol path on the affected rows.
- Commit on PASS: `feat(sk-v9-wave{W}): admit {intervention}` —
  source edits + bench rerun output + REDRESS entry numbering the
  admit.
- Commit on FAIL: `docs(sk-v9-wave{W}-redress): reject {intervention}`
  — REDRESS entry with per-row Mbps measurement evidence + the
  reverted patch saved at `/tmp/skv9-wave{W}-rejected.patch`.

A failed wave still produces three commits. The next wave starts fresh.

## Falsifiability Gates

Every wave's falsifiability gate is the exit gate stated in its SPEC
section — `G-W1-TYPED-ADMISSION` (SPEC §4), `G-W2-RETAINED-PROOF`
(§5), `G-W3-UNION-SUBSTRATE` (§6), `G-W4-STRING-BLOCK` (§7),
`G-W5-CODEC` (§8), `G-W6-CLOSE` (§9). The gates are not restated here;
the SPEC is the single source. Each gate names corpus rows and Mbps
thresholds drawn from the SK-V9-open baseline; an unmeasurable gate is
a REVISE. `[INTEGRATE P3-C]`.

Load-bearing gate facts the redress agent must carry into measurement:

- **The W10b six-row block** (`canada`, `citm_catalog`, `instruments`,
  `marine_ik`, `mesh`, `numbers`) is a binding maintain gate on every
  wave that touches the parse or string loop — W3, W4, W5. Any one row
  below `today × 0.98` or its sonic-strict floor falsifies the wave.
- **W5 admits zero rows on the codec alone.** The four uncloseable
  rows admit only by the SPEC §8 conditional same-wave rule (codec
  paired with the W4 widening), or are recorded NEAR-FAIL / FAIL with
  the honest projection. A NEAR-FAIL/FAIL recorded honestly is a valid
  measured close, not a paper-close.
- **`gsoc-2018` does not bind W3.** W3 falsifies only if the
  structural-rediscovery hot leaf does not drop to ≤ 5% self-time.

## Pre-Blocked Routes

The pre-blocked-route ledger is carried per-wave in the SPEC Section's
"Pre-blocked routes" line. A wave that touches a rejected ownership
boundary must cite the REDRESS entry, state why the shape is materially
different, and pass CHALLENGE before redress. `[INTEGRATE P3-E]`.

Always blocked unless a future accepted plan reopens:

- New directive, BIR variant, substrate surface, `BackendShape`,
  `UnionTape`, public substrate API, sidecar substrate, parser-owned
  cursor/fact slots, or parallel substrate.
- Strict admission from `parse_only`, sidecar, permissive, lossy,
  stale, historical, absent, deferred, or view-boundary evidence.
- Apache/CITM measured typed row admission from source/product parity
  alone (REDRESS 91) — W1 must carry a fresh same-run run-id.
- Canada typed admission from length, digest, schema, field-count,
  coordinate, or partial-fixture evidence.
- W3 union as a storage-only swap (REDRESS 92) — W3 lands only after
  the W2 compile-time proof accepts.
- Direct digest as typed product proof; scalar-parent folding
  (REDRESS 93).
- REDRESS 82 codec falsification / REDRESS 83 StringBlock16 tiny-probe
  — W4/W5 must prove a material differential.
- PMULL prefix-XOR and CTZ/bulk rewires as default hot paths; the SHA3
  `veor3q_u8` collapse is Lock-16-gated by FEAT_SHA3 and out of SK-V9
  scope.
- PMU, masking probes, Criterion slopes, or cycles-per-byte as
  behavior producers — they stay diagnostic.
- Generic JSON policy leaks or Lock 14 weakening.

The full prior pre-block ledger in
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
is binding by reference.

## Non-Negotiables

The SPEC Section 1 non-negotiables bind every wave. Load-bearing for
the implementation agent:

- No primitive ships without a scalar reference, a checkasm
  differential test (`checkasm_parity_status=PASS`), and a same-wave
  hot-path consumer landed in the same commit. An orphan kernel is a
  REJECT — this is the SK-V5 failure shape.
- No behavior source change without a same-wave consumer and a
  measured row gate.
- No wave closes on a future-phase promise. "Wired" or "integrated"
  without a bench-row threshold is a paper-close (CH6).
- Every generic-crate edit carries a CSS L4 / Sheets / BBNF-self
  non-JSON proof (SPEC §2.1) — a named no-op dry run, focused test, or
  unchanged-output audit.
- Substrate cardinality stays at one across every wave; the W3 class
  column is a co-indexed column on the existing offset tape, not a new
  tape.
- Research, plan, CHALLENGE, redress, and close are distinct commits;
  same-commit role merger was the SK-V5 failure pattern.

## Status Discipline

Before any status reply, reconcile agent status, running cargo/rustc
processes, artefact mtimes, and dirty worktree state. Keep research,
plan, CHALLENGE, redress, and close artefacts in distinct commits per
`SKINNY-TRIUMVIRATE.md` §9. Stage the intended slice separately from
any pre-existing dirty worktree state.

Every dispatch carries an explicit minute cap. At 0.9× the cap the
agent commits; at the cap it halts and surfaces an extension decision
to the user.

## Convergence And Escalation

The SK-V9 bracket converges when every SPEC wave W1-W6 has admitted or
rejected with measurement, the §0.1 close condition holds, and the
five close documents agree (`SKINNY-TRIUMVIRATE.md` §3). Convergence
triggers `G-ALPHA-SK-V9` and the Pass Alpha dispatch for the SK-V9 →
SK-V10 synthesis.

If a wave's redress fails, the wave records a measured REDRESS reject
and the next wave starts fresh — a rejected wave is not a bracket
stall. If the SK-V9 bracket exceeds 12 waves without convergence, the
orchestrator escalates to the user with `BLOCKED: skinny bracket V9
exceeded 12 waves; user adjudicate scope or abandon`.

A wave whose falsifiability gate cannot be made measurable, or a
goalset row no shortlist candidate can meet, escalates to the user as
a `BLOCKED` verdict naming the unresolved gate.
