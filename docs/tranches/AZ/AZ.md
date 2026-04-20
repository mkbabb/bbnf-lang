# Tranche AZ — Replay, Recovery, Incremental, and Debug UX on the AY Substrate

AZ is the tooling tranche over the AY-close canonical substrate.

AY's job is near-parity hot-path closure. AZ's job is everything that
must remain on the same substrate but is not required to hit that
parity bar:

- replay,
- recovery,
- incremental reparse,
- parse-step/debug UX,
- richer provenance and navigation side data,
- test-case minimisation and inspection tooling.

AZ opens only after AY closes on one parser and one canonical packed
runtime substrate.

## Architectural thesis

Four propositions govern AZ.

1. **AZ builds on AY's substrate; it does not replace it.** Replay,
   recovery, incremental, and debug are properties of the AY substrate,
   not alternate parsers or alternate runtime outputs.

2. **Every AZ feature stays one-path.** Feature-on and feature-off may
   add metadata, logging, or side tables, but they may not create a new
   parser architecture.

3. **Richer provenance belongs here.** Any side metadata or substrate
   refinement that is not parity-critical for AY but is needed for
   replay/recovery/incremental/debug lands in AZ.

4. **Feature-off regression is tightly bounded.** AZ is valuable for
   tooling, not for perturbing the hot path. Its feature-off path must
   remain close to AY close.

## Invariants

1. **No second parser.** Replay/resume/recovery operate by re-entering
   the same AY shape-emitted parser architecture.

2. **No legacy DTA state resurrection.** No `dispatch_one`, no
   `DtaSnapshot`, no DTA-style interpreter runtime.

3. **Substrate with consumer.** Every snapshot/log/side-table addition
   ships with a production consumer and an end-to-end test.

4. **Feature-off first.** Default-feature regression versus AY close
   must stay within the declared budget.

5. **Debug truth comes from the substrate.** Debug tooling reads stable
   node/span/provenance data from the AY substrate, not a parallel trace
   model.

## Scope

1. **Z0 — Provenance side metadata.** Extend the AY substrate with the
   minimum stable node/rule/span/shape provenance needed for replay,
   resume, and debug inspection.

2. **Z1 — Decision log + replay.** Record shape-emitter decisions as a
   feature-gated property and provide a replay path that re-enters the
   same parser against that log.

3. **Z2 — Snapshot/resume.** Capture resumable parser/substrate state at
   stable shape boundaries and emit `parse_resume_<grammar>` entrypoints.

4. **Z3 — Incremental reparse.** Localize edits against substrate spans,
   resume from stable boundaries, and splice the same canonical
   substrate.

5. **Z4 — Recovery.** Structural-default recovery plus grammar-declared
   override semantics where needed.

6. **Z5 — Debug/inspect/minimise tooling.** CLI and test tooling over
   the same logs/provenance substrate.

7. **Z6 — FINAL.** Close on tooling correctness and bounded feature-off
   cost.

## Wave schedule

| Wave | Scope | Agents | Hard gate |
|------|-------|--------|-----------|
| **Z0** | Provenance side metadata on the AY substrate | 3 parallel | required provenance present with feature-off regression `<= 3%` |
| **Z1** | Decision log + replay | 3 parallel | replay reproduces substrate-identical output on corpus fixtures |
| **Z2** | Snapshot/resume | 2 parallel | `parse_resume_<grammar>` reproduces cold-parse suffix results at stable boundaries |
| **Z3** | Incremental reparse | 2 parallel | edit-local incremental reparse is substrate-identical to cold reparse on canonical edit suites |
| **Z4** | Recovery | 2 parallel | malformed corpus recovers to declared sync points and resumes downstream parse |
| **Z5** | Debug/inspect/minimise | 3 parallel | tooling emits stable traces/minimised repros over the same substrate |
| **Z6** | FINAL | 1 serial | default-feature regression `<= 5%` vs AY close; tooling harnesses green |

## Refinements shifted from AY into AZ

The following belong in AZ rather than AY because they are not required
for AY's near-parity closure:

- richer provenance side tables beyond AY's minimum hot-path needs,
- snapshot-friendly metadata and resumable shape-boundary bookkeeping,
- replay decision logs and inspector/readback tooling,
- debug-oriented navigation metadata and trace presentation,
- recovery sync metadata and `@recover` semantics refinement,
- ergonomic inspection/minimisation workflows.

AY provides the canonical substrate. AZ enriches it for developer and
editor tooling.

## Hard gates

AZ closes only if:

1. replay, resume, incremental, and recovery all operate on the AY
   substrate with no alternate parser path,
2. feature-off regression stays within `<= 5%` of AY close,
3. debug/inspect tools consume the same provenance/logging surfaces the
   parser emits,
4. no DTA-style runtime substrate or state model reappears.
