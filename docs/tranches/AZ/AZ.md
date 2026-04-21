# Tranche AZ — Replay, Recovery, Incremental, and Debug UX on the AY Substrate

AZ is the tooling tranche over AY's close substrate. AY's job is near
parity on the canonical runtime path; AZ's job is to enrich that same
path with replay, recovery, incremental reparse, and debug UX without
reopening architectural duality. AZ therefore treats the AY substrate
as fixed runtime truth and layers provenance, decision logging,
snapshotting, recovery, and inspection on top of it with tightly
bounded feature-off cost.

## Architectural thesis

1. **AZ builds on AY; it does not replace it.** Replay, recovery,
   incremental, and debug are properties of the AY substrate.
2. **Every AZ feature stays one-path.** Metadata and logs may be added;
   alternate parsers and alternate runtime outputs may not.
3. **Richer provenance belongs here.** AY carries only the minimum
   provenance needed not to block AZ; AZ owns the richer surfaces.
4. **Feature-off cost stays bounded.** Tooling value must not come from
   perturbing the default AY hot path.

## Invariants

1. No second parser or replay-only runtime path.
2. No DTA-style state resurrection.
3. Every metadata addition ships with a same-wave consumer.
4. Feature-off regression versus AY close stays within the declared
   budget.
5. Debug truth comes from AY substrate spans, nodes, and provenance.

## Operational posture

1. Every AZ wave proves substrate identity, not merely functional
   similarity, against the cold-parse AY path.
2. Feature-gated logging and metadata are measured both on and off at
   each wave boundary.
3. Recovery and incremental work reopen no grammar-name routing and no
   hand-built side parsers.
4. Every debugging or inspection tool consumes production provenance or
   replay surfaces, not bespoke trace models.
5. Any AZ refinement that materially improves throughput rather than
   tooling ergonomics routes to BA at tranche close.
6. AZ is not the exceedance tranche. Throughput wins discovered here
   are routed out precisely so replay/debug/incremental work does not
   become another mixed-concern performance tranche.

## Wave summary

| Wave | Spec | Headline | Opens after | Status |
|---|---|---|---|---|
| **W0** | [waves/W0.md](waves/W0.md) | Provenance side metadata on the AY substrate | AY close | planned |
| **W1** | [waves/W1.md](waves/W1.md) | Decision log and substrate-identical replay | W0 | planned |
| **W2** | [waves/W2.md](waves/W2.md) | Snapshot and resume at stable shape boundaries | W1 | planned |
| **W3** | [waves/W3.md](waves/W3.md) | Edit-local incremental reparse and substrate splice | W2 | planned |
| **W4** | [waves/W4.md](waves/W4.md) | Recovery semantics and sync-point resume | W3 | planned |
| **W5** | [waves/W5.md](waves/W5.md) | Debug, inspect, and minimise tooling on the same surfaces | W4 | planned |
| **W6** | [waves/W6.md](waves/W6.md) | FINAL and bounded-cost closure | W5 | planned |

## AZ handoff contract

AZ does not close until all of the following are true:

1. Replay, resume, incremental, and recovery all operate on AY's
   substrate with no alternate parser architecture.
2. Feature-off regression stays within `<= 5%` of AY close.
3. Debug and minimise tooling consume the same provenance and replay
   surfaces emitted by production code.
4. Recovery semantics are grammar-derived and do not introduce
   grammar-name branching.
5. The close ledger identifies any throughput-oriented lessons that
   belong in BA rather than leaving them mixed into tooling work.

## Defensible floor

AZ's defensible floor is:

1. Stable provenance and decision logging.
2. Replay and resume that reproduce substrate-identical suffixes.
3. Incremental splice correctness on canonical edit suites.
4. Recovery that resumes downstream parse at declared sync points.
5. Feature-off cost bounded within the declared budget.

Anything less is an incomplete tooling tranche.

## Post-tranche review candidates

Decision at W6 close, not mid-wave:

- Whether any provenance field should migrate from substrate proper to
  an optional side table.
- Whether incremental splice granularity should be widened or narrowed
  before any future editor-facing tranche.
- Whether any replay or debug artefact ought to become a standing CI
  harness rather than a tooling-only surface.

## Indefatigability

When AZ closes correctly, bbnf still has one parser and one substrate,
but now also has truthful replay, recovery, incremental, and debug
tooling that reads the same runtime shape rather than shadowing it.
