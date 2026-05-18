# CH5 - Hidden Coupling Review

Verdict: ACCEPT

Confidence: 97%

## Scope

This V4 CH5 review is the unchanged re-challenge of the SK-V9 Alpha packet at
commit `795bbbec`, with V3 consolidated acceptance as the prior clean cycle. The
lane re-checks hidden coupling, source/product typed ambiguity, direct-to-struct
versus real-typed laundering, Track 1/Track 2 honesty, sidecar evidence-only
scope, proxy-performance claims, and absence of SK-V9 implementation dispatch.

## Findings

### CH5-1 - V3 is a clean prior cycle and requires this unchanged V4 pass

Disposition: ACCEPT.

V3 consolidated reports ACCEPT for all six lenses, with CH5 accepted at 97% and
no fold required for typed/source, direct/real-typed, Track 1/Track 2
independence, or proxy-performance claims
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:10-19`).
It also states that V4 unchanged re-challenge is required before G-Alpha
presentation
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:36-38`).
This review found no CH5-relevant drift from that accepted posture.

Required fold: none.

### CH5-2 - Source/product typed parity is not measured-row admission

Disposition: ACCEPT.

Alpha-A keeps the measured `real_typed_struct A / GO` count at four and states
that Apache/CITM source/product rows are not measured `RESULTS.md` rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:60-73`).
Alpha-D repeats the same measured authority and source/product boundary
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:22-43`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:232-237`).
REDRESS 91 admits only the source/product slice, adds no directive, BIR variant,
`BackendShape`, substrate surface, sidecar, runtime JSON behavior, or direct
digest product claim, and rejects benchmark row-table admission for SK-V8
(`skinny/REDRESS.md:2622-2659`).

The SK-V9 contract preserves the distinction: Apache/CITM can become measured
rows only through fresh run-id/metadata evidence, generated Track 1 DirectBuild,
independent serde/oracle proof, sonic parity, and row rendering
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:81-85`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`). Alpha-E blocks
source-only admission and keeps Canada routed without a fresh full-fixture
checksum proof
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:83-115`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:128-135`).

Required fold: none.

### CH5-3 - Direct digest rows are not laundered into typed product proof

Disposition: ACCEPT.

The current results table separates output planes: direct-to-struct rows are
`digest`, while real typed rows are `typed direct`
(`skinny/RESULTS.md:6-9`, `skinny/RESULTS.md:18-21`,
`skinny/RESULTS.md:27-28`, `skinny/RESULTS.md:38`). Alpha-A explicitly calls the
three current direct GO rows digest-plane guard rows, not product-plane typed
rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:75-90`).
REDRESS 93 routes remaining direct misses to a direct output contract or
control-path tranche and keeps digest evidence guard-plane only
(`skinny/REDRESS.md:2694-2729`).

SYNTHESIS forbids treating direct digest as product proof without a direct output
contract or control-path tranche
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-90`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-181`). Alpha-E requires guard or
control-path labeling when the measured plane remains `digest`, stable
control-row identity if retained, and a typed-product escape hatch that cannot
be satisfied by a digest-only row
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:278-302`).
Alpha-F tells challenge to reject direct digest as product proof
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:89-101`).

Required fold: none.

### CH5-4 - Track 1 and Track 2 remain structurally honest

Disposition: ACCEPT.

The measured authority defines Track 1 as `runtime::generated_json::parse` and
Track 2 as the independent hand-coded parser over `runtime::tape`; the signed
checklist says Track 2 uses `runtime::tape::TapeBuilder`, shares the same parity
oracle, and never calls `runtime::generated_json::parse`
(`skinny/RESULTS.md:138-140`). Real typed rows describe Track 2 as a structural
oracle rather than the SOTA gate
(`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`,
`skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

Alpha-E preserves that separation for typed row admission, where generated Track
1, serde_json Track 2/oracle, sonic checksum parity, and existing GO-row
maintain rules are distinct
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:72-115`).
For direct/control work, Alpha-E keeps the hand Track 2 direct parser as guard
reference only, requires gate/report consumption of the product/control contract,
and blocks coupling Track 2 to generated SinkOnly or generated Track 1
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:258-276`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-324`).
SYNTHESIS also requires Track 2 Mbps and Track 2 independence status in the
telemetry schema
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:263-264`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:291-295`).

Required fold: none.

### CH5-5 - Structural work does not create a hidden parallel substrate

Disposition: ACCEPT.

REDRESS 92 rejects the W3 route because scanner structural positions and
retained tape events are not isomorphic, and it blocks sidecar producers,
parser-owned structural cursors/facts, `tape_vs_tape` as production consumer,
`UnionTape`, new `BackendShape`, new BIR variant, new directive, public
substrate API, and Tier B work under the Tier A name
(`skinny/REDRESS.md:2663-2690`). Alpha-C carries that forward and requires any
future route to replace scalar rediscovery inside one retained tape rather than
adding a parallel sidecar
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-129`).

Alpha-E's retained class/event candidate requires container opens/closes, quote
ownership, number/literal starts, object keys, array values, and nesting without
a second tape, sidecar, `UnionTape`, new `BackendShape`, BIR variant, directive,
or public substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:177-211`).
SYNTHESIS repeats the pre-block list against sidecar substrate,
parser-owned cursor/fact slots, `UnionTape`, new `BackendShape`, new
directive/BIR, public substrate API, and `tape_vs_tape` as production consumer
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-329`).

Required fold: none.

### CH5-6 - Sidecar and comparator data remain evidence-only, not proxy performance

Disposition: ACCEPT.

Alpha-B limits competitor deltas to planning evidence unless the row is
strict-vs-strict, same-run, matching output plane, and measured inside the bbnf
row; it rejects lossy sonic, historical sidecars, absent sidecars, and plane
mismatches as strict wins
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:121-149`).
SYNTHESIS rejects strict admission for deferred strictness, stale sidecar-only
evidence, lossy/permissive comparators, output-plane mismatch, missing measured
validation, missing sample cost, or missing hot leaf
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

Alpha-E's sidecar manifest candidate is gate-only evidence ingestion: it cannot
produce parser data, retained tape data, row output, substrate, or strict
admission by itself, and sidecar evidence cannot act as producer, substrate, row
output source, retained tape source, or strict shortcut
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:326-384`).
The SK-V9-open telemetry refresh is also behavior-frozen and cannot change
parser, scanner, SIMD, asm, codegen, generated output, product behavior, or row
throughput
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:416-493`).

Required fold: none.

### CH5-7 - No SK-V9 implementation dispatch is present

Disposition: ACCEPT.

The contract states that V9 implementation is not dispatched, G-Alpha must be
presented after alpha challenge convergence, `G-Alpha closed` is required before
skinny passes begin, and the Alpha output deliberately does not create `SPEC.md`
or `DISPATCH-PROMPT.md`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:61-75`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`). HANDOFF repeats that no
`SPEC.md` or `DISPATCH-PROMPT.md` exists, no implementation wave dispatches
before downstream planning converges, and the detailed wave plan is absent until
after `G-Alpha closed`
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`). Alpha-F carries the same
boundary and challenge rejection rule
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:5-13`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:87-105`).

Required fold: none.

## Required Folds

None from CH5. The unchanged V4 re-challenge preserves the V3 accepted hidden
coupling posture and does not require any packet fold.

## Blockers To G-Alpha

None from CH5.

G-Alpha remains gated by completion of the full V4 hardening/consolidation path
and mandatory user sign-off. This CH5 ACCEPT does not authorize `SPEC.md`,
`DISPATCH-PROMPT.md`, or SK-V9 implementation dispatch.
