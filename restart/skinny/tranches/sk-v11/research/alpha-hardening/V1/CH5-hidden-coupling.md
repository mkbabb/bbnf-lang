# SK-V11 Pass Alpha CH5 Hidden Coupling

Pass: Pass Alpha CHALLENGE. Cycle: V1.
Date: 2026-05-19.
Lens: CH5 hidden coupling.
Scope: Hidden dependencies between direct-plane closure, non-JSON benchmark
standing, telemetry/gate consumption, aarch64 feature gates, Track 1 / Track 2
independence, and the no-new-directive / BIR / substrate boundary.
Output: this file only.

## Disposition

ACCEPT-WITH-NITS.

The Alpha V1 packet does not introduce a critical hidden coupling. The
contract correctly keeps direct closure on strict same-run digest evidence,
requires generated Track 1 plus independent Track 2, blocks W3 and renamed
substrate routes, makes non-JSON proof executable rather than prose-only, and
requires gate consumption for new telemetry. The nits below are wording and
handoff constraints S-P3 should carry forward so the later SPEC cannot loosen
those boundaries.

## Materials Read

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-1-PROFILE.md`
- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v11/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md`

## Findings

### CH5-1 - ACCEPT: direct closure is not coupled to typed or parse evidence

The direct residual target is structurally honest. `SYNTHESIS.md` requires each
of the 11 residual `direct_to_struct` rows to clear the strict same-run
sonic-rs 1.10x digest gate on both generated Track 1 and independent Track 2,
or receive measured per-row REDRESS proof
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:39-42`). The same section
forbids direct digest evidence as typed product proof and blocks parser-owned
sidecars, second source passes, and JSON policy in generic crates
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:70-76`).

Alpha-A and Alpha-B preserve the output-plane split: direct residual rows are
`N-direct / NO-GO` digest rows, while typed rows are a separate product-plane
surface (`restart/skinny/tranches/sk-v11/research/alpha/alpha-A-results-extraction.md:49-67`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-A-results-extraction.md:97-118`;
`restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md:46-58`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md:60-66`).
Alpha-D explicitly carries the no-analogy rule for both direct and typed
admissions (`restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md:85-93`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md:196-206`).

### CH5-2 - ACCEPT: W3, sidecar, retained-cursor, and parallel-substrate routes are blocked

The packet treats the SK-V9 W3 route as retired negative authority, not merely
a route to be renamed. `SYNTHESIS.md` blocks the union/class-column/
streaming-cursor/class-lane/sidecar substrate family
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:52-54`), and Alpha-C expands
that into retained class columns, `UnionTape`, structural index, streaming
cursor, parser-owned projection, class-lane fallback, W4-through-W3 cascade,
and renamed equivalents
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:73-88`).
That is the same surface CH5 is required to police in the pass contracts
(`restart/prompts/pass-contracts/PASS-ALPHA.md:43-45`,
`restart/prompts/skinny/PASS-2-RESEARCH.md:126-131`,
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138`).

No Alpha candidate proposes a new substrate, sidecar event vector, retained
cursor, parser-owned structural projection, public substrate API, directive,
or BIR variant. Alpha-E's "Not Shortlisted" section rejects exactly those
families (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:473-487`),
and HANDOFF converts the same boundary into refusal conditions
(`restart/skinny/tranches/sk-v11/HANDOFF.md:117-133`).

### CH5-3 - ACCEPT: Track 1 / Track 2 independence is load-bearing

Track independence is consistently carried. `SYNTHESIS.md` requires residual
direct closure on both generated Track 1 and independent Track 2
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:39-42`) and W0 must freeze Track
1 / Track 2 independence before behavior work
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:220-236`). HANDOFF refuses direct
admission without strict same-run sonic-rs direct evidence, generated Track 1,
independent Track 2, output-plane match, provenance, and gate consumption
(`restart/skinny/tranches/sk-v11/HANDOFF.md:127-129`).

Alpha-E repeats the two-track gate in the opening floors and per-candidate
gates (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:76-82`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:223-233`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:298-308`).
The packet therefore does not create a Track 1 equals Track 2 shortcut.

### CH5-4 - ACCEPT: non-JSON standing is coupled to execution and gate consumption, not prose

The non-JSON axis is not a Lock 14 prose escape hatch. `SYNTHESIS.md` requires
one non-JSON grammar to carry an admitted and benchmarked intervention through
a generated direct or typed parser
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:55-59`,
`restart/skinny/tranches/sk-v11/SYNTHESIS.md:148-163`). HANDOFF says the axes
are not alternatives and that the non-JSON wave should exercise the same
primitive family identified on the JSON direct residual surface
(`restart/skinny/tranches/sk-v11/HANDOFF.md:62-80`).

Alpha-E follows that coupling: C2/C3/C4/C5 each require a non-JSON caller-level
microbench or benchmark row in addition to JSON direct-row evidence
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:214-232`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:284-307`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:356-380`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:427-449`).
This satisfies the CH5 concern that grammar generalization could otherwise
become a detached benchmark or a generic-crate policy leak.

### CH5-5 - ACCEPT: aarch64 feature gates are explicit and x86 remains comparator-only

The Alpha packet binds ASM/SIMD to Apple aarch64 only. `SYNTHESIS.md` excludes
x86 implementation work and requires scalar reference, differential/checkasm
where applicable, host flags, feature gate, caller microbench, and same-wave
consumer for NEON/CSSC/PMULL/UDOT/SHA3 EOR3 candidates
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:60-65`,
`restart/skinny/tranches/sk-v11/SYNTHESIS.md:165-179`). Alpha-E names per-route
feature gates for CSSC/TBL, DotProd, and SHA3/EOR3 and requires scalar fallback
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:208-216`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:284-291`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:427-434`).

Alpha-B also discloses that asmjson and other C++ sidecars are absent for
direct/typed rows in the W10 close manifest, preventing sidecar comparator data
from becoming a behavior producer
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md:165-188`,
`restart/skinny/tranches/sk-v11/research/alpha/alpha-B-competitor-deltas.md:212-214`).

## Nits To Fold Forward

1. **Telemetry split must be frozen in W0.** `SYNTHESIS.md` permits non-JSON
   rows either inside `skinny/RESULTS.md` or in a companion report, provided
   S-P3 names the gate command and close condition
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:232-236`). That is acceptable,
   but S-P3 should make W0 choose the exact report/gate binding before any
   non-JSON behavior wave. Otherwise a companion report could drift from
   `gate-json` while still appearing "consumed." Alpha-E's C1 already names
   `gate-json --with-cost-facts --check-results` plus a non-JSON benchmark gate
   as the same-wave consumer
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:145-148`);
   S-P3 should preserve that as a hard entry gate.

2. **"Current string scanner" must be narrowed to a direct/typed or non-JSON
   caller in SPEC language.** C5 allows the "current string scanner or generated
   SinkOnly direct string/key recognizer" as same-wave consumer
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:436-439`).
   That wording is safe only if S-P3 names a live direct/typed/non-JSON caller
   and forbids parse-only structural producer use. The Alpha packet already
   pre-blocks parse-only SOTA and W3 substrate routes
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:49-54`); S-P3 should repeat
   that restriction in the C5 exit gate.

3. **Research directories in C1 are not redress owner paths.** Alpha-E lists
   `restart/skinny/tranches/sk-v11/research/p1/`, `p2/`, and `p3/` inside C1's
   owner path class (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:127-129`).
   Treat those as output roots for pass artefacts, not source owner paths for a
   behavior redress wave. HANDOFF says S-P1 writes under research and edits no
   source (`restart/skinny/tranches/sk-v11/HANDOFF.md:86-98`), and SYNTHESIS
   states no implementation wave exists until S-P3 converges
   (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:270-275`).

## Fold-Forward Requirements For S-P3

- Every direct-row exit gate must name the residual row, refreshed SK-V11-open
  floor, strict same-run sonic-rs direct comparator, generated Track 1,
  independent Track 2 or equivalent independent oracle, output plane, run id,
  and gate command.
- Every non-JSON grammar admission must name the grammar, generated direct or
  typed workload, scalar/reference comparator or internal oracle, benchmark row,
  run id, and gate command.
- Every SIMD/ASM wave must name the aarch64 feature gate and scalar fallback,
  and must fail closed on unsupported feature detection rather than silently
  counting scalar fallback as an ASM admission.
- No wave may add a directive, BIR variant, public substrate API, parser-owned
  sidecar/fact slot, retained cursor, aux density table, second source pass, or
  JSON policy in a generic crate.

With those nits folded, CH5 accepts Pass Alpha V1 for G-Alpha presentation.
