# CH5 - Hidden Coupling Review

Verdict: ACCEPT

Confidence: 97%

## Scope

This V2 CH5 review audits the folded SK-V9 Alpha packet at commit `e3ebe0b4`
after V1 hardening folds. The lane checks hidden coupling, typed
source/product ambiguity, direct-to-struct versus real-typed laundering,
Track 1/Track 2 honesty, sidecar evidence-only scope, and proxy-performance
claims.

## Findings

### CH5-1 - V1 folds landed without creating CH5 coupling

Disposition: ACCEPT.

V1 consolidated required folds for correctness, scope/cost, Lock 14/comparator
telemetry, and regression pre-blocks, while CH5 itself required no unique fold
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:25-68`).
The folded packet now carries the behavior-versus-gate split in `SYNTHESIS.md`
and `HANDOFF.md`: three behavior candidates are separated from two gate-only
prerequisites, and the sidecar prerequisite is explicitly barred from producing
parser data, retained tape data, row output, substrate, or strict admission
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:37-52`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:41-51`). Alpha-E carries the same
sidecar evidence-only rule and a telemetry-only no-behavior-drift gate
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:335-394`).

Required fold: none.

### CH5-2 - Typed source/product rows are not treated as measured rows

Disposition: ACCEPT.

The source/product boundary is now explicit across the packet. Alpha-A says the
measured `real_typed_struct A / GO` count remains four and that Apache/CITM are
W2 source/product parity only, not SK-V8 measured `RESULTS.md` rows
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:66-73`).
Alpha-D repeats that Apache/CITM are not measured rows and that every current
main row is still `Strictness=deferred` and `parse_utf8=view-boundary`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:34-43`).
REDRESS 91 confirms that W2 added no substrate, sidecar, direct digest product
claim, or measured row-table admission, and that `skinny/RESULTS.md` remained
unchanged (`skinny/REDRESS.md:2622-2659`).

The folded contract preserves that distinction: Apache/CITM measured-row
admission requires fresh run-id/metadata evidence, generated Track 1 DirectBuild,
independent serde/oracle proof, a sonic parity lane, and row rendering; source
parity alone cannot count as measured progress
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:81-85`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`). Alpha-E also blocks
source-only admission and keeps Canada routed until a full-fixture checksum proof
exists (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:92-115`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:128-135`).

Required fold: none.

### CH5-3 - Direct digest evidence is not laundered into real typed product proof

Disposition: ACCEPT.

The current result table keeps output planes separate: direct-to-struct rows are
`digest`, while real typed rows are `typed direct`
(`skinny/RESULTS.md:6-9`, `skinny/RESULTS.md:18-21`,
`skinny/RESULTS.md:27-28`, `skinny/RESULTS.md:38`). Alpha-A states that the
three current direct GO rows are digest-plane guard rows, not product-plane typed
rows (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:75-90`).
REDRESS 93 keeps remaining direct misses on a direct-output-contract or
control-path route and says digest evidence remains guard-plane only
(`skinny/REDRESS.md:2694-2729`).

The folded SK-V9 contract forbids treating direct digest as product proof without
a direct output contract or control-path tranche
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-90`). Alpha-E's direct candidate
requires guard/control-path labeling when the measured plane remains `digest`,
requires a distinct control-row identity if control work is retained, and says a
digest-only row cannot satisfy the typed-product escape hatch
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:278-302`).
Alpha-F tells challenge to reject any draft that treats direct digest as product
proof (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:87-101`).

Required fold: none.

### CH5-4 - Track 1 and Track 2 remain structurally honest

Disposition: ACCEPT.

The current measured authority states that Track 1 is
`runtime::generated_json::parse`, while Track 2 is an independent hand-coded
parser over `runtime::tape`; the Track 2 checklist says Track 2 uses
`runtime::tape::TapeBuilder`, shares the parity oracle, and never calls Track 1
(`skinny/RESULTS.md:138-141`). The typed rows also describe Track 2 as a
structural oracle rather than the SOTA speed gate
(`skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`,
`skinny/RESULTS.md:28`).

Alpha-E keeps that split. Typed row-table admission uses generated Track 1,
serde_json Track 2/oracle, and sonic checksum parity, while Track 2 remains a
structural oracle rather than a SOTA speed floor
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:72-115`).
Direct work must not couple Track 2 to generated SinkOnly or generated Track 1
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:258-276`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-324`).
The SK-V9 telemetry schema requires Track 2 Mbps and Track 2 independence status
as reported fields (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:263-264`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:291-295`).

Required fold: none.

### CH5-5 - No parallel substrate, renamed scanner, or sidecar producer is authorized

Disposition: ACCEPT.

REDRESS 92 rejects the W3 structural-projection route because scanner structural
positions and retained tape events are not isomorphic, and it blocks sidecar
producers, parser-owned structural cursors/facts, `tape_vs_tape` as production
consumer, `UnionTape`, new `BackendShape`, new BIR variant, new directive, and
public substrate API (`skinny/REDRESS.md:2661-2690`). Alpha-C carries the same
block forward and requires any future route to replace scalar rediscovery inside
one retained tape rather than adding a parallel sidecar
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-131`).

Alpha-E's retained candidate requires the class/event grammar to account for
container opens/closes, quote ownership, number/literal starts, keys, array
values, and nesting without a second tape, sidecar, `UnionTape`, new
`BackendShape`, BIR variant, directive, or public substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:185-211`).
The folded contract repeats the same pre-block list, including sidecar substrate,
parser-owned cursor/fact slots, `UnionTape`, new `BackendShape`, new
directive/BIR, public substrate API, and `tape_vs_tape` as production consumer
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-329`).

Required fold: none.

### CH5-6 - Sidecar and comparator evidence cannot become proxy performance claims

Disposition: ACCEPT.

Alpha-B keeps competitor deltas as planning evidence unless the row is
strict-vs-strict, same-run, same output plane, and measured inside the bbnf row.
It rejects lossy sonic, historical sidecars, absent sidecars, and parse DOM versus
borrowed-view mismatches as strict wins
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:121-149`).
The folded strict comparator gate rejects strict admission for deferred
strictness, stale sidecar-only evidence, lossy/permissive comparators,
output-plane mismatch, missing validation, missing sample cost, or missing hot
leaf (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

Alpha-E's comparator manifest candidate is gate-only evidence ingestion: DOM
sidecars cannot admit digest or typed-direct rows, sidecar evidence cannot act as
producer/substrate/row output/retained tape source/strict shortcut, and parser,
scanner, generated throughput cells must not move without a separate behavior
wave (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:374-394`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:405-414`).
The SK-V9-open telemetry refresh candidate is also behavior-frozen and cannot
change parser, scanner, SIMD, asm, codegen, generated output, product behavior,
or row throughput (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:416-493`).

Required fold: none.

### CH5-7 - G-Alpha boundary prevents hidden implementation dispatch

Disposition: ACCEPT.

`SYNTHESIS.md` states that the Alpha output does not create `SPEC.md` or
`DISPATCH-PROMPT.md`, and that skinny passes can begin only after alpha challenge
convergence, G-Alpha presentation, and `G-Alpha closed`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:61-75`). `HANDOFF.md` repeats that
no SK-V9 implementation is dispatched and that downstream S-P3 authors the
future wave plan only after G-Alpha and its own entry conditions
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`). Alpha-F carries the same
boundary and instructs challenge to reject pre-G-Alpha implementation dispatch
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:5-13`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:87-105`).

Required fold: none.

## Required Folds

None from CH5. The folded packet satisfies the hidden-coupling lane after V1
fold verification.

## Blockers To G-Alpha

None from CH5.

G-Alpha remains gated by the full V2 hardening/consolidation result and the
mandatory user sign-off. This CH5 ACCEPT does not waive other lenses, does not
authorize `SPEC.md` or `DISPATCH-PROMPT.md`, and does not dispatch SK-V9
implementation.
