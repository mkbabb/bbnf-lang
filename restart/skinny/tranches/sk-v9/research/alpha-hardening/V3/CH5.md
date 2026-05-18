# CH5 - Hidden Coupling Review

Verdict: ACCEPT

Confidence: 97%

## Scope

This V3 CH5 review audits the corrected SK-V9 Alpha packet at commit
`32369fe8` after the V2 citation fold. The lane checks hidden coupling,
source/product typed ambiguity, direct-to-struct versus real-typed laundering,
Track 1/Track 2 honesty, sidecar evidence-only scope, and proxy-performance
claims.

## Findings

### CH5-1 - V2 citation fold does not change CH5 coupling posture

Disposition: ACCEPT.

V2 consolidated left CH5 accepted at 97% and named only one remaining blocker:
Alpha-B through Alpha-F used incomplete complete-table citations
(`skinny/RESULTS.md:3-40`) while the main table ends at line 42
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:12-22`).
The corrected packet now cites `skinny/RESULTS.md:3-42` in Alpha-B through
Alpha-F (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:23-40`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:22-43`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-36`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:27-40`).
This fold is citation-only and does not authorize substrate, sidecar, row, or
implementation movement.

Required fold: none.

### CH5-2 - Typed source/product rows are not treated as measured rows

Disposition: ACCEPT.

Alpha-A keeps the measured `real_typed_struct A / GO` count at four and states
that Apache/CITM are W2 source/product parity only, not SK-V8 measured
`RESULTS.md` rows (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:66-73`).
Alpha-D repeats that Apache/CITM are not measured rows and that current rows
still carry `Strictness=deferred` and `parse_utf8=view-boundary`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:34-43`).
REDRESS 91 confirms the source/product slice added no directive, BIR variant,
`BackendShape`, substrate surface, sidecar, parser-owned cursor, runtime JSON
behavior, direct digest product claim, or measured row-table admission
(`skinny/REDRESS.md:2622-2659`).

The contract preserves this distinction: Apache/CITM measured-row admission
requires fresh run-id/metadata evidence, generated Track 1 DirectBuild,
independent serde/oracle proof, sonic parity lane, and row rendering; SK-V8
source/product parity cannot count as measured progress
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:81-85`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`). Alpha-E also blocks
source-only admission and keeps Canada routed until a full-fixture checksum
proof exists (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:92-115`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:128-135`).

Required fold: none.

### CH5-3 - Direct digest evidence is not laundered into real typed proof

Disposition: ACCEPT.

The current result table keeps planes separate: direct-to-struct rows are
`digest`, while real typed rows are `typed direct`
(`skinny/RESULTS.md:6-9`, `skinny/RESULTS.md:18-21`,
`skinny/RESULTS.md:27-28`, `skinny/RESULTS.md:38`). Alpha-A states that the
three current direct GO rows are digest-plane guard rows, not product-plane
typed rows (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:75-90`).
REDRESS 93 keeps remaining direct misses routed to a direct output contract or
control-path tranche and says digest evidence remains guard-plane only
(`skinny/REDRESS.md:2694-2729`).

The folded SK-V9 contract forbids treating direct digest as product proof
without a direct output contract or control-path tranche
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:86-90`). Alpha-E requires
guard/control-path labeling when the measured plane remains `digest`, requires a
distinct control-row identity if control work is retained, and says a
digest-only row cannot satisfy the typed-product escape hatch
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:278-302`).
Alpha-F tells challenge to reject any draft that treats direct digest as product
proof (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:87-101`).

Required fold: none.

### CH5-4 - Track 1 and Track 2 remain structurally honest

Disposition: ACCEPT.

The measured authority says Track 1 is `runtime::generated_json::parse`, while
Track 2 is the independent hand-coded parser over `runtime::tape`; the signed
Track 2 checklist says it uses `runtime::tape::TapeBuilder`, shares the same
parity oracle, and never calls `runtime::generated_json::parse`
(`skinny/RESULTS.md:138-141`). Typed rows describe Track 2 as a structural
oracle rather than the SOTA speed gate (`skinny/RESULTS.md:7`,
`skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, `skinny/RESULTS.md:28`).

Alpha-E keeps the split. Typed row-table admission uses generated Track 1,
serde_json Track 2/oracle, and sonic checksum parity, while Track 2 remains a
structural oracle rather than a SOTA speed floor
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:72-115`).
Direct work must not couple Track 2 to generated SinkOnly or generated Track 1
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:258-276`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:315-324`).
The SK-V9 telemetry schema requires both Track 2 Mbps and Track 2 independence
status (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:263-264`,
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:291-295`).

Required fold: none.

### CH5-5 - No parallel substrate, renamed scanner, or sidecar producer is authorized

Disposition: ACCEPT.

REDRESS 92 rejects the W3 structural-projection route because scanner structural
positions and retained tape events are not isomorphic, and it blocks sidecar
producers, parser-owned structural cursors/facts, `tape_vs_tape` as production
consumer, `UnionTape`, new `BackendShape`, new BIR variant, new directive, and
public substrate API (`skinny/REDRESS.md:2661-2690`). Alpha-C carries this block
forward and requires any future route to replace scalar rediscovery inside one
retained tape rather than adding a parallel sidecar
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-131`).

Alpha-E's retained candidate requires the class/event grammar to account for
container opens/closes, quote ownership, number/literal starts, keys, array
values, and nesting without a second tape, sidecar, `UnionTape`, new
`BackendShape`, BIR variant, directive, or public substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:185-211`).
The folded contract repeats the same pre-block list, including sidecar
substrate, parser-owned cursor/fact slots, `UnionTape`, new `BackendShape`, new
directive/BIR, public substrate API, and `tape_vs_tape` as production consumer
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-329`).

Required fold: none.

### CH5-6 - Sidecar and comparator evidence cannot become proxy performance claims

Disposition: ACCEPT.

Alpha-B keeps competitor deltas as planning evidence unless the row is
strict-vs-strict, same-run, same output plane, and measured inside the bbnf row.
It rejects lossy sonic, historical sidecars, absent sidecars, and parse DOM
versus borrowed-view mismatches as strict wins
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:121-149`).
The folded strict comparator gate rejects strict admission for deferred
strictness, stale sidecar-only evidence, lossy/permissive comparators,
output-plane mismatch, missing validation, missing sample cost, or missing hot
leaf (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

Alpha-E's comparator manifest candidate is gate-only evidence ingestion: DOM
sidecars cannot admit digest or typed-direct rows, sidecar evidence cannot act
as producer/substrate/row output/retained tape source/strict shortcut, and
parser, scanner, generated throughput cells must not move without a separate
behavior wave (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:374-394`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:405-414`).
The SK-V9-open telemetry refresh candidate is also behavior-frozen and cannot
change parser, scanner, SIMD, asm, codegen, generated output, product behavior,
or row throughput (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:416-493`).

Required fold: none.

### CH5-7 - G-Alpha boundary prevents hidden implementation dispatch

Disposition: ACCEPT.

`SYNTHESIS.md` states that the Alpha output does not create `SPEC.md` or
`DISPATCH-PROMPT.md`, and that skinny passes can begin only after alpha
challenge convergence, G-Alpha presentation, and `G-Alpha closed`
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

None from CH5. The corrected V3 target satisfies the hidden-coupling lane after
the V2 citation fold and the V1 scope, Lock 14, regression, and cost folds.

## Blockers To G-Alpha

None from CH5.

G-Alpha remains gated by full V3 hardening convergence, consolidated acceptance,
and mandatory user sign-off. This CH5 ACCEPT does not authorize `SPEC.md`,
`DISPATCH-PROMPT.md`, or SK-V9 implementation dispatch.
