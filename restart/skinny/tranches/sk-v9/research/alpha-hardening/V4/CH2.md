# SK-V9 Alpha Hardening V4 - CH2 Generality

Date: 2026-05-18.
Lens: CH2 Generality.
Reviewed commit: `795bbbec` (`docs(sk-v9-alpha): record V3 accept convergence cycle`).
Verdict: ACCEPT.
Confidence: 97%.

## Read Set

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md`
- `skinny/RESULTS.md`

## Verdict

The unchanged V4 re-challenge is CH2-clean. The current HEAD is `795bbbec`;
the diff from the V3-reviewed packet at `32369fe8` adds only V3 hardening
records, and excluding `restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/`
there is no packet diff. V3 already accepted CH2 at 97% with no fold required
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:14-21`)
and explicitly required this V4 unchanged re-challenge before G-Alpha
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:34-38`).

No Lock 14 drift, grammar-specific genericization, new directive, BIR variant,
substrate, sidecar producer, or SK-V9 implementation dispatch is present in the
reviewed packet.

## Findings

### F1 - Unchanged Packet / No Drift

Disposition: ACCEPT.

`795bbbec` is a review-record commit over the corrected packet. V3 scoped its
accepted challenge to the corrected packet after `32369fe8`
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:5-7`)
and records 6/6 ACCEPT, minimum confidence 96%, no critical defect, and no
orphan REVISE
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:21-22`).
The V3 consolidated evidence also records that no SK-V9 `SPEC.md` or
`DISPATCH-PROMPT.md` exists
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:24-32`).

Local diff checks for this V4 lane found no changes outside the V3 hardening
artifacts and no `SPEC.md` / `DISPATCH-PROMPT.md` at
`restart/skinny/tranches/sk-v9/`.

### F2 - Lock 14 Grammar-Neutrality Holds

Disposition: ACCEPT.

CH2 is explicitly the Lock 14 / grammar-neutrality lens
(`restart/prompts/ORCHESTRATOR.md:83-84`), and the non-negotiables forbid new
directives, new BIR, new substrate, and JSON code in generic crates
(`restart/prompts/ORCHESTRATOR.md:197-204`). The SK-V9 packet carries the
required Alpha Generality and Lock 14 gate: any candidate touching generic
CostFacts, codegen, runtime, SIMD, tape, parser-template, report, or gate
surfaces must prove public API, grammar branch, primitive/table, role/fact,
template/provider, and non-JSON coverage for CSS L4 / Sheets / BBNF-self
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:122-140`).

The retained class/event grammar remains per-grammar/generated unless that
non-JSON proof promotes it
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:142-143`). Alpha-E repeats the
candidate-local Lock 14 proof requirement for any generic surface edit
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:47-50`).

### F3 - No New Directive, BIR, Or Substrate Surface

Disposition: ACCEPT.

The synthesis proof explicitly bars a new public substrate API, directive, BIR
variant, `BackendShape`, `UnionTape`, or grammar-specific role leakage
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-129`). The pre-block list also
bars sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new
`BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape` as a
production consumer
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-328`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:95-99`).

Alpha-E's retained grammar gate repeats the same ceiling: no second tape,
sidecar, `UnionTape`, new `BackendShape`, BIR variant, directive, or public
substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:185-191`),
and its notes forbid parser-owned structural facts, aux tables, sidecar
substrates, `UnionTape`, a second tape, or renamed side substrate
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:225-232`).

### F4 - Comparator Telemetry Is Grammar-Aware

Disposition: ACCEPT.

The strict comparator gate requires comparator Mbps, percent delta, strictness
plane, output plane, freshness, and hot leaf, and rejects strict admission from
deferred strictness, stale sidecar-only evidence, lossy/permissive comparators,
output-plane mismatch, missing measured validation, missing cost, or missing
hot-leaf attribution
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`). The telemetry binding
states that `gate-json` is only the JSON instance of a grammar-aware report
contract and that generic report/gate code must not encode JSON comparator
policy as the universal schema
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-248`).

Required telemetry now includes `grammar_id`, `domain`, `Strictness`,
`parse_utf8`, `escape_complete`, output plane, comparator identity, comparator
plane, comparator strictness, comparator freshness, and measured validation path
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:250-290`). Alpha-B preserves the
strict-vs-strict, same-run, matching-plane discipline and demotes deferred,
lossy, historical, absent, or plane-mismatched evidence to planning/flaw-probe
status
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:139-149`).

### F5 - Sidecar Scope Is Evidence-Only

Disposition: ACCEPT.

The comparator sidecar manifest is a gate-only prerequisite and cannot produce
parser data, retained tape data, row output, substrate, or strict admission by
itself
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:39-51`). Alpha-E gives the same
boundary: the sidecar candidate is gate-only evidence ingestion, not parser
data, retained tape data, row output, substrate, or strict admission
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:326-336`).

Its same-wave consumer and falsifiability gates allow admission support only
when the row also carries measured bbnf validation, same-run freshness, same
output plane, and strictness-declared comparator evidence consumed by the gate;
sidecar evidence cannot act as a producer, substrate, row-output source,
retained-tape source, or strict shortcut
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-384`).

### F6 - Candidate-Local Strictness / UTF-8 / Escape Gates Hold

Disposition: ACCEPT.

The current main table rows remain `Strictness=deferred`,
`parse_utf8=view-boundary`, and `escape_complete=yes`
(`skinny/RESULTS.md:3-42`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:50-52`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:26-28`).
The SK-V9 telemetry schema requires those fields
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:258-260`), and the strict comparator
gate rejects strict admission from deferred strictness, stale sidecars,
lossy/permissive comparators, output-plane mismatch, missing validation, missing
cost, or missing hot leaf
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222-226`).

Alpha-E makes the boundary local to candidates: no candidate may present a new
row as strict SOTA admission while bbnf-side validation remains
`Strictness=deferred` or `parse_utf8=view-boundary`, and every candidate gate
must render `Strictness`, `parse_utf8`, `escape_complete`, output plane,
comparator id, comparator strictness, comparator freshness, and measured
validation path
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-46`).
The typed, retained, direct, and telemetry-refresh candidates repeat that
boundary locally
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:109-115`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:208-211`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:299-302`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:473-475`).

### F7 - No SK-V9 Implementation Dispatch

Disposition: ACCEPT.

The synthesis says this contract is not an implementation dispatch and that only
after `G-Alpha closed` may the skinny pass sequence begin
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:61-75`). Its Alpha scope matrix
binds candidates for G-Alpha without creating a SK-V9 `SPEC.md`,
`DISPATCH-PROMPT.md`, or wave dispatch
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:106-120`). The G-Alpha boundary
repeats that V9 implementation is not dispatched by the document
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-334`).

HANDOFF matches that posture: V9 implementation is not dispatched, no
`SPEC.md` or `DISPATCH-PROMPT.md` exists, G-Alpha must be presented after
challenge convergence, and downstream S-P3 owns any future detailed wave plan
(`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`). Alpha-E also states that
it dispatches no SK-V9 implementation waves and changes no source, generated
output, benchmark data, `skinny/RESULTS.md`, or `skinny/REDRESS.md`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:5-7`).

## Required Folds

None for CH2.

## Blockers To G-Alpha

No CH2 blocker remains.

Procedurally, G-Alpha still requires full V4 challenge consolidation at >=95%
ACCEPT with zero open critical defects and no orphan REVISE before user sign-off
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`;
`restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`).
