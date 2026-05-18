# SK-V9 Alpha Hardening V3 - CH2 Generality

Date: 2026-05-18.
Lens: CH2 Generality.
Reviewed commit: `32369fe8` (`docs(sk-v9-alpha): fold V2 citation hardening`).
Verdict: ACCEPT.
Confidence: 97%.

## Read Set

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Verdict

The corrected packet is CH2-clean for G-Alpha presentation. V1 required folds for
Lock 14 explicitness, grammar-aware telemetry, sidecar evidence-only scope, and
candidate-local strictness / `parse_utf8` / `escape_complete` boundaries
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:47-60`).
V2 accepted CH2 at 96% and required only a CH1 citation fold
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:14-22`,
`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:24-27`).
Commit `32369fe8` changes the citation range only; the CH2 folds remain intact.

No new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate API,
sidecar substrate, or sidecar producer is authorized. The packet remains an Alpha
contract, not a SK-V9 implementation dispatch.

## Findings

### F1 - Lock 14 grammar-neutrality holds

Disposition: ACCEPT.

The CH2 lane must enforce Lock 14 and non-JSON applicability, not merely JSON
skinny convenience (`restart/prompts/ORCHESTRATOR.md:83-84`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:37-40`). The folded SK-V9
synthesis now requires a Lock 14 proof for generic CostFacts, codegen, runtime,
SIMD, tape, parser-template, report, and gate surfaces
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:122-126`). The proof must include
public API, grammar branch, primitive/table, role/fact, and template/provider
checks, plus CSS L4 / Sheets / BBNF-self non-JSON proof or per-grammar demotion
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-140`). The retained
class/event grammar remains per-grammar/generated unless that proof promotes it
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:142-143`).

Alpha-E repeats the candidate-local generic-surface proof requirement
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:47-50`).
HANDOFF routes readers through `SYNTHESIS.md` and separately pre-blocks generic
JSON policy leaks or Lock 14 weakening
(`restart/skinny/tranches/sk-v9/HANDOFF.md:12-14`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:95-99`).

### F2 - No new directive, BIR, or substrate is admitted

Disposition: ACCEPT.

The orchestrator non-negotiables forbid new BBNF directives, new BIR variants,
new substrate, and generic-crate JSON policy (`restart/prompts/ORCHESTRATOR.md:201-204`).
The SK-V9 Lock 14 scan blocks public substrate API, directive, BIR variant,
`BackendShape`, `UnionTape`, and grammar-specific role leakage
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-129`). The pre-block list also
forbids sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new
`BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape` as a
production consumer (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:324-326`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:95-97`).

Alpha-C carries the same no-substrate/no-directive ceiling for REDRESS 92 and
SC-6-L1-R1 (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-129`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:233-264`).
Alpha-E's retained proof gate likewise forbids a second tape, sidecar,
`UnionTape`, `BackendShape`, BIR variant, directive, or public substrate API
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:187-191`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:229-232`).

### F3 - Comparator telemetry is grammar-aware

Disposition: ACCEPT.

The telemetry table states that `gate-json` is the JSON instance of a
grammar-aware report contract and that generic report/gate code must not encode
JSON comparator policy as the universal schema
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-248`). Required fields now
include `grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
`comparator_strictness`, `comparator_freshness`, and
`measured_validation_path`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:252-290`).

Alpha-B preserves the strict-vs-strict, same-run, matching-plane discipline and
demotes lossy, historical, stale, absent, and plane-mismatched comparator values
to planning/flaw-probe evidence (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:139-149`).
Alpha-E requires every candidate gate to render strictness, UTF-8 boundary,
escape completion, output plane, comparator identity, comparator strictness,
freshness, and measured validation path
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-46`).

### F4 - Sidecar manifest scope is evidence-only

Disposition: ACCEPT.

The synthesis marks the comparator sidecar manifest as a gate prerequisite that
cannot produce parser data, retained tape data, row output, substrate, or strict
admission by itself (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`). The
strict comparator gate rejects stale sidecar-only evidence and output-plane
mismatch for strict admission (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222-240`).

Alpha-E repeats the evidence-only boundary: the sidecar candidate is gate-only
evidence ingestion (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:335-336`),
may support admission only with measured bbnf validation, same-run freshness,
same output plane, and gate-consumed strictness-declared comparator evidence
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-372`),
and cannot act as producer, substrate, row-output source, retained-tape source,
or strict shortcut (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:381-384`).

### F5 - Candidate-local strictness / UTF-8 / escape gates hold

Disposition: ACCEPT.

The current measured row table remains `Strictness=deferred`,
`parse_utf8=view-boundary`, and `escape_complete=yes`
(`skinny/RESULTS.md:3-42`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:50-52`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:26-28`).
The folded synthesis requires those fields in telemetry
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:258-260`) and rejects strict
admission from deferred strictness, stale sidecars, lossy/permissive comparators,
output-plane mismatch, missing validation, missing cost, or missing hot-leaf
attribution (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222-226`).

Alpha-E applies the local gate requested by V1: no candidate may present a new
row as strict SOTA admission while bbnf-side validation remains
`Strictness=deferred` or `parse_utf8=view-boundary`, and every candidate gate
must render `Strictness`, `parse_utf8`, and `escape_complete`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-46`).
Typed, retained, direct, and telemetry-refresh candidates repeat that boundary
locally (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:109-115`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:208-211`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:299-302`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:473-475`).

## Required Folds

None for CH2.

## Blockers To G-Alpha

No CH2 blocker remains. G-Alpha is still procedurally blocked until the full V3
Alpha challenge converges with zero open critical defects, no orphan REVISE, and
user sign-off per the Pass Alpha gate
(`restart/prompts/pass-contracts/PASS-ALPHA.md:167-178`;
`restart/prompts/pass-contracts/PASS-ALPHA.md:180-182`).
