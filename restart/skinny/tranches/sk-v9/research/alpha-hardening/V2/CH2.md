# SK-V9 Alpha Hardening V2 - CH2 Generality

Date: 2026-05-18.
Lens: CH2 Generality.
Reviewed commit: `e3ebe0b4` (`docs(sk-v9-alpha): fold V1 hardening revisions`).
Verdict: ACCEPT.
Confidence: 96%.

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
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

## Verdict

The folded packet satisfies CH2 for G-Alpha presentation. The V1 CH2 blockers
were Lock 14 explicitness, grammar-aware telemetry, sidecar evidence-only scope,
and candidate-local strictness / `parse_utf8` / `escape_complete` gates
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:47-60`).
Those folds are present in the current packet.

No new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate API,
sidecar substrate, or sidecar producer is authorized by the Alpha packet.
SK-V9 remains pre-dispatch until G-Alpha and downstream S-P3 convergence.

## Findings

### F1 - Lock 14 and grammar-neutrality gate is now explicit

Disposition: ACCEPT.

`SYNTHESIS.md` now requires a Lock 14 proof for any candidate touching generic
CostFacts, codegen, runtime, SIMD, tape, parser-template, report, or gate
surfaces (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:122-126`). The required
proof includes public API scan, grammar branch scan, primitive/table scan,
role/fact boundary check, template/provider boundary check, and CSS L4 / Sheets /
BBNF-self non-JSON proof (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-140`).
It also keeps the retained class/event grammar per-grammar/generated unless a
non-JSON proof promotes it (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:142-143`).

Alpha-E repeats the candidate-local Lock 14 proof requirement for generic owner
paths (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:47-50`).
HANDOFF is a routing/read-first surface rather than the full normative gate, but
it links `SYNTHESIS.md` in the required read order and pre-blocks generic JSON
policy leaks or Lock 14 weakening (`restart/skinny/tranches/sk-v9/HANDOFF.md:12-14`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:95-99`). This satisfies the V1 fold.

### F2 - No new directive, BIR, or substrate is admitted

Disposition: ACCEPT.

The folded synthesis blocks public substrate API, directives, BIR variants,
`BackendShape`, `UnionTape`, and grammar-specific role leakage in the Lock 14
scan (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:128-129`). The pre-block list
also rejects sidecar substrates, parser-owned cursor/fact slots, `UnionTape`, new
`BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape` as a
production consumer (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:324-326`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:95-97`).

Alpha-C carries the same substrate ceiling in the REDRESS 92 and SC-6-L1-R1
blocks (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:111-129`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:233-264`).
Alpha-E's retained class/event candidate forbids a second tape, sidecar,
`UnionTape`, `BackendShape`, BIR variant, directive, or public substrate API in
the proof gate (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:187-191`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:229-232`).

### F3 - Comparator telemetry is grammar-aware

Disposition: ACCEPT.

The folded telemetry binding states that `gate-json` is the JSON instance of the
grammar-aware report contract, and that generic report/gate code must not encode
JSON comparator policy as the universal schema
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-248`). The required telemetry
now includes `grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
`comparator_strictness`, `comparator_freshness`, and
`measured_validation_path`
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:252-290`).

Alpha-B preserves strict-vs-strict, same-run, matching-plane admission discipline
and names sidecar/historical values as planning evidence only
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:20-44`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:139-149`).
Alpha-E requires every candidate gate to render strictness, UTF-8 boundary,
escape completion, output plane, comparator identity, comparator strictness,
freshness, and validation path
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-46`).

### F4 - Sidecar scope is evidence-only

Disposition: ACCEPT.

The synthesis separates the comparator sidecar manifest as a gate-only
prerequisite and states that it cannot produce parser data, retained tape data,
row output, substrate, or strict admission by itself
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:47-52`). The strict comparator gate
rejects strict admission from stale sidecar-only evidence and output-plane
mismatch (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222-240`).

Alpha-E applies the same scope to the sidecar candidate. It is "gate-only
evidence ingestion" (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:335-336`);
manifest ingestion may support admission only when the row also carries measured
bbnf validation, same-run freshness, same output plane, and gate-consumed
strictness-declared comparator evidence
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:363-372`).
The `sidecar_evidence_only` gate forbids producer, substrate, row-output,
retained-tape-source, and strict-shortcut interpretations
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:381-384`).

### F5 - Candidate-local strictness, UTF-8, and escape gates hold

Disposition: ACCEPT.

Current SK-V8 rows remain `Strictness=deferred`, `parse_utf8=view-boundary`, and
`escape_complete=yes` (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:50-52`;
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:26-28`).
The folded synthesis requires those fields in SK-V9 telemetry
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:258-260`) and rejects strict
admission from deferred strictness, stale sidecars, lossy/permissive comparators,
output-plane mismatch, missing validation, missing cost, or missing hot-leaf
attribution (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:222-226`).

Alpha-E adds the local guard requested by V1: no candidate may present a new row
as strict SOTA admission while bbnf-side validation remains
`Strictness=deferred` or `parse_utf8=view-boundary`, and every candidate gate must
render `Strictness`, `parse_utf8`, and `escape_complete`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:38-46`).
The typed, retained, direct, and telemetry-refresh candidates each repeat the
strict-boundary rule in their local gates
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:109-115`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:208-211`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:299-302`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:473-475`).

## V1 Fold Verification

| V1 CH2 fold | V2 status |
|---|---|
| Concrete Lock 14 gate for generic surfaces and non-JSON proof | ACCEPT: present in `SYNTHESIS.md` and Alpha-E; HANDOFF read-first plus pre-block language preserves the gate. |
| Grammar-aware comparator telemetry | ACCEPT: `grammar_id`, `domain`, comparator registry fields, and validation path are required. |
| Sidecar manifest evidence-only scope | ACCEPT: sidecar candidate cannot be producer, substrate, row-output source, retained-tape source, or strict shortcut. |
| Candidate-local strictness / `parse_utf8` / `escape_complete` boundaries | ACCEPT: Alpha-E applies the boundary to every candidate and repeats it in row-moving gates. |
| No new directive/BIR/substrate | ACCEPT: all relevant surfaces preserve the prohibition. |

## Required Folds

None for CH2.

## Blockers To G-Alpha

No CH2 blocker remains. G-Alpha still requires full Alpha V2 challenge
convergence, consolidated zero open critical defects, no orphan REVISE, and user
sign-off per `restart/prompts/pass-contracts/PASS-ALPHA.md:167-178`.
