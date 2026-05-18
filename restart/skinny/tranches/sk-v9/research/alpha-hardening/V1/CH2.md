# CH2 Generality Challenge - SK-V9 Alpha V1

Date: 2026-05-18.
Lens: CH2 Generality.
Verdict: REVISE.
Confidence: 88%.

## Read Set

- `restart/prompts/ORCHESTRATOR.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`

## Verdict

The SK-V9 Alpha packet is directionally sound and does not contain a hard
directive, BIR, or substrate admission. It also keeps the current SK-V8
`Strictness=deferred`, `parse_utf8=view-boundary`, and sidecar/planning-signal
boundaries visible.

It is not yet CH2-clean for G-Alpha. Pass Alpha requires the next contract to
respect Lock 14 and remain valid for CSS L4, Sheets, and BBNF-self, not merely
for JSON skinny rows (`restart/prompts/pass-contracts/PASS-ALPHA.md:37-40`).
The downstream S-P3 prompt also requires a SPEC generality gate with non-JSON
proof for every generic-crate edit (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116-120`).
SK-V9 Alpha names candidates that touch generic or semi-generic surfaces, but
the current contract only carries broad "no generic JSON policy" language and
does not fold the concrete SK-V8 Section 2.1 gate forward.

Disposition: REVISE, with four required folds before G-Alpha presentation.

## Findings

### F1 - Lock 14 gate is too implicit for the structural proof candidate

Disposition: REVISE.

The retained class/event proof candidate names generic or cross-grammar owner
surfaces: `bbnf-simd`, generic `runtime/src/tape/*`, JSON runtime modules,
codegen templates, parity/materialization, and gate code
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:125-143`).
Its proof gate is written in JSON terms: object keys, array values, string quote
ownership, numbers, literals, and JSON fixtures
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:166-179`).
The only Lock 14 language in that candidate is the broad sentence "Do not hide
JSON-specific policy in generic crates"
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:202-207`).

That is weaker than the accepted SK-V8 gate, which required public API scans,
grammar-branch scans, primitive/table scans, role/fact boundary checks,
template/provider boundary checks, and CSS L4 / Sheets / BBNF-self proof for
generic CostFacts, codegen, runtime, SIMD, or parser-template edits
(`restart/skinny/tranches/sk-v8/SPEC.md:261-286`). SK-V9 SYNTHESIS only says
Omega may not weaken Lock 14 (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:44-47`)
and pre-blocks generic JSON policy (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:254-258`);
it does not make those checks part of the Alpha goalset.

Required fold: add a SK-V9 Alpha generality gate equivalent to SK-V8 SPEC
Section 2.1. Any candidate touching generic CostFacts, codegen, runtime, SIMD,
tape, parser-template, report, or gate surfaces must prove CSS L4, Sheets, and
BBNF-self compile/lower/cost/run without JSON structural roles. The retained
class/event grammar must be explicitly per-grammar/generated unless and until a
non-JSON proof promotes it to a generic abstraction.

### F2 - Telemetry schema drops the grammar-aware comparator registry

Disposition: REVISE.

Pass Alpha says the next contract's telemetry binding must include competitor
deltas plus future grammar-domain comparators for CSS / Sheets / BBNF-self
(`restart/prompts/pass-contracts/PASS-ALPHA.md:3`). Alpha-B correctly records
that JSON C++ comparators are historical sidecar planning signals and that
strict admission requires strict-vs-strict, same-run, matching-plane validation
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:31-39`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:136-144`).

The SK-V9 telemetry table, however, reverts to fixed JSON comparator columns and
omits the grammar-aware fields that SK-V8 had already made required:
`grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
`comparator_strictness`, `comparator_freshness`, and
`measured_validation_path` (`restart/skinny/tranches/sk-v8/SPEC.md:110-120`).
SK-V9's schema lists corpus/workload, strictness, `parse_utf8`,
`escape_complete`, fixed comparator Mbps columns, sidecar freshness, substrate
surface, and Track 2 status (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:191-235`),
but it does not say that `gate-json` is only the JSON instance of a
grammar-aware report contract.

Required fold: restore the grammar-aware comparator registry or equivalent
fields in SK-V9 SYNTHESIS Section 4.3 and handoff language. Rendered JSON
columns may remain, but generic report/gate code must not encode JSON comparator
policy as the universal schema. CSS, Sheets, and BBNF-self comparator anchors
must be domain-specific or explicitly absent.

### F3 - Sidecar manifest candidate leaves a comparator loophole

Disposition: REVISE.

The packet mostly keeps sidecars fail-closed: Alpha-A says C++ comparator values
are historical sidecar planning signals and that no structured sidecar same-run
manifest exists yet (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:138-143`);
Alpha-B says sidecar and historical values are planning only
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:123-132`);
SYNTHESIS rejects strict admission from stale sidecar-only evidence
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:171-175`).

The shortlist then admits an "optional new sidecar manifest module path" and an
optional checked-in sidecar manifest/provenance path under `skinny/`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:302-314`).
It requires manifest fields and a plane/strictness gate
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:327-350`),
but the language does not explicitly forbid that sidecar path from becoming a
producer, substrate, or substitute strict anchor. This is a CH2/CH5 boundary
because the orchestrator forbids sidecar producers and substrate forks
(`restart/prompts/ORCHESTRATOR.md:83-88`) and SK-V8 globally blocked sidecar
substrates and sidecar evidence as strict admission
(`restart/skinny/tranches/sk-v8/SPEC.md:191-205`).

Required fold: constrain Candidate 4 to evidence ingestion only. A sidecar
manifest may feed report/gate freshness fields, but it must not produce parser
data, retained tape data, row output, substrate, or strict admission by itself.
Sidecar evidence can support admission only when same-run, same-plane,
strictness-declared, gate-consumed, and paired with measured bbnf validation in
the row. DOM sidecars still cannot admit digest or typed-direct rows.

### F4 - Directive, BIR, and substrate claims are blocked

Disposition: ACCEPT.

The alpha packet preserves the hard architectural prohibitions. Alpha-C
pre-blocks sidecar producers, parser-owned structural cursors/facts, aux
density tables, `EventCursor` variants, `tape_vs_tape` as production consumer,
`UnionTape`, new `BackendShape`, new BIR variant, new directive, and public
substrate API (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:109-129`).
Alpha-D repeats that W3 may not reopen as a storage-only swap, sidecar,
`UnionTape`, parser-owned cursor, `tape_vs_tape`, new `BackendShape`, new BIR
variant, or new directive
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:270-277`).
SYNTHESIS and HANDOFF carry the same pre-blocks
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-258`,
`restart/skinny/tranches/sk-v9/HANDOFF.md:64-82`).

No required fold beyond preserving these blocks while addressing F1-F3.

### F5 - Strictness, `parse_utf8`, and escape boundaries mostly hold

Disposition: ACCEPT with hardening.

The packet correctly records that every current main row remains
`Strictness=deferred`, `parse_utf8=view-boundary`, and `escape_complete=yes`
(`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:50-52`,
`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:25-39`).
It demotes parse rows to substrate-guard telemetry rather than strict SOTA
admissions (`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:212-218`)
and says strict admission rejects `Strictness=deferred`, lossy/permissive
comparators, stale sidecars, output-plane mismatch, missing validation, missing
c/B or sample cost, and missing hot-leaf attribution
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:169-175`).
The telemetry schema preserves `Strictness`, `parse_utf8`, and
`escape_complete` as required fields
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:196-205`).

Required hardening fold: candidate-specific gates in Alpha-E should repeat that
no new row can become GO/SOTA-admission while bbnf-side validation remains
`Strictness=deferred` or `parse_utf8=view-boundary`, unless the same accepted
wave changes the measured validation path and gate semantics. This prevents
Candidate 1 typed admission, Candidate 4 sidecar refresh, or Candidate 5
telemetry refresh from relying on the global SYNTHESIS paragraph while leaving a
looser local gate.

## Required Folds Before G-Alpha

1. Add a concrete SK-V9 Alpha generality and Lock 14 gate to SYNTHESIS/HANDOFF
   and Alpha-E candidate language, equivalent to SK-V8 SPEC Section 2.1:
   public API scan, grammar branch scan, primitive/table scan, role/fact
   boundary, template/provider boundary, and CSS L4 / Sheets / BBNF-self
   non-JSON proof for generic edits.
2. Restore grammar-aware telemetry/comparator fields in Section 4.3:
   `grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
   `comparator_strictness`, `comparator_freshness`, and
   `measured_validation_path`, or an equivalent registry consumed by the gate.
3. Tighten the sidecar manifest candidate so it is evidence-only and cannot be
   read as a sidecar producer, sidecar substrate, or strict comparator shortcut.
4. Repeat strictness / `parse_utf8` / `escape_complete` admission boundaries in
   Alpha-E candidate gates, not only in the global SYNTHESIS schema.

## Blockers To G-Alpha

G-Alpha is blocked on CH2 until the four folds above are applied and
re-challenged. There is no blocker from a present new directive, BIR variant, or
substrate claim; the blocker is missing explicit generality enforcement around
generic owner paths, comparator telemetry, and sidecar evidence.
