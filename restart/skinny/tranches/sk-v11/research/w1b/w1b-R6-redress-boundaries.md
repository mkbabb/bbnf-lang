# SK-V11 W1b Phase 1 Research - R6 REDRESS, Preblocks, and Budget Boundaries

Status: read-only research artifact.
Scope owner: W1b R6 REDRESS/preblocks, generated-baseline boundary, budget, and
revert protocol.
Owned path: `restart/skinny/tranches/sk-v11/research/w1b/w1b-R6-redress-boundaries.md`.
Source edits: none.

## Source Authority

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 5, plus Sections 0.3, 1,
  2.2, and 13 where they bind telemetry, generated-output provenance, Track
  2/oracle independence, and hard preblocks.
- `restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md`,
  especially the W1b per-wave ledger and global hard blocks.
- `skinny/REDRESS.md` recent entries, especially SK-V11 W1a item 111 and the
  carried REDRESS families 34, 35, 36, 37, 38, 48, 85, 86, 87, 100, 101, 109,
  and 110.
- W1a research, plan, challenge, and redress artifacts under
  `restart/skinny/tranches/sk-v11/research/w1a/`.
- P3-C/P3-D/P3-B and S-P3 CH4 hardening entries for W1b falsifiability,
  telemetry, sequencing, and cost feasibility.

## R6 Finding

W1b is a C9 accounting and generated-baseline wave. It may create exactly one
generated non-JSON direct or typed parser baseline row plus an independent
Track 2/oracle on the same output plane. It may render baseline throughput,
strict equality, provenance, and gate consumption. It must not land a parser
intervention, claim a behavior row admission, move a JSON `RESULTS.md` row, or
let W2 create the first measurable non-JSON baseline.

W1b can close as baseline-only without admission. That close is valid only as
`G-W1b-NONJSON-BASELINE`: one generated baseline exists, one independent
oracle/Track 2 exists, strict output equality passes, throughput/provenance is
gate-consumed, and no behavior admission is claimed. The baseline Mbps becomes
W2's seed. It is not a Lock 14 close by itself, not the required admitted
non-JSON intervention, and not SK-V11 close evidence.

## Preblocked Routes

| Route pressure | W1b preblock | Binding source |
|---|---|---|
| JSON-provider emission as generality proof | A generated JSON provider, renamed JSON helper, or JSON-only emitted parser cannot establish non-JSON generality. W1b must select `css_l4`, `sheets`, or `bbnf_self` and prove generated non-JSON output. | SPEC Section 5; P3-E W1b ledger; REDRESS 36-38, 85, 86 |
| Old hand non-JSON struct-direct modules | Legacy hand runtimes, witness tests, or non-generated struct-direct code cannot substitute for generated Track 1. | SPEC Section 5 preblocks; W1a CH5; P3-E W1b ledger |
| Coupled oracle | Track 2/oracle must not call generated Track 1, generated SinkOnly helpers, generated typed helpers, generated JSON, benchmark-private parser code, runtime witness paths, stale sidecars, or prose-only sources. | SPEC Sections 2.3 and 5; REDRESS 34, 35, 48; W1a CH5 revise closure |
| Behavior intervention | W1b may stand up a baseline harness only. Any primitive, dispatch, string, numeric, SIMD, output-sink, or codegen optimization belongs to W2 or later. | SPEC Section 5; P3-C `G-W1b-NONJSON-BASELINE` |
| Row admission from baseline creation | Baseline throughput is seed evidence, not `A / GO`, not a JSON row movement, and not a direct/typed product admission. | REDRESS 100, 101, 109; SPEC Section 5 exit gate |
| Documentation-only Lock 14 claim | Prose, status text, W1a schema presence, or an unbenchmarked generated parser cannot close non-JSON generality. | SPEC Section 5 preblocks; W1a CH6 |
| Producer-only telemetry | Every baseline field used as evidence must be consumed by the gate in the same wave. Unknown or display-only fields are failures. | REDRESS 87; W1a R5; P3-D |
| Hidden directives or substrate variants | W1b must not add a directive, BIR variant, new `BackendShape`, `UnionTape`, public substrate API, parser-owned fact slot, retained sidecar, structural-position vector, or alternate tape to make the baseline pass. | SPEC Section 13; P3-E global hard blocks |
| W3 substrate family | Union/event substrate, retained class columns, class-lane-only repair, streaming cursors, parser-owned structural projection, sidecars, and W4-through-W3 cascade lock remain closed. | REDRESS 92, 96, 97, 98, 102 |
| Generic JSON policy leakage | JSON role/policy must not enter `parse-that-regex`, `bbnf-simd`, IR, codegen-generic code, or runtime outside generated grammar-local modules. | SPEC Sections 1, 2.2, and 5; REDRESS 36-38, 85, 86 |
| First-baseline deferral to W2 | W2 may consume the W1b baseline but may not invent baseline and intervention together in one redress. | SPEC Sections 5 and 6; P3-B sequencing |

## Hard No-Go Edits

W1b redress must stop and return REVISE before source work, or revert if already
attempted, if closure needs any of these edits:

- More than one selected non-JSON target, workload, or generated parser row.
- Any W2-style behavior intervention, primitive wiring, SIMD/ASM production,
  parser optimization, or before/after improvement claim.
- `skinny/RESULTS.md` JSON row movement, direct/typed admission, parse-only
  SOTA claim, or SK-V11 close claim.
- A coupled Track 2/oracle, hidden benchmark-private parser, generated Track 1
  reuse, generated SinkOnly/typed helper reuse, generated JSON source reuse,
  runtime witness source, stale sidecar, or prose-only oracle.
- Generic JSON policy outside generated per-grammar modules.
- A new directive, BIR variant, public substrate API, retained sidecar,
  structural-position vector, alternate retained tape, `UnionTape`, or W3
  substrate repair route.
- Hand-patched generated output. Generated output may change only as
  regenerated output from the named selected grammar/schema inputs.
- A new broad `xtask` or global JSON validator relaxation when the W1a
  companion gate path can be extended narrowly.
- Any implementation that cannot fit the W1b hard budget without weakening
  equality, oracle independence, or gate consumption.

## Expected Tests And Evidence

W1b should carry forward the W1a companion gate and add baseline-specific proof.
Exact command names belong to the W1b plan, but the required evidence classes
are fixed:

1. Entry preservation:
   - W1a accepted pass fixture still passes through the gate.
   - W1a producer-only, coupled, shared-source, and admission/baseline claim
     fixtures still fail or have exact W1b-specific failing equivalents.
   - JSON preservation remains green:
     `gate-json --with-cost-facts --check-results`, no `skinny/RESULTS.md`
     diff, and no unintended codegen/runtime/SIMD drift.

2. Generated Track 1 baseline:
   - Exactly one selected target, preferably
     `css_l4/declaration_values/direct/main` unless CHALLENGE selects Sheets or
     BBNF-self.
   - Generated direct or typed parser output exists for that selected target.
   - The generated output is regenerated from named inputs and is not hand
     patched.
   - Baseline throughput is rendered with run id, host, flags, sample count,
     grammar id, workload, output plane, profile artifact, feature mask, and
     generated Track 1 Mbps.

3. Independent Track 2/oracle:
   - Oracle or Track 2 evidence is same-output-plane and strict.
   - Source provenance is gate-consumed separately from any independence status
     string.
   - Coupled, shared-source, generated-Track-1, generated-JSON, stale-sidecar,
     hand-runtime, benchmark-private parser, runtime-witness, and prose-only
     oracle cases fail.

4. Strict equality and gate consumption:
   - Generated Track 1 output equals the independent oracle/Track 2 output on
     the selected corpus.
   - The W1b gate consumes equality status, throughput, run/build/host facts,
     oracle identity, oracle plane, oracle freshness, source artifact, and
     consumer class.
   - Unknown non-JSON evidence fields reject.

5. Non-admission guard:
   - Any `A / GO`, behavior admission, W2 intervention, SK-V11 close,
     `baseline_authority` misused as admission, parse-only SOTA, or JSON row
     movement fixture fails.
   - W1b emits a baseline status only, not an admitted row.

## LOC And Generated Output Budget

SPEC caps W1b at `<=360` handwritten source/test/gate LOC, regenerated output
capped to selected generated parser inputs, and `<=90 min` redress. P3-B and
CH4 make the risk explicit: current generated-runtime/codegen paths are still
JSON-profile-gated, so W1b remains feasible only if the implementation is
narrow.

Recommended budget envelope for the future W1b plan:

- Gate/report extension and focused tests: largest slice, but additive to the
  W1a companion report path.
- Generated baseline harness and benchmark registration: exactly one selected
  direct or typed workload.
- Independent oracle/Track 2 adapter or fixture path: exactly one selected
  output plane, with negative coupling tests.
- Regenerated output: only files produced from the selected grammar/schema
  inputs. Do not count it as handwritten LOC, but do name every generated input
  and output in the plan.

Hard stop before implementation if the planned handwritten source/test/gate LOC
exceeds 360, if generated output expands beyond the selected target, or if the
baseline plus oracle plus gate consumption cannot be delivered without also
landing an intervention.

## Revert Protocol

The W1b revert unit is one slice:
codegen, runtime grammar output, bench harness, gate/report, oracle/Track 2,
and generated baseline changes for the selected target. Preserve the failed
proof in `skinny/REDRESS.md`.

Revert is mandatory if any of these occur:

- The generated Track 1 baseline row is absent or not actually generated.
- More than one target is added without a same-wave CHALLENGE revision.
- Track 2/oracle calls or reuses generated Track 1 or any other coupled source.
- Strict equality fails on the selected corpus.
- Baseline throughput lacks run id, host, flags, sample count, output plane, or
  oracle status.
- Gate/report code cannot consume the baseline evidence without producer-only
  fields or JSON-validator weakening.
- JSON policy leaks into generic crates or runtime outside generated
  per-grammar modules.
- A behavior row admits, a JSON `RESULTS.md` row moves, parse-only SOTA is
  claimed, or W2-style intervention lands in W1b.
- Generated output is hand patched or expands beyond selected generated parser
  inputs.

The REDRESS record for a W1b miss should name the selected target, generated
Track 1 path, independent oracle/Track 2 path, output plane, run id, strict
equality result, baseline Mbps if measured, gate command result, and exact
failure reason.

## Baseline-Only Close Decision

W1b may close baseline-only without admission.

Required close wording: W1b closes `G-W1b-NONJSON-BASELINE` as a generated
non-JSON baseline plus independent oracle, not as admitted behavior. The entry
unblocks W2 by providing the baseline Mbps and oracle lane. It must explicitly
state that:

- no intervention admitted;
- no direct, typed, parse-only, non-JSON, or JSON row admitted;
- no JSON `RESULTS.md` row moved;
- the baseline is seed evidence only;
- the admitted non-JSON intervention requirement remains open for W2 or later.

If any stakeholder wants W1b to claim admission, the wave must return REVISE:
that is W2's surface, with before/after measurement and the W1b baseline as the
seed.
