# CH3 V5 Regression / Strictness / Pre-Block Challenge

Date: 2026-05-18
Pass: SK-V8 S-P3 hardening V5
Role: CH3 regression, strict-vs-strict comparator discipline, pre-block preservation, row-gate integrity

## Scope

Reviewed the unchanged V4-folded S-P3 packet as the required second consecutive challenge cycle after V4 ACCEPT:

- `restart/prompts/ORCHESTRATOR.md` sections 3W and 3Z
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/skinny/tranches/sk-v8/SPEC.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3a-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3b-wave-sequencing.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3e-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3f-spec-draft.md`
- `restart/skinny/tranches/sk-v8/research/p3/p3-v4-exact-traceability-fold.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V4-CONSOLIDATED.md`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` as the row/status and historical route ledgers referenced by the packet

This report does not implement or dispatch any SK-V8 wave.

## Verdict

ACCEPT, confidence 96%.

## Blockers

None.

## Evidence

### V5 is the unchanged second-cycle check

`HARDENING-S-P3-V4-CONSOLIDATED.md:9-20` records V4 as a qualifying 6/6 ACCEPT cycle with minimum confidence 96 and no open critical defect. `HARDENING-S-P3-V4-CONSOLIDATED.md:31-39` states V4 is the first qualifying S-P3 ACCEPT after V3 REVISE, requires one more consecutive qualifying cycle, requires no fold before V5, and does not close G-Alpha or dispatch implementation.

`p3-v4-exact-traceability-fold.md:30-39` states the V4 fold was traceability-only and preserved G-Alpha/W0-only dispatch, strict-vs-strict comparator discipline, Lock 14, no-new-surface constraints, W2 seed gates, W3 Tier A/Tier B split, `tape_vs_tape` demotion, 90-minute/LOC gates, and pre-blocked REDRESS coverage. The current packet still matches those boundaries.

### Strict-vs-strict remains binding

`SPEC.md:61-77` defines same-run strict anchors, same-run flaw probes, and sidecar planning signals. Strict admission must be rejected unless the comparator plane matches the row output plane, comparator strictness is strict, freshness qualifies, and validation occurs inside the measured row. Deferred strictness, view-boundary validation, stale sidecars, sidecar-only evidence, historical deltas, and plane mismatch remain guard telemetry only.

`SPEC.md:230-244` repeats the non-negotiable: no strict admission except strict-vs-strict on a matching output plane, and no stale sidecar, permissive, lossy, historical, or view-boundary evidence as strict admission.

`p3d-telemetry-schema.md:70-80` binds strictness, output plane, comparator plane, comparator strictness, sidecar freshness, and flaw-probe treatment as gate fields. `p3d-telemetry-schema.md:119-128` makes `gate-json` reject strict admission before computing `A`/`G`/`GO` when strictness, freshness, measured validation, or plane predicates fail. `p3d-telemetry-schema.md:152-161` names explicit failure states for plane mismatch, deferred validation, stale/sidecar-only strict claims, producer-only telemetry, W0 behavior drift, W1 CostFacts absence, and W3 telemetry substitution.

`skinny/RESULTS.md:3-42` still shows current rows as `Strictness=deferred` and `parse_utf8=view-boundary`; `skinny/RESULTS.md:219` states C++ sidecar columns do not count as same-run strict anchors. Therefore stale sidecars and permissive/lossy rows cannot become admission evidence under the V5 packet.

### W0/W1 still block behavior

`SPEC.md:29-36` says S-P3 dispatches no implementation wave, G-Alpha signoff remains required, `G-Alpha closed` initially dispatches W0 only, and W1-W6 remain blocked until W0 closes and exact owner paths, row gates, challenge acceptance, and orchestrator/user dispatch are present.

`SPEC.md:327-383` keeps W0 telemetry-only: all 38 current rows must satisfy required telemetry, throughput cells must stay within +/-1.0% of `SK-V8-open`, stale sidecar strict claims must be rejected, no parser/scanner/SIMD/asm/codegen/product/generated behavior change may land, and W0 rejection blocks W1-W6. `SPEC.md:385-440` keeps W1 conditional on W0, binds CostFacts/comparator evidence into `gate-json`, keeps parser/generated behavior unchanged unless a separate challenged behavior consumer is accepted, and makes W1 rejection block W2-W6 behavior waves.

`DISPATCH-PROMPT.md:6-9` and `DISPATCH-PROMPT.md:90-106` repeat that W1-W6 do not dispatch from the prompt alone and require W0 admission plus fresh artifacts, exact row gates, pre-block citations, same-wave consumer, revert protocol, challenge where needed, Lock 14, and <=90 minute fit. `HANDOFF.md:119-149` mirrors the same entry/exit gates, and `HANDOFF.md:191-198` says the next user decision is G-Alpha closed or revise, with no implementation wave before that decision.

P3-A through P3-F are consistent: `p3a-candidate-shortlist.md:40-48` states W0/W1 are mandatory gates before behavior thresholds; `p3b-wave-sequencing.md:16-25` sequences W0 and W1 before W2-W4; `p3c-falsifiability-gates.md:38-92` makes W0/W1 failures abort later waves; `p3f-spec-draft.md:15-18` preserves G-Alpha/W0 dispatch lock.

### Row-gate integrity remains intact

`SPEC.md:144-189` preserves the opening row goalset: 17 `parse_only` rows remain substrate-guard non-admission, six direct rows are A/GO guards with eleven N-direct/NO-GO rows, and four real typed rows are A/GO product-plane rows. It carries W2 real-typed maintain floors, existing direct GO guard floors, and the W2 candidate typed seed table.

`SPEC.md:442-497` requires W2 to add at least two generated typed rows or reject with REDRESS, preserve all four existing real-typed GO rows and existing direct GO guard rows, prove Track 2/oracle independence, keep direct digest rows as guard rows rather than typed proof, and maintain non-target rows. `SPEC.md:621-655` requires W4 direct selected rows to meet Track 1 and Track 2 floors, use same-run strict direct anchors and measured validation, preserve Track 2 independence, maintain non-target rows, and keep digest out of product-plane proof.

`p3c-falsifiability-gates.md:96-148` binds W2 numeric floors, Track 2/oracle independence, direct guard preservation, and full-table maintain. `p3c-falsifiability-gates.md:203-237` binds W4 selected-row floors, Track 2 independence, full-table maintain, and negative gates for parser-owned scratch/direct sidecar, raw f64, digest cap-16, and Track 2 coupling.

`skinny/RESULTS.md:3-42` still provides the current row/status artifact. The four current real typed GO rows are `twitter`, `update_center`, `mesh`, and `marine_ik`; existing direct GO rows include `citm_catalog`, `apache_builds`, `mesh`, `marine_ik`, `numbers`, and `unicode_basic`. `skinny/RESULTS.md:217-218` preserves Track 1/Track 2 independence authority. No V5-reviewed text drops or weakens these row gates.

### Pre-blocked routes remain closed

`ORCHESTRATOR.md:83-88` defines CH3 as preventing REDRESS route reopen, incorrect pre-block lists, and silent row regression. `PASS-3-SYNTHESIS-PLAN.md:122-126` requires P3-E and SPEC coverage of REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, plus historical blocked routes. `PASS-3-SYNTHESIS-PLAN.md:249-273` requires same-row falsifiability, same-wave consumers, no hypothesis transfer, no wave without gates, no future-phase close, and no candidate reopening a pre-blocked REDRESS route.

`SPEC.md:767-812` is the controlling pre-block ledger. Reopening requires fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance. Its global blocks include new directive/BIR/substrate/BackendShape/UnionTape/public API/parser-owned cursor/facts/sidecar/parallel routes, sidecar/permissive/lossy/stale strict admission, `tape_vs_tape`/parse-only/telemetry as W3 production consumer, orphan primitives, Track 1/Track 2 coupling, and automatic implementation dispatch. Its specific blocks include REDRESS 16/17/18/25, 28+33, 36-38/85-86, 49-55, 59-65/72/83, 66-72/80, 74-79/81/87, 82-84, 88-90, Alpha bitmap reserve, and Tier B in W3.

`p3e-preblocked-ledger.md:16-18` states the SPEC minimum list is binding and not exhaustive, and forbids deferral. `p3e-preblocked-ledger.md:38-49` blocks sidecar/parser-owned/aux/cursor routes, lossy/permissive/sidecar strict admission, `tape_vs_tape`/parse-only/telemetry as W3 consumer, orphan primitives, Track 1/Track 2 dishonesty, and automatic dispatch. `p3e-preblocked-ledger.md:55-101` enumerates the historical route families, including pair-token, function-pointer, 12-byte width churn, separator/SWAR, Class A tiny-string, aux/EventCursor/projection sidecars, retained string scanners, direct materialization, hand typed sinks, cap-16/StringBlock16, raw f64, permissive/lossy anchors, PMULL, CTZ, B6 limits, and Alpha-E bitmap reland constraints. `p3e-preblocked-ledger.md:105-115` requires fresh baseline, strict comparator, full-table maintain, same-wave consumer, revert, and REDRESS for any changed framing.

`HANDOFF.md:151-164` and `DISPATCH-PROMPT.md:164-184` repeat the same route closure rule. V5 finds no wording that lets traceability anchors or changed labels reopen a REDRESS route.

### W3 remains Tier A only

`SPEC.md:506-594` keeps W3 conditional on W0/W1, a fresh plan, exact owner files, same-wave production consumer, revert protocol, measured-path proof, scalar/checkasm requirements, pre-block differences, Lock 1 disposition, and challenge acceptance that the plan is not a renamed REDRESS 50-55, 60-72, 82-84, 88, or 89 route. W3 is one retained `Tape` with scan-written opaque structural-class ordinals and generated JSON retained parser consumption. It blocks Tier B, `tape_vs_tape` as consumer, PMULL/CTZ, sidecar event vectors, aux tables, density cache, parser-owned class/fact slots, second scans, old offset append path, StringBlock16, single-quartet Unicode, object-pair value-byte carry, and local materialization.

`p3b-wave-sequencing.md:46-50`, `p3c-falsifiability-gates.md:152-199`, `p3d-telemetry-schema.md:135-146`, and `p3f-spec-draft.md:35-39` preserve the same W3 boundary: Tier B is blocked, `tape_vs_tape` is telemetry/gate-binding only, and generated JSON retained parser is the production consumer.

## Residual Non-Blocking Risks

- P3-D still uses `retained_union_tape` as a telemetry value, while the controlling SPEC forbids a public `UnionTape` and defines W3 as representation replacement inside one retained `Tape`. Because the controlling gates reject new substrate/API/UnionTape routes, this is not a CH3 blocker.
- Several P3 artifacts cite `skinny/RESULTS.md:217-218` for Track 2 independence while sidecar provenance is at `skinny/RESULTS.md:219`. This is a traceability precision note only; strictness demotion is independently enforced by SPEC, P3-D, P3-E, DISPATCH, and HANDOFF.

## Required Fold If REVISE

None.

## Self-Verdict

ACCEPT, confidence 96%. No open critical defect found under the CH3 V5 lens.
