# CH3 V4 Regression / Strictness / Pre-Block Challenge

Date: 2026-05-18
Pass: SK-V8 S-P3 hardening V4
Role: CH3 regression, strict-vs-strict comparator discipline, pre-block preservation, row-gate integrity

## Scope

Reviewed V4 only against the requested packet:

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
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v8/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md`
- `skinny/RESULTS.md` and `skinny/REDRESS.md` as row/status and blocked-route authorities referenced by the packet

## Verdict

ACCEPT, confidence 96%.

## Blockers

None.

## Evidence

### V4 is traceability-only, not a semantic reopening

`p3-v4-exact-traceability-fold.md:19-27` scopes V4 to replacing broad V3 citation bundles with exact section labels or current file:line anchors. `p3-v4-exact-traceability-fold.md:30-39` explicitly preserves the G-Alpha/W0-only dispatch lock, strict-vs-strict comparator discipline, Lock 14, no-new-surface constraints, W2 seed table, W3 Tier A/Tier B split, `tape_vs_tape` demotion, 90-minute/LOC gates, and REDRESS pre-block coverage.

`HARDENING-S-P3-V3-CONSOLIDATED.md:23-28` shows CH3 already accepted V3 at 96% for this lens, while CH1 alone required exact traceability. `HARDENING-S-P3-V3-CONSOLIDATED.md:33-48` required V4 to replace broad citations while preserving substantive gates. V4 does that; it does not add implementation authority.

I also checked P3-A through P3-F for the rejected broad V3 bundle forms (`SPEC Sections 0.1-0.5`, broad `HANDOFF Sections`, generic RESULTS placeholders, generic REDRESS placeholders). No remaining rejected broad-bundle use appears in the P3-A through P3-F artifacts.

### Stale sidecar and permissive rows cannot become admission evidence

`SPEC.md:61-77` defines same-run strict anchors, same-run flaw probes, and sidecar planning signals. It requires `gate-json` to reject strict admission on plane mismatch, non-strict comparator strictness, stale sidecars, sidecar-only evidence, historical deltas, view-boundary validation, and strictness mismatch. `SPEC.md:230-244` repeats the non-negotiable: no strict admission except strict-vs-strict on a matching output plane, and no stale sidecar, permissive, lossy, historical, or view-boundary evidence as strict admission.

`p3d-telemetry-schema.md:70-80` binds strictness, comparator plane, comparator strictness, sidecar freshness, and output plane as gate fields. `p3d-telemetry-schema.md:119-128` makes strict admission fail before `A`/`G`/`GO` when the row/comparator planes differ, strictness is not strict, freshness is not allowed, validation is not measured-row, or stale/sidecar-only/historical evidence is used as admission evidence. `p3d-telemetry-schema.md:152-161` gives explicit failure states for `strict_plane_mismatch`, `deferred_validation_admission`, `stale_or_sidecar_only_strict_claim`, `producer_only_telemetry`, W0 behavior drift, W1 CostFacts absence, and W3 telemetry substitution.

`DISPATCH-PROMPT.md:176-189` likewise blocks `tape_vs_tape`, parse-only, sidecar, permissive, lossy, stale, or telemetry evidence as strict admission and states that sidecar/permissive/lossy comparators are planning signals or flaw probes. `HANDOFF.md:35-42` records the current report caveat: every current main row has `Strictness=deferred`, hot leaves are placeholders, and C++ comparator values are sidecar planning signals unless refreshed under a later same-run/freshness gate. `skinny/RESULTS.md:3-42` shows current rows still carry deferred/view-boundary status, and `skinny/RESULTS.md:219` records C++ sidecar provenance as not same-run strict anchors.

Result: V4 wording does not let stale sidecar or permissive rows become admission evidence.

### W0/W1 still block behavior waves

`SPEC.md:29-36` says no SK-V8 implementation wave dispatches from S-P3 itself, G-Alpha is required, `G-Alpha closed` initially dispatches W0 only, and W1-W6 remain blocked until W0 closes and exact owner paths/row gates/challenge/orchestrator dispatch are present. `SPEC.md:327-383` makes W0 telemetry-only, rejects parser/scanner/SIMD/asm/codegen/product/generated behavior changes, and states W0 rejection blocks W1-W6. `SPEC.md:385-440` makes W1 conditional on W0, binds CostFacts/comparator evidence into `gate-json`, keeps generated/parser behavior unchanged unless separately challenged, and states W1 rejection blocks W2-W6 behavior waves.

`DISPATCH-PROMPT.md:6-9` and `DISPATCH-PROMPT.md:90-106` repeat the W0-first and conditional-wave gates. `DISPATCH-PROMPT.md:211-222` says G-Alpha is not yet closed and no implementation dispatch occurs until the user signs off with `G-Alpha closed`. `HANDOFF.md:5-7`, `HANDOFF.md:119-137`, and `HANDOFF.md:191-198` mirror the same boundary: if G-Alpha closes, only W0 is dispatchable; W1-W6 require W0 admission, exact plan artifacts, row gates, pre-blocks, revert protocol, same-wave consumer, and challenge where needed.

`p3b-wave-sequencing.md:16-18` and `p3b-wave-sequencing.md:36-40` preserve W0 first, W1 second, and W2-W4 behavior waves only after W0/W1. `p3c-falsifiability-gates.md:38-64` and `p3c-falsifiability-gates.md:68-92` make W0 and W1 failure abort conditions for later waves.

Result: V4 does not bypass W0/W1 or dispatch any behavior wave.

### Row-gate integrity remains intact

`SPEC.md:144-189` preserves the opening row goalset: 17 `parse_only` rows are substrate-guard non-admission, six direct rows are A/GO guards with eleven N-direct/NO-GO rows, and four real typed rows are A/GO product-plane rows. It carries W2 real-typed GO maintain floors, existing direct GO guard floors, and W2 candidate typed seed floors. `SPEC.md:442-497` requires W2 to add at least two generated typed rows or reject with REDRESS, preserve all four existing real-typed GO rows and existing direct GO guard rows, prove Track 2/oracle independence, keep direct digest rows as guard rows only, and maintain all non-target rows within the no-regression budget.

`SPEC.md:621-655` requires W4 selected direct rows to meet strict direct thresholds on Track 1 and Track 2, maintain Track 2 independence, maintain all non-target rows, and keep direct digest results out of product-plane proof. `p3c-falsifiability-gates.md:96-148` binds W2 row thresholds, direct guard floors, Track 2/oracle independence, and full-table maintain gates. `p3c-falsifiability-gates.md:203-237` does the same for W4 direct rows.

`skinny/RESULTS.md:3-42` still provides the current row table. The four real typed GO rows are `twitter`, `update_center`, `mesh`, and `marine_ik`; existing direct GO rows include `citm_catalog`, `apache_builds`, `mesh`, `marine_ik`, `numbers`, and `unicode_basic`. `skinny/RESULTS.md:217-218` records Track 1 and Track 2 independence boundaries. V4 does not remove these guard rows or allow status movement without row gates.

### REDRESS pre-blocks stay closed

`ORCHESTRATOR.md:83-88` defines CH3 as the regression lens: no proposal may reopen a route in `skinny/REDRESS.md`, the pre-block list must be correct, and no admitted row may silently regress. `PASS-3-SYNTHESIS-PLAN.md:122-126` requires SPEC coverage of REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89 plus historical blocked routes. `PASS-3-SYNTHESIS-PLAN.md:249-273` requires same-row falsifiability, same-wave consumers, no hypothesis transfer, no waves without gates, no future-phase close, and no reopening pre-blocked REDRESS routes.

`SPEC.md:767-812` is the controlling route ledger. It blocks new directive/BIR/substrate/BackendShape/UnionTape/public substrate API/parser-owned cursor/facts/sidecar/parallel routes; generic JSON policy; sidecar/permissive/lossy/stale strict admission; `tape_vs_tape`/parse-only/telemetry as W3 production consumer; orphan primitives; Track 1/Track 2 coupling; automatic dispatch; REDRESS 28+33, 49-55, 59-65, 66-72, 80, 82-84, 88-90; Alpha bitmap reserve; and Tier B inside W3. Reopening requires fresh W0 evidence, same-wave consumer, scalar/checkasm where relevant, no-regression gate, REDRESS citation, and challenge acceptance.

`p3e-preblocked-ledger.md:38-49` carries the global block table for sidecars, lossy/permissive strict admission, `tape_vs_tape`/parse-only/telemetry as W3 consumer, orphan primitives, Track 1/Track 2 dishonesty, and automatic dispatch. `p3e-preblocked-ledger.md:55-101` enumerates the historical route families, including pair-token, function-pointer, 12-byte width churn, separator/SWAR, Class A tiny-string, sidecar/EventCursor/projection tables, retained string scanners, direct materialization, hand typed sinks, cap-16/StringBlock16, raw f64, permissive/lossy anchors, PMULL, CTZ, and B6. `p3e-preblocked-ledger.md:105-115` requires fresh baseline, strict comparator, full-table maintain, same-wave consumer, and REDRESS/revert for any changed framing.

`DISPATCH-PROMPT.md:164-184` and `HANDOFF.md:151-164` repeat the same closure rule and named blocked families. V4 traceability wording adds exact section/file anchors to those claims; it does not loosen the reopen package.

### W3 remains Tier A only

`SPEC.md:506-594` keeps W3 conditional on W0/W1, fresh plan, exact owner files, same-wave production consumer, revert protocol, measured-path proof, scalar/checkasm requirements, pre-block differences, and challenge acceptance that the plan is not a renamed REDRESS 50-55, 60-72, 82-84, 88, or 89 route. Its lead hypothesis is one retained `Tape` with scan-written opaque structural-class ordinals and generated retained JSON Track 1 parser consumption. It rejects old offset append survival, parser-owned cursor/fact slots, sidecar event vectors, aux tables, density caches, second source scans, Tier B, `tape_vs_tape` as consumer, unconditional PMULL/CTZ, StringBlock16, single-quartet Unicode, object-pair value-byte carry, and local materialization families.

`p3a-candidate-shortlist.md:27-36`, `p3b-wave-sequencing.md:46-50`, `p3c-falsifiability-gates.md:152-199`, `p3d-telemetry-schema.md:135-146`, and `p3f-spec-draft.md:35-39` preserve that W3 boundary. `HARDENING-S-P2-V7-CONSOLIDATED.md:44-64` also preserves the S-P2 boundaries: S-P2 authorizes S-P3 planning only, strict-vs-strict remains mandatory, Tier A is structural-class cursor migration only, Tier B is separate, no new directive/BIR/BackendShape/UnionTape/API/parser-owned cursor/facts/parallel substrate is admitted, and `tape_vs_tape` remains residual telemetry.

Result: V4 does not smuggle Tier B, `tape_vs_tape`, PMULL/CTZ, sidecars, parser-owned cursor/facts, or a new substrate into W3.

## Residual Non-Blocking Risks

- `p3d-telemetry-schema.md` cites `skinny/RESULTS.md:217-218` for several current-row claims; sidecar provenance itself is at `skinny/RESULTS.md:219`. This is a CH1-style traceability precision issue, not a CH3 blocker, because the strictness gates in SPEC, P3-D, DISPATCH, HANDOFF, and P3-E independently reject stale/sidecar-only admission.
- P3-D's telemetry enum label `retained_union_tape` is terminology-adjacent to the forbidden `UnionTape`, but the controlling SPEC and P3-F text define W3 as representation replacement inside one retained `Tape`, not a new substrate or public `UnionTape` surface. This remains a wording watch point for CH5, not a CH3 route reopen.

## Required Fold If REVISE

None.

## Self-Verdict

ACCEPT, confidence 96%. No open critical defect found under the CH3 V4 challenge lens.
