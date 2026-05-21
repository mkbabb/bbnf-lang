# SK-V13 S-P1 V1 CH3: Regression / REDRESS Review

Pass: S-P1 Profile. Cycle: V1.
Date: 2026-05-21.
Lens: CH3 REGRESSION.
Scope: adversarial review of the six S-P1 V1 artifacts under `restart/skinny/tranches/sk-v13/research/p1/` for uncited REDRESS-route reopening.
Disposition: REVISE.

## §1 - Method

Reviewed:

- `restart/prompts/skinny/PASS-1-PROFILE.md` CH3 contract.
- `skinny/REDRESS.md`, with special attention to REDRESS 50-55, 60-72, 80, 82-84, 88-90, 96-98, 119-120, 126-127.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.

CH3 standard: S-P1 may report anomalies, but it must not turn anomaly wording into an implied next implementation route already blocked by REDRESS unless it cites that entry, marks the route pre-blocked, and names the fold action needed before S-P2/S-P3 may use it.

## §2 - Findings

### CH3-R1 - P1-C mode-III gaps can silently reopen REDRESS 96/97/98 structural-union routes

Disposition: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:91` opens the anomaly table for masking and structural probes.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:95` requires fresh `host_call_eager_decode` capture.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:96` requires fresh `alternate_scalar_plan` capture and says CH3 must check later scalar-plan interpretation against REDRESS rejected routes.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:98` requires structural-scan-only capture and warns that treating it as a separable sidecar is a CH5 Lock 1 risk.
- `skinny/REDRESS.md:2910` retires `G-W3-UNION-SUBSTRATE`.
- `skinny/REDRESS.md:2916` summarizes REDRESS 96 as the rejected full class-column substrate plus move-consumed `scan_structurals` vector.
- `skinny/REDRESS.md:2919` summarizes REDRESS 97 as the rejected allocation-free streaming cursor over the aarch64 scanner.
- `skinny/REDRESS.md:2923` rejects the remaining emit-site class-lane-only route as a paper-close.
- `skinny/REDRESS.md:2934` states no wave may force, amend, or split the same union-substrate hypothesis without a new Alpha/S-P3 contract.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md:309` says new same-goal union attempts must cite prior REDRESS, name the material differential, and pass CHALLENGE.

Problem:

P1-C correctly admits that mode-III evidence is absent, but its fold actions still name `alternate_scalar_plan` and structural-scan-only capture as S-P2-facing work without explicitly pinning REDRESS 96/97/98 as pre-blocks. That leaves a route for a later planner to read "structural scan missing" or "alternate scalar plan missing" as permission to revive class-column, streaming-cursor, or class-lane union substrate work.

Required fold action:

In V2 P1-C and consolidated hardening, add an explicit CH3 guard beside every structural/alternate-scalar fold action: "No class-column substrate, streaming structural cursor, emit-site class lane, parser-owned structural projection, sidecar event vector, second source scan, or parallel UnionTape is reopened by this anomaly. Any new union attempt must cite REDRESS 96/97/98, name a material differential, and satisfy the fresh contract gate."

### CH3-R2 - Masking-probe anomalies must preserve REDRESS 50-55 and dispatch-table pre-blocks

Disposition: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md:127` says `dispatch_value` masks inner string/number/structural primitive attribution.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:171` says masking-probe rows named in `skinny/RESULTS.md` are not independent PMU rows in the fresh ledger.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:136` says parse-only dispatch granularity is insufficient and S-P2 must obtain deeper direct or typed profiles before inferring unicode/number/string primitives.
- `skinny/REDRESS.md:715` rejects parse-time aux side tables.
- `skinny/REDRESS.md:742` rejects the byte-class whitespace event cursor.
- `skinny/REDRESS.md:784` rejects the parser-local structural-mask cursor.
- `skinny/REDRESS.md:291` says dispatch-table/function-pointer alternates remain rejected.
- `skinny/REDRESS.md:216` records the dispatch-table alternate as audited and rejected as a signal.

Problem:

The artifacts mostly avoid proposing fixes, but the phrase "deeper direct or typed profile" plus missing masking rows could be misread as reopening parse-time side tables, event cursors, parser-owned structural cursors, or dispatch-table alternates unless V2 names those as pre-blocked. P1-C cites dispatch-table invalidity at `p1c-samply-mode-3.md:101`, but the broader masking/direct-profile fold is not tied to REDRESS 50-55 or the dispatch-table entries.

Required fold action:

In V2 P1-A/P1-C/P1-D/P1-E, keep masking probes as measurement-only. If a fold action says "profile deeper", it must also say that REDRESS 50-55 side-table/cursor routes and dispatch-table/function-pointer alternates are not reopened. Allowed V2 work is artifact capture and attribution only, not implementation routing.

### CH3-R3 - P1-F sidecar and classification gaps can reopen exhausted direct-row history unless REDRESS 119/120 are cited

Disposition: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:131` says fresh JSON PMU changed several direct classifications relative to checked `RESULTS.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:137` opens anomaly and masking signals.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:145` flags simdjson/asmjson rows as `n/a` or absent comparator gaps.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:148` says C++ sidecars for direct/typed planes are absent by plane.
- `skinny/REDRESS.md:3542` says REDRESS 119 is the direct row authority and residual direct rows have measured uncloseable/fixpoint proof.
- `skinny/REDRESS.md:3531` closes SK-V11 as a measured fixpoint, not overall direct `GO`.
- `skinny/REDRESS.md:3549` routes future work to treat the 13 SK-V11 direct residual rows as exhausted unless a material differential beyond REDRESS 114-119 is named with fresh profile and micro-proof evidence.

Problem:

P1-F correctly labels changed PMU classifications as profile signals, but it does not cite REDRESS 119/120 when reporting direct classification movement and sidecar gaps. Without that citation, a later synthesis pass could treat fresh PMU-side direct "A" signals, missing C++ sidecars, or stale comparator anchors as permission to reopen SK-V11 direct-row residual work.

Required fold action:

In V2 P1-F, every direct-row classification movement or sidecar/comparator gap must carry a REDRESS 119/120 note: direct residuals remain exhausted/fixpoint history until a materially differentiated route beyond REDRESS 114-119 is named with fresh same-harness profile, comparator, and micro-proof evidence. Fresh PMU extraction alone is not a direct-row admission.

### CH3-R4 - SIMD/orphan wording is mostly guarded, but structural-scan and PEXT rows need explicit no-orphan carry-forward language

Disposition: REVISE.

Evidence:

- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:100` says `alternate_pext_mask_plan` should be captured or marked unsupported on aarch64.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md:172` explicitly says no dispatch table, SIMD path, PMULL/CSSC route, or alternate union substrate is reopened.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md:145` records simdjson/asmjson sidecar gaps.
- `skinny/REDRESS.md:3860` says USER PIN D3 unblocks future materially differentiated union attempts while REDRESS 96/97/98 remain historical measured failures.
- `skinny/REDRESS.md:3864` records ASM-gen REDRESS-126 as a measured route-production split.
- `skinny/REDRESS.md:3869` says final aarch64 orphan state is zero and lists the demoted orphan primitives.

Problem:

P1-D is clean on this point, but P1-C/P1-F do not fold the zero-orphan state into their structural/PEXT/SIMD sidecar language. A future S-P2/S-P3 reader could treat absent PEXT or sidecar rows as unowned SIMD work instead of a measurement gap subject to REDRESS-126 orphan disposition and fresh same-wave-consumer proof.

Required fold action:

In V2 P1-C/P1-F, add a no-orphan clause to every SIMD/PEXT/sidecar fold: absent rows are telemetry gaps only; they do not create new orphan SIMD primitives, reopen PMULL/CSSC/PREFIX-XOR routes, or bypass REDRESS-126. Any SIMD primitive candidate must carry scalar reference, parity/checkasm, same-wave consumer, feature-mask disclosure, and zero-orphan accounting.

## §3 - Non-Findings

- P1-A keeps Track 1 and Track 2 symbol planes separate at `p1a-samply-mode-1.md:126`; no CH3 reopening found there beyond the masking guard in CH3-R2.
- P1-B says direct samply profiles are invalid panic paths at `p1b-samply-mode-2.md:123`-`125` and does not propose implementation routes.
- P1-D explicitly blocks REDRESS reopening at `p1d-pmu-cycles.md:172`; this is the cleanest CH3 language in V1.
- P1-E treats direct and CSS hot leaves as unprofiled at `p1e-hot-leaf-attribution.md:122`-`130`; this is measurement debt, not an implementation proposal.

## §4 - Required Consolidated Fold

S-P1 V1 should not converge as-is. Fold these guards into V2 artifacts and the consolidated hardening:

1. P1-C must cite REDRESS 96/97/98 beside structural-scan, alternate-scalar, and union-substrate-adjacent gaps.
2. P1-A/P1-C/P1-D/P1-E must cite REDRESS 50-55 and dispatch-table rejection when discussing masking/deeper-profile routes.
3. P1-F must cite REDRESS 119/120 beside direct classification movement, stale comparator anchors, and missing sidecars.
4. P1-C/P1-F must carry REDRESS-126 zero-orphan language for SIMD/PEXT/sidecar gaps.
5. The hardening aggregator should mark these as REVISE fold actions, not S-P2 implementation tickets.

## §5 - Final Disposition

Disposition: REVISE.

Rationale: V1 mostly reports anomalies rather than proposals, and several artifacts explicitly preserve measurement boundaries. However, missing citations around structural-scan/masking/direct/SIMD anomalies leave enough ambiguity to silently reopen REDRESS-preblocked routes in downstream planning. The required correction is documentation-fold precision, not source work.
