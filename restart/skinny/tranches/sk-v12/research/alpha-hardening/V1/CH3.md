# SK-V12 Pass Alpha CH3 - Regression / REDRESS

Pass: Pass Alpha SK-V11 -> SK-V12.
Cycle: CHALLENGE V1.
Lens: CH3 regression / REDRESS.
Date: 2026-05-20.
Scope: Alpha-C / Alpha-E / Alpha-F, `SYNTHESIS.md`, `HANDOFF.md`, and
`skinny/REDRESS.md` through REDRESS 120, with emphasis on REDRESS
96/97/98/102 and 111-120.

## Disposition

ACCEPT.

No critical REDRESS regression finding. The SK-V12 Alpha packet does not reopen
the retired W3 union/event/class-column substrate, does not treat parse-only rows
as SOTA admissions, does not use W1a's non-JSON report lane as a generated
baseline, and does not schedule JSON direct residual work before the generated
non-JSON baseline/intervention priority. The conditional JSON residual path is
still a reopen path, but it carries the material-differential requirements from
REDRESS 114-120: fresh profile-backed evidence, a source delta not equivalent to
W3-W7, independent Track 2/oracle, same-host microbench, strict same-run
comparator/floor evidence, same-wave gate consumption, and preserved guards.

## Critical Findings

None.

## Dispositions By Surface

| Surface | Disposition | CH3 finding |
|---|---|---|
| Alpha-C REDRESS digest | ACCEPT | Correctly identifies REDRESS 119 as direct residual authority, REDRESS 112/113 as the generated non-JSON blocker, and REDRESS 96/97/98/102 as W3/parse-only hard negative authority (`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:25-32`, `:73-88`, `:100-143`). |
| Alpha-E shortlist | ACCEPT | E1-E3 target the REDRESS 112 generated-baseline absence with generated Track 1 plus independent oracle. E4 is gated on an admitted baseline. E5 is conditional, lowest priority, and requires post-REDRESS-119 material evidence rather than a renamed JSON retry (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:27-33`, `:37-88`, `:186-233`, `:235-297`). |
| Alpha-F contract draft | ACCEPT | Carries generated non-JSON baseline first, intervention second, JSON direct residuals pre-blocked, guard rows preserved, parse-only diagnostic, and W3 family closed (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:47-65`, `:66-97`, `:175-197`). |
| `SYNTHESIS.md` goalset | ACCEPT | The close condition keeps the SK-V11 measured-fixpoint surface, puts generated non-JSON baseline/intervention ahead of JSON-only work, records the REDRESS 119 direct residual table, and blocks W3/parse-only/renamed routes (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:23-77`, `:90-109`, `:196-238`). |
| `HANDOFF.md` | ACCEPT | Repeats that residual direct rows are not first-wave targets and can reopen only with fresh material evidence beyond REDRESS 114-119 (`restart/skinny/tranches/sk-v12/HANDOFF.md:39-58`, `:83-100`). |

## Regression Checks

### REDRESS 96/97/98 - W3 Union/Event Substrate

REDRESS 96 rejected the full class-column plus move-consumed structural-index
implementation after correctness checks passed but every W3 must-improve row and
every W10b maintain row failed (`skinny/REDRESS.md:2797-2848`). REDRESS 97
removed the allocation vector with an allocation-free streaming cursor and still
failed the same W3/W10b row family (`skinny/REDRESS.md:2852-2906`). REDRESS 98
then retired `G-W3-UNION-SUBSTRATE`, including class-column, streaming cursor,
class-lane-only, parser-owned structural projection, and W4 cascade-lock routes
(`skinny/REDRESS.md:2910-2950`).

Alpha-C carries these as hard pre-blocks (`alpha-C-redress-digest.md:73-98`,
`:133-135`). Alpha-F and `SYNTHESIS.md` independently refuse W3
union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
`UnionTape`, retained structural vector, parser-owned projection, cascade-lock,
and renamed W3 routes (`alpha-F-contract-draft.md:94-96`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62-64`, `:198-200`).

Verdict: ACCEPT. No Alpha-E candidate names W3, retained class columns,
structural cursors, class lanes, `UnionTape`, or W4-through-W3 cascade as a
consumer or prerequisite. The closest helper language is E4's generated
FIRST/prefix intervention, but it is grammar metadata on an already-admitted
non-JSON generated baseline, not a retained parse-plane substrate
(`alpha-E-candidate-shortlist.md:186-233`).

### REDRESS 102 - Parse-Only Firewall

REDRESS 102 admitted only a proof-only firewall: no behavior source, generated
output, benchmark body, or row movement, and parse-only rows stayed outside
`A / GO` (`skinny/REDRESS.md:3042-3058`). The SK-V12 goalset keeps `parse_only`
diagnostic only and refuses parse-only SOTA close or admission
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:54-56`, `:201`).

Verdict: ACCEPT. Alpha-E explicitly excludes parse-only SOTA movement
(`alpha-E-candidate-shortlist.md:299-304`), and the main contract rejects
parse-only SOTA claims in telemetry and refusal conditions
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:190-193`, `:226-227`).

### REDRESS 111/112/113 - Non-JSON Baseline Axis

REDRESS 111 admitted only the companion non-JSON report lane; it did not relax
JSON validation, update `skinny/RESULTS.md`, create generated non-JSON baseline
authority, or move any parser row (`skinny/REDRESS.md:3284-3309`). REDRESS 112
rejected the CSS L4 generated baseline because codegen/runtime emission remained
JSON-profiled through `json_provider::ensure_runtime_profile` and no generated
CSS L4 runtime existed (`skinny/REDRESS.md:3313-3338`). REDRESS 113 blocked W2
because an intervention wave may not create the first measurable non-JSON row
and then claim the intervention in the same wave (`skinny/REDRESS.md:3342-3355`).

Alpha-E preserves those distinctions: E1-E3 require generated Track 1,
independent same-plane oracle/Track 2, strict equality, finite throughput,
provenance, and gate/report consumption; E4 can run only after E1 admits a
baseline and records `W1_css_baseline_mbps`
(`alpha-E-candidate-shortlist.md:37-88`, `:90-184`, `:186-233`). Alpha-F and
`SYNTHESIS.md` make baseline first and intervention second
(`alpha-F-contract-draft.md:71-82`;
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:35-48`).

Verdict: ACCEPT. The shortlist reopens the non-JSON axis only under the material
differential REDRESS 112 requires: generated non-JSON runtime/codegen authority,
independent oracle/Track 2, strict equality, measured throughput, and gate
consumption. It does not use W1a schema-only evidence as baseline authority.

### REDRESS 114/115/116/117/118/119/120 - Direct Residual Fixpoint

REDRESS 114 measured `number_span_emit_slot` below the `mesh/direct_to_struct`
floor (`skinny/REDRESS.md:3359-3381`). REDRESS 115 measured
`container_tail_next` below the `random/direct_to_struct` floor
(`skinny/REDRESS.md:3385-3409`). REDRESS 116 blocked bounded string span before
source because malformed parity and independent Track 2 cost remained unresolved
(`skinny/REDRESS.md:3413-3432`). REDRESS 117 blocked escaped-segment fold because
the decoded-byte source method reopened REDRESS 54 with the same sink seam and
output contract (`skinny/REDRESS.md:3436-3460`). REDRESS 118 blocked output
digest/host-sink because no legal row/source/consumer/oracle candidate remained
and the closest residual could not clear both tracks (`skinny/REDRESS.md:3464-3493`).
REDRESS 119 closed all 13 direct residual rows as a measured fixpoint with no
row movement (`skinny/REDRESS.md:3497-3527`). REDRESS 120 closed SK-V11 as a
measured fixpoint and routed SK-V12 to solve generated non-JSON baseline first,
treating direct residual rows as exhausted unless a future pass names a material
differential beyond REDRESS 114-119 (`skinny/REDRESS.md:3531-3553`).

Alpha-C carries that exact reopen rule (`alpha-C-redress-digest.md:100-143`,
`:145-158`). Alpha-E's E5 is the only JSON direct candidate and remains
conditional: it can dispatch only after the non-JSON generated template has
landed, with post-E4 profile evidence, a same-host caller microbench showing both
tracks can plausibly close, proof the source delta is not one of the rejected
W3-W7 families under a new name, generated JSON Track 1 plus independent Track
2, row floors, and same-wave `gate-json` consumption
(`alpha-E-candidate-shortlist.md:235-297`). `SYNTHESIS.md` repeats the same
pre-block and material-reopen rule (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-61`,
`:202-209`, `:223-238`).

Verdict: ACCEPT. E5 is not a free JSON retry. It is a conditional reopen with the
required material differential, after the non-JSON priority is satisfied or
explicitly blocked, and it preserves both Track 1 and independent Track 2 floor
requirements.

## Route Reopen Matrix

| Route family | CH3 result | Evidence |
|---|---|---|
| W3 union/event/class-column/streaming cursor/class lane/sidecar substrate | Not reopened | REDRESS 96/97 measured both substrate shapes as failures, REDRESS 98 retired the thesis, and SK-V12 carries the block (`skinny/REDRESS.md:2797-2950`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:62-64`, `:198-200`). |
| Parse-only SOTA close | Not reopened | REDRESS 102 firewall is carried; parse-only stays diagnostic (`skinny/REDRESS.md:3042-3058`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:54-56`). |
| W1a report lane as baseline | Not reopened | E1-E3 require generated Track 1 plus independent oracle/Track 2; W1a can only provide report/gate consumption (`alpha-E-candidate-shortlist.md:55-88`, `:107-136`, `:156-184`). |
| W1b/W2 paper close | Not reopened | Baseline and intervention are split; E4 requires an admitted E1 baseline and `W1_css_baseline_mbps` before intervention (`alpha-E-candidate-shortlist.md:186-233`). |
| Numeric slot / container-tail direct retry | Not reopened | E5 excludes `number_span_emit_slot` and `container_tail_next` under a new name and requires fresh microbench/floor evidence (`alpha-E-candidate-shortlist.md:242-251`, `:291-293`). |
| Bounded string / escaped source fold / digest host-sink | Not reopened | E5 excludes bounded string span, escaped source fold, and output-digest host sink under a new name; Alpha-C carries the REDRESS 116-118 material requirements (`alpha-E-candidate-shortlist.md:242-251`, `:293-295`; `alpha-C-redress-digest.md:154-156`). |
| W0-clamped docs-only admission | Not reopened | SYNTHESIS and Alpha-C keep W0-clamped direct admission pre-blocked without behavior/gate-wave provenance (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:104-106`, `:204`; `alpha-C-redress-digest.md:112-115`). |
| JSON policy leakage into generic crates | Not reopened | Baseline/intervention gates require generated per-grammar runtime proof and fail closed on JSON policy leakage (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:35-42`, `:190-194`, `:212`). |

## CH3 Close

Pass Alpha V1 is REDRESS-honest for CH3. The shortlist and goalset preserve the
SK-V11 measured close, keep pre-blocked routes closed, and only allow reopening
under named material differentials that are stricter than the rejected route
families. No REVISE or REJECT disposition is required for CH3.
