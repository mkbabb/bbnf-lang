# SK-V13 S-P1 V2 CH3: Regression / REDRESS Review

Pass: S-P1 Profile. Cycle: V2.
Date: 2026-05-21.
Lens: CH3 REGRESSION.
Scope: adversarial review of the six S-P1 V2 artifacts under
`restart/skinny/tranches/sk-v13/research/p1/` for uncited REDRESS-route
reopening.
Disposition: REVISE.

## §1 - Method

Reviewed:

- `restart/prompts/skinny/PASS-1-PROFILE.md` §3 CH3.
- `skinny/REDRESS.md`, with special attention to REDRESS 50-55, 60-72,
  80, 82-84, 88-90, 96-98, 119-120, and 126-127.
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.

CH3 standard: S-P1 may surface anomalies and profile signals, but it must not
turn anomaly wording into an implied implementation route already blocked by
REDRESS. If a profile signal points at a pre-blocked route, the artifact must
cite the relevant REDRESS entry, mark the route as history/pre-blocked, and name
the material differential required before S-P2/S-P3 can use it.

The USER-PIN addendum changes the status of REDRESS 119/120: those direct
fixpoints are history only and the rows are wave-eligible, but reopens still
must cite the prior fixpoint and name the material differential.

## §2 - Findings

### CH3-V2-R1 - Direct-row progress signals still need REDRESS 119/120 + material-differential language

Disposition: REVISE.

Evidence:

- `p1f-results-delta.md:30`-`32` correctly says P1-F classifications are
  `profile_signal_not_gate_admission` and only later gate-json/REDRESS waves can
  admit or demote rows.
- `p1f-results-delta.md:52`-`72` lists all 17 direct rows as V2 measured direct
  progress signals, including residual N-direct families.
- `p1f-results-delta.md:108`-`120` flags comparator sidecar, same-run sonic
  parse PMU, typed-surface, and CSS method gaps, then reiterates profile-only
  classification.
- `USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md:60`-`65` makes REDRESS 119/120
  history only, but requires every reopen to cite the fixpoint and name the
  material differential.
- `skinny/REDRESS.md:3531`-`3552` records REDRESS 120 as a measured SK-V11
  fixpoint and REDRESS 119 as the prior direct-row authority.

Problem:

V2 no longer claims direct admission from profile data, which is correct. The
remaining regression risk is inline provenance: the direct progress table and
comparator/sidecar anomalies do not carry the REDRESS 119/120 + USER-PIN
material-differential guard next to the rows. A downstream S-P2/S-P3 reader
could treat fresh V2 PMU/samply evidence as a direct residual reopen even though
P1-F supplies no route differential.

Required fold action:

In P1-F and the V2 consolidated hardening, add an inline guard beside the direct
row table and comparator/sidecar anomalies: REDRESS 119/120 are history under
the USER-PIN addendum, but V2 profile evidence is not itself a direct-row
reopen, admit, or demotion. Every direct residual reopen must cite REDRESS
119/120, name the fresh material differential, and carry same-harness strict
comparator plus micro-proof evidence.

### CH3-V2-R2 - Pre-pin route guards remain incomplete around dispatch, masking, and unescape signals

Disposition: REVISE.

Evidence:

- `PASS-1-PROFILE.md:137`-`141` requires anomalies that point at pre-blocked
  REDRESS routes to cite and mark those routes pre-blocked.
- `p1a-samply-mode-1.md:131`-`135` flags save-only/line-risk attribution and a
  masking signal where Track 1 collapses to `dispatch_value`, with exceptions
  in tiny-string and hex-escape leaves.
- `p1b-samply-mode-2.md:110`-`115` says generated direct envelopes are not a
  license for a broad dispatch rewrite, then names `unescape_string` as a clean
  primitive attribution.
- `p1e-hot-leaf-attribution.md:88`-`91` repeats that direct Track 1 is mostly a
  generated dispatch envelope and names `unescape_string` as the clearest direct
  primitive.
- `skinny/REDRESS.md:216`-`224` and `:291`-`297` reject dispatch-table /
  function-pointer alternates.
- `skinny/REDRESS.md:715`-`813` rejects parse-time aux side tables, byte-class
  event cursors, and parser-local structural-mask cursors.
- `skinny/REDRESS.md:3445`-`3460` records the decoded-byte source-method route
  as REDRESS 54/55/66/69-adjacent rejected history.

Problem:

The V2 artifacts are careful to call these profile facts, not implementation
plans. However, the anomaly text still names `dispatch_value`,
`parse_*_direct`, tiny-string, and `unescape_string` signals without the
pre-pin REDRESS citations required by the CH3 contract. That leaves ambiguity
around dispatch-table/function-pointer rewrites, parse-time side-table/cursor
routes, and decoded-source/digest sink routes.

Required fold action:

In P1-A, P1-B, P1-E, and consolidated hardening, add a pre-pin route guard near
the dispatch/masking/unescape anomaly text: no parse-time aux side table, event
cursor, parser-local structural cursor, dispatch-table/function-pointer
alternate, decoded-string stats sink, generic source visitor, or source-method
digest fold is reopened by the V2 profile. Any S-P2/S-P3 route using these
signals must cite the relevant pre-pin REDRESS family and name a material
differential beyond the rejected route.

### CH3-V2-R3 - SIMD/orphan accounting is better, but REDRESS-126 zero-orphan carry-forward is still missing

Disposition: REVISE.

Evidence:

- `p1c-samply-mode-3.md:110`-`112` correctly says structural SIMD/scalar wins
  do not reopen REDRESS 96/97/98 and any union attempt must cite a material
  differential.
- `p1d-pmu-cycles.md:127`-`129` repeats the REDRESS 96/97/98 guard.
- `p1e-hot-leaf-attribution.md:92`-`94` keeps structural SIMD evidence as a
  fresh-measurement antecedent, not a route.
- `p1c-samply-mode-3.md:119`-`122` marks PEXT as out on aarch64 and leaves
  line-poor NEON symbols as CH6 risk.
- `p1e-hot-leaf-attribution.md:64` and `:97`-`99` expose function-only ASM
  attribution for `bulk_emit_positions_64_neon` and other ASM/system leaves.
- `p1f-results-delta.md:110`-`112` says simdjson, yyjson, asmjson, and RapidJSON
  sidecars remain absent or `n/a`.
- `skinny/REDRESS.md:3864`-`3871` records REDRESS-126 as the ASM-gen
  route-production split and final zero aarch64 orphan disposition.

Problem:

V2 closes the main REDRESS 96/97/98 reopen risk, but it still lacks an explicit
REDRESS-126 no-orphan clause beside PEXT, line-poor NEON, `bulk_emit_positions`,
and sidecar-gap language. Without that clause, absent PEXT/SIMD sidecars or
function-only ASM leaves could be reinterpreted as unowned SIMD primitives
rather than telemetry gaps.

Required fold action:

In P1-C, P1-E, P1-F, and consolidated hardening, carry forward REDRESS-126:
absent PEXT/SIMD sidecars and line-poor ASM leaves are telemetry or CH6
attribution gaps only. They do not create new orphan SIMD primitives, reopen
PMULL/CSSC/PREFIX-XOR or `bulk_emit_positions_64` production routes, or bypass
zero-orphan accounting. Any future SIMD primitive candidate must name a
same-wave consumer, scalar reference, parity/checkasm evidence, feature-mask
disclosure, and zero-orphan disposition.

## §3 - Non-Findings

- REDRESS 96/97/98 are no longer silently reopened by the structural SIMD
  anomaly when read in isolation: P1-C, P1-D, and P1-E each say the scanner
  micro-result is not a union-substrate route and requires a material
  differential before use.
- P1-C correctly marks `alternate_pext_mask_plan` unsupported on aarch64 and
  `alternate_dispatch_table_plan` unsupported/invalid rather than recording them
  as missing rows.
- P1-F correctly keeps CSS V2 throughput as method-mismatched hot-leaf/equality
  signal, not SK-V12 demotion or REDRESS movement.
- No V2 artifact proposes a source patch, generated runtime rewrite, benchmark
  gate change, or `skinny/RESULTS.md` movement.

## §4 - Required Consolidated Fold

S-P1 V2 should not converge under CH3 until the documentation guards above are
folded:

1. P1-F must carry REDRESS 119/120 and USER-PIN material-differential language
   beside direct-row progress, stale comparator, and sidecar-gap signals.
2. P1-A/P1-B/P1-E must cite the pre-pin rejected-route families beside
   dispatch/masking/unescape signals.
3. P1-C/P1-E/P1-F must carry REDRESS-126 zero-orphan language beside
   PEXT/SIMD/ASM sidecar and attribution gaps.
4. Consolidated hardening must mark these as CH3 documentation folds, not S-P2
   implementation tickets.

## §5 - Final Disposition

Disposition: REVISE.

Rationale: V2 substantially improves the V1 CH3 posture. It does not make a
material-differential-free implementation proposal, and it explicitly fences
the core REDRESS 96/97/98 union-substrate risk. The remaining failures are
provenance and guard placement: direct-row, pre-pin, and SIMD/orphan anomalies
still need inline REDRESS citations and material-differential language so later
planning cannot silently reinterpret profile signals as reopened routes.
