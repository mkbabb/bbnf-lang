# SK-V9 Grand Synthesis

Date: 2026-05-18.

Status: Pass Alpha contract draft for SK-V8 -> SK-V9. V9 implementation is not
dispatched. After alpha challenge convergence the orchestrator must present
G-Alpha, and only after `G-Alpha closed` can skinny passes begin. This Pass
Alpha output deliberately does not create `SPEC.md` or `DISPATCH-PROMPT.md`;
the Section 4.4 wave plan is downstream skinny pass work after G-Alpha.

Post-implementation note (2026-05-19): SK-V9 W1 and W2 landed; W3 was
falsified by REDRESS 96 and REDRESS 97, then retired by REDRESS 98 after
CHALLENGE V4 rejected class-lane-only redress. This Alpha draft is historical
provenance for the SK-V9 contract, not authority to force W3 again. The current
next action is `research/alpha/alpha-G-dispatch-sk-v10.md`.

Authority:

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v8/HANDOFF.md`
- `restart/skinny/tranches/sk-v8/research/skv8-W6-close-and-alpha-feedback.md`
- `restart/skinny/tranches/sk-v8/research/wave-6-hardening/V2/HARDENING-W6-V2-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 91, 92, and 93

## Opening State

SK-V8 closed by W6 V1+V2 hardening convergence. W6 admitted no source,
generated-output, benchmark-row, `skinny/RESULTS.md`, or `skinny/REDRESS.md`
change. The current benchmark authority remains the W0-rendered
`skinny/RESULTS.md` report with 38 `SK-V8-open` manifest rows and overall
`N-direct / NoGo`.

Current main-table state:

| Family | Current state | Contract posture |
|---|---|---|
| `parse_only` | 16 `S / NO-GO`, 1 `L / NO-GO` | Substrate guard and hard-failure evidence only; not SOTA admission while the output plane is borrowed view over offset tape vs DOM and strictness is deferred. |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` | Digest guard plane; direct misses need a direct output contract or control-path tranche before renewed triage. |
| `real_typed_struct` | 4 `A / GO` | Product plane; current GO rows must maintain, and Apache/CITM may enter only through fresh measured row-table admission. |

W6 routes exactly three SK-V9 Alpha behavior candidates. Alpha-E also names two
non-behavior gate prerequisites; those are telemetry/report enablers, not extra
row-moving implementation goals:

| Candidate | W6 source | SK-V9 posture |
|---|---|---|
| Typed row-table admission | REDRESS 91 and W6 Alpha Feedback | Apache/CITM are source/product parity rows only in SK-V8; SK-V9 may measure them only with fresh run-id/metadata validation and row evidence. |
| Structural parse precursor | REDRESS 92 and W6 Alpha Feedback | Define retained class/event grammar and prove the retained `ValueRef` cursor contract before reopening any structural-heavy parse wave. |
| Direct output/control path | REDRESS 93 and W6 Alpha Feedback | Direct digest misses route to a direct output contract or control-path tranche; digest-only evidence remains guard-plane evidence. |

Gate-only Alpha-E prerequisites:

| Prerequisite | Scope | Boundary |
|---|---|---|
| Comparator sidecar same-run manifest | Evidence ingestion for same-run comparator freshness where runnable. | Cannot produce parser data, retained tape data, row output, substrate, or strict admission by itself. DOM sidecars cannot admit digest or typed-direct rows. |
| SK-V9-open telemetry/gate refresh | Refresh report/gate run identity and required Alpha telemetry fields. | Behavior-frozen; cannot move throughput cells or admit Apache/CITM rows without the measured typed-row gate. |

Pass Omega remains separate. SC-6-L1-R1, broad lock amendments, canonical path
cleanup, and top-level surface refresh are not SK-V9 skinny defaults. Omega may
add enforcement or clarification; it may not weaken Lock 14 or authorize
generic JSON policy leaks.

## Section 0 Close Condition And Goalset

### Section 0.1 Alpha Close Condition

This SK-V9 contract is not an implementation dispatch. Alpha closes only when:

1. Alpha challenge returns >=95% ACCEPT with zero open critical defects and no
   orphan REVISE disposition.
2. The presented contract preserves the W6 residual routes and the Pass Omega
   boundary.
3. The goalset below remains telemetry-bound to `skinny/RESULTS.md`.
4. The orchestrator presents G-Alpha to the user.
5. The user returns `G-Alpha closed`.

Only after `G-Alpha closed` may the skinny pass sequence begin. Downstream S-P3
owns the future wave plan after its own P1/P2 entry conditions are met, and it
must consume this Section 0 / Section 4.1-Section 4.3 goalset.

### Section 0.2 SK-V9 Cycle Close Condition

If SK-V9 is later authorized, the cycle closes only when:

1. Every admitted change is tied to a current `SK-V8-open` row, fresh measured
   evidence, and the telemetry schema in Section 4.3.
2. Apache/CITM measured-row admission is either fresh `real_typed_struct A / GO`
   evidence or an explicit REDRESS rejection; SK-V8 source/product parity alone
   cannot count as measured row-table progress.
3. No structural parse implementation starts before the retained class/event
   grammar and retained `ValueRef` cursor contract are proven and accepted.
4. No direct digest route is treated as product proof without a direct output
   contract or control-path tranche.
5. Current GO rows maintain GO unless the accepted plan sets stricter guards.
6. Strict comparator claims use strict same-run or gate-consumed structured
   comparator evidence with output-plane compatibility.
7. REDRESS 91, 92, and 93 are preserved as route boundaries; no rejected SK-V8
   route reopens under another name.
8. Pass Omega residuals remain routed unless Omega has separately ratified
   them.

### Section 0.3 Candidate Goalset

| Candidate | Goal | Admission floor | Rejection path |
|---|---|---|---|
| Typed row-table admission | Turn `apache_builds/real_typed_struct` and `citm_catalog/real_typed_struct` from source/product parity into measured rows. | Fresh run-id/metadata evidence; generated Track 1 DirectBuild; independent serde/oracle path; sonic parity lane; `A / GO` row rendering. | REDRESS records measured-row rejection; do not count as 6 measured real-typed rows. |
| Retained class/event grammar precursor | Define class/event grammar over numbers, literals, container events, and string quote ownership, then prove retained `ValueRef` cursor contract. | Accepted proof artifact plus selected-row thresholds from Section 4.1 before any implementation wave. | REDRESS/routes structural candidate; no parser/tape source admission. |
| Direct output/control path | Make direct rows product-contract-capable or keep them as guard rows. | Selected direct rows must meet both Track 1 and Track 2 direct floors and full-table maintain; digest alone is not product proof. | REDRESS routes direct misses; scalar-parent fold remains rejected. |

### Section 0.4 Alpha Scope And Cost Matrix

This matrix binds Alpha-E candidates for G-Alpha without creating a SK-V9
`SPEC.md`, `DISPATCH-PROMPT.md`, or wave dispatch.

| Candidate | G-Alpha status | LOC budget | Risk | Downstream alignment | Same-wave consumer | Hard cap | Expected row effect |
|---|---|---:|---|---|---|---|---|
| Apache/CITM typed row-table admission | Behavior candidate | 300 | Medium | Future measured-row tranche | `gate-json` consumes run-id, metadata, checksums, selected comparator, and row rendering | <=90 min implementation/redress; split before dispatch if exceeded | May add measured Apache/CITM `real_typed_struct` rows only after strict gate evidence |
| Retained class/event grammar plus `ValueRef` proof | Proof precursor | 450 | High | Future proof tranche before any structural parse implementation | `ValueRef` cursor proof consumes the grammar | <=90 min implementation/redress; split before dispatch if production consumer does not fit | No `RESULTS.md` row movement unless a later capped generated retained Track 1 consumer lands in the same accepted wave |
| Direct output/control-path contract | Behavior candidate | 600 | High | Future direct contract or control-path tranche | Gate/report row classifier consumes the product/control contract | <=90 min implementation/redress; split before dispatch if exceeded | May move selected direct guard rows only under direct floors and full-table maintain |
| Comparator sidecar same-run manifest | Gate prerequisite | 500 | Medium-high | Future telemetry/report tranche | `gate-json` parses and validates the manifest | <=90 min implementation/redress; split before dispatch if exceeded | No behavior or throughput movement; fills freshness/plane cells only |
| SK-V9-open telemetry/gate refresh | Gate prerequisite | 450 | Medium | Future W0-style telemetry tranche | `gate-json` produces and consumes the SK-V9-open manifest | <=90 min implementation/redress; split before dispatch if exceeded | No behavior movement; measured row additions require their own accepted candidate gate |

Any future S-P3 wave plan that exceeds the LOC budget or the <=90 minute
implementation/redress cap returns REVISE before dispatch.

## Alpha Generality And Lock 14 Gate

Any SK-V9 candidate that touches generic CostFacts, codegen, runtime, SIMD,
tape, parser-template, report, or gate surfaces must carry a Lock 14 proof at
Alpha/S-P3 boundary. The proof must include:

- Public API scan: no new public substrate API, directive, BIR variant,
  `BackendShape`, `UnionTape`, or grammar-specific role leakage.
- Grammar branch scan: any grammar-specific policy remains per-grammar or
  generated-provider local.
- Primitive/table scan: no JSON-only primitive, byte-class table, comparator
  registry, or CostFacts rule becomes a generic default without non-JSON proof.
- Role/fact boundary check: structural roles, facts, and comparator semantics
  remain grammar-owned, not parser-owned side state.
- Template/provider boundary check: generated templates and provider modules
  keep JSON policy out of generic crates.
- Non-JSON proof: CSS L4, Sheets, and BBNF-self compile/lower/cost/run, or the
  candidate is demoted to per-grammar JSON-only scope before implementation
  planning.

The retained class/event grammar remains per-grammar/generated unless this
non-JSON proof promotes it to a generic abstraction.

## Section 4.1 Per-Row Close Conditions

The tables below are Pass Alpha goalset gates, not wave dispatches. S-P3 may
select a subset, but any selected row must use these floors or a stricter
accepted floor.

Parse rows use `max(ceil(SK-V8-open Track1 * 1.10), ceil(sonic_strict / 1.10))`
as the minimum selected-row guard threshold. The retained class/event route is
proof-only at Alpha depth: `parse_only` rows remain non-admission and
`skinny/RESULTS.md` remains unchanged unless future S-P3 first defines a capped
implementation wave with a same-wave generated retained Track 1 consumer,
output-plane validation, strict validation posture, and challenge acceptance.

| Row | Current | Target if S-P3 selects row | Candidate | Fallback |
|---|---|---|---|---|
| `twitter/parse_only` | S / NO-GO; T1 9581; sonic strict 18176 | guard threshold >=16524 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `citm_catalog/parse_only` | S / NO-GO; T1 28644; sonic strict 21717 | guard threshold >=31509 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `canada/parse_only` | L / NO-GO; T1 15497; sonic strict 8729 | guard threshold >=17047 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `apache_builds/parse_only` | S / NO-GO; T1 12694; sonic strict 16904 | guard threshold >=15368 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `github_events/parse_only` | S / NO-GO; T1 10689; sonic strict 16408 | guard threshold >=14917 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `update_center/parse_only` | S / NO-GO; T1 11926; sonic strict 18769 | guard threshold >=17063 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `mesh/parse_only` | S / NO-GO; T1 9367; sonic strict 8143 | guard threshold >=10304 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `random/parse_only` | S / NO-GO; T1 10011; sonic strict 15639 | guard threshold >=14218 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `gsoc-2018/parse_only` | S / NO-GO; T1 23209; sonic strict 49101 | guard threshold >=44638 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `marine_ik/parse_only` | S / NO-GO; T1 13100; sonic strict 9921 | guard threshold >=14410 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `instruments/parse_only` | S / NO-GO; T1 13320; sonic strict 17976 | guard threshold >=16342 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `numbers/parse_only` | S / NO-GO; T1 12818; sonic strict 9854 | guard threshold >=14100 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `unicode_mixed/parse_only` | S / NO-GO; T1 6390; sonic strict 9943 | guard threshold >=9040 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `unicode_escapes/parse_only` | S / NO-GO; T1 12731; sonic strict 13851 | guard threshold >=14005 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `unicode_basic/parse_only` | S / NO-GO; T1 11189; sonic strict 15797 | guard threshold >=14361 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `distinct_values/parse_only` | S / NO-GO; T1 10279; sonic strict 18282 | guard threshold >=16620 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |
| `y_string_unicode/parse_only` | S / NO-GO; T1 5577; sonic strict 12009 | guard threshold >=10918 Mbps; `parse_only` remains non-admission unless a strict product/DOM plane gate is separately accepted | retained class/event grammar + `ValueRef` cursor proof | reject/reroute; no row-table admission |

Direct rows use `ceil(sonic_strict / 1.10)` as the minimum selected-row floor
for both Track 1 and Track 2, plus full-table maintain. The direct plane stays
guard-only unless S-P3 first defines a direct output contract or control-path
tranche.

| Row | Current | Direct target | Candidate | Fallback |
|---|---|---|---|---|
| `twitter/direct_to_struct` | N-direct / NO-GO; T1 11859; T2 9881; sonic strict 12890 | T1 and T2 >=11719 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `canada/direct_to_struct` | N-direct / NO-GO; T1 6586; T2 9769; sonic strict 12430 | T1 and T2 >=11300 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `apache_builds/direct_to_struct` | N-direct / NO-GO; T1 8306; T2 7796; sonic strict 8852 | T1 and T2 >=8048 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `github_events/direct_to_struct` | N-direct / NO-GO; T1 9088; T2 7337; sonic strict 9818 | T1 and T2 >=8926 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `update_center/direct_to_struct` | N-direct / NO-GO; T1 7863; T2 7514; sonic strict 10525 | T1 and T2 >=9569 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `mesh/direct_to_struct` | N-direct / NO-GO; T1 8640; T2 9049; sonic strict 9967 | T1 and T2 >=9061 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `random/direct_to_struct` | N-direct / NO-GO; T1 7751; T2 6952; sonic strict 8141 | T1 and T2 >=7401 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `gsoc-2018/direct_to_struct` | N-direct / NO-GO; T1 15042; T2 14380; sonic strict 23356 | T1 and T2 >=21233 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `instruments/direct_to_struct` | N-direct / NO-GO; T1 8494; T2 8766; sonic strict 9872 | T1 and T2 >=8975 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `numbers/direct_to_struct` | N-direct / NO-GO; T1 9773; T2 6966; sonic strict 7953 | T1 and T2 >=7230 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `unicode_mixed/direct_to_struct` | N-direct / NO-GO; T1 3596; T2 3694; sonic strict 10077 | T1 and T2 >=9161 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `unicode_escapes/direct_to_struct` | N-direct / NO-GO; T1 4020; T2 4016; sonic strict 13999 | T1 and T2 >=12727 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `distinct_values/direct_to_struct` | N-direct / NO-GO; T1 4438; T2 4151; sonic strict 8950 | T1 and T2 >=8137 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |
| `y_string_unicode/direct_to_struct` | N-direct / NO-GO; T1 4828; T2 3563; sonic strict 9065 | T1 and T2 >=8241 Mbps on selected rows, plus full-table maintain | direct output contract or control-path tranche | reject/reroute; digest remains guard-plane only |

Current GO rows must maintain unless a selected-row gate sets stricter floors:

| Row | Current | Maintain target |
|---|---|---|
| `twitter/real_typed_struct` | A / GO; T1 15333; T2 14516; sonic strict 13646 | maintain GO and T1 >=15027 Mbps unless a stricter selected-row gate applies |
| `citm_catalog/direct_to_struct` | A / GO; T1 21151; T2 19434; sonic strict 18241 | maintain GO and T1 >=20728 Mbps unless a stricter selected-row gate applies |
| `update_center/real_typed_struct` | A / GO; T1 11958; T2 10367; sonic strict 11952 | maintain GO and T1 >=11719 Mbps unless a stricter selected-row gate applies |
| `mesh/real_typed_struct` | A / GO; T1 9623; T2 7674; sonic strict 9305 | maintain GO and T1 >=9431 Mbps unless a stricter selected-row gate applies |
| `marine_ik/direct_to_struct` | A / GO; T1 9357; T2 9488; sonic strict 8559 | maintain GO and T1 >=9170 Mbps unless a stricter selected-row gate applies |
| `marine_ik/real_typed_struct` | A / GO; T1 11783; T2 8321; sonic strict 6951 | maintain GO and T1 >=11548 Mbps unless a stricter selected-row gate applies |
| `unicode_basic/direct_to_struct` | A / GO; T1 9363; T2 8420; sonic strict 8971 | maintain GO and T1 >=9176 Mbps unless a stricter selected-row gate applies |

Typed row-table candidates:

| Row | Current | SK-V9 target | Fallback |
|---|---|---|---|
| `apache_builds/real_typed_struct` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | Fresh measured `A / GO` row with run-id/metadata validation, generated Track 1 DirectBuild, independent serde/oracle proof, and sonic parity lane | REDRESS measured-row rejection; do not present as measured |
| `citm_catalog/real_typed_struct` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | Fresh measured `A / GO` row with run-id/metadata validation, generated Track 1 DirectBuild, independent serde/oracle proof, and sonic parity lane | REDRESS measured-row rejection; do not present as measured |
| `canada/real_typed_struct` | Rejected by REDRESS 91 on full-fixture DirectBuild-vs-serde checksum mismatch | Pre-blocked until a fresh full-fixture checksum proof exists; no length-only or digest-only typed proof | Keep routed |

## Section 4.2 Strict Comparator Gate

Every SK-V9 row must emit comparator Mbps, percent delta, strictness plane,
output plane, freshness, and hot leaf. Strict admission is rejected if the row
uses `Strictness=deferred`, stale sidecar-only evidence, a lossy/permissive
comparator, output-plane mismatch, missing measured validation, missing c/B or
sample cost, or missing hot-leaf attribution.

Comparator requirements:

| Comparator | SK-V9 use |
|---|---|
| sonic-rs strict | Mandatory same-run strict anchor where runnable. Direct and typed rows must use the matching output plane. |
| sonic-rs lossy | Optional flaw probe only; never strict admission. |
| simdjson C++ DOM | Required if a structured same-run manifest exists; otherwise planning signal only. |
| simdjson On Demand | Optional named plane; never substituted for DOM or typed/direct output. |
| yyjson default strict | Required if runnable with disclosed default strictness and same-run freshness; historical rows are planning only. |
| asmjson SWAR | Optional flaw probe; disclose permissive behavior when applicable. |
| asmjson AVX-512 | Optional x86 strict comparator; absent on this aarch64 host unless separately collected. |
| RapidJSON default | Optional flaw probe because default permissiveness prevents strict admission. |
| serde_json | Required strict reference baseline where runnable; direct/typed use must disclose output-plane compatibility. |

## Section 4.3 Telemetry Binding

`skinny/RESULTS.md` and any gate-consumed manifest for SK-V9 must carry this
schema. `gate-json` is the JSON instance of the grammar-aware report contract;
generic report/gate code must not encode JSON comparator policy as the universal
schema. CSS, Sheets, and BBNF-self comparator anchors are domain-specific or
explicitly absent. `gate-json` must reject any row missing required fields.

| Column | Type | Required |
|---|---|---|
| grammar_id | string | yes |
| domain | string | yes |
| Corpus | string | yes |
| Workload | enum (`parse_only`, `direct_to_struct`, `real_typed_struct`, `parse_full_traversal`, `path_lookup`, `unicode_string_float`, `memory`, `cycles_per_byte`) | yes |
| Outcome | enum (`A`, `C`, `G`, `K`, `L`, `N-direct`, `S`) | yes |
| Verdict | enum (`GO`, `NO-GO`) | yes |
| Strictness | enum (`strict`, `permissive`, `deferred`) | yes |
| parse_utf8 | enum (`scan-boundary`, `view-boundary`, `none`) | yes |
| escape_complete | enum (`yes`, `no`) | yes |
| flaw_probe | string | yes |
| Output plane | string (`DOM`, `typed direct`, `direct output`, `digest`, `iterator`, `borrowed view`) | yes |
| Track 1 Mbps | number | yes |
| Track 2 Mbps | number or `n/a` | yes |
| sonic-rs strict Mbps | number or `n/a` | yes |
| sonic-rs lossy Mbps | number or `n/a` | optional flaw probe |
| simdjson DOM Mbps | number or `n/a` | yes if runnable |
| simdjson On Demand Mbps | number or `n/a` | optional |
| yyjson default Mbps | number or `n/a` | yes if runnable |
| asmjson SWAR Mbps | number or `n/a` | optional flaw probe |
| asmjson AVX-512 Mbps | number or `n/a` | optional x86 strict comparator |
| RapidJSON default Mbps | number or `n/a` | optional flaw probe |
| serde_json Mbps | number or `n/a` | yes |
| Delta vs SK-V8 | number or percent | yes |
| Delta vs sonic-strict | percent | yes |
| Delta vs simdjson DOM | percent or `n/a` | yes |
| Delta vs yyjson | percent or `n/a` | yes |
| Hot leaf | string with top symbol and self-time or criterion artifact binding | yes |
| c/B or sample cost | number or structured sample-cost string | yes |
| Profile artifact | path | yes |
| Run id | string | yes |
| Host/build metadata | string | yes |
| CostFacts ids | chosen/rejected ids or explicit `none` | yes |
| Redress entry | id or `none` | yes |
| Sidecar freshness | enum/string (`same-run-native`, `same-run-sidecar`, `historical`, `absent`) | yes |
| comparator_id | string or structured list | yes |
| comparator_plane | string or structured list | yes |
| comparator_strictness | string or structured list | yes |
| comparator_freshness | string or structured list | yes |
| measured_validation_path | string | yes |
| Substrate surface | string | yes |
| Structural projection status | string | yes |
| Substrate cardinality | enum/string (`one`, `parallel`, `n/a`) | yes |
| Same-wave consumer class | string | yes |
| Track 2 independence status | string | yes |
| Signal | string (`PASS` / `NO-GO` with reason) | yes |

The telemetry binding is the auto-converge feedback signal. If SK-V9 later
closes with this goalset met, this Alpha contract was correct. If SK-V9 closes
unmet, the next Alpha iteration must revise the candidate shortlist and
goalset rather than relabeling rejected routes.

## Pre-Blocked Routes

Do not reopen without fresh measured evidence, same-wave consumer, explicit
row thresholds, REDRESS citation, and challenge acceptance:

- The full prior pre-block ledger in
  `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md` is
  binding by reference. Any candidate touching a rejected ownership boundary
  must cite the REDRESS item and pass a changed-shape proof before
  implementation planning.
- REDRESS 91 row-table overclaim: Apache/CITM are not measured SK-V8 rows.
- REDRESS 91 `canada/real_typed_struct` checksum mismatch.
- REDRESS 92 W3 Tier A implementation before retained class/event grammar and
  `ValueRef` cursor proof.
- REDRESS 93 Track 2 scalar-parent fold, including renamed parent-digest folds
  without W4/V9-aware checked gate, full-table maintain, and independent Track
  2 digest-arithmetic backstop.
- REDRESS 73 helper-shape transfer: generated retained array continuation
  shape must not be assumed to transfer monotonically to hand Track 2 or direct
  control paths; future Track 2/control work must profile the hand parser's
  code layout directly.
- Sidecar substrate, parser-owned cursor/fact slots, `UnionTape`, new
  `BackendShape`, new directive/BIR, public substrate API, and `tape_vs_tape`
  as production consumer.
- PMULL prefix-XOR and CTZ/bulk production rewires as default hot paths.
- Generic JSON policy in generic crates or any weakening of Lock 14.

## G-Alpha Boundary

After alpha challenge convergence, present this contract for G-Alpha. V9
implementation is not dispatched by this document. Only after `G-Alpha closed`
can the skinny pass sequence begin, and downstream S-P3 must author the detailed
wave plan from this goalset before any implementation wave exists.
