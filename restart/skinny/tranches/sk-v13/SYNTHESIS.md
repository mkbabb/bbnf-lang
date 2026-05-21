# SK-V13 Grand Synthesis

Date: 2026-05-21.

Status: Alpha-F contract draft for SK-V13. This file is the master synthesis
and close contract. It does not author `SPEC.md` or `DISPATCH-PROMPT.md`; S-P3
must derive those later from this goalset after the required G-Omega gate.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` through REDRESS-127
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-pass-framework-leverage.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`

The 2026-05-21 user pin addendum overrides any prior SK-V12 or SK-V13 clause
that treats CSS as one-row complete, treats `parse_only` as diagnostic-only,
permits JSON rows below strict sonic-rs, or allows tranche close by ordinary
fixpoint without architectural-level intrinsic-block evidence.

## Section 0 - Close Condition And Goalset

SK-V13 closes only by full ADMIT or by architectural-level intrinsic-block
proofs for every remaining row or feature. Implementation-limited misses are
not close evidence; they are reopens. If any goal below remains unmet without
architectural-block proof, SK-V13 close is REJECT and Pass Alpha immediately
brackets SK-V14 under the same pinned bar.

### 0.1 G1 - CSS L4 Parity

Target: full semantic parity with lightningcss for the SK-V13 CSS L4 parity
matrix. The campaign target is 24 non-OUT_OF_SCOPE CSS features. SK-V12
admitted 1 feature family through `css_l4/declaration_values/direct_to_struct/main`;
23 remain to admit or prove architecturally blocked.

Each CSS admission requires:

1. strict equality vs lightningcss on the same corpus, output plane, and host;
2. feature-coverage match: every accepted lightningcss variant is accepted,
   and every rejected lightningcss variant is rejected;
3. Track 1 throughput greater than `lightningcss_mbps + 1`;
4. independent oracle evidence from cssparser or a hand-checked golden table
   for productions cssparser does not cover;
5. gate-consumed provenance for fixture, grammar, generated source/runtime,
   comparator, equality artifact, host, run id, and REDRESS id.

No CSS feature may remain `PARTIAL` at close. It is either `ADMITTED-PARITY` or
`ARCHITECTURAL-BLOCK`.

### 0.2 G2 - Decision-Engine Fold

The hardcoded P1-P8 backend-shape cascade must be replaced or structurally
superseded by the decision-engine fold: bbnf-regex extraction, e-graph
Language wiring, active `egg::CostFunction` cost selection, CSP resolver, and
cascade deletion or gated retirement. The fold must preserve JSON behavior and
must not introduce grammar-specific branches in generic crates.

Abrogate-before-patch criteria from scoping are binding: e-graph OOM, CSP
solve time over the wave threshold, stale cost evidence over 30 percent of
candidate expressions, or order-dependent rewrites over 10 percent variance
must route to REDRESS with a simpler measured fallback rather than ad hoc
patching.

### 0.3 G3 - Union Variant

At least one fresh union-substrate variant must admit, or the tranche must
record an architectural-level block. REDRESS 96/97/98 are history, not category
blockers. Any reopen must cite the prior REDRESS entries and name a material
differential, such as grammar-configured per-rule shape selection, e-graph
equivalence-class union, or SIMD-first lane-index routing.

Admission requires parity, measured row movement, same-wave consumer wiring,
and no hidden Track 1/Track 2 plane collapse.

### 0.4 G4 - Zero AArch64 Orphans

SK-V13 close requires zero aarch64 production orphans. A primitive is closed
only if it is wired to a same-wave production consumer and measured, or deleted
or demoted with REDRESS evidence. The SK-V12 W4 demotions are history only.
New primitives cannot be retained for a future tranche without a consumer.

The W4 `a64_ascii_set_run_skip` production split must either wire into a CSS
scan-block consumer with strict equality and throughput evidence, or record a
measured rejection. No second production-split deferral is permitted.

### 0.5 G5 - Every JSON Row Above Strict Sonic

All 17 JSON corpora across all 3 planes must beat sonic-rs strict on the same
plane, same corpus, and strict equality semantics, or carry per-row
architectural-level intrinsic-block proof. This is 51 JSON rows:
`parse_only`, `direct_to_struct`, and `real_typed_struct` for every corpus.

The prior `parse_only` diagnostic concession is revoked. The 17 `parse_only`
rows are admission-eligible and must satisfy
`Track 1 > sonic-rs strict parse_only Mbps + 1` or prove architectural block.
REDRESS-119 and REDRESS-120 are history only; their 13-row direct fixpoint does
not block fresh SK-V13 reopens.

No previously admitted A/GO row may silently demote. If a wave changes generic
runtime, codegen, generated output, benchmark, report, or gate paths that can
produce JSON, it must refresh the JSON guard run or record measured REDRESS.

### 0.6 G6 - Totality V1.1 And G-Omega

Totality V1.1 must be ratified before SK-V13 Wave 0 dispatch. G-Omega is a
hard pre-W0 gate. The Omega cycle must fold the SK-V12 CSS admission,
GrammarConfig/Lock 14 evidence, REDRESS-119/120/121-127 lessons, Lock 16
SIMD/checkasm discipline, non-JSON telemetry schema, and zero-orphan evidence
into the canonical totality surfaces.

S-P1/S-P2/S-P3 may prepare SK-V13 research and planning, but no implementation
Wave 0, source edit wave, or RESULTS/REDRESS-writing wave may start until
G-Omega is closed.

### 0.7 G7 - Indefatigable No-Demotion Close

SK-V13 must not reduce the admitted surface. Rows that move backward fail the
bracket unless the wave records a measured architectural-block disposition and
the user re-pins the scope. If SK-V13 does not fully admit G1-G6, Pass Alpha
brackets SK-V14 immediately. The same rule continues SK-V14, SK-V15, and later
until full ADMIT or architectural-block proof covers every row and feature.

## Section 1 - Corrected Diagnosis

SK-V12 was a valid PASS-ADMIT under the older close clause because it proved
one generated CSS L4 row above lightningcss with strict equality, preserved
JSON guards, resolved the escape-mask prerequisite, and reduced the aarch64
orphan inventory to zero by evidence. It was not a full campaign close under
the 2026-05-21 addendum.

SK-V13 starts from this corrected state:

| Surface | Current state | SK-V13 consequence |
|---|---|---|
| CSS L4 | 1 admitted declaration-values feature family | 23 remaining target features must admit or prove architectural block |
| JSON `parse_only` | 17 rows previously classified diagnostic NO-GO | all 17 reopen as SOTA rows vs sonic-rs strict |
| JSON `direct_to_struct` | 4 A/GO, 13 N-direct/NO-GO under REDRESS-119 history | all 17 must exceed sonic-rs strict; 13 residuals reopen equally |
| JSON `real_typed_struct` | 7 A/GO plus incomplete full-plane admission | all 17 must be measured against sonic-rs strict and admitted or blocked |
| Decision engine | passive cost ledger and hardcoded P1-P8 cascade | must fold bbnf-regex, e-graph, active cost, CSP, and cascade retirement |
| Value API | GrammarConfig metadata exists but string/number/sink/flag leaks remain | must expand per-grammar config and generated sinks/views as needed |
| SIMD/ASM | W4 microbench proved a delimiter primitive, but production split remains | must wire or reject; no new orphan retention |
| Totality | V1 lacks several SK-V11/SK-V12 lessons | G-Omega must ratify V1.1 before W0 |

## Section 2 - Telemetry Binding

SK-V13 `skinny/RESULTS.md`, companion reports, and gates must emit and consume
the full schema below. Missing required fields fail closed. JSON, CSS, union,
SIMD, and decision-engine reports may add domain-specific fields, but cannot
omit the common columns.

| Column | Required rule |
|---|---|
| `row` / `Corpus` / `Workload` | required; JSON rows must cover 17 corpora x 3 planes |
| `Outcome` / `Verdict` | required; `A/GO` only after strict equality and SOTA margin |
| `Strictness` | required; SOTA anchor must be strict |
| `parse_utf8` / `escape_complete` / `flaw_probe` | required for JSON; CSS equivalent fields must name strictness and recovery mode |
| `Output plane` | required; comparisons only count on same plane |
| `Track 1 Mbps` / `Track 2 Mbps` | required for every row |
| `sonic-rs strict Mbps` | required for every JSON row and plane |
| `sonic-rs lossy Mbps` | optional flaw probe only; never SOTA anchor |
| `simdjson DOM Mbps` / `simdjson On Demand Mbps` | required when runnable; plane disclosed |
| `yyjson default Mbps` | required when runnable; strictness disclosed |
| `asmjson SWAR Mbps` / `asmjson AVX-512 Mbps` | optional flaw probes unless same-plane strict runnable |
| `RapidJSON default Mbps` | optional flaw probe only |
| `serde_json Mbps` | required JSON strict baseline |
| `lightningcss Mbps` | required for every CSS parity row |
| `cssparser_oracle Mbps` or golden oracle | required for every CSS parity row |
| `Delta vs SK-V12` | required for every carried row |
| `Delta vs SOTA` | required; sonic-rs strict for JSON, lightningcss for CSS |
| `Hot leaf` | required; stale inherited profile names fail S-P1 |
| `Signal` | required; PASS or NO-GO with reason |
| `REDRESS id` / `wave id` / `run id` / `host` | required for admission |

The gate must reject stale run ids, mixed output planes, permissive SOTA
anchors, report-only Mbps, producer-only telemetry, missing equality artifacts,
and any row lacking source provenance.

## Section 3 - Rolling SOTA Delta

Every Pass Alpha bracket must publish or refresh
`restart/skinny/ROLLING-SOTA-DELTA.md` with one row for every JSON row/plane
and every CSS feature:

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|

The rolling table is a close gate, not a status appendix. A row with negative
margin remains open unless it carries architectural-block proof. A row whose
margin regresses from the prior tranche fails G7 unless explicitly admitted by
architectural-block/user re-pin. S-P3 must add the concrete gate command and
artifact path in `SPEC.md`.

## Section 4 - S-P3 Constraints

S-P3 owns the detailed wave plan, but it is constrained by this contract:

- wave plans must target G1-G7, not a shorter shortlist;
- every behavior wave must move at least one row toward SOTA or record an
  architectural-block proof for the row family it touched;
- support-only landings are invalid unless they are same-wave wired to a
  measured consumer;
- independent waves may be dispatched concurrently only when file domains do
  not overlap and after required gates close;
- W10-style CSS expansion, W11-style JSON residual reopening, and W14-style
  `parse_only` admissions must be represented unless S-P3 proves a different
  sequence covers all rows;
- no SPEC clause may defer pinned work to a future tranche except the automatic
  Pass Alpha bracket after a rejected close.
- this SYNTHESIS, the HANDOFF, and the 2026-05-21 addendum override weaker
  scoping prose. S-P3 must not inherit scoping labels such as optional,
  fallback, diagnostic, support-only, scaffold-only, or future-tranche when they
  touch pinned CSS/JSON/G2-G7 work; those items become admitted row targets,
  architectural-block proofs, or user re-pin issues.
- no SPEC clause may authorize a new directive, BIR variant, `BackendShape`,
  public substrate API, or grammar-specific generic behavior. The union category
  is unblocked only for same-tape, codegen-private, row-consumed variants.
- any SPEC wave that wires `bbnf-simd` into CSS, union, JSON `parse_only`, or
  shared generated code must include `G-SIMD-GRAMMAR-POLICY`: the selected
  classifier must use the consuming grammar's quote/escape/control policy or a
  no-string policy, with scalar parity, checkasm/differential coverage for JSON
  and CSS policies, same-wave measured row consumption, no public substrate API,
  and no retained sidecar classifier state.
- after the decision-engine resolver lands, the hardcoded P1-P8 cascade must
  fail closed for JSON, CSS, Sheets, and BBNF-self rows unless S-P3 records an
  explicit row rejection/non-admission path. Silent fallback to the old cascade
  is not admission evidence.

## Section 5 - Pre-Blocked And Unblocked Routes

Pre-blocked:

- claiming SK-V13 close from the single SK-V12 CSS declaration-values row;
- using lossy sonic-rs, permissive RapidJSON, or different output planes as a
  SOTA anchor;
- treating `parse_only` as diagnostic-only;
- closing a JSON row through REDRESS-119 history without fresh SK-V13 evidence;
- producer-only SIMD, union, resolver, or codegen artifacts without same-wave
  consumer measurement;
- non-JSON or shared consumers of `bbnf-simd` alphabet-only classifier dispatch
  unless `G-SIMD-GRAMMAR-POLICY` proves the selected path cannot inherit JSON
  quote/escape/control constants;
- grammar-name branches in generic crates, parser-owned sidecars, hidden
  Track 1/Track 2 coupling, or stale comparator sidecars;
- dispatching Wave 0 before G-Omega closes.

Unblocked:

- all 51 JSON rows, including REDRESS-119 residuals and all `parse_only` rows;
- the full CSS parity matrix under strict lightningcss equality;
- union-substrate category attempts with fresh material differential;
- SIMD/ASM attempts after scalar reference, checkasm/parity, Lock 16, and
  same-wave consumer evidence;
- decision-engine replacement of the hardcoded cascade under bounded abrogate
  criteria.

## Section 6 - Close Posture

The SK-V13 contract is intentionally aggressive. It is not a one-row tranche
and not a paper fixpoint. The close is the pinned bar: full CSS parity, every
JSON row and plane above strict sonic-rs, decision-engine fold, one union
admission or architectural block, zero aarch64 orphans, totality V1.1
ratification, and no demotion. Anything less brackets forward immediately.
