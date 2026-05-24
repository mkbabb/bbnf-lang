---
agent: 2D
pass: T-P2-research
cycle: V2
generated_at: 2026-05-23T23:30:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 12
counted_source_ids: [SRC-01, SRC-02, SRC-03, SRC-04, SRC-05, SRC-06, SRC-07, SRC-08, SRC-09, SRC-10, SRC-11, SRC-12]
techniques_grounded: 7
techniques_refuted: 5
v2_fold_packet_consumed:
  - CH1-V1-BLK-01 (Sneller source-repo URL dead; replaced with Wayback-pinned snapshot at SHA 86e9f118cf6517220d8dc8e0af788e1a312fc056 captured 2024-01-11; counted_source_ids register added per V3 convention)
  - CH2-V1-item-2 (cite 2C V3 V2-FOLD / V3-FOLD canonical Lock 14 transfer contract; add Per-Technique Transfer Coverage table for BURG / egg / CSP / SinkOnly / CollapsedStage / EagerTape / OffsetTape / EventTape across CSS L4 / Sheets / BBNF-self)
  - CH4-F2 (4-of-5 marker-string lowerers cross-referenced to 2B-primitive-vocabulary.md:73-74 + crates/codegen/src/lower/{eager,offset,event,collapsed}_tape.rs:15-17; admission gate strengthened in dossier prose; per-shape ledger surfaced)
  - CH5-F-CH5-V1-03 (admits_collapsed_stage predicate at passes/src/lib.rs:874-876 bound to refuse aarch64 admission; every BackendExpr node / rewrite guard / extraction result declares substrate_target)
  - CH6-F8 (same_wave_consumer cell added per T2D-* grounded row — egg/BURG/Mison citation grounds the class, not the bbnf admission; rows downgrade to `partial` where the cell is not namable at V2 fold time)
  - CH6-F11 (Open Research Question rows acquire explicit wave or pass anchor: T-P3 §3C, S-P3 W{N}, or abrogated to T-P3 backlog)
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised:
    - T2D-COLLAPSEDSTAGE-X86-ONLY (V1 partial → V2 partial-with-predicate-bind; aarch64 admission predicate refined per F-CH5-V1-03)
    - T2D-EGRAPH-EXTRACTION (V1 grounded → V2 grounded-partial-pending-consumer; same_wave_consumer cell required per F8)
    - T2D-BURG-FINITE-ALTERNATIVES (V1 grounded → V2 grounded-partial-pending-consumer; same_wave_consumer cell required per F8)
    - T2D-CSP-FEASIBILITY-LAYER (V1 grounded → V2 grounded-partial-pending-consumer; same_wave_consumer cell required per F8)
    - T2D-EQSAT-ORIGIN (V1 grounded → V2 grounded-class-only; same_wave_consumer cell required per F8 — citation grounds *class*, not *admission*)
  first_cycle_additions:
    - T2D-EGRAPH-EXTRACTION
    - T2D-EQSAT-ORIGIN
    - T2D-BURG-FINITE-ALTERNATIVES
    - T2D-CSP-FEASIBILITY-LAYER
    - T2D-REGEX-NFA-DFA-PLAN
    - T2D-TAPE-STAGED-MATERIALIZATION
    - T2D-SINKONLY-PROJECTION
    - T2D-COLLAPSEDSTAGE-X86-ONLY
    - T2D-P1P8-CASCADE-NOT-OPTIMIZER
    - T2D-EIGHT-STEP-ORDER-NOT-CANONICAL
    - T2D-FIVE-SHAPE-FINITE-SET
locks_amendment_candidates: 6
---

## Executive Summary

The literature supports a **search-and-extraction** decision-engine class
for backend-shape selection (equality saturation + cost-model extraction
+ constraint-feasibility filter), not a hardcoded `P1..P8` priority
cascade. The five-shape `BackendShape` enum is a *finite candidate set*
in the BURG / instruction-selection tradition — defensible as the search
domain, but not source-anointed as universal at exactly five members.
The eight-step `derive_backend_shape` ordering is not canonical in any
cited optimizer; it is bbnf-local heuristic vocabulary. `CollapsedStage`
in its current form (`avx512bw` + `Entry(_)` eligibility at
`skinny/crates/passes/src/lib.rs:874-876`, marker-string lowerer at four
of five shapes per `P1-1B-D6`) is below the published asmjson / Sneller
branchless-AVX-512 admission bar; both cited sources are x86 AVX-512
architecture-pressure only and do not admit M5/aarch64 rows. The live
predicate is moreover under-constrained: `target.avx512bw` is the only
architecture indicator consulted, so a cross-build target inheriting
`avx512bw=true` on an aarch64 host admits `CollapsedStage` despite the
absence of any aarch64 lowerer. V2 binds the predicate to refuse
aarch64 admission and binds every `BackendExpr` node / rewrite guard /
extraction result to declare `substrate_target ∈ {local_temp_only,
existing_tape, direct_sink, admitted_fact_output}` per CH5
F-CH5-V1-03 — without the manifest, e-graph extraction may quietly
admit a plan whose mask streams retain into a parser-owned sidecar.
`CostFacts` as live (`rule_id`, `chosen`, `rationale`,
`priority_fired`) is metadata; published cost models (egg's `CostModel`,
BURG dynamic-programming extraction) require an objective vector,
frontier/dominance status, and an extraction method consumed by the
gate. Refutation is the load-bearing output: `P1-1B-D3`, `P1-1B-D4`,
`P1-1B-D6` are not defensible against the cited record.

**V2 fold packet items folded:** (CH1-V1-BLK-01) dead Sneller source-
repo URL replaced with Wayback-Machine archived snapshot pinned at SHA
`86e9f118cf6517220d8dc8e0af788e1a312fc056` (captured 2024-01-11) —
the upstream `github.com/SnellerInc/sneller` returns 404 at HEAD
(`curl -sI` 2026-05-23) because the repository was removed from the
SnellerInc GitHub organisation; the Wayback snapshot preserves the
authoritative HEAD-pinned source tree; `counted_source_ids` register
added to frontmatter per V3 convention. (CH2-V1-item-2) 2C V3 V2-FOLD
/ V3-FOLD canonical Lock 14 transfer contract cited as binding
authority; Per-Technique Transfer Coverage table added below covering
BURG / egg / CSP (decision-engine techniques) plus SinkOnly /
CollapsedStage / EagerTape / OffsetTape / EventTape (shape candidates)
across CSS L4 / Sheets / BBNF-self per the 2C V3 template at
`2C-grammar-neutrality.md:270-286` (Per-Technique Transfer Coverage
header at `:270`; table rows at `:276-286`). (CH4-F2) 4-of-5 marker-string
lowerers cross-referenced to 2B's coverage at `2B-primitive-vocabulary.md:73-74`;
admission gate strengthened in dossier prose; per-shape ledger surfaced
in §"BackendShape Admission Ledger" below. (CH5-F-CH5-V1-03)
`admits_collapsed_stage` predicate at `passes/src/lib.rs:874-876`
bound to refuse aarch64 admission via `target.arch == x86`
co-requirement; every `BackendExpr` node / rewrite guard / extraction
result declares `substrate_target` per Lock 1 v+1 substrate-union
manifest. (CH6-F8) per-row `same_wave_consumer` cell added to every
`T2D-*` grounded row in the Technique Grounding Table; rows without a
namable consumer downgrade to `grounded-class-only` per CH6 anti-paper-
close discipline. (CH6-F11) every Open Research Question row carries
an explicit wave or pass anchor (`discharged at T-P3 §3C amendment
authoring` / `deferred to S-P3 W{N}` / `abrogated to T-P3 backlog`).

## Technique Grounding Table

Per CH6 F8 (V1 fold packet item 24), every `T2D-*` grounded row carries
a `same_wave_consumer` cell naming the generated path or measured row
that would demonstrate the grounded class beats the cascade in bbnf.
Citation grounds the *class*; the consumer cell grounds the *bbnf
admission*. Rows without a namable consumer at V2 fold time downgrade
to `grounded-class-only` and are themselves CH6 anti-paper-close
discharge debts carried to T-P3 §3C.

| spec-claim or T-P1-divergence-id | published source cited | grounded / refuted / partial | bbnf-specific note | same_wave_consumer (per F8) |
|---|---|---|---|---|
| `T2D-EGRAPH-EXTRACTION`: equivalent backend plans can be represented together and the winner extracted by a cost function. | Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://arxiv.org/abs/2004.03082>. | grounded-class-only | Supports lowering `BackendShape` selection from destructive cascade to extraction over an e-graph of candidate plans. Live `backend_egraph::select` invocation at `skinny/crates/passes/src/lib.rs:477` is the correct shape-class, but lacks a `CostModel` with objective/frontier semantics. | **PENDING** at V2 fold — no production row currently selected by e-graph extraction beats a `PriorityStep`-derived plan on a measured row in `skinny/RESULTS.md`. F8 downgrade to `grounded-class-only`; consumer admission deferred to T-P3 §3C governance disposition (LAC-2D-01) + S-P3 W{N} measured-row gate. The egg-as-shape-class claim is real; the egg-as-bbnf-admission claim awaits the consumer. |
| `T2D-EQSAT-ORIGIN`: equality saturation is a recognised optimizer architecture, not a one-off. | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. | grounded-class-only | Establishes that destructive rewrites lose information; bbnf's `P1..P8` cascade at `skinny/crates/passes/src/lib.rs:446-505` is a destructive walk. Equality saturation is the published alternative. | **PENDING** at V2 fold — same as `T2D-EGRAPH-EXTRACTION`; the eqsat origin claim is shape-class lineage, not bbnf admission. Consumer admission deferred to T-P3 §3C governance + S-P3 W{N} measured-row gate. F8 downgrade. |
| `T2D-BURG-FINITE-ALTERNATIVES`: a finite set of code-generation alternatives selected by a cost criterion is the canonical instruction-selection shape. | Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems, 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. | grounded-class-only | Defends the *finite* nature of `BackendShape`. Refutes any claim that the *exact eight-step order* of `derive_backend_shape` is canonical — BURG uses dynamic-programming bottom-up cost minimisation, not a hand-ordered if/else chain. | **PENDING** at V2 fold — BURG grounds the finite-alternative class; the bbnf admission requires a measured row where bottom-up DP extraction beats the live cascade. The `SinkOnly` admission at `passes/src/lib.rs:868-872` is the only present-shape candidate; the other four are marker-string lowerers (see CH4-F2 cross-reference). Consumer admission deferred to T-P3 §3C + S-P3 W{N}. F8 downgrade. |
| `P1-1B-D3`: spec's eight-step priority order is canonical. | Same BURG source above + egg/eqsat sources. None of the cited literature endorses the exact bbnf eight-step name set (recovery → host decode → layout → first-set overlap → direct-only → collapsed-stage → event tape → offset tape). | refuted | The *finite-choice* idea transfers; the *fixed step order* does not. T-P3 should reword as "derive candidates, prove feasibility, extract minimum-cost plan." The `PriorityStep::ALL` table at `skinny/crates/ir/src/cost.rs:138-160` is diagnostic vocabulary, not the optimizer. | n/a (refuted) — no consumer claim required for a refuted row; the refutation row itself is the load-bearing T-P3 §3C input under Lock 10 v+1. |
| `P1-1B-D2`: `CostFacts` should be an active objective/frontier model. | `egg` defines `trait CostFunction` with cost vectors and Pareto/frontier extraction; BURG uses bottom-up DP over a cost lattice; OR-Tools CP-SAT defines multi-objective scalarisation, <https://developers.google.com/optimization/cp/cp_solver>. | partial | Live `CostFacts` at `skinny/crates/ir/src/cost.rs:4-17` carries `priority_fired` + `rationale` only; `ActiveCostFacts` at `:50-76` adds egraph telemetry but still names no objective/frontier/scalarisation fields. Schema must expand before a published cost-model claim is defensible. | n/a (partial — schema-bind row, not admission row); cost-schema population is the V+1 work, not a per-row consumer claim. |
| `T2D-CSP-FEASIBILITY-LAYER`: CSP belongs after candidate generation for feasibility filtering, not as the primary optimizer. | Google OR-Tools CP-SAT documentation, <https://developers.google.com/optimization/cp/cp_solver>. | grounded-class-only | Supports CSP for feature-gate / consumer-availability / parity-availability / budget-feasibility constraints. Local `decision_csp::finalize_rule` invocation at `skinny/crates/passes/src/lib.rs:478` is correctly *post*-egraph; structure is sound. Schema completeness is a separate gate. | **PENDING** at V2 fold — `decision_csp::finalize_rule` is invoked but no measured row currently turns on a CSP UNSAT verdict; the structural placement is sound but no production row uses CSP feasibility to flip an admission. Consumer admission deferred to T-P3 §3C + S-P3 W{N} (UNKNOWN-2D-04 verify_action). F8 downgrade. |
| `T2D-REGEX-NFA-DFA-PLAN`: NFA/DFA plan choice belongs in costed alternatives, not hardcoded. | Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>; Rust `regex-automata` hybrid DFA documentation, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>. | grounded | Defends an analyzer that chooses Pike/Thompson NFA, lazy DFA, or full DFA by state-count / cache pressure / grammar facts. Refutes any "always DFA" or "always NFA" admission in generic IR. The cost-model must consume these regex-engine facts. | **PENDING-via-2F** at V2 fold — admission rides on the `bbnf-regex` absorption decision (2F V5 LAC-2F-V5-01 Q1, anchored to SK-V14 W11). Once `bbnf-regex` ships NFA/lazy-DFA/full-DFA bodies, the cost-model consumes the state-count / cache-pressure facts. Consumer admission anchored to S-P3 W11 (per 2F Q1). |
| `T2D-TAPE-STAGED-MATERIALIZATION`: staged structural-discovery then later materialisation is a proven JSON parser class. | Langdale, Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>; simdjson source at HEAD `168ef580757d75270475b379e83c2b39787a6765`, <https://github.com/simdjson/simdjson>. | grounded | Grounds the `EagerTape` / `OffsetTape` / `EventTape` class generally. Does NOT reopen retained union-tape or streaming-cursor designs, which `skinny/REDRESS.md` 96/97/98 measured as regressive on M5 Max. | **PENDING-via-marker-strings** at V2 fold — the three tape shapes ship as marker-string lowerers at `skinny/crates/codegen/src/lower/{eager,offset,event}_tape.rs:15-17` per 2B Executive Summary at `2B-primitive-vocabulary.md:73-74`. The class is grounded by simdjson; the bbnf admission awaits either kernel emission per LAC-2D-04 or shape retirement per Lock 10 amendment (see "BackendShape Admission Ledger" §). |
| `T2D-SINKONLY-PROJECTION`: direct projection / sink-only lowering can beat general materialisation when consumer is known. | Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann, "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. | grounded | Supports `SinkOnly` as projection-aware shape. Admission requires strict same-plane comparator + independent oracle + generated consumer (not digest-only correctness). Live `admits_sink_only` at `skinny/crates/passes/src/lib.rs:868-872` is structurally correct (requires `target.direct_only_output` + no retained consumer + `DirectBuild` in BIR). | **ADMITTED** — `SinkOnly` is the only present-shape candidate of the five. The CSS L4 declaration-values row is the same-wave consumer (per `skinny/RESULTS.md` CSS telemetry; cross-referenced in 2C V3 ledger at `2C-grammar-neutrality.md:291` as the lone `ADMITTED-EVIDENCE` row). |
| `T2D-COLLAPSEDSTAGE-X86-ONLY`: branchless AVX-512 FSM is a published shape, but is x86-AVX-512-bound. | `asmjson` README/source (AVX-512 DOM kernel), <https://docs.rs/crate/asmjson/0.2.5/source/README.md>; Sneller branchless-AVX-512 blog post, <https://sneller.ai/blog/branchless-code-avx-512/>; Sneller SQL engine source archived at SHA `86e9f118cf6517220d8dc8e0af788e1a312fc056` (captured 2024-01-11), <https://web.archive.org/web/20240111085123/https://github.com/SnellerInc/sneller>. | partial / architecture pressure | Both citations are x86-only. M5 Max / aarch64 has no admissibility path from these sources alone. Current `admits_collapsed_stage` at `skinny/crates/passes/src/lib.rs:874-876` (checks only `avx512bw` + `Entry(_)`) is doubly insufficient: it (a) does not match the published bar (asmjson and Sneller carry concrete FSM tables, kernel emission, branchless dispatch) and (b) leaks aarch64 admission via cross-build target inheritance of `target.avx512bw` — V2 per F-CH5-V1-03 requires the predicate to co-require `target.arch == x86` and to forbid aarch64 admission until a generated aarch64 strategy lands (UNKNOWN-2D-05). Marker-string lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` is not admissible (`P1-1B-D6`); cross-referenced in 2B Executive Summary at `2B-primitive-vocabulary.md:73-74`. | **NOT ADMITTED** — no concrete kernel, no scalar oracle, no checkasm cell, no aarch64 strategy. Admission gated on LAC-2D-04 + LAC-2D-06 (F-CH5-V1-03 predicate refinement) + 2E aarch64 source-backed candidate (UNKNOWN-2D-05). |
| `P1-1B-D4`: `CollapsedStage` eligibility test is published-grade. | Same asmjson / Sneller sources. Both require concrete table-driven FSM emission, not a feature-flag check. | refuted | Eligibility must include the spec's ≥4 byte-disjoint arms hub condition (`restart/ARCHITECTURE.md:1096`) PLUS a kernel emitter PLUS scalar oracle + checkasm differential per Lock 16 PLUS aarch64-admission refusal per F-CH5-V1-03. Current check is below all four bars. | n/a (refuted) — refutation row; no consumer claim required. |
| `P1-1B-D6`: four of five shape lowerers emit marker strings. | Same simdjson / asmjson / Mison sources. No published parser ships marker-string lowerers. | refuted | `EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers at `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` emit `rule {name} -> <shape>` placeholders. The five-shape enum is a defensible search domain; four of its lowerers carry no real logic. Cross-referenced in 2B Executive Summary at `2B-primitive-vocabulary.md:73-74` ("(CH4-F2) 4-of-5 marker-string BackendShape lowerers documented as candidate refutation"). See "BackendShape Admission Ledger" § below for the per-shape disposition. | n/a (refuted) — refutation row; the per-shape consumer cells live in the admission ledger §. |
| `T2D-FIVE-SHAPE-FINITE-SET`: five backend shapes is the defensible candidate set. | BURG/egg sources above support finite alternatives + cost extraction; local `ir::BackendShape` at `skinny/crates/ir/src/lib.rs:339-345`; `all_backend_shapes()` at `skinny/crates/ir/src/cost.rs:208-216`. | partial | Finite is defensible. Exactly five is bbnf-local convention, not source-mandated. Amend Lock 10 to say "the five are the V1 candidate set unless T-P3 admits a generated/derived expansion with G-Omega gate." | n/a (set-membership claim, not admission claim); per-member admission lives in "BackendShape Admission Ledger" §. |

## BackendShape Admission Ledger

Per CH4-F2 (V1 fold packet item 12): each of the five `BackendShape`
variants admits or fails along the 2A T2A-LAC-V1-03 eight-cell manifest
schema (abstract primitive name + published citation + hardware gate +
scalar reference + checkasm cell + corpus parity + same-wave consumer +
row admission/measured rejection). The cross-reference to 2B's marker-
string-lowerer coverage at `2B-primitive-vocabulary.md:73-74` documents
the corresponding lowerer state at HEAD. Per V1 CH4 disposition, each
non-admitted shape resolves to one of two V2 dispositions: (a) implement
the lowerer in the SK-V14 horizon with concrete kernel + scalar oracle +
checkasm cell + same-wave consumer per LAC-2D-04, or (b) amend Lock 10
to retire the shape from the V1 candidate set.

| BackendShape | abstract primitive | published citation | hardware gate | scalar oracle (HEAD) | checkasm cell (HEAD) | corpus parity (HEAD) | same-wave consumer (HEAD) | row admission / disposition |
|---|---|---|---|---|---|---|---|---|
| `SinkOnly` | Mison projection-aware parser (consumer-known direct sink) | Mison VLDB 2017 (SRC-07) | none (algorithmic) | `crates/codegen/src/lower/sink_only.rs:1-100` (substantive logic per CH7 V1 §2.5) | n/a (algorithmic shape, not SIMD primitive) | CSS L4 declaration-values strict comparator vs lightningcss / cssparser | `runtime/src/grammars/css_l4_declaration_values/` (per 2C V3 ledger `:291`) | **ADMITTED** — the lone present-shape candidate. CSS L4 declaration-values is the ADMITTED-EVIDENCE row. |
| `EagerTape` | simdjson tape-builder (full materialisation) | simdjson VLDB 2019 (SRC-08) + simdjson source SHA `168ef580` (SRC-09) | none (algorithmic) | **ABSENT** — lowerer at `crates/codegen/src/lower/eager_tape.rs:15-17` emits `format!("rule {} -> eager_tape", rule.name)` marker string only | **ABSENT** | **ABSENT** | **ABSENT** — no production row currently selects `EagerTape` | **NOT ADMITTED** — V2 disposition per LAC-2D-04: implement concrete tape-builder lowerer + scalar oracle + checkasm cell + same-wave consumer, OR amend Lock 10 to retire from candidate set. Carries CH4-F2 fold debt. |
| `OffsetTape` | simdjson `OffsetTape` (offset-indexed staged tape) | simdjson VLDB 2019 (SRC-08) + simdjson source SHA `168ef580` (SRC-09) | none (algorithmic) | **ABSENT** — lowerer at `crates/codegen/src/lower/offset_tape.rs:15-17` emits marker string | **ABSENT** | **ABSENT** | **ABSENT** | **NOT ADMITTED** — same V2 disposition as `EagerTape`. CH4-F2 fold debt. |
| `EventTape` | simdjson `EventTape` (alt-branch-rich event stream) | simdjson VLDB 2019 (SRC-08) + simdjson source SHA `168ef580` (SRC-09); `prefers_event_tape` heuristic at `passes/src/lib.rs:878-880` (alt-branch ≥8) | none (algorithmic) | **ABSENT** — lowerer at `crates/codegen/src/lower/event_tape.rs:15-17` emits marker string | **ABSENT** | **ABSENT** | **ABSENT** | **NOT ADMITTED** — same V2 disposition as `EagerTape`. CH4-F2 fold debt. |
| `CollapsedStage` | asmjson branchless AVX-512 FSM kernel + Sneller branchless-AVX-512 dispatch | asmjson docs.rs README (SRC-10); Sneller blog (SRC-11); Sneller source archived SHA `86e9f118` (SRC-12, replaces dead live URL per CH1-V1-BLK-01) | x86 AVX-512BW (architecture-pressure ONLY — does NOT close M5 Max / aarch64 rows per Lock 16 v+1 at `LOCKS.md:346-349`) | **ABSENT** — lowerer at `crates/codegen/src/lower/collapsed_stage.rs:15-17` emits marker string | **ABSENT** | **ABSENT** | **ABSENT** — and the live predicate at `passes/src/lib.rs:874-876` admits any rule with `target.avx512bw && Entry(_)`, leaking aarch64 admission via cross-build target inheritance (CH5 F-CH5-V1-03) | **NOT ADMITTED + PREDICATE HARDENING REQUIRED** — V2 disposition: refine predicate per F-CH5-V1-03 (co-require `target.arch == x86`), implement concrete FSM kernel + scalar oracle + checkasm cell + same-wave consumer per LAC-2D-04, OR amend Lock 10 to retire from candidate set. Aarch64 admission gated on 2E source-backed candidate (UNKNOWN-2D-05). CH4-F2 + CH5 F-CH5-V1-03 fold debt. |

**Ledger summary at V2:** 1 admitted (`SinkOnly`); 4 not-admitted with
marker-string lowerers (`EagerTape`, `OffsetTape`, `EventTape`,
`CollapsedStage`). The 1-of-5 admission rate is the load-bearing CH4
operational finding for V2; V2 dispositions are binary per LAC-2D-04
(implement kernel + admission cells OR retire shape from candidate
set). The Lock 10 amendment route (LAC-2D-04) holds the candidate-set
position open while preserving the published BURG-finite-alternative
discipline.

## Per-Technique Transfer Coverage

Per CH2 V1 item 2 (V1 fold packet item 6): cite 2C V3 V2-FOLD / V3-FOLD
canonical Lock 14 transfer contract (the "Closure Criteria For Live
Grammar Leaks" table at `2C-grammar-neutrality.md:303-309` plus the
"Per-Technique Transfer Coverage" template at `2C-grammar-neutrality.md:270-286`).
The
table below applies the 2C V3 template to 2D's grounded techniques —
the three decision-engine techniques (egg, BURG, CSP) plus the five
`BackendShape` candidates (SinkOnly, CollapsedStage, EagerTape,
OffsetTape, EventTape). Each row is a *transfer requirement*, not a
present admission; rows without a same-wave consumer in the target
grammar are `NOT-VALIDATED` for fleet-wide grammar-neutral claims per
2C V3's Lock 14 v+1 strict read at `2C-grammar-neutrality.md:121`
("a primitive claimed grammar-neutral must exercise at least one non-
JSON consumer or record a measured deletion/rejection" — sourced from
Lock 14 v+1 at `restart/locks/LOCKS.md:259-260`).

| technique | CSS L4 transfer | Sheets transfer | BBNF-self transfer | required generated facts | failure mode if absent |
|---|---|---|---|---|---|
| egg equality saturation (`T2D-EGRAPH-EXTRACTION`) | applies to selector/value/declaration shape selection; saturate over alternative `BackendExpr` plans for CSS L4 declaration-values + selectors | applies to formula/reference shape selection; saturate over `BackendExpr` plans for Sheets numeric / string / reference grammars | applies to rule/expression/directive shape selection; saturate over `BackendExpr` plans for BBNF-self grammar shapes | grammar-neutral `BackendExprLanguage` node vocabulary (per UNKNOWN-2D-01 verify_action); `substrate_target` declaration per node (per F-CH5-V1-03); cost-axis schema per UNKNOWN-2D-02 | leak grammar-name into node constructors (Lock 14 v+1 fault per 2C `:302`); graph blow-up without bounded scheduler (UNKNOWN-2D-03) |
| BURG finite-alternative DP extraction (`T2D-BURG-FINITE-ALTERNATIVES`) | DP cost minimisation over five `BackendShape` candidates per CSS L4 rule | DP cost minimisation over five `BackendShape` candidates per Sheets rule | DP cost minimisation over five `BackendShape` candidates per BBNF-self rule | grammar-neutral cost-axis facts (throughput, cycles/byte, IPC, generated LOC, materialisation bytes) per candidate per grammar; identical five-shape candidate set (Lock 10) | hand-ordered cascade per grammar (Lock 10 fault); per-grammar shape carve-outs (Lock 14 fault) |
| OR-Tools CP-SAT feasibility filter (`T2D-CSP-FEASIBILITY-LAYER`) | feasibility constraints over CSS L4 candidate plans: feature gate (avx512bw / arch arm64), same-wave CSS consumer availability, parity-oracle (lightningcss / cssparser), generated-LOC budget | feasibility constraints over Sheets candidate plans: feature gate, same-wave Sheets consumer availability, parity-oracle (ODF 1.3 Part 4 golden table), generated-LOC budget | feasibility constraints over BBNF-self candidate plans: feature gate, same-wave BBNF-self consumer availability, parity-oracle (current grammar parser + golden grammar fixtures), generated-LOC budget | per-grammar feasibility predicates emitted from grammar facts; multi-objective scalarisation profile (SRC-04 OR-Tools CP-SAT) | feasibility checks hardcoded per grammar (Lock 14 fault); CSP timeout unbounded (LAC-2D-06 fault) |
| `SinkOnly` (Mison projection) | **ADMITTED at HEAD** (CSS L4 declaration-values, lone present-shape candidate per ledger above) | NOT-VALIDATED — Sheets has no `JsonSink`-equivalent generated sink at HEAD; admission requires `runtime/src/grammars/google_sheets/sink.rs` per 2C `:119` (`JsonSink` refuted as generic sink contract) | NOT-VALIDATED — BBNF-self has no generated sink at HEAD; admission requires `runtime/src/grammars/bbnf_self/sink.rs` per 2C `:119` | per-grammar generated sink/fact trait (not `JsonSink`); strict same-plane comparator + independent oracle + generated consumer (`admits_sink_only` at `passes/src/lib.rs:868-872`) | reuse JsonSink callback names across grammars (Lock 14 fault per 2C `:119` / `:328`) |
| `EagerTape` / `OffsetTape` / `EventTape` (simdjson staged tape class) | NOT-VALIDATED — marker-string lowerers; no per-grammar tape-builder emission for CSS L4 | NOT-VALIDATED — marker-string lowerers; no per-grammar tape-builder emission for Sheets | NOT-VALIDATED — marker-string lowerers; no per-grammar tape-builder emission for BBNF-self | per-grammar generated tape-builder + materialisation-descriptor labels sourced from grammar rule names (not JSON canonical `"object"`/`"array"`/`"pair"` per 2C `:311` / `:328`) | leak `MaterializationDescriptor.label = "object"` / `"array"` / `"pair"` strings into non-JSON grammars (Lock 14 fault per 2C `passes/src/lib.rs:1059`/`:1079`/`:1102`) |
| `CollapsedStage` (asmjson/Sneller branchless AVX-512 FSM) | NOT-VALIDATED + ARCHITECTURE-PRESSURE-ONLY — x86 AVX-512BW only per Lock 16 v+1; M5 Max has no admissibility path from these sources (per 2E refutation); per-grammar transfer requires aarch64 source-backed candidate from 2E (UNKNOWN-2D-05) | NOT-VALIDATED + ARCHITECTURE-PRESSURE-ONLY — same x86-only constraint | NOT-VALIDATED + ARCHITECTURE-PRESSURE-ONLY — same x86-only constraint | per-grammar FSM transition table emission (per `bbnf.asm:55-60` codegen-emitted `.data` convention); aarch64 strategy source-backing (UNKNOWN-2D-05); F-CH5-V1-03 predicate refinement | admit on aarch64 hosts via cross-build `target.avx512bw` inheritance (live HEAD defect at `passes/src/lib.rs:874-876`) |

The transfer table makes 2C V3's Lock 14 v+1 strict read concrete for
2D's grounded techniques: the three decision-engine techniques (egg,
BURG, CSP) are grammar-neutral by construction (operate on
`BackendExpr` plans + cost facts); their failure modes are not transfer
gaps but grammar-name leaks in node vocabulary, cost-axis schema, or
feasibility predicates. The four non-`SinkOnly` shapes carry transfer
debt by definition — their marker-string lowerers do not emit per-
grammar bodies at HEAD; transfer requires either kernel implementation
per LAC-2D-04 or shape retirement per Lock 10 amendment.

## Architectural Assertions Defended

| assertion | defence | adoption rule |
|---|---|---|
| Backend-shape selection should be a search/extraction problem, not a fixed cascade. | Equality saturation (Tate POPL 2009, Willsey POPL 2021) keeps alternative plans simultaneously so a cost model extracts the winner. Destructive ordering loses information; bbnf's `P1..P8` cascade is destructive. | Introduce `BackendExprLanguage` over BIR/shape candidates, bounded by node/iteration caps and per-rule guards. Live invocation at `skinny/crates/passes/src/lib.rs:477` is correctly shaped; schema completion remains. |
| The five-shape enum is a defensible finite candidate set. | BURG (Fraser/Hanson/Proebsting LOPLAS 1992) grounds finite-alternative selection by cost. Five named shapes (`EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`) form a closed enumeration suitable for bottom-up DP / extraction. | Treat the five as the allowed search domain. Do not add `BackendShape` variants without G-Omega; deriving expansion requires Lock 10 amendment. |
| Cost must become an executable contract, not after-the-fact metadata. | egg's `CostFunction` and BURG's dynamic-programming cost minimisation both require an objective. Live `CostFacts` at `skinny/crates/ir/src/cost.rs:4-17` stores only the *outcome* of selection. | `CostFacts` must include objective axes, evidence freshness, frontier/dominance status, extraction method, and a gate-consumed report before SK-V13 W9 can claim decision-engine fold. |
| CSP belongs after candidate generation, not as a paper optimizer. | OR-Tools CP-SAT is appropriate for feasibility / objective constraints — feature gates, same-wave consumer availability, parity-oracle availability, budget feasibility, generated-size budget. | Use CSP for feasibility filtering, not for shape choice. Live `decision_csp::finalize_rule` invocation at `skinny/crates/passes/src/lib.rs:478` is correctly *post*-egraph. |
| `SinkOnly` is real when tied to a known consumer. | Mison (VLDB 2017) shows projection/filter-aware parsing avoids general materialisation when consumer schema is known. | `SinkOnly` admits only with strict same-plane comparator, independent oracle, and generated consumer (`admits_sink_only` at `skinny/crates/passes/src/lib.rs:868-872` checks the structural pre-conditions). |
| Staged tape materialisation is a published JSON parser class. | simdjson (Langdale/Lemire VLDB 2019) grounds the stage1 (structural discovery) + stage2 (materialisation) shape. | `EagerTape`/`OffsetTape`/`EventTape` are admissible classes; concrete lowerers must exist (current marker strings are not admissions). |

## Architectural Assertions Refuted

| refuted assertion | why it is refuted | consequence |
|---|---|---|
| The hardcoded `P1..P8` cascade is a literature-grounded optimizer. | The sources ground equality saturation, dynamic-programming extraction, and constraint solving — none ground a bbnf-specific if/else chain. Live cascade at `skinny/crates/passes/src/lib.rs:446-505` is heuristic. | Amend Lock 10: the cascade is legacy compatibility until replaced; `PriorityStep::ALL` is diagnostic vocabulary only. |
| The exact eight-step order in `derive_backend_shape` is canonical. | BURG endorses costed finite alternatives, not a fixed bbnf order. simdjson / Mison / sonic-rs use workload-specific materialisation, not this universal step list. | Rewrite as a resolver pipeline: candidate generation → rewrite saturation → constraint filter → cost extraction → diagnostics. |
| Current `CostFacts` implements the spec cost model. | `CostFacts` stores selected/rejected facts + measurements (`skinny/crates/ir/src/cost.rs:4-17`) but no active objective, Pareto/frontier, scalarisation, or extraction method. `ActiveCostFacts` (`:50-76`) adds egraph telemetry but no published cost-model surface. | Mark as partial; require schema expansion + gate-consumed report before SK-V13 W9 can claim decision-engine fold. |
| Existing `CollapsedStage` eligibility is admissible. | `admits_collapsed_stage` at `skinny/crates/passes/src/lib.rs:874-876` checks `target.avx512bw` + `Entry(_)` only; the ≥4-byte-disjoint-arms hub condition (`restart/ARCHITECTURE.md:1096`) is missing. The published AVX-512 sources (asmjson, Sneller) require concrete FSM/table emission. The lowerer at `skinny/crates/codegen/src/lower/collapsed_stage.rs:15-17` emits a marker string. The current predicate is moreover *aarch64-leaky*: a cross-build target inheriting `target.avx512bw = true` on an aarch64 host admits `CollapsedStage` despite the absence of any aarch64 lowerer (per CH5 F-CH5-V1-03). | Keep `CollapsedStage` disabled unless (a) the predicate is bound per LAC-2D-06 / F-CH5-V1-03 to co-require `target.arch == x86`, (b) a generated aarch64 strategy (per 2E, UNKNOWN-2D-05) plus same-wave consumer plus checkasm differential lands, and (c) every `BackendExpr` node / rewrite guard / extraction result declares `substrate_target` per Lock 1 v+1 substrate-union manifest. |
| Four of five lowerers carrying real logic is acceptable. | `P1-1B-D6` records `EagerTape`, `OffsetTape`, `EventTape`, `CollapsedStage` lowerers as marker strings at `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` (note: the actual files are named `eager_tape.rs`, `offset_tape.rs`, `event_tape.rs`, `collapsed_stage.rs` — the lone substantive lowerer is `sink_only.rs`). Cross-referenced in 2B Executive Summary at `2B-primitive-vocabulary.md:73-74` per (CH4-F2) "(CH4-F2) 4-of-5 marker-string BackendShape lowerers documented as candidate refutation". simdjson / Mison / asmjson all ship full kernels for every claimed shape; no published parser ships marker-string lowerers. | Either implement the four lowerers per LAC-2D-04 (concrete kernel emission + scalar oracle + checkasm cell + same-wave consumer) or amend Lock 10 to retire shapes whose lowerers cannot land. The current state — five-shape enum, one-shape implementation — is paper-architecture. The "BackendShape Admission Ledger" § above enumerates the per-shape V2 disposition. |

## Open Research Questions

Per CH6 F11 (V1 fold packet item 27), every Open Research Question row
carries an explicit wave or pass anchor — `discharged at T-P3 §3C
amendment authoring`, `deferred to S-P3 W{N}`, or `abrogated to T-P3
backlog with named owner` — so the OQ ledger never inherits the "later
research pass" deferral the §3 CH6 prohibition forbids.

| UNKNOWN | verify_action | wave / pass anchor |
|---|---|---|
| UNKNOWN-2D-01: Does `BackendExpr` have a stable grammar-neutral node vocabulary suitable for `egraph::Language`, without embedding JSON/CSS names? | Build a V1 `BackendExprLanguage` prototype from BIR nodes only; run a Lock 14 leak scan over node constructors and rewrite guards. | **discharged at T-P3 §3C amendment authoring** (LAC-2D-01 disposition under Lock 14 v+1 + Lock 1 v+1 substrate-union manifest; Lock 14 v+1 leak-scan command at `2C-grammar-neutrality.md:354` is the verification harness) |
| UNKNOWN-2D-02: What measured cost axes are sufficient to prevent stale or overfit extraction? | Add a cost snapshot over JSON + CSS rows with throughput, cycles/byte, IPC, generated LOC, materialisation bytes, feature gate, evidence age; reject if >30% of candidate exprs use stale/static fallback. | **deferred to S-P3 W{N=cost-axis-snapshot}** (S-P3 P3-C wave that lands the cost-axis schema per LAC-2D-02; T-P3 §3C disposes the schema as Lock 10 v+1 amendment first) |
| UNKNOWN-2D-03: Can bounded equality saturation avoid graph blow-up on CSS L4 + BBNF-self grammars? | Run `BackoffScheduler` / `CspScheduler` with node + iteration caps over JSON, CSS declaration-values, synthetic selector grammar; publish node counts + saturation reason. | **deferred to S-P3 W{N=cost-axis-snapshot}** (same wave as UNKNOWN-2D-02 — bounded-scheduler harness is part of the cost-axis snapshot deliverable; T2A-LAC-V1-05 abrogate caps adopted per LAC-2D-06 supply the numeric node + iteration caps) |
| UNKNOWN-2D-04: Does CSP improve plan quality beyond extraction alone? | Compare egraph-only extraction with egraph+CSP on at least three grammars; record UNSAT causes and any selected-plan deltas. | **deferred to S-P3 W{N=cost-axis-snapshot}** (same wave; the CSP-vs-egraph-only differential is a row in the same cost-axis snapshot harness — the egraph+CSP path is already live at `passes/src/lib.rs:478` and produces telemetry consumed by the harness) |
| UNKNOWN-2D-05: Can `CollapsedStage` be restated for aarch64 without x86 leakage? | Require an aarch64-specific source-backed candidate from 2E, then micro-prove + wire to a CSS or JSON row. Until then, treat x86 `CollapsedStage` as totality background, not SK-V13 close-route. | **deferred to S-P3 W{N=2E-source-backed-aarch64-candidate}** (gated on 2E V6 LOCKED candidates C-P2C-2/3/4/5/8 — UNKNOWN-2D-05 cannot discharge until 2E surfaces an aarch64 source-backed candidate; concurrent T-P3 §3C disposition of LAC-2D-06 + F-CH5-V1-03 hardens the live predicate so x86 admission is correct and aarch64 admission is mechanically refused) |
| UNKNOWN-2D-06: Do lowerers actually emit artefacts for all five shapes? | Add golden generated-source tests for `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`. Marker-string lowerers fail this gate. | **deferred to S-P3 W{N=BackendShape-admission-ledger-wave}** (the wave that lands the per-shape disposition from the "BackendShape Admission Ledger" §: implement kernels per LAC-2D-04 OR retire shapes per Lock 10 amendment; the golden-source-test harness is the discharge instrument that distinguishes the two outcomes) |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2D-01 | Lock 10 / cost-model decision engine | Replace "derive backend shape by P1..P8 cascade" with "generate backend-plan candidates, saturate equivalent plans, filter infeasible plans, extract by an active `CostModel`; legacy priority steps are diagnostics only." | Cascade at `skinny/crates/passes/src/lib.rs:446-505`; egg/eqsat sources (SRC-01, SRC-02); BURG source (SRC-03); T-P1 `P1-1B-D2`/`P1-1B-D3`. |
| LAC-2D-02 | Lock 10 / `CostFacts` schema | Require objective vectors, frontier/dominance status, scalarisation profile, extraction method, evidence freshness, stale/static fallback marker, and source reference for every selected and rejected shape. | Current fields at `skinny/crates/ir/src/cost.rs:4-17`; `ActiveCostFacts` at `:50-76`; egg's `CostFunction`; BURG DP cost lattice. |
| LAC-2D-03 | Lock 14 / grammar neutrality | Backend-shape rewrites, CSP constraints, and cost guards must consume *generated* grammar metadata, not grammar-name branches or JSON-role mining. CSS plus Sheets or BBNF-self proof fixtures required before fleet-wide admission. | T-P1 1B/1D findings; `derive_recognizers` JSON whitelist at `skinny/crates/passes/src/lib.rs:331` (D8); `derive_materialization_roles` JSON-role mining at `:1300-1391` (D10); Lock 14 at `restart/locks/LOCKS.md:220`. |
| LAC-2D-04 | Lock 16 / `CollapsedStage` admissibility | `CollapsedStage` admits only as a concrete emitted transient strategy with scalar reference, checkasm/parity, feature gate, local temporary lifetime, and same-wave measured consumer. AVX-512 literature is x86-only and cannot close M5/aarch64 rows. Extend the same admission condition to `EagerTape`, `OffsetTape`, `EventTape` per CH4-F2 "BackendShape Admission Ledger" § disposition (V2 fold packet item 12). | asmjson README (SRC-10); Sneller blog (SRC-11); Sneller source archived at SHA `86e9f118` (SRC-12, replaces dead live URL per CH1-V1-BLK-01); local marker lowerers at `skinny/crates/codegen/src/lower/{eager_tape,offset_tape,event_tape,collapsed_stage}.rs:15-17` cross-referenced in 2B Executive Summary at `2B-primitive-vocabulary.md:73-74`; SK-V13 user pin x86-out-of-scope for skinny. |
| LAC-2D-05 | Lock 1 / union substrate history | Add a material-differential clause for union-shape search: e-graph-selected or grammar-configured union variants may be attempted, but `skinny/REDRESS.md` 96/97/98 block replay of full class-column vector and streaming-cursor shapes. | REDRESS 96/97/98; equality-saturation route can express per-rule alternatives without a new public substrate. |
| LAC-2D-06 | Lock 1 v+1 / substrate-union manifest + Lock 10 v+1 / `admits_collapsed_stage` predicate hardening | Refine the live `admits_collapsed_stage` predicate at `skinny/crates/passes/src/lib.rs:874-876` to co-require `target.arch == x86` alongside `target.avx512bw` and `Entry(_)`, refusing aarch64 admission via cross-build `target.avx512bw` inheritance. Bind every `BackendExpr` node / rewrite guard / extraction result to declare `substrate_target ∈ {local_temp_only, existing_tape, direct_sink, admitted_fact_output}` per Lock 1 v+1 substrate-union manifest; e-graph extraction MUST reject plans whose `substrate_target` is not one of the four admitted values. Adopt T2A-LAC-V1-05 abrogate caps (e-graph saturation ≤50000 nodes / ≤10000 classes / ≤30 iter; CSP timeout ≤1 s/grammar; stale-cost ≤30%; generated-LOC growth; row regression; parity/checkasm failure) into 2D's cost-axis schema. | CH5 F-CH5-V1-03 (V1 fold packet item 19); live predicate at `passes/src/lib.rs:874-876`; CH5 V1 §3.3 "substrate-union invariant HOLDS cohort-wide" + 2D the single REVISE; 2A T2A-LAC-V1-05 abrogate caps; Lock 16 v+1 close-state vocabulary at `LOCKS.md:346-349` (AVX-512 literature is x86 architecture pressure and cannot close M5/aarch64 rows). |

## Source Register

| id | primary source |
|---|---|
| SRC-01 | Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible Equality Saturation", POPL 2021, <https://arxiv.org/abs/2004.03082>. |
| SRC-02 | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: A New Approach to Optimization", POPL 2009, <https://dl.acm.org/doi/10.1145/1480881.1480915>. |
| SRC-03 | Fraser, Hanson, Proebsting, "Engineering a Simple, Efficient Code Generator Generator", ACM Letters on Programming Languages and Systems, 1992, <https://dl.acm.org/doi/10.1145/176454.176487>. |
| SRC-04 | Google OR-Tools CP-SAT documentation, <https://developers.google.com/optimization/cp/cp_solver>. |
| SRC-05 | Rust `regex-automata` hybrid DFA documentation, <https://docs.rs/regex-automata/latest/regex_automata/hybrid/dfa/struct.DFA.html>. |
| SRC-06 | Russ Cox, "Regular Expression Matching Can Be Simple And Fast", <https://swtch.com/~rsc/regexp/regexp1.html>. |
| SRC-07 | Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann, "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017, <https://www.vldb.org/pvldb/vol10/p1118-li.pdf>. |
| SRC-08 | Langdale, Lemire, "Parsing Gigabytes of JSON per Second", VLDB 2019, <https://arxiv.org/abs/1902.08318>. |
| SRC-09 | simdjson source and implementation docs, <https://github.com/simdjson/simdjson>. |
| SRC-10 | asmjson crate source README, <https://docs.rs/crate/asmjson/0.2.5/source/README.md>. |
| SRC-11 | Sneller branchless-code blog post, architecture pressure only, <https://sneller.ai/blog/branchless-code-avx-512/>. |
| SRC-12 | Sneller SQL engine source repository, archived at SHA `86e9f118cf6517220d8dc8e0af788e1a312fc056` (captured 2024-01-11; the upstream `https://github.com/SnellerInc/sneller` returned `HTTP/2 404` at 2026-05-23 — the repository was removed from the SnellerInc GitHub organisation, which still hosts 12 other repos but not the main `sneller` engine; the SnellerInc blog at `sneller.ai/blog/branchless-code-avx-512/` continues to link the dead URL; the Wayback snapshot is the authoritative source-tree citation), <https://web.archive.org/web/20240111085123/https://github.com/SnellerInc/sneller>. |
