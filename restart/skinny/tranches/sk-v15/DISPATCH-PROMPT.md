# SK-V15 DISPATCH-PROMPT - Per-Wave Triumvirate Dispatch Contract

Date: 2026-05-28.

Status: S-P3 V2 dispatch contract for SK-V15 W0-W11. Every SK-V15 wave
is dispatched as a research -> plan -> redress triumvirate per
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

## Section 0 - Authority

Read in this order before dispatching any SK-V15 wave:

1. `restart/skinny/tranches/sk-v15/SPEC.md`
2. `restart/skinny/tranches/sk-v15/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v15/HANDOFF.md`
4. `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`
5. `restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md`
6. `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md`
7. `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md`
8. `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`
9. `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md`
10. `restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md`
11. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
12. `restart/prompts/ORCHESTRATOR.md`
13. `restart/locks/LOCKS.md`
14. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/skinny/ROLLING-SOTA-DELTA.md`

## Section 1 - Triumvirate Contract

Every wave uses separate commits for separate roles.

| Phase | Purpose | Source edits | Cap | Commit prefix |
|---|---|---|---:|---|
| Research | read-only diagnosis and owner-path evidence | no | <=20 min | `docs(sk-v15-wave{W}-research):` |
| Plan | one intervention, owner paths, gates, revert route | no | <=15 min | `docs(sk-v15-wave{W}-plan):` |
| Redress | implementation or ledger repair plus measurement | yes | <=30 min | `feat(sk-v15-wave{W}):` or `docs(sk-v15-wave{W}-redress):` |

The orchestrator refuses redress without a committed plan and refuses a
plan without committed research. Same-commit role merger is REJECT.

## Section 2 - Pre-Dispatch Verification

Before a wave dispatch:

1. Verify prior required waves are admitted, rejected, routed, or
   intrinsically blocked per `SPEC.md`.
2. Verify W1-W11 satisfy the dispatch lock in `SPEC.md`.
3. Verify the current wave's owner paths, tasks, exit gate, and revert
   protocol exist in `SPEC.md`.
4. Inspect dirty and staged state; preserve unrelated work.
5. Verify any delete/retire action has a dependency-table row with provider
   proof no later than the delete wave.
6. Verify the wave does not reopen an S-P2 REJECT or REDRESS pre-block
   under old framing.
7. Verify Apple M5 Max / aarch64 is the only admission target.

## Section 3 - Challenge Triggers

Dispatch a seven-lens CHALLENGE before redress when the wave is
first-of-class, substrate-touching, primitive/SIMD/ASM-bearing,
generator/provenance-bearing, or changes gate close semantics. W2, W3,
W4, W5, W6, W7, W8, W9, and W10 are mandatory CHALLENGE candidates
unless the plan proves the redress is ledger-only and non-behavioral.

The seven lenses are CH1 correctness, CH2 generality, CH3 regression,
CH4 cost, CH5 hidden coupling, CH6 anti-paper-close, and CH7
overfit-prune/gate-exclusion.

## Section 4 - Per-Wave Envelopes

### W0 - Baseline And Telemetry Lock

Research scopes: RESULTS schema, SK-V8/SK-V15 telemetry carrier,
gate-json parser, CSS broadcast rows, JSON 51 guard rows.

Plan must name the telemetry carrier, exact gate rejection predicates,
and no-behavior proof.

Redress closes only when gate consumption of all SK-V15 telemetry fields
is proven and CSS broadcast evidence is diagnostic.

### W1 - CSS Admission Honesty

Research scopes: CSS 24 rows, CSS gate/report surfaces, comparator
workload plane, provider-retirement dependencies.

Plan must choose demotion/collapse or independent retiming. Provider
deletion is forbidden unless W5/W6-grade typed proof lands in the same
wave.

Redress closes only when no CSS live admit can be produced from the W8R
24-row broadcast.

### W2 - Lock 14 / Lock 16 Gate Restoration

Research scopes: omitted Lock 14 roots, gate exclusions, checkasm
strictness, source-present unwired primitives.

Plan must name scan roots, exclusion report schema, fail-closed tests, and
primitive status classification.

Redress closes only when gates consume their own exclusion reports.

### W3 - Codegen Leak Abrogation

Research scopes: grammar profiles/providers, runtime generator, generic
passes, xtask regen commands.

Plan must remove one coherent leak family and name the same-wave
generator/check consumer.

Redress closes only with leak grep, generated-output proof, and JSON guard
rerun if JSON-adjacent generation changes.

### W4 - Pattern H Generated Discipline

Research scopes: 67 root runtime files, generator provenance, non-writing
regen/check route.

Plan must distinguish true generated provenance from header-only edits.

Redress closes only with the 67-file count, line-1 provenance scan, and
regen/check proof or intrinsic-block route.

### W5 - CSS Typed Value Provider

Research scopes: CSS grammar sources, codegen/runtime CSS provider, CSS
value/document/view/visitor shape, typed provider tests.

Plan must build typed output before retiring old parser proof and must
state that W8R metrics are diagnostic negative fixtures only.

Redress closes only with typed CSS provider output, gate-consumed tests,
JSON guard maintain if behavior changes, and no reuse of broadcast
measurements as floors.

### W6 - CSS Same-Workload Retime And Old-Proof Retirement

Research scopes: cssparser typed-value/document comparator path, CSS
bench/report/gate, old CSS proof contracts, RESULTS and rolling delta.

Plan must name the fresh typed cssparser comparator command and the exact
old proof paths to retire.

Redress closes only with fresh typed CSS measurements against `cssparser`,
same comparator workload, strict typed equality, no W8R floors, and JSON
51/51 maintain if behavior changed.

### W7 - Decision Engine Spine

Research scopes: e-graph, CSP, CostFacts, generated selection evidence.

Plan must name one real rewrite, one non-tautological CSP condition, and
the generated selection/report consumer.

Redress closes only when generated behavior or plan selection can change
and generic decision facts are grammar-neutral.

Required consumers or proven successors:

- `cargo test -p passes decision_egraph_rewrite_changes_selected_shape -- --exact`
- `cargo test -p passes decision_csp_rejects_missing_required_fact -- --exact`
- `cargo test -p codegen decision_spine_changes_generated_selection_fixture -- --exact`

### W8 - EagerTape / OffsetTape Lowerers

Research scopes: lowerer fixture harness, EagerTape, OffsetTape, generated
runtime diff tests.

Plan must replace label-string scaffold with real output or a
gate-consumed rejected alternative.

Redress closes only when fixtures fail against the old scaffold and
EagerTape/OffsetTape no longer remain placeholder-only.

Required consumers or proven successors:

- `cargo test -p codegen backend_lowerer_fixture_rejects_label_string_scaffold -- --exact`
- `cargo test -p codegen lower_eager_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_offset_tape_emits_runtime_relevant_diff -- --exact`

### W9 - EventTape / SinkOnly / CollapsedStage Lowerers

Research scopes: EventTape, SinkOnly, CollapsedStage, all-five
BackendShape gate, generated runtime diff tests.

Plan must preserve the five-shape canon and explicitly reject EventTape
as sidecar vector, sixth shape, public substrate API, retained stream, or
alternate document projection.

Redress closes only when tests fail against the old scaffold, remaining
lowerers are real or gate-rejected, and the all-five gate covers exactly
the canonical five BackendShape variants.

Required consumers or proven successors:

- `cargo test -p codegen lower_event_tape_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_sink_only_emits_runtime_relevant_diff -- --exact`
- `cargo test -p codegen lower_collapsed_stage_emits_runtime_relevant_diff -- --exact`
- `cargo xtask gate-json --check-results --skv15-backend-lowerers-report <path>`

### W10 - FNV Quarantine

Research scopes: bench/xtask FNV helpers, strict-product comparator,
production FNV scan, adversarial semantic fixtures.

Plan must quarantine FNV and add adversarial semantic fixtures. It must
also specify that no FNV-keyed arbiter can migrate into production runtime
or generic codegen.

Redress closes only when FNV cannot be runtime selector, arbiter, or
correctness proof.

Required consumers: strict-product gate over quarantine metadata,
production `rg -n "fnv|FNV"` scan, and adversarial semantic fixtures.

### W11 - Close Reconciliation

Research scopes: RESULTS, REDRESS, rolling delta, HANDOFF, dependency
table, PASS-IMPL V2.

Plan must reconcile all admitted, diagnostic, retired, deleted, and
blocked states.

Redress closes only when PASS-IMPL V2 consumes the close packet and every
dependency row is admitted, reverted with REDRESS, or intrinsically
blocked by row-level proof. Any implementation fix, measurement rerun, or
unresolved dependency row discovered during W11 aborts close and spawns
the owning repair wave; it is not deferred to SK-V16.

## Section 5 - Same-Wave Consumer Mandate

Any primitive, kernel, new generated path, new parser helper, new
telemetry field, or gate report must be consumed by the hot path or gate
in the same wave. Source-present but unwired is REJECT unless the wave
explicitly deletes, scalar-delegates, or records intrinsic block.

The shared pre-block list every wave must carry is:

`28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration`

## Section 6 - Commit And Evidence Discipline

Use the local commit-discipline skill for every commit. Stage only the
wave slice. Broad gate/status/generated/deletion commits need a body that
states why, what landed, evidence, and routed remainder.

Wave evidence must include exact commands or artifacts. Documentation-only
close, future-phase promises, warm benches, x86 admission anchors, W8R
metric floors, and hidden broadcast measurements are REJECT.

## Section 7 - Stop Conditions

Stop only at G-Omega, unrepaired invariant violation, or SK-V17 close
under the active user extension. Otherwise continue the SK loop.
