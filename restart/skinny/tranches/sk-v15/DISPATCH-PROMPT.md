# SK-V15 DISPATCH-PROMPT - Per-Wave Triumvirate Dispatch Contract

Date: 2026-05-28.

Status: S-P3 V1 dispatch contract for SK-V15 W0-W9. Every SK-V15 wave
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
2. Verify the current wave's owner paths, tasks, exit gate, and revert
   protocol exist in `SPEC.md`.
3. Inspect dirty and staged state; preserve unrelated work.
4. Verify any delete/retire action has a dependency-table row with
   provider proof no later than the delete wave.
5. Verify the wave does not reopen an S-P2 REJECT or REDRESS pre-block
   under old framing.
6. Verify Apple M5 Max / aarch64 is the only admission target.

## Section 3 - Challenge Triggers

Dispatch a seven-lens CHALLENGE before redress when the wave is
first-of-class, substrate-touching, primitive/SIMD/ASM-bearing,
generator/provenance-bearing, or changes gate close semantics. W2, W3,
W4, W5, W6, W7, and W8 are mandatory CHALLENGE candidates unless the
plan proves the redress is ledger-only and non-behavioral.

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

### W1 - PRUNE-A CSS Admission Honesty

Research scopes: CSS 24 rows, CSS gate/report surfaces, comparator
workload plane, provider-retirement dependencies.

Plan must choose demotion/collapse or independent retiming. Provider
deletion is forbidden unless W5-grade typed proof lands in the same wave.

Redress closes only when no CSS live admit can be produced from the W8R
24-row broadcast.

### W2 - PRUNE-B Lock 14 / Lock 16 Gate Restoration

Research scopes: omitted Lock 14 roots, gate exclusions, checkasm
strictness, source-present unwired primitives.

Plan must name scan roots, exclusion report schema, and fail-closed
tests.

Redress closes only when gates consume their own exclusion reports.

### W3 - PRUNE-C Codegen Leak Abrogation

Research scopes: grammar profiles/providers, runtime generator, generic
passes, xtask regen commands.

Plan must remove one coherent leak family and name the same-wave
generator/check consumer.

Redress closes only with leak grep, generated-output proof, and JSON
guard rerun if JSON-adjacent generation changes.

### W4 - PRUNE-D Pattern H Discipline

Research scopes: 67 root runtime files, generator provenance, non-writing
regen/check route.

Plan must distinguish true generated provenance from header-only edits.

Redress closes only with the 67-file count, line-1 provenance scan, and
regen/check proof or intrinsic-block route.

### W5 - REBUILD-E CSS Typed Value API

Research scopes: CSS grammar sources, codegen/runtime CSS provider,
CSS value/document/view/visitor shape, cssparser comparator workload.

Plan must build typed output before retiring old parser proof.

Redress closes only with typed CSS output, same-workload retiming, JSON
guard maintain, and no reuse of broadcast measurements.

### W6 - REBUILD-F.1 Decision Engine Spine

Research scopes: e-graph, CSP, CostFacts, generated selection evidence.

Plan must name one real rewrite and one non-tautological CSP condition.

Redress closes only when generated behavior or plan selection can change
and generic decision facts are grammar-neutral.

### W7 - REBUILD-F.2 BackendShape Lowerers

Research scopes: all five lowerer files, codegen fixtures, generated
runtime diff tests.

Plan must replace label-string scaffold with real output or a
gate-consumed rejected alternative.

Redress closes only when tests fail against the old scaffold and no
lowerer remains placeholder-only.

### W8 - REBUILD-G FNV Quarantine

Research scopes: bench/xtask FNV helpers, strict-product comparator,
production FNV scan.

Plan must quarantine FNV and add adversarial semantic fixtures.

Redress closes only when FNV cannot be runtime selector, arbiter, or
correctness proof.

### W9 - Close Reconciliation

Research scopes: RESULTS, REDRESS, rolling delta, HANDOFF, PASS-IMPL V2.

Plan must reconcile all admitted, diagnostic, retired, deleted, and
blocked states.

Redress closes only when PASS-IMPL V2 executes and every dependency row
is admitted, redressed, or intrinsically blocked.

## Section 5 - Same-Wave Consumer Mandate

Any primitive, kernel, new generated path, new parser helper, new
telemetry field, or gate report must be consumed by the hot path or gate
in the same wave. Source-present but unwired is REJECT unless the wave
explicitly deletes, scalar-delegates, or records intrinsic block.

## Section 6 - Commit And Evidence Discipline

Use the local commit-discipline skill for every commit. Stage only the
wave slice. Broad gate/status/generated/deletion commits need a body that
states why, what landed, evidence, and routed remainder.

Wave evidence must include exact commands or artifacts. Documentation-only
close, future-phase promises, warm benches, x86 admission anchors, and
hidden broadcast measurements are REJECT.

## Section 7 - Stop Conditions

Stop only at G-Omega, unrepaired invariant violation, or SK-V17 close
under the active user extension. Otherwise continue the SK loop.
