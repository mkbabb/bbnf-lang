# SK-V15 S-P3 V2 CH2 Generality Challenge

Cycle: S-P3 Synthesis-Plan V2.
Date: 2026-05-28.
Input commit: `39e186ee3`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH2.md`.

## Verdict

REVISE.

The V2 packet fixes the major V1 generality failures: W0-W11 is coherent, W8R CSS metrics are diagnostic-only, CSS typed admission is tied to fresh same-workload `cssparser` evidence, and EventTape is bound as one BackendShape lowerer. It still misses two CH2 load-bearing folds: the non-JSON proof receiver matrix is not promoted into current dispatch/spec surfaces, and DISPATCH does not carry the actual Lock 14 / Lock 16 exclusion-report schema.

## Findings

| id | verdict | finding | evidence | required follow-up |
|---|---|---|---|---|
| CH2-V2-01 | REVISE | The V1 fold roster required a surface-specific non-JSON proof receiver table for generic surfaces, but V2 only has broad receiver rules. | The required fold names codegen provider/runtime generator, lowerers, e-graph, CSP, cost facts, xtask regen, and gate/report code (`HARDENING-S-P3-V1-CONSOLIDATED.md:49`-`:51`). Current V2 uses generic rows in P3-B and SPEC (`p3b-wave-sequencing.md:64`-`:80`; `SPEC.md:188`-`:200`) and DISPATCH names scopes/commands without a receiver matrix (`DISPATCH-PROMPT.md:104`-`:113`, `:149`-`:163`). `rg` found no current table rows for `grammar_provider`, `runtime_generator`, `cost.rs`, `gate.rs`, or `report.rs` in P3-C/SPEC/DISPATCH. | Add a non-JSON receiver matrix to P3-C, SPEC, and DISPATCH with rows for grammar provider, runtime generator, lowerers, e-graph, CSP, CostFacts/cost, xtask regen, and gate/report code. Each row must name minimum non-JSON receivers, proof shape, and intrinsic-block handling. |
| CH2-V2-02 | REVISE | Lock 14 / Lock 16 exclusion reporting is concrete in P3-C/SPEC but underspecified in DISPATCH, which is a primary wave-agent entry surface. | P3-C carries included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition (`p3c-falsifiability-gates.md:71`-`:89`, `:135`-`:137`). SPEC repeats the schema requirements (`SPEC.md:202`-`:204`). DISPATCH only says the plan must name scan roots and an exclusion-report schema, then close when gates consume their reports (`DISPATCH-PROMPT.md:94`-`:102`). | Mirror the exclusion-report columns in DISPATCH or add an explicit fail-closed DISPATCH table that points to SPEC and names all required fields. A W2/W3/W4/W6/W7/W8/W9 plan missing the consumed schema must not dispatch. |
| CH2-V2-03 | ACCEPT | CSS typed provider generality no longer reuses W8R as a per-feature SOTA floor. | P3-A excludes W8R from live floors and requires fresh independent CSS typed measurements (`p3a-candidate-shortlist.md:16`, `:29`). P3-C labels the W8R tuple a diagnostic negative fixture and makes W6 derive floors from fresh same-run `cssparser` typed comparison (`p3c-falsifiability-gates.md:32`-`:38`, `:197`-`:212`). SPEC and DISPATCH carry the same W5/W6 split (`SPEC.md:297`-`:329`; `DISPATCH-PROMPT.md:125`-`:147`). | Preserve this split. Do not merge W5 provider construction with W6 retime/old-proof retirement unless the same wave satisfies both exit gates. |
| CH2-V2-04 | ACCEPT | EventTape is counter-bound as one of the existing five BackendShape lowerers, not a sidecar or sixth shape. | P3-B, P3-C, SPEC, and DISPATCH all reject EventTape as sidecar vector, sixth shape, retained stream, public substrate API, or alternate projection (`p3b-wave-sequencing.md:60`; `p3c-falsifiability-gates.md:260`-`:266`; `SPEC.md:202`-`:206`, `:363`-`:378`; `DISPATCH-PROMPT.md:182`-`:200`). | Preserve the W9 all-five gate and require generated-runtime evidence or a gate-consumed rejected alternative for EventTape. |
| CH2-V2-05 | ACCEPT | Stale V1 topology labels are absent from the S-P3 V2 packet surfaces checked. | `rg -n "P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|W0-W9|Cycle: V1|S-P3 V1"` over P3-A..P3-F, SPEC, and DISPATCH returned no matches. | None. |

## Required Follow-Up

1. Add the missing non-JSON proof receiver matrix to P3-C, SPEC, and DISPATCH. Minimum rows: `grammar_provider.rs`, `runtime_generator.rs`, lowerers, `backend_egraph.rs`, `decision_csp.rs`, CostFacts/cost, xtask regen, and gate/report code.
2. Promote the exact Lock 14 / Lock 16 exclusion-report schema into DISPATCH, not just P3-C/SPEC. The schema must include included roots, excluded roots, reason, owner, self-scan status, primitive status, gate consumer, affected rows, and disposition.
3. Re-run CH2 after the fold. CH2 cannot ACCEPT while either generic proof receivers or DISPATCH scan-schema consumption remain prose-only.

## Verification

Commands run:

```sh
rg -n "P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|W0-W9|Cycle: V1|S-P3 V1" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md || true
rg -n "grammar_provider|runtime_generator|backend_egraph|decision_csp|CostFacts|cost\\.rs|xtask|gate\\.rs|report\\.rs|Generic surface|Minimum non-JSON receivers" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md || true
rg -n "included roots|excluded roots|reason, owner|self-scan status|primitive status|gate consumer|affected rows|disposition|exclusion report schema|Plan must name scan roots" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "2319\\.041|2362\\.037|929\\.281|diagnostic negative fixture only|never a typed-admission floor|fresh same-workload|cssparser" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "EventTape is only|sidecar vector|sixth shape|public substrate API|alternate document projection|all-five BackendShape|exactly five BackendShape" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Recommended after follow-up:

```sh
rg -n "grammar_provider|runtime_generator|backend_egraph|decision_csp|CostFacts|cost\\.rs|xtask|gate\\.rs|report\\.rs" restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "included roots|excluded roots|self-scan status|primitive status|gate consumer|affected rows|disposition" restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```
