# PASS-IMPL-OVERFIT-AUDIT — SK Cycle Handoff Implementation Audit

This pass audits the **outgoing SK-V{N}'s implementation** to inform the **incoming SK-V{N+1}'s planning**. It is the complement to `PASS-0-OVERFIT-AUDIT.md`:

- `PASS-0` audits the SPEC pre-tranche — does the spec over-emphasise one grammar / hardcode a route / set up a fake gate?
- `PASS-IMPL` audits the IMPLEMENTATION post-tranche — does the landed code do what the spec promised? Did wave execution honor the generalization contract, or did per-corpus hand-tuning sneak in?

Both passes share the discipline that hand-crafted parsers are admissible during the >SOTA proof phase, but must be ruthlessly validated before the grammar-driven generalization inflection point. `PASS-IMPL` is the gatekeeper for that inflection.

## §1 — Trigger + entry condition

`PASS-IMPL` dispatches when SK-V{N}'s wave program has closed (W{max} R10 admitted; `skinny/ROLLING-SOTA-DELTA.md` shows full admission OR all rejections proven). It runs BEFORE the cycle-close handoff to SK-V{N+1}'s Pass Alpha, so its findings feed:

- Pass Alpha bracketing of SK-V{N+1} (carry-forward LACs + corrective wave receivers).
- S-P0 spec audit for SK-V{N+1} (the next cycle's spec audit benefits from knowing what the prior implementation contrived).
- Pass Omega CRUD if any finding rises to a spec amendment (rare — most findings are wave-graph corrections at the SK level, not lock/architecture).

If the cycle closed without full admission, `PASS-IMPL` still runs — the rejection ledger is part of the implementation surface.

## §2 — Scope matrix (six parallel sub-agents)

Each agent owns one overfitting axis. Each writes one report. Agents read independently from the source tree + the cycle-close artefacts (RESULTS.md, ROLLING-SOTA-DELTA.md, REDRESS.md, the wave-execution commit chain). Hard cap 30 min per agent.

| Agent | Axis | Output |
|---|---|---|
| **AUDIT-1 Grammar-1 hardcoding** | Per-corpus hot leaves, grammar-named identifiers outside legitimate runtime layer, fixture-keyed admission paths, per-corpus benchmark scaffolding bleeding into production. | `restart/audit/skinny-impl-overfit/V{V}/AGENT-1-{grammar-1}-hardcoding.md` |
| **AUDIT-2 Grammar-2 hardcoding** | Same axis as AUDIT-1 but for the second canonical grammar in scope. (SK-V14: JSON = AUDIT-1, CSS L4 = AUDIT-2.) | `AGENT-2-{grammar-2}-hardcoding.md` |
| **AUDIT-3 Pattern H runtime collapse** | Census of hand-written runtime files (`crates/core/src/runtime/<grammar>/`); `@generated` header trace; per-grammar runtime divergence; skinny-twin alignment. | `AGENT-3-pattern-h-runtime.md` |
| **AUDIT-4 Codegen/xtask Lock 14 leaks** | Grammar-named enum variants, match arms, function names, magic constants, template constants. Per-grammar Cmd variants. Exclusion holes in the Lock 14 grep gate itself. | `AGENT-4-codegen-xtask-leaks.md` |
| **AUDIT-5 Bench/test contrivances** | `CANONICAL_FIXTURE` / `CAPTURED_W2_INPUT` short-circuits, fake `@generated` headers, broadcast measurements (1 number → N rows), comparator-plane mismatch, `target-cpu=native` mandatory tuning, per-iter equality stamps. | `AGENT-5-bench-contrivances.md` |
| **AUDIT-6 Cross-cutting substrate/backend specialisation** | BackendShape lower-impls (5 shapes; verify each is real, not stub); FactStream schema; PrimitiveFacts / CostFacts grammar-named fields; Decision Engine status (SCAFFOLD vs LOAD-BEARING); e-graph rewrite-rule count; CSP substrate constraint tautology. | `AGENT-6-substrate-backend-specialisation.md` |

The Grammar-N agents (AUDIT-1, AUDIT-2) generalize to whatever pair of canonical grammars the cycle was proving. For SK cycles that prove >2 grammars (rare), add AUDIT-N+ proportionally.

Each agent's report carries: Findings (severity + verdict per item); Inventory tables (commits, files, line counts); Verdict (ACCEPT-AS-PROOF-OF-CONCEPT / PRUNE-REQUIRED / MIXED); Prune Recommendations (concrete path:line recipes); Inflection-point assessment (is this axis ready for grammar-driven backtrack?); Forward-lens note (what the next cycle's S-P0 should look for that this cycle missed).

## §3 — Consolidated synthesis

After all 6 agents commit (independent; no race because each writes its own file), the orchestrator authors `restart/audit/skinny-impl-overfit/V{V}/CONSOLIDATED-AUDIT.md` with:

1. **Headline** — one-sentence verdict per axis + cross-axis pattern (e.g. "grammar A honest; grammar B contrived").
2. **Dispositive findings (BLOCKERs)** — any finding severe enough to block the cycle handoff or require a Pass Omega cycle.
3. **Other significant findings** — MEDIUM/HIGH findings carried as SK-V{N+1} PRUNE-wave inputs.
4. **Inflection-point assessment** — a binary table per gate (parse perfected / value API perfected / >SOTA / substrate neutral / etc.) with current status.
5. **SK-V{N+1} entry constraints** — explicit PRUNE-WAVE-{A,B,C,...} + REBUILD-WAVE-{X,Y,...} formulations with concrete path:line recipes.
6. **Discipline forward-lens** — procedural addenda for future CHALLENGE lenses (e.g. NEW-CH3-V5-01 wave-graph cycle detection, NEW-CH5-V5-02 broadcast-admission detection).
7. **Trajectory summary** — per-axis projection across the next 1-3 cycles.

## §4 — Convergence + handoff

`PASS-IMPL` does not iterate per §3Z. It is a one-shot audit dispatched at cycle close. Its convergence is "all 6 agents returned + consolidated authored + handoff packet built".

The handoff packet is delivered to:

- **SK-V{N+1} Pass Alpha** — the corrective-receiver constraint set. Pass Alpha must bracket SK-V{N+1} with explicit receivers for each PRUNE-WAVE and REBUILD-WAVE proposed by `CONSOLIDATED-AUDIT.md` §5.
- **SK-V{N+1} S-P0** — the forward-lens addenda. S-P0 must add the new CH-procedural-addenda to its dispatch context.
- **Pass Omega V{V+1}** (only if a finding is spec-class) — surfaced via the same routing as the V3 W2R / V4 W4R precedents.

## §5 — Discipline (binding)

- **Read-only on source.** No PASS-IMPL agent edits production code. All amendments route through the next SK cycle's wave-triumvirate REDRESS dispatches.
- **Cite path:line for every claim.** Per LAC-1E-12 executable verification.
- **Hand-craft vs contrivance distinction is load-bearing.** Hand-crafted parsers are acceptable during >SOTA proof per user latitude. Contrivances (corpus-name branching, fixture short-circuits, broadcast measurements, fake-generated headers, brace-counter "full_parse" comparators) are not. The agent's job is to flag the latter without flagging the former.
- **Inflection-point gate.** The consolidated report MUST take a position on whether the cycle is at the grammar-driven generalization inflection point. "Yes" means the next cycle can be a generalization tranche. "No" means the next cycle must PRUNE the contrivances first.
- **Honest refutation.** If audit findings invalidate the cycle's >SOTA claim, the consolidated report says so plainly. Discoverable contrivance > paper-over.

## §6 — Output structure

```
restart/audit/skinny-impl-overfit/V{V}/
├── AGENT-1-{grammar-1}-hardcoding.md
├── AGENT-2-{grammar-2}-hardcoding.md
├── AGENT-3-pattern-h-runtime.md
├── AGENT-4-codegen-xtask-leaks.md
├── AGENT-5-bench-contrivances.md
├── AGENT-6-substrate-backend-specialisation.md
└── CONSOLIDATED-AUDIT.md
```

Cycle index V increments per execution: V1 closes SK-V14; V2 closes SK-V15; V3 closes SK-V16; etc.

## §7 — Relationship to the SK loop

The SK process per cycle becomes:

```
┌─────────────────── one SK cycle ───────────────────┐
│                                                    │
│ (0) PASS-IMPL-OVERFIT-AUDIT (new)                  │
│     Audits prior SK-V{N-1} implementation.         │
│     Feeds Pass Alpha + S-P0.                       │
│     Cycle 0 (SK-V14 → SK-V15) is the first run.    │
│                                                    │
│ (1) Pass Alpha (bracket SK-V{N})                   │
│                                                    │
│ (2) Skinny passes S-P0/S-P1/S-P2/S-P3 → §3Z LOCK   │
│                                                    │
│ (3) Totality passes T-P1/T-P2/T-P3 → §3Z LOCK      │
│                                                    │
│ (4) Pass Omega (G-Omega gated)                     │
│                                                    │
│ (5) Wave implementation W0..W{max}                 │
│                                                    │
│ (6) Cycle close (R10 admitted or proven blocked)   │
│     ─── return to (0) for SK-V{N+1} ───            │
└────────────────────────────────────────────────────┘
```

`PASS-IMPL` is the first step of each cycle (closing the prior one). It is the audit equivalent of post-flight inspection: every flight starts with the post-flight from the previous flight.

## §8 — Closing posture

`PASS-IMPL` is the gate that keeps the campaign honest. It catches the gap between what the spec promised and what the code delivered. It is the only place in the SK loop where the implementation is interrogated against the generalization contract.

The pass does not slow the cycle: 6 parallel agents at 30 min cap = ~30 min wall. The consolidated synthesis is another 15-20 min. The handoff packet feeds into Pass Alpha and S-P0 dispatches that would have happened anyway.

The cost is one cycle-handoff hour. The benefit is no shipped contrivance, no false >SOTA claim, no Pattern H regression, no Lock 14 gate hole.
