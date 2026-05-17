# SK-V8 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration SK-V8.
It binds to the SK-V8 packet at `restart/skinny/tranches/sk-v8/`.

Do not dispatch any SK-V8 wave until G-Alpha is closed by the user.

If G-Alpha closes, dispatch W0 only. W1-W6 are conditional and require W0 close
plus plan augmentation before implementation.

## Required Reading

Read in order:

1. `docs/precepts/instructions/README.md` and
   `docs/precepts/instructions/STYLE.md`.
2. `docs/precepts/instructions/ORCHESTRATION.md`.
3. `docs/precepts/instructions/tranche/README.md` and
   `docs/precepts/instructions/tranche/SPEC.md`.
4. `restart/prompts/README.md`.
5. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
6. `restart/skinny/tranches/sk-v8/SYNTHESIS.md`.
7. `restart/skinny/tranches/sk-v8/SPEC.md`.
8. `restart/skinny/tranches/sk-v8/HANDOFF.md`.
9. `restart/skinny/tranches/sk-v8/research/alpha/`.
10. `restart/skinny/tranches/sk-v8/research/alpha-hardening/V1/`.
11. `skinny/RESULTS.md`.
12. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | Hard cap |
|---|---|---|---|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | After G-Alpha | 180 min |
| W1 | Section 4 | CostFacts Gate Binding | After W0 plus plan update | 240 min |
| W2 | Section 5 | Typed Product Plane Expansion | Conditional | 300 min |
| W3 | Section 6 | Profile-Selected Parse Candidate | Conditional | 300 min |
| W4 | Section 7 | Direct Guard Triage | Conditional | 240 min |
| W5 | Section 8 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional | 180 min |
| W6 | Section 9 | Close And Alpha Feedback | Conditional | 120 min |

## W0 Dispatch Protocol

W0 is telemetry-only. It may touch only the W0 owner paths in SPEC Section 3.

Phase 1 - Research:

- Dispatch 1-6 parallel research agents.
- Each writes one artifact under
  `restart/skinny/tranches/sk-v8/research/wave-0-*.md`.
- Hard cap 30 min per agent.
- Commit: `docs(sk-v8-wave0-research): archive baseline telemetry cohort`.

Phase 2 - Plan:

- Read research outputs.
- Write one plan artifact at
  `restart/skinny/tranches/sk-v8/research/wave-0-plan.md`.
- Include owner paths, `SK-V8-open` capture method, telemetry fields, gate
  changes, malformed-manifest test, no-behavior-change proof, revert protocol,
  and pre-blocked routes.
- Commit: `docs(sk-v8-wave0-plan): select baseline profile and telemetry lock`.

Phase 3 - Redress:

- Implement only W0 telemetry and gate validation.
- Run focused bbnf-bench/xtask tests named by the W0 plan.
- Run `cargo test --workspace` unless the plan gives a narrower documented
  test directive accepted by challenge.
- Run `cargo run -p xtask --release -- gate-json` or the W0-updated equivalent.
- Verify every current main row has required SK-V8 telemetry.
- Verify throughput cells move no more than +/-1.0 percent versus
  `SK-V8-open`.
- PASS commit: `feat(sk-v8-wave0): admit baseline profile and telemetry lock`.
- FAIL commit: `docs(sk-v8-wave0-redress): reject baseline profile and telemetry lock`.

## Conditional Waves

Do not dispatch W1-W6 from this prompt alone.

Before any conditional wave:

1. W0 must be admitted.
2. The wave must have a fresh research artifact and plan artifact.
3. Exact owner paths and row gates must be named.
4. Pre-blocked routes must be cited.
5. High-risk behavior waves must receive challenge acceptance.
6. The user or orchestrator must confirm the plan update is in scope.
7. Generic-crate edits must include the generality and Lock 14 gate from
   `SPEC.md` Section 2.1, including non-JSON proof when relevant.

## Pre-Blocked Routes

Use `HANDOFF.md` Section 7 and `SPEC.md` Section 10 as the pre-block authority.
Do not reopen a listed route without fresh W0 evidence, same-wave consumer,
scalar reference and checkasm where relevant, no-regression gate, REDRESS
citation, and challenge acceptance.

## Non-Negotiables

- No new BBNF directives.
- No new BIR variant.
- No new substrate without same-wave consumer.
- No JSON policy in generic crates.
- No parser, scanner, SIMD, asm, codegen behavior, or product-plane behavior in
  W0.
- W0 creates the Lock 14 baseline allowlist and grammar-aware comparator fields.
- Strict-vs-strict comparator gates only.
- Sidecar/permissive comparators are not strict admission evidence.
- Research, plan, and redress land as distinct commits.
- No SK-V8 wave closes without measured evidence or REDRESS.

## Status Discipline

Emit status ticks during silent waits:

```text
[sk-v8-W{N}] {phase}: {agents} agents in flight; {returned} returned; ETA {time}
```

Before every status reply, reconcile:

- agent status;
- process list for cargo/rustc/zombie work;
- artifact mtimes.

## Entry Condition

Current state:

- SK-V7 is closed honestly through W10c.
- W10 PMULL and W10b CTZ/bulk are rejected.
- W10c B6 stack-canary Stage 1 is admitted.
- SK-V8 Alpha docs exist.
- G-Alpha is not yet closed.

No dispatch until the user signs off with `G-Alpha closed`.
