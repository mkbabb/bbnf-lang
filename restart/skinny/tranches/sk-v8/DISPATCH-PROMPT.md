# SK-V8 Dispatch Prompt

This is the implementation-agent dispatch contract for skinny iteration SK-V8.
It binds to the SK-V8 packet at `restart/skinny/tranches/sk-v8/`.

Do not dispatch any SK-V8 implementation wave from S-P3 alone. G-Alpha user
signoff is still required. If G-Alpha closes, dispatch W0 only. W1-W6 are
conditional: they require W0 close, a fresh wave plan with exact owner paths and
row gates, required challenge acceptance, and orchestrator/user dispatch.

## Required Reading

Read in order:

1. `docs/precepts/instructions/README.md` and
   `docs/precepts/instructions/STYLE.md`.
2. `docs/precepts/instructions/ORCHESTRATION.md`.
3. `docs/precepts/instructions/tranche/README.md` and
   `docs/precepts/instructions/tranche/SPEC.md`.
4. `restart/prompts/README.md`.
5. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
6. `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
7. `restart/skinny/tranches/sk-v8/SYNTHESIS.md`.
8. `restart/skinny/tranches/sk-v8/SPEC.md`.
9. `restart/skinny/tranches/sk-v8/HANDOFF.md`.
10. `restart/skinny/tranches/sk-v8/research/p3/`.
11. `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/`.
12. `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`.
13. `restart/skinny/tranches/sk-v8/research/alpha/`.
14. `skinny/RESULTS.md`.
15. `skinny/REDRESS.md`.

## Wave Manifest

| Wave | SPEC section | Title | Dispatch status | Implementation/redress cap |
|---|---|---|---|---:|
| W0 | Section 3 | Baseline Profile And Telemetry Lock | After G-Alpha | <=90 min |
| W1 | Section 4 | CostFacts And Comparator Gate Binding | Conditional after W0 | <=90 min |
| W2 | Section 5 | Typed Product Plane Expansion | Conditional after W0/W1 | <=90 min |
| W3 | Section 6 | Tier A Tape Plus Structural-Projection Union | Conditional after W0/W1 and challenge | <=90 min |
| W4 | Section 7 | Direct Guard Triage | Conditional after W0/W1 and W2/W3 disposition | <=90 min |
| W5 | Section 8 | Grammar-Neutral Audit And Lock 14 Preservation | Conditional after W1-W4 dispositions | <=90 min |
| W6 | Section 9 | Close And Alpha Feedback | Conditional after W0-W5 dispositions | <=90 min |

Research is capped at 30 minutes per agent, max 6 agents. Plan synthesis is
capped at 30 minutes. Challenge is capped at 90 minutes when required.
Implementation/redress is capped at 90 minutes inclusive of source edits,
generation, verification, RESULTS/REDRESS updates, and rollback. If the slice
cannot fit, split before dispatch or return REVISE.

## W0 Dispatch Protocol

W0 is telemetry-only. It may touch only the W0 owner paths in SPEC Section 3.

Phase 1 - Research:

- Dispatch 1-6 parallel research agents.
- Each writes one artifact under
  `restart/skinny/tranches/sk-v8/research/wave-0-*.md`.
- Hard cap 30 min per agent.
- Commit form, if a commit is requested by the user:
  `docs(sk-v8-wave0-research): archive baseline telemetry cohort`.

Phase 2 - Plan:

- Read research outputs.
- Write one wave-0 plan artifact under
  `restart/skinny/tranches/sk-v8/research/`.
- Include owner paths, `SK-V8-open` capture method, telemetry fields, gate
  changes, malformed-manifest test, no-behavior-change proof, revert protocol,
  same-wave consumer, and pre-blocked routes.

Phase 3 - Redress:

- Implement only W0 telemetry and gate validation.
- Run focused bbnf-bench/xtask tests named by the W0 plan.
- Run the W0-updated `gate-json` path.
- Verify every current main row has required SK-V8 telemetry.
- Verify throughput cells move no more than +/-1.0% versus `SK-V8-open`.
- Verify no parser, scanner, SIMD, asm, codegen behavior, product-plane
  behavior, or generated parser output change lands.
- PASS disposition: admit baseline profile and telemetry lock.
- FAIL disposition: reject baseline profile and telemetry lock with REDRESS.

## Conditional Wave Gates

Do not dispatch W1-W6 from this prompt alone.

Before any conditional wave:

1. W0 must be admitted.
2. The wave must have a fresh research artifact and plan artifact.
3. Exact owner paths, row gates, same-wave consumer, and revert protocol must be
   named.
4. Pre-blocked routes must be cited with exact REDRESS or P3 references.
5. High-risk behavior, substrate-touching, primitive, or first-of-class waves
   must receive challenge acceptance before redress.
6. Generic-crate edits must include the Lock 14 gate from SPEC Section 2.1,
   including non-JSON proof when relevant.
7. The implementation/redress slice must fit <=90 minutes or split before
   dispatch.

## Per-Wave Dispatch Notes

W1 CostFacts and comparator gate binding:

- Entry: W0 admitted and every row has `SK-V8-open` telemetry.
- Consumer: `gate-json --with-cost-facts` and strict-admission refusal gates.
- Blocked: behavior changes, generic JSON policy, generated-output drift,
  producer-only CostFacts.

W2 typed product plane expansion:

- Entry: W0/W1 admitted and W2 plan names exact typed rows and host/API schema
  facts.
- Consumer: generated Track 1 typed row plus independent Track 2/oracle.
- Blocked: direct digest as product proof, benchmark-private parser, hidden
  directive/schema fact, Track 2 coupling.

W3 Tier A tape plus structural-projection union:

- Entry: W0/W1 admitted; fresh W3 plan; challenge ACCEPT; Lock 1 fork resolved
  or routed.
- Consumer: generated JSON retained parser consuming retained `Tape`
  positions/classes in the same wave.
- Blocked: Tier B string-boundary/parity, `tape_vs_tape` as production
  consumer, sidecar/aux/parser-owned cursor, old offset append path, new
  substrate surface, `UnionTape`, new `BackendShape`, directive, BIR, or public
  substrate API.

W4 direct guard triage:

- Entry: W0/W1 admitted and W2/W3 admitted/rejected/routed or W3 explicitly
  blocked.
- Consumer: selected direct row generated Track 1 plus independent Track 2.
- Blocked: digest as product-plane proof, direct string/materialization
  repeats, parser-owned scratch, Track 2 coupling, raw f64 shortcut.

W5 grammar-neutral audit:

- Entry: W1-W4 admitted/rejected/routed.
- Consumer: audit gate or named small cleanup consumed by existing tests.
- Blocked: generic JSON public APIs, grammar-name branches,
  `StructuralAlphabet::json`, generated-output drift disguised as audit.

W6 close:

- Entry: W0-W5 admitted/rejected/routed.
- Consumer: close checklist and artifact reconciliation.
- Blocked: paper close, missing REDRESS, missing RESULTS rows, unresolved Lock
  1/Omega fork, hidden sidecar/permissive strict admission.

## Pre-Blocked Routes

Use SPEC Section 10 and P3-E as the pre-block authority. Do not reopen a listed
route without fresh W0 evidence, same-wave consumer, scalar reference and
checkasm where relevant, no-regression gate, REDRESS citation, and challenge
acceptance.

Always blocked unless a future accepted plan explicitly reopens:

- New directive, BIR variant, substrate surface, `BackendShape`, `UnionTape`,
  public substrate API, sidecar substrate, parser-owned cursor/facts, or
  parallel substrate.
- `tape_vs_tape`, `parse_only`, sidecar, permissive, lossy, stale, or telemetry
  evidence as strict admission.
- REDRESS 28+33, 50-55, 60-72, 80, 82, 83, 84, 88, 89, and B6 canary as
  performance evidence.
- Function-pointer dispatch, pair-token fusion, 12-byte token churn, separator
  elision, generic SWAR whitespace, capacity prescan, raw f64 shortcut,
  EventCursor/parallel prepasses, and orphan primitive admission.
- Tier B string-boundary / quote-backslash-parity / CostFacts-template union
  inside W3 Tier A.

## Non-Negotiables

- Strict-vs-strict comparator gates only for strict admission.
- Sidecar/permissive/lossy comparators are planning signals or flaw probes.
- Lock 14 grammar neutrality is a per-wave exit gate.
- No source change without a same-wave consumer and measured gate.
- Scalar reference and checkasm before primitive wiring.
- No SK-V8 wave closes without measured evidence or REDRESS.
- No role merger: research, plan, challenge when required, and redress remain
  distinct phases.

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
- S-P2 V6 and V7 form the two qualifying ACCEPT cycles.
- S-P3 folded plan exists.
- G-Alpha is not yet closed.

No implementation dispatch until the user signs off with `G-Alpha closed`.
