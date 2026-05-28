# SK-V15 S-P3 V4 CH6 ANTI-PAPER-CLOSE

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V4. Lens: CH6.
Date: 2026-05-28.
HEAD: `21ae60663`.
Scope: audit the active S-P3 P3-C, SPEC, and DISPATCH packet for paper-close
substitution, producer-only evidence, source-present-but-unwired primitives,
CSS wrong-workload proof, and lowerer/Decision gates without executable
consumers.

## Verdict

ACCEPT.

The active V4 packet preserves the V3 CH6 acceptance conditions. It does not
let SK-V16 routing substitute for SK-V15 close evidence, does not accept
producer-only telemetry or report-only gates, requires source-present
primitives to have oracle/parity/checkasm status plus a same-wave consumer,
keeps CSS proof on fresh typed same-workload `cssparser` comparison, and names
executable consumers for Decision Engine and BackendShape lowerer work.

## Evidence Table

| id | status | evidence | disposition |
|---|---|---|---|
| CH6-V4-01 | ACCEPT | SK-V16 is explicitly not close evidence: SPEC says implementation-limited misses become REDRESS, revert, demotion, or intrinsic block with proof, and SK-V16 routing is only routed remainder after proof (`SPEC.md:82-84`); W11 prepares SK-V16 input only after SK-V15 proof exists and repeats that it cannot substitute for repair (`SPEC.md:447-463`); P3-C states SK-V16 routing is not close evidence (`p3c-falsifiability-gates.md:328-346`); DISPATCH aborts W11 on unresolved implementation fixes, measurement reruns, or dependency rows instead of deferring them (`DISPATCH-PROMPT.md:301-316`). | No edit required. |
| CH6-V4-02 | ACCEPT | Producer-only telemetry rejects across the active packet. P3-C requires the W0 carrier to be gate-consumed and rejects producer-only telemetry and one-to-N measurement stamps (`p3c-falsifiability-gates.md:69-87`), and it rejects scan reports not consumed by the gate (`p3c-falsifiability-gates.md:89-107`). SPEC requires every emitted field to be parsed by `gate-json` or successor and rejects producer-only telemetry, hidden stamps, self-exempting exclusions, and source-present unwired primitives (`SPEC.md:100-122`). DISPATCH requires W0 gate consumption and rejects docs-only or hidden-floor evidence (`DISPATCH-PROMPT.md:118-127`, `DISPATCH-PROMPT.md:337-345`). | No edit required. |
| CH6-V4-03 | ACCEPT | Source-present primitives cannot ship as paper artifacts. P3-C requires any primitive, kernel, generator path, or new API surface to carry scalar reference or executable oracle, parity/checkasm where relevant, and a named same-wave hot-path consumer (`p3c-falsifiability-gates.md:39-41`); each candidate row binds final threshold, same-wave consumer, proof command shape, and fail action (`p3c-falsifiability-gates.md:51-67`). SPEC makes the same oracle/parity/checkasm/same-wave consumer rule non-negotiable (`SPEC.md:143-145`) and rejects source-present unwired primitives through required telemetry (`SPEC.md:119-122`). DISPATCH rejects source-present but unwired unless deleted, scalar-delegated, or intrinsically blocked (`DISPATCH-PROMPT.md:318-323`). | No edit required. |
| CH6-V4-04 | ACCEPT | CSS cannot close on W8R, brace counters, fact streams, or wrong-workload proof. P3-C marks the W8R tuple as a diagnostic negative fixture and defines CSS admission as typed same-workload value/document comparison (`p3c-falsifiability-gates.md:27-38`); W5 admits only typed provider output and W6 requires fresh same-run `cssparser` typed comparison before old CSS proof paths retire (`p3c-falsifiability-gates.md:214-247`). SPEC's close condition and W5/W6 sections require typed CSS surfaces, same-workload comparator proof, and retirement of `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter admission (`SPEC.md:54-63`, `SPEC.md:336-376`). DISPATCH repeats the typed-provider-before-retirement and fresh cssparser retime contract (`DISPATCH-PROMPT.md:186-216`). | No edit required. |
| CH6-V4-05 | ACCEPT | Decision and lowerer work have executable same-wave consumers, not prose close. P3-C names exact Decision Engine tests or proven successors plus lowerer/all-five gate consumers (`p3c-falsifiability-gates.md:249-308`). SPEC requires an asserted e-graph rewrite, non-tautological CSP, grammar-neutral facts, and all five BackendShape lowerers emitting real implementation paths before close (`SPEC.md:71-73`, `SPEC.md:378-428`). DISPATCH carries the same required consumer commands for W7-W9 (`DISPATCH-PROMPT.md:218-280`). | No edit required. |
| CH6-V4-06 | ACCEPT | S-P3 itself cannot paper-close implementation. The pass contract says S-P3 consumes Pass Alpha's goalset and authors the wave plan; implementation lands only in the wave triumvirate redress phase (`PASS-3-SYNTHESIS-PLAN.md:14-18`, `PASS-3-SYNTHESIS-PLAN.md:43-45`). SPEC likewise forbids implementation dispatch from S-P3 and requires W0 only after S-P3 convergence plus gate authorization (`SPEC.md:29-43`), then states S-P3 produces the contract and the orchestrator dispatches W0 through the SKINNY triumvirate after convergence and required gating (`SPEC.md:486-495`). | No edit required. |
| CH6-V4-07 | ACCEPT | The packet carries fail/revert/routing semantics instead of future-phase promises. P3-C rejects "wired", "integrated", "advisory", "future consumer", and "next wave will measure" closes (`p3c-falsifiability-gates.md:17-20`), binds each candidate to a fail action (`p3c-falsifiability-gates.md:58-67`), and gives W11 a PASS-IMPL consumer before close (`p3c-falsifiability-gates.md:328-346`). SPEC routes misses to REDRESS/revert/demotion/intrinsic block with proof (`SPEC.md:82-84`) and blocks W12/CHALLENGE overflow as implementation escape hatches (`SPEC.md:165-170`). DISPATCH requires dependency rows, budget gates, same-wave consumers, and evidence commands before redress (`DISPATCH-PROMPT.md:41-66`, `DISPATCH-PROMPT.md:318-345`). | No edit required. |

## Verification

Read and evaluated:

```sh
git rev-parse --short=9 HEAD
git status --short
sed -n '1,240p' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
sed -n '1,320p' restart/skinny/tranches/sk-v15/SPEC.md
sed -n '1,320p' restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH6.md
sed -n '1,220p' restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md | sed -n '1,380p'
nl -ba restart/skinny/tranches/sk-v15/SPEC.md | sed -n '1,520p'
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md | sed -n '1,420p'
```

No REVISE edits are required for the CH6 lens.
