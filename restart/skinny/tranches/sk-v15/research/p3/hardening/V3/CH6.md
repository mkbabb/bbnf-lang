# SK-V15 S-P3 V3 CH6 ANTI-PAPER-CLOSE

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3. Lens: CH6.
Date: 2026-05-28.
HEAD: `efe1e4b01`.
Scope: audit the active S-P3 P3-C, SPEC, and DISPATCH packet for paper-close
substitution, producer-only evidence, source-present-but-unwired primitives,
CSS wrong-workload proof, and lowerer/Decision gates without executable
consumers.

## Verdict

ACCEPT.

The V3 packet preserves and strengthens the V2 CH6 acceptance conditions. It
does not allow SK-V16 routing to substitute for SK-V15 close evidence; it makes
producer-only telemetry and report-only gates reject; it requires source-present
primitives to carry oracle/parity/checkasm status and same-wave consumers; it
keeps CSS proof on fresh typed same-workload `cssparser` comparison rather than
brace counters, fact streams, or W8R tuples; and it names executable consumers
for Decision Engine and BackendShape lowerer gates.

## Evidence Table

| id | status | evidence | disposition |
|---|---|---|---|
| CH6-V3-01 | ACCEPT | SK-V16 cannot stand in for SK-V15 close: SPEC says implementation-limited misses become REDRESS/revert/demotion/intrinsic block and that SK-V16 routing is only routed remainder after proof (`SPEC.md:82-84`), W11 prepares SK-V16 input only after SK-V15 proof exists (`SPEC.md:451-463`), P3-C states SK-V16 routing is not close evidence (`p3c-falsifiability-gates.md:338-343`), and DISPATCH aborts W11 on unresolved fixes/measurements/dependencies instead of deferring them (`DISPATCH-PROMPT.md:309-313`). | No edit required. |
| CH6-V3-02 | ACCEPT | Producer-only telemetry is rejected across all active surfaces: P3-C requires W0 fields to be gate-consumed and says producer-only telemetry rejects (`p3c-falsifiability-gates.md:69-87`), SPEC requires every emitted field to be parsed by `gate-json` or successor and rejects producer-only telemetry (`SPEC.md:100-122`), and DISPATCH requires W0 gate consumption plus rejects hidden broadcast floors in evidence discipline (`DISPATCH-PROMPT.md:123-127`, `DISPATCH-PROMPT.md:343-345`). | No edit required. |
| CH6-V3-03 | ACCEPT | Source-present primitives cannot close as source-only artifacts: SPEC requires scalar reference or oracle, parity/checkasm where relevant, and a same-wave consumer (`SPEC.md:143-145`); P3-C requires oracle/parity/checkasm and same-wave hot-path consumers for primitives/kernels/generator paths (`p3c-falsifiability-gates.md:39-41`) and classifies primitive status through gate-consumed reports (`p3c-falsifiability-gates.md:89-107`, `p3c-falsifiability-gates.md:170-179`); DISPATCH makes source-present-but-unwired REJECT unless deleted, scalar-delegated, or intrinsically blocked (`DISPATCH-PROMPT.md:318-323`). | No edit required. |
| CH6-V3-04 | ACCEPT | CSS proof is typed same-workload proof, not W8R, fact-stream, or brace-counter proof: P3-C bans W8R as a floor and defines same-workload typed CSS value/document comparison (`p3c-falsifiability-gates.md:27-38`), W5 only admits typed provider output while old proof remains diagnostic (`p3c-falsifiability-gates.md:214-230`), W6 requires fresh same-run `cssparser` typed comparison and retires `CSS_GENERATED_RS`, `CssFullParseSummary`, fact-stream-only `parse()`, and brace-counter proof only with same-wave typed proof (`p3c-falsifiability-gates.md:232-247`); SPEC and DISPATCH repeat the same W5/W6 split (`SPEC.md:336-376`, `DISPATCH-PROMPT.md:186-216`). | No edit required. |
| CH6-V3-05 | ACCEPT | Decision and lowerer gates have executable same-wave consumers: P3-C names exact Decision tests and lowerer/all-five gate commands or equivalent successors (`p3c-falsifiability-gates.md:249-308`), while DISPATCH carries the same required consumers for W7-W9 (`DISPATCH-PROMPT.md:218-280`). SPEC requires the Decision Engine to show an asserted e-graph rewrite, non-tautological CSP, and real lowerer implementation paths before close (`SPEC.md:71-73`, `SPEC.md:378-428`). | No edit required. |
| CH6-V3-06 | ACCEPT | The top-level close rule remains measurement- or gate-proof based: P3-C rejects "wired", "integrated", "advisory", "future consumer", and "next wave will measure" closes (`p3c-falsifiability-gates.md:17-20`); SPEC requires command output, generated artifacts/diffs, strict parity/checkasm, and cold per-parse measurement evidence where relevant (`SPEC.md:76-80`, `SPEC.md:457-463`); DISPATCH rejects documentation-only close and future-phase promises (`DISPATCH-PROMPT.md:343-345`). | No edit required. |

## Verification

Read and evaluated:

```sh
sed -n '1,260p' restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
nl -ba restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md | sed -n '1,360p'
nl -ba restart/skinny/tranches/sk-v15/SPEC.md | sed -n '1,560p'
nl -ba restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md | sed -n '1,430p'
sed -n '1,220p' restart/skinny/tranches/sk-v15/research/p3/hardening/V2/CH6.md
git rev-parse --short HEAD
```

No REVISE edits are required for the CH6 lens.
