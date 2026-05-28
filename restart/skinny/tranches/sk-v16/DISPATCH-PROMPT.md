# SK-V16 DISPATCH-PROMPT - Per-Wave Triumvirate Dispatch Contract

Date: 2026-05-28.

Status: S-P3 V1 dispatch contract for SK-V16 W0-W11. Every SK-V16 wave is
dispatched as research -> plan -> redress per
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.

## Section 0 - Authority

Read in this order before dispatching any SK-V16 wave:

1. `restart/skinny/tranches/sk-v16/SPEC.md`
2. `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
3. `restart/skinny/tranches/sk-v16/HANDOFF.md`
4. `restart/skinny/tranches/sk-v16/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
5. `restart/skinny/tranches/sk-v16/research/p1/hardening/V2/CONSOLIDATED.md`
6. `restart/skinny/tranches/sk-v16/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
7. `restart/skinny/tranches/sk-v16/research/p3/p3a-candidate-shortlist.md`
8. `restart/skinny/tranches/sk-v16/research/p3/p3b-wave-sequencing.md`
9. `restart/skinny/tranches/sk-v16/research/p3/p3c-falsifiability-gates.md`
10. `restart/skinny/tranches/sk-v16/research/p3/p3d-telemetry-schema.md`
11. `restart/skinny/tranches/sk-v16/research/p3/p3e-preblocked-ledger.md`
12. `restart/skinny/tranches/sk-v16/research/p3/p3f-spec-draft.md`
13. `restart/skinny/tranches/sk-v16/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md`
14. `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
15. `restart/prompts/ORCHESTRATOR.md`
16. `restart/locks/LOCKS.md`
17. `skinny/RESULTS.md`, `skinny/REDRESS.md`

## Section 1 - Triumvirate Contract

| Phase | Purpose | Source edits | Cap | Commit prefix |
|---|---|---|---:|---|
| Research | read-only diagnosis and owner-path evidence | no | <=30 min | `docs(sk-v16-wave{W}-research):` |
| Plan | one intervention, owner paths, gates, revert route | no | <=30 min | `docs(sk-v16-wave{W}-plan):` |
| Redress | implementation or ledger repair plus measurement | yes | <=75 min | `feat(sk-v16-wave{W}):` or `docs(sk-v16-wave{W}-redress):` |

Redress without a committed plan is invalid. A plan without antecedent research
is invalid. Role merger is invalid.

## Section 2 - Pre-Dispatch Verification

Before a wave dispatch:

1. Verify prior required waves are admitted, rejected, routed, or
   intrinsically blocked per `SPEC.md`.
2. Inspect dirty/staged state and preserve unrelated work.
3. Verify the selected wave has owner paths, tasks, exit gate, revert
   protocol, and pre-block list in `SPEC.md`.
4. Verify no S-P2 REJECT or REDRESS pre-block re-enters under old framing.
5. Verify Apple M5 Max / aarch64 is the only admission target.
6. Verify generic/generated edits carry the Lock 14/generality gate.
7. Verify any native primitive plan names scalar reference, strict
   checkasm/parity, same-wave consumer, row floor, and no-x86 proof.
8. Verify delete/retire actions have replacement proof no later than the
   delete/retire wave.

## Section 3 - Challenge Triggers

Run CH1-CH7 before redress when the wave is first-of-class,
substrate-touching, native SIMD-bearing, generated-output-bearing, CSS
admission-bearing, or changes gate close semantics. CH7 overfit-prune is
binding and includes gate-exclusion and broadcast-detection checks.

## Section 4 - Per-Wave Envelopes

### W0 - Baseline And SK-V16 Report Consumers

Research scopes: RESULTS schema, report flags, JSON 51 guard, CSS 24 open
rows, existing xtask gate parser.

Plan must name report consumers and negative fixtures. Redress closes only
when gate consumption is proven and no behavior drift occurs.

### W1 - Dirty Generated Disposition

Research scopes: seven dirty CSS generated files, `generated_real_typed.rs`,
regen/check commands.

Plan must choose clean regen, retirement, or intrinsic block per file.
Redress closes only with exact manifest, broad command result, owner, and
disposition consumed by the gate.

### W2 - Lock 14/16 Scan Expansion

Research scopes: omitted Lock 14 roots, codegen, xtask, runtime generator,
report/gate roots, primitive/checkasm manifests.

Plan must name included roots, excluded roots, reason, owner, self-scan
status, primitive status, gate consumer, affected rows, and disposition.

### W3 - CSS Legacy Proof Quarantine

Research scopes: CSS report/gate surfaces, `CSS_GENERATED_RS`, fact streams,
`CssFullParseSummary`, `parse_full`, W8R broadcast evidence.

Plan must make these paths diagnostic-only without deleting replacement
providers too early.

### W4 - CSS Grammar-Derived Provider

Research scopes: `grammar/css/l4`, CSS provider/generator, generated CSS
outputs, Lock 14 scan.

Plan must prove grammar-derived provider source and forbid CSS admission.

### W5 - CSS Typed API And Equality

Research scopes: CSS typed document/value/view/visitor surfaces and cssparser
typed summary comparator.

Plan must prove typed equality before any speed measurement counts.

### W6 - CSS Typed SOTA

Research scopes: CSS speed retime on the same typed workload.

Plan must name all 24 CSS row thresholds as `cssparser_typed_mbps + 1.000`.
Redress admits only if equality remains true and JSON 51 guard rows maintain.

### W7 - Pattern H Census And Roundtrip Gate

Research scopes: 67 runtime files, generator provenance, roundtrip report.

Plan must reject header-only proof and use the correct `-mindepth 2` census.

### W8 - Pattern H Generator-Owned Collapse

Research scopes: root runtime generator/template and generated output.

Plan must pair any destructive replacement with same-wave generator restoration.

### W9 - Decision/BackendShape Preservation

Research scopes: Decision Engine, e-graph/CSP tests, all five BackendShape
lowerers, anti-sidecar scans.

Plan must prove no sixth shape and no label-string lowerer.

### W10 - Conditional Primitive/Tape/Native Consumer

Research scopes: exact selected S-P2 survivor only.

Plan must name one candidate, its scalar oracle, checkasm/parity if native,
same-wave consumer, P3-C row floor, full JSON maintain budget, and pre-block
non-reopen proof. If no legal candidate exists, W10 records not-scoped.

### W11 - Close Reconciliation

Research scopes: all wave dispositions, RESULTS, REDRESS, HANDOFF, close
audit evidence.

Plan is docs-only. Redress closes only when PASS-IMPL V3 accepts each axis or
records intrinsic-block proof.

## Section 5 - Mandatory Redress Evidence

- Commands run and important output.
- Named corpus rows and thresholds.
- JSON 51 guard status for behavior waves.
- Lock 14/generality gate for generic/generated/primitive edits.
- Dirty/generated manifest when generated output is touched.
- Same-wave consumer proof for every primitive/kernel/generated path.
- Revert patch path or committed admit evidence.
- Disposition: ADMIT, REJECT, ROUTE, or intrinsic BLOCK.

Forbidden close language: "wired", "integrated", "future consumer",
"diagnostic proof", "broadcast", "same-plane by construction", "FNV proves",
"dirty generated accepted", "x86 equivalent", or "CSS SOTA" unless the wave's
executable gate consumed exact evidence.
