# SK-V11 S-P3 V3 CH2: Generality / Lock 14

Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Lens: CH2 GENERALITY / Lock 14.
Date: 2026-05-20.
Output: this file.
Scope: evaluate whether the V3 packet keeps generic crates/codegen free of
JSON-only policy, makes CSS/Sheets/BBNF-self proof measured rather than
asserted, and sequences W1a/W1b/W2 so non-JSON floors are measurable before
later behavior admits.
Disposition: ACCEPT.

## Verdict

ACCEPT.

V3 satisfies CH2. The S-P3 lens requires every shortlisted candidate to carry
the S-P2 grammar-neutral verdict, and requires SPEC §2.1/§2.2 to fail any wave
that lets JSON policy enter a generic crate or claims generic/codegen behavior
without a same-wave CSS L4 / Sheets / BBNF-self generated parser proof
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:116-120`). S-P2 convergence
makes that non-negotiable for this packet: non-JSON generality must be measured
through a generated direct/typed parser, not prose or JSON-only telemetry
(`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:23-32`).

The V3 packet carries that requirement into the SPEC, dispatch prompt, P3-B
sequence, P3-C gates, P3-D schema binding, and P3-E pre-blocks. I found no
blocking CH2 issue.

## Checks

| Check | Assessment | Evidence |
|---|---|---|
| Generic crates/codegen do not get a JSON-policy carveout | ACCEPT | SPEC §1 forbids JSON policy in generic crates or runtime outside generated per-grammar modules and requires every generic/codegen/runtime-outside-JSON edit to carry a same-wave CSS L4, Sheets, or BBNF-self proof (`restart/skinny/tranches/sk-v11/SPEC.md:163-183`). SPEC §2.2 then makes this an every-wave exit gate: no generic branch may select JSON/corpus/object/array/field/layout roles, grammar facts must be generated metadata, the non-JSON generated parser proof must run and be consumed in the same wave when generic behavior changes, and the live `json_provider` path must be replaced, bypassed with grammar-neutral proof, or explicitly left untouched before a non-JSON generality claim can pass (`restart/skinny/tranches/sk-v11/SPEC.md:229-245`). P3-E also hard-blocks generic JSON policy in `parse-that-regex`, `bbnf-simd`, IR, codegen, or runtime outside generated grammar-local code (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:214-222`). |
| The known `json_provider` risk is measured, not hand-waved | ACCEPT | P2-F identifies the current codegen path as not yet Lock-14 clean because normal and typed emission still call `json_provider::ensure_runtime_profile`; it allows that only as inventory evidence, not a generalization endpoint (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:10-17`, `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:31-35`, `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:111-113`). V3 folds that risk into W1b and W2: W1b must prove the live `json_provider` path does not leak JSON policy into the selected generated parser, and its exit gate rejects JSON policy in generic crates/runtime outside generated per-grammar modules (`restart/skinny/tranches/sk-v11/SPEC.md:349-367`). W2 repeats the same rejection boundary for the actual intervention (`restart/skinny/tranches/sk-v11/SPEC.md:402-425`). |
| CSS/Sheets/BBNF-self proof is executable and measured | ACCEPT | P2-F requires generated Track 1 before/after throughput, independent Track 2/oracle throughput, strict output equality, primitive self-time, parity when applicable, fallback behavior, no sidecar allocation, no generic-crate grammar names, and a same-wave generated non-JSON consumer (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:86-91`). P3-D binds `css_l4`, `sheets`, and `bbnf_self` into existing telemetry fields and rejects non-JSON admission unless grammar id, workload, comparator/oracle, Track 1, Track 2/oracle, profile artifact, strict output proof, and same-wave consumer are gate-consumed (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:154-172`, `restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:257-261`). P3-C rejects any non-JSON claim that lacks a generated row, independent oracle/Track 2, before/after Mbps, and gate-consumed grammar id (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:50-57`). |
| W1a is only the gate/report lane | ACCEPT | W1a is scoped to C9 accounting plus Lock 14 gate/report infrastructure with no parser row movement (`restart/skinny/tranches/sk-v11/SPEC.md:283-286`). Its tasks add failing and passing fixtures for grammar id, domain, output plane, comparator/oracle, Track 2/oracle independence, run id, host, feature mask, same-wave consumer class, and producer-only telemetry rejection (`restart/skinny/tranches/sk-v11/SPEC.md:299-307`). Its exit gate rejects missing required non-JSON fields and producer-only non-JSON telemetry, keeps JSON `gate-json --with-cost-facts --check-results` green, moves no JSON row, and claims no generated non-JSON baseline authority (`restart/skinny/tranches/sk-v11/SPEC.md:308-315`). |
| W1b creates baseline authority before intervention | ACCEPT | W1b must select exactly one non-JSON target, preferring CSS L4 declaration values, then Sheets, then BBNF-self, and name the independent oracle/Track 2 path (`restart/skinny/tranches/sk-v11/SPEC.md:345-347`). It stands up exactly one generated non-JSON direct or typed parser baseline row, proves strict output equality and gate consumption, and proves `json_provider` does not leak JSON policy into that selected generated parser (`restart/skinny/tranches/sk-v11/SPEC.md:349-356`). Its exit gate requires generated Track 1 baseline, independent oracle/Track 2 that does not call Track 1, strict output equality, baseline throughput with run id/host/flags/sample count/output plane/oracle status, no JSON policy leak, and no behavior row admission (`restart/skinny/tranches/sk-v11/SPEC.md:357-367`). |
| W2 consumes W1b and cannot invent the first measurable non-JSON row | ACCEPT | W2 entry names the generated non-JSON direct/typed intervention, scalar oracle, independent Track 2/oracle, baseline Mbps, target threshold, and Lock 14 proof (`restart/skinny/tranches/sk-v11/SPEC.md:397-400`). Its tasks explicitly consume the W1b baseline and forbid W2 from creating the first measurable non-JSON row (`restart/skinny/tranches/sk-v11/SPEC.md:402-409`). Its exit gate requires generated non-JSON Track 1 plus independent Track 2/oracle, strict output equality, Track 1 at least `ceil(W1b_css_baseline_mbps * 1.01)`, strict scalar differential/checkasm for SIMD, JSON guard preservation if refreshed, and no JSON policy in generic crates/runtime outside generated per-grammar code (`restart/skinny/tranches/sk-v11/SPEC.md:411-425`). |
| Later JSON behavior waves cannot bypass Lock 14 | ACCEPT | The global SPEC §2.2 every-wave exit gate applies to later waves when generic/codegen/runtime-outside-JSON behavior changes (`restart/skinny/tranches/sk-v11/SPEC.md:229-245`). W3 reverts on non-JSON proof miss and preserves CSS/Sheets numeric compatibility if generic parse-that/codegen changes are made (`restart/skinny/tranches/sk-v11/SPEC.md:456-479`). W5 requires non-JSON string/literal proof when generic code changes (`restart/skinny/tranches/sk-v11/SPEC.md:569-581`). W6 keeps CSS variable-width escapes and BBNF literal policy per-grammar and rejects JSON surrogate policy leaks into generic crates (`restart/skinny/tranches/sk-v11/SPEC.md:615-638`). W7 rejects digest/hash state entering generic parser crates as parser semantics (`restart/skinny/tranches/sk-v11/SPEC.md:672-685`). |
| W1a/W1b/W2 sequencing makes non-JSON floors measurable before later behavior admit | ACCEPT | P3-B orders W1a before W1b because non-JSON telemetry must be gate-consumed before baseline authority, W1b before W2 because an independent baseline/oracle must exist before an intervention can claim improvement, and W2 before generic C1-C7 behavior waves because SK-V11 requires exercised non-JSON generality, not prose (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:78-86`). The dispatch prompt repeats that order: W1a gate-consumption first, W1b exactly one generated non-JSON baseline plus independent oracle, W2 the preferred non-JSON admitted intervention consuming that baseline, then W3-W7 row-moving/proof waves (`restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`). SPEC dispatch scope also sends W1a first, then W1b to make grammar generalization measurable, with W2-W8 conditional on their entry gates (`restart/skinny/tranches/sk-v11/SPEC.md:804-817`). |
| Close cannot paper over a missing non-JSON intervention | ACCEPT | SPEC close condition requires at least one admitted, benchmarked non-JSON intervention, with CSS L4 preferred, then Sheets, then BBNF-self (`restart/skinny/tranches/sk-v11/SPEC.md:31-47`). W8 entry allows only an admitted non-JSON axis or a recorded BLOCKED route, and W9 requires at least one non-JSON generated direct/typed parser intervention admitted and benchmarked unless close escalates `BLOCKED` for grammar-generalization fixpoint (`restart/skinny/tranches/sk-v11/SPEC.md:709-729`, `restart/skinny/tranches/sk-v11/SPEC.md:752-765`). P3-E gives the same W9 rule: close requires direct rows admitted/proven uncloseable and at least one non-JSON generated parser intervention admitted (`restart/skinny/tranches/sk-v11/research/p3/p3e-preblocked-ledger.md:202-212`). |

## Residual Watch Items

No V3 REVISE is required.

1. W4 says W2's non-JSON proof remains valid for generic edits
   (`restart/skinny/tranches/sk-v11/SPEC.md:506-509`). This is acceptable only
   because SPEC §2.2 independently requires a same-wave CSS/Sheets/BBNF-self
   proof when W4 or any later wave changes generic behavior. Wave CH2 reviewers
   should reject any W4 plan that treats W2's prior proof as a reusable substitute
   for same-wave proof.
2. If W1b selects Sheets or BBNF-self instead of CSS, the same concrete fields
   must be named before redress: generated Track 1 path, independent oracle or
   Track 2 source path, output plane, strict semantic equality command, baseline
   Mbps, W2 improvement threshold or measured fallback disposition, gate command,
   and same-wave generated direct/typed consumer. V3 has the hooks through W1b
   selection, P3-D allowed values, P3-C unmeasurable-gate rejection, and SPEC
   §2.2; the wave CHALLENGE must enforce them.

No source edits were made.
