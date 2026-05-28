# SK-V15 W2 CHALLENGE V2

Input plan: `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-plan.md`
at commit `d612eeb6c`.

## Verdict

ACCEPT 7/7. W2 may enter redress.

| Lens | Verdict | Acceptance reason |
|---|---|---|
| CH1 correctness | ACCEPT | Primitive status is source-inventory validated across `aarch64/mod.rs`, dispatch `PrimitiveKernels`, public `prim` wrappers, and all native-token hits. |
| CH2 generality | ACCEPT | `non_json_receiver`, `proof_command`, `generated_output_expectation`, and receiver bindings are now load-bearing report fields. |
| CH3 regression | ACCEPT | Legacy W0 telemetry remains compatible, capture path is verified, companion paths are covered, and lock-only mode is constrained to avoid full report regeneration. |
| CH4 cost | ACCEPT | Scope remains report/schema/gate-consumer only, with W3/W6/W7 implementation work routed instead of absorbed into W2. |
| CH5 hidden coupling | ACCEPT | CostFacts / Decision findings route to `DEP-W7-DECISION-SPINE`; `--with-cost-facts` is not a lock-gate bypass; x86 is non-admission diagnostic-only. |
| CH6 anti-paper-close | ACCEPT | Gate consumption is executable: `gate-json --check-results` must consume W0/W1 results, rolling delta, and W2 lock coverage. |
| CH7 overfit-prune / gate-exclusion | ACCEPT | Detached report text is insufficient; W2 must derive or validate coverage from source inventory and reject row-count, source-path, or self-scan mismatches. |

## Redress Guardrails

- Keep implementation inside W2 owner paths and the 120-280 manual LOC budget.
- Implement lock-only gate consumption centrally; do not rewrite every legacy
  companion path unless the central guard cannot close.
- Do not delete provider/template/CSS generated surfaces or Decision Engine
  scaffold in W2.
- Preserve unrelated dirty runtime/generated/root files.
