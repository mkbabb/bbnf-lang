# SK-V15 W2 CHALLENGE V1

Input plan: `restart/skinny/tranches/sk-v15/research/w2/skv15-W2-plan.md`
at commit `0559ea62c`.

## Verdict

REVISE. Do not enter W2 redress from the V1 plan.

## Lens Results

| Lens | Verdict | Reason |
|---|---|---|
| CH1 correctness | REVISE | Primitive manifest can be underfit if built only from intrinsic-token matches; it must cover `aarch64/mod.rs`, dispatch `PrimitiveKernels`, public `prim` wrappers, and all `core::arch` / `target_feature` / `asm!` hits. |
| CH2 generality | REVISE | The V1 report schema lacks load-bearing `non_json_receiver`, `proof_command`, and `generated_output_expectation` fields from SPEC Section 2.3. |
| CH3 regression | REVISE | V1 can reject current W0 CSS telemetry containing `diagnostic:pre-W2-incomplete`, and it lacks result-capture / companion compatibility commands. |
| CH4 cost | ACCEPT | The plan stays inside the W2 gate/report envelope if it uses a lock-gates-only path and does not force full bench regeneration. |
| CH5 hidden coupling | REVISE | CostFacts / Decision coupling is under-specified. `--with-cost-facts` must not bypass W2 lock-gate consumption, and CostFacts grammar-named findings route to `DEP-W7-DECISION-SPINE`. |
| CH6 anti-paper-close | ACCEPT | V1 requires produced coverage to be consumed by `gate-json --check-results`, not just filed as docs. |
| CH7 overfit-prune / gate-exclusion | REVISE | The plan must forbid self-referential reports by requiring the executable gate to construct or validate the W2 report from source inventory, not accept a detached declaration. |

## Required Revisions

1. Promote W2-E's W2-specific fields into the required report schema:
   `source_path`, `finding_kind`, `strict_command`, `scalar_reference`,
   `rollback_or_redress`, `dependency_row`, `non_json_receiver`,
   `proof_command`, `generated_output_expectation`, `json_guard_command`, and
   `fail_action`.
2. Add the SPEC Section 2.3 receiver table per touched generic/gate owner path.
3. Require the Lock 16 primitive manifest to be validated from source inventory
   covering `aarch64/mod.rs`, dispatch `PrimitiveKernels`, public `prim`
   wrappers, and all native token hits; reject row-count or source-path
   mismatch.
4. Broaden Lock 14 forbidden-token findings per SPEC Section 2.3:
   `Json`, `CssL4`, Sheets/corpus names, JSON structural roles, CSS profile
   names, `json_`, `css_`, `RuntimeProvider`, aliases, `static_css_provider_status`,
   `json_sink_only_status`, and `JSON-CSS`.
5. Clarify that `diagnostic:pre-W2-incomplete` rejection applies to the W2
   coverage report, not the legacy W0 manifest until a same-wave capture update
   is explicitly performed and verified.
6. Add a named lock-only bench-gate mode and tests proving it performs no
   result update, Criterion read, full report render, or stale rewrite.
7. Add verification for `--skv14-existing-results-capture`, at least one
   legacy companion fail-closed test, one companion `--check-results` pass
   test, and `--with-cost-facts` non-bypass behavior.
8. Route CostFacts / Decision grammar-named findings to
   `DEP-W7-DECISION-SPINE`, not W3/W6 provider/template rows.
9. Mark `diagnostic-x86` as non-admission-only.

## Disposition

Amend the plan to V2, then rerun the seven-lens CHALLENGE before any W2
implementation edit.
