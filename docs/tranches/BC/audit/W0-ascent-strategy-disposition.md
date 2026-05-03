# BC.W0c — AscentStrategy Disposition

Date: 2026-05-03
Status: settled. Per surgery 29 (HARDENING-PLAN-SYNTHESIS-2026-05-03.md:69) and D08-5 (`audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:14`).

## §1 Context

`AscentStrategy` is a residual from the pre-restart era; it carried a single-pass left-recursion ascent through a non-Pratt path, deferred from BA.W6 carry tags. Per BB.W3 the Pratt + left-recursion concerns subsume into the cost-model-driven `PrattSpine` IR variant per `audit/W0-typed-ir-variant-table.md:§2 (PrattSpine)`. AscentStrategy's residual is a name and a stub-call site without a coherent role.

## §2 Decision

**Delete `AscentStrategy` at BC.W0c.** Per `feedback_no_workarounds` zero tolerance for legacy code. Per `feedback_pluggable_components`, the cost-model-driven Pratt classification is the singular dispatch point for left-recursion; AscentStrategy as a separate substrate is an orthogonal codepath per `feedback_no_orthogonal_codepaths`.

## §3 Migration

| Site | Action |
|---|---|
| `crates/ir/src/passes/types/mod.rs` (or wherever `AscentStrategy` enum lives post BA.W2 split) | Delete the enum and all its variants |
| `crates/ir/src/passes/types/ascent_strategy.rs` (if separate file) | Delete the file |
| `crates/core/src/codegen/rust/ascent.rs` (legacy emit site) | Delete the file; left-recursion dispatch flows through the `PrattSpine` IR variant emit per `audit/W0-typed-ir-variant-table.md` |
| `crates/core/src/codegen/rust/shapes/ascent_dispatcher.rs` | Delete the file; merge any unique logic into `bbnf-codegen::optimiser::pratt_detect` |
| Tests referencing `AscentStrategy` | Delete the tests; the PrattSpine round-trip test (BC.W0a §2.8 fixture) covers the same surface |

## §4 Verification

```
rg -n 'AscentStrategy' crates/ tests/ docs/codegen-IR-CONTRACT.md returns zero
```

Closer-gate condition for BC.W0c.

## §5 Cross-references

| Reference | Description |
|---|---|
| `audit/HARDENING-SYNTHESIS-2026-05-03.md:227` | Ground-truth carry table item naming AscentStrategy disposition |
| `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:14` (D08-5) | The carry-deferral audit caught the missing receiving gate; this document is the receiving gate |
| `feedback_no_workarounds` | Zero tolerance for legacy code |
| `feedback_no_orthogonal_codepaths` | Singular dispatch path; PrattSpine subsumes AscentStrategy |
| `feedback_pluggable_components` | Cost-model is the pluggable decision point, not separate strategy enums |
