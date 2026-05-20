# SK-V11 W3-R6: Grammar-Neutral Numeric Compatibility

Date: 2026-05-20.
Scope: grammar-neutral compatibility and W2 blocked-route impact.
Output: this file.

## §1 — Findings

- W1a is only a gate/report lane and must not create parser behavior, generated
  baseline authority, row movement, or non-JSON admission
  (`restart/skinny/tranches/sk-v11/research/w1a/w1a-plan-gate-matrix.md:23`,
  `skinny/REDRESS.md:3284`).
- W1b selected CSS L4 direct baseline but rejected because skinny codegen and
  runtime only support generated JSON. `emit_with_layout` and
  `emit_typed_with_layout` call `json_provider::ensure_runtime_profile`, which
  accepts only `backend.grammar_name == "json"`
  (`skinny/crates/codegen/src/lib.rs:102`,
  `skinny/crates/codegen/src/lib.rs:139`,
  `skinny/crates/codegen/src/json_provider.rs:4`).
- Runtime exports generated JSON and proof-gated `sheets_witness`, not
  generated CSS L4 (`skinny/crates/runtime/src/lib.rs:3`,
  `skinny/crates/runtime/src/lib.rs:10`).
- W2 is blocked because no `W1b_css_baseline_mbps` exists; W3-W8 may continue
  only as direct-plane closure/fixpoint waves with W2's non-JSON axis explicitly
  blocked (`restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md:20`,
  `skinny/REDRESS.md:3352`).
- W3 is numeric-only: scalar digit span/accumulation, optional UDOT, generated
  JSON numeric consumers, selected direct rows, and no number grammar/policy
  changes (`SPEC.md:434`, `SPEC.md:459`, `SPEC.md:466`, `SPEC.md:474`).
- CSS and Sheets numeric compatibility is real: both admit leading-dot numeric
  forms that JSON-strict scanning rejects (`grammar/css/l4/value-unit.bbnf:9`,
  `crates/core/tests/css_l4_dimensions.rs:141`,
  `grammar/google-sheets/google-sheets.bbnf:6`,
  `crates/core/tests/sheets_expr_parity.rs:298`).
- Core lowering preserves the dialect split through `allow_leading_dot`
  (`crates/ir/src/types/fn_descriptor.rs:28`,
  `crates/core/src/lower/expression/wrap.rs:697`,
  `crates/core/src/backend/rust/emitter/shapes/mod.rs:244`).

## §2 — Recommendations

- Proceed with W3 only if CHALLENGE records that W2 BLOCKED is accepted as the
  W2 disposition for direct-plane-only dispatch; the non-JSON axis remains
  blocked.
- Select one or two numeric rows by default; require extra microbench evidence
  before selecting more.
- Keep the primitive grammar-neutral by accepting span/metadata inputs and
  preserving generated grammar policy outside the primitive.
- If parse-that or codegen generic behavior changes, carry same-wave CSS/Sheets
  proof: CSS leading-dot/dimension tests, Sheets number/formula parity, and
  evidence that `allow_leading_dot` routing remains intact.

## §3 — Risks

- W3 cannot claim W2 admitted the non-JSON intervention axis.
- No numeric fallback/mantissa widening, f64 policy rewrite, or primitive-owned
  number grammar may land (`SPEC.md:793`).
- Generic/codegen/runtime edits require same-wave CSS L4, Sheets, or BBNF-self
  proof (`SPEC.md:177`, `SPEC.md:232`).
- The skinny number scanner is JSON-shaped and rejects leading-dot forms; using
  it as generic policy would break CSS/Sheets compatibility
  (`skinny/crates/parse-that-regex/src/number/mod.rs:51`).

## §4 — Sources

- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v11/research/w1a/`
- `restart/skinny/tranches/sk-v11/research/w1b/`
- `restart/skinny/tranches/sk-v11/research/w2/entry/w2-entry-blocked.md`
- `skinny/crates/codegen/src/`
- `skinny/crates/runtime/src/`
- `skinny/crates/parse-that-regex/src/number/`
- `grammar/css/l4/`
- `grammar/google-sheets/google-sheets.bbnf`
- `crates/core/tests/`
