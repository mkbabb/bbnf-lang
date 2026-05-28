# SK-V15 W1-A: Results Manifest Broadcast State

Date: 2026-05-28.
Scope: `skinny/RESULTS.md` and the SK-V15 W0 telemetry manifest.
Output: this file.

## Findings

- Current `skinny/RESULTS.md` does not present the 24 CSS L4 rows as live
  W8R admissions. The CSS rows in the SK-V15 W0 telemetry manifest carry
  `not_admitted:SK-V15-W0-broadcast-diagnostic`, `AUDIT-FALSIFIED`,
  `pending:SK-V15-W1-CSS-BROADCAST`, `full_parse_summary`, shared
  `measurement_row_id=SK-V14-W8R-css-full-parse-profile-cold-8`, and shared
  `broadcast_group_id=SK-V14-W8R-css-l4-full-parse` in
  `skinny/RESULTS.md:112` through `skinny/RESULTS.md:135`.
- The current manifest validator rejects CSS rows that use `AUDIT-SUSTAINED`,
  `PASS:*`, or `admitted:*` live-admit markers on W8R CSS rows at
  `skinny/xtask/src/skv15_w0.rs:500`, and requires non-admission, W1 routing,
  shared W8R measurement id, diagnostic origin, `full_parse_summary`,
  comparator mismatch disclosure, `CSS_GENERATED_RS` disclosure, incomplete
  Lock 14 disclosure, cssparser diagnostic parity, non-admission gate
  exclusion, and the shared broadcast group at
  `skinny/xtask/src/skv15_w0.rs:509`.
- W1 should collapse or hard-demote the diagnostic shape rather than rely on
  W0 demotion alone. SK-V15 close permits either one diagnostic aggregate or
  independently measured typed-output rows, while W6 owns fresh typed retiming
  (`restart/skinny/tranches/sk-v15/SPEC.md:54`,
  `restart/skinny/tranches/sk-v15/SPEC.md:131`,
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:135`).

## Recommendations

- Preserve the W0 CSS diagnostic predicates if feature rows remain: shared
  W8R measurement id, diagnostic origin, `full_parse_summary`, comparator
  mismatch, `CSS_GENERATED_RS` disclosure, cssparser diagnostic parity,
  non-admission gate exclusion, and shared broadcast group.
- Prefer W1 collapse or gate-level hard demotion over additional producer-only
  prose. A retained 24-row diagnostic shape is legal but keeps the old feature
  admission topology available for accidental reinterpretation.

## Risks

- W1 must not delete or retire CSS providers before W5/W6-grade typed proof
  lands in the same wave (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:135`).
- If W1 keeps the 24 feature rows in RESULTS, the gate must prove each row is
  diagnostic and visibly grouped; if it collapses them, the row-universe checks
  must change deliberately.

## Sources

- `skinny/RESULTS.md:112`
- `skinny/RESULTS.md:135`
- `skinny/xtask/src/skv15_w0.rs:500`
- `skinny/xtask/src/skv15_w0.rs:509`
- `restart/skinny/tranches/sk-v15/SPEC.md:54`
- `restart/skinny/tranches/sk-v15/SPEC.md:131`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:135`
