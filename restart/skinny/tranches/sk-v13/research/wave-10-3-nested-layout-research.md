# SK-V13 W10.3 Research - CSS Nested Layout Pack

Wave: W10.3. Phase: Research. Date: 2026-05-22.

## Scope

The remaining open CSS parity rows in `restart/skinny/ROLLING-SOTA-DELTA.md`
are:

- `nested_rules`
- `logical_properties`
- `grid`
- `flexbox`
- `typed_property_groups`

These rows share one feature shape: stylesheet/rule/declaration facts whose
semantics are CSS-specific and whose comparator is lightningcss typed parsing
plus a same-plane golden fact stream. None requires a generic grammar,
substrate, SIMD, JSON, directive, BIR, `BackendShape`, or public API change.

## Candidate Row

Use one grouped generated row:

```text
css_l4/nested_layout/direct_to_struct/main
```

The row's `covered_feature_rows` must be exactly:

```text
nested_rules
logical_properties
grid
flexbox
typed_property_groups
```

This preserves the rolling-delta row contract while avoiding five support-only
rows with identical fixture/oracle/gate mechanics.

## Fixture Shape

The fixture should include:

- one nested rule under a qualified rule;
- grid declarations: `display:grid`, `grid-template-columns`, `gap`;
- flex declarations: `display:flex`, `flex-direction`, `align-items`,
  `justify-content`;
- logical declarations: `margin-inline-start`, `inline-size`,
  `padding-block`, `border-inline-start`;
- typed property groups: color, length, display/layout, font-ish, and border
  facts derived from property/value classes.

The row may use source-sidecar facts for strict equality, but the lightningcss
sidecar must parse the stylesheet with error recovery disabled and prove the
expected rule/declaration families are present. The independent oracle is a
hand-checked golden table plus the same lightningcss typed-AST assertions.

## Material Differential

This is not a re-run of REDRESS 112/113/123-127. Those entries admitted or
rejected narrower comparator/gate surfaces. W10.3 adds a generated runtime
profile and gate-consumed row for the last ungenerated CSS feature families.
It is also not a parser-owned substrate or SIMD route: Lock 16 is `n/a`, and
the same-wave consumer is the generated CSS fact-stream caller.

## Owner Paths

Source redress should be restricted to:

- CSS-specific codegen/runtime generated profile paths for
  `css_l4_nested_layout`;
- CSS non-JSON bench/oracle/gate/report plumbing;
- Lock 14 owner inventory for the new profile paths;
- `xtask` gate passthrough;
- W10.3 fixture/report/artifacts;
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, and
  `restart/skinny/ROLLING-SOTA-DELTA.md`.

No generic grammar branch, JSON path, SIMD crate, x86 path, new directive, BIR
variant, `BackendShape`, or public substrate API is in scope.
