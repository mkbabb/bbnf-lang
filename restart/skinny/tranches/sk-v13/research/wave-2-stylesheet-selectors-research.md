# SK-V13 W2 Research - CSS Stylesheet Root + Selectors

Wave: W2. Phase: Research. Date: 2026-05-21.

## Scope

W2 is dispatchable because W0 and W1 are admitted. SPEC Section 5 owns CSS
codegen/runtime families, CSS bench/oracle fixtures, `RESULTS.md`, and
`REDRESS.md` only on reject.

## Findings

1. The current non-JSON runtime surface has one generated CSS module:
   `runtime::generated_css_l4_declaration_values`. It is exposed from
   `skinny/crates/runtime/src/lib.rs` and backed by codegen templates under
   `skinny/crates/codegen/src/css_l4_declaration_values_templates/`.

2. Codegen has an explicit runtime-profile provider for the declaration-values
   module. A new generated stylesheet/selectors row should mirror that pattern:
   provider, templates, runtime module, and reproducibility test.

3. Lock 14 currently knows only the declaration-values CSS runtime/template
   paths. A W2 generated module will fail the generic-crate proof unless the
   Lock 14 owner inventory is extended to include the new CSS profile paths.

4. The W1 coverage matrix and `ROLLING-SOTA-DELTA.md` enumerate 24 feature
   rows. W2 can move stylesheet/selector features only if gate-json learns to
   accept measured non-declaration CSS rows; the W1 state intentionally kept
   those rows `OPEN-ABSENT`.

5. A small W2 fixture can exercise the required selector families without
   relying on lightningcss recovery mode:
   type selectors, classes, ids, child/descendant/adjacent/sibling combinators,
   attributes, pseudo-classes, and pseudo-elements.

6. The independent oracle can be a golden fact table for the named fixture,
   with lightningcss used as the strict same-plane SOTA anchor after parsing
   the same fixture successfully. This matches the SK-V12 sidecar shape while
   keeping W2 facts explicit and fixture-bound.

## Selected Research Conclusion

W2 should land one generated runtime row for
`css_l4/stylesheet_and_selectors/direct_to_struct/main`, plus feature-row
rolling movement for the selector/root subset. The implementation must include
the generated runtime profile, fixture, oracle/golden facts, lightningcss
same-plane benchmark, report/gate evidence, Lock 14 owner-path updates, and
rolling-delta consumption in the same redress commit.
