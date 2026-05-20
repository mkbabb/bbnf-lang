# SK-V12 W1 A1 - CSS L4 Preflight

Scope: read-only preflight of the first ordered W1 target,
CSS L4 declaration values, against SPEC Section 4.

## Conclusion

CSS L4 declaration values remain the first ordered W1 target, but the
current owner surface cannot admit them as the W1 generated baseline without
first crossing a JSON-profiled codegen/runtime blocker. That is a concrete
plan-time failure for CSS inside the W1 owner surface.

If CSS were selectable, the row would be:

- row: `css_l4/declaration_values/direct_to_struct/main`
- output plane: `direct_sink`
- runtime module: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`
- same-wave consumer: non-JSON baseline bench plus SK-V12 companion gate

## Evidence

The codegen entrypoints still call the JSON runtime profile guard before
emitting direct or typed generated output:

- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/lib.rs`

The direct renderer is JSON-shaped: it imports `JsonSink` and emits JSON
object, array, string, number, boolean, and null dispatch. The typed renderer
is also JSON-object-shaped. The current runtime exports generated JSON plus a
proof/test-only `sheets_witness`; it has no generated CSS runtime module.

The root CSS grammar is not a small direct import for skinny W1. CSS L4
sources use imports and CSS-specific runtime hooks, including color/value
helpers outside the selected W1 owner surface. A generated W1 baseline would
need a real CSS provider branch and a generated declaration-values runtime,
not a report fixture or JSON provider clone under a neutral name.

## Pre-Blocks

The following CSS routes are not admissible for W1:

- REDRESS 111 report fixture as a baseline.
- REDRESS 112/113 future-phase promise.
- Hand-only CSS parser.
- JSON provider cloning under a neutral grammar name.
- Source-only CSS grammar claims without measured Mbps and strict equality.

## Recommendation

Record this CSS preflight failure in the W1 plan. If the plan skips CSS, it
must cite the JSON-profiled codegen/runtime blocker and the absence of a
generated CSS direct-sink module as the concrete failure.
