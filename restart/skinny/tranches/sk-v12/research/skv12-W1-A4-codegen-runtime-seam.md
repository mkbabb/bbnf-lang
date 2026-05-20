# SK-V12 W1 A4 - Codegen/Runtime Seam

Scope: read-only audit of the owner seam W1 must cross to admit a generated
non-JSON baseline.

## Conclusion

The W1 blocker is the runtime emission boundary, not the absence of a BIR
shape. Skinny codegen currently has a JSON runtime profile guard and
JSON-specific renderer stack. Removing the guard alone would be incorrect:
it would route non-JSON grammars through JSON templates and create a disguised
JSON clone.

## Evidence

The direct generation path calls the JSON profile guard before emitting
runtime support, generated output, and the direct sink renderer. The typed
generation path does the same. The JSON provider supplies JSON runtime files,
templates, and module text. The direct renderer is coupled to `JsonSink` and
JSON value dispatch; the typed renderer is coupled to JSON object/key/value
semantics.

The runtime library exports generated JSON as the only production generated
grammar. `sheets_witness` is proof/test-only and cannot satisfy W1 Track 1.

The regen/check path is JSON-only today, and workspace metadata names only
JSON grammar generation in the skinny workspace.

## Minimum Legal Shape

A legal W1 implementation needs a selected non-JSON provider branch for one
selected grammar, with:

- generated runtime module files under `skinny/crates/runtime/src/grammars/`;
- a direct/sink renderer or template specific to the selected non-JSON row;
- no generic JSON policy leakage;
- no directive, BIR, or backend-shape additions;
- a bench/oracle/gate consumer in the same redress.

## Non-Admissible Route

Deleting `json_provider::ensure_runtime_profile` and reusing JSON templates
for a non-JSON grammar is not admissible. It would create the exact
JSON-provider-clone route blocked by SPEC Section 4 and P3-E.
