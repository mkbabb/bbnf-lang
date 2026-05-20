# SK-V12 W1 A2 - Sheets Preflight

Scope: read-only preflight of the second ordered W1 target, Sheets formula,
after the CSS preflight failure.

## Conclusion

Sheets is the plausible W1 fallback. It is not a shortcut: the W1 plan may
select Sheets only after recording the concrete CSS failure. The selected row
should be a generated direct/sink baseline, not a typed baseline:

- row: `sheets/formula/direct_to_struct/main`
- grammar: `sheets`
- workload: `direct_to_struct`
- output plane: `direct_sink`

Typed Sheets is a poor W1 first target because the current skinny typed
renderer is still JSON-object-shaped. A direct/sink formula digest or event
plane can be kept smaller if generated from grammar facts.

## Evidence

Sheets shares the same JSON-profiled codegen/runtime blocker as CSS:

- `skinny/crates/codegen/src/json_provider.rs` admits only `json`.
- `skinny/crates/codegen/src/lib.rs` calls the JSON guard before direct and
  typed generation.
- `skinny/crates/codegen/src/sink_direct.rs` emits JSON direct parsing.
- `skinny/crates/runtime/src/lib.rs` exposes generated JSON and proof/test
  witnesses, not a generated Sheets parser module.

The existing `sheets_witness` module is not admissible as Track 1 or oracle
evidence. It is a proof/test inventory, not a parser, output plane, equality
path, or throughput path. The SK-V12 companion gate also rejects stale
`sheets_witness` provenance.

The full repository has a compact Sheets parser/runtime shape that can inform
the W1 generated baseline design, but W1 ownership remains inside skinny. Any
baseline must therefore land in skinny codegen/runtime/bench paths and be
measured by the W1 companion gate.

## Required W1 Surface

The Sheets route needs all of the following in one selected redress:

- Break the selected non-JSON provider path without weakening JSON policy.
- Add a generated Sheets runtime module under
  `skinny/crates/runtime/src/grammars/`.
- Add Sheets fixtures and an independent same-plane oracle/Track 2.
- Add a Criterion row with sample count at least 30.
- Produce a `sk-v12-nonjson-generated-v1` report with `grammar_id=sheets`,
  `output_plane=direct_sink`, Track 1 Mbps >= 1, oracle Mbps >= 1, strict
  equality pass, and generated provenance consumed by `gate-json`.

## Recommendation

Select Sheets only if the W1 plan records the CSS failure first. Reject any
route that reuses `sheets_witness`, emits a hand parser, or admits by report
fixture alone.
