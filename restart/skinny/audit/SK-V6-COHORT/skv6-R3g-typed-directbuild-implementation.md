# SK-V6 Wave 3 R3g: typed DirectBuild implementation route

Scope: read-only implementation routing. No repository files edited by the
agent.

## Recommendation

Narrowest viable route: make typed direct output a `DirectBuild` payload
refinement, then generate the Track 1 benchmark from that payload. Do not add
directives, BIR variants, benchmark-only parsers, or JSON branches in generic
crates.

## Route

1. Extend field facts in `skinny/crates/ir/src/lib.rs`: add a
   `DirectFieldPolicy` / materializer enum beside `DirectBuildSource`, with
   variants from the docs: `BorrowSpan`, `SemanticStringFact`, `NumberScalar`,
   `LiteralMap`, `Child`, `Repeated`, `Empty`. Add the policy to
   `DirectBuildField`.
2. Preserve the payload through
   `skinny/crates/codegen/src/lower/sink_only.rs`. It already clones
   `DirectBuildField`; the immediate need is tests asserting policies survive
   into `SinkOnlyProgram`.
3. Replace the JSON-only Track 1 renderer path in
   `skinny/crates/codegen/src/json_sink_direct.rs` with a generic renderer,
   likely `skinny/crates/codegen/src/sink_only_typed.rs`. It should consume
   `SinkOnlyProgram` plus `ShapeFacts`, emit generated typed structs/enums and
   `parse_direct_typed<'i>(input) -> Result<RootDirect<'i>, ParseError<'i>>`,
   and render grammar-specific parser code from BIR facts only. JSON names may
   appear only in generated JSON output, not in generic crates.
4. Remove the generic-crate JSON mining blockers in
   `skinny/crates/passes/src/lib.rs`: stop calling `shapes::shapes_for_json`,
   `nominate_json`, and `materialization_for_rule` by literal rule name. The
   narrow route is to derive `ShapeFacts` and `DirectBuildField` from
   type/layout facts and grammar rule structure, then specialize only through
   facts.
5. Change Track 1 direct benchmark in
   `skinny/crates/bbnf-bench/src/direct_struct.rs` to call generated typed
   output, then digest the typed output. Keep Track 2 only as independent
   comparison, not as the implementation model.
6. Update benchmark rows in
   `skinny/crates/bbnf-bench/benches/json_parity.rs` so
   `track1_direct_to_struct` measures generated typed direct output, not
   `JsonSink` digest events.

## Redress Gate

Gate remains `N-direct`: both generated Track 1 and independent Track 2 must
be within `1.10x` sonic-rs time per `skinny/crates/bbnf-bench/src/gate.rs`.
Correctness must compare generated typed output digest against Track 2, serde,
and sonic shape parity. The expected redress close is: `N-direct / NoGo`
becomes pass only when Track 1 typed direct output is real generated output,
full strictness still passes, and no generic crate contains JSON rule-name
logic.
