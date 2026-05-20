# W4 R1 Generated Dispatch Lowering Research

Scope: research only. No source or generated runtime files were edited.

## Inputs Read

- SK-V11 SPEC Section 8 names W4 candidates as C6 generated
  FIRST/prefix/lookahead dispatch plus P2-D D1 `container_tail_next` and D2
  `direct_slot_dispatch`, and caps the redress plan to one scalar generated
  dispatch shape and at most three direct rows
  (`restart/skinny/tranches/sk-v11/SPEC.md:489`-`537`).
- The active handoff says W4 is ready for research because REDRESS 114 rejects
  W3 with measured `mesh/direct_to_struct` failure, while REDRESS 113's
  non-JSON axis block must be carried forward
  (`restart/skinny/tranches/sk-v11/HANDOFF.md:91`-`132`).
- REDRESS 111 admits only the non-JSON gate/report lane, REDRESS 112 rejects
  the generated CSS L4 baseline, REDRESS 113 blocks W2 before implementation,
  and REDRESS 114 rejects W3 while leaving W4 dispatchable
  (`skinny/REDRESS.md:3284`-`3381`).
- P3A-C1 is the relevant candidate packet: it asks for one generated scalar
  slot dispatcher over the existing cursor, forbids retained next-byte state,
  object-key carry, value-byte compaction, class columns, and sidecars, and
  requires generated Track 1 plus independent Track 2/oracle evidence on the
  same selected rows
  (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:92`-`132`).
- P2-D distinguishes D1 and D2. D1 is a current-cursor container-tail helper;
  D2 is a SinkOnly direct slot dispatcher that preserves existing
  `BackendShape::SinkOnly` and `DirectBuild`
  (`restart/skinny/tranches/sk-v11/research/p2/p2d-substrate-tape.md:28`-`58`).

## Current Lowering Shape

`skinny/crates/codegen/src/lower/sink_only.rs` is metadata-only today. It
lowers existing `BackendExpr` nodes into `SinkOnlyExpr`, records direct shapes,
span kinds, literals, and an `AltMode::Dispatch` branch count, then returns a
`SinkOnlyProgram` only when DirectBuild shapes exist
(`skinny/crates/codegen/src/lower/sink_only.rs:20`-`240`).

That means W4 should not need a directive, BIR variant, or new backend shape.
The current `SinkOnlyProgram` already proves the generated program has
DirectBuild shapes, literals, spans, and dispatch alternatives. Adding a new
semantic fact in `sink_only.rs` would be high-risk unless it is derived only
from existing BIR and used as renderer metadata.

The generated direct parser is actually emitted by `skinny/crates/codegen/src/sink_direct.rs`.
`codegen::emit_with_layout` appends `sink_direct::render(sink_only)` to the
JSON generated template (`skinny/crates/codegen/src/lib.rs:117`-`127`). The
renderer currently emits three near-duplicate direct value dispatch functions:
root `parse_value_direct`, object `parse_object_value_at_direct`, and array
`parse_array_element_at_direct`
(`skinny/crates/codegen/src/sink_direct.rs:120`-`245`). It also emits separate
object and array container-tail loops
(`skinny/crates/codegen/src/sink_direct.rs:247`-`311`).

## Smallest Admissible Slice

Smallest technically admissible shape: D2 `direct_slot_dispatch`, scalar only,
with no container-tail rewrite in the first implementation slice.

Implementation intent:

1. Keep `sink_only.rs` as proof/metadata, or at most add a derived
   renderer-only enum/fact proving the JSON SinkOnly program has root, object,
   and array value slots. Do not add a directive, BIR variant, backend shape, or
   public substrate API.
2. In the generated direct renderer, replace the three duplicated direct value
   match bodies with one scalar slot dispatcher parameterized by root,
   object-value, or array-element sink slot. The helper must preserve the
   current cursor contract: root entry performs leading whitespace skip, while
   object and array callers already enter at a value byte.
3. Keep number, string, literal, object, and array semantics unchanged. Slot
   selection only chooses the existing sink callback family: `sink.*`,
   `sink.object_*`, or `sink.array_*`.
4. Differential generated Track 1 against the current generated output, the
   independent hand direct digest in `direct_struct.rs`, and serde/sonic digest
   oracles before row measurement.

Why D2 first instead of D1:

- D2 is a pure generated direct code-shape change and maps directly to the
  W4 "generated dispatch" request.
- D1 is also valid, but it requires coordinating container-tail semantics in
  generated Track 1 and independent Track 2 if row movement depends on both.
  That is a larger first slice because the hand direct parser in
  `direct_struct.rs` currently has object/array tail loops of its own
  (`skinny/crates/bbnf-bench/src/direct_struct.rs:483`-`539`).
- D2 still has a serious row-risk: without Track 2 source movement, only a
  near-floor row such as `mesh` has any plausible both-track path, and relying
  on measurement noise is not an acceptable plan. A W4 plan should therefore
  require a throwaway microbench before redress and be ready to switch to D1 if
  Track 2 is the limiting side.

## Exact Owner Paths

SPEC Section 8 owner paths for W4 are:

- `skinny/crates/codegen/src/lower/sink_only.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/track2/json.rs`
- `skinny/crates/bbnf-bench/benches/json_parity.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Owner-path gap: the source file that actually renders generated SinkOnly direct
code is `skinny/crates/codegen/src/sink_direct.rs`, but it is not named in
SPEC Section 8. A D2 or D1 source implementation cannot honestly be done by
editing only `sink_only.rs` and `json_templates/generated.rs`, because the
direct parser tail is appended by `sink_direct::render`. Before redress, the
W4 plan/CHALLENGE should either explicitly authorize
`skinny/crates/codegen/src/sink_direct.rs` as part of the codegen owner surface
or reject generated direct dispatch lowering as not implementable inside the
current owner list.

Minimal D2 owner set if that owner gap is resolved:

- `skinny/crates/codegen/src/lower/sink_only.rs` only for derived metadata or
  tests; prefer no semantic change.
- `skinny/crates/codegen/src/sink_direct.rs` for the scalar slot dispatcher.
- `skinny/crates/runtime/src/grammars/json/generated.rs` as regenerated output.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` for generated-vs-hand digest
  parity tests or, if selected, a mirrored independent hand-parser helper.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` only if a throwaway probe
  becomes a durable W4 microbench.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`, `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/RESULTS.md`, and `skinny/REDRESS.md` only after a row-moving redress
  attempt has measurement evidence to consume.

## Likely Regeneration And Checks

From `skinny/`:

```sh
cargo run -p xtask -- regen-json
cargo run -p xtask -- check-json
```

`regen-json` reads `grammars/json.bbnf`, calls `codegen::emit_from_source("json",
...)`, and writes `crates/runtime/src/grammars/json` (`skinny/xtask/src/main.rs:121`-`125`).
`check-json` performs the same emission and rejects stale generated output
(`skinny/xtask/src/main.rs:128`-`134`).

Minimum pre-measurement checks for a later implementation:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- check-json
RUSTFLAGS="-C target-cpu=native" cargo test -p codegen --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench direct_digest -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo check -p runtime -p codegen -p bbnf-bench
```

Row evidence still needs Criterion on selected W4 rows and guard rows, plus
`gate-json --with-cost-facts --check-results` if `RESULTS.md` is updated.

## Risks And Reject Boundaries

- **Owner-path risk:** without `sink_direct.rs`, W4 cannot modify the actual
  generated direct dispatch renderer. Hand-patching
  `runtime/src/grammars/json/generated.rs` would violate the regenerated-output
  rule.
- **Track 2 risk:** D2 mainly affects generated Track 1. W4 admission requires
  both generated Track 1 and independent Track 2/oracle floors, so a plan must
  either select a row with credible Track 2 headroom or include an independent
  hand-parser/container-tail slice without coupling Track 2 to Track 1.
- **No directive/BIR/substrate risk:** the allowed shape is renderer-local
  scalar dispatch over existing SinkOnly/DirectBuild metadata. Any new BBNF
  directive, `BackendExpr`, `BackendShape`, sidecar, retained cursor, class
  lane, or object/key/value-byte carry reopens pre-blocked routes.
- **Lock 14 risk:** dispatch facts must remain grammar-generated metadata.
  JSON punctuation may appear in generated per-grammar code, not in generic
  crates as policy.
- **Cost risk:** a unified slot helper may improve i-cache/code size or may
  regress inlining and branch prediction. The first implementation should be
  guarded by old-vs-new generated direct microbench data before row redress.

## Finding

The smallest W4 research-backed implementation is a scalar D2
`direct_slot_dispatch` renderer refactor, not a new lowering directive or
substrate. The present owner list has a blocking omission: the required source
file is `skinny/crates/codegen/src/sink_direct.rs`, while Section 8 names
`sink_only.rs` and generated/template files but not the renderer. If CHALLENGE
does not authorize that path, W4 should not attempt source redress for generated
direct dispatch lowering.
