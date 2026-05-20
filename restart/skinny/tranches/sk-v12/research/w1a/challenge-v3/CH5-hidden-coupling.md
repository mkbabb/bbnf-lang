# SK-V12 W1a CHALLENGE V3 - CH5 Hidden Coupling

Date: 2026-05-20.
Lens: CH5 hidden coupling.
Disposition: ACCEPT.

## Disposition

CH5 disposition: ACCEPT.

`PLAN-V3.md` closes the V2 hidden-coupling blockers. It makes `scan.rs` and
`sink.rs` JSON-owned source, removes them from the generated roster, requires
their generated provenance to be removed/replaced, and includes them in the
revert slice. That resolves the source/generated ownership ambiguity.

The renderer leak is also closed at plan level: `sink_direct.rs` and
`typed_direct.rs` must be deleted, with retained compatibility stubs allowed
only if policy-free and covered by the generic leak scan.

## Findings

- `scan.rs` / `sink.rs` source ownership: ACCEPT. V3 owns both files as source
  and requires `json_provider` to stop emitting/checking them as generated
  outputs.
- Generated roster exactness: ACCEPT. The W1a generated JSON roster is exactly
  `config.rs`, `generated.rs`, `host.rs`, `mod.rs`, `parser.rs`, `value.rs`,
  `view.rs`, and `visitor.rs`; `check-json` must fail missing, differing, and
  unexpected generated-roster files while ignoring source siblings.
- Stale generated file risk: ACCEPT. Removing generated provenance from
  `scan.rs` / `sink.rs` plus exact eight-file generated checking closes the
  stale-template/source ambiguity.
- Renderer stubs: ACCEPT. Deletion is mandatory; any retained stub must be
  JSON-policy-free and included in the generic leak scan.
- Passes recognizer hidden coupling: ACCEPT. V3 narrowly owns
  `skinny/crates/passes/src/lib.rs` only to replace the current hardcoded JSON
  structural alphabet in `recognizers::derive_recognizers` with
  grammar-derived facts, without authorizing broader pass policy.

## Required Redress Guard

Implementation must keep the V3 boundaries executable: no `scan.rs` /
`sink.rs` in emitted generated files, no retained JSON policy in old renderer
names, and no production JSON structural alphabet literal in generic recognizer
roots.
