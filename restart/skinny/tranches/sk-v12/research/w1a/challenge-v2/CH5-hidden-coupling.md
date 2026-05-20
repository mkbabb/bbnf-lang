# SK-V12 W1a CHALLENGE V2 - CH5 Hidden Coupling

Date: 2026-05-20.
Lens: CH5 hidden coupling.
Disposition: REVISE.

## Disposition

CH5 disposition: REVISE.

V2 fixes the main V1 direction: `scan.rs` / `sink.rs` are no longer supposed to
be generated outputs, typed direct containment is mandatory, and the generic
scan roots are narrowed to `codegen/src/lib.rs`, `grammar_profile.rs`, and
shared runtime/IR/pass roots. That is the right shape.

One ownership inconsistency still blocks ACCEPT: V2 says runtime JSON `scan.rs`
and `sink.rs` become JSON-owned source files with generated headers
removed/replaced, but the exact editable-source roster and rejected-patch roster
do not include those two files. Current code still emits them from
`codegen/src/lib.rs` and reads them through `json_provider::scan_rs()` /
`sink_rs()`, while `scan.rs` still carries a generated header. Redress would
either need an unowned edit or would leave the stale-generated provenance risk
unresolved.

## Findings

- `scan.rs` / `sink.rs` ownership: REVISE. V2 chooses source ownership
  conceptually, but must add `skinny/crates/runtime/src/grammars/json/scan.rs`
  and `sink.rs` to editable source and revert rosters, and must state that
  `json_provider` no longer emits them as generated outputs.
- Typed-direct containment: ACCEPT pending implementation. V2 makes
  `json_typed_direct.rs` mandatory and requires deleting or policy-free
  stubbing `typed_direct.rs`.
- Generic scan roots: ACCEPT with one guard. The root list is coherent only if
  old `sink_direct.rs` / `typed_direct.rs` are deleted or scanned as
  JSON-policy-free stubs.
- Stale generated file risk: REVISE. Exact `check-json` over the eight-file
  generated roster is good, but `scan.rs` / `sink.rs` must lose generated
  provenance under an owned edit, otherwise stale template/source coupling
  survives.

## Required Revision

Add runtime JSON `scan.rs` and `sink.rs` to the owned source and rejected-patch
rosters, require generated headers to be removed/replaced, and require any
retained `sink_direct.rs` / `typed_direct.rs` compatibility stubs to contain no
JSON policy or be included in the generic leak scan.

After that, CH5 should ACCEPT.
