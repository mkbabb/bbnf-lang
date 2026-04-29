# O3a Round 2 Synthesis - P1/A1

Date: 2026-04-29
Scope: orchestrator synthesis of the six-agent Round 2 cohort triads.

## Inputs

- `O3a-P1-research.md`
- `O3a-P1-plan.md`
- `O3a-P1-redress.md`
- `O3a-A1-research.md`
- `O3a-A1-plan.md`
- `O3a-A1-redress.md`

## Verdict

Round 2 closes O3a as routed evidence. No O3b child wave is required
now. P1 is an O3 close gate; A1 is an O5/O6/O7 close gate. No failure
is left as an unowned residual and no compatibility shim is permitted.

## P1 Disposition

`bbnf::projection_totality projection_totality_runtime_call_count` is a
stale runtime witness over the old generated `ValueRoot` /
`<Grammar>Value` materializer path. Current generated parsers return
StructDirect runtime documents, so CSS L4 already renders a concrete
`StyleSheet` instead of a projection enum. The correct close is O3's
generated view/value/materializer purge plus a document-owned
projection/accessor test rewrite.

Required O3 owners:

- O3.P1-G1: gate `generate_views`, `emit_value_surface`, and
  `emit_materialize_fns` by `EmitStrategy`.
- O3.P1-V1: carve generated tape-view production out of StructDirect.
- O3.P1-M1: stop StructDirect `materialize_projection_*` emission.
- O3.P1-D1: prove document-owned runtime projection surfaces.
- O3.P1-T1: rewrite `projection_totality.rs` away from debug
  `"Projection"` markers.
- O3.P1-R1: orchestrator regen and zero-residue scan.

No O3b is justified unless O3 implementation proves the fix requires
`Parsed<R>`, `TapeDirect`, `crates/tape/**`, workspace manifests, or
other files outside O3's ledger.

## A1 Disposition

A1 splits live product failures from historical prototype surface:

- `bbnf-analysis::directives import_directive_has_semantic_tokens` and
  `bbnf-lsp::integration test_hover_recover_keyword` are live
  directive-span failures and must be repaired, not deleted.
- `crates/core/benches/json-prototype/**` is a historical tape-era
  JSON prototype. O5 should archive provenance and delete it from the
  live Cargo/test/bench graph rather than fixture-seeding it.
- `crates/gorgeous/src/jit.rs` is a retired derive-backed dynamic JIT
  path. O5 should delete the surface inside AZ-II; a future dynamic
  grammar product would need a fresh regen-backed tranche.
- `bootstrap_parser.rs` is a bounded bridge. O6 must prove generated
  `BbnfBootstrap::parse` can become canonical or block O7 with the
  exact self-host failure.

## Wave Amendments Applied

- `O3.md`: O3.13 owns P1 projection-totality close and generated
  residue purge.
- `O5.md`: O5.12 owns `json-prototype` archive/deletion and Gorgeous
  JIT deletion.
- `O6.md`: O6.16 owns live A1 verification and bootstrap-parser proof.
- `O7.md`: O7.12 converts A1 into terminal close evidence and blocks
  close while any live A1 surface remains.

## Next Active Wave

O3 is now active. It may deploy source-redress agents under the
`O3.md` file-owner ledger. O5/O6/O7 remain planned and must consume A1
before tape deletion, semantic close, or terminal close claims.
