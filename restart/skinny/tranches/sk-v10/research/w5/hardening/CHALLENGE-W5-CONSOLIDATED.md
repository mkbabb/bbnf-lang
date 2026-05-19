# SK-V10 W5 CHALLENGE - Root-Type Typed Generalization Proof

Pass: CHALLENGE.
Cycle: W5.
Date: 2026-05-19.
Plan under review: `restart/skinny/tranches/sk-v10/research/w5/w5-plan.md`.
Disposition: ACCEPT.

## CH1 Correctness - ACCEPT

The selected mechanism addresses the actual blocker: public typed roots are
currently limited to named struct `type_id` values. Replacing root `type_id`
with `DirectTypeRef` lets the same renderer parse struct, array, and map-entry
roots through one path.

Redress requirement: root-level helpers must be collected before public root
functions are rendered, and validation must reject missing type references
inside root `DirectTypeRef` values.

## CH2 Generality / Lock 14 - ACCEPT

The plan edits generic typed DirectBuild schema and renderer code, but the
representation is grammar-neutral. `DirectTypeRef::Vec` and
`DirectTypeRef::MapEntriesVec` already exist below root level; W5 lifts that
model to roots without adding JSON-specific policy.

Redress requirement: add a codegen test showing emitted typed root code does
not contain `JsonSink` or `serde_json::Value`, and keep JSON-specific fixture
choices in the bench proof layer, not in generic codegen.

## CH3 Regression / REDRESS - ACCEPT

W5 moves no `RESULTS.md` row and does not loosen W1/W2/W4 row contracts. It
also preserves the W4 rejection by using synthetic proof roots rather than
claiming an `instruments`, `github_events`, or `gsoc-2018` admission.

Redress requirement: gate evidence must include an unchanged frozen
`gate-json --with-cost-facts --check-results`.

## CH4 Cost - ACCEPT

The plan fits the W5 budget if the schema/root model change stays localized:
`direct_schema.rs`, `typed_direct.rs`, the existing codegen test helper, the
real typed schema, generated typed output, and proof tests.

Redress requirement: if the generated typed module churns unrelated existing
root bodies, stop and REVISE before committing.

## CH5 Hidden Coupling / Lock 1 - ACCEPT

W5 does not add a substrate, sidecar tape, parse-only producer, runtime public
API, direct digest shortcut, or benchmark-private production parser. The proof
roots are generated typed parser functions in the existing module.

Redress requirement: no `json_parity` row registration, no new telemetry
field, and no new outcome variant.

## CH6 Anti-Paper-Close - ACCEPT

The wave cannot close on type-level expressivity alone. It must prove generated
root parsers against serde_json and sonic-rs sidecars for both array and
map-entry roots.

Redress requirement: add bench-crate tests whose checksums compare generated
Track 1 output to serde_json and sonic-rs typed output for the synthetic array
and map-entry fixtures.

## Accepted Redress Conditions

- `DirectRootSchema` or successor roots carry `DirectTypeRef`.
- Struct roots remain supported through an explicit constructor.
- Array and map-entry root helpers are generated and used by public root
  functions.
- Codegen tests show no JSON sink/value policy in generated typed roots.
- Generated proof roots pass serde_json and sonic-rs checksum parity.
- `RESULTS.md` is unchanged.
- Frozen `gate-json --with-cost-facts --check-results` passes.
