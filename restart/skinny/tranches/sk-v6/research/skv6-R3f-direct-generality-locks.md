# SK-V6 Wave 3 R3f: DirectBuild Generality / Lock Audit

Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Date: 2026-05-14

## Required Inputs Read

- `skinny/REDRESS.md` rows 66-68 summary and surrounding entries.
- `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` Candidate 7/8/9 outcomes and final Wave 1e instruction.
- `restart/skinny/COMPILER.md` DirectBuild, SinkOnly, BIR construction discipline, hand-curated shape waiver, and lowerer sections.
- `restart/ARCHITECTURE.md` BackendShape / DirectBuild / direct-to-struct union / Lock 14 future grammar sections.
- Current skinny code for `DirectBuild`, `DirectBuildField`, `DirectBuildSource`, `passes::extract::materialize_rule`, and `codegen::lower::sink_only`.

## Lock State

REDRESS 66-68 are binding:

- REDRESS 66 rejects direct source-hook field-layout / receiver folding. Do not reopen `*_source` receiver shortcuts as the next close.
- REDRESS 67 rejects parser-owned decoded scratch for generated direct escaped strings.
- REDRESS 68 rejects byte-output `unescape_json_string` materialization. The local allocation / receiver / byte-writing family is exhausted.

The Grand Synthesis final instruction after Candidate 9 is the active constraint: next Wave 3 work must leave local escaped-string writer churn and target DirectBuild field facts or a strict representation-level direct output contract. It must not add directives, BIR variants, JSON code to generic crates, or a parallel source pass.

ARCH adds the hard locks:

- `BackendShape` selection is cost/model/fact derived; no user-visible directive.
- `SinkOnly` is the existing direct output shape; adding a parallel substrate or sidecar structural pass violates Lock 1 / SK-V5 redress.
- Lock 14 requires future grammar onboarding without handwritten generic crate changes or grammar-name branches.
- Direct outputs are caches/projections over the accepted event stream, not a second authoritative tree.

## Current Representation Problem

The existing BIR already has the correct top-level hook:

```rust
BackendExpr::DirectBuild { shape, fields: Vec<DirectBuildField> }
DirectBuildField { name, source: DirectBuildSource }
DirectBuildSource::{Span, ChildRule, RepeatedRule, Literal, Empty}
```

`codegen::lower::sink_only` already preserves `DirectBuild` rosters into `SinkOnlyProgram` without needing grammar IR.

The blocker is that field facts are not yet general:

- `passes::compile` still calls `shapes::shapes_for_json()`.
- recognizer mining still nominates JSON.
- `materialization_for_rule` and `direct_fields_for_rule` match literal JSON rule names.
- `json_sink_direct.rs` validates hardcoded `JsonObject`, `JsonArray`, `JsonPair`, `JsonString`, `JsonNumber`, `JsonBool`, and `JsonNull` rosters inside a generic crate.

Those are existing skinny waivers / known violations. The next route must reduce them, not add another JSON-specialized path.

## Rejected Routes

Reject these before implementation:

- New `.bbnf` directive for direct fields or sink layout. This violates the no-directive BackendShape/DirectBuild contract.
- New top-level BIR variant such as `DirectFieldDigest`, `DecodedStringFact`, `SinkHash`, `DirectString`, or `FieldLayoutBuild`. The existing `DirectBuild { shape, fields }` is the representation point.
- Parallel substrate, second scanner, sidecar source pass, retained structural-event table, or source-hook replay. These repeat rejected SK-V5/SK-V6 routes.
- JSON-specific code in generic crates, including more `Json*` validation/rendering logic in `passes`, `codegen`, `ir`, `cost-model`, or `pipeline`.
- Sink-local decoded stats, quote-source streaming hash, parser-owned scratch, or byte-output unescape changes. These are REDRESS 54/55/66/67/68 recurrences.

## Narrowest Admissible Route

Implement a generic DirectBuild field-fact contract on the existing `DirectBuildField` payload, then make SinkOnly lowering/rendering consume those facts.

Concrete shape:

1. Add generic field facts to the existing `DirectBuildField` payload, not a new BIR expression. The facts should describe representation, not JSON behavior. Minimum likely fields:
   - cardinality: one / optional / repeated
   - source class: span / child rule / repeated rule / literal / empty, using the existing `DirectBuildSource`
   - payload policy: borrowed slice, lazy scalar, eager scalar, normalized string, sink-emitted field, or no payload
   - role/context if needed for generic sink emission: key-like field, element-like field, scalar value field, container begin/end, but named generically and derived from shape/type facts rather than grammar names

2. Derive the field roster from shape/type/payload facts in `passes`, replacing `direct_fields_for_rule(name)` and `materialization_for_rule(name)` with a grammar-derived table. For skinny JSON this table may reproduce the current roster, but the code path must also work for the future grammar test without adding Rust branches.

3. Keep `SinkOnlyProgram` as the carrier. It already stores `direct_shape: Option<DirectShape>` and `DirectShape { shape, fields }`; extend that path to preserve the new field facts.

4. Replace the hardcoded JSON SinkOnly renderer validation with a generic renderer contract, or fence JSON-specific rendering strictly under generated `runtime/src/grammars/json` output. Generic crates may render from `SinkOnlyProgram` + schema facts, but must not test for `JsonObject`/`JsonString` names.

5. Make cost selection still choose existing `BackendShape::SinkOnly`; do not introduce a new shape unless the architecture later defines a strict direct-output contract as a separate documented shape. For this intervention, the narrow route is still `SinkOnly + DirectBuildField facts`.

This route is admissible because it changes the DirectBuild representation payload that already exists, consumes the same accepted event stream, and moves toward arbitrary grammar support. It does not add a directive, new BIR node, parallel substrate, or JSON logic in generic crates.

## Required Spec / Doc Updates

Update these docs before or with implementation:

- `restart/skinny/COMPILER.md` §3.1 / §6.1: redefine `DirectBuild { shape, fields }` as carrying grammar-derived field facts, not just a JSON typed-view roster. State that scalar facts are representation policy and access/materialization policy, not sink-local decoded statistics.
- `restart/skinny/COMPILER.md` §5.5: replace or tighten the `shapes_for_json()` waiver language so V1 graduation requires generic shape + field-fact mining, not only shape names.
- `restart/skinny/COMPILER.md` §3.3-3.4 audit invariants: add a specific invariant that SinkOnly direct output may only consume `DirectBuildField` facts and accepted event/span payloads; it may not rescan source or add a sidecar table.
- `restart/ARCHITECTURE.md` §7.4: change the remaining remediation from broad “target DirectBuild field facts” to the exact route: generic field-fact derivation on existing `DirectBuildField` plus generic SinkOnly rendering.
- `restart/ARCHITECTURE.md` §9.2: clarify that direct scalar/string fields are caches over declared DirectBuild field facts and payload policy, not ad hoc sink helpers.
- `restart/ARCHITECTURE.md` §12 future grammar onboarding: add a gate that a new grammar’s direct field rosters are generated from grammar + metadata, with zero generic-crate Rust edits other than generated runtime output.
- Optional diagnostic addition in `restart/ARCHITECTURE.md` §7.5: `BBNF-DIRECT-FIELD-FACT-NONGENERIC` for grammar-name checks or JSON shape references in generic DirectBuild/SinkOnly code.

## Concise Recommendation

Next direct intervention: implement generic `DirectBuildField` field facts and route SinkOnly codegen through them. Start by replacing JSON-name materialization and field-roster construction in `passes` with a mined shape/type/payload-fact table, then remove hardcoded JSON shape validation from generic codegen. Do not touch `unescape_json_string`, source hooks, parser scratch, or byte-output writers again without a new local fact.
