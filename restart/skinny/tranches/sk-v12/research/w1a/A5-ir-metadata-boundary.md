# SK-V12 W1a A5 IR Metadata Boundary

Date: 2026-05-20
Scope: `skinny/crates/ir/src/**` and codegen metadata/config structs.
Question: does W1a need IR changes, and how should W1a avoid new directives,
BIR variants, `BackendShape` variants, or public substrate APIs?

## Verdict

W1a should not change `skinny/crates/ir/src/**`.

The minimal legal boundary is a codegen-private and generated-module metadata
surface: a `GrammarProfile` / `GrammarConfig` equivalent selected by
`backend.grammar_name`, rendered into per-grammar generated modules, and used by
templates for structural alphabet, FIRST/follow dispatch, layout/trivia,
escape policy, number policy, flag interpretation, and sink/view/kind names.
It must consume existing IR facts rather than extending IR.

Do not add:

- a new BBNF directive,
- a new `BackendExpr` / BIR variant,
- a new `BackendShape` variant,
- a new public runtime/tape/substrate API,
- a generic `match grammar_name { "json" | "css" | ... }` policy branch inside
  shared substrate code.

## Spec And Pin Evidence

1. W1a is a legality gate before CSS emission, not CSS parser admission:
   `restart/skinny/tranches/sk-v12/SPEC.md:314-317` says Section 4 / W1a's
   purpose is to make CSS L4 emission legal before any CSS generated parser is
   emitted.

2. W1a may touch IR only conditionally:
   `restart/skinny/tranches/sk-v12/SPEC.md:318-324` lists owner paths and says
   `skinny/crates/ir/src/` is in scope "only for generated metadata types if
   required." The current source audit below does not find a requirement.

3. W1a's required metadata is grammar policy, not new backend semantics:
   `restart/skinny/tranches/sk-v12/SPEC.md:332-340` requires `GrammarConfig`
   or an equivalent generated metadata surface for structural alphabet,
   FIRST/follow tables, layout/trivia, escape policy, number policy, flag
   semantics, and sink/view/kind bindings; it also requires moving JSON policy
   from generic code into JSON generated metadata/templates.

4. The W1a exit gate forbids the tempting IR/substrate expansion route:
   `restart/skinny/tranches/sk-v12/SPEC.md:341-347` requires generic-crate scan
   pass, JSON parity/guard preservation, no CSS parser row claim, and "No new
   directive, BIR variant, `BackendShape`, or public substrate API."

5. Section 2.1 makes generic code grammar-neutral:
   `restart/skinny/tranches/sk-v12/SPEC.md:261-270` bans grammar-name branches,
   JSON structural alphabets, JSON string/number/object-key policy,
   `OffsetFlags` meaning, and `JsonSink` method shape in generic code; it says
   per-grammar generated modules own those facts.

6. Section 2.1 requires executable CSS evidence later, but W1a is only the
   legality precondition:
   `restart/skinny/tranches/sk-v12/SPEC.md:271-275` says CSS L4 must be
   exercised by benchmark/equality and generated size tracked. W1a Section 4
   separately says no CSS parser row is claimed yet at `SPEC.md:345`.

7. The USER PIN keeps Lock 14 and requires W1's config extraction, but does not
   override the W1a no-public-substrate rule:
   `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-103` carries
   Lock 14 grammar-neutrality and says the seven Lock-14 leaks must be resolved
   by W1's `GrammarConfig` surface before CSS L4 emission is legal.
   `USER-PIN-W1-CSS-L4-SOTA.md:141-150` requires S-P3 to reconverge the W1 plan
   under CSS first and says `GrammarConfig` trait plus per-grammar config
   extraction lands inside the W1 redress. Read with the W1a exit gate, this
   means "trait/config" must be implemented as generated/codegen-local
   metadata, not a broad public substrate trait.

## Decision Engine Evidence

1. The decision engine audit puts W1a away from CSP/e-graph expansion:
   `restart/skinny/tranches/sk-v12/research/skv12-decision-engine-audit.md:54-60`
   says codegen pattern selection does not call cost-model symbols; the cost
   model is a passive ledger and hardcoded pass logic produces `CostFacts`.

2. The audit's active decision points are backend shape choice, recognizer
   mining, materialization strategy, type inference, and evidence collection:
   `skv12-decision-engine-audit.md:126-152` names those as W2 contact points.
   W1a should not make them pluggable in order to legalize CSS metadata.

3. The audit says lowering dispatch is already trait-based but still keyed by
   the existing five-shape enum:
   `skv12-decision-engine-audit.md:111-123` grades `codegen/src/lower/mod.rs`
   as pluggable only after `cost.chosen` exists, while shape choice and
   recognizer nomination remain hardcoded. W1a should not add a sixth shape or
   new optimizer machinery.

## Source Evidence: IR Is Sufficient For W1a

1. `GrammarIr` already carries grammar identity and source hash:
   `skinny/crates/ir/src/lib.rs:30-39` defines `GrammarIr { name, source_hash,
   rules, exprs }`.

2. The front-end already has an annotation node, but the grammar parser rejects
   arbitrary directives:
   `skinny/crates/ir/src/lib.rs:207-235` includes `ExprKind::Annotation`.
   `skinny/crates/grammar/src/lib.rs:80-97` accepts only `@import` and `@token`
   and rejects other directives as unavailable in skinny. W1a therefore should
   not add a `@grammar_config`, `@css`, `@policy`, or similar directive.

3. Backend IR already carries the grammar name, entry rule, recognizers, rules,
   and shape facts:
   `skinny/crates/ir/src/lib.rs:392-399` defines `BackendIr` with
   `grammar_name`, `entry_rule`, `recognizers`, `rules`, and `shape_facts`.
   That is enough for codegen to select a per-grammar profile.

4. `BackendShape` is already the complete legal five-shape surface:
   `skinny/crates/ir/src/lib.rs:401-408` defines `EagerTape`, `OffsetTape`,
   `EventTape`, `SinkOnly`, and `CollapsedStage`. `skinny/crates/ir/src/cost.rs:127-135`
   returns exactly those five from `all_backend_shapes()`. W1a must not add a
   `CssTape`, `GrammarConfig`, `UnionTape`, or policy-specific shape.

5. Existing BIR variants already represent parse structure, spans, tape emits,
   direct build metadata, value projection, and return:
   `skinny/crates/ir/src/lib.rs:416-452` defines `BackendExpr::{Entry, Seq,
   Alt, RepeatLoop, OptionalBranch, ByteLiteral, RegexProgram, CallRule,
   SpanMark, TapeEmit, DirectBuild, ValueProject, Return}`. W1a policy metadata
   should not become another BIR node.

6. Existing recognizer metadata already has a structural alphabet:
   `skinny/crates/ir/src/lib.rs:454-476` defines `Recognizer::SimdScan` with
   `mode`, `alphabet`, and `site`, plus `StructuralAlphabet { bytes }`.
   The current recognizer producer is JSON-curated, but that is a pass/codegen
   policy problem, not an IR shape gap.

7. Existing span and tape facts are generic enough for the W1a boundary:
   `skinny/crates/ir/src/lib.rs:478-502` defines `SpanKind::{String, Number,
   Whitespace}` and `TapeKind::{Container, Sequence, KeyValuePair, StringValue,
   NumberValue, BoolValue, NullValue, Member, Element}`. If CSS needs finer
   semantic facts, W1a should keep them in generated CSS metadata or generated
   value/kind wrappers, not in generic IR.

8. Existing direct-build metadata is already a generic codegen metadata carrier:
   `skinny/crates/ir/src/lib.rs:504-574` defines `DirectBuildField`,
   `DirectBuildSource`, `DirectBuildTarget`, type refs, scalar kinds, presence,
   cardinality, representation, and decode policy. W1a can reuse this for
   sink/direct output binding without new BIR.

9. Existing shape facts are name/string metadata, not grammar policy:
   `skinny/crates/ir/src/lib.rs:576-626` defines `ShapeFacts`, `Shape::Struct`,
   `Shape::Enum`, and `ShapeField`. This can carry generated view/type names
   already; grammar-specific dispatch and escape/number semantics do not need
   to be serialized into IR.

## Source Evidence: Codegen Is The Correct Boundary

1. Current codegen already has a JSON-only profile choke point:
   `skinny/crates/codegen/src/json_provider.rs:4-12` allows only
   `backend.grammar_name == "json"`. W1a should replace this JSON-only provider
   with a profile selector that returns codegen-private/generated metadata for
   JSON and later CSS, without changing IR.

2. Current generated file assembly is provider-owned:
   `skinny/crates/codegen/src/lib.rs:102-136` calls `json_provider`, lowers the
   existing BIR, appends sink-direct rendering, and inserts generated module
   files. This is the minimal insertion point for a `GrammarProfile` passed to
   `generated_rs`, `view_rs`, `value_rs`, `sink_rs`, and `mod_rs` renderers.

3. Codegen direct schema structs are already public codegen metadata for typed
   consumers:
   `skinny/crates/codegen/src/direct_schema.rs:3-9` defines `DirectSchemaSet`
   with `module_name`, `roots`, `types`, and `schema_hash`;
   `direct_schema.rs:11-132` defines roots, type refs, scalar kinds, presence,
   duplicate, and unknown-field policies. W1a can add separate codegen-private
   grammar metadata without expanding this consumer schema unless a typed
   consumer needs it.

4. Lowering dispatch should remain unchanged:
   `skinny/crates/codegen/src/lower/mod.rs:17-24` selects among existing shape
   lowerers by matching the five `BackendShape` variants. W1a metadata should
   affect rendered parser/template policy, not shape selection.

5. The existing JSON template contains the leaks W1a must move into generated
   metadata:
   `skinny/crates/codegen/src/json_templates/generated.rs:10-17` hardcodes the
   JSON structural alphabet; `generated.rs:47-58` hardcodes JSON value FIRST
   dispatch; `generated.rs:90-117` hardcodes quoted key plus colon member
   policy; `generated.rs:142-157` and `generated.rs:171-200` hardcode JSON
   string quote/backslash behavior; `generated.rs:205-217` delegates to a
   JSON-shaped number span matcher. These are template/provider facts, not IR
   facts.

6. Current typed-direct rendering is also JSON-structured:
   `skinny/crates/codegen/src/typed_direct.rs:79-116` emits object braces,
   string keys, colon separators, comma handling, and unknown-field behavior.
   If W1a touches typed-direct legality, it should parameterize this renderer
   through codegen metadata/config. It should not add IR variants.

## Recommended Minimal Boundary

1. Add no files or types under `skinny/crates/ir/src/**` for W1a.

2. Introduce a codegen-internal profile, for example:

   - `pub(crate) struct GrammarProfile`
   - `pub(crate) struct DispatchArm`
   - `pub(crate) struct EscapePolicy`
   - `pub(crate) struct NumberPolicy`
   - `pub(crate) struct FlagPolicy`
   - `pub(crate) struct BindingNames`

   Keep these in `skinny/crates/codegen/src/` and pass them into renderers.
   They are implementation metadata, not substrate API.

3. Render per-grammar generated metadata modules or private constants:

   - JSON: generated `config.rs` or private constants equivalent to today's
     hardcoded JSON values.
   - CSS L4: generated `config.rs` and generated value/kind/sink/view names
     once W1b emits the CSS parser baseline.

   Generated modules may expose grammar-local public parser/view types as part
   of that grammar's generated API, but must not expose new `runtime::tape` or
   generic substrate APIs.

4. Replace `json_provider::ensure_runtime_profile()` with a provider/profile
   selector at the codegen boundary. The selector may key off
   `BackendIr.grammar_name`, but the selected renderer must own policy through
   per-grammar generated metadata. Do not put grammar-name branches in runtime
   substrate, pass logic, or IR semantics.

5. Reuse existing IR:

   - `BackendIr.grammar_name` selects the profile.
   - `Recognizer::SimdScan` and `StructuralAlphabet` carry structural bytes.
   - `SpanKind` keeps broad string/number/whitespace classes.
   - `TapeKind` keeps broad materialization roles.
   - `DirectBuild*` keeps direct/sink binding facts.
   - `ShapeFacts` keeps generated type and view names.

6. Treat CSS-specific facts as generated policy:

   - CSS token/value kinds belong in generated CSS `value.rs` / `view.rs` /
     `config.rs`, not `TapeKind` or `BackendExpr`.
   - CSS escape interpretation belongs in generated CSS policy or helper calls,
     not `OffsetFlags` public constants.
   - CSS sink method shape belongs in generated CSS sink trait/module, not
     `JsonSink` or a generic substrate trait.

7. Gate proof for W1a should include a public API diff/check that shows:

   - `skinny/crates/ir/src/lib.rs` and `skinny/crates/ir/src/cost.rs` unchanged.
   - `BackendShape` remains five variants.
   - `BackendExpr` variant roster is unchanged.
   - `skinny/crates/grammar/src/lib.rs` directive acceptance remains unchanged.
   - no new `runtime::tape` public item, `UnionTape`, side vector, retained
     cursor/list, or grammar policy constant appears in public substrate.

## Practical Answer

W1a does not need IR changes. The safest implementation is codegen-only:
parameterize the existing JSON templates and provider with a private
`GrammarProfile`, emit grammar-local config/constants, keep runtime substrate
opaque and stable, and prove the five-shape/BIR/directive/public-substrate
surfaces did not expand.
