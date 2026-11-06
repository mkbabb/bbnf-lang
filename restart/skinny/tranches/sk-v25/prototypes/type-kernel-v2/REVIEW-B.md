# Type-kernel v2 independent reviewer B

Session `019f76f5-e633-7a12-9ea4-13dd47dbfb86`; recorded model
`gpt-5.6-sol`, effort `xhigh`. This replacement review was independent of critic
A and made no repository edits. Disposable counterexamples were run from
`/tmp/bbnf-type-kernel-v2-review-b2.rzR25d`.

## Verdict

**REJECT — 35% toward a production replacement, approximately 55–60% as an
isolated demonstration.** The documented gates pass twice, but independent
programs disprove soundness, completeness, normalization, recursive-binder,
environment-scheme, ABI-layout, emitter, provenance, and production-connectivity
claims.

## Independently proved failures

1. Row occurs follows tails but not field types. The infinite equation
   `{a | r} = {a, x: {| r}}` is accepted instead of returning `RowOccurs`.
2. Four compatible open rows joined through two solver residuals fail because
   different `Residual` identities cannot later unify. The witness is their
   common extension `{a,b,c,d | s}`.
3. Two sibling records sharing one row tail normalize to
   `record{a:I64,c:I64|closed}` and `record{b:I64|?r0}`. The zonker never removes
   row IDs from its shared visiting set on unwind.
4. Nested vacuous binders are accepted, while a prebuilt outer reference embedded
   beneath a new binder is rejected as `BinderEscapes`; de Bruijn indices are
   not shifted compositionally.
5. The layout ignores its ABI parameter and has no aggregate size, alignment,
   offsets, padding, discriminant, or niche policy. On the native
   `aarch64-apple-darwin` host, `String`, `Vec<String>`, and `Option<String>` are
   24-byte values while the carrier reduces them to pointer-shaped leaves.
6. Top-level `type` and field `bad-label` are accepted and emitted as invalid
   Rust and TypeScript. Rust singleton tuples are emitted as `(i64)`, silently
   losing tuple semantics.
7. A correctly non-generalized environment meta is encoded as a rigid polytype
   which instantiation turns into `DanglingMeta`; no rigid substitution context
   exists.
8. Ten thousand lacks constraints succeed with a zero-step budget. Preprocessing,
   lacks, normalization, rendering, and public resolution are incompletely
   charged. `Exhausted`, `CrossStore`, and `DanglingType` have no origins.
9. The fixtures import no real `IrNode`; they attach fourteen names to invented
   terms. Several mappings contradict production: literal/regex/epsilon spans,
   negate, map return types, referenced-rule projection, and token-dispatch joins.
   The real `TypeDesc` vocabulary is not bridged.
10. Production still calls `project_types` and populates `GrammarIR.types`,
    `TypeMap`, `StructRegistry`, obligations, and payload layouts. Repository
    search finds no production reference to this carrier.

## Structure and test ruling

`solve/state.rs` is 696 lines and combines ordering, validation, budgets, equality,
row solving, lacks, both occurs checks, diagnostics, loading, rendering, and
finalization. External tests are not source-isomorphic: there are no direct
counterparts for `error`, `id`, `project`, `shape`, `term`, `solve/state`, or
`solve/zonk`.

## Minimal superseding contract

- Flexible bindable row variables; field-recursive occurs; path-local
  normalization; qualified lacks-preserving schemes.
- Explicit binder occurrence tracking and correct shifting or levels.
- Rigid environment substitution and cross-solution ownership rules.
- End-to-end operation budgets and attributable provenance on every solver
  failure.
- A real size/alignment/offset ABI implementation with native probes, or a
  deliberately non-physical semantic layout with the stronger claim removed.
- Target-specific name allocation, keyword/collision handling, quoted labels,
  singleton tuple correctness, and hostile compiler consumers.
- A development bridge to real `bbnf-ir`, exhaustive actual-node construction,
  complete `TypeDesc` coverage, source spans, production structures, and live
  downstream consumers.
- Goldilocks solver modules mirrored by external tests.

