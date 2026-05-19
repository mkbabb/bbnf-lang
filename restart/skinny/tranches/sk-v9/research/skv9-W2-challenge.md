# SK-V9 Wave W2 CHALLENGE: Retained Event Grammar Proof

Disposition: REJECT.

Inputs: `restart/skinny/tranches/sk-v9/research/skv9-W2-plan.md`;
`restart/skinny/tranches/sk-v9/SPEC.md` Section 5;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`;
current runtime source.

The plan selects the right proof surface, but the S4 implementation route is
not redressable as written. Current `ValueRef<'doc, 'input, K = AnyKind>` uses
`K` as the generated view node-kind marker: `RootKind`, `ObjectKind`,
`ArrayKind`, `StringKind`, `NumberKind`, `BoolKind`, and `NullKind` appear in
`runtime/src/grammars/json/view.rs`. Replacing `K` with
`G: EventGrammar = AnyGrammar` would either break the generated JSON view layer
or force node-kind markers to implement the retained grammar trait. Both shapes
violate the W2 proof intent.

## CH1 Correctness

Reject S4 as written. The proof must preserve existing node-kind identity while
adding the grammar-retention marker. A viable redress shape is
`ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>`: the
existing third parameter remains the node-kind marker, the new fourth parameter
is zero-sized grammar proof state, and existing three-argument generated view
uses remain source-compatible.

The `proof = []` feature correction is accepted; Section 5 already references
`feature = "proof"`, and `runtime/Cargo.toml` must own that cfg.

## CH2 Generality And Lock 14

Reject any route that implements `EventGrammar` for generated JSON node-kind
markers. That would conflate grammar-level retained-event contracts with
per-view value categories. JSON and Sheets witnesses remain witness-local
opaque ordinals, declared behind parent cfg module declarations in
`runtime/src/lib.rs`.

## CH3 Regression And REDRESS

The plan's default-build and no-RESULTS gates are retained. The revised route
must keep `runtime/src/grammars/json/mod.rs`, generated view/value files,
codegen templates, `bbnf-bench`, and `skinny/RESULTS.md` unchanged.

## CH4 Cost

The corrected fourth-marker route remains inside the W2 LOC budget. It adds
one `PhantomData<fn() -> G>` field and touches the existing constructors,
`Copy`/`Clone`, and `erase` signatures. This is smaller than repairing broken
generated JSON view call sites after a destructive `K -> G` rename.

## CH5 Hidden Coupling

This is the rejecting lens. P2-B assumed `K = AnyKind` was an unused grammar
marker. In the current source it is an active node-kind marker consumed by the
generated retained JSON views. The plan must be revised before redress so the
node-kind and grammar-retention axes are explicit and separate.

## CH6 Anti-Paper-Close

The wave still cannot close on a trait-only addition. The revised plan must
keep the compile witnesses, the negative borrow-check fixture, and the
no-production-reachability audits as exit evidence.

## Required Revision

Return to plan with these binding edits:

- Revise SPEC Section 5 and the W2 plan from
  `ValueRef<'doc, 'input, G: EventGrammar = AnyGrammar>` to
  `ValueRef<'doc, 'input, K = AnyKind, G: EventGrammar = AnyGrammar>`.
- State that `K` is the existing node-kind marker and `G` is the retained
  event-grammar marker.
- Make the negative fixture construct
  `ValueRef<'static, 'static, AnyKind, JsonEventGrammar>` from a local tape.
- Add a CHALLENGE acceptance pass after the revision before source redress.
