# SK-V9 Wave W2 CHALLENGE V2: Retained Event Grammar Proof

Disposition: ACCEPT.

Inputs: `restart/skinny/tranches/sk-v9/research/skv9-W2-plan.md` after
revision; `restart/skinny/tranches/sk-v9/research/skv9-W2-challenge.md`;
`restart/skinny/tranches/sk-v9/SPEC.md` Section 5 after revision; current
runtime source.

The revised plan resolves the hidden coupling found in CHALLENGE V1. W2 keeps
`ValueRef<'doc, 'input, K = AnyKind>` as the existing retained-view node-kind
axis and adds `G: EventGrammar = AnyGrammar` as a fourth zero-sized
event-grammar proof axis.

## CH1 Correctness

Accepted. The revised `ValueRef<'doc, 'input, K, G>` shape preserves generated
JSON view typing and gives the proof tests a grammar marker to instantiate. The
negative fixture must construct
`ValueRef<'static, 'static, AnyKind, JsonEventGrammar>` from a local tape and
assert borrow-check rejection.

## CH2 Generality And Lock 14

Accepted with audit conditions. `EventGrammar` remains grammar-neutral, and the
JSON/Sheets facts are witness-local opaque ordinals. Redress must show that
`admits_fact`, `admits_class`, `STRUCTURAL_CLASS_COUNT`, and `FactId` matches
are confined to `tape/event_grammar.rs`, witness files, or proof tests.

## CH3 Regression And REDRESS

Accepted. W2 is proof-only: no generated JSON runtime files, no parser/scanner
control files, no benchmark crate, no fixtures, and no `skinny/RESULTS.md`
mutation. The default runtime build must pass without `proof`.

## CH4 Cost

Accepted. The extra `ValueRef` marker and witness modules remain within the W2
LOC budget. If redress finds call-site churn in generated views, stop and
return REVISE rather than editing generated source.

## CH5 Hidden Coupling

Accepted. The node-kind and grammar-proof markers are now separate axes, so the
proof no longer requires generated node-kind markers to implement
`EventGrammar`.

## CH6 Anti-Paper-Close

Accepted with evidence requirements. The wave closes only if:

- `cargo check -p runtime --features proof` passes.
- `cargo test -p runtime event_grammar --features proof -- --nocapture`
  passes, including the compile witnesses and compile-fail fixture.
- `cargo build -p runtime` passes without `proof`.
- `git diff --exit-code HEAD -- skinny/RESULTS.md` stays clean.
- The Lock 14 and bbnf-bench reachability audits named by the plan pass.

## Redress Authorization

W2 is authorized for redress under the revised owner paths in SPEC Section 5.
The redress agent may edit only the listed runtime proof files, `tape/mod.rs`,
`runtime/src/lib.rs`, `runtime/Cargo.toml`, and the W2 status/REDRESS/HANDOFF
docs needed to record the admit.
