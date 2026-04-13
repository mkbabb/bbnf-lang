# AR Critique — Corrections and Recalibrations

Cross-check of six AR audit documents against live tree state.
The diagnosis core is sound; several claims are overstated, stale,
or conflate substrate with shipped behavior.

## 1. AR overstates "direct-to-struct"

What exists today is payload projection infrastructure, not true
named struct projection. The live code supports scalar admission
(`crates/ir/src/types/type_desc.rs` — `TypeDesc::Span` already
passes `is_scalar_payload`) and aggregate layout planning
(`crates/ir/src/passes/payload/layout.rs`), plus view accessors
in `crates/core/src/backend/rust/view/`. What is still missing is
the full `Named`-to-concrete-struct lowering/codegen bridge and a
production-proof ABI. Accurate framing: "scalar payload plumbing
is partly live; aggregate/named struct projection is incomplete."

## 2. AR mixes baseline diagnosis with present-tense head-state

Several AQ-era findings are still useful diagnostically, but they
are not all current. `css_monolithic` is already gone from the
live bench config, and `post-AP.json` says Tailwind CSS L4 parses
successfully. Any AR text that still treats Tailwind parse failure
or `css_monolithic` deletion as current work is stale.

## 3. Structural dispatch is not a dormant feature ready to toggle

The current tree shows zero structural-dispatch code path in
`crates/`. `grep -rn "structural_dispatch\|structural dispatch"
crates/` returns nothing. The docs should either remove it from AR
as a near-term activation item or restate it as a fresh design
task. If it returns, it should be a peek-only hybrid dispatcher,
not the old "advance offset to structural byte" model.

## 4. Lazy AST is overstated

The runtime has cheap borrowed tape views via
`crates/core/src/runtime/parsed.rs`, and `children()` in the view
layer returns a zero-alloc iterator (not a Vec). But this is not a
memoized lazy AST. Scalar accessors still parse spans when payloads
are absent. Accurate framing: "borrowed tape views with optional
payload acceleration."

## 5. Self-hosting diagnosis is strong, but sequencing matters

The `variant_idx` overload problem in `audit-self-hosting.md` is
credible and code-grounded. But AR must not move consumers off
`crates/core/src/grammar/host.rs` or delete host-side scaffolding
before the discriminator split is fixed. Otherwise regen will keep
drifting under a different surface API.

**Correction**: discriminator split (old AR.2.1) must precede
AQ.6 activation (old Phase 1), not follow it.

## 6. The plan underweights clone/share overhead

The hottest remaining architectural waste is not just scanners.
There is still clone churn in the egraph and type passes:
`crates/egraph/src/egraph.rs` (6 `.clone()` calls),
`crates/egraph/src/extract.rs` (5), and
`crates/ir/src/passes/types/mod.rs`. AR should prioritize
structure sharing and ID-based reuse earlier than it currently does.

## 7. Regex/scanner "shared layer" claims are only partially true

There is useful caching, but codegen still performs O(n)
string-driven reverse lookups in
`crates/core/src/generate/regex/cost_model.rs` (lines 177, 189:
`ir.strings.iter().position(|s| s == pattern)`) and repeated
classification. `scanner_plan.rs` is clean — structured
`RegexClass` dispatch, no string-driven lookups. Accurate framing:
"partial registry/caching exists, but `cost_model.rs` emit-time
decisions still re-derive via linear string pool search."

## 8. Some AR performance prose is speculative, not measured

There is no `post-AR.json` or `profiles/post-AR/`. The diagnosis
in `audit-sonic-gap.md` is useful, but any "after AR" throughput
numbers in `AR.md` are targets, not facts. The "After AR
(projected)" column must be labeled as such and every claimed gain
must be gated by a post-implementation samply diff.

## What AR gets right

The core thesis is sound: the substrate exists in pieces, but
activation is incomplete end-to-end. The highest-leverage real
problems are:

- Lost type information during lowering / inlining
- Overloaded tape identity for bootstrap/self-hosting
- Mixed payload/cursor enum/view limitations
- CSS classifier/routing gaps
- Clone-heavy compile-time graph and type machinery

That is the right center of gravity.

## Priority corrections

AR is resequenced as follows:

1. **Fix identity first.** Split the tape discriminator path
   before more host/view migration.
2. **Make direct projection claims honest.** Treat "named struct
   projection" as unfinished until `Named` lowers into a real
   emitted ABI.
3. **Put proof gates on every activation claim.** Require
   `cargo expand` evidence for codegen activation,
   `cargo test --workspace` plus bootstrap roundtrip for
   correctness, and symbol-level `samply` diffs for every
   claimed perf win.
4. **Prioritize parser/control-flow and clone elimination over
   more scanner cleverness.** The repo's own profile story
   points more toward control-flow, compounds, and transient
   allocation than toward another round of regex
   micro-specialization.
