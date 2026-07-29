# W2 admission

Disposition: `ADMITTED_GIT_DURABLE`.

## Selected subjects

- parse-that W2 kernel boundary
  `282ccb6f6da15ef1e5b355b5588a23cc6a76ae7f`, tree
  `4e813a353251f86906baee44a9c4d28e743fe27b`;
- BBNF W2 carrier `28a91db51177495efb61cd1c664a85b37fdd9529`,
  tree `df6f1b063acfc49b3e0382fbd4a46892be7e5328`;
- W2 evidence `60ee1a51d694b5cf3fd76779336ef165e593129f`,
  tree `02688383ebb21aeac1904377bf1598eb412fa218`;
- hostile reviews `f46eb9497522ea97e164e8a1e4160784aa53eb1e`,
  tree `fa991843932b0b3f19332a8208e4fa287cf6519d`;
- terminal adjudication
  `3f44a40c1297a962c4a227433814864598f44ae1`, tree
  `64d9b46c3733768f2074a817b74e9ad689741e82`.

Rejected intermediate subjects remain historical ancestors only. In
particular, full-capability carrier `3fbfc1b3` and
`bbnf.analysis-facts.v2` are not admitted authority. Qualified comparisons
against `3faf768e` and `00967fb5` establish wire lineage only.

## Admitted vertical

W2 replaces the mutable semantic-state bag with one immutable consuming
pipeline:

```text
ModuleResult
  -> SourceModule + SourceMap + Revision
  -> Program + NodeRemap
  -> DependencySolution
  -> TypeSolution + executable certificates
  -> AnalysisFacts
  -> LoweringPlan
  -> ScalarEventPlan
  -> direct generated Rust
```

The paired parse-that kernel supplies borrowed source identity, consuming
`ModuleResult`, transactionally coupled cursor/event rollback, allocation-free
counting `NoSink`, typed resource failures, exact crate version `0.4.1`, and
`KERNEL_ABI == 2`.

The BBNF carrier supplies compact private arena ordinals, full-revision public
capabilities, checked foreign-revision rejection, deterministic remapping and
replay, dependency-first SCC analysis, bounded recursive fixed points,
first-order structural types with executable certificates, typed minimum-
consumption overflow, direct lowering, scalar-event planning, and direct
static Rust emission.

The admitted sink is full-consumption scalar events only. Generated controls
cover literal, regex, sequence, choice, repetition, records, transforms, EOF,
rollback, field identity, source identity, exact event ordering, retaining and
discarding parity, and typed resource failure. No runtime grammar graph, host
callback, or mutable earlier phase reaches the generated backend.

## Acceptance evidence

- 57 locked BBNF workspace/all-target/all-feature tests;
- five external identity-opacity compile-fail cases;
- strict Clippy, warning-denying rustdoc, scoped wasm32, formatting, and public
  nameability;
- 8 generated-scalar, 5 recursive, and 4 nominal-record external-control laws
  with strict Clippy;
- 13 colocated representation/remap/order/analysis unit laws;
- deterministic constraint/discovery permutations and four-state full-
  invalidation edit equality;
- independent 1,026- and 4,002-rule acyclic probes with exactly seven work
  units per rule;
- independent 512-rule recursive SCC probe and exact recursive work/pass caps;
- Draft 2020-12 meta-schema and real 39,934-byte complete-packet validation;
- exact reproduction of all 92 file-digest bindings and three grammar digests;
- 32 parse-that regex laws, 48 kernel laws, one serialized allocation law, and
  7 runtime-bridge laws;
- clean strict parse-that Clippy, rustdoc, scoped wasm32, and package gates;
- two independent hostile reviews: `ACCEPT`, no P0/P1/P2;
- fresh terminal adjudication: `ACCEPT`, no P0/P1/P2.

Reproducible package receipts:

- `parse_that-0.4.1.crate`: 15,270 bytes, SHA-256
  `76f1c1f107ea2125cb787e99c1f69c41a4acfb1588c55771f8218899c5846f94`;
- `bbnf-regex-0.1.0.crate`: 82,832 bytes, SHA-256
  `2e5e85ffb8f198665414b5d5744baa47cb3d1b625438684ab4ec1e077294a50b`.

The complete packet is `bbnf.compiled-module.v5`; analysis facts are
`bbnf.analysis-facts.v3`. The independently reproduced packet has SHA-256
`2521dc79b97d094f1ebd5ab086acccab737895a516ab2f5d5e247d20e68b4d56`,
and its standalone analysis facts have SHA-256
`1c833e8325d2ed036c0a19af5cfcb81426696ce625df6135353b55277fff6eca`.

## Explicit dispositions

- the scalar lexical-classifier prototype is `REJECT`,
  `STANDALONE_NONPRODUCTION_PROTOTYPE`, and prohibited from promotion;
- direct first-order algorithms remain the admitted oracle; no unwitnessed
  e-graph, CSP, HM, row, effect, or host-transform expansion is implied;
- individual stage packets are deterministic receipts, not standalone trusted
  deserializers;
- edits are stateful full invalidation, not incremental compilation;
- tape, DOM, path, typed materialization, Rust/ASM, universal grammar coverage,
  totality cutover, and SOTA performance remain outside W2;
- full-workspace wasm is not claimed because host-only benchmark dependencies
  are incompatible; selected libraries and the runtime bridge pass scoped
  wasm gates;
- `cargo package -p bbnf` is `BLOCKED_EXTERNAL_REGISTRY` because
  `bbnf-regex 0.1.0` is unpublished; the sibling package itself verifies.

## Next authority

W3 pre-freeze work is authorized. Held-out selection is not.

Before any freeze or selection, W3 must correct the missing W0 public-witness
matrix/stable-ID artifact, replace implicit rule-name entry selection with a
strict grammar-instance contract, admit the necessary action-free syntax
union, provide genuine data-driven Pratt semantics, close any required
parse-that transactional kernel delta, prove all public witnesses through one
generic external consumer, pass pre-freeze hostile review, and create the
immutable freeze receipt. Only then may the sealed held-out selection rule be
evaluated.
