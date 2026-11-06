# SK-V25 apotheosis registry

This registry supersedes judgment rows from SK-V24 without editing their historical
records. Machine facts remain in `EVIDENCE-CENSUS.md`. Each final section is the
output of two hostile perspectives followed by a fresh Sol adjudicator whose
recorded model metadata was independently checked.

## Parse-that Tranche B bank — final first-pass fold

Status: **RED; six-lock replacement and T/U prototypes authorized**.

| Bank row | Ruling | Corrected fact / route |
|---|---|---|
| global diagnostics | CORRECT | enablement and recovery collection are process-global; invocation context owns them |
| raw cross-source memo | CORRECT | raw execution bypasses the parse-entry epoch; remove/seal raw execution and own cells per call |
| nested recovery rollback | CORRECT | global last-diagnostic popping is not a transaction; checkpoint the run-local recovery log |
| state/result divergence | CORRECT with correction | failed `then` returns stale `"a"`; observer shows a different error state; one discriminated result replaces both |
| combinator runtime/type divergence | CORRECT | tuple projection, wrap/trim, negation, clone, and state mapping require exact runtime/type laws |
| non-ASCII dispatch | INCORRECT as framed | ASCII is documented; the defect is an unrestricted `Record<string>` that neither narrows nor rejects unsupported keys |
| EOF diagnostic | CORRECT | empty-input regex failure bypasses evidence merge |
| core SCC/bundle | CORRECT but incomplete | source core SCC has seven nodes, runtime projection six; a second five-node source/runtime SCC joins the root and domain parsers |
| loader identity | observation CORRECT; ESM-only route UNPROVEN | ESM/CJS constructors differ; supported consumer craters must choose policy |
| artifact skip | CORRECT | tests can pass without `dist`; clean build-pack-install gates replace symbol-only local checks |

Cross-cut corrections:

- Value coordination intent is current; Value 4 runtime coupling is absent.
- Rust is RED under locked resolution, unlocked stale regex tests, and clean
  resolution without untracked patches.
- `SpanParser` was removed only from TypeScript; Rust publicly exports and uses it.
- The no-x86 law is RED in parse-that and bbnf-lang.
- Parse-that Rust regex is the only full engine among the similarly named trees,
  but one-home generation and facade deletion remain unproved.
- Opaque-functional kernel, ESM-only publication, generic Tape/SoA, Pratt,
  WASM-common-core, and exact coordinate representations remain candidate families.

Fresh final-adjudication findings:

- The first invocation prototype is falsified: its checkpoint omits memo/LR and
  effect journals; memo cells store whole-run evidence, drop recovery effects, and
  overwrite caller commitment; one boolean cannot model scoped cut; public
  `Parser.define` exposes the supposedly hidden run; it has no LR algorithm,
  qualified coordinates, or full/prefix distinction.
- Rust safe APIs can reach invalid unchecked state. `Span` admits arbitrary byte
  bounds before unchecked string slicing; `PaddedView` is a debug-only witness;
  default state creates a slice from an allocation-free vector; generic erased
  context can be cast to the wrong private type; recursive `lazy` creates aliasing
  exclusive references through `UnsafeCell`.
- Rust `add_expected` fails to advance `furthest_offset`; diagnostics persist across
  invocations through thread-local storage; every parser-state construction copies
  the full input for padding; the `combinators` feature is inert.
- Regex anchors lower to unconditional epsilon, lazy priority is discarded by DFA
  longest-match, generated Unicode lowering is invalid, invalid hex escapes become
  NUL, and class lowering has a silent catch-all. Executable probes reproduced
  `foo$` on `foobar`, greedy `a.*?b`, malformed escape acceptance, Unicode byte
  corruption, and invalid-boundary panic.
- The committed bootstrap script is syntactically invalid. Handwritten and generated
  syntax routes are both live and semantically divergent.
- Cargo packages are acyclic, but the repository-provider graph cycles: parse-that
  requires bbnf-owned egraph/CSP patches while bbnf patches parse-that/regex.

### Canonical parse-that constitution

| Lock | One inseparable authority |
|---|---|
| PT-E execution | run, result, checkpoint, diagnostics, recovery, memo, LR, and scoped cut |
| PT-J JavaScript/TypeScript | combinator algebra, declarations, module graph, ESM package, artifacts |
| PT-C crossings | coordinate domains, semantic manifest, TS/Rust/WASM parity, unsafe-boundary proof |
| PT-K kernel | Rust parser/scanner ownership, memory safety, scalar/NEON, Cargo topology, no-x86 |
| PT-R regex | grammar, HIR, Unicode, automata tiers, anchors/laziness, generation/bootstrap |
| PT-V Value adoption | packed corpus experiment, migration, suitability, and measured uplift |

Artifact truth is a cross-cut gate and structure is a cross-lock proof plane; neither
is a separately sealable feature lock.

```text
artifact truth -> C0 semantic contract
artifact truth -> PT-E -> PT-J
artifact truth -> PT-K
C0 -> PT-E / PT-J / PT-K / PT-R
PT-E + PT-K -> PT-R
PT-E + PT-J + PT-K + PT-R -> C1 parity closure
PT-J + PT-R + C1 -> PT-V
all six -> GESTALT-P1 -> GESTALT-P2
```

C0 is the versioned schema/manifest gate; C1 is its proved implementation.

### Selected parse-that architecture

- TypeScript publishes immutable definition nodes through private construction and
  one ESM identity unless a named CJS consumer earns dual publication before cut.
  One private evaluator receives an invocation-owned run. `parse` is full-input,
  `parsePrefix` is named, and `parseOrThrow` projects the same result.
- Outcomes distinguish success, mismatch, committed failure, and recovery/budget
  failure. Checkpoints journal cursor/value/evidence, diagnostics/recovery,
  choice-frame depth, memo mutations, and LR head/stack/grow state. Cut seals the
  nearest frame. Memo cells replay entry-relative effects or reject effectful use.
- Public JS positions are qualified UTF-16 offsets; one checked map converts to
  UTF-8 bytes at crossings.
- Rust uses checked byte spans, typed run/arena ownership, invocation diagnostics,
  and safe immutable recursion. Remove padded copying first; restore a verified
  padded carrier only after a paired win. Remove x86 and the inert feature. Fold
  useful SpanParser scanner kinds into one scanner module; delete its duplicate
  parser surface and the Rust CSS grammar after consumer fixtures exist.
- `rust/regex` is the semantic engine. Direct tables/minimization survive
  provisionally; ambient egraph/CSP must win a bounded comparison. One BBNF grammar
  generates one syntax route; ordered/assertion/capture semantics use a preserving
  VM tier or fail explicitly rather than entering the DFA. Unicode tables are
  pinned and lowering is exhaustive.
- Bootstrap is acyclic: `bbnf seed -> generated regex parser -> parse-that -> full
  bbnf`, reproduced twice from empty output directories.

Tranche T owns PT-E/PT-J, C0, and TypeScript PT-C. Its close wave is GESTALT-T, not
GESTALT-P1. Tranche U owns standalone Rust/safety, scanner, regex, C1 parity, PT-V,
and both parse-that gestalts.

## BB-1/BB-2/BB-3 — final first-pass adjudication

Status: **RED; replacement prototype authorized**.

### Correct findings

1. `TypeDomain::join` is right-biased overwrite. It is neither commutative nor
   monotone and therefore is not a lattice join.
2. The solver surface admits nontermination: two contradictory ground constraints
   can alternate under an unbounded full-sweep loop. A current grammar-generation
   trigger remains unproven.
3. Type solutions can depend on constraint order. Both propagation results are
   ignored, unsolved cycles become `BoxedEnum`, and unsolved values become `Span`.
4. The current `TypeDesc` is a finite layout classifier, not HM-like inference. It
   has no variables, schemes, substitutions, functions, rows, occurs-check, or
   explicit recursion. Skinny declares some of these nouns but implements none of
   the corresponding inference.
5. Type obligations have no production reader. `FactAuthority` has one real
   construction and is bypassed by direct mutable-field reads throughout analysis
   and backends.
6. `GrammarIR` has exactly 38 public fields spanning program, options, caches,
   analysis, solver output, backend choices, registries, and diagnostics.
7. DAG identity is pointer-derived and becomes stale after mutation. Twenty-eight
   `GrammarIR` fields are skipped by serialization, so analyzed products cannot
   round-trip.
8. An enabled G3 e-graph rewrite changes `Alt(["a", epsilon])` to `"a"`; empty
   input succeeds before and fails after. Existing tests positively exercise the
   unsound union.
9. E-graph saturation reports are discarded, writeback can substitute epsilon or
   drop failed children, and TokenDispatch hides semantic children from congruence.
10. E-graph scheduling rebuilds CSPs and ignores errors. Strategy CSP drops hard
    constraints on budget/unsat fallbacks. Skinny's CSP hard-pins an answer already
    selected by a minimum-cost e-graph leaf.
11. Root and skinny are both invalid wholesale carriers: root owns broad behavior
    in a failed state bag; skinny owns useful arena/runtime experiments alongside
    placeholder type and decision machinery.

### Selected carrier

Use consuming stages with immutable fact snapshots:

```text
Program + SourceMap + revision digest
  -> DependencyGraph + SCC condensation
  -> TypeSolution + DiagnosticSet
  -> AnalysisFacts
  -> LoweringPlan
  -> backend artifacts
```

- `bbnf-program` owns immutable grammar nodes, stable arena `NodeId`, revision
  digest, and explicit `NodeRemap` after rewrites.
- `bbnf-solve` owns the canonical graph, deterministic worklist, invalidation,
  budgets, and reports; it does not own semantic domains.
- `bbnf-types` owns real semantic terms, variables, substitutions, schemes, rows,
  variants, functions, explicit recursive binders, occurs-check, provenance, and
  immutable `TypeSolution`.
- `bbnf-analysis` owns typed immutable products keyed by revision and node.
- `bbnf-lower` consumes the sealed products and returns `LoweringPlan`.
- Pipeline orchestration alone composes `Result<Output, DiagnosticSet>` stages;
  codegen cannot access mutable pre-analysis state.

`TypeDesc` may survive only as a renamed `LayoutDesc`. Semantic type and storage
layout are distinct domains.

### Prototype ladder

| Prototype | Falsifier |
|---|---|
| P0 laws | type-domain algebra, constraint permutations, conflicting-ground termination, and G3 empty-input differential fail on the incumbent |
| P1 immutable seed | one arena grammar has stable IDs, revision/remap, exact serialization, and byte-identical replay |
| P2 type kernel | schemes, rows, variants, tuples/lists/options/functions/spans/transforms, explicit recursion, occurs-check, principality, and permutation properties run |
| P3 graph solver | SCC worklist solves recursive equations; randomized order and incremental edits equal clean recomputation |
| P4 sealed stages | every stage value serializes with digests and named errors; no semantic field is skipped |
| P5 engine trials | one rewrite island and one genuine global choice compare direct, e-graph, and CSP algorithms on semantics and cost |
| P6 consumer | one recursive BBNF grammar reaches parse-that result types and Value before corpus-wide expansion |

### Organ disposition

- **Survive:** specifications and corpora, observable parser/codegen behavior,
  scalar layout vocabulary, source spans, host-function knowledge, benchmark/runtime
  kernels, generic CSP/e-graph libraries behind proof gates.
- **Transpose:** root grammar operators, skinny arena idiom, obligations into typed
  diagnostics, behavioral suites into differential oracles, DAG into stable graph,
  selected runtime/value organs into the staged product.
- **Delete after replacement proof:** current type domain/projector, pointer DAG,
  mutable analysis fields, `FactAuthority`, G3, silent `Span`/`BoxedEnum`/epsilon
  fallbacks, skinny fake substitution/obligations/e-graph/pinned CSP, and ambient
  optimizer/strategy uses that fail bounded P5 comparison.

## Structure/DAG adjudication

Status: **RED; canonical root shell and extractor prototype authorized**.

### Correct findings

1. Root, skinny, and WASM are overlapping planes. Skinny depends on root egraph and
   CSP crates and cannot replace it independently. Root itself is not hermetic.
2. Retain the root workspace/product identities, manifests, LSP, analysis,
   formatter, TypeScript backend, WASM source, and consumers; replace the mutable
   internals. Freeze skinny as a lab, transpose proved carriers, then delete the
   entire plane. Do not build a second vNext inside skinny.
3. Regex/scanner authority is split. Parse-that owns the only full regex engine;
   skinny `bbnf-regex` is a four-class heuristic and skinny `parse-that-regex` is a
   JSON/Unicode scanner. Root/skinny scanning also duplicates number, string,
   Unicode, and structural kernels.
4. Parse-that becomes the packaged regex/scanner home after differential proof.
5. No-x86 is RED across root source, Cargo, CI, emitters, generated outputs, and
   skinny report/comparator vocabulary. Five architecture-named NEON functions
   merely call scalar code and must be removed or implemented honestly.
6. Handmade god modules are confirmed: skinny report 11,863 lines, gate 5,974,
   lock baseline 5,283, root simple-runtime generator 4,343, skinny xtask 2,780.
   Generated 108k-line CSS is a provenance problem, not a file-splitting target.
7. Skinny SIMD also forms sand: tiny scalar files and five four-to-six-line false
   NEON wrappers lack independent contracts.
8. Forty production files contain inline tests, or forty-three including xtask.
   Core has 106 flat test files against a deep source tree. The playground has no
   typecheck, unit, or component gate.
9. The frontend graph has 95 nodes, 152 import occurrences, 145 unique edges, 88
   nodes reachable from `main.ts`, and one eight-node examples SCC. Move `Example`
   to `examples/model.ts`.
10. Empty `utils.css` and unused `lib/utils.ts` are vacuous. `useScrollMorph` and
    pass-through `PlaygroundControls` delete only after focused parity.
11. `EntryRuleSelector` is a missing live capability, not an orphan. The compiler
    must supply authoritative entry semantics; UI first/last heuristics are both
    invalid. Local HeaderRibbon behavior is not Glass HeaderRibbon behavior.
12. Playground dependencies are several majors stale and update only through
    packed-artifact transposition.
13. Generated provenance lacks source/semantic/generator/toolchain digests. The
    ordinary playground build does not verify WASM; four WASM copies differ.
14. WASM patches local Gorgeous, contrary to the hostile report, but locked metadata
    still wants mutation, so the boundary remains RED.
15. At adjudication there were 78 worktrees, 24 unique HEAD commits, and 60 dirty
    worktrees. No bulk cleanup is authorized.

Exact generated-file counts from loose path/content predicates and naïve frontend
orphan counts are **UNPROVEN**. Declarations and compiler roots are not runtime
orphans.

### Canonical structure

```text
root product shell
  ├─ syntax and grammar
  ├─ immutable program / graph / type / analysis / lowering phases
  ├─ runtime and backend
  ├─ provenanced generated artifacts
  ├─ WASM workspace member
  └─ frontend
       ├─ app
       ├─ features/landing
       ├─ features/docs
       ├─ features/playground
       └─ platform/wasm

parse-that package
  └─ sole regex, scanner, and parser-kernel authority

skinny lab (temporary, read-only product role)
  └─ carriers awaiting proof and transposition
```

The typed multigraph must distinguish package/features; Rust module/use/reexport/
include/cfg; TS runtime/type/reexport/dynamic imports and export conditions; Vue
script/template/provide/style edges; routes; tests; and grammar -> generator ->
artifact -> package -> copied/loaded consumer provenance. Containment, tests, and
provenance do not participate in dependency SCC projection.

### Structural dispositions

- **Delete now:** empty `playground/src/assets/styles/utils.css` and unreferenced
  `playground/src/lib/utils.ts` after rechecking concurrent owner edits.
- **Delete after focused visual/component parity:** `useScrollMorph`, identity
  `PlaygroundControls`, and false NEON wrappers.
- **Delete after carrier/provenance proof:** root `simd-scan`, skinny duplicate
  regex/scanner/SIMD packages, handwritten giant lab gates/reports, manual WASM
  copies, then the entire skinny plane.
- **Keep/transpose:** EntryRule selection, local HeaderRibbon behavior, compiler and
  WASM declarations, grammar/spec corpora, proved fixtures, useful skinny carriers,
  WASM source, route-owned frontend features, external isomorphic tests.
- **Do not delete yet:** generated outputs, worktrees, declarations, EntryRule
  selector, HeaderRibbon, or any whole plane.
