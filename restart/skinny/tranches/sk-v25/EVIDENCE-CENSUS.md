# SK-V25 measured evidence census

This census separates commands and source facts from inherited verdicts. The
forthcoming apotheosis registries may supersede judgments; they may not rewrite
these observations.

## Repository state

| Repository | Measured state at census | Coordination disposition |
|---|---|---|
| bbnf-lang | `af15f63e0`, ahead of origin; owner-modified `playground/vite.config.ts` and untracked `playground/test-results/` | campaign writes stay disjoint until ownership is reconciled |
| parse-that | `ef10d5b`, equal to origin; untracked precepts, Tranche B research bank, Rust configuration, and compiler-crash report | Tranche B remains a bank, not execution authority; no deletion or relabeling |
| value.js | `c654824e`, ahead with active tranche-u documentation and developer-script changes | inspect and dogfood through explicit handoff only |
| keyframes.js | `a59d3a22`, behind origin with more than 200 dirty paths | read-only; no implementation landing |
| glass-ui | moving dirty HEAD during census | idiom reference only; re-census before any coordinated change |

## Parse-that machine facts

Commands were run from `/Users/mkbabb/Programming/parse-that`.

- `npm test` passed 124 of 124 tests in 13 files.
- `npm run build` passed. Its shared packrat output was 40,576 bytes.
- `npm run proof:all` passed, including a measured JSON regression of 10.2%
  beneath the existing 15% threshold. Threshold passage is not a favorable
  performance judgment.
- A clean checkout without `dist/` passed 122 tests and skipped the two artifact
  assertions. The test command therefore does not prove distributable artifacts.
- `cargo test --workspace` failed at
  `rust/parse_that/tests/regex/parser_test.rs:2`: the test imports the absent
  `parse_that::regex::parser` module.
- The untracked `rust/.cargo/config.toml` patches `csp-solver` 0.6 while the regex
  crate requests 0.1. An ordinary Cargo invocation rewrote six lockfile lines;
  that incidental diff was removed before continuing.
- Rust still exposes `SpanParser` and a complete CSS parser after their
  TypeScript counterparts were removed.
- The no-x86 law is RED in Rust configuration, CI, `quote_parity.rs`,
  `number_simd.rs`, and explanatory documentation.
- TypeScript runtime imports contain 18 nodes and 45 edges. The principal SCC is
  `debug -> lazy -> leaf -> packrat -> parser -> state -> utils -> debug`; a
  second SCC joins the root entrypoint to the CSV/JSON parser barrel.
- Rust and TypeScript tests are external but not source-isomorphic; Rust also has
  production-file test modules.

The executable bank probes at
`parse-that/docs/tranches/B/prototypes/reproduce-vnext-bank.mjs` and
`runtime-type-divergence.ts` establish:

1. diagnostics enablement and collection leak between runs;
2. raw memoization can return the first source's value for a second source;
3. nested recovery does not restore the complete checkpoint;
4. parser-instance observer state disagrees with the returned run state; the
   bank's exact `undefined` claim is false because the stale value is `"a"`;
5. runtime and declarations disagree for tuple projection, wrapping, trimming,
   negation, cloning, and state mapping;
6. unsupported non-ASCII dispatch keys are accepted by the public type;
7. EOF failure can omit the expected diagnostic;
8. ESM and CommonJS entrypoints instantiate distinct parser identities while
   root and core ESM imports share identity.

## Constellation correction

Value 4 has no current package or source dependency on parse-that. Its active CSS
parser is handwritten in `value.js/src/css/grammar.ts`; commits `164343c1` and
`f024d385` removed and relocated the old parsing path. The defensible split is:

- coordination and future-adoption intent: **confirmed**;
- current Value 4 runtime coupling: **incorrect**;
- future parse-that runtime coupling: **unproven and unimplemented**.

The present consumer SCC is Value <-> Keyframes <-> Glass. The desired provider
direction is parse-that -> Value -> Keyframes -> Glass and needs packaged-artifact
proof, not workspace-source imports.

## Bbnf-lang machine facts

- Root, skinny, and wasm Cargo package graphs are acyclic, but they are three
  overlapping product planes with duplicated compiler, IR, and scanning concepts.
- `GrammarIR` carries roughly 37 public mutable fields. Passes, DAG metadata,
  e-graph metadata, types, and lowering facts form a state-flow cycle around it.
- `TypeDomain::join` overwrites one unequal solved type with the other. The result
  depends on operand and visitation order, so it is not a lattice join.
- `project_types` ignores both `propagate()` results and silently maps unsolved
  variables to `TypeDesc::Span` in several extraction paths.
- The public `TypeDesc` is finite shape propagation: it has no type variables,
  schemes, function types, rows, substitutions, recursive types, or principled
  unification. Describing it as HM-like inference is incorrect.
- `FactAuthority` is not yet the exclusive source of downstream facts; consumers
  still read mutable `GrammarIR` fields directly.
- The e-graph CSP scheduler rebuilds a CSP inside equality-saturation iterations,
  ignores propagation errors, and has an all-dirty fallback. A direct worklist
  comparison is missing.
- Root still contains x86 configuration, CI, dispatch, and emitter paths, contrary
  to the Apple/aarch64/NEON-only campaign law.
- Forty-one production Rust files contain inline test modules. The 106 core tests
  are mostly flat rather than source-isomorphic.
- The playground import graph has 88 nodes and 149 edges. `useExamples.ts`, the
  examples barrel, and example modules form a cycle because the example model is
  owned by a composable that imports its consumers.
- Playground dependencies target Glass 3, Keyframes 2.2, Value 0.10, and
  parse-that 0.8.2. CI has no playground type, unit, component, or browser gate.
- Candidate orphan components `EntryRuleSelector` and `useScrollMorph` require
  edge and behavior proof before deletion. Entry-rule selection currently has
  first-versus-last semantics in different paths.

## Born-RED consequences

1. Parse-that cannot call its Rust plane green until the checked workspace builds
   without lockfile mutation.
2. Tests cannot certify a package while missing artifacts are skipped.
3. No type-system tranche may call finite shape propagation HM inference.
4. No solved type may be extracted from an ignored solver error or unnamed default.
5. No current Value coupling may justify parse-that API work; the three-arm
   handwritten/generated/hybrid dogfood experiment must create the evidence.
6. No module move is complete without a deterministic edge diff and an external,
   source-isomorphic test disposition.
