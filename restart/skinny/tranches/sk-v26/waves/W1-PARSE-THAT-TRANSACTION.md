# W1 — Parse-that transactional kernel

Purpose: perfect the smallest Rust substrate the W0 BBNF crossing census proves
it actually needs before changing the larger compiler.

All work occurs in the W0-created parse-that worktree. Parse-that main remains
untouched.

## Selected prototype

One invocation-owned Rust kernel:

```text
checked Input -> Cursor/Span
              -> Run<EventSink | NoSink>
              -> literal | regex terminal | sequence | choice | repeat | EOF
              -> Matched | Mismatch | Fault
```

A checkpoint owns cursor state and exactly one append-only event/materialization
sink when that sink is present. `NoSink` is the zero-output participant. The W0
crossing census did not justify memoization, left recursion, recovery, cut,
mutable diagnostics, or user-context transactions, so W1 does not rebuild them.
Mismatch and typed faults roll back cursor and sink; a fault never tries a sibling
choice. Furthest absolute failure evidence is immutable outcome data, not a
transaction participant.

Full parse and prefix parse are different named operations.

The canonical artifact is a Rust crate package built from the isolated
parse-that worktree. TypeScript v1 remains unchanged and supplies shared
language-neutral law vectors; npm publication is not a W1 deliverable.

The packaged `parse_that` runtime has no dependency on `bbnf-regex`, BBNF,
e-graph, CSP, or `pprint`. `bbnf-regex` is the sole compile-time pattern-to-HIR-
to-automaton authority; it emits a checked immutable table plan consumed by
`parse_that::RegexTerminal`. Regex syntax, HIR, NFA/DFA builders, optimizer
internals, and wholesale regex re-exports do not cross the runtime package.

## Required comparisons

Compare:

1. parse-that v1 production kernel;
2. invocation prototype v4;
3. the new minimal kernel.

Use one direct, generated-shaped BBNF grammar-module control, the real
`bbnf-regex -> parse_that` table seam, and generated/adversarial law cases.
Measure:

- semantic equivalence;
- rollback completeness;
- progress/termination;
- allocation and copied bytes;
- compile/package size;
- recognize and retained-output throughput.

### Scope correction discovered during execution

W0 authorized product mutation in parse-that only and kept BBNF semantic source
frozen until the W1 package and crater were sealed. A genuinely generated module
cannot therefore be produced in W1 without either laundering a handwritten
fixture as generated output or violating the wave authorization.

`parse_that/tests/kernel/generated_shaped_control.rs` is explicitly an
isomorphic direct-control fixture, not generator output. It proves the selected
runtime surface; the standalone runtime-bridge contract proves that the checked
regex compiler tables execute through that surface. Genuine BBNF-emitted
literal/regex/sequence/choice/repeat/EOF control in both sink modes is the first
blocking W2 gate, before any broader semantic-spine implementation. Failure of
that gate reopens W1; it may not be routed around.

This correction narrows neither the final tranche close nor the required
crossing. It makes the worktree boundary honest.

## Excluded

- regex-engine redesign beyond retiring the dependency backedge and sealing one
  compile-time authority;
- SIMD;
- Value adoption;
- JSON/CSS domain parsers;
- public major version;
- broad combinator inventory;
- stale Value-specific arity unrolling;
- Rust CSS parser preservation;
- full-input padding by default.
- any checkpoint participant not required by `artifacts/W0-CROSSING.tsv`.

Excluding redesign does not defer ownership. W1 seals one canonical Rust regex
syntax/semantic authority for the selected vertical, shares its law vectors with
the unchanged TypeScript reference, and removes competing authority from the
selected crossing. Broader engine/performance work stays banked until a terminal
profile names it.

## Kill conditions

Retire the candidate if:

- any state leaks across a failed alternative;
- a safe span can express invalid bytes;
- progress can stall without a typed error;
- it cannot carry the generated BBNF control grammar;
- it adds complexity without a measured allocation or correctness advantage over
  v4.

## Conditional transposition

If the candidate survives, perform one clean-break parse-that transposition in the
isolated tree and delete the replaced kernel surface there. No aliases or dual
production paths. BBNF consumes it only after package and crater artifacts are
sealed.

## Exit

- all transaction laws pass;
- one Rust crate package installs from a clean package artifact;
- shared Rust/TypeScript law-vector coordinates are explicit;
- the direct generated-shaped control and real compile/runtime table seam are
  green, the full BBNF crater is classified path-by-path, and genuine generated
  output is sealed as the first blocking W2 gate; the legacy fleet cannot be
  green until W2 consumes the new API, so no W1 gate may pretend otherwise;
- public API receives tri-fold hostile review;
- raw performance samples satisfy the benchmark constitution or are honestly
  non-promoting.

Named evidence:

- `artifacts/W1-TRANSACTION-LAWS.json`;
- `artifacts/W1-REGEX-AUTHORITY.tsv`;
- `artifacts/W1-CRATER.tsv`;
- `artifacts/W1-PACKAGE.json`;
- `artifacts/W1-REPLAY.md`.
