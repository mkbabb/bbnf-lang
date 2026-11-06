# W1 — Parse-that transactional kernel

Purpose: perfect the smallest Rust substrate the W0 BBNF crossing census proves
it actually needs before changing the larger compiler.

All work occurs in the W0-created parse-that worktree. Parse-that main remains
untouched.

## Selected prototype

One invocation-owned Rust kernel:

```text
checked Input -> Cursor/Span
              -> Run<Participants>
              -> literal | regex terminal | sequence | choice | repeat | EOF
              -> Success | Mismatch | CommittedFailure | ResourceFailure
```

A checkpoint always owns cursor state. Other state participates through one
minimal checkpoint/commit/rollback protocol. W0 must justify every participant;
unsupported historical facilities are retired rather than silently rebuilt.
Candidate participants are:

- semantic value stack;
- expected/furthest diagnostics;
- recovery journal;
- memo mutations;
- left-recursion head/stack/growth state;
- scoped cut/choice frames;
- user-context transaction state.

Full parse and prefix parse are different named operations.

The canonical artifact is a Rust crate package built from the isolated
parse-that worktree. TypeScript v1 remains unchanged and supplies shared
language-neutral law vectors; npm publication is not a W1 deliverable.

## Required comparisons

Compare:

1. parse-that v1 production kernel;
2. invocation prototype v4;
3. the new minimal kernel.

Use one generated BBNF grammar-module control plus generated/adversarial law
cases. Measure:

- semantic equivalence;
- rollback completeness;
- progress/termination;
- allocation and copied bytes;
- compile/package size;
- recognize and retained-output throughput.

## Excluded

- regex-engine redesign;
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
- memo replay changes diagnostics/value/cut semantics;
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
- BBNF crater/regeneration dry run is green;
- public API receives tri-fold hostile review;
- raw performance samples satisfy the benchmark constitution or are honestly
  non-promoting.

Named evidence:

- `artifacts/W1-TRANSACTION-LAWS.json`;
- `artifacts/W1-REGEX-AUTHORITY.tsv`;
- `artifacts/W1-CRATER.tsv`;
- `artifacts/W1-PACKAGE.json`;
- `artifacts/W1-REPLAY.md`.
