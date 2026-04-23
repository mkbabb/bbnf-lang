# Tranche AZ-II — Supplementary Research

External grounding and design detail specific to AZ-II's scope:
BBNF self-hosting bootstrap cutover, byte-equal reproducibility
harness, tape deletion risk analysis, and the `bbnf-tape-mini`
escape design.

Consumed alongside `AZ-II.md` and the forthcoming
`BOOTSTRAP-CUTOVER.md` (authored in AZ-II.W0).

The research on direct-to-struct thesis foundations (simdjson
ondemand, sonic-rs pointer, lightningcss derive, parent-pointer
study, struct-shape sketches for the data grammars) lives in
`docs/tranches/AZ-I/RESEARCH.md`. AZ-II extends it to BBNF.

## 1. BBNF as a derived struct

BBNF's grammar describes BBNF itself. `project_types` applied to
`grammar/bbnf/bbnf.bbnf` therefore produces a struct graph whose
shape is the BBNF AST. The derived root type is `BbnfAst`, with
fields drawn from the grammar's top-level productions:

```rust
struct BbnfAst<'a> {
    imports: &'a [Import<'a>],
    directives: &'a [Directive<'a>],
    rules: &'a [Rule<'a>],
    comments: &'a [Comment<'a>],
}

struct Rule<'a> {
    name: Ident<'a>,
    params: &'a [Param<'a>],
    return_type: Option<TypeExpr<'a>>,
    body: Expr<'a>,
}

enum Expr<'a> {
    Alt(&'a [Expr<'a>]),
    Seq(&'a [Expr<'a>]),
    Call { target: Ident<'a>, args: &'a [Expr<'a>] },
    Regex(&'a RegexPattern<'a>),
    Ident(Ident<'a>),
    Literal(&'a str),
    Repeat { inner: &'a Expr<'a>, min: u32, max: Option<u32> },
    Seq2 { a: &'a Expr<'a>, b: &'a Expr<'a> },
    Alt2 { a: &'a Expr<'a>, b: &'a Expr<'a> },
    // ... per grammar alternation
}

enum Directive<'a> {
    Host(&'a HostDirective<'a>),
    Debug(&'a DebugDirective<'a>),
    // ... per grammar alternation
}

struct RegexPattern<'a> { source: &'a str, hir: &'a RegexHir<'a> }
```

This shape mirrors the compiler's existing in-memory IR surface.
The cutover test is that `BbnfAst` parsed via the derived struct
path is byte-equal to the compiler's current internal representation
on every grammar in the corpus.

Each field's element type is itself a derived struct. The
derivation is mechanical: `project_types` produces the graph; the
emitter materialises a Rust type per registered `StructLayout`.

## 2. Bootstrap cutover mechanics

BBNF's parser is produced by `bbnf-derive`. `bbnf-derive` is a
proc-macro that expands `#[bbnf_grammar = "grammar/bbnf/bbnf.bbnf"]`
into a parser specialised to that grammar. The parser, when run on
any BBNF grammar file (including `bbnf.bbnf` itself), produces an
IR representation the compiler consumes.

Pre-AZ-II flow (inherited from AZ-I close):

1. `bbnf-derive` expands the grammar into a parser that writes
   into a tape.
2. The compiler's IR loader reads the tape and builds the in-memory
   IR.
3. The in-memory IR feeds `project_types`, `StructRegistry`, and
   every other pass.

Post-AZ-II flow:

1. `bbnf-derive` expands the grammar into a parser that writes
   into a derived `BbnfAst` struct.
2. The compiler's IR loader reads `BbnfAst` directly and builds
   the in-memory IR.
3. Identical from step 3 onward.

The cutover problem: the pre-AZ-II parser is what builds
`bbnf-derive`, which is what builds the post-AZ-II parser. A naive
cutover (swap the emission target in `bbnf-derive`'s code
generator) produces a post-AZ-II `bbnf-derive` that can only be
built by a pre-AZ-II compiler, breaking reproducibility.

The two-stage bootstrap threads the cutover:

### Stage A (pre-AZ-II → AZ-II-candidate)

The pre-AZ-II compiler (tape-based, inherited from AZ-I close)
builds the AZ-II-candidate compiler (struct-based). The candidate's
`bbnf-derive` now emits struct-writing parsers, but the candidate
itself was built from a tape-writing parser.

Concretely:

1. Check out AZ-II W1 branch.
2. `cargo clean && cargo build -p bbnf` using the AZ-I close
   toolchain.
3. The resulting `bbnf-derive` binary emits struct-writing parsers.
4. Run `bbnf-derive` on every `.bbnf` fixture (`grammar/*/*.bbnf`
   + `tests/fixtures/*.bbnf`) and capture the parsed output to
   `docs/benchmarks/AZ-II/W1/stage-a-output/<fixture>/`.

### Stage B (candidate → final)

The AZ-II-candidate compiler rebuilds itself from its own source.
The final compiler is built from a struct-writing parser and
produces struct-writing parsers. The tape has been unwired in both
directions.

Concretely:

1. `cargo clean && cargo build -p bbnf` using the W1-candidate
   toolchain.
2. The resulting `bbnf-derive` binary emits struct-writing parsers
   (same as Stage A).
3. Run `bbnf-derive` on the same fixture corpus; capture to
   `docs/benchmarks/AZ-II/W2/stage-b-output/<fixture>/`.

### Byte-equal reproducibility check

`diff -r docs/benchmarks/AZ-II/W1/stage-a-output/
 docs/benchmarks/AZ-II/W2/stage-b-output/` on W2 close. Zero byte
differences is the W2 hard gate. Any divergence on any fixture
triggers the reversal path.

The check is run against the parser's serialised output — the
struct graph rendered to a canonical form — not the compiler
binary's own bytes. Compiler-binary byte-equality is not required
(and is sensitive to toolchain + build-host variation); parser-
output byte-equality is the meaningful invariant because it is what
downstream consumers observe.

## 3. Drift source enumeration

Three semantic-drift sources have been identified; each has a
mitigation plan detailed in W0's `BOOTSTRAP-CUTOVER.md`.

### AST ordering

The derived struct may order fields differently than the tape's
cursor traversal. For example, the tape's depth-first-open order
may place a `Rule.params` before `Rule.name` in visit order, while
the struct's declaration order places `name` first.

**Fix.** The struct-building emitter follows the grammar's
declaration order (top-to-bottom within a production, left-to-right
within a sequence). The grammar's order is deterministic and
stable; the tape's order is derived from the same grammar but via
traversal, so the two agree in practice. W0 encodes this as an
explicit contract: struct-build order = grammar-declaration order.

### Trivia handling

The tape preserved certain trivia (comments, whitespace) via a
`Columns` side-channel. The derived struct's `comments` field holds
an explicit `Comment` array, but trivia attached to specific nodes
(e.g., trailing comment on a rule) requires explicit per-node
fields.

**Fix.** W0 enumerates every trivia class in `grammar/bbnf/bbnf.bbnf`
and specifies which struct field preserves each. The preservation
contract is visible at the type level — `Rule.trailing_comment:
Option<&'a str>` is a different signature than `Rule { ... }` with
trivia in a side-channel. A W1 test confirms every trivia
attachment round-trips through the struct path.

### Numeric formatting

f64 precision roundtrip. BBNF does not have many numeric leaves
(repeat counts are u32; regex character-class bounds are u32; the
only f64 is in the rare `@cost` directive), but any drift would
produce non-byte-equal output.

**Fix.** The derived struct carries the source span for every
numeric leaf, so exact-byte recovery is available if the display
form drifts. Stage A and Stage B both render from the source span,
not from a parsed-then-formatted numeric.

## 4. `bbnf-tape-mini` escape design

If W2 byte-equal fails on W2 close, AZ-II invokes the escape
clause: the tape crate shrinks to `bbnf-tape-mini` rather than
being deleted outright, and full tape deletion is deferred to a
follow-on tranche.

### Retention candidates

The minimum surface `bbnf-derive` requires from the tape crate, if
the BBNF bootstrap remains on tape:

- `TapeRec` — the 16-byte record struct.
- `TapeBuilder` — the builder surface the emitter writes into.
- `TapeCursor` — the reader surface the IR loader walks.
- `Columns` — the side-channel trivia storage.
- A minimal `Visitor` trait implementation for IR lowering.

### Pruning candidates

The tape-crate modules that have no BBNF-bootstrap consumer and
can be removed:

- `structural_scan/` — SIMD-scan path for bulk JSON input; BBNF's
  grammar files are small enough that the scan's amortisation does
  not help.
- `dta/` — the DTA interpreter, scoped to JSON/Sheets emission.
- `psi/` — the PSI cursor, scoped to materialised views.
- `dedup/` — the deduplicator, scoped to JSON's key dedup.
- `finaliser/` — the finaliser pass, scoped to data-grammar
  closure.
- `driver/`, `profile/`, `packed/` — infrastructure for modes not
  used in the BBNF bootstrap.
- `decoders/`, `kind/` — typed-payload decoding scoped to data
  grammars.

Expected shrink: from ~17 modules to ~4-5. The retained crate is
named `crates/bbnf-tape-mini/` (sibling to `crates/tape/`; the
rename happens at W3 escape-path close) and is a direct dependency
of `crates/bbnf_derive/` only.

### Follow-on tranche

Full tape deletion (including `bbnf-tape-mini`) routes to `AZ-III`
(or is absorbed into BA.W0 as a sub-task if the volume is small).
The follow-on tranche owns the targeted BBNF cutover once the
drift source is resolved — typically this means resolving the
specific byte-equal failure identified in W2.

The escape design is preservation of forward progress: AZ-II still
ships three-data-grammar direct-to-struct (via the AZ-I inheritance)
+ partial BBNF migration (the classifier work + `project_types`
closure on BBNF lands even in the escape path) + a shrunken tape
crate. The escape is not a full rollback of AZ-II's scope; only
the W2/W3 steps that depended on byte-equal are deferred.

## 5. Tape deletion risk analysis

The W3 tape-deletion step is the single largest LOC delta in
AZ-II. Risk surfaces:

### Downstream consumers missed at W3

Any crate that currently imports `bbnf_tape::*` and is not on the
W3 rewire list risks breaking the workspace build. The mitigation
is the pre-W3 grep check: `rg 'use bbnf_tape' crates/ --type rust`
run on the W3 opening commit produces the exhaustive consumer
list; every consumer is addressed before W3 closes.

Known consumers (on AZ-II W2 close, after Stage B is green):

- `crates/bbnf_derive/` — rewired to struct path in W1/W2.
- `crates/core/src/runtime/bbnf/` — rewired to struct path in W1/W2.
- `crates/core/src/backend/driver/` — view layer; rewired in W3.
- `crates/pprint/` — pretty-printer may consume tape records for
  debug output; rewired in W3.
- `@debug` directive lowerer (under `crates/core/src/backend/`) —
  migrates from tape-replay to struct-tree walker in W3.
- Parity harnesses under `tests/` — rewired to struct-vs-native
  comparison in W3.

### Breaking external tools

Any out-of-tree tool that imports `bbnf_tape::*` will break. The
mitigation is documentation in `FINAL.md` of the tape-crate
deletion and the migration path for external consumers. (bbnf's
user surface does not expose tape types; external consumers would
have had to import via an unsupported path.)

### CI check re-introduction

A future commit might accidentally re-introduce a `bbnf_tape`
import (via IDE auto-import or a revert). Mitigation: the W3
post-commit CI check `rg 'use bbnf_tape' crates/ --type rust`
returns zero matches; any non-zero result fails the build.

## 6. External reference — yyjson, sonic-rs, and the `bbnf-tape-mini` precedent

The shrink pattern AZ-II's escape invokes has precedent. simdjson
shipped a tape API for ~3 years before introducing ondemand; the
tape remained supported but was no longer the primary surface.
simdjson-rs and sonic-rs inherited the tape indirectly through
their simdjson dependencies but eventually removed it in favour of
ondemand-only architectures.

yyjson's position is informative: it never had a tape, and its
author explicitly cites cache-locality and ILP as the reasons
simdjson's tape underperforms on modern hardware. yyjson's direct
tree allocation is what AZ-II's struct path converges to for BBNF.

The `bbnf-tape-mini` escape, if invoked, is a precedent-aligned
retention pattern: the tape crate shrinks to its minimum irreducible
surface for a single-consumer bootstrap, rather than being retained
in full for speculative future use. If the escape is invoked, its
subsequent deletion in the follow-on tranche follows simdjson's
pattern of "tape shrinks, then tape disappears" over two releases.

## 7. Parity harness recode (W3 scope)

Pre-AZ-II parity harnesses compare tape records against external
library output. Post-AZ-II they compare struct graphs. The recode
is mechanical but extensive.

### Harnesses affected

- `tests/json_parity_sonic.rs` (already recoded in AZ-I.W2;
  no-op in AZ-II)
- `tests/json_parity_simdjson.rs` (already recoded in AZ-I.W2)
- `tests/json_parity_serde.rs` (already recoded in AZ-I.W2)
- `tests/sheets_parity.rs` (already recoded in AZ-I.W2)
- `tests/css_parity_lightningcss.rs` (already recoded in AZ-I.W3)
- `tests/css_parity_cssparser.rs` (already recoded in AZ-I.W3)
- `tests/bbnf_self_parity.rs` (new, lands in AZ-II.W1) — compares
  `BbnfAst` parsed via struct path against the hand-maintained
  in-memory IR surface.
- `tests/bbnf_bootstrap_reproducibility.rs` (new, lands in
  AZ-II.W2) — runs the Stage A / Stage B byte-equal check as a
  repeatable test, not just a one-shot W2 gate.

### Permanent invariant

The bootstrap reproducibility test
(`bbnf_bootstrap_reproducibility.rs`) lands in W2 and remains as a
CI gate post-AZ-II. Every commit post-W2 runs the Stage A / Stage B
diff; any commit that breaks byte-equal fails the build. This is
the mechanism that prevents a future tranche from accidentally
re-introducing tape-equivalent drift.

## 8. Cross-reference — how AZ-II's work threads into BA

BA (pointer queries on the struct tree) opens on AZ-II close. The
AZ-II contract that BA consumes:

- **Four grammars on struct-only path.** BA's queries apply
  uniformly; no grammar-specific tape-vs-struct branching.
- **Parent-pointer decision open.** AZ-I RESEARCH.md §3 flagged
  the parent-pointer-vs-root-traversal decision for BA.W0. AZ-II
  does not resolve it; it propagates the struct tree to BBNF and
  leaves the decision surface intact. BA.W0 measures on all four
  grammars and decides.
- **Permanent struct-shape invariant.** The `BbnfAst`, `JsonValue`,
  `StyleSheet`, `Cell` shapes are stable at AZ-II close; BA may
  annotate them with parent pointers or adjacent-field pointers
  without perturbing the core shape.

## Anti-precedents reaffirmed

1. **Era V DTA/PSI substrate-first pattern.** AZ-II closes the last
   tape-crate consumer. The substrate and consumer collapse in the
   same tranche (W1 substrate, W2 consumer, W3 deletion). No
   substrate outlives its consumer.
2. **AW-IV declared-exceedance-with-zero-exceedance.** The
   byte-equal gate is binary: either every fixture matches or the
   wave reverts. No partial declarations.
3. **AO phase-0 substrate-without-consumer.** Stage A (W1) ships
   the candidate compiler as its consumer; Stage B (W2) ships
   reproducibility as the close gate. No consumer deferred to a
   later tranche.
4. **AM.1 EmissionTier lattice.** One decision surface after
   AZ-II close: struct. No tape fallback.
5. **AX.W1.A / AX.W1.B hand-coded values.** `BbnfAst` is
   grammar-derived; no hand-maintained parallel representation.
