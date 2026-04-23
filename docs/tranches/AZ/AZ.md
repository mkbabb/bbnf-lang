# Tranche AZ — Direct-to-Struct Across the Fleet + Tape Dissolution

AZ is the transformational tranche. Every production grammar — JSON,
CSS L4, Sheets, and BBNF itself — derives its own native Rust struct
shape from `project_types` feeding `StructRegistry`, and parse output
lands directly into those structs. `crates/tape/` dissolves entirely:
its roles either vanish, fold into the struct tree, or migrate to
single-purpose test artefacts on the parity harness side.

AZ is not *tape activation followed by a later dissolution*. AZ is
activation-by-dissolution: the native struct replaces the tape on
every grammar in the same tranche, leaving no orthogonal substrate
behind.

## Thesis

Grammar-derived direct-to-struct is the single materialised form of a
parse. A typed `->` in a grammar rule is the declaration of a field
(or a compound shape) in the emitted struct; the IR type-inference
pass composes these into a closed struct graph; the emitter writes
straight into the graph without traversing an intermediate tape. No
role-5 fallback for "undeclared struct targets" — every grammar,
including BBNF's own, flows through `project_types` and lands in a
derived struct.

The `crates/tape/` substrate, in its five historical roles, dissolves
as follows:

1. **Intermediate during parse** — disappears. The emitter writes
   struct fields directly; there is no intermediate record stream.
2. **Materialised view source** — disappears. Views are direct
   projections of the struct graph; accessors are struct field reads.
3. **Debug substrate** — folds into a diagnostic serialiser on the
   struct graph. The `@debug` directive lowers to a struct-tree
   walker, not a tape replay.
4. **Parity oracle substrate** — migrates to parity harnesses only.
   Harnesses that compare bbnf output against sonic-rs, lightningcss,
   simdjson, cssparser, and serde_json compare struct-vs-struct (or
   struct-vs-harness-native), never tape-vs-struct.
5. **Undeclared-grammar fallback** — no such role exists after AZ.
   Every grammar has a `project_types` result; emptiness is a hard
   failure, not a soft fallback.

The struct IS the materialised form. There is no second
representation that the parser or the backend can route to.

## What this replaces

AZ subsumes the scope of the prior "grammar-derived tape activation"
plan (archived as `OLD-BA-absorbed.md` in this directory). That plan
activated `project_types` and `StructRegistry` across the fleet but
preserved the tape substrate as the materialised form, with views
layered on top. AZ keeps the activation scope intact but rejects the
preservation.

Concrete differences:

- Activation-only plan: populate `StructRegistry`, emit into tape,
  view layer projects tape records into typed accessors. Struct
  shape is a *view-time concept*; the tape remains canonical.
- AZ: populate `StructRegistry`, emit into struct fields, view layer
  reads struct fields directly. Struct shape is the *parse-time
  canonical form*; tape is deleted.

The AU-baseline throughput gates carry over unchanged (JSON twitter
≥ 1967 MB/s, CSS bootstrap ≥ 600 MB/s, full 17-entry matrix parity
with AU-baseline). AZ inherits these as its close conditions, not as
its opening budget. The opening budget is AY-II's close state —
whatever AY-II lands is the ceiling AZ cannot regress beneath.

## Invariants

1. **Typed materialisation across the fleet.** Every `->` in every
   production grammar reaches a struct field or a compound-opening
   call on the struct builder. An IR audit pass enforces 100%
   coverage and fails the build on miss. No grammar opts out.
2. **Single codegen path.** One emission path: struct builder calls.
   No combinator fallback. No tape fallback. No "conditional
   struct-or-tape" branch in the emitter. `feedback_no-orthogonal-codepaths`
   is load-bearing.
3. **Rich AST preserved.** Struct shapes carry the full
   lightningcss-level richness. Flattening a typed grammar rule for
   speed is not permitted — `feedback_preserve-rich-ast` is in force.
   CSS L4 `Length`, `Color`, `Dimension`, `Time`, `Resolution`,
   `Percentage`, `Angle` each get a typed Rust enum whose variants
   match the grammar's alternation structure and whose fidelity meets
   or exceeds lightningcss on its own fixtures.
4. **BBNF self-hosts on a derived struct.** BBNF's own grammar
   (`grammars/bbnf/bbnf.bbnf`) feeds `project_types` and produces a
   `BbnfAst` struct (or similarly-named root type) through the same
   pipeline every other grammar uses. The bootstrap does not special-
   case BBNF into a tape path.
5. **`crates/tape/` deleted at close.** By AZ's close commit, the
   `crates/tape/` directory does not exist in the tree.
   `cargo build -p bbnf --no-default-features` does not depend on a
   `bbnf-tape` crate. No downstream crate imports `bbnf_tape::*`.
6. **Measurement gates substrate.** Every wave's substrate lands at
   the same commit as a runtime call site that reads the new
   structure, plus a same-commit bench delta on the 17-entry matrix.
   AX invariant 13 is in force.
7. **No deferrals.** Every optimisation needed to hit an AZ gate
   lands in the wave that owns that gate; no carry-forward to a later
   wave or to the next tranche. `feedback_no-deferrals`.

## Hard gates (AU-baseline anchored)

AZ's close matrix is the AU-baseline 17-entry matrix from
`AU/FINAL.md`, evaluated on the struct-only parse path. AY-II's
close state is AZ's opening ceiling; AZ close must meet or exceed
AY-II close on every entry AND complete tape deletion.

**Throughput gates (parity recovery first, then exceedance):**

| Grammar / fixture | AU-baseline | AZ floor | AZ target |
|---|---:|---:|---:|
| JSON canada | 1231 MB/s | 1231 | 1500 |
| JSON citm | 2438 MB/s | 2438 | 2700 |
| JSON twitter | 1967 MB/s | 1967 | 2200 |
| CSS normalize | 735 MB/s | 735 | 850 |
| CSS bootstrap | 454 MB/s | 600 | 700 |
| CSS tailwind | 496 MB/s | 500 | 600 |
| Sheets parse_simple | 95 MB/s | 95 | 110 |

**Struct-only path gates:**

- `cargo build -p bbnf --no-default-features` succeeds without
  `crates/tape/` existing on disk.
- `rg --files crates/ | rg '^crates/tape/'` returns zero matches at
  AZ close.
- `rg 'use bbnf_tape' crates/` returns zero matches at AZ close.
- No `TapeRec`, `TapeBuilder`, `TapeCursor`, or `Columns` symbol is
  referenced by non-test code at AZ close.

**Coverage gates (structural):**

- IR audit pass reports 100% `->` coverage across all grammars.
- `StructRegistry` non-empty for every `Named` rule in JSON, CSS L4,
  Sheets, and BBNF.
- lightningcss typed-value parity: every `<length>`, `<color>`,
  `<angle>`, `<time>`, `<resolution>`, `<percentage>` rule in CSS L4
  returns a typed value byte-equivalent to lightningcss's through the
  parity harness on its own fixture corpus.

**Workspace gates:**

- Pass count ≥ 967, fail count ≤ 33, ignored count ≤ 30 on
  `cargo nextest run --workspace --profile ax-iter`.
- Parity harnesses (sonic-rs, lightningcss, simdjson OnDemand,
  cssparser, serde_json) green on every wave boundary.
