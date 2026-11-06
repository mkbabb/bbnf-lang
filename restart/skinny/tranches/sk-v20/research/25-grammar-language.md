# BBNF-the-Language: Totality Sufficiency Audit

The binding precept under audit: *self-hosting must use language features; augment
BBNF if features are missing.* The corollary is that a host function (`-> fn(input)`)
or a hand-written emitter shape is a confession that the language lacks an expressive
primitive. This report censuses the language surface, maps which fleet grammars exercise
which constructs, locates every escape hatch, and proposes the concrete language
extensions that would let full-depth CSS L4 and full JSON be expressible WITHOUT a host
escape hatch.

## 1. The Language Surface (census from `grammar/bbnf/*.bbnf`)

The canonical self-definition is `grammar/bbnf/bbnf.bbnf` (89 lines) +
`expressions.bbnf` (37) + `types.bbnf` (7). The full construct set:

| Construct | Defined at | Lowers to |
|---|---|---|
| `->` projection / map | `bbnf.bbnf:42` (`mapped_factor`) | `MapExpr` tree (`map_expr.rs:17`) |
| value-expr DSL | `expressions.bbnf:6-34` | `MapExpr::{IntLit,FloatLit,BoolLit,StringLit,Input,InputProp,FnCall,BinOp,UnaryOp}` |
| `?w` / `?` / `*` / `+` modifiers | `bbnf.bbnf:38` | `OptionalWhitespace`, opt/star/plus IR nodes |
| `<<` / `>>` / `-` binary ops | `bbnf.bbnf:44` | `Skip` / `Next` / `Minus` (`node.rs:64-71`) |
| `@{ ... }` span capture | `bbnf.bbnf:33` | span-preserving wrapper |
| regex literals `/.../`  | `bbnf.bbnf:15` | DFA-compiled leaf |
| `@import` (named + bare) | `bbnf.bbnf:60` | module merge |
| `@host name : T` | `bbnf.bbnf:73` | `FnTable` entry (`types.rs:118`) |
| `@ws`, `@token`, `@pretty`, `@recover`, `@debug` | `bbnf.bbnf:65-72` | analysis directives |
| closures `\|params\| rhs` + calls | `bbnf.bbnf:30,51` | beta-reduced, *never reach IR* (`lower/mod.rs:38`) |
| type annotations `: T` | `expressions.bbnf:37`, `types.bbnf` | `TypeDesc` |

## 2. Dead and Near-Dead Constructs (over-built surface)

The language ADVERTISES more than the fleet uses. Three constructs are implemented
end-to-end yet exercised by ZERO production grammars:

- **Closures / GrammarCall / multi-param grammar functions.** The grammar defines
  rule-level closures (`bbnf.bbnf:51`), call-args (`bbnf.bbnf:27,30`), and the lowerer
  implements compile-time beta-reduction with a substitution environment
  (`lower/mod.rs:34-60`, `value_expr/simple_kinds.rs:133-160`). A repo-wide scan of all
  `.bbnf` files (`grammar/`, `docs/tranches/`) finds **no closure definition and no
  grammar-function call anywhere**. This is a large, untested feature surface that the
  "universal abstraction" comment (`lower/mod.rs:36`) oversells — it is universal in
  principle and unused in fact.
- **`@host` directive.** Defined (`bbnf.bbnf:73`), parsed into the schema
  (`grammar/schema/model.rs:95`, `build.rs:206`), stored in a `FnTable`
  (`types.rs:118`) — yet **no `.bbnf` file contains a live `@host` directive** (only the
  self-definition's own rule). The actual host mechanism in use is the *inline*
  `-> fn(input)` form, which bypasses `@host` declaration entirely. Two parallel
  host pathways exist where the precept demands one.
- **`@debug`.** Used by no fleet grammar. `@recover` is used only in `css/pretty.bbnf`
  (the L2 reference), not by any L4 production grammar.

The recommendation is not to patch these but to decide their fate: either a fleet grammar
must genuinely need grammar-functions (e.g. a parameterized `csv<sep>` or a shared
`delimited(open, close, item)` combinator factored across CSS function bodies), or the
closure surface, `@host` directive, and `@debug` should be deleted under KISS/DRY. Today
they are speculative generality — the exact anti-pattern the precepts forbid.

## 3. The Live Escape Hatches (where host papers over missing features)

Only **two** live host calls exist across the entire fleet:

- `grammar/css/l4/color.bbnf:190` — `hex = "#", /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) : u32`
- `grammar/json/json.bbnf:10` — `string = /.../ -> decode_json_string_to_arena(input) : String`

Both are **byte-level transcoders**, and both are irreducible in the current language for
the same reason: `MapExpr` (`map_expr.rs:17-40`) has arithmetic (`BinOp`/`UnaryOp` over
`Add..Shr`), comparison, and opaque `FnCall`, but **no primitive that iterates over the
matched bytes** — no indexing, no slicing, no char/byte access, no fold/scan, no loop.
`InputProp` (`map_expr.rs:29`) can read `input.len` but cannot read `input[i]`.
`parse_hex_color` (`runtime/css_l4/host.rs:17-48`) is a hex-digit fold; JSON's decoder is
an escape-sequence scan over `\uXXXX`/`\n`/etc. Neither is expressible because the
value-expr DSL is a *scalar expression language*, not a *byte-processing language*.

This is honest at the grammar level (the maps are explicit) but the precept names hex-color
as a smell to assess, and the verdict is: **it IS a smell, but a narrow and defensible
one** — it is the minimal irreducible kernel, not lazy offloading. The selectors, calc(),
var()/env(), nested functions, `!important`, custom properties, color functions, and
color-mix recursion are ALL expressed natively (see §4). The host surface is two
transcoders, not a JSON/CSS semantics dump.

The real overfit is one level down, in the **emitter**: `backend/rust/emitter/shapes/string.rs`
is a grammar-specific "string shape" that hard-branches on whether the rule's decoder is
JSON's `decode_json_string_to_arena` vs Sheets' span-typed string
(`string.rs:79-95`). A function *name* (`decode_json_string_to_arena`,
`parse_hex_color`) is load-bearing in generic codegen, and `xtask/src/regen_css.rs:1336`
splices the literal path `crate::runtime::css_l4::host::parse_hex_color`. The host
*function* is correctly relocated out of the generic crate (Lock 14 honored — `host.rs:1-13`),
but the emitter still *recognizes specific decoder names* to choose a code shape. That is a
Lock-14 leak at the codegen layer: the generic emitter should dispatch on a declared
*capability* of the host function (does it return the inner body or the full span?), not on
its identity.

## 4. Full-Depth CSS L4 / JSON: What's Already Expressible

Adversarially checked, the claim "CSS needs host escape hatches for full depth" is FALSE
for almost everything:

- **Nested functions**: `calc()`/`min()`/`max()`/`clamp()` recurse natively
  (`values.bbnf:39-55`); `var()`/`env()` with fallback (`values.bbnf:61-63`);
  `color-mix()` recurses through `color` (`color.bbnf:299-303`).
- **Full selector grammar**: `:is()`/`:where()`/`:not()`/`:has()` with recursive
  `selectorList`/`relativeSelectorList`, An+B microsyntax, namespaces, attribute matchers,
  pseudo-elements — all native (`selectors.bbnf:54-107`), zero host calls.
- **`!important`**: native `importantSuffix = ("!" ?w , "important") ?` (`properties.bbnf:23`).
- **Custom properties**: native `customPropertyDecl` (`properties.bbnf:206`),
  `dashed = /--[\w-]+/` (`values.bbnf:25`).

So full-depth CSS L4 does **not** need new host hatches. What it documents as missing is
*typed-aggregate codegen*, not language syntax: An+B's `(i32,i32)` pair
(`selectors.bbnf:43-45`), per-variant at-rule bodies (`stylesheet.bbnf:38-40`), and
keyframe-stop percentage capture (`stylesheet.bbnf:25-27`) all sit behind `u8`
discriminant tags awaiting aggregate-composition codegen, not new grammar.

## 5. The Genuine Language Gaps (with syntax sketches)

These are the features whose absence forces host calls or `-> input : Span` degradation:

### G1 — Named-tuple / aggregate type annotations
`type_annotation` (`expressions.bbnf:37`) accepts a single `type_name` token only.
Google-Sheets `cell_ref` declares its shape only in a comment because the grammar cannot
say `(row: u32, col: u32, abs_row: bool, abs_col: bool)`
(`google-sheets.bbnf:54-63`), and degrades to `-> input : Span`. Proposal — extend
`type_name` to a structural type:
```
cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> (col: u32, row: u32, abs_col: bool, abs_row: bool) ;
```
with codegen composing the named tuple from regex captures.

### G2 — Variant-tagged enum projection (sum types)
An `Alt` of heterogeneous branches cannot project to a tagged union without `u8`-tag
hackery. `range_end` (`google-sheets.bbnf:73-75`) and `atRule` (`stylesheet.bbnf:38`)
want `(Cell | Column | Row)` and per-variant payloads. Proposal — a named-variant `->`:
```
range_end = cell_ref -> Cell | columnRef -> Column | rowRef -> Row : RangeEnd ;
```

### G3 — A byte-folding primitive (eliminate transcoder host calls)
The single feature that would retire BOTH live host calls. Add a fold/scan combinator to
the value-expr DSL that consumes the matched span byte-wise. Sketch — a per-byte fold with
an accumulator and a `hex_digit`/`escape` primitive set:
```
hex = "#" , /[0-9a-fA-F]{3,8}/ -> fold(input.bytes, 0u32, |acc, b| acc << 4 | hex_digit(b)) : u32 ;
```
This requires `MapExpr` to gain `Index`/`ByteAt`, a `Fold { seq, init, body }` node, and a
small set of certified lexical primitives (`hex_digit`, `unescape`). It keeps the kernel in
the language (auditable, backend-agnostic) instead of in opaque Rust.

### G4 — Parameterized lexical classes / charset algebra
`@token` (`directives/token.rs`) only *names* a rule as a token for the LSP; it is not a
parameterized character-class constructor. CSS repeats `/[0-9a-fA-F]/`,
`/\s+/`, escape-aware idents by hand. The `-` set-difference operator exists in IR
(`node.rs:71`, used in `ebnf.bbnf:22`, `regex.bbnf:5`) but there is no first-class charset
type. Proposal — a charclass binding usable inside regex and as a leaf:
```
@charclass hexdigit = [0-9a-fA-F] ;
hex = "#" , hexdigit{3,8} ;
```

### G5 — Declared host capability (close the emitter Lock-14 leak)
Rather than the emitter branching on function *names* (`string.rs:79-95`), `@host`
should declare a capability the emitter dispatches on:
```
@host parse_hex_color : u32 #scalar ;
@host decode_json_string : String #decoded-body ;   // returns inner payload, not span
```
This also resolves §2's dual-pathway problem: `@host` becomes the single declaration site,
and inline `-> fn(input)` resolves against it instead of constructing a parallel `FnCall`.

## 6. Verdict

BBNF-the-language is **broadly sufficient** for the structural depth of CSS L4 and JSON —
selectors, nested functions, `!important`, custom properties, and recursive color all live
in the grammar with no host crutch. The honest gaps are narrow: byte-level transcoding
(G3), aggregate/sum-type projection (G1/G2), and charset algebra (G4). The language is
simultaneously **over-built** (dead closures, dead `@host` directive, dead `@debug`) and
**under-built** at the value layer (no byte primitive). The most consequential single
extension is G3's byte-fold, which would retire the only two live host calls and move their
logic into the auditable, backend-agnostic IR. The most consequential cleanup is deciding
the fate of the unused closure surface before it ossifies into permanent speculative
generality.
