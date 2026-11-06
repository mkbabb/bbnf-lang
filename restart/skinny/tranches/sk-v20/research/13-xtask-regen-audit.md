# xtask regen pipeline audit

## Scope

Five source files under `xtask/src/` (7 482 lines total) and nine TOML
projection files under `xtask/runtime-projections/`.  Analysis is
static only.

---

## File census

```
xtask/src/lib.rs                    13 lines  — pub module re-exports only
xtask/src/main.rs                  280 lines  — CLI dispatch + check-runtime loop
xtask/src/regen.rs                 879 lines  — grammar-driven IR pipeline regen
xtask/src/regen_css.rs           1 967 lines  — CSS L4 runtime projection emitter
xtask/src/regen_simple_runtime.rs 4 343 lines — 8-grammar runtime projection emitter
```

---

## Generic infrastructure vs per-grammar accretion

### What is genuinely generic

`regen.rs` is entirely grammar-agnostic.  It reads
`[workspace.metadata.bbnf.grammars]`, validates features against the
`KNOWN_FEATURES` allow-list, drives `bbnf::pipeline::compile_paths_request`
plus `bbnf::generate::generate_all` for each entry, and writes
`crates/core/src/grammar/generated/<ident>.rs` plus a `.registry.json`
sidecar.  No grammar name appears in the logic beyond what flows through
`GrammarEntry`.  `GrammarRegistrySidecar` at line 319 and the
sidecar-path helpers are consumed by both projection files; they are
correctly shared.  This is the model the rest of xtask should follow.

### `regen_css.rs` — a single-grammar fork

The entire 1 967-line file is dedicated to `css_l4`.

`load_projection` at lines 278–299 hard-asserts four CSS-specific string
literals that can never be satisfied by any other grammar:

```rust
if projection.grammar != "css_l4"           { bail!("regen-css only accepts grammar = `css_l4`"); }
if projection.runtime_module != "crate::runtime::css_l4" { bail!(...) }
if projection.parser_type != "CssL4Parser"  { bail!(...) }
if projection.output_dir != "crates/core/src/runtime/css_l4" { bail!(...) }
```

`emit_mod` at line 660 unconditionally emits `pub mod host;\n`, the
declaration for a hand-authored CSS shim.  The comment acknowledges
`host.rs` is not a projection output, but instead of making `host_modules`
a TOML field, the line is hard-wired.  The projection-set validator at
`check` (lines 211–220) then injects `host.rs` into the expected path set
as a special case to avoid a false alarm.  Both sites are CSS-specific
escape hatches in code that claims to be a generic emitter.

`emit_builder` at lines 694–1447 is the worst site.  It contains a
`match frame.as_str()` at lines 697–751 that keys on eight CSS domain
names (`StyleSheet`, `StyleRule`, `MediaRule`, `KeyframesRule`,
`KeyframeBlock`, `GenericAtRule`, `HexColor`, `DirPseudo`) with literal
`OpenFrame::*` initialiser bodies, and a second match on two color names
(`ColorFunction`, `ColorMix`).  Every arm `bail!`s on an unrecognised
frame — meaning a non-CSS projection that happened to route through this
code would fail at runtime, not at TOML parse time.  Below the route
resolution, the entire `CssStructBuilder` struct definition, its
checkpoint type, every `impl` block, and all the `push_leaf_with_f64`,
`push_leaf_with_str`, `push_branch_tag` bodies are emitted as a
2 600-line format string with `crate::runtime::css_l4::*` absolute paths
on lines 781–973.  These are not template tokens: they are literal CSS
paths that the `runtime_template` substitution machinery cannot touch.

`emit_value` at line 452 hard-imports
`crate::runtime::css_l4::arena::{CssDeclListId, CssRuleListId,
CssSelectorListId, CssValueListId}` and at line 526 hard-re-exports
`crate::runtime::css_l4::arena::CssKeyframeListId`.  Neither path comes
from the projection TOML; both would need to be added as structured list
fields before `emit_value` could serve a different grammar.

`emit_document` at line 1449 and `emit_view` at line 1823 take no
`projection` argument at all and embed CSS type names (`CssDocument`,
`StyleSheet`, `CssArena`, `CssView`, `CssFocus`, `CssWalkItem`, etc.)
verbatim.

`emit_parse_with` at lines 1910–1967 reads several projection fields
correctly, but line 1921 still emits `crate::runtime::css_l4::{builder}`
rather than deriving the runtime path from `projection.runtime_module`.
The grammar is used as a literal import path segment rather than
substituted through a token.

### `regen_simple_runtime.rs` — four grammar forks in one file

The file dispatches on a `RuntimeStyle` enum (line 119):

```rust
match projection.runtime_style {
    RuntimeStyle::Simple         => emit_simple_runtime(…),
    RuntimeStyle::TypedFormula   => emit_typed_formula_runtime(…),
    RuntimeStyle::TypedBbnf      => emit_typed_bbnf_runtime(…),
    RuntimeStyle::TypedJson      => emit_typed_json_runtime(…),
}
```

Each branch has its own fleet of `emit_*` functions: `TypedFormula`
owns 11 functions for `google_sheets`; `TypedJson` owns 8 for `json`;
`TypedBbnf` owns 8 for `bbnf`; `Simple` owns 7 for the remaining five
grammars.  Totalling 38 distinct emit functions across 4 343 lines.

**`TypedFormula` branch (google_sheets-specific).**
`emit_typed_formula_parse_with` at line 1431 hard-codes:

```rust
use crate::grammar::generated::google_sheets::{…};
use crate::path::markers::Sheets;
use crate::runtime::google_sheets::__P__StructBuilder;
```

The `__MODULE__` token is present elsewhere in this style's templates but
is not used here; `google_sheets` and `Sheets` are bare literals.
`emit_typed_formula_document_canonical` at line 1148 encodes the entire
Google Sheets formula serialisation — `LET(`, `LAMBDA(`, `TRUE`/`FALSE`,
eight numeric error lexemes (`#N/A`, `#VALUE!`, `#REF!`, `#DIV/0!`,
`#NULL!`, `#NAME?`, `#NUM!`, `#ERROR!`, `#SPILL!`), and 28
`__P__CompoundKind::*` arms (lines 1199–1336) corresponding to
`google_sheets.toml`'s `kind.variants` list.  These are formula
AST-node names (`ParenExpr`, `FuncCall`, `LetCall`, `LambdaCall`,
`ArrayLiteral`, `RangeRef`, etc.) embedded as Rust identifiers in a
code template.

**`TypedJson` branch (json-specific).**
`emit_typed_json_parse_with` at line 2335 hard-codes
`crate::grammar::generated::json::{…}` as a literal import, then patches
around its own `__MODULE__` token with a post-hoc string replace:

```rust
.replace("runtime::__MODULE__", "runtime::json")  // line 2422
```

The same pattern at line 2521.  The `runtime_template` substitution system
(defined at line 434) already handles `__MODULE__` → `projection.runtime_module`;
adding a `.replace()` patch on the result is a workaround layered on top
of a substitution system that was supposed to make workarounds unnecessary.

**`TypedBbnf` branch (bbnf-specific).**
`emit_typed_bbnf_parse_with` at line 3424 uses a double post-hoc replace
(lines 3517–3518):

```rust
.replace("generated::__MODULE__", "generated::bbnf")
.replace("runtime::__MODULE__", "runtime::bbnf")
```

`emit_typed_bbnf_serialize` at line 3545 emits a full BBNF grammar
serialiser with hardcoded compound-kind arms: `Grammar`, `GrammarItem`,
`Lhs`, `Rhs`, `Directive`, `Rule`, `Alternation`, `Concatenation`,
`BinaryFactor`, `MappedFactor`, `Factor`, `Term`, `Closure`, `CallArg`,
etc.  These are BBNF grammar AST node names embedded as Rust identifiers
that must match `bbnf.toml`'s `kind.variants` list exactly or the emitted
code will not compile.

**`regen_command` at lines 372–384** is a match table that must be
extended for every new grammar:

```rust
"math" => "regen-math", "csv" => "regen-csv", "bbnf" => "regen-bbnf",
"bnf" => "regen-bnf", "ebnf" => "regen-ebnf",
"css_pretty" => "regen-css-pretty",
"google_sheets" => "regen-google-sheets", "json" => "regen-json",
other => bail!("unknown simple root runtime projection `{other}`"),
```

### `main.rs` — per-grammar fork at the CLI level

`Cmd` at lines 31–96 has nine named variants (`RegenCss`, `RegenMath`,
`RegenCsv`, `RegenBbnf`, `RegenBnf`, `RegenEbnf`, `RegenCssPretty`,
`RegenGoogleSheets`, `RegenJson`).  `ROOT_RUNTIME_GRAMMARS` at lines
120–130 lists all nine as string literals.  `check_runtime_grammar` at
lines 159–167 is a match arm that forks on `"css_l4"` vs the other eight.
Adding a tenth grammar requires touching all three sites.

### Infrastructure duplication

Five functions are independently defined in both `regen_css.rs` and
`regen_simple_runtime.rs` with identical or near-identical bodies and no
path to call each other (neither is exported from `lib.rs`):

| function | css line | simple line |
|---|---|---|
| `workspace_root` | 257 | 154 |
| `validate_workspace_grammar` | 302 | 186 |
| `load_registry` | 332 | 303 |
| `format_rust` | 418 | 359 |
| `write_if_changed` | 424 | 386 |

---

## What one grammar-driven regen entry point looks like

The `Cmd` enum collapses to two variants: `Regen` (IR pipeline,
manifest-driven, already correct) and `RegenRuntime` with an optional
`--grammar` flag.  `RegenRuntime` reads the TOML filenames from
`xtask/runtime-projections/` to build its grammar list; `ROOT_RUNTIME_GRAMMARS`
and the `check_runtime_grammar` match arm disappear.

The five duplicated infrastructure helpers move to `regen.rs` and are
called from both projection paths.

`regen_css.rs` dissolves.  Its `RuntimeProjection` schema is either merged
into the one defined in `regen_simple_runtime.rs` or extracted to a shared
`xtask::projection` module.  The CSS load-projection hard-assert block
disappears; the TOML's `grammar` field is the identity, not a guard.

`RuntimeStyle` dissolves.  The four style branches unify under a single
pass that iterates the TOML's structured fields.  The value-type layout
that distinguishes `TypedJson` from `TypedFormula` from `TypedBbnf` is
encoded in TOML under new fields (e.g. `value_variants`, `arena_handles`).
A grammar adding a new layout adds TOML keys; it does not add a Rust enum
arm and a new fleet of emit functions.

The CSS builder remains the hardest case.  `emit_builder` encodes
CSS-specific frame semantics (selector lists, keyframe blocks,
color-mix trees, hex-color parsing) that no current projection language
expresses.  Two clean options: (a) `builder.rs` becomes a hand-authored
checked-in file; xtask's check gate verifies it is present but does not
emit it; or (b) the builder TOML fields (`builder.aggregate_routes`,
`builder.declaration_rules`, `builder.color_routes`, etc.) — which the
css_l4.toml already carries — are used to drive a generic `emit_builder`
that emits only the route dispatch table, leaving frame struct definitions
and leaf handlers to a hand-authored base that xtask calls via
`include_str!`.  Either option removes the grammar-specific code from the
emitter.

`pub mod host;\n` in `emit_mod` becomes `host_modules = ["host"]` in the
projection TOML, iterated generically alongside `module.modules`.  The
projection-set validator no longer needs to inject `host.rs` as a
special-case path.

Post-hoc `.replace(...)` patches in `emit_typed_json_parse_with`,
`emit_typed_bbnf_parse_with`, and `emit_typed_formula_parse_with` are
deleted.  The three `parse_with` templates merge into one, driven by
`runtime_module` and `grammar` from the projection TOML, which
`runtime_template` already handles via `__MODULE__` and related tokens.

---

## Violations summary

1. `regen_css.rs` is a single-grammar fork: four hard-assert string
   comparisons at load time (css:278–299), eight CSS frame-name literals in
   `emit_builder` (css:697–751), and two CSS absolute-path imports
   hard-coded into `emit_value` (css:452, 526) that the projection TOML
   does not drive.

2. `regen_simple_runtime.rs` contains three grammar-specific runtime style
   branches (`TypedFormula`/`google_sheets`, `TypedJson`/`json`,
   `TypedBbnf`/`bbnf`) with hard-coded grammar names in import strings and
   post-hoc `.replace()` workarounds on top of the substitution system that
   already handles those replacements generically (simple:2422, 2521, 3517,
   3518).

3. Five infrastructure helpers are duplicated verbatim across two files with
   no shared call site.

4. `main.rs` has three per-grammar touch points for every new grammar: the
   `Cmd` enum (main:31–96), `ROOT_RUNTIME_GRAMMARS` (main:120–130), and
   `check_runtime_grammar` (main:159–167).

5. `emit_typed_formula_document_canonical` (simple:1148) encodes the entire
   Google Sheets formula serialiser as a code template whose compound-kind
   arms are the `google_sheets.toml` variant list rendered as Rust
   identifiers — meaning TOML and template are silently coupled with no
   structural enforcement.
