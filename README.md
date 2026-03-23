# <img src="extension/icons/bbnf-small.png" height="32" align="top" /> bbnf-lang

**Better Backus-Naur Form**—a monorepo for the BBNF grammar ecosystem.

BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

---

## Structure

```
rust/                   Rust workspace
  bbnf/                 BBNF grammar framework, IR lowering, codegen (lib)
  bbnf-ir/              Canonical grammar IR, bytecode compiler, interpreter
  bbnf-derive/          Proc-macro derive for BBNF
  bbnf-analysis/        LSP analysis engine (DocumentState, feature providers)
  lsp/                  Language Server Protocol server
wasm/                   bbnf-wasm crate (wasm-pack → playground, bytecode VM)
typescript/             TS library (@mkbabb/bbnf-lang)
prettier-plugin-bbnf/   Prettier plugin for .bbnf files
playground/             Vue 3 + Monaco playground (uses bbnf-wasm)
extension/              VS Code extension (LSP client)
grammar/                Example grammars + language specification
  css/                  CSS grammar family (value-unit, color, values, selectors, keyframes, stylesheet, css-tokens, css-stylesheet-pretty)
  lang/                 Language/format grammars (JSON, CSV, math, regex, EBNF, Google Sheets, etc.)
docs/                   Documentation (markdown, rendered by playground)
scripts/                Build automation scripts
data/                   Benchmark datasets
server/                 Compiled LSP binary (copied by Makefile)
.github/workflows/      CI + release pipeline
.vscode/                Launch configs, tasks, settings
```

## Language

BBNF extends [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
with features for practical parser generation: regex terminals, skip/next operators,
mapping functions, and an import system. See
[grammar/BBNF.md](grammar/BBNF.md) for the full specification.

Quick orientation:

```bbnf
(* Rules: name = expression ; *)
value = object | array | string | number | "true" | "false" | "null" ;

(* Regex terminals *)
number = /\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/ ;

(* EBNF operators: [] optional, {} repetition, () grouping *)
array = "[", [ value, { ",", value } ], "]" ;

(* Imports—selective imports auto-unfurl transitive dependencies *)
@import { number, integer } from "css-value-unit.bbnf" ;

(* Recovery—per-rule sync expression for multi-error parsing *)
@recover declaration /[;}]/ ;

(* Custom whitespace—overrides ?w to use comment-aware scanning *)
@ws /(?s)(?:\s|\/\*.*?\*\/)*/ ;
declaration = propertyName ?w ":" ?w valueSpan ;

(* Force-inline—splice rule body at every call site, no enum variant *)
@inline optSemicolon ;
```

### Recovery

`@recover rule syncExpr ;` annotates a rule with a synchronization expression used
for multi-error parsing. When the rule fails, the parser advances past input until
`syncExpr` matches, records a diagnostic, and continues. Any valid BBNF expression
(regex, alternation, concatenation, etc.) is valid as the sync expression.

The LSP provides semantic tokens, hover, completion, and diagnostics (undefined target
rule) for `@recover` directives.

### Imports

BBNF supports `@import` directives for composing grammars:

```bbnf
@import "other.bbnf" ;                        (* import all rules *)
@import { number, integer } from "lib.bbnf" ; (* selective import *)
```

Import directives may appear at any position in a file (before, between, or after rules).
Selective imports automatically bring transitive dependencies—importing `percentage`
also brings `number` and `percentageUnit` if `percentage` references them.
Circular imports are handled via Python-style partial initialization (a module's rules
are registered before recursing into its own imports).

Cmd+Click on import paths opens the referenced file. Diagnostics are
import-aware—imported rule names suppress "undefined rule" warnings.

### Whitespace

`@ws /regex/ ;` overrides the `?w` (optional whitespace) operator grammar-wide. By default, `?w` trims ASCII whitespace (`\s`). With `@ws`, it compiles to the given regex instead, enabling comment-aware whitespace without allocating a named rule's enum variant at each call site. CSS grammars use this to route `?w` through a SIMD-accelerated comment scanner.

### Inlining

`@inline ruleName ;` force-inlines a rule at every call site during IR compilation. The rule's body is substituted directly—no enum variant generated, no function emitted. Useful for small helper rules (optional semicolons, opaque spans) where the abstraction aids readability but the codegen overhead doesn't.

### Formatting

BBNF's `@pretty` directives drive pretty-printing in the playground and Prettier plugin.

### Arena Parsing

`#[parser(arena)]` on the derive macro generates a second set of parser methods that use `BumpArena<T>` for allocation instead of `Box<T>`. The arena path emits monolithic recursive functions—direct `match` dispatch on the first byte, inlined rule bodies, zero combinator construction overhead. Fresh arena and parser per parse; bulk deallocation on drop.

```rust
#[derive(Parser)]
#[parser(path = "json.bbnf", arena)]
struct JsonParser;

let arena = BumpArena::<JsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
let ast = JsonParser::value_arena()
    .parse_with_context(&input, &arena)
    .unwrap();
```

## Playground

Live at **[grammar.babb.dev](https://grammar.babb.dev)**.

### Editor

Monaco with full BBNF language support via WASM: hover, completion, go-to-definition,
semantic tokens, inlay hints (FIRST sets + nullable), code lens, code actions,
document symbols, folding, and selection ranges. Diagnostics update on every keystroke.

Four panes show different views of the grammar and input:

| Pane | What it shows |
|------|---------------|
| **Grammar** | BBNF grammar editor with live diagnostics |
| **Input** | Source text parsed against the grammar |
| **Parsed AST** | JSON AST produced by the parser |
| **Formatted** | Pretty-printed output via `@pretty` directives |

Formatting uses [gorgeous](https://github.com/mkbabb/gorgeous) (WASM)—AOT-generated formatters for built-in languages, a bytecode VM for custom grammars.

### Docs

Built-in documentation covering [`parse-that`](https://github.com/mkbabb/parse-that), BBNF, [`pprint`](https://github.com/mkbabb/pprint), [`gorgeous`](https://github.com/mkbabb/gorgeous), and
performance—rendered from Markdown with a sidebar nav grouped by section and syntax-highlighted code blocks.

## Sources, acknowledgements, &c.

- [Extended Backus-Naur form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) — ISO 14977. BBNF's ancestor.
- Wheeler, D. A. [Don't Use ISO 14977 EBNF](https://dwheeler.com/essays/dont-use-iso-14977-ebnf.html). — Motivation for BBNF's syntactic deviations.
- Aho, A. V., Lam, M. S., Sethi, R., & Ullman, J. D. (2006). *Compilers: Principles, Techniques, and Tools* (2nd ed.). Addison-Wesley. — Left recursion, left factoring, FIRST/FOLLOW sets.
- Tarjan, R. E. (1972). Depth-first search and linear graph algorithms. *SIAM Journal on Computing*. — SCC detection used for cycle analysis, FIRST-set propagation, and build ordering.
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). Microsoft. — The protocol implemented by `bbnf-lsp`.
- [`parse-that`](https://github.com/mkbabb/parse-that) — The parser combinator library that consumes BBNF grammars.
