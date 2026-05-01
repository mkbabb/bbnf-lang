# <img src="extension/icons/bbnf-small.png" height="32" align="top" /> bbnf-lang

**Better Backus-Naur Form**, the grammar-derived compiler fleet.

A `.bbnf` grammar lowers through a shared IR substrate (`crates/ir`) into
checked-in Rust source under `crates/core/src/grammar/generated/<ident>.rs`,
plus a bytecode VM for the playground. The same IR feeds both backends; AOT
codegen runs at build time via `cargo xtask regen`. Sibling crates
[`parse-that`](https://github.com/mkbabb/parse-that) and
[`pprint`](https://github.com/mkbabb/pprint) carry the parser-combinator
substrate and the gorgeous auto-formatter.

---

## Quick start

```sh
cargo xtask regen          # regenerate every grammar in [workspace.metadata.bbnf.grammars]
cargo xtask regen --check  # CI / pre-commit drift gate
cargo iter-check           # workspace check on ax-iter (excludes heavy-link crates)
cargo iter-test            # workspace nextest on ax-iter
make build                 # release LSP + VS Code extension bundle
make build-wasm            # WASM module into playground/src/wasm/
```

Bench surface: `cargo bench-json`, `bench-css`, `bench-bbnf`, `bench-sheets`,
`bench-compile`, or `cargo bench-all` (all divan, ay-final profile). Iteration
analogues live under `bench-iter-*`.

## Architecture

`docs/GESTALT.md` is the senior-engineer onboarding read.
`docs/codegen-paths.md` walks the seventeen-pass IR pipeline and the AOT vs VM
divergence. `docs/instructions/PROFILING.md` covers samply discipline and the
bench tier matrix.

## Workspace

Twelve members live under `Cargo.toml:[workspace]`:

| Crate | Role |
|---|---|
| `crates/core` | Grammar framework, lowering façade, generated grammar source |
| `crates/ir` | Canonical `GrammarIR`, seventeen IR operations, bytecode compiler + VM |
| `crates/analysis` | LSP analysis engine (DocumentState, feature providers) |
| `crates/lsp` | Language Server Protocol server (`bbnf-lsp` binary) |
| `crates/ser` | Tape-aware serialization helpers |
| `crates/gorgeous` | Auto-formatter front-end (consumes `pprint`) |
| `crates/bootstrap` | BBNF self-host scaffolding |
| `crates/egraph` | Generalized e-graph substrate |
| `crates/egraph-derive` | `derive(Language)` for e-graph IRs |
| `crates/csp-solver` | Generalized CSP solver |
| `crates/simd-scan` | Hand-rolled SIMD scanners (delim + whitespace) |
| `xtask` | Build-time codegen entrypoint (`cargo xtask regen`) |

`wasm/` builds a WASM module for the playground (excluded from the workspace
per `Cargo.toml:exclude`). Top-level `extension/`, `playground/`, `grammar/`,
`docs/`, `scripts/`, `data/`, and `server/` carry the editor extension, web
playground, grammar sources, documentation, dev scripts, benchmark corpora,
and the compiled LSP artefact.

## Language

BBNF extends [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form)
for practical parser generation: regex terminals, skip/next operators, typed
rules via `->`, `@import` modules, recovery directives, and pretty-printing
hooks.

```bbnf
(* Rules: name = expression ; *)
value = object | array | string | number | "true" | "false" | "null" ;

(* Regex terminals *)
number = /\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/ ;

(* EBNF operators: [] optional, {} repetition, () grouping *)
array = "[", [ value, { ",", value } ], "]" ;

(* Selective imports auto-unfurl transitive dependencies *)
@import { number, integer } from "css-value-unit.bbnf" ;

(* Per-rule sync expression for multi-error parsing *)
@recover declaration /[;}]/ ;

(* Override ?w (optional whitespace) for comment-aware scanning *)
@ws /(?s)(?:\s|\/\*.*?\*\/)*/ ;

(* Splice rule body at every call site; no enum variant *)
@inline optSemicolon ;

(* Lexical token: fusion-inlined + span eligible; variant preserved *)
@token selectorSpan ;
```

Grammar sources live under `grammar/`: `bbnf/` (self-host), `json/`, `css/l4/`,
`css/pretty.bbnf`, `google-sheets/`, `ebnf/`, `bnf/`, and `misc/` (csv, math,
regex, emoji, g4). Production grammars enumerated in
`Cargo.toml:[workspace.metadata.bbnf.grammars]`.

### Directives

`@recover rule syncExpr ;` records a diagnostic and advances past `syncExpr`
when the rule fails; any BBNF expression is valid as the sync.

`@import "other.bbnf" ;` or `@import { rule } from "lib.bbnf" ;` compose
grammars. Selective imports drag transitive dependencies; circular imports
resolve via Python-style partial initialization.

`@ws /regex/ ;` overrides the `?w` operator grammar-wide. CSS grammars route
`?w` through the SIMD comment scanner under `crates/simd-scan/`.

`@inline ruleName ;` substitutes the body at every call site, eliding the
enum variant. `@token ruleName ;` fusion-inlines but preserves the variant
for `@pretty` reference. `@no_collapse ruleName ;` preserves structural
identity in the generated AST.

`@debug ruleName ;` emits trace output across codegen paths; the bytecode VM
uses `Op::DebugBreak` with stepping and breakpoint support. The VS Code
extension wires this to the Debug Adapter Protocol.

`@pretty` directives drive pretty-printing in the playground and Prettier
integration through gorgeous.

### Codegen entry

Generated parsers live at `crates/core/src/grammar/generated/<ident>.rs`,
written by `cargo xtask regen` and consumed via `include!`. The runtime
entrypoint per grammar is:

```rust
let doc = JsonParser::parse(&input)?;  // -> JsonDocument<'_>
```

`<Grammar>Document<'_>` projects directly into the typed value API; no proc-
macro expansion at consumer sites. The historical `#[derive(Parser)]` /
`bbnf_derive` proc-macro path was retired at B2.W2 in favour of checked-in
generation. See `docs/codegen-paths.md` §1 for the full nine-grammar matrix.

## Playground

Live at **[grammar.babb.dev](https://grammar.babb.dev)**.

Monaco with BBNF language support via WASM: hover, completion, go-to-
definition, semantic tokens, inlay hints (FIRST sets, nullable), code lens,
code actions, document symbols, folding, selection ranges. Diagnostics update
on every keystroke.

| Pane | Content |
|---|---|
| Grammar | BBNF editor with live diagnostics |
| Input | Source text parsed against the grammar |
| Parsed AST | JSON projection of the parsed value |
| Formatted | `@pretty`-driven output via gorgeous (WASM) |
| Debug | Step through parse execution with breakpoints + call stack |

Documentation rendered alongside covers
[`parse-that`](https://github.com/mkbabb/parse-that), BBNF,
[`pprint`](https://github.com/mkbabb/pprint),
[`gorgeous`](https://github.com/mkbabb/gorgeous), and performance.

## Sources, acknowledgements, &c.

- [Extended Backus-Naur form](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form). ISO 14977. BBNF's ancestor.
- Wheeler, D. A. [Don't Use ISO 14977 EBNF](https://dwheeler.com/essays/dont-use-iso-14977-ebnf.html). Motivation for BBNF's syntactic deviations.
- Aho, A. V., Lam, M. S., Sethi, R., & Ullman, J. D. (2006). *Compilers: Principles, Techniques, and Tools* (2nd ed.). Addison-Wesley. Left recursion, left factoring, FIRST/FOLLOW sets.
- Tarjan, R. E. (1972). Depth-first search and linear graph algorithms. *SIAM Journal on Computing*. SCC detection for cycle analysis, FIRST-set propagation, and build ordering.
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/). Microsoft. The protocol implemented by `bbnf-lsp`.
- [`parse-that`](https://github.com/mkbabb/parse-that). Parser-combinator substrate consuming BBNF grammars.
- [`pprint`](https://github.com/mkbabb/pprint). Auto-formatter substrate underlying gorgeous.
