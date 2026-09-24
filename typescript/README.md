<!-- SERVED MODEL: claude-opus-5-5 -->
# @mkbabb/bbnf-lang (TypeScript)

The TypeScript face of BBNF: the grammar front end (parser, `@import` modules, analysis) and **the
staged emitter**, which compiles a grammar ahead of time into a plain ES module with no runtime
dependency. It is the TypeScript output of record for value.js: value.js generates its CSS parser
with `bbnf gen` and ships the generated module. The Rust `TsEmitter` (`crates/core/src/backend/ts`)
belongs to the Rust program and is not this package's output (value.js COHESION §0ck 4).

The behaviour of every face is fixed by one document, [`VALUE-SEMANTICS.md`](./VALUE-SEMANTICS.md),
and checked against its conformance corpus (`test/conformance/`).

## One emitter, three faces

`src/emit.ts` is the only code generator. Each rule becomes JavaScript functions: a value mode that
builds the value and runs the action, and a recognize mode that only finds where the match ends,
used wherever the grammar discards a value. An ordered choice is routed by a `switch` on the first
code unit (FIRST sets that respect regex flags, a non-ASCII route and an end-of-input route). A
single-class run is a code-unit loop. A reference is a direct call. Memoization is off.

| Face | When | How |
|---|---|---|
| `bbnf gen` | build time | writes `<out>.js` + `<out>.d.ts`; the module is checked in |
| `compile(ast, options)` | runtime, grammar known only then | evaluates the same emitted text once |
| `BBNFToParser(text)` | parse-that users | wraps the compiled rules as parse-that 2.x `Parser`s (`toParser`) |

There is no interpreter: all three run the emitted functions.

## `bbnf gen`

```sh
bbnf gen src/css/grammar/css.bbnf --actions src/css/bbnf/actions.ts --out src/css/bbnf/generated/css.js \
    [--export actions] [--entries colorTop,valueTop] [--max-depth 256]
bbnf gen … --check   # writes nothing; exit 1 when either file differs from a fresh generation
```

- The grammar's `@import`s are read through the host (node) reader.
- The **action kinds** come from the consumer's own action table (`--export`, default `actions`,
  else the default export), where each entry is `{ kind, fn }`. The generated `Actions` type and the
  table therefore cannot drift apart silently.
- Both files start with `// sha256(grammar ⊕ emitter) <hex>`. The hash covers every grammar module
  read and the emitter's own source. `--check` compares bytes, so any grammar edit or emitter change
  shows up as drift. Put it in CI.
- Output is deterministic: the same inputs give the same bytes.

The module exports `RULE_NAMES`, `ENTRY_NAMES`, `ACTION_KINDS`, `FAIL` (`Symbol.for("@mkbabb/bbnf-lang/FAIL")`)
and `createParser(actions)`:

```ts
import { createParser, FAIL } from "./generated/css.js";
const parser = createParser(actions);      // binds each action once; refuses a stale table
const v = parser.entries.colorTop("red");  // the value, or FAIL: no result object
const end = parser.rules.colorTop(s, i);   // the end offset or -1; then parser.value()
```

The `.d.ts` types `Actions` from the grammar's kinds, so a missing action or an action of the wrong
kind is a compile error. `createParser` also checks the table at runtime.

## Actions

An action is `{ kind: "map", fn(value) }`, `{ kind: "span", fn(value, start, end) }` or
`{ kind: "text", fn(text) }`. A `text` rule is only recognized, never built.

- **Actions are pure and total.** Recognize mode skips them wherever the grammar discards a value
  (`a >> b`'s `a`, `a << b`'s `b`, `a - b`'s `b`, a `text` rule's body), and the routing may try an
  alternative that then fails. An action with an effect would see both.
- **The value register is not re-entrant.** A parser's functions share one register, so an action
  never calls a parse entry (of the same parser or any other), and one parser serves one parse at a
  time.

## Nesting depth

Recursion is bounded by a counter on the recursive back-edges alone: a DFS over the rule graph marks
one edge on every cycle, and only those calls count. Beyond `maxDepth` (default 256) the call
answers failure, the parse trips, and the entry answers `FAIL`. A deeply nested input is refused;
it never throws `RangeError`. An entry that reaches no back-edge carries no counter at all.

The default sits below the smallest engine's measured throw point, with margin. Measured on
value.js's grammar, 13 nesting shapes, a fresh main thread (2026-09-23/24): V8 throws at 881–934
back-edges at the least (node 26, Chromium 148) and JavaScriptCore at 4,970–5,047 (WebKit 26.4).
`calc(` nested 10,000 deep is refused on all three. `--max-depth 0` emits no counter.

## Also in the package

- `BBNFToAST`, `BBNFToASTWithImports`, and `loadModuleGraph(Sync)` with a host-injected reader. The
  library entry imports no `node:` module; `./gen` (and the `bbnf` bin) is the node-only part.
- `analyzeFirst` / `routes`: the one FIRST-set analysis every face routes by.
- `removeAllLeftRecursion` (`optimizeGraph` in the façade) and `@recover` directives.
