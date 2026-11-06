# R4 — parse-that TypeScript implementation map (SK-V24 recon)

READ-ONLY recon. All paths verified via `ls`/`rg`/`Read`. Cite as `path:line`.
TS package root: **`/Users/mkbabb/Programming/parse-that/typescript`** (repo root
`/Users/mkbabb/Programming/parse-that` also holds a Rust port under `rust/` and
docs under `docs/`).

---

## 1. Package layout & published version state

- Manifest: `/Users/mkbabb/Programming/parse-that/typescript/package.json`
  - name `@mkbabb/parse-that`, **version `1.0.0`**, `type: module`, `sideEffects: false`.
  - Multi-entry subpath exports (package.json:8-49): `.` (parse), `./core`,
    `./diagnostics`, `./packrat`, `./utils` — each with types/import/require.
  - `files: ["./dist"]` — only built `dist/` is published.
- Git state (`git log`, `git tag`): HEAD `ef10d5b` (VALUEJS-PT-E letter). Tags
  present: `v1.0.0`, `v0.13.0`, `v0.12.0`, `v0.9.0`, `v0.3.1`, plus
  `parse-that-v1a-done|v1b-done|v1c-done`. The 1.0.0 cut = commit `7eab78c` (S.H4
  ledger closure) + `043c4d1` (S.H2: excise `*Span`, fix `chain()` falsy seed C-16).
- Built output exists locally: `typescript/dist/*.{js,cjs,d.ts,map}` (core, diagnostics,
  packrat, utils, parse, ansi, debug, …).
- **Only ONE tranche exists**: `docs/tranches/A/` (CLOSED+PUBLISHED). Waves A.W0-W3.
  Tranche B (0.12.0), Q (0.13.0), S (1.0.0) are recorded in `docs/tranches/A/PROGRESS.md`
  and commit log, not as separate dirs.

### src tree (all under `typescript/src/parse/`, 2650 LOC total)
| file | LOC | role |
|---|---|---|
| `parser.ts` | 711 | **combinator core** — `Parser<T>` class + all methods |
| `packrat.ts` | 488 | opt-in WDM packrat + left-recursion memoization |
| `leaf.ts` | 399 | leaf/primitive combinators: `eof any dispatch all string regex whitespace` |
| `debug.ts` | 383 | `parserPrint`/`statePrint`/`parserDebug` (pretty-print, cold) |
| `state.ts` | 189 | `ParserState<T>` class, `Span`, `ParserContext`, `parserNames` |
| `utils.ts` | 186 | diagnostics accumulator + `skipWhitespace`/`skipBlockComments` scanners |
| `split.ts` | 58 | `containsDelimiter`/`splitBalanced` (BBNF `toDoc()` format-time) |
| `parsers/json.ts` | 52 | `jsonParser` domain showcase |
| `lazy.ts` | 43 | `getLazyParser`/`createLazyCached`/`lazy` decorator |
| `core.ts` `diagnostics.ts` `packrat-entry.ts` `utils-entry.ts` `index.ts` | 5-26 ea | subpath barrels |
| `parsers/{csv,utils,index}.ts` | 8-23 | csv + string-util domain parsers |
| `ansi.ts` | 17 | ANSI color codes for debug print |

Barrel: `src/parse/index.ts:1-14` re-exports everything. Subpath barrels:
`core.ts` (zero-side-effect primitive set), `diagnostics.ts`, `packrat-entry.ts`,
`utils-entry.ts`.

---

## 2. Combinator core (`parser.ts`)

`Parser<T>` class shape — `parser.ts:24-32`: fields `id:number` (process-global
`PARSER_ID++`, parser.ts:18,25), `state: ParserState<T>|undefined`, `flags:number`
(FLAG_NONE=0/FLAG_TRIM_WS=1/FLAG_EOF=2, parser.ts:20-22), `parser: ParserFunction<T>`,
`context: ParserContext`.

Entry: `parseState` (parser.ts:34-49) opens a packrat epoch via `packratEnter()`
in try/finally; `parseStateInner` (parser.ts:51-75) runs `this.parser(state)` then
builds the furthest-offset error view. `parse` (parser.ts:77-79) returns `.value`.

Combinator methods (each builds a fresh closure + `createParserContext`, once at
grammar-construction time):
- `then` 81-103 · `or` 105-122 · `chain` 124-144 (C-16: threads value on success,
  no falsy-seed drop) · `map` 146-160 · `mapState` 162-187 · `skip` 189-210 ·
  `next` 212-232 · `opt` 234-249 · `not` 251-303 (dual: `negate` no-arg / `not` w-arg) ·
  `minus` 309-331 (EBNF `-` exception) · `peek` 339-357 · `lookAhead` 365-390 ·
  `wrap` 392-431 (inlined `start.next(this).skip(end)`) · `call` 437-478 (flag
  dispatch, fast-path `flags===0`) · `trim` 480-521 · `many` 523-563 · `sepBy`
  569-636 · `eof` 638-642 · `recover` 653-688 (error-recovery + diagnostics) ·
  `debug` 690-696 · static `lazy` 702-707.

`_initWhitespace()` at parser.ts:711 breaks the circular module init (whitespace
Parser constructed post-eval).

Leaf combinators (`leaf.ts`): `eof` 11-25 · `any` 28-79 (arity-2 unrolled 39-57,
general 58-73) · `dispatch` 100-151 (O(1) first-char via `Int8Array(128)` table) ·
`all` 154-164 → `fuseAll` 179-273 (arity-2 unrolled 183-212, arity-3 214-251,
general 255-272) · `string` 276-313 (len-1 charCode fast path) · `regex` 317-369
(sticky-flag `test()` zero-alloc path) · `trimStateWhitespace` 372-391 ·
`whitespace` 395-399.

---

## 3. Regex engine — NO bespoke TS engine

The TS runtime uses **native JS `RegExp`** only. `leaf.ts:317-369` (`regex()`):
strips `/y`, builds a sticky clone `new RegExp(r, flags+"y")`, drives via
`sticky.test()` (zero-alloc default) or `sticky.exec()` (custom match fn).
`whitespace = regex(/\s*/)` at leaf.ts:397.

There is **no NFA/DFA/Thompson engine in `typescript/src/`** (verified: `rg`/`find`
for nfa|dfa|regex returned nothing under `typescript/`). The bespoke regex engine
referenced in project memory lives on the **Rust side**: `rust/regex/` (out of scope
for the TS map; noted for isomorphism only). SK-V24 note: any TS regex-engine work
is greenfield here.

---

## 4. Build / test / bench tooling

- Build: `vite.config.ts` — `vite build`, multi-entry lib (parse/core/diagnostics/
  packrat/utils), `formats: ["es","cjs"]`, `minify:false`, `sourcemap:true`,
  `vite-plugin-dts`. devDeps: vite ^7, vitest ^3.2, typescript ^5.8.
- Test runner: `vitest.config.ts` — `include: ["test/*.test.ts"]`,
  `setupFiles:["test/setup.ts"]`, `pool:"forks"`. **13 `test/*.test.ts` files**
  (chain, reentrancy, verify-parse-output, split, validate-parsers, json-vectors,
  json, csv, math, print, dist-surface, memoize, debug).
- **Bench = vitest `bench()`** — `npm run bench` = `vitest bench`. 4 bench files
  under `test/benchmarks/*.bench.ts`: `json-comprehensive.bench.ts` (83, competitor
  matrix), `pt-b3-fusion.bench.ts` (161, in-realm regression floor — fusion/dispatch/
  json axes; `vitest` `bench`,`BenchOptions` warmup=100/time=1500 at :30-31),
  `wasm-json.bench.ts` (117), `wasm-format.bench.ts` (103). Competitor harness
  wrappers (non-bench): `peggy nearley arcsecond ohm parjs chevrotain parsimmon
  parse-that.ts` under `test/benchmarks/`.
- **Proof gates** (`package.json:34-48`, node scripts, NOT cargo/vitest):
  `proof:all` chains manifest, no-css-surface, subpath, packrat-{cross-input,
  reentrant,large-offset,armed}, no-span-surface, no-dead-combinator, perf.
  Scripts in `typescript/scripts/*.mjs` (8 present) + `test/{manifest-gate,
  subpath-gate}.mjs`. `proof:perf` runs under `--expose-gc`.

### Exact commands (run from `/Users/mkbabb/Programming/parse-that/typescript`)
- `npm test` → `NODE_OPTIONS='--max-old-space-size=8192' vitest run`
- `npm run bench` → `vitest bench`
- `npm run build` → `vite build`
- `npm run proof:all` (or individual `proof:*`)

---

## 5. PT-E1 / PT-E2 open items (docs)

Source: `docs/tranches/A/VALUEJS-PT-E-2026-07-05.md` — value.js 1.1.0+ **ask
letter** (NOT a defect ask, gates no wave). Table at :27-31.

- **PT-E1 (HIGH)** — *scoped per-parse diagnostics*; cures the structurally-dead
  `ParseDiagnostic.expected`. Root cause in code: the `expected` set is gated behind
  a **module-global** flag `diagnosticsEnabled` (`utils.ts:6`), seeded only when
  enabled in `mergeErrorState` (`utils.ts:28-49`, esp. :33 `diagnosticsEnabled &&
  label ? [label] : undefined`). `ParserState.expected?` field at `state.ts:43`;
  `Diagnostic.expected` at `utils.ts:89`; collector `collectDiagnostic`
  `utils.ts:102-128`. Downstream target: kf's `ResolvedKeyframes.diagnostics`.
  value.js-side half (letter :33-41): value.js KEEPS authoring messages + does NOT
  delete `expected` — it is the forward seam PT-E1 lights up. Ask = make diagnostics
  **per-parse scoped**, not global-flag gated.
- **PT-E2 (MED)** — *combinator-inference tightening*; the `Parser<any>` inference
  leak (`parse-that-audit §4.2`). Leak sites in code: `ParserFunction<T>` uses `any`
  (`parser.ts:14-16`); `any<T extends Array<Parser<any>>>` (`leaf.ts:28`);
  `all<T extends Array<Parser<any>>>` (`leaf.ts:154`); `fuseAll(parsers: Array<
  Parser<any>>)` (`leaf.ts:179`); `restore(saved: {…; value: any})` (`state.ts:79`);
  `debug.ts:254`. Pervasive `as Parser<unknown>` casts throughout `parser.ts`.
- **PT-E3 (record, not ask)** — Pratt/calc 2-tier fold stays dormant; value.js will
  NOT pull it forward (letter :31).

---

## 6. V8-sensitive surfaces (hot-path — path:line)

These are the shapes/dispatch sites that govern monomorphism, hidden-class stability,
and allocation on the parse hot path.

**(a) Polymorphic value field / hidden-class churn**
- `ParserState<T>` shape — `state.ts:36-53`: 8 mutable fields incl. `value:T=undefined`
  (holds string|array|object|undefined across every parse) and **optional**
  `expected?: string[]` (`state.ts:43`) — assigning it mid-parse (utils.ts:33-45)
  triggers a hidden-class transition. `suggestions`/`secondarySpans` init to `[]`.
- Type-erased value writes: `unsafeSetValue` `state.ts:87-89`, `unsafeCall`/
  `unsafeCallRaw` `state.ts:91-99` — the single megamorphic-write choke points.
- `state.ok/err/from` `state.ts:55-73` all funnel through `unsafeSetValue`.

**(b) Prototype-chain view object (deopt risk)**
- `mapState` builds `Object.create(state)` per invocation — `parser.ts:177-180`.
  A prototype-chain object; property access is slower and can deopt callers.

**(c) Central megamorphic call site**
- Every combinator dispatches through the `.parser` function-pointer field:
  `this.parser(state)` / `p.parser(state)` — e.g. parser.ts:53,82,88,131,137;
  leaf.ts:44,49,62,140. This is THE megamorphic IC of the engine; V8 sees many
  distinct closure shapes flowing through one indirect call.

**(d) Monomorphic specialization (already applied — do-not-regress)**
- Arity unrolling: `any` arity-2 `leaf.ts:39-57`; `fuseAll` arity-2 `leaf.ts:183-212`,
  arity-3 `leaf.ts:214-251` (value.js's 59 `all()` + hot 3-tuples). General arms
  use classic `for` (no `for…of` iterator object): leaf.ts:58-73,255-272.
- `dispatch` O(1) `Int8Array(128)` table — `leaf.ts:100-151` (avoids sequential-trial
  megamorphism; PT-Q5 subTable widening RETRACTED, leaf.ts:89-98).
- `string` len-1 charCode fast path `leaf.ts:282-294`; `regex` sticky `test()`
  zero-alloc `leaf.ts:344-358`.
- `call()` flag fast-path `flags===0` returns direct `this.parser(state)` — parser.ts:438.

**(e) Closure-allocation discipline**
- `createLazyCached` uses a **closure-local** cache (NOT function-object mutation)
  to avoid "megamorphic IC pollution" — `lazy.ts:19-25` (explicitly documented).
  `LAZY_PARSER_CACHE` WeakMap at lazy.ts:5.
- Combinator closures allocate once per `new Parser(...)` at grammar build — not per
  parse — so per-parse closure alloc is nil on the steady path.

**(f) Array-growth / trim**
- `many`/`sepBy` pre-size `new Array<T>(est)` + index-write, `matches.length = len`
  trim — parser.ts:523-563, 569-636. `fuseAll` `out.length = w` trims to live count
  (leaf.ts:209,248,270).

**(g) Packrat key arithmetic (opt-in tier)**
- `getCijKey` float64-safe `id*MEMO_OFFSET_SPAN+offset`, `MEMO_OFFSET_BITS=32`,
  `MEMO_MAX_ID≈2.1M`, fail-loud RangeError — `packrat.ts:71-100`. Module-global
  `Map<number,MemoCell>` tables MEMO/HEADS/GROWING (packrat.ts:133-193), epoch
  save/restore `packratEnter`/`packratExit` (packrat.ts:216-250). `PACKRAT_ARMED`
  latch (packrat.ts:156) → **true no-op until first `memoize()` constructed**
  (`makeMemoized` arms at :290); default LL(1) path pays nothing.

---

## 7. Consumers (do-not-break gates)

| consumer | path | version | parse-that dep | test cmd |
|---|---|---|---|---|
| **value.js** | `/Users/mkbabb/Programming/value.js` | 3.1.0 | `^1.0.0` (lock→**1.0.0**) | `npm test` = `vitest` |
| **latex-paper** | `/Users/mkbabb/Programming/latex-paper` | 0.2.1 | `^0.7.1` (node_modules→**0.7.1**) | `npm test` = `vitest run` |
| **keyframes.js** | `/Users/mkbabb/Programming/keyframes.js` | 6.0.0 | **transitive** (no direct dep) | `npm test` = `vitest` |

- **value.js** (`package.json:115`, lock:1441→`parse-that-1.0.0.tgz`). Imported
  surface (`rg` of `src`): `Parser, ParserState, all, any, dispatch, regex, string,
  whitespace, mergeErrorState`. Import sites: `src/parsing/math.ts:16`,
  `src/parsing/timeline/easing.ts:1`, `src/parsing/units.ts:1`,
  `src/parsing/color/color.ts`, `src/parsing/stylesheet/stylesheet.ts`,
  `src/parsing/index.ts`, `src/parsing/utils.ts`, `src/parsing/timeline/
  scroll-timeline.ts`, `src/units/color/color-names.ts`. Uses `dispatch()` on the CSS
  function-name bucket + 59 `all()` sites (per pt-b3 bench notes). vitest config:
  `test/**/*.ts`, `environment:"jsdom"` (`value.js/vitest.config.ts`).
- **latex-paper** (`package.json:38`). Imported surface: `Parser, ParserState, regex,
  string, any`. Sites: `src/bibtex/parser.ts:7`, `src/grammar/{primitives,commands,
  math,text,compile,document,environments}.ts`. vitest: `tests/**/*.test.ts`,
  `assetsInclude:["**/*.bbnf"]`.
  ⚠ **VERSION SKEW**: pinned to `^0.7.1` (resolves 0.7.x, `<0.8.0`) — predates the
  1.0.0 breaking cut (`*Span` excision S.H2, `chain()` C-16). It does NOT track
  `1.0.0`. Bumping latex-paper to 1.0.0 must be validated: it imports only
  `Parser/ParserState/regex/string/any` (all still exported), but this is an
  untested jump across 0.9→1.0.
- **keyframes.js** (v6.0.0) has **no direct `@mkbabb/parse-that` in package.json** —
  consumes it transitively via value.js. `src/animation/compile/parse-flatten.ts:117`
  comment: the entrypoint "removes kf's direct `@mkbabb/parse-that` production
  dependency." PT-E1 downstream target is kf's `ResolvedKeyframes.diagnostics`.
  Worktrees exist: `keyframes.js-wt`, `keyframes-wt-H-W2-verify`,
  `keyframes-u-worktrees` (the bare `/Users/mkbabb/Programming/keyframes` dir does
  NOT exist — canonical is `keyframes.js`).

---

## 8. Gotchas / notes for implementers

- Cite the TS package as `.../parse-that/typescript`, NOT the repo root (repo root
  also has `rust/` + `docs/`).
- No `cargo` here for the TS surface — TS gates are `npm test` / `npm run bench` /
  `npm run proof:all`. (SK-V24 recon constraint: this report is read-only; do not run.)
- The `*Span` combinator family was **excised** in 1.0.0 (S.H2); `proof:no-span-surface`
  guards it. Do not reintroduce.
- Diagnostics (`expected`/suggestions/secondarySpans) are gated behind global
  `diagnosticsEnabled` (`utils.ts:6`) — this is exactly what PT-E1 asks to make
  per-parse scoped. Any diagnostics-on hot path is cold today.
- Packrat is OFF by default and arms permanently at first `memoize()` construction
  (packrat.ts:290) — benchmarks/perf claims must state armed vs unarmed.
- keyframes.js path is `keyframes.js`, not `keyframes`; it has no direct dep — verify
  through value.js's resolution when treating it as a gate.
