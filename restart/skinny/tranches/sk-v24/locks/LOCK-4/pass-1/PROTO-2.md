# PROTO-2 — CHAIN-MONADIC-CENSUS (LOCK-4, pass 1, probe 2)

**Lock:** LOCK-4 — THE COMBINATOR CORE (parse-that rust + TS)
**Family:** all four (4A graph / 4B fuser / 4C compiler / 4D optimizer — analyzability gate)
**Date:** 2026-07-16 · **loadavg at run:** 12.50 14.18 12.59 (no perf claim; census is measurement-free)

---

## QUESTION

Do the real consumer grammars use monadic `chain`/`mapState` for GENUINELY data-dependent
parsing, or is every data-dependent site reformulable as an applicative/selective branch?

Parsley's law: static grammar structure (FIRST/FOLLOW, fusion, one-pass compilation, e-graph
rewriting) is exploitable *only* where the monad is dropped. `chain` can generate dynamic parser
structure that no static analysis (4A/4B/4C/4D) can see through. If the irreducibly-monadic set is
tiny and self-scoped, the analyzable subset is the WHOLE grammar and every family keeps
full-grammar analyzability. If it is pervasive or unscoped, every family's optimizer caps at the
applicative nodes and the beyond-SOTA claim shrinks.

## METHOD (construction-free, measurement-free)

Over the three TS parse-that consumer suites + parse-that's own Rust domain parsers:

- value.js — `/Users/mkbabb/Programming/value.js/src`
- latex-paper — `/Users/mkbabb/Programming/latex-paper/src`
- keyframes.js — `/Users/mkbabb/Programming/keyframes.js/src` (transitive consumer via value.js)
- parse-that Rust — `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/`

Command: `rg -n '\.(chain|mapState|map_state|recover|map)\(' <repo>/src` (node_modules/dist
excluded). Then read every `chain`/`mapState`/`recover` hit in context and tag:

- **A** applicative-transparent — `map` on a static-structure result (analyzable; the function
  transforms the *output value*, never the parser structure).
- **S** selective-reformulable — a data-dependent *choice* among a finite set of statically-known
  parsers (expressible as `branch`/`select`/`dispatch`; analyzable).
- **M** irreducibly-monadic — the parse continuation genuinely depends on a runtime value; the
  monad manufactures dynamic structure (opaque).

`parse-that` is READ-ONLY. No build, no bench, no code copied (pure `rg` + Read; nothing to
construct). No patch/diff — this probe changes zero code.

## WHAT RAN

`rg` over the four `src` trees + `parsers/`, then every `chain`/`mapState`/`recover` call site read
in full context (13 files touched). The monadic surface is small enough to hand-classify every
site exhaustively rather than sample.

### The discriminating surface — `chain` / `mapState` / `recover` (every site enumerated)

| # | repo | site | code shape | tag |
|---|------|------|-----------|-----|
| 1 | value.js | `src/parsing/utils.ts:195` `unitParser` | `unitToken.chain(t => lut.has(t) ? succeed(canonical) : fail)` — guard: choice between two **constant** parsers (`succeed`/`fail`) by a lookup predicate | **S** |
| 2 | value.js | `src/parsing/color/color.ts:495` `nameParser` | `namedColorIdent.chain(x => known ? succeed(value) : fail)` — same guard (`value` is a pure fn of `x`, embedded as *result*, not structure) | **S** |
| 3 | value.js | `src/parsing/color/color.ts:546` `systemColorParser` | `namedColorIdent.chain(x => LUT.has ? succeed(tagged) : fail)` — same guard | **S** |
| 4 | value.js | `src/parsing/stylesheet/stylesheet.ts:610` `atRule` | `.chain(name => { if name==="keyframes" return keyframesBody; … return unknownBody(lower) })` — **dispatch** over a fixed finite set of static body parsers; `unknownBody(lower)` folds the name into the *result*, structure is static (`unknownBody` def :412) | **S** |
| 5 | latex-paper | `src/grammar/environments.ts:255` `environment` | `string("\\begin").skip(ws).next(braceBalanced()).chain(envName => …)` — captured `envName` builds the **terminator literal** `\end{${envName}}` (`nodesUntilEnd`/`rawUntilEnd`/`mathEnvBody`, primitives.ts:163 `endTag = \\end{${envName}}`) | **M** |
| — | latex-paper | `src/grammar/environments.ts:3` | doc-comment mention of `.chain()` — not a call site | (n/a) |
| — | keyframes.js | — | zero `chain`/`mapState`/`recover` in `src` | — |
| — | parse-that Rust | `parsers/**` | zero `.chain`/`.map_state`/`.recover` call sites (methods DEFINED — `chain` alternation.rs:37, `recover` recover.rs:18 — but UNUSED in every domain parser: json/csv/css) | — |

### The `map` surface — applicative by type, not individually opaque

`mapState` = 0 sites everywhere. `recover` = 0 call sites everywhere (defined, never invoked by a
consumer or by parse-that's own parsers). `map` is applicative-transparent **by construction**: the
signatures — TS `map<T2>(f: (v:T) => T2)` (parser.ts), Rust `fn map<O2>(self, f: fn(Output)->O2)`
(combinators/methods/map.rs:10) — make it type-impossible for `f` to return a `Parser` or alter
structure. Every Parser.map site is **A**, zero exceptions possible; every `Array.prototype.map`
site is out of scope (not a combinator). Raw `.map(` totals (Parser+Array conflated, upper bound on
the A-surface): value.js 249, latex-paper 98, keyframes.js 71; Rust domain parsers use `.map`
throughout json/csv/css, all A by the signature above.

## THE NUMBERS — site table `|A| |S| |M|` per repo

| repo | Parser.map (A, by type) | chain/mapState/recover → S | chain/mapState/recover → M |
|------|:---:|:---:|:---:|
| value.js | all Parser.map (≤249 raw) | **4** | **0** |
| latex-paper | all Parser.map (≤98 raw) | **0** | **1** |
| keyframes.js | all Parser.map (≤71 raw) | **0** | **0** |
| parse-that Rust `parsers/` | all Parser.map | **0** | **0** |
| **TOTAL** | **A = every Parser.map (uniformly analyzable)** | **S = 4** | **M = 1** |

## GATE

- **|M| = 1 ≤ 3** ✓
- **Poison-scope test on the single M-site** (latex-paper `environment`): the `.chain` fires *after*
  `string("\\begin")` has been consumed, so the rule's FIRST set is the fixed literal `\begin`. The
  data-dependency (`envName` → `\end{envName}` terminator) is confined entirely to the body/close of
  this one rule. In every enclosing alternation `any(command, environment, text, …)` a static
  analyzer still deterministically selects `environment` iff the input starts with `\begin`; the
  opacity degrades only this rule's internal body structure, never a sibling's FIRST set. **Passes.**

Note on the S-sites: sites 1–4 are all the guard/dispatch shape (`succeed`/`fail` choice, or finite
tag→body dispatch) — Parsley's `branch`/`select` sees through every one; each returns a member of a
statically-known parser set, so FIRST/FOLLOW, fusion, one-pass, and e-graph analysis all remain
exact across them. They are analyzable, not opaque.

## VERDICT

**PASS — |A|=every Parser.map (uniformly analyzable), |S|=4, |M|=1 ≤ 3, and the lone M-site is
rule-scoped behind a fixed `\begin` FIRST set (poison-scope PASSES). The selective+opaque-minority
strategy holds: all four families (4A graph / 4B fuser / 4C compiler / 4D optimizer) keep
full-grammar analyzability. The analyzable subset is effectively the WHOLE grammar — exactly one
rule (LaTeX environment close-matching) is irreducibly monadic, and it self-scopes. Pass-2 does NOT
need a capped-at-applicative optimizer; it needs one nameable opaque-boundary escape hatch for the
`environment` rule (and any future parameterized-terminator rule), leaving the rest fully static.**
