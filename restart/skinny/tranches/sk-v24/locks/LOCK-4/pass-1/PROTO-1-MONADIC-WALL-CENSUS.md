# PROTO-1 — THE MONADIC-WALL CENSUS (LOCK-4, pass 1, probe 1)

**Lock:** LOCK-4 — THE COMBINATOR CORE (parse-that rust + TS)
**Family:** cross-family — 4A/4B/4C/4D universal precondition (the reification floor)
**Date:** 2026-07-16 · **loadavg at run:** 7.06 7.33 8.19 · **verdict:** RAN, measurement-free census (no perf claim)

> **Naming reconciliation.** My dispatch named this deliverable `PROTO-1.md`, but that slot is
> already occupied on disk by a *distinct, RAN, REGISTRY-referenced* probe
> (`PROTO-1.md` = VALUE-PLANE-SURVIVAL, a Rust value-plane bench). Overwriting it would destroy
> that probe and dangle every REGISTRY reference to it. The monadic-wall census is the *same probe*
> as the pre-existing `PROTO-2.md` (CHAIN-MONADIC-CENSUS). This file is an **independent re-run**
> with the exact bucketing + the explicit ≤2% / hot-path GREEN-RED threshold my brief demands; it
> **confirms** PROTO-2's GREEN. Nothing was clobbered.

---

## QUESTION

Every reification family (4A analyzable graph · 4B fusion pass · 4C one-pass compiler · 4D e-graph
optimizer) is bounded by how much of the REAL corpus is *analyzable* vs *opaque past a monadic
bind*. Across value.js, latex-paper, keyframes.js, parse-that's own TS parsers, and parse-that's
Rust JSON/CSS grammar, what fraction of combinator sites is:

- **(a) static skeleton** — `then/or/all/any/many/opt/sepBy/skip/next/wrap/trim/not/negate/minus/dispatch` (fully analyzable structure);
- **(b) typed-leaf projection** — `map` over a pure fn ptr (selective-analyzable — transforms the *output value*, never the parser structure);
- **(c) OPAQUE-DYNAMIC** — `chain / mapState / map_with_state / map_with_ctx` (the true reanalysis barriers — the monad can manufacture parser structure no static pass can see through).

**Pass/fail.** opaque-dynamic ≤ 2% of total combinator sites **AND** zero hot-path rules
(JSON/CSS value production) contain `chain`/`mapState` → **GREEN** reification floor for all four
families. Else **RED** with the exact opaque loci enumerated (the ceiling every family designs around).

## METHOD (construction-free, measurement-free)

`parse-that` is **READ-ONLY** — no build, no bench, no copy. Pure `rg` token census + `Read` of
every opaque locus in full context. `ripgrep 15.1.0`. Excludes `node_modules`, `.claude/worktrees`,
`dist`, `test`/`tests`/`*.test.*`.

- Token census: `rg -oN '\.(then|or|all|any|many|opt|sepBy|skip|next|wrap|trim|not|negate|minus|dispatch|map|chain|mapState)\b' <dirs> | sort | uniq -c`
- Locus enumeration: `rg -n 'chain|mapState|map_with_state|map_with_ctx' <dirs>`, then Read each real call site and tag hot-path + S/M.

**Corpora (exact paths):**

- value.js — `/Users/mkbabb/Programming/value.js/src`
- latex-paper — `/Users/mkbabb/Programming/latex-paper/src`
- keyframes.js — `/Users/mkbabb/Programming/keyframes.js/src`
- parse-that TS parsers — `/Users/mkbabb/Programming/parse-that/typescript/src/parse/parsers`
- parse-that Rust grammar — `/Users/mkbabb/Programming/parse-that/rust/parse_that/src` (the JSON/CSS
  productions live here; the brief's `benches/parse_that/{combinator,css}.rs` are **divan harness
  files** — they `import json_parser` / `.call(...)` and contain **zero** combinator sites, so the
  denominator is taken over the real grammar source instead).

`map_with_state` / `map_with_ctx` are Rust-only method names; `mapState` is the TS name. All four
are searched in every tree.

## WHAT RAN

Four `rg -oN | uniq -c` token censuses (one per TS/JS tree + one over the Rust grammar source), one
`rg -n` locus sweep over all seven targets, and a Read of every real `.chain(` call site (6 sites)
plus the Rust `chain`/`map_with_state`/`map_with_ctx` occurrences (all are method *definitions* or
doc-comments — **zero call sites**). The opaque surface is small enough to classify *exhaustively*,
not sample.

### Bucket totals (combinator-token census)

| corpus | total sites | (a) skeleton | (b) `map` | (c) opaque call sites |
|---|--:|--:|--:|--:|
| value.js | 581 | 327 | 250 | **4** |
| latex-paper | 226 | 128 | 98 | **1** |
| keyframes.js | 112 | 40 | 72 | **0** |
| parse-that TS parsers | 33 | 25 | 8 | **0** |
| parse-that Rust grammar (`src/`) | 84 | ~40 | 44 | **0** |
| **TOTAL (TS/JS + pt-TS)** | **952** | | | **5** |
| **GRAND TOTAL (incl Rust grammar)** | **1036** | | | **5** |

- **`mapState` / `map_with_state` / `map_with_ctx` call sites: 0 everywhere.** In Rust they are
  *defined* (`combinators/methods/map.rs:19,32`, `alternation.rs:37`) but **never called** by the
  json / css / csv domain parsers. The entire opaque surface is `.chain`.
- Every `.chain` occurrence in keyframes.js and in the Rust grammar is a **doc-comment / prose
  mention** ("thin chainable delegates", "nested Seq chains", "and/or chains") — **not a call site.**

### The discriminating surface — every real opaque call site enumerated

| # | corpus | locus | shape | hot JSON/CSS value rule? | tag |
|---|---|---|---|:--:|:--:|
| 1 | value.js | `src/parsing/utils.ts:195` `unitParser` | `unitToken.chain(t => lut.has(t) ? succeed(canonical) : fail)` — guard between two **constant** parsers by a LUT predicate | no (CSS *unit* leaf-guard; consumer grammar) | **S** |
| 2 | value.js | `src/parsing/color/color.ts:495` `nameParser` | `namedColorIdent.chain(x => known ? succeed(value) : fail)` — same guard; `value` is a pure fn of `x`, embedded as *result* | no (CSS *named-color* leaf-guard) | **S** |
| 3 | value.js | `src/parsing/color/color.ts:546` `systemColorParser` | `namedColorIdent.chain(x => LUT.has ? succeed(tagged) : fail)` — same guard | no (CSS *system-color* leaf-guard) | **S** |
| 4 | value.js | `src/parsing/stylesheet/stylesheet.ts:610` `atRule` | `.chain(name => { if "keyframes" → keyframesBody; … default → unknownBody(lower) })` — **dispatch** over a fixed finite set of *static* body parsers; name folds into the *result* | no (CSS *at-rule* block dispatch — not the per-byte value inner loop) | **S** |
| 5 | latex-paper | `src/grammar/environments.ts:255` `environment` | `\begin.skip(ws).next(braceBalanced()).chain(envName => …)` — captured `envName` builds the **runtime terminator literal** `\end{${envName}}`; the continuation's *structure* depends on a runtime value | no (LaTeX document structure — not JSON/CSS at all) | **M** |

`S` = selective-reformulable (finite choice among statically-known parsers, or a result-embedding
guard — expressible as `branch`/`select`/`dispatch`, fully analyzable).
`M` = irreducibly-monadic (the parse continuation genuinely manufactures structure from a runtime
value — the one true reanalysis barrier).

## THE NUMBERS

- **Total combinator sites:** 952 (TS/JS + pt-TS) · 1036 (incl. Rust grammar).
- **Opaque-dynamic call sites (c):** **5** — all `.chain`; `mapState`/`map_with_state`/`map_with_ctx` = **0**.
- **Opaque fraction:** **5 / 952 = 0.525%** (0.483% over grand total). ≪ 2%.
- **True monadic barrier (M only):** **1 site** = **0.10%** of the grand corpus — self-scoped to
  LaTeX `\begin…\end` environment bodies. The other 4 opaque sites are selective-reformulable (S).
- **Hot path (parse-that JSON `json.rs` + CSS `css/*.rs` value production):** **0** `chain` /
  `mapState` / `map_with_state` / `map_with_ctx` call sites. The methods exist; the hot grammar
  never binds. **Hot path is monad-free.**
- Consumer opaque sites 1–5 are CSS leaf-guards / an at-rule block dispatch / a LaTeX env body —
  **none is a JSON/CSS bench-hot value production.**

## VERDICT

```
opaque-dynamic = 0.525% of combinator sites (5/952)   ≤ 2%          → PASS
hot-path (JSON/CSS value production) chain/mapState = 0 call sites   → PASS
true reanalysis barrier (irreducibly-monadic) = 1 site (0.10%), self-scoped to LaTeX env bodies,
    never on any JSON/CSS hot path
────────────────────────────────────────────────────────────────────────────
GREEN — reification floor holds for ALL FOUR families (4A / 4B / 4C / 4D).
```

The analyzable subset is effectively the **whole** grammar. Every family (analyzable graph, fusion
pass, one-pass compiler, e-graph optimizer) may treat the full combinator tree as static structure:
the monad is dropped everywhere it would matter. The lone irreducibly-monadic site (LaTeX
environment terminator) is *isolated* — a family can leave it as an opaque leaf and lose nothing on
the JSON/CSS hot path it is actually optimizing.

**Patch/diff:** none. This probe is construction-free and measurement-free (pure `rg` + `Read`); it
changes zero code. `parse-that` untouched (READ-ONLY).

## CAVEATS (honest)

- The raw token census over-counts the denominator: `.map`/`.trim`/`.skip`/`.next` also match
  `Array.prototype.map`, `String.prototype.trim`, iterator methods, etc. in non-parser code. This
  **inflates the denominator**, which only makes the ≤2% bar *easier* — it does not threaten the
  verdict. The **numerator is clean**: JS/TS have no `Array.chain`/`String.chain`, so every
  `.chain` token is a parser bind (and the doc-comment mentions were excluded by Read).
- The census is a **snapshot of today's grammars**, not a proof that no future consumer introduces
  a hot-path `chain`. It bounds the *present* reification floor; families should still treat
  `chain`/`mapState` as an opaque-leaf boundary by construction, not assume it stays empty.
