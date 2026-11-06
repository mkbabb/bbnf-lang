# PROTO-8BD — LOCK-8 pass-3, family 8B + 8D, probe: the tightened-scratch crater

**Plane:** THE TS BACKBONE PLANE (parse-that/typescript) · families 8B (native-shape
budget / emit-identity) + 8D (make-or-break force-scope) · one construction closes both.

**Verdict line: RAN_MEASURED — 8B PASS (dist .js/.cjs byte-diff = 0) · 8D MAKE, NO
CRATER (force-scope TS2322 = 2 = baseline; ZERO new red at text.ts:145,154 /
document.ts:81) · vitest 118✓/9-skip (127) unchanged.** The tightened scratch IS the
candidate — no official 1.1.0 needed. 8B-Q2 (emit-identity, never previously fired),
8B's native-shape budget, and 8D-Q1 (make-or-break) all close GREEN.

> Naming: PROTO-1.md/PROTO-2.md in this dir are the prior 8A/8C probes. This 8B+8D
> probe is written as PROTO-8BD.md to avoid clobbering sibling work (dir is shared).

---

## Questions (exact)

- **Q-8B** (constitution v / zero-cost law): does the tightened typed algebra emit
  **BYTE-IDENTICAL** `dist/*.{js,cjs}`?
- **Q-8D** (make-or-break): does the tightening **redden** latex-paper's parse-that
  consumers under a **FORCE-SCOPE** check (`skipLibCheck:false`, own tsconfig) that the
  project's own `vue-tsc -p tsconfig.vue.json` — a FALSE GREEN (F6) — cannot see?

## The tightening (PT-E2 combinator-inference)

Applied to a copy of `parse-that/typescript` @ HEAD **ef10d5b** (1.0.0), origin
untouched (READ-ONLY), `node_modules` symlinked, scratch at `pt-scratch-8BD/`:

| site | from | to |
|---|---|---|
| `leaf.ts:28` `any` bound | `Array<Parser<any>>` | `ReadonlyArray<Parser<unknown>>` |
| `leaf.ts:154` `all` bound | `Array<Parser<any>>` | `ReadonlyArray<Parser<unknown>>` |
| `leaf.ts:179` `fuseAll` param | `Array<Parser<any>>` | `ReadonlyArray<Parser<unknown>>` |
| `parser.ts:189` `skip` | `skip<S>(p: Parser<T \| S>)` | `skip(p: Parser<unknown>)` |
| `parser.ts:569` `sepBy` | `sepBy<S>(sep: Parser<S \| T>, …)` | `sepBy(sep: Parser<unknown>, …)` |
| `ExtractValue` (dup at `parser.ts:9` + `leaf.ts:155`) | two local copies | lifted to new `src/parse/types.ts`, `import type` in leaf.ts; dead copy deleted from parser.ts |

**Sub-finding (directive under-specification).** The literal directive (drop `<S>`,
param → `Parser<unknown>`) is INCOMPLETE: the method **bodies** still reference `S` at
`parser.ts:192` (`state as ParserState<T | S>`) and `parser.ts:592`
(`state as ParserState<S | T>`) → `tsc --noEmit` = **TS2304 "Cannot find name 'S'"**
×2. esbuild strips types so the *JS emit is unaffected either way*, but the tree is
not type-valid until those two casts retarget to `ParserState<unknown>`. The honest
tightening includes that body-fix (in the patch); with it, `tsc --noEmit` = clean.
Full diff: `PROTO-8BD.patch`.

---

## Method — what actually ran

Toolchain (REAL, from origin `node_modules`, NOT a standalone esbuild bundle):
**vite 7.3.1** (^7 ✓), **vite-plugin-dts 4.5.4**, **typescript 5.9.3** (^5.8 ✓),
node v26.0.0. Build = the real `npm run build` (`vite build`; esbuild transpile +
rollup + vite-plugin-dts for `.d.ts`). loadavg 6.1–8.8 throughout (recorded).

**(0) Toolchain attribution (done first).** Fresh **pristine** rebuild of the untouched
scratch, diffed against the committed origin `dist/`: **0** `.js`/`.cjs` files differ.
The toolchain reproduces the shipped `dist` byte-for-byte → any pristine↔tightened
delta is attributable purely to the tightening, not to a tsc/vite version drift.

**(1) 8B emit-identity.** Built pristine → `dist-pristine/`, applied the tightening,
`tsc --noEmit` (clean), rebuilt → `dist-tightened/`. Byte-diffed all `.js`/`.cjs`.

**(2) 8D force-scope crater.** `npm pack` produced BOTH a pristine-1.0.0 and a
tightened-1.0.0 tgz (isolates the 0.7.1→1.0.0 version jump from the tightening). In a
latex-paper scratch @ **c140473** (`lp-scratch-8D/`, writable `node_modules` copy),
the parse-that package was surgically swapped (extract tgz over
`node_modules/@mkbabb/parse-that`). Three installs each ran, over `src/grammar/**` +
`src/bibtex/**`:
- `vue-tsc --noEmit -p tsconfig.forcescope.json` — a NEW config `extends ./tsconfig.json`
  with **`skipLibCheck:false`** (the FORCE-SCOPE lever; NOT `-p tsconfig.vue.json`).
- `vitest run` (full 14-file suite).

---

## The numbers

### 8B — emit-identity (PASS)

| measurement | result |
|---|---|
| toolchain drift: pristine-rebuild vs committed origin `dist` (.js/.cjs) | **0 files differ** |
| **tightened vs pristine — `.js`/`.cjs` files differing (of 14)** | **0** |
| `md5(parse.js)` pristine == tightened | `3df6a9ff…105` == `3df6a9ff…105` ✓ |
| `md5(parse.cjs)` pristine == tightened | `1ef0bae0…de1` == `1ef0bae0…de1` ✓ |
| `.d.ts` moved (ALLOWED) | 2 — `leaf.d.ts`, `parser.d.ts` |
| new files (ALLOWED, `.d.ts` only) | `types.d.ts`, `types.d.ts.map` |
| `tsc --noEmit` on tightened tree (after body-fix) | **0 errors** |

→ **(i) .js/.cjs byte-diff = 0.** Zero-cost law HOLDS. The typed algebra is pure erasure.

### 8D — force-scope over src/grammar/** + src/bibtex/** (PASS)

| install | TS2322 | TS2307 | locations | text.ts:145,154 / document.ts:81 | vitest |
|---|---|---|---|---|---|
| **baseline 0.7.1** | **2** | 2 | compile.ts:76, compile.ts:124 | — none — | 118✓ / 9 skip (127) |
| pristine 1.0.0 | 2 | 2 | compile.ts:76, compile.ts:124 | — none — | (n/a) |
| **tightened 1.0.0** | **2** | 2 | compile.ts:76, compile.ts:124 | **— none —** | 118✓ / 9 skip (127) |

- Full error-set diff **pristine-1.0.0 ↔ tightened-1.0.0 = IDENTICAL** (zero delta from
  the tightening). Diff **baseline-0.7.1 ↔ tightened-1.0.0 = IDENTICAL** (zero delta
  from the version jump either).
- The 2 baseline TS2322 are **`@mkbabb/bbnf-lang` `Nonterminals` dual-instance**
  (`node_modules/@mkbabb/bbnf-lang/dist/src/types` vs local `compile.ts`) — **NOT a
  parse-that dual-instance**, and unmoved by the tightening. The 2 TS2307 are unrelated
  `.bbnf` virtual-module resolution (`compile.ts:12,13`).
- **document.ts:81** (`const inlineNode = any(...)`), **text.ts:145** (`any(bracedAccent,
  …)`), **text.ts:154** (`any(...alts)`) — the exact `any()` consumers whose bound was
  tightened to `ReadonlyArray<Parser<unknown>>` — **all stay GREEN**: the tightened bound
  still accepts the calls and infers a result assignable to `Parser<LatexNode|null>` /
  `Parser<TextNode>`.

→ **(ii) new force-scope errors attributable to tightening = 0**; dual-instance TS2322
= **2 = the 0.7.1 baseline**. **(iii) vitest = 118 passed / 9 skipped (127), 0 failed**,
identical baseline↔tightened.

---

## Verdict

**RAN_MEASURED.**

- **8B (Q-8B / 8B-Q2 emit-identity):** **PASS.** Tightened vs pristine `dist/*.{js,cjs}`
  byte-diff = **0** (md5-confirmed on the barrels; 14/14 runtime files identical); only
  `.d.ts` moved. The zero-cost law is upheld — the combinator-inference tightening is
  pure type erasure. 8B's native-shape budget is spent at **0**.
- **8D (Q-8D / 8D-Q1 make-or-break):** **MAKE — NO CRATER.** Under FORCE-SCOPE
  (`skipLibCheck:false`, the lens `tsconfig.vue.json`'s F6 false-green cannot open), the
  tightening reddens **nothing**: TS2322 stays at the 2-count baseline, all at
  `compile.ts` and both a *bbnf-lang* (not parse-that) dual-instance; **document.ts:81
  does NOT break.** MIGRATION.md therefore carries **no** parse-that break from this
  tightening — it is a clean additive 1.0.0→1.x candidate. vitest holds at 127.

The tightened scratch stands as the candidate: `pt-tightened-1.0.0.tgz` (this dir).

## Artifacts (this dir)
- `PROTO-8BD.patch` — the full tightening diff (leaf.ts, parser.ts incl. body-fix, new types.ts)
- `pt-scratch-8BD/` — the tightened parse-that copy (`dist-pristine/`, `dist-tightened/`, build logs)
- `pt-pristine-1.0.0.tgz`, `pt-tightened-1.0.0.tgz` — packed candidates
- `lp-scratch-8D/` — latex-paper @ c140473 scratch + `tsconfig.forcescope.json` +
  `{baseline,pristine100,tightened100}-{vuetsc,vitest}.log`
