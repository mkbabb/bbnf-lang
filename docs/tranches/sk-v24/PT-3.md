# PT-3 — THE TS BACKBONE 1.1.0

*The parse-that TypeScript combinator core, productionized to a minor cut: the certified
lazy-diagnostic-allocation lever lands, its true consumer (per-parse-scoped diagnostics)
dissolves the last module-global latch, combinator inference is tightened to extinguish the
`Parser<any>` leaks, the V8 hot-shape discipline is audited and pinned, and 1.1.0 ships with
three consumer suites green.*

Repo: `~/Programming/parse-that`, TS package root `typescript/`. Package `@mkbabb/parse-that`,
frozen at `1.0.0` (commit `7eab78c` S.H4 + `043c4d1` S.H2). This tranche cuts `1.1.0`.

---

## 1. Charter

### 1.1 Mission

parse-that is the one regex home and the TS combinator backbone of the union (SPEC §1); bbnf
grows no combinator surface and consumes this backbone unmodified. The GN-4.W4 TS tape decoder
seeds the TS backend *standalone* — a monomorphic tape decoder that imports no combinators (its
oracle is `JSON.parse`) — while PT-3 productionizes the combinator backbone the wider TS backend
later composes atop that seed; the two are **complementary, not gating**, neither the other's
precondition. The backbone is productionized on its own merit, not left at proof grade. PT-3
carries the parent PT-N2 (SPEC §2b) to full implementation depth:

1. **W1** lands the CONFIRMED_DELTA lazy-diagnostic-alloc lever (PROVE-11) — the substrate.
2. **W2** productionizes PT-E1: the module-global `diagnosticsEnabled` latch (`utils.ts:6`) and
   the module-global collected-diagnostics buffer (`utils.ts:138-146`) become **per-parse
   scoped**. The W1 lazy `suggestions`/`secondarySpans` fields are the substrate; per-parse
   diagnostics is their **consumer** — Y.13 is satisfied *across* W1→W2 (stated, gated).
3. **W3** productionizes PT-E2: the duplicated `ExtractValue` seam and the enumerated
   `Parser<any>` leaks close; combinator return types infer precisely; zero runtime cost.
4. **W4** audits and pins the V8 hot shapes: the arity-unrolled monomorphizations are preserved
   by measurement; packrat armed-vs-unarmed is disclosed in every perf table.
5. **W5** cuts 1.1.0: semver review, the eight proof gates, the three consumer suites green.

### 1.2 Precept bindings (sk-v23 research/07, by number)

- **#8 / #40 (substrate-with-consumer, Y.13):** W1 fields land with W2 as their consumer in the
  same tranche; no dangling substrate. **#37 (Y.13 consumer-invariant gate):** W2 closes on a
  runtime oracle over the per-parse diagnostic surface, not liveness alone.
- **#2 / #50 (no workarounds/stubs), #6 (no legacy — delete, never flag-and-keep):** W2 *deletes*
  the `diagnosticsEnabled` module-global; it is not wrapped or shadowed. W3 closes leaks by
  precise inference, never by a silencing `as any`.
- **#14 (no god modules) / #15 (directory modules) / colocation grand edict (HANDOFF §1.5.4):**
  `utils.ts` is today a god module (diagnostics latch+accumulator+collector at 6-146, whitespace
  and block-comment scanners at 159-186). W2 — already rewriting the diagnostics half — lifts
  diagnostics into a `src/parse/diagnostics/` directory module (the `./diagnostics` subpath
  already exists), leaving the scanners as their own concern. Parsimony-bounded: the split rides
  the wave that already edits the concern, not a separate cleanup pass.
- **#16 / #54 (tests outside `src/`):** honored via the repo's own `test/*.test.ts` convention;
  no `#[cfg(test)]`/`*.test.ts` inside `src/`.
- **#17 / #55 (docs ride the wave):** the `./diagnostics` API doc and CHANGELOG ride W2/W5.
- **#19 (a hard gate needs an artefact):** every gate below names a runnable command and a
  numeric or boolean threshold; grep-gates are supplementary to a test/bench artefact.
- **#43-49 (the profiling standing law), TS instantiation:** cold per-op median ns via
  `process.hrtime.bigint`, **single build per process, paired interleaving** (both arms imported
  into one process so load nulls out), median-of-13-batches × ≥300k ops, **N≥12 rounds**, min
  AND median reported, per-round loadavg recorded. **No warm benches.** **Packrat armed-vs-unarmed
  stated in every table** (it is a true no-op until first `memoize()`, `packrat.ts:290`).

### 1.3 Refutation guards (SPEC §0 laws that constrain this tranche)

TS work is NEON-free, so the digit-scan-SIMD / block-scheduling / tape-navigation / shim-port
laws do not bind by subject. The laws that **do** bind, verbatim:

- **No shim perf claims → the honest-number law.** The PROVE-11 verify halved the prover's
  dispatch claim (+10.2% → ~+5%). PT-3 carries **~+5%** for the dispatch lane everywhere; it is
  a no-regression lane, **not** gated to ≥10%. Inflating it to the prover's figure is the exact
  self-deception §0/09 outlaws.
- **Warm benches forbidden.** Single-build-per-process paired interleaving is the certified
  method; the naive `vitest bench` cross-run is load-confounded (PROVE-11 §3.1 — pristine at
  loadavg 12, modified at loadavg 27) and is refused as evidence.
- **Paired interleaving is mandatory even for "obvious" wins** (the §0/09 standing lesson): a
  two-arm window asymmetry is the classic self-deception. Every PT-3 ratio is paired-in-one-process.

### 1.4 Supersessions

- **BBNF-PT-G-2026-07-16** supersedes the never-delivered BBNF-PT-F; PT-3 is the full
  implementation form of its PT-N2 carry (SPEC §2b supersession, owner order 2026-07-16 — the PT
  series is FULL IMPLEMENTATION, no longer review-not-execute). Execution runs in the parse-that
  repo, by the owner or a parse-that-side session, after the W0 handshake (letter BBNF-PT-G placed
  + owner acknowledgment + the three consumer baselines banked); bbnf-side agents never write into
  `~/Programming/parse-that` — the handshake, not a fence, is how full implementation and the
  non-clobber mechanics coexist.
- The R4 assumption that `1.0.0` is the consumer floor is **superseded**: latex-paper pins
  `^0.7.1` (resolves `<0.8.0`), so the 1.1.0 bump is an untested 0.7→1.1 jump for it — W0 owns
  the migration.

---

## 2. Interlocks

| direction | id | contract |
|---|---|---|
| complements | **GN-4.W4** | GN-4.W4's tape decoder seeds the TS backend standalone (imports no combinators — oracle is `JSON.parse`); PT-3 productionizes the combinator backbone the wider TS backend later composes atop that seed. Non-gating: neither is the other's precondition. |
| rides-with | **PT-1 (PT-N1 prefilter tier)** | both carry on letter BBNF-PT-G; independent code, shared placement law. |
| gated-by | **owner §3.1** | BBNF-PT-G ships as contingent proposals with patches attached (default: yes). |
| inherits | **owner §3.2 (via PT-2.W5)** | `release.yml` aarch64-only in parse-that CI is authored SOLELY by PT-2.W5 (PT-N3 = de-nightly); PT-3's 1.1.0 cut inherits it, does not re-author (default: yes, mirrors x86 abrogation). |

**Placement / execution authority (binding, non-clobber law, HANDOFF §6.9 + SPEC §2b):**
`~/Programming/parse-that` is READ-ONLY to the bbnf campaign. PT-3 is written to full
implementation depth as the tree of record, but **landed only by the owner or a parse-that-side
session** via letter BBNF-PT-G. No bbnf agent edits, stages, or commits parse-that. Prototyping
is against scratch COPIES; the single sanctioned artefact into parse-that is the coordination
letter. This tranche is the letter's PT-N2 attachment.

---

## 3. Wave manifest

| id | name | scope | LOC (+new/−del) | gate | cap |
|---|---|---|---|---|---|
| W0 | THE HANDSHAKE | consumer baselines + the 0.7→1.1 migration adjudication | +40 / −0 (scratch harness) | born-verified (migration dry-run can fail) | 20m |
| W1 | THE LEVER | land the lazy diagnostic-alloc patch (PROVE-11) | +36 / −12 | born-verified (non-regression + reproduce floors) | 25m |
| W2 | THE CONSUMER | PT-E1 per-parse-scoped diagnostics; diagnostics/ directory module | +130 / −70 | **born-RED** (parse-isolation oracle) | 40m |
| W3 | THE TIGHTENING | PT-E2 inference; kill the `Parser<any>` leaks | +80 / −34 | **born-RED** (type-level test suite) | 35m |
| W4 | THE SHAPE AUDIT | V8 hidden-class + monomorphization audit | +44 / −0 | born-verified (shape oracle + monomorph floor) | 25m |
| W5 | THE CUT | 1.1.0 semver, proof gates, consumer suites | +36 / −4 | born-verified (proof:all + 3 suites green) | 30m |
| W6 | THE RECKONING | challenge wave — adversarial re-measure of §0-§5 claims | +40 / −0 | born-RED (each claim reproduces or is delta-documented) | 30m |

---

## 4. Per-wave spec

### W0 — THE HANDSHAKE

**Objective.** Pin the three consumer baselines under the current 1.0.0 world and adjudicate the
latex-paper `^0.7.1` skew before a line changes, so that W5's "consumer suites green" gate has a
falsifiable before-state and a decided migration.

**File-level plan (scratch copies only; non-clobber).**
- Scratch: `parse-that/typescript` @ `1.0.0` pristine (upstream HEAD), node_modules symlinked —
  the PROVE-11 recipe (VERIFY §intro).
- Consumer scratch copies: `value.js` (3.1.0), `latex-paper` (0.2.1), `keyframes.js` (6.0.0).
- Baseline capture script `scratch/pt3-w0-baseline.sh` (harness only, never landed): runs each
  consumer's suite against 1.0.0 and records pass-set + count.

**Seams (R4 §7).**
- value.js `package.json:115` dep `^1.0.0`, lock → `parse-that-1.0.0.tgz`; suite `npm test`
  (`vitest`, jsdom). Imports `Parser ParserState all any dispatch regex string whitespace
  mergeErrorState` (9 sites, R4 §7).
- latex-paper `package.json:38` dep **`^0.7.1`** → resolves `0.7.x`, `<0.8.0`; suite `npm test`
  (`vitest run`). Imports **only** `Parser ParserState regex string any` (7 grammar sites).
- keyframes.js: **no direct dep** — transitive via value.js; suite `npm test` (`vitest`). Path is
  `keyframes.js`, not `keyframes` (R4 §8).

**The skew adjudication (superseding R4's 1.0.0-floor assumption).** `^0.7.1` predates the 1.0.0
breaking cut (`*Span` excision S.H2, `chain()` C-16). A 1.1.0 bump for latex-paper is therefore
an untested 0.7→1.1 jump. W0 dry-runs it: in the latex-paper scratch, `npm install` a packed
1.1.0-candidate tarball (built from the W5 tree once available; for W0, from a 1.0.0 tarball to
prove the 0.7→1.0 jump alone), run `npm test`, enumerate every break. Because latex-paper touches
only `Parser/ParserState/regex/string/any` — all still exported at 1.0.0+ and untouched by
W1-W3 — the expected result is GREEN; any red is enumerated as a W5 blocker with the exact symbol.

**Gate.** `pt3-w0-baseline.sh` records value.js / latex-paper / keyframes.js suites GREEN at
1.0.0 **and** the latex-paper 0.7→1.0 migration dry-run is GREEN **or** its exact break is
enumerated. *Failure semantics:* a red baseline means the consumer is already broken independent
of PT-3 (halt, report upstream); a red migration dry-run with an unexpected symbol break blocks
the 1.1.0 cut until W5 addresses it. This gate can fail — the migration dry-run is a real jump.

**Consumer-in-wave (Y.13).** W0's own consumer is the baseline harness that W5 re-runs; the
before/after pair is the tranche's close evidence.

---

### W1 — THE LEVER

**Objective.** Land PROVE-11's lazy diagnostic-array allocation (CONFIRMED_DELTA), productionized
— shipped source, honest bench, no hardcoded scratch paths.

**Which PROVE patch lands.** `prototypes/PROVE-11/patch.diff`, the **source portion only**:
- `src/parse/state.ts:39-46` — `suggestions`/`secondarySpans` become optional declared fields
  (`undefined` at construction; native class fields so the hidden class is fixed from birth and
  never transitions), symmetric with the pre-existing lazy `expected?`.
- `src/parse/utils.ts` — `mergeErrorState` (`:33-35`) and `resetErrorState` (`:131-136`) reset to
  `undefined` (zero alloc); `addSuggestion`/`addSecondarySpan` (`:51-62`) lazy `(x ??= []).push`;
  `collectDiagnostic` (`:102-128`) guarded spread.
- `src/parse/debug.ts:180,185` — two reads guarded `?? []` (mirrors the existing `expected ?? []`).
- `test/debug.test.ts:143-160` — two diagnostics-on assertions narrowed `?? []`.

**Adaptation notes (the productionization).** The patch also carried two measurement harnesses;
**do not land them verbatim.** VERIFY §3 flagged `test/benchmarks/pt-prove11-paired.bench.ts` as
a patch-portability wart — it hardcodes the prover's absolute scratch paths
(`patch.diff:215-216`) and trips `tsc` (TS7016). Productionize:
- Land the paired harness as `test/benchmarks/pt-diag-alloc.paired.mjs` — a custom
  `process.hrtime.bigint` cold-per-op instrument that **alternates the two arms per sample**
  (A,B,A,B…, median-of-13-batches × ≥300k ops, N≥12 rounds, min AND median, per-round loadavg) so
  load nulls out (the certified PROVE-11 method: `run-paired.sh` + `measure-prove11.mjs`, here
  fused into one process). It imports both arms via a **build-both fixture** (a small
  `test/benchmarks/_build-arms.mjs` that builds pristine-tag and working-tree dists into
  `test/benchmarks/.arms/{a,b}/`) rather than absolute paths — importing built JS keeps it portable
  and `tsc`-clean. It is deliberately **not** a `vitest bench`/tinybench file: that runner is warm,
  throughput-amortized, and cross-run sequential (arm A to steady-state, then arm B), which §1.3
  refuses as evidence. This is a **development instrument** proving the landing; the ongoing
  regression floor stays `pt-b3-fusion.bench.ts`.
- Drop `scripts/measure-prove11.mjs` from the shipped tree; keep it in the letter's evidence pack.

**File-level structure (colocation).** No new directory needed at W1 — the change is three
`src/parse/` files. The bench lands under the existing `test/benchmarks/` module.

**Gate (born-verified — non-regression + reproduce the certified floors).** Run
`node test/benchmarks/_build-arms.mjs && node --expose-gc test/benchmarks/pt-diag-alloc.paired.mjs`
— the custom `process.hrtime.bigint` harness of §1.2, per-sample paired interleaving of both
imported arms (A,B,A,B…), N≥12 rounds, packrat unarmed (default), loadavg recorded (**not**
`vitest bench`, whose warm cross-run §1.3 refuses):
- **then-chain ≥ +11%** (VERIFY aggregate +13.6%; floor set below with noise margin),
- **fused `all(a,b,c)` ≥ +13%** (VERIFY +15.6%),
- **fused alloc B/op ≤ −55%** (VERIFY −60.1%, `--expose-gc`),
- **dispatch lane carried at ~+5%, `≥ 0%` (no-regression only, NOT gated ≥10%)** — the honest
  number,
- `tsc --noEmit` clean, `npm test` **124/124** both arms, `proof:{subpath,no-css-surface,
  no-span-surface}` PASS.

*Failure semantics:* then-chain < +11% or fused < +13% or alloc > −55% ⇒ the lever regressed
under the production build (halt, re-measure fresh windows); a dispatch reading < 0% ⇒ a real
regression (halt). A dispatch reading of +8% is **not** a pass to celebrate — the honest carry is
~+5%; over-claiming trips the W6 audit.

**Oracle.** `test/debug.test.ts` (124/124) is the parity oracle — the widened field type
(`[] | undefined`) must not change any diagnostic output; VERIFY §4 confirmed zero unguarded
consumer reads (`debug.ts` guarded, `parser.ts:63-64` sink is `toString`-only, consumers never
read the fields).

**Consumer-in-wave (Y.13).** The lazy fields' first consumer is the guarded reads in `debug.ts`
+ the bench that proves the removed allocation. Their *semantic* consumer arrives in W2 — stated
below.

---

### W2 — THE CONSUMER (PT-E1: per-parse-scoped diagnostics)

**Objective.** Dissolve the two module-global diagnostics latches into per-parse scope, making
the W1 lazy fields a *live, per-parse-populated* surface. This is the Y.13 consumer for W1:
**W1 lands the substrate (the lazy fields), W2 is their consumer (per-parse population under a
scoped flag) — Y.13 is satisfied across W1→W2, not within either alone.**

**The defect (R4 §5, verified).** Diagnostics are gated by a **module-global mutable** flag
`diagnosticsEnabled` (`utils.ts:6`, set by `enableDiagnostics`/`disableDiagnostics` at 8-13) and
collected into a **module-global buffer** exposed by `getCollectedDiagnostics`/
`clearCollectedDiagnostics`/`popLastDiagnostic` (`utils.ts:138-146`). Two concurrent or nested
parses cross-contaminate: an inner parse that enables diagnostics leaks its flag and its
collected buffer into an outer parse. The `expected` set is structurally dead on the default path
(cold) because the global flag is off. PT-E1 makes diagnostics **per-parse scoped**.

**File-level plan (colocation grand edict — split the god module).** `utils.ts` (186 LOC) holds
two unrelated concerns; W2 is already rewriting the diagnostics half, so it lifts diagnostics into
a directory module and leaves the scanners:

```
src/parse/
  diagnostics/
    mod.ts        # barrel: re-exports scope + collect + the Suggestion/SecondarySpan types
    scope.ts      # DiagnosticScope: the per-parse enabled-flag + collected buffer (was the
                  #   utils.ts:6 latch + utils.ts:138-146 module-globals), owned by ParserContext
    collect.ts    # mergeErrorState / addSuggestion / addSecondarySpan / collectDiagnostic /
                  #   reportUnclosedDelimiter / resetErrorState (was utils.ts:28-136)
  scan.ts         # skipWhitespace / skipBlockComments (was utils.ts:159-186) — distinct concern
  state.ts        # ParserState gains a scope back-reference (or reads scope off ParserContext)
  parser.ts       # parseState opens/closes the DiagnosticScope in the existing try/finally
```

The `./diagnostics` subpath export already exists (`diagnostics.ts` barrel, R4 §1) — the new
`diagnostics/mod.ts` becomes its target; `utils.ts` shrinks to a scanner-only file (rename to
`scan.ts`; update the `./utils` subpath barrel `utils-entry.ts`). This is parsimony-bounded: the
split is a consequence of the latch rewrite, not a separate cleanup wave.

**The scoping mechanism.** `ParserContext` (built once per combinator at grammar-construction,
R4 §2) is the wrong lifetime — the scope must be **per-parse**, so it hangs off the parse epoch.
`parseState` (`parser.ts:34-49`) already opens a packrat epoch in try/finally; the DiagnosticScope
opens/closes in the **same** try/finally — one scope per top-level `.parse()`, threaded to the
`ParserState` (which is already per-parse, constructed in `parseStateInner`). `enableDiagnostics`
becomes a parse option (`.parse(src, { diagnostics: true })` or a scoped `withDiagnostics(fn)`),
never a module-global write. The lazy `suggestions`/`secondarySpans` fields (W1) populate under
`scope.enabled` — now genuinely per-parse.

**Downstream target.** keyframes.js `ResolvedKeyframes.diagnostics` (R4 §5, via value.js) is the
real consumer of a per-parse diagnostic set; W2 makes that surface live and non-global.

**Gate (born-RED — the parse-isolation oracle).** A new `test/diagnostics-scope.test.ts` that
**fails on the current global-flag code and passes only after per-parse scoping**:
- *Isolation:* two parses run interleaved (outer parse invokes an inner parse mid-flight); the
  inner enables diagnostics, the outer does not. Assert the outer collects **zero** diagnostics
  and the inner collects its own — the global latch makes the outer see the inner's flag+buffer,
  so this assertion is **RED at 1.0.0**, GREEN after.
- *Deletion proof (grep-gate, supplementary):* `rg 'let diagnosticsEnabled' src/` returns zero
  hits — the module-global is DELETED (precept #6), not shadowed.
- *No hot-path regression:* re-run the W1 paired harness with diagnostics off; then-chain/fused hold
  their W1 floors (the scope open/close must be free on the default path — a `null` scope or a
  single field read, no allocation when diagnostics are off).
- `npm test` 124/124 (existing diagnostics tests migrated to the scoped API), `tsc` clean.

*Failure semantics:* if the isolation test cannot pass without the global latch, the scoping is
incomplete (a leaked global remains); if the hot path regresses past the W1 floor, the scope has
a per-parse allocation cost that must be removed before close. This gate cannot be satisfied by
the pre-existing code — it is born-RED by construction.

**Consumer-in-wave (Y.13), stated.** *"W1's lazy `suggestions`/`secondarySpans` fields are the
substrate; W2's per-parse scoped population is their consumer. Neither wave lands a dangling
primitive: W1's fields are read by W2's scoped collector, W2's scope is populated into W1's
fields. Y.13 is discharged across the W1→W2 pair."*

---

### W3 — THE TIGHTENING (PT-E2: combinator-inference)

**Objective.** Extinguish the `Parser<any>` inference leaks so combinator return types infer
precisely, at **zero runtime cost** (inference is compile-time; the emitted JS is byte-identical).

**The seams (R4 §5, verified against HEAD).**
- **Duplicated `ExtractValue`:** defined at `parser.ts:9-11` and **re-defined identically inside
  `all()`** at `leaf.ts:155-157`. Consolidate to one exported utility type (`src/parse/types.ts`
  or the `parser.ts` home), imported by `leaf.ts` — DRY (precept #1).
- **`Parser<any>` leaks:** `ParserFunction<T>` erases to `ParserState<any>` (`parser.ts:14-16`);
  `any<T extends Array<Parser<any>>>` (`leaf.ts:28`); `all<T extends Array<Parser<any>>>`
  (`leaf.ts:154`); `fuseAll(parsers: Array<Parser<any>>)` (`leaf.ts:179`); `restore(saved: {…;
  value: any})` (`state.ts:79`). Pervasive `as Parser<unknown>` casts in `parser.ts`.

**File-level plan.**
- `src/parse/types.ts` (new, tiny) — the single `ExtractValue` + the shared parser-tuple helpers;
  `parser.ts:9` and `leaf.ts:155` both import it.
- `src/parse/leaf.ts` — `any`/`all`/`fuseAll` generics tightened from `Array<Parser<any>>` to
  `ReadonlyArray<Parser<unknown>>` with inference through `infer V`; the arity-unrolled arms
  (leaf.ts:39-57, 183-251) keep their runtime shape (W4 guards this) but gain precise positional
  types.
- `src/parse/state.ts:79` — `restore(saved: { offset: number; value: T })` typed on the state's
  own `T`, not `any`.
- `src/parse/parser.ts:14` — `ParserFunction<T>` retains its documented type-erased pointer at
  the *implementation* boundary (the megamorphic IC needs variance, R4 §6c) but the *public*
  combinator signatures stop leaking it into user-facing return types. This is the one place where
  a bounded internal `any` is legitimate (an implementation-boundary variance requirement, not a
  leak); it is documented as such and **not** counted as an open leak by the W3 test suite.

**Gate (born-RED — a type-level test suite).** A new `test/types/inference.test-d.ts` (using
`expect-type` / `@ts-expect-error` assertions, run under `tsc --noEmit` in the type-test project)
that is **RED on 1.0.0 and GREEN after**:
- `all(string("a"), regex(/\d/))` infers `Parser<[string, string]>`, **not** `Parser<any>`;
- `any(string("a"), string("b"))` infers `Parser<string>`;
- a `@ts-expect-error` on a genuinely wrong shape still errors (the suite proves precision both
  ways — it must reject over-widening AND catch true mismatches);
- `rg 'as Parser<' src/parse/` count strictly decreases from the 1.0.0 baseline (grep-gate,
  supplementary — casts are the smell, the type-test is the artefact).

Plus: `npm test` **124/124 unchanged** (runtime parity — the emitted JS must be byte-identical;
prove with a `vite build` diff of `dist/` before/after being empty of semantic change), `tsc`
clean, and the **zero-runtime-cost invariant** held (the type plane is compile-time only — SPEC's
verified invariant for the Rust type plane, mirrored here).

*Failure semantics:* a leak that closes only by inserting a runtime cast or altering emitted JS is
a **defect** (it violates zero-cost) — the wave halts. A consumer (value.js's 59 `all()` sites,
latex-paper's grammar) whose types break under the tightening is a real semver signal escalated to
W5. The suite is RED by construction on the current leaks.

**Consumer-in-wave (Y.13).** The type-test suite is the immediate consumer; the enduring consumers
are value.js's `all()`/`any()`/`dispatch()` call sites, whose inferred types sharpen — W5's
consumer suites confirm no runtime break.

---

### W4 — THE SHAPE AUDIT

**Objective.** Prove — by measurement, not assertion — that W1-W3 preserved the deliberate V8
monomorphizations and that no hidden-class transition crept onto the hot path. This is a
verification wave: it changes (almost) no source, it pins invariants with oracles.

**File-level plan.**
- `test/v8-shapes.test.ts` (new) — a shape-stability oracle run under `node
  --allow-natives-syntax` (via a small runner, since vitest does not expose `%HaveSameMap`
  directly): construct two `ParserState`s, one that never touches diagnostics and one that
  populates `suggestions`, assert `%HaveSameMap(a, b)` is **true** (W1's fixed-from-birth claim —
  the field is `undefined` vs a fresh array, same hidden class). Assert `mapState`'s
  `Object.create(state)` view (parser.ts:177-180, R4 §6b) is the *only* prototype-chain object and
  its callers are not deopted.
- `test/benchmarks/pt-monomorph.paired.mjs` (new) — the arity-unrolled floor, a custom
  `process.hrtime.bigint` cold-per-op harness (**not** a `vitest bench`/tinybench file, for the
  §1.3 warm-bench reason): `any`/`fuseAll` arity-2 (leaf.ts:39-57, 183-212) and arity-3 (214-251)
  unrolled arms must still beat the general `for`-arm at their arities; a regression means W3's
  generic tightening perturbed the emitted closures. It **alternates the unrolled and general arm
  per sample** (A,B,A,B…, median-of-13-batches × ≥300k ops, N≥12 rounds); single-process, packrat
  state disclosed.

**The armed/unarmed disclosure (binding, §1.2 / §0).** Every perf table in W4 (and re-stated in
W5/W6) carries the packrat state: **unarmed** (default LL(1), `PACKRAT_ARMED` false until first
`memoize()`, packrat.ts:156/290) is the shipping hot path; an **armed** column is included only
where a consumer arms it. No table omits the state.

**Gate (born-verified — shape oracle + monomorph floor).**
- `node --allow-natives-syntax test/v8-shapes.runner.mjs` asserts same-map TRUE for the
  diagnostics-off vs diagnostics-populated `ParserState` (W1 invariant holds);
- `node test/benchmarks/pt-monomorph.paired.mjs` shows arity-2/3 unrolled arms ≥ general arm at
  matched arity (the monomorphizations survive W3);
- no new prototype-chain object beyond the documented `mapState` view.

*Failure semantics:* a same-map FALSE means W1/W2 introduced a hidden-class transition (halt —
the lazy-field claim is broken); an unrolled arm slower than the general arm means W3's inference
changes perturbed codegen (halt — revert the perturbing signature). This gate can fail — it is a
real measurement of a real invariant.

**Consumer-in-wave (Y.13).** The oracle consumes the W1 shapes and the W3 signatures directly;
it is the guardrail that lets W5 claim "monomorphizations preserved" with evidence.

---

### W5 — THE CUT (1.1.0)

**Objective.** Ship `1.1.0`: semver-correct, eight proof gates green, three consumer suites green
against the candidate.

**File-level plan.**
- `package.json` `version` `1.0.0 → 1.1.0`; `CHANGELOG.md` entry (the lazy-alloc lever, per-parse
  diagnostics API, tightened inference; all additive/non-breaking).
- `docs/` — the `./diagnostics` subpath doc gains the per-parse scoped API (W2); the diagnostics
  migration note (module-global → scoped) for any downstream still calling `enableDiagnostics()`.
- `release.yml` aarch64-only CI is already cut by **PT-2.W5** (owner §3.2, the x86 abrogation
  guard rails); the 1.1.0 cut inherits it — no re-authoring here.

**Semver review.** Minor bump justified: **additive** per-parse diagnostics API (`withDiagnostics`
/ parse option), **tightened** inference (source-compatible — precise types are a superset of
`any` for well-typed consumers), lazy fields widened `[] → [] | undefined` (non-breaking; VERIFY
§4 + R4 §7 confirm zero consumer reads). The one migration risk — a downstream calling the deleted
module-global `enableDiagnostics()` — is caught by the consumer suites; if any consumer used it,
the deletion is a **breaking** change forcing a 2.0.0 reconsideration (adjudicated here, not
assumed). No `*Span` reintroduction (proof:no-span-surface guards it).

**Gate (born-verified — the close gate).**
- `npm run proof:all` GREEN: `manifest`, `no-css-surface`, `subpath`, `packrat-{cross-input,
  reentrant,large-offset,armed}`, `no-span-surface`, `no-dead-combinator`, `perf`. (The
  `proof:perf` clause-C cross-machine absolute-ns baseline is rebuilt on the cut host via
  `UPDATE_PERF_BASELINE=1`, per its own note — VERIFY §3 established it is machine-relative, not a
  regression.)
- **The three consumer suites GREEN against the 1.1.0 candidate** (packed tarball installed into
  each scratch): value.js re-locked `1.0.0 → 1.1.0` `npm test`; latex-paper `0.7.1 → 1.1.0`
  migration (from W0) completed `npm test`; keyframes.js transitive `npm test`.
- `tsc` clean, `npm test` 124/124 + the new W2/W3/W4 tests.

*Failure semantics:* any proof gate red blocks the cut; any consumer suite red is a real break —
if it traces to the tightened inference it is a semver escalation (minor→major), if to the
diagnostics deletion it forces the enableDiagnostics compat decision. This gate is the tranche's
terminal falsifier.

**Consumer-in-wave (Y.13).** The three consumer suites are the wave's consumer — the whole point
of a backbone cut is that its consumers survive it.

---

### W6 — THE RECKONING (challenge wave)

**Objective.** A non-author adversary re-measures and re-audits every load-bearing PT-3 claim in
fresh windows, exactly as PROVE-11's VERIFY halved the prover's dispatch figure. The tranche's own
numbers are the target.

**Attack list.**
1. **Re-measure W1 in fresh windows (≥2, matched load).** Does then-chain hold ≥+11%, fused
   ≥+13%, alloc ≤−55%? Does dispatch stay honest at ~+5% — or did the landing quietly inflate it?
   Reproduce or delta-document (the VERIFY precedent: a claim that does not reproduce is stated,
   not buried).
2. **Re-run the W2 isolation oracle** under nested and concurrent parses; confirm the global latch
   is truly gone (`rg 'let diagnosticsEnabled'` = 0) and no per-parse allocation slipped onto the
   diagnostics-off path.
3. **Re-audit W3:** confirm no leak closed via a smuggled `as any` / runtime cast; the `dist/`
   diff is semantically empty (zero runtime cost held); the `@ts-expect-error` assertions still
   fire (the suite catches true mismatches, not just over-widening).
4. **Re-check W4:** same-map TRUE reproduces; monomorph floor holds; every table states packrat
   state.
5. **Re-audit the semver claim** against the real consumer `dist/` diffs — is 1.1.0 actually
   non-breaking, or does a consumer type break under tightening?
6. **Edict sweep:** no x86/target tokens (pure TS), no name-gates, no stubs/TODO/shim, parsimony
   held (net LOC within budget).

**Gate (born-RED — reproduce-or-document).** Every W1-W5 numeric and structural claim either
reproduces within noise **or** its exact delta is documented with the fresh measurement (the
CONFIRMED_DELTA discipline). A claim that neither reproduces nor is honestly delta-documented is a
**failed** challenge — the tranche does not close. *Failure semantics:* an inflated W1 number
caught here is redressed to the honest figure before the letter ships; a leaked global caught in
attack 2 reopens W2; a broken consumer in attack 5 reopens W5's semver decision.

**Consumer-in-wave (Y.13).** The challenge consumes the entire tranche's claim surface; its output
is the VERIFY-grade evidence pack attached to letter BBNF-PT-G.

---

## 5. LOC ledger

| wave | +new | −del | net | note |
|---|--:|--:|--:|---|
| W0 | +40 | 0 | +40 | scratch harness, not landed in package |
| W1 | +36 | −12 | +24 | source ~14 net (PROVE-11) + productionized bench |
| W2 | +130 | −70 | +60 | diagnostics/ directory module; utils.ts→scan.ts split |
| W3 | +80 | −34 | +46 | types.ts + tightened generics + type-test suite |
| W4 | +44 | 0 | +44 | shape oracle + monomorph bench |
| W5 | +30 | −4 | +26 | version/changelog/docs (release.yml inherited from PT-2.W5) |
| W6 | +40 | 0 | +40 | challenge measurement (evidence pack) |
| **total** | **+400** | **−120** | **+280** | landed-in-package net ≈ +162 (W0/W6 harness excluded) |

Generated-file budget: N/A (parse-that TS is hand-written, not generated — PROVE-11 REPORT §5).

---

## 6. Exit criteria

1. W1 lever landed, non-regression + certified floors reproduced (dispatch honest ~+5%).
2. W2 per-parse diagnostics scoped; both module-globals deleted; isolation oracle GREEN; Y.13
   discharged across W1→W2.
3. W3 `Parser<any>` leaks closed; type-test suite GREEN; zero runtime cost proven by empty
   `dist/` semantic diff.
4. W4 same-map invariant + monomorph floor proven; packrat state disclosed everywhere.
5. W5 1.1.0 cut: proof:all GREEN, three consumer suites GREEN, semver adjudicated.
6. W6 challenge: every claim reproduces or is delta-documented; evidence pack assembled.
7. The tranche ships as the PT-N2 attachment to letter **BBNF-PT-G-2026-07-16**, placed only by
   the owner or a parse-that-side session (non-clobber law).

---

## 7. Risks with named counterexamples

- **R1 — the dispatch lane tempts inflation.** *Counterexample:* a landing session re-runs W1 in
  a single favorable window, reads +9% on dispatch, and reports it as "near the gate." This is the
  exact §0/09 self-deception; W1's gate carries dispatch at ~+5% no-regression, and W6 attack 1
  re-measures in two windows. The honest number is the number.
- **R2 — the per-parse scope adds a hot-path allocation.** *Counterexample:* W2 threads a scope
  object allocated per `.parse()`, regressing the diagnostics-off path below the W1 floor. Cure:
  the scope is a `null`-or-field read on the default path (no allocation when diagnostics off);
  the W2 gate re-runs the W1 paired harness and holds the floor, or the wave does not close.
- **R3 — inference tightening breaks a consumer's types.** *Counterexample:* value.js's 59
  `all()` sites or latex-paper's grammar rely on the `any` widening to type-check; the W3 precise
  types reject them. Cure: W3's type-test proves precision is a *superset* for well-typed sites;
  W5's consumer suites are the falsifier — a real break escalates semver to 2.0.0, decided not
  assumed.
- **R4 — the latex-paper 0.7→1.1 jump breaks silently.** *Counterexample:* latex-paper's `^0.7.1`
  pin means it never saw the 1.0.0 `*Span`/`chain()` cut; a 1.1.0 bump surfaces a 1.0.0 break
  mislabeled as a 1.1.0 one. Cure: W0 dry-runs the 0.7→1.0 jump *alone* first, isolating which cut
  owns any break before 1.1.0 is blamed.
- **R5 — the utils.ts split over-reaches.** *Counterexample:* W2 refactors the whitespace scanners
  too, sprawling beyond the diagnostics concern and violating parsimony. Cure: the split is
  bounded to *lifting* diagnostics into `diagnostics/`; the scanners move only as far as a rename
  to `scan.ts` (a concern-separation the god-module precept #14 already requires), no logic change.
- **R6 — a bbnf agent edits parse-that.** *Counterexample:* an executing session, seeing full
  file-level specs, writes into `~/Programming/parse-that`. This violates the non-clobber law
  (HANDOFF §6.9). Cure: PT-3 is landed ONLY by the owner or a parse-that-side session; bbnf-side
  work is scratch-copy prototyping and the coordination letter, nothing else.
