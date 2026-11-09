# LOCK-8 · FAMILY 8A — THE SHAPE-STABLE CORE (research, pass 3)

*The V8-first formulation of the parse-that TS combinator core: every state and combinator
object shape-stable and monomorphic by construction, an allocation-zero hot path, diagnostics
and packrat as shape-preserving optional planes. Numbers herein are cold, paired, and disclose
armed/unarmed packrat; the V8 JIT and the TS checker are never conflated. Every measurement
NAMES its HEAD. Corpus of record measured directly this pass, not carried from prior text.*

---

## 0. THE DECISIVE FINDING — the arity-N specializer's consumer is being deleted (CONSUMER LIVENESS, P1)

The pass-2 named risk **R-8A-1** ("the eroding consumer") has **materialized and is staged for
commit**. Measured this pass against `~/Programming/value.js` at branch `tranche-u`, HEAD
`86a9a17`, working-tree version **4.0.0**:

- `git status -s src/parsing` shows the **entire `src/parsing/` combinator layer STAGED FOR
  DELETION** (`D ` in column 1): `color/color.ts`, `math.ts`, `units.ts`,
  `stylesheet/stylesheet.ts`, `timeline/easing.ts`, `timeline/scroll-timeline.ts`, `index.ts`,
  `utils.ts` — every parse-that import site R4 §7 enumerated.
- `git diff HEAD -- package.json` **removes** `"@mkbabb/parse-that": "^1.0.0"` with **no
  replacement line** — the dependency itself is dropped, and the version bumps `3.1.0 → 4.0.0`
  (a major, semver-consistent with an API departure).
- The replacement `src/v4/` (`color/`, `css/`, `value.ts`, `result.ts`, `quantize.ts`,
  `easing.ts`) is **parse-that-FREE** (`grep -rln parse-that src/v4` → 0 hits) — it hand-rolls
  parsing over its own `Result`/`ok`/`err` monad, exactly the trajectory pass-2 flagged.

**Posture answer (P1): this is an INTENTIONAL, staged removal, not a mid-migration WIP artifact.**
139 commits separate `tranche-t-close` from HEAD; the deletions are staged (index), not idle
working-tree noise. value.js is departing the combinator surface entirely.

The combinator corpus therefore exists ONLY at the frozen `tranche-t-close` tag (≡ committed
value.js 3.1.0). Its site census, measured directly:

| file (tranche-t-close) | `all(` | `any(` |
|---|--:|--:|
| color/color.ts | 9 | 33 |
| math.ts | 10 | 9 |
| units.ts | 7 | 5 |
| stylesheet/stylesheet.ts | 12 | 7 |
| timeline/easing.ts | 4 | 1 |
| timeline/scroll-timeline.ts | 5 | 5 |
| **TOTAL** | **61** | **93** |

**Consequence for 8A.** The arity-N build-time specializer (pass-2 §3) was justified by
value.js's high-arity heterogeneous `all()`/`any()` mass. That mass is a **frozen historical
artifact on a tag**, not a live forward-looking consumer. Under the Constitution's clause viii
(Y.13, consumer-less substrate forbidden) and clause vii (concrete artifacts, no vague optimism),
**an arity-N specializer built for value.js's departed corpus is consumer-less substrate.** The
family's center of gravity must move OFF the fusion specializer.

## 1. The surviving consumer set (CONSUMER LIVENESS, P2/P3) — measured

| consumer | HEAD / version | parse-that dep | installed | live combinator sites | disposition |
|---|---|---|---|--:|---|
| **value.js** | `tranche-u` / **4.0.0** | **REMOVED** (was `^1.0.0`) | — | **0** (154 deleted) | **DEPARTING** |
| **latex-paper** | `c140473` / 0.2.1 | `^0.7.1` | **0.7.1** | **3 `all` + 3 `any`** | frozen at 0.7.x |
| **keyframes.js** | 6.0.0 | transitive via value.js | — | 0 direct | vanishes with value.js |

- **latex-paper (P3):** ACTIVE repo (recent commits `c140473`/`f246a11`, "VIRT"/glass-ui work)
  but its parse-that usage is **6 total combinator sites** pinned to `0.7.1` — which **predates
  the 1.0.0 breaking cut** (`*Span` excision, `chain()` C-16). It imports only
  `Parser ParserState regex string any`. It is **not tracking 1.x** and its arity surface is
  trivial (arity 2–3). It is effectively frozen against the current cut, not a migration
  candidate that pulls fusion work forward.
- **keyframes.js:** transitive-only; if value.js drops parse-that, keyframes' transitive edge is
  severed. Not an independent consumer of the combinator surface.

**Net:** the only living external consumer of the combinator surface post-value.js-4.0.0 is
latex-paper's **6 low-arity sites on a 0.7.x pin**. This is the empirical ground the whole 8A
fusion investment now stands on, and it does not support an arity-N specializer.

## 2. The emitter relationship (8A directive 3) — they are NOT one mechanism

**There is no build-time TS combinator emitter in parse-that.** `grep -rlniE
"emit|fuse|specializ|codegen" typescript/src` returns only `leaf.ts`; `fuseAll` (`leaf.ts:179`)
is called **at grammar-construction runtime** inside `all()` (`leaf.ts:161`), not by any build
step. `typescript/scripts/` holds only proof gates (`proof-*.mjs`) and `perf-baseline.json` — no
codegen. The arity-2/3 "specialization" is a **runtime construction-time branch** on
`parsers.length`, not emitted source.

The XL-D "build-time TS EMITTER face" is a **distinct mechanism**: per PT-3 §2 it is the GN-4.W4
**standalone tape decoder** — a monomorphic decoder whose oracle is `JSON.parse` and which
**imports no combinators**. It has no overlap with combinator fusion; it cannot host an arity-N
combinator specializer, and no other build-time emitter exists on the TS side. **Answer: the
arity-N specializer is neither one mechanism with XL-D nor constructible on today's TS side
without net-new codegen infrastructure** — infrastructure whose sole justification (§0) is
departing. Building it would be pure speculative substrate.

## 3. Maglev inlining, per arity (8A directive 1) — structural leg PARTIAL, honestly disclosed

Node **v26.0.0**, `process.config.variables.v8_enable_maglev = 1` (Maglev is the DEFAULT
production tier; TurboFan is reached only on sustained hotness). Probe at scratchpad
`maglev-arity.mjs` / `mg2.mjs`, driving positional-unrolled vs indexed-loop fused closures over
**genuinely distinct function literals** (distinct SharedFunctionInfo — true SFI-heterogeneity,
the discrimination pass-2 §2 established; a first probe draft that shared one literal across the
arms was corrected).

**What reproduced:** under `--trace-maglev-inlining`, small positional callees are taken with
"**greedy inlining … small function, skipping max-depth**" (`== Inlining <SharedFunctionInfo>`).
The positional unrolled site's constant-bound callees are inline-eligible; the mechanism V8 uses
is confirmed as the documented small-callee greedy path.

**What did NOT reproduce cleanly and is named as the sharpest structural residual:** a rigorous
**per-arity homogeneous-vs-heterogeneous inlining table under BOTH Maglev and TurboFan** did not
complete. Two honest obstacles: (a) `%OptimizeFunctionOnNextCall` requires
`%PrepareFunctionForOptimization` first in this V8 build, so forced tier-up needs the full
prepare/optimize dance; (b) a short-running probe does not reliably reach TurboFan, so the
indexed-loop's megamorphic-non-inline claim at `d ≥ 5` is **not yet measured**, only argued from
the IC-degree model. **Per the honest-number law (SPEC §0/09), I report this as un-run rather
than assert the per-arity table.** The temporal leg (paired ns) is NOT taken here — it must be
reported SEPARATELY from this structural leg (pass-2 §4 mandate), and it is moot until §0 resolves
whether any consumer justifies the arm at all.

**The arity histogram the directive demands (from the ACTUAL live corpus) is now un-takeable as a
forward measurement:** value.js's corpus is deleted, latex-paper's is 6 trivial sites. The
histogram can only be reconstructed from the frozen `tranche-t-close` tag (61 `all`/93 `any`;
pass-2 sampling placed arities at 2–6) — a **historical** distribution, which by the same Y.13
logic cannot justify a live specializer.

## 4. Where 8A's value actually survives — the shape-stable core minus the fusion bet

Stripped of the departing fusion justification, the family's DEFENSIBLE, consumer-backed core is:

1. **The certified lazy-diagnostic-alloc lever (structural leg).** PROVE-11 CONFIRMED_DELTA:
   emitted-JS eager-alloc census **6 → 0** (`state.ts:44-45` `suggestions`/`secondarySpans`
   eager `= []` → lazy `undefined`); fused alloc **1.84 → 0.74 B/op (−60.1%)** exact under
   `--expose-gc`. Load-immune, reproduces, and its consumer (per-parse diagnostics, PT-3 W2) is
   real. **The dispatch temporal claim is ~+5%, NOT +10.2% (VERIFY halved it) — carry ~+5%.**
2. **The fixed-shape `mapState` view.** `parser.ts:177` `Object.create(state)` → a `ReadonlyState<T>`
   struct literal; zero-consumer (grep across all three trees → 0 `mapState` reads), so the fix
   is unconditional and typed with no `as unknown as` (pass-2 §0). A pure shape-stability win
   gated only by the combinator's own unit test.
3. **Packrat shape-stability.** A memoized parser IS `Parser<T>` (`packrat.ts:280-283`,
   `:482-487`) — shape-identical, memo logic inside the `.parser` closure. The sole divergence is
   the cold `MemoCell.ans: Answer|LR` union (`packrat.ts:125-129`), quarantined behind the
   `PACKRAT_ARMED` latch (`packrat.ts:156/290`) — a true no-op until first `memoize()`. The
   shipping hot path is unarmed; every table states it.

These three are consumer-backed and measurable **today**. The arity-N specializer is not.

## 5. Named risks / counterexamples

- **R-8A-1 (now MATERIALIZED → escalate to KILL-candidate).** The arity-N build-time specializer
  is consumer-less substrate: value.js 4.0.0 stages the deletion of all 154 sites; latex-paper
  carries 6 trivial arity-2/3 sites on a 0.7.x pin. *Counterexample to keeping it:* investing a
  net-new build-time emitter (which does not exist, §2) for a corpus on a frozen tag violates
  Y.13 and Constitution vii/viii. The specializer should be **RETIRED or reduced to the existing
  runtime arity-2/3 branch**, pending an owner statement that some future consumer will exercise
  high-arity heterogeneous `all()`.
- **R-8A-2 (over-claiming the maglev leg).** *Counterexample:* asserting "positional inlines,
  loop is megamorphic, at every arity under both tiers" from a probe that only reached Maglev and
  only confirmed small-callee greedy inlining. §3 reports the gap honestly instead.
- **R-8A-3 (keyframes gate is phantom).** keyframes.js has no direct dep; its transitive edge
  dies with value.js's departure. Do not treat it as an independent 8A gate.
- **R-8A-4 (latex 0.7→1.x jump).** Any 1.x consumer claim for latex-paper is an untested
  0.7.1→1.x migration (pre-`*Span`-excision); it is a migration cost, not a live-corpus datum.

## 6. Integration seams (path:line, all in `~/Programming/parse-that/typescript`)

- `src/parse/leaf.ts:154-164` `all()` → `:179-273` `fuseAll` (arity-2 `:183-212`, arity-3
  `:214-251`, general loop `:255-272`) — the RUNTIME fuser; **no build-time emitter exists**.
- `src/parse/leaf.ts:28-79` `any()` (arity-2 unroll only; the 93-site historical surface).
- `src/parse/leaf.ts:62`, `:261` — the indexed-loop megamorphic call `parsers[i].parser(state)`.
- `src/parse/state.ts:44-45` — lazy `suggestions`/`secondarySpans` (certified structural leg).
- `src/parse/parser.ts:177` — `Object.create(state)`, the fixed-shape-view fix site.
- `src/parse/state.ts:173-189` + `:141-171` — `context.name`/`parserNames`, the runtime SFI proxy
  for computing `d = |distinct SFIs|` at construction (still valid IF a consumer ever returns).
- `src/parse/packrat.ts:125-129` (`MemoCell` union), `:156/:290` (arming), `:280-283`/`:482-487`
  (`memoize` return `Parser<T>`).
- value.js `src/parsing/` — **staged DELETED** at `tranche-u` HEAD `86a9a17`; last live at tag
  `tranche-t-close`. latex-paper `src/` — 6 sites on `@mkbabb/parse-that@0.7.1`.

## 7. Prior art

- **V8 SFI-keyed inline caches / IC-degree model** (monomorphic → polymorphic ≤4 → megamorphic):
  the positional-unroll cure is the standard "split a megamorphic site into monomorphic sites"
  technique; Maglev's small-callee greedy inliner is the documented path (`--trace-maglev-inlining`
  "small function, skipping max-depth"), more conservative than TurboFan's — confirming pass-2's
  demand that BOTH tiers be traced.
- **Build-time monomorphization** is the rustc/GHC `#[specialize]`/`SPECIALISE` analogue; the §2
  finding is that parse-that has **no** emitter to host it (unlike rustc's MIR monomorphizer),
  so the analogy does not transfer without net-new infrastructure.
- **Warth–Douglass–Millstein** left-recursion-in-packrat (`Answer`/`LR`/`Head` + `getCijKey`);
  the shape-stability framing is novel to the V8 setting, not to packrat.

## 8. What a prototype must test to move family 8A (pass-3 exit)

1. **OWNER LIVENESS ADJUDICATION (blocking).** Confirm value.js 4.0.0's parse-that departure is
   the intended terminal state. If yes, the arity-N specializer is RETIRED (§5 R-8A-1) and 8A
   reduces to §4's three consumer-backed levers. If a future high-arity consumer is named, the
   specializer re-scopes to it — but not to value.js's frozen tag.
2. **Complete the maglev/turbofan per-arity structural table** (§3) using
   `%PrepareFunctionForOptimization` + `%OptimizeFunctionOnNextCall` and a sustained-hotness
   driver, homogeneous vs heterogeneous, arities 2–8, under `--trace-maglev-inlining` AND
   `--trace-turbo-inlining`; report structural (inline yes/no) and temporal (paired ns) legs
   SEPARATELY. Owed regardless — it also governs whether the existing arity-2/3 runtime unroll
   earns its emitted bytes on latex-paper's arity-2/3 sites.
3. **Re-verify the three surviving levers on latex-paper's ACTUAL 6 sites** (the only live
   external corpus): lazy-alloc lever non-regression, `mapState` fixed-shape (0 consumers → GREEN
   trivially), packrat unarmed no-op. These are the consumer-backed close evidence 8A can honestly
   ship.
