# PROTO-1 — V8 constant-capture inlining + mapState map-identity (family 8A)

**Lock / plane:** LOCK-8 THE TS BACKBONE PLANE (`parse-that/typescript`), pass 1, probe 1.
**Settles:** 8A closure-core-made-monomorphic thesis (and 8C-ambitious / 4C from the V8 side).

## Question

Does TurboFan actually inline the child `p0.parser(state)` call in the arity-unrolled
`all()` arm but **not** in the general index-threaded arm — and is `mapState`'s
`Object.create(state)` view (parser.ts:177) really a distinct hidden class per parse (the
de-opt)? One measurement decides whether 8A's "closure core made monomorphic" thesis holds
or concedes to a VM core V8's C1 tier already flattened (−14%).

## Method (what actually ran)

- **Setup:** `rsync` scratch copy of `~/Programming/parse-that/typescript` @ HEAD (pkg
  `1.0.0`, git `ef10d5b`), `node_modules` symlinked (PROVE-11 recipe / R4). **Node v26.0.0.**
  parse-that left READ-ONLY; all edits in `pt-scratch-ts`.
- **Runner:** `test/v8-shapes.runner.mjs` + `test/v8-inline-hetero.mjs` (copied into this
  dir). Real `ParserState` / `string` / `all` / `regex` / `whitespace` / `jsonParser`
  imported from the **shipped 1.0.0 dist** (`dist/parse.js`). The two `fuseAll` arms are
  **byte-identical named replicas** of `leaf.ts:183-272` — the bundled dist chunk is
  minified/anonymous, so named replicas give greppable `--trace-turbo-inlining` lines while
  preserving the exact child call-site shapes (`p0.parser(state)` vs `parsers[i].parser(state)`).
- **Flags:** `--allow-natives-syntax` for `%HaveSameMap` / `%GetOptimizationStatus`;
  `--no-maglev --no-optimize-on-next-call-optimizes-to-maglev --trace-turbo-inlining
  --trace-opt` to force a single **synchronous TurboFan** compile (Node 26 tiers to Maglev
  by default; `%GetOptimizationStatus` numeric bits are unreliable in this V8 — the
  `--trace-opt` `target TURBOFAN_JS` line is authoritative). `--trace-deopt` for the sweep.
- **Load recorded** (perf-hygiene; these are *deterministic VM facts*, not timings, so load
  does not invalidate them): loadavg 10.5 → 15.2 → 8.9 across the run.

## The numbers

### Probe 1 — mapState map-identity (`%HaveSameMap`)

| case | construct | `%HaveSameMap(v0,v1)` | expected |
|---|---|---|---|
| 1a real `.mapState()` grammar, 2 `.parse()` runs | shipped `Object.create(state)` view | **`false`** | FALSE |
| 1b mechanism, 2 distinct source states | shipped `Object.create(state)` | **`false`** | FALSE |
| 1b mechanism, fix | `new StateView(src,value,offset,isError,furthest)` | **`true`** | TRUE |
| 1b view vs base | shipped view vs its `ParserState` | `false` | (distinct class) |

→ **The shipped view is a distinct hidden class per parse; the fixed-shape record collapses
all views onto one map.** PROBE1 = PASS.

### Probe 2 — child `.parser` inlining (TurboFan, `target TURBOFAN_JS` confirmed for both arms)

**Homogeneous string leaves** (the brief's literal corpus, `all(a,b,c)` / `all(a,b,c,d,e)`,
all `string()` len-1 → **one shared `stringParser` SFI**):
- `fusedArity3`: child `stringParser` **inlined** (3 monomorphic sites).
- `fusedGeneral`: child `stringParser` **also inlined** — the single loop site is
  *monomorphic-on-SFI* (5 closure instances, one SFI), so it is **not** megamorphic.

**Heterogeneous leaves** (the real value.js `calc/rgb/hsl` shape — 5 distinct SFIs:
`string("a")` len-1, `string("xx")` len>1, `regex`, `.map`, `whitespace`):

| arm | call site(s) | leaf `.parser` inlined? |
|---|---|---|
| `fusedArity3Hetero` (3 unrolled sites) | 3 monomorphic | **YES** — 2×`stringParser` + `regexParser` all inlined |
| `fusedGeneralHetero` (1 loop site, 5 SFIs) | 1 megamorphic | **NO** — only `ok` + `unsafeSetValue` inline; **zero leaf parsers** |

→ **The arity-3 child inlines; the general-arm child does NOT inline when leaves are
SFI-heterogeneous.** The constant-capture thesis is **CONFIRMED**, with the load-bearing
mechanism made explicit: the unrolled arms convert a would-be-megamorphic single loop site
into N separate monomorphic sites. It is **SFI-heterogeneity**, not arity, that flips the
general arm off — homogeneous leaves inline in both arms.

### Probe 3 — deopt sweep (200k `jsonParser` + 200k synthetic CSS-value triple, steady state)

3 eager deopts total, **none in the parser hot path, none recurring**:
1. `next` / "wrong map" — Node startup internal (before the loop).
2. runner top-level fn / "prepare for OSR" — one-time loop tier-up.
3. runner top-level fn / "Insufficient type feedback for binary operation" — one-time
   first-TF specialization of the same loop counter.

→ **Zero steady-path deopts** in `stringParser` / `regexParser` / fused arms / `jsonParser`
combinators / `mapState`. A recurring steady-path deopt would number in the hundreds. PASS.

## Verdict

**PASS — the 8A closure-core-made-monomorphic thesis HOLDS.** Cold-deterministic VM facts,
Node v26, loadavg recorded.

- (a) 1.0.0 `mapState` views differ (`%HaveSameMap=false`) **AND** the fixed-shape record
  makes them identical (`true`). ✔
- (b) the child `.parser` **inlines** in the arity-3 arm **AND does not inline** in the
  general arm (for the real value.js SFI-heterogeneous leaves). ✔
- (c) **zero** steady-path deopts. ✔

BLOCKED-SHADOW is **not** made fact (the arity-3 child DID inline). 8A does **not** collapse
to the ~+5% megamorphic-call residue.

**Nuance carried forward for the plan:** the general-arm inlining loss is gated on
**SFI-heterogeneity** of the leaf list, not arity per se. Unrolling only buys the inline when
the fused list mixes leaf kinds (the value.js `calc/rgb/hsl` case does); homogeneous lists
(e.g. `all` of same-length string literals) already inline in the general loop. The mapState
fix (fixed-shape `StateView`) is an independent, unconditional win.

## Deliverables in this dir

- `v8-shapes.runner.mjs`, `v8-inline-hetero.mjs` — the probes (run cmds in headers).
- `mapState-fixed-shape-view.patch` — the parser.ts:177 + state.ts fix (unified diff).
- `evidence-probe1.txt`, `evidence-inlining-hetero.txt`, `evidence-deopt.txt` — trace excerpts.
