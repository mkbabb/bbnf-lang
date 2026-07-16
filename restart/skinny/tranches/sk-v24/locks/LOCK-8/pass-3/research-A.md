# LOCK-8 / family 8A — THE V8 RUNTIME PLANE (SHAPE-STABLE CORE) — pass-3 research

The V8-first formulation of the parse-that TypeScript combinator core: every `ParserState` and
combinator closure shape-stable and monomorphic **by construction**, an allocation-zero default
(LL(1), packrat-unarmed) hot path, diagnostics and packrat as shape-preserving optional planes,
proven by IC-state probes and paired cold harnesses. The engine substrate is
`~/Programming/parse-that/typescript/src/parse/` (READ-ONLY; any landing rides letter BBNF-PT-G,
prototypes on scratch copies only). All measurements name Node **v26.0.0** (V8 `14.6.202.33-node.19`)
on Apple Silicon; the parse-that TS package is frozen at `1.0.0` (upstream HEAD `ef10d5b`).

---

## 1. The mechanism made concrete — two legs, reported separately

The shape-stable core has **two physically distinct wins** the directive requires be kept apart;
conflating them into a single ratio is the family's principal reporting hazard (it is the exact
self-deception that halved PROVE-11's dispatch claim, §0/09).

**STRUCTURAL leg (allocation + hidden-class; JIT-tier-INDEPENDENT).** Present in the interpreter,
Sparkplug, Maglev, and Turbofan alike. Substrate:
- `ParserState` is fixed-from-birth: `expected?`, and (post-lever) `suggestions?`/`secondarySpans?`,
  are declared native class fields initialized to `undefined`, so the hidden class is stamped at
  construction and never transitions when a diagnostic later seeds an array (`state.ts:36-53`, esp.
  `state.ts:43-45`). The certified lever removes **6 eager `= []` alloc sites → 0** in the emitted
  `dist/` (ctor ×2, `mergeErrorState` furthest-advance ×2, `resetErrorState` ×2), converting them to
  `void 0` stores; the `??= []` populate sites live inside the `if (diagnosticsEnabled)` guard the
  default path never enters. Corroborated by fused alloc **−60.1% B/op** (exact, `--expose-gc`;
  PROVE-11 VERIFY §1). This leg is REAL and largely BANKED — it is 8A's defensible claim.
- The arity-unrolled `all`/`fuseAll` closures build **exactly one flat result array** grown by an
  index cursor with `out.length = w` trim (`leaf.ts:191,209,222,248,257,270`), never the N−1 nested
  2-tuples the unfused `a.then(b).then(c)` allocates (`parser.ts:90`), and never a `for…of` iterator
  object (classic indexed `for`, `leaf.ts:58-73,255-272`).

**TEMPORAL leg (JIT inlining; tier-DEPENDENT).** The V8-discipline core and the least-understood
claim. The engine's governing surface is the single indirect call through the `.parser`
function-pointer field — `this.parser(state)` / `p.parser(state)` at `parser.ts:53,82,88,131,137`
and `leaf.ts:44,49,62,140` — which R4 §6c correctly names **THE megamorphic IC of the engine** (many
distinct closure shapes flow through one indirect site). The arity unroll's real temporal value is
NOT loop removal: it is that a **per-site unrolled closure gives each captured
`p0.parser`/`p1.parser`/`p2.parser` call its OWN inline-cache slot**. A hot
`all(string("("), calc, string(")"))` site therefore presents each of its three leaf `.parser` calls
as monomorphic-or-low-polymorphic → inlinable; the shared general arm `parsers[i].parser(state)`
(`leaf.ts:62,261`) is **one** call site seeing every parser shape across every `all()`/`any()` in the
program → megamorphic → never inlined.

Directional confirmation exists at Node v26 via `--trace-turbo-inlining` on a homogeneous scratch
probe (the leaf `parser` SFI is repeatedly inlined into the fused-closure SFI; per-site feedback
vectors are distinct instances — the monomorphic-per-closure mechanism directly observed). Node v26
runs `--maglev`/`--maglev-inlining` **default-on**, and exposes `--trace-maglev-inlining` /
`--trace-turbo-inlining` / `--trace-opt-status` — the directive's per-arity trace is runnable exactly
as specified. **But it has NOT been run on REAL parse-that leaves (distinct SFIs), at the shipping
Maglev tier, with the two legs separated and the HEAD named** — that is the open Brief-1 measurement
(zero measured evidence for two passes).

**Consequence for the family:** the arity unroll is a temporal lever whose benefit is **conditional
on the parse tiering up.** A cold/short one-shot `.parse()` — the common consumer shape — never
reaches Maglev/Turbofan and collects ONLY the structural leg. This is why the certified §0 numbers
are modest (light-op lanes +13–15%, dispatch ~+5%): the paired cold harness captures mostly the
structural leg, not the full inlining win. Any prototype claiming the unroll's value must report tier
residency (`--trace-opt-status`) beside the ratio, or the number is uninterpretable.

---

## 2. Arity distribution — the ACTUAL live corpus (not synthetic)

The consumer of record is **value.js-master** (see §4 — LIVE-published, dep `@mkbabb/parse-that
^1.0.0`; 11 source files import `Parser all any dispatch regex string whitespace`). The prior
convergent census over its grammar surface (value.js `src/parsing` + latex-paper `src/grammar` +
`src/bibtex`; top-level comma count per call) recorded:

| combinator | 0/spread | 1 | 2 | 3 | 4 | 5 | 6 | 7 | ≥10 | total |
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| `all()`   | 9 | 1 | **16** | **24** | 7 | 2 | 2 | – | – | 61 |
| `any()`   | 9 | 9 | **29** | **22** | 10 | 4 | 6 | 2 | 5 (10/12/16/23/48) | 96 |
| `dispatch()` | 5 var-table | – | – | – | – | – | – | – | 1×19-key | 6 |

"0/spread" = `all(...xs)`/`any(...xs)` spread-form calls whose arity is **not statically known** (9
each). latex-paper contributes only **3 `any` sites** (one arity-48 char-class alternation) — a
negligible combinator surface.

**Census-integrity caveat (a real pass-3 obligation, not a footnote).** A fresh direct `git grep`
pinned to `origin/master` (`6abef80`) reproduces the *liveness* (11 importing files, symbols
`all/any/dispatch` confirmed on the import lines) but did **not** reproduce the per-arity split via a
naive `\ball\(` line scan — the histogram was taken over a working-tree/earlier-tranche state, and
the combinator calls on master are formatted such that a token grep undercounts (multi-line /
method-position forms). **The 61/96/6 figure therefore carries as prior-convergent-census-PENDING a
clean re-run pinned to `6abef80` with an AST-level arity extractor (not a line grep).** The
liveness is hard; the exact histogram is a prototype obligation (§9.6). This is the honest state.

**Findings the specialization must answer (robust to the ±count caveat):**
1. `all()` — the arity-2 (16) + arity-3 (24) unrolled arms cover **40 of 52** statically-fixed sites
   (77%). The unrolls are justified; arity-4+ (11 sites) legitimately falls to the general arm. This
   matches the code (`fuseAll` unrolls exactly 2 and 3, `leaf.ts:183-251`).
2. `any()` — **the asymmetry gap.** Only arity-2 is unrolled (`leaf.ts:39-57`); arity-2 (29) is the
   largest bucket so that unroll is well-placed, **but arity-3 (22 sites, 23%) hits the general `for`
   arm** (`leaf.ts:58-73`) while `all` arity-3 IS unrolled. The 2-vs-3 asymmetry between `all` and
   `any` is unjustified by the corpus; the candidate closes it (an `any` arity-3 unroll symmetric with
   `fuseAll`).
3. The big `any()` alternations (arity 16/23/48) and the 19-key `dispatch()` object are **LUT
   candidates, not unroll candidates** — they belong on `dispatch`'s `Int8Array(128)` first-char
   table (`leaf.ts:100-151`, O(1)) above a ~3-way unroll ceiling, not a 48-deep sequential-trial
   megamorphism.
4. The **18 spread-form sites** (`all(...)`/`any(...)`) are **statically un-specializable at the
   combinator layer** — they reach the general arm by necessity. Only a build-time emitter that sees
   the spread's origin could unroll them (see §3).

Verdict: specialization IS justified on this corpus for `all`/`any` at arities 2–3 and for `dispatch`
on ≥10-way alternations — **conditioned on value.js-master remaining live** (§4, H2).

---

## 3. Relationship to the build-time TS emitter (XL-D) — one mechanism? constructible on TS?

They are **two different mechanisms today.**
- The arity-N specializer is a **construction-time closure specializer**: it runs once per
  `new Parser(...)` at grammar-build (`fuseAll` picks the arity-2/3 closure with p0/p1/p2 captured as
  constants, `leaf.ts:179-273`), producing a specialized closure the runtime then invokes **through
  the same megamorphic `.parser` IC**. It is not a code emitter; it emits no source; it operates
  entirely on live parser objects.
- The XL-D build-time TS face (LOCK-4B-TS, settled/measured) is the **offset-tape decoder** seeded
  standalone by the Rust generator (GN-4.W4): a monomorphic tape decoder that **imports no
  combinators**, whose oracle is `JSON.parse`, byte-exact node-for-node across cold children with
  4.4–4.6× stop-early get (SPEC §0). It decodes a serialized tape; it neither constructs nor fuses
  combinators. PT-3 §2 states the two are **complementary, not gating** — neither the other's
  precondition.

So the specializer is NOT the emitter, and there is **no extant build-time combinator emitter on the
TS side.** But the constructible bridge is real and is the family's forward mechanism: a **build-time
combinator fuser** — the LOCK-4B COMPTIME-FUSION direction, TS side ("a build-time fuser emits
specialized chains, no runtime graph at all") — would (a) generalize the arity unroll to **arbitrary**
arity by code-generation rather than a hand-written 2/3 ladder, (b) resolve the **18 spread-form
sites** §2 leaves un-specializable — an emitter that sees the grammar source resolves `all(...xs)` to
its concrete arity, which the runtime specializer cannot — and (c) is the **ONLY route that attacks
the megamorphic `.parser` IC itself**, emitting an AOT-fused function with the leaf calls inlined and
no `.parser` indirection at all. That is the true ceiling the runtime unroll cannot touch (the unroll
only splits the IC per-site; it does not remove the indirection).

**Disposition (center-B, the arbitrary-arity build-time specializer).** Center-B IS that emitter — it
is **homed at LOCK-4B, not built within 8A.** Its Y.13 consumer block is LIFTED by 8D's
KEEP-THE-ALGEBRA verdict (first-party JSON/CSS bench grammars are the corpus of record); the LOCK-4B
*mechanism* block REMAINS (LOCK-4B banked its TS face — "no V8 analog," "isomorphism half 0% built").
Route it to GESTALT-P as the shared arity-emitter-AND-IC-ceiling-fix. A scratch construction attempt
(§9.5) is warranted to *measure* whether AOT emission moves the specialization from
"construction-time closure through the megamorphic IC" to "inlined leaf calls, no indirection" — the
number that decides whether the emitter earns its build-time cost.

---

## 4. CONSUMER LIVENESS (plane-wide, answered early) — HARD-CONFIRMED bifurcation; both hypotheses carried (XL-G)

Direct git evidence (`git rev-parse`, `git show <tree>:package.json`, `git grep -l @mkbabb/parse-that`):

| ref | version | parse-that dep | src import files | last commit | posture |
|---|--:|---|--:|---|---|
| **value.js `origin/master`** (published, origin/HEAD) | **3.1.0** | **`^1.0.0`** | **11** (`Parser all any dispatch regex string whitespace`) | `6abef80` **2026-07-13** | **LIVE-published** |
| value.js `tranche-u` (dev tip, checked-out) | 4.0.0 | **pruned** (empty deps) | 12 import matches STILL in src/ | `86a9a17` **2026-07-15** | **SHED — PAUSED, unmerged, incomplete** |
| latex-paper | 0.2.1, pin `^0.7.1` → installed 0.7.1 | — | 3 `any` sites (`Parser ParserState regex string any`) | idle | **LIVE — sole undisputed external, frozen on 0.7.x** |
| keyframes.js | 6.0.0 | none (dep = value.js) | 0 | — | **DEAD — no path any hypothesis** |

**The load-bearing correction (supersedes the pass-2 "value.js MIGRATED OFF" reading AND research-A's
own earlier "working-tree staged" framing):** the divorce is confined to `tranche-u`, a dev branch
that is **PAUSED and NOT merged to master**; **`master` retains the live `^1.0.0` dependency and the
full all/any/dispatch corpus**, committed two days before the divorce tip. Moreover tranche-u is an
*incomplete* divest — its `package.json` deps are pruned yet **12 `@mkbabb/parse-that` import matches
still survive in its `src/`** — i.e. a genuine WIP mid-migration, not a completed removal. The pass-2
claim "the 59-`all()` live corpus no longer exists" is therefore **struck**: it exists at
value.js-master.

Two hypotheses, both carried (XL-G, non-blocking):
- **H1 (tranche-u lands):** value.js exits the consumer set; arity/`all` specialization loses its
  live external corpus, falling back to first-party bench grammars + latex's thin surface. keyframes'
  transitive link dies with it.
- **H2 (master posture holds):** value.js-master @ 3.1.0 is a live `^1.0.0` consumer; the arity corpus
  is real and native-measurable.

**Material consequence for 8A:** the arity histogram's justification is load-bearing on value.js.
Under H1 the specialization's *justification* re-anchors to the first-party JSON/CSS bench grammars
(`test/benchmarks/pt-b3-fusion.bench.ts`, `json-comprehensive.bench.ts`, R4 §4) + latex's 3 sites;
under H2 it stands against a live external CSS-values grammar. This is a
**blocking-for-the-corpus-NUMBER, non-blocking-for-the-MECHANISM** determination — flag to the owner.
Every 8A gate that reports a corpus number MUST pin `origin/master 6abef80`, never the checked-out
`tranche-u` (measuring tranche-u falsely reports an empty consumer). latex-paper is effectively
frozen on 0.7.x (2 API cuts behind); a 1.1.0 bump is an untested 0.7→1.1 jump (PT-3 W0 owns it).

---

## 5. SPEC fidelity — divergences feeding a re-cut

- **PT-3 §4 W4** claims "the arity-unrolled monomorphizations are preserved by measurement" but (a)
  does not record that `any` arity-3 is **not** unrolled while `all` arity-3 is (the §2 asymmetry gap),
  (b) does not address the 18 spread-form un-specializable sites nor the ≥10-way `any` → `dispatch`
  migration, and (c) presents the unroll as an unconditional structural win when §1 shows it is a
  structural win **plus** a tier-conditional temporal win. Re-cut W4 from "preserve the two unrolls" to
  "close the `any` arity-3 gap + route ≥10-way `any` to `dispatch`; report both legs separately with
  tier residency; corpus-gated on value.js liveness."
- **Dispatch lane number:** SPEC §0 already carries the CONFIRMED_DELTA correction (dispatch ~+5%, not
  the prover's +10.2%); PT-3 §1.3 binds ~+5% no-regression. No divergence — but W4's "monomorphizations
  preserved" gate must state the temporal-leg caveat: the cold paired harness under-reports the unroll.
- **Consumer table:** R4/PT-3 assume value.js 3.1.0 as a static floor; the corrected census (§4) needs a
  re-census row distinguishing master (live, `6abef80`) from tranche-u (paused divorce, `86a9a17`),
  with the H1/H2 fork named. A named supersession candidate.

---

## 6. Integration seams (path:line where extant code serves)

- Arity unroll: `any` arity-2 `leaf.ts:39-57`, general `leaf.ts:58-73`; `fuseAll` arity-2
  `leaf.ts:183-212`, arity-3 `leaf.ts:214-251`, general `leaf.ts:255-272`; `all` entry
  `leaf.ts:154-164` (arity-1 → `parsers[0].parser` bypass at `:161`).
- The megamorphic IC (the ceiling): `.parser(state)` at `parser.ts:53,82,88,131,137`;
  `leaf.ts:44,49,62,140`.
- Shape substrate: `ParserState` fields `state.ts:36-53`, lazy `expected?` `state.ts:43`; type-erased
  write choke `unsafeSetValue`/`unsafeCall`/`unsafeCallRaw` `state.ts:87-99`; `state.ok/err/from` funnel
  `state.ts:55-73`.
- The one prototype-chain deopt risk: `mapState` builds `Object.create(state)` per invocation
  `parser.ts:177-180` — the sole non-flat shape; a shape audit must confirm it is the *only* one and
  that its callers are not deopted. (Note: `mapState` has 0/0/0 external consumers — 8D Tier-B — so the
  `ReadonlyState` front is contingent on a first-party test exercising it, NOT banked ADVANCE.)
- `dispatch` LUT `leaf.ts:100-151` (the ≥10-way `any` migration target; note the PT-Q5 subTable
  RETRACTION `leaf.ts:89-98`); `string` len-1 charCode fast path `leaf.ts:282-294`; `regex` sticky
  zero-alloc `leaf.ts:344-358`; `call()` `flags===0` fast path `parser.ts:438`.
- Closure-alloc discipline: `createLazyCached` closure-local cache to avoid megamorphic IC pollution
  `lazy.ts:19-25`. Combinator closures allocate once per `new Parser(...)`, not per parse.
- First-party bench harnesses (the corpus of record under H1): `test/benchmarks/pt-b3-fusion.bench.ts`
  (fusion/dispatch/json floor), `json-comprehensive.bench.ts`; proof gate `proof:perf` under
  `--expose-gc`.

---

## 7. Prior art with citations

- **V8 tiering (Node v26 / V8 14.6):** Ignition (interpreter) → Sparkplug (baseline) → Maglev (SSA
  mid-tier, `--maglev`/`--maglev-inlining` default-on) → Turbofan (top). Inlining budgets:
  `--maglev-max-small-graph-size` / Turbofan `max-inlined-*`. The per-site-IC ⇒ inlinable thesis is the
  standard V8 hidden-class / inline-cache model ("Maglev — V8's Fastest Optimizing JIT", V8 blog 2024;
  "A tale of two compilers", V8 tiering notes; Clifford et al. on Ignition/feedback vectors).
- **Monomorphism-by-construction** is the rescript/elm-in-JS folk discipline; the "one megamorphic
  dispatch through a `.parser` field" is the classic combinator-library V8 wall (parsimmon, chevrotain,
  arcsecond, parjs all pay it). parse-that's arity unroll is the deliberate per-site escape hatch — and
  its ceiling (the IC indirection survives the unroll) is exactly the wall the build-time emitter (§3,
  LOCK-4B) is measured against, not assumed past.
- **Build-time specialization** parallels rustc/LLVM monomorphization and the Futamura-projection frame:
  the runtime `fuseAll` is a partial evaluator over a static arity; a build-time emitter (§3) is the
  residualized program (the first Futamura projection applied to the recognizer). The measured OpenUI /
  PROVE-14 lesson (pure-TS 2.2–4.6× vs wasm-boundary; full-walk 0.91×) bounds the isomorphism claim:
  the fast TS face is native TS build-time emission, not a wasm core, for the combinator hot path.

---

## 8. Named risks & counterexamples

- **R-A (corpus collapse under H1):** value.js tranche-u lands → the arity histogram no longer reflects
  a live external grammar; specialization is defended only by first-party benches + latex's 3 sites.
  Counterexample to "specialize for the corpus": there may be no external corpus. Cure: gate the
  specialization *number* on H2; keep the *mechanism*; re-anchor its justification to the first-party
  JSON/CSS bench grammars (8D KEEP-THE-ALGEBRA verdict).
- **R-B (temporal-leg over-claim):** reporting the unroll's win as a single ratio conflates structural +
  temporal; a favorable hot window inflates it (the §0/09 self-deception that halved the dispatch claim).
  Cure: report legs separately with tier residency (`--trace-opt-status`).
- **R-C (megamorphic ceiling per-leaf):** a deeply shared leaf (e.g. `whitespace`, reused at hundreds of
  sites — `leaf.ts:397`) keeps its `.parser` call site polymorphic even inside an unrolled closure, so
  the unroll buys the structural leg but not the temporal one there. The unroll is not a universal win.
- **R-D (`mapState` deopt):** `Object.create(state)` (`parser.ts:177-180`) is a prototype-chain object;
  if its access pattern deopts callers it violates the shape-stable claim. Probe, do not assume benign.
- **R-E (census artifact):** the 61/96/6 histogram undercounts under a naive line grep on master (§2
  caveat); a specialization gate built on the wrong count mis-sizes the unroll ladder. Cure: the arity
  extractor is AST-level, pinned to `6abef80`, before any gate reads a bucket size.

---

## 9. Exactly what a prototype must test to move family 8A

1. **Per-arity two-leg trace (HEAD-named, SHIPPING tier — Brief 1, the zero-evidence-for-2-passes
   claim).** On REAL parse-that leaves (distinct SFIs, not a homogeneous probe), run
   `--trace-maglev-inlining` AND `--trace-turbo-inlining` for arity 2, 3, and the general arm at Node
   v26 **default** tier; report the STRUCTURAL leg (alloc-B/op + `%HaveSameMap` shape stability,
   tier-independent) and the TEMPORAL leg (leaf `parser` SFI inlined-or-not per site, with
   `--trace-opt-status` tier residency) **separately**. Base-pin the worktree; name the HEAD.
2. **`any` arity-3 unroll (the gap).** Add an `any` arity-3 unrolled arm symmetric with `fuseAll`;
   paired-cold measure vs the general arm at arity-3 (22 live sites under H2). Pass = beats the general
   arm at arity-3 with no arity-2/≥4 regression.
3. **Big-`any` → `dispatch` migration.** For the arity-16/23/48 alternations, measure the LUT
   (`dispatch`) vs sequential-trial `any`; confirm the ≥10-way crossover empirically.
4. **Shape oracle.** `node --allow-natives-syntax` + `%HaveSameMap` on a diagnostics-off vs
   diagnostics-populated `ParserState` — assert TRUE (fixed-from-birth holds); probe `mapState`'s
   `Object.create` view as the only prototype-chain object and check its callers are not deopted.
5. **Build-time emitter scratch (§3 / center-B, routed to LOCK-4B/GESTALT-P).** Emit, from a static
   `all(a,b,c)` expression, an AOT-fused function with the leaf calls inlined (no `.parser`
   indirection); measure whether it beats the construction-time `fuseAll` closure — the only route that
   attacks the megamorphic IC, and the number that decides whether the emitter earns its build cost.
6. **Consumer re-census (AST-level, pinned).** Re-run the arity extractor over `value.js@6abef80`
   (`origin/master`) with an AST parser, not a line grep, to bank the true 2/3/≥4/spread buckets; and
   re-run after the owner confirms H1/H2. If H1, re-anchor every specialization gate to the first-party
   bench corpus.
