# LOCK-8 / family 8A — THE V8 RUNTIME PLANE (SHAPE-STABLE CORE) — pass-3 research

The V8-first formulation of the parse-that TypeScript combinator core: every `ParserState`
and combinator closure shape-stable and monomorphic **by construction**, an allocation-zero
default (LL(1), packrat-unarmed) hot path, diagnostics and packrat as shape-preserving optional
planes, proven by IC-state probes and paired cold harnesses. The engine substrate is
`~/Programming/parse-that/typescript/src/parse/` (READ-ONLY; any landing rides letter
BBNF-PT-G, prototypes on scratch copies only). All measurements below name Node **v26.0.0** on
Apple Silicon; the parse-that TS package is frozen at `1.0.0` (upstream HEAD `ef10d5b`).

---

## 1. The mechanism made concrete — two legs, reported separately

The shape-stable core has **two physically distinct wins** that the directive requires be kept
apart; conflating them is the family's principal reporting hazard.

**STRUCTURAL leg (allocation + hidden-class, JIT-tier-independent).** Present in interpreted,
maglev, and turbofan code alike. Its substrate:
- `ParserState` is fixed-from-birth: `expected?`, and (post-lever) `suggestions?` /
  `secondarySpans?`, are declared native class fields initialized to `undefined`, so the hidden
  class is stamped at construction and never transitions when a diagnostic later seeds an array
  (`state.ts:36-53`, `state.ts:43`). The lever (certified §0 row 11) removes **6 eager `= []`
  alloc sites → 0** in the emitted `dist/` (ctor ×2, `mergeErrorState` furthest-advance ×2,
  `resetErrorState` ×2), converting them to `void 0` stores; the `??= []` populate sites live
  inside the `if (diagnosticsEnabled)` guard the default path never enters. Corroborated by
  fused alloc **−60.1% B/op** (exact, `--expose-gc`).
- The arity-unrolled `all`/`fuseAll` closures build **exactly one flat result array** grown by
  an index cursor with `out.length = w` trim (`leaf.ts:191,209,222,248,257,270`), never the
  N−1 nested 2-tuples the unfused `a.then(b).then(c)` allocates, and never a `for…of` iterator
  object (classic indexed `for`, `leaf.ts:58-73,255-272`).

**TEMPORAL leg (JIT inlining, tier-dependent).** This is the *V8-discipline* core and the least
understood claim. The engine's governing surface is the single indirect call through the
`.parser` function-pointer field — `this.parser(state)` / `p.parser(state)` at e.g.
`parser.ts:53,82,88,131,137` and `leaf.ts:44,49,62,140` — which R4 §6c correctly names **the
megamorphic IC of the engine** (many distinct closure shapes flow through one indirect site).
The arity unroll's real temporal value is NOT loop removal — it is that a **per-site unrolled
closure gives each captured `p0.parser` / `p1.parser` / `p2.parser` call its OWN inline-cache
slot**. A hot `all(string("("), calc, string(")"))` site therefore presents each of its three
leaf `.parser` calls as monomorphic-or-low-polymorphic, which the optimizing tier can inline; the
shared general arm `parsers[i].parser(state)` (`leaf.ts:62,261`) is **one** call site that sees
every parser shape across every `all()`/`any()` in the program → megamorphic → never inlined.

CONFIRMED at Node v26 **default tier** via `--trace-turbo-inlining` on a scratch probe
(`scratchpad/probe.mjs`): the leaf `parser` SharedFunctionInfo is repeatedly `Inlining …
<SharedFunctionInfo parser> into … <fused-closure SFI>` at named `JSCall` call sites, and the
per-site feedback vectors differ per closure (distinct `FeedbackVector[9]` instances), which is
the monomorphic-per-closure mechanism directly observed. Node v26 has `--maglev` and
`--maglev-inlining` **default-on**, plus `--trace-maglev-inlining` and `--trace-turbo-inlining`
available — the directive's per-arity trace is runnable exactly as specified.

The consequence for the family: **the arity unroll is a temporal lever whose benefit is
conditional on the parse being hot enough to tier up.** A cold/short one-shot `.parse()` (the
common consumer shape) never reaches maglev/turbofan, so it collects ONLY the structural leg.
This is why the two legs must be reported separately and why the certified §0 numbers are
modest (light-op lanes +13–15%, dispatch only ~+5%): the paired cold harness captures mostly the
structural leg, not the full inlining win. A prototype claiming the arity unroll's value must
report tier residency (`--trace-opt-status`) alongside the ratio, or the number is uninterpretable.

---

## 2. Arity distribution — the ACTUAL live corpus (not synthetic)

Balanced-paren census over the committed value.js `src/parsing/` (9 grammar files, the still-live
`3.1.0`/tranche-u state) + latex-paper `src/grammar` + `src/bibtex`
(`scratchpad/arity.mjs`, top-level comma count per call):

| combinator | 0/spread | 1 | 2 | 3 | 4 | 5 | 6 | 7 | ≥10 | total |
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| `all()`   | 9 | 1 | **16** | **24** | 7 | 2 | 2 | – | – | 61 |
| `any()`   | 9 | 9 | **29** | **22** | 10 | 4 | 6 | 2 | 5 (10/12/16/23/48) | 96 |
| `dispatch()` | 5 var-table | – | – | – | – | – | – | – | 1×19-key | 6 |

"0/spread" = `all(...xs)` / `any(...xs)` spread-form calls whose arity is **not statically known**
(9 each). latex-paper contributes only **3 `any` sites** total (one arity-48 char-class
alternation) — a negligible combinator surface.

**Findings the specialization must answer:**
1. `all()` — the arity-2 (16) + arity-3 (24) unrolled arms cover **40 of 52** statically-fixed
   sites (77%). The unrolls are justified; arity-4+ (11 sites) legitimately falls to the general
   arm. This matches the code (`fuseAll` unrolls exactly 2 and 3, `leaf.ts:183-251`).
2. `any()` — **asymmetry gap.** Only arity-2 is unrolled (`leaf.ts:39-57`); arity-2 (29) is the
   single largest bucket so that unroll is well-placed, **but arity-3 (22 sites, 23%) hits the
   general `for` arm** (`leaf.ts:58-73`) while `all` arity-3 is unrolled. The 2-vs-3 asymmetry
   between `all` and `any` is unjustified by the corpus; a candidate closes it (an `any` arity-3
   unroll, symmetric with `fuseAll`).
3. The big `any()` alternations (arity 16/23/48) and the value.js 19-key `dispatch()` object are
   **LUT candidates, not unroll candidates** — they belong on `dispatch()`'s `Int8Array(128)`
   first-char table (`leaf.ts:100-151`), which is O(1) vs a 48-deep sequential-trial megamorphism.
   The unroll ceiling is ~3; beyond it, first-char dispatch is the correct mechanism.
4. The **18 spread-form sites** (`all(...)`/`any(...)`) are **statically un-specializable at the
   combinator layer** — they reach the general arm by necessity. Only a build-time emitter that
   sees the spread's origin could unroll them (see §3).

Justification verdict: specialization IS justified on this corpus for `all`/`any` at arities 2–3
and for `dispatch` on ≥10-way alternations — **conditioned on the corpus remaining live** (§4).

---

## 3. Relationship to the build-time TS emitter (XL-D) — one mechanism? constructible on TS?

They are **two different mechanisms today.**
- The arity-N specializer is a **construction-time closure specializer**: it runs once per
  `new Parser(...)` at grammar-build (`fuseAll` picks the arity-2/3 closure with p0/p1/p2 captured
  as constants, `leaf.ts:179-273`), producing a specialized closure the runtime then invokes. It
  is not a code emitter; it emits no source; it operates entirely on live parser objects.
- The XL-D build-time TS face is the **offset-tape decoder** seeded standalone by the Rust
  generator (GN-4.W4): a monomorphic tape decoder that **imports no combinators**, whose oracle is
  `JSON.parse`, byte-exact node-for-node across cold children with 4.4–4.6× stop-early get (§0 row
  14). It decodes a serialized tape; it does not construct or fuse combinators. PT-3 §2 states the
  two are **complementary, not gating** — neither the other's precondition.

So the specializer is NOT the emitter, and there is no extant build-time combinator emitter on the
TS side. **But the constructible bridge is real and is the family's forward mechanism:** a
build-time *combinator fuser* (the LOCK-4B COMPTIME-FUSION direction, TS side — "a build-time fuser
emits specialized chains, no runtime graph at all") would (a) generalize the arity unroll to
**arbitrary** arity by code-generation rather than a hand-written 2/3 ladder, and (b) resolve the
**18 spread-form sites** §2 leaves un-specializable — because an emitter that sees the grammar
source resolves `all(...xs)` to its concrete arity, which the runtime specializer cannot. A scratch
construction attempt is warranted: emit, from a static combinator expression, the same closure the
runtime `fuseAll` builds, and measure whether ahead-of-time emission moves the specialization from
"construction-time, still a runtime closure invoked through the megamorphic IC" to "inlined leaf
calls with no `.parser` indirection at all." That is the only route that attacks the megamorphic
`.parser` IC itself — the true ceiling the runtime unroll cannot touch.

---

## 4. CONSUMER LIVENESS (plane-wide, answered early) — carry both hypotheses (XL-G)

**value.js is MID-MIGRATION OFF parse-that.** Reconciled state (`git status`, `git show`):
- Committed HEAD (branch `tranche-u`, and `master`) = version **3.1.0**, dep
  `@mkbabb/parse-that ^1.0.0` (installed 1.0.0), `src/parsing/` intact — the 61 `all` + 96 `any`
  + 6 `dispatch` corpus of §2. This is the live floor R4 §7 recorded.
- **Working tree (staged)** = version **4.0.0**, ALL dependencies **pruned** (empty `dependencies`
  block), and `src/parsing/*` **staged for deletion** (`D src/parsing/index.ts`,
  `D src/parsing/color/color.ts`, `D src/parsing/math.ts`, … — the whole combinator layer). No
  `@mkbabb/parse-that` import survives anywhere in the working-tree `src/`.

This is the P2 question answered by evidence: value.js 4.0.0's posture is **a real in-progress
removal of parse-that**, not a stale install — but it is **uncommitted**, so per XL-G both
hypotheses are carried until the owner confirms:
- **H-A (drop):** 4.0.0 excises the parse-that combinator surface entirely (staged deletions +
  empty deps are the evidence). If true, value.js — the *sole* corpus justifying arity
  specialization — **exits as a consumer**, collapsing the live specialization corpus to
  latex-paper's 3 `any` sites plus first-party benches. keyframes.js's transitive link (via
  value.js, no direct dep) dies with it.
- **H-B (WIP):** a mid-migration branch state that reverts before release; value.js remains the
  primary consumer at 3.1.0's surface.

**Material consequence for 8A:** the arity histogram's justification for specialization is
load-bearing on value.js. If H-A holds, the family's consumer-set for the V8 specialization
narrows to the **first-party test/bench suite** (the 8D consumer-set-adjudication question) and
latex-paper's thin surface — the specialization is still defensible against the first-party JSON
grammar bench (`test/benchmarks/json-comprehensive.bench.ts`, `pt-b3-fusion.bench.ts`,
R4 §4) but no longer against a live external CSS-values grammar. This must be flagged to the owner
as a **blocking-for-the-corpus-claim, non-blocking-for-the-mechanism** determination.

**latex-paper** = version 0.2.1, pins `^0.7.1` → resolves `<0.8.0`, installed **0.7.1**; imports
only `Parser ParserState regex string any` (3 `any` sites). It is **effectively frozen on 0.7.x**
and predates the 1.0.0 `*Span`/`chain()` breaking cut — a 1.1.0 bump is an untested 0.7→1.1 jump
(the PT-3 W0 handshake owns it). It exercises almost none of the arity surface; it is not a corpus
that justifies specialization.

---

## 5. SPEC fidelity — divergences feeding a re-cut

- **PT-3 §4 W4** claims "the arity-unrolled monomorphizations are preserved by measurement" but
  (a) does not record that `any` arity-3 is **not** unrolled while `all` arity-3 is (the §2
  asymmetry), and (b) does not address the 18 spread-form un-specializable sites nor the big-`any`
  → `dispatch` migration. The histogram is new evidence that should re-cut W4 from "preserve the
  two unrolls" to "close the `any` arity-3 gap + route ≥10-way `any` to `dispatch`, corpus-gated
  on value.js liveness."
- **Dispatch lane number:** §0 row 11 already carries the CONFIRMED_DELTA correction (dispatch
  ~+5%, not the prover's +10.2%); PT-3 §1.3 binds ~+5% no-regression. No divergence — but W4's
  "monomorphizations preserved" gate must state the temporal-leg caveat of §1: the unroll's win is
  tier-conditional and the cold paired harness under-reports it. The spec text presents the unroll
  as an unconditional structural win; §1 shows it is a structural win **plus** a tier-conditional
  temporal win. A precise re-cut separates them.
- **value.js 4.0.0** is not anticipated anywhere in PT-3 (which assumes value.js 3.1.0 as a live
  consumer floor). This is a named supersession candidate: the R4/PT-3 consumer table needs a
  re-census row (§4).

---

## 6. Integration seams (path:line where extant code serves)

- Arity unroll: `any` arity-2 `leaf.ts:39-57`, general `leaf.ts:58-73`; `fuseAll` arity-2
  `leaf.ts:183-212`, arity-3 `leaf.ts:214-251`, general `leaf.ts:255-272`; `all` entry
  `leaf.ts:154-164` (arity-1 → `parsers[0].parser` bypass at :161).
- The megamorphic IC (the ceiling): `.parser(state)` at `parser.ts:53,82,88,131,137`;
  `leaf.ts:44,49,62,140`.
- Shape substrate: `ParserState` fields `state.ts:36-53`, lazy `expected?` `state.ts:43`;
  type-erased write choke `unsafeSetValue`/`unsafeCall` `state.ts:87-99`.
- The one prototype-chain deopt risk: `mapState` builds `Object.create(state)` per invocation
  `parser.ts:177-180` — the sole non-flat shape; a shape audit must confirm it is the *only* one
  and that its callers are not deopted.
- `dispatch` LUT `leaf.ts:100-151` (the ≥10-way `any` migration target); `string` len-1 charCode
  fast path `leaf.ts:282-294`; `regex` sticky zero-alloc `leaf.ts:344-358`; `call()` `flags===0`
  fast path `parser.ts:438`.
- Closure-alloc discipline: `createLazyCached` closure-local cache to avoid megamorphic IC
  pollution `lazy.ts:19-25`.
- Bench harnesses (the first-party consumer if H-A): `test/benchmarks/pt-b3-fusion.bench.ts`
  (fusion/dispatch/json floor), `json-comprehensive.bench.ts`; proof gate `proof:perf` under
  `--expose-gc`.

---

## 7. Prior art with citations

- **V8 tiering (Node v26):** Ignition (interpreter) → Sparkplug (baseline) → Maglev (SSA
  mid-tier, `--maglev`/`--maglev-inlining` default-on) → Turbofan/Turbolev (top). Inlining
  budgets: `--maglev-max-small-graph-size` (10), Turbofan `max-inlined-*`. The per-site-IC ⇒
  inlinable thesis is the standard V8 hidden-class / inline-cache model (Ceccarelli/V8 team, "Maglev
  — V8's Fastest Optimizing JIT", V8 blog 2024; "A tale of two compilers" tiering notes).
- **Monomorphism-by-construction** is the mechanize.js / rescript / ELM-in-JS folk discipline;
  the "one megamorphic dispatch through a `.parser` field" is the classic combinator-library V8
  wall (parsimmon, chevrotain postmortems). arcsecond/parjs pay it; parse-that's arity unroll is
  the deliberate escape hatch.
- **Build-time specialization** parallels rustc/LLVM monomorphization and the "staged
  interpreter → compiler" (Futamura projection) frame: the runtime `fuseAll` is a partial
  evaluator over a static arity; a build-time emitter (§3) is the residualized program.

## 8. Named risks & counterexamples

- **R-A (corpus collapse):** value.js H-A drops parse-that → the arity histogram no longer
  reflects a live external grammar; specialization is defended only by first-party benches +
  latex-paper's 3 sites. Counterexample to "specialize for the corpus": there may be no external
  corpus. Cure: gate the specialization claim on the 8D consumer-set adjudication; keep the
  mechanism, re-anchor its justification to the first-party JSON/CSS bench grammars.
- **R-B (temporal-leg over-claim):** reporting the arity unroll's win as a single ratio conflates
  structural + temporal; a favorable hot window inflates it (the §0/09 self-deception, the exact
  trap that halved the dispatch claim). Cure: report legs separately with tier residency.
- **R-C (megamorphic ceiling):** a deeply shared leaf (e.g. `whitespace`, reused at hundreds of
  sites) keeps its `.parser` call site polymorphic even inside an unrolled closure, so the unroll
  buys the structural leg but not the temporal one there. The unroll is not a universal win.
- **R-D (`mapState` deopt):** `Object.create(state)` (`parser.ts:177-180`) is a prototype-chain
  object; if its access pattern deopts callers it violates the shape-stable claim. Must be probed,
  not assumed benign.

## 9. Exactly what a prototype must test to move family 8A

1. **Per-arity two-leg trace (HEAD-named).** On REAL parse-that leaves (distinct SFIs, not a
   homogeneous probe), run `--trace-maglev-inlining` AND `--trace-turbo-inlining` for arity 2, 3,
   and the general arm; report the STRUCTURAL leg (alloc-B/op + `%HaveSameMap` shape stability,
   tier-independent) and the TEMPORAL leg (leaf `parser` SFI inlined-or-not per site, with
   `--trace-opt-status` tier residency) **separately**. Base-pin the worktree; name the HEAD.
2. **`any` arity-3 unroll (the gap).** Add an `any` arity-3 unrolled arm symmetric with `fuseAll`;
   paired-cold measure vs the general arm at arity-3 (22 live sites). Pass = beats general arm at
   arity-3 with no arity-2/≥4 regression.
3. **Big-`any` → `dispatch` migration.** For the arity-16/23/48 alternations, measure the LUT
   (`dispatch`) vs sequential-trial `any`; confirm the ≥10-way crossover.
4. **Shape oracle.** `node --allow-natives-syntax` + `%HaveSameMap` on a diagnostics-off vs
   diagnostics-populated `ParserState` — assert TRUE (fixed-from-birth holds); probe `mapState`'s
   `Object.create` view for the only prototype-chain object.
5. **Build-time emitter scratch (§3).** Emit, from a static `all(a,b,c)` expression, an
   AOT-fused function with the leaf calls inlined (no `.parser` indirection); measure whether it
   beats the construction-time `fuseAll` closure — the only route that attacks the megamorphic IC.
6. **Consumer re-census.** Re-run the per-tree import census after owner confirms value.js H-A/H-B;
   if H-A, re-anchor every specialization gate to the first-party bench corpus.
