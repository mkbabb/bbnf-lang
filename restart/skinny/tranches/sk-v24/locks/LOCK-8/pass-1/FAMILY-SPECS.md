# LOCK-8 — THE TS BACKBONE PLANE — pass-1 FAMILY SPECIFICATIONS

*Synthesis, pass 1. One cogent spec per family while routes remain incompatible, then the
incompatibility map and the BLOCKED-candidate adjudication. Source: research-{A,B,C,D,priorart}.
Binding facts: parse-that/typescript is READ-ONLY to the bbnf campaign — every family lands only
via letter BBNF-PT-G (owner or a parse-that-side session); prototypes run on scratch copies
(the PROVE-11 recipe). Two engines govern this lock and must never be conflated: the V8 JIT
(runtime plane) and the TS checker (type plane) — what wins in one need not transfer to the
other (priorart §0; SPEC §0/09).*

The four families are genuinely orthogonal **planes over one closure core**, not four cores:
8A is the runtime (V8) plane, 8B the type plane, 8C the cross-language correspondence plane,
8D the API/consumer plane. Three of the four coexist by construction; the incompatibilities
are narrow, named, and mostly reduce to two already-measured walls — the SpanParser runtime-
switch KILL (C1) and the diagnostics-surface contract (8B result-union vs 8D field-triple).

---

## FAMILY 8A — THE SHAPE-STABLE CORE (the V8 runtime plane)

**Thesis.** Shape stability and monomorphism are **structural invariants provable with V8
natives** (`%HaveSameMap` / `%GetOptimizationStatus` / `--trace-ic` / `--trace-turbo-inlining`),
not adjectives. 8A elevates the backbone from "measured faster" (PROVE-11, timing-only) to
"measured monomorphic and deopt-free." PROVE-11's lazy-field patch is the smallest instance;
the family is a **probe harness + a shape contract**.

**Architecture — four centers in dependency order.**
- **A · the shape contract.** `ParserState` (state.ts:36-53) and `Parser` (parser.ts:24-32)
  hold one hidden class each by construction under ES2022 `useDefineForClassFields`. Rule:
  no `ParserState` field carries an eager heap initializer; every optional payload is
  `undefined`-at-birth, lazily `(x ??= []).push(...)` under a gate — so the diagnostics-off and
  diagnostics-populated map are identical. Named hazard: a numeric leaf writing a raw Smi into
  `value` splits the state map (R-F).
- **B · the central megamorphic CALL IC — the deep problem, precisely separated.** The engine's
  `this.parser(state)` is **not** a megamorphic property LOAD (all `Parser`s share one map → the
  load is monomorphic, no 10–50× hashtable cliff). It is a megamorphic **call** whose only price
  is **lost inlining of the child parser**. The lawful de-megamorphization is **constant-capture**:
  arity-unrolled arms (`any` leaf.ts:39-57; `fuseAll` leaf.ts:183-251) capture each child as a
  closure constant → TurboFan sees a known callee and can inline it; the general arm's
  `parsers[i].parser(state)` loads a varying closure → no inline. 8A's move: make constant-capture
  a **construction-time / build-time specializer** (the V8 face of LOCK-4B), not three hand arities.
- **C · the allocation-zero steady path and its honest boundary.** Zero-alloc up to the
  deliverable; the deliverable is one flat array per `all`-node (never N−1 nested `then` tuples).
  Going below the deliverable requires the tape contract (out of family — LOCK-6 / GN-4.W4).
- **D · the IC-state probe substrate (the novum).** `test/v8-shapes.runner.mjs` under
  `--allow-natives-syntax` asserting `%HaveSameMap`, `%GetOptimizationStatus`, `%DebugPrint`
  elementsKind, `--trace-ic`/`--trace-deopt`/`--trace-turbo-inlining`. Paired with the certified
  PROVE-11 temporal harness (single build, A,B,A,B interleave, median-of-13 × ≥300k, N≥12, two
  windows, packrat armed/unarmed disclosed). Structural + temporal are the **two legs**; neither
  alone suffices.

**The one genuinely new source change.** `mapState`'s `Object.create(state)` (parser.ts:177)
mints a per-invocation prototype-chain view with a **distinct map** — the single worst V8 surface;
replace it with a fixed-shape monomorphic snapshot record `{src,value,offset,isError,furthest}`
(every field any consumer reads — R-D) built by one constructor so all views share one map.

**Integration seams.** state.ts:44-45 (eager arrays → lazy); utils.ts `mergeErrorState`
(re-alloc on furthest-advance); parser.ts:177 (`Object.create` → snapshot); leaf.ts:39-57,183-251
(specializer generalization target); leaf.ts:134-145 (`dispatch` — irreducible residue).

**Gates it carries (born-RED).** (1) `%HaveSameMap` diagnostics-off ≡ diagnostics-on state,
constructed *after a full populating parse* not just after `new` (defeats R-B false-green).
(2) `%HaveSameMap(view_i, view_j)` TRUE across `mapState` invocations after the snapshot fix,
and `Object.create` view ≠ real state on 1.0.0 (the deopt, falsifiable). (3) `--trace-deopt`
**zero** steady-path deopts over the value.js corpus. (4) `--trace-turbo-inlining`: unrolled
arm's `p0.parser` **inlines**, general arm's `parsers[i].parser` does **not**. (5) dispatch lane
**~+5% no-regression, never a ≥10% gate** (SPEC §0 honest-number law; the PROVE-11 VERIFY
correction is binding).

**Exact open questions.** (i) **Does constant-capture inlining actually fire?** If TurboFan
declines to inline even the captured-constant callee, the entire closure-core-made-monomorphic
thesis is refuted and 8A must concede to a VM core — which C1 has **already measured as a −14%
loss on V8**. This is the family's pivot (Brief 1). (ii) Can input-dependent `dispatch`
(leaf.ts:134-145) be de-megamorphized at all within a closure core, or is the accepted residue
(cost = lost inlining of a leaf) the terminal answer? The null hypothesis — leave the closures,
the call is cheap because the load is monomorphic — must be defeated by numbers or it wins.

**Disposition: ADVANCE.** Primitives all exist; the pivotal uncertainty (i) is one runnable
measurement. The VM-core fallback is a *refuted* route (C1), so a failure of (i) does not open a
lawful alternative — it collapses 8A to "accept the megamorphic-call residue," a bounded
concession, not a BLOCK.

---

## FAMILY 8B — TYPE-LEVEL TOTALITY (the type plane)

**Thesis.** The TS type system carries the grammar — full `ExtractValue` inference, a typed
combinator algebra with **no `Parser<any>` anywhere**, diagnostics typed by position — while the
emitted JS (and every V8 hot shape) stays **byte-for-byte unchanged**. Zero-runtime-cost
(constitution v) is proven by an **empty `dist/` semantic diff**, not asserted. This is
parse-that's OWN inference story, distinct from LOCK-1's Rust HM/CSP solver.

**Root finding.** `ParserFunction<T> = (val: ParserState<any>) => ParserState<any>`
(parser.ts:14-16) is **phantom in T** — it discards its type parameter, so `new Parser(fn as
ParserFunction<[T,S]>)` cannot propagate `[T,S]` and every combinator falls to the `string`
default. The `as ParserFunction<...>` casts are cosmetic; only trailing force-casts
(`... as Parser<Result>`, leaf.ts:163) rescue `all()`. This severs **outputs** systemically —
deeper than PT-E2's enumerated `Parser<any>` **input** leaks.

**Mechanism.** Keep `ParserFunction<T>` phantom-and-erased at the implementation boundary — this
is the **one legitimate bounded `any`** (documented, not counted a leak; it is exactly the
invariant call shape 8A's megamorphic-call analysis requires) — but give **every public
combinator an explicit precise return annotation** so inference flows through the *signatures*
while the *runtime pointer* stays monomorphic. Type and codegen decouple: an erasable parallel
structure over inert closures.
- **One type-utility home** `src/parse/types.ts` (new, DRY — kills the duplicated `ExtractValue`
  at parser.ts:9-11 and leaf.ts:155-157): `ValueOf`, `Values<Ps>` (variadic mapped tuple, TS 4.0),
  `Union`, `DropUndefined` (tail-recursive accumulator).
- **Precise signatures**: `all(...Ps): Parser<DropUndefined<Values<Ps>>>`; `any(...Ps):
  Parser<Union<Ps>>`; `then<S>: Parser<[T,S]>` (drop the unsound `S|T` widening);
  `or<S>: Parser<T|S>`; `chain<S>: Parser<S>`; `opt(): Parser<T|undefined>`; etc.
- **Literal precision** via TS 5.0 `const` params: `string<const S>(s: S): Parser<S>` so
  `string("+") → Parser<"+">` — the grammar's alphabet enters the types — with a non-`const`
  default path for `string(dynamicVar)` (R4).

**Diagnostics typed by position.** ADOPT the **result-union**: `ParseResult<T> = {ok:true;
value:T; offset} | {ok:false; diagnostics: readonly Diagnostic[]}` — failure is *in the type*,
narrowed on `.ok`. BANK `Parser<T,E>` expected-label typing as a **named negative-result
candidate** (exponential type dimensions, thin consumer value, prime TS2589 offender) unless a
consumer demands it (Y.13).

**Integration seams.** New `src/parse/types.ts` + `test/types/inference.test-d.ts`; edits at
parser.ts:9-11,14-16,81,105,124,234; leaf.ts:28,100,154,179,276; state.ts:79. Runtime files inert
to types; arity-unrolled arms (leaf.ts:39-57,183-251) gain positional types, runtime shape
UNTOUCHED (W4-guarded).

**Gates it carries (born-RED).** (1) **Instantiation-budget** (the signature falsifier):
`tsc --generateTrace`/`--extendedDiagnostics` instantiation count ≤ named ceiling (≤1.5× baseline)
and check-time ≤ named ms on real value.js + latex-paper grammars — **type-level totality that
blows the checker is a failed 8B**. (2) `dist/` semantic diff EMPTY (zero runtime cost proven).
(3) `.test-d.ts` RED on 1.0.0, GREEN after, with `@ts-expect-error` on genuinely wrong shapes
still erroring (precision proven both ways). (4) No compiling consumer becomes uncompilable
(no new TS2589).

**Exact open questions.** (i) **R1 — the `opt()`-inside-`all()` parity hole (the hardest
correctness problem).** `all` drops `undefined` at runtime, so `all(a, opt(b), c)` returns `[A,C]`
or `[A,B,C]` — a length-variable runtime tuple the static type cannot know. `DropUndefined`
mistypes the b-present case; the plain tuple lies. Three routes (type-level filter + document the
one lawful widening / redefine `all` to keep undefined [breaking] / a distinct `allOptional`),
decided by a **runtime differential** against value.js's real `opt`-in-`all` sites. (ii) Does the
tightened plane compile the real grammars within budget (Brief 2)? (iii) Result-union vs the
existing `parseState` field-triple contract — see the incompatibility map (this is where 8B
collides with 8D).

**Disposition: ADVANCE.** All primitives exist (variadic tuples, `const` params, tail-recursion
elimination — hard cap ~1000 tail-recursive instantiations). The budget gate is a live make-or-
break but runnable now; the `Parser<T,E>` frontier is pre-BANKED, not blocking.

---

## FAMILY 8C — THE ISOMORPHIC MIRROR (the correspondence plane)

**Thesis.** The TS backbone is one face of a two-language artifact whose correspondence is
**machine-checked, not asserted** — a *stratified* mirror where each stratum carries the tightest
mechanism it admits. The governing empirical fact is banked: **SPEC §0 row 14 (CONFIRMED)** — the
offset-tape SoA decodes byte-exact node-for-node in TS, stop-early 4.4–4.6× over `JSON.parse`,
full-walk ~0.91× (native won, measured). The mirror is **strongest at the data plane** and must
NOT over-reach into a wasm-core-everything design the same measurement refutes.

**Architecture — three strata + a linter.**
- **Stratum A · DATA (byte-exact, single-spec-generated both sides).** The tape SoA layout
  (offsets.u32, flag_cursors, flag_values, source.bin, **empty payload arena** — the offset-only
  invariant) becomes one machine-readable schema; one generator emits both the Rust dumper/builder
  and the TS `TapeDoc` decoder. `tape.mjs`/`load.mjs` cease to be hand-written (clean-regen,
  regen∘regen byte-identical). Gate: the generated decoder passes PROVE-14's `verify.mjs`
  fingerprint (32-bit + 6 structural counts) **unchanged**. This is where isomorphism is
  *provable byte-for-byte*.
- **Stratum B · SHAPE (shared node vocabulary, generated dispatch *contract*, hand bodies).**
  From `span_parser/leaves.rs` (`SpanKind`) generate the Rust enum+router AND a TS dispatch
  *surface* — but **NOT a naive discriminated union**: that re-introduces the megamorphic switch
  (C1/R4 §6c). Generate **vocabulary + name-map only**; the `Int8Array(128)` LUT (leaf.ts:100-151)
  and arity-unrolled arms stay hand-tuned. The LOCK-4A/4C tie realized *conservatively*.
- **Stratum C · ORACLE (behavioral differential over a shared corpus).** JSON + value.js CSS +
  latex math parsed by both cores; the emitted Stratum-A tape must be byte-identical across cold
  children (the fingerprint harness). A **CONFIRMED win**, not a null hypothesis — it drift-kills
  the regex-engine and state divergences without requiring their layout to mirror.
- **The isomorphism LINTER.** A checked-in name-map (Rust ↔ TS, with `EXEMPT:` rows carrying a
  reason — `regex → oracle-only`, `span_parser/ → rust-ahead`, `ParserState → transient`). CI
  verifies tree correspondence, name-map resolution both sides, and fresh-regen of every generated
  artifact. Divergence is a named exemption or a lint failure, never silent.

**The three structural divergences it must classify (not "fix").** (1) Rust carries two parser
reps (closure-boxed `Parser` + vtable-free `SpanParser`); TS has only the closure core — the
SpanParser has **no TS twin, and a naive twin deopts** (R-B). (2) Regex: Rust bespoke DFA
(leaf.rs:85) vs TS native `RegExp` (leaf.ts:317-369) — **oracle-only, layout-exempt** (LOCK-5
owns the verdict; R-C: "porting the DFA to TS" is a likely regression, forbidden). (3)
Diagnostics: Rust compile-time `#[cfg]` erase vs TS runtime scope — **behavioral mirror, not
mechanism mirror** (R-D: forcing TS to compile-time-erase breaks the live surface keyframes
consumes).

**Integration seams.** `crates/bbnf-bench/src/bin/tape_dump.rs`, `crates/bbnf-bench/ts-tape/{tape,
load,pointer,verify,bench}.mjs` (the crossing lives bbnf-side, where the campaign may write);
wasm crossing = GN-4.W4; `span_parser/leaves.rs` → Stratum B spec.

**Gates it carries.** (1) One spec → two artifacts, byte-exact, regen-stable (Stratum A).
(2) Differential-corpus CI green across cold children (Stratum C — the born-RED drift-killer).
(3) Generated node-vocabulary dispatch holds the arity-unroll floor **or is refuted** (Stratum B).
(4) Linter: zero false positives, all three §1.3 divergences surfaced as named exemptions.
(5) wasm-bindgen export holds stop-early 4.4× **across the wasm boundary** (not just file
round-trip) — GN-4.W4.

**Exact open questions.** (i) **Does the graph mirror to TS at all, or does the closure core
stand?** — the LOCK-4C-in-TS decision. C1 has already measured the *runtime-switch* form as
−14%; the only surviving door is a **build-time-generated specialized dispatcher** (LOCK-4B), a
different thing from a runtime VM. (ii) Should the wasm boundary carry the bulk planes only, with
the read path staying typed-array-native? (PROVE-14 says yes; the boundary cost of the export
itself is unmeasured.)

**Disposition: ADVANCE (conservative arm) / arm-level BANK (ambitious arm).** The data + oracle +
linter arm is fully constructible on existing CONFIRMED substrate. The ambitious "mirror the full
SpanParser graph / generate a runtime TS dispatch" arm is **refuted-on-arrival by C1** and is
BANKED as a named negative-result candidate pending a build-time-specialized-dispatcher
formulation (which belongs to LOCK-4B, not a runtime VM). Cross-lock coupled to LOCK-4/5/6 —
a dependency, not a missing primitive.

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST (the API plane)

**Thesis.** The API, the diagnostics shape, the semver contract, and the migration artifact are
all **derivable from what value.js, latex-paper, and keyframes.js actually import and read** — not
from parse-that's internal ambitions. The backbone is proven by the **survival of its consumers**.

**Ground-truth supersessions (constitution ix — named).** value.js is **4.0.0** (not 3.1.0) and
declares **NO parse-that dependency** — it **bundles** parse-that into its own `dist/`
(`libraryExternal=[]`, `sideEffects:false`). This **inverts the semver model**: parse-that 1.0→1.1
is invisible to value.js's public semver (an internal build input); the break risk is entirely at
value.js *build* time. latex-paper is 0.2.1 pinned `^0.7.1` (installed 0.7.1) — the 0.7→1.1 jump
is real and untested. keyframes.js 6.0.0 rides value.js transitively.

**The whole contract is nine symbols.** `Parser`, `ParserState`, `all`, `any`, `dispatch`,
`regex`, `string`, `whitespace`, `mergeErrorState`. Everything else parse-that exports is unused
by any consumer and free to churn. **Two consumers reach BELOW the combinator layer**: they
construct `new Parser((state)=>…)` and `new ParserState<T>(source)` by hand, and value.js calls
`mergeErrorState(state, msg)` directly and drives via `parseState()` then reads state fields
post-hoc. So the contract includes the **raw `ParserState` ctor arity, the `(state)=>state`
callback signature, and the `mergeErrorState` free function** — verbatim.

**The diagnostics contract — consumers already own the shape.** parse-that's internal
`Suggestion`/`SecondarySpan`/rich `Diagnostic` vocabulary is **not what any consumer wants**.
value.js's `buildDiagnostic` (utils.ts:531-570) reads exactly `state.offset`, `state.furthest`,
`state.expected: string[]`, `state.getLineAndColumn`, `isError`, `value` and maps down to its own
flat coded `ParseIssue {code,start,end,expected,actual}`. keyframes has its own coded
`Diagnostic[]`. **Verdict for PT-E1/W2:** make the **`ParserState` field triple** (`furthest`,
`expected`, `getLineAndColumn`) per-parse-scoped and byte-stable, **readable on the returned state
after `parseState()` returns**; a design that returns a fresh `{ok,value,diagnostics}` and tears
the fields off the state **breaks value.js's post-hoc reader** (R-8D2). Suggestions/SecondarySpans
stay internal (W1 substrate only).

**The hard behavioral contract (born-RED, consumers already encode it).** value.js
`color.ts:720-740` records a version history: parse-that ≤0.8.2 fired `console.error` on every
expected failure; ≥0.9 fixed it. So: **default path = zero console I/O, zero diagnostic
allocation, diagnostics off**; the per-parse scope's "off" path is a null-scope field read (no
alloc), and W2 must not reintroduce the 0.8.2 leak (R-8D5).

**Semver + migration as deliverables.** The corrected consumer-suite gate: pack 1.1.0 → **rebuild
value.js `dist/`** (testing the checked-in stale-1.0.0 bundle is a false green, R-8D4) → `vitest`;
keyframes rides the rebuilt value.js; latex-paper = genuine `^0.7.1→^1.1.0` bump run as a
**three-step** matrix (0.7→1.0 alone, then 1.0→1.1) so any red is attributed to the cut that owns
it. A `MIGRATION.md` fragment enumerates the per-consumer symbol delta.

**Integration seams.** value.js `parsing/utils.ts:1,531-570` (`mergeErrorState`, `buildDiagnostic`),
`v4/css/types.ts:9-27` (target `ParseResult`); latex `document.ts:170` (`new ParserState` ctor),
`math.ts:10` (callback); the top barrel `index.ts` (nine-symbol re-export surface).

**Gates it carries (born-RED).** (1) Barrel-surface proof: all nine symbols resolve from the
package root `.d.ts` after the `diagnostics/` split (defeats R-8D1). (2) The consumer-shaped
diagnostics oracle: value.js's **own** `buildDiagnostic` run verbatim against a per-parse-scoped
returned failed state yields a populated `ParseIssue` (defeats R-8D2). (3) Default-path
console/alloc gate: zero `console.*`, zero diagnostic alloc (defeats R-8D5). (4) The three-repo
bundled-crater run with the mandatory rebuild step (defeats R-8D4). (5) `.test-d.ts` pins
value.js's real `all()`/`any()`/`dispatch()` shapes — precision a **superset**, not a rejection
(defeats R-8D3).

**Exact open questions.** (i) How exactly does value.js resolve parse-that at build time (bundle
input path / phantom install)? — must be pinned before any suite runs (R-8D6). (ii) Does the
result-union form (which value.js's own **v4** target adopts) coexist with the field-triple
contract the **current** value.js reader needs? — the 8B/8D collision (incompatibility map).

**Disposition: ADVANCE.** Pure construction — every primitive is npm/tsc/vitest. No missing
primitive; the family is a discipline plus a runnable crater harness.

---

## THE INCOMPATIBILITY MAP

Most pairs coexist: 8A (runtime), 8B (type), 8D (API) are orthogonal planes over one closure core
and share a boundary object they **agree on** (the phantom-erased `ParserFunction<T>` — 8A needs
its invariant call shape, 8B blesses it as the one legitimate bounded `any`). The real conflicts:

| pair | conflict | precise reason | resolution |
|---|---|---|---|
| **8B ⟂ 8D** (the sharp one) | **diagnostics surface**: 8B ADOPTs a **result-union** `{ok,value,diagnostics}` as *the* typed outcome; 8D requires the existing `parseState()` **field-triple** contract (`furthest`/`expected`/`getLineAndColumn` readable on the returned state) to stay byte-stable. | A fresh result object that tears fields off the state breaks value.js's post-hoc `buildDiagnostic` (R-8D2). Yet value.js's **own v4** target *is* a result-union — so the conflict is a **timing/versioning** split (current reader vs v4 reader), not a pure architecture clash. | **Additive, not replacing**: keep `parseState()` returning the field-bearing state (8D); offer the result-union as a *separate* `.parse(src,{diagnostics:true})`/`withDiagnostics` surface typed by 8B. Pass-2 must adjudicate whether both live or v4-migration retires the field-triple. The `buildDiagnostic` oracle (research-D §8.1) is the deciding experiment. |
| **8A ⟂ 8C (ambitious arm)** | 8C-ambitious would **generate a runtime TS dispatch** mirroring the Rust `SpanParser` graph; 8A (and C1/C2) forbid any runtime tag/switch on V8. | The SpanParser runtime-switch is **MEASURED −14% on V8** (future-research.md:76-109, P-inv-28 KILL). A generated tagged union flows through one switch → the exact megamorphic IC the LUT + arity-unroll exist to avoid. | **8C's conservative arm wins**: generate **vocabulary + name-map only**, hand-tuned bodies. The ambitious arm is BANKED; its only lawful door is a **build-time-generated specialized dispatcher** (LOCK-4B), which is *not* a runtime VM (LOCK-4C-on-V8 is refuted). 8A and 8C-conservative fully coexist. |
| **8C ⟂ 8D** | 8C wants to **minimize hand-written TS / regenerate from a spec**; 8D names three **below-combinator seams** (raw `ParserState` ctor, `(state)=>state` callback, `mergeErrorState` free fn) that must survive verbatim. | Regenerating the combinator surface risks changing ctor arity / callback signature / dropping the free-fn export — each a silent value.js **build-time** break (bundled dep). | Binding **constraint**, not a block: 8C's generator preserves the nine-symbol surface (§8D.1) verbatim; 8D's barrel-surface + crater gates are the falsifier. |
| **8C ⟂ 8D (diagnostics mechanism)** | 8C asks "should TS *also* compile-time-erase diagnostics to mirror Rust's `#[cfg]`?"; 8D/priorart R-D: no. | Compile-time erasure kills the **live per-parse diagnostic surface keyframes.js consumes** (`ResolvedKeyframes.diagnostics`). | Already conceded by 8C (R-D): **behavioral mirror, not mechanism mirror** — both zero-cost strategies satisfy clause v. |
| **8A ⟂ 8B (non-conflict, noted)** | none | 8A keeps arity-unrolled arms' runtime shape; 8B adds only positional *types* to the same arms. Orthogonal. | Coexist by construction. |

**Net:** no two families are *globally* mutually exclusive. One arm is refuted (8C-ambitious,
by C1). One surface must be adjudicated (8B result-union vs 8D field-triple) and has an additive
reconciliation. The rest are binding constraints one family places on another, all satisfiable.

---

## BLOCKED-CANDIDATE ADJUDICATION

*"A family stalled at a missing primitive equal in difficulty to the original problem is
BLOCKED-candidate. Elegance earns nothing on its own."*

**No family is BLOCKED-candidate.** Every family's primitives exist and its central mechanism is
constructible on current substrate:
- **8A** — V8 natives, the lazy-field patch, and the `mapState` snapshot are all concrete. Its
  pivot (does constant-capture inline?) is a *measurement*, not a missing primitive. Its worst
  case is a **bounded concession** (accept the megamorphic-call residue at ~+5%), not a stall.
- **8B** — variadic tuples, `const` params, tail-recursion elimination all ship in TS. The budget
  ceiling is a *risk to measure*, not a missing tool.
- **8C** — the tape crossing is CONFIRMED; schema-driven generation, wasm-bindgen, and the
  fingerprint oracle exist. Only the *ambitious arm* lacks a lawful primitive on V8, and it is
  banked, not the family.
- **8D** — pure npm/tsc/vitest construction.

**The nearest thing to a block — a BLOCKED-SHADOW to watch, not a mark.** If 8A's inlining thesis
fails (constant-capture does **not** inline) **and** the VM-core route is refuted on V8 (C1,
already measured), then de-megamorphizing input-dependent `dispatch` has **no lawful route** —
the "missing primitive" (a V8 monomorphic dispatch over N varying targets) is genuinely
equal-in-difficulty to the original megamorphic-IC problem. But this does **not** block 8A,
because the fallback (leave the closures; the *call* is cheap since the *load* is monomorphic) is
a proven-acceptable terminal answer at ~+5% no-regression. The shadow falls only on the
*aspiration* to fully de-megamorphize dispatch, which was never a gate. Brief 1 exists precisely
to collapse this shadow to a fact.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs)

Chosen to measure **both governing engines**, since priorart §0 insists they are opposite and
must not be conflated. Both are pt-scratch (parse-that TS code probes on scratch copies — the
package is READ-ONLY to the campaign).

**Brief 1 — the V8 plane pivot (8A; settles 8C-ambitious/4C from the V8 side).** Does
constant-capture inlining actually fire, and is `mapState`'s `Object.create` view really the
deopt? This single measurement decides whether the closure-core-made-monomorphic thesis is
achievable or must concede to a route C1 has already killed.

**Brief 2 — the type plane make-or-break (8B; guards 8D non-regression).** Does the tightened
type plane infer strictly more precisely, compile the real value.js grammar within a TS2589/
instantiation budget, and leave `dist/` byte-identical? Type-level totality that blows the checker
or rejects a real consumer is a failed 8B.

*(Full briefs returned in the structured output.)*
