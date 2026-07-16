# LOCK-8 — THE TS BACKBONE PLANE — pass-3 FAMILY-SPECS (distiller of record, Fable-tier)

*One cogent specification per family while routes remain incompatible. Four orthogonal planes
over one closure core: 8A runtime (V8 JIT), 8B type (TS checker), 8C correspondence, 8D API. Two
engines govern and are never conflated (V8 JIT ≠ TS checker). parse-that/typescript is READ-ONLY
to the bbnf campaign; every family lands only via letter BBNF-PT-G (owner or a parse-that-side
session); prototypes run on scratch copies base-pinned to HEAD (`ef10d5b` / `@mkbabb/parse-that`
1.0.0). This doc supersedes the pass-2 FAMILY-SPECS and folds pass-3 research-{A,B,C,D,priorart}.*

---

## 0. THE PLANE-WIDE GOVERNING FACT — THE CENSUS RE-FLIPPED (value.js is DUAL-STATE)

**Pass-2's load-bearing fact ("value.js divorced parse-that; latex-paper is the sole surviving
consumer") is SUPERSEDED. It was a BRANCH ARTIFACT.** research-D ran a per-tree *and per-branch*
census; the distiller git-verified it this pass:

| repo · line | version | parse-that dep | surface | posture |
|---|---|---|---|---|
| **value.js · `master` (shipping)** | 3.1.0 | **`^1.0.0`** (git-verified) | **9 symbols** `{Parser,ParserState,all,any,dispatch,regex,string,whitespace,mergeErrorState}`, **59 `all()` sites** | **LIVE full consumer** |
| value.js · `tranche-u` (137 ahead, checked out) | 4.0.0 | none | ∅ | **PAUSED at owner order**, unmerged, PT-free |
| **latex-paper · master** | 0.2.1, pins `^0.7.1`→installs **0.7.1** | `^0.7.1` (pre-1.0.0) | **5 symbols** `{Parser,ParserState,regex,string,any}`, 8 sites, `.skip`×11 | LIVE, semi-dormant (2wk stale) |
| keyframes.js · master | 6.0.0 | none (transitive via value.js only) | ∅ direct | PT-free (direct) |

**INTER-REPORT INCOMPATIBILITY (the sharpest of the pass).** research-A §2B and research-B §4
BOTH report value.js "divorced / REMOVED / `dependencies:{}`" — because both censused the
*checked-out `tranche-u` working tree* and never `git show master:package.json`. research-D is the
authoritative branch-aware census, corroborated by the distiller's git probe (`value.js` HEAD =
`tranche-u`; `master:package.json` = `^1.0.0`). **The 59-`all()`-site corpus that pass-2 declared
DEAD is LIVE on `master`.** Every pass-2 conclusion premised on the divorce is re-opened:

- `mapState`-has-0-consumers (which dissolved the pass-1 8A defects) — was a `tranche-u` census;
  **must be re-run against value.js-`master`** before it is re-banked.
- 8A center-B's `all()`/`any()` justifying corpus — **not dead** under `master` (disposition A).
- 8B's native instantiation budget — the 59 `all()` sites are the real TS2589 stress, LIVE again.
- 8D's crater — `master` is a *second positive leg*, not a phantom.

**The owner question sharpens (XL-G P1/P2).** It is no longer "is value.js divorced" but: **will
`tranche-u` merge — and until it does, `master` (^1.0.0, full surface) binds any 1.1.0 cut.** Two
dispositions are carried through every family (research-D §2), owner-gated and mutually exclusive:

- **Disposition A — `master` authoritative** (tranche-u stays paused/unmerged): the full 9-symbol
  surface is LIVE; nothing prunes; the 59 `all()` sites are the tightening's blast radius.
- **Disposition B — `tranche-u` is destiny** (owner confirms the merge): external contract shrinks
  to latex-paper's 5 symbols; `all`/`dispatch`/`whitespace`/`mergeErrorState` become
  first-party-consumed (the shipped gate `proof-no-dead-combinator.mjs` already enshrines the
  `test/` tree as a lawful Y.13 consumer). **No combinator prunes under B either** — the algebra is
  not amputated to fit one transiently-frozen downstream.

**Two owner preconditions, both born-verified, gate every 8A/8B/8D number:** (P1) tranche-u merge
intent; (P2) latex-paper is live-forward, not a fossil (2wk stale, gaps to March — a positive gate
only if baseline-GREEN at 0.7.1). **No family is BLOCKED-candidate**: every central mechanism is
constructible on current substrate, and the census re-flip *restores* consumers pass-2 thought
gone. The one BLOCKED line (8A center-B) is a sub-deliverable whose consumer-half is now re-opened
by the re-flip; its mechanism-half stays blocked on LOCK-4B.

---

## FAMILY 8A — THE SHAPE-STABLE V8 RUNTIME CORE

**Architecture.** The V8-first formulation of the closure combinator core: every `ParserState` and
combinator object shape-stable and monomorphic *by construction*, an allocation-zero hot path,
diagnostics and packrat as shape-preserving optional planes — proven by IC/`%HaveSameMap` probes
and cold *paired* harnesses (single-build-per-process, A,B,A,B interleaving; never `vitest bench`,
which is load-confounded). 8A owns the runtime execution face (V8 JIT) and must NOT over-reach into
the central dispatch, which is structurally megamorphic and LOCK-4 territory.

**Mechanism (three pillars, `path:line` in `typescript/src/parse/`).**
- **P1 fixed-from-birth hidden class.** `ParserState<T>` declares five positional fields
  (`src,value,offset,isError,furthest`, `state.ts:36-53`) + three diagnostic fields `undefined`
  at construction (PROVE-11 lazy-field symmetry: eager `= []` sites **6→0**, fused alloc **1.84→
  0.73 B/op, −60%**). `value:T` writes only through the single choke `unsafeSetValue`
  (`state.ts:87-89`) — the sole by-design megamorphic-write site. **Substrate, landed-grade.**
- **P2 the megamorphic dispatch spine is irreducible.** Every combinator threads `this.parser(state)`
  through the `.parser` function-pointer field (`parser.ts:53,82,88`; measured 5.9% CPU,
  `perf-optimization-ts.md:202-207`). 8A keeps every *object* monomorphic so the only polymorphism
  V8 confronts is this deliberate indirection, then reduces what rides across it (alloc, tuple
  materialization) — it does NOT pretend to monomorphize it (prior-art LAW: the runtime
  tagged-union/`switch` escape was measured **−10–14%** and KILLED, B.W0/P-inv-28).
- **P3 construction-time specialization.** Arity unrolls run their `if(n===2/3)` branch once at
  `new Parser(...)` (`leaf.ts:154-273` `fuseAll`; `leaf.ts:28-79` `any`), yielding one
  constant-folded closure per call site; `dispatch` builds its `Int8Array(128)` LUT once
  (`leaf.ts:100-151`).

**THE PASS-3 ADVANCE — the arity unroll does NOT inline; the win is alloc-elimination, not
inlining (research-A §2A, `--trace-maglev-inlining` on Node 26 default tier).** Fused `fuseAll`
closures measure **169 bytecode @ arity-2, 234 @ arity-3** — both overflow V8 maglev's **100-byte**
inline budget; `parseStateInner` (162) is likewise never inlined. **Structural leg (refuted): the
unroll buys no call-site inlining at any arity.** Temporal leg (confirmed): the cold-paired win is
real and reproduces adversarially (+10.1/+15.6% fused, alloc −60%) **because the lever is
allocation elimination** (one flat array vs N−1 nested tuples), *not* inlining. **The two legs MUST
be carried separately** — any PT-3/W4 claim attributing speed to inlining reasons on a false model
(R-A1). Small leaves DO inline (`stringParser`,`unsafeSetValue`,`ok`, ctor); the *sequencing* layer
is an inline boundary by construction, and only a build-time emitter can move it.

**center-B (arity-N specializer) disposition — the double-block, restored and re-scored.**
- **Mechanism half (stays blocked):** the *only* route under the 100-byte ceiling is a build-time
  combinator→JS fuser emitting straight-line offset-threading per static-arity call site (the
  4B-TS shape, research-A §2C). This is a **NEW subsystem** and a **different mechanism** from the
  runtime unroll (a code-generator vs a construction-time closure-picker). LOCK-4B's own REGISTRY
  banked its TS face: "terminal-role BANKED→compile-backend," win "no V8 analog (T1)," "isomorphism
  half 0% built." center-B's inlining goal is mechanism-blocked there. **Named supersession
  standing (Constitution ix); do not silently drop.**
- **Consumer half (RE-OPENED by §0):** pass-2 blocked center-B partly as consumer-less (dead
  value.js corpus). The census re-flip restores 59 LIVE `all()` sites on value.js-`master` under
  disposition A → the runtime unroll's corpus is **not** dead. Under disposition B it re-anchors to
  latex's `any()` arity-3/variadic + first-party bench.
- **Verdict:** the runtime unroll SHIPS as alloc-elimination (real, measured, consumer-live under
  A). The inlining ambition is a LOCK-4B build, un-run. center-B stays a BLOCKED *sub-deliverable*
  on the mechanism axis only.

**Integration seams.** shape core `state.ts:36-53/55-73/87-89`; dispatch `parser.ts:24-49/51-75`;
specializers `leaf.ts:28-79/100-151/154-273`; `mapState` `parser.ts:177-180` (`Object.create(state)`
— the one prototype-chain object; R-A3: a hot grammar routing through it can deopt callers);
diagnostics latch `utils.ts:6`; packrat `packrat.ts:156,290` (unarmed no-op); bench floor
`test/benchmarks/pt-b3-fusion.bench.ts`.

**Gates it would carry (born-RED).**
1. Per-arity inline verdict base-pinned (XL-H): confirm the 169/234 non-inline boundary at arities
   1..6 on a fresh 1.0.0 scratch; record where the general arm crosses. *Structural, deterministic.*
2. `%HaveSameMap(diagOn,diagOff)===true` (fixed-from-birth) under `--allow-natives-syntax`; assert
   `mapState` is the ONLY prototype-chain object and its callers are not deopted.
3. Alloc-zero certification on the default path (`--expose-gc` heapUsed B/op, packrat unarmed,
   diagnostics off): the −60% holds, no per-parse alloc slips in.
4. Maglev inlining trace at each arity (Node 26 default production tier — un-run 2 prior passes).

**Exact open questions.**
- **`mapState` consumer re-census on value.js-`master`.** Pass-2's "0 consumers" was a tranche-u
  read; `master` is unchecked. If `master` routes a hot grammar through `mapState`, R-A3's deopt
  risk is live and the pass-2 "8A defects dissolved" claim is contingent, not settled.
- **The build-time-emitter counterfactual (the one experiment that could dissolve §2A's ceiling):**
  does per-site straight-line `all(a,b,c)` emit <100 bytecode and INLINE where the runtime unroll
  cannot? Un-run; LOCK-4B-homed.
- `CEILING` read from the surviving corpus histogram (latex+self-use under B; +value.js-master 59
  sites under A).

---

## FAMILY 8B — TYPE-LEVEL TOTALITY

**Architecture.** The TS *checker* plane: a typed combinator algebra with no `any` in public return
types, the instantiation budget within TS2589, and the byte-identical-`dist` (zero-runtime-cost)
invariant proven by construction. This family types a *runtime* parser — it does NOT move parsing
into types (the parsebox recursion-depth / checker-slowdown ceiling; the ts-parsec friction tax).

**THE PASS-3 REVERSAL that re-founds the family (research-B §0).** The R4/PT-3 premise — *"the
`Parser<any>` inference leak; combinators return `Parser<any>`"* — **does not reproduce at HEAD.** A
tsc reveal-test at 1.0.0/tsc 5.9.3: `all(string,regex)`⇒`Parser<[string,string]>` (precise tuple),
`any(a,b)`⇒`Parser<string>`, `.skip`/`.sepBy`/`.many` all precise. The `any` in
`all<T extends Array<Parser<any>>>` is a **constraint BOUND, not a return-type leak** —
variadic-tuple inference binds `T` to the actual argument tuple; the mapped `ExtractValue<T>`
recovers per-position types. **8B is reframed: not "fix a soundness hole" but "delete a cosmetic
`any` from the public `.d.ts` at proven zero cost, and prove the algebra was already total."**
SPEC-fidelity divergence: PT-3 §3 / R4 §5 overstate the defect (MED→cosmetic).

**8B-G-ASUNKNOWN RESOLVED — the PROTO-1 `all` cast is a LAWFUL PHANTOM ERASURE (research-B §1,
discharges GRAFT A by classification).** `Parser<T>`'s `T` is a **phantom** parameter: its sole
`T`-typed field is `parser: ParserFunction<T>`, and `ParserFunction<T> = (ParserState<any>) =>
ParserState<any>` — `T` is *already* erased to `any` inside its own definition. No runtime value
flows through a `T`-slot, so `parsers as unknown as Parser<unknown>[]` re-labels a phantom; it
cannot make a value inhabit a type it lacks. This is the GHC `Coercible` law (coercion sound iff the
parameter is representationally irrelevant — a phantom always is). **The pass-2 flagged flagship
soundness leak is a documentable phantom erasure, not a leak.** The ONE load-bearing `any` is
`ParserFunction`'s `ParserState<any>` argument (the central megamorphic IC threads heterogeneous
states through one indirect call) — **KEEP + document** as an implementation-boundary variance
requirement. The charter phrasing refines: "no `any` in *public return types*" (achieved), not "no
`Parser<any>` anywhere."

**Mechanism — the tightened signature, constructed and measured (research-B §2).**
```
any<T extends ReadonlyArray<Parser<unknown>>>(...parsers: T)   // was Array<Parser<any>>
all<T extends ReadonlyArray<Parser<unknown>>>(...parsers: T)   // was Array<Parser<any>>
fuseAll(parsers: ReadonlyArray<Parser<unknown>>)              // was Array<Parser<any>>
skip(parser: Parser<unknown>): Parser<T>                       // drop dead <S>, kill Parser<T|S>
sepBy(sep: Parser<unknown>, …): Parser<T[]>                    // drop dead <S>
```
Inference preserved byte-identical (non-`const` mutable tuple); **no TS2589 at arity-12** (the
homomorphic `{[K in keyof T]:…}` mapped type is O(arity), non-recursive). **`const`-variant is
semver-visible** (narrows to `readonly` tuples — breaks mutate-the-result consumers) → ship
non-`const`; `const` is a named future option. Consolidate the twin `ExtractValue`
(`parser.ts:9-11` ≡ `leaf.ts:155-157`) into one `src/parse/types.ts` (DRY, closes the god-DRY item).

**8B-G4'-EMIT DISCHARGED — byte-identical `dist` PROVEN on the real toolchain (research-B §3).**
Two scratch `typescript/` copies, `vite build` each, HEAD vs tightening: aggregate `shasum` over
**all `dist/*.js`+`*.cjs` is IDENTICAL** (`ef2a829…` both trees); per-file `.js`/`.cjs` diff empty;
only `leaf.d.ts`/`parser.d.ts` move, only in the four signatures. This is the constitution's
clause-v (type plane costs zero at runtime) **proven for TS by construction** — `isolatedModules`
erases types before rollup — on the package's own `vite build`, not an esbuild proxy. **The
clause-v gate that "never fired" (pass-2) FIRED, GREEN.** Step-(i) toolchain-migration delta stays a
separate attributable diff taken FIRST.

**Gates it would carry (born-RED).**
1. **The generateTrace INSTANTIATION-COUNT budget on the NATIVE corpus** — the one figure deferred
   THREE passes. `tsc --diagnostics --generateTrace` HEAD vs tightened; report `Instantiations`+
   `Types` delta + TS2589 count. **Native corpus = value.js-`master`'s 59 `all()` sites** (the real
   stress, LIVE again under §0) — NOT the synthetic latex-shape stress PROTO-1 used. Under
   disposition B, record it un-runnable and take it on `.test-d.ts` + self-use. **This is Brief 1.**
2. latex-paper 0.7→1.1 crater: pack a tightened tarball, `npm i`, `npm test` + full `tsc`/`vue-tsc`;
   `.skip`/`any` sites stay green (expected; §2 non-breaking). Minor-step alone first (attribution).
3. `restore(value:any)→value:T` (`state.ts:79`): no new error at internal call sites.

**Exact open questions.** The native instantiation count (the *count* delta — inference + byte-
identity + no-TS2589 are proven; the count is not); whether disposition B makes the native number
un-runnable; the `skip`/`sepBy` residue tightening's crater confirmation.

---

## FAMILY 8C — THE ISOMORPHIC MIRROR (correspondence)

**Architecture.** The TS plane as the lawful mirror of the Rust parse-that core. 8C does NOT build
the general machinery (linter→6A, single-spec generator→6B, differential harness→6D) — the pass-1
triple double-home is RESOLVED by cession. 8C's SOLE residue is parse-that-specific DATA: the
machine-checkable **DIVERGENCE LEDGER**, the regex **hazard-class enumeration + discriminating
corpus**, and the routing of the regex-engine decision to LOCK-5. 8C supplies data; the three
mechanisms are LOCK-6's (R-C4 — no duplicate harness).

**THE PASS-3 CENTRE OF GRAVITY — the regex row PARTITIONS by expressiveness, and the LIVE hazard is
expressiveness, not selection (research-C §3, R-C7 "highest value").** The two engines diverge along
TWO independent axes:
- **Selection axis (H1-H4, H9):** Rust `nfa.rs:311-314` *builds* priority-ordered ε-edges (intending
  leftmost-first to mirror JS) → `dfa.rs:189-201` subset construction **discards priority**
  (`BTreeSet<StateId>`) → `find_at` returns **leftmost-LONGEST**. JS `RegExp /y` is
  **leftmost-FIRST**. Today `DRIFT-RISK`. **The engine is internally self-contradictory** (priority
  built then thrown away) — a real bbnf-regex/LOCK-5 defect (constitution ii), not merely a mirror
  question.
- **Expressiveness axis (H5-H7):** the Rust `Hir::Look` (`hir/mod.rs:170+`) is a **fixed** assertion
  enum (Start/End/word-boundary) with **no** general `Look(Box<Hir>)` — **lookahead `(?=…)`,
  lookbehind, backreference are INEXPRESSIBLE Rust-side.** `HARD LAWFUL-NON-MIRROR(inexpressible)`.
- **THE DECISIVE FACT: the live corpus exercises EXPRESSIVENESS (H5), not selection.** latex-paper
  ships `regex(/[\s\S]*?(?=\])/)` and `/[\s\S]*?(?=\))/ ` (`math.ts:45,57`) — lazy `*?` **plus
  lookahead**. R-C1 is not a contrived `/a|ab/`; it is shipping consumer code, and the Rust HIR
  cannot even hold the assertion.

**The two routes and 8C's adjudication (routed to LOCK-5 as a DECISION).**
- **Route A — priority-preserving determinization** (RE2 Perl-mode: ordered thread-list ε-closure,
  highest-priority accept, Cox VM Part 2). **Dissolves H1-H4/H9** (the selection axis). Costs, named:
  minimization weakens (a tagged/TDFA must partition on match-priority → fewer merges, larger
  tables); state-blowup risk (DFA-size cap + PikeVM fallback, inherited). **It does NOT touch H5-H7.**
- **Route B — build-time grammar constraint** (forbid/normalize ordered-alternation-shorter-first,
  lazy quantifiers, lookaround). Cheaper engine, constrains the author, **and BREAKS the live latex
  leaves** (forbid-without-rewrite cannot accept H5).
- **8C's recommendation (parsimonious, constitution-fit):** since the LIVE divergence is
  expressiveness (H5), which route A cannot fix, **HOIST context-sensitive assertions out of regex
  leaves into the combinator layer** (`[\s\S]*?(?=\])` → a `take_until("]")` composition both
  engines mirror), reserve route A only if a live *selection-axis* divergence appears, and mark
  H5-H7 a permanent `LAWFUL-NON-MIRROR` with the rewrite as the migration. **This REFRAMES the
  pass-2 "8C-guard ⟂ LOCK-5-route-A-fix" incompatibility:** neither the guard nor route A resolves
  the live class; the assertion-hoist does. The engine choice is LOCK-5's; 8C supplies the corpus.

**Deliverable — the DIVERGENCE LEDGER (still UN-BUILT; the family's whole residue).** One classified
row per unit (`MIRRORED` / `LAWFUL-NON-MIRROR(reason)` / `DRIFT`), emitted `scratch/8c-ledger.tsv`
(landed via BBNF-PT-G). **Zero DRIFT rows** — the mirror is *dense* (method surface near-1:1); the
exceptions are four subsystem rows (SpanParser Rust-only zero-boxing; packrat TS-only; diagnostics
feature-vs-scope; regex partition) plus the mandatory **`not↔minus` NAME-INVERSION** (`minus.rs`
`not` = EBNF `-` = TS `minus`; TS also has a distinct `not`/`negate` — the linter mis-pairs without
it). The regex corpus is H1-H10 paired triples `{id,regex,input,expect_a,expect_b,expect_c}` **plus
the two live latex fixtures**, **PARSED not string-matched** (R-C8: `\*?` at `commands.ts:182,269`
is optional *literal* asterisk, not lazy — a naive `grep '*?'` miscounts).

**Integration seams / cession.** 6A consumes MIRRORED rows as its name-map + LAWFUL-NON-MIRROR rows
as its exemption whitelist; 6B consumes the class column (emit-eligible vs forbidden-to-mirror); 6D
consumes the parity-floor column (span-triple, regex-triple, diagnostics-triple). GRAFT B: 8D's
`state.diag` boolean mechanism populates 8C's diagnostics ledger row so 6D asserts the real scoped
surface. **Tape-crossing 4.78× stays fenced pure-JS-decode** (wasm→JS boundary UNMEASURED; SPEC §0
shim-perf law; home LOCK-6/6C).

**Gates it would carry.** The ledger, mechanically generated (AST/grep census, the un-built
deliverable); the H1-H10+2-live corpus parsed-not-matched; the priority-DFA scratch prototype's
triple output (Brief 2) as LOCK-5's decision input.

**Exact open questions.** Whether route A is worth its minimization cost given it doesn't fix the
live class; whether the assertion-hoist rewrite is accepted by LOCK-5 as the migration; the ledger↔
6A whitelist handshake wiring; the tape-crossing home (LOCK-6 adjudication, deferred). **8C's
ceiling is LOCK-6-bounded** — it cannot reach 100% independent of LOCK-6 (flag for GESTALT-P).

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST

**Architecture.** The backbone designed backward from its LIVE consumers; the API/semver/migration
contract toward 1.1.0 as a first-class deliverable. 8D forks the other three families via the Y.13
consumer-set decision and now via the §0 census re-flip.

**Q1 consumer-set — NO combinator prunes under EITHER disposition (research-D §2).** parse-that's own
`scripts/proof-no-dead-combinator.mjs` already enshrines the first-party `test/` tree as a lawful
consumer (`:63-68`) — Y.13/constitution-viii is discharged for first-party combinators *by the
shipped gate*, not a new assertion. Two gate flaws, both fixable: (a) it omits `latex-paper/src`
from `consumerTrees`; (b) its vacuity guard hard-codes `dispatch` as known-live (`:49-58`), but
`dispatch`'s only external consumer is value.js-`master` — the anchor dies if tranche-u merges.
**Disposition A:** keep all 9 symbols (each externally consumed by master). **Disposition B:** keep
all; reclassify `all/dispatch/whitespace/mergeErrorState` external→first-party-consumed; **pruning a
coherent algebra to fit one transiently-frozen downstream is the wrong move.** The only forced edit
is the gate fix (add latex; re-anchor the vacuity guard).

**Q3 per-parse diagnostics — `state.diag` boolean field (FAVORED, research-D §4).** The residue is a
single module-global `let diagnosticsEnabled` (`utils.ts:6`); storage is ALREADY per-parse on
`ParserState`. Mechanism = replace the global with a **primitive boolean field `state.diag`** on the
already-per-parse state, threaded from a parse option. **Disabled cost = ONE boolean read in the
monomorphic hidden class** — zero alloc, zero new object, zero transition (a boolean fixed at
construction never transitions). REJECT the `DiagnosticScope` object (per-parse alloc on the off
path — the R2 trap). Deletion-proof the global (`rg 'let diagnosticsEnabled'`→0). **GATED on the
PROVE-11 lazy-field lever landing first** (`state.ts:45-46` `suggestions`/`secondarySpans` are STILL
eager `= []` at HEAD — a Y.13 substrate-consumer pair that must land together).

**Q4 inference tightening × consumer type-check.** The native blast radius is value.js-`master`'s 59
`all()` sites (TS2589 risk), NOT latex's 5 (R5). latex's single highest-risk site is
`document.ts:81 const inlineNode: Parser<LatexNode|null> = any(...)` (explicit annotation absorbing a
currently-`any` union) — caught only by `tsup && vue-tsc`, never `vitest`. **Dual-instance hazard is
LIVE, not hypothetical:** latex carries nested parse-that 0.6.0 via bbnf-lang; `compile.ts:19,81`
drives 0.6.0-built `Parser`s via `.map()` while typing them as the direct 0.7.1 `Parser`. Resolution:
**DOCUMENT (MIGRATION.md row) + runtime-gate; do NOT force-dedupe** (a 0.6.0→1.1.0 jump for
bbnf-lang's internals could break bbnf-lang; the crossing is defensively `Parser<any>` so the
tightening does not bite).

**Q2 migration crater — 2 positive legs + 1 negative guard, UN-RUN but constructable.** Leg 1
value.js-`master` (POSITIVE, resurrected — the 59-`all()` native stress); Leg 2 latex-paper
(POSITIVE); Leg 3 keyframes (NEGATIVE tripwire). Mandatory reinstall (`npm i ../parse-that-<cand>.tgz`
— the false-green tripwire). **Minor-step-alone attribution** (value.js 1.0→1.1 alone; latex 0.7→1.0
alone). **Baseline-GREEN-first is a hard precondition** (a consumer red at its pin independent of the
candidate makes the gate uninterpretable — halt).

**Gates it would carry (born-RED).** The 2-leg crater run green (or exact break enumerated); the
minor-step-alone attribution; the TS2589 probe on the native corpus (shared with 8B Brief 1); the
`state.diag` `%HaveSameMap` oracle (gated on the lazy-field lever); the negative-guard tripwire; the
gate fix re-run green under both dispositions.

**Exact open questions.** Both owner preconditions (P1 tranche-u merge; P2 latex live-forward); the
crater result (un-run); whether latex's baseline is GREEN at 0.7.1 (the fossil question).

---

## INCOMPATIBILITY MAP (pass-3 — the census re-flip re-opens two pass-2 "evaporated" collisions)

| pair | pass-2 state | pass-3 disposition | precise reason |
|---|---|---|---|
| **research-A/B census ⟂ research-D census** (INTER-REPORT) | — | **research-D AUTHORITATIVE** | A/B censused only the checked-out `tranche-u` tree ("divorced"); `master:package.json` = `^1.0.0` with 59 `all()` sites (git-verified). Branch-incomplete vs branch-aware. |
| **disposition A ⟂ disposition B** (value.js master-full vs tranche-u-PT-free) | — | **owner-gated, mutually exclusive; carry both** | master ships the full 9-symbol surface; tranche-u is paused/unmerged. A 1.1.0 cut CANNOT assume value.js has left. The crater MUST name its branch. |
| **8A center-B ⟂ Y.13 ∧ LOCK-4B** | COUPLED + mechanism-blocked | **consumer-half RE-OPENED (disp A restores 59 sites); mechanism-half UNCHANGED** | the unroll's `all()` corpus is LIVE on master; but the inlining goal is a NEW build-time emitter 4B banked "no V8 analog (T1), isomorphism 0% built." |
| **8A runtime-unroll ⟂ 8A inlining-goal** | conflated as one deliverable | **SPLIT** | 169/234 > maglev 100-byte budget; the unroll ships as *alloc-elimination* only; inlining needs the 4B-TS code-generator (different mechanism). |
| **8A-mapState-front ⟂ 8D-Y13-prune** | ADD (mapState dead) | **RE-OPENED — re-census mapState on value.js-`master`** | pass-2's "mapState 0 consumers" was a tranche-u read; master unchecked. The "8A defects dissolved" claim is contingent until re-run. |
| **8B "Parser<any> leak" ⟂ HEAD reality** | open soundness hole | **EVAPORATED** | the leak does not reproduce; `any` is a constraint bound; the algebra was already total (research-B §0). |
| **8B-G-ASUNKNOWN (PROTO-1 `all` cast) ⟂ 8B thesis** | flagship soundness leak | **RESOLVED — lawful phantom erasure** | `Parser<T>`'s `T` is phantom (erased in `ParserFunction<T>`); the cast re-labels a phantom, sound by the `Coercible` law (research-B §1, discharges GRAFT A). |
| **8C regex-guard ⟂ LOCK-5-route-A-fix** | ROUTE to LOCK-5 (guard forecloses fix) | **REFRAMED — neither fixes the LIVE class (H5); the assertion-hoist does** | route A fixes selection (H1-H4); the live hazard is expressiveness (H5 lookahead, inexpressible in Hir). Hoist the assertion into combinators; reserve route A for a live selection divergence. |
| **8B ⟂ 8D** (result-union vs field-triple) | ADJUDICATED field-triple | **UNCHANGED, reinforced** | neither latex nor value.js-master reads a result-union; it is 8B's additive typed wrapper, Y.13-thin. |
| **8A ⟂ 8C-ambitious** (runtime TS dispatch) | BANKED | **UNCHANGED — BANKED** | −14% runtime-switch KILL is law; the only door is a build-time-generated dispatcher (= LOCK-4B). |

---

## PLANE-WIDE GAPS & CROSS-LOCK ROUTING

- **The census re-flip re-adjudicates the pass-2 registry's load-bearing fact.** Every pass-2
  conclusion premised on "value.js divorced" (mapState-0-consumers, 8A defects dissolved, 59-tuple
  corpus dead, crater=1-positive) is re-opened pending the branch-aware census + the owner answer.
  The registry must record research-A/B as branch-incomplete and research-D as authoritative.
- **Both owner preconditions restated and sharpened (gate every 8A/8B/8D number):** (P1) tranche-u
  merge intent; (P2) latex live-forward. Until answered, value.js-`master` binds as a live gate.
- **The LOCK-5 regex-engine self-contradiction (priority built then discarded) is routed to LOCK-5
  as a DECISION, REFRAMED by H5:** route A (priority-DFA) is elegant for selection but does not fix
  the live expressiveness class; the parsimonious resolution is the assertion-hoist. 8C supplies the
  discriminating corpus; the engine choice is LOCK-5's (PROPAGATE / GRAFT C).
- **UN-BUILT / UN-RUN deliverables:** the 8C divergence ledger (the family's whole residue); the 8D
  2-leg crater; the 8B native-corpus instantiation budget (Brief 1); the priority-DFA + H5-gate
  triple (Brief 2); the 8A center-B build-time-emitter counterfactual; the mapState master re-census.
- **BANKED / SETTLED (do not re-litigate):** the −14% VM/runtime-switch refutation (C1); packrat
  homed + CONVERGED (type `Parser<T>`, float64 key, `MemoCell.ans` cold-quarantined, UNARMED across
  live consumers); `opt`-in-`all` discharged (ADOPT `DropUndefined` + documented widening); SFI
  predicate answered/RETIRED; `Parser<T,E>` labels + 8C-ambitious runtime dispatch + `dispatch<const
  K>` BANKED. NEW this pass: the byte-identical `dist` invariant PROVEN (8B-G4' discharged); the
  `Parser<any> leak` reversal; the `as unknown as` lawful-phantom classification (8B-G-ASUNKNOWN
  discharged).
- **BLOCKED-candidate assessment:** NO family is BLOCKED-candidate — every central mechanism is
  constructible on current substrate, and the census re-flip *restores* consumers pass-2 thought
  gone. 8A center-B remains a BLOCKED sub-deliverable on the *mechanism* axis (4B-TS) only; its
  consumer axis is re-opened favorably under disposition A.

---

## PASS-3 CONVERGENCE (directional; the registry/agglomerator sets the authoritative floor)

| family | plane | status | direction | one-line |
|---|---|---|---|---|
| **8A** | V8 runtime | ADVANCED | ↑ (inline verdict answered) / caution (mapState master re-census) | the arity unroll does NOT inline (169/234>100); win is alloc-elim; center-B split into shippable alloc-elim + blocked inlining-via-4B-TS |
| **8B** | type | ADVANCED | ↑↑ (leak evaporated, dist proven, ASUNKNOWN resolved) | reframed to cosmetic-`any` deletion at proven-zero-cost; only the native instantiation *count* remains un-run |
| **8C** | correspondence | ADVANCED | ↑ (partition + live-H5 finding) | regex partitions by expressiveness; the LIVE hazard is H5 lookahead (inexpressible), not selection; ledger still un-built |
| **8D** | API | ADVANCED | caution (census DUAL-STATE) | "sole survivor" superseded; two owner-gated dispositions; no combinator prunes; crater un-run |
| *8A center-B* | — | BLOCKED (sub-deliverable) | mechanism-blocked on 4B-TS; consumer-block RE-OPENED by census re-flip | |
