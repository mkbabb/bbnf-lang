# LOCK-8 — THE TS BACKBONE PLANE (parse-that/typescript) — pass-3 FAMILY SPECIFICATIONS

*Synthesis of record, pass 3 (Fable-tier judgment). One cogent spec per family — architecture,
mechanism, integration seams (path:line at HEAD `ef10d5b`, `@mkbabb/parse-that@1.0.0`), the gates
it carries born-RED, and its exact open questions — then the incompatibility map. This doc folds
pass-2 PROTO-1 (8B) and PROTO-2 (8C) that the pass-2 FAMILY-SPECS was frozen BEFORE (the
cross-cutting defect both critics named); the gap ledger here is the corrected one, not the stale
09:50 text. Standing constraints, conveyed as fact: parse-that/typescript is READ-ONLY (any landing
rides letter BBNF-PT-G, owner or parse-that-side session; prototypes on scratch copies per the
PROVE-11 recipe); two engines that must never be conflated — the V8 JIT (8A) and the TS checker
(8B); every measurement names its HEAD and base-pins worktree prototypes (XL-H); the type plane
costs zero at runtime by construction (Constitution v).*

---

## 0. THE LOAD-BEARING PASS-3 CORRECTION — value.js is BIFURCATED, not divorced

The pass-2 registry recorded value.js as fully "MIGRATED OFF" (HEAD `86a9a17`, branch `tranche-u`,
0 imports). **Pass-3 refines this by a fresh per-tree census run three ways (research-A §4,
research-B §6, research-D §1), which converge on a materially different graph:**

| consumer | ref | version | parse-that dep | imports | posture |
|---|---|---|--:|--:|---|
| **value.js** | `master` (origin/HEAD, published) | **3.1.0** | `^1.0.0` (`^` → 1.1.0) | 9 symbols; **61 `all` / 96 `any` / 6 `dispatch`** (research-A §2) | **LIVE-published** |
| **value.js** | `tranche-u` dev tip | **4.0.0** | absent / pruned | 0 | **SHED — PAUSED, unmerged** |
| **latex-paper** | 0.2.1, pin `^0.7.1`, installed 0.7.1 | — | 5 symbols (`Parser ParserState regex string any`) | 8 sites; `any()`×3; methods `map/skip/trim/then/next/opt/chain/not` | **LIVE — sole undisputed external** |
| **keyframes.js** | 6.0.0 | — | none (dep = value.js 4.0.0) | 0 | **DEAD — no path any hypothesis** |

**The correction that reshapes the plane:** the divorce is confined to the `tranche-u` dev branch,
which is **PAUSED** ("THE PAUSE — V campaign halted at owner order post-pass-4, thesis 78%",
research-D §1) and **not merged to master**. **`master` retains the live `^1.0.0` dependency and the
full 61-`all`/96-`any`/6-`dispatch` corpus.** Researchers differ only on whether tranche-u's 4.0.0
divest is *committed* (B, D) or *working-tree-staged* (A) — immaterial; all three agree master holds
the live corpus. Therefore the pass-2 "the 59-`all()` live corpus no longer exists" is **struck**:
it exists at value.js-master, and revives 8A's arity corpus and 8B's native instantiation-budget
corpus **under H2, provided every gate pins `master` not the checked-out `tranche-u`** (measuring
tranche-u falsely reports an empty consumer — research-A R-A, research-B R-C, research-D R1/R5).

**Two hypotheses, both carried (XL-G, non-blocking; consistent with CROSS-LOCK P1):**
- **H1 (tranche-u lands):** value.js exits the consumer set; the arity/`all` specialization loses
  its live external corpus, falling back to first-party bench grammars + latex's thin surface.
- **H2 (master posture holds):** value.js-master @ 3.1.0 is a live `^1.0.0` consumer; the corpus is
  real and native-measurable.

**P2 (the pass-2 symmetric gap — latex live-forward vs fossil):** unchanged and un-closed — latex
is 2 weeks idle, 2 API cuts behind (0.7.1 vs 1.1.0). Both owner confirmations remain preconditions
on banking any 8A/8B/8D *number*; the *mechanisms* proceed under both arms.

---

## FAMILY 8A — THE SHAPE-STABLE CORE (the V8 runtime plane) — ADVANCED

**Architecture.** Two physically distinct wins the directive requires be reported SEPARATELY;
conflating them is the family's principal reporting hazard (research-A §1).

- **STRUCTURAL leg (allocation + hidden-class; JIT-tier-INDEPENDENT).** `ParserState` is
  fixed-from-birth: `expected?` and (post-lever) `suggestions?`/`secondarySpans?` are declared
  native class fields initialized `undefined`, so the hidden class is stamped at construction and
  never transitions when a diagnostic later seeds an array (`state.ts:36-53,43`). The certified
  lever removes 6 eager `= []` alloc sites → 0 (ctor ×2, `mergeErrorState` ×2, `resetErrorState`
  ×2), corroborated by fused alloc **−60.1% B/op** (exact, `--expose-gc`). This leg is REAL and
  largely BANKED (PROVE-11). It is 8A's defensible claim.
- **TEMPORAL leg (JIT inlining; tier-DEPENDENT).** The V8-discipline core and the family's
  least-understood claim. The engine's governing surface is the single indirect call through the
  `.parser` function-pointer field (`parser.ts:53,82,88,131,137`; `leaf.ts:44,49,62,140`) — **the
  megamorphic IC of the engine** (many closure shapes through one indirect site). The arity unroll's
  real temporal value is NOT loop removal — it is that a **per-site unrolled closure gives each
  captured `p0.parser`/`p1.parser`/`p2.parser` call its OWN inline-cache slot** →
  monomorphic-per-closure → inlinable; the shared general arm `parsers[i].parser(state)`
  (`leaf.ts:62,261`) is ONE call site seeing every parser shape program-wide → megamorphic → never
  inlined. **Confirmed at Node v26 default tier** via `--trace-turbo-inlining` on a scratch probe
  (leaf `parser` SFI repeatedly inlined into the fused-closure SFI; per-site feedback vectors
  distinct) — but NOT yet on real parse-that leaves at the shipping tier with legs separated (the
  Brief-1 gap).

**The honest scoping (prior-art §1/§5, binding).** 8A's MAXIMAL claim ("monomorphic-by-construction
hot path") is **REFUTED** by the structural megamorphic-IC ceiling: the closure-per-combinator
pattern hits V8's megamorphic threshold at alternation (5.9% CPU, no inlining, measured). The only
measured cures are the **tagged-union/VM (LOCK-4C)** or the **build-time combinator emitter
(LOCK-4B)** — both LOCK-4 territory, never double-homed here. 8A therefore scopes to (a) the
allocation/hidden-class STRUCTURAL claim (achievable, banked) + (b) the per-site-IC TEMPORAL unroll
win (tier-conditional; a cold one-shot `.parse()` collects only the structural leg), and routes the
IC ceiling to LOCK-4.

**Mechanism (the live within-8A move).** The arity histogram over the ACTUAL live corpus
(value.js-master `src/parsing` + latex; research-A §2) shows `all` unrolls arity-2 (16) + arity-3
(24) = 40/52 fixed sites (77%), justified; but **`any` unrolls only arity-2 (`leaf.ts:39-57`) while
arity-3 (22 sites, 23%) hits the general `for` arm (`leaf.ts:58-73`) — an unjustified 2-vs-3
asymmetry with `all`.** The candidate: an `any` arity-3 unrolled arm symmetric with `fuseAll`. The
big `any` alternations (arity 16/23/48) + the 19-key `dispatch` are **LUT candidates, not unroll
candidates** — they belong on `dispatch`'s `Int8Array(128)` first-char table (`leaf.ts:100-151`,
O(1)) above a ~3-way unroll ceiling. The 18 spread-form `all(...)`/`any(...)` sites are statically
un-specializable at the combinator layer — only a build-time emitter that sees the spread's origin
reaches them (= LOCK-4B).

**center-B (the arbitrary-arity build-time specializer) — relationship to XL-D.** The runtime
arity-N specializer and the XL-D build-time TS face are **two different mechanisms today**
(research-A §3): the specializer is a *construction-time closure specializer* (runs per
`new Parser(...)`, emits no source, invoked through the megamorphic IC); XL-D is the *offset-tape
decoder* (imports no combinators, `JSON.parse` oracle) — complementary, not gating. **The
constructible bridge that generalizes the unroll to arbitrary arity, resolves the 18 spread-form
sites, AND is the ONLY route that attacks the megamorphic `.parser` IC itself (AOT-fused function,
leaf calls inlined, no `.parser` indirection) is the LOCK-4B COMPTIME-FUSION TS face.** Center-B IS
that emitter — homed at LOCK-4B, which its own REGISTRY evidence-banked ("terminal-role
BANKED→compile-backend," "no V8 analog (T1)," "isomorphism half 0% built"). **Pass-3 disposition:
center-B moves from pass-2's DOUBLE-block to a SINGLE-block** — its Y.13 consumer block is LIFTED by
8D's KEEP-THE-ALGEBRA verdict (first-party JSON/CSS bench grammars are the corpus of record); the
LOCK-4B mechanism block REMAINS. Routed to GESTALT-P as the shared arity-emitter-AND-IC-ceiling-fix.

**Integration seams.** `any` arity-2 `leaf.ts:39-57`, general `:58-73`; `fuseAll` arity-2
`:183-212`, arity-3 `:214-251`, general `:255-272`; `all` entry `:154-164` (arity-1 bypass `:161`).
Megamorphic IC (the ceiling): `.parser(state)` `parser.ts:53,82,88,131,137`, `leaf.ts:44,49,62,140`.
Shape substrate: `state.ts:36-53,43`, `unsafeSetValue`/`unsafeCall` `state.ts:87-99`. The one
prototype-chain deopt risk: `mapState`'s `Object.create(state)` per-invocation `parser.ts:177-180`
(the sole non-flat shape — probe, do not assume benign). `dispatch` LUT `leaf.ts:100-151`; `string`
len-1 `leaf.ts:282-294`; `regex` sticky `leaf.ts:344-358`; first-party bench harnesses
`test/benchmarks/pt-b3-fusion.bench.ts`, `json-comprehensive.bench.ts`.

**Gates it carries (born-RED).** (1) **Two-leg per-arity trace at Node v26 DEFAULT (Maglev) tier**
on real parse-that leaves: STRUCTURAL leg (alloc-B/op + `%HaveSameMap` shape stability,
tier-independent) and TEMPORAL leg (leaf `parser` SFI inlined-or-not per site + `--trace-opt-status`
tier residency) reported SEPARATELY, HEAD named (Brief 1). (2) `any` arity-3 unroll beats the
general arm at arity-3 (22 live sites), cold-paired, no arity-2/≥4 regression. (3) `%HaveSameMap`
TRUE for diagnostics-off vs diagnostics-populated `ParserState`; `mapState`'s `Object.create` view
probed for deopt. (4) Every 8A perf table reports structural + temporal separately with tier
residency (a single ratio is uninterpretable — R-B, the exact trap that halved the dispatch claim).

**Open questions.** (i) Does the temporal leg actually inline at each arity at the SHIPPING tier
(Maglev), not just the scratch-probe tier? — Brief 1, zero measured evidence for 2 passes. (ii) Does
the `any` arity-3 unroll pay? (iii) Is `mapState`'s `Object.create` benign under real access
patterns? (iv) **mapState-front parsimony:** the `ReadonlyState<T>` "ADVANCE front" exercises a
combinator with 0 external consumers (`mapState` grep-CONFIRMED 0/0/0); it survives Y.13 pruning
ONLY if a first-party test exercises it (unverified — see the incompatibility map). It is NOT banked
ADVANCE. (v) Is `CEILING` (read from the surviving arity histogram) even worth reading if H1 voids
the external corpus? (vi) center-B's LOCK-4B mechanism block — a GESTALT-P routing, not a within-8A
build.

---

## FAMILY 8B — TYPE-LEVEL TOTALITY (the type plane) — ADVANCED

**Architecture.** The TS checker carries the grammar: a homomorphic typed combinator algebra with no
`Parser<any>` at any PUBLIC boundary, erasures either justified as lawful variance or removed,
compile-time only (erased before emission — the zero-runtime-cost mirror). The plane is a **bounded,
tractable** job (prior-art §5.3): enumerated leak sites, `infer`+DRY, no open-ended type-theory.

**Key correction to the standing brief (research-B §0, decisive).** `ExtractValue` is **already
tight** — a homomorphic mapped type `{[K in keyof T]: T[K] extends Parser<infer V> ? V : never}` over
`T extends ReadonlyArray<Parser<unknown>>` (`parser.ts:9-11`), preserving tuple arity + labels. **The
`Parser<any>` leaks are NOT in the mapped type — they are in the ENTRY GENERIC BOUNDS**
(`any`/`all`/`fuseAll` all `<T extends Array<Parser<any>>>` at `leaf.ts:28,154,179`) + the
`ParserFunction`/`restore` variance points. The mapped type is the MODEL the bounds are rewritten
toward, not the defect. The pass-2/PT-E2 spec framing that located the leak in `ExtractValue` is
**re-cut**: name the bounds. (A DRY defect remains: a byte-identical duplicate `ExtractValue` at
`leaf.ts:155-157` — consolidate to one exported `types.ts` utility.)

**Mechanism (concrete signatures).** One shared `src/parse/types.ts` exporting the single
`ExtractValue`; then the entry bounds move to the **`const`-tuple form** (TS 5.0 `const` type
parameters; pkg pins `typescript ^5.8`):
```ts
export function all<const T extends ReadonlyArray<Parser<unknown>>>(...parsers: T): Parser<ExtractValue<T>>
export function any<const T extends ReadonlyArray<Parser<unknown>>>(...parsers: T): Parser<T[number] extends Parser<infer V> ? V : never>
function   fuseAll<const T extends ReadonlyArray<Parser<unknown>>>(parsers: T): ParserFunction<ExtractValue<T>>
```
`const T` is the single enabling feature (freezes the literal tuple so the homomorphic map indexes
positionally; without it rest-arg inference widens `all(a,b)` → `Parser<Vx[]>`). It is compile-time
only, erased. **`ParserFunction<T>` STAYS `ParserState<any>`** at the implementation boundary — the
deliberate variance seam through which every closure shape flows one indirect call (R4 §6c); it is
documented as an implementation-boundary `any`, NOT counted as a public leak (prior-art R-E:
over-tightening it re-monomorphizes the IC and perturbs 8A's pinned closures). `restore`
(`state.ts:79`) retypes `value: any → T`.

**GRAFT A resolved — the `as unknown as` in `all` (8B-G-ASUNKNOWN, the pass-2 flagship
thesis-violation).** research-B §1 partitions the erasures:
- **(a) Lawful variance — DOCUMENT and keep.** `many`/`sepBy` (`parser.ts:556,629`), `trim`
  (`parser.ts:481,520`), `state.ok/err/from` (`state.ts:59-72`), and the `fuseAll(parsers as unknown
  as …)` PARAM cast are value-reuse/value-discard variance on a single mutable state threading a
  heterogeneous value column — runtime-sound, no fresh object, no field transition. Annotate each
  `// variance:` and leave. This is the same discipline that documents `ParserFunction<T>`.
- **(b) The GENUINE leak — the arity-1 fold, which the `as` cast CONCEALS.** `all()` special-cases
  `parsers.length === 1 ? parsers[0].parser : fuseAll(...)` (`leaf.ts:160-163`): at arity-1 the
  runtime returns the inner parser unchanged (yields bare `V`) while the declared type is
  `ExtractValue<[Parser<V>]> = [V]`. `all(string("a"))` is typed `Parser<[string]>` and delivers
  `"a"`. **Cure (ii) — type arity-1 as `Parser<V>` to match the runtime** (a TYPE change, byte-empty
  dist/). Rejected: cure (i) (build a 1-array) is a RUNTIME change that breaks byte-identity and is a
  separate semver-visible item. Cure (ii) is chosen precisely so 8B's tightening remains pure erasure.

**`skip`/`sepBy` input leaks (8B-G3-RESIDUE).** Both discard the passed parser's value yet constrain
its type against `T`. Tighten `skip<S>(parser: Parser<T|S>)` → `skip(parser: Parser<unknown>):
Parser<T>` and `sepBy<S>(sep: Parser<S|T>, …)` → `sepBy(sep: Parser<unknown>, …): Parser<T[]>` —
input in a discarding position is contravariantly `unknown`; delete the phantom `S`. `next<S>` is the
dual and is already correct — leave. **TS2589 risk: none** — deleting a generic + widening an input
strictly REDUCES the instantiation graph.

**TS2589 discipline (the SOTA negative lesson).** The algebra is NON-RECURSIVE (single-level map,
O(N) flat in arity) — structurally far under TS's ~50-depth / ~5M-instantiation budget. PROTO-1
measured +124 instantiations (+2.9%), 0 new TS2589, precision both ways. **The one trap: do NOT fold
nested `.then` 2-tuples type-side** (that IS the recursive form that trips TS2589); mirror the
flattened runtime array.

**Integration seams.** `parser.ts:9-11` (canonical `ExtractValue` → move to `types.ts`), `:14-16`
(`ParserFunction`, keep-and-document), `:189` (`skip`), `:569` (`sepBy`), `:212` (`next`, correct);
`leaf.ts:28,154,179` (entry bounds → `const`-tuple), `:155-157` (duplicate `ExtractValue`, delete),
`:160-163` (arity-1 fold, cure ii); `state.ts:79` (`restore`).

**Gates it carries (born-RED).** (1) Tightened algebra compiles + infers on `test/types/
inference.test-d.ts`: `all(string("a"), regex(/\d/))` ⟹ `Parser<[string,string]>`; `any(...)` ⟹
union; `@ts-expect-error` fires on a wrong shape; `skip`/`sepBy` accept any element type. (2) **No
new TS2589** — `tsc --noEmit` clean on the type-test project AND on value.js-master's ~61 sites with
the candidate 1.1.0 tarball. (3) **BYTE-IDENTICAL `.js`** — the two-step on the REAL vite+vite-plugin-
dts toolchain (NOT an esbuild proxy): (i) version-migration delta recorded/attributed, (ii)
tightening delta **byte-empty** on `dist/*.{js,cjs}` under cure (ii). `.d.ts` MUST change (that is
the deliverable — fence the diff to `.js`, R-D). *This is the clause-v gate that has NEVER fired.* (4)
Instantiation budget: `--extendedDiagnostics Instantiations` delta baseline-vs-tightened, disclosed
with tsc version + exact signatures + corpus manifest; NATIVE leg on value.js-master @ 3.1.0 (H2, pin
master) + self-use leg always available; assert linear-in-call-sites, not superlinear.

**Open questions.** (i) The byte-empty step-ii on the real toolchain has never run (the zero-runtime-
cost invariant — a non-empty `.js` delta halts). (ii) The native budget number is runnable only iff
H2 holds AND value.js-master will adopt 1.1.0 (P2); else record un-runnable, take it on self-use
under the exact tightened signature. (iii) Does `const T` redden value.js-master's `all(...someArray)`
spread sites (R-B, caught only by full `tsc`, never unit tests)? (iv) result-union (`Parser<T,E>`
labels) stays BANKED (keyframes' demand evaporated via divorced value.js; reopen only on a live named
consumer).

---

## FAMILY 8C — THE ISOMORPHIC MIRROR (the correspondence plane) — ADVANCED

**Architecture.** A **machine-generated divergence ledger** — one checked-in, CI-diffable row per
combinator/leaf/subsystem across the Rust and TS parse-that surfaces, each classified
`MIRRORED` / `LAWFUL-NON-MIRROR(reason)` / `DRIFT` — and, riding on it, the **two-engine regex
reconciliation**. 8C is a *diff artifact*, not a rewrite: it never edits parse-that; it emits a
ledger, a paired-triple corpus, and a scratch priority-DFA prototype (all evidence the isomorphism
claim is measured, not asserted). After the pass-1 cession, 8C's SOLE residue is the ledger; the
linter is LOCK-6A, the single-spec→two-artifact generator LOCK-6B, the behavioral differential oracle
LOCK-6D. **WASM-CORE for the combinator hot path is REFUTED** (prior-art §3: OpenUI 2.2–4.6× faster
in pure TS; PROVE-14 full-walk 0.91×) — the surviving isomorphism forms are 6A mirror-tree + 6D
oracle; 8C consumes that verdict and claims no shared automata engine for the hot path.

**Mechanism — the ledger (P1 tool).** An AST/grep census over `rust/parse_that/src` (`pub fn`/method
names) and `typescript/src/parse` (class methods + `export function`), joined on a name-map, emitting
a TSV with auto-classification: both-sides → MIRRORED-candidate (human confirms semantic parity);
one-side → DRIFT until a reason promotes it to LNM; whitelisted → LNM. **DRIFT is a lint failure**
(mirror the surface or write the reason). Regen-only: a hand-authored ledger drifts and is a
workaround (R5); the §2-seed table (research-C, ~40 rows hand-verified) is the SEED, the shipped
artifact is regenerated and CI-diffed against a checked-in copy. **Rust carries TWO combinator
engines (closure-`Parser` + `SpanParser`/`span_parser/`, 10 files, no V8 analogue); TS carries ONE**
(the `*Span` family excised at 1.0.0, guarded dead by `proof:no-span-surface`). The `SpanParser`
engine is the largest single DRIFT → `LNM(rust-perf-engine)` as a **NAMED supersession** (Constitution
forbids silent mirror-contradiction), not left silently divergent (R6). Each ledger row also carries
a **FIRST-PARTY-ONLY** flag fed by 8D's tier adjudication (§8D) so the gestalt sees which surfaces are
not externally load-bearing.

**Mechanism — the regex row (the one DRIFT that is a semantic ENGINE difference, not layout).** TS
drives V8 Irregexp (backtracking, Perl **leftmost-first**) via a sticky `/y` clone at offset
(`leaf.ts:344`); Rust drives bbnf-regex (HIR→Thompson-NFA→subset-DFA→Hopcroft-min), anchored via
`Dfa::find_at`, recording `last_accept` on every accepting position = **leftmost-longest (POSIX)**
(`dfa.rs:116,122-138`). The two engines disagree by construction on order-sensitive and non-regular
fragments. **PROTO-2 (pass-2) resolved the classification = `MIRRORED-WITH-GUARD`** (not unconditional
lawful-non-mirror): the 10 real parse-that leaves agree byte-for-byte (the two `|` sites have disjoint
first bytes → longest ≡ first), 6 synthetic triples diverge; and found a SECOND hazard class
(lazy-quantifier → greedy, `\d+?` on `123` → Rust 3 / JS 1). The FULL hazard enumeration (research-C
§3a) is H1 alternation-order, H2 greedy-extent, H3 lazy/non-greedy, H4 backreferences (non-regular,
HIR rejects), H5 lookaround (non-regular), H6 capture-submatch, H7 anchors/flags, H8 Unicode, H9
empty-match, H10 catastrophic backtracking (V8 exponential vs bbnf linear — *the reason the two
engines coexist*), H11 anchoring-mode. Many (H2,H7,H9,H11) may be LATENT-agreeing on the anchored-
scanner usage parse-that actually exercises — measure before building (R3/R4).

**The LOCK-5 engine defect (routed as a DECISION; the pass-2 collision RE-CLASSIFIED).** PROTO-2 read
the confirmed defect from source: the NFA carries priority (`EpsilonEdge{priority}`, nfa.rs:23,313 —
leftmost-first intent) but `epsilon_closure`/`move_on_byte` compute over `BTreeSet<StateId>`
(dfa.rs:189,205) — **priority is discarded at determinization**; the DFA then does leftmost-longest.
The NFA's leftmost-first intent never reaches the matcher; the engine is internally self-
contradictory. **Two routes, and pass-3 finds them COMPLEMENTARY, not competing** (research-C §3b —
this DISSOLVES the pass-2 `8C-guard ⟂ LOCK-5-fix` collision):
- **Route A — priority-preserving subset construction** (RE2 / regex-automata
  `MatchKind::LeftmostFirst`): replace the set-based ε-closure with a priority-ordered thread-list
  closure keyed on the ordered sequence; the DFA records its highest-priority accepting member; `find`
  prunes threads reachable only after a higher-priority accept. **Dissolves the REGULAR fragment
  {H1,H2,H3}** — engines agree.
- **Route B — build-time classify-gate** (`classify.rs:55,157` extended to reject H4 backrefs, H5
  lookaround at grammar-compile with a span-named diagnostic). Handles the **non-regular residue no
  DFA can mirror**. B alone cannot fix H1 without forbidding overlapping alternations (draconian).
- **Synthesis: A + B complementary.** Route A closes {H1,H2,H3}; Route B rejects {H4,H5}; H6 is a
  separate output-contract decision (Laurikari TDFA iff capture parity is contracted, else document
  `regex()` as span-yielding). **The engine question ships to LOCK-5** (bbnf-regex is its home, XL-C);
  8C supplies the discriminating corpus + the scratch prototype (P3) as the decision's evidence. The
  guard does NOT foreclose the fix — they cover disjoint fragments.

**Integration seams.** `dfa.rs:116,122-138,189,205` (determinization discarding priority), `:294+`
(Hopcroft — the R2 hazard: minimizes by Myhill-Nerode equivalence, not a leftmost-first-preserving
relation — may re-erase priority); `nfa.rs:23,313,326-433` (priority-carrying NFA + greedy/lazy
fragments); `classify.rs:55,157` (the Route-B gate); TS regex `leaf.ts:317,344` (native `RegExp`
sticky). Ledger census surfaces: `combinators/methods/*`, `leaf.rs`, `span_parser/` vs `parser.ts`,
`leaf.ts`.

**Gates it carries (born-RED).** (1) The ledger is GENERATED (P1 tool) and CI-diffed against a
checked-in copy with **zero unexplained DRIFT rows** — the un-built deliverable, 8C's whole residue.
(2) The paired-triple corpus is tagged H1–H11, run on the CURRENT engines to establish the MEASURED
divergence set (which classes are latent-agreeing on anchored usage); the 6D corpus carries BOTH
confirmed hazard classes (alternation-order AND lazy-quantifier — Gate (3) as written pre-PROTO-2
would have GREENed a corpus hiding the lazy divergence). (3) The regex row is classified
`MIRRORED-WITH-GUARD` by measured evidence, not assumed. (4) The ledger↔6A whitelist handshake is
wired (every LNM row a whitelisted exception with its reason string; single source of truth = the
ledger TSV, the linter reads it). (5) Decode-speed numbers FENCE their measured path — pure-JS-decode
(stop-early **4.78×**, CI [4.20,5.37]; full-walk 0.98× parity) vs wasm-boundary (UNMEASURED, must
never migrate onto a wasm claim, SPEC §0 shim law). (6) The priority-DFA scratch (P3) reports, on the
real 50-pattern bench corpus: (a) does it flip H1–H3 to JS-parity, (b) DFA state-count delta (R1
blowup risk — priority-keyed determinization can be exponentially larger), (c) does Hopcroft still
hold (R2).

**Open questions.** (i) **Is Route A needed AT ALL, or is the divergence latent-only for the live
corpus?** — the P4 consumer-regex census (value.js-master + latex grammar regex literals) decides;
if the corpus never exercises H1/H4/H5, Route B alone suffices and Route A is retired (Brief 2). (ii)
Does priority-keyed determinization blow up state-count (R1) and survive Hopcroft (R2)? (iii) `SpanParser`
disposition — LNM(rust-perf-engine) named supersession vs mirror obligation (a ledger decision, GESTALT-P).
(iv) **8C cannot converge to 100% independent of LOCK-6** (regen gate ceded to 6B, parity oracle to
6D) — its ceiling is LOCK-6-bounded (flag for GESTALT-P). (v) tape-crossing artifact home
(`crates/bbnf-bench/ts-tape/` vs LOCK-6B `ts/tape/`) — adjudicate with LOCK-6; out of scope for the
combinator ledger (it is a generated skeleton, 6B).

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST (the API plane) — ADVANCED

**Architecture.** The TS backbone designed backward from its actual LIVE consumers: their usage
patterns define the retained API, perf gates ride their suites, semver + migration are first-class
deliverables. The pass-3 re-census (§0) rebuilds the family on the MEASURED graph, not the assumed
one: **one certain external consumer (latex-paper) + one contingent (value.js-master under H2)**;
keyframes struck.

**Mechanism — 8D-G-Y13 ADJUDICATED (the pass-2 central decision that forks 3 families; RESOLVED).**
The question: for combinators with no external consumer, is the first-party test/bench suite a
legitimate consumer (Y.13 satisfied), or should the surface prune to the live external contract?
**The governing distinction (research-D §2): parse-that is a PUBLISHED STANDALONE LIBRARY, not an
app-internal module.** For an app-internal module Y.13 bites hard (dead surface is deletion-bait); for
a published combinator algebra the PRODUCT *is* the algebra, and its own conformance + bench suite (13
`test/*.test.ts` + 4 `test/benchmarks/*.bench.ts`) is a **legitimate first-party consumer** — the
latent external installer the suite stands in for. Pruning to latex's 5-symbol slice would gut the
library to a regex+string+alternation front and destroy its reason to exist. **VERDICT:
KEEP-THE-ALGEBRA (route a) + DROP only the diagnostics module-globals the scope rework already
deletes.** "Prune to the live external contract" is REJECTED — the right rule for an app-internal
module, the wrong rule for a published library. Tiering:
- **TIER-A — KEEP (hard external floor, both hypotheses):** `Parser` (+ methods `map skip trim then
  next opt chain not`), `ParserState`, `regex`, `string`, `any`. Signature changes here ride latex's
  `vue-tsc` gate. Under H2 add `all`, `dispatch`, `whitespace`, `mergeErrorState` (value.js-master).
- **TIER-B — KEEP as library algebra (first-party-legitimate), LABEL FIRST-PARTY-ONLY in the 8C
  ledger:** `.or .many .sepBy .wrap .peek .lookAhead .minus .negate .mapState .recover .eof`, the
  `lazy`/packrat families, span/context helpers — each exercised by a first-party test.
- **TIER-C — DROP:** the module-global diagnostics quintet (`enableDiagnostics …`) that the per-parse
  scope rework deletes (a real surface change, semver-visible). `jsonParser`/csv/`splitBalanced`:
  RETAIN-BUT-LABEL, decide at the gestalt with the owner, not unilaterally.

**Consequence for the coupled family fates:** 8D route (a) LIFTS the Y.13 block on 8A center-B (the
first-party bench grammars are its corpus of record) — center-B's remaining block is LOCK-4B
mechanism only. For 8A's **mapState front**: mapState is Tier-B, kept IFF a first-party test exercises
it — but its consumer count is grep-CONFIRMED 0/0/0 externally and a first-party test is UNVERIFIED;
if none exists, mapState prunes even under route (a) and the `ReadonlyState` ADVANCE front is
discarded. The `8A-mapState-front ⟂ Y.13-prune` collision therefore NARROWS to a single verification
(does any `test/*.test.ts` call `mapState`?), not a design conflict.

**Mechanism — per-parse diagnostic enablement (GRAFT B; refines pass-2's `state.diag` boolean).** The
defect: enablement is a module-global mutable flag `diagnosticsEnabled` (`utils.ts:6`) feeding a
module-global buffer (`utils.ts:138-146`) — nested/interleaved parses cross-contaminate.
**Recommended mechanism (c) — a shape-stable NULLABLE per-parse scope field, read lazily**
(research-D §4): enablement is a PARSE OPTION (`.parse(src, {diagnostics:true})` / `withDiagnostics(fn)`),
never a module-global write; a `DiagnosticScope` opens/closes on the parse epoch (the `parser.ts:34-49`
try/finally, the same seam that brackets `packratEnter`/`packratExit`) and threads onto the per-parse
`ParserState` (already constructed per-parse in `parseStateInner`). The scope is NULL on the disabled
path; the lazy `suggestions`/`secondarySpans` fields populate only under `scope !== null &&
scope.enabled`. This carries the buffer PER-PARSE (fixing the module-global cross-contamination the
pass-2 boolean latch did not address) at the same disabled-path cost. **Disabled-path cost: ZERO alloc
(a null field read, no object) + `%HaveSameMap` TRUE** (undefined-vs-fresh-array at the same map). The
one legitimate allocation is a single scope object per TOP-LEVEL `.parse()` when ON — never per inner
combinator-parse (the R2 hazard).

**Mechanism — the migration crater (re-cut two-leg).** latex's `^0.7.1` pin PREDATES the 1.0.0
breaking cut (`*Span` excision S.H2, `chain()` C-16), so a "1.0→1.1 minor step alone" is not directly
runnable for latex. Two legs for attribution isolation: **Leg A (0.7→1.0, MAJOR)** — install a packed
1.0.0 tarball into a latex scratch, `vitest run` + `vue-tsc -p tsconfig.vue.json`; expected GREEN
(latex touches only stable symbols); any RED is a 1.0.0 break, labeled as such. **Leg B (1.0→1.1,
MINOR, the actual PT-3 delta)** — from Leg A's green tree, install the 1.1.0 candidate, re-run both.
value.js crater is hypothesis-dependent: H1 no crater; H2 a clean single minor leg (master already at
`^1.0.0`). keyframes MOOT. **The re-cut close gate (named supersession of PT-3 W5's "three consumer
suites green"): latex Leg-B green + Leg-A attribution recorded + value.js-master green IFF H2, keyframes
struck.**

**Integration seams.** `utils.ts:6` (module-global flag → per-parse scope field), `:138-146`
(module-global buffer → per-parse), `parser.ts:34-49` (the epoch try/finally seam),
`parseStateInner`/`parser.ts:56-65` (the already-per-parse state carrying `furthest`+`getLineAndColumn`
always-on), `state.ts:36-53` (lazy fields), `leaf.ts:28,154` (the `any`/`all` bounds latex's `vue-tsc`
gate covers).

**Gates it carries (born-RED).** (1) Two-leg latex crater GREEN (Leg-B) + Leg-A attribution — baseline
latex GREEN at 0.7.1 first or the sole positive gate is uninterpretable. (2) The `any()`-tightening
reddening test: tightened `leaf.ts:28`/`154` on a scratch 1.1.0 tarball, `vue-tsc -p
tsconfig.vue.json` over latex's 3 `any()` sites, diagnostic-set diff vs baseline — expected CLEAN (a
precise type is a superset for well-typed sites); a new TS2589 or rejected `any()` is a real semver
signal minor→major, decided by measurement (caught only by `vue-tsc`, never `vitest`). (3) `%HaveSameMap`
TRUE diagnostics-off vs -populated; off-path paired-cold harness holds the landed floors (then-chain
≥ +11%, fused ≥ +13%; dispatch carried at the HONEST ~+5% no-regression lane, NOT a ≥10% gate). (4)
Empty semantic `dist/` `.js` diff (shared with 8B-G4'). (5) The dual-instance hazard is DOCUMENTED
not-present (no repo nests two parse-that instances in the live graph — value.js-master 1.0.0 and
latex 0.7.1 are separate installs) with a gestalt watch-note.

**Open questions.** (i) **P2 — is latex live-forward or a fossil?** (2 weeks idle, 2 API cuts behind) —
the deepest cut; the entire 8D construction targets a possibly-frozen repo, and its `vue-tsc` gate
tests a migration latex may never run. A born-verified precondition, symmetric to P1. (ii) P1 — does
tranche-u land (H1, value.js exits) or does master hold (H2)? (iii) Does the first-party suite actually
exercise every Tier-B combinator (the mapState verification above)? (iv) `jsonParser`/csv/`splitBalanced`
prune-vs-label — a gestalt+owner decision.

---

## THE INCOMPATIBILITY MAP (pass-3 — one collision DISSOLVED, one NARROWED, one block HALVED)

| pair | pass-2 disposition | pass-3 disposition | precise reason |
|---|---|---|---|
| **8A ⟂ 8D** (StateView drops the field triple) | EVAPORATED | **EVAPORATED (holds)** | `mapState` 0 consumers; the 8D field triple is read on the RETURNED failed state (`parseStateInner`), never through `mapState`. |
| **8A ⟂ 8B** (`as unknown as` unsound cast) | RE-RESOLVED (ReadonlyState) | **NON-CONFLICT (refined)** | 8B's `all` cast is (a) a lawful `fuseAll` param variance (document) + (b) the arity-1 fold leak cured by type-only cure (ii). No unsound cast survives; GRAFT A discharged. |
| **8B ⟂ 8D** (result-union vs field-triple) | ADJUDICATED (field-triple) | **ADJUDICATED (holds)** | latex reads neither `.expected` nor a result-union (import oracle); result-union is Y.13-thin additive. |
| **8C ⟂ 8D** (below-combinator seams) | SHRUNK to 5-symbol surface | **INTERLOCKED** | 8D's KEEP-THE-ALGEBRA verdict restores the full algebra to the mirror; each Tier-B row is labeled FIRST-PARTY-ONLY in the 8C ledger. The two families now SHARE the tiering, not collide. |
| **8A-mapState-front ⟂ 8D-G-Y13-prune** | ADD (CRIT-2) | **NARROWED to one verification** | Y.13 resolved to KEEP-THE-ALGEBRA; mapState survives IFF a first-party test exercises it (unverified). Not a design conflict — a census question. The `ReadonlyState` front is contingent, NOT banked ADVANCE. |
| **8A center-B ⟂ Y.13 ∧ LOCK-4B** | DOUBLE-block | **HALVED to LOCK-4B mechanism only** | Y.13 block LIFTED (8D route a: first-party bench grammars are center-B's corpus). LOCK-4B mechanism block REMAINS (4B banked its TS face, "no V8 analog, isomorphism half 0% built"). Routed to GESTALT-P. |
| **8C regex-guard ⟂ LOCK-5-fix** | ROUTE to LOCK-5 (foreclosure feared) | **DISSOLVED — COMPLEMENTARY** | Route A (priority-DFA) closes the REGULAR fragment {H1,H2,H3}; Route B (classify-gate) rejects the NON-REGULAR residue {H4,H5} no DFA can mirror. Disjoint fragments; the guard does not foreclose the fix. LOCK-5 decides Route A's necessity, gated on the P4 consumer census (may be latent-only → Route A retired). |
| **8A ⟂ 8C-ambitious** (runtime TS dispatch) | BANKED (−14% law) | **UNCHANGED — BANKED** | Any runtime tag/switch on V8 is dead; the only door is the build-time emitter (= LOCK-4B). |

---

## BLOCKED-CANDIDATE ADJUDICATION

**No family is BLOCKED-candidate.** Every central family mechanism is constructible on current
substrate: 8A's structural leg is banked (PROVE-11) and its temporal per-site unroll + `any` arity-3
gap are live within-8A moves; 8B's tightened algebra RAN (PROTO-1) and the residue is bounded; 8C's
ledger + regex corpus are pure diff artifacts; 8D's crater + scope mechanism are constructible now.
What the (partial) consumer picture removes under H1 is CONSUMERS and banked NUMBERS, not primitives —
and pass-3's §0 correction shows master retains a live corpus under H2.

**One sub-deliverable stays blocked (not a family): 8A center-B (the arbitrary-arity build-time
combinator emitter).** Pass-3 HALVES it from double-block to single-block — Y.13 lifted, LOCK-4B
mechanism block remaining. It is NOT a BLOCKED-candidate by the "missing primitive equal in difficulty
to the original problem" test: the family 8A is fully constructible without it; center-B is the
LOCK-4B-homed ceiling-breaker (the only route past the megamorphic `.parser` IC), routed to GESTALT-P,
not a within-lock stall. 8A's MAXIMAL claim (monomorphic-by-construction hot path) is ceiling-refuted
and honestly scoped down, not blocked.

---

## THE TWO SHARPEST UNCERTAINTIES (prototype briefs) — see StructuredOutput

Both are parse-that code probes (pt-scratch) on scratch copies, base-pinned, HEAD-named. Brief 1
strikes 8A's least-understood, zero-measured-evidence-for-2-passes temporal thesis at the SHIPPING
tier. Brief 2 strikes the 8C/LOCK-5 fork whose resolution most advances GESTALT-P: whether the
priority-DFA engine investment (Route A) is warranted AT ALL, or the divergence is latent-only for the
live consumer corpus (Route B alone).
