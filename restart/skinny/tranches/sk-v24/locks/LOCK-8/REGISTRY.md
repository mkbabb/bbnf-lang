# LOCK-8 — THE TS BACKBONE PLANE (parse-that/typescript) — CONVERGENCE REGISTRY

*Agglomerator record, Fable-tier judgment. Four families, four orthogonal planes over one
closure core: 8A runtime (V8), 8B type, 8C correspondence, 8D API. parse-that/typescript is
READ-ONLY to the bbnf campaign; every family lands only via letter BBNF-PT-G (owner or a
parse-that-side session); prototypes run on scratch copies (PROVE-11 recipe). Two engines
govern and must never be conflated: the V8 JIT (8A) and the TS checker (8B).*

**Convergence discipline.** Percentages are the **critics' floor** unless a named reason to
override is recorded. Where a nudge above the floor is taken, the reason is stated inline. No
family is near converged; this is pass 1.

---

## CURRENT STANDING (as of pass 1)

| family | plane | status | convergence | one-line state |
|---|---|---|---|---|
| **8A** Shape-Stable Core | V8 runtime | **ADVANCED** | **58%** | Pivot RETIRED by real measurement (constant-capture inlines; mapState deopt real); the one new source change (StateView) carries a correctness bug + an unsound cast; the terminal specializer is unbuilt & double-homed (LOCK-4B). |
| **8B** Type-Level Totality | type | **ADVANCED** | **50%** | Literal-precision subset MEASURED & green; but the proto shipped a **weaker** signature than the spec (no `DropUndefined`), the hardest crux (opt-in-`all`) is untested, and the headline "no `Parser<any>`" is untouched. |
| **8C** Isomorphic Mirror | correspondence | **ADVANCED** | **45%** | Data plane rests on CONFIRMED substrate; but all three strata + linter are **double-homed with LOCK-6**, no proto ran, and the ambitious runtime-dispatch arm is refuted. Must re-scope to LOCK-6 consumer. |
| **8D** Consumer-Contract-First | API | **ADVANCED** | **50%** | Strongest recon (semver inversion, R-8D4 anti-false-green), but ran no proto, its make-or-break crater harness is **blocked on open-q (i)**, and PT-E1 is a contract without a mechanism. |
| *8C-ambitious* runtime TS dispatch | — | **BANKED** | 10% | Refuted-on-arrival by C1 (−14% measured, SpanParser KILL). Redundant with 8A center-B. Named negative result. |
| *8B-frontier* `Parser<T,E>` labels | — | **BANKED** | 15% | Pre-banked (Y.13): exponential type dimensions, thin consumer value, prime TS2589 offender. Reopen only on a named consumer. |

**No family is BLOCKED-candidate** (concur with both critics): every central mechanism's
primitives exist; the gaps are un-run measurements, an over-read verdict, a masked narrowing,
and deferred/double-homed adjudications — not missing primitives equal-in-difficulty.

---

## PER-FAMILY LEDGER

### 8A — THE SHAPE-STABLE CORE — ADVANCED, 58%

**Critics:** CRIT-1 72% / CRIT-2 55%. **Floor 55%; nudged to 58%.** Named override: CRIT-2
folds the cross-lock LOCK-4B specializer dependency as a *within-8A* penalty, but 8A's own
design correctly names it a dependency and 8A **retired its within-family pivot** with a real,
refutable VM-facts measurement (PROTO-1) — the strongest-evidenced family in the lock. Held
below CRIT-1's 72% because the **one genuinely new source change 8A ships (StateView) carries
a live correctness bug** both critics independently found.

**What converged (banked evidence, do not re-litigate):**
- Constant-capture inlining **fires**: the arity-3 arm inlines the SFI-heterogeneous child
  leaves (`stringParser`/`regexParser` → `fusedArity3Hetero`); the general loop arm does **not**
  (only `ok`/`unsafeSetValue`). PROTO-1 probe 2, `--trace-turbo-inlining`, `target TURBOFAN_JS`.
- The megamorphic `.parser` **LOAD is monomorphic** (all `Parser` share one map); only the
  **CALL** loses child inlining. No 10–50× hashtable cliff. Architectural fact.
- `mapState`'s `Object.create(state)` view is a **distinct hidden class per parse**
  (`%HaveSameMap=false`); a fixed-shape record collapses all views (`true`). The deopt is real.
- **Zero** steady-path deopts over 200k jsonParser + 200k CSS-triple. BLOCKED-SHADOW not made fact.

**Exact open gaps:**
- **8A-G1 (masked correctness — the StateView field set is WRONG).** The shipped
  `mapState-fixed-shape-view.patch` narrows to `{src,value,offset,isError,furthest}` and
  **omits `expected` and the `getLineAndColumn` method** — which 8D declares are part of the
  consumer-read field triple (`buildDiagnostic`). Any `.mapState` fn reading `.expected` or
  calling `.getLineAndColumn()` (now a `TypeError`; the method lived on `ParserState.prototype`,
  absent on `StateView`) **silently breaks**. The comment "carries exactly the fields a mapState
  fn reads" is unaudited; PROTO-1 measured only `%HaveSameMap`, never ran a consumer against the
  patched `mapState`. **This is a MISSED incompatibility-map entry (8A⟂8D).**
- **8A-G2 (unsound cast — 8A⟂8B is NOT the declared non-conflict).** The patch ends
  `fn(state, oldView as unknown as ParserState<T>)`. An `as unknown as` double cast is exactly
  the `Parser<any>`-grade hole 8B exists to eliminate. 8A's own fix violates 8B's soundness
  thesis; the FAMILY-SPECS claim "8A⟂8B: none, coexist by construction" is **refuted**.
- **8A-G3 (the terminal deliverable is unbuilt & double-homed).** 8A center-B's actual move —
  "make constant-capture a **build-time specializer**" generalizing the hand arity-3 arm to
  value.js's 61 `all()` sites — is **LOCK-4B's TS fuser** (`leaf.ts:154-273`→any arity), whose
  TS-execution number is **unrun** in its home lock. PROTO-1 proved the easy half (hand arms
  inline); the load-bearing generalization is neither built here nor converged in LOCK-4B.
- **8A-G4 (gate-4 mis-specified against its own proto).** Gate 4 reads "general arm does **not**
  inline" — but PROTO-1 proved the general arm **does** inline for SFI-**homogeneous** leaves.
  On a homogeneous corpus gate 4 false-greens. Needs the SFI-heterogeneity qualifier; the
  specializer's scope (only heterogeneous sites need unrolling) is not folded back into center-B.
- **8A-G5 (half-run: the temporal leg is missing).** Center-D declares "structural + temporal
  are the two legs; neither alone suffices." PROTO-1 ran only the structural leg. Throughput
  attributable to the inlining win (bounded ~+5%) is unmeasured; "8A PASS" is a structural
  verdict dressed as a whole one.
- **8A-G6 (named corpus not the corpus run).** Deopt sweep used **synthetic** CSS triples, not
  the real value.js corpus the gate names.
- **8A-G7 (Maglev untested).** PROTO-1 forced synchronous **TurboFan** (`--no-maglev`); Node 26's
  default production tier is **Maglev**, whose inlining policy differs and which most value.js
  code will actually reach.
- **8A-G8 (code-size budget unaddressed).** Unrolling every heterogeneous `all()` site × 61 sites
  is the exact generated-size regression the parsimony clause + generated-size-budget law guard.
  8A names no ceiling.
- **8A-G9 (standing carry, bounded).** `dispatch` (leaf.ts:134-145) de-megamorphization is the
  accepted residue / BLOCKED-SHADOW, correctly bounded to ~+5% no-regression, never a ≥10% gate.
  Not a defect.

---

### 8B — TYPE-LEVEL TOTALITY — ADVANCED, 50%

**Critics:** CRIT-1 58% / CRIT-2 50%. **Floor 50%; held.** No override: both critics
independently found the same over-read (proto shipped `Parser<Values<T>>`, spec mandates
`Parser<DropUndefined<Values<T>>>`) and the same un-run crux (opt-in-`all`). CRIT-1's 58% is
explicitly conditional ("defensible only if the registry records the PASS covers the
literal-precision subset, not the spec's signature") — that condition is now recorded, which
*lowers* the effective claim, not raises it.

**What converged (banked evidence):**
- The literal-precision subset is **real and measured**: `string<const S>` → `Parser<"a">`
  lifts `all(...)` to positional literal tuples; `.test-d.ts` GREEN on candidate / RED (3×
  TS2344 positive control) on 1.0.0. Not vacuous.
- On value.js's **real** 259-file parsing graph: **0 new TS2589**, error set **byte-identical**
  (comm empty both ways, all 21 errors pre-existing env), **1.042×** instantiations, ~1.0×
  check-time. `dist/` zero-cost proven by SHA-identical emit. The signature falsifier did not fire
  **for the thin slice**.
- `dispatch<const K>` correctly reverted to `Record<string,Parser<T>>` (generic const-K collapses
  `Object.entries` to `unknown`; buys the consumer nothing). Clean KISS retreat, do not re-litigate.

**Exact open gaps:**
- **8B-G1 (the PASS covers a WEAKER artifact than the spec — record verbatim).** Proto shipped
  `all → Parser<Values<T>>`; spec mandates `Parser<DropUndefined<Values<T>>>`. `DropUndefined` is
  defined in `types.ts` and **never wired into `all`** — consumer-less substrate inside the proto.
  All PASS metrics (1.042×, byte-identical errors, GREEN test-d) were measured on the **cheap**
  signature. The instantiation-budget gate — 8B's signature falsifier — was **not** put to the
  actual proposed type. `DropUndefined` is an extra tail-recursive conditional at every one of
  value.js's 61 `all()` sites; its budget is **unmeasured**.
- **8B-G2 (the hardest named uncertainty was swerved — R1 opt-in-`all`).** The family's own
  "hardest correctness problem" — `all(a, opt(b), c)` returns `[A,C]` **or** `[A,B,C]`, a
  length-variable runtime tuple the static type cannot know — was **not** exercised. Both
  `Values<T>` (proto) **and** `DropUndefined<Values<T>>` (spec) are **known-unsound** here:
  `Values` lies for b-absent, `DropUndefined` mistypes b-present. The three routes (type-level
  filter + document the one lawful widening / redefine `all` breaking / distinct `allOptional`)
  are unresolved. Deciding experiment (a **runtime differential** against value.js's real
  opt-in-`all` sites) **not run** — and it is unconfirmed value.js even has such a typed site.
- **8B-G3 (the headline is untouched).** "No `Parser<any>` anywhere" got **zero** of PT-E2's
  enumerated `Parser<any>` **input** leaks removed; the uplift is carried **entirely** by
  `string`'s `const S`. The full algebra (`then→[T,S]` dropping the unsound `S|T`, `or`, `chain`,
  `opt→T|undefined`) is unprototyped, each with its own TS2589/instantiation risk.
- **8B-G4 (latex budget corpus not natively run).** The budget gate names value.js **+
  latex-paper**; latex ran only as a bounded 1.0.0-family overlay (pin `^0.7.1`), not a native
  compile. One of two named budget corpora is unmeasured.
- **8B-G5 (proxy build).** Emit-identity proven via **esbuild** transpile of one file, not
  parse-that's actual `dist/` (tsc/bundler) built both ways. Reasonable proxy; named artifact
  not the artifact compared.
- **8B-G6 (result-union not actually adopted).** 8B "ADOPTs the result-union as *the* typed
  outcome," but the incompatibility map demotes it to a **secondary** `.withDiagnostics` surface
  behind 8D's field-triple and defers whether both live. 8B's stated diagnostics-typing mechanism
  is a pending adjudication, not a mechanism — and two coexisting surfaces strain parsimony.

---

### 8C — THE ISOMORPHIC MIRROR — ADVANCED (thin conservative residue), 45%

**Critics:** CRIT-1 55% / CRIT-2 45%. **Floor 45%; held.** Named reason CRIT-1 over-read:
CRIT-1 was within-lock weighted and **missed the LOCK-6 double-home** that the cross-lock read
(CRIT-2) surfaced across all three strata + the linter. The double-home is real and
load-bearing, so the disciplined floor stands.

**What converged (banked evidence):**
- Data-plane anchor is **prior-tranche CONFIRMED** (SPEC §0 row 14 / PROVE-14): tape SoA decodes
  byte-exact node-for-node in TS, stop-early 4.4–4.6× over `JSON.parse`, full-walk 0.91×
  (native won, measured). Legitimately cited, not SPEC_ONLY.
- The three structural divergences are correctly **classified, not "fixed"**: SpanParser has no
  TS twin (a naive twin deopts, R-B); regex is Rust-DFA-vs-native-RegExp **oracle-only,
  layout-exempt** (LOCK-5 owns); diagnostics are **behavioral mirror, not mechanism mirror**
  (forcing TS compile-time-erase kills the live surface keyframes consumes, R-D).

**Redirect + cession (see §REDIRECT below):**
- **8C-ambitious (generate a runtime TS dispatch mirroring SpanParser) → BANKED.**
  Refuted-on-arrival by C1 (−14% measured). Its only lawful door is a **build-time** specialized
  dispatcher (LOCK-4B), which is a different thing from a runtime VM. Redundant with 8A center-B.
- **8C Stratum-A (spec→Rust dumper + TS decoder), Stratum-C (behavioral oracle), and the
  isomorphism LINTER → CEDE to LOCK-6** (6B / 6D / 6A respectively). LOCK-6A explicitly claims
  the parse-that combinator core as its named case; LOCK-6B recasts GN-4.W4's tape port as its
  generated deliverable. 8C is written as **owner**; the portfolio intends it as **consumer**.

**Exact open gaps:**
- **8C-G1 (double-homed across all three strata + linter — the headline).** Re-scope to "the
  parse-that FACE contributing the three named divergences as EXEMPT rows," else GESTALT-P
  inherits a genuine double-home. Nameable now, not at gestalt.
- **8C-G2 (no proto; every novel mechanism is constructibility-asserted).** The single-spec→
  two-artifact generator, the regen-stability claim (regen∘regen byte-identical), and the linter
  are all SPEC_ONLY. PROVE-14 confirms the decoder *works*; it does **not** confirm a *generator
  can emit both sides regen-stably*.
- **8C-G3 (gate-3 is SPEC_ONLY leaning on PROTO-1 by analogy).** "Generated node-vocabulary
  dispatch holds the arity-unroll floor or is refuted" asserts *generated* code inlines like the
  *hand* replicas PROTO-1 tested. Same unbuilt mechanism as 8A-G3. Must be labeled SPEC_ONLY, not
  carried as an ADVANCE-grade gate. (Now BANKED — see redirect.)
- **8C-G4 (the 4.4× number must be quarantined from the wasm-boundary gate).** Gate 5 requires
  stop-early 4.4× **across the wasm boundary**; the 4.4× is CONFIRMED for **file round-trip only**;
  the export cost is **unmeasured** (open-q ii). Deferred to GN-4.W4. Do not let the number
  migrate onto the wasm-crossing claim until measured.
- **8C-G5 (tape-crossing home ambiguity).** 8C homes it at `crates/bbnf-bench/…/ts-tape/`;
  LOCK-6B/GN-4.W4 home it at `ts/tape/`. Two directories for one artifact; adjudicate with LOCK-6.
- **8C-G6 (scope tension).** 8C's most-provable stratum (data) is a **bbnf-bench face**, not
  parse-that proper. State whether 8C's deliverable is a parse-that face or a bbnf-bench face.

---

### 8D — CONSUMER-CONTRACT-FIRST — ADVANCED, 50%

**Critics:** CRIT-1 66% / CRIT-2 50%. **Floor 50%; held.** Named reason CRIT-1 over-read:
CRIT-1's 66% scores within-lock spec-*soundness* (the spec is sound); CRIT-2's cross-lock lens
correctly discounts for **no prototype ran**, the make-or-break crater harness is **blocked on
open-q (i)**, and the charter's named PT-E1 diagnostics design is a **contract without a
mechanism**. The recon is exceptional but is priced as "not below 50," not "above."

**What converged (banked evidence, strongest recon in the lock):**
- Ground-truth supersessions (constitution ix): value.js is **4.0.0** (not 3.1.0), declares **NO**
  parse-that dependency, and **bundles** parse-that (`libraryExternal=[]`, `sideEffects:false`).
  The **semver inversion** (1.0→1.1 invisible to value.js public semver; break risk entirely at
  value.js *build* time) is real and non-obvious.
- The whole contract is **nine symbols**; two consumers reach **below** the combinator layer
  (`new Parser((state)=>…)`, `new ParserState<T>`, direct `mergeErrorState`, post-hoc
  `parseState()` field reads). The diagnostics shape consumers want is the **field triple**
  (`furthest`, `expected: string[]`, `getLineAndColumn`), read on the returned state — **not** a
  rich object; `Suggestion`/`SecondarySpan` stay internal.
- R-8D4 (rebuild value.js `dist/`; the stale checked-in 1.0.0 bundle is a false green) is
  exactly the gate-that-cannot-fail the checklist hunts — 8D caught it itself.

**Exact open gaps:**
- **8D-G1 (the deciding harness is currently unconstructable).** Gate 4, the three-repo
  bundled-crater, **depends on** how value.js resolves parse-that at build time — open-q (i),
  admittedly unresolved, "must be pinned before any suite runs." No crater ran this pass. Not a
  BLOCK (a lookup, not a missing primitive), but "pure construction" understates that (i) gates
  the whole harness.
- **8D-G2 (nine-symbol universal rests on a grep pass).** "Everything else is free to churn" is a
  universal over three repos established by reading, not a mechanical import census. If one missed
  symbol (`opt`/`many`/`seq`/`map`/`trim`/`skip`/`next`/`wrap`) is imported, the migration artifact
  silently omits it. Requires a **mechanical** import oracle (grep/AST over all three dist+src).
- **8D-G3 (PT-E1 is a contract without a MECHANISM).** 8D specifies *what* is readable and *what
  must not happen* (no 0.8.2 console leak) but does **not** design *how* the field triple is
  per-parse-scoped without a hot-path tax (context object? reset-on-entry? null-scope read?). The
  charter names PT-E1 per-parse scoping as a **design problem**; 8D delivers the contract and
  leaves the mechanism asserted.
- **8D-G4 (the hardest consumer is unmeasured and already flashed warning).** latex-paper's real
  `^0.7.1→^1.1.0` jump is born-RED and unrun; PROTO-2's overlay already surfaced **4 errors**
  ("0.7.1→1.0.0 API-drift artifacts") — the real jump has breaks, **unenumerated**. The three-step
  attribution matrix (0.7→1.0, then 1.0→1.1) is specified but unrun; MIGRATION.md has no input yet.
- **8D-G5 (the field triple is already being narrowed by a sibling).** 8A's StateView drops
  `expected` + `getLineAndColumn` (8A-G1) — the very fields 8D is protecting. Must be reconciled
  before the StateView snapshot lands.

---

## REDIRECT — CROWDING & UNDEREXPLORED FORMULATIONS

**Crowding found (within-lock, dispatcher arm).** 8A center-B ("build-time specializer, the V8
face of LOCK-4B") and 8C Stratum-B ("generated node-vocabulary dispatch") **converge to one
mechanism**: the build-time specialized dispatcher owned by LOCK-4B. Per the redirect edict, the
redundant formulation is marked **BANKED**:
- **BANK 8C Stratum-B generated-dispatch** (redundant with 8A center-B). 8A owns the V8
  justification + the measurement (does it inline?); 8C's generated-dispatch is the same mechanism
  from the correspondence side. One home suffices.

**Underexplored formulation opened in its place.** Both critics flagged that gate-4/gate-3 lack
the SFI-heterogeneity qualifier PROTO-1 discovered, and that a build-time generator must decide
*which* sites to unroll from a build-time approximation of a **runtime** property (SFI identity).
Open this genuinely underexplored question for pass-2 (shared 8A/8C, homed in 8A):
- **The static SFI-heterogeneity approximation** — the build-time predicate that decides which
  `all()`/`any()` sites are SFI-heterogeneous (thus need unrolling) versus homogeneous (already
  inline in the general loop). This is the un-designed decision rule the specializer needs and
  the corrected form of gate-4. It is a real, named, within-lock formulation — not a re-derivation
  of the LOCK-4B fuser.

**Cession (cross-lock, not crowding).** 8C Stratum-A / Stratum-C / linter are **double-homed with
LOCK-6** (6B / 6D / 6A). This is an ownership cession, not a redundancy bank: cede the mechanism
to LOCK-6; keep 8C's genuine residue (the parse-that FACE + the three-divergence classification as
EXEMPT rows). Flagged for GESTALT-P; adjudicate the physical tape-crossing home with LOCK-6.

---

## CROSS-POLLINATION (named grafts, both families developed enough)

- **GRAFT 1 — 8D's consumer-field enumeration → 8A's StateView (justified; closes 8A-G1 + a
  missed incompatibility-map entry).** 8A ships a StateView that drops `expected` +
  `getLineAndColumn`; 8D has **already enumerated** the exact consumer field reads
  (`buildDiagnostic`: `offset, furthest, expected, getLineAndColumn, isError, value`). Graft 8D's
  enumerated triple into 8A's StateView **before the snapshot lands**. Both developed: 8A has the
  patch, 8D has the exact field list. This is the concrete redress for the 8A⟂8D collision.
- **GRAFT 2 — 8B's soundness discipline → 8A's `as unknown as` cast (justified; closes 8A-G2).**
  8A's snapshot launders through `oldView as unknown as ParserState<T>` — the exact unsound cast
  8B exists to kill. 8B supplies a precise `StateView` type (the read-surface `ParserState`
  shares) so 8A's snapshot needs no `as unknown as`. Both developed: 8A has the patch, 8B has the
  type-plane machinery. Resolves the falsely-declared 8A⟂8B "non-conflict."
- **PROPAGATE (shared finding, not a graft) — PROTO-1's SFI-heterogeneity nuance → 8A gate-4 +
  8C gate-3.** The general arm inlines for HOMOGENEOUS leaves; both gates false-green on a
  homogeneous corpus. Fold the qualifier into both (and into center-B's specializer scope).

---

## INCOMPATIBILITY-MAP CORRECTIONS (FAMILY-SPECS §"THE INCOMPATIBILITY MAP" is INCOMPLETE)

1. **ADD 8A⟂8D (missed entry).** 8A's `StateView` field set `{src,value,offset,isError,furthest}`
   drops 8D's contracted field triple (`expected`, `getLineAndColumn`). Resolution: GRAFT 1 —
   StateView must carry every field the enumerated consumers read. Must be redressed before the
   snapshot lands.
2. **CORRECT 8A⟂8B (declared "non-conflict" is FALSE).** 8A's `as unknown as ParserState<T>`
   double-cast violates 8B's "no unsound casts" thesis. The map's row "8A⟂8B: none, coexist by
   construction" is refuted. Resolution: GRAFT 2.
3. **STANDING (8B⟂8D, correctly deferred).** Result-union (8B) vs field-triple (8D) reconciled
   only additively-and-provisionally. Deciding experiment: run value.js's own `buildDiagnostic`
   against a per-parse-scoped returned failed state. Unrun this pass; carries to pass-2.

---

## PLANE-WIDE GAPS (charter items no family homes)

- **PACKRAT is UNDESIGNED across all four families** (charter item, PORTFOLIO:189; priorart
  carries 21 mentions the synthesis dropped to one harness-disclosure flag). No family owns
  packrat placement, memo-key/`MemoCell` shape-stability under 8A, or the type of a memoized
  parser under 8B. Constraints to preserve (C5): opt-in latch, **no throughput gate on arming**,
  armed/unarmed disclosed in **every** perf table; KEEP is provisional pending a bbnf-lang LR
  consumer (Y.13). **Pass-2 must home packrat.**
- **The two protos measured the two EASIEST uncertainties** (does inlining fire; does the thin
  type slice compile). The HARD uncertainties are all unmeasured: specializer code-size +
  SFI-decision-rule (8A/4B), the full typed algebra + `DropUndefined` budget (8B), the real latex
  jump + PT-E1 mechanism (8D), the regen-stability + double-home (8C). Pass-2 briefs target THESE.

---

## CROSS-LOCK LEDGER (dependencies, not within-LOCK-8 blocks)

| item | LOCK-8 claim | homed / collides with | disposition |
|---|---|---|---|
| Build-time TS specializer / fuser | 8A center-B; 8C-ambitious's only door | **LOCK-4B** (TS number **unrun**, deciding axis unmeasured) | 8A/8C marked *dependent on unconverged LOCK-4B*, not ADVANCE-clean |
| Isomorphism linter | 8C "the LINTER" | **LOCK-6A** MIRROR-TREE (claims parse-that core) | Cede; 8C re-scoped to consumer |
| One spec → Rust dumper + TS decoder | 8C Stratum A | **LOCK-6B** SPEC-GENERATED-SKELETON (recasts GN-4.W4) | Double-home; 6B owns generator |
| Behavioral differential oracle | 8C Stratum C | **LOCK-6D** ORACLE-ISOMORPHISM | Double-home; 6D owns the null-hypothesis test |
| Tape crossing directory | `crates/bbnf-bench/…/ts-tape/` | **GN-4.W4 / LOCK-6B** `ts/tape/` | Adjudicate physical path with LOCK-6 |

**The −14% VM/runtime-switch refutation anchor is SOUND and reusable** (A.W3
`span-dispatch.bench.ts`, B.W0 P-inv-28; not SPEC_ONLY). Three dispositions lean on it: 8A
worst-case bound, 8C-ambitious BANK, incompatibility-map row 2. No critic objection.

---

## PASS-2 DIRECTIVES

**What research must answer / what to prototype (per family):**
- **8B:** wire `DropUndefined` into `all` as the spec mandates; run the value.js opt-in-`all`
  **runtime differential** (R1); re-measure the instantiation budget on the **real** signature;
  resolve the three-route soundness question; prototype the **full** algebra
  (`then→[T,S]`, `or`, `chain`, `opt→T|undefined`) and remove PT-E2's `Parser<any>` **input**
  leaks (the untouched headline); run latex-paper **natively** for the budget corpus. Re-label the
  pass-1 PASS "literal-precision subset only."
- **8A:** before the StateView snapshot lands, add a behavioral `.mapState`-consumer oracle
  enumerating fields read across value.js/latex/keyframes; carry 8D's triple into StateView
  (GRAFT 1) and replace the `as unknown as` cast with an 8B-supplied precise type (GRAFT 2);
  prototype (or concretely spec) the build-time specializer **including the static
  SFI-heterogeneity decision rule and an emitted code-size ceiling**; re-run the deopt sweep on the
  **real** value.js corpus; add a **Maglev**-tier inlining check; measure the **temporal** leg.
- **8C:** RE-SCOPE from owner to **LOCK-6 consumer** (Stratum A→6B, C→6D, linter→6A); keep only
  the parse-that FACE + three-divergence EXEMPT rows; run a minimal single-spec→two-artifact
  **regen-stability** probe; **quarantine** the 4.4× from the wasm-boundary gate until export cost
  is measured; adjudicate the tape-crossing home; state parse-that-face vs bbnf-bench-face scope.
- **8D:** pin value.js's build-time resolution (open-q i) **first** — then stand up the
  three-repo bundled-crater with the mandatory rebuild step; run the `buildDiagnostic` deciding
  experiment; run the real latex **0.7→1.0→1.1** three-step matrix and enumerate the breaks;
  **design** the PT-E1 per-parse-scoping mechanism (null-scope field read, no alloc when off);
  replace the nine-symbol prose census with a **mechanical** import oracle.
- **PLANE-WIDE:** home **packrat** (memo-cell shape under 8A, memoized-parser type under 8B,
  latch/no-throughput-gate preserved); run the `buildDiagnostic` oracle to adjudicate the
  8B⟂8D result-union-vs-field-triple split.

**Withheld from researchers (settled — do NOT re-open or re-litigate):**
- The **−14% VM/runtime-switch refutation** (C1, measured); any runtime tag/switch on V8 is dead.
- **Constant-capture DOES inline** (PROTO-1); the megamorphic `.parser` **LOAD is monomorphic**
  (only the CALL loses child inlining — no 10–50× cliff).
- The **packrat opt-in latch** with no throughput gate is settled; no "smart auto-arm."
- **Don't split `parser.ts`** (net-negative, r6 #6); god-module cure bounded to utils.ts.
- **8C-ambitious runtime TS dispatch** — BANKED (refuted).
- **`Parser<T,E>` expected-label frontier** — pre-BANKED (Y.13); reopen only on a named consumer.
- **`dispatch<const K>`** literal-key precision — reverted, buys the consumer nothing.

---

## PASS HISTORY

### Pass 1 (agglomerated) — 2026-07-16
- Inputs: FAMILY-SPECS, PROTO-1 (8A, RAN_MEASURED PASS), PROTO-2 (8B, RAN_MEASURED PASS),
  CRIT-1 (within-lock), CRIT-2 (cross-lock), research-{A,B,C,D,priorart}.
- Two prototypes ran; both genuine, refutable, cold-deterministic. PROTO-1 struck 8A's sharpest
  uncertainty dead-center and passed it; PROTO-2 **swerved** 8B's sharpest uncertainty
  (opt-in-`all`/`DropUndefined`) and passed a strictly weaker artifact than the spec proposes.
- Standings set: 8A 58% ADVANCED, 8B 50% ADVANCED, 8C 45% ADVANCED, 8D 50% ADVANCED;
  8C-ambitious BANKED, `Parser<T,E>` BANKED. No family BLOCKED-candidate.
- Redirect: BANK 8C Stratum-B generated-dispatch (redundant with 8A center-B); open the static
  SFI-heterogeneity approximation in its place. Cede 8C Stratum-A/C + linter to LOCK-6.
- Cross-pollination: GRAFT 1 (8D field enumeration → 8A StateView), GRAFT 2 (8B precise type →
  8A cast). Incompatibility-map corrected: +8A⟂8D, 8A⟂8B non-conflict refuted.
- Plane-wide hole named: packrat undesigned. Cross-lock: 8A/8C depend on unconverged LOCK-4B.
