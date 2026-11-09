# LOCK-8 — THE TS BACKBONE PLANE · pass-2 · ADVERSARIAL CRITIC 2 of 2

*Critic 2, cross-lock-weighted. Read: PORTFOLIO.md (LOCK-8 charter + CONSTITUTION),
pass-2 FAMILY-SPECS, pass-2 PROTO-1 (+diff) / PROTO-2 (+patch, +raw evidence traces),
DIRECTIVES-RESEARCH, pass-1 CRIT-2, and the sibling LOCK-4 REGISTRY for the double-home read.
Every §0 census figure re-verified mechanically against the live repos (value.js @ `86a9a17`,
latex-paper @ `master`, parse-that @ `1.0.0`). Convergence is EARNED; this is pass 2 — the
consumer divorce is a genuine and honestly-recorded advance, but the lock has re-homed its
ENTIRE evidence base onto one repo whose health it did not audit, and it has silently dropped
the pass-1 cross-lock double-home rather than resolving it.*

---

## 0. THE §0 CENSUS IS TRUE — AND I RE-RAN IT

Unlike most convergence claims, §0 is falsifiable and I falsified nothing. Mechanical re-run:

| claim | pass-2 asserts | I measured | verdict |
|---|---|---|---|
| value.js HEAD | `86a9a17`, 139 past `tranche-t-close` | `tranche-t-close-139-g86a9a17` | ✓ |
| value.js parse-that imports (src) | 0 | 0 | ✓ |
| value.js `all()`/`any()` | 0 | 0 | ✓ |
| value.js `dependencies` key | absent | absent | ✓ |
| mapState consumers (all 3 trees + core) | 0 external, def+registry only | 0 in value.js/latex; core = `parser.ts:162/168/184/185` + `state.ts:167` registry, **no combinator routes through it** | ✓ |
| latex import sites / symbols | 8 / 5 (`Parser,ParserState,regex,string,any`) | 8 / exactly those 5 | ✓ |
| latex `any()` combinator sites | 3 | 3 (`text.ts:145,154`, `document.ts:81`) | ✓ |
| latex `all()` combinator sites | 0 | 0 (the 3 `all(` greps are `labels.all()` map-method false positives) | ✓ |
| latex `.opt()` | 11, none in `all` | 11 (`commands.ts` ×8, `environments.ts` ×3) | ✓ |

**The consumer divorce is real, the mapState-is-dead finding is real, and the sole-surviving-
consumer pivot is correctly grounded.** This is the strongest measured foundation any of the nine
locks laid this pass, and the family that struck its own pass-1 evidence (8B's 259-file figure)
earns constitution-vii credit. What follows are the cracks the census did NOT look at.

---

## 1. THE DEEPEST CUT — THE SOLE CONSUMER IS ITSELF UNAUDITED, POSSIBLY DORMANT

§0 point 5 demands owner confirmation that value.js's divorce is *intended*. It raises the
**symmetric and larger risk NOT AT ALL**: the whole lock has re-homed 8B's budget corpus, 8D's
contract, and 8A center-B's only surviving justification onto **latex-paper**, and never asked
whether latex-paper is a live consumer. I asked:

- **latex-paper last commit: `c140473`, 2026-07-02** — two weeks stale at this pass (07-16), and
  the commit is *`docs(VIRT)` virtual-copy skeleton work*, not parse-that consumption. value.js
  commits **daily** (`86a9a17` @ 07-15); latex has not moved in a fortnight.
- **latex is frozen at parse-that `0.7.1`** (pinned `^0.7.1`, installed `0.7.1`) while parse-that
  is at **`1.0.0`** with `1.1.0` planned — **two minor cuts behind**, and nothing indicates it
  will ever upgrade. Its last parse-that-touching commit predates the 1.0.0 cut entirely.

So the lock's entire evidentiary pivot rests on a repo that is (a) two weeks idle, (b) two API
cuts behind, (c) last seen doing unrelated virtual-copy work. **If latex is as effectively frozen
as value.js is divorced, then latex is not a "surviving consumer" — it is a *fossil* consumer,
and the plane-wide Y.13 reckoning is not "prune vs internal-suite" but "the contract has NO live
external reader at all."** That is a materially worse constitution-viii position than §0 admits,
and it is un-named. **8D's `vue-tsc` type-break gate at latex's `any()` sites is aimed at a repo
that may never take the bump that would trigger it** — a gate that "cannot fail in practice
because no one will run the migration." The owner-confirmation dependency must be *symmetric*:
confirm value.js's divorce is intended AND confirm latex-paper is a live-forward consumer, or the
sole-consumer pivot is a pivot onto a corpse.

**This is the load-bearing gap the synthesis did not enumerate.**

---

## 2. FAMILY 8A — THE SHAPE-STABLE CORE

**Credit.** The two pass-1 defects (8A-G1 masked-field-drop, 8A-G2 unsound cast) are genuinely
DISSOLVED — I confirmed mapState has zero callers anywhere, including inside parse-that's own
combinators, so the field set cannot break a consumer that does not exist. The `ReadonlyState<T>`
interface is a real fix (`ParserState` declares `src,value,offset,isError,furthest` publicly —
verified `state.ts:36-53`) and removes the pass-1 `as unknown as` at that seam by mechanism, not
assertion. The SFI-heterogeneity predicate is answered. Packrat is homed. This is real advance.

**Gaps (adversarial, cross-lock-weighted):**

1. **8A-G-4B-DOUBLEHOME (the cross-lock finding pass-2 SILENTLY DROPPED — the headline).** Pass-1
   CRIT-2's headline was that center-B's arity-N specializer *is* **LOCK-4B's build-time TS
   fuser** (`leaf.ts:154-273`→any arity), double-homed and unconverged there. Pass-2 re-frames
   center-B **entirely** as a Y.13 *consumer* question and never once names LOCK-4B in the 8A
   section (the only LOCK-4B mention in the whole spec is the 8C-ambitious dispatcher row). The
   double-home did not disappear — it was buried. And the cross-lock reality is now **worse than
   pass-1**: LOCK-4's own REGISTRY records 4B as **"terminal-role BANKED→compile-backend"**, its
   win **"a Rust-monomorphization win with NO V8 analog (T1)"**, its **"isomorphism half 0%
   built."** So the TS fuser center-B depends on is not merely unconverged — **its home lock has
   BANKED it for TS as having no V8 analog.** Center-B is therefore blocked on TWO things: a
   missing consumer (the Y.13 question pass-2 asks) AND a build-time TS-source-fuser mechanism
   LOCK-4B has evidence-banked away from TS. Pass-2 resolves the first and drops the second.
   Constitution ix requires every divergence from the prior finding be a *named supersession*;
   this is a silent one. Center-B cannot be "Y.13-CANDIDATE, ships iff a consumer survives" — it
   must be "Y.13-CANDIDATE **and** mechanism-blocked on LOCK-4B's un-built/T1-banked TS fuser."

2. **8A-G-MAPSTATE-FRONT-Y.13 (mis-classed as "minor"; it collides with 8D-G-Y13).** The family
   dispositions "**ADVANCE (mapState/StateView front)** / center-B → Y.13-CANDIDATE," presenting
   the mapState front as the safe, ship-ready half. But mapState is **dead** (0 callers, verified)
   — the mapState/StateView front is *itself* consumer-less substrate, and its born-RED Gate (1)
   (`%HaveSameMap` across mapState views) exercises a combinator no one calls. The family buries
   this as "minor (survives only for `proof:no-dead-combinator`)" and the incompatibility map
   lists only "8A-center-B ⟂ Y.13," **missing "8A-mapState-front ⟂ 8D-G-Y13-prune."** If 8D's
   reckoning prunes dead surface, mapState is DELETED and the entire `ReadonlyState` fix — the
   family's advertised ADVANCE — is thrown away. It is not "one converged front + one Y.13
   candidate"; it is **two consumer-less items, one of which the family chose to build anyway.**
   The disposition overstates what is safe.

3. **8A-G-MAGLEV (carried, still un-run).** PROTO-1 forced `--no-maglev`; Node 26's default tier
   is Maglev (confirmed — PROTO-2's JS leg ran under "Maglev/production tier"). Inlining under
   Maglev at each arity is un-run. Carried honestly, but it is the tier that actually ships.

4. **8A-G-TEMPORAL (carried, half-run).** The structural leg (alloc −60.1%, deterministic)
   reproduces; the temporal leg for the inlining win itself remains the un-isolated part — the
   dispatch-lane number *halved* on re-measure (+10.2%→~+4.9%), which is itself evidence the
   temporal claims are load-fragile and must never anchor a gate.

**Convergence: 62%.** Two pass-1 defects cleanly closed and the SFI question answered lift it from
55%; but the cross-lock double-home was buried rather than resolved, the "ADVANCE front" is dead
code, and the terminal deliverable is now double-blocked (consumer AND LOCK-4B mechanism).

---

## 3. FAMILY 8B — TYPE-LEVEL TOTALITY

**Credit.** PROTO-1 is a genuine advance over pass-1's thin slice: it RAN the full typed algebra
(`then→[T,S]` sans the unsound `S|T`, `or`, `chain`, `opt`, `all→DropUndefined<Values>`) and
measured **+124 instantiations (+2.9%), zero new TS2589, precision both ways** (baseline RED at
`_t_wideopt`, tightened GREEN, two `@ts-expect-error` negatives fire). The runtime drop-undefined
differential confirms the documented widening (`b` absent → len-2 array vs type's len-3). The two
lawful erasures (`ParserFunction<T>`, `MemoCell.value:unknown`) are correctly preserved. This is
the family that did the most honest new type-checker work this pass.

**Gaps (adversarial):**

1. **8B-G-ASUNKNOWN (NEW — the PASS artifact violates the family's own thesis).** 8B's headline is
   "no `Parser<any>`, no unsound casts." Yet PROTO-1's shipped diff for `all` ends:
   `fuseAll<Result>(parsers as unknown as Parser<unknown>[])` (PROTO-1.diff:80). **The tightening
   prototype meant to prove the algebra is sound reintroduces the exact `as unknown as` double-cast
   class 8B exists to eliminate** — the same class pass-1 CRIT-2 flagged in 8A's StateView fix
   (which pass-2 *did* fix). PROTO-1's report does not mention it. It may be a genuine variance
   fight (`T extends Array<Parser<unknown>>` vs `Parser<unknown>[]` under `readonly`), but that is
   precisely the kind of erasure 8B must either lawfully justify (like `ParserFunction<T>`) or
   remove. A "PASS" that smuggles an unexplained `as unknown as` into the public `all` signature is
   not a clean pass — it is an un-audited soundness leak in the family's flagship deliverable.

2. **8B-G1' (the ONE real budget number is still synthetic).** PROTO-1 discloses it honestly: the
   +2.9% was measured on parse-that self-use + a **latex-*shape* stress corpus**, NOT latex native
   — because latex is 0.7.1 and compiling it against 1.x conflates the migration delta (step-i)
   with the type tightening (step-ii). So the family's load-bearing instantiation figure is on a
   corpus that reproduces latex's shapes but is *not latex*. Combined with §1 (latex may never take
   the migration that would let step-ii be isolated), this number may **never** be measurable on the
   real consumer. The sharpest brief remains un-run and possibly un-runnable without an API bump no
   one will perform.

3. **8B-G4' (emit-identity, un-run).** The two-step `dist/` isolation (step-ii byte-empty) is a
   clause-v gate that has not run on the actual vite toolchain. Carried.

4. **8B-G3 (the full-algebra headline).** PROTO-1 genuinely advances this — it is no longer
   "untouched" as in pass-1. But it is measured on the synthetic corpus, and the `skip`/`sepBy`
   input leaks (`skip<S>(parser: Parser<T|S>)`, `sepBy<S>(sep: Parser<S|T>)`) are explicitly left
   un-tightened. "No `Parser<any>` **anywhere**" is not yet true even in the prototype.

**Convergence: 64%.** The real algebra ran with a genuine budget number and both-way precision —
a solid lift from 50%; docked for the smuggled `as unknown as`, the synthetic (possibly
un-realizable) budget corpus, and the residual un-tightened leaks.

---

## 4. FAMILY 8C — THE ISOMORPHIC MIRROR

**Credit.** The pass-1 headline gap (double-home across LOCK-6's three strata + linter) is
RESOLVED by cession — 8C is re-scoped to the genuine parse-that residue (the divergence ledger +
parity corpora). PROTO-2 is a real, refutable, correctness differential (no perf claim rides on
its load): 19 paired Rust-`find_at` vs JS-sticky-`RegExp` cases, raw traces present and internally
consistent (I diffed `evidence-rust-differential.out` vs `evidence-js-differential.out`: 6 synthetic
diverge, all 10 real leaves agree). This is the highest-value correspondence experiment in the lock.

**Gaps (adversarial, cross-lock-weighted):**

1. **8C-G-SPEC-STALE (the FAMILY-SPECS 8C section is STALE relative to its own PROTO-2).** The 8C
   section (written 09:50) still classifies regex as unconditional
   `LAWFUL-NON-MIRROR(two-engines; HIR-isomorphic per LOCK-5)` with parity floor **"ordered-
   alternation + sticky-anchor cases"** and Gate (3) **"Regex differential GREEN incl. ordered-
   alternation + anchoring."** PROTO-2 (10:02, *newer*) overturns all three: (a) the correct
   classification is **`MIRRORED-WITH-GUARD`**, not unconditional lawful-non-mirror; (b) there is a
   **SECOND hazard class — lazy-quantifier collapse** — the parity floor and Gate (3) do not
   mention; (c) therefore **Gate (3) as written would GREEN a corpus that hides the lazy-quantifier
   divergence** (`\d+?` on `123` → Rust 3, JS 1). The spec never absorbed its own prototype. The
   gate is under-specified per the evidence it commissioned.

2. **8C-G-LOCK5-DEFECT (NEW cross-lock — PROTO-2 found a LOCK-5 bug and 8C did not route it).**
   PROTO-2 read the mechanism from source: `nfa.rs:313` builds priority-ordered ε-edges *intending
   leftmost-first (to mirror JS)*; `dfa.rs` subset construction **discards all NFA priority**;
   `find_at` returns `last_accept` = leftmost-**longest**. So the engine is **internally self-
   contradictory** — a latent defect, not merely a mirror question, and lazy→greedy is its symptom.
   This is a **bbnf-regex correctness matter — LOCK-5's territory** (the regex continuum; automata
   construction). Constitution ii ("the regex engine's HIR has true isomorphism to bbnf HIR; LOCK-5
   decides the lawful form"). 8C's proposed remedy — a **build-time guard forbidding shorter-first
   alternation + lazy quantifiers in leaves** — **forecloses the LOCK-5 fix that would make the
   mirror genuinely unconditional**: if LOCK-5 makes the DFA priority-aware (leftmost-first, JS-
   matching), the divergence vanishes and no guard is needed. 8C proposes to permanently constrain
   the grammar surface to paper over a LOCK-5 defect it discovered, without routing the choice to
   LOCK-5. The regex row's classification is *contingent on a LOCK-5 decision not yet made*, and
   that contingency is un-named.

3. **8C-G-LEDGER (the deliverable is un-built).** The single machine-readable divergence ledger
   (census over `parse_that/src` + `typescript/src/parse`) is the family's residue and is not
   written. Honestly carried, but it is the whole deliverable.

4. **8C-G-TAPE (home ambiguity persists — pass-1 CRIT-2 gap 2, still open).** The tape-crossing
   artifact is still homed at `crates/bbnf-bench/ts-tape/` with "adjudicate the single path with
   LOCK-6" — i.e., the physical double-home vs LOCK-6's `ts/tape/` is *deferred*, not resolved.
   And the 4.78× stop-early stays correctly labelled pure-JS-decode (not a wasm-boundary claim,
   SPEC §0 shim law) — good discipline — but the export cost remains unmeasured.

**Convergence: 60%.** The re-scope-by-cession is a major, correct advance from 45% and PROTO-2 is
excellent; docked hard because the spec text never absorbed PROTO-2 (stale classification + gate),
the discovered LOCK-5 defect is neither routed nor allowed to make the mirror real, and the ledger
is un-built.

---

## 5. FAMILY 8D — CONSUMER-CONTRACT-FIRST

**Credit.** The strongest family this pass. Build-time resolution PINNED (value.js divorce makes
its `dist/` grep true-green; the sole false-green is latex's pinned 0.7.1 tarball →
`rm -rf node_modules/@mkbabb/parse-that && npm i …tgz`). The import oracle RAN mechanically (I
re-verified: latex 8/5, none below combinator layer; value.js/keyframes 0). PT-E1 is DESIGNED as a
**boolean `state.diag` field** on the already-per-parse `ParserState` (one monomorphic-shape
boolean read when off, no alloc, beats a scope object) — a real mechanism, not a contract-shaped
hole. The migration matrix is attributed. `buildDiagnostic` is decided on the consumer axis.

**Gaps (adversarial):**

1. **8D-G-DORMANT (NEW — the contract-first family's consumer may be a fossil; see §1).** "Contract
   derivable from what consumers actually import" is only as live as the consumer. latex is two
   weeks idle, two API cuts behind, last touched on unrelated `VIRT` work. **If latex never bumps
   past 0.7.1, then "the contract is latex's 5-symbol surface" describes a contract with no
   forward-live reader**, and 8D's central positive gate (a reinstalled-candidate crater) tests a
   migration the consumer will never perform. The family must state latex's forward-liveness as a
   born-verified precondition, symmetric to the value.js owner-confirmation dependency it *does*
   name. It names one and omits the larger.

2. **8D-G-Y13 (the central open decision — correctly named, un-adjudicated).** With value.js +
   keyframes gone, `dispatch`/`all`/packrat/rich-diagnostics have zero external consumer. The
   (a) declare-internal-suite vs (b) prune decision is stated as open. This is the right framing —
   but per §1 it is sharper than the spec allows: if latex is also dormant, option (b) prune
   removes nearly the entire public surface, and option (a) means the "consumer" is exclusively
   parse-that's own tests — a Y.13 posture the constitution barely tolerates. This decision *also*
   decides 8A center-B AND (per §2 gap 2) 8A's mapState front. It is one census carrying three
   family fates; it must be run, not deferred to gestalt.

3. **8D-G-CRATER (un-run, but constructable).** Specified, never executed; latex must be baselined
   GREEN at 0.7.1 first or the sole positive gate is uninterpretable. Correctly carried.

4. **8D-G-DUAL (a MIGRATION.md row).** Nested parse-that `0.6.0` via bbnf-lang alongside a bumped
   direct pin — two instances; dedupe-or-document if a `Parser`/`instanceof` crosses the boundary.
   Correctly carried, un-run.

**Convergence: 68%.** The strongest recon-to-mechanism conversion in the lock (crater
constructable, oracle ran, PT-E1 designed, matrix attributed) lifts it well past pass-1's 50%;
capped short of ADVANCE-clean because the entire construction targets a possibly-dormant consumer
and the load-bearing Y.13 adjudication (which forks three families) is deferred.

---

## 6. CROSS-LOCK LEDGER (critic-2 weight)

| item | LOCK-8 pass-2 posture | collides / double-homes with | disposition |
|---|---|---|---|
| Arity-N build-time TS fuser (center-B) | reframed as pure Y.13 *consumer* question; LOCK-4B **not named** in 8A | **LOCK-4B** REGISTRY: 4B "terminal-role BANKED→compile-backend," win "no V8 analog (T1)," "isomorphism half 0% built" | **Silent supersession (constitution ix violation).** center-B is consumer-blocked AND mechanism-blocked on a TS fuser LOCK-4B evidence-banked away from V8. Must be named. |
| Regex leftmost-longest / priority-discard / lazy→greedy | 8C proposes a permanent build-time GUARD in leaves | **LOCK-5** (regex continuum owns NFA→DFA; constitution ii) | PROTO-2 found a real LOCK-5 defect; 8C's guard forecloses the LOCK-5 fix that would make the mirror unconditional. Route to LOCK-5 as a decision, don't paper over it. |
| Divergence ledger + parity corpora | 8C residue (correctly ceded machinery to LOCK-6) | LOCK-6A linter whitelists / LOCK-6D harness runs the corpora | Clean cession — the pass-1 double-home is RESOLVED. Only the ledger↔6A whitelist handshake remains to wire. |
| Tape-crossing artifact home | `crates/bbnf-bench/ts-tape/` "adjudicate with LOCK-6" | **LOCK-6B/6C** `ts/tape/` | Home ambiguity PERSISTS from pass-1; still deferred, not adjudicated. |
| Sole surviving consumer health | latex "sole surviving consumer," un-audited | — (extant PT/GN codex baseline) | latex is 2 weeks idle + 2 API cuts behind; the re-homed evidence base (8B budget, 8D contract, 8A center-B) rests on a possibly-frozen repo. §1 — the deepest un-named gap. |

**Net cross-lock read.** Pass-2 genuinely resolved one of pass-1's two cross-lock headlines (8C's
LOCK-6 double-home, by clean cession) and ran two real prototypes (the typed algebra, the regex
differential). But it **buried the other headline** — the center-B ⟂ LOCK-4B double-home, now
*worse* because 4B has banked its TS face — behind a Y.13 reframe, and it **discovered a LOCK-5
defect it neither routed nor let heal the mirror.** Above all it committed the classic re-homing
error: it moved every scrap of evidence off the divorced value.js onto latex-paper **without
checking latex's pulse.** The census that proved the divorce was rigorous; the census that should
have proved the *replacement* consumer is alive was never run. That omission is the lock's
load-bearing gap and it sits, un-enumerated, under all four families.
