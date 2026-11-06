# LOCK-8 — THE TS BACKBONE PLANE — pass-2 CRIT-1 (adversarial, WITHIN-LOCK weight)

*Critic 1 of 2, pass 2. Charter: internal soundness of each family spec and whether the
prototypes tested the SHARPEST uncertainty. Every load-bearing fact re-verified mechanically
against the live trees (parse-that, value.js@`86a9a17`/`tranche-u`, latex-paper@`c140473`,
keyframes.js) — not taken on the synthesis's word. Percentages are the critics' floor.*

---

## 0. WHAT I VERIFIED MECHANICALLY (the spec's spine holds — mostly)

The §0 consumer-divorce supersession is the load-bearing fact the whole lock now rests on. I
re-ran the census against the live trees and it **holds**:

- **value.js @ HEAD `86a9a17` (branch `tranche-u`):** `grep 'from "@mkbabb/parse-that"' src` = **0**;
  `all(` combinator = **0**; `any(` = **0**; **no `dependencies` key** in `package.json`; one
  `parse-that-FREE` prose marker. CONFIRMED — research-A's 61-`all()`/93-`any()` premise is
  correctly struck.
- **latex-paper @ `c140473`:** **8** imports / **5** symbols (`Parser, ParserState, regex, string,
  any`); installed **0.7.1**; pin `^0.7.1`; `any()` ×3, `.opt()` ×11, parse-that-`all` ×**0**.
  CONFIRMED. (The 3 `all(` grep hits I found are a `.all()` **method** on a labels Map in
  `transform/sections.ts`+`labels.ts` — NOT the combinator. The spec's "latex has no `all`" claim
  survives my adversarial check.)
- **keyframes.js:** 0 parse-that in `package.json`, 0 in `src`. Transitively severed — CONFIRMED.
- **`mapState`:** zero consumers across value.js/latex/keyframes `src` (0/0/0); only 5 core hits,
  all definitional (`parser.ts:162,168,184,185` + `state.ts:167`). The 8A-G1/G2 dissolution is
  **genuine, not vacuous** — a bug in provably dead code is not a live bug. CONFIRMED, and the
  `Object.create(state)` fix seam at `parser.ts:177` is real.
- **8D PT-E1 mechanism:** `diagnosticsEnabled` global lives at `utils.ts:6`, a **single** `let`
  (deletion proof holds), read at 9 sites; the state is already per-parse. The `state.diag`
  boolean-field mechanism is grounded, not asserted. CONFIRMED.
- **8A ReadonlyState:** `ParserState` declares `src,value,offset,isError,furthest` all public
  (`state.ts:41-46`); `implements ReadonlyState<T>` type-checks with no cast, no runtime change.
  CONFIRMED — GRAFT-2 realized by mechanism.

**The spec's recon is honest and its mechanical claims survive adversarial re-measurement.** My
findings below are therefore NOT "the facts are wrong" — they are **(a)** the synthesis document
is frozen before its own prototypes and does not integrate them; **(b)** two families advanced on
design only with no prototype this pass; **(c)** the elegant-reduction trap is live in 8B and 8A;
**(d)** a single unconfirmed owner premise gates the entire re-cut; **(e)** a real cross-lock
defect PROTO-2 surfaced is un-routed.

---

## CROSS-CUTTING DEFECT A — FAMILY-SPECS.md is STALE relative to its own pass-2 prototypes

`FAMILY-SPECS.md` mtime **09:50**; `PROTO-2.md` **10:00**; `PROTO-1.md` **10:02**. The synthesis
of record was frozen **before both prototypes ran** and never re-opened. Consequences the
document ships with, uncorrected:

- **8C-G-REGEX (lines 259, 427) still reads "8C's central regex classification is currently
  unfalsified. This is Brief 2."** PROTO-2 **falsified it** ten minutes later: the row is
  `MIRRORED-WITH-GUARD`, the divergence is REAL and measured, and a **SECOND hazard class**
  (lazy-quantifier → greedy, the DFA discards NFA priority) exists that the spec's gate (3)
  ("ordered-alternation + anchoring") does not name. The spec's born-RED gate and its 6D-corpus
  requirement are **incomplete** relative to the prototype sitting in the same directory.
- **8B-G3 ("the full algebra is unprototyped… load-bearing un-run work"):** PROTO-1 **prototyped
  the 4-combinator algebra** (`then/or/chain/opt` INPUT-leak removal + `DropUndefined` in `all` +
  the duplicate `ExtractValue` collapse), compiled it GREEN at **+2.9% instantiations / 0 new
  TS2589**, and RAN the runtime drop-undefined differential. The gap text overstates what remains.

This is not cosmetic. The **critics and agglomeration are told to read the registry + specs as
the standing truth**; a spec that contradicts its own evidence corpus mis-states which gaps are
open. The synthesis MUST be re-cut to fold PROTO-1/PROTO-2 before pass-3, or the gap ledger is
wrong on two families.

## CROSS-CUTTING DEFECT B — the entire re-cut hangs on ONE unconfirmed owner premise

Every struck value.js figure, the center-B demotion, the whole Y.13 reckoning, and 8D's contract
flip are downstream of "value.js 4.0.0's divorce is **intended** (tree-of-record) vs a
mid-migration WIP branch." I verified value.js is on branch **`tranche-u`, 139 commits past
`tranche-t-close`** — a branch name that reads as *active in-flight work*, not a frozen release.
The spec flags this (§0 item 5, "OWNER CONFIRMATION REQUIRED") but then develops all four families
as though the premise is settled. **This is a single point of failure the within-lock view must
price in:** if `tranche-u` re-adds parse-that (a migration reversal, entirely possible on a WIP
branch), 8A center-B's corpus returns, the struck 8B evidence un-strikes, and 8D's negative guards
invert. No family can reach >~70% while its foundational fact is an unconfirmed lookup.

## CROSS-CUTTING DEFECT C — no 8A or 8D prototype ran this pass

PROTO-1 = 8B, PROTO-2 = 8C. **8A and 8D advanced on design/recon ALONE this pass.** 8A's
Maglev-inlining question (8A-G-MAGLEV) leans entirely on pass-1's PROTO-1, which **forced
`--no-maglev` (synchronous TurboFan)** — so the specializer's inlining under Node 26's *default
production tier* remains un-run for a second pass. 8D's make-or-break crater is "constructable"
(I confirmed the tarball/`node_modules` mechanics) but **un-run**. Two of four families carry zero
new measured evidence.

---

## FAMILY 8A — THE SHAPE-STABLE CORE

**Verified strengths.** The two pass-1 defects (masked field-drop, unsound cast) are **genuinely
dissolved** — `mapState` has zero consumers (I confirmed 0/0/0 across all trees), so the narrowed
field set breaks nothing, and `ReadonlyState<T>` `implements` removes the `as unknown as` cast by
mechanism (I confirmed all 5 fields are public). This is the cleanest defect-closure in the lock.

**The elegant-reduction trap is live.** "The defects dissolve, the boundary is sound, the SFI
predicate is answered, packrat is homed… **and-then the arity-N specializer** (the family's
terminal deliverable) is unbuilt, un-prototyped, un-Maglev-verified, and — per §0 — **consumer-less.**"
Everything that converged is *cleanup and design*; the actual product regressed in viability.

**Enumerated open gaps (CRIT-1):**
1. **8A-G-CORPUS (load-bearing).** Center-B's justifying corpus (value.js `all()`/`any()` mass) is
   gone (verified). Its survival is COUPLED to 8D-G-Y13 and to a census of surviving heterogeneous
   arity≥4 sites that **has not been run**. Building a specializer for a divorced corpus is Y.13
   substrate. UN-RESOLVED.
2. **8A-G-MAGLEV.** Specializer inlining under `--trace-maglev-inlining` at each arity is UN-RUN;
   pass-1's only inlining evidence forced `--no-maglev`. Two passes, still un-verified on the
   default tier.
3. **8A-G-mapState-PARSIMONY (I sharpen this from the spec's "minor").** The `ReadonlyState`
   fix is itself work on a **consumer-less combinator**. If 8D-G-Y13 prunes `mapState`, the entire
   GRAFT-2 boundary redesign evaporates. The spec **counts this fix as banked-converged**; I hold
   that it should be BANKED-PENDING-8D, because its substrate is a deletion candidate. Counting it
   as converged progress inflates 8A's column.
4. **8A-G-SPECIALIZER-UNBUILT.** No prototype exercised the specializer this pass; the emitted-size
   budget (8A-G-CEILING) reads from a corpus so thin it is "evidence center-B may not be worth
   building" (the spec's own words). The terminal deliverable is design-only.

**Convergence: 66%** (floor). Defect-dissolution is real and verified (justifies advance from 58);
capped because the terminal deliverable is consumer-less + unbuilt + un-Maglev, and part of the
"converged" work stands on a deletion-candidate. Gates are falsifiable (not vacuous) — good.

---

## FAMILY 8B — TYPE-LEVEL TOTALITY

**Verified strengths.** PROTO-1 genuinely RAN: the 4-combinator algebra + `DropUndefined` +
`types.ts` collapse compiles GREEN at **+124 instantiations / +2.9%, 0 new TS2589**, precision
both ways (two `@ts-expect-error` negatives fire), non-breaking on parse-that self-use, and the
runtime drop-undefined differential confirms the documented widening. Instantiation counts are
checker-deterministic (load-independent) — the +2.9% is solid; check-time correctly labeled
directional-only. This is legitimate, well-disclosed measured evidence.

**The elegant-reduction trap is live, and it recurs from pass-1.** The spec's OWN named "sharpest
residual" — 8B-G4' step-ii: the instantiation budget on **latex NATIVE**, isolated from the
0.7.1→1.x migration, plus the **emit-identity `dist/` byte-empty proof** (the clause-v
zero-runtime-cost invariant) — was **NOT run**. PROTO-1 ran a **synthetic latex-SHAPE stress
corpus** instead, explicitly deferring latex native ("requires the migration first — out of a
single parsimonious probe's budget"). Pass-1 was criticized for testing the two EASIEST
uncertainties; **Brief 1 was scoped to latex native and PROTO-1 measured the synthetic proxy
again.** The load-bearing consumer-confirmed number is un-run for a THIRD time, and the
constitution clause-v (zero runtime cost) proof is entirely un-executed.

**Enumerated open gaps (CRIT-1):**
1. **8B-G4'-NATIVE.** The ONE surviving real instantiation number (latex 0.7.1 native, step-ii
   isolated from migration) is UN-RUN. The +2.9% is on a synthetic corpus, honestly disclosed but
   not consumer-confirmed.
2. **8B-G4'-EMIT.** `dist/` step-ii byte-empty (the clause-v proof) is UN-RUN. Low risk (types
   erase), but the zero-runtime-cost invariant is asserted, not demonstrated — a constitution
   clause-v gate that has never fired.
3. **8B-G3-RESIDUE.** `skip`/`sepBy` INPUT leaks (`skip<S>(parser: Parser<T|S>)`,
   `sepBy<S>(sep: Parser<S|T>)`) are UN-tightened (PROTO-1 admits, outside the 4-combinator
   scope). "No `Parser<any> anywhere`" is ~70% closed, not closed.
4. **8B-G-STALE-SPEC.** FAMILY-SPECS 8B-G3 text ("full algebra unprototyped… load-bearing un-run
   work") contradicts PROTO-1. Must be re-cut.

**Convergence: 70%** (floor). PROTO-1 is a real, well-disclosed PASS justifying a substantial
advance from 50; capped because the sharpest uncertainty (latex native + emit-identity) was
deferred a third time onto a synthetic proxy, and the clause-v proof has never run.

---

## FAMILY 8C — THE ISOMORPHIC MIRROR

**Verified strengths — the best prototype in the lock.** PROTO-2 drove the EXACT runtime matchers
(Rust `bbnf_regex::Dfa::find_at`, the `leaf.rs` path; TS sticky `RegExp`, the `leaf.ts:316-347`
path — not reimplementations), fed 19 identical triples, and resolved the sharpest 8C
uncertainty: the two-engine divergence is REAL (6 synthetic diverge, Rust leftmost-longest vs JS
leftmost-first), a **SECOND hazard class** exists (lazy→greedy, the DFA discards all NFA
priority), yet **all 10 real parse-that leaves agree byte-for-byte** (census-backed: the two `|`
sites have disjoint first-bytes, every quantifier greedy). I re-read the raw evidence
(`evidence-rust-differential.out` / `evidence-js-differential.out`) — the 13-agree/6-diverge split
is exactly as reported. This genuinely tested the sharpest uncertainty and produced a material
result. The `MIRRORED-WITH-GUARD` reclassification is earned.

**But the deliverable is un-built and the spec lags the prototype.**

**Enumerated open gaps (CRIT-1):**
1. **8C-G-LEDGER (the deliverable).** The single machine-checkable ledger file — 8C's SOLE residue
   after cession to LOCK-6 — is **UN-WRITTEN.** The census and classifications exist in research-C;
   the product does not.
2. **8C-G-STALE-SPEC.** FAMILY-SPECS still classifies the regex row's uncertainty as "unfalsified"
   and its gate as "ordered-alternation + anchoring." PROTO-2 requires: row = `MIRRORED-WITH-GUARD`,
   guard forbids **both** hazard classes, 6D corpus carries **lazy-quantifier** cases too. The
   spec's gate would GREEN a corpus that hides the second divergence. Must be re-cut.
3. **8C-G-ENGINE-DEFECT (NEW, un-routed).** PROTO-2 surfaced that parse-that's Rust regex engine is
   **internally self-contradictory**: `nfa.rs:313` builds priority edges *intending* leftmost-first
   (to mirror JS); `dfa.rs` subset construction **discards them**; `find_at` delivers
   leftmost-longest; lazy quantifiers silently collapse to greedy. This is a latent **defect in
   parse-that itself** (a regex-continuum / LOCK-5 concern), not merely a correspondence question —
   any priority-sensitive leaf exposes it inside Rust alone. **The spec routes it nowhere.** A real
   defect with no owner is the inverse of consumer-less substrate and must be handed to LOCK-5.
4. **8C-G-LOCK6-CEILING.** 8C's regen gate (8C-G-REGEN) is ceded to 6B and its parity oracle to 6D;
   both are un-stood-up. 8C **cannot converge to 100% independent of LOCK-6** — its ceiling is
   bounded by another lock. The re-scope is legitimate but makes 8C a near-sub-deliverable of
   LOCK-6; the within-lock view flags this dependency explicitly.
5. **8C-G-TAPE.** PROVE-14 4.78× correctly quarantined as pure-JS-decode (no shim-perf misuse —
   good); wasm-boundary cost UN-MEASURED; ceded to 6C. Standing quarantine, not resolved.

**Convergence: 65%** (floor). The prototype advanced the *knowledge* enormously (justifies advance
from 45); capped because the family's actual product (the ledger) is unwritten, the synthesis text
lags its own prototype, a real engine defect is un-routed, and the convergence ceiling is bound to
LOCK-6.

---

## FAMILY 8D — CONSUMER-CONTRACT-FIRST

**Verified strengths — the strongest recon.** I confirmed every mechanism claim: import oracle (8
hits / 5 symbols / none below combinator layer), the `diagnosticsEnabled` global + single-`let`
deletion proof, latex's pinned-0.7.1-tarball crater mechanics, the per-parse state. The `state.diag`
boolean-field PT-E1 design (beating a scope object that re-introduces alloc) is grounded and sound.

**But the family's headline is designed, not demonstrated, and its own central decision is un-made.**

**Enumerated open gaps (CRIT-1):**
1. **8D-G-Y13 (the central decision, UN-MADE).** With value.js + keyframes gone, `dispatch`, `all`,
   packrat, and the rich diagnostics vocabulary have **no external consumer** (I confirmed 0
   external `all`/`dispatch` reach). 8D must choose: **(a)** declare parse-that's own suite the
   first-party consumer, OR **(b)** prune to the 5-symbol contract + primitives. This is the
   load-bearing decision of the WHOLE lock (it also decides 8A center-B) and it is **deferred, not
   resolved.** The spec states the two lawful outcomes but makes neither.
2. **8D-G-CRATER (make-or-break, UN-RUN).** The 1-positive + 2-negative crater is constructable (I
   verified) but never executed; it must baseline latex GREEN at 0.7.1 first. The family's central
   claim — "the contract holds under a candidate bump" — is un-demonstrated.
3. **8D-G-VUETSC (the one real 1.1.0 risk, UN-RUN).** The inference tightening may redden `vue-tsc`
   at latex's 3 `any()` sites (`text.ts:145,154`, `document.ts:81` — I confirmed these are the
   parse-that `any` combinator). Caught only by `tsup && vue-tsc`, never `vitest`; un-run and shared
   with 8B-G4.
4. **8D-G-DUAL.** Nested parse-that 0.6.0 via bbnf-lang → two instances; a MIGRATION.md row, un-addressed.
5. **8D-G-OWNER (shared Defect B).** The Y.13 reckoning and every negative guard presume value.js's
   divorce is intended. Unconfirmed (branch `tranche-u`, active). A precondition the campaign has
   not closed.

**Convergence: 70%** (floor). Design convergence is high and every mechanism verified (justifies
advance from 50); capped because the make-or-break crater is un-run, the `vue-tsc` risk is un-run,
the family's OWN central Y.13 decision is deferred, and the owner premise is unconfirmed.

---

## CHECKLIST SWEEP (within-lock)

- **Vacuous convergence / gates-that-cannot-fail:** NONE. Every born-RED gate is falsifiable (8A
  `%HaveSameMap`, 8B byte-empty diff, 8C regex differential PROTO-2 proved *can* fail, 8D crater).
  Good.
- **Spec-cites-itself circularity:** NONE internal — §0 rests on external mechanical facts I
  re-verified. BUT §0 rests on ONE unconfirmed owner premise (Defect B) — fragile, not circular.
- **Elegant-reduction trap:** LIVE in **8B** (algebra compiles → latex-native + emit-identity
  deferred, 3rd time) and **8A** (defects dissolve → specializer unbuilt/consumer-less/un-Maglev).
- **Legacy aliases / masked fallbacks:** NONE found. `not`↔`minus` name-drift is disclosed as a
  MIRRORED row, not masked.
- **Unverified gestalt:** N/A (lock, not gestalt).
- **Consumer-less substrate:** PERVASIVE and spec-self-aware (mapState fix, center-B, packrat,
  diagnostics vocab) — but the resolution (8D-G-Y13) is DEFERRED, so it caps every family.
- **SPEC_ONLY anchoring a perf claim:** NONE. 4.78× quarantined pure-JS-decode; 8A temporal leg
  labeled load-noisy & reported separately from structural; PROTO-1 check-time directional-only.
  Clean discipline throughout — a genuine strength.
- **Prototype-tested-the-sharpest-uncertainty:** 8C YES (PROTO-2, fully); 8B PARTIAL (synthetic
  proxy, sharp latex-native part deferred); 8A NO prototype; 8D NO prototype.
- **NEW un-routed cross-lock defect:** parse-that regex engine internal self-contradiction (8C-G3)
  → belongs to LOCK-5, assigned to no one.

## VERDICT

Pass-2 is materially cleaner than pass-1 and its recon survives adversarial re-measurement — but
the synthesis document is frozen before its own prototypes, two families carry no new evidence, the
elegant-reduction trap is live in the two families with prototypes' hardest halves deferred, and
the whole re-cut hangs on an unconfirmed owner premise. No family is BLOCKED (concur). Floors:
**8A 66% · 8B 70% · 8C 65% · 8D 70%.** Mandatory pass-3 predicates: (i) re-cut FAMILY-SPECS to fold
PROTO-1/PROTO-2; (ii) obtain the value.js-intended owner confirmation before banking any number;
(iii) run the deferred sharp halves — latex-native+emit-identity (8B), the crater+`vue-tsc` (8D),
Maglev inlining (8A); (iv) write the 8C ledger and route the regex-engine defect to LOCK-5.
