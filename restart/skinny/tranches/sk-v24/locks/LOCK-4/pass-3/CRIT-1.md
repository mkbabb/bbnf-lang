# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-1 (adversarial critic 1 of 2, within-lock weighting) — RE-CUT v2

*Supersedes the 19:57 CRIT-1, which could not have seen the two reproduction probes that postdate it:
**PROTO-1-4A-GA1-combinator-graph-RERUN.md (22:42)** and **PROTO-2-mckeeman-repro.md (22:40)**. Read this
pass: PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H binding), FAMILY-SPECS.md (18:43 re-synthesis),
research-{A,B,C,D,priorart}, PROTO-1/-1-numeric-leaf/-2/-2-vmvalue, PROTO-1-4A-GA1 (19:40),
PROTO-2-live-differential (19:43), and the two 22:40/22:42 reruns; CRIT-2 (cross-lock, 20:07 re-cut).
Tree-of-record spot-checks RE-RUN THIS SESSION on HEAD `3f2350f15af1cc886e2968be70e7bee610ae0e2a`
(descendant of base-pin `ce51c36fc`; merge-base confirms ancestry): `rg ApplyFn crates/ir/src`=**0**,
`rg HostClosure crates/core crates/ir`=**0**, `FnDescriptor` = **6 variants**
(EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr — no HostClosure, no ApplyFn),
`bytecode.rs:184` `parse_that::regex::Dfa` dep **LIVE**, `:238` `Dfa::compile` **LIVE**,
`crates/ir/src/vm` = **1772 LOC**, `try_factor_alt` fires at `branches.len() >= 4` (`fuse_token/factor.rs:25`).
The specs' code facts hold on a newer HEAD than any prior critique used — NOT a spec-cites-itself substrate.
Critic-1 weights internal soundness of each family spec and whether the prototypes tested the SHARPEST
uncertainty. Convergence EARNED; pass 3; no family converged.*

---

## THE GOVERNING WITHIN-LOCK FINDING — REPRODUCTION-IN-PLACE-OF-COVERAGE; THE SPEC IS NOW **TWO PROBE-GENERATIONS** BEHIND

The 19:57 CRIT-1's headline was that the 18:43 re-synthesis froze before its own two briefs (BRIEF A, BRIEF B)
ran, and that both then ran same-pass and PASSED (PROTO-1-4A-GA1 19:40; PROTO-2-live-differential 19:43). That
finding stands. **This re-cut adds the next turn of the same screw: the two probes that ran AFTER the 19:57
CRIT-1 are REPRODUCTIONS, not coverage extensions — and they leave every open gap exactly where it was.**

| Probe (this pass, in order) | What it is | Marginal COVERAGE over its predecessor |
|---|---|---|
| PROTO-1-4A-GA1 (19:40) | BRIEF A, first run — 4A `fuse(Graph)→Graph` byte-identity, bbnf-side builder, json | (the run) |
| PROTO-2-live-differential (19:43) | BRIEF B, first run — VM `ApplyFn` vs live `.map`, 12 hand-picked kinds, HostFold | (the run) |
| **PROTO-1-4A-GA1-RERUN (22:42)** | **Independent replication, BYTE-IDENTICAL patch** (`proto_4a_ga1-RERUN.patch` == `proto_4a_ga1.patch`) | **ZERO** — same builder, same json, same ~70-byte timing doc, CSS still unrun |
| **PROTO-2-mckeeman-repro (22:40)** | **Independent reproduction** + cold-run + source-audit of `exec_apply_fn` | **NARROW** — cold build + non-stub source-audit; SAME 12 kinds, SAME closed HostFold, SAME 87 tests |

**The within-lock reading:** four probes this pass touched exactly two gates (4A-G-A1, 4C-G-C0). The marginal two
re-measured the same narrower claims the first two settled. This is good *hygiene* — a SPEC-ONLY-for-four-passes
claim becoming a twice-measured anchor (4A) and a once-audited McKeeman gate becoming a cold-reproduced +
source-audited one (4C) genuinely retire the single-run/fingerprint-cache/pass-through-stub sub-doubts. **But the
SHARPEST uncertainties both original probes sidestepped — 4A's real-parse-that-API reification across the
`parse_that↔bbnf_ir` crate seam, and 4C's arbitrary opaque closure + the canada cross-language crossing — remain
untouched across all FOUR probes.** Reproduction is not coverage; re-running one gate is not a "clean pass" of the
family; and the convergence bar (two consecutive clean passes + zero enumerated gaps) is structurally unreachable
while the artifact of record is a probe-generation behind its own evidence.

**And the spec's staleness compounds, in the FAVORABLE direction.** FAMILY-SPECS.md (18:43) still carries 4A's
G-A1 as "**0% built for a FOURTH pass … Zero running evidence, four passes**" (§4A gate G-A1) and 4C's G-C0 as
"**the REAL gate, UNTESTED … the family's central untested-of-record claim**" (§4C). Both are now doubly false:
built + PASS + reproduced (4A), discharged + reproduced + source-audited (4C). The REGISTRY (16:24) is older still.
GESTALT-P/Ω will mis-DAG LOCK-4 if it reads the 18:43 spec as the state. **Every family % below is computed
against the 22:40/22:42 reruns, not the 18:43 spec.**

**Base-law (XL-H) discipline holds and is correct.** Both reruns base-pin `ce51c36fc`; ancestry verified; the
mckeeman-repro diffed its worktree against the patch of record (**0 content-line divergences across 1253 lines** —
a provenance check that retires wrong-artifact-risk, NOT a coverage check). I re-confirmed on HEAD `3f2350f15`
(newer than the critiques' `06e70053f`) that both builds remain **WORKTREE-ONLY** (`rg ApplyFn`/`rg HostClosure`=0).
"PASS" reads as *byte-identical-fuse / correct-under-live-differential in a scratch worktree*, NOT *landed of record*.

---

## 4A — CLOSURE-TO-GRAPH · **78%** (was 80 at 19:57 — the rerun is robustness-not-coverage; the sharpest uncertainty is untouched by FOUR probes)

**The load-bearing advance stands and is now twice-confirmed.** G-A1 — 4A's single novel claim, SPEC-ONLY for
four passes — ran and PASSED, and PROTO-1-4A-GA1-RERUN reproduced it byte-for-byte at the same base-pin (timing
1.0009 this run vs 0.9981 prior, both in `[0.98,1.02]`, opposite sides of 1.0 — the same-program noise signature).
A combinator-style smart-constructor builder emitting `IrNode` for the json grammar flows through `fuse_single_use`
+ `fuse_token_dispatch` to a graph byte-identical to the grammar-text reference (0/10 pre- AND post-fuse), an
identical 37-node interned DAG, twitter-parity (offset 631514). The elegant-reduction trap flagged across passes
does not bite *this test*. The reproduction genuinely retires the single-run flakiness concern.

**Why I dock 2 points rather than hold: the rerun spent a probe-slot on the LOW-risk half twice, and the sharpest
within-lock uncertainty is untouched.** As critic-1 weighting "did the prototype test the sharpest uncertainty," I
must name that all four probes aimed one ring inside the bullseye:

1. **The "combinator front" is a bbnf-side 85-LOC `IrNode` builder, NOT parse-that's real combinator API — still,
   after a reproduction.** The fuse passes key on the `IrNode` enum discriminant, so provenance-agnosticism was
   near-certain a priori (the *lower-risk* half). The sharpest 4A uncertainty — bridging parse-that's shipped
   `SmallBox<dyn ParserFn>` surface (`parse_that/src/parse.rs:61,74`) to emit `IrNode` **across the
   `parse_that↔bbnf_ir` crate seam** (the exact RF-1 category-error step, and the cycle the VM's `Dfa` dep makes
   live) — was not exercised by either original OR either rerun. Reproducing the provenance-agnostic result a
   second time adds robustness, zero movement on the real-API claim.
2. **CSS arm unrun; `fuse_token_dispatch` is a no-op on json.** BRIEF A asked "JSON/CSS cold-paired ≥0.98×." CSS
   never ran, in either the original or the rerun. json has no `@token` rules (`DirectiveSet::empty()`), so
   `fuse_token_dispatch` early-returns on both fronts — its inheritance is proven *trivially*, not substantively.
   `fuse_single_use` alone is load-bearing; the `Map{fn_id}`/`@pretty`/token-dispatch inheritance is unexercised.
3. **The ≥0.98× timing gate is near-vacuous, and reproducing it is DOUBLY near-vacuous.** Byte-identical IR ⇒
   identical bytecode ⇒ the ratio *cannot* meaningfully deviate; the probe itself calls it "a confirmation, not the
   crux." A near-vacuous gate re-run is not a second independent datum — it is the same tautology measured twice.
4. **Node-ratio thesis is a repo aggregate, not an invariant** (PROTO-2 PART 1: 1.857% repo / **4.84% math FAIL**,
   K=13.78) — 4A reads as "skeleton with opaque leaves" exactly on value.js, LOCK-8's flagship consumer.

**BLOCKED-candidate? NO.** Source-front role coherent; terminal role refuted (RF-1). The residual is a build task
(point extant passes at a real-API front + run CSS), not a missing primitive.

**Open gaps (enumerated):**
1. **Real-parse-that-API reification across the crate seam is UNBUILT** — all four probes used a bbnf-side
   `IrNode` builder; the RF-1-hard step (parse-that `SmallBox<dyn ParserFn>` surface → `IrNode`) is where the
   sharpest 4A uncertainty lives, unmoved.
2. **CSS arm of G-A1 unrun; `fuse_token_dispatch` proven only trivially** (json has no `@token`; early-return on
   both fronts) — `fuse_single_use` alone fired.
3. **Reify→serialize→SAME-emitter→graph-of-record parity (open-Q2) unrun** — a LOCK-3 tower-parity obligation
   (XL-B), not self-dischargeable; zero evidence. PROTO-1 proved the grammar-TEXT→emitter path; the
   runtime-graph→SAME-emitter path — the one 4A's source-front role actually rides — never ran.
4. **Node-ratio FAILS on LOCK-8's flagship consumer** (4.84% math > 2%) — repo aggregate, not a per-grammar
   invariant.

---

## 4B — COMPTIME-FUSION · **72%** (unchanged — NO probe touched 4B this sub-pass; the gate-that-cannot-fail is on its FOURTH pass unset)

**Neither rerun addressed 4B** (both are 4A/4C). Its disposition is exactly the 19:57 reading, and it is internally
sound and honest: G-B0 PASS (recursive JSON, 0 SmallBox/dyn/fn-ptr, ZST parser — PROTO-1 GATE A); G-B3 PASS
(arbitrary host closure fused as 1 named call-out, `bbnf-ir` compile-proven — PROTO-2 PART 2); G-B1 MEASURED-MIXED
(data 0.70–0.72× WIN, twitter 0.83–0.85× WIN, citm 1.00–1.03× TIE, **canada 1.56× REGRESS**, both windows both
orders). The election lever FAILED (PROTO-1-numeric-leaf: SWAR 8-digit fold inert on canada's short digit runs, the
`*p+8<=len` probe straddles the delimiter and falls to the scalar tail; integer fast-path never fires on all-float
canada). The residual ceiling is **EXOGENOUS** — LOCK-9's `number_simd` 16-byte SIMD stripe (9B) + a LOCK-6/8
padded-input `parse()` contract. The spec's correction from "single lever" to "three-lock-away, cross-lock-unstable
baseline" is correct and creditable. It is the lock's strongest family and closest to "converged core with exact
external gaps."

**The gate-that-cannot-fail is now on its FOURTH pass unset.** G-B4 (firing budget: fuse-arity 8, `try_factor_alt`
at 4 [verified on tree], one box per SCC back-edge, `.rlib`/`dist` + incremental-compile) still has **no measured
number**. A firing budget with no number does not gate the monomorphization-blowup risk it exists to bound
(constitution vi / generated-size-budget). Owed on CSS **and** value.js since pass 1. This is the checklist's
canonical gate-that-cannot-fail, and no probe across three passes has produced its curve.

**Open gaps (enumerated):**
1. **Election is EXOGENOUS** (LOCK-9 9B kernel + LOCK-6/8 padded-input re-arch) — 4B cannot elect inside LOCK-4
   even in principle; canada 1.56× is its measured ceiling until 9B lands; the win is string/structure/small
   ONLY (citm is a TIE, not a win — "beyond-SOTA" is met only there).
2. **Canada-via-padding is SPEC_ONLY** — the SIMD stripe + `get_unchecked` + `PaddedView` re-architecture is an
   unbuilt hypothesis presented as the known ceiling; no artifact (constitution-vii).
3. **G-B4 firing budget: NO number, FOURTH pass** — the canonical gate-that-cannot-fail.
4. **G-B3 rides an unattributed LOCK-1-TypeDesc pre-seed** (`rg HostClosure`=0 on record) whose shape is
   *inconsistent* with the shape the mckeeman probe built (§below); core emission arms (`driver/map.rs` +
   `emit_host_closure` default) are "patch-plausible, NOT cold-built" — the PASS partly rides an uncompiled patch.
5. **XL-E supersession has no PT-tranche home** (constitution-ix debt → GESTALT-P) — owed since pass 1.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · **80%** (was 80 at 19:57 — the mckeeman-repro is the strongest reproduction in the lock; the sharpest WITHIN-lock uncertainty is now twice-discharged + source-audited)

**The best-aimed probe in the lock, now reproduced cold with a source audit.** PROTO-2-live-differential ran the
exact McKeeman gate the 16:03/19:57 CRIT-1 named as 4C's sharpest within-lock uncertainty, and ran it *right*:
because `parse_that = "0.4"` is a first-class dep of `crates/ir` (`Cargo.toml:13`, re-confirmed), parse-that's
SHIPPED closure `.map`/`.many` runs **in the same Rust test binary** as `parse_with_ir`, so the oracle is
parse-that's **actual live return value**, not an author golden. 12/12 census pairs, 0 divergences. **PROTO-2-
mckeeman-repro strengthens this three ways the original could not:** (a) independent reproduction on a clean base
pin, worktree byte-identical to the patch of record; (b) a **cold run** (fresh target dir, 57 crates recompiled,
87/87, 1m19s) rules out a fingerprint-cache artifact that the original's incremental build could not; (c) a
**source-level audit** of `exec_apply_fn` (`apply_fn.rs:94–104`) confirming the `HostFold` is genuinely
*evaluated* (parses child span → f64 → `HostFold::eval` → pushes F64/U32), NOT in the `=> child` pass-through arm
(lines 107–108). This retires the "is the non-stub HostClosure actually a pass-through?" doubt both prior critiques
raised — full within-lock credit for aim and for the reproduction discipline.

**But the reproduction did NOT touch any of the four other open gaps, and one qualification the repro's own text
concurs with is load-bearing:**

1. **The corpus is HAND-SELECTED projection kinds, not a real compiled grammar — unchanged, and the repro
   explicitly concurs with the scope.** Neither probe compiles `css/pretty.bbnf` / `google-sheets.bbnf` through
   the front-end (rationale: hand-mirroring in parse-that combinators would "smuggle an author transcription back
   in" — a defensible call the repro re-affirms). G-C0 is discharged against **12 author-chosen projection kinds**,
   twice. The oracle is live (the McKeeman fix is real and reproduced); the *corpus* is author-selected (a milder
   residue of the same selection concern). A real grammar compiled through the actual lowering, crossed full-tree,
   is unrun.
2. **`HostFold` is a CLOSED set of two numeric folds — NOT the arbitrary opaque closure — after a reproduction.**
   The repro is explicit: "a closed set of two numeric folds … it does not claim to be the general arbitrary-closure
   evaluator." The genuinely irreducible residue — value.js's **3 easing closures that RETURN a host function**
   (`easing.ts:124/142/153`, `EasingFunction = (progress)=>number`, research-D Q2) — is still unevaluated. A numeric
   fold is MapExpr-adjacent, not the `Opaque(HostFnId)` + `Value::Host` inhabitant the hard case demands. "Non-stub
   HostClosure" is discharged (and now source-audited) for a numeric fold; the sharpest opaque case is untested.
3. **G-C1 (canada TS crossing) STILL UNRUN; G-C4 STILL UNFIRED.** The differential runs **in-process in Rust** —
   it is NOT a TS re-decode. The cross-language behavioral-iso over the six R3 corpora incl. **canada** (the
   mandatory adversarial member where PROTO-1 shows the f64 leaf diverges) never ran — DAG-blocked on porting
   `parse_that::regex::Dfa` to TS (`bytecode.rs:184/238`, re-confirmed live on HEAD). G-C4 wire-contract idempotence
   still fired only on `serde_json`, not `rmp_serde::to_vec_named`; the born-RED externally-tagged-reorder guard
   never fired on its shipping bus. Both probes explicitly make "No perf claim" — correct (refutation-law-clean),
   but it also means the cross-language iso, the family's whole cross-lock raison d'être, advanced ZERO this pass.
4. **The 87 "passing" tests are the SAME suite re-run.** 10 `live_differential` + 8 `apply_fn_oracle` + 69
   pre-existing `vm` tests. The repro's "87 passed" is not new coverage; the 12 census pairs are the identical 12.
   The build is worktree-only (`rg ApplyFn`=0 on `3f2350f15`).

**Net for 4C:** G-C0 (Rust-side semantic correctness) is **discharged, reproduced cold, and source-audited** — the
lock's strongest single within-lock result. G-C1 (canada cross-language), G-C4 (rmp idempotence), the arbitrary
opaque closure, and a real-grammar full-tree differential are **open**, none on record. Convergence-of-record not
reached; correctness-under-live-differential is, robustly.

**BLOCKED-candidate? NO** — BANKED as the interpret/iso-oracle lane (measured-refuted twice as a fast terminal:
switch-VM −22%, interpret 0.021–0.46×). G-C1/G-C4 are cheap (a ~200-LOC TS decoder + a differential test); no
missing primitive.

**Open gaps (enumerated):**
1. **Corpus is hand-selected projection kinds** — no real projection-dense grammar (`css/pretty.bbnf`) compiled
   through the front-end and crossed full-value-tree (reproduced against the same 12 kinds, not widened).
2. **Arbitrary opaque closure UNEVALUATED** — `HostFold` is a closed 2-fold set; value.js's 3 closure-RETURNING
   easing parses (the true `Opaque(HostFnId)`/`Value::Host` residue) are still not evaluated.
3. **G-C1 canada / full-corpus TS crossing UNRUN** — DAG-blocked on the `Dfa` TS port; the cross-language iso half
   (constitution-iii, 4C's cross-lock reason to exist) never advanced this pass.
4. **G-C4 msgpack idempotence UNFIRED** on `rmp_serde::to_vec_named` (the actual bus); born-RED reorder guard
   never fired where it ships.
5. **Value plane WORKTREE-ONLY** (`rg ApplyFn`=0 on HEAD); ISA home is LOCK-3's (XL-B), un-ratified; the
   internal-descriptor (→LOCK-3) vs host-closure (→LOCK-7) split correctly exported.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **70%** (honestly BLOCKED/split; DAG-bounded — and its tier-1 barrier-dissolution is a SOURCE COUNT, unbuilt)

**Sound disposition, unchanged in kind.** Terminal role refuted three horns (i = full TS grammar-engine port =
primitive of equal-or-greater difficulty → BLOCKED-candidate AND XL-D retires the need; ii = PROVE-14 wasm
full-walk 0.91× LOSS on exactly the value-producing workload; iii = shared decoder −22% on V8). Gate-0's regex arm
severed mechanically (`pub use bbnf_regex::*`); **the VM `Dfa` arm remains LOCK-5-gated** (`bytecode.rs:184/238`,
re-confirmed on HEAD). research-D's split-the-barrier finding (44/84 ~52% of latex-paper non-projection sites
recoverable as `Record`/`Array`/`Select` data; opaque residue = exactly 3 value.js easing closures) is a genuine
sharpening, and the spec correctly self-guards against "4D dominates on parsimony ⇒ elect."

**Within-lock scrutiny — two things the whole thesis rests on remain unbuilt after four probes:**

1. **The tier-1 barrier-dissolution ("~52% dissolves as data") is a SOURCE CENSUS, not a built artifact.**
   research-D's own prototype obligation 2 — *land* `MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record`,
   re-lower the 44 latex-paper object-literal projections, assert they round-trip through `constant_fold` and
   serialize — was **NOT run by any probe this pass.** The mckeeman probes built `HostClosure`/`HostFold`, not the
   three tier-1 data nodes. So "44 of 84 recoverable as analyzable data" is a *count of source sites*, not a
   compiled proof they lower to `constant_fold`-total serializable nodes. This is a SPEC_ONLY claim the 19:57 CRIT-1
   folded into "compile-plausible"; it deserves the sharper label — the 3 new nodes were never landed or
   round-trip-tested.
2. **The irreducible residue — the closure-RETURNING slice — is neither built nor evaluated.** 4D's whole reason
   to need a residue tier is `FnDescriptor::Opaque(HostFnId)` + `Value::Host(HostHandle)` letting a parse *carry* a
   closure it cannot introspect. PROTO-2-live-differential/repro evaluated `HostFold` (numeric folds), the numeric
   slice of tier-2 — MapExpr-adjacent, not the closure-returning case. value.js's 3 easing closures remain the exact
   case no probe has executed. 4D's "split the barrier, `Opaque` carries only the residue" is proven for neither the
   tier-1 data nodes (source-count-only) nor the closure slice of tier-2 (unevaluated) — only the *numeric* slice of
   tier-2 has running evidence. This is not a defect in 4D's reasoning; it is the family's sharpest remaining
   within-lock uncertainty, co-owned with LOCK-7.

**BLOCKED-candidate? YES, split.** 4D-as-fast-shared-runtime is BLOCKED-candidate (dead by the missing-primitive
rule AND XL-D); 4D-as-shared-ownership is DAG-BLOCKED on LOCK-5 + LOCK-7. Do NOT carry "4D dominates on
parsimony ⇒ elect" — dominance is conditional and its Rust runtime is a measured loss.

**Open gaps (all predecessor-owned / report-only per Q8):**
1. **INCOMPAT-1 (grammar-IR sharing) → LOCK-5:** shared type vs lawful functor; DAG-blocking. Gate-0's VM `Dfa`
   arm is a LOCK-5 extraction (confirmed on tree). *(Note: CRIT-2 flags that the spec's "5A vs 5B" framing collides
   with XL-C's RETIRED-5B ledger — a cross-lock re-label owed; I concur it is not a live within-lock fork.)*
2. **Tier-1 barrier-dissolution is a SOURCE COUNT, unbuilt** — the three `Record`/`Array`/`Select` data nodes
   were never landed or round-trip-tested (research-D obligation 2 unrun); "~52% dissolves as data" is SPEC_ONLY.
3. **HostClosure/`Value::Host` closure-RETURNING residue → LOCK-7:** the 3 value.js easing closures are un-built
   and un-evaluated (shared with 4C gap 2); the numeric `HostFold` is a substitute for the hard case.
4. **Node-ratio thin AND FAILS on value.js** (1.857% repo / 4.84% math) — fusion-inheritance safe in aggregate,
   fragile on LOCK-8's consumer (shared with 4A gap 4).
5. **Value-plane ISA home → LOCK-3 (XL-B) FIRST** (internal-descriptor vs host-closure split); un-ratified, built
   worktree-only under a LOCK-4 probe. `FnDescriptor::HostClosure` pre-seed unattributed + INCONSISTENT across two
   probes (§below); constitution-v zero-runtime-cost untested (shared with 4B gap 4).

---

## THE TWO-SHAPES HostClosure INCONSISTENCY (carried within-lock; homing is CRIT-2's)

Three PASS/discharge claims across the lock — G-B3 (4B), G-D-GAP1 (4D), and "non-stub HostClosure evaluated"
(4C) — ride a `FnDescriptor::HostClosure` diff that is `rg HostClosure`=0 of record. The within-lock defect,
independent of homing: **the diff is not one shape.** PROTO-2 PART 2 built `HostClosure{fn_id, arg:TypeDesc,
ret:TypeDesc}` (an opaque emitter call-out); PROTO-2-live-differential/mckeeman-repro built a *different*
`HostClosure{kind: HostFold, coeffs: Vec<f64>}` (a closed, evaluable, NON-opaque numeric fold). Two incompatible
type-plane shapes wearing one name, both worktree-only, and **neither is the arbitrary opaque closure the hard
case demands** (research-B/D's `HostClosure{fn_id, arg_types, ret_type}` → host `fn_table` + `Value::Host` — built
by no probe). Within-lock consequence: the lock's own three gates cannot all be reading the same artifact; the
name "HostClosure discharged" spans two artifacts that were each landed to make a *different* gate go green. This
is not fatal to the mechanism; it is fatal to reading the three gates as jointly converged on one type-plane truth.

---

## CROSS-CUTTING FAILURE-MODE AUDIT (within-lock weight)

- **Stale-evidence / reproduction-in-place-of-coverage — THE GOVERNING HIT, now TWO turns deep.** The 18:43 spec
  froze before its own briefs ran (turn 1); the 22:40/22:42 probes then RE-MEASURED the same two gates rather than
  attacking the sharpest uncertainties (turn 2). The spec undersells 4A ("0% built, four passes" — now PASS+reproduced)
  and 4C ("REAL gate untested" — now discharged+reproduced+audited). Structural, not incidental: two consecutive
  clean passes is unreachable while (a) synthesis is a probe-generation behind, and (b) marginal probe-budget buys
  reproduction of settled narrower claims instead of the unbuilt sharp ones.
- **Elegant-reduction trap — 4A hit DOWNGRADED, not cleared, and NOT further cleared by the rerun.** "The passes
  already exist" is no longer zero evidence — but the probe (and its rerun) used a bbnf-side builder; the genuinely
  hard step (real parse-that API → `IrNode` across the crate seam) is still unbuilt. The reduction contains a
  *smaller* unbuilt step, not none; reproduction did not shrink it.
- **Gate-that-cannot-fail — TWO hits.** (i) G-B4 firing budget: no number, FOURTH pass — the canonical one,
  untouched by either rerun. (ii) PROTO-1-4A-GA1's ≥0.98× timing gate is near-vacuous (byte-identical IR ⇒
  identical bytecode); reproducing it (1.0009 after 0.9981) re-measures a tautology — honestly labeled by the
  probe, but not an independent gate on either run.
- **Oracle self-forgery / faked golden — RETIRED for 4C's closed surface, and now source-audited.** The live
  in-process `.map` oracle + the mckeeman-repro's audit of `exec_apply_fn:94–104` (evaluated, not the `=>child`
  arm) genuinely fix the McKeeman trap for the closed-descriptor + 2-fold surface. Residue: the *corpus* is
  author-selected projection kinds (a milder selection concern), and the hardest case (arbitrary opaque closure)
  is a closed-numeric-fold substitute, not the real thing (masked-fallback, honestly disclosed by the probe).
- **Masked fallback — surviving.** "Non-stub HostClosure evaluated" (both mckeeman probes' VERDICT) is a closed
  numeric fold standing in for the arbitrary opaque closure — a narrower artifact functioning as a green light for
  a gate (the closure-returning easing case) the hard case does not clear. The repro's source-audit confirms the
  *fold* is evaluated but re-confirms it is a *closed* fold — the mask is disclosed, not removed.
- **SPEC_ONLY anchoring a claim — THREE hits.** (i) 4B's canada-via-padded-input, unbuilt, presented as the known
  ceiling. (ii) G-B3's core emission arms "patch-plausible, not cold-built." (iii) **4D's tier-1 "~52% dissolves
  as data" is a source count** — the three `Record`/`Array`/`Select` nodes were never landed or round-trip-tested.
- **Sharpest-uncertainty aim — SPLIT and REPRODUCED off-target.** PROTO-2-live-differential/repro hit 4C's
  sharpest within-lock uncertainty squarely (VM vs live `.map`) — model probe design, now robustly reproduced.
  The 4A probe + rerun hit *a* sharp uncertainty (fuse provenance-agnosticism) but re-measured it rather than
  advancing to THE sharpest (real-API reification, CSS).
- **Consumer-less substrate (constitution viii / Y.13) — CLEAN.** VM has live consumers (DAP adapter, JSON bench);
  the value plane's consumers are the iso-oracle + interpret/LSP lane (all outside LOCK-4, but present).
- **Isomorphism (constitution iii) — half-tested, UNMOVED this pass.** 4C's "iso by construction (parser crosses
  as data)" is proven + reproduced for the value-plane semantics **in-process in Rust** (12/12, twice); the
  **cross-language** half (TS re-decode over canada) is still unrun — G-C1 DAG-blocked on the `Dfa` port. The
  lock's strongest iso claim advanced ZERO on the crossing this pass.
- **Zero-runtime-cost (constitution v) — UNTESTED for the HostClosure pre-seed(s)** — two shapes, both
  worktree-only, neither verified byte-identical-emit.
- **Base-law (XL-H) — CLEAN, re-verified on a newer HEAD.** Both reruns base-pin `ce51c36fc`; I re-confirmed
  worktree-only status on `3f2350f15` (newer than the critiques' `06e70053f`).

---

## NEXT-PASS DIRECTIVES (within-lock)

1. **Re-synthesize folding the 22:40/22:42 reruns, and STOP re-measuring settled gates.** 4A's G-A1 is built +
   twice-reproduced; 4C's G-C0 is discharged + cold-reproduced + source-audited. Update every % and net disposition;
   the "four passes unbuilt" (4A) and "REAL gate untested" (4C) framings are doubly dead. Spend the next probe on an
   UNBUILT sharp uncertainty, not a third reproduction.
2. **4A — close the two within-lock narrowings the reruns did NOT.** (a) Wire parse-that's REAL combinator API (not
   a bbnf-side builder) to emit `IrNode` across the crate seam and re-run byte-identity — the RF-1-hard step. (b)
   Run the CSS arm + a grammar with `@token`/`Map{fn_id}` so `fuse_token_dispatch` fires substantively.
3. **4C — cross canada and evaluate a TRUE opaque closure.** Port the TS `Dfa` (or restrict to LOCK-5's certified
   grammar-lowerable class) and run G-C1 over six corpora incl. canada; fire G-C4 on `rmp_serde::to_vec_named`.
   Evaluate at least one **closure-RETURNING** `Opaque(HostFnId)` (value.js easing), not a numeric HostFold.
4. **4C/4D — differential on a REAL compiled grammar.** Compile `css/pretty.bbnf` through the actual front-end and
   cross its full value tree vs live `.map`, replacing the 12 hand-selected projection kinds.
5. **4D — BUILD the tier-1 nodes.** Land `MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record`; re-lower
   the 44 latex-paper object-literal sites; assert `constant_fold`-total + serializable. Convert "~52% dissolves as
   data" from a source count into a compiled artifact.
6. **4B — G-B4's number or the named ceiling.** Produce the firing-budget curve (`.rlib`/`dist` + incremental
   vs fuse-arity) on CSS + value.js, or record it un-gateable and carry canada 1.56× as the measured exogenous
   ceiling. Pick ONE `HostClosure` shape of record, attribute + home it (LOCK-1 TypeDesc), verify constitution-v.

**Per-family within-lock convergence (critic-1, pass-3 re-cut v2): 4A 78% · 4B 72% · 4C 80% · 4D 70% (BLOCKED-split).**
No family converged. The reruns bought robustness (reproducibility retired, non-stub-evaluation source-audited),
not coverage — every sharpest within-lock uncertainty they sidestepped remains unbuilt. Return LOCK-4 as a
rigorously converged CORE with its exact enumerated gaps, never as a terminal.
