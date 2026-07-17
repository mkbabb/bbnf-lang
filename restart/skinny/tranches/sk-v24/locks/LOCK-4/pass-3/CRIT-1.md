# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-1 (adversarial critic 1 of 2, within-lock weighting) — RE-CUT

*Read: PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H binding), FAMILY-SPECS.md (18:43 re-synthesis),
research-{A,B,C,D,priorart}, PROTO-1.md, PROTO-1-numeric-leaf.md, PROTO-2.md, PROTO-2-vmvalue.md, and — decisively —
the TWO probes that postdate the FAMILY-SPECS: **PROTO-1-4A-GA1-combinator-graph.md (19:40)** and
**PROTO-2-live-differential.md (19:43)**. Tree-of-record spot-checks run on HEAD `06e70053f`
(descendant of base-pin `ce51c36fc`). Critic-1 weights internal soundness of each family spec and whether the
prototypes tested the SHARPEST uncertainty. This re-cut supersedes the 16:03 CRIT-1, which was written against the
15:22 draft and cannot have seen the 18:43 re-synthesis, the 19:40 probe, or the 19:43 probe.*

---

## THE GOVERNING WITHIN-LOCK FINDING — THE SYNTHESIS FROZE BEFORE ITS OWN BRIEFS RAN, **AGAIN**

The 16:03 CRIT-1's headline was that the 15:22 FAMILY-SPECS was stale against its own two follow-on briefs
(numeric-leaf FAIL, vmvalue BUILT). The 18:43 re-synthesis folded that correction — and **then committed the
identical error**. It closes with "PROTOTYPE BRIEFS FOR THE NEXT PASS": **BRIEF A** (4A's `fuse(Graph)→Graph`
byte-identity) and **BRIEF B** (4C's G-C0 REAL gate + one non-stub HostClosure). **Both briefs then ran THIS
pass, after the spec froze:**

- **PROTO-1-4A-GA1 (19:40)** ran BRIEF A → **PASS.** 0/10 pre-fuse divergences, 0/10 post-fuse, DAG 37==37
  identical, parse parity on twitter (631514), timing 0.9981.
- **PROTO-2-live-differential (19:43)** ran BRIEF B → **PASS.** VM `Op::ApplyFn` byte-identical to parse-that's
  **LIVE** `.map`, 12/12 census pairs 0 divergences; one non-stub `HostClosure` (HostFold Lerp/ChannelScale)
  **evaluated**, not forwarded.

So the 18:43 spec's convergence framing is stale in the **favorable** direction this time: it carries 4A's
G-A1 as "**0% built for a FOURTH pass … zero running evidence, four passes**" (now false — built + PASS), and
carries 4C's G-C0 as "the REAL gate, **UNTESTED** … the family's central untested-of-record claim" (now
discharged for the Rust-side semantic gate). **This is a recurring PROCESS failure**, not a one-off: the
synthesis keeps closing before its sharpest levers fire, so every "next-pass brief" is executed same-pass and
never folded. Two consecutive clean passes (the convergence bar) is unreachable while the artifact of record
is always one probe behind its own evidence. **Every family % below is computed against the 19:40/19:43 probes,
not the 18:43 spec.** The mandatory next-pass act is a re-synthesis that folds these two PASSes and STOPS
emitting briefs it will then execute itself.

**Tree-of-record accuracy (creditable, verified on `06e70053f`):** `rg ApplyFn crates/ir/src`=0,
`rg HostClosure crates/core crates/ir`=0, `bytecode.rs:184/238` `parse_that::regex::Dfa` dependency live,
`FnDescriptor` still 6 variants (no HostClosure), `Expr` the sole user-facing arm. **Both 19:40/19:43 builds are
WORKTREE-ONLY; nothing landed of record.** The spec's "worktree-built ≠ on record" discipline holds and is
correct — the uplift is buildability + correctness-under-differential, not convergence-of-record.

---

## 4A — CLOSURE-TO-GRAPH · **80%** (was 68/70 — the four-passes gate is DISCHARGED, with three real narrowings)

**The load-bearing change.** G-A1 — 4A's single novel claim, SPEC-ONLY for four passes — **ran and PASSED**
(PROTO-1-4A-GA1). A combinator-style smart-constructor builder emitting `IrNode` for the json grammar flows
through `fuse_single_use` + `fuse_token_dispatch` to a graph **byte-identical** to the grammar-text reference,
pre- AND post-fuse (0/10 each), with an identical 37-node interned DAG sequence and twitter-parity. The
elegant-reduction trap the 16:03 CRIT-1 flagged as 4A's "whole risk" **does not bite on this test.** This is a
genuine advance and it retires the spec's gap #1 as written.

**But the probe tested a NARROWER claim than 4A's sharpest uncertainty — three narrowings, all within-lock:**

1. **The "combinator front" is a bbnf-side 85-LOC IrNode builder, NOT parse-that's real combinator API.** The
   probe proved the fuse passes are **provenance-agnostic** — that a faithful `IrNode` producer inherits them
   byte-identically. That was the *lower-risk* half: the passes key on the `IrNode` enum discriminant, so
   provenance-agnosticism was near-certain a priori. The *sharpest* uncertainty — bridging parse-that's actual
   runtime `SmallBox<dyn ParserFn>` surface (`parse.rs:74`) to emit `IrNode` across the `parse_that↔bbnf_ir`
   crate seam, the exact bit the RF-1 category-error discussion calls hard — was **not exercised.** 4A's real
   claim ("parse-that's API reifies to this graph") is one wiring step short of proven.
2. **Mild circularity in the oracle.** The builder was authored to match the lowering's conventions (`?w`→
   `OptionalWhitespace`, `<<`/`>>`→`Skip`/`Next` left-nesting, `,` looser than `>>`) and the probe's own text
   says these "matched on the **first build**." The test confirms an author *can* transcribe the grammar into a
   builder that lands on the identical graph — strong evidence, but it is "faithful transcription reproduces the
   lowering," not "combinator semantics *necessarily* land there." The pre-fuse=0 result is the substantive
   guard, honestly framed; the residual is that the conventions were matched by hand, not derived.
3. **Coverage holes vs the brief.** BRIEF A asked "JSON/CSS cold-paired ≥0.98×." **CSS was not run.**
   `fuse_token_dispatch` is a **no-op on json** (no `@token` rules) — only `fuse_single_use` actually fired, so
   the token-dispatch and `Map{fn_id}`/`@pretty` inheritance is proven *trivially* (identical early-return), not
   substantively. Timing (0.9981) was on a ~70-byte doc because the interpreter bars N≥200 on twitter, and — by
   the probe's own admission — is "a confirmation, not the crux" (byte-identical IR ⇒ identical bytecode ⇒ the
   ratio *cannot* meaningfully deviate). The ≥0.98× timing arm is therefore **near-vacuous** once byte-identity
   holds; it is not an independent gate.

**Open gaps (enumerated):**
1. **Real-API reification unbuilt.** parse-that's shipped combinator surface → `IrNode` across the crate seam
   was NOT wired; the probe used a bbnf-side builder. The sharpest 4A uncertainty is softened, not closed.
2. **CSS arm of G-A1 unrun; `@token`/`Map{fn_id}`/`@pretty` inheritance proven only trivially** (json has none;
   `fuse_token_dispatch` early-returned). The load-bearing pass here was `fuse_single_use` alone.
3. **Reify→serialize→SAME-emitter→graph-of-record parity (open-Q2) unrun** — a **LOCK-3 tower-parity obligation
   (XL-B)**, not self-dischargeable; correctly exported, still zero evidence.
4. **Node-ratio thesis is a repo aggregate, not an invariant** (1.857% repo / **4.84% math FAIL**, PROTO-2 PART
   1) — 4A reads as "skeleton with opaque leaves" exactly on value.js, LOCK-8's flagship consumer.

---

## 4B — COMPTIME-FUSION · **72%** (unchanged in substance — NO new probe this round touched 4B; the gate-that-cannot-fail persists)

**Neither 19:40/19:43 probe addressed 4B.** Its disposition is exactly the 16:03 reading, and it is internally
sound and honest: G-B0 PASS (recursive JSON, 0 box/dyn/fn-ptr), G-B3 PASS (host closure fused as 1 named
call-out), G-B1 MEASURED-MIXED (data/twitter WIN, citm TIE, **canada 1.50–1.60× REGRESS**). The election lever
FAILED (PROTO-1-numeric-leaf; canada 1.56× unchanged), and the residual ceiling is **EXOGENOUS** — LOCK-9's
`number_simd` (9B) + a LOCK-6/8 padded-input `parse()` contract. The spec's correction from "single lever" to
"three-lock-away, cross-lock-unstable baseline" is correct and creditable.

**The gate-that-cannot-fail is now on its FOURTH pass unset.** G-B4 (firing budget: fuse-arity 8,
`try_factor_alt` at 4, one box per SCC back-edge, `.rlib`/`dist` + incremental-compile) still has **no measured
number**. A firing budget with no number does not gate the monomorphization-blowup risk it exists to bound
(constitution vi, generated-size). Owed on CSS **and** value.js since pass 1. This is the checklist's canonical
gate-that-cannot-fail, and no probe has ever produced its curve.

**Open gaps (enumerated):**
1. **Election is exogenous** (LOCK-9 9B kernel + LOCK-6/8 padded-input re-arch) — 4B cannot elect inside LOCK-4
   even in principle; canada 1.56× is its measured ceiling until 9B lands.
2. **Canada-via-padding is SPEC_ONLY** — the SIMD stripe + `get_unchecked` + `PaddedView` re-architecture is an
   unbuilt hypothesis presented as the known ceiling; no artifact.
3. **G-B4 firing budget: no number, fourth pass.** Gate cannot fail as written.
4. **G-B3 rides an unattributed LOCK-1-TypeDesc pre-seed** (`rg HostClosure`=0 on record); core emission arms
   (`driver/map.rs` + `emit_host_closure` default) are "patch-plausible, NOT cold-built" — the PASS partly rides
   an uncompiled patch (constitution vii; disclosed but load-bearing).
5. **"Beyond-SOTA" met only on string/structure/small** — citm is a TIE, not a win.
6. **XL-E supersession has no PT-tranche home** (constitution-ix debt → GESTALT-P).

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · **80%** (was 66/70 — G-C0's REAL gate is DISCHARGED; the probe hit the sharpest uncertainty squarely)

**The strongest single advance in the lock this pass, and correctly aimed.** PROTO-2-live-differential ran the
exact McKeeman gate the 16:03 CRIT-1 named as 4C's sharpest untested uncertainty, and ran it *right*: because
`parse_that = "0.4"` is a first-class dependency of `crates/ir`, parse-that's SHIPPED closure `.map`/`.many`
runs **in the same Rust test binary** as `parse_with_ir`, so the oracle is parse-that's **actual live return
value**, not an author golden. **12/12 census pairs, 0 divergences.** The faked-golden trap is genuinely
retired for the closed-descriptor surface. And the pass-through stub is retired: `HostFold::{Lerp,ChannelScale}`
is **evaluated** inside `exec_apply_fn` (proven by the `F64`/`U32` discriminant a forward would violate),
matching both the closure core and live `.map` including the `600→255` clamp branch. This is precisely the
right probe for the family's sharpest uncertainty — full credit for aim.

**But three within-lock qualifications sit on exactly what drove the rise:**

1. **The corpus is HAND-SELECTED projection kinds, not a real compiled grammar.** The probe explicitly does NOT
   compile `css/pretty.bbnf` / `google-sheets.bbnf` through the front-end (rationale: hand-mirroring them in
   parse-that combinators would "smuggle an author transcription back in" — a defensible call). Instead it
   reproduces "the projection *kinds* those grammars are dense in." So G-C0 is discharged against **12
   author-chosen projection kinds**, not a real projection-dense grammar's full value tree. The oracle is now
   live (the McKeeman fix is real), but the *corpus* is author-selected — a milder residue of the same
   selection concern. A real grammar compiled through the actual lowering, crossed full-tree, is unrun.
2. **`HostFold` is a CLOSED set of two numeric folds — NOT the arbitrary opaque closure.** The probe is explicit:
   "it is not (nor claims to be) the general arbitrary-closure evaluator." The genuinely irreducible residue —
   value.js's **3 easing closures that RETURN a host function** (`easing.ts:124/142/153`, `EasingFunction =
   (progress)=>number`, per research-D Q2) — is still **unevaluated**. A numeric fold is arguably
   MapExpr-adjacent, not the `Opaque(HostFnId)` + `Value::Host` inhabitant the hard case demands. So "non-stub
   HostClosure" is discharged for a numeric fold; the sharpest opaque case (closure-returning parse) remains
   stubbed at the arbitrary level.
3. **G-C1 (the canada TS crossing) is STILL UNRUN, and G-C4 is STILL UNFIRED.** The differential runs
   **in-process in Rust** against parse-that's `.map` — it is NOT a TS re-decode. So the cross-language
   behavioral-iso over the six R3 corpora **incl. canada** (the mandatory adversarial member; PROTO-1 shows the
   f64 leaf is where routes diverge) never ran — still DAG-blocked on porting `parse_that::regex::Dfa` to TS
   (`bytecode.rs:184/238`, tree-confirmed). G-C4 wire-contract idempotence still fired only on `serde_json`, not
   `rmp_serde::to_vec_named`; the born-RED externally-tagged-reorder guard has never fired on its shipping bus.
   And the whole build is **worktree-only** (`rg ApplyFn`=0 on `06e70053f`).

**Net for 4C:** G-C0 (Rust-side semantic correctness) is **discharged** — a real +uplift and the right probe.
G-C1 (canada cross-language), G-C4 (rmp idempotence), the arbitrary opaque closure, and a real-grammar full-tree
differential are **open**, and none is on record. Convergence-of-record is not reached; correctness-under-live-
differential is.

**Open gaps (enumerated):**
1. **Corpus is hand-selected projection kinds** — no real projection-dense grammar (`css/pretty.bbnf`) compiled
   through the front-end and crossed full-value-tree.
2. **Arbitrary opaque closure unevaluated** — `HostFold` is a closed 2-fold set; value.js's 3 closure-RETURNING
   easing parses (the true `Opaque`/`Value::Host` residue) are still not evaluated.
3. **G-C1 canada / full-corpus TS crossing UNRUN** — DAG-blocked on the `Dfa` TS port; latent Dfa-vs-`RegExp`
   divergence risk (research-C R5) unowned.
4. **G-C4 msgpack idempotence unfired** on `rmp_serde::to_vec_named` (the actual bus); the born-RED reorder
   guard never fired where it ships.
5. **Value plane worktree-only** (`rg ApplyFn`=0); ISA home is **LOCK-3's (XL-B)**, un-ratified; internal-
   descriptor (→LOCK-3) vs host-closure (→LOCK-7) split correctly exported to GESTALT-Ω.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **70%** (honestly BLOCKED/split; DAG-bounded, not internally unsound)

**Sound disposition, unchanged in kind.** Terminal role refuted three horns (i = full TS grammar-engine port =
primitive of equal-or-greater difficulty → BLOCKED-candidate AND XL-D retires the need; ii = PROVE-14 wasm
full-walk 0.91× LOSS on exactly the value-producing workload; iii = shared decoder −22% on V8). Gate-0's regex
arm severed mechanically; **the VM `Dfa` arm remains LOCK-5-gated** (`bytecode.rs:184/238`, tree-confirmed) —
research-D's honest correction over the 15:22 "not LOCK-5-gated," correctly carried. G-D-GAP1 (HostClosure
type-plane) discharged compile-proven but on the same unattributed LOCK-1-TypeDesc pre-seed as 4B
(`rg HostClosure`=0). research-D's split-the-barrier finding (44/84 ~52% of latex-paper non-projection sites
recoverable as `Record`/`Array`/`Select` data; opaque residue = exactly 3 value.js easing closures) is a genuine
sharpening and the spec correctly self-guards against "4D dominates on parsimony ⇒ elect."

**Within-lock scrutiny — the tier-2 residue 4D's own thesis rests on is STILL unbuilt.** PROTO-2-live-differential
evaluated `HostFold` numeric folds — but 4D's *irreducible* value-plane residue is the **`FnDescriptor::Opaque(HostFnId)`
+ `Value::Host(HostHandle)`** inhabitant that lets a parse *carry* a closure it cannot introspect (research-D Q2
tier-2). That inhabitant is **not built and not evaluated** — the 3 easing closures remain the exact case no
probe has executed. So 4D's "split the barrier, `Opaque` carries only the residue" is proven for tier-1 (the
`Record`/`Array`/`Select` census, compile-plausible) and for the *numeric* slice of tier-2 (HostFold), but the
**closure-returning** slice — the whole reason `Value::Host` is needed — is untested. This is not a defect in
4D's reasoning; it is the family's sharpest remaining within-lock uncertainty, and it is co-owned with LOCK-7.

**Open gaps (all predecessor-owned / report-only per Q8):**
1. **INCOMPAT-1 (Q-ownership) → LOCK-5:** 5A shared type vs 5B lawful functor for the grammar IR; DAG-blocking.
   Gate-0's VM `Dfa` arm is a LOCK-5 extraction.
2. **HostClosure/`Value::Host` lattice → LOCK-7:** typed shape + per-subsystem floor; the closure-RETURNING
   residue (value.js's 3 easing closures) is un-built and un-evaluated (shared with 4C gap 2).
3. **Node-ratio thin AND FAILS on value.js** (1.857% repo / 4.84% math) — fusion-inheritance safe in aggregate,
   fragile on LOCK-8's consumer (shared with 4A gap 4).
4. **Value-plane ISA home → LOCK-3 (XL-B) FIRST, then GESTALT-Ω** (internal-descriptor vs host-closure split);
   correctly routed, un-ratified.
5. **`FnDescriptor::HostClosure` type-plane pre-seed unattributed + un-homed** (LOCK-1 TypeDesc);
   constitution-v zero-runtime-cost untested for it (shared with 4B gap 4).

---

## CROSS-CUTTING FAILURE-MODE AUDIT

- **Stale-evidence / unfolded follow-on probes — THE GOVERNING HIT, RECURRING.** The 18:43 re-synthesis froze
  before PROTO-1-4A-GA1 (19:40) and PROTO-2-live-differential (19:43) — its own BRIEF A and BRIEF B — ran. This
  is the SECOND consecutive pass with this exact pattern (15:22 draft vs 15:38/15:54 probes was the first). The
  spec undersells 4A (G-A1 carried "0% built, four passes" — now PASS) and 4C (G-C0 carried "UNTESTED" — now
  discharged). Structural, not incidental: the two-consecutive-clean-pass bar is unreachable while synthesis is
  always one probe behind its evidence.
- **Elegant-reduction trap — 4A hit DOWNGRADED, not cleared.** G-A1 ran and PASSED, so "the passes already
  exist" is no longer *zero* evidence. But the probe used a bbnf-side builder, not parse-that's real API — the
  genuinely hard step (real-API → `IrNode` across the crate seam) is still unbuilt. The reduction now contains a
  *smaller* unbuilt step, not none.
- **Gate-that-cannot-fail — TWO hits.** (i) G-B4 firing budget: no number, fourth pass — the canonical one,
  untouched by either probe. (ii) PROTO-1-4A-GA1's ≥0.98× timing gate is **near-vacuous**: byte-identical IR ⇒
  identical bytecode ⇒ the ratio cannot meaningfully deviate; the probe itself calls it "a confirmation, not the
  crux." It is not an independent gate (honestly labeled, so a soft hit).
- **Oracle self-forgery / faked golden — RETIRED for 4C's closed surface.** PROTO-2-live-differential replaced
  the author golden with parse-that's live in-process `.map`. The McKeeman trap the 16:03 CRIT-1 flagged is
  genuinely fixed — for the closed-descriptor + 2-fold surface. Residue: the *corpus* is author-selected
  projection kinds, not a real compiled grammar (a milder selection concern, not a faked golden).
- **SPEC_ONLY anchoring a claim — TWO hits, both surviving.** (i) 4B's canada-via-padded-input is unbuilt/
  unmeasured, presented as the known ceiling. (ii) G-B3's core emission arms "patch-plausible, not cold-built."
- **Sharpest-uncertainty aim — SPLIT.** PROTO-2-live-differential hit 4C's sharpest uncertainty squarely
  (VM vs live `.map`) — model probe design. PROTO-1-4A-GA1 hit *a* sharp uncertainty (fuse provenance-
  agnosticism) but sidestepped THE sharpest (real-API reification) — a good probe aimed one ring inside the
  bullseye.
- **Consumer-less substrate (constitution viii / Y.13) — CLEAN.** VM has live consumers (DAP adapter, JSON
  bench); the value plane's consumer is the iso-oracle + interpret/LSP lane.
- **Isomorphism clause (constitution iii) — half-tested still.** 4C's "iso by construction (parser crosses as
  data)" is now proven for the value-plane semantics **in-process in Rust** (12/12) but the **cross-language**
  half (TS re-decode over canada) is still unrun — G-C1 DAG-blocked on the `Dfa` port. The lock's strongest iso
  claim advanced on the Rust side, unmoved on the crossing.
- **Base-law (XL-H) — CLEAN.** Both new probes base-pin `ce51c36fc`, fast-forwarded and ancestry-verified;
  builds worktree-only (`rg ApplyFn`/`rg HostClosure`=0 on `06e70053f`). "PASS" reads as *correct-under-live-
  differential / byte-identical-fuse in a scratch worktree*, NOT *landed of record*.

---

## NEXT-PASS DIRECTIVES (within-lock)

1. **Re-synthesize folding the 19:40/19:43 PASSes, and STOP emitting briefs the same pass will execute.** 4A's
   G-A1 is built (json/fuse_single_use, bbnf-side builder); 4C's G-C0 is discharged (live differential). Update
   every % and the net disposition; the "four passes unbuilt" (4A) and "REAL gate untested" (4C) framings are
   dead. Break the freeze-before-briefs-run cycle or two clean passes never arrive.
2. **4A — close the two narrowings.** (a) Wire parse-that's REAL combinator API (not a bbnf-side builder) to
   emit `IrNode` across the crate seam and re-run byte-identity. (b) Run the CSS arm and a grammar with
   `@token`/`Map{fn_id}` so `fuse_token_dispatch` fires substantively, not as a no-op.
3. **4C — cross canada and evaluate a true opaque closure.** Port the TS `Dfa` (or restrict to LOCK-5's
   certified grammar-lowerable class) and run G-C1 over six corpora incl. canada; fire G-C4 on
   `rmp_serde::to_vec_named`. Evaluate at least one **closure-returning** `Opaque(HostFnId)` (value.js easing),
   not a numeric HostFold — the numeric fold is not the irreducible residue.
4. **4C/4D — differential on a REAL compiled grammar.** Compile `css/pretty.bbnf` through the actual front-end
   and cross its full value tree vs live `.map`, replacing the 12 hand-selected projection kinds.
5. **4B — G-B4's number or the named ceiling.** Produce the firing-budget curve (`.rlib`/`dist` +
   incremental-compile vs fuse-arity) on CSS + value.js, or record it as un-gateable and carry canada 1.56× as
   the measured exogenous ceiling. Attribute + home the `FnDescriptor::HostClosure` pre-seed (LOCK-1) and verify
   constitution v.
