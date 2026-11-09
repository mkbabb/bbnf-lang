# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-2 (adversarial critic 2 of 2, CROSS-LOCK weight) — RE-CUT ②

*Supersedes the 20:07 CRIT-2, which was written against the 18:43 FAMILY-SPECS + the 19:40/19:43 probes and
could NOT have seen the two probes that postdate it: **PROTO-1-4A-GA1-combinator-graph-RERUN.md (22:42)** and
**PROTO-2-mckeeman-repro.md (22:40)**. Read: PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H, binding),
FAMILY-SPECS.md (18:43), research-{A,B,C,D,priorart}, the full PROTO-1/-1-numeric-leaf/-2/-2-vmvalue/
-2-live-differential/-1-4A-GA1 corpus, CRIT-1 (19:57, within-lock), the 20:07 CRIT-2 (my own prior cut).
Tree-of-record spot-checks re-run THIS session on HEAD `3f2350f15` (base-pin `ce51c36fc` confirmed ancestor):
`rg ApplyFn crates/ir/src`=**0**, `rg HostClosure crates/core crates/ir`=**0**, `FnDescriptor` = **6 variants**
(EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr — **no HostClosure**), `bytecode.rs:184/238`
`parse_that::regex::Dfa` dep **LIVE**, `crates/ir/Cargo.toml:13` `parse_that="0.4"` cycle **real**,
`crates/ir/src/vm` = **1772 LOC**. The specs' code facts hold — NOT spec-cites-itself. Critic-2 weights the
cross-lock view. Convergence EARNED; pass 3; no family converged.*

---

## 0. HEADLINE — THE PASS SPENT ITS PROBE BUDGET **REPRODUCING TWO ALREADY-PASSED GATES**, CLOSING ZERO OPEN GAPS; THE TERMINAL IS UNCHANGED AND STILL FIVE-EXTERNAL-LOCKS-BOUND

The two probes that postdate my 20:07 cut are, by their own titles and text, **INDEPENDENT REPRODUCTIONS**:

- **PROTO-1-4A-GA1-RERUN (22:42)** — "an *independent replication* of the same-probe report… a second,
  separately-built confirmation… It exists to turn the SPEC-ONLY-for-four-passes claim into a
  twice-measured anchor, not to supersede the prior text." Same json grammar, same 85-LOC bbnf-side builder,
  same `fuse_single_use`+`fuse_token_dispatch`, timing 1.0009 (prior 0.9981, both in-band).
- **PROTO-2-mckeeman-repro (22:40)** — "an *independent reproduction* of that gate on a clean base pin… It
  does not clobber the prior artifact." Same HostFold closed-fold VM, same 12 projection kinds, same live
  `.map` differential, 87/87 · 12/12 · 0 divergences (identical to the 19:43 census).

**What reproduction earns, and what it does not.** An independent, separately-built confirmation by a
different agent on a clean base pin is *genuinely relevant* to two of the three convergence criteria —
"fresh non-author audit" and "two consecutive clean passes" — and it retires single-run-fluke risk. I credit
that (marginal per-family upticks below). **But the dominant convergence criterion is ZERO ENUMERATED GAPS,
and not one of the ~12 open gaps my 20:07 cut enumerated across the four families was touched:** real-API
reification across the `parse_that↔bbnf_ir` seam (4A gap 1), the CSS/`@token` arm (4A gap 2), the LOCK-3
tower-parity obligation (4A gap 3), the arbitrary opaque closure (4C gap 2 / 4D gap 2), the canada
cross-language crossing (4C gap 1 — the raison d'être), G-B4's number (4B gap 3), the TS `Dfa` port
(§6.4 sixth hand-off), the value.js analyzability FAIL for LOCK-8 (§6.7). **The frontier is exactly where
it was at 20:07.** This is the checklist's "polish-the-green" adjacent pathology: the pass hardened
confidence in already-credited claims rather than attacking the enumerated frontier. Convergence stalls this
way — a pass can be "clean" (both reproductions PASS) and still move the % ~nothing, because the open gaps
are what the % measures. **The cross-lock ceiling my 20:07 cut set — the terminal bounded by FIVE external
verdicts + two provenance debts — is UNCHANGED.** Return LOCK-4 as a rigorously converged CORE with its
exact external gaps; do NOT let a gestalt read "twice-measured + independently reproduced" as
election-readiness.

**Three cross-lock findings THIS reproduction round adds (my load-bearing new material):**

1. **[NEW, tree-verified overstatement] The mckeeman-repro's headline "correct-of-RECORD" is an equivocation
   on "of-record."** It VERDICTs *"The 4C/4D value plane is correct-of-record, not merely worktree-buildable."*
   I re-verified on HEAD `3f2350f15`: `rg ApplyFn`=0, `rg HostClosure`=0, `FnDescriptor`=6 variants. The value
   plane is **NOT of record** — it is worktree-only, byte-identical to the *patch file* (`PROTO-2-live-
   differential.patch`, 1253 lines), which is not the tree. What the probe MEANS is "correct against
   parse-that's live shipped `.map` as an of-record ORACLE." The oracle ships; the CODE does not.
   Conflating "the reference I tested against is of-record" with "my artifact is of-record" is exactly the
   constitution-vii framing failure the prior CRIT-1/CRIT-2 retired ("worktree-built ≠ correct of record") —
   and this reproduction **RE-ASSERTS it** in its verdict line (§3, §5).

2. **[NEW] Reproduction of a SUBSTITUTE amplifies the masked-fallback risk, it does not retire it.** My 20:07
   §5 established HostFold as a **closed 2-fold numeric descriptor**, NOT the arbitrary opaque closure the hard
   case (value.js's 3 easing closures that RETURN a host function) demands. Re-running the identical HostFold
   gate twice doubles the confidence in the *wrong artifact*. The masked-fallback hazard — a closed numeric
   fold standing in for the arbitrary opaque closure — is now **twice-anchored**, which makes it *more* likely
   to be mis-read at gestalt as "the HostClosure barrier is crossed AND independently reproduced." Reproducing
   a substitute is a subtle amplifier, not a discharge (§3, §5).

3. **[NEW] The reproduction re-verified ONE of the two incompatible HostClosure shapes and left the fork
   asymmetrically anchored.** The mckeeman-repro §0 diffs the worktree against `PROTO-2-live-differential.patch`
   (0 divergences) — i.e., it hardened the `HostClosure{kind: HostFold, coeffs: Vec<f64>}` **VM-plane** shape.
   The **emitter-plane** shape `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` (PROTO-2 PART 2, on which G-B3
   and G-D-GAP1 ride) was NOT reconciled. The two-shapes provenance mess (my 20:07 §5) is now not merely
   unresolved but **asymmetrically hardened**: one of two incompatible shapes wearing one name is
   twice-measured, the other untouched, `rg HostClosure`=0 for both (§5).

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH · **convergence 73%** (20:07 crit-2: 72%; +1 for the twice-measured anchor only)

**Why the +1, and why it is only +1.** The RERUN turns G-A1 — 4A's one novel claim, SPEC-ONLY for four passes
— into a **twice-measured** anchor: 0/10 pre- and post-fuse divergences and an identical 37-node DAG on a
second, separately-built run at the same base-pin, timing 1.0009 (prior 0.9981, both sides of 1.0 — the
same-program noise signature). Single-run-fluke risk is retired; the independent-reproduction confirmation is a
real, if small, hardening toward the fresh-non-author-audit criterion. **But the reproduction changed NOTHING
about the claim's SHAPE** — it is the identical json grammar, the identical **bbnf-side 85-LOC `IrNode`
builder** (NOT parse-that's real combinator API), the identical no-op `fuse_token_dispatch`. The RERUN's own
honesty caveats concede every open gap verbatim: *"One grammar (json)… A grammar with `@token`/`@pretty` or
host `Map{fn_id}` branches… are out of this probe's parsimony scope and are the natural next probe."* The
cross-lock-hard step — bridging parse-that's shipped `SmallBox<dyn ParserFn>` (`parse.rs:74`) to emit `IrNode`
**across the `parse_that↔bbnf_ir` crate cycle** I re-confirmed live at `Cargo.toml:13` — is untouched twice.

**Cross-lock (unchanged from 20:07):**
- **[LOCK-3 obligation, XL-B, still unrun]** open-Q2 (reify→serialize→SAME-emitter→graph-of-record parity) is a
  LOCK-3 tower-parity build; the reproduction did not (and structurally cannot) discharge it. Zero evidence.
- **[LOCK-8 consumer hazard, still open]** the node-barrier census FAILS on value.js math (4.84% > 2%; 1.857%
  repo). value.js is LOCK-8's flagship (8D). "Analyzable graph" reads as "skeleton with opaque leaves" on
  exactly the consumer whose suite gates the TS plane; the reproduction round did not re-run the census either.

**Exact open gaps (ALL carried from 20:07, NONE closed):**
1. **Real-API reification across the crate seam is unbuilt** — both runs used the bbnf-side builder; the
   INCOMPAT-1 seam (parse-that surface → `IrNode`) is where the sharpest 4A uncertainty lives, unmoved twice.
2. **CSS arm of G-A1 unrun; `fuse_token_dispatch` a no-op on json (both runs)** — token-dispatch/`Map{fn_id}`/
   `@pretty` inheritance proven only trivially (identical early-return); `fuse_single_use` alone fired, twice.
3. **Reify→SAME-emitter→graph-of-record parity (open-Q2) = a LOCK-3 tower-parity obligation (XL-B)**, zero
   evidence.
4. **Node-ratio FAILS on LOCK-8's flagship consumer** (4.84% math) — repo aggregate, not an invariant; not
   re-examined this round.
5. **The ≥0.98× timing arm is near-vacuous** (byte-identical IR ⇒ identical bytecode; both reports say so) —
   twice-measured does not make a non-independent gate independent.

---

## 2. FAMILY 4B — COMPTIME-FUSION · **convergence 66%** (20:07 crit-2: 66%; UNCHANGED — no probe touched 4B, again)

**Neither reproduction addressed 4B.** Its disposition is exactly my 20:07 reading: G-B0 PASS (recursive JSON,
0 box/dyn/fn-ptr, ZST parser), G-B3 PASS (host closure fused as 1 named call-out on the emitter-plane
HostClosure shape), G-B1 MEASURED-MIXED (data/twitter WIN, citm TIE, **canada 1.50–1.60× REGRESS**), the
numeric lever FAILED (PROTO-1-numeric-leaf; canada 1.56× unchanged), election EXOGENOUS (LOCK-9 9B + LOCK-6/8
padded-input). Internally sound; the strongest family; the most externally-bound terminal.

**The cross-lock crux is unchanged and the gate-that-cannot-fail is now on its FIFTH pass unset.** G-B4 (the
firing budget: fuse-arity 8, `try_factor_alt` at `branches.len()>=4`, one box per SCC back-edge, `.rlib`/`dist`
+ incremental-compile) still has **no measured number** — and a full reproduction round elapsed with the probe
budget spent elsewhere. The `generated-size-budget` edict is cited; no curve exists on CSS + value.js. A
firing budget with no number does not gate the monomorphization-blowup it exists to bound. The G-B1 baseline
remains **cross-lock-unstable**: it is measured against the SHIPPED `json_parser()`, whose f64 leaf IS the
`number_simd` LOCK-9 9B is actively rebirthing — a moving target owned by a lock mid-rewrite (XL-H-adjacent).

**Exact open gaps (unchanged from 20:07):**
1. **Election is EXOGENOUS** (LOCK-9 9B kernel + LOCK-6/8 padded-input re-arch) — canada 1.56× is the ceiling
   until 9B lands; the win is string/structure/small ONLY (citm a TIE, not a win).
2. **Canada-via-padding is SPEC_ONLY** — the SIMD-stripe re-architecture is an unbuilt hypothesis presented as
   the known ceiling; no artifact (constitution-vii).
3. **G-B4 firing budget: NO number, FIFTH pass** — the canonical gate-that-cannot-fail; the reproduction round
   made it more overdue, not less.
4. **G-B3 rides the inconsistent, unattributed emitter-plane HostClosure shape** (`rg HostClosure`=0; the OTHER
   shape of the two-shapes mess — §5); core emission arms "patch-plausible, NOT cold-built."
5. **XL-E supersession has no PT-tranche home** (constitution-ix debt → GESTALT-P) — owed since pass 1.

---

## 3. FAMILY 4C — VM-CORE / PORTABLE-PROGRAM ISO · **convergence 63%** (20:07 crit-2: 62%; +1 for independent reproduction) · BANKED

**Why the +1.** The mckeeman-repro is a genuine, separately-built confirmation on a clean base pin: cold run
(fresh empty target dir, 57 crates recompiled, `Finished in 1m 19s`) AND incremental, both 87/87 · 12/12 · 0
divergences, source-audited that `exec_apply_fn` (`apply_fn.rs:94–104`) EVALUATES HostFold rather than
forwarding (the `F64`/`U32` discriminant a `=>child` forward would violate). The Rust-side semantic-correctness
confidence is now twice-earned. I credit the independent reproduction toward the fresh-non-author-audit
criterion.

**Why still only +1, and why the "correct-of-record" claim must be down-graded (my governing 4C finding):**

- **[tree-verified] "correct-of-RECORD" is FALSE as written.** HEAD `3f2350f15`: `rg ApplyFn`=0,
  `rg HostClosure`=0, `FnDescriptor`=6 variants. The value plane is worktree-only, exactly as at 20:07. The
  probe's own text concedes it — *"Worktree is byte-identical to the patch of record"* — byte-identical to the
  **patch FILE**, not committed to the tree. The verdict sentence "correct-of-record, not merely
  worktree-buildable" equivocates: the ORACLE (parse-that `.map`) is of-record, the CODE is not. This is the
  same overstatement the critics retired, re-asserted (§0.1).
- **[LOCK-6, still unrun] G-C1 (the canada cross-language crossing) never ran — again.** The differential is
  in-process Rust, NOT a TS re-decode. The 6D behavioral-iso evidence LOCK-6 needs — decode-in-TS, `Value`-tree
  identity over six R3 corpora incl. **canada** (the mandatory adversarial member where PROTO-1 shows the f64
  leaf diverges) — is DAG-blocked on porting `parse_that::regex::Dfa` to TS (`bytecode.rs:184/238`, re-confirmed
  live this session). **LOCK-4's headline cross-lock deliverable is half-built after a full reproduction
  round.** Reproducing the Rust half does not narrow the crossing.
- **[substitute hardened, §0.2] the "non-stub HostClosure" is a CLOSED numeric fold, twice-measured.** HostFold
  ∈ {Lerp, ChannelScale} is evaluable/serializable/NON-opaque; it is NOT the `Opaque(HostFnId)`/`Value::Host`
  inhabitant value.js's 3 closure-RETURNING easing parses demand. Re-running it doubles confidence in the
  substitute. The probe honestly says *"it does not claim to be the general arbitrary-closure evaluator"* — but
  the twice-anchoring raises the gestalt-misread risk that "HostClosure discharged, reproduced" reads as the
  hard case crossed.
- **[LOCK-3 double-home, XL-B, unchanged] `Op::ApplyFn` + `fns: Vec<FnDescriptor>` + `Value` 4→8 is a
  tower-shape ISA change LOCK-3 owns.** The reproduction re-built it worktree-first (0 divergences vs the patch).
  Prototyping is legitimate; "correct-of-record" over LOCK-3's un-ratified home, while LOCK-3 runs its own
  pass-3+, is the double-home the hand-off list (#3) correctly routes but the verdict framing over-claims.

**Exact open gaps (ALL carried from 20:07, NONE closed; corpus/oracle re-hardened, gaps intact):**
1. **G-C1 canada / full-corpus TS crossing UNRUN** — DAG-blocked on the `Dfa` TS port (§6.4); the LOCK-6-facing
   6D evidence is the family's raison d'être and it is half-built after a reproduction round.
2. **The arbitrary opaque closure is UNEVALUATED** — HostFold is a closed 2-fold set, twice-measured; the
   closure-RETURNING residue (value.js easing) demanded by 4C/4D is the exact case no probe has executed.
3. **Value plane worktree-only + LOCK-3-homed** (`rg ApplyFn`=0 re-confirmed); "correct-of-record" is an
   equivocation (§0.1); ISA home is LOCK-3's (XL-B), un-ratified.
4. **Corpus is author-selected projection kinds** — no real compiled grammar (`css/pretty.bbnf`) crossed
   full-value-tree; the reproduction used the same 12 kinds.
5. **G-C4 msgpack idempotence unfired** on `rmp_serde::to_vec_named` (the actual bus); born-RED reorder guard
   never fired where it ships — neither reproduction touched it.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **convergence 58%** (20:07 crit-2: 58%; UNCHANGED) · BLOCKED (split)

**No probe this round touched 4D's irreducible residue.** Sound disposition, honestly BLOCKED/split (terminal
refuted three horns: i = full TS grammar-engine port = primitive of equal-or-greater difficulty AND XL-D
retires the need; ii = PROVE-14 wasm full-walk 0.91× LOSS; iii = shared decoder −22% on V8). Gate-0 regex arm
severed by re-export; VM `Dfa` arm LOCK-5-gated (`bytecode.rs:184`, re-confirmed live). research-D's
split-the-barrier finding (44/84 ~52% latex-paper non-projection sites recoverable as `Record`/`Array`/`Select`;
opaque residue = 3 value.js easing closures) is a genuine sharpening; the spec self-guards against "4D dominates
on parsimony ⇒ elect."

**Unchanged cross-lock hazards (both governing, neither touched by the reproduction round):**
- **[contradiction, XL-C] INCOMPAT-1 mis-invokes a RETIRED fork.** open-Q1 frames Q-ownership as "5A shared
  type vs **5B lawful functor**." XL-C: *"5B's functor is RETIRED (conditional re-open only if LOCK-1 abandons
  Map-barrier typing)."* Presenting a settled adjudication as the live fork is a constitution-ix violation
  (divergence must be a NAMED supersession). AND 5A/5B are LOCK-5's **regex-HIR** family names; applying them to
  the **grammar-IR-sharing** question is a category slippage. The genuine open question (does parse-that's
  combinator graph share `bbnf_ir`?) is real, but it is NOT "5A vs 5B." Un-corrected across two more probes.
- **[LOCK-7, residue unbuilt] The `Opaque(HostFnId)` + `Value::Host` inhabitant — the whole reason the
  barrier-split thesis needs a residue tier — is neither built nor evaluated.** The reproduction evaluated
  HostFold (numeric), MapExpr-adjacent, NOT the closure-RETURNING case. The 3 easing closures remain the exact
  case no probe has executed, twice over.

**Exact open gaps (predecessor-owned / report-only per Q8; unchanged from 20:07):**
1. **INCOMPAT-1 (grammar-IR sharing) → LOCK-5, but the "5A vs 5B" framing contradicts XL-C** (5B retired);
   re-label as a named supersession or route to the true home. Gate-0's VM `Dfa` arm IS a LOCK-5 extraction.
2. **HostClosure/`Value::Host` lattice → LOCK-7:** the closure-RETURNING residue (3 easing closures) un-built,
   un-evaluated (shared with 4C gap 2).
3. **Node-ratio thin AND FAILS on value.js** (1.857% repo / 4.84% math) — fragile on LOCK-8's consumer (4A
   gap 4).
4. **Value-plane ISA home → LOCK-3 (XL-B) FIRST** (internal-descriptor vs host-closure split); un-ratified,
   built worktree-only under a LOCK-4 probe, "correct-of-record" over-claimed (4C gap 3).
5. **`FnDescriptor::HostClosure` type-plane pre-seed unattributed, un-homed (LOCK-1), INCONSISTENT across two
   probes AND asymmetrically hardened** (§5, §0.3); constitution-v zero-runtime-cost untested (4B gap 4).

---

## 5. THE TWO-SHAPES HostClosure PROVENANCE MESS — now ASYMMETRICALLY HARDENED (updated from my 20:07 §5)

Three PASS/discharge claims — G-B3 (4B), G-D-GAP1 (4D), and the "non-stub HostClosure evaluated" (4C) — all
ride a `FnDescriptor::HostClosure` diff that is `rg HostClosure`=0 of record (re-verified HEAD `3f2350f15`).
The diff is not one shape — it is **two incompatible shapes wearing one name**, and neither is the arbitrary
opaque closure the hard case demands:

| probe | shape (worktree) | what it is | what it is NOT | reproduced 22:40? |
|---|---|---|---|---|
| PROTO-2 PART 2 (emitter) | `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` | opaque call-out reference (LOCK-7-lattice-shaped) | not evaluable in-VM | **NO** |
| PROTO-2-live-diff / mckeeman-repro (VM) | `HostClosure{kind: HostFold, coeffs: Vec<f64>}` | CLOSED numeric fold (evaluable, serializable, NON-opaque) | not a reference, not opaque, not the arbitrary closure | **YES (0 divergences vs patch)** |
| research-B/D proposal | `HostClosure{fn_id, arg_types, ret_type}` → host `fn_table` | the arbitrary opaque residue + `Value::Host` handle | **built by neither probe** | n/a |

**What the reproduction round changed here: it hardened the VM-plane shape and left the fork asymmetric.** The
mckeeman-repro §0 explicitly diffs the worktree against `PROTO-2-live-differential.patch` (0 divergences across
1253 lines) — so the VM-plane `HostFold` shape is now twice-anchored, while the emitter-plane shape (on which
G-B3/G-D-GAP1 ride) is untouched. The cross-lock consequences from 20:07 stand and sharpen:
- **"HostClosure discharged," now reproduced, reads MORE strongly as "the arbitrary opaque barrier is
  crossed."** It is not: the emitter probe fused a call-out (body opaque, un-evaluated); the VM probe evaluated
  a closed numeric fold (not opaque). The irreducible case — a parse that *carries* a closure it cannot
  introspect — is unbuilt, and the reproduction did not attempt it.
- **The `HostFold` closed-fold pre-empts LOCK-7's lattice with a shape LOCK-7 did not sanction, now
  twice-landed.** LOCK-7 owns the host-fn lattice (Q8); nothing in XL-A..H ratifies a shape. A closed
  `{kind, coeffs}` descriptor invented under a LOCK-4 probe and reproduced under a second is a third design
  point neither LOCK-1 (TypeDesc) nor LOCK-7 (lattice) authored — landed twice to make a McKeeman gate go green.
- **Constitution-v (zero-runtime-cost) is untested for BOTH shapes**, and they cannot both be the type-plane
  truth. Attribution + homing (LOCK-1 TypeDesc + LOCK-7 lattice) must pick ONE shape of record before any gate
  riding it counts. The reproduction round hardened the confidence in a shape the owning locks have not chosen.

---

## 6. CROSS-LOCK VIEW (critic-2 weight)

### 6.1 — [governing, unchanged] The terminal is bounded by FIVE external verdicts + two provenance debts; the reproduction round moved NONE of them.
Hand-off list (from the 18:43 spec, all still open): (1) Q-ownership → LOCK-5; (2) cross-language iso → LOCK-6
+ evidence to LOCK-8; (3) value-plane ISA home → LOCK-3 (XL-B); (4) 4B perf-claim supersession → GESTALT-P;
(5) numeric ceiling → LOCK-9 + LOCK-6/8 padded-input. Plus: HostClosure pre-seed → LOCK-1; G-B1 baseline
cross-lock-unstable under LOCK-9. Seven of nine locks gate LOCK-4's terminal. Two reproductions later, the
count is identical. "Settled AND partly built" must NOT be read as terminal-convergence.

### 6.2 — [double-home, LOCK-3, hardened] The value-plane ISA was BUILT (worktree) AND now RE-BUILT (worktree) inside `crates/ir/src/vm/` under LOCK-4 probes.
`Op::ApplyFn` + `fns: Vec<FnDescriptor>` + `Value` 4→8 is LOCK-3's tower-shape home (XL-B). Two probes now land
it worktree-first; the second calls the result "correct-of-record" (§3, false on tree). If LOCK-3 designs the
VM value plane independently in its concurrent pass, the two collide — and LOCK-4 has now twice-asserted a
shape LOCK-3 has not ratified.

### 6.3 — [double-home, LOCK-1 + LOCK-7, hardened §5] The HostClosure/HostFold pre-seed is TypeDesc + lattice work, unattributed, inconsistent, and now asymmetrically twice-anchored.
The pass's cleanest example of "unattributed pre-seed anchoring a gate" — worsened, because the reproduction
hardened one of the two incompatible shapes without adjudicating the fork.

### 6.4 — [candidate SIXTH hand-off, unchanged] The TS `Dfa` port that unblocks G-C1 has NO owning lock.
G-C1 (the cross-language crossing, LOCK-6's 6D evidence) is DAG-blocked on porting `parse_that::regex::Dfa` to
TS (`bytecode.rs:184/238`, re-confirmed live). research-C §8 exports the Dfa-vs-`RegExp` *divergence* risk to
LOCK-5; the ACT of porting `Dfa` to TS floats with no home. Per XL-C the regex engine home is parse-that; the
TS regex face is LOCK-8/LOCK-5-TS/LOCK-9-via-5C territory. Name its home, or G-C1 — and thus LOCK-6's
behavioral-iso — stays permanently DAG-blocked. The reproduction round, spent on the Rust half, did not
approach it.

### 6.5 — [contradiction, XL-C, unchanged] INCOMPAT-1's "5A vs 5B" contradicts the retired-5B ledger (§4 gap 1). Re-label or re-home. Two more probes elapsed without correction.

### 6.6 — [clean, credit where due, unchanged] INCOMPAT-2 → LOCK-6 is handled RIGHT; the 15:22 overreach stays self-corrected.
The spec exports the EVIDENCE (Rust monomorphization ≠ TS emission) and lets LOCK-6 rule; routes PROTO-1's
emitter/V8 result to LOCK-8; carries XL-D as binding. Exemplary cross-lock discipline — the model the other
hand-offs should meet.

### 6.7 — [LOCK-8 consumer hazard, still under-weighted] The analyzability thesis FAILS on value.js, LOCK-8's flagship.
1.857% repo / 4.84% math. LOCK-8 (8D) designs backward from value.js. "Fully analyzable graph" is safe in
aggregate, fragile on exactly the consumer whose suite is the terminal falsifier (priorart §5). Hand LOCK-8
this explicitly; the reproduction round did not re-examine it.

### 6.8 — [consumer-less-substrate, adjacent, unchanged] The value plane has no PRODUCTION-execution consumer by construction.
The fast face is codegen (born with the arena; no value plane). The value plane's consumers all live OUTSIDE
LOCK-4 (iso-oracle → LOCK-6; interpret/LSP/DAP → LOCK-3/LOCK-8). Not consumer-less in the strict Y.13 sense —
but home-revealing (§6.2). "built + reproduced" ≠ "on any LOCK-4 hot path."

### 6.9 — [NEW, process] Probe-budget allocation is a convergence risk in its own right.
This is the second pass in a row where the probes ran the *previous* pass's briefs (15:22 draft → 15:38/15:54;
18:43 spec → 19:40/19:43), and now a third where the probes REPRODUCED the 19:40/19:43 briefs rather than the
enumerated open gaps. A pass that always confirms yesterday's PASS never reaches two-consecutive-clean, because
the open-gap count — the convergence denominator — is never attacked. This is a cross-lock cost: GESTALT-P/Ω
consume a LOCK-4 disposition whose % has been ~flat for a full round while the artifact-of-record accretes
worktree confirmations. The next pass must spend on the frontier (real-API seam, CSS/@token, arbitrary opaque
closure, canada crossing, G-B4 number), not a third reproduction.

---

## 7. CONSTITUTION & FAILURE-MODE LEDGER (cross-lock weight)

- **Vacuous convergence:** probe-level gates are refutable (a reproduction CAN fail to reproduce) — not vacuous
  at the probe. **Lock-level "settled AND partly built" is vacuous-adjacent:** terminal un-electable (five
  external verdicts), 4C BANKED, 4D BLOCKED-split, 4B election three-lock-away. The % encodes terminal-blockage,
  and this round's reproductions did not move it.
- **Spec-cites-itself / circularity:** substrate facts independently re-verified this session (rg on
  `3f2350f15`) — NOT self-citing. Residue: the McKeeman corpus is author-selected projection kinds
  (live oracle, hand-picked corpus), re-used verbatim by the reproduction — a selection concern, not a faked
  golden.
- **Gates that cannot fail:** (i) **G-B4 firing budget — no number, FIFTH pass** (untouched again). (ii)
  PROTO-1-4A-GA1's ≥0.98× timing gate is near-vacuous (byte-identical IR ⇒ identical bytecode) — twice-measured
  does not make it independent. Both honestly labeled; both still unset/vacuous.
- **Elegant-reduction trap (TWO live forms, unchanged):** (i) the terminal = "fold the mechanisms [done, now
  reproduced] and-then FIVE other locks decide the fork [unconverged, external]"; (ii) the value plane =
  "built [worktree, now re-built] and-then it computes the right values" where the hardest case (arbitrary
  opaque closure) is a **closed numeric-fold substitute** (§5) — twice-anchored.
- **Masked fallback — AMPLIFIED this round (§0.2):** "non-stub HostClosure evaluated," now reproduced, masks
  that the evaluated thing is a closed numeric fold, not the arbitrary opaque closure. Reproduction of a
  substitute doubles the green-light strength of a gate the hard case does not clear.
- **Overstatement / of-record equivocation — NEW (§0.1, §3):** the mckeeman-repro VERDICTs "correct-of-record"
  while `rg HostClosure`/`rg ApplyFn`=0 committed. The oracle is of-record; the code is not. Constitution-vii
  artifact discipline: the label must match the tree.
- **Legacy aliases:** none new.
- **Unverified gestalt / pre-empted authority:** the value-plane ISA (LOCK-3) and the HostClosure/HostFold
  type-plane (LOCK-1/LOCK-7) were built — and now RE-built — under LOCK-4 probes ahead of the owning locks'
  ratification (§6.2, §6.3). Prototyping is legitimate; "correct-of-record"/"discharged" over un-ratified
  owning-lock homes is not, and a second confirmation does not confer the ratification.
- **SPEC_ONLY anchoring a claim:** (i) 4B's canada-via-padded-input unbuilt, presented as the known ceiling;
  (ii) G-B3's core emission arms "patch-plausible, not cold-built."
- **Consumer-less substrate (viii):** VM has live consumers (DAP, JSON bench) — compliant. The value plane's
  consumers all live outside LOCK-4 (§6.8) — compliant but home-revealing.
- **Isomorphism (constitution iii):** advanced on the Rust side (now twice: 12/12 in-process), UNMOVED on the
  cross-language crossing (G-C1 DAG-blocked). LOCK-4's strongest iso claim is still half-tested; the
  LOCK-6-facing half is still the unbuilt half.
- **Extant-tranche consciousness (ix):** (a) INCOMPAT-1 re-litigates retired-5B (XL-C) without naming a
  supersession — live violation, two more probes elapsed (§4, §6.5); (b) XL-E's PT-tranche home still owed.
- **Base-law (XL-H):** both new reproductions base-pin `ce51c36fc` (confirmed ancestor of HEAD `3f2350f15`),
  worktree-only (`rg ApplyFn`/`rg HostClosure`=0 re-confirmed). Deeper: 4B's G-B1 baseline is a MOVING TARGET
  owned by LOCK-9 mid-rewrite. "PASS" reads as correct-under-live-differential / byte-identical-fuse in a
  scratch worktree, NOT landed of record, NOT cross-language, NOT re-based against LOCK-9's 9B.

---

## 8. VERDICT & PASS-4 DIRECTIVES (cross-lock)

No family converged; pass 3. The two post-20:07 probes RAN and PASSED as **independent reproductions** — a
real hardening of confidence toward the fresh-non-author-audit criterion, and I credit it (marginal upticks:
4A +1, 4C +1). **But they closed ZERO enumerated gaps.** The cross-lock reading governs: the terminal is
UNCHANGED — five external verdicts + two provenance debts, exactly as at 20:07; the value plane is still
worktree-only (`rg HostClosure`=0) and its reproduction re-asserted a false "correct-of-record"; the arbitrary
opaque closure is still a twice-anchored closed-fold substitute; the canada cross-language crossing (4C's
raison d'être, LOCK-6's evidence) is still half-built; G-B4 is on its fifth pass without a number.

**Per-family convergence (critic-2, pass-3, cross-lock): 4A 73% · 4B 66% · 4C 63% (BANKED) · 4D 58%
(BLOCKED-split).** The deltas over 20:07 (4A +1, 4C +1) are the independent-reproduction confirmation ONLY;
the frontier is flat. Leadership by axis is unchanged: **4B leads terminal-readiness** (running fast face both
languages) but is most externally-bound (three-lock-away election, no G-B4 number, fifth pass); **4C leads the
iso-of-record** but only on the Rust half, twice; **4A leads analyzability-readiness** but the real-API
cross-seam step and the LOCK-3 parity obligation are unbuilt twice; **4D leads parsimony** but conditional on a
fork whose framing contradicts XL-C.

Pass-4 must:
1. **SPEND ON THE FRONTIER, NOT A THIRD REPRODUCTION (§6.9).** Two gates are twice-confirmed; a third
   confirmation buys nothing. Attack the enumerated open gaps: the real-API `parse_that→bbnf_ir` seam (4A), the
   CSS/`@token` arm (4A), the arbitrary opaque closure (4C/4D), the canada cross-language crossing (4C/LOCK-6),
   G-B4's number (4B).
2. **Correct the "correct-of-record" overstatement (§0.1, §3)** — the value plane is worktree-only
   (`rg HostClosure`=0 on HEAD `3f2350f15`); label it correct-against-a-live-oracle-in-worktree, not
   of-record. The oracle ships; the code does not.
3. **Resolve the HostClosure two-shapes mess (§5), now asymmetrically hardened** — attribute the pre-seed
   (LOCK-1 TypeDesc), home the lattice (LOCK-7), pick ONE shape of record (the reproduction hardened the
   VM-plane `HostFold` while leaving the emitter-plane shape untouched), verify constitution-v, and stop
   reading three gates as passed on an inconsistent un-homed diff.
4. **Re-label INCOMPAT-1** — 5B is RETIRED per XL-C; name a supersession with evidence or route the
   grammar-IR-sharing question (distinct from the regex-HIR 5A/5B axis) to its true home.
5. **Name the TS `Dfa` port's owning lock (§6.4)** or G-C1 — and LOCK-6's behavioral-iso evidence — stays
   permanently DAG-blocked.
6. **Cross canada cross-language + evaluate a TRUE opaque closure** (value.js easing, closure-RETURNING, not a
   numeric HostFold); fire G-C4 on `rmp_serde::to_vec_named`.
7. **Give G-B4 a number** (`.rlib`/`dist` + incremental-compile vs fuse-arity on CSS + value.js) or record it
   un-gateable and carry canada 1.56× as the exogenous LOCK-9-owned ceiling, re-based when 9B lands.
8. **Hand LOCK-8 the value.js analyzability-FAIL explicitly (§6.7).**

**Return LOCK-4 as a rigorously converged CORE with its five external gaps (LOCK-5 ownership-semantics;
LOCK-6 cross-language iso; LOCK-3 value-plane ISA home; LOCK-9 numeric ceiling + moving baseline; LOCK-7
host-closure lattice) + two provenance debts (LOCK-1 pre-seed attribution — now asymmetrically hardened;
XL-E PT-tranche home) — never as a terminal. The two reproductions confirm the mechanisms; they do not
elect the terminal, and they did not narrow the frontier.**
