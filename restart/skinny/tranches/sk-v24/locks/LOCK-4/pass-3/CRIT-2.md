# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-2 (adversarial critic 2 of 2, CROSS-LOCK weight) — RE-CUT

*Supersedes the 16:09 CRIT-2 draft, which was written against the STALE 15:22 FAMILY-SPECS and could not
have seen the 18:43 re-synthesis, PROTO-1-4A-GA1 (19:40), or PROTO-2-live-differential (19:43). Read:
PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H, binding), FAMILY-SPECS.md (18:43 re-synthesis),
research-{A,B,C,D,priorart}, PROTO-1/-1-numeric-leaf/-2/-2-vmvalue, and — decisively — the two probes that
POSTDATE the spec: PROTO-1-4A-GA1 (19:40), PROTO-2-live-differential (19:43); CRIT-1 (19:57, within-lock).
Tree-of-record spot-checks re-run THIS session on HEAD `06e70053f` (descendant of base-pin `ce51c36fc`):
`rg ApplyFn crates/ir/src`=**0**, `rg HostClosure crates/core crates/ir`=**0**, `FnDescriptor` = **6
variants** (EnumWrap/BoxWrap/NumberConvert/HexConvert/SpanCapture/Expr — no HostClosure), `bytecode.rs:184`
`parse_that::regex::Dfa` dep **LIVE**, `crates/ir/Cargo.toml:13` `parse_that="0.4"` cycle **real**,
`crates/ir/src/vm` = **1772 LOC**. The specs' code facts hold — NOT a spec-cites-itself substrate. Critic-2
weights the CROSS-LOCK view: where LOCK-4 contradicts or double-homes work against the extant GN/PT codex
and the sibling locks. Convergence EARNED; pass 3; no family converged.*

---

## 0. HEADLINE — THE FAMILIES ROSE ON RUNNING EVIDENCE; THE TERMINAL MOVED **FURTHER OUT OF LOCK-4'S HANDS**, NOT CLOSER

Both post-spec probes RAN and PASSED — a genuine advance I do not dispute (CRIT-1 credits them at the
within-lock level, correctly). BRIEF A (`fuse(Graph)→Graph` byte-identity) is built; BRIEF B (the
McKeeman live-differential) is discharged for the Rust-side closed surface. On running evidence the
families are up.

**But the cross-lock reading is the inverse of the within-lock one, and it is the governing finding of
this critique.** Pass-2 CRIT-2 named the terminal as jointly owned by **two** external locks (LOCK-5
ownership, LOCK-6 iso). Pass-3's own hand-off list now enumerates **FIVE** cross-lock DAG dependencies
plus **two** provenance debts. Restated plainly: **every measured advance this pass was purchased by
exporting another slice of LOCK-4's terminal to another lock.** The numeric ceiling went to LOCK-9; the
value-plane ISA home to LOCK-3; the HostClosure lattice to LOCK-7; ownership-semantics to LOCK-5; the
cross-language iso to LOCK-6; and PROTO-1's emitter/V8 result feeds LOCK-8. Seven of the nine portfolio
locks now own a piece of what elects LOCK-4's production terminal. The "architecture settled AND partly
built" framing is honest **as architecture** and vacuous-adjacent **as convergence**: the lock cannot
elect a terminal internally, and the number of external verdicts that gate it MORE THAN DOUBLED this
pass. Return LOCK-4 as a rigorously converged CORE with its exact external gaps — never as converged.

**Three cross-lock hazards the 18:43 spec + 19:40/19:43 probes carry that neither the spec nor CRIT-1
fully names (my load-bearing additions):**

1. **[NEW, sharpest] TWO DIFFERENT `FnDescriptor::HostClosure` SHAPES were built across two worktree
   probes, and NEITHER is the arbitrary opaque closure the hard case demands.** PROTO-2 PART 2 built
   `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` (an opaque call-out, emitter plane). PROTO-2-live-
   differential built a *different* `HostClosure{kind: HostFold, coeffs: Vec<f64>}` (a CLOSED numeric
   fold, VM plane). These are incompatible type-plane shapes wearing one name, both `rg HostClosure`=0 of
   record, and the LOCK-1-TypeDesc/LOCK-7-lattice they pre-seed is un-adjudicated. "G-B3/G-D-GAP1
   discharged" and "non-stub HostClosure evaluated" ride an *inconsistent, unattributed, double-homed*
   diff (§5).

2. **[cross-lock contradiction] INCOMPAT-1 re-litigates 5B, which CROSS-LOCK XL-C has RATIFIED as
   RETIRED.** The spec routes 4D's Q-ownership as an OPEN "5A shared type vs 5B lawful functor" (open-Q1,
   INCOMPAT-1). XL-C: *"5B's functor is RETIRED (conditional re-open only if LOCK-1 abandons Map-barrier
   typing)."* Presenting a settled adjudication as the live fork is a constitution-ix violation the
   critic must name (§4, §6.5).

3. **[the LOCK-6-facing contribution — the one cross-lock reason 4C exists — is UNRUN]** 4C's charter
   value is the cross-language isomorphism-of-record (the only place constitution-iii is satisfiable by
   construction). The live-differential ran **in-process in Rust** — it is NOT the TS crossing. G-C1
   (canada, cross-language) is still DAG-blocked on porting `parse_that::regex::Dfa` to TS, and that port
   has **no owning lock on the hand-off list** (candidate SIXTH hand-off, §6.4). LOCK-6 is waiting on
   evidence LOCK-4 said it would produce; the probe produced only the Rust half (§3).

---

## 1. FAMILY 4A — CLOSURE-TO-GRAPH · **convergence 72%** (pass-2 crit-2: 50%)

**Why up, and it is earned.** G-A1 — 4A's one novel claim, SPEC-ONLY for four passes — **ran and PASSED**
(PROTO-1-4A-GA1). A combinator-style smart-constructor builder over the json grammar flows through
`fuse_single_use` + `fuse_token_dispatch` to a graph byte-identical to the grammar-text reference (0/10
pre- and post-fuse), identical 37-node interned DAG, twitter-parity. The elegant-reduction trap flagged
across three passes does not bite this test. I credit it fully. The +22 over my pass-2 figure is the
real running artifact I marked "unrun" then.

**Why capped at 72 — the cross-lock-hard half is exactly what the probe sidestepped.** The probe's
"combinator front" is a **bbnf-side 85-LOC `IrNode` builder**, NOT parse-that's real combinator API. The
passes key on the `IrNode` enum discriminant, so provenance-agnosticism was near-certain a priori — the
*lower-risk* half (CRIT-1 §4A.1 concurs). The **cross-lock-hard** step — bridging parse-that's shipped
`SmallBox<dyn ParserFn>` surface (`parse.rs:74`) to emit `IrNode` **across the `parse_that ↔ bbnf_ir`
crate seam** (the cycle I re-confirmed live at `Cargo.toml:13`) — is untouched. That seam is the whole
INCOMPAT-1 problem; the probe proved fuse-inheritance on a graph that never crossed it.

**Cross-lock:**
- **[LOCK-3 obligation, XL-B, unrun]** open-Q2 (reify→serialize→SAME-emitter→graph-of-record parity) is a
  LOCK-3 tower-parity build, correctly exported, zero evidence. PROTO-1 proved the grammar-TEXT→emitter
  path generalizes; the runtime-graph→SAME-emitter parity path — the one 4A's source-front role actually
  rides — never ran, and its home is LOCK-3's, not LOCK-4's.
- **[LOCK-8 consumer hazard]** the node-barrier census FAILS on value.js math (4.84% > 2%; 1.857% repo
  aggregate). value.js is LOCK-8's flagship consumer (8D consumer-contract-first). "Analyzable graph"
  reads as "skeleton with opaque leaves" exactly on the grammar LOCK-8 designs backward from — the
  analyzability thesis is weakest on the one consumer whose suite gates the TS plane.

**Exact open gaps:**
1. **Real-API reification across the crate seam is unbuilt** — the probe used a bbnf-side builder; the
   INCOMPAT-1 seam (parse-that surface → `IrNode`) is where the sharpest 4A uncertainty lives, unmoved.
2. **CSS arm of G-A1 unrun; `fuse_token_dispatch` was a no-op on json** — token-dispatch/`Map{fn_id}`/
   `@pretty` inheritance proven only trivially (identical early-return); `fuse_single_use` alone fired.
3. **Reify→SAME-emitter→graph-of-record parity (open-Q2) = a LOCK-3 tower-parity obligation (XL-B)**, not
   self-dischargeable; zero evidence.
4. **Node-ratio FAILS on LOCK-8's flagship consumer** (4.84% math) — repo aggregate, not an invariant.
5. **The ≥0.98× timing arm is near-vacuous** (byte-identical IR ⇒ identical bytecode; the probe itself
   calls it "a confirmation, not the crux") — not an independent gate.

---

## 2. FAMILY 4B — COMPTIME-FUSION · **convergence 66%** (pass-2 crit-2: 58%)

**Why up.** Terminal role SETTLED as the codegen fast face (Rust monomorphization + TS emitter). G-B0
PASS (recursive JSON, 0 box/dyn/fn-ptr, ZST parser — PROTO-1 GATE A); G-B3 PASS (arbitrary host closure
fused as 1 named call-out). It is the lock's only running fast face on both languages and the closest to
"converged core with exact external gaps." Neither post-spec probe touched 4B; its disposition is the
16:24/18:43 reading, internally sound.

**Why capped at 66 — the terminal is the MOST externally-bound in the lock, and the gate-that-cannot-fail
is on its FOURTH pass unset.** This is the cross-lock crux of the whole lock:

- **[LOCK-9, the election ceiling — EXOGENOUS] canada 1.50–1.60× REGRESS is UNCHANGED after the numeric
  probe** (PROTO-1-numeric-leaf FAILED; SWAR fold straddles canada's short digit runs, falls to scalar
  tail). The fix — `number_simd` 16-byte SIMD stripe + `PaddedView` + `get_unchecked` — is **LOCK-9's 9B
  NATIVE-REBIRTH plane**, un-exported, plus a **LOCK-6/8 padded-input `parse()` contract**. 4B cannot
  elect inside LOCK-4 even in principle; its election is **three-lock-away**.
- **[XL-H base-law, deeper] The G-B1 baseline is CROSS-LOCK-UNSTABLE.** The MIXED verdict is measured
  against the SHIPPED `json_parser()`, whose f64 leaf IS the `number_simd` LOCK-9 9B is actively
  rebirthing. The baseline is a MOVING TARGET owned by another lock — worse than a stale HEAD, it is a
  baseline whose owning lock is mid-rewrite. The MIXED verdict must be re-based when 9B lands; carrying it
  as "the measured ceiling" is base-law-adjacent risk the spec flags (correctly) but the % must reflect.

**Exact open gaps:**
1. **Election is EXOGENOUS** (LOCK-9 9B kernel + LOCK-6/8 padded-input re-arch) — canada 1.56× is the
   ceiling until 9B lands; the win is string/structure/small ONLY (citm is a TIE, not a win).
2. **Canada-via-padding is SPEC_ONLY** — the SIMD-stripe re-architecture is an unbuilt hypothesis
   presented as the known ceiling; no artifact (constitution-vii, prototype-evidence misuse).
3. **G-B4 firing budget: NO number, FOURTH pass** — the canonical gate-that-cannot-fail. Anchors exist
   (fuse-arity 8, `try_factor_alt` at 4, one box/SCC back-edge, `.rlib`/`dist`+incremental) but no
   measured curve on CSS + value.js; the `generated-size-budget` edict is cited, no number produced.
4. **G-B3 rides an unattributed + inconsistent LOCK-1-TypeDesc pre-seed** (`rg HostClosure`=0; two
   different shapes across probes — §5); core emission arms (`driver/map.rs` + `emit_host_closure`
   default) "patch-plausible, NOT cold-built."
5. **XL-E supersession has no PT-tranche home** (constitution-ix debt → GESTALT-P) — owed since pass 1.

---

## 3. FAMILY 4C — VM-CORE / PORTABLE-PROGRAM ISO · **convergence 62%** (pass-2 crit-2: 32%) · BANKED

**Why the large jump.** PROTO-2-live-differential ran the exact McKeeman gate pass-2 CRIT-2 §8 demanded,
and ran it *right*: because `parse_that="0.4"` is a first-class dep of `crates/ir`, parse-that's SHIPPED
`.map`/`.many` runs **in the same Rust test binary** — the oracle is parse-that's actual live return
value, not an author golden. 12/12 census pairs, 0 divergences; the pass-through stub retired for a
`HostFold` numeric fold (`Lerp`/`ChannelScale`, incl. the `600→255` clamp), proven evaluated by the
`F64`/`U32` discriminant. The faked-golden trap is genuinely fixed for the closed surface. +30 over my
pass-2 figure — earned, and correctly aimed at the family's sharpest uncertainty.

**Why capped at 62 — the CROSS-LANGUAGE half, which is the ONLY reason 4C exists cross-lock, is UNRUN;
and the build is LOCK-3-homed ISA work under a LOCK-4 probe.** 4C is BANKED (measured-refuted twice as a
fast terminal: switch-VM −22%, interpret 0.021–0.46×). Its surviving cross-lock value is the
isomorphism-of-record oracle that feeds LOCK-6 (behavioral-iso, 6D) and is consumed by LOCK-3's tower.
That value advanced ONLY on the Rust side:

- **[LOCK-6, unrun] G-C1 (the canada cross-language crossing) never ran.** The differential is in-process
  Rust, NOT a TS re-decode. The 6D behavioral-iso evidence LOCK-6 needs — decode-in-TS, `Value`-tree
  identity over six R3 corpora incl. **canada** (the mandatory adversarial member where PROTO-1 shows the
  f64 leaf diverges) — is DAG-blocked on porting `parse_that::regex::Dfa` to TS (`bytecode.rs:184/238`,
  re-confirmed live). LOCK-4's headline cross-lock deliverable is half-built.
- **[LOCK-3 double-home, XL-B] `Op::ApplyFn` + `fns: Vec<FnDescriptor>` + `Value` scalar arms is an ISA +
  value-model change in `crates/ir/src/vm/`** — LOCK-3's tower-shape home per XL-B. The spec routes it
  "LOCK-3 FIRST" (hand-off #3, correct) — yet the probe BUILT it in a worktree anyway. Prototyping is
  legitimate; but "the value plane is correct-of-record" (PROTO-2-live-differential VERDICT) overstates:
  it is correct-of-*worktree*, homed in a lock (LOCK-3) that has not ratified the tower-shape change and
  is running its own pass-3+ concurrently. If LOCK-3 designs the VM value plane independently, the two
  collide.
- **[the corpus is author-selected]** the live oracle is real, but the *corpus* is 12 hand-picked
  projection kinds, NOT a real compiled grammar (`css/pretty.bbnf`) crossed full-value-tree — a milder
  residue of the selection concern (CRIT-1 §4C.1 concurs).
- **[G-C4 unfired on its real bus]** wire-contract idempotence fired on `serde_json`, NOT
  `rmp_serde::to_vec_named` (the MessagePack bus it ships on); the born-RED externally-tagged-reorder
  guard never fired where it ships.

**Exact open gaps:**
1. **G-C1 canada / full-corpus TS crossing UNRUN** — DAG-blocked on the `Dfa` TS port (no owning lock,
   §6.4); the LOCK-6-facing 6D evidence is the family's raison d'être and it is half-built.
2. **The arbitrary opaque closure is UNEVALUATED** — `HostFold` is a closed 2-fold set, NOT the
   `Opaque(HostFnId)`/`Value::Host` inhabitant value.js's 3 easing closures (which RETURN a host function)
   demand; the McKeeman "non-stub" claim rides a closed numeric substitute for the hard case (§5, §6.2).
3. **Value plane worktree-only + LOCK-3-homed** (`rg ApplyFn`=0); ISA home is LOCK-3's (XL-B), un-ratified.
4. **Corpus is author-selected projection kinds** — no real compiled grammar crossed full-tree.
5. **G-C4 msgpack idempotence unfired** on `rmp_serde::to_vec_named`; born-RED reorder guard never fired.

---

## 4. FAMILY 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **convergence 58%** (pass-2 crit-2: 30%) · BLOCKED (split)

**Why up.** Sound disposition, honestly BLOCKED/split. Terminal refuted three horns (i = full TS
grammar-engine port = primitive of equal-or-greater difficulty AND XL-D retires the need; ii = PROVE-14
wasm full-walk 0.91× LOSS on exactly the value-producing workload; iii = shared decoder −22% on V8).
Gate-0's regex arm severed by a 4-line re-export; the VM `Dfa` arm confirmed LOCK-5-gated on tree
(`bytecode.rs:184`). research-D's split-the-barrier finding (44/84 ~52% of latex-paper non-projection
sites recoverable as `Record`/`Array`/`Select` data; opaque residue = exactly 3 value.js easing closures)
is a genuine sharpening, and the spec correctly self-guards against "4D dominates on parsimony ⇒ elect."

**Why capped at 58 — the ownership fork the spec presents as OPEN is CONTRADICTED by the CROSS-LOCK
ledger, and the irreducible residue the whole thesis rests on is unbuilt.**

- **[cross-lock contradiction, XL-C] INCOMPAT-1 mis-invokes a RETIRED fork.** open-Q1 frames Q-ownership
  as "is `bbnf_ir` the genuinely shared HIR (**5A**) or a lawful functor across separate HIRs (**5B**,
  which evaporates the inheritance)?" XL-C **RATIFIED** the regex axis: two domain-fit types, cheap
  bridge, and *"5B's functor is RETIRED."* Two problems: (a) presenting 5B as a live binary contradicts a
  binding adjudication (constitution-ix — divergence must be a NAMED supersession, not a silent re-open);
  (b) 5A/5B are LOCK-5's **regex-HIR** family names — applying them to the **grammar-IR-sharing** question
  (does parse-that's combinator graph share `bbnf_ir`?) is a category slippage. The genuine open question
  (grammar-IR sharing) is real, but it is NOT "5A vs 5B," and routing it to LOCK-5 under retired-family
  labels mis-DAGs the hand-off. What IS genuinely LOCK-5's: the gate-0 VM `Dfa` extraction (confirmed).
- **[LOCK-7, the residue is unbuilt] The `Opaque(HostFnId)` + `Value::Host` inhabitant — the whole reason
  the barrier-split thesis needs a residue tier — is neither built nor evaluated.** PROTO-2-live-
  differential evaluated `HostFold` (numeric folds), which is MapExpr-adjacent, NOT the closure-RETURNING
  case. value.js's 3 easing closures (`easing.ts:124/142/153`, `EasingFunction = (progress)=>number`)
  remain the exact case no probe has executed. 4D's "split the barrier, `Opaque` carries only the residue"
  is proven for tier-1 (compile-plausible) and the numeric slice of tier-2 — the CLOSURE slice, LOCK-7's
  lattice member, is untested.

**Exact open gaps (predecessor-owned / report-only per Q8):**
1. **INCOMPAT-1 (grammar-IR sharing) → LOCK-5, but the "5A vs 5B" framing contradicts XL-C** (5B retired);
   re-label as a named supersession or route the grammar-IR-sharing question to its true home. Gate-0's VM
   `Dfa` arm IS a LOCK-5 extraction (confirmed on tree).
2. **HostClosure/`Value::Host` lattice → LOCK-7:** the closure-RETURNING residue (3 easing closures) is
   un-built and un-evaluated (shared with 4C gap 2).
3. **Node-ratio thin AND FAILS on value.js** (1.857% repo / 4.84% math) — fragile on LOCK-8's consumer
   (shared with 4A gap 4).
4. **Value-plane ISA home → LOCK-3 (XL-B) FIRST** (internal-descriptor vs host-closure split); un-ratified,
   built worktree-only under a LOCK-4 probe (shared with 4C gap 3).
5. **`FnDescriptor::HostClosure` type-plane pre-seed unattributed, un-homed (LOCK-1), and INCONSISTENT
   across two probes** (§5); constitution-v zero-runtime-cost untested (shared with 4B gap 4).

---

## 5. THE TWO-SHAPES HostClosure PROVENANCE MESS (my sharpest new cross-lock finding)

Three PASS/discharge claims across the lock — G-B3 (4B), G-D-GAP1 (4D), and the "non-stub HostClosure
evaluated" (4C) — all ride a `FnDescriptor::HostClosure` diff that is `rg HostClosure`=0 of record. That
much the spec and CRIT-1 name (unattributed LOCK-1-TypeDesc pre-seed). **What neither names: the diff is
not one shape — it is TWO INCOMPATIBLE shapes wearing one name, and neither is the arbitrary opaque
closure the hard case demands.**

| probe | shape built (worktree) | what it is | what it is NOT |
|---|---|---|---|
| PROTO-2 PART 2 (emitter) | `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` | an opaque call-out reference (LOCK-7-lattice-shaped) | not evaluable in-VM |
| PROTO-2-live-differential (VM) | `HostClosure{kind: HostFold, coeffs: Vec<f64>}` | a CLOSED numeric fold (evaluable, serializable, NON-opaque) | not a reference, not opaque, not the arbitrary closure |
| research-B/D proposal | `HostClosure{fn_id, arg_types, ret_type}` → host `fn_table` | the arbitrary opaque residue + `Value::Host` handle | **built by neither probe** |

The cross-lock consequences:
- **The name "HostClosure discharged" is misleading across three gates.** It reads as "the arbitrary
  opaque barrier is crossed." It is not. The emitter probe fused a call-out (body opaque, un-evaluated);
  the VM probe evaluated a closed numeric fold (not opaque at all). The genuinely irreducible case — a
  parse that *carries* a closure it cannot introspect — is unbuilt.
- **The `HostFold` closed-fold pre-empts LOCK-7's lattice with a shape LOCK-7 did not sanction.** LOCK-7
  owns the host-fn lattice (Q8; nothing in XL-A..H ratifies a shape). A closed `{kind, coeffs}` numeric
  descriptor invented under a LOCK-4 probe is a third design point neither LOCK-1 (TypeDesc) nor LOCK-7
  (lattice) authored, landed to make a McKeeman gate go green.
- **Constitution-v (zero-runtime-cost) is untested for BOTH shapes**, and they cannot both be the
  type-plane truth. Attribution + homing (LOCK-1 TypeDesc + LOCK-7 lattice) must resolve which shape — if
  either — is of record before any gate riding it counts.

This is not fatal to the mechanism (the VM *can* evaluate a descriptor; the emitter *can* fuse a
call-out). It is fatal to reading G-B3/G-D-GAP1/the-non-stub-claim as convergence: three gates pass on a
diff that is unattributed, un-homed, inconsistent, and — for the hard case — a substitute.

---

## 6. CROSS-LOCK VIEW (critic-2 weight)

### 6.1 — [governing] The terminal is bounded by FIVE external verdicts (was two) — LOCK-4's convergence ceiling FELL this pass even as the families rose.
Pass-2: two forks (LOCK-5 ownership, LOCK-6 iso). Pass-3 hand-off list: (1) Q-ownership → LOCK-5; (2)
cross-language iso → LOCK-6 + evidence to LOCK-8; (3) value-plane ISA home → LOCK-3 (XL-B); (4) 4B
perf-claim supersession → GESTALT-P; (5) numeric ceiling → LOCK-9 + LOCK-6/8 padded-input. Plus two
provenance debts (HostClosure pre-seed → LOCK-1; G-B1 baseline cross-lock-unstable under LOCK-9). Seven of
nine locks now gate LOCK-4's terminal. This is honest DAG accounting — and it means the lock-level
"settled AND partly built" framing must NOT be read as terminal-convergence. Return the core with the
five external gaps enumerated; do not let a gestalt read the running-evidence rise as election-readiness.

### 6.2 — [double-home, LOCK-3] The value-plane ISA was BUILT (worktree) inside `crates/ir/src/vm/` under a LOCK-4 probe.
`Op::ApplyFn` + `fns: Vec<FnDescriptor>` + `Value` 4→8 is a tower-shape change LOCK-3 owns per XL-B. The
spec routes it "LOCK-3 FIRST" (hand-off #3, correct) — but the probe landed it worktree-first to
discharge the McKeeman gate. The prototype is fine; the framing "correct-of-record" is not — it is
correct-of-worktree in LOCK-3's home, un-ratified, and LOCK-3 is running its own pass concurrently.
GESTALT-Ω arbitrates only the internal-descriptor (→LOCK-3) vs host-closure (→LOCK-7) split; the ISA
change itself is LOCK-3's to shape, not LOCK-4's to pre-build.

### 6.3 — [double-home, LOCK-1 + LOCK-7] The HostClosure/HostFold pre-seed is TypeDesc + lattice work, unattributed, inconsistent (§5).
Two shapes, both `rg HostClosure`=0, neither the arbitrary opaque residue. LOCK-1 owns TypeDesc; LOCK-7
owns the lattice. Three gates ride it. This is the pass's cleanest example of the checklist's
"unattributed pre-seed anchoring a gate."

### 6.4 — [NEW, candidate SIXTH hand-off] The TS `Dfa` port that unblocks G-C1 has NO owning lock.
G-C1 (the cross-language crossing, LOCK-6's 6D evidence) is "DAG-blocked on porting `parse_that::regex::
Dfa` to TS." research-C §8 correctly exports the Dfa-vs-`RegExp` *divergence* risk to LOCK-5 (certify the
grammar-lowerable regex class). But the ACT of porting `Dfa` to TS — the build task that makes G-C1
runnable at all — is un-homed. Per XL-C the canonical regex engine home is parse-that; the TS regex face
is LOCK-8/LOCK-5-TS/LOCK-9-via-5C territory. Right now it floats as "a blocker" with no lock. Name its
home, or G-C1 (and thus LOCK-6's behavioral-iso evidence) stays permanently DAG-blocked.

### 6.5 — [contradiction, XL-C] INCOMPAT-1's "5A vs 5B" contradicts the retired-5B ledger (§4, gap 1). Re-label or re-home.

### 6.6 — [clean, credit where due] INCOMPAT-2 → LOCK-6 is handled RIGHT, and the 15:22 overreach is self-corrected.
The 18:43 spec withdraws the earlier "away from 6A/6B" overreach (declaring two LOCK-6 families dead is
LOCK-6's election). It exports the EVIDENCE (Rust monomorphization ≠ TS emission ⇒ different mechanisms,
no shared isomorphic terminal) and lets LOCK-6 rule; routes PROTO-1's emitter/V8-discipline result to
LOCK-8 so it is not re-derived. XL-D (runtime decoders refuted, emitter is fast TS face) is carried as
binding. This is exemplary cross-lock discipline — I flag it as the model the other hand-offs should meet.

### 6.7 — [LOCK-8 consumer hazard, under-weighted] The analyzability thesis FAILS on value.js, LOCK-8's flagship.
1.857% repo / 4.84% math. LOCK-8 (8D) designs its API backward from value.js. 4A/4D's "fully analyzable
graph" is safe in aggregate and fragile on exactly the consumer whose suite is the terminal falsifier
(priorart §5). This is not a LOCK-4 defect — it is a cross-lock risk LOCK-8 must be handed explicitly, not
buried in a node-ratio footnote.

### 6.8 — [consumer-less-substrate, adjacent] The value plane has no PRODUCTION-execution consumer by construction.
The fast face is codegen (born with the arena; no value plane). The value plane's only consumers are the
iso-oracle (LOCK-6-facing) and the interpret/LSP/DAP lane (LOCK-3/LOCK-8 territory). Not consumer-less in
the strict Y.13 sense — but every consumer lives OUTSIDE LOCK-4, reinforcing that the value plane is
LOCK-3's substrate prototyped here (§6.2). The spec discloses this (4C open-Q3); the % must reflect that
"built" ≠ "on any LOCK-4 hot path."

---

## 7. CONSTITUTION & FAILURE-MODE LEDGER (cross-lock weight)

- **Vacuous convergence:** probe-level gates are refutable (BRIEF-1 FAILED, both new briefs could have
  failed) — not vacuous at the probe. **Lock-level "settled AND partly built" is vacuous-adjacent:** the
  terminal is un-electable (five external verdicts), 4C is BANKED, 4D is BLOCKED-split, 4B's election is
  three-lock-away. Convergence % must encode terminal-blockage, not architecture-folding.
- **Spec-cites-itself / circularity:** substrate facts independently re-verified this session (rg on
  `06e70053f`) — NOT self-citing. Milder residue: the McKeeman corpus is author-selected projection kinds
  (live oracle, hand-picked corpus) — a selection concern, not a faked golden.
- **Gates that cannot fail:** (i) **G-B4 firing budget — no number, FOURTH pass** (the canonical one,
  untouched by either probe). (ii) PROTO-1-4A-GA1's ≥0.98× timing gate is near-vacuous (byte-identical IR
  ⇒ identical bytecode). Both honestly labeled; both still unset/vacuous.
- **Elegant-reduction trap (TWO live forms):** (i) the terminal = "fold the mechanisms [done] and-then
  FIVE other locks decide the fork [unconverged, external]" — the load-bearing step is five other locks;
  (ii) the value plane = "built [worktree] and-then it computes the right values" where the hardest case
  (arbitrary opaque closure) is a **closed numeric-fold substitute** (`HostFold`), not the real thing (§5).
- **Masked fallback:** "non-stub HostClosure evaluated" (PROTO-2-live-differential VERDICT) masks that the
  evaluated thing is a closed numeric fold, not the arbitrary opaque closure — a narrower artifact standing
  in for the hard one, functioning as a green light for a gate the hard case does not clear.
- **Legacy aliases:** none new.
- **Unverified gestalt / pre-empted authority:** the value-plane ISA (LOCK-3) and the HostClosure/HostFold
  type-plane (LOCK-1/LOCK-7) were built under LOCK-4 probes ahead of the owning locks' ratification (§6.2,
  §6.3). Prototyping is legitimate; "correct-of-record"/"discharged" framing over the owning-lock's
  un-ratified home is not.
- **SPEC_ONLY anchoring a claim:** (i) 4B's canada-via-padded-input is unbuilt, presented as the known
  ceiling; (ii) G-B3's core emission arms "patch-plausible, not cold-built."
- **Consumer-less substrate (viii):** the VM has live consumers (DAP, JSON bench) — compliant. The value
  plane's consumers all live outside LOCK-4 (§6.8) — compliant but home-revealing.
- **Isomorphism (constitution iii):** advanced on the Rust side (12/12 in-process), UNMOVED on the
  cross-language crossing (G-C1 DAG-blocked). LOCK-4's strongest iso claim is half-tested; the LOCK-6-facing
  half is the unbuilt half.
- **Extant-tranche consciousness (ix):** (a) INCOMPAT-1 re-litigates retired-5B (XL-C) without naming a
  supersession — a live violation (§4, §6.5); (b) XL-E's PT-tranche home still owed (4B gap 5).
- **Base-law (XL-H):** both new probes base-pin `ce51c36fc`, worktree-only (`rg ApplyFn`/`rg HostClosure`=0
  re-confirmed on `06e70053f`). Deeper: 4B's G-B1 baseline is not merely a HEAD — it is a MOVING TARGET
  owned by LOCK-9 mid-rewrite (§2). "PASS" reads as correct-under-live-differential / byte-identical-fuse
  in a scratch worktree, NOT landed of record, NOT cross-language, NOT re-based against LOCK-9's 9B.

---

## 8. VERDICT & PASS-4 DIRECTIVES (cross-lock)

No family converged; pass 3. Both post-spec probes RAN and PASSED — a real advance the families' rising %
reflects. But the cross-lock reading governs: **the terminal moved further out of LOCK-4's hands** (two
external verdicts → five, plus two provenance debts), two builds are LOCK-3/LOCK-1/LOCK-7 work homed under
LOCK-4 probes, the HostClosure pre-seed is two inconsistent shapes neither of which is the hard case, and
the one cross-lock deliverable that justifies 4C (the cross-language iso LOCK-6 needs) is half-built.

**Per-family convergence (critic-2, pass-3, cross-lock): 4A 72% · 4B 66% · 4C 62% (BANKED) · 4D 58%
(BLOCKED-split).** Leadership by axis: **4B leads terminal-readiness** (running fast face both languages,
exact external gaps) but is the most externally-bound (three-lock-away election, no G-B4 number); **4C
leads the iso-of-record** but only on the Rust half; **4A leads analyzability-readiness** but its real-API
cross-seam step and the LOCK-3 parity obligation are unbuilt; **4D leads parsimony** but conditional on a
fork whose framing contradicts XL-C.

Pass-4 must:
1. **Fold the 19:40/19:43 PASSes into a re-synthesis and STOP emitting briefs the same pass executes** —
   the artifact of record is perpetually one-to-two probes behind its evidence; the REGISTRY (16:24) is
   older still. GESTALT-P/Ω will mis-DAG LOCK-4 if it reads "0% built for a FOURTH pass" (4A) or 66-70% as
   the state (CRIT-1's governing within-lock finding; I add its cross-lock cost — the ledger and sibling
   locks consume a stale LOCK-4 disposition).
2. **Resolve the HostClosure two-shapes mess (§5)** — attribute the pre-seed (LOCK-1 TypeDesc), home the
   lattice (LOCK-7), pick ONE shape of record, verify constitution-v, and stop reading three gates as
   passed on an inconsistent un-homed diff.
3. **Re-label INCOMPAT-1** — 5B is RETIRED per XL-C; either name a supersession with evidence or route the
   grammar-IR-sharing question (distinct from the regex-HIR 5A/5B axis) to its true home.
4. **Name the TS `Dfa` port's owning lock (§6.4)** or G-C1 — and LOCK-6's behavioral-iso evidence — stays
   permanently DAG-blocked.
5. **Cross canada cross-language + evaluate a TRUE opaque closure** (value.js easing, not a numeric
   HostFold); fire G-C4 on `rmp_serde::to_vec_named`.
6. **Give G-B4 a number** (`.rlib`/`dist` + incremental-compile vs fuse-arity on CSS + value.js) or record
   it un-gateable and carry canada 1.56× as the exogenous LOCK-9-owned ceiling, re-based when 9B lands.
7. **Hand LOCK-8 the value.js analyzability-FAIL explicitly (§6.7)** — do not bury it in a node-ratio
   footnote; it is LOCK-8's flagship consumer.

**Return LOCK-4 as a rigorously converged CORE with its five external gaps (LOCK-5 ownership-semantics;
LOCK-6 cross-language iso; LOCK-3 value-plane ISA home; LOCK-9 numeric ceiling + moving baseline; LOCK-7
host-closure lattice) + two provenance debts (LOCK-1 pre-seed attribution; XL-E PT-tranche home) — never
as a terminal.**
