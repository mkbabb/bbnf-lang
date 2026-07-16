# LOCK-4 — THE COMBINATOR CORE · PASS-3 · CRIT-2 (adversarial critic 2 of 2, CROSS-LOCK weighting)

*Read: PORTFOLIO/CONSTITUTION, CROSS-LOCK (XL-A..H), FAMILY-SPECS.md (15:22), research-{A,B,C,D,priorart},
PROTO-1-numeric-leaf.md (15:38), PROTO-2-vmvalue.md (15:54), CRIT-1.md. Tree-of-record spot-checks on
`c43126422` / base-pin `ce51c36fc`; cross-lock ownership verified against LOCK-9 REGISTRY. Critic-2 weights
where this lock CONTRADICTS or DOUBLE-HOMES work against the GN/PT codex and the other locks — per charter,
the LOCK interactions live in the portfolio and the CROSS-LOCK adjudications are binding facts, not re-litigable.*

**I ratify CRIT-1's two governing within-lock findings and do not re-derive them:** (a) the FAMILY-SPECS is
STALE against its own two follow-on probes — BRIEF-1 (numeric leaf) RAN → FAIL, BRIEF-2 (4C value plane) RAN →
BUILT — neither folded into any convergence framing; (b) the 4C correction (value plane UNBUILT on HEAD) is
tree-accurate. My percentages are computed against the probes AND against the cross-lock ledger. Where I
differ from CRIT-1 I mark it; my net is one to five points below CRIT-1 on every family because the cross-lock
un-exports are unaccounted debt CRIT-1 scoped as internal.

---

## THE FINDING THAT REORGANIZES THIS CRITIQUE — the hand-off list is INCOMPLETE; two of LOCK-4's live gaps are DOUBLE-HOMED into other locks and named as "internal next probes"

FAMILY-SPECS §"WHAT REMAINS UN-ELECTABLE" enumerates four hand-offs (Q-ownership→LOCK-5;
fast-terminal-iso→LOCK-6/8; value-plane-home→GESTALT-Ω; perf-claim→PT-tranche). **Two of the lock's sharpest
live gaps are missing from that list because the spec homes them inside LOCK-4:**

1. **The numeric-leaf election ceiling is LOCK-9's kernel, not "4B open-question 1."** FAMILY-SPECS calls
   canada parity "the single lever between mixed and elect 4B" and files it as an internal LOCK-4 next probe
   ("the next probe, not this one"). **Verified on the tree:** the fix PROTO-1-numeric-leaf names — the 16-byte
   SIMD digit stripe (`scan_digits_simd`) + `get_unchecked` on a NUL-padded `PaddedView` — lives in
   `parse-that/rust/parse_that/src/parsers/scan/number_simd.rs` + `number.rs` + `number_f64.rs`, which is
   **LOCK-9's 9B NATIVE-REBIRTH plane.** LOCK-9's REGISTRY (9B, 62%, "the only family that moved") is *at this
   moment* rewriting `number_simd.rs:54-55`. So 4B's election lever is a **DAG dependency on LOCK-9 that
   FAMILY-SPECS neither names nor exports.** The correct disposition is hand-off #5 → LOCK-9 (the number
   kernel) + a LOCK-4/LOCK-9/LOCK-6 seam (the padded-input `parse()` contract). Filing it as an internal probe
   is a **double-home** and understates 4B's distance from election.

2. **The G-B1 "identical parse_that core, both arms" baseline is a MOVING TARGET under LOCK-9.** PROTO-1's
   apples-to-apples rests on `.cargo/config.toml` redirecting `parse_that = "0.4"` (crates/ir/Cargo.toml:13, a
   version pin) to the **live** `../parse-that/rust/parse_that` path. That "incumbent" `json_parser()` number
   path is the very `number_simd` LOCK-9 is rebirthing. **The moment LOCK-9 9B lands its `vshrn`/`vpaddq`
   rewrite, the G-B1 incumbent shifts and canada 1.56× is re-based** — the measured MIXED verdict is pinned to
   a HEAD LOCK-9 is actively moving. XL-H (base-law) is satisfied for the *snapshot*; the cross-lock stability
   of the comparison is not. This is unremarked in FAMILY-SPECS and in CRIT-1.

**The consequence:** 4B's "one named lever stands between mixed and elect" is not merely refuted by its own
probe (CRIT-1's finding) — the residual lever is **owned by another lock and un-exported**, so 4B cannot
elect inside LOCK-4 *even in principle*, and its convergence ceiling is bounded by LOCK-9's cutover, not by a
LOCK-4 probe. Every "4B is the strongest family, election is close" reading double-counts work LOCK-9 owns.

---

## 4A — CLOSURE-TO-GRAPH · **68%**

**Ratifying CRIT-1:** role correctly demoted to runtime-front reifier (not terminal); the G-A1 elegant-reduction
trap ("the passes already exist" — true for grammar-text, unbuilt for the combinator front, four passes) is
real and load-bearing. I do not re-derive it.

**CROSS-LOCK addition — 4A's coherence condition double-homes into LOCK-3's tower.** Open-Q2 (the coherence
condition for 4A-as-source-front) is: the reified *runtime* graph serializes and feeds **the same emitter** as
the grammar-text path, producing graph-of-record parity. That reified graph IS `bbnf_ir::IrNode`, and the
emitter is `crates/core/src/backend/rust/emitter/` — **both LOCK-3's IR-transposition-spine home (XL-B: LOCK-3
owns the middle-IR tower's shape and the decision-driver seat).** 4A's one unbuilt coherence claim therefore
cannot be discharged by a LOCK-4 probe alone; it is a LOCK-3 tower-parity obligation. FAMILY-SPECS treats the
emitter as ambient infrastructure; per XL-B it is owned, and 4A's reify→same-emitter equivalence is a
LOCK-3-gated round-trip, not a within-4A build.

**CROSS-LOCK addition — the node-ratio FAIL region is LOCK-8's flagship consumer.** PROTO-2 PART 1: 1.857%
opaque nodes repo-wide but **4.84% math-only (FAIL).** The math-dense grammar that breaks the ≤2% floor is
**value.js** — which is LOCK-8's primary consumer-contract surface (8D CONSUMER-CONTRACT-FIRST) and the
lattice input LOCK-7 must type. So 4A's "analyzable graph" thesis fails precisely on the grammar family the two
downstream locks are designed around, not on a peripheral corner. CRIT-1 flagged the FAIL; I weight *where* it
lands.

**Open gaps (enumerated):**
1. **G-A1 unbuilt (4th pass):** `fuse(Graph)→Graph` on a combinator-constructed graph byte-identical to a
   hand-fused arena — zero running evidence, four passes. The single novel 4A claim.
2. **Reify→serialize→same-emitter equivalence (open-Q2) is a LOCK-3 tower-parity obligation, not exported.**
   The emitter and the IrNode are LOCK-3's (XL-B); 4A cannot self-discharge coherence.
3. **The "analyzable graph" thesis FAILS on value.js math (4.84%) — i.e. on LOCK-8's flagship consumer and
   LOCK-7's lattice input.** Repo-aggregate pass (1.857%, K=13.78, 7.7% cushion) is not a per-grammar
   invariant on the exact surface downstream locks consume.
4. Open-Q3 (4A-TS graph-interpreter refuted by pass-2's −22%) correctly folds; 4A-TS must lower through 4B-TS.

---

## 4B — COMPTIME-FUSION · **70%** (strongest family; but the residual lever is LOCK-9's and un-exported)

**Ratifying CRIT-1:** G-B0 PASS + G-B3 PASS are the lock's only running fast face on both languages; the
election lever FAILED (canada 1.56× after the numeric probe, unfolded in the frozen spec); G-B4 firing budget
has no number across three passes (the gate-that-cannot-fail); G-B3's core arms are "patch-plausible, not
cold-built." Ratified, not re-derived.

**CROSS-LOCK, the governing addition (see the reorganizing finding above):** the residual canada ceiling is
**LOCK-9's `number_simd` kernel + a LOCK-9/LOCK-6 padded-input seam**, mis-filed as an internal 4B probe. This
makes 4B's ceiling exogenous: 4B cannot beat SOTA on canada without LOCK-9's kernel and a `parse()`
input-contract re-architecture that re-bases EOF across every leaf (string/object/array) — the latter is a
LOCK-6 isomorphism concern (the emitted parser's input contract must match on both language faces) and a
LOCK-8 V8-discipline concern. **The election is not "one probe away"; it is three-lock-away.**

**CROSS-LOCK — XL-E's supersession obligation is UNFULFILLED (constitution ix).** 4B open-Q3 and CRIT-1 gap 6
both note "no PT-tranche home named." XL-E settles the *principle* (4B's compile-time win ≠ PROVE-09's runtime
refutation) but a named supersession with no home in the PT codex is an **open constitution-ix debt**, not a
closed one. Until GESTALT-P places it in a PT-tranche letter, 4B's headline Rust perf claim (indirect-call +
codegen axis, 1.16–1.27× cold-paired carried from pass-2) sits beside PT-2's PROVE-09 zero-claim posture with
only a ledger note, not a codex placement. The obligation is live.

**CROSS-LOCK — G-B3's type-plane rests on an UNATTRIBUTED pre-seed touching LOCK-1's TypeDesc.**
`FnDescriptor::HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` is a **type-plane** construct (its arg/ret are
`TypeDesc` — LOCK-1's solver territory) that also lands in `crates/core/backend` + `crates/ir/passes`. PROTO-2-vmvalue
states this diff was **"pre-seeded … those are not mine and are excluded from the patch."** So the type-plane
change anchoring BOTH G-B3 (4B) and G-D-GAP1 (4D) PASSes is of **unknown provenance**, not authored or
cold-built in either probe of record. Constitution vii (concrete-artifact-or-SPEC-ONLY) is stretched: two gate
PASSes ride a diff neither probe owns, whose home is LOCK-1's type plane. This is beyond CRIT-1's "patch-plausible"
flag — it is a provenance + double-home flag.

**Open gaps (enumerated):**
1. **Election lever FAILED and its residual is LOCK-9-owned + un-exported.** Canada 1.56×; the fix
   (`number_simd` SIMD stripe + padded `PaddedView` + `get_unchecked`) is LOCK-9's 9B plane, not a LOCK-4 probe.
   Hand-off #5 (→LOCK-9 kernel + LOCK-6/8 padded-input contract) is missing from the spec's hand-off list.
2. **G-B1's "identical incumbent core" baseline is a moving target under LOCK-9's active `number_simd` rebirth**
   — the MIXED verdict is pinned to a HEAD LOCK-9 is moving; re-base risk unremarked.
3. **G-B4 firing budget: no number, three passes** — the monomorphization-blowup gate (constitution vi,
   generated-size budget) cannot fail as written; owed on CSS **and** value.js.
4. **XL-E supersession has NO PT-tranche home (constitution-ix debt, open).**
5. **G-B3/G-D-GAP1 rest on an unattributed pre-seed in LOCK-1's TypeDesc plane** — provenance + double-home;
   neither probe cold-built the emission arms.
6. Even discounting canada, the win is narrow: data/twitter WIN, citm **TIE**; "beyond-SOTA" met only on
   string/structure/small.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · **66%**

**Ratifying CRIT-1:** the value plane got BUILT in a worktree (Op 30→31 `ApplyFn`, Value 4→8, `fn_id` emitted,
8/8 tests, Oracle A GREEN) — real uplift over "0% built" — BUT (a) G-C0's *real* gate (VM output vs
parse-that's live closure `.map`) was NOT tested; Oracle A's "independently specified golden" is the McKeeman
faked-golden the research forbids; (b) HostClosure is a pass-through stub (the hard value case evaluates
nothing); (c) G-C1's mandatory adversarial member (canada / full corpus) is UNRUN. Ratified, not re-derived.

**CROSS-LOCK, the governing addition — the value plane was BUILT INTO LOCK-3's tower without LOCK-3
ratification.** `Op::ApplyFn` is an **ISA opcode**; extending `Value` with scalar arms is a **VM value-model
change**; both landed in `crates/ir/src/vm/` — which FAMILY-SPECS itself concedes is *"IR-VM machinery in
crates/ir (LOCK-3's home) under a LOCK-4 probe."* Per **XL-B (WORKING for pass 3+, final at GESTALT-B): LOCK-3
owns the middle-IR tower's SHAPE.** Adding an opcode to the ISA and inhabitants to the value model is a
**tower-shape change** — a LOCK-3 decision LOCK-4 executed under its own probe and then routed *straight to
GESTALT-Ω*, skipping LOCK-3. The value-plane-home disposition ("recommended split, GESTALT-Ω owes
ratification") is therefore **mis-routed**: XL-B gives LOCK-3 tower-shape ownership NOW, so the ISA extension
routes to LOCK-3 (pass-coordination) first, GESTALT-Ω only for the final internal/host split. Building ahead of
the owning lock's ruling is a "build-ahead-of-ownership" that the worktree-only status *mitigates* (not landed
on record, `rg ApplyFn`=0 on main HEAD) but does not cure — the disposition still bypasses LOCK-3.

**CROSS-LOCK addition — the un-run G-C1 corpus crossing is DAG-blocked on LOCK-5, not on LOCK-4.** PROTO-2-vmvalue's
own BOUNDARY: the six JSON corpora were not crossed because it "requires porting `parse_that::regex::Dfa` to
TS." That `Dfa` is LOCK-5's engine tier; `crates/ir/Cargo.toml:13` deps `parse_that` and
`vm/bytecode.rs:184/238` uses `parse_that::regex::Dfa`. So 4C's headline iso-of-record claim (the parser
crosses as data) is **half-proven for the value-plane descriptors and DAG-blocked on LOCK-5's regex substrate
for the recognizer plane** — and carries a latent bespoke-`Dfa`-vs-V8-`RegExp` divergence risk (LOCK-5/LOCK-6
territory) the instant the port lands. CRIT-1 named the un-run corpus; I home the blocker in LOCK-5.

**Open gaps (enumerated):**
1. **G-C0's real gate untested** — VM `ApplyFn` vs parse-that's live closure `.map` never differentially
   checked; the probe substituted a hand-authored golden (McKeeman trap). Semantic correctness of record open.
2. **The value plane was built into LOCK-3's tower (an ISA + value-model change) without LOCK-3 ratification;
   the disposition mis-routes to GESTALT-Ω, skipping the XL-B owner.**
3. **HostClosure value case is a pass-through stub** — the VM evaluates no arbitrary closure; byte-identity
   there is un-testable by construction. (The stub forwards to LOCK-7 territory, correctly, but the "value
   plane complete" reading overstates.)
4. **G-C1 canada / full-corpus crossing UNRUN — DAG-blocked on LOCK-5's `Dfa` TS port**, with a latent
   Dfa-vs-RegExp divergence risk homed in LOCK-5/LOCK-6.
5. **G-C4 msgpack idempotence unexercised** — the probe used `serde_json`, not `rmp_serde::to_vec_named`; the
   born-RED guard against externally-tagged-variant reorder never fired on the bus it ships on.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · **66%** (honestly BLOCKED/split; predecessor-DAG-bound — correctly)

**Ratifying CRIT-1:** 4D is soundly dispositioned — terminal role refuted three horns over (horn i = full
TS grammar-engine port is a primitive of equal-or-greater difficulty → BLOCKED-candidate AND XL-D retires the
need; horn ii = PROVE-14 wasm full-walk 0.91× loss; horn iii = shared decoder −22% on V8); the split-the-barrier
finding (research-D: 47% of latex-paper recoverable as `Record`/`Array`/`Select` data) is a genuine sharpening;
the self-guard against "4D dominates on parsimony ⇒ elect it" is correct. Ratified.

**CROSS-LOCK — an internal contradiction between FAMILY-SPECS Gate-0 and research-D Q8 on whether gate-0 is
LOCK-5-gated.** FAMILY-SPECS: *"the parse_that↔bbnf_ir Cargo cycle is severed by a 4-line re-export swap …
mechanical, **not LOCK-5-gated**."* research-D Q8: *"4D's gate-0 is **downstream of LOCK-5's `Dfa`
extraction**"* — because `crates/ir` deps `parse_that` for `vm/bytecode.rs`'s `parse_that::regex::Dfa`
(verified: Cargo.toml:13 + bytecode.rs:184/238), and 4D needs `parse_that → bbnf_ir`, closing a cycle the
re-export alone does not open unless `bbnf_regex` exposes `Dfa`. The two documents **disagree on a cross-lock
dependency**: is gate-0 mechanical-and-done, or LOCK-5-blocked on the `Dfa` extraction? This is unresolved of
record and material to 4D's DAG position. (My read: the regex-arm cycle is severed by the swap, but the VM's
`Dfa` dep arm remains a LOCK-5 extraction — so research-D is right and FAMILY-SPECS' "not LOCK-5-gated" is too
strong.)

**CROSS-LOCK — GAP-1's discharge rests on the same unattributed LOCK-1-TypeDesc pre-seed as 4B (see 4B gap 5).**
G-D-GAP1 ("HostClosure compiles into the IR type-plane, 7 arms, emitter fuses 1 named call-out") is credited
DONE, but the `FnDescriptor::HostClosure` type-plane diff was pre-seeded from an unowned source (PROTO-2-vmvalue:
"not mine, excluded from the patch") and touches `TypeDesc` (LOCK-1) + `crates/core/backend` + `passes` (LOCK-3).
The discharge is compile-proven but its provenance and its home (LOCK-1's solver plane) are un-adjudicated.

**Open gaps (all predecessor-owned / report-only per Q8, plus one internal contradiction):**
1. **INCOMPAT-1 (Q-ownership) → LOCK-5:** 5A shared `bbnf_ir` type vs 5B lawful functor for the *grammar* IR
   (distinct from XL-C's retired *regex* functor). DAG-blocking, un-resolvable inside LOCK-4.
2. **Gate-0's LOCK-5-gating is CONTRADICTED between FAMILY-SPECS ("not LOCK-5-gated") and research-D Q8
   ("downstream of LOCK-5's `Dfa` extraction").** The VM's `parse_that::regex::Dfa` dep is a live LOCK-5
   extraction; the internal contradiction is of record and material.
3. **HostClosure lattice → LOCK-7:** typed shape + per-subsystem floor (value.js's ~5 Pratt/easing closures;
   the tier-1 Record/Array/Select boundary). The pre-seeded type-plane diff's home (LOCK-1 TypeDesc) is also
   un-adjudicated.
4. **Node-ratio thin AND FAILS on LOCK-8's flagship consumer (value.js math 4.84%)** — fusion-inheritance is
   safe in aggregate, fragile on the exact surface LOCK-8/LOCK-7 consume (shared with 4A gap 3).
5. **Value-plane home split → GESTALT-Ω, but the ISA half is XL-B LOCK-3 tower-shape (shared with 4C gap 2)** —
   the recommended split under-routes past LOCK-3.

---

## CROSS-LOCK & CONSTITUTION AUDIT (critic-2's weighting)

- **DOUBLE-HOME #1 — the numeric leaf (LOCK-4 4B ↔ LOCK-9 9B).** 4B's election ceiling is the `number_simd`
  kernel LOCK-9 owns and is actively rebirthing. Mis-filed as an internal "next probe"; missing from the
  hand-off list. **The single most under-weighted cross-lock fact in the pass.**
- **DOUBLE-HOME #2 — the VM value plane (LOCK-4 4C ↔ LOCK-3 tower, XL-B).** `Op::ApplyFn` + `Value` scalar arms
  are an ISA + value-model change in `crates/ir/src/vm/`, LOCK-3's tower home; built under a LOCK-4 probe and
  routed past LOCK-3 straight to GESTALT-Ω. XL-B gives LOCK-3 tower-shape ownership pass-3+; the disposition
  under-routes. (Worktree-only status mitigates but does not cure.)
- **DOUBLE-HOME #3 — the `FnDescriptor::HostClosure` type plane (LOCK-4 4B/4D ↔ LOCK-1 TypeDesc / LOCK-3
  passes).** An unattributed pre-seed anchoring two gate PASSes, homed in LOCK-1's solver plane; provenance +
  ownership un-adjudicated. Constitution vii stretched (concrete-artifact provenance).
- **CROSS-LOCK CONTRADICTION — 4D gate-0 LOCK-5-gating.** FAMILY-SPECS ("not LOCK-5-gated") vs research-D Q8
  ("downstream of LOCK-5's `Dfa` extraction"), grounded in a real `crates/ir → parse_that::regex::Dfa` dep.
- **CONSTITUTION ix (named supersession) — XL-E has NO PT-tranche home.** The 4B/PROVE-09 supersession is a
  principle without a codex placement; open debt to GESTALT-P.
- **CONSTITUTION iii (isomorphism to a reasonable point) — half-tested, and LOCK-4 pre-judges LOCK-6.**
  INCOMPAT-2 (Rust monomorphization ≠ TS emission ⇒ no shared isomorphic terminal) is a genuine refutation of
  source-level rust/ts iso, correctly exported to LOCK-6/8. **But FAMILY-SPECS asserts it forces "6C or 6D,
  away from 6A/6B source-mirroring"** — declaring two of LOCK-6's four families dead is **LOCK-6's election to
  make, not LOCK-4's.** The export should carry the *evidence* (INCOMPAT-2) and let LOCK-6 rule; the "away from
  6A/6B" verdict is a soft cross-lock overreach. (The deferential "the terminal cannot freeze ahead of LOCK-6"
  line elsewhere is correct; the two are in tension.)
- **CONSTITUTION viii (consumer-less substrate, Y.13) — CLEAN but thin.** The VM value plane's only live
  consumers are the DAP/LSP debug lane + the correctness oracle (the interpret terminal is measured-refuted
  twice as a fast face). Oracles and debug are legitimate consumers, so no violation — but the newly-built
  value plane has **no production-execution consumer** by construction (codegen is the fast face). Defensible;
  worth GESTALT-Ω noting so a future pass does not read "value plane built" as "value plane on the hot path."
- **CONSTITUTION v (type plane costs zero at runtime) — untested for the new type-plane surface.** The
  pre-seeded `FnDescriptor::HostClosure{arg:TypeDesc, ret:TypeDesc}` extension has no zero-runtime-cost
  verification of record; it is asserted by the "type plane" framing, not measured.
- **EXTANT-CODEX (constitution ix) — engaged, not contradicted.** PT-2 (arena/ownership) and PT-3 (TS
  backbone) are correctly consumed; XL-D/XL-E carried as facts. No silent contradiction of the GN/PT codex
  found — the un-fulfilled item is the XL-E *placement*, not a contradiction.
- **BASE-LAW (XL-H) — CLEAN, with CRIT-1's split-tree honesty burden ratified.** Protos base-pin `ce51c36fc`;
  correction verified on `c43126422`; the PROTO-2-vmvalue build is worktree-local (`rg ApplyFn`=0 on record).
  I add: the G-B1 comparison's cross-lock stability (LOCK-9 moving the incumbent) is a base-law-adjacent risk
  the snapshot discipline does not cover.

---

## CONVERGENCE (critic-2)

| Family | CRIT-2 % | CRIT-1 % | Δ | governing reason for the delta |
|---|---|---|---|---|
| 4A | **68** | 70 | −2 | open-Q2 coherence is a LOCK-3 tower obligation (XL-B), not self-dischargeable; FAIL region is LOCK-8's consumer |
| 4B | **70** | 75 | −5 | election ceiling is LOCK-9-owned + un-exported; G-B1 baseline moves under LOCK-9; XL-E no PT home; pre-seed provenance |
| 4C | **66** | 70 | −4 | value plane built into LOCK-3's tower without ratification + mis-routed past XL-B owner; corpus crossing LOCK-5-blocked |
| 4D | **66** | 68 | −2 | gate-0 LOCK-5-gating self-contradicted (spec vs research-D); GAP-1 rests on unowned LOCK-1-TypeDesc pre-seed |

**No family converges.** The lock's architecture is settled and partly built (CRIT-1 + the spec are right on
that); what remains is **not four rival terminals but a set of cross-lock DAG dependencies the FAMILY-SPECS
under-exports** — the numeric leaf (→LOCK-9), the VM value plane's ISA home (→LOCK-3 per XL-B), the type-plane
pre-seed (→LOCK-1/LOCK-3), the corpus crossing (→LOCK-5), and the perf-supersession placement (→GESTALT-P).
LOCK-4 is a **rigorously converged core with its exact remaining gaps enumerated above**, correct for the
gestalt hand-off — provided the hand-off list is corrected to five items, not four, and the two double-homes
(numeric leaf, VM value plane) are ratified as cross-lock, not internal.

## NEXT-PASS DIRECTIVES (cross-lock weighting)

1. **Correct the hand-off list to FIVE items.** Add #5: the 4B numeric-leaf ceiling → **LOCK-9** (the
   `number_simd` kernel) + **LOCK-6/8** (the padded-input `parse()` contract). Stop filing it as an internal
   4B probe; it is exogenous.
2. **Route the VM value-plane ISA change to LOCK-3 (XL-B), not GESTALT-Ω whole.** `Op::ApplyFn` + `Value`
   scalar arms are a tower-shape change LOCK-3 owns pass-3+; GESTALT-Ω arbitrates only the internal/host split.
3. **Resolve the gate-0 LOCK-5-gating contradiction** (FAMILY-SPECS "not LOCK-5-gated" vs research-D Q8
   "downstream of `Dfa` extraction") before 4D's DAG position is trusted.
4. **Attribute and home the `FnDescriptor::HostClosure` pre-seed** (LOCK-1 TypeDesc plane); verify its
   zero-runtime-cost (constitution v). Two gate PASSes ride it; its provenance is unowned of record.
5. **Place the XL-E supersession in a PT-tranche letter** at GESTALT-P (constitution ix); it is an open debt,
   not a closed principle.
6. **Adopt CRIT-1's within-lock directives verbatim** (run G-C0's real gate vs live `.map`; land one
   HostClosure evaluation; cross canada; fire G-C4 on `rmp_serde`; build G-A1 once; produce G-B4's number).
