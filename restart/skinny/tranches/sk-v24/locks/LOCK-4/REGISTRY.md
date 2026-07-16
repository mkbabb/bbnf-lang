# LOCK-4 — THE COMBINATOR CORE · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four features — it is four
compilation strategies for one combinator algebra, decided on two axes: **Q-runtime** (what survives to
runtime: graph 4A | bytecode 4C | none 4B) × **Q-ownership** (whose representation `.then()` builds:
parse-that-private 4A/4B/4C | shared `bbnf_ir` 4D). Pass-3 re-frames these from four RIVAL terminals into
the **named parts of one architecture**: a two-front core split by a confirmed **staging boundary** — a
RUNTIME front (reify + pre-codegen passes) and a CODEGEN front (the build-time emitter, the fast face on
both languages).*

> **CURRENT STATE = PASS 3 (2026-07-16).** Inputs folded this pass: pass-3 FAMILY-SPECS (15:22),
> research-{A,B,C,D,priorart}, **PROTO-1 (baseline — Futamura codegen → recursive JSON, RAN/MEASURED/MIXED)**,
> **PROTO-1-numeric-leaf (15:38 — the 4B election lever, RAN/MEASURED/FAIL)**, **PROTO-2 (baseline — general
> emitter through GAP-1 + node-barrier census, RAN — PART 2 PASS / PART 1 QUALIFIED)**, **PROTO-2-vmvalue
> (15:54 — the 4C value plane + live dual iso-oracle, RAN/BUILT-IN-WORKTREE/8-8-GREEN)**, CRIT-1 (within-lock),
> CRIT-2 (cross-lock). Pass-2's body is preserved in PASS HISTORY. **The governing pass-3 fact: the
> FAMILY-SPECS froze at 15:22 and its own two closing briefs were then EXECUTED this pass** — BRIEF-1 (numeric
> leaf) → **FAIL**, BRIEF-2 (4C value plane) → **BUILT (worktree)**. Neither was folded into the frozen spec's
> convergence framing; both critics compute against the probes, and so does this registry. **The pass-3
> shape-change: the lock's ARCHITECTURE is settled and partly BUILT** — the codegen fast face generalizes to
> recursive JSON (PROTO-1 GATE A PASS) and fuses through an arbitrary host closure (PROTO-2 PART 2 PASS),
> **measured competitive-but-not-uniform** against the shipped incumbent (data/twitter WIN, citm TIE, canada
> +56% REGRESS) — **and the single named election lever FAILED, its residual owned by LOCK-9, not LOCK-4.**
> The terminal remains un-electable inside LOCK-4; what remains is **not four rival terminals but a set of
> cross-lock DAG dependencies the FAMILY-SPECS under-exports** (the numeric leaf → LOCK-9; the VM value-plane
> ISA → LOCK-3 per XL-B; the type-plane pre-seed → LOCK-1/LOCK-3; the corpus crossing → LOCK-5; the
> perf-supersession placement → GESTALT-P).

---

## TREE-VERIFIED THIS SESSION (the honesty burden — on the live campaign HEAD, not just the probe base-pin)

Spot-checked on **current HEAD `061c79fa7`** (descendant of base-pin `ce51c36fc`; both protos base-pin
`ce51c36fc`, correction tree-verified by research-C on `c43126422`):

- **`rg ApplyFn crates/ir/src` = 0**, **`rg HostClosure crates/core crates/ir` = 0.** The PROTO-2-vmvalue
  value plane (`Op::ApplyFn` + `Value` scalar arms) and the type-plane `FnDescriptor::HostClosure` pre-seed
  are **WORKTREE-ONLY, NOT LANDED ON RECORD.** "8/8 tests GREEN" reads as *provably buildable +
  self-consistent in a scratch worktree*, **NOT** *landed + validated-against-incumbent*. The value plane's
  status of record is still **UNBUILT.**
- **`crates/ir/src/vm/bytecode.rs:184` `compiled_regexes: Vec<Option<parse_that::regex::Dfa>>` + `:238`
  `Dfa::compile(pattern)` — CONFIRMED.** The VM depends on `parse_that::regex::Dfa`; this grounds the 4D
  gate-0 LOCK-5-gating contradiction (below): the regex-arm cycle is severable by the re-export swap, but the
  **VM's `Dfa` dep arm remains a LOCK-5 extraction** — research-D is right, FAMILY-SPECS' "not LOCK-5-gated"
  is too strong.

---

## THE PASS-3 EVENTS (what RAN, what it decided, where the spec is stale)

**PROTO-1 (baseline) — the codegen fast face GENERALIZES to recursive JSON, measured vs the shipped
incumbent (RAN / MEASURED / MIXED).** `grammar/json/json.bbnf` (recursive, value-bearing) emitted by the
*same* `crates/core/src/backend/rust/emitter/` path to `crates/core/src/grammar/generated/json.rs`.
**GATE A PASS** — hot-path census: **0 SmallBox / 0 dyn ParserFn / 0 Rc/Arc / 0 fn-ptr fields**; value
recursion → **direct byte-dispatched `fn` calls**; ZST `JsonParser`, `'a`-scoped by construction. The "JSON
has left-recursion, regex does not" objection dissolves on a running artifact. **GATE B (vs the SHIPPED,
tuned `json_parser()`, identical parse_that core both arms):** two windows, both orders, cold-paired divan —
**data 0.65–0.73× (emit WINS), twitter 0.75–0.95× (WINS), citm ~1.0–1.14× (TIE), canada 1.50–1.60× (SLOWER).**
The win concentrates on string/structure/small; the loss is **entirely the f64 numeric leaf** (canada is a
wall of floats; the incumbent's tuned number path — eisel_lemire + SIMD `dispatch_byte_multi` — outruns the
emitted `-> f64`). *Minor accuracy flag (CRIT-1): the json artifact was "already realized in-tree at
base-pin"; pass-3 MEASURED a pre-existing artifact — "built this pass" overstates.*

**PROTO-1-numeric-leaf (15:38) — BRIEF-1, the election lever (RAN / MEASURED / FAIL).** Ported the
incumbent's SWAR 8-digit mantissa fold (`parse_eight_digits`) + pure-integer fast-path into the emitted
number leaf, regen `json.rs` (exit 0). **canada UNCHANGED at 1.56×** (gate ≤1.05×), both windows both orders
(3.759ms/2.409ms = 1.560×; reversed 3.771/2.410 = 1.565×); data 0.70–0.72× PASS(WIN), twitter 0.83–0.85×
PASS(WIN), citm 1.00–1.03× PASS(tie). **The lever did NOT fire:** canada's floats have short digit runs
(1–3 int / 6–9 frac), so the `*p+8<=len` guarded probe straddles the delimiter and always falls to the
scalar tail; the integer fast-path never fires (all floats). **The residual is the 16-byte SIMD digit stripe
+ `get_unchecked` on a NUL-padded view — both gated on a padded-input `parse()` contract**, a re-architecture
that re-bases `len`/EOF across every leaf, "the next probe, not this one." **This refutes the FAMILY-SPECS'
own "single lever between mixed and elect" framing** — and (CRIT-2, the governing cross-lock fact) that
residual `number_simd` kernel is **LOCK-9's 9B NATIVE-REBIRTH plane, un-exported.**

**PROTO-2 (baseline) — GAP-1 discharged + the node-barrier census (RAN — PART 2 PASS / PART 1 QUALIFIED).**
PART 2: `FnDescriptor::HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` added to the **type/emitter** plane, 7
exhaustive `crates/ir` arms wired, `bbnf-ir` compiles clean (2.31s); the emitted `lerp` fused region is a
straight-line recognizer with the arbitrary closure fused as **1 named call-out** (`host_closure_15(inner)`),
body opaque, `arg→ret` keeping the node an analyzable graph leaf. *(core emission arms `driver/map.rs` +
`emit_host_closure` default — patch-plausible, NOT cold-built.)* PART 1: value.js math grammar = **4.839%
opaque nodes** (6 HostClosure / 124), site→node K=13.78, repo-wide **1.857% opaque nodes** (25.58% sites ×
1/K) — clears ≤2%-of-nodes repo-wide by a **7.7% cushion**, **FAILS locally** (math 4.84%). Analyzable-graph
reading is a **repo aggregate, not a per-grammar invariant.**

**PROTO-2-vmvalue (15:54) — BRIEF-2, the 4C value plane + the live dual iso-oracle (RAN / BUILT-IN-WORKTREE /
8-8-GREEN).** The four mechanical edits landed in a worktree: `Op::ApplyFn(FnId)` (Op 30→31), `IrNode::Map`
fn_id **emitted** not dropped, `#[serde(default)] fns: Vec<FnDescriptor>` on `BytecodeProgram`, `Value` 4→8
(F64/U32/Str/Variant), a ~150-LOC MapExpr evaluator + a ~200-LOC TS mirror. **8/8 tests, Oracle A
byte-identical GREEN, Oracle B crossing GREEN** (opcode-stream 0 divergences; 9 descriptors crossed
node-for-node Rust↔TS from the same wire, 0 divergences). **This discharges G-C0's *mechanism buildability*
and RAN the cross-language iso-oracle for the first time — a real uplift over the spec's "0% built."**
**BUT (both critics, ratified): the probe tested self-consistency, not the sharpest uncertainty.**

**Three corrections the pass-3 disposition owes (folded from the critics — all load-bearing):**
- **[CRIT-1, governing within-lock] The FAMILY-SPECS is STALE against its own two follow-on probes.** BRIEF-1
  FAILED (canada 1.56×), BRIEF-2 BUILT — neither folded into any convergence framing. The "single lever" and
  "0% built" framings are **both dead.** The 4C correction the spec *did* fold (value plane UNBUILT on HEAD)
  is tree-accurate and creditable.
- **[CRIT-1, the McKeeman trap] G-C0's REAL gate was NOT tested.** G-C0 requires VM output **byte-identical to
  parse-that's LIVE closure `.map`**. Oracle A asserts identity against an **author-written golden** — the
  fabricated golden the differential-testing literature (research-C §4, priorart §7.11) forbids. Oracle B
  proves only Rust↔TS crossing consistency. **Does the VM compute the SAME values the shipped combinator
  does? — never ran.** Worse, the hardest value case is a **pass-through stub** (`apply_fn.rs` forwards
  `HostClosure` — its body opaque), so byte-identity there is *un-testable by construction*. And G-C1's
  **mandatory adversarial member canada was UNRUN** (blocked on porting `parse_that::regex::Dfa` to TS).
- **[CRIT-2, the governing cross-lock] The hand-off list is INCOMPLETE — two live gaps are DOUBLE-HOMED as
  "internal next probes."** (1) The numeric-leaf ceiling is **LOCK-9's `number_simd` kernel** (9B, actively
  rebirthing `number_simd.rs:54-55`) + a LOCK-6/8 padded-input `parse()` contract — mis-filed as "4B
  open-question 1." 4B cannot elect inside LOCK-4 *even in principle*; its ceiling is exogenous, and the
  G-B1 baseline **moves under LOCK-9** the moment 9B lands its `vshrn`/`vpaddq` rewrite. (2) The VM value
  plane is an **ISA + value-model change in `crates/ir/src/vm/`** — LOCK-3's tower-shape home per **XL-B** —
  built under a LOCK-4 probe and routed straight to GESTALT-Ω, skipping the owning lock.

---

## STATUS TABLE (pass 3)

| Family | Status | Conv. | One-line |
|---|---|---:|---|
| **4A CLOSURE-TO-GRAPH** | **ADVANCED** | **68%** | Role settled — *runtime-front reifier + pre-codegen pass host, NOT a terminal* (RF-1 refutes the terminal reading; PROTO-1's zero-box codegen confirms "erase the graph into AOT" is the correct role). Node-barrier census BUILT (G-A3: 1.857% repo QUALIFIED / 4.84% math FAIL — thin, and it FAILS on value.js, LOCK-8's flagship consumer). But its ONE novel claim — `fuse(Graph)→Graph` on a **combinator-constructed** graph byte-identical to a hand-fused arena (G-A1) — has **zero running evidence for a FOURTH pass**; coherence (reify→same-emitter, open-Q2) is a **LOCK-3 tower-parity obligation (XL-B)**, not self-dischargeable. |
| **4B COMPTIME-FUSION** | **ADVANCED · terminal-role settled = the CODEGEN fast face (Rust monomorphization + TS emitter)** | **70%** | The strongest family and the lock's only running fast face on both languages: **G-B0 PASS** (recursive JSON, 0 box/dyn/fn-ptr) + **G-B3 PASS** (arbitrary host closure fused as 1 named call-out). But **the election lever FAILED** (canada 1.56× after the numeric probe) and **its residual is LOCK-9-owned + un-exported** — 4B is workload-conditional (data/twitter WIN, citm TIE, canada REGRESS), electable "for everything EXCEPT number-dominated workloads," and its election is **three-lock-away (LOCK-9 kernel + LOCK-6/8 padded-input), not one probe.** G-B4 firing budget has **no number for a THIRD pass** (the gate-that-cannot-fail); G-B3 rides an **unattributed LOCK-1-TypeDesc pre-seed**; XL-E has **no PT-tranche home** (constitution-ix debt). |
| **4C VM-CORE / PORTABLE-PROGRAM ISO** | **BANKED** (fast-terminal formulation; the interpret/oracle lane survives + rose) | **66%** | Terminal-as-fast-runtime **measured-refuted twice** (switch-VM −22%; interpret 0.021–0.46×) — stays BANKED, folded into the LOCK-3/`bbnf_ir` VM line. **The value plane + the iso-oracle got BUILT (worktree) this pass** — a genuine +34 uplift — BUT: (a) G-C0's REAL gate (VM vs live `.map`) was NOT tested (McKeeman faked-golden trap); (b) the HostClosure value case is a **pass-through stub** (evaluates nothing — the hard case un-testable by construction); (c) G-C1's canada / full-corpus crossing **UNRUN, DAG-blocked on LOCK-5's `Dfa` TS port**; (d) the build is **worktree-only** (`rg ApplyFn`=0 on record) and its ISA home is **LOCK-3's (XL-B), un-ratified.** Buildability discharged; **convergence-of-record not.** |
| **4D GRAMMAR-SUGAR / SHARED-IR** | **BLOCKED (split)** | **66%** | Terminal role refuted **three horns** (i = full TS grammar-engine port is a primitive of equal-or-greater difficulty → BLOCKED-candidate AND XL-D retires the need; ii = PROVE-14 wasm full-walk 0.91× LOSS on exactly the value-producing workload; iii = shared decoder −22× V8). **Gate-0 partly LOCK-5-gated** (the VM's `Dfa` arm — an internal spec-vs-research-D CONTRADICTION, resolved here in research-D's favour). **G-D-GAP1 (HostClosure type-plane) DISCHARGED compile-proven** (PROTO-2 PART 2) — but on the **same unattributed LOCK-1-TypeDesc pre-seed** as 4B. 4D-as-fast-shared-runtime is **dead**; 4D-as-shared-ownership is **DAG-BLOCKED on LOCK-5 (5A vs 5B) + LOCK-7 (lattice).** Do NOT carry "dominates on parsimony ⇒ elect" — conditional on 5A + a measured Rust loss. |

**Net: no family converged; the terminal cannot be named inside LOCK-4.** The pass-3 shape-change is that the
architecture is **settled AND partly built** — a two-front core split by a staging boundary, whose codegen
fast face generalizes to recursive JSON, fuses through an arbitrary closure, and is measured
competitive-but-numeric-leaf-gated. What remains un-electable is not an abstract four-way fork but **five
cross-lock DAG dependencies** (corrected hand-off list below) plus **one still-UNBUILT-of-record correctness
backbone** (the 4C value plane — worktree-built, real gate untested).

---

## 4A — CLOSURE-TO-GRAPH · ADVANCED · 68%

**Floor rationale.** CRIT-1 70 / CRIT-2 68. Take **68** (conservative floor). CRIT-2's −2 is correct and I
adopt it: open-Q2's coherence (reify→serialize→same-emitter parity) is a **LOCK-3 tower obligation (XL-B owns
the emitter + the IrNode)**, not a within-4A build; and the node-ratio FAIL region (4.84% math) lands on
**value.js — LOCK-8's flagship consumer + LOCK-7's lattice input**, not a peripheral corner. The +18 over
pass-2's 50 is warranted by the role settling (RF-1 resolved: source-front, not terminal — analytically AND
grounded on PROTO-1's zero-box codegen) and the node-barrier census being BUILT. It does not rise further
because the ONE novel 4A claim is unbuilt for a fourth pass.

**What is EARNED (do not re-litigate).** The graph is not greenfield: `bbnf_ir::IrNode` is a live
serde-serializable 14-variant near-bijection (`for_each_child`), hash-consed `GrammarDag`, and the
fusion/dispatch/LR passes already **consume** (not clone) the e-graph. Role settled: reifier + pre-codegen
pass host; its graph feeds either the interpret terminal (VM) or, reified offline, the codegen fast face (4B).
Fusion/dispatch/packrat are graph passes that run *before* the staging boundary.

**Exact open gaps.**
1. **[keystone, 0% built — FOURTH pass] G-A1 `fuse(Graph)→Graph` on a combinator-constructed graph
   byte-identical to a hand-fused arena.** Zero running evidence, four passes. "The passes already exist" is
   true for the grammar-TEXT front and **unbuilt** for the combinator front — the elegant-reduction trap, the
   family's whole risk. The single novel 4A claim.
2. **[LOCK-3 obligation, not exported] Reify→serialize→same-emitter equivalence (open-Q2) is a LOCK-3
   tower-parity build (XL-B), not a within-4A probe.** PROTO-1 proved the grammar-TEXT→emitter path
   generalizes; the runtime-graph→serialize→SAME-emitter→graph-of-record parity path never ran, and its home
   is LOCK-3's.
3. **[thin, and it FAILS on the downstream consumer] The "analyzable graph" thesis is a repo aggregate, not
   an invariant.** 1.857% repo (K=13.78, 7.7% cushion) but **4.84% math-only FAIL** — on value.js, exactly
   the surface LOCK-8 (8D consumer-contract) and LOCK-7 (lattice) are designed around. Shared with 4D gap 4.
4. **[folded, settled] Open-Q3: 4A-TS is refuted as a runtime graph-interpreter** (pass-2 PROTO-1's −22×
   switch-VM applies to a `switch(node.tag)` walk). 4A-TS must lower through the 4B-TS emitter (XL-D).

**Pass-4 directives.**
- *Prototype (the keystone, owed FOUR passes):* build `fuse(Graph)→Graph` over a **combinator-constructed**
  `bbnf_ir::IrNode` (not `.bbnf`-text IR), G-A1 = byte-identical to a hand-fused arena (structural-equality
  oracle) AND JSON/CSS cold-paired ≥0.98×, over ONE grammar. This is the claim 4A has carried but never run.
- *Research must answer:* whether open-Q2's reify→same-emitter round-trip is a LOCK-3 tower-parity build
  (XL-B) — coordinate with LOCK-3, do not attempt to self-discharge; and whether the fusion-inheritance claim
  is safe on closure-dense sub-grammars (the 4.84% math FAIL) or only in aggregate.
- *Withheld from researchers:* do NOT carry "the passes already exist ⇒ G-A1 is discharged" — it is
  discharged only for the grammar-text front; the combinator front is the unbuilt claim. Do NOT carry the
  1.857% node ratio as a per-grammar invariant — it FAILS on value.js math.

---

## 4B — COMPTIME-FUSION · ADVANCED · terminal-role settled = the CODEGEN fast face · 70%

**Floor rationale.** CRIT-1 75 / CRIT-2 70. Take **70** (conservative floor). CRIT-2's −5 is correct and I
adopt it: the election ceiling is **LOCK-9-owned + un-exported**; the G-B1 baseline **moves under LOCK-9's
active `number_simd` rebirth**; XL-E has **no PT home**; and G-B3/G-D-GAP1 ride an **unattributed
LOCK-1-TypeDesc pre-seed**. The +12 over pass-2's 58 is warranted — G-B0 (generalize to recursive JSON) and
G-B3 (GAP-1 host closure) were the two biggest pass-2 gaps and both are now PASS. It does not rise to CRIT-1's
75 because the election is not "one probe away" — it is three-lock-away, and the residual is exogenous to
LOCK-4.

**What is NEWLY EARNED — do not re-litigate.** The only family with a running fast face on both languages:
- **G-B0 PASS** — recursive JSON emitted, 0 SmallBox/dyn/fn-ptr, value recursion → direct byte-dispatched
  `fn` calls, ZST parser, `'a`-scoped. The Futamura fast face generalizes past regex. *(alloc sub-claim
  construction<6/per-parse=1 is source-census, not dhat-isolated.)*
- **G-B3 PASS** — the general emitter fuses an arbitrary host closure as 1 named call-out, `bbnf-ir`
  compile-proven. *(core emission arms patch-plausible, not cold-built.)*
- **G-B1 MEASURED, MIXED** — vs the SHIPPED `json_parser()`, identical parse_that core both arms: data
  0.65–0.73× WIN, twitter 0.75–0.95× WIN, citm ~1.0–1.14× TIE, canada 1.50–1.60× REGRESS.

The two faces remain **different mechanisms** — Rust monomorphization (no V8 analog) and TS emission (no Rust
analog), **no shared isomorphic terminal** (INCOMPAT-2, exported to LOCK-6). Terminal-role settled as *the
codegen fast face on both languages*, XL-D ratified for TS.

**Exact open gaps.**
1. **[election lever FAILED; residual is LOCK-9-owned + un-exported — the governing gap] canada 1.56×,
   unchanged after the numeric probe.** The fix (`number_simd` 16-byte SIMD stripe + `PaddedView` +
   `get_unchecked`) is **LOCK-9's 9B plane**, not a LOCK-4 probe; the padded-input `parse()` contract that
   would carry it is a **LOCK-6 isomorphism** + **LOCK-8 V8-discipline** concern. 4B is workload-conditional;
   "beyond-SOTA" is met on string/structure/small only (citm is a TIE, not a win). **This is hand-off #5,
   missing from the spec's list.**
2. **[baseline instability, base-law-adjacent] The G-B1 "identical incumbent core" is a MOVING TARGET** —
   LOCK-9 9B is actively rewriting the very `number_simd` the incumbent `json_parser()` uses; the MIXED
   verdict is pinned to a HEAD LOCK-9 is moving. XL-H holds for the *snapshot*; cross-lock stability does not.
3. **[G-B4, the gate-that-cannot-fail — no number, THIRD pass] The firing budget** (fuse-arity 8,
   `try_factor_alt` fires at 4, one box per SCC back-edge, `.rlib`/`dist` + incremental-compile) has anchors
   but no measured number. It cannot gate the monomorphization-blowup it exists to bound (constitution vi,
   generated-size). Owed on CSS **and** value.js.
4. **[provenance + double-home] G-B3 rests on an unattributed pre-seed in LOCK-1's TypeDesc plane.**
   `FnDescriptor::HostClosure{arg:TypeDesc, ret:TypeDesc}` is a type-plane construct (LOCK-1 solver
   territory) landing in `crates/core/backend` + `crates/ir/passes`; PROTO-2-vmvalue states it was
   "pre-seeded … not mine." Two gate PASSes (G-B3, G-D-GAP1) ride a diff neither probe owns, of unknown
   provenance, homed in LOCK-1. `rg HostClosure`=0 on record — not landed. Constitution v (type plane costs
   zero at runtime) is **untested** for it.
5. **[constitution-ix debt, open] XL-E supersession has NO PT-tranche home.** The principle is settled (4B's
   compile-time win ≠ PROVE-09's runtime refutation) but the *placement* is owed to GESTALT-P.

**Pass-4 directives.**
- *Prototype (either close canada or record the ceiling):* build the **padded-input `parse()` contract** (SIMD
  stripe + `get_unchecked`, coordinated with LOCK-9's `number_simd` + LOCK-6's input-contract iso) and re-run
  canada cold-paired two windows both orders — OR record canada 1.56× as 4B's **measured ceiling** and stop
  calling it "a single lever." Produce **G-B4's number** on CSS + value.js.
- *Research must answer:* is the canada residual truly exogenous (LOCK-9 kernel + padded-input re-arch), and
  what is the isomorphism cost of the padded-input contract on the TS face (LOCK-6)?
- *Withheld from researchers:* do NOT seed "4B is one probe from election" — the residual is LOCK-9-owned and
  the election is three-lock-away. Do NOT read G-B3/G-D-GAP1 as fully owned — they ride an unattributed
  pre-seed. Do NOT carry "beyond-SOTA" — met on string/structure/small only; citm is a tie, canada a loss.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · BANKED · 66%

**Floor rationale.** CRIT-1 70 / CRIT-2 66. Take **66** (conservative floor). CRIT-2's −4 is correct and I
adopt it: the value plane was **built into LOCK-3's tower (an ISA + value-model change) without LOCK-3
ratification** and the disposition **mis-routes past the XL-B owner** to GESTALT-Ω; and the corpus crossing is
**DAG-blocked on LOCK-5's `Dfa` port**, not a LOCK-4 build. The +34 over pass-2's 32 is warranted (the value
plane + iso-oracle went from 0% built to worktree-built + 8/8 GREEN) but is capped hard because the BUILD is
**buildability, not convergence-of-record** — three qualifications sit on the one thing that drove the rise
(real gate untested, hardest case stubbed, worktree-only). I do NOT go below 66: both critics enumerated
exactly these and still floored at 66; naming them again is not naming a critic error.

**What is EARNED (do not re-litigate).** (a) Terminal-runtime-on-V8 **measured-refuted twice** (switch-VM
−22%; interpret 0.021–0.46×) — evidence-BANKED as a fast face; survives as the interpret terminal (DAP/LSP/
dynamic-grammar lane — a live consumer) + the cross-language iso-of-record oracle. (b) The value-plane
completion is **buildable + self-consistent** (Op 30→31, Value 4→8, fn_id emitted, `fns` side table, 8/8
tests, Oracle A GREEN, Oracle B crossing GREEN — 9 descriptors node-for-node Rust↔TS from the same wire). (c)
The VM **is** `bbnf_ir`'s live `vm/` (a private second ISA would be an abrogate-before-patch violation) — the
LOCK-3 double-home is on route.

**Why BANKED (the crowding, unchanged).** As a *fast* TS runtime 4C is refuted-prior→refuted-datum; the
switch-decoder survives only as a portability/correctness ORACLE. Its mechanism *is* the extant VM. It does
not advance as an independent terminal-runtime family — its value-completion + iso-layer fold into the LOCK-3/
`bbnf_ir` VM line. **4C ⊕ 4D share the identical value-plane completion** (PROTO-2-vmvalue built it once for
both); 4C is the iso-oracle, 4D the ownership commitment.

**Exact open gaps.**
1. **[G-C0's REAL gate untested — the McKeeman trap] VM `ApplyFn` output vs parse-that's LIVE closure `.map`
   never differentially checked.** Oracle A substituted an author-written golden — the fabricated golden the
   differential-testing discipline (research-C §4, priorart §7.11) forbids. Semantic correctness of record is
   **open.**
2. **[the hard case is a stub] HostClosure value case is a pass-through** — the VM evaluates NO arbitrary
   closure; byte-identity there is un-testable by construction. "Value plane complete" overstates; the closed
   descriptors (Number/Hex/Enum/MapExpr) compute, the open one forwards.
3. **[DAG-blocked on LOCK-5] G-C1 canada / full-corpus crossing UNRUN** — blocked on porting
   `parse_that::regex::Dfa` (bytecode.rs:184/238, tree-confirmed) to TS; carries a latent bespoke-`Dfa`-vs-V8-
   `RegExp` divergence risk (LOCK-5/LOCK-6 territory) the instant the port lands. canada is the mandatory
   adversarial member and it never crossed.
4. **[double-home, XL-B] The value plane is an ISA + value-model change in `crates/ir/src/vm/` — LOCK-3's
   tower-shape home.** Built under a LOCK-4 probe (worktree-only, `rg ApplyFn`=0 on record) and routed
   straight to GESTALT-Ω, skipping LOCK-3. The disposition mis-routes: the ISA extension routes to **LOCK-3
   (pass-coordination) FIRST**, GESTALT-Ω only for the internal-descriptor/host-closure split.
5. **[G-C4 unexercised] Wire-contract regen-idempotence** used `serde_json`, not `rmp_serde::to_vec_named`;
   the born-RED guard against MessagePack externally-tagged-variant reorder never fired on the bus it ships on.

**Pass-4 directives.**
- *Prototype (G-C0's REAL gate):* differential oracle — VM `ApplyFn` output vs parse-that's **live closure
  `.map`** on a projection-dense grammar. An author-specified golden does NOT discharge it. Land **at least
  one HostClosure evaluation** (not a pass-through) so the hard value case is testable.
- *Prototype (cross canada):* port the TS `Dfa` (or restrict to LOCK-5's certified grammar-lowerable regex
  class) and run G-C1 over the six R3 corpora incl. **canada**; fire **G-C4 on `rmp_serde::to_vec_named`**,
  not `serde_json`.
- *Route (not GESTALT-Ω whole):* the value-plane ISA change (`Op::ApplyFn` + `Value` scalar) routes to
  **LOCK-3 (XL-B tower-shape) FIRST**; GESTALT-Ω arbitrates only the internal-descriptor (→LOCK-3) vs
  host-closure (→LOCK-7) split.
- *Withheld from researchers:* do NOT carry "value plane built + byte-identical" as convergence — it is
  worktree-buildable + self-consistent; the incumbent-parity gate is UNTESTED, the hard case is stubbed, and
  it is not on record. GESTALT-Ω should note the value plane has **no production-execution consumer** by
  construction (codegen is the fast face) — "built" ≠ "on the hot path."

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · BLOCKED (split) · 66%

**Floor rationale.** CRIT-1 68 / CRIT-2 66. Take **66** (conservative floor). CRIT-2's −2 is correct: the
gate-0 LOCK-5-gating is **self-contradicted** (spec vs research-D, and the tree confirms research-D — the VM's
`Dfa` arm is a live LOCK-5 extraction), and G-D-GAP1 rests on the **same unowned LOCK-1-TypeDesc pre-seed**.
The +36 over pass-2's 30 is warranted (characterization is near-complete, GAP-1's type-plane discharged) but
it stays BLOCKED — the terminal role is refuted and the ownership question is DAG-blocked on predecessors.

**What ADVANCED this pass (real).** GAP-1 (the value-plane wall both prior probes dodged) is **crossed on the
emitter side (compile-proven, PROTO-2 PART 2)**: `FnDescriptor::HostClosure` compiles into the IR type-plane
(7 arms), the emitter fuses an arbitrary closure as 1 named call-out. research-D's split-the-barrier finding
is a genuine sharpening: ~47% of latex-paper's non-projection sites are recoverable as analyzable
`Record`/`Array`/`Select` **data**, leaving `Opaque(HostFnId)` for true closures (value.js easing/color) only
— do not smuggle one `Box<dyn Fn>` per `->`.

**The block (confirmed, three horns).**
- **Horn (i):** a full `GrammarIR`+interpreter+optimizer port to TS = a primitive of **equal-or-greater
  difficulty than the original LOCK-4 problem** — BLOCKED-candidate by the rule's text. *But XL-D retires the
  NEED* (the fast TS face is per-language emission, not a shared runtime).
- **Horn (ii):** the wasm crossing — PROVE-14 measured full-walk value materialization at **0.91×, a LOSS**,
  on exactly the value-producing-combinator workload.
- **Horn (iii):** the shared-decoder graft is **−22% on V8**; its fast form needs the build-time emitter (the
  same per-language primitive). **4D's thesis "one VM, two sides, for free" is refuted for the fast path** —
  correct + isomorphic as data, slow to run on V8.

**Exact open gaps (all predecessor-owned / report-only per Q8, plus one internal contradiction).**
1. **[INCOMPAT-1 → LOCK-5] 5A shared `bbnf_ir` type vs 5B lawful functor for the *grammar* IR** (distinct from
   XL-C's retired *regex* functor). DAG-blocking, un-resolvable inside LOCK-4. If 5B, "inherit for free"
   evaporates; if 5A, parsimony dominates *conditional on* the block clearing.
2. **[internal contradiction, resolved here] Gate-0's LOCK-5-gating is CONTRADICTED:** FAMILY-SPECS "not
   LOCK-5-gated" (4-line re-export swap) vs research-D Q8 "downstream of LOCK-5's `Dfa` extraction." **Tree
   confirms research-D:** `crates/ir/src/vm/bytecode.rs:184/238` deps `parse_that::regex::Dfa`. **Disposition:
   the regex-arm cycle is severed by the swap; the VM's `Dfa` arm remains a LOCK-5 extraction** — so gate-0 is
   NOT wholly mechanical-and-done, and FAMILY-SPECS' "not LOCK-5-gated" is too strong.
3. **[→ LOCK-7] HostClosure lattice:** typed shape + per-subsystem floor (value.js's ~5 Pratt/easing folds;
   the tier-1 `Record`/`Array`/`Select` boundary). The pre-seeded type-plane diff's home (LOCK-1 TypeDesc) is
   also un-adjudicated (shared with 4B gap 4).
4. **[thin AND FAILS on LOCK-8's consumer] Node-ratio 1.857% repo / 4.84% math** — fusion-inheritance safe in
   aggregate, fragile on value.js, the exact surface LOCK-8/LOCK-7 consume (shared with 4A gap 3).
5. **[value-plane home split → GESTALT-Ω, but the ISA half is XL-B LOCK-3] shared with 4C gap 4** — the
   recommended split under-routes past the LOCK-3 tower-shape owner.

**Pass-4 directives.**
- *Prototype (the correctness graft, NOT the fast graft):* the 4C iso-oracle (G-C0's REAL gate + canada
  crossing) drives 4D's shared `bbnf_ir` program through a TS decoder byte-identically — this unblocks
  **correctness/parity**, not throughput (the fast TS face is the emitter). Name the distinction explicitly.
- *Research must answer:* LOCK-5's 5A-vs-5B substrate ruling and LOCK-7's host-fn lattice shape — hard DAG
  predecessors; report their shape, do not resolve inside LOCK-4. Attribute + home the HostClosure pre-seed
  (LOCK-1 TypeDesc) and verify its zero-runtime-cost (constitution v).
- *Withheld from researchers:* do NOT seed "4D dominates on parsimony ⇒ elect it" — dominance is *conditional*
  on LOCK-5 ruling 5A, and its Rust runtime is a **measured loss**. Do NOT carry gate-0 as wholly mechanical —
  the VM's `Dfa` arm is LOCK-5-gated.

---

## THE FORKS (pass-3: most rivalry collapses into composition; three genuine forks remain, none LOCK-4's to elect)

**Compositions, NOT incompatibilities (pass-3 corrects the pass-2 "four rivals" framing):**
- **4A ⊕ 4B compose across the staging boundary** (reify → fuse → serialize → emit). RF-1 makes them
  *sequential*, coherent only for the codegen front, never the live combinator API as a runtime terminal.
  PROTO-1 grounds 4B's half on a running artifact.
- **4C ⊕ 4D share the identical value-plane completion** (`Op::ApplyFn` + `fns` + `Value`-scalar, worktree-
  built once for both) and crossing mechanism; 4C is the iso-oracle, 4D the ownership commitment. Not rivals.
- **4B-TS emitter ⊕ {4A,4C,4D}-TS** — every TS terminal lowers through the build-time emitter (XL-D:
  decoder −22%, emitter ×1.23). Runtime-decoder-as-fast-TS-face is BANKED-by-measurement across all three.

**INCOMPAT-1 — Q-OWNERSHIP → LOCK-5.** parse-that-private isomorph (4A/4B/4C) vs shared `bbnf_ir` (4D). gate-0
is *partly* severed (regex arm mechanical; VM `Dfa` arm LOCK-5-gated — corrected); the *semantic* question (5A
shared type vs 5B lawful functor) is LOCK-5's. Irreducible inside LOCK-4.

**INCOMPAT-2 — THE FAST TERMINAL, cross-language → LOCK-6 (+ evidence to LOCK-8).** Rust monomorphization ≠ TS
emission — different mechanisms, no shared isomorphic terminal. Resolved in the emitter's favour on TS
(measured), but the fast Rust face is *also* codegen with a workload-dependent win. **Export the EVIDENCE
(INCOMPAT-2) and let LOCK-6 rule** — the FAMILY-SPECS' "away from 6A/6B" verdict is a **soft cross-lock
overreach** (declaring two of LOCK-6's families dead is LOCK-6's election, not LOCK-4's; CRIT-2). Route
PROTO-1's emitter/V8-discipline result to **LOCK-8** so it is not re-derived.

**INCOMPAT-3 — Q-RUNTIME (reframed + measured): RETIRED as an abstract fork.** codegen fast face (4B) vs
interpret terminal (4C-VM ≈ 4A-graph-walk, the same measured-adverse arm) is NOT incompatible — two terminals
for two use-cases, selected by the cost model. The decision-relevant comparison is the generalized codegen
route vs the SHIPPED closure core, **which PROTO-1 MEASURED (MIXED): competitive but numeric-leaf-gated.** The
concrete open number (canada parity) is exogenous — LOCK-9's, not LOCK-4's.

---

## CROWDING & REDIRECT (pass 3)

- **[carried, XL-D ratified] EVERY TS terminal converges onto the build-time emitter (4B-TS).** The
  runtime-switch decoder is refuted on V8 (−22%). Redirect stands: runtime-decoder-as-fast-TS-face is
  BANKED across 4A/4C/4D; the single live TS execution face is 4B-TS's emitter. **Underexplored formulation
  to keep OPEN in its place (the wall every winning formulation must survive):** the general emitter *through
  an arbitrary host closure* is now GAP-1-DISCHARGED on the emitter side (PROTO-2 PART 2) — the residual
  underexplored formulation is the **value-plane execution** of that closure (4C's HostClosure is a
  pass-through stub; nothing evaluates the arbitrary closure yet — G-C0's real gate).
- **[4C ⊕ 4D value plane — BANKED the redundant one] 4C-as-fast-terminal is the redundant/refuted
  formulation → BANKED; the surviving contribution is the iso-of-record oracle + the interpret lane.** The
  value-plane completion is SHARED with 4D (built once). **Open in its place:** G-C0's REAL gate (VM vs live
  `.map`, not an author golden) + one non-stub HostClosure evaluation + the canada crossing — the
  untested-of-record parts that the worktree build did NOT reach.
- **[the tower-walk graph — ONE mechanism, two homes] 4A-private-node-union ≈ `parse_with_ir`.** Run 4A's
  experiments on `bbnf_ir::IrNode` where the seam allows; the e-graph double-home is RETIRED on route
  (consumed path-dep, not a clone) — conditional on electing the shared route (LOCK-5).

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **4C ⊗ 4D — the value-plane completion + iso-of-record oracle (PRIMARY, now BUILT-in-worktree).** The
   `Op::ApplyFn` + `fns` + `Value`-scalar completion is *identical* for both; PROTO-2-vmvalue built it once
   and crossed 9 descriptors node-for-node Rust↔TS. Justified: 4C's iso mechanism ≡ 4D's crossing need. Scope:
   supplies isomorphism-of-record + parity (*correctness*), NOT the fast TS terminal (XL-D). **Remaining:**
   the REAL gate (vs live `.map`) + canada crossing.
2. **4B-TS emitter ⊗ {4A,4C,4D}-TS — the shared TS execution face (XL-D ratified).** Every TS terminal lowers
   through the build-time emitter — the only V8-survivable fast face. Whichever isomorphic terminal wins
   adopts 4B-TS as its TS lowering, as 4B-Rust is the Rust lowering.
3. **4A ⊕ 4B — reify-then-emit across the staging boundary (GROUNDED this pass).** 4A reifies the runtime
   front + runs the pre-codegen passes; 4B is the offline codegen of that reified plan. Gate: open-Q2's
   reify→serialize→same-emitter round-trip (a LOCK-3 tower-parity obligation per XL-B) — coherent for the
   codegen front, never the live combinator API as a runtime terminal.

---

## THE CORRECTED HAND-OFF LIST (FIVE items, not four — CRIT-2's governing correction)

1. **Q-ownership → LOCK-5** (5A shared type vs 5B lawful functor; INCOMPAT-1). Note: gate-0's VM `Dfa` arm is
   a LOCK-5 extraction (corrected from "not LOCK-5-gated").
2. **The fast-terminal cross-language iso → LOCK-6** (Rust monomorphization ≠ TS emission ⇒ 6C/6D;
   INCOMPAT-2). Export the EVIDENCE, let LOCK-6 rule (do not pre-declare 6A/6B dead). Route PROTO-1's
   emitter/V8-discipline result to **LOCK-8** so it is not re-derived.
3. **The value-plane ISA home → LOCK-3 (XL-B) FIRST, then GESTALT-Ω.** `Op::ApplyFn` + `Value` scalar arms are
   a tower-shape change LOCK-3 owns pass-3+; GESTALT-Ω arbitrates only the internal-descriptor (→LOCK-3) vs
   host-closure (→LOCK-7) split. (Corrected from "GESTALT-Ω whole.")
4. **4B's perf-claim supersession → PT-tranche (GESTALT-P).** XL-E settles the principle; the *placement* in a
   PT-tranche letter is an open constitution-ix debt.
5. **[NEW — the double-home the spec mis-filed as internal] The 4B numeric-leaf ceiling → LOCK-9** (the
   `number_simd` kernel, 9B, actively rebirthing) **+ LOCK-6/8** (the padded-input `parse()` contract). 4B's
   election is exogenous; it cannot elect inside LOCK-4 even in principle.

**Plus two provenance/ownership items owed to the gestalts:**
- **The `FnDescriptor::HostClosure` type-plane pre-seed is unattributed and homed in LOCK-1's TypeDesc plane.**
  Two gate PASSes (G-B3, G-D-GAP1) ride a diff neither probe owns (`rg HostClosure`=0 on record). Attribute
  it, home it (LOCK-1), and verify its zero-runtime-cost (constitution v).
- **The G-B1 baseline is cross-lock unstable** — LOCK-9 9B is moving the incumbent's `number_simd`; the MIXED
  verdict must be re-based when 9B lands (base-law-adjacent, beyond the snapshot discipline).

---

## CROSS-LOCK DAG ORDERING (for GESTALT — pass 3)

- **[FORCED] LOCK-4 ⊗ LOCK-6 are one joint decision.** INCOMPAT-2 (no shared isomorphic terminal) forces 6C
  (wasm-core) or 6D (behavioral/oracle iso). Export the evidence; the terminal cannot freeze ahead of LOCK-6.
- **[FORCED, NEW] LOCK-4 ⊗ LOCK-9 — the numeric-leaf ceiling.** 4B's election ceiling IS LOCK-9's
  `number_simd` kernel; the G-B1 baseline moves under 9B's active rebirth. A LOCK-4/LOCK-9/LOCK-6 seam (the
  padded-input `parse()` contract). **The most under-weighted cross-lock fact in the pass.**
- **[CORRECTED] LOCK-4 ⊗ LOCK-5 — Q-ownership.** gate-0's regex arm is mechanical/done; the **VM `Dfa` arm is
  a LOCK-5 extraction** (tree-confirmed bytecode.rs:184/238); shared-substrate-semantics (5A vs 5B) is LOCK-5.
- **[CORRECTED, XL-B] LOCK-4 ⊗ LOCK-3 — the VM value-plane ISA home.** `Op::ApplyFn` + `Value` scalar arms are
  a tower-shape change (XL-B gives LOCK-3 the shape pass-3+); routed past LOCK-3 straight to GESTALT-Ω by the
  spec — corrected to LOCK-3-first. (Worktree-only status mitigates: `rg ApplyFn`=0 on record.)
- **[NEW] LOCK-4 ⊗ LOCK-1 — the HostClosure type-plane pre-seed.** `arg/ret:TypeDesc` is LOCK-1 solver
  territory; the pre-seed anchoring G-B3/G-D-GAP1 is unattributed and un-adjudicated; constitution v untested.
- **[route to avoid re-derivation] LOCK-4 ⊗ LOCK-8.** PROTO-1's "fast TS terminal = build-time emitter" (hidden
  classes, IC elimination, `new Function` lexical dispatch) is LOCK-8 territory; and the node-ratio FAIL region
  (value.js 4.84%) is LOCK-8's flagship consumer.
- **[extant-tranche, owed] XL-E supersession → PT-tranche (GESTALT-P)** — principle settled, placement open.

---

## THE PASS-4 KEYSTONE SET

1. **4A's `fuse(Graph)→Graph` byte-identity oracle (G-A1)** — the one differentiator SPEC-ONLY for FOUR
   passes; on a *combinator-constructed* graph. Build it once.
2. **4C's G-C0 REAL gate** — VM `ApplyFn` output vs parse-that's LIVE closure `.map` (not an author golden) +
   one non-stub HostClosure evaluation. The McKeeman trap the worktree probe fell into.
3. **4C's G-C1 canada crossing** — port the TS `Dfa` (or restrict to LOCK-5's certified class), cross the six
   corpora incl. canada; fire G-C4 on `rmp_serde::to_vec_named`.
4. **4B's canada decision** — build the padded-input `parse()` contract (with LOCK-9's `number_simd` +
   LOCK-6's input-contract iso) and re-measure, OR record 1.56× as the measured ceiling. Give **G-B4 a
   number** on CSS + value.js.
5. **Attribute + home the `FnDescriptor::HostClosure` pre-seed** (LOCK-1 TypeDesc; verify constitution v).
6. **Route the VM value-plane ISA to LOCK-3 (XL-B); place XL-E in a PT-tranche letter (GESTALT-P).**

---

## PASS HISTORY

### Pass 3 (2026-07-16) — the architecture is SETTLED and partly BUILT; the election lever FAILED and is exogenous

**Events.** Four probes bear on this pass. **PROTO-1 (baseline)** RAN/MEASURED/MIXED — the codegen fast face
generalizes to recursive JSON (GATE A PASS: 0 box/dyn/fn-ptr) and is competitive-but-not-uniform vs the
shipped `json_parser()` (data/twitter WIN, citm TIE, canada +56% REGRESS, loss localized to the f64 leaf).
**PROTO-1-numeric-leaf** RAN BRIEF-1 → **FAIL** — the SWAR fold + integer fast-path is inert on canada (short
digit runs straddle the delimiter); canada stays 1.56×; the residual (16-byte SIMD stripe + padded
`get_unchecked`) is LOCK-9's `number_simd` kernel, un-exported. **PROTO-2 (baseline)** RAN — PART 2 PASS
(GAP-1 discharged: arbitrary host closure fused as 1 named call-out, `bbnf-ir` compile-proven) / PART 1
QUALIFIED (node ratio 1.857% repo / 4.84% math). **PROTO-2-vmvalue** RAN BRIEF-2 → **BUILT-IN-WORKTREE, 8/8
GREEN** — the 4C value plane (Op 30→31, Value 4→8, fn_id emitted, `fns` side table) + the live dual iso-oracle
(9 descriptors crossed node-for-node Rust↔TS).

**Corrections folded from the critics.** (i) CRIT-1 governing: the FAMILY-SPECS (frozen 15:22) is STALE
against its own two follow-on briefs — BRIEF-1 FAILED, BRIEF-2 BUILT; the "single lever" and "0% built"
framings are both dead. (ii) CRIT-1 McKeeman trap: G-C0's REAL gate (VM vs live `.map`) was NOT tested —
Oracle A used an author golden; HostClosure is a pass-through stub; canada crossing UNRUN. (iii) CRIT-2
governing: the hand-off list is INCOMPLETE — the numeric leaf is LOCK-9's (double-home #1), the VM value-plane
ISA is LOCK-3's (XL-B, double-home #2), the HostClosure type-plane is LOCK-1's (double-home #3, unattributed
pre-seed); the G-B1 baseline moves under LOCK-9; gate-0 is partly LOCK-5-gated (spec vs research-D
contradiction, resolved in research-D's favour — tree-confirmed bytecode.rs:184/238); "away from 6A/6B" is a
LOCK-6 overreach. **Tree-verified on live HEAD `061c79fa7`:** `rg ApplyFn`=0, `rg HostClosure`=0 (value plane
+ pre-seed WORKTREE-ONLY, UNBUILT of record); `parse_that::regex::Dfa` dep CONFIRMED.

**Critic floors folded.** CRIT-1 (within-lock): 4A 70 / 4B 75 / 4C 70 / 4D 68. CRIT-2 (cross-lock): 4A 68 /
4B 70 / 4C 66 / 4D 66. **Dispositions (conservative floor):** 4A **ADVANCED 68** (role settled = source-front
reifier; node census BUILT but thin + FAILS on value.js; G-A1 unbuilt 4th pass; open-Q2 is a LOCK-3 XL-B
obligation); 4B **ADVANCED 70 · terminal-role = the codegen fast face on both languages** (G-B0 + G-B3 PASS;
election lever FAILED + exogenous to LOCK-9; G-B4 no number 3rd pass; pre-seed provenance; XL-E no PT home);
4C **BANKED 66** (fast-terminal refuted twice; value plane + iso-oracle BUILT-in-worktree but the REAL gate
untested, hardest case stubbed, ISA home is LOCK-3's — buildability, not convergence-of-record); 4D **BLOCKED
66** (three horns; GAP-1 type-plane discharged on an unattributed pre-seed; gate-0 partly LOCK-5-gated;
ownership DAG-blocked on LOCK-5 5A/5B + LOCK-7).

**Corrected disposition.** No family converged; the terminal is un-electable inside LOCK-4. Shape-change from
pass 2: the architecture is now **settled AND partly built** — a two-front core split by a staging boundary,
codegen fast face generalized to recursive JSON + fused through an arbitrary closure, measured
competitive-but-numeric-leaf-gated. What remains is **five cross-lock DAG dependencies** (numeric leaf →
LOCK-9; value-plane ISA → LOCK-3; type-plane pre-seed → LOCK-1; corpus crossing → LOCK-5; perf-supersession →
GESTALT-P) + **one still-UNBUILT-of-record correctness backbone** (the 4C value plane, worktree-built, real
gate untested).

### Pass 2 (2026-07-16) — TWO prototypes RAN; the sharper ran NEGATIVE; the TS face is measured-decided

**Events.** PROTO-1 (TS execution face) RAN/MEASURED/**PASS** — build-time emitter ×1.23 over `.parser`-IC
baseline (0 `.parser(` emitted), runtime switch-decoder −22% (ties naive); INCOMPAT-2 decided *by
measurement* → **emitter, not decoder** (later ratified XL-D); SpanParser law survives. PROTO-2 (cycle-break +
value floor) RAN — **gate-0 PASS** (cycle mechanically severable, negative control load-bearing) and
value-plane floor **MEASURED-NEGATIVE** (0.021–0.46×; oracle byte-identical GREEN; loss attributed to
per-parse setup allocation, not the value plane).

**Two corrections folded.** (i) CRIT-1 RF-1 — the tower's `4A→4B` "Rust lowering" arrow is SPEC-ONLY and
possibly incoherent (monomorphization needs comptime types; the combinator API builds a runtime `IrNode`); the
confirmed 4B-Rust win is on a mechanism the tower's own front cannot feed. (ii) CRIT-2 §0.1 — PROTO-2's value
floor RAN NEGATIVE; value-completion unblocks *correctness*, NOT a competitive Rust runtime; "viability turns
on amortizing setup" is a NEW unmeasured hypothesis. Also: gate-0 is *mechanical/done*; the runtime-switch
refutation also refutes 4A-TS; a NEW LOCK-3/LOCK-7 value-plane double-home; route the emitter finding to
LOCK-8.

**Critic floors folded.** CRIT-1: 4A 50 / 4B 60 / 4C 48 / 4D 45. CRIT-2: 4A 50 / 4B 58 / 4C 32 / 4D 30.
**Dispositions (conservative floor):** 4A **ADVANCED 50** (role clarified; keystone 0% built three passes; RF-1
+ 4A-TS refutation new-adverse); 4B **ADVANCED 58 · terminal-role BANKED→compile-backend + TS-emitter** (both
faces mechanism-confirmed; two different mechanisms — no isomorphic terminal; general emitter/dilution/budget
open); 4C **BANKED 32** (terminal measured-refuted twice; value-completion built + byte-identical; RISEN
iso-oracle unbuilt-of-record); 4D **BLOCKED 30** (gate-0 PASS + value-completion byte-identical, but the Rust
number is a LOSS of record and the fast-TS block sharpened). No family converged; the terminal un-electable;
the TS face measured-decided (emitter) and gate-0 feasibility-proven.

### Pass 1 (2026-07-16) — agglomerated, then RE-FOLDED after PROTO-2-4B-FUSION + MONADIC-WALL-CENSUS

**Re-fold event.** The first agglomeration (4A 50 / 4B 32·REWARD-AT-RISK / 4C 30·BANKED / 4D 25·BLOCKED) was
written before the lock's deciding throughput experiment ran. PROTO-2-4B-FUSION then RAN and CONFIRMED (0
fused `blr` vs 60 closure, 1.16–1.27× cold-paired A,B,A,B N=250, two windows, both orders, recursion proven);
PROTO-1-MONADIC-WALL-CENSUS RAN GREEN (opaque-dynamic 0.525% ≤ 2% — a *structural* floor only, the value plane
untouched, F-4).

**Pass-1 re-folded floors.** CRIT-1: 4A 45 / 4B 50 / 4C 28 / 4D 25. CRIT-2: 4A 48 / 4B 50 / 4C 28 / 4D 27.
**Dispositions:** 4A **ADVANCED 45**; 4B **ADVANCED 50 · terminal-role BANKED→compile-backend** (throughput
axis closed and never the deciding one); 4C **BANKED 28** (iso-oracle grafts to 4D); 4D **BLOCKED 25**
(TS-execution primitive; value-model tax untouched). The reframe (not the number) was the finding: 4B's win is
Rust-monomorphization with no V8 analog, forcing LOCK-6 away from source-mirroring toward 6C/6D.
