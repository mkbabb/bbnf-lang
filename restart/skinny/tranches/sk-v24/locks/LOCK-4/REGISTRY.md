# LOCK-4 — THE COMBINATOR CORE · REGISTRY

*Agglomerator record (Fable-tier judgment). One row per family; convergence is the **critics'
conservative floor** unless a floor is named as erring. The lock is not four features — it is four
compilation strategies for one combinator algebra, decided on two axes: **Q-runtime** (what survives to
runtime: graph 4A | bytecode 4C | none 4B) × **Q-ownership** (whose representation `.then()` builds:
parse-that-private 4A/4B/4C | shared `bbnf_ir` 4D). Pass-3 re-frames these from four RIVAL terminals into
the **named parts of one architecture**: a two-front core split by a confirmed **staging boundary** — a
RUNTIME front (reify + pre-codegen passes) and a CODEGEN front (the build-time emitter, the fast face on
both languages).*

> **CURRENT STATE = PASS 3 · RE-FOLD (2026-07-16, 23:xx).** This supersedes the **provisional early-pass-3
> fold (16:24)**, which was written BEFORE the pass's two keystone probes ran — it listed G-A1 and the G-C0
> REAL gate as the un-built pass-4 keystone set. **Both then RAN, PASSED, and were independently REPRODUCED
> this pass**, and fresh RE-CUT critics scored against them. The 16:24 fold is preserved in PASS HISTORY as
> the provisional entry. Inputs folded HERE: FAMILY-SPECS (18:43 re-synthesis), research-{A,B,C,D,priorart},
> the full probe corpus, and — the deciding new material — **PROTO-1-4A-GA1-combinator-graph (19:40, G-A1
> PASS) + its RERUN (22:42, byte-identical replication)**, **PROTO-2-live-differential (19:43, G-C0 REAL
> gate GREEN) + PROTO-2-mckeeman-repro (22:40, cold reproduction + source-audit)**, **CRIT-1 v2 (22:57,
> within-lock) + CRIT-2 ② (22:58, cross-lock)** — both RE-CUT to see the reruns.
>
> **The governing pass-3 fact, now discharged:** the two claims the 16:24 fold named as LOCK-4's biggest
> holes — 4A's `fuse(Graph)→Graph` byte-identity (SPEC-ONLY for FOUR passes) and 4C's G-C0 REAL gate (VM
> vs parse-that's LIVE `.map`, the McKeeman-trap the vmvalue probe fell into) — **both ran, both passed,
> both were independently reproduced.** G-A1 is now a twice-measured anchor (0/10 pre- and post-fuse
> divergences, identical 37-node DAG, twitter-parity, timing in-band both runs); G-C0 is discharged +
> cold-reproduced + source-audited (12/12 census pairs vs parse-that's live in-process `.map`, HostFold
> genuinely EVALUATED not forwarded). **But — the governing correction of THIS fold — the probes were
> REPRODUCTIONS, not coverage: four probes touched exactly two gates and closed ZERO of the ~12 enumerated
> gaps.** The reruns bought robustness (single-run-fluke retired, non-stub-evaluation source-audited), not
> frontier. The sharpest uncertainties both original probes sidestepped — 4A's real parse-that API → `IrNode`
> reification ACROSS the `parse_that↔bbnf_ir` crate seam, and 4C's arbitrary OPAQUE closure + the canada
> cross-language crossing — remain untouched across all FOUR probes. **The pass-3 shape-change stands: the
> architecture is SETTLED and now partly BUILT-and-REPRODUCED-in-worktree; what remains un-electable is not
> four rival terminals but FIVE (candidate SIX) cross-lock DAG dependencies + one still-UNBUILT-of-record
> correctness backbone (the value plane — worktree-only, cross-language half unrun).**

---

## TREE-VERIFIED THIS SESSION (the honesty burden — on the live campaign HEAD, not just the probe base-pin)

Re-run independently this fold on **live HEAD `90db093bf`** (newer than the critics' `3f2350f15`, itself
newer than the 16:24 fold's `061c79fa7`; base-pin `ce51c36fc` `merge-base --is-ancestor` → **YES**):

- **`rg ApplyFn crates/ir/src` = 0**, **`rg HostClosure crates/core crates/ir` = 0**, **`FnDescriptor` = 6
  variants** (EnumWrap / BoxWrap / NumberConvert / HexConvert / SpanCapture / Expr — **no HostClosure, no
  ApplyFn**). The value plane (`Op::ApplyFn` + `Value` scalar arms + `fns` side table) and the
  `FnDescriptor::HostClosure` type-plane pre-seed are **WORKTREE-ONLY, NOT LANDED ON RECORD.** The
  mckeeman-repro's VERDICT line "the 4C/4D value plane is **correct-of-record**, not merely
  worktree-buildable" is an **equivocation on "of-record"** (CRIT-2 §0.1, adopted): the ORACLE (parse-that's
  live `.map`) is of-record and ships; the CODE is not committed. Correct label: *correct against a live
  of-record oracle, in a scratch worktree.* "PASS" reads *byte-identical-fuse / correct-under-live-
  differential in a worktree*, NOT *landed of record.*
- **`crates/ir/src/vm/bytecode.rs:184` `Vec<Option<parse_that::regex::Dfa>>` + `:238` `Dfa::compile` —
  CONFIRMED LIVE.** With **`crates/ir/Cargo.toml:13` `parse_that = "0.4"`** the `parse_that↔bbnf_ir` cycle
  is real. The VM depends on `parse_that::regex::Dfa`; the 4D gate-0 regex-arm cycle is severable by the
  re-export swap, but the **VM's `Dfa` dep arm remains a LOCK-5 extraction** — research-D right,
  FAMILY-SPECS' "not LOCK-5-gated" too strong.
- **`try_factor_alt` fires at `branches.len() >= 4`; `crates/ir/src/vm` = 1772 LOC — CONFIRMED.** The G-B4
  firing-budget anchors are real; the *number* is not (below).

**Conclusion: the specs' and probes' code facts hold on a HEAD newer than any critique used — NOT
spec-cites-itself. What is unbuilt-of-record is unbuilt; what ran, ran.**

---

## THE PASS-3 EVENTS (what RAN, what it decided, where the frontier did NOT move)

**PROTO-1-4A-GA1 (19:40) + RERUN (22:42) — 4A's keystone G-A1, built and reproduced (RAN / MEASURED / PASS ×2).**
A combinator-style smart-constructor builder (~85 LOC) emits `IrNode` for all 10 json rules; the IDENTICAL
`fuse_single_use` + `fuse_token_dispatch` pipeline runs over it and over the grammar-text reference.
**Byte-identical both pre- and post-fuse (0/10 each phase), identical 37-node interned DAG, twitter-parity
(offset 631514), timing 1.0009 (rerun) / 0.9981 (original) — both ∈ [0.98,1.02].** The single novel 4A claim,
SPEC-ONLY for four passes, is now a **twice-measured anchor.** The elegant-reduction trap does not bite THIS
test — the fuse passes key on the `IrNode` enum discriminant, so a combinator front that emits the same
shapes inherits them for free. *Governing caveat (both critics, adopted): the "combinator front" is a
**bbnf-side `IrNode` builder, NOT parse-that's real `SmallBox<dyn ParserFn>` combinator API** — the
provenance-agnostic (lower-risk) half. The RF-1-hard step — bridging parse-that's shipped surface to emit
`IrNode` across the crate seam — is untouched by both runs. CSS arm unrun; `fuse_token_dispatch` is a no-op
on json (no `@token`), so its inheritance is proven only trivially; `fuse_single_use` alone fired. The
≥0.98× timing gate is near-vacuous (byte-identical IR ⇒ identical bytecode ⇒ the ratio cannot deviate);
reproducing it re-measures a tautology.*

**PROTO-2-live-differential (19:43) + PROTO-2-mckeeman-repro (22:40) — 4C's keystone G-C0 REAL gate, discharged
and reproduced (RAN / MEASURED / GREEN ×2).** Because `parse_that = "0.4"` is a first-class dep of `crates/ir`,
parse-that's SHIPPED `.map`/`.many` closures run **in the same Rust test binary** as `parse_with_ir` — so the
oracle is parse-that's **actual live return value**, not the author-written golden Oracle A used (the
McKeeman-trap the 16:24 fold flagged). **12/12 census pairs, 0 divergences**, incl. a node-for-node tree match
under `Repeat`/`.many`. The repro adds three hardenings the original could not: (a) independent replication,
worktree byte-identical to the patch of record (0/1253 divergences); (b) a **cold run** (fresh target dir, 57
crates recompiled, 87/87, 1m19s) ruling out a fingerprint-cache artifact; (c) a **source audit** of
`exec_apply_fn` (`apply_fn.rs:94–104`) confirming the `HostFold` is genuinely EVALUATED (parses child → f64 →
`HostFold::eval` → pushes F64/U32), NOT the `=> child` pass-through arm. **G-C0's within-lock Rust-side
semantic-correctness is the lock's strongest single result — discharged, reproduced cold, source-audited.**
*Governing caveats (both critics, adopted): the corpus is **hand-selected projection kinds**, not a real
grammar compiled through the front-end (defensible — a hand-mirror would smuggle an author transcription back
into the oracle — but a milder selection residue); `HostFold` is a **CLOSED 2-fold numeric set** (Lerp,
ChannelScale), evaluable + serializable + NON-opaque — NOT the arbitrary `Opaque(HostFnId)`/`Value::Host`
closure value.js's 3 closure-RETURNING easing parses demand (the hard case, unevaluated); **G-C1 canada
cross-language crossing UNRUN** (DAG-blocked on porting `parse_that::regex::Dfa` to TS); G-C4 fired only on
`serde_json`, not `rmp_serde::to_vec_named` (the actual bus).*

**The pass's two probes that did NOT run again (carried from the 16:24 fold, unchanged): PROTO-1-numeric-leaf
(BRIEF-1 → FAIL, canada 1.56×, residual is LOCK-9's `number_simd`) and PROTO-2 PART 1 (node census 1.857% repo
/ 4.84% math FAIL). No probe touched 4B or 4D's tier-1 barrier-dissolution this sub-pass.**

**The two governing corrections of THIS fold (folded from the RE-CUT critics — both load-bearing):**
- **[CRIT-1 + CRIT-2, governing] REPRODUCTION-IN-PLACE-OF-COVERAGE.** Four probes this pass touched exactly
  two gates (4A-G-A1, 4C-G-C0); the marginal two were REPRODUCTIONS of the first two. The convergence
  denominator is the ENUMERATED-GAP count, and **not one of the ~12 open gaps was touched** — the real-API
  crate seam (4A), the CSS/`@token` arm (4A), the LOCK-3 tower-parity obligation (4A), the arbitrary opaque
  closure (4C/4D), the canada cross-language crossing (4C's raison d'être), G-B4's number (4B), the TS `Dfa`
  port, the value.js analyzability FAIL. **The frontier is exactly where it was.** A pass can be "clean" (both
  reproductions PASS) and move the % ~nothing. This is a convergence risk in its own right (CRIT-2 §6.9): the
  probe budget confirms yesterday's PASS instead of attacking the frontier.
- **[CRIT-1 + CRIT-2, governing] The two-shapes HostClosure mess is now ASYMMETRICALLY HARDENED.** Three
  discharge claims — G-B3 (4B), G-D-GAP1 (4D), "non-stub HostClosure evaluated" (4C) — ride a
  `FnDescriptor::HostClosure` diff that is `rg HostClosure`=0 of record and is **two incompatible shapes
  wearing one name**: the emitter-plane `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}` (opaque call-out,
  PROTO-2 PART 2, un-reconciled) and the VM-plane `HostClosure{kind:HostFold, coeffs:Vec<f64>}` (closed
  numeric fold, live-differential/repro). The repro diffed against the VM-plane patch — **hardening one shape
  twice while leaving the emitter-plane shape untouched**, and NEITHER is research-B/D's `HostClosure{fn_id,
  arg_types, ret_type}` → host `fn_table` + `Value::Host` (the arbitrary opaque residue, built by no probe).
  The lock's three gates cannot all be reading the same artifact.

---

## STATUS TABLE (pass 3 · RE-FOLD)

| Family | Status | Conv. | Δ vs 16:24 | One-line |
|---|---|---:|:---:|---|
| **4A CLOSURE-TO-GRAPH** | **ADVANCED** | **73%** | +5 | Keystone G-A1 built + reproduced — the SPEC-ONLY-for-four-passes claim is now a twice-measured anchor (0/10 fuse divergences, identical 37-node DAG). Role settled = source-front reifier + pre-codegen pass host (RF-1). **But** the combinator front is a bbnf-side builder, NOT parse-that's real API across the crate seam (the RF-1-hard step, unbuilt twice); CSS/`@token` unrun; open-Q2 reify→same-emitter is a LOCK-3 XL-B obligation; node-ratio FAILS on value.js (4.84%). The reruns bought robustness, not the sharpest uncertainty. |
| **4B COMPTIME-FUSION** | **ADVANCED · terminal-role = the CODEGEN fast face (Rust monomorphization + TS emitter)** | **66%** | −4 (cross-lock RE-CUT, not regression) | The strongest MECHANISM and the lock's only running fast face on both languages: **G-B0 PASS** + **G-B3 PASS**. Number DROPS not because 4B got worse — **no probe touched it** — but because the RE-CUT critics sharpen cross-lock: election is EXOGENOUS (LOCK-9 9B + LOCK-6/8, three-lock-away), canada 1.56× is the measured ceiling, **G-B4 the gate-that-cannot-fail is on its FIFTH pass with NO number**, the G-B1 baseline moves under LOCK-9's active rebirth, and G-B3 rides the un-reconciled emitter-plane HostClosure shape. |
| **4C VM-CORE / PORTABLE-PROGRAM ISO** | **BANKED** (fast-terminal refuted twice; the iso-oracle / interpret lane survives) | **63%** | −3 | G-C0 REAL gate **discharged + cold-reproduced + source-audited** — the lock's strongest single within-lock result (CRIT-1 80). Floor is the cross-lock 63: **"correct-of-record" is FALSE on tree** (`rg ApplyFn`=0, worktree-only); the **canada cross-language crossing — the iso-oracle's whole raison d'être — is UNRUN** (DAG-blocked on the TS `Dfa` port); the hard case is a **closed-fold SUBSTITUTE** for the arbitrary opaque closure, now twice-anchored; the ISA home is LOCK-3's (XL-B), un-ratified. Buildability + Rust-side correctness discharged; **convergence-of-record + the cross-language half not.** |
| **4D GRAMMAR-SUGAR / SHARED-IR** | **BLOCKED (split)** | **58%** | −8 | Terminal role refuted **three horns** (i full-TS-port primitive-of-equal-difficulty + XL-D retires the need; ii PROVE-14 wasm 0.91× LOSS; iii shared decoder −22× V8). Its central quantitative claim — research-D's "**~52% dissolves as analyzable data**" — is a **SOURCE COUNT, unbuilt** (the three `Record`/`Array`/`Select` nodes never landed or round-trip-tested); the closure-RETURNING residue is neither built nor evaluated (only the numeric slice of tier-2 has evidence). Ownership DAG-blocked on LOCK-5 (grammar-IR sharing) + LOCK-7 (lattice); INCOMPAT-1's "5A vs 5B" framing **contradicts XL-C's retired-5B ledger** — re-label owed. |

**Net: no family converged; the terminal cannot be named inside LOCK-4.** The pass-3 shape-change stands and
sharpened: the architecture is **settled AND partly built-and-reproduced-in-worktree**, but the reproduction
round closed ZERO enumerated gaps. What remains un-electable is **five (candidate six) cross-lock DAG
dependencies** + **one still-UNBUILT-of-record correctness backbone** (the value plane — worktree-only, the
cross-language half unrun) + **two provenance debts** (the asymmetrically-hardened two-shapes HostClosure
pre-seed → LOCK-1/LOCK-7; the cross-lock-unstable G-B1 baseline → LOCK-9).

---

## 4A — CLOSURE-TO-GRAPH · ADVANCED · 73%

**Floor rationale.** CRIT-1 v2 78 / CRIT-2 ② 73. Take **73** (conservative floor). CRIT-2's −5 vs CRIT-1 is
correct and I adopt it: the reruns closed zero gaps, and the sharpest 4A uncertainty (real parse-that API →
`IrNode` across the `parse_that↔bbnf_ir` seam — the RF-1 category-error step, the cycle the VM's `Dfa` dep
makes live) is unbuilt across all four probes; the +1 CRIT-2 gave over its own 20:07 (72) is the
twice-measured-anchor confirmation only. The **+5 over the 16:24 provisional 68** is warranted and load-bearing:
G-A1 went from "0% built, SPEC-ONLY four passes" to **built + PASS + byte-identically reproduced** — the
family's one novel claim is finally on evidence. It does not rise past 73 because (a) the combinator front is a
bbnf-side builder not the real API, (b) CSS/`@token` never ran so `fuse_token_dispatch` inheritance is trivial,
(c) the ≥0.98× timing arm is near-vacuous and reproducing it re-measures a tautology, (d) the node ratio FAILS
on value.js. **This is the largest earned rise in the lock this fold, and the reason 4A now leads.**

**What is NEWLY EARNED — do not re-litigate.** G-A1 is discharged: the extant fuse passes, run over a
combinator-constructed `IrNode`, produce a graph byte-identical to the grammar-text reference (0/10 pre- AND
post-fuse), an identical 37-node interned DAG, twitter-parity, timing in-band on two independent runs. The
elegant-reduction trap does not bite THIS test — the fuse passes are pure structural rewrites keyed on the
`IrNode` enum, provenance-agnostic. `bbnf_ir::IrNode` remains a live serde-serializable 14-variant
near-bijection with a hash-consed `GrammarDag`; role settled = reifier + pre-codegen pass host; its graph feeds
the interpret terminal (VM) or, reified offline, the codegen fast face (4B). Fusion/dispatch/packrat are graph
passes that run *before* the staging boundary.

**Exact open gaps (the frontier the reruns did NOT touch).**
1. **[THE sharpest 4A uncertainty, unbuilt across four probes] Real parse-that API → `IrNode` across the crate
   seam.** All four probes used an 85-LOC bbnf-side `IrNode` builder. The RF-1-hard step — bridging
   parse-that's shipped `SmallBox<dyn ParserFn>` surface (`parse.rs:61,74`) to emit `IrNode` across the
   `parse_that↔bbnf_ir` cycle (`Cargo.toml:13`, tree-confirmed live) — is where the family's real risk lives,
   unmoved. Reproducing the provenance-agnostic result a second time adds robustness, zero movement here.
2. **[proven only trivially] CSS arm of G-A1 unrun; `fuse_token_dispatch` is a no-op on json.** json has no
   `@token` rules; the pass early-returns on both fronts. `fuse_single_use` alone is load-bearing; the
   `Map{fn_id}`/`@pretty`/token-dispatch inheritance is unexercised. BRIEF-A asked "JSON/CSS ≥0.98×" — CSS
   never ran, in either run.
3. **[LOCK-3 obligation, XL-B, zero evidence] Reify→serialize→SAME-emitter→graph-of-record parity (open-Q2).**
   A LOCK-3 tower-parity build, not self-dischargeable. PROTO-1 proved the grammar-TEXT→emitter path; the
   runtime-graph→SAME-emitter path — the one 4A's source-front role actually rides — never ran.
4. **[thin AND FAILS on the downstream consumer] Node-ratio 1.857% repo / 4.84% math FAIL** — a repo aggregate,
   not a per-grammar invariant; FAILS on value.js, LOCK-8's flagship (8D) + LOCK-7's lattice input. Shared with
   4D gap 4. The reproduction round did not re-run the census.
5. **[folded, settled] Open-Q3: 4A-TS is refuted as a runtime graph-interpreter** (switch-VM −22×); 4A-TS
   lowers through the 4B-TS emitter (XL-D).

**Pass-4 directives.**
- *Prototype (the frontier, NOT a third reproduction):* wire parse-that's **REAL combinator API** (not a
  bbnf-side builder) to emit `IrNode` across the crate seam and re-run byte-identity — the RF-1-hard step. AND
  run the **CSS arm** + a grammar carrying `@token`/`Map{fn_id}` so `fuse_token_dispatch` fires substantively.
- *Research must answer:* whether open-Q2's reify→same-emitter round-trip is a LOCK-3 tower-parity build (XL-B)
  — coordinate with LOCK-3, do not self-discharge; and whether the fusion-inheritance claim is safe on
  closure-dense sub-grammars (the 4.84% math FAIL) or only in aggregate.
- *Withheld from researchers:* do NOT carry "G-A1 is done ⇒ 4A's keystone is discharged" — it is discharged for
  the **grammar-text-shaped bbnf-side builder**; the real parse-that API across the crate seam is the unbuilt
  claim. Do NOT read the twice-measured timing as a second independent datum (byte-identical IR ⇒ identical
  bytecode — a tautology measured twice). Do NOT carry the 1.857% node ratio as a per-grammar invariant.

---

## 4B — COMPTIME-FUSION · ADVANCED · terminal-role settled = the CODEGEN fast face · 66%

**Floor rationale.** CRIT-1 v2 72 / CRIT-2 ② 66. Take **66** (conservative floor). **The −4 vs the 16:24
provisional 70 is a cross-lock RE-CUT, NOT a regression — no probe touched 4B this sub-pass, its mechanism is
unchanged and strongest.** The drop is the RE-CUT CRIT-2 sharpening the cross-lock reading of the *same*
evidence: (a) the election ceiling is **LOCK-9-owned + un-exported** and the election is **three-lock-away**;
(b) the G-B1 baseline **moves under LOCK-9's active `number_simd` rebirth** (base-law-adjacent instability);
(c) **G-B4 — the canonical gate-that-cannot-fail — is on its FIFTH pass with no number**, and a full
reproduction round elapsed with the probe budget spent elsewhere, making it *more* overdue; (d) G-B3 rides the
un-reconciled emitter-plane HostClosure shape (`rg HostClosure`=0). I do NOT hold at CRIT-1's 72: a firing
budget that has gone unset for a fifth pass while probe budget went to reproductions is a genuine (if small)
negative convergence signal, and the election-blocking dependencies are exogenous. I name for GESTALT that
**4B remains the strongest MECHANISM in the lock** — its %-cap is external-election-boundedness + an aging
gate, not mechanism weakness; CRIT-1's 72 reflects that within-lock strength, CRIT-2's 66 the cross-lock cap.

**What is EARNED — do not re-litigate.** The only family with a running fast face on both languages:
- **G-B0 PASS** — recursive JSON emitted, 0 SmallBox/dyn/fn-ptr, value recursion → direct byte-dispatched `fn`
  calls, ZST parser, `'a`-scoped. The Futamura fast face generalizes past regex.
- **G-B3 PASS** — the general emitter fuses an arbitrary host closure as 1 named call-out, `bbnf-ir`
  compile-proven. *(core emission arms `driver/map.rs` + `emit_host_closure` default — patch-plausible, NOT
  cold-built; ride the emitter-plane HostClosure shape — see two-shapes mess.)*
- **G-B1 MEASURED, MIXED** — vs the SHIPPED `json_parser()`, identical parse_that core both arms: data
  0.65–0.73× WIN, twitter 0.75–0.95× WIN, citm ~1.0–1.14× TIE, **canada 1.50–1.60× REGRESS**. The election
  lever FAILED (PROTO-1-numeric-leaf: SWAR 8-digit fold inert on canada's short digit runs). The two faces
  remain different mechanisms (Rust monomorphization ≠ TS emission; INCOMPAT-2 → LOCK-6). "Beyond-SOTA" is met
  on **string/structure/small ONLY** — citm is a TIE, not a win.

**Exact open gaps.**
1. **[election EXOGENOUS — the governing gap] canada 1.56×, unchanged after the numeric probe.** The fix
   (`number_simd` 16-byte SIMD stripe + `PaddedView` + `get_unchecked`) is **LOCK-9's 9B plane**; the
   padded-input `parse()` contract is a **LOCK-6 isomorphism + LOCK-8 V8-discipline** concern. 4B cannot elect
   inside LOCK-4 even in principle. Hand-off #5.
2. **[SPEC_ONLY presented as the ceiling] Canada-via-padding is unbuilt** — the SIMD-stripe re-architecture is
   a hypothesis presented as the known ceiling; no artifact (constitution-vii).
3. **[the gate-that-cannot-fail — NO number, FIFTH pass] G-B4 firing budget** (fuse-arity 8, `try_factor_alt`
   at `branches.len() >= 4` [tree-confirmed], one box per SCC back-edge, `.rlib`/`dist` + incremental-compile)
   has anchors but **no measured curve**. It cannot gate the monomorphization-blowup it exists to bound
   (constitution vi / generated-size-budget). Owed on CSS **and** value.js since pass 1.
4. **[provenance + double-home + inconsistency] G-B3 rests on the un-reconciled emitter-plane HostClosure
   shape** — `HostClosure{fn_id, arg:TypeDesc, ret:TypeDesc}`, `rg HostClosure`=0 on record, homed in LOCK-1's
   TypeDesc plane, INCONSISTENT with the VM-plane `HostFold` shape the mckeeman probe hardened, constitution-v
   untested. See the two-shapes mess.
5. **[constitution-ix debt, open] XL-E supersession has NO PT-tranche home** (→ GESTALT-P) — owed since pass 1.

**Pass-4 directives.**
- *Prototype (close canada or record the ceiling — and GIVE G-B4 ITS NUMBER):* build the **padded-input
  `parse()` contract** (SIMD stripe + `get_unchecked`, coordinated with LOCK-9's `number_simd` + LOCK-6's
  input-contract iso) and re-run canada cold-paired two windows both orders — OR record canada 1.56× as 4B's
  **measured exogenous ceiling** and stop calling it "a single lever." Produce **G-B4's firing-budget curve**
  on CSS + value.js, or record it un-gateable.
- *Research must answer:* is the canada residual truly exogenous (LOCK-9 kernel + padded-input re-arch), and
  what is the isomorphism cost of the padded-input contract on the TS face (LOCK-6)?
- *Withheld from researchers:* do NOT seed "4B is one probe from election" — the residual is LOCK-9-owned and
  the election is three-lock-away. Do NOT read G-B3 as fully owned — it rides an un-reconciled, unattributed
  pre-seed. Do NOT carry "beyond-SOTA" — met on string/structure/small only; citm a tie, canada a loss.

---

## 4C — VM-CORE / PORTABLE-PROGRAM ISOMORPHISM · BANKED · 63%

**Floor rationale.** CRIT-1 v2 80 / CRIT-2 ② 63. The spread is wide (17 pts) because the two critics weight
opposite halves: CRIT-1 (within-lock) credits the McKeeman discharge as the lock's strongest single result;
CRIT-2 (cross-lock) discounts hard because the family's cross-lock RAISON D'ÊTRE — the cross-language
crossing — never ran. Take **63** (conservative floor) and I name why CRIT-2 does NOT err: (a) **"correct-of-
record" is FALSE on tree** — I re-confirmed `rg ApplyFn`=0 on HEAD `90db093bf`; the value plane is
worktree-only, the verdict conflates an of-record ORACLE with of-record CODE; (b) **G-C1 canada cross-language
crossing is UNRUN** — and for a family whose SURVIVING contribution (BANKED) is the *cross-language*
iso-of-record oracle, the cross-language half being DAG-blocked is the load-bearing gap, not a peripheral one;
(c) the hard case is a **closed-fold substitute** now twice-anchored (masked-fallback amplified). The −3 vs the
16:24 provisional 66 is the RE-CUT CRIT-2's sharpening (66 early → 62 at 20:07 on seeing the actual probe's
limits → +1 to 63 for the reproduction) — an evidence-grounded downward correction, then a small
reproduction-uptick. I do NOT go to CRIT-1's 80: it over-weights the (real) within-lock discharge and
under-weights that the crossing advanced ZERO.

**What is EARNED — do not re-litigate.** (a) Terminal-runtime-on-V8 **measured-refuted twice** (switch-VM
−22%; interpret 0.021–0.46×) — BANKED as a fast face; survives as the interpret terminal (DAP/LSP/dynamic-
grammar lane — a live consumer) + the cross-language iso-of-record oracle. (b) **G-C0's within-lock Rust-side
semantic correctness is discharged, cold-reproduced, and source-audited** — VM `Op::ApplyFn` output byte-
identical to parse-that's LIVE in-process `.map` across 12 census pairs, the `HostFold` genuinely EVALUATED
(`apply_fn.rs:94–104`, not the `=>child` arm). This is the lock's strongest single within-lock datum and it
retired the faked-golden + pass-through-stub + single-run sub-doubts. (c) The VM **is** `bbnf_ir`'s live `vm/`
(1772 LOC, tree-confirmed) — a private second ISA would be an abrogate-before-patch violation; the LOCK-3
double-home is on route.

**Why BANKED (unchanged).** As a *fast* TS runtime 4C is refuted-prior→refuted-datum; the switch-decoder
survives only as a portability/correctness ORACLE. Its mechanism *is* the extant VM. **4C ⊕ 4D share the
identical value-plane completion** (built once for both); 4C is the iso-oracle, 4D the ownership commitment.

**Exact open gaps (the frontier the reproduction round did NOT touch).**
1. **[the raison d'être, UNRUN — the governing gap] G-C1 canada / full-corpus cross-language crossing.** The
   differential runs **in-process in Rust**, NOT a TS re-decode. The 6D behavioral-iso evidence LOCK-6 needs —
   decode-in-TS, `Value`-tree identity over six R3 corpora incl. **canada** (the mandatory adversarial member
   where the f64 leaf diverges) — is DAG-blocked on porting `parse_that::regex::Dfa` to TS (`bytecode.rs:
   184/238`, tree-confirmed). **LOCK-4's headline cross-lock deliverable is half-built after a full
   reproduction round.**
2. **[the hard case is a SUBSTITUTE, twice-anchored] The arbitrary opaque closure is UNEVALUATED.** `HostFold`
   is a closed 2-fold set (Lerp, ChannelScale) — MapExpr-adjacent, evaluable, NON-opaque. value.js's 3
   closure-RETURNING easing parses (`easing.ts:124/142/153`, `EasingFunction = (progress)=>number`, the true
   `Opaque(HostFnId)`/`Value::Host` residue) are still not evaluated. Reproducing the fold twice doubles
   confidence in the substitute (masked-fallback amplified — CRIT-2 §0.2).
3. **[worktree-only + "correct-of-record" equivocation] Value plane NOT on record** (`rg ApplyFn`=0 on
   `90db093bf`); ISA home is LOCK-3's (XL-B), un-ratified; the internal-descriptor (→LOCK-3) vs host-closure
   (→LOCK-7) split correctly exported. The mckeeman verdict's "correct-of-record" must be re-labeled
   *correct-against-a-live-oracle-in-worktree*.
4. **[corpus selection residue] Corpus is author-selected projection kinds** — no real projection-dense grammar
   (`css/pretty.bbnf`) compiled through the front-end and crossed full-value-tree (reproduced against the same
   12 kinds, not widened).
5. **[G-C4 unfired on its shipping bus] Wire-contract regen-idempotence** used `serde_json`, not
   `rmp_serde::to_vec_named`; the born-RED guard against MessagePack externally-tagged-variant reorder never
   fired on the bus it ships on.

**Pass-4 directives.**
- *Prototype (the frontier):* **cross canada cross-language** — port the TS `Dfa` (or restrict to LOCK-5's
  certified grammar-lowerable regex class) and run G-C1 over the six R3 corpora incl. canada; fire **G-C4 on
  `rmp_serde::to_vec_named`**. **Evaluate at least one closure-RETURNING `Opaque(HostFnId)`** (value.js easing),
  not a numeric HostFold. Compile a REAL grammar (`css/pretty.bbnf`) through the front-end and cross its full
  value tree, replacing the 12 hand-selected kinds.
- *Route (NOT GESTALT-Ω whole):* the value-plane ISA change (`Op::ApplyFn` + `Value` scalar) routes to
  **LOCK-3 (XL-B tower-shape) FIRST**; GESTALT-Ω arbitrates only the internal-descriptor (→LOCK-3) vs
  host-closure (→LOCK-7) split.
- *Withheld from researchers:* do NOT carry "G-C0 discharged ⇒ value plane correct-of-record" — it is
  correct-against-a-live-oracle-in-worktree; the CODE is `rg ApplyFn`=0. Do NOT read the reproduced HostFold as
  the arbitrary-closure barrier crossed — it is a closed-fold substitute, twice-anchored. Do NOT read the
  in-process Rust differential as the cross-language iso — that half (the raison d'être) is DAG-blocked and
  advanced zero.

---

## 4D — GRAMMAR-SUGAR / SHARED-IR OWNERSHIP · BLOCKED (split) · 58%

**Floor rationale.** CRIT-1 v2 70 / CRIT-2 ② 58. Take **58** (conservative floor). CRIT-2's −12 vs CRIT-1 is
correct and I adopt it: 4D's central quantitative claim is **unbuilt**, its residue tier is **unbuilt +
unevaluated**, its ownership is **DAG-blocked on predecessors**, and its INCOMPAT-1 framing **contradicts a
settled ledger (XL-C)**. The −8 vs the 16:24 provisional 66 is the RE-CUT CRIT-2's sharpening (66 early → 58 at
20:07 on seeing the two-shapes mess + the source-count nature of the tier-1 claim + the XL-C contradiction) —
evidence-grounded, not oscillation. It stays BLOCKED: the terminal role is refuted three horns and the
ownership question is DAG-blocked on LOCK-5 + LOCK-7.

**What is EARNED this pass (real, but narrower than it reads).** research-D's split-the-barrier finding is a
genuine sharpening: ~52% (44/84) of latex-paper's non-projection sites are recoverable as analyzable
`Record`/`Array`/`Select` **data**, leaving `Opaque(HostFnId)` for true closures (value.js easing/color) only —
do NOT smuggle one `Box<dyn Fn>` per `->`. GAP-1 is crossed on the emitter side (compile-proven, PROTO-2 PART
2). **BUT (the governing 4D correction this fold): the "~52% dissolves as data" is a SOURCE COUNT, not a built
artifact** — research-D's own prototype obligation 2 (land `MapExpr::Array` + `MapExpr::Select` +
`FnDescriptor::Record`, re-lower the 44 sites, assert `constant_fold`-total + serializable) was **run by no
probe**; the mckeeman probes built `HostFold`, not the three tier-1 data nodes.

**The block (confirmed, three horns).**
- **Horn (i):** a full `GrammarIR`+interpreter+optimizer port to TS = a primitive of **equal-or-greater
  difficulty** — BLOCKED-candidate; XL-D retires the NEED (the fast TS face is per-language emission).
- **Horn (ii):** PROVE-14 measured wasm full-walk value materialization at **0.91×, a LOSS**, on exactly the
  value-producing-combinator workload.
- **Horn (iii):** the shared-decoder graft is **−22% on V8**. "One VM, two sides, for free" is refuted for the
  fast path — correct + isomorphic as data, slow to run on V8.

**Exact open gaps.**
1. **[INCOMPAT-1 → LOCK-5, BUT the framing contradicts XL-C] Grammar-IR sharing.** The genuine open question
   (does parse-that's combinator graph share `bbnf_ir`?) is real and DAG-blocking. But open-Q1 frames it as
   "5A shared type vs **5B lawful functor**" — and **XL-C RETIRED 5B** (conditional re-open only if LOCK-1
   abandons Map-barrier typing); "5A/5B" are LOCK-5's **regex-HIR** family names, a category slippage onto the
   **grammar-IR** question. Re-label as a named supersession (constitution-ix) or route to the true home.
   Gate-0's VM `Dfa` arm IS a LOCK-5 extraction (tree-confirmed).
2. **[SOURCE COUNT, unbuilt — the governing gap] Tier-1 barrier-dissolution.** The three `Record`/`Array`/
   `Select` data nodes were never landed or round-trip-tested (research-D obligation 2 unrun); "~52% dissolves
   as data" is SPEC_ONLY.
3. **[→ LOCK-7, unbuilt + unevaluated] The closure-RETURNING residue.** `Opaque(HostFnId)` + `Value::Host` —
   the whole reason the split thesis needs a residue tier — is neither built nor evaluated. The numeric
   `HostFold` is a substitute for the hard case (shared with 4C gap 2). LOCK-7 owns the lattice shape + the
   per-subsystem floor (value.js's ~5 Pratt/easing folds; the tier-1 `Record`/`Array`/`Select` boundary).
4. **[thin AND FAILS on LOCK-8's consumer] Node-ratio 1.857% repo / 4.84% math** — fusion-inheritance safe in
   aggregate, fragile on value.js (shared with 4A gap 4).
5. **[value-plane ISA home → LOCK-3 XL-B FIRST] shared with 4C gap 3** — the recommended split under-routes past
   the LOCK-3 tower-shape owner; the `FnDescriptor::HostClosure` pre-seed is unattributed, homed in LOCK-1,
   INCONSISTENT across two probes, asymmetrically hardened, constitution-v untested.

**Pass-4 directives.**
- *Prototype (BUILD the source-count claim + the correctness graft, NOT the fast graft):* **land the tier-1
  nodes** — `MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record` — re-lower the 44 latex-paper
  object-literal sites, assert `constant_fold`-total + serializable. Convert "~52% dissolves as data" from a
  source count into a compiled artifact. Separately, drive 4D's shared `bbnf_ir` program through a TS decoder
  byte-identically via the 4C iso-oracle — this unblocks **correctness/parity**, not throughput (the fast TS
  face is the emitter, XL-D). Name the distinction explicitly.
- *Research must answer:* LOCK-5's grammar-IR-sharing ruling and LOCK-7's host-fn lattice shape — hard DAG
  predecessors; report their shape, do not resolve inside LOCK-4. Re-label INCOMPAT-1 off the retired 5A/5B
  regex-HIR axis. Attribute + home the HostClosure pre-seed (LOCK-1 TypeDesc), pick ONE shape of record, verify
  constitution-v.
- *Withheld from researchers:* do NOT seed "4D dominates on parsimony ⇒ elect" — dominance is conditional on
  the LOCK-5 ruling and its Rust runtime is a **measured loss**. Do NOT carry "~52% dissolves as data" as a
  built fact — it is a source count. Do NOT carry gate-0 as wholly mechanical — the VM's `Dfa` arm is
  LOCK-5-gated. Do NOT invoke "5A vs 5B" — 5B is RETIRED (XL-C).

---

## THE FORKS (pass-3: most rivalry collapses into composition; three genuine forks remain, none LOCK-4's to elect)

**Compositions, NOT incompatibilities:**
- **4A ⊕ 4B compose across the staging boundary** (reify → fuse → serialize → emit). RF-1 makes them
  *sequential*, coherent only for the codegen front, never the live combinator API as a runtime terminal.
  **G-A1 now GROUNDS 4A's half** on a running combinator-constructed graph (byte-identical fuse); PROTO-1
  grounds 4B's half.
- **4C ⊕ 4D share the identical value-plane completion** (`Op::ApplyFn` + `fns` + `Value`-scalar, worktree-
  built once for both) and crossing mechanism; 4C is the iso-oracle, 4D the ownership commitment. Not rivals.
- **4B-TS emitter ⊕ {4A,4C,4D}-TS** — every TS terminal lowers through the build-time emitter (XL-D: decoder
  −22%, emitter ×1.23). Runtime-decoder-as-fast-TS-face is BANKED-by-measurement across all three.

**INCOMPAT-1 — Q-OWNERSHIP → LOCK-5.** parse-that-private isomorph (4A/4B/4C) vs shared `bbnf_ir` (4D). gate-0
is *partly* severed (regex arm mechanical; VM `Dfa` arm LOCK-5-gated); the *semantic* question is LOCK-5's.
**Re-label owed: "5A vs 5B" contradicts XL-C's retired-5B regex-HIR ledger** — this is the grammar-IR-sharing
question, distinct. Irreducible inside LOCK-4.

**INCOMPAT-2 — THE FAST TERMINAL, cross-language → LOCK-6 (+ evidence to LOCK-8).** Rust monomorphization ≠ TS
emission — different mechanisms, no shared isomorphic terminal. Resolved in the emitter's favour on TS
(measured); the fast Rust face is *also* codegen with a workload-dependent win. Export the EVIDENCE and let
LOCK-6 rule — the spec's "away from 6A/6B" is a soft overreach (LOCK-6's election, not LOCK-4's). Handled RIGHT
by the spec (CRIT-2 §6.6 credit): the model the other hand-offs should meet.

**INCOMPAT-3 — Q-RUNTIME: RETIRED as an abstract fork.** codegen fast face (4B) vs interpret terminal (4C-VM ≈
4A-graph-walk) is NOT incompatible — two terminals for two use-cases, selected by the cost model. The
decision-relevant comparison is the generalized codegen route vs the SHIPPED closure core, MEASURED (MIXED):
competitive but numeric-leaf-gated. The open number (canada parity) is exogenous — LOCK-9's.

---

## CROWDING & REDIRECT (pass 3 · RE-FOLD)

- **[THE governing redirect this fold] The REPRODUCTION LANE is now the redundant formulation → BANKED/CLOSED.**
  Both critics converge on it: the pass spent its ENTIRE probe budget reproducing two already-passed gates
  (G-A1, G-C0-Rust-side), closing zero enumerated gaps; a third reproduction buys nothing (CRIT-2 §6.9,
  CRIT-1 directive 1). **Mark re-measuring settled gates BANKED.** *Underexplored formulation to OPEN in its
  place — the frontier the reproductions sidestepped:* (a) 4A's **real parse-that API → `IrNode` across the
  crate seam** (the RF-1-hard step, the sharpest 4A uncertainty); (b) the CSS/`@token` arm so
  `fuse_token_dispatch` fires substantively.
- **[4C ⊕ 4D value plane — the redundant fast-terminal is BANKED; the arbitrary opaque closure is the OPEN
  formulation] 4C-as-fast-terminal is refuted twice → BANKED; the surviving contribution is the iso-of-record
  oracle + the interpret lane.** The value-plane completion is SHARED with 4D (built once). **Open in its
  place:** the ONE `Opaque(HostFnId)` + `Value::Host` shape that carries an ARBITRARY closure (research-B/D's
  proposal), evaluated on a **closure-RETURNING** case (value.js easing) — the hard case NEITHER worktree shape
  is (the emitter-plane call-out is opaque-but-unevaluated; the VM-plane HostFold is evaluated-but-closed).
  Plus G-C0's still-open cross-language half (canada crossing) + G-C4 on `rmp_serde`.
- **[the two-shapes HostClosure name is CROWDED — three gates ride one name spanning two worktree shapes,
  neither the target]** The redirect: **pick ONE shape of record** (research-B/D's arbitrary-opaque residue),
  attribute + home it (LOCK-1 TypeDesc + LOCK-7 lattice), and stop reading G-B3 / G-D-GAP1 / "non-stub
  HostClosure evaluated" as jointly converged on one type-plane truth. The reproduction asymmetrically hardened
  the VM-plane shape — do NOT let that read as the emitter-plane or the arbitrary-opaque shape being settled.
- **[the tower-walk graph — ONE mechanism, two homes] 4A-private-node-union ≈ `parse_with_ir`.** Run 4A's
  experiments on `bbnf_ir::IrNode` where the seam allows; the e-graph double-home is RETIRED on route (consumed
  path-dep, not a clone) — conditional on electing the shared route (LOCK-5).

## CROSS-POLLINATION (named grafts — both families developed enough; graft justified)

1. **4C ⊗ 4D — the value-plane completion + iso-of-record oracle (PRIMARY, BUILT-in-worktree + reproduced).**
   The `Op::ApplyFn` + `fns` + `Value`-scalar completion is *identical* for both; the live-differential probe
   built it once and crossed 12 census pairs node-for-node vs parse-that's live `.map`. Both families are
   developed enough: 4C built the VM value plane; 4D characterized the split-the-barrier tier. Justified: 4C's
   iso mechanism ≡ 4D's crossing need. Scope: supplies isomorphism-of-record + parity (*correctness*), NOT the
   fast TS terminal (XL-D). **Remaining graft (the reproduction did NOT reach):** the cross-language canada
   crossing + one arbitrary-opaque closure evaluation + one real compiled grammar.
2. **4B-TS emitter ⊗ {4A,4C,4D}-TS — the shared TS execution face (XL-D ratified).** Every TS terminal lowers
   through the build-time emitter — the only V8-survivable fast face. Whichever isomorphic terminal wins adopts
   4B-TS as its TS lowering, as 4B-Rust is the Rust lowering.
3. **4A ⊕ 4B — reify-then-emit across the staging boundary (GROUNDED further this pass).** 4A reifies the
   runtime front + runs the pre-codegen passes; 4B is the offline codegen of that reified plan. **G-A1 now
   proves 4A's fuse-inheritance half** (combinator-constructed graph byte-identical through `fuse_single_use`).
   Gate: open-Q2's reify→serialize→same-emitter round-trip (a LOCK-3 tower-parity obligation per XL-B) —
   coherent for the codegen front, never the live combinator API as a runtime terminal.

---

## THE CORRECTED HAND-OFF LIST (FIVE items + a candidate SIXTH — the reproduction round moved NONE)

1. **Q-ownership → LOCK-5** (grammar-IR sharing; INCOMPAT-1). **Re-label owed:** "5A vs 5B" contradicts XL-C's
   retired-5B regex-HIR axis — name a supersession or route the (distinct) grammar-IR-sharing question home.
   gate-0's VM `Dfa` arm is a LOCK-5 extraction (tree-confirmed bytecode.rs:184/238).
2. **The fast-terminal cross-language iso → LOCK-6** (Rust monomorphization ≠ TS emission ⇒ 6C/6D; INCOMPAT-2).
   Export the EVIDENCE, let LOCK-6 rule. Route PROTO-1's emitter/V8-discipline result to **LOCK-8**. *(Handled
   RIGHT by the spec — the model hand-off.)*
3. **The value-plane ISA home → LOCK-3 (XL-B) FIRST, then GESTALT-Ω.** `Op::ApplyFn` + `Value` scalar arms are
   a tower-shape change LOCK-3 owns pass-3+; GESTALT-Ω arbitrates only the internal-descriptor (→LOCK-3) vs
   host-closure (→LOCK-7) split. **Correct the "correct-of-record" overstatement** — worktree-only, `rg
   ApplyFn`=0 on `90db093bf`.
4. **4B's perf-claim supersession → PT-tranche (GESTALT-P).** XL-E settles the principle; the *placement* in a
   PT-tranche letter is an open constitution-ix debt.
5. **[the double-home the spec mis-filed as internal] The 4B numeric-leaf ceiling → LOCK-9** (the `number_simd`
   kernel, 9B, actively rebirthing) **+ LOCK-6/8** (the padded-input `parse()` contract). 4B's election is
   exogenous; it cannot elect inside LOCK-4 even in principle.
6. **[CANDIDATE SIXTH — CRIT-2 §6.4, NEW] The TS `Dfa` port that unblocks G-C1 has NO owning lock.** G-C1 (the
   cross-language crossing, LOCK-6's 6D evidence) is DAG-blocked on porting `parse_that::regex::Dfa` to TS.
   research-C §8 exports the `Dfa`-vs-`RegExp` *divergence* risk to LOCK-5; the ACT of porting floats homeless.
   Name its home (LOCK-8 / LOCK-5-TS / LOCK-9-via-5C) or G-C1 — and LOCK-6's behavioral-iso — stays permanently
   DAG-blocked.

**Plus two provenance/ownership debts owed to the gestalts:**
- **The `FnDescriptor::HostClosure` type-plane pre-seed is unattributed, homed in LOCK-1's TypeDesc plane, and
  now ASYMMETRICALLY HARDENED** — two incompatible shapes wearing one name (emitter-plane call-out untouched;
  VM-plane HostFold reproduced twice), neither the arbitrary-opaque target, `rg HostClosure`=0 on record.
  Attribute it, home it (LOCK-1 + LOCK-7), pick ONE shape, verify constitution-v.
- **The G-B1 baseline is cross-lock unstable** — LOCK-9 9B is moving the incumbent's `number_simd`; the MIXED
  verdict must be re-based when 9B lands (base-law-adjacent).

---

## CROSS-LOCK DAG ORDERING (for GESTALT — pass 3 · RE-FOLD)

- **[FORCED] LOCK-4 ⊗ LOCK-6 are one joint decision.** INCOMPAT-2 forces 6C (wasm-core) or 6D (behavioral/
  oracle iso). The terminal cannot freeze ahead of LOCK-6; G-C1 (LOCK-6's 6D evidence) is DAG-blocked on the
  homeless TS `Dfa` port (candidate SIXTH hand-off).
- **[FORCED] LOCK-4 ⊗ LOCK-9 — the numeric-leaf ceiling.** 4B's election ceiling IS LOCK-9's `number_simd`
  kernel; the G-B1 baseline moves under 9B's active rebirth. A LOCK-4/LOCK-9/LOCK-6 seam (the padded-input
  `parse()` contract). The most under-weighted cross-lock fact in the pass.
- **[CORRECTED] LOCK-4 ⊗ LOCK-5 — Q-ownership.** gate-0's regex arm mechanical/done; the **VM `Dfa` arm is a
  LOCK-5 extraction** (bytecode.rs:184/238); shared-substrate-semantics is LOCK-5. Re-label off the retired
  5A/5B regex-HIR axis.
- **[XL-B] LOCK-4 ⊗ LOCK-3 — the VM value-plane ISA home.** `Op::ApplyFn` + `Value` scalar arms are a
  tower-shape change; routed past LOCK-3 straight to GESTALT-Ω by the spec — corrected to LOCK-3-first.
  Worktree-only status mitigates (`rg ApplyFn`=0 on record), but the repro's "correct-of-record" over-claims.
- **LOCK-4 ⊗ LOCK-1 — the HostClosure type-plane pre-seed.** `arg/ret:TypeDesc` is LOCK-1 solver territory; the
  pre-seed anchoring G-B3/G-D-GAP1 is unattributed, un-adjudicated, and asymmetrically hardened; constitution-v
  untested.
- **LOCK-4 ⊗ LOCK-7 — the host-fn lattice + the closure-RETURNING residue.** The arbitrary opaque closure is
  unbuilt + unevaluated; the closed `HostFold` shape pre-empts LOCK-7's lattice with an unsanctioned shape,
  now twice-landed.
- **[route to avoid re-derivation] LOCK-4 ⊗ LOCK-8.** PROTO-1's "fast TS terminal = build-time emitter" is
  LOCK-8 territory; the node-ratio FAIL region (value.js 4.84%) is LOCK-8's flagship consumer — hand it over
  explicitly (CRIT-2 §6.7).
- **[extant-tranche, owed] XL-E supersession → PT-tranche (GESTALT-P)** — principle settled, placement open.

---

## THE PASS-4 KEYSTONE SET (the FRONTIER — the OLD keystones G-A1 + G-C0-Rust-side are DISCHARGED; do NOT re-run them)

1. **4A's REAL-API cross-seam reification** — wire parse-that's shipped `SmallBox<dyn ParserFn>` combinator API
   (NOT a bbnf-side builder) to emit `IrNode` across the `parse_that↔bbnf_ir` seam; re-run G-A1 byte-identity.
   Plus the **CSS/`@token` arm** so `fuse_token_dispatch` fires substantively. The RF-1-hard step, unbuilt four
   probes.
2. **4C's cross-language + arbitrary-opaque frontier** — port the TS `Dfa` (or restrict to LOCK-5's certified
   class), cross the six corpora incl. **canada**; fire G-C4 on `rmp_serde::to_vec_named`; **evaluate one
   closure-RETURNING `Opaque(HostFnId)`** (value.js easing), not a numeric HostFold. Compile a real grammar
   through the front-end.
3. **4B's canada decision + G-B4's number** — build the padded-input `parse()` contract (with LOCK-9's
   `number_simd` + LOCK-6's input-contract iso) and re-measure, OR record 1.56× as the measured exogenous
   ceiling. Give **G-B4 a firing-budget curve** on CSS + value.js, or record it un-gateable (FIFTH pass unset).
4. **4D's tier-1 BUILD** — land `MapExpr::Array` + `MapExpr::Select` + `FnDescriptor::Record`; re-lower the 44
   latex-paper sites; assert `constant_fold`-total + serializable. Convert "~52% dissolves as data" from a
   source count into a compiled artifact.
5. **Pick ONE `FnDescriptor::HostClosure` shape of record** (research-B/D's arbitrary-opaque residue),
   attribute + home it (LOCK-1 TypeDesc + LOCK-7 lattice), verify constitution-v.
6. **Route + re-label:** the VM value-plane ISA to LOCK-3 (XL-B) FIRST; re-label INCOMPAT-1 off the retired 5A/
   5B axis; name the TS `Dfa` port's owning lock; place XL-E in a PT-tranche letter (GESTALT-P); correct the
   "correct-of-record" overstatement to correct-against-a-live-oracle-in-worktree.

---

## PASS HISTORY

### Pass 3 · RE-FOLD (2026-07-16, 23:xx) — the two keystones RAN + PASSED + REPRODUCED; the frontier did NOT move

**Supersedes the 16:24 provisional pass-3 fold** (below), which was written before the pass's keystone probes
ran. **Events.** 4A's G-A1 (SPEC-ONLY four passes) and 4C's G-C0 REAL gate (the McKeeman-trap) — the 16:24
fold's two named pass-4 keystones — **both ran, both passed, both were independently reproduced.**
PROTO-1-4A-GA1 (19:40) + RERUN (22:42): combinator-constructed `IrNode` byte-identical through the fuse passes
(0/10 pre- and post-fuse), identical 37-node DAG, twitter-parity, timing in-band both runs — a twice-measured
anchor. PROTO-2-live-differential (19:43) + mckeeman-repro (22:40): VM `Op::ApplyFn` byte-identical to
parse-that's LIVE in-process `.map` (12/12 census pairs), `HostFold` genuinely EVALUATED (source-audited
`apply_fn.rs:94–104`), cold-reproduced (57 crates, 87/87). **Governing correction (both RE-CUT critics):
REPRODUCTION-IN-PLACE-OF-COVERAGE** — four probes touched two gates and closed ZERO of the ~12 enumerated gaps;
the reruns bought robustness (single-run-fluke retired, non-stub source-audited), not frontier. The sharpest
uncertainties both originals sidestepped — 4A's real parse-that API across the crate seam, 4C's arbitrary
opaque closure + the canada cross-language crossing — remain untouched across all four probes. **Second
correction: the two-shapes HostClosure mess is now ASYMMETRICALLY HARDENED** — the VM-plane `HostFold` shape
twice-anchored, the emitter-plane shape untouched, neither the arbitrary-opaque target, `rg HostClosure`=0.
**Tree-verified on live HEAD `90db093bf`** (newer than critics' `3f2350f15`): `rg ApplyFn`=0, `rg
HostClosure`=0, `FnDescriptor`=6 variants (value plane + pre-seed WORKTREE-ONLY, UNBUILT of record; the
"correct-of-record" verdict is an equivocation); `parse_that::regex::Dfa` dep + `parse_that="0.4"` cycle +
`try_factor_alt` at `>=4` + vm 1772 LOC all CONFIRMED.

**Critic floors folded (RE-CUT critics).** CRIT-1 v2 (22:57, within-lock): 4A 78 / 4B 72 / 4C 80 / 4D 70.
CRIT-2 ② (22:58, cross-lock): 4A 73 / 4B 66 / 4C 63 / 4D 58. **Dispositions (conservative floor, no critic
error named):** 4A **ADVANCED 73** (keystone built + reproduced = the largest earned rise; real-API cross-seam
unbuilt four probes; CSS unrun; open-Q2 a LOCK-3 XL-B obligation; value.js node-FAIL); 4B **ADVANCED 66 ·
terminal-role = the codegen fast face** (−4 is a cross-lock RE-CUT NOT regression — no probe touched it;
strongest mechanism; election exogenous three-lock-away; G-B4 no number FIFTH pass; baseline moves under
LOCK-9); 4C **BANKED 63** (G-C0 discharged + cold-reproduced + source-audited — strongest within-lock datum;
floor is cross-lock — "correct-of-record" false on tree, canada cross-language crossing UNRUN [the raison
d'être], hard case a twice-anchored substitute); 4D **BLOCKED 58** (three horns; "~52% dissolves as data" is a
SOURCE COUNT unbuilt; closure-returning residue unbuilt + unevaluated; INCOMPAT-1 contradicts XL-C; DAG-blocked
LOCK-5 + LOCK-7). **No family converged; the terminal is un-electable inside LOCK-4.** The reproduction lane is
BANKED; the frontier (real-API seam, CSS/@token, arbitrary opaque closure, canada crossing, G-B4 number, tier-1
BUILD) is the pass-4 keystone set.

### Pass 3 · provisional early fold (2026-07-16, 16:24) — SUPERSEDED by the RE-FOLD above

Written before the pass's keystone probes ran; folded the 15:22 FAMILY-SPECS + PROTO-1/-1-numeric-leaf/-2/
-2-vmvalue + the EARLY critics. **Dispositions:** 4A ADVANCED 68 (G-A1 "0% built, four passes"), 4B ADVANCED 70
(codegen fast face; election three-lock-away), 4C BANKED 66 (value plane worktree-built 8/8 but "REAL gate
untested — the McKeeman trap"), 4D BLOCKED 66 (three horns; GAP-1 type-plane discharged). It named as the
pass-4 keystone set exactly the two probes that then RAN + PASSED this pass (G-A1; G-C0's REAL gate) — the
reason it is superseded. Its "PROTO-1 MIXED / numeric lever FAILED / value plane worktree-only" facts carry
forward unchanged; its convergence framing (G-A1 unbuilt, G-C0 untested) is dead.

### Pass 2 (2026-07-16) — TWO prototypes RAN; the sharper ran NEGATIVE; the TS face is measured-decided

**Events.** PROTO-1 (TS execution face) RAN/MEASURED/**PASS** — build-time emitter ×1.23 over `.parser`-IC
baseline, runtime switch-decoder −22% (ties naive); INCOMPAT-2 decided *by measurement* → **emitter, not
decoder** (later ratified XL-D). PROTO-2 (cycle-break + value floor) RAN — **gate-0 PASS** (cycle mechanically
severable) and value-plane floor **MEASURED-NEGATIVE** (0.021–0.46×; oracle byte-identical GREEN; loss = per-
parse setup allocation). **Corrections:** (i) CRIT-1 RF-1 — the tower's `4A→4B` "Rust lowering" arrow is
SPEC-ONLY and possibly incoherent (monomorphization needs comptime types; combinator API builds a runtime
`IrNode`). (ii) CRIT-2 — value floor RAN NEGATIVE; value-completion unblocks *correctness*, NOT a competitive
Rust runtime. **Critic floors.** CRIT-1: 4A 50 / 4B 60 / 4C 48 / 4D 45. CRIT-2: 4A 50 / 4B 58 / 4C 32 / 4D 30.
**Dispositions:** 4A ADVANCED 50; 4B ADVANCED 58 · terminal-role BANKED→compile-backend + TS-emitter; 4C BANKED
32; 4D BLOCKED 30. No family converged; the terminal un-electable; the TS face measured-decided (emitter).

### Pass 1 (2026-07-16) — agglomerated, then RE-FOLDED after PROTO-2-4B-FUSION + MONADIC-WALL-CENSUS

**Re-fold event.** The first agglomeration (4A 50 / 4B 32·REWARD-AT-RISK / 4C 30·BANKED / 4D 25·BLOCKED) was
written before the lock's deciding throughput experiment ran. PROTO-2-4B-FUSION then RAN and CONFIRMED (0 fused
`blr` vs 60 closure, 1.16–1.27× cold-paired, recursion proven); PROTO-1-MONADIC-WALL-CENSUS RAN GREEN (opaque-
dynamic 0.525% ≤ 2% — a *structural* floor only). **Re-folded floors.** CRIT-1: 4A 45 / 4B 50 / 4C 28 / 4D 25.
CRIT-2: 4A 48 / 4B 50 / 4C 28 / 4D 27. **Dispositions:** 4A ADVANCED 45; 4B ADVANCED 50 · terminal-role
BANKED→compile-backend; 4C BANKED 28; 4D BLOCKED 25. The reframe (not the number) was the finding: 4B's win is
Rust-monomorphization with no V8 analog, forcing LOCK-6 away from source-mirroring toward 6C/6D.
