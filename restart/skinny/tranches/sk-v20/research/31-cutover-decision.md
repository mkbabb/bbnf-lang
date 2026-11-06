# 31 — The Cutover Decision: when skinny STOPS and totality BEGINS

Auditor: 31-cutover-decision. Date: 2026-06-10. HEAD: `36ca10646`.
Scope: the orphaned synthesis dimension (`30-completeness-critic.md:40-72`, GAP-1).
No sibling report owns this; the predicate below is constructed from the evidence in
03/04/05/09/22/24/28 plus the SK-V19 SPEC close conditions and the report-02 Lock census.
Read-only; all status claims trace to a sibling report or a `git`/file fact at HEAD.

---

## Verdict in one line

**Cut over NOW — but "cut over" does not mean "stop proving"; it means INVERT the
default writing surface.** The skinny tree's proving function is *exhausted for the
substrate, value-plane, and decision waves* (W0-W5 of SK-V19): every architectural
question those waves answer has already been answered in skinny and is sitting unported
(reports 24/28/22 each confirm the skinny artifact exists and the `crates/` artifact does
not). What remains is not *proof* but *transfer*, and transfer can only be proven in
`crates/`. The campaign is in the exact failure shape the pre-restart catalogue names
Mechanism C — "substrate-first without consumer at wave close" (`01:250-253`) — except
inverted: the consumer (skinny) is proven and the *production substrate* is the thing
without it. Every additional skinny cycle widens the gap (report 04 §5.4) without
narrowing it. The go/no-go is therefore **GO**, gated not on more documentation but on a
**hard runtime gate in `crates/`** defined in §2.

---

## §1 — What ONLY skinny can still prove cheaply (and the answer is: almost nothing)

The skinny tree's purpose is proving, not shipping. A cutover is justified the moment the
*cheap-to-prove-in-skinny* queue is empty. It is.

**Exhausted (proven in skinny, nothing left to learn there):**

- **SoA tape** — proven and portable verbatim (`28:100-127`): columns
  `offsets/flag_cursors/flag_values/payloads`, commit-by-construction `TapeBuilder` with
  zero checkpoint surface. Report 28's recommendation is explicit: "Port skinny's `tape/`
  module wholesale." There is no open skinny question; the only work is the
  `StructBuilder`-trait shim in core (`28:159-161`).
- **Un-forked generator** — proven across 3 grammars (JSON/CSS/Sheets) in SK-V18
  (`03:258-276`), with the negative control (Sheets, a structurally novel grammar) green.
- **5-shape `BackendShape` as a lowering dispatch** — proven load-bearing *as a shape
  filter* (`24:18-23`). What is NOT proven anywhere — and crucially *cannot be proven in
  skinny either* — is the cost model: `perf_cost == 0` at all eight sites (`24:80-88`),
  the CSP is a tautology (`24:63-73`), the e-graph rewrite is cosmetic (`24:54-62`). The
  cost model is **net-new work that does not exist in skinny** (`24:212-218`). So even the
  decision engine has no remaining *cheap skinny proof* — its missing piece is a measured
  kernel table that must be populated on the M5 Max against `crates/` kernels.
- **NEON structural classifier (G6)** — proven on 3 grammars (`03:264`).
- **Lazy-rich value HEAD over the tape** — proven, with the depth/retention asymmetry
  honestly disclosed (`14:42-54`, `22:136-148`).

**Still genuinely open in skinny (the ONLY residual skinny-cheap items):**

- **`json_templates` Phase 3 un-courier** — the PASS-IMPL marks "Phase 3 pending even in
  skinny" (`05:151-155`, `22:79-85`). The generalized `general_descent` emitter cannot yet
  derive JSON's rich keyed/recursive navigation (`get`/`pairs`/`values`); the rich tree is
  still the hand-authored `json_templates` courier carrying a *false* `@generated` header
  (`22:74-78`). This is the one architectural question with a remaining cheap skinny proof:
  **derive container-access navigation from grammar STRUCTURE** (report 22 Axis B,
  `22:160-172`). Until this lands in skinny, the W3 port to core has no clean precedent.
- **The decision-engine cost-table shape** — the *design* (report 24 §1-§5) can be
  prototyped in skinny, but its *currency* (measured cycles/64B per kernel per uarch) must
  be captured against real kernels — equally cheap in either tree, and the kernels live in
  core's target after W5d.

**Conclusion:** the proving function is ~95% exhausted. Exactly ONE architectural question
(structural derivation of rich navigation, report 22 Axis B) is left that skinny can prove
more cheaply than core. Everything else is transfer. This single residual is the gating
*skinny* item — but it is small (one emitter derivation rule) and must not be allowed to
spawn another full 9-pass cycle (report 04 R4 caps cycles at 3 passes). It is a W3
sub-task, not a tranche.

---

## §2 — The go/no-go predicate as a HARD RUNTIME GATE in `crates/`

Per the precepts (`no-deferrals`, AX-invariant-13 "ledger-only wave = re-plan trigger"
`01:86-90`) and report 30's directive (`30:68-71`), the cutover predicate is **not a
documentation milestone**. It is a bench+oracle that must be GREEN in `crates/` for a
**Minimum Shippable Totality Slice (MSTS)**. The MSTS is deliberately *one grammar end to
end on the production substrate*, not the 9-fleet — because the thesis the cutover proves
is "the skinny architecture transfers and preserves >SOTA in core," and that is provable on
a single grammar pair.

### The MSTS gate (`xtask cutover-gate`, exit 0 = GO):

**Grammars in the gate:** TWO — **JSON** (the rich-recursive navigation case, the hardest
value-plane derivation and the >SOTA-load-bearing one) and **Sheets** (the flat-formula
negative control that SK-V18 proved generalizes; `03:264`). Two grammars is the minimum
that distinguishes "ported one parser" from "ported the *generator*." CSS is added at the
*second* wave (it carries the lightningcss >SOTA claim but also the depth-asymmetry framing
burden; it must not gate the first landing).

**Oracles that must be green (all independent or the gate is theatre — report 15):**

1. `json_parity` vs **sonic-rs full-tree** AND **serde_json** (`15:31`, the 12-field
   `JsonDirectDigest`) — field-for-field, in `crates/core/tests/`, over data/twitter/
   citm/canada. The skinny self-referential `parity.rs` (`15:71-77`) does NOT count.
2. `sheets_parity` typed-payload walk — but note the open deficiency: Sheets has only
   self-referential oracles today (`15:33`, `15:141-148`). The MSTS must add at minimum a
   *byte-canonical re-parse idempotency* gate; a full external evaluator is post-MSTS.
3. `lock14_grammar_generalization` (`crates/ir/tests/`) **widened** per report 09's
   end-state design: roster derived from `[workspace.metadata.bbnf-strategy]`
   (`09:137-140`), scanning *all* generic crates (`ir`, `analysis`, `passes`, `grammar`,
   and the lowering surface of `core` — `09:142-146`), with a re-inject falsifier per
   surface (`09:148-150`). The current gate is green-by-exclusion (`30:109-133`,
   verify-1/verify-0): it scans only `ir`+`analysis`, excludes the xtask emitter that
   still hardcodes `"css_l4"`, and `shape_dict_bbnf.rs` + `GrammarAuditTag` remain
   grammar-named in a generic crate. **Lock-14 is RED in substance** and the cutover gate
   must read RED→GREEN on the *widened* scan, not the narrow one.

**Bench planes that must hold (cold, same-run, disclosed — report 14):**

4. `json_direct_to_struct` / `real_typed_struct` in `crates/core` bench > sonic-rs strict
   struct-deserialize, **cold per-parse** (NOT Criterion-warm — report 14 §2.3 names the
   thermal mismatch; the core gate must use ONE thermal discipline). The parse_only plane's
   +164.7% headline is excluded from the gate — it is a recognizer-vs-recognizer margin
   (`14:130-150`) and must not be conflated.
5. **No regression vs the pre-fold core baseline** captured at HEAD before W1 dispatch
   (the `sota_preserved` close condition, `SPEC.md:25-26`). This is the throughline gate.

**Lock gates that must close for the MSTS:**

6. **Lock 1** (`02:17-38`): `rg TapeRec crates/` == 0; exactly one SoA encoding; no eager
   `StructBuilder` family for the two MSTS grammars.
7. **Lock 10** (`02:150-164`): `rg 'enum BackendShape' crates/ir` == 1, 5 variants; and —
   the part report 24 insists on — `perf_cost` is non-zero and table-derived for the two
   MSTS grammars, so the selector *moves at least one shape by an executable cost row*
   (`SPEC.md:23-24`, the `decision_load_bearing` close condition). A constant selector
   fails the gate (`SPEC.md:118-119` falsifier).
8. **Lock 14** (widened, item 3 above).

The gate's telemetry columns are exactly the SK-V19 SPEC's (`SPEC.md:139-142`):
`tape_encoding`, `dual_encoding_present`, `generator_grammar_branch_count`,
`verbatim_blob_present`, `json_value_from_ref_byte_equal`, `backend_shape_canon[5]`,
`all_five_gate`, `lock14_roster_hits`, `sota_json_ratio`, `host_loadavg`. Every column
consumed at close; `host_loadavg < 1.0` is mandatory (report 14 §1 — no committed
`track1_rich` figure has ever been quiet-clean; the core gate must end that).

**This gate is the cutover.** When `xtask cutover-gate` exits 0 on JSON+Sheets in
`crates/`, the campaign has *demonstrated transfer* and skinny's authority ends.

---

## §3 — Cutover mechanics: what happens to skinny and to the SK-V19 SPEC

### The skinny tree at cutover: FROZEN as reference, NOT deleted, NOT the iteration harness

Three options, adjudicated:

- **Delete** — rejected. The skinny tree is the only place the architecture is *proven
  green end-to-end*; deleting it before core's MSTS gate passes removes the differential
  oracle (skinny-vs-core parity is the safest port-correctness check during the fold).
- **Keep as fast-iteration harness** — rejected *as a standing posture*. Report 04 §5.4 is
  decisive: every cycle run in the skinny context "adds more proven skinny architecture
  that must later be ported, without narrowing the gap." A standing skinny harness is the
  divergence engine. If skinny remains the place new architecture is born, the cutover
  never completes.
- **Freeze as reference + differential oracle** — **selected.** At the W0 ledger commit
  (the D-G reconcile, §0.5 below), tag skinny `sk-v19-frozen` and declare it
  **read-only for architecture**. It survives ONLY as (a) the differential parity source
  during the fold (core output must byte-match skinny output for the same grammar), and
  (b) the executable reference for the one open skinny residual (report 22 Axis B), which
  lands in skinny FIRST then ports. Once the MSTS gate is green and the 9-fleet fold
  completes, skinny is archived under `restart/skinny/archive/` — not deleted (provenance),
  not live (no new commits).

### The certified SK-V19 W0-W6 SPEC: absorbed / amended / superseded, per wave

The SK-V19 SPEC (`restart/skinny/tranches/sk-v19/SPEC.md`) is the certified wave manifest.
But report 28 proves its arithmetic is stale at HEAD (six counts wrong, `28:46-87`) and the
W6 fold already executed ahead of the substrate it was sequenced to follow (`28:88-98`).
The SPEC is the right *thesis* with the wrong *numbers*. Disposition per wave:

| Wave | Disposition | Reason |
|---|---|---|
| **W0** (substrate_target classify) | **AMEND** — re-pin to HEAD `36ca10646`; the `strategy.rs:134-189` line-ref no longer exists (`28:79-87`, W6a already landed). Keep as the W1-gating classification, 0 behaviour LOC. | Stale line-refs only; thesis intact. |
| **W1** (eager builders retire) | **AMEND counts** — "11 families" is FALSE (3 structural families / 4 hand impls + 1 generic; `28:48-52`); "was 47" understates the delete surface ~23× (actual ~1080; `28:67-72`). Re-budget against ~1080 sites. | Executor budgeting against 47 is "blindsided" (`28:72`). |
| **W2** (AoS→SoA) | **ABSORB verbatim** — port skinny `tape/` wholesale; delete AoS `TapeRec` + `tape_substrate.rs` in the same fenced landing (`28:130-151`). Co-land with W1 (report 28's "single fused landing"). | The thesis is exactly right; only the start-count (41 `TapeRec` hits, `28:74-77`) needs stating. |
| **W3** (C fold value-plane) | **AMEND + EXTEND** — absorb the un-courier thesis; ADD report 22's missing *derivation rule* for keyed/indexed/scalar navigation from grammar shape (`22:204-211`). Without it W3 "one generator" still cannot reproduce JSON's `get`/`pairs` and regresses to a flat sweep (the W3 falsifier itself, `SPEC.md:91-92`). | The SPEC under-specifies the single hardest gap; report 22 supplies it. |
| **W4** (StructRegistry compile-time fence) | **ABSORB** — the 0-LOC fence on W3; subsumed by report 22 Axis B (derived navigation emitted as concrete code, `22:212-221`). | W3 and W4 are "one move" (`22:223-225`). |
| **W5** (decision engine + kernel) | **SUPERSEDE the cost-model leg; absorb the rest.** W5a (introduce enum) absorbs. W5b/W5c must be **rewritten** per report 24: the skinny CSP/e-graph are theatre to RETIRE not port (`24:220-223`); the net-new is a measured `KernelCostTable` (`24:136-148`) AND the unification with core's *existing* parallel `csp_strategy` engine (1361 LOC, `24:110-125`) — a risk the SPEC does not name. W5d (x86/wasm delete, `05:181-182`) absorbs. | The SPEC asserts a cost model that does not exist; report 24 proves it and supplies the real design. |
| **W6** (leak collapse) | **PARTIALLY DONE — re-sequence.** W6a/b/c/e are DONE at HEAD (`05:33-37`) but landed *first*, producing a false-green Lock-14 gate over a stub (`05:200-207` F1; report 09 firewall). W6d (StructLayout→Layout, 960 sites) OPEN. The DONE items must be RE-PROVEN green *after* W3/W5 land, because the widened Lock-14 gate (§2 item 3) will catch what the narrow gate excludes. | A green gate over a stub is not a correctness property (`05:204-207`). |

Net: the SPEC is **absorbed as the wave skeleton, amended for HEAD-stale counts, extended
with report 22's derivation rule, and superseded on the W5 cost-model leg.** It is not
discarded — its sequencing DAG (`SPEC.md:138`) and close conditions (`SPEC.md:15-30`) are
the cutover gate's backbone.

---

## §4 — Risk adjudication: premature cutover (report 01) vs paperwork loop (report 04)

The two reports pull opposite directions. The synthesis must adjudicate, not average.

**Report 01 (pre-restart divergence) warns: do NOT cut over prematurely.** Its taxonomy
names the exact ways a too-early implementation start failed before:

- **Mechanism C — substrate-first without consumer at wave close** (`01:250-253`; D-3, D-4,
  D-6, D-9, D-12, D-13). The DTA/PSI interpreter built ~400 commits of substrate with 0/17
  production grammars green at any wave close (`01:71-90`, note: 0/17 not 0/19 per
  verify-0, `30:145-150`). The columnar tape split (D-13) shipped without a bench gate and
  required reversion (`01:227-237`).
- **Mechanism E — deferral chains with no hard runtime gate** (`01:259-261`; D-10). A
  single item deferred eleven consecutive tranches (`01:184`).

**Report 04 (meta-process) warns: the loop is producing paperwork, not code.** Doc:code
ratio 7.3:1 in SK-V18 (`04:20`); three of four largest planned architectural items
(SoA tape, C-fold, BackendShape+engine) have not shipped to `crates/` in any restart cycle
(`04:124-149`); the §3Z convergence protocol certifies prose not code (`04:260-266`); the
astral G-Omega layer audits the audit of the audit with undemonstrated value (`04:117-120`).

**Adjudication — these are NOT in tension; they name the SAME failure from opposite ends.**
Both reduce to one invariant: *no wave closes without a runtime consumer green at the
closing commit.* Report 01's Mechanism C is "substrate shipped, no consumer" — a code-side
failure. Report 04's paperwork loop is "consumer-readiness re-litigated in docs, never
built" — a process-side failure. The pre-restart era failed by shipping substrate too eager
(C); the restart era is failing by *never shipping it at all* (04). The resolution that
satisfies both:

> **Cut over now, but ONLY behind the §2 hard runtime gate.** The gate IS Mechanism C's
> prevention ("every substrate layer must have a runtime callsite proved green at the
> closing commit," `01:269-270`) AND report 04's cure (a binary code gate replaces the
> documentation convergence ceremony, `04:219-226` R3). A cutover that begins W1
> implementation in `crates/` with the MSTS gate as the W3/W5 close condition is *not*
> premature — it is the first wave in the entire restart campaign that carries a runtime
> consumer at close. It is the opposite of the DTA/PSI failure: it ships the production
> substrate *with* its proven consumer (skinny-as-differential-oracle) green.

Report 04's specific process surgery (R1 delete astral layer, R3 binary gate, R4 cap at 3
passes, R5 gate on a chronic item shipping) becomes the *operating discipline* of the
cutover tranche. R5 in particular is the cutover's spine: the next planning cycle may not
open until a CHRONIC item (`05:46-94`: SoA tape, C-fold, BackendShape — deferred 3-5
cycles) actually lands in `crates/`. The cutover gate forces exactly that.

The residual premature-cutover risk report 01 would still raise: the D-G pre-block reconcile
(items 51/53/246/247, `05:96-104` CHRONIC-6, `SPEC.md:47-48`) is a hidden dependency for all
W0-W6 substrate work and is NOT on the committed ledger. **This is the single act that must
precede the cutover.** It is a documentation-only commit (`05:140-142`), so it costs nothing
but blocks the second-substrate/streamed-cursor REJECT routes from re-opening when W2
touches the tape. It lands as the *very first* cutover commit, before any W1 code.

---

## §5 — Dated, conditional recommendation

**Recommendation (2026-06-10):**

> **CUT OVER WHEN the D-G pre-block reconcile (items 51/53/246/247) is committed to the
> ledger** — a single documentation commit, the only true precondition. That commit opens
> the **first totality wave: W1∥W2, the fused substrate landing** (port skinny `tape/`
> wholesale into `crates/core`, delete AoS `TapeRec` + the eager builder families for JSON
> and Sheets in one fenced commit, gated on JSON+Sheets full-fleet parity green — report
> 28's "single fused landing," `28:153-165`).

This is GO, not no-go. The proving function is exhausted (§1); the architecture is proven
and portable (reports 24/28/22); the only thing that has never been proven is *transfer
into `crates/`*, and that is unprovable in skinny by construction (report 04 §5.4). The
risk that report 01 guards against is neutralized by making the MSTS runtime gate (§2) the
W3/W5 close condition — the first restart wave to carry a runtime consumer at close.

**The cutover sequence:**

1. **Cutover commit 0** (doc-only): D-G reconcile on the ledger. Tag skinny `sk-v19-frozen`,
   declare it architecture-read-only. *This is the cutover boundary.*
2. **First totality wave — W1∥W2** (fused substrate): the SoA tape port + eager-builder +
   AoS delete, gated on JSON+Sheets parity. Lands the longest-running chronic item
   (SoA tape, deferred 4+ cycles, `05:46-54`) and satisfies report 04 R5.
3. **Second wave — W3⊗W5 fenced by W4** (value plane + decision engine + cost table),
   closing on the §2 MSTS gate. This is where >SOTA-preservation is re-proven in `crates/`
   and the `perf_cost`-is-zero theatre (report 24) is replaced with a measured table.
4. **Skinny residual (parallel, pre-W3):** land report 22 Axis B (structural navigation
   derivation) in frozen-skinny as the W3 precedent, then port. The ONE remaining cheap
   skinny proof.
5. **Third wave — fleet completion**: extend MSTS from 2 grammars to 9; add CSS (carrying
   its disclosed depth-asymmetry framing, `14:42-54`); W6d StructLayout→Layout rename;
   re-prove the widened Lock-14 gate green over real code, not a stub.

**No-go condition (the gate that would HALT a cutover):** if, after W1∥W2 lands, the
JSON+Sheets parity oracles in `crates/core` cannot be made green without a contrivance — or
if the W3/W5 MSTS bench shows the SoA fold *regresses* JSON >SOTA vs the pre-fold core
baseline and the cost-gated `(a)-(d)` primitives cannot recover it (the D11 falsifier,
`SPEC.md:91-92`) — then the cutover surfaces an **N outcome honestly** (`SPEC.md:29-30`):
the skinny architecture does not transfer without paying a throughput tax, and the campaign
re-plans the value-plane derivation. That is the only legitimate reason to retreat to
skinny — and it is a code finding from a runtime gate, never a documentation re-litigation.

**Bottom line:** the skinny tree has finished its job. The 32-agent SK-V20 research pass is
itself the last paperwork the campaign can afford before the gap (report 04) becomes
permanent. Commit the D-G reconcile, freeze skinny, and dispatch W1∥W2 against the runtime
gate. The cutover is not a future milestone to plan toward; the predicate is met today save
one doc commit.
