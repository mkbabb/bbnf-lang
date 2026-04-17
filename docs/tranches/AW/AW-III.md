# Tranche AW-III — DTA Correctness & Viability Validation

AW-III closes the correctness arc AW-I + AW-II opened: every residual
failure deferred from AW-II, every `#[ignore]` that accumulated across
the preceding tranches, and the load-bearing question this whole arc
leaves unanswered — **is the DTA-primary parse path viable?**

At AW-II close, workspace sits at **1050 passed / 50 failed / 67 ignored**
and the 14 measured bench entries show **5–40× regression vs post-AU**.
The regression is uncomfortable: it could mean the DTA has genuine
head-room AW-IV's optimisation levers recover, or it could mean the
state-machine-interpreted walker is fundamentally outclassed by the
per-rule inlined emitter it replaced. AW-III answers that question
empirically before AW-IV spends another tranche on optimisation work
that rests on a premise it hasn't validated.

Scope triad:

1. **Correctness closure** — every AW-II residual (Cluster A parse
   failures, Cluster C payload activation, Cluster D integration)
   lands green. No `#[ignore]` added; every existing one audited
   (close or delete honestly).
2. **Viability profile** — samply-measured attribution of the 5–40×
   regression across representative bench entries. Bottleneck named,
   not guessed.
3. **Minimum-viable specialisation** — activate the smallest coherent
   set of AW-IV levers that the viability profile implicates (typically
   PSI rayon + ShapeRef dispatch + PHF/SIMD keyword). If closing the
   regression gap to < 2× of post-AU is infeasible even with every
   lever active, AW-III declares DTA non-viable and escalates an
   architecture decision back to the user — not silently-deferred
   optimisation work.

AW-IV's wave schedule (the optimisation arc, formerly AW-III) presumes
viability proven. AW-III proves or disproves it.

## Architectural thesis

The DTA walker is a state-machine interpreter over a table emitted per
grammar. Every input byte visits ≥1 walker state; every state transition
costs ~10s of CPU cycles (match dispatch + Frame update + PSI/counter
bookkeeping + branch-prediction miss). The pre-DTA fn-per-rule path
compiled each rule to an inlined Rust function, letting LLVM inline the
entire parse tree; the CPU pipeline stayed warm and per-byte cost fell
below one cycle for common cases.

The 5–40× regression is exactly the arithmetic of state-machine
dispatch overhead versus inlined recursive descent. AW-IV's levers
amortise some of this cost (PSI rayon parallelises writes; ShapeRef
collapses same-shape rules; PHF + SIMD keyword dispatches the common
Alt fast-path in one probe), but none of them eliminate the per-byte
dispatch baseline.

**The load-bearing question**: given every AW-IV lever active, can the
DTA walker land **within 2× of post-AU** on the 14 measured entries?

- **Yes**: DTA is viable; uniform shape + replay/recovery/incremental-
  reparse benefits outweigh the modest regression. AW-IV is green-lit.
- **No**: DTA is a correctness-architecture win with an unacceptable
  performance cost. AW-III escalates the architecture decision:
  revert to fn-per-rule (undoing AW-I/II), accept the regression as a
  documented tradeoff, or find a specialisation not in AW-IV's inventory.

AW-III's W4 viability profile surfaces the data. AW-III's W5 activates
the minimum-viable specialisation. AW-III's W6 reads the verdict.

## Invariants

1. **Every `#[ignore]` at AW-II close is audited and dispositioned**.
   Each existing ignored test resolves as one of: (a) **CLOSE** —
   test is valid, ignore lifted, passes under current state;
   (b) **DELETE** — test is stale, removed with commit-message
   rationale; (c) **INVESTIGATE-then-resolve** — root cause fixed,
   ignore lifted. Under no circumstance does AW-III close with an
   ignored-count > 0 that hasn't been dispositioned in this tranche.
2. **No new `#[ignore]` added in this tranche**. Inherited edict from
   the operational protocol — strengthened here into a hard gate.
3. **Producer-side surfaces within scope**. Walker, lifter, emitter,
   IR passes, all editable. AW-I/II froze them under the "consumer
   migration" thesis; AW-III's thesis is "close every gap, wherever
   it lives."
4. **One path**. Inherited from AW-I. No dual-path builds, no
   feature-flagged fallbacks, no "legacy mode" shims.
5. **Viability is measured, not asserted**. W4's samply attribution
   sidecar on the worst-regression entry (json_twitter @ 16×) is the
   truth anchor. No claim of "DTA viable" without a profile to cite.
6. **Bootstrap idempotent at every wave boundary** — inherited.

## Wave schedule

| Wave | Scope | Agents | Workspace at close |
|------|-------|--------|--------------------|
| W1 | DTA payload wiring — `DtaState::Regex`/`Literal` carry `PayloadKind`; lifter threads from `IrNode::Map` context; walker consumes; Seq→KvPair promotion; bootstrap regen. Cluster C target (37 tests). | 1 serial (producer-deep) | green or nearly so — Cluster C drops from 37 to < 5 residual |
| W2 | DTA parse completeness — offset-0 EBNF + CSS truncation + JSON large-file parse failures. Cluster A target (13 tests). | 1 serial | green — Cluster A closes |
| W3 | Ignored-test audit + close — all 67 existing `#[ignore]` dispositioned. | 2 parallel (by grammar family) | green — ignored count ≤ 10, every remaining ignore has in-file rationale |
| W4 | Viability profile — samply attribution on json_twitter, sheets_parse_stress, bbnf_ebnf. Decision document. | 1 serial | decision landed (viable / not-viable / conditional) |
| W5 | Minimum-viable specialisation — activate the smallest coherent AW-IV lever set the W4 profile implicates. | 2–3 parallel | bench matrix within 2× of post-AU (or escalation to user) |
| W6 | FINAL + full 19-entry bench matrix + close | 1 serial (orchestrator) | `post-AW-III.json` exists; `FINAL.md` exists; workspace 0 failed / 0 ignored |

## Phases

### W1 — DTA payload wiring (Cluster C close)

Owner: `crates/bbnf-tape/src/dta.rs` (wire contract), `crates/ir/src/passes/recognizers/dta.rs` (lifter), `crates/bbnf-tape/src/driver.rs` (walker), `crates/core/src/backend/rust/emitter/dta.rs` (emitter), `crates/ir/src/passes/materialization/**` (IR materialisation).

The W5c diagnostic named the architectural gap concretely: the DTA
lifter strips `IrNode::Map { inner, .. }` wholesale
(`crates/ir/src/passes/recognizers/dta.rs:525`); the walker's
`DtaState::Regex` arm hardcodes `PayloadKind::F64` for every regex
match (`crates/bbnf-tape/src/driver.rs:912`); `DtaState::Literal`
arms never emit payload at all. Consequence: every `-> Span` / `-> u32`
/ `-> Bool` annotation drops before reaching the tape.

Fix:

1. Extend `DtaState::Regex` and `DtaState::Literal` with a `payload: PayloadKind` field (IR side + wire contract side).
2. Lifter reads the enclosing `IrNode::Map`'s FnDescriptor → resolves to `PayloadKind` → threads into `DtaState::Regex`/`Literal` construction. Alt branches inherit per-branch payload from their FnDescriptor.
3. Walker consumes `state.payload` and emits the correct payload bytes (replacing hardcoded F64; activating Literal payload writes).
4. Seq → KvPair promotion: `frame_to_tape_kind` promotes a Seq compound to KvPair when the enclosing rule's layout is `KvPair`.
5. Bootstrap regen under the extended schema. Verify idempotent.

Hard gate: `cargo test --workspace --no-fail-fast` Cluster C count drops from 37 → ≤ 5. Any residual ≤ 5 must have a named upstream root cause (grammar-level, not lifter/walker).

### W2 — DTA parse completeness (Cluster A close)

Owner: diagnose per-test; fix likely spans `crates/bbnf-tape/src/driver.rs`, `crates/ir/src/passes/recognizers/dta.rs`, `crates/core/src/lower/**`.

Cluster A at AW-II close (13 failures):
- `ebnf_{minimal,recursive_list,expr_grammar}_tape_parity` (3) — offset-0 parse failure despite AW-II.W5b's Minus + double-Repeat fixes. Additional upstream issue in the ebnf lifting pipeline.
- `ebnf_root_has_at_least_one_rule` (1)
- `ebnf_prettify::parse_{single,multi}_rule` (2)
- `css_{normalize,bootstrap,tailwind}_tape_parity` (3) — `bootstrap` emits 9 records for 92228-byte file (truncation); `tailwind` parses up to offset 3633741 then fails.
- `json_{canada,data}_tape_parity` + `parse_{canada,data}_json` (4) — large-file parse failures.

Diagnosis sequence per test: minimal reproducer → trace → root cause → fix → regression test. Large-file failures (canada, data, tailwind) probably hit walker state limits (counter index overflow, frame-stack depth, PSI reserve size) — check the bounds in `crates/bbnf-tape/src/{driver,dta}.rs`.

Hard gate: Cluster A closed; `parse_{canada,data}_json` and `css_tailwind_tape_parity` succeed; the 5 AW-II-blocked bench entries become measurable.

### W3 — Ignored-test audit + close

Owner: two parallel agents (audit by grammar family):
- **W3.A** — `json`, `css`, `bbnf` ignored tests.
- **W3.B** — `sheets`, `ebnf`, `structural`, misc ignored tests.

Each ignored test gets one of three dispositions:

1. **CLOSE**: test is valid and now passes under HEAD state. Lift the `#[ignore]`. Commit: `test(<area>): close AW-III-lifted ignore — <test_name>`.
2. **DELETE**: test is stale (tests a behaviour that's now architecturally different, or a behaviour that was removed). Delete the test with commit-message rationale. Commit: `test(<area>): delete stale ignore — <test_name> (rationale: ...)`.
3. **INVESTIGATE-then-close**: root cause is real and tractable; fix the root cause in-wave, lift the `#[ignore]`, commit both.

Artefact: `docs/tranches/AW/audit/ignore-audit.md` — table of every ignored test with disposition + commit hash.

Hard gate: ignored count ≤ 10 at wave close (ideally 0). Any remaining ignored test must have (a) an in-file comment with named rationale, (b) a tracking doc entry, (c) explicit orchestrator approval.

### W4 — Viability profile

Owner: serial orchestrator + samply.

Measurements (cold, per `docs/instructions/PROFILING.md`):
1. `json_twitter` bench with samply attribution. Expected: state-dispatch hotspot, psi write hotspot, frame_depth update hotspot.
2. `sheets_parse_stress` bench with samply. Expected: similar dispatch hotspot, perhaps more grammar-specific (e.g. Alt branch enumeration).
3. `bbnf_ebnf` bench with samply. Expected: dispatch + recovery-path hotspots if any.

Decision document: `docs/tranches/AW/audit/viability-profile.md`. Contents:
- Per-benchmark hot functions by self-time.
- Attributed cost: % dispatch, % PSI, % frame, % Alt-branch enumeration, % Ref-chase, % other.
- Modelled best-case speedup from each AW-IV lever against the attribution. Table of lever × benchmark → expected MB/s lift.
- Binary decision: viable / viable-with-levers / not-viable.

Hard gate: decision document exists; "viable-with-levers" decision identifies the specific AW-IV levers → W5 activation.

If "not-viable": STOP. Escalate to user with the decision document. Options (user decision, not orchestrator decision):
- Revert AW-I + AW-II (undoing DTA-primary parse path).
- Accept regression as documented correctness tradeoff.
- Research alternative specialisation approach (not in AW-IV's inventory).

### W5 — Minimum-viable specialisation

Owner: 2–3 parallel agents; scope defined by W4's viability document.

Typical activations (informed by viability profile):
- **PSI rayon stage-B** — parallel slab-buffer writes for large inputs. Biggest win on data_xl, canada.
- **ShapeRef runtime dispatch** — collapses same-shape rules into const-hash lookup. Biggest win on CSS declaration compounds.
- **PHF + SIMD keyword dispatch** — replaces walker's AltLinear for keyword sets. Biggest win on CSS / BBNF.

Other levers (from old AW-III plan, now AW-IV):
- selector classifier, scanner closure, document-parallel fork, bloom+GADT dedup, Pratt generalisation, reduce_column+SIMD-pack.

W5 activates ONLY the subset W4's profile implicates for viability. The rest ship in AW-IV as part of the optimisation arc.

Hard gate: `cargo bench` on the 14 measured entries shows geomean within 2× of post-AU. If not met, escalate.

### W6 — FINAL + full bench matrix + close

Orchestrator serial.

1. Full workspace test: 0 failed, ≤ 10 ignored (ideally 0).
2. Full 19-entry bench matrix (all 5 AW-II-blocked entries now measurable).
3. `docs/benchmarks/post-AW-III.json` — bench-checkpoint sidecar.
4. `docs/tranches/AW/FINAL-III.md` — close document with hard-gate attribution.
5. Update `docs/benchmarks/post-AW.json` multi-wave history with AW-III close entry.
6. Update `docs/tranches/AW/FINAL-I.md` + `FINAL-II.md` successor chains to reference AW-IV as the optimisation successor.

Hard gate: green workspace; bench matrix within viability envelope or escalated to user; FINAL authored.

## Cross-tranche debt inherited from AW-II

| Item | Origin | AW-III wave |
|------|--------|-------------|
| Cluster A (13 parse failures) | AW-II.W5c residuals | W2 |
| Cluster C (37 payload activation) | AW-II.W5c residuals; root cause diagnosed | W1 |
| Cluster D (1 integration: test_large_grammar) | AW-II.W5c residuals | W2 or W3 |
| 67 ignored tests | accumulated across AW-series | W3 |
| 5 blocked bench entries (data_s, canada, tailwind) | AW-II.W5 bench matrix | W6 after W2 closes parse failures |
| CSS L4 state_count plan-miscalibration documentation | AW-II.W5.11 | orchestrator note in FINAL-III; no corrective work needed |
| `serialize_roundtrip::css_simple` ignore | AW-I.W2.5 carry | W3 (audit + close or delete) |

## Cross-tranche debt deferred to AW-IV

| Item | Origin | AW-IV wave |
|------|--------|-------------|
| Full AW-IV lever activation (all of PSI rayon, ShapeRef, PHF+SIMD, selector classifier, scanner PaddedView, document-parallel, bloom+GADT, Pratt generalisation, reduce_column+SIMD-pack, parity harnesses) | AV substrate; AW-III activates minimum-viable subset only | AW-IV W1–W5 |
| Full bench parity to match-or-beat post-AU | AW-III targets within 2×; AW-IV closes the gap | AW-IV W5–W6 |

## Operational posture

Inherits `docs/instructions/README.md` + `docs/instructions/TRANCHE_SPEC.md` in full.

Specific notes:

- **Producer-side surfaces in-scope at all waves**. AW-I/II framed
  walker/lifter/emitter as "frozen"; AW-III's thesis rejects that framing
  — the remaining gaps are producer-side, and closing them is the wave.
- **`#[ignore]` discipline**: audit-then-close. Never add a new ignore,
  never leave an existing one un-dispositioned.
- **Bootstrap regen permitted at any wave boundary**. Idempotency
  verified at every regen. W1 + W5 likely candidates; orchestrator
  signs off on each.
- **Escape clause**: declared at plan time for the "not-viable" W4
  outcome. If viability fails, AW-III ships FINAL-III with the
  escalation and the user decides the next step. The tranche does not
  silently-defer by opening yet another letter.
- **Profiling discipline**: every performance claim in W4/W5/W6 cites
  a samply profile per `docs/instructions/PROFILING.md`. No speculative
  throughput numbers.

## Research artefacts

AW-III opens no research wave. AW-II's audits (`find-child-audit.md`,
`w4-scope-reveal.md`, W5c's diagnostic commit `d635086f` payload trace)
supply the diagnostic context. W4's viability profile IS the research
artefact for the optimisation decision.

## Successor chain

AW-III closes green → AW-IV opens (full optimisation arc, the plan
formerly named AW-III, now at `docs/tranches/AW/AW-IV.md`).

AW-III escalates non-viable → user decision; AW-IV's premise revisited.

Indefatigable. No deferrals. No stubs. No shims. No new `#[ignore]`.
Viability measured, not asserted.
