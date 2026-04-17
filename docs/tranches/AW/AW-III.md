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

## Wave schedule (refined 2026-04-17 from SYNTHESIS.md)

| Wave | Scope | Agents | Workspace at close |
|------|-------|--------|--------------------|
| W1 | **Six-point DTA payload wiring** + **two structural levers (Pratt `Next` peel + Scanner closure)** | 1 serial (producer-deep) | Cluster 1 + Groups A-ignores close; 47 tests pass-flip |
| W2 | **Parse completeness** — single EOF/trailing-ws fix closes 4-5 tests; EBNF offset-0; CSV | 1 serial | workspace 0-failed or near-zero |
| W3 | **Ignored audit + close** — 14 CLOSE lifted, 4 DELETE removed, Group A/B cascade from W1/W2, Groups C/D/E/F/G routed-or-closed per relaxed gate | 2 parallel | ignored count ≤ 27 (or ≤ 10 if scope expanded); every remaining has in-file rationale or successor-tranche reference |
| W4 | **Viability profile** — samply on json_twitter, sheets_parse_stress, bbnf_ebnf. Decision document | 1 serial | viable / not-viable / conditional landed (pre-staged by SYNTHESIS.md) |
| W5 | **Minimum-viable specialisation** — ShapeRef consumer + PHF keyword + fused push_compound + selector classifier (CSS) + PSI rayon calibration | 3 parallel | post-AW-III.json within 2× of post-AU on 18/19 entries; Sheets documented tradeoff |
| W6 | FINAL + full 19-entry bench matrix + close | 1 serial (orchestrator) | `post-AW-III.json` exists; `FINAL.md` exists; green workspace |

## Phases

### W1 — DTA payload wiring + structural levers (refined 2026-04-17)

Owner: `crates/bbnf-tape/src/{dta,driver}.rs`, `crates/ir/src/passes/recognizers/dta.rs`, `crates/core/src/backend/rust/emitter/dta.rs`, `crates/ir/src/passes/materialization/**`.

**SYNTHESIS.md refined W1 from a 6-point payload-only wave into an 8-point
wave** that also closes two structural one-peel/one-field levers whose absence
blocks Pratt firing on CSS + forces HashMap-lookup-per-scan everywhere. Both
added fixes are < 100 LOC each and unlock W5 lever work that would otherwise
be blocked on them.

**Six payload-wiring points** (Cluster 1 target — 37 tests):

1. Extend `DtaState::Regex` + `DtaState::Literal` with `payload: PayloadKind` field (IR + wire contract).
2. Lifter reads enclosing `IrNode::Map`'s FnDescriptor → resolves to `PayloadKind` → threads into `DtaState::Regex`/`Literal` construction. Alt branches inherit per-branch payload from their FnDescriptor. Fixes A1's Hole #1 (`dta.rs:525` wholesale strip).
3. Walker consumes `state.payload` and emits correct payload bytes — replaces hardcoded `PayloadKind::F64` at `driver.rs:912` (Hole #3); activates Literal payload writes at `driver.rs:875-891` (Hole #4).
4. Emitter const-folds payload writes into the generated DTA table.
5. Bootstrap regen under the extended schema. Verify idempotent.
6. `frame_to_tape_kind` promotes Seq → KvPair when the enclosing rule's layout is `KvPair` (Hole #5).

**Two structural one-fix levers** (surface per SYNTHESIS.md):

7. **Pratt `IrNode::Next` peel** — extend `strip_transparent_owned` at `crates/ir/src/passes/recognizers/dta.rs:885-890` to peel `IrNode::Next(a, b)` alongside `IrNode::Seq`. Unblocks `match_operator_chain_rule` on CSS `calc()` / `min()` / `max()` / `clamp()` — every grammar using `>>` as operator separator. CSS L4's mathExpr + mathProduct Pratt-lift immediately; state count drops; walker dispatch depth drops.
8. **Scanner closure** — add `pattern_dfa: Arc<Dfa>` field to `DtaState::Regex`; populate at lift time from the compile-time pattern constant. Walker `dispatch_one` Regex arm uses the pre-bound `Arc<Dfa>` directly — no global HashMap lookup, no SipHash, no `Arc::clone` on the hot path. Eliminates 6-33% self-time depending on grammar per P1/P2/P4/P5 attribution.

Hard gate: `cargo test --workspace --no-fail-fast` Cluster 1 count drops from 37 → ≤ 5. Scanner closure verifiable via samply comparison: `cached_dfa` / `HashMap::get` drops out of top-20. Pratt peel verifiable via summarise call on CSS L4 DTA — new ShuntingYard state count > 0.

### W2 — Parse completeness (refined with P1/C1 EOF insight)

Owner: diagnose per-test; fix likely spans `crates/bbnf-tape/src/driver.rs`, `crates/ir/src/passes/recognizers/dta.rs`, `crates/core/src/lower/**`.

**SYNTHESIS.md's load-bearing refinement**: P1 found `json/data` fails at
offset 35490 of 35491 bytes; `json/canada` fails at 2251050 of 2251051 —
**both one byte from EOF**. Paired with `css_tailwind` offset 3633741 (near
end of 3,749,612-byte input) and `css_bootstrap` / `css_normalize`
truncation, this is **one shared EOF / trailing-whitespace handling gap**
rather than 4-5 separate large-file bugs. Primary W2 sub-wave is therefore
a single-fix EOF handling closure.

**Clusters** (13 failures + 1 CSV escalation):

- **Cluster 2 (shared EOF)**: `json_data`, `json_canada`, `parse_data_json`, `parse_canada_json`, `css_tailwind`, `css_bootstrap` truncation, `css_normalize` truncation (7 tests). Walker EOF / trailing-whitespace handling. **Single fix.**
- **Cluster 3 (EBNF offset-0)**: `ebnf_minimal`, `ebnf_recursive_list`, `ebnf_expr_grammar`, `ebnf_root_has_at_least_one_rule`, `ebnf_prettify::parse_{single,multi}_rule` (6 tests). Every EBNF grammar fails at `Syntax { offset: 0, rule: None }`, including `digit = "0" | "1" ;`. AW-II.W5b's Minus + double-Repeat were necessary but insufficient; remaining upstream gap in `@ws` or first-literal dispatch for EBNF.
- **Cluster 5 (CSV Repeat-of-Seq)**: `csv_multi` (1 test) — `csv = record, ( /\r?\n/ >> record ) *` Repeat walker regression at the record-separator boundary.

Hard gate: Cluster 2 + 3 + 5 closed; all 5 AW-II-blocked bench entries measurable (`data`, `canada`, `tailwind`).

### W3 — Ignored-test audit + close (refined gate)

Owner: two parallel agents (C2 audit already produced `ignores-audit.md`).

**Pre-staged dispositions from AW-III.C2** (58 unique source ignores):
- **CLOSE — 14 tests**: already verified passing when `#[ignore]` lifted (7 in `structural.rs` + 7 in `serialize_roundtrip.rs` including the `css_simple` that AW-I.W2.5 marked Category A). Mechanical attribute lift.
- **DELETE — 4 tests**: 3 `unreachable!()` stubs + 2 gorgeous visualisation dumps (non-checked-in fixtures). Mechanical test-function deletion with rationale.
- **INVESTIGATE — 40 tests** across 7 root-cause groups:
  - A (10): CSS percentage + JSON variant_idx — **cascades from W1 payload wiring**.
  - B (1): `ebnf_rule` serialize — **cascades from W2 EBNF completeness**.
  - C (6): structural-mode analysis pipeline — out of AW scope (analysis-mode rework tranche).
  - D (5): closure-body lowering — grammar-closures project.
  - E (6): CSP solver GAC alldiff — csc411 solver tranche.
  - F (4): gorgeous prettify multi-rule + pprint-vm hint-semantics drift.
  - G (7): miscellaneous producer-side + test-data.

**Gate refinement** (from SYNTHESIS.md):
- CLOSE (14) + DELETE (4) + Group A cascade (10) + Group B cascade (1) = **29 tests dispositioned at W3 close**.
- Residual: **~27 tests** in Groups C/D/E/F/G.
- The plan's original "ignored ≤ 10" gate is infeasible without expanding scope into Groups C/F/G. Revised gate: **every remaining ignored test has either (a) an in-file comment naming the successor tranche, or (b) a row in `docs/tranches/AW/audit/ignore-routing.md` declaring its destination**.

Artefact inherits `docs/tranches/AW/research/ignores-audit.md` from C2; W3 produces `docs/tranches/AW/audit/ignore-routing.md` with successor-tranche mappings.

Hard gate: CLOSE batch lifted + DELETE batch removed + routing document exists covering every remaining ignored test. No ignored test without either in-file rationale or routing entry.

### W4 — Viability profile (pre-staged by SYNTHESIS.md)

Owner: serial orchestrator + samply (re-measurement post-W1/W2 to confirm or refute SYNTHESIS.md's pre-stage).

**SYNTHESIS.md pre-stage** (from 8-agent research at HEAD `3c33bc35`):
- **JSON / CSS / BBNF — VIABLE within 2-5× of post-AU**. Scanner closure + ShapeRef + fused push_compound recover the majority. Bench parity practical after W5.
- **Sheets — NOT VIABLE within 2×**. Modelled ceiling 8-9× residual with full W5 scope. `dispatch_one` tagged-union floor isn't addressed by any AW-IV lever.

**W4 action**: after W1 + W2 close, re-run samply on `json_twitter`, `sheets_parse_stress`, `bbnf_ebnf`. Compare attribution shifts against SYNTHESIS.md's pre-stage. Commit `docs/tranches/AW/audit/viability-profile.md` as the authoritative decision document.

**If pre-stage holds** (W4 post-W1/W2 numbers match SYNTHESIS.md predictions):
- Green-light W5 activation.
- Sheets escalation for user: accept 8-9× as DTA-architecture tradeoff, OR open new tranche for codegen-specialised per-grammar walkers.

**If pre-stage fails** (W1/W2 closures unlock better-than-expected amortisation, or uncover new bottlenecks):
- W4 document re-draws the viability envelope.
- W5 scope expands/contracts accordingly.

Hard gate: decision document exists citing post-W1/W2 samply numbers; user escalation on sheets documented with explicit options.

### W5 — Structural specialisation (expanded 2026-04-17 — folds in key AW-IV items)

Owner: 3 parallel agents; scope expanded per the "structural, not granular" fold-in directive.

**Primary structural levers** (the 2× envelope reached):
- **ShapeRef runtime dispatch** — walker-side `push_shape_ref` consumer for the already-emitted `SHAPE_DICT` (13 CSS L4 entries + new JSON + new BBNF dicts emitted by this wave).
- **PHF keyword tables** — per-grammar emitter pass populating `keyword_tables`. CSS (163-branch namedColor + 72-branch keywords + 92-branch properties); BBNF keywords; JSON (`true`/`false`/`null`).
- **Fused `push_compound` write** — replace `reserve_compound`'s 7-vector row with a single struct-write.
- **Selector classifier** — CSS-only. `DtaState::ClassifyByte` LUT for the 5-way `compoundSelector` Alt.
- **PSI rayon stage-B CALIBRATION** (folded in from AW-IV.W1.1) — populate `parallel_break_even_bytes` per grammar. No code; only constants from viability samply on canada/data_xl/bootstrap. Folded in because it's literally a handful of constants — not a granular optimisation but a calibration.

**Structural specialisation (folded in from AW-IV.W3, without which viability is not definitively proven)**:

- **W5.6 Codegen-specialised per-grammar walkers** — emit `dta_run_json`, `dta_run_css`, `dta_run_bbnf`, `dta_run_sheets`, `dta_run_ebnf` with inlined `DtaState` arms. **This closes the `dispatch_one` tagged-union floor (~24% self-time) that SYNTHESIS.md flagged as unaddressable by amortisation levers alone.** Without this in AW-III, the Sheets viability question escalates; with it, AW-III definitively answers "yes, DTA is viable."
- **W5.7 Direct-to-struct expansion** — extend beyond CSS Color (current sole consumer) to JSON `Value` tree + BBNF AST struct. A1 audit confirmed the pattern is a named-type-resolver extension, not new codegen architecture. Structural, not granular.
- **W5.8 Per-grammar Pratt const-fold** — W1.7 landed the `IrNode::Next` peel; W5.8 completes the calibration: per-grammar `PRECEDENCE_LUT` population (CSS 148 operators + BBNF value_expr tower + Sheets arithmetic), const-fold precedence levels into the specialised walker's ShuntingYard arm. Depends on W5.6 specialisation being active; natural extension.

**Kept in AW-IV (genuinely granular — micro-optimisation, arch-specific, or consumer-side)**:
- SIMD u8x32 AVX2 widening (arch-gated x86_64 tuning).
- Scanner PaddedView migration (arch/perf-specific).
- Bloom + GADT dedup (modest benefit; optimisation layer).
- Document-parallel fork (complex, benefit only on large inputs — separate tranche).
- `reduce_column<C,R>` visitor API (consumer-side, not parse hot path).
- SIMD 4-lane pack (micro-optimisation).
- sonic-rs + lightningcss parity harnesses (verification).
- Full PHF frequency-ordering + length-bucket tail (PHF stays in III; the frequency + bucket refinement is IV granular).

Hard gate (revised): `cargo bench` on the full 19-entry matrix (5 AW-II-blocked entries unblocked at W2) shows **geomean within 2× of post-AU on ALL 19 entries** (no Sheets escalation — W5.6 codegen-specialisation closes the floor). AW-III proves DTA viability definitively; AW-IV delivers the exceed-RD surplus.

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

AW-III opened with an 8-agent pre-plan research wave at 2026-04-17 producing:

- `docs/tranches/AW/research/perf-01-json.md` — samply json_monolithic
- `docs/tranches/AW/research/perf-02-css.md` — samply css_l4
- `docs/tranches/AW/research/perf-03-sheets.md` — samply sheets (viability-critical)
- `docs/tranches/AW/research/perf-04-bbnf.md` — samply bbnf_monolithic
- `docs/tranches/AW/research/perf-05-json-value.md` — samply bbnf-vs-sonic twin pair
- `docs/tranches/AW/research/perf-06-code-audit.md` — lever firing + precedence + dead code + direct-to-struct + type inference
- `docs/tranches/AW/research/residuals-triage.md` — 50 failing tests per-test categorisation
- `docs/tranches/AW/research/ignores-audit.md` — 58 ignored tests CLOSE/DELETE/INVESTIGATE
- `docs/tranches/AW/research/SYNTHESIS.md` — cross-artefact synthesis + refined wave schedule

These nine documents pre-stage AW-III's wave schedule above. Inherited AW-II
context: `find-child-audit.md`, `w4-scope-reveal.md`, W5c's diagnostic commit
`acd5d942` payload trace.

## Successor chain

AW-III closes green → AW-IV opens (full optimisation arc, the plan
formerly named AW-III, now at `docs/tranches/AW/AW-IV.md`).

AW-III escalates non-viable → user decision; AW-IV's premise revisited.

Indefatigable. No deferrals. No stubs. No shims. No new `#[ignore]`.
Viability measured, not asserted.
