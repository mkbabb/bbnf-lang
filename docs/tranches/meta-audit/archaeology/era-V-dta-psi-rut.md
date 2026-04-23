# Era V — The DTA/PSI/Activation Rut (2026-04-15 → 2026-04-19)

Era V is the hard era. In five calendar days, seven tranche surfaces
(AV, AW-I, AW-II, AW-III, AW-IV, AW-V, AX) ship ~600 tranche-tagged
commits. At Era V's close, every bench entry is *below* the AU-baseline
— JSON twitter 486 MB/s (24.7% of AU), CSS / Sheets / BBNF 3–7% of AU —
despite a 400-commit substrate build. Era V's signature failure mode
is **substrate-first-consumer-later**: every tranche ships the
compile-time emission of constants, tables, and shape dictionaries; no
tranche fully activates the runtime consumer that reads them.

Commit ledger: AV 53, AW-I ~45, AW-II 40, AW-III 93, AW-IV 92, AW-V 80,
AX 169. Total: ~572 tranche-tagged commits. Including AW-carry-items
from worktrees and planning commits, the arc is the "1000-commit
near-implementation of the fault DTA/PSI interpreter" the user framed.

## Architectural thesis (as declared at AV plan-time)

Per `docs/tranches/AV/AV.md` (`ca0875eb`, 2026-04-15):

> AV is the semantic-parity tranche. It closes AU's typed-
> materialisation debt, ships the dispatch automaton + PSI pipeline +
> columnar substrate as one coherent architecture, and drives the
> sonic-rs and lightningcss parity gates that AU planned but did not
> reach. This is the tranche that earns the creed: *every `->`
> annotation in every grammar reaches the tape; every typed AST bbnf
> emits matches its lightningcss or sonic-rs counterpart node-for-
> node; no fallbacks, no legacy paths, no workarounds*.

The substrate pieces AV + AW set out to ship, per plan doc:

1. **DTA (Dispatch Table Automaton)** — a grammar-derived table-driven
   parser avoiding the recursive `fn __<rule>` descent.
2. **PSI (Parallel Structural Index)** — a pre-computed index allowing
   document-level parallel parse.
3. **Columnar tape** (Era IV's columns made first-class).
4. **ShapeRef** — compile-time shape dictionary dispatched at the
   cursor.
5. **PHF + SIMD keyword classifiers** — compile-time perfect-hash
   keyword tables.
6. **Bloom + GADT runtime dedup** — shared-substring dedup.
7. **Shape emitter** — the unifying substrate. Auto-derives the
   sonic-rs-class inner loop from any BBNF grammar.

Each of the seven is shipped. None reach break-even parse throughput
with the AU baseline before Era V ends.

## Sub-tranche breakdown

### AV — The Flattening (2026-04-15 → 2026-04-16, 53 commits)

Plan: `ca0875eb`. FINAL: `be4b22b1` (`docs(AV): TRANCHE CLOSED — V5
landed, V6-V9 routed to AW, FINAL.md + post-AV.json`).

What landed (per `docs/tranches/AV/FINAL.md`):

- **V0 typed-materialisation completion.** AU Bug 1, Bug 2, Bug 2b
  closed. CO-E1 through CO-E5 close-out waves handled emitter-consumer
  wiring, padded-input cascade, Sheets InlineBody driver threading,
  outer alt checkpoint extension, scalar-Alt layout admission, and
  triaged 26 pre-existing workspace failures into Categories A/B/C.
- **V1 — `GrammarProfile` const emitted** into every grammar's
  `generated.rs`. 17 fields covering push counts, per-byte densities,
  parallel break-even bytes, structural alphabet, active columns, list
  rules, keyword tables, shape dict, branch priors, dedup-eligible
  rules, reorder-unroll visitors.
- **V2-V5 substrate** — `LargeAggregate` variant, colour grammar,
  DTA table scaffolding, PSI pipeline, ShapeRef cursor expansion.

What routed forward to AW (the "scope cut"):

- **V6** document-level parallel parse.
- **V7** SIMD keyword dispatch + PHF + selector classifier.
- **V8** runtime bloom + GADT dedup.
- **V9** walker + reader migration closure.
- **V10** tranche-close.

AV's close honest statement: "The substrate landed; the activation
sits one cherry-pick behind, in AW's opening wave." This is the
earliest explicit recognition of the substrate-first-consumer-later
anti-pattern — but it is stated as a *scope cut*, not a *warning*.

Salvageable from AV: V0 close-out permanently fixed AU Bug 1/2/2b.
`LargeAggregate` payload path. `GrammarProfile` const channel
(though 7 of 17 fields turn out dead and are carved at AX.W0b).
`PaddedView` cascade in 4 SIMD scanner kernels.

### AW-I — Walker completion + `parse()` swap + MemoStore retirement (2026-04-16)

Plan: `d174af30` + `AW-I.md`. FINAL: `docs/tranches/AW/FINAL-I.md`
(286 lines).

Scope: delete the AU-era `fn __<rule>` legacy, swap `parse()` onto the
DTA walker, retire `MemoStore`, activate fuse/inline in the shape
dispatcher.

Outcome: workspace green; performance regression across the matrix.
The DTA walker carries structural overhead the `fn __<rule>` path did
not; the overhead was supposed to be paid back by later AW-II/III/IV
levers that did not materialise on schedule.

### AW-II — DTA self-host round-trip (2026-04-16, 40 commits)

Plan: `AW-II.md`. FINAL: `FINAL-III.md` (795 lines — AW-II and AW-III
share a combined FINAL in the record).

Scope per plan-doc preamble:

> migrate the lowering pipeline off fn-per-rule tape-shape assumptions
> onto DTA's Seq-wrapped structural layer, restore workspace-green,
> and publish a 19-entry parse-bench matrix.

Waves W1–W5 migrated every consumer the plan named; W5b added
producer-side fold-ins the scope-reveal surfaced (Minus + double-
Repeat); W5c added universal named-type projection + `join_types`
recursive unwrap; W5.7 measured the 14 parse-passing bench entries.

Three architectural surfaces remained un-migrated under AW-II's
*consumer-only* invariant — each with root cause diagnosed and named
destination, each absorbed into AW-III. AW-II closes honestly on what
landed: workspace 11-pass-higher / 17-fail-lower than its baseline,
with the *viability question raised for the successor tranche*.

This "viability question raised" is the first hard signal that the
DTA arc is not recovering the AU baseline. It is written into a
FINAL and then routed forward for another 200+ commits.

### AW-III — Fused correctness + architectural transposition (2026-04-17, 93 commits)

Plan: `AW-III.md`. Scope:

- Cluster A/C/D residuals close.
- 67 `#[ignore]` audit.
- Three general emitter passes to flatten the DTA interpreter into a
  per-grammar specialised tape automaton:
  - Walker-specialisation pass.
  - Stage-1 SIMD structural-bitmap pass.
  - Fused SoA write API.
- Activation of the five emitter-mined consumers (ShapeRef, PHF,
  ClassifyByte, direct-to-struct, Pratt const-fold).

Hard gate declared: **strict-better-than post-AU on ≥ 15/19 entries**.

Outcome: substrate landed on all five consumers. Zero entries met the
hard gate at AW-III close.

### AW-IV — Granular exceed + parity harnesses (2026-04-17, 92 commits)

Plan: `AW-IV.md` (979 lines). Scope:

- AVX2 widening.
- Scanner `PaddedView` migration + cluster consolidation.
- NEON 17-digit float scan.
- Bloom + GADT dedup + grammar-level pattern hoisting.
- Document-parallel fork over the AW-III stage-1 index.
- `reduce_column<C, R>` visitor + 4-lane SIMD pack.
- Cost-model grid sweep.
- sonic-rs + lightningcss parity harnesses.
- AU walker/reader migration carry-overs.

Hard gate: **every entry exceeds post-AU; parity harnesses CI-gated**.

Outcome: substrate landed. No entry met the gate. 92 commits of
compile-time emission with runtime consumers partially wired. The
hard gate was "every entry exceeds post-AU" and the actual close was
"0 entries exceed post-AU, 17/17 regressed."

### AW-V — The final activation attempt (2026-04-17 → 2026-04-19, 80 commits)

Plan: `AW-V.md` + `AW-V-W2-close.md`. Scope: finalise the activation
that AW-I through AW-IV did not. *Auto-derive the sonic-rs-class inner
loop from any BBNF grammar.*

Per `docs/tranches/AX/AX.md` preamble (`4177a18c`):

> AW-V closed with 0/17 parse entries exceeding post-AU (JSON twitter
> 486 MB/s = 24.7% of baseline; CSS/Sheets/BBNF at 3-7% of baseline).
> Shape-emitter substrate landed for all grammars but only JSON's
> `parse()` routes through it at runtime; CSS/Sheets/BBNF still
> delegate to `__dta_walker_inline::run`. The `has_w4_classified`
> gate at `crates/core/src/backend/rust/emitter/grammar.rs:718` over-
> restricts JSON's visitor-path. AW-V's thesis — "auto-derive the
> sonic-rs-class inner loop from any BBNF grammar" — was demonstrated
> exactly once, on JSON, at W3 close (commit `c1e86ab3`), and lost by
> W6.

**AW-V demonstrated the thesis and lost it within its own tranche.**
The "exactly once" at W3 is the peak of the Era V arc.

### AX — The RD Reckoning (2026-04-16 → 2026-04-20, 169 commits)

Plan: `4177a18c`. FINAL: `c590bcc2`. 21 invariants declared. Six
architectural propositions:

1. The regression must be repaired before the interpreter deletes.
2. The interpreter is architectural debt — ~78,500 LOC reclaim target.
3. The tape's access API shapes the ceiling more than the tape's
   storage layout does.
4. Novel levers compound only when they share a substrate AND a
   demonstrable floor.
5. Parallelism is an amortisation multiplier over single-thread
   exceed, not a single-thread lever.
6. Parity IS the generality claim. No hand-tuned per-grammar
   prototypes.

Execution: W0a (gate repair + non-Alt-rooted `parse()` routing + wire
contract) + W0a.close + W0b (interpreter deletion) + W0c (AW-V
rewrite) + W1r (view-layer + parity harnesses, seven sub-waves).

**AX.W0b is the most important code-removal commit set of Era V.**
Per `docs/tranches/AX/FINAL.md`:

- `bc550d2c` `feat(emitter): retire walker path + gate predicates,
  regen (AX.W0b.A)`.
- `a206b962` `refactor(emitter): delete dta_walker/ + emitter/dta.rs
  (AX.W0b.A)`.
- `b7aa41c0` `refactor(tape,ir): surgical carves + 7 dead profile
  slots + Lever 4 (AX.W0b.A)`.
- `e4121fdc` `chore(simd-scan): purge dead emit/ directory (AX.W0b.B)`.
- `b464a99c`, `1327491e`, `6ad76124` — rename `bbnf-tape`,
  `bbnf-simd-scan`, `bbnf-json-prototype` crates, drop `bbnf-` prefix.
- `e839378c` delete 8 DTA-coupled test suites.
- `0d730c8f` retire `tape_parity_*` walker oracles per invariant 20.
- `6854f18b` delete W0a diagnostic probe harnesses.
- `0adabb23` delete DTA-walker regression tests + carve dead profile
  fields — AX.W0b cleanup close.

W0b removes the interpreter. W1r lands the view surface + canonical-
form parity against sonic-rs and lightningcss. W2–W14 (the
optimisation arc) do **not** execute under AX's letter; they route
wholesale into AY.

## What landed durably from Era V

- **AU Bug 1/2/2b closure** (AV.0.x) — typed materialisation of
  alt-lit payloads + `-> Span` + `-> i64` / `-> f64` scanner
  threading.
- **`GrammarProfile` const channel** — 10 of 17 fields permanent.
- **`LargeAggregate` payload variant** — colour grammar + tuple-shaped
  declarations reach the tape.
- **Shape emitter (JSON only)** — the one-grammar demonstration at
  W3 close (`c1e86ab3`).
- **`has_w4_classified` gate predicate + its narrowing** — wire
  contract test matrix (`69d28f56` `gate_predicate_wire_contract.rs`)
  freezes the 7×3 = 21-assertion surface.
- **Interpreter deletion** (AX.W0b cluster, 2026-04-20). ~78K LOC
  reclaim.
- **Crate renames** — `bbnf-tape` → `tape`, etc. — because the
  `bbnf-` prefix signified public-API, which those crates are not.
- **Canonical-form parity harnesses** — sonic-rs, lightningcss,
  simdjson OnDemand, serde_json, cssparser. `tests/*_parity.rs`.
- **Invariant discipline.** AX's 21 invariants establish the "bench-
  checkpoint mid-wave", "wire-contract compile-gate", "ledger-review
  at handoff", "frozen-contract rule for gate predicates" protocols
  that Era VI inherits.

## What was reverted or superseded

- **DTA interpreter** (`crates/tape/src/dta.rs` + `dta_walker/`) —
  deleted at AX.W0b. ~550 LOC carved to 80 at AY.W0.3 (`fdbc43a3`).
- **`__dta_walker_inline::run`** runtime fallback — zero call sites
  after AX.W0b.
- **`tape_parity_*.rs`** walker oracles — retired at AX.W0b per
  invariant 20 (shape-emitter is the single source of truth).
- **7 dead `GrammarProfile` slots** — `list_rules`, `shape_dict`,
  `push_*_count`, etc. — `b7aa41c0`.
- **`crates/tape/src/shape_dict.rs`** — `BBNF_SHAPE_DICT` never
  emitted — deleted at AY.W0.
- **Lever 4 (specific scan lever)** — dead; removed at AX.W0b.A.
- **8 DTA-coupled test suites** (`e839378c`).
- **`has_w4_classified` gate predicate** — narrowed to `Pratt |
  Unordered` at W0a.1 (`9f8aed90`); later deleted per AX invariant.
- **AW-V's "auto-derive the sonic-rs-class inner loop"** — thesis
  abandoned post-AW-V; AX.W1r absorbs the reality into a view-layer
  + canonical-parity surface that does not require the full auto-
  derive.
- **Hand-coded `bbnf::json::Value` / `bbnf::css::StyleSheet`** —
  briefly landed in AX.W1.A/W1.B; deleted at W1r.0 (`3429aaba`)
  `Revert W1.A/W1.B (−6,128 LOC); sonic-rs runtime → dev-dep`.

## Salvageable artefacts (present at 2026-04-22)

- **The view layer** — `crates/core/src/backend/rust/view/` —
  `NodeView<'p>`, `TapeCursor<'p>`, per-rule typed accessors. IR-
  derived named-type resolver replaces static `BINDINGS` (AX.W1r.1,
  `ax-w1r-1` branch).
- **Canonical-form parity harnesses** — `tests/*_parity.rs` against
  sonic-rs, lightningcss, simdjson, serde_json, cssparser.
- **`gate_predicate_wire_contract.rs`** — the 21-assertion freeze
  surface.
- **Shape emitter substrate** (`crates/core/src/backend/rust/emitter/
  shapes/`) — `inline.rs`, `flat.rs`, `wrap.rs`, `arg_list.rs`,
  `hregex.rs`.
- **Pratt LUT propagation + arena-frame API** (`64d6ab2f..7d2fa1b8`).
- **Post-AX-W1-close bench matrix** (`docs/benchmarks/post-AX-W1-
  close.json`).
- **The AW / AW-II / AW-III / AW-IV / AW-V / AX FINAL documents** —
  each is a self-contained retrospective the current archaeology
  cites directly.

## What the arc cost

Roughly 600 tranche-tagged commits (AV 53 + AW 45 + AW-II 40 + AW-III
93 + AW-IV 92 + AW-V 80 + AX 169 = 572) across five calendar days.
Sustained substrate-first build, per-tranche FINAL writing,
orchestrator-driven wave dispatch, between-wave bench checkpoints (in
AX; AW and earlier were ledger-only on many waves per AX invariant
13's later prohibition).

The single hardest lesson:

> "Novel levers compound only when they share a substrate AND a
> demonstrable floor. V's substrate-first-consumer-later anti-pattern
> must not recur." — AX.md proposition 4.

## Transition into Era VI

AX closes at HEAD `411eabfd`. AY opens with planning branches
(`ay-a10-value-refinement`, `ay-a7-ax-w2-w15-absorption`, `ay-a9-
structural-pratt`, `ay-w4-regex`). AY-I executes W0–W7; parity close
gates not met; W7 superseded. AY-II opens as the "gestalt re-ordered
remainder". B0 + B1 are the bounded prelude annexes that restore
truthful development infrastructure before AY-II can resume. The full
Era VI is covered in `era-VI-restart.md`.

The decision that opened Era VI is visible in the triumvirate of
W0a's gate repair + W0b's interpreter deletion + W0c's AW-V-rewrite.
Once the interpreter is gone and the view surface is canonical, the
next horizon is *what beats sonic-rs*, not *what activates DTA*.
Era VI is the answer to that question.
