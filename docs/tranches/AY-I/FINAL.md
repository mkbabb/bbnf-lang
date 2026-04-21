# AY-I — FINAL (Pass I Close)

Pass I of AY closes on what it landed. The parity close gates in
`AY-I.md` §B0 → AY handoff contract are **not** met; pass I's
contribution is the write-time substrate experiment, the direct-to-
struct admission broadening, and the honest diagnostic record
documenting why the experiment did not compose. The successor pass
`docs/tranches/AY-II/AY-II.md` executes the gestalt re-ordered
remainder against the architectural transpositions the audit
triumvirate prescribes.

AY-I tranche HEAD: `321d7418` (post-audit cherry-pick, pre-split).

## Scope recap

AY-I dispatched against master HEAD `6516086f` (AX W1r close) and
executed waves W0 through W6, plus a superseded W7. B0 (the AY
execution runway annex) closed cleanly before W5 opened. Pass I
landed 28 commits across the tranche plus 4 audit-triumvirate
artefacts cherry-picked at relinquish.

### W0 — legacy prune + ebnf + AX FINAL + housekeeping (complete)

Seven stale test files retired; `crates/tape/src/dta.rs` carved 550
→ 80 LOC; `shape_dict.rs` deleted; dead `GrammarProfile` fields
retired; EBNF Minus-in-Keyword-Seq codegen deferred to W2. AX FINAL
captured at `post-AX-W1-close.json` + `AX/FINAL.md`. 1491 workspace
tests passed; bootstrap regen cycle-1 = cycle-2 byte-identical.

### W1 — AU AoS substrate revert + Pratt Option C + structural-scan (complete)

Columns reverted from 7 structural Vecs to 1 `Vec<TapeRec>` + parallel
`sib_skip`. Finaliser stack-buffer scratch. Structural-scan substrate
+ consumer probe landed; eager scan retired post-bench regression.
Pratt Option C: `[LocalOpEntry; 16]` op_stack hoist. Twitter recovered
to 688 MB/s after W1-fix.

### W2 — named preservation + G1-G4 canonicalisation + EBNF reactivation (complete with recorded misses)

Named-preservation repaired; EBNF Minus-in-Keyword-Seq codegen
re-activated; wrap-compound elision consumer landed; record count
dropped 8.77% on twitter (intended 50%). Direct-to-struct admission
did not reach ≥4-entry expectation at W2.

### W3 — value surface + path/query + 12-entry value bench (complete with recorded misses)

`handle.rs`, `path.rs`, `Parsed::to_value()`, per-shape inline
materialisers, 12-entry value bench. Eager JSON held far from sonic
(3.63× twitter). Lazy lane `bbnf_get_twitter` = 2953× sonic_get —
not a real lazy parse.

### W4 — SIMD string + `pay_f64` + regex spots (complete with recorded misses)

Inline SIMD unescape at string parse site (+6% twitter spot).
`pay_f64` substrate landed but canada bench-neutral. Regex
specialisation scaffolds; structural-scan consumer wiring. Twitter
746 MB/s at close.

### W5 — packed substrate experiment + direct JSON write (complete with recorded misses)

`TapeBuilder::open_compound` + `close_compound` + `SIB_SKIP_STAMPED_BIT`
+ `note_push` hook landed. JSON object + array Shape-1 retargeted to
open/close (42 inline stamps in `target/expand/ay-json.rs`). Read-side
cursor verified substrate-ready. `w5_close_stamp_activation.rs`
probe landed (5 passing, 1 post-regen-gated).

**Twitter regressed**: 746 → 616 MB/s (-17% under fat-LTO). The
`note_push` per-push hook overhead and the provisional span_hi +
re-stamp pattern exceeded the finaliser post-pass cost.

### W6 — direct-to-struct + navigate_tape + Pratt outer compound (complete with recorded misses)

Consumer unification audit (no rebuild surfaces existed). Direct-to-
struct admission broadened 2 → 71 surfaces across JSON/CSS L4/Sheets/
BBNF via `ir.payload_layouts` + `RustNamedTypes`. `PROJECTION_DIRECT_TO_STRUCT`
grammar-associated const. 69 `materialize_projection_<rule>_<Grammar>`
helpers. Pratt outer compound retargeted (reducers stayed on post-
order mark_children). `runtime/path.rs::navigate_tape` + 4 test
assertions.

**Twitter regressed further**: 616 → 548 MB/s (-11%, cumulative W4
→ W6 -27%). Bootstrap regen produced compile failures in gorgeous +
bbnf-bootstrap from @pretty emission drift; `generated.rs` held at
W5-era state. Schema bumped 15 → 16.

### W7 — superseded by AY-II

Single-agent consolidated dispatch (regen repair + dead-surface
retirement + shared-fact + perf recovery) stalled on a rollback-
invariant violation in `TapeBuilder::note_push`: emitter retry-IIFE
sites (`wrap.rs`, `keyword.rs`, `inline.rs`, `alt_dispatch.rs`,
`flat.rs`, `pratt.rs`) call `columns_mut().truncate(...)` past a
recorded `last_child`, causing `new_idx - prev` underflow. The
agent's draft `prev < new_idx` guard preserves the broken invariant
and was discarded. The halt triggered the
`README.md` relinquish-when-stuck + audit-expand edicts (commit
`62de21c4`); a 4-agent audit triumvirate produced
`AY-II/audit/AUDIT-{A,B,C,D}-*.md` prescribing the architectural
consolidation now owned by AY-II/W0.

### W8 — superseded by AY-II

AY-II/W1 owns the near-parity close + FINAL + handoff.

## Audit triumvirate findings

The four audits live at `docs/tranches/AY-II/audit/` (successor pass
owns the research per SPEC §Multi-pass tranche split). Headline
verdicts:

- **AUDIT-A (plan coherence)**: 2/5 overall. AY's thesis
  under-budgeted scope 2× against the 9-wave plan. The "close with
  recorded misses" pattern across 5 of 7 waves is the AV-era
  substrate-without-activation anti-pattern resuming. Recommended
  split.
- **AUDIT-B (hitherto vs expand truth)**: 6 SOUND / 1 DRIFT / 1
  DEAD. Direct-to-struct admission (71 projections) is sound.
  `navigate_tape` is DEAD — zero production consumers; `__path_walk`
  emitter untouched. W7 band-aid fix is DRIFT; root cause is
  correct; the fix is wrong.
- **AUDIT-C (forward path)**: Top 3 architectural transpositions —
  (1) move `open_stack` into `Columns` OR emitter-side
  `rollback_to`; (2) retire `push_compound` entirely, unify on
  open/close; (3) retire `note_push` + `SIB_SKIP_STAMPED_BIT`,
  restore finaliser as sole stamp source. Score 5/5 on all three.
- **AUDIT-D (perf truth)**: bbnf_value_twitter = 538 MB/s / 5.94
  cyc/byte; sonic_value_twitter = 2151 MB/s / 1.49 cyc/byte; ratio
  **3.995×** (close gate is ≤ 1.15×). CSS + Sheets fat-LTO benches
  PANIC at master HEAD — W5/W6 close only benched JSON and missed
  this. The **visitor-lane codegen already hits the gate**:
  bbnf_visitor geomean = **0.99× sonic** across 5 fixtures (twitter
  1.12×). AY.W8 gate is conditionally attainable in current
  substrate via visitor-lane migration.

## Hard gates — pass-I status table

| Gate | AY-I target | Pass I measured | Routing |
|---|---|---|---|
| Canonical packed substrate + direct JSON write | W5 | partial (Shape-1 only; Shape-2 + Pratt reducer on push_compound) | AY-II/W0 |
| `view()` / `to_value()` / `get()` unified | W6 | unified at interface; tape-reconstruction path survives | AY-II/W0 |
| Grammar-derived direct-to-struct + Pratt lowering | W5.2 + W6.2 + W6.3 | 71 admissions (PASS); reducers still post-order | AY-II/W0 |
| Every mined / emitted surface has a production consumer | W6 | `navigate_tape` is DEAD | AY-II/W0 retires it |
| twitter ≤ 1.15 × sonic | W8 | 3.995× (MISS) | AY-II/W1 |
| canada ≤ 1.20 | W8 | ≈ 3.07× (from value bench, MISS) | AY-II/W1 |
| citm ≤ 1.20 | W8 | ≈ 3.8× (MISS) | AY-II/W1 |
| 5-fixture geomean ≤ 1.20 | W8 | ≈ 3.7× (MISS) | AY-II/W1 |
| CSS / Sheets / BBNF preserve functional guarantees | W8 | CSS + Sheets PANIC at fat-LTO HEAD | AY-II/W0 fixes transitively |
| Structural scan as first-class same-path | W7 | surface available, no consumer | AY-II/W0 retires |
| B0 closes, no parity-critical work parked | W8 | satisfied (B0 at `317cb394`, FINAL.md + post-B0.json) | PASS |

## Pass I contribution (honest ledger)

**Infrastructure landed that AY-II builds on:**
- `Columns::rollback_to(open_offset)` → TBD at AY-II/W0 (not yet a primitive).
- Direct-to-struct admission broadened from 2 → 71 grammar-derived
  projections with 69 materialisers (W6.b).
- `PROJECTION_DIRECT_TO_STRUCT` associated-const pattern on the
  grammar impl.
- `Columns` substrate refactored at W1 (AU AoS revert; 7 columns →
  1 Vec<TapeRec> + parallel sib_skip).
- B0 annex closed — three-tier profile discipline + 10 `ay-*`
  Makefile gate targets + idempotent prepared-binary workflow.
- `w5_close_stamp_activation.rs` probe test documents substrate
  semantics for future regression coverage.

**Experiments failed + retired at AY-II/W0:**
- `TapeBuilder::open_compound` + `close_compound` + `open_stack` +
  `note_push` + `SIB_SKIP_STAMPED_BIT` — the W5 experiment is the
  per-push overhead cause; AY-II/W0 retires in full.
- `runtime/path.rs::navigate_tape` — DEAD (0 consumers) per Audit B;
  AY-II/W0 retires.
- W7 draft `prev < new_idx` band-aid (uncommitted, discarded).

## Cross-tranche debt

- **Parse-that stash**. Sibling repo `/Users/mkbabb/Programming/parse-that`
  carries a stashed in-flight SaturationCache retirement
  (`"WIP: SaturationCache/egraph-rules retirement (pre-B0 in-flight;
  stashed 2026-04-20 by bbnf-lang/B0 orchestrator to unblock cargo
  check)"`). Routes to BA or a future regex-analysis wave.
- **CSS + Sheets fat-LTO panic** surfaced by Audit D — routes to
  AY-II/W0 (transitively fixed by `rollback_to` primitive + emitter
  unification).

## Commit lineage (pass I)

B0 open: `66a0f2cd` → B0 close: `317cb394` (14 commits).
AY-I opens at `317cb394` → AY-I close (this FINAL + rename commit).
See `docs/tranches/B0/FINAL.md` for B0's per-wave commit ledger
and `docs/tranches/AY-I/PROGRESS.md` for AY-I's per-wave ledger.

## Pass I → Pass II handoff

AY-II opens at the commit this FINAL.md lands on. Its plan
document at `docs/tranches/AY-II/AY-II.md` executes the gestalt
re-ordered architectural transpositions prescribed by the audit
triumvirate. Pass II's invariants carry pass I's `ONE parser, ONE
substrate, grammar-derived, no fallback` thesis forward; the only
difference is the wave schedule and the specific emission API
(open/close unified, push_compound retired; finaliser single-pass
stamp restored; visitor-lane default `to_value()`; navigate_tape
retired).
