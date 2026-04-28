# AZ-I Audit 1 — Process Retrospective

Lens: the last ~10 tranches as a process organism — what its
ceremonies deliver, what they consume without paying back, where
its motion is best. Grounded in `docs/tranches/{LETTER}/{FINAL,
PROGRESS}.md`, `REMAINING-TRAJECTORY.md`, and
`docs/instructions/tranche/SPEC.md`.

## 1. Tranche cadence and throughput

Wall-times reconstructed from `git log --since` + dated PROGRESS
entries:

| Tranche | Wall | Signal/noise |
|---|---|---|
| AU | mid 2026-04, dense | **best (pre-B)**; 6 phases, 24 gates, AU baseline anchor (`AU/FINAL.md`). |
| AV | 2026-04, dense | **worst** — Era-V rut. Bench-omission until V10 hid 2.5–4.5× regression (`AV/FINAL.md` §"Performance posture"). |
| AW (I/III/IV/V) | multi-pass | **worst** — 4 sequential FINALs, 21,198-line `generated.rs`, gate 9 unreachable (`AW/FINAL-I.md:149`). |
| AX | 2026-04 | high — 95% interpreter LOC excised (`AX/FINAL.md` §W0b). |
| AY-I | 2026-04-19→21 | medium; 5/7 waves "complete with recorded misses" (`AY-I/FINAL.md`). |
| AY-II-I | 2026-04-21→25 | low — W1-W5 never executed; absorbed at B4.W1. |
| AY-III | 2026-04-27 | superseded same day (`AY-III/PROGRESS.md:9`). |
| **B0..B7** | 2026-04-23→27 | **best in corpus** — 8 closures in 5 calendar days. |
| AZ-I (W0→W2-substrate) | 2026-04-27 single day | mixed — W0/W1 ~17/~70 min; W2 substrate-only, 3 deferrals. |

Peaks: B-series, ~1126 commits across the 2026-04-15→27 window.
Troughs: AV/AW consumed weeks for less durable gain than B5 alone
delivered in three days (substrate restoration of the AV/AW weld;
`B5/FINAL.md` §"Architectural narrative").

## 2. Recurring failure modes

**(a) Substrate-without-consumer.** AY-I.W3 `navigate_tape` 0
consumers (`AY-I/FINAL.md` §"Audit triumvirate findings",
AUDIT-B). AY-II.W0.e `STRUCTURAL_SCAN_POLICY` 0 consumers
(`AY-II-I/PROGRESS.md:239-243`). AZ-I.W2 nine per-shape struct
emitters but `for_grammar` returns `TapeDirect` for every grammar
(`AZ-I/PROGRESS.md` W2 close §"Activation reverted"). **≥3 in
last 4 tranches.**

**(b) Wave proliferation.** AX: 19 wave docs `W0a..W15`
(`docs/tranches/AX/waves/`). AW-I.W4: six sub-waves
`W4α..W4ζ` (`AW/FINAL-I.md` §W4). AY-II-I.W0: five sub-agents
plus W0-fix plus 4-agent audit triumvirate
(`AY-II-I/PROGRESS.md:147-289`). **4 tranches with sub-letter
cascades over five deep.**

**(c) Mid-tranche pivots opening new letters.** AR/AS pair
(SPEC §Scope-reveal anti-example, `SPEC.md:341-344`). AW into
AW-I→AW-II→AW-III→AW-IV→AW-V (five FINAL.md files). AY into
AY-I→AY-II-I→AY-III. **Six letter-multiplications in the
corpus window.**

**(d) Plan miscalibrations.** AW-I gate 9 (`generated.rs ≤ 12000`,
real floor ≈19k; `AW/FINAL-I.md:149` `✗ plan miscalibration`).
B6.W1 budget 660s, measured 17s (38× stale); B6.W1+W2 both
close `rationale-satisfied per SPEC §Plan-time miscalibration`
(`B6/FINAL.md:46-95`). **≥3 hard gates falsifiable at plan-time.**

**(e) Cap-induced halt-and-report.** AZ-I.W2.A "~3,000 LOC across
6+ emitter files... exceeding a single 90-min implementation
dispatch" (`AZ-I/audit/W2-EMITTER-REWIRE.md:5-7`). SPEC
§Diagnostic-loop relinquish (`SPEC.md:397-434`) institutionalises
the pattern: every scope mis-estimate triggers a 3-agent
triumvirate.

**(f) Re-shape-during-execution.** AZ-I.W1 collided on
`crates/ir/src/passes/types/`, re-shaped into 2-stage flow
(`AZ-I/PROGRESS.md:78-83`). AZ-I.W2 collided on
`crates/core/src/backend/rust/emitter/`, re-shaped sequential
(`AZ-I/PROGRESS.md:262-268`). **Both W1 and W2 of a single
tranche re-shaped in flight.**

**(g) Cherry-pick conflict cycles on shared files.**
SPEC §"N-agent shared-file consolidation" (`SPEC.md:172-179`)
exists because the conflict kept recurring (AW-I.W4β
`47496993`); AY-II-I.W0 hand-patched `generated.rs` at
`f372e7ef` (`AY-II-I/PROGRESS.md:191`). The mitigation rule
documents the failure mode rather than removing its cause.

## 3. Successful patterns

**(a) B-series 4-7-day cross-cutting closures.** B0→B7 closed 8
tranches in 5 calendar days. Each B-tranche carries a single,
narrow architectural lever (B2 build-time codegen transposition;
B5 substrate restoration; B6 regen mtime cycle; B7 workspace
modernization). FINAL.md sections collapse from 25-30 (AU/AV/AW)
to 6-12 (`B*/FINAL.md`). What works: **single-lever scope,
evidence-grounded gates** — B6.W0 measured 192× speedup against
the 3-min gate threshold (`B6/FINAL.md:38-44`).

**(b) W1 4-parallel + orchestrator-rescue.** AZ-I.W1.B
dispatched 4 parallel agents on disjoint bounds (JSON / Sheets /
CSS L4 / emitter), 70-min real wall (`AZ-I/PROGRESS.md:139`).
W1.B2 Sheets `(orchestrator-rescue, 6 tests)` confirms
SPEC §"N-agent shared-file consolidation" anticipated the
conflict at plan time and budgeted the consolidation commit.

**(c) Sub-agent triumvirate (research + plan + redress).**
AY-II-I 2026-04-21 `audit/AUDIT-{A,B,C,D}-*.md` produced
convergent findings inside one wave (`AY-II-I/PROGRESS.md:261-289`).
AY-I close triumvirate drove the AY-I→AY-II split disposition
(`AY-I/FINAL.md:105-134`). What works: **parallel disjoint reads
with hard caps** (20/15/30 per `feedback_dispatch-hard-cap`).

**(d) Dispatch hard-caps.** AZ-I.W0 closed 17 min against 20/45
caps (`AZ-I/PROGRESS.md:30`). B6.W0 single 35-LOC dispatch.
Cap is forcing function — agents ship probe-or-relinquish at
0.9× cap rather than grinding.

## 4. Process drag — concrete cuts

**Cut 1 — Per-wave WAVE_SPEC.md docs.** SPEC's WAVE_SPEC declares
9 required sections per wave. AZ-I carries 5 wave docs (529 LOC
across `waves/W0..W4.md`); AX carries 19
(`docs/tranches/AX/waves/`). Most content recapitulates the parent
plan's Phases table. **Fold-in:** plan-table-driven dispatch.
Eliminate per-wave files; agent briefs cite the parent plan's
phase row directly. **Drag: ~500-2000 LOC/tranche.**

**Cut 2 — Multi-pass tranche split overhead.** AY-I→AY-II-I→AY-III
produced 3 plan docs, 3 PROGRESS, 3 audit dirs — and AY-III
deferred without dispatch (`AY-III/PROGRESS.md:9`). The
split-vs-new-letter rule (`SPEC.md:374-379`) is rarely the active
discriminator; the operative one is "did we land enough to write
FINAL." When no, the next pass inherits substrate-without-consumer.
**Fold-in:** allow tranche extension (more waves) over
roman-numeral pass-suffix. Reserve letter-multiplication for
thesis changes only. **Drag: ~3 doc sets / split + audit
triumvirate at every relinquish.**

**Cut 3 — "Substrate-only" close ceremonies.** AZ-I.W2 closed
substrate-only with 3 `DEFERRED-TO-W2-ACT` gates
(`AZ-I/PROGRESS.md:255-260`). SPEC §Activation-gate already
prohibits this (`SPEC.md:243-251`). Violation shipped anyway.
**Fold-in:** activation consumer dispatches in the same wave as
substrate. AZ-I.W2's "W2-act follow-on" should have been W2's
actual scope. **Drag: 1 ceremony wave per substrate landing.**

**Cut 4 — FINAL.md template bloat.** AU/FINAL.md 463 LOC,
AW/FINAL-III.md 795 LOC. B5/FINAL.md 331 LOC, B7 318 LOC — the
B-series proves close fits in 200-350 LOC. SPEC §Closing
ceremony (`SPEC.md:506-521`) requires only 5 items. 600+ LOC
FINALs are recapitulation. **Fold-in:** cap FINAL at 350 LOC.
**Drag: ~300-450 LOC/close.**

**Cut 5 — Phase template ceremony.** SPEC §195-205 mandates
per-phase ID + Owner + Scope + Hard gate + Commit template. B6's
three waves close on one sentence each (`B6/FINAL.md:97-130`).
**Fold-in:** apply B6-template uniformly; single-row phase
descriptors. **Drag: ~50-200 LOC sub-phase prose per tranche.**

**Cut 6 — AGENT_DISPATCH.md per-tranche manifests.** B1, B2, B3,
B4, B5, B6, B7 each carry one. Content is worktree path +
allow-list + return format — already in the orchestrator's
dispatch text. **Fold-in:** retire entirely; dispatch text lives
in PROGRESS.md timeline. **Drag: 1 doc/tranche, ~150-300 LOC.**

**Cut 7 — Status-normalization PROGRESS entries.** AY-II-I has
three consecutive entries closing on "no runtime state changed in
this entry" (`AY-II-I/PROGRESS.md:96-99, 122-124, 142-144`),
~40 LOC each. **Fold-in:** single-line preamble in next dispatch.
**Drag: ~120 LOC/long-tranche.**

## 5. Stop-deferring imperative

Deferrals visible in the corpus, with land-as-one replacements:

| Deferred-to | Origin | Land-as-one motion |
|---|---|---|
| AY-III | AY-II-I W1-W5; never executed (`AY-III/PROGRESS.md:9`) | Already absorbed into AZ-I.W4 + AZ-II.W2 — confirms the pattern. |
| `W2-act` | AZ-I.W2 substrate-only (`AZ-I/PROGRESS.md:215-225`) | W2 dispatch should have included `JsonDocument::view()`/`to_value()` accessors + parity-harness recoding in the same wave. |
| AW-II.W5 | AW-I gate 12 CSS L4 state_count (`AW/FINAL-I.md:189`) | Floor-uncertain at plan-time → soft-target per SPEC §Gate floor-check, not hard `<2000`. |
| AY-II-I W0' ceremony | absorbed at B4.W1 (`AY-II-I/PROGRESS.md:15-71`) | The 5-step ritual (`PATH-FORWARD.md:97-121`) became 1 commit. Over-specified ~4×. |
| AW-I.W5→AW-II close | bench + workspace-green (`AW/FINAL-I.md:128-134`) | Intentional-unworkability stretched 2 tranches; SPEC §Transitional fallback (`SPEC.md:489-504`) bounds it to a single named restoration wave. |
| 5 DEFERRED rows in AZ-I.W0 ledger | (`AZ-I/PROGRESS.md:71-72`) | Workspace-verify flipped from W0-close-required to "W1-pre-flight". Either run the gate or drop it. |

Pattern: ceremony around deferral (named destination, rationale,
ledger row) consumes more LOC than the deferred work takes when
finally executed. **Imperative:** dispatch the activation consumer
inside the substrate's same wave, expand agent count, lift caps
(45→90 min for substrate+consumer pairs), and stop authoring
"W2-act" follow-ons.

## 6. Hand-off to synthesis

Synthesis at `W2-CLOSE-AUDIT.md` reads from this doc:

- **§2(a) substrate-without-consumer** + **§5 W2-act row** —
  AZ-I.W2 reversal is the canonical instance; collapse
  W2-act + W2.B + W3 into one gestalt activation pass
  (`REMAINING-TRAJECTORY.md:18-21` already proposes this).
- **§4 Cuts 1, 2, 4, 5, 6** — per-wave WAVE_SPEC +
  AGENT_DISPATCH + pass-split ceremony + phase template + FINAL
  bloat compounds to ~1500-3000 LOC of pure ritual per
  10-tranche window.
- **§3(a) B-series template** — proves 3-day-single-lever close
  cadence; AZ-I.W0's 17-min wall is the same shape. AZ-I W2-act +
  W2.B + W3 should collapse into one activation-pass wave with
  4-6 parallel agents on disjoint file bounds.
- **§2(e) cap-induced halt + §4 Cut 3** — undersized cap on
  oversized scope produces ceremonial substrate landings. Either
  raise the cap (substrate+consumer = 90 min, not 45) or fan out
  (4 agents on 4 emitter files in parallel, not one agent
  serially).
