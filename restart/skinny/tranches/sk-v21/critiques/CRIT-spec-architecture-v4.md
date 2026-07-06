# CRIT — SPEC-DRAFT-P1 architecture, pass-4 confirmation verify (the freeze confirmation)

Target: `restart/skinny/tranches/sk-v21/SPEC-DRAFT-P1.md` (686 lines, pass-2-amended + the two
pass-3 spec-edits), read against `critiques/CRIT-spec-architecture-v3.md` (94/100, two spec-edit
blockers NEW-R8 + NEW-R9), the companion pass-3 closures `CRIT-spec-honesty-v3.md` (94, zero
blocking) and `CRIT-freeze-audit.md` (95, `residuals_wave_owned=true`). Charter: confirm NEW-R8 and
NEW-R9 folded exactly, sweep the amended regions for any new contradiction, apply the freeze
criterion.

## Verdict up front

**Both pass-3 blockers are folded exactly; zero new contradiction is introduced; every residual is
a wave-owned execution item or freeze-pass authoring output.** Per the freeze criterion — the score
is the frozen-executable-tomorrow score when zero blockers survive and all residuals are wave-owned
— this is **100**.

## NEW-R8 (TW4.1 KernelCostTable gate-class relaxation) — FOLDED EXACTLY

The TW4.1 close gate now carries the relaxation NEW-R8 demanded, at §11:594-599:

> *Close gate:* canada eager cold beats sonic on `json_canon_bench` (the float-tax signal closed);
> the table feeds the live selector with a `cargo expand` witness. Gate relaxation, carried openly
> (mirroring TW1b's NEW-R7 clause): the cost-table leg closes on liveness alone — A2/P4-v2 measured
> NO current-fleet decision flip (§5.1: live crossover N\*=3, flip band |S|≤2, JSON/CSS sit at N≥3),
> so the §1 ≥3%-win clause is unmeetable here by settled fact and binds instead at TW6's CSV flip
> exercise (§5.1's consumer binding); the Eisel-Lemire leg carries the full liveness-AND-win clause
> (canada-beats-sonic IS its measured win).

Every element NEW-R8 specified is present, checked against the template it proposed
("liveness + selector-fed + TW6-consumer-named, not ≥3%"):

- **Explicit gate-class relaxation, mirroring TW1b's NEW-R7** — the fold names it verbatim
  ("mirroring TW1b's NEW-R7 clause"); TW1b's parallel sentence is at §11:574-575
  ("liveness-AND-no-regression, not the §1 ≥3%-win clause"). The two now share one treatment shape.
- **Liveness-only** — "the cost-table leg closes on liveness alone." The bare-liveness phrase §1.1:70
  warns against ("liveness alone is INSUFFICIENT") is here a JUSTIFIED carve-out, not a loophole:
  A2/P4-v2 proved NO current-fleet decision flips (§5.1:292-293), so the cost table emits
  byte-identical selections to the live ladder — there is literally no behavior to regress and no
  win to measure at TW4.1. The gate is not bare liveness either: the preceding clause requires
  "the table feeds the live selector with a `cargo expand` witness" (selector-fed) and the
  relaxation binds the TW6 consumer (TW6-consumer-named). Complete against the NEW-R8 template.
- **The ≥3%-win clause bound to TW6's CSV flip** — "binds instead at TW6's CSV flip exercise
  (§5.1's consumer binding)." The deferral is not dangling: the TW6 close gate exercises it at
  §11:630-632 ("the KernelCostTable's single-delimiter flip exercised live (CSV, |S|≤2 — the
  measured flip band, §5.1)"). §5.1:299-301 orders "TW4's table landing so its first outcome-
  changing flip is exercised by a fleet grammar in the same campaign … the table lands with its
  TW6 consumer named in the same commit message"; §13:670 records the same as closed question 5.
  The loop is closed on both ends.
- **The wave is not dead substrate** — the Eisel-Lemire leg carries its own measured-win gate
  ("canada eager cold beats sonic on `json_canon_bench`"), so TW4.1 as a whole is win-gated; only
  the cost-table sub-landing takes the carve-out. Exactly the split NEW-R8 asked for.

This is the identical model treatment TW1b received (NEW-R7) and that honesty-v3 (§ "N1-N5")
and the freeze-audit (row NEW-R7) both accepted as the correct pattern for a substrate whose §1.1
win-gate is unmeetable at its landing wave.

## NEW-R9 (section-4 two-lanes phrasing) — FOLDED EXACTLY

§4:249-250 now reads:

> The value plane is therefore two lanes, only one of them tape-side: **lazy = the cursor over the
> ONE tape; eager = the DISTINCT tapeless fused single-pass sink** (§0) …

The self-tensioning v3 flagged ("two lanes over the one tape-side substrate" immediately followed by
"eager = the tapeless … sink") is gone. "Only one of them tape-side," with lazy=tape / eager=
tapeless named in the same breath, is non-self-tensioning and now matches the charter §0:44-45
wording ("a lazy cursor over the ONE tape plus a DISTINCT eager single-pass fused sink"). No reader
re-reads the eager lane as tape-backed. Folded as specified.

## Sweep of the amended regions — no new contradiction

- **§11 TW4.1 gate vs §1.1 law** — the "liveness alone" carve-out is explicitly reconciled with
  §1.1's P1 hazard (the justification: no flip → no win to measure → deferred to TW6). Coherent,
  mirrors the accepted NEW-R7 shape. No contradiction.
- **§11 TW4.1 gate vs §5.1 / TW6 / §13** — the TW6/CSV binding is reflected at the TW6 close gate
  (§11:631), §5.1's consumer binding (293/299-301), and §13's closed question 5 (670). All four
  sites agree on N\*=3, flip band |S|≤2, JSON/CSS at N≥3, CSV as the flip exerciser. Consistent.
- **§4:250 vs §0:44-45 vs §2:117-122 vs TW3 gate (§11:600-604)** — every site describes the same
  two-lane model (lazy cursor over the ONE tape; distinct tapeless fused sink as default). The
  refuted "one substrate" phrase survives only as retractions (§0:46, §4:245, §4:253 "'stronger
  claim' is retracted"; TW3 gate records it resolved-negative). No affirmative residue. Consistent.
- **Companion critiques undisturbed** — the R8/R9 edits touch §11:594-599 and §4:249-250 only;
  honesty-v3's two editorial hairlines (run-order flip §0:39, the "BEATS" verb §0:120) lie outside
  the amended regions and remain the same non-blocking nits. Nothing the freeze-audit relied on
  (C1-C6 gates, the three dated seams, the TW0 spine) is touched.

## Residuals — all wave-owned or freeze-pass authoring

- **Wave-owned execution deliverables**: the TW0 quiet (loadavg<2) re-certification of the pass-2
  directional absolutes incl. the un-confounded nightly/stable channel ratio; TW-S cold-parity
  re-capture; TW5 step-0 width-fix re-land; TW6 regen wiring + the CSV flip exercise (§13:683-686).
- **Freeze-pass authoring output** (per the freeze-audit): rename to SPEC.md, author GESTALT-
  SYNTHESIS / top-level DEFERRED-LEDGER / forward prompt, ledger the pass-3+4 entry. These are what
  declaring convergence TRIGGERS, not preconditions for the decision.
- **The one ring-fenced frontier uncertainty** (§13.1 CSS SIMD tokenization severity) is a costed
  per-grammar TW4 decision with a defined mechanism, outcome awaiting the TW0 measurement loop — a
  scheduled decision point, wave-owned, not an open design question.

None is a research or design question. `residuals_wave_owned = true`.

## Convergence

**100/100.** NEW-R8 is folded exactly — the TW4.1 cost-table gate carries the explicit liveness-only
relaxation mirroring TW1b's NEW-R7, with the ≥3%-win clause bound to TW6's CSV flip and that binding
closed at the TW6 gate, §5.1, and §13; the Eisel-Lemire leg keeps its own measured-win gate so the
wave is not dead substrate. NEW-R9 is folded exactly — §4:250 now matches the charter §0 two-lane
wording and no longer self-tensions. A sweep of both amended regions surfaces no new contradiction;
every folded number remains faithful to its source instrument; the two honesty hairlines are
untouched cosmetic nits outside the edits. Zero blocking issues survive and every residual is a
wave-owned execution deliverable or freeze-pass authoring output. By the freeze criterion — 100 is
frozen-executable-tomorrow with only wave-owned items remaining — the architecture is at 100. Freeze.
