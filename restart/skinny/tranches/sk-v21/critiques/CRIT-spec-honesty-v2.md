# CRIT — spec-honesty v2: SPEC-DRAFT-P1 (pass-1-amended) re-read against pass-2 measurement

Date 2026-07-05. Supersedes `CRIT-spec-honesty.md`. Posture: adversarial, conservative. The v1
verdict was 84 with two must-fix editorial defects (AU arithmetic; the dual JSON floor) and one
inherent-to-stage cap (no ranking verified-cold). This pass checks (a) whether the amendment
discharged those two, and (b) whether the pass-2 instruments PROTO-A1/A2/A3 — the first same-window
cold/PMU captures on the mandated `json_canon_bench` family — leave any spec number honest, single-
floored, loadavg-qualified, and consistent with the environmental adjudication (host baseline 6-8,
quiet certification = a TW0 execution gate).

Verdict up front: **the two v1 must-fix defects are genuinely discharged**, and the pass-2 evidence
*confirms* three of the spec's harder honesty calls (the canada loss, the §7.3/smallbox decoupling,
the per-corpus discipline). But A1/A2 resolve two items the spec left OPEN or hedged, and both
resolutions land AGAINST the spec's current flagship text: the fold-over-lazy-cursor default is now
benched and LOSES 2.3–4.9×, and the asm "exception the intrinsic cannot survive" is obviated by an
intrinsic. The spec text predates A1/A2/A3 and has folded none of them; the required corrections are
editorial fold-ins its own deep sections (§4, §5.4) already anticipated — not re-research — but two
touch flagship sentences and one is a live overclaim.

## The two v1 must-fix defects are discharged (verified)

- **v1 Defect 1 (AU arithmetically impossible) — FIXED.** SPEC:76–78 now states "twitter 1967 MB/s,
  1.63 cyc/byte" and explicitly footnotes report 09's internal error ("prose '0.615' is bytes/cycle
  mislabeled as its inverse — the arithmetic 3.2e9/1967e6 ≈ 1.63 settles it"). Arithmetic re-checked:
  3.2e9/1967e6 = 1.627. Correct. A1 Deliverable 4 independently reproduces the floor and the meet:
  structural `parse_tape` twitter **1.5654 < 1.63** (PROTO-A1:156,161).
- **v1 Defect 2 (JSON floor quoted two incompatible ways) — FIXED.** SPEC:82–84 now reads "**The
  single governing JSON floor is the TW0 fresh cold baseline**; every no-regression clause in this
  spec binds to it and to nothing else," and demotes the AU-relative capture to "context, not a close
  condition." A grep of every no-regression clause confirms single-floor binding: SPEC:461 (TW-S),
  470 (TW1b), 474 (TW1c), 491–493 (TW3) all bind to the TW0 cold baseline; SPEC:493 carries the
  AU-relative capture only "as context." No residual "meets the AU-relative floor cold" gate survives.
  The category error is gone.

## What the pass-2 evidence CONFIRMS (the hard honesty calls hold)

- **The canada loss is now double-sourced, exactly as the spec framed it.** SPEC:91–94 states
  "**canada LOSES to sonic cold**" and refuses P2's 4/4 sweep. A1 Deliverable 2 independently
  re-measures it on the same instrument: fused/sonic canada **1.252× wall, 1.137× cyc** (PROTO-A1:88–90),
  confirming report 33's sign and refuting PROTO-P2's 0.79× (PROTO-A1:92–94). The spec's per-corpus
  framing (win twitter/citm, lose canada) is now the measured truth, not an aspiration — and the
  parity gate is byte-exact incl. fingerprint across all three corpora (PROTO-A1:28–41), which is
  stronger than the XOR-under-`black_box` the spec (SPEC:123) worried about.
- **The §3/§7.3 smallbox decoupling the spec asserted is proven true.** SPEC:160–165 refuses to add
  a "de-nightly parse-that first" precondition, on the ground that §7.3 routes onto the canonical
  regex crate whose dep set "carries no smallbox and no parse_that." A3 Deliverable 2 ran the
  `cargo tree` P3 could not: `bbnf-regex` subtree **smallbox count = 0** (PROTO-A3:91–97), the sole
  blocker is legacy root `parse_that 0.4.0 → smallbox[nightly]` (PROTO-A3:77–86), and PROTO-P3's
  coupling recommendation is falsified on the graph (PROTO-A3:88–97). The spec picked the correct
  side of CRIT-P3 before the evidence existed.
- **The DIRECTIONAL / loadavg discipline is consistent and correct.** Every perf claim carries its
  loadavg (report 33 at 8.3–10.2 SPEC:10; P2 at 16–17 SPEC:13; P5 at 17–21 SPEC:26) and every ranking
  is marked DIRECTIONAL until the TW0 quiet gate (SPEC:11–13). A1/A2/A3 all ran at loadavg 8–16 with
  ratios authoritative and absolutes directional — the exact regime the spec declares. The
  environmental adjudication is honored.

## Defect A (new, must-fix) — the asm "exception the intrinsic cannot survive" is refuted by A2

SPEC:24–26 (charter restatement) reads: hand asm buys 0–5% "except at register-pressure failures AND
at instruction-selection misses **the intrinsic cannot survive** — P5 measured **one such exception**
on our own classify kernel (~10%, DIRECTIONAL)." A2 Deliverable 1 refutes the framing directly: door
(a), a pure NEON **intrinsic** (`vshrn_n_u16::<4>` movemask, already shipping at `json.rs:928`),
beats the baseline **+35%** and beats the hand asm by **~20–28%** (asm/door-a 14.786/11.558 = 1.28),
because the binding resource is the 8 SIMD→GPR crossings, not the classify ALU, and the intrinsic
pulls the bigger lever (PROTO-A2:47–80). So the classify kernel is precisely NOT "an
instruction-selection miss no intrinsic spelling survives" — an intrinsic spelling survives it and
wins. The ~10% asm figure itself is confirmed (A2 reproduces +6.7–10.4%, PROTO-A2:55), but the claim
it is cited to support is false.

This is a genuine overclaim in a flagship sentence, of the same kind as v1 Defect 1. The mitigant is
that the spec's own §5.4 (SPEC:263–271) already carries the doors-first order and predicts door (a)
"likely beats all four variants, making the asm question moot" — A2 vindicates §5.4 outright. The
required fix is therefore reconciliation, not research: SPEC:24–26 (and the §2 asm bound) must be
softened to match §5.4 and A2 — the classify kernel is now the case that PROVES an intrinsic obviates
the asm lane, the opposite of an unsurvivable miss. As written, §0/§2 assert a thing the same
tranche's evidence disproves.

## Defect B (new, must-fix) — the spec's actual default construct is now benched NEGATIVE

SPEC:96–97 states the "actual default construct (fold-over-lazy-cursor, §4) remains an OPEN TW3
sub-gate, **unbenched**." A1 Deliverable 3 benched it: the fold-over-lazy-cursor LOSES to sonic
**2.32×/4.85×/2.27×** (twitter/canada/citm) and loses to its own fused sink **3.87–4.23×**
(PROTO-A1:124–145) — the SLOWEST materialization lane measured. So "unbenched" is now factually
false, and the §4 headline "**Eager is a generic fold over the lazy cursor — one substrate**"
(SPEC:191), presented among "externally-corroborated design facts" (SPEC:187), is refuted as
competitive. The honesty discipline itself held — SPEC:192 explicitly hedged "a fold-over-cursor
could plausibly lose," and §2 flagged it OPEN, so the spec did NOT claim the win — but leaving the
default labelled OPEN/unbenched after A1 is the stale-into-dishonest transition v1 Defect 3
anticipated, now triggered.

The sharper consequence is a live internal contradiction in the wave manifest: TW3's close gate
(SPEC:490–493) requires both "the fused lane IS the default with no cold regression vs the TW0
baseline on any corpus" AND "the fold-over-lazy-cursor benched … the §4 OPEN sub-gate discharged." If
the default is the fold-over-cursor, that gate is UNMEETABLE (the construct is 2–5× off floor); the
gate is only meetable if "default" means the fused SINK (which meets floor on twitter/citm — A1 D4
citm 1.354 under AU, twitter +23%, PROTO-A1:164–166). The synthesis must relabel: the default is the
single-pass fused sink; the fold-over-lazy-cursor sub-gate closes NEGATIVE, not "open." §4 SPEC:191
and §11 SPEC:492–493 both carry the stale construct.

## Defect C (new, moderate) — "Sampled buys nothing measurable" is too strong on small corpora

SPEC:102–104 asserts "**Sampled presize ties GrowOnly within drift**, so removing the 7–8 realloc
growth events buys nothing measurable," and SPEC:106–107 expects the interleaved re-run to "certify
… the Sampled-tie noise floor." A1 Deliverable 1 IS that interleaved paired re-run, and it found the
opposite on the small corpus: twitter Sampled **+3.45%, 95% CI [+1.07, +5.83] excludes zero — a
statistically real win** (PROTO-A1:52–71); canada and citm tie (CIs span 0). So "buys nothing
measurable" is refuted on twitter; A1's own summary calls the flat claim "marginally too strong"
(PROTO-A1:70–71). The DELETE of `OneShotSimd`/`Exact` still stands overwhelmingly (they pay 30–170%
cyc/byte to reclaim ≤3.5% — PROTO-A1:73–77), and `GrowOnly`-only remains defensible on KISS grounds,
but the *justification* must change from "Sampled is null" to "Sampled buys ≤3.5% on small inputs,
nothing on large; deleted for KISS, not for null." As a bonus the pass-2 run also overturns
PROTO-P1's canada +9.3% to a +1.25% TIE (PROTO-A1:58–63) — which the spec never cited, so this
corrects the instrument, not the spec.

## Minor residue (not honesty defects; note for the fold-in)

- **The reconstruction-tax figure conflates two constructs.** SPEC:89–90 credits fused with beating
  "the tape-then-walk reconstruction default by ~3.2×" (a P2 number for a different, less-bad
  construct). A1 shows the spec's *actual* default (fold-over-cursor) pays 3.9–4.2× vs the sink, not
  3.2×; keep the two constructs distinct when the number is folded.
- **Divan's warm/cold scope is undisclosed.** SPEC:148 calls divan "the frozen harness"; A3
  Deliverable 3 clarifies divan is inherently WARM/iterated and covers only the kernel-microbench
  tier — the cold-single-parse discipline stays in the `harness=false` mains (PROTO-A3:159–162). Not
  a mislabel, but the spec should scope it. A3 also prices the real cost the spec's §3.4 leaves
  unpriced (SPEC:168–170 already admits TW-S is unpriced): the gate/report criterion-ingestion
  re-path, 60+ references (PROTO-A3:163–169) — the spec's honesty here is intact because it declared
  the leg unpriced.

## Convergence on spec-honesty: 86

The amendment discharged both v1 must-fix defects, and the pass-2 evidence confirms the spec's
hardest honesty calls (canada loss, smallbox decoupling, per-corpus + loadavg discipline). That would
lift the score comfortably into the 90s were the text current. It is held at **86** because the same
tranche's instruments now contradict two flagship-adjacent claims — the asm "intrinsic cannot
survive" (A2 refutes; Defect A) and the fold-over-lazy-cursor default still labelled "unbenched/OPEN"
while benched-negative and failing its own TW3 close gate (A1 refutes; Defect B) — plus the Sampled
"buys nothing" overstatement (Defect C). All three are editorial fold-ins the spec's deep sections
(§5.4, §4's own "could plausibly lose") already anticipated, so this is well above the 70 re-loop
line: the honesty *discipline* is exemplary and none of the three is a deception. But two touch the
charter's flagship framing and must be reconciled before freeze, so the item is not closed at 90+.
