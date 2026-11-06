# CRIT-P2-v2 — Fused parse+value default lane, re-judged on the recovered prototype

Date: 2026-07-05. Item: P2-v2. Convergence: 70/100 (was 43 when PROTO-P2 was missing).

## The prototype now exists and the vacuous grounds of the 43 are gone

`prototypes/PROTO-P2.md` is present, detailed, and resurrectable (a byte-equal tape-walk diff
and the `p2_fused_cold.rs` harness shape). The "no evidence" verdict is retired. A cold-per-parse
JSON instrument now exists on disk in two forms — report 33's `json_canon_bench.rs` and P2's
`p2_fused_cold.rs`, both mirroring `css_canon_bench` cold discipline (fresh input clone OUTSIDE the
timed region, prior tape dropped). The "nonexistent harness" blocker is genuinely closed.

## (a) Does the 0.62-0.64× cold geomean hold up? Partially — one corpus refutes it

**Method / parity: credible and a real value-add.** Cold discipline is legitimate; median-of-60,
two runs; `assert_eq!(fused, tape_walk)` and `assert_eq!(fused, sonic)` run before every timing.
That digest-equality gate is something report 33's harness does NOT do (it only XORs fields under
`black_box`), so P2's parity procedure is a genuine contribution. Caveat, disclosed: parity holds at
digest granularity — a full-materialization proxy, not a typed-struct/serde-Deserialize product.

**The decisive refutation — canada.** PROTO-P2's "fused lane" and report 33's "track1" are the
IDENTICAL entry point `direct_struct::track1_digest` (direct_struct.rs:421), against the identical
`sonic_digest` comparator. On canada they disagree in SIGN:

- Report 33 (`json_canon_bench`, loadavg 8-10): track1 9936 Mbps vs sonic 11794 → fused is **1.19×
  sonic, LOSES**; report 33 devotes a section to it as "the sharpest optimisation signal" — the
  eager `materialize_f64` float tax, where sonic's SIMD number parse wins (33:63-69).
- PROTO-P2 (`p2_fused_cold`, loadavg 16-17): canada fused/sonic **0.79, WINS**, and the report
  claims "beats sonic on all four corpora" with no float-tax caveat at all.

Same function, same comparator, same cold family — opposite verdict on the one corpus that decides
the number-decode lever. The mandated shared harness (json_canon_bench, spec:371) already answered
the question and gives the LESS favorable answer; P2's private harness gives the rosier one. That is
an unreconciled contradiction between two co-cited recovered artifacts, and honesty sides with the
quieter capture.

**loadavg labeling: honest but disqualifying for ranking.** P2 discloses loadavg 16.4-16.9 and calls
itself DIRECTIONAL. But the absolute magnitudes are ~190× off report 33 (twitter fused ~94 MB/s vs
report 33's 17790) — far more than an 8→16 loadavg on an 18-core box can explain as contention, which
undercuts the "ratios are load-robust" premise the whole table rests on. The sign-flip on canada is
that premise failing in practice.

## (b) Which original blockers retire, which do not

- **Nonexistent harness — RETIRED.** Cold instrument exists (both forms).
- **Warm 0.99× provenance — RETIRED in kind.** P2 measures the actual fused single-pass cold, not
  the visitor analog. Both docs agree fused beats sonic cold on twitter and citm (~0.5-0.6×). The
  reconstruction-tax thesis (fused vs tape-then-walk ~3.2×) is cold-confirmed and now doubly-sourced.
- **Geomean masking twitter/canada — HALF-retired.** twitter's warm 1.12× loss was a warm artifact:
  both docs show it winning cold. canada's warm 1.03× loss is NOT cleanly retired — the quieter cold
  harness (report 33) confirms canada still loses to sonic. P2's clean 4/4 sweep is not credible.

## (c) Does the spec section stand on cold evidence, and the close-gate rewording

The direction stands, better than warm: fused-as-default is now cold-corroborated for its CORE claim
(reconstruction tax → single-pass default) by BOTH recovered docs. That is the real highest-ROI bet
and it is de-risked. But three seams keep TW3 short of close-gate-ready:

1. **Wrong construct.** P2 benched a bespoke digest SINK; the spec's default (§4:150-153) is a
   fold-over-the-lazy-cursor benched vs sonic's dual-path direct-to-struct. P2 itself flags this
   unproven (line 148-149). The construct the spec makes default is still unbenched.
2. **Wrong yardstick.** The close gate reads "meets the AU-relative floor cold" (line 348); P2
   measured only fused/sonic ratios. No AU-relative cold number (fused ns/B vs AU 0.615 cyc/byte)
   exists in either doc. Beating sonic is not meeting the AU floor.
3. **canada honesty.** The spec must carry report 33's float-tax caveat, not P2's silent sweep.

Close-gate wording to replace the warm AU references (SPEC-DRAFT-P1.md:73-78, 344-349):
- State the proven lever as the cold, doubly-sourced one: fused single-pass beats the tape-then-walk
  reconstruction (~3.2× tax) and beats sonic on twitter+citm cold; canada loses to sonic's SIMD
  number parse pending Eisel-Lemire/SWAR — per-corpus, not a 4/4 sweep or a geomean.
- Keep "fused-over-cursor vs sonic dual-path direct-to-struct" as an explicit OPEN TW3 sub-gate.
- Add an AU-relative cold capture (fused ns/B vs AU 0.615 cyc/byte) to the TW3 close gate; the
  sonic ratio is not the AU floor.
- Mandate the re-run on `json_canon_bench` itself (not the private `p2_fused_cold`), quiet loadavg
  <2, with P2's assert_eq digest-parity gate folded in and the canada sign adjudicated.

## Score

70. A substantial recovery from 43: the prototype exists, the cold instrument exists, and the
central reconstruction-tax thesis is now cold-corroborated by two independent artifacts — the item's
actual ROI bet is de-risked. It does not reach close-gate readiness because (1) P2's "beats sonic
4/4 cold" headline is refuted on canada by the quieter, mandated shared harness and buries the
float-tax caveat, (2) the spec's default construct (fold-over-cursor) is still unbenched, (3) no
AU-relative cold floor was captured, and (4) the capture ran at loadavg 16-17, DIRECTIONAL by its
own admission. One more quiet-load pass on json_canon_bench, adjudicating canada and measuring the
fold-over-cursor against AU, closes it.
