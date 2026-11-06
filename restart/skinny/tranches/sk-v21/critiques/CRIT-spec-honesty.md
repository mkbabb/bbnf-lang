# CRIT — spec-honesty: SPEC-DRAFT-P1 read whole against the pass-0/1 corpus

Date: 2026-07-05. Posture: adversarial — every performance claim tested for cold/loadavg/same-plane
honesty, every resurrection tested against dig evidence, every SOTA comparator tested for honest
scope, every chronic-ledger item tested for disposition-or-escape. Verdict up front: the draft's
honesty posture is exemplary and its evidential grounding is near-complete. The `DIRECTIONAL`
discipline is applied consistently, comparison asymmetries are co-located at the claim site, and the
corrected forms of the two adjudicated conflicts (serde_json hot-path, Lock-14 double-artifact) are
consumed rather than the stale ones. The residual defects are three: one arithmetic slip in the
flagship perf sentence, one floor that is quoted two incompatible ways, and the structural fact that
no performance ranking is verified-cold yet — the last is honestly dispositioned, not escaped.

## What holds — the honesty is real

- **simdjson On-Demand non-transfer is stated, not elided.** SPEC:92-94 says On-Demand's margin
  "comes entirely from unread substructure and does not transfer to full materialization," faithfully
  consuming 22:44-49 (json2msgpack 0.87-0.91 GiB/s when nothing is skippable) and 22:83-87 (canada
  stage-2 1.847 vs stage-1 0.547 c/B). The comparator is honest: the win is scoped to number decode
  and per-value store, never structural skipping. Correct.
- **lightningcss eager-depth parity is honestly scoped.** SPEC:156-164 discloses bbnf's 27-unit TOML
  catalog vs lightningcss's 100+-variant typed CSSOM, the derived-catalog novelty, the Unparsed/Custom
  deferred escape hatch, and names minify/browserslist/nesting OUT of the mandate — all four faithful
  to 24:17-25,39-41. The CSS ">SOTA" sentence is qualified until TW3+TW4 close (SPEC:164), and the
  stale vendored comparator (alpha.71 / cssparser 0.33) is re-pinned before re-measurement (SPEC:165).
- **sonic-rs dual-path is benched-against, not benched-around.** SPEC:152-155 requires the
  eager-as-fold lane be measured against sonic-rs's dual-path direct-to-struct "where a fold-over-cursor
  could plausibly lose," verbatim to 23:59-64. The stronger claim is not asserted as won.
- **The serde_json correction is consumed.** SPEC:88-90 adjudicates the 19-vs-21 conflict for 21
  (hand-rolled decode, not serde_json), matching 31:110-125 and 21:95-108. Notably the SPEC does NOT
  lift report 23's own stale framing (23:93-94 still repeats "routes THROUGH serde_json::from_str",
  the pre-correction claim) — it navigates a corpus-internal inconsistency correctly.
- **Chronic ledger fully dispositioned.** All six C-items (35 §1) reach a disposition: C6 is TW0's
  literal first commit (SPEC:320), C1's AoS→SoA is reversed to AoS hardening resolving the N19
  blocking tension (SPEC:133-144 vs 35 N19), C3 right-sized to permanent-greedy + KernelCostTable
  (SPEC:180-185 vs 35 C3), C2/C4/C5 folded into named waves. No chronic item escaped.
- **Coordination seams closed with bbnf-side derivation.** The csp P0 (32 U2's open concern that
  severity was carried from csc411 docs) is resolved: report 36 derives the arm unreachable from every
  bbnf call path (ac3_from_variable gated on `Pruning::Ac3`, zero such selections repo-wide, 36:17-58),
  and SPEC:242-244 states "it gates nothing." The parse-that NO-GO (32 G3) is dated with two
  dispositive facts (37:9-51). U3 (the un-re-derived 1810 contamination) is explicitly barred from
  anchoring any regression budget (SPEC:312). The hand-asm standing-aim gap (32 PARTIAL) is priced to
  P5 with the honest 0-5% bound (SPEC:28-30,199-201).

## Defect 1 — the AU rate is arithmetically impossible as quoted (must fix)

SPEC:71 states the historical peak "AU: twitter 1967 MB/s, **0.615 cyc/byte**, 0.91× sonic." This is
wrong and self-refuting: 1967 MB/s on Firestorm at ~3.2 GHz is 3.2e9 / 1967e6 ≈ **1.63 cyc/byte**, and
report 09's own table gives exactly that — `| AU (3b8b757d) | 1967 | 1.626 | 0.91× |` (09:144). The
0.615 figure is the reciprocal (bytes/cycle) mislabeled as its inverse. Report 09 is internally
inconsistent (prose 09:12 says 0.615, table 09:144 says 1.626), and the SPEC propagated the impossible
prose figure instead of catching the contradiction. Load-bearing because this is the flagship "the
summit exists and its coordinates are recorded" sentence (09:423); a coordinate that is off by 2.6×
undermines the "recorded truth" framing. Fix to 1.63 cyc/byte and note report 09's internal error.

## Defect 2 — the JSON floor is quoted two incompatible ways (must collapse)

The draft names a "JSON floor" in two mutually exclusive senses and gates on both:
- **AU-relative** (SPEC:72 "The JSON floor is AU-relative"; SPEC:348 TW3 close "meets the AU-relative
  floor cold"). AU's 1967 was measured under the old warm harness regime, was NOT among verifier 30's
  fourteen re-derived claims, and sits in the era whose "BEAT-sonic" numbers were shown contaminated by
  a modifier-skipping parser (30 claim 6; 05:26). You cannot gate a *cold* close against a *warm*,
  unverified, contamination-adjacent historical number — the categories do not compose.
- **TW0-baseline-relative** (SPEC:337,348 "no cold regression vs the TW0 baseline"). This is the only
  honest cold floor — freshly captured, quiet, same-plane.
The two coexist in the same close-gate sentence (SPEC:348 carries both "AU-relative floor cold" AND,
via the wave order, the TW0 baseline). Outcome: the wave-close is either un-meetable (any cold number
sits far below AU's warm 1967) or silently reinterpreted as the TW0 baseline, at which point the AU
reference is decorative. Synthesis must demote AU to a directional aspiration in prose and make the
TW0 fresh cold baseline the single binding floor for every no-regression clause.

## Defect 3 — no performance ranking is verified-cold (dispositioned, not escaped, but keeps it open)

Every absolute anchor the performance thesis rests on is directional and un-re-derived by the
adversarial verifiers: the 0.99× visitor geomean (09:179), the 0.89-0.94× AW prototype (07:120-122),
the AU 1967 peak (09:144). Verifier 30's fourteen re-derived claims are root-cause and resurrection
facts; none of these three throughput figures is among them. Report 21 establishes JSON has no
cold-per-parse harness at all (21:50-77, the 8349-vs-25448 warm/cold ~3× gap), and 32 U1 records that
the charter's own cold+quiet+samply discipline was never met in pass-0 (loadavg 8.97-10.66, JSON warm,
no flame — 21:9-29). The SPEC is honest about all of this: SPEC:11-13 declares every ranking
DIRECTIONAL, SPEC:95-100 makes the TW0 quiet re-capture and `json_canon_bench` the prerequisite for
ranking any lever, and §13 Q2 routes the 0.99× survival to prototype P2. The architectural conclusions
(RD>DTA, AoS>SoA) rest on within-run ratios, which 21:15,79-81 certifies robust even under load — so
freezing the architecture is defensible. But the lever PRIORITIZATION (P1 vs P2, highest-ROI) cannot
be claimed settled; it is an in-tranche TW0 discovery. This is a property of the loop's stage, honestly
dispositioned — but it is why the honesty item is not fully closed: the SPEC is honest that the
underlying measurement is unsettled, and the measurement resolves only inside TW0/P2, not before.

## Minor — scope-optimism residue (not an honesty defect)

The third-language empirical generalization that answers the no-overfit-JSON/CSS precept (§9, TOML)
lands only at TW6, the last wave; P7 de-risks it pre-freeze (SPEC:289-292,369), but TW1-TW5 are built
JSON/CSS-shaped and are exposed if P7 surfaces deep grammar-specific breaks. Mitigation exists (P7 is a
pass-2 prototype before freeze); the residue is that the generalization proof is scheduled latest.

## Convergence on spec-honesty: 84

Evidential grounding is near-complete; comparators are honestly scoped; chronic items and coordination
seams are all dispositioned; the DIRECTIONAL discipline is consistent and correct. The item is not at
100 because Defect 2 is a genuine plan ambiguity a synthesis pass must resolve (which floor binds), and
Defect 1 is a factual error in the flagship perf sentence. Both are editorial refinements, not
re-research — so this is comfortably above the 70 re-loop line. Defect 3 is inherent to the pass stage
and honestly deferred to TW0/P2; it caps rather than reopens.
