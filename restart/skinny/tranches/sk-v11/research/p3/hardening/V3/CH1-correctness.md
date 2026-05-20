# SK-V11 S-P3 V3 CH1: Correctness And Measurable Row Gates

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V3.
Date: 2026-05-20.
Lens: CH1 correctness.
Disposition: ACCEPT.

## Scope

This lens checks the V3 packet against the S-P3 correctness contract: shortlist
traceability, measurable falsifiability gates, SK-V11-open baseline authority,
strict comparator discipline, direct/typed guard-floor consistency, and the V2
CH1 required folds. It edits no source.

## Findings

1. ACCEPT: V2 stale typed guard floors are folded into one authority.

   V2 required replacing the stale P3-A typed guard floors with `twitter 17385`,
   `citm_catalog 29928`, `apache_builds 8308`, `github_events 11633`,
   `update_center 11613`, `mesh 9214`, and `marine_ik 11552`
   (`research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:37-41`).
   V3 P3-A now uses exactly those Track 1 floors in the opening typed guard
   table (`research/p3/p3a-candidate-shortlist.md:59-67`), the numeric guard
   candidate (`research/p3/p3a-candidate-shortlist.md:261-265`), the typed guard
   candidate (`research/p3/p3a-candidate-shortlist.md:389-395`), and the §3
   summary (`research/p3/p3a-candidate-shortlist.md:411-412`). The same values
   appear in P3-C (`research/p3/p3c-falsifiability-gates.md:151-159`) and SPEC
   §0.5 (`SPEC.md:153-161`).

2. ACCEPT: non-JSON baseline ownership is W1b/W2, not W0 or P3-D.

   V2 required removing the P3-A summary claim that W0/P3-D could create the
   non-JSON performance floor (`research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:42-44`).
   V3 P3-A now says W1a creates only the gate/report lane, W1b creates the
   baseline row, and W2 admits only at `ceil(W1b_css_baseline_mbps * 1.01)`
   (`research/p3/p3a-candidate-shortlist.md:351-357`,
   `research/p3/p3a-candidate-shortlist.md:411-412`). P3-C binds the same split:
   W1a has no baseline authority, W1b creates exactly one generated non-JSON
   baseline plus oracle, and W2 may not create the first non-JSON baseline
   (`research/p3/p3c-falsifiability-gates.md:78-80`). SPEC repeats that W1a
   claims no generated baseline, W1b creates the baseline, and W2 consumes it
   with the 1.01x rounding rule (`SPEC.md:308-319`, `SPEC.md:357-367`,
   `SPEC.md:411-419`). P3-D remains schema binding only and does not set row
   thresholds (`research/p3/p3d-telemetry-schema.md:224-227`).

3. ACCEPT: W5 Unicode wording is residual monitoring, not a plain-string guard
   claim.

   V2 required removing W5 task wording that treated residual Unicode rows as
   guards for a plain-string target (`research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:45-47`).
   V3 SPEC now says to monitor Unicode residual rows when the selected target is
   plain-string and keeps them as floor-bearing W6/W8 residuals unless selected
   in W5 (`SPEC.md:561-567`). The W5 exit gate explicitly says
   `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` are not admitted
   guards when unselected (`SPEC.md:569-581`), and P3-B says the same for the W5
   measurable gate (`research/p3/p3b-wave-sequencing.md:101-110`).

4. ACCEPT: wave gates are measurable and aligned with SPEC/P3-C arithmetic.

   The contract requires named rows, concrete Mbps thresholds, strict comparator
   discipline, and no unmeasurable prose gates (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110-115`,
   `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:270-273`;
   `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112-125`). V3 P3-C
   carries named gates for W0-W9 (`research/p3/p3c-falsifiability-gates.md:75-87`),
   direct residual floors from `ceil(sonic-rs strict direct / 1.10)`
   (`research/p3/p3c-falsifiability-gates.md:106-126`; `SPEC.md:116-134`),
   direct guard floors from the max rule (`research/p3/p3c-falsifiability-gates.md:128-141`;
   `SPEC.md:136-146`), and typed guard floors from the Track 1 max rule plus
   Track 2 oracle maintain rule (`research/p3/p3c-falsifiability-gates.md:143-159`;
   `SPEC.md:148-161`). These values recompute correctly from the SK-V11-open
   rows in the goalset (`SYNTHESIS.md:106-147`).

5. ACCEPT: CH1 strictness and dependency gates remain coherent.

   Direct admission requires generated Track 1, independent Track 2, strict
   same-run direct comparator, matching output plane, and same-wave gate
   consumption (`research/p3/p3c-falsifiability-gates.md:161-185`;
   `SPEC.md:246-251`). The dispatch prompt points implementers back to SPEC
   §0.4/§0.5 for floors and preserves the W0-clamped non-admission rule and
   non-JSON generated-parser rule (`DISPATCH-PROMPT.md:149-163`). The bracket is
   W0, W1a, W1b, and W2-W9: 11 waves with one spare split before the >12-wave
   escalation rule (`SPEC.md:185-211`; `DISPATCH-PROMPT.md:49-72`).

## Verdict

ACCEPT. V3 folds the V2 CH1 findings and presents a measurable, strict,
baseline-consistent wave packet for CH1 purposes. No source edits were made.
