# SK-V11 S-P3 V4 CH1: Correctness And Measurable Row Gates

Pass: S-P3 Synthesis-Plan CHALLENGE.
Cycle: V4.
Date: 2026-05-20.
Lens: CH1 correctness.
Disposition: ACCEPT.

## Scope

This stability lens checks that the V4 packet preserves the V3-accepted S-P3
semantics and still satisfies CH1: shortlist traceability, named measurable row
gates, SK-V11-open baseline authority, strict comparator discipline, direct and
typed floor arithmetic, and wave dependency correctness. It edits no source.

## Findings

1. ACCEPT: V4 is a stability bump, not a semantic rewrite.

   V3 hardening returned 6/6 ACCEPT with zero open critical defects and zero
   open REVISE dispositions, but required one more clean cycle because V2 had a
   CH1 REVISE (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:11-23`).
   The V3 consolidation explicitly says no V3 lens requires a content fold and
   that V4 should preserve V3 semantics (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V3-CONSOLIDATED.md:45-47`).
   I compared `5bdee331..HEAD` over `p3a` through `p3f`, `SPEC.md`, and
   `DISPATCH-PROMPT.md`: the only changes are `V3` to `V4` cycle/status text,
   one `css_l4/... in V4` phrase, and P3-D's self-verdict cycle label. No gate,
   floor, row set, dependency, owner path, pre-block, or comparator language
   changed.

2. ACCEPT: shortlist candidates still trace to the accepted S-P2 pool and not
   to speculative surfaces.

   S-P2 converged with V2 and V3 both 6/6 ACCEPT, leaving C1-C7 as the parser
   primitive pool, C8 as benchmark/oracle or per-product host sink only, C9 as
   Lock-1/output-plane accounting only, and W3 union repair REDRESS-closed
   (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-CONVERGED.md:7-32`).
   V4 P3-A carries the same pool and drops movemask-only, C8, C9,
   `HEX_QUARTET_X4_PROOF`, cache hints, PMULL/CTZ, EOR3/BCAX, x86, parse-only,
   W3 substrate, and generic JSON-policy work as standalone candidates
   (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:15-20`,
   `restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:76-88`).
   P3-F repeats the same admissible pool and states the V4 SPEC is sequenced
   around product-plane consumers, not parse-only substrate repair
   (`restart/skinny/tranches/sk-v11/research/p3/p3f-spec-draft.md:18-38`).

3. ACCEPT: direct target floors and guard floors still recompute from W0.

   The SK-V11 goalset defines direct closure as strict same-run sonic-rs direct
   `ceil(sonic / 1.10)` on both generated Track 1 and independent Track 2, or a
   measured per-row uncloseable proof (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:41-44`,
   `restart/skinny/tranches/sk-v11/SYNTHESIS.md:101-124`). W0 freezes the same
   run id and residual table (`restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:11-20`,
   `restart/skinny/tranches/sk-v11/research/w0/W0-open-baseline.md:34-57`).
   Recomputing from W0 gives the V4 direct floors in P3-A, P3-C, P3-D, P3-F,
   and SPEC: `13740 / 10637 / 13403 / 10059 / 8675 / 7878 / 3737 / 8969 /
   2425 / 2588 / 3441 / 2658 / 3950`
   (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:22-41`,
   `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:106-126`,
   `restart/skinny/tranches/sk-v11/SPEC.md:116-134`).
   Recomputing direct guards from `max(floor(track * 0.98), ceil(sonic / 1.10))`
   gives `18191/17431`, `11028/9996`, `8759/9248`, and `2253/2182`; V4 P3-A,
   P3-C, and SPEC agree (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:43-52`,
   `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:128-141`,
   `restart/skinny/tranches/sk-v11/SPEC.md:136-146`). Recomputing typed guard
   floors gives the V3-corrected Track 1 set `17385 / 29928 / 8308 / 11633 /
   11613 / 9214 / 11552`, with the listed Track 2 oracle guards; V4 keeps those
   in P3-A, P3-C, and SPEC (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:54-67`,
   `restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:143-159`,
   `restart/skinny/tranches/sk-v11/SPEC.md:148-161`).

4. ACCEPT: V2 CH1 folds remain preserved in V4.

   V2 required three folds: replace stale typed guard floors, remove any claim
   that W0/P3-D creates the non-JSON performance floor, and stop calling
   residual Unicode rows plain-string guards (`restart/skinny/tranches/sk-v11/research/p3/hardening/HARDENING-S-P3-V2-CONSOLIDATED.md:35-47`).
   V4 preserves all three. P3-A uses the corrected typed floors and states P3-D
   binds fields while W1b creates the non-JSON baseline and W2 admits at
   `ceil(W1b_css_baseline_mbps * 1.01)` (`restart/skinny/tranches/sk-v11/research/p3/p3a-candidate-shortlist.md:389-412`).
   P3-D states it does not set row thresholds (`restart/skinny/tranches/sk-v11/research/p3/p3d-telemetry-schema.md:224-227`).
   SPEC W5 says Unicode residual rows are monitored and remain W6/W8 residuals
   unless selected, and the W5 exit gate says they are not admitted guards
   (`restart/skinny/tranches/sk-v11/SPEC.md:561-581`).

5. ACCEPT: measurable gates and strict comparator dependencies are still
   executable.

   CH1 requires named corpus rows, concrete Mbps thresholds, SK-V11-open
   baseline comparison, and strict-plane comparator deltas
   (`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:110-114`). V4 P3-C defines
   executable gates for W0-W9, including W1a field rejection, W1b exactly-one
   non-JSON baseline plus oracle, W2 CSS L4 intervention at the W1b 1.01x
   threshold, W3-W6 concrete direct row floors, W8 all-row fixpoint, and W9
   close reconciliation (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:75-87`).
   Its track/oracle section requires same-run strict direct comparator on the
   digest plane, typed oracle parity for typed rows, generated non-JSON direct
   or typed rows with independent oracle, and same-wave gate consumption
   (`restart/skinny/tranches/sk-v11/research/p3/p3c-falsifiability-gates.md:161-185`).
   The SPEC carries the same strict baseline, telemetry, and rejection rules
   (`restart/skinny/tranches/sk-v11/SPEC.md:20-59`,
   `restart/skinny/tranches/sk-v11/SPEC.md:103-114`,
   `restart/skinny/tranches/sk-v11/SPEC.md:246-251`).

6. ACCEPT: wave dependencies and dispatch authority remain correct.

   ORCHESTRATOR and PASS-3 require disposition folding and two consecutive
   >=95% ACCEPT cycles with no open critical defects or orphan REVISE before
   S-P3 can hand off (`restart/prompts/ORCHESTRATOR.md:112-123`,
   `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:153-166`). The V4 SPEC and
   dispatch prompt still forbid behavior source work before S-P3 convergence
   (`restart/skinny/tranches/sk-v11/SPEC.md:6-12`,
   `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:8-14`).
   The dependency order is stable: W1a gate/report before W1b baseline, W1b
   before W2 intervention, W2 before generic C1-C7 behavior waves, W3-W7 by
   dependency and attribution, W8 after row-moving dispositions, and W9 only
   after W8 close or measured proof (`restart/skinny/tranches/sk-v11/research/p3/p3b-wave-sequencing.md:78-90`,
   `restart/skinny/tranches/sk-v11/DISPATCH-PROMPT.md:65-72`). The bracket is
   still 11 waves with one spare split before the skinny >12 escalation rule
   (`restart/skinny/tranches/sk-v11/SPEC.md:185-211`).

## Verdict

ACCEPT. V4 preserves the V3-accepted semantics, keeps the V2 CH1 folds intact,
and still presents measurable, strict, SK-V11-open-baseline gates with correct
floors and dependencies. No source edits were made.
