# SK-V11 S-P3 V1 CH3: Regression, REDRESS, And Pre-Blocks

Pass: S-P3 CHALLENGE. Cycle: V1.
Date: 2026-05-20.
Scope: CH3 only. This reviews whether the SK-V11 S-P3 packet reopens retired
REDRESS routes, whether W3/substrate/parse-only routes are actually closed, and
whether guard floors plus revert rules prevent silent regression.
Output: this file.
Disposition: ACCEPT.

## Standard

CH3 accepts only if the packet satisfies three obligations:

1. Every REDRESS-adjacent route is either hard pre-blocked or carries a material
   differential, same-wave consumer, row gate, and revert rule before redress.
2. The SK-V9 W3 union/event/class-column substrate family and SK-V10 parse-only
   firewall are not live implementation dependencies under new names.
3. Existing admitted direct and typed rows cannot silently demote while direct
   residuals are reclaimed or rejected.

## Findings

1. The pre-block ledger is complete for the SK-V11 wave surface. P3-E carries
   a per-wave ownership map, candidate-to-REDRESS ledger, hard-block list,
   material-differential-only table, proof-only inventory list, per-wave
   pre-blocks, and "routes no SK-V11 wave may reopen" section. The live
   `SPEC.md` and `DISPATCH-PROMPT.md` preserve the same hard blocks: W3
   union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
   parse-only SOTA close, sidecar/parallel substrate producers, direct-vs-typed
   relabeling, generic JSON policy, x86 implementation work, PMULL/CTZ default
   hot paths, string materialization replays, numeric fallback policy rewrites,
   object next-key/value-byte carry, diagnostic producers, and new public
   directive/BIR/substrate surfaces.

2. The SK-V9 W3 substrate route is truly retired, not merely deferred. REDRESS
   96 implemented the class-column plus move-consumed structural-position
   vector route, passed correctness/parity checks, and then missed every W3
   must-improve row plus every W10b maintain row. REDRESS 97 removed the vector
   and tried the allocation-free streaming cursor, also correctness/parity
   green, and again missed every target and maintain floor. REDRESS 98 retires
   `G-W3-UNION-SUBSTRATE` and rejects the remaining class-lane-only route as
   paper-close. SK-V11 preserves that retirement in `SPEC.md` close condition
   7, non-negotiables, W4 pre-blocks, §13 hard pre-blocks, and the dispatch
   prompt's hard pre-block list.

3. Parse-only is firewalled. REDRESS 102 records that parse rows remain
   diagnostic `S / NO-GO` or `L / NO-GO`, rejects parse-only SOTA movement in
   the validator, and forbids W4+ from naming W3 as consumer or substrate
   dependency. SK-V11 repeats this in the close condition, comparator/outcome
   discipline, non-negotiables, `gate-json` fail-closed clauses, P3-D schema
   rejection reasons, P3-E hard blocks, SPEC §13, and dispatch prompt load-
   bearing facts. No SK-V11 wave can admit from parse-only throughput, PMU,
   cycles, structural-scan, masking, or lazy-materialization diagnostics.

4. Material-differential rules are adequate. P3-E explicitly separates hard
   blocks from material-differential-only routes and requires REDRESS citation,
   fresh antecedent, same-wave product consumer, scalar/parity proof, row
   thresholds, Lock 1/14 proof, and revert protocol. SPEC §13 repeats the
   binding rule that a narrower name is not a material differential. This closes
   the common regression path where a rejected route returns as a rename.

5. Guard floors prevent silent demotion. SPEC §0.5 lists exact direct and typed
   guard floors. P3-C §3.3 and §3.4 define how direct and typed guard floors are
   computed from SK-V11-open and same-run strict comparators, and every behavior
   wave W2-W8 includes guard-floor failure in its revert rule. W8 and W9 both
   require all direct residuals to be `A / GO` or have measured REDRESS
   uncloseable proof, while existing direct and typed guards must still satisfy
   §0.5. This is sufficient to prevent a row-table close that quietly trades
   away admitted wins.

6. Revert rules are fail-closed. W1-W8 each revert the relevant source,
   generated, bench, gate, report, and `RESULTS.md` slice on row-floor miss,
   oracle coupling, parity failure, guard regression, Lock 1/14 violation, or
   missing new source delta. W8 preserves per-row measurements in REDRESS
   instead of paper-closing residuals, and W9 cannot close while any W1-W8 wave
   lacks admitted/rejected/measured status.

7. Sibling-artifact drift does not create a CH3 defect. P3-A uses a planning
   shorthand of 99% maintain floors in one place, but P3-C explicitly owns the
   final guard formulas and the live SPEC adopts the P3-C/SK-V11-open floors.
   That is a superseded planning note, not a dispatched weaker gate. P3-B,
   P3-C, P3-E, P3-F, SPEC, and DISPATCH all share the W0-W9 wave ordering and
   do not repeat the SK-V9 P3 manifest inconsistency that caused the prior CH3
   REVISE cycle.

## Accepted Facts

- Pre-blocked routes are complete for the SK-V11 S-P3 V1 packet.
- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  routes are retired for SK-V11 dispatch authority.
- Parse-only rows are diagnostic only and cannot close SK-V11 SOTA.
- Guard floors and revert rules are sufficient to prevent silent regression of
  the 4 direct `A / GO` rows and 7 typed `A / GO` rows.
- Remaining direct residuals can close only by strict same-run both-track
  measurement or by per-row measured REDRESS uncloseable proof.

## Final Judgment

ACCEPT. The S-P3 V1 packet carries the REDRESS history forward as binding gate
law rather than background context. No SK-V11 wave silently reopens W3,
parse-only, sidecar/substrate, string proof-only, numeric fallback, object
carry, PMULL/CTZ, or generic-policy routes, and every row-moving wave has a
guarded fail-closed revert path.
