# SK-V11 S-P3 V2 CH3: Regression, REDRESS, And Pre-Blocks

Pass: S-P3 CHALLENGE. Cycle: V2.
Date: 2026-05-20.
Scope: CH3 only. This reviews whether the V2 packet preserves REDRESS
pre-blocks after the W1 split and row/budget folds, keeps W3/substrate and
parse-only routes retired, and leaves guard/revert rules fail-closed.
Output: this file.
Disposition: ACCEPT.

## Standard

CH3 accepts only if the packet satisfies four obligations:

1. The W1 split and revised row/budget caps do not reopen a REDRESS-blocked
   route or weaken a material-differential requirement.
2. W3 union/event/class-column substrate, parallel substrate, sidecar, and
   parse-only SOTA routes remain hard retired.
3. Existing admitted direct and typed rows cannot silently demote while direct
   residual rows are reclaimed or rejected.
4. Revert rules fail closed: a miss reverts the source/gate/report slice and
   records REDRESS evidence rather than becoming a scope cut or paper close.

## Findings

1. The W1 split preserves the pre-blocked route ledger. V2 splits V1 W1 into
   W1a non-JSON gate/report schema and W1b generated non-JSON baseline plus
   independent oracle. W1a is harness-only, rejects missing or producer-only
   non-JSON telemetry, and cannot create generated baseline authority or move a
   row. W1b creates exactly one generated CSS/Sheets/BBNF-self baseline row and
   oracle, but cannot land an intervention or admit a row. W2 consumes the W1b
   baseline and is explicitly forbidden from creating the first measurable
   non-JSON row. That fold fixes the V1 budget issue without weakening the
   P3-E blocks on JSON-provider proof, hand-only non-JSON proof, hidden
   directives/BIR variants, coupled oracle, generic JSON policy, or producer-
   only telemetry.

2. The revised bracket does not create a regression escape hatch. V2 records
   W0, W1a, W1b, and W2-W9 as 11 waves, leaving one spare split before the
   skinny `> 12` escalation rule. W8 source work must split as W8a and consume
   that spare slot; otherwise W8 remains docs/gate/result accounting. This is
   compatible with CH3 because the spare split is constrained by CHALLENGE,
   owner paths, row gates, and REDRESS preservation. It does not let a second
   rescue wave silently exceed the bracket.

3. Row-count and budget changes preserve pre-blocks. W3 now selects one or two
   numeric rows by default unless same-host microbench evidence justifies more.
   W4 stays capped to a selected control subset, W5 is capped to one
   string/key consumer and at most two target rows, W6 stays on selected
   Unicode/escape rows, W7 requires fresh post-W6 output-sink profile evidence,
   and W8 reconciles residual rows only after W3-W7 dispositions. These caps
   reduce blast radius while retaining the same REDRESS bars: material
   differential, same-wave product consumer, scalar/parity proof, row floor,
   guard block, and revert protocol.

4. W3/substrate remains retired, not renamed. REDRESS 96 and 97 measured
   faithful class-column/vector and streaming-cursor union-substrate
   implementations, both correctness/parity green, and both missed every W3
   target plus maintain rows. REDRESS 98 retired `G-W3-UNION-SUBSTRATE`, and
   REDRESS 102 firewalled parse-only movement and W4-through-W3 dependency.
   V2 preserves those facts in SPEC close condition 7, non-negotiables, W4
   pre-blocks, the hard pre-block ledger, P3-B/P3-C/P3-E, and
   `DISPATCH-PROMPT.md`. No V2 wave may dispatch a retained class column,
   structural-position vector, `UnionTape`, streaming cursor, class lane,
   parser-owned projection, sidecar producer, or W4 cascade-lock through W3.

5. Parse-only remains diagnostic. The outcome enum keeps `S`, `L`, and
   `N-direct` as non-admissions, and `S` is explicitly diagnostic/substrate
   guard only. `gate-json` must fail closed on parse-only SOTA claims and W3
   reopen claims. W0 locks the parse-only surface with no row movement, W3
   blocks direct admission from parse-only or retained-materialization counters,
   W8 admits W0-clamped rows only with W3-W8 measured provenance, and W9 closes
   only when parse-only remains diagnostic and W3 remains closed.

6. Guard floors are still fail-closed. SPEC §0.5 carries exact maintain floors
   for the 4 direct `A / GO` rows and 7 typed `A / GO` rows, using the P3-C
   formulas from SK-V11-open and same-run strict comparator evidence. Behavior
   waves that touch direct, typed, generic, generated, report, or gate surfaces
   include guard floors in their exit gates or revert conditions. W8 and W9
   require existing direct and typed guards to satisfy §0.5 before fixpoint or
   close.

7. Revert rules remain hard enough. W1a reverts gate/report/fixtures on schema
   weakness; W1b reverts codegen/bench/gate/report baseline changes on oracle
   coupling or missing evidence; W2-W7 revert their source/generated/SIMD/
   bench/gate/report/RESULTS slices on row-floor miss, parity failure, guard
   regression, Lock 1/14 leak, missing same-wave consumer, missing source
   delta, or policy leak. W8 preserves per-row measurements in REDRESS on
   unresolved rows. W9 reverts close docs until contradictions are resolved and
   cannot paper over missing dispositions.

## Accepted Facts

- The V2 W1a/W1b split preserves, rather than weakens, the CH3 pre-blocks.
- Row and budget caps narrow implementation blast radius without admitting any
  REDRESS-adjacent rename-only route.
- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate and
  parse-only SOTA routes remain hard retired.
- Guard floors and revert protocols prevent silent regression of existing
  direct and typed admitted rows.
- Remaining direct residuals can close only by strict both-track measurement or
  by per-row measured REDRESS uncloseable proof.

## Final Judgment

ACCEPT. The S-P3 V2 packet preserves CH3 regression discipline after the W1
split and budget folds. It keeps REDRESS 96/97/98 and 102 binding, keeps
parse-only diagnostic, keeps W3/substrate routes closed, and requires every
row-moving or source-touching miss to fail closed through revert plus REDRESS
evidence.
