# SK-V12 W4 A6 - Close, REDRESS, And W5 Routing

Scope: W4 research A6. This artifact owns only the close/redress/routing read
of W4 and W5. It edits no source and does not dispatch W4 redress.

Read set:

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 0.1, Section 9, Section 10.
- `restart/skinny/tranches/sk-v12/HANDOFF.md` Section 3, Section 5, Section 7.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md` D1-D6.
- `skinny/REDRESS.md` REDRESS 125.
- `skinny/RESULTS.md` current head.

## Current Close State

REDRESS 125 closes W1b-2b as `PASS-ADMIT-CANDIDATE`, not final campaign close
and not `RESULTS.md` movement. The measured CSS row is
`css_l4/declaration_values/direct_to_struct/main` on the
`css_l4_declaration_value_fact_stream` output plane:

- Track 1: `429.34420791225705 Mbps`.
- lightningcss same-plane comparator: `168.92962215656692 Mbps`.
- USER PIN threshold: `169.92962215656692 Mbps`.
- Margin: `259.41458575569015 Mbps`.
- Strict retained fact streams are byte-identical and gate-consumed.
- JSON guard command returned status 0 and `RESULTS.md` was unchanged.

This satisfies the USER PIN D2 numeric CSS bar, but Section 0.1 ADMIT still
requires W5-level reconciliation: zero aarch64 orphans, JSON guard disposition,
`RESULTS.md` movement, REDRESS agreement, and close-document agreement.
`skinny/RESULTS.md` currently remains the JSON result surface; it has no
`css_l4`, `nonjson`, or `lightningcss` row. W5 must therefore promote or route
the CSS candidate explicitly.

## W4 Outcome Records

W4's exit gate is `G-W4-ASM-GEN-CONSUMER`. W4 is not a generic close wave; it
has two concrete duties: attempt one selected ASM-gen/CSS-consumer route and
dispose the five production aarch64 orphans:

- `bitmap_prefix_xor_64`
- `bitmap_next_set_bit`
- `bulk_emit_positions_64`
- `byte_context`
- `cache_hints`

For every W4 disposition, the REDRESS entry must include a five-row orphan
table with one of: consumed, removed, or `inventory_demoted_with_evidence`.
An unresolved production orphan prevents Section 0.1 ADMIT and FIXPOINT.

### ADMIT / `BEHAVIOR-PASS-CSS-ADMIT`

W4 may record `BEHAVIOR-PASS-CSS-ADMIT` only when all of these are true:

1. A fresh post-W4 CSS measurement still has Track 1 strictly greater than
   `lightningcss_mbps + 1` on the same corpus, same output plane, same host,
   and strict equality semantics. W4 may rely on the W1b-2b row only as the
   candidate baseline; the W4 gate must consume the current post-W4 state.
2. The selected primary ASM-gen candidate has a scalar reference, strict
   checkasm/parity, same-host microbench proof, same-wave CSS or JSON-guard
   consumer, and Lock 16 evidence.
3. REDRESS cites adjacent historical ASM rejects when applicable
   (REDRESS 88/89/90) and names the material differential.
4. JSON guards hold, or any miss is recorded as a measured REDRESS demotion.
5. The five-orphan table reaches zero production orphans.

The W4 REDRESS entry should then name the next REDRESS id available at dispatch
time, the selected candidate, the post-W4 CSS/lightningcss numbers, JSON guard
state, Lock 14 and Lock 16 state, patch ownership, and final orphan inventory.
W5 can use this as the ASM/orphan leg of PASS-ADMIT.

### MEASURED-REJECT

W4 records `MEASURED-REJECT` when the selected candidate misses or regresses
but the falsification is complete: scalar reference, checkasm/parity,
microbench, same-wave consumer, strict equality/parity, and REDRESS evidence
all exist. The source patch is reverted and saved at
`/tmp/skv12-waveW4-rejected.patch`.

This is a valid W5 entry disposition, but it is not automatically campaign
close. W5 can still PASS-ADMIT only if W4 also leaves the production orphan
set at zero by removal or evidence-backed inventory demotion, and the CSS
ADMIT candidate from W1b-2b still validates in the close report. If the selected
reject leaves any production orphan unresolved, W5 must route SK-V13 or seek a
later in-tranche orphan-disposition wave; it cannot close honestly.

### BLOCKED

W4 records `BLOCKED` only under the SPEC Section 9 blocked shape: W2 fails and
no non-SIMD ASM-gen candidate can legally dispatch. On current REDRESS state,
W2 is already closed as REDRESS 122, so this should not be the expected W4
outcome unless new entry-gate drift is discovered.

A BLOCKED W4 record must name the failed entry condition, state that no source
patch was attempted, preserve or create only the required rejected-patch
artifact if the local protocol demands it, and route the unresolved ASM/orphan
work to W5. W5 cannot PASS-ADMIT or PASS-FIXPOINT with unresolved production
orphans.

## Is W3 Required?

No, not for the current ADMIT path. SPEC Section 10 says W3 has disposition
only when closing as FIXPOINT or when no prior CSS row satisfies ADMIT. REDRESS
125 records a prior CSS row that satisfies the USER PIN numeric ADMIT bar as a
`PASS-ADMIT-CANDIDATE`.

Therefore W4 and W5 should not block PASS-ADMIT on a new W3 union attempt. W3
becomes required only if W5 cannot close as PASS-ADMIT and attempts
PASS-FIXPOINT instead, because Section 0.1 FIXPOINT requires a new measured
union-substrate implementation attempt with material differential from
REDRESS 96/97/98.

## W5 Reconciliation Duties

W5 must reconcile, in order:

1. Confirm W0, W1a, W2, W1b-1, W1b-2a, W1b-2b, and W4 all have evidence-backed
   dispositions.
2. Consume the final CSS row and report the CSS Track 1 vs lightningcss number.
   REDRESS 125 is candidate evidence; W5 must decide whether it is promoted to
   final PASS-ADMIT after W4 state is included.
3. Move `skinny/RESULTS.md` if PASS-ADMIT closes. Current `RESULTS.md` has no
   CSS L4 row, and REDRESS 125 explicitly deferred that movement.
4. Record JSON guard state: held, or measured REDRESS demotion. `parse_only`
   remains diagnostic-only and cannot count toward admission.
5. Record final orphan state from W4. Zero production orphans are mandatory for
   both ADMIT and FIXPOINT.
6. Record ASM-gen disposition from W4: admitted, measured-rejected with
   complete evidence, or blocked with the exact legal blocker.
7. Record W3 as intentionally not required for PASS-ADMIT on the REDRESS 125
   candidate path. If W5 instead claims FIXPOINT, W3 must have a fresh measured
   implementation attempt in REDRESS.
8. Align `SYNTHESIS.md`, `SPEC.md`, `HANDOFF.md`, `DISPATCH-PROMPT.md`,
   `skinny/RESULTS.md`, and `skinny/REDRESS.md`.
9. If ADMIT or FIXPOINT holds, materialize the SK-V12 campaign close document.
   If neither holds, W5 must route SK-V13 with explicit blockers and the
   campaign remains open.

## Routing Conclusion

The clean W4/W5 path is:

1. W4 selects one ASM-gen candidate, proves it by microbench/checkasm/parity,
   wires a same-wave consumer, measures CSS/JSON, and disposes all five
   orphans.
2. If the W1b-2b CSS ADMIT candidate still validates and the orphan set is
   zero, W5 may close SK-V12 as PASS-ADMIT without W3.
3. If W4 cannot reach zero production orphans, W5 must route the unresolved
   orphan/ASM work forward; it cannot use the W1b-2b numeric win alone as
   campaign close.
4. If the campaign chooses FIXPOINT instead of ADMIT, W3 becomes mandatory and
   must record a fresh measured union-substrate attempt in REDRESS.
