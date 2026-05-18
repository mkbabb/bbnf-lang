# SK-V9 Alpha Hardening V4 - CH1 Correctness

Verdict: ACCEPT

Confidence: 97%

## Scope

Reviewed the unchanged SK-V9 Pass Alpha packet at commit `795bbbec`
(`docs(sk-v9-alpha): record V3 accept convergence cycle`) as the V4 CH1
second-cycle convergence check. This review rechecked citation correctness, row
counts, arithmetic, threshold floors, strict comparator discipline, closure of
V1/V2/V3 defects, drift from the V3-accepted packet, and the pre-dispatch
G-Alpha boundary.

The V3 consolidated packet is the first clean cycle: 6/6 ACCEPT, minimum
confidence 96%, zero open critical defects, and no orphan REVISE
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:8-22`).
It explicitly requires this V4 unchanged re-challenge before G-Alpha
presentation
(`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:34-38`).

## Findings

1. ACCEPT: No drift from the V3-accepted packet was found. `git diff
   --name-status 795bbbec -- restart/skinny/tranches/sk-v9` returned no paths
   before this CH1 report was created. The live V3 consolidated evidence remains
   aligned to the corrected packet: no SK-V9 `SPEC.md` or `DISPATCH-PROMPT.md`,
   no unresolved path references, and the V2 citation fold closed
   (`restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:24-32`).

2. ACCEPT: The V1 and V2 required folds remain closed. V1 required correctness,
   scope/cost, Lock 14/comparator, and regression folds
   (`restart/skinny/tranches/sk-v9/research/alpha-hardening/V1/CONSOLIDATED.md:25-68`).
   The current packet carries those folds in the Alpha cost matrix and
   split-before-dispatch cap
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:106-120`), the Lock 14 and
   grammar-aware telemetry gates
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:122-140`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:242-296`), and the Alpha-C /
   REDRESS 73 pre-blocks
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:303-328`). V2's only remaining
   defect was the complete-table citation range, and its required fold is now
   present
   (`restart/skinny/tranches/sk-v9/research/alpha-hardening/V2/CONSOLIDATED.md:24-35`;
   `restart/skinny/tranches/sk-v9/research/alpha-hardening/V3/CONSOLIDATED.md:26-32`).

3. ACCEPT: Row counts and arithmetic are still correct. Alpha-A records 38 main
   rows, 17 `parse_only`, 17 `direct_to_struct`, 4 `real_typed_struct`, 7
   `A / GO`, 31 `NO-GO`, and 38 deferred-strictness rows
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md:33-56`),
   matching `skinny/RESULTS.md:5-42` and the overall `N-direct / NoGo` close at
   `skinny/RESULTS.md:138-141`. A mechanical recount of the table and Alpha-A /
   Alpha-B row matrices found zero mismatches.

4. ACCEPT: Threshold arithmetic remains closed. The parse-row formula is still
   `max(ceil(SK-V8-open Track1 * 1.10), ceil(sonic_strict / 1.10))`, and every
   rendered parse threshold matches the formula
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:151-176`). Direct floors match
   `ceil(sonic_strict / 1.10)`
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:178-198`), and current GO-row
   maintain floors match the 2% maintain floor
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:200-210`). Alpha-E's selected
   retained, typed, and direct floors agree with those master values
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:104-108`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:203-207`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:287-291`).

5. ACCEPT: Citation discipline is clean. Alpha-B through Alpha-F now use
   `skinny/RESULTS.md:3-42` for complete-table claims
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:25-30`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:26-43`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-28`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:29-33`).
   A path/range scan over the packet plus V3 consolidated found 320 backticked
   `.md` references, 0 missing or out-of-range references after resolving local
   shorthand, and 10 intentional absent references to SK-V9 `SPEC.md` /
   `DISPATCH-PROMPT.md` as pre-dispatch boundary statements.

6. ACCEPT: Competitor deltas and strict comparator discipline are still safe.
   Alpha-B preserves already-rendered `sonic-rs strict`, `simdjson DOM`, and
   `yyjson` deltas, and recomputed `sonic-rs lossy`, `RapidJSON default`, and
   `serde_json` deltas match `(bbnf Track 1 Mbps / comparator Mbps - 1) * 100`
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:12-18`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:80-119`).
   The packet keeps lossy/permissive comparators, historical sidecars, absent
   sidecars, and output-plane mismatches as planning evidence only
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:40-60`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:121-149`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:220-240`).

7. ACCEPT: No open REDRESS or historical hardening defect is reopened. Apache/CITM
   remain source/product parity until fresh measured row-table evidence exists
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:212-218`), structural work remains
   blocked on the retained class/event grammar plus `ValueRef` proof
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:137-211`),
   direct digest work remains guard/control-plane only
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:236-324`),
   REDRESS 73 transfer remains pre-blocked
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:320-323`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:321-322`),
   and the prior Alpha-C ledger remains binding by reference
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:215-235`).

8. ACCEPT: The packet does not dispatch SK-V9 implementation. The synthesis
   states that V9 implementation is not dispatched, no Alpha `SPEC.md` or
   `DISPATCH-PROMPT.md` is created, and downstream S-P3 owns any future wave plan
   only after G-Alpha closes
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:5-9`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:63-75`;
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:330-335`). HANDOFF and Alpha-F
   preserve the same boundary
   (`restart/skinny/tranches/sk-v9/HANDOFF.md:5-8`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:67-77`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:107-113`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:11-13`;
   `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:103-105`).

## Required Folds

None for CH1.

## Blockers To G-Alpha

No CH1 blocker remains. From the CH1 lane, this V4 unchanged re-challenge is the
second clean cycle after V3. G-Alpha still requires the full V4 consolidation to
record the required ACCEPT threshold, zero open critical defects, no orphan
REVISE, no SK-V9 dispatch artifact, and then explicit user `G-Alpha closed`
sign-off (`restart/prompts/pass-contracts/PASS-ALPHA.md:167-182`).
