---
lens: CH3 REGRESSION
cycle: V6
pass: SK-V18 T-P1 totality excavation
reviewer: adversarial (challenge)
generated_at: 2026-06-01
targets: 1D-skinny-lessons.md, 1E-locks-evidence.md (+ spot-checks of 1F-coherence-scan.md, 1F-anti-pattern.md)
verdict_convention: REJECT admissible ONLY when an inventory states something FALSE on disk + live falsifying path:line; a self-falsified suspicion is an ACCEPT
---

# CH3 REGRESSION — V6 Verdict

LENS: no inventory re-opens a route already rejected in `skinny/REDRESS.md`; the
rejected-route pre-block list is correctly identified by 1D AND 1E; no admitted
REDRESS row is mis-catalogued as unimplemented.

Method: read 1D + 1E end-to-end; re-grounded every load-bearing REGRESSION
path:line on disk (REDRESS item spans 246/247/51/53; admit spans 249-252; ledger
end; SK-V18 SPEC G2/G4/G6 admissible framing; dual-witness requirement in
PASS-1-EXCAVATION). Every spot-verified claim matches disk.

## Findings (enumerated)

### F1 — Pre-block REJECT items 246 / 247 verify on disk. ACCEPT.
`skinny/REDRESS.md:6184` = "SK-V14 W11T Parse-Only Structural Stream Reject",
item 246 closes `G-SK-V14-W11T-...` as REJECT — a structural-stream parse_only
DRIVER (second substrate over a retained structural stream), reverted post-measure.
`:6230` = item 247 "W11V Parse-Only String64 Reject", a bespoke 64-byte aarch64
JSON string-special mask, reverted. Both 1D (`:168-169`) and 1E (`:161`) catalogue
the EXACT rejected shape. The SK-V18 SPEC admissible counterpart (G4 `Cursor` = a
VIEW over the existing Tape; G2 = a SHARED grammar-neutral primitive) is
materially distinct from each rejected shape. No re-open.

### F2 — Item 51 span `742-768` and item 52 exclusion verify. ACCEPT.
Item 51 ("SK-V5 event-cursor redress ... is REJECTED", `JsonEventCursor`) starts
at `:742`; prose ends at `:767` ("...no aux projection column are admissible");
`:768` is the blank block separator; `:769` opens item 52 ("SK-V5 baseline
reassay"), a samply re-measurement that is NOT a rejected route. 1D's
CH3-V3-004/CH3-V2-005 narrowing of the item-51 sub-span to `742-768` (excluding
item 52) is CORRECT on disk. The block-span convention legitimately includes the
`:768` separator. No regression in the off-by-one correction carried across V2-V5.

### F3 — Item 53 span `784-813` verifies. ACCEPT.
Item 53 ("SK-V5 structural-mask parser-local cursor is REJECTED",
`JsonStructuralCursor`) opens at `:784`; prose ends at `:813` ("...non-canonical
unless a future before/after row overturns this measurement"); `:814` blank before
item 54. 1D (`:171`) and 1E (`:161`) both land item 53 = `784-813`. Correct.

### F4 — Admitted REDRESS rows are NOT mis-catalogued as unimplemented. ACCEPT.
The decision-spine / lowerer admits cited by 1D D-10 / G-3 / G-13 and 1E L10 /
D-1E-V5 verify as genuine ADMITs, correctly graded impl_exceeds_spec / PROVED:
- Item 249 `SK-V15 W7 Decision Engine Spine Admit` (`:6326`) — `egraph_rewrite_count`
  sourced from `RunReport.total_applied`, falsifiable CSP capacity. 1D cite
  `REDRESS.md:6326-6354` accurate.
- Item 250 `W8` (`:6356`), Item 251 `W9 All-Five Gate` (`:6382`) — all five
  lowerers as operation-plan renderers via shared `BackendExpr`. 1D cite
  `REDRESS.md:6356-6414` (straddling W8+W9) accurate.
- Item 252 `W10 FNV Quarantine` (`:6416`) — matches G-5 surface (a): bench-side
  FNV quarantine clean/KEEP.
None of these admits is anywhere recorded as "unimplemented". The unimplemented
verdicts in 1D/1E attach to DISTINCT live residuals (generator-absent, emitter
fork, phantom <G>, x86, gate-by-exclusion, metalang leak) — orthogonal to the
admitted rows.

### F5 — Dual-witness requirement satisfied. ACCEPT.
PASS-1-EXCAVATION.md mandates the pre-block be "correctly identified by 1D and 1E".
1E `:161` independently enumerates the same four items (246/247/51/53) with matching
spans and the same admissible-vs-rejected firewall; 1E's CH3-V4-005 note explicitly
binds the second witness. Both witnesses land 51=`742-768`, 53=`784-813`, 246=`6184-6219`,
247=`6230-6260`. Concordant.

### F6 — No SK-V18 wave re-opens any reject (SPEC cross-check). ACCEPT.
SK-V18 SPEC framing aligns with the admissible side of each pre-block:
- G4: `SPEC.md:1254` "The `Cursor` trait is a view ... No second substrate (Lock 1)";
  `:1623` REJECTS "a second substrate; an eager value tree" — does NOT re-open item 246.
- G2: `SPEC.md:932,990` shared grammar-parameterized `css_balanced_component_scan`;
  `:1621` REJECTS "a NEUTRALLY-named CSS-only primitive" and "re-deriving the scan
  into 7 byte-identical files" — does NOT re-open item 247.
- G6: `SPEC.md:1300-1302,1329` "retarget the existing checkasm-gated `bbnf-simd`
  kernel ... onto the recursive shell of `find_component_delim`" — a retarget of the
  EXISTING in-loop shell, NOT a parser-local second cursor — does NOT re-open 51/53.
1D's predicate that the SPEC does not itself cite these items is verified:
`rg 'W11T|W11V|structural.stream|event.cursor|JsonEventCursor|JsonStructuralCursor'
restart/skinny/tranches/sk-v18/SPEC.md` = 0. The pre-block legitimately discharges
a burden the dispatch chain leaves unmet.

### F7 — U-5 / ledger-end completeness caveat is sound. ACCEPT.
The committed ledger ends at item 253 `SK-V15 W11 Close Reconciliation Admit`
(`:6446`, total 6465 lines) — verified. 1D/1E correctly scope the pre-block to the
captured ledger and flag SK-V16/V17 rejected routes as structurally invisible
(both tranches exist on disk; their rejects are not in REDRESS.md). Items 246/247
are SK-V14-origin (`G-SK-V14-W11T/W11V`) but appear late and item-number-monotonic
(246 < 248 < 253) because the SK-V14 W11T/W11V rejects were recorded in the same
ledger sweep that closed SK-V15. No internal inconsistency between "SK-V14-origin
items" and "ledger ends at SK-V15 W11".

### F8 — 1F-anti-pattern OnceCell row does NOT re-open a reject. ACCEPT.
The totality-tree `OnceCell<StructuralIndex>` probe is Lock-1-classified as the
ADMISSIBLE `generated_function` per-parse class (`&mut ScanState`, NOT cross-call),
explicitly distinguished from the REJECT `retained-across-call-boundary` class
(`LOCKS.md:139-149`). It is flagged as a SK-V19-adoption reconcile burden, NOT a
re-opened skinny reject (items 51/53 are about the SKINNY parser-local cursor; the
totality probe is a separate tree fenced as per-parse scratch). Correct disposition;
no regression.

## Reject hypotheses tested and self-falsified (per convention → ACCEPT, NOT reject)
- Hypothesis: item 51 sub-span `742-768` is off by one (should be `742-767` or
  `742-783`). FALSIFIED on disk: `:767` is last prose, `:768` is the block
  separator the span convention includes, `:769-783` is the distinct samply item 52.
  The inventory is CORRECT → ACCEPT (F2), not reject.
- Hypothesis: an admitted lowerer row (249-252) is mis-graded unimplemented.
  FALSIFIED: every admit verifies as ADMIT and is graded PROVED/impl_exceeds_spec
  by both 1D and 1E. The inventory is CORRECT → ACCEPT (F4), not reject.
- Hypothesis: a SK-V18 wave (G2/G4/G6) re-opens a rejected shape. FALSIFIED: SPEC
  `:1621/:1623` explicitly reject the second-substrate / bespoke-mask / neutral-CSS-only
  shapes; the moves retarget/decorate the existing single-substrate leaf. The
  inventory is CORRECT → ACCEPT (F6), not reject.

## Verdict
Under the REGRESSION lens, the inventories are SOUND. No inventory re-opens a
REDRESS-rejected route; the four-item pre-block (246/247/51/53) is correctly
identified by BOTH 1D and 1E with disk-accurate spans; no admitted REDRESS row is
mis-catalogued as unimplemented. Every load-bearing path:line verifies live. No
nit rises to a T-P2-misleading REVISE; no inventory states anything false on disk,
so there is no admissible REJECT. Honest tally: all findings ACCEPT. This lens has
reached a clean pass.

TALLY accept=8 revise=0 reject=0
