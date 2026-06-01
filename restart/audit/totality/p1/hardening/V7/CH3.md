---
lens: CH3 REGRESSION
cycle: V7
pass: SK-V18 T-P1 totality excavation
reviewer: adversarial (challenge)
generated_at: 2026-06-01
targets: 1D-skinny-lessons.md, 1E-locks-evidence.md (+ spot-checks of 1F-anti-pattern.md, 1F-coherence-scan.md)
verdict_convention: REJECT admissible ONLY when an inventory states something FALSE on disk + live falsifying path:line; a self-falsified suspicion is an ACCEPT
prior_cycle: V6 CH3 = clean (accept=8 revise=0 reject=0)
---

# CH3 REGRESSION — V7 Verdict

LENS: no inventory re-opens a route already REJECTED in `skinny/REDRESS.md`; the
rejected-route pre-block list is correctly identified by 1D AND 1E; no admitted
REDRESS row is mis-catalogued as unimplemented.

Method: read 1D + 1E + 1F-anti-pattern end-to-end; re-grounded every load-bearing
REGRESSION path:line LIVE on disk this cycle — the four-item pre-block (246/247/51/53)
with exact header/prose/separator boundaries; the admitted spans (249-252 + J-2
W11W/W11A); the dual-witness mandate; the SK-V18 SPEC admissible-vs-rejected framing;
the `LOCKS.md:139-149` retention-class taxonomy backing the totality OnceCell carry.
Every spot-verified claim matches disk at the dirty tree HEAD.

## Findings (enumerated)

### F1 — Pre-block items 246 / 247 verify on disk, both REJECT. ACCEPT.
`skinny/REDRESS.md:6184` = "SK-V14 W11T Parse-Only Structural Stream Reject"; `:6187`
closes item 246 `G-SK-V14-W11T-...` as `REJECT` (a structural-stream parse_only DRIVER
= second substrate over a retained structural stream). Its REJECT prose ends at `:6219`;
`:6220` is blank; `:6221-6228` is a DISTINCT "W11U unicode_escapes Supersession Note"
(NOT a rejected route); item 247 header sits at `:6230`. Both 1D (`:168`) and 1E (`:161`)
cite item 246 = `6184-6219` — the span correctly ENDS at the last REJECT prose line and
EXCLUDES the intervening W11U section. Item 247 "W11V Parse-Only String64 Reject" header
`:6230`, item 248 (W11W Admit) header `:6262`; prose ends `:6260`. Both inventories cite
`6230-6260`. Disk-accurate. The SK-V18 admissible counterparts (G4 `Cursor` = VIEW over the
existing Tape; G2 = SHARED grammar-neutral primitive) are materially distinct. No re-open.

### F2 — Item 51 span `742-768`; item 52 correctly excluded. ACCEPT.
Item 51 "SK-V5 event-cursor redress ... is REJECTED" (`JsonEventCursor`) header `:742`;
prose ends `:767` ("...no aux projection column are admissible"); `:768` blank separator;
`:769` opens item 52 "SK-V5 baseline reassay" (a samply re-measurement under
`skinny/profile/skv5-event-redress/`, NOT a rejected route). Both 1D and 1E land
`742-768` (block span including the `:768` separator per the documented off-by-one
convention, item 52 excluded). The off-by-one correction carried V2-V5-V6 holds at V7. No regression.

### F3 — Item 53 span `784-813` verifies. ACCEPT.
Item 53 "SK-V5 structural-mask parser-local cursor is REJECTED" (`JsonStructuralCursor`)
header `:784`; prose ends `:813` ("...non-canonical unless a future before/after row
overturns this measurement"); `:814` blank; `:815` opens item 54. Both 1D (`:171`) and
1E (`:161`) land `784-813`. Correct.

### F4 — Admitted REDRESS rows NOT mis-catalogued as unimplemented. ACCEPT.
The decision-spine / lowerer / FNV admits cited by 1D (D-10, G-3, table row, J-2) and
1E (L10, D-1E-V5 rows) verify as genuine ADMITs, correctly graded PROVED/impl_exceeds_spec:
- Item 249 `SK-V15 W7 Decision Engine Spine Admit` (`:6326`) closes `ADMIT-W7`; records
  `egraph_rewrite_count` sourced from `RunReport.total_applied` (`:6335`) — falsifiable.
- Item 250 `W8` (`:6356`) `ADMIT-W8`; Item 251 `W9 All-Five Gate` (`:6382`) `ADMIT-W9` —
  the five lowerers as operation-plan renderers. 1D cite `REDRESS.md:6356-6414` accurate.
- Item 252 `W10 FNV Quarantine` (`:6416`) `ADMIT-W10` — matches G-5 surface (a).
- J-2 admit routes: W11W (`:6262`, `ADMIT`, cite `6262-6294`), W11A (`:5861`, item 231
  `ADMIT`, cite `5861-5881`) both verify.
None is anywhere recorded as "unimplemented". The unimplemented verdicts attach to DISTINCT
live residuals (generator-absent, emitter fork, phantom `<G>`, x86, gate-by-exclusion,
metalang leak) — orthogonal to the admitted rows. Item 246 is confirmed `REJECT` (`:6187`),
correctly catalogued as a rejected route, not an admit.

### F5 — Dual-witness mandate satisfied. ACCEPT.
`restart/prompts/totality/PASS-1-EXCAVATION.md:115-118` mandates the pre-block be
"correctly identified by 1D and 1E" (the verbatim CH3 charter). 1E `:161` independently
enumerates the same four items (246=`6184-6219`, 247=`6230-6260`, 51=`742-768`,
53=`784-813`) with matching spans and the same admissible-vs-rejected firewall, binding the
second witness (CH3-V4-005). Concordant with 1D `:166-171`.

### F6 — No SK-V18 wave re-opens any reject (SPEC cross-check). ACCEPT.
`rg 'W11T|W11V|structural.stream|event.cursor|JsonEventCursor|JsonStructuralCursor'
restart/skinny/tranches/sk-v18/SPEC.md` = 0 (RC=0, empty) — the SPEC does not cite these
items, so the pre-block legitimately discharges the burden the dispatch chain leaves unmet
(1D `:173`). The SPEC's anti-pattern rows explicitly REJECT each rejected shape:
- G4 row: "a second substrate; an eager value tree" REJECTED; admissible seam at
  `SPEC.md:87-97` ("ONE seam ... a thin `Cursor`/`DocumentView` micro-trait over the
  surviving `ValueRef<K>` ... may NOT LCD-flatten") — does NOT re-open item 246.
- G2 row: "a NEUTRALLY-named CSS-only primitive (FORCED to `css_balanced_component_scan`)"
  and "re-deriving the scan into 7 byte-identical files" REJECTED — does NOT re-open item 247.
- G6 row: "re-emitting the call site 7 ways (P3 re-fork); a test-only admission proof"
  REJECTED; admissible retarget at `SPEC.md:154-165` ("RETARGETED onto the scalar recursive
  shell of `find_component_delim` ... SHARED grammar-neutral runtime primitive the ...
  generated scan CALLS — NOT bespoke vector code re-emitted per-grammar") — does NOT re-open
  51/53. `SPEC.md:398` reaffirms "No second tape, no eager value tree, no parser-owned".

### F7 — U-5 / ledger-end completeness caveat is sound. ACCEPT.
The committed ledger ends at item 253 `SK-V15 W11 Close Reconciliation Admit` (`:6446`,
last prose `:6465`; `:6460` confirms "Pattern H count `67`" corroborating the LAC-1E-15
baseline). 1D (U-5) and 1E correctly scope the pre-block to the captured ledger and flag
SK-V16/V17 rejected routes as structurally invisible. Items 246/247 are SK-V14-origin yet
item-number-monotonic (246 < 248 < 253) because the SK-V14 W11T/W11V rejects were recorded
in the same ledger sweep that closed SK-V15. No internal inconsistency.

### F8 — 1F-anti-pattern OnceCell row does NOT re-open a reject. ACCEPT.
The totality `OnceCell<::simd_scan::StructuralIndex>` probe (`crates/core/src/grammar/
generated/json.rs:701`; `OnceCell::new()` `:711`; `ensure_structural_index` `:719`;
`scan_structural` `:732`) is Lock-1-classified as the ADMISSIBLE per-parse `generated_function`
class (`&mut ScanState`, NOT cross-call) per `LOCKS.md:139-149` (retention taxonomy:
`retained-across-call-boundary` is the REJECT class; this probe is per-parse), explicitly
distinguished from the skinny parser-local cursor of items 51/53. Breadth "8 of 9, all but
math" verifies on disk (`rg -c ensure_structural_index crates/core/.../math.rs` = 0; CH5-V3-003
correction holds). Fenced as a SK-V19-adoption reconcile burden, NOT a re-opened skinny reject.
No regression.

## Reject hypotheses tested and self-falsified (per convention -> ACCEPT, NOT reject)
- Hypothesis: item 246 span `6184-6219` wrongly absorbs the intervening W11U section or is
  off by one. FALSIFIED on disk: `:6219` is the last REJECT prose line, `:6220` blank,
  `:6221-6228` is the DISTINCT W11U Supersession Note (correctly excluded), item 247 at
  `:6230`. The inventory is CORRECT -> ACCEPT (F1), not reject.
- Hypothesis: an admitted lowerer/FNV row (249-252) is mis-graded unimplemented.
  FALSIFIED: every admit verifies as ADMIT and is graded PROVED/impl_exceeds_spec by both
  1D and 1E; the unimplemented verdicts attach to orthogonal residuals. The inventory is
  CORRECT -> ACCEPT (F4), not reject.
- Hypothesis: a SK-V18 wave (G2/G4/G6) re-opens a rejected shape. FALSIFIED: the SPEC
  anti-pattern rows + `:398` explicitly reject the second-substrate / bespoke-mask /
  neutral-CSS-only / 7-way-re-fork shapes; the moves retarget/decorate the existing
  single-substrate leaf. The inventory is CORRECT -> ACCEPT (F6), not reject.
- Hypothesis: the totality OnceCell probe re-opens item 51/53's parser-local cursor reject.
  FALSIFIED: it is the per-parse ADMISSIBLE class (`LOCKS.md:139-149`), a separate tree
  fenced as SK-V19 scratch. The inventory is CORRECT -> ACCEPT (F8), not reject.

## Note (sub-threshold, outside REGRESSION lens — recorded, not raised)
The 1F-anti-pattern OnceCell row cites the `ctns_probe_admits` gate at
`support.rs:74-95`; the fn definition is at `:70` (the `12 && <= 24` byte-window logic at
`:81-84` falls within the cited span). This is a CH1/CH5-domain precision item on a
SK-V19-adoption (NON-pre-block, NON-regression) citation; the cited span still encloses the
load-bearing 12-24-byte window the row leans on, so it would not mislead a T-P2 reader about
any regression. Not a REGRESSION REVISE; flagged to the owning lens.

## Verdict
Under the REGRESSION lens, the inventories are SOUND. No inventory re-opens a
REDRESS-rejected route; the four-item pre-block (246/247/51/53) is correctly identified by
BOTH 1D and 1E with disk-accurate spans (incl. the correct exclusion of the W11U section
between 246 and 247, item 52 between 51 and 53); no admitted REDRESS row (249-252, W11W,
W11A) is mis-catalogued as unimplemented; no SK-V18 wave re-opens a rejected shape. Every
load-bearing path:line verifies live. No nit rises to a T-P2-misleading REGRESSION REVISE;
no inventory states anything false on disk, so there is no admissible REJECT. Honest tally:
all findings ACCEPT. This matches the V6 CH3 clean pass — TWO CONSECUTIVE CLEAN cycles
(V6, V7) under the REGRESSION lens, reaching the 2-consecutive-clean fixed point.

TALLY accept=8 revise=0 reject=0
