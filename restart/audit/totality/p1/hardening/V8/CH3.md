---
lens: CH3 REGRESSION
cycle: V8
pass: SK-V18 T-P1 totality excavation
reviewer: adversarial (challenge)
generated_at: 2026-06-01
targets: 1D-skinny-lessons.md, 1E-locks-evidence.md (+ spot-checks of 1F-anti-pattern.md, 1B-codegen-evidence.md, 1A-substrate-evidence.md)
verdict_convention: REJECT admissible ONLY when an inventory states something FALSE on disk + live falsifying path:line; a self-falsified suspicion is an ACCEPT
prior_cycle: V6 CH3 = clean (8/0/0); V7 CH3 = clean (8/0/0) — 2-consecutive-clean fixed point reached at V7
---

# CH3 REGRESSION — V8 Verdict

LENS: no inventory re-opens a route already REJECTED in `skinny/REDRESS.md`; the
rejected-route pre-block list is correctly identified by 1D AND 1E; no admitted
REDRESS row is mis-catalogued as unimplemented.

Method: this cycle I did NOT inherit V7's clean tally. The lens-primary inventories
(1E mtime 17:51, 1F-anti-pattern 17:51) post-date the V7 verdict, so every
load-bearing REGRESSION path:line was RE-GROUNDED LIVE on the current dirty tree
HEAD (097c4dd90 + uncommitted) — the four-item pre-block (246/247/51/53) with exact
header/close/prose-end/separator boundaries, the admitted spans (248 W11W, 231 W11A,
249-253 W7-W11), the SPEC re-open grep, the dual-witness mandate, and the 1A/1B
EventCursor cross-references. All findings below are first-hand disk verifications
this cycle, not folded forward.

## Findings (enumerated)

### F1 — Pre-block items 246 / 247 verify on disk, both REJECT, spans exact. ACCEPT.
`skinny/REDRESS.md:6184` = "## SK-V14 W11T Parse-Only Structural Stream Reject"; `:6187`
closes item 246 as `REJECT`. REJECT prose ends `:6219` (`distinct_values`.); `:6220`
blank; `:6221` = "## SK-V14 W11U unicode_escapes Supersession Note" (a DISTINCT
non-rejected section, correctly EXCLUDED); item 247 header "## SK-V14 W11V Parse-Only
String64 Reject" at `:6230`, prose ends `:6260`, item 248 "## SK-V14 W11W ... Admit"
at `:6262`. Both 1D (`:168`) and 1E (`:161`) cite 246=`6184-6219`, 247=`6230-6260`.
Disk-exact: the spans end at the last REJECT prose line and exclude the intervening
W11U section. The SK-V18 admissible counterparts (G4 `Cursor` = VIEW over the existing
Tape; G2 = SHARED grammar-neutral primitive over a grammar-DERIVED byte set) are
materially distinct. No re-open.

### F2 — Item 51 span `742-768`; item 52 correctly excluded. ACCEPT.
`:742` = "51. SK-V5 event-cursor redress: byte-class whitespace cursor is REJECTED."
(`JsonEventCursor`); prose ends `:767` ("...no aux projection column are admissible.");
`:768` blank separator; `:769` = "52. SK-V5 baseline reassay after the event-cursor
rejection." — a `samply` re-measurement under `skinny/profile/skv5-event-redress/`,
NOT a rejected route. Both 1D (`:170`) and 1E (`:161`) land `742-768` (block span
including the `:768` separator per the documented off-by-one convention, item 52
excluded). Verified disk-exact this cycle. No regression.

### F3 — Item 53 span `784-813` verifies. ACCEPT.
`:784` = "53. SK-V5 structural-mask parser-local cursor is REJECTED." (`JsonStructuralCursor`);
prose ends `:813` ("...non-canonical unless a future before/after row overturns this
measurement."); `:814` blank; `:815` = "54. SK-V5 exact decoded-string stats sink is
REJECTED." Both 1D (`:171`) and 1E (`:161`) land `784-813`. Correct.

### F4 — Admitted REDRESS rows NOT mis-catalogued as unimplemented. ACCEPT.
Verified live this cycle: every admit header sits at the cited line and closes ADMIT,
and a targeted grep (`rg 'W11W|W11A|W7|W8|W9|W10' 1D 1E | rg -i unimplemented`) returns
EMPTY — no admit is anywhere graded unimplemented.
- Item 249 "## SK-V15 W7 Decision Engine Spine Admit" (`:6326`); records `egraph_rewrite_count`
  (falsifiable). Graded PROVED (1D G-3 / D-10; 1E L10).
- Item 250 "## SK-V15 W8 EagerTape OffsetTape Lowerer Admit" (`:6356`); Item 251
  "## SK-V15 W9 Remaining Lowerer All-Five Gate Admit" (`:6382`) — five lowerers as
  operation-plan renderers. 1D cite `6356-6414` accurate.
- Item 252 "## SK-V15 W10 FNV Quarantine Admit" (`:6416`) — matches G-5 surface (a).
- Item 253 "## SK-V15 W11 Close Reconciliation Admit" (`:6446`); `:6460` confirms
  "Pattern H count `67`" + "lock count `16`" — corroborates the LAC-1E-15 baseline.
- J-2 admit routes: W11W item 248 (`:6262`, ADMIT, 1D cite `6262-6294`); W11A item 231
  "## SK-V14 W11A JSON direct_to_struct Strict Product Admit" (`:5861`, ADMIT, 1D cite
  `5861-5881`) — prose ends `:5881`, item W11B at `:5883`. Both verify.
The unimplemented verdicts attach to DISTINCT live residuals (generator-absent, emitter
fork, phantom `<G>`, x86, gate-by-exclusion, metalang leak) — orthogonal to the admits.
Item 246 is confirmed REJECT (`:6187`), catalogued as a rejected route, not an admit.

### F5 — Dual-witness mandate satisfied. ACCEPT.
1E `:161` independently enumerates the same four items with character-matching spans
(246=`6184-6219`, 247=`6230-6260`, 51=`742-768`, 53=`784-813`) and the same
admissible-vs-rejected firewall, binding the second witness (CH3-V4-005). Concordant
with 1D `:166-171`. The 1B sibling (`:56-57`) independently pre-blocks items 51/53 in
the OffsetTape/EventTape EventCursor rows with matching spans, reinforcing the witness.

### F6 — No SK-V18 wave re-opens any reject (SPEC cross-check). ACCEPT.
`rg 'W11T|W11V|structural.stream|event.cursor|JsonEventCursor|JsonStructuralCursor'
restart/skinny/tranches/sk-v18/SPEC.md` = 0 hits (verified this cycle). The SPEC does
not cite these items, so the pre-block legitimately discharges the burden the dispatch
chain leaves unmet (1D `:173`). G4's seam is a VIEW over the existing
`Tape`/`ValueRef`/`PayloadArena` (SPEC `:87-97`); G2 is a SHARED grammar-neutral
primitive (FORCED-demoted to `css_balanced_component_scan`); G6 RETARGETS NEON onto the
existing `find_component_delim` shell (SPEC `:154-165`) — none re-opens the
second-substrate (246) / bespoke-per-grammar-mask (247) / parser-local-second-cursor
(51/53) shapes.

### F7 — U-5 / ledger-end completeness caveat is sound. ACCEPT.
The committed ledger ends at item 253 (`:6446`, last prose `:6465` — verified
`wc -l = 6465`). 1D (U-5) and 1E correctly scope the pre-block to the captured ledger
(ends SK-V15 W11) and flag SK-V16/V17 rejected routes as structurally invisible. Items
246/247 are SK-V14-origin yet item-number-monotonic (246 < 248 < 253) because the
SK-V14 W11T/W11V rejects were recorded in the same ledger sweep that closed SK-V15. No
internal inconsistency.

### F8 — 1F-anti-pattern references no rejected REDRESS route; OnceCell carry not a re-open. ACCEPT.
`rg 'REDRESS|reject|246|247|item 51|item 53|JsonEventCursor|JsonStructuralCursor'
1F-anti-pattern.md` = EMPTY this cycle. The totality `OnceCell<::simd_scan::StructuralIndex>`
probe it catalogues is the per-parse `generated_function` retention class (admissible per
`LOCKS.md:139-149`), fenced as a SK-V19-ADOPTION reconcile burden in a SEPARATE tree
(`crates/core`), explicitly distinguished from the skinny parser-local cursor of items
51/53. Not a re-opened skinny reject. No regression.

## Reject hypotheses tested and self-falsified (per convention -> ACCEPT, NOT reject)
- H: item 246 span `6184-6219` wrongly absorbs the W11U section or is off by one.
  FALSIFIED: `:6219` last REJECT prose, `:6220` blank, `:6221` the DISTINCT W11U note,
  247 at `:6230`. Inventory CORRECT -> ACCEPT (F1).
- H: an admitted row (W11W / W11A / 249-252) is mis-graded unimplemented. FALSIFIED:
  grep for "unimplemented" against the W-ids returns EMPTY; every admit verifies as
  ADMIT and is graded PROVED/impl_exceeds_spec. Inventory CORRECT -> ACCEPT (F4).
- H: a SK-V18 wave (G2/G4/G6) re-opens a rejected shape. FALSIFIED: SPEC grep = 0; the
  moves retarget/decorate the existing single-substrate leaf. Inventory CORRECT -> ACCEPT (F6).
- H: the totality OnceCell probe re-opens item 51/53's parser-local cursor reject.
  FALSIFIED: per-parse ADMISSIBLE class, separate SK-V19 tree. Inventory CORRECT -> ACCEPT (F8).

## Note (sub-threshold, OUTSIDE the REGRESSION lens — recorded, flagged to owning lens)
The 1B sibling rows `:56` and `:57` both cite the 1A EventCursor fence as
`1A-substrate-evidence.md:75 (1A-SUB-012)`, but the 1A-SUB-012 row actually lives at
`1A-substrate-evidence.md:84`; line `:75` is the DISTINCT 1A-SUB-003 row (tape +
direct-to-struct one-substrate-family). This is a cross-cite line-anchor off by 9 lines
where the bare *id* `1A-SUB-012` still resolves correctly (the row exists, carries the
matching REDRESS fence at `:84`). It is in 1B (a spot-check sibling, NOT a REGRESSION
lens-primary inventory; 1B citation precision is owned by CH1/CH2), and it touches NO
re-opened route — the pre-block spans 51/53 in 1B `:56-57` are themselves correct
(`742-768` / `784-813`). The lens-primary 1D `:170` cross-ref cites only the bare id
`1A-SUB-012` (no line number), which resolves cleanly. Because the id resolves and no
regression claim is affected, this would not mislead a T-P2 reader about any
rejected-route handling. Sub-threshold for REGRESSION; not a REVISE; flagged to CH1/CH2.

## Verdict
Under the REGRESSION lens, the inventories are SOUND, re-verified first-hand this cycle
against the current (post-V7) dirty tree. No inventory re-opens a REDRESS-rejected route;
the four-item pre-block (246/247/51/53) is correctly identified by BOTH 1D and 1E with
disk-exact spans (incl. correct exclusion of the W11U section between 246 and 247, and
item 52 between 51 and 53); no admitted REDRESS row (W7-W11 = 249-253, W11W = 248, W11A =
231) is mis-catalogued as unimplemented; no SK-V18 wave re-opens a rejected shape. The
single off-anchor found is in 1B (outside the lens), bare-id-resolving, and touches no
route — sub-threshold, flagged not raised. No inventory states anything false on disk
within the REGRESSION lens, so there is no admissible REJECT. Honest tally: all findings
ACCEPT. This is the THIRD consecutive clean REGRESSION pass (V6, V7, V8) — the
2-consecutive-clean fixed point reached at V7 holds at V8.

TALLY accept=8 revise=0 reject=0
