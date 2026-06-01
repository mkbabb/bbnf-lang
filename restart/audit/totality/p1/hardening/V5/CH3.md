# CH3 REGRESSION — T-P1 (SK-V18 totality, cycle V5)

## Lens

CH3 REGRESSION. Verify that (1) no T-P1 inventory re-opens a route already
rejected in `skinny/REDRESS.md`; (2) the rejected-route pre-block list is
correctly identified **by 1D and 1E**; (3) no admitted REDRESS row is
mis-catalogued as unimplemented. This SK-V18-cycle-V5 file supersedes the prior
SK-V15 V5 CH3 verdict in place per the SK-V18 totality cycle protocol; the prior
V5/CH3.md scored 7/7 ALL-ACCEPT against the stale SK-V15 `NEW-CH3-V5-01` /
REDRESS-183..213 delete/rebuild packet — that packet is NOT the live SK-V18
inventory (the live 1D/1E/1F are stamped `cycle: V5-SKV18-totality`,
`generated_at: 2026-06-01`), and its all-ACCEPT cannot be carried forward.
Authority: `restart/prompts/totality/PASS-1-EXCAVATION.md:116-119`,
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.

## Method (live truth)

Live packet = the SK-V18 V5 totality-excavation fold (1A-1F stamped V5-SKV18).
`skinny/REDRESS.md` = 6465 lines (ledger ends SK-V15 W11 item 253 at
`:6446-6465`, matching 1D U-5). Every cited REDRESS span read directly with
`sed`/`grep`; the four pre-block REJECT items (246/247/51/53), item 248 (the W6
CSS-typed-retime REJECT the CSS half abuts), the W11W/W11A/W7/W8/W9/W10 ADMIT
spans, the `RESULTS.md:5-25` JSON rows, the production-FNV chain, the 7-replica
md5, and the `crates/core` `OnceCell<StructuralIndex>` probe re-grounded against
both `skinny/crates/` (benched witness) and `crates/core/` (adoption target).
HEAD verification point this pass; tree dirty. This is NOT an all-ACCEPT wave:
spot-reading the cross-reference anchors surfaced three live path:line defects in
the REDRESS-fence cross-citations.

## Spot-Verified Rejected-Route Pre-Block (the load-bearing rows)

All four 1D rows resolve to genuine REJECTs at the cited spans; the
admissible-vs-rejected distinction matches the REDRESS text verbatim.

| pre-block item | 1D span | live REDRESS truth (re-read) | rejected shape match | verdict |
|---|---|---|---|---|
| 246 W11T structural-stream driver | `6184-6219` | hdr `:6184` "W11T Parse-Only Structural Stream Reject"; `:6190` "used the structural stream as the parser driver"; last bullet `:6217-6219` "11/17 ADMITTED and 6 OPEN" | EXACT — second substrate over retained structural stream | ACCEPT |
| 247 W11V string64 mask | `6230-6260` | hdr `:6230`; `:6235` "64-byte aarch64 JSON string-special mask primitive"; last bullet `:6258-6260` | EXACT — bespoke per-grammar 64-byte mask | ACCEPT |
| 51 SK-V5 `JsonEventCursor` | `742-768` | hdr `:742` "byte-class whitespace cursor is REJECTED"; prose ends `:767` ("…aux projection column are admissible"); `:768` blank; item 52 hdr `:769` | EXACT — parser-local whitespace cursor; item-52 carve-out correct | ACCEPT |
| 53 SK-V5 `JsonStructuralCursor` | `784-813` | hdr `:784` "structural-mask parser-local cursor is REJECTED"; `:792` "no retained `StructuralIndex`"; last line `:813` "unless a future before/after row overturns this measurement"; item 54 hdr `:815` | EXACT — parser-local second scanner over retained mask | ACCEPT |

Exact line numbers re-derived live: item 51 = `742`, item 52 = `769`, item 53 =
`784`, item 54 = `815`. So item 51 = `742-768` (`:768` blank separator) and item
53 = `784-813` are exact, and `:769-783` (item 52, "baseline reassay") is a
profiling re-measurement correctly carved out. The off-by-one (CH3-V3-004), the
intra-1B widened-vs-carved asymmetry (CH3-V3-005/V2-004), and the cross-sibling
verb collision (CH3-V3-006) raised by prior cycles are addressed in the live
fold's PROSE — BUT the cross-reference ANCHORS that prose names are stale by one
to two lines after the V5 row-shift (see CH3-V5-005, CH3-V5-006 below).

## Admitted-Row Mis-Catalogue Check (no admit silently demoted to unimplemented)

| admit | REDRESS span (re-read) | live truth | inventory carrier | verdict |
|---|---|---|---|---|
| 231 W11A direct strict product | `5861` | hdr `:5861` "closes … as `ADMIT`" | 1D J-2 / D-9 "accepted JSON close route" | ACCEPT |
| W11W memchr trusted-string split | `6262-6294` | `:6262` "closes … as `ADMIT`"; `:6294` "17/17 ADMITTED and 0 OPEN" | 1D J-2 "accepted JSON close routes" | ACCEPT |
| 249 W7 decision spine | `6326` | "ADMIT-W7" | 1D D-10/G-3, 1E L10 | ACCEPT |
| 250/251 W8/W9 lowerers | `6356`/`6382` | "ADMIT-W8"/"ADMIT-W9" | 1D D-10/G-3, `lower/mod.rs:18-24` | ACCEPT |
| 252 W10 FNV quarantine | `6416-6444` | "ADMIT-W10 … bench-only metadata, not a runtime selector" | 1D G-5 (split) | ACCEPT |

No admit is demoted. The W11W admit's `17/17` vs items 246/247's `11/17 OPEN-6`
is the correct ordering: 246/247 (REJECT) failed to close the six remaining JSON
parse_only rows; W11W (ADMIT) closed them later. `RESULTS.md:5-25` rows resolve
exactly to 1D D-9's cite. The W7/W8/W9 PROVED/`impl_exceeds_spec` grade carries
the SELECTION-DEPTH caveat in both 1D G-3 (`:199`) and 1E L10 (`:90`); the CSS
>SOTA half is graded `directional / not-re-locked` (J-3/G-9, U-4), NOT
re-admitted — correct non-regression of the SK-V13/SK-V15 fake-admit lesson.

## CSS >SOTA vs the W6 CSS-typed-retime REJECT (item 248) — the latent re-open edge

The live ledger carries item 248 (`## SK-V15 W6 CSS L4 Typed Same-Workload
Retime Reject`, `skinny/REDRESS.md:6298-6324`, `ROUTE-W6-REJECT`): the typed
`CssL4Parser::parse` track parsed `2/4` files at `4.317` Mbps vs cssparser
`2051.911` Mbps, typed-summary equality FALSE, "remains `AUDIT-FALSIFIED` /
`OPEN`", `admitted_rows=0` (`:6310-6314`). The SK-V18 inventories assert CSS
>SOTA is now MEASUREMENT-VALID/directional beating lightningcss 1.9–3.3×. This
does NOT re-open item 248: the W6 reject was the typed-CssDocument retime vs
`cssparser`; the SK-V18 CSS headline is the canonical-cold path vs
`lightningcss` — a distinct artifact and comparator, and the inventories grade it
`directional / NOT re-locked` (J-3 `1D:187`, COH18-013 `1F:87`, L08 `1E:88`)
with the H1 `css_canon_bench` re-lock gate named as the falsifier, NOT
re-admitted. The non-regression of the fake-admit lesson is intact. The ONE
thinness: the rejected-route pre-block (1D:166-173, 1E:161) enumerates only the
four JSON-scanner rejects and never cross-cites item 248, so the CSS half's
abutment of a live AUDIT-FALSIFIED CSS reject is fenced only through the J-3
directional caveat, not the pre-block table. Defensible (the pre-block is
scoped to the G6/G2/G4 scanner/substrate moves, and item 248 is a
comparator-retime route, not a scanner shape), so this is a noted thinness, not a
finding — the directional grade does the non-regression work item 248 needs.

## Live Second-Substrate Re-Open Edge (the one regression that could hide)

The `crates/core` adoption tree carries a LIVE `OnceCell<::simd_scan::StructuralIndex>`
per-parse probe — the EXACT `StructuralIndex` shape items 51/53 rejected
(`:765-767`/`:792` "No precomputed `StructuralIndex`"). Re-verified live:
`crates/core/src/grammar/generated/json.rs:701` (`structural_index: OnceCell<…StructuralIndex>`),
`ensure_structural_index` `:719`, `scan_structural` `:732`; emitter names it "The
probe substrate (OnceCell + helper)" at `support.rs:67`; `math.rs` carries 0
`ensure_structural_index` (re-verified: 8 of 9 generated grammars carry it,
`mod.rs` excluded) — so the 8-of-9 breadth correction is exact. It is correctly
fenced as the ADMISSIBLE per-parse `generated_function`/`&mut ScanState` class:
re-verified live that `ensure_structural_index(state: &'a mut ScanState, …)`
takes a per-parse `&mut ScanState` (`ScanState::new` seeds `OnceCell::new()`),
NOT a cross-call retained global — so it is NOT the REJECT
`retained-across-call-boundary` class (`LOCKS.md:137-158`). Fenced consistently
across `1E:159`, `1F-anti-pattern.md:44`, and `1F-coherence-scan.md:104`
(COH18-015), with the substrate-union "BOTH trees" closure withheld for SK-V19.
Neither a false re-open alarm nor a silent admit — BUT the gate-span anchor those
three fences cite is wrong (see CH3-V5-007).

## Two-Inventory Contract (1D AND 1E) — V4 REVISE fold check

CH3-V4-005 found the pre-block was 1D-only. The V5 fold DISCHARGES it: 1E now
carries the full four-item pre-block as the final `No-candidates axes scanned`
row (`1E-locks-evidence.md:161`), naming items 246/247/51/53 with REDRESS spans
`6184-6219`/`6230-6260`/`742-768`/`784-813`, binding the SK-V18 G2/G4/G6 moves to
the admissible single-substrate distinction, and explicitly noting "the list is
no longer 1D-only … second witness added per CH3-V4-005". The pre-block is now
identified by 1D AND 1E per `PASS-1-EXCAVATION.md:117-118`. ACCEPT.

CH3-V4-006 found the 1D completeness claim unscoped. The V5 fold DISCHARGES it:
`1D:173` now carries the COMPLETENESS CAVEAT ("scoped to the committed ledger
(which ends at SK-V15 W11, `skinny/REDRESS.md:6446`); SK-V16/V17 rejected routes
are NOT yet captured … see U-5"). ACCEPT.

CH3-V4-007 found the G-5 harness cite off by `/bin/` + one line. The V5 fold
OVER-corrects and lands EXACT at the new HEAD: `1D:201` cites
`skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:131 fn track1_full` with
`:130` the comment. Re-verified live: line 130 is `// ---- track1_full …`, line
131 is `fn track1_full(src: &str) -> usize {`. (The file shifted down one line
since V4 read `:132`; the live cite is now correct at `:131`.) The G-5
production-FNV substance re-verified TRUE: `…/css_l4_declaration_values/generated.rs:393`
(`out.push_str("source\tinput_fnv64=")`), `:394` (`fnv64(input.as_bytes())`),
`:899` (`fn fnv64`), `parser.rs:42 generated::emit_full_parse(input)`. FNV is
live production telemetry on the recognition path, NOT bench-only, and the G-5
split correctly does NOT contradict the W10 ADMIT (which fences FNV as a
non-equality/non-selector arbiter, and `REDRESS:6438-6441` itself names the CSS
`input_fnv64` emission as "old CSS diagnostic fact-stream metadata, already
outside live CSS admission after W6"). ACCEPT.

## Findings

| ID | Disposition | Finding | Evidence | Correction |
|---|---|---|---|---|
| CH3-V5-001 | ACCEPT | Items 246/247 correctly catalogued REJECT; rejected shapes (structural-stream parse_only driver; bespoke per-grammar 64-byte mask) match the REDRESS text; spans `6184-6219`/`6230-6260` exact (hdr 6184/6230, last bullet 6219/6260 re-read live). | `REDRESS:6184,6190,6219`; `:6230,6235,6260`; 1D pre-block rows 246/247 (`1D:168-169`). | None. |
| CH3-V5-002 | ACCEPT | Items 51/53 correctly catalogued REJECT (parser-local whitespace cursor; parser-local second scanner over retained mask); spans `742-768`/`784-813` exact (item hdrs re-derived live at 742/769/784/815); item-52 (`769-783` reassay) correctly carved out. | `REDRESS:742,767,768,769,784,792,813,815`; `1D:170-171`. | None. |
| CH3-V5-003 | ACCEPT | No admitted REDRESS row (231/W11W/W7/W8/W9/W10) mis-catalogued as unimplemented; PROVED/`impl_exceeds_spec` grades carry the selection-depth caveat; CSS >SOTA graded `directional` not re-admitted; W6 CSS-typed-retime REJECT (item 248) not re-opened; JSON `RESULTS.md:5-25` rows resolve. | `REDRESS:5861,6262,6294,6298-6314,6326,6356,6382,6416-6444`; `RESULTS.md:5-25`; 1D J-2/D-9/D-10/G-3/G-5, 1E L10. | None. |
| CH3-V5-004 | ACCEPT | The two-inventory contract is now met (CH3-V4-005 discharged): 1E carries the four-item pre-block second witness; 1D carries the completeness caveat (CH3-V4-006 discharged); the G-5 harness cite is exact at the new HEAD (CH3-V4-007 discharged). | `1E:161`; `1D:173`; `1D:201` vs live `css_cold_harness.rs:130-131`; production FNV chain `generated.rs:393,394,899`/`parser.rs:42`. | None. |
| CH3-V5-005 | REVISE | The 1B EventCursor REDRESS-fence rows mis-anchor the 1A second witness. Both `1B:56` (OffsetTape) and `1B:57` (EventTape) read "Cross-cite the 1A fence `1A-substrate-evidence.md:75` (1A-SUB-012)." Re-read live: `1A:75` is **1A-SUB-003** (a tape/direct-to-struct substrate-family row, NO REDRESS/EventCursor content); the actual **1A-SUB-012** REDRESS fence ("any future typed-event cursor must not revive EventCursor sidecars, retained structural streams … rejected by Lock 1") is at **`1A:84`**. The parenthetical "(1A-SUB-012)" is correct; the line number `:75` is off by 9 — a reader following the anchor lands on the wrong row. Not a route re-open (the fence content is right), but the load-bearing REDRESS-fence cross-reference resolves to a false path:line. Neither V4 nor the V5 fold caught it. | `1B:56`,`1B:57` cite `1A-substrate-evidence.md:75`; live `1A:75` = 1A-SUB-003 (substrate-family); live `1A-SUB-012` = `1A:84` (REDRESS EventCursor fence). | In `1B-codegen-evidence.md:56` AND `:57`, repair the cross-cite from `1A-substrate-evidence.md:75` to `1A-substrate-evidence.md:84` (1A-SUB-012). |
| CH3-V5-006 | REVISE | The 1D item-51/53 reconcile clause mis-anchors its 1B sibling. `1D:170` reads "Reconciles with `1B:55` CH3-V2-004: 1B widened the 51∪53 PAIR span to cover both rejects; this row narrows the item-51 SUB-span to exclude item 52." Re-read live: `1B:55` is **EagerTape**, a plain marker-string row with NO REDRESS fence. The row that "widened the pair span to cover BOTH per CH3-V2-004" (carrying `:742-813`) is **`1B:57`** (EventTape); the row that "narrows … to exclude item 52" per CH3-V3-005 is **`1B:56`** (OffsetTape). The V5 row-shift (the EagerTape/SinkOnly enumeration_note grew) pushed both fence rows down one line below where the carried-forward `1B:55` pointer expects them. The reconcile's substance (both land 51=`742-768`, 53=`784-813`) is TRUE; only the sibling anchor is stale. | `1D:170` cites `1B:55`; live `1B:55`=EagerTape (no fence), `1B:56`=OffsetTape (carved span, CH3-V3-005), `1B:57`=EventTape (widened pair `742-813`, CH3-V2-004). | In `1D-skinny-lessons.md:170`, repair the reconcile anchor from `1B:55` to `1B:57` (the EventTape widened-pair row) and add a parenthetical that the narrowing/carved-span sibling is `1B:56` (OffsetTape). |
| CH3-V5-007 | REVISE | The `ctns_probe_admits` gate span — the evidentiary anchor for the "one regression that could hide" OnceCell-StructuralIndex non-reopen fence — is mis-cited in both fences. `1E-locks-evidence.md:159` and `1F-anti-pattern.md:44` cite the gate as `support.rs:74-95`. Re-read live: `fn ctns_probe_admits(ir: &GrammarIR) -> bool {` opens at **`support.rs:70`** and its closing brace is at **`:85`** (`alphabet.len() >= 12 && alphabet.len() <= 24` at `:84`); lines `:88-96` are the NEXT fn's doc comment ("emit a CTNS-style structural-index probe"). The span is wrong on BOTH ends (start 74→70; end 95→85). V4/CH3-V4-004 accepted `:70-95` (start right, end wrong); the V5 fold additionally drifted the start to `:74`. The CH3-substantive fact (per-parse admissible, not retained) holds regardless, but the gate anchor for the StructuralIndex fence resolves past the function body. | `1E:159` & `1F-anti:44` cite `support.rs:74-95`; live `ctns_probe_admits` = `support.rs:70-85` (open `:70`, body end `:84`, close `:85`); `:88-96` is the next fn's comment. | In `1E-locks-evidence.md:159` AND `1F-anti-pattern.md:44`, repair the gate cite from `support.rs:74-95` to `support.rs:70-85`. |

## Verdict

The CH3 regression floor HOLDS on substance: no inventory re-opens a rejected
`skinny/REDRESS.md` route; the four-row pre-block (246/247/51/53) is correctly
identified by 1D AND 1E with rejected-shape and admissible-distinction grounded
in live REDRESS text and live code; no admitted row (231/W11W/W7/W8/W9/W10) is
mis-catalogued as unimplemented; the CSS >SOTA half is graded `directional / not
re-locked` and does NOT re-open the W6 CSS-typed-retime AUDIT-FALSIFIED reject
(item 248); the production-FNV G-5 split does not contradict the W10 FNV-quarantine
ADMIT; and the one live re-open edge (the `crates/core` `OnceCell<StructuralIndex>`
probe) is correctly fenced as per-parse admissible across three inventories, with
the per-parse `&mut ScanState` lifetime re-verified live. The three V4 REVISEs
(005/006/007) are discharged.

Three NEW REVISE findings remain — none a route re-open, each a load-bearing
REDRESS-fence cross-reference that resolves to a FALSE path:line after the V5
row-shift, which the all-ACCEPT prior file and the V5 fold both missed: (005) the
1B EventCursor fences cite `1A:75` (substrate-family row) for a fence that lives
at `1A:84` (1A-SUB-012), off by 9; (006) the 1D item-51/53 reconcile cites
`1B:55` (EagerTape, no fence) for the widened-pair row that lives at `1B:57`
(EventTape); (007) the OnceCell-StructuralIndex non-reopen fence — the single
shape that could hide a 51/53 re-open — anchors its gate at `support.rs:74-95`
in both 1E and 1F-anti, while `ctns_probe_admits` lives at `support.rs:70-85`.
No REJECT: every REDRESS span and live-code substance claim resolves to true
content; the defects are anchor-precision in the cross-references that bind the
pre-block to its second witnesses, not recalled or fabricated claims.

TALLY accept=4 revise=3 reject=0
