# CH3 REGRESSION — T-P1 (SK-V18 totality, cycle V3)

## Lens

CH3 REGRESSION. Verify that (1) no T-P1 inventory re-opens a route already
rejected in `skinny/REDRESS.md`; (2) the rejected-route pre-block list is
correctly identified by 1D/1E; and (3) no admitted REDRESS row is
mis-catalogued as unimplemented. This file supersedes the prior SK-V15 V3 CH3
verdict in place per the SK-V18 totality cycle protocol; the prior file scored
7/7 ACCEPT against a different (NEW-CH3-V5-01 delete/rebuild) packet — that rule
no longer carries the regression weight, which has moved to the SK-V18
Rejected-Route Pre-Block (`1D-skinny-lessons.md` §"Rejected-Route Pre-Block").

## Method (live truth)

HEAD `4e4aa0648` (dirty tree). `skinny/REDRESS.md` is 6465 lines — confirming
1D U-5's claim that the ledger ends at SK-V15 W11 (`:6446-6465`). Every cited
REDRESS span was read directly; the four pre-block rows, the J-2/G-3/G-5/D-10
admit citations, and the OnceCell second-substrate edge were spot-verified
against both `skinny/crates/` (benched witness) and `crates/core/` (adoption
target). Code witnesses re-grounded with `sed`/`grep`/`md5`.

## Spot-Verified Rejected-Route Pre-Block (the load-bearing rows)

| pre-block item | 1D-cited span | live REDRESS truth | rejected shape match | verdict |
|---|---|---|---|---|
| 246 W11T structural-stream driver | `6184-6219` REJECT | hdr `:6184`, last bullet `:6219`; `:6192-6195` "structural stream as the parser driver", "replaced the parse_only driver" | EXACT — "second substrate over a retained structural stream" | ACCEPT |
| 247 W11V string64 mask | `6230-6260` REJECT | hdr `:6230`, last bullet `:6260`; `:6235` "64-byte aarch64 JSON string-special mask primitive" | EXACT — "bespoke per-grammar 64-byte mask" | ACCEPT |
| 51 SK-V5 `JsonEventCursor` | `742-768` (narrowed) REJECT | hdr `:742`, prose ends `:767`, blank `:768`, item 52 hdr `:769`; `:766` "No precomputed `StructuralIndex`… admissible" | shape match; span carve-out of item 52 correct; "ends `:768`" off-by-one | REVISE |
| 53 SK-V5 `JsonStructuralCursor` | `784-813` REJECT | hdr `:784`, last line `:813` "unless a future before/after row overturns this measurement"; `:792` "no retained `StructuralIndex`" | EXACT — "parser-local second cursor over a retained mask" | ACCEPT |

Live code grounding the **admissible** side of all four rows (the distinction
that keeps each row a fence and not a re-open): `tape/mod.rs:94`
`pub struct Tape<'input>`; `tape/mod.rs:175` `ValueRef` = `tape: &'doc Tape` +
`cursor` (single substrate, the rejected routes added a *second*);
`css_l4_declaration_values/generated.rs:257` `CssDocument` "Holds exactly the
existing `Tape` — no second substrate" (`tape: Tape<'input>`);
`event_grammar.rs:4` `EventGrammar` is a fact/class admission trait
(`STRUCTURAL_CLASS_COUNT`/`admits_fact`/`admits_class`), NOT a cursor — so
1A-SUB-012's REDRESS fence against reviving EventCursor sidecars is correct.

## Admitted-Row Mis-Catalogue Check (no admit silently demoted to unimplemented)

| admit | REDRESS span | live truth | inventory carrier | verdict |
|---|---|---|---|---|
| 231 W11A direct strict product | `5861-5881` ADMIT | hdr `:5861`, 13/17 admitted, `:5879` | 1D J-2 "accepted JSON close route" | ACCEPT |
| 249 W7 decision spine | `6326-6354` ADMIT-W7 | `egraph_rewrite_count` `:6335`, falsifiable CSP `:6337-6339` | 1D D-10/G-3 "admitted decision spine"; 1E L10 | ACCEPT |
| 250/251 W8/W9 lowerers | `6356-6414` ADMIT-W8/W9 | shared `BackendExpr` operation-plan renderer `:6363`, all-five gate `:6396` | 1D D-10/G-3 "operation-plan renderers"; `lower/mod.rs:18-24` 5-shape | ACCEPT |
| 252 W10 FNV quarantine | `6416-6444` ADMIT-W10 | bench-only metadata, not runtime arbiter `:6418-6421` | 1D G-5 | ACCEPT |

No admitted row is mis-flagged. The one place an over-claim could hide is the
W7/W8/W9 admit graded `impl_exceeds_spec`/PROVED: 1D D-10/G-3 and 1E L10 both
correctly attach the CH3-V1-004 SELECTION-DEPTH caveat (decision-engine depth
under the Sheets R-E precedence tower remains the open L10 stressor,
`1E:90`), and the admit rows themselves carry the "codegen package testing
blocked by dirty CSS generated files" residual (`:6350-6354,6378,6412`). The
admit is real; the caveat prevents a paper-close. The CSS >SOTA half is graded
`directional / not-re-locked` (J-3), NOT re-admitted as a closed CSS route —
this is the correct non-regression of the SK-V13 fake-admit lesson.

## Second-Substrate Re-Open Edge (the genuine regression risk)

The `crates/core` adoption tree carries a LIVE `OnceCell<::simd_scan::StructuralIndex>`
per-parse probe on `ScanState` (`crates/core/src/grammar/generated/json.rs:701`,
`ensure_structural_index` `:719`, `scan_structural` `:732`), the emitter's own
"probe substrate" (`…/dispatcher/support.rs:67`). This is the EXACT
`StructuralIndex` shape REDRESS items 51/53 rejected ("No precomputed
`StructuralIndex`" `:766`/`:792`). Verified it is NOT a re-open: it is threaded
`&mut ScanState` per parse (lazy `get_or_init`, idempotent), NOT a retained
cross-call parser-local cursor; it lives in the totality tree, not the skinny
benched plane where 51/53 measured; the eager variant was the one that regressed
(`json.rs:686-690` "AY.W1-fix eager scans regress JSON twitter -64%"). 1E
(`§No-candidates`) and 1F-anti-pattern (row 43) correctly fence it as a
`retention_lifetime=generated_function` ADMISSIBLE class + SK-V19-adoption
Lock-1 classification carry — explicitly scoping the "NONE new-substrate"
universality to skinny and refusing to close the substrate-union "BOTH trees"
claim. Correct disposition: neither a false re-open alarm nor a silent admit.

## SPEC-citation gap the pre-block discharges

`rg 'W11T|W11V|structural stream|event cursor' sk-v18/SPEC.md = 0`; SPEC cites
no REDRESS item number for 246/247/51/53 (the `REDRESS` tokens at SPEC `:217,305,308,418,431` are generic wave-fail dispositions). The SK-V18 G-moves
(G2 `css_balanced_component_scan`, G4 `Cursor`, G6 NEON retarget) ABUT these
rejected shapes WITHOUT naming them. 1D's note (`REDRESS:6168`-style)
"discharges the burden the dispatch chain otherwise leaves unmet" is therefore
the load-bearing fence — verified accurate and necessary.

## Findings

| ID | Disposition | Finding | Evidence | Correction |
|---|---|---|---|---|
| CH3-V3-001 | ACCEPT | Item 246 (W11T) correctly catalogued REJECT, rejected shape = structural-stream parse_only driver (second substrate), span `6184-6219` exact. | `REDRESS:6184,6192-6195,6219`; `1D` pre-block row 246. | None. |
| CH3-V3-002 | ACCEPT | Item 247 (W11V) correctly catalogued REJECT, rejected shape = bespoke per-grammar 64-byte string64 mask, span `6230-6260` exact. | `REDRESS:6230,6235,6260`; `1D` pre-block row 247. | None. |
| CH3-V3-003 | ACCEPT | Item 53 correctly catalogued REJECT, rejected shape = parser-local second structural cursor, span `784-813` exact. | `REDRESS:784,792,813`; `1D`/`1B:55` pre-block rows. | None. |
| CH3-V3-004 | REVISE | 1D item-51 pre-block row asserts "item 51 ends at `:768`". The item-51 prose ends at `:767`; `:768` is the blank separator before item 52 (`:769`). The narrowed BLOCK span `742-768` is itself correct (it carves item 52 out), but the literal "ends at `:768`" is an off-by-one against prose-end. | `REDRESS:767` (last prose line), `:768` (blank), `:769` (item 52 hdr). | In `1D-skinny-lessons.md` item-51 row, change "item 51 ends at `:768`" to "item 51 prose ends `:767`; block span `742-768` includes the blank separator before item 52 (`:769`)". |
| CH3-V3-005 | REVISE | Intra-1B asymmetry: `1B:55` (EventTape) correctly breaks out items 51/53 as `:742-768` / `:784-813` (carving item 52), but its sibling `1B:54` (OffsetTape) still cites the coarse `742-813` whole-span without the item-52 carve-out. Not a re-open (item 52 is never named as rejected), but inconsistent within one inventory and reads as if item 52 (`769-783`, a profiling reassay) were part of the rejected pair. | `1B-codegen-evidence.md:54` vs `:55`; `REDRESS:769-783` (item 52 = non-rejected reassay). | In `1B-codegen-evidence.md:54`, replace `skinny/REDRESS.md:742-813` with the same break-out used at `:55` — "items 51/53, `:742-768` and `:784-813`; `:769-783` is item 52 (non-rejected reassay)". |
| CH3-V3-006 | REVISE | Cross-sibling correction-id collision: `1B:55` labels its span change "widened … per CH3-V2-004"; `1D` item-51 labels its span change "narrowed `742-783`→`742-768` per CH3-V2-005". Both converge on the SAME final spans (51=`742-768`, 53=`784-813`) but the opposite verbs ("widened" vs "narrowed") under adjacent prior-cycle ids invite a reader to treat them as contradictory. They are not — 1B widened the PAIR span to cover both rejects; 1D narrowed the item-51 SUB-span to exclude item 52. | `1B:55` (CH3-V2-004, "widened"); `1D` item-51 row (CH3-V2-005, "narrowed"). | Add a one-clause cross-reference in the `1D` item-51 row: "(reconciles with `1B:55` CH3-V2-004: 1B widened the 51∪53 PAIR span; this row narrows the item-51 SUB-span to exclude item 52 — both land 51=`742-768`, 53=`784-813`)". |
| CH3-V3-007 | ACCEPT | No admitted REDRESS row is mis-catalogued as unimplemented. W11A/W7/W8/W9/W10 (items 231/249/250/251/252) all carry their ADMIT status in J-2/D-10/G-3/G-5; the `impl_exceeds_spec`/PROVED grades carry the CH3-V1-004 selection-depth caveat; CSS >SOTA is graded `directional`, not re-admitted. | `REDRESS:5861,6326,6356,6382,6416`; `1D` J-2/D-10/G-3/G-5, `1E` L10. | None. |
| CH3-V3-008 | ACCEPT | The `crates/core` `OnceCell<StructuralIndex>` probe — the one shape that could re-open the 51/53 `StructuralIndex` reject — is correctly fenced as per-parse (`&mut ScanState`, NOT cross-call) ADMISSIBLE + SK-V19-adoption classification carry, with the substrate-union "BOTH trees" closure explicitly withheld. Neither a false re-open alarm nor a silent admit. | `crates/core/.../json.rs:701,719,732`; `support.rs:67`; `1E §No-candidates`; `1F-anti-pattern:43`; `1F-coherence:100` (COH18-015). | None. |
| CH3-V3-009 | ACCEPT | The pre-block exists for the right reason: SK-V18 SPEC abuts but does not cite the rejected shapes (`grep W11T\|W11V\|structural stream\|event cursor SPEC = 0`), so 1D's pre-block is the load-bearing fence the dispatch chain otherwise omits. | `sk-v18/SPEC.md` grep = 0; `1D` pre-block note. | None. |

## Verdict

The CH3 regression floor HOLDS: no inventory re-opens a rejected `skinny/REDRESS.md`
route, the four-row pre-block is correctly identified with rejected-shape and
admissible-distinction grounded in live code, and no admitted row (231/249/250/251/252)
is mis-catalogued as unimplemented. The three REVISE findings are span/traceability
precision defects in the pre-block citation prose (off-by-one item-51 end; intra-1B
coarse-vs-broken-out asymmetry; opposite-verb correction-id collision across 1B/1D) —
none re-opens a route and none mis-grades an admit; each is a one-clause edit. No
REJECT: every cited path:line resolves to true live content.

TALLY accept=6 revise=3 reject=0
