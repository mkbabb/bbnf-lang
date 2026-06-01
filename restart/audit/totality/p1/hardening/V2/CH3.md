# CH3 REGRESSION — T-P1 V2 (SK-V18 cycle, adversarial re-review)

## Verdict

REVISE-DOMINANT.

The live T-P1 packet under `restart/audit/totality/p1/` is the **SK-V18 V5
totality-excavation inventory fold** (1A-1F all stamped `cycle: V5-SKV18-totality`,
`generated_at: 2026-06-01`). My CH3 lens — (1) no inventory re-opens a REDRESS
route; (2) the rejected-route pre-block list is correctly identified by 1D/1E;
(3) no admitted REDRESS row is mis-catalogued as unimplemented — passes on the
load-bearing substance: every cited REJECT item (246/247/51/53) verifies as a
genuine REJECT at the cited line, every admitted route (W11W/W11A) is correctly
treated as a close route, and no fence reopens a rejected sidecar/cursor shape.

But three findings are REVISE: a REDRESS line-span on the 1B EventTape row that
mis-locates "items 51/53", a 1D pre-block span that overshoots item 51 into the
item-52 baseline-reassay block, and — most consequentially — the **prior
V2/CH3.md report at this path was written against a STALE packet** (SK-V15 at
`2fcbc1dc8`, citing REDRESS-183/184/209..213 and a 1C EventTape fence at lines
the live 85-line 1C file does not contain). That all-ACCEPT verdict cannot stand
as the V2 CH3 surface for the live SK-V18 inventories. 5 ACCEPT / 3 REVISE.

## Findings

| ID | Disposition | Finding | Evidence | Required action |
|---|---|---|---|---|
| CH3-V2-001 | ACCEPT | The 1D rejected-route pre-block table (items 246/247/51/53) correctly identifies each as a REDRESS REJECT, and the admissible-vs-rejected distinction matches the REDRESS text. Item 246 = W11T parse-only structural-stream **driver** REJECT; item 247 = W11V parse-only string64 bespoke-mask REJECT; item 51 = SK-V5 `JsonEventCursor` parser-local cursor REJECT; item 53 = SK-V5 `JsonStructuralCursor` second-scanner REJECT. The admissible distinction 1D states ("retarget/decorate the existing in-loop single-substrate leaf" vs "second scanner / structural-stream driver / parser-local cursor") is verbatim the REDRESS-item language ("the remaining admissible route is single-substrate event/tape consumption, not a retained parser cursor", `skinny/REDRESS.md:765-768`). | `skinny/REDRESS.md:6186` (item 246 REJECT), `:6232` (item 247 REJECT), `:742` (item 51 REJECT), `:784` (item 53 REJECT); pre-block table `1D-skinny-lessons.md:160-167`. Cross-corroborated: the W11W ADMIT itself names "REDRESS-247's custom 64-byte string-special scanner and REDRESS-246's structural-stream driver" as the rejected routes it is distinct from (`skinny/REDRESS.md:6271-6273`). | None. |
| CH3-V2-002 | ACCEPT | No admitted REDRESS row is mis-catalogued as unimplemented. The 1D J-2 digest treats W11W (memchr trusted-string split) and W11A (direct strict-product) as "the accepted JSON close routes" — both are genuine ADMITs at the cited spans, not REJECTs relabeled and not admits demoted to unimplemented. | `1D-skinny-lessons.md:180` (J-2) cites `REDRESS.md:6262-6294` and `:5861-5881`; live `:6262` "W11W closes … as `ADMIT`", `:5863` "Item 231 closes … as `ADMIT`". JSON guard baseline preserved as PROVED at J-1 (`1D:179`), not regressed. | None. |
| CH3-V2-003 | ACCEPT | No inventory re-opens a rejected route. The EventTape/OffsetTape/EventCursor/typed-event-cursor fences in 1A/1B/1C all bind future work to the in-loop single-substrate event stream and explicitly forbid reviving the SK-V5-rejected retained/parser-local cursor + the Lock-1-rejected class-stream/structural-mask/prefix-carry family. The 1A-SUB-012 fence cross-references the correct Lock-1 rejected-route list. | 1A-SUB-012 fence `1A-substrate-evidence.md:75` → `restart/locks/LOCKS.md:137-158` (closure of REDRESS 96/97/98 generalised to ALL transient classifier-state; `retained-across-call-boundary` = REJECT class); 1B OffsetTape/EventTape fences `1B-codegen-evidence.md:54-55`; 1C Lock-1 substrate-union "no second cursor/sidecar" `1C-runtime-evidence.md:40` (C11), and `:35`/`:74` confirm no EventTape variant is materialised (UNIMPLEMENTED, not reopened). | None. |
| CH3-V2-004 | REVISE | The 1B **EventTape** row cites "the SK-V5-rejected retained parser-local EventCursor (items 51/53, `skinny/REDRESS.md:784-813`, REJECT)". The span `784-813` is item 53 ALONE — item 51 lives at `742-768` (item 52 starts at `769`, item 53 at `784`). Citing "items 51/53" but pointing the line range at only item 53 mis-locates item 51; a reader following the cite lands on the structural-mask cursor (53), not the event-cursor (51). The OffsetTape sibling row (`1B:54`) cites the correct enclosing span `742-813` for the same "items 51/53", which makes the EventTape narrowing demonstrably an error, not a deliberate scope. | `1B-codegen-evidence.md:55` cites `784-813`; live headers `skinny/REDRESS.md:742` (item 51), `:769` (item 52), `:784` (item 53), `:815` (item 54). | **1B-codegen-evidence.md** (D8/EventTape row, line 55): change the EventTape row REDRESS cite from `skinny/REDRESS.md:784-813` to `skinny/REDRESS.md:742-813` to match the OffsetTape sibling and actually cover both items 51 and 53. |
| CH3-V2-005 | REVISE | The 1D pre-block table cites **Item 51** at `skinny/REDRESS.md:742-783`. Item 51 (the `JsonEventCursor` REJECT) ends at line 768; lines `769-783` are item 52 ("SK-V5 baseline reassay after the event-cursor rejection"), a profiling re-measurement, NOT a rejected route. The span overshoots the REJECT into a non-REJECT block. The item-51 REJECT content is `742-768`. | `1D-skinny-lessons.md:164` (Item 51 row) cites `742-783`; live `skinny/REDRESS.md:769` "52. SK-V5 baseline reassay …" — item 51 terminates at 768. | **1D-skinny-lessons.md** (pre-block table, line 164): narrow the Item-51 REDRESS cite from `skinny/REDRESS.md:742-783` to `skinny/REDRESS.md:742-768` so the line range bounds the REJECT block, not the subsequent baseline-reassay item 52. |
| CH3-V2-006 | ACCEPT | The 1D note (line 167) that the SK-V18 SPEC does NOT cite these REDRESS items — so the pre-block discharges a burden the dispatch chain otherwise leaves unmet — is executable-verified true. `rg 'W11T\|W11V\|structural.stream\|event.cursor' restart/skinny/tranches/sk-v18/SPEC.md` returns zero matches (exit 1). The pre-block is therefore a genuine regression-discipline addition, not redundant prose. | `1D-skinny-lessons.md:167`; live `rg` over `restart/skinny/tranches/sk-v18/SPEC.md` = 0 matches. | None. |
| CH3-V2-007 | ACCEPT | The CSS >SOTA claim is correctly re-graded against regression. 1D demotes CSS canonical-cold from PROVED to `DIRECTIONAL / not-re-locked` (J-3, G-9) under loadavg 4.35, with U-4 the H1 re-lock gate; 1F-past-corpora pins "do NOT re-derive CSS as audit-demoted/contrived (that was SK-V15)" while ALSO not re-asserting it as a re-locked bench row. This is anti-regression discipline: it neither resurrects the SK-V13 fake-admit failure mode nor the SK-V15 audit-demoted posture, holding CSS at honest directional status. | `1D-skinny-lessons.md:181` (J-3 directional caveat), `:199` (G-9), `:228-232` (U-4); `1F-past-corpora.md:27` (do-not-re-litigate), `:75-76` (SK-V13 vs SK-V15 vs SK-V18 failure-mode separation). | None. |
| CH3-V2-008 | REVISE | The prior V2/CH3.md occupying this path is rendered against a STALE packet and cannot stand as the SK-V18 CH3 surface. It cites REDRESS-183/184/209..213 (SK-V14/W2R/W4R provider-deletion items) and a "1C EventTape proof-witness fence at `1C-runtime-evidence.md:62` and `:98-100`" — but (a) the live SK-V18 inventory packet cites NONE of REDRESS-183/184/209..213 anywhere (its pre-block is items 246/247/51/53), and (b) the live 1C file is 85 lines, so `1C:62`/`1C:98-100` point past EOF — the cited EventTape fence does not exist in the current 1C. The prior all-ACCEPT verdict reviewed the SK-V15 commit `2fcbc1dc8` packet (which the report's own header admits), not the live SK-V18 V5 fold. An all-ACCEPT carried forward from a non-current artefact is itself the paper-close cycle V1 warns against. | Prior V2/CH3.md lines 7-13 (self-admits "fresh SK-V15 inventory fold at commit 2fcbc1dc8"), rows CH3-V2-002/003/005 cite REDRESS-183/184/209..213 + `1C-runtime-evidence.md:62`/`:98-100`; live `skinny/REDRESS.md` DOES contain item 183 (`:5092`), 184 (`:5117`), 209-213 (`:5171`+) but the SK-V18 packet (1A-1F) cites none of them; live `1C-runtime-evidence.md` = 85 lines (no `:98-100`). | **V2/CH3.md** (this file): the surface is re-authored against the live SK-V18 V5 inventories. The SK-V15 REDRESS-183/184/209..213 pre-block ledger belongs to the SK-V15 packet and is not a CH3 finding against the current 1A-1F. No SK-V18 inventory owes those rows. |

## Rejected-Route Census (executable spot-check, this pass)

Every REDRESS item cited by the live 1D/1B pre-blocks was re-grounded at HEAD:

- Item 246 `skinny/REDRESS.md:6184-6219` — W11T parse-only structural-stream **driver**, REJECT (replaced the parse_only driver; cold margins −2136…−7206 Mbps). Correctly = G4 lazy-Cursor pre-block.
- Item 247 `skinny/REDRESS.md:6230-6260` — W11V parse-only string64 bespoke 64-byte mask, REJECT (distinct from W11T: no structural stream). Correctly = G2 `css_balanced_component_scan` pre-block.
- Item 51 `skinny/REDRESS.md:742-768` — SK-V5 `JsonEventCursor` parser-local whitespace cursor, REJECT. Correctly = G6 NEON-retarget pre-block (EventCursor-adjacent).
- Item 53 `skinny/REDRESS.md:784-814` — SK-V5 `JsonStructuralCursor` second retained-parser scanner, REJECT. Correctly = G6 retarget-onto-shell pre-block.
- Admitted controls: W11W `skinny/REDRESS.md:6262` ADMIT, W11A/item-231 `:5863` ADMIT — both treated as JSON close routes (J-2), neither demoted.

No inventory re-opens a rejected route. The admissible boundary in every pre-block
row ("retarget/decorate the existing in-loop single-substrate leaf") matches the
REDRESS REJECT text and the Lock-1 v+1 cross-call-retention prohibition
(`LOCKS.md:137-158`). The two REVISE line-span corrections (CH3-V2-004/005) are
citation-precision repairs, not regression breaches: every named item is the
correct REJECT, only two line ranges drift off the exact block boundary.

TALLY accept=5 revise=3 reject=0
