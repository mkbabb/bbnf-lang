# CH3 REGRESSION — T-P1 (SK-V18 totality, cycle V4)

## Lens

CH3 REGRESSION. Verify that (1) no T-P1 inventory re-opens a route already
rejected in `skinny/REDRESS.md`; (2) the rejected-route pre-block list is
correctly identified **by 1D and 1E**; (3) no admitted REDRESS row is
mis-catalogued as unimplemented. This file supersedes the prior SK-V15 V4 CH3
verdict in place per the SK-V18 totality cycle protocol; the prior V4/CH3.md
scored 7/7 ALL-ACCEPT against the SK-V15 `NEW-CH3-V5-01` / REDRESS-183..213
delete/rebuild packet — that packet is not the live SK-V18 inventory and its
all-ACCEPT cannot be carried forward (the same defect CH3-V2-008 caught for the
prior V2 file). Authority: `restart/prompts/totality/PASS-1-EXCAVATION.md:116-119`,
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z.

## Method (live truth)

Live packet = SK-V18 V5 totality-excavation fold (1A-1F stamped
`cycle: V5-SKV18-totality`, `generated_at: 2026-06-01`). `skinny/REDRESS.md` =
6465 lines (matching 1D U-5: ledger ends SK-V15 W11 `:6446-6465`). Every cited
REDRESS span read directly with `sed`/`grep`; the four pre-block REJECT items,
the W11W/W11A/W7/W8/W9/W10 ADMIT spans, the `RESULTS.md:5-25` JSON rows, the
production-FNV and OnceCell-StructuralIndex live witnesses re-grounded against
both `skinny/crates/` (benched witness) and `crates/core/` (adoption target).
HEAD verification point this pass; tree dirty.

## Spot-Verified Rejected-Route Pre-Block (the load-bearing rows)

All four rows resolve to genuine REJECTs at the cited spans; the
admissible-vs-rejected distinction matches the REDRESS text verbatim.

| pre-block item | 1D span | live REDRESS truth | rejected shape match | verdict |
|---|---|---|---|---|
| 246 W11T structural-stream driver | `6184-6219` | hdr `:6184`; `:6191-6192` "structural stream as the parser driver", "replaced the parse_only driver instead of decorating"; last bullet `:6219` | EXACT — second substrate over retained structural stream | ACCEPT |
| 247 W11V string64 mask | `6230-6260` | hdr `:6230`; `:6235` "64-byte aarch64 JSON string-special mask primitive"; last bullet `:6260` | EXACT — bespoke per-grammar 64-byte mask | ACCEPT |
| 51 SK-V5 `JsonEventCursor` | `742-768` | hdr `:742`; prose ends `:767` ("…aux projection column are admissible"); `:768` blank; item 52 hdr `:769` | EXACT — parser-local whitespace cursor; item-52 carve-out correct | ACCEPT |
| 53 SK-V5 `JsonStructuralCursor` | `784-813` | hdr `:784`; `:792` "no retained `StructuralIndex`"; last line `:813` "unless a future before/after row overturns this measurement"; item 54 hdr `:815` | EXACT — parser-local second scanner over retained mask | ACCEPT |

The item-51 off-by-one (CH3-V3-004), the intra-1B coarse-vs-broken-out
asymmetry (CH3-V3-005), and the cross-sibling verb collision (CH3-V3-006) the
V3 cycle raised are DISCHARGED in the current fold: 1D item-51 row
(`1D-skinny-lessons.md:170`) now reads "prose ends `:767`; `:768` is the blank
separator… off-by-one corrected per CH3-V3-004" and carries the explicit
"reconciles with `1B:55` CH3-V2-004 … both land 51=`742-768`, 53=`784-813`
(CH3-V3-006)" clause; `1B:55` (OffsetTape) and `1B:56` (EventTape) both now
break out `:742-768`/`:784-813` with the `:769-783` item-52 carve-out. No
residual on those three rows.

## Admitted-Row Mis-Catalogue Check (no admit silently demoted to unimplemented)

| admit | REDRESS span | live truth | inventory carrier | verdict |
|---|---|---|---|---|
| 231 W11A direct strict product | `5861-5881` | hdr `:5861` "closes … as `ADMIT`" | 1D J-2 / D-9 "accepted JSON close route" | ACCEPT |
| W11W memchr trusted-string split | `6262-6294` | `:6262` "closes … as `ADMIT`"; `:6271-6273` names REDRESS-247/246 as the rejects it is distinct from; `:6294` "17/17 ADMITTED and 0 OPEN" | 1D J-2 "accepted JSON close routes" | ACCEPT |
| 249 W7 decision spine | `6326-6354` | ADMIT-W7 | 1D D-10/G-3, 1E L10 | ACCEPT |
| 250/251 W8/W9 lowerers | `6356-6414` | ADMIT-W8/W9 | 1D D-10/G-3, `lower/mod.rs:18-24` | ACCEPT |
| 252 W10 FNV quarantine | `6416-6444` | ADMIT-W10 "bench-only metadata, not a runtime selector" | 1D G-5 | ACCEPT |

No admit demoted. The W11W admit's `17/17` vs items 246/247's `11/17 OPEN-6`
is the correct ordering: 246/247 (REJECT) FAILED to close the six remaining
rows; W11W (ADMIT) closed them later. The pre-block and J-2 reflect this
without confusion. The W7/W8/W9 PROVED/`impl_exceeds_spec` grade carries the
CH3-V1-004 SELECTION-DEPTH caveat in both 1D G-3 (`:199`) and 1E L10 (`:90`);
the CSS >SOTA half is graded `directional / not-re-locked` (J-3/G-9, U-4
`:239-243`), NOT re-admitted — correct non-regression of the SK-V13 fake-admit
lesson. `RESULTS.md:5-25` rows resolve exactly to 1D D-9's cite (twitter
8349.290 > 4913.095, citm 9079.838 > 8335.772, canada 16709.901 > 12970.929).

## Live Second-Substrate Re-Open Edge (the one regression that could hide)

The `crates/core` adoption tree carries a LIVE `OnceCell<::simd_scan::StructuralIndex>`
per-parse probe — the EXACT `StructuralIndex` shape items 51/53 rejected
(`:766`/`:792` "No precomputed `StructuralIndex`"). Re-verified live:
`crates/core/src/grammar/generated/json.rs:701` (`structural_index: OnceCell<StructuralIndex>`),
`ensure_structural_index` `:719`, `scan_structural` `:732`; emitter calls it
"The probe substrate (OnceCell + helper)" at
`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:67`;
`ctns_probe_admits` gate at `support.rs:70-95`; emitted into **8 of 9**
generated grammars — `math.rs` has 0 `ensure_structural_index` (verified) — so
the 1E/1F-anti 9→8 breadth correction (CH5-V3-003) is exact. It is correctly
fenced as the ADMISSIBLE `generated_function`/`&mut ScanState`-per-parse class
(NOT cross-call retained → not the REJECT `retained-across-call-boundary`
class) consistently across `1E-locks-evidence.md:158`,
`1F-anti-pattern.md:43`, and `1F-coherence-scan.md:100` (COH18-015), with the
substrate-union "BOTH trees" closure explicitly withheld for SK-V19 adoption.
Neither a false re-open alarm nor a silent admit.

## Findings

| ID | Disposition | Finding | Evidence | Correction |
|---|---|---|---|---|
| CH3-V4-001 | ACCEPT | Items 246/247 correctly catalogued REJECT, rejected shapes (structural-stream parse_only driver; bespoke per-grammar 64-byte mask) match the REDRESS text, spans `6184-6219`/`6230-6260` exact. | `REDRESS:6184,6191-6192,6219`; `:6230,6235,6260`; 1D pre-block rows 246/247. | None. |
| CH3-V4-002 | ACCEPT | Items 51/53 correctly catalogued REJECT (parser-local whitespace cursor; parser-local second scanner over retained mask); spans `742-768`/`784-813` exact; item-52 (`769-783` reassay) correctly carved out; the V3 off-by-one / intra-1B / cross-verb REVISEs are discharged in the current fold. | `REDRESS:742,767,768,769,784,792,813,815`; `1D:170`; `1B:55-56`. | None. |
| CH3-V4-003 | ACCEPT | No admitted REDRESS row (231/W11W/W7/W8/W9/W10) is mis-catalogued as unimplemented; PROVED/`impl_exceeds_spec` grades carry the selection-depth caveat; CSS >SOTA graded `directional`, not re-admitted; JSON 51/51 `RESULTS.md:5-25` rows resolve exactly. | `REDRESS:5861,6262,6271-6273,6294,6326,6356,6382,6416`; `RESULTS.md:5-25`; 1D J-2/D-9/D-10/G-3/G-5, 1E L10. | None. |
| CH3-V4-004 | ACCEPT | The live `OnceCell<StructuralIndex>` probe in `crates/core` — the single shape that could re-open the 51/53 `StructuralIndex` reject — is correctly fenced as per-parse `generated_function` admissible across 1E/1F-anti/1F-coherence, 8-of-9 breadth verified, substrate-union closure withheld. | `crates/core/.../json.rs:701,719,732`; `support.rs:67,70-95`; `math.rs` 0 hits; `1E:158`; `1F-anti:43`; `1F-coherence:100`. | None. |
| CH3-V4-005 | REVISE | The CH3 contract requires the rejected-route pre-block list to be identified **by 1D AND 1E** (`PASS-1-EXCAVATION.md:117-118`). 1D carries the full four-item table (`:166-171`); **1E does NOT carry the four-item pre-block list at all.** 1E's only REDRESS-REJECT engagement is the OnceCell-StructuralIndex carry at `:158` (a substrate fence, not the W11T/W11V/51/53 enumeration); its sole numeric REDRESS cite is the W8/W9 ADMIT at `:90`. The pre-block is therefore single-sourced to 1D, contrary to the two-inventory contract. Neither V2 nor V3 flagged this — the contract's "1E" half is unmet. NOT a re-open (1E does not reopen anything), but the redundancy the contract mandates is absent, so a 1D-only typo/drift would have no second witness. | `PASS-1-EXCAVATION.md:117-118` ("identified by 1D and 1E"); `1D-skinny-lessons.md:166-171` (full table); `1E-locks-evidence.md` REDRESS-cite census = only `:14` (frontmatter) + `:90` (W8/W9 ADMIT, not a reject) + `:158` (OnceCell carry); `grep` for `246|247|item 51|item 53|W11T|W11V` in 1E = 0. | In `1E-locks-evidence.md`, add a one-row REDRESS rejected-route cross-reference (or a `No-candidates`-section clause) that names the 1D pre-block cluster (items 246/247/51/53, `skinny/REDRESS.md:6184-6219,6230-6260,742-768,784-813`) and binds the SK-V18 G2/G4/G6 moves to the admissible single-substrate distinction — so the pre-block list is identified by 1D **and** 1E per `PASS-1-EXCAVATION.md:117-118`, not 1D alone. |
| CH3-V4-006 | REVISE | The 1D pre-block completeness claim is unscoped against the truncated ledger. The pre-block note (`1D:173`) asserts it "discharges the burden the dispatch chain otherwise leaves unmet" — but the ledger ends at SK-V15 W11 (`REDRESS.md:6446`; 1D U-5 confirms), while SK-V16 and SK-V17 tranches EXIST on disk (`restart/skinny/tranches/sk-v16/{HANDOFF,SPEC,SYNTHESIS}.md`, `…/sk-v17/…`) with redress NOT captured in `skinny/REDRESS.md`. Any SK-V16/V17 rejected route is structurally invisible to the four-item pre-block. 1D U-5 (`:244-248`) discloses this as an UNKNOWN with a verify_action, which is correct handling — but the pre-block NOTE itself carries no completeness caveat, so a reader of the table cannot tell its non-regression coverage stops at SK-V15. | `1D:173` (unscoped "discharges the burden"); `1D:244-248` (U-5, ledger ends SK-V15 W11); `REDRESS.md:6446-6465`; `find restart/skinny/tranches/sk-v16 sk-v17` = both exist with HANDOFF/SPEC. | In `1D-skinny-lessons.md:173`, append a completeness caveat to the pre-block note: "Coverage is scoped to the committed ledger (ends SK-V15 W11, `REDRESS.md:6446`); SK-V16/V17 rejected routes are not yet captured — see U-5. The pre-block is complete for the captured ledger, not for the full skinny history." Cross-link the table header to U-5. |
| CH3-V4-007 | REVISE | The 1D G-5 production-FNV harness cite (`css_cold_harness.rs:130 track1_full`) is a path/line-precision residual. The file is `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs` (the `/bin/` segment is dropped in the cite), and `fn track1_full` is at `:132` (the `// ---- track1_full ----` comment is at `:131`, the `fn` at `:132`), not `:130`. The substance (FNV `input_fnv64` is LIVE production telemetry on the MEASURED Track-1 plane, not bench-only — `generated.rs:393-394,899`, `parser.rs:42`) is verified TRUE; only the harness path:line drifts. | `1D:201` cite `css_cold_harness.rs:130 track1_full`; live `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs` `fn track1_full` at `:132`; production FNV `…/css_l4_declaration_values/generated.rs:393,394,899` + `parser.rs:42` verified. | In `1D-skinny-lessons.md:201` (G-5 row), repair the harness cite to `skinny/crates/bbnf-bench/src/bin/css_cold_harness.rs:132 track1_full` (add `/bin/`, correct `:130`→`:132`). |

## Verdict

The CH3 regression floor HOLDS on substance: no inventory re-opens a rejected
`skinny/REDRESS.md` route; the four-row pre-block (246/247/51/53) is correctly
identified with rejected-shape and admissible-distinction grounded in live
REDRESS text and live code; no admitted row (231/W11W/W7/W8/W9/W10) is
mis-catalogued as unimplemented; and the one live re-open edge (the
`crates/core` `OnceCell<StructuralIndex>` probe) is correctly fenced as
per-parse admissible across three inventories, not revived. The V3 REVISEs are
discharged.

Three REVISE findings remain — none a re-open, each a contract/completeness/
precision defect that prior cycles missed by treating the 1D-only pre-block as
discharging the two-inventory contract: (005) the "1D AND 1E" requirement is
unmet — 1E carries no rejected-route pre-block list, only the OnceCell
substrate carry, so the pre-block has no second witness; (006) the pre-block
completeness claim is unscoped against the SK-V15-terminated ledger while
SK-V16/V17 tranche redress exists uncaptured on disk; (007) the G-5
production-FNV harness cite drops the `/bin/` segment and is one line off. No
REJECT: every cited path:line resolves to true live content; no recalled or
false claim surfaced.

TALLY accept=4 revise=3 reject=0
