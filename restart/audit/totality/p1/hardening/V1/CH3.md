# CH3 REGRESSION — T-P1 V1 (SK-V18, cycle V1)

## Lens Contract

CH3 is the T-P1 regression lens. Per `restart/prompts/ORCHESTRATOR.md` §3W/§3Z
and `restart/prompts/totality/PASS-1-EXCAVATION.md` §3 CH3, this audit checks
three things against the SK-V18 inventories under `restart/audit/totality/p1`:

1. **no inventory re-opens a route rejected in `skinny/REDRESS.md`** (no
   downstream wave dispatches over a REDRESSed predecessor; no revival of a
   measured-and-reverted shape);
2. **the rejected-route pre-block list is correctly identified by 1D and 1E**
   (the two named pre-block owners);
3. **no admitted REDRESS row is mis-catalogued as unimplemented work.**

This is the SK-V18 GENERALIZATION cycle (inventories dated 2026-06-01,
`cycle: SK-V18-totality`/`V5`). The SK-V18 moves with the highest regression
surface are: **G6** (RETARGET the checkasm-gated NEON eq-set kernel onto the
scalar `find_component_delim` shell — `SPEC.md:154-165`, close-cond #10), **G2**
(`css_balanced_component_scan` named primitive over a grammar-derived byte set),
and **G4** (a lazy `CssNode`/`Cursor` view over the existing tape). All three
abut historically-rejected routes: W11T structural-stream parse_only driver
(`REDRESS.md:6184-6219`, item 246, REJECT), W11V bespoke 64-byte string-special
mask (`:6230-6260`, item 247, REJECT), and the SK-V5 event-cursor /
structural-mask **parser-local second scanner** (`:742-813`, items 51 & 53,
REJECT). This V1 output is a fold-input for V2, not an implementation change.

## Evidence Read

- `restart/prompts/ORCHESTRATOR.md` §3W/§3Z; `PASS-1-EXCAVATION.md` §3 CH3.
- T-P1 inventories `1A`, `1B`, `1C`, `1D`, `1E`, `1F-coherence-scan`,
  `1F-anti-pattern`.
- `skinny/REDRESS.md` (rejected routes: W11T item 246 `:6184`, W11V item 247
  `:6230`, SK-V5 event-cursor item 51 `:742`, SK-V5 structural-mask item 53
  `:784`, W10AA `:5680`, W11Q `:6109`; admitted routes: W11W `:6262`, W11A
  `:5861`, W7 `:6326`, W8/W9 `:6356`/`:6382`, W10 FNV `:6416`, W11U
  supersession `:6221`; ledger tail `:6446-6465`).
- `restart/skinny/tranches/sk-v18/SPEC.md` §0.1 close-cond #1-#12,
  audit-overfit `SYNTHESIS-AUDIT-OVERFIT.md` §6, research
  `SYNTHESIS-RESEARCH.md` §3/§4.
- Live spot-verification at HEAD (md5, rg, sed, find) on every load-bearing
  path:line below.

## Spot-Verification Of Load-Bearing Cited Rows (live, at HEAD)

| cited claim | inventory:row | live result | verdict |
|---|---|---|---|
| 7 css_l4 `generated.rs` all md5 `b654562c…` | 1D D-2 `:81`; 1D table `:57` | all 7 identical `b654562ccff46ed62dd48e9ace325830` | exact |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` at `grammar_provider.rs:33,40,110` | 1D `:56`; 1E `:102` | `:33` field, `:40` enum, `:41/42` arms, `:110` gate — all present | exact |
| `parse_w11_1_number` ×7 in `json/generated.rs` | 1D D-8 `:103`; 1E D-1E-V5-08 `:108` | `grep -c = 7` | exact |
| x86 census 24 (`src/x86_64`) + 4 (`ext/x86`) = 28 | 1D `:62` (24); 1E D-1E-V5-04 `:104` (28) | `find` → 24 + 4 = 28; `byte_class_from_eq_set_64.{rs,asm}` present | exact |
| `find_component_delim` scalar def at `generated.rs:657` | 1D `:65`, D-6 `:97` | def at `:657`; call sites `:493/512/550` | exact |
| `lock14_baseline.rs` `GENERIC_SCAN_ROOTS:2409`, `SKV15_W2_EXTRA:2442` | 1D D-7 `:100`; 1E `:105` | `:2409`, `:2442` exact | exact |
| `diagnostic-x86` gate exclusion line | 1D D-7 `:100` cites `:2456`; 1E cites `:2463` | live: `diagnostic-x86` at **`:2463`**; `:2456` is the aarch64 strict-checkasm row | **1D wrong / 1E correct** |
| `tape/mod.rs` phantom `<G>` at `:175,179,197,227` | 1D `:60`; 1E `:103`; 1C C5 `:33` | `:175` struct, `:179` PhantomData, `:227` DocumentView — exact | exact |
| core runtime Pattern-H = 71; LOCKS baseline 67 | 1E D-1E-V5-06 `:106` | `find … wc -l = 71`; `LOCKS.md:408` asserts 67 | exact |
| REDRESS ledger ends at SK-V15 W11 (`:6446-6465`) | 1D U-5 `:204` | last header `## SK-V15 W11` at `:6446`; EOF `:6465` | exact |
| W10 FNV quarantine `:6416-6444` (bench-only) | 1D G-5 `:168` | item 252 ADMIT-W10, W11L/N/O bench-only | exact |

## Findings

| ID | Disposition | Finding | Evidence | Required V2 action |
|---|---|---|---|---|
| CH3-V1-001 | ACCEPT | **No SK-V18 inventory re-opens a rejected REDRESS route.** The three highest-regression SK-V18 moves are all framed as decorating the existing single-substrate leaf, not reviving a rejected shape. G6 RETARGETS NEON onto the *existing* in-loop `find_component_delim` shell as a SHARED grammar-neutral primitive the generated scan CALLS — materially distinct from W11V's bespoke per-grammar 64-byte mask (`REDRESS.md:6230-6260`, REJECT) and from the SK-V5 parser-local *second scanner* (`:742-813`, items 51/53, REJECT). G4's `Cursor` is a VIEW over the existing `Tape`/`ValueRef`/`PayloadArena`, not a second substrate or structural stream (cf. W11T item 246 structural-stream driver, `:6184-6219`, REJECT). | SPEC close-cond #10 `SPEC.md:154-165` ("RETARGETED onto the scalar recursive shell … SHARED grammar-neutral runtime primitive … NOT bespoke vector code re-emitted per-grammar"); #4 `SPEC.md:87-97` ("VIEW over the EXISTING `Tape`/`ValueRef`/`PayloadArena`; no second substrate"); 1A-SUB-024 `1A-substrate-evidence.md:87`; 1C C11 `1C-runtime-evidence.md:39` ("no second cursor/sidecar"); rejected antecedents `REDRESS.md:6184,6230,742,784`. | None. |
| CH3-V1-002 | REVISE | **1D and 1E identify the pre-block only at PATTERN level, not as the enumerated REDRESS route list the CH3 contract requires.** 1D asserts the empirical floor "no retained sidecar HOLDS" (`1D:36`) and 1E scans "retained sidecar: NONE" (`1E:154`), but neither inventory NAMES the specific rejected routes — W11T item 246 (structural-stream parse_only driver), W11V item 247 (string64 mask), SK-V5 items 51/53 (event-cursor / structural-mask parser-local cursor) — that the SK-V18 G6/G2/G4 moves abut. 1D names only the *admitted* close routes (J-2: W11W, W11A); the rejected pre-blocks bounding the NEON retarget and balanced scan are absent. The SK-V18 SPEC itself does not cite them either (`rg 'W11T\|W11V\|structural.stream\|event.cursor' SPEC.md` = 0), so the burden is unmet anywhere in the dispatch chain. | 1D floor claim `1D:36`; 1D admitted-route digest `1D:153` (J-2); 1E sidecar scan `1E:154`; rejected routes uncited: `REDRESS.md:6184` (W11T item 246), `:6230` (W11V item 247), `:742`/`:784` (SK-V5 items 51/53). | Add a 1D (and/or 1E) pre-block ROW enumerating REDRESS items 246/247/51/53 by id+line, keyed to the SK-V18 wave they bound (G6 NEON retarget, G2 balanced scan, G4 cursor): each row stating the FALSIFYING distinction (admissible = retarget/decorate the existing in-loop leaf; rejected = add a second scanner / structural-stream driver / bespoke per-grammar mask / parser-local cursor). |
| CH3-V1-003 | ACCEPT | **No admitted REDRESS row is mis-catalogued as unimplemented.** The JSON 51/51 guard (W11W `:6262`, W11A `:5861`) is preserved as PROVED (1D J-1/J-2, D-9 impl_exceeds_spec). The decision spine (W7 `:6326`) and all-five lowerers (W8/W9 `:6356`/`:6382`) are catalogued as `implemented`/`impl_exceeds_spec` (1D rows `:58`,`:68`; G-3 `:166`), not unimplemented. The W10 FNV quarantine (`:6416-6444`) making W11L/N/O bench-only is PROVED grammar-neutral (1D G-5 `:168`), not unimplemented. The only items marked "unimplemented" are genuinely un-built (the generator, the un-fork, the row-collapse, the CSS value API, x86 deletion, the metalang purge) — none of which is an ADMITTED REDRESS row. | 1D `:55-68` table; J-1/J-2 `:152-153`; G-3 `:166`; G-5 `:168`; admitted rows `REDRESS.md:6262,5861,6326,6356,6382,6416`. | None. |
| CH3-V1-004 | REVISE | **1D and 1E disposition the SAME admitted W7/W8/W9 evidence inconsistently — a latent over/under-statement straddle CH3 must reconcile.** 1D marks the decision engine `implemented` and the 5-shape `impl_exceeds_spec`, declaring it "moved from scaffold to LOAD-BEARING" (G-3 PROVED, `1D:68`,`:166`). 1E, citing the SAME W8/W9 admit lines (`:6356`,`:6382`), marks L10 `over-stated`: "the decision-engine load-bearing depth remains the open L10 question … R-E precedence-tower is the un-tested generality stressor" (`1E:89`). Neither mis-labels the admit as *unimplemented* (so neither breaks CH3 outright), but the contradictory disposition of one admitted REDRESS body is a regression hazard: a V2 reader folding 1D reads "PROVED load-bearing," a reader folding 1E reads "depth open / over-stated." | 1D `:68`,`:166` (PROVED/load-bearing); 1E L10 `:89` (over-stated, depth open); both cite `REDRESS.md:6356`/`:6382`; the open stressor `SYNTHESIS-RESEARCH.md:249-255`. | Reconcile to ONE disposition: the W7/W8/W9 lowerers are ADMITTED-as-operation-plan-renderers (PROVED at the admitted scope) AND the decision-engine *selection depth* under the Sheets precedence tower is PENDING (1D G-3 should carry the L10 depth caveat 1E names; or 1E should down-rank "over-stated" to "implemented-at-admitted-scope, depth-PENDING"). |
| CH3-V1-005 | ACCEPT | **The prior-cycle CH3-V1-007 REVISE on 1A is DISCHARGED at SK-V18.** The prior CH3 (SK-V15) flagged that 1A/1B/1C cite EventTape / typed-event-cursor work without a local REDRESS fence. 1A now carries the explicit fence: 1A-SUB-012 states "any future typed-event cursor must not revive EventCursor sidecars, retained structural streams, retained class lanes, or parser-owned cursor lists rejected by Lock 1 (`LOCKS.md:137-158`)"; 1A-SUB-017 fences the JSON structural scan `local_temp_only` / no cross-call carry; 1A-SUB-024 confirms the Cursor trait is a VIEW over the existing Tape. 1C C11 adds the "no second cursor/sidecar" CSS fence. | 1A-SUB-012 `1A-substrate-evidence.md:75`; 1A-SUB-017 `:80`; 1A-SUB-024 `:87`; 1C C11 `1C-runtime-evidence.md:39`; Lock-1 reject span `LOCKS.md:137-158`. | None for 1A/1C; residual is 1B (see CH3-V1-006). |
| CH3-V1-006 | REVISE | **1B's EventCursor/EventTape rows lack the local REDRESS fence that 1A carries, leaving the rejected-vs-admissible distinction un-marked at the codegen layer.** 1B rows `:53/:54` catalogue `OffsetTape = EventCursor over retained offsets` and `EventTape = EventCursor over compact event cells` as plain UNIMPLEMENTED spec targets (ARCH:1183/1184, ARCH-self-conceded NOT-ADMITTED). These describe the ARCHITECTURE-blessed *single-substrate lowering* target — but 1B carries NO note distinguishing it from the SK-V5-rejected *retained parser-local* EventCursor (items 51/53). A V2/T-P2 reader of 1B alone could read "EventCursor … UNIMPLEMENTED" as a benign open route and re-implement the rejected retained-cursor shape. 1A fences this; 1B does not cross-cite. | 1B EventCursor rows `1B-codegen-evidence.md:53`,`:54`; 1B gap `:123`; the missing fence (1B `grep reject\|Lock 1\|sidecar` returns only Lock-5 emitter rows, none on EventCursor reject); the rejected retained-cursor antecedent `REDRESS.md:784-813` (item 53). | Add to 1B's EventCursor/EventTape rows the same local fence 1A-SUB-012 carries: the admissible EventTape lowering consumes the SINGLE substrate's event stream in-loop; a *retained parser-local* EventCursor (SK-V5 items 51/53) is pre-blocked. Cross-cite `1A-substrate-evidence.md:75`. |
| CH3-V1-007 | REVISE | **1D D-7 (and the matching spec-claim row 1D:63) mis-cite the `diagnostic-x86` gate-exclusion line.** 1D cites `lock14_baseline.rs:2456`; the live `diagnostic-x86` exclusion is at `:2463`. Line `:2456` is the `("crates/bbnf-simd/src/aarch64","strict-checkasm-admitted")` entry — a DIFFERENT (and semantically opposite) row. 1E cites `:2463` correctly. This is a recalled/drifted LOC; CH3 requires cited impl path:line carry the claimed symbol. The error is doubled (D-7 row `1D:100` and the SPEC-claim table `1D:63`). | 1D D-7 `1D-skinny-lessons.md:100` (cites `:2456`); 1D table `:63` (cites `:2456`); live: `lock14_baseline.rs:2463` = `("crates/bbnf-simd/src/x86_64","diagnostic-x86")`, `:2456` = aarch64 strict-checkasm; 1E correct at `1E:104`,`:105`,`:145`,`:147`. | Correct both 1D citations `:2456`→`:2463` for `diagnostic-x86`. |
| CH3-V1-008 | ACCEPT | **1D U-5 correctly flags REDRESS coverage truncation without mis-cataloguing it as a regression.** The committed REDRESS ledger ends at SK-V15 W11 (`:6446`, EOF `:6465`); SK-V16/V17 redress is absent. 1D records this as an UNKNOWN with a verify_action (reconcile SK-V16/V17 tranche REDRESS/HANDOFF before Pass Omega), not as a re-opened or lost route. This is the correct CH3 disposition: a coverage gap surfaced, not a route silently dropped. The Pattern-H 71-vs-67 drift (1E D-1E-V5-06 / LAC-1E-V5-07) is likewise routed as a trace-or-open-regression-scan candidate, consistent with the very W11 close at `:6457` that records the 67 baseline. | 1D U-5 `1D-skinny-lessons.md:204-208`; live EOF `REDRESS.md:6465`; W11 close 67-baseline `REDRESS.md:6457`; 1E drift `1E:106`,`:148`; LOCKS baseline `LOCKS.md:408`. | None. |

## Verdict

Disposition counts: ACCEPT 4, REVISE 4, REJECT 0.

CH3 verdict: **REVISE.**

No SK-V18 inventory re-opens a rejected `skinny/REDRESS.md` route: the G6 NEON
retarget, G2 balanced scan, and G4 lazy cursor are all framed as decorating the
existing single-substrate leaf, materially distinct from the rejected
structural-stream driver (W11T), bespoke per-grammar mask (W11V), and
parser-local second scanner (SK-V5 items 51/53). No admitted REDRESS row
(W11W/W11A JSON, W7/W8/W9 spine+lowerers, W10 FNV) is mis-catalogued as
unimplemented. The four REVISEs are pre-block-precision and citation defects,
not route re-openings: (1) 1D/1E identify the pre-block only at pattern level
("no retained sidecar") rather than enumerating the specific REDRESS items
246/247/51/53 that bound G6/G2/G4 [CH3-V1-002]; (2) 1D and 1E disposition the
same admitted W7/W8/W9 evidence inconsistently — "load-bearing PROVED" vs
"over-stated, depth open" [CH3-V1-004]; (3) 1B's EventCursor/EventTape rows lack
the local REDRESS fence 1A now carries, leaving the rejected retained-cursor
distinction un-marked at the codegen layer [CH3-V1-006]; (4) 1D's `:2456`
citation for the `diagnostic-x86` gate exclusion is a drifted LOC — the live
line is `:2463` [CH3-V1-007]. The prior-cycle 1A EventTape-fence REVISE is
discharged [CH3-V1-005].

TALLY accept=4 revise=4 reject=0
