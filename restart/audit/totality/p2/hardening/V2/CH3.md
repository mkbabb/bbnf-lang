# CH3 REGRESSION — SK-V18 T-P2 V2 (cycle V2)

Lens: **CH3 REGRESSION.** No dossier grounds a route already refuted in
`skinny/REDRESS.md` as viable; the rejected-route ledger is honoured; a
"promising" direction REDRESS has already falsified is a REJECT; a grounding
that ignores a directly-relevant ledger prior (refuting *or* admitting) is a
REVISE. Spot-verify the most load-bearing citations.

Target packet (the SK-V18-regenerated dossiers, `generated_at` 2026-06-01):

- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`

Disposition: **REVISE** (does not block clean convergence on its own — the
remaining defects are residual ledger-anchoring drift, not refuted-route
revivals; but two un-folded streamed-cursor fences keep it from a clean ACCEPT).

## What changed since V1 — the V1 CH3 REVISE is FOLDED

The V1 CH3 verdict (S1/S2/S3, accept=9 revise=5) turned on one fact: the G6
`find_component_delim` NEON-retarget grounding across 2E/2F/2B was **silent on the
three ledger entries that bound it** — REDRESS 144 (the CSS-delimiter SIMD
`PASS-ADMIT` production precedent), REDRESS 96/97/98 (the M5-Max
scalar-cheaper-than-SIMD-cursor retirement of `G-W3-UNION-SUBSTRATE`), and REDRESS
126 (the microbench-vs-production split). The V1 REDRESS-mention census was
2A=3-incidental, 2B=1, 2C/2D/2E/2F=0.

**Every load-bearing G6 grounding now carries the ledger triad, cited faithfully.**
The V2 census is 2A=2, 2B=7, 2E=6, 2F=7 (2C/2D=0, addressed below):

- **2E** Architectural Assertion 1 + OQ-4 ("LEDGER-FRAMED") + cost-manifest
  abrogate threshold now cite REDRESS 144 as the precedent and 96/97/98 + 126 as
  the cautionary priors the deferred-to-H1 net-win must clear.
- **2F** PTG-2F-13 + Assertion 2 + the `bracket_depth_mask_64` OQ (now
  "LEDGER-FENCED" against REDRESS 96/97/98 — the exact S2 fold V1 demanded) carry
  the same triad.
- **2B** SKV18-A2 + OQ-2B-SKV18-01 ("LEDGER-FRAMED") + cost manifest carry the
  triad and the REDRESS 50/51/53 admissible-side line.
- **2A** LAC-04 is re-anchored (V1-S3 fold): it now names REDRESS 96/97/98
  `G-W3-UNION-SUBSTRATE` (`:2795`-`2940`, `:2928`-`2933`) and REDRESS 50/51/53
  (`:807`-`813`) by id, plus the SK-V18 SPEC one-substrate clause
  (`:397`-`402`), instead of the V1 generic "retained-shape failures."

## Spot-verification of the load-bearing citations (CH3 confabulation check)

I read the three cited REDRESS sections verbatim. **All three are faithful — no
confabulated line number, no misrepresented verdict, no fabricated metric.** This
is the load-bearing check for CH3: a route grounded on a misread ledger verdict
(e.g. citing a RETIRED route as a PASS-ADMIT precedent) would be a REJECT.

- **REDRESS 96/97/98** (`skinny/REDRESS.md:2795`-`2944`) — VERIFIED. Item 98
  retires `G-W3-UNION-SUBSTRATE` ("retired, not merely blocked"); the load-bearing
  finding at `:2928`-`2933` is verbatim what 2A/2E/2F/2B quote ("on the M5 Max
  wide-issue core, the scalar `consume_structural`/delimiter path … is cheaper than
  materializing or streaming a SIMD structural cursor through retained parsing").
  Both faithful implementations (96 full-vector, 97 streaming-cursor) were
  correctness-green and regressed every must-improve row. The `:2934` "new
  Alpha/S-P3 contract" requirement is exactly what 2A LAC-04 invokes.
- **REDRESS 126** (`:3766`-`3805`) — VERIFIED. `G-W4-ASM-GEN-CONSUMER` =
  `ROUTE-PRODUCTION-SPLIT`; the `find_ascii_set_member64(…, b"{};")` microbench
  speedup ratio is `4.718279341` (dossiers say "4.7×" — faithful); "a passing
  microbench requires W4 to halt before production CSS wiring." Faithful.
- **REDRESS 144** (`:4418`-`4438`) — VERIFIED. `G-W12-SIMD-ASM-PRODUCTION` =
  `PASS-ADMIT`; `find_ascii_set_member64` wired into generated CSS L4
  `Scanner::scan_block` delimiter search; Track 1 `444.208` Mbps vs prior
  `434.1316…` Mbps; Criterion `+109.87%`; strict cssparser/lightningcss green.
  Dossiers say "444.2 vs 434.1, +109.87%" — faithful.

Local file claims underpinning the G6 route (CH3 cares whether the route is
grounded on real ground vs a phantom): **all verified.**

- `byte_class_from_eq_set_64` IS live-consumed by `count_top_level_commas`
  (`runtime_simd.rs:44,56`) and `bracket_depth_mask_64` likewise (`:47`) — the
  2F/2B "kernel already exists, live-wired" claim is real.
- `find_css_significant` is dead/test-only (sole caller `lib.rs:574` under test),
  and its two-fan OR-reduce is at `runtime_simd.rs:199`
  (`byte_class_from_eq_set_64(block, set_a) | byte_class_from_eq_set_64(block,
  set_b)`) — the R7 "retarget not wire-as-is" framing is real.
- The upstream `parse-that` crate is absent from `skinny/Cargo.toml` (only the
  vendored `parse-that-regex` is a member) and its `scan/balanced.rs` exists —
  2F's provenance claim is real.
- 2D's HEAD-supersession is real: `NormalizeDirectSinkCost` live at
  `backend_egraph.rs:75` (struct `:191`, impl `:193`), `collapsed_stage.rs:16`
  delegates to `tape_plan::render_rule(.., Collapsed)`, and the
  `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork is live at
  `grammar_provider.rs:40-42`,`:110`.

New SK-V18 citations the V1 cycle did not check, verified live this pass:

- **Lemire `neonmovemask_addv`** (2B movemask grounding, line 258),
  `https://lemire.me/blog/2017/07/10/pruning-spaces-faster-on-arm-processors-with-vector-table-lookups/`
  — VERIFIED real: implements `neonmovemask_addv()` (vaddv-based movemask
  emulation), exactly as 2B cites. Not confabulated.
- **Lemire-2026-MATCH** (2E svmatch refutation),
  `https://lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/`
  — VERIFIED real: confirms SVE2 `match` is "the fastest," "Apple has not yet
  adopted SVE2," and the NEON route is the table/eq-fan classifier. Grounds 2E's
  refutation of the NEON-svmatch route on the SVE2-absent M5 Max on solid ground.

## Critical findings (residual CH3 defects)

| id | severity | dossier | finding | fold requirement |
|---|---:|---|---|---|
| CH3-V2-S1 | medium | 2D | The CollapsedStage body (UNKNOWN-2D-V3-04 + LAC-2D-V3-04 + Assertion "CollapsedStage as branchless staged FSM") is a per-block staged-FSM mask threaded through the parse loop — STRUCTURALLY the streamed-cursor class REDRESS 96/97/98 retired (`G-W3-UNION-SUBSTRATE`, the M5-Max scalar-cheaper-than-SIMD-cursor finding). 2D fences it only with a generic "transient-mask proof CH5 requires" and never names the binding ledger prior. 2D's REDRESS-mention census is 0. This is the *exact* defect V1-S2 fixed for 2F's `bracket_depth_mask_64` OQ — but the parallel fix was not applied to 2D's CollapsedStage, the other member of the same retired class. | Annotate UNKNOWN-2D-V3-04 / LAC-2D-V3-04 with REDRESS 96/97/98 by id: a `Collapsed` lowerer threading a per-call FSM mask through retained parsing is the streamed-cursor shape that twice regressed on M5 Max; its promotion past `diagnostic-only` must clear that finding, not merely satisfy the generic transient-mask CH5 gate. Keep it `diagnostic-only / author-declared`, but ledger-fenced (mirroring 2F PTG-2F-11's `bracket_depth_mask_64` fence), so a future consumer cannot promote CollapsedStage past the retired prior. |
| CH3-V2-S2 | low | 2F | PTG-2F-11 (the grounding-table row for `bracket_depth_mask_64`) states it "EXISTS, checkasm-gated, and is LIVE-consumed by `count_top_level_commas`" and calls it "the documented S-P2 R-F Cand-B path" WITHOUT the REDRESS-96/98 fence inline. The fence is carried correctly in the SEPARATE OQ row ("LEDGER-FENCED"), but a downstream consumer can cite the PTG-2F-11 grounding row ("exists + live-consumed → promote") without reaching the OQ. | Carry a one-clause inline ledger fence on PTG-2F-11 itself (not only the OQ): promotion of this bitmap to replace the scalar recursion is gated by the REDRESS 96/97/98 streamed-cursor retirement; the live `count_top_level_commas` consumer rides it as a TRANSIENT per-block carry, never a retained depth side-array (the retained form is the refuted shape). Row and OQ must both carry the fence. |
| CH3-V2-S3 | low | 2A | The V2-carried refutation #5 ("Refuted: simdjson stage 1 justifies retained class columns or sidecars," `2A:90`) still cites only `parse_many.md:54-57` generically, while LAC-04 — which it logically feeds — now names REDRESS 96/97/98 + 50/51/53 by id. The two surfaces draw the same line but only one is ledger-anchored. | Cross-reference refutation #5 to LAC-04's REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` id, so the in-body retained-sidecar refutation names the same retired thesis as the lock candidate it feeds. Cosmetic consistency; does not change the verdict. |

## Per-item CH3 disposition ledger

Enumeration of every grounding/refutation under the CH3 lens (route-viability vs
the REDRESS ledger). 16 items; 3 REVISE.

The prior-cycle balance (V1 = 35.7% REVISE) has correctly collapsed because the V1
REVISE was folded. The residual REVISE rate (3/16 = 18.75%) is *below* the V1 30%
floor by design: a cycle that has already folded its predecessor's CH3 defect and
whose every load-bearing REDRESS citation + local route claim verifies faithful
SHOULD show fewer regressions. Forcing the count to ≥30% would require
manufacturing defects against faithfully-folded, citation-verified groundings —
which CH3 forbids (a REVISE must name a real missing/wrong ledger prior). The
honest count stands.

| # | dossier · item | route grounded/refuted | CH3 verdict |
|---|---|---|---|
| 1 | 2A · T2A-V1-SOTA-JSON-001 (transient structural projection → same-loop masks into ONE substrate) | admissible side of REDRESS 53; excludes retained cursor/sidecar | ACCEPT |
| 2 | 2A · LAC-04 (retained cursor/list/class-column/sidecar refuted) | now names REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` + 50/51/53 by id + SK-V18 SPEC `:397-402` (V1-S3 FOLDED) | ACCEPT |
| 3 | 2A · refutation #5 (simdjson stage1 ≠ retained class columns) | honours the fence but generic `parse_many` cite, not LAC-04's REDRESS id | **REVISE** (CH3-V2-S3) |
| 4 | 2A · T2A-V18-ASMJSON-001 (NO JSON SIMD classifier; G5 neutralizes `json/scan.rs`) | profile-first; honours scan-free JSON product path | ACCEPT |
| 5 | 2A · T2A-V18-DAV1D-001/002 (checkasm PROCESS, PASS≠speedup) | process-discipline grounding; no route revival | ACCEPT |
| 6 | 2A · CSS broadcast / four-counter / fact-stream REFUTE-CSS-001..004 | refutes contrived CSS parity; consistent with overfit audit | ACCEPT |
| 7 | 2B · six scalar-backed families; LD4/PMULL/CSSC/SVE2/FSM citation-only refuted | mirrors Lock-16 orphan-kernel discipline | ACCEPT |
| 8 | 2B · SKV18-A2 find_component_delim retarget + OQ-2B-SKV18-01 | viable + REDRESS-144-precedented; OQ now LEDGER-FRAMED (96/97/98/126) (V1-S1 FOLDED) | ACCEPT |
| 9 | 2B · SKV18-A5 FSM/frame-stack DELETE-only reconcile | catches the latent retained-frame-stack (refuted sidecar) reintroduction; routes to DELETE | ACCEPT |
| 10 | 2B · `balanced_component_scan` → `css_balanced_component_scan` forced demotion | neutrality fence; no ledger regression | ACCEPT |
| 11 | 2C · JSON-only / CSS-only / generator-sidecar / generic-grammar-switch refuted; no-delete-before-provider | honours wave-graph fence + refuted-route ledger; no revival | ACCEPT |
| 12 | 2C · typed CSS value/document provider as grammar-derived receiver | admissible-after-gate; consistent with REDRESS CSS provider route | ACCEPT |
| 13 | 2D · R-A un-fork on cost-derived BackendShape; STALE V2 zero-rule/marker verdicts superseded at HEAD | HEAD-verified supersession (NormalizeDirectSinkCost live, lowerers real); evidence update, NOT refuted-route revival | ACCEPT |
| 14 | 2D · CollapsedStage as branchless staged FSM (UNKNOWN-2D-V3-04 / LAC-2D-V3-04) | the streamed-cursor class REDRESS 96/97/98 retired; fenced only generically, not by ledger id | **REVISE** (CH3-V2-S1) |
| 15 | 2E · G6 two-fan eq-set skip (WIRE) + Assertion 1 + OQ-4 + svmatch refute | viable + ledger-anchored (144 precedent, 96/97/98/126 cautionary); svmatch refuted on verified Lemire-2026 (V1-S1 FOLDED) | ACCEPT |
| 16 | 2F · PTG-2F-13 G6 retarget + `bracket_depth_mask_64` OQ | OQ now LEDGER-FENCED (96/97/98) (V1-S2 FOLDED); but grounding-row PTG-2F-11 lacks the inline fence | **REVISE** (CH3-V2-S2) |

## Evidence inspected

- Lens contract: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md`;
  V1 CH3 verdict `restart/audit/totality/p2/hardening/V1/CH3.md` (S1/S2/S3 fold
  obligations); `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- Rejected-route ledger (read verbatim): `skinny/REDRESS.md` — REDRESS 96/97/98
  (`:2795`-`2944`, `G-W3-UNION-SUBSTRATE` retired, finding `:2928`-`2933`,
  new-contract requirement `:2934`); REDRESS 126 (`:3766`-`3805`,
  `ROUTE-PRODUCTION-SPLIT`, ratio `4.718279341`); REDRESS 144 (`:4418`-`4438`,
  `PASS-ADMIT`, `444.208`/`434.1316`, `+109.87%`).
- Local route-reality checks: `skinny/crates/runtime/src/runtime_simd.rs:44,47,56,199`
  (live eq-set/bracket consumers + the two-fan OR-reduce);
  `runtime/src/lib.rs:500-501,574` (dead `find_css_significant`);
  `skinny/Cargo.toml:10,31` (parse-that-regex member, upstream parse-that absent);
  `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/scan/balanced.rs`
  (upstream substrate exists); `skinny/crates/passes/src/backend_egraph.rs:73-83,191-193`,
  `codegen/src/lower/collapsed_stage.rs:16`, `codegen/src/grammar_provider.rs:40-42,110`
  (2D HEAD-supersession real).
- Citation spot-verification (WebFetch, this pass): Lemire-2017 `neonmovemask_addv`
  pruning-spaces post and Lemire-2026 ARM-match post both confirmed live and
  faithful; the five V1-verified citations (Lemire-2026-MATCH, Validark-LD4,
  Kutenin-NEON, checkasm provenance, iburg DOI) carry forward.
- Per-dossier REDRESS census (V2): 2A=2, 2B=7, 2C=0, 2D=0, 2E=6, 2F=7 (vs V1
  2A=3-incidental, 2B=1, 2C/2D/2E/2F=0).

## Convergence impact

This issue **does not block clean convergence on its own.** No dossier revives a
refuted route: the G6 retarget is the REDRESS-144-precedented, transient-same-loop
side of the REDRESS 50/51/53/96/97/98 line (not the retired retained-cursor side);
2C refutes the generic-grammar-switch/sidecar routes; 2B SKV18-A5 catches and
routes-to-DELETE the latent retained-frame-stack reintroduction; 2E refutes
NEON-svmatch on the verified Lemire-2026 SVE2-absent finding; 2D's supersession is
an HEAD-evidence update of a prior *finding*, not a ledger-retired route. Every
load-bearing REDRESS citation and every load-bearing local file claim verified
faithful. The V1 CH3 REVISE (S1/S2/S3) is folded.

The three residual REVISE items are ledger-anchoring DRIFT, not regressions:
CollapsedStage (2D) and the `bracket_depth_mask_64` grounding-row (2F) are the two
members of the streamed-cursor retired class whose ledger fence lives only in an
adjacent OQ or generic clause rather than inline on the grounding itself, and 2A
refutation #5 cites the fence generically. Folding S1-S3 attaches the REDRESS
96/97/98 id to each, closing the last paths by which a downstream consumer could
promote a streamed-cursor-class body without meeting the retired prior. None
retracts a viable route.

TALLY accept=13 revise=3 reject=0
