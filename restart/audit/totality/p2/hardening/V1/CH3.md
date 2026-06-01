# CH3 REGRESSION — SK-V18 T-P2 V1 (cycle V1)

Lens: **CH3 REGRESSION.** No dossier grounds a route already refuted in
`skinny/REDRESS.md` as viable; the rejected-route ledger is honoured; a
"promising" direction REDRESS has already falsified is a REJECT; a grounding
that ignores a directly-relevant ledger prior (refuting *or* admitting) is a
REVISE. Spot-verify the most load-bearing citations.

Target packet (commit pinned by the SK-V18 T-P2 dispatch):

- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`

Disposition: **REVISE** (blocks clean T-P2 V1 CH3 convergence until folded).

## The load-bearing ledger fact this cycle turns on

The SK-V18 wave plan's single primitive-touching move is **G6 — the NEON
retarget of `find_component_delim`** (`runtime_simd::find_css_significant`
two-fan eq-set skip onto the 94.1% CSS scalar hot leaf). Three dossiers ground
it as a viable WIRE: **2E** (`SKV18-2E-G6-RETARGET-GROUND`, "the kernel ALREADY
EXISTS"), **2F** (`PTG-2F-09/10/11/13`, "the G6 WIRE is not a build-from-scratch
… RETARGET"), **2B** (`SKV18-A2`, "the find_component_delim NEON retarget is a
SALVAGE"). The SK-V18 SPEC §8 admits G6 on **correctness/presence alone** —
`acceleration_at_admission == admission` proven by caller census + profile
reachability + checkasm parity, with **the speedup CLAIM deferred to H1**
(`g6_speedup_median_mbps` null pre-H1; "the G6 outcome is `C` until H1",
SPEC.md:1364-1365). Whether the SIMD skip actually net-beats the scalar loop is
the open question, gated only as a "secondary, inert-run-bounded" risk
(`SYNTHESIS-RESEARCH.md:314`).

That exact question — *does a SIMD scan integrated into the parse loop net-win
on the M5 Max, or does the branch-predictable cache-hot scalar loop beat it?* —
has THREE directly-relevant prior verdicts in the ledger. **No dossier cites any
of them** (REDRESS-mention census: 2A=3 incidental, 2B=1 incidental, 2C/2D/2E/2F
= 0):

1. **REDRESS 96/97/98 — `G-W3-UNION-SUBSTRATE` RETIRED (cautionary).** Two
   faithful, correctness-green implementations of a SIMD structural scan
   materialised/streamed into the retained JSON parse loop *uniformly regressed*
   every must-improve row on the M5 Max (`skinny/REDRESS.md:2795`-`2906`). The
   formal finding (`:2928`-`2933`) is load-bearing: "on the M5 Max wide-issue
   core, the scalar `consume_structural`/delimiter path … is cheaper than
   materializing or streaming a SIMD structural cursor through retained
   parsing. The SIMD scan looked discarded because consuming it adds memory
   traffic and cursor indirection that the … scalar loop does not pay." This is
   precisely the inert-run/dispatch-overhead risk the G6 grounding defers — and
   it is settled NEGATIVELY for the analogous JSON case on the same host.

2. **REDRESS 126 — SK-V12 W4 CSS-delimiter ASM was a `ROUTE-PRODUCTION-SPLIT`,
   not an admit (cautionary).** The *same* `find_ascii_set_member64(…, b"{};")`
   kernel passed a 4.7× microbench but was explicitly closed WITHOUT production
   wiring or a same-wave consumer (`skinny/REDRESS.md:3766`-`3805`): "a passing
   microbench requires W4 to halt before production CSS wiring." The microbench
   win never validated in the production parse loop — the standing caveat the G6
   "kernel already exists" framing elides.

3. **REDRESS 144 — SK-V13 W12 `G-W12-SIMD-ASM-PRODUCTION` = `PASS-ADMIT`
   (PRECEDENT FOR).** The decisive fact: `find_ascii_set_member64` (the exact G6
   kernel class) WAS wired into production CSS `Scanner::scan_block` delimiter
   search and MOVED the production CSS row — Track 1 444.2 Mbps vs prior 434.1,
   Criterion +109.87%, strict cssparser/lightningcss equality green
   (`skinny/REDRESS.md:4418`-`4438`). This is a *successful production admission
   of the CSS delimiter SIMD route on the close host*, distinct from the retired
   JSON union substrate.

**Why this is REVISE and not REJECT.** The G6 route is NOT a refuted-route
revival: REDRESS 144 is a production precedent *for* it (CSS delimiter SIMD
moved a real row), and the retired item (REDRESS 98) is the JSON
*retained-cursor/class-column union substrate* — a structurally different shape
(retained sidecar vs transient same-loop skip) that 2A's LAC-04 correctly
fences. So the route is viable-to-test and ledger-honoured in principle. The
defect is that the grounding is **silent on the ledger record that actually
bounds it**: it cites neither the production precedent (144) that legitimises
"the kernel already exists / retarget" nor the cautionary M5-Max priors (96/97/98,
126) that constrain the deferred-to-H1 net-win. Under CH3, grounding a route
while ignoring the directly-relevant rejected/admitted ledger entries is a
REVISE, not a clean ACCEPT.

## Critical findings

| id | severity | dossier | finding | fold requirement |
|---|---:|---|---|---|
| CH3-V1-S1 | high | 2E, 2F, 2B | The G6 `find_component_delim` NEON-retarget grounding (`2E` G6 row + AA-Defended-1; `2F` PTG-2F-09/10/13 + AA-Defended-1/2; `2B` SKV18-A2 + OQ-2B-SKV18-01) cites the S-P1 profile and literature but NOT the three ledger priors that govern it: REDRESS 144 (CSS-delimiter SIMD `PASS-ADMIT`, the production precedent), REDRESS 126 (microbench-vs-production split), and REDRESS 96/97/98 (M5-Max SIMD-scan-into-parse-loop uniformly regressed → `G-W3-UNION-SUBSTRATE` retired). The "inert-run length bounds the realized speedup" risk is presented as a fresh open measurement when REDRESS 96/98 already measured the analogous JSON case negatively on the same host. | Each G6 grounding row must cite REDRESS 144 as the production precedent (the CSS delimiter SIMD route MOVED a row → "retarget the admitted kernel" is ledger-grounded) AND cite REDRESS 96/97/98 + 126 as the cautionary prior: the deferred-to-H1 net-win must clear the M5-Max scalar-cheaper-than-SIMD-cursor finding, and a microbench/checkasm PASS is explicitly NOT a production-row move (the 126 split). The "inert-run bounded speedup" OQ must be reframed as *re-opening a question REDRESS 96/98 answered negatively for JSON and 144 answered positively for CSS* — not a new unknown. |
| CH3-V1-S2 | medium | 2F | OQ (`2F:148`) proposes `bracket_depth_mask_64` could "replace the SCALAR recursive shell entirely, eliminating `consume_balanced_at` recursion." Eliminating the scalar recursive shell in favour of a materialised SIMD depth-bitmap threaded through the parse loop is the *exact class* REDRESS 96/97/98 retired (a streamed/materialised SIMD structural cursor through retained parsing). It is gated "defer until measured," but the binding prior measurement is uncited. | Annotate the `bracket_depth_mask_64`-replaces-recursion OQ with the REDRESS 96/97/98 retirement: a depth-bitmap threaded across blocks through retained parsing is the streaming-cursor shape that twice regressed on M5 Max; its promotion must clear that finding, not merely "match parity AND beat it" in isolation. Keep it deferred, but ledger-fenced, not open-ended. |
| CH3-V1-S3 | low | 2A | LAC-04 (`2A:113`) correctly fences "retained cursor/list/class-column/sidecar routes are REDRESS-refuted … any retained sidecar-like route requires a new Alpha/P1/SPEC contract," but cites "REDRESS retained-shape failures" generically and is still keyed to SK-V15 SPEC line numbers (`SPEC.md:136-143`). The fence is sound but unanchored to the specific retirement (REDRESS 98 `G-W3-UNION-SUBSTRATE`) and SK-V18 SPEC. | Re-anchor LAC-04 to the SK-V18 SPEC forbidden-sidecar clause and cite REDRESS 96/97/98 (the `G-W3-UNION-SUBSTRATE` retirement) and REDRESS 50/51/53 by id, so the fence names the exact retired thesis rather than "retained-shape failures." Confirms the transient-same-loop-skip (G6) is the admissible side of the same line REDRESS 53 (`:807`-`813`) draws. |

## Spot-verification of load-bearing citations (CH3 confabulation check)

All checked citations exist and the technical claims are faithful — **no
confabulation found**, so no item is a citation-REJECT:

- **SRC-LEMIRE-2026-MATCH** (2E) — VERIFIED real:
  `https://lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/`.
  Confirms SVE2 `match` is "the fastest" AND "Apple has not yet adopted SVE2" —
  exactly grounding 2E's refutation of NEON-`svmatch` on the SVE2-absent M5 Max.
  (Post-cutoff date; URL + content confirmed live.)
- **SRC-VALIDARK-LD4** (2E, 2B) — VERIFIED real:
  `https://validark.dev/posts/interleaved-vectors-on-arm/`. Confirms `ld4`
  interleaved-vector loads for movemask/parsing; 2B correctly REFUTES it as
  citation-only (no oracle/consumer/row-movement).
- **SRC-KUTENIN-NEON** (2E) — VERIFIED real: the Arm Community Blog
  republication of Kutenin "Bit twiddling with Arm Neon," 2022, confirms the
  `vshrn_n_u16` SHRN-by-4 movemask; 2E's SHRN-vs-shift-add divergence claim is
  accurate.
- **checkasm provenance** (2A, 2B, 2D, 2E, 2F) — VERIFIED real:
  `https://checkasm.videolan.me/` confirms checkasm "originally developed for
  x264 … in use by FFmpeg and dav1d … validates optimized assembly against
  reference C code." Grounds the dav1d-discipline (2A T2A-V18-DAV1D-001).
- **iburg** (2D R-A spine) — VERIFIED real: DOI 10.1145/151640.151642 resolves
  (302 → ACM DL) to Fraser/Hanson/Proebsting, "Engineering a Simple, Efficient
  Code-Generator Generator," ACM LOPLAS 1992. egg POPL 2021, Mison VLDB 2017,
  Langdale/Lemire VLDB 2019 are textbook-canonical and consistent with use.

## Per-item CH3 disposition ledger

Enumeration of every grounding/refutation under the CH3 lens (route-viability
vs the REDRESS ledger). 14 items; 5 REVISE (35.7%, ≥ the V1 30% floor).

| # | dossier · item | route grounded/refuted | CH3 verdict |
|---|---|---|---|
| 1 | 2A · T2A-V1-SOTA-JSON-001 (transient structural projection → same-loop masks into ONE substrate) | the *admissible* side of REDRESS 53 (`:811`-`812`); explicitly excludes retained cursor/sidecar | ACCEPT |
| 2 | 2A · LAC-04 (retained cursor/list/class-column/sidecar REDRESS-refuted) | honours REDRESS 50/51/53/96-98 fence, but generic citation + SK-V15-keyed | **REVISE** (CH3-V1-S3) |
| 3 | 2A · T2A-V18-ASMJSON-001 / AA-Defended-4 (NO JSON SIMD classifier; G5 neutralizes `json/scan.rs`) | profile-first refutation; honours REDRESS 56/98 (scan-free JSON product path) | ACCEPT |
| 4 | 2A · CSS broadcast / brace-counter / fact-stream REFUTE-CSS-001..004 | refutes contrived CSS parity; consistent with overfit audit + REDRESS CSS reporting | ACCEPT |
| 5 | 2B · six scalar-backed primitive families, citation-only admission refuted (LD4/PMULL/CSSC/SVE2/FSM) | mirrors REDRESS Lock-16 discipline; orphan-kernel + scalar-delegate honesty | ACCEPT |
| 6 | 2B · SKV18-A2 find_component_delim NEON retarget is a SALVAGE; OQ-2B-SKV18-01 inert-run-bound | viable route, but inert-run OQ uncited vs REDRESS 96/98/126/144 | **REVISE** (CH3-V1-S1) |
| 7 | 2B · `balanced_component_scan` → `css_balanced_component_scan` forced demotion | neutrality fence; no ledger regression | ACCEPT |
| 8 | 2C · CSS broadcast / generator-sidecar / generic-grammar-switch refuted; delete-before-provider fenced | honours no-delete-before-rebuild (REDRESS discipline); no regression | ACCEPT |
| 9 | 2C · typed CSS value/document provider as grammar-derived receiver | admissible-after-gate; consistent with REDRESS CSS provider route | ACCEPT |
| 10 | 2D · R-A un-fork on cost-derived BackendShape; STALE V2 zero-rule/marker verdicts superseded at HEAD | supersedes with HEAD evidence, does not reopen a refuted route; 5-shape canon held | ACCEPT |
| 11 | 2D · `emit_shape_source == lowered_program` firewall + R16 co-gate | strengthens the no-relocated-seam fence; no regression | ACCEPT |
| 12 | 2E · G6 two-fan eq-set significant skip grounded (WIRE) + AA-Defended-1/2 | viable + REDRESS-144-precedented, but priors 96/97/98/126/144 uncited | **REVISE** (CH3-V1-S1) |
| 13 | 2E · NEON `svmatch` refuted on SVE2-absent host; x86 deleted-in-skinny | correct refutation; Lemire-2026 verified; no regression | ACCEPT |
| 14 | 2F · PTG-2F-09/10/13 G6 retarget grounded; OQ `bracket_depth_mask_64` replaces recursive shell | retarget viable, but recursion-elimination OQ is the REDRESS-96/98-retired streaming-cursor class, uncited | **REVISE** (CH3-V1-S1, CH3-V1-S2) |

## Evidence inspected

- Lens contract: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md`;
  `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:138`-`146` (NEW-CH3-V5-01
  wave-graph / deletion-before-rebuild).
- Rejected-route ledger: `skinny/REDRESS.md` — REDRESS 50/51/53/54/55
  (`:715`-`882`, retained side-table / event-cursor / structural-mask cursor /
  decoded-string sinks REJECTED; the admissible single-substrate route at
  `:807`-`813`); REDRESS 96/97/98 (`:2795`-`2940`, `G-W3-UNION-SUBSTRATE`
  retired, the M5-Max scalar-cheaper-than-SIMD-cursor finding `:2928`-`2933`);
  REDRESS 126 (`:3766`-`3805`, CSS-delimiter ASM `ROUTE-PRODUCTION-SPLIT`);
  REDRESS 144 (`:4418`-`4438`, `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT, CSS
  Track 1 444.2 Mbps, +109.87%).
- SK-V18 SPEC G6 contract:
  `restart/skinny/tranches/sk-v18/SPEC.md:1320`-`1377` (exit gate is
  correctness/presence; speedup deferred), `:1364`-`1365` ("G6 outcome is `C`
  until H1"); §5-risk inert-run secondary risk
  `restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md:314`.
- S-P1 profile: `…/sk-v18/research/p1/SYNTHESIS-PROFILE.md:88`-`112` (94.1% CSS
  scalar-scan; R7 dead-kernel caveat) — no REDRESS prior cited.
- Dossier REDRESS-citation census: 2A=3 (all incidental/LAC-04 generic), 2B=1
  (incidental), 2C=0, 2D=0, 2E=0, 2F=0.
- Citation spot-verification: WebSearch/WebFetch confirmed Lemire-2026-MATCH,
  Validark-LD4, Kutenin-NEON, checkasm provenance, iburg DOI (above).

## Convergence impact

This issue **BLOCKS clean T-P2 V1 CH3 convergence until folded.** It is not a
REJECT: the G6 route is ledger-honoured in principle (REDRESS 144 is a CSS
production precedent; the retired item is the structurally distinct JSON union
substrate that 2A's LAC-04 fences), and every checked citation is real. It is a
REVISE because the G6 grounding across 2E/2F/2B is silent on the three ledger
entries (REDRESS 96/97/98, 126, 144) that bound the deferred-to-H1 net-win, and
because two OQs (`2F:148` recursion-elimination, `2B`/`2F` inert-run) re-open
questions the ledger has already partly answered on the same host without
naming the prior. Folding S1-S3 re-anchors the grounding to the ledger without
retracting any viable route. The other five dossier surfaces (JSON same-plane
discipline, CSS-contrivance refutations, primitive-admission discipline,
grammar-neutrality fences, cost-model un-fork) honour REDRESS and introduce no
wave-graph cycle or refuted-route revival.

TALLY accept=9 revise=5 reject=0
