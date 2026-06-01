# CH3 REGRESSION — SK-V18 T-P2 V3 (cycle V3)

Lens: **CH3 REGRESSION.** No dossier grounds a route already refuted in
`skinny/REDRESS.md` as viable; the rejected-route ledger is honoured; a
"promising" direction REDRESS has already falsified is a REJECT; a grounding
that ignores a directly-relevant ledger prior (refuting *or* admitting) is a
REVISE. Spot-verify the most load-bearing citations against the actual ledger.

Target packet (the SK-V18-regenerated dossiers, `generated_at` 2026-06-01):

- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`

Disposition: **REVISE** (does not block clean convergence on its own — both
residual defects are ledger-anchoring DRIFT on faithfully-fenced groundings, not
refuted-route revivals; no route REDRESS has falsified is grounded as viable).

## What this cycle turns on — the dossiers were REGENERATED (2026-06-01)

The packet was regenerated today (`generated_at: 2026-06-01`). I cannot trust the
prior V2 cycle's citation verification — every load-bearing claim was re-checked
against the live ledger and the live source tree this pass. The headline result:
**every prior CH3 fold (V1-S1/S2/S3, V2-S1/S2/S3) is PRESENT in the regenerated
dossiers, and every load-bearing REDRESS citation verifies verbatim.**

The REDRESS-mention census rose sharply from V2 (2A=2, 2B=7, 2C=0, 2D=0, 2E=6,
2F=7) to this regen (2A=4, 2B=9, 2C=1, 2D=4, 2E=6, 2F=8). The two V2 zero-census
dossiers both gained ledger anchors: **2D now carries the REDRESS 96/97/98
CollapsedStage fence inline** (the V2-S1 fold), and 2C carries its prior-cycle
carry-forward header. The G6 LEDGER triad (REDRESS 144 precedent + 96/97/98/126
cautionary) is carried in 2B (`:323`-`338`, OQ-2B-SKV18-01 `:397`), 2E (`:155`-`164`,
OQ-4 `:214`), and 2F (`:85`, `:121`-`126`, OQ `:183`).

## Spot-verification of the load-bearing citations (CH3 confabulation check)

I read the cited REDRESS sections verbatim and re-fetched the post-cutoff external
citations live. **No confabulated line number, no misrepresented verdict, no
fabricated metric.** This is the load-bearing CH3 check: a route grounded on a
misread ledger verdict (citing a RETIRED route as a PASS-ADMIT precedent, or
inventing a citation) would be a REJECT. None found.

REDRESS ledger (read verbatim against `skinny/REDRESS.md`):

- **REDRESS 96/97/98** (`:2795`-`2944`) — VERIFIED. `G-W3-UNION-SUBSTRATE`
  "retired, not merely blocked" (`:2937`-`2939`). The finding is verbatim what
  2A/2D/2F quote: "on the M5 Max wide-issue core, the scalar
  `consume_structural`/delimiter path … is cheaper than materializing or streaming
  a SIMD structural cursor through retained parsing. The SIMD scan looked
  discarded because consuming it adds memory traffic and cursor indirection that
  the … scalar loop does not pay" (`:2928`-`2933`). The `:2934` new-Alpha/S-P3
  contract requirement is exactly what 2A LAC-04 invokes.
- **REDRESS 126** (`:3766`-`3805`) — VERIFIED. `G-W4-ASM-GEN-CONSUMER` =
  `ROUTE-PRODUCTION-SPLIT`; the `find_ascii_set_member64(…, b"{};")` microbench
  ratio is `4.718279341` (dossiers say "4.7×" — faithful); "a passing microbench
  requires W4 to halt before production CSS wiring." Faithful.
- **REDRESS 144** (`:4418`-`4438`) — VERIFIED. `G-W12-SIMD-ASM-PRODUCTION` =
  `PASS-ADMIT`; `find_ascii_set_member64` wired into generated CSS L4
  `Scanner::scan_block` delimiter search; Track 1 `444.208` Mbps vs prior
  `434.1316…` Mbps; Criterion `+109.87%`; strict cssparser/lightningcss green.
  Dossiers say "444.2 vs 434.1, +109.87%" — faithful.
- **REDRESS 50/51/53** (`:807`-`813`) — VERIFIED. The admissible-side line is
  verbatim: "structural projection must be the parser's single substrate, not a
  second scanner bolted onto source-byte recursive descent … or a `CollapsedStage`
  / `SinkOnly` lowering consumes live masks in the same loop. A `ParserState`-owned
  structural cursor over source bytes is non-canonical." This is the exact
  admissible-transient vs inadmissible-retained line 2A LAC-04 cites.

External citations (live re-fetch this pass — CH3 confabulation guard on
post-cutoff sources):

- **Lemire-2026-MATCH** (2A/2E svmatch refutation,
  `https://lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/`)
  — VERIFIED real and faithful. Confirms SVE2 `match` is "the fastest," "Apple has
  not yet adopted SVE2," and the conventional NEON route is the table-driven
  classifier (Langdale/Lemire 2019). Grounds 2E's refutation of NEON-svmatch on
  the SVE2-absent M5 Max on solid ground.
- **Arm SVMATCH blog** (2E `SRC-A64-SVMATCH`,
  `developer.arm.com/.../multi-token-search-strings-svmatch-instruction`) —
  VERIFIED real and faithful: "SVMATCH is an instruction that is introduced by Arm
  SVE2." Corroborates the svmatch-is-SVE2-not-NEON refutation.
- **iburg DOI 10.1145/151640.151642** (2D R-A spine) — VERIFIED: resolves (302) to
  the ACM Digital Library record. The canonical Fraser/Hanson/Proebsting LOPLAS
  1992 paper. egg (10.1145/3434304), Mison (10.14778/3115404.3115416),
  Langdale/Lemire VLDBJ 2019 (arXiv 1902.08318) are textbook-canonical and
  consistent with use.

Local route-reality (CH3 cares whether the grounded route stands on real ground
vs a phantom):

- `byte_class_from_eq_set_64` and `bracket_depth_mask_64` ARE live-consumed in
  `runtime/src/runtime_simd.rs:44,47,56` (the `count_top_level_commas` region),
  the i32 depth threaded as a TRANSIENT per-call carry — the 2F PTG-2F-11
  "live-wired transient" claim is real.
- `find_css_significant` is dead/test-only (`runtime_simd.rs:169`; sole caller
  `lib.rs:574` under `#[cfg(test)]`) — the 2B/2F R7 "retarget not wire-as-is"
  framing is real.
- 2D HEAD-supersession is real: `NormalizeDirectSinkCost` live at
  `backend_egraph.rs:75` (struct/impl `:191`/`:193`); `collapsed_stage.rs:16`
  delegates to `tape_plan::render_rule(.., Collapsed)`; the
  `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork is live at
  `grammar_provider.rs:39-43`.
- Upstream `parse-that` is absent from `skinny/Cargo.toml` (only
  `parse-that-regex` is a member) — 2F's provenance claim is real.

## Critical findings (residual CH3 defects)

| id | severity | dossier | finding | fold requirement |
|---|---:|---|---|---|
| CH3-V3-S1 | low | 2A | **The admitting prior is named on the LOCK candidate and the refutation but NOT on the grounding that rides it.** `T2A-V1-SOTA-JSON-001` (`2A:54`) grounds "transient structural projection can inform same-loop masks consumed by one DOM/tape builder … no retained cursor/list/class-column/sidecar." This IS the ADMISSIBLE side of the REDRESS 50/51/53/96-98 line — but its evidence column cites ONLY simdjson `parse_many.md:54-57`. The directly-relevant ADMITTING prior (REDRESS 53 `:807`-`813`, the ledger entry that *defines* the admissible same-loop-mask side) is carried on the parallel LAC-04 (`2A:113`) and refutation #5 (`2A:90`) but is uncited on the grounding row itself. Per the CH3 lens, a grounding that ignores a directly-relevant ledger prior — refuting *or admitting* — is a REVISE. | Cross-reference `T2A-V1-SOTA-JSON-001`'s admission_gate clause to REDRESS 50/51/53 (`:807`-`813`) by id, so the JSON same-loop-mask grounding names the same admissible-side ledger line that LAC-04 (the lock it feeds) and refutation #5 already carry. Cosmetic ledger-anchoring; does not change the route's viability. |
| CH3-V3-S2 | low | 2E | **The two NEW SK-V18 upgrade rows that ride the G6 inert-run skip carry no inline net-win fence.** `Movemask64Shrn` (`2E:107`, the SHRN-by-4 movemask "10-15% SPEC CPU 2017" upgrade) and `Interleave4Classify` (`2E:113`, the LD4 partial) are grounded/partial in-kernel swaps that thread directly into the same G6 inert-run scan the REDRESS-98 net-win fence governs. They are correctly framed as kernel-internal pack-convention swaps (NOT retained-cursor revivals — so NOT a refuted-route regression), but the REDRESS-98 net-win gate lives only on the G6 row's prose AA-block (`:155`-`164`) and OQ-4 (`:214`), not inline on these two upgrade rows. A downstream consumer could promote the SHRN/LD4 row as a "free movemask win" without reaching the net-win fence the G6 row carries. This is the same inline-anchoring class as the folded V2-S2 (PTG-2F-11) — not applied to the SHRN/LD4 rows. | Carry a one-clause inline note on `Movemask64Shrn`/`Interleave4Classify`: these are kernel-internal pack swaps; any speedup CLAIM still rides the G6 inert-run net-win that must clear the REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor finding (a checkasm bit-identity PASS is NOT a row move — REDRESS 126). Keep them grounded/partial, but fenced so the "10-15% movemask" figure cannot be promoted off the kernel row alone. |

## Per-item CH3 disposition ledger

Enumeration of every grounding/refutation under the CH3 lens (route-viability vs
the REDRESS ledger). 16 items; 2 REVISE.

The V1-cycle ≥30% REVISE expectation does NOT bind a re-converged cycle whose
predecessor's CH3 defects are already folded and whose every load-bearing REDRESS
citation + local route claim + external citation verifies faithful. CH3's contract
is explicit: a REVISE must name a REAL missing/wrong ledger prior. Forcing the
count to ≥30% would require manufacturing defects against faithfully-folded,
citation-verified groundings — which the lens forbids (S1/S2 below are the two
genuine residual ledger-anchoring drifts a fresh adversarial read surfaces; there
are no more without confabulating). The honest count stands, consistent with the
V2 cycle's 3/16 and below it by one (the S1-equivalent 2A-refutation-#5 drift is
now folded; only the JSON-001 grounding-row variant remains).

| # | dossier · item | route grounded/refuted | CH3 verdict |
|---|---|---|---|
| 1 | 2A · T2A-V1-SOTA-JSON-001 (transient structural projection → same-loop masks into ONE substrate) | admissible side of REDRESS 53; excludes retained cursor/sidecar; but grounding row cites only `parse_many`, not the REDRESS-53 admitting id | **REVISE** (CH3-V3-S1) |
| 2 | 2A · LAC-04 (retained cursor/list/class-column/sidecar refuted) | names REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` + 50/51/53 by id + SK-V18 SPEC `:397-402` (V1-S3 FOLDED, present in regen) | ACCEPT |
| 3 | 2A · refutation #5 (simdjson stage1 ≠ retained class columns) | now cross-references LAC-04's REDRESS 96/97/98 + 50/51/53 by id (V2-S3 FOLDED, present in regen) | ACCEPT |
| 4 | 2A · T2A-V18-ASMJSON-001 (NO JSON SIMD classifier; G5 neutralizes `json/scan.rs`, scan-free product path) | profile-first; honours scan-free JSON product path | ACCEPT |
| 5 | 2A · T2A-V18-DAV1D-001/002 (checkasm PROCESS, PASS≠speedup) | process-discipline grounding; no route revival | ACCEPT |
| 6 | 2A · CSS broadcast / fact-stream / `CssFullParseSummary` REFUTE-CSS-001..004 | refutes contrived CSS parity; consistent with overfit audit | ACCEPT |
| 7 | 2B · six scalar-backed families; LD4/PMULL/CSSC/SVE2/FSM citation-only refuted | mirrors Lock-16 orphan-kernel discipline | ACCEPT |
| 8 | 2B · SKV18-A2 find_component_delim retarget + OQ-2B-SKV18-01 | viable + REDRESS-144-precedented; OQ LEDGER-FRAMED (96/97/98/126) (V1-S1 FOLDED, present in regen) | ACCEPT |
| 9 | 2B · SKV18-A5 FSM/frame-stack DELETE-only reconcile | catches the latent retained-frame-stack (refuted sidecar) reintroduction; routes to DELETE | ACCEPT |
| 10 | 2B · `balanced_component_scan` → `css_balanced_component_scan` forced demotion | neutrality fence; no ledger regression | ACCEPT |
| 11 | 2C · JSON-only / CSS-only / `CSS_GENERATED_RS` / generator-sidecar / generic-grammar-switch refuted; delete-before-provider | honours wave-graph fence + refuted-route ledger; no revival | ACCEPT |
| 12 | 2C · typed CSS value/document provider as grammar-derived receiver; Sheets/BBNF-self anti-JSON falsifiers | admissible-after-gate; consistent with REDRESS CSS provider route | ACCEPT |
| 13 | 2D · R-A un-fork on cost-derived BackendShape; STALE V2 zero-rule/marker verdicts superseded at HEAD | HEAD-verified supersession (NormalizeDirectSinkCost live, lowerers real); relocated-seam is a SPEC §5-risk-1 NOVEL move with NO REDRESS prior (verified: 0 ledger hits) — correctly grounded against SPEC+iburg, no REDRESS id owed | ACCEPT |
| 14 | 2D · CollapsedStage as branchless staged FSM (UNKNOWN-2D-V3-04 / LAC-2D-V3-04) | the streamed-cursor class REDRESS 96/97/98 retired; now LEDGER-FENCED inline by id (V2-S1 FOLDED, present in regen) | ACCEPT |
| 15 | 2E · G6 two-fan eq-set skip (WIRE) + AA + OQ-4 + svmatch refute | viable + ledger-anchored (144 precedent, 96/97/98/126 cautionary); svmatch refuted on verified Lemire-2026 + Arm-SVMATCH (V1-S1 FOLDED) | ACCEPT |
| 16 | 2E · Movemask64Shrn (SHRN-by-4) + Interleave4Classify (LD4) NEW G6-riding upgrades | in-kernel pack swaps (NOT retained-cursor revival); but no inline REDRESS-98 net-win fence on the upgrade rows | **REVISE** (CH3-V3-S2) |
| — | 2F · PTG-2F-11 `bracket_depth_mask_64` + PTG-2F-13 G6 retarget | both now carry the INLINE LEDGER FENCE (REDRESS 96/97/98 + 144 + 126) (V1-S2 + V2-S2 FOLDED, present in regen) — rolled into items 8/15 above | ACCEPT |

## Evidence inspected

- Lens contract: `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`;
  prior CH3 verdicts `…/hardening/V1/CH3.md` (S1/S2/S3 folds) and
  `…/hardening/V2/CH3.md` (S1/S2/S3 folds); `…/T-P2-V2-FOLD-ADDENDUM.md`.
- Rejected-route ledger read verbatim: `skinny/REDRESS.md` — REDRESS 96/97/98
  (`:2795`-`2944`, `G-W3-UNION-SUBSTRATE` retired-not-blocked, finding
  `:2928`-`2933`); REDRESS 126 (`:3766`-`3805`, `ROUTE-PRODUCTION-SPLIT`, ratio
  `4.718279341`); REDRESS 144 (`:4418`-`4438`, `PASS-ADMIT`,
  `444.208`/`434.1316`, `+109.87%`); REDRESS 50/51/53 (`:807`-`813`,
  admissible single-substrate line). Confirmed ZERO ledger entry for a
  relocated-seam/RuntimeEmitterKind/un-fork route (grep == 0) — R-A is SK-V18-novel.
- Local route-reality: `skinny/crates/runtime/src/runtime_simd.rs:44,47,56,169,199`
  (live eq-set/bracket transient consumers + dead `find_css_significant`);
  `runtime/src/lib.rs:574` (test-only caller); `skinny/Cargo.toml:10,31`
  (parse-that-regex member, upstream parse-that absent);
  `passes/src/backend_egraph.rs:75,191,193`, `codegen/src/lower/collapsed_stage.rs:16`,
  `codegen/src/grammar_provider.rs:39-43` (2D HEAD-supersession real).
- Citation spot-verification (WebFetch, this pass): Lemire-2026-MATCH and Arm
  SVMATCH blog both confirmed live and faithful (SVE2 `match` fastest, Apple lacks
  SVE2, SVMATCH is SVE2-not-NEON); iburg DOI 10.1145/151640.151642 resolves (302)
  to ACM DL.
- Per-dossier REDRESS census (this regen): 2A=4, 2B=9, 2C=1, 2D=4, 2E=6, 2F=8 (vs
  V2 2A=2, 2B=7, 2C=0, 2D=0, 2E=6, 2F=7).

## Convergence impact

This issue **does not block clean convergence on its own.** No dossier revives a
REDRESS-refuted route: the G6 retarget is the REDRESS-144-precedented,
transient-same-loop side of the REDRESS 50/51/53/96/97/98 line (not the retired
retained-cursor side); 2C refutes the `CSS_GENERATED_RS`/generator-sidecar/
generic-grammar-switch routes; 2B SKV18-A5 catches and routes-to-DELETE the latent
retained-frame-stack reintroduction; 2E refutes NEON-svmatch on the verified
Lemire-2026/Arm-SVMATCH SVE2-absent finding; 2D's R-A is a SK-V18-novel un-fork
with no REDRESS prior to honour (relocated-seam is a SPEC §5-risk-1 mechanism,
correctly fenced by the firewall + R16 co-gate), and its supersession is an
HEAD-evidence update of a prior *finding*, not a ledger-retired route. Every
load-bearing REDRESS citation, external citation, and local file claim verified
faithful. All six prior CH3 folds (V1-S1/S2/S3, V2-S1/S2/S3) are present in the
regenerated dossiers.

The two residual REVISE items are ledger-anchoring DRIFT, not regressions:
`T2A-V1-SOTA-JSON-001` (2A) cites the admissible-side ledger line generically while
the lock it feeds names it by id; the new `Movemask64Shrn`/`Interleave4Classify`
upgrade rows (2E) ride the G6 inert-run net-win without the inline REDRESS-98 fence
the G6 row itself carries. Folding S1-S2 attaches the relevant REDRESS id to each,
closing the last paths by which a downstream consumer could promote a same-loop
JSON mask or an in-kernel movemask swap without reaching the admitting/cautionary
prior. Neither retracts a viable route.

TALLY accept=14 revise=2 reject=0
