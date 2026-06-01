# CH1 CORRECTNESS — SK-V18 T-P2 totality CHALLENGE (cycle V2)

Lens: CH1 CORRECTNESS. Every cited paper must EXIST and carry the claimed
finding; every library-source citation must resolve to the claimed `path:line`;
every benchmark number must trace to a named corpus + platform; refuted-technique
rows must match the literature's actual position. A confabulated or unverifiable
citation is a REJECT; a real source with a wrong attribution/locator/venue is a
REVISE. A REJECT requires citing the specific unverifiable source, not a blanket
suspicion.

Target: the 6 SK-V18 generalization-grounding dossiers under
`restart/audit/totality/p2/` (2A-sota, 2B-primitive-vocab, 2C-grammar-neutral,
2D-cost-model, 2E-host-arch, 2F-parse-that-gaps).

Disposition by dossier: 2A ACCEPT, 2B ACCEPT, 2C ACCEPT, 2D REVISE, 2E ACCEPT,
2F ACCEPT.

## Verdict summary

The V1-cycle fold requirements are LANDED and re-verified: the Mison author list
(2D) now reads "Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann" with
"Pavlo/Zhou" removed; the Hyperscan author (2F) now reads "Wang, Hong, …" not
"Hua"; the simdjson venue (2A) is normalized to "The VLDB Journal 28(6), 2019"
everywhere; and the `NormalizeDirectSinkCost` grounding-table citation (2D:54) is
re-anchored to the symbol's real lines (`:75` instantiation, `:191`/`:193`
struct+impl). I re-verified every one of these against the live source and the
primary record.

This cycle's exhaustive spot-verification — every academic paper (Mison, Pratt
POPL 1973, simdjson/Langdale-Lemire VLDBJ, egg POPL 2021 + Tate POPL 2009,
iburg LOPLAS 1992), every spot-checked library URL (Kutenin Arm-NEON, Validark
interleaved-vectors, Lemire-2026-MATCH, FFmpeg/checkasm provenance), every
load-bearing in-tree `path:line`, the M5 Max host probe, and the REDRESS-144
benchmark provenance — found NO confabulated source and NO falsified refutation.
Therefore NO REJECT.

ONE genuine carried-over CH1 defect survives: the V1 fold #2 re-anchored the
`NormalizeDirectSinkCost` citation in the 2D grounding table but left the SAME
imprecise `:76` locator in the 2D **Source Index** (`2D:135`). This is the exact
real-source-imprecise-locator class V1 flagged as REVISE, only half-folded — so
2D remains a REVISE until the Source-Index locator is corrected. The other five
dossiers verify clean under CH1 at this cycle.

I did NOT manufacture additional REVISEs to meet the cycle-prior >=30% target:
under CH1 the only honest dispositions are what the verification supports, and
five dossiers carry no surviving citation/locator/venue defect. The +/-1 end-of-
range boundary slips on `find_css_significant` (2F `:169-214`, 2B/2E `:169-216`
vs the actual `:169-215`) are below the REVISE threshold — every internal anchor
(`:184-192` set split, `:199` OR-reduce, `:201` trailing_zeros, `:207-214` scalar
tail) resolves EXACTLY, the function is correctly located and correctly attributed,
and a one-line boundary on a correctly-anchored source is not a wrong attribution.

## Critical Findings

| id | dossier | severity | finding | falsifying evidence |
|---|---|---:|---|---|
| CH1-V2-01 | 2D | medium | The V1 fold re-anchored the `NormalizeDirectSinkCost` citation in the grounding-table row (`2D:54` now cites `backend_egraph.rs:75`,`:191-:193`,`:84-87` — correct), but the **Source Index** at `2D-cost-model.md:135` STILL cites the old imprecise `backend_egraph.rs:40-87`,`**:76**`,`:84-87`. Line `:76` is `let rules: [&dyn RewriteFn<…>; 1] = [&normalize];` — inside the rewrite-application block, not the symbol's definition (`:191`) or instantiation (`:75`). The substantive claim (the rewrite is LIVE through `BackoffScheduler`) is CORRECT; only the Source-Index locator is the un-folded half of V1 fold #2. | `rg NormalizeDirectSinkCost skinny/crates/passes/src/backend_egraph.rs` → instantiation `:75`, struct+impl `:191`/`:193`. `2D:135` still reads `…backend_egraph.rs:40-87`,`:76`,`:84-87`. V1 fold #2 required re-anchoring "the citation" off `:76`; the grounding row was fixed, the Source Index was not. |

## Enumeration under the lens (every grounding/refutation judged)

Each dossier was read in full; every load-bearing grounding and refutation row was
judged. Per-dossier disposition with the verification basis:

- **2A — ACCEPT.** The V1 venue defect is FOLDED: `2A:54` and SRC-SIMDJSON-PAPER
  (`2A:120`) now read "The VLDB Journal 28(6), 2019" consistently with 2B/2D/2E/2F.
  Re-verified: simdjson pinned SHA `79bbba…8ce5` doc citations, sonic-rs SHA
  `03545a…4bc5`, cssparser SHA `4c4948…4c71`, lightningcss SHA `ec1652…ebcc`.
  In-tree: `checkasm_parity.rs:3` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`"
  VERBATIM; `generated.rs:304`-`307` "rich, lazy, not eager, not flattened"
  VERBATIM; 14 `checkasm_*.rs` harnesses EXACT count. FFmpeg/checkasm provenance
  ("originated x264, shared by FFmpeg+dav1d+rav1d, fuzzed over seeds") confirmed
  via WebSearch (checkasm.videolan.me). Lemire-2026 / sonic-rs / yyjson rows carry
  correct attributions. The CSS refutation rows (broadcast / four-counter /
  fact-stream / lightningcss-cannot-close) correctly state the implementation's
  position. No surviving CH1 defect.
- **2B — ACCEPT.** The three SK-V18 §6 primitives verify: the eq-set NEON body at
  `aarch64/byte_class_from_eq_set_64.rs:40-56` (four `vld1q_u8` stripes, `vceqq_u8`
  fan, `vorrq_u8` OR-reduce) and the shift-add `vaddv_u8` movemask at `:79-84`
  are EXACT; `find_css_significant` two-fan split (`runtime_simd.rs:184-192`),
  OR-reduce (`:199`), `trailing_zeros` (`:201`) EXACT; the dead-caller proof
  (only `#[cfg(test)]` at `lib.rs:574`, `significant_ref` at `:506`) EXACT;
  `count_top_level_commas` (`:29`) live-consumes `byte_class_from_eq_set_64`+
  `bracket_depth_mask_64` (`:44`,`:47`,`:56`) EXACT. Arm ACLE intrinsics +
  Lemire/Hyperscan-SHUFTI/Langdale lineage correctly attributed. The Hyperscan
  author "Hong" (V1 fold) is present and correct (WebSearch: Wang/Hong/Chang/
  Park/Langdale/Hu/Zhu, NSDI '19). Neutrality-demotion refutation matches 1E.
- **2C — ACCEPT.** The sharpest verifiable assertion — the Lock 14 self-gate
  `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'` over
  `crates/ir/src/`+`crates/analysis/src/` "asserts ZERO, returns 13" — is VERIFIED
  EXACTLY: the live grep returns **13**. The 9-row `PRODUCTION_MANIFEST_TABLE` at
  `strategy.rs:137-185` (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty)
  EXACT; `for_grammar_with_manifest` at `:216` EXACT; `css_types.rs` (66 LOC,
  "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map") EXACT;
  `select_lowering` 5-shape match at `lower/mod.rs:18-24` EXACT. Pratt POPL 1973
  (DOI 10.1145/512927.512931) verified via WebSearch (dl.acm.org/doi/…). W3C spec
  URLs are stable primary sources. No CH1 defect.
- **2D — REVISE** (CH1-V2-01). The "STALE V2 refutation now live" claims verify:
  `NormalizeDirectSinkCost` rewrite LIVE (`:75` instantiation, `:191`/`:193`
  struct+impl, run through `BackoffScheduler` `:73`, extracted via
  `DecisionCostModel` `:83`), `select_lowering` exactly 5 shapes, the Mison author
  list (V1 fold) is CORRECT (WebSearch confirms Li/Katsipoulakis/Chandramouli/
  Goldstein/Kossmann, PVLDB 10(10) 2017; no Pavlo/Zhou). iburg/egg/Tate DOIs and
  OR-Tools URL carried correct. The ONLY defect is the un-folded `:76` Source-Index
  locator (CH1-V2-01).
- **2E — ACCEPT.** The host probe is VERIFIED EXACTLY on this machine:
  `machdep.cpu.brand_string = Apple M5 Max`; `FEAT_PMULL/DotProd/I8MM/CSSC/SHA3/
  SME2 = 1`; `FEAT_SVE2 = unknown oid (ABSENT)` — grounding the SVE2-absent
  svmatch refutation. The movemask divergence (`movemask.rs:5` canonical
  `vshrn_n_u16::<4>` vs `byte_class_from_eq_set_64.rs:79-84` shift-add `vaddv_u8`)
  EXACT. Lemire-2026-MATCH EXISTS (WebSearch confirms the 2026-04-19 post; SVE2
  `match` is "the fastest" but the host lacks FEAT_SVE2 — honest framing). Kutenin
  Arm-NEON post EXISTS at the cited Arm Community URL (WebSearch). Validark
  interleaved-vectors post EXISTS at validark.dev (WebSearch). `find_css_significant`
  on-disk (`runtime_simd.rs:169`), dead-caller `lib.rs:574` EXACT. SVE2-refuted
  and x86-deleted rows match the literature/scope. No CH1 defect.
- **2F — ACCEPT.** The central refutation — the upstream `parse-that` `scan/`
  substrate (`scan_balanced`, `structural_bitmap`, `quote_parity`, `number_simd`)
  V2 never audited — is consistent with the prior cycle's verification. The float
  no-fallback re-confirmation (`number/mod.rs:271` `text.parse::<f64>()`,
  `eisel_lemire/mod.rs` `None`-on-ambiguous) carries. The Hyperscan author (V1
  fold) is CORRECT ("Hong"). REDRESS-144 benchmark provenance VERIFIED EXACTLY:
  `skinny/REDRESS.md:4418-4438` `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT reports
  Track 1 `444.208` Mbps vs prior `434.1316…` Mbps, Criterion `+109.87%`, named
  corpus `nonjson_css_l4/track1_generated_css_l4_decl_values`, strict cssparser/
  lightningcss green — the 2F/2E/2B figures (444.2/434.1/+109.87%) trace to corpus+
  Criterion. `find_component_delim`/`consume_balanced_at` at `generated.rs:657`/
  `:693` EXACT; `count_top_level_commas` live-consumption EXACT. Only the +1
  end-of-range slip (`:169-214` vs actual `:169-215`) — below the REVISE threshold
  (correctly located, every internal anchor exact).

## Refuted-technique rows checked against the literature

- 2A/2B/2C/2E "x86/AVX-512 cannot close an M5 Max row; x86 deleted in skinny" —
  a project-scope decision, not a literature misstatement; the underlying
  intrinsics (Intel guide, GFNI/VPCLMUL) are real. Consistent.
- 2A/2E "simdjson stage 1 does NOT justify retained class columns / sidecars" —
  matches `parse_many.md` (stage 2 builds the tape FROM stage-1 indexes; transient
  projection). Correct reading.
- 2E "NEON `svmatch_u8` cannot be ported; MATCH/NMATCH is SVE2, host lacks
  FEAT_SVE2" — matches Arm SVMATCH doc + Lemire-2026 + the live host probe
  (FEAT_SVE2 absent on this M5 Max). Correct.
- 2F/2B/2E "`find_css_significant` cannot be wired as-is (flat skip vs recursive
  shell)" — matches the in-tree source (flat `:169` two-fan skip with only
  `#[cfg(test)]` callers vs the recursive `find_component_delim`/
  `consume_balanced_at` at `generated.rs:657`/`:693`). Correct.
- 2D "(STALE V2) zero-rule scaffold / marker-string lowerers" superseded — the
  live `NormalizeDirectSinkCost` rewrite (`:75`/`:191`) + delegating lowerers
  (`lower/mod.rs:18-24`) verify the supersession. Correct.
- 2C "the 9-ident grammar-named table in generic `ir` is NOT neutral; the Lock-14
  self-gate is RED (asserts ZERO, returns 13)" — VERIFIED: live grep returns 13.
  Correct.

No refuted row misrepresents the cited literature's actual position.

## Fold Requirements

1. **2D (CH1-V2-01, blocking):** Re-anchor the `NormalizeDirectSinkCost` locator
   in the **Source Index** at `2D-cost-model.md:135` off `:76` to the symbol's
   real lines — `backend_egraph.rs:75` (instantiation), `:191`/`:193`
   (struct+impl) — matching the already-corrected grounding-table row (`2D:54`).
   This completes the un-folded half of V1 fold #2.

Optional precision (non-blocking, below the REVISE threshold; noted for the
authors, NOT a fold gate): normalize the `find_css_significant` end-of-range to
the actual `:169-215` (2F cites `:169-214`, 2B/2E cite `:169-216`).

## Convergence Impact

The single REVISE (CH1-V2-01) BLOCKS T-P2 V2 convergence under CH1 until folded,
but it is NOT a REJECT: across exhaustive spot-verification (every academic paper,
every spot-checked library URL, every load-bearing in-tree `path:line`, the live
M5 Max host probe, and the REDRESS-144 benchmark provenance) I found NO
confabulated source, NO non-resolving locator on a load-bearing local citation,
and NO refuted-technique row that misstates the literature. The V1 folds are
landed and re-verified; the sole surviving defect is the half-folded `:76`
Source-Index locator in 2D, correctable in place without disturbing any grounded
or refuted verdict.

TALLY accept=5 revise=1 reject=0
