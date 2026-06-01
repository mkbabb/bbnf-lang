# CH1 CORRECTNESS — SK-V18 T-P2 totality CHALLENGE (cycle V1)

Lens: CH1 CORRECTNESS. Every cited paper must EXIST and carry the claimed
finding; every library-source citation must resolve to the claimed `path:line`;
every benchmark number must trace to a named corpus + platform; refuted-technique
rows must match the literature's actual position. A confabulated or unverifiable
citation is a REJECT; a real source with a wrong attribution/locator/venue is a
REVISE.

Target: the 6 SK-V18 generalization-grounding dossiers under
`restart/audit/totality/p2/` (2A-sota, 2B-primitive-vocab, 2C-grammar-neutral,
2D-cost-model, 2E-host-arch, 2F-parse-that-gaps).

Disposition by dossier: 2A REVISE, 2B ACCEPT, 2C ACCEPT, 2D REVISE, 2E ACCEPT,
2F REVISE.

## Verdict summary

The citation discipline is, overall, strong: every academic paper cited EXISTS,
every pinned upstream commit SHA resolves, every spot-checked in-tree `path:line`
and every spot-checked upstream doc line-range matches verbatim, and no
refuted-technique row misstates the literature's position. I found NO
confabulated source and NO falsified refutation — therefore NO REJECT. Three
dossiers carry a real, materially-wrong citation-attribution or locator defect
(author list, paper venue, symbol line) that CH1 requires corrected; these are
REVISE. The Mison author-list error (2D) is the load-bearing one — it is wrong on
the authorship of a paper that grounds the SinkOnly direct-projection thesis.

## Critical Findings

| id | dossier | severity | finding | falsifying evidence |
|---|---|---:|---|---|
| CH1-V1-01 | 2D | high | The Mison citation lists the authors as **"Li, Pavlo, Zhou"** in the grounding table (`2D:59`) AND in the Source Index (`2D:113`-`114`). The paper EXISTS and the cited finding (consumer-known speculative projection that skips general materialization) is correctly attributed to the work, but the author list is WRONG: the actual authors are **Yinan Li, Nikos R. Katsipoulakis, Badrish Chandramouli, Jonathan Goldstein, Donald Kossmann** (Mison, PVLDB Vol 10, No 10, 2017). "Pavlo" and "Zhou" are not authors of this paper. This is the single load-bearing JSON-direct-projection grounding (SinkOnly = Mison's contract). | WebSearch + VLDB PVLDB vol10 p1118-li.pdf: authors are Li/Katsipoulakis/Chandramouli/Goldstein/Kossmann; "Pavlo, Zhou" absent. The cite appears at `2D-cost-model.md:59` and `2D-cost-model.md:113`. |
| CH1-V1-02 | 2F | medium | The Hyperscan citation (SRC-HYPERSCAN, `2F:67`) lists authors as **"Wang, Hua, Langdale, et al."** — the second author is **Hong** (Yang Hong), not "Hua". Real paper, correct venue (NSDI '19), correct year, and Langdale is a genuine co-author; only the second surname is mis-transcribed. | WebSearch + dblp/USENIX NSDI '19: "Hyperscan: A Fast Multi-pattern Regex Matcher for Modern CPUs", Xiang Wang, Yang Hong, Harry Chang, KyoungSoo Park, Geoff Langdale, Jiayu Hu, Heqing Zhu. No author "Hua". |
| CH1-V1-03 | 2D | low | `NormalizeDirectSinkCost` is cited at `backend_egraph.rs:76` (`2D:52`,`:53`,`:76`,`:96`). The named struct is defined at `backend_egraph.rs:191` and instantiated `let normalize = NormalizeDirectSinkCost;` at `:75`; line `:76` is inside the rewrite-application block but is not the symbol's definition or instantiation line. The substantive claim (the rewrite is LIVE, run through `BackoffScheduler`, extracted via `DecisionCostModel`) is CORRECT and verified — only the `:76` locator is imprecise. | `rg NormalizeDirectSinkCost crates/passes/src/backend_egraph.rs` → `:75` (instantiation), `:191`/`:193` (struct + impl). The pipeline (`:73` scheduler, `:83`-`84` extractor) is live as claimed. |
| CH1-V1-04 | 2A | low | The simdjson paper is cited as **"VLDB 2019"** in one row (`2A:54` / SRC-SIMDJSON-PAPER `2A:120`) but as the correct **"VLDB Journal 2019"** in 2B/2D/2E/2F. The canonical venue is *The VLDB Journal* 28(6) (a journal), not the VLDB conference. arXiv:1902.08318 is correct everywhere. Internally inconsistent venue label for the same most-cited paper. | WebSearch + Springer link.springer.com/article/10.1007/s00778-019-00578-5: venue is *The VLDB Journal* 28(6), 2019. 2A:120 says "VLDB 2019"; 2E:66, 2F:65, 2D:114-115 say "VLDB Journal 2019". |

## Enumeration under the lens (grounding/refutation rows judged)

Every dossier was read in full and its load-bearing rows judged. Per-dossier
disposition with the basis:

- **2A — REVISE** (CH1-V1-04). 24 primary sources. Verified: simdjson
  `parse_many.md:54-57` and `basics.md:344-350` match VERBATIM at the pinned SHA
  `79bbba…8ce5`; sonic-rs `README.md:78-90` ("directly parses the JSON into a
  Rust struct, and there are no temporary data structures") matches VERBATIM at
  `03545a…4bc5`; cssparser SHA `4c4948…4c71` is the live `HEAD` (git ls-remote);
  lightningcss SHA `ec1652…ebcc` resolves HTTP 200; the in-tree
  `checkasm_parity.rs:3` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" and
  `generated.rs:304`-`307` "rich, lazy, not eager, not flattened" are exact. The
  CSS refutation rows (broadcast / four-counter / fact-stream) correctly state
  the implementation's position. Only defect: the "VLDB 2019" venue label.
- **2B — ACCEPT**. 31 sources. The three SK-V18 §6 primitives verify: the eq-set
  NEON body at `aarch64/byte_class_from_eq_set_64.rs:34-72` (four `vld1q_u8`
  stripes, `vceqq_u8` fan, `vorrq_u8` OR-reduce) is real; the movemask
  shift-add path at `:79-89` is real; `find_css_significant` two-fan OR-reduce
  at `runtime_simd.rs` (`set_a[8]`/`set_b`, `byte_class_from_eq_set_64 |
  byte_class_from_eq_set_64`, `trailing_zeros`) is real; the dead-caller proof
  (only `#[cfg(test)]` at `lib.rs:574`, `significant_ref` at `:506`) is exact;
  14 `checkasm_*.rs` harnesses exist. The Arm ACLE intrinsics and Lemire/Langdale
  movemask lineage are correctly attributed. The neutrality-demotion refutation
  matches 1E. (Inherits the Mison/Hyperscan/simdjson cites only by reference, not
  re-stated here.)
- **2C — ACCEPT**. The single sharpest verifiable assertion — that the Lock 14
  self-gate `rg 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'` "is
  FALSIFIED/RED (asserts ZERO, returns 13)" — is VERIFIED EXACTLY: `LOCKS.md:349`
  literally states that command "returns ZERO", and the live grep over
  `crates/ir/src/`+`crates/analysis/src/` returns **13**. The 9-ident
  `PRODUCTION_MANIFEST_TABLE` at `strategy.rs:137-185`, `for_grammar_with_manifest`
  at `:216`, and `css_types.rs` (66 LOC host shim) all resolve. Pratt POPL 1973
  (DOI 10.1145/512927.512931) verified. W3C spec URLs are stable primary sources.
  No defect under CH1.
- **2D — REVISE** (CH1-V1-01 high, CH1-V1-03 low). The "STALE V2 refutation now
  live" claims verify: `NormalizeDirectSinkCost` rewrite is LIVE (`:75`/`:191`),
  `select_lowering` has exactly 5 shapes (`lower/mod.rs:18-24`),
  `collapsed_stage::lower_rule` delegates to `tape_plan::render_rule`, the
  `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork is real
  (`grammar_provider.rs:40-42`). iburg DOI 10.1145/151640.151642
  (Fraser/Hanson/Proebsting, LOPLAS 1(3) 1992) verified; egg DOI 10.1145/3434304
  (Willsey et al., PACMPL POPL 2021) verified; OR-Tools CP-SAT URL verified. The
  Mison author list (CH1-V1-01) is the materially-wrong attribution; the `:76`
  locator (CH1-V1-03) is imprecise.
- **2E — ACCEPT**. 24 sources, including the most-suspicious citation in the
  packet: **Lemire 2026-04-19** "The fastest way to match characters on ARM
  processors?" — future-dated relative to the dossier's own 2019/2024 sources, so
  flagged for direct verification. It EXISTS (current date is June 2026), is a
  real Lemire post, and the framing is HONEST: SVE2 `match` is "the fastest", the
  post confirms Apple has NOT adopted SVE2 (grounding the SVE2-absent-on-M5-Max
  refutation), and 2E correctly attributes the `vceqq_u8`-eq-fan-as-deployable
  route to the post's **comments** (label "(comments)" in SRC-LEMIRE-2026-MATCH),
  not Lemire's main argument. Kutenin Arm Community blog (shrn/`vshrn_n_u16`-by-4
  movemask, 10-15% SPEC CPU 2017 on strlen/memchr/memcmp) verified. Validark
  `ld4` interleaved-vectors post verified. The local movemask divergence
  (`byte_class_from_eq_set_64.rs:79` shift-add vs `movemask.rs:5`
  `vshrn_n_u16::<4>`) is real and exactly as cited. SVE2-`svmatch`-refuted row
  matches the literature. No defect under CH1.
- **2F — REVISE** (CH1-V1-02). The central refutation — that the upstream
  `parse-that` crate carries a full `scan/` substrate V2 never audited — is
  VERIFIED: `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/scan/`
  exists with `balanced.rs` (`scan_balanced` at `:26`, `BalancedScanConfig`,
  `find_first_of_nibble_lut`, the `<=8` cap `debug_assert` at `:44-45`),
  `structural_bitmap.rs` (`classify_16`, `PaddedView`), `quote_parity.rs`,
  `number_simd.rs`. The float-fallback re-confirmation verifies:
  `parse-that-regex/src/number/mod.rs:271` is `text.parse::<f64>()...` and
  `eisel_lemire/mod.rs:167` returns `None` "Ambiguous rounding — fallback needed".
  `count_top_level_commas` live-consumes `byte_class_from_eq_set_64` +
  `bracket_depth_mask_64` at `runtime_simd.rs`. simdjson/Langdale-Lemire/CSS-Syntax
  cites correct. Only defect: the Hyperscan author "Hua"→"Hong".

## Refuted-technique rows checked against the literature

- 2A/2B/2C/2E "x86/AVX-512 cannot close an M5 Max row; x86 deleted in skinny" —
  position is a project-scope decision, not a literature misstatement; the
  underlying intrinsics (Intel guide, GFNI/VPCLMUL) are real. ACCEPT-consistent.
- 2A/2E "simdjson stage 1 does NOT justify retained class columns / sidecars" —
  matches `parse_many.md:54-57` (stage 2 builds the tape FROM stage-1 indexes;
  transient projection). Correct reading.
- 2E "NEON `svmatch_u8` cannot be ported; MATCH/NMATCH is SVE2, host lacks
  FEAT_SVE2" — matches Arm SVMATCH doc + Lemire-2026 (SVE2 not on Apple). Correct.
- 2F "`find_css_significant` cannot be wired as-is (flat skip vs recursive
  shell)" — matches the in-tree source (flat `:169` fn vs recursive
  `find_component_delim`/`consume_balanced_at`). Correct.
- 2D "(STALE V2) zero-rule scaffold / marker-string lowerers" superseded — the
  live rewrite + delegating lowerers verify the supersession. Correct.

No refuted row misrepresents the cited literature's actual position.

## Fold Requirements

1. **2D (CH1-V1-01, blocking):** Correct the Mison author list at
   `2D-cost-model.md:59` and `2D-cost-model.md:113`-`114` to "Li, Katsipoulakis,
   Chandramouli, Goldstein, Kossmann" (or "Li et al."), PVLDB Vol 10(10), 2017.
   "Pavlo, Zhou" must be removed — they are not authors of this paper.
2. **2D (CH1-V1-03):** Re-anchor the `NormalizeDirectSinkCost` citation from `:76`
   to the symbol's actual lines (`backend_egraph.rs:75` instantiation, `:191`
   definition), or cite the `:73`-`:84` scheduler/extractor block as a range.
3. **2F (CH1-V1-02):** Correct the Hyperscan author "Wang, Hua, Langdale" →
   "Wang, Hong, … Langdale" at `2F-parse-that-gaps.md:67`.
4. **2A (CH1-V1-04):** Normalize the simdjson venue to "The VLDB Journal 28(6),
   2019" at `2A-sota-landscape.md:54` / SRC-SIMDJSON-PAPER `2A:120`, matching the
   2B/2D/2E/2F label.

## Convergence Impact

These REVISE findings BLOCK T-P2 V1 convergence until folded, but none is a
REJECT: across exhaustive spot-verification (every academic paper, every pinned
commit SHA, every spot-checked in-tree `path:line`, every spot-checked upstream
doc line-range, and the single most-suspicious future-dated Lemire-2026 blog) I
found NO confabulated source, NO non-resolving locator on a load-bearing local
citation, and NO refuted-technique row that misstates the literature. The defects
are attribution/locator/venue precision (Mison authors, Hyperscan author,
`:76` symbol line, "VLDB"→"VLDB Journal"), correctable in place without
disturbing any grounded or refuted verdict.

TALLY accept=3 revise=3 reject=0
