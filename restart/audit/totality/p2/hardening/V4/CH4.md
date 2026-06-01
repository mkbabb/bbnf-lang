# CH4 COST — SK-V18 T-P2 CHALLENGE (cycle V4)

Lens name: CH4 COST. Every grounded primitive must carry an admission cost —
scalar reference + checkasm parity per Lock 16, a same-wave consumer named,
realistic LOC/risk, no orphan-kernel research. Confabulated/unverifiable
citations and refuted-route groundings are inadmissible.

Disposition: `REVISE`.

Target packet (SK-V18, regenerated 2026-06-01): `2A-sota-landscape.md`
(cycle SK-V18-T-P2), `2B-primitive-vocabulary.md` (V3-SKV18),
`2C-grammar-neutrality.md` (V3), `2D-cost-model.md` (V3-SKV18),
`2E-host-arch-esoterica.md` (V6-SKV18), `2F-parse-that-gaps.md` (V3).

The cost-field schema is `T-P2-V2-FOLD-ADDENDUM.md:27-64` (transfer_reason /
admission_gate / verification_action / close_status / loc_estimate / risk_class /
wave_owner / hard_cap_fit; plus SIMD-row scalar_reference / parity_or_checkasm /
hardware_gate / same_wave_consumer / row_movement_target; plus the v+1 columns
`rollback path` / `abrogate threshold`). The live wave-band authority is SPEC §8
`restart/skinny/tranches/sk-v18/SPEC.md:436-444` (G1/G2/G3/G4/G5-G6/PROVE each
`≤450 hand source/test/gate LOC`, `≤90 min wave wall`, `30/45 min/redress`).

## V3 fold verification (the three CH4-V3 obligations LANDED)

All three prior CH4-V3 REVISE folds are confirmed present and correct:

- **CH4-V3-01 (2C lacked a per-row cost manifest)** — FOLDED. 2C now carries
  `## V3 SK-V18 Cost Manifest (CH4 per-row loc_estimate / risk_class)`
  (`2C:339-363`), a 13-row table with `wave_owner / loc_estimate / risk_class /
  rollback / abrogate` keyed to the live §8 band, mirroring 2B/2D/2E/2F. Plus the
  `## V3 Wave-Owner Re-key` paragraph (`2C:322-337`) mapping every SK-V15 `W#` to
  the live `P4/P5/G1..H1` ledger. The SK-V19-receiver rows carry `tranche_scope`
  inline (`2C:336-337,361-363`), not an SK-V18 band.
- **CH4-V3-02 (2C missing the CSS-typed second-seam firewall cost)** — FOLDED.
  The `RELOCATED-SEAM-FIREWALL — CSS-typed second seam (css_provider_source==
  generated)` row is live at `2C:357` (`≈ +5 gate LOC`, LOW, additive grep), and
  its grounding row at `2C:219` ("SECOND SEAM (CSS side channel)") names the
  `crates/core/src/runtime/css_l4/` hand-owned surface the `RuntimeTarget`-field
  firewall does not fence. The CSS-typed seam is now separately budgeted.
- **CH4-V3-03 (2E Lemire-2026 eq-fan attribution)** — FOLDED. The `(comments)`
  qualifier rides the Executive Summary (`2E:48-53`), Assertion 3 (`2E:188-194`),
  Source Registry SRC-LEMIRE-2026-MATCH (`2E:77`), and the SECTION-A G6 row
  (`2E:112`). Spot-verified live this pass (see below): the qualifier is correct.

## Citation spot-verification (load-bearing, this lens)

Every load-bearing cost grounding was confirmed against a primary source or the
on-disk tree. No confabulation found.

External (WebFetch / WebSearch):

- **Lemire-2026 ARM match** (the G6 eq-fan deployable-route grounding + the
  CH4-V3-03 attribution caveat): `lemire.me/blog/2026/04/19/the-fastest-way-to-
  match-characters-on-arm-processors/` — VERIFIED LIVE. Exact title/date confirmed.
  The BODY frames SVE2 `match` as the fastest (16.0 vs 15.5 GB/s NEON, 25% fewer
  instrs) and presents the Langdale/Lemire table-driven (TBL/shuffle) classifier as
  the conventional NEON route; the `vceqq_u8` eq-fan appears ONLY in a commenter
  suggestion (user "-.-", 2026-04-20), NOT the author's benchmark text. 2E's
  framing (`:50`, `:112`, `:188-194`) is now accurate to the source. Honest.
- **Kutenin NEON / SHRN movemask** (the `Movemask64Shrn ≈10 LOC` cost grounding):
  the post "Bit twiddling with Arm Neon: beating SSE movemasks, counting bits and
  more" is REAL (Arm Developer Blog, 2022-08-29), about exactly the SHRN/movemask
  technique 2E cites. The post exists and the technique is faithfully attributed.
  CAVEAT (CH4-V4-04 below): I could NOT confirm the specific "10–15% SPEC CPU 2017
  on strlen/memchr/memcmp" figure verbatim from the source; it is presented
  UNQUALIFIED in the 2E Source Registry row (`2E:75`) though correctly fenced in
  the grounding-table row (`2E:113`) and Assertion 4.
- **iburg** (2D R-A cost-model spine): Fraser/Hanson/Proebsting LOPLAS 1(3) 1992,
  DOI 10.1145/151640.151642 — VERIFIED real (confirmed prior CH4-V3 + standing
  citation). Dynamic-programming dispatch-on-selected-pattern grounds R-A. Honest.

Local (on-disk, this lens):

- **`find_css_significant` salvage kernel** (drives the `≤150 LOC` retarget
  estimate in 2B/2E/2F and the SKV18-A2 "salvage not new kernel" assertion):
  `runtime_simd.rs:169-216` — VERIFIED EXACT. Two-fan split `set_a=fixed[..8]` /
  `set_b=fixed[8]⧺delimiters[..≤4]`, `byte_class_from_eq_set_64(block,set_a) |
  byte_class_from_eq_set_64(block,set_b)` at `:199`, `mask.trailing_zeros()`,
  scalar tail `fixed.contains || delimiters.contains`. Dead-caller is
  `#[cfg(test)]`-only at `lib.rs:574`; `rg find_css_significant` finds no live
  caller. The salvage is real → the ≤150 retarget estimate is credible.
- **`count_top_level_commas` live CSS consumer** (the same-wave-consumer cell for
  the eq-set kernel): `runtime_simd.rs:29,44,47,56,199` — VERIFIED consuming
  `byte_class_from_eq_set_64` + `bracket_depth_mask_64`. The "CSS-only by live
  consumer, structurally neutral by caller-data" disclosure is accurate. The
  source-comment INACCURACY 2B/2C/2F flag (`runtime_simd.rs:6-7` "the same kernel
  JSON's scan_structurals rides") is real — confirmed a same-wave G6 source-fix
  obligation, correctly disclosed.
- **SHRN movemask divergence** (2E `Movemask64Shrn`, the `≈10 LOC` LOW-risk
  kernel-internal swap): `movemask.rs:5` uses canonical `vshrn_n_u16::<4>`;
  `byte_class_from_eq_set_64.rs:79-89` carries a DUPLICATE local `movemask_u8x16`
  on the slow shift-add `vaddv_u8` path. VERIFIED EXACT — the divergence is real,
  the swap is a single-site kernel-internal change, bit-identity checkasm-gated.
- **2D DERIVED-not-new engine** (the `≈0 LOC` LAC-2D-V3-03 regression-guard and the
  G3 `≤450 LOC` un-fork cost): `backend_egraph.rs` — `NormalizeDirectSinkCost`
  struct `:191`, impl `:193`, instantiated `:75` inside the `enable_rewrites`
  branch (`:43,:74`); `BackoffScheduler` `:73`; `DecisionCostModel` `:259,:83`;
  `Extractor::new` `:84`. VERIFIED the engine EXISTS → "DERIVED-not-new / ≈0 LOC
  guard" is honest. `RuntimeEmitterKind{CompiledLowering,RequestFacts}` fork live
  at `grammar_provider.rs:39-43` → the G3 DELETE target is real.
- **REDRESS production precedent + cautionary priors** (the cost/risk grounding the
  G6 retarget, the `Movemask64Shrn` and `Interleave4Classify` ledger fences, and
  every "net-win deferred to H1" rest on): REDRESS 144 `:4418-4438`
  `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT — Track 1 `444.208` vs `434.1316` Mbps,
  `+109.87%`, `byte_class_from_eq_set_64` wired into CSS `Scanner::scan_block` —
  VERIFIED EXACT. REDRESS 96/97/98 `:2928-2933` `G-W3-UNION-SUBSTRATE` retired (M5
  Max wide-issue scalar cheaper than a streamed SIMD cursor) — VERIFIED EXACT. The
  ledger framing across 2B/2E/2F is honest, not asserted.
- **checkasm harness count** (2F frontmatter "13 differential harnesses, 12
  single-kernel + 1 aggregate + checkasm_common helper, NOT 14 kernels"):
  `ls checkasm_*.rs` = 14 files = 12 `checkasm_<primitive>.rs` + `checkasm_parity.rs`
  (aggregate) + `checkasm_common.rs` (helper). VERIFIED — 2F's correction is exact.
- **Orphan-kernel discipline** (the load-bearing test of THIS lens): EVERY no-
  consumer / SECTION-B primitive across all six dossiers carries `0`, `n/a`, or an
  explicitly CONDITIONAL `≤N if ever built / if reopened` LOC with a `DO NOT author
  / DEFER permanently / no orphan kernel` abrogate clause (2E `:143-148`, 2F
  `:101-103`, 2A `:58` sonic-rs SK-V19-receiver, 2D `:98` CollapsedStage committed-
  vs-conditional split). NO no-consumer primitive carries a committed SK-V18 budget.
  This is exactly what the lens demands.

## Enumeration under CH4 COST

| # | dossier / row | disposition | basis |
|---|---|---|---|
| 1 | 2B eq-set member scan (`byte_class_from_eq_set_64`, SKV18-A1) — `≈0 LOC`, LOW, body live | ACCEPT | NEON body verified; scalar oracle + checkasm + live CSS `count_top_level_commas` consumer all present. Full Lock-16 cell. |
| 2 | 2B/2E/2F find_component_delim NEON retarget (salvage two-fan OR-reduce) — `≤150 LOC`, MED-HIGH | ACCEPT | Salvage kernel verified exact at `runtime_simd.rs:169-216`; REDRESS 144/96/98/126 ledger fence verified; rollback (revert to scalar) + abrogate (REDRESS-98 risk → keep scalar, G6 outcome C) present and identical across 2B/2E/2F. |
| 3 | 2E `Movemask64Shrn` SHRN-by-4 swap — `≈10 LOC`, LOW, bit-identity checkasm-gated | ACCEPT | SHRN divergence verified exact (`movemask.rs:5` vs `byte_class_from_eq_set_64.rs:79-89`); single-site kernel-internal swap; ledger-fenced against any Mbps claim. |
| 4 | 2E SECTION-B host-present-no-consumer rows (TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3) | ACCEPT | All carry `0` / `n/a` / conditional `≤N if reopened` with `DO NOT author / DEFER permanently / no orphan kernel`. Exemplary orphan-kernel discipline. |
| 5 | 2D R-A G3 un-fork — `≤450 LOC`, HIGH, + LAC-2D-V3-03 `≈0 LOC` regression-guard | ACCEPT | Engine verified to EXIST (`NormalizeDirectSinkCost` :191/:193/:75); G3 relocates the consumer; rollback (PATH-local revert) + abrogate (DELETE-rebuild per [abrogate-before-patch]) present. |
| 6 | 2D CollapsedStage cost SPLIT (committed `≈0` inert / conditional `≤450` G5/G6-gated) | ACCEPT | The CH4-V2-02 split is intact; the ≤450 is no longer budgeted against the inert slot; promotion gated on profile + transient-mask + scalar oracle + checkasm + consumer + clearing REDRESS 96/98. |
| 7 | 2A T2A-V1-SOTA-JSON-003 sonic-rs — SK-V19-receiver, blocked, conditional `150-350/leaf` | ACCEPT | The CH4-V3-03 demotion landed; correctly NOT charged an SK-V18 budget; full v+1 cells (rollback/abrogate/hard_cap_fit) present. |
| 8 | 2F per-gap cost manifest (PTG-2F-09..13) + provenance-reconcile row | ACCEPT | Every gap keyed to §8 band with rollback/abrogate; provenance reconcile is a `≈0 LOC` decision; the substrate-presence refutation of the V2 "gap" is verified against the upstream `parse-that` tree. |
| 9 | 2C CH4-V3-01 cost manifest + CH4-V3-02 second-seam firewall row | ACCEPT | Both folds landed and well-formed; gate-only rows carry small additive gate LOC, no body double-count. |
| 10 | **2C `ONE-GENERATOR-GENERALISATION-THESIS` umbrella row (`2C:351`) — single `≤450 hand LOC` for "the un-fork + grammar-DERIVED CSS provider together"** | **REVISE** | (see CH4-V4-01) The umbrella charges ONE `≤450` for the COMBINED G2 + G3 build. But SPEC §8 budgets G2 its own `≤450` (`SPEC:439`: `lower/css_scan.rs` + `css_scan_direct.rs` + primitive shell + arg-derivation) AND G3 its own `≤450` (`SPEC:443`), and 2D itself charges G3 `≤450` separately (`2D:67,:95`). The real generalization-thesis build cost is the SUM of two `≤450` wave bands across two waves, not a single `≤450`. The "no separate body" note is wrong — the bodies ARE G2 and G3. Understates the thesis cost by ~half. |
| 11 | **2E Source Registry SRC-KUTENIN-NEON (`2E:75`) — the unqualified "10–15% SPEC CPU 2017 on strlen/memchr/memcmp" figure** | **REVISE** | (see CH4-V4-04) The post is real and the technique faithfully attributed, but the specific "10–15% SPEC CPU 2017" percentage is presented UNQUALIFIED at the registry site. I could not confirm that exact figure from the source; the grounding-table row (`:113`) and Assertion 4 correctly fence it as "the technique's published lineage, NOT a promotable bbnf row figure," but the Source-Registry presentation reads as a load-bearing number. Qualify the figure at `:75` ("Kutenin-reported, unverified-by-this-pass / lineage-only") or drop the percentage. |
| 12 | **2A T2A-V18-ASMJSON-001 (`2A:180`) — `primitive_cost=none (G5 is a deletion/neutralize)`, missing the v+1 `rollback path` / `abrogate threshold` columns** | **REVISE** | (see CH4-V4-05) The neighbouring SK-V18 row T2A-V1-SOTA-JSON-003 (`:58`) and all of 2B/2C/2D/2E/2F carry the v+1 `rollback`/`abrogate` cells per the FOLD-ADDENDUM schema; this row carries `primitive_cost=none` but no `rollback`/`abrogate`. For a deletion route the rollback (re-author a JSON SIMD kernel) and abrogate (if the scan-free assumption falsifies under PROVE/Sheets) are NON-trivial and schema-required. Add the two cells. |

## Issues opened by this lens (V4)

- **CH4-V4-01 (2C cost-double-count, REVISE).** `2C:351`
  `ONE-GENERATOR-GENERALISATION-THESIS` charges a SINGLE `≤450 hand LOC` umbrella for
  the COMBINED G2 (grammar-derived CSS provider) + G3 (un-fork) build. SPEC §8 budgets
  G2 and G3 as SEPARATE `≤450` waves (`SPEC:439`, `:443`), and 2D charges G3 its own
  `≤450` (`2D:67,:95`). Correction: re-state the umbrella cost as "the SUM of the G2
  (≤450) + G3 (≤450) bands — owned in the 2C G2/G3 rows and 2D LAC-2D-V3-01, not a
  single ≤450 here," so the generalization thesis is not under-budgeted by half.

- **CH4-V4-04 (2E unqualified Kutenin SPEC-CPU figure, REVISE).** `2E:75` Source
  Registry presents "10–15% SPEC CPU 2017 on strlen/memchr/memcmp" as an unqualified
  fact. The post and technique are verified real; the specific percentage is NOT
  confirmed by this pass and is correctly fenced as lineage-only DOWNSTREAM (`:113`,
  Assertion 4) but reads load-bearing at the registry. Correction: qualify the figure
  at `:75` as Kutenin-reported / unverified-this-pass / lineage-only, OR drop the
  percentage and keep the qualitative "2-instr canonical movemask" grounding.

- **CH4-V4-05 (2A asmjson row missing v+1 cost cells, REVISE).** `2A:180`
  T2A-V18-ASMJSON-001 carries `primitive_cost=none` but lacks the `rollback path` /
  `abrogate threshold` columns the FOLD-ADDENDUM schema and every peer row carry.
  Correction: add `rollback path = re-author the JSON SIMD classifier if a future
  profile shows json/scan.rs hot` and `abrogate threshold = if the S-P1 scan-free
  finding falsifies under PROVE/Sheets, re-open the JSON-kernel question rather than
  ship a neutralized json/scan.rs that silently regresses`.

## Items confirmed NOT defects (orphan-kernel + over-budget axes, scanned explicit)

- **No orphan kernel admitted.** Every primitive with `same_wave_consumer = NONE`
  carries `0`/`n/a`/conditional LOC with a DO-NOT-AUTHOR/DEFER abrogate; no no-consumer
  kernel carries a committed SK-V18 budget. The G6 retarget, the only committed SIMD
  build, is profile-anchored (94.1% hot leaf) WITH its same-wave generated call site.
- **No confabulated citation.** iburg, egg/POPL-2021, Mison, Langdale/Lemire-2019,
  Lemire-2026, Kutenin, OR-Tools CP-SAT, FFmpeg checkasm — all real; the one figure I
  could not verbatim-confirm (Kutenin SPEC-CPU %) is a REVISE, not a confabulation
  (the source and technique are real).
- **No refuted-route grounding charged a cost.** Float no-fallback (2F), SVE2 svmatch
  (2E, host-absent), x86 AVX-512 close (2B/2C/2E, P1-deleted), neutrally-named CSS-only
  primitive (2C `:360`), retained frame-stack rebuild (2B SKV18-A5) — all carry `0
  LOC` / `n/a` / REFUTED, none budgeted.
- **No double-count beyond CH4-V4-01.** The 2C gate-only rows (`:352-360`) carry small
  ADDITIVE gate LOC explicitly distinct from the kernel bodies they gate; the eq-set
  `≈0 LOC` (body live) does not re-charge the find_component_delim `≤150` (retarget).
  The single double-count is the umbrella row.

## §3Z note

V4 is one cycle below the V≤5 ceiling. This `REVISE` opens three folds (CH4-V4-01,
-04, -05), all light-touch cost-cell corrections on otherwise-grounded rows — none
re-litigates a primitive's admission, citation, or wave owner. The shape canon, the
engine-exists re-grounding, the orphan-kernel discipline, and the REDRESS ledger
fences all survive intact. Fold the three corrections into V5 (or close at V5 ceiling
with them surfaced); two of three are single-cell schema parity, one is an
under-budget restatement.

TALLY accept=9 revise=3 reject=0
