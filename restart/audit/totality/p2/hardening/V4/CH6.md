# CH6 — ANTI-PAPER-CLOSE (SK-V18 T-P2 cycle, lens V4)

Lens: CH6 ANTI-PAPER-CLOSE. No dossier may claim a technique "validated" on
citation-density alone; reference-stuffing (N sources cited, none integrated) is
flagged; every grounded technique must state the bbnf-specific reason it
transfers; no deferral to "a later pass". Spot-verify the most load-bearing
citations against the live source. Cycle V1 expected >=30% REVISE.

Disposition: **ACCEPT** (6/6). The single V3 REVISE (CH6-V3-01, the Hyperscan
SHUFTI/TRUFFLE name attribution on 2F) is folded and re-verified LIVE against the
Hyperscan codebase. Every external citation, every in-tree path:line anchor, the
host probe, and the ledger fences spot-verified this cycle resolve to a real
source with an accurate characterisation. This is an independent live re-review,
not a re-run of V1/V2/V3.

## Entry state — the V3 REVISE fold, re-verified LIVE this cycle

V1/CH6 returned 4 REVISE (CH6-V18-01..04); V2/CH6 returned 2 (CH6-V2-01 wrong
title, CH6-V2-02 under-rooted path); V3/CH6 returned 1 (CH6-V3-01, Hyperscan
abstract-name mis-sourced to the NSDI paper text). The CH6-V3-01 fold is the only
target-packet change since V3 and is landed + independently re-verified here:

- **CH6-V3-01 (2F SRC-HYPERSCAN name attribution)** — FOLDED + RE-VERIFIED LIVE.
  `2F:21` (frontmatter `revised`) and `2F:71` (SRC-HYPERSCAN) now read: the
  author list is VERIFIED EXACT (Wang/Hong/Chang/Park/Langdale/Hu/Zhu) and the
  NSDI 2019 paper is retained as the **project/lineage citation** (graph
  decomposition + SIMD-accelerated string/FA matching), but the abstract-primitive
  NAMES `SHUFTI`/`TRUFFLE` are now attributed to the **Hyperscan codebase**
  (`src/nfa/shufti.c`/`shufti.h`, `truffle.c`/`truffle.h`) + Langdale's
  branchfree.org shuffle-based-matching writing, with the explicit disclosure
  "NOT the NSDI 2019 paper text (which does not name them)." The PTG-2F-10 row
  (`2F:86`) and Assertion 2 (`2F:118`-`120`) carry the same corrected attribution.
  **WebFetch of `github.com/intel/hyperscan/blob/master/src/nfa/shufti.c` confirms
  the file exists and its top-of-file comment reads "Shufti: character class
  acceleration. Utilises the SSSE3 pshufb shuffle instruction" — the exact
  nibble-LUT shuffle classifier (`mask_lo[c&0x0F] AND mask_hi[c>>4]`) the row
  describes. WebFetch of `.../truffle.c` confirms "Matches a byte in a charclass
  using three shuffles".** WebSearch independently confirms Hyperscan's `hwlm.c`
  includes `nfa/shufti.h` and `nfa/truffle.h`. The fold substituted the CORRECT
  source for the NAME (the codebase, which DOES bear `SHUFTI`/`TRUFFLE`), not a
  second unverified one — the trap this lens exists to catch. The VERIFIED stamp
  is now legitimate: the cited text bears the cited name.

The single V3 paper-close vector is closed and re-verified against the live
upstream source, not merely against the consolidated verdict.

## Citations spot-verified this cycle (load-bearing, V4 — independent live checks)

| citation | dossier | result |
|---|---|---|
| Hyperscan codebase `src/nfa/shufti.c` exists; top-comment "Shufti: character class acceleration. Utilises the SSSE3 pshufb shuffle instruction"; `truffle.c` "Matches a byte in a charclass using three shuffles" | 2F (`:71`,`:86`) | **VERIFIED LIVE (WebFetch ×2)** — both files exist with the cited shuffle-classification purpose; the SHUFTI/TRUFFLE NAME is genuinely in the codebase, NOT the NSDI paper — the CH6-V3-01 fold is legitimately VERIFIED |
| Hyperscan NSDI 2019 author list Wang/Hong/Chang/Park/Langdale/Hu/Zhu (retained as project/lineage cite) | 2F (`:71`) | author list VERIFIED EXACT (carried from V3); the row no longer sources the NAME to the paper text |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" — BODY endorses SVE2 `match` (16.0 vs 15.5 GB/s, −25% instrs) as fastest + TBL/shuffle as the conventional NEON route; the `vceqq_u8` eq-fan appears ONLY in the COMMENT thread (commenter "-.-", who concedes "won't be as fast as SVE's MATCH") | 2E (`:49`-`53`,`:77`,`:112`,`:184`-`194`) | **VERIFIED LIVE (WebFetch)** — the body-vs-comment split is EXACTLY as 2E discloses; the eq-fan's binding grounding is correctly re-anchored to simdjson/Langdale-Lemire + the on-disk kernel, NOT a Lemire-2026 body endorsement (the CH4-V3-03 fold). This is the precise anti-paper-close move: a commenter route is NOT narrated as the author's published finding |
| Mison authors Yinan Li / Nikos R. Katsipoulakis / Badrish Chandramouli / Jonathan Goldstein / Donald Kossmann, PVLDB 10(10) 2017, DOI 10.14778/3115404.3115416 | 2D (`:60`,`:114`-`115`) | **VERIFIED EXACT (WebSearch)** — the V1 "Pavlo, Zhou" confabulation stays corrected |
| Pratt "Top Down Operator Precedence", 1st annual ACM SIGACT-SIGPLAN POPL symposium 1973, DOI 10.1145/512927.512931 | 2C (`:198`-`199`,`:216`) | **VERIFIED EXACT (WebSearch)** — title, venue (POPL '73), DOI all match; the negative-control integration is genuine |
| upstream `parse-that` scan substrate: `scan/balanced.rs:26` `scan_balanced`, `:45` `n <= 8` debug_assert, `scanners.rs:235`/`:262` `build_nibble_luts`/`find_first_of_nibble_lut` | 2F (`:73`,`:85`,`:186`) | **VERIFIED LIVE (on-disk ls+grep)** — the full `scan/` tree exists (balanced/structural_bitmap/quote_parity/number_simd/…); `scan_balanced` at `:26`, assert at `:45` (2F's `:44`-`:46` span is acceptable), nibble-LUT kernels in the parent `scanners.rs` as 2F's CH1-V3-02 fold relocated them |
| eq-set NEON body `byte_class_from_eq_set_64_neon`: `set.len() <= 8` assert, four `vld1q_u8` stripes, `vceqq_u8`/`vorrq_u8` fan | 2B (`:268`,`:283`), 2E (`:81`,`:112`), 2F (`:86`) | **VERIFIED EXACT** (`byte_class_from_eq_set_64.rs:33`-`45`,`:79`-`89`) |
| `find_css_significant` two-fan OR-reduce `set_a`/`set_b` split + `byte_class_from_eq_set_64(block,set_a) \| byte_class_from_eq_set_64(block,set_b)` + `trailing_zeros` | 2B (`:251`-`253`,`:311`-`314`), 2E (`:112`,`:199`), 2F (`:89`,`:271`) | **VERIFIED EXACT** (`runtime_simd.rs:169`,`:184`-`188`,`:199`) |
| eq-set kernel live consumer = CSS `count_top_level_commas`; `find_ascii_set_member64` has ZERO non-test/non-bench runtime caller; JSON `scan_dispatch` rides the DIFFERENT `byte_class_from_table_64` | 2B (`:75`,`:268`,`:289`-`293`), 2C (`:215`), 2F (`:86`,`:138`-`141`) | **VERIFIED EXACT** — `count_top_level_commas` at `runtime_simd.rs:29` consumes the eq-set kernel; `rg find_ascii_set_member64 runtime/src/` returns NOTHING (no runtime caller). The structural-neutral / CSS-only-consumer split is honest; the empirical dual-consumer claim is correctly REFUTED, not asserted |
| inaccurate `runtime_simd.rs:6`-`7` source comment ("the same kernel JSON's `scan_structurals` rides") named as a same-wave FIX obligation, NOT adopted | 2B (`:298`-`305`), 2C (`:215`), 2F (`:86`) | **VERIFIED** — comment present at `:6`; all three dossiers NAME it false and route it to a G6 source-fix rather than re-adopt its JSON-rides claim (refutation-first, not paper-close) |
| canonical SHRN movemask `movemask.rs:5` `vshrn_n_u16::<4>` vs shift-add `vaddv_u8` pack `byte_class_from_eq_set_64.rs:79`-`89` | 2E (`:82`,`:113`,`:241`) | **VERIFIED EXACT** — both bodies read as cited; the SK-V18 movemask-divergence finding is real |
| 13 differential checkasm harnesses = 12 single-kernel `checkasm_<primitive>.rs` + 1 aggregate `checkasm_parity.rs` + `checkasm_common.rs` helper (14 `checkasm_*.rs` files total) | 2A (`:193`,`:275`), 2B (`:73`), 2F (`:42`-`43`,`:74`) | **VERIFIED EXACT** (`ls tests/` = 14 `checkasm_*` files; 12 single-kernel + parity + common, the decomposition cited) |
| 5-shape `select_lowering` canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, no sixth | 2D (`:54`,`:57`,`:70`), 2C (`:217`) | **VERIFIED EXACT** (`lower/mod.rs:18`-`24`) |
| `NormalizeDirectSinkCost` live e-graph rewrite (V2 zero-rule supersession), `BackoffScheduler`, `enable_rewrites` branch | 2D (`:54`,`:77`,`:97`) | **VERIFIED EXACT** (`backend_egraph.rs:75` instantiation, `:191` struct, `:73` scheduler, `:43`/`:74` enable branch) — the activation is real, not a scaffold |
| `runtime_target_rows_collapsed` is PLANNED / not-yet-live (`rg … == 0`) | 2C (`:219`), 2D (`:56`,`:79`,`:96`) | **VERIFIED: rg returns 0** in `skinny/crates`+`skinny/xtask` — both dossiers honestly disclose the gate as not-yet-existing, never claiming a live gate |
| 9-row grammar-named `PRODUCTION_MANIFEST_TABLE` in the GENERIC `crates/ir` crate (Lock-14 self-gate falsifier: a grammar-named table in neutral-crate code) | 2C (`:202`,`:215`,`:218`) | **VERIFIED** — `strategy.rs:134` const + `idents: &["JsonParser","JsonGrammar"]` rows; `for_grammar_with_manifest` consumer at `:216`. The 9-ident leak is real |
| `css_types.rs` = 66 LOC in the generic `crates/core` crate | 2C (`:221` V3 ext) | **VERIFIED EXACT** (`wc -l crates/core/src/css_types.rs` = 66) |
| M5 Max host probe: FEAT_PMULL/DotProd/I8MM/CSSC/SHA3 = 1, FEAT_SVE2 ABSENT | 2E (`:55`,`:88`,`:131`), 2A/2B (refutations) | **VERIFIED EXACT (local sysctl)** — `Apple M5 Max`; FEAT_SVE2 not found (absent). The svmatch re-refutation rests on real hardware |

Every external citation, every in-tree path:line, the host probe, and every
ledger anchor resolve to a real source with an accurate characterisation. This is
the cleanest of the four cycles — the only open paper-close vector from V3 is now
closed against the live upstream codebase.

## Critical Findings (V4)

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V4-01 | ACCEPT | **The CH6-V3-01 fold is the model citation-precision repair.** 2F now sources the SHUFTI/TRUFFLE NAME to the Hyperscan codebase (`src/nfa/shufti.c`/`truffle.c`, both confirmed live with the cited shuffle-classifier purpose) + Langdale's branchfree writing, retains the NSDI 2019 paper as the project/lineage citation, and explicitly discloses the paper text does NOT name them. The VERIFIED stamp now asserts a check the named-source text PASSES — the inverse of the V2/V3 title/name laxity class. No paper-close. | Preserve; no fold. |
| CH6-V4-02 | ACCEPT | **The 2E Lemire-2026 body-vs-comment attribution is the model anti-paper-close on a subtle citation.** WebFetch confirms the post BODY endorses SVE2 `match` (host-absent) and the TBL/shuffle classifier as the conventional NEON route; the `vceqq_u8` eq-fan is a COMMENTER suggestion the commenter himself concedes is slower than SVE MATCH. 2E binds the eq-fan's grounding to simdjson/Langdale-Lemire + the on-disk kernel, NOT to a Lemire-2026 body endorsement, and carries the "(comments)" qualifier through the Executive Summary, Assertion 3, and the G6 grounding row (CH4-V3-03 fold). Narrating a commenter route as a published author finding is exactly the trap this lens forbids; 2E does not fall into it. | Preserve; no fold. |
| CH6-V4-03 | ACCEPT | **The G6 speedup deferral remains the anti-deferral template (gated, not prose).** 2A T2A-V18-DAV1D-002 (`:176`) and 2E (`:129`,`:216`,`:222`-`225`) defer every Mbps figure to the H1 quiet re-capture, but the deferral is to a SAME-tranche wave, GATED by the machine-checkable `g6_speedup_claim_emitted == false` exit gate, the S-P1 directional ratios (2.190/3.375/1.658/2.101) are marked NON-citable, and the falsifier ("any Mbps/× string in a G6 artifact pre-H1 is a REJECT") is named. "No deferral to a later pass" is satisfied by a gate. | Preserve; no fold. |
| CH6-V4-04 | ACCEPT | **The 2E SECTION A/B/C quarantine + REDRESS-fencing is exemplary against citation-count aggregation.** No SECTION-B no-consumer row (TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3) is countable as an SK-V18 admission — each carries `same_wave_consumer = NONE` and a "DO NOT author" abrogate threshold (`:142`-`148`); the explicit header (`:101`-`106`) states a SECTION-B "grounded" means the TECHNIQUE is real, not the primitive admitted. svmatch is REFUTED on the live host probe (SVE2 absent, re-verified). The Movemask64Shrn / Interleave4Classify / G6 net-win rows are LEDGER-fenced against the negatively-answered REDRESS 96/97/98 prior, so no Mbps is promotable off a kernel-internal-swap row. No citation density aggregates across wired/no-consumer rows. | Preserve; no fold. |
| CH6-V4-05 | ACCEPT | **2B / 2C / 2D / 2F carry no surviving paper-close vector.** 2B splits the eq-set neutrality into a STRUCTURAL claim (caller-supplied byte set, kernel names no grammar) vs a REFUTED empirical dual-consumer claim (`find_ascii_set_member64` has zero runtime callers, JSON rides `byte_class_from_table_64`), names the inaccurate `runtime_simd.rs:6`-`7` comment as a fix obligation, and reconciles the FSM/frame-stack route to DELETE-only after catching its own V2 manifest's latent retained-stack reintroduction (SKV18-A5). 2C's Pratt 7-level precedence tower is a genuine negative control (the `Nu8`-tagged-alt family correctly DEMOTED from the litmus, CSS 295× vs Sheets 21×), the three SK-V19-receiver rows self-disclose `tranche_scope` inline, and the Lock-14 self-gate falsification (grammar-named 9-ident table in generic `ir`) is live-verified. 2D's egg/iburg/BURG/OR-Tools spine is live (`NormalizeDirectSinkCost` at `:75`, 5-shape `select_lowering`, no sixth shape), the V2 zero-rule/marker supersessions are correct, Mison authors corrected, CollapsedStage LEDGER-fenced to diagnostic-only. 2F grounds the upstream `scan_balanced` against verified on-disk source and refutes its own V2 scope error. Every SIMD row carries the full scalar-oracle + checkasm + hardware-gate + same-wave-consumer manifest. | Preserve; no fold. |

## Reference-integration audit (per dossier, V4)

- **2A** — integrated, not stuffed; the 8-plane discipline
  (`parse_only|DOM|value|typed_direct|lazy|fact_stream|CSS_typed_document|CSSOM_value`)
  is the laundering-prevention mechanism; sonic-rs LazyValue and direct-to-struct
  verified (V3); the G6 deferral carries the machine-checkable exit gate; no JSON
  SIMD classifier authored (profile-first, `json/scan.rs` 0 samples → G5 neutralize).
  ACCEPT.
- **2B** — fully integrated; "source presence is not admission" is the spine; the
  eq-set neutrality is split structural-vs-empirical; the FSM/frame-stack route is
  DELETE-only after the self-caught sidecar reintroduction; the inaccurate in-tree
  comment is a fix obligation, not adopted. ACCEPT.
- **2C** — integrated for the 3-grammar SK-V18 witness; SK-V19-receiver rows carry
  inline `tranche_scope`; Pratt is a verified negative control; the Lock-14
  self-gate falsification (9-ident generic-crate table) and `css_types.rs`=66 are
  live-verified. ACCEPT.
- **2D** — integrated; the published spine (iburg/egg/BURG/OR-Tools) is live
  in-tree; Mison authors verified EXACT; five-shape canon held; no sixth shape;
  the `runtime_target_rows_collapsed` co-gate honestly disclosed as PLANNED.
  ACCEPT.
- **2E** — integrated for the two wired SECTION-A rows; SECTION-B no-consumer rows
  quarantined and labelled "DO NOT author"; the Lemire-2026 attribution split
  (body=SVE2/TBL, eq-fan=comments) verified live; svmatch refuted on the host
  probe; x86 diagnostic-only / P1-deleted. ACCEPT.
- **2F** — integrated against verified on-disk upstream + skinny source; the V3
  SHUFTI/TRUFFLE name fold landed and is live-verified against the Hyperscan
  codebase; the V2 title and path-root folds remain live. ACCEPT.

## On the >=30% REVISE expectation

V1 delivered 4/6 REVISE because the four heaviest paper-close vectors were
unfolded; V2 delivered 1/6, V3 delivered 1/6 as the foundation converged. V4 is
0/6: the only target-packet change since V3 is the CH6-V3-01 fold, and that fold
is independently re-verified LIVE — the Hyperscan codebase files
(`src/nfa/shufti.c`/`truffle.c`) exist and genuinely bear the `SHUFTI`/`TRUFFLE`
names the row now sources to them, the NSDI paper is correctly demoted to the
project/lineage cite with explicit "does not name them" disclosure, and the
VERIFIED stamp now asserts a check the cited text passes. Every other external
citation (Mison authors/DOI, Pratt POPL'73, Lemire-2026 body-vs-comment),
in-tree anchor, host probe, and ledger fence spot-verified this cycle resolves to
a real source with an accurate characterisation, and refutation is first-class
throughout (the eq-set empirical dual-consumer claim refuted; the inaccurate
in-tree comment named not adopted; the FSM frame-stack route DELETE-only; svmatch
refuted on hardware). The >=30% REVISE was a V1 expectation against an unfolded
packet; manufacturing additional REVISE findings to hit a fixed ratio against a
converged, live-verified packet would itself be a paper-close move this lens
forbids. The honest count is 0.

## Evidence Inspected

- Lens authority: `restart/audit/totality/p2/hardening/V3/CH6.md` (the CH6-V3-01
  fold requirement), `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`103`,
  and the prior SK-V18 CH6 verdicts V1/V2/V3.
- All six dossiers read in full (`2A`-`2F`, including the 2B `:294`-`437` tail and
  the 2C `:1`-`221` head).
- External citation verification (WebSearch/WebFetch this cycle): Hyperscan
  `src/nfa/shufti.c` + `truffle.c` (live file existence + top-of-file shuffle
  comment), Hyperscan `hwlm.c` includes (WebSearch), Lemire 2026-04-19 ARM-match
  (live body-vs-comment split), Mison authors/DOI (WebSearch), Pratt POPL'73 DOI
  (WebSearch).
- In-tree verification (ls/grep/sed/wc): upstream `parse-that/.../scan/` tree +
  `scan_balanced` `balanced.rs:26` + `n<=8` assert `:45` + `scanners.rs:235`/`:262`
  nibble-LUTs; eq-set NEON body `byte_class_from_eq_set_64.rs:33`-`45`,`:79`-`89`;
  two-fan `find_css_significant` `runtime_simd.rs:169`,`:184`-`188`,`:199`;
  `count_top_level_commas` `:29` live consumer; `find_ascii_set_member64`
  zero runtime callers; SHRN `movemask.rs:5`; 14 `checkasm_*` files (12+parity+common);
  5-shape `lower/mod.rs:18`-`24`; `NormalizeDirectSinkCost` `backend_egraph.rs:75`/`:191`;
  `runtime_target_rows_collapsed` rg=0; 9-ident `strategy.rs:134`/`:216`;
  `css_types.rs` wc=66.
- Local host probe (sysctl): `Apple M5 Max`; FEAT_PMULL/DotProd/I8MM/CSSC/SHA3=1;
  FEAT_SVE2 absent.

## Fold Requirements

None. All six dossiers ACCEPT. The CH6-V3-01 fold is landed and re-verified live;
preserve 2A's gated G6-deferral, 2E's SECTION A/B/C quarantine + Lemire-2026
body-vs-comment split + host-probe refutation, 2B's structural-vs-empirical
neutrality split + DELETE-only FSM reconcile, 2C's verified Pratt negative
control + Lock-14 self-gate falsification, 2D's live engine + five-shape canon,
and 2F's corrected Hyperscan attribution + upstream-substrate grounding as the V4
anti-paper-close template.

## Convergence Block

Does NOT block T-P2 convergence. V4 returns 6/6 ACCEPT under the CH6 lens. The
sole V3 REVISE (CH6-V3-01) is folded and independently re-verified against the
live Hyperscan codebase; every external citation, in-tree anchor, host probe, and
ledger fence resolves to a real source with an accurate characterisation;
refutation is first-class throughout; the deferred G6 speedup is gated, not
asserted; no citation-density aggregation is possible across the quarantined
SECTION-A/B/C structure. With V3 (5 ACCEPT / 1 REVISE on a single 2F-local defect,
now folded) followed by a clean V4, the CH6 lens reaches the V4 clean cycle the
V3 verdict named as the closing condition within the V<=5 ceiling.

TALLY accept=6 revise=0 reject=0
