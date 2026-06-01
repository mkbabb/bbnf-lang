# CH6 — ANTI-PAPER-CLOSE (SK-V18 T-P2 cycle, lens V5)

Lens: CH6 ANTI-PAPER-CLOSE. No dossier may claim a technique "validated" on
citation-density alone; reference-stuffing (N sources cited, none integrated) is
flagged; every grounded technique must state the bbnf-specific reason it
transfers; no deferral to "a later pass". Spot-verify the most load-bearing
citations against the live source. Cycle V1 expected >=30% REVISE.

Disposition: **ACCEPT** (6/6). This is an INDEPENDENT live re-review against the
upstream sources and the in-tree anchors — NOT a re-run of the V4 consolidated
verdict. Every load-bearing external citation I spot-checked this cycle
(iburg/LOPLAS, Mison/PVLDB, Hyperscan `shufti.c` codebase, Lemire-2017
`neonmovemask_addv`, Lemire-2026 body-vs-comment, Kutenin SHRN/SPEC-CPU) and
every in-tree path:line anchor I spot-checked (self-gate=13, `strategy.rs`
9-idents, `css_types.rs`=66, Pattern H=71, the SHRN-vs-`vaddv` movemask
divergence, `find_ascii_set_member64` zero-callers, the inaccurate
`runtime_simd.rs:6`-`7` comment, 14 `checkasm_*` files, the 5-shape
`lower/mod.rs`) resolves to a real source with an accurate characterisation. The
one citation NOT spot-verified by any prior CH6 cycle — the 2B `:269`
`neonmovemask_addv` Lemire-2017 attribution — I verified LIVE this cycle and it
is legitimate (the cited post bears the cited function), closing the last
unverified paper-close vector.

## Entry state — V4 returned 0/6; the prior fold chain, re-verified LIVE

V1/CH6 returned 4 REVISE (the four heaviest paper-close vectors: 2D Mison
authors confabulated, 2A G6-speedup deferral ungated, 2E reference-density
adjacency, 2C SK-V19 scope tag); V2/CH6 returned 2 (CH6-V2-01 wrong z3 title,
CH6-V2-02 under-rooted float path); V3/CH6 returned 1 (CH6-V3-01, SHUFTI/TRUFFLE
NAME mis-sourced to the NSDI paper text); V4/CH6 returned 0 after the
CH6-V3-01 fold landed. No target-packet change has occurred since V4. The lens
obligation at V5 is therefore an INDEPENDENT live re-verification that the
converged packet has not bit-rotted and that every "VERIFIED" stamp still asserts
a check the named source passes — including the ONE citation no prior CH6 cycle
spot-verified.

- **CH6-V3-01 (2F SHUFTI/TRUFFLE name)** — re-verified LIVE. WebFetch of
  `github.com/intel/hyperscan/blob/master/src/nfa/shufti.c` confirms the file
  exists and the top comment reads "Shufti: character class acceleration.
  Utilises the SSSE3 pshufb shuffle instruction" — the exact nibble-shuffle
  classifier. The NAME genuinely lives in the codebase; the NSDI 2019 paper is
  correctly retained as the project/lineage cite with the explicit "does not name
  them" disclosure (`2F:71`). The VERIFIED stamp asserts a check the cited
  source passes.

## Citations spot-verified this cycle (load-bearing, V5 — independent live checks)

| citation | dossier | result |
|---|---|---|
| **NEW THIS CYCLE — never CH6-verified before:** Lemire 2017-07-10 "Pruning spaces faster on ARM processors with Vector Table Lookups" defines `neonmovemask_addv` = `vandq_u16(input, {0x0101,0x0202,…,0x8080})` then `vaddvq_u16` horizontal reduce (the `vaddv`-based AArch64 movemask emulation) | 2B (`:269`) | **VERIFIED LIVE (WebFetch + WebSearch)** — the cited post genuinely defines `neonmovemask_addv` with the exact `vand`+`vaddvq` shape, DESPITE the post's title being "pruning spaces". 2B's "VERIFIED real, not confabulated" stamp is legitimate; the in-tree eq-set body's `vaddv_u8`-per-half pack is the same ADDV lineage. This is the last unverified paper-close vector in the packet, now closed |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" — BODY endorses SVE2 `match` (16.0 vs 15.5 GB/s, −25% instrs) as fastest + the conventional NEON route; the `vceqq_u8` eq-fan appears ONLY in the COMMENT by user "-.-", who concedes it "won't be as fast as SVE's MATCH" | 2E (`:77`,`:112`,`:184`-`194`) | **VERIFIED LIVE (WebFetch)** — the body-vs-comment split is EXACTLY as 2E discloses (Assertion 3 / CH4-V3-03 fold). The eq-fan's binding grounding is correctly re-anchored to simdjson/Langdale-Lemire + the on-disk kernel, NOT a Lemire-2026 body endorsement. Narrating a commenter route as a published author finding is the precise trap this lens forbids; 2E does not fall into it |
| Kutenin "Bit twiddling with Arm Neon…" — author Danila Kutenin (Google), `shrn`/`vshrn_n_u16<4>` replacing `addp`-based movemask, "10-15 percent improvements on a `strlen` distribution extracted from the SPEC CPU 2017 benchmark" | 2E (`:75`,`:113`) | **VERIFIED LIVE (WebFetch)** — author, SHRN-vs-ADDP technique, and the exact SPEC-CPU-2017 figure confirmed verbatim. The figure is correctly fenced as "Kutenin-reported / lineage-only, NOT a promotable bbnf row figure" and routed behind the REDRESS 96/97/98/126 net-win fence |
| Mison authors Yinan Li / Nikos R. Katsipoulakis / Badrish Chandramouli / Jonathan Goldstein / Donald Kossmann, PVLDB 10(10) 2017, DOI 10.14778/3115404.3115416 | 2D (`:60`,`:114`-`115`) | **VERIFIED EXACT (WebSearch)** — the V1 "Pavlo, Zhou" confabulation stays corrected; the consumer-known-projection grounding for SinkOnly is genuine |
| Fraser/Hanson/Proebsting "Engineering a Simple, Efficient Code-Generator Generator", ACM LOPLAS 1(3) 1992, pp. 213-226, DOI 10.1145/151640.151642 (iburg dispatch-on-selected-pattern — the R-A grounding) | 2D (`:53`,`:67`,`:95`,`:109`) | **VERIFIED EXACT (WebSearch)** — authors, venue (LOPLAS Vol 1 Issue 3), DOI all match; the R-A "dispatch on cost-selected shape, never a source-family tag" grounding is the iburg architecture |
| Hyperscan codebase `src/nfa/shufti.c` exists; top comment "Shufti: character class acceleration. Utilises the SSSE3 pshufb shuffle instruction" | 2F (`:71`,`:86`) | **VERIFIED LIVE (WebFetch)** — the SHUFTI NAME is genuinely in the codebase, NOT the NSDI paper; the CH6-V3-01 fold is legitimately VERIFIED |
| Lock-14 self-gate `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` returns 13 (asserts ZERO → RED) | 2C (`:138`,`:223`,`:303`) | **VERIFIED EXACT (on-disk rg)** — returns EXACTLY 13 at the ROOT (totality) crates; the self-gate falsification is real |
| 9-row grammar-named `PRODUCTION_MANIFEST_TABLE` const `strategy.rs:134`; 9 `idents` rows `:137`-`:185` (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty); consumed via `for_grammar_with_manifest` `:216` | 2C (`:218`,`:223`) | **VERIFIED EXACT (on-disk)** — const at `:134`, the 9 idents ROWS at `:137`-`:185` (2C cites the rows, not the const line — accurate), consumer at `:216` |
| `css_types.rs` = 66 LOC in the GENERIC `crates/core` crate, header "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map" | 2C (`:224`) | **VERIFIED EXACT (wc+head)** — 66 LOC, exact header, in the generic core crate (Lock-14-(c) admits only `crates/<grammar>/`) |
| Pattern H live census `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` = 71 | 2C (`:44`,`:78`,`:105`) | **VERIFIED EXACT (on-disk)** — 71; the 67→71 drift is honestly disclosed as the `tape/{mod,cursor,arena,record}.rs` +4, with the structural-provenance claim (not the absolute count) named as binding |
| SHRN movemask `movemask.rs:5` `vshrn_n_u16::<4>` vs shift-add eq-set body `byte_class_from_eq_set_64.rs:79`-`89` `vaddv_u8` two-half pack | 2E (`:82`,`:113`,`:241`) | **VERIFIED EXACT (sed ×2)** — `movemask.rs` uses `vshrn_n_u16::<4>`; the eq-set body uses `vandq_u8`+`vaddv_u8(lo)`+`vaddv_u8(hi)`. The SK-V18 movemask-divergence finding is real |
| eq-set kernel live consumer = CSS `count_top_level_commas` (`runtime_simd.rs:29`); `find_ascii_set_member64` has ZERO non-test runtime caller; inaccurate `runtime_simd.rs:6`-`7` comment ("the same kernel JSON's `scan_structurals` rides") present + named-not-adopted | 2B (`:268`,`:298`-`305`), 2C (`:215`), 2F (`:86`,`:138`-`141`) | **VERIFIED EXACT** — `count_top_level_commas` at `:29`; `rg find_ascii_set_member64 crates/runtime/src` = 0; the false comment is present at `:6`-`7` and all three dossiers NAME it false + route it to a G6 source-fix rather than re-adopt its JSON-rides claim |
| in-tree movemask comment `byte_class_from_eq_set_64.rs:63`-`64` "This is the same shape Lemire + Mula use for the AArch64 movemask spill in their JSON-parsing"; scalar oracle comment `:12`-`13` "asmjson (Lemire et al.) … `vpcmpeqb` … `korq`-reduces"; AVX-512 arm `lib.rs:285`-`287` | 2B (`:269`,`:362`) | **VERIFIED EXACT (rg+sed)** — 2B's "in-tree comment attribution VERIFIED real, not confabulated" is accurate; the AVX512BW `cfg` arm is present exactly as cited |
| 14 `checkasm_*.rs` files = 12 single-kernel + `checkasm_parity.rs` aggregate + `checkasm_common.rs` helper; `checkasm_parity.rs:3` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" | 2A (`:193`,`:275`), 2B (`:73`), 2F (`:42`-`43`) | **VERIFIED EXACT (ls+sed)** — exactly 14 files, the cited 12+parity+common decomposition; the FFmpeg attribution is verbatim |
| 5-shape `select_lowering` canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`, no sixth | 2D (`:54`,`:57`,`:70`) | **VERIFIED EXACT (sed)** — `lower/mod.rs:18`-`24` enumerates exactly the five via `cost.chosen` |

Every external citation and every in-tree anchor I spot-checked resolves to a
real source with an accurate characterisation. The last citation no prior CH6
cycle had verified (`neonmovemask_addv`) is now closed LIVE.

## Critical Findings (V5)

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V5-01 | ACCEPT | **The 2B `:269` `neonmovemask_addv` Lemire-2017 attribution is genuine — the last unverified paper-close vector, now closed.** WebFetch confirms the cited post DEFINES `neonmovemask_addv` (`vandq_u16` against `{0x0101…0x8080}` + `vaddvq_u16`), the exact ADDV-based movemask emulation 2B grounds the eq-set body's `vaddv_u8` pack against — DESPITE the post's title being about pruning spaces (the slug-vs-content trap this lens exists to catch; here the content genuinely bears the cited technique). The in-tree comment 2B reports ("Lemire + Mula … AArch64 movemask spill") is verified present verbatim, and 2B honestly grounds the technique on the Lemire post it verified rather than stamping a separately-uncited Mula paper. No paper-close. | Preserve; no fold. |
| CH6-V5-02 | ACCEPT | **The 2E Lemire-2026 body-vs-comment split is re-verified LIVE and remains the model anti-paper-close on a subtle citation.** The post BODY endorses SVE2 `match` (host-absent) as fastest; the `vceqq_u8` eq-fan is a COMMENTER ("-.-") suggestion the commenter himself concedes is slower than SVE MATCH. 2E binds the eq-fan to simdjson/Langdale-Lemire + the on-disk kernel, carries the "(comments)" qualifier through the Executive Summary / Assertion 3 / the G6 grounding row, and re-refutes svmatch on the live host probe (SVE2 absent). | Preserve; no fold. |
| CH6-V5-03 | ACCEPT | **The G6 speedup deferral is a same-tranche GATE, not a forbidden "later pass".** 2A T2A-V18-DAV1D-002 (`:176`) and 2E (`:129`,`:216`,`:225`) defer every Mbps figure to the H1 quiet re-capture, but the deferral is to a SAME-tranche wave, GATED by the machine-checkable `g6_speedup_claim_emitted == false` exit gate, the S-P1 directional ratios (2.190/3.375/1.658/2.101) marked NON-citable, and the falsifier named ("any Mbps/× string in a G6 artifact pre-H1 is a REJECT"). "No deferral to a later pass" is satisfied by a gate, not by prose. | Preserve; no fold. |
| CH6-V5-04 | ACCEPT | **The 2C SK-V19-receiver routing is a SCOPE BOUNDARY, not a paper-close deferral.** The TOTALITY-TREE-9-IDENT-LEAK (`2C:223`) and CSS-TYPES-HOST-SHIM-LEAK (`2C:224`) carry `tranche_scope=SK-V19-receiver`, but the EMPIRICAL finding is asserted NOW with live verification (self-gate = 13 sites at HEAD, `css_types.rs`=66 LOC in generic core — both re-verified this cycle); only the structural CLOSE (R16 `PartialEq` row-collapse) is routed to SK-V19. The lens forbids deferring a FINDING; it does not forbid scoping a multi-tranche CLOSE when the finding itself is live-verified and the receiver is named. 2C even self-disciplines against the patch route ("do NOT bolt a 9-name regex widen into an SK-V18 gate as a patch", `:361`). The 9-grammar onboarding row (`2C:218`) likewise scopes its `grounded` status to the 3 witnessed grammars and forbids fleet-wide neutral wording — the inverse of citation-density laundering. | Preserve; no fold. |
| CH6-V5-05 | ACCEPT | **The 2E SECTION A/B/C quarantine remains the exemplary defence against citation-count aggregation.** No SECTION-B no-consumer row (TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3) is countable as an SK-V18 admission — each carries `same_wave_consumer = NONE` + a "DO NOT author" abrogate threshold; the header (`:101`-`106`) states a SECTION-B "grounded" means the TECHNIQUE is real, not the primitive admitted. svmatch is REFUTED on the live host probe. The Movemask64Shrn / Interleave4Classify rows are LEDGER-fenced against REDRESS 96/97/98/126. No citation density aggregates across wired/no-consumer rows. | Preserve; no fold. |
| CH6-V5-06 | ACCEPT | **2A / 2C / 2D / 2F carry no surviving paper-close vector.** 2A pins every external cite to a commit SHA (simdjson `79bbba3e`, sonic-rs `03545a95`, yyjson `d6085270`, cssparser `4c494864`, lightningcss `ec165294`) and runs the 8-plane laundering-prevention discipline; the JSON-scan-free / G5-neutralize disposition refutes authoring a JSON SIMD classifier (profile-first, not "SIMD everywhere"). 2C's Pratt 7-level precedence tower is a verified negative control (the `Nu8`-tagged-alt family correctly DEMOTED, CSS 295× vs Sheets 21×). 2D's iburg/egg/BURG/OR-Tools spine is live in-tree (5-shape canon, no sixth; the V2 zero-rule supersession correct). 2F grounds the upstream `scan_balanced` against verified on-disk source + the corrected Hyperscan attribution; the float no-fallback claim stays REFUTED. Every SIMD row carries the full scalar-oracle + checkasm + hardware-gate + same-wave-consumer manifest. | Preserve; no fold. |

## Reference-integration audit (per dossier, V5)

- **2A** — integrated, not stuffed; the 8-plane discipline + SHA-pinned external
  cites are the laundering-prevention mechanism; the G6 deferral carries the
  machine-checkable exit gate; no JSON SIMD classifier authored. ACCEPT.
- **2B** — fully integrated; "source presence is not admission" is the spine; the
  eq-set neutrality is split structural-vs-empirical; the `neonmovemask_addv`
  citation is now LIVE-verified (last open vector); the FSM/frame-stack route is
  DELETE-only after the self-caught sidecar reintroduction (SKV18-A5). ACCEPT.
- **2C** — integrated for the 3-grammar witness; SK-V19-receiver rows carry inline
  `tranche_scope` and route the CLOSE not the FINDING; Pratt is a verified negative
  control; the Lock-14 self-gate falsification (13 sites) and `css_types.rs`=66 are
  live-verified. ACCEPT.
- **2D** — integrated; iburg (DOI verified)/Mison (DOI verified)/egg/OR-Tools spine
  is live in-tree; five-shape canon held; no sixth shape; the
  `runtime_target_rows_collapsed` co-gate honestly disclosed as PLANNED. ACCEPT.
- **2E** — integrated for the two wired SECTION-A rows; SECTION-B no-consumer rows
  quarantined "DO NOT author"; the Lemire-2026 body-vs-comment split + Kutenin
  SHRN figure verified live; svmatch refuted on the host probe. ACCEPT.
- **2F** — integrated against verified on-disk upstream + the live Hyperscan
  codebase; the SHUFTI/TRUFFLE fold is live-verified; the V2 title/path-root folds
  remain live. ACCEPT.

## On the >=30% REVISE expectation

V1 delivered 4/6 REVISE because the four heaviest paper-close vectors were
unfolded; V2 delivered 2/6, V3 delivered 1/6, V4 delivered 0/6 as the foundation
converged. V5 is 0/6, and the lens obligation at V5 is NOT to manufacture a ratio
but to confirm — by INDEPENDENT live re-verification, including the ONE citation
(`neonmovemask_addv`) that no prior CH6 cycle had spot-checked — that no
"VERIFIED" stamp asserts a check the named source fails. That obligation is
discharged: every external citation (iburg/LOPLAS DOI, Mison/PVLDB DOI, Hyperscan
`shufti.c` codebase, Lemire-2017 `neonmovemask_addv`, Lemire-2026 body-vs-comment,
Kutenin SHRN/SPEC-CPU), every in-tree anchor (self-gate=13, `strategy.rs`
9-idents, `css_types.rs`=66, Pattern H=71, the SHRN-vs-`vaddv` divergence,
`find_ascii_set_member64` zero-callers, the false `runtime_simd.rs:6`-`7` comment,
14 `checkasm_*` files, the 5-shape `lower/mod.rs`) resolves to a real source with
an accurate characterisation, and refutation is first-class throughout (the
eq-set empirical dual-consumer claim refuted; the false in-tree comment named not
adopted; the FSM frame-stack route DELETE-only; svmatch refuted on hardware; the
SK-V19 receiver routes the CLOSE not the live FINDING). The >=30% REVISE was a V1
expectation against an unfolded packet; manufacturing additional REVISE findings
to hit a fixed ratio against a converged, live-verified packet would itself be a
paper-close move this lens forbids. The honest count is 0.

## Evidence Inspected

- Lens authority: `restart/audit/totality/p2/hardening/V4/CH6.md` (the 0/6 clean
  V4), `restart/audit/totality/p2/hardening/V3/CH6.md` (the CH6-V3-01 fold),
  `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`103`.
- All six dossiers read in full (`2A`-`2F`, including the 2B `:294`-`437` tail).
- External citation verification (WebSearch/WebFetch this cycle): Lemire 2017-07-10
  `neonmovemask_addv` (LIVE — last unverified vector), Lemire 2026-04-19 ARM-match
  (LIVE body-vs-comment), Kutenin SHRN/SPEC-CPU (LIVE), Mison authors/DOI
  (WebSearch), Fraser/Hanson/Proebsting iburg LOPLAS DOI (WebSearch), Hyperscan
  `src/nfa/shufti.c` (LIVE codebase).
- In-tree verification (rg/sed/wc/ls): Lock-14 self-gate = 13 (root
  `crates/ir`+`crates/analysis`); `PRODUCTION_MANIFEST_TABLE` `strategy.rs:134` +
  9 idents `:137`-`:185` + consumer `:216`; `css_types.rs` wc=66 + header; Pattern
  H census = 71; `movemask.rs:5` `vshrn_n_u16::<4>` vs eq-set body `:79`-`89`
  `vaddv_u8`; `count_top_level_commas` `:29`; `find_ascii_set_member64` zero
  runtime callers; false comment `runtime_simd.rs:6`-`7`; in-tree movemask comment
  `byte_class_from_eq_set_64.rs:63`-`64` + scalar oracle asmjson comment `:12`-`13`
  + AVX512 arm `lib.rs:285`-`287`; 14 `checkasm_*` files + `checkasm_parity.rs:3`;
  5-shape `lower/mod.rs:18`-`24`.

## Fold Requirements

None. All six dossiers ACCEPT. Preserve 2A's SHA-pinned cites + gated G6-deferral,
2B's structural-vs-empirical neutrality split + the now-live-verified
`neonmovemask_addv` grounding + DELETE-only FSM reconcile, 2C's verified Pratt
negative control + Lock-14 self-gate falsification + SK-V19-receiver scope
boundary, 2D's live iburg/Mison/egg engine + five-shape canon, 2E's SECTION A/B/C
quarantine + Lemire-2026 body-vs-comment split + Kutenin-figure fence + host-probe
refutation, and 2F's corrected Hyperscan attribution + upstream-substrate
grounding as the V5 anti-paper-close template.

## Convergence Block

Does NOT block T-P2 convergence. V5 returns 6/6 ACCEPT under the CH6 lens. This is
the V5 hard-ceiling cycle following a clean V4; the independent live re-review
confirms every external citation, in-tree anchor, host probe, and ledger fence
resolves to a real source with an accurate characterisation — including the one
citation (`neonmovemask_addv`) no prior CH6 cycle had spot-verified, now closed
LIVE. Refutation is first-class throughout; the deferred G6 speedup is a
same-tranche GATE; the SK-V19 receiver routes the CLOSE not the live FINDING; no
citation-density aggregation is possible across the quarantined SECTION-A/B/C
structure. With a clean V4 followed by a clean V5, the CH6 lens holds its
converged state at the V<=5 ceiling.

TALLY accept=6 revise=0 reject=0
