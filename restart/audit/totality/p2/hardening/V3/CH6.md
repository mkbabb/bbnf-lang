# CH6 — ANTI-PAPER-CLOSE (SK-V18 T-P2 cycle, lens V3)

Lens: CH6 ANTI-PAPER-CLOSE. No dossier may claim a technique "validated" on
citation-density alone; reference-stuffing (N sources cited, none integrated) is
flagged; every grounded technique must state the bbnf-specific reason it
transfers; no deferral to "a later pass". Spot-verify the most load-bearing
citations against the live source. Cycle V1 expected >=30% REVISE.

Disposition: **REVISE** (one dossier — 2F — on a single citation-precision
defect: the SHUFTI/TRUFFLE abstract-name attribution to the Hyperscan NSDI paper.
The other five ACCEPT. This is an independent re-review, not a re-run of V1/V2.)

## Entry state — the V1 and V2 folds, independently re-verified this cycle

V1/CH6 returned REVISE on four findings (CH6-V18-01..04); V2/CH6 returned
REVISE on two more (CH6-V2-01 wrong-title, CH6-V2-02 under-rooted-path). All six
fold obligations are landed at HEAD and re-verified independently this cycle:

- **CH6-V18-01 (2D Mison authors)** — FOLDED + RE-VERIFIED. `2D:60`,`:114-115`
  read "Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann". WebSearch
  confirms the real authors EXACTLY: Yinan Li, Nikos R. Katsipoulakis, Badrish
  Chandramouli, Jonathan Goldstein, Donald Kossmann (PVLDB 10(10) 2017, DOI
  10.14778/3115404.3115416). The V1 "Pavlo, Zhou" confabulation is gone.
- **CH6-V18-02 (2A G6 speedup deferral)** — FOLDED + RE-VERIFIED. `2A:174`
  (T2A-V18-DAV1D-002) carries the machine-checkable exit gate
  `g6_speedup_claim_emitted == false`, marks the S-P1 directional ratios
  (2.190/3.375/1.658/2.101) NON-citable pre-H1, and names the falsifier
  ("any Mbps/× string in a G6 artifact pre-H1 is a REJECT"). The deferral is
  to a same-tranche wave (H1) and is GATED, not asserted.
- **CH6-V18-03 (2E reference-density adjacency)** — FOLDED + RE-VERIFIED. The 2E
  table is split SECTION A (WIRED-IN-SK-V18: eq-set two-fan, SHRN movemask) /
  SECTION B (HOST-PRESENT-NO-CONSUMER: TBL/LD4/PMULL/CSSC/DotProd/I8MM/SHA3, all
  `same_wave_consumer = NONE`) / SECTION C (PROCESS/CONTRAST/REFUTED), `2E:102-125`,
  with the explicit header that a SECTION-B "grounded" means the TECHNIQUE is
  real, not the primitive admitted.
- **CH6-V18-04 (2C SK-V19-receiver scope tag)** — FOLDED + RE-VERIFIED. The three
  rows now carry an inline `tranche_scope` field:
  9-GRAMMAR-FLEET-ONBOARDING `2C:215` (`SK-V18-witnessed ∧ SK-V19-receiver`),
  TOTALITY-TREE-9-IDENT-LEAK `2C:220` (`SK-V19-receiver`),
  CSS-TYPES-HOST-SHIM-LEAK `2C:221` (`SK-V19-receiver`).
- **CH6-V2-01 (2F wrong quoted title)** — FOLDED + RE-VERIFIED LIVE. `2F:66`
  (SRC-LANGDALE-VECCLASS) now reads the title as "Fast character classification
  with z3" and explicitly discloses "easy-vectorized-classification-with-z3" is
  the URL slug. **WebFetch of the live post confirms the rendered H1 IS "Fast
  character classification with z3"** and the post carries the nibble-pair
  `f(c)=lut_lo[c&0x0F] AND lut_hi[c>>4]`. The VERIFIED stamp is now legitimate —
  the fold substituted the CORRECT title, not a second unverified one (the trap
  this lens exists to catch).
- **CH6-V2-02 (2F under-rooted float path)** — FOLDED + RE-VERIFIED. `2F:15`,
  `:147-155`,`:166`,`:172` root-resolve to
  `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168` (verified:
  `return None; // Ambiguous rounding — fallback needed`) and
  `.../number/mod.rs:271` (verified: `text.parse::<f64>()`), and carry the
  "doc-stated `~0.01%`" qualifier (verified: the `~0.01%` is a doc comment at
  `eisel_lemire/mod.rs:8`, NOT a measured rate).

The six heaviest paper-close vectors V1+V2 caught are therefore closed and
re-verified against the live sources, not merely against the consolidated
verdicts.

## Citations spot-verified this cycle (load-bearing, V3 — independent live checks)

| citation | dossier | result |
|---|---|---|
| Lemire 2025-06-01 z3 post — **rendered H1 title** "Fast character classification with z3" + nibble-pair `lut_lo[c&0x0F] AND lut_hi[c>>4]` | 2F (`:66`) | **VERIFIED LIVE (WebFetch)** — H1 title matches the folded text EXACTLY; the CH6-V2-01 fold is legitimately VERIFIED |
| Lemire 2026-04-19 "The fastest way to match characters on ARM processors?" — SVE2 match/nmatch fastest; deployable NEON route is the `vceqq_u8` eq-fan | 2E (`:71`) | **VERIFIED LIVE (WebSearch)** — post-cutoff real; characterisation (SVE2 match replaces eq-compare+OR-reduce; eq-fan is the deployable route) accurate |
| Kutenin "Bit twiddling with Arm Neon: beating SSE movemasks…", `shrn` movemask, 10–15% SPEC CPU 2017 | 2E (`:69`), 2B (`:268`) | **VERIFIED** — title + author (Danila Kutenin, Google) + Arm Community blog; URL slug differs from title but 2E cites BOTH correctly (no false "title==slug" claim, unlike the V2 2F defect) |
| Validark "Use interleaved vectors for parsing on ARM", 2024-09-03, `vld4q_u8`/ld4 | 2E (`:70`), 2B (`:82`,`:183`) | **VERIFIED** — title, date 2024-09-03, vld4q_u8/ld4 deinterleave all exact |
| Mison authors + PVLDB 10(10) 2017 + DOI 10.14778/3115404.3115416 | 2D (`:60`,`:114-115`) | **VERIFIED EXACT** — the V1 confabulation stays corrected |
| Hyperscan NSDI 2019 authors Wang/Hong/Chang/Park/Langdale/Hu/Zhu | 2F (`:67`) | author list VERIFIED EXACT; **SHUFTI/TRUFFLE name not in the NSDI paper abstract — see CH6-V3-01** |
| Pratt "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 | 2C (`:213`,`:266`) | **VERIFIED EXACT** (title, venue POPL '73, DOI) |
| sonic-rs LazyValue/`get_from` — borrowed raw slice + SIMD string-bitmap + bracket-count container skip (from simdjson) | 2A (`:177`,`:273`) | **VERIFIED LIVE (WebSearch)** — sonic-rs docs state verbatim "SIMD … bitmap of the string … counting the number of brackets … skip the entire JSON container … borrowed from simdjson" |
| M5 Max host probe: FEAT_PMULL/DotProd/I8MM/CSSC/SHA3/BF16/SME2=1, FEAT_SVE2 ABSENT | 2E (`:82` SRC-HOST-PROBE) | **VERIFIED EXACT (local sysctl)** — `Apple M5 Max`; FEAT_SVE2 = `unknown oid` (absent). The svmatch re-refutation rests on real hardware, not citation |
| `find_css_significant` `runtime_simd.rs:169`; two-fan `set_a \| set_b` `:199`; dead `#[cfg(test)]` caller `lib.rs:574` | 2A/2B/2E/2F | **VERIFIED EXACT** |
| SHRN-vs-vaddv divergence: `movemask.rs:5` `vshrn_n_u16::<4>` vs `byte_class_from_eq_set_64.rs:79-87` `vaddv_u8` shift-add | 2E (`:76`,`:107`) | **VERIFIED EXACT** (both bodies read as cited) |
| Lock-14 self-gate "asserts ZERO, returns 13" (`rg` over root `crates/ir`+`crates/analysis`) | 2C (`:220`,`:300`,`:346`) | **VERIFIED: rg returns EXACTLY 13** (root crates, as the cited command names) |
| `css_types.rs` 66 LOC in generic core crate | 2C (`:221`) | **VERIFIED EXACT** (`wc -l` = 66) |
| 9-row `PRODUCTION_MANIFEST_TABLE` `strategy.rs:137`; `for_grammar_with_manifest` `:216`/`:249` | 2C (`:215`,`:220`) | **VERIFIED** (9 idents rows; consumer call site present) |
| `W8_SELECTED_CSS_ROWS = 24` `css_l4_w8.rs:17` | 2A | **VERIFIED EXACT** |
| `checkasm_parity.rs:3-4` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" | 2A/2B/2E/2F | **VERIFIED VERBATIM** |
| upstream parse-that `scan/balanced.rs:26` `scan_balanced` + `build_nibble_luts`/`find_first_of_nibble_lut`; `n<=8` debug_assert `:44-46` | 2F (`:69`,`:81`,`:182`) | **VERIFIED** (assert spans :44-46; 2F's `:44`/`:45` cite the macro line — acceptable) |
| REDRESS 144 `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT, 444.208 vs 434.13 Mbps, +109.87%, `Scanner::scan_block` | 2A/2B/2E/2F | **VERIFIED EXACT** |
| REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` retired — M5 Max scalar-cheaper-than-SIMD-cursor finding `:2928-2933` | 2A/2B/2E/2F | **VERIFIED VERBATIM** (the LEDGER FENCE on the deferred net-win) |
| REDRESS 126 `ROUTE-PRODUCTION-SPLIT` (microbench PASS ≠ row move) | 2E/2F | **VERIFIED EXACT** |
| SYNTHESIS-PROFILE 94.1% = 4121/4379; `find_component_delim` 79.5% | 2A/2B/2E/2F | **VERIFIED EXACT** |

Every external citation, every in-tree path:line, every ledger anchor, and the
host probe resolve to a real source with an accurate characterisation. The
foundation is the cleanest of the three cycles.

## Critical Findings (V3)

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V3-01 | REVISE | **Abstract-primitive NAME attributed to the wrong layer of the Hyperscan source (2F).** `2F:67` (SRC-HYPERSCAN) cites "Hyperscan: A Fast Multi-pattern Regex Matcher for Modern CPUs, NSDI 2019" as "the published lineage of SHUFTI/TRUFFLE shuffle-based byte classification — the abstract primitive name (`SHUFTI`) for `byte_class_from_eq_set_64`'s small-set classifier", and the row is stamped VERIFIED. The author list IS exact (Wang/Hong/Chang/Park/Langdale/Hu/Zhu — confirmed) and the paper is real, BUT the **NSDI 2019 paper abstract/text does not name SHUFTI or TRUFFLE** — those are Hyperscan *codebase* algorithm names (documented in the Hyperscan source and in Langdale's branchfree.org writing), not in the cited paper. Under the anti-paper-close lens, sourcing the *abstract-primitive name* to a paper that does not contain it — under a VERIFIED stamp on a GROUNDED row — is the same citation-text laxity class as the V2 CH6-V2-01 title defect: the verification label asserts a check the named-source text did not pass. Because the SHUFTI primitive is genuinely real (Hyperscan project lineage) and the technique IS integrated (it names the live eq-set classifier the G6 kernel rides), and the row already co-cites the branchfree summary, this is REVISE, not REJECT. | Attribute the SHUFTI/TRUFFLE NAME to its actual source — the Hyperscan codebase / Geoff Langdale's branchfree SHUFTI/TRUFFLE writing — not to the NSDI 2019 paper text; keep the NSDI paper as the project citation (graph-decomposition + SIMD-accelerated string/FA matching) but stop stamping the *name* as "published" by it. Keep the row VERIFIED only once the cited text bears the cited name. |
| CH6-V3-02 | ACCEPT | **The G6 speedup deferral is the model anti-deferral gate.** 2A T2A-V18-DAV1D-002 (`:174`) and 2E `:123`,`:205`,`:214` defer every Mbps figure to the H1 quiet re-capture, but the deferral is to a SAME-tranche wave and is GATED by the machine-checkable `g6_speedup_claim_emitted == false` exit gate, the directional ratios are marked NON-citable, and the falsifier ("any Mbps/× string in a G6 artifact pre-H1 is a REJECT") is named. This is "no deferral to a later pass" satisfied by a gate, not by prose. No paper-close. | Preserve as the V3 anti-deferral template; no fold. |
| CH6-V3-03 | ACCEPT | **The 2E SECTION A/B/C quarantine + REDRESS-fencing is exemplary.** No SECTION-B no-consumer row (LD4/PMULL/CSSC/DotProd/I8MM/SHA3) is countable as an SK-V18 admission; each carries `same_wave_consumer = NONE` and a "DO NOT author" abrogate threshold (`2E:136-144`). svmatch is REFUTED on the live host probe (FEAT_SVE2 absent, re-verified). The G6 net-win is LEDGER-fenced against the negatively-answered REDRESS 96/97/98 prior, not asserted. No citation-count aggregation is possible across wired/no-consumer rows. | Preserve; no fold. |
| CH6-V3-04 | ACCEPT | **2B and 2F (structural-neutrality split) remain the anti-paper-close template.** 2B states "source presence is not admission" as its spine (`2B:48`) and splits the eq-set neutrality into a STRUCTURAL claim (caller-supplied byte set, kernel names no grammar) versus a REFUTED empirical dual-consumer claim (`find_ascii_set_member64` has zero live runtime callers; JSON `scan_dispatch` rides the DIFFERENT `byte_class_from_table_64`; the inaccurate `runtime_simd.rs:6-7` source comment is named as a same-wave fix obligation, not adopted). 2F refutes its own prior V2 scope framing and grounds the upstream `scan_balanced` substrate against verified on-disk source. Every SIMD row carries the full scalar-oracle + checkasm + hardware-gate + same-wave-consumer manifest. No paper-close. | Preserve; no fold. |
| CH6-V3-05 | ACCEPT | **2C and 2D carry no surviving paper-close vector.** 2C's Pratt negative-control (the 7-level precedence tower as the SOLE non-fakeable Sheets construct) is a genuine integration (`Nu8`-tagged-alt correctly DEMOTED from the litmus because CSS uses it 295× vs Sheets 21×); the three SK-V19-receiver rows self-disclose `tranche_scope` inline; the Lock-14 self-gate falsification (asserts ZERO / returns 13) is re-verified live. 2D's egg/iburg/BURG/OR-Tools spine is live in `backend_egraph`/`decision_csp` (5-shape `select_lowering` verified), the V2 zero-rule supersession is correct (`NormalizeDirectSinkCost` live at `backend_egraph.rs:75`), the Mison authors are corrected, and CollapsedStage stays diagnostic-only / LEDGER-fenced (LAC-2D-V3-04). No sixth shape. | Preserve; no fold. |

## Reference-integration audit (per dossier, V3)

- **2A** — sources integrated, not stuffed; the 8-plane discipline
  (`parse_only|DOM|value|typed_direct|lazy|fact_stream|CSS_typed_document|CSSOM_value`)
  is the laundering-prevention mechanism; sonic-rs LazyValue mechanism verified
  live; the G6 deferral carries the machine-checkable exit gate. ACCEPT.
- **2B** — fully integrated; "source presence is not admission" is the spine; the
  eq-set neutrality is split structural-vs-empirical, not overclaimed; the
  inaccurate in-tree source comment is named as a fix obligation, not adopted.
  ACCEPT (template).
- **2C** — integrated for the SK-V18 3-grammar witness; SK-V19-receiver rows carry
  inline `tranche_scope`; Pratt is a negative control, not an ornament; the
  Lock-14 self-gate falsification is live-verified. ACCEPT.
- **2D** — integrated; the published spine is live in-tree; Mison authors
  corrected; five-shape canon held; no sixth shape; CollapsedStage LEDGER-fenced.
  ACCEPT.
- **2E** — integrated for the two wired rows; SECTION-B no-consumer rows are
  quarantined and labelled "DO NOT author"; svmatch refuted on the live host
  probe; x86 diagnostic-only / deleted. ACCEPT.
- **2F** — integrated against verified on-disk upstream + skinny source; the V2
  title and path-root folds landed and are live-verified; one
  abstract-name-attribution defect on the Hyperscan row (CH6-V3-01). REVISE.

## On the >=30% REVISE expectation

V1 delivered 4/6 REVISE because the four heaviest paper-close vectors were
unfolded; V2 delivered 1/6 because the foundation was already clean and the two
2F defects were genuine and load-bearing. V3 is lower still: every external and
in-tree citation spot-verified this cycle resolves to a real source with an
accurate characterisation, the live H1 title of the z3 post matches the
CH6-V2-01 fold EXACTLY, the host probe confirms the SVE2-absent refutation, and
the REDRESS ledger fences the deferred net-win against a negatively-answered
prior. The single REVISE (CH6-V3-01) is real and load-bearing — it sits on a
VERIFIED-stamped GROUNDED row and mis-sources an abstract-primitive NAME to a
paper that does not contain it — but it does not multiply into the other five
dossiers. Manufacturing additional REVISE findings to hit a fixed 30% ratio would
itself be a paper-close move this lens forbids; the honest count is 1.

## Evidence Inspected

- Lens authority: `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`,
  `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78-103`,
  `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`, and the prior SK-V18 CH6
  verdicts `restart/audit/totality/p2/hardening/V1/CH6.md`,
  `restart/audit/totality/p2/hardening/V2/CH6.md`.
- All six dossiers read in full (`2A`-`2F`).
- External citation verification (WebSearch/WebFetch this cycle): Lemire 2025 z3
  post (live H1 title), Lemire 2026-04-19 ARM-match, Kutenin Arm blog, Validark
  interleaved-vectors, Mison authors/DOI, Hyperscan NSDI'19 authors + SHUFTI
  scope, Pratt POPL'73 DOI, sonic-rs LazyValue mechanism.
- In-tree verification (grep/sed/wc): `find_css_significant` `runtime_simd.rs:169`
  + two-fan `:199`, dead caller `lib.rs:574`, SHRN `movemask.rs:5` vs shift-add
  `byte_class_from_eq_set_64.rs:79-87`, `W8_SELECTED_CSS_ROWS=17`,
  `checkasm_parity.rs:3-4`, Lock-14 self-gate rg=13 (root crates),
  `css_types.rs` wc=66, 9-row `strategy.rs:137`/`:216`/`:249`.
- Local host probe (sysctl): `Apple M5 Max`; FEAT_PMULL/DotProd/I8MM/CSSC/SHA3/
  BF16/SME2=1; FEAT_SVE2 unknown-oid (ABSENT).
- Skinny float path: `parse-that-regex/src/number/eisel_lemire/mod.rs:168`
  (None-on-ambiguous), `:8` (`~0.01%` doc comment), `number/mod.rs:271`
  (`text.parse::<f64>()`).
- Upstream parse-that: `scan/balanced.rs:26` `scan_balanced` +
  `build_nibble_luts`/`find_first_of_nibble_lut`, `n<=8` assert `:44-46`.
- Ledger (REDRESS.md): 144 `G-W12-SIMD-ASM-PRODUCTION` (+109.87%, 444.208 vs
  434.13), 96/97/98 `G-W3-UNION-SUBSTRATE` retired finding `:2928-2933`, 126
  `ROUTE-PRODUCTION-SPLIT`; SYNTHESIS-PROFILE 94.1%=4121/4379.

## Fold Requirements

1. 2F: on SRC-HYPERSCAN (`2F:67`), attribute the abstract-primitive NAME
   `SHUFTI`/`TRUFFLE` to its real source (the Hyperscan codebase / Langdale's
   branchfree SHUFTI-TRUFFLE writing), not to the NSDI 2019 paper text; keep the
   NSDI paper as the project/lineage citation; keep the row VERIFIED only once the
   cited text bears the cited name (CH6-V3-01).
2. Preserve 2A's gated G6-deferral, 2E's SECTION A/B/C quarantine + host-probe
   refutation, 2B/2F's structural-neutrality split, and 2C/2D's clean integration
   as the V3 anti-paper-close template (CH6-V3-02/03/04/05); no fold on 2A/2B/2C/2D/2E.

## Convergence Block

Blocks T-P2 V3 convergence: **yes, narrowly** — not a REJECT. Every external and
in-tree citation, every ledger anchor, and the host probe resolve to a real
source; all six prior V1+V2 paper-close findings are folded and re-verified live;
refutation is first-class throughout; the deferred G6 speedup is gated, not
asserted. But V3 cannot return 6/6 clean while a VERIFIED-stamped GROUNDED row
sources an abstract-primitive name to a paper that does not contain it
(CH6-V3-01). The single REVISE is 2F-local and folds cleanly into V4. Because V3
is not 6/6 ACCEPT, the second consecutive clean §3Z cycle is not reached at V3;
a clean V4 over the CH6-V3-01 fold (with the other five lenses also clean)
satisfies the two-consecutive-clean rule within the V<=5 ceiling.

TALLY accept=5 revise=1 reject=0
