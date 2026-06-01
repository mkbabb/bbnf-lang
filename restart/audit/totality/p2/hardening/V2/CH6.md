# CH6 — ANTI-PAPER-CLOSE (SK-V18 T-P2 cycle, lens V2)

Lens: CH6 ANTI-PAPER-CLOSE. No dossier may claim a technique "validated" on
citation-density alone; reference-stuffing (N sources cited, none integrated) is
flagged; every grounded technique must state the bbnf-specific reason it
transfers; no deferral to "a later pass". Cycle V1 expected >=30% REVISE.

Disposition: **REVISE** (one dossier — 2F — on two citation-hygiene defects; the
other five ACCEPT).

## Entry state and the V1 fold

V1/CH6 returned REVISE with four findings (CH6-V18-01..04). All four are folded
into the live dossiers at HEAD, independently re-verified this cycle:

- **CH6-V18-01 (2D Mison authors)** — FOLDED. The author list now reads "Li,
  Katsipoulakis, Chandramouli, Goldstein, Kossmann" at `2D:60` and `2D:114-115`.
  WebSearch confirms the real authors EXACTLY: Yinan Li, Nikos R. Katsipoulakis,
  Badrish Chandramouli, Jonathan Goldstein, Donald Kossmann (PVLDB 10(10) 2017,
  DOI 10.14778/3115404.3115416). The confabulated "Pavlo, Zhou" is gone.
- **CH6-V18-02 (2A G6 speedup deferral)** — FOLDED. `T2A-V18-DAV1D-002`
  (`2A:174`) now carries the machine-checkable exit gate
  `g6_speedup_claim_emitted == false`, marks the S-P1 directional ratios
  (2.190/3.375/1.658/2.101) NON-citable as a close figure pre-H1, and names the
  falsifier ("any Mbps/× string in a G6 artifact pre-H1 is a REJECT").
- **CH6-V18-03 (2E reference-density adjacency)** — FOLDED. The 2E grounding
  table is split into "SECTION A — WIRED-IN-SK-V18" (eq-set two-fan, SHRN
  movemask) and "SECTION B — HOST-PRESENT-NO-CONSUMER" (LD4/PMULL/CSSC/DotProd/
  I8MM/SHA3, all `same_wave_consumer = NONE`), `2E:94-118`, with an explicit
  header that a SECTION-B "grounded" means the TECHNIQUE is real, not that the
  primitive is SK-V18-admitted.
- **CH6-V18-04 (2C SK-V19-receiver scope tag)** — FOLDED. The three rows
  (9-GRAMMAR-FLEET-ONBOARDING `2C:205`, TOTALITY-TREE-9-IDENT-LEAK `2C:210`,
  CSS-TYPES-HOST-SHIM-LEAK `2C:211`) now carry an inline
  `tranche_scope=SK-V18-witnessed` / `SK-V19-receiver` field, so a grounded/
  refuted status cannot read as SK-V18-closeable.

The four heaviest paper-close vectors V1 caught are therefore closed. The V2
cycle is an independent re-review, not a re-run of V1.

## Citations spot-verified (load-bearing, V2 — independent)

| citation | dossier | result |
|---|---|---|
| Mison authors "Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann", PVLDB 10(10) 2017 | 2D | VERIFIED EXACT (WebSearch; the V1 confabulation is corrected) |
| Hyperscan NSDI 2019, authors Wang/Hong/Chang/Park/Langdale/Hu/Zhu | 2F (`:67`) | VERIFIED EXACT (dblp `WangHCPLHZ19`) |
| Kutenin "Bit twiddling with Arm Neon…", Danila Kutenin, `shrn` 4-bit movemask, 10-15% SPEC CPU 2017 strlen | 2E (`:69`) | VERIFIED EXACT (title, author, SHRN, SPEC claim; URL live) |
| Lemire 2017 "Pruning spaces faster on ARM…", `neonmovemask_addv` via `vaddv` reduce | 2B (`:258`) | VERIFIED EXACT (title, date 2017-07-10, neonmovemask_addv) |
| In-tree comment "Lemire + Mula … AArch64 movemask spill" | 2B (`:258`) | VERIFIED real (`byte_class_from_eq_set_64.rs:63-64`), not confabulated |
| simdjson nibble-LUT `f(c)=lut_lo[c&0x0F] AND lut_hi[c>>4]` | 2F (`:66`) | VERIFIED real (post + parse-that `classify_16` both match) |
| `find_css_significant` `runtime_simd.rs:169`; two-fan `set_a \| set_b` `:199` | 2A/2B/2E/2F | VERIFIED EXACT |
| dead `#[cfg(test)]` caller `runtime/src/lib.rs:574` | 2B/2E/2F | VERIFIED EXACT (test-only `find_css_significant`) |
| SHRN-vs-vaddv divergence: `movemask.rs:5` `vshrn_n_u16::<4>` vs `byte_class_from_eq_set_64.rs:83-84` `vaddv_u8` | 2E | VERIFIED EXACT |
| 5-shape `select_lowering(cost.chosen)` `lower/mod.rs:18-24` | 2C/2D | VERIFIED exactly five shapes |
| Lock-14 self-gate "asserts ZERO, returns 13" (rg over `crates/ir`+`crates/analysis`) | 2C | VERIFIED: rg returns exactly 13 |
| 9-row `PRODUCTION_MANIFEST_TABLE` `strategy.rs:134/137` | 2C | VERIFIED (9 idents rows) |
| `css_types.rs` 66 LOC in generic core crate | 2C (`:211`) | VERIFIED EXACT (`wc -l` = 66) |
| `checkasm_parity.rs:3-4` "Modelled on FFmpeg's tests/checkasm/checkasm.h" | 2A/2B | VERIFIED verbatim |
| 94.1% = 4121/4379, find_component_delim 79.5%, consume_balanced_at 14.6%, loadavg 4.35 | 2A/2B/2E/2F | VERIFIED EXACT (directional, loadavg disclosed) |
| REDRESS 144 `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT, 444.208 vs 434.13 Mbps, +109.87% | 2A/2B/2E/2F | VERIFIED EXACT |
| REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` retired; REDRESS 126 `ROUTE-PRODUCTION-SPLIT` | 2A/2B/2E/2F | VERIFIED EXACT |
| upstream parse-that `scan/balanced.rs:26` `scan_balanced`, `n<=8` assert `:45` | 2F | VERIFIED (assert at :45, 2F cited :44 — off-by-one, spans :44-46) |
| `rich_summary`/`nodes()` "rich, lazy, not eager, not flattened" `generated.rs:304` | 2A/2C | VERIFIED verbatim |
| `W8_SELECTED_CSS_ROWS = 24` `css_l4_w8.rs:17` | 2A/2C | VERIFIED EXACT |
| eisel_lemire `None`-on-ambiguous `mod.rs:168`, `~0.01%` doc band; `materialize_f64` `number/mod.rs:271` `text.parse::<f64>()` fallback | 2F | VERIFIED (content), but path under-rooted — see CH6-V2-02 |
| Lemire 2025 z3 vectorized-classification post | 2F (`:66`) | post REAL; **quoted title WRONG** — see CH6-V2-01 |

## Critical Findings (V2)

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V2-01 | REVISE | **Wrong quoted title on a row stamped "VERIFIED" (2F).** `2F:66` (SRC-LANGDALE-VECCLASS) cites Lemire's 2025 post as `"Easy vectorized classification with z3"` and stamps the entire row "VERIFIED". The post is real and the nibble-pair characterisation (`f(c)=lut_lo[c&0x0F] AND lut_hi[c>>4]`) is accurate, but the rendered title is **"Fast character classification with z3"** — "Easy vectorized classification with z3" is the URL slug, not the title. A quoted title that does not match the source, fronted by an explicit "VERIFIED" stamp, is precisely the citation-text laxity the anti-paper-close lens exists to catch: the verification label asserts a check that the title text did not actually pass. Because the post exists and the technique is genuinely integrated (it names the real `classify_16` 4-op classifier the eq-set kernel rides), this is REVISE, not REJECT. | Replace the quoted title with the rendered "Fast character classification with z3" (keep the slug URL), or drop the title and cite by URL+date; keep the row VERIFIED only once the quoted text matches the source. |
| CH6-V2-02 | REVISE | **Under-rooted load-bearing path on the float-no-fallback refutation (2F).** The float no-fallback refutation — one of 2F's two carried refutations — cites bare `eisel_lemire/mod.rs:166-168` and `number/mod.rs:271` with no crate-root prefix (`2F:15`, `:147`, `:166`). Two distinct crates carry an `eisel_lemire/mod.rs`: skinny `parse-that-regex` (where the `None // Ambiguous rounding` is at `:168`, and the `~0.01%` is a DOC comment at `:140`/`:8`, not a measured rate) and the upstream `parse-that` crate `2F` itself declares at `/Users/mkbabb/Programming/parse-that/rust/parse_that` (where the `~0.01%` band lives at `number_f64.rs:39`, NOT at `eisel_lemire/mod.rs:166-168`). The refutation's claim is substantively TRUE in the skinny crate (`:168` returns None; `materialize_f64` at `parse-that-regex/.../number/mod.rs:271` does `text.parse::<f64>()`), but a bare path that resolves to a different file in the crate 2F names in the SAME dossier is the exact root-resolution defect the T-P1 V5 fold eliminated from live inventories. The anti-paper-close lens requires a load-bearing path:line resolve from the repo root unambiguously. | Root-resolve the citation to `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168` and `skinny/crates/parse-that-regex/src/number/mod.rs:271`; if the upstream crate is also intended, cite its actual band location (`number_f64.rs:39`), not `eisel_lemire/mod.rs:166-168`. State that `~0.01%` is a doc-comment estimate, not a measured rate (2F already says "doc-stated" at `:148` — carry that qualifier into the refutation row). |
| CH6-V2-03 | ACCEPT | **2B and 2F (structural-neutrality split) are the V2 anti-paper-close template.** 2B states "source presence is not admission" as its spine (`2B:48`) and splits the eq-set neutrality into a STRUCTURAL claim (caller-supplied byte set, kernel names no grammar — `2B:257`,`:277`) versus a refuted empirical dual-consumer claim (`find_ascii_set_member64` has zero live runtime callers; JSON `scan_dispatch` rides a DIFFERENT primitive `byte_class_from_table_64`), rather than fabricating a JSON consumer. 2F refutes its OWN prior V2 framing ("that was a scope error, refuted here", `2F:29`) and grounds the upstream `scan_balanced` substrate against verified on-disk source. Every SIMD row carries the full scalar-oracle + checkasm + hardware-gate + same-wave-consumer manifest. No paper-close. | Preserve as the V2 row template; no fold. |
| CH6-V2-04 | ACCEPT | **2E quarantine + REDRESS-fencing is exemplary.** The WIRED-IN vs HOST-PRESENT split (CH6-V18-03 fold) is clean; the per-primitive cost manifest names each Section-B row `wave_owner=none` with an explicit "DO NOT author" abrogate threshold (`2E:136-144`). The G6 speedup is fenced to H1 with the directional/loadavg-4.35 disclosure (`2E:123`,`:205`), and the inert-run net-win is LEDGER-fenced against REDRESS-98 (the M5-Max scalar-cheaper-than-SIMD-cursor finding) rather than asserted. | Preserve; no fold. |
| CH6-V2-05 | ACCEPT | **2C and 2D carry no surviving paper-close vector.** 2C's Pratt negative-control (precedence tower as the SOLE non-fakeable Sheets construct) is a genuine integration; the SK-V19-receiver rows self-disclose their tranche scope; the Lock-14 self-gate falsification (asserts ZERO / returns 13) is re-verified live. 2D's egg/iburg/BURG/OR-Tools spine is live in `backend_egraph`/`decision_csp` (5-shape `select_lowering` verified), the V2 zero-rule supersession is correct, and the Mison author list is corrected. | Preserve; no fold. |

## Reference-integration audit (per dossier, V2)

- **2A** — sources integrated, not stuffed; the 8-plane discipline
  (`parse_only|DOM|value|typed_direct|lazy|fact_stream|CSS_typed_document|CSSOM_value`)
  is the laundering-prevention mechanism, and the G6 speedup deferral now carries
  the machine-checkable `g6_speedup_claim_emitted==false` gate. ACCEPT.
- **2B** — fully integrated; "source presence is not admission" is the spine; the
  eq-set neutrality is split structural-vs-empirical rather than overclaimed.
  ACCEPT (template).
- **2C** — integrated for the SK-V18 3-grammar witness; SK-V19-receiver rows
  carry inline `tranche_scope`; Pratt is a negative control, not an ornament.
  ACCEPT.
- **2D** — integrated; the published spine is live in-tree; Mison authors
  corrected; five-shape canon held; no sixth shape. ACCEPT.
- **2E** — integrated for the two wired rows; Section-B no-consumer rows are
  quarantined and labelled "DO NOT author"; x86 diagnostic-only; SVE2 refuted on
  the host probe. ACCEPT.
- **2F** — integrated against verified on-disk upstream + skinny source, with one
  wrong quoted title under a VERIFIED stamp (CH6-V2-01) and one under-rooted
  load-bearing path on a refutation row (CH6-V2-02). REVISE.

## On the >=30% REVISE expectation

V1 expected >=30% REVISE and delivered 4/6 because the four heaviest paper-close
vectors had not yet been folded. Those four are now folded (re-verified above).
For V2 the honest count is lower: the citation foundation is overwhelmingly
clean (every external and in-tree citation spot-verified resolves to a real
source, and refutation is first-class throughout). Manufacturing additional
REVISE findings to hit a fixed ratio would itself be a paper-close move that this
lens forbids. The two 2F defects (CH6-V2-01 title, CH6-V2-02 path-root) are real
and load-bearing — they sit on a row stamped VERIFIED and on a carried refutation
— and they block convergence until folded, but they do not multiply into the
other five dossiers.

## Evidence Inspected

- Lens authority: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md`,
  `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78-103`,
  `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`, and the V1 CH6 verdict
  `restart/audit/totality/p2/hardening/V1/CH6.md`.
- All six dossiers read in full (`2A`-`2F`).
- External citation verification (WebSearch/WebFetch): Mison authors, Hyperscan
  NSDI'19 authors, Kutenin Arm blog, Lemire 2017 pruning-spaces (neonmovemask_addv),
  Lemire 2025 z3 post (title mismatch surfaced).
- In-tree verification (grep/sed/wc): `find_css_significant` `runtime_simd.rs:169`/
  two-fan `:199`, dead caller `lib.rs:574`, SHRN-vs-vaddv `movemask.rs:5` /
  `byte_class_from_eq_set_64.rs:83-84` + comment `:63-64`, 5-shape
  `lower/mod.rs:18-24`, Lock-14 self-gate rg=13, 9-row `strategy.rs:134/137`,
  `css_types.rs` wc=66, `checkasm_parity.rs:3-4`, 94.1%/4121/4379/loadavg-4.35
  `SYNTHESIS-PROFILE.md`, REDRESS 144/96-98/126, `generated.rs:304`,
  `css_l4_w8.rs:17`.
- Upstream parse-that verification (`/Users/mkbabb/Programming/parse-that/rust/parse_that`):
  `scan/balanced.rs:26` `scan_balanced`, `n<=8` assert `:45`,
  `structural_bitmap.rs:37` `classify_16`, `quote_parity.rs:41/54/122`,
  `eisel_lemire/mod.rs:168` None-on-ambiguous (`~0.01%` doc at `:8`),
  `number_f64.rs:39` `~0.01%` band.
- skinny float path: `parse-that-regex/src/number/eisel_lemire/mod.rs:168`,
  `number/mod.rs:271` `text.parse::<f64>()`.

## Fold Requirements

1. 2F: correct the quoted title on SRC-LANGDALE-VECCLASS (`2F:66`) to "Fast
   character classification with z3" (or cite by URL+date) before the row may
   keep its "VERIFIED" stamp (CH6-V2-01).
2. 2F: root-resolve the float-no-fallback paths to
   `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:168` and
   `.../number/mod.rs:271` (`2F:15`,`:147`,`:166`); if the upstream crate is also
   meant, cite its actual band location (`number_f64.rs:39`), not
   `eisel_lemire/mod.rs:166-168`; carry the "doc-stated `~0.01%`" qualifier into
   the refutation row (CH6-V2-02).
3. Preserve 2B/2E/2F-structural-split as the V2 anti-paper-close row template
   (CH6-V2-03/04); 2A/2C/2D carry no fold.

## Convergence Block

Blocks T-P2 V2 convergence: **yes**, narrowly. Not a REJECT — every external and
in-tree citation resolves to a real source, all four V1 paper-close findings are
folded and re-verified, and refutation is first-class. But V2 cannot converge
while a row stamped "VERIFIED" carries a title the source does not bear
(CH6-V2-01) and while a carried refutation rides a bare path that resolves to a
different file in the very crate the dossier names (CH6-V2-02). Both REVISE
findings are 2F-local and fold cleanly into V3.

TALLY accept=5 revise=1 reject=0
