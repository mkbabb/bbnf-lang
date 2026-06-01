# CH6 — ANTI-PAPER-CLOSE (SK-V18 T-P2 cycle, lens V1)

Lens: CH6 ANTI-PAPER-CLOSE. No dossier may claim a technique "validated" on
citation-density alone; reference-stuffing (N sources cited, none integrated) is
flagged; every grounded technique must state the bbnf-specific reason it
transfers; no deferral to "a later pass". Cycle V1 expects >=30% REVISE.

Disposition: **REVISE**

The SK-V18 extensions are markedly more paper-close-resistant than the SK-V15
(V2) base: every grounded row I sampled carries an inline `transfer_reason`,
`admission_gate`, `verification_action`, and `close_status`, which is precisely
the V1/CH6 (SK-V15) fold this cycle inherited. Refutation is first-class and
load-bearing (each dossier carries an explicit refuted table; 2A/2B/2C reject
their own prior framings; "source presence is not admission" is stated verbatim
in 2B). Spot-verification of the heaviest external and in-tree citations came
back overwhelmingly clean — Pratt POPL 1973, iburg LOPLAS 1992, egg POPL 2021,
the Lemire 2026-04-19 ARM-match post (post-cutoff but live and accurately
characterised), checkasm.videolan.me provenance, the Kutenin Arm blog, the
Validark interleaved-vectors post, and every sampled path:line all verified
exact. The lens nevertheless lands REVISE on a confabulated author list, two
genuine "deferral to a later pass" surfaces that the close-status column papers
over, and three reference-stuffing rows where a multi-source citation block
fronts a technique that is not actually integrated into the SK-V18 close (only
into a future SK-V19 receiver or a host-absent ISA).

## Citations spot-verified (load-bearing)

| citation | dossier | result |
|---|---|---|
| Pratt, "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 | 2C | VERIFIED exact (title, venue, DOI) |
| Fraser/Hanson/Proebsting, iburg, LOPLAS 1(3) 1992, DOI 10.1145/151640.151642 | 2D | VERIFIED exact |
| Willsey et al., "egg", PACMPL 5 POPL 2021, DOI 10.1145/3434304 | 2D | VERIFIED exact (6 authors) |
| Lemire, "The fastest way to match characters on ARM processors?", 2026-04-19 | 2E | VERIFIED LIVE (post-cutoff real; SVE2 match fastest, NEON eq-fan deployable, cites Langdale/Lemire 2019) |
| checkasm canonical intro, checkasm.videolan.me (x264 origin, FFmpeg+dav1d shared) | 2A | VERIFIED exact provenance |
| Kutenin, "Bit twiddling with Arm Neon…", Arm Community blog (vshrn movemask, 10-15% SPEC CPU 2017) | 2E | VERIFIED exact (title AND URL both correct) |
| Salter/Validark, "Use interleaved vectors for parsing on ARM", 2024-09-03 | 2E/2B | VERIFIED exact (ld4/vld4q_u8) |
| Li et al., "Mison", VLDB 2017 | 2D | paper/venue REAL; **author list CONFABULATED** (see CH6-V18-01) |
| simdjson On-Demand lazy forward iterator (doc/basics.md) | 2A | VERIFIED (characterisation matches) |
| `find_css_significant` runtime_simd.rs:169, two-fan OR-reduce :199 | 2A/2B/2E/2F | VERIFIED exact |
| `checkasm_parity.rs:3-4` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" | 2A/2B | VERIFIED verbatim |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` grammar_provider.rs:40-42 | 2D | VERIFIED exact |
| `NormalizeDirectSinkCost` live backend_egraph.rs:75, BackoffScheduler/Extractor | 2D | VERIFIED (V2 zero-rule supersession is correct) |
| 5-shape `select_lowering(cost.chosen)` lower/mod.rs | 2D/2C | VERIFIED exactly five shapes |
| 9-ident `PRODUCTION_MANIFEST_TABLE` strategy.rs:137-185 | 2C | VERIFIED exact (9 rows) |
| `css_types.rs` in generic core crate (Lock 14 named mess) | 2C/2D | VERIFIED exact header |
| Lock-14 self-gate "asserts ZERO, returns 13" (rg over crates/ir+crates/analysis) | 2C | VERIFIED: rg returns exactly 13 |
| upstream `scan_balanced` parse-that balanced.rs:26, debug_assert :44 | 2F | VERIFIED exact (13 scan files present) |
| SYNTHESIS-PROFILE 94.1% = 4121/4379, find_component_delim 79.5%, consume_balanced_at 14.6% | 2A/2B/2E/2F | VERIFIED exact |
| `rich_summary`/`nodes()` "rich, lazy, not eager, not flattened" generated.rs:304-305 | 2A/2C | VERIFIED verbatim |

## Critical Findings

| id | severity | finding | required disposition |
|---|---|---|---|
| CH6-V18-01 | REVISE | **Confabulated author list on a real paper (2D).** 2D cites Mison as "Li, Pavlo, Zhou" at `restart/audit/totality/p2/2D-cost-model.md:59` (grounding row) and `:113` (source index). The paper, venue (VLDB 2017), and Microsoft-Research URL are real and the "consumer-known speculative projection" characterisation is correct, but the real authors are Yinan Li, Nikos R. Katsipoulakis, Badrish Chandramouli, Jonathan Goldstein, Donald Kossmann. "Pavlo" (Andy Pavlo, CMU — not a Mison author) and "Zhou" are confabulated. Under the lens, a fabricated author triplet on a load-bearing grounding citation is exactly the kind of provenance error the anti-paper-close pass exists to catch. Because the technique transfer to `SinkOnly` is genuine, this is REVISE not REJECT. | Correct the author list to "Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann" at both `:59` and `:113`. |
| CH6-V18-02 | REVISE | **Deferral-to-a-later-pass smuggled past the close-status column (2A).** T2A-V18-DAV1D-002 (`2A-sota-landscape.md:169`) and the PMU row defer every Mbps/speedup figure to "the H1 symmetric `css_canon_bench` corpus-in-timer harness" — i.e. the G6 wave is permitted to ship a checkasm PASS with the >SOTA *number* deferred to a later wave (H1). The lens forbids "no deferral to a later pass"; the dossier's defence is that the deferral is to a same-tranche wave (H1), not a future tranche, and that G6 reports only PASS/FAIL pre-H1. That distinction is legitimate but is asserted, not gated: there is no row stating what blocks G6 from *narrating* a directional speedup before H1 beyond prose. The close-status `diagnostic-only (pre-H1)` hides an open deferral. | Add an explicit anti-deferral gate to the 2A row: the G6 wave MUST carry `g6_speedup_claim_emitted == false` as a machine-checkable exit gate (not prose), and the directional S-P1 ratios (2.190/3.375/1.658/2.101) are NON-citable as a close figure until the H1 quiet re-capture lands in the SAME tranche. Name the falsifier: any Mbps string in a G6 artifact pre-H1 is a REJECT. |
| CH6-V18-03 | REVISE | **Reference-stuffing: multi-source citation block fronting a technique not integrated into the SK-V18 close (2E).** Three 2E partial/blocked rows — `Interleave4Classify` (LD4, `2E:99`), `PrefixXor64Pmull` (`:100`), `DigitMac4Udot` (`:102`), `TernaryXor3Eor3`/`BicXor3Bcax` (`:104`) — each carry a dense citation stack (ACLE + NEON ref + a named blog/LLVM patch) but every one resolves to `same_wave_consumer = NONE`. The dossier honestly labels them partial/partial-blocked and the lens credits that, but the *grounding tables* still present them as positively grounded technique rows interleaved with the two genuinely-wired G6 rows, inviting a downstream consumer to cite "N grounded NEON techniques" when only the eq-set two-fan and the SHRN movemask swap are on the SK-V18 close path. This is reference-density adjacency, not reference-stuffing in the falsifying sense — but the lens requires the no-consumer rows be visually quarantined from the wired rows. | Split the 2E grounding table into a WIRED-IN-SK-V18 section (eq-set two-fan, SHRN movemask) and a HOST-PRESENT-NO-CONSUMER section (LD4/PMULL/DotProd/I8MM/SHA3), so a consumer cannot aggregate the citation count across the two as "grounded for SK-V18". |
| CH6-V18-04 | REVISE | **Grounded rows whose transfer is to SK-V19, not SK-V18, carry no scope tag in the table (2C).** SK-V18-2C-9-GRAMMAR-FLEET-ONBOARDING-TEST (`2C:201`), SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK (`:206`), and SK-V18-2C-CSS-TYPES-HOST-SHIM-LEAK (`:207`) are marked `grounded`/`refuted` with `wave_owner=SK-V19` and `close_status=refuted (SK-V19 receiver)`. The empirical facts are verified (the 13-site Lock-14 self-gate falsification is real; `css_types.rs` is real). But labelling a row `grounded` while its actual close is a *different tranche* is the subtle form of "deferral to a later pass" — the SK-V18 reader sees a grounded green row whose transfer reason ("the onboarding test for ANY of the 9") is not exercised by the SK-V18 3-grammar witness. 2C is honest about this in prose (LAC-2C-SK18-02 fleet-scoping), but the table rows do not self-disclose the SK-V18-vs-SK-V19 boundary inline. | Add a `tranche_scope` field (`SK-V18-witnessed` vs `SK-V19-receiver`) to the three rows so a grounded status cannot be read as SK-V18-closeable when the receiver is SK-V19. |
| CH6-V18-05 | ACCEPT | **2B and 2F are the strongest anti-paper-close dossiers this cycle.** 2B states "source presence is not admission" as its spine (`2B:48`-`52`) and force-demotes `balanced_component_scan` -> `css_balanced_component_scan` rather than fabricate a non-CSS caller (`2B:262`, `:328`). 2F refutes its OWN prior V2 framing ("that was a scope error, refuted here", `2F:30`), grounds the upstream `scan_balanced` substrate against verified on-disk source, and keeps "no-fallback float" refuted (`2F:118`-`120`). Every SIMD row in both carries the full scalar-oracle + checkasm + hardware-gate + same-wave-consumer manifest. No paper-close. | Preserve as the V2 row template; no fold required. |

## Reference-integration audit (per dossier)

- **2A** — sources integrated, not stuffed; each SOTA row names a comparator plane and a row-local admission gate. The one paper-close vector is the H1 speedup deferral (CH6-V18-02). The 8-plane discipline (`parse_only|DOM|value|typed_direct|lazy|fact_stream|CSS_typed_document|CSSOM_value`) is the integration mechanism that prevents cross-plane citation laundering. ACCEPT-with-REVISE.
- **2B** — fully integrated; every primitive resolves to a wired-or-blocked status with a named consumer or an explicit NONE. ACCEPT.
- **2C** — integrated for the SK-V18 3-grammar witness; the SK-V19-receiver rows need scope tagging (CH6-V18-04). The Pratt negative-control is correctly used to prove the precedence tower is the SOLE non-fakeable Sheets construct — a genuine integration, not a cited ornament.
- **2D** — integrated (the egg/iburg/BURG/OR-Tools spine is live in `backend_egraph`/`decision_csp`, verified), but carries the one confabulated author list (CH6-V18-01).
- **2E** — integrated for the two wired rows; the host-present-no-consumer rows need quarantine (CH6-V18-03). x86 correctly diagnostic-only.
- **2F** — fully integrated against verified on-disk upstream + skinny source. ACCEPT.

## Evidence Inspected

- Lens authority: `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md`, `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:78`-`103` (citation density admits nothing; deep SIMD needs scalar ref + differential + hardware gate + same-wave consumer + row movement).
- All six dossiers read in full: `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`, `2D-cost-model.md`, `2E-host-arch-esoterica.md`, `2F-parse-that-gaps.md`.
- External citation verification (WebSearch/WebFetch): Pratt POPL 1973, iburg LOPLAS 1992, egg POPL 2021, Lemire 2026-04-19, checkasm.videolan.me, Kutenin Arm blog, Validark interleaved-vectors, Mison VLDB 2017, simdjson On-Demand.
- In-tree citation verification (grep/sed): `runtime_simd.rs:169`/`:199`, `checkasm_parity.rs:3-4`, `grammar_provider.rs:40-42`/`:110`, `backend_egraph.rs:75`/`:84`, `lower/mod.rs` 5-shape, `strategy.rs:137-185`, `css_types.rs`, Lock-14 self-gate rg = 13, upstream `parse-that/.../scan/balanced.rs:26`/`:44`, `SYNTHESIS-PROFILE.md:90`-`100` (94.1%), `generated.rs:657`/`:693`/`:304-305`, `css_l4_w8.rs:17` (W8_SELECTED_CSS_ROWS = 24).

## Fold Requirements

1. 2D: replace the confabulated Mison author list "Li, Pavlo, Zhou" with "Li, Katsipoulakis, Chandramouli, Goldstein, Kossmann" at `2D:59` and `2D:113` (CH6-V18-01).
2. 2A: bind a machine-checkable `g6_speedup_claim_emitted == false` exit gate on the G6 row and mark the S-P1 directional ratios non-citable as a close figure pre-H1 (CH6-V18-02).
3. 2E: split the grounding table into WIRED-IN-SK-V18 vs HOST-PRESENT-NO-CONSUMER so the citation count cannot be aggregated across wired and no-consumer rows (CH6-V18-03).
4. 2C: add a `tranche_scope` field (`SK-V18-witnessed` / `SK-V19-receiver`) to the three SK-V19-receiver rows so a grounded status cannot read as SK-V18-closeable (CH6-V18-04).
5. Preserve 2B/2F as the anti-paper-close row template (CH6-V18-05).

## Convergence Block

Blocks T-P2 V1 convergence: **yes**. Not a REJECT — every external and in-tree
citation resolves to a real source, refutation is first-class, and the SK-V18
extensions carry the inline gate the prior CH6 fold demanded. But V1 cannot
converge while a confabulated author list stands on a grounding citation
(CH6-V18-01), while the G6 >SOTA number is deferred to H1 without a
machine-checkable anti-deferral gate (CH6-V18-02), and while SK-V19-receiver
rows read as SK-V18-grounded without a scope tag (CH6-V18-04). All four REVISE
findings fold cleanly into V2.

TALLY accept=2 revise=4 reject=0
