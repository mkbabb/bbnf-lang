# CH2 GENERALITY — SK-V18 T-P2 (cycle V2)

Lens: CH2 GENERALITY. Lock 14 holds — every primitive/technique must be grounded
grammar-NEUTRALLY; 2C must show the technique transferring to CSS L4 / Sheets /
BBNF-self / the 9-grammar fleet, not JSON-only. A technique grounded JSON-only but
used fleet-wide is a REVISE. A neutrality-proof that leans on a JSON consumer which
does not exist in production (while the kernel is in fact fleet-used on CSS) is the
same defect.

Disposition: **REVISE** (the GENERALITY backbone is sound and the four V1 REVISE +
one V1 REJECT folds are all applied in the dossier prose; the residual defects are
that two artifacts the dossiers CITE as Lock-14 neutrality evidence — the
`runtime_simd.rs:6-7` source comment and the upstream `SYNTHESIS-RESEARCH.md:43`
R-F row — still carry the FALSIFIED "JSON `scan_structurals` rides the same eq-set
kernel" claim the dossiers themselves now refute, plus two localized
over-precision rows).

## V1→V2 Fold Audit (independently re-verified at HEAD)

All five V1 CH2 folds are APPLIED in the live dossier text:

- **CH2-V1-01 (2B eq-set dual-consumer)** — FOLDED. 2B:257 now reads
  `same_wave_consumer=the CSS count_top_level_commas path (the ONLY live production
  consumer) AND the G6 retarget shell (find_ascii_set_member64 ... is a wrapper with
  NO non-test/non-bench runtime caller — do NOT cite it as a live JSON consumer; the
  JSON scan_dispatch path rides byte_class_from_table_64, a DIFFERENT primitive)`.
- **CH2-V1-02 (2F PTG-2F-10 / Assertion 3)** — FOLDED. 2F:82 now reads
  `Its ONLY live production consumer is CSS ... there is no live JSON consumer of this
  kernel; the structural neutrality is genuine ... but the empirical dual-consumer
  claim is NOT — record CSS-only`; 2F:134-135 names `byte_class_from_table_64` as the
  different JSON primitive. The `Lock 16 FULLY SATISFIED` cell is downgraded to
  `same-wave-consumer cell satisfied by the CSS consumer ONLY (not a JSON one)`.
- **CH2-V1-03 (2C inner-kernel JSON-consumer proof)** — FOLDED. 2C V3:202 now
  splits the eq-set neutrality into (i) base one-fan kernel STRUCTURALLY neutral
  (`find_ascii_set_member64 has NO live non-test/non-bench runtime caller, so it is
  NOT a JSON consumer proof ... base kernel's only live production consumer is CSS`)
  and (ii) two-fan composition CSS-exercised-only, subject to the same neutrality
  obligation.
- **CH2-V1-04 (2A unscoped fleet >SOTA framing)** — FOLDED. 2A:142-147 now reads
  `SCOPING (per 2C LAC-2C-SK18-02): SK-V18 grounds a measured >SOTA plane on JSON+CSS
  ONLY ... Sheets is a generality (not a SOTA) proof here; the 9-grammar fleet SOTA
  is SK-V19`.
- **CH2-V1-13 (2C 67-vs-71 Pattern-H census)** — FOLDED. 2C now carries `71 at HEAD`
  with a V3 OQ on the 67→71 drift; LAC-2C-SK15-04 binds per-file provenance over the
  live census, not a fixed N. Re-verified live: `find crates/core/src/runtime -mindepth
  2 -type f -name '*.rs' | wc -l` = 71 (+4 = `tape/{mod,cursor,arena,record}.rs`).

## Verdict Census

14 CH2-relevant groundings/refutations enumerated: **9 ACCEPT, 4 REVISE, 1 REJECT.**

## Critical Findings

| id | dossier | disposition | finding | falsifying / corroborating evidence |
|---|---|---:|---|---|
| CH2-V2-01 | 2B/2F/2C | **REVISE** | The dossiers correctly REFUTE the false dual-consumer claim, but they CITE `runtime_simd.rs` as the Lock-14 grammar-neutrality evidence, and that SOURCE FILE'S comment at `:6-7` still asserts the falsified claim verbatim: "`byte_class_from_eq_set_64` is the same kernel JSON's `scan_structurals` rides". This is empirically FALSE: `scan_structurals` (`json/scan.rs:22`) routes to `scan_structurals_scalar` (`:29`) — scan-free/scalar, never calling the eq-set kernel; the JSON `scan_dispatch` path rides `byte_class_from_table_64`. A dossier whose neutrality proof rests on a source comment that itself states a refuted fact is a latent paper-close. Correction: the dossiers (2B, 2F V3 frontmatter, 2C Assertion 2) should record an inline note that `runtime_simd.rs:6-7`'s "the same kernel JSON's `scan_structurals` rides" comment is INACCURATE and is a same-wave G6 source-fix obligation, so a future consumer reading the cited file does not re-adopt the refuted claim. The abstract neutrality (caller-supplied byte set, names no grammar) stands; only the source-comment's JSON-rides claim is the defect. | `sed -n '5,10p' runtime_simd.rs` confirms the false comment is still present at HEAD; `json/scan.rs:22-29` `scan_structurals → scan_structurals_scalar` (no eq-set call) verified; `rg byte_class_from_eq_set_64 skinny/crates/runtime/src/grammars/json/` = 0; the only live runtime callers are `runtime_simd.rs:44,56,199` (all CSS: QUOTES/COMMA/set_a/set_b). |
| CH2-V2-02 | 2C V3 / R-F | **REVISE** | The upstream `SYNTHESIS-RESEARCH.md:43` (R-F row), which 2C V3 / 2B / 2F all derive their G5/G6 retarget grounding from, STILL asserts the refuted claim as live fact: "JSON neutrality is honest (same eq-set kernel JSON's `scan_structurals` already rides), NOT fabricated". The dossiers correctly DO NOT repeat it and even flag it (2B:280-281, 2F:134-135), but the cited synthesis source is internally contradicted by the dossiers it feeds. Correction: 2C V3 should add an OQ/note that the R-F SYNTHESIS-RESEARCH justification ("JSON's `scan_structurals` already rides") is the SAME falsified dual-consumer claim and that the honest R-F grounding is "JSON product path is scan-free, so the eq-set neutrality is STRUCTURAL not empirical-dual-consumer" — closing the loop so the synthesis source cannot be cited at face value. The retarget route itself (salvage two-fan set-split onto the recursive shell) is grammar-neutral and correctly grounded; only its JSON-neutrality justification line is stale. | `SYNTHESIS-RESEARCH.md:43` verbatim "same eq-set kernel JSON's `scan_structurals` already rides"; contradicted by `json/scan.rs:29` scan-free and by the dossiers' own corrected rows. |
| CH2-V2-03 | 2F | **REVISE** | PTG-2F-13 (2F:85) and the dispatch-bullet (frontmatter line 17) state the UPSTREAM `parse-that` crate "carries a FULL scan/ substrate (scan_balanced, structural_bitmap nibble-LUT classifier ...)" and route the CSS balanced-scan re-home (PTG-2F-09 / LAC-2F-V3-01) to VENDORING that upstream shell. Under the CH2 GENERALITY lens this is admissible ONLY if the vendored shell is grammar-PARAMETERISED (byte set = caller data); but 2F's own LAC-2F-V3-01 close test flags the genuine risk (a verbatim nibble-LUT port plants a SECOND mask convention inside `bbnf-simd`). The row is grounded but the GENERALITY status of the vendored `scan_balanced` shell is asserted before any check that the upstream `find_first_of_nibble_lut`/`build_nibble_luts` classifier takes its alphabet as caller data rather than a hard-coded CSS/structural table. Correction: gate PTG-2F-09's `grounded` on the same (a)-(b) byte-set-as-caller-data falsifier 2C applies to `css_balanced_component_scan` — a vendored shell whose classifier hardcodes the structural alphabet is grammar-SPECIFIC and inadmissible regardless of provenance. (2F already names the mask-unification close test; it must also name the caller-data-alphabet test before `grounded`.) | 2F:174,184 LAC-2F-V3-01 names `build_nibble_luts`/`find_first_of_nibble_lut` and the second-mask-substrate risk; the caller-data-alphabet gate is implied by 2C (a)-(b) but not stated for the vendored upstream classifier. |
| CH2-V2-04 | 2A | **REVISE** | The SinkOnly/sonic-rs JSON rows are now honestly scoped (T2A-V1-SOTA-JSON-003 explicitly states the JSON direct product path is scan-free so a sonic-rs leaf "has no SK-V18 JSON consumer; its only viable same-wave consumer is the G5/G6 CSS scan or a SK-V19 receiver"). This is correct discipline. The residual defect is narrower: the same row keeps `close_status=source-present-unwired` AND `transfer_reason=targeted leaves may transfer as grammar-neutral byte-set/string/number/lookup/trivia primitives` — i.e. it proposes transferring a JSON-paper-sourced primitive (sonic-rs float/string/lookup leaves) whose ONLY admissible consumer it itself names is CSS. That is precisely the "grounded JSON-only but used fleet-wide (on CSS)" shape, even though it is honestly disclosed. Correction: tighten to state that NO sonic-rs leaf transfers in SK-V18 absent a profiled CSS hot-leaf that needs that exact primitive (the 94.1% CSS scan needs the eq-set/balanced family, NOT a float/string leaf); the float/string/lookup leaves are SK-V19 receivers with no SK-V18 consumer, so their `close_status` should be `blocked` (no SK-V18 consumer), not `source-present-unwired` (which implies a present-but-unwired SK-V18 consumer exists). | 2A:56 row carries `same_wave_consumer=G5/G6 CSS scan or a SK-V19 receiver` + `close_status=source-present-unwired`; the G5/G6 hot leaf (`SYNTHESIS-PROFILE.md:96-98`) is the balanced/eq-set scan, not a sonic-rs float/string leaf — so those leaves have no SK-V18 consumer. |
| CH2-V2-05 | 2C | ACCEPT | The `css_balanced_component_scan` FORCED-demotion is grammar-NEUTRAL and correctly grounded: the byte-SKIP shell emits nothing, the two offered non-CSS dischargers (JSON `parse_object_direct`/`parse_array_direct`, Sheets `paren_expr→expression`) are parse-with-emit descents (verified `google-sheets.bbnf:137,163`), structurally incompatible — so the CSS-scoped rename + honest disclosure IS the discharge, NOT a fabricated cross-grammar caller. The inner-kernel split (base one-fan STRUCTURALLY neutral; two-fan composition CSS-only) is exactly correct: `find_css_significant` has ONE caller, `lib.rs:574` inside `#[cfg(test)]` (verified). | `sed runtime_simd.rs:160-216` two-fan body verified; `rg find_css_significant` = sole caller `lib.rs:574` under the L5 `#[cfg(test)]` comment at `:500`; W3C CSS Syntax "consume a component value" cited. |
| CH2-V2-06 | 2C | ACCEPT | The Sheets precedence-tower negative control is grammar-NEUTRAL and correctly grounded as the SOLE Sheets-distinctive construct. Pratt "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 — VERIFIED (ACM Digital Library: title/author/venue/DOI all confirmed). The 7-level cascade `comparison → concat → additive → multiplicative → exp → unary → postfix → primary` with cyclic `paren_expr → expression` and `expression = comparison_expr` is verified verbatim in the grammar (`google-sheets.bbnf:103,105,...,167`), including the `concat_expr` link 2C names. Lowers to existing `SinkOnlyExpr` vocabulary, needs no new IR primitive — a true generality stress, not a JSON-only technique. The `Nu8u8` 295×/21× demotion is sourced to `SYNTHESIS-RESEARCH.md:42`, which carries both figures verbatim (verified). | DOI verified via ACM/web; `google-sheets.bbnf:97-167` cascade verified line-by-line; `SYNTHESIS-RESEARCH.md:42` contains "295×"/"21×". |
| CH2-V2-07 | 2C | ACCEPT | The Lock-14 self-gate falsification is real and grammar-NEUTRAL: LOCKS.md:349 asserts the 4-name regex "returns ZERO", but the live `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` returns 13 sites. The narrow 4-name regex catches only 4 of 9 grammar-named `idents` rows (Csv/Math/Bnf/Ebnf/CssPretty escape). 2C correctly routes the structural full-row collapse to SK-V19 with `tranche_scope` inline (the empirical leak is SK-V18-verified; the close is SK-V19). | `strategy.rs:137-185` = 9 grammar-named `idents` rows verified; `rg ... crates/ir/src/ crates/analysis/src/` = 13 (asserts ZERO) verified live; consumed via `for_grammar_with_manifest` at `:216`. |
| CH2-V2-08 | 2C | ACCEPT | `css_types.rs` Lock-14-(c) refutation is correct and grammar-neutral: the file lives in the GENERIC `crates/core/src/css_types.rs` (verified on disk, 66 LOC), not a `crates/<grammar>/` declaration crate, so Lock 14 (c) does not admit it. Correctly carries `tranche_scope=SK-V19-receiver` (relocate-or-delete). | `ls crates/core/src/css_types.rs` confirmed; LOCKS.md:349 names it verbatim as the mess. |
| CH2-V2-09 | 2C/2D | ACCEPT | The 5-shape `BackendShape` dispatch backbone is genuinely grammar-NEUTRAL: `select_lowering(cost.chosen)` dispatches exactly `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` from the cost verdict, never a grammar tag (`lower/mod.rs:18-24` verified, no sixth shape). 2D carries LAC-2D-04 (exactly five, no sixth) verbatim. The `emit_shape_source==lowered_program` firewall + `runtime_target_rows_collapsed` co-gate + the NEW CSS-typed-surface side-channel firewall (`css_provider_source==generated`) are the correct neutrality falsifiers. The `RuntimeEmitterKind{CompiledLowering,RequestFacts}` second discriminator R-A deletes is verified live (`grammar_provider.rs:39-43`). | `lower/mod.rs:18-24`, `grammar_provider.rs:39-43` verified; iburg LOPLAS 1992 DOI 10.1145/151640.151642, egg POPL 2021, Mison VLDB 2017 cite-grounded. |
| CH2-V2-10 | 2D | ACCEPT | The cost-model engine activation is grammar-neutral and the V2 "zero-rule scaffold" supersession is accurate at HEAD: `NormalizeDirectSinkCost` is a live `Rewrite` impl; `collapsed_stage.rs:16 lower_rule` delegates to `tape_plan::render_rule(.., Collapsed)`. Dispatch is on BackendShape, not grammar — no JSON-only overfit. The simdjson tape URL (V1 CH1 fold) is replaced — no `simdjson.github.io/.../tape.html` remains in 2D. | `collapsed_stage.rs:16` verified; `rg simdjson.github.io` in 2D = 0 (folded). |
| CH2-V2-11 | 2A | ACCEPT | The JSON SOTA plane is honest and grammar-appropriate: sonic-rs direct-to-struct (no tape) vs simd-json tape-then-struct — VERIFIED via sonic-rs README ("Sonic-rs directly parses the JSON into a Rust struct, and there are no temporary data structures"). T2A-V18-JSON-SONIC-001 grounds the strict-vs-sonic-rs-strict bar same-plane. The `track1_rich`-vs-eager-CSSOM lazy-vs-eager framing enum (T2A-V18-CSS-LAZY-001) is the correct honest comparator discipline. The dav1d checkasm-correctness-only gate (`g6_speedup_claim_emitted==false` pre-H1) prevents the parity-PASS-narrated-as-SOTA paper-close. | sonic-rs README verified via WebFetch; `RESULTS.md` JSON+CSS rows; the GENERALIZATION header is scoped to JSON+CSS per CH2-V1-04 fold. |
| CH2-V2-12 | 2E | ACCEPT | x86/AVX-512 esoterica are correctly held grammar-neutral SECONDARY-totality-only (never an M5 Max close route); aarch64-ONLY standing grounded; x86 DELETED in skinny (P1). The eq-set NEON eq-fan and the SHRN movemask carry the byte set as caller data (Lock 14). svmatch re-refuted (FEAT_SVE2 ABSENT on the M5 Max host probe). No JSON-only-grounded-but-fleet-used defect in 2E's host-arch rows. | Lemire-2026 ARM-match post EXISTS (verified V1); host probe `FEAT_SVE2=ABSENT`; x86 totality-SECONDARY-only. |
| CH2-V2-13 | 2C/2F | ACCEPT | The two-fan ≤13-byte OR-reduce COMPOSITION (`find_css_significant`) is correctly disclosed as CSS-exercised-ONLY (only `#[cfg(test)]` caller `lib.rs:574`), subject to the SAME neutrality-proof obligation as the shell — forced-demote or prove a non-CSS two-fan caller. This is the correct GENERALITY discipline: the composition is NOT presumed neutral just because its constituent one-fan kernel is structurally neutral. | `rg find_css_significant` = sole non-def caller `lib.rs:574` under `#[cfg(test)]`; 2C:202 + 2B:260 + 2F:85 all carry the CSS-only/forced-demote disclosure. |
| CH2-V2-14 | 2C | **REJECT** | The 9-grammar-fleet onboarding row (SK-V18-2C-9-GRAMMAR-FLEET-ONBOARDING-TEST, 2C:205) carries `grounded / refuted / partial = grounded` in the status column while its own inline `tranche_scope` correctly states "the `grounded` status is SK-V18-closeable ONLY for the 3 witnessed grammars; the fleet-wide claim is a SK-V19 receiver, not SK-V18-closeable". A single row whose STATUS cell says `grounded` and whose SCOPE cell says "not SK-V18-closeable" is internally contradictory under the CH2 lens — the un-qualified `grounded` is exactly the fleet-wide-wording defect LAC-2C-SK18-02 forbids. Correction: re-key the status cell to `partial (SK-V18-witnessed-3 / SK-V19-receiver-9)` so the status column itself carries the scoping, not only the prose suffix; a status-column `grounded` on a fleet-wide row witnessed by 3 of 9 grammars is a status a future consumer can cite without the scope. This is generality-load-bearing because this row IS the fleet-collapse genericity proof. | 2C:205 status cell = `grounded`; same row `tranche_scope` = "not SK-V18-closeable" for the fleet-wide claim; only 3 of 9 grammars witnessed (JSON+CSS+Sheets); LAC-2C-SK18-02 binds: a generalisation witnessed by fewer than the full roster may NOT use fleet-wide grammar-neutral wording — a `grounded` status cell on a 9-grammar row is fleet-wide wording. |

## Spot-Verified Citations (CH2 most load-bearing)

- **Pratt, "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931** —
  VERIFIED (ACM Digital Library record + web; title/author/venue/DOI all confirmed).
  2C Sheets negative-control grounding is real and grammar-neutral.
- **Hyperscan, Wang/Hong/Chang/Park/Langdale/Hu/Zhu, NSDI 2019 (SHUFTI/TRUFFLE)** —
  VERIFIED EXISTS (USENIX NSDI '19; authors confirmed). 2F SHUFTI abstract-name
  grounding for the eq-set classifier is legitimate.
- **sonic-rs direct-to-struct (no tape) vs simd-json tape-then-struct** — VERIFIED via
  README ("directly parses the JSON into a Rust struct, no temporary data structures").
  2A T2A-V18-JSON-SONIC-001 same-plane bar is accurate.
- **`Nu8u8` 295×/21× CSS-vs-Sheets figure** — VERIFIED `SYNTHESIS-RESEARCH.md:42`
  carries both figures verbatim (the 2C V3 citation provenance is accurate).
- **`crates/ir/src/registry/strategy.rs:137-185`** — VERIFIED 9 grammar-named `idents`
  rows; the Lock-14 self-gate `rg ... = 13` (asserts ZERO) is RED (2C refutation accurate).
- **5-shape `select_lowering`** — VERIFIED `lower/mod.rs:18-24` = exactly five shapes,
  no sixth (2C/2D backbone neutrality accurate).
- **eq-set kernel JSON consumer** — RE-CONFIRMED FALSIFIED at HEAD:
  `byte_class_from_eq_set_64` live runtime callers = `runtime_simd.rs:44,56,199` (all
  CSS); `find_ascii_set_member64` = report.rs telemetry + checkasm test only (zero
  runtime callers); `scan_structurals → scan_structurals_scalar` (scan-free, never the
  eq-set kernel). The dossiers fold this correctly; the SOURCE comment
  `runtime_simd.rs:6-7` and `SYNTHESIS-RESEARCH.md:43` do NOT (CH2-V2-01, CH2-V2-02).

## Fold Requirements

REVISE folds (V3):
1. **2B/2F/2C (CH2-V2-01):** add an inline note that the `runtime_simd.rs:6-7` source
   comment ("the same kernel JSON's `scan_structurals` rides") is INACCURATE — a
   same-wave G6 source-fix obligation — so the cited neutrality-evidence file does not
   re-seed the refuted dual-consumer claim. The abstract neutrality stands; only the
   source comment is wrong.
2. **2C V3 (CH2-V2-02):** add an OQ/note that the R-F `SYNTHESIS-RESEARCH.md:43`
   justification ("JSON's `scan_structurals` already rides") is the same falsified
   claim; re-state the honest R-F grounding as "JSON product path is scan-free → eq-set
   neutrality is STRUCTURAL, not empirical-dual-consumer".
3. **2F (CH2-V2-03):** gate PTG-2F-09's vendored `scan_balanced` shell `grounded`
   status on the byte-set-as-caller-data (a)-(b) falsifier, not just the mask-unification
   close test; a vendored classifier hardcoding the structural alphabet is
   grammar-SPECIFIC regardless of provenance.
4. **2A (CH2-V2-04):** re-key the sonic-rs targeted-leaf row `close_status` from
   `source-present-unwired` to `blocked` (no SK-V18 consumer): the 94.1% CSS hot leaf
   needs the eq-set/balanced family, not a sonic-rs float/string/lookup leaf, so those
   leaves are SK-V19 receivers with no SK-V18 consumer.

REJECT fold (V3):
5. **2C (CH2-V2-14):** re-key the 9-grammar-fleet onboarding row STATUS cell from
   `grounded` to `partial (SK-V18-witnessed-3 / SK-V19-receiver-9)` so the status
   column itself carries the LAC-2C-SK18-02 scoping; a `grounded` status on a
   fleet-wide row witnessed by 3 of 9 grammars is the fleet-wide-wording defect.

## Non-Regression Confirmation (V1 ACCEPT surfaces preserved)

- 5-shape `BackendShape` canon intact; no sixth shape (CH2-V2-09).
- Lock-14 self-gate falsification + `css_types.rs` + 9-ident leak correctly
  SK-V19-scoped (CH2-V2-07, -08).
- Sheets precedence-tower negative control + Pratt grounding intact (CH2-V2-06).
- No CSS broadcast/brace-counter/fact-stream parity admission; no self-excluding
  Lock 14 gate; no x86 M5 Max close route (CH2-V2-12); aarch64-ONLY held.

## Convergence Impact

This CH2 result BLOCKS T-P2 V2 convergence: 4 REVISE + 1 REJECT folds are required.
The defects are localized and do NOT touch the core GENERALITY backbone — the
five V1 folds (eq-set neutrality split, 2A SOTA scoping, 67→71 census) are all
correctly applied, and every load-bearing citation (Pratt, Hyperscan, sonic-rs,
iburg/egg/Mison, the 9-ident leak, the 5-shape canon, the 295×/21× figure) VERIFIES.
The residual V2 defects are: (a) two CITED artifacts (the `runtime_simd.rs:6-7`
source comment and the upstream `SYNTHESIS-RESEARCH.md:43` R-F row) still carry the
refuted JSON-`scan_structurals`-rides claim the dossiers themselves refute; (b) a
vendored-shell GENERALITY status asserted before the caller-data-alphabet check; (c)
a sonic-rs leaf row mis-stating `source-present-unwired` for a primitive with no
SK-V18 consumer; and (d) a `grounded` status cell on a 9-grammar fleet row witnessed
by only 3 grammars.

TALLY accept=9 revise=4 reject=1
