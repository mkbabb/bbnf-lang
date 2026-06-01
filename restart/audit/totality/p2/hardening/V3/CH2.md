# CH2 Generality — T-P2 V3 (SK-V18 totality)

Lens: **CH2 GENERALITY** (Lock 14 transfer — every primitive/technique grounded
grammar-NEUTRALLY; the technique must transfer to CSS L4 / Sheets / BBNF-self /
the 9-grammar fleet, not JSON-only). A technique grounded JSON-only but used
fleet-wide is a REVISE; a confabulated/unverifiable citation or a
refuted-route grounding is a REJECT.

Target packet (the freshly-regenerated Jun-1 SK-V18 dossiers, `git`-dirty,
under hardening): `2A-sota-landscape.md`, `2B-primitive-vocabulary.md`,
`2C-grammar-neutrality.md`, `2D-cost-model.md`, `2E-host-arch-esoterica.md`,
`2F-parse-that-gaps.md`.

Disposition: **REVISE** (one residual fold-discipline gap blocks a clean V3;
the generality framing is otherwise sound and the V1 REVISE/REJECT folds were
applied). The lone REVISE is narrow and does not touch the live SK-V18 rows.

## Cycle context

This is the SK-V18 cycle-V3 confirmation pass. V1 (this same SK-V18 packet)
returned `accept=8 revise=4 reject=1` (4 REVISE all on the eq-set dual-consumer
overstatement, 1 REJECT on the Pattern-H "exactly 67" stale-as-fact). This
pass re-verifies that those folds landed and adversarially re-grounds the
generality surface against HEAD + primary citations.

## Critical Findings (CH2-generality groundings/refutations enumerated)

| id | dossier | disposition | finding | falsifying / corroborating evidence (verified this pass) |
|---|---|---|---|---|
| CH2-V3-01 | 2B | **REVISE** | The retained SK-V15 V2 rows — Tech-Grounding row (`2B:74`, "Current aarch64 eq-set is a real NEON primitive body") and the A3a manifest row (`2B:160`) — still carry the V1-STRUCK dual-consumer framing: `transfer_reason=eq-set is a grammar-neutral byte-class primitive **used by JSON and non-JSON receivers**`, `verification_action=measure **find_ascii_set_member64** plus one non-JSON FIRST/trivia row`, `same_wave_consumer=find_ascii_set_member64`, `row_movement_target=named **JSON**/non-JSON row`. This is the exact JSON-consumer claim V1 CH2-V1-01/02/03 refuted and the SK-V18 extension (`2B:267`) explicitly corrects. The Pattern-H "67" stale-fact got an inline supersession parenthetical in 2C (`2C:75`,`:102`); the eq-set JSON-consumer stale-fact did NOT — the historical rows present a refuted JSON-consumer fact as a live transfer reason with no supersession marker. **Generality relevance:** these rows ground the neutrality of a primitive the fleet-wide §6 escape leans on, on a JSON consumer that does not exist. Correction: annotate `2B:74` and `2B:160` (mirroring the 2C Pattern-H treatment) — strike/qualify "used by JSON and non-JSON receivers", note `find_ascii_set_member64` has no live runtime caller and the JSON path rides `byte_class_from_table_64` (a different primitive), and point to the SK-V18 row at `:267`; keep the structural-neutrality argument (caller-supplied byte set, kernel names no grammar). | `rg find_ascii_set_member64 skinny/crates/runtime/src` = ZERO grammar callers (only `lib.rs:209` def, checkasm test, `report.rs` telemetry strings). JSON aarch64 `neon::scan` rides `classify_tbl4` (the TBL `byte_class_from_table_64` family), NOT the eq-set kernel (`json/scan.rs:200-235`). The eq-set kernel's only live consumer is CSS `count_top_level_commas` (`runtime_simd.rs:44,56,199`). 2B's own SK-V18 row `:267` states this correctly; the V2 rows `:74`/`:160` were left un-annotated. |
| CH2-V3-02 | 2B/2C/2F | ACCEPT | The eq-set inner-kernel neutrality is now grounded STRUCTURALLY (caller-supplied byte set, kernel names no grammar) and explicitly disavows the empirical dual-consumer claim across all LIVE SK-V18 rows (`2B:267,288-293`; `2C:212`; `2F:82,130-137`). 2C even SPLITS the neutrality: base one-fan kernel structurally neutral; two-fan OR-reduce COMPOSITION (`find_css_significant` shape) CSS-exercised-only and subject to the same neutrality-proof. The inaccurate `runtime_simd.rs:6-7` source comment ("the same kernel JSON's `scan_structurals` rides") is flagged as a same-wave G6 source-fix in all three dossiers (`2B:298-304`; `2C:317`; `2F:82`). The V1 REVISE folds landed. | `runtime_simd.rs:6-7` comment verified inaccurate (JSON `scan_structurals`→`neon::scan`→`classify_tbl4`, never eq-set). `find_css_significant` callers = `lib.rs:574` `#[cfg(test)]` only (verified). Two-fan OR-reduce verified live at `runtime_simd.rs:199` (`set_a`/`set_b` + `|`). |
| CH2-V3-03 | 2C | ACCEPT | The `balanced_component_scan` → `css_balanced_component_scan` FORCED demotion is correctly grounded grammar-neutrally: the byte-SKIP shell emits nothing, while the two offered non-CSS dischargers are PARSE-with-emit descents, structurally incompatible. The CSS-scoped name + honest disclosure IS the discharge (no fabricated cross-grammar caller). This is the textbook CH2 outcome: a single-grammar-exercised primitive honestly scoped, not falsely neutral. | `parse_object_direct`/`parse_array_direct` verified as `<S: JsonSink>` sink-emit descents (`json/generated.rs:903,937`); Sheets `paren_expr → expression` verified cyclic (`google-sheets.bbnf:137,163`). W3C CSS Syntax "consume a component value" is a real CSS algorithm. |
| CH2-V3-04 | 2C | ACCEPT | The Lock-14 self-gate falsification is real and grammar-NEUTRAL across the fleet: `LOCKS.md:349` asserts the grep "returns ZERO"; the live grep returns 13. The 9-row grammar-named `idents` table sits in the GENERIC `ir` crate; the narrow 4-name regex catches only 4 of 9 (Csv/Math/Bnf/Ebnf/CssPretty escape). Correctly refuted as the totality relocated-seam and routed to SK-V19 (`tranche_scope` inline). | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = **13** (verified). `strategy.rs:137-185` = 9 grammar-named rows (verified), consumed via `for_grammar_with_manifest`/`:216`. |
| CH2-V3-05 | 2C | ACCEPT | `css_types.rs` Lock-14-(c) refutation is correct: the file lives in the GENERIC `crates/core/src/css_types.rs` (Lock 14 (c) admits only `crates/<grammar>/`), so it is the lock-named mess, routed to SK-V19 relocate-or-delete. Grammar-neutral finding. | `ls crates/core/src/css_types.rs` = 2373 B (verified); header "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)`" verified. |
| CH2-V3-06 | 2C | ACCEPT | Sheets precedence-tower negative control is the correct CH2 generality stress and the citation is real: Pratt "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 (VERIFIED via ACM). The tower (`comparison→concat→add→mul→exp→unary→postfix→primary`) is the SOLE Sheets-distinctive shape JSON+CSS structurally lack; lowers to existing `SinkOnlyExpr` vocabulary (no new IR primitive); the `Nu8u8` 295×/21× demotion correctly removes a SHARED construct from the litmus. | DOI resolves (ACM). Tower verified verbatim at `google-sheets.bbnf:97-163` (right-iterated `A = B (op B)*`, op tables `compare_op/add_op/mul_op/unary_prefix`). |
| CH2-V3-07 | 2C/2D | ACCEPT | The 5-shape `BackendShape` is the genuinely grammar-NEUTRAL dispatch backbone, and the relocated-seam firewall (`emit_shape_source==lowered_program` + `runtime_target_rows_collapsed` co-gate; md5-distinct = necessary-not-sufficient) is the correct neutrality falsifier. The `RuntimeEmitterKind{CompiledLowering,RequestFacts}` second discriminator R-A deletes is verified live. The iburg/egg/Mison/OR-Tools cites are real. No sixth shape; no JSON-only overfit. | `lower/mod.rs:18-24` = exactly 5 shapes dispatched on `cost.chosen` (verified). `RuntimeEmitterKind` fork at `grammar_provider.rs:40-43` (verified). iburg LOPLAS 1992 DOI 10.1145/151640.151642 (VERIFIED via ACM/Arizona). |
| CH2-V3-08 | 2C | ACCEPT | The single-file Sheets import-closure onboarding (a derived DATA flag in `RuntimeFrontendRequirements`, NOT a `match grammar` arm) and the 9-grammar fleet onboarding test are correctly scoped: SK-V18 witnesses the un-fork on 3 (JSON+CSS+Sheets); with one negative-control witnessed the claim is SCOPED to the witnessed grammars (LAC-2C-SK18-02). Fleet-wide wording deferred to SK-V19. This is the correct GENERALITY discipline. | Roster verified: `crates/core/src/grammar/generated/` = 9 grammars; `grammar/` = 8 source roots. `tranche_scope=SK-V18-witnessed-3 / SK-V19-receiver` inline on the fleet row. |
| CH2-V3-09 | 2A | ACCEPT | The V1 SOTA-scope fold landed: 2A now explicitly scopes the measured >SOTA plane to JSON+CSS ONLY (per LAC-2C-SK18-02), names Sheets a GENERALITY (not SOTA) proof, and defers the 9-grammar fleet SOTA to SK-V19 (`2A:142-148`). The dav1d/FFmpeg checkasm PROCESS transfer (not pixel kernels) is grounded; the in-tree replica claim verifies. Lemire-2026 ARM-match post is real. | `2A:142-148` scoping verified. `checkasm_parity.rs:3` "Modelled on FFmpeg's `tests/checkasm/checkasm.h`" verified. Lemire 2026-04-19 "fastest way to match characters on ARM" post EXISTS (verified; SVE2-match fastest, NEON eq-fan the deployable route). |
| CH2-V3-10 | 2E | ACCEPT | x86/AVX-512 esoterica held grammar-neutral SECONDARY (never an M5 Max close route); aarch64-ONLY standing grounded; the eq-set/two-fan kernels carry the byte-set as caller data (Lock 14). The SVE2-absence refutation of NEON-svmatch verifies on the host. The movemask-divergence co-gate (`bbnf_simd_single_mask_convention`, `LAC-2E-V6-03`) is a real intra-crate KISS/DRY neutrality-adjacent guard. No JSON-only-grounded-but-fleet-used defect. | Host probe verified: Apple M5 Max, FEAT_PMULL/DotProd/CSSC=1, **FEAT_SVE2 absent**. `movemask.rs:5` canonical `vshrn_n_u16::<4>` SHRN vs `byte_class_from_eq_set_64.rs:79-87` divergent shift-add `vaddv_u8` — both verified (real divergence). |
| CH2-V3-11 | 2D | ACCEPT | The cost-model generality is correctly held as an UNKNOWN, not asserted: `UNKNOWN-2D-V3-03` asks whether `derive_backend_shape` over the Sheets 7-level tower selects a viable shape WITHOUT a grammar-name special-case, routing the answer to PROVE — exactly the CH2 discipline (if the cost model can only shape JSON/CSS rule patterns, the tower exposes a §6 overfit). No sixth shape; dispatch on `BackendShape`, not grammar. The V2 "zero-rule scaffold" supersession is accurate at HEAD. | `lower/mod.rs:18-24` 5-shape verified; `UNKNOWN-2D-V3-03` scopes generality to a PROVE test, not a claim. egg POPL 2021 DOI 10.1145/3434304 + Mison PVLDB 2017 are real (carried from V1 verification). |
| CH2-V3-12 | 2F | ACCEPT | The eq-set member scan is grounded STRUCTURALLY neutral with the shell honestly CSS-scoped (`2F:130-142`), the `find_css_significant` wire-as-is refutation is correct (flat skip vs recursive shell), and the upstream-vs-vendor provenance reconcile is fenced with a structural mask-convention co-gate (`LAC-2F-V3-01`, `bbnf_simd_single_mask_convention`) so a renamed second nibble-LUT classifier is caught alias-immune. The Pattern-H/RegexHir/float rows carry no JSON-only-fleet-used defect. | `find_css_significant` flat-skip vs recursive `find_component_delim`/`consume_balanced_at` verified (`generated.rs:657-713`; `runtime_simd.rs:169-214`). Provenance co-gate is structural, not a name-grep. |

Enumerated: **12 CH2-relevant groundings/refutations — 11 ACCEPT, 1 REVISE, 0 REJECT.**

## Evidence Inspected

- All six target dossiers in full (2A 277L, 2B 436L, 2C 348L, 2D 144L, 2E 240L, 2F 192L).
- Prior SK-V18 CH2 outputs: `hardening/V1/CH2.md` (the 8/4/1 V1 verdict) and the
  SK-V15-era `hardening/V3/CH2.md` (history; overwritten by this file).
- T-P2-DISPATCH-CONTEXT.md, the V3 CHALLENGE-CONTEXT.md.
- On-disk verification (HEAD):
  - `find_ascii_set_member64` callers (`rg` skinny/crates) — no live runtime caller.
  - `runtime_simd.rs:1-15` (the inaccurate JSON-rides source comment), `:44,56,199` (CSS-only live consumers).
  - `json/scan.rs:22-29,200-235` (JSON `scan_structurals`→`neon::scan`→`classify_tbl4`, NOT eq-set; `scan_dispatch`→`byte_class_from_table_64`).
  - `find_css_significant` callers (`lib.rs:574` `#[cfg(test)]` only); two-fan OR-reduce at `runtime_simd.rs:199`.
  - Lock-14 self-gate grep = 13 sites; `strategy.rs:137-185` 9-ident table; `crates/core/src/css_types.rs` (generic core, 2373 B).
  - `lower/mod.rs:18-24` 5-shape canon; `grammar_provider.rs:40-43` `RuntimeEmitterKind` fork.
  - `google-sheets.bbnf:97-163` precedence tower; `json/generated.rs:903,937` sink-emit dischargers.
  - `generated.rs:304-307` CSS lazy-rich; `checkasm_parity.rs:3` FFmpeg-modelled.
  - Host probe (`sysctl`): Apple M5 Max, FEAT_SVE2 absent; `movemask.rs:5` SHRN vs `byte_class_from_eq_set_64.rs:79-87` shift-add.
  - `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` = 71 (Pattern-H census).
- Citation verification (WebSearch): Pratt POPL 1973 DOI 10.1145/512927.512931 (real);
  iburg LOPLAS 1992 DOI 10.1145/151640.151642 (real); Lemire 2026-04-19 ARM-match post (real, at the cited URL).

## Fold Requirements (REVISE)

CH2-V3-01 — Annotate the two retained SK-V15 V2 rows in 2B that still present the
refuted JSON-consumer claim as a live transfer reason, mirroring the inline
supersession 2C already applies to the Pattern-H "67" row:

1. `2B:74` (Tech-Grounding "Current aarch64 eq-set is a real NEON primitive body"):
   strike or qualify `transfer_reason=…used by JSON and non-JSON receivers`,
   `verification_action=…measure find_ascii_set_member64 plus one non-JSON…`,
   and `row_movement_target=named JSON/non-JSON row`. Add a parenthetical:
   "(JSON-consumer framing SUPERSEDED — `find_ascii_set_member64` has no live
   runtime caller; the JSON path rides `byte_class_from_table_64`; see the
   SK-V18 row at `:267`. Structural neutrality stands; the kernel's only live
   production consumer is CSS `count_top_level_commas`.)"
2. `2B:160` (A3a manifest eq-set row): same annotation; change
   `same_wave_consumer=find_ascii_set_member64, plus required non-JSON receiver`
   to disclose the CSS-only live consumer with the supersession pointer.

This is a fold-discipline consistency fix, not a re-derivation: the SK-V18 live
rows are already correct. No 2A/2C/2D/2E/2F edits are required for CH2.

## Convergence Impact

CH2 is **REVISE** and BLOCKS T-P2 V3 convergence: 1 REVISE fold (CH2-V3-01) is
required. The defect is confined to the retained historical section of one
dossier and does not touch any live SK-V18 grounding, citation, or scope claim;
once 2B's two V2 rows carry the supersession annotation, the generality surface
is clean — every primitive/technique is grounded grammar-NEUTRALLY (structural
neutrality for the eq-set kernel, honest CSS-scoping for the shell, real
fleet-stress via the Sheets tower, neutral 5-shape dispatch), the Lock-14
relocated-seam leaks are correctly refuted and SK-V19-scoped, the JSON+CSS-only
SOTA scope is honored fleet-wide, and the load-bearing citations
(Pratt 1973, iburg 1992, Lemire 2026) verify as real.

TALLY accept=11 revise=1 reject=0
