# CH2 Generality — SK-V18 T-P2 (cycle V4)

Lens: **CH2 GENERALITY** (Lock 14 transfer — every primitive/technique must be
grounded grammar-NEUTRALLY; 2C must show the technique transferring to CSS L4 /
Sheets / BBNF-self / the 9-grammar fleet, not JSON-only). A technique grounded
JSON-only but used fleet-wide is a REVISE; a confabulated/unverifiable citation
or a refuted-route grounding is a REJECT.

Target packet (the Jun-1 SK-V18 dossiers, `git`-dirty, under hardening):
`2A-sota-landscape.md`, `2B-primitive-vocabulary.md`, `2C-grammar-neutrality.md`,
`2D-cost-model.md`, `2E-host-arch-esoterica.md`, `2F-parse-that-gaps.md`.

Disposition: **ACCEPT** (12/12 CH2-relevant groundings/refutations ACCEPT; 0
REVISE; 0 REJECT).

## Cycle context

This is the SK-V18 cycle-V4 confirmation pass. The fold history into the
Jun-1-regenerated packet is: V1 returned `accept=8 revise=4 reject=1` (4 REVISE on
the eq-set false-JSON-consumer dual-consumer claim across 2B/2C/2F + the unscoped
2A fleet-wide >SOTA framing; 1 REJECT on the Pattern-H "exactly 67" stale-as-fact);
V3 returned `accept=11 revise=1 reject=0` (the lone REVISE CH2-V3-01 being a
fold-discipline consistency fix: annotate the retained SK-V15 V2 rows `2B:74`/`:160`
that still carried the refuted JSON-consumer framing). This V4 pass independently
re-grounds the GENERALITY surface against HEAD and re-verifies the most
load-bearing citations from primary sources, then confirms every prior fold
landed in the regenerated packet. The cycle-V1 ≥30% REVISE expectation is a
first-cycle heuristic superseded by three fold cycles (V1→V2→V3) that already
drove the residual defect set to a single fold-discipline annotation; forcing a
REVISE here would be confabulation, not adversarial discipline. Every defect
prior cycles named is verified resolved at HEAD, and the independent re-grounding
surfaced no new generality leak.

## Critical Findings (CH2-generality groundings/refutations enumerated)

| id | dossier | disposition | finding | falsifying / corroborating evidence (verified this pass) |
|---|---|---|---|---|
| CH2-V4-01 | 2B | ACCEPT | The V3 lone REVISE (CH2-V3-01) LANDED: the two retained SK-V15 V2 rows now carry the supersession annotation. `2B:74` (Tech-Grounding "Current aarch64 eq-set is a real NEON primitive body") strikes the dual-consumer framing inline — "JSON-consumer framing SUPERSEDED — `find_ascii_set_member64` has NO live runtime caller; the JSON aarch64 path rides the DIFFERENT `byte_class_from_table_64`/TBL classifier via `neon::scan`→`classify_tbl4`, never the eq-set kernel; the eq-set kernel's only live production consumer is CSS `count_top_level_commas`". `same_wave_consumer`/`row_movement_target` cells are re-keyed to the CSS consumer with `(SUPERSEDED ...)` markers. The A3a manifest row (`2B:160`) carries the identical annotation. The structural-neutrality argument (caller-supplied byte set, kernel names no grammar) is kept; the empirical dual-consumer claim is REFUTED throughout. | `2B:74`,`:160`,`:267`,`:288-304` read this pass — all three rows carry the supersession marker. On disk: `rg find_ascii_set_member64 skinny/crates/runtime/src` = ZERO runtime callers (verified this pass); JSON `scan_structurals`→`neon::scan` rides `classify_tbl4` (`json/scan.rs:214,219,228`), NOT the eq-set kernel (`rg byte_class_from_eq_set_64 grammars/json/` = 0); eq-set live consumer is CSS `runtime_simd.rs:44,56,199`. |
| CH2-V4-02 | 2B/2C/2F | ACCEPT | The eq-set inner-kernel neutrality is grounded STRUCTURALLY across all three dossiers (caller-supplied ≤8-byte set, kernel names no grammar) and explicitly disavows the empirical dual-consumer claim on every LIVE SK-V18 row. 2C SPLITS the neutrality correctly: BASE one-fan kernel structurally neutral; TWO-FAN ≤13-byte OR-reduce COMPOSITION (`find_css_significant` shape) CSS-exercised-ONLY and subject to the same neutrality-proof. The inaccurate `runtime_simd.rs:6-7` source comment ("the same kernel JSON's `scan_structurals` rides") is flagged as a same-wave G6 source-fix in all three (`2B:298-304`, `2C:256-264`, `2F:86`), not cited as evidence. | `runtime_simd.rs:6-7` comment verified inaccurate this pass (JSON `scan_structurals`→`scan_structurals_scalar`/`neon::scan`→`classify_tbl4`, never eq-set). `find_css_significant` signature verified `fixed:&[u8;9]`, two-fan `set_a`(8)+`set_b`(≤5) OR-reduce over `byte_class_from_eq_set_64` (`runtime_simd.rs:169-200`); sole caller `lib.rs:574` `#[cfg(test)]`. |
| CH2-V4-03 | 2A | ACCEPT | The V1 SOTA-scope fold (CH2-V1-04) LANDED and is generality-correct: 2A explicitly scopes the measured >SOTA plane to JSON+CSS ONLY (per LAC-2C-SK18-02), names Sheets a GENERALITY (not a SOTA) proof, and defers the 9-grammar fleet SOTA to SK-V19 — "Sheets/CSV/Math/BNF/EBNF/CssPretty have no grounded SOTA comparator here; the 9-grammar fleet SOTA is SK-V19". No fleet-wide >SOTA framing survives over un-witnessed grammars. | `2A:144-150` scoping verified verbatim this pass. The cross-referenced LAC-2C-SK18-02 binds the scoping discipline (`2C:375`). |
| CH2-V4-04 | 2C | ACCEPT | The Lock-14 self-gate falsification is real and grammar-NEUTRAL across the fleet: `LOCKS.md:349` asserts the grep "returns ZERO"; the live grep returns 13. The 9-row grammar-named `idents` table sits in the GENERIC `ir` crate; the narrow 4-name regex catches only 4 of 9 (Csv/Math/Bnf/Ebnf/CssPretty escape). Correctly refuted as the totality relocated-seam, routed to SK-V19 (`tranche_scope` inline at `2C:223`). | `rg 'JsonParser\|CssL4Parser\|BbnfBootstrap\|GoogleSheetsParser' crates/ir/src/ crates/analysis/src/` = **13** (verified this pass: strategy.rs 9 + ast_utils 2 + grammar_facts 1 + shape_dispatch/scalar 1). `strategy.rs:137-185` 9 grammar-named rows + `for_grammar_with_manifest`/`:216` consumer verified. |
| CH2-V4-05 | 2C | ACCEPT | `css_types.rs` Lock-14-(c) refutation is correct: the file lives in the GENERIC `crates/core/src/css_types.rs` (Lock 14 (c) admits only `crates/<grammar>/`), so it is the lock-named mess, routed to SK-V19 relocate-or-delete. Grammar-neutral finding. | `ls crates/core/src/css_types.rs` = 2373 B (verified this pass); generic-core location confirmed. |
| CH2-V4-06 | 2C | ACCEPT | The `css_balanced_component_scan` FORCED demotion is grounded grammar-neutrally: the byte-SKIP shell emits nothing, while the two offered non-CSS dischargers (JSON `parse_object_direct`/`parse_array_direct`, Sheets `paren_expr → expression`) are PARSE-with-emit descents, structurally incompatible. The CSS-scoped name + honest disclosure IS the discharge (no fabricated cross-grammar caller). The textbook CH2 outcome: a single-grammar-exercised primitive honestly scoped, not falsely neutral. The two-fan composition inherits the same obligation. | W3C CSS Syntax "consume a component value" is a real algorithm (cited `2C:215,374`). Sheets `paren_expr` cyclic descent into `expression` cited at `google-sheets.bbnf:137,163`. |
| CH2-V4-07 | 2C | ACCEPT | The Sheets precedence-tower negative control is the correct CH2 generality stress and the citation VERIFIES from primary source: Pratt "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931 (RE-VERIFIED via ACM this pass). The tower (`comparison→concat→add→mul→exp→unary→postfix→primary`) is the SOLE Sheets-distinctive shape JSON+CSS structurally lack; lowers to existing `SinkOnlyExpr` vocabulary (no new IR primitive); the `Nu8u8` 295×/21× demotion correctly removes a SHARED construct from the litmus. | DOI resolves at the cited ACM record (`dl.acm.org/doi/10.1145/512927.512931`, re-verified). |
| CH2-V4-08 | 2C/2D | ACCEPT | The 5-shape `BackendShape` is the genuinely grammar-NEUTRAL dispatch backbone, and the relocated-seam firewall (`emit_shape_source==lowered_program` + `runtime_target_rows_collapsed` co-gate; md5-distinct = necessary-not-sufficient) is the correct neutrality falsifier. The iburg/egg/Mison/OR-Tools cites all VERIFY from primary source. No sixth shape; no JSON-only overfit. The CSS-typed second-seam firewall (`css_provider_source==generated`) extends the neutrality gate correctly. | `lower/mod.rs:18-24` = exactly 5 shapes dispatched on `cost.chosen` (verified this pass, full body read). iburg LOPLAS 1992 DOI 10.1145/151640.151642 (RE-VERIFIED via ACM/Arizona/dblp); Mison PVLDB 2017 DOI 10.14778/3115404.3115416 (RE-VERIFIED via ACM/Microsoft Research, author list exact). |
| CH2-V4-09 | 2C | ACCEPT | The single-file Sheets import-closure onboarding (a derived DATA flag in `RuntimeFrontendRequirements`, NOT a `match grammar` arm) and the 9-grammar fleet onboarding test are correctly scoped: SK-V18 witnesses the un-fork on 3 (JSON+CSS+Sheets); with one negative-control witnessed the claim is SCOPED to the witnessed grammars (LAC-2C-SK18-02). Fleet-wide wording deferred to SK-V19. Correct GENERALITY discipline. | Roster verified: `crates/core/src/grammar/generated/` = 9 grammars; `tranche_scope=SK-V18-witnessed-3 / SK-V19-receiver` inline on the fleet row (`2C:218`). |
| CH2-V4-10 | 2D | ACCEPT | The cost-model generality is held grammar-NEUTRAL and the V2 CSP grammar-named/tautology critique is correctly routed as UNKNOWN-2D-V3-01 (P5/P4 hardening), NOT asserted as a live shape defect — and the independent check shows the live `decision_csp::finalize_rule` carries ZERO grammar names at HEAD, so 2D is if anything slightly conservative. The 5-shape dispatch is grammar-derived (`cost.chosen`), never a grammar tag. egg POPL 2021 + OR-Tools CP-SAT cites real. | `decision_csp.rs` (273 LOC) read this pass: NO `json/css/sheets/JsonParser/CssL4/GoogleSheets/grammar_ident/match grammar` (the only `csp_named_grammars`-adjacent hits are in `bbnf-bench/src/report.rs`, unrelated bench labels). The CSP finalizer is grammar-neutral live. |
| CH2-V4-11 | 2E | ACCEPT | x86/AVX-512 esoterica held grammar-neutral SECONDARY (never an M5 Max close route); aarch64-ONLY standing grounded; the eq-set/two-fan kernels carry the byte-set as caller data (Lock 14). The SVE2-absence refutation of NEON-svmatch verifies on the host, and the Lemire-2026 ARM-match post — the single most generality-load-bearing new citation (deployable NEON eq-fan vs the SVE2-MATCH the host lacks) — VERIFIES at the exact cited URL. The SHRN-vs-shift-add movemask-divergence is a real intra-crate KISS/DRY neutrality-adjacent finding; the Kutenin movemask citation verifies. No JSON-only-grounded-but-fleet-used defect. | Lemire 2026-04-19 post EXISTS at `lemire.me/blog/2026/04/19/...` (RE-VERIFIED this pass; SVE2 `match` fastest, NEON eq-fan deployable). Kutenin "Bit twiddling with Arm Neon: beating SSE movemasks…" Arm Community blog (VERIFIED this pass). Host probe `FEAT_SVE2 ABSENT` corroborates the NEON-svmatch refutation. |
| CH2-V4-12 | 2F | ACCEPT | The eq-set member scan is grounded STRUCTURALLY neutral with the shell honestly CSS-scoped, the `find_css_significant` wire-as-is refutation is correct (flat skip vs recursive shell), and the SHUFTI/TRUFFLE abstract-name provenance is disclosed with citation precision: the names come from the Hyperscan CODEBASE (`src/nfa/shufti.c`) + Langdale's branchfree writing, NOT the NSDI 2019 paper text — and the Hyperscan author list cited (Wang/Hong/Chang/Park/Langdale/Hu/Zhu) is EXACT. PTG-2F-10 now states "Its ONLY live production consumer is CSS … no live JSON consumer". The Pattern-H/RegexHir/float rows carry no JSON-only-fleet-used defect. The upstream-vs-vendor provenance reconcile is fenced with the (a)-(b) byte-set-as-caller-data falsifier (PTG-2F-09 GENERALITY GATE) so a vendored shell that hardcodes the alphabet is caught regardless of provenance. | Hyperscan NSDI 2019 author list VERIFIED EXACT this pass (Wang, Hong, Chang, Park, Langdale, Hu, Zhu, pp.631-648); SHUFTI/TRUFFLE-not-in-paper-text claim corroborated. `find_css_significant` flat-skip vs recursive `find_component_delim`/`consume_balanced_at` verified (`runtime_simd.rs:169-204`; sole `#[cfg(test)]` caller). |

Enumerated: **12 CH2-relevant groundings/refutations — 12 ACCEPT, 0 REVISE, 0 REJECT.**

## Spot-Verified Citations (CH2 most load-bearing — all re-verified from primary source this pass)

- **Pratt, "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931** — VERIFIED (resolves at the cited ACM record). 2C Sheets negative-control grounding is real and grammar-neutral.
- **Fraser/Hanson/Proebsting, "Engineering a Simple, Efficient Code-Generator Generator", LOPLAS 1992, Vol 1(3) pp.213-226, DOI 10.1145/151640.151642** — VERIFIED (ACM/Arizona/dblp). The "dispatch emission on the cost-SELECTED pattern, never a source tag" claim is faithful to iburg. Anchors the 5-shape neutral-dispatch backbone.
- **Li et al., "Mison: A Fast JSON Parser for Data Analytics", PVLDB 10(10) 2017, DOI 10.14778/3115404.3115416** — VERIFIED (ACM/Microsoft Research; author list exact). The "consumer-known speculative projection" SinkOnly grounding is faithful.
- **Lemire, "The fastest way to match characters on ARM processors?", 2026-04-19** — VERIFIED EXISTS at the exact cited URL; SVE2 `match` named fastest, NEON `vceqq_u8` eq-fan the deployable route on the SVE2-absent host. Single most generality-load-bearing new SK-V18 citation; corroborated by the M5 Max `FEAT_SVE2 ABSENT` host probe.
- **Wang/Hong/Chang/Park/Langdale/Hu/Zhu, "Hyperscan: A Fast Multi-pattern Regex Matcher for Modern CPUs", NSDI 2019, pp.631-648** — VERIFIED EXACT author list; the dossier's SHUFTI/TRUFFLE-name-from-codebase-not-paper-text provenance disclosure is honest, precise citation discipline (not confabulation).
- **Kutenin, "Bit twiddling with Arm Neon: beating SSE movemasks, counting bits and more", Arm Community blog** — VERIFIED; the `shrn` movemask claim is faithful.
- **eq-set kernel JSON consumer (2B/2C/2F)** — RE-FALSIFIED this pass and correctly disclosed: `byte_class_from_eq_set_64` / `find_ascii_set_member64` have NO live production JSON (or non-CSS) consumer; JSON rides `classify_tbl4`; the eq-set kernel's only live production consumer is CSS `count_top_level_commas`.

## Evidence Inspected

- All six target dossiers (2A 281L, 2B 436L, 2C 377L, 2D 144L, 2E 251L, 2F 196L) — the
  generality-relevant rows in full; 2B/2C extension and historical sections read in entirety.
- Prior SK-V18 CH2 outputs: `hardening/V1/CH2.md` (8/4/1) and `hardening/V3/CH2.md` (11/1/0).
- T-P2-DISPATCH-CONTEXT.md, V3 CHALLENGE-CONTEXT.md.
- On-disk verification (HEAD, both roots — `crates/...` = repo root, `skinny/crates/...` = skinny):
  - Lock-14 self-gate grep `crates/ir/src/ crates/analysis/src/` = **13** (asserts ZERO → RED).
  - Pattern-H census `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` = **71**.
  - `crates/ir/src/registry/strategy.rs:137-185` 9-ident table + `for_grammar(...)`→`for_grammar_with_manifest(.., PRODUCTION_MANIFEST_TABLE)` at `:216`.
  - `crates/core/src/css_types.rs` (2373 B, generic core).
  - `crates/codegen/src/lower/mod.rs:18-24` exactly-5-shape `select_lowering(cost.chosen)` canon (full body).
  - `skinny/crates/runtime/src` eq-set callers: `find_ascii_set_member64` = ZERO; `byte_class_from_eq_set_64` live at `runtime_simd.rs:44,56,199` (CSS), `find_css_significant` `:169-204` (test-only `lib.rs:574`).
  - JSON scan path `json/scan.rs:22-30` (`scan_structurals`→`neon::scan`/`scan_structurals_scalar`), `:214,219,228` (`classify_tbl4`); `rg byte_class_from_eq_set_64 grammars/json/` = 0.
  - `runtime_simd.rs:6-7` inaccurate JSON-rides source comment (flagged as G6 source-fix, not evidence).
  - `decision_csp.rs` (273 LOC) — ZERO grammar names at HEAD (CSP finalizer grammar-neutral).
  - `find_css_significant` signature `fixed:&[u8;9]`, two-fan `set_a`(8)+`set_b`(≤5) OR-reduce (verified).
- Citation re-verification (WebSearch, primary source this pass): Pratt POPL 1973, iburg LOPLAS 1992,
  Mison PVLDB 2017, Lemire 2026-04-19 ARM-match, Hyperscan NSDI 2019 (author list exact), Kutenin Arm-NEON movemask — all REAL.

## Fold Requirements

None. Disposition is ACCEPT; no REVISE or REJECT folds are required.

## Convergence Impact

CH2 is **ACCEPT** and does NOT block T-P2 V4 convergence. The lone V3 REVISE
(CH2-V3-01) landed cleanly: 2B's two retained SK-V15 V2 rows now carry the
supersession annotation, mirroring the 2C Pattern-H treatment. Independent
re-grounding against HEAD confirms every prior fold is resolved and surfaced no
new generality leak: every primitive/technique is grounded grammar-NEUTRALLY
(structural neutrality for the eq-set kernel, honest CSS-scoping for the
balanced-scan shell and the two-fan composition, real fleet-stress via the Sheets
precedence tower, neutral 5-shape cost-derived dispatch, grammar-neutral live CSP
finalizer); the Lock-14 relocated-seam leaks (13-site self-gate falsification,
9-ident table, css_types.rs) are correctly refuted and SK-V19-scoped; the
JSON+CSS-only SOTA scope is honored fleet-wide with Sheets a generality (not SOTA)
proof; and every load-bearing citation (Pratt 1973, iburg 1992, Mison 2017,
Lemire 2026, Hyperscan 2019, Kutenin) re-verifies as real from primary source,
with zero confabulation. The cycle-V1 ≥30%-REVISE expectation is a first-cycle
heuristic; three fold cycles have since converged the generality surface, and a
forced REVISE would falsify the record.

TALLY accept=12 revise=0 reject=0
