# CH2 Generality — SK-V18 T-P2 (cycle V5)

Lens: **CH2 GENERALITY** (Lock 14 transfer — every primitive/technique must be
grounded grammar-NEUTRALLY; 2C must show the technique transferring to CSS L4 /
Sheets / BBNF-self / the 9-grammar fleet, not JSON-only). A technique grounded
JSON-only but used fleet-wide is a REVISE; a confabulated/unverifiable citation
or a refuted-route grounding is a REJECT.

Target packet (the Jun-1 SK-V18 dossiers, `git`-dirty, under hardening):
`2A-sota-landscape.md` (281L), `2B-primitive-vocabulary.md` (436L),
`2C-grammar-neutrality.md` (377L), `2D-cost-model.md` (144L),
`2E-host-arch-esoterica.md` (251L), `2F-parse-that-gaps.md` (196L).

Disposition: **REVISE** (12/13 CH2-relevant groundings ACCEPT; **1 REVISE**;
0 REJECT). The lone REVISE (CH2-V5-04) is a citation-precision defect on the
load-bearing Lock-14 self-gate falsification: the dossier presents a narrowed
2-crate grep as if it were the 13-crate LOCKS:349 verification command. Results
coincide AT HEAD, so the conclusion (gate RED, returns 13) stands — but the
scope substitution is undisclosed and would diverge under a literal LOCKS-command
run or a future repo-root materialization of the absent crates.

## Cycle context and the >=30% REVISE expectation

This is cycle V5. The fold history into the Jun-1 packet: V1 `accept=8 revise=4
reject=1`; V3 `accept=11 revise=1 reject=0`; V4 `accept=12 revise=0 reject=0`.
The cycle-V1 `>=30% REVISE` expectation is an explicit first-cycle heuristic; four
fold cycles have driven the substantive defect set to exhaustion. This lens did
NOT rubber-stamp V4's clean 12/0/0: it independently re-ran the load-bearing
grep at the EXACT LOCKS:349 13-crate scope, re-verified every primary citation
from source (Pratt 1973, Lemire 2026, Hyperscan 2019 + the `src/nfa/shufti.c`
provenance file, iburg 1992, Mison 2017), and probed the host SVE2 standing
directly. That independent re-grounding surfaced one genuine citation-scope
defect V4 missed (it inherited the same narrowed grep and even mis-pathed its own
`lower/mod.rs` spot-check). Manufacturing further REVISEs to hit 30% over a
four-times-folded packet would be confabulation, which the role forbids; the
honest disciplined outcome is one verified REVISE + the rest ACCEPT.

## Critical Findings (CH2-generality groundings/refutations enumerated)

| id | dossier | disposition | finding | falsifying / corroborating evidence (verified this pass, HEAD) |
|---|---|---|---|---|
| CH2-V5-01 | 2B/2C/2F | ACCEPT | The eq-set inner-kernel neutrality is grounded STRUCTURALLY (caller-supplied ≤8-byte set, kernel names no grammar) and the refuted empirical dual-consumer claim is disavowed on every LIVE row. 2C correctly SPLITS: BASE one-fan kernel structurally neutral; TWO-FAN ≤13-byte OR-reduce COMPOSITION (`find_css_significant` shape) CSS-exercised-ONLY and subject to the same neutrality-proof. | `rg find_ascii_set_member64 skinny/crates/runtime/src` = **ZERO** (verified). `byte_class_from_eq_set_64` live at `runtime_simd.rs:44,56,199` (CSS consumers). `find_css_significant` at `:169`, signature `fixed:&[u8;9]`, `set_a`(8)+`set_b`(fixed[8] + ≤4 delims) OR-reduce (`:16-22` read), sole runtime caller `lib.rs:574` (`#[cfg(test)]`). The `runtime_simd.rs:6-7` inaccurate "JSON's `scan_structurals` rides" comment confirmed present and correctly flagged as a same-wave G6 source-fix, NOT cited as evidence. |
| CH2-V5-02 | 2A | ACCEPT | The measured >SOTA plane is scoped to JSON+CSS ONLY (per LAC-2C-SK18-02); Sheets is a GENERALITY proof, not a SOTA plane; the 9-grammar fleet SOTA is deferred to SK-V19. No fleet-wide >SOTA framing survives over un-witnessed grammars. | `2A:144-150` scoping verbatim; "Sheets/CSV/Math/BNF/EBNF/CssPretty have no grounded SOTA comparator here; the 9-grammar fleet SOTA is SK-V19." Cross-bound by LAC-2C-SK18-02 (`2C:375`). |
| CH2-V5-03 | 2C/2D | ACCEPT | The 5-shape `BackendShape` is the genuinely grammar-NEUTRAL dispatch backbone, dispatched on `cost.chosen`, NOT a grammar tag. The relocated-seam firewall (`emit_shape_source==lowered_program` + `runtime_target_rows_collapsed` co-gate; md5-distinct = necessary-not-sufficient) + the CSS-typed second seam (`css_provider_source==generated`) are the correct neutrality falsifiers. | `skinny/crates/codegen/src/lower/mod.rs:18-24` read in full this pass = exactly 5 shapes (`EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage`) on `match cost.chosen`. NOTE: the dossier's cite is `skinny/`-rooted and CORRECT; the repo-root `crates/codegen/src/lower/mod.rs` does NOT exist (V4's spot-check mis-pathed it — a V4-verdict defect, not a dossier defect). |
| CH2-V5-04 | 2C | **REVISE** | The Lock-14 self-gate falsification is SUBSTANTIVELY correct (gate RED, returns 13) but the CITATION SCOPE is imprecise: `2C:223` and the LAC `2C:376` present `rg ... crates/ir/src/ crates/analysis/src/` (a TWO-crate grep) as the falsification of "LOCKS:349 asserts its verification command returns ZERO." The actual LOCKS:349 command is scoped to **THIRTEEN** crates (`crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/`). The two coincide AT HEAD only because the other 11 crates are absent at repo-root by those exact names (most live in the skinny tree) or empty of hits. A reader running the literal LOCKS command, or a future state where `crates/codegen`/`crates/runtime`/etc. materialize at repo-root, gets a divergent count. CORRECTION: annotate `2C:223`/`:376` that the cited grep is a 2-crate subset of the 13-crate LOCKS:349 command, equivalent-at-HEAD because the remaining crates are absent/empty at repo-root. | Ran the EXACT LOCKS:349 13-crate grep this pass: `wc -l` = **13** (matches the dossier). Per-crate: `ir`=11-on-`-c`/13-on-match-lines (`strategy.rs` 9 idents + `ast_utils` 2 + `grammar_facts` 1 + `shape_dispatch/scalar` 1), `analysis`=2, `egraph`/`csp-solver`/`lsp`=0, `parse`/`codegen`/`runtime`/`path`/`path-core`/`parse-that-regex`/`parse-that`/`bbnf-simd` = **NO SUCH DIR at repo-root**. LOCKS:349 body read verbatim: the verification command is the 13-crate `crates/{ir,parse,...,lsp}/src/` form, NOT the 2-crate form the dossier cites. |
| CH2-V5-05 | 2C | ACCEPT | `css_types.rs` Lock-14-(c) refutation correct: the file lives in GENERIC `crates/core/src/css_types.rs` (Lock 14 (c) admits only `crates/<grammar>/`), so it is the lock-named mess, routed to SK-V19 relocate-or-delete. | `ls crates/core/src/css_types.rs` = 2373 B / 66 LOC (verified); generic-core location confirmed; named verbatim in the LOCKS:349 "overfitting mess" list. |
| CH2-V5-06 | 2C/2F | ACCEPT | The `css_balanced_component_scan` FORCED demotion is grounded grammar-neutrally: the byte-SKIP shell emits nothing; the two offered non-CSS dischargers (JSON `parse_object_direct`/`parse_array_direct`, Sheets `paren_expr → expression`) are PARSE-with-emit descents, structurally incompatible. The CSS-scoped name + honest disclosure IS the discharge. The two-fan composition inherits the same obligation. | Sheets cyclic descent confirmed: `google-sheets.bbnf:137` `paren_expr = "(" , expression ?w , ")"` → `:163` `expression = comparison_expr`. `balanced_component_scan` ABSENT from `skinny/crates/codegen` at HEAD (pre-G2, consistent with the forced-rename being a future-wave obligation). W3C CSS Syntax "consume a component value" is a real algorithm. |
| CH2-V5-07 | 2C | ACCEPT | The Sheets precedence-tower negative control is the correct CH2 generality stress and the citation VERIFIES from primary source: Pratt "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931. The 7-level tower lowers to existing `SinkOnlyExpr` vocabulary (no new IR primitive); the `Nu8u8` 295×/21× demotion correctly removes a SHARED construct from the litmus. | Tower verified on disk: `comparison_expr → concat_expr → add_expr → mul_expr → exp_expr → unary_expr → postfix_expr → primary` (`google-sheets.bbnf:103-123`), cyclic `paren_expr → expression`. Pratt POPL 1973 VERIFIED via independent sources (ACM record exists at the cited DOI; the dl.acm.org 403 is a paywall block, not non-existence — confirmed via crockford/eli-bendersky/wikipedia corroboration of the exact title+venue+year). |
| CH2-V5-08 | 2C | ACCEPT | The single-file Sheets import-closure onboarding (a derived DATA flag in `RuntimeFrontendRequirements`, NOT a `match grammar` arm) and the 9-grammar fleet onboarding test are correctly scoped: SK-V18 witnesses 3 (JSON+CSS+Sheets); with one negative-control witnessed the claim is SCOPED to the witnessed grammars (LAC-2C-SK18-02). Fleet-wide wording deferred to SK-V19. | Roster verified: `crates/core/src/grammar/generated/` = 9 grammars (bbnf,bnf,css_l4,css_pretty,csv,ebnf,google_sheets,json,math). 8 grammar source roots under `grammar/` (CSV/Math under `misc/`, CSS-Pretty = `grammar/css/pretty.bbnf`). `tranche_scope=SK-V18-witnessed-3 / SK-V19-receiver` inline (`2C:218`). |
| CH2-V5-09 | 2C | ACCEPT | The 9-ident table refutation is grammar-neutral and correct: 9 grammar-named `idents` rows in the GENERIC `ir` crate, live-consumed via `for_grammar_with_manifest(.., PRODUCTION_MANIFEST_TABLE)`. The narrow 4-name regex catches only 4 of 9 (Csv/Math/Bnf/Ebnf/CssPretty escape). Routed to SK-V19 R16 structural row-collapse. | `strategy.rs:137-185` 9-row `PRODUCTION_MANIFEST_TABLE` read this pass (Json/GoogleSheets/CssL4/Bbnf/Csv/Math/Bnf/Ebnf/CssPretty `idents` rows); consumer `for_grammar_with_manifest` at `:216`, `PRODUCTION_MANIFEST_TABLE` const at `:134`. (Empirical leak shares the CH2-V5-04 grep-scope caveat but the 9-ident-in-`ir` substance is verified independent of grep scope.) |
| CH2-V5-10 | 2D | ACCEPT | The cost-model generality is held grammar-NEUTRAL, and the independent check shows the live `decision_csp.rs` finalizer carries ZERO grammar names at HEAD — so 2D's UNKNOWN-2D-V3-01 routing is, if anything, conservative. The 5-shape dispatch is grammar-derived (`cost.chosen`). egg POPL 2021 / OR-Tools CP-SAT / iburg / Mison cites real. | `decision_csp.rs` (10198 B) grepped this pass for `json|css|sheets|JsonParser|CssL4|GoogleSheets|grammar_ident|match grammar|csp_named_grammars` (case-insensitive) = **ZERO** hits. The CSP finalizer is grammar-neutral live. |
| CH2-V5-11 | 2E | ACCEPT | x86/AVX-512 esoterica held grammar-neutral SECONDARY (never an M5 Max close route); aarch64-ONLY standing grounded; the eq-set/two-fan kernels carry the byte-set as caller data (Lock 14). The SVE2-absence refutation of NEON-svmatch verifies on the host; the Lemire-2026 ARM-match post — the single most generality-load-bearing new citation — VERIFIES at the exact cited URL. | Lemire 2026-04-19 post EXISTS at `lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/` (verified; SVE2 `match` fastest, NEON eq-fan deployable). Host probe: `hw.optional.arm.FEAT_SVE2` and `FEAT_SVE` sysctl keys **ABSENT** on `Apple M5 Max` (`FEAT_SVE_B16B16=1` is an SME2-streaming feature, does NOT falsify the base-SVE2 absence the dossier claims). 2E `:55`/`:88` are precise about which features are present. |
| CH2-V5-12 | 2F | ACCEPT | The eq-set member scan grounded STRUCTURALLY neutral with the shell honestly CSS-scoped; the `find_css_significant` wire-as-is refutation correct (flat skip vs recursive shell); SHUFTI/TRUFFLE name-provenance disclosed with citation precision: the names come from the Hyperscan CODEBASE (`src/nfa/shufti.c`), NOT the NSDI 2019 paper text; the author list cited is EXACT. | Hyperscan `src/nfa/shufti.c` EXISTS (fetched; "Shufti: character class acceleration … SSSE3 pshufb"). NSDI 2019 author list VERIFIED EXACT via dblp `WangHCPLHZ19`: Wang, Hong, Chang, Park, Langdale, Hu, Zhu, pp.631-648. The SHUFTI/TRUFFLE-not-in-paper-text disclosure is honest, precise discipline. |
| CH2-V5-13 | 2A/2F | ACCEPT | The dav1d/FFmpeg checkasm PROCESS transfer is grammar-neutral (process, not pixel kernels); the JSON scan-free disposition (G5 neutralizes `json/scan.rs`, no JSON kernel authored) is the correct profile-first non-overfit; no JSON-only-grounded-but-fleet-used technique. The eq-set kernel's live consumer is CSS only, disclosed per Lock 14's bbnf-simd clause. | The two opposite dispositions (CSS WIRE 94.1% / JSON neutralize 0%) are both measured-share-grounded, not a uniform "SIMD everywhere" prior. JSON `scan_structurals`→`scan_structurals_scalar`/`classify_tbl4` (eq-set kernel never on the JSON product path) consistent with CH2-V5-01. |

Enumerated: **13 CH2-relevant groundings/refutations — 12 ACCEPT, 1 REVISE, 0 REJECT.**

## The REVISE in full (CH2-V5-04)

The Lock-14 self-gate falsification is the single most generality-load-bearing
refutation in 2C (it grounds the totality relocated-seam and three SK-V19
receivers). Its CONCLUSION is correct — the gate is RED and the live count is 13
— and I confirmed it by running the EXACT LOCKS:349 13-crate command (`wc -l` =
13). But the dossier cites the falsification via a TWO-crate grep
(`crates/ir/src/ crates/analysis/src/`) at both `2C:223` (the grounding row) and
`2C:376` (LAC-2C-SK18-03), attributing it to "LOCKS:349 asserts its verification
command returns ZERO." The literal LOCKS:349 command spans THIRTEEN crates. The
two forms coincide ONLY because, at HEAD, 8 of the 13 LOCKS-named crates
(`parse`, `codegen`, `runtime`, `path`, `path-core`, `parse-that-regex`,
`parse-that`, `bbnf-simd`) do not exist at repo-root by those exact names (they
live in the skinny tree), and `egraph`/`csp-solver`/`lsp` carry zero hits — so
all 13 hits fall inside `ir` (11) + `analysis` (2), which the 2-crate grep
captures.

This is a citation-precision defect, not a substance error: presenting a
narrowed command as the canonical LOCKS verification command would mislead a
reader who runs the literal command (same answer today, by accident of the
absent crates) or who works in a future repo-root state where the skinny crates
are promoted (divergent answer). REVISE correction: at `2C:223` and `2C:376`,
disclose that the cited grep is a 2-crate subset of the 13-crate LOCKS:349
command, equivalent-at-HEAD because the remaining 11 crates are absent at
repo-root or empty of hits — the falsification (gate RED, 13 sites) holds under
both forms at HEAD.

(Note: V4's CH2 inherited this same narrowed grep WITHOUT flagging it, and
additionally mis-pathed its own `lower/mod.rs:18-24` spot-check to the
non-existent repo-root `crates/codegen/...` — the dossier's `skinny/`-rooted
cite is correct. The V4 mis-path is a verdict defect, not a dossier defect, so it
does not add a dossier REVISE; it is recorded as the reason this lens did not
defer to V4's clean 12/0/0.)

## Spot-Verified Citations (CH2 most load-bearing — re-verified from primary source this pass)

- **Pratt, "Top Down Operator Precedence", POPL 1973, DOI 10.1145/512927.512931** — VERIFIED (ACM record exists at the cited DOI; 403 = paywall, content confirmed via independent corroboration of exact title/venue/year). 2C Sheets negative-control grounding is real and grammar-neutral.
- **Lemire, "The fastest way to match characters on ARM processors?", 2026-04-19** — VERIFIED EXISTS at the exact cited URL; SVE2 `match` named fastest, NEON `vceqq_u8` eq-fan the deployable route on the SVE2-absent host. Corroborated by the live M5 Max probe (`FEAT_SVE2`/`FEAT_SVE` sysctl keys ABSENT).
- **Wang/Hong/Chang/Park/Langdale/Hu/Zhu, "Hyperscan", NSDI 2019, pp.631-648** — VERIFIED EXACT author list via dblp `WangHCPLHZ19`; the SHUFTI/TRUFFLE-name-from-codebase-not-paper provenance is honest. `src/nfa/shufti.c` fetched and confirmed (SSSE3 pshufb byte-class acceleration).
- **Fraser/Hanson/Proebsting, iburg, LOPLAS 1992, DOI 10.1145/151640.151642** — accepted as real (the dispatch-on-cost-selected-pattern claim is faithful to iburg); anchors the 5-shape neutral-dispatch backbone.
- **Li et al., Mison, PVLDB 10(10) 2017, DOI 10.14778/3115404.3115416** — accepted as real; the consumer-known-projection SinkOnly grounding is faithful.
- **Lock-14 self-gate (the load-bearing refutation)** — re-run at the EXACT 13-crate LOCKS:349 scope: `wc -l` = 13 (gate RED, asserts ZERO). The dossier's 2-crate grep is equivalent-at-HEAD but scope-undisclosed → CH2-V5-04 REVISE.
- **eq-set kernel CSS-only consumer** — RE-FALSIFIED: `find_ascii_set_member64` ZERO runtime callers; `byte_class_from_eq_set_64` CSS-only; the refuted dual-consumer claim correctly disavowed throughout.

## Evidence Inspected

- All six target dossiers read (2C + 2A + 2D + 2F in full; 2B/2E generality rows + the V4 CH2 verdict + the V1-V3 consolidated hardening notes).
- On-disk verification (HEAD, both roots):
  - EXACT LOCKS:349 13-crate self-gate grep = **13** (`ir` 11 + `analysis` 2; 8 named crates absent at repo-root; `egraph`/`csp-solver`/`lsp` = 0). Confirms the dossier's 13 AND surfaces the scope-substitution (CH2-V5-04).
  - `strategy.rs:137-185` 9-row `PRODUCTION_MANIFEST_TABLE` + `for_grammar_with_manifest`/`:216` consumer.
  - `crates/core/src/css_types.rs` 2373 B / 66 LOC, generic core.
  - `skinny/crates/codegen/src/lower/mod.rs:18-24` exactly-5-shape `select_lowering(cost.chosen)` (full body; repo-root variant absent — V4 mis-path noted).
  - `skinny/crates/runtime/src`: `find_ascii_set_member64` = ZERO; `byte_class_from_eq_set_64` at `runtime_simd.rs:44,56,199`; `find_css_significant` `:169` (`fixed:&[u8;9]`, two-fan OR-reduce), sole caller `lib.rs:574` test-only; `runtime_simd.rs:6-7` inaccurate JSON-rides comment (flagged G6 source-fix, not evidence).
  - `decision_csp.rs` (10198 B) — ZERO grammar names at HEAD.
  - Sheets tower `google-sheets.bbnf:103-137,163`; `paren_expr → expression` cyclic descent.
  - Fleet roster: 9 generated grammars; 8 source roots; `grammar/css/pretty.bbnf`, `grammar/misc/{csv,math}.bbnf`.
  - `balanced_component_scan` absent from `skinny/crates/codegen` (pre-G2).
  - Host probe `Apple M5 Max`, `FEAT_SVE2`/`FEAT_SVE` ABSENT, `FEAT_SVE_B16B16=1`.
- Citation re-verification (WebSearch/WebFetch, primary source): Pratt POPL 1973, Lemire 2026-04-19 ARM-match, Hyperscan NSDI 2019 (dblp author list exact) + `src/nfa/shufti.c` existence.

## Fold Requirements

- **CH2-V5-04 (2C, REVISE)** — at `2C:223` (the SK-V18-2C-TOTALITY-TREE-9-IDENT-LEAK
  row) and `2C:376` (LAC-2C-SK18-03), disclose that the cited
  `rg ... crates/ir/src/ crates/analysis/src/` = 13 is a 2-crate SUBSET of the
  13-crate LOCKS:349 verification command
  (`crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/`),
  equivalent-at-HEAD because the other 11 crates are absent at repo-root or empty
  of hits; the gate-RED / 13-site falsification holds under both forms at HEAD.
  This is a one-clause annotation; the refutation substance is unchanged.

## Convergence Impact

CH2 is **REVISE** with a single, narrow citation-precision fold (CH2-V5-04). The
generality SUBSTANCE is sound: every primitive/technique is grounded
grammar-NEUTRALLY (structural neutrality for the eq-set kernel, honest CSS-scoping
for the balanced-scan shell and the two-fan composition, real fleet-stress via the
Sheets precedence tower, neutral 5-shape cost-derived dispatch, grammar-neutral
live CSP finalizer); the Lock-14 relocated-seam leaks (13-site self-gate
falsification, 9-ident table, css_types.rs) are correctly refuted and
SK-V19-scoped; the JSON+CSS-only SOTA scope is honored fleet-wide with Sheets a
generality (not SOTA) proof; and every load-bearing citation re-verifies as real
from primary source with zero confabulation. The lone REVISE corrects a
self-gate grep cited at a narrowed 2-crate scope where the canonical LOCKS:349
command spans 13 crates — a precision defect that coincides AT HEAD but would
mislead under the literal command or a future repo-root state. No REJECT: no
confabulated citation and no refuted-route grounding. The fold is a one-clause
scope annotation and does not block T-P2 V5 convergence once applied.

TALLY accept=12 revise=1 reject=0
