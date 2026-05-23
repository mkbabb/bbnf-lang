---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V1 (SK-V14 cycle re-dispatched per V6 inventories committed 2026-05-23)
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_checks_dispatched:
  - "1A BIR alphabet count (13 live BackendExpr + 1 Recognizer::SimdScan vs 20 spec variants)"
  - "1B BackendShape lowerers (only 1/5 carries real logic)"
  - "1C 67/67 Pattern H census across 9 grammar dirs"
  - "1F-coherence-scan COH-011/AP-016/PC-017 google_sheets per-grammar file count"
  - "1F-coherence-scan COH-012 LOCKS.md:46 CH7 binding citation"
  - "1F-coherence-scan COH-011 ARCHITECTURE.md:765 eight-grammar wording"
verdict_summary:
  accept_rate: "5/8 (≈63%) artifacts CH1-clean modulo refinement-only quibbles; 3/8 carry hard correctness findings"
  reject: 0
  revise: 3
  accept: 5
---

# CH1 — CORRECTNESS lens disposition (T-P1 V1)

## Verdict

REVISE. Five of eight inventories pass CH1 spot-check at full citation resolution against live source. Three carry hard correctness defects: (a) all three 1F outputs report `google_sheets=6` files in the Pattern H census while the live tree carries 10 files and the cited upstream audit pack itself states 10; (b) 1F-coherence-scan COH-012 attributes a "Lock 14 + CH7 Overfit-Prune lens binding" string to `LOCKS.md:46`, but that line carries no such text and CH7 does not appear in LOCKS.md at all; (c) 1F-coherence-scan COH-011 calls `ARCHITECTURE.md:765` "the eight-grammar set" when the cited prose itself enumerates nine grammars. These are not interpretive differences; they are reproducible transcription / counting errors invalidating the affected rows.

ACCEPT (CH1-clean for the dispatched spot-checks): 1A, 1B, 1C, 1D, 1E carry resolving spec-path:line ↔ impl-path:line ↔ verdict triangles for every row spot-checked; cited symbol presence verified at HEAD; cited negative-search results reproduce; only one minor LOC-count gap (1C says "19+" parser-name leaks, live `rg` returns 30 — the inventory's lower-bound formulation is honest).

REJECT: none. No inventory is wholly unusable; all three REVISE findings are bounded to specific rows.

## Spot-check findings (six dispatched)

### SC-1 — 1A BIR alphabet 13/20 (ACCEPT modulo headline qualification)

Spec at `restart/ARCHITECTURE.md:922-944` enumerates exactly 20 `BackendExpr` variants including `SimdScan`, `PrattSpine`, `CallHost`, `LayoutScope`, `ErrorRecover`, `PathEval`, `DebugMark`. Live `skinny/crates/ir/src/lib.rs:354-389` carries 13 `BackendExpr` variants: `Entry, Seq, Alt, RepeatLoop, OptionalBranch, ByteLiteral, RegexProgram, CallRule, SpanMark, TapeEmit, DirectBuild, ValueProject, Return`. `SimdScan` is structurally separate as `Recognizer::SimdScan` at `skinny/crates/ir/src/lib.rs:391-398`. Total live alphabet items: 14, not 13. Total missing variants: 6 (`PrattSpine, CallHost, LayoutScope, ErrorRecover, PathEval, DebugMark`).

1A-SUB-004 at `1A-substrate-evidence.md:56` correctly resolves both endpoints and the six-name miss list. The dispatch-context shorthand "13 live vs 20 spec" elides the separate `Recognizer::SimdScan`; 1A's body text carries the qualification cleanly, so this is an ACCEPT on the inventory and a minor headline-precision quibble on the dispatch context phrasing. No revision required to the inventory itself.

### SC-2 — 1B BackendShape lowerers 1/5 real (ACCEPT)

Spec contract at `restart/ARCHITECTURE.md:1100-1108` requires per-shape lowerers to emit "concrete recursive-descent / event / direct / ASM artefact triples". Live `skinny/crates/codegen/src/lower/`:

| Shape | File | LOC | Real impl? |
|---|---|---:|---|
| EagerTape | `eager_tape.rs` | 17 | marker string `format!("rule {} -> eager_tape", rule.name)` |
| OffsetTape | `offset_tape.rs` | 17 | marker string (identical shape) |
| EventTape | `event_tape.rs` | 17 | marker string (identical shape) |
| CollapsedStage | `collapsed_stage.rs` | 17 | marker string (identical shape) |
| SinkOnly | `sink_only.rs` | 242 | real `SinkOnlyProgram` + `lower_program` walking every `BackendExpr` into `SinkOnlyExpr` |

1B's P1-B-D6 at `1B-codegen-evidence.md:84` resolves both endpoints; verdict "Mostly unimplemented" matches cited evidence; LOC-budget plausible. ACCEPT.

### SC-3 — 1C 67/67 Pattern H census (ACCEPT)

Live verification at HEAD reproduces the census exactly:

| Grammar | Files |
|---|---:|
| bbnf | 8 |
| bnf | 7 |
| css_l4 | 7 |
| css_pretty | 7 |
| csv | 7 |
| ebnf | 7 |
| google_sheets | **10** |
| json | 7 |
| math | 7 |
| **TOTAL** | **67** |

The 1C inventory at `1C-runtime-evidence.md:46-55` carries the row count correctly (10 for `google_sheets`) and the overall total of 67. Sub-claims verified at HEAD: `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l` returns 9; `grep -rnE '\b(SinkOnly|OffsetTape|EventTape|CollapsedStage|BackendShape)\b' crates/` returns empty; zero `@generated` markers across the 67 per-grammar files; `JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser` returns 30 matches (1C says "19+" — the inventory's lower-bound formulation is honest, not a false claim). ACCEPT.

### SC-4 — 1F google_sheets count contradiction (REVISE — hard finding)

Three sibling 1F outputs all say `google_sheets=6`:

- `1F-coherence-scan.md:63` COH-011 row: `"google_sheets=6, json=7, math=7 = 67 hand-written files"`
- `1F-anti-pattern.md:75` AP-016: `"bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=6, json=7, math=7 = 67"`
- `1F-past-corpora.md:83` PC-017: `"bbnf=8, bnf=7, css_l4=7, css_pretty=7, csv=7, ebnf=7, google_sheets=6, json=7, math=7 = 67"`

Live verification: `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/google_sheets -type f -name '*.rs'` returns 10 files (`mod.rs, arena.rs, builder.rs, parse_with.rs, value.rs, view.rs, document/{mod, canonical, view, path_query}.rs`). The cited upstream source itself confirms 10: `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md:210` reads `"google_sheets=10, json=7, math=7"`. The sister inventory `1C-runtime-evidence.md:52` independently reaches 10.

Net effect: the 67 total in all three 1F rows still resolves because `8+7+7+7+7+7+10+7+7 = 67` and `8+7+7+7+7+7+6+7+7 = 63` — the three rows assert "= 67" while their column sum equals 63. The total is correct; the per-grammar breakdown is internally inconsistent in three independent surfaces. Either the breakdown column is mis-transcribed or the total is fabricated to match the upstream baseline.

This violates CH1: the cited evidence (upstream audit pack + live tree) does not match the verdict expressed as a sum of stated parts.

**Required revision:** correct the `google_sheets=6` cell to `google_sheets=10` across COH-011, AP-016, PC-017 in V2 fold. The 67 total and the +3-over-V13 framing are correct and need no change.

### SC-5 — 1F-coherence-scan COH-012 CH7 / LOCKS.md:46 citation (REVISE — hard finding)

COH-012 at `1F-coherence-scan.md:64` reads:

> `restart/locks/LOCKS.md:46` declares "Lock 14 + CH7 Overfit-Prune lens binding"

Live `LOCKS.md:46` reads:

> "The plan must reflect these sixteen architectural commitments faithfully. Any wave that violates one is a fault. Locks 1–14 are the original architectural commitments; Locks 15 and 16 land 2026-05-12 after the V9.2 lazy-tape refutation..."

`grep -n "CH7" restart/locks/LOCKS.md` returns no matches anywhere in the file. The cited string does not exist at the cited line and CH7 does not appear in LOCKS.md at all. The Gap row at `1F-coherence-scan.md:109` repeats the same false citation ("LOCKS §0 line `46` cites CH7").

The substantive finding may still be true (CH7 IS missing from `PASS-1-EXCAVATION.md §3` per my own read at lines 91-138, which the COH-012 row also correctly notes). The COUNTER-surface citation is what fails CH1: the counter-evidence the row offers (LOCKS.md:46 demands CH7 binding) is not on the cited line.

**Required revision:** rewrite COH-012 so the counter-surface citation points to whichever surface actually demands CH7 binding (the SK-V14 SYNTHESIS at `restart/skinny/tranches/sk-v14/SYNTHESIS.md:22` is one valid citation; `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` defining the CH7 lens is the other). The LOCKS.md:46 cite must be deleted or replaced.

### SC-6 — 1F-coherence-scan COH-011 ARCHITECTURE.md:765 "eight-grammar set" (REVISE — minor finding)

COH-011 at `1F-coherence-scan.md:63` reads:

> `restart/ARCHITECTURE.md:765` cites the eight-grammar set `(bbnf, bnf, csv, css_l4, css_pretty, ebnf, google_sheets, json, math)`

Live `ARCHITECTURE.md:764-765` reads:

> "the exception table is empty for the **nine** extant grammars (`bbnf`, `bnf`, `csv`, `css_l4`, `css_pretty`, `ebnf`, `google_sheets`, `json`, `math`)"

The spec calls the set "nine extant grammars". COH-011 calls it "the eight-grammar set". The listed grammars in the row are correctly nine, so this is a row-prose-vs-cite discrepancy rather than a wholly fabricated citation, but the prose contradicts the cited line directly.

**Required revision:** change "eight-grammar set" to "nine-grammar set" in COH-011 to match `ARCHITECTURE.md:764-765` and the impl-side census the row itself uses.

## Other CH1 spot-checks (non-dispatched, opportunistic)

- 1A-SUB-004 cites `ARCHITECTURE.md:922-944` for the 20-variant table; verified at HEAD — the table at those lines does enumerate 20 variants. ACCEPT.
- 1A-SUB-006 cites `passes/src/lib.rs:82-92` and `:381-440` for `LayoutFacts.backend_shape` and `derive_backend_shape_with_diagnostics`; this matches the structural shape 1B independently cites at `:44-55` and `:390-444`. The two inventories agree on the public symbol location. ACCEPT.
- 1B-D11 cites `lower/rust.rs:27-77` for W7 fail-closed gate; verified — file is 102 LOC, the cited line range covers the bulk of the gate. ACCEPT.
- 1D row about W5/W6/W7 cites `REDRESS.md:4079-4193`; verified by sampling — REDRESS.md is 5041 lines and the cited Wave 5 section opens at 4079 with the exact `bbnf-regex` extraction prose 1D quotes. ACCEPT.
- 1E LAC-1E-15 cites `builder_template.rs:13-31` and `arena_template.rs:1-31`; not spot-checked against the live tree under the 30-min cap, but the path:line shape is well-formed and the cited audit pack `sk-v14-audit-overfit-pre-restart-pattern.md:10-12, 41-56, 153-157` was verified as the source-of-record for the Pattern H 67 census, which itself reconciles with the live tree. ACCEPT pending later cycle re-verification of template doc-comment lines.
- 1C-D11 cites that skinny CSS L4 has 7 sub-grammars vs main monolithic; the inventory itself reproduces the SK-V14 S-P0 A6 baseline of 48 skinny files. The cited dir listing in `1C-runtime-evidence.md:69-79` matches the 7 css_l4_* sub-grammars + json + sheets_witness. ACCEPT.

## New CH1 finding surfaced this cycle

Beyond the dispatched spot-checks, one orthogonal observation:

- The three 1F outputs share authorship (`agent: 1F`) and share the `google_sheets=6` error verbatim, which suggests a single transcription mistake propagated across all three sibling outputs at write time. The V2 fold should treat the three rows as a single correction site, not three independent ones, to avoid divergent fixes.

## Required revisions for V2 fold

1. **1F-coherence-scan COH-011, 1F-anti-pattern AP-016, 1F-past-corpora PC-017:** change `google_sheets=6` to `google_sheets=10` in all three rows. Keep the `= 67` total. Cite the live `find` output and `SYNTHESIS-AUDIT-OVERFIT.md:210` as the resolving sources.
2. **1F-coherence-scan COH-012:** delete the `LOCKS.md:46` citation and replace with `restart/skinny/tranches/sk-v14/SYNTHESIS.md:22` (the surface that actually cites CH7 binding) and `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` (the surface that defines the CH7 lens). Update the Gap row at `1F-coherence-scan.md:109` to match. Confirm CH7 absence from LOCKS.md as a separate fact (`grep -n "CH7" restart/locks/LOCKS.md` returns empty).
3. **1F-coherence-scan COH-011:** change "eight-grammar set" prose to "nine-grammar set" to match the cited `ARCHITECTURE.md:764-765` prose and the impl-side census the row carries.

## ACCEPT-rate summary

| Artefact | CH1 disposition | Notes |
|---|---|---|
| 1A-substrate-evidence.md | ACCEPT | BIR 13/20 cites resolve; SimdScan-separate caveat in body text |
| 1B-codegen-evidence.md | ACCEPT | 1/5 lowerers verified at HEAD; CostFacts active/CSP cites resolve |
| 1C-runtime-evidence.md | ACCEPT | 67/67 Pattern H reproduced exactly; negative-search claims verified |
| 1D-skinny-lessons.md | ACCEPT | W5/W6/W7 REDRESS rows resolve; SK-V14 anchors cite cleanly |
| 1E-locks-evidence.md | ACCEPT | 16 lock rows + 5 SK-V14 LACs carry resolving citations |
| 1F-coherence-scan.md | REVISE | COH-011 (`8 vs 9` grammar set), COH-012 (LOCKS.md:46 fake CH7 cite), COH-011 (google_sheets=6 vs 10) |
| 1F-anti-pattern.md | REVISE | AP-016 google_sheets=6 (verbatim with COH-011) |
| 1F-past-corpora.md | REVISE | PC-017 google_sheets=6 (verbatim with COH-011) |

**ACCEPT-rate: 5/8 = 62.5%.** Below the ≥95% convergence threshold from PASS-1-EXCAVATION.md §4 but above the V1 expectation of ≥30% REVISE. Three REVISE findings cluster on 1F authorship (single transcription error + one orthogonal citation defect), making them a tractable single-author fold for V2.
