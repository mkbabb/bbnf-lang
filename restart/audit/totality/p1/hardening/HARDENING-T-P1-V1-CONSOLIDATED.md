# SK-V14 T-P1 Totality Excavation — V1 CHALLENGE Consolidated

Aggregator: SK-V14 T-P1 V1 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V1 over the eight committed T-P1 inventory
artefacts (8 files at `restart/audit/totality/p1/`: 1A 1B 1C 1D 1E
1F-coherence-scan 1F-anti-pattern 1F-past-corpora — V6 inventory cycle
committed 2026-05-23 ahead of the V1 lens dispatch).
Authority: `restart/prompts/totality/PASS-1-EXCAVATION.md §3 + §5`
(CH1-CH6 specialisations and convergence rule); `restart/prompts/ORCHESTRATOR.md
§3W` (universal CH-lens registry) + `§3Z` (≥95 % × 2 consecutive cycles
+ zero orphan REVISEs); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7`
(Overfit-Prune lens binding from S-P0; LOCKS.md silent on CH7 per LAC-1E-12);
dispatch
`restart/audit/totality/p1/hardening/V1/CHALLENGE-CONTEXT.md` §0-§4.
Input ledger: seven V1 lens dispositions under
`restart/audit/totality/p1/hardening/V1/`
(`CH1.md` 161 lines, `CH2.md` 91, `CH3.md` 57, `CH4.md` 99, `CH5.md` 31,
`CH6.md` 65, `CH7.md` 247 — 751 lens lines + 38 CHALLENGE-CONTEXT lines).

This consolidator supersedes the SK-V13-era T-P1 V1 consolidator
previously occupying this path (which audited a different inventory
cycle prior to the SK-V14 V6 refresh).

## §0 — V1 cycle verdict

### §0.1 Per-lens dispositions (verbatim from each CH file's §verdict / §cycle)

| Lens | Definition | Sub-axes | ACCEPT | REVISE | REJECT | Per-lens ACCEPT-rate | Verdict |
|---|---|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | every row's spec path:line ↔ impl path:line ↔ verdict triangle resolves against HEAD; cited symbol presence verified; cited negative-search results reproduce | 8 inventories | 5 | 3 | 0 | **62.5 %** | REVISE (1F triplet google_sheets=6 transcription error + COH-012 LOCKS.md:46 fabricated cite + COH-011 eight-vs-nine grammar mis-prose) |
| CH2 GENERALITY | Lock 14 holds; no JSON-only divergence catalogued when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral; no grammar-name leak passes uncited | 10 rows (8 inv + 2 cross-cite) | 6 | 4 | 0 | **60 %** | REVISE (1C parser-leak undercount 19+ vs live 30; reexport undercount 60+ vs live 126; 1B pass-layer leaks not folded into 1D matrix; bbnf-simd PC-008 cross-cite gap; CSS L4 layout-asymmetry uncited) |
| CH3 REGRESSION (REDRESS) | no T-P1 inventory re-opens a REDRESS route already rejected, blocked, or admitted-then-falsified; 1D pre-block list + 1E LAC ledger correctly identify SK-V14 pre-blocks; no admitted REDRESS row mis-catalogued as unimplemented | 9 findings | 6 | 3 | 0 | **66.7 %** | REVISE (1D frontmatter divergence_count drift; dispatch "5 proved/11 disproved" crosses two semantic axes; admit-vs-reject conflation in falsified-row 7-11 typed range) |
| CH4 COST | every divergence carries LOC-delta + risk class under orchestrator six-field schema (loc_budget / risk / wave / hard_cap / same_wave_consumer / evidence_basis); 1E amendment candidates state wave-alignment hint | 8 artefacts | 8 | 0 | 0 | **100 %** | ACCEPT (16/16 LACs pass wave-alignment + path:line check; 1.2-1.4× hard-cap multiplier convention noted as T-P3 governance item) |
| CH5 HIDDEN COUPLING | no parallel substrate / sidecar / retained cursor / second source scan; substrate union holds; no renamed-scanner closure; 1F anti-pattern scan catches live couplings | 7 findings | 3 | 4 | 0 | **42.9 %** | REVISE (1A-SUB-014 renamed StructuralIndex scanner closure; 1F missing CSS source-sidecar coupling; Track 2 shared-substrate-helper caveat; proof-witness generic-runtime coupling under-named) |
| CH6 ANTI-PAPER-CLOSE | self-reports of "resolved/wired/honoured/proved/implemented pre-block" require live-evidence citation; every UNKNOWN carries verify_action; no divergence deferred to "a later inventory" | 19 row classifications | 13 | 6 | 0 | **68.4 %** | REVISE (1A-SUB-001/005/006/010 verdict-vs-note conflict; 1B :53 WASM-deferral vs VM-replay-UNKNOWN conflation; 1D :100 single-substrate-proved vs 1A-DIV-008 two-cursor tension; 1E :89 L16 sustained-UNKNOWN elision; 1F-anti AP-010 proof-gate verdict softness; 1F-past PC-001/002/004 historical pre-block live-scan absence) |
| CH7 OVERFIT-PRUNE | SK-V14 audit-corrected baseline propagation; no fake-pattern recurrence in inventory text; 1F coherence-scan correctly flags LOCKS.md:46 CH7-binding leak (per anti-fabrication phrasing); 1C 67-file Pattern H matches S-P0 A6 byte-for-byte | 9 inventory targets | 7 | 2 | 0 | **77.8 %** | REVISE (1F COH-012 fabricated LOCKS.md:46 CH7-binding cite; 1F triplet google_sheets=6 breakdown sums to 63 not 67) |

### §0.2 Aggregate ACCEPT-rate

Two aggregation methods (per `ORCHESTRATOR.md §3Z`):

- **Sub-axis-weighted (load-bearing for §3Z convergence):**
  (5+6+6+8+3+13+7) / (8+10+9+8+7+19+9) = **48 / 70 = 68.6 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (62.5 + 60 + 66.7 + 100 + 42.9 + 68.4 + 77.8) / 7 = **68.3 %**.

Both aggregates land **well below** the §3Z ≥95 % floor. Per
`ORCHESTRATOR.md §3Z` the binding rule is "≥95 % × 2 consecutive cycles
+ zero orphan REVISEs"; this cycle satisfies neither sub-clause (both
aggregates below floor AND six lenses carry orphan REVISEs totalling 22
REVISE-class findings).

### §0.3 REJECT roster

**Zero REJECT findings** across all 7 lenses. The seven-lens sweep
surfaces no falsification of any T-P1 inventory claim. All REVISEs are
bounded to specific rows / counts / verdict-wording and admit
mechanical or near-mechanical correction in a V2 fold.

### §0.4 REVISE roster (deduplicated across lenses)

22 REVISE-class findings cluster into 11 fold-class groups:

1. **F-V2-1F-PATTERN-H** (CH1-SC-4 + CH7-§1-row-4 + CH7-§2.4 + CH7-§3.2):
   `google_sheets=6` transcription error appears verbatim in
   `1F-coherence-scan.md:63` (COH-011), `1F-anti-pattern.md:75` (AP-016),
   `1F-past-corpora.md:83` (PC-017). Live `find` and S-P0 A6 baseline
   `sk-v14-audit-overfit-pre-restart-pattern.md:53` both confirm
   `google_sheets=10`. Breakdown `8+7+7+7+7+7+6+7+7 = 63` arithmetically
   fails the asserted total `= 67`. Single-author propagation; single
   correction site per row.

2. **F-V2-1F-COH-012-FABRICATED-CITE** (CH1-SC-5 + CH7-§1-row-5 +
   CH7-§3.1 + CH7-§2.3): COH-012 at `1F-coherence-scan.md:64` asserts
   `LOCKS.md:46` "declares Lock 14 + CH7 Overfit-Prune lens binding".
   Live `grep -n "CH7\|Overfit" restart/locks/LOCKS.md` returns zero
   hits. The underlying coherence finding (PASS-1-EXCAVATION.md §3
   registers only CH1-CH6; CH7 binding missing) is correct; only the
   counter-surface citation is fabricated. 1E LAC-1E-12 carries the
   correct anti-fabrication phrasing `LOCKS.md (no CH7 mention)` and is
   the promotion candidate. Five in-file cross-references must be
   corrected together (`:64`, `:83`, `:100`, `:109`, `:117`).

3. **F-V2-1F-COH-011-EIGHT-VS-NINE** (CH1-SC-6): COH-011 at
   `1F-coherence-scan.md:63` writes "eight-grammar set" but cites
   `ARCHITECTURE.md:765` which reads "**nine** extant grammars".
   Single-word prose correction; the enumerated list itself is the
   correct nine.

4. **F-V2-1C-LIVE-COUNTS** (CH2-row-7 + CH2-row-8 + CH2-§Lock-14-table):
   1C `:91,123` cites "at minimum 19 matches" for parser-name leak;
   live `rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser'
   crates/core/src/runtime/` returns 30 matches across 15 files. 1C
   `:90,122` cites "60+ grammar-named types" reexported; mechanical
   extraction from `crates/core/src/runtime/mod.rs:25-71` yields 126
   distinct grammar-named symbols (after subtracting 10 neutral). The
   `~50 LOC of reexports to delete` estimate at 1C:122 is consequently
   undersized: mod.rs:25-71 is 47 lines holding 126 names with
   proportional downstream consumer rewires.

5. **F-V2-1D-PASS-LAYER-LEAK** (CH2-row-9 + CH2-required-revision-2):
   1B carries P1-1B-D8 (recognizer mining JSON-byte whitelist at
   `passes/src/lib.rs:331`) and P1-1B-D10 (role mining JSON literals
   at `passes/src/lib.rs:1300-1391`) but 1D's JSON-empirical-vs-grammar-
   neutral matrix at `1D:106` collapses codegen-layer grammar-name
   leaks and pass-layer grammar-shape leaks into one "disproved" row.
   Split needed: (a) codegen-layer grammar-name leak; (b) pass-layer
   grammar-shape leak with verify_action at the two `passes/src/lib.rs`
   sites.

6. **F-V2-1D-BBNF-SIMD-CROSS-CITE** (CH2-row-10): 1D:113 marks
   `bbnf-simd` "grammar-neutral substrate" PROVED on SK-V14 axis A3 v3
   §4 authority; 1F-past-corpora PC-008 + open question `U-PC-002` at
   `1F-past-corpora.md:158` retain SK-V5 verify-before-rederive
   obligation. Cross-cite needed at 1D:113 so the proved row carries
   its open-question pointer.

7. **F-V2-CSS-L4-LAYOUT-ASYMMETRY** (CH2-row-11 + CH2-required-revision-4):
   1C-D11 at `1C-runtime-evidence.md:167` flags skinny CSS L4 7-cluster
   vs main monolithic `css_l4/`+`css_pretty/` asymmetry but neither 1D
   nor 1F-coherence-scan carries the row. SK-V14 R4 `regen-css` xtask
   obligation at `SYNTHESIS.md:96` is canonical-layout determinant.

8. **F-V2-1D-TALLY-RECONCILIATION** (CH3-CH3-004 + CH3-CH3-008): 1D
   frontmatter `divergence_count: 5/11/2/7` cannot be reconstructed
   from either of its two evidence tables (spec-claim table at 100-115
   carries 7 proved + 6 disproved + 3 impl_unimplemented = 16 rows;
   divergence table at 119-137 carries 11 spec_unimplemented + 3
   impl_unimplemented + 1 impl_exceeds_spec + 1 disproved + 1
   disproved-baseline = 17 rows). Dispatch text "5 proved / 11
   disproved" propagates a further axis-crossing reading. Reconcile
   frontmatter + dispatch wording + divergence-row counts to one
   explicit tally schema.

9. **F-V2-1D-ADMIT-VS-REJECT-SPLIT** (CH3-CH3-005): 1D row at `:121`
   carries "Forty admit rows AUDIT-FALSIFIED across SK-V12+SK-V13" with
   "7-11 typed + 4-6 direct" range; REDRESS contains exactly 5 typed
   PASS-ADMITs (W13.1/.2/.3/.4 + W15.1) and 2 direct PASS-ADMITs
   (W11.1, W11.3). W13.5-9 are MEASURED-REJECT, not admit rows; the
   collapsed range risks future agents treating W13.5-9 REJECTs as
   reopen candidates. Split into explicit PASS-ADMIT subrow + broader
   ROLLING-SOTA-DELTA audit-overlay subrow with `SYNTHESIS.md:75-84`
   cite.

10. **F-V2-CH5-SUBSTRATE-CLASSIFICATIONS** (CH5-002 + CH5-004 +
    CH5-005 + CH5-007): four CH5 REVISEs converge on substrate-coupling
    nuance — (a) 1A-SUB-014 reads renamed `StructuralIndex` scanner as
    closure but live `scan.rs:1` "JSON-owned structural scan source"
    must remain fenced as transient capacity/proof input; (b) 1F
    anti-pattern scan misses CSS source-sidecar coupling at
    `nonjson_css_l4.rs:222,234,299,504`; (c) Track 2 imports
    `runtime::grammars::json::scan::structural_capacity_for` +
    `TapeBuilder` at `bbnf-bench/src/track2/json.rs:5,24,43` — parser
    independent, substrate-helper shared; (d) `json_event_grammar_witness`
    + `sheets_witness` exported under cfg gates at `runtime/src/lib.rs:9`
    are generic-root proof exports, not "harmless residue".

11. **F-V2-CH6-VERDICT-VS-NOTE** (CH6-row-2 + CH6-row-4 + CH6-row-7 +
    CH6-row-9 + CH6-row-15 + CH6-row-17): six verdict-cells read as
    closure ("implemented" / "Partial; VM replay UNKNOWN" / "proved" /
    "held through two more independent confirming cycles" / "partial /
    residue" / "accepted historical pre-block") while the same row's
    note or sister-inventory cross-fold admits scheduling UNKNOWN,
    conflates two closure modes, or lacks current-cycle live-scan
    confirmation. All six are surface-level closure-wording fixes; none
    require fresh evidence gathering.

## §1 — Cross-lens convergence (critical findings)

Four convergence patterns surface where two or more lenses independently
land on the same finding, raising its priority for V2 fold:

### §1.1 — 1F citation drift converges across CH1 + CH7 (F-V2-1F-PATTERN-H + F-V2-1F-COH-012-FABRICATED-CITE)

Two independent lenses (CH1 CORRECTNESS at SC-4 + SC-5; CH7 OVERFIT-PRUNE
at §1 row 4 + row 5 + §2.3 + §2.4 + §3.1 + §3.2) land on the same 1F
authorship: the three 1F outputs share the `google_sheets=6` transcription
error verbatim, and the 1F-coherence-scan COH-012 row fabricates a
"Lock 14 + CH7 Overfit-Prune lens binding" string into
`LOCKS.md:46` that does not exist. CH7 names this a meta-CH7-relevant
finding: the coherence-scan inventory adopted the very citation pattern
the CH7 lens is built to prevent (asserting prose into a source that
does not contain it). The 1E LAC-1E-12 row at `1E-locks-evidence.md:120`
already carries the correct anti-fabrication phrasing `LOCKS.md (no CH7
mention)` and is the promotion-candidate template for the 1F COH-012
correction. CH7 §2.5 executable verification confirms zero productive
`@generated by skinny bbnf-codegen` admits across all 8 inventories;
CH7 §2.6 confirms audit-zero baseline propagation across all 8
inventories; CH7 §2.7 confirms 1C ↔ S-P0 A6 byte-for-byte. The
correction surface is narrow and well-localised.

### §1.2 — 1A/1D substrate coupling nuance converges across CH5 + CH6 (F-V2-CH5-SUBSTRATE-CLASSIFICATIONS + F-V2-CH6-VERDICT-VS-NOTE row 3)

CH5 §3 explicitly REVISEs 1A-SUB-014 (renamed `StructuralIndex` scanner
plane) and Track 2 shared-substrate-helper caveat; CH6's New Finding §3
identifies "the strongest paper-close vulnerability in the V1 corpus" —
the 1D row at `:100` marks "Single substrate: Lock 1 tape ∪ direct-to-
struct union" as **proved** while 1A simultaneously catalogues 1A-DIV-008
as a SK-V14 first-cycle divergence recording two structurally independent
cursor types at HEAD (`runtime/src/grammars/json/parser.rs:7-12`
ParserState.cursor vs `codegen/src/json_typed_direct.rs:518-522`
DirectParser.cursor with no tape). Both lenses converge: the
substrate-union ratification IS the substantive T-P3 §3C disposition.
Either (a) ratify the two-cursor shape as the substrate-union (in which
case 1D :100 reads correctly under the ratified definition) or
(b) mandate unification (in which case 1D :100 must downgrade to
"disproved at HEAD; obligation deferred to T-P2 unification wave"). The
renamed scanner, CSS source-sidecar, Track 2 substrate-helper, and
proof-witness rows are all sub-cases of the same Lock 1 union-vs-split
disposition T-P3 must close.

### §1.3 — 1C undercount escalation (CH2; F-V2-1C-LIVE-COUNTS) — most material lone-lens finding

CH2's two row-7 + row-8 REVISEs are the largest material gap surfaced
this cycle: 19+→30 parser-name matches (+58 %) and 60+→126 reexports
(+110 %). The repair-LOC estimate at 1C:122 ("~50 LOC of reexports to
delete") was consequently undersized; 47 lines hold 126 names with
proportional downstream rewires. This does NOT change Lock 14's
authority (the floor formulations are not false claims — they are
honest "at minimum" hedges), but it does materially weaken 1C's
defensible authority for T-P3 disposition. F-V2-1C-LIVE-COUNTS replaces
floor-with-live-count and widens the cost band on 1E LAC-1E-15 (Pattern
H 67-file census + Lock 14 reexport closure) accordingly.

### §1.4 — 1B pass-layer leak unfolded into 1D (CH2; F-V2-1D-PASS-LAYER-LEAK)

CH2 row-9 identifies that 1B's P1-1B-D8 (recognizer mining JSON-byte
whitelist) and P1-1B-D10 (role mining JSON literals) are catalogued at
the pass layer (`passes/src/lib.rs:331,1300-1391`) but 1D's matrix
collapses codegen-layer grammar-name leaks and pass-layer grammar-shape
leaks into one disproved row at 1D:106. This is the only finding that
crosses the inventory-to-inventory dispatch axis (1B → 1D): the
codegen-layer Lock 14 fix per LAC-1E-15 is sufficient for codegen-name
leaks but does NOT unblock non-JSON role mining because the
shape-coding pre-blocker lives in the pass layer. Splitting 1D:106 into
two rows surfaces a same-wave consumer obligation for T-P3 §3C: any
non-JSON grammar fixture demonstrating that role facts arise without
code change in passes.

### §1.5 — LAC-1E-12 promotion candidate (CH7-binding surface authority)

CH7 §1 row 6 + §3.1 explicitly identifies 1E LAC-1E-12 at
`1E-locks-evidence.md:120` as carrying the correct anti-fabrication
phrasing `LOCKS.md (no CH7 mention)`. This is the template both for
the F-V2-1F-COH-012-FABRICATED-CITE correction AND for any future
inventory wishing to cite the LOCKS.md CH7 silence. CH7's
recommendation: T-P3 §3C should consider promoting LAC-1E-12 to a
first-class Lock 17 or Lock 18 amendment in the lock-amendment slate,
codifying `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` as
the binding CH7 surface authority alongside the existing 16 locks.
The promotion candidacy is non-blocking for V2 mechanical convergence
but is the most substantive cross-lens governance signal in V1.

## §2 — V2 fold packet (deduplicated, with priority + size estimate)

Eleven V2 fold packets prescribed below. All eleven are **light**
mechanical edits (total ≈75 min wall) that close all 22 REVISE
findings across the six REVISE lenses; CH4 lens passes ACCEPT and
needs no V2 fold. No HEAVY wave-program work surfaces this cycle —
the V1 inventories are documentary in nature; substantive
implementation work (Pattern H closure, Lock 14 reexport deletion,
two-cursor unification) is correctly routed to T-P3 §3C / SK-V14 C-1
PRUNE-3/PRUNE-4 by the V1 LAC ledger.

### §2.1 — F-V2-1F-PATTERN-H — google_sheets=6 → 10 across three 1F rows (LIGHT)

**Closes:** CH1 SC-4; CH7 §1 row 4 + §2.4 + §3.2.

**Scope:** Correct `google_sheets=6` to `google_sheets=10` at three
single-cell sites:
- `1F-coherence-scan.md:63` COH-011 breakdown
- `1F-anti-pattern.md:75` AP-016 breakdown
- `1F-past-corpora.md:83` PC-017 breakdown

Per-file repeated occurrences (e.g., `1F-coherence-scan.md:75` row
prose; `1F-anti-pattern.md:99` V2 Planning Metadata; `1F-past-corpora.md:120`
V2 Planning Metadata) also need the same single-token correction. The
asserted `= 67` totals are correct and require no change. The +3 over
V13's 64 framing in 1E:100 is correct and requires no change.

**Cost:** ≈8 min wall (LOW). Three sed-replacements.

**Convergence impact:** CH1 5/8 → 6/8 (75 %); CH7 7/9 → 8/9 (88.9 %).

### §2.2 — F-V2-1F-COH-012-FABRICATED-CITE — replace LOCKS.md:46 fabrication (LIGHT)

**Closes:** CH1 SC-5; CH7 §1 row 5 + §2.3 + §3.1.

**Scope:** Replace at five 1F-coherence-scan sites
(`:64,83,100,109,117`) the fabricated string
`\`restart/locks/LOCKS.md:46\` declares "Lock 14 + CH7 Overfit-Prune
lens binding"` with the LAC-1E-12-template phrasing
`\`restart/locks/LOCKS.md\` carries no CH7 binding clause (verified
\`grep -n "CH7\|Overfit" restart/locks/LOCKS.md\` returns zero hits);
\`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87\` is the lens
authority; the SK-V14 contract relies on it`.

**Cost:** ≈6 min wall (LOW). Five edit sites, identical replacement.

**Convergence impact:** CH1 6/8 → 7/8 (87.5 %); CH7 8/9 → 9/9 (100 %).
LAC-1E-12 promotion candidate confirmed.

### §2.3 — F-V2-1F-COH-011-EIGHT-VS-NINE — eight-grammar → nine-grammar prose (LIGHT)

**Closes:** CH1 SC-6.

**Scope:** Single prose substitution at `1F-coherence-scan.md:63`:
change "eight-grammar set" to "nine-grammar set". The cited
`ARCHITECTURE.md:764-765` reads "nine extant grammars" and the
enumerated list itself is correctly nine.

**Cost:** ≈2 min wall (LOW). Single token correction.

**Convergence impact:** CH1 7/8 → 8/8 (100 %).

### §2.4 — F-V2-1C-LIVE-COUNTS — live counts replace floor formulations (LIGHT)

**Closes:** CH2 row 7 + row 8 + Lock 14 verification table rows 2 + 5.

**Scope:** Replace at `1C-runtime-evidence.md:90,91,122,123` the
"at minimum 19 matches" / "60+ grammar-named types" formulations with
the captured live counts (30 matches across 15 files; 126 distinct
reexports from mod.rs:25-71 with 10-neutral subtraction enumerated).
Widen the repair-LOC band at 1C:122 from "~50 LOC of reexports to
delete" to reflect the 47-lines-holding-126-names ratio plus
proportional downstream consumer rewires. Add the `google_sheets/builder.rs`
and `google_sheets/document/mod.rs:43,142` sites missed by 1C:91's
enumeration.

**Cost:** ≈10 min wall (LOW). Mechanical re-grep + count update.

**Convergence impact:** CH2 6/10 → 8/10 (80 %); widens LAC-1E-15
hard-cap band for T-P3 §3C consumption.

### §2.5 — F-V2-1D-PASS-LAYER-LEAK — split 1D:106 into codegen + pass layer (LIGHT)

**Closes:** CH2 row 9 + required revision 2.

**Scope:** Split the 1D row at `1D-skinny-lessons.md:106` ("Generic
non-JSON grammar generation is proven") into two rows:
- (a) codegen-layer grammar-name leak (8 `RuntimeProvider` variants /
  8 per-grammar provider modules / Pattern H 67 files — already cited
  via 1C/1E)
- (b) pass-layer grammar-shape leak (recognizer mining + role mining
  at `passes/src/lib.rs:331,1300-1391`) with verify_action pointing at
  a Sheets or BBNF-self grammar fixture whose role facts arise without
  code change in passes

**Cost:** ≈6 min wall (LOW). Single row split with cross-cite.

**Convergence impact:** CH2 8/10 → 9/10 (90 %); surfaces same-wave
consumer obligation for T-P3 §3C.

### §2.6 — F-V2-1D-BBNF-SIMD-CROSS-CITE — PC-008 + U-PC-002 cross-cite at 1D:113 (LIGHT)

**Closes:** CH2 row 10.

**Scope:** Add to the note column at `1D-skinny-lessons.md:113`:
"Survives SK-V14 axis A3 audit; 1F-past-corpora PC-008 + `U-PC-002`
retain SK-V5 verify-before-rederive obligation pending captured
`rg -n 'JSON_STRUCTURAL|scan_json|JsonParseIndex' skinny/crates/bbnf-simd
skinny/crates/runtime` artefact."

**Cost:** ≈3 min wall (LOW). Single annotation.

**Convergence impact:** CH2 9/10 → 10/10 (100 %).

### §2.7 — F-V2-CSS-L4-LAYOUT-ASYMMETRY — catalogue 1C-D11 in 1D (LIGHT)

**Closes:** CH2 row 11 + required revision 4.

**Scope:** Add a 1D row reading "skinny CSS L4 7-cluster vs main
monolithic css_l4/ + css_pretty/ layout asymmetry — neither layout is
canonical for V1; SK-V14 R4 `regen-css` xtask obligation determines
canonical at `SYNTHESIS.md:96`" with T-P3 disposition obligation. Lens
preference is 1D (per CH2 required revision 4 rationale: SK-V14 R4 is
the canonical-layout determinant and 1D is the dispatch surface for
SK-V14-binding obligations).

**Cost:** ≈4 min wall (LOW). Single row addition.

**Convergence impact:** Already at 10/10 from §2.6; this is reinforcement.

### §2.8 — F-V2-1D-TALLY-RECONCILIATION — frontmatter + dispatch + table tally schema (LIGHT)

**Closes:** CH3 CH3-004 + CH3-008.

**Scope:** Reconcile 1D frontmatter `divergence_count` with one
authoritative table. Option (a) re-key frontmatter to spec-claim-table
totals (7 proved / 6 disproved / 3 impl_unimplemented / 0 unknown);
option (b) re-key frontmatter to divergence-table totals
(11 spec_unimplemented / 3 impl_unimplemented / 1 impl_exceeds /
1 disproved / 1 disproved-baseline = 17). Update CH-CONTEXT §2
dispatch text to match the chosen schema. Recommended: option (b)
because divergence-table totals are load-bearing for T-P3 §3C
amendment routing.

**Cost:** ≈5 min wall (LOW). Frontmatter + dispatch alignment.

**Convergence impact:** CH3 6/9 → 7/9 (77.8 %).

### §2.9 — F-V2-1D-ADMIT-VS-REJECT-SPLIT — split falsified-row count (LIGHT)

**Closes:** CH3 CH3-005.

**Scope:** Split the 1D row at `1D-skinny-lessons.md:121` ("Forty admit
rows AUDIT-FALSIFIED across SK-V12+SK-V13 close") into two explicit
subrows:
- (a) REDRESS PASS-ADMIT typed rows AUDIT-FALSIFIED: cardinality 5
  (W13.1/.2/.3/.4 + W15.1) and direct rows AUDIT-FALSIFIED: cardinality
  2 (W11.1, W11.3)
- (b) ROLLING-SOTA-DELTA broader audit-overlay: 7-11 typed / 4-6
  direct ranges with explicit cite to `SYNTHESIS.md:75-84`

Prevents future agents from treating W13.5-9 REJECTs as reopen
candidates.

**Cost:** ≈5 min wall (LOW). Single row split.

**Convergence impact:** CH3 7/9 → 8/9 (88.9 %).

### §2.10 — F-V2-CH5-SUBSTRATE-CLASSIFICATIONS — four classifications added (LIGHT)

**Closes:** CH5-002 + CH5-004 + CH5-005 + CH5-007.

**Scope:** Four classifications across three inventories:
1. **1A-SUB-014 verdict downgrade** at `1A-substrate-evidence.md:43`
   from "implemented" to "partial / unknown: no retained
   `StructuralIndex` identity found, but renamed scanner side plane
   is live and must stay fenced as transient capacity/proof input"
   (cite `scan.rs:1,22,51`).
2. **Add CSS source-sidecar row** to `1F-anti-pattern.md` between
   AP-019 and U-AP-001: AP-020 classifying
   `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504` as
   comparator-sidecar evidence requiring same fenced treatment as 1A
   CSS fact streams.
3. **Track 2 substrate-helper caveat** at
   `1D-skinny-lessons.md:30` (existing row about Track 1/2 sharing
   `TapeBuilder`): add note "parser-independent, substrate-helper-
   shared; Track 2 imports `runtime::grammars::json::scan::
   structural_capacity_for` + `TapeBuilder` at
   `bbnf-bench/src/track2/json.rs:5,24,43`; independence holds for
   parser implementation, not substrate helpers".
4. **AP-010 verdict strengthen** at `1F-anti-pattern.md:69` from
   "partial / residue" to either "proof-cfg fenced; production absent"
   (with captured `cargo build` evidence) or "Lock 14 leak under
   unverified proof gate" (pending capture). 1C-U2 EventTape route
   applies.

**Cost:** ≈15 min wall (LOW). Four edit sites, classification work.

**Convergence impact:** CH5 3/7 → 7/7 (100 %); CH6-row-15 (AP-010
softness) co-closed.

### §2.11 — F-V2-CH6-VERDICT-VS-NOTE — six verdict-cell alignments (LIGHT)

**Closes:** CH6-row-2 + row-4 + row-7 + row-9 + row-17 (row-15
co-closed by §2.10; row-3 substrate-cardinality co-closed by §2.10's
Track 2 caveat + T-P3 §3C substrate-union ratification).

**Scope:** Six verdict-cell alignments:
1. **1A-SUB-001/005/006/010** at `1A:53,57,58,62`: downgrade verdict
   cells from "implemented" / "implemented side table" to "partial /
   scheduling UNKNOWN" with pointer to 1A-UNK-003.
2. **1B :53** row: split into (a) WASM/TS deferral per V1 (cite
   ARCH §10.2 deferral line) and (b) VM replay audit gap (route to
   UNKNOWN-1). Conflation of deferred-by-spec vs live-evidence-UNKNOWN
   in single Partial cell broken.
3. **1D :100** row "Single substrate proved as substrate cardinality":
   downgrade to "proved historically; SK-V14 1A-DIV-008 records
   two-cursor structural split at HEAD pending T-P3 §3C disposition".
4. **1E :89** L16 executive summary: add explicit sustained-UNKNOWN
   paragraph listing L03 + L16 + the two NEW SK-V14 UNKNOWNs at the
   `1E:33-35` framing to prevent held-disposition language reading as
   universal closure.
5. **1F-past-corpora PC-001/002/004** at `1F-past-corpora.md:67,68,70`:
   add "current absence UNKNOWN; route to verify_action" to verdict
   cells in parity with PC-003 (`:69`), OR capture live-scan output
   that proves no regression and cite inline.
6. **1F-coherence-scan divergence_count metadata** at
   `1F-coherence-scan.md:38-42`: either correct `spec_claims_implemented`
   to match the V2 Planning Metadata row table OR add a column noting
   which 4 rows count as "implemented" in metadata's terms.

**Cost:** ≈16 min wall (LOW). Six verdict-cell / metadata alignments.

**Convergence impact:** CH6 13/19 → 19/19 (100 %).

## §3 — V2 dispatch shape

### §3.1 — Inventories requiring V2 micro-fold

All eight inventories require at least one V2 fold edit (six of seven
lenses REVISE; CH4 alone ACCEPTs):

| Inventory | V1 lens findings | V2 fold packets | Edit count | Est. wall |
|---|---|---|---:|---:|
| 1A-substrate-evidence.md | CH5-002 (SUB-014); CH6-row-2 (SUB-001/005/006/010) | §2.10 #1 + §2.11 #1 | 5 cells | ~10 min |
| 1B-codegen-evidence.md | CH6-row-4 (:53) | §2.11 #2 | 1 row split | ~3 min |
| 1C-runtime-evidence.md | CH2-row-7 + row-8 | §2.4 | 4 cells + 1 prose | ~10 min |
| 1D-skinny-lessons.md | CH2-row-9 + row-10 + row-11; CH3-004 + 005 + 008; CH5-005; CH6-row-7 | §2.5 + §2.6 + §2.7 + §2.8 + §2.9 + §2.10 #3 + §2.11 #3 | 7 row edits | ~25 min |
| 1E-locks-evidence.md | CH6-row-9 (L16) | §2.11 #4 | 1 prose paragraph | ~3 min |
| 1F-coherence-scan.md | CH1-SC-5 + SC-6; CH7 §1 row 4 + 5; CH6-row-17 | §2.1 + §2.2 + §2.3 + §2.11 #6 | 8 cells | ~15 min |
| 1F-anti-pattern.md | CH1-SC-4; CH5-004 + 007; CH6-row-15; CH7 §1 row 4 | §2.1 + §2.10 #2 + §2.10 #4 | 3 cells + 1 new row | ~8 min |
| 1F-past-corpora.md | CH1-SC-4; CH2-row-10 (bbnf-simd cross-cite anchor); CH6-row-19 (PC-001/002/004); CH7 §1 row 4 | §2.1 + §2.6 anchor + §2.11 #5 | 4 cells | ~8 min |

**No inventory locks at V1.** Total V2 fold surface: ~82 min wall
across 11 fold packets distributed over 8 inventories. All packets are
LIGHT (mechanical correction / single-cell verdict alignment / row
split); zero HEAVY wave-program work surfaces this cycle.

### §3.2 — V2 dispatch recommendation

Dispatch a single T-P1 V2 micro-fold packet to all eight inventories
in one parallel agent fan-out (per V2 inventory; ~10-25 min per agent).
Each agent receives the inventory's specific fold-packet subset from
§3.1 plus the cross-lens convergence context from §1 (1F citation drift
+ 1A/1D substrate coupling + 1C undercount + 1B pass-layer leak +
LAC-1E-12 promotion candidate). The V2 aggregator dispatches a V2
CHALLENGE cycle over the seven lenses, expecting all six REVISE lenses
to converge to 100 % per the §2 fold packet sizing.

## §4 — §3Z gate evaluation

### §4.1 — V1 cycle gate state

Per `ORCHESTRATOR.md §3Z` (≥95 % × 2 cycles, zero orphan REVISEs):

- V1 is the **first** cycle (no prior ≥95 % cycle to chain).
- Sub-axis aggregate **68.6 % below floor**; per-lens mean **68.3 %
  below floor**. Both aggregation methods fail.
- **22 orphan REVISEs** across six lenses (CH1: 3; CH2: 4; CH3: 3;
  CH5: 4; CH6: 6; CH7: 2). CH4 alone discharges ACCEPT.

**Cycle verdict: NOT-CONVERGED-V2-REQUIRED.** V2 must (a) clear all 22
orphan REVISEs and (b) drive both aggregates above the 95 % floor with
zero new orphan REVISEs; only then can a V3 cycle attempt the "× 2
cycles" close.

### §4.2 — V2 forecast (light-fold-only)

With the eleven light packets (§2.1-§2.11, ~82 min wall total):

| Lens | V1 rate | Expected V2 rate (light-only) | Net |
|---|---:|---:|---|
| CH1 | 62.5 % | 100 % (8/8) | §2.1 + §2.2 + §2.3 close all 3 REVISE rows |
| CH2 | 60 % | 100 % (10/10) | §2.4 + §2.5 + §2.6 + §2.7 close all 4 REVISE rows |
| CH3 | 66.7 % | 100 % (9/9) | §2.8 + §2.9 close 3 REVISE findings (CH3-006 ACCEPT untouched) |
| CH4 | 100 % | 100 % (8/8) | No V2 work needed; T-P3 governance items noted for later cycles |
| CH5 | 42.9 % | 100 % (7/7) | §2.10 closes all 4 REVISE findings (substrate classification quartet) |
| CH6 | 68.4 % | 100 % (19/19) | §2.11 closes 5 REVISE findings; §2.10 #4 closes the 6th (AP-010) |
| CH7 | 77.8 % | 100 % (9/9) | §2.1 + §2.2 close both REVISE findings (1F triplet google_sheets + COH-012 fabrication) |

**Expected sub-axis-weighted V2 aggregate:** ≈100 % (70 / 70 if every
fold packet lands cleanly; conservative 95-97 % allowing for any V2
fold introducing one new minor finding).

**Expected per-lens mean V2:** ≈100 %.

**V2 outcome under light-only:** ≥95 % on both aggregation methods;
zero orphan REVISEs; **first ≥95 % cycle achieved**. V3 then has the
burden of producing the second consecutive ≥95 % cycle to discharge
§3Z "≥95 % × 2 cycles".

### §4.3 — V3 forecast and predicted §3Z close path

Under V2 light-fold-only:

- V3 inherits V2's ≈100 % sub-axis aggregate + 100 % per-lens ACCEPT
  on all seven lenses.
- V3 work surface: re-verify the V2 light-fold edits land cleanly;
  surface any new cite drift introduced by V2 textual edits;
  reconfirm the seven executable verifications from CH7 §2.1-§2.7
  still PASS at HEAD; reconfirm the four CH7 cross-cutting
  observations (§3.1 LOCKS.md leak; §3.2 1C-vs-1F asymmetry; §3.3
  no SCAFFOLD-ONLY admits; §3.4 Lock 14 prose compliance) still hold.
- V3 expected outcome: ≥95 % on both aggregation methods with zero
  new orphan REVISEs. **§3Z convergence on the second consecutive
  ≥95 % cycle.** T-P3 §3C dispatch gate opens per
  `PASS-1-EXCAVATION.md §5`.

**Predicted §3Z close path: V2 (mechanical fold) → V3 (verification
re-pass) → LOCK.** Not the V2 → LOCK shortcut: §3Z requires the
"× 2 consecutive cycles" sub-clause and V1's 68.6 % aggregate is the
first cycle, so V2 cannot serve as both the convergence cycle and the
×2 chain entry. V3 is structurally required.

## §5 — §3Z LOCK criteria for V2

Per `ORCHESTRATOR.md §3Z`, V2 lock criteria are:

1. **Sub-axis aggregate ≥ 95 %** on the post-V2 lens dispositions
   (V1 baseline 68.6 % → V2 target ≥95 %; expected 100 % per §4.2).
2. **Zero orphan REVISEs.** Every REVISE finding from V1 must either
   (a) resolve to ACCEPT in V2 OR (b) reclassify to a non-orphan
   verdict (contracted-deferral / impl-exceeds-spec / unknown-with-
   verify-action). No V1 REVISE may carry into V2 as still-orphan
   without explicit reclassification justification.
3. **No new orphan REVISEs surfaced by V2 work.** V2's textual edits
   to the eight inventories must not introduce new cite drift, new
   tally inconsistency, or new closure-wording-without-evidence. CH7
   §2.5 fake-pattern recurrence scan and §2.6 audit-zero baseline
   propagation scan should re-run cleanly at V2 commit.

**Predicted V2 lens ACCEPT-rate vector for §3Z close:** CH1 100, CH2
100, CH3 100, CH4 100, CH5 100, CH6 100, CH7 100 (all seven lenses).
Sub-axis-weighted aggregate ~100 %; per-lens mean 100 %.

**Likely V2 → V3 → LOCK trajectory:** V2 closes 22 REVISEs in
~82 min wall + one CHALLENGE V2 dispatch cycle; V3 re-runs the seven
CHALLENGE V2 lenses against the V2-folded artefacts (expected
~30-60 min wall lens dispatch + ~30 min aggregator); V3 ratifies the
second consecutive ≥95 % cycle and discharges §3Z. T-P3 §3C dispatch
context can be staged in parallel with V3 to land immediately on V3
LOCK.

## §6 — Sources

V1 lens dispositions (all verified existing at write-time):
- `restart/audit/totality/p1/hardening/V1/CH1.md` (161 lines)
- `restart/audit/totality/p1/hardening/V1/CH2.md` (91 lines)
- `restart/audit/totality/p1/hardening/V1/CH3.md` (57 lines)
- `restart/audit/totality/p1/hardening/V1/CH4.md` (99 lines)
- `restart/audit/totality/p1/hardening/V1/CH5.md` (31 lines)
- `restart/audit/totality/p1/hardening/V1/CH6.md` (65 lines)
- `restart/audit/totality/p1/hardening/V1/CH7.md` (247 lines)
- `restart/audit/totality/p1/hardening/V1/CHALLENGE-CONTEXT.md` (38 lines)

V1 T-P1 inventory artefacts under review:
- `restart/audit/totality/p1/1A-substrate-evidence.md` (112 lines; Lock 1 honoured-with-caveat; 8 divergences; BIR 13/20)
- `restart/audit/totality/p1/1B-codegen-evidence.md` (116 lines; only 1/5 BackendShape lowerers carries real logic; 20 divergences)
- `restart/audit/totality/p1/1C-runtime-evidence.md` (203 lines; 67/67 hand-written runtime files Pattern H; BackendShape names absent from crates/)
- `restart/audit/totality/p1/1D-skinny-lessons.md` (161 lines; spec-claim + divergence dual table; SK-V14 first_cycle_additions)
- `restart/audit/totality/p1/1E-locks-evidence.md` (162 lines; 16 LACs — 11 V4-carried + 5 SK-V14 NEW; LAC-1E-12 anti-fabrication phrasing)
- `restart/audit/totality/p1/1F-coherence-scan.md` (117 lines; 12 coherence drifts; COH-012 fabricated cite)
- `restart/audit/totality/p1/1F-anti-pattern.md` (120 lines; 19 anti-patterns; AP-016 google_sheets typo)
- `restart/audit/totality/p1/1F-past-corpora.md` (159 lines; 17 past-findings; PC-017 google_sheets typo)

Binding authorities:
- `restart/prompts/totality/PASS-1-EXCAVATION.md §3 + §5` (CH1-CH6 specialisations; §5 convergence + T-P3 dispatch gate)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry) + `§3Z` (convergence rule)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens definition; the SK-V14 contract relies on it as the de facto CH7 binding source since LOCKS.md is silent on CH7)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` (SK-V14 contract; R1-R10 + C-1..C-5 + PRUNE-1..PRUNE-7)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` (S-P0 prune list; 74 findings; google_sheets=10 baseline at `:210`)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:46-56,153` (S-P0 A6 Pattern H baseline; google_sheets=10)
- `restart/locks/LOCKS.md` (governance text; verified zero CH7/Overfit references via `grep -n` at lens dispatch time; LAC-1E-12 anti-fabrication phrasing template)
- `restart/ARCHITECTURE.md:764-765` (nine extant grammars enumeration; `:922-944` 20-variant BackendExpr table; `:1100-1108` per-shape lowerer contract)

On-disk artefacts verified at V1 write-time (per CHALLENGE-CONTEXT §3
executable-verification mandate; CH7 §2.1-§2.7 path-existence pass):
- `crates/core/src/runtime/` (9 grammar directories, 67 hand-written .rs files; `find` reproduces 1C `:38` baseline byte-for-byte)
- `crates/core/src/runtime/mod.rs:25-71` (47 lines, 126 grammar-named reexports + 10 neutral exports + 3 CSS aliases)
- `crates/core/src/runtime/google_sheets/` (10 files; 1C `:53` reproduces; 1F triplet incorrectly cites 6)
- `cargo metadata --format-version 1 --no-deps | jq '.metadata.bbnf.grammars'` (9 grammars; CH7 §2.1 PASS)
- `grep -n "CH7\|Overfit" restart/locks/LOCKS.md` (zero hits; CH7 §2.3 confirms COH-012 fabricated cite; LAC-1E-12 anti-fabrication phrasing template)
- `grep -c '@generated by skinny bbnf-codegen' restart/audit/totality/p1/1*.md` (zero hits across all 8 inventories; CH7 §2.5 PASS)
