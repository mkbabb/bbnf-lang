# SK-V14 T-P1 Totality Excavation — V2 CHALLENGE Consolidated

Aggregator: SK-V14 T-P1 V2 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V2 over the eight V2-amended T-P1 inventory
artefacts (8 files at `restart/audit/totality/p1/`: 1A 1B 1C 1D 1E
1F-coherence-scan 1F-anti-pattern 1F-past-corpora — committed at
HEAD `87816a2cd` as the T-P1 V2 atomic micro-fold, 8 inventories
amended in one pass per the V1 consolidator §2 fold packet).
Authority: `restart/prompts/totality/PASS-1-EXCAVATION.md §3 + §5`
(CH1-CH6 specialisations and convergence rule); `restart/prompts/ORCHESTRATOR.md
§3W` (universal CH-lens registry) + `§3Z` (≥95 % × 2 consecutive cycles
+ zero orphan REVISEs); `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7`
(Overfit-Prune lens binding from S-P0; LOCKS.md silent on CH7 per LAC-1E-12);
prior V1 consolidator at `restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md`
(format reference + baseline carry-forward); dispatch
`restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md` §0-§4.
Input ledger: seven V2 lens dispositions under
`restart/audit/totality/p1/hardening/V2/`
(`CH1.md` 341 lines, `CH2.md` 112, `CH3.md` 89, `CH4.md` 202, `CH5.md`
158, `CH6.md` 73, `CH7.md` 291 — 1266 lens lines + 37 CHALLENGE-CONTEXT
lines).

This consolidator supersedes the SK-V13-era T-P1 V2 stub previously
occupying this path (which audited an earlier inventory cycle prior to
the SK-V14 V6 inventory refresh + V1 lens dispatch).

## §0 — V2 cycle verdict

### §0.1 Per-lens dispositions (verbatim from each CH file's §verdict / §cycle)

| Lens | Definition | Sub-axes | ACCEPT | REVISE | REJECT | Per-lens ACCEPT-rate | V1 ACCEPT-rate | Verdict |
|---|---|---:|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | every row's spec path:line ↔ impl path:line ↔ verdict triangle resolves against HEAD; cited symbol presence verified; cited negative-search results reproduce | 8 inventories | 8 | 0 | 0 | **100 %** | 62.5 % | ACCEPT (first ≥95 % cycle; one V1 CH6 carry-forward at 1E:33-35 routed to V3 CH6, not CH1) |
| CH2 GENERALITY | Lock 14 holds; no JSON-only divergence catalogued when grammar-neutral substrate fact; 1C runtime census flags every grammar-named module in a generic crate; 1D separates JSON-empirical from grammar-neutral; no grammar-name leak passes uncited; pass-layer grammar-shape leaks distinct from codegen-layer name leaks | 12 rows (5 V1-discharge + 3 V2-new + 4 Lock 14 coverage) | 11 | 1 | 0 | **91.7 %** | 60 % | REVISE (single off-by-one: 1C reexport count 126 → 127; css_l4 = 43 not "~41"; only 6 of the 10 cited neutrals fall inside the cited 25-71 window — `IntoPathSegment, Path, PathSegment, RuntimeView` sit at `mod.rs:72,76` OUTSIDE window) |
| CH3 REGRESSION (REDRESS) | no T-P1 inventory re-opens a REDRESS route already rejected, blocked, or admitted-then-falsified; 1D pre-block list + 1E LAC ledger correctly identify SK-V14 pre-blocks; no admitted REDRESS row mis-catalogued as unimplemented | 9 findings | 9 | 0 | 0 | **100 %** | 66.7 % | ACCEPT (first ≥95 % cycle; V1 CH3-004 tally + CH3-005 admit-vs-reject split + CH3-008 dispatch axis cross all discharged; 1D divergence-row count 19 reconciles cell-for-cell with frontmatter integer keys) |
| CH4 COST | every divergence carries LOC-delta + risk class under orchestrator six-field schema (loc_budget / risk / wave / hard_cap / same_wave_consumer / evidence_basis); 1E amendment candidates state wave-alignment hint | 8 artefacts | 8 | 0 | 0 | **100 %** | 100 % | ACCEPT (2-cycle LOCK satisfied on CH4 alone; 16/16 LACs pass wave-alignment + path:line; 5/5 V2 dispatch-context convergence points verify; hard-cap multiplier 1.2-1.4× upheld across all V2-amended rows) |
| CH5 HIDDEN COUPLING | no parallel substrate / sidecar / retained cursor / second source scan; substrate union holds; no renamed-scanner closure; 1F anti-pattern scan catches live couplings | 12 findings | 12 | 0 | 0 | **100 %** | 42.9 % | ACCEPT-with-caveat (first ≥95 % cycle; V1 CH5-002/004/005/007 quartet all folded; CH5-V2-008 caveat = cite-staleness on `nonjson_css_l4.rs:222,234,299,504` + Track 2 `:5,24,43` + proof-witness `runtime/src/lib.rs:9` — disposition NON-blocker, V3 housekeeping ~45 LOC) |
| CH6 ANTI-PAPER-CLOSE | self-reports of "resolved/wired/honoured/proved/implemented pre-block" require live-evidence citation; every UNKNOWN carries verify_action; no divergence deferred to "a later inventory" | 19 row classifications | 19 | 0 | 0 | **100 %** | 68.4 % | ACCEPT (first ≥95 % cycle; all six V1 verdict-cell + bundle-deferral REVISEs discharged; row-100 paper-close vulnerability carries exact V1-CH6-required two-branch wording; AP-020 cite-drift surfaced to CH1 V2 as cite-correction, not CH6 paper-close) |
| CH7 OVERFIT-PRUNE | SK-V14 audit-corrected baseline propagation; no fake-pattern recurrence in inventory text; 1F coherence-scan correctly flags LOCKS.md:46 CH7-binding leak (per anti-fabrication phrasing); 1C 67-file Pattern H matches S-P0 A6 byte-for-byte; executable verification mandate applies to every cite-bearing micro-fold | 9 inventory targets | 6 | 3 | 0 | **66.7 %** | 77.8 % | **REVISE — REGRESSION** (V1 77.8 % → V2 66.7 %; AP-020 / AP-009 / 1A-SUB-014 cite cluster `nonjson_css_l4.rs:222,234,299,504` propagated verbatim from V1 CH5-004 into V2 micro-fold without re-execution of verification mandate — V2 caught V1's COH-012 fabrication but introduced new fabrication of the same class; AP-009 cites symbol `lightningcss_facts` with zero hits anywhere in source) |

### §0.2 Aggregate ACCEPT-rate

Two aggregation methods (per `ORCHESTRATOR.md §3Z`):

- **Sub-axis-weighted (load-bearing for §3Z convergence):**
  (8+11+9+8+12+19+6) / (8+12+9+8+12+19+9) = **73 / 77 = 94.8 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (100 + 91.7 + 100 + 100 + 100 + 100 + 66.7) / 7 = **94.0 %**.

Both aggregates land **just below** the §3Z ≥95 % floor (sub-axis 94.8 %
vs 95 %; per-lens 94.0 % vs 95 %). The V1 → V2 trajectory is a sharp
upward step (V1 68.6 % → V2 94.8 % sub-axis; +26.2 pp) but the two
binding REVISEs (CH2 91.7 %, CH7 66.7 %) prevent first-cycle ≥95 %
close. §3Z gate not yet open — V3 fold required to land both lenses
above floor before any "× 2 consecutive cycles" chain can begin.

### §0.3 REJECT roster

**Zero REJECT findings** across all 7 lenses on the V2 cycle (matches V1
zero-REJECT posture). All four V2 REVISEs (1 in CH2, 3 in CH7) admit
mechanical correction in a V3 fold; none falsifies a substantive
inventory claim. The CH7 REVISE is a structural anti-fabrication failure
mode (cite-carry without re-verification across cycles), not a
conceptual error: AP-020's fence semantics + wave routing + LOC band
all ACCEPT per CH7 §3.4; only the path:line cites need rebinding.

### §0.4 REVISE roster (deduplicated across lenses)

Four REVISE-class findings cluster into 2 fold-class groups + 3 light
housekeeping items:

1. **F-V3-CH2-1** (CH2 single REVISE; 1C reexport count off-by-one):
   1C `:91,123` cites "126 grammar-named symbols" via "subtract 10
   grammar-neutral exports from `mod.rs:25-71`". Live mechanical
   extraction at HEAD `87816a2cd` returns 133 raw `pub use` entries
   inside the cited 25-71 window; only 6 of the 10 cited neutrals
   (`CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle,
   StructBuilder`) are present inside that window — the other 4
   (`IntoPathSegment, Path, PathSegment` at `mod.rs:72` +
   `RuntimeView` at `mod.rs:76`) sit OUTSIDE the cited window. Correct
   count is **127** (subtract 6 neutrals from 133 inside the window)
   OR widen the cited window to 25-77 (137 raw - 10 neutrals = 127).
   Per-grammar breakdown: bbnf 10, bnf 10, css_l4 **43** (not "~41"),
   css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math 10
   (sum 127). Same defect class V1 CH2 flagged ("60+" floor
   under-reporting) — V2 closed the order-of-magnitude gap (60→126) but
   introduced a +1 arithmetic error in the neutral subtraction.
   Downstream propagation contained to 1C (1F-coherence COH-011 +
   1F-past PC-017 quote only file/dir counts, not the 126 figure).

2. **F-V3-CH7-1** (CH7 binding REVISE; AP-020 + AP-009 + 1A-SUB-014
   stale-cite cluster): `bbnf-bench/src/nonjson_css_l4.rs:222,234,299,504`
   propagated verbatim from V1 CH5-004 (`hardening/V1/CH5.md:23`,
   itself never executable-verified) into three V2 inventory rows:
   `1F-anti-pattern.md:55,80,105` (AP-020 prose + LOC-budget row + exec
   summary), `1F-anti-pattern.md:69,94` (AP-009 prose + LOC-budget),
   `1A-substrate-evidence.md:10,67` (1A-SUB-014 verdict cell + V1
   hardening fold note). HEAD verification (CH7 §2.5): line 222 = CSS
   token hex literal `"tok\tdecl=1\tidx=3\tdepth=0\tkind=paren_close\t..."`;
   line 234 = same (paren_close token); line 299 = CSS declaration
   literal `"decl\tidx=3\tdepth=1\tproperty_hex=636f6c6f72\t..."`;
   line 504 = `impl fmt::Display for CssOracleError`. Real cites:
   `fixture_sidecar_facts` callsite at `:648`, definition at `:2691`;
   `same-plane-source-sidecar` literal writers at `:1082, 1203, 1354,
   1511, 1661, 1815, 1964` (seven literals, one per CSS L4 sub-grammar
   wave). File grew V1→V2 from cited range to 3,644 LOC. **Direct
   violation of CHALLENGE-CONTEXT.md §3 "Executable verification
   mandate"**: V2 micro-fold did not re-execute verification before
   propagating V1 cite cluster.

3. **F-V3-CH7-2** (CH7 binding REVISE; AP-009 symbol `lightningcss_facts`
   has zero hits): AP-009 at `1F-anti-pattern.md:69,94` cites symbol
   `lightningcss_facts` as the comparator-sidecar entry point. HEAD
   verification (CH7 §3.1 + §2.5): `grep -n 'lightningcss_facts'
   skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns zero hits.
   The relevant comparator-sidecar symbols are `fixture_sidecar_facts`
   (`:648, :2691`) and `same-plane-source-sidecar` (`:1082+`). Rebind
   AP-009 to executable-verified symbol + line set, or remove the
   `lightningcss_facts` cite if not present in source.

4. **F-V3-CH5-1** (light housekeeping; CH5 V2 cite caveat): Track 2
   `bbnf-bench/src/track2/json.rs:5,24,43` drifts off-by-2 to
   `:7,26,45` at HEAD (per CH5 §2 live verification). Proof-witness
   cite `runtime/src/lib.rs:9` drifts to `:29-33` at HEAD (cfg-gated
   `pub mod json_event_grammar_witness` + `pub mod sheets_witness`).
   Total ~45 LOC across 4 cite sites. Non-blocker for V2 ACCEPT per CH5
   ACCEPT-with-caveat disposition; V3 housekeeping.

5. **F-V3-CH1-1** (cosmetic; CH1 V2 SC-V2-1): 1D row 157 (Track 2
   substrate-helper caveat per CH5-005 fold) prose includes
   "folds into row 100 substrate-union ratification" — actually row 117
   in V2 numbering. V1→V2 row-renumbering artefact; substantive cite
   resolves; flagged for V3 cosmetic-correction only.

**Structural addition for T-P3 §3C (LAC-1E-12 procedural addendum):**
CH7 §Failure-mode-characterisation explicitly proposes adding to the
LAC-1E-12 promotion a procedural rule: *any inventory edit that carries
a path:line cite forward from a prior cycle MUST re-execute the
verification (grep / find / line read) at the new HEAD before commit*.
The V2 failure mode was V1 cite-carry without re-verification — exactly
the anti-fabrication pattern CH7 is built to surface. Institutionalising
this rule in the LAC-1E-12 carrier closes the structural loophole.

## §1 — Cross-lens convergence (critical findings)

Four convergence patterns surface where two or more lenses
independently land on the same finding, raising priority for V3 fold or
flagging cross-lens disagreement:

### §1.1 — Stale cite cluster `nonjson_css_l4.rs:222,234,299,504` converges across CH5 + CH6 + CH7

Three independent lenses (CH5 §CH5-V2-008 ACCEPT-with-caveat; CH6 §New
Finding CH6-V2 surfaced to CH1; CH7 §3.1 binding REVISE) all land on
the identical fabricated-cite cluster carried verbatim from V1 CH5-004
(`hardening/V1/CH5.md:23`) into V2 1A-SUB-014 + 1F-anti-pattern AP-020 +
1F-anti-pattern AP-009. CH5 dispositions as ACCEPT-with-caveat
(documentation drift, classification holds) and CH6 surfaces to CH1
(path:line drift = CH1 concern, not paper-close). CH7 elevates to
binding REVISE because the V2 micro-fold cited V1 CH5-004's never-
verified cite text without re-executing the verification mandate — a
structural violation of CHALLENGE-CONTEXT.md §3. The three readings are
all accurate under their respective lens contracts; the strictness
ordering CH7 > CH6 > CH5 is correct for an anti-fabrication mandate
lens, and the binding REVISE forces V3 fold. The convergent finding
spans 3 inventories + 5 cite sites; a single V3 fold packet (F-V3-CH7-1)
discharges all three lens dispositions simultaneously.

### §1.2 — CH1 ↔ CH6 1E:33-35 sustained-UNKNOWN paragraph cross-lens disagreement

CH1 V2 SC-V2-4 explicitly classifies the 1E executive summary at
`1E-locks-evidence.md:33-35` as **NOT discharging** V1 CH6 REVISE #4 (no
explicit sustained-UNKNOWN paragraph listing L03 + L16 + the two NEW
SK-V14 UNKNOWNs); CH6 §Findings row 9 (`1E-locks-evidence.md:89`)
classifies it as **discharged** via the Open Questions table at
`1E:157-162` (L03 + L16 + audit-overlay gap + Lock 1 fact-stream
taxonomy all named) + the §1.5 LAC-1E-12 promotion candidacy block at
`:126-128` (binding-surface-authority precondition for any future
CH7-citing artefact). Both readings are accurate under their respective
lens contracts: CH1 polices verbatim presence of the prescribed
paragraph at the prescribed location; CH6 polices substantive
anti-paper-close discharge (which the Open Questions table + §1.5
together satisfy). NON-BLOCKING — CH6 returns 100 % ACCEPT on this row
(the substantive concern is met); CH1's flag is CH6-carry-forward, not
own-lens REVISE. V3 may add an executive-summary cross-reference to the
Open Questions table for full CH1 cosmetic satisfaction, but no V3 fold
is binding on this disagreement.

### §1.3 — LAC-1E-12 promotion validated across CH4 + CH6 + CH7

Three lenses converge on the V2 LAC-1E-12 promotion at
`1E-locks-evidence.md:120` + §1.5 governance-signal block at `:126-128`
as honest (non-paper-close) governance disposition: CH4 §5 verifies
zero new LOC obligation (V1 `60-180 LOC docs / low risk / 240 LOC hard
cap` preserved verbatim; promotion is wave-alignment sharpening, not
cost-class change); CH6 §Findings row 4 verifies explicit non-amendment
posture ("Promotion remains non-blocking for V2 mechanical convergence;
T-P3 §3C disposes whether Lock 17/Lock 18 numbering or in-preface
CH7-binding clause is the carrier") + 1E :166 "no T-P1 1E direct edits
to LOCKS.md"; CH7 §3.3 verifies three legitimacy markers (non-blocking;
full authority-chain cite to V1 CONSOLIDATED §1.5 + hardening/V1/CH7.md;
honest meta-CH7 reinforcement naming V1 COH-012 as empirical proof of
LAC-1E-12 necessity). Triple-lens convergence on legitimacy across cost,
paper-close, and anti-fabrication axes — strongest substantive
cross-lens validation in V2.

### §1.4 — Meta-CH7 collision (V2 fix landed) recognised by CH6 + CH7

CH6 §New Finding (meta-CH7 collision validation, sustained from V1
CH6) + CH7 §3.3 + 1E :126-128 §1.5 promotion explainer all recognise
that 1F-coherence COH-012's V1 fabricated cite "empirically reproduced
exactly the anti-pattern CH7 is built to prevent" and that the
anti-fabrication phrasing `LOCKS.md (no CH7 mention)` at 1E `:97, :120,
:145` is the canonical template for any future inventory wishing to
cite LOCKS.md CH7 silence. This is the strongest single closure
delivered by V2: the V1 paper-close pattern was replaced with
executable verification + cross-inventory binding-surface-authority
precondition. The convergent recognition across CH6 + CH7 + 1E §1.5
hardens the LAC-1E-12 promotion candidacy from V1 "non-blocking
governance proposal" to V2 "binding-surface-authority precondition for
any S-P2 / T-P3 CH7-citing artefact". The V3 cite-carry-without-
re-verification failure mode (F-V3-CH7-1 + F-V3-CH7-2) is the next
governance loophole to close, and the LAC-1E-12 procedural addendum
(§0.4 structural addition) is the proposed carrier.

## §2 — V2 strengthening packet (what V2 fold discharged)

The V2 micro-fold (commit `87816a2cd`; 8 inventories amended in one
atomic pass) discharged the V1 CONSOLIDATED §2 fold packet across all
11 fold groups, moving 4 lenses from V1 REVISE to V2 100 % ACCEPT and
1 lens (CH4) confirming the V1 ACCEPT into a per-lens 2-cycle LOCK:

### §2.1 — CH1 CORRECTNESS: V1 62.5 % → V2 100 % (8/8 ACCEPT)

All three V1 REVISEs discharged at HEAD `87816a2cd`:
- **SC-4 (google_sheets=6→10 transcription error)**: V2 1F-coherence
  COH-011 + 1F-anti AP-016 + 1F-past PC-017 all read `google_sheets=10`
  with arithmetic reconciliation `8+7+7+7+7+7+10+7+7=67` and live
  `find` verification cited inline.
- **SC-5 (COH-012 LOCKS.md:46 fabricated CH7 cite)**: V2 COH-012 at
  `:74,93,110,127` replaces fabrication with anti-fabrication phrasing
  (`LOCKS.md carries no CH7 binding clause; verified grep returns zero
  hits at HEAD 2026-05-23`) + three resolving citations
  (`PASS-1-EXCAVATION.md:91-138`, `PASS-0-OVERFIT-AUDIT.md:62-87`,
  `SYNTHESIS.md:22`).
- **SC-6 (COH-011 eight-vs-nine grammar prose)**: V2 COH-011 at `:73`
  reads "nine-grammar set" matching cited `ARCHITECTURE.md:764-765`.

CH1 V2 first ≥95 % cycle achieved; 1E:33-35 carry-forward is CH6 lens
concern (cite-resolution intact; framing softness only), routed to V3
CH6 disposition per §1.2. CH1 V3 trajectory: ≥95 % expected if
1E:33-35 addressed under V3 CH6 fold; no CH1-specific regression risk.

### §2.2 — CH2 GENERALITY: V1 60 % → V2 91.7 % (11/12 ACCEPT, single REVISE on 1C reexport count)

Four of five V1 REVISEs discharged at HEAD:
- **V1-REVISE-7 (parser-name 19+→30)**: V2 1C `:91,123,124,199` all
  cite "30 matches across 15 files" with full google_sheets/document
  enumeration; HEAD verification matches.
- **V1-REVISE-9 (1B D8/D10 → 1D matrix collapse)**: V2 1B carries D8
  + D10 as distinct rows in all four 1B tables (spec-claim,
  generic-crate census, Sheets/BBNF-Self Implications, divergences-
  catalogued); 1D `:123` (codegen-layer) + `:124` (pass-layer) splits
  with explicit verify_action.
- **V1-REVISE-10 (1D bbnf-simd cross-cite)**: V2 1D `:131` carries
  PC-008 + U-PC-002 verify-before-rederive obligation with rg
  verify_action.
- **V1-REVISE-11 (CSS L4 layout asymmetry)**: V2 1D `:134` adds new
  row with R4 regen-css canonical-layout determinant framing.

Single V2 REVISE on **V1-REVISE-8 (reexports 60+→126)**: V2 closed the
order-of-magnitude gap (60→126) but introduced a +1 arithmetic error in
the neutral-subtraction list (4 of 10 cited neutrals sit outside the
cited 25-71 window). Correct count is 127. CH2 V3 trajectory: ≥95 %
expected after F-V3-CH2-1 micro-correction.

### §2.3 — CH3 REGRESSION: V1 66.7 % → V2 100 % (9/9 ACCEPT)

All three V1 REVISEs discharged:
- **CH3-004 + CH3-008 (1D tally drift + dispatch axis cross)**: V2 1D
  frontmatter at `:53-75` carries explicit V2-fold comment naming both
  axes (divergence-table 19-row totals load-bearing for T-P3 §3C;
  spec-claim-table 18-row epistemic axis acknowledged as DISTINCT);
  V2 dispatch text reads exact cardinality "5+2+5+24=36 AUDIT-FALSIFIED"
  dropping V1's axis-crossing "5 proved/11 disproved" phrasing.
- **CH3-005 (admit-vs-reject conflation)**: V2 1D `:140` split into
  explicit REDRESS PASS-ADMIT typed (5: W13.1-4 + W15.1) + direct (2:
  W11.1, W11.3) + parse_only (5: W14.1-5) + CSS L4 (24: W5 lineage) =
  36; `:141` ROLLING-SOTA-DELTA broader audit-overlay with
  `SYNTHESIS.md:75-84` cite; W13.5-9 explicitly fenced as
  MEASURED-REJECT inside note column. REDRESS PASS-ADMIT cardinality
  byte-verified at `REDRESS.md:4293/4377/4463/4504/4542/4581/4995`
  (ADMIT) vs `:4622/4646/4675/4705/4734` (MEASURED-REJECT).

CH3 V3 trajectory: 100 % expected (verification re-pass; no
regression-discipline change introduced by V2 micro-fold).

### §2.4 — CH4 COST: V1 100 % + V2 100 % = per-lens 2-cycle LOCK SATISFIED

CH4 alone among the seven lenses satisfies §3Z "× 2 consecutive cycles
≥95 %" at V2 commit — V1 ACCEPT 8/8 + V2 ACCEPT 8/8. V2 dispatch-
context five convergence points all verify at HEAD:
- 1C undercount rescale (~50→~190 LOC + 2.5× consumer-rewire band
  proportional to 126-symbol surface) properly composes root + consumer
  cost components.
- 1B D8/D10 row split carries two distinct 250-500 LOC budgets with
  600/650 LOC hard caps + distinct same-wave consumer obligations.
- AP-020 LOC band (40-120 fence + 160 hard cap; 1.33× multiplier
  conforms to V1 CH4 1.2-1.4× convention) + co-wave routing with
  AP-009.
- 1D row 113 cross-cite preserves SK-V14 R4 regen-css xtask
  wave-alignment hint across four sibling rows + sequencing-only row.
- LAC-1E-12 promotion adds zero new LOC obligation; V1 `60-180 LOC docs
  / low / 240 cap` frame preserved verbatim; wave-alignment sharpening
  to T-P3 §3C priority is the entire delta.

CH4 standalone-closed at V2; no V3 CH4 dispatch required. T-P3 §3C
carry-forward governance items: (a) adopt LAC-1E-15 per-tranche framing
as load-bearing Pattern H budget; (b) codify 1.2-1.4× hard-cap
multiplier convention in Lock 8 V+1 wording.

### §2.5 — CH5 HIDDEN COUPLING: V1 42.9 % → V2 100 % (12/12 ACCEPT, 1 ACCEPT-with-caveat)

All four V1 CH5 REVISEs discharged with explicit classifications:
- **CH5-002 (renamed StructuralIndex scanner)**: V2 1A-SUB-014 verdict
  downgraded to "partial / unknown — no retained StructuralIndex
  identity found, renamed scanner side plane live" with cites to
  `scan.rs:1,22,47-54,51` + `generated.rs:12-15`; transient capacity/
  proof fencing made explicit.
- **CH5-004 (CSS source-sidecar)**: NEW V2 AP-020 row in 1F-anti-pattern
  + 1A-SUB-014 cite cross-ref classifying CSS source-sidecar as
  comparator-sidecar coupling requiring fence (non-runtime-authoritative).
- **CH5-005 (Track 2 substrate-helper)**: V2 1D `:157` + 1F-anti AP-011
  classify as Lock 1 union sub-case (not Track-independence violation);
  parser-independent, substrate-helper-shared.
- **CH5-007 (proof-witness)**: V2 1F-anti AP-010 verdict strengthened
  to "Lock 14 leak under unverified proof gate (pending captured
  `cargo build` evidence)".

CH5-V2-008 caveat = cite-staleness on V1-fold-forward line numbers
(documentation drift, classifications hold at HEAD per live verification).
V3 housekeeping ~45 LOC. CH5 V3 trajectory: 100 % expected after
F-V3-CH5-1 housekeeping fold.

### §2.6 — CH6 ANTI-PAPER-CLOSE: V1 68.4 % → V2 100 % (19/19 ACCEPT)

All six V1 CH6 REVISEs discharged:
- **1A-SUB-001/005/006/010 verdict cells**: V2 reads `partial /
  scheduling UNKNOWN (route → 1A-UNK-003)` at `:54,58,59,63`.
- **1B :53 WASM-deferral vs VM-replay-UNKNOWN conflation**: V2 1B
  D8/D10 row split (cross-fold with CH2) breaks the conflation.
- **1D :100 substrate-cardinality paper-close**: V2 1D `:117` reads
  exact V1-CH6-required wording "proved historically; SK-V14 1A-DIV-008
  records two-cursor structural split at HEAD pending T-P3 §3C
  disposition" with two-branch ratification-or-unification rule.
- **1E :89 L16 sustained-UNKNOWN elision**: V2 Open Questions table at
  `:157-162` lists L03 + L16 + the two NEW SK-V14 UNKNOWNs explicitly;
  §1.5 LAC-1E-12 promotion at `:126-128` sharpens binding-surface
  authority. (CH1 V2 SC-V2-4 flags executive-summary cross-reference
  gap; CH6 dispositions as discharged per substantive Open Questions
  presence — cross-lens disagreement per §1.2.)
- **1F-anti AP-010 proof-gate verdict softness**: V2 verdict strengthened
  (cross-fold with CH5-007 §2.10 #4).
- **1F-past PC-001/002/004 historical pre-block parity**: V2 `:67,68,70`
  all read "accepted historical pre-block; current absence UNKNOWN" with
  targeted rg verify_action (parity with PC-003 `:69`).

CH6 V3 trajectory: ≥95 % expected; only non-CH6 surface item is the
AP-020 cite-drift surfaced to CH1/CH7 (path:line concern, not
paper-close).

### §2.7 — CH7 OVERFIT-PRUNE: V1 77.8 % → V2 66.7 % (6/9 ACCEPT — REGRESSION)

CH7 is the only lens that **regressed** V1→V2 (77.8 % → 66.7 %). Two
V1 REVISE items mechanically cleared:
- COH-012 fabricated LOCKS.md:46 cite replaced with anti-fabrication
  phrasing + executable grep evidence at all four sites
  (`:74,93,110,127` + 1E §1.5 meta-CH7 acknowledgement at `:128`).
- COH-011/AP-016/PC-017 google_sheets=6 typo corrected to 10 with
  explicit arithmetic reconciliation `8+7+7+7+7+7+10+7+7=67` and
  verify_action cited inline.

But V2 introduced a **new fabrication of the same class**: V1 CH5-004's
never-verified `nonjson_css_l4.rs:222,234,299,504` cite cluster was
carried verbatim into V2 AP-020 (`1F-anti-pattern.md:55,80,105`) +
AP-009 (`:69,94`) + 1A-SUB-014 (`1A-substrate-evidence.md:10,67`).
HEAD verification (CH7 §2.5): cited lines are CSS token hex literals +
`impl Display`, NOT `fixture_sidecar_facts` routing or
`same-plane-source-sidecar` writer code. Real cites: `:648` callsite,
`:2691` definition, `:1082, 1203, 1354, 1511, 1661, 1815, 1964`
writers. AP-009 additionally cites symbol `lightningcss_facts` with
zero `grep` hits anywhere in source.

**Regression analysis**: V2 caught one V1 fabrication (COH-012) but
propagated another (CH5-004 → AP-020 + AP-009 + 1A-SUB-014). The
structural anti-fabrication failure mode is **cite-carry across cycles
without re-execution of the verification mandate** (direct violation of
CHALLENGE-CONTEXT.md §3). The fix is to extend the executable-
verification mandate to every cite-bearing micro-fold — including
forward-carried cites from prior cycles — and to fold this rule into
the LAC-1E-12 promotion as a binding procedural addendum (proposed in
§0.4 structural addition; carries to T-P3 §3C disposition). V3 redress
is deterministic and tractable: replace four line numbers in three
files + one symbol rebind in AP-009; conceptual content + wave routing
+ fence semantics all stand per CH7 §3.4. CH7 V3 trajectory: 100 %
expected after F-V3-CH7-1 + F-V3-CH7-2 micro-corrections.

## §3 — V3 fold packet (5 items, all LIGHT/MEDIUM, 4 inventories touched)

Five V3 fold packets prescribed below. Four are **LIGHT** mechanical
edits + one **MEDIUM** (multi-site cite rebind across three inventories)
totalling ≈ 40 min wall, closing the four CH2 + CH7 binding REVISEs
plus discharging the two non-blocking carry-forwards (CH5 V3 cite
refresh, CH1 cosmetic row-numbering). No HEAVY wave-program work
surfaces this cycle.

### §3.1 — F-V3-CH2-1 — 1C reexport count 126 → 127 (LIGHT)

**Closes:** CH2 single REVISE on 1C reexport count off-by-one.

**Scope:** Replace at `1C-runtime-evidence.md:91,123` (and propagation
sites `:124,200` if quoting the figure) the "126 distinct grammar-named
symbols" formulation with one of two equivalent repairs:
- **Repair A (preferred, preserves 47-line window):** "127 distinct
  grammar-named symbols within `mod.rs:25-71` after subtracting the
  **6** grammar-neutral exports present inside the window
  (`CompoundHandle, DtaError, GenericAtRule, ParseErr, StringHandle,
  StructBuilder`)".
- **Repair B (widens window):** "127 distinct grammar-named symbols
  within `mod.rs:25-77` after subtracting all **10** grammar-neutral
  exports (`IntoPathSegment, Path, PathSegment` at `:72`, `RuntimeView`
  at `:76` added to the 6 in-window neutrals)".

Per-grammar breakdown updates to: bbnf 10, bnf 10, css_l4 **43** (not
"~41"), css_pretty 10, csv 10, ebnf 10, google_sheets 11, json 13, math
10 (sum 127). No LOC-band change required (47-line window prose remains
valid under Repair A; widens to 53-line window under Repair B). Add
NEW-CH2-V2-03 discipline rule to V3 dispatch: any "N grammar-named X"
subtract-from-K cite must enumerate the K neutrals with `path:line`
inside the cited window. Downstream propagation contained to 1C — no
cross-file fold required.

**Cost:** ≈ 8 min wall (LOW). Two cell updates + per-grammar breakdown.

**Convergence impact:** CH2 11/12 → 12/12 (100 %).

### §3.2 — F-V3-CH7-1 — AP-020 + AP-009 + 1A-SUB-014 cite rebind (MEDIUM)

**Closes:** CH7 binding REVISE on stale cite cluster
`nonjson_css_l4.rs:222,234,299,504`; CH5 ACCEPT-with-caveat cite-
staleness; CH6 surfaced-to-CH1 cite-drift flag.

**Scope:** Replace at five sites the V1 CH5-004-propagated fabricated
cite cluster with executable-verified HEAD cites:
- `1F-anti-pattern.md:80` (AP-020 evidence cell): `:222,234` → `:648`
  (`fixture_sidecar_facts` callsite); `:299` → `:1082` (representative
  `same-plane-source-sidecar` writer; full list `:1082, 1203, 1354,
  1511, 1661, 1815, 1964`); `:504` → `:2691` (function definition).
- `1F-anti-pattern.md:105` (AP-020 LOC-budget row): same cite correction.
- `1F-anti-pattern.md:55` (AP-020 executive summary): same cite
  correction.
- `1F-anti-pattern.md:69,94` (AP-009 evidence cell + LOC-budget row):
  same cite correction (parallel cluster `:222-234, :298-303` from V1
  carries forward and needs identical rebind).
- `1A-substrate-evidence.md:10,67` (1A-SUB-014 V1-hardening-fold-note +
  verdict cell): same cite correction; same fix in 1A frontmatter fold
  note.

All five sites cite the same physical source (`bbnf-bench/src/nonjson_css_l4.rs`)
at HEAD `87816a2cd`; one V3 fold packet discharges all three lens
dispositions (CH5 V2 caveat + CH6 surfaced-to-CH1 + CH7 binding REVISE)
per §1.1 convergence.

**Cost:** ≈ 15 min wall (MEDIUM). Five edit sites, three inventories.

**Convergence impact:** CH7 6/9 → 8/9 (88.9 %); CH5 cite-staleness
caveat fully closed.

### §3.3 — F-V3-CH7-2 — AP-009 symbol `lightningcss_facts` rebind (LIGHT)

**Closes:** CH7 binding REVISE on AP-009 zero-hits symbol.

**Scope:** AP-009 at `1F-anti-pattern.md:69,94` cites symbol
`lightningcss_facts` with zero `grep -n` hits anywhere in source. Rebind
to executable-verified symbol set: `fixture_sidecar_facts` (`:648,
:2691`) and `same-plane-source-sidecar` (`:1082, 1203, 1354, 1511,
1661, 1815, 1964`). Verify with `rg -n 'lightningcss_facts'
skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` at HEAD before commit
(expect zero hits if symbol does not exist; remove `lightningcss_facts`
cite text + replace with the verified symbol set).

**Cost:** ≈ 5 min wall (LOW). Two edit sites in one inventory.

**Convergence impact:** CH7 8/9 → 9/9 (100 %).

### §3.4 — F-V3-CH5-1 — Track 2 + proof-witness cite refresh (LIGHT)

**Closes:** CH5 ACCEPT-with-caveat (V3 housekeeping per CH5 V2 §Required
V3 Fold).

**Scope:** Two cite refreshes per CH5 §Required V3 Fold:
- **Track 2 cite refresh** at `1D-skinny-lessons.md:157` and
  `1F-anti-pattern.md:71`: replace `bbnf-bench/src/track2/json.rs:5,24,43`
  with `:7,26,45` (off-by-2 drift at HEAD). ~10 LOC.
- **Proof-witness cite refresh** at `1D-skinny-lessons.md:117`: replace
  `runtime/src/lib.rs:9` with `runtime/src/lib.rs:29-33` (cfg-gated
  `pub mod json_event_grammar_witness` + `pub mod sheets_witness`). ~5
  LOC.

**Cost:** ≈ 8 min wall (LOW). Three edit sites across 2 inventories.

**Convergence impact:** CH5 ACCEPT-with-caveat → ACCEPT (caveat
discharged).

### §3.5 — F-V3-CH1-1 — 1D row 157 cosmetic row-pointer (LIGHT)

**Closes:** CH1 V2 SC-V2-1 cosmetic flag.

**Scope:** Single prose substitution at `1D-skinny-lessons.md:157`:
change "folds into row 100 substrate-union ratification" to "folds into
row 117 substrate-union ratification" (V1→V2 row-renumbering artefact;
substantive cite resolves; row 100 is V1 numbering of what is row 117
in V2 numbering).

**Cost:** ≈ 2 min wall (LOW). Single token correction.

**Convergence impact:** CH1 cosmetic clean-up; no per-lens rate change.

### §3.6 — Structural addition for T-P3 §3C: LAC-1E-12 procedural addendum (PROPOSAL ONLY)

**Closes:** Structural anti-fabrication failure mode surfaced by F-V3-CH7-1
(cite-carry without re-verification across cycles).

**Scope (proposal):** Fold into the LAC-1E-12 promotion candidacy
(`1E-locks-evidence.md:120` + §1.5 at `:126-128`) a procedural rule:
*any inventory edit that carries a path:line cite forward from a prior
cycle MUST re-execute the verification (grep / find / line read) at the
new HEAD before commit*. The V2 CHALLENGE-CONTEXT.md §3 "Executable
verification mandate" currently reads as applying to newly-authored cites
only; the V2 CH7 failure mode demonstrates it must also apply to
forward-carried cites. T-P3 §3C disposes whether the addendum lands as
Lock 17/Lock 18 numbering, in-preface CH7-binding clause, or LAC-1E-12
expansion. T-P1 PROPOSES; T-P3 disposes (per CH6 §1.5 governance-signal
rule); Pass Omega merges.

**Cost:** Zero LOC at T-P1 (proposal carried as 1E §1.5 paragraph
addition + V3 dispatch-context discipline rule); ~20-60 LOC docs at
T-P3 §3C disposition (within LAC-1E-12 V1 `60-180 LOC docs / low risk /
240 LOC hard cap` envelope per CH4 §5).

**Convergence impact:** Structural fix preventing recurrence of the V2
failure mode in V3, V4, and S-P2 cycles; no V3 ACCEPT-rate change.

## §4 — V3 dispatch shape

### §4.1 — Inventories requiring V3 micro-fold

Four of eight inventories require at least one V3 fold edit:

| Inventory | V2 lens findings | V3 fold packets | Edit count | Est. wall |
|---|---|---|---:|---:|
| 1A-substrate-evidence.md | CH7 stale cite cluster (1A-SUB-014 + V1-hardening-fold-note); CH5 caveat (cross-cite via 1D fold) | F-V3-CH7-1 (2 sites at :10,67) | 2 cells | ~5 min |
| 1C-runtime-evidence.md | CH2 reexport count off-by-one (126→127) | F-V3-CH2-1 (2 sites at :91,123 + per-grammar breakdown) | 2 cells + breakdown | ~8 min |
| 1D-skinny-lessons.md | CH5 caveat (Track 2 :157 + proof-witness :117); CH1 cosmetic (row 157 prose "row 100"→"row 117") | F-V3-CH5-1 (2 sites) + F-V3-CH1-1 (1 site) | 3 cells | ~8 min |
| 1F-anti-pattern.md | CH7 stale cite cluster (AP-020 :55,80,105 + AP-009 :69,94); CH7 symbol rebind (AP-009 `lightningcss_facts` zero-hits); CH5 caveat (AP-011 :71 Track 2 cite) | F-V3-CH7-1 (5 sites) + F-V3-CH7-2 (2 sites) + F-V3-CH5-1 (1 site) | 8 cells | ~18 min |
| 1E-locks-evidence.md (PROPOSAL) | Structural anti-fabrication addendum | F-V3-§3.6 (1E §1.5 paragraph addition; proposal-only) | 1 paragraph | ~5 min (proposal text only) |

**Four inventories pass V3-unchanged**: 1B (CH2 D8/D10 split clean,
CH4 LOC budgets clean, CH7 ACCEPT), 1F-coherence-scan (COH-011/COH-012
V2 fixes landed cleanly across all 7 lenses), 1F-past-corpora (PC-017
V2 fix landed; PC-001/002/004 parity discharged; PC-008 anchor sound),
1E (with optional §3.6 addendum proposal text).

**No HEAVY wave-program work surfaces.** Total V3 fold surface: ~40 min
wall across 5 LIGHT/MEDIUM packets distributed over 4 inventories. All
packets are mechanical correction / single-cite rebind / row-pointer
fix; zero substantive wave-program work.

### §4.2 — V3 dispatch recommendation

Dispatch a single T-P1 V3 micro-fold packet to the four inventories
requiring V3 edits in one parallel agent fan-out (per V3 inventory;
~5-18 min per agent). Each agent receives the inventory's specific
fold-packet subset from §4.1 plus the cross-lens convergence context
from §1 (stale cite cluster CH5+CH6+CH7; CH1↔CH6 1E:33-35 disagreement;
LAC-1E-12 triple-lens validation; meta-CH7 collision recognition).

After V3 micro-fold commit, the V3 aggregator dispatches a V3 CHALLENGE
cycle over the seven lenses, expecting all seven lenses to converge to
≥95 % per the §3 fold packet sizing (CH4 at 2-cycle LOCK already
satisfied; CH1/CH3/CH5/CH6 expected first ≥95 % chain confirmation;
CH2/CH7 expected to clear floor on V3 first ≥95 % cycle).

### §4.3 — V3 CHALLENGE-CONTEXT executable-verification mandate hardening

V3 dispatch context must extend the V2 CHALLENGE-CONTEXT.md §3
"Executable verification mandate" to include: *any cite-bearing edit —
including forward-carried cites from prior cycles — must re-execute the
verification (grep / find / line read) at the new HEAD before commit*.
This closes the V2 CH7 failure-mode loophole and operationalises the
LAC-1E-12 procedural addendum (§3.6 proposal) at the dispatch contract
level pending T-P3 §3C governance disposition. NEW-CH2-V2-03 discipline
rule (any "N grammar-named X" subtract-from-K cite must enumerate the K
neutrals with `path:line` inside the cited window) lands alongside as a
generalisation of the mechanical-extraction defect class.

## §5 — §3Z gate evaluation

### §5.1 — V2 cycle gate state

Per `ORCHESTRATOR.md §3Z` (≥95 % × 2 cycles, zero orphan REVISEs):

- V2 is the **second** cycle (V1 = first cycle, did not reach ≥95 %).
- Sub-axis aggregate **94.8 %** (73/77) **just below** floor; per-lens
  mean **94.0 %** (658.4/7) just below floor. Both aggregation methods
  fail by < 1 pp.
- **4 orphan REVISEs** across 2 lenses (CH2: 1; CH7: 3); CH1/CH3/CH4/
  CH5/CH6 all discharge ACCEPT.
- **CH4 alone** at full 2-cycle LOCK satisfied (V1 100 % + V2 100 %).
- **5 lenses (CH1/CH3/CH5/CH6) + CH4 LOCK** at first ≥95 % cycle on V2
  (6 of 7 lenses at ≥95 %).
- **2 lenses (CH2/CH7)** below ≥95 % floor; binding REVISEs.

**Cycle verdict: NOT-YET-LOCKED — V3-REQUIRED.** V3 must (a) clear the
4 orphan REVISEs (CH2-1 + CH7-3) and (b) drive both aggregates above
the 95 % floor (predicted 100 % on both methods after F-V3-CH2-1 +
F-V3-CH7-1 + F-V3-CH7-2 micro-corrections); only then can the V4 cycle
attempt the "× 2 consecutive cycles" cohort LOCK close.

### §5.2 — V3 forecast (light-fold-only)

With the five light/medium packets (§3.1-§3.5, ~40 min wall total):

| Lens | V1 rate | V2 rate | Expected V3 rate (light-only) | Net |
|---|---:|---:|---:|---|
| CH1 | 62.5 % | 100 % | 100 % (8/8) | F-V3-CH1-1 cosmetic; carry-forward 1E:33-35 routes to V3 CH6 (cross-lens disagreement) |
| CH2 | 60 % | 91.7 % | 100 % (12/12) | F-V3-CH2-1 closes single REVISE on 1C reexport off-by-one |
| CH3 | 66.7 % | 100 % | 100 % (9/9) | No V3 work; verification re-pass at HEAD |
| CH4 | 100 % | 100 % | 100 % (8/8) | No V3 work; 2-cycle LOCK already satisfied |
| CH5 | 42.9 % | 100 % (caveat) | 100 % (12/12) | F-V3-CH5-1 closes ACCEPT-with-caveat housekeeping |
| CH6 | 68.4 % | 100 % | 100 % (19/19) | No V3 work; CH1 cross-lens flag (1E:33-35) may add executive-summary cross-ref for cosmetic CH1 close |
| CH7 | 77.8 % | 66.7 % | 100 % (9/9) | F-V3-CH7-1 + F-V3-CH7-2 close 3 REVISEs; V3 dispatch executable-verification mandate hardening prevents recurrence |

**Expected sub-axis-weighted V3 aggregate:** ≈ 100 % (77/77 if every
fold packet lands cleanly; conservative 97-99 % allowing for any V3
fold introducing one new minor finding).

**Expected per-lens mean V3:** ≈ 100 %.

**V3 outcome under light-fold-only:** all 7 lenses ≥95 %; zero orphan
REVISEs; **first cohort-wide ≥95 % cycle achieved on V3**. V4 then has
the burden of producing the second consecutive cohort-wide ≥95 % cycle
to discharge §3Z "≥95 % × 2 cycles" for the full T-P1 lens cohort.

### §5.3 — V4 forecast and predicted §3Z LOCK close path

Under V3 light-fold-only:

- V4 inherits V3's ≈ 100 % sub-axis aggregate + 100 % per-lens ACCEPT
  on all seven lenses.
- V4 work surface: re-verify the V3 light-fold edits land cleanly at
  new HEAD; surface any new cite drift introduced by V3 textual edits;
  reconfirm the ten executable verifications from CH7 §2.1-§2.7 still
  PASS at V4 HEAD; reconfirm the F-V3-CH7 cite rebind via re-execution
  of the verification mandate (closing the V2 cite-carry loophole at
  the V4 confirming pass).
- V4 expected outcome: ≥95 % on both aggregation methods with zero new
  orphan REVISEs. **§3Z cohort LOCK on the second consecutive ≥95 %
  cycle.** T-P3 §3C dispatch gate opens per `PASS-1-EXCAVATION.md §5`.

**Predicted §3Z LOCK close path: V3 (fold packet + first cohort-wide
≥95 % cycle) → V4 (confirming verification re-pass + second consecutive
≥95 % cycle) → cohort LOCK.** Not the V3 → LOCK shortcut: §3Z requires
the "× 2 consecutive cycles" sub-clause and V2's 94.8 % aggregate
falls just below the floor, so V3 cannot serve as both the convergence
cycle and the × 2 chain entry. V4 is structurally required for cohort
LOCK; V≤5 ceiling honoured (V3+V4 land within the 5-cycle envelope per
`ORCHESTRATOR.md §3Z` ceiling).

**S-P2 trajectory note**: S-P2 cohort tracks to LOCK one cycle earlier
at V3 (per dispatch context). T-P1 and S-P2 LOCK at different cycles
per `[no-deferrals]` discipline — each cohort closes on its own
trajectory; cross-pass synchronisation defers to Pass Omega merge.

### §5.4 — §3Z LOCK criteria for V3+V4 chain

Per `ORCHESTRATOR.md §3Z`, V3+V4 cohort LOCK criteria are:

1. **V3: Sub-axis aggregate ≥95 %** on the post-V3 lens dispositions
   (V2 baseline 94.8 % → V3 target ≥95 %; expected 100 % per §5.2).
2. **V3: Zero orphan REVISEs.** Every REVISE finding from V2 must
   either (a) resolve to ACCEPT in V3 OR (b) reclassify to a
   non-orphan verdict. The 4 V2 REVISEs (CH2-1 + CH7-3) all resolve to
   ACCEPT via the §3 fold packet.
3. **V3: No new orphan REVISEs surfaced by V3 work.** V3's textual
   edits to the four inventories must not introduce new cite drift, new
   tally inconsistency, or new closure-wording-without-evidence. CH7
   §2.5 fake-pattern recurrence scan and §2.6 audit-zero baseline
   propagation scan should re-run cleanly at V3 commit; the V3
   dispatch-context executable-verification mandate hardening (§4.3)
   prevents recurrence of the V2 failure mode.
4. **V4: Confirming ≥95 % cycle** with all 7 lenses re-ACCEPT and zero
   new REVISEs.

**Predicted V3 lens ACCEPT-rate vector for cohort first ≥95 % cycle:**
CH1 100, CH2 100, CH3 100, CH4 100 (2-cycle LOCK already satisfied),
CH5 100, CH6 100, CH7 100 (all seven lenses). Sub-axis-weighted
aggregate ~100 %; per-lens mean 100 %.

**Likely V3 → V4 → LOCK trajectory:** V3 closes 4 REVISEs + 1
housekeeping caveat + 1 cosmetic in ~40 min wall + one CHALLENGE V3
dispatch cycle; V4 re-runs the seven CHALLENGE V3 lenses against the
V3-folded artefacts (expected ~30-60 min wall lens dispatch + ~30 min
aggregator); V4 ratifies the second consecutive ≥95 % cycle and
discharges §3Z cohort LOCK. T-P3 §3C dispatch context can be staged in
parallel with V4 to land immediately on V4 LOCK.

## §6 — Sources

V2 lens dispositions (all verified existing at write-time):
- `restart/audit/totality/p1/hardening/V2/CH1.md` (341 lines)
- `restart/audit/totality/p1/hardening/V2/CH2.md` (112 lines)
- `restart/audit/totality/p1/hardening/V2/CH3.md` (89 lines)
- `restart/audit/totality/p1/hardening/V2/CH4.md` (202 lines)
- `restart/audit/totality/p1/hardening/V2/CH5.md` (158 lines)
- `restart/audit/totality/p1/hardening/V2/CH6.md` (73 lines)
- `restart/audit/totality/p1/hardening/V2/CH7.md` (291 lines)
- `restart/audit/totality/p1/hardening/V2/CHALLENGE-CONTEXT.md` (37 lines)

V2 T-P1 inventory artefacts under review (all at HEAD `87816a2cd`):
- `restart/audit/totality/p1/1A-substrate-evidence.md` (113 lines; +1
  from V1; 1A-SUB-014 verdict downgrade + 1A-DIV-008 substrate-union
  nuance disposition with T-P3 §3C conditional ratification rule;
  carries V1 CH5-004 cite cluster verbatim — F-V3-CH7-1 target)
- `restart/audit/totality/p1/1B-codegen-evidence.md` (116 lines; D8/D10
  row split across 3 tables with distinct path:line + CH2 upstream
  blocker stamp; CH7 ACCEPT — no V3 work)
- `restart/audit/totality/p1/1C-runtime-evidence.md` (205 lines; +2 from
  V1; parser-name leak 19+→30; reexports 60+→126 — F-V3-CH2-1 target;
  LOC repair rescale ~50→~190 LOC + 2.5× consumer-rewire band)
- `restart/audit/totality/p1/1D-skinny-lessons.md` (182 lines; +21 from
  V1; 7 V2 sub-folds; CH3 counting/taxonomy re-stratification;
  substrate-union T-P3 §3C conditional at row 117 — F-V3-CH1-1 + F-V3-CH5-1
  targets)
- `restart/audit/totality/p1/1E-locks-evidence.md` (166 lines; +4 from
  V1; LAC-1E-12 promoted to "candidate-promoted-to-T-P3-§3C-priority"
  + §1.5 governance-signal block; meta-CH7 collision acknowledgement)
- `restart/audit/totality/p1/1F-coherence-scan.md` (127 lines; +10 from
  V1; COH-011 nine-grammar + google_sheets=10 + COH-012 anti-fabrication
  template; 7-key divergence_count schema; CH7 ACCEPT — no V3 work)
- `restart/audit/totality/p1/1F-anti-pattern.md` (123 lines; +3 from V1;
  AP-016 google_sheets=10; NEW AP-020 CSS source-sidecar row;
  AP-010 verdict strengthened — F-V3-CH7-1 + F-V3-CH7-2 targets)
- `restart/audit/totality/p1/1F-past-corpora.md` (159 lines; PC-017
  google_sheets=10; PC-008 anchor reinforcement + U-PC-002 cross-cite;
  PC-001/002/004 verdict parity with PC-003; CH7 ACCEPT — no V3 work)

V1 baseline aggregator:
- `restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md`
  (657 lines; V1 ACCEPT-rate 68.6 % sub-axis / 68.3 % per-lens; 22 REVISE
  findings clustered into 11 fold-class groups; V2 fold packet §2.1-§2.11
  discharged by V2 atomic micro-fold at HEAD `87816a2cd`)

Binding authorities (carry-forward from V1):
- `restart/prompts/totality/PASS-1-EXCAVATION.md §3 + §5` (CH1-CH6
  specialisations; §5 convergence + T-P3 dispatch gate)
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens registry)
  + `§3Z` (≥95 % × 2 consecutive cycles + zero orphan REVISEs convergence
  rule; V≤5 cohort ceiling)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
  lens definition; the SK-V14 contract relies on it as the de facto CH7
  binding source since LOCKS.md is silent on CH7 — LAC-1E-12 anti-
  fabrication phrasing template)
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md` + `HANDOFF.md` (SK-V14
  contract; R1-R10 + C-1..C-5 + PRUNE-1..PRUNE-7)
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  (S-P0 prune list; 74 findings; google_sheets=10 baseline at `:210`)
- `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-audit-overfit-pre-restart-pattern.md:46-56,153`
  (S-P0 A6 Pattern H baseline; google_sheets=10)
- `restart/locks/LOCKS.md` (governance text; verified zero CH7/Overfit
  references via `grep -n` at V2 dispatch + at every V2 lens dispatch
  time; LAC-1E-12 anti-fabrication phrasing template)
- `restart/ARCHITECTURE.md:764-765` (nine extant grammars enumeration;
  verified at V2 HEAD; matches `find crates/core/src/runtime -mindepth
  1 -maxdepth 1 -type d | wc -l` = 9)

On-disk artefacts verified at V2 lens dispatch time (per CHALLENGE-CONTEXT
§3 executable-verification mandate; CH7 §2.1-§2.7 + CH1 §Executable
Verification Output path-existence pass):
- `crates/core/src/runtime/` (9 grammar directories, 67 hand-written .rs
  files; `find` reproduces 1C `:38` baseline byte-for-byte; per-grammar
  census `8+7+7+7+7+7+10+7+7=67` arithmetically reconciles)
- `crates/core/src/runtime/mod.rs:25-77` (53-line reexport block;
  mechanical extraction returns 133 raw `pub use` entries inside 25-71
  + 4 additional inside 72-77; 10 grammar-neutral exports across full
  block — F-V3-CH2-1 repair target)
- `crates/core/src/runtime/google_sheets/` (10 files: `arena.rs,
  builder.rs, document/canonical.rs, document/mod.rs, document/path_query.rs,
  document/view.rs, mod.rs, parse_with.rs, value.rs, view.rs`)
- `cargo metadata --format-version 1 --no-deps | jq '.metadata.bbnf.grammars'`
  (9 grammars; CH7 §2.4 PASS at V2 HEAD)
- `grep -n "CH7\|Overfit" restart/locks/LOCKS.md` (zero hits; CH7 §2.1
  + CH1 §Executable Verification Output PASS at V2 HEAD; COH-012
  anti-fabrication template verified)
- `grep -c '@generated by skinny bbnf-codegen' restart/audit/totality/p1/1*.md`
  (zero productive admits; 1 diagnostic citation in 1E:79 of the real
  `// @generated` header at `skinny/crates/runtime/src/grammars/json/parser.rs:1`;
  CH7 §2.6 PASS)
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` (3,644 LOC at V2 HEAD;
  `fixture_sidecar_facts` callsite at `:648`, definition at `:2691`;
  `same-plane-source-sidecar` writers at `:1082, 1203, 1354, 1511, 1661,
  1815, 1964`; `lightningcss_facts` zero hits — F-V3-CH7-1 + F-V3-CH7-2
  rebind targets)
- `skinny/crates/bbnf-bench/src/track2/json.rs:7,26,45` (HEAD line
  positions; V2 cites at `:5,24,43` off-by-2 — F-V3-CH5-1 target)
- `skinny/crates/runtime/src/lib.rs:29-33` (cfg-gated `pub mod
  json_event_grammar_witness` + `pub mod sheets_witness`; V2 cite at
  `:9` drifts — F-V3-CH5-1 target)
