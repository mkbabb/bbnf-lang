---
lens: CH2-GENERALITY
pass: T-P3-synthesis
cycle: V2
reviewer: CH2 GENERALITY (V2)
generated_at: 2026-05-29T21:10:00Z
master_head: 2a76916ac
scope: 3A + 3B + 3C (+3c-locks-v+1-diff) + 3E — Lock-14 generality discipline
mandate: Lock 14 holds across 3A/3B/3E; the tape/ValueRef/NEON generalise to JSON/CSS/Sheets/BBNF-self; 3C accepts no CSS-narrowing amendment.
verdict: 11 ACCEPT, 0 REVISE, 0 REJECT (11 dispositioned sections)
prior_cycle: V1 — 8 ACCEPT, 3 REVISE (S9, S10, S11), 0 REJECT
prior_revise_fold_status: all 3 V1 REVISEs folded into V2 — ACCEPT
---

# CH2 GENERALITY — T-P3 SK-V17 cycle V2

## Lens posture

CH2 scans whether Lock 14 holds across the synthesis: do 3A's surface deltas
and 3B's wave reconciliation generalise to non-JSON; is 3E's grammar-
generalisation story concrete for CSS L4 / Sheets / BBNF-self; does 3C accept
no amendment that narrows a lock to JSON (or CSS); does the future-grammar
onboarding test survive. The firewall is against JSON/CSS-narrowing and against
asserted-not-proven generality. V2 is a fold cycle: CH2's V1 returned three
REVISEs (S9 predicted-cell tags, S10 P5a/P5b classifier-vs-tape split, S11 P6
value-axis firewall); this pass verifies the folds discharge them and re-scans
the full artefact set for any new generality regression.

## V1 REVISE fold verification (the load-bearing V2 check)

CH2-V1's three REVISEs were generality-precision defects — each a place where a
TRUE underlying fact was presented one notch beyond what HEAD proves. All three
are folded; the by-exercise/by-construction boundary is now crisp at the cell/
prose level.

| V1 REVISE | required fold | V2 status | evidence |
|---|---|---|---|
| **S9** — 3E matrix Sheets/BBNF-self *dominant-shape* cells assigned concrete shapes (`OffsetTape`/`EventTape`/`EagerTape`) read as proven, but `derive_backend_shape` is skinny-only/unwired to those grammars | tag the non-witnessed shape cells `predicted (cost-model-pending)` so the boundary is crisp at the CELL level, not only the status column | **FOLDED → ACCEPT** | 3e:117-126 "Cell-level provenance discipline" para added; 7 `*(predicted, cost-model-pending)*` cell tags across every non-JSON/non-CSS matrix row (`grep -c 'predicted, cost-model-pending' = 7`). A reader scanning the dominant-shape column reads `predicted` on every non-witnessed row and proven shapes only on JSON+CSS (3e:124-126). |
| **S10** — onboarding P5 "CSS L4 binds the eq-set fan NEON body … a real measured non-JSON consumer" conflated CSS *classifier-scan* (wired/measured) with CSS *tape-consumer* (the SK-V18 fold-target; `SK17L-008` "the residual gap is the missing TAPE CONSUMER, not the scan") | split P5 into P5a classifier-scan-measured / P5b tape-consumer-SK-V18-pending | **FOLDED → ACCEPT** | 3e:173 P5a "CSS L4 **classifier scan** is wired + measurable … Lock 16 ≥1 non-JSON consumer SATISFIED"; 3e:174 P5b "CSS L4 **tape consumer** is the SK-V18 fold-target, NOT yet measured … the classifier-scan measurement (P5a) does NOT imply the tape-consumer chain is measured". The over-claim is severed. |
| **S11** — §12 onboarding gave no GENERALITY falsifier that a NEW grammar rides `ValueRef<G>` without a generator branch; the value-plane re-emit firewall (JSON byte-equal) was absent from 3E's predicate set | add P6 value-plane generality falsifier — one `ValueRef<G>` generator re-emits JSON byte-equal AND CSS lazy from one walk; a `match grammar` arm or CSS-specific value branch FAILS | **FOLDED → ACCEPT** | 3e:175 P6 added (NEW); 3e:177-184 fail-closed condition extended with the value-axis firewall; the onboarding test now carries TWO orthogonal generality axes — classifier/leak (P1/P3) and value-plane (P6); "a grammar that passes the classifier axis but needs a generator value-branch still FAILS Lock 14" (3e:183-184). |

The V2 frontmatter `prior_cycle_dispositions_folded.revised` block (3e:33-37)
enumerates all five CH-V1 REVISEs targeting 3E (the three CH2 + CH4-03 +
CH6-08); each is addressed in-place, no delta overturned, no new delta added.
CH6-08 (EBNF/BNF/CSV/math DEFER triple) is verified at 3e:140 carrying the full
`receiver = SK-V18 onboarding wave; blocker = no structural_index/scan witness
for math; gate = Lock-14 future-grammar onboarding test, 3E17-D07` triple.

## Live verification performed (master HEAD 2a76916ac)

| claim | source | live check | result |
|---|---|---|---|
| `ValueRef<'doc,'input,K,G:EventGrammar>` is a grammar TYPE param, zero runtime branch | 3E17-D01; `tape/mod.rs:175` | `sed -n 173,178p skinny/crates/runtime/src/tape/mod.rs` | CONFIRMED — `pub struct ValueRef<'doc,'input:'doc, K=AnyKind, G:EventGrammar=AnyGrammar>` with `_kind: PhantomData<fn()->K>`; monomorphised, no `match grammar`. |
| classifier wired across 8 grammars (config-breadth, non-JSON proven), math=0 | 3E17-D04; SK17L-008 | `grep -c scan_structural crates/core/src/grammar/generated/{json,ebnf,bnf,csv,css_l4,css_pretty,google_sheets,bbnf,math}.rs` | CONFIRMED — 8×`=1`, math`=0`. Non-JSON classifier generality is by-EXERCISE across 8 grammars, not assertion; math's `=0` is exactly the DEFER blocker the matrix cites (3e:140). |
| eq-set fan is the ONE real NEON body; table is a scalar delegate | 3E17-D04/D06, L14-17-HC-04 | `wc -l + grep neon-intrinsics aarch64/byte_class_from_eq_set_64.rs`; `head aarch64/byte_class_from_table_64.rs` | CONFIRMED — eq-set fan 87 LOC / 14 NEON intrinsics; `byte_class_from_table_64_neon` is a 4-LOC delegate to `byte_class_from_table_64_scalar`. CSS non-JSON consumer binds the REAL body via `;{` slot-59, not the passthrough. |
| `sheets_witness` cannot serve as a value-plane projection exercise | 3E17-D07, SK17L-009 | `sed -n 110,114p restart/skinny/tranches/sk-v17/SPEC.md` | CONFIRMED — SPEC §0.1.11: "Projection generality exercised by-construction on JSON + CSS only; … `sheets_witness` has no `BackendRule` shape and cannot serve as an SK-V17 projection exercise". Scoping clause well-grounded. |
| P1 HEAD leak baseline = 7 hits in strategy.rs (catalogued, NOT a clean pass) | 3E P1 predicate | `rg -c 'JsonParser\|CssL4Parser' crates/ir/src crates/simd-scan/src` | CONFIRMED — 7 hits, all `crates/ir/src/registry/strategy.rs`; string-ident registry + doc-comments, NOT runtime `match grammar {}`. Monotonic-decrease-to-zero, published-baseline falsifier (not a present-tense clean gate). |
| 5-shape canon string holds verbatim across 3A/3B/3C/3E/3c-diff | §8.2 | `grep -coE 'EagerTape…CollapsedStage' …/3{a,b,c-crystallisation,e}.md + 3c-locks-v+1-diff.md` | CONFIRMED — canon string present in 3a(1)/3b(2)/3e(1)/3c-diff(4); 3c-crystallisation references "5-shape" at 8 sites. Every "6th/sixth shape" mention across all artefacts is a negation/fence/G-Omega-gate (verified V1, re-confirmed). |
| no-6th-shape stands on a second mechanical ground (aarch64 refusal) | 3A-D04, 3C Lock-10, 3E17-D05 | `grep admits_collapsed_stage restart/ARCHITECTURE.md` | CONFIRMED — `admits_collapsed_stage` binds CollapsedStage to `target.arch == x86`; mechanically refused on aarch64 M5 Max. The categorical precedent (LAC-1E-14 FactStream) + the arch-binding are two independent grounds. |
| 3C narrows no lock to CSS/JSON; fleet-wide value-plane is REFUTED, not used to narrow | mandate | `grep -niE 'narrow\|fleet-wide' …/3c-locks-crystallisation.md` | CONFIRMED — 3c:97 Lock 14 disposition is scope-HONEST (value-fold JSON+CSS by-exercise, classifier 8-grammar config-breadth, "never asserted fleet-wide", "no grammar branch enters any generic crate"); 3c:180 "Fleet-wide value-plane proof — REFUTED" folds into the Lock 14 clause WITHOUT narrowing — the lock stays grammar-NEUTRAL. |

## Two-axis generality model holds (V2 sharpened)

The synthesis carries a two-axis generality model and, after the V2 folds, holds
both axes honestly at the cell/prose/predicate level:

- **Classifier axis (NEON `select_classifier(alphabet)`)** — generality is
  breadth-of-CONFIG, proven **by-exercise** across 8 grammars at HEAD
  (`scan_structural=1` in 8 generated parsers, math=0; alphabet-as-data). The
  strongest non-JSON generality evidence in the packet. 3E17-D04 + L14-17-HC-04
  bind CSS L4 to the REAL eq-set fan body (87-LOC/14-intrinsic), not the 4-line
  scalar-delegate. P5a now states this is the wired/measured axis; P5b states the
  CSS tape-consumer chain is the SK-V18 fold-target (NOT measured). The S10 fold
  removed the conflation.

- **Value-plane axis (`ValueRef<G>` projection)** — generality is breadth-of-
  PROOF, exercised **by-construction JSON+CSS only**; Sheets/BBNF-self are SK-V18
  by-construction (`sheets_witness` is a stub with no `BackendRule`). The matrix
  Sheets/BBNF-self/EBNF/BNF/CSV/math shape cells are now tagged `predicted
  (cost-model-pending)` (S9 fold). The scoping clause (3E17-D07 / L14-17-HC-07)
  and the new P6 firewall (S11 fold) prevent the value-fold's generality from
  being mis-stated fleet-wide — "a new grammar that requires a generator branch
  to ride `ValueRef<G>` FAILS Lock 14".

Keeping these two axes DISTINCT is the discipline that makes Lock 14 honest
rather than paper-general. 3C's Lock 14 clause states this split verbatim
(3c:97); the 3c-locks-v+1-diff Lock 14 hunk carries it into the proposed LOCKS
text (`The shared classifier's grammar-generality is config-breadth … a SEPARATE
axis from the value-fold, never the same as fleet-wide value-plane proof`).
**No CSS-narrowing; no JSON-narrowing.**

## Anti-narrowing verification across 3A/3B (firewall, not over-claim)

Every "fleet-wide" occurrence in 3A/3B is a NEGATION — the firewall against the
over-claim, never the over-claim itself:
- 3a:76 D02 consequence: "the value-fold is JSON+CSS-exercised, **not falsely
  claimed fleet-wide**".
- 3b:102 H.W4.LOCK14: "GrammarConfig legality is evidence, **not fleet-wide Lock
  14 closure** … Sheets/BBNF-self projection generality stays SK-V18 proof, not
  SK-V17 claim".
- 3b:186 CH2 open question: "what exact non-JSON onboarding test does MP.SK18.W3
  require **before** Lock 14 claims fleet-wide closure".
The P6 firewall correctly catches the "generic-named-CSS-generator failure mode"
(SPEC:62: "a generator that leaves JSON's hand-written path untouched … FAILS") —
the anti-CSS-narrowing guard for the value plane.

## Non-JSON generality story is concrete for Sheets / BBNF-self (CH2 deliverable)

The 3E matrix (3e:128-141) carries concrete predicted-shape cells for Sheets
(`formula`/`cell_ref` OffsetTape; `LET`/`LAMBDA` EventTape — anti-JSON falsifier,
"no JSON object/array role reuse"; infix EagerTape/Pratt) and BBNF-self
(`grammar`/`declaration` OffsetTape; directives EventTape+SinkOnly; expression
EagerTape/Pratt), each `(predicted, cost-model-pending)` with grammar-source
file:line evidence (`grammar/google-sheets/google-sheets.bbnf`,
`grammar/bbnf/bbnf.bbnf`). The Primitive Vocabulary Transfer table (3e:150-158)
carries CSS-proven / Sheets-by-construction / BBNF-self-by-construction columns
for each primitive family. The future-grammar onboarding test (3e:160-187)
survives with a live HEAD baseline and TWO orthogonal generality axes. **The CH2
deliverable — a concrete non-JSON story for CSS L4 / Sheets / BBNF-self — is
met.**

## Section dispositions

| # | section | V1 | V2 | rationale |
|---|---|---|---|---|
| 1 | 3A-D02 lazy `ValueRef<G>` value-plane | ACCEPT | **ACCEPT** | Names the grammar-parametric projection as the materialization plane; fail action REJECTs "CSS-only generator that never re-emits JSON" (3a:93) — JSON byte-equal re-emit is the generality firewall. Grammar-neutral, scope-honest (3a:76 "not falsely claimed fleet-wide"). |
| 2 | 3A-D04 BackendShape-category disposition | ACCEPT | **ACCEPT** | Tape = substrate-manifest category, 5-shape canon verbatim, no 6th shape; two independent grounds verified (LAC-1E-14 precedent + `admits_collapsed_stage` x86-binding). Coherent with 3B §13 and 3E matrix (§8.2). |
| 3 | 3B H.W4.LOCK14 wave row | ACCEPT | **ACCEPT** | "Every fold design is grammar-neutral (Lock 14)"; `begin_compound` reads `layout.rule_id & 0x1F` only; classifier config-breadth cited; Sheets/BBNF-self kept SK-V18, not SK-V17 claim. Generalises. |
| 4 | 3B-D03 tape-as-substrate-category MASTER coherence | ACCEPT | **ACCEPT** | Restates the FactStream precedent verbatim; 5-shape canon STAYS UNCHANGED across §13/§13.5/§13.1; 6th variant G-Omega-gated. Cross-surface coherence holds. |
| 5 | 3C Lock 14 ValueRef/classifier-generalisation clause | ACCEPT | **ACCEPT** | Crystallisation does NOT narrow Lock 14 to CSS/JSON. Value-fold JSON+CSS by-exercise; classifier 8-grammar config-breadth; "may not be claimed fleet-wide"; "No grammar branch … enters any generic crate". The mandate's central requirement — 3C accepts no CSS-narrowing — is SATISFIED. Fleet-wide proof REFUTED (3c:180), not used to narrow. |
| 6 | 3C Lock 10 tape-category clause + 3c-locks-v+1-diff Lock-10 hunk | ACCEPT | **ACCEPT** | Five-shape Lock-10 search domain held verbatim (4 canon-string hits in the diff); substrate-category placement explicit; 6th variant G-Omega-gated + SK-V17 §9-barred. No grammar-narrowing. |
| 7 | 3C Lock 16 NEON-classifier-manifest clause | ACCEPT | **ACCEPT** | Registers `select_classifier(alphabet)` as a manifest row with the alphabet as grammar-policy DATA; CSS L4 the non-JSON same-wave consumer binding the real NEON body; honest scalar-delegate disposition for table/prefix. aarch64-only, no x86 close. Generality-preserving. |
| 8 | 3E17-D04/D06 + L14-17-HC-04 alphabet-as-data + CSS non-JSON consumer | ACCEPT | **ACCEPT** | The single strongest generality claim, re-verified: 8-grammar classifier wiring + the real eq-set fan body (87/14) + the `;{` slot-59 collision route. CSS binds a REAL NEON body, not the 4-LOC delegate. By-exercise non-JSON generality. |
| 9 | 3E Per-Grammar BackendShape Matrix — Sheets/BBNF-self/EBNF/BNF/CSV/math cells | **REVISE** | **ACCEPT** | S9 FOLDED: 7 `predicted (cost-model-pending)` cell tags + "Cell-level provenance discipline" para (3e:117-126); by-construction boundary crisp at the CELL level. EBNF/BNF/CSV/math DEFER carries the receiver/blocker/gate triple (3e:140). |
| 10 | 3E Future-Grammar Onboarding Test P5 classifier-scan-vs-tape-consumer | **REVISE** | **ACCEPT** | S10 FOLDED: P5 split into P5a (classifier scan wired/measured, Lock-16 ≥1-non-JSON-consumer SATISFIED) / P5b (tape consumer SK-V18 fold-target, NOT measured); the conflation severed (3e:173-174). |
| 11 | 3E17-D08 + onboarding tape predicates — value-axis falsifier | **REVISE** | **ACCEPT** | S11 FOLDED: P6 value-plane generality firewall added (3e:175); fail-closed condition extended with the value-axis firewall (3e:177-184); the onboarding test now carries TWO orthogonal generality axes (classifier/leak P1/P3 + value-plane P6). |

## Counts

- Sections dispositioned: **11**
- ACCEPT: **11**
- REVISE: **0**
- REJECT: **0**
- ACCEPT rate: **100%** (≥95% convergence threshold met for V2)
- V1 REVISEs folded: **3/3** (zero orphan REVISE)

## Note on the all-ACCEPT V2 verdict

V1 returned 27% REVISE (3 generality-precision defects). All three are folded
with verified evidence at the cell/prose/predicate level — not paper-folded: the
predicted-cell tags, the P5a/P5b split, and the P6 value-axis firewall each
discharge a specific over-claim. No new generality defect surfaced under the full
V2 re-scan: the 5-shape canon holds verbatim across 3A/3B/3C/3E; every "fleet-
wide" / "6th-shape" mention is a firewall negation; 3C narrows no lock to JSON or
CSS (the fleet-wide value-plane proof is REFUTED, not used to specialise); the
classifier axis (8-grammar by-exercise) and value-plane axis (JSON+CSS by-
construction) stay distinct; Sheets/BBNF-self have concrete predicted cells and
the future-grammar onboarding test survives with a live HEAD baseline. CH2's
mandate is satisfied: **Lock 14 holds across 3A/3B/3E; tape/ValueRef/NEON
generalise (classifier by-exercise, value-plane by-construction); 3C accepts no
CSS-narrowing.** A V2 all-ACCEPT here is convergence on a genuinely-folded prior
cycle, not paper-close — each ACCEPT names the fold artefact line that discharged
the V1 REVISE.

## Open questions tagged forward

| q | receiver | gate |
|---|---|---|
| Should the SK-V18 W3 onboarding gate require a NON-CSS-non-JSON value-plane witness (Sheets OR BBNF-self riding `ValueRef<G>` without a generator branch) before any fleet-wide Lock 14 wording, or is the witnessed-grammars scoping rule (3E17-D07) sufficient indefinitely? | SK-V18 W3 owner (3B) + 3E | W3 byte-equal re-emit gate (P6) + the witnessed-grammars scoping rule; no fleet-wide wording without ≥1 non-JSON-non-CSS value-plane witness. |
| If only one of Sheets or BBNF-self fits the SK-V18 cap, which is the first non-CSS witness — Sheets (function/reference/operator materialisation) or BBNF-self (import/directive recursion)? | SK-V18 wave owner (3B/G-Omega); 3E carries both as mandatory falsifiers | SK-V18 onboarding plan gate with source/metadata-only diff + Lock 14 scan; the witnessed-grammars scoping rule (3E17-D07). |
| Does the `ValueRef<G>` generator re-emit ALL per-grammar value surfaces from one `BackendRule` walk, or do CSS colour-function/calc semantics force a generator branch (a Lock-14 violation)? | SK-V18 W3 generator owner | onboarding predicate P6 (value-axis firewall): re-emit JSON byte-equal AND CSS lazy from one walk; any `match grammar` arm or CSS-specific value branch FAILS. |
