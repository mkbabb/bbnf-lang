---
lens: CH2-GENERALITY
pass: T-P3-synthesis
cycle: V3
reviewer: CH2 GENERALITY (V3)
generated_at: 2026-05-29T22:40:00Z
master_head: 2a76916ac
scope: 3A + 3B + 3C (+3c-locks-v+1-diff) + 3D + 3E — Lock-14 generality discipline
mandate: Lock 14 holds across 3A/3B/3E; the tape/ValueRef/NEON generalise to JSON/CSS/Sheets/BBNF-self; 3C accepts no CSS-narrowing amendment.
verdict: 11 ACCEPT, 0 REVISE, 0 REJECT (11 dispositioned sections)
prior_cycle: V2 — 11 ACCEPT, 0 REVISE, 0 REJECT (S9/S10/S11 V1 REVISEs folded in V2)
prior_revise_fold_status: zero CH2-directed REVISE outstanding entering V3; the one V3-new fold (CH5-V2-R02 leak-census receiver) is generality-neutral and re-verified
convergence_note: second consecutive ≥95% ACCEPT cycle (V2=100%, V3=100%) — CH2 convergence threshold met
---

# CH2 GENERALITY — T-P3 SK-V17 cycle V3

## Lens posture

CH2 scans whether Lock 14 holds across the synthesis: do 3A's surface deltas
and 3B's wave reconciliation generalise to non-JSON; is 3E's grammar-
generalisation story concrete for CSS L4 / Sheets / BBNF-self; does 3C accept
no amendment that narrows a lock to JSON (or CSS); does the future-grammar
onboarding test survive. The firewall is against JSON/CSS-narrowing and against
asserted-not-proven generality.

V3 is the convergence-confirmation cycle. CH2-V2 returned 11 ACCEPT / 0 REVISE
after folding the three V1 generality-precision REVISEs (S9 predicted-cell tags,
S10 P5a/P5b classifier-vs-tape split, S11 P6 value-axis firewall). The single
3X-touching delta in V3 is the **CH5-V2-R02** fold — the P1 grammar-name
leak-census receiver routed to MP.SK18.W3 (3E author, not CH2-owned). CH2's V3
duty is twofold: (1) re-verify that the V2-folded generality discipline still
holds at HEAD `2a76916ac` after the V3 re-authoring of all six 3X artefacts;
(2) confirm the CH5-V2-R02 receiver-binding introduces **no** generality
regression — that routing the leak-census to a named wave keeps Lock 14
grammar-NEUTRAL and does not narrow the falsifier to JSON/CSS.

## V3 re-scan: V2 folds persist at HEAD (no regression on re-author)

All six 3X artefacts were re-authored at cycle V3 (`grep cycle: V3` = all six).
CH2 re-ran every load-bearing V2 fold check; all persist:

| V2 fold | V3 status | live re-verification (HEAD 2a76916ac) |
|---|---|---|
| S9 — Sheets/BBNF-self/EBNF/BNF/CSV/math shape cells tagged `predicted (cost-model-pending)` | **HOLDS** | `grep -c 'predicted, cost-model-pending' 3e.md = 7`; the Cell-level provenance discipline para survives at 3e:134-143; the dominant-shape column reads `predicted` on every non-witnessed row, proven shapes only on JSON+CSS. |
| S10 — P5 split into P5a classifier-scan-measured / P5b tape-consumer-SK-V18-pending | **HOLDS** | 3e:190 P5a "CSS L4 **classifier scan** is wired + measurable … Lock 16 ≥1 non-JSON consumer SATISFIED"; 3e:191 P5b "CSS L4 **tape consumer** is the SK-V18 fold-target, NOT yet measured". The conflation stays severed. |
| S11 — P6 value-plane generality firewall (NEW in V2) | **HOLDS** | 3e:192 P6 carries "a generator with a `match grammar` arm or a CSS-specific value branch FAILS"; the fail-closed condition (3e:194-201) carries TWO orthogonal axes — classifier/leak (P1/P3) and value-plane (P6). |
| math/EBNF DEFER triple (CH6-V1-08) | **HOLDS** | 3e:157 carries the full `receiver = SK-V18 onboarding wave; blocker = no structural_index/scan witness for math; gate = Lock-14 future-grammar onboarding test, 3E17-D07` triple; live `scan_structural` census confirms math=0 (the exact cited blocker). |

The V3 re-author did not paper over any folded REVISE. The generality
discipline is preserved verbatim in substance.

## CH5-V2-R02 fold is generality-neutral (the V3-new check)

The V3-new content is the P1 leak-census receiver-binding (CH5-V2-R02). It is a
CH5 (hidden-coupling) obligation, but it touches the Lock-14 onboarding
predicate set, so CH2 must confirm it carries no generality regression. It does
not:

- The 7 `strategy.rs` sites are **string-ident registry keys + doc-comments,
  not runtime `match grammar {}` arms** — live-verified: `rg 'JsonParser|CssL4Parser'
  crates/ir/src crates/simd-scan/src` returns exactly 7 hits, all at
  `crates/ir/src/registry/strategy.rs:{132,137,149,197,198,292,315}`; every one
  is a `///` doc-comment or an `idents: &[…]` registry literal. None is a
  runtime grammar dispatch. The leak is coupling-honest, not coupling-active
  (3e:213-217; `2c:279-281`).
- Routing the decrease to **MP.SK18.W3** (the `ValueRef<G>` projection generator
  wave, live-confirmed at `3b:147`) binds the falsifier to a named owner WITHOUT
  narrowing it to JSON/CSS. The predicate stays "grammar-name leak in generic
  crates returns ZERO" — a grammar-NEUTRAL falsifier with a monotonic-decrease
  target and a fail-closed admitted baseline + re-entry trigger (next SK-V18
  onboarding-wave leak-census gate). This STRENGTHENS Lock 14 (the unowned
  "HEAD → 0" is eliminated) rather than weakening or specialising it.

CH2 finding: the CH5-V2-R02 fold is a generality-positive change. No CH2-axis
regression.

## Live verification performed (master HEAD 2a76916ac)

| claim | source | live check | result |
|---|---|---|---|
| `ValueRef<'doc,'input,K,G:EventGrammar>` is a grammar TYPE param, zero runtime branch | 3E17-D01; `tape/mod.rs:175` | `sed -n 175,182p skinny/crates/runtime/src/tape/mod.rs` | CONFIRMED — `pub struct ValueRef<'doc,'input:'doc, K=AnyKind, G:EventGrammar=AnyGrammar>` with `_grammar: PhantomData<fn()->G>`; monomorphised, no `match grammar`. |
| classifier wired across 8 grammars (non-JSON by-exercise), math=0 | 3E17-D04; SK17L-008 | `grep -c scan_structural crates/core/src/grammar/generated/{json,ebnf,bnf,csv,css_l4,css_pretty,google_sheets,bbnf,math}.rs` | CONFIRMED — 8×`=1`, math`=0`. Non-JSON classifier generality is by-EXERCISE across 8 grammars; math's `=0` is exactly the DEFER blocker the matrix cites (3e:157). |
| eq-set fan is the ONE real NEON body; table is a 4-LOC scalar delegate | 3E17-D04/D06, L14-17-HC-04 | `wc -l aarch64/byte_class_from_eq_set_64.rs` + `head aarch64/byte_class_from_table_64.rs` | CONFIRMED — eq-set fan = 87 LOC; `byte_class_from_table_64_neon` is a 4-LOC delegate to `…scalar`. CSS binds the REAL body via `;{` slot-59, not the passthrough. |
| P1 HEAD leak baseline = 7 hits in strategy.rs, all string-ident/doc, NOT a clean pass | 3E P1 predicate | `rg -n 'JsonParser\|CssL4Parser' crates/ir/src crates/simd-scan/src` | CONFIRMED — 7 hits, all `crates/ir/src/registry/strategy.rs`, all doc-comment/registry literal. Monotonic-decrease OWNED by MP.SK18.W3; not a runtime `match grammar {}`. |
| MP.SK18.W3 is the `ValueRef<G>` projection generator wave (leak-census receiver) | 3E17-D08, CH5-V2-R02 fold | `sed -n 147p 3b-master-plan-reconciliation.md` | CONFIRMED — 3b:147 "MP.SK18.W3 Lazy `ValueRef<G>` projection generator … ONE grammar-agnostic accessor generator (Lock 14) … JSON+CSS-exercised only — Sheets/BBNF-self by-construction". The receiver resolves. |
| Sheets/BBNF-self matrix cells carry concrete grammar-source evidence | 3E matrix 3e:151-156 | `sed -n '54p;84p;97p;160p' grammar/google-sheets/google-sheets.bbnf`; `sed -n '4p;15p;41p;85p' grammar/bbnf/bbnf.bbnf` | CONFIRMED — cited grammar lines resolve (`cell_or_range`, `array_rows`, `@import`, `grammar = (grammar_item ?w)*`); each cell `(predicted, cost-model-pending)`. Concrete non-JSON story, not assertion. |
| 5-shape canon string holds verbatim across 3A/3B/3C/3E/3c-diff | §8.2 | `grep -cE 'EagerTape, ?OffsetTape, ?EventTape, ?SinkOnly, ?CollapsedStage' …` | CONFIRMED — 3a(1)/3b(2)/3e(1)/3c-diff(4); 3c-crystallisation references "5-shape/five-shape" at 8 sites. Every "6th/sixth shape" mention is a negation/fence/G-Omega-gate/delta-id-name (`tape-category-not-sixth-shape`). |
| no-6th-shape stands on a second mechanical ground (aarch64 refusal) | 3A-D04, 3C Lock-10, 3E17-D05 | `grep admits_collapsed_stage` in 3c-diff Lock-10 clause | CONFIRMED — `admits_collapsed_stage` binds CollapsedStage to x86+avx512bw, mechanically refused on aarch64 M5 Max. LAC-1E-14 precedent + arch-binding = two independent grounds. |
| 3C narrows no lock to CSS/JSON; fleet-wide value-plane is REFUTED, not used to narrow | mandate | `grep -niE 'narrow\|fleet-wide\|config-breadth' 3c-locks-crystallisation.md + 3c-locks-v+1-diff.md` | CONFIRMED — 3c:107 Lock 14 disposition scope-HONEST (value-fold JSON+CSS by-exercise, classifier 8-grammar config-breadth, "never asserted fleet-wide"); 3c:190 "Fleet-wide value-plane proof — REFUTED" folds into the Lock 14 clause WITHOUT narrowing; the v+1 diff Lock 14 hunk (3c-diff:78) carries "a SEPARATE axis from the value-fold, never the same as fleet-wide value-plane proof". The lock stays grammar-NEUTRAL. |

## Two-axis generality model holds (carried, V3 re-confirmed)

The synthesis carries a two-axis generality model and holds both axes honestly
at the cell/prose/predicate level after V3 re-author:

- **Classifier axis (NEON `select_classifier(alphabet)`)** — generality is
  breadth-of-CONFIG, proven **by-exercise** across 8 grammars at HEAD
  (`scan_structural=1` in 8 generated parsers, math=0; alphabet-as-data). The
  strongest non-JSON generality evidence in the packet. 3E17-D04 + L14-17-HC-04
  bind CSS L4 to the REAL eq-set fan body (87-LOC), not the 4-LOC scalar
  delegate. P5a is the wired/measured axis; P5b states the CSS tape-consumer
  chain is the SK-V18 fold-target (NOT measured).

- **Value-plane axis (`ValueRef<G>` projection)** — generality is breadth-of-
  PROOF, exercised **by-construction JSON+CSS only**; Sheets/BBNF-self are
  SK-V18 by-construction (`sheets_witness` is a 24-LOC stub with no
  `BackendRule`). The matrix non-JSON/non-CSS shape cells are tagged `predicted
  (cost-model-pending)`. The scoping clause (3E17-D07 / L14-17-HC-07) and the P6
  firewall prevent the value-fold's generality from being mis-stated fleet-wide.

Keeping the two axes DISTINCT is the discipline that makes Lock 14 honest rather
than paper-general. 3C's Lock 14 clause and the 3c-locks-v+1-diff Lock-14 hunk
both carry the split verbatim. **No CSS-narrowing; no JSON-narrowing.**

## Anti-narrowing verification across 3A/3B/3D (firewall, not over-claim)

Every "fleet-wide" occurrence in 3A/3B/3D is a NEGATION — the firewall against
the over-claim, never the over-claim itself:
- 3a:96 D02 consequence: "the value-fold is JSON+CSS-exercised, **not falsely
  claimed fleet-wide**".
- 3b:107 H.W4.LOCK14: "GrammarConfig legality is evidence, **not fleet-wide
  Lock 14 closure** … Sheets/BBNF-self projection generality stays SK-V18 proof,
  not SK-V17 claim".
- 3d:48,71-73 routes the Sheets/BBNF-self generality gap explicitly to 3E
  (`3D-SK17-D07-sheets-bbnf-generality-gap-to-3E`); the non-JSON gap is
  "explicit-and-monotonic", JSON the only tape WITNESS, Sheets/BBNF-self
  by-construction. 3D never dictates back to the live skinny iteration (monotonic
  fold, §8.4). The scope-honesty banner forces a G3 skim of every Sheets/BBNF-self
  claim.

## Non-JSON generality story is concrete for Sheets / BBNF-self (CH2 deliverable)

The 3E matrix (3e:145-158) carries concrete predicted-shape cells for Sheets
(`formula`/`cell_ref` OffsetTape; `LET`/`LAMBDA` EventTape — anti-JSON falsifier,
"no JSON object/array role reuse"; infix EagerTape/Pratt) and BBNF-self
(`grammar`/`declaration` OffsetTape; directives EventTape+SinkOnly; expression
EagerTape/Pratt), each `(predicted, cost-model-pending)` with grammar-source
file:line evidence (live-verified to resolve). The Primitive Vocabulary Transfer
table (3e:167-175) carries CSS-proven / Sheets-by-construction /
BBNF-self-by-construction columns for each primitive family. The future-grammar
onboarding test (3e:177-204) survives with a live HEAD baseline and TWO
orthogonal generality axes. **The CH2 deliverable — a concrete non-JSON story
for CSS L4 / Sheets / BBNF-self — is met.**

## Section dispositions

| # | section | V2 | V3 | rationale |
|---|---|---|---|---|
| 1 | 3A-D02 lazy `ValueRef<G>` value-plane | ACCEPT | **ACCEPT** | Names the grammar-parametric projection as the materialization plane; fail action REJECTs "CSS-only generator that never re-emits JSON" (3a:113); JSON byte-equal re-emit is the generality firewall. Scope-honest "not falsely claimed fleet-wide" (3a:96). Grammar-neutral. |
| 2 | 3A-D04 BackendShape-category disposition | ACCEPT | **ACCEPT** | Tape = substrate-manifest category, 5-shape canon verbatim, no 6th shape; two independent grounds (LAC-1E-14 precedent + `admits_collapsed_stage` x86-binding). Coherent with 3B §13 and 3E matrix (§8.2). |
| 3 | 3B H.W4.LOCK14 wave row | ACCEPT | **ACCEPT** | "Every fold design is grammar-neutral (Lock 14)"; `begin_compound` reads `layout.rule_id & 0x1F` only; classifier config-breadth cited; Sheets/BBNF-self kept SK-V18, not SK-V17 claim (3b:107). Generalises. |
| 4 | 3B MP.SK18.W3 ValueRef generator wave (leak-census receiver) | ACCEPT | **ACCEPT** | 3b:147 names W3 as ONE grammar-agnostic accessor generator (Lock 14), JSON+CSS-exercised only; a CSS-only generator that never re-emits JSON FAILS CH2. The CH5-V2-R02 receiver-binding resolves to a real wave; the binding keeps the leak-census falsifier grammar-neutral. |
| 5 | 3C Lock 14 ValueRef/classifier-generalisation clause + 3c-diff Lock-14 hunk | ACCEPT | **ACCEPT** | Crystallisation does NOT narrow Lock 14 to CSS/JSON. Value-fold JSON+CSS by-exercise; classifier 8-grammar config-breadth; "never asserted fleet-wide"; "No grammar branch … enters any generic crate" (3c:107; 3c-diff:78). Fleet-wide proof REFUTED (3c:190), not used to narrow. The mandate's central requirement — 3C accepts no CSS-narrowing — is SATISFIED. |
| 6 | 3C Lock 10 tape-category clause + 3c-diff Lock-10 hunk | ACCEPT | **ACCEPT** | Five-shape Lock-10 search domain held verbatim (4 canon-string hits in the diff); substrate-category placement explicit; 6th variant G-Omega-gated + SK-V17 §9-barred; two grounds. No grammar-narrowing. |
| 7 | 3C Lock 16 NEON-classifier-manifest clause + 3c-diff Lock-16 hunk | ACCEPT | **ACCEPT** | Registers `select_classifier(alphabet)` as a manifest row with alphabet as grammar-policy DATA; CSS L4 the non-JSON same-wave consumer binding the real NEON body (87-LOC eq-set fan); honest `scalar-delegate-non-ASM` disposition for table/prefix; aarch64-only, no x86 close. Generality-preserving. |
| 8 | 3E17-D04/D06 + L14-17-HC-04 alphabet-as-data + CSS non-JSON consumer | ACCEPT | **ACCEPT** | The single strongest generality claim, re-verified: 8-grammar classifier wiring + the real eq-set fan body (87) + the `;{` slot-59 collision route. CSS binds a REAL NEON body, not the 4-LOC delegate. By-exercise non-JSON generality. |
| 9 | 3E Per-Grammar BackendShape Matrix — Sheets/BBNF-self/EBNF/BNF/CSV/math cells | ACCEPT | **ACCEPT** | 7 `predicted (cost-model-pending)` cell tags + Cell-level provenance discipline para (3e:134-143) persist after V3 re-author; by-construction boundary crisp at the CELL level; EBNF/BNF/CSV/math DEFER carries the receiver/blocker/gate triple (3e:157); grammar-source citations live-verified. |
| 10 | 3E Future-Grammar Onboarding Test P5a/P5b + P6 two-axis | ACCEPT | **ACCEPT** | P5a/P5b split (3e:190-191) and P6 value-axis firewall (3e:192) persist; the onboarding test carries TWO orthogonal generality axes (classifier/leak P1/P3 + value-plane P6); fail-closed condition extended (3e:194-201). Survives. |
| 11 | 3E17-D08 P1 leak-census receiver (CH5-V2-R02 fold) | n/a (V2 was CH5) | **ACCEPT** | The V3-new fold routes the P1 leak-census decrease to MP.SK18.W3 with a fail-closed admitted Lock-14 baseline + re-entry trigger (3e:206-221). Live-verified: 7 sites, all string-ident/doc-comment, NOT a runtime `match grammar {}`. Generality-POSITIVE — eliminates the unowned "HEAD → 0", binds a grammar-neutral falsifier to an owner; no JSON/CSS-narrowing. |

## Counts

- Sections dispositioned: **11**
- ACCEPT: **11**
- REVISE: **0**
- REJECT: **0**
- ACCEPT rate: **100%** (≥95% convergence threshold met for V3)
- Prior CH2-directed REVISEs outstanding entering V3: **0** (zero orphan REVISE)
- V3-new fold (CH5-V2-R02 leak-census receiver) generality verdict: **generality-neutral / positive**

## Note on the all-ACCEPT V3 verdict (anti-paper-close)

This is the SECOND consecutive ≥95% ACCEPT CH2 cycle (V2=100%, V3=100%), meeting
the convergence threshold. The V3 all-ACCEPT is convergence on a genuinely-folded
synthesis, not paper-close:

1. CH2's V1 REVISEs (S9/S10/S11) were folded in V2 with verified cell/prose/
   predicate evidence; V3 re-author **preserves** all three (7 predicted cells,
   P5a/P5b split, P6 firewall) — re-verified live, not assumed.
2. The single V3-new fold (CH5-V2-R02 leak-census receiver) was independently
   re-scanned from the CH2 axis: routing to MP.SK18.W3 keeps the falsifier
   grammar-NEUTRAL (string-ident, not a runtime branch), live-confirmed at HEAD;
   it strengthens rather than narrows Lock 14.
3. Every load-bearing generality claim was re-executed at HEAD `2a76916ac`: the
   `ValueRef<G>` type-param vehicle, the 8-grammar classifier census (math=0),
   the 87-LOC eq-set fan vs 4-LOC table delegate, the 7-site P1 baseline, the
   MP.SK18.W3 receiver, the Sheets/BBNF-self grammar-source citations, the
   5-shape canon, and the no-CSS-narrowing in 3C/3c-diff. Each ACCEPT names the
   line and the live check that backs it.

CH2's mandate is satisfied: **Lock 14 holds across 3A/3B/3E; tape/ValueRef/NEON
generalise (classifier by-exercise across 8 grammars, value-plane by-construction
on JSON+CSS); 3E's non-JSON story is concrete for CSS L4 / Sheets / BBNF-self;
the future-grammar onboarding test survives with TWO orthogonal axes; 3C and the
3c-locks-v+1-diff accept NO CSS-narrowing or JSON-narrowing — the fleet-wide
value-plane proof is REFUTED, not used to specialise a lock.**

## Open questions tagged forward

| q | receiver | gate |
|---|---|---|
| Should the SK-V18 W3 onboarding gate require a NON-CSS-non-JSON value-plane witness (Sheets OR BBNF-self riding `ValueRef<G>` without a generator branch) before any fleet-wide Lock 14 wording, or is the witnessed-grammars scoping rule (3E17-D07) sufficient indefinitely? | SK-V18 W3 owner (3B) + 3E | W3 byte-equal re-emit gate (P6) + the witnessed-grammars scoping rule; no fleet-wide wording without ≥1 non-JSON-non-CSS value-plane witness. |
| Does MP.SK18.W3's `ValueRef<G>` generator codegen fully zero the 7 `strategy.rs` grammar-name string-ident sites, or does a residual persist as the admitted catalogued non-zero Lock-14 baseline past W3's close gate? | MP.SK18.W3 owner (3B) + Lock-14 leak-census gate; 3E17-D08 carries the receiver-binding | W3 close gate re-runs `rg 'JsonParser\|CssL4Parser' crates/ir/src crates/simd-scan/src`; any residual recorded as the admitted baseline with re-entry trigger = next SK-V18 onboarding-wave leak-census gate — never an unowned "HEAD → 0". |
| Does the `ValueRef<G>` generator re-emit ALL per-grammar value surfaces from one `BackendRule` walk, or do CSS colour-function/calc semantics force a generator branch (a Lock-14 violation)? | SK-V18 W3 generator owner | onboarding predicate P6 (value-axis firewall): re-emit JSON byte-equal AND CSS lazy from one walk; any `match grammar` arm or CSS-specific value branch FAILS. |
