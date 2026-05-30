---
lens: CH2 GENERALITY
pass: T-P2-research
cycle: V2
reviewer: CH2 (V2)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_sha: 445925167
subject: CH2 GENERALITY review of T-P2 SK-V17 fold dossiers (2a..2f) cycle V2 per PASS-2-RESEARCH §3 + ORCHESTRATOR §3W
scope: "Lock 14 holds — every fold is grammar-neutral; the tape/ValueRef<G>/NEON-classifier generalize across JSON/CSS/Sheets/BBNF-self, not CSS-only and not JSON-only. A technique grounded JSON-only that the spec uses fleet-wide is a REVISE."
dossiers_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
prior_cycle: V1 (CH2-V1-R1..R6, 6 REVISE, 0 REJECT)
prior_revise_folded: [CH2-V1-R1, CH2-V1-R2, CH2-V1-R3, CH2-V1-R4, CH2-V1-R5, CH2-V1-R6]
sections_dispositioned: 52
accept: 51
revise: 1
reject: 0
verdict: PASS (98.1% ACCEPT; all 6 V1 REVISE folded with live evidence; 1 residual folding-grade REVISE; 0 REJECT; 0 orphan)
---

# CH2 GENERALITY — T-P2 SK-V17 Fold Review (V2)

## Lens charge

CH2 scans whether **Lock 14 holds**: every primitive/technique the dossiers
ground is grounded *grammar-neutrally*; the fold of the SKINNY-proven
unified-tape / lazy `ValueRef<G>` projection / shared NEON `select_classifier`
model transfers across JSON / CSS L4 / Sheets / BBNF-self — not CSS-only, not
JSON-only. A technique grounded JSON-only (or CSS-only) that the spec uses
fleet-wide is a REVISE. The generality vehicle under test is the type-parameter
carriage (`ValueRef<…,G:EventGrammar>`) + alphabet-as-data classifier + the
`@generated` per-grammar allowance Lock 14 permits, with the by-construction /
by-exercise split (JSON+CSS proven, Sheets/BBNF-self SK-V18) load-bearing.

## Executive verdict

**PASS.** 51 ACCEPT / 1 REVISE / 0 REJECT across 52 dispositioned surfaces
(98.1% ACCEPT). The V2 cohort folded **all six V1 REVISE** (CH2-V1-R1..R6) with
verifiable live evidence, and the folds did not introduce a single new
grammar-neutrality slip — the central CH2 fault (JSON-only grounded, fleet-wide
used) is absent throughout, and the by-construction/by-exercise scoping that V1
flagged as slipping at *summary lines* is now consistently applied at every
re-stated assertion. The one residual REVISE (CH2-V2-R1) is folding-grade and
local to a single primitive row's grammar enumeration; it does not re-open a
REDRESS route, is not a structural defect, and carries a one-line fix.

**V1 REVISE fold verification (all six discharged, live-anchored):**

- **CH2-V1-R1 → 2B FOLD-L9** — FOLDED. L9 (Commit-by-construction Alt-mode) is
  **moved out of the wired enumeration into a "Deferred-Pending-Reprofile
  Appendix"** (`2b:323-347`) and the verdict re-stated: "JSON-WITNESSED-ONLY
  codegen property … a JSON witness alone does not establish grammar-neutrality
  — generality requires ≥1 non-JSON consumer OR a recorded measured
  deletion/rejection … admitted under the **deletion/rejection clause** …
  disposition is **recorded-not-shortlisted**" (`2b:336-342`). This is the exact
  Lock-14 v+1 `LOCKS.md:423-425` escape the V1 disposition prescribed. Discharged.

- **CH2-V1-R2 → 2B FOLD-L8** — FOLDED. L8 (sparse-flag side-table) now carries
  "**Scoping (CH2-V1-R2):** the branch-tag projection is EXERCISED on JSON+CSS
  ONLY — the kind-disambiguation rides `ValueRef<G>` (JSON+CSS by-exercise;
  `sheets_witness` is a 24-LOC stub). Sheets/BBNF are by-construction, proof
  SK-V18; this row may NOT claim fleet-wide generality (`LOCKS.md:382-387`)"
  (`2b:306-310`). Aligns L8 with L1/L3's explicit scoping. Discharged.

- **CH2-V1-R3 → 2A refuted-row-2 + :320** — FOLDED. 2A's refuted-row-2 now reads
  "the classifier's grammar-generality is CONFIG-BREADTH (alphabet-as-data, the
  Lock-14 vehicle), value-plane-exercised on JSON+CSS only — the 8-grammar scan
  wiring … is NOT a fleet-wide value-fold proof (CH2-V1-R3, `LOCKS.md:423-425`;
  the value-plane fold under test, FOLD-2A-C lazy `ValueRef<G>`, is JSON+CSS
  only)" (`2a:379-390`), and explicitly closes "2E `:238-239` carries this
  distinction; this assertion site now matches it" (`2a:390`). The two dossiers
  no longer diverge on the same claim. Discharged.

- **CH2-V1-R4 → 2C Candidate-E + LAC-SK17-2C-02** — FOLDED. Candidate-E now binds
  the CSS non-JSON consumer to the eq-set fan via the slot-59 collision: "CSS's
  `;{` structural pair collides on the `& 0x3f` low-6-bit table slot … so
  `lo6_table_admissible` returns false … and CSS routes to the **eq-set fan**
  `byte_class_from_eq_set_64` … That eq-set fan IS the one real aarch64 NEON
  Layer-1 body" (`2c:210-219`); `byte_class_from_table_64_neon` filed as a
  scalar passthrough. Discharged — verified live (eq-set fan is a real NEON body,
  `aarch64/byte_class_from_eq_set_64.rs:33`; table_64 is a passthrough to
  `crate::scalar::byte_class_from_table_64_scalar`, `aarch64/byte_class_from_table_64.rs:3`).

- **CH2-V1-R5 → 2E FOLD-2E-E** — FOLDED. FOLD-2E-E now carries a "**Close-state
  taxonomy (CH2-V1-R5 + CH4-2e-001 fold)**" block (`2e:271-296`) admitting **only**
  the eq-set fan as the one real NEON Layer-1 body, filing
  `byte_class_from_table_64`/`bitmap_prefix_xor_64` as `scalar-delegate-non-ASM`
  (3-line passthroughs) and FSM/frame macros as `source-present-unwired`, with the
  explicit "A `1:1 ARCH-signature mapping` is **naming, not exercise-proof**"
  (`2e:294-296`). Adopts 2B's close-state taxonomy exactly as the V1 disposition
  prescribed. Discharged.

- **CH2-V1-R6 → 2F F5 + Defended-#3** — FOLDED. Defended-#3 now reads "**config-breadth**
  (the alphabet is the only grammar datum) … The `ValueRef<G>` **value-plane**
  fold (F2) is a SEPARATE axis, **exercised JSON+CSS only**; scan-leaf wiring
  across 8 grammars is NOT a fleet-wide value-fold proof (`LOCKS.md:382-387`,
  `:423-425`)" with the explicit "[CH2-V1-R6 fold: this row previously dropped the
  value-fold-vs-scan-wiring scoping …]" (`2f:523-532`). The summary line is now
  bound to the scan-leaf config-breadth axis only. Discharged — verified live: the
  `math.rs` exception (`math.rs:281` is a doc-comment `/// OnceCell<StructuralIndex>`,
  not a field; scan_structural wired across exactly 8 generated grammars).

The cohort's **by-construction/by-exercise refutation of the fleet-wide claim**
remains the model CH2 posture and is verified live: `sheets_witness` is a 24-LOC
`event_grammar_witness.rs` stub + 1-LOC `mod.rs` (no `.bbnf`, no `BackendRule`),
so generality is scoped JSON+CSS by-exercise across all six dossiers
(`2a:180-181`, `2b:178`, `2c:82`, `2d:99-101`, `2e:309-311`, `2f:527-529`). The
ONBOARD falsifier (2C) is verified live: Predicate-1 grammar-name leak in generic
crates is exactly **7 hits, all in `crates/ir/src/registry/strategy.rs`**
(`rg -c 'JsonParser|CssL4Parser' crates/ir/src crates/simd-scan/src` = 7,
single-file) — the catalogued ARCH-3A-D09 string-ident resolver leak under a
monotonic-decrease rule, not a runtime `match grammar` branch.

## Disposition census

| dossier | rows dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---|---|---|
| 2A sota-landscape | 8 (6 folds + 2 refuted) | 8 | 0 | 0 |
| 2B primitive-vocabulary | 9 (FOLD-L1..L9) | 8 | 1 | 0 |
| 2C grammar-neutrality | 7 (A,B,C,D,E,F,ONBOARD) | 7 | 0 | 0 |
| 2D cost-model | 7 (FOLD-2D-01..07) | 7 | 0 | 0 |
| 2E host-arch | 6 (FOLD-2E-A..F) | 6 | 0 | 0 |
| 2F fold-gaps | 9 (F1..F9) + 6 cross-cut | 15 | 0 | 0 |
| **total** | **52** | **51** | **1** | **0** |

(2F's 9 F-candidates plus its 4 Architectural-Assertions-Defended + 2
Refuted-rows that carry a grammar-neutral claim = 15 CH2 surfaces; all ACCEPT.)

## REVISE disposition (the one residual)

### CH2-V2-R1 — 2B FOLD-L6 (`2b:265-283`): the REQUIRED-NEW `bracket_depth_mask_64` verdict enumerates the JSON/CSS/BBNF/Sheets fleet as if exercised; scope the enumeration to the parameterisation domain or carry the by-construction qualifier

**REVISE.** FOLD-L6 (`bracket_depth_mask_64`, NET-NEW depth-balance mask) carries
the grammar-neutral verdict "GRAMMAR-NEUTRAL — **the canonical Lock-14
nested-balance primitive (JSON arrays/objects, CSS component blocks, BBNF
`()`/`[]`, Sheets parens)**; sees only masks" (`2b:273-274`). The primitive is
genuinely grammar-neutral by the **strongest** form — it "sees only masks
(open/close abstracted by L1 …), never literal bracket bytes" (`2b:266-267,:274`),
so it carries *zero* grammar datum, stronger than alphabet-as-data. The verdict's
mask-only grounding is correct and ACCEPT-worthy. **The CH2 slip is the
enumeration:** L6 is **REQUIRED-NEW** — its scalar reference + checkasm are
**absent at HEAD** (`2b:277` `src/scalar/bracket_depth_mask_64.rs` REQUIRED-NEW;
verified live — no scalar/checkasm sibling exists) and it has **no wired
consumer** (disposition `wired` if W3 lands AND the abrogate gate clears, else
`deleted`, `2b:282-283`). Listing four named fleet grammars (JSON, CSS, BBNF,
Sheets) as the primitive's exercisers reads as a fleet-wide *exercise* claim on a
primitive that is exercised on **zero** grammars today — the same
by-construction-vs-by-exercise conflation the V1 cohort scoped everywhere else.
Its sibling L5 (`comment_body_mask_64`, also REQUIRED-NEW) does this correctly:
"GRAMMAR-NEUTRAL by digraph parameterisation (C/Rust/JS/SQL block comments); **no
CSS pin**" (`2b:254-255`) — L5 names the *parameterisation domain* (a class of
languages), not the bbnf *fleet*, and carries no exercise implication. **Concrete
fix:** align L6's verdict with L5's parameterisation framing: "GRAMMAR-NEUTRAL by
**mask-only construction** — the primitive sees only the L1-abstracted open/close
masks, never literal bracket bytes, so it carries zero grammar datum (the
strongest Lock-14 form). The nested-balance pattern parameterises over any
bracket-pair grammar (the JSON/CSS/BBNF/Sheets enumeration is the
**parameterisation domain**, NOT an exercise set — L6 is REQUIRED-NEW with no
wired consumer at HEAD; by-construction grammar-neutral, by-exercise unwired
pending the W3 abrogate gate)." This converts a fleet-wide-reading exercise
enumeration into the by-construction-domain form the rest of the cohort applies,
and is consistent with L6's own `disposition: wired if W3 lands … else deleted`.

## ACCEPT dispositions (grammar-neutrality grounded)

The following fold-rows ground grammar-neutrality correctly and are ACCEPTED; the
load-bearing reasons are recorded so T-P3 carries the CH2-cleared surface.

- **2A FOLD-2A-A/B/C/D/E/F + both refuted rows** — ACCEPT (all 8). The type-param
  `G` carriage (A/C, verified live `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`,
  `tape/mod.rs:175`), the `OpenFrame` retirement as a Lock-14 obligation (B), the
  substrate-manifest-not-6th-shape (D, LAC-1E-14 + the independent
  `admits_collapsed_stage` x86-bound anchor), the classifier-as-Lock-16-entry with
  per-primitive admission (E, now carrying the scalar-delegate-vs-NEON-body split
  table at `2a:247-259`), and the compile-time `FieldSource` fence (F, now naming
  the live `arena.rs:47` coupling-site) are each grammar-neutral by construction.
  Refuted-row-2 (R3 fold) now matches 2E's config-breadth distinction exactly.

- **2B FOLD-L1/L2/L3/L4/L5/L7/L8 + FOLD-L9 (deferred)** — ACCEPT (8 of 9; L6 is
  the lone REVISE). L1 (eq-set fan) names alphabet-as-data + JSON+CSS-witnessed /
  Sheets-BBNF-by-construction (`2b:177-178`); L3 carries the type-witnessed
  `ValueRef<G>` carriage + the PayloadArena substrate manifest declaration
  (CH5-V1-001 fold); L5 is the **model REQUIRED-NEW row** — "no CSS pin",
  parameterisation-domain framing (the form L6 should adopt); L8 now scoped
  JSON+CSS (R2 fold); L9 deferred + recorded-not-shortlisted (R1 fold). The eq-set
  fan / table-delegate split (A4, `2b:136-146`) is verified live and is the
  load-bearing grammar-neutral NEON-body anchor.

- **2C Candidate-A/B/C/D/E/F/ONBOARD** — ACCEPT (all 7). Candidate-E (R4 fold)
  binds the CSS non-JSON consumer to the eq-set fan via the slot-59 collision —
  verified. The ONBOARD verify_action carries the live HEAD baseline (Predicate-1
  = 7 single-file string-ident leak sites under monotonic-decrease, Predicate-2 =
  8 `@generated` grammar dirs clean) — both verified live. Candidate-C's CH2
  firewall ("a CSS-only generator that never re-emits JSON FAILS the round-trip")
  is the precise generic-named-CSS-generator failure mode the lens hunts,
  pre-empted. The by-construction/by-exercise split (`2c:82`) is the model CH2
  refutation.

- **2D FOLD-2D-01..07** — ACCEPT (all 7). 2D remains the cleanest CH2 dossier:
  every fold-row states grammar-neutrality as "the substrate carries no grammar
  policy … the shapes are per-rule projection modes selected by the cost model
  from grammar-derived facts". FOLD-2D-01's "FactStream precedent already covers
  CSS (`LOCKS.md:110-112`)" is verified accurate — the LAC-1E-14 amendment
  explicitly "resolves the CSS L4 declaration-values substrate-classification gap"
  (`LOCKS.md:110-114`), a correct grammar-neutral grounding. FOLD-2D-02 carries the
  binding gate "the CSP/cost facts MUST carry zero grammar names
  (`json_*`/`css_*` forbidden)" with the prior-2D `csp_named_grammars` tautology
  refutation. FOLD-2D-05 grounds the classifier as a scan-cost fact carrying
  "alphabet cardinality / digraph count, not a grammar name".

- **2E FOLD-2E-A/B/C/D/E/F** — ACCEPT (all 6). FOLD-2E-E (R5 fold) now admits only
  the eq-set fan on exercised-consumer grounds with the close-state taxonomy; the
  scoped "breadth-of-config, value-plane-exercised JSON+CSS only … may not claim
  fleet-wide" (`2e:309-311`) matches the cohort. FOLD-2E-C's CH2 firewall (JSON
  byte-equal re-emission, the R-CH2-1 anchor) is correct; FOLD-2E-D carries both
  the LAC-1E-14 precedent and the independent `admits_collapsed_stage` anchor.

- **2F F1..F9 + Defended-1/2/3/4 + Refuted-1/2** — ACCEPT (all 15). F2's W2 gate
  (JSON `value_from_ref` rider must re-emit BYTE-EQUAL; a CSS-only generator FAILS
  the generic-named-CSS-generator CH2 failure mode) is the lens's own criterion
  stated as a fold gate (`2f:182-184`). F5 + Defended-#3 (R6 fold) are now
  consistently scoped to scan-leaf config-breadth. F7's all-8-carrier
  `substrate_target` census (correcting the COH-014 4-grammar undercount) is the
  grammar-blind classification CH2 wants. F8's selector derives `backend_shape`
  from grammar-derived facts with "no grammar author annotates the shape; no
  grammar name enters the selector" (`2f:447-448`) — grounded.

## CH2 cross-cutting observations (no disposition; T-P3 carry)

1. **The V1→V2 fold is clean: zero new generality slips introduced.** All six V1
   REVISE were summary-line/scoping slippage; the V2 folds applied the
   by-construction/by-exercise qualifier at each slipping site and the
   eq-set-fan-binding at each classifier-generality site, without over-correcting
   into a new defect. The one residual (CH2-V2-R1) is a pre-existing
   parameterisation-vs-fleet enumeration on a REQUIRED-NEW row that V1 ACCEPTed —
   on the second pass, with the cohort's scoping discipline now uniform, L6's
   fleet enumeration stands out as the lone unscoped grammar-neutral verdict.

2. **The eq-set-fan-is-the-only-real-NEON-body fact is now uniformly carried.**
   R4/R5's traces are folded: 2B's A4 close-state taxonomy is adopted into 2A's
   FOLD-2A-E table (`2a:247-259`), 2C's Candidate-E (`2c:218-220`), and 2E's
   FOLD-2E-E (`2e:271-287`). The Lock-16 "≥1 non-JSON consumer" requirement now
   binds to a real NEON body (the eq-set fan) across the cohort, not a
   scalar-delegate name — verified live at HEAD.

3. **No grammar-name leak into generic-crate fold surfaces.** Verified live: the
   only grammar-name occurrences in generic crates (`crates/ir`, `crates/simd-scan`)
   are 7 string-ident resolver sites in one file (`strategy.rs`), catalogued under
   the monotonic-decrease rule; `StructuralAlphabet` carries
   singletons/digraph_mask/digraph_pairs/quote_classes as config data (the
   quote_classes doc names JSON `"` vs CSS `'`/`"` as config *values*, not
   branches); `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F` only.
   The onboarding test (2C ONBOARD) is the correct standing falsifier. CH2 finds
   no Lock-14 generic-crate-grammar-branch in any fold proposal.

## CH2 verdict

**PASS.** 51 ACCEPT / 1 REVISE / 0 REJECT (98.1% ACCEPT). The V2 cohort folded all
six V1 REVISE (CH2-V1-R1..R6) with live-anchored evidence — the deletion/rejection
clause on L9, the JSON+CSS scoping on L8, the config-breadth-vs-value-fold
distinction on 2A/2F, the eq-set-fan binding on 2C/2E, and the close-state
taxonomy on 2E — and introduced no new generality slip. The grammar-neutrality
vehicle (type-param `G:EventGrammar` carriage, alphabet-as-data classifier,
`@generated` per-grammar allowance, future-grammar onboarding falsifier) is
grounded live at HEAD `91b6893b0` and matched to the Lock 14 body across all six
divergence folds. The fleet-wide claim is REFUTED (not asserted) on the verified
`sheets_witness` 24-LOC-stub evidence; the by-construction/by-exercise scoping is
now uniform across the cohort. The lone residual REVISE (CH2-V2-R1) is
folding-grade: FOLD-L6's REQUIRED-NEW `bracket_depth_mask_64` verdict enumerates
the JSON/CSS/BBNF/Sheets fleet as the parameterisation domain in exercise-reading
form, where its sibling L5 names the parameterisation domain without an exercise
implication. The fix is a one-line re-scope to the by-construction/mask-only form;
the underlying primitive is genuinely grammar-neutral by the strongest (zero
grammar datum) form. None re-opens a REDRESS route; none is a structural
generality defect; zero orphan REVISE. This is the first ≥95%-ACCEPT CH2 cycle —
one more clean cycle meets the §3Z convergence criterion.
