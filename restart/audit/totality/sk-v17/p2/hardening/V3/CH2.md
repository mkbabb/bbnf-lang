---
lens: CH2 GENERALITY
pass: T-P2-research
cycle: V3
reviewer: CH2 (V3)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_sha: 91b6893b0
subject: CH2 GENERALITY review of T-P2 SK-V17 fold dossiers (2a..2f) cycle V3 per PASS-2-RESEARCH §3 + ORCHESTRATOR §3W
scope: "Lock 14 holds — every fold is grammar-neutral; the tape/ValueRef<G>/NEON-classifier generalize across JSON/CSS/Sheets/BBNF-self, not CSS-only and not JSON-only. A technique grounded JSON-only that the spec uses fleet-wide is a REVISE."
dossiers_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
prior_cycle: V2 (CH2-V2-R1, 1 REVISE, 0 REJECT, 98.1% ACCEPT)
prior_revise_folded: [CH2-V2-R1]
sections_dispositioned: 52
accept: 52
revise: 0
reject: 0
verdict: CONVERGED (100% ACCEPT; the lone V2 REVISE CH2-V2-R1 folded with live evidence; 0 REVISE; 0 REJECT; 0 orphan — second consecutive >=95% CH2 cycle, §3Z convergence met)
---

# CH2 GENERALITY — T-P2 SK-V17 Fold Review (V3)

## Lens charge

CH2 scans whether **Lock 14 holds**: every primitive/technique the dossiers
ground is grounded *grammar-neutrally*; the fold of the SKINNY-proven
unified-tape / lazy `ValueRef<G>` projection / shared NEON `select_classifier`
model transfers across JSON / CSS L4 / Sheets / BBNF-self — not CSS-only, not
JSON-only. A technique grounded JSON-only (or CSS-only) that the spec uses
fleet-wide is a REVISE. The generality vehicle under test is the type-parameter
carriage (`ValueRef<…,G:EventGrammar>`) + alphabet-as-data classifier + the
`@generated` per-grammar allowance Lock 14 permits, with the
by-construction / by-exercise split (JSON+CSS proven, Sheets/BBNF-self SK-V18)
load-bearing.

## Executive verdict

**CONVERGED.** 52 ACCEPT / 0 REVISE / 0 REJECT across 52 dispositioned surfaces
(100% ACCEPT). The V3 cohort folded the lone V2 REVISE (CH2-V2-R1) with
live-anchored evidence, and the fold introduced no new grammar-neutrality slip.
The central CH2 fault — a technique grounded JSON-only (or CSS-only) and then
used fleet-wide — is absent throughout. The by-construction/by-exercise scoping
that V1 first flagged and V2 made uniform is now applied at **every** re-stated
grammar-neutral verdict, including the previously-unscoped REQUIRED-NEW
`bracket_depth_mask_64` row. This is the **second consecutive >=95%-ACCEPT CH2
cycle** (V2 = 98.1%, V3 = 100%); with zero open critical defects, zero orphan
unresolved REVISE, and V = 3 (<= 5), the §3Z CH2 convergence criterion is met.

**V2 REVISE fold verification (CH2-V2-R1 discharged, live-anchored):**

- **CH2-V2-R1 → 2B FOLD-L6** — FOLDED. FOLD-L6 (`bracket_depth_mask_64`,
  NET-NEW depth-balance mask) now carries the re-scoped verdict: "GRAMMAR-NEUTRAL
  by **mask-only construction** — the primitive sees only the L1-abstracted
  open/close masks, never literal bracket bytes, so it carries zero grammar datum
  (the strongest Lock-14 form, stronger than alphabet-as-data). The
  nested-balance pattern parameterises over any bracket-pair grammar; the JSON
  arrays/objects, CSS component blocks, BBNF `()`/`[]`, Sheets parens enumeration
  is the **parameterisation DOMAIN, NOT an exercise set** — L6 is REQUIRED-NEW
  with no wired consumer at HEAD … by-construction grammar-neutral, by-exercise
  unwired pending the W3 abrogate gate" (`2b:285-294`). The fold explicitly binds
  L6 to its sibling L5's framing ("This matches sibling L5's parameterisation-domain
  framing ('no CSS pin', `2b` FOLD-L5), not a fleet-wide exercise claim",
  `2b:293-295`). This is the exact one-line re-scope the V2 disposition prescribed.
  Frontmatter records the fold (`2b:20`:
  `CH2-V2-R1-FOLD-L6-mask-only-construction-parameterisation-DOMAIN-not-exercise-set-by-construction-grammar-neutral-by-exercise-unwired`).
  Discharged — verified live: no `src/scalar/bracket_depth_mask_64.rs` exists at
  `91b6893b0` (`ls skinny/crates/bbnf-simd/src/scalar/ | grep bracket` = ABSENT),
  and `depth_carry`/`scan_components_to_index` are absent from live source, so the
  REQUIRED-NEW / zero-wired-consumer classification is honest. The primitive is
  genuinely grammar-neutral by the strongest (zero grammar datum) form; the fix
  converted only the enumeration's exercise-reading into the parameterisation-domain
  form the rest of the cohort applies.

The cohort's **by-construction/by-exercise refutation of the fleet-wide claim**
remains the model CH2 posture and is verified live at HEAD `91b6893b0`:

- `sheets_witness` is a **24-LOC** `event_grammar_witness.rs` + 1-LOC `mod.rs`
  (`wc -l` = 24 + 1), with directory contents `event_grammar_witness.rs` + `mod.rs`
  only — **no `.bbnf`, no `BackendRule`** (`grep -rl BackendRule
  skinny/crates/runtime/src/grammars/sheets_witness/` = empty). It cannot serve as
  a projection exercise; generality is therefore scoped JSON+CSS by-exercise across
  all six dossiers (`2a:289`, `2b:332-335`, `2c:70-72,:85,:92`, `2d:155-156`,
  `2e:340-350`, `2f:316-318,:528-531`).
- The grammar-name leak into generic crates is **exactly 7 hits, all in one file**
  (`grep -rc 'JsonParser|CssL4Parser' crates/ir/src crates/simd-scan/src` =
  `crates/ir/src/registry/strategy.rs:7`, single-file) — the catalogued
  ARCH-3A-D09 string-ident resolver leak under the monotonic-decrease rule, NOT a
  runtime `match grammar {…}` branch. Matches the V2 claim exactly.
- The eq-set fan is the **one real aarch64 NEON Layer-1 body** (`wc -l
  byte_class_from_eq_set_64.rs` = 87 LOC); `byte_class_from_table_64_neon` is a
  **line-3 scalar delegate** to `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar`
  (verified live `aarch64/byte_class_from_table_64.rs:3`). The cohort's "only the
  eq-set fan is a proven NEON body; CSS `;{` routes there via the slot-59
  collision" binding holds at HEAD.
- The type-param carriage is live: `pub struct ValueRef<'doc, 'input: 'doc, K =
  AnyKind, G: EventGrammar = AnyGrammar>` (`tape/mod.rs:175`), with `impl<…, K, G:
  EventGrammar> Copy/Clone` (`:183,:185`) — the grammar enters as a TYPE param,
  zero runtime `match grammar` arm.

## Disposition census

| dossier | rows dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---|---|---|
| 2A sota-landscape | 8 (6 folds + 2 refuted) | 8 | 0 | 0 |
| 2B primitive-vocabulary | 9 (FOLD-L1..L9) | 9 | 0 | 0 |
| 2C grammar-neutrality | 7 (A,B,C,D,E,F,ONBOARD) | 7 | 0 | 0 |
| 2D cost-model | 7 (FOLD-2D-01..07) | 7 | 0 | 0 |
| 2E host-arch | 6 (FOLD-2E-A..F) | 6 | 0 | 0 |
| 2F fold-gaps | 9 (F1..F9) + 6 cross-cut | 15 | 0 | 0 |
| **total** | **52** | **52** | **0** | **0** |

(2F's 9 F-candidates plus its 4 Architectural-Assertions-Defended + 2
Refuted-rows that carry a grammar-neutral claim = 15 CH2 surfaces; all ACCEPT.)

## ACCEPT dispositions (grammar-neutrality grounded)

The following fold-rows ground grammar-neutrality correctly and are ACCEPTED; the
load-bearing reasons are recorded so T-P3 carries the CH2-cleared surface.

- **2A FOLD-2A-A/B/C/D/E/F + both refuted rows** — ACCEPT (all 8). The type-param
  `G` carriage (A/C, verified live `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>`,
  `tape/mod.rs:175`), the `OpenFrame` retirement as a Lock-14 obligation (B,
  `2a:149-152` — "deletion of per-grammar runtime is Lock-14-ALLOWED-but-EAGER …
  their EAGER shape is the AZ-IV pre-block"), the substrate-manifest-not-6th-shape
  (D, LAC-1E-14 + the independent `admits_collapsed_stage` x86-bound anchor), the
  classifier-as-Lock-16-entry with per-primitive admission (E, carrying the
  scalar-delegate-vs-NEON-body split — "only the eq-set fan is a proven NEON body;
  `byte_class_from_table_64_neon`/`bitmap_prefix_xor_64_neon` are live line-3 scalar
  delegates", `2a:79`), and the compile-time `FieldSource` fence (F) are each
  grammar-neutral by construction. Refuted-row classifier-generality is
  config-breadth, value-plane-exercised JSON+CSS only (`2a:289,:294-295,:404-408`),
  matching 2E `:238-239` exactly (the two dossiers no longer diverge on the same
  claim).

- **2B FOLD-L1..L9** — ACCEPT (all 9; the V2 lone REVISE on L6 now folded).
  L1 (eq-set fan) names alphabet-as-data + JSON+CSS-witnessed /
  Sheets-BBNF-by-construction; L4 (tokenize-once reuse) is grammar-neutral with the
  index==tape-offsets identity; L5 (`comment_body_mask_64`) is the model
  REQUIRED-NEW row — "GRAMMAR-NEUTRAL by digraph parameterisation (C/Rust/JS/SQL
  block comments); no CSS pin" (`2b:263-264`), naming the parameterisation domain
  (a class of languages), not the bbnf fleet; **L6 (`bracket_depth_mask_64`) now
  matches L5** — "GRAMMAR-NEUTRAL by mask-only construction … parameterisation
  DOMAIN, NOT an exercise set" (`2b:285-294`), the CH2-V2-R1 fold; L8 (sparse-flag
  side-table) is scoped JSON+CSS by-exercise with the `BackendRule` branch-tag
  guard (`2b:330-335`); L9 (commit-by-construction Alt-mode) is deferred to the
  appendix + admitted under the deletion/rejection clause, recorded-not-shortlisted
  (`2b:348-367`, CH2-V1-R1 fold preserved). The eq-set fan / table-delegate split
  is verified live and is the load-bearing grammar-neutral NEON-body anchor.

- **2C Candidate-A/B/C/D/E/F/ONBOARD** — ACCEPT (all 7). Candidate-A (flat offset
  tape) carries "no grammar column in the substrate — FENCE: a kind-partitioned
  dense class … is the Lock-14 line" (`2c:114-122`); Candidate-B grounds OpenFrame
  retirement as a Lock-14 *obligation* not merely a perf one (`2c:134-144`);
  Candidate-E binds the CSS non-JSON consumer to the eq-set fan via the slot-59
  collision (`2c:85`) — verified live; Candidate-C's CH2 firewall ("a CSS-only
  generator that never re-emits JSON FAILS the round-trip") is the precise
  generic-named-CSS-generator failure mode the lens hunts, pre-empted. The
  Technique-Grounding-Table's "Fleet-wide grammar-neutral CLAIM on JSON+CSS
  exercise alone" row is **refuted (as a fleet-wide claim)** (`2c:92`) — the model
  CH2 refutation. The ONBOARD verify_action carries the live HEAD baseline
  (Predicate-1 = 7 single-file string-ident leak sites under monotonic-decrease;
  Predicate-2 = 9 `@generated` grammar dirs + tape/ substrate, clean) — both
  verified live, and the V3 count-nit fold (`2c:27,:419-420`) corrected the
  Predicate-2 leading numeral to 9, ground-truth-exact at HEAD.

- **2D FOLD-2D-01..07** — ACCEPT (all 7). 2D remains the cleanest CH2 dossier:
  every fold-row states grammar-neutrality as "the substrate carries no grammar
  policy … the shapes are per-rule projection modes selected by the cost model
  from grammar-derived facts". FOLD-2D-02 carries the binding gate "the CSP/cost
  facts MUST carry zero grammar names (`json_*`/`css_*` forbidden)" (`2d:128-136`)
  with the prior-2D `csp_named_grammars` tautology refutation. FOLD-2D-03 grounds
  the value-API generator as "JSON-WITNESSED + CSS first-mover … A CSS-only
  generator that never re-emits JSON FAILS … never carry-forward" (`2d:155-162`) —
  the CH2 firewall stated as a fold gate. FOLD-2D-05 grounds the classifier as a
  scan-cost fact carrying "alphabet cardinality / digraph count, not a grammar
  name" (`2d:204`, T2D17 grounding row `:76`).

- **2E FOLD-2E-A/B/C/D/E/F** — ACCEPT (all 6). FOLD-2E-C's CH2 firewall is correct:
  "the projection generator must re-emit JSON byte-equally (the W2 R-CH2-1
  isomorphism anchor); a CSS-only generator that never re-emits JSON FAILS CH2"
  with the preserve-rich-ast obligation marked "an obligation/target the fold must
  meet, NOT a property held at this pass" (`2e:218-226`). The "one
  grammar-parametric lazy projection generator emits the value API for all 8
  grammars" (`2e:204-205`) is **API-emission breadth** (the recipe is
  grammar-parametric), explicitly distinct from value-plane exercise — the
  immediately-following verdict scopes the exercise to JSON+CSS with the CH2
  firewall, so this is NOT an unscoped fleet-wide claim. FOLD-2E-E admits only the
  eq-set fan on exercised-consumer grounds with the close-state taxonomy; the
  scoped "breadth-of-config, value-plane-exercised JSON+CSS only … may not claim
  fleet-wide" (`2e:340-350`) matches the cohort.

- **2F F1..F9 + Defended-1/2/3/4 + Refuted-1/2** — ACCEPT (all 15). F2's W2 gate
  (the `ValueRef<G>` generator must re-emit JSON BYTE-EQUAL; a CSS-only generator
  FAILS the generic-named-CSS-generator CH2 failure mode) is the lens's own
  criterion stated as a fold gate. F5 grounds the classifier as config-breadth and
  explicitly separates it from the `ValueRef<G>` value-plane fold (F2): "this is
  config-breadth (the alphabet is the only datum); it is NOT the same as the
  `ValueRef<G>` value-plane fold (F2), which is exercised JSON+CSS-only … not a
  fleet-wide value-fold proof (`LOCKS.md:382-387`, `:423-425`)" (`2f:314-318`).
  Defended-#3 (the cross-cut summary) is bound to the scan-leaf config-breadth axis
  only (`2f:528-534`). F6 (FieldSource fence) and F8 (`backend_shape` selector — "no
  grammar author annotates the shape; no grammar name enters the selector") are the
  grammar-blind classification CH2 wants.

## CH2 cross-cutting observations (no disposition; T-P3 carry)

1. **The V2→V3 fold is clean: zero new generality slips introduced.** The lone V2
   REVISE (CH2-V2-R1) was a parameterisation-vs-fleet enumeration on a REQUIRED-NEW
   row; the V3 fold applied the by-construction/mask-only qualifier at that single
   slipping site and bound it to sibling L5's framing, without over-correcting into
   a new defect. With the cohort's scoping discipline now uniform across all six
   dossiers, no grammar-neutral verdict reads as an exercise claim where the
   exercise is unwired.

2. **The eq-set-fan-is-the-only-real-NEON-body fact is uniformly carried and live
   at HEAD.** 2A FOLD-2A-E (`:79`), 2B L1 + close-state taxonomy, 2C Candidate-E
   (`:85`), 2E FOLD-2E-E (`:316-317,:419-424`) all bind the Lock-16 "≥1 non-JSON
   consumer" requirement to a real NEON body (the eq-set fan, CSS reached via the
   slot-59 collision), not a scalar-delegate name. `byte_class_from_table_64_neon`
   is a verified line-3 scalar passthrough; the cohort prices it `scalar-delegate-non-ASM`,
   not as a NEON-body row.

3. **No grammar-name leak into generic-crate fold surfaces.** Verified live: the
   only grammar-name occurrences in generic crates (`crates/ir`, `crates/simd-scan`)
   are **7 string-ident resolver sites in one file** (`strategy.rs`), catalogued
   under the monotonic-decrease rule. `StructuralAlphabet` carries
   singletons/digraph_mask/digraph_pairs/quote_classes as config data (the
   quote_classes doc names JSON `"` vs CSS `'`/`"` as config *values*, not
   branches); `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F` only.
   The ONBOARD future-grammar onboarding census (2C) is the correct standing
   falsifier. CH2 finds no Lock-14 generic-crate-grammar-branch in any fold proposal.

4. **The config-breadth vs value-plane-fold axis split is the cohort's durable CH2
   safeguard.** Across 2A/2C/2E/2F the dossiers consistently distinguish (a) the
   classifier's grammar-generality = config-breadth (alphabet-as-data, wired
   scan-leaf across 8 grammars) from (b) the `ValueRef<G>` value-plane fold =
   by-exercise JSON+CSS only (Sheets/BBNF-self SK-V18 by-construction). This split
   is the precise mechanism that prevents a JSON+CSS-exercised fold being asserted
   fleet-wide; it is now stated at every assertion site, not only summary lines.

## CH2 verdict

**CONVERGED.** 52 ACCEPT / 0 REVISE / 0 REJECT (100% ACCEPT). The V3 cohort folded
the lone V2 REVISE (CH2-V2-R1) — the FOLD-L6 `bracket_depth_mask_64`
parameterisation-domain re-scope — with live-anchored evidence, binding L6 to its
sibling L5's "names the parameterisation domain, not the bbnf fleet" framing, and
introduced no new generality slip. The grammar-neutrality vehicle (type-param
`G:EventGrammar` carriage `tape/mod.rs:175`, alphabet-as-data classifier,
`@generated` per-grammar allowance, future-grammar onboarding falsifier) is
grounded live at HEAD `91b6893b0` and matched to the Lock 14 body across all six
divergence folds. The fleet-wide claim is REFUTED (not asserted) on the verified
`sheets_witness` 24-LOC-stub evidence; the by-construction/by-exercise scoping is
uniform across the cohort; the config-breadth vs value-plane-fold axis split holds
at every assertion site. The eq-set fan is the one real NEON Layer-1 body
(87 LOC, verified), with `byte_class_from_table_64_neon` a verified line-3 scalar
delegate; no grammar-name leak into generic crates beyond the catalogued 7-hit
single-file string-ident resolver. None of the 52 surfaces re-opens a REDRESS
route; none is a structural generality defect; zero orphan REVISE. This is the
**second consecutive >=95%-ACCEPT CH2 cycle** (V2 = 98.1%, V3 = 100%) — with zero
open critical defects, zero orphan unresolved REVISE, and V = 3 (<= 5), the §3Z
CH2 convergence criterion is met.
