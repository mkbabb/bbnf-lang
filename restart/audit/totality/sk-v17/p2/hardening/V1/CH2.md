---
lens: CH2 GENERALITY
pass: T-P2-research
cycle: V1
reviewer: CH2 (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 91b6893b0
t_p1_locked_sha: 445925167
subject: CH2 GENERALITY review of T-P2 SK-V17 fold dossiers (2a..2f) per PASS-2-RESEARCH §3 + ORCHESTRATOR §3W
scope: "Lock 14 holds — every fold is grammar-neutral; the tape/ValueRef<G>/NEON-classifier generalize across JSON/CSS/Sheets/BBNF-self, not CSS-only. A technique grounded JSON-only that the spec uses fleet-wide is a REVISE."
dossiers_reviewed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
sections_dispositioned: 50
accept: 44
revise: 6
reject: 0
verdict: PASS-WITH-REVISE (88% ACCEPT; V1 expects ≥30% REVISE — the 6 REVISE are folding-grade, no REJECT, no orphan)
---

# CH2 GENERALITY — T-P2 SK-V17 Fold Review (V1)

## Lens charge

CH2 scans whether **Lock 14 holds**: every primitive/technique the dossiers
ground is grounded *grammar-neutrally*; the fold of the SKINNY-proven
unified-tape / lazy `ValueRef<G>` projection / shared NEON `select_classifier`
model transfers across JSON / CSS L4 / Sheets / BBNF-self — not CSS-only, not
JSON-only. A technique grounded JSON-only (or CSS-only) that the spec uses
fleet-wide is a REVISE. The generality vehicle under test is the type-parameter
carriage (`ValueRef<…,G:EventGrammar>`) + alphabet-as-data classifier + the
`@generated` per-grammar allowance Lock 14 permits.

## Executive verdict

The T-P2 cohort is **CH2-disciplined to an unusual degree**. The central
generality move — the grammar enters the value plane as a *type parameter*
`G:EventGrammar` (monomorphised at codegen, zero runtime `match grammar` arm)
and the classifier's only grammar datum is `alphabet: &[u8;64]` (data, not a
JSON constant) — is grounded live at HEAD `91b6893b0` and matches the Lock 14
body verbatim ("the substrate carries ZERO grammar-specific code … per-grammar
runtime modules … emitted from a single grammar-agnostic generator template",
`LOCKS.md:349`). Verified live:

- `ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>`
  (`skinny/crates/runtime/src/tape/mod.rs:175`) — the type-param carriage 2A:61 /
  2C:69 / 2E:154 / 2F:151 all cite is exact.
- `select_classifier(alphabet: &'static [u8;64])`
  (`skinny/crates/bbnf-simd/src/dispatch.rs:42`) — alphabet-as-data, exact.
- `scan_structural` wired across **8** generated core grammars
  (`grep -rln scan_structural crates/core/src/grammar/generated/` = 8) — the
  "8 grammars / impl-exceeds-spec" claim (2A:66, 2C:184, 2D:67, 2E:230, 2F:269)
  is grounded.
- `classify_tbl4` lo6 path is `build_lo6_table(self.alphabet)`
  (`dispatch.rs:24`) — the table is built FROM the alphabet, NOT a JSON
  constant; the "JSON path" framing in 2A/2B is a *selection* distinction
  (which classifier the dispatcher picks), not a JSON-pin. Grammar-neutral.

The cohort's strongest CH2 posture is its **refutation of the fleet-wide
claim**: 2C:77 / 2A:168 / 2E:239 / 2F all carry the by-construction-not-
by-exercise split as a REFUTED row, not an asserted one. Verified: `sheets_witness`
is a **24-LOC `event_grammar_witness.rs` stub** with no `.bbnf` and no
`BackendRule` (`skinny/crates/runtime/src/grammars/sheets_witness/` =
`event_grammar_witness.rs` 24 LOC + `mod.rs` 1 LOC; `find … -name '*.bbnf'` = ∅;
`grep BackendRule` = ∅). This is exactly the Lock 14 v+1 per-wave gate
condition (`LOCKS.md:382-387`): "With only one of Sheets or BBNF-self, the claim
is scoped to the witnessed grammars and may not use fleet-wide grammar-neutral
wording." The cohort honours it: every dossier scopes generality to JSON+CSS
by-exercise and names Sheets/BBNF-self as SK-V18 proof. This is the inverse of
a CH2 fault — the dossiers pre-empt the lens.

The 6 REVISE are **folding-grade**, not structural: each names a fold-row whose
grammar-neutral *verdict text* leans on the eq-set-fan / classifier generality
without surfacing the per-grammar-selection or the Lock 16 "≥1 non-JSON
consumer" requirement that the V1 lock body makes load-bearing. None re-opens a
REDRESS route; none is JSON-only-grounded-then-used-fleet-wide in a way that
escaped the author. Zero REJECT.

## Disposition census

| dossier | rows dispositioned | ACCEPT | REVISE | REJECT |
|---|---|---|---|---|
| 2A sota-landscape | 8 (6 folds + 2 refuted) | 7 | 1 | 0 |
| 2B primitive-vocabulary | 9 (FOLD-L1..L9) | 7 | 2 | 0 |
| 2C grammar-neutrality | 7 (A,B,C,D,E,F,ONBOARD) | 6 | 1 | 0 |
| 2D cost-model | 7 (FOLD-2D-01..07) | 7 | 0 | 0 |
| 2E host-arch | 6 (FOLD-2E-A..F) | 5 | 1 | 0 |
| 2F fold-gaps | 9 (F1..F9) + 4 cross-cut | 12 | 1 | 0 |
| **total** | **50** | **44** | **6** | **0** |

(2F's 9 F-candidates plus its 4 Architectural-Assertions / CH2-pre-emption
cross-cut claims = 13 surfaces; one REVISE, twelve ACCEPT.)

## REVISE dispositions (the load-bearing rows)

### CH2-V1-R1 — 2B FOLD-L9 (`2b:274-286`): "JSON-witnessed; not CSS-keyed" verdict needs the non-JSON-consumer clause or an explicit deletion record

**REVISE.** FOLD-L9 (Commit-by-construction Alt-mode) carries the grammar-neutral
verdict "GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt shape,
**JSON-witnessed; not CSS-keyed**" (`2b:282-283`). Lock 14's v+1 simd/primitive
clause is explicit (`LOCKS.md:423-425`): "A primitive claimed grammar-neutral
must exercise at least one non-JSON consumer or record a measured
deletion/rejection." FOLD-L9 is CONDITIONAL (gated on a post-CF-1 re-profile, no
live consumer on the LOCKED profile), so it is genuinely unexercised — but the
verdict asserts grammar-neutrality on a JSON witness alone without invoking the
deletion/rejection escape. **Concrete fix:** re-state the FOLD-L9 verdict as
"GRAMMAR-NEUTRAL by `BackendRule`-Alt-shape construction; JSON-witnessed only;
NO non-JSON consumer on the LOCKED profile → admitted under the Lock 14 v+1
'measured deletion/rejection' clause (`LOCKS.md:423-425`), disposition
`recorded-not-shortlisted` until the re-profile surfaces a non-JSON
consumer." This converts an implicit JSON-only generality assertion into the
explicit Lock-14-compliant deferred form the dossier's own `disposition: wired
ONLY if the re-profile fires` already implies.

### CH2-V1-R2 — 2B FOLD-L8 (`2b:260-272`): sparse-flag "GENERALISABLE-WITH-GUARD" must name the Sheets/BBNF branch-tag witness or scope to JSON+CSS

**REVISE.** FOLD-L8 (sparse-flag side-table) carries "GENERALISABLE-WITH-GUARD —
flag = `BackendRule` branch-tag projection only" (`2b:268-269`) with the guard
that each flag bit MUST be a `BackendRule` branch-tag projection, "else it
relocates `W5C_REQUEST_FACT_PROFILES` into flag form → CH2 REVISE" (self-flagged).
The guard is correct and the self-flag is honest, but the row does not state
which grammars the branch-tag projection is *exercised* on — the kind-disambiguation
mechanism is the value-API support surface (L3 / `ValueRef<G>`), and L3's
generality is JSON+CSS-by-exercise only (sheets_witness is a 24-LOC stub). A
flag-bit catalogue that is grammar-neutral-by-construction but exercised JSON+CSS
only must carry the same by-construction-not-by-exercise scoping the rest of the
cohort applies. **Concrete fix:** append to the FOLD-L8 verdict: "branch-tag
projection exercised on JSON+CSS only (the `ValueRef<G>` carriers); Sheets/BBNF-self
branch-tag flags are by-construction, proof SK-V18 — the flag catalogue may not
claim fleet-wide grammar-neutrality (`LOCKS.md:382-387`)." This aligns L8 with
L1/L3's explicit scoping.

### CH2-V1-R3 — 2A refuted-row 2 (`2a:318-323`) + FOLD-2A-E: the "classifier already grammar-general across 8 grammars" claim must distinguish config-breadth from proof-breadth at the assertion site

**REVISE.** 2A's second refuted assertion states the JSON-scanner framing
"understates the proven classifier as JSON-pinned … the classifier is
alphabet-parametric and ALREADY grammar-general across 8 grammars" (`2a:318-320`).
The 8-grammar wiring is verified (live: 8). But "grammar-general across 8
grammars" reads as proof-breadth, while the cohort's own discipline (2C:77,
2E:238-239) is that generality is **config-breadth** (alphabet/digraph/quote-class
as data), JSON+CSS-*exercised*, with the richer `StructuralAlphabet` config
"not breadth-of-proof" (2E:238-239 says this explicitly; 2A's assertion site
does not). The 8 wired grammars include bnf/csv/ebnf/css_pretty/google_sheets/bbnf
— but wiring a scan-leaf is not the same as exercising the *value-plane fold*
through them; the fold-under-test (FOLD-2A-C lazy `ValueRef<G>`) is JSON+CSS only.
**Concrete fix:** at `2a:320` qualify "grammar-general across 8 grammars" →
"alphabet-parametric and wired (scan-leaf) across 8 grammars; the classifier's
grammar-generality is config-breadth (alphabet-as-data), value-plane-exercised on
JSON+CSS only — the 8-grammar scan wiring is not a fleet-wide value-fold proof
(`LOCKS.md:423-425`)." 2E's row already carries this distinction; 2A must match it
so the two dossiers do not diverge on the same claim.

### CH2-V1-R4 — 2C Candidate-E (`2c:179-202`): the "CSS L4 is the named non-JSON consumer" claim must bind to the eq-set fan, not the lo6 table

**REVISE.** Candidate SK17-2C-E grounds the classifier as a Lock-16 entry and
cites the Lock 14 v+1 requirement "A primitive claimed grammar-neutral must
exercise at least one non-JSON consumer … CSS L4 is the non-JSON consumer (the
SK-V17 first-mover)" (`2c:196-197`). This is correct AND verified — but the row
does not surface that the CSS consumer rides the **eq-set fan**
(`byte_class_from_eq_set_64_neon`, the one real NEON body,
`aarch64/byte_class_from_eq_set_64.rs:33-72`), NOT the lo6 `classify_tbl4` table
(which CSS deliberately avoids — the `;{`→slot-59 `& 0x3f` collision, `dispatch.rs:106`).
The generality claim "CSS exercises the classifier" is true at the
`select_classifier` dispatch level but routes to a *different backend body* than
JSON. Since `byte_class_from_table_64` is a 4-LOC scalar delegate (2B A4,
verified live), the only NEON body the CSS non-JSON consumer exercises is the
eq-set fan. **Concrete fix:** in Candidate-E's grammar-neutral verdict, state:
"the CSS non-JSON consumer exercises the eq-set fan backend
(`byte_class_from_eq_set_64_neon`), NOT the lo6 table path — the lo6 table is
JSON-selected and its NEON body is a scalar delegate (`2b` A4); the Lock-14
'≥1 non-JSON consumer' requirement is satisfied by the eq-set fan, the one real
NEON Layer-1 body." Without this, a reader could infer CSS exercises a NEON table
body that does not exist.

### CH2-V1-R5 — 2E FOLD-2E-E (`2e:213-246`): the `PrimitiveKernels` 5-name vocabulary admitted as "grammar-neutral, already proven" overstates — three of five are scalar delegates / unwired

**REVISE.** FOLD-2E-E folds the shared classifier vocabulary and lists the skinny
`PrimitiveKernels` set (`byte_class_from_table_64`, `bitmap_prefix_xor_64`,
`bitmap_next_set_bit`, `bulk_emit_positions_64`, `eob_pad_clamp`) as
"already a named primitive set in skinny … matching ARCH's grammar-neutral
signature set" (`2e:216-219`), and Assertion-Defended #3 (`2e:293-297`) states
"the skinny `PrimitiveKernels` names map 1:1 to ARCH's grammar-neutral primitive
signatures … the fold installs them as manifest rows with scalar-ref + checkasm
+ same-wave consumer." This is a generality-by-naming overstatement that 2B's own
dossier refutes: `byte_class_from_table_64` and `bitmap_prefix_xor_64` are 4-LOC
scalar passthroughs (verified live, `aarch64/byte_class_from_table_64.rs:3`,
`aarch64/bitmap_prefix_xor_64.rs:3`); the FSM/frame-stack and `bulk_emit` macros
are source-only-unwired (2B refuted rows). Admitting all five as "already proven
grammar-neutral primitives" with "scalar-ref + checkasm + same-wave consumer"
conflates the one real grammar-neutral NEON body (eq-set fan) with scalar
delegates and unwired contracts. CH2 reads this as a grammar-generality claim
asserted on vocabulary presence rather than exercised consumers. **Concrete fix:**
2E must adopt 2B's close-state taxonomy (`2b` LAC-2b-SKV17-03): file
`byte_class_from_table_64`/`bitmap_prefix_xor_64` as `scalar-delegate-non-ASM`,
the FSM/frame macros as `source-present-unwired`, and admit only
`byte_class_from_eq_set_64_neon` (+ the alphabet-parametric `classify_tbl4`
selection) as the proven grammar-neutral NEON Layer-1 body. The "1:1 mapping to
ARCH signatures" is a *naming* correspondence, not a proof of grammar-neutral
exercise — say so.

### CH2-V1-R6 — 2F F5 (`2f:259-294`) + Defended-#3 (`2f:439-441`): "8 of 9 grammars wire it … the alphabet is the only grammar datum" needs the value-fold-vs-scan-wiring distinction

**REVISE.** F5 states the classifier "is WIRED across 8 of 9 generated grammars
(`math.rs` excepted)" (`2f:269-270`) and Defended-#3 says "8 of 9 grammars wire
it … the alphabet is the only grammar datum" (`2f:439-441`). The math.rs
exception is verified correct (its line-281 `OnceCell<StructuralIndex>` reference
is a *doc-comment*, not a field — `grep` matches the comment only). The 8-of-9
scan-wiring is accurate. The CH2 issue is identical to R3: scan-leaf wiring across
8 grammars is *config-breadth* generality, not a fleet-wide value-plane fold
proof; F2 (the `ValueRef<G>` fold) is JSON+CSS-exercised only, and F5's own
lock-surface row correctly scopes "Sheets/BBNF-self by-construction" — but the
Defended-#3 summary line drops that scoping and reads as proof-breadth.
**Concrete fix:** align Defended-#3 with F5's lock-surface scoping: "8 of 9
grammars wire the scan-leaf (config-breadth, alphabet-as-data); the value-plane
fold is exercised on JSON+CSS only — the 8-grammar scan wiring is not a
fleet-wide grammar-neutral proof (`LOCKS.md:382-387,:423-425`)." This is a
one-line scoping qualifier on a summary assertion, not a structural change.

## ACCEPT dispositions (grammar-neutrality grounded)

The following fold-rows ground grammar-neutrality correctly and are ACCEPTED;
the load-bearing reasons are recorded so T-P3 carries the CH2-cleared surface.

- **2A FOLD-2A-A/B/C/D/F, refuted-row-1** — ACCEPT. The type-param `G` carriage
  (A/C), the `OpenFrame` retirement as a Lock-14 *obligation* (B, grounded against
  `LOCKS.md:349` naming the "CSS L4 14-variant `OpenFrame`" overfit), the
  substrate-manifest-not-6th-shape (D, LAC-1E-14 / `FactStream` precedent), and
  the compile-time `FieldSource` fence (F) are each grammar-neutral by
  construction; the `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F`
  only (verified pattern) — no per-grammar route string. Refuted-row-1
  (CollapsedStage-as-NEON-route) is an arch refutation, CH2-orthogonal but
  correctly grammar-neutral.

- **2B FOLD-L1..L7** — ACCEPT. L1 (eq-set fan) names alphabet-as-data as the
  Lock-14 vehicle and JSON+CSS-witnessed/Sheets-BBNF-by-construction explicitly
  (`2b:164-165`); L2-L4 carry "no grammar-keyed field / generic reuse pattern /
  index-IS-tape"; L5/L6 are grammar-neutral by *digraph parameterisation*
  (`(open:[u8;2],close:[u8;2])` — C/Rust/JS/SQL, not CSS-pinned) with scalar-ref +
  checkasm REQUIRED-NEW (verified: comment/bracket scalar refs absent at HEAD,
  confirming the "REQUIRED-NEW before wiring" discipline). L5/L6 are the
  *best* CH2 rows in the cohort — net-new masks proven grammar-neutral by
  parameterisation, not by naming.

- **2C Candidate-A/B/C/D/F/ONBOARD** — ACCEPT. The future-grammar onboarding test
  (ONBOARD) is the explicit Lock-14 falsifier ("adding a grammar is a config +
  grammar-source change with NO code change in any generic crate",
  `LOCKS.md:349`); the `rg 'JsonParser|CssL4Parser|…' crates/{ir,simd-scan,…}` = 0
  census is the correct CH2 gate. Candidate-C's CH2-firewall ("a CSS-only
  generator that never re-emits JSON FAILS the round-trip") is the precise
  generic-named-CSS-generator failure mode the lens hunts — pre-empted.

- **2D FOLD-2D-01..07** — ACCEPT (all 7). 2D is the cleanest CH2 dossier: every
  fold-row states grammar-neutrality as "the substrate carries no grammar policy
  … the shapes are per-rule projection modes selected by the cost model from
  grammar-derived facts" and FOLD-2D-02 carries the binding gate "the CSP/cost
  facts MUST carry zero grammar names (`json_*`/`css_*` forbidden)" with the
  prior-2D `csp_named_grammars` tautology refutation cited. No proof-breadth
  overstatement; the FactStream-not-6th-shape precedent is correctly the
  grammar-blind substrate-manifest category.

- **2E FOLD-2E-A/B/C/D/F** — ACCEPT. The type-param carriage, the OpenFrame
  deletion target, the substrate-manifest category, and the compile-time fence
  are grammar-neutral; FOLD-2E-C's CH2-firewall (JSON byte-equal re-emission, the
  R-CH2-1 isomorphism anchor) is correct. Only FOLD-2E-E overstates (R5).

- **2F F1/F2/F3/F4/F6/F7/F8/F9 + Defended-1/2/4 + CH2-pre-emption** — ACCEPT.
  F2's W2 gate ("the JSON `value_from_ref` rider must re-emit BYTE-EQUAL … a
  CSS-only generator that never re-emits JSON FAILS the generic-named-CSS-generator
  CH2 failure mode") is the lens's own criterion stated as a fold gate. F7's
  all-8-carrier `substrate_target` census (correcting the COH-014 4-grammar
  undercount) is exactly the grammar-blind classification CH2 wants. F8's selector
  derives `backend_shape` from grammar-derived facts with "no grammar author
  annotates the shape; no grammar name enters the selector" — grounded.

## CH2 cross-cutting observations (no disposition; T-P3 carry)

1. **The cohort's by-construction/by-exercise split is the model CH2 posture.**
   Five of six dossiers explicitly refute the fleet-wide claim and scope to
   JSON+CSS by-exercise. This is the inverse of the CH2 fault (JSON-only grounded,
   fleet-wide used). The 6 REVISE are all about *summary-line* slippage where a
   correctly-scoped fold-row's executive/defended-assertion restatement drops the
   scoping qualifier — a folding hygiene matter, not a generality defect.

2. **The eq-set-fan-is-the-only-real-NEON-body fact (2B A4, verified live) is
   underplayed in 2A/2C/2E's classifier-generality rows.** R4/R5 both trace to
   this: the classifier's grammar-generality is real at the `select_classifier`
   dispatch level (alphabet-as-data), but the *exercised NEON body* for the CSS
   non-JSON consumer is the eq-set fan alone; the table/prefix paths are scalar
   delegates. T-P3 should fold 2B's close-state taxonomy into the classifier
   manifest row so the Lock-16 "≥1 non-JSON consumer" requirement binds to a real
   NEON body, not a scalar-delegate name.

3. **No grammar-name leak into generic-crate fold surfaces.** Verified: the fold
   target `crates/simd-scan` carries `StructuralAlphabet` (config), not a JSON
   constant; `scan_structural` takes `&StructuralAlphabet` as data; the
   `ValueRef<G>` carries the grammar in `G`, not a `match`. The onboarding test
   (2C ONBOARD) is the correct standing falsifier. CH2 finds no Lock-14
   generic-crate-grammar-branch in any fold proposal.

## CH2 verdict

**PASS-WITH-REVISE.** 44 ACCEPT / 6 REVISE / 0 REJECT (88% ACCEPT). The cohort
grounds grammar-neutrality (Lock 14) correctly across all six divergence folds:
the type-param `G:EventGrammar` carriage, the alphabet-as-data classifier, the
`@generated` per-grammar allowance, and the future-grammar onboarding falsifier
are each grounded live at HEAD `91b6893b0` and matched to the Lock 14 body. The
fleet-wide claim is REFUTED (not asserted) on the verified `sheets_witness`
24-LOC-stub evidence — the cohort pre-empts the lens's central concern. The 6
REVISE are folding-grade: each names a fold-row whose grammar-neutral verdict
text or summary assertion drops the by-construction/by-exercise scoping or the
Lock-16 "≥1 non-JSON consumer" binding, on the eq-set-fan-vs-table NEON-body
distinction (R4/R5), the JSON-witnessed-only conditional (R1), the sparse-flag
exercise scope (R2), or the scan-wiring-vs-value-fold conflation (R3/R6). None
re-opens a REDRESS route; none is a structural generality defect; zero orphan
REVISE. Each REVISE carries a concrete one-to-three-line fix that aligns the
slipping row with the cohort's own already-correct scoping discipline.
