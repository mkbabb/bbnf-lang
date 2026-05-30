---
lens: CH2-GENERALITY
pass: T-P3-synthesis
cycle: V1
reviewer: CH2 GENERALITY (V1)
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac
scope: 3A + 3B + 3C(+3c-locks-v+1-diff) + 3E — Lock-14 generality discipline
mandate: Lock 14 holds across 3A/3B/3E; the tape/ValueRef/NEON generalise to JSON/CSS/Sheets/BBNF-self; 3C accepts no CSS-narrowing amendment.
verdict: 8 ACCEPT, 3 REVISE, 0 REJECT (11 dispositioned sections)
---

# CH2 GENERALITY — T-P3 SK-V17 cycle V1

## Lens posture

CH2 scans whether Lock 14 holds across the synthesis: do 3A's surface deltas
and 3B's wave reconciliation generalise to non-JSON; is 3E's grammar-
generalisation story concrete for CSS L4 / Sheets / BBNF-self; does 3C accept
no amendment that narrows a lock to JSON (or CSS); does the future-grammar
onboarding test survive. The firewall is against JSON/CSS-narrowing and against
asserted-not-proven generality.

## Live verification performed (master HEAD 2a76916ac)

| claim | source | live check | result |
|---|---|---|---|
| `ValueRef<'doc,'input,K,G:EventGrammar>` is a grammar TYPE param, zero runtime branch | 3E17-D01; `tape/mod.rs:175` | `sed -n 170,190p skinny/crates/runtime/src/tape/mod.rs` | CONFIRMED — `pub struct ValueRef<…, G: EventGrammar = AnyGrammar>` with `PhantomData<fn() -> G>`; monomorphised, no `match grammar`. |
| alphabet-as-data is the only NEON grammar datum | 3E17-D04; `SPEC.md:314-317` | `sed -n 314,317p restart/skinny/tranches/sk-v17/SPEC.md` | CONFIRMED — "The L1 classifier's only grammar datum is the `alphabet: &[u8;64]`"; CSS `;{` slot-59 eq-set fan route stated verbatim. |
| classifier wired across 8 grammars (config-breadth, non-JSON proven) | 3E17-D04, SK17L-008 | `grep -c scan_structural crates/core/src/grammar/generated/{json,ebnf,bnf,csv,css_l4,css_pretty,google_sheets,bbnf,math}.rs` | CONFIRMED — 8×`=1`, math=0. `scan_structural(input, &StructuralAlphabet)` is the alphabet-parametrised signature (`crates/simd-scan/src/lib.rs:80`). Non-JSON classifier generality is by-EXERCISE, not by-assertion. |
| eq-set fan is the one REAL NEON body; table is a scalar delegate | 3E17-D04/D06, L14-17-HC-04 | `cat aarch64/byte_class_from_table_64.rs`; `wc -l + grep intrinsics aarch64/byte_class_from_eq_set_64.rs` | CONFIRMED — `byte_class_from_table_64_neon` is a 3-line delegate to scalar; `byte_class_from_eq_set_64.rs` is 87 LOC w/ 17 NEON intrinsic-uses. The CSS non-JSON consumer binds the real body, not the passthrough. |
| `sheets_witness` cannot serve as a projection exercise | 3E17-D07, SK17L-009 | `sed -n 110,114p restart/skinny/tranches/sk-v17/SPEC.md` | CONFIRMED — SPEC §0.1.11: "no `BackendRule` shape and cannot serve as an SK-V17 projection exercise". Scoping clause well-grounded. |
| 5-shape canon holds verbatim across 3A/3B/3E/3C | §8.2 | `grep` canon string in all four artefacts + `LOCKS.md:107-108` | CONFIRMED — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; every "6th/sixth shape" mention is a negation/fence/G-Omega-gate. |
| no-6th-shape stands on a second mechanical ground | 3A-D04, 3C Lock-10 clause | `grep admits_collapsed_stage restart/ARCHITECTURE.md` | CONFIRMED — `:1151` LAC-2D-06 binds it to `target.arch == x86`; aarch64 mechanically refused. |
| P1 HEAD leak baseline = 7 hits in strategy.rs | 3E P1 predicate | `rg -c JsonParser\|CssL4Parser crates/ir/src crates/simd-scan/src` | CONFIRMED — 7 hits, all `crates/ir/src/registry/strategy.rs` (`:132,:137,:149,:197,:198,:292,:315`), string-ident registry + doc-comments, NOT runtime `match grammar {}`. |
| P2 = 9 grammar dirs + tape/ all generated | 3E P2 predicate | `find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d` | CONFIRMED — bbnf/bnf/css_l4/css_pretty/csv/ebnf/google_sheets/json/math + tape/. (math present in runtime dirs; matrix cites the 9-grammar set correctly.) |
| 3C narrows no lock to CSS/JSON | mandate | `grep -n CSS …/3c-locks-crystallisation.md` for narrowing wording | CONFIRMED — Lock 14 clause is scope-HONEST: value-fold JSON+CSS by-exercise, classifier 8-grammar config-breadth, Sheets/BBNF-self by-construction; the lock stays grammar-NEUTRAL, no fleet-wide claim, no CSS-specialisation. |

## The load-bearing generality finding (ACCEPT)

The synthesis carries a **two-axis generality model** and holds it honestly:

- **Classifier axis (NEON `select_classifier(alphabet)`)** — generality is
  breadth-of-CONFIG, proven **by-exercise** across 8 grammars at HEAD
  (`scan_structural=1` in 8 generated parsers; alphabet-as-data). This is the
  strongest non-JSON generality evidence in the packet: the classifier is
  already grammar-general, not a fold-target. 3E17-D04 + L14-17-HC-04 correctly
  bind CSS L4 to the REAL eq-set fan body (verified 87-LOC/17-intrinsic), not
  the 3-line scalar-delegate `byte_class_from_table_64_neon`.

- **Value-plane axis (`ValueRef<G>` projection)** — generality is breadth-of-
  PROOF, exercised **by-construction JSON+CSS only**; Sheets/BBNF-self are
  SK-V18 by-construction (`sheets_witness` is a stub w/ no `BackendRule`). The
  scoping clause (3E17-D07 / L14-17-HC-07) prevents the value-fold's generality
  from being mis-stated fleet-wide.

Keeping these two axes DISTINCT (classifier = 8-grammar config-breadth; value-
plane = JSON+CSS exercise) is the discipline that makes Lock 14 honest rather
than paper-general. 3C's Lock 14 clause states this split verbatim
(`3c-locks-crystallisation.md:96`): "The shared classifier's grammar-generality
is config-breadth (alphabet-as-data) across 8 grammars — a SEPARATE axis from
the value-fold, never asserted fleet-wide." This is exactly the Lock-14
discipline CH2 firewalls for. **No CSS-narrowing; no JSON-narrowing.**

## Section dispositions

| # | section | disposition | rationale |
|---|---|---|---|
| 1 | 3A-D02 lazy `ValueRef<G>` value-plane | **ACCEPT** | Names the grammar-parametric projection as the materialization plane; CH4-coverage fail action explicitly REJECTs "CSS-only generator that never re-emits JSON" (`3a:88`) — the JSON byte-equal re-emit is the generality firewall. Grammar-neutral, scope-honest. |
| 2 | 3A-D04 BackendShape-category disposition | **ACCEPT** | Tape = substrate-manifest category, 5-shape canon verbatim, no 6th shape; two independent grounds verified (LAC-1E-14 precedent + `admits_collapsed_stage` x86-binding at ARCH:1151). Coherent with 3B §13 and 3E matrix (§8.2). |
| 3 | 3B H.W4.LOCK14 wave row | **ACCEPT** | "Every fold design is grammar-neutral (Lock 14)"; `begin_compound` reads `layout.rule_id & 0x1F` only (verified grep-zero StructRegistry in `tape/mod.rs:185-186` per 3A-D05); classifier config-breadth cited. Sheets/BBNF-self kept SK-V18, not SK-V17 claim. Generalises. |
| 4 | 3B-D03 tape-as-substrate-category MASTER coherence | **ACCEPT** | Restates the FactStream precedent verbatim; 5-shape canon "STAYS UNCHANGED across §13/§13.5/§13.1"; 6th variant G-Omega-gated. Cross-surface coherence holds — generality-neutral wave move. |
| 5 | 3C Lock 14 ValueRef/classifier-generalisation clause | **ACCEPT** | The crystallisation does NOT narrow Lock 14 to CSS/JSON. Value-fold JSON+CSS by-exercise; classifier 8-grammar config-breadth; Sheets/BBNF-self by-construction; "may not be claimed fleet-wide"; "No grammar branch … enters any generic crate." Lock stays grammar-neutral. The mandate's central requirement — 3C accepts no CSS-narrowing — is SATISFIED. |
| 6 | 3C Lock 10 tape-category clause + 3c-locks-v+1-diff Lock-10 hunk | **ACCEPT** | Five-shape Lock-10 search domain held verbatim (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, 4 canon-string hits in the diff); substrate-category placement explicit; 6th variant G-Omega-gated + SK-V17 §9-barred. No grammar-narrowing. |
| 7 | 3C Lock 16 NEON-classifier-manifest clause | **ACCEPT** | Registers `select_classifier(alphabet)` as a manifest row with the alphabet as grammar-policy DATA; CSS L4 the non-JSON same-wave consumer binding the real NEON body; honest scalar-delegate disposition for table/prefix. aarch64-only, no x86 close. Generality-preserving. |
| 8 | 3E17-D04/D06 + L14-17-HC-04 alphabet-as-data + CSS non-JSON consumer | **ACCEPT** | The single strongest generality claim, fully verified: 8-grammar classifier wiring + the real eq-set fan body + the `;{` slot-59 collision route. CSS binds a REAL NEON body, not the delegate. This is by-exercise non-JSON generality, not assertion. |
| 9 | 3E Per-Grammar BackendShape Matrix — Sheets/BBNF-self rows (`3e:104-109`) | **REVISE** | The status column ("by-construction (SK-V18 proof)") is correct, but the *dominant shape* cells assign concrete shapes (`OffsetTape`/`EventTape`/`EagerTape`) to Sheets `formula`/`LET`/infix and BBNF `grammar`/expression. These are PREDICTIONS — the cost-model selector `derive_backend_shape` is skinny-only and unwired to these grammars (confirmed: `grep scan_structural` and selector both skinny-resident). A reader scanning the matrix column sees proven shape assignments. FIX: annotate the Sheets/BBNF-self shape cells "predicted (cost-model-pending)" or move the concrete shape into the evidence column with a "cost-model will derive" qualifier, so the by-construction boundary is crisp at the CELL level, not only the status column. |
| 10 | 3E Future-Grammar Onboarding Test P5 (`3e:143`) — "a real measured non-JSON consumer" | **REVISE** | The eq-set fan body, the slot-59 collision, and the 8-grammar classifier wiring are all verified REAL. But P5's wording "CSS L4 binds the eq-set fan NEON body … a real measured non-JSON consumer" conflates two states: CSS *classifier scan* IS wired/measurable (SK17L-008), but the CSS *tape consumer* is itself the SK-V18 fold-target — SK17L-008 itself states "the residual gap is the missing TAPE CONSUMER, not the scan." FIX: split P5 into "CSS L4 classifier scan: wired/measured (8-grammar census)" vs "CSS L4 tape consumer: SK-V18 fold-target"; the Lock-16 "≥1 non-JSON consumer" is satisfied by the classifier scan, but the prose should not imply the full tape-consumer chain is measured. |
| 11 | 3E17-D08 + P3 future-grammar onboarding tape predicates | **REVISE** | P3 ("tape substrate wiring") states the falsifier is "the per-wave grammar-name + grammar-shape leak census with monotonic-decrease-to-zero (HEAD baseline non-zero, catalogued)" — verified P1=7 hits in strategy.rs. The predicate is sound, but the onboarding test as written gives no GENERALITY falsifier that a NEW grammar (beyond the witnessed JSON+CSS) actually rides `ValueRef<G>` without a generator branch. The value-plane re-emit firewall (JSON byte-equal, cited in 3A-D02/3B-W3) is the right falsifier but is absent from 3E's §12 predicate set. FIX: add to P3 (or as P6) the value-plane generality falsifier — "the single `ValueRef<G>` generator re-emits JSON byte-equal AND CSS lazy from one `BackendRule` walk; a generator with a `match grammar` arm or a CSS-specific value branch FAILS" — so the onboarding test carries the value-axis generality gate, not only the classifier/leak axis. |

## Counts

- Sections dispositioned: **11**
- ACCEPT: **8**
- REVISE: **3** (sections 9, 10, 11)
- REJECT: **0**
- REVISE rate: **27%** (below the V1 ≥30% expectation; see note)

## Note on REVISE rate

The three REVISE items are real generality-precision defects, not paper-close
nits: each names a place where a TRUE underlying fact (8-grammar classifier
wiring, eq-set fan body, type-param carriage) is presented one notch beyond what
HEAD proves (predicted shapes shown as assignments; classifier-scan "measured"
read as tape-consumer measured; value-axis generality falsifier absent from the
onboarding test). CH2's mandate is firewalled cleanly: **Lock 14 holds across
3A/3B/3E; the tape/ValueRef/NEON generalise (classifier by-exercise across 8,
value-plane by-construction JSON+CSS); 3C accepts NO CSS/JSON-narrowing.** No
amendment narrows a lock to JSON or CSS; the future-grammar onboarding test
survives, strengthened by the live HEAD baseline. The synthesis is unusually
honest on the by-exercise/by-construction boundary; the REVISEs sharpen that
boundary at the cell/prose level rather than overturn it.

## Open questions tagged forward

| q | receiver | gate |
|---|---|---|
| Should the SK-V18 W3 onboarding gate require a NON-CSS-non-JSON value-plane witness (Sheets OR BBNF-self) before any fleet-wide Lock 14 wording, or is the witnessed-grammars scoping rule (3E17-D07) sufficient indefinitely? | SK-V18 W3 owner (3B) + 3E | W3 byte-equal re-emit gate + the witnessed-grammars scoping rule. |
| Does the `ValueRef<G>` generator re-emit ALL per-grammar value surfaces from one `BackendRule` walk, or do CSS colour-function/calc semantics force a generator branch (a Lock-14 violation)? | SK-V18 W3 generator owner | JSON byte-equal re-emit firewall; zero `match grammar` arm in the generator. |
