# Pass Omega V10 Hardening — CH2 GENERALITY lens (cycle V3)

Lens: CH2 GENERALITY. (1) Does the Ω-C locks amendment respect Lock 14 across
JSON / CSS L4 / Sheets / BBNF-self? (2) Does the Ω-D master-plan reconciliation
generalise to non-JSON — i.e. NO JSON/CSS-narrowing amendment? REJECT a
non-applying diff / a revived REDRESS route / a Lock-14 narrowing / a coupling /
an uncited claim. Cycle V1 target was ≥30% REVISE.

Scope reviewed: ΩC-locks-amendments + locks-diff.md (11-clause SK-V18 addendum),
ΩD-master-plan-reconciliation + master-plan-diff.md (6 staged diffs), the
generality-bearing legs of ΩA / ΩB / ΩE / ΩF + migration/handoff deltas, against
live `restart/locks/LOCKS.md`, `restart/MASTER-PLAN.md`,
`crates/ir/src/registry/strategy.rs`, the skinny tree, and the converged
T-P1/T-P2/T-P3 evidence.

## Cycle-V3 standing: V1 + V2 REVISE corrections are ALL INCORPORATED at this HEAD

V3 runs against staged diffs that have moved past BOTH prior cycles. The honest
adversarial job is (a) confirm the prior corrections landed faithfully against
the live surfaces, (b) NOT re-raise resolved items to chase the V1 quota, and (c)
surface only genuine residuals. The convergence is real: every V1 (R01/R02/R03/
R05) and V2 (R-V2-01) REVISE has been folded, and the lone V1 REVISE that was a
stale read (R04, the "4-grammar table") is confirmed-WRONG — the live roster IS
nine grammars. Disposition of the six prior REVISE items at this HEAD:

- **CH2-V1-R01 (scoped-non-JSON-witness label) — APPLIED.** locks-diff.md:63 now
  reads verbatim "Per the live MP.NW6 (`restart/MASTER-PLAN.md:662`) /
  H.W4.LOCK14 (`:605`) single-negative-control standard … SK-V18's un-fork
  generality is a `scoped non-JSON witness` (CSS + a single Sheets control)".
  Verified live: MP.NW6 (`:662`) and H.W4.LOCK14 (`:605`) carry the
  `scoped non-JSON witness` standard ("CSS plus both Sheets and BBNF-self … with
  only one negative control, label the result `scoped non-JSON witness`")
  byte-for-byte. RESOLVED.
- **CH2-V1-R02 (Sheets by-construction→by-exercise reconcile) — APPLIED.**
  locks-diff.md:63 states the delta against the live SK-V17 `:620` clause
  ("Sheets/BBNF-self are by-construction under SK-V18, NOT by-exercise
  (`sheets_witness` 24-LOC stub)", verified live). RESOLVED.
- **CH2-V1-R03 (cursor-generality 8-of-9 + e-graph coupling) — APPLIED.**
  locks-diff.md:77 reads "the SK-V17 8-of-9 figure is NOT re-verified at the
  SK-V18 HEAD census post-P3-collapse … re-census is owed at SK-V18 G4/G5".
  The e-graph guard is parenthesised as a Lock-4/Lock-10 decision-engine note
  "NOT a grammar-generality invariant". RESOLVED (citation-precision residual
  R-V3-01 below).
- **CH2-V1-R04 (Pattern-H roster) — V1 STALE READ; diff CORRECT.** The live
  `strategy.rs:137`-`185` registers NINE grammars (Json, GoogleSheets, CssL4,
  Bbnf, Csv, Math, Bnf, Ebnf, CssPretty); the narrow Lock-14 verification regex
  lists exactly FOUR names (`JsonParser|CssL4Parser|BbnfBootstrap|
  GoogleSheetsParser`) and the literal 13-crate scan returns 13. The diff's
  "registers 9 grammars … 17 ident strings … catches 4 of those 9 … returns 13"
  is fully correct and faithful to 2C LAC-2C-SK18-02/03. C11 is ACCEPT.
- **CH2-V1-R05 (§13.7 inline 3-grammar caveat) — APPLIED.** master-plan-diff.md
  global-gate bullet carries "This is the `scoped non-JSON witness` (3-grammar)
  un-fork … the F.W5 nine-grammar close is FED by this generator and adopted at
  SK-V19 scale — §13.7 does NOT satisfy F.W5". RESOLVED.
- **CH2-V2-R-V2-01 (PROVE-conditional Sheets witness) — APPLIED.** locks-diff.md:63
  now reads "SK-V18's PROVE wave PROMOTES Sheets … to a by-EXERCISE negative
  control IF AND ONLY IF PROVE returns non-`N` … the third exercised witness DOES
  NOT YET EXIST at this HEAD (PROVE undispatched) and materialises only on a
  non-`N` PROVE verdict … the third-witness claim inherits the PROVE wave's own
  falsifier rather than presupposing its success" — the R-V2-01 correction
  verbatim. RESOLVED.

## Spot-Verification (the load-bearing items, run live at HEAD)

- **`git apply --check` on the staged locks-diff: EXIT 0.** The extracted diff
  body applies cleanly against live `restart/locks/LOCKS.md` at HEAD. Hunk header
  `@@ -622,6 +622,33 @@` matches the live anchor: SK-V17 Lock 16 NEON-classifier
  clause ends at `:622`, two blanks at `:623`/`:624`, `## v+1 Governance
  Boundary` at `:625`. The 27-line addendum lands between; the SK-V15
  (`:581`-`607`) and SK-V17 (`:610`-`622`) addenda are neither restated nor edited.
- **master-plan-diff is a multi-section staged proposal, NOT a single applyable
  patch — by design, NOT a REJECT.** Concatenated extraction fails
  `git apply --check` ("No valid patches") because the six per-section diffs carry
  NO `diff --git` file headers and NO `@@` hunk markers — they are prose-anchored
  proposal hunks the post-G-Omega CRUD pass applies section-by-section, matching
  the doc's own "STAGED ONLY — NOT APPLIED" framing (master-plan-diff.md:4,
  §8 Application Order). Every old-side anchor resolves verbatim against the live
  tree (staging HEAD `25297a7fc` == current HEAD): §13.6 header (`:974`), §5 F row
  (`:196`), F.W5 slot (`:519`), §24 carry row (`:1346`), §13.5 header (`:912`),
  MP.NW6 (`:662`), H.W4.LOCK14 (`:605`). NOT a non-applying-diff REJECT.
- **Live invariants hold.** 16 numbered locks (`grep -nE '^[0-9]+\. \*\*'` →
  `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`); five
  `BackendShape` variants `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}`, no 6th. Both PLANNED co-gate symbols absent
  (`runtime_target_rows_collapsed` rg=0; `bbnf_simd_single_mask_convention` rg=0).
- **§13.7 falsifier counts verified EXACT.** P1 x86 file count "today 28" —
  `find …/x86_64 …/ext/x86 -type f` = 28. P2 bench-leak "today 64 = 48 in
  nonjson_css_l4.rs + 16 in bin/gate.rs" — `grep -c measure_mbps|lightningcss_facts`
  = 48 (nonjson_css_l4.rs) + 16 (bin/gate.rs) = 64. C11 "71 at HEAD" —
  `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` = 71;
  the 67 Pattern-H baseline is live at LOCKS.md:408-409.
- **REDRESS references resolve, NONE revived.** Items 51 (`742-768`, cursor
  REJECT) / 53 (`784-813`, structural-mask cursor REJECT) / 246 (W11T parse-only
  structural-STREAM driver REJECT) / 247 (W11V string64 mask REJECT) resolve in
  1D-skinny-lessons.md with explicit ADMISSIBLE-vs-REJECTED distinctions; the
  master-plan-diff's "item 246 = … bounds G4" and "abut REDRESS items
  51/53/246/247" use them as REJECTED boundaries, not revived routes. REDRESS
  96/97/98 streamed-cursor (the M5-Max scalar-cheaper-than-SIMD-cursor finding,
  `skinny/REDRESS.md:2928`-`2933`) is the CollapsedStage clause's RETIRED prior,
  carried "must clear that retired prior", not re-opened.
- **C2 firewall `css_provider_source` distinction verified — no hidden
  collision.** The clause asserts the PLANNED firewall predicate
  `css_provider_source == generated` is "distinct from the live
  `bbnf-bench/src/report.rs` bench-report field of the same name; not yet a
  codegen/firewall gate symbol". Verified: `css_provider_source` appears ONLY in
  `report.rs` (6 hits, a `String` bench field), ZERO hits in
  `skinny/crates/codegen` or `skinny/crates/runtime`. The clause correctly scopes
  its skinny CSS scan (`grammars/css_l4_*` + `runtime_simd.rs`) and routes the
  totality `crates/core/src/runtime/css_l4/` surface to the SK-V19 DEFER bundle —
  NO narrowing.
- **C8 retarget-not-author anchor verified at HEAD.** `find_css_significant`
  (`runtime_simd.rs:169`) takes `delimiters`/`fixed` as CALLER-supplied byte-set
  args and the inner `byte_class_from_eq_set_64(block, set_a)` is the
  grammar-neutral eq-set member scan — exactly the "neutral sub-kernel /
  CSS-scoped shell" two-axis split the clause claims. The kernel names no grammar.
- **C3 neutrality evidence anchors resolve.** SYNTHESIS-RESEARCH.md:231-237 is the
  NEUTRALITY-PROOF obligation (the JSON `{}`/`[]` OR Sheets `paren_expr`
  dischargers + CSS-scoped demotion); SPEC:972-983 is the GROUND s6/C4 finding;
  2C:380-381 is the LAC-2C-SK18-01/02 candidate-matrix rows. All resolve.

## Enumerated Amendments / CRUD Operations Under the CH2 Lens

### Ω-C locks-diff (11 addendum clauses)

| # | Amendment | V1 | V2 | V3 |
|---|---|---|---|---|
| C1 | Named-primitive (a)-(d) gate (Lock 14/16/8) | ACCEPT | ACCEPT | ACCEPT |
| C2 | Relocated-seam firewall + un-fork (Lock 5/14/1) | ACCEPT | ACCEPT | ACCEPT |
| C3 | Neutrality-proof clause (Lock 14/16) | REVISE | REVISE | ACCEPT (R01/R02/R-V2-01 applied) |
| C4 | aarch64-ONLY clause (Lock 16/8) | ACCEPT | ACCEPT | ACCEPT |
| C5 | Verbatim-blob-courier clause (Lock 6/14) | ACCEPT | ACCEPT | ACCEPT |
| C6 | Green-by-exclusion precondition (Lock 14) | ACCEPT | ACCEPT | ACCEPT |
| C7 | Single-SIMD-substrate + one-movemask (Lock 16) | ACCEPT | ACCEPT | ACCEPT |
| C8 | Retarget-not-author clause (Lock 16/14) | ACCEPT | ACCEPT | ACCEPT |
| C9 | CollapsedStage shape-slot (Lock 10/16) | ACCEPT | ACCEPT | ACCEPT |
| C10 | Cursor-generality re-anchor (Lock 14/1/10) | REVISE | ACCEPT | REVISE (R-V3-01, citation precision) |
| C11 | Pattern-H re-census clause (Lock 13/14) | REVISE | ACCEPT | ACCEPT |

### Ω-D master-plan-diff (6 staged diffs)

| # | Amendment | V1 | V2 | V3 |
|---|---|---|---|---|
| D1 | Diff 1 — §13.6 re-key SK-V18 tape-fold → SK-V19 totality-fold | ACCEPT | ACCEPT | ACCEPT |
| D2 | Diff 2 — NEW §13.7 SK-V18 GENERALIZATION 12-wave block | REVISE | ACCEPT | REVISE (R-V3-02, global-gate scope) |
| D3 | Diff 3 — §25 Implementation Order monotonic skinny→totality | ACCEPT | ACCEPT | ACCEPT |
| D4 | Diff 4 — §24 Carry Ledger re-key + SK-V19 tee-up rows | ACCEPT | ACCEPT | ACCEPT |
| D5 | Diff 5 — §5 F.W5 / §13.5 CSS verdict reconciliation | ACCEPT | ACCEPT | ACCEPT |
| D6 | Diff 6 — §13 H-row + Lock-10 cross-ref label alignment | ACCEPT | ACCEPT | ACCEPT |

## ACCEPT findings (generality respected; the lens's core question answered)

- **No JSON/CSS-narrowing amendment.** Every Ω-C clause and Ω-D diff that names
  CSS does so as an HONEST scope: the FORCED `balanced_component_scan` →
  `css_balanced_component_scan` demotion (the GROUND s6/C4 finding grounds both
  offered non-CSS dischargers on disk — JSON `{}`/`[]` parse-with-emit descents,
  Sheets `paren_expr` descent — and finds them structurally incompatible with the
  byte-SKIP shell, so the CSS-scoped name is FORCED, not chosen); the skinny-only
  `css_provider_source == generated` side-channel scan; the CSS-scoped recognizer
  SHELL over a grammar-NEUTRAL eq-set sub-kernel (caller-data). NO clause restricts
  the substrate or the un-fork TO JSON/CSS. The substrate stays grammar-neutral
  (`backend_shape` dispatch, `.bbnf`-driven generator, 9-grammar
  `strategy.rs` manifest as DATA). The CSS naming is demotion-for-honesty — the
  OPPOSITE of narrowing.
- **C1/C2 grammar-neutral by construction.** The (a)-(d) gate conjuncts
  ("grammar-INVOKED-by-name", "output VARIES under invoking-rule mutation") apply
  to ANY grammar's hot leaf — the gate's named admits are BOTH the CSS
  `balanced_component_scan` AND the JSON `string`/`number` scanners (SPEC:382-384),
  not one grammar. The firewall dispatches `render(program)` on `backend_shape`
  with `RuntimeEmitterKind` DELETED at the post-G3 end-state (the un-fork that
  makes adding a grammar a config change — the direct Lock-14 spirit).
  md5-NECESSARY-NOT-SUFFICIENT + the PLANNED `runtime_target_rows_collapsed` co-gate
  is the correct guard against a per-grammar branch relocated into a data table.
  iburg citation (DOI 10.1145/151640.151642) is the right back-end-stage precedent.
- **C3 now ACCEPT.** R01 (scoped-non-JSON-witness label inherited from MP.NW6/
  H.W4.LOCK14 verbatim), R02 (by-construction→by-exercise delta stated, not
  silently contradicting `:620`), and R-V2-01 (PROVE-conditional tense: "third
  exercised witness DOES NOT YET EXIST at this HEAD … materialises only on a
  non-`N` PROVE verdict") are all folded. The clause is generality-EXACT: a
  single-control witness is labelled `scoped non-JSON witness`, fleet-wide wording
  requires SK-V19 OR both Sheets AND BBNF-self in one wave, and the third witness
  inherits the PROVE falsifier. This is the live Lock-14 standard respected across
  JSON/CSS/Sheets/BBNF-self, not narrowed.
- **C11 now ACCEPT.** The 9-grammar / 17-ident roster is correct at HEAD
  (verified) and faithful to the 2C source; the "do NOT bolt a 9-name regex widen
  as an SK-V18 patch; the R16 PartialEq collapse + roster-wide regex +
  `css_types.rs` relocation are SK-V19 (D11b, ≈+217)" disposition routes the leak
  forward without narrowing or revived REDRESS, citing Lock 14(c)
  declaration-crate admissibility. The +4 (71 vs 67) attribution (tape-fold roster
  trace, not an O(N) generator regression) is sound.
- **D1/D3/D4/D5/D6.** The identity pivot (MASTER's "SK-V18" `crates/core` fold →
  SK-V19; the certified GENERALIZATION cycle → §13.7) is generality-CORRECT:
  monotonic skinny→totality preserved, the 9-grammar fleet adoption routed to
  SK-V19 rather than over-claimed at SK-V18. D5 carries the CH2-V1-R03 fold
  (strikes the un-caveated "MEASUREMENT-VALID", keeps "directionally-valid pending
  H1 re-lock"). D4's §24 tee-up routes the 9-ident table / css_types.rs /
  `simd-scan`-vs-`bbnf-simd` / BBNF-self litmus to SK-V19 with explicit Lock-14(c)
  reasoning — no silent drop, no narrowing.

## REVISE findings (the genuine residuals under the lens; ≥30% target context below)

**CH2-V3-R-V3-01 — REVISE C10 (ΩC cursor-generality re-anchor clause,
locks-diff.md:77): the parenthetical decision-engine note's e-graph anchor
mis-points.** The clause's parenthetical states the Decision Engine "MUST keep ≥1
asserted e-graph rewrite (`NormalizeDirectSinkCost`, live)" and the clause's
Evidence list cites `skinny/crates/passes/src/backend_egraph.rs:40`-`87`. At HEAD,
`NormalizeDirectSinkCost` is DEFINED at `backend_egraph.rs:191` (`struct`) /
`:193` (`impl Rewrite`); the cited `:40-87` range is the
`select_with_rewrite_policy` consumer, which references the rule at `:75` but does
not define it. The V2 cycle itself pointed the reader to `:191` ("the cited
e-graph rule `NormalizeDirectSinkCost` is live at `backend_egraph.rs:191`"),
confirming the clause's own `:40-87` anchor is a near-miss on the definition site.
Correction: extend the Evidence anchor to
`skinny/crates/passes/src/backend_egraph.rs:75`,`:191`-`193` (the consumer
reference + the rule definition+impl) so the "≥1 asserted e-graph rewrite, live"
claim cites the symbol's definition, not only its call site. This is a
citation-precision residual on a parenthetical Lock-4/Lock-10 note (already
correctly de-coupled from the generality axis per R03), NOT a Lock-14 narrowing —
but a v+1 lock clause must cite the symbol it asserts is live at its definition.

**CH2-V3-R-V3-02 — REVISE D2 (ΩD §13.7 global gates, master-plan-diff.md:140-148):
`generator_grammar_count == 3` is filed under "Global SK-V18 gates (every wave
carries …)" while the SPEC scopes the count-of-3 strictly to PROVE-exit.** The
§13.7 block lists `generator_grammar_count == 3 (json + css + sheets …)` as a
GLOBAL per-wave gate. But the live SPEC binds this symbol to the PROVE wave
exclusively — `sk-v18/SPEC.md:254`: "MUST be 3 at PROVE (json+css+sheets); 7-css
inflation = the P3 overfit, REJECT". Sheets does not enter the generator until
PROVE (and only on a non-`N` verdict, per the very R-V2-01 correction now folded
into C3), so the count is 2 (json+css) through G1-G6 and reaches 3 only at
PROVE-exit. Presenting `== 3` as a flat global-wave gate is in mild tension with
the PROVE-conditional Sheets framing the corpus and C3 carefully established; the
inline `scoped non-JSON witness (3-grammar)` qualifier (folded per R05) cures the
FLEET over-claim but not the PER-WAVE-vs-PROVE-exit scope. Correction: re-file the
`generator_grammar_count == 3` bullet from the "Global SK-V18 gates (every wave)"
list to a PROVE-exit gate — "at the PROVE wave exit, `generator_grammar_count == 3`
(json + css + sheets); through G1-G6 the count is 2 (json + css), Sheets entering
only on a non-`N` PROVE verdict" — mirroring SPEC:254 and the §13.7 PROVE-row
falsifier (master-plan-diff.md:175) so the count is not read as already-3 at
every wave. This is a generality-scope-honesty fix (the witness count must be
wave-gated, not asserted across all waves), NOT a Lock-14 narrowing.

## Non-narrowing / no-revival / no-coupling confirmation (the lens's core, answered)

- **No JSON/CSS-narrowing amendment** (established above; CSS naming is
  demotion-for-honesty across every clause and diff; the substrate and un-fork
  stay grammar-neutral via `backend_shape` dispatch and the `.bbnf` generator).
- **No revived REDRESS route.** REDRESS 51/53 (cursor), 96/97/98 (streamed
  cursor), 246/247 (parse-only structural-stream / string64) all carried
  RETIRED/REJECT and not re-opened; CollapsedStage promotion "must clear that
  retired prior". REDRESS routes are cited as REJECTED boundaries that bound
  G4/G6, never as admitted routes.
- **No coupling that closes a row on x86 / a 6th shape / a sidecar.** 16-lock
  count and 5-shape canon preserved (verified live); aarch64-only (x86 a P1
  DELETION target, no close path); no new directive / BIR variant / public
  substrate API / retained sidecar. The lone V1-flagged coupling (e-graph guard
  on the cursor clause) is de-coupled into a parenthetical Lock-4/Lock-10 note at
  this HEAD (the R-V3-01 residual is its citation anchor, not a re-coupling).
- **No uncited claim in the load-bearing clauses.** Every C1/C2 evidence chain,
  the SPEC:711-712 token form, the MP.NW6/H.W4.LOCK14 standard, the REDRESS
  246/247 anchors, the `strategy.rs` 9-grammar/17-ident roster, the
  `runtime_simd.rs:169` retarget anchor, the `css_provider_source` field
  distinction, and the §13.7 28/64/71 falsifier counts resolve at HEAD. The two
  REVISE items are a definition-site citation extension (R-V3-01) and a
  per-wave-vs-PROVE-exit scope re-file (R-V3-02) — precision fixes, not
  fabrications, narrowings, or non-applying diffs.

## Disposition on the ≥30% V1 REVISE target

The ≥30% figure is the cycle-V1 expectation. V1 met it (5/17 ≈ 29.4%, 6/17 ≈
35.3% counting distinct clauses); V2 honestly fell to 5.9% because four of five
V1 REVISEs were applied and the fifth was a stale read. V3 finds the staged text
further converged — C3, C10, C11, D2 all absorbed their prior corrections — and
surfaces 2 genuine residuals (R-V3-01 citation precision, R-V3-02 per-wave scope),
both GENERALITY-PRESERVING precision fixes, neither a Lock-14 narrowing, a
non-applying diff, a revived REDRESS, nor a coupling. 2 of 17 = 11.8%, BELOW the
V1 target — correctly so on a third convergent cycle: manufacturing additional
REVISEs by re-raising the resolved R01/R02/R03/R04/R05/R-V2-01 items would be a
false adversarial signal. The Ω-C locks amendment respects Lock 14 across
JSON/CSS L4/Sheets/BBNF-self; the Ω-D reconciliation generalises to non-JSON with
no narrowing amendment.

TALLY accept=15 revise=2 reject=0
