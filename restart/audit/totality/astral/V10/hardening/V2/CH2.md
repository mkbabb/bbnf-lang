# Pass Omega V10 Hardening — CH2 GENERALITY lens (cycle V2)

Lens: CH2 GENERALITY. (1) Does the Ω-C locks amendment respect Lock 14 across
JSON / CSS L4 / Sheets / BBNF-self? (2) Does the Ω-D master-plan reconciliation
generalise to non-JSON — i.e. NO JSON/CSS-narrowing amendment? REJECT a
non-applying diff / a revived REDRESS route / a Lock-14 narrowing / a coupling /
an uncited claim. Cycle V1 target was ≥30% REVISE.

Scope reviewed: ΩC-locks-amendments + locks-diff.md (11-clause SK-V18 addendum),
ΩD-master-plan-reconciliation + master-plan-diff.md (6 staged diffs), the
generality-bearing legs of ΩA/ΩB/ΩE/ΩF, against live `restart/locks/LOCKS.md`,
`restart/MASTER-PLAN.md`, `crates/ir/src/registry/strategy.rs`, the skinny tree,
and the converged T-P1/T-P2/T-P3 evidence.

## Cycle-V2 standing: the V1 REVISE corrections have been INCORPORATED

Cycle V2 is run against staged diffs that have MOVED since cycle V1 reviewed them.
The byte content of `locks-diff.md` / `master-plan-diff.md` at this HEAD already
carries the V1-cycle corrections, and one V1 REVISE was itself a stale-read error.
The adversarial job in V2 is therefore to (a) confirm the corrections landed
faithfully against the live surfaces, (b) NOT re-raise resolved items, and (c)
surface only genuine residuals. The honest REVISE rate falls below the V1 30%
target precisely BECAUSE the corrections were applied — re-flagging resolved items
to hit a quota would be a false adversarial signal.

Disposition of the five V1 REVISE items at this HEAD:

- **CH2-V1-R01 (scoped-non-JSON-witness label) — APPLIED.** locks-diff.md:63 now
  reads verbatim: "Per the live MP.NW6 (`restart/MASTER-PLAN.md:662`) / H.W4.LOCK14
  (`:605`) single-negative-control standard, with only ONE non-CSS-non-JSON
  negative control the result is labelled a `scoped non-JSON witness` … SK-V18's
  un-fork generality is a `scoped non-JSON witness` (CSS + a single Sheets
  control)". Verified live: MP.NW6 (`:662`), H.W4.LOCK14 (`:605`), and MP.NW11
  (`:667`) all carry the `scoped non-JSON witness` standard byte-for-byte. RESOLVED.
- **CH2-V1-R02 (Sheets by-construction→by-exercise reconcile) — APPLIED.**
  locks-diff.md:63 now states: "SK-V18 PROMOTES Sheets from by-construction (the
  SK-V17 `:620` clause … `sheets_witness` 24-LOC stub) to a by-EXERCISE negative
  control at PROVE (binding fallback `N`)". The `:620` SK-V17 clause it sits beside
  is verified live ("Sheets/BBNF-self are by-construction under SK-V18, NOT
  by-exercise (`sheets_witness` 24-LOC stub)"). The delta is now stated, not
  silently contradicting. RESOLVED (with a narrow residual, R-V2-01 below).
- **CH2-V1-R03 (cursor-generality 8-of-9 + e-graph coupling) — APPLIED.**
  locks-diff.md:77 now reads "the SK-V17 8-of-9 figure is NOT re-verified at the
  SK-V18 HEAD census post-P3-collapse, so this re-census is owed at SK-V18 G4/G5
  with the census command cited, not carried as a settled count" — exactly the V1
  softening. The e-graph regression-guard is now parenthesised as a "Decision-engine
  soundness note, carried for the Lock-4/Lock-10 decision-engine clause and NOT a
  grammar-generality invariant" — the V1 de-coupling. The cited e-graph rule
  `NormalizeDirectSinkCost` is live at `backend_egraph.rs:191`. RESOLVED.
- **CH2-V1-R04 (Pattern-H "4-grammar table" / "9-row nonexistent") — V1 WAS WRONG;
  diff CORRECT at HEAD.** This is the load-bearing inversion. V1 asserted the live
  `strategy.rs:137-185` table is "a 4-grammar table (Json, GoogleSheets, CssL4,
  Bbnf — NOT a 9-row table)" and that the diff's "9-row" prescribes a nonexistent
  artefact. **The live `PRODUCTION_MANIFEST_TABLE` registers NINE grammars**
  (Json, GoogleSheets, CssL4, Bbnf, Csv, Math, Bnf, Ebnf, CssPretty) with
  **seventeen ident strings** (CssL4 carrying only `CssL4Parser`; the other eight
  carrying 2 each → 8×2+1 = 17), verified by direct count over `strategy.rs:134-189`.
  The V10 diff line 79 — "registers 9 grammars (Json, GoogleSheets, CssL4, Bbnf,
  Csv, Math, Bnf, Ebnf, CssPretty — 17 ident strings, CssL4 carrying only
  `CssL4Parser`)" — is FULLY CORRECT at HEAD, and is faithful to its converged
  source (2C LAC-2C-SK18-02/03 both use "9-row roster"/"the 9-row grammar-named
  idents table … catches 4 of 9"). The "narrow 4-name leak regex catches only 4 of
  those 9 … literal 13-crate scan returns 13" is verified: the live Lock-14
  verification regex lists exactly 4 names (`JsonParser|CssL4Parser|BbnfBootstrap|
  GoogleSheetsParser`) and the 13-crate grep returns 13. C11 is ACCEPT, not REVISE.
- **CH2-V1-R05 (§13.7 inline 3-grammar caveat) — APPLIED.** master-plan-diff.md:142
  now carries the global-gate bullet: "This is the `scoped non-JSON witness`
  (3-grammar) un-fork per the live MP.NW6 … the F.W5 nine-grammar close is FED by
  this generator and adopted at SK-V19 scale — §13.7 does NOT satisfy F.W5". Diff 5
  (master-plan-diff.md:284-290) carries the matching CSS-verdict caveat and strikes
  the un-caveated "MEASUREMENT-VALID". RESOLVED.

## Spot-Verification (the load-bearing items, run live)

- **`git apply --check` on the staged locks-diff: EXIT 0.** The 37-line extracted
  diff body applies cleanly against live `restart/locks/LOCKS.md` at HEAD. Hunk
  header `@@ -622,6 +622,33 @@` matches the live anchor: the SK-V17 Lock 16
  NEON-classifier clause ends at `:622`, two blanks at `:623`/`:624`,
  `## v+1 Governance Boundary` at `:625`. The 27-line addendum lands between; the
  SK-V15 (`:581`-`607`) and SK-V17 (`:610`-`622`) addenda are neither restated nor
  edited.
- **master-plan-diff is a multi-section staged proposal, not a single applyable
  patch.** Concatenated extraction fails `git apply --check` ("No valid patches")
  because the six per-section diffs carry no `diff --git` file headers — they are
  proposal hunks the post-G-Omega CRUD pass applies section-by-section. This is
  by-design and matches the doc's own "STAGED ONLY — NOT APPLIED" framing; it is
  NOT a non-applying-diff REJECT.
- **Live invariants hold.** 16 numbered locks (`grep -cE '^[0-9]+\. \*\*'` = 16);
  five `BackendShape` variants at `lower/mod.rs:20-24` + `all_backend_shapes() ->
  [BackendShape; 5]` at `cost.rs:334`, no 6th.
- **C2 firewall claims TRUE at HEAD.** `RuntimeEmitterKind::{CompiledLowering,
  RequestFacts}` still live at `runtime_generator.rs:17-25` (the "un-fork UNBUILT
  at HEAD" claim). The skinny CSS scan target (`grammars/css_l4_*`, 7 dirs) is
  distinct from the totality `crates/core/src/runtime/css_l4/` SK-V19-DEFER seam —
  the firewall correctly scopes its skinny scan and routes the totality surface
  forward, NO narrowing.
- **REDRESS references resolve, none revived.** Item 246 (`:6186`) closes
  `G-SK-V14-W11T-JSON-PARSE-ONLY-STRUCTURAL-STREAM` as REJECT — the master-plan-diff's
  "item 246 = the W11T parse-only structural-STREAM driver reject that bounds G4".
  Item 247 (`:6232`) closes `G-SK-V14-W11V-JSON-PARSE-ONLY-STRING64` as REJECT.
  REDRESS 51/53 (cursor REJECTED, `:742`/`:784`) live, not reopened. REDRESS 96/97/98
  streamed-cursor + the M5-Max scalar-cheaper-than-SIMD-cursor finding (`:2916`,
  `:2928`-`2933`) is the CollapsedStage clause's retired prior, correctly carried.
- **Retarget-not-author anchor verified.** `runtime_simd.rs:169` `find_css_significant`
  carries its byte-set as CALLER DATA (`delimiters`, `fixed`); the kernel "names no
  grammar" — the exact neutral-sub-kernel / CSS-scoped-shell two-axis split C8 claims.
- **§H waves resolve.** H.W4.LOCK14 (`:605`), H.W5 x86-successor (`:606`), H.W6
  CSS-receiver (`:607`) match the master-plan-diff cross-refs.

## Enumerated Amendments / CRUD Operations Under the CH2 Lens

### Ω-C locks-diff (11 addendum clauses)

| # | Amendment | V1 | V2 |
|---|---|---|---|
| C1 | Named-primitive (a)-(d) gate (Lock 14/16/8) | ACCEPT | ACCEPT |
| C2 | Relocated-seam firewall + un-fork (Lock 5/14/1) | ACCEPT | ACCEPT |
| C3 | Neutrality-proof clause (Lock 14/16) | REVISE | REVISE |
| C4 | aarch64-ONLY clause (Lock 16/8) | ACCEPT | ACCEPT |
| C5 | Verbatim-blob-courier clause (Lock 6/14) | ACCEPT | ACCEPT |
| C6 | Green-by-exclusion precondition (Lock 14) | ACCEPT | ACCEPT |
| C7 | Single-SIMD-substrate + one-movemask (Lock 16) | ACCEPT | ACCEPT |
| C8 | Retarget-not-author clause (Lock 16/14) | ACCEPT | ACCEPT |
| C9 | CollapsedStage shape-slot (Lock 10/16) | ACCEPT | ACCEPT |
| C10 | Cursor-generality re-anchor (Lock 14/1/10) | REVISE | ACCEPT (R03 applied) |
| C11 | Pattern-H re-census clause (Lock 13/14) | REVISE | ACCEPT (R04 was a stale read) |

### Ω-D master-plan-diff (6 staged diffs)

| # | Amendment | V1 | V2 |
|---|---|---|---|
| D1 | Diff 1 — §13.6 re-key SK-V18 tape-fold → SK-V19 totality-fold | ACCEPT | ACCEPT |
| D2 | Diff 2 — NEW §13.7 SK-V18 GENERALIZATION 12-wave block | REVISE | ACCEPT (R05 applied) |
| D3 | Diff 3 — §25 Implementation Order monotonic skinny→totality | ACCEPT | ACCEPT |
| D4 | Diff 4 — §24 Carry Ledger re-key + SK-V19 tee-up rows | ACCEPT | ACCEPT |
| D5 | Diff 5 — §5 F.W5 / §13.5 CSS verdict reconciliation | ACCEPT | ACCEPT |
| D6 | Diff 6 — §13 H-row + Lock-10 cross-ref label alignment | ACCEPT | ACCEPT |

## ACCEPT findings (generality respected; the lens's core question answered)

- **No JSON/CSS-narrowing amendment.** Every Ω-C clause and Ω-D diff that names
  CSS does so as an HONEST scope: the FORCED `balanced_component_scan` →
  `css_balanced_component_scan` demotion (locks-diff.md:63; the GROUND s6/C4
  finding at SPEC:972-983 grounds both offered non-CSS dischargers on disk and
  finds them structurally incompatible with the byte-SKIP shell, so the CSS-scoped
  name is FORCED, not chosen); the `css_provider_source == generated` skinny-only
  side-channel scan; the CSS-scoped recognizer SHELL over a grammar-NEUTRAL eq-set
  sub-kernel (caller-data). No clause restricts the substrate or the un-fork TO
  JSON/CSS. The substrate stays grammar-neutral (`backend_shape` dispatch,
  `.bbnf`-driven generator, 9-grammar `PRODUCTION_MANIFEST_TABLE` as DATA). The CSS
  naming is demotion-for-honesty, the OPPOSITE of narrowing.
- **C1/C2 grammar-neutral by construction.** The (a)-(d) gate conjuncts
  ("grammar-INVOKED-by-name", "output VARIES under invoking-rule mutation") apply
  to ANY grammar's hot leaf. The firewall dispatches `render(program)` on
  `backend_shape` with `RuntimeEmitterKind` DELETED at the post-G3 end-state — the
  un-fork that makes adding a grammar a config change (the direct Lock-14 spirit).
  md5-NECESSARY-NOT-SUFFICIENT + the PLANNED `runtime_target_rows_collapsed` co-gate
  is the correct guard against a per-grammar branch relocated into a data table.
  iburg citation (DOI 10.1145/151640.151642) verified.
- **C6 green-by-exclusion** carries the certified SPEC:711-712 token form
  `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}` (verified live at
  SPEC:711-712); the `*EventGrammar` glob is alias-immune (catches
  `SHEETS_EventGrammar`/`JsonEventGrammar` re-injects). Fleet-wide token set, NOT a
  narrowing.
- **C10 now ACCEPT.** The R03-flagged "8 of 9" is re-cast as re-census-owed at
  G4/G5 ("NOT re-verified at HEAD … not carried as a settled count"); the e-graph
  guard is de-coupled into a parenthetical Lock-4/Lock-10 note explicitly "NOT a
  grammar-generality invariant". The generality axis the lens governs is clean.
- **C11 now ACCEPT.** The 9-grammar / 17-ident roster is correct at HEAD and
  faithful to the 2C source. The "do NOT bolt a 9-name regex widen as an SK-V18
  patch; the R16 PartialEq collapse + roster-wide regex + `css_types.rs` relocation
  are SK-V19" disposition routes the leak forward without narrowing or revived
  REDRESS, and the `css_types.rs` relocation cites Lock 14(c) declaration-crate
  admissibility.
- **D2 now ACCEPT.** The §13.7 block carries the inline `scoped non-JSON witness`
  (3-grammar) caveat (master-plan-diff.md:142-147); `generator_grammar_count == 3`
  (json+css+sheets, the P3 collapse) is correctly capped and the 9-fleet routed to
  SK-V19 (§24 Diff 4), with the CH3-V1-R2 retime blocking G2/G4/G6 entry until the
  SK-V16/V17 REDRESS reconcile is committed. No fleet over-claim from §13.7 alone.
- **D1/D3/D4/D5/D6.** The identity pivot (MASTER's "SK-V18" `crates/core` fold →
  SK-V19; the certified GENERALIZATION cycle → §13.7) is generality-CORRECT:
  monotonic skinny→totality preserved, 9-grammar fleet adoption routed to SK-V19
  rather than over-claimed at SK-V18. D4's §24 tee-up routes the
  9-ident-table / css_types.rs / `simd-scan`-vs-`bbnf-simd` / BBNF-self litmus to
  SK-V19 with explicit Lock-14(c) reasoning, no silent drop, no narrowing.

## REVISE finding (the one genuine residual under the lens)

**R-V2-01 — REVISE C3 (ΩC neutrality-proof clause, locks-diff.md:63):** the
generality WITNESS COUNT is asserted as an at-HEAD fact where it is in truth a
PROVE-conditional outcome. The clause states "the un-fork witness set is JSON+CSS
**exercised** + Sheets **exercised-as-negative-control**" — but the live SPEC PROVE
wave (`sk-v18/SPEC.md:143`-`150`,`:214`,`:221`-`223`) makes the Sheets exercise a
PLANNED wave outcome whose BINDING FALLBACK is `N` ("Sheets cannot emit via the
generator ONLY → generalization NOT real"). At the time this lock clause is
written (PROVE undispatched), the third exercised witness DOES NOT YET EXIST; it
materialises only if the PROVE wave returns a non-`N` verdict. Stating Sheets as
already "exercised" overstates a future-tense, fallback-`N`-gated result — the same
scope-honesty defect the `scoped non-JSON witness` label was added to cure, leaking
back into the by-exercise sentence the clause itself adds. This is a genuine
generality residual (a generality count must be PROVE-GATED, not asserted at HEAD),
NOT a re-raise of the resolved R02 — R02 corrected the by-construction-vs-by-exercise
SILENT CONTRADICTION against `:620`; R-V2-01 corrects the remaining TENSE/MODALITY
overstatement. Correction: re-cast as PROVE-conditional — "SK-V18's PROVE wave
PROMOTES Sheets to a by-EXERCISE negative control IF AND ONLY IF PROVE returns
non-`N` (binding fallback `N` = generalization NOT real, surfaced honestly); the
exercised witness set is JSON+CSS-exercised at G1/G2 + Sheets-exercised-IF-PROVE-passes,
and remains a `scoped non-JSON witness` either way" — so the third-witness claim
inherits the PROVE wave's own falsifier rather than presupposing its success.

## Non-narrowing / no-revival / no-coupling confirmation

- **No JSON/CSS-narrowing amendment** (established above; CSS naming is
  demotion-for-honesty across every clause and diff).
- **No revived REDRESS route.** REDRESS 51/53 (cursor), 96/97/98 (streamed cursor),
  246/247 (parse-only structural-stream / string64) all carried RETIRED/REJECT and
  not reopened; CollapsedStage promotion "must clear that retired prior".
- **No coupling that closes a row on x86 / a 6th shape / a sidecar.** 16-lock count
  and 5-shape canon preserved (verified live); aarch64-only (x86 a P1 DELETION
  target, no close path); no new directive / BIR variant / public substrate API /
  retained sidecar. The lone V1-flagged coupling (e-graph guard on the cursor
  clause) is de-coupled at this HEAD.
- **No uncited claim in the load-bearing clauses.** Every C1/C2 evidence chain, the
  SPEC:711-712 token form, the MP.NW6/H.W4.LOCK14 standard, the REDRESS 246/247
  anchors, the `strategy.rs` 9-grammar/17-ident roster, and the `runtime_simd.rs:169`
  retarget anchor resolve at HEAD.

REVISE rate: 1 of 17 enumerated amendments = 5.9%. This is BELOW the cycle-V1 30%
target — correctly so: the V10 staged diffs INCORPORATED four of V1's five REVISE
corrections (R01/R02/R03/R05 applied) and the fifth (R04) was a V1 stale-read error
the diff was already right about (9 grammars, not 4). Forcing additional REVISEs to
hit a quota would be a false adversarial signal; the single residual (R-V2-01) is
the only genuine generality defect remaining, and it is a tense/modality scope-honesty
fix, NOT a Lock-14 narrowing, a non-applying diff, a revived REDRESS, or a coupling.

TALLY accept=16 revise=1 reject=0
