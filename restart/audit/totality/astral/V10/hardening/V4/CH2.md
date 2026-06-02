# Pass Omega V10 Hardening — CH2 GENERALITY lens (cycle V4)

Lens: CH2 GENERALITY. (1) Does the Ω-C locks amendment respect Lock 14 across
JSON / CSS L4 / Sheets / BBNF-self — no JSON/CSS-narrowing amendment? (2) Does
the Ω-D master-plan reconciliation generalise to non-JSON? REJECT a non-applying
diff / a revived REDRESS route / a Lock-14 narrowing / a coupling / an uncited
claim. Cycle V1 target: ≥30% REVISE.

Scope reviewed: ΩC-locks-amendments + locks-diff.md (11-clause SK-V18 addendum),
ΩD-master-plan-reconciliation + master-plan-diff.md (6 staged diffs), the
generality-bearing legs of ΩA / ΩB / ΩE / ΩF + migration/handoff deltas, against
live `restart/locks/LOCKS.md`, `restart/MASTER-PLAN.md`,
`crates/ir/src/registry/strategy.rs`, the skinny tree, and the converged
T-P1/T-P2/T-P3 evidence.

## Cycle-V4 standing: V1+V2+V3 REVISE corrections ALL INCORPORATED at this HEAD

V4 runs against staged diffs that have moved past THREE prior cycles. The honest
adversarial job is (a) confirm the prior corrections landed faithfully, (b) NOT
re-raise resolved items to chase the V1 quota, (c) surface only genuine
residuals. The convergence is real and monotone (REVISE rate V1 29.4% → V2 5.9%
→ V3 11.8%). Disposition of the two V3 residuals at this HEAD:

- **CH2-V3-R-V3-01 (C10 e-graph definition-site anchor) — APPLIED.**
  locks-diff.md:77 now reads its Evidence list verbatim
  "`skinny/crates/passes/src/backend_egraph.rs:75` (the `NormalizeDirectSinkCost`
  instantiation in `select_with_rewrite_policy`),`:191`-`193` (the `struct`
  definition + its `impl Rewrite<DecisionNode, NoAnalysis>`)". Verified live:
  `backend_egraph.rs:75` = `let normalize = NormalizeDirectSinkCost;` (the
  consumer inside `select_with_rewrite_policy`, fn at `:40`); `:191` =
  `struct NormalizeDirectSinkCost;`; `:193` = `impl Rewrite<DecisionNode,
  NoAnalysis> for NormalizeDirectSinkCost`. The clause now cites the symbol's
  DEFINITION, not only its call site. RESOLVED.
- **CH2-V3-R-V3-02 (D2 §13.7 `generator_grammar_count == 3` PROVE-exit scope) —
  APPLIED.** master-plan-diff.md:140-152 now reads
  "`generator_grammar_count == 3` … is a PROVE-EXIT gate, NOT a per-wave global:
  through G1-G6 the count is 2 (json + css); Sheets enters the generator only at
  the PROVE wave and only on a non-`N` verdict (`sk-v18/SPEC.md:254`)". Verified
  live: SPEC:254 binds the symbol to PROVE ("MUST be 3 at PROVE (json+css+sheets);
  7-css inflation = the P3 overfit, REJECT"). The count is no longer asserted as
  already-3 at every wave. RESOLVED.

## Spot-Verification (the load-bearing items, run live at HEAD)

- **`git apply --check` on the staged locks-diff: EXIT 0.** The extracted diff
  body applies cleanly against live `restart/locks/LOCKS.md` at HEAD. Hunk header
  `@@ -622,6 +622,33 @@` matches the live anchor: SK-V17 Lock 16 NEON-classifier
  clause ends at `:622`, two blanks at `:623`/`:624`, `## v+1 Governance
  Boundary` at `:625`. The 27-line addendum lands between; the SK-V15
  (`:581`-`607`) and SK-V17 (`:610`-`622`) addenda are neither restated nor edited.
- **master-plan-diff is a multi-section staged proposal, NOT a single applyable
  patch — by design, NOT a REJECT.** The six per-section diffs carry NO
  `diff --git` headers and NO `@@` markers; they are prose-anchored proposal
  hunks the post-G-Omega CRUD applies section-by-section, matching the doc's own
  "STAGED ONLY — NOT APPLIED" framing. Every old-side anchor resolves verbatim
  against the live tree (staging HEAD `25297a7fc`): §13.6 header (`:974`),
  MP.NW6 (`:662`), H.W4.LOCK14 (`:605`). NOT a non-applying-diff REJECT.
- **Live invariants hold.** 16 numbered locks
  (`:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`); five
  `BackendShape` variants `{EagerTape, OffsetTape, EventTape, SinkOnly,
  CollapsedStage}`, no 6th. Both PLANNED co-gate symbols absent
  (`runtime_target_rows_collapsed` rg=0; `bbnf_simd_single_mask_convention` rg=0).
- **§13.7 falsifier counts verified EXACT at HEAD (I re-ran each independently).**
  P1 x86 "today 28": `find skinny/crates/bbnf-simd/src/x86_64
  skinny/crates/bbnf-simd/ext/x86 -type f` = 24 + 4 = **28** (the diff's `…/x86_64
  …/ext/x86` glob is correct; a naive single-subtree `find` returns 24, which is
  why the path-pair matters). P2 leak "today 48 + 16":
  `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` = **48**;
  `bin/gate.rs` = **16**. C11 "71 at HEAD":
  `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l` = 71.
- **REDRESS references resolve, NONE revived.** Item 96 (`:2797`, W3
  class-column REJECT) / 97 (`:2852`, V2 streaming-cursor REJECT) / 98 (`:2910`,
  `G-W3-UNION-SUBSTRATE` RETIRED) carry the M5-Max scalar-cheaper-than-SIMD-cursor
  finding (`:2928`-`:2933`) the CollapsedStage shape-slot clause cites as the
  RETIRED prior ("must clear that retired prior"). Items 51/53 (`:763`/`:2673`
  cursor REJECT) / 246 (`:6186` W11T parse-only structural-STREAM REJECT) / 247
  (`:6232` W11V string64 REJECT) resolve in 1D-skinny-lessons.md:168-171 with
  explicit ADMISSIBLE-vs-REJECTED distinctions; the master-plan-diff uses them as
  REJECTED G4/G6 boundaries, not revived routes.
- **§H waves resolve with the live single-negative-control standard.** MP.NW6
  (`MASTER-PLAN.md:662`) and H.W4.LOCK14 (`:605`) carry the "CSS plus both Sheets
  and BBNF-self … with only one negative control, label the result `scoped
  non-JSON witness`" standard. The C3 clause and §13.7 global-gate bullet inherit
  this verbatim — the un-fork is labelled `scoped non-JSON witness`, NOT fleet-wide.
- **C7 multi-pack census verified ≥4 at HEAD.** `vaddv_u8` shift-add bodies
  present in `byte_class_from_eq_set_64.rs`, `bracket_depth_mask_64.rs`,
  `comment_body_mask_64.rs` (each =2 hits), plus the SHRN duplicate in
  `match_tiny_plain_string.rs`; the one canonical pack is `vshrn_n_u16::<4>` at
  `aarch64/movemask.rs:5`. The "≥4 distinct non-delegating packs at G2 entry"
  claim is accurate, not an undercount.
- **P2 `bin/gate.rs` carve-out does NOT orphan a Lock-14 leak.** SPEC `:614`/`:627`
  binds the P2 owner-path to `nonjson_css_l4.rs` ALONE; SPEC names `bin/gate.rs`
  ZERO times for P2. The 16 `bin/gate.rs` hits are `measure_mbps`/`lightningcss_facts`
  bench-MEASUREMENT identifiers in the perf-gate binary, NOT `match grammar` arms
  or grammar-named modules — they are not a Lock-14 grammar-name leak. The
  carve-out ("NOT a P2 gate target, no SPEC/1D/3B wave owns their retirement") is
  scope-FIDELITY to the SPEC's own file-scoping, not a silently-dropped leak.

## Enumerated Amendments / CRUD Operations Under the CH2 Lens

### Ω-C locks-diff (11 addendum clauses)

| # | Amendment | V1 | V2 | V3 | V4 |
|---|---|---|---|---|---|
| C1 | Named-primitive (a)-(d) gate (Lock 14/16/8) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C2 | Relocated-seam firewall + un-fork (Lock 5/14/1) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C3 | Neutrality-proof clause (Lock 14/16) | REVISE | REVISE | ACCEPT | ACCEPT |
| C4 | aarch64-ONLY clause (Lock 16/8) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C5 | Verbatim-blob-courier clause (Lock 6/14) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C6 | Green-by-exclusion precondition (Lock 14) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C7 | Single-SIMD-substrate + one-movemask (Lock 16) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C8 | Retarget-not-author clause (Lock 16/14) | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| C9 | CollapsedStage shape-slot (Lock 10/16) | ACCEPT | ACCEPT | ACCEPT | REVISE (R-V4-01) |
| C10 | Cursor-generality re-anchor (Lock 14/1/10) | REVISE | ACCEPT | REVISE | ACCEPT (R-V3-01 applied) |
| C11 | Pattern-H re-census clause (Lock 13/14) | REVISE | ACCEPT | ACCEPT | ACCEPT |

### Ω-D master-plan-diff (6 staged diffs)

| # | Amendment | V1 | V2 | V3 | V4 |
|---|---|---|---|---|---|
| D1 | Diff 1 — §13.6 re-key SK-V18 tape-fold → SK-V19 totality-fold | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| D2 | Diff 2 — NEW §13.7 SK-V18 GENERALIZATION 12-wave block | REVISE | ACCEPT | REVISE | ACCEPT (R-V3-02 applied) |
| D3 | Diff 3 — §25 Implementation Order monotonic skinny→totality | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| D4 | Diff 4 — §24 Carry Ledger re-key + SK-V19 tee-up rows | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| D5 | Diff 5 — §5 F.W5 / §13.5 CSS verdict reconciliation | ACCEPT | ACCEPT | ACCEPT | ACCEPT |
| D6 | Diff 6 — §13 H-row + Lock-10 cross-ref label alignment | ACCEPT | ACCEPT | ACCEPT | REVISE (R-V4-02) |

## ACCEPT findings (generality respected; the lens's core question answered)

- **No JSON/CSS-narrowing amendment.** Every Ω-C clause and Ω-D diff that names
  CSS does so as an HONEST scope: the FORCED `balanced_component_scan` →
  `css_balanced_component_scan` demotion (the GROUND s6/C4 finding grounds both
  offered non-CSS dischargers on disk — JSON `{}`/`[]`, Sheets `paren_expr` — and
  finds them structurally incompatible with the byte-SKIP shell, so the CSS name
  is FORCED, not chosen); the skinny-only `css_provider_source == generated`
  side-channel scan; the CSS-scoped recognizer SHELL over a grammar-NEUTRAL eq-set
  sub-kernel carrying its byte-set as CALLER DATA. NO clause restricts the
  substrate or the un-fork TO JSON/CSS. The substrate stays grammar-neutral
  (`backend_shape` dispatch, `.bbnf`-driven generator, 9-grammar `strategy.rs`
  manifest as DATA). CSS naming is demotion-for-honesty — the OPPOSITE of narrowing.
- **C1/C2 grammar-neutral by construction.** The (a)-(d) gate conjuncts
  ("grammar-INVOKED-by-name", "output VARIES under invoking-rule mutation") apply
  to ANY grammar's hot leaf; the gate's named admits are BOTH the CSS
  `balanced_component_scan` AND the JSON `string`/`number` scanners. The firewall
  dispatches `render(program)` on `backend_shape` with `RuntimeEmitterKind`
  DELETED at the post-G3 end-state — the un-fork that makes adding a grammar a
  config change (the direct Lock-14 spirit). md5-NECESSARY-NOT-SUFFICIENT + the
  PLANNED `runtime_target_rows_collapsed` `PartialEq` full-row co-gate is the
  correct guard against a per-grammar branch relocated into a neutral data table.
  iburg citation (DOI 10.1145/151640.151642) is the right back-end-stage precedent.
- **C3/C10/C11/D2 all ACCEPT at this HEAD** — their V1/V3 corrections (the
  `scoped non-JSON witness` label inherited from MP.NW6/H.W4.LOCK14 verbatim, the
  by-construction→by-exercise PROVE-conditional tense, the e-graph definition-site
  anchor, the `generator_grammar_count == 3` PROVE-exit re-file) are all folded
  and re-verified live above. None narrows Lock 14; all four route the 9-grammar
  fleet close forward to SK-V19 rather than over-claiming it at SK-V18.
- **D1/D3/D4/D5.** The identity pivot (MASTER's "SK-V18" `crates/core` fold →
  SK-V19; the certified GENERALIZATION cycle → §13.7) is generality-CORRECT:
  monotonic skinny→totality preserved, 9-grammar fleet adoption routed to SK-V19.
  D5 carries the un-caveated-"MEASUREMENT-VALID" strike (keeps
  "directionally-valid pending H1 re-lock"); D4's §24 tee-up routes the 9-ident
  table / css_types.rs / `simd-scan`-vs-`bbnf-simd` asymmetry / BBNF-self litmus
  to SK-V19 with explicit Lock-14(c) declaration-crate reasoning — no silent drop.

## REVISE findings (the genuine residuals under the lens)

**CH2-V4-R-V4-01 — REVISE C9 (ΩC CollapsedStage shape-slot clause,
locks-diff.md:75): the in-clause `:1206` ARCHITECTURE secondary-reference is
described as carrying a clause that the SAME-staged `architecture-delta.staged.md`
OA-V10-05 DEMOTES, creating a cross-artefact reference that may dangle after the
companion CRUD lands — and the clause never names the post-demotion line the
secondary reference should re-anchor to.** The C9 clause (locks-diff.md:75)
retains `restart/ARCHITECTURE.md:1206` as the SECONDARY ledger reference and
states the OA-V10-05 splice "demotes its x86 co-require WORDING but PRESERVES the
`aarch64 candidate is UNKNOWN-2D-05 …` clause verbatim". Verified live: ARCH:1206
IS the CollapsedStage ledger row pinned to `x86 AVX-512 collapsed-stage FSM` /
`target.arch == x86` (it reads "CollapsedStage | x86 AVX-512 collapsed-stage FSM
| asmjson AVX-512 …"), and OA-V10-05 (ΩA-coherence-audit.md:136-145, CF-05) marks
`:1206` as one of the four in-body demote-to-diagnostic splice targets
(`:1151`/`:1171`/`:1186`/`:1206`). The clause correctly elects the demote-STABLE
`:1289` U3 directive as PRIMARY (NOT in the OA-V10-05 splice set, verified: ARCH:1289
carries the same "no admission without a 2E source-backed strategy" bar). But the
SECONDARY `:1206` reference is asserted to "PRESERVE the `aarch64 candidate is
UNKNOWN-2D-05 (requires 2E source-backed aarch64 strategy …)` clause verbatim" —
and that exact preserved wording is NOT present at the live `:1206` (which is the
x86-pinned ledger ROW being demoted, whose aarch64-candidate annotation moves
under the splice). The clause cites a post-splice line that does not yet exist,
so a LOCKS reader at the live HEAD cannot resolve the "verbatim preserved" clause
at `:1206`. This is not a Lock-14 narrowing and not a coupling — it is a
forward-reference into a post-CRUD line that the locks addendum cannot guarantee
absent an apply-order constraint. Correction: either (a) add an explicit
ordering note that the `:1206` secondary reference is valid only POST-OA-V10-05
demotion and re-cite the PRIMARY `:1289` U3 directive as the AT-HEAD-resolvable
anchor (which it already is), or (b) drop the `:1206` secondary reference and let
the `:1289` U3 primary carry the whole bar — so the LOCKS clause cites only lines
resolvable at the live HEAD it is amending, never a line the companion staged diff
must first produce. (The PRIMARY `:1289` anchor is already AT-HEAD-resolvable, so
this is a secondary-reference hygiene fix, not a load-bearing-anchor failure.)

**CH2-V4-R-V4-02 — REVISE D6 (ΩD §13 H-row + Lock-10 cross-ref label alignment,
master-plan-diff Diff 6): the master-plan-diff's own per-delta table
(ΩD-master-plan-reconciliation.md:64-66) maps `MP-3B-SKV18-D05` to
"`css_balanced_component_scan` CSS-scoped" and `MP-3B-SKV18-D06` to the Sheets
negative control, but Diff 6's H-row label alignment is the ONE staged diff whose
old-side anchors I could not resolve to a single cited live H-row line in the
diff body — the §13.7 receiver names H.W4/H.W6/H.W2.5/J.W1 cross-refs inline, yet
Diff 6 ("§13 H-row + Lock-10 cross-ref label alignment") carries no per-row
old-side `:NNN` anchor in the reconciliation doc the way Diffs 1-5 do (§13.6:974,
§5 F:196, F.W5:519, §24:1346, §13.5:912 all cite exact lines).** This is a
citation-completeness residual on the lowest-churn diff: D6 is a label-alignment
pass whose generality content (it touches no grammar branch, adds no shape, only
re-labels H-row cross-references) is sound and non-narrowing, but the staged
proposal should cite the exact H-row line(s) it re-labels so the CRUD-2 operator
can re-grep-before-apply the same bounded way Diffs 1-5 permit. Correction: add
the per-row old-side `:NNN` anchors for the H-rows D6 re-labels (the H.W4/H.W6/
H.W2.5 rows live at `MASTER-PLAN.md:644`-`648` region, the Lock-10 5-shape cross-ref
at `:616`/`:945`/`:996`) to the Diff 6 hunk so every staged diff carries the same
re-grep-HALT guarantee. This is a staging-hygiene precision fix, NOT a Lock-14
narrowing, a non-applying diff, a revived REDRESS, or a coupling — D6's label
alignment generalises across all H-rows uniformly and narrows no grammar.

## Non-narrowing / no-revival / no-coupling confirmation (the lens's core, answered)

- **No JSON/CSS-narrowing amendment** (established above; CSS naming is
  demotion-for-honesty across every clause and diff; the substrate and un-fork
  stay grammar-neutral via `backend_shape` dispatch and the `.bbnf` generator;
  the eq-set sub-kernel carries its byte-set as caller data, neutral even under a
  CSS-scoped shell).
- **No revived REDRESS route.** REDRESS 51/53 (cursor), 96/97/98 (streamed
  cursor, the M5-Max scalar-cheaper finding), 246/247 (parse-only structural-stream
  / string64) all carried RETIRED/REJECT and not re-opened; CollapsedStage
  promotion "must clear that retired prior". REDRESS routes are cited as REJECTED
  G4/G6 boundaries, never as admitted routes.
- **No coupling that closes a row on x86 / a 6th shape / a sidecar.** 16-lock
  count and 5-shape canon preserved (verified live); aarch64-only (x86 a P1
  DELETION target, no close path — and the P1 falsifier "today 28" is exactly the
  src/x86_64 (24) + ext/x86 (4) census); no new directive / BIR variant / public
  substrate API / retained sidecar. The e-graph guard on the C10 cursor clause is
  de-coupled into a parenthesised Lock-4/Lock-10 note ("NOT a grammar-generality
  invariant"), and its anchor now cites the definition site (R-V3-01 applied).
- **No uncited claim in the load-bearing clauses.** Every C1/C2 evidence chain,
  the SPEC:711-712 token form, the MP.NW6/H.W4.LOCK14 standard, the REDRESS
  96/97/98/246/247 anchors, the `strategy.rs` 9-grammar/17-ident roster, the
  C7 ≥4-pack census, the §13.7 28/48+16/71 falsifier counts, and the SPEC:254
  PROVE-exit `generator_grammar_count` binding all resolve at HEAD. The two REVISE
  items are a secondary-reference forward-citation hygiene fix (R-V4-01) and a
  per-row anchor-completeness fix on the lowest-churn diff (R-V4-02) — precision
  fixes, not fabrications, narrowings, or non-applying diffs.

## Disposition on the ≥30% V1 REVISE target

The ≥30% figure is the cycle-V1 expectation. V1 met it (5/17 ≈ 29.4%); V2 fell to
5.9% and V3 to 11.8% as the staged text converged. V4 finds the text further
converged — both V3 residuals (R-V3-01 e-graph anchor, R-V3-02
`generator_grammar_count` PROVE-exit scope) are folded and re-verified live — and
surfaces 2 genuine residuals (R-V4-01 the C9 `:1206` secondary-reference forward
citation into a post-OA-V10-05-demotion line; R-V4-02 the Diff 6 per-row H-row
anchor completeness). 2 of 17 = 11.8%, BELOW the V1 target — correctly so on a
fourth convergent cycle: manufacturing additional REVISEs by re-raising the
resolved R01-R05 / R-V2-01 / R-V3-01 / R-V3-02 items would be a false adversarial
signal. Both V4 residuals are GENERALITY-PRESERVING staging-hygiene precision
fixes, neither a Lock-14 narrowing, a non-applying diff, a revived REDRESS, nor a
coupling. The Ω-C locks amendment respects Lock 14 across JSON / CSS L4 / Sheets /
BBNF-self; the Ω-D reconciliation generalises to non-JSON with no narrowing
amendment.

TALLY accept=15 revise=2 reject=0
