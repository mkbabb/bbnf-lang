# Pass Omega V10 Hardening — CH2 GENERALITY lens (cycle V5)

Lens: CH2 GENERALITY. (1) Does the Ω-C locks amendment respect Lock 14 across
JSON / CSS L4 / Sheets / BBNF-self — no JSON/CSS-narrowing amendment? (2) Does
the Ω-D master-plan reconciliation generalise to non-JSON? REJECT a non-applying
diff / a revived REDRESS route / a Lock-14 narrowing / a coupling / an uncited
claim. Cycle V1 target: ≥30% REVISE.

Scope reviewed: ΩC-locks-amendments + locks-diff.md (11-clause SK-V18 addendum),
ΩD-master-plan-reconciliation + master-plan-diff.md (6 staged diffs), the
generality-bearing legs of ΩA / ΩB / ΩE / ΩF + architecture/migration/handoff
deltas, against live `restart/locks/LOCKS.md`, `restart/MASTER-PLAN.md`,
`restart/ARCHITECTURE.md`, `crates/ir/src/registry/strategy.rs`, the skinny tree,
and the converged T-P1/T-P2/T-P3 evidence.

## Cycle-V5 standing: V1-V4 REVISE corrections re-verified; one overshoot found

V5 runs against staged diffs that have moved past FOUR prior cycles (REVISE rate
V1 29.4% → V2 5.9% → V3 11.8% → V4 11.8%). The locks-diff (22:42) and
master-plan-diff (22:40) were both re-staged AFTER the V4 verdict (22:38), so the
honest job is (a) confirm the V4 corrections landed, (b) NOT re-raise resolved
items, (c) surface only genuine residuals — including any defect the V4
correction itself introduced.

Disposition of the two V4 residuals at this HEAD:

- **CH2-V4-R-V4-02 (Diff 6 per-row H-row anchor completeness) — APPLIED.**
  master-plan-diff.md Diff 6 (`:307`-`322`) now carries the per-row old-side
  anchors: H.W1 (`:642`), H.W4 (`:646`), the Lock-10 inheritance row (`:616`),
  the §13 preamble (`:584`-`:592`). Resolved live: `:642` = "H.W1 (typed event
  cursor over tape projection — load-bearing)"; `:646` = "H.W4 (workload gates +
  direct-to-struct `SinkOnly` closure + 5-shape backend_shape per-rule)"; `:616`
  = "Lock 10. | Pratt and SIMD are auto-detected"; `:584` = the §13 preamble
  carrying the SK-V18 / §13.6 cross-refs. Every D6 site is now re-grep-HALT
  bounded the same way Diffs 1-5 are. RESOLVED.
- **CH2-V4-R-V4-01 (C9 `:1206` secondary-reference) — APPLIED but OVERSHOT;
  re-raised as CH2-V5-R-V5-01.** The C9 clause (locks-diff.md:75) was re-authored
  with the requested apply-order note (PRIMARY `:1289` U3 directive demote-stable
  + AT-HEAD-resolvable; `:1206` SECONDARY apply-order-dependent + a HALT-anchor
  note in the architecture-delta). That part is correct. But the re-author added
  a NEW factual error in fixing V4 — see R-V5-01 below.

## Spot-Verification (the load-bearing items, run live at HEAD)

- **`git apply --check` on the staged locks-diff: EXIT 0.** The extracted diff
  body applies cleanly against live `restart/locks/LOCKS.md` at HEAD. Hunk header
  `@@ -622,6 +622,33 @@` matches the live anchor: SK-V17 Lock 16 NEON-classifier
  clause ends at `:622`, two blanks at `:623`/`:624`, `## v+1 Governance
  Boundary` at `:625`. The 27-line addendum lands between; the SK-V15
  (`:581`-`607`) and SK-V17 (`:610`-`622`) addenda are neither restated nor edited.
- **master-plan-diff is a multi-section staged proposal, NOT a single applyable
  patch — by design, NOT a REJECT.** The six per-section diffs carry NO
  `diff --git` headers; they are prose-anchored proposal hunks the post-G-Omega
  CRUD applies section-by-section, matching the doc's "STAGED ONLY — NOT APPLIED"
  framing. Every old-side anchor resolves verbatim against the live tree (staging
  HEAD `25297a7fc`): §13.6 header (`:974`), MP.NW6 (`:662`), H.W4.LOCK14 (`:605`).
- **Live invariants hold.** 16 numbered locks
  (`:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`); Lock 14
  at `:349` is the "Full grammar generalisation; zero overfitting" lock — the
  very surface this lens guards. Five `BackendShape` variants
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, no 6th. Both
  PLANNED co-gate symbols absent (`runtime_target_rows_collapsed` rg=0;
  `bbnf_simd_single_mask_convention` rg=0).
- **§H waves resolve with the live single-negative-control standard (the lens's
  core anchor).** MP.NW6 (`MASTER-PLAN.md:662`): "Lock 14 generated … repair with
  CSS plus both Sheets and BBNF-self … with only one negative control, label the
  result `scoped non-JSON witness`, not fleet-wide or grammar-neutral closure".
  H.W4.LOCK14 (`:605`) carries the same "CSS plus both Sheets and BBNF-self …"
  standard. The C3 neutrality clause and the §13.7 global-gate bullet inherit
  this VERBATIM: the un-fork is labelled `scoped non-JSON witness`, and
  fleet-wide / grammar-neutral wording requires SK-V19 adoption OR both Sheets
  AND BBNF-self witnesses in one wave (confirmed:
  `grep -c "both Sheets AND BBNF-self negative-control witnesses in the same wave"`
  = 1 in locks-diff; LAC-2C-SK18-02 at `2C-grammar-neutrality.md:380`-`381`
  partners it). This is the OPPOSITE of a narrowing.
- **REDRESS references resolve, NONE revived.** Item 246 (`skinny/REDRESS.md:6184`,
  "SK-V14 W11T Parse-Only Structural Stream Reject" — a structural-stream parse_only
  DRIVER = second substrate, REJECT) bounds G4 per 1D:168-171; item 247
  (`:6230`, W11V string64 REJECT) bounds G2; items 51/53 (`742-768`/`784-813`,
  cursor REJECT) bound G6 NEON retarget. 1D-skinny-lessons.md:168-171 carries the
  explicit ADMISSIBLE-vs-REJECTED distinctions; the master-plan-diff uses them as
  REJECTED G4/G6 boundaries, not revived routes. CollapsedStage REDRESS 96/97/98
  carried RETIRED (M5-Max scalar-cheaper-than-SIMD-cursor finding, `:2928`-`2933`).
- **No JSON/CSS-narrowing in any clause or diff.** The FORCED
  `balanced_component_scan` → `css_balanced_component_scan` demotion is grounded
  at SPEC `:972`-`983`: BOTH offered non-CSS dischargers (JSON `{}`/`[]` at
  `generated.rs:833-834`; Sheets `paren_expr` at `google-sheets.bbnf:137`) are
  PARSE-with-emit descents structurally incompatible with the CSS byte-SKIP shell
  `consume_balanced_at`, so the CSS name is FORCED — demotion-for-honesty, the
  opposite of narrowing. The inner eq-set sub-kernel stays grammar-neutral
  (caller-data byte-set) independently of the CSS-scoped SHELL. The
  `FORBIDDEN_GENERIC_TOKENS ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar,
  *EventGrammar}` form (SPEC `:711`-`712`) is GENERALITY-CORRECT: the `GENERATED_RS`
  suffix substring catches a future `SHEETS_GENERATED_RS` courier too (verified
  the live couriers `CSS_GENERATED_RS`/`JSON_PARSE_ONLY_GENERATED_RS` at
  `runtime_generator.rs:91`,`:195`,`:701`), and the `*EventGrammar` glob is
  alias-immune across grammars — the token set is not JSON/CSS-narrowed.
- **`css_provider_source` live-vs-planned disambiguation is honest.** The C2
  firewall clause names `css_provider_source == generated` as a PLANNED firewall
  predicate AND explicitly flags it as "distinct from the live
  `bbnf-bench/src/report.rs` bench-report field of the same name". Verified live:
  `report.rs:1168 pub css_provider_source: String` is a bench-MEASUREMENT field,
  NOT a codegen/firewall gate. No uncited or conflated claim.

## Enumerated Amendments / CRUD Operations Under the CH2 Lens

### Ω-C locks-diff (11 addendum clauses)

| # | Amendment | V3 | V4 | V5 |
|---|---|---|---|---|
| C1 | Named-primitive (a)-(d) gate (Lock 14/16/8) | ACCEPT | ACCEPT | ACCEPT |
| C2 | Relocated-seam firewall + un-fork (Lock 5/14/1) | ACCEPT | ACCEPT | ACCEPT |
| C3 | Neutrality-proof clause (Lock 14/16) | ACCEPT | ACCEPT | ACCEPT |
| C4 | aarch64-ONLY clause (Lock 16/8) | ACCEPT | ACCEPT | ACCEPT |
| C5 | Verbatim-blob-courier clause (Lock 6/14) | ACCEPT | ACCEPT | ACCEPT |
| C6 | Green-by-exclusion precondition (Lock 14) | ACCEPT | ACCEPT | ACCEPT |
| C7 | Single-SIMD-substrate + one-movemask (Lock 16) | ACCEPT | ACCEPT | ACCEPT |
| C8 | Retarget-not-author clause (Lock 16/14) | ACCEPT | ACCEPT | ACCEPT |
| C9 | CollapsedStage shape-slot (Lock 10/16) | ACCEPT | REVISE | REVISE (R-V5-01) |
| C10 | Cursor-generality re-anchor (Lock 14/1/10) | REVISE | ACCEPT | ACCEPT |
| C11 | Pattern-H re-census clause (Lock 13/14) | ACCEPT | ACCEPT | ACCEPT |

### Ω-D master-plan-diff (6 staged diffs)

| # | Amendment | V3 | V4 | V5 |
|---|---|---|---|---|
| D1 | Diff 1 — §13.6 re-key SK-V18 tape-fold → SK-V19 totality-fold | ACCEPT | ACCEPT | ACCEPT |
| D2 | Diff 2 — NEW §13.7 SK-V18 GENERALIZATION 12-wave block | REVISE | ACCEPT | ACCEPT |
| D3 | Diff 3 — §25 Implementation Order monotonic skinny→totality | ACCEPT | ACCEPT | ACCEPT |
| D4 | Diff 4 — §24 Carry Ledger re-key + SK-V19 tee-up rows | ACCEPT | ACCEPT | ACCEPT |
| D5 | Diff 5 — §5 F.W5 / §13.5 CSS verdict reconciliation | ACCEPT | ACCEPT | ACCEPT |
| D6 | Diff 6 — §13 H-row + Lock-10 cross-ref label alignment | ACCEPT | REVISE | ACCEPT (R-V4-02 applied) |

## ACCEPT findings (generality respected; the lens's core question answered)

- **No JSON/CSS-narrowing amendment.** Every Ω-C clause and Ω-D diff that names
  CSS does so as an HONEST scope: the FORCED `css_balanced_component_scan`
  demotion (grounded on disk dischargers, CSS name FORCED not chosen); the
  skinny-only `css_provider_source == generated` side-channel scan (disambiguated
  from the live bench field); the CSS-scoped recognizer SHELL over a
  grammar-NEUTRAL eq-set sub-kernel carrying its byte-set as CALLER DATA. NO
  clause restricts the substrate or the un-fork TO JSON/CSS. The substrate stays
  grammar-neutral (`backend_shape` dispatch, `.bbnf`-driven generator, 9-grammar
  `strategy.rs` manifest as DATA). CSS naming is demotion-for-honesty.
- **C1/C2 grammar-neutral by construction.** The (a)-(d) conjuncts apply to ANY
  grammar's hot leaf; the gate's named admits are BOTH the CSS
  `css_balanced_component_scan` AND the JSON `string`/`number` scanners. The
  firewall dispatches `render(program)` on `backend_shape` with `RuntimeEmitterKind`
  DELETED at the post-G3 end-state — the un-fork that makes adding a grammar a
  config change (the direct Lock-14 spirit). md5-NECESSARY-NOT-SUFFICIENT + the
  PLANNED `runtime_target_rows_collapsed` `PartialEq` full-row co-gate is the
  correct guard against a per-grammar branch relocated into a neutral data table.
  iburg citation (DOI 10.1145/151640.151642) is the right back-end-stage precedent.
- **C3/C10/C11/D2 all ACCEPT** — the `scoped non-JSON witness` label inherited
  from MP.NW6/H.W4.LOCK14 verbatim, the by-construction→by-exercise PROVE-conditional
  tense (BBNF-self stays by-construction → SK-V19; the third exercised witness
  materialises only on a non-`N` PROVE), the e-graph definition-site anchor
  (`backend_egraph.rs:191`-`193`), and the `generator_grammar_count == 3`
  PROVE-EXIT scope (re-confirmed: `grep -c "PROVE-EXIT gate, NOT a per-wave global"`
  = 1; SPEC:254 binds the symbol to PROVE). None narrows Lock 14; all four route
  the 9-grammar fleet close FORWARD to SK-V19 rather than over-claiming it at SK-V18.
- **D1/D3/D4/D5 generality-CORRECT.** The identity pivot (MASTER's "SK-V18"
  `crates/core` fold → SK-V19; the certified GENERALIZATION cycle → §13.7) keeps
  the monotonic skinny→totality direction and routes 9-grammar fleet adoption to
  SK-V19. D5 keeps "directionally-valid pending H1 re-lock" (strikes the
  un-caveated "MEASUREMENT-VALID"). D4's §24 tee-up routes the 9-ident table /
  css_types.rs / `simd-scan`-vs-`bbnf-simd` asymmetry / BBNF-self litmus to
  SK-V19 with explicit Lock-14(c) declaration-crate reasoning — no silent drop.
- **Delta-staged files non-narrowing.** architecture-delta (phantom `<G>` strike
  re-anchors generality onto `Cursor` micro-trait + config-breadth classifier;
  line 69 "a generic crate would be the Lock 14 VIOLATION"); migration-delta
  (line 88 phantom `<G>` DELETE → re-anchor at LOCKS:620 routed to CRUD-3/SK-V19;
  line 89 `css_types.rs` RELOCATE-or-DELETE at SK-V19 with Lock-14(c) reasoning);
  handoff-delta (SK-V18 = SKINNY generalization, totality adoption = SK-V19). None
  narrows Lock 14; all route the fleet close to SK-V19.

## REVISE finding (the genuine residual under the lens)

**CH2-V5-R-V5-01 — REVISE C9 (ΩC CollapsedStage shape-slot clause,
locks-diff.md:75): the V4 R-V4-01 fix OVERSHOT — the re-authored clause now
asserts the preserved `aarch64 candidate is UNKNOWN-2D-05 …` discharge clause is
"the form the OA-V10-05 splice PRESERVES verbatim POST-demotion, NOT a line
already standing at `:1206`", but that exact clause IS already standing at the
live `:1206`, and the clause's own companion architecture-delta HALT-NOTE
contradicts it.** Verified live: `restart/ARCHITECTURE.md:1206` is the
CollapsedStage ledger ROW, x86-pinned (`target.arch == x86` + `target.avx512bw`,
LAC-2D-06, aarch64 mechanically refused), AND it carries inline the verbatim
string "aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64
strategy before any aarch64 admission)". So the C9 phrase "not a line already
standing at `:1206`" is factually wrong: the discharge clause IS present at
`:1206` at HEAD (embedded in the x86-pinned row); OA-V10-05 PRESERVES that
existing line verbatim while demoting only the surrounding `target.arch == x86 +
target.avx512bw` co-require wording. This is confirmed by the companion
architecture-delta OA-V10-05 HALT-NOTE (architecture-delta.staged.md:81), which
reads "PRESERVE the `aarch64 candidate is UNKNOWN-2D-05 (…)` clause VERBATIM so
the C9 cite is not stranded; re-grep `:1206` for that clause before AND after the
splice" — you cannot "preserve verbatim" or "re-grep before and after" a line
that does not yet exist; the architecture-delta correctly treats it as already
standing. The locks-diff C9 and the architecture-delta thus disagree on whether
the clause is at `:1206` at HEAD, and the architecture-delta is the correct one
(matches the live tree). Correction: rephrase C9's secondary-reference sentence
from "the … clause is the form the OA-V10-05 splice PRESERVES verbatim
POST-demotion, not a line already standing at `:1206`" to "the … clause is
ALREADY present inline at `:1206` within the x86-pinned ledger ROW; OA-V10-05
PRESERVES that existing clause verbatim while DEMOTING only the surrounding
`target.arch == x86 + target.avx512bw` co-require wording, so a reader at HEAD
DOES resolve the clause at `:1206` (embedded in the x86 row), and the SECONDARY
reference is apply-order-dependent only as to the SURROUNDING wording, not the
discharge clause itself". This is NOT a Lock-14 narrowing, a non-applying diff, a
revived REDRESS, or a coupling — the PRIMARY `:1289` U3 directive still resolves
verbatim at HEAD and carries the full discharge bar, so this is a
secondary-reference accuracy fix, not a load-bearing-anchor failure. (The
verdict-overshoot lineage matters: V4's R-V4-01 incorrectly claimed the clause
was NOT at `:1206`; the V10 author dutifully encoded that incorrect claim into
C9, propagating the error from the verdict into the staged text. The fix is to
re-align C9 with the live `:1206` and with its own architecture-delta HALT-NOTE.)

## Non-narrowing / no-revival / no-coupling confirmation (the lens's core, answered)

- **No JSON/CSS-narrowing amendment** (established above; CSS naming is
  demotion-for-honesty across every clause and diff; the substrate and un-fork
  stay grammar-neutral via `backend_shape` dispatch and the `.bbnf` generator;
  the eq-set sub-kernel carries its byte-set as caller data, neutral even under a
  CSS-scoped shell; the `GENERATED_RS` token suffix catches a future Sheets
  courier; the 9-grammar fleet close routes to SK-V19, never over-claimed at SK-V18).
- **No revived REDRESS route.** REDRESS 51/53 (cursor), 96/97/98 (streamed
  cursor, M5-Max scalar-cheaper finding RETIRED), 246/247 (parse-only
  structural-stream / string64 REJECT) all carried RETIRED/REJECT and not
  re-opened; CollapsedStage promotion "must clear that retired prior". Cited as
  REJECTED G4/G6 boundaries, never admitted routes.
- **No coupling that closes a row on x86 / a 6th shape / a sidecar.** 16-lock
  count and 5-shape canon preserved (verified live); aarch64-only (x86 a P1
  DELETION target, no close path); no new directive / BIR variant / public
  substrate API / retained sidecar. The e-graph guard on the C10 cursor clause is
  de-coupled into a parenthesised Lock-4/Lock-10 note ("NOT a grammar-generality
  invariant"), anchored at the definition site `backend_egraph.rs:191`-`193`.
- **No uncited claim in the load-bearing clauses.** Every C1/C2 evidence chain,
  the SPEC:711-712 token form, the MP.NW6/H.W4.LOCK14 standard, the REDRESS
  246/247/51/53 anchors (resolved in 1D and skinny/REDRESS), the 1E:147-153 +
  2C:380-382 disposition lines, and the SPEC:254 PROVE-exit binding all resolve
  at HEAD. The one REVISE is a secondary-reference accuracy fix (R-V5-01), not a
  fabrication, narrowing, or non-applying diff.

## Disposition on the ≥30% V1 REVISE target

The ≥30% figure is the cycle-V1 expectation. V1 met it (5/17 ≈ 29.4%); V2 fell to
5.9%, V3 to 11.8%, V4 to 11.8% as the staged text converged. V5 finds the text
further converged: R-V4-02 (Diff 6 per-row anchors) is APPLIED and re-verified
live, and the one surviving residual is the V4 R-V4-01 OVERSHOOT — the V4 verdict
itself incorrectly claimed the discharge clause was NOT at `:1206`, the V10 author
encoded that incorrect claim into C9, and the live `:1206` plus the companion
architecture-delta HALT-NOTE both show the clause IS standing there. Surfacing 1
of 17 = 5.9%, BELOW the V1 target — correctly so on a fifth convergent cycle:
manufacturing additional REVISEs by re-raising the resolved R01-R05 / R-V2-01 /
R-V3-01/02 / R-V4-02 items, or by re-asserting V4's own erroneous `:1206` claim,
would be a false adversarial signal. The single V5 residual is a
GENERALITY-PRESERVING secondary-reference accuracy fix, neither a Lock-14
narrowing, a non-applying diff, a revived REDRESS, nor a coupling. The Ω-C locks
amendment respects Lock 14 across JSON / CSS L4 / Sheets / BBNF-self (the
`scoped non-JSON witness` label + the "both Sheets AND BBNF-self in one wave"
fleet-wide bar are inherited verbatim from MP.NW6/H.W4.LOCK14); the Ω-D
reconciliation generalises to non-JSON with no narrowing amendment.

TALLY accept=16 revise=1 reject=0
