# CH3 REGRESSION — Pass Omega V10 (astral) CHALLENGE Lens, Cycle V4

Lens: CH3 REGRESSION — does any staged amendment reintroduce a skinny/REDRESS
route or a T-P2-refuted assertion; cross-check Ω-C + Ω-D + Ω-E against the
REDRESS ledger.

Scope reviewed: the 6 Ω artefacts + staged diffs under
`restart/audit/totality/astral/V10/` (ΩA-coherence-audit, ΩB-skinny-lessons,
ΩC-locks-amendments + `locks-diff.md`, ΩD-master-plan-reconciliation +
`master-plan-diff.md`, ΩE-skinny-corpus + `ΩE-skinny-corpus-staged-diff.md`,
ΩF-migration-handoff + `migration-delta.staged.md` + `handoff-delta.staged.md` +
`architecture-delta.staged.md`) against the live V1 surfaces
(`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`, `locks/LOCKS.md`, `MIGRATION.md`,
`HANDOFF.md`), the committed REDRESS ledger (`skinny/REDRESS.md`, 6465 lines),
and the converged T-P1/T-P2/T-P3 evidence.

Date: 2026-06-01. HEAD `25297a7fc`-era staging; the whole
`restart/audit/totality/astral/V10/` tree is UNTRACKED (staged-only). Cycle V4
re-reviews V1+V2+V3 independently (re-runs every load-bearing spot-check at
HEAD, confirms which prior REVISE the staged text now satisfies) and hunts the
residue the prior cycles did not reach. V3 widened scope to the
ARCHITECTURE-delta leg and found ONE cross-leg cite/edit collision at `:1206`;
V4 confirms that fix is APPLIED and then asks the natural follow-on: was the V3
fix applied POINT-WISE (only `:1206`) or to the whole defect-CLASS (every
LOCKS-clause cite that lands on an OA-V1x splice target)?

## Load-Bearing Spot-Verifications (re-run at HEAD, V4)

| Check | Command / target | Result |
|---|---|---|
| Staged locks-diff applies | `awk … \| git apply --check -` on `locks-diff.md` | **EXIT=0** (applies cleanly) |
| architecture-delta 2 gated hunks apply | `awk … \| git apply --check -` (2 `@@`, 2 `--- a/`) | **EXIT=0** |
| master-plan-diff is NOT a git-apply target | extract fenced diffs → 0 `@@`, 0 `--- a/`; "No valid patches" | confirmed illustrative anchored snippets; master-plan-diff/ΩD make **no `git apply` claim** (correct, not a false claim) |
| 16 numbered locks / no Lock 17 | `grep -cE '^[0-9]+\. \*\*'` / `^17\. ` | **16** / **0** — addendum adds no Lock 17 |
| 5 BackendShape variants | `lower/mod.rs:20-24`; `cost.rs:334 all_backend_shapes()->[BackendShape;5]` | 5 `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no 6th |
| PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `… bbnf_simd_single_mask_convention` | both **0** in all skinny crates/xtask (written PLANNED, not live) |
| Un-fork owes no REDRESS id | `grep -ic RuntimeEmitterKind\|relocated.seam\|un.fork\|emit_shape_source skinny/REDRESS.md` | **0** — un-fork is SK-V18-NOVEL, NOT a refuted route |
| REDRESS 96/97/98 labelling faithful | `skinny/REDRESS.md:2795`(96 class-column),`:2852`(97 streaming-cursor),`:2908`(98 `G-W3-UNION-SUBSTRATE` retirement) | accurate: 96/97 = two faithful regressed union-substrate impls; 98 = gate retirement carrying the M5-Max scalar-cheaper-than-SIMD-cursor finding (`:2928`-`:2933`); C9's "REDRESS 96/97/98 RETIRED" framing exact |
| Four-item set 51/53/246/247 present | migration-delta `:116`; ΩF `:162`; master-plan-diff `:207`,`:343`; ΩD `:84` | all FOUR present; **zero** surviving three-item "51/53/247" fence in any V10 production surface |
| REDRESS items 51/53/246/247 EXIST in ledger | `grep '^- Item (246\|247)'` | `:6186` item 246 closes `G-SK-V14-W11T-JSON-PARSE-ONLY-STRUCTURAL-STREAM` REJECT; `:6232` item 247 closes `G-SK-V14-W11V-JSON-PARSE-ONLY-STRING64` REJECT — item-246 = W11T parse-only structural-STREAM driver, the attribution is correct |
| REDRESS ledger end-of-coverage | `grep '^- Item' \| tail` → item 253 `:6448` closes SK-V15-W11 | ledger ENDS at SK-V15 W11 (`:6446`); SK-V16/V17 rejected routes NOT captured |
| SK-V16/V17 completeness caveat CARRIED | ΩB `:89`,`:203`-`:207` (U-5) | the gap is SURFACED as a pre-W-PRUNE blocker (not silently treated complete); 1D `:173` carries the same "COMPLETENESS CAVEAT (per CH3-V4-006)" — no over-claim |
| §H wave MP.NW6 / H.W4.LOCK14 resolution | `MASTER-PLAN.md:662`,`:605` | `scoped non-JSON witness` single-negative-control standard FAITHFUL; "FactStream is a substrate-target classification, NOT a 6th BackendShape variant" — 5-shape canon preserved; C3 neutrality-proof cite is exact |
| CH3-V1-R2 retime carried (G2/G4/G6 blocked) | ΩD `:81`-`:85`,`:100`; migration-delta `:113`-`:120`; ΩF `:158`-`:164` | G2/G4/G6 entry BLOCKED until SK-V16/V17 REDRESS reconcile is on the committed ledger as Pass-Omega-V10 / pre-W-PRUNE blocker; rejected shapes (second scanner / structural-stream driver / parser-local cursor) fenced |
| V3 REVISE (CH3-V3-01 `:1206`) APPLIED | `locks-diff.md:75` C9 clause; `architecture-delta.staged.md:81` | **APPLIED** — C9 PRIMARY anchor is now `:1289` (demote-stable U3 directive); `:1206` retained as SECONDARY with cross-leg note; OA-V10-05 row carries an explicit HALT-NOTE naming the C9 cite + PRESERVE-verbatim instruction |
| `:1289` is demote-STABLE | `architecture-delta.staged.md:81` splice set = `:1151`/`:1171`/`:1186`/`:1206`; `:1289` ABSENT | confirmed — `:1289` not in any splice/hunk set; `ARCHITECTURE.md:1289` carries "CollapsedStage = UNKNOWN-2D-05, no admission without a 2E source-backed…" at HEAD |
| BENCH inversion dual caveat (win AND loss) | ΩE-staged-diff `:274`,`:310`,`:335` | symmetric; un-caveated MEASUREMENT-VALID FORBIDDEN before H1 (CH2-V1-R03) honored both directions |

## What V1+V2+V3's REVISEs Became (re-verified ACCEPT at HEAD)

Cycle V4's first duty is to confirm the prior REVISEs are absorbed. They are:

- **V1 CH3-V1-01..03 (framing, item-246, BENCH caveat) → APPLIED.** §7 invariant
  carries the four-item set; item 246 = W11T parse-only structural-STREAM driver
  (verified at `REDRESS.md:6186`); BENCH win AND loss carry identical
  directional/H1-pending caveat.
- **V2 CH3-V2-01 (`:1282` mis-cite in C9) → APPLIED.** `:1282` is GONE from every
  production surface; C9 now cites `:1206`/`:1289`.
- **V3 CH3-V3-01 (the `:1206` cross-leg cite/edit collision) → APPLIED, and well.**
  Both V3 corrections (a) AND (b) landed: (a) the OA-V10-05 splice row
  (`architecture-delta.staged.md:81`) now carries a HALT-NOTE that names the C9
  cite, instructs "Demote the `target.arch == x86 + target.avx512bw` co-require
  WORDING but PRESERVE the `aarch64 candidate is UNKNOWN-2D-05 …` clause VERBATIM
  so the C9 cite is not stranded", and orders a re-grep of `:1206` before/after
  the splice; (b) C9's PRIMARY discharge anchor is re-pointed to the
  demote-STABLE `:1289` U3 directive (`:1289` is NOT in the splice set), with
  `:1206` demoted to SECONDARY. This is a model fix and flips C9 to ACCEPT at the
  V4 census.

## The CROSS-LEG RESIDUE V4 Hunts (the natural follow-on to V3)

V3's fix was applied POINT-WISE to `:1206`. V4 enumerated EVERY ARCHITECTURE.md
line-cite in the staged production surfaces (LOCKS/MP/ΩD/ΩF/migration/handoff,
excluding the architecture-delta itself):

```
restart/ARCHITECTURE.md:1289   (C9 PRIMARY — demote-stable, GOOD)
restart/ARCHITECTURE.md:1990   (C10 cursor-generality "companion §9.2 prose carrier")
```

and cross-checked each against the OA-V10-05/06 splice/hunk sets
(`:1151`/`:1171`/`:1186`/`:1206` for OA-V10-05; `:1990` annotate + `:1998` strike
for OA-V10-06). The `:1289` cite is demote-stable (resolved by the V3 fix). The
`:1990`/`:1997` cite is NOT — and that is the V4 finding.

## Enumerated Staged Amendments / CRUD Operations Under CH3 (V4 disposition)

### Ω-C — `locks-diff.md` (11 addendum clauses)

| # | Clause | CH3-V4 disposition |
|---|---|---|
| C1 | Named-primitive (a)-(d) gate | **ACCEPT** — SPEC-only → lock; no refuted route; STRENGTHENS Lock 14 |
| C2 | Relocated-seam firewall + un-fork | **ACCEPT** — un-fork REDRESS negative-witness 0; `css_provider_source` PLANNED-labelled; md5-distinctness NECESSARY-NOT-SUFFICIENT carried |
| C3 | Neutrality-proof (forced demotion) | **ACCEPT** — `scoped non-JSON witness` faithful to live MP.NW6:662; no fabricated cross-grammar caller |
| C4 | aarch64-ONLY (x86 PRUNE target) | **ACCEPT** — FEAT_SVE2 ABSENT (2E:244); svmatch REFUTED-on-host preserved, not revived |
| C5 | Verbatim-blob-courier prohibition | **ACCEPT** — Lock-6 co-bind; no refuted route |
| C6 | Green-by-exclusion precondition (P4-before-G2/G3) | **ACCEPT** — leak-surface promotion before gate |
| C7 | Single-SIMD-substrate + one-movemask | **ACCEPT** — skinny-scoped; `simd-scan:67` carry routed to SK-V19, NOT claimed as totality proof |
| C8 | Retarget-not-author | **ACCEPT** — `find_css_significant` wire-as-is REFUTATION honored (retargets, does not author) |
| C9 | CollapsedStage shape-slot | **ACCEPT** — V3's `:1206`/`:1289` fix APPLIED; REDRESS 96/97/98 + UNKNOWN-2D-05 discharge sound; PRIMARY anchor now demote-stable `:1289`; HALT-NOTE binds the SECONDARY `:1206` cite. Route NOT revived. |
| C10 | Cursor-generality re-anchor (`<G>` strike) | **REVISE** (CH3-V4-01) — `NormalizeDirectSinkCost` regression-guard live; phantom-axis delete sound; BUT the clause cites `restart/ARCHITECTURE.md:1990`,`:1997` as the "companion §9.2 prose carrier" of a SK-V19/CRUD-3 reconcile, and `:1997`/`:1990` are CONCURRENTLY the OA-V10-06 CRUD-1 strike/annotate targets, with NO symmetric HALT-NOTE — the V3 defect-class surviving at the sibling site |
| C11 | Pattern-H re-census (67→71) | **ACCEPT** — +4 = tape-fold roster trace; 71 at HEAD |

### Ω-D — `master-plan-diff.md` (6 staged diffs)

| # | Diff | CH3-V4 disposition |
|---|---|---|
| D1 | §13.6 SK-V18 tape-fold → SK-V19 totality-fold re-key | **ACCEPT** — identity pivot; F1-F9 verbatim; refuted-census 0 (ΩD:53-54); no wave refuted |
| D2 | NEW §13.7 12-wave block + CH3-V1-R2 retime | **ACCEPT** — four-item "51/53/246/247" with "item 246 bounds G4"; G2/G4/G6 blocked until reconcile committed |
| D3 | §25 Implementation Order reconciliation | **ACCEPT** — monotonic SK-V15→V17→V18→V19; no route revived |
| D4 | §24 Carry Ledger re-key + SK-V19 tee-up rows | **ACCEPT** — 3 totality leaks DEFERRED to SK-V19, not bolted into SK-V18 |
| D5 | §5 F.W5 / §13.5 CSS verdict reconciliation | **ACCEPT** — CSS UPGRADE carries directional/H1-pending caveat; refuses un-caveated MEASUREMENT-VALID (CH2-V1-R03) |
| D6 | §13 H-row + Lock-10 cross-ref alignment | **ACCEPT** — label-only; 5-shape canon row UNCHANGED |

### Ω-E — `ΩE-skinny-corpus` (6 CRUD-5 surface updates)

| # | Surface | CH3-V4 disposition |
|---|---|---|
| E1 | INDEX (SK-V15 → SK-V18 W-PRUNE→G1..G6→PROVE→H1) | **ACCEPT** — supersession, not route-revival |
| E2 | WORKSPACE (telemetry → SPEC §3 gate schema) | **ACCEPT** — FNV stays quarantine/telemetry |
| E3 | HARDENING (seven-lens re-key + §6 (a)-(d) lens) | **ACCEPT** — adds delete-before-rebuild cycle detection |
| E4 | COMPILER (DELETE `RuntimeEmitterKind`, dispatch on BackendShape) | **ACCEPT** — un-fork NOVEL (REDRESS=0); deletion PLANNED-at-G3 |
| E5 | BENCH (CSS comparator inversion + track1_rich) | **ACCEPT** — win AND loss carry identical directional/loadavg/H1-pending caveat |
| E6 | SUBSTRATE (limited authority/status flip) | **ACCEPT** — "NO substrate data-structure change"; 5-shape canon + Lock 1 union preserved |

### Ω-F — `migration-delta` + `handoff-delta` + `architecture-delta`

| # | Operation | CH3-V4 disposition |
|---|---|---|
| F1 | MIGRATION OP-1 §0.0 SK-V18 receiver (renumber-down) | **ACCEPT** — historical receivers preserved as provenance |
| F2 | MIGRATION OP-2 rename/abrogate/refactor rows | **ACCEPT** — `css_types.rs` routed to SK-V19; phantom `<G>` LOCKS:620 strike explicitly recorded as "Pass Omega CRUD-3 / SK-V19, NOT an Ω-F edit" (`:88`) — consistent with ΩD:95 + ΩF:175 |
| F3 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate (§17/§19) | **ACCEPT** — `:116` four-item "51/53/246/247"; rejected shapes fenced, none revived |
| F4 | HANDOFF OP-1..OP-5 (override + blocker matrix + directive) | **ACCEPT** — blocker matrix maps phantom `<G>` AND CSS-Value-API to G4 with measurable gates; each blocker requires REDRESS/revert/intrinsic-block to override |
| F5 | ARCHITECTURE-delta OA-V10-04..11 (2 gated hunks + 4 anchored splices) | **REVISE** (CH3-V4-01) — the OA-V10-06 §9.2 strike/annotate at `:1990`/`:1998` is the CRUD-1 in-flight execution of the SAME re-anchor the C10 locks clause governs; the architecture-delta intro (`:50`-`:55`) flags the relationship to `locks-diff.md` but the OA-V10-06 row carries NO HALT-NOTE naming the C10 `:1990`/`:1997` cite — asymmetric with the applied OA-V10-05 `:1206` fix |

## CH3-V4 Finding (the REVISE)

### CH3-V4-01 — REVISE: the V3 `:1206` cross-leg fix was applied POINT-WISE; its sibling — the C10 cursor-generality clause's `ARCHITECTURE.md:1990`/`:1997` "companion §9.2 prose carrier" cite, concurrently struck/annotated by OA-V10-06 — carries NO symmetric HALT-NOTE

Artefacts: `restart/audit/totality/astral/V10/locks-diff.md:77` (the C10
cursor-generality re-anchor clause, LOCKS/CRUD leg) AND
`restart/audit/totality/astral/V10/architecture-delta.staged.md:82` (OA-V10-06,
the §9.2 phantom-vehicle strike + lazy-`ValueRef` re-open, ARCHITECTURE/CRUD-1
leg).

The C10 clause closes: *"This is a SK-V19 LOCKS reconcile (a one-clause strike +
re-anchor at `LOCKS.md:620`, **with the companion `restart/ARCHITECTURE.md:1990`,
`:1997` §9.2 prose carrier**)."* So C10 names `:1990` + `:1997` as the
ARCHITECTURE-side carrier of a reconcile it ROUTES TO SK-V19 / Pass Omega
CRUD-3 (confirmed routed-to-SK-V19 by migration-delta OP-2 `:88`, ΩD `:95`, and
ΩF `:175`).

But the sibling OA-V10-06 (`architecture-delta.staged.md:82`) executes the
ARCHITECTURE-side strike NOW, at CRUD-1 / in-flight at SK-V18: its splice
"annotate[s] the lazy-`ValueRef` value-plane as in-flight-at-SK-V18" at `:1990`
and (per the architecture-delta intro `:50`-`:55` and ΩA `:152`,`:232`,`:271`)
STRIKES the phantom "The `G:EventGrammar` type parameter is the generality
vehicle" sentence at `:1998`. At HEAD that sentence spans `ARCHITECTURE.md:1997`
("type parameter is the generality") → `:1998` ("vehicle; …"). So C10's `:1997`
cite points at the EXACT sentence OA-V10-06 strikes, and C10's `:1990` cite
points at the EXACT header line OA-V10-06 re-annotates.

This is the V3 defect-class (a LOCKS-clause cite landing on a line a sibling
ARCHITECTURE splice concurrently re-writes) recurring at a SECOND site. It is
NARROWER than CH3-V3-01 in two ways that keep it a precision REVISE, not a
REJECT:

1. The collision is FLAGGED at the architecture-delta intro (`:50`-`:55`):
   OA-V10-06 says the §9.2 strike happens "per the cursor-generality re-anchor
   clause in `locks-diff.md`", so the cross-leg relationship is cross-referenced
   — unlike the original `:1206` case, which had NO binding sentence.
2. C10's `:1997`/`:1990` cite is a reconcile-TARGET cite (the carrier the
   SK-V19 reconcile will re-anchor AWAY), not a falsifiability verify-cite into
   governance text whose alteration breaks a discharge. Finding the carrier
   already-struck at SK-V19 is the INTENDED post-state, not a stranded
   discharge.

But it is STILL a genuine precision defect, and it is exactly the asymmetry the
V3 fix should have closed class-wide:

- The applied OA-V10-05 row (`:81`) carries a HALT-NOTE that NAMES the C9
  `:1206` cite, instructs PRESERVE-verbatim, and orders a re-grep before/after.
  The OA-V10-06 row (`:82`) carries NO such note for the C10 `:1990`/`:1997`
  cite — no instruction on what the SK-V19 reconcile should expect to find, no
  re-grep-before/after, no statement that the `:1997` carrier is struck at
  CRUD-1 so the SK-V19 reconcile must NOT re-grep `:1997` expecting the live
  sentence.
- The two legs are SEQUENCED OPPOSITELY here vs the `:1206` case: the
  ARCHITECTURE strike is EARLIER (CRUD-1) than the LOCKS reconcile (SK-V19 /
  CRUD-3). So an operator running the SK-V19 LOCKS reconcile and following C10's
  `:1997` "companion prose carrier" cite to find the sentence to strike will
  find it ALREADY struck by OA-V10-06 — benign, but unstated and surprising
  unless the cite carries the same "this carrier is struck at CRUD-1" annotation
  the `:1206` HALT-NOTE models.

Correction (any ONE of):

- (a) Add a HALT-NOTE to the OA-V10-06 row (`architecture-delta.staged.md:82`)
  symmetric with the OA-V10-05 `:1206` note: *"`:1990`/`:1997`/`:1998` are the
  C10 locks-diff cursor-generality clause's `companion §9.2 prose carrier` cite;
  this CRUD-1 strike PRE-EMPTS the SK-V19/CRUD-3 LOCKS:620 reconcile — re-grep
  `:1997` for the `generality vehicle` sentence before the strike; at the SK-V19
  reconcile the carrier is ALREADY struck, so the reconcile re-anchors LOCKS:620
  WITHOUT re-grepping `:1997` for the live sentence."*; OR
- (b) Amend the C10 clause (`locks-diff.md:77`) to annotate its `:1990`/`:1997`
  cite "(STRUCK at CRUD-1 by `architecture-delta` OA-V10-06; cited here as the
  PRE-strike carrier — the SK-V19 LOCKS:620 reconcile inherits the
  already-struck §9.2 post-state, not a live carrier)", mirroring the
  demote-stable framing C9 received for `:1206`.

This is a citation/coupling-precision REVISE, the V4 analogue of CH3-V3-01: V3
fixed the FIRST cross-leg cite/edit collision (`:1206`/C9); V4 finds the V3 fix
was point-wise and the SIBLING collision (`:1990`/`:1997`/C10) was left without
the symmetric HALT-NOTE. The discharge gates, the 5-shape canon, and the
phantom-axis delete are all correct; the residue is the unbound second cite.

## What Did NOT Regress (ACCEPT rationale, the load-bearing nulls)

- **No revived REDRESS route.** The un-fork (`RuntimeEmitterKind` deletion /
  relocated-seam / `emit_shape_source`) owes NO REDRESS id (`grep -ic … skinny/
  REDRESS.md == 0`) → SK-V18-NOVEL. REDRESS 96/97/98 (union-substrate cursor)
  and 51/53/246/247 (second-scanner / structural-stream / parser-local cursor)
  are carried as BLOCKERS, never revived; the four-item set is correct in EVERY
  fence (zero surviving three-item under-citation); items 246/247 verified to
  EXIST at `REDRESS.md:6186`/`:6232` with item 246 = the W11T parse-only
  structural-STREAM driver, matching the attribution.
- **REDRESS-ledger completeness is HONESTLY caveated.** The committed ledger ends
  at SK-V15 W11 (`:6446`); SK-V16/V17 rejected routes are NOT yet captured — and
  the staged text SURFACES this (ΩB `:89`,`:203`-`:207` U-5; 1D `:173`) and
  routes the SK-V16/V17 REDRESS reconcile as a Pass-Omega-V10 / pre-W-PRUNE
  BLOCKER on G2/G4/G6 entry (ΩD:81-85,:100; migration-delta:113-120; ΩF:158-164).
  No staged amendment over-claims a refuted-route fence as "complete against the
  REDRESS ledger" without the SK-V16/V17 caveat. This is the correct disposition
  of an incomplete ledger — a strength, not a defect.
- **No Lock-14 narrowing.** C6 green-by-exclusion + C1 named-primitive (a)-(d)
  STRENGTHEN Lock 14; the `<G>` strike (C10) re-anchors generality onto
  `Cursor`+config-breadth, preserving `@generated` grammar-neutrality; MP.NW6:662
  confirms "FactStream is a substrate-target classification, NOT a 6th
  BackendShape variant" — the 5-shape canon is intact across every leg.
- **No T-P2-refuted assertion revived.** "Wire-as-is" REFUTED →
  retarget-not-author (C8, `find_css_significant` kernel framing); NEON-svmatch on
  SVE2-absent hosts REFUTED → aarch64-ONLY FEAT_SVE2 ABSENT (C4);
  md5-distinctness NECESSARY-NOT-SUFFICIENT → structural row-collapse co-gate
  carried (C2/D2/migration OP-2:87).
- **No coupling INTO the runtime.** The CH3-V4-01 finding is a STAGED-DOC
  cross-leg cite/edit precision gap, not a runtime/architecture coupling; the
  CollapsedStage conditional rebuild stays G5/G6-GATED.
- **5-shape canon + 16-lock count preserved** byte-verbatim across every staged
  surface; `git apply --check` EXIT=0 for `locks-diff.md` and both
  `architecture-delta.staged.md` gated hunks; master-plan-diff makes no
  git-apply claim (its blocks are illustrative anchored snippets); no Lock 17.

## Disposition Summary

29 enumerated amendments/CRUD operations (11 Ω-C + 6 Ω-D + 6 Ω-E + 5 Ω-F at
clause/diff/surface granularity, the Ω-F census scoring the
`architecture-delta.staged.md` leg explicitly as F5):
**28 ACCEPT / 1 REVISE / 0 REJECT**.

No REJECT: no non-applying diff (`git apply --check` EXIT=0 for locks-diff and
both architecture-delta gated hunks; master-plan-diff correctly claims no
apply), no revived REDRESS route (un-fork REDRESS-id 0; 96/97/98 and 51/53/246/247
carried as blockers; ledger-completeness honestly caveated), no Lock-14
narrowing, no reintroduced runtime coupling, no uncited claim — items 246/247
verified present in the ledger, the four-item table correct in every fence, the
symmetric CSS caveat, and the faithful 96/97/98 + UNKNOWN-2D-05 labelling keep
the lone finding at coupling/citation-precision level.

V4 confirms ALL prior REVISEs are applied and re-verified ACCEPT at HEAD: V1's
three (framing, item-246, BENCH caveat), V2's one (`:1282` mis-cite), and V3's
one (the `:1206` cross-leg cite/edit collision, fixed by both (a) the OA-V10-05
HALT-NOTE and (b) re-pointing C9 to the demote-stable `:1289`). V4's single
REVISE (CH3-V4-01) is the V3 fix's POINT-WISE limit: the SIBLING cross-leg cite
(`:1990`/`:1997`, C10's "companion §9.2 prose carrier") was left without the
symmetric HALT-NOTE that OA-V10-05 `:1206` received — found only by enumerating
EVERY ARCHITECTURE.md cite in the LOCKS leg against the OA-V1x splice sets and
noticing the V3 fix was applied to one of the two colliding cites, not the
class.

On the CH3-eligible amendment surface (the REDRESS-fence + refuted-assertion +
cross-leg-cite clauses CH3 touches with weight: C2, C4, C7, C8, C9, C10, D2, D5,
E5, F3, F5 = 11), the V4 REVISE rate is 1/11 = 9%, BELOW the ≥30% cycle bar.
This is the correct and honest count: the ≥30% expectation is a V1 expectation
(MET at V1, 3/9 = 33%); V1's three, V2's one, and V3's one REVISEs are all
applied and re-verified, and the surface is near-converged on the CH3 axis.
Padding a V4 census to ≥30% would require fabricating findings the evidence does
not support — the [no-workarounds] / accurate-narrative discipline declines it.
The single genuine REVISE (CH3-V4-01, the unsymmetric sibling `:1990`/`:1997`
HALT-NOTE) is the full residue; recommended disposition is APPLY CH3-V4-01 (add
the symmetric OA-V10-06 HALT-NOTE naming the C10 cite, OR annotate C10's
`:1990`/`:1997` cite as STRUCK-at-CRUD-1), after which the CH3 cross-leg
cite/edit class is converged-clean class-wide for G-Omega.

TALLY accept=28 revise=1 reject=0
