# CH3 REGRESSION — Pass Omega V10 (astral) CHALLENGE Lens, Cycle V5

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
`HANDOFF.md`), the committed REDRESS ledger (`skinny/REDRESS.md`), and the
converged T-P1/T-P2/T-P3 evidence.

Date: 2026-06-01. The whole `restart/audit/totality/astral/V10/` tree is
UNTRACKED (staged-only). Cycle V5 re-reviews V1+V2+V3+V4 independently (re-runs
every load-bearing spot-check at HEAD, confirms which prior REVISE the staged
text now satisfies) and hunts the residue the prior cycles did not reach.

The critical V5 fact: the staging MOVED after the V4 verdict. The V4 CH3 verdict
was written at 22:32; `locks-diff.md` (22:42), `architecture-delta.staged.md`
(22:45), `ΩA-coherence-audit.md` (22:45), `master-plan-diff.md` (22:40),
`ΩD-master-plan-reconciliation.md` (22:43), `migration/handoff-delta` (22:40),
and `ΩF-migration-handoff.md` (22:22) were all re-staged AFTER V4. So V5's first
duty is to confirm the V4 REVISE (CH3-V4-01) was absorbed by the re-staging, and
its second duty is to hunt for any NEW cross-leg cite/edit collision the
re-staging may have introduced.

## Load-Bearing Spot-Verifications (re-run at HEAD, V5)

| Check | Command / target | Result |
|---|---|---|
| Staged locks-diff applies | `awk … \| git apply --check -` on `locks-diff.md` | **EXIT=0** (applies cleanly) |
| architecture-delta 2 gated hunks apply | `awk … \| git apply --check -` (2 `@@`, 2 `--- a/`) | **EXIT=0** |
| master-plan-diff is NOT a git-apply target | makes no `git apply` claim; blocks are illustrative anchored snippets | confirmed (correct, not a false claim) |
| 16 numbered locks / no Lock 17 | `grep -cE '^[0-9]+\. \*\*'` / `^17\. ` | **16** / **0** — addendum adds no Lock 17 |
| 5 BackendShape variants | `cost.rs:334 all_backend_shapes()->[BackendShape;5]` | `[BackendShape; 5]` confirmed; `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no 6th |
| PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `… bbnf_simd_single_mask_convention` over `skinny/` | both **total 0** (written PLANNED, not live) |
| Un-fork owes no REDRESS id | `grep -ic 'RuntimeEmitterKind\|relocated.seam\|emit_shape_source\|un.fork' skinny/REDRESS.md` | **0** — un-fork is SK-V18-NOVEL, NOT a refuted route |
| REDRESS items 51/53/246/247 EXIST | `grep -nE 'Item (51\|53\|246\|247)'` | item 51 `:763`-region, item 53 `:763`, item 246 `:6186` (W11T parse-only structural-STREAM REJECT), item 247 `:6232` (W11V STRING64 REJECT) — all four present |
| Four-item fence in every production surface | master-plan-diff `:207`,`:343`; migration-delta `:116`; ΩF `:162`,`:279` | all FOUR present; **zero** surviving three-item "51/53/247" fence anywhere in the V10 production tree |
| REDRESS ledger end-of-coverage | `grep '^- Item' \| tail` → item 253 `:6448` | ledger ENDS at SK-V15 W11 (`:6446`); SK-V16/V17 rejected routes NOT captured |
| SK-V16/V17 completeness caveat (U-5) CARRIED | ΩB `:89`,`:202`-`:207` | gap SURFACED as a Pass-Omega-V10 / pre-W-PRUNE blocker; NOT silently treated complete |
| REDRESS 96/97/98 labelling faithful | `REDRESS.md:2797`(96 class-column),`:2852`(97 streaming-cursor),`:2910`(98 `G-W3-UNION-SUBSTRATE` retirement); finding `:2928`-`:2933` | accurate: 96/97 = two faithful regressed union-substrate impls; 98 = gate retirement carrying the M5-Max scalar-cheaper-than-SIMD-cursor finding (verbatim at `:2928`-`:2933`); C9's "REDRESS 96/97/98 RETIRED" framing exact |
| §H wave MP.NW6 / H.W4.LOCK14 resolution | `MASTER-PLAN.md:662`,`:605` | `scoped non-JSON witness` single-negative-control standard FAITHFUL; both rows state verbatim "FactStream is a substrate-target classification, NOT a 6th BackendShape variant"; 5-shape canon preserved; C3 neutrality-proof cite exact |
| **CH3-V4-01 (the V4 REVISE) APPLIED — BOTH legs** | `locks-diff.md:77` C10 clause + `architecture-delta.staged.md:82` OA-V10-06 row | **APPLIED, and class-wide.** See below. |
| C8 retarget guard cite resolves | clause cites `lib.rs:574`; `rg find_css_significant lib.rs` | `:574` is the live-path REACH (the `find_css_significant(...)` call inside `#[cfg(test)] neon_significant_skip_matches_scalar`, fn header `:562`); the "reached ONLY from … guard (`:574`)" cite is the call site, precise — NOT a drift |
| C10 e-graph regression-guard live | `backend_egraph.rs:75` instantiation, `:191` struct def, `:193` `impl Rewrite` | `NormalizeDirectSinkCost` live — a guard, not a revival; the "≥1 asserted e-graph rewrite, live" claim cites the symbol DEFINITION at `:191`-`:193`, not only the call site |
| C2 `css_provider_source` PLANNED-labelled | `rg -lc css_provider_source skinny/crates` | live ONLY as `bbnf-bench/src/report.rs` (6 occurrences), zero codegen — the "PLANNED firewall predicate, distinct from the live bench field of the same name" label is accurate |
| C11 Pattern-H census 71 | `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | **71** at HEAD; +4 = `tape/{mod,cursor,arena,record}.rs` tape-fold roster trace |
| BENCH inversion dual caveat | no un-caveated MEASUREMENT-VALID survives in master-plan-diff / ΩE | win AND loss carry identical directional/H1-pending caveat (CH2-V1-R03 honored both directions) |
| PRUNE-before-GENERALIZE fence (rejected shapes) | migration-delta `:103`,`:119`-`:120` | second-scanner / structural-stream driver / parser-local cursor explicitly fenced from re-entering G2/G4/G6 |

## What V1+V2+V3+V4's REVISEs Became (re-verified ACCEPT at HEAD)

Cycle V5's first duty is to confirm every prior REVISE is absorbed. They are:

- **V1 CH3-V1-01..03 (framing, item-246, BENCH caveat) → APPLIED.** §7 invariant
  carries the four-item set; item 246 = W11T parse-only structural-STREAM driver
  (verified at `REDRESS.md:6186`); BENCH win AND loss carry identical
  directional/H1-pending caveat.
- **V2 CH3-V2-01 (`:1282` mis-cite in C9) → APPLIED.** `:1282` is GONE from every
  production surface; C9 cites `:1206` (SECONDARY) + `:1289` (PRIMARY).
- **V3 CH3-V3-01 (the `:1206` cross-leg cite/edit collision) → APPLIED.** The
  OA-V10-05 row (`architecture-delta.staged.md:81`) carries a HALT-NOTE naming
  the C9 `:1206` cite, instructing PRESERVE-verbatim and a re-grep before/after;
  C9's PRIMARY discharge anchor is the demote-stable `:1289` U3 directive
  (`:1289` is NOT in the splice set).
- **V4 CH3-V4-01 (the SIBLING `:1990`/`:1997` cite — C10's "companion §9.2 prose
  carrier" — left without a symmetric HALT-NOTE) → APPLIED, BOTH ways, and
  class-wide.** V4 offered corrections (a) OR (b); the re-staging applied BOTH:
  - (a) The OA-V10-06 row (`architecture-delta.staged.md:82`) now carries the
    symmetric HALT-NOTE: *"`:1990`/`:1997`/`:1998` … are the `locks-diff.md` C10
    cursor-generality clause's `companion §9.2 prose carrier` cite. Gated Hunk 2
    STRIKES the … sentence at CRUD-1 … re-grep `:1997` for the `generality
    vehicle` sentence BEFORE the strike … at the SK-V19/CRUD-3 reconcile the
    carrier is ALREADY struck — the reconcile re-anchors `LOCKS.md:620` … WITHOUT
    re-grepping `:1997` for the live sentence … The C10 cite is the PRE-strike
    carrier, not a live carrier."*
  - (b) The C10 clause (`locks-diff.md:77`) now annotates its own cite:
    *"`restart/ARCHITECTURE.md:1990`,`:1997` §9.2 prose carrier — which is STRUCK
    EARLIER at CRUD-1 by the sibling `architecture-delta.staged.md` OA-V10-06
    Gated Hunk 2 … so this C10 cite is the PRE-strike carrier; the SK-V19/CRUD-3
    reconcile inherits the already-struck §9.2 post-state … mirroring the
    demote-stable framing the C9 clause received for its `:1206` secondary cite;
    see the architecture-delta OA-V10-06 HALT-NOTE."*

  This is the V3 defect-class closed CLASS-WIDE: both colliding ARCHITECTURE
  cites in the LOCKS leg (`:1206`/C9 and `:1990`/`:1997`/C10) now carry symmetric
  bidirectional HALT-NOTEs, the C9 framing models the C10 framing and vice
  versa, and each leg names the other. The asymmetry V4 flagged is gone.

## The Residue V5 Hunts (the natural follow-on to V4)

V4 closed the SECOND cross-leg cite. V5 asked: with the re-staging touching ΩA,
the locks-diff, the architecture-delta, and the MASTER leg AFTER V4, did the
re-staging (i) introduce a THIRD cross-leg cite/edit collision, (ii) drift any
load-bearing line cite, or (iii) revive any refuted route in the new content?

- **(i) No third collision.** The only ARCHITECTURE.md line-cites in the LOCKS
  leg are `:1206` (C9, demote-stable PRIMARY is `:1289`), `:1289` (C9 PRIMARY,
  NOT in any splice set), and `:1990`/`:1997` (C10, now PRE-strike-annotated +
  symmetric HALT-NOTE). The architecture-delta splice/hunk set is
  `:19`-`:37`/`:1151`/`:1171`/`:1186`/`:1206`/`:1990`/`:1205`/`:1307`/`:1398`/`:2402`/`:1274`/`:2146`
  + the two gated hunks (§7.4 title `:1370`, §9.2 strike `:1997`). Every LOCKS-leg
  cite landing on a splice target (`:1206`, `:1990`/`:1997`) carries its
  HALT-NOTE; `:1289` is demote-stable. No new uncovered collision.
- **(ii) No load-bearing cite drift.** The C8 `lib.rs:574` cite resolves to the
  live-path reach (the `find_css_significant(...)` call inside the test guard),
  not the fn header at `:562` — this is the "reached ONLY from … guard" semantic
  and is precise. The C10 `backend_egraph.rs:75`/`:191`-`:193`, C2
  `report.rs` 6-field, C11 census-71, and the REDRESS 96/97/98/246/247 cites all
  resolve at HEAD post-re-staging.
- **(iii) No revived route in new content.** ΩD's refuted census is 0; the
  §13.6→SK-V19 re-key is an identity pivot (F1-F9 verbatim); the §13.7 12-wave
  block carries the CH3-V1-R2 retime (G2/G4/G6 BLOCKED until SK-V16/V17 reconcile
  committed); migration-delta OP-3 fences the second-scanner / structural-stream
  / parser-local-cursor rejected shapes; no un-caveated MEASUREMENT-VALID
  survives.

## Enumerated Staged Amendments / CRUD Operations Under CH3 (V5 disposition)

### Ω-C — `locks-diff.md` (11 addendum clauses)

| # | Clause | CH3-V5 disposition |
|---|---|---|
| C1 | Named-primitive (a)-(d) gate | **ACCEPT** — SPEC-only → lock; no refuted route; STRENGTHENS Lock 14 |
| C2 | Relocated-seam firewall + un-fork | **ACCEPT** — un-fork REDRESS negative-witness 0; `css_provider_source` PLANNED-labelled (live only as bench field); md5-distinctness NECESSARY-NOT-SUFFICIENT carried |
| C3 | Neutrality-proof (forced demotion) | **ACCEPT** — `scoped non-JSON witness` faithful to live MP.NW6:662 / H.W4.LOCK14:605; no fabricated cross-grammar caller |
| C4 | aarch64-ONLY (x86 PRUNE target) | **ACCEPT** — FEAT_SVE2 ABSENT; svmatch REFUTED-on-host preserved, not revived |
| C5 | Verbatim-blob-courier prohibition | **ACCEPT** — Lock-6 co-bind; no refuted route |
| C6 | Green-by-exclusion precondition (P4-before-G2/G3) | **ACCEPT** — leak-surface promotion before gate |
| C7 | Single-SIMD-substrate + one-movemask | **ACCEPT** — skinny-scoped; `simd-scan:67` carry routed to SK-V19, NOT claimed as totality proof |
| C8 | Retarget-not-author | **ACCEPT** — `find_css_significant` wire-as-is REFUTATION honored (retargets, does not author); `:574` reach-cite precise |
| C9 | CollapsedStage shape-slot | **ACCEPT** — PRIMARY anchor demote-stable `:1289`; `:1206` SECONDARY carries OA-V10-05 HALT-NOTE; REDRESS 96/97/98 + UNKNOWN-2D-05 discharge sound; M5-Max scalar-cheaper finding verbatim at `:2928`-`:2933`. Route NOT revived. |
| C10 | Cursor-generality re-anchor (`<G>` strike) | **ACCEPT** — CH3-V4-01 APPLIED: C10 now carries the PRE-strike-carrier annotation + names the OA-V10-06 HALT-NOTE; the sibling cross-leg cite/edit class is closed class-wide; `NormalizeDirectSinkCost` regression-guard live; phantom-axis delete sound |
| C11 | Pattern-H re-census (67→71) | **ACCEPT** — +4 = tape-fold roster trace; 71 at HEAD |

### Ω-D — `master-plan-diff.md` (6 staged diffs)

| # | Diff | CH3-V5 disposition |
|---|---|---|
| D1 | §13.6 SK-V18 tape-fold → SK-V19 totality-fold re-key | **ACCEPT** — identity pivot; F1-F9 verbatim; refuted-census 0 (ΩD:53-54); no wave refuted |
| D2 | NEW §13.7 12-wave block + CH3-V1-R2 retime | **ACCEPT** — four-item "51/53/246/247" with "item 246 bounds G4"; G2/G4/G6 blocked until reconcile committed |
| D3 | §25 Implementation Order reconciliation | **ACCEPT** — monotonic SK-V15→V17→V18→V19; no route revived |
| D4 | §24 Carry Ledger re-key + SK-V19 tee-up rows | **ACCEPT** — 3 totality leaks DEFERRED to SK-V19, not bolted into SK-V18 |
| D5 | §5 F.W5 / §13.5 CSS verdict reconciliation | **ACCEPT** — CSS UPGRADE carries directional/H1-pending caveat; refuses un-caveated MEASUREMENT-VALID (CH2-V1-R03) |
| D6 | §13 H-row + Lock-10 cross-ref alignment | **ACCEPT** — label-only; 5-shape canon row UNCHANGED |

### Ω-E — `ΩE-skinny-corpus` (6 CRUD-5 surface updates)

| # | Surface | CH3-V5 disposition |
|---|---|---|
| E1 | INDEX (SK-V15 → SK-V18 W-PRUNE→G1..G6→PROVE→H1) | **ACCEPT** — supersession, not route-revival |
| E2 | WORKSPACE (telemetry → SPEC §3 gate schema) | **ACCEPT** — FNV stays quarantine/telemetry |
| E3 | HARDENING (seven-lens re-key + §6 (a)-(d) lens) | **ACCEPT** — adds delete-before-rebuild cycle detection |
| E4 | COMPILER (DELETE `RuntimeEmitterKind`, dispatch on BackendShape) | **ACCEPT** — un-fork NOVEL (REDRESS=0); deletion PLANNED-at-G3 |
| E5 | BENCH (CSS comparator inversion + track1_rich) | **ACCEPT** — win AND loss carry identical directional/loadavg/H1-pending caveat |
| E6 | SUBSTRATE (limited authority/status flip) | **ACCEPT** — "NO substrate data-structure change"; 5-shape canon + Lock 1 union preserved |

### Ω-F — `migration-delta` + `handoff-delta` + `architecture-delta`

| # | Operation | CH3-V5 disposition |
|---|---|---|
| F1 | MIGRATION OP-1 §0.0 SK-V18 receiver (renumber-down) | **ACCEPT** — historical receivers preserved as provenance |
| F2 | MIGRATION OP-2 rename/abrogate/refactor rows | **ACCEPT** — `css_types.rs` routed to SK-V19; phantom `<G>` LOCKS:620 strike recorded as Pass Omega CRUD-3 / SK-V19, NOT an Ω-F edit |
| F3 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate (§17/§19) | **ACCEPT** — `:116` four-item "51/53/246/247"; rejected shapes (`:119`-`:120`) fenced from G2/G4/G6, none revived |
| F4 | HANDOFF OP-1..OP-5 (override + blocker matrix + directive) | **ACCEPT** — blocker matrix maps phantom `<G>` AND CSS-Value-API to G4 with measurable gates |
| F5 | ARCHITECTURE-delta OA-V10-04..11 (2 gated hunks + 6 anchored splices) | **ACCEPT** — CH3-V4-01 APPLIED: the OA-V10-06 row (`:82`) now carries the symmetric HALT-NOTE naming the C10 `:1990`/`:1997`/`:1998` cite + PRE-EMPTS-the-reconcile instruction + re-grep-before/after; symmetric with the applied OA-V10-05 `:1206` note; both gated hunks `git apply --check` EXIT=0 |

## CH3-V5 Finding

**None.** V5 finds no genuine REGRESSION defect. The cross-leg cite/edit
defect-class — the engine of V3's and V4's single REVISEs — is now closed
CLASS-WIDE: the only two ARCHITECTURE.md cites in the LOCKS leg that land on
architecture-delta splice targets (`:1206`/C9 and `:1990`/`:1997`/C10) each carry
a symmetric bidirectional HALT-NOTE, each leg names the other, and both clauses
carry the demote-stable / PRE-strike-carrier framing. C9's PRIMARY anchor and the
C10 phantom-strike are both demote-stable or pre-empt-annotated. No third
colliding cite exists. No load-bearing cite drifted in the post-V4 re-staging. No
refuted route was revived in the new §13.6→SK-V19 / §13.7 content.

## What Did NOT Regress (ACCEPT rationale, the load-bearing nulls)

- **No revived REDRESS route.** The un-fork (`RuntimeEmitterKind` deletion /
  relocated-seam / `emit_shape_source`) owes NO REDRESS id (`grep -ic … == 0`) →
  SK-V18-NOVEL. REDRESS 96/97/98 (union-substrate cursor) and 51/53/246/247
  (second-scanner / structural-stream / parser-local cursor) are carried as
  BLOCKERS, never revived; the four-item set is correct in EVERY fence (zero
  surviving three-item under-citation); items 246/247 verified to EXIST at
  `REDRESS.md:6186`/`:6232` with item 246 = the W11T parse-only structural-STREAM
  driver, matching the attribution.
- **REDRESS-ledger completeness is HONESTLY caveated.** The committed ledger ends
  at SK-V15 W11 (`:6446`); SK-V16/V17 rejected routes are NOT yet captured — and
  the staged text SURFACES this (ΩB `:89`,`:202`-`:207` U-5) and routes the
  SK-V16/V17 REDRESS reconcile as a Pass-Omega-V10 / pre-W-PRUNE BLOCKER on
  G2/G4/G6 entry (ΩD `:81`-`:85`,`:100`; migration-delta `:113`-`:120`). No staged
  amendment over-claims a refuted-route fence as "complete against the REDRESS
  ledger" without the SK-V16/V17 caveat. This is the correct disposition of an
  incomplete ledger — a strength, not a defect.
- **No Lock-14 narrowing.** C6 green-by-exclusion + C1 named-primitive (a)-(d)
  STRENGTHEN Lock 14; the `<G>` strike (C10) re-anchors generality onto
  `Cursor`+config-breadth, preserving `@generated` grammar-neutrality; MP.NW6:662
  + H.W4.LOCK14:605 confirm "FactStream is a substrate-target classification, NOT
  a 6th BackendShape variant" — the 5-shape canon is intact across every leg.
- **No T-P2-refuted assertion revived.** "Wire-as-is" REFUTED →
  retarget-not-author (C8, `find_css_significant` kernel framing); NEON-svmatch on
  SVE2-absent hosts REFUTED → aarch64-ONLY FEAT_SVE2 ABSENT (C4);
  md5-distinctness NECESSARY-NOT-SUFFICIENT → structural row-collapse co-gate
  carried (C2/D2).
- **No coupling INTO the runtime.** The CH3 surface carries no runtime/architecture
  coupling; the CollapsedStage conditional rebuild stays G5/G6-GATED.
- **5-shape canon + 16-lock count preserved** byte-verbatim across every staged
  surface; `git apply --check` EXIT=0 for `locks-diff.md` and both
  `architecture-delta.staged.md` gated hunks; master-plan-diff makes no
  git-apply claim (its blocks are illustrative anchored snippets); no Lock 17.

## Disposition Summary

29 enumerated amendments/CRUD operations (11 Ω-C + 6 Ω-D + 6 Ω-E + 5 Ω-F at
clause/diff/surface granularity, the Ω-F census scoring the
`architecture-delta.staged.md` leg explicitly as F5):
**29 ACCEPT / 0 REVISE / 0 REJECT**.

No REJECT: no non-applying diff (`git apply --check` EXIT=0 for locks-diff and
both architecture-delta gated hunks; master-plan-diff correctly claims no apply),
no revived REDRESS route (un-fork REDRESS-id 0; 96/97/98 and 51/53/246/247
carried as blockers; ledger-completeness honestly caveated), no Lock-14
narrowing, no reintroduced runtime coupling, no uncited claim — items 246/247
verified present, the four-item table correct in every fence, the symmetric CSS
caveat, and the faithful 96/97/98 + UNKNOWN-2D-05 labelling all hold.

No REVISE: V5's load-bearing duty was to confirm the V4 REVISE survived the
post-V4 re-staging (it did — CH3-V4-01 is APPLIED both ways and the cross-leg
cite/edit class is closed class-wide) and to hunt the re-staging for a new
collision, a cite drift, or a revived route (none found). V1's three REVISEs,
V2's one, V3's one, and V4's one are ALL applied and re-verified ACCEPT at HEAD;
the CH3 axis is converged-clean for G-Omega.

On the cycle V1 ≥30% REVISE expectation: that bar is a V1 expectation and was MET
at V1 (3/9 = 33%). The progression V2(1)→V3(1)→V4(1)→V5(0) is monotone
convergence: each successive cycle found exactly the residue of its predecessor's
fix being applied point-wise, and V4 closed the cross-leg defect-class
class-wide. Padding a V5 census to ≥30% would require fabricating REGRESSION
findings the evidence does not support — every cross-leg cite resolves, every
HALT-NOTE is symmetric, every REDRESS fence is the full four-item set, and the
5-shape / 16-lock canon is byte-verbatim. The [no-workarounds] /
accurate-narrative discipline declines the pad. The honest CH3 verdict is full
convergence: 29 ACCEPT, 0 REVISE, 0 REJECT.

TALLY accept=29 revise=0 reject=0
