# CH3 REGRESSION — Pass Omega V10 (astral) CHALLENGE Lens, Cycle V3

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
`HANDOFF.md`) and the converged T-P1/T-P2/T-P3 evidence.

Date: 2026-06-01. HEAD `25297a7fc`-era staging; the whole
`restart/audit/totality/astral/V10/` tree is UNTRACKED (staged-only, not
committed). Cycle V3 re-reviews V1+V2 independently: it re-runs every
load-bearing spot-check at HEAD, confirms which V2 finding the staged text now
satisfies, and hunts CROSS-LEG residue the prior cycles did not reach (V2
audited the LOCKS leg in isolation; V3 widens to the ARCHITECTURE-delta leg that
edits the SAME lines the LOCKS leg cites).

## Load-Bearing Spot-Verifications (re-run at HEAD, V3)

| Check | Command / target | Result |
|---|---|---|
| Staged locks-diff applies | `awk … | git apply --check -` on `locks-diff.md` | **exit 0** (applies cleanly) |
| 16 numbered locks / no Lock 17 | `grep -cE '^[0-9]+\. \*\*'` / `grep -cE '^17\. '` | **16** / **0** — addendum adds no Lock 17 |
| 5 BackendShape variants | `lower/mod.rs:18-24`; `cost.rs:334 all_backend_shapes()->[BackendShape;5]` | 5 `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no 6th |
| PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed skinny/crates skinny/xtask`; `… bbnf_simd_single_mask_convention skinny/crates` | both **0** (written PLANNED, not live) |
| Un-fork owes no REDRESS id | `grep -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md` | **0** — un-fork is SK-V18-NOVEL, NOT a refuted route |
| V2 REVISE `:1282`→`:1206`/`:1289` APPLIED | `locks-diff.md:75` C9 clause | **APPLIED** — C9 now cites `ARCHITECTURE.md:1206` ledger row + `:1289` U3 directive; `:1282` gone from all production surfaces |
| `:1206` is the UNKNOWN-2D-05 ledger row | `ARCHITECTURE.md:1206` | confirmed: carries verbatim "aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64 strategy before any aarch64 admission)" |
| `:1282` is WIRE-posture text (not UNKNOWN-2D-05) | `ARCHITECTURE.md:1282` | confirmed "wiring the existing engine." — V2's diagnosis correct, defect now absent from C9 |
| `:1289` is the U3 directive | `ARCHITECTURE.md:1289` | confirmed "CollapsedStage = UNKNOWN-2D-05, no admission without a 2E source-backed…" |
| Four-item REDRESS set 51/53/246/247 | master-plan-diff `:202`,`:338`; migration-delta `:116`; ΩF `:162`,`:277` | all FOUR items present; **zero** surviving "51/53/247" three-item fence in any V10 production surface |
| MP §7 invariant carries the four-item set | `master-plan-diff.md:338` | present — V1 CH3-V1-02 "add to §7" correction satisfied |
| ΩE BENCH dual caveat (win AND loss) | `ΩE-skinny-corpus.md:301-305`,`:377-379` | symmetric: BOTH 1.66-3.38× win AND 0.60-0.76× loss carry directional/loadavg-4.35/H1-pending; forbidden MEASUREMENT-VALID honored |
| C2 `css_provider_source` labelled PLANNED | `locks-diff.md:61` | "a PLANNED SK-V18 firewall predicate, distinct from the live `bbnf-bench/src/report.rs` bench-report field of the same name" — V2 secondary obs FOLDED |
| `css_provider_source` live only as bench field | `rg -c skinny/crates` | 6 occurrences, all in `bbnf-bench/src/report.rs` — label is accurate |
| C8 retarget kernel live | `runtime_simd.rs:169 find_css_significant` → `byte_class_from_eq_set_64` at `:199`; guard `neon_significant_skip_matches_scalar` at `lib.rs:562` | live; retargets, does not author — T-P2 "wire-as-is" REFUTATION honored |
| C4 aarch64-ONLY + FEAT_SVE2 ABSENT | `2E:244` LAC-2E-V6-01 | "FEAT_SVE2 ABSENT"; x86 = P1 DELETION; x86/AVX-512/GFNI = totality-SECONDARY, closes NO skinny row |
| C9 REDRESS 96/97/98 labelling | `REDRESS.md:2797`(96),`:2852`(97),`:2910`(98 retires `G-W3-UNION-SUBSTRATE`); finding `:2928-2933` | accurate: 96/97 = two faithful regressed union-substrate impls; 98 = gate retirement; finding = M5-Max scalar-cheaper — no conflation |
| C10 e-graph regression-guard live | `passes/backend_egraph.rs:193 NormalizeDirectSinkCost` asserted Rewrite | live — a guard, not a revival |
| C11 Pattern-H 71 + the +4 | `find crates/core/src/runtime -mindepth 2 …| wc -l` = 71; `tape/{mod,cursor,arena,record}.rs` exist | 71 at HEAD; +4 tape-fold roster trace verified |
| `simd-scan:67` 2nd classifier carry | `crates/simd-scan/src/lib.rs:67` | `pub use alphabet::{KernelShape,NibbleLut,StructuralAlphabet,WideLut}` present, distinct from `:68` probe API — SK-V19 carry, NOT a totality single-substrate proof |
| §H wave resolution (MP.NW6 / H.W4.LOCK14) | `MASTER-PLAN.md:662`,`:605` | `scoped non-JSON witness` single-negative-control standard FAITHFUL; "FactStream is a substrate-target classification, NOT a 6th BackendShape" — 5-shape canon preserved |
| ARCHITECTURE-delta gated hunks apply | `architecture-delta.staged.md` 2 gated hunks | the §7.4-title + §9.2-phantom-strike hunks `git apply --check` exit 0; the 4 anchored splices are re-grep-HALT span edits |

## What V2's Single REVISE Became (ADDRESSED in the staged text)

Cycle V3's first duty is to confirm whether the staged text absorbed the V2
REVISE. It did:

- **V2 CH3-V2-01 (`:1282` mis-cite in C9) → ADDRESSED.** The C9 CollapsedStage
  clause (`locks-diff.md:75`) now reads "DISCHARGES UNKNOWN-2D-05
  (`restart/ARCHITECTURE.md:1206` the CollapsedStage ledger row + `:1289` the U3
  directive…)". The bad `:1282` is GONE from every production surface
  (locks-diff, ΩC, ΩD, ΩF, master-plan-diff, migration-delta, handoff-delta —
  `grep :1282` returns hits ONLY in the V1/V2 hardening verdicts, where V2's
  "propagate the fix to hardening/V1/CH3.md" was honored: V1/CH3.md:111,:123 now
  read "corrected per CH3-V2-01 — `:1282` is WIRE-posture text"). At HEAD `:1206`
  is the ledger row carrying the attributed verbatim UNKNOWN-2D-05 quote and
  `:1289` is the U3 directive — both cites RESOLVE. This flips C9 to ACCEPT at
  the V3 census.
- **V2 secondary observation (`css_provider_source` status label) → ADDRESSED.**
  The C2 firewall clause now labels it "a PLANNED SK-V18 firewall predicate,
  distinct from the live `bbnf-bench/src/report.rs` bench-report field of the
  same name; not yet a codegen/firewall gate symbol" (`locks-diff.md:61`),
  matching the live state (`css_provider_source` = 6 report.rs occurrences,
  zero codegen). The status-label asymmetry V2 flagged is resolved.

## Enumerated Staged Amendments / CRUD Operations Under CH3 (V3 disposition)

### Ω-C — `locks-diff.md` (11 addendum clauses)

| # | Clause | CH3-V3 disposition |
|---|---|---|
| C1 | Named-primitive (a)-(d) gate | **ACCEPT** — SPEC-only → lock; no refuted route; T-P2 literature-validated |
| C2 | Relocated-seam firewall + un-fork | **ACCEPT** — REDRESS negative-witness 0; "UNBUILT at HEAD" stated; `css_provider_source` now PLANNED-labelled (V2 sec-obs folded); CSS scope split (skinny scan / totality DEFER) |
| C3 | Neutrality-proof (`css_balanced_component_scan` forced demotion) | **ACCEPT** — `scoped non-JSON witness` faithful to live MP.NW6:662; no fabricated cross-grammar caller |
| C4 | aarch64-ONLY (x86 PRUNE target) | **ACCEPT** — FEAT_SVE2 ABSENT carried (2E:244); svmatch REFUTED-on-host preserved, not revived |
| C5 | Verbatim-blob-courier prohibition | **ACCEPT** — Lock-6 co-bind; no refuted route |
| C6 | Green-by-exclusion precondition (P4-before-G2/G3) | **ACCEPT** — `FORBIDDEN_GENERIC_TOKENS` byte-identical across 3A-D11/3C/3B-P4/3D-D04 |
| C7 | Single-SIMD-substrate + one-movemask | **ACCEPT** — skinny-scoped; `simd-scan:67` carry routed to SK-V19, NOT claimed as totality proof |
| C8 | Retarget-not-author | **ACCEPT** — live kernel `find_css_significant`→`byte_class_from_eq_set_64` verified; T-P2 "wire-as-is" REFUTATION honored |
| C9 | CollapsedStage shape-slot | **REVISE** (CH3-V3-01) — route NOT revived, REDRESS 96/97/98 + UNKNOWN-2D-05 discharge sound, V2's `:1282` fix APPLIED — but the now-correct `:1206` cite anchor is a line the sibling `architecture-delta.staged.md` OA-V10-05 concurrently demote-splices; the cross-leg coupling is unflagged |
| C10 | Cursor-generality re-anchor (`<G>` strike) | **ACCEPT** — deletes phantom axis; `NormalizeDirectSinkCost` regression-guard live |
| C11 | Pattern-H re-census (67→71) | **ACCEPT** — +4 = tape-fold roster trace (`tape/{mod,cursor,arena,record}.rs`); 71 verified at HEAD |

### Ω-D — `master-plan-diff.md` (6 staged diffs)

| # | Diff | CH3-V3 disposition |
|---|---|---|
| D1 | §13.6 SK-V18 tape-fold → SK-V19 totality-fold re-key | **ACCEPT** — identity pivot; F1-F9 verbatim; refuted-census 0; no wave refuted |
| D2 | NEW §13.7 12-wave block + CH3-V1-R2 retime | **ACCEPT** — retime line `:198-204` reads full four-item "51/53/246/247" with "item 246 = … bounds G4" |
| D3 | §25 Implementation Order reconciliation | **ACCEPT** — monotonic skinny→totality restored; no route revived |
| D4 | §24 Carry Ledger re-key + SK-V19 tee-up rows | **ACCEPT** — 3 totality leaks DEFERRED to SK-V19 (3B:177/:197 "DEFER, do NOT bolt"), not bolted into SK-V18 |
| D5 | §5 F.W5 / §13.5 CSS verdict reconciliation | **ACCEPT** — CSS UPGRADE carries directional/H1-pending caveat; refuses un-caveated MEASUREMENT-VALID (CH2-V1-R03) |
| D6 | §13 H-row + Lock-10 cross-ref alignment | **ACCEPT** — label-only; 5-shape canon row UNCHANGED |

### Ω-E — `ΩE-skinny-corpus` (6 CRUD-5 surface updates)

| # | Surface | CH3-V3 disposition |
|---|---|---|
| E1 | INDEX (SK-V15 → SK-V18 W-PRUNE→G1..G6→PROVE→H1) | **ACCEPT** — supersession, not route-revival; SK-V14/V15 kept historical |
| E2 | WORKSPACE (telemetry → SPEC §3 gate schema) | **ACCEPT** — no refuted construct re-armed; FNV stays quarantine/telemetry |
| E3 | HARDENING (seven-lens re-key + §6 (a)-(d) lens) | **ACCEPT** — CH3 lens adds delete-before-rebuild cycle detection |
| E4 | COMPILER (DELETE `RuntimeEmitterKind`, dispatch on BackendShape) | **ACCEPT** — un-fork NOVEL (REDRESS=0); deletion PLANNED-at-G3, matches live "still branches" |
| E5 | BENCH (CSS comparator inversion + track1_rich) | **ACCEPT** — V1 CH3-V1-03 satisfied: win AND loss carry identical directional/loadavg/H1-pending caveat |
| E6 | SUBSTRATE (limited authority/status flip) | **ACCEPT** — "NO substrate data-structure change"; 5-shape canon + Lock 1 union preserved |

### Ω-F — `migration-delta` + `handoff-delta` + `architecture-delta`

| # | Operation | CH3-V3 disposition |
|---|---|---|
| F1 | MIGRATION OP-1 §0.0 SK-V18 receiver (renumber-down) | **ACCEPT** — historical receivers preserved as provenance |
| F2 | MIGRATION OP-2 rename/abrogate/refactor rows | **ACCEPT** — `css_types.rs` routed to SK-V19, not silently dropped; phantom `<G>` LOCKS strike routed to SK-V19/CRUD-3 |
| F3 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate (§17/§19) | **ACCEPT** — `:116` reads full four-item "51/53/246/247"; rejected shapes (second scanner / structural-stream driver / parser-local cursor) fenced, none revived |
| F4 | HANDOFF OP-1..OP-5 (override + blocker matrix + directive) | **ACCEPT** — blocker matrix maps phantom `<G>` AND CSS-Value-API to G4 with measurable gates; ΩF CH3 row `:277` carries the four-item set |
| F5 | ARCHITECTURE-delta OA-V10-04..11 (2 gated hunks + 4 anchored splices) | **REVISE** (CH3-V3-01) — the OA-V10-05 demote-to-diagnostic splice edits `ARCHITECTURE.md:1206`, the EXACT line the locks-diff C9 clause cites for the UNKNOWN-2D-05 discharge; the cross-leg `:1206` collision is unsequenced and unflagged |

## CH3-V3 Finding (the REVISE)

### CH3-V3-01 — REVISE: the V2-applied C9 `:1206` cite anchor is concurrently demote-spliced by the sibling `architecture-delta.staged.md` OA-V10-05 — an unflagged cross-leg coupling that re-creates the V2 falsifiability failure through the other leg

Artefacts: `restart/audit/totality/astral/V10/locks-diff.md:75` (the C9
CollapsedStage clause, LOCKS/CRUD leg) AND
`restart/audit/totality/astral/V10/architecture-delta.staged.md:81` (OA-V10-05,
the §7.3 x86-pin demote, ARCHITECTURE/CRUD-1 leg).

V2's CH3-V2-01 correctly re-pointed the C9 UNKNOWN-2D-05 discharge cite from the
WIRE-posture `:1282` to `restart/ARCHITECTURE.md:1206` (the CollapsedStage
ledger row) + `:1289` (the U3 directive). That fix is APPLIED and `:1206` does
carry the attributed verbatim quote at HEAD:

> `| `CollapsedStage` | x86 AVX-512 … | **`target.arch == x86` + `target.avx512bw`
> + `Entry(_)`** (LAC-2D-06; aarch64 mechanically refused) | … | **NOT-ADMITTED**:
> x86-only; aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64
> strategy before any aarch64 admission); … |`

But the SAME line `:1206` is ALSO a demote-to-diagnostic edit target in the
sibling ARCHITECTURE leg. `architecture-delta.staged.md:81` (OA-V10-05) lists
"demote-to-diagnostic splices at `:1151`/`:1171`/`:1186`/**`:1206`**;
CollapsedStage SHAPE SLOT retained", and the ΩA coherence audit confirms the
scope verbatim: OA-V10-05 at `ΩA-coherence-audit.md:141` records "`:1151`,`:1171`,
**`:1206`** hard-code `target.arch == x86` as the CollapsedStage co-require", and
`:1240` reads "`admits_collapsed_stage` x86-binding (`:1151`, **`:1206`**) …
mechanically". So OA-V10-05's demote-to-diagnostic edit MUTATES the
`target.arch == x86 + target.avx512bw` framing that the C9 clause quotes verbatim
and attributes to `:1206`.

This is a genuine cross-leg coupling defect, and it is the V2 failure-class
reintroduced through the other leg:

1. The two legs are unsequenced. The CRUD application order in `ΩF:178-183` runs
   CRUD-1 (ARCHITECTURE, `architecture-delta`) and the LOCKS leg
   (`locks-diff`) as separate CRUD operations; nothing orders the `:1206`
   demote-splice relative to the C9 cite's resolution, and nothing in
   either staged file flags that the C9 `:1206` anchor is a concurrent
   ARCHITECTURE edit target.
2. The architecture-delta's re-grep HALT anchor for OA-V10-05 is the `:1151`
   string (`(LAC-2D-06 binds \`admits_collapsed_stage\`…`), NOT `:1206`. So the
   operator applying the four-site demote-splice has no anchor warning that
   `:1206` is also a load-bearing LOCKS cite — the demote can land silently.
3. After the demote-to-diagnostic splice mutates `:1206`'s
   `target.arch == x86 + target.avx512bw` co-require framing, a reader following
   the C9 LOCKS cite to verify the UNKNOWN-2D-05 discharge will find ALTERED
   text — the EXACT falsifiability failure (cite-points-at-mutated-line) that
   V2's CH3-V2-01 raised the `:1282`→`:1206` fix to prevent. V2 fixed the cite to
   point at the right line; it did NOT see that the right line is itself being
   re-written by the sibling leg.

This is NOT a route revival (CollapsedStage stays a SHAPE SLOT,
demote-to-diagnostic not retired; the x86-mechanically-refused row is not
re-opened), NOT a Lock-14 narrowing, NOT a 6th shape, NOT a coupling INTO the
runtime — the discharge gate itself is sound. It is a staged-CRUD coupling-
precision defect: two staged legs edit/cite the same governance line with no
sequencing or HALT-anchor binding them.

Correction (any ONE of):
- (a) Add `:1206` to the OA-V10-05 re-grep-HALT anchor set in
  `architecture-delta.staged.md:81` with an explicit note "`:1206` is the C9
  locks-diff UNKNOWN-2D-05 discharge cite — demote the x86 co-require WORDING but
  PRESERVE the `aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed
  aarch64 strategy before any aarch64 admission)` clause verbatim", so the
  demote does not strand the LOCKS cite; AND
- (b) Add a CRUD application-order sentence (ΩF or the architecture-delta
  invariant block) ordering the C9 cite resolution AFTER the `:1206` demote, OR
  re-point the C9 cite at the demote-STABLE `:1289` U3 directive as the PRIMARY
  anchor (the U3 directive at `:1289` carries the same `no admission without a 2E
  source-backed strategy` bar and is NOT in the OA-V10-05 splice set), keeping
  `:1206` as the secondary ledger reference.

This is a citation/coupling-precision REVISE; the discharge gate and the 5-shape
canon are correct. It mirrors the V2 finding one level up: V2 fixed WHICH line
C9 cites; V3 fixes that the cited line is concurrently rewritten by a sibling
staged leg.

## What Did NOT Regress (ACCEPT rationale, the load-bearing nulls)

- **No revived REDRESS route.** Every staged deletion/retirement (x86 crate,
  `CSS_GENERATED_RS` courier, css_l4 replicas, phantom `<G>`, the
  `RuntimeEmitterKind` fork) is fenced PRUNE-before-GENERALIZE
  (`migration-delta.staged.md` OP-3) and the un-fork owes no REDRESS id
  (`grep -ic … skinny/REDRESS.md == 0`). REDRESS 96/97/98 (union-substrate
  cursor) and 51/53/246/247 (second-scanner/structural-stream/parser-local
  cursor) are carried as BLOCKERS, never as revived routes; the four-item set is
  correct in EVERY staged fence (no surviving three-item under-citation).
- **No Lock-14 narrowing.** C6 green-by-exclusion + C1 named-primitive (a)-(d)
  STRENGTHEN Lock 14; the `<G>` strike (C10) re-anchors generality onto
  `Cursor`+config-breadth, preserving `@generated` grammar-neutrality and
  `preserve-rich-ast`; MP.NW6:662 confirms "FactStream is a substrate-target
  classification, NOT a 6th BackendShape" — the 5-shape canon is intact across
  every leg.
- **No coupling INTO the runtime.** The CollapsedStage conditional rebuild is
  G5/G6-GATED on a profiled hot leaf, not a committed build; the firewall keeps
  skinny-vs-totality scan scope distinct (skinny `css_l4_*`+`runtime_simd.rs`
  scanned; totality `crates/core/src/runtime/css_l4/` DEFERRED to SK-V19 under
  3B:177/:197). The CH3-V3-01 coupling is a STAGED-DOC cross-leg cite/edit
  collision, not a runtime/architecture coupling.
- **No T-P2-refuted assertion revived.** "Wire-as-is" REFUTED →
  retarget-not-author (C8, live kernel verified); `MatchSetSve2`/NEON-svmatch on
  SVE2-absent hosts REFUTED → aarch64-ONLY with FEAT_SVE2 ABSENT (C4);
  md5-distinctness NECESSARY-NOT-SUFFICIENT → structural co-gate carried (C2/D2).
- **5-shape canon + 16-lock count preserved** byte-verbatim across every staged
  surface; the diff is amendment-by-addition; `git apply --check` exit 0 for
  both `locks-diff.md` and the two `architecture-delta.staged.md` gated hunks;
  no Lock 17.

## Disposition Summary

29 enumerated amendments/CRUD operations (11 Ω-C + 6 Ω-D + 6 Ω-E + 5 Ω-F,
the Ω-F census widened this cycle to score the `architecture-delta.staged.md`
leg explicitly as F5), scored at clause/diff/surface granularity:
**28 ACCEPT / 1 REVISE / 0 REJECT**.

No REJECT: no non-applying diff (`git apply --check` exit 0 for locks-diff and
both architecture-delta gated hunks), no revived REDRESS route, no Lock-14
narrowing, no reintroduced runtime coupling, no uncited claim — the un-fork's
NOVELTY (REDRESS=0), the now-correct four-item table in every fence, the
symmetric CSS caveat, and the faithful 96/97/98 + UNKNOWN-2D-05 labelling keep
the lone finding at coupling/citation-precision level.

V3 differs from V2 in WHERE it found the residue, not in census shape. V2's
single REVISE (the `:1282` mis-cite) and V1's three (framing, item-246, BENCH
caveat) are ALL applied and re-verified ACCEPT at HEAD. V3's single REVISE
(CH3-V3-01) is the V2 falsifiability-failure recurring one level up: V2 fixed
WHICH line C9 cites (`:1206`); V3 finds that the cited `:1206` is concurrently
demote-spliced by the sibling `architecture-delta` OA-V10-05 with no sequencing
or HALT-anchor binding the two staged legs — found only because V3 widened scope
to the ARCHITECTURE-delta leg the prior cycles audited in isolation.

On the CH3-eligible amendment surface (the REDRESS-fence + refuted-assertion +
cross-leg-cite clauses CH3 touches with weight: C2, C4, C7, C8, C9, C10, D2, D5,
E5, F3, F5 = 11), the V3 REVISE rate is 1/11 = 9%, BELOW the ≥30% cycle bar.
This is the correct and honest count: the bar was MET at V1 (3/9 = 33%), the V1
and V2 REVISEs were applied, and the surface is now near-converged on the CH3
axis. The ≥30% expectation is a V1 expectation; padding a V3 census to ≥30%
would require fabricating findings the evidence does not support, which the lens
declines to do. The single genuine REVISE (CH3-V3-01, a real unsequenced
cross-leg `:1206` cite/edit collision) is the full residue; the recommended
disposition is APPLY CH3-V3-01 (add the `:1206` HALT-anchor + preserve the
UNKNOWN-2D-05 clause verbatim, OR re-point C9's primary anchor to the
demote-stable `:1289`), after which the CH3 axis is converged-clean for G-Omega.

TALLY accept=28 revise=1 reject=0
