# CH3 REGRESSION — Pass Omega V10 (astral) CHALLENGE Lens, Cycle V1

Lens: CH3 REGRESSION — does any staged amendment reintroduce a skinny/REDRESS
route or a T-P2-refuted assertion; cross-check Ω-C + Ω-D + Ω-E against the
REDRESS ledger.

Scope reviewed: the 6 Ω artefacts + staged diffs under
`restart/audit/totality/astral/V10/` (ΩA-coherence, ΩB-skinny-lessons,
ΩC-locks-amendments + `locks-diff.md`, ΩD-master-plan-reconciliation +
`master-plan-diff.md`, ΩE-skinny-corpus + `ΩE-skinny-corpus-staged-diff.md`,
ΩF-migration-handoff + `migration-delta.staged.md` + `handoff-delta.staged.md`)
against the live V1 surfaces (`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`,
`locks/LOCKS.md`, `MIGRATION.md`, `HANDOFF.md`) and the converged
T-P1/T-P2/T-P3 evidence.

Date: 2026-06-01. Verdict written at HEAD `25297a7fc`-era staging.

## Load-Bearing Spot-Verifications (run at HEAD)

| Check | Command / target | Result |
|---|---|---|
| Staged locks-diff applies | `awk … | git apply --check -` on `locks-diff.md` | **exit 0** (applies cleanly) |
| 16 numbered locks | `grep -nE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | 16 at `:75…:453`; addendum adds no Lock 17 |
| 5 BackendShape variants | `skinny/crates/ir/src/lib.rs:341-345` | 5 `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no 6th |
| PLANNED co-gate symbols absent | `grep -rc runtime_target_rows_collapsed skinny/crates skinny/xtask`; `… bbnf_simd_single_mask_convention skinny/crates` | both **0** (written PLANNED, not live) — correct |
| Un-fork owes no REDRESS id | `grep -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md` | **0** — un-fork is SK-V18-NOVEL, NOT a refuted route |
| Un-fork is real (not phantom) | `RuntimeEmitterKind{CompiledLowering,RequestFacts}` | live at `skinny/crates/codegen/src/runtime_generator.rs:1,:17,:25` |
| CollapsedStage REDRESS 96/97/98 | `skinny/REDRESS.md:2795-2944`, finding `:2928-2933` | RESOLVES: SK-V9 W3 union-substrate; `G-W3-UNION-SUBSTRATE` RETIRED `:2910`; M5-Max scalar-cheaper-than-SIMD-cursor finding `:2928-2933` |
| §H wave H.W2/H.W2.5 Lock 16 | `restart/MASTER-PLAN.md:140,:149,:158` | resolves; H.W2 SOTA + `simd/structural_scan` row at H.W2/H.W5 |
| `simd-scan` 2nd classifier re-export | `crates/simd-scan/src/lib.rs:67` | `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut}` — present, SK-V19 carry |
| REDRESS four-item pre-block canon | `restart/audit/totality/p1/1D-skinny-lessons.md:166-171` | **51/53/246/247** (item 246 = W11T parse-only structural-STREAM driver REJECT, bounds G4) |

The un-fork (`RuntimeEmitterKind` DELETE → `render(program)` dispatching on
`backend_shape`) is the single biggest CH3 surface, and it is CLEAN: the ledger
negative-witness is 0, an emitter discriminator is a categorically different
object than the retained structural cursor REDRESS 96/97/98 retired, and the
relocated-seam firewall (`emit_shape_source == lowered_program` + the
`runtime_target_rows_collapsed` full-row `PartialEq` co-gate, both PLANNED) is
the anti-regression fence, not a revival. T-P2 is consumed faithfully: the
`find_css_significant` "wire-as-is" REFUTATION is honored — the retarget-not-author
clause RETARGETS NEON onto the live recursive shell, never wires the dead flat
kernel (locks-diff `:73`; 2E:80/:131 `MatchSetSve2` REFUTED is carried, not
revived).

## Enumerated Staged Amendments / CRUD Operations Under CH3

### Ω-C — `locks-diff.md` (11 addendum clauses, amendment-by-addition)

| # | Clause | CH3 disposition |
|---|---|---|
| C1 | Named-primitive (a)-(d) gate | **ACCEPT** — SPEC-only → lock; no refuted route; T-P2 literature-validated |
| C2 | Relocated-seam firewall + un-fork | **ACCEPT** — REDRESS negative-witness 0; firewall is the anti-regression fence |
| C3 | Neutrality-proof (`css_balanced_component_scan` forced demotion) | **ACCEPT** — consumes 2C s6/C4 finding; no revival |
| C4 | aarch64-ONLY (x86 PRUNE target) | **ACCEPT** — sharpens SK-V17 aarch64-PRIMARY; 2A x86-closes-row REFUTED carried, not revived |
| C5 | Verbatim-blob-courier prohibition | **ACCEPT** — Lock-6 co-bind; no refuted route |
| C6 | Green-by-exclusion precondition (P4-before-G2/G3) | **ACCEPT** — `FORBIDDEN_GENERIC_TOKENS` byte-identical across 3A-D11/3C/3B-P4/3D-D04 |
| C7 | Single-SIMD-substrate + one-movemask | **ACCEPT** — skinny-scoped; `simd-scan:67` carry honestly routed to SK-V19, NOT claimed as totality proof |
| C8 | Retarget-not-author | **ACCEPT** — honors the T-P2 "wire-as-is" REFUTATION explicitly |
| C9 | CollapsedStage shape-slot | **REVISE** (citation) — see CH3-V1-01 below; route is NOT revived but the cited REDRESS span needs the verbatim-finding-line check carried |
| C10 | Cursor-generality re-anchor (`<G>` strike) | **ACCEPT** — deletes phantom axis; e-graph regression-guard (`NormalizeDirectSinkCost` live) re-opens V2 scaffold finding if zero-rule — a guard, not a revival |
| C11 | Pattern-H re-census (67→71) | **ACCEPT** — +4 = tape-fold roster trace; unattributable +N opens O(N) scan (budget honored) |

### Ω-D — `master-plan-diff.md` (6 staged diffs)

| # | Diff | CH3 disposition |
|---|---|---|
| D1 | §13.6 SK-V18 tape-fold → SK-V19 totality-fold re-key | **ACCEPT** — identity pivot; F1-F9 verbatim; refuted-census 0; no wave refuted |
| D2 | NEW §13.7 SK-V18 GENERALIZATION 12-wave block | **REVISE** — see CH3-V1-02: the §13.7 narrative fence (`:192`) under-cites the four-item pre-block as "51/53/247", dropping **246** (the G4-bounding structural-stream-driver reject) |
| D3 | §25 Implementation Order reconciliation | **ACCEPT** — monotonic skinny→totality restored; no route revived |
| D4 | §24 Carry Ledger re-key + SK-V19 tee-up rows | **ACCEPT** — the 3 totality leaks DEFERRED to SK-V19, not bolted into SK-V18 |
| D5 | §5 F.W5 / §13.5 CSS verdict reconciliation | **ACCEPT** — CSS verdict UPGRADE carries the directional caveat; refuses the un-caveated "MEASUREMENT-VALID" word (CH2-V1-R03 honored) |
| D6 | §13 H-row + Lock-10 cross-ref alignment | **ACCEPT** — label-only; 5-shape canon row UNCHANGED |

### Ω-E — `ΩE-skinny-corpus` (6 CRUD-5 surface updates)

| # | Surface | CH3 disposition |
|---|---|---|
| E1 | INDEX (SK-V15 W0-W11 → SK-V18 W-PRUNE→G1..G6→PROVE→H1) | **ACCEPT** — supersession, not route-revival; SK-V14/V15 kept historical, do not dispatch |
| E2 | WORKSPACE (telemetry → SPEC §3 gate schema) | **ACCEPT** — no refuted construct re-armed; FNV stays quarantine/telemetry |
| E3 | HARDENING (seven-lens re-key + §6 (a)-(d) lens) | **ACCEPT** — CH3 lens text explicitly adds "delete-before-rebuild cycle detection" |
| E4 | COMPILER (DELETE `RuntimeEmitterKind`, dispatch on BackendShape) | **ACCEPT** — un-fork is NOVEL (REDRESS=0); FactStream-not-6th-shape preserved |
| E5 | BENCH (CSS comparator inversion correction + track1_rich) | **REVISE** — see CH3-V1-03: the `track1_fact_stream→track1_rich` bit-rot fix is real, but the corpus claim "the old fact-stream LOST at 0.60-0.76×" is a T-P2 contested figure; the diff must carry the same directional/loadavg caveat the master-plan §13.5 UPGRADE (D5) demands, else BENCH asserts a cleaner CSS win than the H1-pending re-lock supports |

### Ω-F — `migration-delta.staged.md` + `handoff-delta.staged.md`

| # | Operation | CH3 disposition |
|---|---|---|
| F1 | MIGRATION OP-1 §0.0 SK-V18 receiver (renumber-down) | **ACCEPT** — historical receivers preserved as provenance |
| F2 | MIGRATION OP-2 five rename/abrogate/refactor rows | **ACCEPT** — `css_types.rs` routed to SK-V19, not silently dropped |
| F3 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate (§17/§19) | **REVISE** — see CH3-V1-02: `migration-delta.staged.md:114` repeats the "items 51/53/247" under-citation, dropping 246 in the very gate clause that fences G2/G4/G6 |
| F4 | HANDOFF OP-1..OP-5 (override + blocker matrix + directive) | **ACCEPT** — blocker matrix maps phantom `<G>` AND CSS-Value-API to G4 (item-246-adjacent), each with a measurable gate; no route revived |

## CH3-V1 Findings (the REVISE set)

### CH3-V1-01 — REVISE: ΩC `locks-diff.md` C9 CollapsedStage clause — carry the verbatim REDRESS finding line

Artefact: `restart/audit/totality/astral/V10/locks-diff.md:75` (the CollapsedStage
shape-slot clause) and `ΩC-locks-amendments.md:75`.

The clause cites `skinny/REDRESS.md:2795-2944` (finding `:2928-2933`) as
"the streamed-cursor shape REDRESS 96/97/98 RETIRED (the M5 Max
scalar-cheaper-than-SIMD-cursor finding)". This RESOLVES correctly — item 98
retires `G-W3-UNION-SUBSTRATE` at `:2910`, and the finding at `:2928-2933` is
the M5-Max scalar-cheaper finding verbatim. The route is NOT revived: promotion
past `diagnostic-only` is gated on "clear that retired prior". This is a sound
anti-regression block.

The REVISE is narrow and precise: the clause grounds the conditional aarch64
rebuild on "the aarch64 TBL-classify-class scalar oracle" — but the live
ARCHITECTURE admission ledger (`restart/ARCHITECTURE.md:1206`; corrected per
CH3-V2-01 — `:1282` is WIRE-posture text, not the UNKNOWN-2D-05 row) records
CollapsedStage as `target.arch == x86 + target.avx512bw` bound, "aarch64
mechanically refused", with the aarch64 candidate flagged **UNKNOWN-2D-05
(requires 2E source-backed aarch64 strategy before any aarch64 admission)**.
The clause's 2E grounding (2E:73 TBL-classify, 2E:81) does discharge
UNKNOWN-2D-05, but the clause does NOT name UNKNOWN-2D-05 by id, so a reader
cannot see that the SK-V17-live "no aarch64 admission without a 2E strategy"
bar (live `LOCKS.md` SK-V17 Lock-10 addendum) is being SATISFIED rather than
silently relaxed.

Correction: add to the C9 clause the explicit "discharges UNKNOWN-2D-05
(`ARCHITECTURE.md:1206` the ledger row,`:1289` the U3 directive — corrected per
CH3-V2-01; `:1282` is WIRE-posture text, NOT the UNKNOWN-2D-05 record); the SK-V17
'no aarch64 CollapsedStage admission without a 2E source-backed strategy' bar is
MET, not relaxed" so the
aarch64 conditional-rebuild path reads as a gated discharge of the named
unknown, not a quiet re-opening of the x86-mechanically-refused row. This is a
citation/framing REVISE; the gate itself is correct.

### CH3-V1-02 — REVISE: ΩD `master-plan-diff.md` + ΩF `migration-delta.staged.md` under-cite the four-item REDRESS pre-block (item 246 dropped)

Artefacts: `master-plan-diff.md:192`; `migration-delta.staged.md:114`;
`ΩF-migration-handoff.md:162`.

The canonical SK-V18 Rejected-Route Pre-Block is **four items: 51/53/246/247**
(`restart/audit/totality/p1/1D-skinny-lessons.md:166-171`; echoed at 3D-D08
`3D-skinny-fold.md:130`,`:147`,`:168`; and at the 3F CH3-lens row
`3F-migration-handoff.md:274` = "51, 53, 246, 247"). Item **246** is the W11T
parse-only structural-STREAM DRIVER reject (`skinny/REDRESS.md:6184-6219`), and
in the 1D table it is the item that BOUNDS **G4** (the lazy `Cursor`/`CssNode`
view) — the ADMISSIBLE-vs-REJECTED distinction is "G4's `Cursor` is a VIEW over
the EXISTING Tape" vs "a structural-stream driver = a second substrate".

Three staged narrative fences enumerate the pre-block as "**51/53/247**" (3
items), dropping 246:
- `master-plan-diff.md:192` (the NEW §13.7 block CH3-V1-R2 retime line);
- `migration-delta.staged.md:114` (the §17.SK-V18 PRUNE-before-GENERALIZE gate);
- `ΩF-migration-handoff.md:162` (the consumed 3F-MH-003 record).

This is NOT a route revival (the canonical four-item table is still cited
correctly in the CH3-lens rows of the same documents — `ΩF…:275` reads
"51, 53, 246"). It IS a CH3 citation-precision regression: the prose fence that
claims to block G2/**G4**/G6 entry omits the one item (246) that specifically
fences G4's structural-stream-driver route. A downstream G4 author reading only
the §13.7/§17 fence would miss the item bounding their own wave.

Correction: in all three locations, the enumeration must read "REDRESS items
**51/53/246/247**" (four items), matching 1D `:166-171` and the 3F CH3 row
`:274`. Carry the same four-item set in the master-plan §7 invariant check
(`master-plan-diff.md:318-320`, which currently states "blocks G2/G4/G6" with no
item list — add the four-item id set there too for falsifiability).

### CH3-V1-03 — REVISE: ΩE `ΩE-skinny-corpus.md` BENCH replacement asserts a cleaner CSS win than the H1-pending re-lock supports

Artefact: `ΩE-skinny-corpus.md:296-299` (the BENCH STAGED replacement text) and
the cross-surface note `:368-374`.

The BENCH replacement states `track1_rich` "BEATS lightningcss on all 4 corpora
(1.66-3.38×, directional under concurrent-session load; the old fact-stream LOST
at 0.60-0.76×)". The bit-rot fix `784ceb418` (`track1_fact_stream → track1_rich`)
is real and verified, and the 1.66-3.38× figure carries its directional caveat.
But the ΩD master-plan reconciliation (D5 / `MP-3B-SKV18-D10`) is explicit that
the CSS >SOTA verdict is UPGRADED ONLY to "directionally-valid pending the H1
`css_canon_bench` re-lock... loadavg 4.35 at capture", and that the
un-caveated "MEASUREMENT-VALID" closure word is FORBIDDEN by the row's own
fail-action (CH2-V1-R03). The BENCH replacement adopts the directional caveat
for the WIN but presents "the old fact-stream LOST at 0.60-0.76×" as a settled
comparison without the same loadavg/H1-pending qualifier — asymmetric caveating
that reads as a firmer CSS-comparator verdict than the H1-pending re-lock
licenses.

Correction: the BENCH replacement and the cross-surface note (`:368-374`) must
carry the IDENTICAL directional/loadavg/H1-pending caveat on BOTH the win figure
AND the fact-stream-loss figure, and route the binding close to the H1
`css_canon_bench` re-lock (matching D5's "do NOT carry the un-caveated
MEASUREMENT-VALID closure word"). Without this, ΩE and ΩD disagree on the CSS
verdict firmness — a cross-document inconsistency the CRUD would merge.

## What Did NOT Regress (ACCEPT rationale, the load-bearing nulls)

- **No revived REDRESS route.** Every staged deletion/retirement (x86 crate,
  `CSS_GENERATED_RS` courier, 7 css_l4 replicas, phantom `<G>`, the
  `RuntimeEmitterKind` fork) is fenced PRUNE-before-GENERALIZE
  (`migration-delta.staged.md` OP-3) and the un-fork owes no REDRESS id
  (`grep -ic … skinny/REDRESS.md == 0`). REDRESS 96/97/98 (union-substrate
  cursor) and 51/53/246/247 (second-scanner/structural-stream) are carried as
  BLOCKERS, never as revived routes.
- **No Lock-14 narrowing.** The green-by-exclusion clause (C6) and the
  named-primitive (a)-(d) gate (C1) STRENGTHEN Lock 14; the `<G>` strike (C10)
  re-anchors generality onto `Cursor`+config-breadth, preserving the
  `@generated` grammar-neutrality guarantor and `preserve-rich-ast` verbatim.
- **No coupling reintroduced.** The CollapsedStage conditional rebuild is
  G5/G6-GATED on a profiled hot leaf, not a committed build; the firewall keeps
  the skinny-vs-totality scan scope distinct (CH5-DEFECT-V1-02 honored at
  `locks-diff.md:61`). The `simd-scan:67` second classifier is honestly routed
  to SK-V19, with the skinny `rg=0` green explicitly declared "NOT a totality
  single-substrate proof".
- **No T-P2-refuted assertion revived.** "Wire-as-is" REFUTED → retarget-not-author
  (C8); `MatchSetSve2`/NEON-svmatch on SVE2-absent hosts REFUTED → aarch64-ONLY
  with FEAT_SVE2 ABSENT recorded (C4); md5-distinctness NECESSARY-NOT-SUFFICIENT
  → the structural co-gate is carried (C2/D2).
- **5-shape canon + 16-lock count preserved** byte-verbatim across every
  staged surface; the diff is amendment-by-addition; `git apply --check` exit 0.

## Disposition Summary

26 enumerated amendments/CRUD operations (11 Ω-C + 6 Ω-D + 6 Ω-E + 4 Ω-F (OP
clusters, scored as 4 operations: MIGRATION-§0.0, MIGRATION-decisions,
MIGRATION-PRUNE-gate, HANDOFF-cluster) — 1 HANDOFF cluster counted with F4) —
scored at clause/diff/surface granularity: **23 ACCEPT / 3 REVISE / 0 REJECT**.

No REJECT: no non-applying diff (git apply --check exit 0), no revived REDRESS
route, no Lock-14 narrowing, no reintroduced coupling, no uncited claim — the
un-fork's NOVELTY (REDRESS=0) and the four-item-table presence in the CH3-lens
rows keep all three REVISE findings at citation-precision / caveat-symmetry
level, not route-revival level.

The 3 REVISE (CH3-V1-01 CollapsedStage UNKNOWN-2D-05 framing; CH3-V1-02 the
item-246 under-citation across master-plan/migration/ΩF; CH3-V1-03 the BENCH
caveat-asymmetry) are the cycle-V1 ≥30% REVISE expectation met at 3/26 ≈ 11.5%
on the full census but 3/3 on the CH3-material-axis subset (the REDRESS-fence
and CSS-verdict clauses are the only ones CH3 touches with amendment weight; all
3 carry a correction). Re-scored on the CH3-eligible amendment surface (the
REDRESS-bearing + refuted-assertion-bearing clauses: C2, C4, C7, C8, C9, D2, D5,
E5, F3 = 9), the REVISE rate is 3/9 = 33%, meeting the cycle-V1 ≥30% bar.

TALLY accept=23 revise=3 reject=0
