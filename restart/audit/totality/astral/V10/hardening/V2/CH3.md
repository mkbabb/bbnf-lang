# CH3 REGRESSION — Pass Omega V10 (astral) CHALLENGE Lens, Cycle V2

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

Date: 2026-06-01. HEAD `25297a7fc` (the staging point; the whole
`restart/audit/totality/astral/V10/` tree is UNTRACKED — staged-only, not
committed). Cycle V2 re-reviews the V1 verdict independently: it re-runs every
load-bearing spot-check at HEAD, confirms which V1 REVISE findings the staged
text now satisfies, and hunts for residue V1 did not catch.

## Load-Bearing Spot-Verifications (re-run at HEAD, V2)

| Check | Command / target | Result |
|---|---|---|
| Staged locks-diff applies | `awk … | git apply --check -` on `locks-diff.md` | **exit 0** (applies cleanly) |
| 16 numbered locks / no Lock 17 | `grep -cE '^[0-9]+\. \*\*'` / `grep -cE '^17\. '` | **16** / **0** — addendum adds no Lock 17 |
| 5 BackendShape variants | `lower/mod.rs:20-24`; `cost.rs:334 all_backend_shapes() -> [BackendShape; 5]` | 5 `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}`; no 6th |
| PLANNED co-gate symbols absent | `grep -rc runtime_target_rows_collapsed skinny/crates skinny/xtask`; `… bbnf_simd_single_mask_convention skinny/crates` | both **0** (written PLANNED, not live) — correct |
| Un-fork owes no REDRESS id | `grep -ic 'relocated.seam\|RuntimeEmitterKind\|un.fork' skinny/REDRESS.md` | **0** — un-fork is SK-V18-NOVEL, NOT a refuted route |
| Un-fork UNBUILT at HEAD (no overclaim) | `runtime_generator.rs:1,:17,:25` | `RuntimeEmitterKind{CompiledLowering,RequestFacts}` STILL live + branched — locks-diff C2 correctly says "UNBUILT at HEAD", DELETE is a PLANNED-at-G3 gate |
| CollapsedStage REDRESS 96/97/98 | `skinny/REDRESS.md:2908`(item 98 retires `G-W3-UNION-SUBSTRATE`), finding `:2928-2933` | RESOLVES verbatim: M5-Max scalar-cheaper-than-SIMD-cursor finding present at `:2928-2933` |
| Four-item pre-block (1D source of truth) | `1D-skinny-lessons.md:166-171` | header `:166` + 4 rows `:168-171` = **246/247/51/53**; item 246 (`:168`) bounds **G4** (`REDRESS.md:6184-6219`) |
| §H wave H.W2/H.W2.5 + `simd/structural_scan` | `MASTER-PLAN.md:140,:158` | resolves; `simd/structural_scan` row owner = H.W2/H.W5 |
| `simd-scan` 2nd classifier re-export | `crates/simd-scan/src/lib.rs:67` | `pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut}` — present, SK-V19 carry |
| 3F CH3-lens row four-item set | `3F-migration-handoff.md:274` | "REDRESS items 51, 53, 246, 247" — four items present |
| C8 retarget kernel live | `runtime_simd.rs:169 find_css_significant` consumes `byte_class_from_eq_set_64` at `:200`; guard `neon_significant_skip_matches_scalar` at `lib.rs:562` | live; FIXED set `b"'\"/()[]{}"` (`lib.rs:566`) |
| C9 slot real (not phantom) | `lower/collapsed_stage.rs:16`; `passes/lib.rs:658` `collapsed_stage_author_declared` gate | live shape slot |
| C10 e-graph regression-guard live | `passes/src/backend_egraph.rs:75,:191,:193 NormalizeDirectSinkCost` | live asserted rewrite — re-opens V2 scaffold finding if zero-rule |
| C2 firewall scope targets exist | skinny `grammars/css_l4_*` (7 dirs) + `runtime_simd.rs`; totality `crates/core/src/runtime/css_l4/` DEFER seam exists | scope split is real; totality side DEFERRED to SK-V19 (3B:177,:197 "DEFER, do NOT bolt") |

## What V1's Three REVISE Findings Became (all ADDRESSED in the staged text)

Cycle V2's first duty is to confirm whether the staged text already absorbed the
V1 REVISE set. It did — all three are now satisfied, so each flips to ACCEPT at
the V2 census:

- **V1 CH3-V1-02 (item 246 dropped) → ADDRESSED.** Every enumeration the V1
  finding flagged now reads the full four items: `master-plan-diff.md:201`,`:335`
  (the §7 invariant block V1 asked for is present and reads "REDRESS items
  51/53/246/247 — 1D:166-171; item 246 bounds G4's structural-stream-driver
  route"), `migration-delta.staged.md:116`, `ΩF-migration-handoff.md:163`. No
  surviving "51/53/247" three-item fence remains in any staged file.
- **V1 CH3-V1-03 (BENCH caveat asymmetry) → ADDRESSED.** The ΩE BENCH STAGED
  replacement (`ΩE-skinny-corpus.md:298-303`) now caveats BOTH figures
  symmetrically: "BOTH the track1_rich win (1.66-3.38×) AND the old fact-stream
  loss (0.60-0.76×) are DIRECTIONAL figures captured under concurrent-session
  load (loadavg 4.35 at capture); neither is a settled MEASUREMENT-VALID
  comparison until the H1 css_canon_bench re-lock". The cross-surface note
  (`:373-377`) carries the same dual caveat. The CH2-V1-R03 forbidden
  "MEASUREMENT-VALID" word is honored.
- **V1 CH3-V1-01 (CollapsedStage UNKNOWN-2D-05 framing) → ADDRESSED-IN-SUBSTANCE,
  but its line-number residue is the new V2 finding.** The C9 clause now
  explicitly reads "DISCHARGES UNKNOWN-2D-05 (…); the SK-V17 'no aarch64
  CollapsedStage admission without a 2E source-backed strategy' bar is MET, not
  relaxed — the x86-mechanically-refused row is not quietly re-opened." The
  framing V1 demanded is present. See CH3-V2-01 for the line-number defect the
  fix carried forward verbatim from V1's own correction text.

## Enumerated Staged Amendments / CRUD Operations Under CH3 (V2 disposition)

### Ω-C — `locks-diff.md` (11 addendum clauses)

| # | Clause | CH3-V2 disposition |
|---|---|---|
| C1 | Named-primitive (a)-(d) gate | **ACCEPT** — SPEC-only → lock; no refuted route; T-P2 literature-validated |
| C2 | Relocated-seam firewall + un-fork | **ACCEPT** — REDRESS negative-witness 0; "UNBUILT at HEAD" stated (no present-tense overclaim); CSS side-channel scope correctly split (skinny scan / totality DEFER) |
| C3/neutrality | Neutrality-proof (`css_balanced_component_scan` forced demotion) | **ACCEPT** — consumes 2C s6/C4; no revival; `scoped non-JSON witness` labelling honored |
| C4 | aarch64-ONLY (x86 PRUNE target) | **ACCEPT** — sharpens SK-V17 aarch64-PRIMARY; FEAT_SVE2 ABSENT carried; svmatch REFUTED-on-this-host preserved, not revived (2E:244) |
| C5 | Verbatim-blob-courier prohibition | **ACCEPT** — Lock-6 co-bind; no refuted route |
| C6 | Green-by-exclusion precondition (P4-before-G2/G3) | **ACCEPT** — `FORBIDDEN_GENERIC_TOKENS` byte-identical across 3A-D11/3C/3B-P4/3D-D04 |
| C7 | Single-SIMD-substrate + one-movemask | **ACCEPT** — skinny-scoped; `simd-scan:67` carry honestly routed to SK-V19, NOT claimed as totality proof |
| C8 | Retarget-not-author | **ACCEPT** — honors the T-P2 "wire-as-is" REFUTATION; live kernel `find_css_significant`→`byte_class_from_eq_set_64` verified |
| C9 | CollapsedStage shape-slot | **REVISE** (CH3-V2-01) — route NOT revived, REDRESS 96/97/98 cite correct, UNKNOWN-2D-05 discharge framing present, but the `ARCHITECTURE.md:1282` line cite is wrong |
| C10 | Cursor-generality re-anchor (`<G>` strike) | **ACCEPT** — deletes phantom axis; `NormalizeDirectSinkCost` regression-guard live — a guard, not a revival |
| C11 | Pattern-H re-census (67→71) | **ACCEPT** — +4 = tape-fold roster trace; 71 verified at HEAD; unattributable +N opens O(N) scan |

### Ω-D — `master-plan-diff.md` (6 staged diffs)

| # | Diff | CH3-V2 disposition |
|---|---|---|
| D1 | §13.6 SK-V18 tape-fold → SK-V19 totality-fold re-key | **ACCEPT** — identity pivot; F1-F9 verbatim; refuted-census 0 (3B:14); no wave refuted |
| D2 | NEW §13.7 12-wave block + CH3-V1-R2 retime | **ACCEPT** — the §13.7 retime line (`:197-203`) now reads the full four-item "51/53/246/247" with "item 246 = the W11T parse-only structural-STREAM driver reject that bounds G4"; V1 CH3-V1-02 satisfied |
| D3 | §25 Implementation Order reconciliation | **ACCEPT** — monotonic skinny→totality restored; no route revived |
| D4 | §24 Carry Ledger re-key + SK-V19 tee-up rows | **ACCEPT** — the 3 totality leaks DEFERRED to SK-V19 (3B:197 "DEFER, do NOT bolt"), not bolted into SK-V18 |
| D5 | §5 F.W5 / §13.5 CSS verdict reconciliation | **ACCEPT** — CSS verdict UPGRADE carries the directional/H1-pending caveat; refuses un-caveated MEASUREMENT-VALID |
| D6 | §13 H-row + Lock-10 cross-ref alignment | **ACCEPT** — label-only; 5-shape canon row UNCHANGED |

### Ω-E — `ΩE-skinny-corpus` (6 CRUD-5 surface updates)

| # | Surface | CH3-V2 disposition |
|---|---|---|
| E1 | INDEX (SK-V15 W0-W11 → SK-V18 W-PRUNE→G1..G6→PROVE→H1) | **ACCEPT** — supersession, not route-revival; SK-V14/V15 kept historical, do not dispatch |
| E2 | WORKSPACE (telemetry → SPEC §3 gate schema) | **ACCEPT** — no refuted construct re-armed; FNV stays quarantine/telemetry |
| E3 | HARDENING (seven-lens re-key + §6 (a)-(d) lens) | **ACCEPT** — CH3 lens text explicitly adds delete-before-rebuild cycle detection |
| E4 | COMPILER (DELETE `RuntimeEmitterKind`, dispatch on BackendShape) | **ACCEPT** — un-fork is NOVEL (REDRESS=0); deletion is PLANNED-at-G3, matching the live "still branches" state |
| E5 | BENCH (CSS comparator inversion + track1_rich) | **ACCEPT** — V1 CH3-V1-03 satisfied: both win AND loss figures now carry the IDENTICAL directional/loadavg/H1-pending caveat (`ΩE:298-303`,`:373-377`) |
| E6 | SUBSTRATE (limited authority/status flip) | **ACCEPT** — explicit "NO substrate data-structure change"; 5-shape canon + Lock 1 union preserved |

### Ω-F — `migration-delta.staged.md` + `handoff-delta.staged.md`

| # | Operation | CH3-V2 disposition |
|---|---|---|
| F1 | MIGRATION OP-1 §0.0 SK-V18 receiver (renumber-down) | **ACCEPT** — historical receivers preserved as provenance |
| F2 | MIGRATION OP-2 rename/abrogate/refactor rows | **ACCEPT** — `css_types.rs` routed to SK-V19, not silently dropped |
| F3 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate (§17/§19) | **ACCEPT** — `migration-delta.staged.md:116` now reads the full four-item "51/53/246/247 (… item 246 = the W11T parse-only structural-STREAM driver reject that bounds G4)"; V1 CH3-V1-02 satisfied |
| F4 | HANDOFF OP-1..OP-5 (override + blocker matrix + directive) | **ACCEPT** — blocker matrix maps phantom `<G>` AND CSS-Value-API to G4 with measurable gates; no route revived; ΩF CH3 row (`:278`) carries the four-item set |

## CH3-V2 Finding (the REVISE)

### CH3-V2-01 — REVISE: ΩC `locks-diff.md` C9 CollapsedStage clause mis-cites `ARCHITECTURE.md:1282` for the UNKNOWN-2D-05 discharge

Artefact: `restart/audit/totality/astral/V10/locks-diff.md:75` (the C9 CollapsedStage
shape-slot clause) and the mirrored prose in `ΩC-locks-amendments.md` /
`ΩD`/`ΩF` carriers.

The C9 clause discharges the SK-V17 aarch64-admission bar by writing:

> "The aarch64 TBL-classify scalar-oracle grounding (2E:73,`:81`) DISCHARGES
> UNKNOWN-2D-05 (`restart/ARCHITECTURE.md:1282`,`:1206`, which records
> CollapsedStage as `target.arch == x86 + target.avx512bw`-bound with the
> aarch64 candidate 'requires a 2E source-backed aarch64 strategy before any
> aarch64 admission')"

The DISCHARGE itself is sound and is NOT a route revival: item 98 retiring
`G-W3-UNION-SUBSTRATE` is the union-substrate cursor, a categorically different
object from the staged-FSM shape slot; the x86-mechanically-refused row is
fenced (`diagnostic-only / author-declared`, G5/G6-GATED on a profiled hot leaf);
the 2E:73/:81 TBL-classify grounding is real. This is anti-regression discipline.

The defect is a verifiable mis-citation that the CRUD would merge into the live
LOCKS surface. At HEAD, `restart/ARCHITECTURE.md:1282` is the WIRE-posture text
("…rather than wiring the existing engine."), NOT the UNKNOWN-2D-05 row. The
canonical UNKNOWN-2D-05 record is at:
- `ARCHITECTURE.md:1206` — the CollapsedStage ledger ROW, which carries the
  verbatim quote the clause attributes ("`target.arch == x86` + `target.avx512bw`
  … aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64
  strategy before any aarch64 admission)"). This cite is CORRECT.
- `ARCHITECTURE.md:1289` — the U3 directive ("aarch64 CollapsedStage =
  UNKNOWN-2D-05, no admission without a 2E source-backed strategy").

So `:1206` is right; `:1282` is the wrong companion line. The bad `:1282` is not
a fresh authoring error — it was carried VERBATIM from V1's own CH3-V1-01
correction text (`hardening/V1/CH3.md:110`,`:121` both cite `:1282`), so the
hardening loop propagated the defect from the verdict INTO the staged clause. A
reader following the cite to verify the UNKNOWN-2D-05 discharge would land on
WIRE-posture prose and could not confirm the bar is being satisfied — the exact
falsifiability failure the discharge framing exists to prevent.

Correction: in the C9 clause replace `restart/ARCHITECTURE.md:1282`,`:1206` with
`restart/ARCHITECTURE.md:1206`,`:1289` (the ledger row + the U3 directive that
together carry the UNKNOWN-2D-05 record and the attributed quote). Propagate the
same line-number fix to the `hardening/V1/CH3.md` correction text so a future
cycle does not re-inject `:1282`. This is a citation-precision REVISE; the
discharge gate itself is correct.

## Secondary Observation (sub-REVISE weight, carried not scored as REJECT)

The C2 firewall clause asserts the CSS-typed side-channel gate "via
`css_provider_source == generated`" without the PLANNED/live status label it
gives its two sibling co-gate symbols. `runtime_target_rows_collapsed` and
`bbnf_simd_single_mask_convention` are each explicitly tagged "not yet a live
symbol / PLANNED SK-V18 gate" and verified `rg=0` in `skinny/crates`. By
contrast `css_provider_source` is live ONLY as a `bbnf-bench/src/report.rs`
report field (6 occurrences), NOT a codegen/firewall gate symbol, and the clause
labels neither. This is a status-label asymmetry the same clause's own
discipline exposes; it does not revive a route, narrow a lock, or introduce a
coupling, so it is folded into CH3-V2-01's correction as a one-line "mark
`css_provider_source` as a PLANNED firewall predicate, distinct from the live
bench-report field" addendum rather than scored as an independent REVISE.

## What Did NOT Regress (ACCEPT rationale, the load-bearing nulls)

- **No revived REDRESS route.** Every staged deletion/retirement (x86 crate,
  `CSS_GENERATED_RS` courier, css_l4 replicas, phantom `<G>`, the
  `RuntimeEmitterKind` fork) is fenced PRUNE-before-GENERALIZE
  (`migration-delta.staged.md` OP-3) and the un-fork owes no REDRESS id
  (`grep -ic … skinny/REDRESS.md == 0`). REDRESS 96/97/98 (union-substrate
  cursor) and 51/53/246/247 (second-scanner/structural-stream/parser-local
  cursor) are carried as BLOCKERS, never as revived routes; the four-item set is
  now correct in EVERY staged fence (V1's three-item under-citation is gone).
- **No Lock-14 narrowing.** The green-by-exclusion clause (C6) and the
  named-primitive (a)-(d) gate (C1) STRENGTHEN Lock 14; the `<G>` strike (C10)
  re-anchors generality onto `Cursor`+config-breadth, preserving the `@generated`
  grammar-neutrality guarantor and `preserve-rich-ast` verbatim.
- **No coupling reintroduced.** The CollapsedStage conditional rebuild is
  G5/G6-GATED on a profiled hot leaf, not a committed build; the firewall keeps
  the skinny-vs-totality scan scope distinct (skinny `css_l4_*`+`runtime_simd.rs`
  scanned; totality `crates/core/src/runtime/css_l4/` DEFERRED to SK-V19 under
  3B:177/:197 "DEFER, do NOT bolt"). The `simd-scan:67` second classifier is
  honestly routed to SK-V19, with the skinny `rg=0` green explicitly declared
  "NOT a totality single-substrate proof".
- **No T-P2-refuted assertion revived.** "Wire-as-is" REFUTED →
  retarget-not-author (C8, live kernel verified); `MatchSetSve2`/NEON-svmatch on
  SVE2-absent hosts REFUTED → aarch64-ONLY with FEAT_SVE2 ABSENT recorded (C4);
  md5-distinctness NECESSARY-NOT-SUFFICIENT → the structural co-gate is carried
  (C2/D2).
- **5-shape canon + 16-lock count preserved** byte-verbatim across every staged
  surface; the diff is amendment-by-addition; `git apply --check` exit 0; no
  Lock 17.

## Disposition Summary

27 enumerated amendments/CRUD operations (11 Ω-C + 6 Ω-D + 6 Ω-E + 4 Ω-F),
scored at clause/diff/surface granularity: **26 ACCEPT / 1 REVISE / 0 REJECT**.

No REJECT: no non-applying diff (`git apply --check` exit 0), no revived REDRESS
route, no Lock-14 narrowing, no reintroduced coupling, no uncited claim — the
un-fork's NOVELTY (REDRESS=0), the now-correct four-item table in every fence,
and the symmetric CSS caveat keep the lone finding at citation-precision level.

V2 differs from V1 in census because the staged text has HARDENED between cycles:
V1's three REVISE findings (CH3-V1-01 framing, CH3-V1-02 item-246 under-citation,
CH3-V1-03 BENCH caveat asymmetry) are all ADDRESSED in the current staged surfaces
and re-verified ACCEPT at HEAD. The single surviving REVISE (CH3-V2-01) is the
line-number residue the V1 CH3-V1-01 correction itself carried into the C9 clause
(`ARCHITECTURE.md:1282` for UNKNOWN-2D-05, which is WIRE-posture text — should be
`:1206`,`:1289`).

On the CH3-eligible amendment surface (the REDRESS-fence + refuted-assertion-
bearing clauses CH3 touches with weight: C2, C4, C7, C8, C9, C10, D2, D5, E5, F3
= 10), the V2 REVISE rate is 1/10 = 10%, BELOW the ≥30% cycle bar. This is the
correct and honest count: the bar was MET at V1 (3/9 = 33%) and the V1 REVISEs
were applied, so the surface is now near-converged on the CH3 axis. Padding to
≥30% would require fabricating findings the evidence does not support; the lens
declines to do so. The single genuine REVISE plus the carried secondary
observation are the full residue; the recommended disposition is APPLY CH3-V2-01,
then the CH3 axis is converged-clean for G-Omega.

TALLY accept=26 revise=1 reject=0
