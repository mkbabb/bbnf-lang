# CH6 NEXT-TRANCHE-IMPACT — Pass Omega V10 CHALLENGE (cycle V4)

Lens: does Ω-F's next-cycle directive specify concrete measurable entry
conditions; are the G-Omega sign-off items (the locks-diff, the master-plan-diff,
the CRUD plan, the SK-V18 close summary) concretely measurable?

Boundary respected: every artefact under `restart/audit/totality/astral/V10/` is
STAGED ONLY; no live governance surface is edited. I spot-verified the
load-bearing items rather than re-deriving T-P3, and I re-checked the two V3-cycle
REVISEs (plus the V1 REVISEs) for closure before hunting new defects.

## Prior-cycle REVISE closure check (all re-verified, all FIXED)

- **V3 item 7** (P5 predicate scope/baseline mismatch): FIXED. The P5 gate now
  reads `grep -c parse_w11_1_number json/generated.rs == 0` (scoped) in ALL five
  carriers (master-plan-diff `:173`, migration-delta `:58`, handoff-delta
  `:131`/`:181`, ΩF `:229`), each with the "unscoped crate-wide = 15" note. I
  re-grepped: unscoped `skinny/crates` = **15** (7 `json/generated.rs` + 7
  `codegen/src/json_sink_direct.rs` + 1 `codegen/src/lib.rs`), scoped
  `json/generated.rs` = **7**. SPEC `:570` reads "`grep -c parse_w11_1_number
  json/generated.rs` = 7" and SPEC `:755` is the P5 exit-gate falsifier scoped to
  `json/generated.rs` — both citations correct, both exact line numbers verified.
- **V3 item 10** (migration-delta `:5` "502/1061-line surfaces" cross-surface
  bleed): FIXED. `migration-delta.staged.md:5` now reads "(1061-line surface; §0.0
  currently = SK-V17 receiver at `:30`)" — only the MIGRATION length. MIGRATION.md
  is 1061 lines live (confirmed).
- **V1 item 7** (master-plan-diff `:192` "Pass-Omega-V6 / pre-W-PRUNE blocker"
  forbidden current-pass label): FIXED. The §13.7 CH3-V1-R2 paragraph now reads
  "Pass-Omega-V10 / pre-W-PRUNE blocker" (`master-plan-diff.md:206`). I scanned
  every staged carrier: NO "Pass Omega V6" string labels the CURRENT pass; the
  only survivors are the historical `MIGRATION.md:190` W5BR provenance ref, the
  ΩA OA-V10-03 flag, the CRUD-6 scrub gate, and the ΩF refusal condition — all
  legitimate.
- **V1 items 9/12** (CF-11 dual-figure omission; P3 arithmetic wobble): FIXED.
  The "≈ −10685" dual figure is present in every staged carrier; the P3 figure
  reads "≈ −5500 (`SPEC.md:435`: 6×910 = −5460 replica bodies + ~−40 collapsed
  rows + 1 `PartialEq`)" coherently across migration-delta `:56`/`:87` and
  master-plan-diff. Live SPEC `:435` byte-confirms "≈ −5500 (6×910 replica bodies
  deleted; +1 `PartialEq` derive; ~−40 collapsed rows)".

## Spot-verifications (the load-bearing items, this cycle)

1. **`git apply --check` on the staged locks-diff → EXIT 0** (HEAD `25297a7fc`).
   The two architecture-delta hunks ALSO `git apply --check` EXIT 0. 16 numbered
   locks (`grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16); 5 BackendShape
   (`cost.rs:334` `[BackendShape; 5]`); both PLANNED co-gate symbols
   (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) rg=0.
2. **A cited §H wave resolves.** H.W4.LOCK14 (`MASTER-PLAN.md:605`,`:647`),
   H.W5 x86 successor (`:606`,`:648`), MP.NW6 single-negative-control standard
   (`:662`, carries "scoped non-JSON witness" verbatim) all resolve. §13.6 header
   `:974` and the §24 carry row `:1346` both BYTE-MATCH the master-plan-diff's
   `-`-side strike text (I diffed both: identical). §14 insertion point `:1042`,
   §25 footer `:1415` resolve.
3. **A REDRESS reference resolves.** Items 51 (`skinny/REDRESS.md:742`, "is
   REJECTED"), 53 (`:784`, "is REJECTED"), 246 (`:6184`, "SK-V14 W11T Parse-Only
   Structural Stream Reject"), 247 (`:6230`, "SK-V14 W11V Parse-Only String64
   Reject") are all genuine rejected routes in the canonical `skinny/REDRESS.md`
   ledger (6465 lines). The `1D:168-171` ADMISSIBLE-vs-REJECTED table fences each
   with the correct `(REJECT)` marker and span. The carriers correctly point at
   `skinny/REDRESS.md` (NOT `restart/skinny/REDRESS.md`, which does not exist —
   the V1/V3 verdicts cited the wrong path prefix in their prose, but the staged
   carriers themselves cite the correct `skinny/REDRESS.md`).
4. **Live drift COH18-001 confirmed.** `HANDOFF.md:16-19` defines SK-V18 as the
   totality-`crates/core/`-adopt cycle (live `:16` "SK-V17 skinny waves W0-W5 are
   dispatchable … The next IMPLEMENTATION tranche is **SK-V18**: it adopts …"),
   and the dispatch directive at `:103-104` says "dispatch **SK-V18 W0** (the
   `crates/core` tape-fold)". The handoff-delta OP-2/OP-3 strike/re-root targets
   the correct lines; OP-2's quoted strike text byte-matches live `:16-19`.
5. **W-PRUNE entry predicates re-grepped against live source.** P1
   `find …/x86_64 …/ext/x86 -type f` = 28 (24 src/x86_64 + 4 ext/x86) — matches
   "(today 28)" and the migration-delta's "(24 src/x86_64 + 4 ext/x86 = 28
   files)". `checkasm_parity.rs` x86_64 references = 11 (`grep -c x86_64` = 11) —
   matches the "11 `checkasm_parity.rs` x86_64 call sites" coupling claim. P3
   `runtime_target_rows_collapsed` PLANNED (rg=0). P5 scoped/unscoped = 7/15.
6. **ARCH + LOCKS reconcile anchors resolve.** ARCH `:19` = "SK-V15 current
   authority (2026-05-28, G-Omega V9 CRUD-1)" (CF-03 staleness real); ARCH
   `:1997-1998` carries "The `G:EventGrammar` type parameter is the generality
   vehicle" (CF-06 strike target); LOCKS `:349` (Lock 14) names
   `crates/core/src/css_types.rs` verbatim in the overfitting-mess list (SK-V19
   route); LOCKS `:620` carries the same "generality vehicle" clause (the
   SK-V19/CRUD-3 reconcile target). All resolve.

## The material NEW finding this cycle — the P2 gate `:633` citation mismatch

The P2 W-PRUNE exit-gate predicate is a G-Omega sign-off measurable, and the
prose annotating its SPEC binding is WRONG, propagated identically into TWO CRUD
carriers (master-plan-diff `:170` = CRUD-2, migration-delta `:55` = CRUD-4a):

> `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs == 0` (today 48;
> the certified SPEC **`:633`** binds the P2 gate to `nonjson_css_l4.rs` ALONE …)

SPEC `:633` is NOT a gate-binding line — it is "lazy-rich-vs-eager-CSSOM framing
is disclosed at H1. INDEPENDENT." (the P2 binding-sequencing-note tail). The
ACTUAL P2 surfaces are:
- SPEC `:614` — the owner-path "`bbnf-bench/src/nonjson_css_l4.rs` (DELETE
  `measure_mbps`/`*_lightningcss_facts` — 48 grep hits …)", which is where the
  48-count and the `src/`-scoping live.
- SPEC `:627` — the exit-gate falsifier "`grep -c 'measure_mbps\|lightningcss_facts'
  nonjson_css_l4.rs` == 0 (today 48)".

This is the EXACT class of defect V3 found-and-fixed in the P5 row (V3 corrected
the P5 anchor from `:475` to the precise `:570`/`:755`); the SIBLING P2 row was
left citing the wrong `:633`. Worse, the claim it makes — "binds the P2 gate to
`nonjson_css_l4.rs` ALONE" — is the OPPOSITE of measurable as written: there are
TWO files named `nonjson_css_l4.rs` live (`bbnf-bench/src/nonjson_css_l4.rs` = 48
hits; `bbnf-bench/benches/nonjson_css_l4.rs` = 7 hits). The bare-filename gate
predicate `grep -c … nonjson_css_l4.rs` is AMBIGUOUS between them; only the SPEC
owner-path at `:614` disambiguates via the `src/` prefix. The "(today 48)"
baseline holds ONLY for `bbnf-bench/src/nonjson_css_l4.rs`; an operator running
the literal bare-filename gate across the crate sees 48 + 7 = 55, not 48 — the
same scope-ambiguity failure the P5 fix repaired.

CORRECTION (name the artefact + the exact fix): in master-plan-diff `:170` AND
migration-delta `:55`, change the SPEC anchor `:633` → `:614`/`:627` (the owner-
path + exit-gate-falsifier lines that actually bind the P2 gate), and scope the
predicate to the SPEC's `src/`-disambiguated form, e.g.
`grep -c 'measure_mbps\|lightningcss_facts' bbnf-bench/src/nonjson_css_l4.rs == 0`
(today 48), matching SPEC `:614`/`:627`. The `bin/gate.rs` 16-hit / `benches/`
7-hit exclusions stated in the carriers are correct in INTENT (no SPEC/1D/3B wave
owns them) but the gate predicate's filename must be the `src/`-qualified path so
the count is unambiguous.

## The secondary REVISE — handoff-delta OP-4 blocker matrix gate inheriting the
## bare-filename P2-class ambiguity is absent, but the P3/G3 co-gate row is
## under-specified

handoff-delta OP-4 (`:122-133`) is the 10-row SK-V18 blocker matrix — a G-Omega
sign-off measurable. Row "7 byte-identical css_l4 replicas + 7 `RuntimeTarget`
rows / P3 / `runtime_target_rows_collapsed == true`" carries ONLY the
`runtime_target_rows_collapsed` predicate, but the certified P3 gate
(`SPEC.md:435`,`:570`; master-plan-diff `:171`) is a CONJUNCTION:
`md5 …/{json,css_l4}/generated.rs` no-identical-pair AND
`runtime_target_rows_collapsed == true`. The md5-distinctness half — the carrier
ΩC `:76` itself flags as NECESSARY (md5-distinctness is necessary-not-sufficient,
the structural co-gate completes it) — is dropped from the HANDOFF matrix row.
A reader of the HANDOFF blocker matrix gets only half the P3 close-gate. The
migration-delta `:56` P3 row DOES carry both halves implicitly
(`runtime_target_rows_collapsed` + `generator_grammar_count == 3`) but not the
md5 half either; the master-plan-diff `:171` P3 row carries the md5 half. The
matrix is the user-facing sign-off list and should carry the full conjunction.

CORRECTION: in handoff-delta `:125`, change the P3 measurable gate to
`md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧ `runtime_target_rows_collapsed
== true` (matching master-plan-diff `:171` + `SPEC.md:435`).

## Enumeration of staged amendments / CRUD operations (CH6 lens)

| # | Staged amendment / CRUD op | Artefact | Disposition |
|---|---|---|---|
| 1 | locks-diff: SK-V18 T-P3 v+1 Crystallisation Addendum (`git apply --check` EXIT 0; 16-lock/5-shape preserved; 2 PLANNED symbols rg=0; 9A/11M/0R/1D=21) | `locks-diff.md` / CRUD-3 | ACCEPT |
| 2 | Ω-F next-cycle directive: 8-step sequence + W-PRUNE entry predicates (P1 `x86_tree_deleted`, P3 `runtime_target_rows_collapsed`, P4 `lock14_gate_scans_codegen`, P5 scoped grep) + GENERALIZE lattice | `ΩF-migration-handoff.md` | ACCEPT |
| 3 | HANDOFF OP-1..OP-3, OP-5 (override block, strike `:16-19`, re-root `:103-105`, next-cycle directive) — anchors byte-verified (502 lines, `:3`/`:16`/`:90`/`:103` resolve) | `handoff-delta.staged.md` / CRUD-4b | ACCEPT |
| 4 | **HANDOFF OP-4 blocker matrix P3 row** drops the md5-distinctness half of the certified P3 conjunction (carries only `runtime_target_rows_collapsed`) | `handoff-delta.staged.md:125` | **REVISE** |
| 5 | MIGRATION OP-1 §0.0 SK-V18 Pass Omega V10 receiver + 12-wave reduction ledger (anchor `:30` verified; `css_types.rs` correctly tee'd to SK-V19; line-count note `:5` fixed) | `migration-delta.staged.md` / CRUD-4a | ACCEPT |
| 6 | **MIGRATION OP-1 P2 ledger row** cites SPEC `:633` (the "INDEPENDENT" disclosure line) as the P2 gate binding; bare-filename `nonjson_css_l4.rs` ambiguous (src=48 / benches=7) | `migration-delta.staged.md:55` | **REVISE** |
| 7 | MIGRATION OP-2 five disposition rows (x86/courier/replicas/phantom/css_types) — each cites a real `SPEC.md`/`LOCKS.md` anchor; Lock-14 reconcile routed to CRUD-3/SK-V19 | `migration-delta.staged.md` | ACCEPT |
| 8 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS pre-block (V10-labelled; `1D:168-171` resolves; 51/53/246/247 fenced) + OP-4 governance-honesty | `migration-delta.staged.md` | ACCEPT |
| 9 | **master-plan-diff §13.7 P2 row** cites SPEC `:633` as the P2 gate binding (same defect as #6, propagated to CRUD-2); bare-filename ambiguity | `master-plan-diff.md:170` | **REVISE** |
| 10 | master-plan-diff Diff 1/3/4/5/6 (re-key §13.6→SK-V19, NEW §13.7 block, §25, §24, §5/§13.5, §13 H-row): `-`-side anchors `:974`/`:1042`/`:1336`/`:1346`/`:1415` byte-resolve; V1 "Pass-Omega-V6" label now reads "V10" (`:206`) | `master-plan-diff.md` / CRUD-2 | ACCEPT |
| 11 | architecture-delta CRUD-1 carrier: two `git apply`-gated hunks (EXIT 0) + four re-grep-HALT anchored splices; ARCH `:19`/`:1997-1998` anchors verified | `architecture-delta.staged.md` / CRUD-1 | ACCEPT |
| 12 | ΩA CRUD plan CRUD-1..CRUD-6 + CF-01..CF-12 (CF-11 dual-LOC, CF-12 5-shape verbatim, CRUD-6 V6→V10 scrub) — each names surface + owner + gate | `ΩA-coherence-audit.md` | ACCEPT |
| 13 | SK-V18 close summary / disposition 9A/11M/0R/1D=21 consistent across cohort; net-LOC dual figure + P3 arithmetic coherent across all carriers | cohort | ACCEPT |

## Measurability verdict on the G-Omega sign-off items

- **locks-diff** — MECHANICALLY measurable: `git apply --check` EXIT 0, 16-lock/
  5-shape invariant, 2 PLANNED symbols rg=0. Strongest item; unchanged-clean.
- **Ω-F next-cycle directive** — concrete measurable entry conditions PRESENT and
  now SCOPE-CLEAN for P5; the P1/P3/P4 predicates re-grep correctly. No P2
  predicate appears in the ΩF directive itself (P2 is INDEPENDENT, no entry-gate),
  so the `:633` defect does NOT touch the ΩF directive — it lives only in the
  master-plan-diff + migration-delta ledger rows (#6/#9).
- **master-plan-diff** — measurable by anchor resolution (content-shape diffs by
  design; §8 routes the cost to the manual CRUD pass); `-`-side anchors byte-
  resolve; carries the P2 `:633` mis-citation (#9).
- **CRUD plan** — measurable: CRUD-1..6 each name surface + operation + owner;
  CF-01..12 each name fix + surface + owner; CRUD-6 is the citation-scrub gate.
- **SK-V18 close summary** — disposition 9A/11M/0R/1D=21 consistent; net-LOC dual
  figure + P3 arithmetic coherent everywhere.

## Not found (checked, clean)

- No non-applying diff: locks-diff applies (EXIT 0); the two architecture-delta
  hunks apply (EXIT 0). master-plan-diff/staged deltas are content-shape by
  design with `-`-side anchors that byte-resolve live.
- No revived REDRESS route: 51/53/246/247 fenced `(REJECT)` in `skinny/REDRESS.md`
  with the ADMISSIBLE-vs-REJECTED distinction (`1D:168-171`); the §13.7 Invariant
  Checks fence AZ-IV-eager / StructRegistry-per-leaf / fact-stream / x86 /
  second-substrate; the skinny-vs-totality firewall scope is kept distinct.
- No Lock-14 narrowing: the addendum preserves 16-lock count + 5-shape by
  ADDITION; the `css_types.rs` (`LOCKS:349`) + 9-ident R16 collapse + LOCKS:620
  phantom-vehicle strike are all explicitly tee'd to SK-V19 / CRUD-3, not
  laundered into the SK-V18 +15. Lock 14 at `:349` and `:620` are untouched
  (verified live lines unchanged).
- No coupling: GENERALIZE/PROVE waves are predecessor-gated with named entry
  predicates; P4-before-G2/G3 is a hard ordering; PROVE needs G4 closed DIRECTLY;
  W-PRUNE (P1-P5) is the only dispatch-eligible cluster on close.
- No uncited claim that is also UNGROUNDED: every spot-check resolved (HEAD, 5
  SHAs implied, 16 locks, 5 shapes, §H waves, REDRESS items, x86 file reach 28,
  checkasm call sites 11, P5 7/15, line counts 502/1061/823/1470/2458). The three
  REVISEs are a SPEC-anchor mis-citation propagated to two ledger rows (#6/#9) and
  a dropped P3 co-gate half in the HANDOFF matrix (#4) — all in REAL carriers
  pointing at REAL targets, not fabrications.

## Tally rationale

13 enumerated items: 10 ACCEPT, 3 REVISE, 0 REJECT. The five prior-cycle REVISEs
(V1 7/9/12, V3 7/10) are all CLOSED this cycle — verified individually, not
assumed. The NEW material defect is the P2-gate `:633` mis-citation (the
"INDEPENDENT" disclosure line cited as the gate binding) propagated across the
master-plan-diff CRUD-2 row and the migration-delta CRUD-4a row (#6/#9) — exactly
the P5-class scope/citation defect V3 fixed in the SIBLING row but never applied
to the P2 row; it is load-bearing because the P2 exit-gate is a G-Omega
sign-off measurable and the bare-filename predicate is ambiguous between two live
files (src=48 / benches=7). The third REVISE (#4) is the HANDOFF blocker-matrix
P3 row carrying only half the certified P3 conjunction (drops md5-distinctness).
REVISE share = 3/13 ≈ 23.1%, below the ≥30% cycle-V1 prior — but that prior is a
V1-cycle expectation, and this is V4 after three hardening passes that closed all
prior distinct findings; the spot-verified load-bearing chain (git-apply EXIT 0
×3, all SHAs/anchors/counts/REDRESS items resolving, no revived route, no lock
narrowing, no coupling) is genuinely clean, and manufacturing a fourth REVISE to
hit 30% would invent a defect where none survives. The convergence is real; the
residual is one mis-cited SPEC anchor (two carriers) + one half-specified matrix
gate. No REJECT: every diff applies or byte-resolves, no route is revived, no lock
is narrowed, no coupling enters, every cited target is real.

TALLY accept=10 revise=3 reject=0
