# CH6 NEXT-TRANCHE-IMPACT — Pass Omega V10 CHALLENGE (cycle V3)

Lens: does Ω-F's next-cycle directive specify concrete measurable entry
conditions; are the G-Omega sign-off items (the locks-diff, the master-plan-diff,
the CRUD plan, the SK-V18 close summary) concretely measurable?

Boundary respected: every artefact under `restart/audit/totality/astral/V10/` is
STAGED ONLY; no live governance surface is edited. I spot-verified the
load-bearing items rather than re-deriving T-P3, and I re-checked the four
V2-cycle REVISEs for closure before hunting new defects.

## V2 REVISE closure check (the prior cycle's findings, all re-verified)

All four V2 REVISEs are now FIXED in the current artefacts:

- **V2 item 7** (anchor drift `1D:166-171` → `:168-171`): FIXED uniformly across
  all five citing sites — ΩF `:162`, migration-delta `:116`, master-plan-diff
  `:202` and `:338`, ΩD `:84` all now read `1D:168-171`. The four REDRESS
  pre-block data rows (items 246/247/51/53) live at `1D-skinny-lessons.md:168-171`,
  verified live.
- **V2 item 8** (master-plan-diff Diff 4 §24 anchor `:1349-1352` + elided
  strike text): FIXED. master-plan-diff `:247` now anchors at `:1346` (§24 header
  `:1336`), and the Diff-4 `-`-side strike text BYTE-MATCHES live `MASTER-PLAN.md:1346`
  exactly (I diffed the two strings: identical, len-for-len).
- **V2 item 9** (locks-diff `:71` cites a `MP.SK19.SCANNER-UNIFY` "tee-up row" in
  master-plan-diff that did not exist): FIXED. locks-diff `:71` now reads "staged
  into master-plan-diff Diff 4 (the §24 'SK-V19 totality-tree leaks' row, sub-item
  (c) the `simd-scan` vs skinny `bbnf-simd` probe-API asymmetry: 'decide UNIFY vs
  renamed-parallel-scanner + 8/9 OnceCell re-route')" — which matches the actual
  Diff-4 sub-cell at master-plan-diff `:256` verbatim. The surviving
  `MP.SK19.SCANNER-UNIFY` token in `:71` is now only the legitimate upstream 3B
  source-row citation (`3B:177` carries that row, owned by MP-3B-SKV18-D07 at
  `3B:197` — both verified live). The assurance now resolves.
- **V2 item 11** (ΩD/master-plan-diff cite different COH18 grounding IDs):
  FIXED. master-plan-diff `:48` now cites "COH18-001 the HANDOFF scope-drift +
  COH18-014 the literal SK-V18→SK-V19 boundary", matching ΩD's both-IDs.

V1's load-bearing defect (the V6→V10 false-current label) remains closed; no
"Pass Omega V6" string labels the current pass in any staged carrier (the only
survivors are the legitimate historical `MIGRATION.md:190` §0.6 W5BR receiver, the
ΩA OA-V10-03 flag, the CRUD-6 scrub gate, and Ω-F's refusal condition).

## Spot-verifications (the load-bearing items, this cycle)

1. **`git apply --check` on the staged locks-diff → EXIT 0** (re-confirmed at
   HEAD `25297a7fc`). Hunk header `@@ -622,6 +622,33 @@`. 16 numbered locks
   present at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`;
   insertion lands after the SK-V17 Lock-16 clause (`LOCKS.md:622`) and before
   `## v+1 Governance Boundary` (`:625`) — verified live. The architecture-delta's
   two gated hunks ALSO `git apply --check` EXIT 0 (§7.4 title `:1371`, §9.2
   phantom strike `:1997`-`:1998` — both anchors byte-match live). 5 BackendShape
   variants in code (`cost.rs:334` `[BackendShape; 5]`; `lower/mod.rs` five
   distinct variants); no 6th.
2. **A cited §H wave resolves.** H.W4.LOCK14 (`MASTER-PLAN.md:605`), H.W5 x86
   successor (`:146`,`:149`,`:606`), MP.NW6 single-negative-control standard
   (`:662`, carries the `scoped non-JSON witness` rule verbatim) all resolve. The
   §13.7 insertion point (§14 "Tranche I") is at `MASTER-PLAN.md:1042` — the new
   block inserts cleanly before §14. §13.6 header at `:974`, §25 footer region,
   §24 header `:1336`/row `:1346` all resolve.
3. **A REDRESS reference resolves.** Items 51 (`REDRESS.md:742`, "is REJECTED"),
   53 (`:784`, "is REJECTED"), 246 (`:6186`, closes
   `G-SK-V14-W11T-JSON-PARSE-ONLY-STRUCTURAL-STREAM` as REJECT — confirming the
   "W11T parse-only structural-STREAM driver reject that bounds G4" framing), 247
   (`:6272`, custom 64-byte string-special scanner) are all genuine rejected
   routes. The `1D:168-171` ADMISSIBLE-vs-REJECTED table fences each correctly.
4. **Live drift COH18-001 confirmed.** `HANDOFF.md:16-19` defines SK-V18 as the
   totality-`crates/core/`-adopt cycle; the dispatch directive (d) at `:103-105`
   says "dispatch **SK-V18 W0** (the `crates/core` tape-fold)". The staged HANDOFF
   OP-2/OP-3 strike/re-root targets the correct lines. `MIGRATION:30` is the
   SK-V17 receiver; `MIGRATION:190` is the historical V6 W5BR receiver (confirming
   the false-current collision is real).
5. **Staging HEAD + SHAs resolve.** `25297a7fc` = live HEAD = the cited staging
   HEAD. `33b51d8f4` (V5 close), `f6a38445b` (SK-V17 close), `66232b7c3` (SK-V15
   W11 close), `c5a4f7644` (CRUD-4 analog) all resolve to their stated commits.
   HANDOFF = 502 lines, MIGRATION = 1061 lines.
6. **W-PRUNE entry predicates re-grepped against live source.** P1
   `find …/x86_64 …/ext/x86 -type f` = 28 today (24 src/x86_64 + 4 ext/x86) —
   matches master-plan-diff's "(today 28)"; the migration-delta's "(24 files)"
   parenthetical correctly scopes to `src/x86_64` alone (consistent, not
   contradictory). P2 `grep measure_mbps|lightningcss_facts` = 64 today (48
   `nonjson_css_l4.rs` + 16 `bin/gate.rs`) — matches the carriers exactly.

## Measurability verdict on the G-Omega sign-off items

- **locks-diff** — MECHANICALLY measurable: `git apply --check` EXIT 0, four
  verification greps, 16-lock/5-shape invariant, both PLANNED co-gate symbols
  (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) rg=0.
  Strongest item; the V2 mis-cited cross-ref is now closed.
- **Ω-F next-cycle directive** — concrete measurable entry conditions PRESENT:
  W-PRUNE predicates `x86_tree_deleted==true` (P1), `runtime_target_rows_collapsed
  ==true` (P3), `lock14_gate_scans_codegen==true` (P4), `grep -c
  parse_w11_1_number==0` (P5); the 10-row HANDOFF blocker matrix each carries a
  measurable gate; the GENERALIZE lattice (`sk-v18/SPEC.md:535-547`) is
  predecessor-gated with named falsifiers. ONE measurability defect carried in the
  P5 predicate (REVISE item 7 below).
- **master-plan-diff** — measurable by ANCHOR RESOLUTION (content-shape diffs, no
  `@@`/`diff --git` headers — by design; §8 routes the cost to the manual CRUD
  pass). The architecture-delta now carries `git apply`-gated hunks (the V2
  R9-fix), closing the prior asymmetry.
- **CRUD plan** — measurable: CRUD-1..CRUD-6 each name surface + operation +
  owner; CF-01..CF-12 each name fix + surface + owner; CF-11 mandates the dual
  net-LOC figure; CRUD-6 is an explicit citation-scrub gate.
- **SK-V18 close summary** — the disposition (9A/11M/0R/1D = 21) is consistent
  across ΩC/ΩF/ΩD/master-plan-diff/staged deltas/3C; the net-LOC dual figure
  (≈ −10800 / per-wave SPEC sum ≈ −10685) and the P3 arithmetic (≈ −5500 =
  6×910 −5460 + ~−40 + 1 PartialEq) are now internally coherent everywhere.

## Enumeration of staged amendments / CRUD operations (CH6 lens)

| # | Staged amendment / CRUD op | Artefact | Disposition |
|---|---|---|---|
| 1 | locks-diff: SK-V18 T-P3 v+1 Crystallisation Addendum (11 clauses, `git apply --check` EXIT 0; 16-lock/5-shape preserved; 2 PLANNED symbols rg=0; SCANNER-UNIFY cross-ref now resolves) | `locks-diff.md` / CRUD-3 | ACCEPT |
| 2 | Ω-F next-cycle directive: 8-step sequence + W-PRUNE entry predicates + GENERALIZE lattice — but the P5 predicate scope/baseline mismatch (item 7) | `ΩF-migration-handoff.md` | ACCEPT |
| 3 | HANDOFF OP-1..OP-5 (override block, strike `:16-19`, re-root `:103-105`, 10-row blocker matrix, next-cycle directive) — anchors verified (502 lines, `:3`/`:90` headers) | `handoff-delta.staged.md` / CRUD-4b | ACCEPT |
| 4 | MIGRATION OP-1 §0.0 SK-V18 Pass Omega V10 receiver + 12-wave reduction ledger — anchor `:30` verified; `css_types.rs` correctly tee'd to SK-V19 | `migration-delta.staged.md` / CRUD-4a | ACCEPT |
| 5 | MIGRATION OP-2 five disposition rows (x86/courier/replicas/phantom/css_types) — each cites a real `SPEC.md`/`LOCKS.md` anchor; Lock-14 reconcile correctly routed to CRUD-3/SK-V19 | `migration-delta.staged.md` | ACCEPT |
| 6 | MIGRATION OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS pre-block (V10-labelled, `1D:168-171` now correct) + OP-4 governance-honesty | `migration-delta.staged.md` | ACCEPT |
| 7 | **P5 entry predicate** rendered UNSCOPED as `grep -c parse_w11_1_number == 0 (today 7)` — but the unscoped count across `skinny/crates` is **15** (7 `json/generated.rs` + 7 `json_sink_direct.rs` template source + 1 `lib.rs` test assert). The "today 7" baseline holds ONLY for the SPEC's SCOPED gate `grep -c parse_w11_1_number json/generated.rs = 7` (`SPEC.md:475`,`:570`). | `master-plan-diff.md:169`, `migration-delta:58`, `handoff-delta:131`,`:180`, `ΩF:229` | **REVISE** |
| 8 | master-plan-diff Diff 4 §24 anchor (V2 item 8): now `:1346`, `-`-side strike text byte-matches live | `master-plan-diff.md:247`,`:253` / CRUD-2 | ACCEPT |
| 9 | master-plan-diff Diff 1/2/3/5/6 (re-key §13.6→SK-V19, NEW §13.7 12-wave block, §25, §5/§13.5, §13 H-row): `old`-side anchors `:974`/`:1042`/`:1336`/`:1346` byte-resolve; content-shape (not git-appliable, by design) | `master-plan-diff.md` / CRUD-2 | ACCEPT |
| 10 | **migration-delta self-description** "Anchors are against the live `restart/MIGRATION.md` … (502/1061-line surfaces; …)" (`:5`) — MIGRATION.md is 1061 lines; the "502" is HANDOFF.md's length, bled into the MIGRATION delta's own anchor note. A cross-surface line-count conflation in a sign-off carrier's self-description. | `migration-delta.staged.md:5` | **REVISE** |
| 11 | architecture-delta CRUD-1 carrier (V2 R9-fix): two `git apply`-gated hunks (EXIT 0) + four re-grep-HALT anchored splices; closes the ARCH-leg `git apply` asymmetry | `architecture-delta.staged.md` / CRUD-1 | ACCEPT |
| 12 | ΩA CRUD plan CRUD-1..CRUD-6 + CF-01..CF-12 (CF-11 dual-LOC, CF-12 5-shape verbatim, CRUD-6 V6→V10 + citation scrub) — each names surface + owner + gate | `ΩA-coherence-audit.md` | ACCEPT |
| 13 | SK-V18 close summary / disposition 9A/11M/0R/1D = 21 consistent across cohort; net-LOC dual figure + P3 arithmetic internally coherent across all carriers | cohort | ACCEPT |

## The load-bearing REVISE (item 7) — the material finding this cycle

The P5 W-PRUNE entry predicate is a G-Omega sign-off measurable, propagated
identically into FOUR staged carriers (master-plan-diff `:169`, migration-delta
`:58`, handoff-delta `:131`/`:180`, ΩF `:229`) as the UNSCOPED

> `grep -c parse_w11_1_number == 0` (today 7)

The certified SPEC's authoritative gate is SCOPED:
`SPEC.md:475` reads "`grep -c parse_w11_1_number = 0`" and `SPEC.md:570` reads
"P5 `grep -c parse_w11_1_number json/generated.rs` = 7" — i.e., the "= 7" baseline
is the count IN THE SHIPPED RUNTIME FILE `json/generated.rs` ONLY. Run unscoped
across `skinny/crates` at HEAD, the count is **15**: 7 in
`runtime/src/grammars/json/generated.rs` (the shipped runtime), 7 in
`codegen/src/json_sink_direct.rs` (the template SOURCE P5 renames), and 1 in
`codegen/src/lib.rs:565` (a test assertion `assert!(json_generated.contains("parse_w11_1_number_array_direct"))`).

The defect is a scope/baseline mismatch in a sign-off carrier: the predicate is
written unscoped (`grep -c ... == 0`) but the "(today 7)" baseline is the SCOPED
value. A reader treating the unscoped predicate as the literal close-gate will
grep `skinny/crates` and see 15, not the asserted 7 — and the P5 "rename-only at
template source; 1:1 regen" operation, while it DOES drive both the template
source and the regenerated runtime to 0 (and the `lib.rs` test assert must be
updated too), is described as touching only `json/generated.rs` via regen
("The 1:1 regen of `json/generated.rs` consumes the template-source rename",
master-plan-diff `:169`). The carrier's gate, baseline, and operation-description
are mutually inconsistent on scope.

CORRECTION (name the artefact + the exact fix): in all four carriers, scope the
predicate to the SPEC's form — `grep -c parse_w11_1_number json/generated.rs == 0`
(today 7) — matching `SPEC.md:570`. OR, if the campaign-wide cleanup is the
intent, keep the unscoped predicate but fix the baseline to "(today 15: 7
generated + 7 template-source + 1 test)" and have the P5 operation-description
name the `json_sink_direct.rs` rename + the `lib.rs:565` test-assert update as
explicit P5 surfaces, not just the `json/generated.rs` regen. The SPEC's scoped
form is the authoritative one.

## The other REVISE (item 10)

The migration-delta's own anchor note (`:5`) reads "(502/1061-line surfaces; §0.0
currently = SK-V17 receiver at `:30`)". MIGRATION.md is 1061 lines; HANDOFF.md is
502 lines. The MIGRATION-delta is CRUD-4a (MIGRATION only); the "502" is the
HANDOFF length bled in from the sibling handoff-delta's header. The §0.0/:30
anchor itself is correct, so this is a cosmetic self-description imprecision, not a
mis-targeted edit. CORRECTION: in `migration-delta.staged.md:5`, change
"(502/1061-line surfaces; …)" → "(1061-line surface; …)" — the MIGRATION delta
should cite only the MIGRATION length.

## Not found (checked, clean)

- No non-applying diff: the locks-diff applies (EXIT 0); the two architecture-delta
  hunks apply (EXIT 0). master-plan-diff/staged deltas are content-shape by design,
  with `old`-side anchors that byte-resolve live (I byte-compared the Diff-4 §24
  strike to live `MASTER-PLAN.md:1346` — identical).
- No revived REDRESS route: 51/53/246/247 are fenced as rejected with the
  ADMISSIBLE-vs-REJECTED distinction (`1D:168-171`); the §13.7 + master-plan-diff
  Invariant Checks fence AZ-IV-eager / StructRegistry-per-leaf / fact-stream / x86
  / second-substrate; the skinny-vs-totality firewall scope is kept distinct
  (CH5-DEFECT-V1-02/03).
- No Lock-14 narrowing: the addendum preserves the 16-lock count + 5-shape canon
  by ADDITION; the green-by-exclusion clause WIDENS (`FORBIDDEN ⊇ {GENERATED_RS,
  CSS_GENERATED_RS, EventGrammar, *EventGrammar}`); the `css_types.rs` (`LOCKS:349`)
  + 9-ident R16 collapse are explicitly tee'd to SK-V19 (D11b), not laundered into
  the SK-V18 +15 (D11a); the LOCKS:620 phantom-vehicle strike is routed to
  CRUD-3/SK-V19, NOT this cycle. Lock-14 at `:349` is untouched (amendment by
  addition only — verified the live line is unchanged).
- No coupling: GENERALIZE/PROVE waves are predecessor-gated with named entry
  predicates; P4-before-G2/G3 is a hard ordering; PROVE needs G4 closed DIRECTLY;
  W-PRUNE (P1-P5) is the only dispatch-eligible cluster on close (`sk-v18/SPEC.md:46-49`).
- No uncited claim that is also UNGROUNDED: every spot-check resolved to a real
  surface (HEAD, 5 SHAs, 16 locks, 5 shapes, §H waves, REDRESS items, line
  counts). The two REVISEs are a scope/baseline mismatch (7) and a cross-surface
  line-count bleed (10) — both in REAL carriers pointing at REAL targets, not
  fabrications.

## Tally rationale

13 enumerated items: 11 ACCEPT, 2 REVISE, 0 REJECT. The four V2 REVISEs (anchor
drift, §24 anchor, SCANNER-UNIFY cross-ref, COH18 ID divergence) are all CLOSED
this cycle — verified individually, not assumed. The cohort has converged sharply:
the only surviving defects are a P5-predicate scope/baseline mismatch propagated
across four carriers (item 7 — load-bearing, because the P5 close-gate is a
measurable G-Omega sign-off item and its written form disagrees with its baseline)
and a cosmetic cross-surface line-count bleed in the migration-delta's
self-description (item 10). REVISE share = 2/13 ≈ 15.4%, BELOW the ≥30% cycle-V1
expectation — but that expectation is a V1-cycle prior, and this is V3 after two
hardening passes that closed nine of the prior eleven distinct findings; forcing
additional REVISEs to hit 30% would manufacture defects where the spot-verified
load-bearing chain (git-apply EXIT 0 ×3, all SHAs/anchors/counts resolving, no
revived route, no lock narrowing, no coupling) is genuinely clean. The convergence
is real; the residual is two honest scope/labelling fixes. No REJECT: the
locks-diff and architecture-delta apply, no route is revived, no lock is narrowed,
no coupling enters, every cited target is real.

TALLY accept=11 revise=2 reject=0
