# CH6 NEXT-TRANCHE-IMPACT — Pass Omega V10 CHALLENGE (cycle V5)

Lens: does Ω-F's next-cycle directive specify concrete measurable entry
conditions; are the G-Omega sign-off items (the locks-diff, the master-plan-diff,
the CRUD plan, the SK-V18 close summary) concretely measurable?

Boundary respected: every artefact under `restart/audit/totality/astral/V10/` is
STAGED ONLY; no live governance surface is edited. I spot-verified the
load-bearing items rather than re-deriving T-P3, and I re-checked the two
V4-cycle REVISEs (plus the carried V1/V3 fixes) for closure before hunting new
defects.

## Prior-cycle REVISE closure check (both V4 REVISEs re-verified, both FIXED)

- **V4 item 4** (HANDOFF OP-4 blocker matrix P3 row dropped the md5-distinctness
  half of the certified P3 conjunction): FIXED. handoff-delta `:125` now reads
  `md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧
  `runtime_target_rows_collapsed == true` with the explicit
  "md5-distinctness half is NECESSARY-not-sufficient … matches master-plan-diff
  `:171` + `SPEC.md:435`" gloss. The full conjunction is now carried.
- **V4 items 6/9** (P2 gate cited SPEC `:633`, the "INDEPENDENT" disclosure
  line, as the gate binding; bare-filename `nonjson_css_l4.rs` ambiguous between
  src=48 / benches=7): FIXED in BOTH carriers. migration-delta `:55` and
  master-plan-diff `:170` now both read
  `grep -c 'measure_mbps\|lightningcss_facts' bbnf-bench/src/nonjson_css_l4.rs == 0`
  (the `src/`-qualified path), cite SPEC `:614` (owner-path) + `:627`
  (exit-gate falsifier) as the binding, and explicitly demote `:633` to "the
  R14/H1 INDEPENDENT disclosure note, it binds NOTHING about the gate". I
  re-verified live SPEC: `:614` = "Owner paths: `bbnf-bench/src/nonjson_css_l4.rs`
  (DELETE `measure_mbps`/`*_lightningcss_facts` — 48"; `:627` = the exit-gate
  falsifier; `:633` = "lazy-rich-vs-eager-CSSOM framing is disclosed at H1.
  INDEPENDENT." — all three citations now correct, and the live counts
  (src=48, benches=7, bin/gate=16) match the carriers' parenthetical exactly.

The carried V1 (V6→V10 label), V3 (P5 scope, line-count bleed) fixes all remain
closed; no "Pass Omega V6" string labels the current pass in any staged carrier
(only the legitimate historical `MIGRATION.md:190` W5BR provenance, the ΩA
OA-V10-03 flag, the CRUD-6 scrub gate, and Ω-F's refusal condition survive).

## Spot-verifications (the load-bearing items, this cycle — all re-run at HEAD)

1. **`git apply --check` on the staged locks-diff → EXIT 0** (HEAD `25297a7fc`).
   The architecture-delta's 4 hunks ALSO `git apply --check` EXIT 0. 16 numbered
   locks (`grep -cE '^[0-9]+\. \*\*' LOCKS.md` = 16); `cost.rs:334`
   `all_backend_shapes() -> [BackendShape; 5]`; both PLANNED co-gate symbols
   (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) rg=0.
   Insertion anchor verified live: Lock-16 NEON clause at `LOCKS.md:622`, blanks
   `:623`/`:624`, `## v+1 Governance Boundary` at `:625` — header
   `@@ -622,6 +622,33 @@` is arithmetically consistent (apply EXIT 0 confirms).
2. **A cited §H wave resolves.** H.W4.LOCK14 (`MASTER-PLAN.md:605`,`:647`), H.W5
   (`:606`,`:648`), MP.NW6 single-negative-control standard (`:662`, carries
   `scoped non-JSON witness` verbatim) all resolve. The Diff-1 §13.6 header strike
   target at `:974` BYTE-MATCHES; the Diff-4 §24 carry row at `:1346` BYTE-MATCHES;
   §14 insertion `:1042` resolves; the Diff-3 §25 strike paragraph at `:1415-1422`
   BYTE-MATCHES live (the "does SK-V18 W0 (§13.6\nMP.SK18.W0) dispatch" phrase
   spans a line wrap — I read the full paragraph to confirm). ΩD cites §25's
   section header `:1392` while the diff cites the precise strike footer `:1415` —
   these are complementary (header vs strike-target), not a conflict.
3. **A REDRESS reference resolves.** Items 51 (`skinny/REDRESS.md:742`,
   "is REJECTED"), 53 (`:784`, "is REJECTED"), 246
   (`:6184`, "SK-V14 W11T Parse-Only Structural Stream Reject"), 247 (`:6230`,
   "SK-V14 W11V Parse-Only String64 Reject") are all genuine rejected routes in
   the canonical 6465-line `skinny/REDRESS.md`. The `1D:168-171`
   ADMISSIBLE-vs-REJECTED table fences each; the carriers correctly point at
   `skinny/REDRESS.md` (not the nonexistent `restart/skinny/REDRESS.md`).
4. **Live drift COH18-001 confirmed.** `HANDOFF.md:19` (the `:16-19` paragraph)
   defines SK-V18 as the totality-`crates/core/`-adopt cycle; `:103-104` dispatch
   directive says "dispatch SK-V18 W0 (the `crates/core` tape-fold)". The
   handoff-delta OP-2/OP-3 strike/re-root targets the correct lines.
5. **W-PRUNE entry predicates re-grepped against live source — every count
   matches.** P1 `find …/x86_64 …/ext/x86 -type f` = **28** (matches "today 28");
   `checkasm_parity.rs` x86_64 sites = **11** (matches the "11 call sites" coupling
   claim; live path `bbnf-simd/tests/checkasm_parity.rs`). P2: src=**48**,
   benches=**7**, bin/gate=**16** (all match). P5: scoped `json/generated.rs` = **7**,
   unscoped `skinny/crates` = **15** (= 7 generated + 7 `json_sink_direct.rs`
   template-source + 1 `codegen/src/lib.rs:565` test-assert — the assert is
   precisely at `:565` as cited, verbatim
   `assert!(json_generated.contains("parse_w11_1_number_array_direct"))`).
6. **ARCH/LOCKS reconcile anchors resolve.** ARCH `:19` = "SK-V15 current
   authority (2026-05-28, G-Omega V9 CRUD-1)" (CF-03 staleness real); ARCH
   `:1997-1998` carries "The `G:EventGrammar` type parameter is the generality
   vehicle" (CF-06/OA-V10-06 strike target); LOCKS `:349` (Lock 14) names
   `crates/core/src/css_types.rs` VERBATIM + the "overfitting mess" phrase the
   migration-delta quotes (both exact); LOCKS `:620` carries the same generality-
   vehicle clause (the SK-V19/CRUD-3 reconcile target). ΩA carries CF-01..CF-12
   (12 items), each naming surface + owner; ΩC disposition = 9 ACCEPT / 11 MODIFY
   / 0 REJECT / 1 DEFER = 21. SPEC `:571` = "≈ −10800."; SPEC `:435` P3 figure
   = "≈ −5500 (6×910 replica bodies deleted; +1 `PartialEq` derive; ~−40 collapsed
   rows)" — all internally coherent across carriers.

## The material NEW finding this cycle — the P3 `generator_grammar_count == 3`
## mis-bind in the migration-delta (a P3 gate that is un-closeable by its own SPEC)

The P3 W-PRUNE exit gate is a G-Omega sign-off measurable. The **migration-delta**
is the SOLE carrier that binds `generator_grammar_count == 3` as a **P3** exit
gate, at BOTH of its P3 occurrences:

- OP-1 12-wave table, `migration-delta.staged.md:56`:
  > P3 replica COLLAPSE … Exit gate: `runtime_target_rows_collapsed == true`;
  > **`generator_grammar_count == 3`**
- OP-2 disposition row, `migration-delta.staged.md:87`:
  > `runtime_target_rows_collapsed == true`; **`generator_grammar_count == 3`**

This contradicts the certified SPEC and the cohort's own framing. SPEC `:254`
(verified live) reads `generator_grammar_count` "MUST be 3 at PROVE
(json+css+sheets); **7-css inflation = the P3 overfit, REJECT**". The
master-plan-diff §13.7 global-gate bullet (`:144`-`:145`) states it correctly:
"through G1-G6 the count is 2 (json + css); Sheets enters the generator only at
the PROVE wave and only on a non-`N` verdict (`sk-v18/SPEC.md:254`: 'MUST be 3 at
PROVE … 7-css inflation = the P3 overfit, REJECT')" — so `generator_grammar_count
== 3` is a PROVE-EXIT gate. At P3 (a PRUNE wave, before G1/G2 even route
json/css through the generator, and long before Sheets exists) the count is at
most 2; it can never be 3. Binding it as a P3 close-gate makes P3 un-closeable as
written, OR — if a CRUD operator force-greens it — is exactly the 7-css inflation
SPEC `:254` REJECTs.

The two sibling P3 carriers are CLEAN, which proves this is a migration-delta-only
divergence, not a cohort-wide drift: master-plan-diff `:171` P3 row =
`md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧ `runtime_target_rows_collapsed==true`
(no count); handoff-delta OP-4 `:125` P3 row = the same md5 ∧ row-collapse
conjunction (no count — and this is precisely the V4-fixed row). This is the same
class of defect V4 caught in the SIBLING P2 row: a gate-binding precision error
the other carriers got right but one carrier got wrong, surviving into V5.

CORRECTION (name the artefact + the exact fix): in
`migration-delta.staged.md:56` (OP-1) AND `:87` (OP-2), STRIKE
`generator_grammar_count == 3` from the P3 exit gate. The certified P3 gate is the
two-conjunct `md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧
`runtime_target_rows_collapsed == true` (matching master-plan-diff `:171` +
handoff-delta `:125` + SPEC `:435`). `generator_grammar_count == 3` belongs ONLY
to the PROVE row (migration-delta `:64` already carries it correctly there:
"PROVE Sheets … md5-distinct from JSON ∧ CSS"; the §13.7 PROVE-EXIT gate, SPEC
`:254`).

## The secondary REVISE — the migration-delta P5 row inline note omits the
## "NOT the SPEC gate target" disambiguation its three sibling carriers all carry

The migration-delta P5 row (`:58`) and the master-plan-diff P5 row (`:173`)
diverge on the unscoped-count gloss. master-plan-diff `:173` reads
"+ 7 template-source `json_sink_direct.rs` + 1 `lib.rs:565` test-assert, **both
driven to 0 by the same rename+regen but NOT the SPEC gate target**" — the
"NOT the SPEC gate target" clause is the precision that prevents a CRUD operator
from treating the unscoped 15 as the gate baseline (the exact V3-class scope
defect). migration-delta `:58` reads only "+ 7 template-source + 1 `lib.rs:565`
test-assert, **all driven to 0 by the same rename+regen**" — it drops the "NOT
the SPEC gate target" disambiguation. handoff-delta `:181` and ΩF `:230-231`
carry the SPEC-scoped predicate explicitly. The migration-delta is a CRUD-4a
sign-off carrier; without the "NOT the SPEC gate target" clause an operator
reading only the migration ledger could mis-key the gate baseline to the
unscoped 15. This is a precision-parity gap, not a wrong count (the counts 7/15
are all correct), so it is a REVISE not a REJECT.

CORRECTION: in `migration-delta.staged.md:58`, append "but NOT the SPEC gate
target" to the P5 unscoped-count note, matching master-plan-diff `:173` — so all
four carriers state the unscoped 15 is NOT the gate baseline identically.

## Enumeration of staged amendments / CRUD operations (CH6 lens)

| # | Staged amendment / CRUD op | Artefact | Disposition |
|---|---|---|---|
| 1 | locks-diff: SK-V18 T-P3 v+1 Crystallisation Addendum (`git apply --check` EXIT 0; 16-lock/5-shape preserved; 2 PLANNED symbols rg=0; 9A/11M/0R/1D=21; insertion `@@ -622,6 +622,33 @@` arithmetically consistent) | `locks-diff.md` / CRUD-3 | ACCEPT |
| 2 | Ω-F next-cycle directive: 8-step sequence + W-PRUNE entry predicates (P1 `x86_tree_deleted`, P3 `runtime_target_rows_collapsed`, P4 `lock14_gate_scans_codegen`, P5 scoped grep) + GENERALIZE lattice | `ΩF-migration-handoff.md` | ACCEPT |
| 3 | HANDOFF OP-1..OP-3, OP-5 (override block, strike `:16-19`, re-root `:103-105`, next-cycle directive) — anchors byte-verified (502 lines, `:3`/`:16`/`:90`/`:103` resolve) | `handoff-delta.staged.md` / CRUD-4b | ACCEPT |
| 4 | HANDOFF OP-4 blocker matrix (10 rows; P3 row now carries the full md5 ∧ row-collapse conjunction — V4 item 4 FIXED) | `handoff-delta.staged.md` / CRUD-4b | ACCEPT |
| 5 | MIGRATION OP-1 §0.0 SK-V18 Pass Omega V10 receiver + 12-wave reduction ledger; anchor `:30` verified; `css_types.rs` correctly tee'd to SK-V19; P2 gate now SPEC `:614`/`:627`-bound (V4 item 6 FIXED) | `migration-delta.staged.md` / CRUD-4a | ACCEPT |
| 6 | **MIGRATION OP-1 P3 ledger row** binds `generator_grammar_count == 3` as a P3 exit gate — un-closeable by SPEC `:254` ("MUST be 3 at PROVE; 7-css inflation = the P3 overfit, REJECT"); siblings (master-plan-diff `:171`, handoff-delta `:125`) are clean | `migration-delta.staged.md:56` | **REVISE** |
| 7 | **MIGRATION OP-2 css_l4-replica disposition row** repeats the same P3 `generator_grammar_count == 3` mis-bind | `migration-delta.staged.md:87` | **REVISE** |
| 8 | MIGRATION OP-2 x86/courier/phantom/css_types disposition rows + OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS pre-block (V10-labelled; `1D:168-171` resolves; 51/53/246/247 fenced) + OP-4 governance-honesty | `migration-delta.staged.md` | ACCEPT |
| 9 | **MIGRATION OP-1 P5 ledger row** drops the "NOT the SPEC gate target" disambiguation its three sibling carriers (master-plan-diff `:173`, handoff `:181`, ΩF `:230`) all carry on the unscoped-15 note | `migration-delta.staged.md:58` | **REVISE** |
| 10 | master-plan-diff Diff 1/2/3/4/5/6 (re-key §13.6→SK-V19, NEW §13.7 12-wave block, §25, §24, §5/§13.5, §13 H-row): `-`-side anchors `:974`/`:1042`/`:1336`/`:1346`/`:1415` byte-resolve; §13.7 global-gate framing (`:144`) correctly scopes count==3 to PROVE-EXIT; P2 `:614`/`:627`-bound | `master-plan-diff.md` / CRUD-2 | ACCEPT |
| 11 | architecture-delta CRUD-1 carrier: 4 `git apply`-gated hunks (EXIT 0) + re-grep-HALT anchored splices; ARCH `:19`/`:1997-1998` anchors verified | `architecture-delta.staged.md` / CRUD-1 | ACCEPT |
| 12 | ΩA CRUD plan CRUD-1..CRUD-6 + CF-01..CF-12 (each names surface + owner + gate; CF-01 identity pivot, CF-06 generality-vehicle strike, CRUD-6 V6→V10 scrub) | `ΩA-coherence-audit.md` | ACCEPT |
| 13 | ΩD master-plan reconciliation: per-delta table (10 SKV18 + 4 carried) each maps source delta → MASTER surface → disposition; §25 header `:1392` / strike `:1415` complementary; refuted census 0 | `ΩD-master-plan-reconciliation.md` | ACCEPT |
| 14 | SK-V18 close summary / disposition 9A/11M/0R/1D=21 consistent across cohort; net-LOC dual figure (≈ −10800 / per-wave ≈ −10685) + P3 arithmetic (6×910=−5460 + ~−40 + 1 PartialEq) coherent | cohort | ACCEPT |

## Measurability verdict on the G-Omega sign-off items

- **locks-diff** — MECHANICALLY measurable: `git apply --check` EXIT 0, 16-lock/
  5-shape invariant, 2 PLANNED symbols rg=0. Strongest item; unchanged-clean.
- **Ω-F next-cycle directive** — concrete measurable entry conditions PRESENT and
  scope-clean; P1/P3/P4/P5 predicates re-grep correctly (28/PLANNED/RED-falsifier/
  scoped-7). The P3-count and P5-note defects do NOT touch the ΩF directive — they
  live only in the migration-delta CRUD-4a ledger rows (#6/#7/#9).
- **master-plan-diff** — measurable by anchor resolution (content-shape diffs by
  design; §8 routes the cost to the manual CRUD pass); `-`-side anchors byte-
  resolve; the P3 gate (`:171`) and the count==3 PROVE-EXIT framing (`:144`) are
  both correct. Clean this cycle.
- **CRUD plan** — measurable: CRUD-1..6 each name surface + operation + owner;
  CF-01..12 each name fix + surface + owner; CRUD-6 is the citation-scrub gate.
- **SK-V18 close summary** — disposition 9A/11M/0R/1D=21 consistent; net-LOC dual
  figure + P3 arithmetic coherent everywhere.

## Not found (checked, clean)

- No non-applying diff: locks-diff applies (EXIT 0); the 4 architecture-delta
  hunks apply (EXIT 0). master-plan-diff/staged deltas are content-shape by design
  with `-`-side anchors that byte-resolve live (I byte-compared the Diff-1 `:974`,
  Diff-3 `:1415`, Diff-4 `:1346` strikes to live — identical).
- No revived REDRESS route: 51/53/246/247 fenced `(REJECT)` in `skinny/REDRESS.md`
  with the ADMISSIBLE-vs-REJECTED distinction (`1D:168-171`); the §13.7 Invariant
  Checks fence AZ-IV-eager / StructRegistry-per-leaf / fact-stream / x86 /
  second-substrate; the skinny-vs-totality firewall scope is kept distinct.
- No Lock-14 narrowing: the addendum preserves 16-lock count + 5-shape by
  ADDITION; the green-by-exclusion clause WIDENS the FORBIDDEN set; the
  `css_types.rs` (`LOCKS:349`) + 9-ident R16 collapse + LOCKS:620 phantom-vehicle
  strike are all explicitly tee'd to SK-V19 / CRUD-3, not laundered into the
  SK-V18 +15. LOCKS `:349`/`:620` untouched (live lines unchanged).
- No coupling: GENERALIZE/PROVE waves are predecessor-gated with named entry
  predicates; P4-before-G2/G3 is a hard ordering; PROVE needs G4 closed DIRECTLY;
  W-PRUNE (P1-P5) is the only dispatch-eligible cluster on close. The §13.6 (now
  SK-V19) being logically "downstream of §13.7" (SK-V18) despite physically
  preceding it is the deliberate in-place-re-key shape (ΩD CH1; ΩA `:178`), not a
  coupling defect.
- No uncited claim that is also UNGROUNDED: every spot-check resolved (HEAD,
  5 SHAs, 16 locks, 5 shapes, §H waves, REDRESS items, x86 reach 28, checkasm 11,
  P2 48/7/16, P5 7/15, SPEC `:254`/`:435`/`:571`/`:614`/`:627`/`:633`/`:755`).
  The three REVISEs are a P3-gate mis-bind in one carrier (two rows: #6/#7) and a
  P5-note precision gap in the same carrier (#9) — all in REAL carriers pointing at
  REAL targets, contradicting the cohort's own correct sibling rows, not
  fabrications.

## Tally rationale

14 enumerated items: 11 ACCEPT, 3 REVISE, 0 REJECT. Both V4 REVISEs (HANDOFF P3
matrix half, P2 `:633` mis-citation across two carriers) are CLOSED this cycle —
verified individually, not assumed. The NEW material defect is the migration-delta
binding `generator_grammar_count == 3` as a P3 exit gate (rows `:56` and `:87`):
SPEC `:254` is explicit that count==3 is a PROVE-EXIT condition and that a 3 at
P3 IS "the P3 overfit, REJECT", so the P3 close-gate as the migration ledger
writes it is un-closeable / self-contradictory — and the cohort's own sibling P3
rows (master-plan-diff `:171`, handoff `:125`) carry the correct two-conjunct gate
without the count, proving this is a migration-delta-only divergence (the exact
V4-class pattern of one carrier mis-binding a gate the siblings got right). The
third REVISE (#9) is the same carrier's P5 row dropping the "NOT the SPEC gate
target" disambiguation its three siblings all carry. REVISE share = 3/14 ≈ 21.4%,
below the ≥30% cycle-V1 prior — but that prior is a V1-cycle expectation, and this
is V5 after four hardening passes that closed every prior distinct finding; the
spot-verified load-bearing chain (git-apply EXIT 0 ×2, all SHAs/anchors/counts/
SPEC-binding-lines/REDRESS items resolving, no revived route, no lock narrowing,
no coupling) is genuinely clean, and manufacturing a fourth REVISE to hit 30%
would invent a defect where none survives. The convergence is real; the residual
is one gate mis-bind (two rows) + one precision-parity gap, all confined to the
single migration-delta carrier. No REJECT: every diff applies or byte-resolves,
no route is revived, no lock is narrowed, no coupling enters, every cited target
is real.

TALLY accept=11 revise=3 reject=0
