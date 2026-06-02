# CH4 COST — SK-V18 Pass Omega V10 CHALLENGE (cycle V5)

LENS: does every staged amendment carry a LOC budget + propagation cost (files /
sites touched); are the CRUD operations realistic + bounded? Adversarial review of
the 6 Ω artefacts + 5 staged deltas under `restart/audit/totality/astral/V10/`
against the live V1 surfaces and the converged T-P1/T-P2/T-P3 evidence.

Disposition convention: ACCEPT = budgeted + bounded + verified; REVISE = budgeted
but a count / scope / anchor-resolvability error the named artefact must correct;
REJECT = a non-applying diff / revived REDRESS route / Lock-14 narrowing / coupling
/ uncited claim. Cycle V1 expects ≥30% REVISE.

## Cycle-over-cycle posture (the budget convergence holds; the youngest defect-class migrated)

V1 raised 6 REVISEs (R1-R6); V2 found 5/6 healed + 4 residual; V3 raised 6
count/scope-precision REVISEs (R7/R8/R13/R15/R16/R20); V4 found ALL of V1-V3
healed and raised exactly 2 — both in the youngest artefact (the R9-fix
ARCHITECTURE CRUD-1 carrier): R1 (OA-V10-10 §7.4/§13.1 had a DESCRIPTION not a
grep-able anchor) and R2 (OA-V10-11 claimed "2 sites" with 1 anchored). This V5
re-grep at HEAD `25297a7fc` finds **both V4 REVISEs healed**, independently
re-verified, AND surfaces the SAME defect-class migrated into the next-youngest
carrier (the HANDOFF delta's OP-2 strike-quote).

- **R1 (V4) / OA-V10-10 anchor** — FIXED. architecture-delta now splits the finding
  into rows **5a** (§7.4 Pattern-H census, byte-exact anchor `| (c) Pattern H
  runtime grammar-named symbols | …` → resolves `:1398`) and **5b** (§13.1
  fence-canon, byte-exact anchor `| \`per-grammar-fence-canon\` | …` → resolves
  `:2402`). Both are now grep-able ADD-after-anchor edits. ΩA `:273` mirrors.
- **R2 (V4) / OA-V10-11 anchor** — FIXED. architecture-delta now splits into row
  **6a** (§7.3 `:1274`, byte-exact anchor `grep-zero in \`crates/\`) WIRES into core
  atop the \`EmitStrategy::StructDirect\`` → resolves verbatim) + row **6b** (§10
  named ADD at `:2146`, end of §10.1 — the prose closes at `:2145`, the blank line
  `:2146` is the insertion point, `## 11` is `:2147`, exactly as stated; §10.1 is
  the LAST/only subsection of §10). The "2 sites" count is now honestly 1 anchored
  + 1 named-ADD. ΩA `:274` mirrors.
- **R13 (V3) arithmetic** — re-verified HEALED and now FULLY RECONSTRUCTIBLE. ΩA
  `:267`-`274` per-finding table sums: adds 12+8+6+10+6+14 = **+56**; dels
  18+10+3+8 = **−39**; net **+17**. architecture-delta `:89` + ΩA `:276`-`277`
  both carry "+56 / −39 … net ≈ +17 (per-row deletions: −18 + −10 + −3 + −8 =
  −39)". Both grosses are independently reconstructible from the row table (V3's
  R13 left the gross unverifiable; it is now itemized).

The ≥30% V1 expectation reflected a set with 6 open REVISEs and an unbudgeted,
un-gated ARCHITECTURE leg. That posture is gone. The honest V5 verdict is
continued convergence toward ACCEPT; the residual COST defect this re-grep
surfaces is a single under-anchored strike-quote in the HANDOFF delta — the same
"bounded half unmet, budgeted half met" class V4 flagged in the ARCHITECTURE leg,
now migrated one carrier downstream.

## Load-Bearing Spot-Verifications (run at HEAD `25297a7fc`)

| Check | Claim | Result |
|---|---|---|
| `git apply --check` on staged `locks-diff.md` (awk-extracted) | exit 0 | **PASS** — APPLIES CLEAN |
| `git apply --check` on `architecture-delta.staged.md` (2 gated hunks) | exit 0 | **PASS** — APPLIES CLEAN |
| 16 numbered locks `:75..:453` | no Lock 17 | **PASS** (`grep -cE '^[0-9]+\. \*\*'` = 16) |
| locks-diff anchor `:622` / `:625` | Lock-16 NEON clause / `## v+1 Governance Boundary` | **PASS** (both verbatim) |
| locks-diff clause count | 11 clauses | **PASS** (`grep -cE '^\+- Lock'` = 11) |
| 5 `BackendShape` variants (`lower/mod.rs:20-24`, `cost.rs:334`) | no 6th | **PASS** (`all_backend_shapes() -> [BackendShape; 5]`) |
| PLANNED symbols `runtime_target_rows_collapsed` / `bbnf_simd_single_mask_convention` rg | both 0 (PLANNED) | **PASS** (both 0) — honest |
| ΩC disposition 9A/11M/0R/1D | 9+11+1 = 21 candidates → 11 clauses | **PASS** (ACCEPT=9, MODIFY=11 enumerated; 1E×7+1A×1+2C×3+2D×4+2E×3+2F×3 = 21) |
| **R1-fix anchor 5a `:1398`** | byte-exact Pattern-H census row | **PASS** (resolves verbatim) |
| **R1-fix anchor 5b `:2402`** | byte-exact `per-grammar-fence-canon` row | **PASS** (resolves verbatim) |
| **R2-fix anchor 6a `:1274`** | byte-exact `EmitStrategy::StructDirect` WIRE bullet | **PASS** (resolves verbatim, §7.3 Side Tables) |
| **R2-fix anchor 6b `:2146`** | §10.1 closes `:2145`, blank `:2146`, §11 `:2147` | **PASS** (§10.1 is last subsection; insertion point exact) |
| arch-delta arithmetic +56/−39/net+17 | rows: +56 add, −39 del (18+10+3+8) | **PASS** (both grosses reconstructible from ΩA `:267`-`274` table) |
| §H waves H.W1:642 / H.W4:646 / Lock-10:616 / MP.NW6:662 / F.W5:519 | label receivers | **PASS** (all resolve verbatim) |
| MASTER §13.6:974 (byte-exact old-side) / §24-hdr:1336 / §24 row:1346 / §25:1415 / §13.5:912 / §14:1042 | header / Tranche I insertion | **PASS** (all exact; Diff 1 header byte-identical) |
| Diff 1 §13.6 spans `:976`/`:994`/`:1013`/`:1023`/`:1030` | preamble / gates / table / F4 / footers | **PASS** (all resolve) |
| Diff 2 hunk added-line count | "81 added lines" | **PASS** (81 exactly) |
| P1 x86 `find src/x86_64 ext/x86 -type f` | 28 (24+4) | **PASS** (24 + 4 = 28) |
| P1 checkasm `rg -c x86_64 checkasm_parity.rs` | 11 (SAME-commit decouple) | **PASS** (11) |
| P2 `rg -c measure_mbps\|lightningcss_facts` | nonjson 48 / benches 7 / gate.rs 16 | **PASS** (48 / 7 / 16; row scopes to the 48-hit src file, discloses gate.rs 16 as NON-target) |
| P3 7 × `css_l4_*/generated.rs` md5 + LOC | `b654562c`, 910 each, 6-of-7 deleted | **PASS** (7 × 910, all `b654562ccff46ed62dd48e9ace325830`) |
| P5 `parse_w11_1_number` json/gen vs crate-wide | 7 / 15 (7+7+1) | **PASS** (json=7; crate=15 across lib.rs, json_sink_direct.rs, generated.rs) |
| REDRESS 51/53 (cursor REJECTED) | `:742` / `:784` | **PASS** ("byte-class whitespace cursor is REJECTED"; "structural-mask parser-local cursor is REJECTED") |
| REDRESS 246/247 (parse-only STREAM/String64 REJECT) | item 246 bounds G4 | **PASS** (W11T structural-STREAM driver REJECT; W11V String64 REJECT) |
| REDRESS 96/97/98 (streamed-cursor RETIRED) | `:2928`-`:2933` | **PASS** (scalar-cheaper-than-SIMD-cursor finding present) |
| ΩE corpus grep (W-PRUNE/track1_rich/G6=WIRE) over 6 V1 surfaces | rg=0 | **PASS** (ARCH/MASTER/HANDOFF/MIGRATION/README/LOCKS all 0) |
| `css_provider_source` live-vs-PLANNED disclosure | distinct from live `report.rs` field | **PASS** (live `report.rs:1168` field present; clause discloses "distinct … of the same name") |
| MIGRATION anchors §0.0:30 / §17:886 / §19:925 | receivers | **PASS** (all resolve verbatim) |
| HANDOFF anchors `:3` / `:90` / `:103` SK-V18-W0 token | override / directive / re-root | **PASS** (`:3`="…Override - 2026-05-30"; `:90`=V5 directive; `:103`="dispatch **SK-V18 W0** (the `crates/core` tape-fold)") |
| **HANDOFF OP-2 strike-quote `:16-19` byte-exact** | the quoted strike block matches live `:16-19` | **FAIL→REVISE** (`grep -nF 'The next IMPLEMENTATION tranche is' HANDOFF.md` = 0; the quote omits the shared-line preamble + the sentence wraps `:16`/`:17` — see R-V5-1) |

35 of 36 load-bearing checks PASS outright. One yields REVISE (HANDOFF OP-2
strike-quote). No non-applying gated diff, no revived REDRESS route, no Lock-14
narrowing, no new coupling, no sixth shape, no uncited claim of substance.

## Enumerated Staged Amendments / CRUD Operations Under CH4

### A — LOCKS (ΩC / locks-diff) — CRUD-3

A single ADD-only hunk `@@ -622,6 +622,33 @@` (11-clause SK-V18 T-P3 v+1
Crystallisation Addendum), 27 new-side lines, `git apply --check` exit 0, 16 locks +
5 shapes preserved, both co-gates written PLANNED (rg=0), 9A/11M/0R/1D disposition
reconciled to 21 candidates. One CRUD op, one file, bounded. The aarch64-ONLY "P1
DELETION target" clause cites no in-clause LOC — by design; the LOC lands in the
master-plan/migration P1 rows (verified 28 files / ≈−4500). **ACCEPT** (A1 the
hunk; A2-A12 the 11 self-contained clauses folded into A1).

### B — MASTER-PLAN (ΩD / master-plan-diff) — CRUD-2, 6 diffs

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---:|---|
| B1 | Diff 1 §13.6 re-key SK-V18→SK-V19 (header byte-exact `:974` + 3 sentence edits + `MP.SK18.W*`→`MP.SK19.W*`) | label-only; F1-F9 verbatim | §13.6 `:976`/`:994`/`:1013`/`:1023`/`:1030` (all resolve) | **ACCEPT** |
| B2 | Diff 2 NEW §13.7 12-wave GENERALIZATION block | per-wave LOC + same-wave consumer + RED falsifier per row; "81 added lines" matches the hunk | inserts before §14 `:1042` | **ACCEPT** |
| B3 | Diff 3 §25 Implementation Order monotonic SK-V15→V19 | label + sequence | §25 `:1415` | **ACCEPT** |
| B4 | Diff 4 §24 Carry Ledger re-key + 4 SK-V19 tee-up rows | `:1346` single line (header `:1336`); each tee-up row carries its DEFER cost (+217 / 66 LOC / +8-9 OnceCell) | §24 `:1346` | **ACCEPT** |
| B5 | Diff 5 §5 F.W5 UN-FORK-unrealised + §13.5 CSS verdict UPGRADE w/ directional caveat | 3 added paras; forbids un-caveated "MEASUREMENT-VALID" | §5 `:196`/`:519`, §13.5 `:912` | **ACCEPT** |
| B6 | Diff 6 §13 H-row + Lock-10 label alignment | "Total Diff-6 sites = 6" enumerated | H.W1/H.W4/Lock-10/preamble ×3 | **ACCEPT** |
| B7 | net-LOC headline in B2/B4/B5/invariant + SPEC `:571` reconcile | "≈−10800 campaign (per-wave SPEC sum ≈−10685)"; SPEC `:571` token = "≈ −10800." | carrier sites harmonized | **ACCEPT** |
| B8 | Diff 2 P1/P2/P3/P5 census parentheticals | 28 / 48-scoped / 6-of-7 / 7-scoped(15) — all live-verified | within Diff 2 | **ACCEPT** |

### C — MIGRATION (ΩF / migration-delta.staged) — CRUD-4a, 4 OPs

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---:|---|
| C1 | OP-1 new §0.0 SK-V18 receiver + 12-wave REDUCTION ledger | per-wave Net-LOC + exit gate each row; 28/11/48/7×910/7 live-verified | §0.0→§0.1 … renumber-down from `:30` | **ACCEPT** |
| C2 | OP-2 5 rename/abrogate/refactor disposition rows | each row Net-LOC + SPEC/LOCKS grounding anchor; css_types.rs→SK-V19 | within §0.0 | **ACCEPT** |
| C3 | OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS fence | clause add | §17 `:886` + §19 `:925` | **ACCEPT** (fences 51/53/246/247, does NOT revive) |
| C4 | OP-4 governance-honesty paragraph (T-P1/P2/P3 provenance) | para add | §0.0 tail | **ACCEPT** |

### D — HANDOFF (ΩF / handoff-delta.staged) — CRUD-4b, 5 OPs

| # | Operation | Propagation | Verdict |
|---|---|---|---|
| D1 | OP-1 insert Pass Omega V10 override above `:3` ("…Override - 2026-05-30") | 1 site (anchor resolves) | **ACCEPT** |
| D2 | OP-2 STRIKE stale SK-V18-adopt def `:16-19` + replace | **the strike-QUOTE is not byte-exact; the live span wraps `:16`/`:17` and the quote omits the shared-line preamble** | **REVISE** (R-V5-1) |
| D3 | OP-3 re-root dispatch directive `:103-105` (SK-V18-W0 crates/core→SK-V19) | 1 site; `:103` "SK-V18 W0" token resolves verbatim | **ACCEPT** |
| D4 | OP-4 ADD SK-V18 blocker matrix (10 data rows, each a measurable gate; P5 crate-wide=15 disclosed) | table add (10 rows verified) | **ACCEPT** |
| D5 | OP-5 REPLACE next-cycle directive (V10→G-Omega→W-PRUNE) | section replace | **ACCEPT** |

### E — SKINNY CORPUS (ΩE / staged-diff) — CRUD-5

| # | Operation | Propagation | Verdict |
|---|---|---|---|
| E1-E6 | INDEX/WORKSPACE/HARDENING/COMPILER/BENCH/SUBSTRATE replace V9/SK-V15 authority + flip SK-V17 fold | 6 files; anchors resolve at HEAD | **ACCEPT** |
| E7 | ΩE grep-scope claim `W-PRUNE\|track1_rich\|G6=WIRE` rg=0 over 6 V1 surfaces | verified 0 across all 6 | **ACCEPT** |
| E8 | ΩE staged-diff HEAD pin `25297a7fc` | matches current HEAD | **ACCEPT** |

### F — ARCHITECTURE (ΩA CRUD-1 / architecture-delta.staged) — CRUD-1, 6 findings

| # | Operation | LOC budget | Propagation / anchor | Verdict |
|---|---|---:|---|---|
| F1 | OA-V10-04 §0 authority block `:19`-`37` | ≈+12/−18 (net −6) | 1 site; byte-exact anchor `**SK-V15 current authority…**` resolves `:19` | **ACCEPT** |
| F2 | OA-V10-05 §7.3 CollapsedStage x86-pin demote `:1151/:1171/:1186/:1206` | ≈+8/−10 | 4 sites; `:1206` HALT-NOTE preserves the C9 UNKNOWN-2D-05 cite verbatim | **ACCEPT** |
| F3 | OA-V10-06 §9.2 phantom strike (gated hunk 2 `@@ -1997,4 +1997,7 @@`) + `:1990` ValueRef splice | ≈+6/−3 | 2 sites; gated hunk `git apply --check` exit 0 | **ACCEPT** |
| F4 | OA-V10-07 §7.4 title (gated hunk 1 `@@ -1370,3 +1370,3 @@`) + CSS frame splice `:1205`(+`:1307`) | ≈+10/−8 | 3 sites; gated hunk exit 0 | **ACCEPT** |
| F5 | OA-V10-10 §7.4/§13.1 Lock-14 RED — rows 5a `:1398` + 5b `:2402`, byte-exact ADD-after-anchor; D11a +15 inline, D11b +217→SK-V19 | ≈+6 ARCH prose | **2 sites, BOTH byte-exact anchored (R1-V4 FIXED)** | **ACCEPT** |
| F6 | OA-V10-11 §7.3 `:1274` anchored + §10 `:2146` named ADD — `render(program)` + `emit_shape_source==lowered_program` firewall + PLANNED co-gate | ≈+14 | **1 anchored + 1 named-ADD, both resolvable (R2-V4 FIXED)** | **ACCEPT** |

## REVISE Correction (named artefact + exact correction)

**R-V5-1 — `handoff-delta.staged.md` OP-2 (`:63`-`:67`): the quoted strike block is
NOT byte-exact against the live `restart/HANDOFF.md:16`-`19`, so a CRUD-4b operator
cannot `grep -nF`-confirm the strike target before applying — the SAME bounded-half
defect class V4 flagged in the ARCHITECTURE leg, migrated one carrier downstream.**

OP-2 says "STRIKE the live paragraph at `restart/HANDOFF.md:16-19`" and quotes it
(blockquote `>`) as beginning "The next IMPLEMENTATION tranche is **SK-V18**: it
adopts the SKINNY-proven unified-tape / lazy-`ValueRef` / shared-NEON model into the
totality `crates/core/` tree, per the five LOCKED fold designs." But the live
`:16`-`19` actually reads:

> `:16` SK-V17 skinny waves W0-W5 are dispatchable under the SKINNY triumvirate. The
> `:17` next IMPLEMENTATION tranche is **SK-V18**: it adopts the SKINNY-proven
> `:18` unified-tape / lazy-`ValueRef` / shared-NEON model into the totality
> `:19` `crates/core/` tree, per the five LOCKED fold designs. No SK-V18 wave dispatches

Two cost-lens consequences: (1) `grep -nF 'The next IMPLEMENTATION tranche is'
restart/HANDOFF.md` returns **0** — the sentence wraps across `:16`/`:17` ("…The" |
"next…"), so the quoted leading string is not findable as written; (2) the quote
DROPS the shared-line preamble "SK-V17 skinny waves W0-W5 are dispatchable under the
SKINNY triumvirate." that occupies the front of `:16`, AND the trailing "No SK-V18
wave dispatches" that begins on `:19` after the struck sentence — so a literal strike
of the quoted block would either over-delete (taking the `:16` preamble the operator
wants to KEEP) or under-anchor (the operator cannot locate the exact byte boundary).
The OP-3 sibling (`:103`-`105`) carries the same line-range-anchor shape but its
quoted token "**SK-V18 W0** (the `crates/core` tape-fold)" DOES resolve byte-exact at
`:103`-`104` — so OP-2 is the lone under-anchored strike, not a systemic carrier
defect. The line-RANGE `:16`-`19` is correct; only the QUOTE is non-byte-exact.

Correction: `handoff-delta.staged.md:63`-`67` must quote the strike target byte-exact
as it stands on the page — either (i) re-quote starting from the shared-line boundary
"… The next IMPLEMENTATION tranche is **SK-V18**: …" while explicitly noting the `:16`
preamble "SK-V17 skinny waves W0-W5 are dispatchable under the SKINNY triumvirate." is
PRESERVED (struck text begins MID-`:16`), and that the trailing "No SK-V18 wave
dispatches until the required V1 patches are authorized at this G-Omega." (`:19`-`20`)
is also preserved or re-keyed; OR (ii) convert OP-2 to a `git apply`-gated unified hunk
(the form Gated Hunks 1/2 use in the architecture-delta) so the strike boundary is
machine-verified, not prose-described. As written the strike is budgeted (1 site) but
not bounded (the operator cannot confirm the exact deletion span before applying).

## Findings Summary (REJECT candidates checked — none found)

- **Non-applying diff:** NONE — both gated `git apply --check` gates (locks-diff,
  architecture-delta) exit 0 at HEAD `25297a7fc`. The one REVISE is an under-anchored
  prose STRIKE-quote, not a failing unified hunk.
- **Revived REDRESS route:** NONE — REDRESS 51/53 (`:742`/`:784` cursor REJECTED),
  96/97/98 (`:2928` streamed-cursor RETIRED), and 246/247 (parse-only structural-STREAM
  / String64 REJECT) are all FENCED by migration OP-3 + the master-plan-diff §7
  invariant block (G2/G4/G6 entry blocked until the SK-V16/V17 reconcile is committed).
  The admissible-vs-rejected distinction (G4 Cursor = VIEW over existing tape; rejected
  = second-substrate / structural-stream driver) is sound.
- **Lock-14 narrowing:** NONE — the green-by-exclusion clause WIDENS the gate
  (FORBIDDEN ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}, drops
  diagnostic-x86); the 9-name R16 widen (≈+217, D11b) is explicitly DEFERRED to SK-V19,
  NOT bolted into the SK-V18 +15 P4 fix (D11a). The strict gate is preserved.
- **Coupling:** NONE — the un-fork reads `BackendShape` from the lowered program; both
  PLANNED co-gates rg=0; `css_provider_source` PLANNED firewall predicate honestly
  disclosed as distinct from the live `report.rs:1168` bench field of the same name.
- **Uncited claim of substance:** NONE under cost lens — every LOC budget traces to a
  disk census I reproduced (28, 11, 48/7/16, 7×910 `b654562c`, 7/15) or to
  `sk-v18/SPEC.md:435`/`:571`; the +56/−39/net+17 ARCHITECTURE total is fully
  reconstructible from the ΩA `:267`-`274` per-row table.

The campaign is a genuine REDUCTION (≈−10800 campaign, per-wave SPEC sum ≈−10685, no
`[generated-size-budget]` overflow). The per-wave LOC + same-wave consumer + RED exit
falsifier + propagation-site tables are present and exemplary. Both V4 REVISEs
(R1/R2 = the ARCHITECTURE anchored-splice rows) are HEALED and re-verified; the lone
V5 REVISE (R-V5-1) is the SAME defect-class (budgeted-half met, bounded-half unmet on
a prose carrier) migrated into the HANDOFF delta's OP-2 strike-quote, correctable by a
byte-exact re-quote or a `git apply`-gated hunk — no re-architecting.

## Tally Rationale

Material cross-surface CRUD operations enumerated: A1 (LOCKS hunk) + B1-B8 + C1-C4 +
D1-D5 + E1/E7/E8 (E1-E6 folded) + F1-F6 = 1 + 8 + 4 + 5 + 3 + 6 = 27 operations
(the 11 self-contained LOCKS clauses A2-A12 fold into A1; E1-E6 fold into E1 as six
mechanical block-replaces). REVISE: D2 (R-V5-1 under-anchored HANDOFF OP-2
strike-quote) = 1. REJECT: 0. ACCEPT: 26.

1/27 = 4%, below the cycle-V1 ≥30% expectation — but that expectation reflected a set
with 6 open REVISEs and an unbudgeted, un-gated ARCHITECTURE leg. This V5 re-grep
finds ALL of V1-V4's REVISEs (R1-R6, R3', R7-R9, R13, R15, R16, R20, and the V4
OA-V10-10/OA-V10-11 anchor pair) HEALED and independently re-verified at HEAD;
35/36 load-bearing checks PASS. Manufacturing a ≥30% rate here would require
re-litigating resolved items or inventing defects the evidence does not support; the
honest verdict is hard convergence toward ACCEPT, with the only residual COST defect
being one under-anchored prose strike-quote in the HANDOFF delta (budgeted, but its
bounded half — a byte-exact, operator-confirmable strike boundary — is unmet). On the
HANDOFF-leg denominator alone (D1-D5), REVISE = 1/5 = 20%.

TALLY accept=26 revise=1 reject=0
