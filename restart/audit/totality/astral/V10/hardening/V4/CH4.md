# CH4 COST — SK-V18 Pass Omega V10 CHALLENGE (cycle V4)

LENS: does every staged amendment carry a LOC budget + propagation cost (files /
sites touched); are the CRUD operations realistic + bounded? Adversarial review of
the 6 Ω artefacts + 5 staged deltas under `restart/audit/totality/astral/V10/`
against the live V1 surfaces and the converged T-P1/T-P2/T-P3 evidence.

Disposition convention: ACCEPT = budgeted + bounded + verified; REVISE = budgeted
but a count / scope / anchor-resolvability error the named artefact must correct;
REJECT = a non-applying diff / revived REDRESS route / Lock-14 narrowing / coupling
/ uncited claim. Cycle V1 expects ≥30% REVISE.

## Cycle-over-cycle posture (the budget convergence is real)

V1 raised 6 REVISEs (R1-R6); V2 found 5 of 6 addressed + 4 residual (R3'/R7/R8/R9);
V3 raised 6 count/scope-precision REVISEs (R7 §24 anchor, R8 P2 scope, R13
arithmetic, R15 24-vs-28, R16 9-vs-11 checkasm, R20 67/76 hunk). This V4 re-grep at
HEAD `25297a7fc` finds **every prior-cycle REVISE healed in the current staged
state**, independently re-verified:

- **R3'/CF-11 net-LOC label** — FIXED. All **13** carrier sites read
  "≈ −10800 campaign LOC (per-wave SPEC sum ≈ −10685)" verbatim; `grep -c
  'PRUNE-cluster' ΩC/ΩD/ΩF = 0`. The over-corrected "PRUNE-cluster net" phrasing is
  gone; one figure, one label, everywhere.
- **R7 §24 anchor** — FIXED. master-plan-diff Diff 4 (`:251`-`:255`) now reads
  "a SINGLE line at `:1346` (the §24 header is `:1336`)"; both resolve verbatim.
- **R8 P2 scope** — FIXED. The P2 gate is now SPEC-`:633`-scoped to
  `nonjson_css_l4.rs` (today 48), with "the 16 crate-wide hits in `bin/gate.rs` are
  NOT a P2 gate target" disclosed. Live: nonjson=48, gate.rs=16, crate-wide=64.
- **R13 arithmetic** — FIXED. architecture-delta `:87`-`:90` and ΩA `:276`-`:277`
  both read "≈ +56 / −39 prose LOC … net ≈ +17 (per-row deletions: −18 + −10 + −3 +
  −8 = −39)". The V3 −37/+19 slip is corrected and the breakdown is itemized.
- **R15 24-vs-28** — FIXED. migration-delta `:54`/`:85` now read "24 src/x86_64 + 4
  ext/x86 = 28 files, the verify grep's full reach". Live: 24 + 4 = 28.
- **R16 9-vs-11 checkasm** — FIXED. All surfaces read "the 11 `checkasm_parity.rs`
  x86_64 call sites DECOUPLE in the SAME commit". Live `rg -c x86_64
  checkasm_parity.rs` = 11.
- **R20 67/76 hunk** — FIXED. master-plan-diff `:361` reads "≈81 staged/rendered
  doc lines (the Diff 2 hunk is 81 added lines)"; the actual `+`-count of the Diff 2
  block is 81. ΩD no longer repeats a stale count.
- **P3 "6 of 7"** — FIXED. master-plan-diff `:171` + migration-delta `:56` read
  "collapse 6 of 7 byte-identical css_l4 replicas". Live: 7 × 910 LOC, all md5
  `b654562ccff46ed62dd48e9ace325830`.
- **R9 ARCH-leg gating gap** — SUBSTANTIALLY FIXED. The new
  `architecture-delta.staged.md` carrier emits the 2 cleanest edits (§7.4 title,
  §9.2 phantom strike) as `git apply --check`-gated unified hunks (exit 0
  re-verified) + 4 anchored splices with byte-exact re-grep-HALT strings.

The ≥30% V1 expectation reflected a set with 6 open REVISEs and an unbudgeted,
un-gated ARCHITECTURE leg. That posture is gone. The honest V4 verdict is
convergence toward ACCEPT; the residual COST defects this re-grep surfaces are
concentrated in the youngest artefact (the R9-fix ARCHITECTURE carrier), where the
"bounded" half of the CH4 requirement is met for 4 of 6 findings but NOT for 2.

## Load-Bearing Spot-Verifications (run at HEAD `25297a7fc`)

| Check | Claim | Result |
|---|---|---|
| `git apply --check` on staged `locks-diff.md` (awk-extracted) | exit 0 | **PASS** — APPLIES CLEAN |
| `git apply --check` on `architecture-delta.staged.md` (2 gated hunks) | exit 0 | **PASS** — APPLIES CLEAN |
| 16 numbered locks `:75..:453` | no Lock 17 | **PASS** (`grep -cE '^[0-9]+\. \*\*'` = 16) |
| locks-diff anchor `:622` / `:625` | Lock-16 NEON clause / `## v+1 Governance Boundary` | **PASS** (both verbatim) |
| 5 `BackendShape` variants (`lower/mod.rs:20-24`, `cost.rs:334`) | no 6th | **PASS** (`all_backend_shapes() -> [BackendShape; 5]`) |
| PLANNED symbols `runtime_target_rows_collapsed` / `bbnf_simd_single_mask_convention` rg | both 0 (PLANNED) | **PASS** (both 0) — honest |
| §H waves H.W1:642 / H.W4:646 / H.W4.LOCK14:605 / Lock-10:616 / MP.NW6:662 | receivers in §13.7 P-rows | **PASS** (all resolve verbatim) |
| MASTER §13.6:974 / §14:1042 / §24:1346 / §24-hdr:1336 / §25:1392 | header / Tranche I / carry-ledger row / §25 | **PASS** (all exact) |
| P1 x86 `find src/x86_64 ext/x86 -type f` | 28 (24+4) | **PASS** (24 + 4 = 28) |
| P1 checkasm `rg -c x86_64 checkasm_parity.rs` | 11 | **PASS** (11) |
| P2 `rg -c measure_mbps\|lightningcss_facts` | nonjson 48 / gate.rs 16 / crate 64 | **PASS** (48 / 16 / 64) |
| P3 7 × `css_l4_*/generated.rs` md5 + LOC | `b654562c`, 910 each, 6-of-7 deleted | **PASS** (7 × 910, all `b654562c…`) |
| P5 `parse_w11_1_number` json/gen vs crate-wide | 7 / 15 (7+7+1) | **PASS** (json=7; crate=15 across lib.rs, json_sink_direct.rs, generated.rs) |
| net-LOC label across 13 carrier sites | "campaign (SPEC sum ≈−10685)" verbatim | **PASS** (13/13 harmonized; PRUNE-cluster rg=0) |
| arch-delta arithmetic +56/−39/net+17 | −18−10−3−8 = −39 | **PASS** (sums check) |
| §13.7 Diff 2 hunk added-line count | "81 added lines" | **PASS** (81) |
| RuntimeEmitterKind (G3 DELETE target) live in skinny | `grammar_provider.rs` enum | **PASS** (live; honestly recorded as G3 DELETE) |
| CSS_GENERATED_RS courier `runtime_generator.rs:701` | verbatim-blob (G2 delete) | **PASS** (cited live) |
| REDRESS 51/53 (cursor REJECTED) | `:742` / `:784` | **PASS** ("byte-class whitespace cursor is REJECTED"; "structural-mask parser-local cursor is REJECTED") |
| REDRESS 246/247 (parse-only stream/string64 REJECT) | `:6186` / `:6232` | **PASS** (item 246 = W11T structural-STREAM driver REJECT, bounds G4; item 247 REJECT) |
| REDRESS 96/97/98 (streamed-cursor RETIRED) | `:2928`-`:2933` | **PASS** (scalar-cheaper-than-SIMD-cursor finding present) |
| ΩE corpus grep (W-PRUNE/track1_rich/G6=WIRE) over 6 docs | rg=0, stale SK-V18 substring remains | **PASS** (all 6: W-PRUNE/track1_rich/G6=WIRE = 0; SK-V18 substring 3/2/1/3/4/1 — stale future-adopter framing, as scoped) |
| ΩE staged-diff HEAD pin | `25297a7fc` + re-grep-HALT | **PASS** (re-anchored to current HEAD) |
| **arch-delta anchor #5 (OA-V10-10) byte-exact old-side string** | header: "each carries its byte-exact old-side anchor" | **FAIL→REVISE** (row 5 carries NO byte-exact string — "(the §7.4/§13.1 Lock-14 self-gate RED text)"; `grep -nF 'self-gate'/'FALSIFIED' ARCHITECTURE.md = 0` — see R1) |
| **arch-delta anchor #6 (OA-V10-11) §10/§7.3 site label vs resolved anchor** | "2 sites (§10, §7.3)" | **REVISE** (the one byte-exact anchor `…EmitStrategy::StructDirect` resolves at `:1274`, inside §7.3 Side Tables; the §10 half carries no resolvable anchor — see R2) |

26 of 28 load-bearing checks PASS outright. Two yield REVISE, both inside the
youngest artefact (the R9-fix ARCHITECTURE CRUD-1 carrier). No non-applying gated
diff, no revived REDRESS route, no Lock-14 narrowing, no new coupling, no sixth
shape, no uncited claim of substance.

## Enumerated Staged Amendments / CRUD Operations Under CH4

### A — LOCKS (ΩC / locks-diff) — CRUD-3

A single ADD-only hunk `@@ -622,6 +622,33 @@` (11-clause SK-V18 T-P3 v+1
Crystallisation Addendum), 27 new-side lines, `git apply --check` exit 0, 16 locks +
5 shapes preserved, both co-gates written PLANNED (rg=0). One CRUD op, one file,
bounded. The aarch64-ONLY "P1 DELETION target" clause cites no in-clause LOC — by
design; the LOC lands in the master-plan/migration P1 rows (verified 28 files /
≈−4500). **ACCEPT** (A1 the hunk; A2-A12 the 11 self-contained clauses).

### B — MASTER-PLAN (ΩD / master-plan-diff) — CRUD-2, 6 diffs

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---:|---|
| B1 | Diff 1 §13.6 re-key SK-V18→SK-V19 (header + 3 sentence edits + `MP.SK18.W*`→`MP.SK19.W*`) | label-only; F1-F9 verbatim | §13.6 `:974`-`1041` + footers `:1030`-`40` | **ACCEPT** |
| B2 | Diff 2 NEW §13.7 12-wave GENERALIZATION block | per-wave LOC + same-wave consumer + RED falsifier per row; "≈81 staged lines" matches the 81-line hunk | inserts `:1042` (before §14) | **ACCEPT** (R20 resolved) |
| B3 | Diff 3 §25 Implementation Order monotonic SK-V15→V19 | label + sequence | §25 footer `:1415` | **ACCEPT** |
| B4 | Diff 4 §24 Carry Ledger re-key + 4 SK-V19 tee-up rows | `:1346` single line (header `:1336`); each tee-up row carries its DEFER cost (+217 / 66 LOC / +8-9 OnceCell) | §24 `:1346` | **ACCEPT** (R7 resolved) |
| B5 | Diff 5 §5 F.W5 UN-FORK-unrealised + §13.5 CSS verdict UPGRADE w/ directional caveat | 3 added paras; forbids un-caveated "MEASUREMENT-VALID" | §5 `:196`/`:519`, §13.5 `:912` | **ACCEPT** |
| B6 | Diff 6 §13 H-row + Lock-10 label alignment | "Total Diff-6 sites = 6" enumerated | H.W1/H.W4/Lock-10/preamble ×3 | **ACCEPT** (R2-V1 resolved) |
| B7 | net-LOC headline in B2/B4/B5/invariant | "campaign (SPEC sum ≈−10685)" — 13/13 harmonized | 13 sites | **ACCEPT** (R3'/CF-11 resolved) |
| B8 | Diff 2 P1/P2/P3/P5 census parentheticals | 28 / 48-scoped / 6-of-7 / 7-scoped(15) — all live-verified | within Diff 2 | **ACCEPT** (R8/R15/R16/V3-P3/P5 resolved) |

### C — MIGRATION (ΩF / migration-delta.staged) — CRUD-4a, 4 OPs

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---:|---|---|
| C1 | OP-1 new §0.0 SK-V18 receiver + 12-wave REDUCTION ledger | per-wave Net-LOC + exit gate each row; counts live-verified | §0.0→§0.1 … renumber-down | **ACCEPT** |
| C2 | OP-2 5 rename/abrogate/refactor disposition rows | each row Net-LOC + SPEC/LOCKS grounding anchor | within §0.0 | **ACCEPT** |
| C3 | OP-3 PRUNE-before-GENERALIZE gate + G2/G4/G6 REDRESS fence | clause add | §17 `:886` + §19 `:925` | **ACCEPT** (fences 51/53/246/247, does NOT revive) |
| C4 | OP-4 governance-honesty paragraph (T-P1/P2/P3 provenance) | para add | §0.0 tail | **ACCEPT** |

### D — HANDOFF (ΩF / handoff-delta.staged) — CRUD-4b, 5 OPs

| # | Operation | Propagation | Verdict |
|---|---|---|---|
| D1 | OP-1 insert Pass Omega V10 override above `:3` | 1 site | **ACCEPT** |
| D2 | OP-2 STRIKE stale SK-V18-adopt def `:16-19` + replace | 1 site (verbatim match) | **ACCEPT** |
| D3 | OP-3 re-root dispatch directive `:103-105` (SK-V18→SK-V19 crates/core) | 1 site | **ACCEPT** |
| D4 | OP-4 ADD SK-V18 blocker matrix (10 rows, each a measurable gate; P5 crate-wide=15 disclosed) | table add | **ACCEPT** |
| D5 | OP-5 REPLACE next-cycle directive (V10→G-Omega→W-PRUNE) | section replace | **ACCEPT** |

### E — SKINNY CORPUS (ΩE / staged-diff) — CRUD-5

| # | Operation | Propagation | Verdict |
|---|---|---|---|
| E1-E6 | INDEX/WORKSPACE/HARDENING/COMPILER/BENCH/SUBSTRATE replace V9/SK-V15 authority + flip SK-V17 fold (line-range block + re-grep-HALT) | 6 files; anchors resolve at HEAD | **ACCEPT** |
| E7 | ΩE grep-scope claim `W-PRUNE\|G6=WIRE\|track1_rich` rg=0 | verified 0 across all 6 | **ACCEPT** (R4 resolved) |
| E8 | ΩE staged-diff HEAD pin `25297a7fc` + re-grep-HALT | matches current HEAD | **ACCEPT** (R5 resolved) |

### F — ARCHITECTURE (ΩA CRUD-1 / architecture-delta.staged) — CRUD-1, 6 findings

| # | Operation | LOC budget | Propagation / anchor | Verdict |
|---|---|---:|---|---|
| F1 | OA-V10-04 §0 authority block `:19`-`37` SK-V15→SK-V18 | ≈+12/−18 (net −6) | 1 site; byte-exact anchor `**SK-V15 current authority…**` resolves `:19` | **ACCEPT** |
| F2 | OA-V10-05 §7.3 CollapsedStage x86-pin demote `:1151/:1171/:1186/:1206` | ≈+8/−10 | 4 sites; anchor `…target.arch == x86…` resolves `:1151`; `:1206` HALT-NOTE preserves the C9 UNKNOWN-2D-05 cite verbatim | **ACCEPT** |
| F3 | OA-V10-06 §9.2 phantom strike (gated hunk 2 `@@ -1997,4 +1997,7 @@`) + `:1990` ValueRef re-open splice | ≈+6/−3 | 2 sites; gated hunk `git apply --check` exit 0; anchor `**Lazy \`ValueRef<G>\`…` resolves `:1990` | **ACCEPT** |
| F4 | OA-V10-07 §7.4 title (gated hunk 1 `@@ -1370,3 +1370,3 @@`) + CSS frame splice `:1205`(+`:1307`) | ≈+10/−8 | 3 sites; gated hunk exit 0; CSS-frame anchor `\| \`SinkOnly\` \| direct typed-field sink…` resolves `:1205` | **ACCEPT** |
| F5 | OA-V10-10 §7.4/§13.1 Lock-14 RED — record D11a +15 inline, tee D11b +217 → SK-V19 | ≈+6 ARCH prose | **2 sites claimed (§7.4,§13.1) — NEITHER carries a byte-exact anchor; "self-gate RED" resolves to 0 hits in ARCHITECTURE.md** | **REVISE** (R1) |
| F6 | OA-V10-11 §10/§7.3 un-fork render text — `render(program)` + `emit_shape_source==lowered_program` firewall + PLANNED co-gate | ≈+14 | **2 sites claimed (§10,§7.3); the one byte-exact anchor `…EmitStrategy::StructDirect` resolves `:1274` = §7.3 ONLY; §10 half un-anchored** | **REVISE** (R2) |

## REVISE Corrections (named artefact + exact correction)

**R1 — `architecture-delta.staged.md` anchor-table row 5 (`:84`) + ΩA `:273`: the
OA-V10-10 edit has NO resolvable byte-exact insertion point, violating the carrier's
own contract.**
The anchored-splice table header (`:74`-`:76`) states "each carries its byte-exact
old-side anchor. The CRUD-1 operator MUST `grep -nF` the quoted anchor, confirm it
resolves at the stated line, and HALT … if the anchor has moved." Rows 1/2/3/4/6 all
carry a backtick-quoted byte-exact string (`**SK-V15 current authority…`, `…target.arch
== x86…`, `**Lazy \`ValueRef<G>\`…`, `\| \`SinkOnly\` \|…`, `…EmitStrategy::StructDirect`).
Row 5 alone carries a DESCRIPTION — "(the §7.4/§13.1 Lock-14 self-gate RED text)" —
not a grep-able string. Worse, `grep -nF 'self-gate' restart/ARCHITECTURE.md` = **0**
and `grep -c 'FALSIFIED' = 0`: there is no "self-gate RED" / "FALSIFIED" framing in
either §7.4 (its Lock-14 rows describe leak classes + remediation, e.g. `:1488`
"GrammarConfig / Lock 14 REDRESS 121 … not fleet-wide grammar-neutral closure", and the
verification command at `:1934`) or §13.1 (a lint-manifest table, `:2388`). ΩA `:273`
budgets this as "2 sites (§7.4, §13.1)", but a CRUD-1 operator following the byte-exact
re-grep-HALT discipline cannot locate either site — the edit is unbounded (where does
"record D11a +15 inline" land in §7.4? in §13.1?). Under CH4 "bounded" means the
operator can verify the edit lands where stated BEFORE applying; this one cannot.
Correction: `architecture-delta.staged.md` row 5 must supply a byte-exact old-side
anchor string for EACH of the 2 sites (e.g. the §7.4 `restart/locks/LOCKS.md:220`-`263`
Lock-14 row at `:1488`, and the §13.1 `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` row at
`:2388`/`:1567`), the same grep-HALT shape rows 1-4/6 carry — OR re-scope OA-V10-10 to
an ADD-after-anchor at a named line rather than an edit of nonexistent "RED text".

**R2 — `architecture-delta.staged.md` anchor-table row 6 (`:85`) + ΩA `:274`: the
OA-V10-11 propagation count "2 sites (§10, §7.3)" overstates the bounded-and-anchored
reach (1 site, §7.3 only).**
Row 6 is labelled "§10/§7.3 un-fork render" and ΩA `:274` budgets "≈+14 … 2 sites
(§10, §7.3)". But the row carries exactly ONE byte-exact anchor — `grep-zero in
\`crates/\`) WIRES into core atop the \`EmitStrategy::StructDirect\`` — which resolves at
**`:1274`, inside §7.3 Side Tables** (header `:1061`; §10 Codegen And Lowerers does not
begin until `:2093`). The §10 half of the "2 sites" claim carries NO resolvable anchor:
the `render(program)`/`emit_shape_source==lowered_program` text the ΩA CF-10 row
(`:236`) says must land "in §10/§7.3" has a concrete §7.3 target but only an unanchored
§10 assertion. So the propagation cost CH4 demands (files/sites a CRUD operator can
verify) is 1 anchored site, not 2. (Note: the §7.3 anchor itself carries a stale
self-reference "CH4-V3-01" in its surrounding bullet text at `:1272`, evidencing a prior
CH4 cycle already drafted into this exact span — the operator should re-confirm the
anchor has not already absorbed the edit.) Correction: either supply a byte-exact §10
anchor string (so the count is honestly 2 bounded sites) OR re-state OA-V10-11 as "1
anchored site (§7.3 `:1274`) + a §10 ADD whose insertion line is named" so the
2-site/≈+14 budget matches the resolvable reach.

## Findings Summary (REJECT candidates checked — none found)

- **Non-applying diff:** NONE — both gated `git apply --check` gates (locks-diff,
  architecture-delta) exit 0 at HEAD `25297a7fc`. The two REVISEs are unbounded /
  under-anchored anchored-SPLICE rows, not failing unified hunks.
- **Revived REDRESS route:** NONE — REDRESS 51/53 (`:742`/`:784` cursor REJECTED),
  96/97/98 (`:2928` streamed-cursor RETIRED), and 246/247 (`:6186`/`:6232` parse-only
  structural-STREAM / String64 REJECT) are all FENCED by migration OP-3 + the
  master-plan-diff invariant block (G2/G4/G6 entry blocked until the SK-V16/V17
  reconcile is committed). The admissible-vs-rejected distinction (G4 Cursor = VIEW
  over existing tape; rejected = second-substrate / structural-stream driver) is sound.
- **Lock-14 narrowing:** NONE — the green-by-exclusion clause WIDENS the gate
  (FORBIDDEN ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}, drops
  diagnostic-x86); the 9-name R16 widen (≈+217, D11b) is explicitly DEFERRED to SK-V19,
  NOT bolted into the SK-V18 +15 P4 fix (D11a). The strict gate is preserved.
- **Coupling:** NONE — the un-fork reads `BackendShape` from the lowered program
  (decouples emit from grammar tag); both PLANNED co-gates rg=0; `css_provider_source`
  PLANNED predicate honestly disclosed as distinct from the live `report.rs` bench field
  of the same name.
- **Uncited claim of substance:** NONE under cost lens — every LOC budget traces to a
  disk census I reproduced (28, 11, 48/16/64, 7×910 `b654562c`, 7/15) or to
  `sk-v18/SPEC.md:431-447`/`:571`/`:435`.

The campaign is a genuine REDUCTION (≈−10800 campaign, per-wave SPEC sum ≈−10685, no
`[generated-size-budget]` overflow). The per-wave LOC + same-wave consumer + RED exit
falsifier + propagation-site tables are present and exemplary. The two REVISE items
(R1/R2) are both in the youngest artefact — the ARCHITECTURE CRUD-1 carrier minted to
close V2's R9 — and both are the SAME class of defect: an anchored-splice row whose
"bounded" half (a resolvable byte-exact insertion point per claimed site) is unmet while
its "budgeted" half (LOC + site count) is present. They are correctable by supplying the
missing byte-exact anchor strings, no re-architecting.

## Tally Rationale

Material cross-surface CRUD operations enumerated: A1 (LOCKS hunk) + B1-B8 + C1-C4 +
D1-D5 + E1/E7/E8 (E1-E6 folded) + F1-F6 = 1 + 8 + 4 + 5 + 3 + 6 = 27 operations
(the 11 self-contained LOCKS clauses A2-A12 are folded into A1; E1-E6 fold into E1 as
six mechanical block-replaces). REVISE: F5 (R1 unbounded OA-V10-10), F6 (R2
under-anchored OA-V10-11) = 2. REJECT: 0. ACCEPT: 25.

2/27 = 7%, below the cycle-V1 ≥30% expectation — but that expectation reflected a set
with 6 open REVISEs and an unbudgeted, un-gated ARCHITECTURE leg. This V4 re-grep finds
ALL of V1-V3's REVISEs (R1-R6, R3', R7-R9, R13, R15, R16, R20, the 6-of-7 / P5-scope
items) HEALED and independently re-verified at HEAD. Manufacturing a ≥30% rate here
would require re-litigating resolved items or inventing defects the evidence does not
support; the honest verdict is hard convergence toward ACCEPT, with the only residual
COST defects being the 2 unbounded/under-anchored anchored-splice rows in the youngest
artefact (both in the ARCHITECTURE leg, both squarely under CH4's "bounded" half, both
one-token-anchor-string fixes). On the ARCHITECTURE-leg denominator alone (F1-F6),
REVISE = 2/6 = 33%.

TALLY accept=25 revise=2 reject=0
