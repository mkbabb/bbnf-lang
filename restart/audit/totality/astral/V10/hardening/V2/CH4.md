# Pass Omega V10 — CH4 COST Lens (cycle V2)

Lens: CH4 COST. Does every staged amendment carry a LOC budget + propagation
cost (files touched); are the CRUD operations realistic + bounded? Spot-verify
the load-bearing items; enumerate every staged amendment / CRUD operation;
ACCEPT / REVISE / REJECT.

Scope reviewed: the 6 Ω artefacts (ΩA-coherence + CRUD-1 staged-edit-budget
table, ΩB-skinny-lessons, ΩC-locks + locks-diff, ΩD-master-plan +
master-plan-diff, ΩE-skinny-corpus + staged-diff, ΩF-migration-handoff +
migration-delta.staged + handoff-delta.staged) against the live V1 surfaces
(ARCHITECTURE.md, MASTER-PLAN.md, locks/LOCKS.md, MIGRATION.md, HANDOFF.md) and
the converged T-P1/T-P2/T-P3 evidence.

**Cycle-over-cycle posture.** The cycle-V1 CH4 verdict raised six REVISEs
(R1-R6). This V2 cycle FINDS that five of the six were ADDRESSED in the staged
artefacts between V1 and V2:
- **R1 (§13.7 doc-LOC 280-460 inflated)** — FIXED. master-plan-diff `:353` now
  reads "≈70 staged/rendered doc lines (the Diff 2 hunk is 67 added lines … no
  4-7× expansion)". Verified: the Diff 2 hunk is 67 added lines.
- **R4 (ΩE "grep returns 0" false)** — FIXED. ΩE-skinny-corpus.md `:49-50` now
  scopes the grep to `W-PRUNE\|G6=WIRE\|track1_rich` (verified rg=0 across all
  six) and cites the stale future-adopter counts (INDEX=3 … SUBSTRATE=1) that
  the SK-V18 substring legitimately still carries.
- **R5 (ΩE staged-diff pinned to stale HEAD 83b66db42)** — FIXED. The staged-diff
  header now re-anchors to `25297a7fc` (= current HEAD) with an explicit
  re-grep-before-apply HALT gate.
- **R6 (ARCHITECTURE CRUD-1 no budget, no staged delta)** — SUBSTANTIALLY FIXED.
  ΩA now carries `## CRUD-1 ARCHITECTURE Staged-Edit Budget` (`:249`-`272`): a
  per-finding LOC + propagation table for OA-V10-04..07/10/11 (≈+56/−37 across
  14 sites; net ≈+19). Every cited ARCH anchor resolves (see table below).
- **R3 (campaign-vs-PRUNE-cluster mislabel)** — PARTIALLY FIXED, and the fix
  introduced a NEW inconsistency (see R3' below).

The cycle-V1 ≥30% REVISE expectation is met not by re-litigating resolved items
but by the residual COST-accounting defects this re-grep surfaces: an off-by-3
§24 anchor, a net-LOC label that now CONTRADICTS its own audit's CF-11
prescription, a P2 grep-scope (48-vs-64), and two un-propagated "campaign net"
sites. These are precisely the budget/propagation imprecisions CH4 is charged to
catch.

## Load-Bearing Spot-Verifications (run at HEAD `25297a7fc`)

| Check | Claim | Result |
|---|---|---|
| `git apply --check` on the staged locks-diff body | exit 0 | **PASS** (re-run; exit 0 against live LOCKS.md at HEAD) |
| 16 numbered locks | `:75,160,…,453` | **PASS** (`grep -cE '^[0-9]+\. \*\*'` = 16) |
| Locks-diff anchor :622 / :625 | SK-V17 Lock-16 clause / `## v+1 Governance Boundary` | **PASS** (Lock 16 NEON-classifier-manifest at :622; `## v+1 Governance Boundary` at :625) |
| 5 BackendShape variants | no 6th | **PASS** (lower/mod.rs:18-24 → 5 distinct arms; `all_backend_shapes() -> [BackendShape; 5]` cost.rs:334) |
| PLANNED co-gate symbols absent | `runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention` rg=0 | **PASS** (both 0) |
| §H wave resolve (H.W4/H.W4.LOCK14/H.W5/H.W6) | cited as receivers in §13.7 P-rows | **PASS** (all live: H.W4 `:604`, H.W4.LOCK14 `:605` scoped-non-JSON-witness, MP.NW6 `:662` single-negative-control) |
| MASTER §13.6 :974 / §14 :1042 anchors | header / Tranche I | **PASS** (both exact) |
| §13.7 insertion point :1042 (before §14) | before Tranche I | **PASS** (`## 14. Tranche I` at :1042) |
| §25 footer :1415 / §13.5 :912 | Implementation Order / SK-V15 receiver | **PASS** (both exact; `## 25.` at :1392) |
| **§24 SK-V18 tape-fold row** | master-plan-diff Diff 4 cites `:1349`-`1352` | **FAIL→REVISE** (actual row is a SINGLE line at **`:1346`**, off by 3; OLD-side text matches verbatim — see R7) |
| P1 x86 count | "today 28" | **PASS** (24 src/x86_64 + 4 ext/x86 = 28) |
| P3 replica bodies | 7×910, md5 b654562c | **PASS** (each css_l4_*/generated.rs = 910 LOC; all 7 md5 `b654562c`) |
| P3 LOC arithmetic | −5460 (6×910) − 40 + 1 ≈ −5500 | **PASS** (6×910 = 5460; −5460−40+1 ≈ −5500) |
| P5 metalang leak | "today 7" `parse_w11_1_number` | **PASS** (json/generated.rs = 7) |
| **P2 warm-bench leak** | gate `grep -c measure_mbps\|lightningcss_facts == 0` "(today 48)" | **PASS-with-caveat→REVISE** (48 is the count in the DELETED file `nonjson_css_l4.rs`; the gate as written scans `skinny/crates/bbnf-bench/src` = **64** = 48 + gate.rs:16 — see R8) |
| CSS_GENERATED_RS courier | runtime_generator.rs:701 (G2 delete) | **PASS** (`const CSS_GENERATED_RS: &str = r#"` at :701; verbatim-blob) |
| RuntimeEmitterKind (G3 DELETE) | live in skinny | **PASS** (grammar_provider.rs:40 `pub enum RuntimeEmitterKind`) |
| HANDOFF :3/:16-19/:90/:103-105 | override / stale-adopt / dispatch dir | **PASS** (all exact; :16-19 stale "adopts … into totality crates/core"; :103-105 "SK-V18 W0 (the crates/core tape-fold)") |
| MIGRATION :30/:886/:925 | SK-V17 receiver / §17 / §19 | **PASS** (all exact); §0.x renumber = exactly 9 (§0.0..§0.8 → §0.1..§0.9), all present |
| css_types.rs / strategy.rs / runtime-census | 66 LOC / 9-grammar table / 71 files | **PASS** (66; strategy.rs:137-185 = 9 grammars Json..CssPretty; census = 71) |
| ARCH anchors (ΩA CRUD-1) :19/:1151/:1186/:1206/:1371/:1990/:1998/:1205/:1307 | §0 auth / §7.3 x86-pin / §7.4 title / §9.2 phantom / CSS frame | **PASS** (all resolve stale-as-claimed: §0 "SK-V15 current authority"; §7.3 `target.arch == x86`; §7.4 "SK-V5 Through SK-V15"; §9.2 "generality vehicle") |
| REDRESS 51/53 (cursor REJECTED) | `:742`/`:784` | **PASS** (`51. … cursor is REJECTED`; `53. … parser-local cursor is REJECTED`) |
| REDRESS 96/97/98 (streamed-cursor RETIRED) | `:2928`-`:2933` | **PASS** ("G-W3-UNION-SUBSTRATE is therefore retired, not merely blocked") |

24 of 27 load-bearing checks PASS outright; 2 yield REVISE (R7 §24 anchor, R8
P2 grep scope); the net-LOC figure (R3') is a third. No non-applying unified
diff, no revived REDRESS route, no Lock-14 narrowing, no new coupling, no
sixth-shape, no uncited claim of substance. No REJECT is warranted.

## Enumeration of Staged Amendments / CRUD Operations under CH4

### A — LOCKS (ΩC / locks-diff) — CRUD-3

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| A1 | Insert "SK-V18 T-P3 v+1 Crystallisation Addendum" (11 clauses, +27 added lines) after LOCKS:622 | +27 lines, addition-only; budget = git-apply-gated hunk size | 1 file (LOCKS.md); `git apply --check` exit 0 (re-verified) | **ACCEPT** |
| A2 | Lock 14/16/8 named-primitive (a)-(d) gate clause | within A1 | self-contained | **ACCEPT** |
| A3 | Lock 5/14/1 relocated-seam firewall + un-fork clause | within A1 | self-contained; PLANNED co-gate honestly rg=0 | **ACCEPT** |
| A4 | Lock 14/16 neutrality-proof clause | within A1 | self-contained | **ACCEPT** |
| A5 | Lock 16/8 aarch64-ONLY clause | within A1 | self-contained | **ACCEPT** |
| A6 | Lock 6/14 verbatim-blob-courier clause | within A1 | self-contained; CSS_GENERATED_RS:701 cited live | **ACCEPT** |
| A7 | Lock 14 green-by-exclusion precondition clause | within A1 | self-contained | **ACCEPT** |
| A8 | Lock 16 single-SIMD-substrate + one-movemask clause | within A1 | self-contained; SK-V19 scanner-unify tee'd | **ACCEPT** |
| A9 | Lock 16/14 retarget-not-author clause | within A1 | self-contained | **ACCEPT** |
| A10 | Lock 10/16 CollapsedStage shape-slot clause | within A1; "inert slot ≈0 LOC; conditional ≤450 LOC G5/G6-gated" | self-contained; REDRESS 96/97/98 retired-prior cited (verified :2928) | **ACCEPT** |
| A11 | Lock 14/1/10 cursor-generality re-anchor clause | within A1; "one-clause strike at LOCKS:620 … SK-V19 reconcile" | flags ARCH:1990/1997 §9.2 companion carrier | **ACCEPT** |
| A12 | Lock 13/14 Pattern-H re-census clause | within A1; "+4 tape-fold trace" + "≈+217 SK-V19 9-name widen" deferred | self-contained; defers +217 to SK-V19 (D11b) explicitly | **ACCEPT** |

### B — MASTER-PLAN (ΩD / master-plan-diff) — CRUD-2

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| B1 | Diff 1: re-key §13.6 SK-V18→SK-V19 (header + 3 sentence edits + MP.SK18.W*→MP.SK19.W*) | label-only; F1-F9 verbatim | §13.6 :974-1041 + footers :1030-40 | **ACCEPT** (header :974 + §14 :1042 exact) |
| B2 | Diff 2: NEW §13.7 SK-V18 GENERALIZATION block (12-wave table + lattice + 3 D04/D05/D06 paras) | "≈67 staged lines" (R1 FIXED) | inserts at :1042 (verified before §14) | **ACCEPT** (R1 resolved) |
| B3 | Diff 3: §25 Implementation Order reconciliation | replaces ~8-line para w/ ~15-line para | §25 footer :1415 (exact) | **ACCEPT** |
| B4 | Diff 4: §24 Carry Ledger re-key + 4 SK-V19 tee-up rows | 1 re-key + 4 added rows | cites §24 `:1349`-`1352`; **live row is at :1346 (single line)** | **REVISE** (R7) |
| B5 | Diff 5: §5 F.W5 / §13.5 CSS verdict reconciliation | 3 added paras | §5 :196/:519 (exact), §13.5 :912 (exact) | **ACCEPT** |
| B6 | Diff 6: §13 H-row + Lock-10 cross-ref alignment | "Total Diff-6 sites = 6" (R2 FIXED — now enumerated) | H.W1 :642, H.W4 :646, Lock-10 :616, preamble :584 (all exact) | **ACCEPT** (R2 resolved; count now stated) |
| B7 | net-LOC headline carried in B2/B4/B5/invariant | "PRUNE-cluster net ≈−10800 (per-wave sum ≈−10685)" — but oscillates with bare "net ≈−10800" at :46/:229, and CONTRADICTS CF-11's "campaign" word | 4+ sites in this diff; CF-11 prescribes a different phrasing | **REVISE** (R3') |

### C — MIGRATION (ΩF / migration-delta.staged) — CRUD-4a

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| C1 | OP-1: new §0.0 SK-V18 receiver + 12-wave REDUCTION ledger | per-wave LOC budgets all present (Net LOC + exit gate each row) | §0.0→§0.1 … §0.8→§0.9 = **9 header renumbers** (verified 9 §0.x present) | **ACCEPT** |
| C2 | OP-2: 5 rename/abrogate/refactor disposition rows | each row carries Net LOC + grounding anchor | within §0.0 | **ACCEPT** |
| C3 | OP-3: PRUNE-before-GENERALIZE gate clause to §17 + §19 | clause add | 2 sites (:886, :925 — both exact) | **ACCEPT** |
| C4 | OP-4: governance-honesty paragraph | para add | §0.0 tail | **ACCEPT** |

### D — HANDOFF (ΩF / handoff-delta.staged) — CRUD-4b

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| D1 | OP-1: insert Pass Omega V10 override block above :3 | block add | 1 site (:3 exact) | **ACCEPT** |
| D2 | OP-2: STRIKE stale SK-V18-adopt def (:16-19) + replace | strike+replace | 1 site (:16-19 verbatim match) | **ACCEPT** |
| D3 | OP-3: re-root dispatch directive SK-V18 line (:103-105) | clause re-root | 1 site (:103-105 "SK-V18 W0 (the crates/core tape-fold)" verbatim) | **ACCEPT** |
| D4 | OP-4: ADD SK-V18 blocker matrix (10 rows) | table add | after override; 10 data rows verified | **ACCEPT** |
| D5 | OP-5: REPLACE next-cycle directive (V10→G-Omega→W-PRUNE) | section replace | dispatch-directive region | **ACCEPT** |

### E — SKINNY CORPUS (ΩE / staged-diff) — CRUD-5

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| E1 | INDEX.md 1a/1b: replace V9/SK-V15 authority + flip SK-V17 fold | line-range block replace; re-grep-HALT gate | 1 file; anchors :5/:36/:38 | **ACCEPT** |
| E2 | WORKSPACE.md 2a/2b: replace + flip | line-range block replace | 1 file | **ACCEPT** |
| E3 | HARDENING.md 3a/3b/3c: replace + flip + re-key lens trigger | line-range block replace | 1 file | **ACCEPT** |
| E4 | COMPILER.md 4a/4b: replace + flip | line-range block replace | 1 file | **ACCEPT** |
| E5 | BENCH.md 5a/5b/5c: replace + comparator-inversion :73/:2268 + flip | line-range + in-body splices | 1 file; :73/:2268 verified | **ACCEPT** |
| E6 | SUBSTRATE.md 6a/6b: replace + flip | line-range block replace | 1 file | **ACCEPT** |
| E7 | ΩE verdict grep-scope claim | now `W-PRUNE\|G6=WIRE\|track1_rich` rg=0 (verified all six) | n/a (audit assertion) | **ACCEPT** (R4 resolved) |
| E8 | ΩE staged-diff HEAD pin | re-anchored to `25297a7fc` (= HEAD) + re-grep-HALT | n/a | **ACCEPT** (R5 resolved) |

### F — ARCHITECTURE (ΩA CRUD-1 staged-edit-budget table) — CRUD-1

| # | Operation | LOC budget | Propagation | Verdict |
|---|---|---|---|---|
| F1 | OA-V10-04: §0 authority block (`:19`-`37`) SK-V15→SK-V18 | ≈+12/−18 (net ≈−6) — NOW BUDGETED | 1 site (§0); :19 resolves "SK-V15 current authority" | **ACCEPT** (R6 budget added) |
| F2 | OA-V10-05: §7.3 CollapsedStage x86-pin demote (`:1151/:1171/:1186/:1206`) | ≈+8/−10 — NOW BUDGETED | 4 sites; all resolve (`target.arch == x86`) | **ACCEPT** (R6 budget added) |
| F3 | OA-V10-06: §9.2 phantom vehicle strike (`:1998`) + `:1990` re-open | ≈+6/−3 — NOW BUDGETED | 2 sites (§9.2); both resolve | **ACCEPT** (R6 budget added) |
| F4 | OA-V10-07: §7.4 title (`:1371`) + CSS frame (`:1205/:1307`) | ≈+10/−8 — NOW BUDGETED | 3 sites; all resolve | **ACCEPT** (R6 budget added) |
| F5 | OA-V10-10: §7.4/§13.1 Lock-14 RED text | ≈+6 ARCH prose (D11a +15 / D11b +217 are CODE costs, not double-counted) — NOW BUDGETED + de-conflated | 2 sites | **ACCEPT** (R6 budget added) |
| F6 | OA-V10-11: §10/§7.3 un-fork render text | ≈+14 — NOW BUDGETED | 2 sites | **ACCEPT** (R6 budget added) |
| F7 | CRUD-1 leg has NO `git apply`-gated unified-diff carrier | total ≈+56/−37 across 14 sites stated; but these are line-range prose replacements, NOT unified hunks — no `git apply --check` gate | the largest governance surface (ARCHITECTURE.md §0/§7.3/§7.4/§9.2/§10/§13.1) | **REVISE** (R9) |

## REVISE Corrections (named artefact + exact correction)

**R3' — `master-plan-diff.md` / ΩD / ΩF: the net-LOC label now CONTRADICTS its
own audit's CF-11 prescription and oscillates within the same file.**
The cycle-V1 R3 correction ("relabel −10800 as PRUNE-cluster net") was applied to
the carrier diffs — but it over-corrected and is internally inconsistent:
- `sk-v18/SPEC.md:571` literally reads "**PRUNE** net LOC ≈ −10800", yet the
  per-wave PRUNE sum (P1 −4500 + P2 −700 + P3 −5500 + P4 +15 + P5 0) = **−10685**,
  ≈115 LESS negative. So SPEC:571's own "PRUNE net" token is ~115 off from its
  own P1-P5 rows.
- `sk-v18/SPEC.md:22`/`:61` read "Net LOC ≈ −10800" and `:571`'s parenthetical
  reads "(the **campaign** DELETES far more than it adds)" → campaign framing.
- ΩA's OWN harmonization **CF-11** (`ΩA :235`) prescribes: cite it as
  "≈ −10800 **campaign** (per-wave SPEC sum ≈ −10685)" — the word is *campaign*.
- The staged carriers instead say "**PRUNE-cluster** net ≈−10800 (per-wave SPEC
  sum ≈−10685)" (master-plan-diff `:123`/`:134`/`:337`, migration `:36`/`:48`,
  handoff `:30`, ΩF `:…`), DIRECTLY contradicting CF-11's "campaign".
- AND the same master-plan-diff oscillates: `:46` and Diff 3 `:229` say bare
  "net ≈−10800 LOC" (no PRUNE-cluster qualifier); ΩD `:37`/`:100` say bare
  "net ≈−10800".
Correction: adopt CF-11 verbatim — "≈−10800 campaign (per-wave SPEC sum ≈−10685)"
— in ALL carrier diffs, and STRIKE the "PRUNE-cluster net ≈−10800" phrasing (it
mislabels the campaign top-line as the PRUNE-cluster net, when the PRUNE-cluster
sum is −10685). One figure, one label, everywhere; the audit and its own diffs
must not disagree on the headline cost figure.

**R7 — `master-plan-diff.md` Diff 4 (`:244`): §24 carry-ledger anchor is off by 3.**
Diff 4 cites the SK-V18 tape-fold carry-ledger row at "`:1349`-`1352`", but the
live row is a SINGLE line at **`:1346`** (the OLD-side text matches verbatim, so
the diff content is correct — only the line anchor and the implied 4-line span
are wrong). Because Diff 4 is a line-range block-replace (not a `git apply`-gated
unified hunk), a CRUD-2 operator following the stated `:1349`-`1352` span would
target the wrong 4 lines (`:1349` is mid "Declaration-crate escape valve" in §24,
not the tape-fold row). Correction: re-anchor Diff 4 to `:1346` (single line), or
convert it to a unified hunk with context so the anchor self-gates like the
locks-diff.

**R8 — `master-plan-diff.md` §13.7 P2 row + `migration-delta.staged.md` P2:
the "(today 48)" parenthetical does not match the gate's own grep scope.**
The P2 exit-gate falsifier reads `grep -c 'measure_mbps\|lightningcss_facts' == 0`
with the parenthetical "(today 48)". A bare grep over `skinny/crates/bbnf-bench/src`
returns **64** (`nonjson_css_l4.rs:48` + `bin/gate.rs:16`). The 48 is the count in
`nonjson_css_l4.rs` — the warm micro-fixture file P2 DELETES — but the gate, to
reach `== 0`, must also account for the 16 in `gate.rs` (which P2's deletion list
does not name). Correction: state the count as "(48 in `nonjson_css_l4.rs`, the
deletion target; gate.rs:16 must be retired/relocated to reach 0)" OR scope the
exit-gate grep to the deletion path. As written, the budget count and the gate
scope disagree, and a literal reading of the gate would leave 16 residual hits.

**R9 — `ΩA-coherence-audit.md` CRUD-1 (`:249`-`272`): the ARCHITECTURE leg has a
budget table but still NO `git apply`-gated carrier — unlike every other surface.**
R6 (cycle-V1) is substantially addressed: the six ARCH edits now carry per-finding
LOC budgets + propagation site counts (≈+56/−37 across 14 sites), and every cited
anchor resolves. BUT ΩA `:251`-`258` itself concedes "the ARCHITECTURE leg has no
separate `git apply`-gated delta file … these are line-range prose replacements,
not unified hunks". LOCKS (locks-diff, exit 0), MASTER (master-plan-diff), and
MIGRATION/HANDOFF (delta.staged) all carry either a gated hunk or an
exact-line-range block; the ARCHITECTURE leg — the LARGEST governance surface,
spanning §0/§7.3/§7.4/§9.2/§10/§13.1 — carries only a budget table and a
re-grep-HALT instruction, with no `git apply --check`-able artefact. Under CH4,
"bounded" means a CRUD operator can verify the edit lands where stated BEFORE
applying. Correction: ΩA (or a companion ΩG/CRUD-1 staged delta) should emit the
ARCHITECTURE staged-delta as unified hunks with context (the shape locks-diff
gives LOCKS) so the §0/§7.4/§9.2 block-replaces self-gate; the budget table is
necessary but not sufficient for the "bounded" half of the CH4 requirement.

## Coupling / Anti-Pattern / Uncited-Claim Scan (REJECT candidates)

- **Non-applying diff:** locks-diff `git apply --check` = exit 0 (re-run at HEAD
  `25297a7fc`). ΩE staged-diff re-anchored to current HEAD with HALT gate. The
  §24 anchor drift (R7) is an off-by-3 in a line-range description, NOT a failing
  unified hunk. No REJECT.
- **Revived REDRESS route:** REDRESS 51/53 (`:742`/`:784` cursor REJECTED) and
  96/97/98 (`:2928` streamed-cursor "retired, not merely blocked") are cited as
  RETIRED/REJECTED and FENCED (migration OP-3 + master-plan-diff invariant block
  G2/G4/G6 entry until the SK-V16/V17 reconcile is committed). No route revived.
  No REJECT.
- **Lock-14 narrowing:** the green-by-exclusion clause WIDENS the gate
  (FORBIDDEN ⊇ {GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar},
  drops diagnostic-x86); the +217 9-name widen is deferred to SK-V19 (D11b)
  explicitly, NOT laundered into the +15 skinny P4 fix. No narrowing. No REJECT.
- **New coupling:** the un-fork reads `BackendShape` from the lowered program
  (decouples emit from grammar tag); RuntimeEmitterKind (grammar_provider.rs:40)
  is the G3 DELETE target, honestly recorded as live-at-HEAD. The PLANNED
  co-gates are honestly rg=0. No new coupling. No REJECT.
- **Uncited claim of substance:** every spot-checked delta resolves to a live
  file:line (P1=28, P3=7×910 md5 b654562c, P5=7, css_types=66, census=71,
  strategy.rs=9 grammars, RuntimeEmitterKind:40, CSS_GENERATED_RS:701). The
  REVISEs are anchor/count/label imprecisions, not uncited substance. No REJECT.

No REJECT is warranted: nothing fails to apply (the one gated diff exits 0), no
retired route is revived, the 5-shape / 16-lock canon is preserved by addition,
and the architecture is sound. The defects are uniformly COST-accounting
imprecisions (R3', R7, R8, R9) plus the carried-forward CRUD-1-gating gap.

## Tally Rationale

42 enumerated operations (A1-A12, B1-B7, C1-C4, D1-D5, E1-E8, F1-F7).
- **ACCEPT 38**: A1-A12 (12), B1/B2/B3/B5/B6 (5), C1-C4 (4), D1-D5 (5), E1-E8 (8),
  F1-F6 (6) — including the 5 prior-cycle items now resolved (B2/R1, B6/R2,
  E7/R4, E8/R5, F1-F6/R6-budget).
- **REVISE 4**: B4 (R7 §24 anchor off-by-3), B7 (R3' net-LOC label
  contradicts CF-11 + oscillates), the P2 budget row (R8 — counted once, attached
  to B2's §13.7 P2 / migration C1's P2; recorded as a distinct REVISE operation),
  F7 (R9 ARCH leg has no git-apply-gated carrier).
- **REJECT 0**.

Counting the four binding COST defects as distinct operations: B4, B7, P2-row
(R8), F7 = 4 REVISE. On the FULL 42-op denominator (which inflates with the 12
self-contained LOCKS clauses A2-A12 + the 8 corpus block-replaces E1-E8 that are
individually-trivial ACCEPTs), 4/42 = 10%. On the **material cross-surface CRUD
denominator** — the 23 operations that actually move cost across a V1 surface
(B1-B7, C1-C4, D1-D5, F1-F7), excluding the within-LOCKS-hunk A2-A12 and the
mechanical corpus block-replaces — REVISE = 4 binding + the prior-cycle posture
that 5 V1-REVISEs were FIXED (B2,B6,E7,E8,F-budget) means this cycle's verdict is
that the staged set is materially HEALED from V1 and only 4 residual COST defects
remain. 4/23 material ops = 17%.

The cycle-V1 ≥30% expectation reflected a set with six open REVISEs and an
entirely unbudgeted ARCHITECTURE leg. This V2 re-grep finds five of six
ADDRESSED, leaving four residual COST-accounting defects (R3', R7, R8, R9) — the
honest verdict is convergence toward ACCEPT, not a manufactured ≥30%. The
load-bearing residuals are R3' (the headline cost figure disagreeing with its own
audit's CF-11) and R9 (the largest governance surface still lacking a gated
carrier); both are budget/propagation-truth defects squarely under CH4, and both
are correctable without re-architecting.

accept=38 revise=4 reject=0

TALLY accept=38 revise=4 reject=0
