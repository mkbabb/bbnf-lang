# Pass Omega V10 CHALLENGE — CH1 CORRECTNESS — Cycle V3

Lens: CH1 CORRECTNESS. Does every cited file:line/SHA resolve; does every REDRESS
reference match content; does the staged `locks-diff` apply cleanly
(`git apply --check` exit 0) to live `LOCKS.md`; does the `master-plan-diff` cite
real §H waves + real SHAs.

Scope: the 6 Ω artefacts (ΩA-ΩF) + the staged diffs under
`restart/audit/totality/astral/V10/` (`locks-diff.md`, `master-plan-diff.md`,
`architecture-delta.staged.md`, `ΩE-skinny-corpus-staged-diff.md`,
`handoff-delta.staged.md`, `migration-delta.staged.md`) against the live V1
surfaces and the converged T-P1/T-P2/T-P3 evidence. HEAD at audit `25297a7fc`
(verified `git rev-parse HEAD`; the git-status snapshot was stale).

Verdict: **REVISE REQUIRED.** Every primary CH1 gate PASS — independently re-run,
not parroted. The cross-document corpus has CONVERGED on the ENTIRE cycle-V1 +
cycle-V2 REVISE set (the "Pass-Omega-V6" current-pass labels, the `LOCKS:621`
off-by-one, the 3-vs-4-item REDRESS abutment set, ΩC's false "byte-identical to
3C" provenance claim, the §24 carry-row `:1349-1352` mis-range are ALL redressed
at this snapshot — verified by direct re-grep, zero residue). The fresh V3
adversarial pass surfaces THREE citation-correctness defects cycles V1/V2 graded
ACCEPT and missed — all in the merge-bound `master-plan-diff` (two echoed into
the staged `*-delta` files): two SPEC-scoped exit-gate falsifiers rewritten as
unscoped crate-wide greps (P2 widens to a crate-wide grep + invents an
out-of-SPEC `bin/gate.rs` retirement; P5 drops the `json/generated.rs` file-scope
the SPEC binds), and a self-inconsistent Diff-2 added-line count (claimed 67,
actually 76). The two falsifier defects are independent author corrections
(distinct waves, files, SPEC anchors), enumerated as rows 4a/4b. Cycle-V1 ≥30%
REVISE is approached (5 of 18 = 27.8% on the per-op enumeration; the corpus has
already absorbed the entire V1+V2 REVISE set, so the residual defect surface is
necessarily thinner — the falsifier-scope-drop is the load-bearing fresh class,
and at the merge-bound-diff granularity the 3 defect-classes touch
master-plan-diff Diff 2 (×2) + §8 + both staged `*-delta` echoes).

## Primary CH1 Gate Results (independently re-run at HEAD `25297a7fc`)

| Gate | Command | Result |
|---|---|---|
| Staged locks-diff applies cleanly | `awk '/^```diff$/{flag=1;next}/^```$/{flag=0}flag' locks-diff.md \| git apply --check -` | **EXIT 0 (CLEAN)** — extracted 37 diff lines, applies |
| Staged architecture-delta applies cleanly | same awk over `architecture-delta.staged.md` (the NEW CH4-V2-R9 gated hunks) | **EXIT 0 (CLEAN)** — 20 diff lines, both gated hunks land |
| 16 numbered locks preserved | `grep -cE '^[0-9]+\. \*\*' LOCKS.md` | **16** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; addendum adds no Lock 17 |
| Five BackendShape variants, no sixth | `lower/mod.rs:18-24` match arm + `cost.rs:334 [BackendShape; 5]` | **5** `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` — both anchors byte-exact |
| Two PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `bbnf_simd_single_mask_convention` | **0 / 0** — both PLANNED, not live |
| Insertion anchor resolves | Lock-16 NEON clause `:622`; `## v+1 Governance Boundary` `:625` | **EXACT** — addendum lands `:622`→`:625`; SK-V15 (`:581-607`)/SK-V17 (`:610-622`) addenda untouched |
| Hunk math internally consistent (locks-diff) | `@@ -622,6 +622,33 @@`; 6 context + 27 added = 33 | **CONSISTENT** |
| `RuntimeEmitterKind` un-fork UNBUILT-at-HEAD honesty | `rg RuntimeEmitterKind runtime_generator.rs` | **LIVE at `:1,17,25`** ({CompiledLowering,RequestFacts}) — the locks-diff present-tense "UNBUILT at HEAD … still branches" claim is ACCURATE |

## §H Wave + SHA Resolution (master-plan-diff) — all spot-checks resolve EXACTLY

§H wave anchors, re-resolved against live `restart/MASTER-PLAN.md`:
- §13.6 header `:974` = live "SK-V18 Tape-Fold Adoption Receiver Block …"
  (Diff 1 old-side byte-exact). **EXACT**
- §13.6 preamble `:976-990` + receiver-row table `:1013-1021` (MP.SK18.W0..W6)
  + §13 preamble `:584-592` + H.W1 `:642` + H.W4 `:646` + Lock-10 row `:616`. **ALL EXACT**
- §14 Tranche I `:1042` = "## 14. Tranche I - Recovery, Incremental, LSP"
  (§13.7 insert anchor). **EXACT**
- §24 header `:1336`; carry-row a SINGLE line at `:1346` (Diff 4 old-side byte-exact;
  the V2 `:1349-1352` mis-range is REDRESSED). **EXACT**
- §25 footer `:1415-1422` (Diff 3 old-side byte-exact; ADOPT line `:1419`). **EXACT**
- §5 F-row `:196` + F.W5 `:519` = "Nine seed grammars build through new template". **EXACT**
- MP.NW6 `:662` (single-negative-control standard) + H.W4.LOCK14 `:605`
  (PARTIAL row), both cited in the locks-diff neutrality clause. **EXACT**
- §H wave-ref existence: H.W1/H.W2/H.W2.5/H.W4/H.W5/H.W6/J.W1/A.W2/F.W5 all present
  (15/21/9/16/10/12/22/2/14 occurrences). **ALL RESOLVE**

SHAs: ALL 22 spot-checked resolve to real commits with matching descriptions —
`25297a7fc` (=live HEAD, T-P3), `66232b7c3` (SK-V15 W11 close), `1c5bd7a25`
(SK-V16 W6-tape), `f6a38445b` (SK-V17 W4/W5 close), `9a0079cfb`/`a913a1ffa`/
`a21573cdf`/`b8b61b047` (SK-V15 W7/W8/W9/W10), `fb8848cf2`/`6bb4b2a6c` (SK-V17
W0/W3), `232479e4d`/`ea8138056` (SK-V16 quarantine/parity), `6fb812752`/
`3f6eb603d` (T-P1/T-P2), `9b52e162d`/`784ceb418`/`820798161`/`4e4aa0648`
(S-P1/S-P2/S-P3), `83b66db42`/`0fbee121f` (alpha/S-P0), `c64148ef2`/`a82196b9e`
(SK-V16 W0 / SK-V15 W0). The S-P2 token is `820798161` (correct), NOT `820598161`.
The 3B source deltas `MP-3B-SKV18-D01..D10` + carried `MP-3B-V1-D01/D02/D09/D10`
ALL resolve in `3B-master-plan-reconciliation.md`.

## REDRESS References — all resolve, all match content

- locks-diff CollapsedStage clause `skinny/REDRESS.md:2795-2944` (finding
  `:2928-2933`): `:2795` = "## SK-V9 Wave 3 Union Event-Model Class-Column
  Redress"; `:2928-2933` = the M5-Max scalar-cheaper-than-SIMD-cursor finding.
  Items 96/97/98 at `:2797`/`:2852`/`:2910`; item 98 "retires
  `G-W3-UNION-SUBSTRATE`" at `:2910` — the "REDRESS 96/97/98 RETIRED" prior the
  CollapsedStage clause clears (does NOT re-open). **MATCH.**
- master-plan-diff/ΩD/migration-delta pre-block items `51/53/246/247`
  (`1D:168-171`): item 246 = "W11T parse-only structural stream" REJECT
  (`REDRESS.md:6184-6219`), item 247 = "W11V string64 mask" REJECT, item 51 =
  "SK-V5 event-cursor" REJECT, item 53 = "SK-V5 structural-mask cursor" REJECT;
  the 4-item set is uniform corpus-wide, "item 246 bounds G4" correct. **MATCH.**
- locks-diff single-substrate clause `3B:177` = "MP.SK19.SCANNER-UNIFY `simd-scan`
  probe-API reconcile … the renamed/parallel-scanner risk is ACTIVE". **MATCH.**

## Cycle-V1 + Cycle-V2 REVISE Set — independently re-checked, ALL REDRESSED

Every prior REVISE item is corrected at THIS snapshot — re-ran each, not assumed:
- **"Pass-Omega-V6" current-pass labels** (V1 #4/#8/#9): `grep -rinE
  'omega.?v6|v6.?blocker'` over the V10 corpus returns ONLY ΩA's OA-V10-03
  finding that FLAGS the stale-V6 in 3F, ΩF's reject-rationale `:257`, and the
  honest historical-V6 collision references — zero current-pass mislabels.
  master-plan-diff `:200` reads "Pass-Omega-V10 / pre-W-PRUNE blocker". **REDRESSED.**
- **`LOCKS:621` off-by-one** (V1 #11/#12/#13): zero `:621` references remain in
  any V10 doc; all cite `LOCKS.md:620` (the "G:EventGrammar type parameter is the
  generality vehicle" line, verified single-long-line at `:620`). **REDRESSED.**
- **3-item-vs-4-item REDRESS set** (V1 #14): no bare `51/53/247` residue; every
  occurrence cites `51/53/246/247`. **REDRESSED.**
- **ΩC "byte-identical to 3C" false provenance** (V2 #2): ΩC `:19-25` + `:128-133`
  now read "V10-hardened consolidation … no clause dropped, added, or REVERSED"
  and explicitly "The diff body is NOT byte-identical … (md5 differs; 6 of 11
  clauses changed)". locks-diff `:5` says "consolidation", not "byte-identical".
  **REDRESSED.**
- **§24 carry-row `:1349-1352` mis-range** (V2 #6): no `:1349` residue;
  master-plan-diff Diff 4 cites the row at `:1346` (single line; §24 header
  `:1336`), matching live. **REDRESSED.**

## Fresh V3 Adversarial Yield — SPEC-scoped exit-gate falsifiers widened/dropped

The certified `sk-v18/SPEC.md` binds P2's and P5's exit-gate falsifiers to
SPECIFIC FILES. The merge-bound `master-plan-diff` (+ the staged `*-delta`
echoes) rewrote both as unscoped crate-wide greps — over-asserting against
occurrences the SPEC's own gate does not target, and (P2) inventing an
out-of-SPEC retirement obligation.

- **P5 — `json/generated.rs` file-scope DROPPED.** SPEC `:755` binds
  `grep -c parse_w11_1_number json/generated.rs == 0` (today 7), re-asserted at
  `:852` on `json_sink_direct.rs == 0` (the template, post-rename). 1D D-8
  (`1D:112-114`) scopes it "×7 in `json/generated.rs`". The LIVE crate-wide count
  is **15** (`json/generated.rs:7` + `json_sink_direct.rs:7` template source +
  `lib.rs:565:1` a TEST `assert!(…contains("parse_w11_1_number_array_direct"))`).
  master-plan-diff MP.SK18.P5 (`:169`) writes the gate UNSCOPED — `grep -c
  parse_w11_1_number == 0` (today 7) — pairing the `json/generated.rs`-scoped
  count "7" with an unscoped command. handoff-delta `:131`/`:180` repeats the
  unscoped gate. An unscoped `== 0` would over-assert: it would RED on the
  template source AND the test at `lib.rs:565` (which itself references the OLD
  symbol), neither of which the SPEC's P5 gate counts.

- **P2 — falsifier widened crate-wide + an UNCITED `bin/gate.rs` retirement.**
  SPEC `:633` binds `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs
  == 0` (today 48); SPEC owner-paths name `nonjson_css_l4.rs` + the bench harness
  ALONE; the SPEC mentions `bin/gate.rs` NOWHERE (verified `grep -ni gate.rs
  SPEC.md` = empty), and no 1D/3B evidence assigns its 16 hits to a wave. The
  LIVE crate-wide count is 64 = `nonjson_css_l4.rs:48` + `bin/gate.rs:16`.
  master-plan-diff MP.SK18.P2 (`:166`) + migration-delta P2 (`:55`) BOTH rewrite
  the gate to `… skinny/crates/bbnf-bench/src == 0` (today 64) and assert "16 in
  `bin/gate.rs` which must be retired/relocated to reach 0" — an out-of-SPEC P2
  obligation. The raw 16-count is real; the "must be retired/relocated as a P2
  gate" claim is uncited and couples P2 to a retirement the certified SPEC does
  not carry.

- **master-plan-diff §8 self-inconsistent Diff-2 line count.** §8 (`:356`) reads
  "the Diff 2 hunk is 67 added lines". The Diff 2 block (lines 129–204, a PURE
  insertion with no `+++` header to subtract) carries **76** `+`-prefixed content
  lines. The "no 4-7× expansion" ARGUMENT survives (76 is still reduction-friendly
  and the 12-row wave table renders 12 rows), but the stated number undercounts
  its own merge-bound hunk by 9.

## Live-Surface Evidence Spot-Checks (independently resolved)

- LOCKS: `:620` generality-vehicle phrase; `:622`/`:625` insertion anchors;
  `:610-622` SK-V17 addendum; `:581-607` SK-V15 addendum.
- ARCH (architecture-delta anchors): `:1371` "### 7.4 SK-V5 Through SK-V15
  Implementation Status" (Gated Hunk 1 old-side byte-exact); `:1997-2000`
  "The `G:EventGrammar` type parameter is the generality vehicle" (Gated Hunk 2
  old-side byte-exact); `:19` "**SK-V15 current authority …**" (splice 1);
  `:1990` "**Lazy `ValueRef<G>` value-plane …" (splice 3). Both gated hunks
  `git apply --check` EXIT 0; all 4 anchored-splice old-sides resolve byte-exact.
- SPEC: `:19-21` GENERALIZATION; `:46-49` W-PRUNE only-dispatch-eligible;
  `:358-390` (a)-(d) gate; `:431-447` 12-wave manifest; `:535-547` lattice;
  `:571` "≈ −10800."; `:711-712` FORBIDDEN_GENERIC_TOKENS.
- Code: `lower/mod.rs:18-24` 5 lowerings; `cost.rs:334 [BackendShape; 5]`;
  7 `css_l4_*/generated.rs` all md5 `b654562c`; `strategy.rs:137-185` 9 grammar
  names (Bbnf/Bnf/CssL4/CssPretty/Csv/Ebnf/GoogleSheets/Json/Math); x86 tree
  `find x86_64 + ext/x86 -type f` = 28 (= 24 `src/x86_64/` + 4 `ext/x86`,
  reconciling the locks-diff "28" and migration-delta-P1 "24 files").

## Enumerated Staged Amendments / CRUD Operations Under CH1

| # | Artefact | Staged amendment / CRUD op | Verdict |
|---|---|---|---|
| 1 | locks-diff (ΩC) | 11-clause SK-V18 T-P3 v+1 Crystallisation Addendum inserted `:622`→`:625`; `git apply --check` EXIT 0; 16 locks / 5 shapes preserved; 2 PLANNED co-gates absent; REDRESS 2795/2928-2933 + 3B:177 + MP.NW6:662 + H.W4.LOCK14:605 + SPEC §6:358-390 + 1E:147-153 all resolve; `LOCKS:620` cited correctly; `RuntimeEmitterKind`-live honesty accurate | **ACCEPT** |
| 2 | ΩC-locks-amendments | 9A/11M/0R/1D disposition resolves against 3C; verification commands resolve; the V2 false-byte-identical claim is now corrected to "hardened consolidation … no clause REVERSED" with explicit md5-differs disclosure | **ACCEPT** (V2 REVISE redressed) |
| 3 | master-plan-diff Diff 1 | Re-key §13.6 SK-V18 Tape-Fold → SK-V19 Totality-Fold; header `:974` + preamble `:976-990` + table `:1013-1021` old-side byte-exact; F1-F9 preserved verbatim | **ACCEPT** |
| 4 | master-plan-diff Diff 2 (block, structure) | NEW §13.7 GENERALIZATION 12-wave block, insert before §14 `:1042`; wave manifest matches SPEC `:431-447`; 4-item REDRESS + V10 label correct; lattice matches `:535-547`; 12 wave rows render 12 lines | **ACCEPT** (structure/anchors clean; the two falsifier defects below are itemised as 4a/4b) |
| 4a | master-plan-diff Diff 2 — MP.SK18.P2 row (`:166`) | **DEFECT: rewrites the SPEC-scoped `nonjson_css_l4.rs == 0` (48) gate to `skinny/crates/bbnf-bench/src == 0` (64) and asserts "16 in `bin/gate.rs` … must be retired/relocated to reach 0" — UNCITED** (SPEC mentions `bin/gate.rs` nowhere; `grep -ni gate.rs SPEC.md` empty; no 1D/3B wave-assignment), an out-of-SPEC P2 obligation that couples P2's RED-condition to a retirement the certified SPEC does not carry | **REVISE** — Ω-D author: restore the SPEC scope — P2 gate → `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs == 0` (today 48); DROP the `bin/gate.rs` "must be retired" clause or cite a wave that owns it. |
| 4b | master-plan-diff Diff 2 — MP.SK18.P5 row (`:169`) | **DEFECT: writes `grep -c parse_w11_1_number == 0` UNSCOPED while pairing the `json/generated.rs`-scoped count "7"; SPEC `:755` binds the gate to `json/generated.rs == 0`, live crate-wide = 15** (template `json_sink_direct.rs:7` + test `lib.rs:565:1` referencing the OLD symbol) — the unscoped gate over-asserts | **REVISE** — Ω-D author: P5 gate → `grep -c parse_w11_1_number json/generated.rs == 0` (today 7) to match SPEC `:755`. |
| 5 | master-plan-diff Diff 3 | §25 implementation-order re-key; old-side `:1415-1422` byte-exact; monotonic skinny→totality restored | **ACCEPT** |
| 6 | master-plan-diff Diff 4 | §24 carry-ledger re-key + 4 SK-V19 tee-up rows; old-side carry-row at `:1346` (V2 `:1349-1352` mis-range redressed); the 3 SK-V19 totality leaks (a/b/c) + BBNF-self row cite `strategy.rs`/`css_types.rs`/`simd-scan` + `3B:177`, all resolve | **ACCEPT** (V2 REVISE redressed) |
| 7 | master-plan-diff Diff 5/6 | §5 F.W5 `:519` + §13.5 CSS verdict + §13 H-row label alignment (H.W1 `:642`/H.W4 `:646`/Lock-10 `:616`/§13 preamble `:584-592`); label-only; the CSS-verdict caveat (loadavg 4.35, H1 re-lock pending) carried | **ACCEPT** |
| 8 | master-plan-diff §8 (application order) | CRUD application order + the "≈70 doc lines / Diff 2 = 67 added lines" sizing note. **DEFECT: the Diff 2 block (129–204, pure insertion, no `+++` header) is 76 added `+`-lines, not 67 — the §8 self-count undercounts by 9.** The "no 4-7× expansion" argument survives at 76 (12 wave rows render 12 lines), but the merge-bound diff's own line-count is wrong. | **REVISE** — Ω-D author: in §8 (`:356`) correct "67 added lines" → "76 added lines" (or recount); the ≈70 rendered-doc-line figure should track the 76. |
| 9 | architecture-delta.staged (CRUD-1, NEW R9 fix) | 2 `git apply`-gated hunks (§7.4 title `:1371`, §9.2 phantom strike `:1997-2000`) + 4 anchored splices (`:19`,`:1151`,`:1990`,`:1205`); `git apply --check` EXIT 0; all 4 splice anchors byte-exact; CollapsedStage stays SHAPE SLOT; net ≈+19 prose; resolves CH4-V2-R9 (ARCH leg previously had no git-apply-gated artefact) | **ACCEPT** |
| 10 | ΩA-coherence | 12 findings (OA-V10-01..12) + cohesion fixes + CRUD-1..6 plan; ARCH/LOCKS/SHA anchors resolve; correctly self-identifies OA-V10-03 (the now-flagged stale-V6 in 3F) + the net-LOC harmonization (`:221`); its own citations clean | **ACCEPT** |
| 11 | ΩB-skinny-lessons | SK-V1..V18 trajectory digest; `runtime_generator.rs`, 7-replica, REDRESS 246/247/51/53 resolve; V6 labels redressed to V10 | **ACCEPT** |
| 12 | ΩD-master-plan-reconciliation | 10 SKV18 + 4 carried delta dispositions; `MP-3B-SKV18-D01..D10` map; COH18-001/014, 3D-D08, 3F-MH-003 resolve; `:84` 4-item REDRESS + "item 246 bounds G4" correct; consistent with master-plan-diff | **ACCEPT** |
| 13 | ΩE-skinny-corpus + staged diff | 6-surface prose-resync (INDEX/WORKSPACE/HARDENING/COMPILER/BENCH/SUBSTRATE) carrying first-line anchors + a CRUD-5 re-grep mandate (NOT a `git apply` target); anchors at `83b66db42`, self-mitigating | **ACCEPT** |
| 14 | ΩF-migration-handoff | Pass-label reconcile (V6→V10; MIGRATION `:190` historical-V6 collision REAL) + 5 migration decisions + COH18-001 cross-check; `LOCKS:620` cited correctly; 4-item REDRESS set | **ACCEPT** |
| 15 | handoff-delta.staged | OP-1..OP-5 HANDOFF deltas; live anchors `:3`/`:16-19`/`:90`/`:103-105` byte-exact; 502-line claim correct; `LOCKS:620` correct; V6→V10 harmonized; 10-row blocker matrix. **DEFECT (inherited from #4): the blocker matrix `:131` + the entry-condition `:180` write `grep -c parse_w11_1_number == 0` UNSCOPED — same SPEC `json/generated.rs` file-scope drop.** | **REVISE** — handoff-delta author: scope both P5 gate occurrences to `json/generated.rs` to match SPEC `:755` (and Diff 2's corrected form). |
| 16 | migration-delta.staged | OP-1..OP-4 MIGRATION deltas; live anchors §0.0 `:30`/§0.6-V6 `:190`/§17 `:886`/§19 `:925` byte-exact; SPEC `:435`/`:571` resolve; `LOCKS:620`/`:349` correct; 4-item REDRESS set; V6→V10 harmonized. **DEFECT (inherited from #4): the P2 row (`:55`) rewrites the gate to `… bbnf-bench/src == 0` (64) + the same uncited `bin/gate.rs` "to be retired/relocated" obligation.** (Cosmetic, not REVISE: `:5` header "502/1061-line surfaces" conflates HANDOFF 502 with MIGRATION 1061; both individually correct.) | **REVISE** — migration-delta author: scope P2 gate to `nonjson_css_l4.rs == 0` (48) per SPEC `:633`; drop or wave-attribute the `bin/gate.rs` retirement. |

## REJECT Scan (none triggered)

No CH1 REJECT condition fires:
- The staged locks-diff AND the architecture-delta both APPLY (`git apply --check`
  exit 0, re-run stable).
- No REDRESS route is revived: the §13.7 gates fence AZ-IV eager / StructRegistry
  per-leaf / fact-stream / x86 / second-substrate; CH3-V1-R2 blocks G2/G4/G6 until
  the SK-V16/V17 reconcile commits (abutting items 51/53/246/247); the
  CollapsedStage clause CLEARS (does not re-open) the M5 scalar-cheaper REDRESS
  96/97/98-RETIRED prior.
- Lock-14 is NOT narrowed — amendment by ADDITION; the green-by-exclusion clause
  STRENGTHENS the gate; `FORBIDDEN_GENERIC_TOKENS` written byte-identically across
  3A-D11/3C/3B-P4/3D-D04/v+1 (the ONE token-set the corpus claims byte-identity
  for, and that claim is TRUE — distinct from the SPEC-scoped grep WIDENING the V3
  REVISE flags, which is the inverse error: the diffs LOST a SPEC scope, not
  narrowed a lock).
- No coupling introduced INTO the architecture — the un-fork reads `BackendShape`
  from the lowered program; the relocated-seam firewall + `runtime_target_rows_collapsed`
  co-gate are NECESSARY-NOT-SUFFICIENT-aware. (The P2 `bin/gate.rs` clause is a
  citation over-reach in a falsifier, not an architectural coupling — REVISE, not REJECT.)
- No uncited LOAD-BEARING architectural claim — every lock clause + master-plan
  delta carries a resolving Evidence anchor; the V3 REVISE items are
  falsifier-command precision defects, not uncited locks.

## Conclusion

CH1's load-bearing gates PASS, independently re-run: both staged diffs apply
cleanly (locks-diff + the new architecture-delta), all 22 SHAs resolve, all §H
waves resolve at the live file:line, all REDRESS references match content. The
corpus has CONVERGED on the entire cycle-V1 + cycle-V2 REVISE set — verified by
direct re-grep, zero residue. The fresh V3 yield is the citation-precision class
cycles V1/V2 graded ACCEPT and missed: two SPEC-scoped exit-gate falsifiers
rewritten as unscoped crate-wide greps (P2 widens to `bbnf-bench/src == 0` and
invents an out-of-SPEC `bin/gate.rs` retirement; P5 drops the `json/generated.rs`
file-scope the SPEC binds, pairing the scoped count "7" with an unscoped gate
that over-asserts against the template source + a test), echoed into the
handoff-delta and migration-delta; and a self-inconsistent Diff-2 added-line
count (claimed 67, actually 76) in the merge-bound master-plan-diff §8. None
blocks G-Omega on a structural ground — the diffs still apply, the §13.7 gates
still fence the rejected routes, the locks count stays 16, the 5-shape canon
holds — but all must be corrected BEFORE the CRUD merge so no over-asserting
falsifier (one that RED-blocks a wave on occurrences the certified SPEC does not
target) and no mis-counted hunk size reaches a live V1 surface.

TALLY accept=13 revise=5 reject=0
