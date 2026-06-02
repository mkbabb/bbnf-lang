# Pass Omega V10 CHALLENGE — CH1 CORRECTNESS — Cycle V4

Lens: CH1 CORRECTNESS. Does every cited file:line/SHA resolve; does every REDRESS
reference match content; does the staged `locks-diff` apply cleanly
(`git apply --check` exit 0) to live `LOCKS.md`; does the `master-plan-diff` cite
real §H waves + real SHAs.

Scope: the 6 Ω artefacts (ΩA-ΩF) + the staged diffs under
`restart/audit/totality/astral/V10/` (`locks-diff.md`, `master-plan-diff.md`,
`architecture-delta.staged.md`, `ΩE-skinny-corpus-staged-diff.md`,
`handoff-delta.staged.md`, `migration-delta.staged.md`) against the live V1
surfaces (`restart/ARCHITECTURE.md`, `MASTER-PLAN.md`, `locks/LOCKS.md`,
`MIGRATION.md`, `HANDOFF.md`) and the converged T-P1/T-P2/T-P3 evidence.
HEAD at audit `25297a7fc` (verified `git rev-parse HEAD`; the git-status snapshot
was stale, as it was in cycles V2/V3).

Verdict: **REVISE REQUIRED.** Every primary CH1 gate PASS — independently re-run,
not parroted. The V10-current corpus has CONVERGED on the ENTIRE cycle-V1 +
cycle-V2 + cycle-V3 REVISE set (the P2 crate-wide widening + uncited `bin/gate.rs`
retirement, the P5 `json/generated.rs` file-scope drop, the §8 Diff-2 line-count
miscount are ALL redressed at this snapshot — verified by direct re-grep, zero
residue). The fresh V4 adversarial pass surfaces ONE citation-correctness defect
that cycles V1/V2/V3 all graded ACCEPT and — worse — V3 itself INTRODUCED in its
own redress instruction: the P2 exit-gate falsifier is mis-attributed to SPEC
`:633` (the R14/H1 INDEPENDENT-disclosure sentence) when the actual P2 falsifier
binds at SPEC `:627`. The defect rides in TWO merge-bound surfaces
(`master-plan-diff.md:170`, `migration-delta.staged.md:55`), each carrying the
exact false prose "the certified SPEC `:633` binds the P2 gate to
`nonjson_css_l4.rs` ALONE".

On the per-op enumeration the residual yield is 2 of 16 (12.5%) — BELOW the
cycle-V1 ≥30% expectation. This is reported HONESTLY, not inflated: the corpus has
already absorbed the entire V1+V2+V3 REVISE set across three prior hardening
cycles, so the residual defect surface is genuinely near-exhausted; the only fresh
class is one mis-line-attribution-inside-a-correct-scope, propagated into two
sites. Manufacturing additional REVISE rows to hit a 30% quota would itself be a
CH1 correctness violation. The ≥30% target is a cycle-V1 calibration on a
not-yet-converged corpus; at V4 the diffs apply, the SHAs/§H-waves/REDRESS all
resolve, and 14 of 16 staged operations are citation-clean. The honest verdict is
2 REVISE, both the same fresh P2-`:633` mis-citation class.

## Primary CH1 Gate Results (independently re-run at HEAD `25297a7fc`)

| Gate | Command | Result |
|---|---|---|
| Staged locks-diff applies cleanly | `awk '/^```diff$/{f=1;next}/^```$/{f=0}f' locks-diff.md \| git apply --check -` | **EXIT 0 (CLEAN)** |
| Staged architecture-delta applies cleanly | same awk over `architecture-delta.staged.md` | **EXIT 0 (CLEAN)** |
| 16 numbered locks preserved | `grep -nE '^[0-9]+\. \*\*' LOCKS.md \| head -16` | **16** at `:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`; addendum adds no Lock 17 |
| Five BackendShape variants, no sixth | `lower/mod.rs:18-24` match arm + `cost.rs:334 [BackendShape; 5]` | **5** `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` — both anchors byte-exact |
| Two PLANNED co-gate symbols absent | `rg -c runtime_target_rows_collapsed`; `bbnf_simd_single_mask_convention` | **0 / 0** — both PLANNED, not live |
| Insertion anchor resolves | Lock-16 NEON clause `:622`; `## v+1 Governance Boundary` `:625` | **EXACT** — addendum lands `:622`→`:625`; SK-V15 (`:581-607`)/SK-V17 (`:610-622`) addenda untouched |
| Hunk math internally consistent (locks-diff) | `@@ -622,6 +622,33 @@`; 6 context + 27 added = 33 | **CONSISTENT** |
| `LOCKS:620` cursor-generality cite | `sed -n '620p' LOCKS.md` | **EXACT** — "The `G:EventGrammar` type parameter is the generality vehicle" |

## §H Wave + SHA Resolution (master-plan-diff) — all spot-checks resolve EXACTLY

§H wave anchors, re-resolved against live `restart/MASTER-PLAN.md`:
- §13.6 header `:974` = "SK-V18 Tape-Fold Adoption Receiver Block …" (Diff 1
  old-side byte-exact). **EXACT**
- §14 Tranche I `:1042` = "## 14. Tranche I - Recovery, Incremental, LSP"
  (§13.7 insert anchor). **EXACT**
- §24 header `:1336`; §24 SK-V18 tape-fold carry-row a SINGLE line at `:1346`
  (Diff 4 old-side byte-exact). **EXACT**
- MP.NW6 `:662` (single-negative-control standard) + H.W4.LOCK14 `:605`
  (PARTIAL row), both cited in the locks-diff neutrality clause. **EXACT**
- F1-F9: live §13.6 (`:976`-`:1021`) carries 9 fold-design rows F1-F9; Diff 1
  preserves all verbatim. **EXACT**

SHAs spot-checked, ALL resolve to real commits with matching descriptions:
`25297a7fc` (=live HEAD, T-P3 synthesize), `6fb812752` (T-P1 certify),
`3f6eb603d` (T-P2 research), `820798161` (S-P2), `4e4aa0648` (S-P3),
`1c5bd7a25` (SK-V16 W6-tape), `f6a38445b` (SK-V17 W4/W5 close), `66232b7c3`
(SK-V15 close, cited in Diff 1). The S-P2 token is `820798161` (correct, NOT
`820598161`).

## REDRESS References — all resolve, all match content

- locks-diff CollapsedStage clause `skinny/REDRESS.md:2795`/`:2928`-`:2933`:
  `:2795` = "## SK-V9 Wave 3 Union Event-Model Class-Column Redress"; `:2928`-`:2933`
  = the M5-Max scalar-cheaper-than-SIMD-cursor finding ("on the M5 Max wide-issue
  core, the current branch-predictable, cache-hot scalar loop does not pay"). The
  CollapsedStage clause CLEARS (does not re-open) the REDRESS 96/97/98 RETIRED
  prior. **MATCH.**
- 4-item REDRESS set 51/53/246/247: `REDRESS.md:6184` = "## SK-V14 W11T Parse-Only
  Structural Stream Reject" (item 246 anchor; "item 246 bounds G4" correct,
  master-plan-diff `:208`). Uniform corpus-wide; no bare 3-item residue. **MATCH.**
- locks-diff single-substrate clause `3B:177` = the `MP.SK19.SCANNER-UNIFY` row;
  the `simd-scan` lib re-export at `crates/simd-scan/src/lib.rs:67`
  (`pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut}`) vs
  `:68` probe API (`{StructuralIndex, next_structural_at_or_after}`) both byte-exact.
  **MATCH.**

## Cycle-V1 + V2 + V3 REVISE Set — independently re-checked, ALL REDRESSED

Every prior REVISE item is corrected at THIS snapshot — re-ran each, not assumed:
- **V3-R4a — P2 crate-wide widening + uncited `bin/gate.rs` retirement.**
  master-plan-diff `:170` now scopes the P2 gate to `nonjson_css_l4.rs == 0`
  (today 48; live count re-grepped = **48**, exact) and EXPLICITLY records "the 16
  crate-wide hits in `bin/gate.rs` are NOT a P2 gate target, no SPEC/1D/3B wave
  owns their retirement" (live `bin/gate.rs` count re-grepped = **16**, exact).
  The uncited "must be retired/relocated to reach 0" obligation is GONE.
  **REDRESSED.**
- **V3-R4b / R15 / R16 — P5 `json/generated.rs` file-scope drop.**
  master-plan-diff `:173`, handoff-delta `:131`/`:181`, migration-delta all now
  scope to `grep -c parse_w11_1_number json/generated.rs == 0` (today 7; live =
  **7**, exact). The crate-wide 15 is disclosed as "+ 7 template-source
  `json_sink_direct.rs` + 1 `lib.rs:565` test-assert" — re-grepped crate-wide =
  **15** exactly (`json/generated.rs:7` + `codegen/json_sink_direct.rs:7` +
  `codegen/lib.rs:565:1`, the `assert!(json_generated.contains(
  "parse_w11_1_number_array_direct"))`). **REDRESSED.**
- **V3-R8 — §8 self-inconsistent Diff-2 line count.** §8 (`:361`) now reads "the
  Diff 2 hunk is 81 added lines"; the Diff 2 fence (lines 129-209, a pure
  insertion, no `+++` header) carries **81** `+`-prefixed content lines (re-counted
  `awk '… /^\+/' | wc -l` = 81). The ≈81 rendered-doc-line figure tracks. The block
  grew +5 since V3's 76 and the count is now self-consistent. **REDRESSED.**

## Fresh V4 Adversarial Yield — P2 falsifier mis-attributed to SPEC `:633`

While the V3 P2-SCOPE redress is correct (the gate now points at
`nonjson_css_l4.rs`, not the crate-wide tree), the redress INTRODUCED a new
file:line citation defect that the CH1 lens exists to catch. The merge-bound
prose now asserts the WRONG SPEC line as the gate's binding authority:

- **master-plan-diff MP.SK18.P2 (`:170`) + migration-delta P2 (`:55`)** both read
  "**the certified SPEC `:633` binds the P2 gate to `nonjson_css_l4.rs` ALONE**".
  Re-resolved against live `restart/skinny/tranches/sk-v18/SPEC.md`:
  - SPEC `:633` = "lazy-rich-vs-eager-CSSOM framing is disclosed at H1.
    INDEPENDENT." — this is the §3.2 binding-sequencing note stating R14/H1
    disclosure is NOT P2's job. It binds NOTHING about the P2 gate.
  - The ACTUAL P2 exit-gate falsifier is **SPEC `:627`**: "Exit-gate falsifier:
    `grep -c 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` == 0
    (today 48)". The §3.2 P2 section header is `:608`; the owner-path scoping
    ("`bbnf-bench/src/nonjson_css_l4.rs`") is `:614`. The single-file scope the
    prose CORRECTLY asserts is in fact carried by `:614`/`:627`, not `:633`.
  The `sk-v18/SPEC.md:608-633` RANGE in the master-plan-diff SOURCE column is
  defensible (it spans the whole §3.2 P2 section). But the inline prose claim
  "SPEC `:633` BINDS the P2 gate" cites the one line in that range that does the
  OPPOSITE — it disclaims P2 ownership of the H1 framing. A reviewer resolving
  `:633` to verify the gate lands on the wrong sentence. This is the precise CH1
  failure mode (a load-bearing file:line that does not resolve to the claimed
  content), and it is doubly notable because V3's own redress instruction
  (V3 CH1 `:137`, `:196`) told the author to "scope P2 gate to `nonjson_css_l4.rs
  == 0` (48) per SPEC `:633`" — V3 PARROTED the bad line, the author obeyed, and
  the bad line propagated into two merge-bound surfaces.

## Live-Surface Evidence Spot-Checks (independently resolved)

- LOCKS: `:620` generality-vehicle phrase; `:622`/`:625` insertion anchors;
  `:610-622` SK-V17 addendum; `:581-607` SK-V15 addendum (re-resolved exact).
- SIMD multi-pack census (locks-diff one-movemask clause, the grown V10 content):
  `byte_class_from_eq_set_64.rs:79`-`87` (local `movemask_u8x16`, `vaddv_u8`
  shift-add), `bracket_depth_mask_64.rs:74` (local `movemask_u8x16`, same body),
  `comment_body_mask_64.rs:68` (local `movemask_u8x16`, same body), and the fourth
  non-delegating SHRN duplicate `match_tiny_plain_string.rs:100`
  (`vshrn_n_u16::<4>`) all resolve byte-exact; the ONE canonical pack
  `aarch64/movemask.rs:5` (`vshrn_n_u16::<4>`) resolves. The "≥4 non-delegating
  packs at G2 entry" census claim is ACCURATE.
- ARCH (architecture-delta anchors): `:1371` "### 7.4 SK-V5 Through SK-V15
  Implementation Status" (Gated Hunk 1 old-side byte-exact); `:1997` the
  `G:EventGrammar` phantom-strike old-side; `:1206` CollapsedStage x86-pinned
  ledger row (`target.arch == x86 + target.avx512bw`); `:1289` U3 directive
  "CollapsedStage = UNKNOWN-2D-05, no admission without a 2E source-backed
  strategy" (the locks-diff C9 PRIMARY demote-stable anchor, NOT in the OA-V10-05
  splice set). The OA-V10-05 HALT-NOTE coordination is internally consistent.
- HANDOFF: live anchors `:3` ("## Current Totality Override - 2026-05-30"), `:90`
  ("## Pass Omega V5 SK-V17 tape-fold dispatch directive"); 502-line claim exact
  (`wc -l` = 502).
- MIGRATION: `:30` ("## 0.0 Current SK-V17 Tape-Fold Migration Receiver"), `:190`
  ("## 0.6 Historical Pass Omega V6 W5BR Migration Receiver"); 1061-line claim
  exact (`wc -l` = 1061).
- Totality-tree SK-V19 leaks (Diff 4): `crates/ir/src/registry/strategy.rs:137`-`185`
  registers all 9 grammars (Bbnf/Bnf/CssL4/CssPretty/Csv/Ebnf/GoogleSheets/Json/
  Math); `crates/core/src/css_types.rs:1` exists; x86 tree `find …/x86_64 …/ext/x86
  -type f` = **28** (exact).

## Enumerated Staged Amendments / CRUD Operations Under CH1

| # | Artefact | Staged amendment / CRUD op | Verdict |
|---|---|---|---|
| 1 | locks-diff (ΩC) | 11-clause SK-V18 T-P3 v+1 Crystallisation Addendum inserted `:622`→`:625`; `git apply --check` EXIT 0; 16 locks / 5 shapes preserved; 2 PLANNED co-gates absent; REDRESS 2795/2928-2933 + 3B:177 + MP.NW6:662 + H.W4.LOCK14:605 + SPEC §6:358-390 all resolve; `LOCKS:620` cited correctly; the ≥4-pack SIMD census byte-exact | **ACCEPT** |
| 2 | ΩC-locks-amendments | 9A/11M/0R/1D disposition resolves against 3C; verification commands resolve; the "hardened consolidation … no clause REVERSED" provenance with md5-differs disclosure stands | **ACCEPT** (V2 REVISE stays redressed) |
| 3 | master-plan-diff Diff 1 | Re-key §13.6 SK-V18 Tape-Fold → SK-V19 Totality-Fold; header `:974` old-side byte-exact; F1-F9 preserved verbatim; SHAs `66232b7c3`/`f6a38445b` resolve | **ACCEPT** |
| 4 | master-plan-diff Diff 2 (block, structure) | NEW §13.7 GENERALIZATION 12-wave block, insert before §14 `:1042`; wave manifest matches SPEC `:431-447`; 4-item REDRESS + V10 label correct; 12 wave rows render 12 lines; 81 added lines | **ACCEPT** (structure/anchors clean) |
| 4a | master-plan-diff Diff 2 — MP.SK18.P2 row (`:170`) | **DEFECT: P2 scope is now CORRECT (`nonjson_css_l4.rs == 0`, 48 — re-grepped exact) but the prose mis-attributes the BINDING line: "the certified SPEC `:633` binds the P2 gate to `nonjson_css_l4.rs` ALONE". SPEC `:633` is the R14/H1 "INDEPENDENT" disclosure note; the ACTUAL P2 exit-gate falsifier binds at SPEC `:627` (header `:608`, owner-paths `:614`).** A reviewer resolving `:633` lands on a sentence that disclaims P2 ownership of H1 framing | **REVISE** — Ω-D author: change "SPEC `:633` binds the P2 gate" → "SPEC `:627` binds the P2 gate" (header `:608`, owner-paths `:614`). The source-column range `:608-633` may stay. |
| 4b | master-plan-diff Diff 2 — MP.SK18.P5 row (`:173`) | P5 scope CORRECT (`json/generated.rs == 0`, 7) and SPEC `:755`/`:570` BOTH resolve to the real P5 falsifier + count lines; crate-wide 15 disclosed exactly | **ACCEPT** (V3 REVISE stays redressed) |
| 5 | master-plan-diff Diff 3 | §25 implementation-order re-key | **ACCEPT** |
| 6 | master-plan-diff Diff 4 | §24 carry-ledger re-key + 4 SK-V19 tee-up rows; old-side carry-row at `:1346` byte-exact; the 3 SK-V19 totality leaks (a) `strategy.rs:137-185` 9-grammar / (b) `css_types.rs:1` / (c) `simd-scan/src/lib.rs:67` all resolve; `3B:177` resolves | **ACCEPT** |
| 7 | master-plan-diff Diff 5/6 | §5 F.W5 + §13.5 CSS verdict + §13 H-row label alignment (H.W4 `:605`/MP.NW6 `:662`); label-only | **ACCEPT** |
| 8 | master-plan-diff §8 (application order) | CRUD application order + the "Diff 2 = 81 added lines" sizing note. **The §8 self-count now MATCHES (re-counted 81 `+`-lines exactly; V3's 67→76→now-81 progression converges).** | **ACCEPT** (V3 REVISE redressed) |
| 9 | architecture-delta.staged (CRUD-1) | 2 `git apply`-gated hunks (§7.4 title `:1371`, §9.2 phantom strike `:1997`) + anchored splices; `git apply --check` EXIT 0; CollapsedStage stays SHAPE SLOT; OA-V10-05 `:1206` HALT-NOTE coordination internally consistent with the locks-diff C9 PRIMARY `:1289` anchor | **ACCEPT** |
| 10 | ΩA-coherence | 12 findings (OA-V10-01..12) + CRUD-1..6 plan; ARCH/LOCKS/SHA anchors resolve; OA-V10-03 stale-V6 self-flag + net-LOC harmonization; its own citations clean | **ACCEPT** |
| 11 | ΩB-skinny-lessons | SK-V1..V18 trajectory digest; `runtime_generator.rs`, REDRESS 246/247/51/53 resolve; V6 labels redressed to V10 | **ACCEPT** |
| 12 | ΩD-master-plan-reconciliation | 10 SKV18 + 4 carried delta dispositions; `MP-3B-SKV18-D01..D10` map; "item 246 bounds G4" + 4-item REDRESS correct; consistent with master-plan-diff | **ACCEPT** |
| 13 | ΩE-skinny-corpus + staged diff | 6-surface prose-resync carrying first-line anchors + a CRUD-5 re-grep mandate (NOT a `git apply` target); self-mitigating | **ACCEPT** |
| 14 | ΩF-migration-handoff | Pass-label reconcile (V6→V10) + 5 migration decisions + COH18-001 cross-check; `LOCKS:620` cited correctly; 4-item REDRESS set | **ACCEPT** |
| 15 | handoff-delta.staged | OP-1..OP-5 HANDOFF deltas; live anchors `:3`/`:90` byte-exact; 502-line claim correct; `LOCKS:620` correct; the P5 blocker-matrix `:131` + entry-condition `:181` now scope to `json/generated.rs` (V3 REVISE redressed) | **ACCEPT** |
| 16 | migration-delta.staged | OP-1..OP-4 MIGRATION deltas; live anchors §0.0 `:30`/§0.6-V6 `:190` byte-exact; 1061-line claim correct; `LOCKS:620` correct; 4-item REDRESS set. **DEFECT (inherited from #4a): the P2 row (`:55`) carries the SAME false "SPEC `:633` binds the P2 gate" attribution; scope is correct (`nonjson_css_l4.rs`, 48) but the binding line should be `:627`.** | **REVISE** — migration-delta author: change "SPEC `:633` binds the P2 gate" → "SPEC `:627` binds the P2 gate". |

## REJECT Scan (none triggered)

No CH1 REJECT condition fires:
- The staged locks-diff AND the architecture-delta both APPLY (`git apply --check`
  exit 0, re-run stable).
- No REDRESS route is revived: the §13.7 gates fence AZ-IV eager / StructRegistry
  per-leaf / fact-stream / x86 / second-substrate; CH3 blocks G2/G4/G6 until the
  SK-V16/V17 reconcile commits (abutting items 51/53/246/247); the CollapsedStage
  clause CLEARS (does not re-open) the M5 scalar-cheaper REDRESS 96/97/98-RETIRED
  prior.
- Lock-14 is NOT narrowed — amendment by ADDITION; the green-by-exclusion clause
  STRENGTHENS the gate; `FORBIDDEN_GENERIC_TOKENS` written byte-identically across
  3A-D11/3C/3B-P4/3D-D04/v+1. The V4 REVISE is the INVERSE of a lock narrowing:
  the diffs cite the wrong SPEC LINE for a correctly-scoped falsifier (a citation
  precision defect), not a relaxed gate.
- No coupling introduced INTO the architecture — the un-fork reads `BackendShape`
  from the lowered program; the relocated-seam firewall + `runtime_target_rows_collapsed`
  co-gate are NECESSARY-NOT-SUFFICIENT-aware. The P2 `:633`-vs-`:627` defect is a
  citation over-reach in a falsifier's authority line, not an architectural
  coupling — REVISE, not REJECT.
- No uncited LOAD-BEARING architectural claim — every lock clause + master-plan
  delta carries a resolving Evidence anchor; the V4 REVISE item is a
  falsifier-line mis-attribution, not an uncited lock.

## Conclusion

CH1's load-bearing gates PASS, independently re-run: both staged diffs apply
cleanly (locks-diff + architecture-delta), the spot-checked SHAs resolve, the §H
waves resolve at the live file:line, all REDRESS references match content, the
≥4-pack SIMD census is byte-exact, and the totality-tree SK-V19 leak anchors
(strategy.rs 9-grammar / css_types.rs / simd-scan probe API) all resolve. The
corpus has CONVERGED on the entire cycle-V1 + V2 + V3 REVISE set — verified by
direct re-grep, zero residue (P2 scope, P5 scope, §8 line-count all corrected).
The fresh V4 yield is one citation-correctness defect cycles V1/V2/V3 missed and
that V3 itself parroted into the author's redress instruction: the P2 exit-gate
falsifier is attributed to SPEC `:633` (the R14/H1 INDEPENDENT-disclosure note)
when it binds at SPEC `:627` (header `:608`, owner-paths `:614`), riding in two
merge-bound surfaces (master-plan-diff Diff 2 P2 row + migration-delta P2 row).
None blocks G-Omega on a structural ground — the diffs still apply, the §13.7
gates still fence the rejected routes, the locks count stays 16, the 5-shape canon
holds, and the P2 gate COMMAND + SCOPE are correct (only its cited SPEC line is
wrong) — but the mis-attributed binding line must be corrected to `:627` BEFORE
the CRUD merge so no merge-bound V1 surface points a reader at a sentence that
disclaims the very gate it claims to bind.

TALLY accept=14 revise=2 reject=0
