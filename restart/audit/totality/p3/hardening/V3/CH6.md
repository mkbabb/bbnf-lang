# CH6 ANTI-PAPER-CLOSE — SK-V18 T-P3 cycle V3

Verdict: REVISE

The packet has folded all V1 blockers and they remain folded: the load-bearing
v+1 diff — the G-Omega gate object — APPLIES (`git apply --check` AND `--recount`
both exit 0); the 3C disposition tally matches the matrix it describes (9 ACCEPT /
11 MODIFY / 0 REJECT / 1 DEFER, column-verified 21/21, zero silent drops); the
lone 3C DEFER (LAC-2F-V3-03) names a concrete re-entry trigger (`ls` both scan
trees in the evidence header); 3F's next-cycle directive carries concrete
measurable entry conditions; and every LIVE deferral across 3B/3C/3D/3F/the two V4
tables routes to a receiver/blocker/gate on the certified SK-V18 manifest. No
standalone `validated`-close survives; the 13 T-P2 refutations appear only as
REJECT gates / REDRESS fences, never as proposed deltas; no V1 surface is edited.
CH6 cannot ACCEPT because the SAME two carried-HISTORICAL surfaces the V2 fold
flagged (3A:143-151, 3E:207-215) are STILL UNREPAIRED in V3: each retains an
Open-Questions table whose row cells literally name retired SK-V15 waves
(W1/W3/W4/W5/W6/W8/W9/W10 in 3A; W1/W2/W4/W5/W6/W7/W8/W9 in 3E) as receiver+gate,
with the SK-V18 re-key supplied only out-of-band in the preamble. A reader
cost-routing one row in isolation lands on a gate the SK-V18 plan does not contain.
This is a tighten-not-reject REVISE — the rows are demarcated NON-live and a fully
live SK-V18 V4 table exists beside each, so it is NOT an engineered close — but the
row-level routing is not yet self-contained, and 3B already exemplifies the
correct fix (it REMOVED its consumed SK-V15-routing deltas outright rather than
retaining them as a re-keyed historical table). The V2 fold left this surface; the
V3 fold did not address it.

## Evidence Commands And Outputs

```sh
# GATE OBJECT APPLIES (V1's REJECT stays cleared):
$ awk '/^```diff$/{d=1;next} d&&/^```$/{exit} d{print}' \
    restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v3.diff
$ git apply --check /tmp/tp3-locks-v3.diff           ; echo exit=$?   # exit=0
$ git apply --check --recount /tmp/tp3-locks-v3.diff ; echo exit=$?   # exit=0
# hunk header @@ -622,6 +622,33 @@ ; LOCKS.md:622=Lock16 :623/:624=2 blanks
# :625=## v+1 Governance Boundary  (verified on-disk)

$ grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md                    # 16 (intact)

# 3C disposition column over the 21 matrix rows (robust 5th-col extract):
9 ACCEPT   11 MODIFY   0 REJECT   1 DEFER   (sum=21, 0 silent drops)
# 3C Exec Summary line 47 reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"  (matches)

# SK-V18 manifest = P1..P5 / G1..G6 / PROVE / H1 / W-PRUNE (NO W1/W4..W10):
$ grep -cE '\bW(1|4|5|6|7|8|9|10)\b' restart/skinny/tranches/sk-v18/SPEC.md   # 0

# PLANNED co-gate symbols rg=0 live (not laundered into met gates):
$ rg -l runtime_target_rows_collapsed skinny/crates skinny/xtask | wc -l      # 0
$ rg -l bbnf_simd_single_mask_convention skinny/crates | wc -l                # 0
$ rg -l lock14_gate_scans_codegen skinny/crates | wc -l                       # 0
$ rg -l parse_w11_1_number skinny/crates | wc -l                              # 3
#   parse_w11_1_number STILL LIVE -> 3F entry-cond grep==0 is a genuine future
#   gate, not pre-met.

# standalone close-word scan across all six artefacts:
$ rg -n '\bvalidated\b' restart/audit/totality/p3/3{A,B,C,D,E,F}*.md          # empty
```

```sh
# CARRY-OVER DEFECT (the only surviving CH6-class surface):
# 3A carried HISTORICAL table (143-151) row-cell W# refs:
$ awk 'NR>=143&&NR<=151' .../3A-architecture-synthesis.md | grep -oE 'W[0-9]+' | sort -u
W1 W10 W3 W4 W5 W6 W8 W9          # named as receiver+gate IN THE CELLS
#  inline SK-V18 re-key in those cells: essentially none (lone P2 is question text)
# 3E carried HISTORICAL table (207-215) row-cell W# refs:
$ awk 'NR>=207&&NR<=215' .../3E-grammar-generalisation.md | grep -oE 'W[0-9]+' | sort -u
W1 W2 W4 W5 W6 W7 W8 W9           # named as receiver+gate IN THE CELLS
# BOTH have a re-key PREAMBLE above the table (3A:132-141, 3E:197-205) and a
# separate LIVE "## V4 Open Questions" table (3A:262-271, 3E:374-383).
# => identical to V2's CH6-V2-01/02; NOT folded in V3.
```

Load-bearing finding-ids spot-verified to resolve on-disk: `1E:147` (LAC-1E-V5-01
named-primitive (a)-(d)), `1E:148` (LAC-1E-V5-02 relocated-seam — itself now
carries the PLANNED-symbol caveat inline), `1A:180` (1A-LOCK1-AMEND-001 phantom
`<G>` strike), `2C:380` (LAC-2C-SK18-01 FORCED-demotion), `2F:196` (LAC-2F-V3-03
DEFER with its `ls`-both-trees re-entry trigger). Cited LOCKS sections resolve:
`:349` (Lock 14 generic-crate self-gate), `:620` (generality-vehicle clause),
`:622` (SK-V17 Lock 16 NEON-classifier-manifest clause), `:625` (`## v+1
Governance Boundary`). The 21-candidate set (7×1E + 1×1A + 13×2X) resolves and 3C
disposes all 21 exactly once.

## Enumerated Deltas / Dispositions Under CH6

| # | artefact / object | judgement | basis |
|---|---|---|---|
| 1 | 3C v+1 diff — applies to live LOCKS.md (the G-Omega gate object) | **ACCEPT** | `git apply --check` AND `--recount` both exit 0; header `@@ -622,6 +622,33 @@`, leading context carries BOTH blanks at :623/:624. My lens's mandated spot-verify ("the v+1 diff applies") PASSES. |
| 2 | 3C Executive Summary disposition tally (line 47) | **ACCEPT** | Reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"; the disposition column over 21 matrix rows tallies exactly 9/11/0/1. Headline matches the matrix it describes (V1's CH6-V1-02 transposition stays fixed). |
| 3 | 3C disposition completeness (21/21, 0 silent drops) | **ACCEPT** | Each of the 21 candidates (LAC-1E-V5-01..07, 1A-LOCK1-AMEND-001, 2C-SK18-01..03, 2D-V3-01..04, 2E-V6-01..03, 2F-V3-01..03) dispositioned exactly once; no silent drop (the CH6-REJECT trigger does not fire). |
| 4 | 3C DEFER (LAC-2F-V3-03) re-entry trigger | **ACCEPT** | Names a concrete re-entry trigger ("any 2F-class re-audit citing a 'balanced-scan gap' must `ls` both `parse-that/.../scan/` and `skinny/crates/bbnf-simd/src/` in its evidence header"); folded into D-SKV18-L16-single-substrate-movemask as an audit-scope note, not dropped. `2F:196` resolves to exactly this. Satisfies my lens's DEFER clause. |
| 5 | 3C two PLANNED co-gate symbols (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) | **ACCEPT** | Both `rg`=0 live (verified); disclosed as PLANNED-at-SK-V18-SPEC in the matrix, never cited as live symbols. The 1E:148 evidence row itself carries the PLANNED caveat. No laundering of a planned gate into a met one. |
| 6 | 3C Open Questions (3 rows incl. CH6 named-primitive row) | **ACCEPT** | Full receiver/blocker/gate triad; the CH6 row REFUSES to treat the named-primitive (a)-(d) gate as already-satisfied by the SK-V17 Lock-16 NEON clause (`LOCKS.md:622`) — "treating it as satisfied is a paper close" — and binds a `grep`=0 falsifier. Direct anti-paper-close discipline. |
| 7 | 3A SK-V18 extension deltas (ARCH-3A-V4-SK18-D01..D14) | **ACCEPT** | 14 deltas, each cites a resolving 1A-1F / 2C-2F finding-id and an SK-V18 wave (P1-P5/G1-G6/PROVE/SK-V19); D14 self-flags "a routing note... NOT a closure." No prose-close. |
| 8 | 3A carried-verbatim V3 packet (ARCH-3A-V1-D01..D12) | **ACCEPT** | Frontmatter discloses SK-V15 authorship, "retained as the historical V3 synthesis record"; several already applied by intervening Pass Omega CRUD; not asserted as a current-cycle close. |
| 9 | 3A LIVE "V4 Open Questions" table (262-271) | **ACCEPT** | All 7 rows carry receiver/blocker/gate on the SK-V18 manifest (Pass-Omega-ARCH-CRUD / G4 / W-PROVE / SK-V19) with concrete SK-V18 falsifiers (`value_ref_grammar_param_deleted`, `verbatim_blob_present==false`). Genuine live deferral set; not a paper-close. |
| 10 | 3A carried HISTORICAL Open-Questions table (143-151) + re-key preamble | **REVISE** | The preamble (132-141) demarcates the rows NON-live and supplies a W#→SK-V18 re-key, so the deferral is NOT an engineered close — but the 7 row cells STILL name W1/W3/W4/W5/W6/W8/W9/W10 as receiver+gate, and the re-key lives only out-of-band. A reader cost-routing one row in isolation lands on a gate absent from the certified SK-V18 12-wave manifest. Identical to V2's unfolded CH6-V2-01. CORRECTION (3A author): inline the per-row SK-V18 re-key INTO each receiver/gate cell (W1→P2/§7.4, W3/W5/W6→G1/G2/G3, W4→P5/SK-V19, W8/W9→G4/G5, W10→Lock-16 FNV quarantine) OR collapse the historical table to a one-line pointer to the V4 table, mirroring 3B's outright removal of its consumed SK-V15 deltas. |
| 11 | 3E SK-V18 extension deltas (3E-D12..D18) | **ACCEPT** | SK-V18-grounded deltas citing 2C SK-V18 ids + 1E V5 ids; CSS narrative re-folded onto G2 (`SPEC:439`) / H1; scopes fleet wording to witnessed grammars with PROVE/SK-V19 as fleet receiver. Anti-overclaim discipline intact. |
| 12 | 3E LIVE "V4 Open Questions" table (374-383) | **ACCEPT** | Rows carry receiver/blocker/gate on the SK-V18 manifest (3C-LOCKS-diff / PROVE / G2-G6 / SK-V19 / H1) with concrete SK-V18 commands + falsifiers. The CH6 row binds (d) PROFILE-PROVEN-NARROW-LEAF to the H1 re-captured profile — refuses crediting a checkasm PASS as a speedup close. |
| 13 | 3E carried HISTORICAL Open-Questions table (207-215) + re-key preamble | **REVISE** | Same shape as #10: the preamble (197-205) demarcates NON-live and re-keys W2/W5/W6/W7, but the 7 row cells still name W1/W2/W4/W5/W6/W7/W8/W9 as receiver+gate. A live SK-V18 V4 table exists at 374-383; the historical rows are not self-contained. Identical to V2's unfolded CH6-V2-02. CORRECTION (3E author): inline the per-row re-key (W2→P4, W5/W6→G2, W7→G3∧PROVE∧SK-V19) into the receiver/gate cells OR reduce the historical table to a pointer to the V4 table. |
| 14 | 3B Open Questions (5 rows) + §13.7 NEW-wave consumers | **ACCEPT** | All rows carry receiver/blocker/gate keyed to SK-V18/SK-V19 (Pass Omega / SK-V19 entry / §13.7 G2/G4/G6 / H1). The SK-V15-routing deltas (MP-3B-V1-D03..D08/D11) were REMOVED outright as "consumed by the now-landed §13.5/§13.6 text," replaced with §13.7-routed MP-3B-SKV18-D01..D10. Cleanest of the carried-table artefacts — no live deferral routes to a retired W#; the CH6 row binds "a Sheets `N` blocks the generalization claim, never paper-closed." |
| 15 | 3D Open Questions (rows incl. CH6) + §6 (a)-(d) machine-checked bundle | **ACCEPT** | Full receiver/blocker/gate triad; SK-V18-keyed; the CH6 row demands the §6 (a)-(d) bundle "machine-checked, not prose" and 3D-D11 states "the four predicates are machine-checked, not prose." T-P3-proposes-only honoured. |
| 16 | 3F next-cycle dispatch directive (Step 6) | **ACCEPT** | Lists concrete measurable entry conditions (`x86_tree_deleted==true`, `runtime_target_rows_collapsed==true`, `lock14_gate_scans_codegen==true`, `grep -c parse_w11_1_number==0`); Step 4 records CRUD blocked/extension remainder with receiver/blocker/gate; Step 8's five SK-V19 carriers are each cited (COH18-005/007/015, 3F-MH-013, 1A-LOCK1-AMEND-001), "none silently dropped." |
| 17 | 3F Open Questions (4 rows) | **ACCEPT** | Full receiver/blocker/gate triad; the CH3 row BLOCKS G2/G4/G6 entry until the SK-V16/V17 reconcile is on the committed ledger "NOT deferred to SK-V19 entry" (anti-engineered-deferral); the CH4/CH7 row forbids crediting an un-caveated CSS "MEASUREMENT-VALID" closure before the H1 `css_canon_bench` re-lock. |
| 18 | T-P1/T-P2 governance honesty across the packet | **ACCEPT** | 3F carries T-P1 as IN-CYCLE / near-converged NON-normal-§3Z (not the SK-V15 V5 prior-cycle record) and the CSS ratios as DIRECTIONAL not re-locked (U-4); never laundered to a normal two-clean §3Z close. G3 auto-pass on cohort lock under the active pin; G-Omega the only mandatory user gate. |
| 19 | Refuted-route non-revival (13 T-P2 refutations) | **ACCEPT** | tree-walk, wire-as-is `find_css_significant`, neutral-name-on-one-grammar, checkasm-as-speedup, x86/AVX-512 close, bracket_depth_mask appear ONLY as REJECT gates / REDRESS-fences / forced-demotion obligations (3A-D10 forced CSS-scoped name, 3D-D11 tree-walk REJECT, 3A-D09 retarget-not-author), never as proposed deltas. |
| 20 | Cross-scope discipline (proposal-only, no V1-surface edit) | **ACCEPT** | All six artefacts proposal-only; the v+1 diff is gated behind the `## v+1 Governance Boundary`; 3A D14 self-flags a routing note; no sixth shape / new directive / new substrate / lock retirement; LOCKS numbered-lock count = 16. |

## Findings (repair directives)

| id | severity | target lines | finding | repair | owner |
|---|---|---|---|---|---|
| CH6-V3-01 | MEDIUM | `restart/audit/totality/p3/3A-architecture-synthesis.md:143`-`151` | The carried HISTORICAL Open-Questions table is correctly demarcated NON-live with a W#→SK-V18 re-key PREAMBLE (132-141), but each of the 7 row cells still names a retired SK-V15 wave (W1/W3/W4/W5/W6/W8/W9/W10) as receiver+gate; the re-key is out-of-band, so a row read in isolation routes to a gate absent from the certified SK-V18 12-wave manifest. This is the SAME surface V2's CH6-V2-01 flagged; the V3 fold did not repair it. Not an engineered close (rows flagged historical; a fully live SK-V18 V4 table exists at 262-271), but the row-level routing is not self-contained. | Inline the per-row SK-V18 re-key into each receiver/gate cell (W1→P2/§7.4, W3/W5/W6→G1/G2/G3, W4→P5/SK-V19, W8/W9→G4/G5, W10→Lock-16 FNV quarantine) OR collapse the historical table to a one-line pointer to the V4 Open Questions table — mirroring 3B, which REMOVED its consumed SK-V15-routing deltas outright. No row may name a retired W# without its SK-V18 receiver in the same cell. | 3A author (V4 fold). |
| CH6-V3-02 | MEDIUM | `restart/audit/totality/p3/3E-grammar-generalisation.md:207`-`215` | Identical pattern (V2's CH6-V2-02, also unfolded): the carried HISTORICAL table is demarcated NON-live with a re-key preamble (197-205), but the 7 row cells still name W1/W2/W4/W5/W6/W7/W8/W9 as receiver+gate. A live SK-V18 V4 table exists at 374-383; the historical rows are not self-contained. | Inline the per-row re-key (W2→P4, W5/W6→G2, W7→G3∧PROVE∧SK-V19) into the receiver/gate cells OR reduce the historical table to a pointer to the V4 table. | 3E author (V4 fold). |

## Non-Findings Checked

- No uncited "validated"/"verified"/"proven" close: the standalone-`validated`
  scan across all six artefacts is empty; every "proven"/"verified" carries an
  adjacent path:line or a falsifier-command.
- No silent-dropped candidate: 3C disposes all 21 (7×1E + 1×1A + 13×2X), one
  disposition each (column tally 9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER); the
  lone DEFER carries a concrete re-entry trigger and is folded as an audit-scope
  note. 3F Step 8's five SK-V19 carriers are each cited, none dropped.
- No engineered deferral on any LIVE row: every live deferral (3B/3C/3D/3F + the
  two V4 tables) routes to a receiver/blocker/gate on the SK-V18 manifest; the CH3
  3F/3B rows REFUSE to push the SK-V16/V17 reconcile to SK-V19 entry, blocking
  G2/G4/G6 during SK-V18 instead.
- No revived refuted-route: the 13 T-P2 refutations appear only as REJECT gates /
  REDRESS-fences / forced-demotion obligations, never as proposed deltas.
- No cross-scope violation: all six artefacts are proposal-only; the v+1 diff is
  gated behind the `## v+1 Governance Boundary`; no sixth shape / new directive /
  new substrate / lock retirement; numbered-lock count = 16.
- No planned-gate laundering: `runtime_target_rows_collapsed`,
  `bbnf_simd_single_mask_convention`, `lock14_gate_scans_codegen` all `rg`=0 live;
  `parse_w11_1_number` is still live (3 files), so 3F's `grep -c ... == 0` entry
  condition is a genuine future gate, not pre-met.
- No G-Omega gate-object corruption: the v+1 diff applies cleanly (`--check` and
  `--recount` exit 0); the cited LOCKS sections (:349/:620/:622/:625) all resolve.
- No G3/G-Omega confusion: 3F carries G3 auto-pass on cohort lock under the active
  pin with G-Omega as the only mandatory user gate.

## Residual Risk

This CH6 pass re-ran its mandated spot-verifications (the v+1 diff apply, the 3C
tally and 21/21 completeness, the load-bearing finding-id + LOCKS-section
resolution, the planned-gate `rg`=0, the refuted-route non-revival, the SK-V18
manifest membership of every LIVE receiving gate, the standalone-close scan) and
did not re-run CH1's full citation-resolution matrix nor CH5's hidden-coupling
sweep. The two REVISE findings are cosmetic-routing tightenings on
demarcated-historical tables, not deferral-integrity breaches; the deferral
SUBSTANCE of the packet is sound and the V1 blockers stay folded. The single
CH6-class surface that survived BOTH the V1 and V2 folds is the carried-historical
row-routing in 3A and 3E — 3B already demonstrates the correct fix (outright
removal of consumed SK-V15-routing deltas). Because the verdict is REVISE, V3 must
not be counted as a clean hardening cycle; a V4 fold must self-contain the 3A/3E
historical row cells (inline re-key or collapse-to-pointer) before lock.

TALLY accept=18 revise=2 reject=0
