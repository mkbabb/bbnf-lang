# CH6 ANTI-PAPER-CLOSE — SK-V18 T-P3 cycle V5

Verdict: REVISE

The structural floor of the packet remains sound under my lens. The load-bearing
v+1 diff — the G-Omega gate object — APPLIES (`git apply --check` AND `--recount`
both exit 0; 37 extracted lines); the 16 numbered locks are intact; the 3C
disposition matrix tallies exactly **9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER**
over 21 candidates (7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F), zero silent drops,
and the Exec-Summary headline (3C:49) matches the body. The lone DEFER
(LAC-2F-V3-03) names a concrete falsifiable re-entry trigger (any 2F-class
re-audit citing a "balanced-scan gap" must `ls` BOTH `parse-that/.../scan/` AND
`skinny/.../bbnf-simd/src/` in its evidence header) and is folded as an
audit-scope note, not dropped. 3F's next-cycle directive (Steps 1-8) carries
concrete sequenced measurable entry conditions (`x86_tree_deleted==true`,
`runtime_target_rows_collapsed==true`, `lock14_gate_scans_codegen==true`,
`grep -c parse_w11_1_number==0`, the H1 `css_canon_bench` ≥1-corpus->1.0×
re-lock predicate). The 3A/3E/3B/3C/3D/3F live V4 Open-Questions tables route
every row to a receiver/blocker/gate on the SK-V18/SK-V19 manifest with concrete
falsifiers. No standalone `validated`-close survives (scan empty across all six
artefacts). The 13 T-P2 refutations appear ONLY as REJECT gates / REFUTED-flags /
REDRESS-fences (checkasm-as-speedup and `find_css_significant` wire-as-is both
appear exactly once, each flagged "REFUTED" at 3E:55/:53; md5-distinctness is
NECESSARY-NOT-SUFFICIENT everywhere, co-gated by `runtime_target_rows_collapsed`).
No V1 surface is edited.

CH6 cannot ACCEPT because the V4/CH6 finding **CH6-V4-01 was NOT folded** — the
exact un-caveated closure word survives at **3D:110**. The artefact mtimes
confirm the gap: 3D-skinny-fold.md last changed at **20:48:54**, the V4/CH6
verdict was written at **21:02**, so the V5 fold has not yet reached 3D. Line 110
(the SK-V15->SK-V18 re-key NOTE table) still reads "...CSS is now
**MEASUREMENT-VALID**, so the demotion gate is STANDING, not active repair." with
NO directional / H1-re-lock caveat IN THE CELL — the precise word that this SAME
artefact's own monotonic-fold rule (3D:88: "...CSS must NOT carry an un-caveated
'MEASUREMENT-VALID' closure word on its half"), 3B's R03-closed verdict
(3B:200/219: "...do NOT carry the un-caveated 'MEASUREMENT-VALID' closure word the
row's own fail-action forbids"), and 3F:275 (CH4/CH7: "Migration must not credit
the un-caveated 'MEASUREMENT-VALID' closure word ... until `css_canon_bench`
re-locks") all explicitly forbid before the H1 re-lock. The leak is aggravated by
3D's own CH1 V4 Open Question (3D:166), which PRE-COMMITS Pass Omega CRUD to scrub
"un-caveated 'CSS MEASUREMENT-VALID' closure" lines — the artefact introduces the
forbidden word and then routes its own cleanup downstream, the paper-close-adjacent
pattern CH6 fences. This is a tighten-not-reject REVISE: narrow (one re-key NOTE
cell, not a delta or headline), the cell's operative claim ("STANDING because P2
prunes `measure_mbps`") does not itself turn on a re-locked ratio, every OTHER
MEASUREMENT-VALID instance in 3D is correctly caveated (3D:43 "but
DIRECTIONAL/not-re-locked (loadavg 4.35...)", 3D:58 "but DIRECTIONAL, not yet
[re-locked]", 3D:123 "DIRECTIONAL pending H1 re-lock"), and every load-bearing 3D
delta (D01/D03/D10/D12) carries the directional caveat. But the word as written
credits CSS measurement-closure the H1 gate has not delivered, and a one-cell
caveat insertion repairs it.

## Evidence Commands And Outputs

```sh
# GATE OBJECT APPLIES (V1's REJECT stays cleared; V2/V3/V4 clean state holds):
$ awk '/^```diff$/{d=1;next} d&&/^```$/{exit} d{print}' \
    restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v5.diff   # 37 lines
$ git apply --check          /tmp/tp3-locks-v5.diff ; echo exit=$?   # exit=0
$ git apply --check --recount /tmp/tp3-locks-v5.diff ; echo exit=$?   # exit=0

$ grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md                    # 16 (intact)

# 3C disposition column over the 21 candidate rows:
9 ACCEPT   11 MODIFY   0 REJECT   1 DEFER   (sum=21, 0 silent drops)
# candidate composition: 7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F = 21
# 3C Exec Summary :49 = "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"   (matches body)

# standalone close-word scan across all six artefacts:
$ grep -nE '\bvalidated\b' restart/audit/totality/p3/3{A,B,C,D,E,F}*.md   # EMPTY

# PLANNED co-gate symbols rg=0 live (not laundered into met gates):
$ rg -l runtime_target_rows_collapsed   skinny/crates skinny/xtask | wc -l   # 0
$ rg -l bbnf_simd_single_mask_convention skinny/crates | wc -l               # 0
$ rg -l lock14_gate_scans_codegen        skinny/crates skinny/xtask | wc -l  # 0
$ rg -l parse_w11_1_number               skinny/crates | wc -l               # 3
#   parse_w11_1_number STILL LIVE -> 3F's `grep -c == 0` entry-cond (3F:245) is a
#   genuine future gate, not pre-met.

# refuted-route non-revival (each appears ONLY as a REFUTED/REJECT flag):
$ grep -n 'find_css_significant wire-as-is' .../3E*.md   # :53 "REFUTED ... (refute 3)"
$ grep -n 'checkasm-PASS is a speedup'      .../3E*.md   # :55 "REFUTED ... (refute 8)"
$ grep -niE 'md5.?distinct' .../3{A,B,D,E,F}*.md | grep -v 'necessary|co-gate|collapse|_RS|distinct from'  # EMPTY
```

```sh
# CARRIED CH6-V4-01 LEAK — STILL OPEN (the only surviving CH6-class surface) at 3D:110:
$ stat -f "%Sm %N" -t "%H:%M:%S" restart/audit/totality/p3/3D-skinny-fold.md
20:48:54 .../3D-skinny-fold.md          # PRE-DATES the V4/CH6 verdict (21:02) -> not yet folded
$ sed -n '110p' restart/audit/totality/p3/3D-skinny-fold.md
| W0 baseline / W1 broadcast demotion | W0 lock + P2 (warm-bench prune) | SK-V18
broadcast is already PRUNED (P2 deletes `measure_mbps`); CSS is now MEASUREMENT-VALID,
so the demotion gate is STANDING, not active repair. |
#   ^ bare "CSS is now MEASUREMENT-VALID" — NO directional/H1 caveat in the cell.

# Every OTHER 3D MEASUREMENT-VALID instance IS correctly caveated (so :110 is isolable):
$ grep -nE 'MEASUREMENT-VALID' .../3D-skinny-fold.md
:43  "...MEASUREMENT-VALID but DIRECTIONAL/not-re-locked (loadavg 4.35, H1 ...)"   # OK
:58  "...MEASUREMENT-VALID — ... but DIRECTIONAL, not yet [re-locked]"             # OK
:88  "...CSS must NOT carry an un-caveated 'MEASUREMENT-VALID' closure word ..."   # the RULE
:110 "...CSS is now MEASUREMENT-VALID, so the demotion gate is STANDING ..."       # LEAK
:123 "...carry CSS as DIRECTIONAL pending H1 re-lock."                             # OK
:166 (own CH1 V4 OQ) routes Pass Omega CRUD to scrub these exact lines             # downstream-defer

# The same word forbidden by the sibling artefacts:
# 3B:200/219 "...do NOT carry the un-caveated 'MEASUREMENT-VALID' closure word the
#             row's own fail-action forbids (CH2-V1-R03)."   (R03 CLOSED in V3/CH2)
# 3F:275     "...Migration must not credit the un-caveated 'MEASUREMENT-VALID' closure
#             word on the CSS ratio until `css_canon_bench` re-locks."
```

Load-bearing finding-ids and citations spot-verified to resolve on-disk: `1E:147`
(LAC-1E-V5-01 named-primitive (a)-(d) gate), `1E:148` (LAC-1E-V5-02 relocated-seam
firewall), `1A:180` (1A-LOCK1-AMEND-001 `<G>` generality-vehicle strike), `2C:382`
(LAC-2C-SK18-03 totality-tree row-collapse), `2F:194`/`:196` (LAC-2F-V3-01 +
LAC-2F-V3-03 DEFER re-entry trigger). The LAC-2C-SK18-03 self-gate-RED citation
`crates/ir/src/registry/strategy.rs:137-185` resolves to a 334-line ROOT-tree file
(the totality-leak the disposition asserts); `css_types.rs:1` resolves at
`crates/core/src/css_types.rs` (`//! Host shims for the CSS L4 grammar`). The
LAC-2D-V3-03 live-`Rewrite` spot-claim resolves: `NormalizeDirectSinkCost` is a
live `Rewrite<DecisionNode, NoAnalysis>` at
`skinny/crates/passes/src/backend_egraph.rs:193` instantiated at `:75`. The cited
LOCKS sections all resolve: `:349` (Lock 14 generic-crate self-gate), `:620`
(`G:EventGrammar` generality-vehicle clause the 1A strike targets), `:622`
(SK-V17 Lock 16 NEON-classifier-manifest), `:625` (`## v+1 Governance Boundary`).
The SK-V18 SPEC carries NO retired W# (`grep -cE '\bW(1|4|5|6|7|8|9|10)\b'` = 0),
so no live receiving gate routes to a retired wave.

## Enumerated Deltas / Dispositions Under CH6

| # | artefact / object | judgement | basis |
|---|---|---|---|
| 1 | 3C v+1 diff — applies to live LOCKS.md (the G-Omega gate object) | **ACCEPT** | `git apply --check` AND `--recount` both exit 0; my lens's mandated spot-verify ("the v+1 diff applies") PASSES. Stays cleared since V2. |
| 2 | 3C Exec Summary disposition tally (:49) | **ACCEPT** | "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER" matches the field-6 column over 21 candidate rows. V1's CH6-V1-02 transposition stays fixed. |
| 3 | 3C disposition completeness (21/21, 0 silent drops) | **ACCEPT** | Candidate set = 7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F = 21, each dispositioned exactly once; the CH6-REJECT silent-drop trigger does not fire. |
| 4 | 3C DEFER (LAC-2F-V3-03) re-entry trigger | **ACCEPT** | Names a concrete falsifiable re-entry trigger ("any 2F-class re-audit citing a 'balanced-scan gap' must `ls` both trees in its evidence header"); folded into D-SKV18-L16-single-substrate-movemask as an audit-scope note, not dropped. Satisfies my lens's DEFER clause. |
| 5 | 3C PLANNED co-gate symbols (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) | **ACCEPT** | Both `rg`=0 live (verified); disclosed as PLANNED-not-yet-live in the matrix, never cited as met. No laundering of a planned gate into a satisfied one. |
| 6 | 3C Open Questions (incl. CH6 named-primitive + CH2 DEFER re-entry rows) | **ACCEPT** | Full receiver/blocker/gate triad; the CH6 row REFUSES to treat the named-primitive (a)-(d) gate as already-satisfied by the SK-V17 Lock-16 NEON clause (LOCKS grep `named-primitive`/`PROFILE-PROVEN-NARROW-LEAF`/`emit_shape_source` = 0 binds the falsifier). Direct anti-paper-close discipline. |
| 7 | 3A SK-V18 extension deltas (ARCH-3A-V4-SK18-D01..D14) | **ACCEPT** | Each cites a resolving 1A-1F / 2C-2F finding-id and an SK-V18 wave; D14 self-flags "a routing note... NOT a closure"; D13 (md5-distinct) is co-gated by no-`_RS`-blob grep. No prose-close. |
| 8 | 3A carried-verbatim V3 packet + carried HISTORICAL Open-Questions table | **ACCEPT** | V3's CH6-V3-01 FOLDED: each historical row cell carries its SK-V18 receiver IN-CELL (W#->SK-V18-receiver re-key). A row read in isolation lands on a live SK-V18 gate. Frontmatter discloses SK-V15 authorship as historical record, not a current-cycle close. |
| 9 | 3A LIVE "V4 Open Questions" table (265-274) | **ACCEPT** | All 6 rows route to receiver/blocker/gate on the SK-V18/SK-V19 manifest (Pass-Omega-ARCH-CRUD / G4 / W-PROVE / G3 / SK-V19) with concrete falsifiers (`value_ref_grammar_param_deleted`, `verbatim_blob_present==false`, REDRESS-98 scalar-cheaper clearance); the CH6 row binds the CSS-generator-exists claim to byte-equivalence-vs-deleted-oracle + (b) mutation falsifier — refuses a byte-count-delta close. |
| 10 | 3E SK-V18 extension deltas + carried HISTORICAL table + REFUTED frontmatter | **ACCEPT** | V3's CH6-V3-02 FOLDED: every historical receiver/gate cell names its live SK-V18 receiver in-cell; the 13 refutations (incl. `find_css_significant` wire-as-is :53, checkasm-as-speedup :55, tree-walk-cannot-preserve :245/:292) appear ONLY as REFUTED-flags, never as deltas. |
| 11 | 3E LIVE "V4 Open Questions" table | **ACCEPT** | Rows route to receiver/blocker/gate on the SK-V18 manifest with concrete commands; the CH6 (d) PROFILE-PROVEN-NARROW-LEAF row binds to the H1 re-captured profile — refuses crediting a checkasm PASS as a speedup close. |
| 12 | 3B Open Questions + MP-3B-SKV18-D10 CSS verdict (3B:200/219) | **ACCEPT** | All rows carry receiver/blocker/gate keyed to SK-V18/SK-V19; D10 reads "directionally-valid pending the H1 `css_canon_bench` re-lock... do NOT carry the un-caveated 'MEASUREMENT-VALID' closure word the row's own fail-action forbids" — R03 CLOSED and stays closed in the SAME cell. This is the exemplar 3D:110 fails to mirror. |
| 13 | 3D SK-V18 re-anchor wave-map STRUCTURE (108-117, rows :111-:117) | **ACCEPT** | Every prior SK-V15 W0-W11 receiver re-keyed in-cell to the SK-V18 manifest (W2->P4, W3->P5/G1, W4->SK-V19, W5/W6->G2+H1, W7-W9->G3, W10->standing Lock-16, W11->G4+G5/G6); fully self-contained — the correct fold pattern. The structure is sound; only the :110 cell's CSS-half wording is in fault. |
| 14 | 3D:110 re-key NOTE cell — bare "CSS is now MEASUREMENT-VALID" | **REVISE** | CARRIED CH6-V4-01, NOT folded (3D mtime 20:48 < V4 verdict 21:02). The cell carries the un-caveated "MEASUREMENT-VALID" closure word on the CSS half with NO directional/H1 caveat in the cell — the exact word 3D's own monotonic-fold rule (3D:88), 3B:200/219 (R03-closed), and 3F:275 (CH4/CH7) all forbid before the `css_canon_bench` H1 re-lock. 3D:166 PRE-COMMITS Pass Omega CRUD to scrub these lines, so the artefact introduces the word it then asks CRUD to remove. Tighten-not-reject: narrow, the cell's operative "STANDING" claim turns on P2 pruning `measure_mbps` not a re-locked ratio, and every OTHER 3D MEASUREMENT-VALID instance (:43/:58/:123) and every load-bearing 3D delta carries the caveat. CORRECTION (3D author, V6 fold): amend 3D:110 to "CSS is now **MEASUREMENT-VALID-DIRECTIONAL** (not re-locked; loadavg 4.35, H1 `css_canon_bench` gate)" OR drop the MEASUREMENT-VALID clause and keep only "broadcast is PRUNED via P2, so the demotion gate is STANDING" — mirroring 3B:200, which states the directional caveat in the SAME cell. |
| 15 | 3D load-bearing deltas (3D-D01/D03/D10/D12) | **ACCEPT** | Each carries CSS as DIRECTIONAL pending the H1 re-lock with the gate cited (3D:88/123); 3D-D12 binds the un-fork to the co-gate CONJUNCTION (`runtime_target_rows_collapsed==true`), NOT md5-distinctness alone. T-P3-proposes-only honoured. |
| 16 | 3D Open Questions (incl. own CH1 :166 row + CH5 :170 NEON-admission row) | **ACCEPT** | Full receiver/blocker/gate triad; the CH5 row binds G6 NEON to BOTH non-`#[cfg(test)]` caller census AND `simd_admission_profile_sampled==true` — "a census-only proof is `dead`, not `admission`". (The :166 row is what aggravates the :110 leak, but the row itself is correctly gated.) |
| 17 | 3F next-cycle dispatch directive (Steps 1-8) | **ACCEPT** | Concrete sequenced measurable entry conditions: T-P3 lock -> G3 auto-pass -> Pass Omega V6 -> CHALLENGE-before-CRUD -> CRUD current-state cleanup (blocked/extension remainder names receiver/blocker/gate) -> G-Omega -> W-PRUNE (P1-P5 first, P4-before-G2/G3) -> SK-V19 tee-up; entry-gates carry `x86_tree_deleted==true`, `runtime_target_rows_collapsed==true`, `lock14_gate_scans_codegen==true`, `grep -c parse_w11_1_number==0`, the H1 ≥1-corpus->1.0× re-lock. SK-V19 carriers (a)-(e) each cited, none silently dropped. Satisfies my lens's 3F clause directly. |
| 18 | 3F Open Questions (incl. CH4/CH7 MEASUREMENT-VALID fence :275) | **ACCEPT** | Full receiver/blocker/gate triad; 3F:275 forbids crediting an un-caveated CSS "MEASUREMENT-VALID" closure word in MIGRATION/HANDOFF before the H1 re-lock — the very fence 3D:110 leaks past. The handoff surface itself is clean. |
| 19 | T-P1/T-P2 governance honesty across the packet | **ACCEPT** | 3F-MH-004 carries T-P1 as IN-CYCLE near-converged NON-normal-§3Z and CSS ratios as DIRECTIONAL not re-locked (U-4); CH1-V1-C5 forbids citing the SK-V15 T-P3 V5 file as the SK-V18 record; G3 auto-pass on cohort lock under the active pin; G-Omega the only mandatory user gate. Never laundered to a normal two-clean §3Z close. |
| 20 | Refuted-route non-revival (13 T-P2 refutations) + cross-scope discipline | **ACCEPT** | tree-walk, wire-as-is `find_css_significant`, checkasm-as-speedup, x86/AVX-512-close, md5-distinctness-alone, `bracket_depth_mask` appear ONLY as REJECT gates / REFUTED-flags / REDRESS-fences. All six artefacts proposal-only; v+1 diff gated behind `## v+1 Governance Boundary`; no sixth shape / new directive / new substrate / lock retirement; numbered-lock count = 16. |

## Findings (repair directives)

| id | severity | target lines | finding | repair | owner |
|---|---|---|---|---|---|
| CH6-V5-01 (carries CH6-V4-01) | MEDIUM | `restart/audit/totality/p3/3D-skinny-fold.md:110` | The SK-V15->SK-V18 re-key NOTE cell carries a bare "CSS is now **MEASUREMENT-VALID**, so the demotion gate is STANDING, not active repair" with NO directional/H1-re-lock caveat IN THE CELL. This is the exact un-caveated closure word that the SAME artefact's monotonic-fold rule (3D:88), 3B:200/219 (R03-closed in V3/CH2), and 3F:275 (CH4/CH7) all explicitly forbid before the `css_canon_bench` H1 re-lock. Aggravated by 3D:166, which pre-commits Pass Omega CRUD to scrub "un-caveated CSS MEASUREMENT-VALID closure" lines — the artefact introduces the forbidden word and then routes its own cleanup downstream. The V4/CH6 fold did NOT reach this line (3D mtime 20:48:54 < V4/CH6 verdict 21:02); the finding is CARRIED, still open. Not an engineered deferral or fabricated validation (the cell's operative "STANDING" claim turns on P2 pruning `measure_mbps`, and every OTHER 3D MEASUREMENT-VALID instance :43/:58/:123 plus every load-bearing 3D delta carries the caveat) — a tighten-not-reject closure-word leak. | Amend 3D:110 to "CSS is now MEASUREMENT-VALID-DIRECTIONAL (not re-locked; loadavg 4.35, H1 `css_canon_bench` gate)" OR drop the MEASUREMENT-VALID clause and keep only "broadcast is PRUNED via P2, so the demotion gate is STANDING" — mirror 3B:200, which states the directional caveat in the same cell. No 3D cell may carry the bare MEASUREMENT-VALID word on the CSS half. | 3D author (V6 fold). |

## Non-Findings Checked

- No uncited "validated"/"verified"/"proven" close: the standalone-`validated`
  scan across all six artefacts is empty; every "proven"/"verified" carries an
  adjacent path:line or a falsifier-command.
- No silent-dropped candidate: 3C disposes all 21 (7×1E + 1×1A + 13×2X), one
  each; the lone DEFER carries a concrete `ls`-both-trees re-entry trigger and is
  folded as an audit-scope note. 3F Step 8's SK-V19 carriers (a)-(e) are each
  cited.
- No engineered deferral on any LIVE row: every live deferral (3B/3C/3D/3F + the
  six V4 tables) routes to a receiver/blocker/gate on the SK-V18/SK-V19 manifest;
  the 3F/3B CH3 rows REFUSE to push the SK-V16/V17 reconcile to SK-V19 entry,
  blocking G2/G4/G6 during SK-V18 instead. The carried-historical 3A/3E row cells
  (V3's REVISE surface) remain ROW-SELF-CONTAINED.
- No revived refuted-route: the 13 T-P2 refutations appear only as REJECT gates /
  REFUTED-flags / REDRESS-fences (checkasm-as-speedup 3E:55, wire-as-is 3E:53,
  tree-walk-cannot-preserve 3E:245/:292), never as deltas. md5-distinctness is
  NECESSARY-NOT-SUFFICIENT everywhere, co-gated by `runtime_target_rows_collapsed`.
- No cross-scope violation: all six artefacts proposal-only; the v+1 diff is
  gated behind `## v+1 Governance Boundary`; no sixth shape / new directive / new
  substrate / lock retirement; numbered-lock count = 16.
- No planned-gate laundering: `runtime_target_rows_collapsed`,
  `bbnf_simd_single_mask_convention`, `lock14_gate_scans_codegen` all `rg`=0
  live; `parse_w11_1_number` is still live (3 files), so 3F's `grep -c == 0`
  entry condition (3F:245) is a genuine future gate, not pre-met.
- No G-Omega gate-object corruption: the v+1 diff applies cleanly (`--check` and
  `--recount` exit 0); the cited LOCKS sections (:349/:620/:622/:625) all resolve;
  the LAC-2C-SK18-03 self-gate-RED citation (`crates/ir/src/registry/strategy.rs`)
  resolves to the live 334-line ROOT-tree file, not a phantom; the SK-V18 SPEC
  carries no retired W#.
- No G3/G-Omega confusion: 3F carries G3 auto-pass on cohort lock under the
  active pin with G-Omega as the only mandatory user gate.

## Residual Risk

This CH6 pass re-ran its mandated spot-verifications (the v+1 diff apply, the 3C
tally + 21/21 completeness + candidate-composition, the load-bearing finding-id +
LOCKS-section + live-code citation resolution, the planned-gate `rg`=0, the
refuted-route non-revival, the SK-V18 manifest membership of every LIVE receiving
gate, the standalone-close scan) and confirmed the V1-V3 REVISE findings (the
3A/3E carried-historical row-routing) stay FOLDED. It did NOT re-run CH1's full
citation-resolution matrix nor CH5's hidden-coupling sweep. The single surviving
CH6-class surface (CH6-V5-01, carries CH6-V4-01, 3D:110) is a cosmetic
closure-word leak on a re-key NOTE cell that the V4 fold did not reach (mtime
proves the gap), NOT a deferral-integrity breach: the deferral SUBSTANCE of the
packet is sound, the V1-V4 blockers stay folded, and the fix is a one-cell caveat
insertion that 3B:200 already exemplifies. Because the verdict is REVISE, V5 must
not be counted as a clean hardening cycle; a V6 fold must add the directional/H1
caveat to 3D:110 (or drop the MEASUREMENT-VALID clause from that cell) before
lock. The V≤5 ceiling is now reached with this surface still open — if a V6 is not
admitted to fold the one cell, the orchestrator must treat 3D:110 as a forced
Pass-Omega-CRUD pre-edit fence (the 3D:166 receiver already names it) so the
forbidden word never reaches a V1 surface.

TALLY accept=19 revise=1 reject=0
