# CH6 ANTI-PAPER-CLOSE — SK-V18 T-P3 cycle V4

Verdict: REVISE

The V3 fold closed both V3 REVISE findings: the 3A carried-HISTORICAL Open-Questions
table (143-154) and the 3E carried-HISTORICAL Open-Questions table (213-218) are now
ROW-SELF-CONTAINED — every receiver/gate cell names its live SK-V18 receiver IN THE
SAME CELL (3A: W1→P2/§7.4, W3/W5/W6→G1/G2/G3, W4→P5/SK-V19, W8/W9→G4/G5,
W10→Lock-16 quarantine; 3E: W2→P4, W5/W6→G2/H1, W7→G3∧PROVE∧SK-V19), so no row read
in isolation routes to a retired W# absent from the certified SK-V18 12-wave
manifest. The structural floor remains sound: the load-bearing v+1 diff — the
G-Omega gate object — APPLIES (`git apply --check` AND `--recount` both exit 0); the
3C disposition matrix tallies exactly 9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER over
21 candidates (7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F), zero silent drops, headline
(Exec Summary :49) matches the body; the lone DEFER (LAC-2F-V3-03) names a concrete
falsifiable re-entry trigger (`ls` both scan trees in any future "balanced-scan gap"
audit); 3F's next-cycle directive carries concrete sequenced measurable entry
conditions; the live V4 Open-Questions tables in all six artefacts route every row to
a receiver/blocker/gate on the SK-V18/SK-V19 manifest with concrete falsifiers; no
standalone `validated`-close survives; the 13 T-P2 refutations appear ONLY as REJECT
gates / REDRESS fences, never as proposed deltas; no V1 surface is edited.

CH6 cannot ACCEPT because a NEW closure-word leak — not examined by the V1-V3 folds
(which scoped CH6 to carried-historical row-routing) — survives in 3D. Line 110 (the
SK-V15→SK-V18 re-key NOTE table) carries a bare "CSS is now **MEASUREMENT-VALID**, so
the demotion gate is STANDING" with NO DIRECTIONAL / H1-re-lock caveat IN THE CELL —
the exact un-caveated closure word that this SAME artefact's monotonic-fold rule
(3D:88: "CSS must NOT carry an un-caveated 'MEASUREMENT-VALID' closure word on its
half"), 3B's R03-closed verdict (3B:200/219), and 3F's CH4/CH7 row (3F:275) all
explicitly forbid before the `css_canon_bench` H1 re-lock. The leak is aggravated by
3D's own CH1 V4 Open Question (3D:166), which PRE-COMMITS Pass Omega CRUD to scrub
"un-caveated CSS MEASUREMENT-VALID closure" lines — i.e. the artefact introduces the
forbidden word and then routes its own cleanup downstream, the paper-close-adjacent
pattern CH6 fences. This is a tighten-not-reject REVISE: narrow (one re-key NOTE
cell, not a delta or headline), the cell's operative claim ("STANDING because P2
prunes `measure_mbps`") does not itself turn on a re-locked ratio, and every
load-bearing 3D delta (D01/D03/D10/D12) carries the directional caveat correctly — so
it is a closure-word leak, NOT a fabricated validation or an engineered deferral. But
the word as written credits CSS measurement-closure the H1 gate has not delivered,
and a one-cell caveat insertion repairs it.

## Evidence Commands And Outputs

```sh
# GATE OBJECT APPLIES (V1's REJECT stays cleared; V3's clean state holds):
$ awk '/^```diff$/{d=1;next} d&&/^```$/{exit} d{print}' \
    restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v4.diff   # 37 lines
$ git apply --check          /tmp/tp3-locks-v4.diff ; echo exit=$?   # exit=0
$ git apply --check --recount /tmp/tp3-locks-v4.diff ; echo exit=$?   # exit=0

$ grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md                    # 16 (intact)

# 3C disposition column over the 21 candidate rows (field-6 extract):
9 ACCEPT   11 MODIFY   0 REJECT   1 DEFER   (sum=21, 0 silent drops)
# candidate composition: 7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F = 21
# 3C Exec Summary :49 = "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"   (matches body)

# V3 REVISE findings FOLDED — carried-historical row cells now self-contained:
#  3A:148-154 cells each carry "(W#->SK-V18-receiver re-key)" inline
#  3E:213-218 cells each carry "(SK-V15 W#->SK-V18-receiver re-key)" inline
#  3A/3E live "## V4 Open Questions" (3A:265-274, 3E:377+) route only to SK-V18 gates
$ awk 'NR>=265&&NR<=274' .../3A-architecture-synthesis.md | grep -oE 'W[0-9]+' | sort -u
W12          # the SOLE W# in the live V4 table — a NEGATIVE guard ("no W12"), not a route

# SK-V18 manifest contains NO retired W#:
$ grep -cE '\bW(1|4|5|6|7|8|9|10)\b' restart/skinny/tranches/sk-v18/SPEC.md   # 0

# PLANNED co-gate symbols rg=0 live (not laundered into met gates):
$ rg -l runtime_target_rows_collapsed   skinny/crates skinny/xtask | wc -l   # 0
$ rg -l bbnf_simd_single_mask_convention skinny/crates | wc -l               # 0
$ rg -l lock14_gate_scans_codegen        skinny/crates | wc -l               # 0
$ rg -l parse_w11_1_number               skinny/crates | wc -l               # 3
#   parse_w11_1_number STILL LIVE -> 3F's `grep -c == 0` entry-cond is a genuine
#   future gate, not pre-met.

# standalone close-word scan across all six artefacts:
$ rg -n '\bvalidated\b' restart/audit/totality/p3/3{A,B,C,D,E,F}*.md          # empty
```

```sh
# NEW CH6 LEAK (the only surviving CH6-class surface) — 3D:110:
$ sed -n '110p' restart/audit/totality/p3/3D-skinny-fold.md
| W0 baseline / W1 broadcast demotion | W0 lock + P2 (warm-bench prune) | SK-V18
broadcast is already PRUNED (P2 deletes `measure_mbps`); CSS is now MEASUREMENT-VALID,
so the demotion gate is STANDING, not active repair. |
#   ^ bare "CSS is now MEASUREMENT-VALID" — NO directional/H1 caveat in the cell.

# The same artefact's own rule forbids it (3D:88):
"...CSS must NOT carry an un-caveated 'MEASUREMENT-VALID' closure word on its half"
# 3B:200/219 carry the caveat + the explicit prohibition (R03 CLOSED in V3/CH2):
"...do NOT carry the un-caveated 'MEASUREMENT-VALID' closure word the row's own
 fail-action forbids (CH2-V1-R03)."
# 3F:275 (CH4/CH7) forbids it; 3D:166 (own CH1 V4 OQ) routes cleanup of these exact
# lines to Pass Omega CRUD — the artefact introduces the word it then asks CRUD to scrub.
```

Load-bearing finding-ids and citations spot-verified to resolve on-disk: `1E:153`
(LAC-1E-V5-07 Pattern-H recensus), `1A:180` (1A-LOCK1-AMEND-001 `<G>` generality-
vehicle strike), `2C:382` (LAC-2C-SK18-03 totality-tree row-collapse), `2F:196`
(LAC-2F-V3-03 DEFER + re-entry trigger). The LAC-2C-SK18-03 self-gate-RED citation
`crates/ir/src/registry/strategy.rs:137-185` resolves to a 334-line ROOT-tree file
(not the skinny tree) — the totality-leak the disposition asserts; `css_types.rs:1`
resolves at `crates/core/src/css_types.rs`. The LAC-2D-V3-03 live-`Rewrite`
spot-claim resolves: `NormalizeDirectSinkCost` is a live `Rewrite<DecisionNode,
NoAnalysis>` at `skinny/crates/passes/src/backend_egraph.rs:193` instantiated at `:75`.
The cited LOCKS sections all resolve: `:349` (Lock 14 generic-crate self-gate), `:620`
(generality-vehicle clause), `:622` (SK-V17 Lock 16 NEON-classifier-manifest), `:625`
(`## v+1 Governance Boundary`).

## Enumerated Deltas / Dispositions Under CH6

| # | artefact / object | judgement | basis |
|---|---|---|---|
| 1 | 3C v+1 diff — applies to live LOCKS.md (the G-Omega gate object) | **ACCEPT** | `git apply --check` AND `--recount` both exit 0; my lens's mandated spot-verify ("the v+1 diff applies") PASSES. Stays cleared since V2. |
| 2 | 3C Exec Summary disposition tally (:49) | **ACCEPT** | "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER" matches the field-6 column over 21 candidate rows. V1's CH6-V1-02 transposition stays fixed. |
| 3 | 3C disposition completeness (21/21, 0 silent drops) | **ACCEPT** | Candidate set = 7×1E + 1×1A + 3×2C + 4×2D + 3×2E + 3×2F = 21, each dispositioned exactly once; the CH6-REJECT silent-drop trigger does not fire. |
| 4 | 3C DEFER (LAC-2F-V3-03) re-entry trigger | **ACCEPT** | Names a concrete falsifiable re-entry trigger ("any 2F-class re-audit citing a 'balanced-scan gap' must `ls` both `parse-that/.../scan/` AND `skinny/.../bbnf-simd/src/`"); folded into D-SKV18-L16-single-substrate-movemask as an audit-scope note, not dropped. Satisfies my lens's DEFER clause. |
| 5 | 3C PLANNED co-gate symbols (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) | **ACCEPT** | Both `rg`=0 live (verified); disclosed as PLANNED-not-yet-live in the matrix, never cited as met. No laundering of a planned gate into a satisfied one. |
| 6 | 3C Open Questions (incl. CH6 named-primitive + CH2 DEFER re-entry rows) | **ACCEPT** | Full receiver/blocker/gate triad; the CH6 row refuses to treat the named-primitive (a)-(d) gate as already-satisfied by the SK-V17 Lock-16 NEON clause; binds a falsifier. Direct anti-paper-close discipline. |
| 7 | 3A SK-V18 extension deltas (ARCH-3A-V4-SK18-D01..D14) | **ACCEPT** | Each cites a resolving 1A-1F / 2C-2F finding-id and an SK-V18 wave; D14 self-flags "a routing note... NOT a closure." No prose-close. |
| 8 | 3A carried-verbatim V3 packet (ARCH-3A-V1-D01..D12) | **ACCEPT** | Frontmatter + section :198 disclose SK-V15 authorship, "retained as the historical V3 synthesis record," several already applied by intervening CRUD; not asserted as a current-cycle close. |
| 9 | 3A carried HISTORICAL Open-Questions table (146-154) | **ACCEPT** | V3's CH6-V3-01 FOLDED: each of the 7 row cells now carries its SK-V18 receiver IN-CELL (W1→P2/§7.4, W3/W5→G1/G2/G3, W5→G2, W6→G2/H1, W4→P5/SK-V19, W8/W9→G4/G5, W10→Lock-16 quarantine); the re-key is no longer out-of-band. A row read in isolation now lands on a live SK-V18 gate. |
| 10 | 3A LIVE "V4 Open Questions" table (265-274) | **ACCEPT** | All 7 rows route to receiver/blocker/gate on the SK-V18 manifest (Pass-Omega-CRUD / G2 / G3 / G4 / W-PROVE / SK-V19) with concrete falsifiers (`value_ref_grammar_param_deleted`, `verbatim_blob_present==false`, REDRESS-98 clearance). The lone W# ("W12") is a NEGATIVE guard, not a route. |
| 11 | 3E SK-V18 extension deltas (3E-D12..D18) + carried HISTORICAL table (213-218) | **ACCEPT** | V3's CH6-V3-02 FOLDED: every receiver/gate cell now names its live SK-V18 receiver in-cell (W2→P4, W5/W6→G2/H1, W7→G3∧PROVE∧SK-V19, W1→P2, W4→P5/SK-V19, W8/W9→G4/G5); extension deltas cite 2C SK-V18 ids + 1E V5 ids; CSS narrative re-folded onto G2 / H1. |
| 12 | 3E LIVE "V4 Open Questions" table (377+) | **ACCEPT** | Rows route to receiver/blocker/gate on the SK-V18 manifest with concrete commands; the CH6 row binds (d) PROFILE-PROVEN-NARROW-LEAF to the H1 re-captured profile — refuses crediting a checkasm PASS as a speedup close. |
| 13 | 3B Open Questions + MP-3B-SKV18-D10 CSS verdict | **ACCEPT** | All rows carry receiver/blocker/gate keyed to SK-V18/SK-V19; the carried SK-V15-routing deltas were REMOVED outright; D10 reads "directionally-valid pending the H1 `css_canon_bench` re-lock... do NOT carry the un-caveated 'MEASUREMENT-VALID' closure word" — R03 CLOSED and stays closed (the model fix 3D:110 fails to mirror). |
| 14 | 3D SK-V18 re-anchor wave-map (108-117) — STRUCTURE | **ACCEPT** | Every prior SK-V15 W0-W11 receiver re-keyed in-cell to the SK-V18 manifest (W5/W6→G2+H1, W7-W9→G3, W10→standing Lock-16, W11→G4+G5/G6); fully self-contained — the correct fold pattern. |
| 15 | 3D:110 re-key NOTE cell — bare "CSS is now MEASUREMENT-VALID" | **REVISE** | The cell carries the un-caveated "MEASUREMENT-VALID" closure word on the CSS half with NO directional/H1 caveat in the cell — the exact word 3D's own monotonic-fold rule (3D:88), 3B:200/219 (R03-closed), and 3F:275 (CH4/CH7) all forbid before the `css_canon_bench` H1 re-lock. 3D:166 PRE-COMMITS Pass Omega CRUD to scrub these lines, so the artefact introduces the word it then asks CRUD to remove. Tighten-not-reject: narrow, the cell's operative "STANDING" claim turns on P2 pruning `measure_mbps` not on a re-locked ratio, and every load-bearing 3D delta carries the caveat correctly. CORRECTION (3D author): amend 3D:110 to "CSS is now **MEASUREMENT-VALID-DIRECTIONAL** (not re-locked; loadavg 4.35, H1 `css_canon_bench` gate)" OR drop the MEASUREMENT-VALID clause and keep only "broadcast is PRUNED via P2, so the demotion gate is STANDING" — mirroring 3B:200, which states the directional caveat in the SAME cell. |
| 16 | 3D load-bearing deltas (3D-D01/D03/D10/D12) | **ACCEPT** | Each carries CSS as DIRECTIONAL pending the H1 re-lock with the gate cited (3D:88/123/125/132); 3D-D12 binds the un-fork to the 3-co-gate CONJUNCTION (md5∧branch_count==0∧type_count==0∧rows_collapsed==true), NOT md5-distinctness alone. T-P3-proposes-only honoured. |
| 17 | 3F next-cycle dispatch directive (Step 1-6) | **ACCEPT** | Concrete sequenced measurable entry conditions: T-P3 lock → Pass Omega V6 → CHALLENGE-before-CRUD → CRUD current-state cleanup (with blocked/extension remainder naming receiver/blocker/gate) → G-Omega → W-PRUNE; entry-gates carry `x86_tree_deleted==true`, `runtime_target_rows_collapsed==true`, `lock14_gate_scans_codegen==true`, `grep -c parse_w11_1_number==0`. |
| 18 | 3F Open Questions (incl. CH4/CH7 MEASUREMENT-VALID fence) | **ACCEPT** | Full receiver/blocker/gate triad; 3F:275 forbids crediting an un-caveated CSS "MEASUREMENT-VALID" closure word in MIGRATION/HANDOFF before the H1 re-lock — the very fence 3D:110 leaks past. |
| 19 | T-P1/T-P2 governance honesty across the packet | **ACCEPT** | 3F carries T-P1 as IN-CYCLE / near-converged NON-normal-§3Z (not the SK-V15 V5 prior-cycle record) and the CSS ratios as DIRECTIONAL not re-locked (U-4); never laundered to a normal two-clean §3Z close. G3 auto-pass on cohort lock under the active pin; G-Omega the only mandatory user gate. |
| 20 | Refuted-route non-revival (13 T-P2 refutations) + cross-scope discipline | **ACCEPT** | tree-walk, wire-as-is `find_css_significant`, neutral-name-on-one-grammar, checkasm-as-speedup, x86/AVX-512 close, md5-distinctness-alone, `bracket_depth_mask` appear ONLY as REJECT gates / REFUTED-flags / REDRESS-fences / forced-demotion obligations, never as proposed deltas. All six artefacts proposal-only; v+1 diff gated behind `## v+1 Governance Boundary`; no sixth shape / new directive / new substrate / lock retirement; numbered-lock count = 16. |

## Findings (repair directives)

| id | severity | target lines | finding | repair | owner |
|---|---|---|---|---|---|
| CH6-V4-01 | MEDIUM | `restart/audit/totality/p3/3D-skinny-fold.md:110` | The SK-V15→SK-V18 re-key NOTE cell carries a bare "CSS is now **MEASUREMENT-VALID**, so the demotion gate is STANDING, not active repair" with NO directional/H1-re-lock caveat IN THE CELL. This is the exact un-caveated closure word that the SAME artefact's monotonic-fold rule (3D:88), 3B:200/219 (R03-closed in V3/CH2), and 3F:275 (CH4/CH7) all explicitly forbid before the `css_canon_bench` H1 re-lock. Aggravated by 3D:166, which pre-commits Pass Omega CRUD to scrub "un-caveated CSS MEASUREMENT-VALID closure" lines — the artefact introduces the forbidden word and then routes its own cleanup downstream. NOT examined by V1-V3 CH6 (scoped to carried-historical row-routing); a genuine NEW surface. Not an engineered deferral or fabricated validation (the cell's operative "STANDING" claim turns on P2 pruning `measure_mbps`, and every load-bearing 3D delta carries the caveat) — a tighten-not-reject closure-word leak. | Amend 3D:110 to "CSS is now MEASUREMENT-VALID-DIRECTIONAL (not re-locked; loadavg 4.35, H1 `css_canon_bench` gate)" OR drop the MEASUREMENT-VALID clause and keep only "broadcast is PRUNED via P2, so the demotion gate is STANDING" — mirror 3B:200, which states the directional caveat in the same cell. No 3D cell may carry the bare MEASUREMENT-VALID word on the CSS half. | 3D author (V5 fold). |

## Non-Findings Checked

- No uncited "validated"/"verified"/"proven" close: the standalone-`validated` scan
  across all six artefacts is empty; every "proven"/"verified" carries an adjacent
  path:line or a falsifier-command.
- No silent-dropped candidate: 3C disposes all 21 (7×1E + 1×1A + 13×2X), one each;
  the lone DEFER carries a concrete `ls`-both-trees re-entry trigger and is folded as
  an audit-scope note. 3F Step 8's SK-V19 carriers are each cited.
- No engineered deferral on any LIVE row: every live deferral (3B/3C/3D/3F + the two
  V4 tables) routes to a receiver/blocker/gate on the SK-V18/SK-V19 manifest; the
  3F/3B CH3 rows REFUSE to push the SK-V16/V17 reconcile to SK-V19 entry, blocking
  G2/G4/G6 during SK-V18 instead. The carried-historical 3A/3E row cells (V3's REVISE
  surface) are now ROW-SELF-CONTAINED.
- No revived refuted-route: the 13 T-P2 refutations appear only as REJECT gates /
  REFUTED-flags / REDRESS-fences / forced-demotion obligations, never as deltas.
- No cross-scope violation: all six artefacts proposal-only; the v+1 diff is gated
  behind `## v+1 Governance Boundary`; no sixth shape / new directive / new substrate
  / lock retirement; numbered-lock count = 16.
- No planned-gate laundering: `runtime_target_rows_collapsed`,
  `bbnf_simd_single_mask_convention`, `lock14_gate_scans_codegen` all `rg`=0 live;
  `parse_w11_1_number` is still live (3 files), so 3F's `grep -c == 0` entry condition
  is a genuine future gate, not pre-met.
- No G-Omega gate-object corruption: the v+1 diff applies cleanly (`--check` and
  `--recount` exit 0); the cited LOCKS sections (:349/:620/:622/:625) all resolve; the
  LAC-2C-SK18-03 self-gate-RED citation (`crates/ir/src/registry/strategy.rs`)
  resolves to the live ROOT-tree file, not a phantom.
- No G3/G-Omega confusion: 3F carries G3 auto-pass on cohort lock under the active pin
  with G-Omega as the only mandatory user gate.

## Residual Risk

This CH6 pass re-ran its mandated spot-verifications (the v+1 diff apply, the 3C tally
+ 21/21 completeness + candidate-composition, the load-bearing finding-id + LOCKS-
section + live-code citation resolution, the planned-gate `rg`=0, the refuted-route
non-revival, the SK-V18 manifest membership of every LIVE receiving gate, the
standalone-close scan) and confirmed both V3 REVISE findings (3A/3E carried-historical
row-routing) FOLDED. It did NOT re-run CH1's full citation-resolution matrix nor CH5's
hidden-coupling sweep. The single surviving CH6-class surface (CH6-V4-01, 3D:110) is a
cosmetic closure-word leak on a re-key NOTE cell, not a deferral-integrity breach: the
deferral SUBSTANCE of the packet is sound, the V1-V3 blockers stay folded, and the fix
is a one-cell caveat insertion that 3B:200 already exemplifies. Because the verdict is
REVISE, V4 must not be counted as a clean hardening cycle; a V5 fold must add the
directional/H1 caveat to 3D:110 (or drop the MEASUREMENT-VALID clause from that cell)
before lock. V5 is the hard ceiling.

TALLY accept=19 revise=1 reject=0
