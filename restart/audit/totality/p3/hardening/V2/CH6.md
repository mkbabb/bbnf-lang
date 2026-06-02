# CH6 ANTI-PAPER-CLOSE — SK-V18 T-P3 cycle V2

Verdict: REVISE

The packet has folded V1's three blockers. The load-bearing v+1 diff — the
G-Omega gate object — now APPLIES (`git apply --check` exits 0, with and without
`--recount`); the 3C disposition tally is repaired (headline now reads the matrix
it describes, 9 ACCEPT / 11 MODIFY / 0 REJECT / 1 DEFER, column-verified 21/21);
3A and 3E both gained a HISTORICAL-WAVE RE-KEY preamble plus a separate live
SK-V18-keyed "V4 Open Questions" table, and 3E (V1: zero sk-v18/SPEC refs) now
cites sk-v18/SPEC 11× with the CSS narrative re-folded onto G2/H1. Deferral
integrity is materially restored: every LIVE deferral routes to a receiver/blocker
on the certified SK-V18 manifest, the lone 3C DEFER names its re-entry trigger,
and 3F's next-cycle directive carries concrete measurable entry conditions. No
naked "validated"-close survives; the 13 T-P2 refutations appear only as REJECT
gates / REDRESS fences, never as proposed deltas; no V1 surface is edited
(proposal-only honoured). CH6 cannot ACCEPT outright because two artefacts (3A,
3E) retain a carried HISTORICAL Open-Questions table whose ROW CELLS still
literally name retired SK-V15 waves (W4/W5/W6/W7/W8/W9/W10) as receiver+gate; the
re-key is supplied only as an out-of-band preamble, so a reader cost-routing a
row in isolation still lands on a gate the SK-V18 plan does not contain. This is a
tighten-not-reject REVISE — the deferral is demarcated NON-live, so it is not an
engineered close, but the row-level routing is not yet self-contained.

## Evidence Commands And Outputs

```sh
# THE GATE OBJECT NOW APPLIES (V1's lone REJECT is cleared):
$ awk '/^```diff$/{d=1;next} d&&/^```$/{exit} d{print}' \
    restart/audit/totality/p3/3C-locks-v+1-diff.md > /tmp/tp3-locks-v2.diff
$ git apply --check /tmp/tp3-locks-v2.diff           ; echo exit=$?   # exit=0
$ git apply --check --recount /tmp/tp3-locks-v2.diff ; echo exit=$?   # exit=0
# header now @@ -622,6 +622,33 @@ (was -622,6 +622,38 @@); leading context
# carries BOTH blank lines at LOCKS.md:623,624 -> lands cleanly before :625.
# LOCKS.md:622=Lock16 clause :623/:624=2 blanks :625=## v+1 Governance Boundary  (verified)

# 3C tally repaired (V1's CH6-V1-02 transposition cleared):
# disposition-column tally over the 21 matrix rows:
9 ACCEPT   11 MODIFY   1 DEFER   (21 rows; 0 silent drops)
# Executive Summary line 47 now reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"  (matches)

# PLANNED co-gate symbols genuinely rg=0 live (not laundered):
$ rg -l runtime_target_rows_collapsed skinny/crates skinny/xtask | wc -l   # 0
$ rg -l bbnf_simd_single_mask_convention skinny/crates | wc -l             # 0
$ rg -l lock14_gate_scans_codegen skinny/crates | wc -l                    # 0
# parse_w11_1_number STILL LIVE (3 files) -> 3F entry-cond grep==0 is a real future gate, not pre-met.

# SK-V18 manifest = P1..P5 / G1..G6 / PROVE / H1 / W-PRUNE  (no W1/W4/W5/W6/W7/W8/W9/W10)
$ grep -cE '\bW(1|4|5|6|7|8|9|10)\b' restart/skinny/tranches/sk-v18/SPEC.md   # 0
```

```sh
# per-artefact sk-v15 vs sk-v18 SPEC ref split (3E improved from V1's zero):
3A: sk-v15=35  sk-v18=14      3B: sk-v15=0  sk-v18=25     3C: sk-v15=0  sk-v18=0 (cites inline+LOCKS)
3D: sk-v15=0   sk-v18=19      3E: sk-v15=20 sk-v18=11     3F: sk-v15=0  sk-v18=25

# 3A: TWO Open-Q tables — carried HISTORICAL (143-151, cells name W1/W3/W5/W6/W4/W8/W9/W10)
#      + LIVE "## V4 Open Questions" (262-272): receivers G2/G3/G4/W-PROVE/SK-V19/Pass-Omega-ARCH-CRUD,
#      gates name SK-V18 falsifiers (value_ref_grammar_param_deleted, verbatim_blob_present==false).
# 3E: same shape — carried HISTORICAL (207-215, W2/W5/W6/W7) + LIVE "## V4 Open Questions" (374-383):
#      receivers 3C-LOCKS-diff/PROVE/G2-G6/SK-V19/H1; gates name SK-V18 commands + falsifiers.
# 3B: CLEANEST — SK-V15-routing deltas D03-D08/D11 REMOVED ("consumed by landed §13.5");
#      new MP-3B-SKV18-D01..D10 route to §13.7 12-wave manifest; bare W# only in §13.6→SK-V19 re-key ledger.
```

Load-bearing finding-ids spot-verified to resolve on-disk: `1E:147` (LAC-1E-V5-01
named-primitive), `1E:148` (LAC-1E-V5-02 relocated-seam), `1E:149/150/151/152/153`,
`1A:180` (1A-LOCK1-AMEND-001), `2C:213/217/218/380/381/382/215/251`, `2D:95/96/97/98`,
`2E:244/245/246`, `2F:194/195/196`. Cited LOCKS sections resolve: `:118` (substrate
manifest), `:137` (v+1 ELEVATION), `:408` (Pattern-H 67 baseline), `:620`
(generality-vehicle clause), `:622` (Lock 16 NEON clause), `:625` (governance
boundary). LOCKS numbered-lock count = 16 (governance invariant preserved).
SPEC re-fold targets resolve: `sk-v18/SPEC.md:439` (G2 CSS lowering), `:444` (H1
honesty close). The 21-candidate set (7×1E + 1×1A + 13×2X) resolves and 3C
disposes all 21 exactly once.

## Enumerated Deltas / Dispositions Under CH6

| # | artefact / object | judgement | basis |
|---|---|---|---|
| 1 | 3C v+1 diff — applies to live LOCKS.md (the G-Omega gate object) | **ACCEPT** | V1's lone REJECT is cleared: `git apply --check` AND `--recount` both exit 0; header corrected to `@@ -622,6 +622,33 @@`; leading context carries BOTH blanks at :623/:624. My lens's mandated spot-verify ("the v+1 diff applies") now PASSES. |
| 2 | 3C Executive Summary disposition tally | **ACCEPT** | V1's CH6-V1-02 transposition is fixed: line 47 reads "9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER"; the disposition column over 21 matrix rows tallies exactly 9/11/1. Headline matches the matrix it describes. |
| 3 | 3C disposition completeness (21/21, 0 silent drops) | **ACCEPT** | Each of the 21 candidates (LAC-1E-V5-01..07, 1A-LOCK1-AMEND-001, 2C-SK18-01..03, 2D-V3-01..04, 2E-V6-01..03, 2F-V3-01..03) dispositioned exactly once; no silent drop. |
| 4 | 3C DEFER (LAC-2F-V3-03) re-entry trigger | **ACCEPT** | Names a concrete re-entry trigger ("any 2F-class re-audit citing a 'balanced-scan gap' must `ls` both `parse-that/.../scan/` and `skinny/crates/bbnf-simd/src/`"), folded into D-SKV18-L16 as an audit-scope note, not dropped. Satisfies my lens's DEFER clause exactly. |
| 5 | 3C two PLANNED co-gate symbols (`runtime_target_rows_collapsed`, `bbnf_simd_single_mask_convention`) | **ACCEPT** | Both `rg`=0 live (verified); disclosed as PLANNED-at-SK-V18-SPEC, never cited as live symbols. No laundering of a planned gate into a met one. |
| 6 | 3A SK-V18 extension deltas (ARCH-3A-V4-SK18-D01..D14) | **ACCEPT** | Each cites a resolving 1A-1F / 2C-2F finding-id and a SK-V18 wave (P1-P5/G1-G6/PROVE/SK-V19); D14 self-flags "a routing note... NOT a closure." No prose-close. |
| 7 | 3A carried-verbatim V3 packet (ARCH-3A-V1-D01..D12) | **ACCEPT** | Frontmatter discloses SK-V15 authorship, "RETAINED VERBATIM as historical synthesis record"; not asserted as a current-cycle close. |
| 8 | 3A LIVE "V4 Open Questions" table (262-272) | **ACCEPT** | All 7 rows carry receiver/blocker/gate on the SK-V18 manifest (G2/G3/G4/W-PROVE/SK-V19/Pass-Omega-ARCH-CRUD) with concrete SK-V18 falsifiers. Genuine live deferral set; not a paper-close. |
| 9 | 3A carried HISTORICAL Open-Questions table (143-151) + re-key preamble | **REVISE** | The preamble (132-141) demarcates the rows NON-live and supplies a W#→SK-V18 re-key, so the deferral is NOT an engineered close — but the seven row cells THEMSELVES still name W1/W3/W5/W6/W4/W8/W9/W10 as receiver+gate, and the re-key lives only out-of-band. A reader cost-routing one row in isolation lands on a gate absent from the SK-V18 manifest. CORRECTION (3A author): inline the per-row SK-V18 re-key INTO each receiver/gate cell (e.g. "W4 Pattern H → P5/SK-V19 provenance") OR collapse the table to a one-line pointer to the V4 table, so no row routes to a retired W# in isolation. |
| 10 | 3E SK-V18 extension deltas (3E-D12..D18) | **ACCEPT** | Seven SK-V18-grounded deltas citing 2C SK-V18 ids (`2C:213/217/218`) + 1E V5 ids; CSS narrative re-folded onto G2 (`SPEC:439`) / H1 (`SPEC:444`); scopes fleet wording to witnessed grammars (3E-D18) with PROVE/SK-V19 as fleet receiver. Anti-overclaim discipline intact; closes V1's "zero sk-v18/SPEC refs" gap. |
| 11 | 3E LIVE "V4 Open Questions" table (374-383) | **ACCEPT** | Six rows, each receiver/blocker/gate on the SK-V18 manifest (3C-LOCKS-diff/PROVE/G2-G6/SK-V19/H1) with concrete SK-V18 commands + falsifiers. The CH6 row binds (d) PROFILE-PROVEN-NARROW-LEAF to the H1-re-captured profile — refuses crediting a checkasm PASS as a speedup close. |
| 12 | 3E carried HISTORICAL Open-Questions table (207-215) + re-key preamble | **REVISE** | Same shape as #9: preamble (197-205) demarcates NON-live and re-keys W2/W5/W6/W7, but the row cells still name W7/W4/W5/W6/W1/W2/W8/W9 as receiver+gate. CORRECTION (3E author): inline the per-row re-key into the receiver/gate cells OR reduce the historical table to a pointer to the V4 table. |
| 13 | 3B Open Questions (5 rows) + §13.7 NEW-wave consumers | **ACCEPT** | All rows carry receiver/blocker/gate; SK-V18/SK-V19-keyed (G1-G6/PROVE/H1/SK-V19); the SK-V15-routing deltas were REMOVED (consumed by landed §13.5) and replaced with §13.7-routed MP-3B-SKV18-D01..D10. Cleanest of the three carried-table artefacts: no live deferral routes to a retired W#. |
| 14 | 3D Open Questions (7 rows) + §6 (a)-(d) monotonic-fold bundle | **ACCEPT** | Full receiver/blocker/gate triad; SK-V18-keyed; the CH6 row demands the §6 (a)-(d) bundle "machine-checked, not prose" and the 3D-D11 cost row states "the four predicates are machine-checked, not prose." T-P3-proposes-only honoured. |
| 15 | 3D SK-V18 extension deltas (3D-D09/D10/D11) | **ACCEPT** | Cite 1D findings (G-9/D-1/G-6/D-4); re-anchored from SK-V15 W0-W11 to SK-V18 P1-P5/G1-G6/PROVE/H1; the one-generator inflection thesis is carried as a FINDING ("impl forked/replicated"), never a close. |
| 16 | 3F next-cycle dispatch directive | **ACCEPT** | Step 6 lists concrete measurable entry conditions (`x86_tree_deleted==true`, `runtime_target_rows_collapsed==true`, `lock14_gate_scans_codegen==true`, `grep -c parse_w11_1_number==0`); Step 4 records CRUD blocked/extension remainder with receiver/blocker/gate naming the exact remainder. Names Pass Omega V6 as receiver. |
| 17 | 3F Open Questions (4 rows) + 3F-MH-001..013 deltas | **ACCEPT** | Full receiver/blocker/gate triad; CH4/CH7 row forbids crediting an un-caveated "CSS >SOTA" close before the H1 re-lock; every 3F-MH delta carries receiver/blocker/gate + a concrete falsifier in the cost matrix; 3F is proposal-only ("does not amend MIGRATION.md or HANDOFF.md"). |
| 18 | T-P1/T-P2 governance honesty across the packet | **ACCEPT** | 3C and 3F carry T-P1 as near-converged NON-normal-§3Z (V7 lone clean, V8 broke streak, consec=0) and T-P2 as near-converged, never laundered to a normal two-clean §3Z lock; the v+1 addendum repeats the honest provenance. |
| 19 | Refuted-route non-revival (13 T-P2 refutations) | **ACCEPT** | tree-walk, wire-as-is `find_css_significant`, two-fan ≤13-byte composition, x86/AVX-512 close, bracket_depth_mask appear ONLY as REJECT gates / REDRESS-fences / forced-demotion obligations (3D:168 dead `#[cfg(test)]` kernel, 3E:316 "no live two-fan caller"), never as proposed deltas. |
| 20 | Cross-scope discipline (proposal-only, no V1-surface edit) | **ACCEPT** | `git status` shows no T-P3 edit to LOCKS/ARCHITECTURE/MASTER-PLAN/MIGRATION/HANDOFF; 3A D14 self-flags a routing note; the v+1 diff is labelled proposed-only, gated behind the governance boundary; no sixth shape / new directive / new substrate. |

## Findings (repair directives)

| id | severity | target lines | finding | repair | owner |
|---|---|---|---|---|---|
| CH6-V2-01 | MEDIUM | `restart/audit/totality/p3/3A-architecture-synthesis.md:143`-`151` | The carried HISTORICAL Open-Questions table is correctly demarcated NON-live with a W#→SK-V18 re-key PREAMBLE (132-141), but each of the 7 row cells still names a retired SK-V15 wave (W1/W3/W5/W6/W4/W8/W9/W10) as receiver+gate. The re-key is out-of-band, so a row read in isolation routes to a gate absent from the certified SK-V18 12-wave manifest. Not an engineered close (the rows are flagged historical, and a fully live SK-V18 table exists at 262-272), but the row-level routing is not self-contained. | Inline the per-row SK-V18 re-key into each receiver/gate cell (W1→P2/§7.4, W3/W5/W6→G1/G2/G3, W4→P5/SK-V19, W8/W9→G4/G5, W10→Lock-16 FNV quarantine) OR collapse the historical table to a one-line pointer to the V4 Open Questions table; no row may name a retired W# without its SK-V18 receiver in the same cell. | 3A author (V3 fold). |
| CH6-V2-02 | MEDIUM | `restart/audit/totality/p3/3E-grammar-generalisation.md:207`-`215` | Identical pattern: the carried HISTORICAL table is demarcated NON-live with a re-key preamble (197-205), but the 7 row cells still name W7/W4/W5/W6/W1/W2/W8/W9 as receiver+gate. A live SK-V18 V4 table exists at 374-383; the historical rows are not self-contained. | Inline the per-row re-key (W2→P4, W5/W6→G2, W7→G3∧PROVE∧SK-V19) into the receiver/gate cells OR reduce the historical table to a pointer to the V4 table. | 3E author (V3 fold). |

## Non-Findings Checked

- No uncited "validated"/"verified"/"proven" close: the standalone-`validated`
  scan across all six artefacts is empty; every "proven"/"verified" carries an
  adjacent path:line or a falsifier-command.
- No silent-dropped candidate: 3C disposes all 21 (7×1E + 1×1A + 13×2X), one
  disposition each (column tally 9 ACCEPT / 11 MODIFY / 1 DEFER, 0 REJECT); the
  lone DEFER carries a re-entry trigger and is folded as an audit-scope note.
- No revived refuted-route: the 13 T-P2 refutations appear only as REJECT gates /
  REDRESS-fences / forced-demotion obligations, never as proposed deltas.
- No cross-scope violation: all six artefacts are proposal-only; `git status`
  shows no T-P3 V1-surface edit; the v+1 diff is gated behind the governance
  boundary; no sixth shape / new directive / new substrate / lock retirement.
- No planned-gate laundering: `runtime_target_rows_collapsed`,
  `bbnf_simd_single_mask_convention`, `lock14_gate_scans_codegen` all `rg`=0 live;
  `parse_w11_1_number` is still live (3 files), so 3F's `grep -c ... == 0` entry
  condition is a genuine future gate, not pre-met.
- No G-Omega gate-object corruption: the v+1 diff applies cleanly (the V1 blocker
  is cleared); LOCKS numbered-lock count = 16; the five `BackendShape` variants are
  preserved.

## Residual Risk

This CH6 pass re-ran its mandated spot-verifications (the v+1 diff apply, the 3C
tally, the load-bearing finding-id resolution, the planned-gate `rg`=0, the
refuted-route non-revival, the SK-V18 manifest membership of every live receiving
gate) and did not re-run CH1's full citation-resolution matrix nor CH5's hidden-
coupling sweep. The two REVISE findings are cosmetic-routing tightenings on
demarcated-historical tables, not deferral-integrity breaches; the deferral
substance of the packet is sound. The carried-historical-table ambiguity is the
only CH6-class surface that survived the V1 fold; 3B already exemplifies the
correct fix (remove the consumed SK-V15-routing deltas outright rather than retain
them as a re-keyed historical table).

TALLY accept=18 revise=2 reject=0
