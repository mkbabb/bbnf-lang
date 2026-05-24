---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V4
disposition: ACCEPT
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
spot_checks_dispatched:
  - "V4 fold F-V4-CH1-1: 1F-anti AP-009 cosmetic refresh 24→27 (lightningcss_facts hit count at HEAD)"
  - "V4 fold F-V4-CH5-1: 1A `t_p1_v1_hardening_fold_note` `:117` cross-cite refresh (3 substitutions in fold-note text + 3 substitutions across 1A-DIV-008 row body); 1D row 117 sub-case cite refresh (CH5-005 Track 2 `:7,26,34,45` + CH5-004 CSS source-sidecar 9-cite cluster)"
  - "V4 fold F-V4-CH2-1: 1C exec summary single-token 126→127 (line 40); preserved tokens (1C:21, :23, :24, :92, :124, :162, :201) carry justified mechanical-extraction narrative"
  - "V4 fold F-V4-CH3-1: 1D row 140 W13.9 CORRECTNESS-REJECT label split from W13.5-W13.8 MEASURED-REJECT bundle"
  - "V4 fold F-V4-CH6-1: 1E:35 sustained-UNKNOWN paragraph added with 4 explicit verify_actions citing L03 + L16 + 2 NEW SK-V14 UNKNOWNs (audit-overlay column gap + Lock 1 fact-stream taxonomy)"
  - "V3-LOCKED axes drift check: 1B, 1F-coherence, 1F-past-corpora — git diff 0a9f1288c..8f4756113 empty across all three"
  - "Executable cite re-verification at V4 HEAD: every V3 cite (lightningcss_facts, fixture_sidecar_facts, same-plane-source-sidecar, Track 2 :7,26,34,45, proof-witness :29-33, reexport :25-71) re-grepped and confirmed"
  - "Cycle frontmatter inspection: 1A=V6, 1B=V6, 1E=SK-V14 (matches SK-V14 baseline binding per V3 CH1 schema clarification)"
verdict_summary:
  accept_rate: "8/8 (100%) artifacts CH1-clean at V4; single V3 housekeeping item (AP-009 24→27 cosmetic) discharged by F-V4-CH1-1; F-V4-CH5-1 row 100→117 refresh closes V3 finding-6 carry-forward; zero drift on three V3-LOCKED axes; no new CH1 defect surfaced"
  reject: 0
  revise: 0
  accept: 8
  v5_carry_forward: 0
head_commit_verified: 8f4756113a0332cc32414c9b0cbe95a3732d5e2c
---

# CH1 — CORRECTNESS lens disposition (T-P1 V4, LOCK-eligible confirming cycle)

## Verdict

ACCEPT. All eight T-P1 V4 inventories pass CH1 spot-check at full
citation resolution against live source at HEAD `8f4756113` (T-P1 V4
atomic micro-fold + S-P3 V1 CHALLENGE-CONTEXT commit). The V4 atomic
micro-fold packet discharges every V3 housekeeping item that fell
inside CH1 scope (F-V4-CH1-1 AP-009 24→27 cosmetic refresh +
F-V4-CH5-1 row 100→117 cosmetic refresh); the three V3-LOCKED axes
(1B, 1F-coherence, 1F-past-corpora) carry zero drift to V4; and no new
CH1 defect surfaces under V4 re-verification.

This is the **third consecutive 100% ACCEPT cycle for CH1**
(V2 100% → V3 100% → V4 100%). Per `ORCHESTRATOR.md §3Z` (cohort LOCK
= ≥95% × 2 consecutive cycles), CH1 has been LOCK-eligible since the
V3 close; the V4 confirming cycle delivers a third consecutive ≥95%
pass and overshoots the LOCK precondition by one full cycle. CH1 stands
ready for §3Z cohort-wide LOCK ratification at the V4 aggregator stage,
contingent on the other five lenses (CH2-CH6) also returning ≥95% at
V4.

REJECT: none.
REVISE: none.

The prior V4 CH1 report on disk before this rewrite (dated stub
referencing "cycle: V4 metadata-only fold" framing) misread both the
frontmatter schema and the V4 fold packet substance. Direct inspection
at HEAD disproves: 1A carries `cycle: V6`, 1B carries `cycle: V6`,
1E carries `cycle: SK-V14`, and the remaining five inventories carry
`cycle: V6` — matching the SK-V14 baseline binding established at V3
CH1 close (the inventories are V6-converged baselines folded under
SK-V14 T-P1 binding, NOT V4-cycle artefacts). The V4 fold packet is
also NOT "metadata-only" — it carries five substantive micro-folds
(F-V4-CH1-1 + F-V4-CH2-1 + F-V4-CH3-1 + F-V4-CH5-1 + F-V4-CH6-1). The
prior stub mis-read both schema and substance; this V4 CH1 ACCEPT
supersedes it.

## V3 Housekeeping Discharge Verification (CH1-bound at V4)

### F-V4-CH1-1 — AP-009 `lightningcss_facts` 24→27 cosmetic refresh

V3 CH1 disposition raised a single non-defect housekeeping note: the
AP-009 row's inline V3-correction text cited "24 hits" for
`lightningcss_facts` at HEAD when the live count was 27. V3 CH1 marked
this off-by-3 as cosmetic-only (substance "REBIND not REMOVE" holds
from any non-zero count) and recommended V4 refresh "for hygiene; not
a CH1 defect." V4 fold packet F-V4-CH1-1 executes the refresh.

Re-verification at HEAD `8f4756113`:

```
$ grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
27

$ grep -n '27 hits\|24 hits' restart/audit/totality/p1/1F-anti-pattern.md
69:| AP-009 | ... grep -n 'lightningcss_facts' ... returns 27 hits (definition + 7 per-grammar siblings + call sites). | ...
```

The V4 diff is a one-token replacement at 1F-anti-pattern.md:69
(`24 hits` → `27 hits`) inside the existing V3 cite-rebind narrative;
no surrounding prose changed; cosmetic-only refresh confirms the V3
self-correction note's live count. Cite refresh verified, cosmetic
housekeeping discharged. ACCEPT.

Note: AP-009 row at `1F-anti-pattern.md:94` (Planning Metadata) and
AP-020 row at `:80`,`:105` were untouched by V4 — those rows already
carried no count-literal narrative, only the cite cluster + provenance.
No further refresh required on those rows.

### F-V4-CH5-1 — Row 100 → row 117 cosmetic cross-cite refresh

V3 CH1 disposition observed that several rows still carried stale `:100`
pointers to the 1D substrate-union row when the live row number at HEAD
is 117 (post-V2 renumbering). V4 fold packet F-V4-CH5-1 executes the
cross-cite refresh across 1A and 1D (and the related cite cluster
refresh in 1D for the row 117 sub-cases).

Re-verification at HEAD `8f4756113`:

```
$ grep -n 'row 100\|row 117\|:100\|:117' \
    restart/audit/totality/p1/1A-substrate-evidence.md \
    restart/audit/totality/p1/1D-skinny-lessons.md \
    restart/audit/totality/p1/1F-anti-pattern.md
restart/audit/totality/p1/1A-substrate-evidence.md:10:... '1D `:117` records 'Single substrate proved as substrate cardinality' ...
restart/audit/totality/p1/1A-substrate-evidence.md:84:... 1D `:117` records "Single substrate proved as substrate cardinality" ...
restart/audit/totality/p1/1D-skinny-lessons.md:157:... T-P3 §3C disposition (folds into row 117 substrate-union ratification) ...
```

Zero residual `:100` or `row 100` pointers across the three CH5-coupled
files. The 1A `t_p1_v1_hardening_fold_note` (line 10) carries three
`:117` substitutions; the 1A-DIV-008 row body (line 84) carries three
`:117` substitutions; the 1D CH5-005 row (line 157) carries the `:117`
substrate-union ratification cross-cite. F-V4-CH5-1 row 117 cite
refresh fully discharged.

Companion sub-case cite refresh in 1D row 117 also verified at HEAD:

```
$ sed -n '7p;26p;34p;45p' skinny/crates/bbnf-bench/src/track2/json.rs
    tape::{CapacityPlan, OffsetFlags, TapeBuilder},
        let capacity = runtime::grammars::json::scan::structural_capacity_for(
            tape: TapeBuilder::new(input.as_bytes(), capacity),
        Ok(JsonRoot::from_tape(self.input, self.tape.finish()))

$ grep -n 'track2/json.rs:7,26,34,45\|nonjson_css_l4.rs:648, 1082' \
    restart/audit/totality/p1/1D-skinny-lessons.md
117:... Track 2 substrate-helper sharing (CH5-005 / `bbnf-bench/src/track2/json.rs:7,26,34,45`), CSS source-sidecar (CH5-004 / `nonjson_css_l4.rs:648, 1082, 1203, 1354, 1511, 1661, 1815, 1964, 2691`), ...
```

The row 117 cite cluster (CH5-005 Track 2 `:7,26,34,45` + CH5-004 CSS
source-sidecar 9-cite cluster + CH5-007 proof-witness `:29-33`) is
fully refreshed to V3-rebound cites — closing the V3 CH1 housekeeping
quibble that the row body still narrated old V1/V2 cite text. ACCEPT.

## V3-LOCKED Axes — Zero V4 Drift Verification

The V4 fold packet declared three V3-LOCKED axes (1B, 1F-coherence,
1F-past-corpora) untouched. Verification:

```
$ git diff 0a9f1288c..8f4756113 -- \
    restart/audit/totality/p1/1B-codegen-evidence.md \
    restart/audit/totality/p1/1F-coherence-scan.md \
    restart/audit/totality/p1/1F-past-corpora.md | wc -l
0
```

Zero lines of diff across all three V3-LOCKED axes between the V3
amended HEAD `0a9f1288c` and the V4 amended HEAD `8f4756113`. No drift.
V3 ACCEPT chain carries forward unchanged for these three axes. ACCEPT.

(Note: this V4 cycle has three V3-LOCKED axes, not four — the V3 cycle
had four because 1E was V2-LOCKED through V3; V4 fold packet F-V4-CH6-1
amended 1E for the sustained-UNKNOWN paragraph, which is a CH6 disposition
fold but not a CH1 drift. The three V3-LOCKED axes that remain LOCKED
at V4 are 1B + the two 1F sibling files.)

## Non-CH1 V4 Folds — Verify Co-Disposition Concurrence at HEAD

V4 also folded three non-CH1 dispositions that touch inventories
shared with CH1 spot-checks. Verify each fold leaves V3-bound CH1 cites
intact (negative-evidence audit):

### F-V4-CH2-1 — 1C exec summary 126→127 single-token

```
$ grep -n '126 grammar\|127 grammar' restart/audit/totality/p1/1C-runtime-evidence.md
21:    - Update the Lock 14 leak audit ... 127 grammar-named type reexports ...
23:    - V1-fold (CH2 GENERALITY): ... 127 distinct grammar-named symbols ...
24:    - V2-fold (F-V3-CH2-1 off-by-one repair): ... Correct count subtracting only the 6 in-window neutrals is **127**, not 126. ...
40:... + **127 grammar-named type reexports** ...
92:... reexports **127 distinct grammar-named symbols** ...
124:... reexports **127 distinct grammar-named symbols** ...
162:... + **127 grammar-named type reexports** ...
201:... → **127 distinct grammar-named symbols** ...
```

Eight `127` occurrences at HEAD; zero unjustified `126` occurrences
remaining (the two `126` mentions at `:23` and `:24` sit inside the V1+V2
historical-fold narrative explicitly framed as "the V2 cycle cited
'126'… correct count is **127**", which preserves the fold history while
binding the live count to 127). Per-grammar breakdown re-derives 127:
10+10+43+10+10+10+11+13+10 = 127. F-V4-CH2-1 exec summary single-token
fold satisfies NEW-CH2-V3-02 orphan-cell propagation guard. ACCEPT
(CH2 lens disposition; CH1 verifies cite arithmetic resolves).

### F-V4-CH3-1 — 1D row 140 W13.9 CORRECTNESS-REJECT label split

```
$ grep -n 'W13.5-W13.8\|W13.9 CORRECTNESS' restart/audit/totality/p1/1D-skinny-lessons.md
140:... V2 fold (CH3-005 split #1): W13.5-W13.8 MEASURED-REJECT at `REDRESS.md:4621/4645/4674/4704`; W13.9 CORRECTNESS-REJECT at `:4734` — NOT PASS-ADMIT, NOT part of the audit-falsified admit tally, and MUST NOT be treated as reopen candidates. ...
```

W13.5-W13.8 MEASURED-REJECT label bound to four REDRESS line cites
(`:4621/4645/4674/4704`); W13.9 CORRECTNESS-REJECT label separately
bound to `:4734`. Both verdict labels distinct and individually
attributed. No REDRESS route re-opened. F-V4-CH3-1 label split
discipline confirmed. ACCEPT (CH3 lens disposition; CH1 verifies REDRESS
cites resolve to the bound REJECT verdicts).

### F-V4-CH6-1 — 1E:35 sustained-UNKNOWN paragraph

```
$ sed -n '35p' restart/audit/totality/p1/1E-locks-evidence.md
**Sustained-UNKNOWN posture (anti-paper-close anchor; F-V4-CH6-1 close of V1 CH6 REVISE #4 + CH1 V3 finding 7 carry-forward).** Four UNKNOWNs survive V4/V5 fold ...

$ sed -n '161,164p' restart/audit/totality/p1/1E-locks-evidence.md
| L03 cursor elision | sustained from V4 — no `__EAGER_EMPTY_PATH` artifact at SK-V14 baseline | sustained from V4 |
| L16 full allowlist coverage | sustained from V4 — V+1 primitive manifest binding present in LOCKS.md but per-use-site mapping artifact still pending | sustained from V4 |
| **NEW SK-V14: Does SK-V14 SYNTHESIS §2 audit-overlay column binding require any current row's xtask gate-json delta beyond R1 + R2 + CH5 wave deliverables?** | The 4 NEW columns map to C-2 wave deliverable per `SYNTHESIS.md:272` C-2 row; ... | Verify in C-2 redress: capture `grep -c 'track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict' skinny/RESULTS.md` ... |
| **NEW SK-V14: Does Lock 1 V+1 fact-stream wording at `LOCKS.md:66-71` already admit CSS L4 as 5th substrate category, or does LAC-1E-14 require explicit `FactStream` taxonomy addition?** | The V+1 text says fact streams "are output-plane contracts, not retained internal sidecars" ... | T-P3 disposes: either (a) explicit `FactStream` taxonomy addition extends BackendShape to 5 variants (changes Lock 10 too), or (b) fact-stream stays as `admitted_fact_output` substrate_target per V+1 §75-82 without taxonomy promotion. |
```

All four sustained-UNKNOWNs (L03 at :161, L16 at :162, audit-overlay
column gap NEW SK-V14 at :163, Lock 1 fact-stream taxonomy NEW SK-V14
at :164) carry explicit, executable `verify_action` attributes. F-V4-CH6-1
paragraph at 1E:35 cites all four with their `:161-164` table-row
anchors. The V1 CH6 REVISE #4 carry-forward (sustained-UNKNOWN
paragraph) is discharged at V4; the V3 CH1 finding-7 carry-forward
(same item, CH6 lens) is concurrently discharged. ACCEPT (CH6 lens
disposition; CH1 verifies the verify_action cites and table anchors
resolve).

## Static-Census Re-Verification at V4 HEAD

V2/V3 CH1 verified four executable-mandate counts. All four re-verify
at V4 HEAD `8f4756113`:

```
$ grep -n "CH7\|Overfit" restart/locks/LOCKS.md
(no output; exit 1 = zero matches — LAC-1E-12 CH7 binding still candidate, not landed in LOCKS.md)

$ find crates/core/src/runtime/google_sheets -type f | wc -l
10

$ rg -n 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' \
       crates/core/src/runtime/ | wc -l
30

$ find crates/core/src/runtime -mindepth 1 -maxdepth 1 -type d | wc -l
9
```

All four match V2/V3 baseline exactly. V4 introduced no static-census
regression. ACCEPT.

Additionally, the V4-specific `lightningcss_facts` count re-verifies at
27 (matches the V4 fold target):

```
$ grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
27
```

## Cycle Frontmatter Inspection (V3 CH1 schema clarification preserved)

Frontmatter cycle fields re-inspected at V4 HEAD:

```
$ grep -n '^cycle:' restart/audit/totality/p1/1A-substrate-evidence.md \
                    restart/audit/totality/p1/1B-codegen-evidence.md \
                    restart/audit/totality/p1/1E-locks-evidence.md
restart/audit/totality/p1/1A-substrate-evidence.md:4:cycle: V6
restart/audit/totality/p1/1B-codegen-evidence.md:4:cycle: V6
restart/audit/totality/p1/1E-locks-evidence.md:4:cycle: SK-V14
```

Matches the V3 CH1 schema clarification: 1A/1B carry `cycle: V6`
(V6-converged baseline folded under SK-V14 T-P1 binding); 1E carries
`cycle: SK-V14` (lock evidence is the SK-V14 audit-corrected baseline
binding). The other five inventories continue to carry the same cycle
fields as V3. No frontmatter regression. The pre-V3 CH1 mis-read
(which alleged a REVISE on the basis of cycle field reading) and the
pre-V4 CH1 stub mis-read (which alleged "cycle: V4 metadata-only" as
V4 substance framing) are both vacated by direct frontmatter
inspection. ACCEPT.

## CH7 V2 Dispatch Self-Correction Discipline at V4

The V3 CH1 audit established that every V3 cite-rebind site carries an
inline V2-dispatch-correction note (anti-fabrication discipline at
maximum). At V4, F-V4-CH1-1 refresh preserved this discipline — the
"24 hits" → "27 hits" cosmetic refresh sits inside the V3 self-
correction paragraph and the note text continues to call out
"CH7 V2 dispatch assertion 'lightningcss_facts has zero hits' was
itself off." The refresh updates the LIVE-COUNT-AT-HEAD anchor without
removing the V2-dispatch self-correction provenance. ACCEPT.

LAC-1E-12 procedural addendum (executable `grep -n` verification at
HEAD for every cite-bearing micro-fold) is now institutionalised
in `1A-substrate-evidence.md:10` and applied consistently across the
V4 fold packet. NEW-CH2-V3-02 orphan-cell propagation guard (every
non-cited count token in the inventory body must be justified with
inline rationale, not silent value-only mention) is satisfied at every
V4 fold site verified above.

## CH1 Carry-Forward (V4 → V5)

Zero CH1 carry-forwards remain at V4 close. The V3 → V4 single-item
CH1 carry-forward (AP-009 cosmetic 24→27) is fully discharged by
F-V4-CH1-1. The V1 CH6 REVISE #4 sustained-UNKNOWN paragraph carry-
forward (which had been tracked under V3 CH1 as "finding 7" though
classified as CH6 lens) is fully discharged by F-V4-CH6-1. No new CH1
defect surfaces at V4.

## ACCEPT-rate summary

| Artefact | CH1 V4 disposition | Notes |
|---|---|---|
| 1A-substrate-evidence.md | ACCEPT | F-V4-CH5-1 `:117` cross-cite refresh executes (6 substitutions across :10 fold-note + :84 1A-DIV-008 row); zero residual `:100` orphans; SUB-014 cite cluster + 1A-DIV-008 substrate-union nuance preserved exactly per V3 binding |
| 1B-codegen-evidence.md | ACCEPT | V3-LOCKED; zero V4 drift confirmed (`git diff 0a9f1288c..8f4756113 -- 1B = 0 lines`) |
| 1C-runtime-evidence.md | ACCEPT | F-V4-CH2-1 single-token exec summary refresh 126→127 (line :40); NEW-CH2-V3-02 orphan-cell propagation guard satisfied (preserved tokens at :21, :23, :24, :92, :124, :162, :201 carry justified mechanical-extraction narrative); per-grammar breakdown re-derives 127 = 10+10+43+10+10+10+11+13+10 |
| 1D-skinny-lessons.md | ACCEPT | F-V4-CH3-1 W13.9 CORRECTNESS-REJECT label split from W13.5-W13.8 MEASURED-REJECT bundle (line :140); F-V4-CH5-1 row 117 sub-case cite refresh (CH5-005 Track 2 `:7,26,34,45`; CH5-004 CSS source-sidecar 9-cite cluster; CH5-007 proof-witness `:29-33`) at line :117 |
| 1E-locks-evidence.md | ACCEPT | F-V4-CH6-1 sustained-UNKNOWN paragraph added at :35 listing L03 + L16 + 2 NEW SK-V14 UNKNOWNs; all 4 verify_actions cite table rows `:161-164` and re-verify at HEAD; V1 CH6 REVISE #4 + V3 CH1 finding 7 carry-forward concurrently discharged |
| 1F-coherence-scan.md | ACCEPT | V3-LOCKED; zero V4 drift |
| 1F-anti-pattern.md | ACCEPT | F-V4-CH1-1 AP-009 cosmetic refresh 24→27 hits (line :69) — `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns 27 at HEAD; V3 cite-rebind narrative preserved exactly; AP-011, AP-020, AP-009-planning-row, AP-020-planning-row all untouched (no count-literal narrative to refresh) |
| 1F-past-corpora.md | ACCEPT | V3-LOCKED; zero V4 drift |

**ACCEPT-rate: 8/8 = 100.0%.** Third consecutive 100% ACCEPT cycle for
T-P1 CH1 (V2 100% → V3 100% → V4 100%). Cohort §3Z LOCK precondition
for CH1 lens met by **two full cycles of margin** (only ≥95% × 2
required; CH1 has delivered 100% × 3).

## Cycle disposition

**V4 LOCK-eligible confirming cycle ACHIEVED for CH1** (100% ACCEPT;
third consecutive ≥95% cycle). All V3 housekeeping items (F-V4-CH1-1
AP-009 24→27 cosmetic + F-V4-CH5-1 row 100→117 cosmetic) fully
discharged by V4 fold packet; zero drift on three V3-LOCKED axes (1B,
1F-coherence, 1F-past-corpora); zero new CH1 defect surfaced at V4;
non-CH1 V4 folds (F-V4-CH2-1, F-V4-CH3-1, F-V4-CH6-1) all leave V3-bound
CH1 cites intact and resolve cleanly at HEAD; CH7-V2-failure-mode
(V→V+1 cite-carry without re-verification) discipline preserved (LAC-1E-12
+ NEW-CH2-V3-02 institutionalised at the V3 close and applied at every
V4 micro-fold).

**LOCK confirmation:** §3Z cohort LOCK precondition for CH1 lens is
**MET WITH MARGIN** (3 consecutive ≥95% cycles where 2 required). CH1
stands ready for cohort-wide LOCK ratification at the V4 aggregator
stage. Final §3Z LOCK ratification is contingent on the other five
lenses (CH2-CH6) also returning ≥95% at V4 — that cohort-aggregation
is the V4 aggregator's responsibility, not this CH1 lens's.

**Predicted trajectory:** V4 CH1 100% → V5 CH1 ≥95% expected (if V5
runs; not required for cohort LOCK since precondition already met at
V4). No CH1-specific regression risk identified.

## Notes on accuracy quibbles (non-defects)

1. The 1F-anti-pattern V4 fold (F-V4-CH1-1) did not add an explicit
   `F-V4-CH1-1` provenance label to the AP-009 row itself; the row's
   V3 cite-rebind paragraph was edited in place with the cosmetic
   24→27 token swap. This is consistent with the V4 atomic micro-fold
   commit message's framing of F-V4-CH1-1 as a "cosmetic refresh"
   rather than a full fold-note addition. The V3 cite-rebind authority
   (`HARDENING-T-P1-V2-CONSOLIDATED §3.1 F-V3-CH7-2`) remains the
   authoritative provenance for the AP-009 rebind narrative; F-V4-CH1-1
   refresh sits inside that V3 narrative without creating a new
   provenance layer. Acceptable per the LOCK-eligible cycle's
   stricter-discipline framing (the V4 commit message documents the
   refresh; the in-file narrative carries the live count without
   introducing a new fold-note that would itself need re-verification
   in V5). Not a CH1 defect; design choice.

2. 1E:35 sustained-UNKNOWN paragraph cites `1E-locks-evidence.md:161-164`
   as the verify_action anchor — this is a SELF-reference inside the
   same file. Verified at HEAD: lines 161-164 do hold the four
   sustained-UNKNOWN rows in the Open Questions table; the self-cite
   resolves. Self-referential cite style is uncommon but unambiguous
   here (the paragraph is a header summary pointing to the detail
   table). Not a CH1 defect.

3. The V4 cycle inventories continue to carry V5-converged content
   plus V6/SK-V14 fold note (per V3 schema clarification). The
   `prior_cycle_dispositions_folded.accepted` list in 1A line 12
   lists `V4-CH*-...` and `V5-CH*-ACCEPT` entries — these are the
   *prior totality* V4/V5 cycles (the SK-V14 baseline that the
   SK-V14 T-P1 fold absorbs), NOT the current V4 cycle's dispositions.
   The current V4 cycle's dispositions would be folded into V5 via the
   V5 dispatch context if V5 runs. Schema preserved per V3 CH1 finding.
   Not a CH1 defect.

4. The pre-V4 CH1 stub on disk (timestamp pre-dating this V4 dispatch
   context) framed V4 as "metadata-only fold" with all 1A-1F carrying
   `cycle: V4` — direct frontmatter inspection at HEAD disproves both
   claims (V4 fold packet carries 5 substantive micro-folds; 1A/1B/1C/
   1D/1F-* carry `cycle: V6`, 1E carries `cycle: SK-V14`). The stub
   mis-read both schema and substance and its ACCEPT findings rest on
   incorrect premises; this V4 CH1 ACCEPT (resting on the corrected
   schema reading + verified V4 fold substance) supersedes it.
