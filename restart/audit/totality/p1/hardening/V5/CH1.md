---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V5
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
  - "V5 fold F-V5-CH6-1: 1E:35 sustained-UNKNOWN paragraph self-cite anchor refresh `:126-128` → `:128-130` (single-token cosmetic; relocates §1.5 LAC-1E-12 promotion candidacy block pointer to live header+body line range)"
  - "V4-LOCKED axes drift check (7 inventories): `git diff 8f4756113..9833295d5 -- 1A 1B 1C 1D 1F-coherence 1F-anti-pattern 1F-past-corpora` returns 0 lines"
  - "1E V4→V5 diff isolation: only line 35 cross-reference token changes (`:126-128` → `:128-130`); zero other modifications"
  - "F-V5-CH6-1 anchor target re-verification at V5 HEAD: 1E:128 = '### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)' header; 1E:129 = blank separator; 1E:130 = full T-P1 V2 promotion paragraph body; anchor range :128-130 correctly bounds the §1.5 governance signal block"
  - "Four sustained-UNKNOWNs at 1E:161-164 re-verified at V5 HEAD (L03 at :161, L16 at :162, audit-overlay column gap NEW SK-V14 at :163, Lock 1 fact-stream taxonomy NEW SK-V14 at :164); all four verify_action attributes resolve unchanged"
  - "Executable cite re-verification at V5 HEAD: lightningcss_facts count = 27 (matches AP-009 F-V4-CH1-1 binding); 127 grammar-named reexports (matches F-V4-CH2-1 NEW-CH2-V3-02 binding); google_sheets file count = 10; runtime dirs = 9; parser census = 30; LOCKS.md 'CH7\\|Overfit' grep = 0 matches (LAC-1E-12 still candidate)"
  - "Cycle frontmatter inspection: 1A=V6, 1B=V6, 1E=SK-V14 (unchanged from V4)"
verdict_summary:
  accept_rate: "8/8 (100%) artifacts CH1-clean at V5; V5 atomic cosmetic fold F-V5-CH6-1 (single-token anchor refresh at 1E:35) discharged and verified; zero V5 drift on 7 V4-LOCKED axes; no new CH1 defect surfaced"
  reject: 0
  revise: 0
  accept: 8
  v6_carry_forward: 0
head_commit_verified: 9833295d5a295938019de54af2411c24e386530e
---

# CH1 — CORRECTNESS lens disposition (T-P1 V5, LOCK-TRIGGER cycle)

## Verdict

ACCEPT. All eight T-P1 V5 inventories pass CH1 spot-check at full
citation resolution against live source at HEAD `9833295d5` (T-P1 V5
atomic cosmetic-fold commit + V5 LOCK-trigger dispatch context). The
V5 fold packet is a single trivial cosmetic anchor refresh
(F-V5-CH6-1: 1E:35 self-cite `:126-128` → `:128-130`); the seven
V4-LOCKED axes (1A/1B/1C/1D/1F-coherence/1F-anti-pattern/1F-past-corpora)
carry zero drift to V5; and no new CH1 defect surfaces under V5
re-verification.

This is the **fourth consecutive 100% ACCEPT cycle for CH1**
(V2 100% → V3 100% → V4 100% → V5 100%). Per `ORCHESTRATOR.md §3Z`
(cohort LOCK = ≥95% × 2 consecutive cycles; V≤5 ceiling), CH1 has
been LOCK-eligible since the V3 close; the V4 confirming cycle
delivered the LOCK precondition with margin; the V5 LOCK-TRIGGER
cycle is the second consecutive cohort-wide ≥95% pass that triggers
cohort §3Z LOCK ratification at V≤5 ceiling EXACTLY. CH1 lens is now
LOCK-MET with **two full cycles of margin** beyond the §3Z
precondition (only 2 cycles required at ≥95%; CH1 has delivered 4
cycles at 100%).

REJECT: none.
REVISE: none.

The prior V5 CH1 report on disk before this rewrite (stub framing
inventories as `cycle: V4` and claiming the V4→V5 repair was
"metadata-only/no substantive evidence change") mis-read the
inventory cycle fields at HEAD and mis-characterised the V5 fold
substance. Direct inspection at HEAD disproves both claims:
1A carries `cycle: V6`, 1B carries `cycle: V6`, 1E carries
`cycle: SK-V14`, and the remaining five inventories carry `cycle: V6`
(matching the V3 CH1 schema clarification preserved at V4 close).
The V5 fold packet is also NOT "metadata-only" — F-V5-CH6-1 is a
single-token in-body cosmetic anchor refresh at 1E:35 (`:126-128` →
`:128-130`), a real diff. The prior stub mis-read both schema
(cycle fields) and substance (V5 fold packet); this V5 CH1 ACCEPT
supersedes it. This supersession parallels the V4 CH1 ACCEPT's
supersession of an analogous pre-V4 stub (V4 CH1 §Notes-on-accuracy-quibbles
#4) — confirming the stub-shadow regression is a recurring V→V+1
cycle-frontmatter mis-read class that LAC-1E-12 + NEW-CH2-V3-02
discipline is designed to catch.

## V5 Fold Discharge Verification (F-V5-CH6-1, CH1-bound)

### F-V5-CH6-1 — 1E:35 self-cite anchor refresh `:126-128` → `:128-130`

V5 atomic cosmetic-fold packet executes exactly one substitution: the
V4-introduced sustained-UNKNOWN paragraph at 1E:35 carries a closing
cross-reference to the `§1.5 LAC-1E-12 promotion candidacy block`,
and that anchor pointer was stale at V4 close (pointed to `:126-128`
when the actual block had shifted to `:128-130` due to intermediate
LAC additions LAC-1E-15 + LAC-1E-16 at V4-fold time). V5 fold
F-V5-CH6-1 refreshes the anchor token to the live line range.

Re-verification at HEAD `9833295d5`:

```
$ git diff 8f4756113..9833295d5 -- restart/audit/totality/p1/1E-locks-evidence.md
diff --git a/restart/audit/totality/p1/1E-locks-evidence.md b/restart/audit/totality/p1/1E-locks-evidence.md
@@ -32,7 +32,7 @@ locks_amendment_candidates: 16
 ...
-...Cross-reference to §1.5 LAC-1E-12 promotion candidacy block at `1E-locks-evidence.md:126-128`...
+...Cross-reference to §1.5 LAC-1E-12 promotion candidacy block at `1E-locks-evidence.md:128-130`...
```

Single line modified; zero other diff bytes inside 1E. Anchor target
resolution at V5 HEAD:

```
$ sed -n '128,130p' restart/audit/totality/p1/1E-locks-evidence.md
### LAC-1E-12 promotion candidacy (T-P1 V2 fold — §1.5 governance signal)

T-P1 V2 promotes **LAC-1E-12 from candidate-addition to candidate-promoted-to-T-P3-§3C-priority** as the most substantive cross-lens governance signal surfaced by V1 hardening. ...
```

Line 128 is the section header `### LAC-1E-12 promotion candidacy
(T-P1 V2 fold — §1.5 governance signal)`; line 129 is the blank
separator; line 130 is the full T-P1 V2 promotion paragraph body
(running on a single long line). The refreshed `:128-130` anchor
correctly bounds the §1.5 governance signal block in its entirety —
header + separator + paragraph body. F-V5-CH6-1 refresh cosmetic
discharge verified, anchor binds the intended block at HEAD. ACCEPT.

The pre-V5 anchor `:126-128` at V4 HEAD `8f4756113` resolved to the
last two LAC table rows (LAC-1E-15 + LAC-1E-16) plus the closing
table newline + section break, NOT the §1.5 governance signal block;
the V5 refresh corrects this drift. The shift of 2 lines (`126→128`)
is consistent with the V3→V4 LAC table extension that added two new
LACs (LAC-1E-15, LAC-1E-16) at the V4 fold cycle, pushing the
downstream §1.5 header down by two lines.

## V4-LOCKED Axes — Zero V5 Drift Verification (7 inventories)

The V5 fold packet declared seven V4-LOCKED axes
(1A/1B/1C/1D/1F-coherence/1F-anti-pattern/1F-past-corpora) untouched.
Mechanical drift verification:

```
$ git diff 8f4756113..9833295d5 -- \
    restart/audit/totality/p1/1A-substrate-evidence.md \
    restart/audit/totality/p1/1B-codegen-evidence.md \
    restart/audit/totality/p1/1C-runtime-evidence.md \
    restart/audit/totality/p1/1D-skinny-lessons.md \
    restart/audit/totality/p1/1F-coherence-scan.md \
    restart/audit/totality/p1/1F-anti-pattern.md \
    restart/audit/totality/p1/1F-past-corpora.md | wc -l
0
```

Zero lines of diff across all seven V4-LOCKED axes between the V4
amended HEAD `8f4756113` and the V5 amended HEAD `9833295d5`. No
drift. V4 ACCEPT chain carries forward unchanged for these seven
axes. The V4 CH1 ACCEPT findings (F-V4-CH1-1 AP-009 24→27 cosmetic;
F-V4-CH5-1 row 100→117 cross-cite refresh + sub-case cite cluster
refresh; F-V4-CH2-1 1C exec summary 126→127 single-token + NEW-CH2-V3-02
orphan-cell propagation guard satisfied; F-V4-CH3-1 W13.9
CORRECTNESS-REJECT label split; F-V4-CH6-1 1E:35 sustained-UNKNOWN
paragraph) all carry forward bit-for-bit to V5 HEAD without
modification. ACCEPT.

## Static-Census Re-Verification at V5 HEAD

V2/V3/V4 CH1 verified five executable-mandate counts; all re-verify
at V5 HEAD `9833295d5` identical to V4 baseline:

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

$ grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs
27
```

All five match V4 baseline exactly. V5 introduced no static-census
regression. The four substrate counts (google_sheets=10, runtime
dirs=9, parser census=30, lightningcss_facts=27) plus the LOCKS.md
CH7-silence sentinel (zero matches) all hold at V5 HEAD. ACCEPT.

## Cycle Frontmatter Inspection (V3 CH1 schema clarification preserved at V5)

Frontmatter cycle fields re-inspected at V5 HEAD:

```
$ grep -n '^cycle:' restart/audit/totality/p1/1A-substrate-evidence.md \
                    restart/audit/totality/p1/1B-codegen-evidence.md \
                    restart/audit/totality/p1/1E-locks-evidence.md
restart/audit/totality/p1/1A-substrate-evidence.md:4:cycle: V6
restart/audit/totality/p1/1B-codegen-evidence.md:4:cycle: V6
restart/audit/totality/p1/1E-locks-evidence.md:4:cycle: SK-V14
```

Matches the V3 CH1 schema clarification (preserved at V4, preserved
at V5): 1A/1B carry `cycle: V6` (V6-converged baseline folded under
SK-V14 T-P1 binding); 1E carries `cycle: SK-V14` (lock evidence is
the SK-V14 audit-corrected baseline binding). The other five
inventories continue to carry the same cycle fields as V4. No
frontmatter regression. The V5 atomic cosmetic fold did NOT amend
the 1E cycle frontmatter field — the `cycle: SK-V14` binding is
preserved bit-for-bit. ACCEPT.

## Sustained-UNKNOWN Posture Re-Verification at V5 HEAD

The V4 fold F-V4-CH6-1 introduced the sustained-UNKNOWN paragraph at
1E:35 with four UNKNOWN rows cited at `:161-164`. V5 F-V5-CH6-1
refreshed only the §1.5 self-cite anchor; the paragraph's primary
table-row anchors `:161-164` were NOT modified. Verification:

```
$ sed -n '161,164p' restart/audit/totality/p1/1E-locks-evidence.md
| L03 cursor elision | sustained from V4 — no `__EAGER_EMPTY_PATH` artifact at SK-V14 baseline | sustained from V4 |
| L16 full allowlist coverage | sustained from V4 — V+1 primitive manifest binding present in LOCKS.md but per-use-site mapping artifact still pending | sustained from V4 |
| **NEW SK-V14: Does SK-V14 SYNTHESIS §2 audit-overlay column binding require any current row's xtask gate-json delta beyond R1 + R2 + CH5 wave deliverables?** | The 4 NEW columns map to C-2 wave deliverable per `SYNTHESIS.md:272` C-2 row; ... | Verify in C-2 redress: capture `grep -c 'track2_entry_point\|comparator_plane\|per_iter_equality\|audit_overlay_verdict' skinny/RESULTS.md` ... |
| **NEW SK-V14: Does Lock 1 V+1 fact-stream wording at `LOCKS.md:66-71` already admit CSS L4 as 5th substrate category, or does LAC-1E-14 require explicit `FactStream` taxonomy addition?** | The V+1 text says fact streams "are output-plane contracts, not retained internal sidecars" ... | T-P3 disposes: either (a) explicit `FactStream` taxonomy addition extends BackendShape to 5 variants (changes Lock 10 too), or (b) fact-stream stays as `admitted_fact_output` substrate_target per V+1 §75-82 without taxonomy promotion. |
```

All four sustained-UNKNOWNs (L03 at :161, L16 at :162, audit-overlay
column gap NEW SK-V14 at :163, Lock 1 fact-stream taxonomy NEW
SK-V14 at :164) carry their V4 verify_action attributes unchanged.
Anti-paper-close anchor discipline preserved at V5. The V5 cosmetic
refresh of the §1.5 anchor (`:126-128` → `:128-130`) is the
SECONDARY cite in the paragraph; the PRIMARY cites (`:161-164` for
the four UNKNOWN rows) were untouched and continue to resolve
correctly. ACCEPT.

## CH7 V2 Dispatch Self-Correction Discipline at V5

The V3 CH1 audit established that every V3 cite-rebind site carries
an inline V2-dispatch-correction note (anti-fabrication discipline at
maximum). V4 F-V4-CH1-1 preserved this discipline at the AP-009
refresh. V5 F-V5-CH6-1 modifies only the §1.5 self-cite anchor token
inside the V4 sustained-UNKNOWN paragraph — the V2-dispatch
self-correction text at the AP-009 row (in 1F-anti-pattern.md:69) is
in a different file entirely and was NOT touched by V5. ACCEPT.

LAC-1E-12 procedural addendum (executable `grep -n` verification at
HEAD for every cite-bearing micro-fold) is institutionalised since
V3 close and applied at the V5 F-V5-CH6-1 fold site (the cosmetic
anchor refresh `:126-128` → `:128-130` was itself verified by
executable `sed -n` at HEAD before commit, per the V5 dispatch
context's `LAC-1E-12 institutionalised` mandate). NEW-CH2-V3-02
orphan-cell propagation guard is N/A at V5 (V5 introduces no new
count tokens). ACCEPT.

## CH1 Carry-Forward (V5 → V6 / Post-LOCK)

Zero CH1 carry-forwards remain at V5 close. The V4 → V5 single-item
CH1 carry-forward (F-V5-CH6-1 1E:35 self-cite anchor refresh) is
fully discharged. No new CH1 defect surfaces at V5. Per V5 dispatch
context §5 post-LOCK trajectory, T-P3 §3C carry-forward packet
captures the 5 governance items (LAC-1E-12 procedural addendum
institutionalisation; NEW-CH2-V2-03 K-neutrals enumeration discipline;
NEW-CH2-V3-02 orphan-cell propagation guard; CH4 cite-rebind
cost-neutrality discipline (5 classes); substrate-union ratify-or-unify
rule) — these are T-P3 lens dispositions, NOT CH1 lens carry-forwards.

## ACCEPT-rate summary

| Artefact | CH1 V5 disposition | Notes |
|---|---|---|
| 1A-substrate-evidence.md | ACCEPT | V4-LOCKED; zero V5 drift confirmed (`git diff 8f4756113..9833295d5 -- 1A = 0 lines`); V4 F-V4-CH5-1 row 117 cross-cite refresh + V3 F-V3-CH7-1 CSS source-sidecar rebind preserved bit-for-bit |
| 1B-codegen-evidence.md | ACCEPT | V3-LOCKED through V5; zero V4 drift + zero V5 drift confirmed (`git diff 8f4756113..9833295d5 -- 1B = 0 lines`) |
| 1C-runtime-evidence.md | ACCEPT | V4-LOCKED; zero V5 drift confirmed; V4 F-V4-CH2-1 single-token exec summary 126→127 + NEW-CH2-V3-02 orphan-cell propagation guard preserved; per-grammar breakdown re-derives 127 = 10+10+43+10+10+10+11+13+10 at V5 HEAD |
| 1D-skinny-lessons.md | ACCEPT | V4-LOCKED; zero V5 drift confirmed; V4 F-V4-CH3-1 W13.9 CORRECTNESS-REJECT label split + F-V4-CH5-1 row 117 sub-case cite cluster (CH5-005 Track 2 `:7,26,34,45`; CH5-004 CSS source-sidecar 9-cite cluster; CH5-007 proof-witness `:29-33`) preserved bit-for-bit |
| 1E-locks-evidence.md | ACCEPT | V5 F-V5-CH6-1 cosmetic anchor refresh at :35 (`:126-128` → `:128-130`); anchor target at :128-130 resolves to §1.5 LAC-1E-12 promotion candidacy block (header + separator + paragraph body); four sustained-UNKNOWNs at :161-164 unchanged; cycle frontmatter `SK-V14` preserved; no other diff |
| 1F-coherence-scan.md | ACCEPT | V3-LOCKED through V5; zero V4 drift + zero V5 drift |
| 1F-anti-pattern.md | ACCEPT | V4-LOCKED; zero V5 drift confirmed; V4 F-V4-CH1-1 AP-009 cosmetic refresh 24→27 hits at :69 preserved; `grep -c 'lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` returns 27 at V5 HEAD |
| 1F-past-corpora.md | ACCEPT | V3-LOCKED through V5; zero V4 drift + zero V5 drift |

**ACCEPT-rate: 8/8 = 100.0%.** Fourth consecutive 100% ACCEPT cycle
for T-P1 CH1 (V2 100% → V3 100% → V4 100% → V5 100%). Cohort §3Z
LOCK precondition for CH1 lens met by **three full cycles of margin**
(only ≥95% × 2 required; CH1 has delivered 100% × 4).

## Cycle disposition

**V5 LOCK-TRIGGER cycle ACHIEVED for CH1** (100% ACCEPT; fourth
consecutive ≥95% cycle; second consecutive cohort-wide ≥95% cycle
following V4 first cohort-wide ≥95% achievement). The V5 atomic
cosmetic-fold packet (F-V5-CH6-1) discharges fully; zero drift on
seven V4-LOCKED axes; zero new CH1 defect surfaced at V5; the §1.5
anchor refresh resolves correctly at V5 HEAD with the LAC-1E-12
promotion candidacy block bounded by header (:128) + separator (:129)
+ paragraph body (:130); CH7-V2-failure-mode (V→V+1 cite-carry
without re-verification) discipline preserved (LAC-1E-12 +
NEW-CH2-V3-02 institutionalised since V3 close, applied at every
V4+V5 micro-fold).

**LOCK confirmation:** §3Z cohort LOCK precondition for CH1 lens is
**MET WITH EXTREME MARGIN** (4 consecutive ≥95% cycles where 2
required; 2 consecutive cohort-wide ≥95% cycles where 2 required for
trigger). V5 is the second consecutive cohort-wide ≥95% cycle
following V4's first cohort-wide ≥95% achievement, satisfying the §3Z
cohort LOCK trigger at V≤5 ceiling EXACTLY. CH1 lens is LOCK-MET
absolutely; cohort §3Z LOCK ratification at the V5 aggregator stage
is unblocked from CH1 lens perspective. Final cohort §3Z LOCK
declaration is contingent on the other six lenses (CH2-CH7 if CH7
runs at V5; otherwise CH2-CH6) also returning ≥95% at V5 — that
cohort-aggregation is the V5 aggregator's responsibility, not this
CH1 lens's.

**Predicted trajectory:** V5 CH1 100% closes the CH1 cycle chain at
V5 cohort LOCK trigger. No V6 cycle expected (V≤5 ceiling); if V6
runs as exceptional discretionary cycle, CH1 trajectory remains
≥95% baseline with no CH1-specific regression risk identified.

## Notes on accuracy quibbles (non-defects)

1. The V5 fold packet description in the V5 dispatch context §1
   ("F-V5-CH6-1 self-cite `:126-128` → `:128-130` at 1E:35;
   single-cell trivial cosmetic anchor refresh") matches the V5 commit
   substance exactly. Direct verification via `git diff
   8f4756113..9833295d5 -- 1E-locks-evidence.md` confirms a single
   line modified (line 35), single token swapped (`126-128` →
   `128-130`), zero other diff bytes. The "single-cell trivial
   cosmetic" framing accurately characterises the V5 fold cost — V5
   is the minimum-fold LOCK-trigger cycle as predicted by V4 CH1's
   §3Z trajectory prediction. Not a defect; characterisation
   accuracy confirmed.

2. The F-V5-CH6-1 refresh classifies as a 5th cost-neutrality class
   under CH4 V3+V4 cite-rebind cost-neutrality discipline — the
   prior 4 classes (V2 row-number renumber, V3 source-line rebind,
   V3 historical-fold cite preservation, V4 single-token count
   refresh) all hold cost-neutral; V5 introduces "anchor refresh
   following downstream insertion" as the 5th class. CH4 lens
   dispatches the cost-neutrality verification; CH1 verifies the
   refresh resolves at HEAD (which it does — §1.5 block at :128-130
   is the intended target).

3. The V5 CH1 ACCEPT preserves the V4 CH1 cite cluster verification
   for 1D row 117 (Track 2 substrate-helper sharing
   `bbnf-bench/src/track2/json.rs:7,26,34,45`) — note the 4-line
   cite at row 117 vs the 3-line cite at row 157 (`:7,26,45` without
   `:34`); the two cite clusters are intentionally different
   sub-cases (row 117 cites all four substrate-helper import sites;
   row 157 cites only the three direct-from-bench-harness imports,
   omitting the `:34` which is an internal helper-of-helper site).
   The cite differential was already audited and ACCEPTED at V4 CH1
   and carries forward unchanged at V5. Not a CH1 defect.

4. The V5 atomic commit message `docs(sk-v14-t-p1-V5): atomic
   cosmetic fold (1E:35 anchor) + V5 LOCK-trigger dispatch context`
   accurately frames the V5 commit substance: one cosmetic fold +
   one dispatch context. The dispatch context itself
   (`HARDENING-T-P1-V4-CONSOLIDATED.md` V5 fold-packet section + V5
   per-lens CHALLENGE-CONTEXT) is out-of-scope for CH1 lens (CH1
   reviews only the inventory artefacts under T-P1, not the
   meta-orchestration dispatch documents). The V5 CHALLENGE-CONTEXT
   at `restart/audit/totality/p1/hardening/V5/CHALLENGE-CONTEXT.md`
   accurately frames F-V5-CH6-1 as `single-cell trivial cosmetic
   anchor refresh` — verified by direct inspection. Not a CH1 defect.

5. The pre-V5 CH1 stub on disk (timestamped 2026-05-21 02:36 per
   filesystem stat, pre-dating this V5 dispatch context) framed
   inventories as `cycle: V4` and the V5 fold as "metadata-only / no
   substantive evidence change" — direct frontmatter inspection at
   HEAD disproves the cycle claim (1A/1B/1C/1D/1F-* carry `cycle: V6`,
   1E carries `cycle: SK-V14`), and direct `git diff` inspection
   disproves the "metadata-only" claim (F-V5-CH6-1 modifies 1E:35
   substantive in-body anchor token). This pre-V5 stub mis-read both
   schema (cycle fields) and substance (V5 fold packet); this V5 CH1
   ACCEPT (resting on the corrected schema reading + verified V5
   fold substance) supersedes it. The supersession pattern parallels
   the V4 CH1 supersession of an analogous pre-V4 stub (V4 CH1
   §Notes-on-accuracy-quibbles #4), confirming the stub-shadow
   regression is a recurring V→V+1 cycle-frontmatter mis-read class
   that LAC-1E-12 + NEW-CH2-V3-02 discipline is designed to catch.
