# SK-V9 S-P2 CHALLENGE V3 — CH4 COST

Pass: S-P2 Research. Cycle: V3.
Lens: CH4 COST.
Date: 2026-05-18.
Scope: second-consecutive-confirmation re-audit of the six P2 reports
at `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-{A..F}-*.md`
after the V3 fold. The V3 fold applied eight surgical single-sentence
edits across two files only — P2-D and P2-F — per
`HARDENING-S-P2-V2-CONSOLIDATED.md` §"V3 fold requirements". P2-A,
P2-B, P2-C, P2-E carry no V3 edit; their CH4 surfaces are unchanged
from the V2 audit. CH4 V2 disposed ACCEPT at 100% (41/41); CH4 V3 is
the candidate second-consecutive qualifying cycle per ORCHESTRATOR
§3Z (≥95% for two consecutive cycles).
Disposition vocabulary: ACCEPT / REVISE / REJECT.

The V3 fold is the smallest of the SK-V9 S-P2 cycle: of the eight
edits, exactly two are CH4-surface (the P2-D §5.3.1 EOR3 no-regression
gate, and the latency-cite that supports the EOR3 cost claim); the
other six are CH3/CH6/CH1/CH2 surface (citations, wording, line
ranges, vocabulary anchors). This lens verifies (1) the V2 per-slice
cost discipline survived the V3 edits unchanged on every report; (2)
the V3-added P2-D §5.3.1 EOR3 no-regression gate is properly
cost-classed; (3) no V3 edit introduced an S-P3 overreach (no new wave
manifest, sequencing table, or cumulative projection); (4) ≥15
dispositions spot-checked.

## §1 — V2-discipline preservation

The CH4 V2 verdict rested on six per-slice surfaces (LOC envelope,
risk class, minute cap, same-wave consumer, revert protocol, pre-block
reference) plus two structural checks (no S-P3 overreach; P2-F §7 path
defers cost-authorship). The V3 fold is surgical and two-file. The
preservation audit, per report:

### §1.1 — P2-A — untouched, V2 ACCEPT 8/8 preserved

P2-A carries no V3 edit (`Cycle: V2`, line 3; §0 footer records only
the V2 F4/F5 fold). The §5 "Per-slice cost discipline (S-P3 owns final
cost set)" eight-sub-section structure (§5.1–§5.9), the per-slice LOC
envelopes, risk classes, minute caps keyed to the band schedule,
one-sentence revert protocols, and the §5.9 aggregate (~265 hand +
~120 regen LOC, ~155 min preliminary cap, cohort-level revert
protocol) are byte-identical to the V2 audit's §2.1 surface. The §6
REDRESS pre-block ledger (50, 51, 53, 60–72, 82–84, 88, 89, 92) is
preserved. **No V3 regression possible — the file was not edited.**

### §1.2 — P2-B — untouched, V2 ACCEPT 5/5 preserved

P2-B carries no V3 edit (`Cycle: V1 → V2 fold`, line 3). The §6.1
five-slice break-out table (S1–S5) with per-slice LOC cap + revert
protocol and the §6.3 per-slice minute caps are unchanged. The one
cosmetic note CH4 V2 §4.3 flagged (the stale "55 LOC" line at §6.1)
was explicitly classed non-blocking and not a V3 fold item; it
survives unchanged and remains non-blocking. **No V3 regression
possible.**

### §1.3 — P2-C — untouched, V2 ACCEPT 5/5 preserved

P2-C carries no V3 edit (`Cycle: V1`, line 3 — the V2 fold footer
records F4 + F5; no V3 footer). The §2.0 five-slice LOC + minute
sub-budget table (a)–(e), the §4.1 owner-files restatement, and the
§4.3 revert-language exemplar sentence are unchanged. **No V3
regression possible.**

### §1.4 — P2-D — two CH4-surface edits, three non-CH4 edits

P2-D received five of the eight V3 edits (`revision: V3`, line 7; §0
V3-fold footer lines 1180–1192). Of the five:

- **CH4-surface (1):** §5.3.1 EOR3 slice gains the explicit six-row
  W10b no-regression maintain gate (lines 854–866). Audited in §2
  below — this is the load-bearing CH4 verification for V3.
- **CH4-adjacent (1):** §5.3.1's EOR3 latency claim now cites ARM DDI
  0487 FEAT_SHA3 / FEAT_PMULL with the M5-Max-unpublished caveat
  (lines 818–822). This supports the EOR3 cost claim ("trades 6 cheap
  µops for 3 1-cycle µops") — a CH6 residual by classification, but it
  touches the cost narrative, so CH4 verifies it does not inflate the
  cost set. It does not: the cite *weakens* the claim to a
  host-capability-gated estimate and preserves only the monotonic
  ordering — a conservative move, never a cost overrun.
- **Non-CH4 (3):** §6.3 reword (CH6), REDRESS 28+33 line ranges in
  §5.5/§8 (CH1), §0 cascade-sequencing footer (CH3). Verified in §2 to
  carry no S-P3 overreach.

The four V2 "Preliminary LOC envelope + risk class (final cost-set
authored by S-P3)" tables — §3.5, §4.3, §4.4, §5.3.1, §5.4 — survive
the V3 fold structurally intact. §3.5 (codec broadening), §4.3
(string-block widening), §4.4 (CSSC CTZ), §5.4 (dead-SIMD-scanner
wiring) carry **byte-identical** preliminary-LOC tables to V2; only
§5.3.1's table-adjacent prose gained the no-regression gate sentence
and the latency cite — the §5.3.1 LOC table itself (the two-row
table at lines 849–852: 40-80 LOC MEDIUM, 20-40 LOC LOW) is
unchanged. Every table still carries the "final cost-set authored by
S-P3" deferral header, the per-section closing deferral sentence, and
the named same-wave consumer. The V2 audit's §2.4 ACCEPT 8/8 P2-D
disposition holds: every opportunity carries a preliminary LOC + risk
table, the S-P3 cost-authorship deferral, and the CH3 "blocks on P2-A
landing" sentence on the P2-A-dependent slices.

### §1.5 — P2-E — untouched, V2 ACCEPT 11/11 preserved

P2-E carries no V3 edit (`Cycle: V2`, line 3; §0 footer records F2 +
F4 + F5, no V3 footer). The §7.1 eleven-slice table (S1–S11) with LOC
+ minute cap + revert + same-wave consumer in every row, the §7.2
per-axis risk envelope, the §4.4 TOML no-production-consumer
disposition, and the §7.4 "Total wave envelope" cost summary are
unchanged. **No V3 regression possible.**

### §1.6 — P2-F — three non-CH4-surface edits

P2-F received three of the eight V3 edits (`Cycle: V3`, line 3; §0 V3
sub-footer lines 674–687). All three are non-CH4: §5.2's REDRESS-33
inline citation (CH3), the §2.1 ContainerNext `generated.rs:341` cite
+ §5.4 CollapsedStage `ARCHITECTURE.md` §7.3 anchor (CH6), and the §5
asmjson P1-V3-B §1.5 path anchor (CH2). None touches a cost surface.
The V2-resolved §7.4 dependency-graph reframe — the cleared V1 REJECT
— is byte-identical: §7.4 still opens "This synthesis does *not*
author a wave sequence or a cumulative impact projection", still
carries the three-edge dependency graph (`I ← P2-A ← P2-B`; `II ←
P2-E` secondary `II ← P2-A`; `III ← P2-D ← P2-A`), and still closes
with the "No cost set" paragraph (lines 568–572). §7.1/§7.2/§7.3
cost-deferral framing is unchanged. **The V3 edits did not touch §7
cost surface; the V2 ACCEPT 4/4 holds.**

**§1 verdict.** The V2 per-slice cost discipline — LOC envelope,
minute cap (where the artefact class requires one), revert protocol,
same-wave consumer — survived the V3 fold unchanged on all six
reports. Four reports (P2-A, P2-B, P2-C, P2-E) were not edited at all;
P2-D's four V2 cost tables are intact with one gaining a no-regression
gate; P2-F's three edits are all non-cost-surface. No V2 cost surface
degraded.

## §2 — V3 dispositions

The V3 audit grades the eight V3 edits plus a re-confirmation of the
load-bearing V2 cost surfaces. Twenty rows: rows 1–8 are the eight V3
edits; rows 9–20 spot-check the V2 cost discipline survived per
slice/report.

| # | Surface | V3 status | CH4 disposition |
|---:|---|---|---|
| 1 | P2-D §5.3.1 EOR3 six-row W10b no-regression gate (lines 854–866) | Added: "the EOR3 candidate's S-P3 admission carries an explicit no-regression maintain gate on the six W10b WIN-block rows … as a hard blocking precondition." Mirrors §4.4's CSSC CTZ falsification posture. Properly cost-classed — see §2-detail below. | **ACCEPT** |
| 2 | P2-D §5.3.1 EOR3 LOC table (lines 849–852) | Unchanged: 40-80 LOC MEDIUM (ladder body) + 20-40 LOC LOW (checkasm). The no-regression gate sentence did not inflate the LOC envelope — the gate is a *measurement precondition*, not a new code slice; the checkasm-differential row already covers the parity oracle the gate consumes. | **ACCEPT** |
| 3 | P2-D §5.3.1 risk class (line 854) | Unchanged: "**MEDIUM** risk despite the monotonic-µop argument." The V3 gate sentence reinforces — not relaxes — the MEDIUM rationale (the prefix-XOR hot body is the W10b-proven regression surface). Risk class consistent with §4.4's HIGH only because §4.4 is REDRESS-89-adjacent; §5.3.1 EOR3 is REDRESS-88-distinct (different intrinsic). | **ACCEPT** |
| 4 | P2-D §5.3.1 EOR3 latency cite (lines 818–822) | Added: "per ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL … M5 Max P-core specifics are unpublished by Apple — treat the absolute cycle counts as a host-capability-gated estimate, the monotonic *ordering* EOR3 < PMULL is the load-bearing claim." Conservative — weakens the cost claim to a gated estimate. No cost inflation. | **ACCEPT** |
| 5 | P2-D §6.3 reword (lines 1049–1063) | Reworded to distinguish per-primitive checkasm tests (same-wave admission preconditions per §6.2.1) from deferred host-instrumentation infrastructure (invariants 2-5). No cost figure changed; the §4.3 checkasm LOC slice and §6.2.1 dispatch-ownership table are the cost-bearing surfaces and both are unchanged. | **ACCEPT** |
| 6 | P2-D §5.5/§8 REDRESS 28+33 line ranges (lines 946, 951, 1113) | Added: `REDRESS.md:324-337` (28) / `:394-418` (33). Citation precision only; no cost surface. | **ACCEPT** |
| 7 | P2-D §0 cascade-sequencing footer (lines 1165–1171, 1191–1192) | Records the wave-sequencing *constraint* ("P2-A must land in the same wave as any of these P2-D consumer slices … the wave may not be split"). This names a *necessity* S-P3 must honour — it does NOT author a wave manifest, sequence, or cost set. No S-P3 overreach — see §3. | **ACCEPT** |
| 8 | P2-F §5.2 / §2.1 / §5.4 / §5 citation anchors (lines 357–361, 90, 386, 314) | REDRESS-33 inline cite, ContainerNext `generated.rs:341` cite, CollapsedStage `ARCHITECTURE.md` §7.3 anchor, P1-V3-B §1.5 path anchor. All citation-precision edits; none touches a cost surface; §7 cost-deferral framing unchanged. | **ACCEPT** |
| 9 | P2-A §5.1 class-column slice (+60/-20, LOW, ~30 min) | Untouched; V2 surface intact. | **ACCEPT** |
| 10 | P2-A §5.5 codegen-template slice (+120, MEDIUM, ~45–60 min) | Untouched; V2 surface intact. | **ACCEPT** |
| 11 | P2-A §5.9 aggregate (~265 hand + ~120 regen, ~155 min, cohort revert) | Untouched; the W3 450/650 LOC budget reconciliation holds. | **ACCEPT** |
| 12 | P2-B §6.1 five-slice table (S1–S5, ≤425 LOC total) | Untouched; the stale "55 LOC" cosmetic note remains non-blocking. | **ACCEPT** |
| 13 | P2-C §2.0 five-slice sub-budget (~255 hand + run-id ≈ 300, ~85 min ≤ 90) | Untouched; inside the HANDOFF envelope with ~5 min margin. | **ACCEPT** |
| 14 | P2-D §3.5 codec-broadening LOC table | Untouched by V3; "final cost-set authored by S-P3" deferral + consumer = P2-A union substrate intact. | **ACCEPT** |
| 15 | P2-D §4.3 string-block-widening LOC table | Untouched by V3; 4-row preliminary table + S-P3 deferral intact. | **ACCEPT** |
| 16 | P2-D §4.4 CSSC CTZ LOC table (15-35 LOC, HIGH, blocks on P2-A) | Untouched by V3; the six-row W10b falsification gate that §5.3.1 V3 now mirrors was already present here at V2. | **ACCEPT** |
| 17 | P2-D §5.4 dead-SIMD-scanner LOC table (4-row, HIGH, blocks on P2-A) | Untouched by V3; "final cost-set authored by S-P3" deferral intact. | **ACCEPT** |
| 18 | P2-D §6.2.1 checkasm dispatch-ownership table | Untouched by V3; `digit_mac` test still explicitly carried forward to the first SK-V9+ wave that wires it (no paper-close). | **ACCEPT** |
| 19 | P2-E §7.1 eleven-slice table (S1–S11, LOC + cap + revert + consumer) | Untouched; V2 cohort-exemplar cost discipline intact. | **ACCEPT** |
| 20 | P2-F §7.4 dependency graph + "No cost set" paragraph | Untouched by V3; the cleared V1 REJECT holds — no sequencing table, no cumulative projection. | **ACCEPT** |

**§2-detail — the load-bearing V3 verification (row 1).** The P2-D
§5.3.1 EOR3 no-regression gate is the one CH4-surface addition the V3
fold made. CH3 V2 REVISEd §5.3.1 for lacking the six-row gate the
§4.4 CSSC CTZ slice already carried. The V3 edit (lines 857–864)
reads: "Mirroring the §4.4 CSSC CTZ slice's falsification posture, the
EOR3 candidate's S-P3 admission carries an explicit no-regression
maintain gate on the six W10b WIN-block rows (`canada`, `citm_catalog`,
`instruments`, `marine_ik`, `mesh`, `numbers`) as a hard blocking
precondition — the prefix-XOR hot body is the surface W10b proved
regresses the WIN block even when correctness-green, so no EOR3 body
ships unless those six rows hold."

CH4 verifies the gate is **properly cost-classed**:

1. **The gate is a measurement precondition, not a code slice.** It
   does not add an LOC line to the §5.3.1 two-row table — the table
   (40-80 LOC ladder + 20-40 LOC checkasm) is unchanged. The
   no-regression gate is satisfied by the *existing* checkasm
   differential row plus the bench corpus-parity run; it authors no
   new artefact. Classing it as a code slice would have been a cost
   inflation; classing it as a precondition is correct.
2. **The gate strengthens, not relaxes, the MEDIUM risk class.** The
   V2 §5.3.1 risk rationale ("the vector-ladder representation differs
   from the u64-word scalar representation, so the parity oracle must
   cover the vector-vs-scalar-vs-PMULL three-way differential")
   already carried MEDIUM. The V3 gate sentence adds the W10b
   regression surface as a second MEDIUM driver — consistent, not
   contradictory.
3. **The gate is symmetric with §4.4.** §4.4's CSSC CTZ slice carries
   the identical six-row W10b maintain gate at V2 (lines 695–700:
   "the falsification gate against the W10b six-row WIN block … is a
   hard blocking precondition"). §5.3.1's V3 gate is a verbatim mirror
   — same six rows, same "hard blocking precondition" language, same
   "blocks on P2-A" tail. The cohort is now internally consistent: every
   P2-D slice that touches a W10b-regression-class hot body
   (`bitmap_prefix_xor_64` for §5.3.1, the bitmap consumer for §4.4)
   carries the same six-row gate. P2-A §4.2 carries the identical
   six-row gate for its class-column write. The W10b six-row gate is
   the cohort's standard no-regression instrument.
4. **The gate authors no S-P3 product.** It says "the EOR3 candidate's
   S-P3 admission carries" the gate — i.e. it is preliminary input to
   S-P3's cost authoring, expressed as a falsification condition, not
   an authored wave gate. This is the same discipline the V2 audit
   confirmed for §4.4. No cost set, no Mbps threshold authored beyond
   the "`today × 0.98` maintain floor" which is the standing W10b
   instrument, not a P2-D invention.

The gate is properly cost-classed: a measurement precondition,
risk-consistent, symmetric with §4.4 and P2-A §4.2, and S-P3-deferred.

## §3 — Aggregate verdict

Per-report V3 disposition across the eight V3 edits + twelve V2
cost-surface re-confirmations (20 rows in §2):

| Report | V3 edits | CH4-surface | ACCEPT | REVISE | REJECT | V2 verdict | V3 verdict |
|---|---:|---:|---:|---:|---:|---|---|
| P2-A | 0 | 0 | (8/8 carried) | 0 | 0 | ACCEPT | **ACCEPT (carried)** |
| P2-B | 0 | 0 | (5/5 carried) | 0 | 0 | ACCEPT | **ACCEPT (carried)** |
| P2-C | 0 | 0 | (5/5 carried) | 0 | 0 | ACCEPT | **ACCEPT (carried)** |
| P2-D | 5 | 2 | 5 | 0 | 0 | ACCEPT | **ACCEPT** |
| P2-E | 0 | 0 | (11/11 carried) | 0 | 0 | ACCEPT | **ACCEPT** |
| P2-F | 3 | 0 | 3 | 0 | 0 | ACCEPT | **ACCEPT** |
| **TOTAL (V3 rows graded)** | **8** | **2** | **20/20** | **0** | **0** | **100%** | **100%** |

ACCEPT rate on the V3-graded rows: 20/20 = **100.0%**. The four
untouched reports (P2-A, P2-B, P2-C, P2-E) carry their V2 ACCEPT
verdicts forward unchanged — graded against the V2 census of 41
slices, the V3 cohort is 41/41 ACCEPT, identical to V2. Either
denominator yields 100%.

**Second-consecutive-confirmation verdict.** CH4 V2 disposed ACCEPT at
100% (41/41); CH4 V3 disposes ACCEPT at 100%. CH4 has now cleared ≥95%
ACCEPT for **two consecutive cycles** (V2 + V3) with zero open
critical defects and no orphan unresolved REVISE. The ORCHESTRATOR
§3Z convergence criterion for the CH4 lens is **satisfied**: CH4 is
**converged** for S-P2.

**No new S-P3 overreach from the V3 fold.** The audit checked each of
the eight V3 edits against the structural check (no new wave manifest,
no sequencing table, no cumulative projection):

- The P2-D §0 cascade-sequencing footer (V3 edit 7) records a
  sequencing *constraint* ("P2-A must land in the same wave as any of
  these P2-D consumer slices; the wave may not be split"). This is a
  statement of dependency *necessity* — substrate-before-consumer,
  the same class of statement P2-F §7.4's dependency graph carries.
  It does NOT enumerate waves, assign per-wave cost, or project
  cumulative throughput. It is the correct survey discipline: name the
  constraint, defer the wave plan to S-P3 P3-B. Not an overreach.
- The P2-D §5.3.1 EOR3 gate (V3 edit 1) authors a falsification
  condition, not a wave gate — symmetric with §4.4's V2-accepted
  gate. Not an overreach.
- The six non-cost V3 edits (citations, line ranges, wording,
  anchors) cannot constitute an overreach by construction — they add
  no plan content.

The S-P1 V4 CH4 failure mode (a research artefact authoring the wave
sequence under the guise of a cost envelope) — extinguished at V2 by
the P2-F §7.4 reframe — remains extinguished at V3. No V3 edit
reintroduced a sequencing table or a cumulative projection. The P2-D
§0 cascade footer is the closest V3 edit to the failure mode, and it
is on the correct side of the line: it names a constraint S-P3 must
*honour*, it does not author the sequence that honours it.

## §4 — Any new cost gaps

The V3 fold introduced **no new cost gap**. The audit checked each
edit for a cost regression:

- **P2-D §5.3.1 EOR3 gate** — adds a measurement precondition, not a
  code slice; the §5.3.1 LOC table is unchanged. No LOC inflation, no
  minute-cap inflation (P2-D is a survey and correctly carries no
  minute caps — the CH4 V2 §4.2 disposition that P2-D minute caps are
  a deliberate downstream artefact holds unchanged).
- **P2-D §5.3.1 latency cite** — weakens the EOR3 cost claim to a
  host-capability-gated estimate; this is a *conservative* cost
  narrative correction, the opposite of a cost gap.
- **P2-D §6.3 reword** — changes no cost figure; the §4.3 checkasm LOC
  slice and §6.2.1 dispatch-ownership table (the cost-bearing
  surfaces) are untouched.
- **The six citation/line-range/anchor edits** — add no cost content.

Two residual notes carry forward from CH4 V2, both re-verified as
non-blocking and S-P3-routable:

### §4.1 — P2-F §7.4 >SOTA close-criterion sentence (note, not a gap — unchanged from V2)

CH4 V2 §4.1 classed the §7.4 closing ">SOTA gate" paragraph as a
statement of the standing SK-V9 close target, not an authored
falsifiability gate. The V3 fold did not touch this paragraph (the
three P2-F V3 edits are §5.2, §2.1/§5.4, §5 — none in §7.4). The V2
disposition holds verbatim: not a CH4 defect; S-P3 P3-C authors the
operational gate.

### §4.2 — P2-D minute caps deferred (correct, not a gap — unchanged from V2)

P2-D's preliminary LOC + risk tables carry no per-opportunity minute
caps; CH4 V2 §4.2 confirmed this is the correct survey discipline
matching the F4 instruction verbatim. The V3 fold added no minute cap
to any P2-D table (the §5.3.1 gate is a measurement precondition, not
a minute budget). The V2 disposition holds: P2-D's minute budget is
deliberately a downstream S-P3 artefact.

### §4.3 — No new cost gaps from the V3 fold

The V3 fold is eight surgical single-sentence edits across two files,
of which exactly one touches a CH4 cost surface (the §5.3.1 EOR3
no-regression gate) and one touches the cost narrative conservatively
(the §5.3.1 latency cite). Neither adds an LOC line, a minute cap, a
wave manifest, or a cumulative projection. The four untouched reports
(P2-A, P2-B, P2-C, P2-E) carry their V2 cost surfaces forward
byte-identically. The one cosmetic note from CH4 V2 §4.3 (the stale
P2-B §6.1 "55 LOC" line) survives unchanged and remains a cosmetic
copy-edit, not a cost defect.

---

End of CH4 V3 disposition. The cohort clears at **100% ACCEPT** on the
V3-graded rows (20/20), equivalently 41/41 against the V2 census. CH4
has cleared ≥95% ACCEPT for two consecutive cycles (V2 100% + V3
100%) with zero open critical defects and no orphan unresolved
REVISE. The ORCHESTRATOR §3Z CH4 convergence criterion is
**satisfied** — CH4 is **converged** for S-P2. The V3 fold introduced
no new cost gap and no new S-P3 overreach; the two §4 residual notes
are unchanged from V2, S-P3-routable, and non-blocking.
