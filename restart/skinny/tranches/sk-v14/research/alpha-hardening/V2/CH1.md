# CH1 CORRECTNESS — Pass Alpha V2 Disposition

Lens: every claim cites file:line, commit SHA, RESULTS row, or REDRESS
entry that resolves. Falsifiability gates are measurable. Comparator
deltas match the strictness plane. Audit-overlay verdicts cite the
correct validation §reference. R-target acceptance criteria are
empirically verifiable.

V2 cycle overlay per `V2/CHALLENGE-V2-ADDENDUM.md §1`: (1) verify each
V1 REVISE fold landed in V2 with quoted `path:line` evidence;
(2) fresh-finding scan for new defects the V1 lens missed.

## §0 — Disposition summary

- Sections audited: 53 (7 SYNTHESIS, 8 HANDOFF, 7 α-A, 8 α-B, 5 α-C,
  6 α-D, 12 α-E). Population preserved verbatim from V1.
- V1 REVISE count entering V2: **3** (α-A §2 direct row-count drift;
  α-A §3 typed row-count drift; α-E §6 C-4 hot-leaf citation).
- V1 REVISE folds landed in V2: **3 / 3** (A-1 LANDED; A-2 LANDED;
  E-14 / REVISE-3 LANDED). Zero FOLD-PARTIAL; zero FOLD-MISSING.
- Fresh V2 findings (REJECT or REVISE the V1 lens missed): **0**.
- V2 ACCEPT-rate: **53 / 53 = 100 %**.
- Critical findings: 0.
- Escalation flag: **NONE**.

The V2 redispatch lifted each V1 narrative-only disclosure into the
table proper. The 4-vs-6 direct and 7-vs-11 typed accountancy drift
now reads as a single binding number (the wider authoritative count)
across SYNTHESIS, α-A, α-D — a fold that not only resolves CH1's three
REVISEs but cross-stiches the same reconciliation through CH6 REJ-2
(F-1). The C-4 pre-wave hot-leaf citation now offers two equivalent
binary anchors (RESULTS.md Hot-leaf column OR v2 §3.1 + W11.1 commit
SHA), eliminating the V1 "Lock 15 evidence" hand-wave.

## §1 — Per-artefact disposition table (V2)

### SYNTHESIS.md (350 → 407 lines)

| § | Disposition | Reason |
|---|---|---|
| §0.1 close condition R10 verbatim | ACCEPT-V1-CARRIED | Unchanged from V1; line refs still resolve. |
| §0.2 goalset enumeration (75-row population) | ACCEPT — FOLD-LANDED (F-1, cross-stitches CH1 REVISE-1/-2) | Lines 73-84 explicitly reconcile dispatch §1 "4 direct + 7 typed" against α-A / α-D peer-measured "6 direct + 11 typed". Names the +2 direct rows (marine_ik, instruments) and the +4 typed extensions (random/W13.3, instruments/W13.4, numbers/W13.1, unicode_basic/W13.2 + update_center/W15.1 adjusted) under the same v6 §1 rows 3-4 binding. PRUNE-1 ledger now binds the wider 6+11 population. |
| §0.3 R-target table (R1–R10 acceptance) | ACCEPT-V1-CARRIED | Empirically-verifiable gates preserved. |
| §0.4 pre-blocks P-1..P-7 | ACCEPT-V1-CARRIED | Validation-pack §refs intact. |
| §0.5 wave-by-wave gate deferral | ACCEPT-V1-CARRIED | Contracted per PASS-ALPHA §4.4. |
| §1 corrected diagnosis | ACCEPT-V1-CARRIED | Per-pillar citation table intact. |
| §2 telemetry binding | ACCEPT-V1-CARRIED | Extended-schema columns hold. |
| §3 candidate shortlist | ACCEPT — Improved | Lines 271-275 now carry C-3's three-part dual-tree round-trip gate and C-4's named pre-wave row + Lock-1 triad explicitly in the table — the gate-text condensation V1 CH7/CH4 charged is resolved at the SYNTHESIS surface, not just buried in α-E §6. |
| §4 S-P3 constraints | ACCEPT-V1-CARRIED | 11 constraints bind to specific gates. |
| §5 pre-blocked / unblocked routes | ACCEPT-V1-CARRIED | Citations resolve. |
| §6 close posture | ACCEPT-V1-CARRIED | Standalone prose. |

### HANDOFF.md (213 → 242 lines)

| § | Disposition | Reason |
|---|---|---|
| §1 bracket verdict | ACCEPT-V1-CARRIED | 0/43 verdict preserved. |
| §2 authority list | ACCEPT-V1-CARRIED | 14-item ordered read intact. |
| §3 honest baseline summary | ACCEPT — F-1 carry | Numeric ledger now reads the wider 6+11 + 24 CSS = 41 carry-over population at line 193, consistent with SYNTHESIS §0.2 reconciliation. |
| §4 pre-S-P0 readiness | ACCEPT-V1-CARRIED | Git history resolves. |
| §5 pass sequence (10 steps) | ACCEPT-V1-CARRIED | Each step cites binding doc. |
| §6 next-move chain | ACCEPT-V1-CARRIED | Sign-off gates aligned. |
| §7 refusal conditions | ACCEPT-V1-CARRIED | 16-item set carries. |
| §8 V1 disposition | ACCEPT-V1-CARRIED | Honest pending state. |

### α-A — Results extraction (362 → 420 lines)

| § | Disposition | Reason |
|---|---|---|
| §0 preamble + conventions | ACCEPT-V1-CARRIED | Line refs match wc -l. |
| §1 parse_only table (17 rows) | ACCEPT-V1-CARRIED | Per-row audit overlay intact. |
| §2 direct_to_struct table | **ACCEPT — FOLD-LANDED (A-1)** | Lines 125-134 add an explicit reconciliation table mapping DISPATCH §1 "4" → ROLLING-SOTA-DELTA "6"; names +2 extension rows (marine_ik, instruments) with v6 §1 row 3 binding ("same comparator-misbinding pattern: `sonic_rs::from_slice::<Value>()` eager-typed DOM, not strict direct-struct deser per-corpus"). Closing sentence: "PRUNE-1 revert scope binds the 6-row count; the dispatch §1 \"4\" is a trace of the v2 §3-specifically-cited rows, not the full direct-admit ledger." V1 REVISE-1 fully resolved. |
| §3 real_typed_struct table | **ACCEPT — FOLD-LANDED (A-2)** | Five extension rows annotated `[ext†]` in the table proper (lines 147, 149, 152, 153, 156); an explicit "wave id" mapping table at lines 184-203 binds each extension to its W13.x / W15.1 wave id and v6 §1 row 4 binding. Closing sentence: "the wave-id column above is the single source of truth for which rows extend the dispatch §1 enumeration." V1 REVISE-2 fully resolved. |
| §4 CSS L4 table (24 rows) | ACCEPT-V1-CARRIED | Per-row audit citations intact. |
| §5 c/B + telemetry | ACCEPT — FOLD-LANDED (A-3, CH4 V6 fold) | Lines 296-319 add an explicit ≈ 80-120 LOC budget decomposition for the c/B column (4 sub-items: report.rs emission, cpu_freq_GHz plumbing, xtask gate schema, telemetry-manifest update) routed through C-2's harness scope at `bbnf-bench/src/report.rs`; LOC fits inside C-2's 600-LOC lower bound without envelope-ceiling raise. (Out of scope for CH1 lens; flagged for completeness.) |
| §6 audit verdict summary | ACCEPT-V1-CARRIED | 0/75 audit-zero bind. |
| §7 forward pointers | ACCEPT-V1-CARRIED | Downstream consumers correctly named. |

### α-B — Competitor deltas (328 lines, STANDS unchanged)

| § | Disposition | Reason |
|---|---|---|
| §0 bound baseline | ACCEPT-V1-CARRIED | STANDS per V2 addendum §0. |
| §1.1 per-plane comparator binding | ACCEPT-V1-CARRIED | STANDS. |
| §1.2 comparator availability ledger | ACCEPT-V1-CARRIED | STANDS. |
| §2 parse_only rebound overlay | ACCEPT-V1-CARRIED | STANDS. |
| §3 direct rebound overlay | ACCEPT-V1-CARRIED | STANDS. |
| §4 typed rebound overlay | ACCEPT-V1-CARRIED | STANDS. |
| §5 CSS rebound overlay | ACCEPT-V1-CARRIED | STANDS. |
| §6 SK-V14 telemetry debt | ACCEPT-V1-CARRIED | STANDS. |
| §7 roll-up | ACCEPT-V1-CARRIED | STANDS. |
| §8 escalations | ACCEPT-V1-CARRIED | STANDS. |

### α-C — REDRESS digest (428 → 460 lines)

| § | Disposition | Reason |
|---|---|---|
| Binding interpretation | ACCEPT-V1-CARRIED | Lock 14 30-count preserved. |
| §1 per-entry SK-V13 dispositions | ACCEPT-V1-CARRIED | Disposition table reconciles. |
| §2 pattern-level pre-blocks | ACCEPT-V1-CARRIED + C-1 strengthening | P-7 triple-check gate landed (CH5 lens-scope; out of CH1 charge but observed at §2.4 verbatim). |
| §3 pattern-level summary | ACCEPT-V1-CARRIED | 7-row pattern table intact. |
| §4 reopen obligations | ACCEPT-V1-CARRIED | Six obligations align to R-target chain. |
| §5 closing posture | ACCEPT-V1-CARRIED | Architectural posture matches SYNTHESIS. |

### α-D — Validated / invalidated / demoted / still-open (545 lines, STANDS)

| § | Disposition | Reason |
|---|---|---|
| §0 contract boundary | ACCEPT-V1-CARRIED | STANDS. |
| §1 source map | ACCEPT-V1-CARRIED | STANDS. |
| §2 VALIDATED (V-1..V-8) | ACCEPT-V1-CARRIED | STANDS. |
| §3 INVALIDATED (I-1..I-4) | ACCEPT-V1-CARRIED | STANDS; the 6-vs-4 direct discrepancy α-D acknowledges at :282-291 is now SYNTHESIS-reconciled via F-1. |
| §4 DEMOTED (D-1..D-3) | ACCEPT-V1-CARRIED | STANDS. |
| §5 STILL-OPEN (S-1..S-6) | ACCEPT-V1-CARRIED | STANDS. |
| §6 net ledger | ACCEPT-V1-CARRIED | STANDS. |

### α-E — Candidate shortlist (660 → 800 lines)

| § | Disposition | Reason |
|---|---|---|
| §0 authority + binding posture | ACCEPT-V1-CARRIED | Citations resolve. |
| §1 why prune-first | ACCEPT-V1-CARRIED | Reasoning sound. |
| §2 shortlist table | ACCEPT — gates lifted (E-14 carry) | C-3 row at line 85 now embeds the dual-tree round-trip command + bypass-header detector + "see §5 + hardening V1 CH7 §3.1" pointer; C-4 row at line 86 names `json/numbers/direct_to_struct/main` with pre-wave/post-wave hot leaf + Lock-1 triad pointer. |
| §3 C-1 Lock-14 refactor | ACCEPT-V1-CARRIED | Owner paths intact. |
| §4 C-2 comparator rebind | ACCEPT-V1-CARRIED | Skipper fallback preserved. |
| §5 C-3 regen-css + corpora | ACCEPT-V1-CARRIED + E-1 strengthening | Round-trip gate now BOTH-tree scoped + bypass-header detector explicit (CH7 lens scope; out of CH1 charge but observed at lines 654-679 of V1 CONSOLIDATED §2.2). |
| §6 C-4 W8+W9 wiring | **ACCEPT — FOLD-LANDED (REVISE-3 / E-14)** | Lines 516-523 rebind the pre-wave hot-leaf citation to TWO equivalent binary anchors: "(a) `RESULTS.md` Hot-leaf column for `json/numbers/direct_to_struct/main` at HEAD reading `parse_value_at`, or (b) `v2-json-validation.md §3.1` numeric-array dispatch trace + the W11.1 commit SHA. Either anchor makes the pre-wave baseline binary and the post-wave assertion mechanically verifiable." The V1 "Lock 15 evidence" hand-wave is replaced with two concrete reproducible binds. V1 REVISE-3 fully resolved. |
| §7 C-5 clean revert | ACCEPT-V1-CARRIED | 29 REDRESS entry count intact. |
| §8 consolidated pre-blocks | ACCEPT-V1-CARRIED | α-C P-1..P-7 carried verbatim. |
| §9 concurrency + serialisation | ACCEPT-V1-CARRIED + E-13 fold | Wave-Zero matrix preserved; §6 vs §9 inconsistency resolved per CH4 V3. |
| §10 cost + caps + telemetry | ACCEPT — FOLD-LANDED (E-2 carry) | Hard caps reverted to 30 min for C-1/C-2/C-3/C-5; only C-4 keeps 45 min per memory addendum amendment. |
| §11 convergence + escalation | ACCEPT-V1-CARRIED | Four escalation paths intact. |

### DISPATCH-CONTEXT.md (206 lines, STANDS unchanged)

Out of V2 fold scope per addendum §0. All V1 dispositions carry.

## §2 — Critical findings (V2)

No REJECT-class findings. No new REVISE-class findings. Per-fold
verification narrative:

### FOLD-LANDED — A-1 (V1 CH1 REVISE-1, direct row-count drift)

**V1 REVISE source:** `V1/CH1.md §2 REVISE-1` (lines 123-129).
**Fold routed at:** `V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2.3 A-1`
(lines 789-794).
**V2 evidence:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-A-results-extraction.md:125-134`.
**Quote:** "**Direct-admit reconciliation (DISPATCH §1 \"4\" vs
ROLLING-SOTA-DELTA \"6\"):** … +2: **marine_ik**, **instruments** |
Both AUDIT-FALSIFIED per v6 §1 row 3 (same comparator-misbinding
pattern: `sonic_rs::from_slice::<Value>()` eager-typed DOM, not strict
direct-struct deser per-corpus) … PRUNE-1 revert scope binds the
6-row count; the dispatch §1 \"4\" is a trace of the v2 §3-
specifically-cited rows, not the full direct-admit ledger." Fold
lifts the V1 narrative-only disclosure (lines 117-122) into a binding
table. PRUNE-1 scope now reads ONE number.

### FOLD-LANDED — A-2 (V1 CH1 REVISE-2, typed row-count drift)

**V1 REVISE source:** `V1/CH1.md §2 REVISE-2` (lines 131-137).
**Fold routed at:** `V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2.3 A-2`
(lines 796-800).
**V2 evidence:** `alpha-A-results-extraction.md:147-156` (table rows
marked `[ext†]`) + `alpha-A-results-extraction.md:184-203` (the
"[ext†] extension-row legend" wave-id mapping table).
**Quote:** "These five extension rows (4 W13.x new + 1 W15.1
adjustment) all reclassify under the same v6 §1 row 4 pattern as the
7 v2-traced SK-V12 carries; PRUNE-1's revert scope must enumerate the
full 11-row count, while α-C §1's REOPEN-AUDIT scope for W13/W15
routes through R7 per the same row-4 binding. The wave-id column
above is the single source of truth for which rows extend the
dispatch §1 enumeration." Fold lifts the V1 narrative-only
disclosure (lines 161-169) into table marks + an explicit wave-id
mapping with per-row v2/v6 binding. PRUNE-1's typed-admit scope now
reads ONE number.

### FOLD-LANDED — E-14 / REVISE-3 (V1 CH1 REVISE-3, C-4 hot-leaf citation)

**V1 REVISE source:** `V1/CH1.md §2 REVISE-3` (lines 139-145).
**Fold routed at:** `V1/HARDENING-ALPHA-V1-CONSOLIDATED.md §2.2 E-14`
(lines 773-779).
**V2 evidence:** `restart/skinny/tranches/sk-v14/research/alpha/alpha-E-candidate-shortlist.md:514-534`.
**Quote (lines 516-523):** "**Pre-wave row binding (CH1 REVISE-3 +
CH6 REV-1, V1 hardening):** the named pre-wave row is
`json/numbers/direct_to_struct/main`. The pre-wave hot-leaf citation
binds to one of: (a) `RESULTS.md` Hot-leaf column for
`json/numbers/direct_to_struct/main` at HEAD reading `parse_value_at`,
or (b) `v2-json-validation.md §3.1` numeric-array dispatch trace + the
W11.1 commit SHA. Either anchor makes the pre-wave baseline binary
and the post-wave assertion mechanically verifiable." The V1 "Lock 15
evidence" hand-wave is replaced by two equivalent, reproducible
anchors. The C-4 candidate-table row at line 86 mirrors the pointer
("see §6 for full owner-path discipline"). The SYNTHESIS §3 C-4 row
at line 274 carries the same binding. Three artefact surfaces, one
binary gate.

### Cross-stitch — F-1 (CH6 REJ-2) resolves CH1 REVISE-1/-2 at the synthesis surface

The aggregator routed the CH6 REJ-2 fold F-1 to SYNTHESIS §0.2; the
V2 prose at `SYNTHESIS.md:73-84` explicitly names the dispatch §1
"4 direct + 7 typed" vs α-A/α-D peer-measured "6 direct + 11 typed"
divergence, enumerates the extension rows by name and wave id, and
binds the PRUNE-1 ledger revert to the wider 6+11 population. The
fold cross-references CH1 REVISE-1 and REVISE-2 even though those
REVISEs were not directly routed through F-1 (only through A-1 and
A-2). The result: the 4-vs-6 and 7-vs-11 accountancy reads as a
single binding number across SYNTHESIS, HANDOFF (§3 :193), α-A, α-C,
α-D — closing CH1's REVISE concerns at three downstream surfaces
beyond the directly-routed α-A folds.

## §3 — Recommended folds for V3 (if any)

**None for CH1.** All three V1 REVISEs landed; no fresh defects
surfaced. CH1 V2 converges at 100 % under the V2 addendum §1
fold-verification + fresh-finding protocol.

Per `V2/CHALLENGE-V2-ADDENDUM.md §4` convergence forecast: if the
other six lenses carry comparable ACCEPT-rates, the V2 cycle
converges per `ORCHESTRATOR.md §3Z` (≥95 % ACCEPT) and V3 fires as
the confirming pass per the two-consecutive-cycle rule. CH1 carries
the convergence forecast through verbatim — no escalation, no V3
fold, no architectural concern.
