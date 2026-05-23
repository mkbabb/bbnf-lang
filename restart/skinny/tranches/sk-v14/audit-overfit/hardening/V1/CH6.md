# SK-V14 S-P0 V1 CHALLENGE — CH6 ANTI-PAPER-CLOSE

Lens: `restart/prompts/ORCHESTRATOR.md §3W` row CH6 — "No agent self-
report of 'complete'/'wired'/'verified' stands without orchestrator-cited
live evidence (bench row, samply symbol path, checkasm pass). No
deferral to a future phase." Dispatch context: `restart/skinny/tranches/
sk-v14/audit-overfit/hardening/V1/CHALLENGE-CONTEXT.md §3` row CH6 —
"A5 PARTIAL PASS verdict; ensure no scaffold-only finding gets paper-
closed."

## §0 — Disposition summary

| Artefact | Claims reviewed | ACCEPT | REVISE | REJECT | New finding |
| --- | ---: | ---: | ---: | ---: | ---: |
| A1 css-measurement | 4 | 4 | 0 | 0 | 0 |
| A2 admit-mechanism | 4 | 4 | 0 | 0 | 0 |
| A3 lock14-scan | 3 | 3 | 0 | 0 | 0 |
| A4 generator-truth | 4 | 4 | 0 | 0 | 0 |
| **A5 decision-engine** | **6** | **3** | **3** | **0** | **1 NEW MED (CH6-N1)** |
| A6 pre-restart-pattern | 3 | 3 | 0 | 0 | 0 |
| SYNTHESIS-AUDIT-OVERFIT | 6 | 3 | 3 | 0 | 1 NEW MED (CH6-N2; same root as CH6-N1, escalation channel) |
| **Aggregate** | **30** | **24** | **6** | **0** | **1 (cross-cited)** |

ACCEPT-rate: 24/30 = **80.0 %**. Below §3Z's 95 % gate; the V1 cycle
does not converge on CH6 alone. The six REVISEs cluster on one root —
the `PASS at SK-V14 starting baseline` phrasing in A5 §0 + A5 §3 + the
synthesis §0.1 / §0.2 / §5.1 inheritance of that phrasing. One NEW
MED finding (CH6-N1) escalates to the synthesis (CH6-N2) by inheritance;
the underlying source-of-truth defect is a single line about which
ledger surface holds the `0/17 / 0/17 / 0/17 / 0/24` cells.

Verdict for the lens: **A5 PARTIAL PASS is honestly framed for the
resolver clause but papers over the scaffold clause by re-anchoring
the no-scaffold-only-admit obligation onto a baseline that exists only
as a post-PRUNE target — not as the present file state.** The
synthesis's `S-P0 V1 verdict: FAIL — PRUNE LIST CONFIRMED` is
measured against the 74-finding ledger (not declared) and survives
this lens; the residual REVISEs land inside the verdict-framing prose,
not against the count or the verdict itself.

## §1 — Per-artefact disposition

### §1.1 A5 decision-engine (3 ACCEPT / 3 REVISE)

**ACCEPT A5-CH6-1 — resolver-drives-emission clause is live-evidenced.**
A5 §1.3 quotes `passes/src/lib.rs:476-478` verbatim (the per-rule W5 →
W6 → W7 pipeline); §1.4 quotes the five fail-closed checks at `codegen/
src/lower/rust.rs:37-89`; `grep -nE 'fail.?closed|csp|egraph|active.?
cost|decision.?csp' skinny/crates/codegen/src/lower/rust.rs` yields 8
hits at the cited lines (audit §1.2 row 6, re-run for this lens). The
PASS half of the verdict is not paper-close; it cites the executable
artefacts that would deny compilation if removed.

**ACCEPT A5-CH6-2 — W8 SCAFFOLD finding (HIGH, row 1) carries live
evidence.** `git grep -n 'GrammarConfig' skinny/crates/codegen/ skinny/
crates/runtime/ skinny/crates/passes/ skinny/crates/ir/` returns empty
(re-run for this lens at HEAD `12ff0744e`). Finding does not paper-
close; it states a present-state absence.

**ACCEPT A5-CH6-3 — W9 SCAFFOLD finding (HIGH, row 2) carries live
evidence.** `find skinny/crates/runtime/src -name "*.rs" | xargs grep -l
"UnionTape\|same_substrate\|union_tape"` returns empty (re-run for this
lens). Finding does not paper-close.

**REVISE A5-CH6-R1 — `PASS at SK-V14 starting baseline` (A5 §0 +
§3) paper-closes the scaffold clause by anchoring it on a baseline
that exists only as the contracted post-PRUNE target.** A5 §3 verdict
prose:

> **No-scaffold-only-admit clause: FAIL at SK-V13 close, PASS at
> SK-V14 starting baseline.** … the baseline is `JSON parse_only
> 0/17, JSON direct 0/17, JSON typed 0/17, CSS L4 0/24`. PRUNE-1 +
> PRUNE-2 revert the scaffold-cited admits; PRUNE-5 (C-4) wires W8 /
> W9 to load-bearing before any new admit may cite them.

The audit cites `tranches/sk-v14/SYNTHESIS.md §0.2` as authority for
the `0/17 / 0/17 / 0/17 / 0/24` cells. §0.2 reads (header row,
verbatim):

> | Surface | Population | SK-V13 nominal | **Audit-corrected** |
> SK-V14 obligation |

The `0 ADMITTED` cells live under the **Audit-corrected** column, the
header for which is explicitly the contracted post-revert target. The
**SK-V13 nominal** column on the same row records `5 ADMITTED
(W14.1–.5) / 4 ADMITTED carry-over / 7 ADMITTED carry-over / 24
ADMITTED (incl. SK-V12 W1b)`. Executable check against the actual
file at HEAD:

```
$ git log --oneline restart/skinny/ROLLING-SOTA-DELTA.md | head -10
7ec4a474c feat(sk-v13-waveW15.1): admit UpdateCenter typed plugin fast path
93eb60182 feat(sk-v13-waveW14.5): admit Mesh parse-only row
71508ea93 feat(sk-v13-waveW14.4): admit Marine IK parse-only row
…  (no PRUNE-* commit appears)

$ grep -c 'ADMITTED' restart/skinny/ROLLING-SOTA-DELTA.md
57
```

The `0/17 / 0/17 / 0/17 / 0/24` cells are **the target, not the
present state**. The phrasing "PASS at SK-V14 starting baseline"
collapses the present file state (still carrying the W14.1-5 + W13.*
+ W15.1 admit rows + 24 CSS L4 rows ADMITTED) into the target state.
The clause is not a present PASS; it is a PASS conditioned on
PRUNE-1 + PRUNE-2 + PRUNE-5 landing. The audit makes the conditioning
explicit two sentences later ("PRUNE-1 + PRUNE-2 revert the scaffold-
cited admits; PRUNE-5 (C-4) wires …"); the disposition header should
not assert PASS ahead of the conditioning. REVISE V2 to read
**"FAIL at HEAD; PASS conditioned on PRUNE-1 + PRUNE-2 + PRUNE-5
landing in C-5 / C-4 sequencing"**, restoring the present-state-vs-
target-state distinction the rest of the audit observes elsewhere.

**REVISE A5-CH6-R2 — A5 §0 disposition-line phrasing "the SK-V14
starting baseline reads CLEAN" paper-closes by adoption of the target
nomenclature.** A5 §0 line 11:

> Wave-level scaffold persistence is HIGH severity because PRUNE-5
> must consume it; no row currently held at the SK-V14 starting
> baseline cites W8 / W9 as load-bearing evidence.

The "no row currently held" qualifier is true only under the audit-
corrected baseline; under the HEAD file state, rows W14.1-5 (parse_
only ADMITTED) and the W13.* / W15.1 admits are still "held" — they
cite the W8 / W9 research artefacts per A2 + A5 cross-cite — and the
24 CSS L4 ADMITTED rows are still "held". The line should read
**"under the audit-corrected baseline, no row cites W8 / W9 as
load-bearing evidence; at HEAD the W14.1-5 + W13.* + W15.1 rows are
PRUNE-1 targets and the 24 CSS L4 rows are PRUNE-2 targets, all
audit-falsified per A2 F1-F5 + A1 CRIT"**, anchoring the conditional
PASS on the audit pack rather than the unrealized target.

**REVISE A5-CH6-R3 — A5 §4 row 4 ("LOW, honest self-labelling — no-
op pre-C-4") is a paper-close-by-redirection.** The row's *action
class* column reads "No-op pending C-4"; this is a deferral to a
future wave under the §3W CH6 strict reading ("No deferral to a
future phase"). The finding is correctly observed (the resolver self-
labels via `decision_csp.rs:160-164` block IDs) but the disposition
encodes "no action" against a finding for which the no-action posture
is conditional on PRUNE-5 landing. A6 NEW-LOW (asm bibliographic)
takes the same "KEEP, no action" disposition without the future-wave
conditioning and is acceptable; this row is not. REVISE V2 to either
(a) re-class the finding as **observational telemetry** (LOW
NOTE-only, no action-class column entry) so the finding does not
appear to be a closable item with deferred action, or (b) widen the
action class to **"Preserve through PRUNE-5; gate-rejection invariant
inside C-4 entry-gates"** so the C-4 dispatch carries the obligation
explicitly. Either form removes the deferral framing.

### §1.2 SYNTHESIS-AUDIT-OVERFIT (3 ACCEPT / 3 REVISE)

**ACCEPT SYNTH-CH6-1 — aggregate FAIL verdict (§0.2) is measured.**
74 = 31 CRIT + 20 HIGH + 12 MED + 11 LOW, arithmetic over the per-
axis tables; cross-cited from the per-axis files which each cite live
greps / counts. The FAIL verdict is "measured against the 74-finding
ledger, not declared" per CH6 dispatch focus.

**ACCEPT SYNTH-CH6-2 — §3.2 "Zero orphans" claim is measurable.**
§3.1 table maps 41 + 7 + 11 + 4 + 11 = 74 findings to C-1..C-5; the
arithmetic checks; each row cites the per-axis source files for the
finding population. No paper-close.

**ACCEPT SYNTH-CH6-3 — §4 CH1-CH7 readiness claims are each backed
by §4.1 citation-discipline spot-checks across all six per-axis
files** (A1 6 verification swathes, A2 10 commands, A3 distribution
tables, A4 6 grep/find, A5 9 commands, A6 find enumerations). The
synthesis self-audit cites verifiable surfaces.

**REVISE SYNTH-CH6-R1 — §0.2 prose inherits A5's paper-close framing
verbatim.** §0.2 lines 36-39:

> A5 returns a PARTIAL PASS — the W5/W6/W7 resolver clause is PASS
> … but the no-scaffold-only-admit clause failed at SK-V13 close and
> PASSES at SK-V14 starting baseline only because every scaffold-
> citing row (W14.1-5, W13.1-4, W15.1) is held under PRUNE-1 +
> PRUNE-2 revert and the audit-zero honest delta in `tranches/sk-
> v14/SYNTHESIS.md §0.2` reads `0/17 / 0/17 / 0/17 / 0/24` for
> parse_only / direct / typed / CSS L4 respectively.

The phrase "held under PRUNE-1 + PRUNE-2 revert" papers over that
PRUNE-1 + PRUNE-2 have not landed (last `ROLLING-SOTA-DELTA.md`
touch is `7ec4a474c W15.1` per the §1.2 commit log A2 + A5 both
cite). The rows are "held under" a contracted future operation, not
under a present hold. The `audit-zero honest delta in §0.2 reads
0/17 …` claim cites the **Audit-corrected** column of §0.2 — the
target column — and elides that §0.2's adjacent **SK-V13 nominal**
column reads `5 / 4 / 7 / 24 ADMITTED`. REVISE V2: rewrite the
clause as **"the no-scaffold-only-admit clause failed at SK-V13
close and remains FAIL at SK-V14 HEAD; the C-5 (PRUNE-1 + PRUNE-2)
revert is the gating wave that converts FAIL → PASS, and no row
admit may cite W8 / W9 until C-4 (PRUNE-5) wires them load-bearing;
the audit-corrected target in `SYNTHESIS.md §0.2` reads `0/17 / 0/17
/ 0/17 / 0/24` post-PRUNE"**.

**REVISE SYNTH-CH6-R2 — §0.1 disposition cell for A5 ("scaffold-
clause FAIL at v13 close, PASS at v14 starting baseline") echoes
the same paper-close in tabular form.** REVISE V2 to read **"scaffold-
clause FAIL at v13 close + at v14 HEAD; PASS conditioned on C-5 +
C-4"**, identical fix to A5 §0; the disposition table is the
primary surface a reader reads first.

**REVISE SYNTH-CH6-R3 — §5.1 final verdict line bullet 2 ("1 of 6
PARTIAL PASS (A5: resolver clause PASS, scaffold-clause PASS at
SK-V14 baseline conditional on PRUNE-1 + PRUNE-2 + PRUNE-5
sequencing)") is the most accurate of the three §0 / §0.1 / §5.1
phrasings — it carries the "conditional on" qualifier explicitly —
but still leads with "scaffold-clause PASS" before the conditioning
clause.** The reader-first surface should lead with the present state.
REVISE V2 to read **"A5: resolver clause PASS; scaffold-clause FAIL
at HEAD, conditional PASS upon PRUNE-1 + PRUNE-2 + PRUNE-5 landing
per C-5 → C-4 sequencing"**, fronting the present-state FAIL before
the conditional PASS.

### §1.3 A1-A4 + A6 (16 ACCEPT / 0 REVISE)

The other five per-axis files do not exhibit paper-close patterns
under CH6's strict reading.

- **A1 (4 ACCEPT)** — Pass criterion fails outright; every finding is
  a present-state measurement (corpus size below 1 KB; sub-Mbps
  cluster; misnamed comparator). No findings are deferred.
- **A2 (4 ACCEPT)** — Findings F1-F5 cite per-row source diffs that
  touched only gate / report / lock14_baseline (parser unchanged);
  F6-F7 cite present-state absences of comparator strictness +
  per-iter oracle. F8 (NEW, structural) cites `json_parity.rs:87-102`
  single-lane fanout — present-state. F9 (NEW, negative drift —
  "zero admit-row commits since `7ec4a474c`") is a negative
  confirmation, not a closure; CH6 accepts negative confirmations
  that cite the watched surface explicitly.
- **A3 (3 ACCEPT)** — 29 CONFIRMS + D1 DELTA-NOTE; each citation
  resolves to a present grep / find output. The D1 DELTA-NOTE is
  classified as "future-rename concern, not a new violation" — this
  could be paper-close, but the body of D1 explicitly does not count
  it against the V14 audit-pass criterion (which it already FAILs
  outright on 11 CRIT + 7 HIGH violations); the DELTA-NOTE classify
  serves to mark cross-tranche carry, not to soften the present-
  tranche FAIL.
- **A4 (4 ACCEPT)** — All 16 findings cite either include_str byte
  counts, file system orphan census, or grep output. NEW-1 (JSON
  `generated.rs` fake `@generated`), NEW-2 (CSS scanners as fixture
  lookups), NEW-3 (14/15 .bbnf orphan) are all CRITs with present-
  state citation. The §4 recommendation table includes "R4 must land
  before PRUNE-2" — this is sequencing, not deferral.
- **A6 (3 ACCEPT)** — 67-file Pattern H count + 48-file skinny mirror
  count + NEW-HIGH-1/2 + NEW-MED + NEW-LOW all cite present-state
  surfaces. NEW-HIGH-2 (substrate-doc opt-out enshrinement) is the
  inverse of paper-close: it surfaces a design-of-record that
  enshrines opt-out, demanding either deletion or rewrite. No
  finding deferred.

## §2 — Critical findings

### §2.1 CH6-N1 (NEW MED) — A5 §3 "PASS at SK-V14 starting baseline"
verdict-line phrasing is the central paper-close vector across the
S-P0 V1 audit pack

**Location:** `restart/skinny/tranches/sk-v14/audit-overfit/sk-v14-
audit-overfit-decision-engine.md §3` lines 102-107 + §0 disposition
line 11.

**Class:** verdict-prose paper-close. Not a falsified count, not a
re-opened REDRESS route. The finding *itself* (W8 / W9 SCAFFOLD
persists at HEAD) is correctly recorded as HIGH and correctly
mapped to C-4; the *verdict-line phrasing* paper-closes the
no-scaffold-only-admit pass criterion by relocating the baseline
from the present file state onto a contracted post-PRUNE target.

**Mechanism:** the audit treats the `SK-V14 SYNTHESIS.md §0.2`
**Audit-corrected** column as if it were a present measurement,
asserting "PASS at SK-V14 starting baseline" without the
"conditional on PRUNE-1 + PRUNE-2 + PRUNE-5 landing" qualifier
being foregrounded. The same phrasing inherits into SYNTHESIS-
AUDIT-OVERFIT §0.1, §0.2, and §5.1 (channel CH6-N2).

**Why MED, not HIGH:** the underlying source-of-truth is preserved
elsewhere — A5 §3 itself two sentences down ("PRUNE-1 + PRUNE-2
revert the scaffold-cited admits; PRUNE-5 (C-4) wires …"), the
SYNTHESIS §5.1 final bullet ("conditional on PRUNE-1 + PRUNE-2 +
PRUNE-5 sequencing"), and the S-P0 aggregate FAIL verdict itself.
A reader who reads the audit end-to-end recovers the right picture;
a reader who reads §0 / §0.1 alone walks away with "A5 passes,
move on". The verdict-line phrasing is the surface that drives the
downstream impression.

**Why NEW, not CONFIRMS:** V13's audit pack did not surface a
PARTIAL PASS axis (it was 6/6 FAIL); the present-state-vs-target-
state baseline-conflation pattern is new to the V14 pack and would
recur whenever a future tranche records a PARTIAL PASS that
conditions on prune-wave landing.

**V2 fold recommendation (single edit):** A5 §0 line 11, A5 §3
lines 102-107, SYNTH §0.1 row A5 cell, SYNTH §0.2 lines 35-39,
SYNTH §5.1 bullet 2 — five locations, single edit pattern: replace
`PASS at SK-V14 starting baseline` with `FAIL at HEAD, PASS
conditioned on C-5 (PRUNE-1 + PRUNE-2) + C-4 (PRUNE-5) landing`.

### §2.2 CH6-N2 (NEW MED, escalation channel) — synthesis §0 /
§0.1 / §0.2 / §5.1 inherit CH6-N1 verbatim

Not a distinct finding; the same edit V2 fold above closes both
CH6-N1 and CH6-N2 atomically. Recorded as a separate ledger entry
only because the synthesis is a distinct artefact and a V2 fold
must touch it explicitly.

## §3 — V2 fold recommendations

1. **Single-pattern verdict-line revise across 5 locations.** Per
   §2.1 V2 fold. Closes 6/6 REVISEs in this lens; restores CH6 to
   ≥95 % ACCEPT on the V2 cycle. Cost: ~30 minutes prose edit;
   zero structural change.

2. **A5 §4 row 4 action-class re-classification** (per A5-CH6-R3).
   Either (a) drop action-class to NOTE-only or (b) widen to
   "Preserve through PRUNE-5; gate-rejection invariant inside C-4
   entry-gates". Recommend (b) — the resolver's self-labelling is
   load-bearing telemetry that PRUNE-5 must not silence, and
   encoding this as a C-4 entry-gate invariant turns the deferral
   into a forward obligation.

3. **No-op for the rest of the audit pack.** A1, A2, A3, A4, A6,
   and the synthesis §1 / §2 / §3 / §4 sections do not require
   CH6 revision; the V2 fold is local to A5's verdict-line and its
   §0 / §0.1 / §0.2 / §5.1 inheritances.

4. **Tranche-forward gating recommendation (not V2-blocking).**
   The pattern of treating a contracted post-PRUNE target as a
   present-state baseline will recur whenever a future audit cycle
   records a PARTIAL PASS conditioning on a not-yet-landed prune
   wave. Recommend lifting a CH6-companion gate into S-P0 dispatch
   prose (PASS-0-OVERFIT-AUDIT.md §CH7 already binds against
   scaffold-only-as-load-bearing; the verdict-line phrasing
   discipline is the dual). One-line addition to the §CH7 lens
   text: **"a PARTIAL PASS that conditions on a not-yet-landed
   prune wave must lead with the present-state verdict and append
   the conditional, not the inverse"**.

## §4 — Closing posture

CH6 reads the S-P0 V1 audit pack as **structurally honest on the
finding ledger and partially paper-closed on the A5 verdict-line
phrasing**. The 74-finding count + the aggregate FAIL verdict + the
PRUNE-list mapping survive CH6 strict review. The A5 PARTIAL PASS
disposition is correctly recorded for the resolver clause (live
fail-closed evidence quoted in §1.4) but papers over the scaffold
clause by anchoring the no-scaffold-only-admit pass criterion onto
a baseline that exists only as a post-PRUNE target. The same
phrasing inherits into the synthesis §0 / §0.1 / §0.2 / §5.1
surfaces.

V2 fold cost is low (single-pattern prose edit across five
locations + one action-class re-classification). The aggregate
verdict `FAIL — PRUNE LIST CONFIRMED` is unchanged by the fold;
the fold restores the present-state-vs-conditional-target
distinction inside the A5 row that the rest of the audit observes
elsewhere.

ACCEPT-rate: **80.0 %** (24/30). Below §3Z 95 % gate. CH6 returns
**REVISE** for V1; six REVISEs cluster on a single root that
collapses to a single V2 edit pattern. One NEW MED finding (CH6-
N1) + one inheritance entry (CH6-N2). No REJECTs; no escalation
required.
