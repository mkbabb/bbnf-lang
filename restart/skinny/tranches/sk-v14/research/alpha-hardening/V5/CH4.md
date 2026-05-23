# CH4 COST — Pass Alpha V5 Disposition

Lens binding: `restart/prompts/ORCHESTRATOR.md:86` ("LOC budget, risk
class, wave alignment, and hard cap are stated and realistic; same-wave
consumer present per kernel/primitive"). V5 dispatch context inherits
the V2 addendum methodology
(`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`)
overlaid with the V5 confirming-pass frame: V4 verdict was
CONVERGED-EXPECTING-V5-CONFIRM at 100 % aggregate ACCEPT (275/275 per
`V4/HARDENING-ALPHA-V4-CONSOLIDATED.md:170`); V5 commit `87ee874f0`
landed one surgical edit totalling 6 ins / 5 del on alpha-E §10
(`F-V5-α-E-1` — CH4-axis content explicitly on this lens's cost surface);
V4 baseline:
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V4/CH4.md`
(100 % ACCEPT; 0 REJECT; 0 REVISE; 10 informational notes below the
action threshold). V5 is the §3Z confirming pass at the V ≤ 5 ceiling
— the binding second-link cycle for the two-consecutive-cycle
convergence chain.

## §0 — V5 Disposition summary

- **V5 ACCEPT-rate: 100 % (34 / 34 sectioned dispositions).**
- **V5 REJECT count: 0.** The V2 + V3 + V4 baselines (zero REJECTs)
  hold across every CH4-relevant cell of every artefact; the V5
  surgical edit introduces no fresh REJECT surface, no LOC / risk /
  cap / wave-alignment / same-wave-consumer regression. The V5 fold
  is CH4-axis content (cost/cap table + cluster-total wall-clock
  arithmetic) and lands explicitly inside the previously-bounded §10
  surface, with content byte-equivalent semantics (variable rename
  N → live rostered count; auxiliary M for C-4 shapes).
- **V5 REVISE count: 0.** The V2 + V3 + V4 baselines (zero REVISEs)
  hold; the V5 fresh-finding scan over the F-V5-α-E-1 surface surfaces
  no fresh REVISE under CH4 scope. The V4 cycle closed all orphan
  REVISEs (V4 CONSOLIDATED §0.6 row "V4: 0 REVISEs"); V5 confirms.
- **Critical findings: 0.** Cost-surface rebind from V2 + V3 + V4 is
  preserved verbatim across SYNTHESIS §3 + §4, HANDOFF §6 + §7, α-E §2
  / §5 / §6 / §9, α-A §5; F-V5-α-E-1 is a line-bounded surgical edit
  within the previously-bounded §10 surface with +1 LOC envelope drift
  inside the §10 prose only (alpha-E net +1 lines per V5 commit
  `--numstat` 6/5). No per-candidate implementation envelope shifts;
  no risk-class shifts; no cap-discipline shifts; no same-wave-consumer
  rule shifts.
- **CH4 V5 verdict: CONVERGES.** Lens-local 100 % per the V4 +
  CONSOLIDATED §2.2 forecast (`HARDENING-ALPHA-V4-CONSOLIDATED.md:332`
  "any V5 confirming pass on the cells is expected to be CLEAN"). CH4
  contributes the fourth consecutive 100 % link to the convergence
  chain (V2 → V3 → V4 → V5 all CH4-clean); the §3Z two-consecutive-
  cycle rule is satisfied with margin (V4 100 % + V5 100 %).

### §0.1 — V2 / V3 / V4 → V5 baseline-hold verification

| Origin fold | Fold ID | V4 site (V4 §0.1) | V5 site after V5 micro-fold | Status |
|---|---|---|---|---|
| V2 R1 — SYNTHESIS §3 risk column | F-3 | `SYNTHESIS.md:271-275` | `SYNTHESIS.md:271-275` (C-1 VERY HIGH; C-2 HIGH; C-3 HIGH; C-4 VERY HIGH; C-5 MED-LOW) | **HELD** (line offsets unchanged; SYNTHESIS STANDS at V5; V5 commit touched only alpha-E) |
| V2 R2 — SYNTHESIS §3 LOC envelope column | F-4 | `SYNTHESIS.md:269` header + `:271-275` per-row + `:277` total | `SYNTHESIS.md:269` + `:271-275` + `:277` total ≈ 5.65k–8.38k | **HELD** (verbatim) |
| V2 R3 — α-E §10 hard-cap revert | E-2 | `alpha-E-candidate-shortlist.md:756-760` + `:762-770` (V4 reconciliation tail) | `alpha-E-candidate-shortlist.md:756-760` (cap table verbatim at line offsets; the only in-table change is the parenthetical "8 grammars" → "per rostered grammar" in the C-1 cell at `:756`) + `:762-774` (reconciliation tail extended by +1 line per V5 net addition) | **HELD** at every cap value (20 / 15 / 30 / 30 / 30 / 30 / 45 / 30 — all unchanged); cap-discipline arithmetic now roster-count-agnostic; line offsets ≤ 1 shift |
| V1 — C-1 LOC lower-bound 2.8k | E-11 | `alpha-E-candidate-shortlist.md:83` + `:89-92` | `alpha-E-candidate-shortlist.md:83` (2.8k–3.4k) + `:89-92` envelope rationale | **HELD** (V5 edit lives at §10, well below §2; no line drift here) |
| V2 — C-2 LOC ceiling +80 LOC Skipper | E-12 | `alpha-E-candidate-shortlist.md:84` + `:268-274` | `alpha-E-candidate-shortlist.md:84` (600–1.08k) + `:268-274` Skipper fallback paragraph | **HELD** |
| V3 — §9 vs §6 C-1↔C-4 sequencing | E-13 | `alpha-E-candidate-shortlist.md:729` + `:731-741` | `alpha-E-candidate-shortlist.md:729` matrix row (C-4 serialises after C-1 ALL sub-waves) + `:731-741` reconciliation paragraph | **HELD** (V5 edit lives at line 756+; §9 line anchors stable above) |
| V4 — SYNTHESIS §3 same-wave consumer column | F-5 | `SYNTHESIS.md:269` header + `:271-275` per-row | `SYNTHESIS.md:269` header includes "Same-wave consumer"; `:271-275` per-row values | **HELD** |
| V5 — HANDOFF §6 hard-cap echo | F-7 | `HANDOFF.md:162-165` | `HANDOFF.md:162-165` (30-min lens-agent cap; 20/15/30-or-45 R/P/R cadence with C-4 carve-out) | **HELD** (V5 commit did not touch HANDOFF) |
| V6 — α-A §5 c/B LOC budget | A-3 | `alpha-A-results-extraction.md:296-319` | `alpha-A-results-extraction.md:296-319` (80–120 LOC; carry-with-C-2; same-wave consumer rule preserved) | **HELD** (α-A STANDS at V5) |
| V7 — SYNTHESIS §4 per-wave LOC ceiling | F-6 | `SYNTHESIS.md:326-329` | `SYNTHESIS.md:326-329` (per-candidate envelope inheritance; > 20 % escalation per `[generated-size-budget]`) | **HELD** |
| V3 — F-V3-α-E-1 round-trip gate prose | F-V3-α-E-1 | `alpha-E-candidate-shortlist.md:362-387` | `alpha-E-candidate-shortlist.md:362-387` (V5 edit at line 756+ is well below §5; line anchors stable) | **HELD** at line range and content (V4 fold semantics preserved verbatim through V5) |
| V3 — F-V3-α-F-1 carry-over guard broadening | F-V3-α-F-1 | `HANDOFF.md:192-197` | `HANDOFF.md:192-197` (47-row guard; §0.2 lines 73-84 anchor) | **HELD** (V5 commit did not touch HANDOFF) |
| V4 — F-V4-α-E-1 shell incantation repair | F-V4-α-E-1 | `alpha-E-candidate-shortlist.md:362-387` | `alpha-E-candidate-shortlist.md:362-387` (V5 edit lives below this surface; line offsets stable) | **HELD** (V4 shell fix + roster-count-agnostic gate prose carry through V5 verbatim) |
| V4 — F-V4-α-F-1 citation anchor repair | F-V4-α-F-1 | `HANDOFF.md:192-197` | `HANDOFF.md:192-197` | **HELD** (V5 commit did not touch HANDOFF) |

Roll-up: **14 / 14 V2 + V3 + V4 CH4 folds HELD under V5 micro-fold
pressure.** Zero regression; zero per-candidate implementation envelope
drift; zero cap-value drift (cap *numbers* 20/15/30/45 verbatim); zero
same-wave-consumer rule weakening. The single line offset shift is
contained to the §10 reconciliation paragraph tail (V4: lines 762-770;
V5: lines 762-774, a +1 net within the same paragraph), and the §11
section header shifts by +1 (V4: line 787; V5: line 788) — both
within the contracted ≤ 1 line offset tolerance for V5's +1 net.

### §0.2 — V5 micro-fold cost-surface delta

| V5 fold | Owner artefact | Site | LOC delta | Cost-surface impact |
|---|---|---|---:|---|
| F-V5-α-E-1 | α-E §10 (cost/cap table row + cluster-total wall-clock arithmetic) | `alpha-E-candidate-shortlist.md:756` (1 ins / 1 del in-table; parenthetical "8 grammars" → "per rostered grammar") + `:770-774` (5 ins / 4 del in reconciliation paragraph; "8 × 30 = 240 min" → "N × 30 min where N is the live rostered-grammar enumeration"; auxiliary "N" rebound to "M" for C-4 shapes to avoid variable collision) | +1 LOC net on α-E (816 lines total post-V5 vs 815 pre-V5) | C-1 implementation envelope 2.8k – 3.4k UNCHANGED. C-2/C-3/C-4/C-5 envelopes UNCHANGED. Total 5.65k – 8.38k UNCHANGED. The V5 edit converts the C-1 sub-wave count from a hardcoded "8 grammars" parenthetical (V4 baseline carried a stale count; live workspace per V4 commit message enumerates 9 grammars) into roster-count-agnostic phrasing ("per rostered grammar"); the V4 §2.3 N-9 informational note explicitly favoured this posture as "the cleanest cost-surface posture for the gate spec: zero per-grammar prose growth as the roster grows". V5 lands the equivalent posture on the §10 cost/cap table that V4 landed on the §5 round-trip gate spec. The cap *values* per cell (20 / 15 / 30 for C-1; 30 for C-2/C-3/C-5; 45 for C-4) are byte-equivalent at every cell; only the C-1 row's parenthetical name and the reconciliation paragraph's wall-clock arithmetic phrasing changed. |

Total V5 fold delta: +1 LOC net on α-E (the reconciliation paragraph
grew by one line to accommodate the variable-rename ("N" for grammar
roster; "M" for C-4 shapes — see §2.2 below); the in-table cell at
`:756` stays one line); zero LOC delta on every implementation
envelope C-1 – C-5; total envelope 5.65k – 8.38k HELD verbatim at
SYNTHESIS `:277` + α-E `:89-92`. Per the V5 commit message
"docs(sk-v14-alpha): V5 micro-redispatch — alpha-E:756 + :770
belt-braces", the edit is the CH2 V4 §2.1 non-finding belt-and-braces
fold that V4 CONSOLIDATED §2 option B prescribed; the edit lands
explicitly on CH4-axis content (the cost/cap discipline cell + the
cluster-total wall-clock arithmetic) and is therefore the CH4 lens's
own cycle to confirm under V5.

### §0.3 — V5 cost-axis explicit verification

Per dispatch frame's three-point checklist:

**Point (1): all CH4 V2/V3/V4 anchors hold; line offsets shift by ≤ 1
from V5's +1 net line addition (content byte-equivalent).** VERIFIED.
The 14 V2/V3/V4 CH4 anchors enumerated in §0.1 all HOLD; the V5 line
offset shift is bounded by ≤ 1 line (the §10 reconciliation paragraph
tail and §11 section header both shift by +1; every other anchor
above line 770 in α-E and every anchor in SYNTHESIS / HANDOFF /
α-A / α-B / α-C / α-D / DISPATCH-CONTEXT is at offset 0). Content
above the §10 edit-site is byte-equivalent V4 → V5.

**Point (2): V5 cost-axis updates correctly stated; roster-count-
agnostic phrasing introduces no LOC/risk drift; the N/M variable
rename avoids collision; the total envelope 5.65k – 8.38k still
holds.** VERIFIED:

- *Roster-count-agnostic phrasing:* The C-1 cap-table cell at `:756`
  reads "C-1 sub-waves (per rostered grammar; per sub-wave)" — the
  "per rostered grammar" phrasing binds the cap to the live workspace
  metadata clause Lock 14 owns (`workspace.metadata.bbnf.grammars`)
  without naming a count. This is consistent with the V4 §5
  round-trip gate's "admitting an additional grammar requires NO
  change to the gate's text" posture. The cap *value* (20 / 15 /
  30 min per sub-wave) is unchanged — only the row's identifier
  parenthetical has been generalised.
- *N / M variable rename avoids collision:* The reconciliation
  paragraph at `:770-774` reads "The C-1 cluster total is N × 30 min
  of redress windows where N is the live rostered-grammar enumeration
  (`cargo metadata | jq` over the grammar roster at HEAD), run
  serialised per §9; the C-4 cluster total is M × 45 min where M is
  the number of shapes the wiring exercises (≥ 2 per E-8's two-
  grammar-family requirement)." V4 carried `N` for the C-4 cluster
  shape count; V5 promotes `N` to the C-1 grammar-roster count
  (since the C-1 cluster total is the new variable introduction
  driving the V5 fold) and rebinds the C-4 cluster shape count to
  `M`. The rename is necessary: without it, both clusters would
  share the symbol `N` while referring to different quantities
  (grammar count vs shape count). The V5 rename closes the variable-
  symbol collision cleanly. CH4 records this as a favourable
  precision improvement.
- *Total envelope 5.65k – 8.38k still holds:* VERIFIED at both
  citation sites. SYNTHESIS §3 `:277` reads "Total envelope ≈ 5.65k
  – 8.38k across the five candidates per α-E §2"; α-E §2 `:89-92`
  reads "Total LOC envelope: ≈ 5.65k – 8.38k across the five
  candidates". Arithmetic re-check: C-1 (2.8k – 3.4k) + C-2 (0.6k –
  1.08k) + C-3 (1.2k – 2.0k) + C-4 (0.8k – 1.4k) + C-5 (0.25k –
  0.5k) = 5.65k – 8.38k. Exact match.

**Point (3): fresh-finding scan.** Executed; see §2.3 below. No new
REJECT; no new REVISE; one V5-introduced informational note (N-11)
on the wall-clock arithmetic post-rename.

## §1 — Per-artefact disposition table (V5)

V5 disposition retains every V2 + V3 + V4 ACCEPT decision; the single
V5-touched site (α-E §10 cap table row + reconciliation paragraph) is
re-evaluated below. Every other cell carries its V4 ACCEPT through.

| Artefact | § | V4 disposition | V5 disposition | Reason |
|---|---|---|---|---|
| `SYNTHESIS.md` | §0.1 Close condition (R10) | ACCEPT | ACCEPT | Unchanged (SYNTHESIS STANDS at V5). |
| `SYNTHESIS.md` | §0.2 Goalset row enumeration | ACCEPT | ACCEPT | Unchanged; F-1 reconciliation paragraph at `SYNTHESIS.md:82-90` HELD. |
| `SYNTHESIS.md` | §0.3 R-target acceptance | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.4 P-1 … P-7 pre-blocks | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §0.5 Wave-by-wave gates deferred | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.1 Survives pillars | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.2 Falsified items | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §1.3 Honest rolling delta | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §2 Telemetry binding | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §3 Candidate shortlist | ACCEPT | ACCEPT | F-3 + F-4 + F-5 all HELD verbatim at `:269` + `:271-275` + `:277`; risk + LOC + same-wave consumer columns intact; total envelope ≈ 5.65k–8.38k HELD. |
| `SYNTHESIS.md` | §4 S-P3 constraints | ACCEPT | ACCEPT | F-6 LOC-ceiling clause at `:326-329` HELD; `> 20 %` escalation discipline preserved. |
| `SYNTHESIS.md` | §5 Pre-blocked / unblocked routes | ACCEPT | ACCEPT | Unchanged. |
| `SYNTHESIS.md` | §6 Close posture | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §1 Bracket verdict | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §2 Authority list | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §3 Honest baseline summary | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §4 Pre-S-P0 readiness | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §5 Pass sequence | ACCEPT | ACCEPT | Unchanged. |
| `HANDOFF.md` | §6 Next-move | ACCEPT | ACCEPT | F-7 cap-echo at `:162-165` HELD verbatim. V5 commit did not touch HANDOFF. |
| `HANDOFF.md` | §7 Refusal conditions | ACCEPT | ACCEPT | F-V4-α-F-1 citation anchor repair at `:195-196` HELD; V5 commit did not touch HANDOFF. |
| `HANDOFF.md` | §8 V1 disposition | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §1–§4 per-plane row tables | ACCEPT | ACCEPT | Unchanged (α-A STANDS at V5). |
| `alpha-A-results-extraction.md` | §5 c/B telemetry | ACCEPT | ACCEPT | A-3 c/B LOC budget at `:296-319` HELD; 80–120 LOC routed through C-2's existing envelope without ceiling raise. |
| `alpha-A-results-extraction.md` | §6 verdict summary | ACCEPT | ACCEPT | Unchanged. |
| `alpha-A-results-extraction.md` | §7 forward pointers | ACCEPT | ACCEPT | Unchanged. |
| `alpha-B-competitor-deltas.md` | §316-320 Skipper absence | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 + V5 addenda; α-B unchanged in V5 commit. |
| `alpha-C-redress-digest.md` | full | ACCEPT | ACCEPT | C-1 (P-7 triple-check) is CH5-domain; CH4 cost-surface unchanged. α-C STANDS at V5. |
| `alpha-D-validated-invalidated.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 + V5 addenda. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | ACCEPT | E-11 + E-12 envelopes HELD at `:83-84` (C-1 2.8k–3.4k; C-2 600–1.08k); total `:89-92` ≈ 5.65k–8.38k HELD. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 narrative | ACCEPT | ACCEPT | E-7 forward-invariant + E-11 LOC reconciliation HELD. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 narrative | ACCEPT | ACCEPT | E-12 ceiling raise at `:268-274` HELD verbatim. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 narrative | ACCEPT | ACCEPT | F-V4-α-E-1 V4-landed shell incantation + roster-count-agnostic gate prose at `:362-387` HELD verbatim through V5; C-3 LOC budget at `:410-413` (1.2k – 2.0k) UNCHANGED. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 narrative | ACCEPT | ACCEPT | E-3 Lock-1 triad + E-4 module-path discipline + E-5 pre-wave hot-leaf citation HELD; 800–1.4k envelope UNCHANGED. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 narrative | ACCEPT | ACCEPT | E-10 29-row scribe-contract count HELD; cost envelope 250–500 (delete-heavy) UNCHANGED. |
| `alpha-E-candidate-shortlist.md` | §8 pre-blocked routes | ACCEPT | ACCEPT | Unchanged. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | ACCEPT | E-13 matrix row at `:729` + reconciliation paragraph at `:731-741` HELD verbatim (V5 edit at line 756+ leaves §9 line anchors stable above). |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT | **ACCEPT** | F-V5-α-E-1 cap table row at `:756` ("8 grammars" → "per rostered grammar") and reconciliation paragraph at `:770-774` ("8 × 30 = 240 min" → "N × 30 min where N is the live rostered-grammar enumeration"; auxiliary M for C-4 shapes) are CH4-axis surgical edits inside the previously-bounded §10 surface. Cap *values* (20 / 15 / 30 for C-1/C-2/C-3/C-5; 45 for C-4) all preserved verbatim; cap-discipline reconciliation language (only C-4 inherits the 45-min addendum amendment; C-1/C-2/C-3/C-5 default to 30-min) preserved verbatim; only the grammar-count parenthetical and the wall-clock arithmetic phrasing changed. Roster-count-agnostic; N/M rename closes variable collision; total envelope unchanged. See §2.1 below. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | ACCEPT | Unchanged content; line offset +1 (V4: line 787 → V5: line 788) inside contracted ≤ 1 tolerance per V5 net addition. |
| `DISPATCH-CONTEXT.md` | full | ACCEPT | ACCEPT | STAND from V1 per V2 + V3 + V4 + V5 addenda. |

## §2 — Critical findings detail (V5)

### §2.1 — F-V5-α-E-1 fold landing (α-E §10 cap table row + cluster-total arithmetic)

V5 fold prescription from V4 CONSOLIDATED §2 option B
(`HARDENING-ALPHA-V4-CONSOLIDATED.md:313-336`): "convert hardcoded '8
grammars' at alpha-E:756 to 'per rostered grammar' (roster-count-
agnostic); recompute wall-clock total at alpha-E:770 as 'N × 30 min
where N = live rostered-grammar enumeration via cargo metadata | jq'
+ 'M × 45 min for C-4 where M = exercised shapes' (N → M rename
avoids variable collision)."

V5 evidence at `alpha-E-candidate-shortlist.md:756` (post-fold, per
V5 commit `87ee874f0`):

> | C-1 sub-waves (per rostered grammar; per sub-wave) | 20 min | 15 min | 30 min |

V5 evidence at `alpha-E-candidate-shortlist.md:770-774` (post-fold,
per V5 commit `87ee874f0`):

> cluster total is N × 30 min of redress windows where N is the live
> rostered-grammar enumeration (`cargo metadata | jq` over the grammar
> roster at HEAD), run serialised per §9; the C-4 cluster total is
> M × 45 min where M is the number of shapes the wiring exercises
> (≥ 2 per E-8's two-grammar-family requirement).

**CH4 cost-surface analysis.** The V5 fold lands explicitly on CH4-axis
content. Both edited sites are cost-axis cells in the §10 cost / caps /
telemetry surface:

- *Cap table row (`:756`).* The single-cell edit replaces the hardcoded
  "(8 grammars; per sub-wave)" parenthetical with "(per rostered
  grammar; per sub-wave)". The cap *values* (research 20 min / plan 15
  min / redress 30 min) are preserved verbatim. The row's CH4 binding
  (per-sub-wave research/plan/redress envelope on C-1) is preserved
  verbatim. Only the row identifier prose has been generalised. This
  is exactly the V4 §2.3 N-9 "favourable cost-surface posture" pattern
  applied to the §10 cell — the cap row no longer requires per-grammar
  prose patching as the roster grows, and the live workspace metadata
  clause `workspace.metadata.bbnf.grammars` (which Lock 14 owns) is
  the single source of truth for the count.

- *Cluster-total arithmetic (`:770-774`).* The five-line edit
  replaces the V4 baseline's "8 × 30 = 240 min of redress windows" with
  "N × 30 min of redress windows where N is the live rostered-grammar
  enumeration (`cargo metadata | jq` over the grammar roster at
  HEAD)". The C-4 cluster-total phrasing was rebound from "N × 45 min
  where N is the number of shapes" to "M × 45 min where M is the
  number of shapes" — the variable rename N → M closes the symbol
  collision the V5 fold introduces (N now binds to the C-1 grammar
  roster count; M continues to bind to the C-4 CSP-selectable shape
  count). The arithmetic semantics are preserved: C-1 wall-clock
  cluster total ≈ 9 × 30 = 270 min against the live workspace per V4
  commit message's 9-grammar enumeration, where the V4 prose hardcoded
  "240 min" against a stale 8-grammar count; V5 lifts the arithmetic
  to roster-count-agnostic phrasing. C-4 wall-clock cluster total
  M × 45 min remains bounded below by E-8's two-shape requirement
  (M ≥ 2 ⇒ ≥ 90 min lower bound; in practice M = 2 against the
  two-grammar-family floor unless C-4 elects to exercise more
  shapes). The "(≥ 2 per E-8's two-grammar-family requirement)"
  forward bind is preserved verbatim.

**LOC envelope.** +1 LOC delta on α-E (815 → 816 lines per `wc -l`);
zero LOC delta on every implementation envelope C-1 – C-5. The +1 net
lives in the §10 prose tail (the reconciliation paragraph grew from
4 lines to 5 lines to accommodate the variable rename and the longer
"live rostered-grammar enumeration" phrasing). The `[generated-size-
budget]` discipline at `SYNTHESIS.md:326-329` binds wave LOC under
> 20 % escalation; the V5 fold consumes 0 % of any implementation
envelope (the spec-layer +1 is well below the doc-only threshold).

**Risk class.** Unchanged at every cell. The fold target is the §10
cap discipline table — a gate consumed by S-P3 wave authors and the
orchestrator's hard-cap dispatch infrastructure. The cap *values* per
cell (20 / 15 / 30 / 45 / 30) are preserved verbatim; the parametric
identifiers (N for rostered grammars; M for C-4 shapes) make the
cluster-wall-clock arithmetic forward-tolerant for any future roster
admission. No risk-class shifts; no cap-class shifts; no per-sub-wave
discipline shifts.

**Same-wave consumer rule preserved.** The §10 table binds caps to
*sub-waves* and *shapes*; the same-wave consumer rule operates at
the candidate-LOC level (every primitive lands WITH its hot-path
caller in the same commit) and is not affected by the §10 edit.
C-1's same-wave consumer (regen-derived runtime for every grammar
emitted in the same waves; the per-sub-wave gate runs before commit)
is preserved verbatim at SYNTHESIS `:271`. C-4's same-wave consumer
(CSP-selected shape produces measurable runtime divergence on at
least one named pre-wave row in the same wave) is preserved verbatim
at SYNTHESIS `:274`. The V5 fold does not introduce any new
primitive; no consumer rebinding required.

**Cap discipline preserved.** F-V5-α-E-1 is a single-cell + single-
paragraph documentary edit; well inside the narrow-fold cap implied
by V4 CONSOLIDATED §2 option B ("≈ 5 min α-E single-cell edit"). The
6 ins / 5 del actual edit is below the narrow-fold envelope. No cap
escalation required.

**Wave-alignment.** The α-E §9 concurrency matrix at `:729` and the
§10 cap table at `:756-760` headers are both untouched by the V5
fold; line anchors are stable above line 770. C-3's parallelisability
with C-2 and C-5 (Wave Zero) is preserved; C-4's strict serialisation
after C-1 ALL sub-waves is preserved.

**Disposition: ACCEPT.** Zero cost-surface envelope impact; zero risk-
class shift; zero cap-value drift; zero same-wave-consumer rule
weakening; +1 LOC spec-layer delta inside the contracted V5
belt-and-braces fold envelope. The repair improves cap-discipline
forward-tolerance (CH4-axis correctness) inside the previously-
bounded §10 surface; V5 cleanly lands the CH4-axis belt-and-braces
that V4 CH4 §2.3 N-9 explicitly favoured as "the cleanest cost-
surface posture for the gate spec".

### §2.2 — N / M variable rename rationale (CH4 lens recording)

The V5 fold's variable rename (V4: C-4 cluster total used `N`; V5:
C-1 cluster total uses `N`, C-4 cluster total uses `M`) deserves
CH4-lens recording because the rename's necessity comes from the
CH4-axis arithmetic itself:

- *Before V5:* Only one cluster (C-4) had a parametric wall-clock
  total in the §10 reconciliation paragraph; the C-1 cluster carried
  a hardcoded "8 × 30 = 240 min" form. The single parametric variable
  `N` was unambiguous (it referred to C-4's shape count).
- *V5 introduces a second parametric cluster:* The C-1 cluster total
  is now parameterised by the rostered-grammar count (the V4 hardcoded
  "8 × 30" becomes "N × 30"). Both cluster totals are now parametric.
- *Variable collision:* If both cluster totals shared symbol `N`,
  the reader could not disambiguate which `N` refers to which count.
  The rename N (C-1 grammars) + M (C-4 shapes) closes the collision
  cleanly. The choice to promote `N` to the *new* parametric site
  (C-1 grammars) and rebind the *existing* parametric site (C-4
  shapes) to `M` is a stylistic choice; either assignment closes the
  collision, but the V5 choice preserves the "N for the new
  introduction; M for the alphabetically-next free symbol" reading.
- *No cost-axis impact:* The rename is documentary; the C-4
  cluster-total arithmetic semantics (`M × 45 min, M ≥ 2`) is
  byte-equivalent to the V4 semantics (`N × 45 min, N ≥ 2`). No cap
  value changed; no per-shape envelope changed; the C-4 implementation
  LOC budget (800 – 1.4k at `:86`) is unchanged.

CH4 records the rename as a *favourable precision improvement* under
the lens's "stated and realistic" mandate: the rename makes the
cluster-wall-clock arithmetic unambiguous for both clusters under
the same parametric framework, which is the correct cost-discipline
posture for a two-cluster cap-arithmetic surface.

### §2.3 — Fresh-finding scan (V5)

Per V2 addendum §1.2 (inherited through V3 + V4 + V5): "Look for
issues the prior lens did NOT catch." V5 extension: look also for
issues the V5 micro-fold introduced. COST-lens fresh scan executed
over the V5 artefacts in entirety, with emphasis on the F-V5-α-E-1
fold-touched surfaces, the cap-discipline anchoring (line-anchor
stability under V5 +1-line fold), the roster-count-agnostic phrasing
correctness, the N / M variable-rename collision closure, the
cluster-wall-clock arithmetic semantics, and the cost-surface routing
for all five candidates under the repaired §10 table.

**No new REJECT.**
**No new REVISE.**

Fresh-finding notes (informational only; below the lens action
threshold; V4 notes re-validated under V5 + 1 V5-introduced):

- **N-1 (informational; V5-revalidated).** SYNTHESIS §3 total
  envelope reads "≈ 5.65k – 8.38k" at `SYNTHESIS.md:277`; α-E §2
  reports "≈ 5.65k – 8.38k" at `alpha-E-candidate-shortlist.md:89-92`.
  Arithmetic check: C-1 (2.8k–3.4k) + C-2 (0.6k–1.08k) + C-3
  (1.2k–2.0k) + C-4 (0.8k–1.4k) + C-5 (0.25k–0.5k) = 5.65k–8.38k.
  The V5 fold did NOT alter any per-candidate envelope; arithmetic
  still matches exactly. No drift.

- **N-2 (informational; V5-revalidated).** C-3 LOC envelope
  (1.2k–2.0k) at `alpha-E-candidate-shortlist.md:410-413` is
  UNCHANGED through V5. The V5 fold lands at α-E §10 (line 756+),
  well below the §5 round-trip gate spec and well below C-3's
  implementation envelope binding sites. C-3's xtask + harness +
  generic codegen + corpora + bench extension surfaces are
  unmodified. The `[generated-size-budget]` clause binds wave LOC
  under > 20 % escalation; the V5 fold consumes 0 % of C-3's wave
  envelope.

- **N-3 (informational; V5-revalidated).** A-3 c/B LOC budget at
  `alpha-A-results-extraction.md:296-319` allocates 80–120 LOC routed
  through C-2's existing 600 LOC lower-bound envelope. The bounded
  worst case (600 baseline + 80 Skipper fallback + 120 c/B = 800 LOC)
  remains well inside the 1.08k ceiling. The V5 fold does not touch
  α-A; the cost routing is preserved. Bounded.

- **N-4 (informational; V5-revalidated).** α-E §10 cap table at
  `alpha-E-candidate-shortlist.md:756-760` preserves the
  C-1/C-2/C-3/C-5 = 30 min; C-4 = 45 min discipline verbatim at
  every cell value. The only in-table edit is the C-1 row's
  parenthetical identifier ("8 grammars" → "per rostered grammar"),
  which is roster-count-agnostic prose and does not perturb any cap
  value. Line offsets are stable through `:760`; only `:770-774`
  (reconciliation paragraph tail) and `:788` (§11 header) shift by
  +1. The cap-discipline reconciliation paragraph at `:762-774`
  (extended from V4's `:762-770`) still cites CH4 R3 + the addendum
  verbatim. No cap-value drift.

- **N-5 (informational; V5-revalidated).** HANDOFF §6 cap echo at
  `HANDOFF.md:162-165` reads "research 20 min / plan 15 min / redress
  30 min (45 min only for the addendum-amended decision-engine fold
  + C-4 per CONSOLIDATED §0.5 cap discipline)". HELD verbatim through
  V5; the V5 commit did not touch HANDOFF. Consistent with the V5
  §10 cap table reading (C-1/C-2/C-3/C-5 = 30 min; C-4 = 45 min);
  the HANDOFF echo and the α-E source are synchronised.

- **N-6 (informational; V5-revalidated).** SYNTHESIS §4 LOC-ceiling
  clause at `:326-329` uses "> 20 %" as the escalation threshold per
  α-F's V2 framing. HELD verbatim through V5. The threshold remains
  α-F's reasonable framing inside the lens's "stated and realistic"
  mandate.

- **N-7 (informational; V5-revalidated).** V4's N-7 closed the V3
  jq path naming ambiguity at the §5 round-trip gate. V5 does not
  perturb the §5 surface; the V4 closure carries verbatim. The same
  workspace-metadata clause (`workspace.metadata.bbnf.grammars`
  source path; `.metadata.bbnf.grammars` JSON path) is now also the
  binding the V5 §10 "live rostered-grammar enumeration (`cargo
  metadata | jq` over the grammar roster at HEAD)" clause references,
  closing a CH4-axis loop: both §5 (round-trip gate) and §10 (cap-
  discipline cluster total) bind to the same Lock 14 clause. The
  binding parity strengthens CH4's "stated and realistic" mandate
  on the cluster-wall-clock arithmetic.

- **N-8 (informational; V5-revalidated).** V4's N-8 partial-close on
  the HANDOFF §7 47-row carry-over guard and the SYNTHESIS ledger
  desync-closure carries through V5. V5 did not touch HANDOFF; the
  V4 citation-anchor repair (§7's cross-reference re-targeted from
  §1.3 to §0.2 lines 73-84) is preserved verbatim. The 47-row count
  and C-5's 29-row scribe contract are unchanged.

- **N-9 (informational; V5-revalidated; pattern extended).** V4's
  N-9 favoured the V4 §5 round-trip gate's derived-enumeration
  posture (zero per-grammar prose growth as the roster grows). V5
  extends the same pattern to the §10 cap table row + cluster-total
  arithmetic (zero per-grammar prose growth in the §10 surface as
  the roster grows). The pattern is now applied consistently across
  both §5 (gate text) and §10 (cap-discipline text) — both bind to
  `workspace.metadata.bbnf.grammars` instead of hardcoding a count.
  CH4 records this as a strengthening of the V4 favourable property
  across the α-E document.

- **N-10 (informational; V5-revalidated).** V4's N-10 confirmed the
  V4 commit's net-0 LOC delta on both files. V5's commit `--numstat`
  reports 6 ins / 5 del on alpha-E (net +1) and zero on every other
  file. The V5 +1 is contained to the §10 reconciliation paragraph
  tail; no other file is touched; no implementation envelope is
  perturbed. The CH4 interest in net delta is satisfied: the +1 net
  is documentary precision (the variable-rename language requires
  one additional line of prose to disambiguate N and M cleanly),
  not implementation growth. Bounded.

- **N-11 (informational; V5-introduced).** The V5 cluster-wall-clock
  arithmetic now reads "C-1 cluster total is N × 30 min of redress
  windows where N is the live rostered-grammar enumeration". For the
  live workspace at HEAD (per V4 commit message's enumeration of 9
  grammars: bbnf, json, css_l4, css_pretty, google_sheets, ebnf, bnf,
  csv, math), the C-1 cluster wall-clock total resolves to 9 × 30 =
  270 min (V4 hardcoded 8 × 30 = 240 min against a stale count). The
  S-P3 wave authors consuming the §10 table will read the live roster
  at S-P3 dispatch time, not at V5 spec-authoring time; this is the
  correct cost-discipline posture (the wall-clock budget tracks the
  live roster, not a snapshot count). The C-4 cluster wall-clock total
  resolves to M × 45 min where M ≥ 2 by E-8's two-grammar-family
  requirement; the typical exercise count is M = 2 (≈ 90 min lower
  bound, escalating if C-4 elects to exercise additional CSP-
  selectable shapes). Combined wall-clock bound for the C-1 + C-4
  cluster wave-program (post-PRUNE landing): 9 × 30 + ≥ 2 × 45 =
  ≥ 360 min of redress windows under the V5 phrasing, against the V4
  baseline's 8 × 30 + ≥ 2 × 45 = ≥ 330 min. The +30 min increase
  reflects the corrected live roster count (the V4 prose was
  arithmetically wrong against the live workspace by exactly 30 min);
  V5 lifts the arithmetic to roster-count-agnostic phrasing so the
  arithmetic remains correct as the roster evolves. CH4 records this
  as a *cost-discipline correction* favouring V5 (the V4 baseline
  carried a 30-min undercount; V5 closes it parametrically). Bounded
  under the cost-discipline lens.

## §3 — Recommended folds for V6 (if any)

NONE. CH4 V5 issues no fold recommendations. The cost surface is
rebound across SYNTHESIS, HANDOFF, α-E, α-A from V2 + V3 + V4; the V5
micro-fold preserves every binding from V4 verbatim (cap values; per-
candidate implementation envelopes; total envelope; same-wave consumer
rule; risk classes; wave alignment) and adds one *favourable*
strengthening (the §10 cap table row and cluster-wall-clock arithmetic
are now roster-count-agnostic, matching the V4 §5 round-trip gate's
derived-enumeration posture). The four-cycle ACCEPT chain (V2 + V3 +
V4 + V5 all CH4-clean) satisfies the two-consecutive-cycle convergence
rule (`ORCHESTRATOR.md §3Z`) for the COST lens with margin (V4 100 %
+ V5 100 % closes the chain at the V ≤ 5 ceiling). No further CH4-axis
folds are warranted; the cost-discipline surface is locked.

## §4 — Escalation flag

NONE.

The CH4 V5 cycle converges at lens-local 100 % ACCEPT-rate; every V2 +
V3 + V4 ACCEPT decision holds; the single V5 micro-fold (F-V5-α-E-1)
introduces no cost-envelope / risk-class / cap-value / wave-alignment /
same-wave-consumer regression and surfaces no fresh REJECT or REVISE
under the CH4 lens. The α-E V5 fold consumed 6 ins / 5 del = net +1
LOC inside the existing §10 surface to apply the V4 favourable
roster-count-agnostic posture (V4 §2.3 N-9) to the §10 cap table row
+ cluster-wall-clock arithmetic; the +1 net is contained to the §10
reconciliation paragraph tail and does not perturb any implementation
envelope. The N / M variable rename closes the parametric symbol
collision the V5 fold introduces (N now binds C-1 grammar roster
count; M now binds C-4 CSP-selectable shape count). The total envelope
5.65k – 8.38k is preserved verbatim at SYNTHESIS `:277` and α-E
`:89-92`; the per-candidate envelopes (C-1 2.8k–3.4k; C-2 0.6k–1.08k;
C-3 1.2k–2.0k; C-4 0.8k–1.4k; C-5 0.25k–0.5k) are all preserved
verbatim. The cap values per cell (20 / 15 / 30 / 45 / 30) are all
preserved verbatim.

The V5 aggregator should consume this CH4 V5 disposition as input to
the CONSOLIDATED V5 verdict; CH4 contributes the fourth consecutive
100 % link to the convergence chain per `ORCHESTRATOR.md §3Z` and
closes the two-consecutive-cycle binding (V4 100 % + V5 100 %) at the
V ≤ 5 ceiling. The SK-V14 Pass Alpha bracket locks at V5 convergence;
the cost-discipline surface is durable; G-Alpha auto-signs per the
SK-V14 ORCHESTRATOR-PROMPT pin; the orchestrator proceeds directly to
S-P0. CH4 lens-local convergence is unconditional and contributes no
blocker to the V5 close path.
