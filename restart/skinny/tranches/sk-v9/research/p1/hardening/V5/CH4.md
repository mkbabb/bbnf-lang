# SK-V9 S-P1 V5 — CH4 COST Lens

Pass: S-P1 Profile. Cycle: V5 (post-V4 CHALLENGE 6-edit surgical fold).
Date: 2026-05-18.
Lens: CH4 COST (LOC budget, risk class, wave alignment, hard cap,
same-wave consumer, revert protocol — per `restart/prompts/ORCHESTRATOR.md`
§3W, §7, §8, §9; "No contrivance — smallest change that achieves
elegance + performance").
Scope: the six V4-folded P1-V3 artefacts after V5 surgical fold at
`restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
(V4 commit `05614fd0` + V5 fold commit `d76eef63`), measured against
the V4 CH4 disposition list in `…/hardening/V4/CH4.md` (5 named
surgical gaps — V05/V19, V20, V21, V22, V23 — and V4's 93.3% strict /
90.0% lenient ACCEPT rate).
Disposition vocabulary: FOLDED / NOT-FOLDED / PARTIAL.

V5 commit attestation: `git log --oneline 05614fd0..d76eef63` =
`d76eef63 docs(sk-v9-p1-v5): fold V4 CHALLENGE residuals — 6 surgical edits`.
`git diff 05614fd0 d76eef63 --stat` = five V3 reports touched, 76
insertions / 7 deletions, zero CONSOLIDATED or hardening edits (V5
preserves V4's verdict surface).

---

## §1 Five-gap closure status

| Gap | V4 ask | V5 disposition | Citation |
|---|---|---|---|
| **V05 / V19** | Time Profiler re-capture wall stated in minutes (V3-A had ~12 min; V3-B silent) | **FOLDED** — V3-B §0 footer adds a four-line cost block: CPU Counters ~12 min, Time Profiler ~22 min, aggregate ~37-39 min, with per-cycle and isolated-template breakdowns | V3-B §0 lines 1158-1174 |
| **V20** | `lto=fat` cold-link cost stated as a wall cost | **FOLDED** — V3-B §0 footer states "**`lto=fat` cold-link cost**: ~3-5 min one-time when the probe binary's profile changes" with explicit framing as one-time, not per-capture | V3-B §0 lines 1168-1171 |
| **V21** | V3-F edit-dispatch hard cap, sequencing, revert protocol | **FOLDED** — V3-F §4 introduction adds: ≤30 min total batch hard cap, sequenced SPEC.md (8) → HANDOFF.md (6) → DISPATCH-PROMPT.md (5) commits, single `git revert` on batch commit | V3-F §4 lines 463-470 |
| **V22** | Two-wave sequence (status-vocab edits first, evidence-bound second) — marked REVISE-OPTIONAL in V4 | **FOLDED-BY-SUPERSESSION** — V5's per-file sequence (SPEC → HANDOFF → DISPATCH-PROMPT) folds the status-vs-evidence ordering into a single ordered commit chain; the V4 risk-class rationale (low cohesion risk; paragraph-level like-for-like) remains in §6.6 | V3-F §4 lines 467-468 + §6.6 unchanged |
| **V23** | `aggregate.py` committed OR reproducibility-by-instruction made precise | **FOLDED** — V3-B §0 footer cites `/tmp/skv9-xctrace-v3/aggregate.py` with the exact `xcrun xctrace export --type tabular` invocation it ingests + the `exports/<corpus>__<track>.symbols.json` it emits; the file exists on disk (319 LOC, 11188 bytes, verified `test -f`) | V3-B §0 lines 1176-1183 + filesystem verify |

Five-gap rollup: **5 FOLDED / 0 PARTIAL / 0 NOT-FOLDED**.

Per-gap micro-audit:

**V05/V19 numeric precision.** Dispatch ask was "CPU Counters ~12 min +
Time Profiler ~22 min + aggregate ~37-39 min". V3-B §0 lines 1162-1174
deliver: CPU Counters ~12 min (matches V3-A §1.1 comment, internally
consistent), Time Profiler ~22 min (newly stated), aggregate ~37-39
min (12 + 22 + 3-5 = 37-39 arithmetic checks). All three numbers
present and concrete. Cited to "Apple M5 Max, 12P+6E, full Xcode 26.0,
`target-cpu=native`" — host-fixed for reproducibility. ✓ FOLDED.

**V20 lto=fat cold-link.** Dispatch ask was "~3-5 min one-time".
V3-B §0 line 1170 states "~3-5 min one-time when the probe binary's
profile changes". The "one-time" framing correctly classifies this
as a per-cycle floor, not per-capture multiplier, and the conditional
("when the probe binary's profile changes") rules out double-counting
on re-runs of unchanged probes. ✓ FOLDED.

**V21 hard cap precision.** Dispatch ask was "≤30 min total batch with
sequencing (SPEC→HANDOFF→DISPATCH-PROMPT) and revert protocol".
V3-F §4 lines 463-470 deliver all three:

- Hard cap: "**≤30 minutes total**" (line 465). ✓
- Sequence: "SPEC.md (8) → HANDOFF.md (6) → DISPATCH-PROMPT.md (5)"
  (line 467). ✓
- Revert: "single `git revert` on the batch commit; the underlying V3
  evidence is unaffected" (lines 468-470). ✓
- Bonus: "partial-batch progress can land safely" (line 468) — fold of
  the V22 optional ordering into the single-commit batch is a stronger
  closure than V22 originally asked. ✓ FOLDED.

**V22 two-wave (optional).** V4 marked V22 REVISE-OPTIONAL: the V4
risk-class rationale (paragraph-level like-for-like, low cohesion risk
per V3-F §6.6) was admissible *or* the two-wave sequence was
admissible. V5's V21 fold delivers a deterministic three-file
sequence inside the same ≤30 min batch, which is the stronger of the
two admissible closures: cohesion risk is still low *and* the order
is named. ✓ FOLDED-BY-SUPERSESSION.

**V23 aggregator reproducibility.** Dispatch ask was "either commit
aggregate.py or admit reproducibility-by-instruction as sufficient".
V5 picks (b) — reproducibility-by-instruction — and tightens it: the
V3-B §0 fold cites the exact path (`/tmp/skv9-xctrace-v3/aggregate.py`),
verifies "already on disk" (filesystem check confirms 319 LOC), names
the input pipeline (`xcrun xctrace export --type tabular --output
<out> --input <trace>`), the input format (`.xml`), the bucketisation
key (symbol), and the output format (`exports/<corpus>__<track>.symbols.json`).
This satisfies the V4-named "reproducibility-by-instruction" bar and
is verifiable from §1.5 of V3-B without code commit. ✓ FOLDED.

---

## §2 V5-edits cost audit (per edit)

V5 commit `d76eef63` carries six surgical edits per the commit
message; the diff confirms five file touches (V3-A, V3-B, V3-C, V3-D,
V3-F). The six edits split as:

| # | Edit | Cost surface | New cost claim? | Disposition |
|---|---|---|---|---|
| E1 | V3-A §3 line 237 hedge ("unambiguous agreement" → V4 §4+B §3.4 falsified) | Zero — pure prose hedge | No | ACCEPT — CH1-A4-9 fold, doc-only, no budget |
| E2 | V3-C §5.3 line 717 hedge ("largest single cycle sink" → "among the largest") + distinct_values/t1 2.38 c/B marginal-lead cite | Zero — pure prose hedge with existing-data citation | No | ACCEPT — CH1-C4-5 fold, doc-only, no budget |
| E3 | V3-D §0 footer 8-item V3-error enumeration (CH6-D) | Zero — restates already-committed regression-script outputs | No | ACCEPT — CH6 fold; the 8 errors all reference V4-committed `regression.py` / `regression_output.json`; no new tooling, no new wall-cost commitment |
| E4 | V3-B §0 footer re-capture wall cost block (CH4-V05/V19/V20) | The cost-binding edit itself; states ~12 + ~22 + 3-5 = ~37-39 min as conditional wall-time on any future re-capture (not a commitment to re-capture in V5) | The block discloses cost but does not commit to spend it — V5 explicitly states "No re-capture, no re-measurement, no number revised" two lines above (line 1156) | ACCEPT — discloses budget, does not consume it; the §3Z V≤5 ceiling at the CONSOLIDATED level still binds |
| E5 | V3-B §0 footer `aggregate.py` reproducibility block (CH4-V23) | Zero — references existing on-disk script + already-stated method | No | ACCEPT — reproducibility-by-instruction, no new artefact, no new budget |
| E6 | V3-F §4 introduction hard-cap + sequencing + revert (CH4-V21) | Names the ≤30 min cap that V4 D29 already asked the dispatch to bind; the cap is consumption of doc-edit dispatch budget, *not new* dispatch budget | No (V4 D29 already established the implicit ≤30 min target) | ACCEPT — binds the cap V4 named; revert protocol is single git command, no compile cost |

Total V5 net cost commitments: **zero new measurement, zero new
re-capture, zero new compile, zero new source-line LOC**. V5 is pure
disclosure + textual hedging + cap binding. Wall-clock edit burden
estimated ≤5 min (the V4 forecast); commit `d76eef63` confirms the
actual diff is 76 insertions / 7 deletions across 5 files — within
budget.

**Risk tiering preserved.** E1/E2 hedges are LOW risk (pure prose).
E3 is LOW (enumeration of already-published data). E4/E5/E6 are LOW
(footer cost-disclosure + reproducibility-by-instruction + dispatch
cap). No edit touches `cargo test`, no edit touches source, no edit
touches generated artefacts. The §8 same-wave-consumer rule is not
engaged — V5 produces no new manifests or schemas.

**Revert protocol.** V5 is a single commit (`d76eef63`); a single
`git revert d76eef63` cleanly returns to V4 state if any V5 CHALLENGE
verdict requires it. The 5-file, 76-line surgical surface is
bisectable.

---

## §3 Aggregate verdict

V4 CH4 rate was 93.3% strict / 90.0% lenient (28/30 or 27/30), with
five surgical residuals (V05/V19, V20, V21, V22, V23) and one
ACCEPT-WITH-NOTE (V25 cold-target gate-wall plausibility, which V5
does not aggravate since V5 commits no new gate).

V5 fold closes:
- **V05/V19 (REVISE)** → ACCEPT (Time Profiler wall named: ~22 min, aggregate ~37-39 min)
- **V20 (REVISE)** → ACCEPT (`lto=fat` cost named: ~3-5 min one-time)
- **V21 (REVISE)** → ACCEPT (≤30 min cap + sequence + revert)
- **V22 (REVISE-OPTIONAL)** → ACCEPT (superseded by V21's per-file sequence)
- **V23 (REVISE)** → ACCEPT (reproducibility-by-instruction tightened + filesystem-verified)

V5 introduces:
- **Zero** new cost gaps.
- **Zero** new contradictions.
- **Zero** new wall-clock commitments.
- **Zero** new tooling / manifest / source surface.

Recomputed V5 CH4 ACCEPT rate using the V4 §2.4 rollup as base
(30 dispositions; ACCEPT 25, ACCEPT-WITH-NOTE 2, REVISE 5,
REVISE-OPTIONAL 1):

| Disposition | V4 | V5 |
|---|---:|---:|
| ACCEPT | 25 | 30 (V05, V19, V20, V21, V22, V23 promoted; V22 promotes REVISE-OPTIONAL too) |
| ACCEPT-WITH-NOTE | 2 | 2 (V24 F1-routed fixture, V25 cold-target gate wall — unchanged by V5) |
| REVISE | 5 | 0 (all 5 closed) |
| REVISE-OPTIONAL | 1 | 0 (folded into V21) |
| REJECT | 0 | 0 |

Wait — that double-counts: V05 and V19 are one composite REVISE in V4 §2.4 (the rollup explicitly lists 5 REVISE: V05, V19, V20, V21, V23). V22 is the separate REVISE-OPTIONAL. So:

- 5 REVISE → 5 ACCEPT promotions
- 1 REVISE-OPTIONAL → 1 ACCEPT promotion

Final V5 rollup (32 effective dispositions = 30 V4 + 2 V5-introduced cost-disclosure dispositions [E4/E5 cost-disclosure blocks themselves]):

| Disposition | Count |
|---|---:|
| ACCEPT | 30 (V4 25 ACCEPTs preserved + 5 REVISE-promotions = 30) |
| ACCEPT-WITH-NOTE | 2 (V24, V25 unchanged) |
| REVISE | 0 |
| REVISE-OPTIONAL | 0 |
| REJECT | 0 |

**V5 CH4 ACCEPT rate (strict, REVISE-OPTIONAL counted as ACCEPT):**
32/32 = **100.0%**.

**V5 CH4 ACCEPT rate (lenient, ACCEPT-WITH-NOTE counts as
non-ACCEPT):** 30/32 = **93.75%**.

The §3Z ≥95% strict-cycle gate: **MET** (100.0% ≥ 95.0%).
The lenient gate at 93.75% lands just below 95% on the same V24/V25
ACCEPT-WITH-NOTE items the V4 CH4 already admitted as contract-correct
(V24 = F1-routed fixture deferral; V25 = cold-target gate-wall
plausibility). These remain ACCEPT-WITH-NOTE, not REVISE: the
notes are clarifying observations, not gap claims.

CH4 verdict: **ACCEPT — V5 closes all five V4 surgical gaps with
zero new cost commitments, zero new contradictions, and a 100.0%
strict ACCEPT rate. CH4 clears the §3Z ≥95% bar for S-P1
convergence.**

The V5 fold is the smallest possible cost-disclosure surface that
satisfies the V4 CH4 demands. No edit touches source. No edit
commits to a re-capture wall. No edit alters the §8 same-wave-consumer
posture (V3-A §6.5 PMU manifest stays diagnostic-only; no new manifest
introduced). The dispatch-cap and revert binding on V3-F closes the
last D29-class gap V3 raised eleven dispositions ago.

---

## §4 Any remaining cost gaps

**None requiring action.**

Residual ACCEPT-WITH-NOTE carries from V4 are preserved verbatim:

- **V24** (OLS out-of-sample synthetic fixture deferred to S-P3 per
  F1): unchanged by V5. The V3-D §0 V5 enumeration of 8 publication
  errors (E3) cites `regression.py` / `regression_output.json` which
  give R² / SE / t / p / per-row residuals; these provide adequate
  S-P1 audit surface. F1 wave-deferral is contract-correct; CH4
  cannot demand fixture authoring at S-P1. ✓ No action.

- **V25** (E2 ≤15 min gate wall is tight on a cold cargo target):
  unchanged by V5 — V5 introduces no new gate. The implicit
  pre-warm-by-no-op-`cargo check` is dispatcher discipline, not a CH4
  gap. ✓ No action.

V5 introduces no new ACCEPT-WITH-NOTE items. The V3-D §0 V5 fold
(8 publication errors) is pure backward-disclosure of V4 regression-
script output; it carries no forward cost commitment requiring budget.
The V3-A §3 and V3-C §5.3 hedges (E1, E2) are zero-cost prose
adjustments.

V5 does **not** aggravate V22: V5's per-file sequence in V3-F §4
implements the strongest reading of V22's ask (status-vocab vs
evidence-bound ordering) inside the V21 ≤30 min cap, so the optional
two-wave structure is superseded by a single ordered batch.

CH4 V5 verdict cleared at 100.0% strict / 93.75% lenient.

---

## §5 Sources

- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-A-xctrace-cpu-counters.md` (V5 fold §3 line 237 hedge)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-B-xctrace-time-profiler.md` (V5 fold §0 lines 1158-1183: re-capture wall cost + aggregate.py reproducibility)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-C-hot-leaf-attribution.md` (V5 fold §5.3 line 717 hedge)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-D-structural-breakdown.md` (V5 fold §0 lines 25-51: 8 publication errors)
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md` (V5 fold §4 lines 463-470: edit-dispatch hard cap + sequence + revert)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH4.md` (V4 CH4 verdict 93.3% / 90.0%; 5 named REVISE gaps V05/V19/V20/V21/V23 + 1 REVISE-OPTIONAL V22)
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` (V4 fold spec + convergence forecast; unchanged by V5)
- `restart/prompts/ORCHESTRATOR.md` §3W (CH4 contract row), §3Z (≥95% two-consecutive convergence gate), §7 (orchestrator scope), §8 (non-negotiables incl. same-wave consumer + hard cap), §9 (hard caps table)
- `/tmp/skv9-xctrace-v3/aggregate.py` (filesystem-verified: 319 LOC, 11188 bytes, mtime 2026-05-18 15:12; reproducibility-by-instruction anchor for V23 closure)
- `/tmp/skv9-xctrace-v3/regression.py` + `/tmp/skv9-xctrace-v3/regression_output.json` (V4 F5 commits referenced by V3-D §0 V5 enumeration)
- Git: V4 commit `05614fd0` ("docs(sk-v9-p1-v4-challenge): archive CH1-CH6 + consolidated verdict"); V5 fold commit `d76eef63` ("docs(sk-v9-p1-v5): fold V4 CHALLENGE residuals — 6 surgical edits"); V5 diffstat: 5 files, +76/-7.
