# CH5 — HIDDEN COUPLING disposition for SK-V9 S-P1 V5 cohort

Pass: S-P1 Profile. Cycle: V5. Lens: CH5 HIDDEN COUPLING.
Date: 2026-05-18.
Subjects: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
(V5-folded in place per `HARDENING-S-P1-V4-CONSOLIDATED.md` §"V5 fold
requirements" / commit `d76eef63`).
Output: this file.

CH5 verifies, per `restart/prompts/ORCHESTRATOR.md` §3W: no proposal
introduces a parallel substrate, a sidecar producer, a renamed-scanner
Lock-1 violation, or Track 1 ≡ Track 2 dishonesty; the substrate union
holds. The non-negotiables enforced are `LOCKS.md` Lock 1 ("Tape is the
substrate, properly unioned with direct-to-struct; … orthogonal codepaths
and parallel substrates are dead … A SIMD mask stream is a transient
producer, not a retained sidecar; if structural offsets are retained, the
structural projection IS the tape.") and Lock 14 (substrate carries ZERO
grammar-specific code), against the ledger of `skinny/REDRESS.md` 50–72
sidecar/projection rejections plus the SK-V8 W3 union rejection at
REDRESS 92.

V4 CHALLENGE CH5 returned **100% ACCEPT** (49/49) with all V3 REVISE
items folded cleanly; CH5 listed four long-tail risks (§4.1–§4.4) bound
to spec-citation rather than substrate change. V5 fold landed six
surgical edits per the V4 consolidation §"V5 fold requirements" (commit
`d76eef63`), all targeting CH4 / CH1 / CH6 residuals — none of the V5
fold items was CH5-blocking under V4. CH5's job in V5 is therefore to
audit that the six new V5 edits do not silently *re-open* a Lock-1
fault line by smuggling a sidecar, renamed scanner, parallel substrate,
or Track 1 ≡ Track 2 collapse into the report bodies under cover of
CH4 / CH1 / CH6 surgery.

## §1 — V5-edits Lock-1 audit (per edit)

The six V5 surgical edits are enumerated in `d76eef63` commit body and
land at `HARDENING-S-P1-V4-CONSOLIDATED.md:46-61` ("V5 fold
requirements"). Below, each edit is audited against the Lock-1
non-negotiable set: (a) no new substrate variant beyond
`LayoutFacts.backend_shape ∈ {EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage}` (`LOCKS.md` Lock 10); (b) no sidecar
producer; (c) no renamed scanner; (d) no Track 1 ≡ Track 2 collapse;
(e) no retained PMU/cycles stream as comparator producer.

### §1.1 — Edit V5-1 (V3-A §3 line 237; CH1-A4-9 narration qualification)

**Surface.** `skv9-p1-v3-A-xctrace-cpu-counters.md:237-246`. The V3
sentence "The agreement is unambiguous: every parse-only Track 1 row,
including the string-heavy and unicode-heavy rows the dispatch said to
load-bear, has the same hot leaf at ~95-99% self-time" is replaced
with "The V2 baseline (superseded; see §4 and P1-V3-B §3.4): every
parse-only Track 1 row appears to share the same hot leaf at ~95-99%
self-time. That ~95-99% figure is a frame-pointer-coalescing artefact
of the samply mode-I capture; xctrace Time Profiler with DWARF
resolves the inlined leaves and falsifies the single-symbol
attribution."

**Lock-1 audit.** The edit is a narration qualification on the V2
samply baseline. Five Lock-1 checks:

(a) **New substrate variant?** No. The edit references the existing
`dispatch_value` fused-symbol body and the V2 samply mode-I capture;
the `LayoutFacts.backend_shape` variant set is not touched. The edit
explicitly defers sub-leaf attribution to S-P2 ("Samply alone cannot
do that; PMU + sub-leaf sample correlation is the only mechanical
path"), which carries forward the F1 "wave authorship deferred to
S-P3" disposition.

(b) **Sidecar producer?** No. The edit reframes an *existing*
sub-symbol attribution problem ("what's inside `dispatch_value`?")
without proposing any new producer. The "PMU + sub-leaf sample
correlation" phrase names the *diagnostic* attribution method, not a
producer surface. The PMU manifest's non-producer binding from V4 CH5
A.1 (`skv9-p1-v3-A-xctrace-cpu-counters.md:421-439`) carries forward
unchanged; this edit does not amend §6.5.

(c) **Renamed scanner?** No. The edit cites `dispatch_value` (the
LTO-fused parse-only entrypoint at `runtime/src/grammars/json/generated.rs:47`)
by its existing name; no scanner is renamed, no scanner is reframed
as a different surface. The "frame-pointer-coalescing artefact" phrase
references samply's *capture limitation*, not a scanner surface.

(d) **Track 1 ≡ Track 2 collapse?** No. The edit explicitly names
"every parse-only Track 1 row" — the Track-1 scope is preserved
verbatim. The "see §4 and P1-V3-B §3.4" cross-reference points to
P1-V3-B's per-symbol attribution (Track-1 oracle role); Track 2
(direct + typed) is not invoked. V4 CH5 A.5's "(`<track:track1|track2>`)
switch, never coalescing" disposition is not weakened.

(e) **PMU as retained comparator producer?** No. The edit's "PMU +
sub-leaf sample correlation is the only mechanical path" phrase
preserves PMU's *diagnostic* role per V4 CH5 F.6 (PMU/cycles umbrella)
and F.9 (gate-bar item 13). The PMU evidence is named as a
characteriser for "what's inside `dispatch_value`?", not as a route-
fact substrate for any admission gate.

**Disposition.** ACCEPT. The edit is a subtractive narration fix —
removing a falsified V2 attribution claim and binding it to the
"superseded; see §4 / B §3.4" cross-reference. No coupling-relevant
surface is amended.

### §1.2 — Edit V5-2 (V3-C §5.3 line 717; CH1-C4-5 hedge)

**Surface.** `skv9-p1-v3-C-hot-leaf-attribution.md:715-721`. The V3
sentence "The escape-codec class is the largest single cycle sink in
the entire 34-row table" is hedged to "The escape-codec class is
among the largest single cycle sinks in the 34-row table
(distinct_values/t1 per-string-span at 3.850 × 0.619 = 2.38 c/B is
marginally larger; cf. CH1 V4 A4-9 / C4-5 hedges)".

**Lock-1 audit.** The edit is a numerical hedge on the relative
ranking of two existing cycle sinks.

(a) **New substrate variant?** No. Both `escape-codec` (the V4-B
`escape_codec_hex_unit` primitive class, REDRESS 64/82 rejected
retained-validator pre-block) and `per-string-span scanner` (the
`string_tiny_scan` primitive class, REDRESS 60-65 pre-block class)
are existing classifier vocabulary entries per V4 CH2 fold. No new
class is introduced; the hedge concerns their *relative magnitudes*,
not their cardinality.

(b) **Sidecar producer?** No. The two named cycle sinks remain
diagnostic attribution surfaces. The 3.850 × 0.619 = 2.38 c/B
factorisation cites P1-V3-B's per-symbol attribution table; no new
producer wired.

(c) **Renamed scanner?** No. `per-string-span scanner` is the V4
canonical primitive-class name (V4 CH5 B.4 — grammar-neutral
classifier vocabulary). The hedge uses the canonical name; no
renaming, no aliasing.

(d) **Track 1 ≡ Track 2 collapse?** No. Both `y_string_unicode/t1`
and `distinct_values/t1` are explicitly Track-1 rows; the hedge does
not cross-reference Track 2. V3-C §1.2 disambiguation (V4 CH5 C.1)
holds.

(e) **PMU retained?** No. The c/B values cited are from P1-V3-A's
PMU rows (`/tmp/skv9-xctrace-v3/pmu_rows.tsv`) which V4 CH5 §1.3
bound to "diagnostic profile evidence" status. The hedge consumes
the manifest *as characteriser*, in line with the §6.5 non-producer
binding.

**Disposition.** ACCEPT. The edit is a numerical-rank-only hedge;
both sinks remain admitted classifier-vocabulary entries. No new
producer; no sidecar; no Track collapse.

### §1.3 — Edit V5-3 (V3-D §0 footer; CH6-D V3 publication errors)

**Surface.** `skv9-p1-v3-D-structural-breakdown.md:25-51`. New
paragraph enumerating 8 V3 publication errors (~8× over-stated OLS
coefficients, missing R², superseded "10% cut 7/11" forecast, 4 of 11
LOSS rows uncloseable by delimiter-only, sign-convention provenance
gap, per-row residuals absent).

**Lock-1 audit.** The edit is an *error enumeration* — listing V3
publication faults that V4's regression-script commit surfaced. Five
checks:

(a) **New substrate variant?** No. The enumeration cites the
existing OLS regression (`/tmp/skv9-xctrace-v3/regression.py`) and
existing per-row residuals; no new substrate, no new
`LayoutFacts.backend_shape` variant.

(b) **Sidecar producer?** No. Critically, item 5 *removes* the V3
"10% per-quote cut clears 7/11 LOSS rows" forecast — a subtractive
move that explicitly retires a delimiter-only intervention claim,
which would have been precisely the surface CH5 watches for sidecar
proposals. The replacement "4 of 11 (unicode_mixed, unicode_escapes,
y_string_unicode, gsoc-2018) cannot be closed by a delimiter-only
intervention because the throughput gap exceeds the entire delimiter
contribution" is a *hypothesis-sized finding, not a wave-sized
intervention* — explicit subtractive disposition.

**Lock-1 cardinality discipline preservation check (Q3 of the
review brief):** the 8-item enumeration does **not** introduce a
parallel substrate proposal anywhere in the list. Items 1-4 are pure
narration corrections (coefficient magnitudes, R², significance).
Items 5-6 are subtractive (V3 forecasts removed). Item 7 is provenance
(script commit). Item 8 is residual-table publication. Every item
either corrects a narration or removes a V3 wave-class proposal; none
authors a new producer. Item 5's "hypothesis-sized finding" framing
explicitly defers wave authorship (consistent with F1's S-P3 deferral
and V4 CH5 §4.3). Lock-1 cardinality (one substrate, transient
producers only) is preserved.

(c) **Renamed scanner?** No. The enumeration cites no scanner
surface. The "per-quote" / "per-number" / "delimiter" terms are
regression-coefficient names, not scanner names.

(d) **Track 1 ≡ Track 2 collapse?** No. The enumeration is silent on
the Track distinction; the underlying regression (the corpus-level
correlation table at D §1) is Track-1 only per the V3 column definition
(`Mbps_p / Mbps_d / Mbps_t` — three separate columns, never coalesced).
The enumeration does not coalesce them.

(e) **PMU retained?** No. The OLS regression is over structural
counts (q/B, n/B densities) and `ns_per_byte`, not PMU cycles. The
regression artefact (`regression.py`, `regression_output.json`) is in
`/tmp`, not committed to substrate. PMU manifest non-producer binding
(V4 CH5 §1.3) is independent of this regression and unaffected.

**Disposition.** ACCEPT. The enumeration is purely subtractive +
narration; it tightens honesty without introducing any new substrate
or producer. The "4 of 11 uncloseable by delimiter-only" claim
explicitly *reduces* the surface area on which a future sidecar
proposal could land (a smaller delimiter-class intervention covers
fewer rows; the remaining rows escalate to S-P3 hypothesis scope, not
S-P1 wave authoring).

### §1.4 — Edit V5-4 (V3-B §0 footer; CH4-V05/V19/V20 re-capture wall cost)

**Surface.** `skv9-p1-v3-B-xctrace-time-profiler.md:1158-1174`. New
paragraph adding deterministic wall costs: xctrace CPU Counters ~12
min, Time Profiler ~22 min, `lto=fat` cold-link ~3-5 min one-time,
aggregate ~37-39 min.

**Lock-1 audit.** The edit is a *capture cost annotation* — naming
how long a re-capture run takes on the SK-V9 host.

(a) **New substrate variant?** No. The wall costs reference existing
xctrace templates (CPU Counters, Time Profiler), the existing probe
binary (`xctrace_probe`), and the standard release profile (`lto=fat`
is the existing `LOCKS.md` Lock 15 requirement, not a new substrate).
No new variant.

(b) **Sidecar producer?** No. The edit names *re-capture cost*, not
a re-capture artefact. The capture-output paths (`exports/`,
`pmu_rows.tsv`) are unchanged from V3/V4; the wall-cost annotation
does not propose committing the capture output as a producer. V4 CH5
A.1 / §1.3's "diagnostic profile evidence, non-producer" binding for
`pmu_rows.tsv` carries forward; this V5 edit does not amend the §6.5
paragraph or weaken its binding.

**Lock-1 question Q5 of the review brief (re-capture wall cost):**
the wall-cost annotation enumerates measurement wall-time for the
existing characterisation surfaces. It does *not* propose a new
producer or substrate. The annotation answers "how long does it cost
to re-run V3's measurement?", not "what new artefact would we admit?".
The two xctrace templates remain diagnostic capture surfaces; the
PMU manifest remains characteriser-bound per V4 CH5 §1.3. No new
producer is proposed.

(c) **Renamed scanner?** No. No scanner surface mentioned.

(d) **Track 1 ≡ Track 2 collapse?** No. The "17 corpora × {track1,
track2}" capture matrix preserves the two tracks as separate launches
(V4 CH5 A.5's per-process-launch isolation discipline). The probe
binary is invoked separately per track; no fusion.

(e) **PMU retained?** No. The wall costs reference the *capture
duration*; the captured artefact (`pmu_rows.tsv`) remains
diagnostic-only per V4 CH5 §1.3 + F.6 umbrella + F.9 gate-bar item 13.
The triple-binding holds.

**Disposition.** ACCEPT. The edit is a cost annotation on existing
diagnostic capture surfaces; no new producer, no substrate change,
no Track collapse.

### §1.5 — Edit V5-5 (V3-B §0 footer; CH4-V23 `aggregate.py` reproducibility-by-instruction)

**Surface.** `skv9-p1-v3-B-xctrace-time-profiler.md:1176-1183`. New
paragraph: "The TP-symbols aggregator script lives at
`/tmp/skv9-xctrace-v3/aggregate.py` (already on disk). Re-running it
against the captured `.trace` bundles deterministically regenerates
`exports/<corpus>__<track>.symbols.json`. The script is
reproducible-by-instruction…"

**Lock-1 audit.** This is the V5 edit most directly addressing the
review-brief's Q2 ("does it bind the script to diagnostic-only status,
not a production substrate?"). Five checks:

(a) **New substrate variant?** No. The script reads
`xcrun xctrace export --type tabular` output and bucketises by symbol;
the buckets are "the per-symbol self-time tables surfaced in §2-§3 of
this report" — i.e. the script *regenerates the existing report
content* from existing capture artefacts. No new substrate variant.

**Lock-1 question Q2 of the review brief (aggregate.py reproducibility
binding):** the V5 fold's language is unambiguous on diagnostic-only
status. Three textual evidences:

(i) **Location is `/tmp`**, not workspace-committed: "lives at
`/tmp/skv9-xctrace-v3/aggregate.py` (already on disk)". The script is
*not* placed under `crates/`, `tools/`, `xtask/`, or any repo-relative
path that would commit it as substrate. The CH4-V23 disposition is
the "reproducibility-by-instruction" branch (V5 fold requirement #4
"either commit it or admit reproducibility-by-instruction explicitly"
per `HARDENING-S-P1-V4-CONSOLIDATED.md:51`); V5 chose the
admit-reproducibility-by-instruction branch over the commit branch.

(ii) **Output destination is `exports/<corpus>__<track>.symbols.json`
under `/tmp/skv9-xctrace-v3/`**, not a retained per-corpus committed
manifest. The output stays inside the same diagnostic temp-dir as
`pmu_rows.tsv`; the V4 CH5 §1.3 "diagnostic profile evidence,
non-producer" binding for `pmu_rows.tsv` extends naturally to
`exports/*.symbols.json` via the same temp-dir scope.

(iii) **Consumer naming is `§2-§3 of this report`**, not a
`gate-json` or admission-gate consumer. The V4 CH5 A.1 binding ("does
not participate in admission gates and does not extend `RESULTS.md`
schema") applies isomorphically — the `aggregate.py` output is
consumed only by S-P1 narration (§2 / §3 tables in this report),
never by SK-V9 `gate-json` or any other admission gate. This matches
V4 CH5 §4.1's "ACCEPT with watch" discipline: any *future* wave that
proposes ingesting the script output into `gate-json` would re-trigger
CH5 on the new producer surface; V5 pre-empts that leak by binding
the script to diagnostic-only status by location (`/tmp`) and
consumer naming (§2-§3 report tables).

Verdict on Q2: **the script is bound to diagnostic-only status**.
The binding is implicit-by-location and explicit-by-consumer-naming
rather than by verbatim Lock-1 citation (the earlier V4 CH5 A.1 fold
used a verbatim Lock-1 quote at §6.5); a CH5-tight read could request
an explicit "non-producer" sentence parallel to A §6.5's binding. But
the existing language is sufficient for CH5 ACCEPT because: the §6.5
binding *already covers* "the manifest is a profiling artefact emitted
by the read-only `xctrace_probe` binary and consumed only by S-P1 /
S-P2 narration of cycle-cost decomposition"; the `aggregate.py` script
operates over the same trace bundles and produces the same shape of
narrative table, so the umbrella binding extends. No new producer is
proposed.

(b) **Sidecar producer?** No. Per the above, the script + its output
remain inside the `/tmp/skv9-xctrace-v3/` diagnostic root.

(c) **Renamed scanner?** No. No scanner surface.

(d) **Track 1 ≡ Track 2 collapse?** No. The output path schema
`exports/<corpus>__<track>.symbols.json` preserves the per-track
distinction (the `__<track>` segment binds track 1 vs track 2 into
separate files); the script does not coalesce them.

(e) **PMU retained?** No — and notably the `aggregate.py` script is
*not* the PMU aggregator; it is the TP-symbols aggregator (xctrace
Time Profiler export → per-symbol self-time JSON). PMU rows
(`pmu_rows.tsv`) come from CPU Counters via a separate path (per V3-B
§0 footer and V3-A §6). The V5 edit names this distinction
implicitly by tagging the script "TP-symbols aggregator" and citing
"`.trace` bundles" + "`xcrun xctrace export --type tabular`". The PMU
non-producer binding is unaffected.

**Disposition.** ACCEPT. The script is bound to diagnostic-only
status by location (`/tmp`), output schema (under the same diagnostic
temp-dir), and consumer naming (§2-§3 report tables, never
`gate-json`). The Q2 review concern is satisfied.

### §1.6 — Edit V5-6 (V3-F §4; CH4-V21 edit-dispatch hard cap)

**Surface.** `skv9-p1-v3-F-redress-reconciliation.md:463-470`. New
paragraph: "The full batch of 19 surgical edits below carries a single
dispatch hard cap of ≤30 minutes total. … sequenced commits SPEC.md
(8) → HANDOFF.md (6) → DISPATCH-PROMPT.md (5) so partial-batch
progress can land safely. Revert protocol: single `git revert` on the
batch commit; the underlying V3 evidence is unaffected."

**Lock-1 audit.** This is the V5 edit most directly addressing the
review-brief's Q4 ("do the proposed SPEC/HANDOFF/DISPATCH-PROMPT edits
respect Lock-1 — no parallel substrate proposed?").

(a) **New substrate variant?** No. The 19 edits enumerated below the
new V5 paragraph (§4.1 Edit A-I excluding Edit E; §4.2 A-F; §4.3 A-E,
per the existing V3-F structure) are *narration tightenings* on
SPEC.md / HANDOFF.md / DISPATCH-PROMPT.md. The substantive Lock-1-
adjacent edit in the list — Edit F at lines 547-559 — *strengthens*
the PMU non-producer clause: "V3 real-PMU c/B is a diagnostic
characteriser of hot leaves, not a producer; it does not enable any
behavior admission path that was blocked in V2." This matches V4 CH5
F.7's prior fold and is *additive* to the §1 non-negotiable. No
edit proposes a new `LayoutFacts.backend_shape` variant; no edit
proposes a parallel substrate.

**Lock-1 question Q4 of the review brief:** scanning the 19 enumerated
edits (V3-F §4.1 / §4.2 / §4.3) under the V5 hard-cap envelope:

- Edits A-D (SPEC §0 status + authority block + dispatch lock + §0.2
  table): pure status-disposition updates moving V2-BLOCKED → V3
  capture-in-flight. No new substrate.
- Edit E: explicitly *deferred* — "do not edit §0.3 in this pass". A
  subtractive disposition.
- Edit F (SPEC §1 non-negotiable PMU clarifier): *strengthens* the
  PMU non-producer clause; verbatim quoted above. Lock-1-positive.
- Edit G (SPEC §2 Wave Manifest Interlock row): updates trace-bundle
  citation. No new substrate.
- Edit H (SPEC §4 BLOCKED → G-S-P1-RERUN-CONVERGED): path-to-
  convergence narration. No new substrate.
- Edit I (SPEC closing — out of CH5 scope per V3-F's §4.1 grouping).
- Edits A-F under HANDOFF.md §4.2: HANDOFF §5 umbrella additions per
  V4 CH5 F.4's four class umbrellas. *Broadens* pre-block surface;
  Lock-1-positive.
- Edits A-E under DISPATCH-PROMPT.md §4.3: dispatch vocabulary
  tightenings. No new substrate.

None of the 19 edits proposes a new producer, a new
`LayoutFacts.backend_shape` variant, a renamed scanner, or a Track
collapse. The V5 hard-cap paragraph itself adds three pieces of
*orchestration* discipline: total wall budget (≤30 min), sequenced
commit order (SPEC → HANDOFF → DISPATCH-PROMPT), and a revert protocol
(single `git revert` on the batch commit). None is a substrate
artefact; all three are workflow conventions.

(b) **Sidecar producer?** No. The hard-cap paragraph names a *commit
batch envelope*, not a substrate artefact. The "underlying V3
evidence is unaffected" clause makes this explicit: the V5 hard-cap
binds the *edit application*, not the *evidence body*.

(c) **Renamed scanner?** No. No scanner surface.

(d) **Track 1 ≡ Track 2 collapse?** No. The hard-cap paragraph is
silent on the Track distinction; the underlying 19 edits preserve V4
CH5 A.5's per-track isolation discipline.

(e) **PMU retained?** No. The hard-cap paragraph does not name PMU;
Edit F under §4.1 *strengthens* the PMU non-producer clause as cited
above.

**Disposition.** ACCEPT. The hard-cap paragraph is orchestration
discipline (commit envelope + revert protocol), not a substrate
proposal. The 19 underlying edits remain narration tightenings; Edit
F is Lock-1-positive (strengthens PMU non-producer clause). Q4 of
the review brief is satisfied: the SPEC/HANDOFF/DISPATCH-PROMPT edits
respect Lock-1 in full.

## §2 — Aggregate verdict

Disposition tally across §1: **6 V5 edits; 6 ACCEPT, 0 REVISE, 0
REJECT.**

Per-edit summary:

| # | Edit | Surface | Verdict |
|---:|---|---|---|
| 1 | V3-A §3 L237 narration qualifier | `skv9-p1-v3-A:237-246` | ACCEPT |
| 2 | V3-C §5.3 L717 hedge | `skv9-p1-v3-C:715-721` | ACCEPT |
| 3 | V3-D §0 footer V3 publication errors | `skv9-p1-v3-D:25-51` | ACCEPT |
| 4 | V3-B §0 footer re-capture wall cost | `skv9-p1-v3-B:1158-1174` | ACCEPT |
| 5 | V3-B §0 footer `aggregate.py` reproducibility | `skv9-p1-v3-B:1176-1183` | ACCEPT |
| 6 | V3-F §4 edit-dispatch hard cap | `skv9-p1-v3-F:463-470` | ACCEPT |

**ACCEPT rate: 6/6 = 100.0%.** V5 clears the CH5 lens at the
convergence threshold (≥95% ACCEPT per `ORCHESTRATOR.md` §3Z) with
strict margin. Combined with V4's 100% ACCEPT, V5 is the second
consecutive qualifying cycle on CH5 — per §3Z the lens has converged
for S-P1.

The V4 CH5 V3-REVISE folds (REVISE 1 D.4 cardinality binding; REVISE
2 D.5 escape-complete subtractive deletion; A.6 PMU manifest §6.5
diagnostic-only binding) carry forward unchanged across V5 — none of
the six V5 edits amends those bindings. The triple-binding for PMU
non-producer status (V4 CH5 F.6 umbrella + F.7 SPEC clarifier + F.9
gate-bar item 13) is *reinforced* by V5 edit 6's Edit F clarifier
landing inside the SPEC.md §1 non-negotiable surface.

Cohort honours Lock 1 strictly across V5:

- **Substrate cardinality at one.** No V5 edit introduces a new
  `LayoutFacts.backend_shape` variant; the five-variant set
  ({EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}) per
  `ARCHITECTURE.md` §7.3 is preserved. D §6.5's "no new `BackendShape`"
  forbiddance (V4 CH5 D.6) carries forward.
- **Diagnostic substrate union preserved.** The PMU manifest
  (`pmu_rows.tsv`) and the TP-symbols aggregator output
  (`exports/<corpus>__<track>.symbols.json`) remain bound to
  diagnostic-only status. Both artefacts live under
  `/tmp/skv9-xctrace-v3/`, neither extends `RESULTS.md` schema,
  neither feeds `gate-json`.
- **Track 1 vs Track 2 distinction preserved.** All V5 edits respect
  the per-track scope. V4 CH5 A.5 / C.1 / C.4's bench-distinct
  discipline carries forward.
- **No renamed scanner.** V4 CH5 E.2's "critical-distinction"
  subordinate clause (NEON `match_tiny_plain_string` deletion vs
  scalar `match_tiny_plain_string_with_cap` preservation) is not
  reopened by any V5 edit.
- **PMU evidence remains characteriser, never producer.** V5 edit 4
  (re-capture wall cost) costs the re-measurement workflow, not a new
  producer wiring. V5 edit 5 (`aggregate.py`) binds the script to
  `/tmp` location + §2-§3 narration consumer, never to `gate-json`.
  V5 edit 6's underlying SPEC Edit F strengthens the §1 non-negotiable
  PMU clause.
- **Subtractive disposition discipline preserved.** V5 edits 1, 3, 6
  are subtractive (V5-1 retracts a falsified V2 attribution; V5-3
  retracts V3 OLS forecasts; V5-6 sequences a revertible batch). V5
  edits 2, 4, 5 are annotation-only (hedge, cost annotation,
  reproducibility-by-instruction). None is additive in the producer-
  surface sense.

The "renamed-scanner Lock 1 violation" failure mode remains positively
guarded by V4 CH5 E.2; V5 does not re-open it. The "Track 1 ≡ Track
2 dishonesty" failure mode remains positively guarded by V4 CH5 A.5
/ C.1 / C.4; V5 preserves the per-track distinction in every edit.
The "sidecar producer" failure mode remains positively guarded by V4
CH5 A §6.5 binding + D §6.1 REPLACES binding + F's umbrella iv
triple-binding; V5 does not amend any of these and reinforces the SPEC
§1 clause via edit 6.

## §3 — Any new sidecars

**None.** Audit findings:

- **V5 edit 5 (`aggregate.py` reproducibility) is the only edit that
  names a new diagnostic artefact** (the TP-symbols aggregator output
  at `/tmp/skv9-xctrace-v3/exports/<corpus>__<track>.symbols.json`).
  Per §1.5 above, the script + output are bound to diagnostic-only
  status by location (`/tmp`, not workspace-committed), schema (under
  the same diagnostic temp-dir as `pmu_rows.tsv`), and consumer naming
  (§2-§3 report tables, never `gate-json`). The umbrella binding from
  V4 CH5 §1.3 ("a profiling artefact emitted by the read-only
  `xctrace_probe` binary and consumed only by S-P1 / S-P2 narration of
  cycle-cost decomposition") extends naturally to the
  `aggregate.py`-derived JSON since both surfaces share the same
  diagnostic provenance (xctrace `.trace` bundles → narrative tables
  in §2-§3 / §6.5).

- **V5 edit 4 (re-capture wall cost) does not name a new artefact**;
  it costs the existing capture workflow. The captured artefacts
  (`pmu_rows.tsv`, TP `.trace` bundles) are V3/V4 surfaces with
  existing diagnostic-only bindings.

- **V5 edit 6 (edit-dispatch hard cap) does not name a new artefact**;
  it is orchestration discipline (commit-batch envelope, sequence,
  revert protocol). The 19 enumerated underlying edits are SPEC /
  HANDOFF / DISPATCH-PROMPT narration tightenings; none proposes a
  new artefact or substrate.

- **V5 edits 1, 2, 3 do not name new artefacts**; they are pure
  narration corrections (qualifier, hedge, error enumeration) on V3
  prose.

### §3.1 — Long-tail watch: `aggregate.py` location stability

The CH5-conservative reading of V5 edit 5 is ACCEPT — the script's
`/tmp` location is appropriate for the
reproducibility-by-instruction branch (the V5 fold requirement #4
admitted "either commit it or admit reproducibility-by-instruction
explicitly"; V5 chose the second branch). The CH5 long-tail watch
is: if any future tranche proposes promoting the script from `/tmp`
into the workspace (e.g. under `tools/`, `xtask/`, or `crates/bbnf-
bench/`), that promotion would re-trigger CH5 on the new producer
surface, because a workspace-committed script reads as a *retained
infrastructure surface* in a way a `/tmp` script does not. The V4 CH5
§1.3 fold's parallel structure — "if a later wave wishes to gate on
cycles/B, it must wire a same-wave Mbps-isomorphic comparator that
resists the strict-vs-permissive flaw-probe gate before consuming PMU
evidence too" — extends here: if a later wave wishes to commit
`aggregate.py` into the workspace, it must wire a same-wave consumer
that justifies the promotion before the script becomes a retained
substrate.

CH5 disposition for V5: ACCEPT — the `/tmp` location binds the
script to diagnostic-only status for the V5 fold; the long-tail watch
is not a current leak, just a guard for any future tranche that
proposes a script promotion.

### §3.2 — Long-tail watch carry-forward from V4

V4 CH5's four long-tail risks (§4.1 PMU manifest deferred wiring; §4.2
WIN-row guard enforcement; §4.3 F1 wave-authorship deferral; §4.4
escape-complete subtractive fold) carry forward unchanged across V5.
None of the six V5 edits closes any of these (the closures are S-P3
scope per F1); none of the six V5 edits worsens any of these either.
The V4 dispositions ("ACCEPT with watch" on §4.1 and §4.3; "ACCEPT" on
§4.2 and §4.4) stand for V5.

### §3.3 — Convergence note

V5 is the second consecutive 100% ACCEPT cycle on CH5 (V4 = 100% on
49 V4 dispositions; V5 = 100% on 6 V5 edits as audited above, with
all 49 V4 dispositions inherited and unchanged). Per
`ORCHESTRATOR.md` §3Z, CH5 has converged for S-P1; the substrate
union holds across V3/V4/V5; no Lock-1 leak surfaced by the V5 fold.

## §4 — Sources cited

- `restart/locks/LOCKS.md:34` (Lock 1), `:60` (Lock 14).
- `restart/prompts/ORCHESTRATOR.md` §3W (CH5 contract), §3Z (≥95%
  convergence threshold, two-consecutive-cycle requirement).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V4/CH5.md`
  (V4 CH5 disposition; 100% ACCEPT, four long-tail watches).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  (V5 fold spec, items 1-8 at lines 46-61; V5 plan at lines 71-78).
- Commit `d76eef63` (`docs(sk-v9-p1-v5): fold V4 CHALLENGE residuals
  — 6 surgical edits`).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
  (V5-folded subjects; line ranges cited per-edit in §1).
- `restart/ARCHITECTURE.md` §7.3 (five-variant `LayoutFacts.backend_shape`).
- `skinny/REDRESS.md` 50-72, 82-84, 92, 93 (sidecar / projection
  rejection ledger; SK-V8 W3 union rejection).
- `skinny/RESULTS.md:139` (Track 1 vs Track 2 definition).
