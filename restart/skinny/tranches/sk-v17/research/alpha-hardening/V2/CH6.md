# CH6 — ANTI-PAPER-CLOSE — Pass Alpha SK-V17 cycle V2

Lens: CH6 ANTI-PAPER-CLOSE (ORCHESTRATOR §3W; PASS-ALPHA §3 CH6 = Next-Tranche-Impact).
Cycle: V2 (folds the V1 CHALLENGE dispositions). Subject: SK-V17 Pass Alpha artefacts
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + the αF output
(`sk-v17/SYNTHESIS.md` + `sk-v17/HANDOFF.md`, per PASS-ALPHA §6, αF writes to the
tranche root, not an `alphaF.md` — verified absent and correctly so).
Host: aarch64 Apple M5 Max. HEAD `1c5bd7a25` (`git rev-parse --short HEAD` confirmed).

CH6 focus (this dispatch): no candidate deferred to "future wave will detail";
goalset measurable + telemetry-bound (N≥50 cold sampling, lightningcss materializing
comparator); every claim orchestrator-citable; revert protocol / hard caps /
triumvirate discipline present where this pass owns them; no self-report of
"complete"/"wired"/"verified" standing without orchestrator-citable live evidence.

The load-bearing CH6 line (unchanged from V1): PASS-ALPHA §4.4 **explicitly defers**
the wave-by-wave revert protocol, owner paths, entry/exit gates, and per-wave hard
caps to skinny pass S-P3 (`sk-v17/SPEC.md`). A deferral the contract sanctions is NOT
a paper-close. A deferral the contract does NOT sanction — a candidate whose
*measurability* is pushed to a future phase, a gate that resolves to a promise rather
than a number, a self-report of a landed artefact, a mandate stated without its
authority — IS a paper-close (or its citability cousin) and CH6 rejects/revises it.
ORCHESTRATOR §3W:88 ("No deferral to a future phase") and §3W:211 ("No deferrals — a
wave closes on measurement, not a future-phase promise") are the binding text.

## V1 → V2 fold verification (the four V1 CH6 REVISEs)

V1 CH6 returned 52 ACCEPT / 4 REVISE / 0 REJECT (92.9%, below the §3Z bar), all four
REVISEs the same shape: a number or mandate stated without its falsifiable/citable
binding. CH6 V2 re-reads each artefact section the V1 REVISE named and confirms the
fold:

| V1 REVISE | V1 disposition | V2 state (verified) | Resolved? |
|---|---|---|---|
| αA §7 `:250-251` | `>974` stated as close threshold | `alphaA-...:314-324` now reads "The close threshold is the same-run re-baselined lightningcss full-CSSOM median measured in the SK-V17 Wave 0 re-baseline at N≥50 — NOT a frozen literal", and "~974 Mbps is a PRIOR-RUN REFERENCE only". The gate is `Track 1 median > same-run measured lightningcss full-CSSOM median, N≥50`. | **YES** |
| αB §2 `:96-102` | inferred per-corpus multiples liftable as measured | `alphaB-...:142-153` every per-corpus cell now carries `[INF]` / `[INF — confirm at N≥50]` INLINE (not only in a footnote); `:144-145` defines the marker legend; `:166-169` adds an UNMEASURED-PENDING clause "No SK-V17 wave exit-gate may key on an inferred per-corpus endpoint". §3 cssparser table carries the identical inline markers (`:184-187`). | **YES** |
| αD §3 `:79-82` | O-table estimates without falsifiable thresholds | `alphaD-...:136-140` adds an explicit "Falsifiable NO-GO thresholds (per α-E C1-C4, bound here so still-open candidates are not estimate-closed downstream)" block (O1 ≥30/<20, O3-NEON ≥80/<60, O4 ≥300/<200, tailwind cross-or-honest-residual), AND the per-row thresholds are inlined into O1/O3/O4 in the table itself (`:144,146,147`). | **YES** |
| HANDOFF gate posture `:75-77` | CH7 asserted mandatory without citing its authority | `HANDOFF.md:116-123` now reads "CH7 overfit-prune is a **pass-added monotonic extension lens** (the six-lens set CH1-CH6 is the orchestrator-citable canon; CH7 is added by this pass beyond it, not elevated to the mandatory set by §3W)" and states its scan scope explicitly (W5C retired-not-relocated, css_l4.toml→json.toml LOC trend, no per-rule-id match arms JSON does not need). This is verbatim correct against ORCHESTRATOR §3W:90-92 ("a pass that surfaces a failure mode the six lenses cannot disposition may add CH7+"). | **YES** |

All four V1 CH6 REVISEs are folded. Zero orphan REVISE from V1.

## Citability spot-check (every CH6 claim must be orchestrator-citable; uncited = reject)

CH6 re-verified the load-bearing citation anchors the goalset rests on, against the
working tree at HEAD `1c5bd7a25`:

- HEAD `1c5bd7a25` — `git rev-parse --short HEAD` confirmed.
- Core-tree symbols grep-clean-absent from `skinny/crates/`: `StructLayout`=0,
  `OpenFrame`=0, `CssArena`=0, `TapeStructBuilder`=0, `begin_compound`=0 (the
  benched-surface note's load-bearing claim; verified). A goalset gate keyed on
  these in `crates/core/` would be wrong-tree dishonesty — the artefacts correctly
  REJECT that (SYNTHESIS benched-surface note `:21-58`, telemetry `tape_activated`
  "NOT satisfiable by a grep in `crates/core/`" `:330`).
- `W5C_REQUEST_FACT_PROFILES` at `codegen/src/lib.rs:336` (declared), `:299`/`:567`
  (consumed) — the Lock-14 phrase-#1 retire target. Verified present.
- `digit_mac` udot orphan: `parse_4_digits_dotprod` at `digit_mac.rs:27`, `udot`
  asm at `:40`, dispatch call at `:12`. Verified — C4a's "wire the existing orphan"
  framing is real, not aspirational.
- `select_classifier` at `dispatch.rs:42`, `lo6_table_admissible` at `:101` — the
  C2 neutrality vehicle. Verified present.
- Skinny tape types: `PayloadArena` `mod.rs:38`, `Tape` `:94`, `ValueRef` `:175`,
  `TapeBuilder` `assembler.rs:42`. Verified at the cited lines.

Every CH6 disposition below is decided against live evidence; no claim is uncited.

## Deferral scan (the core CH6 mandate: no candidate deferred to "future wave")

`grep -rni "future wave will detail|will be detailed|TBD|to be determined|deferred
to a future|figure out later"` across `research/alpha/`, `SYNTHESIS.md`, `HANDOFF.md`
returns **zero hits**. The only deferrals present are the §4.4 wave-plan deferrals to
S-P3 (owner paths, entry/exit gates, per-wave hard caps, revert protocol), and EVERY
one of them is cited with the contract reference:
- SYNTHESIS `:380-386`: "authored downstream by skinny pass S-P3 ... per PASS-ALPHA §4.4".
- HANDOFF `:211-212`: "Revert protocol, hard caps, and per-wave triumvirate discipline
  are sanctioned-deferred to S-P3 (PASS-ALPHA §4.4 authority), not paper-closed here."
- αE §2/§4: the wave SEQUENCING (owner paths, per-wave hard caps) is handed to S-P3;
  the five candidates themselves are fully specified at Pass-Alpha altitude.

This is the legitimate boundary CH6 V1 established: Pass Alpha owns the measurable
goalset; S-P3 owns the wave revert protocol. The deferral is contract-sanctioned and
cited — NOT a paper-close.

---

## Disposition ledger

### αA — Results extraction (`alphaA-results-extraction.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing | ACCEPT | "0/24 admitted", ">SOTA bar UNMET", substrate "UNWIRED dead code". No close-claim. |
| §1 canonical bench | ACCEPT | N=100 canonical cited; single-sample inadequacy disclosed; harness cite verified. |
| §2 per-corpus | ACCEPT | "no SK-V16 per-corpus typed-CSS row to delta against" — honest absence. |
| §3 8-field equality | ACCEPT | EXACT counts cited; "re-prove EXACT before every admit" — gate-before-speed. |
| §4 20x checkpoint | ACCEPT | Distinguishes 20x (vs fragment) from 14.2x/15.6x direct; watermark-unsound divergence honest. |
| §5 sub-wave ledger | ACCEPT | Every row cites commit SHA + report:line; all REJECTED on gate; W6-tape "did nothing measurable / unwired dead code" verbatim. |
| §6 banked wins | ACCEPT | Provenance per win; V6 substrate citation CORRECTED to skinny tree with grep-clean-absence proof for the core-tree symbols. |
| §7 goalset seed | **ACCEPT** (was REVISE in V1) | `:314-324` now states the close threshold as "the same-run re-baselined lightningcss full-CSSOM median measured in the SK-V17 Wave 0 re-baseline at N≥50 — NOT a frozen literal"; "~974 Mbps is a PRIOR-RUN REFERENCE only". The V1 REVISE is folded verbatim. The 300-600 first-cross band is cited to the architecture synthesis, framed as expected-value not close-claim. |
| §8 citation ledger | ACCEPT | Every claim mapped to file:line / SHA; the `grep TapeStructBuilder skinny/ = EMPTY` anchor verified. |

αA: 9 ACCEPT, 0 REVISE, 0 REJECT.

### αB — Competitor deltas (`alphaB-competitor-deltas.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 plane taxonomy | ACCEPT | lightningcss = fair materializing bar; cssparser = flaw-probe. Plane-honest. |
| §1 baseline | ACCEPT | Per-run scatter (3.093 / 69.668 / 13-15) disclosed as statistical inadequacy, not architecture contradiction; canonical used for arithmetic, scatter not used. Wave-0 re-baseline mandate stated `:85-86`. |
| §2 per-corpus vs lightningcss | **ACCEPT** (was REVISE in V1) | `:142` "every inferred cell is marked `[INF]` inline"; `:144-145` marker legend; the table `:149-153` carries `[INF]` / `[INF — confirm at N≥50]` on every per-corpus endpoint AND every gap multiple; `:166-169` UNMEASURED-PENDING clause forbids any wave exit-gate keying on an inferred endpoint. The V1 paper-close risk (a downstream artefact lifting an inferred multiple as measured) is structurally closed — only the aggregate row is cited. |
| §3 vs cssparser | ACCEPT | Plane-mismatch disclosed; per-corpus rows carry the same `[INF]` inline markers (`:184-187`); "beating cssparser is NOT the SK-V17 win condition" `:177-178`. |
| §4 inter-comparator | ACCEPT | Materialization-tax framing cited as inter-comparator relation, "not inferred" `:217`; "reaching cssparser is ... arguably plane-incoherent" honest. |
| §5 JSON guard | ACCEPT | Carry-forward, cites RESULTS rows; Track1/Track2 independence (Lock 1) named. |
| §6 findings feed | ACCEPT | Finding 1 close-threshold = same-run median; Finding 2 forces the per-corpus split UNMEASURED-PENDING so wave gates key only on the aggregate crossing until N≥50 emits the split — self-correcting. |

αB: 7 ACCEPT, 0 REVISE, 0 REJECT.

### αC — REDRESS digest (`alphaC-redress-digest.md`)

CH6 re-reads αC as the strongest anti-paper-close artefact in the set (V1 carried it
10/0/0). Every pre-block carries a *measurable* re-open test, not a prose prohibition:
AZ-IV eager (per-leaf typed/f64 heap alloc → payload-arena counters), StructRegistry/Arena
(canada/bootstrap/tailwind + no-WATCHDOG), fact-stream String (push_str/fnv64 on hot
path + output-plane column), 24-row broadcast (N≥50 cold median per corpus), FNV/fixture
(quarantine gate consumer named), x86/AVX (Lock 16 manifest). The "typed=goal,
eager=carrier" line is the anti-paper-close invariant for every wave. No section
introduces a deferral or an uncited mandate.

αC: 10 ACCEPT, 0 REVISE, 0 REJECT.

### αD — Validated/invalidated ledger (`alphaD-validated-invalidated.md`)

| § | Disposition | Basis |
|---|---|---|
| §1 validated wins | ACCEPT | Each win cites commit SHA + measured evidence; V6 (tape) carries "UNWIRED" caveat verbatim; the no-StructRegistry guard is asserted *on the measured tree* (grep over `skinny/crates/` returns zero) — no false-wired claim. |
| §2 invalidated | ACCEPT | I1/I2/I3/I7 each refute a claim *by measurement* (the W6 self-refutation is the model anti-paper-close move; I3 — "summary margins do not transfer to the typed lane" — is the load-bearing honesty). I6 is a provenance correction, not a candidate. |
| §3 still-open O1-O5 | **ACCEPT** (was REVISE in V1) | `:136-140` now carries the explicit "Falsifiable NO-GO thresholds ... bound here so still-open candidates are not estimate-closed downstream" block; O1 ≥30/<20, O3-NEON ≥80/<60, O4 ≥300/<200, tailwind cross-or-honest-residual; AND the per-row thresholds are inlined into the O1/O3/O4 table cells. The V1 paper-close risk (Wave 1 declared "done" on the estimate rather than a measured gate) is closed — every open candidate now carries its αE-aligned falsifiable gate. O5's TOML-LOC convergence is an "explicit telemetry-bound exit gate" `:148`. |
| §4 demoted | ACCEPT | Pattern H folds into O5; FNV stays bench-only quarantine — honest. |

αD: 4 ACCEPT, 0 REVISE, 0 REJECT.

### αE — Candidate shortlist (`alphaE-candidate-shortlist.md`)

The artefact CH6 scrutinises hardest — where a candidate could be deferred to "future
wave will detail." It is not. The V2 changelog (`:12-34`) folds the V1 dispositions:
C4 SPLIT into C4a (unconditional orphan wiring) + C4b (GATED net-new i8mm), W5C named
on the retire-list, generality scoped to JSON+CSS with Sheets as an EXIT gate, fixture
count corrected 187→148.

| § | Disposition | Basis |
|---|---|---|
| §0 anchors | ACCEPT | Every anchor cites path:line / SHA (re-verified live). Statistical-adequacy precondition binds ALL gates to N≥50 (`:62-69`) with the lightningcss bar as the same-run re-baselined median (833.199 demoted to prior-run reference). The architecture-doc translation correction (`:71-95`) PRE-EMPTS a citability paper-close and is load-bearing. |
| C0 de-fact-stream | ACCEPT | Gate is a *measurable boolean + count*, NOT a deferral: "benched CSS Track 1 row exists, is TYPED, 8-field EXACT, `W5C_REQUEST_FACT_PROFILES` deleted (grep clean), AND throughput is measured (any value — this wave does not promise a lift)" `:138-144`. The "any value" is honest: it refuses to promise a speed it cannot guarantee. Same-wave consumer named `:135-137`. |
| C1 tape wiring + lazy cursor | ACCEPT | Gate ≥30 PASS / <20 NO-GO / fallback REJECT+REDRESS `:198-205`. Borrowed-slice-vs-lazy is an ENTRY GATE "not a wave step" `:208-213` — the W6 stall decision is forced forward, not deferred. Generality EXIT gate (emit `sheets_witness` view OR scope to JSON+CSS) `:187-197` is the anti-"by-construction" binding. No-relocated-overfit pruning test `:218-225`. |
| C2 NEON pre-scan | ACCEPT | Gate ≥80 PASS / <60 NO-GO + checkasm-fail NO-GO `:272-280`; scalar-ref PRESENT + checkasm PRECEDENTED with named existing tests `:247-262`; the ~56% hot-leaf % is tagged `S-P1-re-confirm-on-benched-path` (actual-profiling) `:229-234`; lo6-admissibility fallback to scalar is the honest answer, not a CSS special-case. Same-wave consumer `:263-267`. |
| C3 commit-by-construction | ACCEPT | Gate ≥300 PASS, > same-run lightningcss median plausible / <200 NO-GO / 150-200 PARTIAL `:324-332`. The ~31% own-compute % tagged core-tree S-P1-re-confirm. Risk class HIGH + "most likely to need a triumvirate" `:334-336` — names the triumvirate trigger. |
| C4a udot orphan | ACCEPT | Admits unconditionally (scalar-ref + checkasm satisfiable today); same-wave consumer retires the orphan `:358-360`; gate = byte-exact checkasm + routes on benched path, speed measured-not-promised `:361-365`. |
| C4b net-new i8mm | ACCEPT | The CH6 crux row, and it is correct: GATED behind a Wave-5 re-profile proving the digit leaf is top-N tailwind self-time — "If not proven, C4b is NOT dispatched (no net-new orphan kernel)" `:400-402`. PASS = tailwind crosses OR "an HONEST profiled residual ... per no-paper-close (CH6)"; "Fallback: report residual; do NOT fabricate a cross" `:403-411`. This is the canonical anti-paper-close clause, citing the discipline by name. |
| §2 dependency order | ACCEPT | Gates carried into the wave diagram with thresholds `:427-445`; no orphan promise. |
| §3 cross-cutting | ACCEPT | "No paper-close (CH6): C4b may close with an honest profiled residual ... it may NOT fabricate a cross" `:486-487` — CH6 discipline as a binding gate. Grammar-neutral witnessed-not-asserted `:470-480`. |
| §4 escalation | ACCEPT | If C0 cannot land typed benched Track 1, "the entire CSS goalset is UNMEASURABLE and Pass Alpha must escalate per §8 (`BLOCKED`)" `:491-493` — refuses to manufacture a measurable goalset where none exists. |

αE: 11 ACCEPT, 0 REVISE, 0 REJECT. Every candidate carries a numbered falsifiability
gate with a NO-GO threshold and a fallback. ZERO candidates are deferred to "future
wave will detail." Only the §4.4 wave SEQUENCING is handed to S-P3 — which PASS-ALPHA
§4.4 sanctions and αE cites.

### αF output — SYNTHESIS.md (the goalset; PASS-ALPHA §4.1-§4.3)

| § | Disposition | Basis |
|---|---|---|
| benched-surface note | ACCEPT | The wrong-tree dishonesty REJECT is structural: every tape/layout gate "verifiable by grepping `skinny/crates/`, not `crates/core/`" `:93-95`; the five core-tree symbols are grep-clean-absent (verified). This is the citability backbone of the entire goalset. |
| §0.1 close condition | ACCEPT | Every gate row resolves to a *measurable* test: tape activation = "a grep over those files returns non-zero" + "`PayloadArena` write/alloc counters confirm"; CSS equality = EXACT 8-field; >SOTA = "median Track 1 typed > median lightningcss full-CSSOM on same run (>1.0x), N≥50". The generality clause binds to a named exercised grammar or scopes the claim. No standalone paper-close row. |
| §0.2 starting state | ACCEPT | "LANDED, UNWIRED dead code (... zero parse-path callers)"; the lightningcss figure run-dependence (793/833/929/974, "No single committed measurement equals 974") is disclosed verbatim `:113-121` — no false-baseline. |
| §0.3 receiver goalset | ACCEPT | Each obligation concrete + names the deletion ("DELETE the hand-coded `W5C_REQUEST_FACT_PROFILES`"); "NO new cursor/builder type is introduced" stated. |
| §0.4 pre-blocks | ACCEPT | Verbatim CONTEXT pre-block + inherited REDRESS families + hidden-coupling escape list + the no-second-substrate clause + the witness-honest generality clause (JSON+CSS only, Sheets-or-scope). Binding, cited. |
| §0.5 per-corpus close | ACCEPT | THE anti-paper-close win: "the prior numbers (793/833/929/974 ...) are NOT the gate; the gate is the same-run measured lightningcss median" `:237-239`; "All per-corpus endpoints are UNMEASURED-PENDING: no wave exit-gate may key on an inferred per-corpus endpoint until the N≥50 harness emits the per-corpus split" `:244-247`. Tailwind "explicitly allowed to land short on first pass; record gap honestly in REDRESS; NOT a tranche-blocking failure" `:253`. Tranche success = ≥1 regular corpus crosses, else honest residual + WARN escalation `:256-261`. |
| §0.6 comparator gate | ACCEPT | lightningcss full-CSSOM = fair bar; cssparser = flaw-probe; W6 fact-stream comparator retired; Track 2 ≠ Track 1 anchor stated. |
| Section 1 ledger | ACCEPT | Validated/invalidated cite SHAs; A-series 454/735/496 cited as recognition-only, "explicitly NOT recovering the AZ-IV overfit". |
| Section 2 telemetry | ACCEPT | The §4.3 binding: gate "rejects any CSS row whose `sample_count < 50` or `sample_statistic != median`", full-CSSOM comparator-plane enforced, equality-before-speed boolean, `tape_activated` "NOT satisfiable by a grep in `crates/core/`", no-phantom-normalize, single-tuple-broadcast rejection (the W8R tripwire). Gate consumer command named `:341-342`. Telemetry-bound + bench-verifiable — CH6's core mandate MET. |
| Section 3 trajectory | ACCEPT | Four-lever route cited to architecture doc; close + WARN-escalation both stated; §4.4 wave plan deferred to S-P3 *with the contract citation*. |

SYNTHESIS: 11 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — HANDOFF.md

| § | Disposition | Basis |
|---|---|---|
| benched-substrate disclosure | ACCEPT | Core-tree symbols "grep-clean-absent from `skinny/crates/` (verified)"; benched substrate + fact-stream path cited. |
| Current state | ACCEPT | "LANDED BUT UNWIRED for CSS — zero CSS parse-path callers"; ">SOTA bar is NOT met and nothing on the CSS path moved"; lightningcss run-dependence ("no single committed number is 974") disclosed. No overclaim. |
| What SK-V17 opens | ACCEPT | "The gating artefact is the lazy-view accessor generator — it does not exist yet" — honest absence; the four-lever route carries the S-P1 re-profile obligation on the hot-leaf %. |
| Gate posture | **ACCEPT** (was REVISE in V1) | `:116-123` now frames CH7 as "a **pass-added monotonic extension lens** (the six-lens set CH1-CH6 is the orchestrator-citable canon; CH7 is added by this pass beyond it, not elevated to the mandatory set by §3W)" and states its scan scope explicitly. This is verbatim correct against ORCHESTRATOR §3W:90-92 ("a pass that surfaces a failure mode the six lenses cannot disposition may add CH7+"). The V1 uncited-mandate defect is folded — CH7 is now an authority-cited extension with stated scope, not a mandate elevated into §3W without citation. |
| Pre-blocked routes | ACCEPT | Verbatim, cites SYNTHESIS §0.4; the no-second-substrate clause stated. |
| Next move | ACCEPT | Steps 1-7 each measurable; step 7 close criterion = the §0.5 gate; step 1 carries the CH1-CH6 canon + CH7 extension; `tape_activated` "NOT by a grep returning non-zero in `crates/core/`" `:194-196`; escalation path named. Revert protocol sanctioned-deferred to S-P3 with §4.4 authority `:211-212`. |

HANDOFF: 6 ACCEPT, 0 REVISE, 0 REJECT.

---

## CH6 cross-cutting findings (V2)

**1. All four V1 CH6 REVISEs are folded; zero orphan REVISE carries into V2.**
αA §7 (same-run median, not `>974`), αB §2 (`[INF]` inline on every per-corpus cell),
αD §3 (αE-aligned falsifiable gates on the O-table), HANDOFF gate posture (CH7 as
§3W-cited extension lens) are each verified resolved against the live artefact text.

**2. Goalset is measurable + telemetry-bound — the central CH6 mandate is MET.**
N≥50 cold + median is bound at the gate level (Section 2: "rejects any CSS row whose
`sample_count < 50` or `sample_statistic != median`"). lightningcss is the materializing
comparator (full-CSSOM plane enforced; fact-stream comparator retired; same-run
re-baseline mandated — the prior fixed literals 793/833/929/974 are demoted to
references). Equality-before-speed is a boolean gate. Tape activation resolves to a grep
over `skinny/crates/` + `PayloadArena` write/alloc counters — explicitly NOT a grep in
`crates/core/` (wrong-tree dishonesty REJECTed). The gate consumer command is named and
bench-verifiable. No goalset row resolves to a prose promise; each resolves to a number,
a boolean, or a grep result.

**3. No candidate deferred to "future wave will detail."**
The deferral scan returns zero unsanctioned deferrals. All five αE candidates (C0-C4b)
are fully specified with falsifiability gates + NO-GO thresholds + fallbacks at
Pass-Alpha altitude. C4b's gating (lands ONLY if re-profile proves the digit leaf
top-N) is the model anti-orphan-kernel posture. The escalation note (αE §4) refuses to
manufacture a measurable goalset where C0 cannot land — the correct BLOCKED posture.

**4. Revert protocol / hard caps / per-wave triumvirate — contract-sanctioned deferral,
cited.** PASS-ALPHA §4.4 sanctions deferral of owner paths, entry/exit gates, per-wave
hard caps, revert protocol to S-P3. SYNTHESIS Section 3 and HANDOFF "Next move" both
cite this deferral *with the contract reference*. αE C3 names the triumvirate trigger.
This is the legitimate Pass-Alpha/S-P3 boundary, not a paper-close.

**5. No false-wired / false-complete self-report.** Every artefact carries "UNWIRED
dead code" / "does not exist yet" / "0/24 admitted" / "nothing on the CSS path moved"
verbatim. The substrate is never claimed live; the lazy-view generator never claimed
built. The W6 self-refutation (lever-1 did not move throughput; I3 summary margins do
not transfer) is reproduced honestly across αA/αD. The no-StructRegistry guard is
asserted on the *measured* tree, not the design intent.

**6. Every CH6 claim orchestrator-citable.** The six load-bearing citation anchors
(HEAD SHA, core-tree-absence grep, W5C array, digit_mac orphan, select_classifier/lo6,
skinny tape types) were re-verified live at HEAD `1c5bd7a25`. The artefacts' citability
is sound — no claim rests on an uncited number or a wrong-tree path.

---

## Counts

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| αA results-extraction | 9 | 0 | 0 |
| αB competitor-deltas | 7 | 0 | 0 |
| αC redress-digest | 10 | 0 | 0 |
| αD validated-invalidated | 4 | 0 | 0 |
| αE candidate-shortlist | 11 | 0 | 0 |
| SYNTHESIS (αF) | 11 | 0 | 0 |
| HANDOFF (αF) | 6 | 0 | 0 |
| **Total** | **58** | **0** | **0** |

ACCEPT rate: 58 / 58 = **100%**. Above the §3Z 95% bar. Zero REVISE, zero REJECT, zero
orphan REVISE. All four V1 CH6 REVISEs are folded and verified against live artefact
text. The goalset is measurable, telemetry-bound (N≥50 cold median + full-CSSOM
lightningcss comparator + equality-before-speed boolean + grep-verifiable tape
activation), and every claim is orchestrator-citable. No candidate is deferred to a
future wave; the only deferral (the §4.4 wave revert protocol) is contract-sanctioned
and cited. CH6 ANTI-PAPER-CLOSE: PASS for cycle V2.

CH6 V2 is the second of the two consecutive ≥95% cycles ORCHESTRATOR §3Z requires
(V1 92.9% → V2 100%); the V2 rate clears the bar and the V1→V2 fold leaves zero orphan
REVISE. The convergence determination across the full CH1-CH6(+CH7) set is the
CONSOLIDATED author's call; CH6's own contribution converges at V2.
