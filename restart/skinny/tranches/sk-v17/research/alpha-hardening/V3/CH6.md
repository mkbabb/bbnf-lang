# CH6 — ANTI-PAPER-CLOSE — Pass Alpha SK-V17 cycle V3

Lens: CH6 ANTI-PAPER-CLOSE (ORCHESTRATOR §3W; PASS-ALPHA §3 CH6 = Next-Tranche-Impact).
Cycle: V3 (folds V1 + V2 CHALLENGE dispositions). Subject: SK-V17 Pass Alpha artefacts
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + the αF output
(`sk-v17/SYNTHESIS.md` + `sk-v17/HANDOFF.md`, per PASS-ALPHA §6 — αF writes to the
tranche root, not an `alphaF.md`; verified absent and correctly so).
Host: aarch64 Apple M5 Max. HEAD `1c5bd7a25` (`git rev-parse --short HEAD` confirmed live).

CH6 focus (this dispatch): no candidate deferred to "future wave will detail"; goalset
measurable + telemetry-bound (N≥50 cold sampling, lightningcss materializing comparator);
every claim orchestrator-citable; revert protocol / hard caps / triumvirate discipline
present where this pass owns them; no self-report of "complete"/"wired"/"verified"
standing without orchestrator-citable live evidence.

The load-bearing CH6 line (unchanged from V1/V2): PASS-ALPHA §4.4 **explicitly defers**
the wave-by-wave revert protocol, owner paths, entry/exit gates, and per-wave hard caps
to skinny pass S-P3 (`sk-v17/SPEC.md`). A deferral the contract sanctions is NOT a
paper-close. A deferral the contract does NOT sanction — a candidate whose *measurability*
is pushed to a future phase, a gate that resolves to a promise rather than a number, a
self-report of a landed artefact, or a gate keyed on a surface the canonical contract has
demoted — IS a paper-close (or its citability cousin) and CH6 rejects/revises it.
ORCHESTRATOR §3W (CH6 row: "No agent self-report of complete/wired/verified stands
without orchestrator-cited live evidence. No deferral to a future phase.") is the binding text.

## V2 → V3 fold verification (the five V3-mandated reconciliations)

V2 CH6 returned 58 ACCEPT / 0 REVISE / 0 REJECT (100%), the second of the two consecutive
≥95% cycles §3Z requires. V3 carries five named reconciliations (SYNTHESIS `:5-10`):
the V2 CH2-V2-F1 (b′) sheets_witness-non-dischargeable repair; the 6-vs-24 broadcast-count
reconciliation (CH1-R1); the `nonjson_css_l4.rs:776` definition citation (CH1-R2); the
`regen_css.rs:45-153` seam-flip site; and the `css_l4.toml`-is-totality SK-V18-fold
demotion. CH6 re-verifies each against the live working tree at HEAD `1c5bd7a25`:

| V3 reconciliation | Live-verified state | Folded? |
|---|---|---|
| 6→24 broadcast count | `grep -c '^| css_l4/.*/direct_to_struct/main ' skinny/RESULTS.md` = **24** (live). SYNTHESIS `:139-157`, HANDOFF `:48-53`, alphaC §4 `:215-233`/`:350`, alphaD `:112,:128-131,:183` all read 24; the prior "6" undercount is corrected with the grep cited. The substantive conclusion (zero ADMITTED typed CSS rows; broadcast pre-blocked) is unchanged. | **YES** |
| `nonjson_css_l4.rs:776` assert def citation | `grep -n assert_lightningcss_strict_equality skinny/crates/bbnf-bench/src/nonjson_css_l4.rs` = def `:776`, call sites `:1057`/`:3460` (live). SYNTHESIS `:110` cites all three + the harness `benches/nonjson_css_l4.rs:8`. Exact. | **YES** |
| `regen_css.rs:45-153` seam-flip site | `grep -n 'RuntimeEmitterKind::RequestFacts' skinny/xtask/src/regen_css.rs` = seven literals at `:45,63,81,99,117,135,153` (live); `regen_css` fn `:164` (live). SYNTHESIS `:172` + HANDOFF Next-Move name them as the concrete flip site. Exact. | **YES** |
| `css_l4.toml`-is-totality SK-V18 demotion | `find skinny -name css_l4.toml` = **0**; lives only at totality root `./xtask/runtime-projections/css_l4.toml`. SYNTHESIS §0.1 `:105` demotes TOML-LOC convergence to "INFORMATIONAL only, NOT an SK-V17 close gate"; HANDOFF gate posture `:139-142` "NOT a CH7 scan gate... noted not gated"; alphaC `:34` "does NOT exist in skinny — a core-tree artefact". Three of four surfaces reconciled. **alphaD O5 `:154` is NOT reconciled** (see F1). | **PARTIAL → F1 REVISE** |
| sheets_witness non-dischargeable (CH2-V2-F1 b′) | `sheets_witness` codegen fail-closed: `lib.rs:1075-1090` (verified present). SYNTHESIS §0.4 generality clause `:234-257`, telemetry `projection_generality_exercise` "`sheets_witness` is NOT a valid value here" `:365`, alphaE `:22-25,:223-227`, alphaD O2 `:151`, alphaD `:55-65` all carry the "no `.bbnf` / no parser / no `BackendRule` to walk → structurally non-dischargeable" repair. | **YES** |

Four of five V3 reconciliations are fully folded. The fifth (the TOML demotion) is folded
in the three downstream-canonical surfaces (SYNTHESIS / HANDOFF / alphaC) but a residual
contradicting label survives in alphaD O5 — the single CH6 V3 REVISE (F1 below).

## Citability spot-check (every CH6 claim orchestrator-citable; uncited = reject)

CH6 re-verified the load-bearing citation anchors live at HEAD `1c5bd7a25`:

- HEAD `1c5bd7a25` — `git rev-parse --short HEAD` confirmed.
- Core-tree symbols grep-clean-absent from `skinny/crates/`: `StructLayout`=0, `OpenFrame`=0,
  `CssArena`=0, `TapeStructBuilder`=0, `begin_compound`=0 (verified). The wrong-tree
  dishonesty REJECT (SYNTHESIS benched-surface note `:25-62`, telemetry `tape_activated`
  "NOT satisfiable by a grep in `crates/core/`" `:363`) rests on this.
- `W5C_REQUEST_FACT_PROFILES` `codegen/src/lib.rs:336` (declared), `:299` (selected),
  `:567`/`:611` (iterated) — the Lock-14 phrase-#1 retire target. Verified.
- `digit_mac` udot orphan: `parse_4_digits_dotprod` `:27`, `udot` asm `:40`, dispatch call
  `:12`. Verified — C4a's "wire the existing orphan" framing is real, not aspirational.
- `select_classifier` `dispatch.rs:42`, `lo6_table_admissible` `:101`, `PrimitiveKernels`
  `:50` — the C2 neutrality vehicle. Verified.
- Skinny tape types: `PayloadArena` `mod.rs:38`, `Tape` `:94`, `ValueRef` `:175`,
  `DocumentView` trait `:227`; `TapeBuilder` `assembler.rs:42`, `push_plain_offset` `:71`.
  Verified at the cited lines.
- `W6_SAMPLE_COUNT=1` is grep-clean-absent from `skinny/` source and lives in the audit
  reports (`sk-v16-w5-w6-build-report.md:79`, `sk-v16-w6p1-dimension-dispatch-report.md:13`);
  the contract attributes it to the W6 harness correctly, not to a skinny src construct.
- CSS benched corpus set `css_l4_corpus.rs:22-54` = `{bootstrap, tailwindcss,
  material-components-web, animate}`; `normalize` grep-clean-absent. Verified — every
  per-corpus gate names only benched corpora.

Every CH6 disposition below is decided against live evidence; no claim is uncited.

## Deferral scan (the core CH6 mandate: no candidate deferred to "future wave")

`grep -rniE "future wave will detail|will be detailed|TBD|to be determined|deferred to a
future|figure out later|will detail later"` across `research/alpha/`, `SYNTHESIS.md`,
`HANDOFF.md` returns **zero hits**. The only deferrals present are (a) the §4.4 wave-plan
deferrals to S-P3 (owner paths, entry/exit gates, per-wave hard caps, revert protocol),
each cited with the contract reference (SYNTHESIS `:414-420`; HANDOFF `:231-232`); and
(b) the Sheets/BBNF-self projection-generality proof deferred to SK-V18, which is the
correct boundary (no `.bbnf` shape exists for `sheets_witness` to walk — non-dischargeable,
not paper-closed). Both are contract-sanctioned and cited. No candidate's *measurability*
is pushed to a future phase.

---

## Disposition ledger

### αA — Results extraction (`alphaA-results-extraction.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing | ACCEPT | "0/24 admitted", ">SOTA bar UNMET", substrate "UNWIRED dead code". No close-claim. |
| §1 canonical bench | ACCEPT | N=100 canonical cited; close gate keyed to same-run re-baselined lightningcss median (`:96-101`), NOT a frozen literal; single-sample inadequacy disclosed. |
| §2 per-corpus | ACCEPT | "no SK-V16 per-corpus typed-CSS row to delta against" — honest absence. |
| §3 8-field equality | ACCEPT | EXACT counts cited; re-prove-before-speed stated. |
| §4 20x checkpoint | ACCEPT | 20x vs fragment distinguished from 14.2x/15.6x direct; watermark-unsound divergence honest. |
| §5 sub-wave ledger | ACCEPT | Every row cites SHA + report:line; W6-tape "unwired dead code" verbatim. |
| §6 banked wins | ACCEPT | V6 substrate citation corrected to skinny tree with core-tree-absence proof. |
| §7 goalset seed | ACCEPT (held from V2) | `:319-327` close threshold = "same-run re-baselined lightningcss full-CSSOM median... NOT a frozen literal"; "~974 Mbps is a PRIOR-RUN REFERENCE only". V1 REVISE remains folded; V3 introduces no regression. |
| §8 citation ledger | ACCEPT | Every claim mapped to file:line / SHA; `grep TapeStructBuilder skinny/ = EMPTY` anchor verified. |

αA: 9 ACCEPT, 0 REVISE, 0 REJECT.

### αB — Competitor deltas (`alphaB-competitor-deltas.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 plane taxonomy | ACCEPT | lightningcss = fair materializing bar; cssparser = flaw-probe. Plane-honest. |
| §1 baseline | ACCEPT | Per-run scatter disclosed as statistical inadequacy; Wave-0 re-baseline mandate stated. |
| §2 per-corpus vs lightningcss | ACCEPT (held from V2) | 19 `[INF]` markers grep-counted; `:153-154` "every inferred cell marked `[INF]` inline"; `:178-181` UNMEASURED-PENDING clause "No SK-V17 wave exit-gate may key on an inferred per-corpus endpoint". The paper-close risk (downstream lifting an inferred multiple as measured) is structurally closed. |
| §3 vs cssparser | ACCEPT | Plane-mismatch disclosed; per-corpus rows carry `[INF]` inline; "beating cssparser is NOT the win condition". |
| §4 inter-comparator | ACCEPT | Materialization-tax framing cited as inter-comparator relation "not inferred" `:229`. |
| §5 JSON guard | ACCEPT | Carry-forward, cites RESULTS rows; Track1/Track2 independence (Lock 1) named. |
| §6 findings feed | ACCEPT | Close-threshold = same-run median; per-corpus split UNMEASURED-PENDING so wave gates key only on the aggregate crossing until N≥50 emits the split — self-correcting. |

αB: 7 ACCEPT, 0 REVISE, 0 REJECT.

### αC — REDRESS digest (`alphaC-redress-digest.md`)

CH6 re-reads αC as the strongest anti-paper-close artefact in the set (V1/V2 carried it
10/0/0). Every pre-block carries a *measurable* re-open test, not a prose prohibition. The
V3 broadcast-count reconciliation lands cleanly: §4 `:215-233` and the §7 table row 4
`:350` both read 24 with `grep -c` cited and `css_l4_w8.rs:206-228`/`W8_SELECTED_CSS_ROWS=24`
as the falsified source. The `css_l4.toml` handling is exemplary: `:34` "does NOT exist in
skinny — a core-tree artefact", framing skinny's overfit fingerprint as the greppable
`W5C_REQUEST_FACT_PROFILES` + 7 `RequestFacts` registrations + 148 fixture parse fns — no
TOML-LOC gate claim. No section introduces a deferral or an uncited mandate.

αC: 10 ACCEPT, 0 REVISE, 0 REJECT.

### αD — Validated/invalidated ledger (`alphaD-validated-invalidated.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing / generality | ACCEPT | `:55-65` sheets_witness "structurally non-dischargeable (it has no shape to lower)"; "only exercised projection riders are JSON + CSS"; SK-V18 fold target named. The V2 CH2-V2-F1 (b′) repair is folded. |
| §1 validated wins | ACCEPT | Each win cites SHA + measured evidence; V6 (tape) carries "UNWIRED dead code" verbatim; no-StructRegistry guard asserted *on the measured tree* (grep over `skinny/crates/` = 0). |
| §2 invalidated | ACCEPT | I1/I2/I3/I7 each refute by measurement; I3 "summary margins do not transfer to the typed lane" is the load-bearing honesty. |
| §3 still-open O1-O4 | ACCEPT (held from V2) | `:142-146` "Falsifiable NO-GO thresholds... bound here so still-open candidates are not estimate-closed downstream": O1 ≥30/<20, O3-NEON ≥80/<60, O4 ≥300/<200, tailwind cross-or-honest-residual; inlined into the table cells `:150,152,153`. O2 sheets_witness non-dischargeable repair present `:151`. The estimate-close paper-close risk is shut. |
| §3 still-open O5 (TOML-LOC label) | **REVISE (F1)** | `:154` labels "**TOML-LOC convergence is an explicit telemetry-bound exit gate**" and lists the 594-line `css_l4.toml` in the "Retire-list... must be deleted (TOTALITY fold)". The *operationalized* test that follows is skinny-greppable and sound (`.bbnf`-rule derivation + CSS regen profile array trending toward JSON), BUT the LABEL contradicts the downstream-canonical V3 demotion: SYNTHESIS §0.1 `:105` ("its LOC convergence is an SK-V18 totality-fold metric, INFORMATIONAL only, NOT an SK-V17 close gate; gating an SK-V17 close on a non-benched totality file would be the wrong-tree dishonesty this contract REJECTs") and HANDOFF gate posture `:139-142` ("the `css_l4.toml` LOC convergence is NOT a CH7 scan gate... noted not gated"). alphaC `:34` and alphaD's own O5 prose ("a TOTALITY-tree artifact... the fold target, not a skinny owner path") already agree it is non-benched. The residual "explicit telemetry-bound exit gate" label, attached to a surface SYNTHESIS demotes, is a wrong-tree-gate paper-close cousin: S-P3 reading alphaD O5 in isolation could lift the totality TOML-LOC count as a gate. This is precisely the fifth V3 reconciliation ("the `css_l4.toml`-is-totality SK-V18-fold demotion") — it landed in SYNTHESIS/HANDOFF/alphaC but the alphaD O5 label was not updated to match. **Concrete fix (`alphaD-validated-invalidated.md:154`):** relabel the sentence to name the gate for what it actually tests — e.g. "**The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count:** every residual CSS routing entry must name the `.bbnf` rule it derives from, and the CSS regen profile array (`regen_css.rs:45-153`) must trend toward the JSON emitter shape. The 594-line `css_l4.toml` LOC convergence is a TOTALITY-tree metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (gating on a non-benched totality file is wrong-tree dishonesty — SYNTHESIS §0.1)." This brings alphaD O5 into verbatim agreement with the three reconciled surfaces. The substantive content (W5C retire + grammar-derived routing as the real gate) is correct and unchanged; only the totality-file label is demoted. |
| §4 demoted | ACCEPT | Pattern H folds into O5; FNV stays bench-only quarantine. |

αD: 5 ACCEPT, 1 REVISE, 0 REJECT.

### αE — Candidate shortlist (`alphaE-candidate-shortlist.md`)

The artefact CH6 scrutinises hardest — where a candidate could be deferred to "future wave
will detail." It is not. Every candidate carries a numbered falsifiability gate + NO-GO
threshold + fallback at Pass-Alpha altitude. The V3 sheets_witness repair is folded:
`:22-25` and `:223-227` mark `sheets_witness` "NOT a dischargeable projection target in
SK-V17... has no runnable projection... codegen fail-closed negative control
(lib.rs:1075-1090)", and the EXIT-gate disjunct `:470` reads "view emitter walks one
BackendRule shape for JSON+CSS (sheets = SK-V18)".

| § | Disposition | Basis |
|---|---|---|
| §0 anchors | ACCEPT | Every anchor cites path:line / SHA (re-verified live). N≥50 statistical-adequacy precondition binds all gates; lightningcss bar = same-run re-baselined median. |
| C0 de-fact-stream | ACCEPT | Gate = measurable boolean + count + "throughput measured (any value — this wave does not promise a lift)"; same-wave consumer named. The "any value" is honest. |
| C1 tape wiring + lazy cursor | ACCEPT | Gate ≥30 PASS / <20 NO-GO / fallback REJECT+REDRESS; borrowed-slice-vs-lazy forced as an entry gate, not deferred; sheets_witness EXIT disjunct re-scoped to SK-V18 (V3 repair). |
| C2 NEON pre-scan | ACCEPT | Gate ≥80 PASS / <60 NO-GO + checkasm-fail NO-GO; scalar-ref PRESENT + checkasm PRECEDENTED; ~56% hot-leaf % tagged `S-P1-re-confirm-on-benched-path` (actual-profiling); same-wave consumer. |
| C3 commit-by-construction | ACCEPT | Gate ≥300 PASS / >same-run lightningcss cross plausible / <200 NO-GO / 150-200 PARTIAL; HIGH risk + triumvirate trigger named. |
| C4a udot orphan | ACCEPT | Admits unconditionally (scalar-ref + checkasm satisfiable today); same-wave consumer retires the orphan; speed measured-not-promised. |
| C4b net-new i8mm | ACCEPT | The CH6 crux row: GATED behind a Wave-5 re-profile proving the digit leaf is top-N tailwind self-time — "If not proven, C4b is NOT dispatched (no net-new orphan kernel)"; PASS = tailwind crosses OR "an HONEST profiled residual... per no-paper-close (CH6)"; "Fallback: report residual; do NOT fabricate a cross." The canonical anti-paper-close clause, citing the discipline by name. |
| §2 dependency order | ACCEPT | Gates carried into the wave diagram with thresholds; no orphan promise. |
| §3 cross-cutting | ACCEPT | "No paper-close (CH6): C4b may close with an honest profiled residual... it may NOT fabricate a cross." Grammar-neutral witnessed-not-asserted. |
| §4 escalation | ACCEPT | If C0 cannot land typed benched Track 1, "the entire CSS goalset is UNMEASURABLE and Pass Alpha must escalate per §8 (`BLOCKED`)" — refuses to manufacture a measurable goalset where none exists. |

αE: 11 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — SYNTHESIS.md (the goalset; PASS-ALPHA §4.1-§4.3)

| § | Disposition | Basis |
|---|---|---|
| benched-surface note | ACCEPT | The wrong-tree dishonesty REJECT is structural: five core-tree symbols grep-clean-absent (verified); every tape/layout gate "verifiable by grepping `skinny/crates/`, not `crates/core/`". The seam-flip site (`regen_css.rs:45-153`, 7 `RequestFacts` literals) named `:50-52,:172` — verified live. |
| §0.1 close condition | ACCEPT | Every gate resolves to a measurable test (tape activation = grep non-zero + `PayloadArena` write/alloc counters; CSS equality = EXACT 8-field; >SOTA = median Track 1 > median lightningcss N≥50). `:105` carries the V3 `css_l4.toml` demotion verbatim ("INFORMATIONAL only, NOT an SK-V17 close gate"). |
| §0.2 starting state | ACCEPT | "LANDED, UNWIRED" + zero parse-path callers; lightningcss run-dependence disclosed ("No single committed measurement equals 974"); broadcast count 24 with grep cited `:139-157`. |
| §0.3 receiver goalset | ACCEPT | Each obligation concrete; names the deletion ("DELETE `W5C_REQUEST_FACT_PROFILES`"); the seam-flip seven literals + `regen_css` fn `:164` named; "NO new cursor/builder type". |
| §0.4 pre-blocks | ACCEPT | Verbatim CONTEXT pre-block + inherited REDRESS families + hidden-coupling escape list + no-second-substrate clause + the witness-honest generality clause (JSON+CSS only; sheets_witness non-dischargeable, V3 repair). Binding, cited. |
| §0.5 per-corpus close | ACCEPT | "the prior numbers (793/833/929/974) are NOT the gate; the gate is the same-run measured lightningcss median"; "All per-corpus endpoints are UNMEASURED-PENDING"; tailwind "explicitly allowed to land short... record gap honestly in REDRESS... NOT a tranche-blocking failure"; success = ≥1 regular corpus crosses else WARN escalation. |
| §0.6 comparator gate | ACCEPT | lightningcss full-CSSOM = fair bar; cssparser = flaw-probe; W6 fact-stream comparator (`assert_lightningcss_strict_equality:776`) retired; Track 2 ≠ Track 1 anchor. |
| Section 1 ledger | ACCEPT | A-series 454/735/496 cited as recognition-only, "explicitly NOT recovering the AZ-IV overfit". |
| Section 2 telemetry | ACCEPT | Gate "rejects any CSS row whose `sample_count < 50` or `sample_statistic != median`"; full-CSSOM comparator-plane enforced; equality-before-speed boolean; `tape_activated` "NOT satisfiable by a grep in `crates/core/`"; no-phantom-normalize; single-tuple-broadcast rejection (W8R tripwire). Gate consumer command named `:376`. Telemetry-bound + bench-verifiable — CH6's core mandate MET. |
| Section 3 trajectory | ACCEPT | Four-lever route cited to architecture doc; close + WARN-escalation stated; §4.4 wave plan deferred to S-P3 *with the contract citation* `:414-420`. |

SYNTHESIS: 11 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — HANDOFF.md

| § | Disposition | Basis |
|---|---|---|
| benched-substrate disclosure | ACCEPT | Core-tree symbols "grep-clean-absent from `skinny/crates/` (verified)"; benched substrate + fact-stream path cited at the verified lines. |
| Current state | ACCEPT | "LANDED BUT UNWIRED for CSS — zero CSS parse-path callers"; ">SOTA bar is NOT met and nothing on the CSS path moved"; lightningcss run-dependence disclosed; broadcast count 24 with `grep -c` cited `:48-53`. No overclaim. |
| What SK-V17 opens | ACCEPT | "The gating artefact is the lazy-view accessor generator — it does not exist yet"; four-lever route carries the S-P1 re-profile obligation; generality scope JSON+CSS with sheets_witness non-dischargeable (V3 repair) `:86-96`. |
| Gate posture | ACCEPT (held from V2) | `:127-142` CH7 framed as "a **pass-added monotonic extension lens** (the six-lens set CH1-CH6 is the orchestrator-citable canon)" — verbatim correct against §3W ("monotonically extensible... may add CH7+"). The `css_l4.toml` LOC convergence "NOT a CH7 scan gate... noted not gated" `:139-142` — the V3 demotion, correctly applied here. |
| Pre-blocked routes | ACCEPT | Verbatim, cites SYNTHESIS §0.4; no-second-substrate clause stated. |
| Next move | ACCEPT | Steps 1-7 each measurable; step 7 close criterion = the §0.5 gate; `tape_activated` "NOT by a grep returning non-zero in `crates/core/`"; escalation path named; revert protocol sanctioned-deferred to S-P3 with §4.4 authority `:231-232`. |

HANDOFF: 6 ACCEPT, 0 REVISE, 0 REJECT.

---

## CH6 cross-cutting findings (V3)

**1. Four of five V3 reconciliations are fully folded; one (the TOML demotion) is folded
in three of four surfaces.** Broadcast 6→24 (grep-verified 24 across SYNTHESIS/HANDOFF/
alphaC/alphaD), the `:776` assert citation, the `regen_css.rs:45-153` seam-flip site, and
the sheets_witness non-dischargeable repair are each verified resolved against live text.
The `css_l4.toml`-is-totality demotion landed in SYNTHESIS §0.1, HANDOFF gate posture, and
alphaC — but a contradicting "explicit telemetry-bound exit gate" label survives in alphaD
O5 `:154` (F1 REVISE). This is an orphan from the V3 reconciliation, not a new defect.

**2. Goalset is measurable + telemetry-bound — the central CH6 mandate is MET.** N≥50 cold
+ median bound at the gate level (Section 2: "rejects any CSS row whose `sample_count < 50`
or `sample_statistic != median`"). lightningcss is the materializing comparator (full-CSSOM
plane enforced; the `assert_lightningcss_strict_equality:776` fact-stream comparator
retired; same-run re-baseline mandated — the fixed literals 793/833/929/974 demoted to
references). Equality-before-speed is a boolean gate. Tape activation resolves to a grep
over `skinny/crates/` + `PayloadArena` write/alloc counters — explicitly NOT a grep in
`crates/core/`. No goalset row resolves to a prose promise.

**3. No candidate deferred to "future wave will detail."** The deferral scan returns zero
unsanctioned deferrals. All five αE candidates (C0-C4b) are fully specified with
falsifiability gates + NO-GO thresholds + fallbacks. C4b's gating (lands ONLY if re-profile
proves the digit leaf top-N) is the model anti-orphan-kernel posture. The escalation note
(αE §4) refuses to manufacture a measurable goalset where C0 cannot land.

**4. Revert protocol / hard caps / per-wave triumvirate — contract-sanctioned deferral,
cited.** PASS-ALPHA §4.4 sanctions deferral to S-P3. SYNTHESIS Section 3 and HANDOFF Next
Move both cite this *with the contract reference*. αE C3 names the triumvirate trigger.
Legitimate Pass-Alpha/S-P3 boundary, not a paper-close.

**5. No false-wired / false-complete self-report.** Every artefact carries "UNWIRED dead
code" / "does not exist yet" / "0/24 admitted" / "nothing on the CSS path moved" verbatim.
The W6 self-refutation (lever-1 did not move throughput; I3 summary margins do not transfer)
is reproduced honestly across αA/αD. The no-StructRegistry guard is asserted on the
*measured* tree, not the design intent.

**6. The one V3 REVISE is a citability cousin, not a measurability failure.** alphaD O5's
*operationalized* gate (`.bbnf`-derivation + regen-array trend) is skinny-greppable and
sound; only the LABEL "TOML-LOC convergence is an explicit telemetry-bound exit gate" —
attached to a totality file the canonical contract demotes — is the defect. The fix is a
relabel, not a content change. Because SYNTHESIS §0.1 and HANDOFF (the surfaces S-P3
consumes as canonical) already carry the demotion, the downstream risk is bounded; but
alphaD is an antecedent artefact S-P3 may read, so the contradiction must be reconciled to
avoid a wrong-tree-gate lift. This is an orphan REVISE under §3Z and blocks CH6's own
≥95%-x2 convergence determination until folded.

---

## Counts

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| αA results-extraction | 9 | 0 | 0 |
| αB competitor-deltas | 7 | 0 | 0 |
| αC redress-digest | 10 | 0 | 0 |
| αD validated-invalidated | 5 | 1 | 0 |
| αE candidate-shortlist | 11 | 0 | 0 |
| SYNTHESIS (αF) | 11 | 0 | 0 |
| HANDOFF (αF) | 6 | 0 | 0 |
| **Total** | **59** | **1** | **0** |

ACCEPT rate: 59 / 60 = **98.3%**. Above the §3Z 95% bar. One REVISE (F1: alphaD O5
`:154` — relabel the "TOML-LOC convergence exit gate" to the skinny-greppable
grammar-derivation gate + demote the totality TOML-LOC count to INFORMATIONAL per SYNTHESIS
§0.1). Zero REJECT.

The goalset is measurable, telemetry-bound (N≥50 cold median + full-CSSOM lightningcss
comparator + equality-before-speed boolean + grep-verifiable tape activation), and every
claim is orchestrator-citable against live evidence at HEAD `1c5bd7a25`. No candidate is
deferred to a future wave; the only deferrals (the §4.4 wave revert protocol and the
SK-V18 Sheets/BBNF generality proof) are contract-sanctioned and cited. CH6 ANTI-PAPER-CLOSE:
PASS-with-one-fold for cycle V3.

Convergence note (§3Z): V2 was the second of the prior two consecutive ≥95% cycles, but
V3's single REVISE (F1) is an orphan REVISE that must fold into V4 before CH6's contribution
re-converges. F1 is a surgical relabel confined to one cell (`alphaD:154`) with the exact
replacement text supplied; it introduces no new measurement and no new candidate. The
cross-lens convergence determination is the CONSOLIDATED author's call; CH6's own
contribution returns 98.3% ACCEPT with one foldable orphan REVISE.
