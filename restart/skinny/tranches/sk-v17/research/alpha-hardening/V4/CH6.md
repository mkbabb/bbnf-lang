# CH6 — ANTI-PAPER-CLOSE — Pass Alpha SK-V17 cycle V4

Lens: CH6 ANTI-PAPER-CLOSE (ORCHESTRATOR §3W; PASS-ALPHA §3 CH6 = Next-Tranche-Impact).
Cycle: V4 (folds V1 + V2 + V3 CHALLENGE dispositions). Subject: SK-V17 Pass Alpha artefacts
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` + the αF output
(`sk-v17/SYNTHESIS.md` + `sk-v17/HANDOFF.md`, per PASS-ALPHA §6 — αF writes to the
tranche root, not an `alphaF.md`; verified absent and correctly so).
Host: aarch64 Apple M5 Max. HEAD `1c5bd7a25` (`git rev-parse --short HEAD` confirmed live).

CH6 focus (this dispatch): no candidate deferred to "future wave will detail"; goalset
measurable + telemetry-bound (N≥50 cold sampling, lightningcss materializing comparator);
every claim orchestrator-citable; revert protocol / hard caps / triumvirate discipline
present where this pass owns them; no self-report of "complete"/"wired"/"verified"
standing without orchestrator-citable live evidence.

The load-bearing CH6 line (unchanged from V1/V2/V3): PASS-ALPHA §4.4 **explicitly defers**
the wave-by-wave revert protocol, owner paths, entry/exit gates, and per-wave hard caps
to skinny pass S-P3 (`sk-v17/SPEC.md`). A deferral the contract sanctions is NOT a
paper-close. A deferral the contract does NOT sanction — a candidate whose *measurability*
is pushed to a future phase, a gate that resolves to a promise rather than a number, a
self-report of a landed artefact, or a gate keyed on a surface the canonical contract has
demoted — IS a paper-close (or its citability cousin) and CH6 rejects/revises it.
ORCHESTRATOR §3W (CH6 row: "No agent self-report of complete/wired/verified stands
without orchestrator-cited live evidence. No deferral to a future phase.") is the binding text.

## V3 → V4 fold verification (the single V3 orphan REVISE + two count-correction folds)

V3 CH6 returned 59 ACCEPT / 1 REVISE / 0 REJECT (98.3%), with a single orphan REVISE (F1:
alphaD O5 `:154` — the "TOML-LOC convergence is an explicit telemetry-bound exit gate"
label, contradicting the SYNTHESIS §0.1 / HANDOFF / alphaC demotion of `css_l4.toml` to a
non-benched totality artefact). Per §3Z the orphan must fold into V4 before CH6 re-converges.
SYNTHESIS `:5-15` + HANDOFF `:5-11` declare three V4 folds: (a) F1 (alphaD:154 relabel);
(b) V3-CH1-a stale meta-note rewrite ("all cohort artefacts state 24 as of V3"); (c)
V3-CH1-b grep-substring mislabel ("25 substring matches, of which 24 are `^| css_l4/` table
rows and the 25th :154 is a prose REDRESS-127 companion reference, not a row"). CH6
re-verifies each against the live working tree at HEAD `1c5bd7a25`:

| V4 fold | Live-verified state | Folded? |
|---|---|---|
| F1 — alphaD O5 `:154` TOML-LOC relabel | `grep -n 'explicit telemetry-bound exit gate' alphaD-validated-invalidated.md` = **EMPTY** (the contradicting label is gone). alphaD `:154` now reads "**The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count:** every residual CSS routing entry must name the `.bbnf` rule it derives from, and the CSS regen profile array (`regen_css.rs:45-153`) must trend toward the JSON emitter shape. The 594-line `css_l4.toml` LOC convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (SYNTHESIS §0.1)." This is the exact V3-prescribed replacement text, now in verbatim agreement with SYNTHESIS §0.1 `:111`, HANDOFF `:143-146`, and alphaC. The operationalized gate (`.bbnf`-derivation + regen-array trend) is unchanged and sound. | **YES** |
| V3-CH1-a stale meta-note | SYNTHESIS `:155-158` + `:12-13` now read "all cohort artefacts state the broadcast row count as 24 / lines 112-135 as of V3 ... the V2 '6' undercount is resolved across the cohort." No stale "6" survives in the broadcast-count meta-note. | **YES** |
| V3-CH1-b grep-substring mislabel | SYNTHESIS `:159-164` + `:13-14` read "`grep -c 'css_l4/' skinny/RESULTS.md` = 25, whose 25th match :154 is a prose REDRESS-127 companion reference, not a row." CH6 live-verified: `grep -c '^| css_l4/.*/direct_to_struct/main '` = **24**; `grep -c 'css_l4/'` = **25**; `sed -n '154p' RESULTS.md` = prose ("`css_l4/declaration_values/direct_to_struct/main`; REDRESS-127; companion …" — the path is a backtick-quoted reference *inside prose*, not a `^|`-anchored table row); `sed -n '154p' \| grep -c '^| css_l4/.*/direct_to_struct/main '` = **0**. The 25-vs-24 split is exactly as labelled. | **YES** |

All three V4 folds are fully landed and live-verified. The one V3 orphan REVISE (F1) is
resolved with the exact prescribed text; the two count-correction folds match live grep.
Zero orphan REVISE carries into V4.

## Citability spot-check (every CH6 claim orchestrator-citable; uncited = reject)

CH6 re-verified the load-bearing citation anchors live at HEAD `1c5bd7a25`:

- HEAD `1c5bd7a25` — `git rev-parse --short HEAD` confirmed.
- Core-tree symbols grep-clean-absent from `skinny/crates/`: `StructLayout`=0, `OpenFrame`=0,
  `CssArena`=0, `TapeStructBuilder`=0, `begin_compound`=0 (each `grep -rl … skinny/crates/`
  = 0 files, verified). The wrong-tree dishonesty REJECT (SYNTHESIS benched-surface note
  `:31-68`, telemetry `tape_activated` "NOT satisfiable by a grep in `crates/core/`" `:370`)
  rests on this and holds.
- `W5C_REQUEST_FACT_PROFILES` `codegen/src/lib.rs:336` (declared), `:299` (selected),
  `:567`/`:611` (iterated) — the Lock-14 phrase-#1 retire target. Verified live (all four
  line numbers exact).
- `regen_css.rs` seven `RuntimeEmitterKind::RequestFacts` literals at
  `:45,63,81,99,117,135,153` (live, exact) — the concrete seam-flip site (SYNTHESIS `:179`).
- `assert_lightningcss_strict_equality` def `nonjson_css_l4.rs:776`, call sites `:1057`/`:3460`
  (live, exact) — the retired fact-stream comparator (SYNTHESIS `:116`).
- `digit_mac` udot orphan: `parse_4_digits_dotprod` `:27`, `udot` asm `:40`, dispatch call
  `:12`. Verified — C4a's "wire the existing orphan" framing is real, not aspirational.
- `select_classifier` `dispatch.rs:42`, `lo6_table_admissible` `:101`, `PrimitiveKernels`
  `:50` — the C2 neutrality vehicle. Verified live.
- `sheets_witness` codegen fail-closed: the negative-control contract test
  `w5a_sheets_bbnf_fail_closed_through_runtime_contract` iterating `["google_sheets","bbnf"]`
  is present around `lib.rs:1075-1090` (verified) — the structural basis for the
  non-dischargeable repair.
- CSS benched corpus set `css_l4_corpus.rs:22-54` = `{bootstrap, tailwindcss,
  material-components-web, animate}`; `normalize` grep-clean-absent. Verified — every
  per-corpus gate names only benched corpora.
- `css_l4.toml` grep-clean-absent from `skinny/` (`find skinny -name css_l4.toml` = 0);
  lives only at the totality root — the basis for the F1 demotion.

Every CH6 disposition below is decided against live evidence; no claim is uncited.

## Deferral scan (the core CH6 mandate: no candidate deferred to "future wave")

`grep -rniE "future wave will detail|will be detailed|TBD|to be determined|deferred to a
future|figure out later|will detail later"` across `research/alpha/`, `SYNTHESIS.md`,
`HANDOFF.md` returns **zero hits** (exit 1). The only deferrals present are (a) the §4.4
wave-plan deferrals to S-P3 (owner paths, entry/exit gates, per-wave hard caps, revert
protocol), each cited with the contract reference (SYNTHESIS `:421-427`; HANDOFF `:235-236`);
and (b) the Sheets/BBNF-self projection-generality proof deferred to SK-V18, which is the
correct boundary (no `.bbnf` shape exists for `sheets_witness` to walk — non-dischargeable,
not paper-closed). Both are contract-sanctioned and cited. No candidate's *measurability*
is pushed to a future phase.

---

## Disposition ledger

### αA — Results extraction (`alphaA-results-extraction.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing | ACCEPT | "0/24 admitted", ">SOTA bar UNMET", substrate "UNWIRED dead code". No close-claim. |
| §1 canonical bench | ACCEPT | N=100 canonical cited; close gate keyed to same-run re-baselined lightningcss median, NOT a frozen literal; single-sample inadequacy disclosed. |
| §2 per-corpus | ACCEPT | "no SK-V16 per-corpus typed-CSS row to delta against" — honest absence. |
| §3 8-field equality | ACCEPT | EXACT counts cited; re-prove-before-speed stated. |
| §4 20x checkpoint | ACCEPT | 20x vs fragment distinguished from direct; watermark-unsound divergence honest. |
| §5 sub-wave ledger | ACCEPT | Every row cites SHA + report:line; W6-tape "unwired dead code" verbatim. |
| §6 banked wins | ACCEPT | V6 substrate citation corrected to skinny tree with core-tree-absence proof. |
| §7 goalset seed | ACCEPT | Close threshold = "same-run re-baselined lightningcss full-CSSOM median … NOT a frozen literal"; "~974 Mbps is a PRIOR-RUN REFERENCE only". V1 REVISE remains folded; V4 introduces no regression. |
| §8 citation ledger | ACCEPT | Every claim mapped to file:line / SHA; `grep TapeStructBuilder skinny/ = EMPTY` anchor re-verified. |

αA: 9 ACCEPT, 0 REVISE, 0 REJECT.

### αB — Competitor deltas (`alphaB-competitor-deltas.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 plane taxonomy | ACCEPT | lightningcss = fair materializing bar; cssparser = flaw-probe. Plane-honest. |
| §1 baseline | ACCEPT | Per-run scatter disclosed as statistical inadequacy; Wave-0 re-baseline mandate stated. |
| §2 per-corpus vs lightningcss | ACCEPT | 19 `[INF]` markers grep-counted live (`grep -c '\[INF\]'` = 19); UNMEASURED-PENDING clause "No SK-V17 wave exit-gate may key on an inferred per-corpus endpoint". The paper-close risk (downstream lifting an inferred multiple as measured) is structurally closed. |
| §3 vs cssparser | ACCEPT | Plane-mismatch disclosed; per-corpus rows carry `[INF]` inline; "beating cssparser is NOT the win condition". |
| §4 inter-comparator | ACCEPT | Materialization-tax framing cited as inter-comparator relation, not inferred. |
| §5 JSON guard | ACCEPT | Carry-forward, cites RESULTS rows; Track1/Track2 independence (Lock 1) named. |
| §6 findings feed | ACCEPT | Close-threshold = same-run median; per-corpus split UNMEASURED-PENDING so wave gates key only on the aggregate crossing until N≥50 emits the split — self-correcting. |

αB: 7 ACCEPT, 0 REVISE, 0 REJECT.

### αC — REDRESS digest (`alphaC-redress-digest.md`)

The strongest anti-paper-close artefact in the set (V1/V2/V3 carried it 10/0/0). Every
pre-block carries a *measurable* re-open test, not a prose prohibition. The broadcast-count
reconciliation holds: §4 and the §7 table row 4 both read 24 with `grep -c` cited (CH6
live-verified `= 24`) and the `W8_SELECTED_CSS_ROWS=24` falsified source. The `css_l4.toml`
handling is exemplary: framed as the design fold target, not a skinny owner path, with no
TOML-LOC gate claim — consistent with the F1 fold now landed in alphaD. No section
introduces a deferral or an uncited mandate.

αC: 10 ACCEPT, 0 REVISE, 0 REJECT.

### αD — Validated/invalidated ledger (`alphaD-validated-invalidated.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing / generality | ACCEPT | sheets_witness "structurally non-dischargeable (it has no shape to lower)"; "only exercised projection riders are JSON + CSS"; SK-V18 fold target named. The V2 CH2-V2-F1 (b′) repair holds. |
| §1 validated wins | ACCEPT | Each win cites SHA + measured evidence; V6 (tape) carries "UNWIRED dead code" verbatim; no-StructRegistry guard asserted *on the measured tree* (grep over `skinny/crates/` = 0, live-re-verified). |
| §2 invalidated | ACCEPT | I1/I2/I3/I7 each refute by measurement; I3 "summary margins do not transfer to the typed lane" is the load-bearing honesty. |
| §3 still-open O1-O4 | ACCEPT | Falsifiable NO-GO thresholds inlined: O1 ≥30/<20, O3-NEON ≥80/<60, O4 ≥300/<200, tailwind cross-or-honest-residual. O2 sheets_witness non-dischargeable repair present `:151`. The estimate-close paper-close risk is shut. |
| §3 still-open O5 (codegen unification) | **ACCEPT (F1 RESOLVED)** | `:154` now reads "**The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count:** … The 594-line `css_l4.toml` LOC convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (SYNTHESIS §0.1)." The V3-prescribed relabel is landed verbatim; `grep 'explicit telemetry-bound exit gate' alphaD = EMPTY` (live-verified). The exit gate now names the skinny-greppable test it actually applies (`.bbnf`-rule derivation + `regen_css.rs:45-153` trend) and demotes the totality TOML-LOC count to INFORMATIONAL, in verbatim agreement with the three downstream-canonical surfaces. The wrong-tree-gate paper-close cousin is closed. |
| §4 demoted | ACCEPT | Pattern H folds into O5; FNV stays bench-only quarantine. |

αD: 6 ACCEPT, 0 REVISE, 0 REJECT.

### αE — Candidate shortlist (`alphaE-candidate-shortlist.md`)

The artefact CH6 scrutinises hardest — where a candidate could be deferred to "future wave
will detail." It is not. Every candidate carries a numbered falsifiability gate + NO-GO
threshold + fallback at Pass-Alpha altitude. The sheets_witness repair is folded:
`sheets_witness` "NOT a dischargeable projection target in SK-V17 … codegen fail-closed
negative control (lib.rs:1075-1090)", and the EXIT-gate disjunct reads "view emitter walks
one BackendRule shape for JSON+CSS (sheets = SK-V18)".

| § | Disposition | Basis |
|---|---|---|
| §0 anchors | ACCEPT | Every anchor cites path:line / SHA (re-verified live). N≥50 statistical-adequacy precondition binds all gates; lightningcss bar = same-run re-baselined median. |
| C0 de-fact-stream | ACCEPT | Gate = measurable boolean + count + "throughput measured (any value — this wave does not promise a lift)"; same-wave consumer named. The "any value" is honest. |
| C1 tape wiring + lazy cursor | ACCEPT | Gate ≥30 PASS / <20 NO-GO / fallback REJECT+REDRESS; borrowed-slice-vs-lazy forced as an entry gate, not deferred; sheets_witness EXIT disjunct re-scoped to SK-V18. |
| C2 NEON pre-scan | ACCEPT | Gate ≥80 PASS / <60 NO-GO + checkasm-fail NO-GO; scalar-ref PRESENT + checkasm PRECEDENTED; ~56% hot-leaf % tagged `S-P1-re-confirm-on-benched-path` (actual-profiling); same-wave consumer. |
| C3 commit-by-construction | ACCEPT | Gate ≥300 PASS / >same-run lightningcss cross plausible / <200 NO-GO / 150-200 PARTIAL; HIGH risk + triumvirate trigger named. |
| C4a udot orphan | ACCEPT | Admits unconditionally (scalar-ref + checkasm satisfiable today); same-wave consumer retires the orphan; speed measured-not-promised. |
| C4b net-new i8mm | ACCEPT | The CH6 crux row: GATED behind a Wave-5 re-profile proving the digit leaf is top-N tailwind self-time — "If not proven, C4b is NOT dispatched (no net-new orphan kernel)"; PASS = tailwind crosses OR "an HONEST profiled residual … per no-paper-close (CH6)" (`:478,:563,:580`); "report residual; do NOT fabricate a cross" (`:482`). The canonical anti-paper-close clause, citing the discipline by name (live-verified). |
| §2 dependency order | ACCEPT | Gates carried into the wave diagram with thresholds; no orphan promise. |
| §3 cross-cutting | ACCEPT | "No paper-close (CH6): C4b may close with an honest profiled residual … it may NOT fabricate a cross." Grammar-neutral witnessed-not-asserted. |
| §4 escalation | ACCEPT | If C0 cannot land typed benched Track 1, "the entire CSS goalset is UNMEASURABLE and Pass Alpha must escalate per §8 (`BLOCKED`)" — refuses to manufacture a measurable goalset where none exists. |

αE: 11 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — SYNTHESIS.md (the goalset; PASS-ALPHA §4.1-§4.3)

| § | Disposition | Basis |
|---|---|---|
| benched-surface note | ACCEPT | The wrong-tree dishonesty REJECT is structural: five core-tree symbols grep-clean-absent (live-verified); every tape/layout gate "verifiable by grepping `skinny/crates/`, not `crates/core/`". The seam-flip site (`regen_css.rs:45-153`, 7 `RequestFacts` literals) named — verified live. |
| §0.1 close condition | ACCEPT | Every gate resolves to a measurable test (tape activation = grep non-zero + `PayloadArena` write/alloc counters; CSS equality = EXACT 8-field; >SOTA = median Track 1 > median lightningcss N≥50). `:111` carries the V3 `css_l4.toml` demotion verbatim ("INFORMATIONAL only, NOT an SK-V17 close gate"). |
| §0.2 starting state | ACCEPT | "LANDED, UNWIRED" + zero parse-path callers; lightningcss run-dependence disclosed ("No single committed measurement equals 974"); broadcast count 24 with grep cited; the V4 count-correction folds (24 rows / 25 substring / :154 prose) landed and live-verified `:155-164`. |
| §0.3 receiver goalset | ACCEPT | Each obligation concrete; names the deletion ("DELETE `W5C_REQUEST_FACT_PROFILES`"); the seam-flip seven literals + `regen_css` fn named; "NO new cursor/builder type". |
| §0.4 pre-blocks | ACCEPT | Verbatim CONTEXT pre-block + inherited REDRESS families + hidden-coupling escape list + no-second-substrate clause + the witness-honest generality clause (JSON+CSS only; sheets_witness non-dischargeable). Binding, cited. |
| §0.5 per-corpus close | ACCEPT | "the prior numbers (793/833/929/974) are NOT the gate; the gate is the same-run measured lightningcss median"; "All per-corpus endpoints are UNMEASURED-PENDING"; tailwind "explicitly allowed to land short … record gap honestly in REDRESS … NOT a tranche-blocking failure"; success = ≥1 regular corpus crosses else WARN escalation. |
| §0.6 comparator gate | ACCEPT | lightningcss full-CSSOM = fair bar; cssparser = flaw-probe; W6 fact-stream comparator (`assert_lightningcss_strict_equality:776`) retired; Track 2 ≠ Track 1 anchor. |
| Section 1 ledger | ACCEPT | A-series 454/735/496 cited as recognition-only, "explicitly NOT recovering the AZ-IV overfit". |
| Section 2 telemetry | ACCEPT | Gate "rejects any CSS row whose `sample_count < 50` or `sample_statistic != median`"; full-CSSOM comparator-plane enforced; equality-before-speed boolean; `tape_activated` "NOT satisfiable by a grep in `crates/core/`"; no-phantom-normalize; single-tuple-broadcast rejection (W8R tripwire). Gate consumer command named. Telemetry-bound + bench-verifiable — CH6's core mandate MET. |
| Section 3 trajectory | ACCEPT | Four-lever route cited to architecture doc; close + WARN-escalation stated; §4.4 wave plan deferred to S-P3 *with the contract citation* `:421-427`. |

SYNTHESIS: 11 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — HANDOFF.md

| § | Disposition | Basis |
|---|---|---|
| benched-substrate disclosure | ACCEPT | Core-tree symbols "grep-clean-absent from `skinny/crates/` (verified)"; benched substrate + fact-stream path cited at the verified lines. |
| Current state | ACCEPT | "LANDED BUT UNWIRED for CSS — zero CSS parse-path callers"; ">SOTA bar is NOT met and nothing on the CSS path moved"; lightningcss run-dependence disclosed; broadcast count 24 with `grep -c` cited `:52-57`. No overclaim. |
| What SK-V17 opens | ACCEPT | "The gating artefact is the lazy-view accessor generator — it does not exist yet"; four-lever route carries the S-P1 re-profile obligation; generality scope JSON+CSS with sheets_witness non-dischargeable `:90-100`. |
| Gate posture | ACCEPT | CH7 framed as "a **pass-added monotonic extension lens** (the six-lens set CH1-CH6 is the orchestrator-citable canon)" — verbatim correct against §3W. The `css_l4.toml` LOC convergence "NOT a CH7 scan gate … noted not gated" `:143-146` — the demotion correctly applied here. |
| Pre-blocked routes | ACCEPT | Verbatim, cites SYNTHESIS §0.4; no-second-substrate clause stated. |
| Next move | ACCEPT | Steps 1-7 each measurable; step 7 close criterion = the §0.5 gate; `tape_activated` "NOT by a grep returning non-zero in `crates/core/`"; escalation path named; revert protocol sanctioned-deferred to S-P3 with §4.4 authority `:235-236`. |

HANDOFF: 6 ACCEPT, 0 REVISE, 0 REJECT.

---

## CH6 cross-cutting findings (V4)

**1. All three V4 folds are fully landed and live-verified.** F1 (alphaD O5 `:154` TOML-LOC
relabel) is resolved with the exact V3-prescribed text — the contradicting "explicit
telemetry-bound exit gate" label is gone (`grep = EMPTY`), and the exit gate now names the
skinny-greppable test it actually applies (`.bbnf`-rule derivation + `regen_css.rs:45-153`
trend). The two V3-CH1 count-correction folds (24 table rows / 25 substring matches / :154
prose) match live grep exactly: `grep -c '^| css_l4/.*/direct_to_struct/main '` = 24,
`grep -c 'css_l4/'` = 25, RESULTS.md:154 is a backtick-quoted prose REDRESS-127 reference
(`grep -c '^|…'` on that line = 0). Zero orphan REVISE carries forward.

**2. Goalset is measurable + telemetry-bound — the central CH6 mandate is MET.** N≥50 cold
+ median bound at the gate level (Section 2: "rejects any CSS row whose `sample_count < 50`
or `sample_statistic != median`"). lightningcss is the materializing comparator (full-CSSOM
plane enforced; the `assert_lightningcss_strict_equality:776` fact-stream comparator retired;
same-run re-baseline mandated — the fixed literals 793/833/929/974 demoted to references).
Equality-before-speed is a boolean gate. Tape activation resolves to a grep over
`skinny/crates/` + `PayloadArena` write/alloc counters — explicitly NOT a grep in
`crates/core/`. No goalset row resolves to a prose promise.

**3. No candidate deferred to "future wave will detail."** The deferral scan returns zero
unsanctioned deferrals (exit 1). All five αE candidates (C0-C4b) are fully specified with
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
*measured* tree (grep over `skinny/crates/` re-verified = 0), not the design intent.

**6. CH6 re-converges at V4 (second consecutive ≥95% with zero orphan REVISE).** V3's single
orphan REVISE (F1) is now folded; V4 introduces no new defect. Every disposition is ACCEPT.
The §3Z second-of-two-consecutive condition is satisfied by CH6's own contribution; the
cross-lens convergence determination remains the CONSOLIDATED author's call.

---

## Counts

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| αA results-extraction | 9 | 0 | 0 |
| αB competitor-deltas | 7 | 0 | 0 |
| αC redress-digest | 10 | 0 | 0 |
| αD validated-invalidated | 6 | 0 | 0 |
| αE candidate-shortlist | 11 | 0 | 0 |
| SYNTHESIS (αF) | 11 | 0 | 0 |
| HANDOFF (αF) | 6 | 0 | 0 |
| **Total** | **60** | **0** | **0** |

ACCEPT rate: 60 / 60 = **100%**. Above the §3Z 95% bar; zero orphan REVISE; zero REJECT.

The goalset is measurable, telemetry-bound (N≥50 cold median + full-CSSOM lightningcss
comparator + equality-before-speed boolean + grep-verifiable tape activation), and every
claim is orchestrator-citable against live evidence at HEAD `1c5bd7a25`. No candidate is
deferred to a future wave; the only deferrals (the §4.4 wave revert protocol and the SK-V18
Sheets/BBNF generality proof) are contract-sanctioned and cited. The single V3 orphan REVISE
(F1) is resolved with the exact prescribed text. CH6 ANTI-PAPER-CLOSE: **clean PASS** for
cycle V4 — second consecutive ≥95% with zero orphan REVISE.
