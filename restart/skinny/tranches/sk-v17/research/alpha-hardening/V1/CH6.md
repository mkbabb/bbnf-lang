# CH6 — ANTI-PAPER-CLOSE — Pass Alpha SK-V17 cycle V1

Lens: CH6 ANTI-PAPER-CLOSE (ORCHESTRATOR §3W; PASS-ALPHA §3 CH6 = Next-Tranche-Impact).
Cycle: V1. Subject: SK-V17 Pass Alpha artefacts
`restart/skinny/tranches/sk-v17/research/alpha/{alphaA..E}.md` + the αF output
(`sk-v17/SYNTHESIS.md` + `sk-v17/HANDOFF.md`, per PASS-ALPHA §6, αF writes to the
tranche root, not an `alphaF.md` — verified absent and correctly so).
Host: aarch64 Apple M5 Max. HEAD `1c5bd7a25` (`git rev-parse --short HEAD` confirmed).

CH6 focus (this dispatch): no candidate deferred to "future wave will detail";
goalset measurable + telemetry-bound (N≥50 cold sampling, lightningcss materializing
comparator); every claim orchestrator-citable; revert protocol / hard caps /
triumvirate discipline present where this pass owns them; no self-report of
"complete"/"wired"/"verified" standing without orchestrator-citable live evidence.

The load-bearing CH6 distinction for this pass: PASS-ALPHA §4.4 **explicitly defers
the wave-by-wave revert protocol, owner paths, entry/exit gates, and per-wave hard
caps to skinny pass S-P3** (`sk-v17/SPEC.md`). A deferral that the contract sanctions
is NOT a paper-close. A deferral the contract does NOT sanction — a candidate whose
*measurability* is pushed to a future phase, a gate that resolves to a promise rather
than a number, a self-report of a landed artefact — IS a paper-close and CH6 rejects
it. Every disposition below is decided on that line.

---

## Disposition ledger

### αA — Results extraction (`alphaA-results-extraction.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 standing | ACCEPT | Honest: "0/24 admitted", ">SOTA bar UNMET", substrate "UNWIRED dead code". No close-claim. |
| §1 canonical bench | ACCEPT | N=100 canonical cited; single-sample inadequacy disclosed (`:54-59`, harness `nonjson_css_l4.rs:1134` verified). |
| §2 per-corpus | ACCEPT | "no SK-V16 per-corpus typed-CSS row to delta against" (`:97`) — honest absence, not fabricated baseline. |
| §3 8-field equality | ACCEPT | EXACT counts cited (`:118-128`); "re-prove EXACT before every admit" — gate-before-speed. |
| §4 20x checkpoint | ACCEPT | Distinguishes 20x (vs fragment baseline) from 14.2x/15.6x direct (`:172-174`); residual deferral named honest. |
| §5 sub-wave ledger | ACCEPT | Every row cites commit SHA + report:line; verdicts honest (all REJECTED on gate). |
| §6 banked wins | ACCEPT | Provenance per win. |
| §7 goalset seed | **REVISE** | `:250-256` "BEAT lightningcss (>974 Mbps median) on the regular corpora first (animate/bootstrap/material)". The target threshold `>974` is stated as the close, but αA's own §4.3-bound telemetry says the gate is the **same-run re-baselined lightningcss median**, not the W6 `~974`. αF/SYNTHESIS §0.5 corrects this ("the W6 numbers are NOT the gate; the gate is the same-run measured lightningcss median"). αA's §7 hands `>974` to αE/αF as a fixed number — orchestrator-citable as a *baseline*, NOT as the close threshold. **Fix:** §7 must state the close threshold as ">same-run measured lightningcss full-CSSOM median (re-baselined Wave 0), N≥50", and demote `~974` to the prior-run reference. path:line `alphaA-results-extraction.md:250-251`. |
| §8 citation ledger | ACCEPT | Every claim mapped to file:line / SHA. |

αA: 7 ACCEPT, 1 REVISE, 0 REJECT.

### αB — Competitor deltas (`alphaB-competitor-deltas.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 plane taxonomy | ACCEPT | lightningcss = fair materializing bar; cssparser = flaw-probe. Plane-honest. |
| §1 baseline | ACCEPT | Single-sample scatter disclosed; canonical used for arithmetic, scatter not. Harness cite `nonjson_css_l4.rs:1134` verified. |
| §2 per-corpus vs lightningcss | **REVISE** | The per-corpus table (`:96-102`) pairs *inferred* per-corpus Track 1 endpoints (animate↔164, tailwind↔51) against the lightningcss **corpus aggregate** ~974. αB self-flags this as "the one inferential step" (`:118-119, :242-245`) — which is honest. But the gap-multiple column ("~5.9× / ~13.9× / ~16.2× / ~19.1×") is then read as a per-corpus targeting signal feeding the wave plan, while the lightningcss per-corpus split is admittedly unpublished. A CH6 paper-close risk: a future row could cite "animate ~5.9× gap" as if measured. **Fix:** mark every per-corpus gap-multiple cell `INFERRED — confirm at N≥50` inline (not only in the §6/ledger footnote), so no downstream artefact lifts an inferred multiple as a measured datum. path:line `alphaB-competitor-deltas.md:96-102`. |
| §3 vs cssparser | ACCEPT | Plane-mismatch disclosed; "beating cssparser is NOT the SK-V17 win condition" (`:128`). |
| §4 inter-comparator | ACCEPT | Materialization-tax framing sound; "reaching cssparser is plane-incoherent" honest. |
| §5 JSON guard | ACCEPT | Carry-forward, cites RESULTS rows. |
| §6 findings feed | ACCEPT | Finding 5 mandates the N≥50 per-corpus lightningcss split so §2 "stops being inferential" — self-correcting. |
| Verification ledger | ACCEPT | Inferential step flagged explicitly for CH1. |

αB: 7 ACCEPT, 1 REVISE, 0 REJECT.

### αC — REDRESS digest (`alphaC-redress-digest.md`)

| § | Disposition | Basis |
|---|---|---|
| §0 method | ACCEPT | Two-bucket classification + re-open test + different-framing admission per pre-block — exactly the anti-paper-close structure. |
| §1 AZ-IV eager | ACCEPT | Re-open test is a *falsifiable counter* ("per-leaf typed/f64 heap alloc"), telemetry-bound to payload-arena counters (`:62-65`). |
| §2 StructRegistry/Arena | ACCEPT | SPLIT into permanent-block + admit-under-framing; telemetry binding names canada/bootstrap/tailwind + no-WATCHDOG (`:112-115`). |
| §3 fact-stream String | ACCEPT | Re-open test concrete (`push_str/fnv64 on hot path`); output-plane column gate named (`:153-156`). |
| §4 24-row broadcast | ACCEPT | "no different-framing admission" stated outright; telemetry = N≥50 cold median per corpus (`:189-191`). |
| §5 FNV/fixture | ACCEPT | Quarantine gate consumer cited (`gate-json --skv15-fnv-quarantine-report`, `:229-232`). |
| §6 x86/AVX | ACCEPT | "no different-framing admission in this pass"; Lock 16 manifest binding (`:265-266`). |
| §7 ledger | ACCEPT | Per-row re-open test + admission, all cited. |
| §8 load-bearing distinction | ACCEPT | The "typed=goal, eager=carrier" line is the anti-paper-close invariant for every wave. |

αC: 10 ACCEPT, 0 REVISE, 0 REJECT. (αC is the strongest anti-paper-close artefact in the set: every
pre-block carries a *measurable* re-open test, not a prose prohibition.)

### αD — Validated/invalidated ledger (`alphaD-validated-invalidated.md`)

| § | Disposition | Basis |
|---|---|---|
| §1 validated wins | ACCEPT | Each win cites commit SHA + measured evidence; V6 (tape) carries "UNWIRED" caveat verbatim — no false-wired claim. |
| §2 invalidated | ACCEPT | I1/I2/I7 each refute a claim *by measurement* (the W6 self-refutation is the model anti-paper-close move). |
| §3 still-open O1-O5 | **REVISE** | O1 (`:79`) states "rich CSS ~70 -> ~30-40 Mbps after alloc floor falls is the architecture estimate". This is an *estimate cited as a wave-1 outcome*. The other open candidates carry estimates too (O3, O4 bands). Estimates are legitimate as expected-value, but O1's framing ("the win is removing the syslib alloc/copy floor") risks a downstream paper-close where Wave 1 is declared "done" on the *estimate* rather than a *measured* ≥X Mbps gate. αE C1 fixes this with an explicit "≥30 Mbps PASS / <20 NO-GO" gate; αD's O-table does not carry the falsifiable threshold. **Fix:** O1-O4 must each append the falsifiable gate from αE (C1 ≥30 / C2 ≥80 / C3 ≥300 / C4 tailwind-cross-or-honest-residual) so the ledger's still-open candidates are not lifted as estimate-closed. path:line `alphaD-validated-invalidated.md:79-82`. |
| §4 demoted | ACCEPT | Pattern H / FNV / BackendShape folded honestly. |
| §5 ledger text | ACCEPT | "SK-V16 delivered the substrate and the honest diagnosis, not the CSS >SOTA beat" — no overclaim. |

αD: 4 ACCEPT, 1 REVISE, 0 REJECT.

### αE — Candidate shortlist (`alphaE-candidate-shortlist.md`)

This is the artefact CH6 scrutinises hardest, because it is where a candidate could be
deferred to "future wave will detail."

| § | Disposition | Basis |
|---|---|---|
| §0 anchors | ACCEPT | Every anchor cites path:line / SHA. Statistical-adequacy precondition binds ALL gates to N≥50 (`:30-35`) — exactly CH6's telemetry mandate. The architecture-doc translation correction (`:37-51`, skinny has no `StructLayout`/`OpenFrame`; core-tree paths ≠ benched surface) PRE-EMPTS a citability paper-close and is load-bearing. |
| C0 de-fact-stream | ACCEPT | Falsifiability gate is a *measurable boolean + count*, NOT a deferral ("benched CSS Track 1 row exists, is TYPED, 8-field EXACT, throughput measured — any value, this wave does not promise a lift", `:90-95`). The "any value" is honest: it refuses to promise a speed it cannot guarantee. Same-wave consumer named (`:87-89`). |
| C1 tape wiring + lazy cursor | ACCEPT | Gate ≥30 Mbps PASS / <20 NO-GO / fallback REJECT+REDRESS (`:138-142`) — concrete number, concrete fallback. The borrowed-slice-vs-lazy directive is named an ENTRY GATE "not a wave step" (`:150`), and §4 escalation makes it resolve at goalset-authoring time — this is anti-paper-close: the W6 stall decision is forced forward, not deferred. |
| C2 NEON pre-scan | ACCEPT | Gate ≥80 PASS / <60 NO-GO + checkasm-fail NO-GO (`:193-200`); scalar-ref PRESENT + checkasm PRECEDENTED with named existing tests (`:178-187`); lo6-admissibility fallback to scalar is the honest answer, not a CSS special-case. Same-wave consumer (`:188-192`). |
| C3 commit-by-construction | ACCEPT | Gate ≥300 PASS, >833 plausible / <200 NO-GO / 150-200 PARTIAL (`:243-250`). Risk class HIGH + "most likely to need a triumvirate" (`:254`) — names the triumvirate trigger, anti-paper-close. |
| C4 tailwind tuning | ACCEPT | The CH6 crux row, and it is correct: PASS = tailwind >833 **OR an HONEST profiled residual** ("tailwind achieves X% of lightningcss; the residual is Y leaf at Z% self-time"), "Fallback: report residual; do NOT fabricate a cross" (`:288-294`). This is the canonical anti-paper-close clause and it cites the no-paper-close discipline by name. |
| §2 dependency order | ACCEPT | Gates carried into the wave diagram with thresholds; no orphan promise. |
| §3 cross-cutting | ACCEPT | "No paper-close (CH6): C4 may close with an honest profiled residual; it may NOT fabricate a cross" (`:347-348`) — CH6 discipline stated as a binding gate. |
| §4 escalation | ACCEPT | If C0 cannot land typed benched Track 1, "the entire CSS goalset is UNMEASURABLE and Pass Alpha must escalate per §8 (BLOCKED)" (`:351-358`). This is the correct anti-paper-close posture: it refuses to manufacture a measurable goalset where none exists. |

αE: 11 ACCEPT, 0 REVISE, 0 REJECT. Every candidate carries a numbered falsifiability gate
with a NO-GO threshold and a fallback. ZERO candidates are deferred to "future wave will detail."
The five candidates are fully specified at Pass-Alpha altitude; only the §4.4 wave SEQUENCING
(owner paths, per-wave hard caps, revert protocol) is handed to S-P3 — which PASS-ALPHA §4.4
sanctions.

### αF output — SYNTHESIS.md (the goalset; PASS-ALPHA §4.1-§4.3)

| § | Disposition | Basis |
|---|---|---|
| §0.1 close condition | ACCEPT | Every gate row resolves to a *measurable* test: "grep for tape types in a parse path returns non-zero" (tape activation), "EXACT 8-field structural equality" (CSS equality), "median Track 1 typed > median lightningcss full-CSSOM on same run (>1.0x), N≥50" (>SOTA). The "Foldable into TOTALITY" and "PASS-IMPL close audit" rows are softer but bounded by the prior measurable rows — not standalone paper-close. |
| §0.2 starting state | ACCEPT | "LANDED, UNWIRED dead code (10 green substrate tests, zero parse-path callers)" — no false-wired claim. |
| §0.3 receiver goalset | ACCEPT | Each receiver obligation is concrete + names the deletion ("DELETE the OpenFrame template + match rule_id begin_compound"). |
| §0.4 pre-blocks | ACCEPT | Verbatim CONTEXT pre-block + inherited REDRESS families + hidden-coupling escape list. Binding, cited. |
| §0.5 per-corpus close | ACCEPT | THE anti-paper-close win of the goalset: "the W6 numbers are NOT the gate; the gate is the same-run measured lightningcss median re-baselined in Wave 0" (`:146-147`). Per-corpus current/target/intervention/fallback table. Tailwind explicitly "allowed to land short on first pass; record gap honestly in REDRESS; NOT a tranche-blocking failure" (`:153`) — honest residual sanctioned, not paper-closed. Tranche success = "at least one regular corpus crosses... else record honest residual and escalate per PASS-ALPHA §8 (WARN)" (`:156-161`). |
| §0.6 comparator gate | ACCEPT | lightningcss full-CSSOM = fair bar; cssparser = flaw-probe; fact-stream comparator retired. Track 2 ≠ Track 1 anchor stated. |
| Section 1 ledger | ACCEPT | Validated/invalidated cite SHAs; A-series 454/735/496 cited as recognition-only, "explicitly NOT recovering the AZ-IV overfit." |
| Section 2 telemetry | ACCEPT | The §4.3 binding: gate "rejects any CSS row whose sample_count < 50 or sample_statistic != median" + full-CSSOM comparator-plane enforcement + equality-before-speed boolean. The gate consumer command is named: `gate-json --check-results --skv17-css-sota-report <path>` (`:239`). This is telemetry-bound + bench-verifiable — CH6's core mandate is met. |
| Section 3 trajectory | ACCEPT | Four-lever route cited to architecture doc; close + WARN-escalation branches both stated; §4.4 wave plan deferred to S-P3 **with the contract citation** ("authored downstream by skinny pass S-P3"). |

SYNTHESIS: 9 ACCEPT, 0 REVISE, 0 REJECT.

### αF output — HANDOFF.md

| § | Disposition | Basis |
|---|---|---|
| Current state | ACCEPT | "LANDED BUT UNWIRED dead code — zero parse-path callers"; ">SOTA bar is NOT met and nothing on the CSS path moved." No overclaim. |
| What SK-V17 opens | ACCEPT | "The gating artefact is the lazy-view accessor generator — it does not exist yet" — honest absence. |
| Gate posture | **REVISE** | `:75-80` "Alpha hardening still runs CH1-CH7... CH7 overfit-prune is binding and cannot be folded into CH6." PASS-ALPHA §3 + ORCHESTRATOR §3W define the lens set as **CH1-CH6**; CH7 is permitted only as a monotonic extension a pass *adds when it surfaces a failure mode the six cannot disposition* (ORCHESTRATOR `:90-92`). The HANDOFF asserts CH7 is mandatory ("cannot be folded into CH6 or deferred") without citing where CH7 is defined for this pass or what overfit-prune scans that CH3 (regression/pre-block) + CH4 (cost/contrivance) do not already cover. This is not a paper-close, but it is an *uncited mandate* — CH6 requires every claim orchestrator-citable. **Fix:** either (a) cite the authority that elevates CH7 to mandatory for SK-V17 Pass Alpha, or (b) reframe CH7 as a pass-added extension lens with its explicit scan scope stated, so the orchestrator can dispatch it without inventing its contract. path:line `sk-v17/HANDOFF.md:75-77`. |
| Pre-blocked routes | ACCEPT | Verbatim, cites SYNTHESIS §0.4. |
| Next move | ACCEPT | Steps 1-7 each measurable; step 7 close criterion = the §0.5 gate; escalation path named. |

HANDOFF: 4 ACCEPT, 1 REVISE, 0 REJECT.

---

## CH6 cross-cutting findings

**1. Revert protocol / hard caps / per-wave triumvirate — correctly deferred, not paper-closed.**
PASS-ALPHA §4.4 sanctions deferral of owner paths, entry/exit gates, per-wave hard caps, and
revert protocol to S-P3 (`sk-v17/SPEC.md`). SYNTHESIS Section 3 and HANDOFF step 3 both cite
this deferral *with the contract reference*. αE C3 names the triumvirate trigger ("most likely
to need a triumvirate"). This is the legitimate boundary — Pass Alpha owns the *measurable
goalset*, S-P3 owns the *wave revert protocol*. CH6 does NOT reject this deferral; it is
contract-sanctioned. (Had the deferral been silent — no §4.4 citation — it would be a REJECT.)

**2. Goalset is measurable + telemetry-bound — the central CH6 mandate is MET.**
N≥50 cold + median is bound at the gate level (Section 2: "rejects any CSS row whose
sample_count < 50 or sample_statistic != median"). lightningcss is the materializing comparator
(full-CSSOM plane enforced; fact-stream comparator retired). Equality-before-speed is a boolean
gate. The gate consumer command is named and bench-verifiable. No goalset row resolves to a
prose promise; each resolves to a number, a boolean, or a grep result.

**3. No candidate deferred to "future wave will detail."**
All five αE candidates (C0-C4) are fully specified with falsifiability gates + NO-GO thresholds
+ fallbacks at Pass-Alpha altitude. The escalation note (αE §4) refuses to manufacture a
measurable goalset where C0 cannot land — the correct BLOCKED posture, not a deferral.

**4. No false-wired / false-complete self-report.**
Every artefact carries "UNWIRED dead code" / "does not exist yet" / "0/24 admitted" verbatim.
The substrate is never claimed as live; the lazy-view generator is never claimed as built. The
W6 self-refutation (lever-1 did not move throughput) is reproduced honestly across αA/αD.

**5. The three REVISEs are all the same failure shape: a number or mandate stated without its
falsifiable/citable binding** — αA §7's `>974` as close threshold (should be same-run median),
αB §2's inferred per-corpus multiples (should be marked INFERRED inline), αD §3's O-table
estimates (should append the αE falsifiable gates), and HANDOFF's CH7 mandate (should cite its
authority or state its scan scope). None is a paper-close; each is a citability tightening that
αF/SYNTHESIS has already gotten right and the upstream extractions must inherit. Zero orphan
REVISE: each fix points to the line in the same artefact set that already resolves it.

---

## Counts

| Artefact | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| αA results-extraction | 7 | 1 | 0 |
| αB competitor-deltas | 7 | 1 | 0 |
| αC redress-digest | 10 | 0 | 0 |
| αD validated-invalidated | 4 | 1 | 0 |
| αE candidate-shortlist | 11 | 0 | 0 |
| SYNTHESIS (αF) | 9 | 0 | 0 |
| HANDOFF (αF) | 4 | 1 | 0 |
| **Total** | **52** | **4** | **0** |

ACCEPT rate: 52 / 56 = **92.9%**. Below the §3Z 95% bar; V2 fold required for the four
REVISEs. Zero REJECT — no candidate fails CH6 anti-paper-close; the goalset is measurable,
telemetry-bound, and orchestrator-citable. The four REVISEs are citability tightenings, all
with the resolving artefact already in-set (zero orphan REVISE).
