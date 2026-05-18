# SK-V9 S-P3 CHALLENGE — CH4 COST

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V1. Lens: CH4 COST.
Date: 2026-05-18.
Scope: Adversarial cost-discipline review of the SK-V9 S-P3 P3 cohort —
seven artefacts at `restart/skinny/tranches/sk-v9/research/p3/`. S-P3 is
the pass that authors the wave manifest plus the per-wave cost set;
S-P1/S-P2 deferred it. CH4 verifies the cost discipline is complete:
every wave carries a LOC budget, a risk class, a hard cap, a same-wave
consumer, and a revert protocol; the LOC envelopes are realistic; the
caps are achievable inside the triumvirate phase budgets.
Disposition vocabulary: ACCEPT / REVISE / REJECT.

---

## §1 — Method

CH4 is the cost lens of the six-lens S-P3 CHALLENGE registry
(`PASS-3-SYNTHESIS-PLAN.md` §3): "does every wave carry a LOC budget, a
hard cap, a phase breakdown (research / plan / redress per
`SKINNY-TRIUMVIRATE.md`), and a same-wave-consumer requirement per
primitive? Is the wave count ≤ 12? Is the shortlist ≤ 8?" The
companion non-negotiable is `ORCHESTRATOR.md` §8 row "No contrivance —
smallest change that achieves elegance + performance" (CH4) and the
"Same-wave consumer — no orphan kernel" row (CH4 + CH6).

The cost yardsticks used below:

- **Phase caps** — `SKINNY-TRIUMVIRATE.md` §7: research 30 min × ≤6
  agents; plan 30 min × 1–2 agents; CHALLENGE 60–90 min wall when
  interposed; redress 75 min (60 impl + 15 measure). Wave wall ~3–4 h.
- **Bracket ceiling** — `ORCHESTRATOR.md` §3Z / `SKINNY-TRIUMVIRATE.md`
  §3: ≤12 waves per skinny bracket.
- **Shortlist ceiling** — `PASS-3-SYNTHESIS-PLAN.md` §2: ≤8 P3-A
  candidates.
- **LOC envelopes** — the per-candidate prelim LOC in P3-A §2.1, the
  per-wave Source-LOC-budget column in the P3-F SPEC §2 manifest, the
  HANDOFF Alpha cost binding (Apache/CITM ~300, retained proof ~450),
  and the upstream P2-A §5 / P2-B §6.1 / P2-C §2.0 / P2-D §3-§6 / P2-E
  §7.4 per-slice cost tables.

CH4 read all seven P3 artefacts, `ORCHESTRATOR.md` §8/§9,
`PASS-3-SYNTHESIS-PLAN.md`, and `SKINNY-TRIUMVIRATE.md` §1/§4/§7/§8.
Cost defects are graded against whether they would let a wave dispatch
uncosted, over-scoped, or with a cap the triumvirate cannot meet.

The single load-bearing structural finding governing this lens: **the
three artefacts that own the wave manifest disagree on wave count and
wave content.** P3-A/P3-C number five behavior waves W1–W5 with the
codec at W4 and the ASM kernels at W5; P3-B numbers five behavior waves
W1–W5 but pairs codec+kernels into one W4 with W5 as the close wave;
P3-F numbers six waves W1–W6 bound to SPEC §3–§9 with codec at W4,
kernels at W5, close at W6. This disagreement is itself a CH4 cost
defect — a wave whose LOC budget and cap live under a different wave
letter in each artefact is not coherently costed — and it propagates
through several rows below.

---

## §2 — Disposition table

| # | Locus | Finding | Lens check | Disposition |
|---|---|---|---|---|
| 1 | P3-F SPEC §2 manifest | Every behavior wave (W1–W5) carries a `Source LOC budget` column and a `Hard cap` column; W0/W6 carry "telemetry/gate/report only" / "docs only". The five mandatory cost fields are all present per wave across §3–§9. | Every wave carries LOC budget + hard cap. | ACCEPT |
| 2 | P3-F SPEC §2 vs P3-A/P3-C wave count | P3-F manifests **six** waves (W1–W6, behavior W1–W5 + close W6); P3-A §1.4/§3 and P3-C §1.4 manifest **five** (W1–W5, close = W5). The wave-letter→content binding is non-isomorphic across the cohort. A wave's budget cannot be verified when its identity floats. | No wave uncosted. | REVISE |
| 3 | P3-B §2 vs P3-F §2 — W4/W5 split | P3-B pairs P2-D+P2-E into one W4 (codec + 32-byte string-block + EOR3 + CTZ) and makes W5 the close. P3-F splits them: W4 = string-block widening (P2-D §4), W5 = codec (P2-E + P2-D §3). The two splits carry different LOC budgets for the same source surface. | No wave over-scoped; budget realistic. | REVISE |
| 4 | P3-F SPEC §2 W5 budget `<=600 hand + <=120 regen` | P3-F W5 is the codec alone. P3-A C4 sizes the codec at **~1,045 net** (P2-E §7.4: ~890 hand + ~120 regen + ~250 test − 215 deletion). The P3-F 600-hand budget is **~445 LOC under** the P2-E-sourced envelope it claims to carry. The wave as budgeted cannot land the codec it names. | P3-A LOC envelopes carried into wave sections. | REJECT |
| 5 | P3-F SPEC §2 W4 budget `<=300 hand + tests` | P3-F W4 = string-block widening only. P3-A C5 sizes it ~145–270 LOC including the `_32` body, scalar oracle, producer rewire, OR-fold, and `checkasm_string_block.rs` (P2-D §4.3). The 300-hand budget is realistic for C5 in isolation — but only because P3-F has moved the codec out to W5. Internally consistent with P3-F's own (defective) split; flagged for the §3-cited split disagreement. | P3-A envelope carried. | REVISE |
| 6 | P3-A §2.1 C4 envelope `~1,045 net` | C4 is the largest candidate; P2-E §7.4 names eleven slices, ~6.0 h aggregate per-slice cap. CH4 finding: ~1,045 net LOC + an 11-slice, 6-hour aggregate **cannot complete inside one 75-min redress** (`SKINNY-TRIUMVIRATE.md` §7). P3-A itself flags this ("the largest candidate") but neither P3-A nor P3-F splits it. A single-wave codec is structurally over-capped. | Hard cap realistic for the triumvirate redress phase. | REJECT |
| 7 | P3-F SPEC §2 W5 hard cap `<=90 min` | The SPEC §2 manifest assigns every wave a `<=90 min` hard cap. `SKINNY-TRIUMVIRATE.md` §7 sizes the **redress phase** at 75 min (60 impl + 15 measure); the 90-min figure is the wave's redress allowance, not the wave wall. For the W5 codec — ~1,045 net LOC, 6 h of P2-E per-slice cap — 90 min of redress is unmeetable by a factor of ~4. The cap is nominal, not realistic. | Wave hard cap realistic. | REJECT |
| 8 | P3-F SPEC §2 manifest note | "Phase caps per `SKINNY-TRIUMVIRATE.md` §7: Research 30 min × ≤6 agents; Plan 30 min; CHALLENGE 90 min …; Redress 75 min (60 impl + 15 measure)." The phase breakdown is present and cites the contract verbatim. | Phase breakdown per `SKINNY-TRIUMVIRATE.md`. | ACCEPT |
| 9 | P3-F SPEC §2 — "90 min" vs §7 "75 min" redress | The manifest's per-wave `<=90 min` column and the manifest note's "Redress 75 min" disagree by 15 min for the same phase. The reader cannot tell whether a wave's redress budget is 75 or 90. A cost field that contradicts its own contract citation is not a coherent budget. | Hard cap = realistic + contract-consistent. | REVISE |
| 10 | P3-F SPEC §2 W3 budget `<=265 hand + <=120 regen` | Matches P2-A §5.9 (~265 hand + ~120 regen net) and P3-A C3 (~265 hand + ~120 regen). The envelope is faithfully carried from P2-A into the wave section. | P3-A envelope carried. | ACCEPT |
| 11 | P3-F SPEC §2 W3 — folded P2-D §5 structural-bitmap chain | P3-A C3 §2.2 explicitly states the P2-D §5 structural-bitmap chain (~120–220 LOC bbnf-simd + ~30–60 VEXT + ~60–120 cursor wire + ~50–90 checkasm = **~260–490 additional LOC**) is "C3-internal" and that "S-P3 P3-B/P3-C reconcile whether the §5 body lands in C3's wave." P3-F W3 §6 owner table A.6 says "plus the `bbnf-simd/src/aarch64/` structural-bitmap chain" but the W3 budget `<=265 hand + <=120 regen` does **not** include the §5 chain LOC. W3 is under-budgeted by ~260–490 LOC, and P3-A §2.2's explicit reconciliation request is unanswered. | No wave over-scoped vs budget; P3-A envelope carried. | REJECT |
| 12 | P3-A §2.2 C3 vs P3-C §1.4 W5 — §5 chain ownership | P3-A says the §5 chain is "C3-internal" (W3 scope). P3-C §1.4 maps "W5 — P2-D §5 aarch64 ASM kernels (EOR3 ladder, CSSC CTZ, structural-bitmap)" — i.e. P3-C puts the structural-bitmap chain in W5, not W3. P3-A and P3-C place the same ~260–490-LOC body in different waves. Its budget is therefore double-uncertain: which wave, and whether counted. | No wave uncosted. | REVISE |
| 13 | P3-F SPEC §2 W1 budget `<=300 hand` | Matches P2-C §2.0 (~255 hand + run-id refresh ≈ ~300) and the HANDOFF Alpha cost binding (Apache/CITM ~300 LOC). P3-A C1 confirms five slices ~85 min ≤ 90-min cap. Realistic; the wave is mechanical (no kernel, no substrate). | P3-A envelope realistic + carried; cap realistic. | ACCEPT |
| 14 | P3-F SPEC §2 W2 budget `<=425 hand, 0 generated` | Matches P2-B §6.1 (~425 aggregate per-slice cap) and inside the HANDOFF 450-LOC envelope; P3-A C2 confirms ~395–425, ≤90 min, five small slices all NEW files. The "0 generated" qualifier is correct (proof is hand-written `cfg`-gated). Realistic. | P3-A envelope realistic + carried; cap realistic. | ACCEPT |
| 15 | P3-F SPEC §2 — generated-LOC exclusion rule | §2 states "Generated outputs do not consume the source LOC budget, but every generated file is named, diff-audited, and included in the revert slice." Cost-clean: generated regen is not double-counted against the hand budget, and the W3 `<=120 regen` / W5 `<=120 regen` columns make the regen volume explicit and bounded. | LOC budget discipline coherent. | ACCEPT |
| 16 | P3-F SPEC §2 — "LOC budgets are conjunctive with the 90-minute cap … A wave plan exceeding either bound splits before dispatch or returns REVISE." | The split-before-dispatch escape valve is present and correctly conjunctive (both bounds bind). This is the right cost-discipline mechanism — but it is only as good as the budgets it gates, and rows 4/6/7/11 show W3 and W5 already exceed their bounds at draft time, so the split should have fired in P3-F itself. | Over-scoped wave splits before dispatch. | REVISE |
| 17 | P3-A §2.1 shortlist count | Eight candidates C1–C8. Exactly at the `PASS-3-SYNTHESIS-PLAN.md` §2 ≤8 ceiling. C8 (checkasm backfill) and C6/C7 are correctly classed as non-standalone (preconditions / sub-slices), so the eight do not inflate the wave count. | Shortlist ≤ 8. | ACCEPT |
| 18 | P3-B §1 / §2 wave count | Five behavior waves W1–W4 + close W5 (six counting closed W0). P3-F: six W1–W6 + W0. Both are well inside the ≤12 bracket ceiling (`ORCHESTRATOR.md` §3Z). Wave count is not over the ceiling under either numbering. | Wave count ≤ 12. | ACCEPT |
| 19 | P3-B §2 W4 — codec + string-block + EOR3 + CTZ in one wave | P3-B W4 bundles C4 (~1,045) + C5 (~145–270) + C6 (~60–120) + C7 (~15–35) + the C8 checkasm files (~330–390) = **~1,595–1,860 LOC in one redress wave**. P3-B's stated cap for W4 is the §7 75-min redress. This is the most over-scoped wave in the cohort by a wide margin; P3-B's pairing rationale (P2-E §6.4 same-wave conditional) is correct for C4+C5 but does not license folding C6+C7+all checkasm into the same 75-min redress. | No wave over-scoped; cap realistic. | REJECT |
| 20 | P3-B §2 manifest — per-wave LOC budget column absent | P3-B §2's wave-manifest table columns are Wave / Name / Candidate / Entry gate / Class / Triumvirate shape. There is **no LOC-budget column** and **no hard-cap column**. P3-B is the wave-sequencing artefact; `PASS-3-SYNTHESIS-PLAN.md` §2 P3-B scope explicitly requires "Per-wave: entry gate, owner-path family, conditional-dispatch status, **hard cap**." P3-B omits the hard-cap column its own scope mandates. | Every wave carries hard cap (in the sequencing artefact). | REVISE |
| 21 | P3-B §3 per-wave prose | The §3 dependency-justification prose cites LOC inline ("300 LOC, ≤90 min" for W1; "~395 LOC" for W2; "~265 hand + ~120 regen LOC" for W3) — so LOC is present narratively. But it is prose, not a budgeted manifest column, and W4 (the bundled wave) gets no aggregate LOC figure at all in §3. The cost of the largest wave is the least costed. | LOC budget present + per-wave. | REVISE |
| 22 | P3-C §2 per-wave gate tables — revert protocol | Every W1–W5 gate table in P3-C §2 carries a `Revert protocol` row, and §3.3 carries the W2 proof-only revert. Each cites its upstream slice-level revert (P2-C §4.3, P2-B §6.1, P2-A §5, P2-E §7.1, P2-D §5.3.1/§5.4). The falsifiability gates each carry a revert protocol. | Falsifiability gates carry a revert protocol. | ACCEPT |
| 23 | P3-C §2 per-wave gate tables — maintain envelope | Every gate carries a `Maintain envelope` row: the W10b six-row WIN-block on W3/W4/W5, the four typed-GO + two/three direct rows on W1/W4/W5, and the explicit "W10b not gated" vacuity note on W1. The maintain envelope is present per gate. | Falsifiability gates carry a maintain envelope. | ACCEPT |
| 24 | P3-C §1.1 four-part gate contract | §1.1 defines the gate as four mandatory parts — exit gate, maintain envelope, revert protocol, same-wave consumer — and §2 honors all four per wave. The same-wave-consumer row is present per gate (W1 `gate_only`; W3 `at_cursor`; W4 the x4 JSON path + `match_string_at_quote_trusted_utf8`; W5 the union-substrate consumers). The per-primitive same-wave-consumer requirement is met. | Same-wave-consumer requirement per primitive. | ACCEPT |
| 25 | P3-C §2 W5 vs P3-F §8 W5 — incompatible wave content | P3-C §2 "W5" = P2-D §5 ASM kernels (EOR3, CSSC CTZ, structural-bitmap), exit rows `gsoc-2018 ≥ 41198`, `github_events ≥ 19418`, `random ≥ 13788`. P3-F §8 "W5" = the unicode codec, exit rows `unicode_escapes`/`y_string_unicode`/`unicode_mixed`/`gsoc-2018`. The W5 falsifiability gate P3-C authored gates a **different wave** than the W5 the SPEC manifests. The SPEC W5 has no P3-C gate; the P3-C W5 gate has no SPEC wave. | Every wave carries a measurable, matching gate. | REJECT |
| 26 | P3-B §4 dependency-driven sequencing vs caps | P3-B §4 establishes W1–W5 all dispatch under SK-V9 authority (no Pass Omega gate). The topological order P2-B→P2-A→P2-D/E is sound and creates no cycle. But the sequencing **forces** the cascade-lock (P2-D §0: "the wave may not be split") onto C3+C4+C5+C6+C7, and that cascade-lock is exactly what produces the over-capped W4 of row 19 — the dependency graph mandates a wave that cannot complete in 75 min. The sequencing is dependency-correct but cap-incoherent: it does not reconcile "may not be split" with the 75-min redress ceiling. | Sequencing does not create a wave that cannot complete in its cap. | REJECT |
| 27 | P3-A §2.1 C8 envelope `~330–390 test LOC` | C8 checkasm backfill is realistically sized (P2-E S6 ~250 + P2-D §4.3 ~40–70 + ~40–70) and correctly classed infra-only / LOW risk. But C8's LOC is a same-wave precondition of C4/C5 and is **not added** into any W4/W5 SPEC budget — P3-F W4 says "`<=300 hand + tests`" with "tests" uncosted, and W5 `<=600 hand + <=120 regen` names no checkasm allowance. The ~330–390 checkasm LOC is real work with no budget line. | P3-A envelope carried into wave sections. | REVISE |
| 28 | P3-F SPEC §3 W0 / §9 W6 | W0 budget "telemetry/gate/report only", W6 "docs only", both `<=90 min`. Non-source waves correctly carry a qualitative budget and a cap; W6 close-reconciliation at 90 min is realistic for a five-document reconcile. | Every wave carries a budget + cap. | ACCEPT |
| 29 | P3-A §2.1 risk classes | Every candidate carries an explicit risk class: C1 LOW, C2 LOW, C3 MEDIUM, C4 MEDIUM-HIGH, C5 MEDIUM, C6 MEDIUM, C7 HIGH, C8 LOW. P3-F SPEC §2, however, has **no risk-class column** — the manifest carries LOC + cap but drops the risk grade P3-A established. The CH4 scope item "every wave carries … risk class" is met in P3-A but not carried into the P3-F manifest. | Every wave carries a risk class (in the SPEC manifest). | REVISE |
| 30 | P3-F SPEC §2 W3 — folded chain risk escalation | P3-A C3 §2.2 warns "the P2-D §5 structural-bitmap chain raises the wave's aggregate risk to HIGH if folded in whole." P3-F W3 §6 folds the chain (owner A.6) but the SPEC neither records the risk escalation nor splits the chain out. A wave silently promoted MEDIUM→HIGH without a recorded risk grade is uncosted on the risk axis. | Risk class per wave; over-scope split. | REVISE |
| 31 | P3-F SPEC §2 / §4 — W1 CHALLENGE phase | W1 §4 / dispatch §"Phase 2.5" mark CHALLENGE optional for W1 (mechanical baseline-whitelist expansion). Correct per `SKINNY-TRIUMVIRATE.md` §4 ("for routine waves … CHALLENGE may be skipped"). Skipping CHALLENGE keeps W1 inside its cap without phantom cost. | Phase breakdown realistic. | ACCEPT |
| 32 | P3-F dispatch-draft §"Per-Wave Triumvirate Protocol" | The dispatch draft restates the phase caps (research 30, plan 30, CHALLENGE 90, redress 75) and the "0.9× cap commit / cap halt" rule from `ORCHESTRATOR.md` §9. The per-wave phase breakdown is present and contract-faithful in the dispatch artefact. | Phase breakdown per `SKINNY-TRIUMVIRATE.md`. | ACCEPT |

Disposition tally: 32 rows — 12 ACCEPT, 13 REVISE, 7 REJECT.

---

## §3 — Aggregate verdict

**CH4 COST verdict: REJECT.**

The cost discipline is structurally present but materially incoherent.
The P3-F SPEC manifest carries a LOC-budget column and a hard-cap
column for every wave, the phase breakdown cites `SKINNY-TRIUMVIRATE.md`
§7 verbatim, the generated-LOC exclusion is cost-clean, the
split-before-dispatch escape valve exists, and P3-C carries a revert
protocol and a maintain envelope on every gate. On the formal-presence
checklist the cohort largely passes (12 ACCEPT).

But three load-bearing cost failures block ACCEPT:

1. **The wave manifest is non-isomorphic across the cohort (rows 2, 3,
   12, 25).** P3-A/P3-C number five behavior waves with the codec at W4
   and the ASM kernels at W5; P3-B pairs codec+kernels into one W4 with
   W5 = close; P3-F numbers six waves W1–W6 with codec at W5, kernels
   at W5's neighbour, close at W6. The P3-C W5 falsifiability gate
   (ASM kernels, `gsoc-2018 ≥ 41198`) gates a wave the P3-F SPEC does
   not manifest, and the P3-F W5 codec wave has no P3-C gate. A wave
   whose LOC budget, cap, risk grade, and gate live under three
   different wave letters across the cohort is not coherently costed —
   this is the CH4 "no wave uncosted" failure in its purest form.

2. **W5 (the codec, under P3-F numbering) is budgeted ~445 LOC under
   its own sourced envelope and capped ~4× under its own per-slice
   estimate (rows 4, 6, 7).** P3-F SPEC §2 budgets W5 at `<=600 hand`;
   P3-A C4, carrying P2-E §7.4, sizes the identical codec at ~1,045 net
   across eleven slices with a ~6.0 h aggregate per-slice cap. A
   75-/90-min redress cannot land an 1,045-LOC, eleven-slice, six-hour
   intervention. The wave as drafted is a paper budget.

3. **W3 is under-budgeted by ~260–490 LOC and its risk grade is
   silently escalated (rows 11, 12, 30).** P3-A C3 §2.2 explicitly
   handed S-P3 a reconciliation question — does the P2-D §5
   structural-bitmap chain land in W3 or a co-sequenced slice — and
   warned the fold pushes W3 from MEDIUM to HIGH. P3-F W3 §6 folds the
   chain (owner path A.6) but the W3 `<=265 hand` budget excludes the
   chain's ~260–490 LOC and the SPEC manifest carries no risk column to
   record the escalation. P3-A's explicit ask is unanswered.

Underneath these, the dependency-driven sequencing itself is
cap-incoherent (row 26): P2-D §0's "the wave may not be split"
cascade-lock, faithfully honored by P3-B, mandates a W4 of
~1,595–1,860 LOC (row 19) that cannot complete in a 75-min redress.
The cohort never reconciles "may not be split" against the 75-min
redress ceiling — it inherits the constraint and ships the over-cap.

The P3-A LOC envelopes are themselves realistic and well-sourced (C1
~300, C2 ~425, C3 ~265+120, C5 ~145–270, C8 ~330–390 all trace to P2
per-slice tables); the defect is not in P3-A's estimation but in the
**carry-through** — P3-F does not faithfully transcribe P3-A's
envelopes into the wave sections, and where it does transcribe (W1,
W2, W3-hand) it then under-counts the folded or paired surface.

This must fold to V2. Until the manifest is isomorphic and W3/W5 are
either re-budgeted to their sourced envelopes or split, no behavior
wave past W2 can be dispatched against a budget that means anything.

---

## §4 — Cost gaps requiring V2 fold

The following must close before CH4 can return ACCEPT:

1. **Unify the wave manifest across P3-A, P3-B, P3-C, P3-F.** Pick one
   wave numbering and one wave→content binding and make all four
   artefacts isomorphic to it. The split decision (codec+kernels as one
   wave vs two) must be settled once; every artefact's LOC budget, cap,
   risk grade, gate, and revert protocol must then bind to the agreed
   wave letter. (Rows 2, 3, 12, 25.)

2. **Re-budget or split W5 (the codec).** P3-F W5 `<=600 hand` must
   either rise to the P2-E §7.4-sourced ~1,045 net (and then the
   `<=90 min` redress cap must rise with it, or the wave must split into
   two redress passes), or the codec must be sub-divided so each
   sub-wave fits a 75-min redress. A 1,045-LOC eleven-slice wave under a
   600-LOC/90-min budget is not dispatchable. (Rows 4, 6, 7.)

3. **Reconcile the P2-D §5 structural-bitmap chain into a budgeted
   wave.** Answer P3-A C3 §2.2's explicit reconciliation request: the
   ~260–490-LOC chain lands in W3 (then W3's budget rises to
   ~525–755 hand + ~120 regen and the risk grade records HIGH) or in a
   separate co-sequenced wave with its own budget/cap/gate. P3-A and
   P3-C must stop placing it in different waves. (Rows 11, 12, 30.)

4. **Add a risk-class column to the P3-F SPEC §2 manifest.** P3-A
   establishes a risk grade per candidate (C1–C8); the SPEC manifest
   drops it. CH4 scope requires every wave to carry a risk class; carry
   P3-A's grades into the manifest and record the W3 MEDIUM→HIGH
   escalation if the §5 chain is folded. (Rows 29, 30.)

5. **Add a hard-cap column to the P3-B §2 wave-manifest table.**
   `PASS-3-SYNTHESIS-PLAN.md` §2 P3-B scope mandates a per-wave hard
   cap; P3-B §2's table omits it (LOC and cap appear only in §3 prose,
   and the bundled W4 gets no aggregate LOC figure at all). (Rows 20,
   21.)

6. **Budget the C8 checkasm LOC explicitly.** The ~330–390 LOC of
   checkasm differential tests are a same-wave precondition of the
   codec and string-block waves; P3-F W4's "`+ tests`" and W5's budget
   carry no checkasm line. Add an explicit test-LOC allowance to each
   wave that lands a primitive. (Row 27.)

7. **Resolve the 75-min vs 90-min redress-cap contradiction.** The
   P3-F SPEC §2 manifest column says `<=90 min`; the same section's
   note says "Redress 75 min (60 impl + 15 measure)". Fix to one figure
   and state whether 90 is wave-redress-wall or whole-wave. (Row 9.)

8. **Reconcile the cascade-lock against the redress ceiling.** P2-D §0
   says the C3+C4+C5+C6 consumer slices "may not be split" from the
   union substrate; the 75-min redress cannot hold ~1,595–1,860 LOC.
   V2 must either (a) demonstrate the cascade-lock permits the kernels
   to land in a *subsequent* wave so long as C3 lands first (re-reading
   P2-D §0's "same wave" as "after the substrate, not literally one
   redress"), or (b) escalate to the user that the SK-V9 behavior
   bracket genuinely needs more than the nominal wave count and the
   ≤12 ceiling absorbs it. A cap that the dependency graph structurally
   violates is not a cap. (Rows 19, 26.)

CH4 disposition: **REJECT** — fold all eight gaps into S-P3 V2.
