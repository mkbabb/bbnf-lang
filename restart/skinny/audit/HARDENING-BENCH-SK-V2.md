# HARDENING-BENCH-SK-V2

## §1 Target identification

- **Target**: `restart/skinny/BENCH.md` (post-iteration; 1,796 lines).
- **Cycle**: SK-V2 (post-iteration hardener-side verification of the SK-V1 punch list).
- **Trigger**: SK-V1 returned `SK-AMENDMENT-REQUIRED-NARROW` for BENCH with 15 punch items. The user has since iterated (the redress log at `skinny/REDRESS.md`, the regenerated full run at `skinny/RESULTS.md`, and the alternate-route invalidations described therein). SK-V2 walks each SK-V1 BENCH item, classifies CLOSED / SUPERSEDED / STILL-OPEN / NEW, and renders the post-iteration verdict.
- **Lens stack applied**: Lanes 1-9 (Lane 2 N/A — single-wave skinny) + Lenses F/G/H/I/J/K + Lenses L/M/N. Lens M and Lens L are load-bearing this cycle (the iteration produced measurement evidence which Lens M consumes; the iteration produced two false-route invalidations which Lens H ratifies and Lens L re-classifies the cost-model masking signal against).
- **Cross-quadrant context**: read REDRESS.md in full; RESULTS.md (regenerated full run + masking probe rows) read in full; HARDENING-BENCH-SK-V1.md punch list read in full; HARDENING-CONSOLIDATED-SK-V1.md items C1-C20 read in full.
- **Out-of-scope per skinny HARDENING.md §10**: this report touches only `restart/skinny/audit/HARDENING-BENCH-SK-V2.md` (the present file); it does not modify BENCH.md, sister-quadrant specs, or V1-corpus surfaces. The amendment dispatch is the orchestrator's responsibility.
- **Time consumed**: ~38 minutes against 45-minute cap.

## §2 Cohort verdict — disposition table

Per the SK-V2 prompt directive, walk each of the 15 SK-V1 BENCH punch items.

| # | SK-V1 item (subject) | Disposition | Evidence |
|---:|---|---|---|
| 1 | §10.2 outcome ID enumeration `<A|B|C|D|E|F|G|H|I|J|K|L>` (stale) | **STILL-OPEN** | BENCH.md:1361 still emits the pre-redress string; missing `F-positive`, `F-noise`, `M`; still contains stale `H`. |
| 2 | §6.3 line 675 "Outcomes G and H exist..." (stale) | **STILL-OPEN** | BENCH.md:677 still reads "Outcomes G and H exist precisely because the bench is the arbiter". |
| 3 | §6.2.1 F-band classification gap (Track 2 ∈ (S×1.05, S×1.10] AND Track 1 > Track 2 × 1.10 — no matching outcome) | **STILL-OPEN** | BENCH.md:614-624: F-positive (Track 1 ≤ Track 2 × 1.05) and F-noise (Track 1 ∈ (Track 2 × 1.05, Track 2 × 1.10]) cover only Track 1 ≤ Track 2 × 1.10. No row admits Track 1 > Track 2 × 1.10 when Track 2 is in the F-band. The classification-order at §6.2.1 step 7 still uses the same un-augmented sub-band logic; subsequent steps require Track 2 ≤ S × 1.05. Empirically irrelevant for the *current* full-run measurements (which fall in outcome G uniformly, Track 1 / sonic ∈ [53.1%, 74.0%]), but the matrix gap remains as a falsifiability hole the next iteration could land in if the substrate gap narrows. |
| 4 | §6.1 F-noise rationale ("criterion `noise_threshold(0.02)` plus 5% headroom") hand-waved | **STILL-OPEN** | BENCH.md:622 verbatim still reads "within noise (criterion `noise_threshold(0.02)` plus 5% headroom)". No measurement-driven boundary substituted. |
| 5 | §7.8.3 cold-cache eviction primitives wrong (`__dsb` is a barrier; `_mm_clflush` per-line; TLB / branch-predictor cooling absent) | **STILL-OPEN** | BENCH.md:1063-1064 still names `core::arch::aarch64::__dsb` and `_mm_clflush` without stride loop or TLB qualifier. The redress focus was the host-call probe + matrix amendments; the cold-cache primitive correctness amendment did not land. |
| 6 | §7.8.1 Probe A pseudo-precision ("≤ 50 ns/call canonically ~10-30 ns") | **STILL-OPEN** | BENCH.md:986-987 unchanged. Empirical evidence STRENGTHENS the steelman now (measured dispatch overhead is 0.71-0.73 ns per call per RESULTS.md), which makes the 50 ns ceiling look generous to the point of theatre — but the *spec text* still carries the pseudo-precise "canonically ~10-30 ns" hedge that SK-V1 flagged. The amendment is now both editorial AND should incorporate the empirical 0.7-ns measurement as a new floor sanity check ("measured prototype: ~0.7 ns/call M1 Pro"). |
| 7 | §7.8.3 < 1.2× cold/warm branch qualifier (about whether eviction primitives are insufficient) | **PARTIAL — STILL-OPEN** | BENCH.md:1073-1075 reads "< 1.2× is suspicious — likely the cache eviction did not actually cool the relevant lines; RESULTS notes the row as inconclusive rather than passing". This is roughly the epistemic guard SK-V1 wanted, but the surgery was an explicit "if the eviction primitives in this probe are insufficient for the platform, the < 1.2× cold/warm ratio fires the suspicious branch automatically" qualifier. Empirical evidence: cold_first_parse rows at twitter 86.3%, citm 98.7%, canada 96.8% of warm — i.e., cold/warm ratios of 1.16×, 1.01×, 1.03× — three of three corpora ARE under 1.2×. The "inconclusive rather than passing" branch fires for all three corpora on the current run. The RESULTS.md instead reports them as `PASS <=2.00x T1`, which contradicts the spec's "inconclusive" disposition. **This is a real cohesion fault on top of the SK-V1 finding.** |
| 8 | §9.6 peak RSS forward-projection ("3× threshold may never fire; gate appears ceremonial") | **STILL-OPEN** | BENCH.md:1298-1316 unchanged — no forward-projection calculation added. The 3× ratio remains an asserted safety net without computation. |
| 9 | §10.3 probability mapping for "alternate_pext_mask_plan < canonical × 0.90 on x86_64 → MASKING" | **SUPERSEDED** (per SK-V2 prompt) | The SK-V2 prompt directly states this item is "SUPERSEDED by dispatch-table invalidation". Reading the iteration evidence (REDRESS.md §17, RESULTS.md masking probes), the alternate-dispatch-table-plan probe duplicated canonical Track 1; a real function-pointer table regressed; canonical lowering remains Rust `match`. The PEXT mask plan row is marked "missing" in RESULTS.md (never landed an implementation). The C7 / item 9 surgery (probability mapping row addition for x86_64 plan divergence) is now moot at the dispatch-table axis — there is no empirical x86_64 advantage to map. **However**: the PEXT axis is still a residual question. The disposition is: the original C7 surgery as worded ("alternate_pext_mask_plan < canonical × 0.90 on x86_64") is no longer load-bearing because the supporting probe is unimplemented; the broader Lens-L MASKING concern (cross-platform plan divergence) is no longer empirically supported and is genuinely SUPERSEDED. See §3.L below. |
| 10 | §1.2 TapeBuilder cross-ref to SUBSTRATE.md §8 | **STILL-OPEN** | BENCH.md grep returns one mention of `TapeBuilder` at line 1785 (in §15 scope summary's incremental-parse row), but BENCH §1.2 still references `runtime::tape` generically without citing SUBSTRATE §8 / INDEX deviation ledger row 6. The named-inversion API contract is still uncited from the BENCH §1.2 Track 2 section. |
| 11 | §11.1 LOC reconciliation with WORKSPACE.md row 9 + INDEX headline | **STILL-OPEN** | WORKSPACE.md:36 row 9 still cites "≤500 LOC Track 2 handwritten substrate probe"; WORKSPACE.md:73 still budgets `bbnf-bench` at 2,000 LOC with "Track 2 handwritten parser (≤500)". INDEX.md:3 + :14 still headline "~31,400 handwritten LOC". The BENCH §11.1 measurement-driven model (800-1,500 Track 2 + optional 600 CSS prior) is unreconciled with the sister quadrants. Cross-quadrant fault unchanged. |
| 12 | §7.8.1 Probe B band rationale clarification (dispatch overhead atop eager-decode work) | **STILL-OPEN AND NOW LOAD-BEARING** | BENCH.md:996-1006 unchanged: "expected delta **5-15% on twitter**, **3-8% on citm**, **< 2% on canada**". RESULTS.md measured deltas: twitter 64.9% (T1 12470 → eager 7187 Mbps, gross-time 1.74×), citm 29.6% (T1 12246 → eager 9452, gross-time 1.30×), canada 22.1% (T1 8895 → eager 7282, gross-time 1.22×). All three measured deltas are **outside** the spec's "expected" bands by a factor of 4-30×. Per the spec's own rule at line 1001 ("Pass: probe within the per-corpus expected band. Fail: probe **outside** the expected band — high or low. A high outlier means eager decode costs more than expected on that corpus and V1 SOTA probability drops"), all three corpora FAIL Probe B. The RESULTS.md row correctly marks them as `MASKING >1.15x T1` / `>1.08x T1` / `>1.02x T1` — but the BENCH.md spec text still asserts the unmet bands as "expected". This is the strongest Lens L finding of the cycle: **the spec's bands are empirically refuted; the iteration log (REDRESS.md §19) concedes that "V1 JSON must either keep decode lazy OR accept the SOTA hit", but BENCH.md §7.8.1 still encodes the obsolete expected-delta numbers**. SK-V1 framed this as an editorial clarification (item 12); post-iteration it is a *load-bearing* falsification — the spec's expected bands are wrong by an order of magnitude and the "must keep lazy" must replace the "must keep lazy or accept the SOTA hit" framing per the SK-V2 prompt directive. |
| 13 | §8.3-§8.4 CI runner discount over-engineering (collapse to "CI advisory") | **STILL-OPEN** | BENCH.md:1158-1184 unchanged: §8.3 still emits the discount table; line 1176 still references `runners.toml`; §8.4 still describes the local override. No collapse landed. |
| 14 | §11.1 LOC budget hedge (metadata.rs ≤ 280, gate.rs ≤ 400) | **STILL-OPEN** | BENCH.md:1589-1591 verbatim still reads `metadata.rs ≤ 250 LOC`, `gate.rs ≤ 350 LOC`. The hedge did not land. |
| 15 | §9.5 cross-ref to §7.8.2 (Pratt / cost-model dispatch bounded by alternate-plan probe) | **PARTIAL CLOSURE** | BENCH.md:1295-1296 reads "The alternate-plan probe (§7.8) bounds only the JSON cost-plan cut; it does not validate the full V1 recognizer miner". This is the cross-ref item 15 substantively requested. The wording could be sharper ("§7.8.2" not "§7.8" — §7.8 is the masking-probes parent), but the substantive cross-reference exists. Treat as closed pending one-word edit. |

**Summary disposition counts**:

| Disposition | Count | Items |
|---|---:|---|
| CLOSED | 0 | — |
| PARTIAL CLOSURE | 2 | 7 (qualifier present but contradicted by data), 15 (cross-ref present but section-imprecise) |
| STILL-OPEN | 12 | 1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 14 |
| SUPERSEDED | 1 | 9 (per SK-V2 prompt + iteration evidence) |
| **NEW (SK-V2-surfaced)** | 4 | N1-N4 below |

### NEW items (N1-N4) surfaced post-iteration

| ID | Subject | Source | Lens |
|---|---|---|---|
| N1 | §7.8.1 must say "V1 must keep lazy string decode" (not "must keep lazy or accept the SOTA hit") | SK-V2 prompt + REDRESS.md §19 + RESULTS.md masking probes | Lens L |
| N2 | §7.8.1 expected-delta bands (5-15% twitter, 3-8% citm, <2% canada) are empirically refuted; must be replaced with measured prototype values (twitter 1.74× gross, citm 1.30× gross, canada 1.22× gross) or removed in favour of the masking-classification thresholds (>1.15× T1 / >1.08× T1 / >1.02× T1) already in use by RESULTS.md | RESULTS.md row 14, REDRESS.md §19 | Lens L |
| N3 | §7.8.2 framing was "confirmatory + one plausibly-better candidate (PEXT)" with dispatch-table as the second confirmatory; iteration invalidated the dispatch-table probe (duplicated canonical → real implementation regressed). Per SK-V2 prompt: "the framing in SK-V1's BENCH was 'confirmatory + one plausibly-better candidate'; that framing is partly refuted (the candidate ITSELF was the false-win)". §7.8.2 should now read "confirmatory: scalar passes; dispatch-table INVALID-and-rejected; PEXT mask unimplemented (V1 H tranche owns)" — without claiming "plausibly-better candidate" status the spec no longer carries empirical backing for. | REDRESS.md §17 + RESULTS.md alternate-plan rows | Lens H + Lens L |
| N4 | §7.8.3 cold-cache probe: the three cold/warm ratios from the current RESULTS.md are 1.16× / 1.01× / 1.03× (i.e., all under the 1.2× "suspicious / inconclusive" threshold), yet the gate marks them `PASS <=2.00x T1`. The pass-line predicate at BENCH.md:1069 (`cold_first_parse_us ≤ track1_generated_us × 2.0`) does not have an else-branch for the < 1.2× "inconclusive" case. The classifier writes PASS when the spec text says INCONCLUSIVE. | RESULTS.md cold_first_parse rows | Lens H + Lens M |

## §3 Cohort verdict — 9-lane + lens table

| Lane / Lens | Verdict | Drivers |
|---|---|---|
| 1 — Lock-Adherence | honoured-with-narrow-amendment | TapeBuilder cross-ref (item 10) still open; Lock 8 honoured throughout the matrix. |
| 2 — Sequencing | N/A | Single-wave skinny. |
| 3 — Cohesion | **honoured-with-narrow-amendment** | Items 1, 2 (stale outcome refs); item 15 cross-ref section-imprecise; N4 spec/gate disagreement. |
| 4 — SOTA Anchoring | LOAD-BEARING / honoured | Every threshold cites competitor + corpus + platform. BEAT_BOUND construction survives empirically (the current measured Track 2 / sonic ratios — 56.9% / 53.0% / 64.0% — land cleanly in outcome G, NOT in the F-band boundary; the matrix is empirically separating). |
| 5 — Grammar-Authoritative | honoured | No grammar-name dispatch; JSON is sole skinny grammar. |
| 6 — LOC Budget | honoured-with-narrow-amendment | Item 11 cross-quadrant reconciliation unchanged. Item 14 hedge unmade. |
| 7 — Friction Forecast | honoured | RESULTS.md verdict-first format remains user-readable. |
| 8 — Carry & Deferral | honoured | §9 omissions name receivers. §9.5 cross-ref present but imprecise (item 15). |
| 9 — Greenfield Discipline | honoured | No legacy code; root-cause framing. |
| F — LLM bias | honoured-with-narrow-amendment | Item 6 (Probe A pseudo-precision) still open; the empirical 0.7 ns/call measurement now makes the 50 ns ceiling look ornamental rather than calibrated. |
| G — Overfitting | honoured | CSS prior probe (§9.1) unchanged; still the load-bearing anti-overfit lever. No regression. |
| H — Hallucination + provenance | **honoured-on-dispatch-table-invalidation; STILL AMENDMENT-REQUIRED-NARROW on cold-cache primitives** | The §7.8.2 alternate_dispatch_table_plan row at line 1029 now correctly documents the invalidation ("SK prototype's first row duplicated canonical Track 1 and is invalid; a real 256-entry function-pointer table regressed, so canonical remains `match`") — Lens H provenance closed on that axis. Item 5 (`__dsb` / `_mm_clflush` correctness) still open. N4 spec/RESULTS disagreement on cold-cache pass-line a new fault. |
| I — Contrivance | honoured-with-narrow-amendment | Item 13 (CI runner discount) still open. |
| J — Host-language leverage | honoured | No change. |
| K — Meta-grammar discipline | honoured | No change. |
| **L — Premise fidelity** | **AMENDMENT-REQUIRED-NARROW — load-bearing** | N1 / N2 / N3 are LENS L items: the iteration produced measurement evidence that REFUTES the spec's expected bands (item 12 / N2); the SK-V2 prompt mandates "must keep lazy" (N1); the PEXT-mask-as-plausibly-better framing has lost empirical support (N3). |
| **M — Falsifiability** | **honoured-with-narrow-amendment — empirically validated** | The matrix HAS now produced a NO-GO (outcome G across all three corpora in the regenerated full run); the matrix HAS been measured against. The matrix is empirically calibrated and falsifiable at the substrate-gap axis. The F-band classification gap (item 3) remains a latent falsifiability hole but is not currently load-bearing. The F-noise rationale (item 4) remains hand-waved. The peak RSS projection (item 8) is still asserted; with the canada full run reporting payload-arena writes/allocations 0/0 and allocated tape bytes 3.57 MB on canada (1.59× input), the projection should be empirically computable now. |
| **N — Graduation mechanicality** | honoured | No new V1-deviating apparatus introduced by iteration; the redress is internal to the skinny prototype. |

**Final decision**: **SK-AMENDMENT-REQUIRED-NARROW** (BENCH-side).

The iteration successfully closed one provenance axis (dispatch-table invalidation now documented at §7.8.2), but the BENCH.md spec itself was not amended against the SK-V1 punch list — 12 of 15 items remain STILL-OPEN, 1 is SUPERSEDED, 2 are partial. Additionally, the iteration produced measurement evidence that surfaces 4 new items (N1-N4) — chiefly that the §7.8.1 expected-delta bands for Probe B are empirically refuted by an order of magnitude and the spec's framing ("V1 must keep lazy decode or accept the SOTA hit") is too weak given the data.

## §4 Lens L findings (load-bearing this cycle)

### §4.1 Probe B bands refuted (item 12 → N2)

The SK-V1 audit flagged the band rationale as "ambiguous between dispatch overhead and total cost" and proposed editorial clarification (item 12 in BENCH-V1, C8 in CONSOLIDATED). SK-V2 finds the issue is sharper than that: the iteration prototype routed `host_call_eager_decode` as the eager-decode work bound (per BENCH.md:1008-1013 "Prototype redress note: if the runnable skinny has not yet routed this probe through the host registry, the row is still valid as an eager-decode-work bound, not as a dispatch-overhead bound") and the *measured* eager-decode gross-time penalty is:

| Corpus | Spec expected band (BENCH.md:997-1000) | Measured (RESULTS.md, derived from row vs T1) | Spec gate (line 1001) verdict |
|---|---|---|---|
| twitter | 5-15% delta | T1 12470 → eager 7187 Mbps = **+74% gross-time / 57.6% T1 Mbps ratio** | FAIL (high outlier) |
| citm | 3-8% delta | T1 12246 → eager 9452 Mbps = **+30% gross-time / 77.2% T1 Mbps ratio** | FAIL (high outlier) |
| canada | < 2% delta | T1 8895 → eager 7282 Mbps = **+22% gross-time / 81.9% T1 Mbps ratio** | FAIL (high outlier) |

The spec's own classifier (BENCH.md:1001 "Fail: probe **outside** the expected band — high or low. A high outlier means eager decode costs more than expected on that corpus and V1 SOTA probability drops") commits each of these to FAIL. The gate code does fire MASKING signals (per RESULTS.md rows 14, 20, 26: `MASKING >1.15x T1`, `>1.08x T1`, `>1.02x T1`).

The Lens L finding: the §7.8.1 spec text encodes obsolete expected-delta bands that the empirical iteration has refuted. The bands were drafted as the *dispatch* overhead atop eager work (per SK-V1 §3.L commentary); the prototype reroutes the probe as the *gross* eager-decode-work bound, and the measured gross-time bound is 4-30× larger than the dispatched-only bound. Two amendments are needed simultaneously:

1. **N1 — premise commitment strengthening**: per the SK-V2 prompt directive, §7.8.1 must say "V1 must keep lazy string decode" (not "must keep lazy **or** accept the SOTA hit"). The "or accept" alternative is a confirmation-bias escape hatch — given the measured 22-74% gross-time penalty, the SOTA hit is severe enough that the alternative is no longer "accept and ship": it is "abandon SOTA viability for the JSON line". The empirical data closes the disjunction.

2. **N2 — band replacement**: the §7.8.1 expected-delta numbers (5-15% / 3-8% / < 2%) should either be re-derived against the gross-time measurement (>1.15× / >1.08× / >1.02× T1 — the thresholds RESULTS.md already uses to fire MASKING) or removed entirely in favour of "this probe is a MASKING bound on parse-time eager decode; V1 must keep decode lazy". The current spec text predicts numbers the implementation cannot land within an order of magnitude.

Crucially, the surgery is not "tighten the spec until the prototype passes" — the surgery is "remove the spec's false predictions and replace them with the empirical MASKING threshold the gate now uses". The gate is the arbiter; the spec must align with what the gate measures, not assert a counterfactual band.

### §4.2 Cost-model masking signal SUPERSEDED (item 9 → SUPERSEDED, plus N3)

The SK-V1 audit identified one Lens L MASKING signal (C7 in CONSOLIDATED): the "alternate_pext_mask_plan < canonical × 0.90 on x86_64" branch in §7.8.2 routed to "cross-platform plan divergence as a tranche-H input" but did not propagate to §10.3's probability mapping. SK-V1 proposed adding a row "alternate_pext_mask_plan < canonical × 0.90 on x86_64 → MASKING: cross-platform plan divergence; V1-SOTA-beat probability on Intel line drops by 0.10-0.20".

The post-iteration evidence: the alternate_dispatch_table_plan probe was invalidated empirically (REDRESS.md §17 + RESULTS.md). The alternate_pext_mask_plan probe is marked "missing" in RESULTS.md (not implemented). The original SK-V1 surgery as worded is now moot at the dispatch-table axis (the cost-model masking from dispatch shape DOES NOT EXIST per measurement), and is contingent at the PEXT axis (no implementation to draw evidence from).

Per the SK-V2 prompt: "The cost-model masking signal SK-V1 carried as C7 is therefore SUPERSEDED."

I steelman the alternative: do not the probabilities still need a cross-platform divergence row because PEXT *might* still be the V1 cost-model selection on Intel even though the skinny didn't measure it? Answer: no — the §7.8.2 confirmatory framing was always "the alternate-plan probes bound cost-driven rewrite", and the post-iteration empirical signal is that the canonical match-arm dispatch is NOT dominated by the function-pointer alternative; what *might* dominate on x86_64 is unmeasured and therefore not a skinny-side update. The probability-mapping row would be a *prediction without evidence*, which is precisely what SK-V1's Lens L would flag in a fresh audit. The honest disposition: SUPERSEDED, with an explicit note that the PEXT axis is V1 H.W2 work and the skinny does not bound it.

The §7.8.2 spec text framing itself should be amended (N3) to reflect the empirical state:
- `alternate_scalar_plan`: confirmatory; PASSES (canonical > alternate_scalar across all three corpora per RESULTS.md rows showing scalar at 48.3% / 61.8% / 47.9% of canonical T1).
- `alternate_dispatch_table_plan`: INVALID and EMPIRICALLY REJECTED; canonical lowering remains Rust `match`.
- `alternate_pext_mask_plan`: not implemented; remains a V1 H.W2 question; the skinny is silent.

The "plausibly-better candidate" descriptor at §7.8.2 paragraph 1 (and at §15 line 1780 scope summary) no longer carries empirical backing — the PEXT plan is unimplemented; the dispatch-table plan is rejected. The framing should be "confirmatory only (scalar passes); the previously-described 'plausibly-better' candidates are either rejected (dispatch table) or out of skinny scope (PEXT — V1 H.W2 owns)".

### §4.3 Host-fn-free Lens L disposition (unchanged from SK-V1; iteration confirms)

The two host-call probes (Probe A dispatch-overhead, Probe B eager-decode) deliver exactly the empirical split SK-V1 ratified:

- Probe A: PASSES at 0.71-0.73 ns/call across all three corpora (per RESULTS.md). The 50 ns/call threshold is met with three orders of magnitude headroom. The host-fn-free skinny is FAITHFUL on the *dispatch* axis: V1 grammars with many `@host fn` calls per parse will not pay measurable dispatch overhead.

- Probe B: FAILS the spec's expected bands across all three corpora (see §4.1 above). The host-fn-free skinny is MASKING on the *eager-decode-work* axis: V1 JSON cannot ship `decode_json_string_to_arena` at parse time without taking a 22-74% gross-time hit on SOTA.

The empirical split is exactly what SK-V1 said the two-probe design tested for, and the split lands as ratified: dispatch is fine, eager decode is not. The Lens L disposition is FAITHFUL on dispatch + MASKING on eager decode, and the V1 closure (keep decode lazy) is now empirically forced rather than merely "preferred".

## §5 Lens M findings

### §5.1 The matrix has produced a NO-GO and was measured against (load-bearing closure)

The redressed matrix's load-bearing claim from SK-V1 was that it could return NO-GO. The current full run (RESULTS.md) returns outcome G / NO-GO across all three corpora — Track 2 / sonic ratios of 54.6% / 50.0% / 68.0% are all worse than the S × 1.10 floor (which would require Track 2 / sonic ≥ ~90.9%). The matrix fires the correct verdict. Lens M's load-bearing function (preventing confirmation-bias dispatch) is **empirically validated** by the iteration.

This is the strongest Lens M finding of any skinny cycle to date: the matrix is not theatre; it is in production and returning NO-GO. The substrate gap is honestly accounted for at the gate level.

### §5.2 F-band classification gap (item 3) latent but not load-bearing

The classification-order hole at Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10 remains structurally present (no matching outcome). However, the current run is uniformly in outcome G (Track 2 > S × 1.10 across all three corpora) — Track 2 / sonic ratios 54.6%, 50.0%, 68.0% versus the S × 1.10 boundary at 90.9%. The substrate is far enough from the F-band that the next iteration would need to *more than double* Track 2 throughput to enter the F-band, and the gap would only matter if codegen overhead also widens. The fault is real but not currently load-bearing.

Steelman: should this be deferred to SK-V3 when the substrate has closed enough that the F-band is in play? Counter-steelman: the matrix is the *spec*, and a spec falsifiability hole is a spec defect regardless of whether the current run lands there. Surgery is one-row addition or sub-band collapse (per SK-V1 item 3). Keep STILL-OPEN; close in SK-V2 amendment dispatch.

### §5.3 F-noise rationale unchanged (item 4) — still hand-waved

Same disposition as SK-V1 §3.M. The "criterion `noise_threshold(0.02)` plus 5% headroom" conflation of iteration-to-iteration drift with track-to-track ratio is unchanged. Item 4 must close in SK-V2 amendment dispatch — replace with "Track 1 95% CI upper bound overlaps Track 2 × 1.05" (or equivalent measurement-driven derivation).

### §5.4 Peak RSS projection (item 8) now empirically computable

SK-V1 proposed a forward-projection: "tape (8-byte tokens × ~280K offsets on canada) ≈ 2.24 MB; payload arena empty; typed root ≈ 3-5 MB; total ~5-7 MB ≈ 1× sonic-rs canada peak. M outcome's 3× threshold is a safety net, not a primary gate."

Post-iteration: the canada tape materialization row reports 167,196 tokens, 2.68 MB logical tape bytes, 3.57 MB allocated tape bytes, 0 payload bytes (RESULTS.md). The SK-V1 estimate (2.24 MB tape) was close to the measured logical (2.68 MB) and notably below the measured allocated (3.57 MB — the private-Vec semantic sealing per REDRESS.md #15 over-allocates by ~33%). The forward-projection is now better grounded but should be updated against measurement: canada tape ~2.68-3.57 MB + typed root ~3-5 MB ≈ 5.7-8.6 MB. Versus sonic-rs canada peak (community-anchored ~5-7 MB lazy materialisation), the substrate is now at ~1.1-1.4× sonic-rs — still well under the 3× safety net.

Item 8 amendment dispatch should incorporate the post-iteration measured tape allocation as the projection basis, not the pre-iteration estimate.

### §5.5 Cold-cache spec/RESULTS contradiction (N4)

The §7.8.3 spec text at BENCH.md:1073-1075 says < 1.2× cold/warm ratio is "suspicious — likely the cache eviction did not actually cool the relevant lines; RESULTS notes the row as inconclusive rather than passing." Empirically, all three corpora land at 1.16× / 1.01× / 1.03× (cold_first_parse Mbps / warm T1 Mbps inverted: 10759 / 12470 = 1.16× faster, etc., interpreting RESULTS.md's "vs Track 1" 86.3% / 98.7% / 96.8% as cold_throughput / warm_throughput, which means cold/warm time ratio = warm/cold throughput = 1.16× / 1.01× / 1.03×). The RESULTS.md gate marks them `PASS <=2.00x T1`.

The spec says these should be INCONCLUSIVE; the gate marks them PASS. Either:
1. The spec text is wrong and < 1.2× is genuinely PASS (eviction primitives work) — but per SK-V1 item 5 Lens H finding, the primitives `__dsb` and unstrided `_mm_clflush` are technically wrong, so the eviction probably did NOT cool the lines, and the spec's "suspicious" classification is the honest one;
2. The gate is wrong and should report INCONCLUSIVE — bringing the gate code in line with the spec.

The amendment dispatch should resolve this: replace the eviction primitives (SK-V1 item 5), and update the spec's < 1.2× branch to either fire INCONCLUSIVE (gate change) or document why it now fires PASS (spec change). This is a Lens H + Lens M cross-fault: provenance (wrong primitives) cascades to falsifiability (the gate's PASS does not match the spec's INCONCLUSIVE).

## §6 Lens H findings

### §6.1 Dispatch-table invalidation now documented (load-bearing closure)

The §7.8.2 row for alternate_dispatch_table_plan at BENCH.md:1029 now reads:

> "Confirms LLVM's match-arm codegen on byte-disjoint alts is ≈ direct table. The SK prototype's first row duplicated canonical Track 1 and is invalid; a real 256-entry function-pointer table regressed, so canonical remains `match`."

This is the iteration's load-bearing Lens H closure. Per the SK-V2 prompt:

> "Verify the BENCH spec reflects the empirical invalidation of the dispatch-table alternate. The §7.8.2 framing in SK-V1's BENCH was 'confirmatory + one plausibly-better candidate'; that framing is partly refuted (the candidate ITSELF was the false-win)."

The spec now correctly documents the false-win invalidation. Lens H provenance on the dispatch-table axis is closed.

What remains under Lens L (per N3 above): the §7.8.2 *parent-paragraph* framing at line 1015-1022 still describes the alternates as "**confirmatory, not adversarial**: they verify the canonical structural-index + alt-dispatch plan is not dominated by other plausible plans within the implementation envelope". This framing is now PARTIALLY refuted — the dispatch-table candidate that was supposed to be one of the plausible plans is now invalidated; the PEXT mask candidate is unimplemented; the only remaining confirmatory alternate is the scalar plan, which passes. The honest framing for the §7.8.2 parent paragraph: "**Confirmatory only — scalar plan; the previously-described dispatch-table alternate was invalidated post-implementation (duplicated canonical, then regressed on real implementation); the PEXT mask alternate defers to V1 H.W2 absent skinny-side implementation**." This is N3.

### §6.2 Cold-cache primitives (item 5) and TLB / branch-predictor cooling (item 5 detail)

Unchanged from SK-V1. `__dsb` is a barrier, not an eviction primitive; `_mm_clflush` is per-line and needs 64-byte stride iteration over corpus + parser hot-data regions; TLB and branch-predictor cooling are absent. The < 1.2× / inconclusive guard SK-V1 added at item 7 is in place but contradicted by the gate's PASS (per §5.5 above).

The amendment is one paragraph: replace primitives with `dc civac` loop (aarch64) and 64-byte-stride `_mm_clflush` (or `_mm_clflushopt`) loop (x86_64); add explicit qualifier on TLB and branch-predictor scope; resolve the < 1.2× INCONCLUSIVE-vs-PASS contradiction.

### §6.3 Probe A pseudo-precision (item 6) now empirically falsifiable

The spec text says "≤ 50 ns/call on M1 Pro (one virtual call + table lookup is canonically ~10-30 ns; 50 is generous)" (BENCH.md:986-987). The measured prototype reports 0.71-0.73 ns/call. The discrepancy is not that the 50 ns ceiling is wrong (the prototype passes with 70× headroom); it is that the "canonically ~10-30 ns" claim is now also wrong by an order of magnitude — the measured per-call dispatch is *sub-nanosecond*. The pseudo-precise hedge SK-V1 flagged is now empirically falsifiable: either the spec text is wrong about the canonical range (it is — the prototype proves it), or the prototype's dispatch shape is non-canonical (which would itself be a finding).

Steelman: the 0.71 ns/call measurement is likely a near-zero-overhead amortised number — the registry is presumably inlined at the call site in release mode or the dispatch becomes a single load + indirect branch. The "canonically ~10-30 ns" range applies to a worst-case un-inlined virtual call + hash-map lookup. Counter-steelman: the spec should say either the worst-case (cite it with provenance) OR the measured prototype value (cite it as measurement), but NOT a hand-waved middle ground.

Amendment dispatch: replace the bracketed text "canonically ~10-30 ns; 50 is generous" with either:
- "measured M1 Pro prototype: ~0.7 ns/call (inlined release); the 50 ns/call threshold leaves headroom for V1 grammars where the registry path is not inlined (estimate worst case: virtual call ~5-10 ns + hash-map lookup ~20-40 ns ≈ 30-50 ns)"
- or simply remove the bracketed phrase: "Threshold: ≤ 50 ns/call on M1 Pro. Pass: probe ≤ 50 ns/call. Fail: probe > 50 ns/call."

The first is more informative; the second avoids re-introducing pseudo-precision.

## §7 Lens N findings (graduation mechanicality)

Unchanged from SK-V1. No new V1-deviating apparatus introduced by the iteration; the redress is internal to the bench spec and prototype. The 7-row deviation ledger at INDEX.md §"Open contradictions" continues to be MECHANICAL-with-named-inversion across the board. The TapeBuilder cross-reference (item 10) is editorial; the name-the-inversion-from-§1.2 amendment is one cross-reference line.

## §8 Lane findings (selected high-signal rows)

### §8.4 Lane 4 — SOTA Anchoring (the load-bearing lens, empirically validated)

The iteration ratifies Lane 4 — the matrix's thresholds are anchored to competitor + corpus + platform; the BEAT_BOUND construction stands; the gate now fires NO-GO honestly on the current substrate. The matrix is empirically calibrated and producing correct verdicts. Lane 4 honoured.

The sonic-rs anchor numbers in RESULTS.md (18440 / 23075 / 12021 Mbps) versus the SOTA.md canonical anchors are slightly different from the M1 Pro 436 µs / 854 µs / 3.144 ms canonical numbers (which compute to ~14,485 Mbps / ~20,260 Mbps / ~7,158 Mbps). The discrepancy is non-trivial — the measured prototype's sonic-rs row is faster by ~27% on twitter and ~14% on citm, slower by ~40% on canada compared to the SOTA.md table. This may be (a) the prototype uses a different sonic-rs API than SOTA.md's anchor, (b) M1 Pro thermal state difference, (c) PGO disclosure: SOTA.md may have measured a different build. The metadata schema (§5.1) is supposed to record this. Lens H residual: SOTA.md citation in BENCH.md §2.5 / §3.3 is now ~15-40% off from the in-run measurements; the gate uses in-run minimum which is correct per spec, but the static table at §3.3 is now stale.

This is a minor item — the spec already routes the threshold matrix to use the in-run minimum (`S = min(in-run anchors)`), so the stale SOTA.md numbers do not bias verdict. But the spec text at §3.3 should add a one-line note that the SOTA.md anchors are illustrative; the gate uses in-run measurement.

### §8.6 Lane 6 — LOC budget reconciliation (item 11) unchanged

WORKSPACE.md row 9 still caps at 2,000 LOC with ≤500 LOC Track 2; BENCH.md §11.1 cites 2,200 LOC + measurement-driven Track 2; INDEX.md headline still 31,400 LOC. The cross-quadrant fault is unchanged. Amendment dispatch routes through C1 in CONSOLIDATED.

### §8.8 Lane 8 — Carry & Deferral (item 15 PARTIAL)

§9.5 line 1295-1296 reads "The alternate-plan probe (§7.8) bounds only the JSON cost-plan cut; it does not validate the full V1 recognizer miner." This is the cross-ref SK-V1 item 15 substantively requested. The section reference is `§7.8` (the masking-probes parent) rather than `§7.8.2` (alternate-plan probes specifically) — a one-word amendment to sharpen. Treat as PARTIAL-CLOSURE.

## §9 Per-item table (high-signal rows)

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| BENCH.md:1361 (§10.2) | Outcome ID enumeration `<A|B|C|D|E|F|G|H|I|J|K|L>` | Pre-redress; missing F-positive/F-noise/M; stale H. | — | Editorial cohesion fault unchanged from SK-V1. | Editorial. | DISCARD (line-level; replace with current outcome set) |
| BENCH.md:677 (§6.3) | "Outcomes G and H exist..." | Stale post-redress reference (H was collapsed into G). | — | Editorial cohesion fault unchanged from SK-V1. | Editorial. | DISCARD (sentence-level) |
| BENCH.md:614-624 (§6.1 matrix) | F-band classification has no outcome for Track 2 ∈ (S×1.05, S×1.10] AND Track 1 > Track 2 × 1.10 | Substrate borderline-weak + codegen gap is unclassifiable. | — | Falsifiability hole; latent (current run is in G); but spec defect regardless. | Add F-codegen-gap row OR consolidate F-* with Track 1 sub-band in action text. | REINVENT |
| BENCH.md:622 (F-noise rationale) | "criterion noise_threshold(0.02) plus 5% headroom" | Conflates iteration drift with track-to-track ratio. | The redress intent — distinguish positive codegen from noise — is real. | Threshold derivation hand-waved. | Replace with "Track 1 95% CI upper bound overlaps Track 2 × 1.05". | REINVENT |
| BENCH.md:1063-1064 (§7.8.3 primitives) | `__dsb` (not an eviction primitive) + un-strided `_mm_clflush` | Technical-correctness Lens H fault. | — | iCache + dCache + TLB + branch-predictor cooling not addressed. | Replace primitives; add qualifier. | REINVENT |
| BENCH.md:986-987 (Probe A threshold rationale) | "canonically ~10-30 ns; 50 is generous" | Pseudo-precise hand-waved range; measured prototype is 0.7 ns/call. | — | Pseudo-precision unchanged from SK-V1; now also empirically falsifiable. | Replace with measured value + worst-case estimate, or remove the bracketed range. | REINVENT |
| BENCH.md:1073-1075 (§7.8.3 < 1.2× qualifier) | "< 1.2× is suspicious — RESULTS notes the row as inconclusive" | Epistemic guard SK-V1 wanted is present. | But gate writes PASS for current cold/warm ratios of 1.16× / 1.01× / 1.03×; spec says INCONCLUSIVE. | Spec/gate disagreement. | Resolve INCONCLUSIVE-vs-PASS; preferably bring gate in line with spec by reporting inconclusive when ratio < 1.2× per spec. | REINVENT |
| BENCH.md:1298-1316 (§9.6 outcome M) | Peak RSS forward-projection still missing | — | — | 3× threshold may never fire; gate appears ceremonial. | Add the projection (now empirically computable post-iteration: canada tape 2.68 MB logical / 3.57 MB allocated). | REINVENT |
| BENCH.md §10.3 (probability mapping) | No row for cross-platform plan divergence on x86_64 | Item 9 surgery in SK-V1. | — | Empirically SUPERSEDED — alternate_dispatch_table_plan invalidated, alternate_pext_mask_plan unimplemented. No measurement supports a divergence-row prediction. | The SK-V1 surgery is moot; the residual question (PEXT on Intel) defers to V1 H.W2 with the skinny silent. | SUPERSEDED |
| BENCH.md §1.2 | TapeBuilder cross-reference still missing | Item 10 SK-V1 surgery. | — | Named-inversion API contract not cited from BENCH §1.2. | Add cross-reference line to SUBSTRATE §8 + INDEX deviation ledger row 6. | REINVENT (one-line) |
| WORKSPACE.md:36 + :73 ↔ BENCH.md §11.1 | LOC reconciliation unchanged | Item 11 / C1. | — | Cross-quadrant fault. | WORKSPACE row 9 → 3,000-3,500 LOC; INDEX headline → ~32,500-33,000. | REINVENT (cross-quadrant) |
| BENCH.md:996-1006 (§7.8.1 Probe B bands) | "expected delta 5-15% twitter, 3-8% citm, < 2% canada" | Item 12 SK-V1 (editorial); SK-V2 strengthens to Lens L load-bearing (N2) | The two-probe split is FAITHFUL; the bands themselves are not. | Empirically refuted by 22-74% measured gross-time deltas. | Replace bands with empirical MASKING thresholds (>1.15× / >1.08× / >1.02× T1) the gate already uses, OR remove in favour of "this probe is a MASKING bound; V1 must keep decode lazy". | REINVENT (load-bearing) |
| BENCH.md:1013 (§7.8.1 closing sentence) | "V1 JSON must either keep string decode lazy or accept the SOTA hit" | Item N1 — per SK-V2 prompt directive. | The "or accept" hedge is now too weak. | Empirical gross-time deltas of 22-74% are severe; "accept" is no longer a viable alternative. | Replace with "V1 JSON must keep string decode lazy in the substrate/view layer". | REINVENT |
| BENCH.md §7.8.2 paragraph 1 (line 1015-1022) | "confirmatory + plausibly-better candidate" framing | Item N3 — framing partly refuted. | — | Dispatch-table candidate INVALIDATED; PEXT candidate UNIMPLEMENTED; only scalar remains confirmatory. | Reframe as "Confirmatory only — scalar plan; the previously-described dispatch-table alternate was invalidated post-implementation; PEXT mask defers to V1 H.W2 absent skinny-side implementation." | REINVENT |
| BENCH.md §8.3-§8.4 (CI runner discount) | Discount table + runners.toml unchanged | Item 13 / C9. | The steelman: reduces CI false NO-GO. | Adds ~50 LOC of gate logic; every NO-GO requires local re-run anyway; load-bearing role unclear. | Collapse to "CI bench is advisory non-gating; local bench is authoritative"; remove `runners.toml`. | REINVENT |
| BENCH.md §11.1 (LOC budget hedge) | `metadata.rs ≤ 250` / `gate.rs ≤ 350` unchanged | Item 14. | Plausible if the classifier is table-driven. | Tight against the §6.2.1 12-step cascade + matrix-row data + per-corpus rendering. | Hedge to `metadata.rs ≤ 280`, `gate.rs ≤ 400`; reclaim 50 LOC if Lens-I item 13 lands. | REINVENT |
| BENCH.md:1295-1296 (§9.5 cross-ref) | Cross-ref present but section-imprecise (§7.8 not §7.8.2) | Item 15 PARTIAL. | The substantive cross-ref is there. | One-word imprecision. | Sharpen to "§7.8.2 alternate-plan probes". | REINVENT (one-word) |
| RESULTS.md cold_first_parse vs spec | Gate reports PASS at cold/warm < 1.2× | Item N4 (new). | — | Spec at BENCH.md:1073-1075 says INCONCLUSIVE. | Resolve: bring gate in line with spec OR amend spec to allow current behaviour with new rationale. | REINVENT |
| BENCH.md §3.3 vs in-run anchors | SOTA.md canonical numbers diverge from prototype measured by 14-40% | New cohesion observation. | The matrix uses in-run min `S`; the static table is illustrative. | Static table now stale; one-line note suffices. | Add note: "SOTA.md anchors are illustrative; the gate computes S from in-run measurements per §6 notation." | KEEP-with-editorial-note |

## §10 Punch list (ordered surgical edits for SK-V2 amendment dispatch)

The 12 STILL-OPEN items from SK-V1 carry forward, plus the 4 NEW items (N1-N4), plus the editorial note at SOTA.md anchor staleness. The 2 PARTIAL items get a one-word sharpening. Item 9 is SUPERSEDED and removed.

Numbered for SK-V2 amendment dispatch (re-numbered, not preserving SK-V1 numbering):

| # | Site | Surgery | Lens | Owner | Scope | Source |
|---:|---|---|---|---|---|---|
| 1 | BENCH.md:1361 | Replace `<A|B|C|D|E|F|G|H|I|J|K|L>` with `<A|B|C|D|E|F-positive|F-noise|G|I|J|K|L|M>`. | Lane 3 cohesion | BENCH author | Editorial | SK-V1 item 1 |
| 2 | BENCH.md:677 | Replace "Outcomes G and H exist" with "Outcomes G/I/J/K/L/M exist". | Lane 3 cohesion | BENCH author | Editorial | SK-V1 item 2 |
| 3 | BENCH.md:614-624 (§6.1) | Add F-codegen-gap row for Track 2 ∈ (S×1.05, S×1.10] AND Track 1 > Track 2 × 1.10, OR consolidate F-positive/F-noise/F-codegen-gap into single F outcome with Track 1 sub-band reported in action text. Update §6.2.1 classification order accordingly. | Lens M (load-bearing) | BENCH author | Matrix | SK-V1 item 3 |
| 4 | BENCH.md:622 | Replace "criterion `noise_threshold(0.02)` plus 5% headroom" with "Track 1 95% confidence interval upper bound overlaps Track 2 × 1.05" (measurement-driven boundary). | Lens M (load-bearing) | BENCH author | Threshold | SK-V1 item 4 |
| 5 | BENCH.md:1063-1064 (§7.8.3) | Replace `core::arch::aarch64::__dsb` with the correct cache-evict primitive (`dc civac` loop, OR `__clear_cache` syscall + buffer pressure). For x86_64, document the `_mm_clflush` (or `_mm_clflushopt`) 64-byte stride loop over corpus + parser hot-data ranges. Add explicit qualifier: "TLB and branch-predictor state are not cooled by this probe; the cold/warm ratio reported is dCache + iCache delta only". | Lens H | BENCH author | Probe definition | SK-V1 item 5 |
| 6 | BENCH.md:986-987 (§7.8.1 Probe A threshold) | Replace "canonically ~10-30 ns; 50 is generous" with either (a) "M1 Pro virtual call ~5-10 ns; bounds-check + registry lookup adds ~20-40 ns; total per registry call ~30-50 ns; measured prototype: ~0.7 ns/call (inlined release path)" or (b) simply remove the bracketed phrase. Prefer (a) — informative without pseudo-precision. | Lens F | BENCH author | Editorial-with-rationale | SK-V1 item 6 |
| 7 | BENCH.md:1069-1075 (§7.8.3 < 1.2× branch + N4) | Resolve the spec / gate disagreement: either (a) bring the gate in line with the spec ("inconclusive when cold/warm < 1.2×") and verify the SK gate output, OR (b) amend the spec to relax the < 1.2× branch with a measurement-driven exception (e.g., "< 1.2× is suspicious for L3-eviction probes; for instruction-cache-dominated workloads the natural ratio may legitimately be near 1.0× — RESULTS now records the cold-mode primitive in metadata so the row's pass/inconclusive disposition is reproducible"). Preferred: (a). | Lens H + Lens M | BENCH author | Probe definition + gate | SK-V1 item 7 + N4 |
| 8 | BENCH.md §9.6 | Add forward-projection (now empirically computable): "canada tape ~2.68 MB logical / 3.57 MB allocated (per `skinny/RESULTS.md` materialization rows); typed root ~3-5 MB; total ~5.7-8.6 MB; sonic-rs canada peak ~5-7 MB; substrate operates at ~1.1-1.4× sonic-rs. M outcome's 3× threshold is a safety net for substrate drift, not a primary gate." | Lens M | BENCH author | Editorial | SK-V1 item 8 (updated with measurement) |
| 9 | BENCH.md §10.3 | (SUPERSEDED — no surgery; item removed from punch list.) Note in §10.3 that "the alternate-plan probes do not currently bound cross-platform plan divergence: dispatch-table candidate invalidated; PEXT mask unimplemented (V1 H.W2 owns)." | Lens L | BENCH author | Editorial | SK-V1 item 9 → SUPERSEDED + replacement note |
| 10 | BENCH.md §1.2 | Add cross-reference: "Track 2's substrate access is via `TapeBuilder<'a>` per `SUBSTRATE.md` §8 — the named-inversion contract that V1 graduation closes (per `INDEX.md` deviation ledger row 6)." | Lane 1 + Lens N | BENCH author | Cross-ref | SK-V1 item 10 |
| 11 | BENCH.md §11.1 ↔ WORKSPACE.md row 9 ↔ INDEX.md headline | Cross-quadrant LOC reconciliation. WORKSPACE row 9 → 3,000-3,500 LOC (drop "≤500 LOC Track 2 handwritten substrate probe" constraint; cite BENCH §10.6 substrate-API correspondence checklist); INDEX headline → ~32,500-33,000 LOC handwritten. | Lane 6 + Lens N | WORKSPACE + INDEX authors | Cross-quadrant | SK-V1 item 11 / C1 |
| 12 | BENCH.md:996-1006 (§7.8.1 Probe B bands) | Replace the per-corpus "expected delta" bands (5-15% twitter, 3-8% citm, < 2% canada) with measured MASKING thresholds the gate already uses (>1.15× T1 twitter, >1.08× T1 citm, >1.02× T1 canada), OR remove the bands in favour of "this probe is a MASKING bound on parse-time eager string decode; V1 must keep decode lazy per the empirically-falsified bands captured in `skinny/RESULTS.md`". Either form must NOT continue to encode the refuted spec bands. | Lens L (load-bearing) | BENCH author | Threshold | SK-V1 item 12 → N2 (strengthened) |
| 13 | BENCH.md §8.3-§8.4 | Collapse CI runner discount + local override into "CI bench is advisory non-gating; local bench is authoritative". Remove `runners.toml` reference; reclaim ~50 LOC from gate.rs budget. | Lens I | BENCH author | Apparatus reduction | SK-V1 item 13 |
| 14 | BENCH.md §11.1 LOC hedge | Hedge `metadata.rs ≤ 280` (was 250); `gate.rs ≤ 400` (was 350); reclaim 50 LOC if Lens-I item 13 lands. | Lane 6 | BENCH author | Budget hedge | SK-V1 item 14 |
| 15 | BENCH.md:1295-1296 (§9.5 cross-ref) | Sharpen "§7.8" to "§7.8.2 alternate-plan probes". | Lane 8 (carry) | BENCH author | One-word | SK-V1 item 15 (close to closure) |
| 16 (N1) | BENCH.md:1013 (§7.8.1 closing sentence) | Replace "V1 JSON must either keep string decode lazy or accept the SOTA hit" with "V1 JSON must keep string decode lazy in the substrate/view layer; the eager-decode rows in `skinny/RESULTS.md` show 22-74% gross-time penalty across the three corpora, refuting the original expected-delta bands". | Lens L (load-bearing) | BENCH author | Threshold-and-premise | SK-V2 prompt directive + N1 |
| 17 (N3) | BENCH.md §7.8.2 paragraph 1 (lines 1015-1022) | Reframe from "confirmatory + one plausibly-better candidate (PEXT)" to "Confirmatory only — scalar plan; the dispatch-table alternate was empirically invalidated (false-win duplicate, then real implementation regressed); the PEXT mask alternate defers to V1 H.W2 absent skinny-side implementation". Update §15 line 1780 scope-summary row similarly. | Lens H + Lens L | BENCH author | Framing | SK-V2 prompt directive + N3 |
| 18 | BENCH.md §3.3 (illustrative anchors) | Add one-line note: "SOTA.md anchors above are illustrative against the M1 Pro baseline; the gate computes `S` from in-run measurements per §6 — `skinny/RESULTS.md` currently records sonic-rs at 18440 / 23075 / 12021 Mbps across twitter / citm / canada, differing from the static table by 14-40% (run conditions per metadata schema §5.1)." | Lens H | BENCH author | Editorial | New (SOTA.md anchor staleness vs in-run) |

Total: 17 active items (item 9 SUPERSEDED is logged as the comment-only entry but routes no surgery). Cross-quadrant items: 1 (item 11; routes through C1 in CONSOLIDATED + WORKSPACE + INDEX). The remainder are BENCH-side.

## §11 Lane verdict line totals (post-iteration)

- Lane 1 (Lock-Adherence): 11 KEEP, 1 REINVENT (TapeBuilder cross-ref unchanged from SK-V1).
- Lane 2 (Sequencing): N/A.
- Lane 3 (Cohesion): 0 REINVENT, 2 DISCARD (line-level editorial — same items as SK-V1).
- Lane 4 (SOTA Anchoring): 8 KEEP, 1 REINVENT (F-noise rationale unchanged). The matrix is empirically validated as honest NO-GO-firing.
- Lane 5 (Grammar-Authoritative): honoured.
- Lane 6 (LOC Budget): 5 KEEP, 2 REINVENT (item 11 cross-quadrant; item 14 hedge).
- Lane 7 (Friction): honoured.
- Lane 8 (Carry/Deferral): 7 KEEP, 1 REINVENT (cross-ref imprecision — close to closure).
- Lane 9 (Greenfield): honoured.
- Lens F: 1 REINVENT (Probe A pseudo-precision — now empirically falsifiable).
- Lens G: KEEP (CSS prior probe — no change).
- Lens H: 2 REINVENT (cold-cache primitives + N4 spec/gate disagreement); 1 closure (dispatch-table invalidation now documented at §7.8.2).
- Lens I: 1 REINVENT (CI runner discount unchanged).
- Lens J: honoured.
- Lens K: honoured.
- **Lens L**: 1 SUPERSEDED (item 9 x86_64 plan divergence); 1 LOAD-BEARING REINVENT (item 12 + N2 Probe B bands empirically refuted); 1 REINVENT (N1 premise commitment strengthening); 1 REINVENT (N3 §7.8.2 framing); 1 KEEP (host-call probe two-axis split, empirically ratified — dispatch passes, eager decode FAILS as predicted).
- **Lens M**: 1 LOAD-BEARING REINVENT (item 3 classification gap; latent on current run but spec defect); 1 LOAD-BEARING REINVENT (item 4 F-noise rationale); 1 REINVENT (item 8 peak RSS projection — now empirically computable); 1 REINVENT (N4 cold-cache spec/gate disagreement); 1 LOAD-BEARING CLOSURE (matrix has produced a NO-GO and was measured against — empirically validated).
- **Lens N**: honoured.

Counts: ~24 KEEP, ~14 REINVENT, 2 DISCARD (line-level editorial), 1 SUPERSEDED.

KEEP fraction: ~60% — within 60-80% target band; slightly toward the contested end (the iteration produced strong measurement evidence that the SK-V1 audit could not yet draw on, and the spec text has not yet been amended against the SK-V1 punch list).

## §12 Cross-quadrant impact

The SK-V2 BENCH punch list of 17 active items has ONE cross-quadrant surgery (item 11 — LOC reconciliation across BENCH ↔ WORKSPACE ↔ INDEX). The other 16 are BENCH-only edits. The cross-quadrant CONSOLIDATED-SK-V1 punch list items C2-C5, C7-C9, C11-C12 are BENCH-internal (per the CONSOLIDATED §5 disposition table); SK-V2 BENCH inherits those plus the four new items N1-N4. CONSOLIDATED-SK-V2 must absorb:

- C1 (BENCH item 11): unchanged from SK-V1; still open.
- C6 (TapeBuilder cite from BENCH §1.2): unchanged; still open.
- C7 (cross-platform plan divergence in §10.3): SUPERSEDED at BENCH-side; CONSOLIDATED should reflect SUPERSEDED disposition.
- C8 (Probe B band rationale): UPGRADED in SK-V2 BENCH from "editorial ambiguity" to "Lens L load-bearing empirical refutation". CONSOLIDATED should reflect the upgrade.
- New for CONSOLIDATED-SK-V2: N1 (premise commitment), N3 (§7.8.2 framing), N4 (cold-cache spec/gate disagreement), and BENCH-side empirical re-grounding of N2 / item 12.

## §13 Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The iteration delivered a critical Lens H closure (the alternate_dispatch_table_plan probe is now documented as INVALIDATED at BENCH.md §7.8.2 line 1029) and a critical Lens M validation (the matrix produced a verdict G / NO-GO on the regenerated full run, proving the matrix is empirically falsifiable rather than confirmation-biased theatre). These two closures are load-bearing; the matrix is now in production and the dispatch-table false-win is recorded.
>
> However, the BENCH.md spec itself was not amended against the 15-item SK-V1 punch list — 12 items remain STILL-OPEN (1, 2, 3, 4, 5, 6, 8, 10, 11, 12, 13, 14), 2 items are PARTIAL (7, 15), 1 item is SUPERSEDED by the iteration's empirical invalidation (9), and the iteration surfaces 4 NEW items (N1-N4) that the SK-V1 audit could not yet anticipate.
>
> The new items are dominated by Lens L: the §7.8.1 Probe B expected-delta bands (5-15% / 3-8% / < 2%) are empirically refuted by the measured 22-74% gross-time deltas in `skinny/RESULTS.md`; the §7.8.1 closing sentence's "V1 JSON must keep decode lazy **or** accept the SOTA hit" disjunction is too weak given the data and must read "must keep lazy" per the SK-V2 prompt directive; the §7.8.2 "confirmatory + one plausibly-better candidate" framing has lost its supporting candidates (dispatch-table invalidated; PEXT unimplemented) and should reframe to "confirmatory only — scalar plan". One Lens H + Lens M item (N4) surfaces a spec/gate disagreement on the cold-cache < 1.2× INCONCLUSIVE-vs-PASS branch that must be resolved alongside the SK-V1 item 5 primitive correctness amendment.
>
> Lens M (the load-bearing falsifiability lens) returns **honoured-with-narrow-amendment with empirical validation** — the matrix has produced a NO-GO; the substrate gap is honestly accounted for; the residual classification-gap (item 3) is latent on the current run but remains a spec defect; the F-noise rationale (item 4) and peak RSS projection (item 8) still need sharpening; the cold-cache spec/gate contradiction (N4) is now visible from RESULTS.md.
>
> Lens L (premise fidelity) returns **AMENDMENT-REQUIRED-NARROW** — the iteration's strongest evidence is that the JSON skinny CAN keep substrate `@host fn` dispatch overhead near zero (Probe A passes at 0.7 ns/call across all three corpora) AND CANNOT keep parse-time eager decode within SOTA budget (Probe B fails by 22-74% gross-time across all three corpora). The two-axis host-call split delivered exactly the disambiguation it was designed for; the spec's bands and framing must now align with the measurements.
>
> Lens H (provenance) returns **honoured on the dispatch-table invalidation axis; still amendment-required-narrow on the cold-cache primitive correctness axis**. The dispatch-table closure is the iteration's load-bearing Lens H win; the cold-cache primitive amendment did not land in this iteration.
>
> Hereupon: dispatch the SK-V2 BENCH amendment agent with the 17-item §10 punch list (item 9 SUPERSEDED, no surgery). The cross-quadrant reconciliation (item 11) routes through C1 in CONSOLIDATED-SK-V2; the other 16 items are BENCH-only. After SK-V2 amendments land, BENCH.md re-runs through SK-V3 verification before SKINNY-SUITE consolidation. The iteration delivered the load-bearing evidence the matrix needed; the BENCH.md spec must now align with what the gate measures.
>
> The skinny remains a defensible prior-validation device under SK-V2: the matrix is honest, the host-call split is empirically calibrated, the dispatch-table false-win is buried. The remaining work is mechanical alignment of the spec text with the measurement record.

### Critical files for implementation (of the amendment dispatch)

- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md` — primary surgery target for items 1-10, 12-15, 16 (N1), 17 (N3), 18 (SOTA staleness note).
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/WORKSPACE.md` — row 9 + line 73 LOC reconciliation (item 11 / C1).
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md` — headline LOC total recompute (item 11 / C1).
- `/Users/mkbabb/Programming/bbnf-lang/skinny/RESULTS.md` — empirical reference for items 6, 8, 12, 17 (N3); the gate code that produces RESULTS.md is the authority on item 7 (N4) cold-cache pass-line resolution.
- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md` — iteration log; the authoritative narrative for items 9 (SUPERSEDED), 12 (N2), 17 (N3) re-framings.
