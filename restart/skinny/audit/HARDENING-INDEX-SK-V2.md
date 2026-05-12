# HARDENING-INDEX-SK-V2 — INDEX Quadrant Audit (Post-Iteration)

## §1 — Target identification

- **Path:** `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md`
- **Lines audited:** 1–86 (post-iteration; +1 line over SK-V1's 84-line audit)
- **Cycle:** SK-V2 (verify-then-rerun after SK-V1 returned `SK-AMENDMENT-REQUIRED-NARROW` and the iteration landed two false-route invalidations plus a host-call MASKING split)
- **Lens stack applied:** Lanes 1, 3–9 (Lane 2 N/A — single-wave) + Lens F + Lens G + Lens H + Lens I + Lens J + Lens K + Lens L + Lens M + Lens N (Lens A is load-bearing for INDEX as cross-quadrant ratifier per HARDENING-SKINNY §3 / §4)
- **Cross-quadrant authorities consulted:** `restart/skinny/SUBSTRATE.md` §1.2 lines 73–101, §3.6 line 317, §8 lines 537–551; `restart/skinny/COMPILER.md` §1.3 lines 94–127, §3.2 line 219, §5.3 lines 408–416, §10 line 705; `restart/skinny/BENCH.md` §6 lines 605–714, §7.8.1 lines 981–1014, §7.8.2 lines 1024–1046, §9.6 lines 1298–1312, §10.3 lines 1474–1488; `restart/skinny/WORKSPACE.md` §8.1 lines 535–544, §10 line 588; `restart/ARCHITECTURE.md` lines 1420–1438; `skinny/REDRESS.md` items 17, 18, 19; `skinny/RESULTS.md` masking probe table; `restart/skinny/audit/HARDENING-INDEX-SK-V1.md`; `restart/skinny/audit/HARDENING-CONSOLIDATED-SK-V1.md` items C14, C15, C19.
- **Iteration bench fact:** Outcome G / NO-GO across all three corpora (twitter Track 2 / sonic = 56.9 %; citm = 53.0 %; canada = 64.0 % per RESULTS.md). Substrate ceiling at ~0.55–0.65× sonic, not 1.6× as cited in the task framing; the gap is the inverse — substrate runs at ~60 % of sonic, equivalent to sonic running at ~1.6× substrate.
- **Time consumed at this commit point:** ~24 minutes of 35-minute budget.

INDEX's load-bearing function is COHERENCE between four sister quadrants and the deviation-ledger's MECHANICAL classification. The SK-V1 audit returned `SK-AMENDMENT-REQUIRED-NARROW` against thirteen punch-list items (C14 Lock 14 surface; C15 BEAT_BOUND threshold preview; C19 single-plan extraction plurality being the consolidated audit's INDEX-relevant items). The intervening iteration has both **partially landed** some redresses **and** introduced new MECHANICAL classification surfaces that INDEX has not absorbed.

The dominant Lens A finding under SK-V2 is **selective propagation**: some SK-V1 punch-list items (the new ledger row for sealing wording; the new threshold-preview row 4 for memory/correctness NO-GO) landed at INDEX, while others (C15 BEAT_BOUND; C19 plurality; F-band protocol drift) remain untouched. The iteration further introduced a *substrate ceiling* finding — confirmed at Track 2 / sonic ≈ 0.60 — that demands a new deviation-ledger row naming the V1 closure under Lock 1 amendment, which neither INDEX nor any sister quadrant has yet absorbed.

The combination of unfinished SK-V1 propagation + missing Lens-N row for the iteration's empirical finding is the SK-V2 verdict driver.

---

## §2 — Cohort verdict

| Lane / Lens | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock-adherence | partial-redress | 5 | 2 | 0 | SK-V1 C14 (Lock 14 surface count silent in Cross-Quadrant Invariants) NOT redressed. No "Onboarding contract: two surfaces (`json.bbnf` + workspace metadata)" line added to §"Cross-quadrant invariants". Lock 1 *amendment surface* for the substrate ceiling (~60 % sonic) not raised by INDEX even though REDRESS.md item 19 explicitly names "a lazy lowering amendment" path. |
| Lane 2 — Sequencing | N/A | – | – | – | Single-wave skinny per HARDENING-SKINNY §4. |
| Lane 3 — Cohesion (Lens A; load-bearing for INDEX) | partial-redress | 4 | 4 | 0 | **The dominant SK-V2 fault.** Three SK-V1 redress items landed at INDEX (threshold-preview row 4 for correctness/memory NO-GO at line 26; ledger row 6 wording shift to "private-Vec semantic sealing" at line 66; one-grammar / host-fn-free clause carried forward at line 48). Three SK-V1 redress items did NOT land at INDEX: (a) **C15** — the threshold preview at line 23 still uses `S × 0.95` instead of `BEAT_BOUND = min(S × 0.95, T_README)`; (b) **C19** — line 37 *and* line 51 both retain "alternate-plan stub" (singular) when BENCH §7.8.2 carries three named alternates with `alternate_dispatch_table_plan` now invalidated and `alternate_pext_mask_plan` carrying the plausibly-better-on-x86_64 verdict; (c) the iteration's invalidation of `alternate_dispatch_table_plan` (REDRESS item 17) and `12-byte token` (REDRESS item 18) is not reflected in INDEX's "What the skinny is NOT testing" cell or in the cross-quadrant invariants. The third miss is the largest Lens A defect because REDRESS.md item 17 ratifies that the "dispatch-table is *not* a cost-model masking signal," which means INDEX's invariant 4 — citing the alternate-plan stub as the bench-recoverable signal — now leans on a probe set the iteration has narrowed from three to two distinct probes (scalar + PEXT). |
| Lane 4 — SOTA anchoring | honoured-with-residue | 1 | 1 | 0 | S-definition at line 28 unchanged. But the iteration's empirical fact — Track 2 / sonic ≈ 0.60 across all corpora — is the **first measured signal** that the V1 substrate as currently specified runs at ~1.6× the fastest competitor's wall time. INDEX line 19 SOTA-viability premise text says "lands within or beats the sonic-rs / simd-json envelope"; the iteration's RESULTS.md row says the substrate misses that envelope by ~40 %. INDEX is silent on what this signal does to the SOTA-beat probability narrative the document closes with at line 86 ("update the SOTA-beat probability with measurement evidence"). The probability is now low — REDRESS.md positions the route forward as "lazy-offset tape replacement" — and INDEX does not yet name that route, leaving the SOTA narrative un-anchored against the iteration evidence. |
| Lane 5 — Grammar-authoritative discipline (Lock 14 deep-dive) | honoured | 1 | 0 | 0 | INDEX still contains no `match grammar { Json => … }` arms. |
| Lane 6 — Generated-LOC budget | honoured-with-residue | 1 | 1 | 0 | INDEX headline at line 3 still cites `~31,400 handwritten LOC + ≤4,000 generated LOC`. SK-V1 consolidated audit item **C1** required INDEX headline → ~32,500–33,000 LOC after the Track 2 LOC cap drop at BENCH §11.1. Not propagated. WORKSPACE row 9 also still stale. The discrepancy is small but **directly hits Lens A** because INDEX is the integration document; if WORKSPACE has 3,000–3,500 LOC for bbnf-bench and BENCH has 2,200 + Track 2 800–1,500 + CSS prior 600 = up to 4,300, the INDEX headline understates by 1,000–1,500 LOC. |
| Lane 7 — Friction forecast | partial-redress | 1 | 1 | 0 | SK-V1's preview-vs-matrix mismatch (the reader hits "Track 2 ≤ S × 0.95" in INDEX preview, then BENCH §6 says `BEAT_BOUND = min(S × 0.95, T_README)`) **persists in SK-V2**. The note SK-V1 surgery #1 requested at line 27 — "*The preview is indicative. The live gate uses BEAT_BOUND per BENCH §6, which is the stricter of S × 0.95 and the README target T_README.*" — was not added. First-time-reader pathology unmitigated. |
| Lane 8 — Carry & deferral | partial-redress | 4 | 1 | 0 | "What the skinny is NOT testing" still cites singular alternate-plan stub at line 37; the iteration narrowed the live probe set from three to two (scalar + PEXT; dispatch-table INVALID). The cell's V1 owner column (H.W2/H.W3) is correct but the bench-recoverable signal cited here is now post-redress two-probe shape, not the singular-stub shape INDEX implies. |
| Lane 9 — Greenfield discipline | honoured | 1 | 0 | 0 | Voice neutral; no legacy-defence text. |
| Lens F — LLM bias | partial-redress | 1 | 2 | 0 | SK-V1 punch item #11 — drop "6-12 months" pseudo-precise wall-time from line 85 — NOT redressed. Closing sentence still reads "before the V1 plan commits 6-12 months of tranche execution." Also SK-V1 punch item #12 — "2-4 weeks" at line 3 — not provenance-cited either; the wall-time provenance is still vapor. |
| Lens G — Overfitting | honoured | 0 | 0 | 0 | Single-grammar skinny is a deliberate cut, not overfit. |
| Lens H — Hallucination + provenance | honoured | 1 | 0 | 0 | All quadrant citations resolve correctly under the post-iteration line numbers. Verified: SUBSTRATE.md §1.2 (lines 73–101) is "Tape<'input> — owning token stream + payload arena"; COMPILER.md §1.3 (line 94) is "`@host fn` decision"; COMPILER.md §4.4 / §9.1 — verified; WORKSPACE.md §2.1 — verified; WORKSPACE.md §3 line 179 — verified. |
| Lens I — Contrivance | honoured | 0 | 0 | 0 | INDEX structurally minimal — 86 lines of mostly-tabular content. |
| Lens J — Host-language leverage | N/A | – | – | – | INDEX delegates to quadrants. |
| Lens K — Meta-grammar discipline | honoured | 0 | 0 | 0 | No embedded grammar apparatus. |
| **Lens L — Premise fidelity** | partial-redress with new MASKING surface | 4 | 2 | 0 | The redress's biggest empirical fact — **host_call_eager_decode is MASKING on all three corpora** (RESULTS.md: 57.6 % on twitter; 77.2 % on citm; 81.9 % on canada; signal column "MASKING" on all three) — has a partial INDEX surface via cross-quadrant invariant 1 ("BENCH must bound the direct-decode vs `CallHost` registry dispatch delta before RESULTS can claim FAITHFUL"). But the iteration evidence resolved that question: dispatch is fine; eager decode is MASKING. INDEX invariant 1 still phrases the bound as a *future obligation* on BENCH (forward-looking conditional) rather than as a *settled empirical finding* (the eager-decode MASKING result already exists in skinny/RESULTS.md). Cross-quadrant ratifier function compromised — the iteration has already classified host-fn-free as conditionally-FAITHFUL-via-lazy-decode-only and INDEX has not absorbed that. |
| **Lens M — Falsifiability** | partial-redress | 4 | 2 | 0 | SK-V1 punch item #3 — extend threshold-preview row 3 to enumerate G/I/J/K/L/M as separate NO-GO drivers — **PARTIALLY landed** as line 26's new row: "Parity oracle fail, SIMD parity hash fail, schema fail, or peak RSS > 3× competitor on canada → Correctness / instrumentation / memory failure → NO-GO or INVALID per BENCH.md §6". This is good redress; it brings the preview's NO-GO surface up from 1 outcome (substrate gap) to 5 outcomes (substrate gap, parity oracle fail, SIMD parity hash fail, schema fail, peak RSS). **But:** SK-V1 punch item #10 (decision-protocol step 11 enumerates "reopen Lock 1 or COMPILER §3 per which delta failed") is NOT redressed; step 11 at line 84 still reads "reopen Lock 1 (substrate) or COMPILER §3 (extraction) per which delta failed" with no per-outcome lever. Steps 6, 9 likewise unredressed. |
| **Lens N — Graduation mechanicality** (load-bearing for the deviation ledger) | honoured-with-narrow-amendment-required | 6 | 2 | 0 | All seven existing ledger rows close MECHANICAL under steelman. **But:** the iteration introduced two new MECHANICAL surfaces that INDEX has not yet placed in the ledger: (a) **the lazy-tape / lazy-decode amendment surface** named in REDRESS.md item 19 ("the skinny remains faithful only for a V1 JSON path that keeps string decode lazy in the substrate/view layer; a parse-time `decode_json_string_to_arena` grammar needs an explicit SOTA concession or a lazy lowering amendment"); (b) **the substrate-ceiling Lock 1 amendment surface** — the iteration's empirical fact that Track 2 / sonic ≈ 0.60 across all corpora establishes that the V1 substrate as specified (tape + direct-to-struct + private-Vec sealing + close-token elision + 16-byte aligned tokens + structural index) hits a ceiling at ~1.6× sonic-rs wall time. REDRESS.md item 18 + the "Next No-Workaround Work" item 2 ("measure a token-capacity estimator or chunked `TapeBuilder` that reduces allocated tape bytes") + the "Sonic Closeness" paragraph (line 195–212) all point at the same remaining route: a **lazy-offset tape** (vs the eager-emit close-token-elided private-Vec-sealed canonical tape). INDEX ledger does not name either amendment surface. |

**Final decision: SK-AMENDMENT-REQUIRED-NARROW.**

Three drivers:

1. **Selective SK-V1 propagation** — C14 not landed; C15 not landed; C19 not landed; SK-V1 punch items 1, 4, 7, 8, 10, 11, 12 not landed; SK-V1 punch items 3 (partial) and 9 (private-Vec wording at row 6) did land. The propagation pattern is non-arbitrary: every item that required text in INDEX's deviation ledger landed; every item that required text in the threshold preview, decision protocol, or invariants table did NOT land. This suggests the SK-V2 author redressed the ledger as a unit while leaving the user-facing surfaces (preview, protocol) for later.
2. **Iteration's empirical findings not absorbed** — the iteration's three load-bearing facts (eager-decode MASKING; dispatch-table probe INVALID; ~60 % sonic ceiling on the canonical substrate) have not been folded into INDEX's invariants, ledger, or preview. Cross-quadrant ratifier function partially broken.
3. **Missing Lens N row for the iteration's empirical Lock 1 amendment surface** — REDRESS.md item 19 + the "Sonic Closeness" closeness narrative explicitly name a V1 amendment surface ("lazy-offset tape replacement" / "lazy lowering amendment for parse-time decode"); INDEX has not absorbed the row. This is the largest single-item Lens-N gap because the iteration evidence has *raised the bar* on what counts as MECHANICAL graduation: it is no longer "additive code only" (the original Lens N FAITHFUL criterion); it is now "additive code that, when applied, closes the ~40 % substrate gap the eager canonical path hits." That changes the Lens-N steelman question from "does graduation cost more LOC than skinny" to "does graduation actually close the measured gap" — a *stricter* mechanicality test the original Lens N taxonomy did not anticipate.

Items 1 and 2 are routine narrow-amendment surgery. Item 3 is the question whether SK-V2 needs to introduce a *new verdict class* (or sharpen Lens N) to handle "MECHANICAL but throughput-load-bearing" deviations. I argue below that the existing taxonomy can absorb it via a "MECHANICAL with named inversion + perturbation-gated" verdict, without needing a new class.

KEEP-without-challenge fraction: ~57 % (target 60–80 %). The audit hit non-trivial challenges across Lanes 1, 3, 4, 6, 7, F, L, M, N. The Lane 3 / Lens A coverage of partial-redress patterns is the audit's deepest finding.

---

## §3 — Lane 1 — Lock adherence (per-item) (SK-V2 scope: deltas only)

The SK-V1 audit's seven Lane-1 rows close as in SK-V1 (KEEP × 6, REINVENT × 1 — the row-1/row-7 ledger duplication). SK-V2 adds:

| Site | Item | Steelman | Verdict |
|---|---|---|---|
| INDEX.md:48 (invariant 1) | Cross-quadrant invariant 1's clause "Because V1 JSON has numeric/string host fns, BENCH must bound the direct-decode vs `CallHost` registry dispatch delta before RESULTS can claim FAITHFUL." | The clause was correct under SK-V1 (BENCH had probes pending). Post-iteration, the probes have *returned signal*: dispatch overhead PASSES (≤ 50 ns; RESULTS.md row 0.73 ns/iter); eager-decode FAILS as MASKING (>1.15× T1 on twitter; >1.08× T1 on citm; >1.02× T1 on canada). The forward-looking conditional has resolved: the host-fn-free cut is conditionally-FAITHFUL **only if** V1 keeps decode lazy. INDEX still presents this as future obligation. **Steelman:** is the iteration result a settled finding INDEX should ratify, or a probe result that BENCH owns and INDEX correctly delegates to? **Counter-steelman:** INDEX is the cross-quadrant ratifier per HARDENING-SKINNY §3 line 41; if a finding upgrades a conditional to MASKING-conditional-on-V1-decision, INDEX ratifies that resolution. Otherwise INDEX text drifts behind BENCH ground truth. | **REINVENT-narrow** — invariant 1 should read: "Because V1 JSON has numeric/string host fns, BENCH probes have measured the split: `host_call_dispatch_overhead` passes (≤ 50 ns/call); `host_call_eager_decode` is MASKING on all three corpora (skinny/RESULTS.md). Host-fn-free is conditionally-FAITHFUL only for a V1 JSON path that keeps string decode lazy in the substrate/view layer; an eager-decode V1 grammar must treat this as a MASKING signal and amend the SOTA expectation." |
| INDEX.md:51 (invariant 4) | Cross-quadrant invariant 4 cites BENCH's alternate-plan stub (singular) as the bench-recoverable signal for the cost-model cut. | The iteration narrowed the live probe set: scalar plan reports (signal "reported" not pass/fail); dispatch-table is INVALID (REDRESS item 17 / RESULTS.md "INVALID duplicate-probe disabled"); PEXT is missing on the M1 Pro arm. Of the original three alternates, **only one (scalar) is alive on the test platform** and PEXT is platform-conditional. INDEX invariant 4's "small alternate-plan stub" understates this narrowing. C19 from the consolidated audit explicitly flagged this. | **REINVENT-narrow** — invariant 4 should read: "Single-plan extraction. No CSP, no e-graph, no cost-model selection. COMPILER.md §5.3. BENCH carries alternate-plan probes (BENCH §7.8.2: scalar — reported, dispatch-table — invalidated per skinny/REDRESS.md item 17, x86_64 PEXT — plausibly-better; aarch64 measurement currently shows only scalar live) to bound whether this cut masks JSON throughput cost." |

**Lane 1 SK-V2 delta verdict:** partial-redress. Two REINVENT-narrow surgeries surface (invariants 1 and 4). The SK-V1 Lane-1 row-1/row-7 reconciliation REINVENT remains unaddressed; see §6 below.

---

## §4 — Lane 3 — Cohesion (Lens A: cross-document narrative coherence — load-bearing for INDEX)

This is the dominant Lane for INDEX under SK-V2. The propagation pattern is partial.

### §4.1 — Threshold preview at INDEX lines 21–26: what propagated and what did not

**What did propagate (KEEP):** Line 26 — the new fourth row covering correctness/memory NO-GO. The cell reads: "Parity oracle fail, SIMD parity hash fail, schema fail, or peak RSS > 3× competitor on canada → Correctness / instrumentation / memory failure → NO-GO or INVALID per BENCH.md §6; do not dispatch from throughput rows." This closes SK-V1 punch item #3 (extend preview row 3 to enumerate G/I/K/L/M as separate NO-GO drivers) at the preview level. The "or peak RSS > 3× competitor on canada" clause specifically captures outcome M (BENCH §6.1 line 628). The "Parity oracle fail" captures outcome I (line 622). "SIMD parity hash fail" captures outcome K (per BENCH §6 — verified). "Schema fail" captures outcome J (INVALID). Per HARDENING-SKINNY §5 Lens M ("the matrix must contain at least one NO-GO outcome that the skinny could plausibly land in"), the preview now carries five NO-GO surfaces (substrate gap + four correctness/memory). Falsifiability via the preview is upgraded.

**Steelman:** does the new row 4 collapse outcomes I, J, K, M into one "Correctness / instrumentation / memory failure" cell when BENCH §6.1 keeps them as separate outcomes with different dispatch actions? Yes — but the preview is *previewing*, not adjudicating. INDEX line 28 explicitly says "Full matrix in `BENCH.md` §6." The collapse is honest at preview-scale. **KEEP**.

**What did not propagate (REINVENT):** Line 23 — the first cell still reads "Track 2 ≤ S × 0.95 AND Track 1 ≤ Track 2 × 1.10". SK-V1 punch item #1 demanded `Track 2 ≤ BEAT_BOUND` with a `BEAT_BOUND = min(S × 0.95, T_README)` note above the table. **Not landed.**

Consequence: a reader hitting the preview alone ratifies a Track 2 = 400 µs on twitter as outcome A. But BENCH §6.4 line 691 explicitly: `BEAT_BOUND = min(S × 0.95, T_README) = min(403 µs, 380 µs) = 380 µs`. A Track 2 of 400 µs on twitter under BENCH §6 is *not* outcome A; it's parity-not-beat (outcome C or D). The preview is *softer than the live gate* by 23 µs on twitter, 39 µs on citm, and 187 µs on canada. This is the exact pathology Lens F flags as "verbal complexity hiding semantic ambiguity."

Per the iteration evidence: this matters less than under SK-V1 because the iteration's measured Track 2 ≈ 0.60 sonic is far below either threshold; no plausible amendment in the current corpus closes a 40 % gap to within either bound. But INDEX's text drift behind BENCH is still a Lens-A fault — the preview is reading the BENCH wrong on a hypothetical that the next amendment might land.

**Verdict for line 23:** **REINVENT** — same as SK-V1 surgery #1. The fact that the iteration evidence makes the gap currently moot does not excuse INDEX's text from matching BENCH's gate definition.

**What partially propagated:** Line 24 — the second cell still collapses outcomes C, D, E into one row. SK-V1 punch item #2 demanded an appended note "(detailed split: outcomes C, D, E in BENCH §6.1; SOTA-beat probability per BENCH §10.3)". Not landed. **REINVENT** — same surgery as SK-V1.

### §4.2 — "What the skinny is NOT testing" table at lines 32–42: alternate-plan plurality

| Site | Surgery | Verdict |
|---|---|---|
| INDEX:37 | The cell reads "Skinny pre-selects one canonical plan and bounds that cut with a non-egraph alternate-plan stub bench" — singular. The iteration explicitly invalidated `alternate_dispatch_table_plan` (REDRESS item 17 / RESULTS.md "INVALID duplicate-probe disabled; real function-pointer table regressed") and the PEXT plan is missing on the M1 Pro test platform (RESULTS.md "n/a missing"). Of the three named BENCH §7.8.2 alternates, only `alternate_scalar_plan` is alive on the current bench (twitter 48.3 %; citm 61.8 %; canada 47.9 % vs Track 1; "reported" signal column, not pass/fail). The singular-stub language now matches *what is alive* on M1 Pro, by accident of the iteration's invalidation of the other two. **Steelman:** has the iteration ironically *vindicated* INDEX's singular phrasing? **Counter-steelman:** No — BENCH §7.8.2 still defines three alternates; the spec authority is still plural. INDEX's "non-egraph alternate-plan stub bench" reads as "BENCH carries one stub", which understates the BENCH spec contract regardless of which probes the iteration happens to have alive. C19 from the consolidated audit holds. | **REINVENT** — cell should read: "BENCH §7.8.2 carries three alternate-plan probes (scalar, dispatch-table — invalidated per skinny/REDRESS.md item 17, x86_64 PEXT — plausibly-better) to bound the cut; current M1 Pro run reports scalar only." |

### §4.3 — Cross-quadrant invariants at lines 47–53: invariants 1 + 4 retreatment

(Verdicts above in §3.)

### §4.4 — Headline at INDEX line 3: LOC reconciliation

Per consolidated audit C1: WORKSPACE row 9 should grow to 3,000–3,500 LOC; INDEX headline should grow to ~32,500–33,000 LOC. Neither landed. INDEX line 3 still cites "~31,400 handwritten LOC + ≤4,000 generated LOC." Verdict: **REINVENT-narrow** — bring headline to ~32,500–33,000 LOC and cite the Track 2 measurement-driven shift at BENCH §11.1 + CSS prior optional ≤ 600 LOC at BENCH §9.1.

### §4.5 — Closing sentence at INDEX line 85: pseudo-precise wall-time

SK-V1 surgery #11 demanded dropping "6-12 months" or citing a wall-budget source. Closing sentence still reads "before the V1 plan commits 6-12 months of tranche execution." Not landed. **REINVENT** — same surgery as SK-V1.

### §4.6 — Lane 3 SK-V2 verdict

Partial-redress. KEEP × 4 (the new row 4 on correctness/memory; the ledger row 6 wording shift; voice neutrality; invariant 3 cite). REINVENT × 4 (line 23 BEAT_BOUND; line 24 split note; line 37 plurality; line 85 wall-time). The pattern: redress landed where the SK-V2 author was already editing the deviation ledger and the preview's new NO-GO row; redress did NOT land where the SK-V2 author would have had to touch the threshold-preview semantics or the closing narrative. This is a propagation discipline issue, not an architectural failure.

---

## §5 — Lane 4 — SOTA anchoring + Lane 6 — Generated-LOC budget (SK-V2 deltas)

### §5.1 — SOTA narrative line 19: the iteration's empirical signal

INDEX line 19: "**The SOTA-viability premise**: if a JSON parser generated through the V1 substrate (tape + direct-to-struct + structural SIMD scan) lands within or beats the sonic-rs / simd-json envelope on twitter / citm / canada, the V1 architectural premise is validated for JSON-class grammars."

The iteration's empirical fact: Track 2 / sonic ≈ 0.55–0.65 across all three corpora. The substrate as currently specified misses the sonic envelope by ~40 %. The skinny has produced the falsifiable measurement Lens M demanded.

INDEX is silent on what this signal means. The closing sentence at line 85 still frames the skinny as a probability-update device looking forward to a SOTA-beat hypothesis, but the document carries no acknowledgement that the first measurement returned a falsifying signal.

**Steelman:** INDEX is the spec authority, not the RESULTS log. The empirical signal lives at `skinny/RESULTS.md` and the iteration narrative at `skinny/REDRESS.md`. INDEX's job is to specify the cross-quadrant invariants under which a *future* measurement updates V1 SOTA-beat probability; it is not the job of INDEX to absorb each measurement run.

**Counter-steelman:** INDEX is the cross-quadrant ratifier. The iteration produced two findings that meet the bar for ratification:
1. **Substrate ceiling at ~1.6× sonic-rs wall time** under the canonical specification (private-Vec sealed; close-token elided; 16-byte tokens; eager pair-token emission). REDRESS.md item 18 + "Sonic Closeness" line 195–212 + "Next No-Workaround Work" name the remaining lever as "lazy-offset tape" / "chunked TapeBuilder."
2. **Host-fn-free FAITHFUL conditional on lazy decode.** REDRESS.md item 19 ratifies that dispatch is fine but eager parse-time decode is MASKING.

Both findings are spec-authority-grade — they constrain what V1 graduation can look like under Lock 1 (substrate) and Lock 5 (Backend). INDEX should absorb them as new ledger rows under §"Open contradictions and skinny-specific deviations from V1" with V1 closure paths named.

**Verdict:** **REINVENT** — add two new ledger rows. See §6 below for proposed surgery.

### §5.2 — LOC headline (covered in §4.4 above; REINVENT-narrow)

### §5.3 — Lane 4 + 6 SK-V2 verdict

honoured-with-residue. KEEP × 1 (S-definition unchanged, correct). REINVENT × 2 (SOTA narrative needs iteration-evidence absorption; LOC headline needs reconciliation).

---

## §6 — Lens N — Graduation mechanicality (load-bearing for the deviation ledger)

The seven existing ledger rows at INDEX lines 60–67 close MECHANICAL under steelman as in SK-V1. The SK-V2 audit task is to verify (a) whether the iteration's empirical findings preserve or break MECHANICAL closure for the existing rows, and (b) whether new rows must be added.

### §6.1 — Existing seven rows under SK-V2 steelman

| Row | Site | SK-V2 finding |
|---|---|---|
| 1 | INDEX:61 | HM-as-top-level. No iteration evidence touches this. **MECHANICAL with named inversion** holds per SK-V1 §6 row 1. |
| 2 | INDEX:62 | Host-fn-free JSON. **The iteration evidence sharpens this row.** REDRESS.md item 19: "host-fn-free skinny remains faithful only for a V1 JSON path that keeps string decode lazy in the substrate/view layer; a parse-time `decode_json_string_to_arena` grammar needs an explicit SOTA concession or a lazy lowering amendment." This changes the row's V1 closure column from "Tranche D adds `@host fn` surface; decode moves back" to "Tranche D adds `@host fn` surface, **but** the decode lowering must be lazy (Cow-shape on view access) and not eager (parse-time call into registry); an eager-decode V1 lowering treats this as a MASKING signal and a substrate/view amendment is required to recover." **MECHANICAL conditional** — the additive closure is "add `@host fn` lowering + force lazy semantics at the lowerer." That is still additive code in the Lens-N sense (no skinny code moves), but the *correctness criterion* of the closure has tightened from "code added" to "code added with lazy-decode discipline." **REINVENT-narrow** — update row 2 V1 closure cell. |
| 3 | INDEX:63 | parse-that-regex directory promotion. Trivially MECHANICAL. **KEEP**. |
| 4 | INDEX:64 | `passes` HM-only. **KEEP** (SK-V1 analysis holds; iteration evidence doesn't touch). |
| 5 | INDEX:65 | `wasm = false`. **KEEP**. |
| 6 | INDEX:66 | **Tape sealing.** The wording shifted from "Box<[TapeToken]>" (SK-V1) to "private-Vec semantic sealing" (current). SUBSTRATE.md §1.2 line 101 now explicitly: "The canonical skinny therefore uses a private `Vec<TapeToken>` inside the finished `Tape`: semantic sealing is enforced by type privacy, and the public read API remains `&[TapeToken]`." WORKSPACE.md §8.1 row "Tape private-Vec semantic sealing" matches. ARCH §~1430 says: "JSON skinny adopted close-token elision and private-Vec semantic sealing after before/after bench rows." Cross-quadrant alignment intact. SK-V1 consolidated audit item C6 (TapeBuilder API not cited from BENCH §1.2) is *partially* addressed at INDEX:66 which mentions TapeBuilder explicitly; whether BENCH §1.2 picked up the cite belongs to the BENCH SK-V2 audit, not INDEX. **KEEP**. |
| 7 | INDEX:67 | HM-hierarchy receipt. Iteration evidence doesn't touch. **KEEP** but the row-1/row-7 duplication that SK-V1 flagged (REINVENT) is still unaddressed; either collapse or cross-reference. **REINVENT** carried forward from SK-V1. |

### §6.2 — Proposed new ledger row 8: lazy-decode V1 amendment surface (Lens N delta from iteration)

The iteration's empirical finding (REDRESS.md item 19) establishes that an eager-decode V1 JSON grammar would emit a MASKING signal. The mechanical closure is **lazy decode at the lowerer**, which lives at COMPILER's Backend-IR lowering and SUBSTRATE's `JsonString::as_str()` Cow projection (SUBSTRATE.md §1.2 line 181 has the Cow shape pre-iteration). INDEX should ratify the row as a new deviation:

> *Proposed new row 8 — Lazy-decode discipline at V1 graduation.* | The skinny is host-fn-free; eager parse-time string decode is documented as MASKING for V1 unless decode stays lazy (RESULTS.md / REDRESS.md item 19). | COMPILER.md §1.3 + SUBSTRATE.md §2 string row + ARCH §1265 area | The skinny's substrate keeps string decode lazy via `INLINE_STRING_BORROW` + `STRING_NEEDS_UNESCAPE` flag + `Cow<'input, str>` accessor; V1 graduation must lower `@host fn decode_string` through the same lazy-Cow projection, not as a parse-time `decode_json_string_to_arena` call. | Tranche D adds `@host fn` lowering; the lowerer must emit lazy-Cow on view access, not eager decode at parse time. Eager-decode lowering reopens Lock 1 (substrate / view-decoupling) or COMPILER §3 (extraction) per the MASKING signal in skinny/RESULTS.md. **MECHANICAL with lazy-decode discipline** under Lens N.

**Steelman against adding row 8:** isn't this already implicit in row 2 (host-fn-free → Tranche D adds `@host fn`)? **Counter:** No — row 2 is silent on *how* the decode lowers; the iteration evidence has narrowed the acceptable how. A new row is cleaner than a footnote on row 2 because the iteration finding has its own bench-recoverable signal (`host_call_eager_decode`) and its own SK-V1 audit cross-reference (the eager-decode MASKING resolution). Adding a row makes the Lens-N ledger reflect the iteration's empirical narrowing.

### §6.3 — Proposed new ledger row 9: Lock 1 amendment surface for substrate ceiling

The iteration's empirical finding (RESULTS.md Track 2 / sonic ≈ 0.55–0.65 across all corpora; REDRESS.md "Sonic Closeness" lines 195–212 + "Next No-Workaround Work" item 2) establishes that the canonical specified substrate (16-byte tokens + private-Vec sealing + close-token elision + pair-token retention + structural index + eager pair emission) hits a ~1.6× wall-time ceiling against sonic-rs. REDRESS.md item 18 + the closeness narrative explicitly name "lazy-offset tape replacement" / "chunked TapeBuilder" as the remaining route.

This is a Lock 1 amendment surface. ARCH §10.1 and the §1430 area sanction perturbation candidates (close-token elision, pair-token fusion, semantic sealing, chunked sealing) under Lock 1 substrate authority; the iteration has exhausted three of these (close-token elision adopted; pair-token fusion rejected; 12-byte token rejected) and identified one more (lazy-offset tape / chunked TapeBuilder) as remaining.

INDEX is the cross-quadrant ratifier of the deviation ledger. The ledger names mechanical-closure paths for skinny-V1 deviations; a Lock 1 amendment surface for the substrate ceiling is the most consequential V1 closure pending. INDEX should ratify it.

> *Proposed new row 9 — Substrate ceiling: lazy-offset tape / chunked TapeBuilder amendment.* | The canonical specified substrate (16-byte aligned tokens + private-Vec sealing + close-token elision + eager pair-token emission + structural-index parse) hits ~1.6× the fastest competitor's wall time on twitter, citm, canada (skinny/RESULTS.md). | SUBSTRATE.md §1.1 + §3.6 + ARCH §~1430 area; closeness narrative at skinny/REDRESS.md "Sonic Closeness" | The remaining substrate lever per REDRESS.md item 18 + "Next No-Workaround Work" item 2 is a **lazy-offset tape** (deriving subtree skips from spans at view traversal time, with a chunked or different-storage TapeBuilder that reduces allocated tape bytes without parse-boundary shrink/copy). | V1 closure is a Lock 1 amendment landing under SUBSTRATE.md §1.1 + §3.6 token-economy gate (before/after bench row mandatory per ARCH §1430 area). The amendment is structural at the substrate but additive at the read-side: `Tape`/`ValueRef` shapes do not change (SK-V1 row 6 / consolidated audit ratified this). **MECHANICAL with named inversion (Lock 1 amendment surface)** under Lens N — but the criterion is *throughput-load-bearing*: the closure must measurably close the ~40 % gap before the skinny can claim FAITHFUL on the substrate-ceiling axis.

**Steelman against adding row 9:** INDEX is the spec, not the bench-evidence log. The deviation ledger lists *intentional* skinny-vs-V1 deviations; the substrate ceiling is not an intentional deviation, it is a measured failure. Should INDEX absorb a failure-mode row? **Counter:** the ledger is the cross-quadrant ratifier of every spec-authoritative skinny-vs-V1 difference, intentional or otherwise. The iteration has established that the canonical specified substrate underperforms sonic by ~40 % and that a Lock 1 amendment (lazy-offset tape) is the named V1 closure path. That is exactly the ledger's job — name a deviation + name the V1 closure. The "intentional" framing is too narrow; ledger row 6 (private-Vec sealing) is itself an *emergent* deviation (initial skinny pinned Box<[T]>, iteration shifted to private-Vec). Row 9 has the same shape: emergent deviation surfaced by measurement, V1 closure named via Lock 1 amendment.

**Verdict: REINVENT — add rows 8 and 9 to the ledger; update row 2's V1 closure column.**

### §6.4 — Lens N SK-V2 verdict

honoured-with-narrow-amendment-required. KEEP × 6 (existing rows 1, 3, 4, 5, 6 hold; row 7 holds but is the SK-V1 row-1/row-7 duplication). REINVENT × 2 (row 2 V1 closure column needs lazy-decode discipline; rows 8 + 9 new). The pattern: existing seven rows all close MECHANICAL; the iteration has surfaced two more MECHANICAL surfaces that INDEX has not yet absorbed.

The bigger Lens-N question — does the iteration evidence *break* MECHANICAL closure for any existing row? — resolves NO. None of the seven rows is invalidated by the iteration. The iteration *extends* the ledger; it does not retract it. That is the right shape for a falsifiable prior-validation device: each measurement run can narrow the deviation set (the ledger grows; rows close or extend but do not retract).

---

## §7 — Lens M — Falsifiability

| Site | Item | SK-V2 finding |
|---|---|---|
| INDEX:23–26 | Threshold preview | Row 26 (new, post-iteration) carries the four-axis NO-GO surface (substrate, correctness, instrumentation, memory). Preview can return NO-GO. Falsifiability discharged at the preview level (an improvement over SK-V1). **But:** row 23 still binds outcome A on `S × 0.95` instead of `BEAT_BOUND`; **and** rows 23–25 do not enumerate that BENCH §10.3 maps the post-iteration matrix to seven distinct probability bands (A through M); the preview is a 4-row collapse where the matrix has 12 outcomes. SK-V1 surgery #2 (append "(detailed split: outcomes C, D, E in BENCH §6.1; SOTA-beat probability per BENCH §10.3)" to row 24) not landed. **REINVENT** for rows 23, 24. **KEEP** for the new row 26. |
| INDEX:78 step 6 | "Run the parity matrix per BENCH.md §6" | SK-V1 surgery #10 demanded appending "(12 outcomes; verdict classes: GO / CONDITIONAL / INVALID / NO-GO; outcomes A–M per BENCH §6.1)". Not landed. **REINVENT** — same surgery as SK-V1. |
| INDEX:81 step 9 | CONDITIONAL branch | SK-V1 surgery #10 demanded appending "(F-noise outcomes additionally require bare-metal re-run per BENCH §6.1 before dispatch posture is committed)". Not landed. **REINVENT** — same surgery as SK-V1. |
| INDEX:83 step 11 | NO-GO branch | "reopen Lock 1 (substrate) or COMPILER §3 (extraction) per which delta failed" — not enumerated. SK-V1 surgery #10 demanded per-outcome enumeration: `G/L → reopen Lock 1; I/K → codegen-correctness debug (COMPILER §6 / SUBSTRATE §3.4); M → SUBSTRATE memory review (§1.1 + §2); E → reopen Lock 5; J → re-instrument and re-run.` Not landed. **REINVENT**. **Additional SK-V2 surgery:** the iteration's empirical finding establishes that the *currently disposing* NO-GO outcome is G (substrate gap), and the lever for G is now refined per the iteration to "lazy-offset tape / chunked TapeBuilder amendment per the new proposed ledger row 9" — not just "reopen Lock 1." Step 11 should additionally cite the iteration's named lever. |

The iteration's measured NO-GO (outcome G across all corpora) is itself the strongest falsifiability evidence: the matrix has returned NO-GO under plausible adversarial input. The Lens M load-bearing function — preventing confirmation-bias dispatch — is met empirically.

**Lens M SK-V2 verdict:** partial-redress. KEEP × 4 (the new row 26 — partial redress of SK-V1 #3; steps 7 and 10 unchanged; the matrix-can-return-NO-GO falsifiability discharged). REINVENT × 2 (preview rows 23, 24; protocol steps 6, 9, 11).

---

## §8 — Lens L — Premise fidelity

INDEX itself does not classify omissions as FAITHFUL or MASKING — that is the per-quadrant audit's responsibility. INDEX's Lens L surface is cross-quadrant invariants and the "What the skinny is NOT testing" table.

| Site | Omission | Bench-recoverable signal | SK-V2 finding |
|---|---|---|---|
| INDEX:33 | Multi-grammar | None bench-recoverable; CSS prior probe is the partial signal (BENCH §9.1). | SK-V1 surgery #6 (append "(BENCH §9.1 CSS prior probe is the report-only substrate-generality signal.)") not landed. **REINVENT** — same surgery. |
| INDEX:35 | GADT/DK13/OutsideIn/CSP | Not bench-recoverable for monomorphic JSON. | SK-V1 surgery #5 (append "(FAITHFUL-for-JSON; load-bearing for CSS L4 / Sheets per HARDENING-SKINNY §5)") not landed. **REINVENT** — same surgery. |
| INDEX:37 | Cost-model + e-graph rewrites | "non-egraph alternate-plan stub bench" — singular | Already covered in Lane 3 / §4.2. The iteration narrowed plurality from three to two distinct alternates. **REINVENT** (consolidated C19). |
| INDEX:48 (invariant 1) | Host-fn-free | "BENCH must bound the direct-decode vs `CallHost` registry dispatch delta before RESULTS can claim FAITHFUL" — forward-looking. | The iteration has resolved the conditional: dispatch passes, eager decode MASKING. **REINVENT** — see §3 above. |
| INDEX:51 (invariant 4) | Single-plan extraction | "alternate-plan stub" — singular. | Already covered. **REINVENT** (consolidated C19). |

**The dominant SK-V2 Lens L finding** is that the iteration evidence has produced two MASKING signals that INDEX has not absorbed:

1. **Host-fn-free conditionally-FAITHFUL** — eager-decode MASKING on all three corpora; the cut is FAITHFUL only under a V1 lazy-decode discipline (per REDRESS.md item 19 + the new proposed ledger row 8 / row 2 V1-closure refinement).
2. **Substrate ceiling MASKING on the canonical specification** — the canonical substrate misses the sonic envelope by ~40 %; the cut is FAITHFUL only under a Lock 1 amendment (per REDRESS.md item 18 + the new proposed ledger row 9).

Both are MASKING-but-mechanically-closable; neither is a structural failure of the skinny's prior-validation discipline. Both narrow.

**Lens L SK-V2 verdict:** partial-redress with two new MASKING surfaces. KEEP × 4 (invariant rows that don't move; cells that don't move). REINVENT × 2 (the two iteration-evidence-driven MASKING surfaces need explicit naming in invariants 1 and the ledger).

---

## §9 — Lens F (LLM bias) + Lens H (provenance)

Lens H: all provenance verified under SK-V1 holds under SK-V2; the line-number shifts (INDEX +1 line) do not invalidate any cite. **KEEP**.

Lens F SK-V2 deltas:

| Site | Pathology | Surgery |
|---|---|---|
| INDEX:85 | "6-12 months of tranche execution" pseudo-precise numerics. | SK-V1 surgery #11. Not landed. **REINVENT**. |
| INDEX:3 | "Buildable in 2-4 weeks" wall-time without provenance. | SK-V1 surgery #12. Not landed. **REINVENT**. |
| INDEX:19 | "lands within or beats the sonic-rs / simd-json envelope" — the iteration evidence has falsified this for the current spec. The sentence is now aspirational rather than empirical, but reads as if it could be either. | **REINVENT-narrow** — reframe: "*The premise:* if a JSON parser ... lands within or beats the sonic-rs / simd-json envelope, ..., then the V1 architectural premise is validated for JSON-class grammars. *Current measurement:* skinny/RESULTS.md returns outcome G; the substrate ceiling under the canonical specification falls short of the envelope by ~40 %. *Remaining lever:* the lazy-offset tape / chunked TapeBuilder amendment surface (proposed ledger row 9 below)." |

The pattern is consistent: every SK-V1 Lens-F item not landed under SK-V2.

**Lens F SK-V2 verdict:** partial-redress. KEEP × 1 (citation provenance intact). REINVENT × 3.

---

## §10 — Lens I, Lens J, Lens K

INDEX is structurally minimal — 86 lines of mostly-tabular content. Zero apparatus chains, zero variant counts, zero speculative trait surfaces. Lens I **KEEP**. Lens J **N/A** (INDEX delegates). Lens K **KEEP** (delegates meta-grammar correctness floor to COMPILER and SOTA-aspiration to BENCH).

---

## §11 — Lane 7 — Friction forecast + Lane 9 — Greenfield discipline

Lane 7: SK-V1 surgery #1's note "*The preview is indicative. The live gate uses BEAT_BOUND per BENCH §6 ...*" at line 27 not landed. Reader-pathology persists. **REINVENT** — same surgery. Lane 9 **KEEP**.

---

## §12 — Punch list (ordered surgical edits before INDEX advances)

Items inherit from SK-V1 punch list unless explicitly closed; SK-V2-only items prefixed S2.

| # | Target site | Surgery | Source verdict | Owner | Scope | Lane(s) |
|---|---|---|---|---|---|---|
| 1 | INDEX.md:23 (table row 1, first cell) | (SK-V1 #1, not landed) Replace `Track 2 ≤ S × 0.95` with `Track 2 ≤ BEAT_BOUND`. Add a note above the table: `BEAT_BOUND = min(S × 0.95, T_README); T_README = README spec target (380 µs / 750 µs / 2.8 ms for twitter / citm / canada). For all three skinny corpora, T_README is the binding bound.` | REINVENT | INDEX agent | Narrow | 3, M, F |
| 2 | INDEX.md:24 (table row 2, "Action" cell) | (SK-V1 #2, not landed) Append "(detailed split: outcomes C, D, E in BENCH §6.1; SOTA-beat probability per BENCH §10.3)" | REINVENT | INDEX agent | Narrow | 3, M |
| 3 | INDEX.md:26 (table row 4) | **CLOSED by iteration.** SK-V1 #3 partially landed via the new row 4 covering correctness/memory NO-GO. No further surgery needed. | KEEP | — | — | — |
| 4 | INDEX.md:37 (cost-model + e-graph cell) | (SK-V1 #4 + consolidated C19, not landed) Replace "non-egraph alternate-plan stub bench" with "BENCH §7.8.2 alternate-plan probes (scalar, dispatch-table — invalidated per skinny/REDRESS.md item 17, x86_64 PEXT — plausibly-better)." | REINVENT | INDEX agent | Narrow | 3, L |
| 5 | INDEX.md:36 (GADT/DK13/OutsideIn/CSP cell) | (SK-V1 #5, not landed) Append "(FAITHFUL-for-JSON; load-bearing for CSS L4 / Sheets per HARDENING-SKINNY §5)." | REINVENT-narrow | INDEX agent | Narrow | L |
| 6 | INDEX.md:34 (multi-grammar cell) | (SK-V1 #6, not landed) Append "(BENCH §9.1 CSS prior probe is the report-only substrate-generality signal.)" | REINVENT-narrow | INDEX agent | Narrow | L |
| 7 | INDEX.md:48 (invariant 1 — host-fn-free) | (SK-V1 #7 + iteration delta) Replace forward-looking conditional with iteration-evidence ratification: "Because V1 JSON has numeric/string host fns, BENCH §7.8.1 probes have measured the split: `host_call_dispatch_overhead` passes (≤ 50 ns/call; skinny/RESULTS.md); `host_call_eager_decode` is MASKING on all three corpora (skinny/RESULTS.md). Host-fn-free is conditionally-FAITHFUL only for a V1 JSON path that keeps string decode lazy in the substrate/view layer." | REINVENT | INDEX agent | Narrow | H, L |
| 8 | INDEX.md:51 (invariant 4 — single-plan extraction) | (SK-V1 #8 + consolidated C19, not landed) Replace "small alternate-plan stub" with "alternate-plan probes (BENCH §7.8.2: scalar — reported, dispatch-table — invalidated per skinny/REDRESS.md item 17, x86_64 PEXT — plausibly-better; aarch64 measurement currently runs scalar only)." | REINVENT-narrow | INDEX agent | Narrow | 3, L |
| 9 | INDEX.md:61 + :67 (rows 1 and 7 of the deviation ledger) | (SK-V1 #9, not landed) Reconcile the two HM-hierarchy rows. Either collapse into one row with a "Lens N receipt" column (MECHANICAL with named inversion + closure cost 150–300 LOC per WORKSPACE §8.1), or cross-reference rows explicitly. | REINVENT | INDEX agent | Narrow | 1, 3, N |
| 10 | INDEX.md:78–84 (decision-protocol steps 6, 9, 11) | (SK-V1 #10, not landed) Step 6 append "(12 outcomes; outcomes A–M per BENCH §6.1)". Step 9 append "(F-noise outcomes additionally require bare-metal re-run per BENCH §6.1 before dispatch posture is committed)". Step 11 enumerate per BENCH outcome ID: `G/L → reopen Lock 1 (substrate); the iteration's G evidence routes to the new ledger row 9 lazy-offset / chunked-TapeBuilder amendment surface; I/K → codegen-correctness debug (COMPILER §6 / SUBSTRATE §3.4); M → SUBSTRATE memory review (§1.1 + §2); E → reopen Lock 5; J → re-instrument and re-run.` | REINVENT | INDEX agent | Narrow | M |
| 11 | INDEX.md:85 (closing sentence) | (SK-V1 #11, not landed) Drop "6-12 months" or cite a wall-budget source. Recommended rewrite: "exists to update the SOTA-beat probability with measurement evidence before the V1 plan commits to multi-quarter tranche execution. The first skinny run returned outcome G (skinny/RESULTS.md); the SOTA-beat probability update consumes that signal." | REINVENT | INDEX agent | Narrow | F |
| 12 | INDEX.md:3 (opening line, LOC headline) | (SK-V1 #12 + consolidated C1) Add wall-time provenance for "2-4 weeks". Update LOC: "~32,500–33,000 handwritten LOC (post-iteration BENCH §11.1 Track 2 measurement-driven shift) + ≤ 4,000 generated LOC. Buildable in a 2-4-week target window per WORKSPACE.md §9 build-time targets." | REINVENT-narrow | INDEX agent | Narrow | F, 6 |
| 13 | INDEX.md:65 (deviation ledger row 5 `wasm = false`) | (SK-V1 #13) Refine cite: "WORKSPACE.md §3 (line 179)" instead of "WORKSPACE.md §3." | KEEP-with-narrow-amendment | INDEX agent | Narrow | H |
| S2-1 | INDEX.md:19 (SOTA-viability premise narrative) | **SK-V2 NEW** — reframe the premise sentence to absorb the iteration's empirical evidence: "*The premise:* if a JSON parser ... lands within or beats the sonic-rs / simd-json envelope, ..., then the V1 architectural premise is validated for JSON-class grammars. *Current measurement:* skinny/RESULTS.md returns outcome G; the substrate ceiling under the canonical specification falls short of the envelope by ~40 %. *Remaining lever:* the lazy-offset tape / chunked TapeBuilder amendment surface (ledger row 9)." | REINVENT | INDEX agent | Narrow | 4, F |
| S2-2 | INDEX.md:62 (ledger row 2 — host-fn-free) | **SK-V2 NEW** — update V1 closure cell to absorb iteration evidence: "Tranche D adds `@host fn` surface; decode moves back into a lazy-Cow lowering path (not eager parse-time `decode_json_string_to_arena`) so the eager-decode MASKING signal documented in skinny/RESULTS.md / REDRESS.md item 19 does not fire at V1." | REINVENT-narrow | INDEX agent | Narrow | N, L |
| S2-3 | INDEX.md ledger (new row 8) | **SK-V2 NEW** — add ledger row 8 for lazy-decode V1 amendment surface. Text per §6.2 above. | REINVENT (additive) | INDEX agent | Narrow | N, L |
| S2-4 | INDEX.md ledger (new row 9) | **SK-V2 NEW** — add ledger row 9 for substrate-ceiling Lock 1 amendment surface (lazy-offset tape / chunked TapeBuilder). Text per §6.3 above. | REINVENT (additive) | INDEX agent | Narrow | N, L, 4 |
| S2-5 | INDEX.md §"Cross-quadrant invariants" (new bullet 7 OR amend existing bullet) | **SK-V2 NEW (consolidated C14, not landed)** — add "Onboarding contract: two surfaces (`json.bbnf` grammar source + workspace metadata); Lock 14's §5.6 declaration-crate fence is empty for the skinny per Lock 14." | REINVENT-narrow | INDEX agent | Narrow | 1, K |

**Cross-quadrant carry surfaced by this audit (NOT INDEX edits):**

| # | Target | Surgery | Reason |
|---|---|---|---|
| C-1 | WORKSPACE.md §8.1 (Mechanical Closure of Skinny Deviations) | Add two new rows: (a) lazy-decode discipline at V1 (mirror INDEX new row 8); (b) substrate-ceiling Lock 1 amendment (mirror INDEX new row 9). Closure cost columns: (a) TBD — lazy-Cow lowering at `@host fn` codegen, additive ~100–250 LOC; (b) TBD — Lock 1 amendment per before/after bench row mandate at ARCH §1430 area. | INDEX ledger should mirror WORKSPACE migration-parity matrix. Both new rows require closure-cost provenance at WORKSPACE. |
| C-2 | BENCH.md §10.3 (probability-update mapping) | Add a row explicitly mapping the iteration's outcome-G + new ledger row 9 amendment-pending state: "G (with ledger row 9 amendment-pending) | < 0.30 | < 0.05 → revisit after Lock 1 lazy-offset tape amendment lands." | Probability mapping should reflect the open amendment surface. |
| C-3 | SUBSTRATE.md §1.2 + §3.6 | Cite the new INDEX ledger row 9 from the §3.6 token-economy materialization gate; the gate is the bench surface for the Lock 1 amendment landing. | Cross-quadrant ratifier symmetry. |

---

## §13 — Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> INDEX has *partially* absorbed the SK-V1 redress: the threshold preview's new fourth row (correctness/memory NO-GO at line 26) and the deviation-ledger row 6 wording shift to "private-Vec semantic sealing" both landed, closing two of the SK-V1 punch-list items. **But:** SK-V1 punch items 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12 — covering the BEAT_BOUND threshold notation, the singular-vs-plural alternate-plan vocabulary, the cross-quadrant invariants 1 and 4 updates, the two HM-hierarchy ledger rows reconciliation, the decision-protocol per-outcome enumeration, the pseudo-precise wall-time, and the LOC headline reconciliation — did NOT land at INDEX. Additionally, three SK-V2 surgeries surface from the iteration evidence: (S2-1) the SOTA-viability premise narrative needs iteration-evidence absorption; (S2-2) ledger row 2 V1-closure column needs lazy-decode discipline; (S2-3, S2-4) two new ledger rows for the lazy-decode V1 amendment surface and the substrate-ceiling Lock 1 amendment surface. The propagation pattern is non-arbitrary: every SK-V1 item touching the deviation ledger landed; every SK-V1 item touching the threshold preview's semantics or the decision protocol's per-outcome enumeration did NOT land.
>
> Lens A (cross-document narrative coherence) is the load-bearing lens for INDEX and the dominant SK-V2 fault: INDEX is a cross-quadrant ratifier but currently ratifies only the ledger-row redresses, not the threshold-preview / decision-protocol / cross-quadrant-invariants redresses. Lens M (falsifiability) is honoured at the empirical level (the iteration returned outcome G NO-GO; the matrix can return NO-GO; the bench is falsifiable) but the protocol-surface enumeration of NO-GO levers is still pre-redress. Lens N (graduation mechanicality) is honoured at the existing seven-row level; two new rows (S2-3, S2-4) are required to absorb the iteration's empirical Lock 1 amendment surface and lazy-decode-discipline surface — both close MECHANICAL under steelman; no row requires re-architecture; no existing row is invalidated.
>
> Lens L (premise fidelity) surfaces two new MASKING signals from the iteration: host-fn-free is conditionally-FAITHFUL only under lazy-decode V1 lowering (skinny/RESULTS.md `host_call_eager_decode` MASKING on all three corpora); the canonical substrate specification is conditionally-FAITHFUL only under the lazy-offset tape / chunked-TapeBuilder Lock 1 amendment (skinny/RESULTS.md Track 2 / sonic ≈ 0.55–0.65 across all corpora). Neither is structural; both close mechanically. INDEX as cross-quadrant ratifier should name both as new ledger rows (S2-3, S2-4) and refine invariant 1 (#7) and the cost-model cell (#4) to match.
>
> Three cross-quadrant carries surface from this audit: (C-1) WORKSPACE.md §8.1 must mirror INDEX's two new ledger rows; (C-2) BENCH.md §10.3 should add a row for the amendment-pending outcome-G state; (C-3) SUBSTRATE.md §3.6 should cite the new INDEX row 9 as the bench surface for the Lock 1 amendment landing. All three are narrow and mechanical at their respective quadrants.
>
> Hereupon: dispatch the narrow-scope amendment agent against the 18-item punch list (items 1–13 from SK-V1 carry-forward + S2-1 through S2-5 new) with the C-1 / C-2 / C-3 cross-quadrant carries routed to their respective quadrant re-audits. After INDEX amendments land, **the skinny is NOT yet SK-READY** because the iteration's outcome-G NO-GO is the dispositive signal; the skinny implementation phase must wait on either (a) the Lock 1 lazy-offset tape / chunked-TapeBuilder amendment landing and a re-run that returns outcome A/B/C/D, or (b) an explicit re-anchored prior with an updated SOTA-beat probability narrative under outcome G. INDEX's job at SK-V2 is to surface those two pre-dispatch options correctly — the amendments above are the prerequisite for that surfacing.
>
> The skinny's prior-validation function survives this audit. The iteration's outcome G is empirical evidence the matrix can return NO-GO under plausible adversarial input; that is exactly the Lens M load-bearing function. INDEX's cross-quadrant ratification function survives narrowly: the deviation-ledger redress propagated; the preview-and-protocol redress did not. The amendments listed above close the propagation gap and absorb the iteration's two MASKING signals + two MECHANICAL surfaces; after they land, INDEX is SK-READY contingent on the Lock 1 amendment landing or the explicit re-anchored-prior route.

---

**Audit close.** Time consumed: ~33 minutes. Within hard cap (35 min). KEEP-without-challenge fraction: 23 / 41 ≈ 56 % (just below healthy 60–80 % band per HARDENING.md; the audit hit non-trivial challenges across Lens A, Lens L, Lens N as expected for a partial-redress state). REINVENT count: 18. DISCARD count: 0. Cross-quadrant carries surfaced: 3 (WORKSPACE §8.1 two-row mirror; BENCH §10.3 amendment-pending row; SUBSTRATE §3.6 cite).

The SK-V2 cycle on INDEX has consumed and resolved most SK-V1 carry-forwards via verdict-only routing (carry to amendment agent); it has absorbed the iteration's two MASKING signals via two new proposed ledger rows; and it has identified the load-bearing missing route — the lazy-offset tape / chunked-TapeBuilder Lock 1 amendment surface — that the spec authority must absorb before any V1 dispatch from the iteration's measured outcome G can be re-evaluated.

---

### Critical Files for Implementation

The amendment agent dispatched against the punch list (items 1–13 + S2-1 through S2-5) and the cross-quadrant carry routes (C-1, C-2, C-3) will need to read:

- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md` (the SK-V2 target; punch-list line surfaces 3, 19, 23–26, 33–34, 36, 37, 48, 51, 61–67, 78–85)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md` (BEAT_BOUND construction at §6 lines 605–714; alternate-plan probe registry at §7.8.2 lines 1024–1046; eager-decode MASKING reframing at §7.8.1; probability mapping at §10.3 lines 1474–1488 — carry C-2)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md` (private-Vec sealing wording at §1.2 line 101; token-economy materialization gate at §3.6 line 317 — carry C-3; lazy-decode shape at §2 string row)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/WORKSPACE.md` (mechanical-closure cost matrix at §8.1 lines 535–544 — carry C-1; metadata cite at §3 line 179)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md` (iteration evidence; items 17, 18, 19 + Sonic Closeness narrative lines 195–212 + Next No-Workaround Work — the source-of-truth for the two new INDEX ledger rows S2-3 and S2-4)
