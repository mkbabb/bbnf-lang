# CRIT-P1 — NEON structural presize: promote or delete

Adversarial critique of PROTO-P1 (`prototypes/PROTO-P1.md`, worktree `wf_be85d0ab-073-7`) and the
spec section it tests (SPEC-DRAFT-P1 §2 lines 84-85, §5.6 line 210, §11 TW4 line 353, §13 Q4).
Convergence: **77 / 100**.

## What holds — evidence verified, not contrived

The code anchors are real, not fabricated: `CapacityPlan` at `tape/assembler.rs:14-27`, the
`OneShotSimd` arm at `grammars/json/scan.rs:63`, `scan_structurals` at `scan.rs:34`. The load-bearing
strawman characterization is CORRECT and independently confirmed here: `attach_structural_index`
(`grammars/json/generated.rs:12`) is a literal `let _ = state;` no-op in both master and the tested
worktree — the NEON scan runs only inside `structural_capacity_for` (scan.rs:63) to read
`.positions().len()`, then discards the index. So OneShotSimd genuinely pays for a full simdjson-stage-1
pass plus a ~66K-223K-entry throwaway `Vec` purely to size a tape. The liveness proof is triply
established (runtime witness backend==Neon, `cargo expand`, `nm` symbol). Method inheritance is honest:
report 21 named the cold-per-parse JSON harness lacuna (21:50-77) and P1 built exactly the mandated
`json_canon_bench` mirror; the `proc_pid_rusage` PMU path is 21's stated fallback (21:27-30). The
scope boundary (lines 106-111) is exemplary — it refutes presize *as implemented* (thrown-away scan),
explicitly NOT a consumed stage1→stage2 index (handed to P2), preventing over-generalization to "NEON
never helps." The verdict corroborates the dig's six prior universal-pre-pass losses (§2 line 82); it
is the seventh, and coherent.

## Where the evidence is thinner than the verdict

1. **The load-bearing control is the weakest-measured comparison.** The DELETE-everything recommendation
   (drop Sampled/Exact/OneShotSimd, keep only GrowOnly — lines 152-155) rests NOT on the dramatic
   OneShotSimd regression but on the claim that `Sampled` *ties* `GrowOnly` (line 91: twitter −2.2%,
   canada **+9.3%**, citm +1.4%, "within noise"). No stddev/CI is reported in the report (the method
   computes it, line 139, but never shows it). A +9.3% swing on canada is asserted-as-noise without a
   quantified noise floor. The gross OneShotSimd delta (58-135%) dwarfs any drift and is safe; the
   delicate "presize-buys-nothing" claim that justifies deleting `Sampled` is under-supported.

2. **Plans measured sequentially, not interleaved, under a 4-point load band.** Reproduce (line 149)
   runs `for P in D A B C` — four separate binary invocations. "Identical system load" (line 32) is
   overstated when loadavg is a 14-18 range across minutes; the finest comparison (Sampled vs GrowOnly)
   is exactly the one most exposed to inter-invocation drift. Interleaving all four plans per iteration
   inside one process would null this; P1 did not.

3. **Cold-quiet mandate formally unmet.** loadavg 14-18 vs the charter's <2 (PASS-ALPHA:62). P1's
   cycles/byte load-insensitivity argument is only partly true — `ri_cycles` is scheduling-insensitive
   but NOT immune to shared-cache / memory-bandwidth contention, which disproportionately taxes exactly
   the extra full-input pass OneShotSimd adds, plausibly inflating the headline magnitude. Direction is
   safe; the "2.35×" figure is not a cold-quiet number and must be re-captured at loadavg<2 in TW0.

4. **Presentational:** Mbps is tabled (line 80) despite the report saying it "must NOT be quoted"
   (line 38) — put it behind a stark directional-only label or drop it. "GrowOnly measured optimal among
   the four" (line 103) overstates a *tie* with Sampled; honest phrasing is "tied-best and simplest, so
   kept (KISS)." The headline "1.6-2.4× regression" measures a self-described strawman; the honest
   headline is "presize buys nothing measurable; the dead scan is pure waste."

## Where the spec must move after P1

The spec draft predates P1 and frames the pre-scan as an OPEN two-way decision. P1 closes it NEGATIVE;
pass-2 synthesis must fold DELETE in, not carry an open decision into TW4:

- **§2 lines 84-85** — reframe from "two-way measured decision... opening gate" to "resolved NEGATIVE:
  DELETE the throwaway-scan presize; churn is cheap." The directional lean "would remove 7-8 growth
  events" (as if valuable) is contradicted — the removal buys nothing (Sampled tie).
- **§5.6 line 210 / §11 TW4 line 353** — "structural pre-scan decision per §2/P1" and "pre-scan
  two-way decision with the cargo-expand opening gate" must become a resolved DELETE landing in TW1's
  N24 reclaim sweep / TW0 hygiene, NOT an open TW4 gate.
- **The activation-gate lesson P1 surfaces:** the spec's "prove the scan live under cargo expand before
  the wave counts it" is shown INSUFFICIENT — P1 proves it live yet losing. Liveness is necessary, not
  sufficient; the real gate template is liveness AND a measured ≥3% win. This is the correct Y.13-class
  gate and should be stated as such.
- **§13 Q4** — P1 satisfies the "P1 establishes the measurement loop" half (json_canon_bench built);
  the CSS-SIMD-tokenization question remains open.

## Residual work before fully settled

None requires a new prototype or research loop — the decision (DELETE; keep GrowOnly only) is settled
and architecturally over-determined. Two confirmatory items remain, both in-tranche: (a) a loadavg<2
re-run of the four plans with stddev/CI reported, interleaved per-iteration, so the Sampled tie is
demonstrated not asserted — this is the TW0 quiet re-capture already scheduled (§2 line 98); (b) the
`json_canon_bench` harness itself lands and gets methodology review as the JSON truth-floor instrument
(it is a shared P1/P2 deliverable, currently only in a discarded worktree). Plus the synthesis fold-in
above. The item is plan-ready; it is not yet cold-quiet-certified.
