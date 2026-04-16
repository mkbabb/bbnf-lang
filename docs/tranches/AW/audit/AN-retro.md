# Tranche AN — Retrospective (R-AN)

Forensic review of tranche AN ("Correctness, Generalization,
Hyper-Optimization"), bracketed by `acaa1898` (plan) and `bc8277a5`
(post-AN bench doc). Prior: AM. Forward: AO.

## 1. Scope reality vs. plan

`AN.md` declared six work-streams: AN.0 correctness (6 bugs), AN.1 CSS
`@ws` SIMD, AN.2 scanner generalization, AN.3 single-pass string
scan, AN.4 whitespace bitmap, AN.5 32-byte SIMD, AN.6 cost/CSP
instrumentation.

Actually landed (per `git log`): AN.0.1-0.4, AN.1, AN.4.2 (bitmap
only). Never shipped: AN.0.5 (CSS L4 tailwind parse), AN.2 scanner
generalization, AN.3 single-pass string scan, AN.5 32-byte SIMD, AN.6
instrumentation. AN.0.6 rolled into a combined `serialize+gorgeous`
fix (`97987a08`).

Two **unplanned** phases materialized under contact: Phase 0
three-tier payload projection (`0c61dd56`) and Phase 1.1 serialize
tape-first rewrite (`e6574a98`, 675 compile-errors, 22 roundtrip
tests) — the latter was AL.1 debt surfacing.

Bench vs. target ("beat sonic-rs on every dataset"): canada +15%
BEAT; citm/data/data_xl/twitter all **regressed or stalled** vs
post-AM (twitter 1,671→1,650, data_xl 1,153→1,108, citm
2,138→2,052). Target unmet on 4/5 JSON datasets.

## 2. Silent vs. declared deferrals

`AN.md` has no "deferred" section. AN.0.5, AN.2, AN.3, AN.5, AN.6 are
all **silent deferrals** — violating `no-deferrals`. AO.md
explicitly absorbs them: "Also incorporates remaining AN items:
parse-that generalization ... SIMD widening, CSS L4 tailwind grammar
fix, and global CSP solve."

## 3. Orchestration friction

No `PROGRESS.md`, `FINAL.md`, `research/`, or `audit*.md` for AN —
"What Landed" was appended to `AN.md`. Per-tranche directory structure
(`36945f60`) post-dates AN, so those artefacts cannot be retroactively
demanded, but their absence means no dated log distinguishes declared
scope from contact-driven scope.

## 4. Agent-layer friction

Unobservable — no wave records survive. Commits show linear
single-author cadence, not the multi-agent wave pattern the README
later codifies. Wave discipline itself post-dates AN.

## 5. Edict adherence

- `no-deferrals`: violated (§2).
- `accurate-perf-narrative`: the "Post-AN Final Baseline" table
  foregrounds +15% canada while concealing four flat/regressed
  datasets. The stated aim ("close the sonic-rs gap via single-pass
  string scanning") was not met on the datasets that motivated it
  (citm/twitter).
- `inspect-generated-output`: upheld — AN.1 cites expanded-LOC delta
  (-2190 lines, -29%) as evidence.
- `new-tranche-new-doc`: violated — Phase 0 and Phase 1.1 were new
  bodies of work retrofitted onto AN rather than opening new tranche
  letters.

## 6. Chronic deferrals — IN/OUT

**IN** (from AM): structural bitmap activation (AM.5 "infrastructure"
landed but not wired); sonic-rs string-heavy gap.

**OUT** (to AO and beyond):
- AN.2 scanner generalization → AO Phase 2.
- AN.3 single-pass string scan → AO discussion of `quoted_simd.rs`.
- AN.5 32-byte SIMD → AO Phase 3.1.
- AN.6 CSP/e-graph instrumentation → AO Phase 4.3
  (`BBNF_EGRAPH_REPORT`/`BBNF_CSP_REPORT`).
- CSS L4 tailwind offset 387594 → AO Phase 5.1 → AP → AQ → AR.
  Four-tranche chronic originating in AN.0.5.
- Structural-dispatch activation (implicit AN.2 premise) → AO Phase 0
  shipped "code-complete but never exercised" → AP.

## 7. Mid-tranche restructuring

Phase 0 (payload projection) and Phase 1.1 (serialize rewrite) were
inserted mid-tranche without re-planning. Phase 1.1 was a 675-error
cross-cut triggered by AL.1 debt — the kind of scope reveal that
should open a fresh tranche letter. Instead it landed under
`AN.md`'s "What Landed".

## 8. Lessons

1. **Bench claim honesty.** A "+15% BEAT on canada" banner cannot
   stand beside four silent regressions. Post-tranche summaries must
   report worst-case delta alongside best-case, against the prior
   baseline.
2. **Silent-deferral cascade.** AN punted four optimization items to
   AO; AO shipped Phase 0 "code complete, never exercised"; AP
   inherited the same activation gap. One silent deferral compounded
   across four tranches of dead or dormant code.
3. **Scope reveal begets tranche letter.** Phase 0 and Phase 1.1
   should have opened new tranche letters, not been retrofitted onto
   `AN.md`'s "What Landed" block.
