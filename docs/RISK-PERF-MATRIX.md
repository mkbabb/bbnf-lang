# Risk and Performance Matrix — B1 → AY-II → AZ-I → AZ-II → BA → BB

A calibrated estimate of landing probability per wave and per
tranche, and the measured performance target each grammar is
expected to meet at each juncture. This is a planning artefact,
not a forecast. The probabilities reflect honest judgment against
the project's observed history: Era V's 572-commit substrate rut,
AW-IV's 0-of-17 gate miss, AQ.5's clean reversal, AP.5's landed
delim-scan, AX.W1r's column revert, AY-I.W1's 688 MB/s recovery
plateau. Every estimate is a "best guess under current evidence"
with the dominant risk named.

## How to read this matrix

Each row of a probability table carries two numbers:

- **P(declared)** — probability the wave closes on the specific
  numerical gate the plan names. Missing the gate by any margin
  counts against this number.
- **P(floor)** — probability the wave closes on its declared
  *defensible floor* (the minimum outcome acceptable for the wave
  to pass). Defensible floors are explicit in `AZ-I/AZ-I.md or AZ-II/AZ-II.md §Escape
  clause` and analogous sections of the other tranche docs.

Both numbers are point estimates; actual confidence intervals are
wide and asymmetric. A P(declared) = 0.55 reading means *if the
project were to attempt this wave ten times under independent
starting conditions, the declared gate would be met between four
and six attempts*. The declared-vs-floor gap measures plan
ambition — a wave with P(declared) = 0.50 and P(floor) = 0.95 has
an aggressive headline target over a safe backstop; a wave with
P(declared) = 0.70 and P(floor) = 0.72 is declaring something
close to its floor.

Performance tables are per-grammar. Every cell is the MB/s floor
the wave must meet at that grammar's benchmark entry; "≥" reads
as a hard lower bound, "—" reads as out-of-scope for the wave,
"parity" means no regression vs the prior wave's close measurement,
and "(AU)" annotates the AU-baseline target this cell recovers.

## Calibration anchors

The probability estimates are anchored against the project's
observed base rates from the commit archaeology (2787 events, 18
tranches with plan docs, 6 FINAL docs on master):

| Historical anchor | Count | Observation |
|---|---:|---|
| Tranches with FINAL docs (full close) | 6 | AU, B0, F? (pre-formal-tranche tag), plus 3 additional at substrate closes |
| Tranches opened and reverted wholesale | 3 | AW-V (rewritten at AX.W0c), EmissionTier/struct-dispatch (AQ.5), columns (AY-I.W1) |
| Tranches closed on explicit escape clause | 2 | AY-I (honest relinquish), AX (replay-ledger close) |
| Gate misses that triggered re-plan | 4 | AW-IV (0/19 gates), AW-V (0/17), AV.2.x (reverted), AE (EmissionTier) |
| Tranches with hard perf gate that hit it | 3 | AP.5 (delim-scan), AO (structural), AJ (zero-alloc child) |
| Tranches whose perf gate missed by > 20 % | 6 | Every tranche in Era V's AV-AW-AX cluster |

Base rate for a declared perf gate landing without revert: ~35 %
across Era V; ~70 % across Eras III-IV. Era VI is a restart; the
probabilities below reflect the renewed discipline (every wave
same-commit runtime call site, reversal-as-health-signal, measurement
gates substrate per AX invariant 13) but do not assume Era VI
mechanics will outperform Era III-IV until evidence arrives.

---

# Probability per tranche

## B1 — dev-loop truth + proof-surface hardening

Bounded prelude. Four waves, 14 agent-slots, one-week horizon.
Well-understood mechanical work; recipe borrowed from
`TOOLCHAIN-SOTA.md`.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Pin + cargo config + nextest + Makefile rewrite; ICE-clean gate | 0.92 | 0.97 | lld path absent on a dev box; cranelift component unavailable on pinned nightly |
| W1 | Divan port of 19 benches + bencher removal + iai-callgrind CI | 0.75 | 0.90 | Bench-baseline parity drift outside ± 5 %; 1-2 of 19 ports surface divan-specific API surprises |
| W2 | CI rewire + script abrogation + cross-repo propagation | 0.82 | 0.92 | Cross-repo CI workflow permission issues; pin-diff CI needs three synchronized PRs |
| W3 | PROFILING.md refresh + FINAL + post-B1.json | 0.95 | 0.98 | Nothing substantive; doc consolidation |
| **B1 tranche close** | **All four waves declared gates** | **0.55** | **0.80** | Compound of above; defensible floor is "pin + divan live for ≥ 12 / 19 benches + CI nextest-mandatory" |

B1 is the highest-probability tranche on the runway because it
buys nothing it hasn't already proven. The declared-gate estimate
is pulled down by the multiplicative product across four waves;
the floor is much higher because partial divan migration still
delivers the dev-loop truth objective.

## AY-II — W0' close + W1-W5 resume on tape substrate

Infra-truth closure. AY-II stays at its pre-audit scope:
FusedBuilder consolidation, typed-materialisation on the tape
substrate, parity harness closure. No tape abrogation work in
AY-II.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0' close | Bootstrap regen + alias retirement + fat-LTO bench matrix + samply/nm captures | 0.80 | 0.92 | One of the four primary grammars surfaces an unresolved `Unknown` variant; bootstrap double-regen non-idempotent |
| W1 | JSON semantic parity + peer-referenced performance | 0.70 | 0.85 | Twitter gap from 688 MB/s to AU-baseline 1967 MB/s is steep; partial recovery is the floor |
| W2 | CSS L4 typed-semantic parity | 0.65 | 0.82 | CSS L4 has 3 known deep driver gaps at time of audit (per `project_css_typed_codegen`); any unresolved blocks close |
| W3 | Sheets typed semantics + performance | 0.75 | 0.88 | Sheets corpus smaller than JSON/CSS; baseline 95 MB/s is modest so easier to meet |
| W4 | BBNF self-hosting identity + grammar-meta typed semantics | 0.70 | 0.85 | BBNF's self-parse must produce IR identical to the tape-based parser — cutover mid-tranche is tight |
| W5 | Close matrix + FINAL + successor handoff | 0.95 | 0.98 | Consolidation |
| **AY-II tranche close** | **All waves** | **0.20** | **0.55** | Compound drops sharply across five waves; floor is "AU parity on at least JSON + Sheets, partial on CSS + BBNF" |

The twitter recovery gap (688 → 1967 MB/s) is AY-II's hardest
single problem. The floor estimate of 0.55 assumes the recovery
lands to at least 1500 MB/s (76 % of AU) on twitter with
regression elsewhere minimised — a partial but creditable close.

## AZ-I — direct-to-struct for JSON + CSS L4 + Sheets (NEW transformational tranche, first half)

The first of the two tranches that dissolve the tape. AZ-I
activates direct-to-struct for the three data grammars; BBNF
continues on the tape substrate through AZ-I close. Four waves
plus FINAL. Scope is narrower than the monolithic AZ originally
planned, which boundaries AZ-I away from the single highest-risk
piece (BBNF bootstrap cutover, now AZ-II).

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Classifier unification research + derive-cache lift + IR audit pass + baseline bench | 0.65 | 0.88 | Classifier unification requires a deeper refactor than AZ-I can carry; triggers re-plan at AZ-I opening (front-loaded per Q9 resolution) |
| W1 | StructRegistry + project_types closure across JSON + CSS L4 + Sheets; hard-fail-and-block | 0.65 | 0.85 | One of the three data grammars surfaces a rule that does not project cleanly to a native struct; BA eventually blocks until carry wave lands |
| W2 | Scalar payload direct-to-struct (JSON + Sheets); twitter ≥ 1967 MB/s hard gate | 0.45 | 0.72 | Direct-to-struct activation does not reach AU parity on twitter in one wave; recovery plateau similar to AY-I.W1 |
| W3 | Aggregate/Named direct-to-struct (CSS L4); lightningcss node-for-node typed parity | 0.40 | 0.68 | Lightningcss typed parity is the most lawyered gate; partial parity counts as floor, full parity rarely hits first try |
| W4 FINAL | 17-entry matrix parity on data grammars; tape scoped to BBNF only; AZ-I handoff contract for AZ-II | 0.92 | 0.96 | Aggregation |
| **AZ-I tranche close** | **All five waves** | **0.070** | **0.34** | Compound of above. Defensible floor: direct-to-struct on JSON + Sheets with CSS partial; tape retained for CSS + BBNF. |

AZ-I's full declared-gate estimate at 0.070 looks worse than the
monolithic AZ's 0.09 because the per-wave gates here are
individually more stringent (hard twitter recovery gate, full
lightningcss parity); the floor estimate at 0.34 is a compound
weighted toward the W2/W3 perf gates where reversal is the
expected case.

## AZ-II — BBNF self-hosting cutover + `crates/tape/` deletion

The second transformational tranche. Opens on AZ-I close.
Three waves plus FINAL. This is where the single highest-risk
piece (BBNF bootstrap cutover via two-stage reproducibility)
lives with its own reversal gates, isolated from the data-grammar
activation work.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | BBNF bootstrap cutover design + classifier extension for BBNF-specific patterns | 0.70 | 0.90 | Drift sources (AST ordering, trivia, numeric formatting) surface at design time but no mitigation lands; W1 opens with known drift |
| W1 | Stage A — tape-based compiler builds struct-based BBNF parser candidate; byte-compare against pre-AZ-II | 0.55 | 0.78 | Stage A candidate is structurally correct but not byte-equal due to unforeseen emission ordering |
| W2 | Stage B — W1 candidate rebuilds itself; byte-equal vs Stage A | 0.50 | 0.72 | Byte-equal reproducibility is the hardest single check in the entire runway; triggers `bbnf-tape-mini` escape if missed |
| W3 FINAL | `crates/tape/` deletion + view codegen rewrite + parity harness recoding + 17-entry parity | 0.88 | 0.94 | Mechanical given W2 passes; `cargo build --no-default-features` without `crates/tape/` is the close gate |
| **AZ-II tranche close** | **All four waves** | **0.17** | **0.45** | Compound. Declared outcome: `crates/tape/` deleted wholesale. Last-resort floor (invoked only on intractable W2 byte-equal failure): `bbnf-tape-mini` retained for BBNF bootstrap with tape-deletion routed to a follow-on micro-tranche; this is the escape valve, not a planning alternative. |

The split across the BBNF-cutover boundary materially changes the
cascade arithmetic (see §Cascade below). The single highest-impact
risk — Stage A/B byte-equal reproducibility — is now contained
inside AZ-II rather than folded into a monolithic AZ; failure in
AZ-II triggers the `bbnf-tape-mini` escape without contaminating
the data-grammar activation in AZ-I, and every prior tranche that
attempted a mid-pipeline cutover (AK-tape-substrate, AV-columnar,
AY-I-column-revert) is a cautionary precedent that informs AZ-II's
explicit drift-source enumeration in W0.

## BA — lazy typed pointer-path queries over struct tree

Opens on AZ-II close with `crates/tape/` fully dissolved. If
AZ-II invokes its last-resort `bbnf-tape-mini` escape, BA still
opens — the BBNF parser's residual `bbnf-tape-mini` consumer does
not affect BA's substrate, which is the grammar-derived struct
tree for every grammar.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Path IR + type checker + parent-pointer micro-bench deciding sidecar-vs-embedded | 0.78 | 0.90 | Parent-pointer benchmark ambiguous; `AscentStrategy` trait deferred to W1 decision rather than wave close |
| W1 | Lazy traversal + `path!` macro + per-grammar micro-bench | 0.65 | 0.83 | Zero-allocation traversal goal missed on one grammar; dhat-verification shows residual heap use |
| W2 | Host-binding isomorphism — TS + Python macro expansion | 0.55 | 0.78 | Cross-language macro hygiene is the hardest single piece; TS decorator ergonomics differ from Python callable |
| W3 FINAL | Close matrix + handoff to BB | 0.94 | 0.97 | Consolidation |
| **BA tranche close** | **All four waves** | **0.27** | **0.55** | Defensible floor is "Rust-only path queries on JSON + CSS with zero-alloc confirmed"; TS + Python bindings are stretch |

BA's probability is gated by AZ's outcome. If AZ closes on
declared gates, BA.P(declared) rises by ~15 %; if AZ closes on
escape floor, BA's TS/Python work may not open at all (the
`path!` macro over an unsettled StructRegistry is ill-defined).

## BB — e-graph rule inference + VM oracle + ranker

Opens on AZ-I + AY-II close. Four waves. Not blocked on BA or
AZ-II — rewrite rules operate on `IrNode` which is substrate-
independent, so BB can run in parallel with AZ-II and BA after
the three data grammars' IR stabilises at AZ-I close.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Enumerator + e-graph residue split + VM oracle wrapper + ranker + `crates/ir/src/rewrites/` scaffold + Tranche H ≥ 80 % rediscovery | 0.50 | 0.70 | Tranche H rediscovery misses 80 % threshold on first run; requires enumerator alphabet or oracle refinement |
| W1 | JSON + Sheets enumeration run; initial curation batch | 0.65 | 0.82 | Ranker ranking inversion — candidates scored high but humans reject; class-2/3 split tuning needed |
| W2 | CSS + BBNF enumeration | 0.55 | 0.75 | CSS complexity produces e-graph saturation explosions; enumeration bound too tight or too loose |
| W3 | Grammar-specific rule discovery + per-grammar rule files | 0.62 | 0.80 | Few grammar-specific rules actually surface; per-grammar `rewrites/` directories may ship near-empty |
| W4 FINAL | Cost-model integration + CI for auto-accept + close | 0.90 | 0.95 | Consolidation |
| **BB tranche close** | **All five waves** | **0.10** | **0.32** | Compound; floor is "JSON enumeration working + Class-1 auto-accept live + Tranche H rediscovery ≥ 50 %" |

BB is the second-lowest declared-gate probability after AZ, but
its failure mode is softer. A BB close on escape floor still
delivers useful automation (auto-accept on algebraic identities)
without blocking downstream work.

---

# Cascade — joint probability across the runway

The tranches are mostly sequential with one parallel edge: BA
opens on AZ-II close, while BB can open on AZ-I close in parallel
with AZ-II (rewrite rules operate on `IrNode`, which stabilises
at AZ-I regardless of the BBNF cutover state). Dependency graph:

```
B1 ─── AY-II ─── AZ-I ─── AZ-II ─── BA
                   │
                   └──────────────── BB ── (also requires AY-II close)
```

Under the declared-gate probabilities, the end-to-end joint
probabilities are:

| Milestone | Joint P(declared) | Joint P(floor) | Reading |
|---|---:|---:|---|
| B1 + AY-II close | 0.55 × 0.20 = 0.11 | 0.80 × 0.55 = 0.44 | Tape-substrate recovery through AY-II |
| + AZ-I close | 0.11 × 0.070 = 0.008 | 0.44 × 0.34 = 0.15 | Direct-to-struct on 3 data grammars; tape retained for BBNF only |
| + AZ-II close | 0.008 × 0.17 = 0.0014 | 0.15 × 0.45 = 0.068 | BBNF cutover; tape crate deleted |
| + BA close | 0.0014 × 0.27 = 0.00038 | 0.068 × 0.55 = 0.037 | Pointer queries over struct tree |
| + BB close | 0.00038 × 0.10 = 0.000038 | 0.037 × 0.32 = 0.012 | Rule inference |

**Honest accounting of the split's effect.** The pre-split
monolithic AZ had P(declared) 0.09 and P(floor) 0.38. The split
gives AZ-I + AZ-II joint P(declared) 0.012 and P(floor) 0.15 —
*lower* raw multiplicative joint than the monolith. This is
counterintuitive but correct: the split adds waves (9 total across
AZ-I + AZ-II versus 7 in the monolith), and each additional wave
is another probability-less-than-one factor in the cascade.

What the split *does* buy is not raw joint probability — it is
**mid-runway closure preservation**. In the monolithic AZ, if W4
(BBNF cutover) fails, the *entire* tranche closes on escape and
the direct-to-struct activation for the data grammars is tangled
with the BBNF failure. In the split, AZ-I is a hard closure gate
*before* BBNF cutover starts. P(at least AZ-I closes at floor) is
0.15 — meaning in 15 % of attempts, the runway reaches a world
where three of four grammars are on direct-to-struct and the plan
has a clean checkpoint, even if AZ-II never closes. That
checkpoint has genuine engineering value — benches, type checkers,
host-binding work can all start against AZ-I's output without
waiting for BBNF's outcome.

The declared-gate end-to-end 0.000038 reads as "every tranche
closes on its exact declared gate without any replan or escape";
that number is not a forecast of project failure, it is an honest
reading of *how many independent ambitious gates are stacked*.
The floor cascade at 0.012 (roughly 1 in 80) is below the
pre-split 0.05 — the price paid for the split is a less favourable
cascade multiplier, offset by genuine mid-runway-closure value
that simple multiplication cannot capture.

At the floor level, the runway delivers:

- Dev-loop truth, divan-harnessed bench, pinned toolchain (B1).
- JSON + Sheets at AU-baseline parity, CSS partial (AY-II floor).
- Direct-to-struct on JSON + Sheets with CSS partial; tape retained
  for CSS + BBNF (AZ-I floor).
- BBNF on direct-to-struct with `crates/tape/` deleted wholesale
  (AZ-II declared). Last-resort floor: `bbnf-tape-mini` retained
  for BBNF bootstrap only, invoked *only* when byte-equal
  reproducibility proves intractable after genuine attempt.
- Rust-only pointer queries on JSON + CSS with zero-alloc traversal;
  TS + Python bindings stretch (BA floor).
- JSON enumeration + Class-1 auto-accept + partial Tranche H
  rediscovery (BB floor).

That set of outcomes is the realistic plan even if the declared
gates slip. Every declared gate that actually hits is a bonus.

**What the split does improve empirically**: reversal surface.
A revert inside AZ-I reverts only AZ-I substrate; a revert inside
AZ-II reverts only AZ-II substrate. In the monolith, any revert
touches seven waves of interleaved substrate. AQ.5's clean
reversal (32 commits, one wave's worth of substrate) was the
project's cleanest revert because its scope was narrow; the
AZ-I/AZ-II boundary enforces similar narrow-scope reverts by
construction. The expected-reversal-count rises, but each
reversal is cheaper.

**AZ-II is required, not optional.** Full tape abrogation is a
hard architectural requirement — the last orthogonal codepath
must dissolve so every downstream optimisation (BA pointer
queries, BB rule inference, and every future tranche beyond them)
lives in a single-substrate world. Halting after AZ-I leaves
`crates/tape/` alive for one grammar, which is exactly the
"two-decision-surfaces" pathology `feedback_no-orthogonal-codepaths`
prohibits. The split improves reversal surface and mid-runway
checkpointing, it does not create a legitimate halt point. The
`bbnf-tape-mini` escape defined in `AZ-II/AZ-II.md` is an
escape-of-last-resort invoked only when W2 byte-equal
reproducibility proves intractable after genuine attempt — it is
not a planning alternative.

---

# Performance marks per grammar per juncture

All values in MB/s on the reference hardware (Apple M1 Pro, cold
per-parse, no warmup — `no-warm-benches` discipline). The AU
baseline column reproduces the 17-entry matrix from
`AU/FINAL.md`. Current (AY-I.W1) values reflect the observed
state after the column revert. Wave-close targets are the *hard
gate* for that wave; missing by > 20 % triggers the wave's
reversal criterion.

## JSON grammar

| Wave | canada | citm | twitter | Notes |
|---|---:|---:|---:|---|
| AU baseline | 1231 | 2438 | 1967 | The floor to recover; measured at `5281ec23` |
| Current (AY-I.W1) | ~450 | ~950 | 688 | 35-37 % of AU; column-revert plateau |
| B1 close | parity | parity | parity | Infra only; no perf regression vs current |
| AY-II W1 close | ≥ 800 | ≥ 1600 | ≥ 1200 | Partial recovery on tape substrate |
| AY-II close | ≥ 1000 | ≥ 2000 | ≥ 1500 | Floor target; full AU recovery is W1 stretch |
| **AZ-I.W2 close** | **≥ 1231 (AU)** | **≥ 2438 (AU)** | **≥ 1967 (AU)** | **Direct-to-struct recovers AU parity** |
| AZ-II.W3 FINAL | ≥ 1300 | ≥ 2500 | ≥ 2000 | BEAT AU slightly — direct-to-struct overhead lower than tape+projection |
| BA W1 close | parity | parity | parity | Pointer queries do not regress full-parse |
| BA W1 (lazy 3-field) | — | ≥ 3000 | ≥ 2400 | 3-field extraction micro-bench; beats sonic-rs by ≥ 20 % |
| BB W2 close | parity | parity | parity | Rule inference maintains full-parse perf |

## CSS L4 grammar

| Wave | normalize | bootstrap | tailwind | Notes |
|---|---:|---:|---:|---|
| AU baseline | 735 | 454 | 496 | Measured at `5281ec23`; lightningcss reference 450-900 range |
| Current (AY-I.W1) | ~300 | ~200 | ~210 | ~40 % of AU |
| B1 close | parity | parity | parity | Infra only |
| AY-II W2 close | ≥ 500 | ≥ 350 | ≥ 380 | Partial recovery with known deep-driver gaps |
| AY-II close | ≥ 600 | ≥ 400 | ≥ 440 | Floor; full parity remains AZ's responsibility |
| **AZ-I.W3 close** | **≥ 735 (AU)** | **≥ 600** | **≥ 496 (AU)** | Typed parity with lightningcss node-for-node; bootstrap gate from ARCHIVAL-SYNTHESIS |
| AZ-II.W3 FINAL | ≥ 800 | ≥ 650 | ≥ 550 | BEAT AU |
| BA W1 close | parity | parity | parity | Pointer queries over selector trees do not regress full-parse |
| BB W2 close | parity | parity | parity | Rule inference over selector/property IR preserves perf |

## Sheets grammar

| Wave | parse_simple | parse_complex | Notes |
|---|---:|---:|---|
| AU baseline | 95 | (unrecorded) | AU baseline; parse_simple only in the 17-entry matrix |
| Current (AY-I.W1) | ~45 | — | ~47 % of AU |
| B1 close | parity | — | Infra only |
| AY-II W3 close | ≥ 75 | — | Partial recovery |
| AY-II close | ≥ 85 | — | Floor; AU parity is stretch |
| **AZ-I.W2 close** | **≥ 95 (AU)** | — | Recovery on direct-to-struct; parse_complex may land here |
| AZ-II.W3 FINAL | ≥ 110 | ≥ 60 | BEAT AU; parse_complex activated |
| BA / BB | parity | parity | — |

## BBNF grammar (self-hosting)

| Wave | self-parse | Notes |
|---|---:|---|
| AU baseline | (not in 17-entry matrix) | BBNF self-parse was a correctness gate at AU, not a perf gate |
| Current | functional, tape-substrate | Self-parse works; perf number not tracked against AU |
| B1 close | parity | Infra only |
| AY-II W4 close | functional, byte-identical to master | Correctness maintained on tape |
| **AZ-II.W1 + W2 close** | **byte-identical to pre-AZ-II compiler** | **Two-stage bootstrap cutover close gate**; perf not primary |
| AZ-II.W3 close | byte-identical, tape-free | Tape deletion verified |
| AZ-II.W3 FINAL | ≥ current + 10 % on self-parse micro-bench | Direct-to-struct on BBNF itself |

---

# Sensitivity — what moves the numbers

Four levers dominate the cascade probability:

1. **Classifier unification intractability** (AZ-I.W0 research, Q9
   resolution). If research shows unification requires a dedicated
   tranche, AZ-I.W0 closes on escape; AZ-I.W2 slips to a separate
   tranche; cascade drops by ~ 20 % across AZ-I (AZ-II may or may not open).

2. **Twitter recovery plateau** (AZ-I.W2 gate; currently 688 MB/s →
   ≥ 1967 MB/s target). If direct-to-struct activation on JSON
   does not hit AU parity in one wave, AZ-I.W2 floor at 1500 MB/s
   is still acceptable but pushes the full-AU recovery to a
   carry wave. Cascade drops by ~ 10 %.

3. **BBNF bootstrap byte-equal reproducibility** (AZ-II.W2 gate).
   Failure here is the single highest-impact lever — triggers the
   "bbnf-tape-mini" escape, keeping the tape alive in reduced
   form for BBNF only. Cascade drops by ~ 25 % but defensible
   floor is preserved.

4. **Tranche H rediscovery threshold** (BB.W0 gate; ≥ 80 % of
   hand-coded rules re-derived). If the enumerator produces
   candidates that don't match Tranche H shapes, BB.W0 floor
   drops to 50 %; downstream waves still function.

The other five open questions (pin drift, thread collision,
cache key, pointer form, partial registry) are high-probability-
of-closure items — their failure modes are mitigated by explicit
guardrails (CI jobs, test suites, hard-fail gates) rather than
soft probability.

---

# Reading the matrix as a planning instrument

This document is a calibration tool. It answers two questions the
tranche plan docs alone cannot:

- *How confident am I that the declared gates land?* The answer
  is "moderately on any individual wave, low across the runway as
  a compound product, comfortable at the defensible-floor level".

- *Where does a grammar stand at each checkpoint?* The per-grammar
  tables name the expected MB/s (or correctness property) per wave
  and highlight where recovery of AU-baseline is the primary
  outcome versus where BEAT-AU is targeted.

Update cadence: this matrix should be refreshed at every tranche
close (B1 FINAL, AY-II FINAL, AZ FINAL, BA FINAL, BB FINAL) with
the actual-landed column appended and the remaining-runway
probabilities re-estimated against the new evidence. The
calibration anchors above should also be refreshed whenever a
tranche closes on escape, reverts wholesale, or exceeds its
declared gates — each is a data point that updates the base-rate
priors for the remaining runway.
