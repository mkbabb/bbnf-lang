# Risk and Performance Matrix — B1 → AY-II → AZ → BA → BB

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
  to pass). Defensible floors are explicit in `AZ/AZ.md §Escape
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

## AZ — direct-to-struct + tape dissolution (NEW transformational tranche)

The highest-risk tranche on the runway. Seven waves. Three novel
architectural moves landing together: classifier unification,
StructRegistry universal closure, and tape deletion across the
fleet including BBNF self-hosting.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Classifier unification research + derive-cache lift + IR audit pass + baseline bench | 0.60 | 0.85 | Classifier unification turns out to require a deeper refactor than AZ can carry; triggers re-plan at AZ opening per `09-classifier-collision-frontload.md` |
| W1 | StructRegistry + project_types closure across all production grammars; hard-fail-and-block | 0.55 | 0.80 | One of the four grammars surfaces a rule that does not project cleanly to a native struct; BA blocked until carry wave lands |
| W2 | Scalar payload direct-to-struct (JSON + Sheets); twitter ≥ 1967 MB/s hard gate | 0.40 | 0.68 | Direct-to-struct activation does not reach AU parity on twitter in one wave; recovery plateau similar to AY-I.W1 |
| W3 | Aggregate/Named direct-to-struct (CSS L4); lightningcss node-for-node typed parity | 0.35 | 0.65 | Lightningcss typed parity is the most lawyered gate we have planned; partial parity counts as floor, full parity rarely hits first try |
| W4 | BBNF self-hosting direct-to-struct; two-stage bootstrap cutover; parity harness recoding | 0.40 | 0.65 | Stage A/B bootstrap cutover has historically produced mid-pipeline drift (AST ordering, trivia); byte-equal reproducibility is the hardest single check in AZ |
| W5 | Tape deletion; crates/tape/ removed; view codegen rewritten | 0.85 | 0.93 | Mechanical given W4 passes; `cargo build --no-default-features` without the crate is the close gate |
| W6 FINAL | 17-entry matrix parity + tape-gone verification | 0.92 | 0.96 | Aggregation |
| **AZ tranche close** | **All seven waves** | **0.09** | **0.38** | Compound of above. The declared estimate is low; the floor estimate includes the "bbnf-tape-mini retained for BBNF only, direct-to-struct on other three grammars" escape. |

The full-declared AZ landing at 0.09 reads as "maybe one attempt
in ten". This is not a prediction of failure; it reflects the
honest ambition of the plan. AZ's *defensible floor* at 0.38 —
roughly two in five — is the realistic planning horizon. The plan
explicitly budgets for escape-clause close per `AZ.md §Defensible
floor` and preserves tape-abrogation-for-BBNF as a follow-on
mini-tranche if W4 fails.

The single highest-impact risk is W4's BBNF bootstrap cutover.
Stage A produces a struct-based parser from the tape-based
compiler; Stage B rebuilds that parser from itself. Byte-equal
reproducibility across the two stages is the gate. Every prior
tranche that attempted a mid-pipeline cutover (AK-tape-substrate,
AV-columnar, AY-I-column-revert) had mid-tranche surprises; AZ.W4
is three such cutovers stacked because every grammar cuts over at
roughly the same wave boundary.

## BA — lazy typed pointer-path queries over struct tree

Opens on AZ close. Four waves. Conditional on AZ landing at least
at defensible floor on JSON + CSS.

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

Opens on AZ + AY-II close. Four waves. Not blocked on BA.

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

The tranches are strictly sequential except that BA and BB both
open on AZ close (BA does not block BB; BB depends on AY-II close
in addition to AZ). The dependency graph is:

```
B1 ─── AY-II ─── AZ ─── BA
                  │
                  └─── BB ── (also requires AY-II close)
```

Under the declared-gate probabilities, the end-to-end joint
probabilities are:

| Milestone | Joint P(declared) | Joint P(floor) | Reading |
|---|---:|---:|---|
| B1 + AY-II close | 0.55 × 0.20 = 0.11 | 0.80 × 0.55 = 0.44 | Gets us to a tape-substrate recovery |
| + AZ close | 0.11 × 0.09 = 0.010 | 0.44 × 0.38 = 0.17 | Gets us to direct-to-struct + tape deletion |
| + BA close | 0.010 × 0.27 = 0.003 | 0.17 × 0.55 = 0.09 | Adds pointer queries |
| + BB close | 0.010 × 0.10 = 0.001 | 0.17 × 0.32 = 0.05 | Adds rule inference |

The declared-gate end-to-end number of 0.001 reads as "one in a
thousand chance that every tranche closes on its exact declared
gate without any replan, escape, or carry wave". This is not an
honest forecast of project failure; it is an honest reading of
*how many independent ambitious gates we are stacking*. Every
tranche has declared reversal criteria; every tranche is allowed
to close on escape floor; every tranche has been planned assuming
some gates will be missed and the missing gates route into a
carry wave or the next tranche's opening scope.

The floor-level cascade at 0.05 (1 in 20) is the relevant
planning number. At the floor level, the runway delivers:

- Dev-loop truth, divan-harnessed bench, pinned toolchain (B1).
- JSON + Sheets at AU-baseline parity (AY-II floor).
- Direct-to-struct on JSON + CSS + Sheets, tape dissolved on
  those three; tape retained as `bbnf-tape-mini` for BBNF only
  (AZ floor).
- Rust-only pointer queries on JSON + CSS with zero-alloc
  traversal (BA floor).
- JSON enumeration + Class-1 auto-accept + partial Tranche H
  rediscovery (BB floor).

That set of outcomes is the realistic plan even if the declared
gates slip. Every declared gate that actually hits is a bonus.

The single most effective way to improve the cascade is to split
AZ into two tranches — one for JSON + Sheets + CSS direct-to-struct
and a separate one for BBNF self-hosting cutover + tape deletion.
Each split tranche individually would carry P(declared) ~ 0.55 and
P(floor) ~ 0.85; the joint for the two would be ~ 0.30 declared
and ~ 0.72 floor — substantially better than AZ's 0.09 / 0.38 as
currently scoped. Whether this split is worth the tranche-letter
cost is the plan's single biggest unresolved strategic question;
everything else that surfaced during the audit is resolved and
recorded in `GESTALT.md §10 — Decision record`.

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
| **AZ.W2 close** | **≥ 1231 (AU)** | **≥ 2438 (AU)** | **≥ 1967 (AU)** | **Direct-to-struct recovers AU parity** |
| AZ.W6 FINAL | ≥ 1300 | ≥ 2500 | ≥ 2000 | BEAT AU slightly — direct-to-struct overhead lower than tape+projection |
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
| **AZ.W3 close** | **≥ 735 (AU)** | **≥ 600** | **≥ 496 (AU)** | Typed parity with lightningcss node-for-node; bootstrap gate from ARCHIVAL-SYNTHESIS |
| AZ.W6 FINAL | ≥ 800 | ≥ 650 | ≥ 550 | BEAT AU |
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
| **AZ.W2 close** | **≥ 95 (AU)** | — | Recovery on direct-to-struct; parse_complex may land here |
| AZ.W6 FINAL | ≥ 110 | ≥ 60 | BEAT AU; parse_complex activated |
| BA / BB | parity | parity | — |

## BBNF grammar (self-hosting)

| Wave | self-parse | Notes |
|---|---:|---|
| AU baseline | (not in 17-entry matrix) | BBNF self-parse was a correctness gate at AU, not a perf gate |
| Current | functional, tape-substrate | Self-parse works; perf number not tracked against AU |
| B1 close | parity | Infra only |
| AY-II W4 close | functional, byte-identical to master | Correctness maintained on tape |
| **AZ.W4 close** | **byte-identical to pre-AZ compiler** | **Two-stage bootstrap cutover close gate**; perf not primary |
| AZ.W5 close | byte-identical, tape-free | Tape deletion verified |
| AZ.W6 FINAL | ≥ current + 10 % on self-parse micro-bench | Direct-to-struct on BBNF itself |

---

# Sensitivity — what moves the numbers

Four levers dominate the cascade probability:

1. **Classifier unification intractability** (AZ.W0 research, Q9
   resolution). If research shows unification requires a dedicated
   tranche, AZ.W0 closes on escape; AZ.W2 slips to a separate
   tranche; cascade drops by ~ 20 % across AZ.

2. **Twitter recovery plateau** (AZ.W2 gate; currently 688 MB/s →
   ≥ 1967 MB/s target). If direct-to-struct activation on JSON
   does not hit AU parity in one wave, AZ.W2 floor at 1500 MB/s
   is still acceptable but pushes the full-AU recovery to a
   carry wave. Cascade drops by ~ 10 %.

3. **BBNF bootstrap byte-equal reproducibility** (AZ.W4 gate).
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
