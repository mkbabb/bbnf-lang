# Risk and Performance Matrix — B1 → B3 → B4 → B2 → B5 → AY-II → AZ-I → AZ-II → BA → BB

A calibrated estimate of landing probability per wave and per
tranche, and the measured performance target each grammar is
expected to meet at each juncture. This is a planning artefact,
not a forecast. The probabilities reflect honest judgment against
the project's observed history: Era V's 572-commit substrate rut,
AW-IV's 0-of-17 gate miss, AQ.5's clean reversal, AP.5's landed
delim-scan, AX.W1r's column revert, AY-I.W1's 688 MB/s recovery
plateau. Every estimate is a "best guess under current evidence"
with the dominant risk named.

The post-B1 predecessor sequence (B3 parser-baseline restoration →
B4 codegen `syn::parse2` emit-correctness + unified rollback → B2
build-time codegen transposition → B5 substrate restoration → B6
dev-loop annex → B7 cross-repo modernization) closed 2026-04-27;
AZ-I.W0 (CLASSIFIER-UNIFICATION + audit pass) and AZ-I.W1
(`StructRegistry` + `project_types` closure on JSON / Sheets /
CSS L4) closed clean; AZ-I.W2 closed substrate-only 2026-04-28
(StructBuilder + JSON runtime + EmitStrategy + 9 per-shape struct-
direct emitters all integrated and compile-clean; resolver returns
TapeDirect for every grammar pending the W2-act follow-on wave that
lands JsonDocument view/value accessors, parity harness recoding,
and the cargo bench gate). Probability lifts on AZ-I (residual
W2-act / W2.B / W3 / W4) and downstream tranches track that
substrate footing.

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

The 2026-04-24 four-agent preflight was executed and folded into the
owning tranche docs. It found missing `cargo-nextest`, absent
`rust-toolchain.toml`, pre-divan `bencher` state, absent
`StructRegistry`, absent BB rewrite storage, and a generic
`cargo expand -p bbnf --test projection_totality` probe that was
killed after roughly two minutes because it pulled the full heavyweight
graph. The lift comes from making those facts explicit before
implementation dispatch, not from lowering the architecture.

## Calibration anchors

The probability estimates are anchored against the project's
observed base rates from the commit archaeology (2787 events, 18
tranches with plan docs, 6 FINAL docs on master):

| Historical anchor | Count | Observation |
|---|---:|---|
| Tranches with FINAL docs (full close) | 7 | AU, B0, F? (pre-formal-tranche tag), plus 4 substrate closes (B1, B2, B3, B4); B5 closes 2026-04-26 |
| Tranches opened and reverted wholesale | 3 | AW-V (rewritten at AX.W0c), EmissionTier/struct-dispatch (AQ.5), columns (AY-I.W1) |
| Tranches closed on explicit escape clause | 2 | AY-I (honest relinquish), AX (replay-ledger close) |
| Gate misses that triggered re-plan | 5 | AW-IV (0/19 gates), AW-V (0/17), AV.2.x (reverted), AE (EmissionTier), B5.W2 parts 1+2 (audit-rerouted to W6 via W2b architectural diagnosis; full close held) |
| Tranches with hard perf gate that hit it | 4 | AP.5 (delim-scan), AO (structural), AJ (zero-alloc child), B5 (`compile_bbnf` within 5 % of B4 baseline) |
| Tranches whose perf gate missed by > 20 % | 6 | Every tranche in Era V's AV-AW-AX cluster |
| Mid-tranche audit-driven re-routing without revert | 1 | B5.W2b architectural diagnosis routed Parts 1+2 to W6 substrate-level depth-stamp inversion; the wave's plan-incorrect prescription retired, the tranche stayed open, and the substrate transposition landed at W6+W6b without breaking the workspace |
| B-series closed (B1/B3/B4/B2/B5/B6/B7) | 7 | B1/B3/B4/B2/B5 closed on declared gates; B6.W0 closed on declared (192× cold-wall speedup); B6.W1+W2 closed on rationale-satisfied per SPEC §Plan-time miscalibration (prescribed mechanisms structurally incapable of moving prescribed metrics); B7 closed on declared gates (cross-repo modernization, infrastructure-only) |

Probabilities below reflect post-B7 substrate closure. B7's
cross-repo modernization (divan/nextest unification, 2026-04-27)
is infrastructure-only; it does not alter AY/AZ/BA/BB probability
estimates but anchors the measurement-harness stability required
by those tranches.

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

Current preflight reality from 2026-04-24: no `rust-toolchain.toml`
is present, ambient `rustc` is `1.96.0-nightly (9602bda1d
2026-04-05)`, `cargo-nextest` is missing, `bench-json` and the
per-exclude aliases are not live, the Makefile still lacks
`bench-json`, `make -n ay-bench-close WAVE=close` still clears
`.bbnf-cache`, and `bencher = "0.1"` remains in `crates/core`. B1's
probability increases only after W0 first makes that surface executable
and cache-preserving.

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

Post-preflight estimate once B1.W0 makes the host/tool packet green:
P(declared) 0.65, P(floor) 0.88. The lift is not from new runtime
work; it is from preventing agents from spending a wave on absent
commands.

## AY-II — W1-W5 resume on post-B5 substrate

Infra-truth closure. AY-II stays at its pre-audit scope:
typed-materialisation on the unified substrate, parity harness
closure. No tape abrogation work in AY-II. AY-II.W0' close
ceremony folded into B4.W1 close (2026-04-25); the post-B5
substrate further restores the surface to one type (`Tape<R>`)
with a two-method parser boundary. Cycle-2 idempotency, fat-LTO
5-bench matrix, samply per primary grammar, and `nm` of bench
binaries route to wave-specific close gates (W1.c JSON, W2 CSS,
W3 Sheets, W4.e BBNF) where peer-parity context is meaningful.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W1 | JSON semantic parity + peer-referenced performance on the post-B5 substrate | 0.74 | 0.87 | Twitter gap from 688 MB/s to AU-baseline 1967 MB/s is steep; partial recovery is the floor |
| W2 | CSS L4 typed-semantic parity | 0.68 | 0.84 | CSS L4 has 3 known deep driver gaps at time of audit (per `project_css_typed_codegen`); any unresolved blocks close |
| W3 | Sheets typed semantics + performance | 0.77 | 0.90 | Sheets corpus smaller than JSON/CSS; baseline 95 MB/s is modest so easier to meet |
| W4 | BBNF self-hosting identity + grammar-meta typed semantics | 0.73 | 0.87 | BBNF's self-parse must produce IR identical to the xtask-regen-emitted bbnf parser; cutover mid-tranche is tight |
| W5 | Close matrix + FINAL + successor handoff | 0.95 | 0.98 | Consolidation |
| **AY-II tranche close** | **All waves** | **0.32** | **0.68** | Compound gate across five waves; post-B5 lift from substrate cleanup (one type, two-method parser boundary, single-writer `frame_depth` invariant). Floor is "AU parity on JSON + Sheets, typed CSS L4 surfaces present but not yet lightningcss-complete, BBNF identity retained" |

The twitter recovery gap (688 → 1967 MB/s) is AY-II's highest-
leverage performance problem. The floor estimate of 0.68 (post-B5
lift from 0.65) assumes the recovery lands to at least 1500 MB/s
(76 % of AU) on twitter with regression elsewhere minimised — a
useful recovery checkpoint that keeps the unified substrate moving
toward AU parity. The lift on the floor reflects substrate truth,
not performance optimism: post-B5 the substrate is `Tape<R>` over
`Columns` with the welded surface dissolved, so a regression
inside W1's wave cap is more likely to surface a tractable
substrate-locatable fix than to spend the wave's budget reaching
the right surface.

Post-preflight estimate after a fresh narrow expand matrix proves
no second parse, projection totality, and CSS same-path materializer
consumption: P(declared) 0.38, P(floor) 0.74. A generic expand of a
large test is not acceptable evidence; the 2026-04-24 probe showed it
pulls the full heavyweight graph; post-B2 the per-grammar generated
source is on disk and the expand boundary is no longer crossed.

## AZ-I — direct-to-struct for JSON + CSS L4 + Sheets (NEW transformational tranche, first half)

The first of the two tranches that dissolve the tape. AZ-I
activates direct-to-struct for the three data grammars; BBNF
continues on the tape substrate through AZ-I close. Four waves
plus FINAL. Scope is narrower than the monolithic AZ originally
planned, which boundaries AZ-I away from the most concentrated-risk
piece (BBNF bootstrap cutover, now AZ-II).

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Classifier unification research + IR audit pass + baseline bench (post-B2 amendment: derive-cache + Watt sub-agents dropped — T3-superseded) | 0.72 | 0.92 | Classifier unification requires a deeper refactor than AZ-I can carry; triggers re-plan at AZ-I opening (front-loaded per Q9 resolution) |
| W1 | StructRegistry + project_types closure across JSON + CSS L4 + Sheets; hard-fail-and-block | 0.66 | 0.86 | One of the three data grammars surfaces a rule that does not project cleanly to a native struct; BA eventually blocks until carry wave lands |
| W2 | Scalar payload direct-to-struct (JSON + Sheets); twitter ≥ 1967 MB/s hard gate | 0.46 | 0.74 | Direct-to-struct activation does not reach AU parity on twitter in one wave; recovery plateau similar to AY-I.W1 |
| W3 | Aggregate/Named direct-to-struct (CSS L4); lightningcss node-for-node typed parity | 0.42 | 0.60 | Lightningcss typed parity is the most lawyered gate; floor is struct-only CSS with named semantic gaps, never a CSS tape bridge |
| W4 FINAL | 17-entry matrix parity on data grammars; tape scoped to BBNF only; AZ-I handoff contract for AZ-II | 0.92 | 0.96 | Aggregation |
| **AZ-I tranche close** | **All five waves** | **0.085** | **0.38** | Compound of above. Post-B5 lift on top of post-B2: substrate cleanup retires the largest aesthetic-debt risk; module decomposition makes change-blast-radius locatable for the W1 StructRegistry closure. Defensible floor: direct-to-struct on JSON + Sheets with CSS L4 struct-only but semantically partial; tape retained for BBNF only. |

AZ-I's full declared-gate estimate at 0.085 (post-B5 lift from 0.080)
reflects the W0 amendment that drops the derive-cache lift + Watt
sub-agents and concentrates the wave on its load-bearing
classifier-unification + IR audit work, layered with B5's substrate
cleanup. The per-wave gates remain stringent (hard twitter recovery
gate, full lightningcss parity); the floor estimate at 0.38
(post-B5 lift from 0.36) reflects that bisect + reversal cycles
run against a sharper substrate (one rollback, one position
accessor, one cousin-leak guard at the iteration boundary). The
planning posture is not retreat; W2/W3 carry planned revert-and-
replan rails so any miss narrows toward struct-only CSS instead of
closing on a mixed tape/struct state.

Post-preflight estimate after `CLASSIFIER-UNIFICATION.md`,
`payload_coverage.rs`, `StructRegistry`, and the JSON/Sheets/CSS
struct-only vertical slices exist and their xtask regen checks
pass: P(declared) 0.13-0.15, P(floor) 0.42-0.46. Without those
gates, the floor should be discounted because the live materializer
surface still contains tape payload reads and `CursorChild` panic
paths.

## AZ-II — BBNF self-hosting cutover + `crates/tape/` deletion

The second transformational tranche. Opens on AZ-I close.
Three waves plus FINAL. This is where the most concentrated
bootstrap risk (BBNF cutover via two-stage reproducibility) lives
with its own reversal gates, isolated from the data-grammar
activation work.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | BBNF bootstrap cutover design + classifier extension for BBNF-specific patterns | 0.74 | 0.92 | Drift sources (AST ordering, trivia, numeric formatting) surface at design time but no mitigation lands; W1 opens with known drift |
| W1 | Stage A — tape-based compiler builds struct-based BBNF parser candidate; byte-compare against pre-AZ-II | 0.58 | 0.80 | Stage A candidate is structurally correct but not byte-equal due to unforeseen emission ordering |
| W2 | Stage B — W1 candidate rebuilds itself; byte-equal vs Stage A | 0.54 | 0.76 | Byte-equal reproducibility is the tightest single check in the runway; post-B2 a miss triggers wave revert + re-plan against captured drift evidence at seconds-cost cycles (vs the pre-B2 hours-cost) — no partial-closure floor is declared but reversal narrows in practice |
| W3 FINAL | `crates/tape/` deletion + view codegen rewrite + parity harness recoding + 17-entry parity | 0.89 | 0.95 | Mechanical given W2 passes; `cargo build --no-default-features` without `crates/tape/` is the close gate; post-B5 the tape crate is sharper (one type, no welded wrapper) so deletion target shrinks |
| **AZ-II tranche close** | **All four waves** | **0.21** | **0.52** | Compound. Post-B5 lift on top of post-B2: tape deletion target is sharper (no FusedBuilder weld, no welded value-side wrappers, no escape hatch — just `Tape<R>` over `Columns`); byte-equal reproducibility cycles cost seconds; reversal cycles tractable. Declared and only acceptable close: `crates/tape/` deleted wholesale. No partial-closure floor is pre-declared; W2 byte-equal miss triggers wave revert and re-plan against captured drift evidence until full dissolution holds. |

The split across the BBNF-cutover boundary materially changes the
cascade arithmetic (see §Cascade below). The highest-impact
cutover risk — Stage A/B byte-equal reproducibility — is now contained
inside AZ-II rather than folded into a monolithic AZ; a miss in
AZ-II reverts AZ-II's own substrate without contaminating the
data-grammar activation AZ-I already landed, and every prior
tranche that attempted a mid-pipeline cutover (AK-tape-substrate,
AV-columnar, AY-I-column-revert) is a cautionary precedent that
informs AZ-II's explicit drift-source enumeration in W0. Full
dissolution remains the only acceptable AZ-II close; re-plan
cycles may be required until byte-equal holds.

Post-preflight estimate after AZ-II.W0 splits into design plus
executable preflight, with `BOOTSTRAP-CUTOVER.md`, actual tape-symbol
census, `project_types_bbnf`, Stage A/B runner skeleton, and injected
drift negative test: P(declared) 0.23-0.26, P(floor) 0.55-0.60.

## BA — lazy typed pointer-path queries over struct tree

Opens on AZ-II close with `crates/tape/` fully dissolved. No
partial-substrate opening is accepted — BA requires the full
struct-tree surface that AZ-II's close guarantees.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Path IR + type checker + parent-pointer micro-bench deciding sidecar-vs-embedded | 0.78 | 0.90 | Parent-pointer benchmark ambiguous; `AscentStrategy` trait deferred to W1 decision rather than wave close |
| W1 | Lazy traversal + `path!` macro + per-grammar micro-bench | 0.65 | 0.83 | Zero-allocation traversal goal missed on one grammar; dhat-verification shows residual heap use |
| W2 | Host-binding isomorphism — TS + Python macro expansion | 0.55 | 0.78 | Cross-language macro hygiene is the densest integration piece; TS decorator ergonomics differ from Python callable |
| W3 FINAL | Close matrix + handoff to BB | 0.94 | 0.97 | Consolidation |
| **BA tranche close** | **All four waves** | **0.28** | **0.56** | Defensible floor is "Rust-only path queries on JSON + CSS with zero-alloc confirmed"; TS + Python bindings are stretch. Post-B5: substrate-truth canon for the typed cursor surface BA's typed `Path<Grammar, Target>` reads off. |

BA's probability is gated by AZ's outcome. If AZ closes on
declared gates, BA.P(declared) rises by ~15 %; if AZ closes on
escape floor, BA's TS/Python work may not open at all (the
`path!` macro over an unsettled StructRegistry is ill-defined).

Post-preflight estimate after BA.W-1 proves "no tape, full
registry, struct-only parse, bootstrap reproducibility permanent" and
the legacy tape path surface is retired or renamed before typed paths
open: P(declared) 0.34-0.38, P(floor) ~0.65.

## BB — e-graph rule inference + VM oracle + ranker

Opens on AZ-I + AY-II close. Four waves. Not blocked on BA or
AZ-II — rewrite rules operate on `IrNode` which is substrate-
independent, so BB can run in parallel with AZ-II and BA after
the three data grammars' IR stabilises at AZ-I close.

| Wave | Scope | P(declared) | P(floor) | Dominant risk |
|---|---|---:|---:|---|
| W0 | Enumerator + e-graph residue split + VM oracle wrapper + ranker + `crates/ir/src/rewrites/` scaffold + Tranche H ≥ 80 % rediscovery | 0.50 | 0.70 | Tranche H rediscovery misses 80 % threshold on first run; requires enumerator alphabet or oracle refinement |
| W1 | JSON + Sheets enumeration run; initial curation batch | 0.65 | 0.82 | Ranker ranking inversion — candidates scored high but humans reject; class-2/3 split tuning needed |
| W2 | CSS and BBNF enumeration | 0.55 | 0.75 | CSS complexity produces e-graph saturation explosions; enumeration bound too tight or too loose |
| W3 | Grammar-specific rule discovery + per-grammar rule files | 0.62 | 0.80 | Few grammar-specific rules actually surface; per-grammar `rewrites/` directories may ship near-empty |
| W4 FINAL | Cost-model integration + CI for auto-accept + close | 0.90 | 0.95 | Consolidation |
| **BB tranche close** | **All five waves** | **0.10** | **0.33** | Compound; floor is "JSON enumeration working + Class-1 auto-accept live + Tranche H rediscovery ≥ 50 %". Post-B5: rules operate on substrate-independent `IrNode`, so substrate cleanup is orthogonal; any net effect is from cleaner cost-model surface in the post-B5 e-graph rule library. |

BB has a small declared-gate probability because discovery,
ranking, and emission proof are stacked together; its floor remains
valuable. A BB close on floor still
delivers useful automation (auto-accept on algebraic identities)
without blocking downstream work.

Post-preflight estimate after rule storage, derive-cache discovery,
VM-residue wrapper, and fire/extract/writeback/emission proof are
code-real: P(declared) 0.18-0.25, P(floor) 0.45-0.55. The current live
rewrite substrate is fixed Rust rules under `crates/ir/src/egraph/rules/`;
BB must prove discovered-rule impact through generated-code changes.

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
| + AZ-I close | 0.11 × 0.070 = 0.008 | 0.44 × 0.29 = 0.128 | Direct-to-struct on 3 data grammars; tape retained for BBNF only |
| + AZ-II close | 0.008 × 0.17 = 0.0014 | 0.128 × 0.45 = 0.058 | BBNF cutover; tape crate deleted |
| + BA close | 0.0014 × 0.27 = 0.00038 | 0.058 × 0.55 = 0.032 | Pointer queries over struct tree |
| + BB close | 0.00038 × 0.10 = 0.000038 | 0.032 × 0.32 = 0.010 | Rule inference |

**Honest accounting of the split's effect.** The pre-split
monolithic AZ had P(declared) 0.09 and P(floor) 0.38. The split
gives AZ-I + AZ-II joint P(declared) 0.012 and P(floor) 0.13 as
raw multiplicative arithmetic. This is expected: the split adds
waves (9 total across AZ-I + AZ-II versus 7 in the monolith), and
each additional wave is another probability-less-than-one factor in
the cascade.

What the split buys is not raw joint probability — it is
**mid-runway closure preservation**. In the monolithic AZ, if W4
(BBNF cutover) misses, the *entire* tranche closes on escape and
the direct-to-struct activation for the data grammars is tangled
with the BBNF cutover miss. In the split, AZ-I is a hard closure gate
*before* BBNF cutover starts. P(at least AZ-I closes at floor) is
0.128 — meaning in roughly 13 % of attempts, the runway reaches a world
where three of four grammars are on direct-to-struct and the plan
has a clean checkpoint, even if AZ-II never closes. That
checkpoint has genuine engineering value — benches, type checkers,
host-binding work can all start against AZ-I's output without
waiting for BBNF's outcome.

The declared-gate end-to-end 0.000038 reads as "every tranche
closes on its exact declared gate without any replan or escape";
that number is not a ceiling. It is a proof obligation signal:
the runway stacks many independent ambitious gates and must be
driven by wave-local proof, not by hope. The floor cascade at 0.010
(roughly 1 in 100) is below the pre-split 0.05 because the split adds
gates; the engineering value is the clean checkpoint and cheaper
re-plan surface that simple multiplication cannot capture.

At the floor level, the runway delivers:

- Dev-loop truth, divan-harnessed bench, pinned toolchain (B1).
- JSON + Sheets at AU-baseline parity, CSS partial (AY-II floor).
- Direct-to-struct on JSON + Sheets with CSS L4 struct-only but
  semantically partial; tape retained for BBNF only (AZ-I floor).
- BBNF on direct-to-struct with `crates/tape/` deleted wholesale
  (AZ-II declared, and the only acceptable close). Byte-equal
  reproducibility misses route to wave revert and re-plan, not
  to a partial-closure state.
- Rust-only pointer queries on JSON + CSS with zero-alloc traversal;
  TS + Python bindings stretch (BA floor).
- JSON enumeration + Class-1 auto-accept + partial Tranche H
  rediscovery (BB floor).

That set of outcomes is the disciplined floor while the declared
gates remain the target. The plan does not lower the target to the
floor; it uses the floor to keep architectural progress useful when
a wave needs re-plan.

**What the split does improve empirically**: reversal surface.
A revert inside AZ-I reverts only AZ-I substrate; a revert inside
AZ-II reverts only AZ-II substrate. In the monolith, any revert
touches seven waves of interleaved substrate. AQ.5's clean
reversal (32 commits, one wave's worth of substrate) was the
project's cleanest revert because its scope was narrow; the
AZ-I/AZ-II boundary enforces similar narrow-scope reverts by
construction. The number of planned re-plan opportunities rises, but
each re-plan is cheaper and better isolated.

**AZ-II is required, not optional.** Full tape abrogation is a
hard architectural requirement — the last orthogonal codepath
must dissolve so every downstream optimisation (BA pointer
queries, BB rule inference, and every future tranche beyond them)
lives in a single-substrate world. Halting after AZ-I leaves
`crates/tape/` alive for one grammar, which is exactly the
"two-decision-surfaces" pathology `feedback_no-orthogonal-codepaths`
prohibits. The split improves reversal surface and mid-runway
checkpointing, it does not create a legitimate halt point. Full
tape abrogation is binding repo policy. AZ-II does not declare a
partial-closure floor; a W2 byte-equal miss triggers wave revert
and re-plan against captured drift evidence, repeated as many
times as required until the dissolution holds.

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

3. **CSS struct-only typed richness** (AZ-I.W3 gate). The former
   floor permitted CSS aggregate tape retention; that is now
   rejected. If lightningcss node-for-node parity misses, the
   acceptable floor is CSS L4 on struct-only output with named
   semantic gaps and no tape fallback. Cascade drops by ~ 15 %
   relative to the stale floor.

4. **BBNF bootstrap byte-equal reproducibility** (AZ-II.W2 gate).
   A miss here is the highest-impact lever — triggers
   wave revert and AZ-II re-plan against captured drift evidence,
   repeated as many cycles as required. Cascade drops by ~ 25 %
   per additional re-plan cycle; the close gate (full dissolution)
   does not move.

5. **Tranche H rediscovery threshold** (BB.W0 gate; ≥ 80 % of
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
  is "moderately on any individual wave, small as a full-runway
  compound product, and materially stronger at the defensible-floor
  level".

- *Where does a grammar stand at each checkpoint?* The per-grammar
  tables name the expected MB/s (or correctness property) per wave
  and highlight where recovery of AU-baseline is the primary
  outcome versus where BEAT-AU is targeted.

Update cadence: this matrix should be refreshed at every tranche
close (B1 FINAL, AY-II FINAL, AZ-I FINAL, AZ-II FINAL, BA FINAL,
BB FINAL) with the actual-landed column appended and the
remaining-runway probabilities re-estimated against the new
evidence. The architecture/optimisation inventory that explains
these probabilities lives in `docs/tranches/REMAINING-TRAJECTORY.md`.
The calibration anchors above should also be refreshed whenever a
tranche closes on escape, reverts wholesale, or exceeds its
declared gates — each is a data point that updates the base-rate
priors for the remaining runway.
