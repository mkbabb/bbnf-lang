# SKINNY PASS 1 — S-P1 PROFILE (Empirical Excavation Of The SK-V{N} Baseline)

S-P1 is the **profile pass** of the skinny track. It is the empirical
counterpart of the totality T-P1 Excavation: where T-P1 catalogues the
greater-spec surface, S-P1 measures the running JSON engine. S-P1 produces
no plan and proposes no intervention. It produces the **measured truth**
of the current SK-V{N} baseline — the profile artefacts, the
cycles-per-byte ledger, the per-corpus hot-leaf attribution, and the
delta-vs-prior-SK telemetry — upon which S-P2 grounds its primitive
design and S-P3 builds the wave plan.

The pass is **iterative + auto-convergent**. Six parallel sub-agents
P1-A–P1-F fan out per the scope matrix in §2. A six-lens CHALLENGE wave
adversarially reviews the output per `ORCHESTRATOR.md` §3W. Dispositions
fold into V{N+1}. The loop terminates at the convergence criterion in
`ORCHESTRATOR.md` §3Z. Re-execution is composable — S-P1 may re-run
against a fresh baseline without contract drift, and indeed must: per
the profile-first non-negotiable in `ORCHESTRATOR.md` §8, no hypothesis
transfers between SK iterations without a fresh profile of the new
baseline.

## §1 — Trigger + entry condition

S-P1 dispatches when all of the following hold:

- **SK-V{N} opened.** Pass Alpha closed at G-Alpha(N-1→N); the
  `restart/skinny/tranches/sk-v{N}/HANDOFF.md` next-move line reads
  `ready-for-S-P1`.
- **W0 baseline exists.** The SK-V{N} bracket's Wave 0 has produced a
  checked `SK-V{N}-open` baseline in `skinny/RESULTS.md` with no
  placeholder hot leaves — S-P1 profiles a *real* baseline, not a stub.
- **Working tree clean.** No uncommitted source edits; S-P1 is read-only
  against `skinny/` source and writes only under its own output root.
- **Comparators runnable.** sonic-rs strict and serde_json build on the
  host; sidecar comparators (simdjson, yyjson, asmjson, RapidJSON) are
  either runnable or declared `sidecar_freshness=absent:<reason>`.

Or the user explicitly invokes `dispatch S-P1 SK-V{N}`.

The entry condition is profile-readiness, not green performance. S-P1
expects to open against an `N-direct / NoGo` report; that is the normal
SK-V{N} starting state per `restart/skinny/tranches/sk-v8/SPEC.md` §0.1.

## §2 — Scope matrix (six parallel sub-agents)

Output root: `restart/skinny/tranches/sk-v{N}/research/p1/`. Each agent
writes ONE artefact at the assigned path, overwritten in place each
cycle (git history preserves V1, V2, …). Hard cap 45 min per agent.

| Agent | Scope | Output |
|---|---|---|
| **P1-A samply profile — mode I** | samply profiling of the SK-V{N} baseline in profiling mode I (cold per-parse, `parse_only` workload, release + `debug=true`). Profile **every** JSON corpus (§2.1). Per corpus: flame profile artefact path, top-20 self-time symbols, run id, host triple, build flags. | `p1/p1a-samply-mode-1.md` |
| **P1-B samply profile — mode II** | samply profiling in mode II (cold per-parse, `direct_to_struct` + `real_typed_struct` workloads). Same per-corpus discipline; same corpus coverage. The product-plane counterpart of P1-A. | `p1/p1b-samply-mode-2.md` |
| **P1-C samply profile — mode III** | samply profiling in mode III (the masking-probe workloads — `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse` — and the structural-scan-only path). Same per-corpus discipline. The instrumentation-divergence counterpart of P1-A/P1-B. | `p1/p1c-samply-mode-3.md` |
| **P1-D PMU + cycles-per-byte** | PMU counters (cycles, instructions, branch-misses, L1/LLC misses) and derived cycles-per-byte for every corpus × workload. Re-run the masking-probe table from `skinny/RESULTS.md` and attribute each MASKING signal. Establish the c/B baseline that `gate-json` consumes. | `p1/p1d-pmu-cycles.md` |
| **P1-E hot-leaf attribution** | Per-corpus, per-row top self-time symbol synthesis across P1-A/P1-B/P1-C. Resolve every `unprofiled` cell in `skinny/RESULTS.md` to a named symbol + % self-time + file:line. Classify each hot leaf: scan / number / string / unicode / structural / tape / dispatch. | `p1/p1e-hot-leaf-attribution.md` |
| **P1-F RESULTS extraction + delta** | Extract every row of `skinny/RESULTS.md`. Compute per-row Δ vs the prior SK iteration (SK-V{N-1} close). Classify outcome per the schema-v3 enum (A / C / G / K / L / N-direct). Flag every telemetry field absent or stale against `restart/skinny/tranches/sk-v8/SPEC.md` §0.4. | `p1/p1f-results-delta.md` |

Each agent reads, before producing its artefact: `skinny/RESULTS.md`
(the bench-gate authority), `skinny/REDRESS.md` (the rejected-route
ledger), `restart/skinny/tranches/sk-v{N}/HANDOFF.md`, the prior SK
iteration's RESULTS, and this prompt end-to-end. P1-E and P1-F
additionally read P1-A/P1-B/P1-C output if those committed first; if
the orchestrator parallelises all six, P1-E/P1-F consume the committed
artefacts in the CHALLENGE-fold cycle.

### §2.1 — Mandatory corpus coverage (no overfit)

Every profiling agent (P1-A, P1-B, P1-C, P1-D, P1-E) profiles **all
seventeen** JSON corpora, with no exception:

```
twitter   citm_catalog   canada   apache_builds   github_events
update_center   mesh   random   gsoc-2018   marine_ik   instruments
numbers   unicode_mixed   unicode_escapes   unicode_basic
distinct_values   y_string_unicode
```

A profile that covers only the float-heavy rows (canada, mesh,
marine_ik, numbers) and skips the string + unicode rows is **rejected**
by the CHALLENGE CH1 lens. The string corpora (twitter, github_events,
gsoc-2018, distinct_values) and the four unicode corpora
(unicode_mixed, unicode_escapes, unicode_basic, y_string_unicode) carry
the worst current sonic-strict deltas in `skinny/RESULTS.md` and are
therefore the load-bearing rows. Float-heavy overfit is the named
failure mode S-P1 exists to prevent.

### §2.2 — Per-agent output-schema frontmatter

Every P1 artefact opens with this frontmatter block:

```markdown
# SK-V{N} P1-{X}: {Topic}

Pass: S-P1 Profile. Cycle: V{N}.
Date: YYYY-MM-DD.
Scope: {one-line scope spec}.
Output: this file.
Baseline: SK-V{N}-open ({commit SHA of the W0 baseline}).
Host triple: {aarch64-apple-darwin / x86_64-...}.
Build flags: {release profile + debug=true + feature mask}.
Profile tool: {samply version / perf / PMU source}.
Corpus coverage: {17/17 or the explicit subset + reason}.

## §1 — Method (commands run; verbatim, reproducible)
## §2 — Findings (per-corpus table; file:line on every hot-leaf claim)
## §3 — Delta vs SK-V{N-1} (per row; Mbps + c/B + classification)
## §4 — Anomalies + masking signals (flagged for S-P2)
## §5 — Sources (every artefact path + run id)
```

The §2 table is the load-bearing artefact: a profiling agent that
returns prose without a per-corpus per-symbol table fails CH1.

## §3 — Six-lens CHALLENGE pass (CH1–CH6 specialised to S-P1)

After all six P1 artefacts commit, the CHALLENGE wave dispatches per
`ORCHESTRATOR.md` §3W. Six lens agents fan out; each writes
`p1/hardening/V{N}/CH{n}.md`; one aggregator writes
`p1/hardening/HARDENING-S-P1-V{N}-CONSOLIDATED.md`. Disposition
vocabulary is ACCEPT / REVISE / REJECT.

**CH1 CORRECTNESS** — does every hot-leaf claim cite a samply symbol
path + % self-time + source file:line? Are the c/B figures derived from
real PMU counters, not estimated? Is corpus coverage 17/17 for every
profiling agent? Is every `unprofiled` cell from `skinny/RESULTS.md`
resolved to a named symbol?

**CH2 GENERALITY** — does the profile attribute hot leaves to
grammar-neutral primitives (scanner, classifier, tape) rather than
JSON-named code paths? A hot leaf named for a JSON role
(`scan_json_object`) when the underlying symbol is a generic primitive
is a Lock 14 mis-attribution — REVISE. The profile must name the
*primitive*, so S-P2 can ask whether it generalises to CSS L4 / Sheets
/ BBNF-self.

**CH3 REGRESSION** — does any anomaly flagged in §4 silently re-propose
a route already in `skinny/REDRESS.md`? S-P1 proposes nothing, but a §4
"this hot leaf suggests X" that points at a pre-blocked route (REDRESS
50-55, 60-72, 80, 82-84, 88, 89) must cite the entry and mark it
pre-blocked, not implicitly re-open it.

**CH4 COST** — is the profile reproducible? Every §1 method block must
carry verbatim commands a third party can re-run. A profile whose run
id, host triple, or build flags are absent cannot be reproduced and
fails CH4.

**CH5 HIDDEN COUPLING** — does the profile honour the substrate union?
A hot-leaf attribution that implies a parallel substrate (a sidecar
event vector, a second source scan, a retained cursor) must name it as
a Lock 1 observation, not normalise it. Track 1 ≡ generated runtime;
Track 2 is structurally independent — the profile must not conflate
their symbol paths.

**CH6 ANTI-PAPER-CLOSE** — no agent's self-report of "profiled" stands
without the orchestrator-citable artefact: the flame profile file must
exist on disk, the symbol must be resolvable (samply needs `debug=true`
+ interactive `samply record`, not `--save-only`, per the
`samply-symbol-resolution` feedback). A §2 table cell reading
"unprofiled" or "n/a" without a stated cause is a paper-close.

Per `ORCHESTRATOR.md` §3W the lens registry is monotonically
extensible: if S-P1 surfaces a failure mode CH1–CH6 cannot disposition,
add CH7+; never renumber CH1–CH6.

## §4 — Iteration + auto-convergence

S-P1 executes cycles V1, V2, V3, … per `ORCHESTRATOR.md` §3Z. The cycle
counter is per-pass and independent of every other pass's counter.

Per cycle: (1) the six P1 agents dispatch and commit; (2) the CHALLENGE
wave dispatches; (3) the aggregator produces the consolidation with the
ACCEPT-rate and the REVISE/REJECT lists; (4) every disposition folds
into the V{N+1} dispatch — hardening without folding is paper-hardening
and the orchestrator does not advance.

**Convergence criterion.** S-P1 advances to S-P2 when CHALLENGE returns
**≥95% ACCEPT for two consecutive cycles**, with zero open critical
defects and no orphan unresolved REVISE; or the user pins the cycle
final at sign-off (§6).

**Hard ceiling.** V ≤ 5. An S-P1 reaching V5 without convergence
escalates to the user with a `BLOCKED` verdict naming the unresolved
REVISE dispositions — typically an unresolvable symbol path or an
unrunnable comparator.

## §5 — Output structure

```
restart/skinny/tranches/sk-v{N}/research/p1/
├── p1a-samply-mode-1.md
├── p1b-samply-mode-2.md
├── p1c-samply-mode-3.md
├── p1d-pmu-cycles.md
├── p1e-hot-leaf-attribution.md
├── p1f-results-delta.md
└── hardening/
    ├── V{N}/
    │   ├── CH1.md  CH2.md  CH3.md
    │   ├── CH4.md  CH5.md  CH6.md
    └── HARDENING-S-P1-V{N}-CONSOLIDATED.md
```

The flame profile artefacts themselves land outside the doc tree (under
`/tmp/skv{N}-p1/` or the bench harness's profile directory); the P1
artefacts cite their paths. Profile binaries are not committed.

## §6 — Sign-off + hand-on

S-P1 has no mandatory user gate of its own; it is an internal pass of
the SK-V{N} bracket. On convergence the orchestrator:

1. Reads the six P1 artefacts + the consolidation end-to-end.
2. Updates `restart/skinny/tranches/sk-v{N}/HANDOFF.md`: next-move line
   becomes `ready-for-S-P2`.
3. Dispatches S-P2 Research per `skinny/PASS-2-RESEARCH.md`.

S-P2 consumes the S-P1 profile as its empirical floor: every primitive
S-P2 designs must answer to a hot leaf S-P1 named. A primitive with no
P1 hot-leaf antecedent is a speculative kernel and CH1 of S-P2 rejects
it. The chain is **S-P1 (measure) → S-P2 (ground SOTA + design
primitives) → S-P3 (synthesise the wave plan) → wave triumvirate
(execute)**.

## §7 — Hard caps

| Phase | Wall budget |
|---|---|
| Six P1 agents (parallel) | 45 min per agent; ~60 min wall incl. commit |
| CHALLENGE wave (6 + 1 consolidation) | ~90 min wall |
| Per cycle total | ~2.5 hours wall |
| Whole pass (V ≤ 5) | ceiling ~12 hours wall |

Every dispatch carries an explicit minute cap. At 0.9× the cap the
agent commits what it has; at the cap it halts. An overrun surfaces to
the user as an extension decision — the orchestrator engineers no
silent deferral.

## §8 — Bbnf-lang specific axes for S-P1

1. **Cold per-parse only.** Per the `no-warm-benches` feedback, every
   profiling mode measures a cold parse — no warmed cache, no
   amortised allocation. Warm benches are disingenuous and CH6 rejects
   any §1 method block that warms the corpus before measurement.
2. **Sequential, single-invocation bench.** Per the
   `bench-sequential-regression` + `bench-single-run` feedback,
   profiling runs the bench sequentially in a single invocation;
   profiling agents do not re-invoke cargo per corpus filter. Long
   runs redirect output to one file, then grep over the file
   (`test-output-to-file` feedback).
3. **samply discipline.** Per `samply-symbol-resolution`: samply needs
   `debug=true` in the profile and an interactive `samply record`
   invocation; `--save-only` loses symbol resolution. Every P1-A/B/C
   §1 method block names the exact `samply record` command.
4. **Single cargo per target.** Per `single-cargo-per-target`, at most
   one cargo invocation is in flight per `CARGO_TARGET_DIR` at any
   instant; profiling agents that share a target directory serialise.
5. **The masking probes are first-class.** The `host_call_eager_decode`
   / `alternate_scalar_plan` MASKING signals in `skinny/RESULTS.md` are
   not noise — P1-C and P1-D attribute each one, because a masking
   probe that beats Track 1 names a structural inefficiency S-P2 must
   research.
6. **Profile the substrate union.** The offset-tape, the lazy
   materialisation counters, and the structural projection are one
   substrate (Lock 1). P1-E attributes tape symbols as substrate, not
   as a separable producer.

## §9 — Closing posture

S-P1 is the measure pass. It prescribes nothing and designs nothing; it
makes the SK-V{N} baseline *legible*. Every hot leaf is named to a
symbol; every symbol is named to a file:line; every corpus is covered;
every delta is computed against the prior iteration. The profile is
reproducible by construction.

No S-P2 dispatch without S-P1 convergence. No hot-leaf claim without a
samply symbol path. No corpus skipped. No warm bench. No hypothesis —
S-P1 produces evidence, and S-P2 produces the hypotheses the evidence
will or will not support.

The work is bounded by the bench. The bench is cold. The truth is the
profile.
