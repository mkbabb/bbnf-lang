# Tranche B6 — FINAL

B6 closes after W0's mtime-cycle fix landed a 192× speedup on
`cargo xtask regen --grammar bbnf` cold wall (88.26 s → 0.46 s).
W1 + W2 close on rationale-satisfied per SPEC §Plan-time
miscalibration after Phase 0 measurements showed the asserted
baselines (660 s+ for `iter-check-full`; ~20 s for `iter-test`)
were stale by 38× and ~12 % respectively against the post-B5
substrate, and the prescribed levers were structurally incapable
of moving the prescribed metrics regardless of baseline drift.
One source-side commit lands; two waves close on rationale; the
dev-loop floor descends from 5 min cold + 660 s cold + 20 s warm
to 0.46 s + 17 s + 22 s.

## Architectural narrative

B6 was authored as a bounded prelude annex for AY-III. The
charter scoped three measurable drag categories, one wave each:
cold xtask wall (W0), cold `iter-check-full` wall (W1), warm
`iter-test` wall (W2). Under contact, the post-B5 substrate's
actual cost profile diverged from the plan's premises in ways
that compound across all three waves.

### W0 — substrate-wide breakthrough

The W0 plan prescribed a 250-LOC hand-written direct-descent
CSP parser bypassing `BbnfBootstrap::parse` (~3 ms) on the
premise that parser cost dominated the cold xtask wall. Direct
measurement showed the true bottleneck was a self-invalidation
fingerprint cycle: `xtask::regen::regen_grammar` called
`std::fs::write(target_path, &output)` unconditionally, advancing
mtime on `crates/core/src/grammar/generated/bbnf.rs`; the `bbnf`
core crate `include!`s that file, so cargo's fingerprint check
observed the mtime delta and rebuilt `bbnf` (~85 s) on the next
regen invocation, regardless of whether the regen output changed.
The act of regen guaranteed the next regen paid the rebuild cost.

The W0 fix lands at the actual bottleneck: a 35-LOC content-
equality skip in `xtask::regen::regen_grammar`. Read existing
target bytes; compare to emitted bytes; skip the write when they
match. Output is byte-identical by construction; the cycle break
preserves mtime so cargo reuses cached `bbnf` rmeta. Cold wall
collapses 88.26 s → 0.46 s across three runs of the spec-exact
methodology — 192× speedup, 391× under the 3-min gate threshold.

### W1 — plan-time miscalibration

Phase 0 measured cold `iter-check-full` at 17.02 s (38.8× under
the plan's 660 s asserted baseline) and warm at 0.14 s (35×
under the dispatch's 5 s vacuity threshold). Both vacuity
conditions triggered the dispatch's halt-and-report criterion.

Beyond the staleness, the W1 plan's mechanism (egraph
`BackoffScheduler::run` deferred-pass shape for rules whose
`--check` consumer-count is zero) cannot reduce the wall it
targets: every egraph rule is consumed by `bbnf_ir::egraph::
build_and_saturate` at runtime; `cargo iter-check-full` performs
typechecking only, so saturation never executes during the wall
being measured. Lazy-deferring runtime work cannot reduce a
typecheck cold wall. The companion `iter-check-az` alias was a
byte-identical duplicate of `iter-check`. The gorgeous default-
features audit verified `default = []` already in place.

W1 closes on rationale-satisfied per SPEC §Plan-time
miscalibration. No source files modified; no aliases changed.

### W2 — plan-time miscalibration (architectural)

Direct measurement showed warm `iter-test` at 22.353 s (close to
the plan's ~20 s assertion; mild drift), but the slowest-test
census revealed the actual bottleneck:

  bbnf-lsp::bench_lsp::bench_lsp_actions       16.485 s
  simd-scan::fuzz::json_alphabet_skewed        17.794 s
  simd-scan::fuzz::css_with_digraphs           19.5–20.3 s

The W2 plan's prescribed levers cannot move these tests. W2.a
(JSON bench partition) operates on `[[bench]]` targets that
nextest does not run; `cargo iter-test` runs `nextest --workspace`
which picks up `[[test]]` targets only. W2.b (IR audit feature-
gate) targets `payload_layouts.rs` and `projection_totality.rs`,
which do not appear in the slowest-test census; the warm wall
floor is set by `simd-scan::fuzz::css_with_digraphs` at ~20.3 s,
so even removing the IR audits cannot drop the wall below
20.3 s.

The actual bottleneck is bench-class + fuzz-class tests mis-
routed to the routine-iteration surface — a test-surface
partition problem, not a feature-flag problem. Re-routing those
tests is a scope-revealing architectural correction beyond W2's
prescribed levers and beyond B6's annex bounds. Natural
destination: AY-III's close-ceremony surface design.

W2 closes on rationale-satisfied per SPEC §Plan-time
miscalibration.

## Wave-by-wave recap

### W0 — content-equality skip on regen file write (3 commits)

| Commit | One-line |
|--------|----------|
| `5967d37b` | feat(xtask): content-equality skip on regen file write (B6.W0.1) |
| `ca77aae1` | docs(b6): document W0 self-invalidation cycle + cold-wall artefact |
| `1c96a4d5` | docs(b6): W0 close ceremony + scope-reveal record (B6.W0) |

35 LOC change in `xtask/src/regen.rs` (+33 / -2); 9 LOC in
`crates/bootstrap/src/lib.rs` (W0 split point doc); 156 LOC in
`docs/benchmarks/archive/post-B6-W0-walls.txt`. No flags, no parallel
path, no shadow surface — single-line write-skip.

### W1 — plan-time miscalibration close (1 commit, vacuous)

| Commit | One-line |
|--------|----------|
| `f685a9a6` | docs(b6): W1 plan-time miscalibration close + Phase-0 measurements (B6.W1) |

210 LOC in `docs/benchmarks/archive/post-B6-W1-walls.txt`; PROGRESS.md
W1 close section appended. No source files modified.

### W2 — plan-time miscalibration close (1 commit, vacuous)

| Commit | One-line |
|--------|----------|
| `f13f0bc8` | docs(b6): W2 plan-time miscalibration close + Phase-0 measurements (B6.W2) |

233 LOC in `docs/benchmarks/archive/post-B6-W2-walls.txt`; PROGRESS.md
W2 close section appended. No source files modified. (FINAL.md
lands as a follow-on commit; W2.md status-stamp commits last per
the wave-status convention.)

## Performance

The W0 cold-wall measurement, three-run median per SPEC §Hard-
gate measurement methodology:

| Metric | Pre-B6 baseline | Post-B6 measured | Δ |
|---|---:|---:|---:|
| `cargo xtask regen --grammar bbnf` cold | 88.26 s | 0.46 s | -99.5 % (192×) |
| `cargo iter-check-full` cold | (plan 660 s; actual 17 s) | 17.02 s | unchanged |
| `cargo iter-check-full` warm | 0.14 s | 0.14 s | unchanged |
| `cargo iter-check` warm | 0.13 s | 0.13 s | unchanged |
| `cargo iter-test` warm | (plan ~20 s; actual 22 s) | 22.353 s | unchanged |
| `cargo bench-bbnf` median | 2.806 ms | 2.806 ms | unchanged |

Workspace nextest: 1477/1477 passed (27 skipped) at every wave
close. `cargo xtask regen --check` exit 0 across all 9 grammars
at 1.10 s. The hot-path bench `compile_bbnf` is unaffected by
W0's xtask-side change.

## Test results

`cargo nextest run --workspace --profile ax-iter --no-fail-fast`
post-W2: 1477 passed, 0 failed, 27 skipped, 42.4 s wall (inherited
from W0 close ceremony). The 27 skipped tests are pre-existing
release-only / feature-gated fixtures unrelated to B6 scope.
W0's 35-LOC content-equality skip in `xtask::regen::regen_
grammar` does not touch the test surface; the inherited result
holds across W1 + W2 vacuous-close ceremonies.

## API surface changes

Pre/post-B6 surface diff: zero. The W0 commit modifies
`xtask/src/regen.rs` only; downstream consumers see no API
change. Generated parser code regenerates byte-identically by
construction (the content-equality skip writes if and only if
bytes differ; on a steady-state worktree no write fires).

## Cross-tranche debt

**Inherited (closed in B6):**

- Cold `cargo xtask regen --grammar bbnf` self-invalidation
  fingerprint cycle (W0). The 35-LOC content-equality skip ends
  the cycle; cold wall collapses 88.26 s → 0.46 s.

**Vacuous-closed (rationale-satisfied per SPEC §Plan-time
miscalibration):**

- W1's `iter-check-full` ≥ 30 % reduction gate. Plan-asserted
  baseline was 660 s; post-B5 substrate measured 17.02 s.
  Mechanism (egraph lazy-pass deferral) targets runtime work
  that does not execute during typecheck.
- W2's `iter-test` ≥ 20 % reduction gate. Plan-asserted
  bottlenecks (JSON bench expansion + IR audit compile) are
  not the actual bottlenecks (bench-class + fuzz tests on
  routine surface).

**Forwarded:**

None. The slow-test surface boundary observation (W2
reconciliation) is not a deferred-ledger item: AY-III's close-
ceremony surface design (`make ay-bench-close WAVE=close`)
already factors close-vs-routine; the missing piece — partitioning
within the routine tier between fast-iteration and saturation-
fuzz tests — naturally surfaces in AY-III's plan, not B6's.

The transitional `--no-fast` flag the B6 plan named for B7's
restoration wave is not applicable: W0's mechanism is byte-
preserving by construction, so no legacy path needed preservation.
The flag does not land; B7 inherits no flag-retirement debt.

## Defensible floor

Per SPEC §Gate floor-check at plan time, the W0 floor of "≤ 3 min
cold xtask wall" carried 100 % margin above the asserted
~60–90 s structural lower bound. W0's actual landing — 0.46 s —
exceeds the gate's threshold by 391× and the plan's 25 %
conservative floor by far more. W1 + W2's vacuous-close
ceremonies honor SPEC §Plan-time miscalibration: when a numeric
gate's prescribed mechanism is structurally incapable of moving
the metric the gate measures, the wave closes on rationale-
satisfied rather than shipping a gate that cannot close.

The defensible floor at B6 close:

1. Workspace nextest at 1477/1477 green; 27 skipped pre-existing.
2. `cargo bench-bbnf` median holds at 2.806 ms.
3. Cold `cargo xtask regen --grammar bbnf` wall at 0.46 s
   (192× pre-B6 baseline).
4. Cold `cargo iter-check-full` wall at 17.02 s.
5. Warm `cargo iter-check` wall at 0.13 s (under the 0.5 s B6
   invariant 3 ceiling).
6. Warm `cargo iter-test` wall at 22.353 s (slow-test surface
   partition is AY-III's domain).
7. `cargo xtask regen --check` exit 0 across all 9 grammars at
   every wave close.
8. No `#[allow(...)]` introductions outside macro contexts; no
   path duplications; no shim flags; no shadow surfaces.

## Verdict

**B6 closes. AY-III opens against post-W2-close SHA.** Three
waves complete: W0 landed with 192× cold-wall speedup; W1 + W2
close on rationale-satisfied per SPEC §Plan-time miscalibration
after Phase-0 measurements showed the plan-prescribed mechanisms
were structurally incapable of moving the prescribed metrics.
The substrate-wide breakthrough was W0's content-equality skip
— a 35-LOC architectural correction at the actual bottleneck
rather than the asserted bottleneck — which transitively
eliminated the regen-induced rebuild cost that inflated the
W1 + W2 plan-time baselines. The user's "expedite testing/
benching/building" directive is substantively served by W0's
landing; the residual slow-test surface boundary observation
routes to AY-III's close-ceremony surface design as a natural
follow-up.
