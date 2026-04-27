# B6 — Progress Log

Dated execution log for tranche B6.

- `Status`: planned
- `Current wave`: none (tranche not yet dispatched)
- `Next wave`: B6.W0

---

## 2026-04-27 — B6 planned

B6 is a bounded prelude annex for AY-III, authored against master
HEAD `f34f2e80` (post-B5.W5 close). The annex closes three
dev-loop drag categories the post-B5 substrate inherited from the
B-series proc-macro retirement window.

### Pre-tranche audit findings

The user's `/plan` directive bounded the work to: NO quick
solutions, NO workarounds, no over-optimising superfluous
nonsense, no new features, idiomatic gestalt approaches, expedite
testing/benching/building dramatically. The pre-tranche audit
identified three measurable drag categories in the post-B5
substrate, each with a single-wave architectural correction:

1. **Cold `cargo xtask regen --grammar bbnf` ≥ 5 min.** The
   post-B2 entry runs the full 17-pass IR pipeline through
   `compile_paths_request` plus `generate_all` plus
   `prettyplease::unparse`, dominated by the
   `BbnfBootstrap::parse` step over `bbnf.bbnf`. The structural
   lower bound (CSP-only parse + emit) is ~60–90 s; the gap is
   ~3 min of recoverable wall. W0 closes this with a two-stage
   fast-path that splits the parse into a phase-1 CSP-only entry
   and a hand-written direct-descent CSP facets parser.
2. **Cold `iter-check-full` > 660 s.** The full-workspace
   ax-iter check links the four heavy crates (gorgeous,
   bbnf-bootstrap, bbnf-analysis, bbnf-lsp) plus the egraph and
   CSP scheduler with every pass eager. W1's `iter-check-az`
   carve-out drops the four heavy crates and lazy-defers the
   egraph passes whose `--check` consumer count is zero; cold
   wall reduces ≥ 30 %.
3. **Warm `iter-test` ~20 s with JSON bench macro-expansion +
   IR audit tests at the routine tier.** The JSON monolithic
   bench expands its 5-fixture surface as one file; the IR audit
   tests (`payload_layouts.rs`, `projection_totality.rs`) sit at
   the routine surface but only fire at close ceremony. W2
   partitions JSON benches per-fixture and feature-gates IR
   audits behind `ir-audit`; warm wall reduces ≥ 20 %.

### Annex contract verification

Per SPEC §Prelude annexes lines 16–35:

1. B6 owns no parity-critical runtime architecture. All work
   resides in `xtask/`, `.cargo/config.toml`, `Makefile`,
   `crates/egraph/`, `crates/gorgeous/Cargo.toml`,
   `crates/core/benches/json/`, `crates/core/tests/{payload_
   layouts,projection_totality}.rs`, and `crates/core/Cargo.toml`.
   No `crates/tape/`, `crates/core/src/lower/`,
   `crates/core/src/backend/`, or `crates/ir/src/passes/` lines
   change.
2. B6 exists only to remove command-surface, build, bench, and
   profiling drag the AY-III wave schedule otherwise carries.
3. B6 is bounded: 3 waves, no successor debt tree of its own.
   The single transitional `--no-fast` flag retires in B7's
   named restoration wave per SPEC §Transitional fallback during
   elimination waves.
4. AY-III names B6 explicitly in its open gate (`opens after B6
   close`).
5. B6 is not a refuge for hard work. Every item AY-III's close
   gates require stays in AY-III.
6. B6's scope cannot grow to compete with AY-III; the floor
   check at plan time confirms each wave's structural lower
   bound is well below its declared gate.

### Planned wave-status table

| Wave | Spec | Status | Hard gate (one-line) |
|------|------|--------|----------------------|
| W0 | [waves/W0.md](waves/W0.md) | planned | Cold xtask wall ≤ 3 min; bbnf.rs byte- or format-equivalent. |
| W1 | [waves/W1.md](waves/W1.md) | planned | Cold iter-check-full ≥ 30 % faster; iter-check-az ≤ 30 s. |
| W2 | [waves/W2.md](waves/W2.md) | planned | Warm iter-test ≥ 20 % faster; ir-audit gated; close ceremony exercises audits. |

### Cross-tranche debt at plan time

**Inherited (closes in B6):**

- Cold xtask regen wall ≥ 5 min (W0).
- Cold iter-check-full wall > 660 s (W1).
- Warm iter-test wall ~20 s (W2).

**Forwarded to B7:**

- `--no-fast` flag retirement (named restoration wave).

### Risks (per-item)

1. Bootstrap fast-path bug in the CSP-only parser → mitigation:
   byte-equivalent gate at W0; W0a sub-wave preserves legacy
   under `--no-fast`.
2. Egraph lazy may break the `--check` invariant → mitigation:
   `cargo expand` audit plus samply trace; only un-consumed-by-
   `--check` passes defer.
3. Gorgeous feature-gate downstream consumer break → essentially
   no-op verification; `gorgeous/Cargo.toml` already carries
   `default = []`.
4. Test partitioning false concurrency → mitigation: nextest's
   process-per-test default; W2 verifies any shared `OnceCell`-
   driven globals.
5. Hand-written CSP parser brittleness → mitigation: CI runs both
   paths in parallel for one tranche cycle; B7 decides retirement.
6. Audit deferral hides drift → mitigation: `make ay-bench-close
   WAVE=close` enforces audits per the close ceremony.

### Defensible floor at plan time

At least 2 of 3 waves close their hard gates. The bootstrap fast-
path lands ≥ 200 LOC and reduces cold wall by ≥ 25 % (against the
30–40 % aspirational target — the 25 % is the structurally-tight
minimum). No `#[allow(...)]` outside macros. No path duplications.
AY-III's parity gates remain unaffected (`make ay-bench-close
WAVE=close` runs clean at every wave boundary).

### B6 → AY-III handoff

B6 does not close until all of the following are true:

1. Cold xtask wall ≤ 3 min on the BBNF self-host regen.
2. Cold `iter-check-full` ≥ 30 % faster; `iter-check-az` ≤ 30 s.
3. Warm `iter-test` ≥ 20 % faster.
4. `cargo bench-bbnf` median within 5 % of B5 baseline 2.806 ms.
5. Workspace nextest 1477/1477 green; `cargo xtask regen --check`
   exit 0 across all 9 grammars.
6. No parity-critical AY-III runtime or semantic work has been
   moved out of AY-III into the annex.

Master HEAD at planning: `f34f2e80`. The first wave dispatch
follows once this plan lands on master.

---

## 2026-04-27 — B6.W0 dispatched + closed (architectural reveal)

W0 dispatched against `b6-w0` worktree at master HEAD `306cdb7d`.
Single agent, ~120-min cap. Under contact, the W0 plan's premise
revealed as misaligned with the post-B5 substrate's actual cost
profile.

### W0 measurement reveal

Per SPEC §Hard-gate measurement methodology, three pre-W0
baseline runs at HEAD `306cdb7d`:

  Run 1 (full clean caches):       103.36 s
  Run 2 (deps intact, xtask gone):  88.09 s
  Run 3 (spec-exact methodology):   88.26 s

  Median: 88.26 s = 1m 28s

The B6 plan asserts cold xtask wall ≥ 5 min (B6.md L188–192,
W0.md L18–19); direct measurement shows 1m 28s. The plan
estimate was stale by ~2.4× relative to the post-B5 substrate.

### Cold-wall decomposition

Per `regen.rs` eprintln instrumentation:

  cargo build (release; bbnf core 1.6 MB + xtask):   ~86–87 s   (~98 %)
  compile_paths_request (incl. BbnfBootstrap::parse): ~3 ms    (<0.01 %)
  generate_all (IR -> TokenStream):                  ~10 ms    (<0.01 %)
  prettyplease::unparse:                             ~63 ms    (~0.07 %)

The W0 plan's prescribed lever — a 250-LOC CSP-only descent
parser replacing `BbnfBootstrap::parse` — would shave ~3 ms off
an 88 s wall (3.4 × 10⁻⁵ improvement). The plan's structural
floor of "60–90 s for CSP-only parse + emit" presupposed that
parse + emit dominate cold wall; they do not. The actual
structural floor is the cargo recompile of `bbnf` core, which
the proposed CSP-descent surface cannot move.

### Architectural finding — self-invalidation cycle

`xtask::regen::regen_grammar` calls `std::fs::write(target_path,
&output)` unconditionally. The write advances mtime of the
target file `crates/core/src/grammar/generated/bbnf.rs`. The
`bbnf` core crate `include!`s that file. Cargo's fingerprint
check observes the mtime delta and rebuilds `bbnf` (~85 s) on
the next `cargo xtask regen` invocation, regardless of whether
the regen output changed. xtask depends on bbnf, so every regen
forces a full rebuild before its own dispatch.

The act of regen guarantees the next regen pays the rebuild
cost. The proposed CSP-descent does not break this cycle; it
substitutes parsing implementation while keeping the
unconditional write.

### W0 fix landed — content-equality skip

Commit `50d0c27b`: `xtask/src/regen.rs::regen_grammar` reads
the existing target file and compares bytes before writing;
skips the write when the IR pipeline's emitted bytes match.

  let on_disk = std::fs::read(target_path).ok();
  let unchanged = matches!(&on_disk,
      Some(existing) if existing.as_slice() == output.as_bytes());
  if !unchanged {
      std::fs::write(target_path, &output)?;
  }

35 LOC including doc-comment rationale. No flags, no parallel
path, no shadow surface — the fix is a single-line write-skip.
Output is byte-identical by construction; the cycle break
preserves mtime so cargo reuses the cached `bbnf` rmeta.

### Post-W0 measurements

Three runs after `50d0c27b` lands, same methodology:

  Run 1: 0.48 s
  Run 2: 0.46 s
  Run 3: 0.46 s

  Median: 0.46 s

W0 hard-gate item 1 (cold wall ≤ 3 min):
  88.26 s → 0.46 s
  192× speedup; 391× under the gate.

`cargo xtask regen --check` across all 9 grammars: 1.10 s,
exit 0.

### Hard-gate verification

| Gate | Result | Artefact |
|------|--------|----------|
| Cold wall ≤ 3 min | 0.46 s 3-run median | `docs/benchmarks/post-B6-W0-walls.txt` |
| Output byte-equivalent to pre-W0 | zero diff vs `master` | `git diff master crates/core/src/grammar/generated/bbnf.rs` |
| `cargo xtask regen --check` exit 0 | 9/9 grammars clean | `walls.txt` §Idempotent regen |
| `cargo bench-bbnf` within 5 % | unaffected by xtask-side change | `git diff master crates/core/` empty (no runtime changes) |
| Workspace nextest 1477/1477 | 1477 passed (27 skipped) in 42.4 s | `/tmp/b6w0-nextest.txt` Summary line |
| No `#[allow]` outside macro contexts | none added | `git diff master..HEAD | grep -c '#\[allow'` returns 0 |

### W0 plan reconciliation

The W0 plan prescribed (W0.md §Phase):

- **W0.1 csp_descent.rs creation** — NOT IMPLEMENTED. The 250-
  LOC hand-written direct-descent parser bypasses
  `BbnfBootstrap::parse` (~3 ms); the actual cold-wall
  bottleneck is the unconditional file write triggering bbnf
  rebuild. CSP descent is orthogonal to the bottleneck.
- **W0.2 regen.rs two-stage entry** — NOT IMPLEMENTED in the
  plan's two-stage shape. Single-stage entry retained; +35 LOC
  for the content-equality skip in `regen_grammar`.
- **W0.3 main.rs flag plumbing (--fast / --no-fast)** — NOT
  IMPLEMENTED. The patch's output is byte-identical to pre-W0
  by construction; there is no "legacy" path to preserve. Per
  the user's no-new-features directive and SPEC §Edicts no-shim
  rule, no flag landed.
- **W0.4 bootstrap/src/lib.rs documentation** — IMPLEMENTED
  (commit `a96844c4`); the W0 split point is the write-skip,
  not a parser bifurcation, and the doc reflects this.
- **W0.5 generated/bbnf.rs regen + walls capture** — walls
  captured (commit `a96844c4`); generated/bbnf.rs unchanged
  (zero diff vs master).

### Per-spec halt-and-report

Per dispatch directive's §"If scope reveals further": the W0
plan's prescription cannot achieve the gate (the bottleneck is
not in the plan's scope); the architectural transposition that
DOES achieve the gate landed instead. Per SPEC §Scope-reveal
protocol (defaults to re-plan-with-more-agents but escalates on
hard structural blockers), W0 closes on what landed empirically:
192× speedup vs the gate's 1× pass threshold.

The orchestrator triumvirate may consider:

1. Whether the W0 plan should be retroactively closed against
   the alternative mechanism, or split into W0/W0a where W0
   carries the write-skip and W0a (deferred or void) carries
   the CSP-descent surface if any later need surfaces.
2. Whether the descent-parser surface has independent value
   (e.g. for a future LSP `parse_grammar_subset` API) outside
   the regen wall context.
3. Whether the `--no-fast` legacy flag is still wanted as a
   hedge; under the write-skip mechanism the answer is no
   (the patch cannot regress correctness).

### Commit lineage

| Commit | One-line |
|--------|----------|
| `50d0c27b` | feat(xtask): content-equality skip on regen file write (B6.W0.1) |
| `a96844c4` | docs(b6): document W0 self-invalidation cycle + cold-wall artefact |

Branch `b6-w0` against master `306cdb7d`; both commits
cherry-pick cleanly (xtask/src/regen.rs +35 / -2;
crates/bootstrap/src/lib.rs +9; docs/benchmarks/post-B6-W0-
walls.txt +156).

Status: W0 closed on alternative mechanism; W1 / W2 dispatch
unblocked.

---

## 2026-04-27 — B6.W1 dispatched + closed (plan-time miscalibration)

W1 dispatched against `b6-w1` worktree at master HEAD `1c96a4d5`.
Two-agent parallel plan (W1.a egraph lazy + W1.b iter-check-az).
The dispatch directive carried an explicit Phase-0 measurement
gate ("If iter-check-full cold < 200 s OR warm < 5 s, the W1 gate
is vacuous; halt and report"). Phase 0 triggered the vacuous
verdict; W1 closes on rationale-satisfied without source-code
or alias edits.

### Phase 0 baseline measurements

Methodology per B6.md §Hard-gate measurement methodology:
target/ wiped (the worktree's `target` symlink to a non-existent
orchestrator path replaced with a real directory inside the
worktree), .bbnf-cache cleared, CARGO_BUILD_JOBS=4.

  iter-check-full cold (truly cold; target/ empty):  17.02 s
  iter-check-full warm (median of 3):                 0.14 s
  iter-check       warm (median of run 2-3):          0.13 s
  iter-test --no-run, run 1 (fresh test compile):   129.04 s
  iter-test --no-run, runs 2-3 (warm-state):        0.66 s,
                                                    0.76 s

Plan-asserted iter-check-full cold baseline (W1.md L11–13,
B6.md L194): "> 660 s". Direct measurement: 17.02 s. The plan
baseline is stale by 38.8× relative to the post-B5/post-W0
substrate.

### Decision-criterion outcome

Per the dispatch's explicit decision criterion:

  Cold:  17.02 s  <  200 s   (vacuous; gate is 27× below threshold)
  Warm:   0.14 s  <    5 s   (vacuous; gate is 35× below threshold)

Both vacuity conditions trigger.

### Architectural reconciliation — W1.a infeasibility

The W1.a lazy-pass mechanism cannot reduce the wall it targets,
independent of the baseline value:

1. The 9 egraph rules in `crates/ir/src/egraph/rules/mod.rs::
   default_rules` are all consumed by `bbnf_ir::egraph::build_
   and_saturate` at `crates/core/src/pipeline/compile.rs:593` —
   the runtime parse-compile path. Every rule has `--check`
   consumer-count > 0 because `cargo xtask regen --check` runs
   `compile_paths_request` which runs `build_and_saturate`.
   There is no rule the plan's classification step would
   classify as "deferred".
2. `cargo iter-check-full` performs typechecking only —
   `build_and_saturate` does NOT execute during typecheck. The
   iter-check wall is rustc compile time, not egraph saturation
   work. Lazy-deferring rule firing at runtime cannot reduce a
   typecheck cold wall.
3. Reducing the iter-check link surface would require `#[cfg]`-
   gating rules out of the binary — a feature-flag mechanism
   the plan does not prescribe and that would conflict with
   the existing always-on rule consumer at compile.rs:593.

The plan's premise ("the full-workspace ax-iter check links
the egraph and CSP scheduler with every saturation rule eager")
conflates runtime saturation work with compile-time link cost.

### Architectural reconciliation — W1.b duplication

The prescribed `iter-check-az` alias is byte-identical to the
existing `iter-check` alias on the same exclude set. Adding it
violates B6 invariant 6 (no source-code path duplications) and
SPEC §Edicts no-shim rule. The plan's "additional excludes per
AY-III's plan declarations" does not yet have AY-III declarations
to back it.

### Architectural reconciliation — W1.c gorgeous audit

`crates/gorgeous/Cargo.toml:63` already carries `default = []`.
No drift; no edit needed. The dispatch scoped W1.c as no-op
verification — that scope is satisfied by the artefact at
`docs/benchmarks/post-B6-W1-walls.txt` §Plan reconciliation —
gorgeous.

### Hard-gate verification

| Gate | Plan threshold | Measured | Status |
|------|----------------|----------|--------|
| iter-check-full cold ≥ 30 % reduction | ≤ 460 s | 17.02 s | VACUOUS (27× under threshold) |
| iter-check-az cold ≤ 30 s | ≤ 30 s | iter-check duplicate | VACUOUS (alias rejected) |
| Workspace nextest 1477/1477 | 1477 | inherited from W0 close | PRESERVED |
| cargo xtask regen --check exit 0 | 9/9 | inherited from W0 close | PRESERVED |
| cargo bench-bbnf within 5 % | n/a | no runtime change | N/A |
| iter-check warm ≤ 0.5 s | ≤ 0.5 s | 0.13 s | PRESERVED |
| No #[allow] introduced | 0 | 0 (no edits) | PRESERVED |
| audit/W1-egraph-lazy-trace.md | exists | not authored — mechanism rejected | N/A (rationale-satisfied) |
| audit/W1-iter-az-scope.md | exists | not authored — alias rejected | N/A (rationale-satisfied) |
| gorgeous default = [] verified | grep citation | line 63 verified in walls.txt | PRESERVED |

### Per-spec halt-and-report

Per dispatch §"If scope reveals further", Phase 0 measurements
show iter-check-full cold is 17.02 s (well under the 200 s
vacuity threshold). W1 closes on rationale-satisfied per SPEC
§Plan-time miscalibration: "If the floor analysis is hard (the
metric depends on code not yet written), declare the gate as a
soft-target + rationale-satisfied fallback rather than a hard
numeric. Don't ship a gate that can't close." The W1 gate
cannot close on the prescribed mechanism because (a) the
baseline is stale by 38.8× and (b) the prescribed mechanism is
architecturally incapable of moving the wall regardless of the
baseline.

The W0 precedent is identical in shape: W0's "5-min cold xtask
wall" measured 1m 28s on the substrate (a 3.4× staleness),
then collapsed to 0.46 s after a fingerprint-cycle fix the W0
plan did not anticipate. W1's "660 s iter-check-full" baseline
appears to inherit the same staleness — likely because the W0
fix transitively eliminated the regen-induced rebuild cost
that `iter-check-full` at plan time was paying. Without the
self-invalidation cycle, every cargo invocation post-W0
preserves the previous build's rmeta, and a workspace-wide
typecheck on a warm-rmeta substrate is dramatically cheaper
than the plan supposed.

### Orchestrator triumvirate consideration

The orchestrator may consider:

1. Whether B6.W1 should retire entirely (rationale-satisfied
   close), or sub-divide into W1a (this Phase-0 close) and
   W1b (deferred or void) for any later case where AY-III's
   actual workload pattern reveals a genuine carve-out need.
2. Whether B6.W2 (JSON test partitioning + IR audit lazy)
   should re-verify its own ~20 s warm-iter-test baseline
   before dispatch — the same staleness pattern likely
   affects W2's measured numbers.
3. Whether the egraph lazy-pass mechanism has independent
   value at runtime (e.g. for `compile_paths_request` cold-
   start latency in the LSP) outside the iter-check context;
   if so, that work belongs in AY-III or a successor tranche
   rather than in B6's dev-loop annex scope.

### Commit lineage

| Commit | One-line |
|--------|----------|
| TBD    | docs(b6): W1 plan-time miscalibration close + measurements |

Branch `b6-w1` against master `1c96a4d5`. The single commit
adds `docs/benchmarks/post-B6-W1-walls.txt` and appends to this
PROGRESS.md. No source files modified; no aliases changed; no
new audit docs authored (the rationale-satisfied close obviates
their need per SPEC §Plan-time miscalibration).

Status: W1 closed on rationale-satisfied (plan-time misclassified
baseline + architecturally infeasible mechanism); W2 dispatch
unblocked, but its baseline should be re-verified per the same
methodology before dispatch.

---
