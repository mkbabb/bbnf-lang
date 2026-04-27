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
