# AY-II — Path Forward (2026-04-22)

Concrete, ordered work that closes AY-II on the now-working dev-loop
surface (post-`700501f5`). Every item is addressable by one agent or
one small sub-triumvirate. Each carries its own runtime-verifiable gate.

## Preconditions (verified)

- Master HEAD: `133a87ee`.
- `cargo iter-check` cold 11.3s / warm 0.14s / touch-cascade 1.6s —
  B0's spec restored.
- W0'.a substrate + W0'.b consumer wiring + W0'.c scan-policy splice +
  W0'.d1 test migration + W0'.d3 O(1) close + W0'.d4-d7 infra fix all
  landed.
- 100/100 tape tests green.
- All open triumvirate tasks closed.

## Ordered work items

### 1. Bootstrap regen under d3 + d7 (blocks 2, 3, 4)

**Gate**: `rm -rf target/.bbnf-cache && time bash scripts/bootstrap-bbnf.sh`
completes ≤ 3 min wall-clock; double-regen cycle-1 = cycle-2 byte-
identical MD5.

**Expected**: the d3 O(1) `value_end_compound` closes the O(N²) parse
hot path; cycle time drops from the pre-d3 15-min stall to the pre-
W0'.a 3–5 min baseline.

**If stall persists** (> 5 min): dispatch a narrow research agent per
SPEC §Diagnostic-loop relinquish; probes land under
`docs/tranches/AY-II/audit/W0p-regen-cycle2-*.md`. Do not cross-wire
with infra work; regen is its own perf domain.

**Artefacts**: `target/expand/bbnf_monolithic.rs`, `md5` of both cycles,
regen wall-clock in PROGRESS.md W0' close entry.

### 2. Retire W0'.a transient compose-escape aliases (task #16)

**Scope**: four aliases exist only to let pre-regen `generated.rs`
compile against the post-W0'.a type names. After the successful regen
in item 1, the generated.rs itself uses the canonical names, so the
bridges retire.

**Sites**:
- `crates/tape/src/builder/mod.rs:1203` — `pub type TapeBuilder = FusedBuilder;`
- `crates/core/src/runtime/mod.rs:46` — `pub type ValueBuilderOutput<R> = FusedOutput<R>;`
- `crates/core/src/runtime/mod.rs:62-139` — the entire `value_builder`
  module shim (`_ValueBuilderShim` ZST + `ValueBuilder<R>` alias +
  counter re-exports)
- `crates/core/src/runtime/parsed.rs:173` — 4-arg `new_fused` shim
  (keeps `new_fused_output`)
- `crates/core/tests/value_api_apples_to_apples.rs:219` — migrate the
  counter import from `bbnf::runtime::value_builder::{reset_*,
  value_builder_new_call_count}` to the FusedBuilder counter accessors
  exported directly under `tape::builder::*`.

**Gate**:
- `rg 'pub type ValueBuilder|pub type ValueBuilderOutput|pub struct ValueBuilder|_ValueBuilderShim|pub type TapeBuilder' crates/` → 0 matches.
- `rg 'pub fn new_fused\b' crates/core/src/runtime/parsed.rs` → 0 (only `new_fused_output` survives).
- `cargo iter-check` clean.
- `cargo test -p tape --tests` 100/100 green.
- `cargo test -p bbnf --test value_api_apples_to_apples --release` green (verifies the counter migration).

**Commit**: `refactor(runtime): retire W0'.a compose-escape aliases (AY-II.W0'.e1)`.

### 3. W0' close ceremony (task #18)

Runs AFTER items 1 + 2 land. Per `waves/W0p.md` §Orchestrator-owned
close ceremony.

**Steps**:

1. Fresh expands per grammar:
   ```
   cargo expand -p bbnf --bench json_monolithic
     > target/expand/ay-ii-W0p-json.rs
   cargo expand -p bbnf --bench css_l4
     > target/expand/ay-ii-W0p-css-l4.rs
   cargo expand -p bbnf --bench google_sheets_monolithic
     > target/expand/ay-ii-W0p-sheets.rs
   cargo expand -p bbnf --bench bbnf_monolithic
     > target/expand/ay-ii-W0p-bbnf.rs
   ```
   Each expand < 3 min post-d3+d7.

2. Fat-LTO 5-bench matrix:
   ```
   make ay-bench-close WAVE=W0p-close
   ```
   Writes `docs/benchmarks/post-AY-W0p-close-{json,css,sheets,bbnf,compile}.txt`.

3. Samply per primary grammar:
   ```
   CARGO_TARGET_DIR=... make ay-prepare-profile-wave
   make ay-samply-json-twitter WAVE=AY-II-W0p
   (+ css-tailwind, sheets-stress, bbnf-self equivalents)
   ```
   Artefacts under `.profiles/samply/AY-II-W0p/{json_twitter,css_tailwind,sheets_stress,bbnf_self}/`.

4. nm per bench binary:
   ```
   for B in json_monolithic css_l4 google_sheets_monolithic bbnf_monolithic; do
     nm target/bench/deps/${B}-* | grep -E 'ValueBuilder|push_compound|FusedBuilder' >> docs/benchmarks/post-AY-II-W0p-nm.txt
   done
   ```
   Expected: `ValueBuilder` absent, `push_compound` absent, `FusedBuilder` present.

5. PROGRESS.md W0' close entry + `waves/W0p.md` status `in_progress` → `closed`.

**Gate**: all of W0p.md §Hard gate (1-10).

### 4. Wave execution (W1 → W2 → W3 → W4 → W5)

Sequential, each dispatching its own sub-agent set per
`docs/tranches/AY-II/waves/W<N>.md`. Orchestrator cherry-picks +
tracks. No agent gets dispatched until the prior wave closes.

**Expected duration** (with d7 iteration loop):
- W1 (JSON): 1-2 sessions.
- W2 (CSS L4): 2 sessions (typed parity is broad).
- W3 (Sheets): 1 session.
- W4 (BBNF): 1 session.
- W5 (cross-matrix + FINAL): 1 session.

### 5. B1 build-infra audit (parallel to W1)

`docs/tranches/B1/` — a prelude-annex successor to B0, opened to
comprehensively audit the B0 surface that W0'.d7 surfaced as partial.
Brief: `docs/tranches/B1/AGENT_BRIEF.md`.

B1 DOES NOT block AY-II close. B1's scope is strictly build/test/
bench/expand/profile infrastructure — parity-critical runtime work
stays in AY-II. If B1 surfaces a test or bench regression mid-flight,
it redresses it in-annex; if it surfaces runtime behavior drift, it
escalates to the owning AY-II wave.

## Discipline

- **Use `cargo iter-check` / `iter-test-leaf` / `iter-test-grammar`
  for routine work.** `iter-check-full` / `cargo test --workspace` /
  `ay-bench-close WAVE=close` are the close-proof surface only.
- **Before any heavy command**, check if the routine surface suffices
  — typically it does. `scripts/bootstrap-bbnf.sh`, `cargo expand`,
  fat-LTO bench, and samply are the four operations that legitimately
  cost minutes; everything else should be seconds.
- **Hard caps on all agents**: research 20 min, plan 15 min, redress
  30 min. Halt + report partial findings at cap.

## Audit trail

- Research attributions: `docs/tranches/AY-II/audit/W0p-regen-root-cause.md` + `W0p-infra-root-cause.md`.
- Fix plans: `W0p-regen-fix-plan.md` + `W0p-infra-fix-plan.md`.
- Verification: `W0-iter-surface-verification.md`.
- Diagnostic (superseded): `W0p-regen-diagnostic.md`.

All audit docs stay in `AY-II/audit/` per the multi-pass-tranche edict
(research/plan artefacts belong to their authoring pass).
