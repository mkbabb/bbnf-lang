# AZ-I.W2-close audit synthesis — refined path forward

This document folds the six parallel audits at
`AUDIT-{1..6}-*.md` into one binding refined trajectory. It
supersedes the W3 / W4 wave docs and the AZ-II three-wave shape;
it is consumed by the next dispatch.

The audits converged on three meta-findings: the codebase is
healthier than the planning doc set suggests; the planning
ceremony manufactures wave count that the architecture does not
require; and the AZ-I.W2 substrate-only close violates the SPEC's
own activation-gate invariant. The refined path retires the wave
ceremony, lands the activation in one motion, folds AZ-II into a
single cutover wave, and runs BB rule inference in parallel with
the cutover.

## 1. Convergent meta-finding

Every audit, on its own lens, named the same root cause: AZ-I has
been treating the wave structure as a unit of caution rather than
a unit of work. The W2 substrate landed nine per-shape struct-
direct emitters (Object / Array / Number / String / Scalar /
Keyword / Wrap / AltDispatch / Flat) plus the StructBuilder trait,
the JSON runtime, the EmitStrategy substrate, the parse_body
two-path emission, the dispatcher signature threading, and the
parity harness scaffold — all on disjoint file bounds, all
compile-clean, all test-green at 1546 / 1546 workspace nextest.
The activation gap is a single resolver-arm flip plus three
downstream consumer migrations, not a wave.

The substrate-vs-consumer ratio (`AUDIT-2`) is 9 / 44 production-
consumed (~20%); the W2-act follow-on flips ~25 of the 35 dead-
or-test-only items in one pass. The trajectory simplification
(`AUDIT-5`) collapses 17 declared waves to 8; this synthesis
pushes further to 5 by folding W4 FINAL into the activation close
and running BB scaffold against post-W1 IR.

## 2. The refined path — 5 waves total to AZ-I + AZ-II close

| Letter | Name | Scope | Hard gate | Wall (est.) | Opens after |
|---|---|---|---|---|---|
| **AZ-I.W2-act** | GESTALT-ACTIVATE | Resolver flip activates JSON + Sheets + CSS L4 simultaneously; JsonDocument + SheetsValue + CssTypedValue accessor APIs; parity harness recoding (sonic-rs / simdjson / serde_json / lightningcss / cssparser); cargo bench gate (twitter ≥ 1967, canada ≥ 1231, citm ≥ 2438; CSS normalize ≥ 735, bootstrap ≥ 600, tailwind ≥ 500; Sheets parse_simple ≥ 95); EmitStrategy hoist to `bbnf-ir::registry::strategy`; W2.RE panic quartet retires by landing real Flat / HRegex struct-direct (Sheets + CSS L4 surface them); dead-substrate sweep (delete `audit_payload_coverage` if unwired post-flip + `registry_observer` per its own docstring); W4 FINAL absorbed (matrix re-run + AZ-II handoff verification in close ceremony). | All seventeen 17-entry-matrix entries at AU floor on struct-only path; parity harnesses green; tape-symbol grep returns BBNF-only hits; nextest workspace unchanged or +activation-test count; `cargo xtask regen` byte-stable. | 4-7 days | AZ-I.W2 substrate (current master `8b2b2709`) |
| **AZ-II.cutover** | BBNF self-host + tape deletion | One-pass Stage A + B byte-equal cutover; `crates/tape/` deletion; `tape::dta` hoist to `bbnf-ir::dta` consumed bidirectionally; `tape::visitor` family deletion (7 traits + 746 LOC); BBNF-grammar typed-leaf authoring closes `StructRegistry` for BBNF; `bbnf-ser` audit (single-consumer fold-in or justified retain). | Workspace builds without `crates/tape/` on disk; `cargo build -p bbnf --no-default-features` succeeds; bootstrap byte-equal cycle holds; nextest workspace green. | 5-10 days | AZ-I.W2-act close (substrate-clean prerequisite) |
| **AZ-II.cutover-parallel-BB** | BB scaffold opens against post-AZ-I-W2-act IR | Per the trajectory note that BB is substrate-independent on `IrNode`. Lands rule storage at `crates/ir/src/rewrites/`, the e-graph→VM-residue oracle skeleton, the cost-model interface, the per-grammar rule inference loop. | First inferred rule fires on a fixture grammar; e-graph hit-rate measurement gate per BB.W0 plan. | parallel ~3-5 days | parallel with AZ-II.cutover (no shared file bounds) |
| **BA** | Lazy typed pointer-path queries | Path IR + type checker + `path!` macro over JSON / CSS / Sheets / BBNF struct trees; parent-pointer-vs-root-traversal micro-bench on opening preflight (folded from BA.W-1 + BA.W0); host bindings (Rust + TS + Python) under one isomorphic-API close. | `path!("store", "books", 0, "title")` resolves at compile time; zero-allocation traversal; bench surface beats sonic-rs `pointer!` on extracted-field micro-benches. | 5-10 days | AZ-II.cutover close |
| **BB.close** | Rule inference + ranking + emission | Folds the parallel-opened scaffold into the closing wave: rule discovery + ranking + emission of inferred rewrites into per-grammar rewrite slots; competitor delta refresh (sonic-rs + simdjson + lightningcss). | Inferred rules visible in samply attribution on JSON / CSS bench profiles; cost-model selects rewrites; bench suite shows +5-15% on at least one tranche-targeted entry. | 5-10 days | AZ-II.cutover close + BA close |

**Total declared:** 5 waves to BB.close.
**Previous declared:** 17 waves (W2-act + W2.B + W3 + W4 + AZ-II.W0/W1/W2/W3 + BA.W-1/W0/W1/W2/W3 + BB.W0/W1/W2/W3).
**Reduction:** 70%.

The reduction is honest because the waves the trajectory invented were
substrate-without-consumer ceremonies. Per `AUDIT-1` §2(a), AZ-I
shipped substrate-only at W2 close in violation of SPEC §Activation-
gate; the refined path repairs that by landing the consumer in the
same wave that ships the substrate.

## 3. Pre-wave hygiene — landed before AZ-I.W2-act dispatches

These are dev-loop and decay cuts that compound across the remaining
five waves; they pay for themselves once and amortize across every
fan-out worktree.

1. **Shared sccache** (`AUDIT-4` cut #1). `RUSTC_WRAPPER=sccache` +
   `SCCACHE_DIR` exported in `Makefile` + `seed-worktree.sh`.
   Reclaim: 3-5 min cold per fan-out worktree × N agents per wave.
2. **Hardlink-clone `target.local`** (`AUDIT-4` cut #2). `cp -al
   <main>/target/ax-iter <worktree>/target.local/ax-iter`
   in `seed-worktree.sh` when `--no-target` requested. Reclaim:
   3-5 min cold + 15-20 GB disk per fan-out worktree.
3. **`shapes/mod.rs` factoring** (`AUDIT-4` cut #3). Factor the
   per-shape match-arm dispatch into a per-tag table populated at
   each shape's mod registration; eliminates the W2 stage-1
   sequential dependency for future per-shape redress waves.
   Reclaim: 45-60 min per multi-shape wave by enabling true
   N-parallelism from wave start.
4. **xtask alias to `--profile ax-iter`** (`AUDIT-4` cut #4).
   Reclaim: 30-60 s per emitter change.
5. **`AGENT_DISPATCH_TEMPLATE.md` retire per-tranche
   AGENT_DISPATCH.md** (`AUDIT-1` cut #5). The orchestrator
   authors the dispatch prompt at dispatch time; per-tranche
   pre-authored AGENT_DISPATCH.md adds 150-300 LOC × tranche
   without saving any work. Retire under W2-act prelude.
6. **WAVE_SPEC.md retirement** (`AUDIT-1` cut #1). Plan-table-
   driven dispatch from `{LETTER}.md` is sufficient; the per-wave
   spec doc duplicates the parent. Retire under W2-act prelude;
   amend `tranche/SPEC.md` to remove the conditional requirement.
7. **README §Cache clearing ritual deletion** (`AUDIT-4` cut #5).
   Post-B2 the proc-macro retired; `.bbnf-cache` is orphan; the
   ritual confuses first-time agents.

The hygiene pass is a single ~30-min orchestrator-direct commit
ahead of W2-act dispatch. It does not require sub-agents.

## 4. Decay deletions — landed during AZ-I.W2-act and AZ-II.cutover

These are dead/legacy/shim items whose retention is itself drag.
Each lands inside the wave that owns its consumer surface.

| Item | LOC | Wave | Mechanism |
|---|---:|---|---|
| `crates/json-prototype/` retire (single-consumer crate) | ~2200 | W2-act prelude | demote to `crates/core/benches/json-prototype/` adjunct, drop workspace member |
| `crates/ir/src/passes/recognizers/dta.rs` amputation | ~900 | W2-act | the `lift_dta` pattern-set surface stays at <120 LOC; the rest is sunset |
| `crates/tape/src/visitor.rs` family deletion (7 traits) | ~746 | AZ-II.cutover | per-grammar concrete builders inline (W2-act establishes the pattern) |
| `tape::dta` hoist to `bbnf-ir::dta` | ~80 | AZ-II.cutover | tape consumes bbnf-ir, not the reverse |
| Tape driver dead helpers (`emit_leaf` / `emit_reducer_compound` / `lookup_precedence`) | ~150 | AZ-II.cutover | zero non-doc consumers verified by grep; delete-now |
| W2.RE `panic!` quartet at `hregex.rs:285,446,579,718` | trivial | W2-act | retires by Sheets/CSS L4 hitting HRegex struct-direct — write the body, panic dies |
| `crates/core/src/backend/rust/emitter/grammar.rs` god-module split | ~600 | W2-act prelude | directory module per `feedback_directory-modules` |
| `crates/core/src/backend/rust/view/value.rs` per-Value-shape split | ~600 | W2-act | becomes obsolete on activation if the typed view replaces the generic |
| `crates/ir/src/passes/csp_strategy/mod.rs` god-module split | ~1300 | BA prelude | directory module |
| `crates/core/tests/common/css_normalize.rs` split | ~1500 | W2-act prelude | god-module per `feedback_no-god-modules` |

Total reclaim: ~8,100 LOC. Each deletion lands in the wave whose
consumer surface stabilises around it; no orphan deletion waves.

## 5. The substrate ownership shift — EmitStrategy hoist

`AUDIT-6` §8.1 names this as a P0 architectural transposition:
`EmitStrategy` lives in `crates/core/src/backend/rust/emitter/strategy.rs`
today, coupling the substrate-selection decision to the Rust
backend. The TS and WASM backends will need the same decision
when they land at BA-host-bindings; pre-emptive hoist to
`bbnf-ir::registry::strategy` makes the selection a per-backend
`SubstrateBinding` record instead of a per-backend
re-implementation.

This is wave-aligned: lands inside W2-act (as the resolver flip
dispatches against the new location), no separate refactor wave.

## 6. The substrate-with-consumer invariant — restored

The W2-act wave's hard gate is the SPEC §Activation-gate invariant
made operational: every substrate item this session added either
(a) fires in production code at wave close, or (b) is deleted.
The `audit_payload_coverage` pass + `registry_observer` submodule
+ orphaned `*_struct_direct` emit fns are the test cases —
the wave delivers them or deletes them, not both.

`AUDIT-2` §6 enumerates the wire-or-delete decisions; the wave
imports them as exit-criteria.

## 7. Workspace expedition during W2-act

The single workspace nextest wall is 8.5 s. The dev-loop is
healthy. The remaining drag is fan-out cost (`AUDIT-4` §5),
addressed by §3 hygiene cuts #1 + #2. No new tooling is necessary;
the existing `cargo iter-check` / `iter-test-{leaf,grammar,ws}` /
`make ay-bench-close WAVE=close` surface (`AUDIT-4` §4)
covers every iteration the wave needs.

## 8. Sub-agent dispatch shape for W2-act

`W2-act` is the largest single wave proposed. Its scope (resolver
flip + 3 grammars × runtime view-API + 5 parity harnesses + bench
gate verification + dead-substrate sweep + EmitStrategy hoist)
fits the SPEC's max-6-parallel constraint. Proposed shape:

- **W2-act.A (sequential, 60 min)** — EmitStrategy hoist to
  `bbnf-ir::registry::strategy` + view-layer typed-accessor API
  on `JsonDocument` (`view()` / `to_value()` / `path!`-prep
  signatures matching today's `Parsed<Grammar>` API surface).
  Unblocks the 3 broken JSON tests + parity harness recoding.
- **W2-act.B1 + B2 + B3 (parallel after A)** — JSON / Sheets /
  CSS L4 activation. Each: resolver-arm flip extension +
  per-grammar runtime types + parity harness recoding + bench
  gate verification on the grammar's slice. Disjoint file bounds
  (per-grammar runtime + per-grammar parity test). Each agent
  cap 60 min.
- **W2-act.C (sequential after B*)** — close ceremony absorbing
  W4: 17-entry matrix re-run, samply capture under
  `docs/benchmarks/profiles/AZ-I/W2-act/`, AZ-II handoff contract
  verification, FINAL.md authoring, dead-substrate sweep
  (delete `audit_payload_coverage` if unwired + `registry_observer`),
  W2.RE `panic!` quartet retirement. 60 min.

Total wall: ~3 sequential 60-min agent waves with one 3-parallel
phase = ~3 hours real wall under fan-out. The wave's reversal
posture: any miss > 20% on the bench gate reverts the activation
arm + re-plans through W2-act.A research; the substrate stays.

## 9. AZ-II.cutover dispatch shape

The current AZ-II 3-wave plan (W0 design + W1 Stage A + W2 Stage B
+ W3 deletion) collapses to one wave because:

- The W2-act activation establishes the per-shape struct-direct
  pattern; AZ-II reuses unchanged.
- The Stage A / Stage B byte-equal cycle is two regen + diff
  invocations — minutes of wall, not days.
- The tape deletion is mechanical once `crates/tape/` has zero
  remaining consumers (W2-act activation severs the data
  grammars; AZ-II.cutover severs BBNF in the same wave).

Proposed shape:

- **AZ-II.cutover.A (sequential, 90 min)** — `tape::dta` hoist to
  `bbnf-ir::dta`; `tape::visitor` family deletion; tape driver
  dead-helper deletion; BBNF-grammar typed-leaf authoring closes
  `StructRegistry` on BBNF; resolver-arm extension for
  BbnfGrammar.
- **AZ-II.cutover.B (sequential, 60 min)** — Stage A regen
  produces struct-writing BBNF parser; Stage B re-regen byte-
  equality.
- **AZ-II.cutover.C (sequential, 90 min)** — `crates/tape/`
  directory deletion; cross-crate dep severance; alias retirement;
  cutover FINAL.md authoring; AZ-II close.

Total wall: ~4 hours real wall.

## 10. Reversal posture

Each refined wave declares its reversal at dispatch:

- **W2-act**: bench-gate miss > 20% on any of the 17 entries
  reverts the activation arm (resolver returns to all-TapeDirect)
  + re-plans through W2-act.A. The substrate stays. The
  per-grammar test migrations stay. The hygiene cuts stay.
- **AZ-II.cutover**: byte-equal miss reverts the tape deletion +
  re-plans through cutover.A. The dta hoist + visitor deletion
  stay (independent retirements).
- **BA**: zero-allocation miss on one grammar reverts the host-
  binding sub-agent for that grammar; the typed `Path<G, T>`
  substrate stays.
- **BB**: rule inference no-op reverts the inference loop; the
  rule storage substrate stays for manual rule authoring.

In every case, the substrate landed earlier in the path stays. The
reversal is wave-local, not tranche-local.

## 11. Hand-off

This synthesis is the dispatch input for the next agent wave.
Specifically:

- W2-act.A dispatch reads §3 (hygiene cuts) + §5 (EmitStrategy
  hoist) + §8.A (view-layer accessor API).
- W2-act.B1/B2/B3 each read §8.B + their grammar's section in
  AUDIT-2 §3 (consumer audit).
- W2-act.C reads §4 (decay deletions) + §6 (substrate-with-
  consumer invariant) + AUDIT-1 §4 cut 4 (FINAL.md cap at 350
  LOC).
- AZ-II.cutover dispatches read §9 + AUDIT-3 items 6 + AUDIT-6
  §8.2-8.3.

The audits at `AUDIT-{1..6}-*.md` are the supporting corpus; the
synthesis doc is the binding plan. Cite the audit doc + section
when redispatching.

## 12. What this audit synthesis explicitly does NOT propose

To avoid scope creep:

- No new IR passes.
- No new substrate beyond the EmitStrategy hoist.
- No competitor-parity gate beyond the 17-entry matrix.
- No path-typed `Path<G, T>` infrastructure ahead of BA.
- No e-graph rule schema beyond what BB.W0 declares today.
- No reorganisation of `crates/ir/src/passes/` directory beyond
  the dta amputation.

Per `feedback_no-deferrals` and `feedback_no-orthogonal-codepaths`:
the path forward is the smallest set of architectural transpositions
that closes AZ-I + AZ-II + BA + BB on the declared gates. Every
proposed wave folds existing substrate into a working consumer; no
wave introduces orthogonal substrate the consumer-side has not
been authored against.
