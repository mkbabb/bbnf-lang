# 24 — The Decision Engine: Census, Theatre, and the Totality Design

## Verdict

A decision engine exists in skinny as a three-stage pipeline (candidate-gen →
e-graph extract → CSP finalize) over the five-shape `BackendShape` canon, but it
is **load-bearing only in its shape filter, not in its cost model**. The
performance axis that the binding D11 law rests on — `perf_cost` — is the literal
integer `0` at every one of its eight assignment sites. Selection therefore
reduces to a lexicographic tie-break over two hardcoded priority constants
(`static_size_cost`, `shape_rank`). The e-graph runs one cosmetic rewrite; the
CSP re-derives an answer it was handed. The SIMD kernels — the actual >SOTA
hot leaves — are selected by `cfg!(target_arch)` and a static admissibility
predicate, **entirely disjoint** from the decision engine. The cost model that
D11 demands does not exist; it is asserted in prose and contradicted in code.

## What ACTUALLY exists (skinny census)

**Shape canon.** `BackendShape{EagerTape,OffsetTape,EventTape,SinkOnly,
CollapsedStage}` is a real 5-variant enum (`skinny/crates/ir/src/lib.rs:361-367`)
with a real per-shape lowerer dispatch (`skinny/crates/codegen/src/lower/mod.rs:19-27`).
This part is genuine and load-bearing: a different shape produces different code.

**Candidate generation** (`skinny/crates/passes/src/lib.rs:508-597`). The real
decision is here, as a cascade of structural predicates, NOT a cost comparison:
- `requires_eager_tape` (`:762-767`) — recovery annotation ∨ host-decode ∨
  layout policy ∨ first-set dispatch overlap. If true, all non-eager candidates
  are `hard_pruned`.
- `admits_sink_only` (`:954-957`) — contains a `DirectBuild` ∧ a target consumer flag.
- `admits_collapsed_stage` (`:959-961`) — **`target.avx512bw && Entry(_)`**. On
  aarch64 (`avx512bw` is hardcoded `false`, `:380`), CollapsedStage is *never*
  admissible. This is the only gate for a shape the campaign treats as a first-class
  member of the canon, and it is unreachable on the only supported architecture.
- `prefers_event_tape` (`:963-965`) — `alt_branch_count ≥ 8`, a magic constant.
- `offset_tape` — the default, ranked whenever a backend rule exists and not eager.

**E-graph "extraction"** (`skinny/crates/passes/src/backend_egraph.rs:36-149`).
Each non-pruned candidate becomes a childless `DecisionNode`; they are all
`union`ed into one e-class (`:62-70`); one rewrite `NormalizeDirectSinkCost`
runs (`:191-237`); `Extractor` picks the minimum-cost node. The cost tuple
`DecisionCost` (`:239-247`) is `(freshness_rank, perf_cost, capacity_cost,
static_size_cost, shape_rank, candidate_hash)`, ordered lexicographically by
derived `Ord`. Because:
- `freshness_rank = u32::from(candidate.stale)` and nothing is ever marked stale
  (`stale: false` at the sole producer, `lib.rs:589`) → always 0;
- `perf_cost = 0` always (see below);
- `capacity_cost` is 0/1 and only 1 for a string-cap OffsetTape;

the selection collapses to **`static_size_cost` then `shape_rank`** — both
hardcoded constants (`lib.rs:603-611`, `:545-574`). SinkOnly (size cost 1) beats
OffsetTape (2) whenever admissible; otherwise OffsetTape wins. The e-graph,
scheduler, and extractor are an elaborate `min_by_key` over a constant.

**The one rewrite is cosmetic.** `NormalizeDirectSinkCost` matches a SinkOnly
node and zeroes its `static_size_cost` and `shape_rank` (`:216-220`). Since
SinkOnly already has the lowest static_size_cost (1) among admitted shapes, the
rewrite changes the winner in no case the cascade did not already decide. It
exists to satisfy the LOCKS "≥1 asserted rewrite" regression-guard
(`restart/locks/LOCKS.md:647`), not to alter an outcome. The guard's own framing
admits this: a zero-rule e-graph "re-opens the V2 scaffold finding" — i.e. the
finding that this is scaffolding was made, then papered over with one inert rule.

**The CSP is a tautology** (`skinny/crates/passes/src/decision_csp.rs:29-112`).
`solve_rule` locates the index the e-graph already chose, then builds a domain in
which that index costs `0.0` and every other costs `1.0` (`:43-47`), adds five
predicates that are true of all candidates by construction (every shape `matches!`
the full shape set, `:66-74`; every id is non-empty; capacity ≤ 1), and
"solves" to recover the index it was handed. `selected_shape: active.shape`
(`:153`) is passed straight through. The CSP cannot change a decision; its own
test asserts exactly that (`:264`, "preserves the active shape"). It produces a
40-field `DecisionCspFacts` record of String status constants ("pass", "pass",
"evidence-only"…) — telemetry, not computation.

## The D11 law is unmet

D11: ">SOTA survives the generator un-fork ONLY via cost-model-gated named
hot-leaf primitives; a naive grammar-walk regresses to lightningcss-class speed."
The decision engine is supposed to be the gate. It is not, for two independent
reasons:

1. **There is no cost.** `perf_cost` — the only axis that could encode "this leaf
   is fast / this walk is slow" — is `0` at all eight sites:
   `backend_egraph.rs:170`, `decision_csp.rs:242`/`:254`, `lib.rs:590`/`:2023`/
   `:2035`/`:2062`/`:2083`. No microbench table, no instruction estimate, no
   corpus-profiled weight ever reaches it. `BackendCandidate` carries the field
   and the e-graph reads it, but it is dead. The "cost model" is a sort over
   author-assigned size ranks.

2. **The hot-leaf primitives are not engine-selected.** The SIMD kernels that
   actually deliver >SOTA — the named hot leaves D11 names — are dispatched by
   `cfg!(target_arch="aarch64")` plus a static predicate, with **no cost model
   in the path** (`skinny/crates/bbnf-simd/src/dispatch.rs`):
   - `select_backend` (`:89-98`): if `lo6_table_admissible(alphabet)` → NeonTbl4,
     else Scalar. A boolean structural test, not a cost comparison.
   - `select_primitive_kernels` (`:63-87`): a `#[cfg(target_arch="aarch64")]`
     block returns the NEON function pointers; otherwise the scalar set. Pure
     compile-time arch dispatch.

   The kernels are NOT "cost-model-selectable alternatives behind one contract."
   They are two hardcoded branches. The `DecisionCost` lattice never sees a
   kernel. The decision engine (operating on `BackendShape`) and kernel dispatch
   (operating on alphabets and arch flags) are two unconnected subsystems that
   share no cost currency.

So the D11 mechanism is, today, **structural admissibility + arch cfg**, dressed
as cost-driven equality saturation. It happens to produce fast code because the
hand-written kernels are fast — not because any model chose them over a naive walk.

## Core (totality) state — and the parallel-engine hazard

`crates/` has **zero `BackendShape`** (grep returns nothing in `crates/ir`,
`crates/core`). But it is not decision-free: it carries a SEPARATE, larger
decision subsystem the skinny census omits —
`crates/ir/src/passes/csp_strategy/mod.rs` (1361 lines), which solves
grammar-component strategies with real `cfg.egraph.weights` (`:969`, `:1040`,
`:1119`) and extracts regex-engine decisions (`extract_regex_engine_decisions`,
`:1306`), plus `csp_domains.rs` (500 lines) and a kernel-shape selector
(`recognizers/kernel_shape.rs`).

This is the central totality risk the assignment must name: **porting skinny's
`BackendShape` engine into a tree that already has its own egraph-weighted CSP
strategy solver yields TWO decision engines** unless they are unified into one.
That violates "ONE codegen path / no orthogonal subsystems." The net-new work is
not "add a decision engine" — it is "reconcile two."

**A second, identical dishonesty already lives in core.** `kernel_shape.rs:38-79`
documents "thresholds are pluggable via the cost model" but implements
`for_singleton_count` with two hardcoded `const` boundaries (`NIBBLE_LUT_MAX = 8`,
`WIDE_LUT_MAX = 16`, `:63-79`) and a comment conceding they "today match the
architectural-lever boundaries." Same pattern as skinny: a cost model asserted in
the doc, hardcoded constants in the body. The core tree has the same gap, in a
different file.

## DESIGN — the decision engine for totality

### 1. What is costed: a measured kernel table, not estimates

The cost model must carry a non-zero `perf_cost` grounded in **measured
per-kernel-per-uarch microbench cells**, dav1d/`checkasm`-style, NOT instruction
estimates (estimates cannot rank a NEON `vqtbl1q` lane against a scalar fallback
without modelling port pressure — and the campaign already runs checkasm). The
artifact is a `KernelCostTable`: `(KernelId, Uarch) → cycles_per_64B`, populated
once by the existing checkasm/PMU harness on the M5 Max (`SRC-A64-PMU`), checked
in as data, and versioned. `perf_cost` for a shape becomes the sum over the leaf
kernels its lowerer emits, read from that table. The naive grammar-walk has no
kernel rows → its `perf_cost` is the scalar-fallback cell, which is strictly
worse than the NEON cell → the model *mechanically* prefers the hot leaf. That,
and only that, makes D11 a law the code enforces rather than prose.

Corpus-profiled weights belong on the *frequency* axis, not the per-kernel cost:
the existing `crates/ir/src/passes/profile.rs` mines call/branch frequency; the
total cost is `Σ frequency(leaf) × table_cost(kernel(leaf), uarch)`. This is the
iburg dynamic-programming form (`2D-cost-model.md:108`) the audit already cites
as grounding — but with real cells, not zeros.

### 2. How the e-graph consumes the cost

Keep the existing `Extractor`/`Lattice`/`CostModel` from `crates/egraph` — it is
sound. The fix is upstream of it: `DecisionCost.perf_cost` must be the
table-derived value above, and the cost tuple's lead axis must be `perf_cost`,
not `freshness_rank`. The e-graph then does real work: when two shapes are both
admissible (e.g. SinkOnly vs OffsetTape on a string-heavy rule), the cheaper
*measured* leaf wins, and the rewrite set can express genuine equivalences
(e.g. "an OffsetTape whose only consumer is a sink ≡ a SinkOnly") whose payoff is
a lower extracted cost — not a cosmetic field-zeroing.

### 3. How shape AND kernel are selected by ONE model

Collapse the two subsystems. The decision is a **single extraction over a product
language**: a rule's e-class holds nodes keyed by `(BackendShape, KernelStrategy)`
where `KernelStrategy` is the core `kernel_shape` selection promoted from a
hardcoded `for_singleton_count` into cost-table lookups. The hand-asm kernels
become *alternatives in the same e-class behind one `Kernel` contract* (a
function-pointer table is already the runtime shape, `dispatch.rs:50-56`); the
extractor picks the kernel whose table cell is cheapest among those admissible on
the host uarch. `cfg!(target_arch)` survives only as an *admissibility filter*
(a NEON node is hard-pruned off aarch64), never as the selector. One model, one
extraction, both axes — eliminating the parallel `csp_strategy` engine.

### 4. Where decisions are recorded: the Lock-16 manifest

The reproducibility artifact must be a real decision manifest, not the current
`DecisionCspFacts`/`BenchFacts` String-bags (`cost.rs:202-248`,
`metadata.rs:38-65`), which record "pass"/"evidence-only" status strings with no
recoverable decision. The Lock-16 manifest (LOCKS `:607`) should be a generated,
checked-in, per-grammar table with ONE row per selected leaf:
`(rule_id, shape, kernel_id, uarch, table_cost_cell, table_version,
scalar_oracle, checkasm_cmd, rejected_alternatives_with_their_costs)`. It is
emitted by the extractor (not hand-maintained), diffable across regens, and
falsifiable: re-running selection must reproduce it byte-for-byte, and every
`kernel_id` must trace to a checkasm-gated cell. This subsumes the present
`ActiveCostFacts`/`DecisionCspFacts` — delete those 40-field String structs.

### 5. Pluggability (no hardcoded branches)

Per the binding precepts, every threshold currently a `const` must move into the
cost table / a pluggable cost model: the `8`/`16` kernel boundaries
(`kernel_shape.rs:63-68`), the `alt_branch_count ≥ 8` EventTape trigger
(`lib.rs:964`), the `tiny_string_cap = 16` (`lib.rs:680`), and the `static_size`/
`shape_rank` ladders. None of these survive as `match`-arm magic numbers; each is
a data-driven weight the manifest records. The `admits_collapsed_stage` →
`avx512bw` gate is dead on aarch64 and should be struck, not ported (LOCKS `:645`
already concedes CollapsedStage is a "shape slot only, diagnostic-only body").

## What skinny PROVED vs what is NET-NEW

**Proved:** the 5-shape canon as a *lowering* dispatch is real and works; the
e-graph/extractor/CSP-solver libraries integrate and run; structural
admissibility predicates (eager-forcing on first-set overlap, sink-only on
direct-build) are sound shape *filters*.

**Net-new (does not exist anywhere):** a non-zero `perf_cost` sourced from a
measured kernel table; the cost-table itself; kernel implementations as
cost-selectable alternatives in one e-class; the unification of skinny's
`BackendShape` engine with core's `csp_strategy` engine into ONE extraction; the
Lock-16 manifest as a generated, reproducible, checkasm-traceable decision record
(today it is a String-status telemetry bag); and honest pluggability of every
currently-hardcoded threshold.

**Must be retired, not ported:** the tautological CSP (`decision_csp.rs`), the
cosmetic `NormalizeDirectSinkCost`-as-regression-guard, the `avx512bw`
CollapsedStage gate, and both 40-field String fact structs.
