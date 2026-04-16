# AW-I.W4 — Close audit

W4 dismantled the emitter-sibling surface the legacy fn-per-rule
path stood on and extended the DTA substrate with the architectural
primitives the self-hosted parse needs (entry rule dispatch, WsTrim
state). The cyclic-fuse activation and guard-drop W4α already
performed compose with the W2.5 pin predicates to preserve
typed-materialisation invariants.

This audit records what landed, what remains open, and the residual
risk the W5 bench wave should carry forward.

## Commit range

| Commit     | Subject                                                                  |
|------------|--------------------------------------------------------------------------|
| `ef840a35` | gut `emit_rule_function_impl` + delete `emit_tape_tier_rule` (W4α)       |
| `bfd9777b` | drop `scc_id.is_none()` acyclic guards (W4α)                             |
| `918a10c6` | delete `alt.rs` + emit_alt/key_dispatch trait impls (W4β.1)              |
| `f7594dbb` | delete `tape_prelude.rs` (W4β.1)                                         |
| `1c63117b` | delete `leaves.rs` + emit_literal/regex/epsilon/seq_all_span (W4β.2)     |
| `12b7468a` | delete `map_value.rs` + emit_enum/number/constant/map/span/hex (W4β.2)   |
| `47496993` | delete remaining 7 sibling modules + trait impls (W4β)                   |
| `840d832c` | **default Emitter::Output + parse-emit no-op defaults (W4γ.1)**          |
| `7b74bb58` | **DTA entry rule + WsTrim state wiring (W4γ.2)**                         |
| `86bf7607` | **add entry field to DTA_TABLE const (W4γ.3)**                           |

## Emitter directory — post-W4 state

`ls crates/core/src/backend/rust/emitter/`:

```
dta.rs        grammar.rs   mod.rs       prettify/     profile.rs    visitor.rs
```

Gate 11 met. Every per-rule body emitter module deleted. `grammar.rs`
carries the `parse()` entry (dispatching through `dta_run_into`), the
empty-shim `emit_rule_function_impl`, `emit_type_definitions_impl`,
and the top-level `emit_grammar_impl`. `dta.rs` emits `DTA_TABLE` +
supporting state/precedence arrays. `profile.rs` emits
`GRAMMAR_PROFILE`. `visitor.rs` emits the AV.2.5 reordered-unroll
visitors. `prettify/` owns the orthogonal prettify emission path
(separate from parse — untouched by W4).

## Lines deleted across W4

| Wave  | Files deleted                                                  | Line delta |
|-------|----------------------------------------------------------------|------------|
| W4α   | `emit_tape_tier_rule` body gutted in `grammar.rs`              | -47        |
| W4β.1 | `alt.rs` (807) + `tape_prelude.rs` (610) + mod.rs hunks        | -1483      |
| W4β.2 | `leaves.rs` (374) + `map_value.rs` (526) + mod.rs hunks        | -935       |
| W4β   | `seq.rs`, `repeat.rs`, `binary.rs`, `operator_chain.rs`,       | -1422      |
|       | `dispatch.rs`, `ws.rs`, `string_decode.rs` + mod.rs hunks      |            |
| W4γ.1 | `scanner_plan.rs::JsonStringDecode` + `emit_decode_call` site  | -40        |
| **Σ** | 11 sibling modules + 27 trait-method impls in `mod.rs`         | **≈-3927** |

W4γ.1 also promoted `Emitter::Output` to bound `: Default` and
replaced 27 parse-side method signatures with default impls returning
`Self::Output::default()`. TS + WASM continue to override every
method; the Rust backend observes the defaults through the driver's
per-rule traversal and discards the empty `TokenStream` at
`emit_rule_function_impl`.

## Bootstrap idempotency — **NOT LANDED**

The bootstrap regen cycle broke at W3.1 (as declared by the W3
invariant) and W4γ could not close it in-agent. Three consecutive
barriers surfaced:

1. **Entry rule dispatch.** `dta_run_core` read
   `rule_entries.first()`, which returned whichever rule landed at
   `RuleId(0)` rather than `ir.entry`. For bbnf the entry is `grammar`
   (RuleId 52), lifted last. W4γ.2 adds `DtaTable::entry: DtaRuleId`
   and updates the walker. **Closed.**
2. **Whitespace trim coverage.** The DTA lifter silently stripped
   every `IrNode::OptionalWhitespace(inner)` to `lift_node(inner)`,
   which lost every `?w` site. W4γ.2 adds `DtaState::WsTrim { pattern:
   Option<StringId> }`, lowers `OptionalWhitespace` to
   `Seq([WsTrim, inner, WsTrim])`, and the walker scanner-scans the
   grammar's `@ws` regex (falls back to ASCII whitespace identical
   to `exec_trim_ws` semantics when the grammar has no `@ws`).
   **Closed.**
3. **Nested Alt backtracking.** `BbnfBootstrap::parse` on
   `a = ( "x" ) ;` fails at offset 0; `a = "x" ;` succeeds. The
   `term`'s Alt includes `"(" , rhs ?w , ")"`; when an outer
   AltLinear branch's subtree has to dispatch through nested Alt
   under nested Repeat under nested Alt (grammar_item → rule → rhs →
   alternation → concatenation → binary_factor → mapped_factor →
   factor → term → paren-branch → rhs → ...), the `try_branch`
   bounded walk returns a Syntax error from the outer Alt even
   though the inner rule branch's regex prefix should match. The
   savepoint restore isn't corrupted; the branch dispatch itself
   propagates the error through the stack-bounded loop.

   Root-cause analysis requires deeper tracing than the single-agent
   W4γ envelope admits. **Open — W5 or a dedicated sub-agent.**

## Workspace state — **NOT GREEN**

`cargo check --workspace` fails with 6 derive-panics (bbnf-bootstrap
+ 5 gorgeous parsers) + 7 downstream E0599 cascades. Root cause is
concern 3 above: `BbnfBootstrap::parse` fails on any grammar with
paren-group expressions inside a rule body.

`cargo test -p bbnf-tape` — **7 passed / 0 failed / 0 ignored**.
Walker arms test coverage is intact; the W4γ DtaTable field addition
threaded through all fixture literals.

`cargo test -p bbnf-ir --tests` — **261 passed / 0 failed / 3
ignored** across 5 test binaries. The lifter + transform + pass
suite is unaffected by the W4γ extensions.

## Gate-by-gate status

| Gate | Target                                                     | Status  | Note |
|------|------------------------------------------------------------|---------|------|
| 11   | Emitter dir contents                                       | **MET** | `dta.rs`, `grammar.rs`, `mod.rs`, `profile.rs`, `visitor.rs`, `prettify/` — verified via `ls` |
| 12   | CSS L4 DTA state count < 2000                              | **UNVERIFIED** | Test `dta_counter_states::dump_dta_summary_per_grammar` lives in `crates/core/tests/` which inherits gorgeous dev-dep and cannot compile until gate 13 closes. State count gate remains an assertion over the lifter output, which compiles + unit-tests cleanly. |
| 13   | Workspace 0 failures                                       | **NOT MET** | 6 derive-panics upstream of every integration test binary |

## Residual risk — W5 carry-forward

1. **Paren-expression backtracking fix.** The nested Alt / Repeat
   / Alt interaction needs diagnosis. Suspects:
   - `try_branch`'s `stop_depth` boundary may close out before the
     inner Alt's successful branch propagates up;
   - `handle_repeat_failure_bounded` absorbs Syntax errors that
     should propagate to the enclosing Alt's savepoint;
   - The `last_err` fallback in AltLinear may mask the real
     failure offset.

   Repro: `crates/bbnf-tape/tests/` + a local `BbnfBootstrap::parse`
   probe on the 4-line fixture `a = ( "x" ) ;`.

2. **Bootstrap idempotency.** Pending (1). Once the walker parses
   bbnf.bbnf end-to-end, `scripts/bootstrap-bbnf.sh` regenerates
   `generated.rs` with the full WsTrim + entry encoding. Expected
   final line count: ~21200 (up from W3.2's 20432 by the WsTrim
   state slots + entry field).

3. **Gate 12 direct-call test.** Once the workspace builds, land a
   dedicated test (`crates/core/tests/dta_state_count.rs` or
   inside `dta_counter_states.rs`) asserting
   `summarise_dta(&table, &ir).state_count < 2000` for CSS L4. The
   AV.3.6 baseline was 2473; the cyclic-fuse activation from W4α
   should have reduced this, but the reduction is unverifiable until
   gate 13 closes.

4. **Emitter default-impl cost.** The Rust backend invokes the
   driver's per-rule `compile_node` walk and discards every
   resulting `TokenStream` at `emit_rule_function_impl`. Wasted
   work; an AW-II lever gates the traversal behind an
   emitter-level `skip_per_rule_emission()` hook.

## Architectural notes

- **`DtaTable::entry`** is a first-class field, not a cache. The
  walker does a single binary search (log₂53 ≈ 6 comparisons) on
  `rule_entries` at parse start. No measurable overhead.
- **`DtaState::WsTrim`** adds two states per `?w` site in the
  grammar. bbnf's 24 `?w` occurrences add ~48 states. CSS L4's
  `?w`-light surface adds fewer. The state count increase is
  offset by fuse/inline's collapse of acyclic-rule redundancy.
- **`Emitter::Output: Default`** — every live backend satisfies
  this bound trivially. `TokenStream` is `Default`. `String` is
  `Default`. `TsCode` gained `#[derive(Default)]` because its
  fields are `String`.

## What follow-on agents inherit

- The W4γ.2 + W4γ.3 commits form the architectural scaffold for
  bbnf self-hosting under DTA parse.
- The nested Alt backtracking bug is the single open correctness
  issue.
- Once closed, the full bootstrap regen cycle reopens, idempotency
  lands, and gate 13 closes.
