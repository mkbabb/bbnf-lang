# AY-II — Progress Log

Dated execution log for tranche AY-II (pass II of AY; see
`../AY-I/FINAL.md` for pass-I close and `audit/AUDIT-{A,B,C,D}-*.md`
for the triumvirate that informs this pass).

- `Status`: in_progress (W0 landed substrate + emitter + runtime; pending 4-agent audit for forward path)
- `Current wave`: W0 (pre-close pause)
- `Next wave`: deferred — awaiting audit triumvirate

---

## W0 dispatch — 2026-04-21

Five parallel sub-agents dispatched on disjoint file bounds per
`waves/W0.md` §File bounds. Every brief carries the agreed contract
signatures so each worktree writes against compile-incomplete
reciprocal dependencies; orchestrator composes at cherry-pick.

Dispatched:

- **W0.a** — `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0a`: tape
  substrate rollback + unified compound API (`Columns::rollback_to`,
  `TapeBuilder::{begin,end}_compound`; retire `open_compound`,
  `close_compound`, `note_push`, `SIB_SKIP_STAMPED_BIT`, `open_stack`,
  `OpenFrame`; finaliser-only stamping).
- **W0.b** — `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0b`: emitter
  unification across `shapes/*.rs` — every `push_compound` +
  `mark_children` + `columns_mut().truncate` migrates; every retry
  site pairs tape rollback + value rollback.
- **W0.c** — `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0c`: fused
  pipeline (`ValueBuilder<R>`, `ValueCheckpoint`); `Parsed::to_value`
  becomes a thin projector; `navigate_tape` retired from `runtime/path.rs`.
- **W0.d** — `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0d`: projection
  totality (`PROJECTION_DIRECT_TO_STRUCT.len() == materializer count ==
  consumer count`); `__named_type_shim_*` retired; typed-CSS skeleton
  in `ir/src/passes/payload/layout.rs`; `projection_totality.rs` test.
- **W0.e** — `/Users/mkbabb/Programming/bbnf-wt-ay-ii-w0e`: promoted
  structural-scan as cursor API (`object_key_seek`, `bounded_lookahead`,
  `scan_structural_bounded`); per-grammar `STRUCTURAL_SCAN_POLICY`
  const from CSP alphabet + digraph facts.

Contract signatures fixed at dispatch so all five worktrees write
against the same target API. Orchestrator-owned close ceremony (§W0.md
Orchestrator close steps): cherry-pick → regen → fat-LTO 5-bench
matrix → samply per grammar → bootstrap double-regen idempotency.

## W0 cherry-pick ledger — 2026-04-21

All 5 worktrees cherry-picked onto master. Commit ledger:

- `a13840a0` + `b2ac3cf5` — W0.a tape substrate rollback + tests.
- `61d0338c` + `487b17b7` — W0.e cursor primitives + `STRUCTURAL_SCAN_POLICY`.
- `4f42f6bb` — W0.c `ValueBuilder<R>` + fused `Parsed::to_value` + `navigate_tape` retire.
- `2ddb8c33` + `f2e458ec` + `2b24b0a4` + `1f97a8cc` — W0.b emitter shape migrations.
- `db979564` + `58271da1` — W0.d projection-totality emission + wire-contract test.
- `f372e7ef` — compose bridge: thread `root_off` into `ValueBuilder::finish`;
  hand-patched BbnfBootstrap `ValueRoot` stub (regen replaces at close).

### W0-fix composition repair

Bootstrap cycle-1 produced a 32938-line generated.rs that compiled but
whose NEW parse emitted tapes the walker couldn't navigate — `host.rs`
panicked on `find_rhs_expression_descendant` on the next build because
`begin_compound` was silently hard-coding `variant_idx = 0` for every
compound (collapsing rule_kind() to variant-0) and walker-parity
post-order triplets never stamped `HAS_CHILDREN_BIT`.

Fix landed as:

- `f8ac2cd7` — W0-fix tape: `begin_compound` re-admits
  `variant_idx` + `meta_idx` (now uses `TapeRec::pack_kind_meta`);
  adds `end_compound_post_order` that atomically stamps span_hi +
  child_off + HAS_CHILDREN_BIT.
- `c9142405` — W0-fix emitter: every shape callsite migrates to the
  6-arg `begin_compound` signature; walker-parity compounds collapse
  their open/close + `set_child_off_at` triplet into one
  `end_compound_post_order` call.

## Pre-close pause — 2026-04-21

W0's core transposition landed (10 original commits + 2 fix commits);
`cargo check --workspace` did not run to completion before the pause.
Outstanding quality concerns before opening W1:

1. **`f372e7ef` hand-patched `generated.rs`.** The compose-bridge
   commit carries a hand-edit (stub `project_value_output` with
   `unreachable!()`) as the substrate to compile the lib while W0.b
   migrated the emitter shapes. SPEC §"Generated files are output of
   fresh regen; never hand-patch" treats this as a violation — even
   as the one-shot escape recipe, a commit carrying hand-edits
   should not persist on master history. The correct regen-only
   state must replace it.
2. **Idempotency unverified.** Bootstrap cycle-1 completes but
   cycle-2 has not been demonstrated byte-identical to cycle-1 since
   W0-fix landed. Double-regen is a W0 close gate.
3. **`W0.b` landed ValueBuilder allocation at parse entry but did
   NOT thread `value_builder` through per-shape fn signatures.** The
   fused pipeline's lockstep construction (W0.c §2, AY-II.md §1)
   remains half-complete: compound emission still writes only to
   the tape; value construction has not been wired into shape
   emission. The `ValueBuilder::finish` call at parse-entry
   produces an empty slab — `Parsed::to_value` is currently
   unreachable without post-parse reconstruction.
4. **W0.e `STRUCTURAL_SCAN_POLICY` emission has no consumer.** The
   policy const is emitted per grammar but no `__path_walk` /
   `Parsed::get` emission consumes it. This is the substrate-
   without-consumer pattern AY-II invariant §7 rejects at wave
   close.
5. **W0.c + W0.d hit usage limits mid-run.** Their committed work is
   architecturally sound but the test coverage they authored
   (`projection_totality.rs`, parse-count invariant in
   `value_api_apples_to_apples.rs`) has not been run against the
   composed + regen'd substrate.

These concerns are not individual bugs to patch — they are a
coherence gap between W0's thesis and its landed state. Before W1
opens, the 4-agent audit triumvirate investigates: (a) W0's
architectural outcome against its plan-declared invariants; (b)
the fused-pipeline completion path W0.b deferred; (c) W1-W5 plan
validity against W0's actual landing; (d) predecessor + successor
alignment (AY-I close, BA/BB/BC openers).

No quick fixes. No workarounds. The audit frames the forward path;
execution resumes after the user's disposition.

## Audit triumvirate — 2026-04-21

Four parallel audit sub-agents delivered convergent findings:

- `baeed709` — AUDIT-A plan coherence. Headline: Invariant §5
  FAIL — `ValueBuilder` allocated but never threaded; slab
  empty; `project_value_*` panics on non-empty parse.
- `a809d12f` — AUDIT-B hitherto-expand. Confirmed AUDIT-A §5
  load-bearing FAIL at shape-file granularity (zero
  `value_builder.*` write calls from any shape's expand output);
  BbnfBootstrap totality holds 15:15:15; JSON / CSS / Sheets
  expand capture PARTIAL (incremental cache corruption from
  earlier cutoff); sibling alignment ZERO contests.
- `ad70effd` — AUDIT-C forward path. Prescribes Path B
  (FusedBuilder) — type-level collapse of TapeBuilder +
  ValueBuilder inside the tape crate; every shape's
  `builder: &mut TapeBuilder<R>` becomes
  `&mut FusedBuilder<R>`; atomic bi-column stamping; -485 / +280
  LOC net; zero signature churn across 25 shape fns. W1-W5
  specs hold unchanged; in-AY-II plan pivot (Absorb), not a new
  letter. f372e7ef history retained (rebase would break audit
  SHA citations).
- `319c432a` — AUDIT-D predecessor/successor alignment.
  Confirmed `Parsed::to_value()` panics. Confirmed CSS/Sheets
  `columns.rs:409` panic class extinguished at source (15/15
  retry sites use `rollback_to`; zero residuals). BA invariant
  §7 currently FAILS; closes at W0' landing. Cross-tranche debt
  ledger: 15 items, 11 to AY-II internally.

## W0' dispatch plan — 2026-04-21

User disposition: Path B, in-AY-II pivot, transient history,
NO un-wired legacy cruft, NO dead IIFEs, NO workarounds. Plan
audit of W1-W5 + BA/BB/BC surfaced 10 findings, now routed in
AY-II.md §Plan-audit findings.

`waves/W0p.md` authored with three-agent decomposition:

- **W0'.a** — FusedBuilder collapse + tape legacy deletion
  (`crates/tape/src/{builder,columns}.rs`,
  `crates/core/src/runtime/{value_builder,mod,parsed}.rs`,
  `crates/core/src/backend/rust/emitter/grammar.rs` parse entry).
  Retires `TapeBuilder::push_compound` + `mark_children` public
  API + entire `value_builder.rs` module + `parse_with_visitor`
  emission path + dead IIFE wrappers.
- **W0'.b** — Projection-consumer wiring + materializer slab
  migration (`crates/core/src/backend/rust/view/value.rs`,
  `emitter/shapes/value_materialize.rs`). Routes per-admission
  arms through `materialize_projection_<rule>_<Grammar>` fns
  (69 currently zero-call-site). Retires `<Grammar>Value::Unknown`
  fallback where totality holds.
- **W0'.c** — Scan-policy splice + emitter cleanup
  (`emitter/shapes/dispatcher.rs`, `view/value.rs::emit_path_walk`).
  Inlines `object_key_seek` / `bounded_lookahead` /
  `scan_structural_bounded` at codegen per
  `STRUCTURAL_SCAN_POLICY` flag. Retires W0-era `#[allow(dead_code)]`
  additions (6 known).

Each sub-agent's scope embeds explicit dead-code / dead-IIFE /
un-wired-substrate hunts within its file bounds. No deferrals:
every surface whose role ends in W0 retires at W0' in the same
commit as its supersession.

## W0' execution — 2026-04-21 → 2026-04-22

### Cherry-pick sequence (master `60f92743`)

All 12 W0' sub-agent commits + 1 d1 test migration landed:

- W0'.a 7 commits (`bd563c1d`…`1bfcf359`) — FusedBuilder collapse, `finish_fused` rename, ValueBuilder retire, parse_with_visitor retire, 4-arg `new_fused` shim, counter ungate, doc scrub.
- W0'.c 3 commits (`30aa83aa`, `0993cc89`, `bc8fa8b2`) — scan-policy splice, 8-site `#[allow(dead_code)]` retire, raw-name arm routing.
- W0'.b 2 commits (`550dac11`, `b1bb4579`) — projection-consumer wiring through `materialize_projection_*`, raw-name materializer lookup.
  - `view/value.rs` 4-region merge conflict resolved by splicing W0'.b's `emit_value_surface` top half + W0'.c's `emit_path_query_impls` bottom half.
- W0'.d1 1 commit (`60f92743`) — out-of-bounds test migration from `push_compound`/`mark_children` to FusedBuilder API: `json-prototype/src/visitor.rs`, `tape/tests/{tape_basic,close_compound}.rs`, `core/tests/tape_walker_allocs.rs`.

### Regen stall + diagnostic triumvirate — 2026-04-22

`scripts/bootstrap-bbnf.sh` ran 12–15 min wall-clock without completion
across two attempts at master `60f92743`. Per SPEC §Diagnostic-loop
relinquish, halted and dispatched research + plan + redress:

- `5cb76753` — research attribution at `audit/W0p-regen-root-cause.md`.
  Diagnosis: `value_end_compound` called recursive `subtree_size` on
  every compound close, turning each close into Θ(N) and the whole
  parse into Θ(N²). Introduced at W0'.a `bd563c1d`.
- `9a718199` — plan at `audit/W0p-regen-fix-plan.md`. O(1) in-stack
  `direct_child_count` on `ValueCheckpoint`, incremented by every
  direct-child push + decremented on rollback. Preserves every W0p.md
  §14–19 invariant; `subtree_size` retained for the projection-time
  `ValueChildren::next` iterator.
- `f768f50d` — redress executed. Tape tests 100/100 green post-fix
  (`cargo test -p tape --tests`), 55 tests in `tape_basic` alone pass in
  <1s with artefact parity (`frame.child_count`, `HAS_CHILDREN_BIT`,
  `child_off`, `span_hi` byte-identical).

### Unresolved: broader dev-loop infrastructure stall — 2026-04-22

Regen still stalls at 15+ min post-`f768f50d`. Observable dev-loop
symptoms across the day:
- `cargo check --profile ax-iter -p bbnf --lib` — 2s (fine).
- `cargo check --profile ax-iter -p bbnf --tests` — blocked 8+ min on
  gorgeous transitive build (gorgeous has 5× `#[derive(Parser)]` sites
  across ebnf/bbnf/json/google_sheets/jit).
- `cargo expand -p bbnf-bootstrap --lib` — 15+ min, same profile as the
  W0'.d3 stall (pre and post fix).
- `cargo check --profile ax-iter --workspace` — 10+ min.

The d3 fix was necessary (value-side O(N²) was real) but not sufficient.
A second hot path of equal or greater cost survives. Per user directive
"These processes are taking far too long. Totally unacceptable," the
build-infrastructure triumvirate opens at this boundary (per
`docs/instructions/memory/feedback_build_infra_first.md` — build/test
infra lands FIRST in any tranche where dev iteration time is a
bottleneck; never deferred). Hard time caps applied:
- Research: 20 min wall-clock cap.
- Plan: 15 min wall-clock cap.
- Redress: 30 min wall-clock cap.

Target gates (per user expectations):
- `cargo check -p <crate> --lib` ≤ 10s cold.
- Single-test-binary rebuild ≤ 60s cold.
- `scripts/bootstrap-bbnf.sh` ≤ 3 min cold.
- No compile invokes a full `#[derive(Parser)]` re-expansion when the
  grammar file has not changed.

W0'.d3 commit (`f768f50d`) stays landed — its correctness is independent
of and complementary to whatever the infra triumvirate surfaces.

---

## Scaffold landing

AY-II opens at the commit that lands the split + this scaffold.
The four audit artefacts at `audit/AUDIT-{A,B,C,D}-*.md` were
cherry-picked from their worktrees during the pass-I → pass-II
transition and placed under this pass's `audit/` directory per
the multi-pass-tranche edict
(`docs/instructions/tranche/SPEC.md` §Multi-pass tranche split).

The plan (`AY-II.md`), wave specs (`waves/W0.md` + `waves/W1.md`),
and this PROGRESS were authored without an execution dispatch.
Any sub-agent dispatched into AY-II waves operates on the scaffold
as-is; mid-wave plan edits follow the SPEC §Scope-reveal protocol.

W7's preempted worktree from AY-I — and the four audit worktrees —
are discarded as part of this scaffold commit. The W7 draft fix
(`prev < new_idx` guard in `TapeBuilder::note_push`) is explicitly
abandoned; the architectural consolidation in W0 supersedes.

## Scaffold revision — gestalt tightening

A senior-perf-engineer pass against the initial AY-II scaffold
surfaced three architectural errors and three scope gaps. The
scaffold is rewritten accordingly:

- **Fused pipeline, not a second parse.** The initial scaffold
  routed `Parsed::to_value()` through `parse_with_visitor` — a
  second parse of the source. Corrected: `AY-II.W0.c` introduces
  `ValueBuilder<R>` parallel to `TapeBuilder`, constructing the
  `<Grammar>Value` in lockstep with the tape during the single
  parse pass. `to_value()` is a thin projector over the already-
  constructed value.
- **Peer-referenced close gates, not internal ratios.** The
  initial scaffold keyed close on `bbnf_value_* / sonic_value_*`
  only. Corrected: `AY-II.W1` adds `crates/core/benches/json/competitors.rs`
  + `crates/core/benches/css/competitors.rs` publications vs
  sonic-rs + simd-json + lightningcss + cssparser.
- **CSS typed-semantic parity as hard close gate.** Corrected:
  `AY-II.W0.d` extends grammar-derived typed projection to cover
  lightningcss's typed surfaces (rule, declaration, value,
  selector families); `AY-II.W1` gates on
  `lightningcss_parity.rs` + `css_l4_canonical_parity.rs` +
  `typed_accessor_surface.rs` all green.
- **Structural scan promoted, not retired.** Corrected:
  `AY-II.W0.e` migrates `StructuralIndex` + `scan_structural`
  into cursor API + emitted navigation primitives with
  grammar-derived activation policy (CSP-inferred
  alphabet-density + digraph-signature drives per-grammar-per-rule
  emission). `navigate_tape` as a dead free function dies; the
  capability lives.
- **Projection totality as hard invariant.** Corrected:
  `crates/core/tests/projection_totality.rs` asserts
  `PROJECTION_DIRECT_TO_STRUCT.len() == materializer count ==
  consumer count` per grammar. W0 close + W1 close both verify.
- **BBNF + Sheets + CSS L4 first-class peers to JSON.**
  Corrected: every wave boundary runs the full 5-bench fat-LTO
  matrix; samply per primary grammar (not JSON alone).

The main rewrite rule: AY-II closes on ONE path, no second parse
hidden in `to_value()`, no consumerless substrate surfaces, no
JSON-only parity close. All semantic information grammar-derived
via CSP + egraph — no hardcoded bindings for any grammar.
