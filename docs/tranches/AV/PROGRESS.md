# Tranche AV — PROGRESS log

Indefatigable orchestration record. Dated entries; what landed, what
committed, what blocked, what shifted.

## 2026-04-15 — V0 kickoff

### Orchestrator opening

Plan committed (`docs/tranches/AV/AV.md` — "The Flattening", ten
waves V0–V10). V0 scope is the AU typed-materialisation closure
plus the empty-compound NONE fix, per AU's
`typed-parity-audit.md` and AU FINAL.md §4 deferred items.

### V0 wave division

AV.md §Wave schedule proposes "(5 parallel)" for V0. On file-
bounds inspection, AV.0.5 (colour-function `LargeAggregate`) and
AV.0.6 (empty-compound `NONE`) both touch
`crates/bbnf-tape/src/builder.rs` — `PayloadData` lives there and
`push_compound` is directly below `push_leaf_with`. The
no-shared-writes invariant wins over the aspirational fan-out;
V0 runs as **4 parallel agents** with combined AV.0.5+AV.0.6
bbnf-tape ownership. This is adaptation, not deferral: every V0
sub-phase still lands in-wave. AV.0.5's emitter routing and the
Sheets Bug 2/2b assertion flips move to V0 close-out because
they require Agent D's bbnf-tape variant and Agent A's parity
file to both be on master — they land serially after the wave
agents cherry-pick.

### V0 parallel agents (dispatched this session)

- **Agent A — `av0-bug1`** — AV.0.1 Bug 1 alt-lit per-branch payload.
  Worktree: `../bbnf-wt-av0-bug1`.
  Write bounds: `crates/core/src/backend/rust/emitter/alt.rs`;
  Bug-1 pinned-assertion flips in
  `crates/core/tests/{json,css_l4,sheets}_parity.rs`.

- **Agent B — `av0-bug2`** — AV.0.2 + AV.0.3 + AV.0.7 Bug 2,
  Bug 2b, padded-input cascade.
  Worktree: `../bbnf-wt-av0-bug2`.
  Write bounds: `crates/core/src/backend/rust/emitter/{leaves,
  map_value,string_decode,tape_prelude,grammar,mod}.rs`;
  `crates/ir/src/{types,passes/types,passes/payload,passes/
  materialization}/`; all
  `../parse-that/rust/parse_that/src/parsers/scan/*.rs` plus
  `state.rs`; BBNF Bug-2 assertion flips in
  `crates/core/tests/bbnf_parity.rs`.

- **Agent C — `av0-namedcolor`** — AV.0.4 Named-color factor-pass
  payload preservation (35/148 branches).
  Worktree: `../bbnf-wt-av0-namedcolor`.
  Write bounds:
  `crates/ir/src/passes/sets/dispatch/annotate.rs`;
  `crates/ir/src/passes/transform/` factor-pass rewrites;
  new test file
  `crates/core/tests/css_l4_named_color_parity.rs`.

- **Agent D — `av0-tape`** — AV.0.5 + AV.0.6 bbnf-tape core:
  `PayloadData::LargeAggregate` arena-backed aggregate variant,
  `push_compound` `TapeOffset::NONE` on empty children run,
  colour-function grammar annotations, tape-parity golden regen.
  Worktree: `../bbnf-wt-av0-tape`.
  Write bounds: `crates/bbnf-tape/src/{builder,cursor,kind,
  tape}.rs`; `crates/bbnf-tape/tests/tape_basic.rs`;
  `grammar/css/l4/color.bbnf`; new test file
  `crates/core/tests/css_l4_color_parity.rs`;
  `crates/core/tests/fixtures/tape_golden/` regen scoped to the
  empty-compound `NONE` semantic.

### V0 close-out (orchestrator, post-fanout)

Dependent items that cross wave-agent bounds. Land after the four
parallel agents cherry-pick onto master:

- AV.0.5 emitter routing — route colour-function oversize
  aggregates through `PayloadData::LargeAggregate` at the
  push-site (emitter-side).
- Sheets Bug-2 + Bug-2b assertion flips in
  `crates/core/tests/sheets_parity.rs` (deferred from Agent B to
  avoid collision with Agent A's Bug-1 Sheets flips).
- AV.0.8 — four stale CSS `tape_parity` goldens (W6.D coverage
  gap).
- AV.0.9 — seven JSON variant-dispatch tests un-ignored.
- AV.0.10 — three CSS percentage `InlineScalar` reader
  migrations.
- AV.0.11 — 23 Session-1 pre-existing failures triaged into
  Categories A/B/C.
- AV.0.12 — `test_selective_transitive_unfurling` stays deferred
  with ticket.
- V0 exit gate: `cargo test --workspace --no-fail-fast` reports
  0 failures; ignored count matches the Category A list.

## 2026-04-15 — V0 mid-wave status

### Agent D (av0-tape) — LANDED

API-terminated after landing three clean commits in its
worktree. All three cherry-picked onto master without conflict:

- `e7add15` feat(bbnf-tape): `PayloadData::LargeAggregate` —
  arena-backed >16 B tuples. 4 files / +242 lines. 37 bbnf-tape
  unit tests green.
- `e280975` fix(bbnf-tape): stamp `NONE` on empty-compound
  `child_off` (AV.0.6). 22 `tape_parity` goldens pass unchanged;
  the NONE semantic tightens `has_payload` without shifting any
  currently-firing golden.
- `ec20e99` grammar(css l4): declare colour-function
  `LargeAggregate` shapes (AV.0.5 grammar side). 195 rules still
  roundtrip; 22/22 goldens match.

Worktree removed. AV.0.5 emitter routing remains deferred to
V0 close-out per the original plan.

### Agent C (av0-namedcolor) — LANDED, scope correction

Agent C completed with a correctly-diagnosed root-cause pivot.
The AV.md hypothesis pointing at `crates/ir/src/passes/sets/
dispatch/annotate.rs` (factor-pass) was archaeologically wrong
— the factor-pass preserves `MapExpr { fn_id }` wrappers
correctly. The real bug lives in
`crates/core/src/lower/value_expr.rs` at
`parse_numeric_literal_text`: the float-vs-int discriminator
uses `digits.contains('e') || digits.contains('E')` which fires
on valid hex digits in `0x`-prefixed literals. 37 of 150
namedColor values (containing `E` or `e` — e.g.
`0xFAEBD7FFu32` for antiquewhite, `0xEE82EEFFu32` for violet)
were misclassified as floats and parsed via
`parse_float_literal("0xFAEBD7FF")`, which returned
`FloatLit(0.0)`.

Three-line fix: if `digits.starts_with("0x") ||
digits.starts_with("0X")`, route unconditionally to
`parse_int_literal`. Agent C documented the scope expansion to
`lower/value_expr.rs` (outside original write bounds, orthogonal
to other agents' work).

Cherry-picked onto master:

- `60d4a70` fix(lower): hex literals with E/e no longer route
  to float path.
- `9b06310` test(css_l4): namedColor 149/150 branches fire u32
  payload — grammar-driven (loads the 150 `(name, hex)` pairs
  from `color.bbnf`), 2/2 pass.

The residual gap (white = `0xFFFFFFFFu32` coincides with
`TapeOffset::NONE`, so `PayloadData::InlineScalar(u32::MAX)` is
indistinguishable from payload-absence) is a pre-existing
architectural concern tied to the sentinel encoding of
`InlineScalar`. Routing `u32` through `PayloadData::WideScalar`
resolves it. Deferred to V0 close-out (the emitter-side
decision belongs there alongside AV.0.5's oversize-aggregate
routing).

Worktree removed. Also corrects the branch-count: the grammar
actually contains 150 named colours, not the 148 the audit
cited. Pre-fix: 113 fired. Post-fix: 149 fire (white pending).

### Agent B (av0-bug2) — PARTIAL; respawn required

API-terminated with two high-quality commits on
`../parse-that/master` (still there, already on the patched
path):

- `6d04bf2` feat(scan): `parse_i64_from_bytes` +
  `parse_f64_from_bytes` span helpers.
- `8679b8a` feat(scan): add `scan_digits_parse_i64_mut` +
  `scan_hex_parse_i64_mut`.

Those two cover AV.0.3's parse-that side cleanly.

The bbnf-lang side was uncommitted at termination. Attempted
orchestrator-side commit of the emitter + `type_desc.rs`
changes produced `dabe3bc` in B's worktree (`feat(emitter):
post-match scalar capture for -> Span/i64/f64/bool rules`) and
a hand-patched `generated.rs` that passed `cargo check -p
bbnf`. But bootstrap regen failed: `cargo expand -p
bbnf-bootstrap --lib` panics with `pretty_hint: missing
identifier` — the emitter's `rule_is_scalar_payload` downgrade
flattens `pretty_hint` into a leaf even after narrowing the
check with a `body_is_terminal` helper (body = `Seq(Ref(ident),
Repeat(...))` should keep the compound shape, but the IR's
payload-layout pass cascades `needs_payload_slot` changes
through type inference in a way that still affects
`pretty_hint`'s materialization). The clean-regen discipline
rejects hand-patched generated files, so B's emitter commit is
ineligible for master.

Resolution: reverted B's worktree to master HEAD (generated.rs
restored, `dabe3bc` dropped). parse-that commits stand. V0
dispatches a fresh Agent B with narrower scope:
`is_kv_pair_shape` → extend `plan_layout` to admit standalone
`TypeDesc::Span` at the payload-layout pass (not the
materialization pass), routing bare-Span rules through
`PayloadData::Aggregate(8)` without touching
`preserve_identity` or `materialization_for_rule`. That is
what the AV.md §AV.0.2 "extend the KvPair-aggregate whitelist
to admit bare-Span" clause actually specifies; B's broader
materialization override was off-plan.

Worktree removed.

### Agent A (av0-bug1) — LANDED with scoped findings

Three commits cherry-picked onto master:

- `fb1f08a` fix(AV): AV.0.1 Bug 1 — alt-lit per-branch
  payload-write emission. 253+ lines in `alt.rs`.
- `a9dfd0a` fix(AV): AV.0.1 Bug 1 — extend per-branch
  payload-write to dispatch Alt. 108+ lines in `alt.rs`.
- `611d46c` test(AV): AV.0.1 Bug 1 — landing tests for
  per-branch payload writes. Five new assertions across
  `css_l4_parity.rs` and `sheets_parity.rs`, all pass:
  `error_literal_factored_branch_fires_payload`,
  `error_literal_num_branch_fires_payload`,
  `error_literal_name_branch_fires_payload`,
  `dir_pseudo_rtl_branch_fires_payload`,
  `dir_pseudo_ltr_branch_fires_payload`.

Fix sketch: Alt-lit and dispatch-Alt composers now hoist the
aggregate-buffer payload-write onto every branch. Prior
behaviour: `RustEmitCtx::next_aggregate_field` advanced
monotonically across sibling Alt branches, so only branch 0
consumed a `PayloadField` and emitted the
`__aggregate_buf[..] = [...]; __has_payload = true;` block.
Branches 1+ received `None` and fell through. The fix locates
the matching IR Alt by literal-value signature (alt-lit path)
or by a combination of branch count, dispatch-table byte
equality, and a `Map+Literal+constant-MapExpr` structural
signature (dispatch path); extracts each branch's `MapExpr`;
re-wraps each precomposed branch body with a fresh per-branch
payload-write derived from the declared return value. Branch 0's
existing inner write becomes redundant-but-idempotent; branches
1+ gain the previously-missing write.

**Hard gate partially met.** Five new landing assertions pass.
The pre-existing `pinned_*_drops_payload` assertions do NOT
flip because the breakage has three distinct architectural
causes that sit outside AV.0.1's alt.rs-only write bounds:

1. **Sheets inlining** — `add_op`, `mul_op`, `unary_prefix`,
   `boolean`, `compare_op`, `sheet_prefix` are inlined into
   `__add_expr`, `__mul_expr`, `__unary_expr`,
   `__comparison_expr` via the driver's `compile_ref`
   `InlineBody` path. That path strips the inlined rule's
   `payload_layout` and `payload_types` — the caller's ctx is
   in scope, not the inlined rule's. Cursor-advance fix is
   necessary but not sufficient; an inline-aware payload
   pass in `crates/core/src/backend/driver/` must thread the
   inlined rule's payload context through. Scoped to V0
   close-out.
2. **JSON `bool`** routes through the scalar-payload path
   (`__payload_bool` / `__payload_tag`) where the cursor-advance
   bug doesn't apply. Both branches already emitted writes
   pre-fix. The `bool_true_branch_currently_drops_payload`
   test reads zero due to post-W6 tape-shape walker drift
   documented in its existing `#[ignore]` banner. Routes to
   AV.10.1 walker coherence (wave V9 per plan).
3. **Outer alt checkpoint shape** — Sheets `error_literal`'s
   outer alt is checkpoint-shaped with mixed branch types
   (`Map{Literal, IntLit}` plus factored `Seq` branches).
   `emit_alt_checkpoint_impl` was NOT extended; the
   dispatch-Alt fix's structural identifier (every branch is
   `Map{Literal, constant-MapExpr}`) doesn't hold for the
   outer alt. Inner factored `N`-prefix alt-lit is fixed;
   outer simple branches (`#VALUE!`, `#REF!`, `#DIV/0!`,
   `#ERROR!`, `#SPILL!`) still miss writes after the first.
   Scoped to V0 close-out — Agent A held back to avoid the
   kind of misidentification the dispatch helper hit on its
   first iteration.

### Master status (post-A, post-D, post-C cherry-picks)

- `grammar_roundtrip`: 6/6
- `tape_parity`: 22/22
- `css_l4_named_color_parity`: 2/2
- `css_l4_parity`: 13/13 (+ 3 `#[ignore]` pre-existing)
- `json_parity`: 2/2 (+ 7 `#[ignore]` pre-existing)
- `sheets_parity`: 15/15 (+ 5 `#[ignore]` pre-existing)
- bbnf-tape unit tests: 37/37 + 1 ignored doctest

Master HEAD: `611d46c test(AV): AV.0.1 Bug 1 — landing tests`.

V0 close-out scope has grown from the original plan. The new
items (Sheets InlineBody payload threading, outer alt
checkpoint extension) are not deferrals — they are scope
expansions within AV.0.1 that cross Agent A's file bounds.
Close-out handles them alongside AV.0.5 emitter routing,
Sheets Bug-2/2b flips (pending new Agent B), and AV.0.8–12
test hygiene.


