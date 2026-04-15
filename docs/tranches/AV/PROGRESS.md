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

