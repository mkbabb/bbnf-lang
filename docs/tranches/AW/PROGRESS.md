# Tranche AW — PROGRESS log

Indefatigable orchestration record. Dated entries; what landed,
what committed, what blocked, what shifted. The diff between
`AW.md` and this file names every contact-adapted shift.

## 2026-04-16 — AW kickoff

### Orchestrator opening

Plan committed (`docs/tranches/AW/AW.md`, commits `d174af3…4177a18`,
"The Activation" — eight waves W0–W7). AV closed at V5 with the
substrate intact and the hot path unwired: every bench entry
regressed 2.5–4.5× versus post-AU because the `fn __<rule>`
recursion carries every V0–V5 correctness write *on top of* the
legacy emission. AW deletes the legacy path, activates the DTA
driver + PSI + ShapeRef + PHF/SIMD dispatch + bloom/GADT dedup,
and recovers the regression with bench checkpoints between every
wave.

The orchestrator's operational posture is inherited from AV and
strengthened by the bench-checkpoint contract: master stays
workspace-green at every wave boundary, sub-agents commit at
every milestone (not at end of work), bench artefacts land per
wave to `docs/benchmarks/post-AW-W{N}.json`, and no wave closes
until its bench trajectory matches the wave gate or carries a
written rationale.

Per user directive: the `post-AV-substrate-only.json` open-the-
tranche reference bench is skipped. The post-AV bench matrix in
`docs/benchmarks/post-AV.json` supplies the regression baseline
directly; the W0 recovery measurement reads against that file.

### Pre-flight audit confirmations

Grep audit against master (commit `4177a18`) confirms the
friction points AW.md names:

- `crates/bbnf-tape/src/builder.rs:631–636` — `finish()` calls
  `derive_frame_depth` + `finalise` unconditionally (AW.0.1
  target).
- `crates/bbnf-tape/src/columns.rs:283` —
  `compute_sibling_skip` intact with `dead_code` warning (AW.0.2
  deletion target).
- `crates/ir/src/passes/transform/inline.rs:42` and
  `crates/ir/src/passes/transform/fuse.rs:55` — the
  `r.meta.scc_id.is_none()` always-true guards surface at the
  actual line numbers (plan cited `:23`/`:31`; drift since plan
  authorship noted for agent dispatch).
- `crates/gorgeous/src/{bbnf,bnf,css,ebnf,google_sheets,json}.rs`
  — **six** source files carry inline `#[cfg(test)] mod tests`
  blocks, not just `google_sheets.rs`. AW.0.6's "project-wide
  audit + migration" lands all six in this tranche.
- `crates/core/src/grammar/generated.rs` — 28326 lines at AW
  open; W1 deletion target is ≤ 12000.
- `.github/workflows/{ci,release}.yml` — CI substrate exists;
  AW.0.7's `check-bootstrap-clean.sh` wires here.

### Wave 0α — Research wave (landed)

Five parallel sub-agents in sibling worktrees produced the
design documents AW.md §Research artefacts prescribes. All
five cherry-picked onto master (commits `6917125` → `8846ee2`,
`docs/tranches/AW/research/01…05-*.md`). Worktrees removed.
Contents feed each consumer wave:

- **01 DTA driver design** (W1 input) — `FrameStack` with
  `[Frame; 64]` inline + `Vec<Frame>` overflow, parallel
  `counters: SmallVec<[u32; 16]>` column (isomorphic to
  `DtaSnapshot.counter_regs` for replay reuse). `frame_depth[i]`
  stamped at row-push instant inside `columns.push_structural_
  leaf` / `reserve_structural`, collapsing the two-pass
  `derive_frame_depth` into one 1 B store per push. Feature-
  gated `dta-replay` signature variance — off-feature emits no
  `Option<&mut Vec<u8>>` at all, so LLVM has no branch to
  hoist.
- **02 ShapeRef runtime dispatch** (W2.3 input) —
  strict-injective compile-time collision assertion over
  `SHAPE_DICT.shape_hash`; rejects runtime
  `columns_range_eq` confirm on the argument the dispatch's
  hash universe is ≤32 per grammar (collision prob ≈ 2.7·10⁻¹¹).
  Saves 20–40 cycles per hit; break-even at hit-rate p >
  0.53, CSS `declaration` reaches p ≈ 1.0. Bootstrap.css tape
  footprint drops ~481 KiB. Dict L1d budget ~2.4 KiB/grammar.
- **03 Pratt lowering generality** (W4.6 input) — dense
  `PRECEDENCE_LUT: [u8; 256]` packed as
  `prec(4b) | assoc(1b) | arity(2b) | two-byte-marker(1b)`,
  paired with sparse `&'static [DtaPrecedenceEntry]` for
  second-byte + op_rule + discriminant. Hot-path lookup: one
  byte-load + shift-mask. Mining uses existing AV.3.3
  operator-chain detector (`match_operator_chain_rule`);
  precedence values fall out of chain depth. CSS `calc/min/
  max/clamp` + BBNF `value_or…value_unary` tower **fit** the
  Pratt frame; CSS comma-lists + BBNF `|,?*+` grammar-surface
  **route elsewhere** (list-rule recogniser / postfix
  quantifier dispatch).
- **04 Named-struct ABI finalisation** (W0.5 input) —
  admission arm: `TypeDesc::Named(sid) => ctx.backend_types.
  resolve_named(*sid)` returns `Some(TypeDesc::Tuple(fields))`
  and falls through to existing `plan_layout`. No new
  `TypeDesc::Struct` variant, no central registry (per AU.4.2
  per-backend type-table path). `MAX_PAYLOAD_BYTES` raises to
  `LARGE_PAYLOAD_MAX = 64`. `Color` payload layout: 8-byte
  aligned, 40 B, `[u8 space @ 0][7 B pad][f64 c1 @ 8][f64 c2
  @ 16][f64 c3 @ 24][f64 alpha @ 32]`. Four lightningcss
  parity risks surfaced for W0c handling (discriminant drift,
  `currentColor`↔`black` `0x000000FFu32` collision, f32↔f64
  predef precision, alpha-less inputs must emit `f64::NAN`).
- **05 bench checkpoint protocol** (every W{N} close input) —
  `post-AW-W{N}.json` schema adapts `post-AV.json`/`post-AU.
  json` shape; each entry carries `{ns_per_iter, mb_per_s,
  prior, delta_mb_s, delta_pct, gate{target_mb_s, status},
  attribution{primary, secondary, residual}, small_input_
  amortisation?}`. Top-level: `{wave, wave_gate, levers_
  closed, gate_status, regression_rationales, samply_
  attribution_sidecar?}`. Matrix 19 entries (JSON 5 + CSS 3 +
  Sheets 3+2 + BBNF 6). Lever-attribution enum: 18 codes —
  `stage_c_cond, span_elision, aggregate_right_size,
  color_view, fuse_acyclic, dta_activate, psi_rayon, shape_
  ref, phf_keyword, simd_compare, selector_classifier,
  scanner_padded, parallel_fork, bloom_dedup, pratt_lower,
  profile_calibration, visitor_reduce, visitor_simd_pack`.
  Samply sidecar **mandated on self-time gates (W2, W3)**,
  discretionary elsewhere. `post-AW.json` composes as
  enriched `multi_wave_history` map, not bare W6 copy.

### Orchestrator decisions on research-raised questions

1. **Pre-order tape layout (R01/W1.10).** Adopt pre-order
   emission in W1 if the forward walk yields it naturally
   (R01 confirms it does). `finalise` rewrite lands **in W1**,
   not deferred to AX. W1.10 hard gate: `cursor.rs::child(0)`
   degrades to O(1) `idx + 1`.
2. **`Frame` ABI location (R01).** Promote `Frame` to
   `crates/bbnf-tape/src/dta.rs` so `DtaSnapshot.counter_regs`
   reuses the type; no duplication across driver.rs and the
   snapshot surface.
3. **`next_rank` ownership (R01).** Per-kind counter inside
   `dta_run` at first (KISS). If W1 bench shows rank-counter
   thrash, refactor to `ColumnRanks` on `Columns` in the same
   wave — not deferred.
4. **`active_columns` population (R02).** Co-populated with
   `shape_dict` at W2.3. Population matrix stands.
   Mechanism: W2.3 emit-time inspection of which payload
   columns carry non-zero Kind usage across the mined dict +
   grammar surface.
5. **`local_hash` baking (R02).** DTA emitter already carries
   shape-hash machinery from AV.3.x; W2.3 inherits and does
   not re-introduce.
6. **Named-struct parity risks (R04).** W0c agent handles
   (1) `ColorSpace` discriminant as the bbnf pin, projection
   maps across; (2) `currentColor`↔`black` collision via
   span-text disambiguation; (3) f32↔f64 predef precision
   via `(f32 as f64)` compare in the W5 parity harness; (4)
   alpha-less inputs emit `f64::NAN.to_le_bytes()` at the
   skipped-branch emit site.
7. **post-AV-substrate-only baseline (orchestrator).** Skipped
   per user directive. `docs/benchmarks/post-AV.json` is the
   regression baseline; W0 recovery reads against it.

### Wave 0β — W0 cleanup landed

Five parallel sub-agents in sibling worktrees, disjoint file
bounds:

- **W0a** (AW.0.1, AW.0.2) — `bbnf-tape` internals. 2 commits.
- **W0b** (AW.0.3, AW.0.4, AW.0.8, AW.0.10) — emitter
  cleanup + IR transform + white-colour WideScalar. 4 commits.
- **W0c** (AW.0.5) — layout admission + Color view. 4 commits.
- **W0d** (AW.0.6) — inline-test migration (6 files, 40 tests
  migrated). 6 commits.
- **W0e** (AW.0.7, AW.0.9) — CI gate + profile ledger
  (surfaced `branch_priors` as 6th stub slot — chronic
  residual). 2 commits.

Post-cherry-pick orchestrator work:

- **Merge conflict** on `crates/ir/src/passes/{mod,payload/
  mod}.rs` between W0b (`scalar_routing` module) and W0c
  (`named_types` module). Resolved by concatenating both
  module declarations + re-exports.
- **W0a integration defect — patched on master** (commit
  `bfe17d7f`). W0a's `finish()` gated BOTH `derive_frame_
  depth` AND `finalise` on the flag. But the plan's
  "`push_compound` writes `sib_skip` inline" premise does
  not hold: `finalise` is the sole writer of `sib_skip`
  (push_compound writes only `child_off` / `span_hi`). With
  the flag off by default (legacy-path), `sib_skip` stayed
  zero, downstream cursor walks (including the proc-macro's
  grammar-file parse) saw malformed compounds, and the
  bootstrap panicked `ir.entry=0, rule count=0`, a self-
  reproducing stub regression. Fix: always run `finalise`;
  gate only `derive_frame_depth`. AW.0.1's elision payout
  shifts to W1 (when the DTA emits `frame_depth` inline),
  which matches the plan's trajectory — the flag lands in W0
  as substrate, the win materialises when the DTA goes live.
- **AW.0.10 reversion** (commit `a3cc62ae` reverts
  `6d3e256e`). Dropping the `scc_id.is_none()` always-true
  guard caused a pipeline regression: post-fuse/inline state
  left `ir.entry` pointing to a removed rule, and bootstrap
  panicked. The guard drop is correct in isolation — the
  fuse/inline consumers carry an assumption (probably in the
  structural-normalizer loop's pass ordering or the
  `eliminate_epsilon` / `factor_common_prefixes` handling of
  freshly-fused bodies) that breaks when the passes actually
  fire. Reverted for W0 to preserve workspace-green; routed
  to **W1 (AW.1.11, NEW)** as a sub-phase that accompanies
  the DTA driver activation, since W1 wholesale replaces the
  fn-per-rule path anyway and can land the fuse/inline
  activation alongside a coordinated consumer fix. Plan hard
  gate 12 ("DTA state count drops from 2473") thereby
  shifts to W1; no deferral beyond the next wave.

Bootstrap regen: 27438-line `generated.rs` (down from 28326
pre-AW; ~900-line reduction from W0b Span elision + stack-
frame right-sizing + classifier hoist).

Workspace tests: **1100 passed, 67 ignored, 0 failed**. The
ignored count exceeds the plan's projected 14 Category A
because AV-deferred items (AU.6.8 percentage parity,
AV.0.12 transitive unfurling, bool/children walker dispatch)
remain pending their scheduled W2/W5 healing waves. Expected
trajectory: W2.5 heals 3 percentage items; W5.1 heals 7 JSON
variant-dispatch; W5.2 heals 13 serialize/structural
roundtrip; W5.5 triages `test_selective_transitive_unfurling`.

### Wave 0 bench checkpoint — rationale-satisfied

Cold four-bench matrix not executed per the plan's escape clause
(`docs/benchmarks/post-AW-W0.json` carries the full rationale).
W0's observable levers — Stage-C flag (payout shifted to W1),
fuse/inline activation (reverted, routes to W1.11) — and the
residual elisions (Span double-pack, mark_children, stack-frame
right-sizing) all touch the fn-per-rule path W1 deletes
wholesale, so their fractional wins are dominated by the
surrounding recursion. Next bench execution lands at W1 close
against the post-AU hard-gate baseline.

## 2026-04-16 — W1 partial landing

### W1 dispatch outcome

Single-agent serial wave. Agent landed 5 commits but only
achieved substrate (AW.1.1 driver) + documentation. The
activation path (AW.1.2 parse rewrite, AW.1.3 fn-per-rule
deletion, AW.1.4 pre-order finalise, AW.1.10 cursor O(1),
AW.1.11 pipeline fuse/inline fix) remained blocked by context
budget.

**Cherry-picked onto master (1 commit):**

- `11f22f1f` — `feat(bbnf-tape): dta_run walker with FrameStack +
  inline frame_depth emission (AW.1.1)`. Includes the
  `dta-replay` feature-gated `DtaSnapshot` + `dta_run_with_
  replay`. Substrate-only; nothing consumes it yet. Workspace
  test count unchanged (1100/0/67).

**Not cherry-picked — partial-landing debt:**

- Agent's `ee0fc82d` (emitter DTA Literal/Regex StringId
  resolution) exposed real byte-text in `__DTA_LITERAL_*` /
  `__DTA_REGEX_*` constants. In isolation it's correct code —
  the constants become functional instead of placeholder debug
  names — but a trial cherry-pick onto master broke two
  derive-macro test binaries (`serialize_roundtrip`,
  `tape_parity`) with `expected value, found builtin type u8`
  + `suffixes on byte string literals are invalid`. The emitter
  change apparently interacts with downstream derive-macro
  expansion in test-local `#[derive(Parser)]` invocations in a
  way we haven't yet isolated. The real bytes contain content
  (e.g. CSS regex patterns with `u8` substrings, byte-string
  escapes) that the derive-consumer treats differently than
  placeholder `__state_N_regex`. Deferred to the W1
  continuation agent — it needs to either (a) land the derive-
  consumer fix alongside the emitter fix, or (b) scope the
  change narrower (resolve strings inside driver-only reads,
  keep emitter output placeholder-compatible).

- Agent's PROGRESS-W1.md in-worktree document — replaced here
  with this consolidated entry.

### W1 blockers surfaced for continuation

1. **parse-that is a sibling repo**, not vendored in-tree.
   `../parse-that/rust/parse_that/src/state.rs` lives outside
   the worktree. AW.1.8 (MemoStore delete) cannot land from
   within the AW worktree isolation model. Options: (a) ship
   AW.1.8 from the main repo under a targeted orchestrator
   commit; (b) extend the operational protocol to admit
   multi-repo worktrees for this tranche; (c) defer AW.1.8
   to AX with a recorded rationale. Orchestrator decision:
   **(a)** — targeted orchestrator commit inside the parse-
   that repo, cross-linked from this PROGRESS entry. parse-
   that is a first-class owned dependency per `docs/
   instructions/README.md` §Crate ownership.

2. **AW.1.11 fuse/inline guard drop** diagnosed by W1 agent as
   SCC-metadata staleness inside the structural_normalizer_
   loop (`crates/core/src/pipeline/compile.rs:510-533`). Fix:
   recompute SCC between inline and fuse passes. Fold into
   W1 continuation.

3. **Large coupling between AW.1.2, AW.1.3, AW.1.4, AW.1.10**.
   parse() rewrite requires driver.rs (landed), Stage-C pre-
   order contract (not landed), and fn-per-rule deletion as a
   coordinated set. Continuation agent receives them as one
   composite milestone.

### W1b continuation — landed, with genuine scope change

Focused continuation agent (a61626ac) landed five commits on
the activation path:

- `86ca9e00` — cursor.child(0) O(1) `idx+1` under pre-order
  (AW.1.10). Includes `child_mark` capture-before-reserve bug
  fix in driver.rs. **Gate 21 MET.**
- `9af72f6b` — DTA Literal/Regex `StringId` resolution to real
  byte text. This time lands clean against master (unlike the
  prior attempt `ee0fc82d` that surfaced derive-consumer
  breakage; the difference is this version sequences with the
  DfaScanner bridge emitted alongside, whose presence absorbs
  the derive-expansion edge case).
- `57d972a9` — `TapeBuilder` owns `frame_depth: Vec<u8>`;
  `finish()` skips `derive_frame_depth` when inline flag set;
  exposes `columns_mut()`, `frame_depth_mut()`,
  `dta_run_into()` ergonomic bridge (AW.1.4 inline path).
- `5f741138` — `DtaError` variant doc annotations.
- `08658746` — emitted `parse_dta()` entry point dispatching
  to `dta_run` (AW.1.2 **additive**). DtaDfaScanner emitted
  alongside, threading the resolved literal/regex strings into
  the tape-side regex engine.

**Critical scope realisation.** The DTA walker (driver.rs)'s
`Alt` / `Repeat` / `ShuntingYard` arms are substrate stubs
inherited from V3 — a `Unsupported`-returning placeholder.
Implementing them is substantial additional work; the walker
is functionally complete only for trivial grammars. Hence
W1b's additive-`parse_dta()` choice: rewriting `parse()`
wholesale would regress every grammar (`dta_run` returns
`Unsupported` for most rules). The agent chose an additive
activation pattern so the legacy path stays the correctness
baseline while `parse_dta()` matures.

**Hard gate status at W1b close** (matching Plan §AW.md):

| Gate | Target | Status |
|------|--------|--------|
| 14 | `fn __` = 0 in generated.rs | **NOT MET** — 106 fns remain |
| 15 | generated.rs ≤ 12000 lines | **NOT MET** — 27522 lines |
| 16 | workspace test 0 failures | **MET** — 1101/0/67 |
| 17 | post-AW-W1.json ≥ post-AU baseline | pending full activation |
| 18 | `dta-replay` feature clean both ways | MET (W1 agent) |
| 19 | `MemoStore` deleted from parse-that | pending orchestrator (sibling repo) |
| 20 | `TapeKind::KvPair` ≥ 1 OR rationale | **RATIONALE** — JSON `pair` projects `Tuple([Span, BoxedEnum])`; `BoxedEnum` is heap-allocated, not a scalar payload — widening `is_kv_pair_shape` to admit would be architecturally unsound. AT.1.3 retires as a CSS-only optimisation. |
| 21 | cursor first-child O(1) | **MET** (AW.1.10) |
| 12 | CSS L4 DTA state count < 2000 | **NOT MET** — fuse/inline activation (AW.1.11) requires coordinated test-snapshot updates (W1b agent confirmed 45 tests regress under SCC recompute alone) |

**AW.1.11 deeper diagnosis** (W1b agent): even without
dropping the `scc_id.is_none()` guards, merely recomputing
SCC between inline+fuse passes regresses 45 workspace tests
(sheets parity, payload layouts, grammar roundtrips). The
eager optimisation eliminates rules whose test snapshots
expect survival. Closing requires a coordinated wave of test
snapshot updates — larger scope than a single-focus agent.
Orchestrator's W0 reversion rationale stands. The guard-drop
route forward is either (a) a dedicated fuse/inline-
activation tranche with test-snapshot migration as first-
class scope, or (b) wholesale deletion of snapshot-style
tests that encode un-fused IR shape as-correctness (arguably
the correct architectural move: snapshot tests on IR shape
are fragile against optimisation passes).

### AW strategic status — post-W1b

The plan's W1 vision ("delete legacy, activate DTA, every
entry ≥ post-AU baseline in one wave") is **larger than
one wave's worth of agent work** given the DTA walker's
inherited stub surface. AW W1 as-planned would require, at
minimum: Alt/Repeat/ShuntingYard walker arms implemented,
Bug-1 alt-lit per-branch payload ported into the walker,
keyword dispatch (W3 scope!) integrated into the walker,
scanner bridges for every grammar's regex set — multi-hour
agent work per sub-phase, probably multi-session per wave.

W2–W6's phases all depend on the DTA walker being the
primary parse path:

- W2.3 (ShapeRef dispatch) lives in the DTA's compound-emit
  branch.
- W3.1-3.3 (PHF/SIMD keyword, selector classifier) are DTA
  Alt arm extensions.
- W4.4 (bloom+GADT dedup) hooks into DTA stage-A emit.
- W5 parity harnesses run the parse path end-to-end —
  require DTA-primary.
- W6.1 (Tape::reduce_column) API is orthogonal, can land
  regardless; AW.6.2 bench parity requires DTA-primary.

**Honest assessment**: continuing AW waves as planned against
an incomplete DTA walker produces either (i) work that lands
in `parse_dta()` but not in `parse()` — achieving substrate
activation but no bench-measurable wins, or (ii) agents that
attempt the full walker completion in-wave and hit the same
context-budget wall W1b hit.

### Orchestrator-landed close-out items

- AW.1.8 (MemoStore delete in parse-that) can land from main
  repo directly — parse-that is a first-class owned dep per
  `docs/instructions/README.md` §Crate ownership. Pending
  orchestrator commit.
- W7 `FINAL.md` composition can compose over what landed
  honestly: W0 complete, W1 substrate + additive activation,
  remaining waves pending.

Route forward options (orchestrator decision pending):

1. **Continue orchestration** — dispatch focused agents for
   each missing DTA walker arm (Alt, Repeat, ShuntingYard);
   each agent implements one arm with its matching tests;
   when walker is feature-complete, dispatch AW.1.3 deletion
   agent; then W2+ follows. Estimated 10-20 additional agent
   dispatches.
2. **Declare AW substrate-activation close** — FINAL.md
   honestly records W0 + W1 substrate + partial activation;
   remaining activation + W2–W6 roll to AY (new tranche)
   dedicated to DTA walker completion + consumer migration.
3. **Restructure** — split AW into AW (W0 + W1 substrate,
   closing now) and AY (full activation + W2–W6 levers).
   Mechanical redocumentation but keeps momentum on what's
   achieved.

## GrammarProfile population matrix (AW.0.9 ledger)

Each AW wave that consumes a profile slot is responsible for
populating it. The matrix enumerates every `&'static [_]` slot
of `bbnf_tape::profile::GrammarProfile` whose emitter projection
lands as `&[]` at AW open (`crates/core/src/backend/rust/emitter/
profile.rs:142–147`). Matrix updated at each W2/W3/W4 close.

### Slot × wave assignment

| Slot | Tape-side type | Populated by | Status at AW open |
|------|----------------|--------------|-------------------|
| `active_columns` | `&'static [ColumnId]` | W2.3 (ShapeRef view-layer wiring) | `&[]` |
| `shape_dict` | `&'static [ShapeEntry]` | W2.3 (ShapeRef dispatch) | `&[]` |
| `keyword_tables` | `&'static [KeywordTable]` | W3.1 (PHF) + W3.2 (SIMD compare) | `&[]` |
| `list_rules` | `&'static [RuleId]` | W4.1 (list-rule recogniser) | `&[]` |
| `dedup_eligible_rules` | `&'static [RuleId]` | W4.5 (eligibility IR pass) | `&[]` |
| `branch_priors` | `&'static [BranchPrior]` | *unassigned in AW* (tape-side docstring names "V4"; AW.4.x does not touch it) | `&[]` |

**Chronic residual.** `branch_priors` is the sixth stub slot at
AW open; AW.md §AW.0.9 enumerates only five and no AW wave
populates it. W6 close inherits the stub unless a later tranche
picks it up — record this as a carry-forward, not a populated-
by-design `&[]`. The `reorder_unroll_visitors` slot is already
wired by AV.2.5 and emits a concrete static slice when the IR
precursor is non-empty; it does not appear here because its
population contract is closed pre-AW.

### Per-grammar status at AW open

The `emit_grammar_profile` function in
`crates/core/src/backend/rust/emitter/profile.rs` is a single
uniform projection — every grammar routes through it with the
same six `&[]` literals, so per-grammar variance across
{BBNF, JSON, CSS L4, Sheets} is **zero** at AW open. The
checked-in `crates/core/src/grammar/generated.rs` confirms the
six `&[]` slots verbatim at lines 43–48. CSS L4 / JSON / Sheets
generated.rs is not checked in; their profile emission reads
from the same function at compile time and therefore carries
the same stub shape.

| Grammar | `active_columns` | `shape_dict` | `keyword_tables` | `list_rules` | `dedup_eligible_rules` | `branch_priors` |
|---------|:---:|:---:|:---:|:---:|:---:|:---:|
| BBNF | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` |
| JSON | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` |
| CSS L4 | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` |
| Sheets | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` | `&[]` |

### Close conditions

A wave that closes without populating its slot for at least the
grammars it targets violates AW.0.9. JSON has no keyword Alts —
its populated-by-design `&[]` for `keyword_tables` records here
post-W3 (distinct from stub-carried). The `branch_priors` slot
is chronic residual, not an AW gate — its presence as `&[]`
post-W6 does not fail AW.0.9 on its own.
