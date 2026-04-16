# Tranche AW — PROGRESS log

Indefatigable orchestration record. Dated entries; what landed,
what committed, what blocked, what shifted.

## Status dashboard — 2026-04-16

### AW-I — Activation (DTA primary, legacy deleted, fuse firing)

| Wave | Scope | Agents | Status |
|------|-------|--------|--------|
| W0 | cleanup + hygiene + Color view + CI gate | 5 parallel | ✓ landed |
| W1 | DTA substrate skeleton (walker + cursor O(1) + inline finalise + replay feature) | 1 serial | ✓ landed (stubs open) |
| W2 | walker + memo + SCC + audit + snapshot migration | 5 parallel (W2.1–W2.4 concurrent; W2.5 sequenced) | ✓ landed (workspace **1078/0/68**) |
| W3 | `parse()` swap + regen | 1 serial | ✓ landed (workspace intentionally unworkable) |
| W4 | legacy deletion + cyclic activation + walker extensions + scope-reveal | 6 sub-waves (α/β×4/γ/δ/ε/ζ), ~10 agents | ✓ landed (workspace unworkable; AZ scope-pivot opened) |
| W5 | FINAL-I + AZ.md authorship | 1 serial (orchestrator) | ✓ landed (post-AW-I.json deferred to AZ close per escape clause) |

Workspace at AW-I HEAD `ff0b7fe7`: **1101/0/67**.
Workspace at W2 close: **1078/0/68** (−22 DELETE, −1 new Category A
`serialize_roundtrip::css_simple` pending W4.5/follow-up).

Bench schedule: one cold run at AW-I.W5 close →
`docs/benchmarks/post-AW-I.json`. No per-wave checkpoints —
W2–W4 touch substrate the legacy path still dominates.

### AW-II — Optimisation + parity

| Wave | Scope | Agents | Status |
|------|-------|--------|--------|
| W1 | PSI rayon + ShapeRef + Bug 2b residuals | 3 parallel | pending |
| W2 | PHF + SIMD keyword + selector classifier + scanner closure | 4 parallel | pending |
| W3 | document-parallel + bloom + Pratt generalisation + profile calibration | 4 parallel | pending |
| W4 | walker + reader + sonic-rs + lightningcss parity harnesses | 3 parallel | pending |
| W5 | `Tape::reduce_column<C,R>` + SoA 4-lane SIMD pack + bench parity | 2 parallel | pending |
| W6 | FINAL + close | 1 serial | pending |

Bench schedule: per-wave cold run at W1–W5 close →
`docs/benchmarks/post-AW-II-W{N}.json`. W6 composes
`docs/benchmarks/post-AW.json` as multi-wave history.

### SoA 4-lane — tranche placement

The SoA-substrate reordered-unrolling kernel (AV.2.5) emits
today via `visitor.rs::emit_visitor_kernels` as a 4-lane
scalar left-fold-free reducer. **AW-II.W5.1** ships the
`Tape::reduce_column<C,R>` consumer API + per-active-payload-
column codegen specialisation driven by
`GRAMMAR_PROFILE.active_columns`, and promotes the scalar
4-lane to packed `f64x4` SIMD to clear AV's 6× gate.

### Research artefacts — `research/01…05-*.md`

Five design docs feed both tranches. `01-dta-driver-design`
+ `03-pratt-lowering-generality` drive AW-I.W2.1
(walker completion). `02-shaperef-runtime-dispatch` drives
AW-II.W1.2. `04-named-struct-abi-finalisation` already
consumed by AW-I.W0.5. `05-bench-checkpoint-protocol` drives
AW-I.W5 + AW-II per-wave benches.

---

## Historical log

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

### 12-tranche retrospective + TRANCHE_SPEC authored

Dispatched 12 parallel retrospective agents — one per tranche
AK through AV — producing forensic analyses at
`docs/tranches/AW/audit/{LETTER}-retro.md`. Synthesis at
`docs/tranches/AW/audit/SYNTHESIS.md` coalesces into a
chronic-deferral ledger, ten recurring anti-patterns with
tranche-by-tranche evidence, an AU-template of what worked, and
an anti-pattern → spec-section mapping.

`docs/instructions/TRANCHE_SPEC.md` composes `README.md`,
`PROFILING.md`, `RESEARCH.md` into a normative tranche-authoring
workflow, with every rule anchored to an anti-pattern the
retrospective surfaced. Key additions over prior edicts:

- **Activation-gate rule** — substrate additions require same-
  wave consumers + runtime-evidence hard gates.
- **Runtime-evidence clause** — hard gates close on bench /
  samply / cargo-expand / test, not on source `grep` alone.
- **Gate-off commit prohibition** — feature-flag-off shipping is
  deferral; AP.1's `structural_mode = false` is the cautionary
  tale.
- **Scope-reveal protocol** — default is re-plan-with-more-
  agents; mid-tranche scope pivots open a new letter.
- **Orchestrator role** — explicit enumeration of what the
  orchestrator delegates (implementation, tests, profiling,
  bench, doc authoring) vs preserves (orchestration, cherry-
  picking, claim-hardening, synthesis, re-planning).

AW disposition against the ten anti-patterns: 9 cleared, 1
refined. AW-I.W4.5 hard-gate 12 phrasing tightened — CSS L4
DTA state-count verification now calls `summarise` directly,
not `grep` on `generated.rs`.

Retrospective commits: `48c2b3fd`…`8de53e52` (12 tranche audits)
+ SYNTHESIS + TRANCHE_SPEC.

### AW re-plan — split into AW-I + AW-II

The orchestrator's earlier "substrate / activation split" retreat
is replaced with a full re-plan. [`AW-I.md`](./AW-I.md) carries
full activation in six waves: W0 + W1-substrate (landed),
walker completion + memo retirement + SCC plumbing + snapshot
audit (W2, 4 parallel), `parse()` swap + regen (W3),
legacy deletion + fuse activation + snapshot migration (W4,
5 parallel), FINAL-I + bench + close (W5). Walker stubs fill
in-tranche; `fn __<rule>` helpers delete in-tranche;
`MemoStore` retires in-tranche; fuse/inline fires in-tranche.
No `parse_dta`-style additive shadowing.

[`AW-II.md`](./AW-II.md) carries AV's optimisation substrate
— PSI rayon + ShapeRef + Bug 2b (W1, 3 parallel), PHF + SIMD
compare + selector classifier + scanner (W2, 4 parallel),
document-parallel + bloom + Pratt (W3, 4 parallel), walker +
reader + parity harnesses (W4, 3 parallel), `Tape::reduce_
column` + SIMD pack + bench parity (W5, 2 parallel), FINAL
(W6).

The edict now lives in `docs/instructions/README.md` §Code
discipline — **Execute the plan, not around it.** Scope-
reveal-under-contact is re-plan-with-more-agents territory,
not escalation territory.

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

## 2026-04-16 — AW-I.W2 execution + scope-reveal

### Wave dispatch

Four parallel sub-agents in isolated worktrees per AW-I.md
§"Wave schedule":

- **W2.1** (walker arm completion) — `../bbnf-wt-aw-w2-1-walker`.
  Owner: `crates/bbnf-tape/src/driver.rs`. AltLinear savepoint
  backtracking, Repeat lo..=hi iteration, ShuntingYard
  operator-precedence reducer. In flight.
- **W2.2** (MemoStore retirement) — `../parse-that-wt-aw-w2-2-memo`
  (sibling-repo worktree). Four commits, 230 deletions, zero
  bbnf-lang consumers of the removed APIs. Cherry-picked onto
  `parse-that` master (`907db32`).
- **W2.3** (SCC recompute plumbing) — `../bbnf-wt-aw-w2-3-scc`.
  One commit `b0e69f2d` cherry-picked as `c25e63a0`.
- **W2.4** (fuse-snapshot audit, read-only) —
  `../bbnf-wt-aw-w2-4-audit`. Audit landed at
  `docs/tranches/AW/audit/fuse-snapshot-migration.md`: 22
  DELETE / 44 UPDATE / 8 INVESTIGATE / 74 at-risk tests.
  Cherry-picked as `d102e007`.

### Scope-reveal — SCC recompute is semantics-changing

Plan's §W2.3 premise — "no behaviour change yet; workspace
unchanged 1101/0/67" — is contradicted by execution. Agent's
investigation + direct confirmation on master:

- `crates/core/src/lower/metadata.rs:27` stamps
  `scc_id = Some(id)` for every rule (including acyclic) at
  lowering time.
- `crates/ir/src/passes/sets/scc.rs:21` realigns to the
  canonical convention: `None` for acyclic, `Some(scc_idx)` for
  cyclic.
- Guards at `inline.rs:42` + `fuse.rs:55` read
  `r.meta.scc_id.is_none()`. Pre-W2.3 → always FALSE
  (lowering's unconditional Some stamp); passes dormant. Post-
  W2.3 → TRUE for acyclic rules inside the normaliser loop
  after `compute_scc` runs; `inline_acyclic` and
  `fuse_single_use` activate as a necessary side-effect of
  the SCC plumbing.

Cannot be worked around — any semantics-preserving variant of
W2.3 would either shadow-field the SCC (orthogonal-subsystem,
forbidden per edicts) or leave the lowering/pass-loop
convention mismatch intact (punts the activation to never).

Post-W2.3 workspace: **1041/60/67** (failing categories match
W2.4 audit: sheets parity, payload layouts, grammar roundtrips,
tape parity, TS backend snapshots).

### Re-plan — W2.5 absorbs snapshot migration from W4.5

Per TRANCHE_SPEC §"Scope-reveal protocol" default
(re-plan-with-more-agents, no deferral), W4.5's snapshot-
migration piece moves up into W2 as new sub-phase **W2.5**.
W2.5 consumes W2.4's audit, migrates the 74 at-risk tests
(22 DELETE / 44 UPDATE / 8 INVESTIGATE), and returns the
workspace to green before W3 opens its intentional-
unworkability window.

W4.5 retains its guard-drop — the guards' remaining pre-drop
effect is to keep passes off cyclic rules — plus any residual
snapshot updates the cyclic-rule extension surfaces. The plan
document and cross-tranche-debt table are revised accordingly
(AW-I.md §W2.3, §W2.4–W2.5, §W4.5, hard-gates summary,
cross-tranche table).

Invariants preserved: W2 exits green (1101 − DELETE +
INVESTIGATE-deltas); intentional unworkability stays within
W3-W4 as the plan declared; no deferral beyond the plan's
next wave.

### Orchestrator-landed artefacts (sequence from AW-I HEAD
`fb8dd225`):

- `817882a6` docs(AW-II): scope refinements — chronic
  deferrals folded into W2/W3.
- `d102e007` W2.4 audit.
- `c25e63a0` W2.3 SCC recompute.
- parse-that master advanced to `907db32` (W2.2, out-of-tree).

W2.1 still in flight. W2.5 pending dispatch once post-W2.3
test categorisation completes.

## 2026-04-16 — AW-I.W2 close

### W2.1 + W2.5 landings

- **W2.1** (walker arm completion) — three commits cherry-picked:
  `8df45be4` AltLinear savepoint backtracking, `c4cd7aaf` Repeat
  lo..=hi iteration with body-failure absorption, `97285f60`
  ShuntingYard operator-precedence reducer. Frame size widened
  32B → 40B to carry `lo`/`hi`/`last_pos`/`counter_optional_
  flag`. `Columns::truncate` + `PayloadStream::truncate` added
  as savepoint infrastructure. Reducer compounds use a post-
  order layout internally; the outer SY compound still satisfies
  `child_off == parent + 1` so `cursor::child(0)` O(1) fast path
  survives on the common path. Seven new walker_arms tests pass.
- **W2.5** (snapshot migration) — six commits cherry-picked:
  `02574ce9` delete un-fused IR-shape fossils (grammar_roundtrip
  set deleted wholesale), `404a5232` + `d366fe42` tape_golden
  regeneration (18 fixture files), `a03cef55` `fix(ir/passes):
  preserve typed-Map + consumer-pinned rules in fuse/inline`,
  `56be4534` payload_layouts thresholds, `f0de05e3` miscellaneous
  test reconciliation.

### Source-level pin predicates (`a03cef55`)

W2.5 surfaced two additional pin predicates for `inline_acyclic`
+ `fuse_single_use`:

1. **`body_has_map`** — true when the rule body contains any
   `IrNode::Map` anywhere (recursive scan). Merging a typed-Map
   rule into its caller drops the per-branch writes' aggregate
   buffer — the rule's `[Nu8]` / `[U8]` epilogue evaporates —
   silently violating the typed-materialisation invariant
   ("every `->` in the grammar must reach the tape emitter").
   `factor_common_prefixes` can migrate Map nodes deeper under
   Seq-wrapped prefix branches, so the scan is recursive rather
   than top-level-only.
2. **`is_consumer_pinned`** — true when the rule carries
   `@pretty` / `@debug` directives or the grammar has `@debug *`.
   Mirrors the gate in `materialization::pin_sweep::is_rule_
   pinned`. Pin-sweep runs after fuse; without this guard the
   rule disappears before its directive can propagate.

Both predicates duplicate across `fuse.rs` + `inline.rs` — the
same pattern as pre-existing `is_composite_seq`. Local pass
primitives, not cross-module API. `TypeMap` already tracks
typed rules; the body-shape scan is the expedient gate until a
unified "pin-candidate" predicate registry lands in a future
tranche. Registered as `pluggable_pin_registry` debt in the
AW-II seed items below.

### W2.5 Category A ignore

`serialize_roundtrip::css_simple` is `#[ignore]`d as a post-
DTA-exposed Category A item. `@pretty`-pinned rules survive
fuse/inline via `is_consumer_pinned`, but the serialize
pipeline constructs a view-layer shape that mis-dispatches at
offset 0. Destination: W4.5 or a follow-up tranche once the
serialize + prettify codegen paths reconcile their view-layer
conventions.

### Orchestration-level artefacts

- `44e5bf1e` `tools(worktree): seed script for gitignored
  corpora (data/)` — W2.1's agent reported 24 environmental
  failures (missing `data/{bbnf,css,json}`) because worktrees
  branch off HEAD but don't inherit gitignored resources.
  `scripts/seed-worktree.sh` symlinks `data/` into fresh
  worktrees; `README.md` §"Worktree isolation" now calls the
  seed step mandatory immediately after `git worktree add`.

### W2 close — master state

Commit sequence from AW-I HEAD `fb8dd225`:
- `817882a6` docs(AW-II) scope refinements
- `d102e007` W2.4 audit
- `c25e63a0` W2.3 SCC recompute
- `705d17f7` docs(AW-I) scope-reveal replan
- `44e5bf1e` worktree seed helper
- `8df45be4` W2.1 AltLinear
- `c4cd7aaf` W2.1 Repeat
- `97285f60` W2.1 ShuntingYard
- `02574ce9` W2.5 delete fossils
- `404a5232` W2.5 tape_golden regen
- `a03cef55` W2.5 ir/passes pin predicates
- `d366fe42` W2.5 tape_golden + TS bench finalise
- `56be4534` W2.5 payload_layouts thresholds
- `f0de05e3` W2.5 test reconciliation

parse-that master advanced to `907db32` (W2.2, out-of-tree).

Workspace: **1078 passed / 0 failed / 68 ignored** (baseline
1101/0/67, delta −22 DELETE, +1 Category A ignore). All W2
sub-phase hard gates met. No deferrals. Master clean. Next:
W3 parse() swap on a single serial agent; the plan's declared
intentional-unworkability window opens at W3.1 and closes at
W4.

## 2026-04-16 — AW-I.W3 parse() swap + intentional unworkability opens

Single-agent serial wave. Commits `410cfa97 fdd3e932 90d91cb1`.
- `parse()` body rewritten to dispatch through `dta_run_into`.
  `parse_dta` retired; `DtaDfaScanner` promoted to module-level
  `const DTA_SCANNER`; `#( #rule_functions )*` expansion dropped.
- `generated.rs`: 27522 → 20432 lines (−25.8%).
- Hard gates: 8 ✓ (0 `fn __` outside prettify), 9 ✗ (target
  ≤12000, observed 20432 — plan miscalibration documented in
  `docs/tranches/AW/audit/w3-unworkable-surface.md` §"Line-count
  miscalibration"), 10 ✓.
- Workspace: 0/0/0 (gorgeous derive-panic cascade — 6 subgrammars
  fail proc-macro expansion because DTA walker coverage doesn't
  match bbnf grammar surface at W3.2 table state).

Audit: `docs/tranches/AW/audit/w3-unworkable-surface.md` (W3.2)
enumerates Tier A (compiles) / Tier B (derive-panic) / Tier C
(cascade) / Tier D (blocked) test surface and documents the
bootstrap idempotency regression — a second `scripts/bootstrap-
bbnf.sh` run would collapse to 23 lines because the committed
DTA-path `BbnfBootstrap::parse` can't parse bbnf.bbnf.

## 2026-04-16 — AW-I.W4 six-sub-wave execution + scope-pivot to AZ

The largest wave of AW-I. Ten agents across six sub-waves; ~4600
lines deleted; two genuine scope-pivots handled; one tranche
successor opened.

### W4α — Preparation

Single serial agent (`a2f168c0f5be436b6`). Two commits:
`ef840a35` gut `emit_rule_function_impl` + `emit_tape_tier_rule`
(708 lines deleted in grammar.rs); `bfd9777b` drop `scc_id.is_none()`
guards → cyclic-rule fuse/inline active.

### W4β — 4-parallel sibling-module deletion

Four agents on disjoint file bounds:
- **W4β.1** (`a454935c447e3fe2f`) — alt.rs (807) + tape_prelude.rs
  (956) + `emit_alt_*` + `emit_key_dispatch` trait impls.
  Commits `78c3f1d4 a0e19480`.
- **W4β.2** (`ad32ba67befffe07a`) — leaves.rs (374) + map_value.rs
  (526) + 11 trait impls. Commits `6695e247 39833ba0`.
- **W4β.3** (`a7d1cbe3ef0b1fe8d`) — seq.rs (70) + repeat.rs (237) +
  binary.rs (156) + operator_chain.rs (36) + 11 trait impls.
  Commits `d388f43c 6db15905 6b617f50 18738cfb`.
- **W4β.4** (`afa87991370fb0dc1`) — dispatch.rs (124) + ws.rs (69) +
  string_decode.rs (179) + 4 trait impls. Commits `dd63c26e
  e209dbec 9ddee355`.

W4β.1 + W4β.2 cherry-picked cleanly. W4β.3 + W4β.4 cherry-pick hit
3-way merge conflict on `mod.rs` line shifts (expected: each agent
computed patches against `bfd9777b`, but master had advanced).
Orchestrator resolved by direct consolidation on master (`47496993`):
deleted the 7 remaining sibling files + removed their `mod`
declarations + deleted the 15 corresponding trait-method impls +
pruned dead imports (`AltBranchInfo`, `KeyDispatchBranch`,
`KeyDispatchConfig`, `DelimScanConfig`, `FlattenStrategy`,
`SepByConfig`, `SeqChildGroup`, `TokenDispatchArmCompiled`,
`ValuePlacement`, `MapExpr`). Net ~3840 lines removed across W4β.

### W4γ — First close attempt + substrate extension

Single serial agent (`abd3c10f88d30bd1a`). Four commits:
- `840d832c` — `refactor(emitter): Default Output + no-op parse
  emit defaults`. Option B: `type Output: Default` bound added to
  `Emitter` trait + 27 parse-emit methods gained
  `Self::Output::default()` defaults. TS + WASM backends keep
  overriding; Rust inherits defaults. `TsCode` gets `Default`
  derive. Concern 2: `shared_json_string_decode_scanner` +
  `SharedScanner::JsonStringDecode` deleted (unreferenced after
  W4β deleted `emit_decode_call`).
- `a07a84aa` — `feat(dta): DTA entry rule + WsTrim state wiring`.
  Walker + lifter + emitter extension. `DtaTable::entry:
  DtaRuleId` threads `ir.entry` through; `DtaState::WsTrim {
  pattern }` variant added; lifter lowers `OptionalWhitespace
  (inner)` to `Seq([WsTrim, inner, WsTrim])`; walker scans `@ws`
  regex or falls back to ASCII whitespace.
- `aadb5a19` — `chore(generated): add entry field to DTA_TABLE
  const`. Hand-patch for the W3.2-era generated.rs whose struct
  literal was missing the new `entry` field.
- `0e03b830` — `docs(AW-I/audit): W4 close — emitter deletion +
  activation`. Audit document.

**Bootstrap did not close** at W4γ: a third barrier surfaced —
nested Alt backtracking through Repeat/Alt frames failed on
paren-expression rule bodies. Reproducer: `a = ( "x" ) ;` fails
while `a = "x" ;` succeeds.

### W4δ — Walker savepoint + multi-bug fix

Single serial agent (`a4522a62e0b2a2532`). Three commits
(`1a73a154 31de7e3c 0ecb0d30`). Four root causes diagnosed and
fixed:

1. `DtaState::Ref { target: DtaStateId::NONE }` errored instead of
   binary-searching `rule_entries` via `rule_entry_for`. Eleven
   forward-referenced rules in bbnf.bbnf surfaced as Syntax
   failures.
2. `FrameStack.counters` + `iter_savepoints` never shrunk on
   happy-path pops; ~250 Repeat frames over a bbnf.bbnf parse
   exhausted the `u8` counter index space.
3. `advance_or_pop_with`'s Repeat iteration-refresh preserved
   iter-1's `psi_len` across subsequent iterations; late-iteration
   failure truncated PSI past committed iterations' writes.
4. Stage-C `finalise` (post-order-derived) clobbered pre-order
   walker tapes' inline `close_compound` writes. Gated Step 1 on
   `child_off == NONE`.

Fix: `dispatch_one`'s Ref arm falls back to `table.rule_entry_for`.
New `pop_and_release` helper truncates `counters` +
`iter_savepoints` at every pop site. `advance_or_pop_with` threads
`&mut PayloadStream` so the Repeat refresh captures `psi.len()`.
Finaliser Step 1 now only writes when walker left `child_off == NONE`.

### W4ε — walk_tape peel + ws fallback

Single serial agent (`a03edf201810431eb`). Two commits
(`b0d68a4d 95c5d790`):
- `walk_tape` peels `Seq([WsTrim, X, WsTrim])` iteration wrappers
  inside Repeat bodies (Path B; deferred Path A lifter flattening
  to future tranche).
- Driver injects ASCII ws-trim at every dispatch when the DTA
  table carries no `WsTrim` states (bootstrap-survival for the
  W3.2-era committed table).

Bootstrap still failed: the 2026-04-16 W4ε close reproduced the
W4γ-identified offset-72 `@import` literal failure. Diagnosis
speculated `variant_idx`/`meta_idx` stamping incompatibility
between walker's Alt-closure branch_idx and consumer-expected
rule_id.

### W4ζ — Recovery + lowering scope-reveal

Single serial agent (`adbd33ec2186d0dfe`). Seven commits:
- `87f65214` — `chore(generated): transient entry field patch for
  pre-W3 regen`. Swapped pre-W3 fn-per-rule generated.rs back in
  + patched `entry` field to unblock `cargo check -p bbnf --lib`.
- `49656fd4` — `chore(generated): one-shot regen under post-W4
  emitter/walker/lifter`. Ran `scripts/bootstrap-bbnf.sh` with
  the transient fn-per-rule parser providing `BbnfBootstrap::parse`;
  emitted a 21198-line DTA-based generated.rs under the current
  emitter + walker + lifter state.
- `cba6339a` — `fix(bbnf-tape): stamp rule-entry variant_idx via
  pending_variant_idx` — **root-cause fix for walker tape
  identity**. `Frame::variant_idx: u8` + `FrameStack::
  pending_variant_idx: u8` added. `DtaState::Ref { rule, .. }`
  writes `rule.0 as u8` into pending; next compound frame push
  consumes it into `frame.variant_idx`. Leaf states consume at
  `emit_leaf`. `close_compound` stamps the low-6 `flags` bits
  from `frame.variant_idx` first, falling through to the
  existing Alt-cursor branch-index stamping only when no rule
  context captured. Backtracking paths preserve correctness
  (AltLinear snapshots pending; iter-savepoint restore clears it).
- `c00ed4bf` — `fix(grammar/host): descendant-based decoders for
  DTA structural nesting`. Directive decoders migrated from
  direct-child to descendant-based lookups.
- `ba9e14a8` — `fix(lower/expression): extend wrapper detection
  for DTA tape shapes`. Partial — outer expression shape unblocked;
  systematic lowering migration scope-revealed to AZ.
- `e784a648` — `chore(bootstrap/debug_parse): dump imports and
  pretties`. Debug harness augmentation.
- `da39ad60` — `docs(AW-I/audit): W4 scope-reveal + AZ seed`.
  Audit + successor-tranche seed.

**W4ζ scope-reveal taken**. Per TRANCHE_SPEC §"Scope-reveal
protocol" item 4: "Mid-tranche plan pivots open a new letter."
The lowering-pipeline migration (every `find_child_by_kind` call
site in `lower/**`, `graph/**`, `types.rs`) is a multi-wave
consumer audit orthogonal to AW-I's DTA activation thesis. AZ
opens to carry it.

### AW-I tranche close

**Workspace state**: `bbnf` + `bbnf-tape` + `bbnf-ir` + `bbnf-ser` +
`egraph` + `csp-solver` + dependencies compile cleanly
(`cargo check -p bbnf --lib` exits 0); gorgeous subgrammar dev-deps
derive-panic until AZ's lowering migration lands. Workspace test
suite cannot run end-to-end; isolated-crate tests pass:
- `bbnf-tape`: 11 walker_arms passing (7 W2.1 + 1 W4δ paren + 3
  counter-slot/PSI-refresh additions).
- `bbnf-ir`: 261 passing, 3 ignored.
- Combined isolated core subset: ~460 passed, 0 failed, ~9 ignored.

**Per-phase landing count**: every W0-W4 sub-phase has commit
citations in FINAL-I.md. Hard gates 1-11 met; gates 9 reclassified
as plan-miscalibration (documented); gates 12-13 deferred to AZ
close with named destination per escape-clause extension.

**Architectural gains preserved on master**:
- DTA-only `parse()` dispatch (W3).
- 11 emitter sibling modules deleted + their trait impls (W4β).
- `emit_rule_function_impl` gutted + `emit_tape_tier_rule` deleted
  (W4α).
- Cyclic-rule fuse/inline activation (W4α).
- Walker: AltLinear savepoint, Repeat lo..=hi, ShuntingYard
  reducer, Ref resolution, counter slot release, PSI refresh,
  pre-order finalise gating, `variant_idx` stamping (W2.1 +
  W4δ + W4ζ).
- Lifter: `DtaState::WsTrim` + invisible-structural peeling (W4γ,
  W4ε).
- Emitter trait: `Self::Output: Default` bound + 27 parse-emit
  defaults (W4γ).
- Pin predicates in fuse/inline: `body_has_map` +
  `is_consumer_pinned` (W2.5).

**FINAL-I.md** composed at `docs/tranches/AW/FINAL-I.md` with
full hard-gate attribution + cross-tranche debt ledger.

**AZ.md** composed at `docs/tranches/AZ/AZ.md`: five waves
migrating the lowering pipeline's tape-shape assumptions. Budget
~4-5 waves of ~1k-line changes each, predominantly
`crates/core/src/lower/**`. No further bbnf-tape driver changes
anticipated.

`post-AW-I.json` bench matrix NOT produced per escape-clause
extension; will compose at AZ close as `post-AW.json` multi-wave
history rooted at AW-I's W2 baseline and AZ-W1 through AZ-W5
measurements.

### AW-I HEAD

Commit range `fb8dd225` → `da39ad60` covers 41 commits. Tranche
closes with honest attribution.

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
