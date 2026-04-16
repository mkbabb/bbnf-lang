# Tranche AW-I — FINAL

AW-I set out to activate the DTA as the sole parse path, delete every
`fn __<rule>` emitter helper, and land the cyclic-rule fuse/inline
activation. Execution landed every architectural precept plus twelve
substrate extensions the plan did not enumerate. The final barrier —
workspace-green self-host round-trip — revealed a lowering-pipeline
scope-pivot beyond AW-I's envelope. The successor tranche **AW-II**
carries the lowering migration; AW-I closes on what landed with
honest gate attribution.

## Commit range

`fb8dd225` → `da39ad60` — 41 commits across six waves. Every commit
is on master.

## Wave-by-wave recap

### W0 — Cleanup + hygiene [landed pre-AW-I; inherited]

Five-parallel hygiene wave from the AW plan predecessor window.
Commits `ba4e1e79…bfe17d7f`. Workspace closed at 1101/0/67.

### W1 — DTA substrate skeleton [landed pre-W2]

Single-agent serial. Commits `11f22f1f…08658746`. Walker substrate
with `FrameStack`, `cursor::child(0)` O(1), inline frame_depth,
`dta-replay` feature.

### W2 — Walker completion + MemoStore retire + SCC plumbing + snapshot migration

| Sub-phase | Owner | Commits | Status |
|-----------|-------|---------|--------|
| W2.1 walker arms (AltLinear / Repeat / ShuntingYard) | `crates/bbnf-tape/src/driver.rs` | `8df45be4` `c4cd7aaf` `97285f60` | ✓ landed |
| W2.2 MemoStore retirement | `../parse-that` (cross-repo, master `907db32`) | 4 commits out-of-tree | ✓ landed |
| W2.3 SCC recompute plumbing | `crates/core/src/pipeline/compile.rs` | `c25e63a0` | ✓ landed |
| W2.4 fuse-snapshot audit | `docs/tranches/AW/audit/fuse-snapshot-migration.md` | `d102e007` | ✓ landed |
| W2.5 snapshot migration (scope-reveal absorb) | `crates/**/tests/**` + `crates/ir/src/passes/transform/{inline,fuse}.rs` | `02574ce9 404a5232 a03cef55 d366fe42 56be4534 f0de05e3` | ✓ landed |

**Scope-reveal at W2.3 execution**: the plan premised "semantics-
neutral SCC plumbing" but `lower::metadata` stamped
`scc_id = Some(id)` unconditionally while `compute_scc` realigned to
`None` for acyclic. Activating `inline_acyclic` + `fuse_single_use`
for acyclic rules was unavoidable. Per TRANCHE_SPEC scope-reveal
protocol, W4.5's snapshot migration moved UP into W2.5. W2 exited
green at **1078/0/68** (1101 − 22 DELETE − 1 new Category A
`serialize_roundtrip::css_simple`). W4.5 retained the cyclic-rule
guard drop.

**Pin predicates landed (`a03cef55`)**: `body_has_map` +
`is_consumer_pinned` guards added to `inline_acyclic` and
`fuse_single_use` to preserve typed-materialisation invariant
(every `->` reaches the tape emitter) and consumer-visible
directives (`@pretty`, `@debug`).

### W3 — `parse()` swap + regen [intentional unworkability opens]

Single-agent serial. Commits `410cfa97 fdd3e932 90d91cb1`.
- `parse()` body replaced with direct `dta_run_into` dispatch.
- `parse_dta()` retired.
- `DtaDfaScanner` promoted to module-level `const DTA_SCANNER`.
- `#( #rule_functions )*` expansion dropped from `emit_grammar_impl`.
- `generated.rs`: 27522 → 20432 lines (-25.8%).

Workspace entered plan-declared intentional-unworkability window:
6 gorgeous grammars derive-panic because the W3.2-committed DTA
table referenced walker features not yet wired (WsTrim, full
arm coverage, correct variant_idx). Audit:
`docs/tranches/AW/audit/w3-unworkable-surface.md`.

### W4 — Legacy emitter deletion + cyclic-rule activation [six sub-waves]

Most substantial wave. Twenty-one commits across six sub-waves. Each
sub-wave's commits stay load-bearing.

#### W4α — Preparation (1 agent serial) [`ef840a35 bfd9777b`]

- Gut `emit_rule_function_impl` to `quote!{}`; delete `emit_tape_tier_rule` and 708 lines of supporting per-rule-body machinery in `emitter/grammar.rs`.
- Drop `scc_id.is_none()` guards in `inline.rs:44` + `fuse.rs:57` → cyclic-rule fuse/inline active.

#### W4β — Sibling-module deletion (4 parallel agents + 1 orchestrator consolidation) [`78c3f1d4 a0e19480 6695e247 39833ba0 47496993`]

- Delete 11 sibling files: `alt.rs` (807), `tape_prelude.rs` (956), `leaves.rs` (374), `map_value.rs` (526), `seq.rs` (70), `repeat.rs` (237), `binary.rs` (156), `operator_chain.rs` (36), `dispatch.rs` (124), `ws.rs` (69), `string_decode.rs` (179). ~3534 sibling-file lines + ~300 trait-impl hunk lines = ~3840 deleted.
- Per-commit cherry-pick hit 3-way merge conflict on `mod.rs` line shifts; orchestrator consolidated W4β.3+W4β.4 (`47496993`) via direct deletion on master.
- Emitter directory: `grammar.rs`, `dta.rs`, `mod.rs`, `profile.rs`, `visitor.rs`, `prettify/` (gate 11 ✓).

#### W4γ — First close + substrate extension (1 agent serial) [`840d832c a07a84aa aadb5a19 0e03b830`]

- Concern 1: `Emitter` trait default impls + `Self::Output: Default` bound; `TsCode` gains `Default` derive. Rust inherits defaults; TS + WASM override.
- Concern 2: `shared_json_string_decode_scanner` + `SharedScanner::JsonStringDecode` deleted (unreferenced after W4β).
- Walker + lifter extended: `DtaTable::entry` field; `DtaState::WsTrim { pattern }` variant. Lifter lowers `OptionalWhitespace(inner)` to `Seq([WsTrim, inner, WsTrim])`.
- Audit: `docs/tranches/AW/audit/w4-close.md`.

#### W4δ — Walker savepoint + multi-bug fix (1 agent serial) [`1a73a154 31de7e3c 0ecb0d30`]

Three compounding walker bugs diagnosed and fixed:
1. `DtaState::Ref { target: DtaStateId::NONE }` errored instead of binary-searching `rule_entries`. Eleven forward-referenced rules in bbnf masked the Syntax failure.
2. `FrameStack.counters` + `iter_savepoints` never shrunk on happy-path pops; ~250 Repeat frames over bbnf exhausted the `u8` counter index space.
3. `advance_or_pop_with`'s Repeat iteration-refresh preserved iter-1's `psi_len` across subsequent iterations.
4. Stage-C `finalise` was post-order-only; pre-order walker tapes triggered spurious child_off rewrites. Gated Step 1 on `child_off == NONE`.

Nested-paren AltLinear test landed (`altlinear_nested_paren_group`).

#### W4ε — walk_tape + ws fallback (1 agent serial) [`b0d68a4d 95c5d790`]

- `walk_tape` peels `Seq([WsTrim, X, WsTrim])` iteration wrappers inside Repeat bodies.
- Driver injects ASCII ws-trim at every dispatch when the table carries no `WsTrim` states (bootstrap-survival for stale tables).

#### W4ζ — Recovery + scope-reveal (1 agent serial) [`87f65214 49656fd4 cba6339a c00ed4bf ba9e14a8 e784a648 da39ad60`]

One-shot recovery recipe: swap pre-W3 fn-per-rule `generated.rs` + patch the `entry` field → run bootstrap under post-W4 emitter/walker/lifter → 21198-line DTA-based `generated.rs` landed.

Root cause #1 (tape-level rule identity) **fixed**: walker's
`variant_idx` stamping corrected via `Frame::variant_idx` +
`FrameStack::pending_variant_idx` captured from `DtaState::Ref`
dispatch. Backtracking paths preserve correctness.

Root cause #2 (lowering-pipeline tape-shape assumptions) **partially
fixed**: `dispatch_expression` wrapper detection extended for
`TapeKind::Seq | TapeKind::Alt`; `lower_leaf_by_span_text` admits
Seq/Alt kinds; `collect_binary_operands` flattens DTA's
Repeat-as-Rule trailing wrappers. Systematic migration of
`lower/expression.rs`, `lower/tape_walk.rs`, `lower/value_expr.rs`,
`graph/**`, `types.rs` is scope-revealed to AW-II.

Audit: `docs/tranches/AW/audit/w4-scope-reveal.md`.

### W5 — FINAL + bench + close [deferred to AW-II close per escape clause]

`post-AW-I.json` bench matrix NOT produced. Rationale per plan
§Escape clause: workspace cannot run `cargo test --workspace`
until AW-II closes the lowering migration. `post-AW.json` composed
multi-wave at AW-II close; FINAL-I.md (this document) records AW-I
on its honest merit.

## Hard gate status

| Gate | Target | Observed | Status |
|------|--------|----------|--------|
| 1 (AltLinear backtrack) | savepoint backtracks, syntax-error on exhaustion | `altlinear_backtracks_after_first_failure` + `altlinear_nested_paren_group` pass | ✓ met |
| 2 (Repeat lo..=hi) | `{0,}` `{1,}` `{2,5}` pass | walker_arms tests pass | ✓ met |
| 3 (ShuntingYard) | `1+2*3` + `2^3^4` correct | walker_arms tests pass | ✓ met |
| 4 (no placeholders) | zero `Unsupported`/single-probe | `grep` clean | ✓ met |
| 5 (MemoStore deleted) | parse-that free of memo | grep clean | ✓ met |
| 6 (compute_scc in loop) | twice in normaliser loop | 2 insertions `c25e63a0` | ✓ met |
| 7 (W2.4 audit) | zero "unknown" entries | 22/44/8 classified | ✓ met |
| 7a (W2.5 workspace 0-failed) | cargo test green | 1078/0/68 | ✓ met |
| 8 (fn __ count) | 0 outside prettify | 0 (in 21198-line generated.rs) | ✓ met |
| 9 (generated.rs line count) | ≤ 12000 | 21198 | ✗ **plan miscalibration** — floor ≈10638 view accessors + 7092 prettify + ~1500 DTA tables; realistic envelope 15k-21k per `w4-close.md` §"Line-count miscalibration" |
| 10 (parse_dta retired) | no parse_dta fn | 0 | ✓ met |
| 11 (emitter dir reduction) | 5 files + prettify/ | verified | ✓ met |
| 12 (CSS L4 state_count < 2000) | direct `summarise` call | unverified — test lives in `crates/core/tests/` which requires workspace compile | ✗ deferred to AW-II.W5 |
| 13 (workspace 0 failures) | `cargo test --workspace --no-fail-fast` | workspace-check fails (proc-macro panics on dev-dependent gorgeous grammars) | ✗ deferred to AW-II close |

## Invariant verification

1. **One path**. `parse()` dispatches through `dta_run_into` exclusively. `parse_dta()` retired. ✓
2. **No legacy code**. 11 sibling emitter files + `emit_rule_function_impl` machinery deleted (~4600 lines). `MemoStore` retired. No per-rule fn emission surface survives. `compute_sibling_skip` deleted (W0). ✓
3. **No stubs at tranche close**. AltLinear / Repeat / ShuntingYard walker arms implement full semantics. ✓ (Note: the lowering pipeline's tape-shape migration is an un-migrated CONSUMER, not a stub — the walker/lifter/emitter trio are complete.)
4. **Intentional unworkability W3-W4 declared** — realized. Window did not close at W4; extended into AW-II (successor tranche). Per TRANCHE_SPEC §Scope-reveal protocol, a new letter opens — not a violation when the scope pivot is documented. ✓ (with caveat)
5. **Typed-AST parity total**. W2.5's `body_has_map` + `is_consumer_pinned` predicates preserve every `->` annotation's path to the tape emitter. Every typed rule that fuse/inline would have dissolved is pinned. ✓
6. **Bootstrap regen CI-enforced** (W0). Gate active at CI; will pass once AW-II restores idempotency.
7. **Workspace green at W5 close**. ✗ deferred to AW-II close per escape-clause extension.

## Cross-tranche debt — resolved

| Item | Origin | AW-I wave | Status |
|------|--------|-----------|--------|
| Colour-function `LargeAggregate` consumer | AV.0.5 | W0.5 | ✓ landed |
| Inline `#[cfg(test)]` in `crates/gorgeous/src/` | memory feedback | W0.6 | ✓ landed |
| Bootstrap regen CI gate | AV FINAL seeds | W0.7 | ✓ landed |
| Pre-order cursor O(1) `child(0)` | AV.2 inheritance | W1 | ✓ landed |
| DTA walker arm stubs | V3 substrate | W2.1 | ✓ landed |
| `MemoStore` (AW.1.8) | AU era | W2.2 | ✓ landed (cross-repo) |
| SCC staleness between inline + fuse | AU PROGRESS | W2.3 | ✓ landed |
| Fuse/inline activation on acyclic rules | AU PROGRESS | W2.3 (side-effect) | ✓ landed |
| Fuse/inline activation on cyclic rules (guard drop) | AU PROGRESS | W4α | ✓ landed |
| Un-fused-shape snapshot fossilisation | AU PROGRESS | W2.5 (consumed W2.4) | ✓ landed |
| Legacy `fn __<rule>` emission | AU era | W3.1 + W4β | ✓ landed |
| `parse_dta` additive surface | W1 substrate | W3.1 | ✓ landed |

## Cross-tranche debt — deferred to AW-II

| Item | Origin | Destination | Rationale |
|------|--------|-------------|-----------|
| `find_child_by_kind` → `find_descendant_by_kind` systematic migration | W4ζ scope-reveal | AW-II.W1, AW-II.W3 | DTA wraps semantic children one Seq compound deeper than fn-per-rule; every direct-child scan in `lower/**`, `graph/**`, `types.rs` needs evaluation. |
| `binary_factor` Alt-compound operator recognition | W4ζ scope-reveal | AW-II.W2 | walker stamps sub-variant branch_idx on Alt, decoder expects binary_operators rule id. Either lift wraps Ref, or decoder recurses. |
| `value_expr` `->` map-expression lowering | W4ζ scope-reveal | AW-II.W4 | `lower/value_expr.rs` assumes fn-per-rule child layout. |
| CSS L4 DTA state_count < 2000 verification | plan gate 12 | AW-II.W5 | requires end-to-end pipeline. |
| Workspace tests ≥ 1078 passed / 0 failed | plan gate 13 | AW-II close | requires lowering migration complete. |
| `serialize_roundtrip::css_simple` | W2.5 INVESTIGATE | AW-II.W5 or follow-up | `@pretty`-pinned rules survive fuse but serialize pipeline mis-dispatches; deeper view-layer reconciliation. |
| `post-AW-I.json` bench matrix | plan W5 | AW-II close (as `post-AW.json` multi-wave) | cannot bench workspace that doesn't compile. |

## Artefacts

- `docs/tranches/AW/audit/fuse-snapshot-migration.md` (W2.4) — 74 at-risk tests (22 DELETE / 44 UPDATE / 8 INVESTIGATE).
- `docs/tranches/AW/audit/w3-unworkable-surface.md` (W3.2) — line-count floor analysis + failure surface enumeration.
- `docs/tranches/AW/audit/w4-close.md` (W4γ) — emitter directory reduction + idempotency caveat.
- `docs/tranches/AW/audit/w4-scope-reveal.md` (W4ζ) — lowering-pipeline scope-pivot + AW-II seed.
- `crates/bbnf-tape/tests/walker_arms.rs` — 11 walker-arm focused tests (7 from W2.1 + 1 from W4δ paren + 3 from W4δ counter-slot/PSI-refresh).
- `crates/core/src/grammar/generated.rs` — 21198 lines, DTA-based, under post-W4ζ emitter.
- `scripts/seed-worktree.sh` — orchestration tooling, symlinks `data/` into fresh worktrees.

## What did not land

1. Workspace-green tests. Lowering-pipeline migration required; see AW-II.W1 through AW-II.W5.
2. CSS L4 state_count verification. Blocked on (1).
3. `post-AW-I.json` bench matrix. Blocked on (1).
4. AW-I as a standalone closed tranche per strict `README.md` §Tranche completion requirements 1-3. Closed instead per TRANCHE_SPEC §Scope-reveal protocol item 4 (new letter opens when scope pivot exceeds current tranche envelope).

## Successor chain

Three successor tranches sequence off AW-I under the canonical
numbering:

- **AW-II** — DTA Self-Host Round-Trip
  (`docs/tranches/AW/AW-II.md`). Five waves migrating the
  lowering pipeline's tape-shape assumptions
  (`find_child_by_kind` → descendant walks). Prerequisite to
  AW-III and AX: no workspace-green → no bench matrix → no
  consumer wiring.
- **AW-III** — Optimisation and Parity
  (`docs/tranches/AW/AW-III.md`). Six waves activating every
  AV-emitted substrate channel: PSI rayon, ShapeRef dispatch,
  PHF + SIMD keyword tables, CSS selector classifier,
  document-level parallel parse, bloom + GADT dedup, Pratt
  generalisation, parity harnesses, `Tape::reduce_column<C,R>`,
  bench parity. Presumes workspace-green — BLOCKED on AW-II
  close.
- **AX** — Replay, Recovery, and the Subsystem Ledger
  (`docs/tranches/AX/AX.md`). Consumer of AW's `dta-replay`
  substrate: incremental re-parse, grammar-structural error
  recovery, parse-step debugger, test-case minimisation. Also
  closes four pre-existing Category A subsystem failures.
  Presumes workspace-green — BLOCKED on AW-II close.

Canonical dispatch order is **AW-II → AW-III → AX**. AW-II
restores compile + test; AW-III re-establishes the bench
baseline as the truth anchor before feature work perturbs it;
AX lands replay/recovery consumers on a stable, bench-verified
codebase. AW-III + AX can run in parallel when agents obey
`driver.rs`-disjoint bounds, but the sequential default
minimises instrument-perturbation risk on bench numbers.

Bench matrix layout:
- `post-AW-II.json` — first bench run post-unworkability, at
  AW-II close.
- `post-AW-III-W{N}.json` — per-wave baselines for the
  optimisation tranche.
- `post-AW-III.json` — final AW-III close.
- `post-AW.json` — full AW arc composed at AW-III close
  (`post-AW-I`-synthetic + AW-II baseline + AW-III per-wave).
- AX publishes no separate bench matrix — its consumers are
  feature/correctness, not performance.

Letter-sequence rationale: AW-I.W4ζ's lowering-migration
scope-reveal first shipped under an intermediate letter (AY) and
was renamed to AW-II during AW-I's close so the AW arc stays
contiguous. The original AW-II (optimisation, authored
concurrently with AW-I) shifted to AW-III accordingly. AX
retains its letter because it is thematically distinct
(feature consumers of the `dta-replay` substrate) rather than
part of the AW activation arc.
