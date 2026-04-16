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

### Wave 0β — W0 cleanup dispatch

Five parallel sub-agents in sibling worktrees, disjoint file
bounds:

- **W0a — `bbnf-tape` internals** (AW.0.1, AW.0.2).
- **W0b — emitter cleanup + IR transform + white-colour
  WideScalar** (AW.0.3, AW.0.4, AW.0.8, AW.0.10 — fold of
  plan's (b) and (e)'s white-colour item to keep emitter
  writes single-owner).
- **W0c — layout admission + Color view** (AW.0.5).
- **W0d — inline-test migration** (AW.0.6, six gorgeous
  source files, not one).
- **W0e — bootstrap CI gate + profile ledger** (AW.0.7,
  AW.0.9).

Bootstrap regen runs once post-cherry-pick on master, owned
by the orchestrator. No agent regens.

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
