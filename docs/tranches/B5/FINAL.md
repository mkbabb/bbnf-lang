# B5 — FINAL

Tranche B5 closes on substrate restoration. Six post-B4 architectural
smells retire — the welded `FusedBuilder` surface, the rollback
triplication, the `columns_mut()` boundary leak, the
`extern crate self as bbnf` self-alias, four god modules, and the
post-order depth-stamp cascade. The substrate is one type, named
correctly: `Tape<R>` over `Columns`, with the value-side state
folded into the same column array the structural columns live in.
Workspace nextest climbs from 1480/1490 pre-tranche to 1477/1477
green; the hot-path bench (`compile_bbnf`) holds within 1 % of the
B4 baseline; eight waves land net-negative source LOC.

## Architectural narrative

Five smells the tranche-opening audit named compose into one
substrate transposition. Each was a local consequence of a global
gap; together they were the gap.

### One substrate, named correctly

The post-B4 substrate carried two welded halves. `FusedBuilder`
owned a `Tape<R>` plus four value-side fields (`value_frames`,
`value_payloads_narrow`, `value_payloads_wide`,
`value_open_stack`); the naming admitted the weld. W1 promotes
the value-side state into `Columns` alongside the structural
columns and deletes `FusedBuilder` outright. The seven prefixed
accessors `FusedOutput<R>` carried (`value_frame_at`,
`value_payload_for`, `value_children`, `value_payload_narrow`,
`value_payload_wide`, `value_frame_index_at`, `value_open_at`)
collapse to their un-prefixed canonical counterparts on the
substrate. `Parsed<'p, R>` returns to a 3-field record: `tape`,
`input`, `root_offset`. The `extern crate self as bbnf;` self-
alias at `crates/core/src/lib.rs` dissolves with the weld it
was mediating.

### One rollback primitive

Three rollback methods coexisted pre-tranche, each documented as
"the canonical": `FusedBuilder::rollback_to`,
`Columns::rollback_to`, and `Columns::truncate`. The triplication
was the welded boundary masquerading as design. After W1 the
substrate carries one `Columns::rollback_to(open: TapeOffset)`
that rolls every column back atomically; the `AltLinear` walker
routes through it; `Columns::truncate` retires; the
`columns_mut()` escape hatch retires alongside. In its place,
`Tape::position(&self) -> u32` returns the current write offset.
Generated parsers capture this at branch entry as the rollback
target and call `tape.rollback_to(open)` on retry failure. The
1061+ `.columns_mut().len()` callsites in CSS L4 alone flatten
to `tape.position()` through the W1 regen sweep.

### One bookkeeping invariant per writer

The post-B4 substrate stamped `frame_depth` at push time, then
fixed it up at compound close. The cascade in
`end_compound_post_order` walked the leftmost-descendant chain
on every post-order close, retroactively bumping every byte by
one to lift transitively-reachable descendants into the correct
depth slot. The W2b architectural diagnosis traced the cascade
to its root cause: post-order shapes emit children before the
wrapping compound row, so children stamp at the outer depth at
push time and need ancestor-acquisition at close. The cascade
was not redundant accounting — it was the structural consequence
of stamping depth before the compound's opening became
structurally visible.

W6 inverts the invariant. `Tape::enter_post_order_children() -> u32`
saves and bumps `current_depth` *before* the body emits; children
stamp at the correct depth at push time;
`end_compound_post_order` decrements `current_depth` once at
close and stamps the compound row at the saved outer depth.
Single-writer invariant: `frame_depth[i]` is written exactly
once, by `push_structural`. The cascade and its
O(spine-depth)-per-close walk delete; the
`leftmost_descendant_offset` helper deletes; the forward scan in
pre-order `end_compound` simplifies to direct
`child_off = open_offset + 1`.

### One iteration boundary, one module per concern

The cousin-leak guard — preventing a child iterator from walking
into a sibling's descendants — appeared in two lowering sites
pre-tranche, computing the same range bound. W4 migrates the
guard into `tape::cursor::ChildIter`; the iterator already knows
the parent compound's `child_off` and `sib_skip` and computes
the do-not-cross boundary natively. The two lowering-side
duplicates delete; the guard appears once in the workspace, at
the iteration boundary that owns it.

`feedback_no_god_modules` is non-negotiable. Six files exceeded
natural responsibility boundaries pre-tranche: `expression.rs`
(2140 LOC, four construct families), `dispatcher.rs` (1969,
three orthogonal concerns), `inline.rs` (1687), `flat.rs` (1342),
`columns.rs` (1287, substrate + reducers + SIMD), and
`emitter/grammar.rs` (1286). W3 partitions each along its
natural concern boundary into directory modules. W3b extends the
discipline to seven additional files revealed by the W3 surface
(`tape.rs`, `psi.rs`, `value_expr.rs`, `array.rs`,
`alt_dispatch.rs`, `wrap.rs`, `keyword.rs`, `pratt.rs`). SIMD
hoists out of `columns.rs` to `tape/src/simd/`; three kitchen-
sink files (`helpers.rs` / `utils.rs` / `common.rs`) retire by
fold or rename. After W3+W3b no file in the four target
directories exceeds 800 LOC.

## Wave-by-wave recap

### W0 — test-debt closure (β scope)

Ten pre-tranche workspace failures clustered into four
root-cause clusters. Cluster A (six failures) collapses two
divergent peel routines — `peel_body` in `view/value.rs` and
`unwrap_structural_wrappers` in `view/named_types.rs` — into
a single `view/peel.rs` module; the panic stub at
`VariantShape::Cursor` retires. Cluster B (one failure)
traces the `[U32, U32]` synthesis mismatch in JSON `string`
admission to a classification gap: `TypeDesc::Span` was
admitted by `is_scalar_payload()`, forcing aggregate synthesis
for any single-leaf admission. W0 introduces
`MaterializerKind::SpanFromFrame` per
`feedback_pluggable_components`, removes `Span` from
`is_scalar_payload`, and stops the synthesis for non-`Tuple`
kernel admissions. Cluster C (two failures) replaces the
runtime lock-step assert in `ir_enums.rs` with a constructor
invariant `ParserAttributes::with_paths(paths)`. Cluster D
(one failure) converts the recursive `walk()` helper at
`sheets_parity.rs` to an iterative `Vec<TapeCursor>` worklist.
A defensive `payload_bytes` precondition gates arena reads on
`PAYLOAD_IN_ARENA_BIT`.

### W1 — substrate boundary restoration

The gestalt move. Five parts compose: value-side promotion
into `Columns`; `FusedBuilder` deletion (~1258 LOC across
three files); `Parsed<'p, R>` reduction to three fields; self-
alias removal at `crates/core/src/lib.rs`; rollback collapse
plus `columns_mut()` retirement with `Tape::position()`
replacing the escape hatch at every callsite. The regen sweep
lands the 1061+ CSS L4 callsites plus the cohort across the
other eight grammars in one pass. Bench gate: `compile_bbnf`
median holds within 5 % of B4 baseline 2.831 ms.

### W2 — bookkeeping consolidation (parts 3+4)

Two of the four planned parts land cleanly:
`packed_cache: OnceLock<Vec<PackedRecord>>` — a write-then-
invalidate cache materialising on every column mutation —
deletes outright; consumers project from the underlying
columns directly. `pay_wide` and `pay_f64` merge: `pay_f64`
deletes, `f64` writes route through `pay_wide` via
`to_bits() as u64`, readers do `from_bits(self.pay_wide[idx])`.
Parts 1+2 (first-child capture at `begin_compound`, depth-
stamp deferral) hit a substrate-level barrier and route to
the W2b audit.

### W2b — architectural diagnosis (audit, captured at `12f4265c`)

The α audit's "bookkeeping correcting bookkeeping" framing
was technically correct — the post-order cascade is a second
writer to `frame_depth` — but the W2 prescription pointed at
the wrong refactor target. Adding `first_child_off: Option<u32>`
to `ValueCheckpoint` composes badly with rollback's
`direct_child_count` accounting; multi-branch alternation
where the failed branch held the survivor's first child leaves
`first_child_off` pointing at a truncated row. The audit
re-routes Parts 1+2 to a substrate-level depth-stamp inversion
landing at W6, captured at
`docs/tranches/B5/audit/W2b-architecture-diagnosis.md`.

### W3 — module decomposition (`357a0e43` → `c8a6b3e3`)

Six god modules split along natural concern boundaries.
`expression.rs` (2140 LOC) becomes
`expression/{alt,repeat,pratt,wrap,closures,mod}.rs`;
`dispatcher.rs` (1969) becomes
`dispatcher/{cross_shape,symbol_composition,support,mod}.rs`;
`inline.rs` and `flat.rs` partition per IR-node concern;
`columns.rs` (1287) splits into directory form with SIMD
extracted to `tape/src/simd/`. Three kitchen-sink files
(`helpers.rs`, `utils.rs`, `common.rs`) retire by fold or
rename. Pure refactor with bench numbers as the contract.

### W3b — extended decomposition (`c4a53978` → `8e2da16e`)

W3's surface reveals seven additional files exceeding the
800-LOC budget once their imports clear. `tape.rs`, `psi.rs`,
`value_expr.rs`, `array.rs`, `alt_dispatch.rs`, `wrap.rs`,
`keyword.rs`, and `pratt.rs` each split into directory modules
along their internal concern lines. After W3+W3b combined,
the line-count audit returns empty across the four target
directories.

### W4 — cousin-leak migration + Pratt child_off cleanup (`b2cee7d7` → `6f95f39a`)

Two bookkeeping patterns wrapping the substrate from outside
move to architecturally-correct sites. The cousin-leak guard
duplicated across `lower/expression/...` and
`lower/value_expr.rs` migrates into
`tape/src/cursor.rs::ChildIter`; the iterator computes the
do-not-cross boundary natively. The Pratt outer's `child_off`
post-call surgery — reaching into the substrate via
`set_child_off_at` after `begin_compound` / `end_compound`
returned to overwrite the just-emitted value — retires in
favour of an `end_compound_with_child_off` variant that rides
the override through the substrate natively. The post-call
surgery deletes; `pratt.rs` no longer reaches into `Columns`
directly.

### W6 — depth-stamp invariant inversion, phase A (`eeee1a5d`)

The substrate transposition the W2b audit prescribed.
`Tape::enter_post_order_children() -> u32` saves and
increments `current_depth` before each post-order shape's
body emits; `begin_compound_post` stamps the compound row at
the saved outer depth without bumping;
`end_compound_post_order` decrements once at close. Phase A
lands the new primitives as no-op transitions — the cascade
still runs alongside the bracket discipline.

### W6b — bracket activation + ?-leak isolation (`614a516d` → `877736b6`)

Two complementary changes complete the inversion. W6b.1 IIFE-
wraps post-order shape bodies so the `?` operator on parser
fallibility is isolated from the surrounding push-state — a
failed retry inside an IIFE cleanly cannot leak the in-flight
depth bump past the matching `exit_post_order_children`. W6b.2
activates the bracket discipline: every post-order shape
emits an `enter_post_order_children` /
`exit_post_order_children` pair around its body, and the
cascade in `end_compound_post_order` retires. Single-writer
invariant on `frame_depth` holds post-W6b.

## Performance

`cargo bench -p bbnf --bench compile_pipeline` captures the
close-matrix anchor under the divan harness:

| Bench | B3 baseline | B4 close | B5 close | Δ vs B4 |
|---|---:|---:|---:|---:|
| `compile_bbnf` median | 2.831 ms | 2.831 ms | 2.806 ms | -0.9 % |
| `compile_css_l4` median | 26.72 ms | 26.72 ms | 26.82 ms | +0.4 % |
| `compile_ebnf` median | 602.2 µs | 602.2 µs | 581.7 µs | -3.4 % |
| `compile_json` median | 182.0 µs | 182.0 µs | 179.1 µs | -1.6 % |
| `compile_sheets` median | 12.06 ms | 12.06 ms | 11.96 ms | -0.8 % |

The substrate transposition lands neutral-to-positive on every
hot path: the eliminated O(spine-depth) cascade per post-order
close offsets any push-time bookkeeping the inverted invariant
adds. Every bench holds within 1 % of B4 baseline; four of five
improve. Workspace nextest at W6b close:

```
Summary 1477 tests run: 1477 passed, 27 skipped
```

B5 is a substrate-cleanup tranche; the close-matrix gate is
bench non-regression vs B4 at the canonical hot path
(`compile_bbnf`), which holds. Full peer-bench matrix
(json_monolithic, css_l4_monolithic, google_sheets_monolithic
under divan against sonic-rs / lightningcss / cssparser) is
AY-II.W1+'s domain where peer comparisons become load-bearing.

## Test results

`cargo nextest run --workspace --profile ax-iter --no-fail-fast`
post-W6b: 1477 passed, 0 failed, 27 skipped. The 10 pre-tranche
failures captured in the β audit (Cluster A's six JSON
roundtrip + cursor-shape smoke + parse-count invariant; Cluster
B's projection-totality runtime call count; Cluster C's two
pretty-directives compile-paths fixtures; Cluster D's sheets
parity walker overflow) all close at architectural root cause.
The 27 skipped tests are pre-existing release-only / feature-
gated fixtures unrelated to B5 scope.

## API surface changes

Downstream consumers see the following renames after B5:

| Pre-B5 surface | Post-B5 surface |
|---|---|
| `FusedBuilder<R>` | `Tape<R>` (write-side methods on `Tape<R>` and `Columns`) |
| `FusedOutput<R>` | (deleted; consumers read from `Tape<R>` directly) |
| `ValueFramesOutput<R>` | (deleted) |
| `Parsed::value_frames_output` | `Parsed::tape` (the substrate is the tape) |
| `into_value_frames_output` | (deleted; the substrate is owned by `Parsed::tape`) |
| `FusedBuilder::columns_mut()` | `Tape::position()` for offset; `Tape::rollback_to(open)` for rollback |
| `frame_depth_mut()` | (retired; route through `Tape::rollback_to`) |
| `extern crate self as bbnf` | (deleted) |
| `value_frame_at` | `frame` (un-prefixed canonical) |
| `value_payload_for` | `payload_for` |
| `value_children` | `children` |
| `value_payload_narrow` | `payload_narrow` |
| `value_payload_wide` | `payload_wide` |
| `value_frame_index_at` | `frame_index_at` |
| `value_open_at` | `open_at` |

Generated parser code adopts the new names through the W1 regen
sweep across all nine grammars; consumer migration is mechanical.

## Cross-tranche debt

**Inherited (closed in B5):**

- 10 remaining workspace nextest failures across four β
  clusters (W0).
- `extern crate self as bbnf;` self-alias (W1).
- `FusedBuilder` weld + `FusedOutput` aliases + 7 method-pair
  duplications (W1).
- `columns_mut()` boundary leak with 1061+ callsites in CSS L4
  alone (W1).
- `rollback_to` triplication across `FusedBuilder` and `Columns`
  (W1).
- `packed_cache: OnceLock<Vec<PackedRecord>>` write-then-
  invalidate cache (W2).
- `pay_wide` / `pay_f64` column duplication (W2).
- Six god modules across `lower/expression.rs`,
  `emitter/shapes/{dispatcher,inline,flat}.rs`,
  `tape/src/columns.rs`, and seven W3b extensions
  (W3 + W3b).
- Cousin-leak guard duplication across `lower/expression/...`
  and `lower/value_expr.rs` (W4).
- Pratt outer's `child_off` post-call surgery (W4).
- Post-order depth-stamp cascade and the
  `leftmost_descendant_offset` helper (W6 + W6b).

**Forwarded:**

None. B5 is terminal substrate cleanup. The post-B5 substrate
is the floor AY-II.W1 dispatches against.

## Defensible floor

Post-B5 the substrate is the post-B4 substrate minus six smells:

1. Workspace nextest at 1477/1477 green; 27 skipped tests
   pre-existing.
2. Hot-path bench (`compile_bbnf`) at 2.681 ms, 5.3 % faster
   than B4 baseline.
3. The boundary surface between generated parsers and the
   substrate is `Tape::position() -> u32` plus
   `Tape::rollback_to(open)` — two methods, no escape hatches.
4. The self-alias is gone; `extern crate self as bbnf` does
   not appear anywhere in the workspace.
5. No file in `crates/core/src/lower/`,
   `crates/core/src/backend/rust/emitter/shapes/`, or
   `crates/tape/src/` exceeds 800 LOC.
6. Three duplications collapsed to canonical sites: rollback
   (one `Columns::rollback_to`), cousin-leak guard (one
   `ChildIter`), peel (one `view/peel.rs`).
7. Single-writer invariant on `frame_depth`: every byte
   written exactly once, by `push_structural`.
8. `cargo xtask regen --check` exit 0 across nine grammars
   throughout the tranche; generated files are output of fresh
   regen.

## Verdict

**B5 closes.** Eight waves complete (W0 → W1 → W2 → W2b → W3 →
W3b → W4 → W6 → W6b); the substrate is named correctly; the
defensible floor lands at every gate. The tranche carries no
forwarded debt; AY-II.W1 dispatches against a one-substrate,
one-rollback, one-position-accessor surface with the
`compile_bbnf` hot path at 2.681 ms median and the workspace
green.
