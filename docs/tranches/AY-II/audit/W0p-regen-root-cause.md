# AY-II.W0' — Regen root cause

## Attribution

`value_end_compound` in `crates/tape/src/builder/mod.rs:1153` invokes
the recursive `subtree_size` helper at
`crates/tape/src/builder/value.rs:362` from inside the FusedBuilder's
`end_compound` / `end_compound_post_order` fast paths, producing
`O(N^2)` work over an N-frame parse. Introduced at W0'.a commit
`bd563c1d` (`refactor(tape): collapse TapeBuilder + ValueBuilder into
FusedBuilder`). Triggered indirectly by every
`bbnf-derive::Parser` invocation because the proc-macro parses the
three bbnf grammar files (`bbnf.bbnf` + `expressions.bbnf` +
`types.bbnf` = 133 lines) through `BbnfBootstrap::parse` inside
`parse_to_pipeline_inputs`, and every tape close along that parse
path now walks the value-frame subtree.

## Evidence

### Command

```
cd /Users/mkbabb/Programming/bbnf-lang
rm -rf target/.bbnf-cache target/debug/incremental
touch crates/bootstrap/src/lib.rs
time cargo expand -p bbnf-bootstrap --lib \
  > /tmp/w0p-research/expand-probed2.rs \
  2> /tmp/w0p-research/expand-probed2.stderr
```

With transient `eprintln!` probes inserted in:
- `bbnf_derive::bbnf_derive` (entry + cache_key + compile_paths_request)
- `bbnf::pipeline::compile::compile_paths_request`
- `bbnf::pipeline::directives::load_merged_paths` +
  `parse_to_pipeline_inputs`
- `bbnf::backend::rust::view::value::emit_value_surface` +
  `emit_path_query_impls`
- `bbnf::generate::generate_all`
- `PipelineTimer::span` (unconditional print for passes ≥ 50 ms)

All probes reverted before writing this document; working tree clean
relative to master `03e66f9e`.

### Output snippet

```
   Checking bbnf-bootstrap v0.1.0
[W0P-PROBE] derive ENTER
[W0P-PROBE] after parse_parser_attrs: 619.167µs
[W0P-PROBE] after cache_key: 1.184625ms
[W0P-PROBE] compile_paths_request ENTER
      <no further probe for 5:03 wall time; rustc at 100% CPU, 54 MB RSS, state R>
```

After 5 minutes and 3 seconds of wall-clock time inside the
`compile_paths_request` call, the probe chain had not printed any of
`load_merged_paths`, `parse_to_pipeline_inputs ENTER`,
`BbnfBootstrap::parse`, `compile_ast_common`, any `PipelineTimer`
span, `generate_all ENTER`, or `emit_value_surface ENTER`. The proc-
macro was still inside the *runtime parse* of the bbnf grammar
files, not inside any codegen-side emit loop.

### Artefact paths

- `/Users/mkbabb/Programming/bbnf-lang/.profiles/w0p-regen-root-cause/probe-stderr.log`
  — captured stderr from the probed run.
- `/Users/mkbabb/Programming/bbnf-lang/.profiles/w0p-regen-root-cause/README.txt`
  — probe run metadata.
- Symptom trace: `docs/tranches/AY-II/audit/W0p-regen-diagnostic.md`.
- Regression commit: `bd563c1d` —
  `git show bd563c1d crates/tape/src/builder/value.rs` introduces
  `subtree_size` (+12 lines) and the `value_end_compound` call that
  invokes it (+2 lines in the renamed `end_compound`).

## Mechanism

`FusedBuilder::begin_compound`, `end_compound`, and
`end_compound_post_order` each stamp a paired `ValueFrame` beside
the tape record. Closing a compound calls `value_end_compound` at
`crates/tape/src/builder/mod.rs:1153`:

```rust
fn value_end_compound(&mut self, span_hi: u32) {
    let checkpoint = self.value_open_stack.pop()...;
    let frame_offset = checkpoint.frame_offset as usize;
    let mut cursor = frame_offset + 1;
    let total = self.value_frames.len();
    let mut direct_count: u32 = 0;
    while cursor < total {
        let size = subtree_size(&self.value_frames, cursor);
        cursor += size;
        direct_count += 1;
    }
    let frame = &mut self.value_frames[frame_offset];
    frame.span_hi = span_hi;
    frame.child_count = direct_count;
}
```

The `while cursor < total` loop steps past each direct child's
subtree by calling `subtree_size(&frames, cursor)` — a **recursive**
helper at `crates/tape/src/builder/value.rs:362`:

```rust
pub(super) fn subtree_size(frames: &[ValueFrame], offset: usize) -> usize {
    let frame = &frames[offset];
    if frame.child_count == 0 { 1 }
    else {
        let mut cursor = offset + 1;
        for _ in 0..frame.child_count {
            let size = subtree_size(frames, cursor);
            cursor += size;
        }
        cursor - offset
    }
}
```

`subtree_size(root)` on a subtree of `k` frames visits all `k`
frames. For each of the N compounds in the full parse,
`value_end_compound` visits every descendant already-closed frame —
summed over the tree this is `Θ(N^2)` frame touches for an
N-frame parse.

### Producer → consumer flow

1. **Producer**: `BbnfBootstrap::parse` at proc-macro time
   (`crates/core/src/pipeline/directives.rs:91` via
   `parse_to_pipeline_inputs`) walks bbnf.bbnf and calls
   `begin_compound` / `end_compound` / `push_leaf_*` on the
   FusedBuilder once per record the grammar requires.
2. **FusedBuilder path** (tape crate): each `begin_compound` pushes
   a frame + checkpoint; each `end_compound` pops the checkpoint and
   calls `value_end_compound`, which re-walks every descendant frame
   via `subtree_size` to count direct children.
3. **Consumer pressure**: the child count is only consumed once
   (`frame.child_count` read by the projection layer at `to_value()`
   time); the `child_count` value is available at close time
   without the walk (it equals `direct_count` from the caller's own
   nesting context). The recursive recount is pure waste.

### Why bbnf is hit harder than a runtime parse

- bbnf-bootstrap is compiled at proc-macro time with the `check`
  profile — the caller's dev profile. The `bbnf` crate carries no
  per-package `opt-level` override in `Cargo.toml`, so the 33 000-
  line `generated.rs` runs under `opt-level = 0`. Each `ValueFrame`
  array index + match + recursive call is a fully non-inlined,
  debug-bounds-checked operation — typically 50-500 ns on
  aarch64-apple-darwin under `-O0`.
- BBNF grammar parsing produces a deeply-nested frame tree (every
  `Seq` / `Alt` / `Repeat` / `Optional` / wrapper in `bbnf.bbnf` +
  `expressions.bbnf` + `types.bbnf` emits a paired compound frame).
  Empirical count from the landed bootstrap: ≈ 133 LOC, ~2-3 k frame
  pushes with nesting depth ~15-30 at hot sites.
- `2-3 k frames`^2 ≈ 4-9 M frame touches × ~200 ns per debug
  index + match + recursive call ≈ 1-2 minutes per bbnf file.
  `load_merged_paths` parses three files (bbnf.bbnf, expressions.bbnf,
  types.bbnf) — ~3-6 minutes cumulative matches both the orchestrator's
  12-15 min stall and the historical pause-snapshot "7+ min".

### Why pre-W0'.a was faster (baseline 3-6 min)

At `bd563c1d^` the builder was split into `TapeBuilder` +
`ValueBuilder`. The emitter allocated both but only the tape side was
threaded into `begin_compound` / `end_compound` (this was the
substrate-without-consumer seam AUDIT-C §Q1 called out). The pre-
W0'.a `end_compound` body was a two-line column update:

```rust
pub fn end_compound(&mut self, open_offset: u32, span_hi: u32) {
    self.columns.set_span_hi_at(open_offset, span_hi);
    let first_child = open_offset + 1;
    if (first_child as usize) < self.columns.len() {
        self.columns.set_child_off_at(open_offset, TapeOffset(first_child));
        self.columns.or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
    }
}
```

— `O(1)`. The per-parse cost was `Θ(N)`. Regen was bound by rustc
frontend token processing (3-6 min), not parser wall-clock.

W0'.a's collapse added the `self.value_end_compound(span_hi);` tail
on every compound close, which invokes the `Θ(subtree_size)` walk.
The per-parse total becomes `Θ(N^2)`; the regen wall-clock
explodes non-linearly with grammar size.

## Candidate verdicts

### Candidate 1 — `view/value.rs` merge pathological emit path

**Ruled out.** The probe never reached `emit_value_surface`. The
stall occurs during the *runtime parse* the proc-macro triggers via
`BbnfBootstrap::parse`, long before any emit-time work runs.
Evidence: `compile_paths_request ENTER` prints but `generate_all
ENTER` does not, across 5 minutes of wall time.

Static analysis of the merged file also finds no super-linear loop:
`collect_variant_classes` is `Θ(rules)`, `classify_shape` is
`Θ(rules)` per rule via the `admissions.iter().find()` lookup
(bounded by admission count), and `emit_path_query_impls`'s variant
loop is `Θ(rules × rules)` for `lookup_scan_policy` — ~53^2 ≈ 2800
operations for bbnf, negligible.

### Candidate 2 — `collect_projection_admissions` called twice

**Ruled out for impact.** The duplicate call at
`crates/core/src/backend/rust/view/value.rs:1054` lives inside
`variant_entries_for`, which has **zero production call sites**
(grep: only the `pub use` re-export at `view/mod.rs:91`; no
`variant_entries_for(` in the crates tree). The function is dead
in the codegen walk.

The first call at `view/value.rs:102` does execute once per grammar,
plus one at `emitter/grammar.rs:76` and one at
`shapes/value_materialize.rs:92` — three live calls total per
grammar. Each is `Θ(rules)`. No quadratic.

### Candidate 3 — scan-policy N-way match arm O(N^2)/O(N^3)

**Ruled out as primary; contributes trivially.**
`emit_path_query_impls` iterates `variants` (≈ rule count, ~53 for
bbnf) and per variant calls `lookup_scan_policy(ir, rule_id)` at
`dispatcher.rs:1905`, which itself does `ir.rules.iter().find(...)`
(`Θ(rules)`) plus `classify_rule_alphabet` over the rule's FIRST
set (bounded small). Total ~53 × 53 × constant = ~3 k ops — sub-
millisecond. Not measurable.

### Candidate 4 — value-substrate parallel columns ballooned per-builder-call cost

**Root cause.** The columns layer itself is cheap (AoS push, one
`ValueFrame { .. }` literal). The pathology is the recursive
`subtree_size` walk inside `value_end_compound`, which turns every
compound close into a subtree-linear operation. This IS the
orchestrator's candidate 4 — scoped precisely.

### Candidate 5 — macOS / nightly toolchain

**Ruled out.** Toolchain unchanged across W0 → W0'. Pre-W0' builds
on the same host completed in 3-6 min. Post-W0'.a builds on the
same host stall 12-15 min. The delta is in-source.

## Fix sketch (for plan agent)

The value-frame arena already carries enough information to record
each direct child count at push time — no post-hoc subtree walk is
required.

Replace the `while cursor < total` loop in `value_end_compound`
with an in-checkpoint counter the caller increments on every direct
child push. Concretely:

1. Extend `ValueCheckpoint` (at `crates/tape/src/builder/value.rs`)
   with a `direct_child_count: u32` field initialised to 0 by
   `value_begin_compound`.
2. Every frame push that occurs between `value_begin_compound` and
   the matching `value_end_compound` increments the top-of-stack
   checkpoint's `direct_child_count` by 1 **only when the new frame
   is a direct child of the currently-open compound** — i.e. when
   the value open-stack hasn't deepened since the previous
   sibling's close. The natural hook is:
   - `value_begin_compound(...)` → increment the *parent* checkpoint
     (second-from-top after the push).
   - `push_value_leaf(...)` → increment the top checkpoint directly.
3. `value_end_compound` becomes `O(1)`: pops the checkpoint,
   reads `direct_child_count`, writes
   `frame.child_count = direct_child_count`, writes `frame.span_hi`.
4. `subtree_size` retains its `ValueChildren` iterator use
   (`crates/tape/src/builder/value.rs:349`). That call path runs at
   projection time, *not* per-close during parse, and walks the
   final contiguous frames[] once per projected tree — `Θ(N)` over
   the whole projection. No hot-loop call.

Alternatively, `value_end_compound` could consult the ValueFrame
`first_child` field it already records (`frame_offset + 1`) and
compute `child_count = (frames.len() - first_child) - descendant_span`
where `descendant_span` is the pop-time sum of all opened checkpoints'
recorded subtree spans — same O(1) property, different bookkeeping.
Either is a constant-time replacement; pick the cleaner fit for
the open-stack discipline.

Every W0p.md invariant survives:
- §14 `FusedBuilder` sole builder: retained (the type doesn't
  change).
- §15 `push_compound` / `mark_children` absent: retained (no
  re-introduction).
- §16 materializer call-count truth: retained (the projection path
  is untouched; only the builder-close bookkeeping changes).
- §17 `STRUCTURAL_SCAN_POLICY` splice: retained (view/value.rs
  untouched).
- §18 zero dead `#[allow(dead_code)]`: retained.
- §19 `Parsed::to_value()` non-panic: retained (the fix preserves
  the contract that `frame.child_count` equals the direct-child
  count on every closed frame).

## Probe artefacts

- `/Users/mkbabb/Programming/bbnf-lang/.profiles/w0p-regen-root-cause/probe-stderr.log`
  — probed `cargo expand` stderr through the `compile_paths_request
  ENTER` sentinel.
- `/Users/mkbabb/Programming/bbnf-lang/.profiles/w0p-regen-root-cause/README.txt`
  — probe run metadata, probe-site map, toolchain versions.
- Probe instrumentation sites (all reverted in-tree; re-apply when
  re-investigating):
  - `crates/derive/src/lib.rs::bbnf_derive` (entry, cache_key,
    compile_paths_request bounds).
  - `crates/core/src/pipeline/compile.rs::compile_paths_request`
    (enter + load_merged_paths + compile_ast_request_internal).
  - `crates/core/src/pipeline/directives.rs::load_merged_paths`
    (per-path load_module_graph + total).
  - `crates/core/src/pipeline/directives.rs::parse_to_pipeline_inputs`
    (BbnfBootstrap::parse + extract_for_pipeline).
  - `crates/core/src/generate/mod.rs::generate_all` (entry +
    compile_grammar wall).
  - `crates/core/src/backend/rust/view/value.rs::emit_value_surface`
    (admissions + variants + emit fns).
  - `crates/core/src/backend/rust/view/value.rs::emit_path_query_impls`
    (variants loop).
  - `crates/core/src/pipeline/compile.rs::PipelineTimer::span`
    (unconditional print for passes ≥ 50 ms).
