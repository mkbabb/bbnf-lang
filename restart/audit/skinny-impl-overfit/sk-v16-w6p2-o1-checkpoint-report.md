# SK-V16 W6.2 — O(1) Speculative-Checkpoint Status

Commit: `8153236e8` — perf(sk-v16-W6): O(1) speculative-checkpoint via scratch-stack hoist
Worktree base: current master (W5 grammar-derived CSS + lightningcss comparator)
Platform: aarch64 Apple M5 Max. Cold benches (warmup_iters=0).

## Does CSS now beat lightningcss?

No. CSS track1 does **not** beat lightningcss, and does not beat cssparser.

| Engine | Corpus throughput |
|---|---|
| track1 (this work) | 63.904 Mbps |
| lightningcss (the >SOTA bar) | 754.692 Mbps |
| cssparser | 2239.519 Mbps |

track1 is ~11.8x slower than lightningcss and ~35x slower than cssparser on the full
979,638-byte corpus. **This is NOT the W6 >SOTA win.** It is reported honestly with evidence.

## The change

A **sound** O(1)-amortized speculative checkpoint, fixed at the generic mechanism source —
not by hand-editing generated files. Every growing per-frame container was hoisted out of the
`OpenFrame` into builder-owned append-only **scratch stacks**; each container-owning frame now
holds only a `*_base: usize` cursor (CSS: pending_rules/decls/selectors/values/blocks/components;
JSON: pending_items/pending_pairs; bbnf+sheets+simple-cohort: pending_children).

- `begin_compound` records `base = scratch.len()`; interior deposits append to scratch top;
  `end_compound` drains `[base..]` as one contiguous list. LIFO nesting keeps ranges disjoint.
- Frames are now `Copy` (scalars + cursors), so the checkpoint `stack.clone()` collapses from an
  O(document) deep clone of the growing root-compound `rules` Vec at every checkpoint (the O(N^2)
  cause) to an O(stack-depth) memcpy.
- Rollback truncates each scratch to its checkpoint length-marker — the precise inverse of append.
  Below-marker frame scalars are restored by the now-cheap `stack.clone()`.
- commit/drop stay no-ops; no checkpoint/commit/rollback signature changes; zero emitter call-site
  changes.

Sources edited: `crates/core/src/runtime/builder.rs` (trait doc), `xtask/src/regen_css.rs`,
`xtask/src/regen_simple_runtime.rs`, and hand-written `crates/core/src/runtime/builder_template.rs`.
All 9 grammars regenerated; `regen --check` clean 9/9; every runtime-builder regen idempotent.

The implementer **diverged from the design's deferral-journal/watermark prescription** because the
watermark scheme is unsound here: profiling the emitted code showed checkpoints are routinely
DROPPED without commit (commit count 916 << checkpoint count 1452 in generated CSS), so a
push-on-checkpoint/pop-on-commit watermark stack leaks, and the design's "min watermark"
discriminator is wrong (must be max). The scratch-stack hoist achieves the identical
O(N^2)->O(N) elimination with strictly simpler, provably-sound machinery.

## Correctness — the gate

Correctness is the gate, not speed. This is the GENERIC speculative-parse mechanism shared by all
grammars; a wrong rollback would silently corrupt parses. Both correctness gates pass EXACTLY.

**W5 8-field structural equality (Gate 1+2) holds.** `css_l4_w6_typed_retime` 2/2 pass,
`shared_summary_equal=true`, all counts == cssparser:
- track1_rules = 10136 == cssparser 10136
- track1_style = 9561 == 9561
- track1_sel = 9561 == 9561
- track1_decls = 20043 == 20043
- track1_errors = 0, cssparser_errors = 0

track1 builds the full typed document via `CssL4Parser::parse + visit_document`, not a constant.

**Full cross-grammar parity is green** (ax-iter, --no-fail-fast; scrutineer independently re-ran
all 10 binaries, 0 failures):
- JSON 57: json_parity 13/13, json_value_parity 14/14, json_canonical_parity 10/10,
  json_parity_struct 20/20
- CSS: css_l4_parity 17/17, css_l4_canonical_parity 3/3, css_pretty 3/3
- bbnf: bbnf_parity 2/2, bbnf_self_parity 56/56, no_grammar_name_branch 1/1
- sheets: sheets_parity / sheets_self_parity / sheets_expr_parity all green

Aggregate gate run: 274 tests run, 274 passed, 0 failed (2 competitor-feature-gated skips). Full
workspace `cargo iter-check-full` compiles clean.

The only non-gate failure in the broad sweep is `ts_node_execute::node_execute_twitter_statuses_zero_text`
(node v26 TypeScript-backend incompatibility) — pre-existing, byte-unchanged source, unrelated to
this Rust StructBuilder change, not in the gate suite.

## Speedup

The O(N^2)->O(N) win is real and exposed on single-sheet bootstrap (cold, warmup=0, release):

- `data/css/bootstrap.css` (280,311 B): 0.617 -> 8.741 Mbps = **14.2x**
- corpus `bootstrap-5.3.3.min.css` (232,803 B): 0.509 -> 7.957 Mbps = **15.6x**

(Scrutineer's independent re-run on the equality/parity build: 10.007 MB/s and 8.543 MB/s — same
order, honest variance.)

Full corpus track1 release = 63.904 Mbps; vs the design's ~3.1 Mbps fragment-corpus baseline that
is ~20x.

This is the SOUND recovery of the proven 62x clone-free-stub diagnostic. The stub hit 62x by
eliminating ALL checkpoint work, which is NOT sound (below-marker scalar writes would survive a
bare stack_len truncate). The sound version retains an O(depth) `stack.clone()` of Copy frames —
the honest ceiling — recovering ~14-16x on bootstrap rather than the unsound 62x. Parity was NOT
traded for speed.

## Scrutineer verdict

**ACCEPT.** rollback_sound=true, parity_real=true, equality_real=true, speedup_real=true.

The scrutineer independently verified:
- Rollback soundness with a proof: truncate is the exact inverse of speculative append because the
  checkpoint-placing emitter (structural_branch.rs Repeat/Alt/Negate/Minus) wraps each speculative
  sub-expression in a self-contained IIFE between checkpoint() and rollback(), and begin/end_compound
  are emitted as a matched lexical pair inside one rule body — so every `split_off(base)` has
  base >= marker and no lower-frame deposit escapes truncate. This emitter was NOT modified.
- Equality real (re-ran css_l4_w6_typed_retime, all counts match, 2/2).
- Parity real (re-ran all 10 binaries, 0 failures).
- Speedup real + cold + correct-AST on the same build that passed equality+parity.
- Clean regen: `cargo xtask check-runtime` exit 0, all 9 builders byte-identical, generated headers
  present, no grammar/parser/arena files touched.
- One minor non-blocking note: the bench labels bytes/elapsed/1e6 as "Mbps" but computes MB/s; units
  mislabel only — the 14.2x ratio is internally consistent and correctness is unaffected.

## Recommendation

**MERGE + materialization follow-on.** The change is DONE and SOUND, both correctness gates pass
exactly, full cross-grammar parity is green, and the scrutineer independently ACCEPTs with a rollback
soundness proof. The O(N^2)->O(N) algorithmic defect is genuinely eliminated and the ~14-16x bootstrap
recovery is real.

This is NOT REJECT-and-redo: nothing is unsound. But it is NOT the W6 >SOTA result either — track1 at
63.9 Mbps remains ~11.8x under lightningcss (754.7) and ~35x under cssparser. The >SOTA bar is not met,
so a materialization follow-on is required to close the remaining gap.

The only remaining cost is the residual per-checkpoint `stack.clone()` (one small Vec<OpenFrame> alloc
of Copy frames). Eliminating it soundly is pure speed work with soundness risk and was deliberately
deferred to keep the parity gate untraded: either (a) stack_len-truncate plus a below-marker
scalar-only undo-log recorded unconditionally and replayed guarded-by-frame-alive (the
watermark/active-count approach is unsound — proven by commit 916 << checkpoint 1452), or (b) a
builder-owned reusable snapshot buffer. The design's deferral-journal/watermark as written would NOT
be sound (dropped-checkpoint leak + wrong min-vs-max discriminator); the scratch-stack hoist supersedes
it. Closing the lightningcss gap also needs the broader materialization work beyond checkpoint cost.
