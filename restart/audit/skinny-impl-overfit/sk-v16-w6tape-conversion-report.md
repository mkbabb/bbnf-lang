# sk-v16 W6 — CSS-on-Tape Conversion: Honest Status

## Verdict up front

**REJECT. The CSS-on-tape conversion does not exist.** No source changed, no commits landed, and the typed CSSOM is still the eager `OpenFrame`/`CssArena` value tree — not a lazy view over the flat tape. Equality and parity are genuine but they certify the **eager** path that was already on master. There is no tape speedup to report because no grammar rides the tape.

Worktree HEAD `1c5bd7a25 feat(sk-v16-W6-tape): add shared flat-tape runtime substrate` — zero commits ahead of master, working tree clean. This is the substrate-only commit, not the conversion.

## Where we are vs >SOTA — plainly

We are **not at >SOTA, and we did not move toward it this pass.** The >SOTA bar is to beat lightningcss (~61 Mbps) on the 979638-byte corpus; the actual SOTA reference cssparser sits far above (~151–259 Mbps token-summary, depending on comparator/thermal state). Track1 typed CSS remains pinned to the eager baseline. CSS beats lightningcss on **no** corpus. The whole point of the wave — remove the ~69% syslib alloc floor by decoding the typed CSSOM lazily from tape offsets, expecting a multi-x jump — was never implemented.

## 1. The CSS-on-tape conversion

**Status: NOT DONE.** The conversion is entirely absent at both the generated and source level.

- Typed CSSOM is **eager**, not a lazy tape view. `crates/core/src/runtime/css_l4/arena.rs:60-68` `CssArena` is Vec-of-Vec eager slabs (`rules: Vec<Vec<CssRule>>`, `decls`, `values`, `selectors`, `keyframes`), plus `colors: Vec<Box<CssColor>>` — `arena.rs:127-133 push_color` does `Box::new` **per color** (per-leaf eager heap alloc, the exact failure mode the discipline forbids). `document.rs:11-44` `CssDocument` holds the arena and returns `&[CssRule]` by slab-index; `view.rs` walks those eager slices. There is **no** `TapeCursor`, no `StructLayout`-driven offset decode, no lazy reconstruction (grep for `TapeCursor`/`emit_open`/`records.len` in `crates/core/src/runtime/css_l4/` is empty).
- Tape substrate is **unwired**. `grep 'use crate::runtime::tape'` across `crates/core/src` returns zero hits — no production runtime consumes the flat tape. `tape/mod.rs:58 TapeStructBuilder` exists as a generic impl but is referenced only by `record.rs`, `mod.rs`, and `tests/tape_substrate.rs`. CSS is **not** the first grammar wired to it; CSS uses its own eager `CssStructBuilder`.
- `OpenFrame` template is **not deleted** (no single codegen path). `builder.rs:16 enum OpenFrame` is the eager stack-of-frames builder; `builder.rs:468-680` calls `arena.push_rules/push_decls/push_values/push_selectors/push_color` (eager materialization). `builder.rs:210 self.stack.clone()` at checkpoint is clone-based eager rollback, not the substrate's O(1) `(records.len(), arena.len())` truncate. `runtime/mod.rs:12 pub mod builder_template` (the SimpleStructBuilder template) is still present and used by bnf/csv/ebnf/css_pretty/math builders.
- Source generator emits the **eager** path. `xtask/src/regen_css.rs` has ~90 `OpenFrame`/`CssArena`/`push_*` references and zero `TapeStructBuilder`/`TapeCursor`/`tape::` references. Regen would be self-consistent — but it regenerates the eager path.

## 2. W5 equality + full parity

- **W5 8-field equality: EXACT and GREEN — but proves the eager path.** `css_l4_w6_typed_retime`: `shared_summary_equal=true`; track1 == cssparser exactly: `rules=10136, style=9561, sel=9561, decls=20043`; `errors=0`. Corpus = `skinny/corpora/css-l4-sk-v14` (979638 bytes). This was already green on master and certifies the eager CSSOM build, not a tape view.
- **Full cross-grammar parity battery: NOT RUN this pass** (`parity_real=false`). The premise failed — there is no tape view to validate — so parity over a nonexistent lazy view is moot. The suites (json_parity 13/13, json_value_parity 14/14, json_canonical_parity, json_parity_struct 20/20, css_l4_parity 17/17, css_l4_canonical_parity 3/3, css_pretty, bbnf_parity, bbnf_self_parity, tape_substrate 10/10) exist and exercise the eager path; expected green on master, but they would not validate any lazy-view correctness.
- **Rich AST preserved** — as an eager tree, not dropped. `preserve-rich-ast` is satisfied only in the trivial sense that nothing was flattened; the mandated form (typed CSSOM as a lazy view reconstructed from tape offsets + `StructLayout`) was never built.

## 3. Track1 vs cssparser AND lightningcss — corpus + bootstrap

There is **no tape speedup** (`speedup_real=false`). The measured CSS path is the eager `OpenFrame` path; the tape does not decode CSS.

| Path | track1 typed (Mbps, cold, M5 Max) | Notes |
|---|---|---|
| Track1 eager baseline (this worktree) | **24.846** (build run) / 67.397 (scrutineer run) | same 979638-byte corpus; scale differs by build/thermal state, verdict identical |
| lightningcss (>SOTA bar) | ~61 | track1 below it on the lower run, near it on the higher — never beats it |
| cssparser (SOTA reference) | ~151–259 (token-summary 2402.8 in retime comparator) | track1 far below; margin reported -2336.4 in-gate |

The retime gate itself reports `disposition=REJECTED reason=track1_typed_below_cssparser_threshold`. No multi-x jump from alloc-floor removal occurred because the alloc floor (Vec slabs + `Box<CssColor>` per leaf) is fully intact. Bootstrap-corpus numbers are unchanged from the eager baseline for the same reason — no path was rewired.

## 4. Speedup vs the 13.4 Mbps eager baseline

**None.** The discipline anchors the real-corpus eager baseline at ~13.4 Mbps; this worktree's track1 (24.846 / 67.397 depending on run) is the **same eager build**, not a tape acceleration. Any delta from 13.4 is build-config/thermal noise, not a conversion gain. There is no tape number to attribute speedup to.

## 5. Does CSS now beat lightningcss on any corpus?

**No. On no corpus.** Track1 typed CSS sits at or below lightningcss and an order of magnitude below cssparser, on the eager path. The conversion that was supposed to clear the bar was not implemented.

## 6. Scrutineer verdict

**REJECT.** `equality_real=true`, `parity_real=false`, `speedup_real=false`, `rich_ast_lazy=false`, `no_registry_indirection=true` (vacuously — no conversion attempted, so no generic-registry indirection reintroduced; the `CssArena` slab-index is pre-existing eager indirection, not the forbidden hot-path generic registry). The deliverable — CSS reconstructed as a lazy view over the flat tape via `TapeCursor`+`StructLayout`, `OpenFrame` deleted, CSS wired to `TapeStructBuilder` — is entirely absent. Equality and counts are real but belong to the eager path. No agent work landed.

## 7. Why it stalled (the real blockers)

1. **Routing fidelity.** The eager `OpenFrame` builder encodes context-dependent CSS routing (declarations/selectors/aggregate/numeric/functions/colors rule-id sets in `regen_css.rs resolve_builder_routes`) that the generic tape does not carry. Reproducing it bit-exactly on the tape is multi-thousand-line, and any drift breaks the EXACT `10136/9561/9561/20043` gate.
2. **Borrowed-slice vs lazy conflict.** `value.rs` is mandated unchanged with borrowed-slice accessors. Those cannot be genuinely lazy without either re-materializing the eager arena (no speedup) or changing the visitor/accessor API (forbidden). This conflict was never resolved at the directive level. Generated parse fns are monomorphized to `CssStructBuilder` by name, so CSS cannot ride the tape without regenerating the parser and landing the lazy view in the same coupled change.

## 8. Generalization path (JSON / sheets / bbnf onto the tape)

Not reached. Generalization is downstream of a working CSS conversion: once one grammar rides `TapeStructBuilder` via the `StructLayout`-driven lazy-view generator (`SeqPosition->cursor.child(pos)`, `BranchTag->meta_idx/branch dispatch`, `TypedLeaf->decode-by-type_desc`, `RuleReference->child+recurse`), JSON/sheets/bbnf follow by flipping their builder seam to the same generic impl and deleting their `builder_template` instances. None of that can begin until the CSS path proves the lazy view is lossless and faster. Today the substrate is grammar-agnostic and ready; no grammar consumes it.

## 9. Recommendation

**REJECT-REDO.** Do not merge as a CSS speedup — there is no speedup and no conversion. The substrate (1c5bd7a25) is correct and worth keeping on master as foundation, but the wave's deliverable is absent.

Redispatch the actual conversion against `1c5bd7a25`:

1. **Resolve the borrowed-slice-vs-lazy conflict at the directive level first** — decide cursor/iterator accessors vs a post-parse arena materialized from tape. This is the gating decision; everything else is blocked on it.
2. Implement `emit_view`/`emit_document` in `xtask/src/regen_css.rs` over `TapeStructBuilder` records+arena, walking `layout.fields` with the route tables from `resolve_builder_routes`.
3. Port CSS semantics node-by-node, gated continuously against the eager path.
4. Delete `OpenFrame` wholesale (no fallback fork); flip `rust.builder_path` (the `substrate.rs:41` seam) to `TapeStructBuilder`.
5. Regen (`cargo xtask regen --grammar css_l4` + regen-css, `--check` clean 9/9), then re-prove W5 EXACT + full cross-grammar parity `--no-fail-fast` + cold W6 bench.

Only then is the >SOTA question — does lazy tape decode beat lightningcss — actually testable. Until that lands, **we are at the eager baseline, below lightningcss, far below cssparser, with no path moved.**
