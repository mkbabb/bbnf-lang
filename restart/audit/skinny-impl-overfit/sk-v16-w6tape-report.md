# sk-v16 W6 — Flat-Tape Rearchitecture: Honest Status

## Verdict up front

**PARTIAL. Foundational substrate only. REJECT for merge-as-CSS-speedup; the >SOTA bar is NOT met and nothing on the CSS path moved.**

DESIGN steps 1-2 (the shared, grammar-agnostic tape substrate) are real, tested, formatted, and committed. DESIGN steps 3-10 (the actual CSS-on-tape conversion that the whole wave exists for) are **not done and not verified**. The committed code is correct but **unwired dead code** with respect to any parse path. No grammar — CSS included — rides the tape yet.

Commit: `1c5bd7a25 feat(sk-v16-W6-tape): add shared flat-tape runtime substrate` (ahead of master `deddb2830`).

## 1. The tape + lazy-view conversion

**Built (steps 1-2):** `crates/core/src/runtime/tape/` — a grammar-agnostic, crate-internal, append-only post-order tape.

- `record.rs` — 16B `#[repr(C)]` align-4 `TapeRec`, const-asserted `size==16 / align==4`. `TapeKind{Open,Leaf}`, `LeafType` tag + `STRING_BORROW_BIT` in `extra`, 5-bit `meta_idx` + 6-bit branch packed into `kind_meta/flags`.
- `arena.rs` — 8-aligned `PayloadArena` bump allocator, O(1) truncate.
- `cursor.rs` — `TapeCursor` lazy on-demand decode, post-order backward child walk (`child_count`/`child`/`children_forward` in source order).
- `mod.rs` — `TapeStructBuilder<'p>`, the single generic layout-driven `StructBuilder` impl (no per-grammar route strings), O(1) `Copy` checkpoint = `(records_len, arena_len, scope_depth)`, rollback by truncate, zero-copy string-borrow detection, inline-pack small u64 vs arena for wide.

10 substrate unit tests in `tests/tape_substrate.rs` — all green. They are **correctness-only**, not throughput.

**NOT built (steps 3-10):** the lazy-view accessor generator (step 3), the `regen_css.rs` `emit_builder/emit_view/emit_document/emit_arena` rewrite (step 4), the CSS builder-selection seam flip (step 5), the CSS-on-tape regen + W5/parity re-prove + W6 cold bench (steps 6-10).

**The lazy view does not exist.** The typed CSSOM today is still an **eager value tree**: `crates/core/src/runtime/css_l4/builder.rs` `OpenFrame` stack + `pending_*` scratch materialise into `CssArena` `Vec<Vec<...>>` slabs (`arena.rs`). `CssView` (`document.rs`) is a borrowed navigator over those retained eager values, **not** reconstructed from tape offsets + `StructLayout`. The generated `css_l4.rs` parser hardcodes `CssStructBuilder<'p>` at every call site — it is not generic over `B: StructBuilder`, so CSS cannot ride the tape without regenerating the parser AND landing the lossless lazy view in the same coupled change.

## 2. W5 equality + full cross-grammar parity integrity

**Both hold — because the parity-bearing tree was never touched.** The commit is purely additive: new `tape` module + one-line `mod tape;` + the new test file. No generated file and no selection seam was edited.

- **W5 8-field equality (the directly-affected gate): EXACT.** `css_l4_w6_typed_retime` → `shared_summary_equal=true`; track1 == cssparser exactly: `rules=10136, style=9561, sel=9561, decls=20043`; `track1_errors=0, cssparser_errors=0`; 4/4 corpora. Re-run live, cold, native aarch64 by both builder and scrutineer.
- **Full cross-grammar parity: ALL GREEN** (scrutineer, release + `target-cpu=native`, `--test-threads=1`): json_parity 13/13, json_value_parity 14/14, json_canonical_parity 11/11, json_parity_struct 20/20, css_l4_parity 17/17, css_l4_canonical_parity 3/3, css_pretty 3/3, bbnf_parity 2/2, bbnf_self_parity 56/56, no_grammar_name_branch (1 pass / 1 ignore), tape_substrate 10/10. **0 failures.**
- `cargo xtask regen --check`: clean **9/9** grammars matched, exit 0. No hand-edited generated files.

**Rich AST preserved:** `CssColor/CssDimension/CssFunction/Selector/CssRule/CssTypedValue` all defined and fully traversed; value plane richly populated (dimensions=2963, colors=1169, functions=883, lists=6754) — not flattened to Span. Preserved as an eager tree, not dropped for speed.

## 3. Track1 Mbps vs cssparser AND lightningcss — the >SOTA bar

**There is NO tape speedup. The measured CSS path is still the eager `OpenFrame` path; the tape does not decode CSS.** No tape throughput is reported because none exists, and none was fabricated.

Two cold, native-aarch64 (M5 Max), `warmup_iters=0` measurements were taken on the 979638-byte corpus. They differ in absolute scale (build run vs scrutineer run — different machine thermal/build state) but **agree on the ratio and the verdict**:

| Source | track1 Mbps | cssparser Mbps | lightningcss Mbps | track1/lightningcss |
|---|---|---|---|---|
| Build run | 13.416 (first cold 14.884) | 150.715 | 60.96 | 0.220 |
| Scrutineer run | 69.668 | 2529.390 | 793.326 | 0.0878 |

**Corpus (979638 B):** track1 is **~4.5x to ~11x slower than lightningcss** and **~11x to ~36x slower than cssparser**, depending on run. The scrutineer's ~70 Mbps track1 corresponds to the brief's stated 63.9 baseline; the build run's ~13-15 Mbps is lower-scale but the same gap.

**Bootstrap single-sheet:** NOT separately benched this pass — there is no tape CSS path to compare against, so the corpus number is the only honest baseline.

## 4. Speedup vs 63.9 baseline

**NONE.** Scrutineer's live track1 = 69.668 Mbps ≈ the stated 63.9 baseline — i.e. no improvement, within run-to-run noise. The eager path is byte-for-byte the same code as before the commit. The 4-5x (corpus-run) / 11x (scrutineer-run) gap to lightningcss is exactly the gap the tape rearchitecture exists to close, and it **remains fully open**.

## 5. Scrutineer verdict

**REJECT.** Findings (all cited, all verified live):

- `equality_real: true`, `parity_real: true`, `rich_ast_preserved: true`.
- `speedup_real: **false**` — disposition REJECTED, reason `track1_typed_below_cssparser_threshold`, margin −2460.722 Mbps.
- **Tape substrate is unwired dead code.** `grep` shows ZERO usage of `TapeStructBuilder`/`TapeCursor`/`TapeRef` in any parse path — only the `runtime/mod.rs` module decl and past-tense test doc-comments. The emitter emits NO tape code (all `Tape` refs there are comments). The HEAD commit message itself states CSS/JSON remain on the eager `OpenFrame` path.
- **Lazy-view-over-tape is false** (see §1).
- No `StructRegistry`/`Arena<G>`/`Builder<G>` indirection reintroduced (the AZ-IV 28-65x failure mode is absent; `StructRegistry` import is unused). `TapeStructBuilder` is a single non-generic layout-driven struct. This is architecturally clean.
- Minor non-gate: `OpenFrame::ColorMix` variant never constructed (dead-code warning) though `CssColor::Mix` exists.

## 6. Does CSS now beat lightningcss on any corpus?

**No. Not on any corpus.** CSS does not run on the tape at all, so the tape changes nothing measurable. On the only benched corpus (979638 B) the eager track1 path is 4.5-11x slower than lightningcss and far slower than cssparser. We are nowhere near the >SOTA bar.

## 7. Generalization path (JSON / sheets / bbnf onto the tape)

**Real by construction, but NONE converted yet.** `TapeStructBuilder` dispatches only on `StructLayout` (no per-grammar route strings), and JSON / sheets / bbnf already implement the same `StructBuilder` trait — confirming the substrate is the correct drop-in seam for every grammar. The intended sequence is CSS first (it carries the hardest semantic routing), then JSON / sheets / bbnf follow once the lazy-view generator is proven. But the conversion is gated on the lazy-view accessor generator, which does not exist. The path is credible; the work is unstarted.

## 8. Where we stand vs >SOTA — plainly

We are **not at SOTA, not approaching it, and this pass did not move us toward it on any measurable axis.** The CSS typed path is ~70 Mbps cold (≈ the 63.9 baseline) against lightningcss ~793 and cssparser ~2529 on the scrutineer's machine; ~13-15 vs lightningcss ~61 on the build machine. Either way: **roughly an order of magnitude below the >SOTA bar.** The A-series tape that historically reached CSS bootstrap 454 / normalize 735 Mbps (commit `3b8b757d`, `crates/bbnf-tape`) is the proof-of-concept this substrate is meant to reconstitute generally — but the reconstitution is, today, an empty foundation.

## 9. Recommendation

**Option C — merge the substrate as foundation, REJECT as a speedup claim, do NOT advance the wave.**

Concretely:

1. **MERGE the substrate is acceptable but not as a wave-completing artifact.** It is correct, clean, additive, gate-preserving, and the architecturally right seam (single non-generic layout-driven `StructBuilder`, no `Arena<G>` indirection). It can land as scaffolding. It must NOT be reported as any kind of speedup — there is none.
2. **Do NOT claim CSS-on-tape, lazy-view, or any throughput win.** All of steps 3-10 are open. Reporting otherwise would be fabrication; the builder explicitly refused to ship a parity-breaking half-conversion or invent bench numbers, which is correct.
3. **Next wave is the actual conversion** — the coupled multi-pass effort, iterated against the W5 gate:
   - Write the layout-walk lazy-view accessor generator in `crates/core/src/backend/rust/emitter/` that reconstructs each `CssRule/StyleRule/Selector/Declaration/CssTypedValue` from `(TapeCursor, &StructLayout, source)` — `SeqPosition→cursor.child(pos)`, `BranchTag→meta_idx/branch dispatch`, `TypedLeaf→decode by type_desc`, `RuleReference→child+recurse` — **and thread the CSS-specific routing the eager builder encodes as DATA, not lose it**: the regen route tables (declarations/selectors/aggregate/numeric/functions/colors rule-id sets) + 0/1/N value-list collapse + selector span-vs-single + dir-pseudo synthesis + hex packing + color component order. None of this is recoverable from `StructLayout` alone.
   - Rewrite `regen_css.rs` `emit_builder` (DELETE the `OpenFrame` template) to select `TapeStructBuilder`; emit `emit_view/emit_document/emit_arena` as lazy accessors over the tape.
   - `regen --grammar css_l4` + `regen-css`; confirm `regen --check` 9/9.
   - Iterate the lazy view against `css_l4_w6_typed_retime` until `shared_summary_equal=true` with EXACT counts; then full parity `--no-fail-fast`.
   - **Only then** cold-bench tape track1 vs cssparser/lightningcss on the 979638 B corpus + bootstrap single-sheet.
4. The NEON pre-scan is premature: there is no tape decode path to pre-scan into yet. It belongs after CSS-on-tape lands and the gap is re-measured, not before.

**Bottom line:** the foundation is sound and worth keeping; the wave's purpose — beating, or even approaching, lightningcss — is **unmet**. Treat this as "substrate landed," not "W6 done."
