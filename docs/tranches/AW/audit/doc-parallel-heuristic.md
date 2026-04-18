# AX.W9 Doc-Parallel Heuristic Derivation — Audit Agent 6

## Angle headline

Document-parallel fork is ONE of six HIGH-confidence AX levers; its precedent (AW-IV.W4.4, `tailwind.css` 16 → 37 MB/s, +131% at 4 threads, the single AW arc breakthrough) tells us the mechanism works. The open question is split-point derivation post-W0b walker deletion: the existing `list_rules` substrate + `structural_alphabet` + shape classifier together already hold every fact needed to derive per-grammar fork boundaries heuristically. No `@parallel` directive, no new IR pass — only a minimum mining addition (one cut-byte per admitted list rule) and a shape-emitter-side consumer that replaces `dta_run_parallel` when the walker dies.

## 1. Existing `list_rules` substrate — provided + lacking

**Provided** (`crates/ir/src/passes/recognizers/list_rules.rs:138-147`):

- Entry-rule restriction: mines **exactly one entry** — the grammar's entry rule when `strip_transparent(body)` resolves to `Repeat { inner: Alt | Ref | Seq, hi: u32::MAX }`.
- Per-grammar mining result (per FINAL-IV.md:54): **CSS=1 `stylesheet`, BBNF=1 `grammar`, Sheets=1 `formula`, JSON=0** (JSON's entry `value = object | array | string | number | bool | null` is `Alt`, not `Repeat`, so it drops out).
- Emitted literal: `__GRAMMAR_PROFILE_LIST_RULES: [RuleId; 1] = [RuleId(52)]` (BBNF grammar rule id 52, verified at `crates/core/src/grammar/generated.rs:49`).
- Runtime consumer: `generated.rs:93504` — `if !GRAMMAR_PROFILE.list_rules.is_empty() && input_len > parallel_break_even_bytes { dta_run_parallel(..., list_rule_id, ..., dta_run_BbnfBootstrap, ...) }`.

**Lacking** for shape-emitter-side fork:
- **No cut-byte spec.** `dta_run_parallel_rayon` at `driver.rs:3004-3008` hard-codes `b'{'` / `b'}'` depth tracking — this is brace-specialised to CSS and does not generalise. For JSON arrays it would be `b'['`/`b']'`, for Sheets inter-formula `b'\n'` or N/A, for BBNF `b';'` terminators.
- **No per-grammar "expected cut-byte count at input size N".** Needed for the "enough boundaries to partition" check (currently done at runtime via `depth_0_close_slots.len() < effective_workers`).
- **Sheets entry actually admits falsely.** Per FINAL-IV.md:54 Sheets mined 1 list rule (`formula = /=?/ , expression`). But the grammar file (`grammar/google-sheets/google-sheets.bbnf:164`) shows `formula = /=?/ , expression` — a **2-child Seq**, not `Repeat`. The entry admission here is either stale (the Sheets "file" rule the doc references doesn't exist in the current grammar) or `strip_transparent` walks through to find a Repeat descendant. Either way, Sheets benches operate on single-formula inputs 505 B-1.8 KB and **fork is N/A** per AX invariant 15 (small-input amortisation bound).

## 2. Per-grammar safe-fork-boundary IR derivation

| Grammar | `list_rules` mines? | Entry body shape | IR-derived cut byte | Confidence |
|---|---|---|---|---|
| JSON | **No** (entry is Alt) | `value = object \| array \| string \| …` | None at root. Top-level `array` rule is Array-shaped but **not entry**; fork-at-sub-rule is deferred per W4.4 admission invariant. **JSON excluded from fork.** | N/A |
| CSS | Yes — `stylesheet` | `stylesheet = ruleList ?w`; `ruleList = (ruleItem ?w)*` (both Repeat-over-Seq). `ruleItem` body contains `ruleBlock = "{" >> blockContent << "}"` | **`}` at depth 0** (brace-pair nesting). Mineable: scan grammar for `single_byte_literal` pairs `(open, close)` inside the entry rule's transitive body where close-byte is in `structural_alphabet`. CSS `structural_alphabet` includes `{` (123), `}` (125). | HIGH |
| Sheets | Claimed yes (1) but grammar is single-formula | `formula = /=?/ , expression` | **None.** Fork N/A. `list_rules` mining should reject — only Sheets benches with multi-formula corpora (which don't exist today) would admit. | N/A |
| BBNF | Yes — `grammar` | `grammar = (grammar_item ?w)*` where `grammar_item = comment \| big_comment \| directive \| rule`, all terminating with `";" \| "."` | **`;` or `.` at depth 0** (directive/rule terminators). BBNF `structural_alphabet[14]=59 (;)`, `[11]=46 (.)`. | HIGH |

Key observation: **the fork-relevant cut byte is the close-byte of the outermost structural pair inside the Repeat's inner** (CSS `}`, BBNF `;`). `list_rules` admits the list rule; a minimal IR projection walks `entry.body` one level deeper to extract the cut byte from the body's Repeat child.

## 3. Minimum IR mining addition

Existing facts suffice for admission. One field addition closes the cut-byte gap:

```rust
// in tape/src/profile.rs GrammarProfile
pub fork_cut_byte: Option<u8>,  // None = no fork; Some(b) = cut on `b` at depth 0
```

Mining rule in `list_rules.rs::mine_list_rules`:
1. When an entry rule admits as list-shaped, recurse into `Repeat.inner`.
2. Find the first `single_byte_literal` (the helper already exists at `passes::inspect::single_byte_literal`) whose byte is in `structural_alphabet` and whose pairing is balanced (`open`, `close` both present — CSS `{`/`}`).
3. Emit the **close byte** as `fork_cut_byte` (CSS: 125, BBNF: 59 `;`).
4. If inner has no paired structural bytes, `fork_cut_byte = None` — fork rejects at runtime.

This replaces the hardcoded `b'{'` / `b'}'` scan at `driver.rs:3004-3008` with a per-grammar literal the shape emitter splices inline. **One new field, ~20 LOC of mining, zero new passes.**

## 4. Break-even heuristic — IR-derived formula

Current state: `parallel_break_even_bytes: u32` is hardcoded `1 << 20` (1 MiB) whenever `list_rules` non-empty (`passes/profile.rs:511-515`). `expected_ns_per_byte` is declared dead per `psi-and-dead-substrate.md:67` (**"read only by tests"**) and zeroed at `passes/profile.rs:541`.

The confirmed formula from the question body:

```
break_even = (fork_overhead + merge_overhead) / speedup_factor_per_byte
           ≈ 30 µs / (1.5 × 2 ns) ≈ 8 KB  (theoretical)
           but measured W4.4-fix raised to 1 MiB after bootstrap.css (280 KB) regression
```

The **1 MiB number** is the empirical floor from W4.4-fix — bootstrap.css at 280 KB lost throughput to join cost. The theoretical 8 KB is achievable only if join cost is genuinely linear; measured cost includes `Columns::with_capacity + Vec<u8>::with_capacity + rayon worker spawn + per-slot partition_point + positions.iter().map().collect()` (driver.rs:3133-3145), all of which scale with worker count, not bytes.

**IR-derived formula for AX.W9** — two inputs fold in naturally:

```
fork_cost_bytes = worker_count × (fork_fixed_µs + per_worker_alloc_bytes) / nanos_per_byte
```

Where `nanos_per_byte` is **derivable from `compounds_per_input_byte + leaves_per_input_byte`** (both already populated per `passes/profile.rs:540`). A grammar that emits ~1 record per byte (all 4 grammars per `profile.rs:290-299`) has `nanos_per_byte ≈ 2-4 ns` depending on path length. The `expected_ns_per_byte` slot is **not dead** — it is **unwritten**; W9 should populate it from `compounds_per_input_byte` × measured `ns_per_push` constant (already known — ~1.5 ns/push per AU bench regressions).

**`expected_ns_per_byte` ALIVE after AX.W9.** It replaces the hardcoded 1 MiB with `(n_workers × 30 µs) / expected_ns_per_byte` — derived per-grammar, not a blanket constant.

## 5. Emitter-side consumer post-W0b

Post-W0b (`AX.md:90`, DTA walker ~74 KLOC deletes, `dta_run_parallel` with it), the consumer is re-grown inside the **shape-emitter dispatcher**:

```rust
pub fn parse(input: &str) -> Result<Parsed<'_, Self>, ParseErr> {
    let mut builder = TapeBuilder::with_capacity(GRAMMAR_PROFILE.capacity_for(input.len()));
    const STRUCTURAL: StructuralAlphabet = StructuralAlphabet::from_profile(&GRAMMAR_PROFILE);
    let idx = scan_structural(input.as_bytes(), &STRUCTURAL);

    let root_off = if let Some(cut_byte) = GRAMMAR_PROFILE.fork_cut_byte
        && input.len() > fork_break_even(&GRAMMAR_PROFILE, rayon_num_threads())
    {
        fork_shape_parse::<Self>(input.as_bytes(), &idx, cut_byte, &mut builder,
                                  parse_<grammar>_<root>)
    } else {
        parse_<grammar>_<root>(input.as_bytes(), &mut 0, &mut state, &mut builder)?
    }?;
    Parsed::new(builder.finish(), input, root_off)
}
```

Where `fork_shape_parse<G>` is a **single generic in `crates/tape/src/parallel.rs`** (new module, post-rename path): takes the per-shape root fn pointer, partitions on `idx.positions` by scanning for `cut_byte` at depth-0 using `structural_digraph_mask` to track paired opens (`{` matched against `}`, `[` against `]`), spawns workers, each worker re-runs the same per-shape `parse_<grammar>_<root>` on its sub-slice, joins via `TapeBuilder::merge_from(&mut other_builder, byte_offset, rec_offset)`.

**Key shift vs W4.4**: workers call the **per-shape emitted function** (grammar-specialised, inlined), not a generic `WalkerFn`. The fork becomes a thin parallel.rs wrapper over the same code that runs sequentially. No walker, no PSI, no frame_depth stream.

## 6. Split-point discovery — runtime or IR time

Given §3's `fork_cut_byte: Option<u8>`, discovery is **IR-time spec, runtime scan**. The IR emits the cut byte; runtime does a single forward pass over `idx.positions` + `idx.kinds` (both already built) counting matched-opens against `structural_digraph_mask` to find depth-0 positions of the cut byte. O(idx.positions.len()), which is O(structural-byte-count), typically 2-5% of input. For 1 MiB input ≈ 50 KB of positions × 4 ns/lookup = 200 µs, amortised over a multi-MB parse.

This is what `driver.rs:3016-3031` already does — just parametrised on `fork_cut_byte` instead of hardcoded `b'}'`.

## 7. Prior-attempt archaeology — AW-IV.W4.4 learnings

From FINAL-IV.md:54-57 + `driver.rs:2981-3100`:

1. **Naive even-slot partition was wrong.** W4.4-fix landed the depth-0 brace walk after bootstrap.css + tailwind.css tape-parity regressions. **Heuristic: shards must start at structural boundaries, never mid-rule.** The new fork must preserve depth-tracked cut selection.
2. **Byte-balance, not item-count-balance.** Tailwind has short custom-property rules early and huge `@keyframes` later; item-count balance produced lopsided shards. W4.4-fix uses `target_byte = w × input.len() / n_workers` and picks the nearest depth-0 close. Preserve.
3. **Sub-index materialisation moved from serial to parallel.** `driver.rs:3126-3141` moved `sub_positions: Vec<u32>` + `sub_kinds: Vec<u8>` construction inside `into_par_iter`. Preserve — serial pre-materialisation dominates wall-clock on large inputs.
4. **Threshold raised 256 KB → 1 MiB.** Below 1 MiB, join cost won. AX.W9's per-grammar `expected_ns_per_byte` must retain a conservative default ≥ 1 MiB for un-calibrated grammars.
5. **Worker count cap at 4 per `generated.rs:93509`.** This is a dev-platform heuristic; should be `rayon_num_threads()` clamped to `input.len() / break_even`.

## 8. Concrete AX.W9 scope — substrate + consumer in one wave

**Substrate** (2 parallel agents):
- `list_rules.rs`: add `fork_cut_byte` mining (walk entry Repeat inner for balanced structural pair).
- `profile.rs`: add `fork_cut_byte: Option<u8>` field; populate `expected_ns_per_byte` from `compounds_per_input_byte × 1.5`; derive `parallel_break_even_bytes` from that.
- `crates/core/src/backend/rust/emitter/profile.rs`: emit new field.

**Consumer** (1 parallel agent, depends on above):
- `crates/tape/src/parallel.rs`: new module, `fork_shape_parse<G>(input, idx, cut_byte, break_even, worker_fn)` — generic over per-shape entry fn. Lifts depth-0 cut scan + byte-balanced shard selection + rayon dispatch + `TapeBuilder::merge_from` join.
- `TapeBuilder::merge_from`: new method in `tape` lifting the offset-rewrite logic from `driver.rs:2883-2894` but over shape-emitted records (no `FrameStack`, no frame_depth stream).
- Dispatcher emitter (`shapes/dispatcher.rs`): wrap emitted `parse_<grammar>_<root>` call with the fork branch, consuming `GRAMMAR_PROFILE.fork_cut_byte`.

**Wire-contract test** (mandatory per README.md:197-232): `tests/fork_heuristic_wire_contract.rs` — for each of CSS, BBNF, Sheets, JSON: assert `GRAMMAR_PROFILE.fork_cut_byte` matches the expected mined byte (CSS=125, BBNF=59, Sheets=None, JSON=None); invoke `parse()` on a known-multi-item fixture ≥ break-even size; assert both fork path and sequential path produce tape-parity-identical output.

**Hard gate**: CSS `tailwind.css` ≥ 2.5× sequential on 4 cores (matches AX.W9 invariant at AX.md:105); BBNF `bbnf.bbnf × 10-concatenated` ≥ 2× sequential; all parity harnesses green.

**Delete at W0b, not W9**: `dta_run_parallel` + `dta_run_parallel_rayon` (~300 LOC in `driver.rs:2819-3300+`) deletes with the walker. W9 replaces the consumer; W0b kills the old one. One wave each, no overlap — fork_cut_byte mining lives on from W9, walker-sided consumer evaporates at W0b.

**Forbidden surface confirmed absent.** No `@parallel` directive, no user-declared split points, every heuristic traces to:
- `list_rules.rs::mine_list_rules` (existing, extended with cut-byte)
- `structural_alphabet` (existing)
- `compounds_per_input_byte` (existing, now drives `expected_ns_per_byte`)

Total minimum IR mining delta: **one field, ~20 LOC**. Total minimum emitter delta: **one parallel.rs module, one TapeBuilder::merge_from method, one dispatcher branch**. The 37-item AX plan's W9 fits in a single substrate-with-consumer wave per the README.md mandate.
