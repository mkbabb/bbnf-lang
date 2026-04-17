# Tranche AW — The Activation

> **Split into AW-I + AW-II + AW-III + AW-IV.**
> [`AW-I.md`](./AW-I.md) carries W0 + W1-substrate (landed) plus
> walker completion, legacy `fn __<rule>` deletion, `parse()`
> swap, `MemoStore` retirement, fuse/inline activation.
> [`AW-II.md`](./AW-II.md) carries the DTA self-host round-trip
> (lowering pipeline migration off fn-per-rule tape-shape
> assumptions onto DTA's Seq-wrapped structural layer).
> [`AW-III.md`](./AW-III.md) carries DTA correctness + viability
> validation: Cluster A/C/D residuals close, 67 `#[ignore]`
> audit, samply viability profile, minimum-viable lever subset.
> [`AW-IV.md`](./AW-IV.md) (formerly the plan named AW-III)
> carries PSI rayon, ShapeRef dispatch, PHF/SIMD keyword tables,
> selector classifier, scanner closure, document-parallel parse,
> bloom + GADT dedup, Pratt generalisation, parity harnesses,
> visitor API, bench parity. `FINAL.md` composes per-letter;
> this file preserves the original plan as reference; AW-I.md
> through AW-IV.md are authoritative for execution.

AW is the substrate-activation tranche. AV laid every emission
channel — `pub const DTA_TABLE`, `pub const GRAMMAR_PROFILE`,
`pub const SHAPE_DICT`, `pub const BBNF_SHAPE_DICT`, the
`PayloadStream` + Stage-C finaliser API, the `TapeKind::ShapeRef`
cursor expansion, the visitor reordered-unrolling codegen — but
every one of those constants is consulted by no consumer. The
runtime parse path remains the AU-era recursive `fn __<rule>`
descent carrying every V0–V5 correctness write *on top of* the
legacy emission. AW deletes the legacy path, lights the
substrates, and recovers the perf the V0–V5 substrate work
overshot. *The compile-time emit lives; AW makes it run.*

The post-AV reality is honest in only one direction: the
correctness invariants AV claimed are upheld (every BBNF Bug-2
pinned assertion flipped, every Sheets Bug-1 op flipped, all
parity suites green), but parsing throughput regressed 2.5–4.5×
versus post-AU across every bench in the close-out matrix. The
regression is not a flaw in the AV substrate — it is the
substrate carrying weight without yet doing the work that
substrate enables. AW reverses the regression and clears the
post-AU baseline by activating the DTA driver, the PSI runtime,
the ShapeRef dispatch, the PHF + SIMD keyword classifiers, and
the bloom + GADT runtime dedup, with bench checkpoints between
every wave so the perf trajectory is observable not asserted.

This tranche also closes the typing debt AV's substrate work
*partially* addressed: AV.0.5 landed the `LargeAggregate`
arena-backed payload variant and the colour-function tuple-
shaped grammar declarations, but the layout pass refuses
`TypeDesc::Named("Color")` at admission so no rule's runtime
push fires the substrate. The view-layer accessor that
projects the packed blob into a
`lightningcss::values::color::Color::RGBA`-shaped Rust value
was never written. The `StructRegistry` approach AS.2.3
scaffolded was *deleted* in AU.4.2 (commit `ab8588a`) per the
no-legacy-code invariant — AU explicitly stated the forward
path: per-backend type tables, not a central IR registry.
AV's tuple-via-`LargeAggregate` design follows that path; AW
W0.5 wires the runtime consumer that completes it. The inline
`#[cfg(test)]` violations in
`crates/gorgeous/src/google_sheets.rs` and the 13 serialize/
structural roundtrip regressions AV ignored to get the
workspace green also close here. Each is an item AV explicitly
forwarded with a ticket; the ledger zeroes at AW close.

## Post-AV reality check

Per `docs/tranches/AV/FINAL.md` and `docs/benchmarks/post-AV.
json`, AV closed at the V5 boundary. Five waves landed
substrates; four waves (V6–V9) routed forward; ten test
families took `#[ignore]` with explicit AW V6+ forward-tickets.
The bench delta versus post-AU exposes the substrate-vs-perf
split:

| Bench | Entry | post-AU MB/s | post-AV MB/s | Δ | AW W6 gate |
|-------|-------|-------------:|-------------:|--:|-----------:|
| json_monolithic | canada | 1231 | 455 | −63% | **2000** |
| json_monolithic | twitter | 1967 | 481 | −76% | **2400** |
| json_monolithic | data_s | 1746 | 491 | −72% | **2000** |
| json_monolithic | data_xl | 1179 | 380 | −68% | **1700** |
| json_monolithic | citm | 2438 | 701 | −71% | **3000** |
| css_l4 | bootstrap | 454 | 182 | −60% | **800** |
| css_l4 | normalize | 735 | 299 | −59% | **1100** |
| css_l4 | tailwind | 496 | 207 | −58% | **1200** |
| google_sheets_monolithic | parse_simple | 95 | 21 | −78% | **250** |
| google_sheets_monolithic | parse_nested | 128 | 29 | −77% | **300** |
| google_sheets_monolithic | parse_stress | 121 | 28 | −77% | **300** |
| bbnf_monolithic | bbnf_self | 394 | 106 | −73% | **500** |
| bbnf_monolithic | json | 283 | 82 | −71% | **400** |
| bbnf_monolithic | ebnf | 223 | 60 | −73% | **350** |
| bbnf_monolithic | google_sheets | 858 | 253 | −71% | **1000** |
| bbnf_monolithic | css_pretty | 647 | 189 | −71% | **800** |
| bbnf_monolithic | css_l4_grammar | 496 | 145 | −71% | **650** |

AW W6's gates are AV's original V10 gates (it could not have
known V0–V5 would close below baseline; the gates remain
valid). The AW W0 cleanup baseline (post-cleanup, pre-DTA) is
expected to recover ~30% of the regression by elision alone;
the DTA activation in W1 is the lever that restores and
exceeds the post-AU floor.

## Architectural thesis

The substrate AV emitted is correct. The runtime that consumes
it is not built. AW's premise: every substrate channel AV
landed becomes the first-class code path; the legacy
`fn __<rule>` recursion is deleted in its entirety.

Two architectural questions warrant explicit answer here
before the wave schedule, because they recur across phases:

### Why SoA stays — and is necessary for DTA

AV's V2 substrate replaced `Vec<TapeRec>` with a column-of-
arrays `Columns` substrate. Per-push cache traffic rose from
one cache line to seven (one per active column). On the legacy
fn-per-rule hot path that AW W0 inherits, this is pure cost:
push frequency is unchanged but per-push work increased.

The cost amortises away once the DTA drives stage-A:

- The DTA emits records via *batch column appends* during the
  counter-DFA's natural dispatch loop, not via per-call push
  helpers. Each column receives a sequential block of values
  per stage-A traversal — sequential stores, prefetcher-
  friendly, vectorisable.
- Per-rule prelude/epilogue work disappears entirely. No
  `mark_children`, no `__span_lo` capture, no
  `__aggregate_buf: [u8; 16]` stack init, no
  `if __has_payload` epilogue branch — each of these is per-
  rule overhead the DTA collapses to per-skeleton-record
  overhead, which is much rarer.
- The PSI stream lands typed payloads into the matching
  column in one rayon-parallelisable pass post-stage-A. AoS
  cannot give that property — payloads in row-major order
  cannot be batched by type without scatter.
- The visitor codegen pass (AV.2.5, 3.3× scalar-left-fold-
  free already) requires `pay_f64: &[f64]` as a contiguous
  slice. AoS gives it `Vec<TapeRec>` with f64 payloads
  interleaved among non-payload records — no batch-friendly
  slice.

Pivoting back to AoS would force the DTA to walk row-major
records during stage-B payload fill, defeating the
parallelism PSI was designed to deliver, and would reverse
V2.5's visitor wins. The SoA cost is paid up-front; the SoA
returns compound across W1–W4. **AW keeps SoA. AW activates
DTA. Both are non-negotiable.**

### Why Stage-C must stop running unconditionally

`TapeBuilder::finish` today always invokes
`derive_frame_depth(&columns)` (one O(N) backward walk) then
`finalise(&mut columns, &frame_depth)` (one O(N) forward sweep)
— two extra full-tape scans per parse. Worse, Stage-C
overwrites `sib_skip` / `child_off` / `span_hi` columns that
`push_compound` already wrote correctly during the parse: the
push work is wasted. The finaliser was designed for the DTA
stage-A emit path where frame_depth is produced inline at zero
cost; running it on the fn-per-rule path is double-counting.

AW W0 makes Stage-C activation conditional on the DTA driver's
inline frame_depth emission. While the legacy path runs (W0
window only), Stage-C is skipped and `compute_sibling_skip`
(V2's backward-walk method, currently dead with a `dead_code`
warning) is the correct finaliser. After W1 deletes the legacy
path, `compute_sibling_skip` deletes too, Stage-C activates
unconditionally with frame_depth produced by the DTA.

## Architectural invariants

Inherited from AU and AV, strengthened where the activation
demands it:

1. **No legacy code, no fallbacks, no workarounds.** AW
   *deletes* the recursive fn-per-rule path in W1. There is
   one parse path post-W1: the DTA-driven stage-A skeleton
   mine + PSI stage-B payload fill + Stage-C finaliser. No
   feature flag, no dual-path build, no opt-in.
2. **Every substrate AV emitted has an active consumer at AW
   close.** `DTA_TABLE` drives `parse()`. `SHAPE_DICT` /
   `BBNF_SHAPE_DICT` dispatch via `push_shape_ref` at stage A.
   `keyword_tables` resolve to PHF / SIMD compare. `dedup_
   eligible_rules` drive runtime bloom + GADT. `parallel_
   break_even_bytes` is per-grammar-calibrated, not zero.
   The visitor reordered-unrolling kernels emit unconditionally
   for every active payload column per grammar via
   `Tape::reduce_column<C, R>` — Rust-side API, no grammar
   surface.
3. **Bench between every wave.** Each wave concludes with an
   agent-driven bench checkpoint that captures the four
   parse-bench matrix to `docs/benchmarks/post-AW-W{N}.json`.
   No wave is "closed" until its bench checkpoint lands and
   shows progress against the previous checkpoint OR carries
   a written rationale for why the wave's lever does not move
   the bench it targets. The trajectory is observable, not
   asserted.
4. **Typed-AST parity is total.** `lightningcss::values::
   color::Color`, `sonic_rs::Value`, every BBNF Span
   annotation — every typed projection round-trips bit-
   identically against its peer parser's output. Parity
   harnesses land in W5 and gate workspace-green at AW close.
5. **Colour-function `LargeAggregate` consumer wires in
   this tranche.** Not a Named-struct-registry restoration
   — that scaffold was *deleted* in AU.4.2 (commit
   `ab8588a`) with explicit rationale: two tranches of
   zero-population scaffold, removed per the no-legacy-code
   invariant. The actual hole AV.0.5 left open: the
   `LargeAggregate` arena-backed payload variant exists
   (commit `e7add15`); the `colorFunction` / `colorFn` /
   `colorMix` grammar declarations exist (commit `ec20e99`,
   tuple-shaped `(u8 space, f64 c1, f64 c2, f64 c3, f64
   alpha)`); the emitter routing in `tape_prelude.rs` is
   ready (CO-E1's `aggregate_payload_ctor` switches on
   `total_bytes > 16`). What never landed: the layout pass
   refusing `TypeDesc::Named("Color")` at admission, so no
   rule actually drives the routing at runtime, and the
   view-layer accessor that projects the packed
   `LargeAggregate` blob into the typed shape
   `lightningcss::values::color::Color::RGBA { r, g, b, a }`
   compares against. AW W0.5 admits the Named CSS Color
   types to the layout pass and lands the view-layer
   projection. Per-backend type-table resolution per AU.4.2's
   stated path; no central IR registry.
6. **Inline `#[cfg(test)]` blocks under `src/` migrate to
   `tests/` directories.** The flagged violations in
   `crates/gorgeous/src/google_sheets.rs` and any siblings
   surfaced during W0 audit move to proper `tests/` files
   with no behaviour change. The `no-inline-tests` memory
   directive is enforced project-wide at AW close.
7. **Bootstrap regen is CI-enforced.** AW lands a CI step
   that diffs `crates/core/src/grammar/generated.rs` against
   a fresh `bash scripts/bootstrap-bbnf.sh` run on every PR
   into master. Hand-patch slips that flowed through V0
   close-out (and were caught by orchestrator-side `cargo
   expand` checks) become impossible.
8. **Workspace green at every wave boundary.** AV's wave-
   failure policy permitted intentional unworkability between
   V3 and V9. AW does not permit it. Each wave closes with
   `cargo test --workspace --no-fail-fast` reporting 0
   failures. The 13 serialize/structural roundtrip tests AV
   ignored un-ignore in W5; no test stays `#[ignore]` past
   AW close except the documented Category A items in
   FINAL.md.

## Cross-tranche debt — ledger reconciled

Items deferred across AR, AS, AT, AU, AV that AW closes:

| Item | Origin | Deferred through | AW phase |
|------|--------|------------------|----------|
| Colour-function `LargeAggregate` runtime consumer + view-layer Color projection | AV.0.5 (substrate landed; consumer never wired) | AV V5 close | **W0 Phase 0.5** |
| `find_next_structural_from` SIMD kernel + `scan_quoted_string_simd`/`decode_json_string_to_arena` paired migration | CO-E2 (V0.7) | AV V6+ | **W3 Phase 3.4** |
| `pinned_number_drops_f64_payload` (Sheets) | AV.0.3 | V0 close-out | **W2 Phase 2.5** |
| Sheets `boolean` FALSE branch drops `0u8` | AV CO-E4 | V5 close | **W2 Phase 2.5** |
| White-colour `0xFFFFFFFFu32` InlineScalar↔NONE collision | Agent C V0 (`60d4a70`) | V0 close-out | **W0 Phase 0.3** |
| 13 serialize/structural roundtrip regressions | V5 close (`ceb2764`) | V5 ignored | **W5 Phase 5.4** |
| 7 JSON variant-dispatch parity tests | AU.6.8 | V0 → V9 → V5 ignored | **W5 Phase 5.3** |
| 3 CSS percentage InlineScalar reader | AU.6.7 | V0 → V5 ignored | **W2 Phase 2.5** |
| `test_let_parses_as_let_call` (gorgeous in-src) | V0 close-out | V5 ignored | **W4 Phase 4.4 (Pratt) heals** |
| `test_selective_transitive_unfurling` | Pre-AV | AV.0.12 | **W5 Phase 5.5** |
| Inline `#[cfg(test)]` in `crates/gorgeous/src/` | Memory feedback | AV-noted | **W0 Phase 0.5** |
| Bootstrap regen CI gate | AV FINAL.md seeds | AV-noted | **W0 Phase 0.6** |
| `compute_sibling_skip` dead-code warning | V4 finaliser landing | AV close | **W1 Phase 1.5 deletion** |
| GrammarProfile slot calibration (`expected_ns_per_byte`, `parallel_break_even_bytes`, `payload_bytes_per_input_byte`) | AV V1 stub | AV V6 deferred | **W4 Phase 4.6** |
| Sonic-rs JSON-value parity harness | AU.3.2 | AV invariant 3 partial | **W5 Phase 5.6** |
| lightningcss CSS AST parity harness | AV.0.5 hard gate | AV V5 partial | **W5 Phase 5.6** |

## V0–V5 substrate overshoots — must clean before activation

These are not deferred items. They are *incorrect emitter
output* AV produced and the orchestrator caught at AV close
but did not clean. Letting the activation waves consume them
amplifies the cost. AW W0 cleans them in-tranche, before W1
dispatches:

- **Stage-C runs unconditionally.** Two extra O(N) tape scans
  per parse on the legacy path. `derive_frame_depth` produces
  data Stage-C uses to overwrite columns the parser already
  wrote correctly. (Diagnosed end of V5 session.)
- **Double Span pack in `__identifier` and similar.** Body
  match-arm packs `(lo, hi)` into `__aggregate_buf`; epilogue's
  `bare_span_epilogue_fixup` packs the same bytes again.
- **Always-true `if __has_payload`** in Span-rule epilogues.
  `__has_payload` is set unconditionally to `true` inside the
  body's success arm; the epilogue's `if` is dead control flow.
- **`mark_children` for leaf-route rules.** Assigned to
  `__children`, never read because `push_leaf_with` is the
  taken epilogue branch.
- **Per-rule `__aggregate_buf: [u8; 16] = [0u8; 16]` stack
  init** for Span rules with no aggregate need beyond the
  8-byte (lo, hi) pack. The 16-byte default sits on the stack
  consuming D-cache.
- **`compute_sibling_skip` dead-code warning** in
  `crates/bbnf-tape/src/columns.rs:283` — V2's method that
  Stage-C displaced; carries no callers in production code.
  (Suppression silently emits warning today; deletion lands
  alongside Stage-C's conditional activation.)

## Substrate-cost ledger

Each AV correctness fix added a mechanical per-instance or per-
parse overhead. The ledger maps each cost to the AW wave that
eliminates it (or the reason it stays). Triage aid for the
orchestrator: if a wave's bench checkpoint underperforms its
expected recovery, the ledger names the residual cost to chase.

| Cost | AV origin | Per-unit overhead | Eliminated by |
|------|-----------|-------------------|---------------|
| Span aggregate-buf 8B copy | AV.0.2 (Bug 2 leaf-payload route) | 1 store + 1 load per Span push (BBNF identifier/literal/regex/comment, Sheets cell_ref/identifier/string) | W1 — DTA writes columns in batch, no per-call buffer dance |
| Per-branch payload-write hoisting | AV.0.1 (Bug 1 alt-lit fix) | ~2 stores per Alt branch (was 1 first-only) | W3 — PHF + SIMD compare on dense keyword alts (CSS namedColor 148, *Keyword family); remaining structural alts keep cost as correctness |
| i64/f64 span-helper threading | AV.0.3 (Bug 2b) | `parse_{i64,f64}_from_bytes` per BBNF int_lit/float_lit | Stays (correctness invariant); AV.3.5's Eisel-Lemire short-circuit + 16-digit SIMD fastpath reduce per-call cost on the parse-that side |
| Scalar-Alt single-byte payload stamp | AV.0.1 close-out CO-E4 | 1 B store per Sheets `add_op`/`mul_op`/`unary_prefix` match | W4.6 — Pratt lowering removes the per-op wrapper compound; the stamp moves to the operator-precedence stack push |
| Empty-compound NONE compare | AV.0.6 (`push_compound` correctness) | 1 `child_off` compare per `push_compound` | W1 — DTA stage-A bypasses `push_compound` entirely (writes columns directly); the legacy push helper deletes alongside fn-per-rule |
| Stage-C double O(N) scan | AV V4 finaliser landing | 2 full-tape walks per parse on every grammar | W0.1 — conditional gate (skip while legacy path runs); W1.4 — unconditional with DTA-inline `frame_depth` |
| Double Span pack in `__identifier` | AV V0.2 emitter | 2 aggregate-buf writes per identifier match (body + epilogue) | W0.3 — single pack, epilogue authoritative |
| Always-true `if __has_payload` | AV V0.2 emitter | Dead branch per Span-rule epilogue | W0.3 — branch elision when emitter knows path is provably-only `push_leaf_with` |
| `mark_children` for leaf-route rules | AV emitter | Dead store + read per Span-rule prelude | W0.3 — prelude gate when epilogue is provably-only `push_leaf_with` |
| `__aggregate_buf [u8; 16]` over-allocation | AV emitter | 16 B stack + zero-init per rule (only 8 needed for Span; 1 for unit) | W0.4 — right-sized to layout `total_bytes`, padded to alignment |

The W0 elision targets the bottom four rows wholesale. W1 DTA
activation removes the next three (Span buffer dance, empty-
compound check, fn-per-rule call overhead). W3 keyword dispatch
collapses the per-branch hoist on dense alts. W4 Pratt
collapses Sheets operator wrappers. The two cost rows that stay
through AW close are the i64/f64 span-helper threading
(correctness — required for typed-AST parity) and the per-
branch payload writes on non-keyword alts (correctness — the
Bug 1 fix is non-negotiable). Both have measured per-unit cost
in the single-digit-cycle range and are bounded; they shape the
AW.6.2 bench-confirmation expected ceilings, not the wave
gates.

## Friction areas — operational lessons from AV

Not architectural items, but lessons the orchestrator carries
forward and the wave schedule encodes:

1. **API termination during long sub-agents** killed three
   V0 attempts at AV.0.2 plus the first av4-finaliser
   bbnf-tape test run. Wave prompts SHALL instruct sub-agents
   to commit at every milestone — not at end of work — and the
   orchestrator SHALL prefer narrower file bounds with smaller
   scope per agent.
2. **Bootstrap regen with stale `.bbnf-cache`** produced
   23-line truncated `generated.rs` outputs that cargo expand
   silently consumed. Every wave SHALL clear `.bbnf-cache`
   directories before bootstrap and verify line count > 24000
   before committing the regen.
3. **Conflict resolution on multi-agent shared files.**
   `bbnf-tape/src/lib.rs` and `crates/core/src/backend/rust/
   emitter/dta.rs` saw conflicts in V4 and V5 cherry-picks
   when two parallel agents added module declarations or
   interpolation sites. Wave prompts SHALL pre-declare
   alphabetical placement for module additions and named-token
   interpolation so cherry-picks merge without manual editing.
4. **Hand-patched generated files.** Agent B's first AV.0.2
   attempt produced a 25K-line hand-patched `generated.rs`
   that compiled clean but bootstrap regen failed. Agent
   prompts SHALL forbid `generated.rs` edits except via
   bootstrap output, and the W0 CI gate makes this
   structural.
5. **Test OOM during agent-poll loops.** av4-finaliser
   blocked on `cargo test -p bbnf-tape` consuming ~150 GB
   under a runaway test build. Wave prompts SHALL instruct
   agents to use `timeout 60 cargo test ... -- --test-
   threads=1 --skip <pathological>` rather than naked
   workspace runs when iterating.
6. **Bench-between-waves was absent in AV.** AV did not
   bench until V10, so the 2.5–4.5× regression that V0–V5
   accreted was invisible until tranche close. AW makes per-
   wave bench checkpoints structural to wave closure.

## Wave schedule

Eight waves, each closing with an agent-driven bench
checkpoint. Workspace green at every wave boundary; no
intentional unworkability. The W1 DTA driver activation is
the largest single lift; W2–W4 are parallel activations on
top of it.

| Wave | Parallel sub-agents | Workspace state | Bench gate |
|------|---------------------|-----------------|------------|
| **W0 — Cleanup + ABI finalisation + hygiene** (5 parallel) | (a) Stage-C conditional + dead-code deletion (AW.0.1, AW.0.2). (b) Span-rule emitter elision + IR-pass no-op fix (AW.0.3, AW.0.4, AW.0.10). (c) Colour-function `LargeAggregate` consumer + view-layer Color projection (AW.0.5). (d) Inline-test migration (AW.0.6). (e) Bootstrap regen CI gate + white-colour WideScalar routing + GrammarProfile stub-field ledger (AW.0.7, AW.0.8, AW.0.9). Wave opens with `post-AV-substrate-only.json` — a one-shot bench of master-as-AV-closed before any cleanup, captured by the bench agent at W0 dispatch as the reference for the W0 elision recovery measurement. | Green at W0 close. | **post-AW-W0.json** — recovers 25–40% of regression by elision alone; trajectory measurement, not gate. |
| **W1 — DTA runtime driver activation** (serial, single owner) | Single agent: AW.1.x replaces every grammar's `parse()` entry point with the DTA-driven stage-A walk. Legacy `fn __<rule>` deleted from the hot path; `__rule_kind()` dispatch retained for IR consumers. Stage-C activates unconditionally with DTA-emitted `frame_depth`. Includes AW.1.9 KvPair JSON `pair` activation verification (closes AT.1.3) and AW.1.10 pre-order tape verification (closes AV.2 substrate inheritance). | Green at W1 close — primary correctness gate. | **post-AW-W1.json** — gate: every entry ≥ post-AU baseline. |
| **W2 — PSI stage-B + ShapeRef + percentage closure** (3 parallel) | (a) PSI rayon stage-B activation per `parallel_break_even_bytes` (AW.2.1, AW.2.2). (b) ShapeRef runtime dispatch — `push_shape_ref` fires on `shape_hash` match (AW.2.3, AW.2.4). (c) Bug 2b residuals + Sheets boolean FALSE + percentage InlineScalar (AW.2.5). | Green. | **post-AW-W2.json** — gate: bootstrap ≥ 700 MB/s, twitter `decode_json_string` self-time < 5%. |
| **W3 — SIMD keyword dispatch + PHF + selector classifier** (4 parallel) | (a) PHF for CSS `namedColor` + Sheets function names (AW.3.1). (b) SIMD keyword compare for ≤ 16-keyword Alts (AW.3.2). (c) CSS selector classifier over structural bitmap (AW.3.3). (d) `find_next_structural_from` paired migration + remaining SIMD scanner holdouts (AW.3.4). | Green. | **post-AW-W3.json** — gate: `__compoundSelector` self-time < 15%; bootstrap ≥ 900 MB/s. |
| **W4 — Document-level parallel parse + bloom+GADT dedup + Pratt** (3 parallel) | (a) List-rule mining + chunk boundary detection + offset remap (AW.4.1, AW.4.2, AW.4.3). (b) Runtime bloom + GADT dedup gated on `dedup_eligible_rules` (AW.4.4, AW.4.5). (c) Pratt precedence-tower lowering for Sheets (and any grammar with chained operators) — heals `test_let_parses_as_let_call` (AW.4.6). Plus GrammarProfile slot calibration (AW.4.7). | Green. | **post-AW-W4.json** — gate: tailwind ≥ 1.2 GB/s on 4 cores; canada ≥ 1800 MB/s on 4 cores; sheets `parse_simple` ≥ 250 MB/s. |
| **W5 — Walker + reader migration + parity harnesses** (3 parallel) | (a) variant_idx walker coherence — un-ignore 7 JSON tests (AW.5.1). (b) 13 serialize/structural roundtrip un-ignore + fix (AW.5.2). (c) sonic-rs JSON-value parity harness + lightningcss CSS AST parity harness (AW.5.3, AW.5.4). Plus `test_selective_transitive_unfurling` triage (AW.5.5). | Green; ignored count = documented Category A only. | **post-AW-W5.json** — gate: every parity harness green; ignored count ≤ 5. |
| **W6 — Visitor API surface + bench parity confirmation** (2 parallel) | (a) Land `Tape::reduce_column<C, R>` plus the per-payload-column codegen specialisations on the columnar substrate; tests in `crates/core/tests/visitor_reduce.rs` exercise one reducer per grammar against a fixture (AW.6.1). (b) Bench confirmation matrix vs the post-AU baseline (AW.6.2); compose `post-AW.json`. | Green. | **post-AW.json** — every entry from the post-AV reality-check table meets its W6 gate. |
| **W7 — Tranche completion** (serial, no code changes) | Single agent: FINAL.md composition, post-AW.json publish, workspace test confirmation, deferred-item ledger reconciliation. | Green. | — |

**Bench-checkpoint contract.** Each `post-AW-W{N}.json` is
produced by a dedicated bench agent dispatched at wave close.
The agent runs the four parse-bench matrix cold, sequential,
file-output per `docs/instructions/README.md` §Benchmarking.
The agent is read-only on source code; its sole writes are
the JSON artefact and a one-paragraph commentary in
`docs/tranches/AW/PROGRESS.md` summarising what moved and
why. The orchestrator reviews the artefact against the wave
gate before dispatching the next wave. A wave whose bench
checkpoint shows regression against the prior checkpoint
without a written rationale is a violation; the wave reopens.

**Cross-wave invariants.** Master clean before each wave
dispatches. Every sub-agent commits at every natural
milestone (the V0 termination losses are the cautionary
tale). No file is written by two agents in the same wave.
Cherry-pick order is documented per wave in PROGRESS.md so
conflict resolution is reproducible.

## Phases

### Phase 0 — Cleanup, ABI finalisation, hygiene (W0)

W0 is the cost-elision wave. Every item below removes
incorrect emitter output, dead control flow, or carried-over
wart that the activation waves would compound. The wave is
five parallel agents because each item touches a disjoint
file set; the parallelism is genuine, not aspirational.

#### AW.0.1 Stage-C conditional activation

`TapeBuilder::finish` today calls `derive_frame_depth` then
`finalise` unconditionally. With the legacy fn-per-rule path
running, this is double-counting: the parser writes
`sib_skip` / `child_off` / `span_hi` correctly during
`push_compound`, then Stage-C overwrites them with derived
values that should match (test gate: `tape_parity` 22/22
holds, so the values DO match — the derive + sweep is purely
wasted work). Gate Stage-C activation on a `Tape::has_inline_
frame_depth` flag set only when the DTA emits stage-A
`frame_depth` directly. While the legacy path runs (W0
window), the flag is false, Stage-C is skipped, and the
parser's `compute_sibling_skip`-equivalent inline writes are
authoritative. After W1 deletes the legacy path, the flag is
permanently true and `compute_sibling_skip` deletes.

Hard gate: post-AW-W0 bench shows ≥ 15% improvement on every
entry attributable to Stage-C elision (verifiable via samply
self-time delta on `derive_frame_depth` + `finalise` —
expected to drop from ~10% combined to 0%).

#### AW.0.2 Dead V2 method deletion

`crates/bbnf-tape/src/columns.rs::compute_sibling_skip` is
unused in production after V4's Stage-C took its role; only
the V4 finaliser test references it. Deletion alongside
AW.0.1: the method goes; the test's reference V2 walk inlines
into the test file (where AV.4.4's bit-equality test already
keeps a copy under that exact name). Workspace `dead_code`
warning clears.

#### AW.0.3 Span-rule emitter elision

The emitted `__identifier` body packs `(lo, hi)` into
`__aggregate_buf` inside the body match-arm AND in the
epilogue via `bare_span_epilogue_fixup`. Pick one — the
epilogue is the natural home because it sees rule-final
`state.offset`. Drop the body-arm pack. Drop the always-true
`if __has_payload` branch in Span-rule epilogues (the body
unconditionally sets it on success; on failure the function
breaks `'rule_blk None` before the epilogue runs). Drop
`mark_children` from prelude when the rule's epilogue is
provably-only `push_leaf_with` (no `MustTape` branch).

This is a `crates/core/src/backend/rust/emitter/leaves.rs` +
`tape_prelude.rs` + `grammar.rs` change. The emitter knows
at codegen time whether a rule routes through `push_leaf_
with`-only or `push_compound`-only; the prelude/epilogue
selection becomes binary, not hedged.

Hard gate: bbnf_self bench ≥ +30% post-AW.0.3 isolation
(BBNF's hot path is dominated by identifier matches; the
elision compound).

#### AW.0.4 Stack-frame size reduction for Span-only layouts

Today every Span-bearing rule allocates `__aggregate_buf:
[u8; 16] = [0u8; 16]` even when the layout is a single Span
field needing 8 bytes. Size the buffer to the layout's actual
`total_bytes`, padded to 8-byte alignment. Most BBNF Span
rules drop to `[u8; 8]`; CSS unit rules drop to `[u8; 1]`;
colour-function rules stay `[u8; 40]`.

Stack-frame audit: deeply-nested CSS L4 rules (selector →
declaration → value → mathExpr → mathTerm chain) carry
nested aggregate buffers. A 5-level nesting at 16 B / level
costs 80 B per parse; at 8 B / level it costs 40 B. Per-
parse this is not the dominant cost, but D-cache pressure
across sibling rules in tight loops is.

#### AW.0.5 Colour-function `LargeAggregate` runtime consumer + view-layer Color projection

Not a Named-struct-registry restoration. The AS.2.3 / AT.6.1
StructRegistry approach was deleted in **AU.4.2** (commit
`ab8588a`) with explicit rationale: scaffold with zero
population, zero effective consumption across two tranches;
removed per the no-legacy-code invariant. AU.4.2 stated the
path forward: *"codegen handles struct projections via
per-backend type tables, not a central registry."* AV.0.5
took that path with `LargeAggregate` arena-backed payloads
plus tuple-shaped grammar projections (`colorFunction
-> (u8 space, f64 c1, f64 c2, f64 c3, f64 alpha)` in
`grammar/css/l4/color.bbnf`).

What landed in AV.0.5:

- `PayloadData::LargeAggregate(&[u8])` variant in
  `bbnf-tape` (commit `e7add15`) — arena-backed > 16 B
  aggregate, identical wire shape to `Bytes` minus the
  length prefix.
- Colour-function grammar declarations (commit `ec20e99`)
  — tuple types, not Named structs.
- Emitter routing in `tape_prelude.rs` via CO-E1's
  `aggregate_payload_ctor(total_bytes)` helper —
  `> 16 B → LargeAggregate`, `≤ 16 B → Aggregate`.

What never landed:

- `crates/ir/src/passes/payload/layout.rs::compute_payload_
  layouts` excludes `TypeDesc::Named("Color")` at admission
  (it is not a `TypeDesc::Tuple` shape, not a bare scalar,
  not currently admitted). No rule's `payload_layout`
  populates with the Color shape, so the emitter's
  `ctx.payload_layout.is_some()` path never fires for
  colour-function rules at runtime.
- The view-layer accessor that decodes a `LargeAggregate`
  byte blob into a typed Rust value matching
  `lightningcss::values::color::Color::RGBA { r: f32, g:
  f32, b: f32, a: f32 }`. Today `.view().as_color()` on a
  colour-function record returns the cursor span text
  (correct for source preservation, wrong for typed-AST
  parity).

W0.5 closes both:

1. **Layout pass admission for tuple-shaped Named types.**
   `compute_payload_layouts` admits `TypeDesc::Named(sid)`
   when the IR's per-backend type table resolves `sid` to a
   tuple of scalars. The grammar's `colorFunction -> (u8
   space, f64 c1, f64 c2, f64 c3, f64 alpha)` projects to
   `Tuple([U8, F64, F64, F64, F64])` via the per-backend
   resolver; layout planning reuses the existing scalar-
   tuple arm. No new `TypeDesc::Struct` variant; the
   existing `Tuple` path carries the payload and the
   accessor name is per-backend.
2. **View-layer Color projection.** `crates/core/src/
   backend/rust/view/color.rs` (new) carries the
   `pub struct Color { pub space: ColorSpace, pub c1: f64,
   pub c2: f64, pub c3: f64, pub alpha: f64 }` Rust-side
   projection plus `.view().as_color()` that decodes the
   `LargeAggregate` byte blob into the struct. The Rust
   struct lives in the BACKEND code, not in IR — per AU.4.2's
   per-backend type-table principle. Other backends (TS,
   WASM) get equivalent projections in their own backend
   modules without crossing IR boundaries.

Hard gate: `CssL4Parser::parse("rgb(255 128 0 / 0.5)").
view().as_color()` returns a backend-projected `Color {
space: ColorSpace::Rgb, c1: 255.0, c2: 128.0, c3: 0.0,
alpha: 0.5 }` whose field-by-field accessors match
`lightningcss::values::color::Color::RGBA { r: 255.0, g:
128.0, b: 0.0, a: 0.5 }` under the W5 parity harness.
AV.0.5's hard gate ("byte-equivalent to lightningcss")
satisfied via tuple-projection + per-backend view, as
AU.4.2 prescribed.

#### AW.0.6 Inline `#[cfg(test)]` migration

Project-wide audit + migration. The flagged offender is
`crates/gorgeous/src/google_sheets.rs::tests` (eight tests
inline). W0 moves the entire `mod tests` to
`crates/gorgeous/tests/google_sheets.rs`, restoring the
`no-inline-tests` invariant. The W0 audit greps `src/**/
*.rs` for `#[cfg(test)]\nmod tests` patterns and migrates
every match — the gorgeous offender is the known one; the
audit catches any siblings that crept in unnoticed.

Hard gate: `grep -rn '^#\[cfg(test)\]' crates/*/src/`
returns 0 matches outside doctest contexts.

#### AW.0.7 Bootstrap regen CI gate

`scripts/check-bootstrap-clean.sh` (new): runs `bash scripts/
bootstrap-bbnf.sh` against a fresh checkout, diffs
`crates/core/src/grammar/generated.rs` against the committed
version, and exits non-zero on any diff. Wired into
`.github/workflows/ci.yml` (or whatever the repo's CI
substrate is — to be discovered in audit).

Hand-patch slips are now CI-rejected. The V0 close-out
confusion (`r#as` token surprise from a stale-cache
expansion) becomes structurally impossible.

#### AW.0.8 White-colour `0xFFFFFFFFu32` InlineScalar↔NONE collision

Discovered Agent C V0 (`60d4a70` PROGRESS entry):
`PayloadData::InlineScalar(u32::MAX)` is indistinguishable
from `payload-absent` because `u32::MAX == TapeOffset::NONE`.
The `white = 0xFFFFFFFFu32` named-color value collides.

Fix: route `u32` payloads through `PayloadData::WideScalar`
when the rule's value range *includes* `u32::MAX`. The
emitter checks the rule's mined `MapExpr` value range at
codegen time; rules whose range cannot reach `u32::MAX` keep
`InlineScalar`. namedColor's range hits the sentinel only
for white, so the per-grammar promotion is small (CSS
namedColor → WideScalar across the board for uniformity).

Hard gate: `css_l4_named_color_parity::white_materialises`
(new test) passes.

#### AW.0.9 GrammarProfile stub-field population contract

V1's hard-gate 10 ("every per-grammar emitter constant has
moved into `GRAMMAR_PROFILE`") passed on the grep criterion
that `crates/core/src/backend/rust/emitter/` carries no
per-grammar `const X: &[u8]` constants. The profile struct
itself, however, has five array fields that V1 emits as
`&[]` for every grammar today: `active_columns`,
`list_rules`, `keyword_tables`, `shape_dict`,
`dedup_eligible_rules`. AW waves populate as they activate
(W2 → `active_columns` + `shape_dict`, W3 → `keyword_
tables`, W4.1 → `list_rules`, W4.5 → `dedup_eligible_
rules`), but no shared precondition ledger names the stub
state, so an interrupted wave's stub residue is invisible
to the hard-gate tally.

W0.9 is a ledger-only entry: `docs/tranches/AW/PROGRESS.md`
gains a "GrammarProfile population matrix" table updated at
each W2/W3/W4 wave close, listing which slots each grammar's
emitted profile populates and which remain `&[]`. No code
change at W0.9 itself; the contract is documentary, the
verification is per-wave.

Hard gate: PROGRESS.md carries the matrix; W6 close shows
zero `&[]` slots for any grammar except where the slot is
genuinely inapplicable (e.g. JSON has no `keyword_tables`
because it has no keyword Alts; record this as a
populated-by-design `&[]`, not a stub).

#### AW.0.10 `inline_acyclic` / `fuse_single_use` no-op fix

AU PROGRESS Session 1 flagged: both passes guard on
`r.meta.scc_id.is_none()` which is always `Some` during the
normalizer loop — the SCC pass populates `scc_id` before
the transform pass runs, so the guard rejects every rule.
Effective behaviour: zero rules inlined / fused; the passes
no-op silently. Source: `crates/ir/src/passes/transform/
inline.rs:23`, `crates/ir/src/passes/transform/fuse.rs:31`.

Consumers depend on the fused shape. The DTA lifter at
`crates/core/src/backend/rust/emitter/dta.rs:670` carries
the comment "post-fuse_single_use shape" — its expected
input is the post-fuse IR; it currently consumes the un-
fused IR because the fuse pass no-ops.

Fix: drop the always-true guard. The passes fire as
designed. Verify post-fix that DTA state count drops
measurably (CSS L4 today reports 2473 states; expected
drop after fuse_single_use actually fuses single-use
rules).

Hard gate: post-W0.10 `dta_run_unit_tests` show the
expected state-count reduction for CSS L4 (target: < 2000,
measured against the AV.3.6 PROGRESS-cited 2473 baseline).
If state count does not drop, root-cause investigation
required — the no-op fix should observably reduce DTA
table size.

### Phase 1 — DTA runtime driver activation (W1)

**Single-agent serial wave.** This is the largest single lift
in AW; the entire parse hot-path swaps from `fn __<rule>`
recursion to a DTA-driven stage-A walk. The wave is serial
because parallelism inside it produces merge conflicts
across `parse()` entry points that every grammar shares.

#### AW.1.1 DTA stage-A driver

`crates/bbnf-tape/src/driver.rs` (new) carries the runtime
DTA walker. The `DTA_TABLE` per-grammar `pub const` (AV.3.1)
is its input; the output is the populated `Columns`
substrate plus the `frame_depth: Vec<u8>` column for Stage-C.

The walker is one function:

```rust
pub fn dta_run(
    table: &DtaTable,
    profile: &GrammarProfile,
    input: PaddedView<'_>,
    columns: &mut Columns,
    psi: &mut PayloadStream,
    frame_depth: &mut Vec<u8>,
) -> Result<TapeOffset, ParseErr>
```

It dispatches via `trailing_zeros(structural_bitmap_mask)
→ src[offset] → Alt branch lookup`, manages the frame
counter stack (`[Frame; 64]` + heap overflow), emits one
`PayloadJob` per scalar leaf into `psi`, stamps `frame_
depth[i]` per record. No per-rule callable; the entire
parse is one loop body indexed by the current state.

#### AW.1.2 Generated `parse()` entry rewrite

`crates/core/src/backend/rust/emitter/grammar.rs::emit_grammar
_impl` emits a new `parse(input)` body that calls
`bbnf::runtime::tape::dta_run(&DTA_TABLE, &GRAMMAR_PROFILE,
state.padded(), &mut columns, &mut psi, &mut frame_depth)`,
then `psi_with_capacity(...).fill_columns(...)` runs stage B
(or stage-B parallel via `should_parallelise`), then
`finalise(&mut columns, &frame_depth)` runs stage C
unconditionally (the AW.0.1 flag is now permanently true).

#### AW.1.3 Legacy fn-per-rule deletion

Every `fn __<rule><'a>(state, tape) -> Option<TapeOffset>`
in `generated.rs` deletes — they are the consumer the new
`parse()` no longer calls. The IR-side `__rule_kind()`
dispatch retains (it's keyed by rule_id, not by callable);
that table is read by `bbnf-analysis` and the LSP at compile
time, not parse time.

`crates/core/src/backend/rust/emitter/{alt,leaves,map_value,
seq,repeat,binary,operator_chain,dispatch,ws,string_decode,
tape_prelude}.rs` — every per-rule emission helper that
produces fn bodies deletes alongside. The DTA emits its own
inline state transitions; the per-rule helpers were the
fn-per-rule support and have no remaining caller.

`emit_grammar_impl`'s output collapses to: rule kind enum,
view types, the visitor kernels, `pub const DTA_TABLE`, `pub
const GRAMMAR_PROFILE`, `pub const SHAPE_DICT`, the new
`parse()` entry. Generated.rs shrinks dramatically — likely
60–70% reduction in line count (today 28K lines; expected
~10K post-W1).

#### AW.1.4 Stage-C unconditional activation

The W0 `Tape::has_inline_frame_depth` flag flips
permanently true. `derive_frame_depth` deletes (DTA emits
frame_depth inline; deriving is dead work). `compute_
sibling_skip` (already deleted in W0) stays gone. `finalise`
runs every parse, consuming the DTA-emitted frame_depth
directly.

#### AW.1.5 PSI stage-B activation (sequential default)

PSI fill runs single-threaded by default at W1; the rayon
parallel path activates per-grammar at W4 calibration.
Sequential PSI is the W1 correctness gate: every typed leaf
the DTA emits a `PayloadJob` for has its decoded value in
the matching column after `fill_columns` returns.

#### AW.1.6 Visitor codegen survives

V2.5's `emit_visitor_kernels` continues to fire and is
generalised in W6 to emit one reordered-unrolling specialisation
per active payload column per grammar (no grammar-author
declaration needed). The kernel emission is unchanged at
substrate; the consumer changes from "any grammar whose
`mine_visitors()` returns non-empty" to "every grammar whose
`GrammarProfile::active_columns` includes a reducible payload
column." Rule-walker unaffected by the DTA driver swap.

#### AW.1.7 Replay substrate hooks (decision log + snapshot)

The DTA's flat state machine makes two adjacent
capabilities trivial to expose without committing to the
incremental-parsing infrastructure that consumes them. AW
ships the substrate; AX builds the consumer (incremental
re-parse, generalised error recovery, parse-step debugger,
test-case minimisation).

**Decision log.** The DTA driver accepts an optional
`decision_log: Option<&mut Vec<u8>>` sink. When provided,
the driver writes the low byte of each visited state ID
into the sink — one append per structural transition, ~1
byte per ~6–8 input bytes on typical grammars (so ~250 KB
for canada.json). Cost when `None`: a single
`Option::is_some` branch the optimiser hoists; effectively
zero. Replay = re-drive the DTA with the log as transition
oracle instead of consulting the byte stream + structural
bitmap; produces a bit-identical tape.

**Snapshot format.** A `pub struct DtaSnapshot { frame_
stack: SmallVec<[Frame; 64]>, depth: u8, counter_regs:
SmallVec<[u32; 16]>, byte_offset: u32 }` captures the
DTA's full state at any byte offset. Resume = pass the
snapshot back into the driver; parsing continues from
exactly that point. Snapshot-and-resume is O(stack depth) —
typical depth ≤ 8 for JSON, ≤ 12 for CSS L4, ≤ 6 for
Sheets, ≤ 10 for BBNF.

Both hooks are feature-gated behind `dta-replay` (default
off) so production builds carry zero cost. The on-by-
default tape and `parse()` API are unchanged.

#### AW.1.8 Packrat memo retirement

`ParserState::memo` (parse-that) was per-parse, dropped
each invocation, never reused across parses; no production
consumer relies on it. The DTA driver carries no memo —
the counter-DFA is deterministic over the input; packrat
caching is structurally unnecessary. AW.1.8 deletes the
`MemoStore` field from `ParserState` and the `memo`-related
plumbing. The `parse-that` test suite that exercises memo
storage either ports to the DTA's deterministic-replay
fixture or deletes alongside.

#### AW.1.9 KvPair JSON `pair` activation verification (AT.1.3 closure)

AT.1.3 planned `KvPair` shape activation for JSON's `pair`
rule. The substrate landed: `is_kv_pair_shape` exists in
`crates/ir/src/passes/payload/layout.rs`; the emitter
consumes the layout when `compute_payload_layouts` admits
the rule. But `grep KvPair crates/core/src/grammar/
generated.rs` returns zero matches at HEAD — the layout
admission excludes `pair` because its shape doesn't reach
`is_kv_pair_shape`'s `(Span, scalar)` recogniser today
(JSON `pair = string : value` projects as
`Tuple([Span, BoxedEnum])`, not `Tuple([Span, scalar])`).

W1.9 verifies post-DTA-driver landing: `KvPair` should
fire for JSON `pair` (and Sheets `key_value` if the
pattern surfaces). If the layout admission still excludes,
W1.9 widens `is_kv_pair_shape` to admit `Tuple([Span,
BoxedEnum])` provided the BoxedEnum's value range
satisfies the KvPair payload constraint, OR documents why
the exclusion remains correct (i.e., AV's `LargeAggregate`
+ ShapeRef path is the right substitute for JSON pairs and
KvPair is a CSS-only optimisation post-AV).

Hard gate: post-W1.9 `grep -c 'TapeKind::KvPair'
crates/core/src/grammar/generated.rs` returns ≥ 1 (JSON
`pair` fires KvPair) OR PROGRESS.md carries the written
rationale that JSON `pair` is the wrong fit for KvPair and
the AT.1.3 item retires.

#### AW.1.10 Pre-order emission verification (AV.2 substrate inheritance)

AV.2's substrate keeps post-order emission — parents
emit after children — so `cursor.rs::child(0)` retains a
bounded backward walk to seed first-child-from-parent.
AW.1.1's DTA driver emits records in stage-A order
(structural skeleton in pre-order natively); the SoA
column writes go in pre-order naturally because the DTA
walks the input forward.

W1.10 verifies post-DTA-emit that the tape shape IS
pre-order (parents precede their children's tape offsets)
and the cursor's first-child accessor degrades to the O(1)
`idx + 1` lookup AV.md originally specified. If the DTA
driver inherits AV.2's post-order convention, document
that as the design decision and route the `idx + 1`
optimisation to AX (the bounded backward walk is correct,
just not maximally efficient).

Hard gate: `cursor.rs::first_child_complexity_test` (new)
asserts O(1) behaviour OR PROGRESS.md documents the
post-order inheritance with rationale.

Hard gates:

- `grep -cE 'fn __[a-zA-Z_]+<' crates/core/src/grammar/
  generated.rs` returns 0 outside the prettify path
  (delete every rule fn from the hot path).
- `wc -l crates/core/src/grammar/generated.rs` returns ≤
  12000 (down from 28K).
- `cargo test --workspace --no-fail-fast` 0 failures.
- bench gate: every entry from the post-AV.json baseline
  matches or exceeds the post-AU baseline. The DTA win is
  the lever that recovers the regression in one stroke.

### Phase 2 — PSI rayon + ShapeRef + Bug 2b residuals (W2)

#### AW.2.1 PSI rayon stage-B activation per grammar

`GrammarProfile::parallel_break_even_bytes` carries the
per-grammar threshold today as `0` (gate-closed). W2.1
calibrates the threshold per grammar from a fresh samply
profile run on a representative input:

- JSON canada: ~50 KiB break-even (large numeric-dense input)
- CSS bootstrap: ~100 KiB
- CSS tailwind: ~50 KiB
- Sheets parse_stress: stays sequential (small inputs)
- BBNF self: stays sequential

Calibration agent reads `.profiles/samply/<bench>/<entry>/
profile.json.syms.json`, computes the crossover point where
rayon overhead < parse savings, writes the value into the
emitter's `GrammarProfile` literal, regen.

Hard gate: canada bench shows per-core scaling on a 4-core
machine for inputs ≥ break-even (not super-linear — memory
bandwidth bound).

#### AW.2.2 PSI lock-free column writes

PSI rayon today writes into pre-resized columns at distinct
`column_idx` positions per worker (false-sharing-free per
the design note in `psi.rs:54-65`). W2.2 verifies the
contract holds under stress: a tortured-input test with
4-core parallel fill where adjacent records land on the same
cache line confirms no torn writes.

#### AW.2.3 ShapeRef runtime dispatch — DTA stage-A integration

The DTA's stage-A loop checks each compound emit against the
grammar's `SHAPE_DICT` via `shape_hash` comparison. On match
→ `push_shape_ref(span, dict_idx, packed_payload)` instead
of `push_compound(...)` + child run. On miss → normal
skeleton. The check is one indexed read into the dict's
shape-hash array and one equality compare; the cost is
amortised by the record-count reduction (5–7 records → 1).

The dict consultation lives in the DTA driver's compound-
emit branch (the AW.1.1 walker). Bootstrap.css's 5000+
declaration subtrees collapse into ShapeRef leaves; tape
records drop accordingly.

#### AW.2.4 ShapeDict view-layer parity

The view layer's `ShapeRefSyntheticChild` cursor expansion
(AV.5.1) lazily synthesises the structural children at read
time. W2.4 verifies parity: the same view-layer call against
a ShapeRef and against the equivalent normal compound
returns byte-identical typed-AST projections.

Hard gate: a dedicated `shape_ref_view_parity` test compares
ShapeRef-emit vs. compound-emit for every CSS L4 declaration
in bootstrap.css; zero divergences.

#### AW.2.5 Bug 2b residuals

Three Sheets/CSS items deferred from AV V0:

- `pinned_number_drops_f64_payload` (Sheets `number -> f64`):
  Map-bodied regex rule needs admission to the layout pass.
  Today `scalar_layout_eligible` requires `IrNode::Alt`;
  extend to admit Map-bodied rules whose body is a regex
  match producing a typed payload. Gate carefully — the
  prior agent (CO-E4) noted this risks BBNF `int_lit`
  regression because the regex emitter's layout path only
  handles F64. W2.5 extends the regex-emitter to also
  handle I64 / U64 / Bool / U8 single-scalar Map bodies,
  then admits them.
- Sheets `boolean` FALSE branch drops `0u8`: dispatch
  composer requires literal-branch Alts; `boolean` uses
  regex-branch (`/TRUE/i`, `/FALSE/i`). Extend the
  dispatch composer to recognise regex-constant-Map
  branches (`Map { Regex, BoolLit }`).
- 3 CSS percentage InlineScalar reader: `payload_u8` reader
  call sites in `crates/core/tests/css_l4_parity.rs`
  un-ignore. The src-side scanner→payload wiring for `%`
  literal emits `255u8` (already in `value-unit.bbnf`); the
  3 ignored tests confirm reader migration works post-DTA.

Hard gate: every previously-pinned `pinned_*_drops_payload`
flips; the 3 percentage tests un-ignore; all parity suites
green.

### Phase 3 — SIMD keyword dispatch + selector classifier (W3)

#### AW.3.1 PHF for CSS `namedColor` + Sheets function names

CSS `namedColor` (148 entries) → `phf::OrderedMap<&'static
[u8], u32>`. Emitted as `pub const NAMED_COLOR_PHF: phf::Map
= phf_map! { ... };` in the grammar's `generated.rs` via the
emitter's keyword-dispatch path (`crates/core/src/backend/
rust/emitter/keyword_dispatch.rs`, new).

CSS `*Keyword` rules (positionKeyword, overflowKeyword,
etc.) → PHF each.

Sheets function names (`SUM`, `AVG`, `IF`, `LET`, ~150
total) → PHF.

The DTA's Alt dispatch consumes the PHF directly: branch
selection for namedColor becomes one PHF lookup instead of
the 148-branch linear scan or the prior factor-pass byte-
dispatch. White-colour collision (AW.0.8) routes via
WideScalar so PHF stays homogeneous.

#### AW.3.2 SIMD keyword compare for ≤ 16-keyword Alts

CSS `colorType` (9 entries: oklch / oklab / rgba / rgb /
hsla / hsl / hwb / lab / lch). Pack all 9 keywords into one
128-bit NEON register (9 × 8-byte slots, padded). One
parallel 8-byte-lane compare emits a match bitmask;
`trailing_zeros` picks the branch index. Hits the typed u8
discriminant via the V0 Bug-1 fixed per-branch payload
emission.

Same pattern for BBNF `__directive` (8 entries: `@import`,
`@recover`, `@pretty`, `@ws`, `@token`, `@debug`, `@host`,
`@extern`).

#### AW.3.3 CSS selector classifier over structural bitmap

The AU.2.7 structural bitmap names every structural
character in the input. A selector classifier on top of it
reads the bitmap's positions and dispatches the selector's
classified type (`.class`, `#id`, `tag`, `[attr]`,
`:pseudo`, `::elem`, `>combinator`) in one pass. Replaces
the compound-heavy byte-level alt dispatch in
`__compoundSelector` (33–43% self-time pre-AU; expected to
drop below 15% post-W3.3).

Classifier lookup table: 256-entry byte-to-selector-kind
LUT, consumed by the DTA at compound-selector positions.

#### AW.3.4 `find_next_structural_from` paired migration

CO-E2 (V0.7) deferred this: 7 emitter call sites pass
`&state.src_bytes` (unpadded). Migrate to `PaddedView` with
a coordinated bbnf-lang codegen update at
`crates/core/src/generate/regex/emit/simd.rs`.

Plus the `scan_quoted_string_simd` / `decode_json_string_to_
arena` paired migration (both kernels move to PaddedView,
emitter at `crates/core/src/backend/rust/emitter/string_
decode.rs:115` updates `state.src_bytes` → `state.padded()`).

Last per-chunk SIMD bounds guard remaining in the hot path
clears.

Hard gates:

- `__compoundSelector` self-time < 15% on bootstrap +
  tailwind via samply.
- `grep -rn 'const [A-Z_]*: \[&\[u8\]'
  crates/core/src/backend/rust/emitter/` returns 0 — every
  keyword table routes through PHF or SIMD compare emitted
  from `GrammarProfile::keyword_tables`.
- bench gate: bootstrap ≥ 900 MB/s; tailwind on 4 cores ≥
  1.4 GB/s.

### Phase 4 — Document-level parallel parse + bloom dedup + Pratt (W4)

#### AW.4.1 List-rule identification

`crates/ir/src/passes/recognizers/list_rules.rs` (new). A
rule is a fork candidate iff:

- body is a Repeat over an Alt or a single compound rule,
- children carry no cross-item state (first-set check over
  all alternatives),
- each item's byte extent is bounded by a structural-bitmap
  position (every item starts at a known byte class).

Candidates emitted to `GrammarProfile::list_rules`. Targets:
CSS `stylesheet = (ruleset | at_rule)*`, JSON root `value`
when array/object, BBNF `grammar = rule+`, Sheets `file =
formula_line*`.

#### AW.4.2 Chunk boundary detection

The stage-1 structural bitmap marks every ruleset / array-
element / rule / formula-line boundary; workers take
contiguous bitmap regions. Boundaries align to the
structural alphabet, not byte count — preserves parser
correctness across boundaries.

#### AW.4.3 Tape offset remap at join

Each worker writes into a local `Columns` instance; the
join phase concatenates (memcpy each column in order) and
rewrites all `sib_skip` cross-worker references by the
worker's contribution offset. One linear pass per column.

Activation gated by `parallel_break_even_bytes` (W2.1
calibrated). Tailwind.css forks; bootstrap.css may or may
not (empirical break-even ~ 4 cores × 50 KB per chunk).

#### AW.4.4 Bloom + GADT runtime dedup

Layered over the DTA's stage-A emit per AV.md §Phase 6
(routed forward as AW V8). Mandatory where `GrammarProfile::
dedup_eligible_rules` is non-empty (CSS `compoundSelector`,
`identifier`, `namedColor`-wrap, fixed unit suffixes; JSON
`null`, `true`-branch, `emptyObject`, `emptyArray`; BBNF
literal-only Alt branches).

`crates/bbnf-tape/src/dedup.rs` (new) carries the GADT +
bloom infrastructure. The bloom is the admission gate; on
hit, GADT lookup → `columns_range_eq` confirms; on confirm,
`push_compound_referring(rule_id, existing, span)` shares
the existing subtree.

`hash_children_tail` is a 64-bit rolling FNV over the raw
column bytes of the child records. Span_lo/span_hi ignored
for structural rules (two `border: 0` declarations at
different file positions are structurally identical).

#### AW.4.5 Dedup eligibility IR pass

`crates/ir/src/passes/recognizers/dedup_eligibility.rs`
(new) classifies each rule. The classifier uses existing
IR facts: `TypeDesc`, `EClassFacts.closure_free`,
`EClassFacts.all_descendants_elidable`. No new IR data.

Populates `GrammarProfile::dedup_eligible_rules`.

#### AW.4.6 Pratt precedence-tower lowering for Sheets

Sheets' six-level left-recursive chain (`__formula →
__comparison_expr → __concat_expr → __add_expr →
__mul_expr → __exp_expr → __unary_expr`) — DTA shunting-
yard substrate (AV.3.3) emits the precedence table; W4.6
wires the runtime consumer.

The `__expr` dispatch becomes a single Pratt loop over the
operator-precedence byte-LUT, emitting one `push_compound`
per operator that fires — zero redundant tower-wrappers.

Pattern generalises to CSS value expressions and BBNF
binary expressions; the Sheets case is the canonical first
landing.

Heals `test_let_parses_as_let_call` (Sheets dispatch
surface naturally touched by Pratt lowering).

#### AW.4.7 GrammarProfile slot calibration + small-input amortisation

Calibrate per-grammar `expected_ns_per_byte`, `parallel_
break_even_bytes`, `payload_bytes_per_input_byte` against
the post-W4 single-threaded measurement matrix; commit the
chosen values into `GrammarProfile` so downstream waves
have stable comparison points.

Sub-item: **single-threaded setup-cost ceiling.** Sub-100 µs
parses (Sheets `parse_simple` ~5 µs, BBNF `json` ~6 µs)
amortise the DTA's frame-stack init + `Columns::new` + PSI
allocator init over very few bytes. Calibration commits a
per-grammar `dta_setup_floor_ns` constant derived from a
zero-input parse measurement; the W4 expected MB/s for
small inputs is `(input.len() × 1e9) / (dta_setup_floor_ns
+ input.len() × expected_ns_per_byte)`. The W4 bench
checkpoint compares against the formula, not against a
fixed gate, for inputs below the small-input threshold.
This prevents W4 from chasing a bench gate that's
mathematically unreachable while still catching genuine
regressions on small workloads.

Calibrate the V1-stub fields from samply data:

- `payload_bytes_per_input_byte` per grammar from PSI fill
  measurement.
- `expected_ns_per_byte` from per-bench cold parse time.
- `parallel_break_even_bytes` from W2.1 PSI calibration.

These ride forward as bench-derived constants in the
`GRAMMAR_PROFILE` const literal; no runtime cost.

Hard gates:

- Tailwind.css cold parse ≥ 1.2 GB/s on 4 cores.
- JSON canada ≥ 1800 MB/s on 4 cores.
- Sheets `parse_simple` ≥ 250 MB/s.
- bootstrap.css combined (ShapeDict + bloom + GADT) tape
  record reduction ≥ 30% vs. post-W3 column baseline.
- canada.json (zero-sharing input): bloom-AND steady-state
  overhead < 2% of parse time.

### Phase 5 — Walker + reader migration + parity harnesses (W5)

#### AW.5.1 variant_idx walker coherence — un-ignore 7 JSON tests

AU's 7 ignored JSON variant-dispatch tests (`json_parity`)
assume AoS + payload_idx semantics that the V2 columnar
substrate replaced. Walker migration updates the dispatch
surface; tests un-ignore and pass.

The fix is in `crates/core/src/backend/rust/view/alt.rs` —
the cursor's variant_idx accessor reads from `flags`
column, not from the (deleted) `payload_idx` field.
Mechanical reader migration.

#### AW.5.2 13 serialize/structural roundtrip un-ignore + fix

V5 close (`ceb2764`) ignored these with explicit AW V6+
forward-tickets. W5.2 fixes them:

- 5 JSON `json_*` serialize tests (json_array, json_nested,
  json_empty_arr, json_object, json_empty_obj)
- 7 structural `structural_*` tests (object_with_array,
  array_three_numbers, object_two_pairs, nested_objects,
  empty_array, empty_object, data_json_sanity)
- 1 BBNF `bbnf_rule` test

The serialize_emit path was correct pre-V0 but regressed as
V0–V5 added Span aggregate writes etc. on the legacy hot
path. W1's DTA driver replaces the legacy path; W5.2
verifies the serialize-emit path against the DTA-emitted
tape and updates the serialize logic if the tape shape
shifted.

Un-ignore. All 13 tests pass.

#### AW.5.3 sonic-rs JSON-value parity harness

`crates/core/tests/sonic_rs_parity.rs` (new). For every
JSON file in `data/json/`, parse with bbnf and with sonic-
rs, compare the typed-AST projections value-by-value:

- `view().as_value()` returns a `bbnf::Value` whose tree
  matches `sonic_rs::Value` node-for-node.
- Numbers compare bit-for-bit (f64 ULP tolerance).
- Strings compare byte-for-byte (escape decoding included).
- Objects compare key-set equality + per-key value
  equality.
- Arrays compare length + per-index value equality.

Zero divergences permitted on canada / twitter / citm /
data / data_xl.

#### AW.5.4 lightningcss CSS AST parity harness

`crates/core/tests/lightningcss_parity.rs` (new). Per-
declaration equivalence over bootstrap.css + tailwind.css:
parse with lightningcss, parse with bbnf, compare typed
declarations declaration-by-declaration. Color values
compare via the W0 `pub struct Color` projection;
selectors compare via tokenised form.

Zero divergences permitted.

#### AW.5.5 `test_selective_transitive_unfurling` triage

The pre-AV deferred `imports.rs` test. W5.5 either fixes
or documents — if the import-system bug is genuinely
orthogonal to AW scope, the test stays `#[ignore]` with a
ticket pointing at a dedicated imports-subsystem pass for
the next tranche. If fixable in W5 scope, fix.

#### AW.5.6 Parity harness CI gate

The two new parity harnesses (W5.3, W5.4) gate CI alongside
`grammar_roundtrip` + `tape_parity`. Future tranches that
break parity fail CI, not just the local test run.

Hard gates:

- 0 failures in `cargo test --workspace --no-fail-fast`.
- ignored count ≤ 14, comprising the enumerated Category A
  set:
  - `test_selective_transitive_unfurling` — imports-
    subsystem bug, AW.5.5 disposition (fix in W5 if scope
    permits, otherwise route to AX as standalone tranche).
  - 5 closure tests (`closure_*_param`, `lower::expression`
    gap) — language-feature scope, route to AX.
  - 4 analysis structural-mode gates (cycle/alias detection,
    diagnostics) — analysis-subsystem scope, route to AX.
  - 3 gorgeous dump tests (non-checked-in fixtures) —
    fixture-side, the tests reference snapshot files not
    in-repo. Either commit the fixtures or delete the
    tests; W5 audit decides per-test.
  - 2 pprint-vm hint tests (softbreak/indent_group drift) —
    pprint-vm scope, route to AX.

Every AV-routed forward-ticket lands in an AW phase: the 7
JSON variant-dispatch tests un-ignore at AW.5.1; the 13
serialize/structural roundtrip tests un-ignore at AW.5.2;
the 3 CSS percentage InlineScalar tests un-ignore at
AW.2.5; the Sheets Bug-1 inline tests un-ignore at AW.2.5
or via Pratt at AW.4.6; `test_let_parses_as_let_call`
heals at AW.4.6. Anything still ignored after W5 close is
a Category A item per the enumerated list above.

### Phase 6 — Visitor production wiring + bench parity (W6)

#### AW.6.1 Visitor API on the columnar `Tape`

V2.5's reordered-unrolling kernels reach end users via a
Rust-side API on the columnar `Tape`, not via a grammar-
author directive. The visitor invocation is a method call
the consumer writes against the parsed tape:

```rust
let total: f64 = parsed.tape().reduce_column::<F64Column, _>(
    0.0,
    |acc, x| acc + x,
);
```

The codegen emits one `reduce_column<C, R>` impl per active
payload column per grammar (driven by `GrammarProfile::
active_columns`); LLVM monomorphises the reducer at the call
site, producing the V2.5 4-lane reordered-unrolled loop. No
proc-macro on the consumer side; no grammar surface; no new
BBNF directive. The kernel was always grammar-agnostic — it
needs the column type and the reducer, both of which the
Rust call site supplies.

Test surface: `crates/core/tests/visitor_reduce.rs` exercises
one reducer per grammar against a fixture — JSON sum-all-f64
on canada.json; CSS count-all-declarations on bootstrap.css;
BBNF count-all-rules on bbnf_self.bbnf; Sheets sum-all-cell-
refs on stress.txt — proving the kernel reaches end users
through the new API and matches the V2.5 microbench
performance ceiling.

The choice keeps the AV invariant 5 alignment: codegen
specialised from `GrammarProfile::active_columns` (a
fingerprint output, not a grammar annotation); consumer
ergonomics through Rust generics; zero grammar-author surface
introduced by AW.

#### AW.6.2 Bench parity confirmation

A dedicated bench agent runs the four parse-bench matrix
cold, sequential, mimalloc, cwd `crates/core`. Every entry
from the post-AV reality-check table is checked against the
W6 gate. Results land in `docs/benchmarks/post-AW.json`.

Each entry's MB/s ≥ its W6 gate or carries a written
explanation. The agent's report includes per-entry samply-
based attribution: which W1–W4 lever delivered the win,
which residual costs remain.

Hard gate: every gate met.

#### AW.6.3 Visitor SIMD-packed 6× gate (AV.2.5 closure)

AV.2.5 measured 3.3× scalar-left-fold-free speedup on
synthetic `Vec<f64>`. AV FINAL hard-gate 12 was partial:
the plan's 6× SIMD-packed target was unmet at AV close
because portable `f64x4` packing was not wired into the
emitted kernel. AW.6.3 closes it.

The 4-lane reordered accumulator the visitor codegen emits
already breaks the strict-IEEE left-fold dependency chain;
LLVM auto-vectoriser produces four independent scalar
`fadd d*` chains on AArch64. Promotion to packed
`std::simd::f64x4` (or arch-intrinsic `vfadd.2d` x 2) is a
mechanical change to `crates/core/src/backend/rust/emitter/
visitor.rs::emit_visitor_kernels`: the inner loop body
swaps from 4 independent scalar adds to one `f64x4` SIMD
add per stripe. portable_simd is stable; no nightly
dependence required.

Verified against AW.6.1's `reduce_column<F64Column,_>` API
on a representative dense-numeric input (canada.json's
`f64` column, ~6M entries):

```bash
cargo bench -p bbnf --bench visitor_reduce_simd \
  -- --measurement-time 10
```

Hard gate: ≥ 6× speedup over the AV.2.5-baseline scalar
left-fold on the canada.json `f64` column. If portable
`f64x4` lowering on AArch64 cannot clear 6× (the AV.md
projection was based on x86_64 AVX2 measurements; AArch64
NEON has 2-lane f64 width), document the per-arch ceiling
and route the 6× target to AX with arch-specific
intrinsic kernels (`vfaddq_f64` pairs on NEON, AVX2
`_mm256_add_pd` on x86_64). Either ≥ 6× lands or the gate
formally retires per-arch with measurement evidence.

### Phase 7 — Tranche completion (W7)

No code changes. `FINAL.md` composition with full per-phase
recap, hard-gate status table, invariant verification,
cross-tranche debt ledger reconciled, future-work seeds for
AX (the next tranche after AW). `post-AW.json` is the W6
artefact, finalised. `cargo test --workspace --no-fail-
fast` confirmation. Workspace test confirmation per
`docs/instructions/README.md`'s tranche-completion
requirements.

## Critical files

| File | Phase |
|------|-------|
| `crates/bbnf-tape/src/columns.rs` (Stage-C conditional + dead method deletion) | 0, 1 |
| `crates/bbnf-tape/src/builder.rs` (Stage-C activation flag) | 0, 1 |
| `crates/core/src/backend/rust/emitter/leaves.rs` (Span elision) | 0 |
| `crates/core/src/backend/rust/emitter/tape_prelude.rs` (always-true branch elision, mark_children prelude gate) | 0 |
| `crates/core/src/backend/rust/emitter/grammar.rs` (parse() rewrite, fn-per-rule deletion) | 1 |
| `crates/core/src/backend/rust/emitter/{alt,map_value,seq,repeat,binary,operator_chain,dispatch,ws,string_decode}.rs` (per-rule helpers delete) | 1 |
| `crates/bbnf-tape/src/driver.rs` (**new** — DTA stage-A walker; `dta-replay` feature exposes `decision_log` + `DtaSnapshot`) | 1 |
| `parse-that/rust/parse_that/src/state.rs` (`MemoStore` deletion) | 1 |
| `crates/ir/src/types/type_desc.rs` (TypeDesc::Struct admission) | 0 |
| `crates/ir/src/passes/payload/layout.rs` (Map-bodied scalar admission) | 2 |
| `crates/bbnf-tape/src/dedup.rs` (**new** — bloom + GADT) | 4 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (**new**) | 4 |
| `crates/ir/src/passes/recognizers/dedup_eligibility.rs` (**new**) | 4 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (**new** — PHF + SIMD compare) | 3 |
| `crates/core/src/backend/rust/emitter/selector_classifier.rs` (**new** — CSS selector classifier) | 3 |
| `crates/core/src/grammar/generated.rs` (DTA-driven, ~10K lines) | 1, 2, 3, 4, 6 |
| `grammar/css/l4/color.bbnf` (Color / ColorMix struct projection) | 0 |
| `crates/bbnf-tape/src/columns.rs` (`Tape::reduce_column<C, R>` API + per-column codegen specialisations) | 6 |
| `crates/core/tests/visitor_reduce.rs` (**new** — one-reducer-per-grammar fixture suite) | 6 |
| `crates/core/tests/sonic_rs_parity.rs` (**new**) | 5 |
| `crates/core/tests/lightningcss_parity.rs` (**new**) | 5 |
| `crates/core/tests/{json,structural}_parity.rs` (un-ignore + fix) | 5 |
| `scripts/check-bootstrap-clean.sh` (**new** — CI gate) | 0 |
| `crates/gorgeous/tests/google_sheets.rs` (**new** — inline-test migration target) | 0 |
| `docs/tranches/AW/{PROGRESS,FINAL}.md` + `docs/benchmarks/{post-AV-substrate-only,post-AW,post-AW-W0,post-AW-W1,post-AW-W2,post-AW-W3,post-AW-W4,post-AW-W5}.json` | 0–7 |

## Hard gates summary

### W0 — Cleanup

1. Stage-C conditional gate lands; runs only when DTA emits frame_depth.
2. `compute_sibling_skip` deleted; `dead_code` warning clears.
3. `__identifier`-style rules emit single Span pack (epilogue), not double.
4. Always-true `if __has_payload` elided in Span-only rule epilogues.
5. `mark_children` skipped in prelude when epilogue is provably-only `push_leaf_with`.
6. `__aggregate_buf` sized to layout's `total_bytes`, not fixed 16.
7. `pub struct Color` / `pub struct ColorMix` lands; `TypeDesc::Struct(StructId)` admitted.
8. `grep -rn '^#\[cfg(test)\]' crates/*/src/` returns 0.
9. `scripts/check-bootstrap-clean.sh` lands in CI; PRs that skip bootstrap regen fail.
10. White-colour collision routed via WideScalar; `white_materialises` test passes.
11. PROGRESS.md carries the GrammarProfile population matrix (AW.0.9); each W2/W3/W4 wave's profile-population contract enumerated.
12. `inline_acyclic` / `fuse_single_use` always-true guard dropped; CSS L4 DTA state count drops measurably from the AV.3.6 baseline of 2473.
13. **post-AW-W0.json** shows ≥ 25% recovery on the post-AV regression vs. four-bench matrix (trajectory measurement, not bench gate).

### W1 — DTA driver activation

14. `grep -cE 'fn __[a-zA-Z_]+<' crates/core/src/grammar/generated.rs` returns 0 outside prettify.
15. `wc -l crates/core/src/grammar/generated.rs` ≤ 12000.
16. `cargo test --workspace --no-fail-fast` 0 failures, ignored count unchanged from W0.
17. **post-AW-W1.json**: every entry ≥ post-AU baseline (small inputs amortised per AW.4.7 formula once W4 calibration lands).
18. `dta-replay` feature builds clean; with the feature on, `dta_run` accepts a `decision_log` sink and a `DtaSnapshot` resume entry; with the feature off, the `Option`-typed sink hoists out of the hot loop (asm-verified zero overhead).
19. `ParserState::memo` and `MemoStore` deleted from `parse-that`; no production consumer remains.
20. AW.1.9: `grep -c 'TapeKind::KvPair' crates/core/src/grammar/generated.rs` ≥ 1 (JSON `pair` fires KvPair) OR PROGRESS.md carries the AT.1.3-retire rationale.
21. AW.1.10: cursor first-child accessor degrades to O(1) `idx + 1` lookup post-DTA OR PROGRESS.md carries the post-order inheritance rationale routing the optimisation to AX.

### W2 — PSI rayon + ShapeRef + Bug 2b

16. `parallel_break_even_bytes` per-grammar-calibrated; PSI rayon path activates per fingerprint gate.
17. `__compoundSelector` consults SHAPE_DICT at stage A; bootstrap.css declaration record count drops ≥ 30%.
18. ShapeRef view-layer parity test passes (zero divergences).
19. `pinned_number_drops_f64_payload` flips; Sheets `boolean` FALSE branch fires; 3 CSS percentage tests un-ignore.
20. **post-AW-W2.json**: bootstrap ≥ 700 MB/s; twitter `decode_json_string` self-time < 5%.

### W3 — SIMD keyword + selector classifier + scanner closure

21. `__compoundSelector` self-time < 15% via samply.
22. `grep -rn 'const [A-Z_]*: \[&\[u8\]' crates/core/src/backend/rust/emitter/` returns 0.
23. `find_next_structural_from` + `scan_quoted_string_simd` / decode pair migrated to PaddedView; per-chunk SIMD bounds guards in consolidated loops return 0.
24. **post-AW-W3.json**: bootstrap ≥ 900 MB/s; tailwind on 4 cores ≥ 1.4 GB/s.

### W4 — Parallel parse + bloom + Pratt

25. List-rule identification populates `GrammarProfile::list_rules`; tailwind chunks fork.
26. Bloom + GADT activates per `dedup_eligible_rules`; canada bloom-AND overhead < 2%.
27. Pratt lowering emits one push_compound per operator; Sheets `parse_simple` ≥ 250 MB/s.
28. `test_let_parses_as_let_call` un-ignores (heals naturally via Pratt grammar touch).
29. GrammarProfile slot calibration commits per-grammar values for `expected_ns_per_byte`, `parallel_break_even_bytes`, `payload_bytes_per_input_byte`.
30. **post-AW-W4.json**: tailwind ≥ 1.2 GB/s on 4 cores; canada ≥ 1800 MB/s on 4 cores.

### W5 — Walker + reader + parity harnesses

31. 7 JSON variant-dispatch tests un-ignore.
32. 13 serialize/structural roundtrip tests un-ignore.
33. `sonic_rs_parity` harness: zero divergences.
34. `lightningcss_parity` harness: zero divergences.
35. `cargo test --workspace --no-fail-fast` 0 failures; ignored count ≤ 14 (the enumerated Category A set per W5 phase narrative — closure tests, analysis structural-mode gates, gorgeous fixture tests, pprint-vm hint tests, plus `test_selective_transitive_unfurling` if W5.5 routes to AX).

### W6 — Visitor production + bench parity

36. `Tape::reduce_column<C, R>` lands; one reducer per grammar in `crates/core/tests/visitor_reduce.rs` exercises the kernel and matches V2.5's microbench performance ceiling. Zero new BBNF grammar directives introduced by AW (verifies AV invariant 5 alignment).
37. **post-AW.json**: every entry from the post-AV reality-check table meets its W6 gate (canada ≥ 2000, twitter ≥ 2400, bootstrap ≥ 800, tailwind ≥ 1200, parse_simple ≥ 250, bbnf_self ≥ 500, etc.).

### W7 — Completion

38. `docs/tranches/AW/FINAL.md` exists per `docs/instructions/README.md` requirements.
39. `docs/benchmarks/post-AW.json` exists covering the four parse-bench matrix.
40. Every AW invariant verified with artefact citation.

## Cross-tranche parity

41. **sonic-rs parity.** `sonic_rs_parity` harness W5.3: zero divergences across canada / twitter / citm / data / data_xl. `json_monolithic_value` bench bbnf/sonic ratio ≥ 0.95 on canada, ≥ 0.85 on twitter / data_s / citm / data_xl.
42. **lightningcss parity.** `lightningcss_parity` harness W5.4: per-declaration equivalence on bootstrap + tailwind. Zero divergences.
43. **Named struct ABI.** `pub struct Color` matches `lightningcss::values::color::Color::RGBA` field layout under the W5 harness.

## Deferred-item fold-in from AV FINAL.md §"Cross-tranche debt"

Every item AV deferred to AW has a phase home above. Mapping
ledger:

| AV deferred item | AW phase |
|------------------|----------|
| pinned_number_drops_f64_payload (Sheets) | AW.2.5 |
| Sheets `boolean` FALSE branch drops 0u8 | AW.2.5 |
| White-colour `0xFFFFFFFFu32` collision | AW.0.8 |
| `find_next_structural_from` + decode pair migration | AW.3.4 |
| 13 serialize/structural roundtrip | AW.5.2 |
| 7 JSON variant-dispatch | AW.5.1 |
| 3 CSS percentage InlineScalar reader | AW.2.5 |
| `test_let_parses_as_let_call` (gorgeous) | AW.4.6 (heals via Pratt) |
| `test_selective_transitive_unfurling` | AW.5.5 |
| Inline `#[cfg(test)]` in `crates/gorgeous/src/` | AW.0.6 |
| Bootstrap regen CI gate | AW.0.7 |
| `compute_sibling_skip` dead-code warning | AW.0.2 |
| GrammarProfile slot calibration | AW.4.7 |
| sonic-rs JSON-value parity harness | AW.5.3 |
| lightningcss CSS AST parity harness | AW.5.4 |

Plus AW-internal fold-ins from V0–V5 substrate overshoots:

| Overshoot | AW phase |
|-----------|----------|
| Stage-C unconditional run | AW.0.1 |
| Double Span pack in `__identifier` | AW.0.3 |
| Always-true `if __has_payload` | AW.0.3 |
| `mark_children` for leaf-route rules | AW.0.3 |
| `__aggregate_buf [u8; 16]` over-allocation | AW.0.4 |

Plus AT/AU items the AW orchestrator INITIALLY misframed as
deferred-but-actually-deleted:

| Item | Actual history | AW posture |
|------|----------------|------------|
| `StructRegistry` (AS.2.3 / AT.6.1 / AT.6.2) | **DELETED** AU.4.2 (commit `ab8588a`) per no-legacy-code invariant; explicit decision to "handle struct projections via per-backend type tables, not a central registry" | AW does NOT reintroduce; W0.5 follows AU.4.2's stated path: backend-side Color projection over AV.0.5's `LargeAggregate` substrate |
| `ParsedGrammar` elimination (AR.7.2, 11-tranche deferral) | **DELETED** AU (commit `688d6ea`) | nothing for AW |
| `Tape::iter` / cursor-allocation walker (AT.5.1) | **WIRED** AT, validated AU | nothing for AW |
| 64-byte input padding | **LANDED** AU (parse-that `64fe9f2`); cascade closed AV.0.7 | nothing for AW |
| `.map(|_| ())` discards | **ELIMINATED** AT/AU (commit `4e4a75e`) | nothing for AW |

## Long-deferral audit ledger (AM–AV ten-tranche read-only audit)

Four parallel audit agents read every tranche doc from AM
through AV (and grep-confirmed against current master code)
to surface items consistently deferred but neither
implemented nor accounted for above. The audit findings
fold here so no item drifts through a seventh tranche
unadjudicated. Items below are separated into AW-fold
(genuine W0–W7 scope additions), formal-retire (chronic
deferrals that need explicit closure), and AX-route
(genuinely out-of-AW-scope hyperopt or post-DTA polish).

### AW-fold — genuine W0–W7 additions surfaced by the audit

| Item | Origin chain | AW phase fold |
|------|--------------|---------------|
| GrammarProfile 5 stub fields = `&[]` (`active_columns`, `list_rules`, `keyword_tables`, `shape_dict`, `dedup_eligible_rules`). V1 hard-gate 10 passed on grep criterion (no per-grammar `const X: &[u8]`); profile fields themselves remain stubs. AW waves populate as they activate — but no shared precondition ledger. Source: `crates/core/src/backend/rust/emitter/profile.rs:142-147`. | AV.1.3 (AU-AV agent) | **AW.0.9** (NEW) — ledger-only entry confirming each W2/W3/W4 wave's profile-population contract. The wave that consumes the slot is responsible for populating it; W0.9 enumerates the four contracts so an interrupted wave's stub state is auditable. |
| AT.1.3 KvPair JSON `pair` rule activation gap. `is_kv_pair_shape` exists in `crates/ir/src/passes/payload/layout.rs`; never fires for JSON because `pair`'s layout admission excludes it. `grep KvPair crates/core/src/grammar/generated.rs` returns 0. AW.1 wholesale DTA-replaces emission — but if the layout pass excludes `pair`, the DTA inherits the gap. Source: AR-AT agent grep. | AT.1.3 (AR-AT agent) | **AW.1.7** (NEW) — sub-verification step in W1: post-DTA-driver landing, confirm `KvPair` fires for JSON `pair` (and Sheets `key_value` if applicable). If layout admission still excludes, widen `compute_payload_layouts` to admit `pair`-shape rules. |
| AV.2.5 reorder-unrolling 6× SIMD-packed gate (currently 3.3× scalar-left-fold-free per V2.5 microbench). AV FINAL hard-gate 12 partial; AW.6.1 wires the `@visitor` directive but no explicit 6× numeric gate. Source: AV FINAL.md hard-gate 12. | AV.2.5 (AU-AV agent) | **AW.6.3** (NEW) — explicit numeric gate added to W6: at least one production visitor on a scalar-dense grammar measures ≥ 6× scalar-left-fold via portable_simd `f64x4` packing, OR rationale documents the AArch64-specific lowering ceiling that holds at 3.3× scalar. |
| `inline_acyclic` / `fuse_single_use` latent no-op (AU PROGRESS Session 1 flag): both passes guard on `r.meta.scc_id.is_none()` which is always `Some` during the normalizer loop. Effectively no-ops at IR level. Source: `crates/ir/src/passes/transform/inline.rs:23`, `crates/ir/src/passes/transform/fuse.rs:31`. | AU PROGRESS (AU-AV agent) | **AW.0.10** (NEW) — drop the always-true guard so the passes fire as designed; consumers (DTA lifter at `dta.rs:670` "post-fuse_single_use shape" comment) currently consume a no-op output. Verify post-fix that fused-shape DTA states drop in count. |

### Chronic 5+-tranche deferrals — AX scope (must adjudicate, not drift)

The two longest unresolved deferral chains land as **AX
opening scope**, not retired. Both have been re-proposed in
every cost/CSP-touching tranche from AL/AM through AR, then
silently dropped each cycle. AR.md acknowledged each as
"intentionally shelved at current grammar scale" but did not
close the item. AW does not let them slip a seventh time.
AX adopts both as named phases on day one of its plan
authorship.

| Item | Defer chain (5+ tranches) | AX posture |
|------|--------------------------|------------|
| **Cost-model grid sweep** — egraph `CostWeights` calibration via grid search over a representative grammar corpus. Proposed AM.6 → AO.4.1 → AP.6.4 → AQ.9.4. AR.md: "manual calibration adequate." | AM.6 → AO.4.1 → AP.6.4 → AQ.9.4 | **AX scope** — distinct cost surface from AW.4.7's runtime per-grammar scalars (`expected_ns_per_byte`, `parallel_break_even_bytes`, `payload_bytes_per_input_byte`). The egraph `CostWeights` are compile-time IR ranking weights driving cost-aware extraction; the AW.4.7 calibration moves orthogonal levers. AX writes a grid-sweep harness across the four-grammar corpus + the representative inputs, regenerates `CostWeights`, measures DTA state-count + extraction depth deltas, commits the calibrated weights as a `pub const`. Acceptance criterion: ≥ 5% reduction in either DTA state count OR extraction-pass wall-clock against the post-AW master baseline. If neither moves, the item closes with a recorded null-result and AR's manual-calibration stance becomes the documented permanent decision. |
| **Global CSP solve** — replace per-component CSP with a single global solve. Proposed AL-prototype → AO.4.2 → AP.6.5 → AQ.9.5. AR.md: "per-component CSP is sufficient at current grammar scale." | AL → AO.4.2 → AP.6.5 → AQ.9.5 | **AX scope** — AX explicitly investigates whether the post-AW grammar corpus (CSS L4 ~1200 rules, BBNF ~400, JSON ~25, Sheets ~80) carries cross-component coupling that per-component CSP pessimises. Concrete deliverable: implement a single-solver path behind a feature flag, measure the per-component vs global solve quality on the four-grammar corpus, compare emitted constants byte-for-byte. Acceptance criterion: global solve produces strictly-better-or-equal cost on every grammar, OR documents specific cases where it under-fits; if no measurable improvement, the per-component design is adopted as the permanent choice with measurement evidence. |
| **AU.2.7 intermediate perf gate** (CSS bootstrap ≥ 650 MB/s from v2 structural bitmap *alone*). Silently rolled into AW W3 composite (`bootstrap ≥ 900 MB/s` after PHF + selector classifier). Original gate's attribution lost. | AU.2.7 (AU-AV agent) | **SUPERSEDED** by AW W3 composite gate. AW FINAL records the supersession explicitly: the v2-bitmap-alone gate was a phase-internal AU target; the AW composite gate (PHF + classifier + bitmap together at ≥ 900 MB/s) subsumes it. Future tranches do not re-list as "missed AU gate". |

### AX-route — items genuinely out of AW scope (next tranche or later)

| Item | Origin | AX classification |
|------|--------|------------------|
| AN.5 32-byte SIMD widening (`u8x32` on AVX2). Every SIMD call site uses `u8x16` today; no `u8x32` matches in scanners. Hyperopt; gated on workload where chunk-count dominates. | AN.5 (AM-AN agent) | AX **scanner hyperopt** wave |
| AO.5.3 Branch frequency ordering for dispatch (frequency-hint consumer, not specificity sort). | AO.5.3 (AO-AQ agent) | AX **dispatch tuning** wave |
| AP.3.2 Redundant trim-call elision via `last_trim_offset` fused-scan. Sub-5% marginal post-DTA. | AP.3.2 (AO-AQ agent) | AX **post-DTA WS polish** wave |
| AP.4.2 Hoist duplicated patterns (`ws + ':' + ws`, `!important`) — grammar-level dedup of 43/42 repeats. | AP.4.2 (AO-AQ agent) | AX **grammar-dedup** scope |
| AP.5.4 Deferred UTF-8 validation — lazy validation hooks; substrate-level change. | AP.5.4 (AO-AQ agent) | AX **substrate** scope |
| AQ.7.3 Length-bucketed perfect hash for key dispatch (generalised first/last-byte PHF). AW.3.1 covers `namedColor` and Sheets functions; the *generalised* length-bucket variant is a separate lever. | AQ.7.3 (AO-AQ agent) | **AW.3.1 partial** — generalised tail to AX |
| AQ.8.1 `skip_space` bitmap caching (`nospace_bits`, `nospace_start`). | AQ.8.1 (AO-AQ agent) | AX **post-DTA WS polish** |
| AQ.8.3 TLS-recycled scratch (`#[parser(tls_scratch)]`) codegen flag. | AQ.8.3 (AO-AQ agent) | AX **codegen flags** scope |
| AT.4.3 NEON 17-digit fractional scan (4-tranche chain AR→AT). AV.3.5 landed Eisel-Lemire Clinger short-circuit + 16-digit SIMD integer fastpath; the *17-digit fractional* kernel specifically remains. | AT.4.3 (AR-AT agent) | AX **number-scan finishing** scope |
| Scanner-architecture cluster: AR.6.1/6.2/6.4/6.5/6.6/6.8 (mirrored as AS.5.1/5.2/5.4/5.5). `RegexClassMiner` consolidation, `ScanLut` registry, `WsCommentConfig` parameterisation, `FnDescriptor` post-pass, HIR predicate re-exports. AS.5 marked most as "Not applicable" / "Premature optimisation" / "Negligible overhead"; AT silent. ~600 LOC delete + 350 LOC net reduction estimated. | AR.6.x / AS.5.x (AR-AT agent) | AX **focused scanner-hygiene tranche** — recommend a dedicated tranche, not folding into a multi-purpose wave |
| AV.3.6 CSS L4 DTA state count narrowing (2473 vs predicted ~1200). AV PROGRESS: "factoring can land in V9 closure if the table size becomes a runtime constraint." | AV.3.6 (AU-AV agent) | AX **conditional** — only if AW W6 bench shows table-size pressure (DTA dispatch I-cache miss rate); otherwise stays at 2473 |
| AV.2 pre-order emission (sibling-skip walker still uses bounded backward walk for first-child seeding). AW.1.1 DTA emits pre-order natively, so the AV.2 substrate inherits the gap silently. | AV.2 / AV.2.6 (AU-AV agent) | **AW.1.4 sub-verification** — confirm post-DTA emit yields pre-order tape and the bounded backward walk in `cursor.rs::child(0)` becomes O(1); otherwise route to AX |
| FDMP mimalloc segment-class rounding (research 02). Never implemented. | AV research 02 (AU-AV agent) | AX **allocator-tuning** scope |
| Per-grammar column overlays `sel_col` / `pay_cellref` (research 04). Never implemented. | AV research 04 (AU-AV agent) | AX **column-overlay** scope (low priority — current 6 structural + 6 typed-payload columns suffice) |

### Audit confirmations (no action required)

- AT.4.1 fresh samply profiles → SUPERSEDED by AW per-wave bench checkpoint contract.
- AO.5.4 structural position prefetch → SUPERSEDED (deleted substrate per AQ.5).
- AN.3 single-pass string scan → SUPERSEDED by AU.3.1 commit `ceab7e8` + the fused `decode.rs` SIMD loop.
- AM.5.3 structural bitmap codegen consumer → DELETED in AQ.5 commit `2f7c1bd` (net regression on citm).
- AO.0.1–0.6 / AP.1 / AP.3.4 structural dispatch family → DELETED in AQ.5.

## Style note

AW preserves AU's and AV's voice: imperative invariants, no
hedging on hard gates, archaic-Latinate cadence where the
register reads natural ("therein", "therefrom", "wherefore"
used sparingly for emphasis, not ornament). Architectural
language ("activation", "elision", "lattice", "substrate")
carries the same precision the prior tranches set; it is
load-bearing, not decorative. The bench-checkpoint contract
is novel to AW because AV's omission of intra-tranche bench
was the single largest operational miss; the contract makes
its absence structurally impossible.

## Research artefacts

`docs/tranches/AW/research/` is reserved for the targeted
audit deliverables that seed wave dispatch:

- `01-dta-driver-design.md` — the W1 walker contract,
  frame-stack overflow handling, `frame_depth` emission
  rules.
- `02-shaperef-runtime-dispatch.md` — the W2.3 dict
  consultation cost model, `shape_hash` collision strategy,
  view-layer expansion correctness proof sketch.
- `03-pratt-lowering-generality.md` — the W4.6 Pratt loop's
  generalisation from Sheets to CSS value expressions and
  BBNF binary expressions; precedence-table generation from
  the grammar's mined operator chain.
- `04-named-struct-abi-finalisation.md` — the W0.5 layout
  contract: how `TypeDesc::Struct(StructId)` interacts with
  the existing `LargeAggregate` payload route, view-layer
  accessor codegen, lightningcss equivalence proof.
- `05-bench-checkpoint-protocol.md` — the per-wave bench
  agent's contract: input matrix, output JSON shape,
  attribution requirements, regression rationale format.

These land before the relevant wave dispatches; the W0
audit kicks off the research wave the same day the
orchestrator dispatches W0 cleanup.

## AX seeds — opening scope for the next tranche

AW FINAL.md will enumerate AX seeds in detail post-W7. The
ones already named in this plan, captured here so they
carry forward without further drift:

- **Cost-model grid sweep** (AM.6 → AO.4.1 → AP.6.4 →
  AQ.9.4 → AW long-deferral ledger). Egraph `CostWeights`
  calibration with a grid-sweep harness over the four-
  grammar corpus. Acceptance: ≥ 5% reduction in DTA state
  count or extraction wall-clock vs post-AW master, OR
  null-result close with measurement evidence.
- **Global CSP solve** (AL → AO.4.2 → AP.6.5 → AQ.9.5 →
  AW long-deferral ledger). Single-solver path behind a
  feature flag, byte-for-byte comparison of emitted
  constants vs per-component on the four-grammar corpus.
  Acceptance: strictly-better-or-equal cost everywhere, OR
  documented null-result with per-component as the
  permanent decision.
- **Scanner-architecture cluster** (AR.6.1/6.2/6.4/6.5/
  6.6/6.8 + AS.5.1/5.2/5.4/5.5). Dedicated scanner-hygiene
  tranche: `RegexClassMiner` consolidation, `ScanLut`
  registry, `WsCommentConfig` parameterisation,
  `FnDescriptor` post-pass, HIR predicate re-exports.
  Estimated ~600 LOC delete + 350 LOC net reduction.
- AX hyperopt cluster (post-DTA polish): AN.5 `u8x32`
  AVX2 widening, AO.5.3 frequency-ordered dispatch,
  AP.3.2 trim elision, AP.4.2 grammar-level pattern
  dedup, AP.5.4 deferred UTF-8 validation, AQ.7.3
  generalised length-bucket PHF tail, AQ.8.1 skip_space
  bitmap caching, AQ.8.3 TLS-recycled scratch, AT.4.3
  NEON 17-digit fractional scan.
- AX substrate cluster: FDMP mimalloc segment-class
  rounding (AV research 02), per-grammar column overlays
  `sel_col` / `pay_cellref` (AV research 04), AV.3.6 CSS
  L4 DTA state-count narrowing (only if AW W6 bench shows
  table-size pressure on I-cache).

AX adopts these on day one of its plan authorship. None
drifts to a seventh tranche unadjudicated.

## Indefatigability

The orchestrator does not relinquish control until AW's
completion requirements are met: every wave's bench
checkpoint lands, every hard gate passes with artefact
citation, FINAL.md and post-AW.json compose, `cargo test
--workspace` exits zero. AV's V0–V5 close was the longest
sustained orchestration this codebase has carried; AW's
W0–W7 will exceed it. The bench-between-waves discipline is
the trajectory check that lets the orchestrator catch
overshoots before they accrete. Every wave that does not
move its target bench in the expected direction halts for
diagnosis, not for next-wave dispatch.

Begin AW with W0 — the cleanup wave that recovers the V0–V5
substrate overshoot before any activation work touches it.
