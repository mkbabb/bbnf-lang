# Tranche AW — The Activation

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

This tranche also closes the typing debt that has carried
across the last five tranches — Named struct ABI finalization
(deferred AT.6.1 → AS.2.3 → AT.6.2 → AV residual), proper
grammar-side typed-AST equivalence with `lightningcss::values::
color::Color` and `sonic_rs::Value`, the inline `#[cfg(test)]`
violations in `crates/gorgeous/src/google_sheets.rs`, and the
13 serialize/structural roundtrip regressions AV ignored to
get the workspace green. None of these are new scope. Each is
an item that has been deferred at least once, in some cases
five times. AW is where the ledger zeroes.

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
   `reorder_unroll_visitors` ships at least one production
   visitor per scalar-dense grammar.
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
5. **Named struct ABI finalises in this tranche.** The five-
   tranche deferral (AT.6.1 → AS.2.3 → AT.6.2 → AU →
   AV residual) closes here. CSS L4 colour-function rules
   (`colorFunction`, `colorFn`, `colorMix`) project to
   concrete Rust struct layouts via `pub struct Color` /
   `pub struct ColorMix` over the existing `LargeAggregate`
   payload route. No more `TypeDesc::Named("Color")`
   placeholders.
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
| Named struct ABI finalisation | AT.6.1 | AT.6.2 → AU → AV | **W5 Phase 5.2** |
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
| **W0 — Cleanup + ABI finalisation + hygiene** (5 parallel) | (a) Stage-C conditional + dead-code deletion (AW.0.1, AW.0.2). (b) Span-rule emitter elision (AW.0.3, AW.0.4). (c) Named struct ABI finalisation (AW.0.5). (d) Inline-test migration (AW.0.6). (e) Bootstrap regen CI gate + white-colour WideScalar routing (AW.0.7, AW.0.8). | Green at W0 close. | **post-AW-W0.json** — recovers 25–40% of regression by elision alone; trajectory measurement, not gate. |
| **W1 — DTA runtime driver activation** (serial, single owner) | Single agent: AW.1.x replaces every grammar's `parse()` entry point with the DTA-driven stage-A walk. Legacy `fn __<rule>` deleted from the hot path; `__rule_kind()` dispatch retained for IR consumers. Stage-C activates unconditionally with DTA-emitted `frame_depth`. | Green at W1 close — primary correctness gate. | **post-AW-W1.json** — gate: every entry ≥ post-AU baseline. |
| **W2 — PSI stage-B + ShapeRef + percentage closure** (3 parallel) | (a) PSI rayon stage-B activation per `parallel_break_even_bytes` (AW.2.1, AW.2.2). (b) ShapeRef runtime dispatch — `push_shape_ref` fires on `shape_hash` match (AW.2.3, AW.2.4). (c) Bug 2b residuals + Sheets boolean FALSE + percentage InlineScalar (AW.2.5). | Green. | **post-AW-W2.json** — gate: bootstrap ≥ 700 MB/s, twitter `decode_json_string` self-time < 5%. |
| **W3 — SIMD keyword dispatch + PHF + selector classifier** (4 parallel) | (a) PHF for CSS `namedColor` + Sheets function names (AW.3.1). (b) SIMD keyword compare for ≤ 16-keyword Alts (AW.3.2). (c) CSS selector classifier over structural bitmap (AW.3.3). (d) `find_next_structural_from` paired migration + remaining SIMD scanner holdouts (AW.3.4). | Green. | **post-AW-W3.json** — gate: `__compoundSelector` self-time < 15%; bootstrap ≥ 900 MB/s. |
| **W4 — Document-level parallel parse + bloom+GADT dedup + Pratt** (3 parallel) | (a) List-rule mining + chunk boundary detection + offset remap (AW.4.1, AW.4.2, AW.4.3). (b) Runtime bloom + GADT dedup gated on `dedup_eligible_rules` (AW.4.4, AW.4.5). (c) Pratt precedence-tower lowering for Sheets (and any grammar with chained operators) — heals `test_let_parses_as_let_call` (AW.4.6). Plus GrammarProfile slot calibration (AW.4.7). | Green. | **post-AW-W4.json** — gate: tailwind ≥ 1.2 GB/s on 4 cores; canada ≥ 1800 MB/s on 4 cores; sheets `parse_simple` ≥ 250 MB/s. |
| **W5 — Walker + reader migration + parity harnesses** (3 parallel) | (a) variant_idx walker coherence — un-ignore 7 JSON tests (AW.5.1). (b) 13 serialize/structural roundtrip un-ignore + fix (AW.5.2). (c) sonic-rs JSON-value parity harness + lightningcss CSS AST parity harness (AW.5.3, AW.5.4). Plus `test_selective_transitive_unfurling` triage (AW.5.5). | Green; ignored count = documented Category A only. | **post-AW-W5.json** — gate: every parity harness green; ignored count ≤ 5. |
| **W6 — Bench parity confirmation + visitor production wiring** (2 parallel) | (a) Wire at least one production `@visitor` directive per scalar-dense grammar (BBNF `$count_rules`, JSON `$sum_of_numbers`, etc.) — proves V2.5 visitor codegen reaches end users (AW.6.1). (b) Bench confirmation matrix vs the post-AU baseline (AW.6.2); compose `post-AW.json`. | Green. | **post-AW.json** — every entry from the post-AV reality-check table meets its W6 gate. |
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

#### AW.0.5 Named struct ABI finalisation

The five-tranche deferral closes here. AT.6.1 introduced the
plan; AT.6.2 deferred to AS.2.3; AS deferred to AT.6.2; AU
deferred via §4 to AV; AV V0–V5 closed without addressing.

Concrete deliverable: CSS L4 colour-function rules
(`colorFunction`, `colorFn`, `colorMix`) project to concrete
Rust struct layouts via `pub struct Color { pub space:
ColorSpace, pub c1: f64, pub c2: f64, pub c3: f64, pub
alpha: f64 }` and `pub struct ColorMix { pub mix_space:
MixSpace, pub hue_method: HueMethod, pub a: ColorRef, pub
b: ColorRef, pub a_pct: Option<u8>, pub b_pct: Option<u8> }`
over the existing `LargeAggregate` payload route AV.0.5
landed.

`crates/ir/src/types/type_desc.rs::TypeDesc::Named("Color")`
admits to a new `TypeDesc::Struct(StructId)` variant with a
registered `StructLayout` that the emitter resolves at
codegen time. View-layer accessors (`.as_color()`) project
through `pub struct Color` rather than returning a tuple.

Hard gate: `CssL4Parser::parse("rgb(255 128 0 / 0.5)").
view().as_color()` returns a `Color { space: ColorSpace::
Rgb, c1: 255.0, c2: 128.0, c3: 0.0, alpha: 0.5 }` whose
field layout matches `lightningcss::values::color::Color::
RGBA { r: 255.0, g: 128.0, b: 0.0, a: 0.5 }` accessor-by-
accessor under the parity harness W5 lands. AV.0.5's hard
gate ("byte-equivalent to lightningcss") finally satisfied.

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

V2.5's `emit_visitor_kernels` continues to fire for any
grammar whose `mine_visitors()` returns non-empty (today
none; W6 wires production directives). The kernel emission
is unchanged; the consumer is the rule-walker, which is
unaffected by the DTA driver swap.

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

#### AW.4.7 GrammarProfile slot calibration

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
- ignored count ≤ 5 (only the documented Category A
  pre-existing items: `test_selective_transitive_
  unfurling` if not fixed; closure tests if still gapping;
  whatever the W5 audit confirms is genuinely out-of-AW
  scope).

### Phase 6 — Visitor production wiring + bench parity (W6)

#### AW.6.1 Production `@visitor` directives

V2.5's `mine_visitors()` returns empty for every shipped
grammar today because the `@visitor` directive isn't wired
through the BBNF lexer/parser. W6.1 lands the directive +
adds at least one production visitor per scalar-dense
grammar:

- BBNF: `@visitor count_rules : column any reduce count ;`
- JSON: `@visitor sum_of_numbers : column F64 reduce sum ;`
- CSS L4: `@visitor declaration_count : column any reduce count ;`
- Sheets: `@visitor formula_count : column any reduce count ;`

The kernels emit via V2.5's `emit_visitor_kernels` —
unchanged from AV. The wiring is grammar-side: extend the
BBNF grammar to recognise `@visitor name : column TYPE
reduce OP ;`, lower to `IrRule { meta.directives.visitor:
Some(VisitorDirective {...}) }`, populate `GrammarProfile::
reorder_unroll_visitors` from the directive.

Verifies V2.5's substrate reaches end users; the 3.3×
microbench gain becomes available to grammar authors.

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
| `crates/bbnf-tape/src/driver.rs` (**new** — DTA stage-A walker) | 1 |
| `crates/ir/src/types/type_desc.rs` (TypeDesc::Struct admission) | 0 |
| `crates/ir/src/passes/payload/layout.rs` (Map-bodied scalar admission) | 2 |
| `crates/bbnf-tape/src/dedup.rs` (**new** — bloom + GADT) | 4 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (**new**) | 4 |
| `crates/ir/src/passes/recognizers/dedup_eligibility.rs` (**new**) | 4 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (**new** — PHF + SIMD compare) | 3 |
| `crates/core/src/backend/rust/emitter/selector_classifier.rs` (**new** — CSS selector classifier) | 3 |
| `crates/core/src/grammar/generated.rs` (DTA-driven, ~10K lines) | 1, 2, 3, 4, 6 |
| `grammar/css/l4/color.bbnf` (Color / ColorMix struct projection) | 0 |
| `grammar/bbnf/bbnf.bbnf` (`@visitor` directive grammar) | 6 |
| `crates/core/tests/sonic_rs_parity.rs` (**new**) | 5 |
| `crates/core/tests/lightningcss_parity.rs` (**new**) | 5 |
| `crates/core/tests/{json,structural}_parity.rs` (un-ignore + fix) | 5 |
| `scripts/check-bootstrap-clean.sh` (**new** — CI gate) | 0 |
| `crates/gorgeous/tests/google_sheets.rs` (**new** — inline-test migration target) | 0 |
| `docs/tranches/AW/{PROGRESS,FINAL}.md` + `docs/benchmarks/post-AW{,-W0,-W1,-W2,-W3,-W4,-W5}.json` | 0–7 |

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
11. **post-AW-W0.json** shows ≥ 25% recovery on the post-AV regression vs. four-bench matrix (trajectory measurement, not bench gate).

### W1 — DTA driver activation

12. `grep -cE 'fn __[a-zA-Z_]+<' crates/core/src/grammar/generated.rs` returns 0 outside prettify.
13. `wc -l crates/core/src/grammar/generated.rs` ≤ 12000.
14. `cargo test --workspace --no-fail-fast` 0 failures, ignored count unchanged from W0.
15. **post-AW-W1.json**: every entry ≥ post-AU baseline.

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
35. `cargo test --workspace --no-fail-fast` 0 failures; ignored count ≤ 5 (Category A only).

### W6 — Visitor production + bench parity

36. ≥ 1 `@visitor` directive lands per scalar-dense grammar; visitor kernels emit + active.
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

Plus AT/AU long-deferred:

| AT/AU item | AW phase |
|------------|----------|
| Named struct ABI finalisation (5-tranche deferral) | AW.0.5 |

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
