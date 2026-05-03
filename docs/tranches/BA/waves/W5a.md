# BA.W5a — JSON Direct-To-Struct Migration

**Thesis** (the JSON `OpenFrame` substrate retires; direct-projection emit replaces the speculative `Vec<OpenFrame>::clone` checkpoint pattern; per-rule generated `parse_<rule>(...)` functions return typed `JsonValue<'p>` directly via byte-disjoint Alt; CSS L4, BBNF, Sheets, and the five-grammar cohort follow at W5b..W5e; Lock 1 begins its honoured close at BA close). **Closer-gate** (BA-G1: `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro, beating sonic-rs's 436 µs by ≥ 8%; BA-G2: ≤ 2 heap allocations per parse-call; samply post-W5a shows `Vec<OpenFrame>::clone` retired from JSON profile; `rg -n 'enum OpenFrame' crates/core/src/runtime/json/` returns 0).

## §1 — Deliverable

Hereupon the JSON parse path's 86.07% inclusive pathology — the `Vec<OpenFrame>::clone` checkpoint at `crates/core/src/runtime/json/builder.rs:243` per `audit/RESTART-SKETCH-2026-05-03.md:154-220` — retires by mechanism. The mechanism is direct-projection: per-rule generated `parse_<rule>(input, &mut p, &mut arena, &mut state, &mut cursor) -> Result<JsonValue<'p>, ParseErr>` functions return typed values directly; byte-disjoint Alt emits direct `match first { b'{' => parse_object(...), b'[' => parse_array(...), b'"' => parse_string(...), b'-' | b'0'..=b'9' => parse_number(...), b't' | b'f' => parse_bool(...), b'n' => parse_null(...) }` without speculative checkpoint. `OpenFrame` deletes from the JSON path.

Per `audit/RESTART-SKETCH-2026-05-03.md:512-543` the post-restart emitter for JSON's `value` rule emits ~25 lines (versus the current `parse_object_JsonParser_object`'s 130 lines + `parse_wrap_*`'s 300 lines per `audit/RESTART-SKETCH-2026-05-03.md:592-595`). The construct-by-construct emission shapes per `docs/tranches/BA/audit/W5-generated-parser-shape.md` §1.1-§1.10 are the canonical contract: Alt (byte-disjoint), Seq, Repeat, Optional, CharClass, Keyword, Scanner (DFA), MapExpr, HostCall — JSON exercises Alt, Seq, Repeat, CharClass, Keyword, Scanner. JSON has no host fns at BA.W5a.

The direct-projection shape's properties per `audit/RESTART-SKETCH-2026-05-03.md:582-590`:

- No `StructLayout` runtime literal. `String::from("object")` / `Vec::new()` for fields — gone.
- No `OpenFrame` stack. Local Rust variables hold the pair vec on the call stack; the recursion forms the stack naturally.
- No `checkpoint()` / `rollback()` on byte-disjoint Alt. The dispatcher is a `match` on first byte — no speculative entry.
- `JsonValue` is a Copy 16-byte tag-and-payload — passed by value into recursion, returned by value, stored by value.
- The arena owns interned slices.

The W5a emitter scope is JSON only. CSS L4, BBNF, Sheets, and the five-grammar cohort retain their `OpenFrame`-builder emitter path until W5b/c/d/e — all five sub-waves land within BA. The substrate-without-consumer concern is mitigated because W5a's consumer (JSON parsing) is benched at BA-G1 ≤ 400 µs and BA-G2 ≤ 2 heap allocations IN THE SAME WAVE. The Era V failure mode is closed for JSON at W5a; the remaining grammars' Era V closes at W5b, W5c, W5d, W5e respectively.

The performance trajectory: pre-W5a baseline (per `audit/RESTART-SKETCH-2026-05-03.md:154-220`) carries the 86.07% inclusive sample share at `Vec<OpenFrame>::clone`. W5a retires it; samply post-W5a shows the share retired. The target ≤ 400 µs (BA-G1 per `audit/SOTA-2026-05-03.md:50-58` sonic-rs benchmark_aarch64) is achievable because (per `audit/RESTART-SKETCH-2026-05-03.md:206-217`):

- Item 1 (StructLayout literal): retired by direct-projection.
- Item 2 (Vec deep-clone): retired; byte-disjoint Alt has no checkpoint.
- Item 3 (18 attempt arms): retired; predictive byte-dispatch emits `match first { b'{' => ..., b'[' => ... }` directly.
- Item 4 (per-frame Vec::new()): retired; stack-allocated `JsonValue` returns by value.
- Item 5 (deposit re-discrimination): retired; direct-projection emits parent-typed deposit.
- Item 6 (linear pair scan post-parse): retired at BA.W4b (`Document::get<T>` reroutes through `parse_with`).
- Item 7 (Wrap frame on byte-disjoint Alt): retired; codegen elides Wrap entirely.
- Item 8 (parse fn → __value thunk): retired; codegen inlines.
- Item 11 (persistent stack alloc): retired; recursion IS the stack.

The post-W5a emit shape for JSON's `value` rule is the canonical sonic-rs analogue per `audit/SOTA-2026-05-03.md:30, 212`: typed enum + recursive descent + arena-borrowed slices + zero speculative checkpoint on byte-disjoint Alt.

## §2 — Milestones

> **M0 — Direct-projection emitter scaffolding (JSON-only)**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/` (post-W2.M2 split per `audit/MODULES-2026-05-03.md:868`); `crates/core/src/backend/rust/emitter/grammar.rs` (per MODULES:827); `crates/ir/src/registry/strategy.rs` (post-W1; the metadata-driven strategy resolver).
> *Action*: Introduce a new `EmitStrategy::DirectToStruct` variant. Wire the JSON grammar's `[workspace.metadata.bbnf-strategy]` entry to declare `strategy = "direct_to_struct"`. CSS L4, BBNF, Sheets, and cohort entries STILL retain `EmitStrategy::OpenFrame` at W5a (their migrations land at W5b..W5e respectively). Per `feedback_no_orthogonal_codepaths`, the dual-strategy resolver is transient — at W5e close all grammars resolve to DirectToStruct and the OpenFrame variant deletes.
> *Gate*: workspace metadata declares the strategy per grammar; the emitter dispatch reads metadata and routes JSON to DirectToStruct, others (transient) to OpenFrame.
> *Exit-criteria*: `cargo metadata --format-version 1 | jq '.metadata."bbnf-strategy".grammars[] | select(.ident=="json") | .strategy' | tr -d '"\n'` returns `direct_to_struct`.

> **M1 — Per-rule `parse_<rule>` direct-projection emit for JSON**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct/{header,body,fields,finalize}.rs`; the JSON grammar at `grammar/json/json.bbnf`; the regen target `crates/core/src/grammar/generated/json.rs` (3,500 LOC pre-W5a per MODULES:621).
> *Action*: Implement direct-projection emit. For each JSON rule (`null`, `bool`, `number`, `string`, `pair`, `object`, `array`, `value`), emit `fn parse_<rule><'p>(input: &'p [u8], p: &mut usize, arena: &mut JsonArena<'p>, state: &mut ScanState, cursor: &mut PathCursor) -> Result<JsonValue<'p>, ParseErr>` (or `Result<JsonObjectId, ParseErr>` for object/array). Body emits direct `JsonValue` variant construction; no `begin_compound`/`end_compound`.
> *Gate*: post-regen `crates/core/src/grammar/generated/json.rs` contains `pub fn parse_value<'p>(...) -> Result<JsonValue<'p>, ParseErr>` etc.; no `OpenFrame` references.
> *Exit-criteria*: `cargo xtask regen --grammar json && rg -n 'OpenFrame' crates/core/src/grammar/generated/json.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'pub fn parse_value' crates/core/src/grammar/generated/json.rs | wc -l | tr -d '\n'` returns ≥ 1.

> **M2 — Byte-disjoint Alt emits direct `match first` without speculative checkpoint**
>
> *Surface*: `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs` (338 LOC per `audit/MODULES-2026-05-03.md:863`); `crates/ir/src/passes/recognizers/grammar_facts/alt_classifier.rs` (post-W2.M1 split).
> *Action*: Extend alt-classifier to detect byte-disjoint Alt (every alt-branch's FIRST is a singleton byte or non-overlapping byte set); emit `match first { b'{' => ..., b'[' => ... }` directly. No `attempt_p = *p; attempt_builder = builder.checkpoint()`. Per `audit/RESTART-SKETCH-2026-05-03.md:174` the speculative wrap retires for byte-disjoint cases.
> *Gate*: `parse_value` body in post-regen `generated/json.rs` contains direct `match first {...}`; no `attempt_builder = builder.checkpoint()` calls.
> *Exit-criteria*: `rg -n 'attempt_builder = builder.checkpoint' crates/core/src/grammar/generated/json.rs 2>&1 | wc -l | tr -d '\n'` returns `0`; `rg -n 'match first {' crates/core/src/grammar/generated/json.rs | wc -l | tr -d '\n'` returns ≥ 1.

> **M3 — `OpenFrame` deletes from the JSON path**
>
> *Surface*: `crates/core/src/runtime/json/builder.rs` (382 LOC per `audit/MODULES-2026-05-03.md:939`; declares `enum OpenFrame` at lines 61-87 per `audit/RESTART-SKETCH-2026-05-03.md:140-145`); `crates/core/src/runtime/json/arena.rs` (186 LOC per MODULES:938).
> *Action*: Delete `OpenFrame`; delete `JsonStructCheckpoint` + `checkpoint`/`commit`/`rollback` methods from `JsonStructBuilder`; reduce `JsonStructBuilder` to minimal arena-management surface required by direct-projection. The file shrinks from 382 LOC to ≤ 80 LOC.
> *Gate*: `enum OpenFrame` is gone from `runtime/json/`; no checkpoint surface.
> *Exit-criteria*: `rg -n 'enum OpenFrame' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/json/ 2>&1 | wc -l | tr -d '\n'` returns `0`; `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/json/builder.rs | awk '{print ($1 < 100)}' | tr -d '\n'` returns `1`.

> **M4 — BA-G1: `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro**
>
> *Surface*: bench harness at `crates/core/benches/bench_json.rs`; fixture `data/json/twitter.json`.
> *Action*: Run `cargo bench --bench bench_json -- twitter` post-regen; verify mean ≤ 400 µs. Per `audit/SOTA-2026-05-03.md:50-58` sonic-rs's twitter parse is 436 µs on M1 Pro; BA-G1 requires ≤ 400 µs (≥ 8% margin). Per `feedback_no_warm_benches` cold per-parse only; per `feedback_bench_single_run` single invocation.
> *Gate*: bench output JSON's `bbnf_parse_twitter.mean ≤ 400 µs`.
> *Exit-criteria*: `cargo bench --bench bench_json -- twitter 2>&1 | rg 'parse_twitter'` shows mean ≤ 400 µs.

> **M5 — BA-G2: ≤ 2 heap allocations per parse-call; zero `Vec<OpenFrame>::clone` sites in samply**
>
> *Surface*: post-W5a generated `crates/core/src/grammar/generated/json.rs`; samply harness; allocation profiler (`dhat-rs` or equivalent).
> *Action*: Run `cargo bench --bench bench_json -- twitter` under `dhat-rs`; verify ≤ 2 heap allocations per parse-call (1 arena slab + 1 root vec). Run `samply record --save-only -- cargo bench --bench bench_json -- twitter`; inspect profile for `Vec<OpenFrame>::clone` symbol — must be zero.
> *Gate*: dhat profile shows ≤ 2 allocs/iter; samply profile shows zero `Vec::clone` symbols on JSON path.
> *Exit-criteria*: dhat output `total_allocations / iterations ≤ 2`; samply profile lacks `Vec<OpenFrame>::clone` symbol.

> **M6 — JSON nextest filter passes 100%; W5a generated-LOC budget verification**
>
> *Surface*: pre-W5a `crates/core/src/grammar/generated/json.rs` (3,500 LOC); post-W5a same file; `crates/core/tests/parse_with_json.rs` + JSON-side parity tests.
> *Action*: Per `feedback_no_workarounds` and the BA.md §Generated-LOC budget table, post-W5a `json.rs` ≤ 2,200 LOC. Run `cargo nextest run -p bbnf -E 'test(json)' --profile ax-iter`; verify 100% pass.
> *Gate*: `json.rs` post-W5a LOC ≤ 2,200; `cargo nextest run -p bbnf -E 'test(json)'` 100% pass.
> *Exit-criteria*: `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/core/src/grammar/generated/json.rs | awk '{print ($1 <= 2200)}' | tr -d '\n'` returns `1`; `cargo nextest run -p bbnf -E 'test(json)' --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M7 — W5a artefact emission**
>
> *Surface*: `docs/tranches/BA/audit/W5a-json-direct-to-struct.md` (new artefact).
> *Action*: Emit a per-grammar disposition artefact recording (a) JSON pre-/post-W5a generated-LOC, (b) sample post-regen `parse_value` body, (c) bench measurement BA-G1 (twitter mean), (d) dhat allocation count BA-G2, (e) samply profile evidence (no `Vec<OpenFrame>::clone`).
> *Gate*: artefact exists; records the five evidence items.
> *Exit-criteria*: `test -f /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5a-json-direct-to-struct.md && wc -l /Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/audit/W5a-json-direct-to-struct.md | awk '{print ($1 > 50)}' | tr -d '\n'` returns `1`.

## §3 — Closer gate

```
# BA-G1: JsonParser::parse(twitter.json) ≤ 400 µs on M1 Pro
cargo bench --bench bench_json -- twitter            ; expect: ≤ 400 µs

# BA-G2: ≤ 2 heap allocations per parse-call
cargo bench --bench bench_json -- twitter --features dhat
                                                     ; expect: ≤ 2 allocs/iter

# samply post-W5a shows Vec<OpenFrame>::clone retired
samply record --save-only -- cargo bench --bench bench_json -- twitter
# inspect profile: Vec<OpenFrame>::clone inclusive samples = 0

# JSON OpenFrame deleted
rg -n 'enum OpenFrame' crates/core/src/runtime/json/  ; expect: 0 matches

# json.rs LOC reduction
wc -l crates/core/src/grammar/generated/json.rs       ; expect: ≤ 2,200

# JSON test cohort 100% pass
cargo nextest run -p bbnf -E 'test(json)' --profile ax-iter
                                                      ; expect: 100% pass
```

## §4 — Invariants

§I1. **Lock 1 partial honour** (JSON-side). Per `docs/HARDENING-PLAN-PROMPT.md:34` direct-to-struct is both visible API and underlying memory layout — sonic-rs / lightning-css discipline. W5a lands the discipline for JSON; W5b/c/d/e land it for the remaining eight grammars. Lock 1's BA close honour completes at W5e.

§I2. **Lock 3** (one parse impl). The W4-introduced `parse_with(input, &path)` is the single parse implementation; W5a's direct-projection rewrites the body but preserves the unified surface.

§I3. **Lock 9** (slice-borrow primary; bumpalo + owned escape hatches). The W5a `JsonValue<'p>` is slice-borrowed by default per `audit/SOTA-2026-05-03.md:122-123`. Three-surface API (parse / parse_in / parse_owned) routes to BB.W4.

§I4. **No orthogonal codepaths** (per `feedback_no_orthogonal_codepaths`). The dual-strategy resolver (DirectToStruct for JSON; OpenFrame for non-JSON) is transient through W5a..W5d; W5e collapses to one strategy.

§I5. **No combinators monolithic** (per `feedback_no_combinators_monolithic`). Direct-projection consumes typed IR directly; W5a's emit extends alt-classifier (M2) — no combinator/SpanParser usage.

§I6. **Preserve rich AST** (per `feedback_preserve_rich_ast`). `JsonValue` enum preserves shape: `Null | Bool(bool) | Number(JsonNumber) | String(&'p str) | Array(JsonArrayId) | Object(JsonObjectId)` — same shape as today's per `audit/MODULES-2026-05-03.md:942`.

§I7. **Beat sonic-rs target** (per `feedback_beat_lightning` extended to JSON). BA-G1 ≤ 400 µs vs sonic-rs's 436 µs — 8% margin. Performance target is to BEAT, not approach.

§I8. **Inspect generated output** (per `feedback_inspect_generated_output`). M1 + M2 verification requires reading post-regen `generated/json.rs`; closer-gate's `rg -n 'match first {'` is the inspection step.

## §5 — Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| The 8% margin to sonic-rs's 436 µs is unattainable on M1 Pro for `parse_twitter` (BA-G1) | Medium | M4 bench output | Items 1, 2, 3, 4, 5, 7 from RESTART-SKETCH:206-217 collectively retire the 86.07% sample share; if missed, profile-driven via `samply record` + surgical |
| Byte-disjoint Alt detection at M2 misclassifies a JSON rule | Low | M2 alt-classifier test cohort | Conservative classifier: only when FIRST sets are statically non-overlapping does direct `match first {...}` emit; overlapping cases retain speculative emit (gone at W5b..W5e) |
| `OpenFrame` deletion at M3 cascades into `JsonStructBuilder::checkpoint` callers across `crates/core/tests/` | Medium | `cargo nextest run -p bbnf -E 'test(json)'` | Checkpoint surface is W5a-internal (only speculative emit consumed it); deletion is consumer-tested |
| Post-W5a `json.rs` LOC reduction (≤ 2,200) is missed | Low | M6 LOC verification | Per `audit/RESTART-SKETCH-2026-05-03.md:592-595` the order-of-magnitude reduction is mechanical |

## §6 — Cross-references

- **Closes BA-G1** (per BA.md §Hard gates): `JsonParser::parse(twitter.json)` ≤ 400 µs on M1 Pro.
- **Closes BA-G2** (per BA.md): ≤ 2 heap allocations per parse-call; zero `Vec<OpenFrame>::clone`.
- **Begins Lock 1 honour** (per BA.md §13-Lock honoured): JSON OpenFrame retires; CSS L4/BBNF/Sheets/cohort follow at W5b/c/d/e (same tranche).
- **Honours Lock 9** (per BA.md): JSON parse returns slice-borrowed `JsonValue<'p>`.
- **Preceding wave**: BA.W4c (legacy lowering deletion).
- **Following wave**: BA.W5b (CSS L4 direct-to-struct migration).
- **Producing**: direct-projection emitter pattern; W5b consumes the pattern at scale.
- **No carry to BB**: option (a) per `docs/tranches/BA/audit/W5-substrate-identity-decision.md` — BA owns the all-grammar migration; BB.W1 retires.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo xtask regen --grammar json` | ≤ 25 s | exit 0 | Per-grammar regen for JSON direct-projection emit |
| `cargo bench --bench bench_json -- twitter` | ≤ 60 s | mean ≤ 400 µs | BA-G1 closer-gate |
| `cargo bench --bench bench_json -- twitter --features dhat` | ≤ 90 s | ≤ 2 allocs/iter | BA-G2 closer-gate |
| `samply record --save-only -- cargo bench --bench bench_json -- twitter` | ≤ 90 s | exit 0 | Vec<OpenFrame>::clone retiral verification |
| `cargo nextest run -p bbnf -E 'test(json)' --profile ax-iter` | ≤ 18 s | 100% | JSON test cohort post-direct-projection |
| `wc -l crates/core/src/grammar/generated/json.rs` | < 1 s | ≤ 2,200 | Post-W5a generated-LOC budget gate |

## §8 — Verification artefacts

- `docs/tranches/BA/audit/W5a-json-direct-to-struct.md` — per-grammar disposition artefact (M7).
- `docs/tranches/BA/audit/W5-generated-parser-shape.md` — per-construct emission shapes (W5a is the canonical exemplar; W5b..W5e cite per-construct deviations).

## §9 — Audit lane forecast

The W5a audit lane forecast: post-W5a, the following lanes are closed:

- Lane 04 (sota anchoring) — JSON-side BA-G1 closes the sonic-rs comparison row.
- Lane 06 (generated code budget) — `json.rs` ≤ 2,200 closes the W5 windows table for JSON.
- Lane 09 (closer-gate ratification) — BA-G1 + BA-G2 are closer-gates.

Lanes still open: Lane 05 (substrate audit) closes at W5e; Lane 03 (cross-tranche carry) closes at W6.

## §10 — Phase-4 surgery ledger

| Surgery # | Description | Landed at | Verification |
|---|---|---|---|
| 1 | Delete CSS L4/BBNF/Sheets `OpenFrame` preservation OR move full migration to BA.W6 | W5a (JSON side; non-JSON side at W5b/c/d/e) | M6 + W5b/c/d/e closer-gates |
| 2 | Lock 1 substrate identity flipped from option (b) to option (a) per user override; W5 split into W5a..W5e per-grammar | W5a + W5b + W5c + W5d + W5e | per-sub-wave closer-gates |
