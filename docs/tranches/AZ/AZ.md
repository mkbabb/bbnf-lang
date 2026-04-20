# Tranche AZ — Replay, Recovery, Incremental Re-Parse

AZ ships the developer-tooling layer over the AX substrate: decision-log replay, structural-default recovery, incremental re-parse, test-case minimisation, parse-step debugging. The old AX (pre-reckoning) carried these as X0–X3; they moved out of AX because AX's architectural scope — interpreter deletion + novel-lever portfolio + parity harnesses — was load-bearing enough on its own. AZ is the tooling tranche it deserves to be.

AZ operates on an AX-clean substrate: no DTA, no `DtaSnapshot`, no `dispatch_one`-based replay. Every AZ primitive is built freshly on the shape-emitter's decision points — RD-native by construction. No legacy hooks, no shims from the DTA era are carried forward; AX deleted them at W0, and AZ does not re-introduce them.

## Architectural thesis

Four propositions:

1. **Replay is a re-walk of the shape emitter's decision sequence, not a DTA state-id trace.** At every Alt / Wrap / Keyword-dispatch / ByteDispatch call site, the shape emitter (under `AZ::feature("replay")`) records a `DecisionRecord { rule_id: RuleId, shape_tag: ShapeTag, branch_idx: u8 }`. Replay re-enters `parse_<grammar>_<root>` with the log as a branch-hint source; the walker dispatches per-record against log entries instead of byte predicates. When the log is consistent with the input, the walker produces a bit-identical tape without re-inspecting bytes at dispatch points (bytes are still read for string/number/regex bodies where the log is insufficient). The log is compact — ~1 byte per shape-level decision, far smaller than the input.

2. **Snapshot-and-resume is a shape-stack capture, not a frame-stack capture.** The `ShapeSnapshot` records the shape emitter's call-graph position via `(pos, columns_len, shape_stack: [ShapeTag; 32], depth: u8, slot: u32)`. Resume is a function call into the shape emitter's `parse_resume_<grammar>(snapshot, input, visitor)` entrypoint — re-enters the call graph at the saved shape, NOT a re-hydrated interpreter state. Stack depth ≤ 32 (matches AW-V FrameStack cap); snapshot is ~80 bytes; O(1) in input size.

3. **Incremental re-parse is two snapshots, a subtree re-walk, and a Columns splice.** Given `Edit { old_lo, old_hi, new_bytes }`:
   - Binary-search master tape's `span_lo` for highest record fully containing `[old_lo, old_hi]`.
   - That record's snapshot metadata (populated during original parse at every shape-begin boundary) gives the `ShapeSnapshot` to resume from.
   - Call `parse_resume_<grammar>(snapshot, new_input_slice, visitor)`.
   - Splice resulting Columns at the affected record range; shift downstream `span_lo`/`span_hi` by byte delta.
   - `SHAPE_DICT` + bloom+GADT dedup re-fire on the spliced slice; structurally identical re-parses hit the prior tape verbatim.
   
   Tree-sitter ships this as its headline feature; AZ ships it over RD because the shape emitter's decision points are already natural snapshot boundaries. No bytecode interpreter required.

4. **Recovery walks the shape stack upward for a structural-sync byte.** Every shape has a mined "sync byte set" (from `structural_alphabet.rs`): CSS `{`, `}`, `;`; JSON `,`, `]`, `}`; BBNF `;`, `.`; Sheets newline. On dispatch failure:
   - For current shape, look up sync bytes from `GRAMMAR_PROFILE.sync_bytes[shape_tag]`.
   - Jump pos to next sync byte via stage-1 kind-separated stream (AX.2.3 substrate) or SIMD `find_next_of`.
   - Pop current shape frame; resume at parent shape.
   - Emit `RecoveryEvent { failed_at, synced_to, recovered_shape }` to the decision log.
   
   `@recover ruleName syncExpr ;` grammar directive refines to an override on the structural default — per the old AX Phase 3 audit, with deprecation analysis shipped in `recover-audit.md`.

## Invariants

1. **No DTA carry-forward.** No `DtaSnapshot`, no `dispatch_one`, no `DtaTable`. If any AZ design needs these primitives, the design is wrong — re-architect over the shape emitter's decision points.

2. **Substrate with consumer.** Every AZ wave ships substrate + consumer in one unit; no substrate-without-consumer landings. `ShapeSnapshot` without `parse_resume_<grammar>` consumer = not landed. Decision log without inspector CLI consumer = not landed.

3. **One recovery mechanism.** Structural-default is the primary recovery path; `@recover` is an override. No per-grammar recovery hacks, no fallback parsers, no dual paths.

4. **Wire-contract end-to-end tests per AZ output.** Every emitted `pub const` AZ introduces (per-grammar `SYNC_BYTES[shape_tag]`, `RESUME_ENTRYPOINTS[rule_id]`) carries a wire-contract test.

5. **Replay is a property, not a mode.** Under `dta-replay` — rename to `replay` — feature flag, the shape emitter emits `record_decision(rule_id, shape_tag, branch_idx)` calls inline at each dispatch point. Feature-off: no runtime cost. Feature-on: ~5% cold-parse regression (measured in Z0 close ledger). No separate "replay parser"; one emitter, two feature-gated variants.

6. **Incremental is a property, not a path.** One shape emitter, one `parse_<grammar>_<root>` entrypoint; `parse_resume_<grammar>` is a sibling that shares the emitter's shape templates — it consumes a snapshot as initial state instead of parsing from position 0. No alternate "incremental parser" with divergent semantics.

7. **AX closes first.** AZ opens only after AX.W12 close per wave-verification-ledger discipline. AX's substrate (shape emitter, Value API, hybrid tape, parity harnesses) is authoritative ground truth.

## Scope

1. **Z0 — Substrate: `ShapeSnapshot` + per-record metadata + decision log emit.** Shape emitter extension to emit snapshot capture at every shape-begin + decision record at every dispatch point, both feature-gated. Per-record snapshot column populated during parse.

2. **Z1 — `parse_resume_<grammar>` entrypoint + snapshot serde.** Shape emitter emits `parse_resume_<grammar>(snapshot, input, visitor)` alongside `parse_<grammar>_<root>`. `ShapeSnapshot` stable bincode serde with round-trip parity tests per grammar.

3. **Z2 — Incremental re-parse.** Edit-localisation via span binary-search; subtree re-walk via `parse_resume`; Columns splice + span-shift in one linear pass. Per-grammar incremental parity harness: edit + incremental re-parse produces bit-identical tape to cold re-parse of post-edit buffer.

4. **Z3 — Recovery + `@recover` audit.** Structural-default recovery in shape emitter (dispatch-failure → sync-byte walk); `@recover` semantics refined to override per rule level; `recover-audit.md` ships per-site disposition.

5. **Z4 — Replay tooling.** CLI: `inspect-log` (per-transition trace dump), `minimise` (O(log n × parse) shrink loop), `replay-test` (#[test] macro capturing log + asserting bit-identical re-walk).

6. **Z5 — Cranelift JIT per-schema** (moved from AX per scope re-sequencing). Opt-in per-grammar JIT + SHA cache. Separate from replay/recovery/incremental substrate but lives in AZ as post-performance-terminus feature.

7. **Z6 — FINAL.** FINAL.md + `post-AZ.json` (bench regression ≤ 5% vs AX close default-features; tranche value is feature-correctness + JIT option, not throughput on default path).

## Wave schedule

Seven waves.

| Wave | Scope | Agents | Opens after | Hard gate |
|------|-------|--------|-------------|-----------|
| **Z0** Substrate | `ShapeSnapshot` type + decision-log emit (feature-gated) + per-record snapshot column | 3 parallel (snapshot type, emitter extension, per-record column) | AX.W12 close | Feature-off: no regression. Feature-on: ≤ 5% cold-parse regression (documented in `post-AZ-Z0.json`). Per-record snapshot column populated on every list-rule / array-element / rule / formula-line boundary. |
| **Z1** Resume entrypoint | `parse_resume_<grammar>` emit + `ShapeSnapshot` bincode serde + round-trip parity | 2 parallel (emitter, serde) | Z0 | `ShapeSnapshot` bincode round-trips for every grammar; `parse_resume` re-enters at snapshot state and produces tape-identical output to a cold parse from the snapshot's `pos`. |
| **Z2** Incremental re-parse | Edit-localisation + subtree re-walk + Columns splice | 2 parallel (edit-local + splice, incremental harness) | Z1 | 100 KB CSS edit median ≤ 200 µs; 10 KB JSON edit ≤ 50 µs; 1 KB Sheets edit ≤ 10 µs; incremental-parity harness: per-grammar canonical edits (insert / delete / replace) produce bit-identical tapes vs cold re-parse on ≥ 50 test cases per grammar. |
| **Z3** Recovery + `@recover` audit | Structural-default recovery in shape emitter + `@recover` semantics refinement + deprecation audit | 2 parallel (recovery impl, audit) | Z2 | Per-grammar recovery harness: malformed inputs (truncated CSS rules, missing JSON braces, BBNF syntax errors mid-grammar, Sheets formula errors mid-row) each assert a specific recovery point + downstream continuation. `recover-audit.md` ships per-site verdict for every `@recover` annotation in BBNF self-host; deprecation decision recorded. |
| **Z4** Replay tooling | `inspect-log` CLI + `minimise` CLI + `replay-test` #[test] macro | 3 parallel (one per tool) | Z3 | `bbnf-cli inspect-log <grammar> <input>` emits a per-transition trace dump with (byte_offset, rule_id, shape_tag, branch_idx, frame_depth). `minimise` shrinks a 1 KB malformed input to ≤ 32 bytes in O(log n) parse calls. `replay-test` macro: log-record → replay → assert tape-identical across all corpus fixtures. |
| **Z5** Cranelift JIT per-schema | Opt-in JIT (`jit` crate) + SHA cache on grammar AST hash + workload-profile-informed specialization | 2 parallel (JIT emitter, cache) | Z4 | Canada JIT 1.10-1.25× AOT; JIT compile ≤ 5 ms per grammar; cache cold-start < 1 µs; `ParseOptions::jit = true` default off; feature-off: no change from Z4 |
| **Z6** FINAL | `FINAL.md`, `post-AZ.json`, aggregator | 1 serial | Z5 | Replay/recovery/incremental + JIT feature complete; `cargo test --workspace` green; `cargo test --features replay` green; `cargo test --features jit` green; default-features cold-parse regression ≤ 5% vs AX close |

## Phases

### Phase 0 — Substrate (Z0)

Three agents, parallel.

#### AZ.0.1 — `ShapeSnapshot` type

Owner: `crates/bbnf-tape/src/snapshot.rs` (**new**).

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ShapeSnapshot {
    pub pos: u32,                    // byte offset in input
    pub columns_len: u32,            // tape records committed so far
    pub shape_stack: [ShapeTag; 32], // grammar nesting, LIFO
    pub depth: u8,                   // current stack depth (0..=32)
    pub slot: u32,                   // stage-1 kind-separated stream cursor (or 0 if not used)
    pub payload_stream_len: u32,     // payload arena cursor for rollback
}

impl ShapeSnapshot {
    pub fn new() -> Self { /* zero-init */ }
    pub fn push(&mut self, tag: ShapeTag) { /* bounds-check depth < 32 */ }
    pub fn pop(&mut self) -> ShapeTag { /* bounds-check depth > 0 */ }
    pub fn current(&self) -> ShapeTag { self.shape_stack[self.depth as usize - 1] }
}
```

No `DtaState`, no `FrameStack`, no `Vec`. 32-element stack-allocated shape-tag array (max depth 32 per AW-V). Total size ~80 bytes.

#### AZ.0.2 — Shape emitter decision-log extension

Owner: `crates/core/src/backend/rust/emitter/shapes/*.rs`; `crates/core/src/backend/rust/emitter/decision_log.rs` (**new**).

At every dispatch point in a shape emitter output, splice a feature-gated `record_decision` call:

```rust
match input[pos] {
    b'{' => {
        #[cfg(feature = "replay")]
        replay::record_decision(RULE_ID, ShapeTag::Object, 0);
        parse_object(...)
    }
    b'[' => {
        #[cfg(feature = "replay")]
        replay::record_decision(RULE_ID, ShapeTag::Array, 1);
        parse_array(...)
    }
    // ...
}
```

The `#[cfg(feature = "replay")]` gate compiles the call away under default features; under `replay` feature, calls emit to a thread-local `DecisionLog`.

#### AZ.0.3 — Per-record snapshot column

Owner: `crates/bbnf-tape/src/columns.rs` (**extend**).

New SoA column: `snapshot_at: Vec<ShapeSnapshot>`, sparse. Populated during parse at every list-rule / array-element / rule / formula-line boundary via `Columns::mark_snapshot(pos, shape_stack)` called from shape emitter at shape-begin boundaries.

Cost per tailwind.css (~12k rulesets): ~12k × 80 B ≈ 960 KB, comparable to tape size. Cheaper grammars carry proportionally fewer snapshots.

**Hard gate** (Z0): feature-off cold-parse bench identical to AX close (replay calls compiled away); feature-on cold-parse regression ≤ 5% (measured and recorded in `post-AZ-Z0.json`); per-record snapshot column populated at every list-rule boundary per per-grammar fixture assertion.

### Phase 1 — Resume entrypoint (Z1)

Two agents, parallel.

#### AZ.1.1 — `parse_resume_<grammar>` emission

Owner: `crates/core/src/backend/rust/emitter/resume.rs` (**new**).

Shape emitter emits a sibling entrypoint per grammar:

```rust
pub fn parse_resume_<grammar><V: GrammarVisitor>(
    snapshot: &ShapeSnapshot,
    input: &[u8],
    visitor: &mut V,
) -> Result<(), ParseError> {
    // Re-enter at snapshot's current shape with appropriate state
    match snapshot.current() {
        ShapeTag::Object => parse_<grammar>_object_resume(snapshot, input, visitor),
        ShapeTag::Array => parse_<grammar>_array_resume(snapshot, input, visitor),
        ShapeTag::Pratt => parse_<grammar>_pratt_resume(snapshot, input, visitor),
        // ...
    }
}
```

Each per-shape `*_resume` function is emitted by the same per-shape emitter module (adds a `_resume` variant to each template). The variant takes the snapshot's `shape_stack` as pre-populated state instead of starting from the shape's begin-boundary.

#### AZ.1.2 — `ShapeSnapshot` serde + round-trip tests

Owner: `crates/bbnf-tape/src/snapshot.rs` (**extend**); `crates/core/tests/snapshot_roundtrip.rs` (**new**).

bincode derive on `ShapeSnapshot`. Per-grammar round-trip tests: `snapshot → bytes → snapshot` produces identical value; `parse → capture snapshot at boundary → serialize → deserialize → parse_resume → tape compare` produces bit-identical continuation.

**Hard gate** (Z1): `ShapeSnapshot` round-trips via bincode for every grammar; `parse_resume_<grammar>(snapshot, input, visitor)` produces tape-identical output to a cold parse from `snapshot.pos` across ≥ 50 fixture cases per grammar.

### Phase 2 — Incremental re-parse (Z2)

Two agents, parallel.

#### AZ.2.1 — Edit-localisation + subtree re-walk

Owner: `crates/bbnf-tape/src/incremental.rs` (**new**).

```rust
pub struct Edit<'a> {
    pub old_lo: u32,
    pub old_hi: u32,
    pub new_bytes: &'a [u8],
}

pub fn incremental_reparse<V: GrammarVisitor>(
    master_tape: &mut Tape,
    master_input: &mut Vec<u8>,
    edit: Edit<'_>,
    visitor: &mut V,
) -> Result<(), ParseError> {
    // 1. Binary-search master_tape.span_lo for highest record fully containing [old_lo, old_hi]
    let record_idx = master_tape.locate_covering_record(edit.old_lo, edit.old_hi)?;
    let snapshot = master_tape.snapshot_at[record_idx];
    
    // 2. Apply edit to input buffer
    master_input.splice(edit.old_lo as usize .. edit.old_hi as usize, edit.new_bytes.iter().copied());
    
    // 3. parse_resume from snapshot
    let new_columns = parse_resume_<grammar>(&snapshot, &master_input[snapshot.pos as usize..], visitor)?;
    
    // 4. Splice new_columns into master at affected record range
    master_tape.splice_records(record_idx, record_idx + affected_count, new_columns);
    
    // 5. Shift downstream span_lo/span_hi by byte delta
    let delta = edit.new_bytes.len() as i64 - (edit.old_hi - edit.old_lo) as i64;
    master_tape.shift_spans_after(record_idx + affected_count, delta);
    
    Ok(())
}
```

Splice is a linear pass per affected column; span-shift is a second linear pass. `SHAPE_DICT` + bloom+GADT re-fire on the spliced slice (AX.2.6 substrate), so structurally identical re-parses hit prior tape verbatim.

#### AZ.2.2 — Incremental parity harness

Owner: `crates/core/tests/incremental_parity.rs` (**new**).

Per grammar × edit-kind × scope:

| Grammar | Edit kinds | Scopes |
|---|---|---|
| JSON | insert / delete / replace | scalar value, object key, array element, nested object |
| CSS | insert / delete / replace | declaration, selector, at-rule, nested rule |
| Sheets | insert / delete / replace | cell ref, operator, function call |
| BBNF | insert / delete / replace | rule body, alt branch, regex literal |

50 cases per grammar = 200 cases total. Each asserts: `cold_parse(post_edit_buffer).tape == incremental_reparse(cold_parse(pre_edit_buffer), edit).tape` bit-for-bit.

**Hard gate** (Z2): 100 KB CSS edit median ≤ 200 µs (measured on `data/css/bootstrap.css` edits); 10 KB JSON edit ≤ 50 µs; 1 KB Sheets edit ≤ 10 µs; incremental-parity harness 200/200 green.

### Phase 3 — Recovery + `@recover` audit (Z3)

Two agents, parallel.

#### AZ.3.1 — Structural-default recovery

Owner: `crates/bbnf-tape/src/recovery.rs` (**new**); `crates/core/src/backend/rust/emitter/shapes/*.rs` (**extend with recovery branches**).

Each shape emitter's dispatcher gains a recovery branch when every byte-arm has failed:

```rust
match input[pos] {
    b'{' => parse_object(...),
    b'[' => parse_array(...),
    // ... all the admitted bytes ...
    Some(b) => {
        #[cfg(feature = "replay")]
        replay::record_event(RecoveryEvent::DispatchFailed { at: pos, byte: b });
        
        // Walk shape stack upward to find sync byte
        let sync_bytes = GRAMMAR_PROFILE.sync_bytes[shape_stack.current()];
        let sync_pos = find_next_of(input, pos, sync_bytes);
        
        match sync_pos {
            Some(p) => {
                pos = p;
                shape_stack.pop();
                // Continue at parent shape
            }
            None => return Err(ParseError::UnrecoverableDispatchFailure(pos, b)),
        }
    }
    None => return Err(ParseError::Eof(pos)),
}
```

`find_next_of` uses the AX.2.3 kind-separated stream when available OR a SIMD `multi_cmp_scan`.

`RecoveryEvent` enum:
```rust
pub enum RecoveryEvent {
    DispatchFailed { at: u32, byte: u8, recovered_shape: ShapeTag, resumed_at: u32 },
    RecoverDirectiveFired { rule_id: RuleId, sync_expr: &'static str, at: u32, resumed_at: u32 },
    Unrecoverable { at: u32, reason: UnrecoverableReason },
}
```

#### AZ.3.2 — `@recover` semantics refinement + deprecation audit

Owner: `crates/ir/src/passes/recognizers/recover.rs` (**extend**); `docs/tranches/AZ/recover-audit.md` (**new**).

`@recover ruleName syncExpr ;` refines: `syncExpr` overrides the structural-default `sync_bytes[shape_tag]` for `ruleName`'s level. Existing BBNF recovery annotations work unchanged; rules without `@recover` get the structural default.

**Deprecation audit**: enumerate every `@recover` site in the BBNF self-host grammar (`grammar/bbnf/*.bbnf`). JSON / CSS / Sheets declare none. For each site:

1. Name the site: `(file:line, rule_name, sync_expr)`.
2. Derive the structural-default sync byte for that rule's shape.
3. Compare: does the explicit `syncExpr` differ from the default?
4. Per-site verdict: `redundant` (remove in a follow-on cleanup) OR `required` (keep; document why).
5. Aggregate: if every site is redundant, route `@recover` for removal in a future cleanup tranche (grammar-syntax simplification). If any is required, retain.

The audit ships as `docs/tranches/AZ/recover-audit.md` with a table of sites + verdicts + aggregate recommendation.

**Hard gate** (Z3): per-grammar recovery harness (canonical malformed inputs per grammar, each asserting specific recovery point + downstream continuation); `@recover` preserves AT/AU bootstrap-recovery behaviour (BBNF self-hosting recovery test suite passes unchanged); `RecoveryEvent` stream consumable by the Z4 CLI inspector; `recover-audit.md` lands with every site's verdict.

### Phase 4 — Replay tooling (Z4)

Three agents, parallel.

#### AZ.4.1 — `inspect-log` CLI

Owner: `crates/bbnf-cli/src/inspect_log.rs` (**new**).

```bash
cargo run -p bbnf-cli -- inspect-log <grammar> <input>
```

Parses with `--features replay` enabled; dumps the decision log with per-transition annotation:

```
byte_offset  rule_id     shape_tag  branch_idx  frame_depth
0            root        Object     0           0
1            pair        Object     0           1
1            string      String     0           2
15           string      String     EOF         2
16           value       Value      0           1
...
```

Also emits `RecoveryEvent` stream entries inline. Reads as a parse trace at the shape-level granularity.

#### AZ.4.2 — `minimise` CLI

Owner: `crates/bbnf-cli/src/minimise.rs` (**new**).

```bash
cargo run -p bbnf-cli -- minimise <grammar> <input>
```

Given a malformed input that triggers a parse error, produce the shortest substring that triggers the same error. Binary-search prefixes and suffixes; the decision log identifies when the failing transition first appears. O(log n × parse cost) shrink.

#### AZ.4.3 — `replay-test` `#[test]` macro

Owner: `crates/bbnf-tape/src/replay.rs` (**new**); `crates/bbnf-tape-macros/src/replay.rs` (**new proc-macro crate**).

```rust
#[bbnf_tape::replay_test(grammar = "json", input = "data/json/twitter.json")]
fn twitter_replay_roundtrip() { /* macro generates body */ }
```

Macro expansion:
1. Parse input with `--features replay`; capture decision log + tape.
2. Replay: re-run the parser with log injected as branch hints; capture replay-tape.
3. Assert `original_tape == replay_tape` bit-for-bit.

Catches regressions in the shape emitter that would change decision sequence even when the final tape matches.

**Hard gate** (Z4): `inspect-log` produces per-transition trace for every grammar; `minimise` shrinks a 1 KB malformed input to ≤ 32 bytes in O(log n) parse calls; `replay_test` macro generates passing tests across all corpus fixtures for every grammar.

### Phase 5 — FINAL (Z5)

One serial agent.

`docs/tranches/AZ/FINAL.md` + `docs/benchmarks/post-AZ.json`.

Bench posture: AZ's feature value is tooling, not throughput. Default-features cold-parse regression ≤ 5% vs AX close (the `mark_snapshot` per list-rule boundary has a small cost even feature-off — documented). `--features replay` cold-parse regression ≤ 10% vs default-features (documented).

Tranche summary:
- Per-wave ledger with verification artefacts (Z0 substrate; Z1 round-trip harness; Z2 incremental parity; Z3 recovery harness + audit; Z4 CLI output samples).
- Ignored tests: 0 (inherit from AX.W11).
- Cross-tranche invariants: `cargo test --workspace` green; `cargo test --features replay` green.

**Hard gate** (Z5):
- `cargo test --workspace` green; zero `#[ignore]` additions.
- `cargo test --features replay` green on every grammar.
- Incremental-parity harness green (200/200 cases).
- Recovery harness green per grammar.
- `inspect-log` / `minimise` / `replay-test` CLI + macro functional across all primary grammars.
- Default-features bench regression ≤ 5% vs AX close.
- `recover-audit.md` ships with aggregate recommendation.

## Critical files

| File | Phase |
|------|-------|
| `crates/bbnf-tape/src/snapshot.rs` (**new** — `ShapeSnapshot` type + serde) | 0, 1 |
| `crates/core/src/backend/rust/emitter/decision_log.rs` (**new** — record_decision splice) | 0 |
| `crates/core/src/backend/rust/emitter/shapes/*.rs` (**extend** — decision-log splice + recovery branches) | 0, 3 |
| `crates/bbnf-tape/src/columns.rs` (**extend** — `snapshot_at` sparse column) | 0 |
| `crates/core/src/backend/rust/emitter/resume.rs` (**new** — `parse_resume_<grammar>` emit) | 1 |
| `crates/bbnf-tape/src/incremental.rs` (**new** — edit-localisation + splice + span-shift) | 2 |
| `crates/core/tests/incremental_parity.rs` (**new** — 200-case harness) | 2 |
| `crates/bbnf-tape/src/recovery.rs` (**new** — structural-default + `RecoveryEvent`) | 3 |
| `crates/ir/src/passes/recognizers/recover.rs` (**extend** — `@recover` refinement) | 3 |
| `crates/core/tests/recovery_parity.rs` (**new** — per-grammar recovery harness) | 3 |
| `docs/tranches/AZ/recover-audit.md` (**new** — per-site audit) | 3 |
| `crates/bbnf-cli/src/inspect_log.rs` (**new** — log inspector CLI) | 4 |
| `crates/bbnf-cli/src/minimise.rs` (**new** — test-case minimiser CLI) | 4 |
| `crates/bbnf-tape/src/replay.rs` (**new** — replay substrate) | 4 |
| `crates/bbnf-tape-macros/src/replay.rs` (**new** — `replay_test` proc macro) | 4 |
| `crates/core/tests/replay_roundtrip.rs` (**new** — per-grammar `replay_test` invocations) | 4 |
| `docs/tranches/AZ/{PROGRESS,FINAL}.md` + `docs/benchmarks/{post-AZ,post-AZ-Y{0..5}}.json` | 0–5 |

## Hard gates summary

### Z0 — Substrate
1. `ShapeSnapshot` type + bincode derive.
2. `#[cfg(feature = "replay")]` decision-log splice at every dispatch point in every shape emitter.
3. `snapshot_at` sparse column populated at every list-rule / array-element / rule / formula-line boundary.
4. Default-features cold-parse: no regression vs AX close.
5. `--features replay` cold-parse: ≤ 5% regression, documented in `post-AZ-Z0.json`.

### Z1 — Resume
6. `parse_resume_<grammar>` emitted per grammar.
7. `ShapeSnapshot` bincode round-trips for every grammar.
8. Per-grammar fixture: `parse_resume` from snapshot produces tape-identical output to cold parse from same `pos` (≥ 50 cases per grammar).

### Z2 — Incremental
9. 100 KB CSS edit median ≤ 200 µs.
10. 10 KB JSON edit ≤ 50 µs.
11. 1 KB Sheets edit ≤ 10 µs.
12. Incremental-parity harness: 200/200 green (insert / delete / replace × scalar / compound × 4 grammars × scopes).
13. `SHAPE_DICT` + bloom+GADT re-fire on spliced slice (structurally identical re-parses hit prior tape verbatim).

### Z3 — Recovery + audit
14. Per-grammar recovery harness green (malformed inputs each assert recovery point + continuation).
15. `@recover` preserves AT/AU bootstrap-recovery behaviour.
16. `recover-audit.md` ships per-site verdicts + aggregate recommendation.
17. `RecoveryEvent` stream consumable by `inspect-log` CLI.

### Z4 — Replay tooling
18. `inspect-log <grammar> <input>` produces per-transition trace for every grammar.
19. `minimise` shrinks 1 KB → ≤ 32 B in O(log n) parse calls.
20. `replay_test` macro: log-record → replay → assert tape-identical across all corpus fixtures for every grammar.

### Z5 — FINAL
21. `cargo test --workspace` green; 0 `#[ignore]`.
22. `cargo test --features replay` green per grammar.
23. Default-features bench regression ≤ 5% vs AX close.
24. `--features replay` bench regression ≤ 10% vs default-features.
25. `FINAL.md` + `post-AZ.json` land.

## Indefatigability

AZ is the tooling tranche. Performance is not the gate; feature-correctness is. When AZ closes:

- RD-native replay: decision log is a trace of shape-emitter dispatches, not a DTA state-id log.
- RD-native incremental: `parse_resume_<grammar>` re-enters the shape emitter's call graph from a captured `ShapeSnapshot`.
- RD-native recovery: structural-default sync walks the shape stack, not a frame stack.
- CLI tooling: `inspect-log`, `minimise`, `replay-test` all operational.
- Zero DTA carry-forward: no `DtaSnapshot`, no `dispatch_one`, no `DtaTable` is consumed anywhere. The AX deletion holds.
- `cargo test --workspace` green; zero ignored.

Post-AZ, the codebase has:
- One codegen path (AX).
- One runtime shape (shape emitter).
- One snapshot primitive (`ShapeSnapshot`).
- One replay mechanism (decision log over shape dispatches).
- One recovery mechanism (structural-default + `@recover` override).
- One incremental mechanism (edit → snapshot → resume → splice).

The architectural terminus. AX proved performance; AZ proved developer ergonomics. Successor tranches — if any — address new language features, new target backends, or new performance horizons on top of this substrate.
