# Tranche AS — Parse Activation, Scanner Truth, Direct Projection

## Diagnosis

AR landed the substrate: discriminator split, payload layouts (14
across JSON+CSS L4), mixed Alt enum view, KvPair tape kind, SIMD
fractional scan, host.rs structural extraction, FAMILY_HELPER policy
move. But the generated parsers are broken — every monolithic and VM
bench target fails at parse time. The infrastructure is wired but
not activated end-to-end. This is a pattern: AQ built infrastructure,
AR activated some of it, AS must close the loop.

Six concurrent audits identified five root causes that converge on
the same activation gap.

### Root cause 1 — generated parsers fail at parse time

All 7 monolithic/VM bench targets fail. `json_monolithic` fails at
offset 1 on `{`. `css_l4` fails at offset 196. `json_vm` fails on
canada.json. The compile_pipeline bench works (grammars compile
correctly), but the compiled parsers can't parse input. The codegen
produces syntactically valid Rust that compiles but generates
incorrect parsing logic — likely a dispatch table or Alt branch
ordering issue introduced in the AQ.5/AQ.6 era.

### Root cause 2 — bootstrap regen coherence break

`scripts/bootstrap-bbnf.sh` produces a 21,672-line parser that
cannot parse ANY `.bbnf` grammar file. The current working
`generated.rs` (24,685 lines) was built before the host.rs
refactor. The new host.rs `peel_wrappers` changes the extracted AST
enough that the IR differs, producing a different parser. The 6/6
grammar_roundtrip tests pass (extraction is correct), but the
resulting parser doesn't work. The bootstrap loop is open.

### Root cause 3 — Identifier sub-flags ignored in codegen

CSS `selectorIdent` classifies correctly as
`Identifier { allows_escapes: true }` via structural detection in
bbnf-regex. The `FAMILY_HELPER` bit is set. The recognizer miner
captures it. But `plan_regex_scanner` at `scanner_plan.rs:146` uses
`RegexClass::Identifier { .. }` — a catch-all that discards
`allows_escapes`, `allows_leading_dash`, and
`allows_double_dash_prefix`. The escape-aware scanner is never
emitted. The classification work is wasted.

### Root cause 4 — six hand-rolled string parsers duplicate HIR

`generate/regex/emit/` contains six sites that re-parse regex
pattern strings to extract information the HIR already provides:
negated char-class parsing, bracket/range detection, shorthand class
matching, ws-padded literal detection, and SIMD positive-class
range extraction. Each pays a full parse cost when
`RegexInfo.classification` and `RegexInfo.quantified_class` already
carry the answers.

### Root cause 5 — Span not admitted as scalar payload

`TypeDesc::Span` was attempted and reverted (commit `c0ef0be`)
because `push_leaf_with_Span` / `payload_Span` don't exist.
Without Span as a scalar, `Tuple([Span, scalar])` layouts can't
form naturally — they require the KvPair special case. CSS L4
`important = (Span, BoxedEnum, Span)` and similar multi-Span tuples
remain excluded from payload projection.

## Bench infrastructure state

| Target | Status | Notes |
|--------|--------|-------|
| compile_pipeline | **WORKING** | JSON 0.12ms, CSS L4 9.4ms |
| css_competitors | **WORKING** | cssparser 465 MB/s, lightningcss 129 MB/s |
| css_stress | **WORKING** | up to 28 GB/s on selectors |
| json_parse_that | **WORKING** | combinator baseline 610-1108 MB/s |
| json_monolithic | **BROKEN** | offset 1 parse failure |
| json_stress | **BROKEN** | offset 104 parse failure |
| json_vm | **BROKEN** | VM parse failure |
| css_l4 | **BROKEN** | offset 196 parse failure |
| css_vm | **BROKEN** | VM incomplete parse |
| google_sheets_* | **BROKEN** | parse failure |
| runtime_root | **BROKEN** | API mismatch (fixed in AR) |

Post-AQ targets: JSON 1800-2700 MB/s, CSS L4 500-535 MB/s.
Combinator baseline: 610-1108 MB/s. Monolithic was 2-3x faster
than combinators when it worked.

## AS plan — 6 phases

### Phase 1 — Restore parse activation (~3 days)

The highest-leverage item. Every other measurement depends on
working parsers.

#### AS.1.1 Diagnose monolithic parse failure

Expand the JSON monolithic bench parser via `cargo expand`. Compare
the generated `__value` rule function's dispatch table against the
grammar's Alt branches. Find which byte-to-branch mapping is wrong
or missing. The failure at offset 1 (`{`) means the object branch
is unreachable.

#### AS.1.2 Fix codegen dispatch

The root cause is in the IR-to-Rust codegen pipeline — likely in
`generate_dispatch_tables` or the Alt branch ordering pass. The
compile_pipeline bench works (the IR is correct), so the issue is
in the final emission step. Fix the dispatch table generation or
the Alt branch ordering to produce correct byte→branch mappings.

#### AS.1.3 Fix VM parse path

The VM interpreter shares the IR but has its own execution path.
If the monolithic fix also fixes the VM, this is free. If not,
investigate the VM compiler's dispatch table encoding separately.

#### AS.1.4 Validate all 7 bench targets

**Hard gate**: all 12 bench targets compile and run without parse
failures. Post-AS.json has measured throughput for every dataset.

### Phase 2 — Close bootstrap loop (DONE — landed in AR audit phase)

#### AS.2.1 Diagnose regen IR diff

Save the working generated.rs. Regen. Diff the two IRs (not the
generated code — the IR that produced them). Use
`BBNF_IR_DUMP=1` or equivalent to emit the IR for the BBNF grammar
under both extraction paths. Find the specific rule or expression
that differs.

#### AS.2.2 Fix extraction/lowering coherence

The host.rs `peel_wrappers` change is architecturally correct (6/6
roundtrip green). The issue is downstream: the different IR
produces a parser with different behavior. Either the lowering
must normalize the IR to be peel-invariant, or the codegen must
handle both IR shapes.

#### AS.2.3 Regen + roundtrip + freeze

**Hard gate**: `scripts/bootstrap-bbnf.sh` produces a `generated.rs`
that passes 6/6 grammar_roundtrip and all bench parse tests.

### Phase 3 — Span scalar admission + direct projection (~2 days)

#### AS.3.1 Wire Span through builder + tape

Add `push_leaf_with_Span(kind, span_lo, span_hi, variant_idx,
meta_idx, value_lo: u32, value_hi: u32)` to `TapeBuilder`.
Add `payload_Span(rec) -> Option<(u32, u32)>` to `Tape`.
Restore the 4 lines reverted in `c0ef0be` in `type_desc.rs`.

#### AS.3.2 Wire Span through view layer

Add Span case to `scalar_value_fallback` in `view/leaves.rs`.
Add Span case to `emit_scalar_value_decode` in `view/alt.rs`.
Update `emit_tape_span_only_scalar_epilogue` for Span.

#### AS.3.3 Named struct projection

Add `StructRegistry` to `GrammarIR` mapping `StringId -> Vec<TypeDesc>`.
Extend `compute_payload_layouts` to resolve `Named` types through
the registry. Add emitter and view routing for Named payload types.

**Hard gate**: CSS L4 `important = (Span, BoxedEnum, Span)` has
Span fields recognized as scalar. At least one `Named` struct type
projects to a concrete layout.

### Phase 4 — Scanner truth (~3 days)

Eliminate the impedance mismatch between classification and emission.

#### AS.4.1 Wire Identifier sub-flags through plan_regex_scanner

Replace `RegexClass::Identifier { .. }` catch-all with explicit
dispatch on `allows_escapes`, `allows_leading_dash`,
`allows_double_dash_prefix`. Route to:
- `emit_call()` — generic identifier
- `emit_call_css()` — CSS with leading dash
- `emit_call_with_escapes()` — CSS with escape sequences (new)

#### AS.4.2 Implement `scan_ident_with_escapes` in parse-that

New scanner function that extends `scan_ident` to consume `\\`
followed by any non-newline byte. Wire through
`kernels::identifier::emit_call_with_escapes`.

#### AS.4.3 Kill six hand-rolled string parsers

Replace each with `RegexInfo` field matches:

| Current site | Replacement |
|-------------|-------------|
| `negated_class.rs` hand-rolled `[^XYZ]+` parser | `RegexClass::CharClassQuantified(ClassRangeInfo { negated: true, .. })` |
| `generalized/mod.rs` bracket/range/quantifier parsing | `ClassRangeInfo` fields directly |
| `generalized/mod.rs` ws-padded literal detection | HIR concat inspection |
| `mod.rs` comma-or-whitespace string match | New `RegexClass::Separator` variant |
| `mod.rs` SIMD positive-class range extraction | `ClassRangeInfo.chars` set |
| `mod.rs` shorthand class loop | `ClassRangeInfo` width |

#### AS.4.4 Replace 3 redundant classify_regex calls

- `leaves.rs:88,121` — use `opts.classify_regex(pattern)` (cached)
- `cost_model.rs:146-150` — resolve once, cache the bool

**Hard gate**: zero hand-rolled regex string parsers in
`generate/regex/emit/`. `grep -rn "classify_regex(" leaves.rs`
returns zero raw calls. `plan_regex_scanner` dispatches on all
Identifier sub-flags.

### Phase 5 — Profile and close sonic-rs gap (~2 days)

Depends on Phase 1 (working parsers).

#### AS.5.1 Fresh samply profiles

Run `samply record` for JSON (5 datasets) and CSS L4 (3 datasets)
against the post-AS parsers. Compare to AR-baseline profiles.

#### AS.5.2 Measure and report

Post-AS.json with measured throughput for every dataset. Delta vs
post-AQ baseline. Target: restore post-AQ numbers at minimum.

#### AS.5.3 Hot-path optimization pass

Based on samply evidence, not speculation. The profile will point
to the actual bottlenecks — likely in dispatch control flow,
whitespace handling, or tape push overhead.

**Hard gate**: post-AS.json has measured numbers for all 12 bench
targets. JSON twitter >= 2000 MB/s. CSS L4 bootstrap >= 500 MB/s.

### Phase 6 — Remaining scanner consolidation (~2 days)

The 6 AR.6 items that didn't ship:

#### AS.6.1 Collapse RegexClass passthrough miners into `RegexClassMiner`
#### AS.6.2 Single source of truth via `ScanLut` registry
#### AS.6.3 Fold regex-pattern-parser-lites into `RegexClassEmitter::route`
#### AS.6.4 Parameterize `WhitespaceWithBlockComment`
#### AS.6.5 Move `FnDescriptor` specialization to post-`compute_regex_info`
#### AS.6.6 Symmetric `kernels::number::emit_call_generic`

**Hard gate**: five regex-pattern-parser-lites collapsed to one
dispatch table. `wc -l` shows >= 350 LOC net reduction.

## Hard gates summary

1. All 12 bench targets run without parse failures (Phase 1)
2. Bootstrap regen produces working `generated.rs` (Phase 2)
3. `TypeDesc::Span` admitted as scalar with builder+tape methods (Phase 3)
4. `plan_regex_scanner` dispatches on Identifier sub-flags (Phase 4)
5. Zero hand-rolled regex string parsers in emit/ (Phase 4)
6. Post-AS.json with measured throughput (Phase 5)
7. JSON twitter >= 2000 MB/s (Phase 5)
8. Five regex miners collapsed to one dispatch (Phase 6)
9. `cargo test --workspace` no new failures (all phases)

## What is NOT in scope

- **Named struct ABI finalization**: AS.3.3 scaffolds it; the full
  ABI with cross-crate stability is a separate tranche.
- **Global CSP solve**: per-component sufficient.
- **ParsedGrammar elimination**: depends on stable bootstrap loop
  (Phase 2). After AS.2 proves stable, this becomes viable.

## Operational directives

- **6 parallel agents per wave**, isolated worktrees.
- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues**.
- **Commit frequently with `/commit`**.
- **Every claimed perf win has a samply diff**.
- **Clear ALL `.bbnf-cache` directories before any bench or regen**.
  `find . -name ".bbnf-cache" -exec rm -rf {} +` is mandatory.
- **`cargo expand` evidence for every codegen claim**.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1, 3 |
| `crates/core/src/backend/rust/emitter/leaves.rs` | 4 |
| `crates/core/src/backend/rust/view/{alt,leaves,seq}.rs` | 3 |
| `crates/ir/src/passes/sets/dispatch/*.rs` | 1 |
| `crates/ir/src/vm/compiler/*.rs` | 1 |
| `crates/core/src/grammar/{host,generated}.rs` | 2 |
| `crates/core/src/lower/expression.rs` | 2 |
| `crates/bbnf-tape/src/{builder,tape}.rs` | 3 |
| `crates/ir/src/types/type_desc.rs` | 3 |
| `crates/core/src/generate/regex/emit/*.rs` | 4 |
| `crates/core/src/generate/regex/emit/scanner_plan.rs` | 4 |
| `crates/core/src/backend/kernels/identifier.rs` | 4 |
| `parse-that/rust/parse_that/src/parsers/scan/ident.rs` | 4 |
| `crates/ir/src/passes/recognizers/*.rs` | 6 |
| `docs/benchmarks/post-AS.json` | 5 (NEW) |
