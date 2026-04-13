# Tranche AS — Scanner Truth, Direct Projection, CSS Activation

## Current state (post-AR audit fixes)

AR landed 30 commits. The AR audit phase fixed both chronic
blockers: modifier recovery in `lower_factor` closed the 10-tranche
bootstrap loop (idempotent gen1==gen2), and the same fix restored
JSON + Google Sheets monolithic benches.

### Feature wiring audit (12/12 wired)

| Feature | Status | Evidence |
|---------|--------|----------|
| Meta_idx side-channel | **WIRED** | 40+ uses in generated.rs for branch discrimination |
| Payload layouts (14) | **WIRED** | Gates accessor emission at view/mod.rs; 13 tests pass |
| Mixed Alt enum (Cursor variants) | **WIRED** | Emitted at alt.rs; generates typed+cursor enum |
| KvPair tape kind | **WIRED** | `is_kv_pair_shape` gates at view/mod.rs + layout.rs |
| SIMD fractional scan | **WIRED** | `mod number_simd;` declared, `scan_digits_simd` called at integer+fraction loops, 12 tests pass |
| CSS escape classifier | **WIRED** | `allows_escapes` dispatches in plan_regex_scanner for QuotedString |
| FAMILY_HELPER policy | **WIRED** | `has_kernel_coverage` called at regex_info.rs:60 |
| Tape capacity /2+2 | **WIRED** | Emitter source uses `input.len() / 2 + 2` |
| Host.rs structural extraction | **WIRED** | 607→423 LOC, 6/6 roundtrip green |
| TypeDesc::from_scalar_name | **WIRED** | Called 2× in lower/expression.rs for type suffix detection |
| is_nullable re-export | **WIRED** | Imported from bbnf-regex at hir/mod.rs:19 |
| Egraph clone reduction | **WIRED** | 5 clones (down from 11) in egraph crate |

### Bench infrastructure state (post-modifier-fix)

| Target | Status | Numbers |
|--------|--------|---------|
| compile_pipeline | **WORKING** | JSON 0.13ms, BBNF 1.6ms, CSS L4 9.7ms |
| json_monolithic | **WORKING** | canada 1097, citm 2353, twitter 2069, data 1832, data_xl 1084 MB/s |
| google_sheets_monolithic | **WORKING** | 122-129 MB/s |
| css_competitors | **WORKING** | cssparser 465 MB/s, lightningcss 129 MB/s |
| css_stress | **WORKING** | up to 28 GB/s on selectors |
| json_parse_that | **WORKING** | combinator baseline 610-1108 MB/s |
| css_l4 | **BROKEN** | bootstrap.css parse fails |
| css_vm | **BROKEN** | VM incomplete parse |
| json_vm | **NEEDS VERIFY** | may be fixed by modifier recovery |
| json_stress | **NEEDS VERIFY** | may be fixed by modifier recovery |

Post-AQ targets were invalid — those parsers silently dropped
modifiers. The post-AR numbers are the first CORRECT measurements.
sonic-rs: canada 1540, citm 3000, twitter 2643, data 2346 MB/s.

### Remaining root causes

#### Root cause 1 — CSS L4 parse still fails

JSON and Google Sheets parse correctly after the modifier fix.
CSS L4 does not. The CSS grammar likely has additional expression
shapes where modifier or binary-operator recovery fails —
possibly in the deeper nesting of CSS selector combinators,
media query expressions, or value function call syntax.

#### Root cause 2 — Identifier sub-flags ignored in codegen

CSS `selectorIdent` classifies as `Identifier { allows_escapes: true }`
but `plan_regex_scanner` at `scanner_plan.rs:146` uses
`RegexClass::Identifier { .. }` catch-all. `allows_escapes`,
`allows_leading_dash`, and `allows_double_dash_prefix` are
discarded. The escape-aware scanner is never emitted.

NOTE: `allows_escapes` IS dispatched for `QuotedString` (line 133),
just not for `Identifier`. The fix is to mirror that pattern.

#### Root cause 3 — six hand-rolled string parsers

`generate/regex/emit/` re-parses regex patterns at 6 sites:
`negated_class.rs`, `generalized/mod.rs` (4 sites),
`mod.rs` (comma-or-whitespace). Each re-derives data already
in `RegexInfo.classification` / `RegexInfo.quantified_class`.

#### Root cause 4 — Span not admitted as scalar payload

`push_leaf_with_Span` / `payload_Span` don't exist.
Without Span as scalar, `Tuple([Span, scalar])` needs the
KvPair special case. Multi-Span tuples remain excluded.

#### Root cause 5 — Named struct projection incomplete

`TypeDesc::Named` is rejected by `is_scalar_payload()`,
`plan_layout()`, and all emitter paths. No `StructRegistry`
maps `StringId → Vec<TypeDesc>`. The direct-to-struct
chain is planned but not built.

## AS plan — 5 phases

### Phase 1 — CSS L4 parse activation (~2 days)

JSON + Google Sheets benches are working. CSS L4 is the remaining
gap. The modifier recovery fix addressed `factor` lowering;
CSS L4 likely has additional lowering gaps.

#### AS.1.1 Diagnose CSS L4 parse failure

Expand the CSS L4 bench parser. Find the first rule that fails.
Compare the generated dispatch table against the grammar's
expected branches. The failure offset (196 in bootstrap.css)
points to a specific CSS construct.

#### AS.1.2 Fix remaining lowering gaps

The modifier recovery fix in `lower_factor` handles `? * + ?w`.
CSS L4 may have:
- Additional expression shapes that lose children through
  `Repeat(vi=0)` optional wrappers
- Binary operator chains that fail `recover_binary_op` in
  deeper nesting contexts
- Selector combinator syntax that doesn't match the BBNF
  grammar's structural expectations

#### AS.1.3 Fix VM parse path

Verify JSON VM bench works post-modifier-fix. If CSS L4 VM
also fails, the fix from AS.1.2 should cover it.

#### AS.1.4 Validate all bench targets

**Hard gate**: all 12 bench targets compile and run without parse
failures. Post-AS.json has measured throughput for every dataset.

### Phase 2 — Span scalar admission + direct projection (~2 days)

#### AS.2.1 Wire Span through builder + tape

Add `push_leaf_with_Span(kind, span_lo, span_hi, variant_idx,
meta_idx, value_lo: u32, value_hi: u32)` to `TapeBuilder`.
Add `payload_Span(rec) -> Option<(u32, u32)>` to `Tape`.
Restore the 4 lines reverted in `c0ef0be` in `type_desc.rs`.

#### AS.2.2 Wire Span through view layer

Add Span case to `scalar_value_fallback` in `view/leaves.rs`.
Add Span case to `emit_scalar_value_decode` in `view/alt.rs`.
Update `emit_tape_span_only_scalar_epilogue` for Span.

#### AS.2.3 Named struct projection

Add `StructRegistry` to `GrammarIR` mapping
`StringId -> Vec<TypeDesc>`. Extend `compute_payload_layouts`
to resolve `Named` types. Add emitter and view routing.

**Hard gate**: `TypeDesc::Span` admitted as scalar with builder+tape
methods. CSS L4 `important = (Span, BoxedEnum, Span)` has Span
fields recognized. At least one `Named` struct projects concretely.

### Phase 3 — Scanner truth (~3 days)

#### AS.3.1 Wire Identifier sub-flags through plan_regex_scanner

Replace `RegexClass::Identifier { .. }` catch-all with explicit
dispatch on `allows_escapes`, `allows_leading_dash`,
`allows_double_dash_prefix`. Route to:
- `emit_call()` — generic identifier
- `emit_call_css()` — CSS with leading dash
- `emit_call_with_escapes()` — CSS with escape sequences (new)

#### AS.3.2 Implement `scan_ident_with_escapes` in parse-that

New scanner function extending `scan_ident` to consume `\\`
followed by any non-newline byte. Wire through
`kernels::identifier::emit_call_with_escapes`.

#### AS.3.3 Kill six hand-rolled string parsers

Replace each with `RegexInfo` field matches:

| Current site | Replacement |
|-------------|-------------|
| `negated_class.rs` `[^XYZ]+` parser | `ClassRangeInfo { negated: true, .. }` |
| `generalized/mod.rs` bracket/range parsing | `ClassRangeInfo` fields |
| `generalized/mod.rs` ws-padded literal | HIR concat inspection |
| `mod.rs` comma-or-whitespace match | New `RegexClass::Separator` |
| `mod.rs` SIMD positive-class extraction | `ClassRangeInfo.chars` |
| `mod.rs` shorthand class loop | `ClassRangeInfo` width |

#### AS.3.4 Replace 3 redundant classify_regex calls

- `leaves.rs:88,121` — use `opts.classify_regex(pattern)` (cached)
- `cost_model.rs:146-150` — resolve once, cache the bool

**Hard gate**: zero hand-rolled regex string parsers in
`generate/regex/emit/`. `plan_regex_scanner` dispatches on
all Identifier sub-flags. Zero raw `classify_regex(` in leaves.rs.

### Phase 4 — Profile and close sonic-rs gap (~2 days)

#### AS.4.1 Fresh samply profiles

`samply record` for JSON (5 datasets) + CSS L4 (3 datasets).
Compare to AR-baseline profiles in `docs/benchmarks/profiles/`.

#### AS.4.2 Post-AS.json with measured throughput

Delta vs post-AR numbers. The post-AR JSON numbers (1097-2353 MB/s)
are the first correct baseline. sonic-rs targets:
canada 1540, citm 3000, twitter 2643, data 2346 MB/s.

#### AS.4.3 Hot-path optimization pass

Profile-directed, not speculative. Likely bottlenecks:
- Dispatch control flow overhead
- Whitespace handling in `?w` modifier paths
- Tape push per-record cost (meta Vec write)
- SIMD scan integration gaps (verify NEON/SSE paths fire)

**Hard gate**: post-AS.json with all bench targets measured.
JSON twitter >= 2000 MB/s. CSS L4 bootstrap >= 400 MB/s.

### Phase 5 — Scanner consolidation (~2 days)

The 6 AR.6 items that didn't ship:

#### AS.5.1 Collapse RegexClass passthrough miners → `RegexClassMiner`
#### AS.5.2 Single source of truth via `ScanLut` registry
#### AS.5.3 Fold regex-pattern-parser-lites → `RegexClassEmitter::route`
#### AS.5.4 Parameterize `WhitespaceWithBlockComment`
#### AS.5.5 Move `FnDescriptor` specialization to post-`compute_regex_info`
#### AS.5.6 Symmetric `kernels::number::emit_call_generic`

**Hard gate**: five regex-pattern-parser-lites collapsed to one
dispatch table. `wc -l` shows >= 350 LOC net reduction.

## Hard gates summary

1. All 12 bench targets run without parse failures (Phase 1)
2. `TypeDesc::Span` admitted with builder+tape+view chain (Phase 2)
3. At least one Named struct projects to concrete layout (Phase 2)
4. `plan_regex_scanner` dispatches on Identifier sub-flags (Phase 3)
5. Zero hand-rolled regex string parsers in emit/ (Phase 3)
6. Post-AS.json with measured throughput for all targets (Phase 4)
7. JSON twitter >= 2000 MB/s (Phase 4)
8. Five regex miners collapsed to one dispatch (Phase 5)
9. `cargo test --workspace` no new failures (all phases)

## Items already landed (from AR + AR audit)

- Bootstrap loop closed — idempotent regen (gen1 == gen2)
- JSON monolithic bench restored: 1097-2353 MB/s
- Google Sheets bench restored: 122-129 MB/s
- 6/6 grammar_roundtrip green
- 335+ tests pass, 0 failures
- host.rs 607→423 LOC, zero span-text fallback functions
- 14 payload layouts (3 JSON + 11 CSS L4)
- SIMD fractional scan (NEON + SSE4.2) integrated + tested
- KvPair tape kind wired through emitter + view
- Meta_idx discriminator split wired through all codegen paths
- Egraph clones 11→5
- FAMILY_HELPER policy in bbnf-lang, not bbnf-regex

## What is NOT in scope

- **Named struct ABI finalization**: AS.2.3 scaffolds; full ABI is
  a separate tranche.
- **Global CSP solve**: per-component sufficient.
- **ParsedGrammar elimination**: bootstrap loop is closed; this is
  now viable but out of scope for AS.
- **64-byte input padding**: requires parse-that ownership change.

## Operational directives

- **6 parallel agents per wave**, isolated worktrees.
- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues**.
- **Commit frequently with `/commit`**.
- **Every claimed perf win has a samply diff**.
- **Clear ALL `.bbnf-cache` dirs before any bench or regen**.
- **`cargo expand` evidence for every codegen claim**.
- **Verify SIMD paths fire**: `cargo asm` or runtime feature
  detection logs for both NEON and SSE4.2 paths.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/lower/expression.rs` | 1 |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1, 2 |
| `crates/ir/src/passes/sets/dispatch/*.rs` | 1 |
| `crates/ir/src/vm/compiler/*.rs` | 1 |
| `crates/bbnf-tape/src/{builder,tape}.rs` | 2 |
| `crates/ir/src/types/type_desc.rs` | 2 |
| `crates/core/src/backend/rust/view/{alt,leaves,seq}.rs` | 2 |
| `crates/ir/src/passes/payload/layout.rs` | 2 |
| `crates/core/src/generate/regex/emit/scanner_plan.rs` | 3 |
| `crates/core/src/generate/regex/emit/*.rs` | 3 |
| `crates/core/src/backend/kernels/identifier.rs` | 3 |
| `crates/core/src/backend/rust/emitter/leaves.rs` | 3 |
| `parse-that/rust/parse_that/src/parsers/scan/ident.rs` | 3 |
| `crates/ir/src/passes/recognizers/*.rs` | 5 |
| `docs/benchmarks/post-AS.json` | 4 (NEW) |
