# Tranche AR — Activate AQ.6, Close Self-Hosting, Generalize Scanners, Close sonic-rs Gap

## Audit synthesis

Six concurrent audits performed by parallel agents in isolated worktrees.
Each is a standalone document with grounded findings; AR builds on them
without restating their evidence.

- `docs/tranches/AR-audit-prior.md` — five-tranche retrospective + chronic deferrals
- `docs/tranches/AR-audit-aq-code.md` — AQ code state verification
- `docs/tranches/AR-audit-scanners.md` — scanner + regex generalization map
- `docs/tranches/AR-audit-direct-struct.md` — direct-to-struct dormancy diagnosis
- `docs/tranches/AR-audit-self-hosting.md` — self-hosting closure paths
- `docs/tranches/AR-audit-sonic-gap.md` — sonic-rs technique inventory + bench gap

Bench baseline: `docs/benchmarks/post-AQ.json`. Samply profiles for JSON
(5 datasets) + sheets parse + compile pipeline at
`docs/benchmarks/profiles/AR-baseline/`.

## The diagnosis

AQ landed every claim at the code level — PayloadKind deletion, full
TypeDesc integer suite, RegexClass deoverfit, scanner deoverfit,
aggregate payload layout planner, Alt typed enum view codegen, IR
inspect module, structural dispatch deletion — but **zero rules in any
of the six production grammars (json, css_l4, bbnf, sheets, ebnf,
css_pretty: 318 rules total, 162 Tuple types) exercise the central
AQ.6 path**. The infrastructure is dormant. Two independent root
causes converge on the same activation gap.

### Root cause 1 — lowering loses type annotations

`lower_map_arrow` (`crates/core/src/lower/expression.rs:1229-1292`)
walks three return-type extraction paths that all assume the
value-expression sub-tree exposes a distinguishable `int_lit` /
`float_lit` / `bool_lit` `rule_kind` compound. Post tape-rewrite,
those leaf compounds are folded into `value_atom` and the checks
silently fail. Every `-> 0u8` / `-> true` / `-> f64` mapping loses
its type suffix; every Map projects to `Span`; every Tuple containing
such a Map projects to `Tuple(Span, ...)`; the layout planner accepts
none. A 15-line span-text inspection on `value_atom` activates every
dormant scalar path.

### Root cause 2 — `TypeDesc::Span` rejected as scalar

Span is a fixed `(u32, u32)` pair — exactly 8 bytes, naturally
aligned — but `is_scalar_payload` rejects it. CSS L4 `important =
(Span, BoxedEnum, Span)`, JSON's `string` branch of `value`, and
many other nontrivially-typed rules all fail on this. Admitting
Span as a scalar variant of payload immediately makes
~12 CSS L4 dimension rules and JSON's string path payload-eligible
without expanding the design surface.

### Root cause 3 — `fuse_single_use` breaks Alt branch detection

`crates/core/src/backend/rust/emitter/mod.rs:436` walks Alt branches
looking for `IrNode::Ref(rid)` to scalar-typed rules. After
`fuse_single_use` (always-on, gated only by SCC membership) inlines
JSON's `null/bool/number/string`, the branches are no longer Refs.
JSON's `__value` emits `scan_number_strict_span` instead of
`scan_number_strict_f64` — the canonical AQ.6.A example doesn't
fire on the canonical example grammar. Recursive peel of Map/Regex
shapes in the Alt branch loop fixes this.

### Root cause 4 — Alt typed enum view early-bails on mixed branches

`crates/core/src/backend/rust/view/alt.rs:188` early-bails the moment
one branch fails the payload-eligibility predicate. Per the design
brief AQ.6.C.3 ("Where branches aren't all payload-eligible, mix
payload branches with cursor-wrapped branches"), the implementation
should mix typed reads with cursor-wrapped sub-views — but it
rejects the mixed case instead.

### Root cause 5 — `variant_idx` overloading

Separate failure axis governing self-hosting. Non-Alt-bodied rules
stamp `rule.id`; Alt-bodied rules under Tranche AK.1 stamp
`__branch_idx`. The same byte means two incompatible things,
breaking every consumer that walks by `rule_kind()`. Bootstrap regen
produces a 78,307-line `generated.rs` that fails six
`grammar_roundtrip` tests. Fix: add `meta_idx: u8` alongside
`variant_idx`; Alt-bodied codegen stamps both; consumers read the
half they need.

### Root cause 6 — CSS classifier rejects escape-augmented identifiers

CSS L4 `selectorIdent = (?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*`
classifies as `RegexClass::Unknown` because `try_classify_identifier`
does not handle the inner alternation against `\\[^\n]`. The
`__compoundSelector` rule (40.2% normalize / 36.6% tailwind
self-time) therefore stays on the hand-rolled byte-by-byte path —
AQ.7.2 cannot ship until the classifier accepts CSS escape
branches.

### Root cause 7 — Tape capacity over-allocates 64×

Generated `parse(...)` preallocates `input.len() * 4` records. For
twitter (632 KB input) that's 2.5M records × 16 B = 40 MB, vs.
actual ~40K records (~640 KB). The over-allocation churns mimalloc
on every parse. sonic-rs uses `input.len() / 2 + 2`. A one-line
constant change recovers ~150-300 MB/s per dataset.

## Performance state vs sonic-rs

| Dataset | post-AQ | sonic-rs | Gap | After AR (projected) |
|---------|---------|----------|-----|---------------------|
| canada  | 1810    | 1540     | **+17% LEAD** | 2100 (+36% lead) |
| citm    | 2657    | 3000     | -11%          | 2950 (parity)    |
| twitter | 2046    | 2643     | -22%          | 2650 (parity)    |
| data    | 1929    | 2346     | -18%          | 2400 (parity)    |
| data_xl | 1355    | 1460     | -7%           | 1700 (+16% lead) |

CSS L4 (post-AQ): bootstrap 500, normalize 962, tailwind 535 MB/s.
After AR Phase 4: bootstrap 800+, normalize 1400+, tailwind 800+.

## AR plan — 7 phases

Each phase has a hard gate. No phase is "deferred" — items either
ship, are deleted with rationale, or move to the explicit
`deferred_work` block in post-AR.json.

### Phase 1 — Activate AQ.6 (highest-leverage; ~3 days)

#### AR.1.1 Fix `lower_map_arrow` type-suffix detection
File: `crates/core/src/lower/expression.rs:1229-1292`. Replace the
`int_lit`/`float_lit`/`bool_lit` `rule_kind` checks with span-text
inspection on the `value_atom` (per Agent D's proposal). ~15 lines.

**Hard gate**: probe via `cargo test -p bbnf payload_layouts_active`
(new test) shows every `-> 0u8` Map projects to `U8`, every `-> f64`
projects to `F64`, every `-> true` projects to `Bool` across CSS L4,
JSON, and at least one BBNF grammar.

#### AR.1.2 Admit `TypeDesc::Span` as scalar payload
File: `crates/ir/src/types/type_desc.rs`. Extend
`is_scalar_payload` (true for Span), `payload_size_bytes` (8),
`payload_align_bytes` (4), `rust_ident` (returns `"Span"` — view
returns the borrowed `Span<'p>` struct).

File: `crates/bbnf-tape/src/{builder,tape}.rs`. Add
`push_leaf_with_span(kind, span_lo, span_hi, variant_idx)` and
`payload_span(rec) -> Option<(u32, u32)>` reader. The Span fits in
the existing payload slot bytes (no aggregate needed for single-Span
case).

File: `crates/core/src/backend/rust/{emitter/grammar.rs,view/leaves.rs}`.
Wire scalar-Span path through prelude/epilogue + view accessor.

**Hard gate**: probe shows CSS L4 `important = (Span, BoxedEnum,
Span)` becomes payload-eligible at the field-level (still rejected
overall due to BoxedEnum, but fields enumerate as scalar);
`payload_layouts.len() > 0` for every grammar with at least one
all-scalar Tuple after this admission.

#### AR.1.3 Peel inlined Map/Regex shapes in Alt branch loop
File: `crates/core/src/backend/rust/emitter/mod.rs:436-451`. After
`fuse_single_use` inlines JSON's `null/bool/number/string`, the Alt
branches are no longer Refs but inlined Map/Regex/Literal nodes.
Walk Map's `inner` field for the immediate `IrNode::Map { return_type:
Some(td), .. }` shape and infer `ctx.payload_type` from it.

**Hard gate**: `cargo expand -p bbnf-bootstrap --lib | grep
'scan_number_strict_f64'` returns at least one match (currently zero).
JSON `__value`'s `number` branch writes `__payload_f64`; view's
`.value() -> f64` calls `tape.payload_f64(rec)` directly.

#### AR.1.4 Mixed payload + cursor branches in Alt typed enum view
File: `crates/core/src/backend/rust/view/alt.rs:164-291`. Replace the
single early-bail with a per-branch dispatch: payload-eligible →
typed read; non-eligible → cursor-wrapped sub-view. Per AQ.6.C.3.

**Hard gate**: every Alt-bodied rule with at least one
payload-eligible branch generates a `<RuleName>Value` enum. Probe:
JSON `value` generates `valueValue<'p>` enum with variants
`String(&'p str)`, `Number(f64)`, `Bool(bool)`, `Null`, `Array(...)`,
`Object(...)`.

#### AR.1.5 Hoist `BBNF_CSP_REPORT` above the constraint short-circuit
File: `crates/ir/src/passes/csp_strategy/mod.rs:591` vs `:634`. Move
the report eprintln above `decode_min_cost_per_variable` so trivial
components also report.

**Hard gate**: `BBNF_CSP_REPORT=1 cargo run --example
probe_payload_layouts` emits exactly one `solve_component` line per
production grammar (currently 4 of 6).

### Phase 2 — Close self-hosting (Path 6 + Path 1; ~1 week)

Per AR-audit-self-hosting.md.

#### AR.2.1 Path 6 — `meta_idx: u8` alongside `variant_idx: u8`
File: `crates/bbnf-tape/src/tape.rs::TapeRec`. Add `meta_idx: u8`
(branch index for Alt-bodied; 0 for Seq/leaf). The existing flags
byte has room without widening the record (`TapeRec` is currently
12 bytes + alignment padding to 16; meta_idx fits in unused padding).

File: `crates/core/src/backend/rust/emitter/grammar.rs`. Alt-bodied
prelude/epilogue stamps `(rule.id, branch_idx)`; non-Alt stamps
`(rule.id, 0)`.

File: `crates/core/src/grammar/schema/emit/rust/directives.rs`. The
`try_as_*_directive` accessors check `cursor.variant_idx() ==
rule_id_for(directive)` AND `cursor.meta_idx() == branch_index`.

File: `crates/core/src/backend/rust/view/alt.rs`. The `as_<variant>`
methods read `meta_idx` instead of `variant_idx`.

**Hard gate**: bootstrap regen produces `generated.rs` that compiles
clean and passes all six `grammar_roundtrip` tests.

#### AR.2.2 Path 1 — gate inline plan on `preserve_identity`
File: `crates/core/src/backend/rust/analysis/inline.rs::analyze_parse_inline_plan`.
Re-add the AQ.6.A change with a tighter gate: only force `DirectCall`
when `options.structural && rule.meta.preserve_identity`. The AQ.6.A
regression on CSS L4 was likely caused by `preserve_identity` leaking
outside structural mode.

**Hard gate**: bootstrap-regen path produces standalone `__import_path`,
`__import_items`, `__recover_directive`, etc. functions that are
actually called from `__import_directive`'s body (currently dead-coded).

#### AR.2.3 Bootstrap regen + freeze constants + delete fallback
After AR.2.1 + AR.2.2 land:
1. Run `./scripts/bootstrap-bbnf.sh`. New `generated.rs` should be
   smaller than the current 78,307-line attempt (the `meta_idx`
   approach removes the need for separate sub-rule functions).
2. Verify `cargo test --workspace` — all six `grammar_roundtrip`
   tests pass without `#[ignore]`.
3. Delete the span-text fallback in `crates/core/src/grammar/host.rs`
   (lines 244-423) AND the catch-all dispatcher (lines 212-236).
   Update `absorb_item` to dispatch purely on typed view accessors.

**Hard gate**: `grep "absorb_.*_by_text\|process_grammar_item_structural"
crates/` returns zero matches. `wc -l crates/core/src/grammar/host.rs`
< 250 lines (currently 606).

#### AR.2.4 (stretch) Path 4 — eliminate `host.rs`/`ParsedGrammar`
After AR.2.3, the bbnf-bootstrap proc-macro can lower directly to
IR. ParsedGrammar's 8 fields all have IR equivalents except
`imports` (needs new `IrModule` wrapper). Removes ~600 LOC plus the
entire CST-walking layer.

**Soft gate** (stretch goal; skip if Phase 3-4 are blocked):
`crates/core/src/grammar/host.rs` deleted; `crates/core/src/types.rs`
no longer carries `ParsedGrammar`; `crates/core/src/lower/tape_walk.rs`
deleted; the bbnf-derive macro path consumes IR directly.

### Phase 3 — Scanner + regex generalization (per AR-audit-scanners.md; ~1 week)

8 refactors, sequenced to minimize merge churn:

#### AR.3.1 Collapse 4 RegexClass passthrough miners → `RegexClassMiner`
Files: `crates/ir/src/passes/recognizers/{quoted_string,identifier,comment_ws}.rs`
→ replaced by `crates/ir/src/passes/recognizers/regex_class.rs` with a
parameterized acceptance predicate.
Net: -110 LOC.

#### AR.3.2 Single source of truth for canonical byte sets via `ScanLut` registry
Files: `parse_that/parsers/scan/digits.rs`, `kernels/charclass.rs`,
`kernels/charset_shapes.rs`, `generate/regex/patterns/shorthand.rs`
→ collapsed to `parse_that/parsers/scan/class_lut.rs` +
`kernels/scan_class.rs`. The DIGIT/ALNUM/HEX triplet becomes a
registry; `charset_shapes` deleted.
Net: ~50 LOC reduction; eliminates three independent encodings.

#### AR.3.3 Lift `EngineSet::FAMILY_HELPER` to bbnf-lang via `KernelCoverage`
File: `bbnf_regex::info::derive_feasible_engines`. The hardcoded
family-variant `matches!` arm is replaced by a
`EmitOpts.kernel_coverage: EngineSet` mask passed in by bbnf-lang.
Removes the single piece of cross-tier policy leak.
Net: net-zero LOC; corrects an architectural smell.

#### AR.3.4 Fold 5 regex-pattern-parser-lites → `RegexClassEmitter::route`
Files: `emit/{mod,negated_class,generalized/mod,generalized/class_segments}.rs`
→ unified into `emit/route.rs` consuming `RegexInfo` + dispatching
to `kernels::*`.
Net: -400 LOC. The `emit/mod.rs::emit_regex_fast_path` ad-hoc
`",|\s+"` branch becomes a routed `RegexClass::Separator` variant.

#### AR.3.5 Parameterize `WhitespaceWithBlockComment` → `WhitespaceWithComments { line, block }`
Files: `bbnf_regex::RegexClass`, `parse_that::scan_ws_block_comments`.
Add `WsCommentConfig { line_comment, block_comment_open,
block_comment_close }` to runtime; promote variant to carry
`Option<(Vec<u8>, Vec<u8>)>` for each.

#### AR.3.6 Move `FnDescriptor` specialization to post-`compute_regex_info` pass
File: `crates/core/src/lower/expression.rs::try_specialize_map_fn` →
new `crates/ir/src/passes/specialize_fns.rs`. Reads
`ir.regex_info[sid].classification` from cache instead of
re-classifying. Eliminates double HIR parse per pattern.

#### AR.3.7 Symmetric `kernels::number::emit_call_generic`
File: `crates/core/src/backend/kernels/number.rs`. Currently only
strict variants are exposed; add the relaxed-config kernel call so
non-JSON numeric grammars route through it.

#### AR.3.8 Re-export bbnf-regex HIR predicates
File: `crates/core/src/generate/regex/emit/hir/mod.rs`. Replace the
local `is_nullable`, `is_broad_byte_class`,
`contains_lazy_quantifier` helpers with re-exports from
`bbnf_regex`. Net: -50 LOC.

**Hard gate (Phase 3)**:
- `grep -rn "JsonString\|JsonNumber\|CssIdent\|CssQuotedString\|WsBlockComment" parse-that/rust/regex/`: zero matches
- Five regex-pattern-parser-lites collapsed to one dispatch table
- `cargo test --workspace` no new failures
- `wc -l` shows ~350 LOC reduction

### Phase 4 — CSS classifier extension (unlocks AQ.7.2; ~2 days)

#### AR.4.1 Extend `try_classify_identifier` for CSS escape patterns
File: `parse-that/rust/regex/src/classify/structural.rs:419`. Detect
patterns where the inner alt has a non-letter branch matching
`\\[^\n]` (CSS escape sequence). Add `allows_escapes: bool` flag to
`Identifier` variant.

#### AR.4.2 Wire `kernels::identifier::emit_call_with_escapes`
File: `crates/core/src/backend/kernels/identifier.rs`. New emit
variant routing through `parse_that::scan_ident_with_escapes` (new
scanner that handles `\\` escape continuation).

File: `parse_that/rust/parse_that/src/parsers/scan/ident.rs`. Extend
`scan_ident` to consume `\\` followed by any non-newline byte when
`config.allow_escapes` is true.

#### AR.4.3 Validate CSS L4 `__compoundSelector` routes through kernel
**Hard gate**: `cargo expand -p bbnf` for CSS L4 contains
`scan_ident_with_escapes` calls; CSS L4 `bootstrap` bench reaches
800+ MB/s, `normalize` reaches 1400+ MB/s.

### Phase 5 — sonic-rs gap close (per AR-audit-sonic-gap.md; ~3 days)

#### AR.5.1 Tape capacity heuristic
File: `crates/core/src/backend/rust/emitter/grammar.rs:467`. Replace
`input.len().saturating_mul(4)` with `input.len() / 2 + 2`. Trims
40 MB over-allocation per twitter parse.

**Hard gate**: twitter +200 MB/s, data +150, citm +80, data_xl +200.

#### AR.5.2 Pair compound flattening
JSON `pair = string, colon >> value` today emits a compound with 2
children. When `value` is a leaf span/scalar, emit a single
2-field-aggregate leaf with `TapeKind::KvPair` instead.

File: `crates/core/src/backend/rust/emitter/grammar.rs`. New
emission shape gated by AQ.6.B's `compute_payload_layouts` —
recognize `Tuple(Span, scalar)` for KV-pair shape and emit aggregate
+ kv-pair view accessor.

**Hard gate**: twitter +300 MB/s, data +200, citm +120.

#### AR.5.3 64-byte input padding
File: `parse-that/rust/parse_that/src/state.rs::ParserState::new`.
Allocate a zero-terminated padded buffer. Eliminates EOF bounds-check
in `scan_quoted_string_simd`.

**Hard gate**: twitter +100 MB/s, citm +50.

#### AR.5.4 NEON 17-digit fractional scan
File: `parse-that/rust/parse_that/src/parsers/scan/number.rs`. Port
SIMD fraction parsing for the Eisel-Lemire fast path. sonic-rs has
this on x86 only — porting to NEON gives us an edge on Apple Silicon
+ aarch64 Linux.

**Hard gate**: canada +150 MB/s.

### Phase 6 — Activate scalar payload on EVERY production grammar

Per AR-audit-aq-code.md's recommended follow-up. After Phase 1
fixes the activation gap, re-probe every grammar:

| Grammar | Expected layouts |
|---------|-----------------|
| json    | 1 (number → F64)            |
| css_l4  | 7-12 (length, angle, time, frequency, resolution, flex, percentage, dimension) |
| bbnf    | 0-2                          |
| sheets  | 1-2                          |
| ebnf    | 0                            |
| css_pretty | 0-3                       |

**Hard gate**: `payload_layouts.len() ≥ 8` summed across all
production grammars.

### Phase 7 — Validation + bench + documentation

Same protocol as AQ.9: full bench sweep, samply profiles per dataset,
post-AR.json with delta vs post-AQ. Hard gates:
- `cargo test --workspace` no new failures vs 28-fail baseline
- Bootstrap regen produces compileable `generated.rs`
- `payload_layouts.len() ≥ 8` summed across production grammars
- JSON twitter ≥ 2,500 MB/s (parity with sonic-rs)
- JSON canada ≥ 2,000 MB/s (extends lead)
- CSS L4 bootstrap ≥ 800 MB/s
- Bootstrap `host.rs` < 250 LOC (was 606)
- Zero `#[allow(...)]` outside of `generated.rs` increases
- Every `BBNF_CSP_REPORT=1` line accounts for a unique component

## Hard gates summary (cross-phase)

The tranche is "done" only when:

1. `payload_layouts.len() > 0` for ≥ 3 production grammars (Phase 1)
2. `<RuleName>Value` typed enum generated for JSON `value`, CSS L4
   `colorFn`, and at least one BBNF Alt rule (Phase 1)
3. Bootstrap regen succeeds without `#[ignore]` on grammar_roundtrip
   (Phase 2)
4. `crates/core/src/grammar/host.rs` is ≤ 250 LOC, no `_by_text`
   functions (Phase 2)
5. Five regex-pattern-parser-lites collapsed to one dispatch table
   (Phase 3)
6. `EngineSet::FAMILY_HELPER` is a bbnf-lang policy mask, not a
   bbnf-regex hard-coded list (Phase 3)
7. CSS L4 `selectorIdent` classifies as `Identifier { allows_escapes:
   true, ... }` (Phase 4)
8. `bootstrap` CSS bench ≥ 800 MB/s (Phase 4)
9. JSON `twitter` bench ≥ 2,500 MB/s (Phase 5)
10. `BBNF_CSP_REPORT=1` emits one line per component, no
    short-circuit skip (Phase 1.5)

## Operational directives

Same orchestration model as AQ:

- **6 parallel agents per wave**, isolated worktrees (`isolation:
  "worktree"`), cherry-pick onto master.
- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues**.
  Per Agent A's retro: AQ.5 + AQ.6 succeeded by deleting infrastructure
  rather than papering over it. AR continues that discipline.
- **Commit frequently with `/commit`**. Each agent commits at every
  natural milestone, not at task end.
- **Check-ins every 30 minutes** via brief `/loop` status if working
  in autonomous mode; agents post one-paragraph progress before
  starting any non-trivial new file.
- **No file collisions across agents in the same wave**. Use the
  file-bounds discipline from AQ.md's orchestration directives —
  exclusive write per file per wave; cross-wave conflicts are
  resolved by sequencing.
- **Caches**: clear `target/.bbnf-cache` before any bench, touch
  `crates/derive/src/lib.rs` to force proc-macro re-expansion, clean
  `bbnf-analysis` if rustc ICEs (recurring issue with incremental
  cache).

### Wave plan

#### Wave 1 (Phase 1 — activate AQ.6; ~3 days)
- Agent A: AR.1.1 lower_map_arrow fix
- Agent B: AR.1.2 TypeDesc::Span as scalar
- Agent C: AR.1.3 Alt branch peel inlined shapes
- Agent D: AR.1.4 mixed payload+cursor enum view
- Agent E: AR.1.5 BBNF_CSP_REPORT hoist
- Agent F: validation + post-Wave-1 probe report; samply diff vs AR-baseline

#### Wave 2 (Phase 2 — self-hosting; ~1 week)
- Agent A: AR.2.1 meta_idx tape format (TapeRec, builder, cursor)
- Agent B: AR.2.1 emitter Alt + Seq stamping; schema accessor updates
- Agent C: AR.2.2 inline plan gate
- Agent D: AR.2.3 bootstrap regen + roundtrip freeze + host.rs delete
- Agent E: AR.2.4 (stretch) ParsedGrammar → IrModule
- Agent F: validation + cherry-pick orchestration

#### Wave 3 (Phase 3 — scanner generalization; ~1 week)
- Agent A: AR.3.1 RegexClassMiner
- Agent B: AR.3.2 ScanLut registry + AR.3.8 HIR predicate re-export
- Agent C: AR.3.3 KernelCoverage mask
- Agent D: AR.3.4 RegexClassEmitter::route (largest delta)
- Agent E: AR.3.5 + AR.3.6 + AR.3.7 (configurable comments + post-pass + symmetric kernels)
- Agent F: validation

#### Wave 4 (Phase 4 + 5 + 6 + 7; ~3-5 days)
- Agent A: AR.4.1+4.2 CSS classifier escape extension + scanner
- Agent B: AR.5.1+5.3 capacity heuristic + 64-byte padding
- Agent C: AR.5.2 pair compound flattening
- Agent D: AR.5.4 NEON 17-digit fraction
- Agent E: full bench sweep + samply profiles + post-AR.json
- Agent F: documentation + memory entry + cleanup

## What's NOT in scope

Items deliberately deferred to a future tranche:

- **Global CSP solve** (chronic deferral since AL): per-component
  CSP is sufficient at current grammar scale. Re-evaluate when
  grammars grow past 500 rules.
- **Cost-model grid sweep** (chronic deferral since AM): manual
  cost calibration after each tranche has been adequate. Build a
  proper sweep harness only when there's a measurable drift.
- **LazyValue / RawStr** (sonic-rs technique): architectural
  mismatch with our tape model. Not a gap; a different shape.
- **In-place string unescape**: breaks borrow model. Not portable
  to our tape architecture.
- **Structural pre-scan** (deleted in AQ.5): proven net-negative
  at current WS budget. Revisit only if WS overhead returns.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/lower/expression.rs` | 1 (AR.1.1), 3 (AR.3.6) |
| `crates/ir/src/types/type_desc.rs` | 1 (AR.1.2) |
| `crates/bbnf-tape/src/{builder,tape}.rs` | 1 (AR.1.2), 2 (AR.2.1) |
| `crates/core/src/backend/rust/emitter/{grammar,mod}.rs` | 1, 2, 5 |
| `crates/core/src/backend/rust/view/{alt,leaves}.rs` | 1, 2 |
| `crates/ir/src/passes/csp_strategy/mod.rs` | 1 (AR.1.5) |
| `crates/core/src/backend/rust/analysis/inline.rs` | 2 (AR.2.2) |
| `crates/core/src/grammar/{host,generated}.rs` | 2 (AR.2.3, AR.2.4) |
| `parse-that/rust/regex/src/classify/{mod,structural}.rs` | 4 (AR.4.1) |
| `crates/core/src/backend/kernels/identifier.rs` | 4 (AR.4.2) |
| `parse-that/rust/parse_that/src/parsers/scan/{number,ident}.rs` | 4, 5 (AR.5.4) |
| `parse-that/rust/parse_that/src/state.rs` | 5 (AR.5.3) |
| `docs/benchmarks/post-AR.json` | 7 (NEW) |

## Summary

AR is the activation tranche. AQ built the infrastructure; AR makes
it real. Six audit reports converge on the same diagnosis: every
piece of AQ.6 is wired correctly but a small set of upstream bugs
prevents activation. Fix those (Phase 1), close the chronic
self-hosting deferral (Phase 2), generalize the regex/scanner
substrate (Phase 3), unlock CSS L4 (Phase 4), close the
sonic-rs gap (Phase 5), validate (Phases 6-7).
