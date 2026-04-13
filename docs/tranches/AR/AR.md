# Tranche AR — Discriminator Split, Payload Activation, Clone Reduction, CSS Hardening

## Audit synthesis

Six concurrent audits performed by parallel agents in isolated worktrees,
followed by a cross-check critique (`critique.md`) that corrected
overstated claims, stale head-state references, and speculative
performance projections.

- `audit-prior.md` — five-tranche retrospective + chronic deferrals
- `audit-aq-code.md` — AQ code state verification
- `audit-scanners.md` — scanner + regex generalization map
- `audit-direct-struct.md` — direct-to-struct dormancy diagnosis
- `audit-self-hosting.md` — self-hosting closure paths
- `audit-sonic-gap.md` — sonic-rs technique inventory + bench gap
- `critique.md` — cross-check corrections and recalibrations

Bench baseline: `docs/benchmarks/post-AQ.json`. Samply profiles for JSON
(5 datasets) + sheets parse + compile pipeline at
`docs/benchmarks/profiles/AR-baseline/`.

## The diagnosis (corrected)

AQ landed every claim at the code level — PayloadKind deletion, full
TypeDesc integer suite, RegexClass deoverfit, scanner deoverfit,
aggregate payload layout planner, Alt typed enum view codegen, IR
inspect module, structural dispatch deletion — but **zero rules in any
of the six production grammars exercise the central AQ.6 path**. The
infrastructure is dormant. Root causes converge on activation gaps
at five specific sites.

### Root cause 1 — lowering loses type annotations

`lower_map_arrow` (`crates/core/src/lower/expression.rs:1229-1292`)
walks three return-type extraction paths that all assume the
value-expression sub-tree exposes a distinguishable `int_lit` /
`float_lit` / `bool_lit` `rule_kind` compound. Post tape-rewrite,
those leaf compounds are folded into `value_atom` and the checks
silently fail. Every `-> 0u8` / `-> true` / `-> f64` mapping loses
its type suffix; every Map projects to `Span`.

### Root cause 2 — `fuse_single_use` breaks Alt branch detection

`crates/core/src/backend/rust/emitter/mod.rs:436` walks Alt branches
looking for `IrNode::Ref(rid)` to scalar-typed rules. After
`fuse_single_use` inlines JSON's `null/bool/number/string`, the
branches are no longer Refs. The Alt typed enum view at
`crates/core/src/backend/rust/view/alt.rs:188` early-bails on the
first non-Ref branch (`_ => return TokenStream::new()`).

### Root cause 3 — Alt typed enum view rejects mixed branches

The AQ.6.C.3 design brief specifies mixed payload + cursor-wrapped
branches. The implementation rejects the mixed case instead.

### Root cause 4 — `variant_idx` overloading (self-hosting)

Non-Alt-bodied rules stamp `rule.id`; Alt-bodied rules stamp
`__branch_idx`. The same byte means two incompatible things,
breaking every consumer that walks by `rule_kind()`. Bootstrap regen
produces a 78,307-line `generated.rs` that fails six
`grammar_roundtrip` tests.

### Root cause 5 — CSS classifier rejects escape-augmented identifiers

CSS L4 `selectorIdent` classifies as `RegexClass::Unknown` because
`try_classify_identifier` does not handle `\\[^\n]` alternation
branches. The `__compoundSelector` rule stays on the byte-by-byte
path.

### Root cause 6 — Tape capacity over-allocates 64x

Generated `parse(...)` preallocates `input.len() * 4` records.
For twitter (632 KB input) that is 2.5M records x 16 B = 40 MB,
vs actual ~40K records (~640 KB).

### Corrected status of infrastructure

- `TypeDesc::Span` is **already admitted** as scalar in
  `is_scalar_payload()` (line 65 of `type_desc.rs`). The
  `payload_size_bytes`, `payload_align_bytes`, and `rust_ident`
  methods are wired. AR.1.2's type_desc.rs work is done.
- `PayloadLayout` planner exists and computes correctly, but returns
  empty maps for all production grammars (no all-scalar tuples
  populated because root cause 1 prevents type propagation).
- View `children()` returns a zero-alloc iterator. The "lazy AST"
  framing is overstated — accurate description is "borrowed tape
  views with optional payload acceleration."
- Structural dispatch has been fully deleted. No code path exists
  anywhere in `crates/`. It is not a near-term activation item.
- `css_monolithic` is gone from live bench configs.
- No `post-AR.json` or `profiles/post-AR/` exist. All "after AR"
  numbers in the original plan were targets, not measurements.

## Performance state vs sonic-rs (targets, not facts)

| Dataset | post-AQ | sonic-rs | Gap | AR target |
|---------|---------|----------|-----|-----------|
| canada  | 1810    | 1540     | **+17% LEAD** | 2100 |
| citm    | 2657    | 3000     | -11%          | 2950 |
| twitter | 2046    | 2643     | -22%          | 2650 |
| data    | 1929    | 2346     | -18%          | 2400 |
| data_xl | 1355    | 1460     | -7%           | 1700 |

CSS L4 (post-AQ): bootstrap 500, normalize 962, tailwind 535 MB/s.
AR targets: bootstrap 800+, normalize 1400+, tailwind 800+.

Every target requires a post-implementation samply diff before being
claimed as achieved.

## AR plan — 9 phases (resequenced per critique, deferred items reintegrated)

Each phase has a hard gate. No phase is "deferred" — items either
ship, are deleted with rationale, or move to the explicit
`deferred_work` block in post-AR.json.

### Phase 1 — Fix identity (discriminator split; ~2 days)

Sequenced first per critique §5: the discriminator split must land
before AQ.6 activation or host/view migration, otherwise bootstrap
regen drifts under a changing surface API.

#### AR.1.1 `meta_idx: u8` alongside `variant_idx: u8`

File: `crates/bbnf-tape/src/tape.rs::TapeRec`. Add `meta_idx: u8`
(branch index for Alt-bodied; 0 for Seq/leaf). The existing flags
byte has room without widening the record.

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

**Proof**: `cargo test --workspace` passes. `cargo expand -p
bbnf-bootstrap --lib | grep 'meta_idx'` returns matches.

#### AR.1.2 Gate inline plan on `preserve_identity`

File: `crates/core/src/backend/rust/analysis/inline.rs::analyze_parse_inline_plan`.
Re-add the AQ.6.A change with a tighter gate: only force `DirectCall`
when `options.structural && rule.meta.preserve_identity`.

**Hard gate**: bootstrap-regen path produces standalone `__import_path`,
`__import_items`, `__recover_directive`, etc. functions that are
actually called from `__import_directive`'s body.

#### AR.1.3 Bootstrap regen + freeze constants

After AR.1.1 + AR.1.2 land:
1. Run `./scripts/bootstrap-bbnf.sh`. New `generated.rs` should be
   smaller than the current 78,307-line attempt.
2. Verify `cargo test --workspace` — all six `grammar_roundtrip`
   tests pass without `#[ignore]`.

**Hard gate**: six `grammar_roundtrip` tests green. `generated.rs`
compiles clean.

### Phase 2 — Activate AQ.6 (payload projection; ~3 days)

Sequenced after Phase 1 so that the tape discriminator is settled
before wiring payload writes. Per critique §2, `TypeDesc::Span`
scalar admission is already live — Phase 2 focuses on the lowering
and view-layer gaps that prevent activation.

#### AR.2.1 Fix `lower_map_arrow` type-suffix detection

File: `crates/core/src/lower/expression.rs:1229-1292`. Replace the
`int_lit`/`float_lit`/`bool_lit` `rule_kind` checks with span-text
inspection on the `value_atom`. ~15 lines.

**Hard gate**: `cargo test -p bbnf payload_layouts_active` (new test)
shows every `-> 0u8` Map projects to `U8`, every `-> f64` projects
to `F64`, every `-> true` projects to `Bool` across CSS L4 and JSON.

#### AR.2.2 Peel inlined Map/Regex shapes in Alt branch loop

File: `crates/core/src/backend/rust/view/alt.rs:164-291`. After
`fuse_single_use` inlines branches, the Alt walk must peel through
Map/Regex wrappers to find the underlying payload shape, instead of
requiring bare `IrNode::Ref`.

**Hard gate**: `cargo expand -p bbnf-bootstrap --lib | grep
'scan_number_strict_f64'` returns at least one match (currently zero).

#### AR.2.3 Mixed payload + cursor branches in Alt typed enum view

File: `crates/core/src/backend/rust/view/alt.rs:164-291`. Replace the
single early-bail with a per-branch dispatch: payload-eligible branches
get typed reads; non-eligible branches get cursor-wrapped sub-views.
Per AQ.6.C.3.

**Hard gate**: JSON `value` generates `valueValue<'p>` enum with
typed scalar variants alongside cursor-wrapped compound variants.

#### AR.2.4 Hoist `BBNF_CSP_REPORT` above the constraint short-circuit

File: `crates/ir/src/passes/csp_strategy/mod.rs:591` vs `:634`. Move
the report eprintln above `decode_min_cost_per_variable`.

**Hard gate**: `BBNF_CSP_REPORT=1` emits one `solve_component` line
per production grammar.

#### AR.2.5 Validate scalar payload on every production grammar

Re-probe every grammar after Phase 2 fixes:

| Grammar    | Expected layouts |
|------------|-----------------|
| json       | 1 (number -> F64) |
| css_l4     | 7-12 (length, angle, time, frequency, resolution, flex, percentage, dimension) |
| bbnf       | 0-2 |
| sheets     | 1-2 |
| ebnf       | 0 |
| css_pretty | 0-3 |

**Hard gate**: `payload_layouts.len() >= 8` summed across all
production grammars. Verified via `cargo expand`, not assertion alone.

### Phase 3 — Clone elimination + compile-time reduction (~3 days)

Per critique §6: the plan underweights clone/share overhead. Profile
story points more toward control-flow, compounds, and transient
allocation than toward scanner micro-specialization. This phase
addresses the architectural waste the profiles actually reveal.

#### AR.3.1 ID-based reuse in egraph extract

File: `crates/egraph/src/extract.rs`. Replace the 5 `.clone()` calls
with ID-based sharing where the extracted tree is borrowed, not owned.

File: `crates/egraph/src/egraph.rs`. Replace the 6 `.clone()` calls
with reference-based access where possible; remaining clones must be
justified by ownership boundary.

**Hard gate**: `grep -c '\.clone()' crates/egraph/src/{egraph,extract}.rs`
shows reduction >= 6. `cargo test -p egraph` passes.

#### AR.3.2 Intern cost_model string lookups

File: `crates/core/src/generate/regex/cost_model.rs:177,189`. Replace
`ir.strings.iter().position(|s| s == pattern)` O(n) linear search
with `StringId`-keyed lookup (the interned ID is already available at
every call site).

**Hard gate**: `grep 'strings.iter().position'
crates/core/src/generate/regex/cost_model.rs` returns zero matches.

#### AR.3.3 Structure sharing in type projection

File: `crates/ir/src/passes/types/mod.rs`. Audit and reduce clone
churn in the CSP type propagation loop. `TypeDesc` is `Clone` but
many propagation sites clone defensively where an `&TypeDesc` or
`Cow<TypeDesc>` suffices.

**Hard gate**: `cargo test --workspace` passes. Compile-time samply
profile shows reduced allocator pressure in `project_types`.

### Phase 4 — CSS classifier hardening (unlocks AQ.7.2; ~2 days)

#### AR.4.1 Extend `try_classify_identifier` for CSS escape patterns

File: `parse-that/rust/regex/src/classify/structural.rs:419`. Detect
patterns where the inner alt has a non-letter branch matching
`\\[^\n]` (CSS escape sequence). Add `allows_escapes: bool` flag to
`Identifier` variant.

#### AR.4.2 Wire `kernels::identifier::emit_call_with_escapes`

File: `crates/core/src/backend/kernels/identifier.rs`. New emit
variant routing through `parse_that::scan_ident_with_escapes`.

File: `parse_that/rust/parse_that/src/parsers/scan/ident.rs`. Extend
`scan_ident` to consume `\\` followed by any non-newline byte when
`config.allow_escapes` is true.

#### AR.4.3 Validate CSS L4 `__compoundSelector` routes through kernel

**Hard gate**: `cargo expand -p bbnf` for CSS L4 contains
`scan_ident_with_escapes` calls. samply profile diff shows
`__compoundSelector` self-time reduction.

### Phase 5 — Tape capacity + sonic-rs gap close (~2 days)

#### AR.5.1 Tape capacity heuristic

File: `crates/core/src/backend/rust/emitter/grammar.rs:467`. Replace
`input.len().saturating_mul(4)` with `input.len() / 2 + 2`.

**Hard gate**: samply diff shows reduced allocator pressure on
twitter dataset. Throughput improvement measured, not projected.

#### AR.5.2 64-byte input padding

File: `parse-that/rust/parse_that/src/state.rs::ParserState::new`.
Allocate a zero-terminated padded buffer. Eliminates EOF bounds-check
in `scan_quoted_string_simd`.

**Hard gate**: samply diff shows reduced branch-miss in scan paths.

### Phase 6 — Scanner generalization (~3 days)

Reintegrated from the original 8-item AR.3 plan. Phase 1's
discriminator split is stable; clone reduction (Phase 3) has
reduced compile-time noise; these refactors are now the correct
next step for architectural hygiene.

#### AR.6.1 Collapse RegexClass passthrough miners

Files: `crates/ir/src/passes/recognizers/{quoted_string,identifier,comment_ws}.rs`
replaced by parameterized `regex_class.rs` with acceptance predicate.

#### AR.6.2 Single source of truth via `ScanLut` registry

Collapse `parse_that/parsers/scan/digits.rs`,
`kernels/charclass.rs`, `kernels/charset_shapes.rs`,
`generate/regex/patterns/shorthand.rs` into
`class_lut.rs` + `scan_class.rs`.

#### AR.6.3 Lift `EngineSet::FAMILY_HELPER` to bbnf-lang

Replace hardcoded family-variant arm with
`EmitOpts.kernel_coverage: EngineSet` mask.

#### AR.6.4 Fold regex-pattern-parser-lites into `RegexClassEmitter::route`

Unify `emit/{mod,negated_class,generalized/mod,generalized/class_segments}.rs`
into `emit/route.rs` consuming `RegexInfo`.

#### AR.6.5 Parameterize `WhitespaceWithBlockComment`

Add `WsCommentConfig { line_comment, block_comment_open,
block_comment_close }` to runtime.

#### AR.6.6 Move `FnDescriptor` specialization to post-`compute_regex_info`

`try_specialize_map_fn` becomes `crates/ir/src/passes/specialize_fns.rs`,
reads cached `RegexInfo` instead of re-classifying.

#### AR.6.7 Symmetric `kernels::number::emit_call_generic`

Expose relaxed-config kernel call for non-JSON numeric grammars.

#### AR.6.8 Re-export bbnf-regex HIR predicates

Replace local `is_nullable`, `is_broad_byte_class`,
`contains_lazy_quantifier` with re-exports from `bbnf_regex`.

**Hard gate (Phase 6)**:
- Five regex-pattern-parser-lites collapsed to one dispatch table
- `EngineSet::FAMILY_HELPER` is a policy mask, not hardcoded
- `cargo test --workspace` no new failures
- `wc -l` shows >= 350 LOC net reduction

### Phase 7 — Host.rs deletion / ParsedGrammar elimination

Phase 1 discriminator split is stable, bootstrap regen is green.
The tape identity is now trustworthy. Consumers can migrate off
the span-text fallback in `host.rs`.

#### AR.7.1 Delete span-text fallback in host.rs

File: `crates/core/src/grammar/host.rs` lines 244-423 (span-text
extraction) AND catch-all dispatcher (lines 212-236). Update
`absorb_item` to dispatch purely on typed view accessors.

**Hard gate**: `grep "absorb_.*_by_text\|process_grammar_item_structural"
crates/` returns zero matches. `wc -l crates/core/src/grammar/host.rs`
< 250 lines (currently 606).

#### AR.7.2 Eliminate `ParsedGrammar` — direct IR consumption

After host.rs is lean, the bbnf-bootstrap proc-macro can lower
directly to IR. `ParsedGrammar`'s 8 fields all have IR equivalents
except `imports` (needs `IrModule` wrapper). Removes ~600 LOC plus
the entire CST-walking layer.

**Hard gate**: `crates/core/src/grammar/host.rs` deleted;
`crates/core/src/types.rs` no longer carries `ParsedGrammar`;
the bbnf-derive macro path consumes IR directly.

### Phase 8 — SIMD fractional scan (x86 + aarch64)

Fused SIMD fraction parsing for the Eisel-Lemire fast path.
sonic-rs has this on x86 only — implementing for both x86 (SSE4.2 /
AVX2) and aarch64 (NEON) gives parity or edge on all platforms.

#### AR.8.1 NEON 17-digit fractional scan

File: `parse_that/rust/parse_that/src/parsers/scan/number.rs`.
NEON implementation of 17-digit fraction accumulation for
aarch64 targets.

#### AR.8.2 SSE4.2 / AVX2 fractional scan

File: `parse_that/rust/parse_that/src/parsers/scan/number.rs`.
x86_64 SIMD implementation with runtime feature detection.
`cfg(target_arch)` dispatch at the call site.

**Hard gate (Phase 8)**:
- `cargo test --workspace` on both x86_64 and aarch64
- canada dataset throughput improvement measured via samply diff
- Both architectures exercise the SIMD path (verified via
  `cargo asm` or `RUSTFLAGS=--emit=asm`)

### Phase 9 — Pair compound flattening

Phase 2 payload activation is stable. JSON `pair = string, colon >>
value` can now be recognized as a 2-field aggregate when `value`
is a leaf span/scalar.

#### AR.9.1 Recognize KV-pair shape

File: `crates/core/src/backend/rust/emitter/grammar.rs`. Recognize
`Tuple(Span, scalar)` for KV-pair shape in `compute_payload_layouts`.
Emit aggregate + kv-pair view accessor.

#### AR.9.2 Emit `TapeKind::KvPair` for pair compounds

File: `crates/bbnf-tape/src/kind.rs`. Add `KvPair` variant to
`TapeKind`. File: `crates/core/src/backend/rust/view/seq.rs`.
Emit accessor that reads key Span + value payload in one shot.

**Hard gate (Phase 9)**:
- JSON twitter throughput improvement measured via samply diff
- `cargo expand` shows `KvPair` in JSON parser output
- `cargo test --workspace` no new failures

### Validation — bench + documentation

Same protocol as AQ.9: full bench sweep, samply profiles per dataset,
post-AR.json with delta vs post-AQ. Hard gates:

- `cargo test --workspace` no new failures
- Bootstrap regen produces compilable `generated.rs`
- Six `grammar_roundtrip` tests pass without `#[ignore]`
- `payload_layouts.len() >= 8` summed across production grammars
- Every performance claim backed by samply diff
- Zero speculative "after AR" numbers — only measured results

## Hard gates summary (cross-phase)

The tranche is "done" only when:

1. Bootstrap regen succeeds without `#[ignore]` on grammar_roundtrip
   (Phase 1)
2. `cargo expand` shows `meta_idx` in emitted code (Phase 1)
3. `payload_layouts.len() > 0` for >= 3 production grammars (Phase 2)
4. `<RuleName>Value` typed enum generated for JSON `value` with
   mixed scalar + cursor variants (Phase 2)
5. `BBNF_CSP_REPORT=1` emits one line per component (Phase 2)
6. Clone count in egraph reduced by >= 6 (Phase 3)
7. Zero `strings.iter().position` calls in cost_model.rs (Phase 3)
8. CSS L4 `selectorIdent` classifies as `Identifier { allows_escapes:
   true }` (Phase 4)
9. Post-AR samply diffs accompany every throughput claim (Phase 5)
10. `cargo test --workspace` no new failures (all phases)

## What is NOT in scope

Items that remain outside AR:

- **Named struct projection ABI**: the full `Named`-to-concrete-struct
  lowering/codegen bridge is a separate tranche.
- **Structural pre-scan**: deleted in AQ.5, proven net-negative.
- **Global CSP solve**: per-component CSP is sufficient at current
  grammar scale.
- **Cost-model grid sweep**: manual calibration adequate.
- **LazyValue / RawStr** (sonic-rs technique): architectural mismatch
  with tape model.
- **In-place string unescape**: breaks borrow model.

## Operational directives

Same orchestration model as AQ:

- **6 parallel agents per wave**, isolated worktrees (`isolation:
  "worktree"`), cherry-pick onto master.
- **NO workarounds, NO hacks, NO `#[allow(...)]` to mask issues**.
- **Commit frequently with `/commit`**. Each agent commits at every
  natural milestone.
- **No file collisions across agents in the same wave**.
- **Every claimed perf win has a samply diff**. No speculative
  throughput numbers.
- **`cargo expand` evidence for every codegen activation claim**.
  Visual inspection, not just test-pass.

### Wave plan

#### Wave 1 (Phase 1 — discriminator split; ~2 days)

- Agent A: AR.1.1 `meta_idx` tape format (TapeRec, builder, cursor)
- Agent B: AR.1.1 emitter Alt + Seq stamping; schema accessor updates
- Agent C: AR.1.2 inline plan gate (`preserve_identity`)
- Agent D: AR.1.3 bootstrap regen + roundtrip freeze (after A+B merge)
- Agent E: Phase 2 prep — write `payload_layouts_active` test harness
- Agent F: validation + cherry-pick orchestration

#### Wave 2 (Phase 2 — payload activation; ~3 days)

- Agent A: AR.2.1 `lower_map_arrow` fix
- Agent B: AR.2.2 peel inlined Map/Regex shapes in Alt branch loop
- Agent C: AR.2.3 mixed payload + cursor branches in Alt enum view
- Agent D: AR.2.4 CSP report hoist
- Agent E: AR.2.5 validate payload layouts across all grammars
- Agent F: validation + samply diff vs AR-baseline

#### Wave 3 (Phase 3 + 4 — clone reduction + CSS; ~3 days)

- Agent A: AR.3.1 egraph clone reduction
- Agent B: AR.3.2 cost_model string intern
- Agent C: AR.3.3 type projection clone reduction
- Agent D: AR.4.1 + AR.4.2 CSS classifier escape extension + scanner
- Agent E: AR.4.3 validate CSS L4 routing
- Agent F: validation

#### Wave 4 (Phase 5 + final validation; ~2 days)

- Agent A: AR.5.1 tape capacity heuristic
- Agent B: AR.5.2 64-byte input padding
- Agent C: full bench sweep + samply profiles + post-AR.json
- Agent D: documentation + memory entry + cleanup
- Agent E+F: reserved for rework from prior waves

## Critical files

| File | Phase |
|------|-------|
| `crates/bbnf-tape/src/{builder,tape}.rs` | 1 (AR.1.1) |
| `crates/core/src/backend/rust/emitter/grammar.rs` | 1 (AR.1.1), 2, 5 |
| `crates/core/src/backend/rust/view/alt.rs` | 1 (AR.1.1), 2 (AR.2.2, AR.2.3) |
| `crates/core/src/grammar/schema/emit/rust/directives.rs` | 1 (AR.1.1) |
| `crates/core/src/backend/rust/analysis/inline.rs` | 1 (AR.1.2) |
| `crates/core/src/grammar/{host,generated}.rs` | 1 (AR.1.3) |
| `crates/core/src/lower/expression.rs` | 2 (AR.2.1) |
| `crates/ir/src/passes/csp_strategy/mod.rs` | 2 (AR.2.4) |
| `crates/egraph/src/{egraph,extract}.rs` | 3 (AR.3.1) |
| `crates/core/src/generate/regex/cost_model.rs` | 3 (AR.3.2) |
| `crates/ir/src/passes/types/mod.rs` | 3 (AR.3.3) |
| `parse-that/rust/regex/src/classify/structural.rs` | 4 (AR.4.1) |
| `crates/core/src/backend/kernels/identifier.rs` | 4 (AR.4.2) |
| `parse-that/rust/parse_that/src/state.rs` | 5 (AR.5.2) |
| `docs/benchmarks/post-AR.json` | validation (NEW) |
