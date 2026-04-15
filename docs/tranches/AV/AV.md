# Tranche AV — The Flattening

AV replaces the recursive-descent-per-rule codegen with a grammar-
derived dispatch automaton that mines the tape skeleton in one linear
byte pass, fills typed payloads in parallel, and collapses repeated
structural shapes into dictionary references. The tape stays flat;
parsing becomes a pipeline of grammar-specialised stages.

Everything AU builds toward this: the typed-materialisation invariant,
the unified arena, the per-grammar capacity closures, the padded input
buffer, the SIMD structural bitmap. AV is where those landings stop
being per-phase optimisations and start being a single coherent
architecture.

## Architectural thesis

The grammar's static fingerprint is a first-class **codegen output
channel**, not merely an optimiser input. Every per-grammar bit of
knowledge — push counts, payload classifications, fixed-shape e-classes,
structural alphabet, document-list rules, branch priors, keyword sets —
reaches the emitted binary as a specialised constant, layout, or
kernel. This is what an owned-stack grammar/compiler/tape pipeline can
do that a library (simdjson, sonic-rs, serde_json, lightningcss) can
never do.

## Invariants (inherited from AU)

- Tape-first materialisation with full typed AST (sonic-rs + lightningcss parity).
- Every `->` annotation in the grammar reaches the emitter; inference composes types, never loses them.
- No fallbacks, no legacy paths, no workarounds. One path per role.
- One access API across JSON, CSS, Sheets, BBNF.

## Precedent and archaeology

AV reaches for three architectural moves the codebase has touched
before and put down:

- **Flattening recursive-descent into a dispatch automaton.** Never
  attempted in production. AA-prototype-3 sketched an alternative
  substrate; AM/AN/AO/AP pursued incremental scanner and dispatch
  refinement but kept the fn-per-rule recursion.
- **SIMD structural bitmap pre-scan.** Landed across commits `4114695`
  (AO.0.1), `7198c97` (AO.0.4-0.6), `2fa3172` / `4417f8a` (AP.1b);
  disabled at `2a8af08` (AP.1); deleted at `2f7c1bd` (AQ.5) due to a
  -190 µs citm regression whose root cause was four integration bugs
  (scalar `filter_quote_parity`, hybrid dispatch, unsaved cursor on
  checkpoint, disabled WS elision). AU.2.7 ships v2 as a scanner-only
  kernel; AV extends v2 into the stage-1 primitive of the dispatch
  automaton.
- **Subtree deduplication.** AE/tape-shapes.md catalogued shape
  taxonomy but never dedup. E-graph `is_fixed_shape` + `RecognizerSignature.shape_hash`
  were built (AM/AN era), used for inline/fuse optimisations, but
  never reached the tape. AV is the first tape-level dedup.

The peer review folded into `docs/tranches/AU/PROGRESS.md` Session 2
makes the design constraint explicit: **scanner-only proposals land
first and are safe; tape-layout proposals prototype per-rule before
generalising; control-flow proposals require the complete feature
before shipping or become the next AQ.5.**

## Wave schedule

AV is more serial at the top than AU — the DTA substrate has to land
before anything built on it can parallelise. The early waves
concentrate on the automaton; later waves fan out over grammars and
layers. See `docs/instructions/README.md` for the sub-agent
isolation and file-bounds contract every wave honours.

| Wave | Parallel sub-agents | Blocks |
|------|---------------------|--------|
| **V1 — GrammarProfile plumbing (serial)** | Single agent: `AV.1` emits the `GrammarProfile` struct into `generated.rs` for every grammar. Every downstream wave reads this struct. | V2 |
| **V2 — DTA synthesis (serial)** | Single agent: `AV.2.1` lifts `GrammarIR` to a counter-DFA; `AV.2.2` adds counter states for optional-with-lookahead (BBNF `__mapped_factor`, CSS optionals); `AV.2.3` shunting-yard DTA for Sheets precedence tower. The automaton's state machine is a single file; no parallelism inside this wave. | V3 |
| **V3 — Pipeline staging (parallel)** | (a) `AV.2.4` diagnostic replay on top of the DTA, (b) `AV.3` stage-B parallel payload fill wired through `rayon` with fingerprint-gated activation, (c) `AV.4` stage-C prefix-scan finaliser. Disjoint file bounds: replay plugs into the DTA driver, stage B owns `bbnf-tape/src/psi.rs`, stage C owns the span/child finaliser module. | V4 |
| **V4 — Shape dedup (parallel)** | (a) `AV.5` ShapeDictionary for CSS (declaration shape on bootstrap.css as the proof point), (b) `AV.6` ShapeDictionary for BBNF (`__big_comment`, `__mapped_factor`). Shared `bbnf-tape/src/kind.rs` addition for `TapeKind::ShapeRef` lands in V3 so both V4 agents can consume it without collision. | V5 |
| **V5 — Parallel document parse (serial)** | Single agent: `AV.7` identifies list rules via structural mining, wires fork-point detection via the structural bitmap, implements tape-offset remap at join. The structural bitmap prerequisite from AU must be in master. | V6 |
| **V6 — Keyword dispatch (parallel)** | (a) `AV.8` perfect-hash tables for CSS named colors and Sheets function names, (b) `AV.8` SIMD-wide keyword compare for CSS `colorType` and BBNF `__directive`, (c) `AV.9` runtime bloom-hashcons extension — only if V4 measurement shows ShapeDictionary saturation; otherwise skipped. Disjoint file bounds per dispatch family. | V7 |
| **V7 — Tranche completion (serial)** | Single agent: full bench re-run, `post-AV.json` write, `FINAL.md` composition, workspace test confirmation. No code changes this wave. |

**Cross-wave invariants.**

- Every wave commits onto master before the next wave dispatches.
- No file is written by two agents in the same wave. When in
  doubt, sequence instead of parallelise — the DTA and the tape
  are the two substrates where parallel writes produce the
  largest collision surface.
- Wave V4 depends on `TapeKind::ShapeRef` existing; that addition
  lands in V3's finaliser-file worktree so V4 can fan out without
  editing the kind enum concurrently.
- If V3's stage-B rayon path regresses any fingerprint-gated
  grammar below the AU-complete baseline, V3 halts and the
  parallel activation gate is re-tuned before V4 dispatches.

## Plan — 8 phases (post-AU-pivot)

AU moved the columnar substrate pivot (AoS → SoA) from AV into AU
itself (AU Phase 7), along with the complete CSS grammar typing
audit (AU Phase 2) and the auto-generation / total-type-coverage
invariants. AV therefore assumes a columnar tape is the ground
truth and focuses on the parsing-pipeline flattening, dedup, and
parallelism layers.

### Phase 1 — GrammarProfile as codegen output channel

Promote the push fingerprint, fixed-shape e-classes, payload-type
inventory, structural alphabet, list-rule set, keyword tables, and
per-rule branch priors into a single `GrammarProfile` struct emitted
into `generated.rs`. Every AV phase reads this struct; every downstream
decision (capacity, dispatch LUT, shape-dict selection, parallel fork
point) resolves against it.

```rust
pub struct GrammarProfile {
    pub push_compound_count: u16,
    pub push_leaf_count: u16,
    pub push_leaf_with_count: u16,
    pub compounds_per_input_byte: f32,
    pub leaves_per_input_byte: f32,
    pub payload_bytes_per_input_byte: f32,
    pub structural_alphabet: &'static [u8],
    pub structural_digraphs: &'static [[u8; 2]],
    pub list_rules: &'static [RuleId],
    pub keyword_tables: &'static [KeywordTable],
    pub shape_dict: &'static [ShapeEntry],
    pub branch_priors: &'static [BranchPrior],
}
```

This struct is the runtime face of the existing IR data: `bbnf-ir`
passes already compute every field; Phase 1 just plumbs them through
`GrammarIR → Emitter → generated.rs`. No new analysis.

Hard gate: `generated.rs` contains a `const GRAMMAR_PROFILE:
GrammarProfile` for every grammar that AV targets, and every
subsequent phase's codegen consumes it.

### Phase 2 — Dispatch Tape Automaton (DTA)

Replace the monolithic per-rule recursive functions with a grammar-
derived DFA + counter that mines the full tape skeleton in one linear
byte pass. Each record is emitted with correct `kind_meta`, `variant_idx`,
`meta_idx`, and empty `span_hi` / `child_off` / `payload_idx`. The
automaton tracks Seq/Alt/Repeat frames with a counter stack instead of
a Rust call stack; Alt branch selection consumes bytes from the input
or positions from the AU.2.7 structural bitmap. No per-rule function
call in the hot path.

Sub-phases:

- **AV.2.1 DTA synthesis.** Lift `GrammarIR` to a counter-DFA.
  Seq frames are linear advance counters; Alt frames are branch-
  index registers populated by a dispatch LUT; Repeat frames are
  count + body-DFA pointer. The synthesiser lives in
  `crates/core/src/backend/rust/emitter/dta.rs` (new file).
- **AV.2.2 Counter-DFA states for optional-with-lookahead.** BBNF's
  `__mapped_factor` body `(… ( "->" __value_expr __type_annotation? )? …)`
  requires counter states — pure DFA cannot represent the optional-
  with-empty-body case without exploding state count. Counter-DFA
  handles it with one extra counter per nested optional.
- **AV.2.3 Shunting-yard DTA for Sheets precedence tower.** The
  six-level left-recursive chain (`__comparison_expr → … →
  __unary_expr`) collapses into a single operator-precedence loop
  with a rank-encoded operator stack. The DTA emits one compound
  per operator that actually fires, not one per level per formula.
  This is AU.6.3's "Pratt flattening" — AV discovers it naturally
  through the DTA, not as a special case.
- **AV.2.4 Diagnostic replay.** Recoverable errors require
  `furthest_offset` tracking. The DTA's happy path does not
  backtrack; diagnostics live behind a replay pass over the same
  DTA state machine, re-entered on parse failure with a different
  cost model. One automaton, two driver modes — no second codegen
  path.

Hard gates: every hot rule body in `crates/core/src/grammar/generated.rs`
is replaced by a table-driven DTA step. No function named `__<rule>`
exists in the monolithic hot path. Counter-DFA states for every
BBNF/CSS optional-with-lookahead rule round-trip through the existing
tape parity fixtures.

### Phase 3 — Stage B parallel payload fill

The DTA emits a PSI (`Vec<PayloadJob>`) alongside the skeleton tape —
one entry per scalar leaf: `(rec_idx: u32, input_lo: u32, input_hi:
u32, payload_kind: u8)`. Stage B is a `rayon::par_iter_mut` over PSI
chunks; each worker runs the terminal scanner (`scan_number_strict_f64`,
`decode_json_string_to_arena`, `parse_hex_color`, CSS dimension
`(f64, u8)` packer, named-color u32 table lookup, etc.) and writes
the decoded payload into a dense typed column (see Phase 7).

Sub-phases:

- **AV.3.1 PSI layout and pre-allocation.** `Vec<PayloadJob>`
  capacity derived from `GrammarProfile::leaves_per_input_byte ×
  input.len()`. Pre-ordered by `rec_idx` so stage-B workers own
  disjoint column slot ranges.
- **AV.3.2 rayon thread pool with fingerprint-gated activation.**
  `input.len() × expected_parse_ns_per_byte < 50 µs` →
  single-threaded stage B (the thread-startup cost would dominate).
  The gate is compile-time per grammar, chosen from `GrammarProfile::expected_ns_per_byte`.
- **AV.3.3 False-sharing avoidance.** Chunk PSI into cache-line-
  aligned strides (4 records / 64 B) so close `rec_idx` values
  don't live on the same line across workers.

Hard gates: stage B uses `par_iter_mut` for payload decode on all
grammars where the gate passes; non-gated grammars run stage B
single-threaded on the same API (no code-path fork). Canada.json
payload decode shows a per-core throughput gain proportional to core
count; thread-startup cost stays below 5% of the smallest gated
dataset's parse budget.

### Phase 4 — Stage C prefix-scan finaliser

Stage A emitted records with `span_hi = 0` and `child_off = 0`
alongside a `frame_depth: Vec<u8>` per record. Stage C runs a
segmented prefix scan over `frame_depth` to reconstruct parent-child
relationships (Prüfer-style tree unfolding), then writes `span_hi`
from each compound's last-child end and `child_off` from each
compound's first-child index. Single-threaded initially; parallelises
as a segmented prefix scan when measurements justify it.

Hard gate: after stage C, the tape is bit-identical to the
pre-AV recursive-descent tape for every fixture under
`crates/core/tests/fixtures/tape_golden/`. No structural record count
drift, no `child_off` divergence, no `span_hi` off-by-one.

### Phase 5 — ShapeDictionary for CSS

Prototype the shape-dedup path on CSS's single highest-dedup shape:
`declaration = propertyName : value ;` on `bootstrap.css`. Select via
CSP: e-graph's `EClassFacts.is_fixed_shape` + `RecognizerSignature.shape_hash`
identify candidate templates; the CSP minimises `-freq(c) × savings(c)
+ static_entry_cost` under a 256-entry budget. The selected shape
becomes a `ShapeEntry` in `GRAMMAR_PROFILE.shape_dict`.

The DTA recognises matching subtrees at emission time and writes a
single `TapeKind::ShapeRef` leaf (`kind_meta = ShapeRef`, `meta_idx =
dict_idx`, span covers the full matched region, payload column holds
the shape's non-constant leaf spans/values) instead of the 5–7
records the shape's expansion would otherwise produce.

Sub-phases:

- **AV.5.1 TapeKind::ShapeRef kind + cursor expansion.** Add the
  variant; extend `TapeCursor::children` to lazily expand a
  `ShapeRef` into its skeleton children by template lookup. No
  other cursor accessor changes.
- **AV.5.2 ShapeDictMiner.** Folds into the single-walk miner
  substrate at `crates/ir/src/passes/recognizers/mod.rs`. Emits
  `(NodeId, ShapeTemplate)` into `MineOutputs`.
- **AV.5.3 shape-dict CSP constraint.** One new file
  `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs`
  adds the include/exclude variable per candidate and the 256-
  entry cardinality constraint.
- **AV.5.4 Bootstrap.css measurement.** Validate on fixture: the
  5000+ declaration-shape records collapse to ShapeRef leaves;
  bench throughput must exceed 700 MB/s.

Hard gate: bootstrap.css parses to a tape where `declaration`
subtrees are `ShapeRef` leaves; `.view().declaration_iter()` returns
semantically identical typed declarations; bench ≥ 700 MB/s.

### Phase 6 — ShapeDictionary for BBNF

Generalise Phase 5 to BBNF's two highest-dedup shapes:

- `__big_comment` — a single-hole template (`(* …comment body… *)`
  or `/* … */`). One dict entry, one-record collapse from the
  current three-record `Rule → Repeat → Span` wrap. Subsumes
  AU.6.9.
- `__mapped_factor` with empty `->` branch — the common case where
  no mapping appears. The optional `( "->" __value_expr
  __type_annotation? )?` clause collapses into a template whose
  non-constant slot is just the base factor's tape offset.

Hard gate: BBNF self-hosting bench throughput (all six entries:
json, ebnf, css_pretty, google_sheets, bbnf_self, css_l4_grammar)
improves by ≥ 20% from AU baseline. `__big_comment` self-share
across all six drops below 3%.

### Phase 7 — Document-level parallel parse

The `list_rules` entry of `GrammarProfile` names the rules whose
body shape is "list-of-X" at document scope: CSS `stylesheet = (ruleset
| at_rule)*`, JSON root `value` when it's an array or object, BBNF
`grammar = rule+`, Sheets `file = formula_line*`. At these rules, and
only at these rules, the DTA emits a **fork point**: a safe boundary
where a worker can start parsing its chunk independently. Tape
chunks are concatenated after all workers finish; offsets are remapped
in one pass.

Sub-phases:

- **AV.8.1 List-rule identification pass.** Structural mining
  flags candidate rules in `GrammarProfile::list_rules`. A rule is
  a fork candidate iff (a) its body is a Repeat over an Alt or a
  single compound rule, (b) its children carry no cross-item state
  (first-set check), (c) each item's byte extent is bounded by a
  structural-bitmap position.
- **AV.8.2 Chunk boundary detection.** The stage-1 structural
  bitmap already marks every ruleset / array-element / rule /
  formula-line boundary. Workers take contiguous bitmap regions.
- **AV.8.3 Tape offset remap.** Each worker writes into a local
  tape; the join phase concatenates and rewrites all `child_off`
  references by a single offset. O(records) linear pass.
- **AV.8.4 Parallel activation threshold.** Fingerprint-gated like
  Phase 3: only activate when `input.len() ≥ parallel_break_even`
  for the grammar. Small inputs stay single-threaded on the same
  DTA.

Hard gate: tailwind.css cold parse ≥ 1.2 GB/s (2× the AU baseline
of 608 MB/s). JSON canada ≥ 1800 MB/s on a 4-core machine. No
regression on small inputs (BBNF `json.bbnf`, Sheets `parse_simple`)
— the fingerprint gate guarantees it.

### Phase 8 — SIMD keyword dispatch + perfect-hash tables

Grammar keyword sets reach the emitter through `GrammarProfile::keyword_tables`:

- CSS `namedColor` — 148 entries → `phf::Map<&'static str, u32>` (2 KiB perfect-hash table, fits in L1).
- CSS `colorType` — 9 entries → SIMD-wide compare (pack all keywords into one vector; one parallel compare emits a match bitmask; `trailing_zeros` picks the branch).
- BBNF `__directive` — 8 entries → keyword-prefix SIMD match keyed on the `@`-suffix byte.
- Sheets function names — variable, also PHF.

The DTA's Alt dispatch consumes these tables directly. An if-else
chain over keyword comparison becomes one table lookup or one
SIMD operation.

Hard gate: CSS `__compoundSelector` self-time drops below 20% (from
33–43% on wave-2). Every keyword-heavy Alt in every grammar resolves
via table or SIMD, not a sequential comparison chain.

### Phase 9 — Runtime bloom-hashcons extension (conditional, gated)

If Phase 5's 256-entry compile-time shape dictionary saturates on
production workloads (bootstrap / tailwind / large real-world
stylesheets), activate the runtime extension: a bloom-gated
`FxHashMap<(rule_id u16, body_hash u64), TapeOffset>` that dedups
subtrees matching rule patterns the CSP didn't admit statically.

Gate: measurement-driven. If the static dictionary captures ≥ 90%
of dedup opportunities on the training corpus, Phase 10 is not
needed. If it captures less, Phase 10 activates behind a per-grammar
`@dedup_runtime` directive. One architecture either way — the
runtime extension layers over the compile-time dictionary without
changing the tape layout or the access API.

## Critical files

| File | Phase |
|------|-------|
| `crates/core/src/backend/rust/emitter/grammar_profile.rs` (new) | 1 |
| `crates/core/src/backend/rust/emitter/dta.rs` (new) | 2 |
| `crates/core/src/backend/rust/emitter/alt.rs` (DTA integration) | 2 |
| `crates/core/src/backend/rust/emitter/grammar.rs` (DTA entrypoint) | 2 |
| `crates/core/src/grammar/generated.rs` (replaces fn-per-rule with DTA) | 2 |
| `crates/bbnf-tape/src/psi.rs` (new — PayloadJob stream) | 3 |
| `crates/bbnf-tape/src/columns.rs` (new — per-type columns) | 7 |
| `crates/bbnf-tape/src/builder.rs` (stage A/B/C builder split) | 2, 3, 4 |
| `crates/bbnf-tape/src/kind.rs` (TapeKind::ShapeRef) | 5 |
| `crates/ir/src/passes/recognizers/shape_dict.rs` (new) | 5, 6 |
| `crates/ir/src/passes/csp_strategy/constraints/shape_dict.rs` (new) | 5 |
| `crates/ir/src/passes/recognizers/list_rules.rs` (new) | 8 |
| `crates/core/src/backend/rust/emitter/keyword_dispatch.rs` (new) | 9 |

## Hard gates summary

1. `GrammarProfile` emitted into `generated.rs` for every grammar (Phase 1).
2. No `fn __<rule>` monolithic recursion in hot path; DTA drives every grammar (Phase 2).
3. Sheets `parse_simple` ≥ 200 MB/s after shunting-yard DTA (Phase 2.3).
4. Stage B uses `par_iter_mut` for grammars where fingerprint gate passes (Phase 3).
5. Tape bit-identical to pre-AV for every tape-parity fixture (Phase 4).
6. bootstrap.css ≥ 700 MB/s with `declaration` as ShapeRef leaves (Phase 5).
7. BBNF self-hosting benches +20% from AU baseline; `__big_comment` self &lt; 3% (Phase 6).
8. tailwind.css ≥ 1.2 GB/s with document-level parallel parse (Phase 7).
9. CSS `__compoundSelector` self &lt; 20% (Phase 8).
10. Every `->` annotation still reaches the tape; every tape-parity fixture passes; sonic-rs and lightningcss equivalence tests still hold (inherited invariants).

## Artifacts folded in

AV's concrete shape comes from six architecture research passes
conducted during AU. Their verbatim proposals live alongside this
document at `docs/tranches/AV/research/` — they are the source
material, not derivative summaries, and each contains specific
ISA-level, bit-layout-level, or algorithm-level detail that the
phase plan above references but does not reproduce:

- `01-simd-structural-bitmap.md` — grammar-parameterised structural-bitmap pre-pass (AU.2.7's v2 redesign; AV.2's stage-1 primitive).
- `02-fdmp-cache-locality.md` — fingerprint-directed memory planner; feeds Phase 1 `GrammarProfile` and the capacity closures for every AV-era buffer.
- `03-shape-dictionary-csp-egraph.md` — CSP + e-graph + structural mining pooled to select shape templates; backs Phases 5–6 and the compile-time half of Phase 10.
- `04-columnar-soa.md` — kind-partitioned struct-of-arrays; informs Phase 7.
- `05-parse-dag-bloom.md` — runtime content-addressed dedup; backs the optional Phase 10 extension.
- `06-psi-dta-parallelism.md` — two-stage skeleton/payload parse with grammar-fingerprinted DTA; the architectural spine of Phases 2–4 and 8.
