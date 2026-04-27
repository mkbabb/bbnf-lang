# AZ-I.W0.1 — Classifier-unification disposition

**Disposition: locked-split.**

The regex-HIR classifier, the structural-alphabet classifier, and the
payload-kind classifier each consume a different intermediate
representation, produce a different output type, and feed a different
consumer surface. They already cooperate across one shared boundary
(`RegexInfo::classification` → `StructuralAlphabet::quote_classes`) and
that boundary is load-bearing exactly because the producer knows
something the consumer cannot rederive. Folding the three onto a
unified driver would either lose that asymmetry (and re-introduce the
phantom-`t`/`f`/`n` and `[0..127]`-CSS over-flagging the AW-IV.W1.γ
restriction documents in `crates/ir/src/passes/sets/structural_alphabet.rs:41-54`)
or carry such large per-axis trait machinery that the merger is purely
nominal. The single load-bearing reason is that **each classifier's
input substrate is incommensurable with the others** — bytes (regex
HIR), node-shape categories (alphabet), projected-type tuples (payload
layout). A unified surface would have to carry all three and dispatch
internally; that is the existing three-classifier split with one extra
layer of indirection. AZ-I.W1 needs `StructRegistry` populated, not
classifier surface unification, and W1's scope cannot expand to absorb
unification without missing AZ-I close.

## The three classifiers in place

### 1. Regex-HIR classifier

- **Entry:** `../parse-that/rust/regex/src/classify.rs:180`
  `pub fn classify_regex_from_hir(hir: &Hir) -> RegexClass`.
  Wrapper at `:157` `pub fn classify_regex(pattern: &str) -> RegexClass`
  parses pattern → HIR first, then dispatches.
- **Input:** a parsed `parse_that::regex::hir::Hir` tree (byte-class
  ranges, `CharClass`, `Repetition`, `Group`, `Alternation`).
- **Output:** `RegexClass` enum
  (`../parse-that/rust/regex/src/classify.rs:55`) — `Numeric { allows_sign,
  allows_fraction, allows_exponent, reject_leading_zero,
  allow_leading_dot }`, `QuotedString { quote_char, allows_escapes,
  allows_u_escapes }`, `HexDigits`, `Identifier { allows_leading_dash,
  allows_double_dash_prefix }`, `WhitespaceWithBlockComment`,
  `CharClassQuantified(ClassRangeInfo)`,
  `PrefixThenClass { prefix, tail }`, `AccelDriven(u8)`, `Unknown`.
- **Decision substrate:** byte-level — character-class membership,
  literal-byte runs, repetition bounds, alternation shape. Pure
  structural HIR walk; no grammar-rule context.
- **Consumers (file:line):**
  - `crates/core/src/backend/kernels/quoted_string.rs:4` —
    `RegexClass::QuotedString` selects the JSON-string / CSS-string
    kernel.
  - `crates/core/src/backend/kernels/number.rs:5` —
    `RegexClass::Numeric` selects the JSON-number / CSS-number kernel.
  - `crates/core/src/backend/kernels/identifier.rs:4` —
    `RegexClass::Identifier` selects the CSS-ident / generic-ident
    kernel.
  - `crates/core/src/generate/regex/emit/scanner_plan.rs:142-168` —
    `RegexClass` arms drive `ScannerPlan::Kernel(...)` selection.
  - `crates/core/src/lower/expression/wrap.rs:584,608,622` — lowering
    inspects `classify_regex(...)` results to choose hex / numeric /
    leading-dot wrappers.
  - `crates/core/src/generate/regex/cost_model.rs:145-162` — cost
    model dispatches per `RegexClass` variant.
  - `crates/ir/src/passes/sets/structural_alphabet.rs:248` (cross-
    classifier link, see §Shared boundary).
- **Invariant preserved:** classification is purely structural —
  semantically equivalent regexes that differ only in surface form
  (`\d` vs `[0-9]`, group nesting, ordering) collapse to the same
  variant with identical field bindings. There is no nominal fast
  path; consumers never need to maintain a dialect dictionary.
  (Stated in module docstring `../parse-that/rust/regex/src/classify.rs:8-14`.)

### 2. Structural-alphabet classifier

- **Entry:** `crates/ir/src/passes/sets/structural_alphabet.rs:182`
  `pub fn compute_structural_alphabet(ir: &mut GrammarIR)`.
- **Input:** the entire `GrammarIR` — every `IrNode::Literal`,
  `IrNode::Repeat`, `IrNode::Alt` branch shape, plus
  `ir.regex_info.get(sid).classification` for `IrNode::Regex` quote-
  class extraction.
- **Output:** `StructuralAlphabet` written into
  `ir.structural_alphabet` (`structural_alphabet.rs:121-142`).
  Fields: `single_bytes: BTreeSet<u8>`, `digraphs: Vec<(u8, u8)>`,
  `digraph_mask: StructuralBitmap` (256-bit packed into `[u64; 4]`),
  `quote_classes: BTreeSet<u8>`. Bitmap helpers
  `build_byte_bitmap` / `bitmap_contains` / `bitmap_popcount` at
  `:89-110`.
- **Decision substrate:** node-shape categories — "single-byte
  literal," "Repeat separator," "Alt branch leading with single-byte
  literal," "exactly-2-byte literal" — *not* byte-class membership.
  The AW-IV.W1.γ restriction (`structural_alphabet.rs:41-54`) is the
  load-bearing scoping: pre-γ admission walked byte sets directly
  and over-flagged on multi-byte keywords (`t` from `"true"`) and
  byte-class-led Alt branches (every letter from `[a-zA-Z_]`); CSS L4
  mined `[0..127]` and JSON mined phantom singletons. The classifier
  is therefore *intentionally narrower* than a pure FIRST-set walk.
- **Consumers (file:line):**
  - `crates/core/src/backend/rust/emitter/profile.rs:52-60` — emits
    `GRAMMAR_PROFILE.structural_alphabet` slot; non-empty bake.
  - `crates/core/src/backend/rust/emitter/shapes/dispatcher/scan_policy.rs:62,221`
    — scan policy reads `profile.structural_alphabet.to_vec()` to
    pick kernel shape per cardinality.
  - `crates/core/src/runtime/mod.rs:54-55` — runtime constructs
    `scan::StructuralAlphabet::from_profile(GRAMMAR_PROFILE)` for the
    SIMD stage-1 scanner.
  - `crates/ir/tests/kernel_shape.rs:14-19` — kernel-shape selector
    keys on alphabet cardinality.
- **Invariant preserved:** `single_bytes` admits exactly the four
  enumerated categories (single-byte literals, Repeat separators,
  single-byte-led Alt branches, digraph first-bytes); the alphabet
  is a delimiter set, not a FIRST-set. Phrased in
  `structural_alphabet.rs:7-25`.

### 3. Payload-kind classifier

The payload classification surface is split across two cooperating
passes — one for tuple/scalar layout, one for materialization-class
gating — that share `GrammarIR` but project orthogonal axes. Both
post-AQ the monolithic `types/payload.rs` is retired; the
`crates/ir/src/passes/payload/` directory module is the post-B5
successor (per `feedback_directory_modules`).

- **Entry (a) — layout planner:**
  `crates/ir/src/passes/payload/layout.rs:107`
  `pub fn compute_payload_layouts(ir: &GrammarIR) -> HashMap<RuleId, PayloadLayout>`.
  Backend-resolver variant at `:128`
  `pub fn compute_payload_layouts_with_resolver<R: NamedTypeResolver>(...)`.
- **Entry (b) — materialization classifier:**
  `crates/ir/src/passes/materialization/classify.rs:88`
  `pub fn classify_materialization(ir: &mut GrammarIR)`.
  Fact-injecting test variant at `:116`
  `pub fn classify_materialization_with_facts(ir, facts)`.
- **Input:** projected per-rule `TypeDesc` (`ir.types`) for layout;
  `ir.dag` + `ir.fns` + per-rule `RuleMeta` directives + bottom-up
  `EClassFacts` for materialization. Backend-pluggable
  `NamedTypeResolver` trait
  (`crates/ir/src/passes/payload/named_types.rs:59-67`) projects
  `TypeDesc::Named(StringId)` to a concrete tuple shape per backend
  (`RustNamedTypes` lives in
  `crates/core/src/backend/rust/view/named_types.rs`).
- **Output (a):** `PayloadLayout { fields: Vec<PayloadField>,
  total_bytes: u8 }` (`layout.rs:49-64`) keyed per `RuleId`. Caps:
  `MAX_PAYLOAD_BYTES = 16` (`:23`), `LARGE_PAYLOAD_MAX = 64` (`:39`).
  KV-pair shape predicate `is_kv_pair_shape` at `:468`. Sentinel-
  routing predicate `scalar_range_includes_sentinel`
  (`scalar_routing.rs:39`) decides `InlineScalar` vs `WideScalar` at
  the `u32::MAX` collision boundary.
- **Output (b):** `MaterializationClass` map (`MustTape`,
  `TapeSpanOnly`, `TransparentElide`) written to
  `ir.materialization`. Lattice in
  `crates/ir/src/passes/materialization/lattice.rs:42-70`; monotone
  join `mat_join` widens toward `MustTape`.
- **Decision substrate:** projected types and per-node grammar shape
  — neither byte-level nor delimiter-level. Layout consumes
  `TypeDesc::Tuple(scalars)` and produces aligned offsets; the
  materialization pass consumes the IR DAG and an e-graph fact
  lattice and produces tape-emission commitments.
- **Consumers (file:line):**
  - `crates/core/src/pipeline/compile.rs:174,180,200,743-745` —
    `compute_payload_layouts` + `classify_materialization` invoked
    in the analysis pipeline.
  - `crates/core/src/backend/rust/emitter/grammar.rs:332` — emitter
    reads `ir.payload_layouts.get(&rule.id)` to admit scalar-packed
    projections; `:790-795` reads `ir.materialization.get(&node_id)`
    for per-node emission shape.
  - `crates/core/src/backend/driver/analysis.rs:151,159-160` — driver
    populates `ir.payload_layouts` via the resolver-backed entry,
    citing both passes.
  - `crates/core/src/backend/driver/mod.rs:103,147` — driver's
    `materialization_class(...)` accessor wraps the per-NodeId map.
  - `crates/core/src/generate/serialize/serialize.rs:45` — serialize
    pass keys on materialization to choose tape-record shape.
- **Invariant preserved:** the lattice is monotone — `mat_join`
  widens toward `MustTape`, never collapses an already-pinned class.
  Layout admission is gated on `is_scalar_payload()` per field; non-
  scalar fields force the rule to keep the compound pathway. The
  classification is *closed* (never emits an `Unsupported` /
  unknown-arm fallback) because every legal `TypeDesc` projects to
  exactly one disposition. Stated in `lattice.rs:7-26` and
  `layout.rs:7-23`.

## Where two classifiers disagree on a sub-question

There is exactly one canonical disagreement axis and one canonical
shared input. Both currently resolve in favour of "the producer
knows something the consumer cannot rederive."

- **Shared boundary — quote-class.** `compute_structural_alphabet`
  reads `ir.regex_info.get(sid).classification` and admits
  `quote_char` exclusively on
  `RegexClass::QuotedString { quote_char, .. }`
  (`structural_alphabet.rs:236-252`). The dependency is real: the
  alphabet pass cannot rederive whether a regex pattern is a
  quoted-string family without re-running the HIR classifier; the
  HIR classifier cannot tell the alphabet pass which `IrNode::Regex`
  references the pattern. The link is the canonical example of the
  three classifiers cooperating without merging — each side knows
  what it knows, and the boundary carries exactly the structured
  fact (a byte) the consumer needs.
- **Disagreement axis — leading-byte admission.** The HIR
  classifier admits the *first byte of every literal class* into
  classification surface (e.g. `[a-zA-Z_]` participates fully in
  `RegexClass::Identifier`). The alphabet classifier intentionally
  *excludes* class-led byte ranges from `single_bytes`
  (`structural_alphabet.rs:27-35`). A unified surface would have to
  carry both views simultaneously — "this is the structural
  delimiter set" *and* "these are the byte classes the kernel
  emitter accepts" — which is exactly the present split. The
  AW-IV.W1.γ restriction is preserved by the split; folding the
  passes loses the boundary that enforces it.

## Why locked-split is the binding answer

A unified driver is theoretically expressible as a trait family
`Classifier { type Input; type Output; fn classify(&self, ...) }`
parameterised over the three input substrates. That family
materialises three trait-object dispatches, three input projections,
and three output enums — at which point the merger has not reduced
the surface, only renamed it. The pluggable plug-points
(`feedback_pluggable-components`) that the unified design would
demand already exist in their natural homes:

- `NamedTypeResolver`
  (`crates/ir/src/passes/payload/named_types.rs:59`) — backend-
  specific name resolution for `TypeDesc::Named`.
- `RegexClass::canonical_pattern()`
  (`../parse-that/rust/regex/src/classify.rs:132-150`) — the producer-side
  canonical-form accessor that lets consumers avoid hard-coded
  pattern strings.
- `RegexInfo` cache
  (`compute_regex_info` ordered before `compute_structural_alphabet`
  in `crates/core/src/pipeline/compile.rs:696-706`) — the materialised
  cross-classifier handshake.

The migration cost of a unified surface is therefore high (rewire
every kernel and emitter consumer through a new dispatch surface),
the architectural payoff is low (the three classifiers continue to
hold three orthogonal axes), and the AZ-I.W1 budget is reserved for
`StructRegistry` closure on the three data grammars. Unification
inside W1 would either expand W1's scope past its declared bound or
leave the registry partially closed — both unacceptable per the
tranche's invariants (`AZ-I.md` §Invariants 1–2).

The locked-split disposition preserves the AW-IV.W1.γ restriction at
the alphabet boundary, the AF.1 e-graph fact gate at the
materialization boundary, and the AW.0.5 backend-resolver
abstraction at the payload boundary — three durable architectural
gains, each protected by its classifier's local invariants. A
unification proposal must defeat all three to pay its way.

## Conditions under which a future tranche could revisit unification

Unification becomes worth re-examining only when *every* one of the
following holds:

1. The three classifiers come to share a fourth substrate — for
   example, a unified type lattice that subsumes byte-class membership,
   node-shape categories, and projected `TypeDesc` simultaneously.
   The egraph substrate (`crates/egraph/`) is the candidate
   substrate; if egraph analysis grows to express both `RegexClass`
   variants and `MaterializationClass` lattice positions as the same
   e-class fact axis, the merger has a substrate to land on. As of
   AZ-I open this is hypothetical.
2. The `RegexClass`-as-canonical-pattern pathway
   (`canonical_pattern()` at `../parse-that/rust/regex/src/classify.rs:132`)
   becomes load-bearing across more than the two variants it
   currently covers (`Identifier`, `QuotedString`). A wider
   canonical-pattern surface signals the regex classifier's outputs
   are nominal-equivalent to grammar declarations and the
   structural-alphabet pass could read them directly.
3. A samply profile shows the three classifiers' separate walks of
   `ir.rules` constitute > 5% of compile-time on a representative
   grammar. Today the three passes cost negligible time on the
   profiles in `docs/benchmarks/profiles/` because each walks once
   and shares no work; if a future grammar size or pass-frequency
   shift makes the triple-walk cost dominant, fusing the walks
   becomes a win. Until then the duplication is *not* duplication —
   each pass extracts a different axis.

None of those three conditions hold at AZ-I open. The disposition
is binding for AZ-I and AZ-II.

## Hand-off

This document is read by two downstream consumers:

- **AZ-I.W1** consumes the locked-split disposition as an
  authorisation to populate `StructRegistry` *without* touching the
  three classifier surfaces. W1 reads `ir.payload_layouts` and
  `ir.materialization` through their existing entry points
  (`compute_payload_layouts_with_resolver`,
  `classify_materialization`) and adds a new
  `crates/ir/src/registry/struct.rs` module that *consumes* both as
  inputs to its `StructLayout` per Named rule. The shared-boundary
  contract `RegexInfo::classification → StructuralAlphabet::quote_classes`
  remains as the canonical inter-classifier handshake template; W1's
  registry-population pass is permitted to read the same `regex_info`
  and `structural_alphabet` slots without rederiving them.
- **AZ-II.W0** consumes this disposition as the constraint that the
  BBNF-bootstrap cutover must respect. AZ-II inherits three
  classifiers, not one; the BBNF-specific patterns AZ-II must add
  (per `AZ-I.md` §Handoff contract item 6) are extensions of the
  payload-kind classifier (BBNF Named rules entering the registry)
  and the materialization classifier (BBNF rule classifications
  joining the lattice). The regex-HIR classifier and structural-
  alphabet classifier are unchanged across AZ-II open. The shared
  boundary `RegexInfo::classification → StructuralAlphabet::quote_classes`
  continues to operate at the same point in the pipeline; AZ-II
  does not add new cross-classifier handshakes. If AZ-II's BBNF
  cutover surfaces a fourth classifier (e.g. a tape-emission policy
  classifier specific to `bbnf-derive`'s replacement), this
  disposition does not authorise its absorption — that is a fresh
  scope-reveal driving an AZ-II-internal sub-wave per
  `tranche/SPEC.md` §Scope-reveal protocol.

Both readers cite this document as a load-bearing input. AZ-I.W1
opens against it directly; AZ-II.W0 opens against it across a
tranche boundary. A change to the disposition statement at the top
of this document invalidates both downstream waves and triggers
re-planning per `tranche/SPEC.md` §Mid-tranche scope pivots.
