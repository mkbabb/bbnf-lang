# SK-V13 W5 Research - Regex Extraction And Decision Feature Gate

Cycle: W5 Research. Scope: read-only research for SPEC Section 8.

## Authority

W5 is `Decision Fold A: Regex Extraction + Feature Gate`. The owner paths are:

- `skinny/crates/parse-that-regex/`.
- Optional `skinny/crates/bbnf-regex/` if the plan accepts a new crate.
- `skinny/crates/ir/src/`.
- `skinny/crates/passes/src/`.
- Tests and reports named by the accepted plan.

Entry gate: W0 admitted, plus CHALLENGE acceptance for the new crate/path and
Lock 11/14 shape.

Exit gate: regex facts must be unit/property tested, consumed by IR/passes in
the same wave, hardcoded JSON regex pattern strings must leave generic decision
logic, JSON/CSS guards must maintain, and the facts must be consumed by a named
generated selection path that moves a row, admits a row, or records a measured
architectural block. Support-only extraction is a reject.

## Six-Agent Fan-Out

Six read-only research agents were dispatched for disjoint scopes:

1. `parse-that-regex` API and call-site surface.
2. `ir`/`passes` hardcoded regex predicate and decision-cascade surface.
3. `bbnf-bench` report/gate/Lock 14 surface.
4. CSS generated row consumer surface.
5. JSON direct/parse consumer surface.
6. SPEC/DISPATCH/REDRESS/Lock 14 constraints.

The constraints agent returned first and confirms that W5 cannot use a
support-only extraction, cannot reopen REDRESS 84/87/114/115/121 as local JSON
patches, and must pass CH1-CH6 before redress. The remaining scopes were also
inspected locally to keep the wave moving.

## Current Regex Surface

`skinny/crates/parse-that-regex/src/lib.rs` is a runtime scanner crate, not an
analysis crate. It exposes:

- string recognizers: `match_string`, `match_string_at_quote_trusted_utf8`,
  `unescape_string`, `StringMode`, `StringFlags`, and `StringMatch`.
- number recognizers through `number::match_number_span`,
  `match_number_span_from_first`, and `NumberSpan`.
- SIMD hook traits under `integration/`.

It has no grammar-neutral nullable, first-set, span-kind, or byte-class
analysis API. Those facts are duplicated directly in `ir` and `passes`.

The API scout recommends a new lightweight `skinny/crates/bbnf-regex` analysis
crate rather than making `ir` or `passes` depend on `parse-that-regex`, because
`parse-that-regex` depends on `bbnf-simd` and is already the runtime scanner
surface for generated JSON. The scanner APIs should remain stable to avoid
churning generated parse/direct/typed code.

## Hardcoded Decision Logic

Generic crates currently inspect JSON-pattern text directly:

- `skinny/crates/ir/src/lib.rs`:
  - `nullability` calls local `regex_is_nullable`.
  - `regex_is_nullable`, `is_single_regex_atom`, `ends_with_unescaped`, and
    `balanced_parenthesized` implement an IR-local partial regex parser.
- `skinny/crates/passes/src/lib.rs`:
  - `layout::types::regex_type` treats `[ \t\n\r]*` as unit and
    `starts_with('"')` as string-ish.
  - `recognizers::first_bytes` calls local `regex_first_bytes`.
  - `regex_first_bytes` hardcodes whitespace, JSON number, and quote-start
    string patterns.
  - `extract::lower_expr` and materialization role detection call
    `span_kind(pattern)`, which classifies whitespace by exact pattern,
    strings by `starts_with('"')`, and everything else as number.

This is the W5 target: the decisions are mechanically local, but the logic is
grammar-pattern text in generic passes. It also leaves `CostFacts` stale because
regex facts are not serialized into the decision evidence.

Additional agent findings tighten the risk:

- `passes::regex_first_bytes` hardcodes a JSON number shape using `[0-9]`, but
  `skinny/grammars/json.bbnf` uses `\d`. That exact-string mismatch can make
  number first-set facts silently become unknown.
- `branches_overlap` currently continues when `first_bytes` returns `None`.
  Once W5 extracts real facts, unknown regex first sets must fail closed or emit
  explicit decision evidence; silent skip is not admissible.
- `span_kind` classifies every non-whitespace and non-quote-leading regex as
  `Number`. That leaks JSON semantics into CSS, Sheets, or BBNF-self if a new
  grammar introduces another regex class.
- `derive_materialization_roles` still detects JSON roles from literals such as
  `true`, `false`, `null`, `{`, `}`, `[`, `]`, and `:`. W5 can route this as a
  separate policy/materialization problem, but it must not hide that JSON role
  inference under a regex-analysis abstraction.

## Minimal Extractable API

The smallest safe extraction is a grammar-neutral analysis crate rather than an
IR dependency on the runtime scanner crate:

```rust
pub enum RegexAtomKind { Whitespace, QuotedString, Number, Other }
pub struct ByteSet { bits: [u64; 4] }
pub struct RegexFacts {
    pub nullable: bool,
    pub first: FirstSet,
    pub byte_classes: Vec<ByteClass>,
    pub hir: RegexHir,
    pub string: Option<StringFacts>,
}
pub fn analyze(pattern: &str) -> RegexFacts;
```

This can live in scoped `skinny/crates/bbnf-regex/`, with
`parse-that-regex` optionally re-exporting it. `ir` and `passes` consume
`bbnf_regex::analyze` without depending on the scanner/SIMD crate. The API is
deliberately conservative: unknown regexes return `Other`, no first bytes, and
`nullable=false` unless a safe local shape proves otherwise.

The implementation should expose repo-owned fact types, not external HIR shape.
Using `regex-syntax` internally is lower risk than hand-parsing `\d`, negated
classes, ranges, non-capturing groups, and escapes, but external dependency
shape should not leak past `bbnf-regex`.

## Same-Wave Consumer

The same-wave consumer should be `passes::compile` and the generated lowering
metadata path:

- `ir::nullability` consumes `RegexFacts::nullable`.
- `passes::layout::types::regex_type` consumes `RegexFacts::kind`.
- `passes::recognizers::first_bytes` consumes `RegexFacts::first_bytes`.
- `passes::extract::lower_expr` and materialization role detection consume
  `RegexFacts::kind` for `SpanKind`.
- `CostFacts` or diagnostics should include a compact regex-decision evidence
  count so `codegen::cost_facts_from_source` and gate/docs can prove that the
  extracted facts reached generated selection.
- The extracted API should expose an explicit unsupported/unknown status so
  dispatch overlap and span-kind selection can fail closed instead of treating
  unknown as no-overlap or number.

This is row-moving only if a downstream generated row changes or improves.
Given current JSON/CSS generated rows already meet their selected paths, the
more realistic W5 disposition is a measured architectural block for row movement
in this fold: regex fact extraction is necessary for W6-W7, but W5 alone should
record unchanged JSON/CSS guards plus a gate-consumed block rather than claiming
throughput admission.

## Guard And Measurement Route

Recommended redress checks:

- `cargo test -p bbnf-regex` if the new crate is selected.
- `cargo test -p ir regex`.
- `cargo test -p passes regex`.
- `cargo test -p codegen cost_facts`.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory`
  to prove JSON/CSS guards remain readable.

The companion report shape identified by the gate/report scout should carry:

- `schema_id=sk-v13-decision-regex-v1`, `wave_id=SK-V13-W5`, a stable run id,
  and source/gate provenance.
- regex fact source, fact artifact path, fact SHA-256, IR/passes consumer paths,
  generated selection path, hardcoded-regex scan status, feature-gate status,
  cascade fallback status, and `row_move_toward_sota_status`.
- `row_move_toward_sota_status` must be `pass`, `admitted`, or
  `measured_architectural_block`; `support_only`, `gate_only`, empty generated
  selection, stale hashes, and silent cascade fallback reject.

The report should be consumed by `gate-json` via a
`--skv13-decision-regex-report` passthrough. Lock 14 must add SK-V13 W5 owner
paths for `parse-that-regex`, accepted `bbnf-regex` if created, `ir/src`,
`passes/src`, and the named report/gate paths.

The W5 redress artifact must state whether row movement occurred. If not, it
must use `REJECTED-MEASURED` or `IMPLEMENTATION-BLOCK` language for row
movement while still admitting/rejecting the extraction according to the SPEC
gate. It must not claim G2 completion by itself; W6/W7 own e-graph, active
cost, CSP, and cascade deletion.

The JSON consumer scout identifies the only W5-compatible row-movement class as
generated FIRST/follow dispatch selection for object/array envelopes. Candidate
measurement rows are `canada`, `mesh`, `github_events`, and `update_center`
`direct_to_struct`, because their hot leaves include generated object/array
envelope dispatch. Do not select `unicode_escapes`, `y_string_unicode`, string,
number, or digest routes for W5; those replay REDRESS 119/120 or unicode
primitive work instead of regex extraction.

If generated JSON/CSS selection cannot consume W5 facts in the same wave, the
block disposition should be named
`JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH` and cite unchanged
runtime/codegen consumers as measured evidence.

## Risks

- Adding `parse-that-regex` as an `ir` dependency would drag scanner/SIMD
  concerns into generic IR. A separate `bbnf-regex` analysis crate avoids this
  ownership leak.
- Any exact JSON pattern text left in `passes` after W5 violates the exit gate.
- A W5 source commit without gate-consumed decision evidence is support-only and
  must reject.
- A row-movement claim without strict JSON/CSS measurements is a paper close.
- The CSS consumer scout identified `css_l4/declaration_values_extended` as the
  lowest-risk CSS row because it runs `Scanner::emit_tokens` rather than a
  captured constant fast path. However, CSS runtime profiles are still emitted
  from static provider templates before JSON `lower_to_rust` is used, so CSS can
  only count as a W5 consumer if W5-generated regex facts are wired into the CSS
  generated selection path. Otherwise it is guard evidence and an architectural
  block, not row movement.

## Recommendation

Plan W5 around a scoped `bbnf-regex` crate plus consumers in `ir` and `passes`.
Keep redress small: extract facts, replace hardcoded generic predicates, add
tests, add decision evidence consumed by codegen/gate, and record measured
row-movement block if guard rows do not move. Route e-graph/cost/CSP work to
W6/W7 as required by the manifest, not as W5 deferral.
