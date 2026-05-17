# CH2 Generality Challenge - SK-V8 Alpha V1

Date: 2026-05-17.
Lens: CH2 Generality.
Overall disposition: REVISE.

## Read Set

- `restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-F-contract-draft.md`
- `restart/locks/LOCKS.md`
- `skinny/REDRESS.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`

## Verdict

The alpha packet is directionally sound for the current JSON skinny evidence,
but it is not yet generality-clean enough for G-Alpha. The final SK-V8 contract
must make Lock 14 a per-wave gate, not a late clean-up theme. JSON fixture rows
may drive SK-V8 row selection, but generic crates must not gain JSON policy,
JSON alphabets, JSON shape allowlists, or JSON-named public APIs.

The central revision is surgical: keep the JSON row evidence, but require every
intervention to declare whether it is (1) generated JSON output only, (2) a
grammar-neutral template/lowering change, or (3) a generic primitive. Cases 2
and 3 need non-JSON proof or an explicit no-op proof for CSS L4, Sheets, and
BBNF-self shapes before admission.

Any final-contract wording that weakens Lock 14, admits JSON code in generic
crates, or treats a JSON-only row win as a grammar-general architecture win is
REJECT.

## Alpha Artifact Dispositions

| Artifact | Disposition | Finding | Required fix |
|---|---|---|---|
| Alpha-A results extraction | ACCEPT | It reports the current JSON skinny authority without pretending those rows are non-JSON evidence. It also preserves caveats around strictness, output plane, and missing hot-leaf telemetry. | No content fix. Carry the caveat into final SK-V8: JSON row data is the opening benchmark surface, not proof of generic parser architecture. |
| Alpha-B competitor deltas | REVISE | The competitor registry is JSON-domain specific: sonic-rs, simdjson, yyjson, asmjson, RapidJSON, and serde_json are valid for JSON rows, but not generic comparator columns for CSS L4, Sheets, or BBNF-self. | In the final telemetry schema, render JSON comparator columns for JSON reports, but store comparator evidence as a registry of `{grammar, comparator_id, strictness, output_plane, run_id}`. Add a domain-comparator extension point for lightning-css, formula engines, and BBNF self-host rows. |
| Alpha-C REDRESS digest | REVISE | The pre-block list is strong, but the Lock 14 carry must explicitly include the older generic-crate JSON residue from REDRESS 36 and 37, plus the detached scanner/fossil concern from REDRESS 38 where it can reintroduce scanner drift. | Add a Lock 14 pre-block cluster: JSON alphabets in `bbnf-simd`, JSON god-module scanner surfaces, detached duplicate scanner crates, JSON binding helpers, `StructuralAlphabet::json`, JSON schema allowlists, and public `Json*` generic APIs. |
| Alpha-D validated/invalidated ledger | ACCEPT | It correctly demotes Lock 14 cleanup to architecture hygiene rather than performance, keeps typed product wins scoped to host/API schemas, and does not generalize JSON parse rows to other grammars. | No content fix. Fold its demotions into SPEC non-negotiables. |
| Alpha-E candidate shortlist | REVISE | All four candidates are usable only after tighter generality gates. Candidate 1 is JSON-row driven; Candidate 2 risks JSON-specific telemetry shape; Candidate 3 is required but too narrow; Candidate 4 must stay an abstract primitive envelope, not a JSON bitmap route. | Apply the candidate-specific fixes below before using Alpha-E as a dispatch basis. |
| Alpha-F contract draft | REVISE | The wave structure has the right themes, but grammar-neutral audit is too late, `gate-json` is written as if it owns the generic report model, and the Pass Omega open item could be read as permission to soften locks. | Split Lock 14 into a W0 preflight plus every-wave exit gate, then keep W5 as final reconciliation. Rephrase Pass Omega work as additive enforcement only; no Lock 14 weakening may block or enable G-Alpha. |

## Candidate-Specific Dispositions

### E1 - Twitter yyjson residual fusion-quality retained parser refactor

Disposition: REVISE.

The target row is legitimate: `twitter parse_only` is a hard JSON residual.
The risk is framing. A fusion-quality JSON parser rewrite is acceptable as
generated JSON output or grammar-neutral template work; it is not acceptable as
a generic parser rewrite that imports JSON control policy into `runtime`,
`codegen`, `parse-that`, `parse-that-regex`, `bbnf-simd`, `ir`, or `passes`.

Required fixes:

1. State that the row win is JSON-only unless a non-JSON grammar row is also
   measured.
2. If the wave edits generic templates or lowerers, name the grammar-neutral
   IR fact that selects the shape. Do not name JSON object, JSON array, JSON
   pair, colon, comma, quote, or brace policy in generic code.
3. Add a Lock 14 exit gate proving any new helper is either generated
   per-grammar output or a grammar-neutral helper selected by CostFacts.
4. Add a non-JSON no-op or dry-run proof: CSS L4 token classes, Sheets formula
   operators/ranges, and BBNF-self directives must not require a generic code
   edit to avoid the JSON path.
5. Do not cite yyjson as a generic comparator. It is a JSON-domain SOTA anchor.

### E2 - RESULTS schema completion and sidecar freshness gate

Disposition: REVISE.

This is necessary, but the data model must not turn JSON comparator columns
into the universal bench schema. The rendered JSON table can keep JSON columns;
the internal evidence model must be a grammar-aware comparator registry.

Required fixes:

1. Add fields `grammar_id`, `domain`, `comparator_id`, `comparator_plane`,
   `comparator_strictness`, and `sidecar_freshness` to the report model.
2. Treat `gate-json` as the JSON instance of a more general report contract,
   not as the owner of all future grammar-domain comparators.
3. For non-JSON rows, allow domain anchors such as lightning-css for CSS L4 and
   a formula/sheet engine for Sheets without adding hard-coded columns to
   generic crates.
4. Keep lossy and permissive comparator rows as flaw probes across all
   grammars, not just JSON.

### E3 - Remaining Lock 14 template-residue boundary audit and relocation

Disposition: REVISE.

This candidate is mandatory, but it is too narrow if it only audits codegen,
`bbnf`, `passes`, and `xtask`. REDRESS 36 and 37 show that `bbnf-simd` and
scanner surfaces are part of the Lock 14 risk. W7/W8 fixed major leaks, but
the final SK-V8 contract must prove no new leaks are added and that any old
residue is either relocated or explicitly frozen behind a per-grammar boundary.

Required fixes:

1. Expand owner/audit scope to `ir`, `passes`, `codegen`, generic `runtime`
   modules, `parse-that`, `parse-that-regex`, `bbnf-simd`, `analysis`, `lsp`,
   `bbnf`, and `xtask`.
2. Add a checked allowlist that permits only grammar source files, generated
   per-grammar output, tests, and an explicitly named per-grammar template
   provider while relocation is pending.
3. Move the Lock 14 audit preflight before performance waves. W5 can remain as
   final reconciliation, but it cannot be the first time W1-W4 are checked.
4. Include REDRESS 36/37/38 in the pre-block list and prove no JSON structural
   alphabet remains embedded in a generic primitive API.

### E4 - Bitmap asm bodies under changed density-gated measurement framing

Disposition: REVISE.

The changed framing is better than W10/W10b, but a generic SIMD crate cannot
admit a JSON-density predicate as generic architecture. The predicate must be
defined over grammar-neutral mask density and class-table facts. JSON rows can
be the first consumer, but not the only proof of generality if generic code
changes.

Required fixes:

1. Define the selector in terms of grammar-neutral facts: class-table width,
   mask density, quote/string-state equivalent if present, chunk-spanning-token
   presence, and CostFacts evidence.
2. Keep JSON punctuation, quote, slash, brace, colon, and comma data out of
   `bbnf-simd`; pass class tables or generated constants from the grammar
   boundary.
3. Add non-JSON primitive parity tests that exercise at least one non-JSON byte
   class table. If no non-JSON bench exists, the production admission remains
   JSON-only and cannot be cited as generic close evidence.
4. Retain scalar default outside the proven predicate and require full row
   non-regression on the W10/W10b falsifier rows.

## Alpha-F Wave Revisions

| Wave | Disposition | Required generality fix |
|---|---|---|
| W0 Baseline Profile And Telemetry Lock | REVISE | Add `grammar_id`, comparator registry fields, and a Lock 14 baseline allowlist. W0 must freeze existing JSON residue and fail any new generic-crate JSON policy before W1 opens. |
| W1 Typed Product Plane Expansion | ACCEPT with fix | This is the best generality path because host/API schemas apply beyond JSON. Add a non-JSON schema dry-run or IR-level test showing the same DirectSchema facts can describe CSS AST, Sheets formula, or BBNF-self outputs without a new directive. |
| W2 Parse Candidate From Fresh Profiles | REVISE | W2 entry must include CH2 acceptance for the selected route. If W2 touches a generic crate, it needs grammar-neutral API proof and non-JSON tests. If it touches only generated JSON output, the result must be labeled JSON-only. |
| W3 Direct Guard Triage | REVISE | Keep digest guard rows as JSON stressors unless converted to real host/API product schemas. Do not let digest closure become a product-plane claim for non-JSON grammars. |
| W4 CostFacts Gate Integration | ACCEPT with fix | CostFacts is the right substrate. Add a test that `CostFacts` contains no grammar names and can represent at least one non-JSON rule with chosen shape, rejected alternatives, and evidence source. |
| W5 Grammar-Neutral Audit And Lock 14 Preservation | REVISE | Split this into W0 preflight, every-wave exit checks, and final W5 reconciliation. A late-only Lock 14 audit is insufficient. |
| W6 Close, Redress Reconciliation, And Alpha Feedback | ACCEPT with fix | Close report must label every admitted row as JSON-only, product-plane, or grammar-neutral. It must list any generic-crate edit and its non-JSON proof. |

## Required Lock 14 Gate

Add this gate, or its equivalent, to every implementation-wave exit:

1. Public API scan: no new `Json*`, `json_*`, `StrictJson`, `JsonObject`,
   `JsonArray`, `JsonSink`, JSON structural alphabet, or JSON rule-name policy
   in generic crates.
2. Branch scan: no `match grammar { Json => ... }`, `if grammar == "json"`,
   per-grammar feature flags, or grammar-specific public types in generic
   crates.
3. Primitive scan: no JSON punctuation or JSON whitespace tables embedded in
   `bbnf-simd`; generic primitives receive class tables or selectors from the
   grammar boundary.
4. Template scan: per-grammar templates are either generated output or an
   explicit provider boundary. Generic lowerers consume IR, CostFacts, and
   metadata only.
5. Non-JSON proof: any generic crate edit must include at least one CSS L4,
   Sheets, BBNF-self, or synthetic non-JSON grammar test proving no JSON policy
   is required to compile, lower, or cost the shape.

The final SPEC should name exact commands after the file layout is finalized.
The command set must cover at least:

```text
skinny/crates/ir
skinny/crates/passes
skinny/crates/codegen
skinny/crates/runtime generic modules
skinny/crates/parse-that
skinny/crates/parse-that-regex
skinny/crates/bbnf-simd
skinny/crates/analysis
skinny/crates/lsp
```

Allowed locations must be explicit: grammar source files, generated
per-grammar output, tests, and the temporary per-grammar template provider if
it has not yet been relocated.

## Non-JSON Grammar Implications

### CSS L4

CSS L4 does not share JSON object/array control. It has identifiers, at-rules,
hashes, comments, functions, dimensions, strings, blocks, and layout-sensitive
token boundaries. Any parse fusion or SIMD byte-class candidate must be
selected from grammar IR token classes and CostFacts, not from JSON
punctuation. The relevant domain comparator is lightning-css, not yyjson.

Fix: W0 must let CSS rows or synthetic CSS token rules report their own
comparator and class-table facts. W2/W4 must prove generic changes do not
require JSON structural roles to exist.

### Sheets

Sheets formulas exercise Pratt/operator detection, cell/range references,
string literals, arrays, and function calls. JSON direct digest rows do not
prove Sheets materialization. Any typed product claim must enter through a
host/API schema, not a JSON key schema or hidden BBNF directive.

Fix: W1 typed expansion should include an IR-level or fixture-level proof that
DirectSchema facts can name formula/cell outputs without adding grammar arms to
generic crates.

### BBNF-self

BBNF-self has directives, grammar rules, closures, host functions, layout, and
pretty-printing vocabulary. It must not inherit JSON object/pair/string policy
from generic crates. Parse candidates that only optimize JSON recursive
descent cannot be cited as BBNF-self progress.

Fix: W5 final audit must include BBNF-self terminology in the grammar-name
scan and prove any generic CostFacts or codegen changes are role-based rather
than JSON-name-based.

## Rejected Interpretations

1. REJECT any Pass Omega or SK-V8 language that weakens Lock 14. Only additive
   clarifications, stronger gates, or enforcement automation are admissible.
2. REJECT any generic-crate patch that adds JSON-specific code, JSON-named
   public APIs, JSON structural tables, or grammar match arms.
3. REJECT any claim that a `twitter parse_only` or yyjson-gap win is evidence
   for CSS L4, Sheets, or BBNF-self without a separate non-JSON proof.
4. REJECT any SIMD/ASM admission based on correctness and JSON row data alone
   when the code lands in a generic primitive crate.
5. REJECT any direct-to-struct product claim that is only the JSON digest
   stressor. Product-plane claims require explicit host/API schema facts.

## Fold-In Checklist

Before G-Alpha, revise the final SK-V8 packet as follows:

1. Add a `Generality and Lock 14` non-negotiables section to SPEC.
2. Add W0 Lock 14 baseline allowlist and every-wave Lock 14 exit gates.
3. Add REDRESS 36/37/38 to the pre-blocked generality cluster.
4. Convert telemetry internals from fixed JSON comparator columns to a
   grammar-aware comparator registry, while keeping the JSON table rendering.
5. For W1, add a non-JSON DirectSchema proof or fixture-level dry run.
6. For W2, require CH2 acceptance after W0 profiles identify the parse route.
7. For W3, label digest rows as guard stressors unless converted to real
   typed product schemas.
8. For W4, prove CostFacts remains grammar-neutral and supports non-JSON rule
   facts.
9. Split W5 into preflight, per-wave checks, and final reconciliation.
10. Remove or constrain Alpha-F open item 7 so Pass Omega cannot weaken Lock 14.

After those fixes, CH2 should move from REVISE to ACCEPT.
