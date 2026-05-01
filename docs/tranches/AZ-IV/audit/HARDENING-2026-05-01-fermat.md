# AZ-IV Hardening Pass 3 — FERMAT — Grammar Generality and Overfitting Audit

**Author**: agent FERMAT, hardening pass 3
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-harden-fermat`
**Mandate**: audit grammar generality and rule-name overfitting per AZ-IV §Invariants 2 + 4 and Loss-Prevention §3.

The user's two orienting clauses are non-negotiable: **"NO GRAMMAR
OVERFITTING!"** and **"semantic parity, it MUST be grammar derived for
all parsers"**. This audit reads every observable place where literal
parser names, literal grammar rule names, or hand-coded discriminator
tables route production behaviour, and classifies each.

## 1. Scope And Counts

`rg` census across `crates/core/src/` + `crates/ir/src/`, excluding
`generated/`, `tests/`, doc-comments, and `.bbnf` source:

| Pattern | Production count | Doc count (excluded) | Notes |
|---|---:|---:|---|
| Literal parser-struct identifiers (`JsonParser`, `BbnfParser`, ...) | 11 | 38+ | 9 in `EmitStrategy::for_grammar` allowlist; 2 BBNF self-host entry points |
| `from_rule_name(&str) -> Kind` impls (production runtime) | 7 | n/a | one per non-JSON struct-direct grammar |
| `match (layout.kind, layout.rule_name.as_str())` (production runtime) | 2 | n/a | JSON builder + CSS L4 builder |
| `match rule_name { "<literal>" => ... }` (production passes) | 1 | n/a | `shape_dict_bbnf::mine_bbnf_shape_templates` |
| `_ => panic!(unknown grammar)` | 1 | n/a | `EmitStrategy::for_grammar` |
| Fallback to `::bbnf::runtime::JsonStructBuilder` | 1 | n/a | `substrate.rs:76` (Meitner's "fallback-to-JSON") |
| `leak_static_str` rule-name interner (CSS L4) | 1 | n/a | 30 hard-coded rule names |
| Recovery functions coupled to BBNF byte-text | 3 | n/a | `recover_modifier`, `recover_binary_op`, plus PrettyHint argument decoders |

The non-doc parser-name surface is small (11 sites), but the
**rule-name surface is large and load-bearing**: 7 `from_rule_name`
tables + 2 `(kind, rule_name)` builder dispatches + 1 mining pass
match + 1 `leak_static_str` rule-name interner. Every one of these is
the actual `begin_compound` discriminator deciding which `OpenFrame`
the runtime opens — they are parity-bearing, not diagnostic.

## 2. Findings By Classification Severity

### 2.1 OVERFIT-HARD (W1 hard gate must reject; rewrite required before close)

**F1. Per-grammar `from_rule_name(&str) -> CompoundKind` discriminator
tables in production runtime.** The `bbnf`, `bnf`, `csv`, `css_pretty`,
`ebnf`, `google_sheets`, `math` runtime crates each ship a hand-coded
`from_rule_name` lookup that maps grammar-rule-name strings to a
discriminator enum the builder dispatches on at `begin_compound`:

- `crates/core/src/runtime/bbnf/arena.rs:133-` — 30+ rule-name → `BbnfCompoundKind` arms
- `crates/core/src/runtime/bnf/arena.rs:24-`
- `crates/core/src/runtime/csv/arena.rs:48-`
- `crates/core/src/runtime/css_pretty/arena.rs:34-58` — 21 rule-name arms
- `crates/core/src/runtime/ebnf/arena.rs:30-`
- `crates/core/src/runtime/google_sheets/arena.rs:147-187` — 30+ rule-name arms (full Sheets vocabulary, including `comparison_expr`, `concat_expr`, `add_expr`, ..., `cell_or_range`, `error_literal`, `sheet_prefix`)
- `crates/core/src/runtime/math/arena.rs:43-`

Loss-prevention §3 explicitly identifies this pattern as an AZ-IV W1
gate failure: "Hand-coded normalizers, rule-name dispatch tables,
host-shim duplicates, and synthetic payload defaults are
**supplementary diagnostics, not parity proof**". These are the
parity proof.

The W1 hard gate text already says: "CSS/Sheets parity cannot close on
early-return payload gaps, hand normalizer equivalence, **rule-name
projection**, or synthetic default payloads" (AZ-IV.md §Hard Gates 4).
That gate, read literally, fails closed against the seven
`from_rule_name` tables and against the JSON+CSS-L4
`(layout.kind, layout.rule_name.as_str())` builder dispatches at
`crates/core/src/runtime/json/builder.rs:262` and
`crates/core/src/runtime/css_l4/builder.rs:325`.

**F2. CSS L4 `(layout.kind, layout.rule_name.as_str())` builder
dispatch at `crates/core/src/runtime/css_l4/builder.rs:325-451` plus
the dimension-unit dispatch at `:521-548` plus the 30-name
`leak_static_str` interner at `:790-833`.** This is the parity-bearing
choice of `OpenFrame::StyleSheet`/`OpenFrame::StyleRule`/`OpenFrame::Numeric`/etc.,
keyed by literal rule names like `"stylesheet"`, `"qualifiedRule"`,
`"length"`, `"angle"`, `"colorFunction"`. A grammar rename in
`grammar/css/css_l4.bbnf` silently breaks parity; the runtime keeps
parsing but emits wrong-tagged compounds. Same condemnation as F1.

**F3. JSON `(layout.kind, layout.rule_name.as_str())` builder dispatch
at `crates/core/src/runtime/json/builder.rs:262-282`.** Same pattern.
Smaller surface (`array`, `object`, `pair`, default `Wrap`) but the
same overfit shape; the W1 hard gate must reject it.

**F4. Fallback-to-`JsonStructBuilder` substrate path at
`crates/core/src/backend/rust/emitter/shapes/substrate.rs:70-78`.**

```rust
fn substrate_path(path: &'static str) -> TokenStream {
    match syn::parse_str::<syn::Path>(path) {
        Ok(parsed) => quote! { #parsed },
        Err(_) => quote! { ::bbnf::runtime::JsonStructBuilder },
    }
}
```

If the per-grammar `SubstrateBinding::builder_path` string fails to
parse, codegen silently falls back to `JsonStructBuilder` — a CSS
grammar would emit code that tries to drive JSON's compound family.
This is exactly the AZ-IV thesis-violating substrate: "fallback-to-JSON
substrate path" is named in §Invariants 7 and §Hard Gates 10 as a
deletion target. **Must be replaced with hard-fail (panic with the
binding string).**

### 2.2 OVERFIT-CARRY (ship in AZ-IV with named replacement plan)

**F5. `EmitStrategy::for_grammar` at `crates/ir/src/registry/strategy.rs:143-262`.**
Nine literal parser-name arms (`"JsonParser" | "JsonGrammar"`,
`"GoogleSheetsParser" | "GoogleSheetsGrammar"`, ..., `"CssPrettyParser"
| "CssPrettyGrammar"`), each binding a hard-coded `builder_path` /
`document_path` string. AZ-IV §Hard Gates 2 says "Parser strategy
binding is manifest/registry driven; **a synthetic grammar
rename/addition test fails if a new literal parser-name arm is
required**". This arm-list directly violates that gate. The synthetic
test does not exist yet — it is an unwritten W0 task.

The legitimate piece of this design is that *some* registry must
resolve a grammar to its substrate binding; the illegitimate piece is
that the registry is a hand-coded match expression in a Rust source
file. Manifest-driven, registry-keyed binding (one entry per grammar
in `Cargo.toml`'s `package.metadata.bbnf-grammars` or in the grammar
manifest the W0 strict-regen consumes) is the architecture this gate
implies.

**F6. `pipeline/compile.rs:219-224` rule-name → `GrammarAuditTag`
table.**

```rust
let tag = match entry_name {
    "value" | "json" => GrammarAuditTag::Json,
    "stylesheet" | "css_l4" | "cssL4" => GrammarAuditTag::CssL4,
    "spreadsheet" | "sheets" | "google_sheets" => GrammarAuditTag::Sheets,
    other => GrammarAuditTag::Custom(other),
};
```

This is hard-coded grammar-entry-name aliasing. The audit-coverage
artefact is debug-only (`#[cfg(debug_assertions)]`) and consumed by
`target/audit/<entry>.json` only, so it is diagnostic, not parity
proof. **Carry**: replace with `Custom(entry_name)` for every grammar
and let the artefact key on the entry-rule string directly.

**F7. CSS L4 `leak_static_str` rule-name allowlist at
`runtime/css_l4/builder.rs:790-833`.** A 30-arm match that returns the
input string back as `&'static str` after confirming it lives in the
allowlist. If the grammar adds a new rule, the function returns `""`
and the OpenFrame's `rule_name` field is empty — silently wrong. This
should be a `Box::leak(name.to_string().into_boxed_str())` or rerouted
through an interner so the table is not the gating discriminator.

### 2.3 OVERFIT-BUT-NEEDED (substrate exists; needs grammar-fact migration)

**F8. `recover_modifier` / `recover_binary_op` BBNF lowering recovery
at `crates/core/src/lower/expression/{repeat.rs:125-161,
pratt.rs:279-309, wrap.rs:89-99}`.** These functions read the BBNF
source bytes directly to recover `?w` / `?` / `*` / `+` modifier
tokens and `<<` / `>>` / `-` binary operators that the codegen
alt_dispatch path drops as Span leaves. The recovery is structurally
sound but couples lowering to BBNF's own source byte sequence. Per
AZ-III.W2.4.t/s, the architectural fix is for the alt_dispatch
emitter to push modifier/operator tokens as typed leaves so recovery
becomes unnecessary. **Substrate exists** (alt_dispatch typed-payload
push at `shapes/alt_dispatch/branches.rs:227-298`). **Carry**: add
W1 sub-unit "BBNF self-host alt_dispatch surfaces modifier+operator
tokens; lower deletes recover_*."

**F9. `shape_dict_bbnf::mine_bbnf_shape_templates` at
`crates/ir/src/passes/recognizers/shape_dict_bbnf.rs:79-101`.** Mines
two BBNF-specific templates (`big_comment`, `mapped_factor`) by
literal rule name. Currently the only consumer of the BBNF shape dict.
**Carry**: AZ-IV.W2 (substrate denominator ledger) decides whether
this miner is consumed by a generated shape table or deleted.

**F10. PrettyHint host-binding decoders at
`crates/core/src/grammar/host.rs:329-376` (and surrounding directive
decoders).** The directive-extraction path uses `BbnfCompoundKind`
discriminator (typed enum from generated code) for shape, then
`pretty_hint_text` walks the leaf children. This is GRAMMAR-DERIVED at
the structural level (the enum variants are derived from BBNF's own
grammar), but the recovery argument logic at `decode_single_name`
strips text via `text.strip_prefix(keyword)` — keyword strings
(`@token`, `@debug`) are hand-coded. **Carry**: harden via grammar's
keyword projection (already typed in BBNF) rather than literal
prefix-strip.

### 2.4 GRAMMAR-DERIVED (clean — keep)

**G1. `alt_dispatch::branch_payload_push` (CSS named_color substrate)
at `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs:227-298`.**
Walks `IrNode::Map { fn_id }` chains, inspects `FnDescriptor::Expr {
expr, .. }`, constant-folds `MapExpr::IntLit(n)` etc. into
`push_leaf_with_u64(n)`. **Zero rule-name compares.** Generic across
any AltDispatch rule whose branches carry typed `->` projections; CSS
named_color is the canonical consumer but the mechanism is
grammar-general per its doc comment. **Audit confirms grammar-general,
not CSS-specific.**

**G2. `root_rule_name(ir: &GrammarIR)` consumers at
`crates/core/src/backend/rust/emitter/shapes/{object,array,arglist,unordered,pratt/struct_direct,flat/struct_direct}.rs`.**
These call `root_rule_name(ir)` and use the returned string only to
synthesize the `dispatcher_fn_ident(grammar_suffix, root)` ident. The
"match" patterns are `Some(name) => format_ident!(...)` / `None =>
return quote! {}`. **No literal-name branching**, only a grammar-fact
projection (the IR's entry rule). Clean.

**G3. `host.rs` BBNF directive decoders (structural).** Use
`BbnfCompoundKind::PrettyHint` etc. (typed enum projected from BBNF's
generated grammar). Grammar-derived discriminator; not a string match.

**G4. `FnDescriptor::HexConvert { fn_path: StringId }`.** The
`fn_path` is interned and resolved at codegen time (`ir.get_string`)
or runtime call sites (`ir.get_string(*fn_path)` then spliced as a Rust
path). Per `crates/core/src/lower/expression/wrap.rs:492-494`, the path
is captured from the BBNF grammar's host-fn name; the descriptor is
*data*, not behaviour. **No grammar overfit.** The ts/wasm backends'
`emit_hex_convert` translates the path through its own rules
(`crates/core/src/backend/ts/emitter/value.rs:103-110`, last segment
becomes JS function ident); they consume the same descriptor without
adding their own arm-list.

**G5. CSS L4 named_color emit (the W3c.1 substrate).** Confirmed
grammar-derived per G1. Not CSS-specific in mechanism.

## 3. Top 5 Overfit Surfaces (by parity-load weight)

1. `crates/core/src/runtime/google_sheets/arena.rs:147-187`
   (`SheetsCompoundKind::from_rule_name`) — 30+ rule-name arms,
   parity-bearing.
2. `crates/core/src/runtime/css_l4/builder.rs:325-451` (begin_compound
   `(kind, rule_name)` dispatch) + `:521-548` (dimension unit) +
   `:790-833` (`leak_static_str`).
3. `crates/core/src/runtime/bbnf/arena.rs:133-` plus the six other
   non-JSON `from_rule_name` impls.
4. `crates/ir/src/registry/strategy.rs:143-262`
   (`EmitStrategy::for_grammar` literal parser-name allowlist).
5. `crates/core/src/backend/rust/emitter/shapes/substrate.rs:70-78`
   (fallback-to-`JsonStructBuilder` on parse failure).

## 4. Tailwind regex_scan Adapter And Per-Pattern Admission

Per AZ-III.C5 carry, the tailwind perf miss was routed to BB.W2 / now
AZ-IV.W2 for CSS-wide alphabet enumeration. The current emitted scanner
at `crates/core/src/backend/rust/emitter/dfa_codegen.rs:711-759`
already gates the LAST-byte narrowing on `input.len() >= 64 * 1024` and
`states.len() >= 4`, and the doc comment explicitly cites tailwind as
the intended beneficiary.

This is **GRAMMAR-DERIVED**: thresholds (`4`, `64 * 1024`) are emitted
constants on a per-pattern admission gate, not grammar-name branches.
The comment cites tailwind by name as motivation, but the code never
matches on `"tailwind"` or any CSS rule name. Clean.

## 5. Per-Grammar Runtime Modules — Hand-Coded vs Templated

`crates/core/src/runtime/{bbnf,bnf,csv,css_l4,css_pretty,ebnf,google_sheets,json,math}/`:

- `arena.rs`, `builder.rs`, `value.rs`, `document.rs`, `view.rs`,
  `path.rs`, `serialize.rs` — **hand-coded per grammar**, no shared
  template.
- `JsonDocument` / `SheetsDocument` / `CssDocument` / `BbnfDocument`
  share a shape (root + arena + input), but each file rewrites the
  surface independently.
- The `serialize.rs` per-grammar lexeme tables are the
  W1.2-sub-gate concern (Sheets, BBNF) — they encode token-level
  grammar facts as Rust constants. AZ-IV.W1.2 explicitly says: "no
  hand-coded serializer lexeme maps that duplicate grammar facts."

This is the broader manifestation of the same overfit: every grammar
gets a hand-coded runtime module pair that re-encodes its rule
vocabulary. **Carry to AZ-IV.W1**: a grammar-fact-driven runtime
template (parameterised by `StructRegistry` + `TypeDesc`) eliminates
the per-grammar arena+builder duplication. The `from_rule_name` tables
follow trivially because the registry already knows which compound
kinds exist for each rule.

## 6. Sonic-rs / lightningcss / Sheets / BBNF Oracle Parity Coverage

| Oracle | Coverage source | Grammar-derived? |
|---|---|---|
| sonic-rs (JSON) | `crates/core/tests/sonic_rs_parity.rs`, `json_value_parity.rs`, `json_canonical_parity.rs` | YES — compares generated `JsonDocument` value tree against `serde_json::Value` from sonic-rs. No rule-name dispatch in test. Cast through `cast_f64` oracle (W2.1) is value-level. |
| lightningcss (CSS) | `lightningcss_parity.rs`, `css_l4_parity.rs`, `css_l4_canonical_parity.rs`, `css_l4_named_color_parity.rs` | PARTIAL — `token_normalize` (in `tests/common/css_normalize.rs`) is a hand-coded byte-level normalizer applied symmetrically to BOTH sides; it cancels canonical-form whitespace/comment divergences. Diagnostic, not parity proof, per AZ-IV.W1.3 sub-gate. |
| Sheets corpus | `sheets_parity.rs`, `sheets_expr_parity.rs`, `sheets_self_parity.rs` | PARTIAL — needs grammar-derived projection per AZ-IV.W1.2. Current 122/133 (AZ-III) routed 11 to BA.W0 path API. |
| BBNF self-host | `bbnf_parity.rs`, `bbnf_self_parity.rs`, `bbnf_ast_parity.rs` | YES — 95/95 green via canonical generated path. Self-host parity is grammar-fact identity. |

## 7. Recommendations And Wave-Amendment Text

### Exact Wave-Amendment Text (to be added to `docs/tranches/AZ-IV/waves/W1.md`)

Append to W1 §Hard Gate (after current item 9):

```markdown
10. No production runtime path branches on a literal grammar parser
    struct ident or a literal grammar rule name string. The CI gate
    is a static AST scan over `crates/core/src/runtime/**`,
    `crates/core/src/backend/rust/emitter/shapes/**` (excluding
    `generated/` and `#[cfg(test)]` blocks): any
    `match <expr> { "<literal-rule-name>" => ... }` arm or
    `match (<expr>, "<literal-rule-name>") => ...` arm fails the
    gate. Replacement is type-inference-derived discriminator
    (`StructRegistry::compound_kind(layout)`,
    `TypeDesc::*` projection), not hand allowlists.
11. `EmitStrategy::for_grammar` is replaced by a manifest-driven
    registry: each grammar contributes one
    `[package.metadata.bbnf-grammars.<ident>]` row with builder/document
    paths; the resolver reads from the parsed manifest, no Rust source
    arm-list. A synthetic grammar `__test_strategy_synth__` registered
    only via manifest must round-trip codegen without adding a Rust
    arm.
12. The `substrate.rs:70-78` fallback to `::bbnf::runtime::JsonStructBuilder`
    on `syn::parse_str::<syn::Path>` failure is replaced with a hard
    panic naming the offending binding string. The W0 manifest gate
    already enforces well-formed binding paths; the runtime fallback
    is dead substrate that must be deleted.
```

Append to W1 §Scope as new sub-units:

```markdown
10. Eliminate every `from_rule_name(&str) -> Kind` discriminator
    impl in production runtime
    (`crates/core/src/runtime/{bbnf,bnf,csv,css_pretty,ebnf,google_sheets,math}/arena.rs`).
    Replacement: `StructRegistry`-projected discriminator field
    populated at codegen via the layout's already-tracked rule-id;
    the runtime reads `layout.compound_kind` directly.
11. Eliminate the `(layout.kind, layout.rule_name.as_str())` builder
    dispatches at `crates/core/src/runtime/json/builder.rs:262`,
    `crates/core/src/runtime/css_l4/builder.rs:325, :521`. Same
    replacement: typed `OpenFrame` selection from registry/typedesc.
12. Delete `runtime/css_l4/builder.rs:790-833 leak_static_str`
    rule-name allowlist; rule names that need static lifetimes go
    through one canonical interner.
13. Replace the `pipeline/compile.rs:219-224` rule-name →
    `GrammarAuditTag` aliasing with `GrammarAuditTag::Custom(entry_name)`
    for every grammar; the audit artefact keys directly on the entry
    rule string.
14. Architectural patch for `recover_modifier` / `recover_binary_op` /
    `wrap.rs:89-99`: the alt_dispatch emitter pushes modifier and
    binary-operator tokens as typed Span leaves; the lower path
    deletes its three byte-recovery functions. Substrate at
    `shapes/alt_dispatch/branches.rs:227-298` is ready; the activation
    is the W1 task.
15. Per-grammar runtime module deduplication: emit
    `arena.rs` + `builder.rs` from a shared template parameterised by
    `StructRegistry` + `TypeDesc` rather than maintaining seven
    hand-coded twins. Document/value/view surfaces are scope-bounded
    (some grammars need typed accessors, e.g. CSS's typed-color
    family); the deduplication target is the structural skeleton, not
    the typed-leaf API.
```

Add to W1 §Triumvirate Dispatch trigger list:

```markdown
- a parity surface needs a new `from_rule_name` arm, a new literal
  rule-name projection table, or a new `EmitStrategy::for_grammar`
  parser-name arm to land;
- a regression test demonstrates that deleting a `from_rule_name`
  arm cannot be replaced by a `StructRegistry` lookup without a
  type-inference pass extension.
```

### Routing Of OVERFIT-CARRY Items

| Item | Carry letter | Resolution wave |
|---|---|---|
| F5 (`for_grammar` allowlist) | AZ-IV.W0 | manifest-driven binding registry lands with the strict-regen W0 gate (already partially scoped per AZ-IV.md row "Strict regen drift"). |
| F6 (audit-tag aliasing) | AZ-IV.W1 | absorb in W1.5 shape-generality unit (touches the audit pass already). |
| F7 (`leak_static_str`) | AZ-IV.W1.3 CSS | already in CSS file bounds. |
| F8 (recover_*) | AZ-IV.W1 | open W1.6 sub-unit "BBNF alt_dispatch typed-leaf push for modifiers/operators". |
| F9 (BBNF shape miner) | AZ-IV.W2 | substrate denominator ledger decides consume-or-delete. |
| F10 (host directive decoders) | AZ-IV.W1.5 shape generality | grammar-derived keyword projection. |

### Hard-Fail Conversions In W1 (no carry; must land same wave)

| Item | Action |
|---|---|
| F4 substrate fallback | replace with `panic!("substrate_path: invalid binding {path}: {err}")`. |
| F1 `from_rule_name` impls (all 7) | delete bodies; replace with `StructRegistry::compound_kind_for_layout(layout) -> CompoundKindId`. |
| F2/F3 builder `(kind, rule_name)` matches | delete; replace with `OpenFrame::from_layout(layout, &registry)`. |

### CI Static Scan (file new at W1 close)

```rust
// crates/core/tests/no_grammar_name_branch.rs
// Static AST scan: no production source under runtime/** or
// backend/rust/emitter/shapes/** matches a literal rule-name in a
// match arm. Excludes generated/, fixtures, and #[cfg(test)] blocks.
```

The regression test for AZ-IV §Hard Gates 2 ("a synthetic grammar
rename/addition test fails if a new literal parser-name arm is
required") lands in the same wave at
`crates/core/tests/synthetic_grammar_strategy.rs`.

## Closing Note

The current architecture has the right bones: `StructRegistry` exists,
`TypeDesc` is durable, `FactAuthority` is wired, alt_dispatch already
demonstrates grammar-general typed-payload push (G1). The overfit is
in the runtime/builder seam — every grammar's runtime module
re-encodes the rule vocabulary the registry already knows, and the
strategy resolver re-encodes the parser-binding the manifest already
knows. AZ-IV.W1 is the wave that closes that seam. The seven sub-unit
amendments above name the closure pieces by file:line so no surface
escapes the pass.
