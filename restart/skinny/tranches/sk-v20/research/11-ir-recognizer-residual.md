# IR Recognizer Residual Audit — Post-W6 at HEAD f4633d4

Scope: `crates/ir/src/passes/recognizers/` (shape_dict_bbnf.rs, grammar_facts.rs,
shape_dispatch/scalar.rs), `crates/ir/src/registry/strategy.rs`, `crates/ir/build.rs`,
`[workspace.metadata.bbnf-strategy]` in the root `Cargo.toml`, and
`crates/ir/tests/lock14_grammar_generalization.rs`.

---

## 1. Lock-14 Token Scan — Narrow but Correct

`lock14_grammar_generalization.rs:26–44` defines a 17-token roster covering every
parser-struct ident the nine production grammars advertise (`JsonParser`, `JsonGrammar`,
`CssL4Parser`, `BbnfBootstrap`, …). The scanner excludes doc-comment lines and passes
cleanly; the re-inject falsifier at line 131–148 confirms the gate is load-bearing.

No grammar-name token appears in code lines in `crates/ir/src/` or
`crates/analysis/src/` at this HEAD. Lock-14 is nominally satisfied.

However, the gate is narrower than it appears:

- **The roster is `&[&'static str]` compiled directly into the test file at lines
  26–44.** A grammar whose ident does not match the `…Parser` / `…Grammar`
  convention — say `"BnfInterpreter"` or `"SheetsParser"` (an alias variant not
  yet landed) — would not be caught. The gate is a roster-match, not a
  name-pattern match. A new grammar whose `idents` slice includes a novel stem
  must also extend the test roster; that step is manual and failure-silent if
  omitted.

- The gate scans only the `src/` tree. `build.rs` and inline `include!`-d generated
  sources are outside the scan. `build.rs` reads workspace TOML directly but emits
  no grammar names into IR source files; this is benign today but not closed by the
  gate.

---

## 2. `shape_dict_bbnf.rs` — Structurally Keyed, Not Name-Laundered

The file's two recognizers both key on concrete-syntax literals, not rule names:

- `is_big_comment_shape` (`shape_dict_bbnf.rs:116`): admits `Seq[Literal("/*"),
  Regex(_), Literal("*/")]` after peeling `Map`/`OptionalWhitespace` wrappers.
  The literals `/*` and `*/` are the universal block-comment syntax in most grammars;
  they are not BBNF-rule tokens.

- `is_mapped_factor_empty_shape` (`shape_dict_bbnf.rs:146`): admits
  `Seq[Ref(_), Repeat(0,1){ leads with Literal("->") }]`. The arrow `->` is the
  BBNF map operator — this is a grammar-language token, not a user-defined rule name.

`crates/ir/tests/shape_dict_bbnf.rs` proves both admissions are name-independent
(rules named `"totally_unrelated_rule"` still match), and that structurally similar
shapes with wrong delimiters (`<<`/`>>`, `,`) do not match.

**However, the recognizer module is named `shape_dict_bbnf`.** The `Bbnf` suffix in
the module name, the type names `BbnfShapeTemplate` and `BbnfShapeKind`, the enum
variants `BigComment` and `MappedFactorEmpty`, and the field doc-comment at
`profile.rs:107` ("Empty for non-BBNF grammars") together encode the assumption that
only the BBNF self-host grammar will ever have rules matching these shapes. That
assumption has two exposures:

1. **`/*` `*/` block comments are not BBNF-exclusive.** Any grammar that adds a C-style
   block comment rule — `Seq[Literal("/*"), Regex, Literal("*/")]` — will silently
   receive a `BigComment` template. Since the emitter currently discards
   `bbnf_shape_templates` (see §5 below), this is harmless today, but it confounds
   the grammar-generality invariant at the point where the field is eventually wired.

2. **The map arrow `->` is a BBNF metalanguage token**, not a user-grammar token.
   Every grammar that uses BBNF's `-> type_annotation` map projection at the
   `mapped_factor` structural level will match `is_mapped_factor_empty_shape`. In the
   current corpus only the BBNF self-host grammar contains a rule whose body is
   literally `Seq[Ref, Repeat(0,1){ Seq[Literal("->"), …] }]` because only the BBNF
   grammar parses BBNF source. But the claim "structural not name-based" requires
   that the match remain inert when hit by an unexpected grammar. Today it does not:
   any grammar with that shape silently produces a `MappedFactorEmpty` template.

The honest framing is: these recognizers are structurally keyed on BBNF metalanguage
tokens (`/*`, `*/`, `->`), not on user-defined rule names. That is meaningfully
different from name-keying, but it is not fully grammar-agnostic. The module name
`shape_dict_bbnf` correctly signals the scope.

---

## 3. `grammar_facts.rs` — `infer_associativity` Hardcodes `"^"` as Right-Associative

`grammar_facts.rs:1517–1522`:

```rust
fn infer_associativity(literal: &str) -> Associativity {
    match literal {
        "^" => Associativity::Right,
        _ => Associativity::Left,
    }
}
```

The doc-comment at line 1515 explicitly names Sheets: "Sheets `^` is the only
right-associative operator in the target corpus." This is a direct corpus assumption
baked into generic IR code. Any grammar whose `^` operator is intended to be
left-associative (e.g. a hypothetical bitwise-XOR grammar) will have the wrong
associativity inferred. Any grammar whose `**` (exponentiation) or `->` (right-arrow
in an expression language) should be right-associative will not be detected.

This function is called from three sites in the operator-chain extraction path
(`grammar_facts.rs:1123, 1165, 1420`). The `Associativity::Right` variant exists in
the emitted precedence LUT packing (`crates/core/src/backend/rust/emitter/
precedence.rs:73`) and is tested at `crates/core/tests/pratt_const_fold.rs:197`.

The correct transposition is an `@assoc` or `@right` directive in the grammar source,
resolved through `IrRule::meta` or a hint-map. Until that directive exists, the
function should default to `Left` with no special cases — no hardcoded grammar-corpus
knowledge belongs in the generic IR crate.

---

## 4. `CounterOptional::Nested` — BBNF Exemplar in a Generic Enum

`grammar_facts.rs:276–285`:

```rust
pub enum CounterOptional {
    /// body is itself possibly empty. BBNF `mapped_factor`: …
    Nested,
    /// carrying a flag across the dispatch (CSS `alphaSep?` in …
    Lookahead,
}
```

Both variants name specific production grammar rules in their doc-comments.
`Nested` is described in terms of BBNF `mapped_factor`; `Lookahead` in terms of
CSS `alphaSep?`.

More critically: `CounterOptional::Lookahead` is declared but never assigned anywhere
in the codebase. `grep -rn "CounterOptional::Lookahead"` returns no hits across the
entire workspace. The `detect_counter_optional` function at line 873 only ever
produces `Some(CounterOptional::Nested)` or `None`. The `Lookahead` variant is dead
code — an unfired reserve variant that exists only as documentation of a known future
case (CSS `alphaSep?`). Dead variants in a generic public enum are a specification
debt; they will be hit in match exhaustion if another arm is added, and they silently
do not fire even if a grammar meets the described structural condition.

The `counter_optional_rules` map is populated in `grammar_facts.rs` but consumed
nowhere outside `crates/ir`. Neither `crates/core/src/` nor any other crate calls
into the `counter_optional_rules` field of `DtaTable`. The fact is produced but not
lowered. This is either a deferred wiring (the emitter does not yet emit the
counter-DFA path) or a dead accumulation.

---

## 5. `bbnf_shape_templates` — Mined, Stored, Not Emitted

`GrammarProfile::bbnf_shape_templates` (`profile.rs:108`) is populated by
`mine_bbnf_shape_templates` on every `ir.profile()` call. The field is `pub`. The
profile is passed to `emit_grammar_profile` (`core/src/backend/rust/emitter/
profile.rs:23`), which only emits structural-alphabet constants and does not read
`bbnf_shape_templates` at all. No production code in `crates/core/src/` reads
`bbnf_shape_templates`. The field is also absent from the `emit_grammar_profile`
output TokenStream.

This means the shape-template miner runs, produces records for every compiled BBNF
grammar pass, writes them into `GrammarProfile`, and they are then dropped silently.
The module doc at `shape_dict_bbnf.rs:21` says "templates feed into
`GrammarProfile::shape_dict` for the emitter to lower" — but there is no `shape_dict`
field on `GrammarProfile` (the actual field is `bbnf_shape_templates`), and no
emitter path exists that reads it. The `ShapeRef` tape variant referenced in the
file's top-level doc-comment (lines 8–9) is not visible anywhere in `crates/core`.
This is a designed-but-not-wired V5 feature (`AV.5.6`): the miner, struct field, and
tests exist; the emitter wire does not.

The stale doc reference at `profile.rs:34` — `crate::passes::recognizers::shape_dict
::ShapeDictMap` — points to a module that does not exist; the actual module is
`shape_dict_bbnf`. This is a broken intra-doc link.

---

## 6. `build.rs` + `[workspace.metadata.bbnf-strategy]` — Design Correct, Schema Incomplete

The `build.rs` / `strategy.rs` / workspace-metadata approach is the right home for
the manifest. The two-table split (`[workspace.metadata.bbnf]` for grammar paths and
features; `[workspace.metadata.bbnf-strategy]` for codegen substrate bindings) keeps
the generic `bbnf-ir` crate clean.

A 10th grammar author must add two entries: one row to `[workspace.metadata.bbnf]`
(path, features) and one row to `[workspace.metadata.bbnf-strategy]` (idents,
`rust_builder_path`, `rust_document_path`). The `build.rs` enforces that every entry
in the strategy table has both path fields, but it does not cross-validate that every
grammar in the `bbnf` table also has a row in the `bbnf-strategy` table. A grammar
that is registered but has no strategy row will compile silently and then panic at
`EmitStrategy::for_grammar_with_manifest` at codegen time.

The `ManifestStrategyEntry` struct (`strategy.rs:264`) has only Rust-backend fields
(`rust_builder_path`, `rust_document_path`). TS and WASM fields do not exist in the
struct, only as `Option<SubstrateBinding>` fields in the `EmitStrategy::StructDirect`
variant (`strategy.rs:114, 117`). The comment at `strategy.rs:261` says "W1.5/W1.8
extends the row with `ts`/`wasm` substrate bindings" — but `ManifestStrategyEntry`
has not been extended and the workspace TOML rows have no `ts_builder_path` or
`wasm_builder_path` keys. When the BA wave lands, adding TS/WASM fields requires
changing the struct, the `build.rs` TOML reader, and every row in the workspace TOML.
The schema gap is acknowledged deferred scope, not an architectural defect.

The comment at `strategy.rs:228` in the panic message still says "add a row to
`PRODUCTION_MANIFEST_TABLE` in `crates/ir/src/registry/strategy.rs`" — but after
W1.8 the table is generated from workspace TOML, not edited in source. The panic
message directs a contributor to the wrong file.

---

## 7. `scalar.rs` — Genuinely Structural

`shape_dispatch/scalar.rs:38–51` is clean. The two admission cases — `IrNode::Literal`
and `IrNode::Ref(rid)` where `ir.shape_assignments.get(*rid).is_classified()` — are
fully grammar-agnostic structural checks. The note at line 11 mentioning "JSON's
`comma`" and "`colon`" is doc-comment illustration only; the predicate itself contains
no grammar-name reference.

---

## 8. Contradictory Test Documentation — `core/tests/shape_dict_bbnf.rs`

`crates/core/tests/shape_dict_bbnf.rs:14–16` states:

> "the miner is name-gated on the canonical BBNF rule names and produces an empty list
> elsewhere"

This is factually wrong. `mine_bbnf_shape_templates` at `shape_dict_bbnf.rs:75` iterates
every rule in the IR and applies structural predicates; it is not name-gated. The
companion test suite in `crates/ir/tests/shape_dict_bbnf.rs` explicitly proves this with a
test named `big_comment_recognized_by_delimiters_not_name` (line 76) that names the rule
`"totally_unrelated_rule"` and confirms a template is still produced. The two test file
doc-comments give contradictory accounts of the same function.

The correct framing: the miner fires on structural shape (specific concrete-syntax literals
as structural keys) and produces no templates for JSON/CSS because those grammars have no
`Seq[Literal("/*"), Regex, Literal("*/")]` or `Seq[Ref, Repeat(0,1){leads with "->"}]`
rule bodies — not because of a name check. The `core/tests` comment misleads future readers
into thinking the gate is by name when it is by shape.

---

## Summary of Findings

| Issue | Location | Severity |
|---|---|---|
| Lock-14 roster is static; novel ident stems miss the gate | `lock14_grammar_generalization.rs:26–44` | Medium |
| `infer_associativity("^")` hardcodes Sheets corpus knowledge in generic IR | `grammar_facts.rs:1517–1522` | High |
| `CounterOptional::Lookahead` is dead code (never assigned) | `grammar_facts.rs:284` | Medium |
| `counter_optional_rules` produced but not lowered by any emitter | `grammar_facts.rs:370` | Medium |
| `bbnf_shape_templates` mined but never emitted; `emit_grammar_profile` silently drops the field | `profile.rs:108, 163`; `profile.rs (emitter):23` | Medium |
| `core/tests/shape_dict_bbnf.rs` doc says "name-gated" — contradicts the `ir/tests` structural proof | `core/tests/shape_dict_bbnf.rs:14–16` vs `ir/tests/shape_dict_bbnf.rs:76–87` | Low |
| Stale broken intra-doc link: `shape_dict::ShapeDictMap` → `shape_dict_bbnf` | `profile.rs:34` | Low |
| Panic message in `for_grammar_with_manifest` directs to wrong file | `strategy.rs:228` | Low |
| `ManifestStrategyEntry` lacks `ts_builder_path`/`wasm_builder_path`; schema gap for BA wave | `strategy.rs:264` | Deferred |
| No cross-validation that `bbnf` table grammars each have a `bbnf-strategy` row | `build.rs + Cargo.toml` | Low |

---

## Recommendations

**`infer_associativity`**: Delete the `"^"` arm. Associativity is a grammar-declared
property; the correct path is an `@assoc right` directive on the operator rule,
resolved through `IrRule::meta` or a grammar hint. Until that directive exists, the
function should default to `Associativity::Left` with no special cases.

**`CounterOptional::Lookahead`**: Either wire the detector (write a structural predicate
for CSS `alphaSep?`-like shapes and assign `Lookahead` where appropriate) or delete
the variant. Dead variants in public generic enums invite incorrect match-arm reasoning
downstream. The present state — documented but inert — violates the no-stubs rule.

**`bbnf_shape_templates` dead field**: Either wire `emit_grammar_profile` to emit the
`ShapeRef` tape path for rules matching the templates, or delete the miner, the struct
field, and the tests until that wave lands. A feature that mines into a struct field
that no production code reads is a stub.

**Panic message**: At `strategy.rs:228`, update the message to say "add a row to
`[workspace.metadata.bbnf-strategy]` in the workspace `Cargo.toml`", not the old
source-file path.

**Lock-14 gate hardening**: Change the roster to a pattern-based check (any token
ending in `Parser`, `Grammar`, `Bootstrap`, or matching any `idents` entry in
`PRODUCTION_MANIFEST_TABLE`) so the gate does not require manual extension for each
new grammar alias.
