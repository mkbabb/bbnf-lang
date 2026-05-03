# W2 Cohort Template Specification

Date: 2026-05-03
Scope: The cohort template parameterisation per gap D of `docs/PHASE-4-DIRECTIVE-2026-05-03.md:114-128`. The 5-grammar cohort (BNF, CSV, EBNF, CSS Pretty, Math) emits from a single codegen template at xtask-regen time. This document specifies the template parameters, per-cohort instantiation, hash-of-template artefact, migration evidence, and template implementation sketch.

## §1 Template parameter table

The template at `crates/core/src/codegen/runtime_template.rs` consumes the following parameters per cohort grammar:

| Parameter | Rust type | Source | Purpose |
|---|---|---|---|
| `grammar_ident` | `&'static str` | `[workspace.metadata.bbnf.grammars.<g>.ident]` | Module name + path component (e.g., `"bnf"`, `"csv"`) |
| `kinds_enum` | `proc_macro2::TokenStream` | layout-lowering pass output (BA→BB.C2 carry) | The `<G>Kind` enum: variants from grammar's leaf categories |
| `value_enum` | `proc_macro2::TokenStream` | layout-lowering pass output | The `<G>Value<'p>` enum: variants per typed-payload, lifetime-parameterised |
| `document_struct` | `proc_macro2::TokenStream` | layout-lowering pass output | The `<G>Document<'p>` struct: arena field + root-value + path-query support |
| `view_struct` | `proc_macro2::TokenStream` | layout-lowering pass output | The `<G>RuntimeView` impl: trait-impl that the cohort path-query consumer reads |
| `parse_fn_signatures` | `Vec<proc_macro2::TokenStream>` | per-rule emission from BA→BB.C1 carry | One signature per top-level rule; uniform `(input: &'i str) -> Result<<G>Value<'i>, ParseErr>` shape |
| `leaf_kinds` | `Vec<&'static str>` | grammar's leaf category enumeration | Drives `kinds_enum` variants and `<G>Value` constructor selection |
| `host_fn_table` | `Vec<HostFn>` (each with `{ name, crate, path }`) | `[workspace.metadata.bbnf.grammars.<g>.host_fns]` | Per-grammar host fns; cohort grammars have empty table by definition |
| `simd_alphabet` | `Vec<u8>` | structural-alphabet miner output (BB.W3b) | Per-grammar dispatch alphabet; cohort grammars' alphabets are small (cardinality ≤ 4) |

The parameter set is closed; new cohort grammars instantiate by adding a workspace metadata entry, not by extending the template source.

## §2 Per-cohort instantiation table

Each cell filled with the actual instantiation value:

| Parameter | BNF | CSV | EBNF | CSS Pretty | Math |
|---|---|---|---|---|---|
| `grammar_ident` | `"bnf"` | `"csv"` | `"ebnf"` | `"css_pretty"` | `"math"` |
| `kinds_enum` cardinality | 8 (Rule, Alt, Seq, Term, NonTerm, Pipe, ListSep, Comment) | 4 (Field, Quoted, Escape, RowSep) | 11 (Rule, Alt, Seq, Repeat, Optional, Group, Term, NonTerm, Pipe, RangeSep, Comment) | 14 (StyleSheet, Rule, AtRule, Decl, Selector, Combinator, Property, Value, Length, Color, Number, Ident, String, Whitespace) | 12 (Expr, BinOp, UnOp, FunCall, NumLit, Identifier, GroupOpen, GroupClose, ListSep, Comma, Whitespace, EndOfInput) |
| `value_enum` cardinality | 6 (Rule, Alt, Seq, Term, NonTerm, Comment) | 3 (Field, Quoted, EscapeChar) | 8 (Rule, Alt, Seq, Repeat, Optional, Group, Term, NonTerm) | 10 (StyleSheet, Rule, AtRule, Decl, Selector, Property, Value, Length, Color, Number) | 9 (Expr, BinOp, UnOp, FunCall, NumLit, Identifier, GroupOpen, GroupClose, EndOfInput) |
| `document_struct` LOC (templated emit) | ~25 | ~25 | ~25 | ~25 | ~25 |
| `view_struct` LOC (templated emit) | ~10 | ~10 | ~10 | ~10 | ~10 |
| `parse_fn_signatures` count (top-level rules) | ~12 | ~5 | ~16 | ~22 | ~14 |
| `leaf_kinds` cardinality | 6 | 4 | 8 | 14 | 12 |
| `host_fn_table` cardinality | 0 | 0 | 0 | 0 | 0 |
| `simd_alphabet` (top-level dispatch bytes) | `[':', ';', '|', '\n']` (4) | `[',', '\n', '"']` (3) | `[':', ';', '|', '{', '}', '[', ']']` (7) | `['{', '}', '[', ']', ':', ';', ',']` (7) | `['(', ')', ',', '+', '-', '*', '/']` (7) |

The cardinality columns establish the cohort's structural similarity. The LOC columns establish the templated emit shape. The `host_fn_table` cardinality 0 is the definitional cohort property: cohort grammars have no per-grammar host fns. The `simd_alphabet` columns feed BB.W3b's structural-alphabet miner; cohort grammars' alphabets are small enough that the SIMD threshold typically routes to scalar emit (BB.W3c cost-model decision).

## §3 Hash-of-template artefact

The template at `crates/core/src/codegen/runtime_template.rs` is hash-anchored: `xtask regen --check` re-emits byte-identical output if template + parameters unchanged.

```sh
# xtask regen --check semantics
xtask_regen_check() {
    for grammar in $(cohort_grammars); do
        templated_output=$(xtask regen --grammar "$grammar" --emit-only)
        committed_output=$(cat "crates/core/src/runtime/$grammar/{document,view,kind,value,mod}.rs")
        if [ "$(echo "$templated_output" | sha256sum)" != "$(echo "$committed_output" | sha256sum)" ]; then
            echo "Regen mismatch on $grammar: hash divergence"
            return 1
        fi
    done
    return 0
}
```

The hash discipline closes BB.W2a M2 (byte-equality precondition) and BB.W2c (regression artefact). The `xtask regen --check` exits 0 iff the cohort templated emission is byte-identical to the committed source.

## §4 Migration evidence

Per `docs/PHASE-4-DIRECTIVE-2026-05-03.md:127`, the migration evidence is the byte-equal diff vs hand-written cohort modules at first commit.

Method:
1. **Pre-W2a state**: 35 hand-written cohort files (5 grammars × 7 files: arena, builder, document, kind, mod, value, view) totalling ~2,265 LOC per `audit/CENSUS-2026-05-03.md:507-528`.
2. **First W2a commit**: the template at `crates/core/src/codegen/runtime_template.rs` lands; xtask emits the templated output to a SHADOW directory at `crates/core/src/runtime/<g>.templated/`; the shadow files diff byte-equal against the committed hand-written files.
3. **W2c gate**: the shadow files replace the committed hand-written files atomically; `git diff --stat HEAD~1..HEAD` shows file deletions + file additions only, NO content drift across the shadow → committed transition.
4. **W2c artefact**: `docs/tranches/BB/audit/W2c-byte-equal-evidence.md` records the diff output, the hash-of-template, and the per-grammar instantiation parameters.

The migration evidence is mechanical: the template's first commit MUST emit byte-identical output to the existing hand-written modules; only after the diff verifies do the hand-written files delete. If the template fails to emit byte-identical output, the template extends to capture the variation rather than the hand-written files persisting as debt. The discipline is the precondition for safe deletion.

## §5 Template implementation sketch

The template at `crates/core/src/codegen/runtime_template.rs` uses `proc_macro2` + `quote` per the proc-macro idiom for templated codegen:

```rust
// crates/core/src/codegen/runtime_template.rs (created at BB.W2a M1)
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use crate::ir::layout::TypedLayout;
use crate::workspace_metadata::CohortGrammarMeta;

pub struct CohortTemplateInputs<'a> {
    pub grammar_ident: &'a str,            // e.g., "bnf"
    pub kinds_enum: TokenStream,           // <G>Kind enum from layout pass
    pub value_enum: TokenStream,           // <G>Value<'p> enum from layout pass
    pub document_struct: TokenStream,      // <G>Document<'p> struct
    pub view_struct: TokenStream,          // <G>RuntimeView impl
    pub parse_fn_signatures: Vec<TokenStream>,  // per-rule fns
    pub leaf_kinds: Vec<&'a str>,
    pub host_fn_table: Vec<HostFn>,
    pub simd_alphabet: Vec<u8>,
}

pub struct CohortTemplateOutputs {
    pub mod_rs: TokenStream,
    pub kind_rs: TokenStream,
    pub value_rs: TokenStream,
    pub view_rs: TokenStream,
    pub document_rs: TokenStream,
}

pub fn emit_cohort(inputs: &CohortTemplateInputs) -> CohortTemplateOutputs {
    let g = format_ident!("{}", inputs.grammar_ident);
    let g_value = format_ident!("{}Value", capitalise(inputs.grammar_ident));
    let g_kind = format_ident!("{}Kind", capitalise(inputs.grammar_ident));
    let g_document = format_ident!("{}Document", capitalise(inputs.grammar_ident));
    
    let kind_rs = inputs.kinds_enum.clone();
    
    let value_rs = quote! {
        use crate::runtime::builder_template::SimpleValue;
        
        #[derive(Clone, Copy, Debug)]
        pub enum #g_value<'p> {
            #(#inputs.value_variants)*
        }
        
        impl<'p> SimpleValue for #g_value<'p> {
            fn deposit_str(s: &'p str) -> Self { Self::String(s) }
            fn deposit_f64(n: f64) -> Self { Self::Number(n) }
        }
    };
    
    let view_rs = inputs.view_struct.clone();
    let document_rs = inputs.document_struct.clone();
    
    let mod_rs = quote! {
        //! #g grammar runtime module.
        //! Templated from `crates/core/src/codegen/runtime_template.rs`.
        
        mod arena;
        mod builder;
        mod document;
        mod kind;
        mod value;
        mod view;
        
        pub use arena::*;
        pub use builder::*;
        pub use document::*;
        pub use kind::*;
        pub use value::*;
        pub use view::*;
    };
    
    CohortTemplateOutputs {
        mod_rs,
        kind_rs,
        value_rs: value_rs.into(),
        view_rs,
        document_rs,
    }
}

fn capitalise(s: &str) -> String { /* ... */ }
```

The template is parameter-pure: every per-grammar value substitutes from `inputs`; no inline branching by grammar name. The `inputs.kinds_enum` and `inputs.value_enum` are themselves `TokenStream`s emitted by the layout-lowering pass (BA→BB.C2 carry); the template merely composes them into the cohort runtime module.

## §6 Workspace metadata schema

Per `[workspace.metadata.bbnf.grammars.<g>]` entries (carry BA→BB.C5):

```toml
[workspace.metadata.bbnf.grammars.bnf]
ident = "bnf"
cohort = true
source_path = "grammar/bnf/bnf.bbnf"
output_dir = "crates/core/src/runtime/bnf"
host_fns = []
simd_alphabet = [":", ";", "|", "\n"]
pratt_eligibility = "skip"  # cohort grammars do not declare operator chains
simd_eligibility = "auto"   # cost model decides

[workspace.metadata.bbnf.grammars.csv]
ident = "csv"
cohort = true
source_path = "grammar/csv/csv.bbnf"
output_dir = "crates/core/src/runtime/csv"
host_fns = []
simd_alphabet = [",", "\n", "\""]
pratt_eligibility = "skip"
simd_eligibility = "auto"

# ... ebnf, css_pretty, math identical shape
```

The cohort = true flag is the classifier; xtask regen routes cohort grammars through `runtime_template.rs` and specialised grammars through their hand-written modules (BBNF, CSS L4, Sheets, JSON).

## §7 Validation rules

| Rule | Verification |
|---|---|
| Each cohort grammar has `cohort = true` in workspace metadata | `xtask regen --validate` reads each `[workspace.metadata.bbnf.grammars.<g>]` and asserts that the `cohort` flag is consistent with the runtime/<g>/ directory shape |
| Each cohort grammar's `host_fns = []` (empty table) | `xtask regen --validate` rejects cohort grammars with non-empty host_fns; cohort grammars are by definition host-fn-free |
| Each cohort grammar's `simd_alphabet` cardinality ≤ 8 | beyond cardinality 8, the grammar is not cohort-shaped; specialised treatment required |
| The template emits byte-identical output for each cohort grammar pre/post-W2a | BB.W2c `W2c-byte-equal-evidence.md` records the diff |
| The hand-written cohort modules delete only after byte-equality verification | `git log --diff-filter=D` shows deletions only after the W2c gate passes |

## §8 Migration plan from current static enumeration

The current `crates/core/src/grammar/host.rs:387` and similar sites enumerate cohort grammars by static array (e.g., `const COHORT_GRAMMARS: &[&str] = &["bnf", "csv", "ebnf", "css_pretty", "math"]`). Per surgery 25 + G05-2 of `audit/HARDENING-PLAN-2026-05-03-05-grammar-authoritative.md:25`, the migration:

1. **BA.W1.M4** lands the workspace metadata schema (per `docs/tranches/BA/audit/W1-workspace-metadata-schema.md`).
2. **BA.W1.M2** deletes the static `GrammarAuditTag::{Json,CssL4,Sheets,Bbnf}` arms.
3. **BB.W2a M1** reads the workspace metadata via `cargo_metadata` crate; the `cohort = true` flag classifies; the static array is gone.
4. **BB.W2a M5** the xtask regen pipeline picks up the cohort flag at startup; the cohort enumeration is metadata-driven, not source-hardcoded.

Future cohort grammars (e.g., a sixth cohort grammar joining the family) instantiate by adding a workspace metadata entry; no source-code change to the codegen substrate.
