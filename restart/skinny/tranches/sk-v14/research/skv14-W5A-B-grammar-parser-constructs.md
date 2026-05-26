# SK-V14 W5A-B: Grammar Parser Constructs

Date: 2026-05-26.
Scope: W5A source-consuming runtime generator research, focused on the current skinny grammar parser versus CSS L4 V1 grammar-source constructs.
Output: `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md`.

## Section 1 - Findings

W5A is explicitly the source-consuming runtime generator capability wave. The owner paths include `skinny/crates/grammar/src/lib.rs`, codegen source/metadata routing, `regen-css`, and a temporary provider/template deletion guard; the entry gate requires the plan to name the grammar-neutral source plus workspace metadata contract and the required V1 grammar constructs; the tasks require grammar source plus workspace metadata to enter codegen and require those constructs to parse without `grammar_id == css_l4` branching (`restart/skinny/tranches/sk-v14/SPEC.md:641`, `restart/skinny/tranches/sk-v14/SPEC.md:657`, `restart/skinny/tranches/sk-v14/SPEC.md:662`, `restart/skinny/tranches/sk-v14/SPEC.md:663`). Provider/template deletion remains W5B-only (`restart/skinny/tranches/sk-v14/SPEC.md:678`, `restart/skinny/tranches/sk-v14/SPEC.md:686`).

The dispatch contract makes W5A sequencing load-bearing: provider/template deletion is forbidden before W5A admits all seven CSS companion checks plus JSON/Sheets/BBNF-self proof (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:139`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:143`). W5A is mandatory challenge-class substrate work (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:226`), and CSS L4 rows cannot admit without grammar-derived emission and production corpus comparator coverage (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:348`).

REDRESS-209 is still accurate at HEAD for the parser/generator gap. It records that `regen-css` emits via `codegen::emit_runtime_profile(target.profile)`, hashes CSS source and metadata only for freshness, and the skinny grammar parser rejects CSS L4 `->` projections and `@{...}` span capture (`skinny/REDRESS.md:5173`, `skinny/REDRESS.md:5178`, `skinny/REDRESS.md:5180`, `skinny/REDRESS.md:5181`). The earlier W5 redress packet adds the same three-part blocker: static provider/template mesh, freshness-only source hashing, and inability to parse CSS L4 source (`restart/skinny/tranches/sk-v14/research/skv14-W5-redress.md:16`, `restart/skinny/tranches/sk-v14/research/skv14-W5-redress.md:23`, `restart/skinny/tranches/sk-v14/research/skv14-W5-redress.md:25`).

The current skinny parser is much narrower than the V1 grammar syntax. It accepts only top-level `@import` and `@token` directives and discards their payloads (`skinny/crates/grammar/src/lib.rs:80`, `skinny/crates/grammar/src/lib.rs:91`). Its sequence parser has no comma separator (`skinny/crates/grammar/src/lib.rs:130`), postfix support is only `*`, `?`, and `+` (`skinny/crates/grammar/src/lib.rs:153`), atoms are only double-quoted literals, regexes, parenthesized expressions, and refs (`skinny/crates/grammar/src/lib.rs:196`), and unexpected `@`, `,`, `>`, and `-` tokens fail directly (`skinny/crates/grammar/src/lib.rs:230`). The tests intentionally verify non-skinny directive rejection (`skinny/crates/grammar/src/lib.rs:407`).

The IR also lacks source-level nodes for imports, `?w`, discard operators, map projections, and span capture. `ExprKind` has `Seq`, `Alt`, `Repeat`, `Optional`, `Literal`, `Regex`, `Ref`, and `Annotation` only (`skinny/crates/ir/src/lib.rs:210`). The backend does contain `ValueProject` and `SpanMark` shapes (`skinny/crates/ir/src/lib.rs:374`, `skinny/crates/ir/src/lib.rs:385`), but current source parsing never creates the source constructs that would lower to them, and existing pass lowering drops `ExprKind::Annotation` as an empty sequence (`skinny/crates/passes/src/lib.rs:1257`).

The source contract is present in name only. `codegen::emit_from_source` parses source and compiles it (`skinny/crates/codegen/src/lib.rs:98`), but `regen-css` reaches `codegen::emit_runtime_profile(target.profile)` through `skinny/xtask/src/regen.rs:18` and then static profile rendering. Runtime rendering matches per-profile providers for all seven CSS L4 profiles (`skinny/crates/codegen/src/lib.rs:117`, `skinny/crates/codegen/src/lib.rs:162`, `skinny/crates/codegen/src/lib.rs:166`). The CSS source and workspace metadata lists are in `RuntimeTarget`, but `validate_inputs` hashes them and does not pass bytes into codegen (`skinny/xtask/src/regen.rs:5`, `skinny/xtask/src/regen.rs:14`, `skinny/xtask/src/regen.rs:61`, `skinny/xtask/src/regen.rs:72`). `regen-css` lists all fifteen CSS L4 source files and two metadata files for every profile (`skinny/xtask/src/regen_css.rs:5`, `skinny/xtask/src/regen_css.rs:23`, `skinny/xtask/src/regen_css.rs:25`).

The V1 grammar syntax that CSS L4 uses is documented by the BBNF self grammar: `@{ rhs }` terms, `?w` modifiers, mapped factors using `"->"`, `<<` and `>>` binary operators, comma concatenation, import directives, `@ws`, `@token`, `@pretty`, and `@host` directives (`grammar/bbnf/bbnf.bbnf:33`, `grammar/bbnf/bbnf.bbnf:38`, `grammar/bbnf/bbnf.bbnf:42`, `grammar/bbnf/bbnf.bbnf:44`, `grammar/bbnf/bbnf.bbnf:47`, `grammar/bbnf/bbnf.bbnf:60`, `grammar/bbnf/bbnf.bbnf:68`, `grammar/bbnf/bbnf.bbnf:70`, `grammar/bbnf/bbnf.bbnf:71`, `grammar/bbnf/bbnf.bbnf:73`). Value expressions and type annotations on the right side of `->` include integers/floats, bools, `input`, paths, function calls, closures, and `: type_name` (`grammar/bbnf/expressions.bbnf:1`, `grammar/bbnf/expressions.bbnf:6`, `grammar/bbnf/expressions.bbnf:13`, `grammar/bbnf/expressions.bbnf:14`, `grammar/bbnf/expressions.bbnf:33`, `grammar/bbnf/expressions.bbnf:37`, `grammar/bbnf/types.bbnf:3`).

Constructs that fail or are semantically absent for CSS L4 runtime generation at HEAD:

| Construct | CSS L4 evidence | Current failure | Smallest W5A support surface |
|---|---|---|---|
| `@import` graph | `values.bbnf` imports seven files (`grammar/css/l4/values.bbnf:1`), selectors import tokens (`grammar/css/l4/selectors.bbnf:7`), stylesheet imports properties/selectors/media (`grammar/css/l4/stylesheet.bbnf:1`) | Top-level parser accepts `@import` but discards path/items, so imported refs are unresolved. Probe: `ERR import_unresolved: unresolved rule reference ident`. | Add a source-loader/import graph that records canonical paths, follows relative imports, de-dupes by canonical path, supports `@import "path"` now, and either supports `@import { items } from "path"` or fail-closes it with a named unsupported construct for BBNF-self. |
| `@token` | active in stylesheet (`grammar/css/l4/stylesheet.bbnf:13`); token rules live in `tokens.bbnf` (`grammar/css/l4/tokens.bbnf:7`) | Parser accepts but ignores. It provides no token boundary metadata and no generated-role witness. | Parse into grammar metadata. It can be non-semantic for W5A only if generator emits an explicit "token directive observed" witness and does not claim token-aware runtime behavior from it. |
| `@ws` and `@pretty` | stylesheet uses `@ws` for CSS comment-aware whitespace (`grammar/css/l4/stylesheet.bbnf:12`) and eight `@pretty` directives (`grammar/css/l4/stylesheet.bbnf:53`) | `@ws` and `@pretty` are rejected as non-skinny directives. Probe: `BBNF-DIRECTIVE-NOT-IN-SKINNY`. | `@ws` must become runtime metadata because `?w` depends on it. `@pretty` can parse into ignored runtime metadata, preserving a witness that it was seen and intentionally not used by runtime generation. |
| comma concatenation | dimensions and many expressions use comma sequence syntax (`grammar/css/l4/value-unit.bbnf:62`, `grammar/css/l4/selectors.bbnf:13`, `grammar/css/l4/values.bbnf:49`) | Parser treats comma as unexpected. Probe: `ERR comma_seq: BBNF-PARSE: unexpected token ,`. | Treat comma as an optional sequence separator in expression parsing, equivalent to sequence adjacency for recognition, while preserving source spans. |
| `?w` whitespace modifier | stylesheet and selector grammar are dense with `?w` (`grammar/css/l4/stylesheet.bbnf:15`, `grammar/css/l4/stylesheet.bbnf:29`, `grammar/css/l4/selectors.bbnf:39`) | Parser consumes `?` as optional and then sees `w` as a rule ref. Probe: `ERR whitespace_modifier: unresolved rule reference w`. | Add a distinct `?w` postfix before `?` matching. Represent it as a whitespace/trivia postfix tied to the current `@ws` policy, or lower to a synthetic hidden `__ws` rule with no materialized output. |
| `>>` and `<<` discard/keep operators | math and function bodies use them (`grammar/css/l4/values.bbnf:47`, `grammar/css/l4/values.bbnf:52`), selectors use them for pseudo functions (`grammar/css/l4/selectors.bbnf:54`), colors use them inside captures (`grammar/css/l4/color.bbnf:222`) | Parser stops after the left atom and errors on `>`. Probe: `ERR discard_ops: BBNF-PARSE: unexpected token >`. | Parse binary operators after mapped factors. For W5A, recognition can lower them to sequence plus capture/materialization policy; the policy must be present so W6/root-runtime work is not silently lost. |
| `->` value projection | numeric/scalar projections (`grammar/css/l4/value-unit.bbnf:15`), keyword discriminants (`grammar/css/l4/values.bbnf:37`), selector combinator tags (`grammar/css/l4/selectors.bbnf:93`), color host/type projections (`grammar/css/l4/color.bbnf:189`) | Parser errors on `-`. Probe: `ERR projection_u8: BBNF-PARSE: unexpected token -`; all non-token CSS files hit this or an earlier comma/directive failure. | Add `MappedFactor { body, value_expr_raw, type_annotation }` or equivalent. W5A needs constants (`0u8`, hex `u32`), scalar decodes (`f64`, `i64`, `Span`), bool literals, `input : Type`, and host-call metadata as parseable nodes. Unsupported host execution can fail closed later, but parsing must name the projection. |
| `@{...}` span capture | URL capture (`grammar/css/l4/values.bbnf:67`, `grammar/css/l4/values.bbnf:69`) and color aggregate capture (`grammar/css/l4/color.bbnf:220`, `grammar/css/l4/color.bbnf:228`) | Parser errors on `@`. Probe: `ERR span_capture: BBNF-PARSE: unexpected token @`. | Add `SpanCapture(body)` source node and lower to span start/end metadata. Existing backend `SpanMark` can be the generation target, but the source AST needs a capture wrapper. |
| value expressions and type annotations after `->` | CSS color uses host decode and aggregate type annotations (`grammar/css/l4/color.bbnf:189`, `grammar/css/l4/color.bbnf:190`, `grammar/css/l4/color.bbnf:253`, `grammar/css/l4/color.bbnf:255`) | Blocked by `->` today; once `->` parses, the current parser still has no value-expression parser. | Initially preserve a balanced raw value expression plus parsed optional type name. Decode only the small scalar/constant subset needed by generated-role witnesses; fail closed on unimplemented host calls by construct name if W5A does not emit them. |

One important nuance: `grammar/css/l4/tokens.bbnf` parses by itself because it uses only refs/literals/regexes/alternation (`grammar/css/l4/tokens.bbnf:7`). That does not mean import/token support exists. It means the simplest leaf file avoids the unsupported constructs.

## Section 2 - Executed Commands And Results

HEAD and dirty-state check:

```sh
git rev-parse --short HEAD && git status --short
```

Result:

```text
66d15dfb5
 M restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json
 M restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json
 M restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json
 M restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json
 M restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json
 M restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json
 M restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json
?? restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md
```

Existing parser rejection test:

```sh
cargo test --manifest-path skinny/Cargo.toml -p grammar rejects_non_skinny_directives -- --nocapture
```

Result:

```text
running 1 test
test tests::rejects_non_skinny_directives ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 3 filtered out; finished in 0.00s
```

CSS L4 raw construct census:

```sh
for p in '@import' '->' '@{' '>>' '<<' '?w' ',' '@token'; do
  printf '%-8s ' "$p"
  rg -n -F -g '*.bbnf' -- "$p" grammar/css/l4 | wc -l | tr -d ' '
done
```

Result:

```text
@import  25
->       454
@{       3
>>       51
<<       39
?w       92
,        223
@token   2
```

Active directive/capture locations:

```sh
rg -n -g '*.bbnf' '^@import|^@token|@\{' grammar/css/l4
```

Result excerpt:

```text
grammar/css/l4/values.bbnf:1:@import "tokens.bbnf" ;
grammar/css/l4/values.bbnf:69:urlFunction = @{ "url" , "(" >> (string | /[^)"'\s]+/) << ")" } ;
grammar/css/l4/selectors.bbnf:7:@import "tokens.bbnf" ;
grammar/css/l4/stylesheet.bbnf:13:@token ident ;
grammar/css/l4/color.bbnf:220:colorFunction = @{
```

Parser snippet/file probe. This compiled a temporary binary from stdin and only read repo files:

```sh
GRAMMAR_RLIB=$(ls skinny/target/debug/deps/libgrammar-*.rlib | tail -1)
rustc --edition=2021 -L dependency=skinny/target/debug/deps --extern grammar="$GRAMMAR_RLIB" -o /tmp/skv14_parser_probe - <<'RS'
use grammar::parse_grammar;
fn probe(label: &str, source: &str) {
    match parse_grammar(label, source) {
        Ok(grammar) => println!("OK  {label}: rules={} exprs={}", grammar.rules.len(), grammar.exprs.len()),
        Err(err) => println!("ERR {label}: {err}"),
    }
}
fn main() {
    for (label, source) in [
        ("skinny_ok", "a = \"x\" ;"),
        ("comma_seq", "a = \"x\" , \"y\" ;"),
        ("discard_ops", "a = \"(\" >> \"x\" << \")\" ;"),
        ("whitespace_modifier", "a = \"x\" ?w ;"),
        ("projection_u8", "a = \"x\" -> 0u8 ;"),
        ("projection_f64", "a = /[0-9]+/ -> f64 ;"),
        ("span_capture", "a = @{ \"url\" } ;"),
        ("import_unresolved", "@import \"tokens.bbnf\" ;\na = ident ;"),
        ("ws_directive", "@ws /\\s*/ ;\na = \"x\" ;"),
        ("pretty_directive", "@pretty a group ;\na = \"x\" ;"),
    ] {
        probe(label, source);
    }
    for file in [
        "grammar/css/l4/tokens.bbnf",
        "grammar/css/l4/value-unit.bbnf",
        "grammar/css/l4/values.bbnf",
        "grammar/css/l4/stylesheet.bbnf",
    ] {
        let source = std::fs::read_to_string(file).expect(file);
        probe(file, &source);
    }
}
RS
/tmp/skv14_parser_probe
```

Result:

```text
OK  skinny_ok: rules=1 exprs=1
ERR comma_seq: BBNF-PARSE: unexpected token `,` at byte 8
ERR discard_ops: BBNF-PARSE: unexpected token `>` at byte 8
ERR whitespace_modifier: unresolved rule reference `w`
ERR projection_u8: BBNF-PARSE: unexpected token `-` at byte 8
ERR projection_f64: BBNF-PARSE: unexpected token `-` at byte 13
ERR span_capture: BBNF-PARSE: unexpected token `@` at byte 4
ERR import_unresolved: unresolved rule reference `ident`
ERR ws_directive: BBNF-DIRECTIVE-NOT-IN-SKINNY: directive @ws is not available in the skinny compiler at byte 0
ERR pretty_directive: BBNF-DIRECTIVE-NOT-IN-SKINNY: directive @pretty is not available in the skinny compiler at byte 0
OK  grammar/css/l4/tokens.bbnf: rules=3 exprs=5
ERR grammar/css/l4/value-unit.bbnf: BBNF-PARSE: unexpected token `-` at byte 763
ERR grammar/css/l4/values.bbnf: BBNF-PARSE: unexpected token `-` at byte 1362
ERR grammar/css/l4/stylesheet.bbnf: BBNF-DIRECTIVE-NOT-IN-SKINNY: directive @ws is not available in the skinny compiler at byte 559
```

All CSS L4 files through `grammar::parse_grammar`:

```text
ERR grammar/css/l4/color.bbnf: BBNF-PARSE: unexpected token `-` at byte 1154
ERR grammar/css/l4/easing.bbnf: BBNF-PARSE: unexpected token `,` at byte 409
ERR grammar/css/l4/filters.bbnf: BBNF-PARSE: unexpected token `-` at byte 502
ERR grammar/css/l4/func-body.bbnf: BBNF-PARSE: unexpected token `,` at byte 460
ERR grammar/css/l4/gradients.bbnf: BBNF-PARSE: unexpected token `-` at byte 513
ERR grammar/css/l4/keyframes.bbnf: BBNF-PARSE: unexpected token `,` at byte 680
ERR grammar/css/l4/keywords.bbnf: BBNF-PARSE: unexpected token `-` at byte 217
ERR grammar/css/l4/media.bbnf: BBNF-PARSE: unexpected token `-` at byte 430
ERR grammar/css/l4/properties.bbnf: BBNF-PARSE: unexpected token `,` at byte 841
ERR grammar/css/l4/selectors.bbnf: BBNF-PARSE: unexpected token `,` at byte 359
ERR grammar/css/l4/stylesheet.bbnf: BBNF-DIRECTIVE-NOT-IN-SKINNY: directive @ws is not available in the skinny compiler at byte 559
OK  grammar/css/l4/tokens.bbnf: rules=3 exprs=5
ERR grammar/css/l4/transforms.bbnf: BBNF-PARSE: unexpected token `-` at byte 556
ERR grammar/css/l4/value-unit.bbnf: BBNF-PARSE: unexpected token `-` at byte 763
ERR grammar/css/l4/values.bbnf: BBNF-PARSE: unexpected token `-` at byte 1362
```

## Section 3 - Recommended Plan Inputs

Use a source-consuming contract shaped around source roots, import closure, and workspace metadata, not just a profile string:

```text
RuntimeGenerationInput {
  profile_id,
  grammar_name,
  entry_rule,
  source_roots,
  source_roster,
  workspace_metadata,
  generated_file_roster,
  runtime_roles
}
```

The W5A parser surface should be grammar-neutral and CSS-capable:

1. Add a V1 source parser/loader in `skinny/crates/grammar` that handles import graphs, directive metadata, comma concatenation, `?w`, `>>`, `<<`, `@{...}`, and mapped factors with raw value-expression/type metadata.
2. Keep import loading path-based and profile-agnostic. Avoid concatenating all fifteen CSS files blindly; `regen_css.rs` currently lists all fifteen files as freshness inputs for every profile (`skinny/xtask/src/regen_css.rs:5`), but the parser should load the dependency closure from each profile's declared root(s) to avoid duplicate rule collisions.
3. Parse `@ws` as runtime whitespace metadata and `@pretty` as ignored runtime metadata with an explicit witness. `@token` should be stored as metadata, even if W5A does not yet consume it for scanner specialization.
4. Represent `->` projections and `@{...}` captures in the source AST even if W5A only emits generated-role witnesses for some projection kinds. Do not erase them into comments or annotations; that would recreate REDRESS-209's "static centralization" problem.
5. Wire `regen-css` to pass `RuntimeGenerationInput` into codegen before static providers are deleted. The call boundary must not be `emit_runtime_profile(profile)` for CSS profiles after W5A.

Recommended W5A gates:

- `cargo test --manifest-path skinny/Cargo.toml -p grammar` includes table tests for the snippet failures above.
- A CSS L4 import-closure test parses the source roots for all seven CSS profiles and asserts all required constructs are represented without `css_l4` grammar-name branches.
- `cargo xtask regen-css` and all seven `check-css-l4-*` commands run through the source-consuming call boundary.
- JSON unchanged-output proof compares current JSON runtime output before/after through the same generator contract.
- Sheets and BBNF-self either parse to generated-role witnesses or fail closed with named unsupported constructs. For BBNF-self, likely unsupported constructs are import-items, closures, and host directives unless included in W5A.
- Temporary guard: no new `*_provider.rs` or `css_l4_*_templates` directories, and no deletion before W5B.

## Section 4 - Risks

Parsing without semantic preservation is the main risk. If W5A merely skips `->`, `@{...}`, `?w`, `@ws`, and `@token`, it can pass a parser test while producing a runtime with false whitespace, capture, and typed-value claims. That would reopen REDRESS-209.

A naive import flattening strategy can create duplicate-rule failures or accidental shadowing. The current `regen_css.rs` source roster is a freshness list, not a parse-root list; W5A needs explicit source roots and import closure.

The existing backend has `ValueProject` and `SpanMark`, but the source IR has no corresponding constructs. Adding parser recognition without a carried AST field will strand the data before codegen.

Do not use grammar-name branches to special-case CSS L4. SPEC pre-blocks grammar-name branches in generic crates (`restart/skinny/tranches/sk-v14/SPEC.md:686`), and CH2 generality asks whether the plan generalizes beyond JSON (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:118`).

Do not delete provider/template directories in W5A. SPEC allows provider count to remain non-zero in W5A and moves deletion to W5B (`restart/skinny/tranches/sk-v14/SPEC.md:678`, `restart/skinny/tranches/sk-v14/SPEC.md:701`).

## Section 5 - Sources

All sources are local repo files re-read at HEAD `66d15dfb5`. No external sources were used.
