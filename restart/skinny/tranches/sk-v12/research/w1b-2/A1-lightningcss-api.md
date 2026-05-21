# SK-V12 W1b-2 A1 - lightningcss API Path

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: lightningcss comparator API for
`css_l4/declaration_values/direct_to_struct/main`.

## Finding

Use `lightningcss` as the W1b-2 SOTA parser/comparator, but do not attempt to
emit the W1b-1 byte-identical raw-token stream from the typed AST alone. The
public lightningcss AST is semantic and canonicalized; the W1b-1 facts preserve
source lexemes and offsets such as `.5`, `#ff00ff`, `-10px`, and `rgb(...)`
token boundaries.

The admissible route is a hybrid same-plane extractor:

1. Parse and traverse with lightningcss.
2. Independently scan the original source slice for declaration value
   spans/tokens.
3. Cross-check the source scan against the lightningcss AST declaration
   sequence: depth, property name, `!important`, declaration count.
4. Emit the existing fact format from the source scan only after the AST
   cross-check passes.

This avoids Track 1, avoids direct cssparser calls for the lightningcss
comparator, and keeps the comparator same-plane.

## Dependency

Pin the comparator exactly:

```toml
lightningcss = { version = "=1.0.0-alpha.71", default-features = false }
```

`1.0.0-alpha.71` is the current crate version inspected. Default features pull
bundler/nodejs/sourcemap/rayon/serde-related dependencies that are unnecessary
for parser + AST comparison.

## Candidate API

```rust
use lightningcss::declaration::DeclarationBlock;
use lightningcss::properties::{Property, PropertyId};
use lightningcss::rules::{CssRule, CssRuleList};
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
```

Parse with error recovery disabled:

```rust
let stylesheet = StyleSheet::parse(
    input,
    ParserOptions {
        filename: FIXTURE_RELATIVE.to_string(),
        error_recovery: false,
        ..ParserOptions::default()
    },
)?;
```

Walk the public rule/declaration surfaces:

```rust
fn walk_rules<'i>(rules: &CssRuleList<'i>, depth: u32, out: &mut Vec<LightningDecl<'i>>) {
    for rule in &rules.0 {
        match rule {
            CssRule::Style(style) => {
                emit_decl_block(&style.declarations, depth + 1, out);
                walk_rules(&style.rules, depth + 1, out);
            }
            CssRule::Media(media) => walk_rules(&media.rules, depth + 1, out),
            CssRule::Supports(supports) => walk_rules(&supports.rules, depth + 1, out),
            CssRule::LayerBlock(layer) => walk_rules(&layer.rules, depth + 1, out),
            CssRule::Container(container) => walk_rules(&container.rules, depth + 1, out),
            CssRule::Scope(scope) => walk_rules(&scope.rules, depth + 1, out),
            _ => {}
        }
    }
}

fn emit_decl_block<'i>(block: &DeclarationBlock<'i>, depth: u32, out: &mut Vec<LightningDecl<'i>>) {
    for (property, important) in block.iter() {
        let property_id = property.property_id();
        out.push(LightningDecl {
            depth,
            property: property_id.name(),
            important,
        });
    }
}
```

Verified public surfaces:

- `StyleSheet::parse(code, ParserOptions)` returns a stylesheet with public
  `rules`.
- `CssRuleList(pub Vec<CssRule>)` exposes rule iteration.
- `CssRule::Style`, `CssRule::Media`, and related variants expose nested rule
  lists.
- `StyleRule` exposes `declarations` and nested `rules`.
- `DeclarationBlock::iter()` yields `(&Property, bool)`.
- `Property::property_id()` and `PropertyId::name()` expose property identity.

## W1b-2 Fixture Sequence

The AST verifier should require this declaration sequence:

```text
depth=1 color important=0
depth=1 width important=0
depth=1 opacity important=0
depth=1 margin-left important=0
depth=1 background-color important=1
depth=2 height important=0
depth=2 color important=0
```

The source scanner for the committed fixture only needs these token classes:
`hash`, `percentage`, `number`, `dimension`, `function`, `ident`, `delim`, and
`paren_close`.

## Risks

- lightningcss is still `1.0.0-alpha.*`; pin exact `=1.0.0-alpha.71`.
- Public AST is not a raw token stream and cannot preserve exact source
  lexemes.
- `DeclarationBlock` stores normal and important declarations separately.
  Current fixture is safe because the only important declaration is alone in its
  block; broader fixtures need explicit source-order reconstruction.
- Public source ranges are insufficient for W1b-1 byte offsets.
- lightningcss uses cssparser internally. W1b-2 must forbid direct cssparser API
  calls in the lightningcss comparator, not transitive implementation use.

## Artifacts

Comparator outputs should be:

- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json`
