# SK-V14 W6.0 Redress: CSS L4 Root Runtime Projection Gap

Date: 2026-05-26.

Disposition: REJECT.

## Gate

`G-SK-V14-W6.0-CSS-L4-ROOT-RUNTIME-COLLAPSE`

SPEC §9 requires W6.0 to collapse `crates/core/src/runtime/css_l4/` by adding a
root `cargo xtask regen-css` path that regenerates the seven CSS L4 root runtime
files and survives:

```sh
rm -rf crates/core/src/runtime/css_l4
cargo xtask regen-css
git diff --exit-code -- crates/core/src/runtime/css_l4
```

## Finding

The route gate was admitted first at `e3c8c8706`, so W6.0 owner scope is now
narrowly authorized. The implementation gate still fails because no current
root generator can produce the CSS L4 root runtime tree.

Evidence:

- `CARGO_TARGET_DIR=/tmp/bbnf-w6-proof-target cargo xtask regen-css` built the
  root xtask and exited `2` with `error: unrecognized subcommand 'regen-css'`.
- `CARGO_TARGET_DIR=/tmp/bbnf-w6-proof-parser-target cargo xtask regen --grammar css_l4 --output /tmp/w6-css-parser-proof`
  exited `0`, but `find /tmp/w6-css-parser-proof -maxdepth 1 -type f -print`
  returned only:
  - `/tmp/w6-css-parser-proof/css_l4.rs`
  - `/tmp/w6-css-parser-proof/css_l4.registry.json`
- `xtask/src/regen.rs` writes parser and registry sidecars under
  `crates/core/src/grammar/generated/`; it has no runtime-output target for
  `crates/core/src/runtime/css_l4/`.
- `crates/core/src/runtime/arena_template.rs` and
  `crates/core/src/runtime/builder_template.rs` explicitly keep CSS L4 outside
  the simple runtime template because CSS L4 owns multiple arena slabs, a
  recursive color DAG, and grammar-specific builder frame/deposit logic.
- The W5C skinny generator emits seven CSS L4 fact-stream runtime profiles, not
  the root `CssDocument` / `CssStructBuilder` / typed value API consumed by
  `crates/core/src/grammar/generated/css_l4.rs`.

## Why Static Generation Is Rejected

The current `crates/core/src/runtime/css_l4/` implementation is semantic
machinery, not a formatting shell. The committed files define:

- domain AST constructors and unit discriminants (`CssColor`, `CssDimension`,
  `CssFunction`, `CssTypedValue`);
- six CSS arena families plus recursive color storage;
- `CssDocument` walkers and typed path projection;
- `RuntimeView` traversal over stylesheet/rule/declaration/value focus nodes;
- `CssStructBuilder` rule-id dispatch for stylesheet, rule, declaration,
  selector, numeric, color, function, and pseudo-class frames.

The current parser registry sidecar does not carry those semantics. Re-emitting
the existing Rust bodies from `include_str!`, copied templates, or a central
string table would preserve bytes while hiding hand-written code behind a
generated header. That is the same recurrence vector W4/W5 already pruned.

## Revert Status

No W6.0 runtime source/test edits were made after the admitted route-gate
commit. `/tmp/skv14-waveW6.0-rejected.patch` is therefore intentionally empty:
there is no rejected source patch to recover.

The working tree after redress contains only documentation edits for this
rejection plus pre-existing unrelated user/generated changes outside W6.0.

## Corrective Route

W6.0 needs a real runtime projection source and emitter before it can be
retried. The projection source must be declarative data, not copied Rust, and
must define at least:

- exported CSS runtime symbols and module roster;
- enum/struct constructor families for CSS values, units, colors, functions,
  selectors, declarations, rules, and stylesheet;
- rule-name/rule-id to builder-frame mapping;
- leaf/tag routing into frames;
- arena-family placement and rollback counters;
- recursive color-reference handling;
- document focus, path query, walker, and runtime-view traversal.

Only after that emitter can destructively regenerate
`crates/core/src/runtime/css_l4/` from grammar source, workspace metadata,
registry data, and runtime projection data should W6.0 be reattempted.

## Downstream Effect

W6.1..W6.8 remain blocked by the W6 sub-wave order. W7 and W8/W9/W10 also remain
blocked because PRUNE-4 has not admitted.
