# SK-V14 W4R-D: No-Deletion Guard

Date: 2026-05-26.
Wave: W4R.
Phase: research.
Agent: Ampere.
Scope: read-only inspection of W4 non-owner paths.

## Question

Define the amended-W4 proof that no CSS source, generator, provider,
template, runtime-twin, or `regen_css` deletion occurred.

## Finding

Pass Omega V4 made W4 ledger-only. The following surfaces are explicit
non-owner paths for W4 and must remain preserved until W5 or W6 owns them:

- `grammar/css/l4/*.bbnf`;
- `skinny/crates/codegen/src/css_l4_*_provider.rs`;
- `skinny/crates/codegen/src/css_l4_*_templates/*.rs`;
- `skinny/crates/runtime/src/grammars/css_l4_*/*.rs`;
- `skinny/xtask/src/regen_css.rs`;
- `crates/core/src/runtime/css_l4/*.rs`;
- `crates/core/src/grammar/generated/css_l4.rs`;
- `crates/core/src/grammar/generated/css_l4.registry.json`.

Expected preserved counts at W4 close:

- 15 CSS source files;
- 7 CSS provider modules;
- 35 CSS template files;
- 35 skinny runtime CSS files.

## Guard Commands

```sh
git diff --name-status --diff-filter=D HEAD -- \
  grammar/css/l4 \
  crates/core/src/runtime/css_l4 \
  crates/core/src/grammar/generated/css_l4.rs \
  crates/core/src/grammar/generated/css_l4.registry.json \
  skinny/crates/codegen/src \
  'skinny/crates/runtime/src/grammars/css_l4_*' \
  skinny/xtask/src/regen_css.rs

git diff --name-only HEAD -- \
  skinny/crates/codegen/src \
  skinny/crates/runtime/src/grammars \
  skinny/xtask/src/regen_css.rs

find grammar/css/l4 -type f -name '*.bbnf' | sort | wc -l
find skinny/crates/codegen/src -maxdepth 1 -name 'css_l4_*_provider.rs' | sort | wc -l
find skinny/crates/codegen/src -maxdepth 2 -path '*/css_l4_*_templates/*.rs' -type f | sort | wc -l
find skinny/crates/runtime/src/grammars -maxdepth 2 -path '*/css_l4_*/*.rs' -type f | sort | wc -l
```

Expected close values: the two `git diff` commands print no deletion or
source-generation edits; the four `find` counts print `15`, `7`, `35`, and
`35`.

## Consumer

W4 redress records these checks before marking the amended ledger PRUNE
admitted. W5 then inherits the provider/template deletion and must perform it
only in the same slice as the replacement generic provider path.
