# SK-V14 W5 Plan: Reject Static Provider Collapse

Date: 2026-05-26.
Wave: W5.
Phase: plan.
Status: REJECT.

## Decision

Do not implement W5 under the current SPEC shape.

W5 requires a real Lock 14 provider collapse: one grammar-agnostic generator
template consumes grammar source plus workspace metadata, `regen-css` consumes
that path, the seven CSS provider modules and seven CSS template directories are
deleted in the same replacement slice, and the post-W5 Lock 14 forward invariant
passes.

Current HEAD cannot honestly satisfy that contract. It can re-emit the seven
skinny CSS runtime profiles, but it does so through the static provider/template
mesh W5 is ordered to delete. Moving those static bodies into a single
`grammar_provider.rs` would satisfy a file-count predicate while preserving the
same hand-written per-profile runtime implementation. That is not the W5
generator promised by SPEC §8 or Lock 14.

## Re-anchored Evidence

SPEC §8 binds W5 to the provider-collapse surface:

- `restart/skinny/tranches/sk-v14/SPEC.md:635-647` names W5 owner paths:
  `passes`, `codegen`, one `grammar_provider.rs`, the eight current providers,
  the seven CSS template dirs, `regen_css.rs`, and Lock 14 baseline.
- `restart/skinny/tranches/sk-v14/SPEC.md:654-668` requires the plan to name
  the trait surface, collapse the per-grammar providers, consume grammar source
  plus workspace metadata, migrate `regen_css.rs`, and enforce the post-W5
  forward invariant.
- `restart/skinny/tranches/sk-v14/SPEC.md:670-680` requires zero provider
  modules, zero CSS template dirs, Lock 14 grep cleanliness, `cargo xtask
  regen-css`, and all seven `check-css-l4-*` companions.
- `restart/skinny/tranches/sk-v14/SPEC.md:699-703` says W5 rejection blocks W6
  and W7. The final sentence claiming W8-W10 can proceed independently conflicts
  with the active prune-before-new-admit chain and must be corrected by Omega.

Current code is still pre-W5:

- `skinny/crates/codegen/src/lib.rs:1-10` declares seven CSS provider modules
  and `json_provider`.
- `skinny/crates/codegen/src/lib.rs:117-120` exposes
  `emit_runtime_profile(grammar_name)` without grammar source or workspace
  metadata.
- `skinny/crates/codegen/src/lib.rs:162-210` matches over
  `RuntimeProvider::{CssL4..., Json}`.
- `skinny/crates/codegen/src/grammar_profile.rs:1-6` imports the provider
  modules; `grammar_profile.rs:16-26` defines `RuntimeProvider`; and
  `grammar_profile.rs:100-110` returns the eight static runtime profiles.
- `skinny/xtask/src/regen.rs:14-33` validates source/metadata inputs, hashes
  them, then calls `codegen::emit_runtime_profile(target.profile)`.
- `skinny/xtask/src/regen.rs:61-74` proves those inputs are freshness inputs
  only: the source and metadata bytes feed a digest, not codegen.
- `skinny/crates/grammar/src/lib.rs:80-99` accepts only `@import` and `@token`
  directives.
- `skinny/crates/grammar/src/lib.rs:196-231` parses atoms as literals, regexes,
  grouped expressions, or identifiers; it has no value-projection or span-capture
  atom.
- `grammar/css/l4/values.bbnf:37` uses `->` value projection, and
  `grammar/css/l4/values.bbnf:67-69` documents and uses `@{...}` span capture.

Executable checks re-run at HEAD:

```sh
find skinny/crates -name '*.rs' | xargs grep -l 'RuntimeProvider::Json\|JsonGrammar\|parse_json_grammar' | wc -l
# 5

find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
# 8

find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l
# 7

find grammar/css/l4 -type f -name '*.bbnf' | wc -l
# 15

cd skinny && cargo xtask regen-css
# exits 0 through the static provider/template mesh

cd skinny && cargo test -p grammar rejects_non_skinny_directives -- --nocapture
# 1 passed
```

A temporary parse probe against `grammar/css/l4/values.bbnf` returned:

```text
ERR BBNF-PARSE: unexpected token `-` at byte 1362
```

That probe is consistent with the grammar parser's current accepted atom and
directive surface and confirms the CSS source cannot yet feed a generic source
consumer.

## Rejected Implementation Routes

- **Static centralization**: move the seven CSS provider/template bodies into one
  file and delete the old paths. This hides the per-profile runtime code but does
  not consume grammar source or workspace metadata.
- **Provider deletion before replacement**: repeats REDRESS-184, because current
  `regen-css` compiles through the provider modules.
- **Runtime edits in W5**: violates SPEC §8 entry gate; W6 owns root runtime
  Pattern H collapse.
- **W8/W9/W10 advance**: violates the prune-before-new-admit chain. W8 depends
  on CSS PRUNE completion; W9/W10 remain globally blocked until PRUNE-1 through
  PRUNE-5 close despite their local comparator prerequisites.

## Disposition

Route W5 to REDRESS-209 and Pass Omega V5 W5R. The amendment should split the
current W5 obligation into a generator-capability wave and a provider/template
deletion wave, then keep W6 and later waves blocked until the replacement path
exists and is consumed by `regen-css`.
