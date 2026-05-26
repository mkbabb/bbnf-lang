# SK-V14 W5A-C: CSS Companion Emission

Date: 2026-05-26.
Scope: W5A research agent C, CSS L4 runtime-profile and companion emission path.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md`.
HEAD: `66d15dfb5` (`docs(omega-v5-crud6): close w5r gate`).

## Section 1 - Findings

### 1. W5A dispatch obligation is all-seven, source-consuming, and non-deleting

SK-V14 SPEC Section 8 makes W5A a generator-capability wave, not a deletion wave:
source plus workspace metadata must enter codegen, required CSS L4 grammar-source
constructs must parse without grammar-id branches, `regen-css` must migrate, and
all seven CSS L4 profiles plus companions must run through the migrated path
(`restart/skinny/tranches/sk-v14/SPEC.md:637-680`). The same-wave consumer is
`cargo xtask regen-css` plus the seven `check-css-l4-*` companions
(`restart/skinny/tranches/sk-v14/SPEC.md:682-684`). Provider/template deletion is
explicitly pre-blocked until W5B (`restart/skinny/tranches/sk-v14/SPEC.md:686-695`).

The dispatch prompt repeats the same block: before W5A/W5B, Pass Omega V5 CRUD
must exist, and provider/template deletion is forbidden before W5A admits all
seven CSS companions plus JSON/Sheets/BBNF-self proof
(`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:135-145`). W5A is also
mandatory CHALLENGE scope because it is substrate-touching
(`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:224-228`).

The triumvirate contract keeps this file read-only research: one artifact under
`restart/skinny/tranches/sk-v14/research/`, no source edits
(`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11-39`,
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190-200`).

### 2. Current xtask rosters all seven companions but only hashes source/metadata

`skinny/xtask/src/regen_css.rs` names the 15 CSS L4 source inputs
(`skinny/xtask/src/regen_css.rs:5-21`), workspace metadata inputs
(`skinny/xtask/src/regen_css.rs:23`), and all seven runtime targets
(`skinny/xtask/src/regen_css.rs:25-75`). It also exposes one check function per
companion (`skinny/xtask/src/regen_css.rs:81-118`) and tests that the roster has
seven distinct companions and all 15 source files (`skinny/xtask/src/regen_css.rs:125-147`).

The shared `regen` helper validates and hashes those inputs
(`skinny/xtask/src/regen.rs:61-74`) but then calls
`codegen::emit_runtime_profile(target.profile)` for both write and check
(`skinny/xtask/src/regen.rs:14-21`, `skinny/xtask/src/regen.rs:30-40`). That
means source and metadata are freshness inputs only at current HEAD. They are
not codegen inputs.

### 3. Current codegen route is static provider/template dispatch

`emit_runtime_profile` accepts only a profile name, selects a runtime profile,
and renders it (`skinny/crates/codegen/src/lib.rs:117-120`). The profile enum
contains one JSON provider plus seven CSS L4 providers
(`skinny/crates/codegen/src/grammar_profile.rs:16-26`), and the runtime profile
registry is an eight-entry static list
(`skinny/crates/codegen/src/grammar_profile.rs:100-110`).

The CSS render path is seven hardcoded match arms, each calling a per-profile
provider module (`skinny/crates/codegen/src/lib.rs:162-210`). Each provider is a
thin wrapper over `include_str!` template files and adds the generated header
itself. Examples: at-rules/media provider profile and template includes
(`skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs:4-14`,
`skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs:20-52`);
declaration-values provider (`skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:4-13`,
`skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:20-56`);
nested-layout provider (`skinny/crates/codegen/src/css_l4_nested_layout_provider.rs:4-13`,
`skinny/crates/codegen/src/css_l4_nested_layout_provider.rs:20-50`).

The runtime crate then exports the seven generated CSS L4 modules as concrete
module names (`skinny/crates/runtime/src/lib.rs:6-25`) and re-exports them under
`runtime::grammars::*` (`skinny/crates/runtime/src/lib.rs:35-44`).

### 4. All seven profiles that W5A must reroute

| Profile | Companion command | Runtime output | Current provider and branch |
|---|---|---|---|
| `css_l4_at_rules_and_media` | `check-css-l4-at-rules-and-media` | `crates/runtime/src/grammars/css_l4_at_rules_and_media` | target at `skinny/xtask/src/regen_css.rs:26-32`; branch at `skinny/crates/codegen/src/lib.rs:191-195`; provider at `skinny/crates/codegen/src/css_l4_at_rules_and_media_provider.rs:4-52` |
| `css_l4_declaration_values` | `check-css-l4-declaration-values` | `crates/runtime/src/grammars/css_l4_declaration_values` | target at `skinny/xtask/src/regen_css.rs:33-39`; branch at `skinny/crates/codegen/src/lib.rs:167-171`; provider at `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:4-56` |
| `css_l4_declaration_values_extended` | `check-css-l4-declaration-values-extended` | `crates/runtime/src/grammars/css_l4_declaration_values_extended` | target at `skinny/xtask/src/regen_css.rs:40-46`; branch at `skinny/crates/codegen/src/lib.rs:173-177`; provider at `skinny/crates/codegen/src/css_l4_declaration_values_extended_provider.rs:4-60` |
| `css_l4_nested_layout` | `check-css-l4-nested-layout` | `crates/runtime/src/grammars/css_l4_nested_layout` | target at `skinny/xtask/src/regen_css.rs:47-53`; branch at `skinny/crates/codegen/src/lib.rs:203-207`; provider at `skinny/crates/codegen/src/css_l4_nested_layout_provider.rs:4-50` |
| `css_l4_stylesheet_selectors` | `check-css-l4-stylesheet-selectors` | `crates/runtime/src/grammars/css_l4_stylesheet_selectors` | target at `skinny/xtask/src/regen_css.rs:54-60`; branch at `skinny/crates/codegen/src/lib.rs:179-183`; provider at `skinny/crates/codegen/src/css_l4_stylesheet_selectors_provider.rs:4-58` |
| `css_l4_vendor_and_custom_atrules` | `check-css-l4-vendor-and-custom-atrules` | `crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules` | target at `skinny/xtask/src/regen_css.rs:61-67`; branch at `skinny/crates/codegen/src/lib.rs:197-201`; provider at `skinny/crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs:4-60` |
| `css_l4_visual_functions` | `check-css-l4-visual-functions` | `crates/runtime/src/grammars/css_l4_visual_functions` | target at `skinny/xtask/src/regen_css.rs:68-74`; branch at `skinny/crates/codegen/src/lib.rs:185-189`; provider at `skinny/crates/codegen/src/css_l4_visual_functions_provider.rs:4-52` |

Every row above currently routes through `emit_runtime_profile(profile)` and a
static provider. W5A must make every row route through a source-consuming
request/contract instead.

### 5. Counts at HEAD

Command:

```sh
printf 'providers='; find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '; \
printf '\ntemplate_dirs='; find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '; \
printf '\ntemplate_rs='; find skinny/crates/codegen/src -path '*/css_l4_*_templates/*.rs' -type f | wc -l | tr -d ' '; \
printf '\nruntime_dirs='; find skinny/crates/runtime/src/grammars -maxdepth 1 -type d -name 'css_l4_*' | wc -l | tr -d ' '; \
printf '\nruntime_rs='; find skinny/crates/runtime/src/grammars -path '*/css_l4_*/*.rs' -type f | wc -l | tr -d ' '; \
printf '\ncss_sources='; find grammar/css/l4 -type f -name '*.bbnf' | wc -l | tr -d ' '; printf '\n'
```

Result:

```text
providers=8
template_dirs=7
template_rs=35
runtime_dirs=7
runtime_rs=35
css_sources=15
```

Provider enumeration:

```sh
find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | sort | nl -ba
```

Result: eight providers: seven CSS L4 provider modules plus
`skinny/crates/codegen/src/json_provider.rs`. Template-dir enumeration returns
exactly seven `skinny/crates/codegen/src/css_l4_*_templates` directories. Runtime
enumeration returns exactly seven `skinny/crates/runtime/src/grammars/css_l4_*`
directories.

### 6. Companion checks pass, but prove byte reproducibility only

Command:

```sh
cd skinny
for cmd in check-css-l4-at-rules-and-media check-css-l4-declaration-values \
  check-css-l4-declaration-values-extended check-css-l4-nested-layout \
  check-css-l4-stylesheet-selectors check-css-l4-vendor-and-custom-atrules \
  check-css-l4-visual-functions; do
  echo "== $cmd =="
  cargo xtask "$cmd" || exit $?
done
```

Result: exit 0. Each companion printed the same source/metadata digest,
`0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea`, for its
profile:

```text
css_l4_at_rules_and_media PASS
css_l4_declaration_values PASS
css_l4_declaration_values_extended PASS
css_l4_nested_layout PASS
css_l4_stylesheet_selectors PASS
css_l4_vendor_and_custom_atrules PASS
css_l4_visual_functions PASS
```

Interpretation: the companions prove current generated bytes match the static
provider/template output. They do not prove source-consuming emission because
`check_target` still calls `codegen::emit_runtime_profile(target.profile)`
(`skinny/xtask/src/regen.rs:30-40`).

Additional read-only tests:

```sh
cd skinny && cargo test -p xtask css_l4_roster -- --nocapture
# result: 2 passed; roster has seven distinct companions and all 15 sources.

cd skinny && cargo test -p codegen css_l4_ -- --nocapture
# result: 14 passed; seven profile-field tests and seven generated-runtime reproducibility tests passed.

cd skinny && cargo test -p grammar rejects_non_skinny_directives -- --nocapture
# result: 1 passed; current skinny grammar parser still rejects non-skinny directives.
```

### 7. Parser surface is still narrower than CSS L4 source

The skinny parser accepts directives only for `@import` and `@token`
(`skinny/crates/grammar/src/lib.rs:80-99`). Its atom parser handles string
literals, regex literals, groups, and references, then errors on other tokens
(`skinny/crates/grammar/src/lib.rs:196-231`).

CSS L4 source uses constructs outside that surface:

```sh
rg -n -- '->|@\{|>>|<<|@ws|@pretty|\?w' grammar/css/l4/*.bbnf
```

Representative results:

- `grammar/css/l4/values.bbnf:37` uses `->` constant projection.
- `grammar/css/l4/values.bbnf:67-69` uses `@{...}` span capture.
- `grammar/css/l4/values.bbnf:47-55` uses `>>` and `<<` drop/keep delimiters.
- `grammar/css/l4/stylesheet.bbnf:12` uses `@ws`.
- `grammar/css/l4/stylesheet.bbnf:53-60` uses `@pretty`.
- `grammar/css/l4/stylesheet.bbnf:16`, `:29`, `:36-37` use `?w` whitespace modifiers.

W5A therefore needs a runtime-generation parser path that can consume at least
the constructs required by the seven CSS profiles, or a fail-closed witness for
constructs deliberately not supported. A profile-name-only path cannot satisfy
SPEC Section 8.

### 8. REDRESS-209 remains the controlling risk

REDRESS-209 records the original W5 rejection: current HEAD emits `regen-css`
through `codegen::emit_runtime_profile(target.profile)`, CSS source and metadata
are hashed only for freshness, and the parser rejects the CSS L4 source surface
(`skinny/REDRESS.md:5171-5188`). Its supersession note says Pass Omega V5 changed
only the dispatch route: current dispatch is W5A source-consuming generator
capability, then W5B provider/template deletion after W5A admits
(`skinny/REDRESS.md:5189-5193`).

## Section 2 - Recommendations

Recommended W5A plan inputs:

1. Add one source-consuming request type, likely under
   `skinny/crates/codegen/src/grammar_provider.rs` or a successor module named by
   SPEC Section 8. Minimum fields: grammar id, target/profile id, ordered source
   bundle, workspace metadata digest or parsed metadata, output roster, runtime
   contract, and source-consumption provenance.
2. Change `skinny/xtask/src/regen.rs` so `write_targets` and `check_target` call
   a new source-consuming function instead of `emit_runtime_profile(target.profile)`.
   Keep `validate_inputs`, but pass the actual source bytes and metadata into
   codegen after validation.
3. Extend the runtime-generation parser enough for the CSS L4 source surface
   used by all seven profiles: import bundle resolution, comma sequencing,
   `->` constants/types/host projections, `@{...}` span capture, `>>`/`<<`,
   `?w`, `@ws`, and `@pretty` either as consumed syntax or explicit
   fail-closed/generator-irrelevant facts. Do not branch on `grammar_id == css_l4`.
4. Add a W5A proof that every CSS companion goes through the new contract. A
   good falsifier is an in-memory source mutation test: static-provider output
   must no longer pass without the parser/provenance layer observing the source
   delta.
5. Keep the seven CSS providers and seven template directories untouched in W5A.
   They can remain as non-consuming legacy surfaces only until W5B.
6. Preserve JSON unchanged-output by comparing `emit_from_source("json", source)`
   before/after, not by adding JSON policy to generic codegen.
7. Add Sheets and BBNF-self witnesses through the same contract: either generated
   role witnesses or fail-closed errors naming unsupported constructs.
8. Add a temporary Lock 14 W5A guard: existing provider/template paths are
   tolerated, new provider modules or template directories are rejected, and W5B
   remains the only deletion owner.

Recommended falsifiability gates for the plan:

- `cd skinny && cargo xtask check-json`
- `cd skinny && cargo xtask check-css-l4-at-rules-and-media`
- `cd skinny && cargo xtask check-css-l4-declaration-values`
- `cd skinny && cargo xtask check-css-l4-declaration-values-extended`
- `cd skinny && cargo xtask check-css-l4-nested-layout`
- `cd skinny && cargo xtask check-css-l4-stylesheet-selectors`
- `cd skinny && cargo xtask check-css-l4-vendor-and-custom-atrules`
- `cd skinny && cargo xtask check-css-l4-visual-functions`
- `cd skinny && cargo test -p xtask css_l4_roster -- --nocapture`
- `cd skinny && cargo test -p codegen css_l4_ -- --nocapture`
- Provider/template non-deletion check:
  `find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l`
  remains `8`, and
  `find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l`
  remains `7`.
- New source-consuming proof check: no CSS `regen-css` path calls
  `emit_runtime_profile(target.profile)`; all seven companions call the new
  request/contract path.

## Section 3 - Risks

- Parser scope risk: the 15 CSS L4 source files use more than `->` and
  `@{...}`. W5A must either parse or explicitly classify `>>`, `<<`, `?w`,
  comma sequencing, `@ws`, and `@pretty`; otherwise only part of the source
  surface is consumed.
- Paper-close risk: all seven companion checks already pass at HEAD, but through
  static providers. Passing them after W5A is not sufficient unless the check
  path is proven to call the source-consuming contract.
- Static-centralization risk: moving provider/template text into one file would
  reduce path count but repeat REDRESS-209 and P-6 because runtime bodies would
  remain hand-authored per profile.
- Lock 14 guard risk: the current baseline allowlists the existing CSS providers,
  templates, and generated headers. W5A needs a temporary state that blocks new
  provider/template surfaces without forcing W5B deletion early.
- Budget risk: a full CSS L4 semantic generator is larger than W5A's 1.0k source
  cap. The plan should target a minimal runtime-generation parser/contract plus
  provenance proof, leaving provider/template deletion to W5B and root runtime
  collapse to W6.0.
- Cross-grammar proof risk: JSON unchanged-output and Sheets/BBNF-self witnesses
  are part of the W5A exit gate. A CSS-only contract would fail the Lock 14
  generality requirement.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v14/SPEC.md:637-680`
- `restart/skinny/tranches/sk-v14/SPEC.md:682-695`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:135-145`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:224-228`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11-39`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190-200`
- `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:70-110`
- `skinny/REDRESS.md:5171-5193`
- `skinny/xtask/src/regen.rs:14-21`
- `skinny/xtask/src/regen.rs:30-40`
- `skinny/xtask/src/regen.rs:61-74`
- `skinny/xtask/src/regen_css.rs:5-75`
- `skinny/xtask/src/regen_css.rs:81-118`
- `skinny/xtask/src/regen_css.rs:125-147`
- `skinny/crates/codegen/src/lib.rs:117-120`
- `skinny/crates/codegen/src/lib.rs:162-210`
- `skinny/crates/codegen/src/grammar_profile.rs:16-26`
- `skinny/crates/codegen/src/grammar_profile.rs:100-110`
- `skinny/crates/grammar/src/lib.rs:80-99`
- `skinny/crates/grammar/src/lib.rs:196-231`
- `grammar/css/l4/values.bbnf:37`
- `grammar/css/l4/values.bbnf:67-69`
- `grammar/css/l4/stylesheet.bbnf:12`
- `grammar/css/l4/stylesheet.bbnf:53-60`
