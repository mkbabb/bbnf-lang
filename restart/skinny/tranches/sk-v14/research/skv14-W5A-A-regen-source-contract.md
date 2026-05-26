# SK-V14 W5A-A: regen-css Source-Consuming Generator Contract

Date: 2026-05-26.
Scope: Research agent A; read SK-V14 W5A dispatch surfaces, REDRESS-209/W5R, and current skinny xtask/codegen source to identify the minimal source-consuming generator contract for W5A.
Output: this file.

## 1. Findings

1. W5A is explicitly a research/plan/redress-separated, no-source-edit research phase. The triumvirate contract makes research read-only and requires one artefact under `restart/skinny/tranches/sk-v{N}/research/` (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:13`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:27`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:35`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:194`).

2. Current dispatch authority names Pass Omega V5 W5R as binding, verifies W5A before W5B, and forbids provider/template deletion before W5A admits all-seven CSS companion coverage plus JSON/Sheets/BBNF-self proof (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:5`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:29`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:137`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:143`, `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:224`).

3. SPEC Section 8 requires W5A to pass grammar source plus workspace metadata into codegen for runtime emission, parse required V1 grammar-source constructs without `grammar_id == css_l4` behavior, migrate `regen-css`, prove JSON unchanged output, prove Sheets/BBNF-self fail-closed or generated-role witnesses, and keep provider/template deletion out of W5A (`restart/skinny/tranches/sk-v14/SPEC.md:637`, `restart/skinny/tranches/sk-v14/SPEC.md:641`, `restart/skinny/tranches/sk-v14/SPEC.md:656`, `restart/skinny/tranches/sk-v14/SPEC.md:662`, `restart/skinny/tranches/sk-v14/SPEC.md:670`, `restart/skinny/tranches/sk-v14/SPEC.md:682`, `restart/skinny/tranches/sk-v14/SPEC.md:686`, `restart/skinny/tranches/sk-v14/SPEC.md:693`).

4. REDRESS-209 is the local failure this W5A work must not repeat. It records that current HEAD emits `regen-css` through `codegen::emit_runtime_profile(target.profile)` and the static provider/template mesh; CSS source and metadata are hashed for freshness only, not passed into codegen; the skinny grammar parser also rejects CSS L4 constructs such as `->` projections and `@{...}` span capture (`skinny/REDRESS.md:5173`, `skinny/REDRESS.md:5178`, `skinny/REDRESS.md:5180`, `skinny/REDRESS.md:5181`, `skinny/REDRESS.md:5189`). The W5R corrective packet restates the gap and names the amendment: introduce a source-consuming runtime emission request carrying grammar id, target id, grammar-source bundle, workspace metadata digest, output roster, and runtime contract; parse CSS L4 source constructs; migrate `regen-css`; do not delete providers/templates in W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:8`, `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:14`, `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:74`, `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:80`, `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:88`, `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md:91`).

5. The current skinny `regen-css` command is command-stable but source-passive. `skinny/xtask/src/main.rs` exposes `regen-css` plus seven exact companion checks (`skinny/xtask/src/main.rs:10`, `skinny/xtask/src/main.rs:22`, `skinny/xtask/src/main.rs:23`). `skinny/xtask/src/regen_css.rs` hardcodes the 15 CSS L4 source paths and the two metadata inputs (`Cargo.toml`, `skinny/Cargo.toml`) for all seven targets (`skinny/xtask/src/regen_css.rs:5`, `skinny/xtask/src/regen_css.rs:23`, `skinny/xtask/src/regen_css.rs:25`, `skinny/xtask/src/regen_css.rs:77`, `skinny/xtask/src/regen_css.rs:109`). `skinny/xtask/src/regen.rs` validates uniqueness, reads and hashes the source/metadata files, then calls `codegen::emit_runtime_profile(target.profile)` with only the profile string (`skinny/xtask/src/regen.rs:5`, `skinny/xtask/src/regen.rs:14`, `skinny/xtask/src/regen.rs:17`, `skinny/xtask/src/regen.rs:18`, `skinny/xtask/src/regen.rs:30`, `skinny/xtask/src/regen.rs:61`, `skinny/xtask/src/regen.rs:83`).

6. Current skinny codegen has one source-consuming path for JSON and one profile-only path for CSS. `emit_from_source` parses grammar source and compiles passes (`skinny/crates/codegen/src/lib.rs:98`, `skinny/crates/codegen/src/lib.rs:99`, `skinny/crates/codegen/src/lib.rs:100`); `emit_runtime_profile` selects by name only (`skinny/crates/codegen/src/lib.rs:117`, `skinny/crates/codegen/src/lib.rs:118`). CSS rendering matches `RuntimeProvider` variants and calls per-profile provider modules (`skinny/crates/codegen/src/lib.rs:162`, `skinny/crates/codegen/src/lib.rs:166`, `skinny/crates/codegen/src/lib.rs:167`, `skinny/crates/codegen/src/lib.rs:203`, `skinny/crates/codegen/src/lib.rs:209`). The profile registry is eight hardcoded variants (`skinny/crates/codegen/src/grammar_profile.rs:16`, `skinny/crates/codegen/src/grammar_profile.rs:17`, `skinny/crates/codegen/src/grammar_profile.rs:100`). A representative CSS provider returns `include_str!` template bodies with a generated header (`skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:20`, `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:24`, `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:30`, `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs:51`).

7. The parser gap is larger than just `->` and `@{...}`. The skinny grammar parser accepts only `@import` and `@token` directives (`skinny/crates/grammar/src/lib.rs:80`, `skinny/crates/grammar/src/lib.rs:91`), postfix `* ? +` (`skinny/crates/grammar/src/lib.rs:153`), literals/regex/groups/refs (`skinny/crates/grammar/src/lib.rs:196`), comments (`skinny/crates/grammar/src/lib.rs:314`), and does not model explicit comma sequencing, `>>`, `<<`, value projections, typed projections, or span capture. CSS L4 source uses these constructs in currently load-bearing files: `mathOperator = "+" -> 0u8 ...` (`grammar/css/l4/values.bbnf:37`), `("(" >> mathExpr << ")")` (`grammar/css/l4/values.bbnf:47`), comma-separated function bodies (`grammar/css/l4/values.bbnf:52`), span capture (`grammar/css/l4/values.bbnf:67`, `grammar/css/l4/values.bbnf:69`), and global keyword discriminants (`grammar/css/l4/values.bbnf:74`).

8. Workspace metadata is split. The root workspace has the fleet-level grammar list including `css_l4` at `grammar/css/l4/stylesheet.bbnf` (`Cargo.toml:18`, `Cargo.toml:19`, `Cargo.toml:22`). The skinny workspace metadata currently names only JSON under `workspace.metadata.bbnf.grammars.json` (`skinny/Cargo.toml:54`, `skinny/Cargo.toml:60`, `skinny/Cargo.toml:61`, `skinny/Cargo.toml:69`), and `workspace_root()` finds skinny by requiring that JSON metadata exists (`skinny/xtask/src/main.rs:1350`, `skinny/xtask/src/main.rs:1367`). Therefore W5A can consume existing metadata without editing manifests only if it treats root `Cargo.toml` as the CSS grammar-source authority and skinny `Cargo.toml` as skinny runtime output/workspace policy. Any stronger profile metadata probably needs an explicit owner-path amendment.

9. The non-skinny root `xtask` is the local reference pattern for source + metadata flow. It reads `[workspace.metadata.bbnf.grammars]` via `cargo_metadata` (`xtask/src/regen.rs:225`, `xtask/src/regen.rs:229`, `xtask/src/regen.rs:236`, `xtask/src/regen.rs:246`), validates feature flags fail-closed (`xtask/src/regen.rs:249`, `xtask/src/regen.rs:253`), resolves manifest paths to grammar sources (`xtask/src/regen.rs:51`, `xtask/src/regen.rs:63`), turns metadata features into parser attributes with source paths and portable relative paths (`xtask/src/regen.rs:91`, `xtask/src/regen.rs:95`, `xtask/src/regen.rs:108`), and runs the compiler over source paths (`xtask/src/regen.rs:313`, `xtask/src/regen.rs:327`, `xtask/src/regen.rs:330`, `xtask/src/regen.rs:341`). W5A should adapt this contract shape rather than invent a parallel manifest parser.

## 2. Current Executable Evidence

Commands were re-run at HEAD `66d15dfb5` (`docs(omega-v5-crud6): close w5r gate`).

```sh
git rev-parse --short HEAD
# 66d15dfb5

find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
# 8
find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l
# 7
```

Read-only CSS companion check loop:

```sh
cd skinny
for cmd in check-css-l4-at-rules-and-media check-css-l4-declaration-values check-css-l4-declaration-values-extended check-css-l4-nested-layout check-css-l4-stylesheet-selectors check-css-l4-vendor-and-custom-atrules check-css-l4-visual-functions; do
  cargo xtask "$cmd"
done
# exit 0
# inputs: css_l4_at_rules_and_media 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_declaration_values 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_declaration_values_extended 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_nested_layout 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_stylesheet_selectors 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_vendor_and_custom_atrules 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# inputs: css_l4_visual_functions 0e6cf1a85b96581c879535dcf06c48ca34edc8ec617f272410fff25b22f001ea
# Cargo also emitted existing unused-patch/profile warnings; no check failed.
```

Read-only JSON unchanged-output baseline:

```sh
cd skinny && cargo xtask check-json
# exit 0
# Cargo emitted the same existing unused-patch/profile warnings; no generated JSON drift.
```

Source-shape grep:

```sh
rg -n -- '->|@\{' grammar/css/l4 skinny/crates/grammar/src/lib.rs skinny/crates/codegen/src skinny/xtask/src
# grammar/css/l4/values.bbnf:37:mathOperator = "+" -> 0u8 | "-" -> 1u8 | "*" -> 2u8 | "/" -> 3u8 ;
# grammar/css/l4/values.bbnf:67:// @{...} span capture: preserves raw URL text for source maps and
# grammar/css/l4/values.bbnf:69:urlFunction = @{ "url" , "(" >> (string | /[^)"'\s]+/) << ")" } ;
# many CSS keyword/unit projection rows; no parser support hit in skinny/crates/grammar/src/lib.rs beyond the generic error text for unsupported directives.
```

Post-command status before writing this artifact showed no skinny source/generated drift:

```sh
git diff --name-only -- skinny xtask restart/skinny/tranches/sk-v14/research/skv14-W5-A-provider-dispatch.md restart/skinny/tranches/sk-v14/research/skv14-W5-B-regen-css-consumer.md restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md
# no output
```

## 3. Minimal W5A Generator Contract

The minimal honest contract is a new codegen entrypoint that cannot be satisfied by passing source through a digest side channel:

```rust
pub struct RuntimeEmitRequest<'a> {
    pub grammar_id: &'a str,
    pub target_id: &'a str,
    pub sources: &'a [GrammarSource<'a>],
    pub workspace_metadata: RuntimeWorkspaceMetadata<'a>,
    pub expected_files: &'a [&'a str],
}

pub struct GrammarSource<'a> {
    pub rel_path: &'a str,
    pub text: &'a str,
}

pub fn emit_runtime_from_request(req: RuntimeEmitRequest<'_>) -> Result<EmittedSource, CodegenError>;
```

Required semantics:

- `regen-css` constructs `RuntimeEmitRequest` for each of the seven targets and passes the 15 source files plus parsed/canonical metadata into codegen. `validate_inputs` may remain as a guard, but hash-only validation is not the consumption proof.
- Codegen parses the source bundle into a grammar-neutral `RuntimeGrammarBundle`: import closure, rule table, source spans, literals, regexes, refs, alt/seq/postfix, explicit comma sequence, `>>`/`<<` skip/boundary operators, `->` projections, typed projections (`f64`, `i64`, `u8`), and `@{...}` span capture. Unsupported constructs return path:line/source-construct errors.
- Metadata is consumed for policy, not merely hashed: grammar id/source root, target id, generated file roster, output root, runtime mode/profile, host registry, and feature flags must be part of request validation and emission planning.
- Emission is selected from the parsed bundle plus metadata, not from `RuntimeProvider` alone. The W5A call boundary must not call `codegen::emit_runtime_profile(target.profile)`, and the source-consuming path must not call `css_l4_*_provider::emit_runtime_files()` as its CSS runtime source.
- JSON uses the same entrypoint or a thin adapter over it and must prove byte-identical `check-json` output. Sheets and BBNF-self must either produce generated-role witnesses from the same parser/contract or fail closed with named unsupported constructs.
- W5A may leave the eight provider modules and seven template directories present, but they must be legacy/non-consuming inputs with a W5B deletion owner. No new provider module or template directory is allowed.

Minimum materiality tests/gates:

- Parser fixture over the 15 CSS files proves the listed constructs parse and imports resolve without `grammar_id == css_l4` branches.
- Source materiality: changing a copied source rule/projection/span capture changes the parsed emission plan or fails with a named construct error; changing source must not merely change a digest print.
- Metadata materiality: changing copied output roster/runtime metadata changes validation or fails closed.
- Call-boundary scan proves `regen-css` no longer reaches `emit_runtime_profile(target.profile)` for CSS targets.
- Existing commands remain same-wave consumers: `cargo xtask check-json` and the seven CSS companions.

## 4. Recommendations

Plan input should select one intervention: "introduce `RuntimeEmitRequest` and route `regen-css` through `emit_runtime_from_request` with source-bundle parsing and metadata-backed emission planning; leave provider/template deletion to W5B."

Recommended owner paths:

- `skinny/crates/grammar/src/lib.rs` or a new parser module in `skinny/crates/grammar/src/`.
- `skinny/crates/codegen/src/lib.rs`.
- `skinny/crates/codegen/src/grammar_provider.rs` or an equivalent new source-consuming contract module.
- `skinny/xtask/src/regen.rs` and `skinny/xtask/src/regen_css.rs`.
- Focused tests in the same crates.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs` only for the temporary W5A no-new-provider/no-template-deletion guard.

Recommended plan gates:

- `cargo xtask check-json`.
- All seven `cargo xtask check-css-l4-*` companions.
- `find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l` remains at the pre-W5A count or lower only if W5B is explicitly in scope; for W5A, no deletion is required and no new provider file may appear.
- `find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l` remains at the pre-W5A count; no new template directory may appear.
- Grep gate: CSS `regen-css` path has no `emit_runtime_profile(target.profile)` call.
- Parser/materiality tests over copied CSS sources and copied metadata.

## 5. Risks

- The current skinny metadata does not declare CSS profiles, only JSON. W5A can consume root `Cargo.toml` for the `css_l4` source anchor and skinny `Cargo.toml` for generated-root/runtime policy, but full profile metadata may need an owner-path amendment if the plan wants to edit manifests.
- A digest/provenance constant in generated output is not sufficient. REDRESS-209 already identifies hash-only source handling as the rejected shape.
- Static centralization into one `grammar_provider.rs` is still the P-6 recurrence if it just moves hand-written template bodies.
- Import resolution is mandatory: the root `css_l4` source anchor is `stylesheet.bbnf`, while the current `regen-css` freshness roster names 15 source files.
- The parser work must stay grammar-neutral. Any `if grammar_id == "css_l4"` acceptance branch is a direct SPEC Section 8 failure.
- Running `cargo xtask regen-css` would write generated files; this research phase used only check commands.

## 6. Sources

- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md`
- `skinny/xtask/src/main.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_provider.rs`
- `skinny/crates/grammar/src/lib.rs`
- `Cargo.toml`
- `skinny/Cargo.toml`
- `grammar/css/l4/values.bbnf`
- `xtask/src/regen.rs`
