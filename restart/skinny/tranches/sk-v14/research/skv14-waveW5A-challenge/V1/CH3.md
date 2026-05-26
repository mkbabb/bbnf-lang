# SK-V14 W5A CHALLENGE V1 - CH3 Regression

Date: 2026-05-26.

Scope: CH3 Regression review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` against SPEC Section 8, DISPATCH Section 4.3 including NEW-CH3-V4-01, REDRESS-184, REDRESS-209, and W5A research artifacts. Focus: whether W5A reopens deleted-before-rebuild failures, and whether provider/template/runtime delete-targets are ordered after rebuild capability.

Disposition: ACCEPT.

## §1 Findings

1. The plan does not reopen REDRESS-184. REDRESS-184 rejected W4 because it would delete the seven CSS L4 provider modules and template directories, then immediately run `cargo xtask regen-css` even though the live path still compiled through those provider modules (`skinny/REDRESS.md:5105`, `skinny/REDRESS.md:5106`, `skinny/REDRESS.md:5107`, `skinny/REDRESS.md:5108`, `skinny/REDRESS.md:5111`, `skinny/REDRESS.md:5112`). W5A keeps provider/template files outside owner paths (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:31`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:32`), requires no CSS provider/template deletion or rename during W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`), and pre-blocks deleting or renaming CSS providers/templates before W5B (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:84`).

2. The plan does not reopen REDRESS-209. REDRESS-209 rejected monolithic W5 because `regen-css` still emitted through `codegen::emit_runtime_profile(target.profile)`, CSS source/metadata were hashed only for freshness, the static provider/template mesh remained load-bearing, and the skinny parser rejected CSS L4 `->` and `@{...}` constructs (`skinny/REDRESS.md:5173`, `skinny/REDRESS.md:5177`, `skinny/REDRESS.md:5178`, `skinny/REDRESS.md:5179`, `skinny/REDRESS.md:5180`, `skinny/REDRESS.md:5181`). The supersession note keeps REDRESS-209 historical while routing current dispatch to W5A source-consuming capability followed by W5B deletion after W5A admits (`skinny/REDRESS.md:5189`, `skinny/REDRESS.md:5190`, `skinny/REDRESS.md:5191`, `skinny/REDRESS.md:5192`). The W5A plan selects one `RuntimeGenerationRequest` path carrying grammar source plus workspace metadata (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`), requires `regen-css` and all seven companions to call that path instead of the profile-only call boundary (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`), names the CSS L4 constructs to parse without `grammar_id == "css_l4"` branching (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`), and pre-blocks static centralization (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:81`).

3. NEW-CH3-V4-01 is satisfied for the W5A plan. DISPATCH requires CH3 to grep delete-target/rebuild-capability pairs and assert rebuild capability precedes deletion (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:178`), matching the V5 lesson that rebuild capability must precede deletion unless the same wave first proves the replacement is load-bearing (`restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:44`, `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:45`, `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:46`, `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:47`). For provider/template delete-targets, W5A proves the rebuild capability through source+metadata codegen, parser constructs, `regen-css`, all seven CSS companions, JSON proof, and Sheets/BBNF-self proof (`restart/skinny/tranches/sk-v14/SPEC.md:662`, `restart/skinny/tranches/sk-v14/SPEC.md:663`, `restart/skinny/tranches/sk-v14/SPEC.md:664`, `restart/skinny/tranches/sk-v14/SPEC.md:665`, `restart/skinny/tranches/sk-v14/SPEC.md:666`, `restart/skinny/tranches/sk-v14/SPEC.md:668`), while W5B deletion is gated on W5A admission and on the profiles/companions already passing through the W5A path (`restart/skinny/tranches/sk-v14/SPEC.md:715`, `restart/skinny/tranches/sk-v14/SPEC.md:716`, `restart/skinny/tranches/sk-v14/SPEC.md:721`, `restart/skinny/tranches/sk-v14/SPEC.md:724`).

4. Runtime deletion/collapse ordering is also preserved. The W5A plan excludes `skinny/crates/runtime/src/grammars/css_l4_*/` except generated output from `cargo xtask regen-css` and excludes `crates/core/src/runtime/css_l4/` entirely (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:33`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:34`), adds a no-root-runtime diff check (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:70`), and pre-blocks editing root CSS runtime before W6 (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:85`). SPEC then makes W6 runtime collapse conditional on W5B admitted, with the W5A source-consuming generator path already existing (`restart/skinny/tranches/sk-v14/SPEC.md:775`), and W6.0 owns the destructive CSS root-runtime gate only then (`restart/skinny/tranches/sk-v14/SPEC.md:806`, `restart/skinny/tranches/sk-v14/SPEC.md:807`).

## §2 Required plan edits if any

None.

## §3 Executable/read-only evidence

Read-only count check at HEAD:

```sh
printf 'provider_rs='
find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '
printf '\ncss_l4_template_dirs='
find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
printf '\nskinny_css_runtime_dirs='
find skinny/crates/runtime/src/grammars -maxdepth 1 -type d -name 'css_l4_*' | wc -l | tr -d ' '
printf '\nroot_css_runtime_exists='
test -d crates/core/src/runtime/css_l4 && printf yes || printf no
```

Result:

```text
provider_rs=8
css_l4_template_dirs=7
skinny_css_runtime_dirs=7
root_css_runtime_exists=yes
```

Read-only grep target:

```sh
rg -n "delete|deleting|renam|replacement|rebuild|regen-css|provider/template|runtime" \
  restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md \
  restart/skinny/tranches/sk-v14/SPEC.md \
  restart/skinny/tranches/sk-v14/HANDOFF.md \
  restart/audit/totality/astral/V5/ΩB-skinny-lessons.md
```

Result summary: matches show W5A capability and no-deletion gates at `skv14-W5A-plan.md:18`, `:39`, `:43`, `:70`, `:84`, `:85`; W5B provider/template deletion after W5A at `SPEC.md:715`-`:724`; W6 runtime collapse after W5B at `SPEC.md:775`-`:807`; and the NEW-CH3-V4-01 rebuild-before-delete rule at `ΩB-skinny-lessons.md:44`-`:48`.

No mutating cargo/xtask commands were run for this CH3 artifact.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `:29`-`:35`, `:39`-`:43`, `:63`-`:70`, `:75`, `:77`, `:79`-`:88`, `:92`.
- `restart/skinny/tranches/sk-v14/SPEC.md:637`-`:698`, `:701`-`:753`, `:755`-`:807`.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:158`-`:181`.
- `skinny/REDRESS.md:5103`-`:5118`, `:5171`-`:5193`.
- `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:11`-`:15`, `:29`-`:40`, `:44`-`:52`.
- `restart/skinny/tranches/sk-v14/HANDOFF.md:167`-`:188`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md:105`-`:120`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:211`-`:226`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:72`-`:85`.
