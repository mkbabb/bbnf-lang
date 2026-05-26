# SK-V14 W5A CHALLENGE V2 - CH3 Regression

Date: 2026-05-26.

Scope: CH3 Regression review of the revised W5A plan after the V1 folds, against V1 CH3, the V1 consolidated packet, SPEC Section 8, DISPATCH NEW-CH3-V4-01, REDRESS-184, and REDRESS-209. Focus: whether the folded plan still avoids deleted-before-rebuild recurrence and provider/template/runtime deletion cycles.

Disposition: ACCEPT.

## §1 Findings

1. The V1 fold did not introduce a CH3 regression. V1 consolidated records CH3 as ACCEPT and lists folds that strengthen observability, LOC budgeting, fail-closed checks, rejected-patch escrow, and downstream routing rather than moving deletion into W5A (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11-17`, `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:19-32`). The revised plan still defines W5A as one `RuntimeGenerationRequest` path that carries grammar source plus workspace metadata and leaves CSS provider/template deletion to W5B (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`).

2. REDRESS-184 remains closed. REDRESS-184 rejected deleting the seven CSS L4 provider modules and template directories before the live `regen-css` path stopped compiling through those providers (`skinny/REDRESS.md:5105-5112`). The revised W5A plan marks the CSS provider modules, CSS template directories, root CSS runtime, and grammar CSS source as non-owner paths (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29-35`), requires no provider/template deletion or rename in W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`), and pre-blocks deleting or renaming providers/templates before W5B (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:120`).

3. REDRESS-209 remains closed. REDRESS-209 rejected monolithic W5 because it combined provider/template deletion with an unproven source-consuming generator: current HEAD still used `emit_runtime_profile(target.profile)`, the static provider/template mesh remained load-bearing, source and metadata were freshness-only, and CSS L4 grammar-source constructs were rejected (`skinny/REDRESS.md:5173-5183`). Its supersession routes current dispatch to W5A source-consuming capability followed by W5B deletion after W5A admits (`skinny/REDRESS.md:5189-5193`). The W5A plan directly targets that gap: `regen-css` and all seven companions must call the new request path, CSS L4 source constructs must parse as source facts without `grammar_id == "css_l4"` branching, and static centralization remains pre-blocked (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39-43`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:117`).

4. NEW-CH3-V4-01 is satisfied. DISPATCH requires CH3 to grep delete-target/rebuild-capability pairs and assert rebuild capability precedes deletion (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:173-179`); the V5 lesson states the rebuild-capability wave must precede deletion unless the same wave first proves the replacement load-bearing (`restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:44-48`). SPEC makes W5A the rebuild-capability wave through source+metadata codegen, parser construct support, `regen-css`, all seven CSS companions, JSON proof, Sheets/BBNF-self proof, and a temporary no-deletion guard (`restart/skinny/tranches/sk-v14/SPEC.md:654-668`, `restart/skinny/tranches/sk-v14/SPEC.md:670-680`). SPEC then makes W5B deletion conditional on W5A admission and proof that profiles/companions already pass through the W5A path (`restart/skinny/tranches/sk-v14/SPEC.md:713-724`).

5. Runtime deletion/rebuild ordering is not reopened. W5A excludes `crates/core/src/runtime/css_l4/`, excludes skinny CSS runtime directories except generated `regen-css` output, and adds a root CSS runtime/source no-diff gate (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:33-34`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:104`). SPEC keeps W6 root-runtime collapse conditional on W5B admission, with W6.0 owning the destructive CSS root-runtime gate only after the W5A generator path and W5B deletion are load-bearing (`restart/skinny/tranches/sk-v14/SPEC.md:773-807`).

## §2 Remaining Required Edits If Any

None for CH3 V2.

## §3 Evidence

Read-only checks run at HEAD:

```sh
find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '
find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
find skinny/crates/runtime/src/grammars -maxdepth 1 -type d -name 'css_l4_*' | wc -l | tr -d ' '
test -d crates/core/src/runtime/css_l4 && printf yes || printf no
rg -n "delete|delet|renam|rebuild|replacement|provider/template|runtime|regen-css|RuntimeGenerationRequest|source-consuming" \
  restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md \
  restart/skinny/tranches/sk-v14/SPEC.md \
  restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md \
  skinny/REDRESS.md \
  restart/audit/totality/astral/V5/ΩB-skinny-lessons.md
```

Results: provider module count is `8`, CSS L4 template directory count is `7`, skinny CSS runtime directory count is `7`, and root `crates/core/src/runtime/css_l4` still exists. The grep surfaced W5A rebuild-capability lines, W5B/W6 deletion ownership, REDRESS-184/209 rejection text, and NEW-CH3-V4-01 ordering text; no W5A-owned deletion/rebuild inversion was found.

No mutating cargo, xtask, git, or filesystem cleanup commands were run for this CH3 artifact.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `:29-35`, `:39-43`, `:86-104`, `:109-121`, `:128`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH3.md:1-21`, `:47-59`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11-17`, `:19-36`.
- `restart/skinny/tranches/sk-v14/SPEC.md:637-698`, `:701-753`, `:773-807`.
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:158-181`.
- `skinny/REDRESS.md:5103-5118`, `:5171-5193`.
- `restart/audit/totality/astral/V5/ΩB-skinny-lessons.md:11-15`, `:29-40`, `:44-52`.
