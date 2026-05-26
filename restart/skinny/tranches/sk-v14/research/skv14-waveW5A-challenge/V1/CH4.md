# SK-V14 W5A CHALLENGE V1 CH4 Cost

Date: 2026-05-26.
Scope: CH4 cost review of `skv14-W5A-plan.md`: <=1.0k C-1 part-A source/test LOC cap, W5A/W5B/W6 budget separation, hard-cap realism, and parser/request/guard scope size.
Disposition: REVISE.

## §1 Findings

### F1 - Budget separation is present and binding - ACCEPT

SPEC binds W5A to `<=1.0k C-1 part-A source/test LOC` and forbids borrowing from W5B or W6 (`restart/skinny/tranches/sk-v14/SPEC.md:242`). It separately binds W5B to `<=400` and W5A+W5B to `<=1.4k` (`restart/skinny/tranches/sk-v14/SPEC.md:243`), and W6 to `<=2.0k C-1 part-B` across the root-runtime sub-waves (`restart/skinny/tranches/sk-v14/SPEC.md:244`). SPEC also says generated outputs do not consume the source LOC budget only if named, diff-audited, and included in the revert slice; any wave exceeding LOC or the 90-minute cap must split or return REVISE (`restart/skinny/tranches/sk-v14/SPEC.md:251`, `restart/skinny/tranches/sk-v14/SPEC.md:255`, `restart/skinny/tranches/sk-v14/SPEC.md:256`). V5 CH4 hardening carries the same split: W5A <=1.0k, W5B <=400, W5A+W5B <=1.4k, W6 <=2.0k, and W6 borrowing or sub-cap overflow returns REVISE before dispatch (`restart/audit/totality/astral/V5/hardening/CH4.md:15`, `restart/audit/totality/astral/V5/hardening/CH4.md:17`, `restart/audit/totality/astral/V5/hardening/CH4.md:22`). The W5A plan repeats the no-borrow cap (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:44`) and excludes CSS provider/template paths and `crates/core/src/runtime/css_l4/` from W5A ownership (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:34`).

### F2 - The <=1.0k cap is asserted but not operationally budgeted - REVISE

The selected W5A intervention combines one request path, grammar source plus workspace metadata, runtime-generation construct parsing, `regen-css`, JSON checks, and W5B deferral in one slice (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`). Its owner paths span the grammar parser, codegen entrypoint, a new `grammar_provider.rs`, both regen helpers, Lock 14 baseline, and final attribution files (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:22`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:27`). The falsifiability gate then requires source-consuming CSS routing, parser support for import metadata, `@token`, `@ws`, `@pretty`, comma sequencing, `?w`, `>>`, `<<`, `->`, typed projections, and `@{...}` capture, plus JSON equivalence, Sheets/BBNF witnesses, and provider/template guards (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:42`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`). The plan only says the delta must remain <=1.0k (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:44`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:73`); it does not allocate that cap across parser, request/metadata contract, xtask routing, Lock 14 guard, and tests. CH4 cannot verify cap realism until the plan adds a per-component LOC ledger and an executable count gate.

### F3 - Parser scope is too broad unless narrowed to source facts and fail-closed witnesses - REVISE

The CSS L4 source surface is materially larger than the plan's single-line cap statement. The W5A parser research counted 454 `->` projections, 223 comma sequence hits, 92 `?w` hits, 51 `>>` hits, 39 `<<` hits, 25 imports, plus `@ws`, `@pretty`, `@token`, and span captures in CSS L4 source (`restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:76`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:96`). The same artifact recommends import graph support, directive metadata, comma sequencing, `?w`, discard operators, span capture, and mapped factors with raw value-expression/type metadata (`restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:211`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:217`). The companion research explicitly warns that a full CSS L4 semantic generator is larger than W5A's 1.0k cap and that W5A should target a minimal parser/contract plus provenance proof (`restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md:277`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md:279`). Sheets/BBNF research similarly recommends fail-closed witnesses for W5A and says full Sheets plus BBNF-self generation likely exceeds the cap (`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:107`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:143`). The plan must narrow "parse required constructs" to source-fact preservation and named unsupported semantics, not full semantic generation.

### F4 - Hard-cap realism needs an explicit stop rule tied to the narrowed scope - REVISE

The plan carries a 75-minute redress target with a 90-minute ceiling and says to commit or reject at cap (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:73`). That is directionally correct, but the verification list includes grammar tests, codegen tests, `check-json`, `regen-css`, all seven CSS companions, and `gate-json --check-results` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:48`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:60`). SPEC says a planned implementation that cannot fit the 90-minute cap must split before dispatch or return REVISE (`restart/skinny/tranches/sk-v14/SPEC.md:271`, `restart/skinny/tranches/sk-v14/SPEC.md:274`). W5A-F already warns that broad Lock 14 allowance can hide budget creep and that if parser support for `->` and `@{...}` cannot fit inside W5A, the plan should return REVISE rather than move deletion forward (`restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:66`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:83`). The plan needs to make that stop rule executable before redress starts.

## §2 Required Plan Edits

1. Add a `W5A Cost Ledger` section with component budgets totaling <=1.0k source/test LOC. It must budget at least: grammar/runtime-source parser facts and tests; codegen request/metadata contract and JSON equivalence test; `regen.rs`/`regen_css.rs` routing; temporary Lock 14 guard; Sheets/BBNF named fail-closed tests; and final hand-written attribution edits if counted by SPEC.

2. Add an executable LOC gate. Minimum shape: capture `git diff --numstat` for W5A owner paths, separately report hand-edited source/test LOC and generated-output LOC, and fail if source/test delta exceeds 1.0k or if generated outputs are unnamed/not diff-audited.

3. Narrow parser scope in the plan: W5A parses or preserves source facts needed to prove source consumption; it does not implement a full CSS L4 semantic generator. `->` value expressions and host calls should be preserved as raw/source-located facts or fail closed by named construct where semantics are not implemented.

4. Make Sheets/BBNF-self default to named fail-closed witnesses through the same request path. Generated-role witnesses should be selected only if they reuse parser work already required for CSS and do not break the component ledger.

5. Keep W5B/W6 separation as an executable gate: provider/template counts may remain 8/7 in W5A, no provider/template `D`/`R`/unplanned `A`, no `crates/core/src/runtime/css_l4/` edits, and no W6 root-runtime collapse work.

6. Add a pre-redress cap stop: if the estimated component ledger or first implementation slice cannot fit <=1.0k and the 90-minute cap, return REVISE before source edits rather than spending W5B or W6 budget.

## §3 Executable/read-only evidence

Read-only commands were run from `/Users/mkbabb/Programming/bbnf-lang` at HEAD `1dd390065`.

```sh
git rev-parse --short HEAD
# 1dd390065
```

```sh
git status --short
# Pre-existing unrelated dirty SK-V12/SK-V13 research JSON files and one untracked prompt were present.
# The W5A challenge directory was untracked; this agent wrote only CH4.md.
```

```sh
for p in '@import' '->' '@{' '>>' '<<' '?w' ',' '@token' '@ws' '@pretty'; do
  printf '%-8s ' "$p"
  rg -n -F -g '*.bbnf' -- "$p" grammar/css/l4 | wc -l | tr -d ' '
done
# @import  25
# ->       454
# @{       3
# >>       51
# <<       39
# ?w       92
# ,        223
# @token   2
# @ws      1
# @pretty  8
```

```sh
wc -l skinny/crates/grammar/src/lib.rs \
  skinny/crates/codegen/src/lib.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs
#  427 skinny/crates/grammar/src/lib.rs
#  808 skinny/crates/codegen/src/lib.rs
#   95 skinny/xtask/src/regen.rs
#  148 skinny/xtask/src/regen_css.rs
# 2380 skinny/crates/bbnf-bench/src/lock14_baseline.rs
# 3858 total
```

```sh
printf 'providers='
find skinny/crates/codegen/src -maxdepth 1 -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l | tr -d ' '
printf '\ncss_template_dirs='
find skinny/crates/codegen/src -maxdepth 1 -type d -name 'css_l4_*_templates' | wc -l | tr -d ' '
printf '\nruntime_css_dirs='
find skinny/crates/runtime/src/grammars -maxdepth 1 -type d -name 'css_l4_*' | wc -l | tr -d ' '
# providers=8
# css_template_dirs=7
# runtime_css_dirs=7
```

No cargo verification commands were run for this CH4 review; the assignment was a read-only challenge artifact and no source files were edited.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/audit/totality/astral/V5/hardening/CH4.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md`
- `skinny/crates/grammar/src/lib.rs`
- `skinny/crates/codegen/src/lib.rs`
- `skinny/xtask/src/regen.rs`
- `skinny/xtask/src/regen_css.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `grammar/css/l4/*.bbnf`
