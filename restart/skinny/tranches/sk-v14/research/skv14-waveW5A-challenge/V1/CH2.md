# SK-V14 W5A CH2: Generality

Date: 2026-05-26.
Scope: CH2 review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` for Lock 14 v+1 generality, generic-crate grammar-name leakage, non-JSON transfer, and Sheets/BBNF-self same-contract proof.
Disposition: ACCEPT.

## §1 — Findings

The plan matches SPEC §8's W5A contract instead of narrowing to a CSS-only or JSON-only path. SPEC requires grammar source plus workspace metadata to enter codegen, required V1 constructs to parse without `grammar_id == css_l4`, JSON unchanged-output proof, and Sheets/BBNF-self fail-closed or generated-role witnesses through the same parser/contract (`restart/skinny/tranches/sk-v14/SPEC.md:662`-`667`). The plan selects one `RuntimeGenerationRequest` path carrying grammar source plus metadata into codegen and explicitly routes `regen-css`, JSON checks, and parser facts through that request (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`).

The plan respects Lock 14 v+1 at the plan level. Lock 14 permits only grammar source, workspace metadata, and optional declaration crates as grammar-specific inputs, while generic crates carry zero grammar-name branches, zero grammar-named modules, zero grammar-specific public types, and zero per-grammar feature flags (`restart/locks/LOCKS.md:349`). The plan's falsifiability gate requires CSS L4 constructs to parse without `grammar_id == "css_l4"` or equivalent profile-specific generic branches (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`), then pre-blocks grammar-name branches and JSON policy leakage in generic crates (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:83`-`86`).

The generated-output allowance is handled with the correct W5A/W5B split. Lock 14 v+1 allows grammar names under generated `runtime/src/grammars/<name>/` only when emitted from the rostered generator using grammar source plus workspace metadata, and excludes hand-coded provider enums, generic grammar branches, proof fixtures routed through generic roots, and hand-patched generated files (`restart/locks/LOCKS.md:351`-`364`). W5A keeps existing provider/template files as non-owner legacy surfaces and forbids adding, deleting, or renaming them (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`-`35`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`), matching SPEC's allowance that provider count may remain non-zero in W5A but no new provider/template directory may appear (`restart/skinny/tranches/sk-v14/SPEC.md:678`).

The non-JSON proof is concrete, not a slogan. W5A-B shows the current parser rejects CSS L4 `,`, `>>`, `?w`, `->`, `@{...}`, `@ws`, and `@pretty` cases (`restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:157`-`172`) and recommends a grammar-neutral parser surface for import graphs, directives, comma concatenation, `?w`, discard operators, span capture, and mapped factors (`restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:211`-`225`). The plan imports that surface directly in its gate (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`).

Sheets and BBNF-self are routed through the same parser/contract, which is the load-bearing CH2 point. W5A-E says the Sheets/BBNF proof must use the same parser/contract as CSS/JSON, not an adjacent witness path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:9`). The plan requires Sheets and BBNF-self to use the same request path and either emit generated-role witnesses or fail closed with named source-located unsupported constructs (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:42`), and its same-wave consumer line says JSON `check-json` plus Sheets/BBNF-self tests exercise the same request path as non-CSS proof consumers (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:77`). The plan also blocks the two known paper-close routes: reusing `sheets_witness` / SK-V13 witness JSON and accepting generic parser errors like `unexpected token '-'` as sufficient evidence (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:87`-`88`).

Lock 10 is not disturbed. Lock 10 keeps Pratt/SIMD/materialization decisions auto-detected, rejects grammar-authored `@pratt`/`@simd`/materialization directives, and keeps `backend_shape` as a side-table fact (`restart/locks/LOCKS.md:269`). Its v+1 clause keeps the five `BackendShape` variants gated and rejects new backend shape, directive, or BIR variants without G-Omega (`restart/locks/LOCKS.md:271`-`278`). W5A only asks the runtime-generation parser to preserve/consume existing V1 source constructs as facts (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`); it does not ask authors to annotate Pratt, SIMD, or materialization policy and does not add a BackendShape.

The challenge obligation itself is satisfied. DISPATCH §4.3 defines CH2 as "does the intervention respect Lock 14 v+1? Does it generalise to non-JSON grammars?" (`restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:169`-`173`), and SKINNY-TRIUMVIRATE §4 defines the same CH2 lens for adversarial plan review (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112`-`120`). On that lens, the W5A plan has no CH2-blocking gap.

## §2 — Required Plan Edits

None for CH2 V1. Redress must still prove the gates literally: no generic-crate grammar-name branches, no JSON policy leakage into generic source routing, source/metadata consumed rather than freshness-hashed, and Sheets/BBNF-self evidence produced by the same request path.

## §3 — Executable / Read-Only Evidence

```sh
git status --short
test -e restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH2.md; printf '%s\n' $?
find restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge -maxdepth 3 -type f -print
rg -n "grammar-neutral|source-consuming|same request path|same parser/contract|grammar_id|css_l4|Sheets|BBNF-self|emit_runtime_profile|provider/template|unexpected token" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '637,698p'
nl -ba restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md | sed -n '158,180p'
nl -ba restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md | sed -n '112,123p'
nl -ba restart/locks/LOCKS.md | sed -n '269,307p;349,420p'
```

Read-only result: unrelated dirty files pre-existed outside W5A; `CH2.md` did not exist before this write; the challenge directory contained only `CH7.md`; the plan and W5A research contain the expected Lock 14, non-JSON, and same-contract witnesses. I did not run `cargo xtask regen-css`, `check-css-l4-*`, or `gate-json` because those are redress verification commands and may write generated/build artifacts.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`-`92`
- `restart/skinny/tranches/sk-v14/SPEC.md:637`-`698`
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:158`-`180`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:112`-`123`
- `restart/locks/LOCKS.md:269`-`307`, `restart/locks/LOCKS.md:349`-`420`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md:84`-`124`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:157`-`238`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md:211`-`239`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md:60`-`153`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:1`-`147`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:1`-`98`
