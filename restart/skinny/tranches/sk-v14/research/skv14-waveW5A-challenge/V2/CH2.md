# SK-V14 W5A CH2 V2: Generality

Date: 2026-05-26.
Scope: CH2 review of revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` after V1 folds, focused on Lock 14 generality, generic-crate grammar-name leakage, JSON-only carveouts, request-contract generality, and Sheets/BBNF-self same-contract evidence.
Disposition: ACCEPT.

## §1 Findings

The revised plan remains aligned with SPEC §8's source-consuming generator contract. SPEC requires grammar source plus workspace metadata to enter codegen, required V1 constructs to parse without `grammar_id == css_l4` or equivalent generic-branch behavior, JSON unchanged-output proof, and Sheets/BBNF-self fail-closed or generated-role witnesses through the same parser/contract (`restart/skinny/tranches/sk-v14/SPEC.md:662`-`667`). The revised plan names one grammar-neutral `RuntimeGenerationRequest` path carrying grammar source plus workspace metadata into codegen, parsing required runtime-generation constructs into source facts, routing `regen-css` and JSON checks through that request, and leaving CSS provider/template deletion to W5B (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`).

The V1 folds did not introduce grammar-name generic branches. Lock 14 permits grammar-specific inputs only as grammar source, workspace metadata, and optional declaration crates, while generic crates must carry zero grammar-name branches, zero grammar-named modules, zero grammar-specific public API types, and zero per-grammar feature flags (`restart/locks/LOCKS.md:349`). The revised plan requires CSS L4 runtime-generation constructs to parse as source facts without `grammar_id == "css_l4"` or equivalent profile-specific generic-branch behavior (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`) and pre-blocks grammar-name branches in generic crates (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:119`).

The revised plan does not create a JSON-only carveout. JSON unchanged-output is proven by `cargo xtask check-json`, a before/after whole-directory hash or `git diff --exit-code -- skinny/crates/runtime/src/grammars/json`, and an in-code equality test comparing the new request path to current `emit_from_source("json", source)` (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:41`). The same plan also requires `regen-css` and every `check-css-l4-*` companion to call the new request path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`) and states that JSON `check-json` plus Sheets/BBNF-self tests exercise the same request path as non-CSS proof consumers (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:111`).

The revised parser/request semantics are general rather than CSS-semantic special cases. W5A-B records the actual failing CSS L4 source surface: comma sequences, `>>`, `?w`, projections, `@{...}`, `@ws`, and `@pretty` currently fail or are absent (`restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:157`-`172`). The revised plan consumes that surface as grammar-neutral runtime-generation source facts: import graph metadata, directives, comma sequence, whitespace modifier, discard operators, projection metadata, raw host/value-expression spans, and span capture (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`).

Sheets and BBNF-self remain same-contract, not adjacent witnesses. SPEC requires them to fail closed with named unsupported constructs or emit generated-role witnesses through the same parser/contract (`restart/skinny/tranches/sk-v14/SPEC.md:666`-`667`, `restart/skinny/tranches/sk-v14/SPEC.md:677`). W5A-E says the proof must use the same parser/contract as CSS/JSON rather than an adjacent witness path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:9`). The revised plan requires Sheets and BBNF-self to use the same request path and default to named, source-located fail-closed constructs, with generated-role witnesses allowed only when reusing parser work already needed for CSS (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:42`).

The V1 consolidation's CH2 acceptance is preserved and the V1 required folds strengthened CH2 rather than weakening it. The consolidated packet records CH2 V1 as ACCEPT because the plan respected Lock 14 v+1, avoided plan-level grammar-name branches, and bound Sheets/BBNF-self to the same parser/contract (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11`-`13`). The folds added fail-closed provider/template checks, narrowed parser scope to grammar-neutral source facts plus named fail-closed constructs, and made Sheets/BBNF-self fail-closed witnesses the default W5A proof (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:24`-`30`).

Lock 14's generated-output allowance and provider/template split remain intact. Lock 14 allows grammar names under generated `runtime/src/grammars/<name>/` only when emitted from the rostered generator using grammar source plus workspace metadata, and excludes hand-coded provider enums, generic grammar branches, generic-root proof fixtures, and hand-patched generated files (`restart/locks/LOCKS.md:351`-`364`). The revised plan keeps CSS provider/template paths as explicit non-owner paths (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`-`35`), allows the legacy counts to remain 8 providers and 7 CSS template directories, and forbids increasing, deleting, or renaming them in W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:43`).

Lock 10 is not disturbed by the revisions. Lock 10 rejects grammar-authored `@pratt`, `@simd`, and materialization directives, and keeps `backend_shape` as an auto-detected side-table fact rather than a surface annotation (`restart/locks/LOCKS.md:269`). Its v+1 clause keeps the five `BackendShape` variants gated and rejects new backend shape, directive, or BIR variants without G-Omega (`restart/locks/LOCKS.md:271`-`280`). The revised W5A plan only asks the runtime-generation parser to preserve existing V1 source constructs as source facts (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`) and does not ask grammar authors to annotate Pratt, SIMD, materialization policy, or a new backend shape.

## §2 Remaining required edits if any

None for CH2 V2. Redress still must prove the plan literally: no generic-crate grammar-name branches, no JSON policy leakage into generic CSS/source routing, source and workspace metadata consumed rather than freshness-hashed, and Sheets/BBNF-self evidence produced by the same request path.

## §3 Evidence

Read-only commands used:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md | sed -n '1,260p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH2.md | sed -n '1,260p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md | sed -n '1,320p'
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '620,710p'
nl -ba restart/locks/LOCKS.md | sed -n '250,310p;340,430p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md | sed -n '1,260p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md | sed -n '1,170p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md | sed -n '1,125p'
rg -n "grammar_id|css_l4|json|JSON|same request path|same parser/contract|Sheets|BBNF|RuntimeGenerationRequest|emit_runtime_profile|provider/template|sheets_witness|unexpected token|grammar-name|JSON policy|fail-closed" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH2.md
```

Result: the revised plan keeps one grammar-neutral request path, explicitly blocks `grammar_id == "css_l4"`-style branches, routes JSON/CSS/Sheets/BBNF evidence through the same request, pre-blocks JSON policy leakage and witness reuse, and leaves provider/template deletion to W5B. I did not run redress verification commands such as `cargo xtask regen-css`, `check-css-l4-*`, or `gate-json` because this assignment is a challenge review artifact and those commands may write build or generated artifacts.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:29`-`45`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:61`-`70`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:86`-`104`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:111`-`124`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH2.md:9`-`25`.
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md:11`-`32`.
- `restart/skinny/tranches/sk-v14/SPEC.md:637`-`698`.
- `restart/locks/LOCKS.md:269`-`280`, `restart/locks/LOCKS.md:349`-`397`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-A-regen-source-contract.md:84`-`124`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-B-grammar-parser-constructs.md:157`-`238`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-C-css-companion-emission.md:211`-`239`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-D-json-unchanged-output.md:60`-`153`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-E-sheets-bbnf-witness.md:1`-`147`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-F-lock14-guard-budget.md:1`-`98`.
