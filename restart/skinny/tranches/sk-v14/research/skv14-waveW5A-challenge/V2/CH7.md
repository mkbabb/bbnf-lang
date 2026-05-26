# SK-V14 W5A CH7 V2: Overfit-Prune

Date: 2026-05-26.
Scope: CH7 review of revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` after V1 folds.
Disposition: ACCEPT.

## §1 — Findings

The V1 fold strengthens the P-6 guard rather than weakening it. The revised plan keeps the single `RuntimeGenerationRequest` intervention, forbids grammar-name branches, and adds fail-closed checks for provider/template counts and add/delete/rename diffs (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:39`-`45`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:89`-`92`). This remains aligned with the SK-V14 P-6 requirement that W5A prove one grammar-agnostic source-consuming generator before W5B deletion (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:139`-`147`).

The plan no longer risks CH7 paper closure through broad test names. It now names exact parser/codegen tests and follows each command with a log assertion requiring at least one passing test (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:64`-`73`). That blocks the scaffold-only proof pattern described by S-P0 CH7 (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:87`-`89`).

The parser scope is narrowed enough to avoid CSS-only overfit while preserving source materiality. The plan requires CSS constructs to parse as source facts, raw host/value-expression spans, and named fail-closed constructs rather than full CSS semantic generation (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`-`42`). This reduces the risk of a hidden fixture-like or handwritten template recurrence.

The plan still blocks P-1 fake generated headers and hand-written templates. `regen-css`, the seven CSS checks, `check-json`, and the gate-json maintain check are production consumers (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:74`-`85`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:112`), while static centralization and fake evidence routes remain pre-blocked (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:116`-`123`).

## §2 — Remaining Required Edits

None for CH7 V2.

## §3 — Evidence

Read-only commands:

```sh
rg -n "RuntimeGenerationRequest|source facts|test result: ok|Static centralization|sheets_witness" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
rg -n "P-6|P-1|P-7" restart/skinny/tranches/sk-v14/SYNTHESIS.md
rg -n "CH7|SCAFFOLD-ONLY|generated" restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md
```

The revised plan contains the expected source-request, fail-closed, and pre-blocked-route language.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md`
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`
