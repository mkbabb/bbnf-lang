# SK-V14 W5A CH7: Overfit-Prune

Date: 2026-05-26.
Scope: CH7 review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` against SK-V14 P-1..P-7 and S-P0 overfit-prune criteria.
Disposition: ACCEPT.

## §1 — Findings

The plan directly targets the P-6 recurrence instead of centralising it under a new name. SK-V14 SYNTHESIS defines P-6 as per-grammar provider modules in generic codegen and says W5A must prove one grammar-agnostic generator contract before W5B deletes provider/template clusters (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:139`-`147`). The W5A plan selects a single `RuntimeGenerationRequest` path and pre-blocks static centralisation (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:81`).

The plan blocks P-1 fake generated output rather than relying on headers. S-P0 CH7 requires generated code to be grammar-derived and round-trip verified, never hand-written under a generated header (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:79`-`89`). The W5A plan requires `cargo xtask regen-css`, all seven CSS check companions, and no provider/template deletion in W5A (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:52`-`59`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:77`-`87`).

The plan does not reopen P-3 tiny-fixture or P-4 gate-relabel patterns. W5A is not an admit wave and does not claim Mbps admission; the only planned consumers are source/codegen checks and generated-runtime round trips (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:37`-`45`, `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:49`-`60`). SK-V14 P-3 and P-4 apply to tiny benchmark fixtures and gate-only admits (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:125`-`134`), neither of which is selected here.

The plan preserves non-JSON generality and blocks CSS-only overfit at the plan level. It requires the parser/contract to accept CSS L4 constructs without `grammar_id == "css_l4"` branches and requires Sheets/BBNF-self evidence through the same request path (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`-`42`). That satisfies the CH7 demand that new code not be a grammar-specific hidden path (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:81`-`82`).

The plan blocks scaffold-only proof. S-P0 CH7 says scaffold-only research without source wiring cannot count as an admit (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:87`-`89`). The W5A plan requires `regen-css` and all seven check companions to exercise the source-consuming request in the W5A redress commit (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:77`).

## §2 — Required Plan Edits

None for CH7 V1. Redress must still prove that parser support represents `->`, `@{...}`, `?w`, `<<`, and `>>` as source facts rather than ignoring them, because raw-token preservation without consumption would become P-6 static centralisation in a different form.

## §3 — Executable / Read-Only Evidence

```sh
rg -n "P-1|P-2|P-3|P-4|P-5|P-6|P-7" restart/skinny/tranches/sk-v14/SYNTHESIS.md
rg -n "CH7|generated header|SCAFFOLD" restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md
rg -n "Static centralization|grammar_id|sheets_witness|regen-css|check-css-l4" restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md
```

Read-only result: the plan names the W5A source-consuming request, forbids static centralisation, forbids grammar-name branches, routes same-wave consumers through `regen-css` and the seven CSS checks, and blocks `sheets_witness` reuse.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SYNTHESIS.md:109`-`151`
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:79`-`89`
- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:18`-`92`
