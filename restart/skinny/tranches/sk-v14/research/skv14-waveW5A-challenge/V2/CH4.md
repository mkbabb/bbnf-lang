# SK-V14 W5A CHALLENGE V2 CH4 Cost

Date: 2026-05-26.
Scope: CH4 cost review of revised `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` after the V1 fold: component LOC ledger, executable LOC gate, narrowed source-fact parser scope, Sheets/BBNF fail-closed default, pre-redress cap stop, and W5A/W5B/W6 budget separation.
Disposition: ACCEPT.

## §1 Findings

### F1 - Component LOC ledger is sufficient - ACCEPT

V1 CH4 required a component ledger covering parser facts/tests, codegen request/metadata and JSON equivalence, `regen.rs`/`regen_css.rs` routing, temporary Lock 14 guard, Sheets/BBNF fail-closed tests, and counted attribution edits (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md:27`). The revised W5A plan now allocates exactly those components at 300 + 300 + 150 + 120 + 100 + 30 = 1000 LOC (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:49`-`54`). This equals the SPEC W5A ceiling and preserves the W5A/W5B/W6 split: W5A is <=1.0k and cannot borrow from W5B or W6, W5B is <=400 with W5A+W5B <=1.4k, and W6 remains a separate <=2.0k aggregate (`restart/skinny/tranches/sk-v14/SPEC.md:242`-`244`, `restart/skinny/tranches/sk-v14/SPEC.md:680`, `restart/skinny/tranches/sk-v14/SPEC.md:738`). The V5 CH4 hardening split is also preserved: W5A <=1.0k, W5B <=400, W5A+W5B <=1.4k, W6 unchanged, and borrowing/exceeding sub-cap returns REVISE before dispatch (`restart/audit/totality/astral/V5/hardening/CH4.md:17`-`23`). The SK-V14 audit-overfit V5 CH4 pass also keeps the global C-1..C-5 LOC envelopes stable, including the C-1 2.8k-3.4k envelope that contains W5A/W5B/W6 (`restart/skinny/tranches/sk-v14/audit-overfit/hardening/V5/CH4.md:67`-`70`).

### F2 - Executable LOC and separation gates are sufficient - ACCEPT

V1 CH4 required an executable `git diff --numstat` gate for W5A owner paths and a failing source/test cap check (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md:29`). The revised plan names the W5A owner paths and explicitly marks provider/template clusters, root CSS runtime, and grammar CSS sources as non-owner paths except generated `regen-css` output (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:22`-`35`). It then counts `git diff --numstat HEAD` over the grammar source module tree, codegen request/contract files, regen routing files, and Lock 14 guard, prints `W5A source/test LOC delta`, and fails when the delta exceeds 1000 (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:94`-`103`). It also fail-closes provider/template count drift, provider/template add/delete/rename diffs, and forbidden core/grammar CSS edits (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:91`-`104`). Generated runtime output cannot be used to hide source/test cost because it is outside the C-1 count only when produced by `cargo xtask regen-css`, named in REDRESS, byte-diff audited, and included in the revert slice (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:56`; `restart/skinny/tranches/sk-v14/SPEC.md:251`-`257`).

### F3 - Parser scope is now narrowed to source facts - ACCEPT

V1 CH4 required the plan to narrow parsing to source-fact preservation and named unsupported semantics rather than a full CSS L4 semantic generator (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md:31`). The revised plan does that directly: CSS L4 constructs are accepted as runtime-generation source facts, including import metadata, directives, comma sequences, whitespace/discard markers, projection metadata, typed projection metadata, raw host/value-expression spans, and `@{...}` span capture, explicitly "not full CSS semantic generation" and without `grammar_id == "css_l4"` or equivalent generic-branch behavior (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:40`). This matches SPEC Section 8's requirement to make required V1 grammar-source constructs parseable for runtime generation without grammar-id branches (`restart/skinny/tranches/sk-v14/SPEC.md:662`-`663`).

### F4 - Sheets/BBNF default is fail-closed and cost-bounded - ACCEPT

V1 CH4 required Sheets/BBNF-self to default to named fail-closed witnesses through the same request path, with generated-role witnesses allowed only if they reuse CSS-needed parser work and remain in budget (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md:33`). The revised plan adopts that exact default: Sheets and BBNF-self use the same request path and fail closed with named source-located unsupported constructs, while generated-role witnesses are conditional on reusing source-fact parser work and staying inside the component ledger (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:42`). The plan also blocks reused `sheets_witness`/SK-V13 witness JSON and generic parser errors as sufficient evidence (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:123`-`124`). This satisfies SPEC Section 8's same-parser/contract proof requirement for Sheets and BBNF-self (`restart/skinny/tranches/sk-v14/SPEC.md:666`, `restart/skinny/tranches/sk-v14/SPEC.md:677`).

### F5 - Pre-redress cap stop is sufficient - ACCEPT

V1 CH4 required a stop before source edits if the estimated ledger or first implementation slice could not fit <=1.0k and the 90-minute cap (`restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md:37`). The revised plan now says to return REVISE before source edits if the estimate cannot fit the ledger or 90-minute ceiling, rather than borrowing W5B or W6 budget (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:56`). It also binds redress to 75 minutes with a 90-minute ceiling, commit-or-reject at cap, and <=1.0k source/test delta (`restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md:107`). That aligns with SPEC's global rule that budgets are conjunctive with the 90-minute cap and that an over-budget wave must split before dispatch or return REVISE (`restart/skinny/tranches/sk-v14/SPEC.md:251`-`257`).

## §2 Remaining Required Edits

None for CH4 V2.

## §3 Evidence

Read-only checks run from `/Users/mkbabb/Programming/bbnf-lang`:

```sh
nl -ba restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md | sed -n '37,57p;83,109p;126,128p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md | sed -n '25,38p'
nl -ba restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md | sed -n '19,37p'
nl -ba restart/skinny/tranches/sk-v14/SPEC.md | sed -n '235,260p;637,682p;701,740p'
nl -ba restart/audit/totality/astral/V5/hardening/CH4.md | sed -n '1,30p'
nl -ba restart/skinny/tranches/sk-v14/audit-overfit/hardening/V5/CH4.md | sed -n '47,70p'
sed -n '49,54p' restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md | rg -o '<=[0-9]+' | tr -d '<=' | awk '{total += $1} END { print "ledger_total=" total }'
```

The ledger arithmetic command returned `ledger_total=1000`. A scoped `git status --short` before writing this file showed pre-existing untracked `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V2/CH7.md`; `CH4.md` did not exist before this artifact was written.

## §4 Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH4.md`
- `restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/HARDENING-SKV14-W5A-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/audit/totality/astral/V5/hardening/CH4.md`
- `restart/skinny/tranches/sk-v14/audit-overfit/hardening/V5/CH4.md`
