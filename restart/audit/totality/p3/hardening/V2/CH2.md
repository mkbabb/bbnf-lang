# CH2 GENERALITY

Verdict: ACCEPT

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`).
Context commit: `d1d073a50`.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3` | Clean target packet shape: 7 T-P3 artifacts changed, 287 insertions, 206 deletions. |
| `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3` | Passes, no whitespace output. |
| Extract `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v2.diff`; `git apply --check /tmp/tp3-locks-v2.diff` | Passes, proposed lock addendum applies cleanly. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H invariant. |
| Required stale-pattern `rg` over 3A..3F | No matches; exit 1 is the expected no-match result for this check. |

## Evidence

Lock 14 holds. The live lock requires grammar source, workspace metadata, and optional per-grammar declaration crates as the only legal fleet inputs, with no grammar-specific branches in generic crates (`restart/locks/LOCKS.md:349`). It also requires same-wave Lock 14 gates over generic crates, generated provider manifests, primitive policy, runtime roots, codegen templates, decision facts, and reports (`restart/locks/LOCKS.md:377`-`390`). V2 carries that boundary into the proposed lock diff: generated code may consume provider manifests and generated facts, but not `RuntimeGenerationMode`, profile arrays, CSS profile matches, JSON/CSS runtime families, role mining, grammar switches, or generic-crate grammar branches (`restart/audit/totality/p3/3C-locks-v+1-diff.md:59`).

No JSON narrowing enters the packet. 3D preserves JSON only as a scoped same-plane guard baseline and explicitly refuses to use it as CSS or arbitrary-grammar closure (`restart/audit/totality/p3/3D-skinny-fold.md:48`). 3E states the generality story is source-owned generation plus negative-control proof, not wider prose from JSON success (`restart/audit/totality/p3/3E-grammar-generalisation.md:33`-`46`). 3A keeps JSON as a 51-row guard baseline while demoting CSS until W5 typed provider and W6 same-workload retime evidence exist (`restart/audit/totality/p3/3A-architecture-synthesis.md:61`).

The non-JSON receivers are concrete. 3E requires CSS L4 plus Sheets or BBNF-self for generic surface claims, backed by SK-V15's provider/generator/lowerer/egraph/CSP/cost/xtask/gate receiver matrix (`restart/audit/totality/p3/3E-grammar-generalisation.md:66`; `restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`). Its proof matrix names CSS L4 positive receivers and Sheets or BBNF-self negative controls for provider manifests, CSS typed provider, BackendShape resolver, primitive policy, and future onboarding (`restart/audit/totality/p3/3E-grammar-generalisation.md:80`-`86`). Its per-grammar matrix covers CSS tokens/selectors/values, Sheets formula/reference/function/operator forms, and BBNF-self grammar/directive forms (`restart/audit/totality/p3/3E-grammar-generalisation.md:92`-`97`). The grounding evidence agrees that JSON-only, CSS-only, sidecar, and generic-branch routes are refuted, and the admissible route is CSS L4 plus at least one Sheets or BBNF-self receiver without generic-code edits (`restart/audit/totality/p2/2C-grammar-neutrality.md:35`-`55`, `restart/audit/totality/p2/2C-grammar-neutrality.md:72`-`75`, `restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`).

The packet adds no directive, BIR variant, substrate, public substrate API, retained sidecar, or sixth `BackendShape`. 3C's proposed diff says the addendum preserves 16 locks and the exact five variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, adding none of those forbidden surfaces (`restart/audit/totality/p3/3C-locks-v+1-diff.md:28`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:39`). The Lock 1 clause keeps `FactStream` as output-plane language only and rejects retained cursor/list/class-column/sidecar, public `UnionTape`, second tape, and runtime regex/DFA substrate unless G-Omega first amends Lock 1 (`restart/audit/totality/p3/3C-locks-v+1-diff.md:41`). The Lock 10 and Lock 14 clauses explicitly gate any sixth shape, new directive, or new BIR variant (`restart/audit/totality/p3/3C-locks-v+1-diff.md:55`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:59`). 3E independently states no JSON narrowing, grammar switch, new directive, new BIR variant, new substrate, or sixth `BackendShape` is proposed (`restart/audit/totality/p3/3E-grammar-generalisation.md:48`-`51`).

The five-shape canon is intact in code and in the target packet. The Rust enum contains only `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` (`skinny/crates/ir/src/lib.rs:339`-`345`), and `all_backend_shapes()` returns exactly those five (`skinny/crates/ir/src/cost.rs:333`-`340`). T-P2 requires all-five activation without adding a sixth shape (`restart/audit/totality/p2/2D-cost-model.md:47`-`53`, `restart/audit/totality/p2/2D-cost-model.md:62`-`68`). 3A and 3B carry the same boundary into W8/W9 rather than opening a new shape route (`restart/audit/totality/p3/3A-architecture-synthesis.md:67`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:131`-`132`).

The sidecar/substrate boundary is preserved. Live Lock 1 rejects retained class/mask streams, parser-owned cursor/list state, public substrate APIs, `UnionTape`, and second tape without a G-Omega amendment (`restart/locks/LOCKS.md:118`-`126`). ARCH classifies fact streams as output-plane products, not a sixth `BackendShape` or retained substrate (`restart/ARCHITECTURE.md:1777`-`1798`). SK-V15 repeats the hidden-coupling reject list for sidecars, public `UnionTape`, retained streams, public substrate API, alternate projection, and new/sixth shape (`restart/skinny/tranches/sk-v15/SPEC.md:147`-`153`, `restart/skinny/tranches/sk-v15/SPEC.md:233`-`244`). V2 mirrors that in 3D-D08 and 3A-D12 (`restart/audit/totality/p3/3D-skinny-fold.md:64`; `restart/audit/totality/p3/3A-architecture-synthesis.md:71`).

The V1 CH2 accepted ground is not regressed. V1 CH2 already accepted Lock 14, no JSON narrowing, no forbidden surface additions, and CSS plus Sheets/BBNF-self proof (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:28`). V2 records that CH2 ground as carried (`restart/audit/totality/p3/3D-skinny-fold.md:20`; `restart/audit/totality/p3/3E-grammar-generalisation.md:20`) while folding the CH5 regex/substrate repairs into 3A and 3C (`restart/audit/totality/p3/3A-architecture-synthesis.md:22`-`24`; `restart/audit/totality/p3/3C-locks-crystallisation.md:21`-`22`).

## Defects

None.
