# CH2 GENERALITY

Verdict: ACCEPT

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context commit: `5b85f7d5d`.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | Pass: target packet changes only the seven T-P3 proposal artifacts, 170 insertions and 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | Pass: no whitespace output. |
| Extract `3C-locks-v+1-diff.md` to `/tmp/tp3-locks-v3.diff`; `git apply --check /tmp/tp3-locks-v3.diff` | Pass: proposed lock addendum applies cleanly. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the lock-count invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H invariant. |
| Required stale-pattern `rg` over 3A..3F | Pass: no matches; exit 1 is the expected no-match result. |

## Evidence

Lock 14 holds. The live lock requires grammar source, workspace metadata, and optional per-grammar declaration crates as the only legal fleet inputs; generic crates carry no grammar-specific branches, modules, public grammar types, or per-grammar feature flags (`restart/locks/LOCKS.md:349`). It also requires Lock 14 gates over generic crates, generated provider manifests, primitive policy, runtime roots, codegen templates, decision facts, and reports (`restart/locks/LOCKS.md:377`-`390`). V3 carries that boundary into 3A's generated-provider discipline (`restart/audit/totality/p3/3A-architecture-synthesis.md:68`), 3C's D-L14 clause (`restart/audit/totality/p3/3C-locks-crystallisation.md:55`), and 3E's source/metadata-only future-onboarding rule (`restart/audit/totality/p3/3E-grammar-generalisation.md:76`).

No JSON narrowing enters the packet. 3D preserves JSON as scoped same-plane guard evidence, not CSS or fleet closure (`restart/audit/totality/p3/3D-skinny-fold.md:50`-`53`). 3E says the generality story is source-owned generation plus negative-control proof, not JSON success with wider prose (`restart/audit/totality/p3/3E-grammar-generalisation.md:35`-`48`), and explicitly states no JSON narrowing is proposed (`restart/audit/totality/p3/3E-grammar-generalisation.md:50`-`53`). 3A likewise keeps JSON as a guard while demoting CSS until typed CSS provider and same-workload retime proof exist (`restart/audit/totality/p3/3A-architecture-synthesis.md:64`).

The non-JSON receivers are concrete. T-P2 requires CSS L4 and at least one Sheets or BBNF-self receiver without generic-code edits (`restart/audit/totality/p2/2C-grammar-neutrality.md:47`-`55`, `restart/audit/totality/p2/2C-grammar-neutrality.md:72`-`75`). SK-V15 applies that across provider, generator, lowerer, e-graph, CSP, cost, xtask, and gate/report surfaces (`restart/skinny/tranches/sk-v15/SPEC.md:206`-`217`). V3 mirrors it: 3E-D01 requires CSS L4 plus Sheets or BBNF-self for generic surface claims (`restart/audit/totality/p3/3E-grammar-generalisation.md:68`), 3E-D07 and D08 give concrete Sheets and BBNF-self fixtures (`restart/audit/totality/p3/3E-grammar-generalisation.md:74`-`75`), and the proof matrices cover CSS, Sheets, and BBNF-self across provider manifests, typed providers, BackendShape resolver, primitive policy, and future onboarding (`restart/audit/totality/p3/3E-grammar-generalisation.md:80`-`99`).

The packet adds no directive, BIR variant, substrate, public substrate API, retained sidecar, or sixth `BackendShape`. 3C's crystallisation summary and proposed diff both preserve 16 locks and the exact five variants while adding none of those forbidden surfaces (`restart/audit/totality/p3/3C-locks-crystallisation.md:31`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:29`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:40`). The Lock 1 diff keeps `FactStream` output-plane only and rejects retained sidecars, public `UnionTape`, second tape, and runtime regex/DFA substrate without a later G-Omega Lock 1 amendment (`restart/audit/totality/p3/3C-locks-v+1-diff.md:42`). The Lock 10 and Lock 14 clauses gate any sixth shape, new directive, new BIR variant, or generic branch (`restart/audit/totality/p3/3C-locks-v+1-diff.md:56`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:60`).

The five-shape canon is intact in code and proposal text. `BackendShape` contains only `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` (`skinny/crates/ir/src/lib.rs:339`-`345`), and `all_backend_shapes()` returns exactly those five (`skinny/crates/ir/src/cost.rs:333`-`340`). 3A keeps exactly five variants and forbids solving lowerer debt by adding a sixth shape (`restart/audit/totality/p3/3A-architecture-synthesis.md:70`); 3B routes W8/W9 through the all-five gate (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:131`-`133`); 3E's per-grammar matrix covers CSS L4, Sheets, and BBNF-self without adding `FactStream` as a shape (`restart/audit/totality/p3/3E-grammar-generalisation.md:72`).

The residual CH2 open question is acceptable. 3A, 3D, and 3E ask whether Sheets or BBNF-self should land first if only one fits a wave cap (`restart/audit/totality/p3/3A-architecture-synthesis.md:118`; `restart/audit/totality/p3/3D-skinny-fold.md:98`; `restart/audit/totality/p3/3E-grammar-generalisation.md:157`). Those rows do not weaken the requirement: they keep fleet-wide wording blocked until CSS plus a concrete non-CSS receiver lands or an intrinsic-block record is produced.

## Defects

None.
