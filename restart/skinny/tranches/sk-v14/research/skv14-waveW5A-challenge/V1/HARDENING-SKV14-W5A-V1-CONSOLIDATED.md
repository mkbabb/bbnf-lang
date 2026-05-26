# SK-V14 W5A CHALLENGE V1 Consolidated

Date: 2026-05-26.
Scope: Seven-lens review of `restart/skinny/tranches/sk-v14/research/skv14-W5A-plan.md` before W5A redress.
Disposition: REVISE, folded into plan; dispatch V2.

## §1 — Lens Dispositions

| Lens | Disposition | Summary |
|---|---|---|
| CH1 Correctness | REVISE | Verification had observational or inverted gates: forbidden-call grep, broad zero-match test filters, provider/template diff, LOC cap, and full-table maintain. |
| CH2 Generality | ACCEPT | Plan respects Lock 14 v+1, avoids plan-level grammar-name branches, and binds Sheets/BBNF-self to the same parser/contract. |
| CH3 Regression | ACCEPT | Plan does not reopen REDRESS-184/209 and preserves V4 deletion/rebuild ordering. |
| CH4 Cost | REVISE | Plan needed component LOC ledger, executable LOC gate, narrowed source-fact parser scope, and pre-redress cap stop. |
| CH5 Hidden Coupling | ACCEPT | Plan keeps provider/template deletion coupled to W5B and blocks the current profile-only boundary. |
| CH6 Anti-Paper-Close | REVISE | Same-wave consumers are real, but tests/gates needed fail-closed semantics, rejected-patch escrow, and explicit downstream routing. |
| CH7 Overfit-Prune | ACCEPT | Plan blocks P-1..P-7 recurrence, static centralization, fake generated headers, fixture lookup, and scaffold-only proof. |

## §2 — Required Folds Applied

The plan now:

- replaces broad `w5a_` test filters with exact named tests plus nonzero-pass log assertions;
- makes the forbidden `emit_runtime_profile(target.profile)` call-boundary check fail closed;
- makes provider/template count and `A`/`D`/`R` diff checks fail closed;
- adds a component-level W5A cost ledger totaling <=1.0k source/test LOC;
- adds a `git diff --numstat` W5A source/test LOC gate;
- narrows parser scope to grammar-neutral source facts, raw value/host spans, and named fail-closed constructs rather than full CSS semantic generation;
- makes Sheets/BBNF-self fail-closed witnesses the default W5A proof;
- routes full-table maintain through `cargo xtask gate-json --check-results --skv14-existing-results-capture`;
- adds rejected-patch escrow at `/tmp/skv14-waveW5A-rejected.patch`;
- states downstream routing: W5A ADMIT unlocks W5B only; W5A REJECT blocks W5B, W6, W7, W8, W9, and W10 until rerouted.

## §3 — Open Status

V1 has orphan REVISE dispositions by design and therefore cannot converge. The plan fold removes the identified gaps, but the W5A challenge must run V2 and reach zero orphan REVISEs before redress dispatch.

## §4 — Evidence

Read-only / doc-only checks:

```sh
for f in restart/skinny/tranches/sk-v14/research/skv14-waveW5A-challenge/V1/CH{1,2,3,4,5,6,7}.md; do
  rg -m1 '^Disposition:' "$f"
done
git diff --check
```

`git diff --check` produced no whitespace errors after the plan fold.
