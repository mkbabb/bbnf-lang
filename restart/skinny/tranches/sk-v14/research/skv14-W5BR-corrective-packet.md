# SK-V14 W5BR Corrective Packet

Date: 2026-05-26.
Source wave: W5B PRUNE-3B.
Disposition: Pass Omega V6 input.

## Rejection

W5B is rejected under the current SPEC. W5A admitted a source-consuming
`RuntimeGenerationRequest` boundary and moved `regen-css`, CSS companion checks,
and JSON checks to that request. It did not build a provider-free runtime
generator body. At HEAD, `grammar_provider.rs` still delegates to
`render_runtime_profile`, `grammar_profile.rs` still owns `RuntimeProvider`, and
`lib.rs` still calls CSS/JSON provider modules.

Deleting providers/templates now is therefore not an abrogation of the old mesh;
it is an attempt to remove load-bearing code without a replacement.

## Dependency Cycle

Current graph:

```text
W5A source-consuming request boundary
  -> W5B delete provider/template mesh
  -> W6 root-runtime collapse
```

Actual dependency:

```text
W5A request boundary
  -> provider-free generator body exists and is same-wave consumed
  -> provider/template deletion + Lock 14 baseline close
  -> W6 root-runtime collapse
```

The missing middle step is not editorial. It is the code that turns grammar
source + workspace metadata into runtime bytes without static providers.

## Proposed Omega V6 Amendment

Split the current W5B obligation:

| Wave | Proposed scope | Entry gate | Exit gate |
|---|---|---|---|
| W5B-GEN | Provider-free runtime generator body. Replace `render_runtime_profile` and the `RuntimeProvider` roster with one request-driven generator path. CSS L4 and JSON bytes must come from grammar source + workspace metadata, not provider modules, per-grammar templates, or committed generated output. | W5A admitted. | `regen-css`, seven CSS companions, `check-json`, W5A JSON/Sheets/BBNF proof carry, and strengthened Lock 14 generic-codegen scan pass with provider files still tolerated only as unused legacy residue. |
| W5C-DELETE | Delete eight legacy provider files, seven CSS template dirs, any retired JSON template residue, and close the post-W5 Lock 14 baseline. | W5B-GEN admitted. | Provider count excluding `grammar_provider.rs` is 0; CSS template dirs are 0; post-W5 Lock 14 baseline passes; all same-wave consumers pass. |

W6 becomes conditional on W5C-DELETE close. W7 and W8/W9/W10 remain globally
blocked until PRUNE-1 through PRUNE-5 close.

## Grep Repair

Replace the current W5B generic-crate grep command with ripgrep-correct,
production-scoped checks. Candidate gates:

```sh
cd skinny && ! rg -nU 'match\s+[^{]+\{[^}]*\b(Json|CssL4\w*|Bbnf\w*|GoogleSheets\w*)\b\s*=>' crates/{codegen,runtime,passes,bbnf,grammar}/src
cd skinny && ! rg -n '\b(RuntimeProvider|GrammarProfile|CssL4\w*|GoogleSheets\w*|Bbnf\w*)\b' crates/codegen/src --glob '!**/tests/**'
```

The exact CRUD patch can refine the production/test split, but it must not use
`rg -E` as grep-style extended regex and must not scan unrelated root
historical audit tags as W5B failures.

## Risk Class

HIGH. This is a wave-graph and cap/cost correction. It does not require a locks
amendment: Lock 14 already requires the provider-free shape. It does require
MASTER-PLAN/SPEC/SYNTHESIS/ORCHESTRATOR/DISPATCH/HANDOFF amendments before
implementation can continue.

## Next Action

Dispatch Pass Omega V6. Inputs:

- REDRESS-210.
- `skv14-W5B-plan.md`.
- W5B research artifacts A-D.
- W5A redress commit `286233fa2`.
- Pass Omega V5 signoff, because V6 corrects the W5A/W5B split created there.
