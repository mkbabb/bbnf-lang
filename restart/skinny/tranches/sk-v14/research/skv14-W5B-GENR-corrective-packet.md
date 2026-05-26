# SK-V14 W5B-GENR Corrective Packet

Date: 2026-05-26.
Source wave: W5B-GEN PRUNE-3B.
Disposition: Pass Omega V7 input.

## Rejection

W5B-GEN is rejected under the current SPEC. W5A admitted the
`RuntimeGenerationRequest` boundary, and Pass Omega V6 correctly split
provider-free generation before provider/template deletion. However, the
provider-free generator body itself still requires a CSS L4 source frontend that
does not exist at HEAD.

The current production path remains provider-backed: `grammar_provider.rs`
delegates non-JSON runtime requests to `render_runtime_profile`; `lib.rs`
dispatches over `RuntimeProvider`; and JSON/CSS bytes still come from provider
modules and template directories. The W5A source-fact scanner proves source
materiality but does not compile CSS L4 source into IR.

## Dependency Cycle

Current V6 graph:

```text
W5A request boundary
  -> W5B-GEN provider-free generator body
  -> W5C-DELETE provider/template deletion
  -> W6 root-runtime collapse
```

Actual dependency:

```text
W5A request boundary
  -> CSS L4 source frontend/import/IR support
  -> provider-free generator body consumes request/frontend IR
  -> provider/template deletion + Lock 14 baseline close
  -> W6 root-runtime collapse
```

The missing frontend step is not optional. Without it, W5B-GEN can only either
copy current provider/template bodies under a new name or read committed runtime
output as source truth. Both are pre-blocked.

## Proposed Omega V7 Amendment

Split the current W5B-GEN/W5C-DELETE obligation:

| Wave | Proposed scope | Entry gate | Exit gate |
|---|---|---|---|
| W5B-FRONTEND | CSS L4 grammar-source frontend/import/IR support for `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture, typed host projections, and import graph consumption through W5A's request. No runtime provider/template deletion. | W5A admitted + REDRESS-211. | CSS L4 source roots compile or lower through the request-owned frontend with executable construct coverage; provider/template counts unchanged. |
| W5C-GEN | Provider-free runtime generator body consuming the request/frontend IR for CSS L4 and the existing JSON source path. No provider/template deletion. | W5B-FRONTEND admitted. | Production entrypoints are provider-free; `regen-css`, seven companions, `check-json`, `gate-json`, and provider-reachability greps pass. |
| W5D-DELETE | Delete legacy provider modules/template directories and close the Lock 14 baseline. | W5C-GEN admitted. | Provider count excluding `grammar_provider.rs` is 0; CSS template dirs are 0; retired JSON template residue is gone or unreachable per Lock 14; all consumers pass. |

W6 becomes conditional on W5D-DELETE close. W7 and W8/W9/W10 remain globally
blocked until PRUNE-1 through PRUNE-5 close.

## Risk Class

HIGH. This is a wave-graph and cap correction. It probably does not require a
LOCKS amendment because Lock 14 already requires the provider-free/generated
shape, but Omega-C must still verify 16 locks and the five-shape BackendShape
canon.

## Next Action

Close W5B-GEN as REDRESS-211, then dispatch Pass Omega V7. Inputs:

- REDRESS-211.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-{A..E}-*.md`.
- Pass Omega V6 signoff, because V7 corrects the W5B-GEN/W5C-DELETE split.
