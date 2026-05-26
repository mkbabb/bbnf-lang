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
delegates non-JSON runtime requests to `render_runtime_profile` at
`skinny/crates/codegen/src/grammar_provider.rs:77` through
`grammar_provider.rs:78`; `lib.rs` dispatches over `RuntimeProvider` at
`skinny/crates/codegen/src/lib.rs:180` through `lib.rs:185`; and JSON bytes
still come from `json_provider` at `lib.rs:233` through `lib.rs:244`. CSS bytes
still come from the CSS provider modules via the same `RuntimeProvider` match.
The W5A source-fact scanner proves source materiality but does not compile CSS
L4 source into IR; `skinny/crates/grammar/src/lib.rs:320` through `lib.rs:327`
accepts only `@import` and `@token` directives in `parse_grammar`.

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
  -> generic BBNF source frontend/import/IR closure (CSS L4 strict witness)
  -> provider-free generator body consumes request/frontend IR for all admitted grammars
  -> provider/template deletion + Lock 14 baseline close
  -> W6 root-runtime collapse
```

The missing generic frontend step is not optional. Without it, W5B-GEN can only
either copy current provider/template bodies under a new name or read committed
runtime output as source truth. Both are pre-blocked.

## Proposed Omega V7 Amendment

Split the current W5B-GEN/W5C-DELETE obligation:

| Wave | Proposed scope | Entry gate | Exit gate |
|---|---|---|---|
| W5B-FRONTEND | Generic BBNF grammar-source frontend/import/IR closure for `@ws`, `@pretty`, `?w`, `>>`, `<<`, span capture, typed host projections, and import graph consumption through W5A's request. CSS L4 is the strict positive witness; no runtime provider/template deletion. | W5A admitted + REDRESS-211. | CSS L4 source roots compile or lower through the request-owned generic frontend with executable construct coverage; JSON remains on the same source path; Sheets/BBNF remain fail-closed or gain generated-role witnesses; provider/template counts unchanged. |
| W5C-GEN | Provider-free runtime generator body consuming the request/frontend IR for every admitted grammar. No provider/template deletion. | W5B-FRONTEND admitted. | Production entrypoints are provider-free; `regen-css`, seven companions, `check-json`, `gate-json`, provider-reachability greps, and no-grammar-name-branch greps pass. |
| W5D-DELETE | Delete legacy provider modules/template directories and close the Lock 14 baseline. | W5C-GEN admitted. | Provider count excluding `grammar_provider.rs` is 0; CSS template dirs are 0; retired JSON template residue is gone or unreachable per Lock 14; all consumers pass. |

W6 becomes conditional on W5D-DELETE close. W7 and W8/W9/W10 remain globally
blocked until PRUNE-1 through PRUNE-5 close.

V7 must assign explicit LOC and time caps before dispatch. Proposed starting
caps are W5B-FRONTEND ≤1.0k source/test LOC, W5C-GEN ≤1.0k source/test LOC, and
W5D-DELETE ≤400 source/test LOC, each with a ≤90 minute redress ceiling. Any
frontend/import/IR slice that cannot fit its envelope must split again before
dispatch. Generated output is uncounted only when produced by fresh regen
through the active generator and diff-audited.

The V7 CRUD packet must also add explicit Lock 14 owner-path and parent-diff
routing for W5B-FRONTEND and W5C-GEN before either redress commit, because the
current Lock 14 gate only routes the W5A shape.

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
