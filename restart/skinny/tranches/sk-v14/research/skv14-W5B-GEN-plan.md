# SK-V14 Wave W5B-GEN Plan: Reject Provider-Free Generator Body Under Current Cap

Date: 2026-05-26.
Wave: W5B-GEN.
Phase: plan.
Disposition: REJECT; route to Pass Omega V7.

## Inputs

- `restart/skinny/tranches/sk-v14/SPEC.md:706` through `SPEC.md:756` binds
  W5B-GEN to a provider-free runtime generator body: CSS L4 and JSON bytes must
  come from grammar source plus workspace metadata, not providers/templates,
  committed generated output, or grammar-name branches.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-A-codegen-reachability.md`
  shows the current production path still reaches `render_runtime_profile`,
  `RuntimeProvider`, `json_provider`, and the CSS providers.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-B-xtask-routing.md`
  shows xtask already builds the W5A request, so the remaining gap is below
  `emit_runtime_from_request`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-C-request-facts.md`
  shows CSS L4 source is fact-scanned, not compiled through the skinny grammar
  parser.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-D-verification-gates.md`
  names the mutation-proof and provider-reachability gates.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-E-proof-carry.md`
  names the W5A JSON and Sheets/BBNF proof carry that must be preserved by any
  future route.

## Decision

Reject W5B-GEN under the current SPEC shape. There is no honest ≤1.0k
source/test LOC intervention that can replace the live provider/template-backed
runtime emitter with one source-plus-metadata generator body for both CSS L4 and
JSON.

The intrinsic blocker is the missing CSS L4 frontend/generator substrate. The
only real generic compile route calls `grammar::parse_grammar`, but
`skinny/crates/grammar/src/lib.rs:320` accepts only `@import` and `@token`
directives. The CSS L4 grammar uses constructs that current `parse_grammar`
does not parse into IR: `@ws` at `grammar/css/l4/stylesheet.bbnf:12`, `?w`,
`>>`, and `<<` at `stylesheet.bbnf:15` through `stylesheet.bbnf:16`,
`@pretty` at `stylesheet.bbnf:53` through `stylesheet.bbnf:60`, span capture at
`grammar/css/l4/values.bbnf:67`, and typed host projections at
`grammar/css/l4/color.bbnf:190` and `color.bbnf:228`.

Current HEAD still emits runtime bytes through the provider mesh:
`skinny/crates/codegen/src/grammar_provider.rs:77` delegates non-JSON requests
to `render_runtime_profile`, and `skinny/crates/codegen/src/lib.rs:180` dispatches
over `RuntimeProvider`. Moving the 5.8k LOC template/runtime bodies into a
neutral-looking module would be static centralization, not a provider-free
generator body. Reading committed generated output as input is explicitly
pre-blocked by `SPEC.md:752`.

## Falsifiability Gate

This rejection is closed by proving no W5B-GEN source slice was attempted or
retained, while the old provider reachability remains as the failing evidence:

```sh
git diff --exit-code -- \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_provider.rs \
  skinny/crates/grammar/src/lib.rs \
  skinny/xtask/src/regen.rs \
  skinny/xtask/src/regen_css.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs

git diff --exit-code -- skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md

cd skinny && rg -n '\b(render_runtime_profile|RuntimeProvider|GrammarProfile|json_provider|css_l4_.*provider)\b' crates/codegen/src/{lib.rs,grammar_provider.rs}

grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l | tr -d ' '
```

Expected disposition: the first two diff commands are clean; the provider grep
finds the live failing route; the lock count remains 16; Pattern H remains 67.

## Rejected Routes

- Static centralization of current CSS/JSON template bodies in a neutral module.
- Reading committed generated runtime output as source truth.
- Keeping `RuntimeProvider`/`GrammarProfile` reachable under compatibility names.
- Adding grammar-name branches in generic crates.
- Borrowing W5C-DELETE or W6 budget to implement the missing CSS L4 source
  frontend.

## Corrective Route

Pass Omega V7 is required because the rejection materially changes the SK-V14
wave graph. W5C-DELETE, W6, W7, and W8/W9/W10 remain blocked until the PRUNE
chain is rerouted.

The proposed V7 split is:

| Wave | Scope | Entry | Exit |
|---|---|---|---|
| W5B-FRONTEND | Real CSS L4 grammar-source frontend/import/IR support for constructs W5A only fact-scans today; no provider/template deletion. | W5A admitted + REDRESS-211. | CSS L4 sources compile through a request-owned frontend; no runtime/provider deletion. |
| W5C-GEN | Provider-free runtime generator body consuming the frontend/request IR; no provider/template deletion. | W5B-FRONTEND admitted. | `regen-css`, seven companions, `check-json`, and provider-reachability grep pass with providers/templates only as unreachable residue. |
| W5D-DELETE | Former W5C-DELETE provider/template deletion plus Lock 14 baseline close. | W5C-GEN admitted. | Provider/template counts zero, Lock 14 closes, same-wave consumers pass. |

W6 moves to `W5D-DELETE admitted`; W7 and W8/W9/W10 remain globally blocked by
the PRUNE chain.

## Revert Protocol

No source patch is attempted in this plan phase. If a later redress attempt
exists before rejection close, save it to `/tmp/skv14-waveW5B-GEN-rejected.patch`,
revert only that attempted W5B-GEN source slice, preserve W5A's request-boundary
work, then append REDRESS-211.

## Same-Wave Consumer

There is no admissible same-wave consumer under the current W5B-GEN shape. The
planned consumer set remains the amended SPEC set (`regen-css`, seven CSS
companions, `check-json`, and `gate-json`), but it cannot be wired honestly until
V7 inserts the CSS L4 frontend work before provider-free generation.
