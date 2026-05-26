# SK-V14 W5B-FRONTEND CHALLENGE V2 CH5 Hidden Coupling

Date: 2026-05-26.
Lens: CH5 Hidden Coupling.
Disposition: REVISE.

## Findings

1. Provider/template guards are too narrow. The V2 shell guard at
   `skv14-W5B-FRONTEND-plan.md:170` covers `_provider.rs` and
   `css_l4_.*_templates`, but topology includes eight `*_templates`
   directories including `json_templates` per
   `skv14-W5B-FRONTEND-A6-provider-template-topology.md:21`. The plan says all
   provider/template modification is forbidden except `grammar_provider.rs` at
   `skv14-W5B-FRONTEND-plan.md:178` and `:234`, and the current protected-path
   predicate covers any `_templates` path at `lock14_baseline.rs:1305`. A
   modified `json_templates/*` file would bypass the V2 shell guard.
2. Lock 14 routing is still coupled to source work. W5B.0 combines Lock 14
   routing with import/`@ws` source work at `skv14-W5B-FRONTEND-plan.md:51` and
   `:61`. SPEC/DISPATCH require owner-path and parent-diff routing before W5B
   touches source owner paths at `SPEC.md:726`, `SPEC.md:733`, and
   `DISPATCH-PROMPT.md:149`.

## Accepted Checks

- Existing reachability/no-sidecar folds remain useful: `RuntimeProvider`,
  `GrammarProfile`, and `render_runtime_profile(profile, None)` stay live, and
  frontend facts stay request-local only.

## Required Fold

- Broaden provider/template guards to `(_provider\\.rs|_templates)` and add an
  all-template count gate expecting 8.
- Add W5B Lock 14 tests rejecting modified providers and modified CSS/JSON
  template files while still allowing `grammar_provider.rs`.
- Split W5B.0 into a Lock14-only first checkpoint, with
  `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passing before any
  grammar/codegen/xtask frontend edits.
