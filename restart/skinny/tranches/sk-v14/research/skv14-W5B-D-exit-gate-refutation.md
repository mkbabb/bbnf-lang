# SK-V14 W5B-D: Exit Gate Refutation

Date: 2026-05-26.
Scope: W5B research agent D, executable gate feasibility.
Output: `restart/skinny/tranches/sk-v14/research/skv14-W5B-D-exit-gate-refutation.md`.
HEAD: `286233fa2`.

## Finding

W5B cannot be executed as a deletion-only wave at current HEAD. W5A admitted a
source-consuming request boundary and all same-wave consumers, but the actual
runtime bytes are still produced by the old provider/template renderers inside
codegen. That was acceptable for W5A's boundary-level exit gate; it is not
enough for W5B's provider/template deletion exit gate.

Evidence:

```sh
rg -n "render_runtime_profile|RuntimeProvider|json_provider|css_l4_.*provider" \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_profile.rs \
  skinny/crates/codegen/src/grammar_provider.rs
```

The search still reaches:

- provider module imports in `lib.rs`;
- `emit_runtime_profile(grammar_name)`;
- `render_runtime_profile(profile, ...)`;
- `RuntimeProvider` variants and the hard-coded eight-profile roster;
- the JSON special case in `grammar_provider.rs`.

The literal SPEC grep also has an execution ambiguity. Running the written
`rg -nE ... crates/` command from the repository root uses ripgrep's `-E`
encoding flag, not grep-style extended regex, and errors. Rewriting it as a
ripgrep pattern reveals that root `crates/` already contains historical audit
tag variants in `crates/ir/src/passes/audit/payload_coverage.rs`, outside W5B
ownership. The intended Lock 14 gate is a generic-codegen/skinny forward
invariant, but SPEC Section 8B's command text is too broad and tool-inaccurate
as written.

## Rejected Routes

- Moving provider/template contents into `grammar_provider.rs` is static
  centralization, the exact workaround rejected by REDRESS-209 and Pass Omega
  V5.
- Reading committed generated runtime output as the generation source is
  fixture lookup, not grammar-source/workspace-metadata generation.
- Deleting provider files while keeping equivalent per-grammar JSON/CSS
  templates under different names closes the count but not Lock 14.

## Required Disposition

W5B should return REDRESS rather than proceed to destructive deletion. The
corrective packet should amend the wave graph to add an explicit provider-free
runtime generator body construction step before provider/template deletion, and
should repair the W5B grep command to target the intended generic-crate forward
surface with ripgrep-correct syntax.
