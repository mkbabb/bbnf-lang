# SK-V14 W5B-FRONTEND A6: Provider/Template Topology

Date: 2026-05-26.
Scope: inspect `skinny/crates/codegen/src` topology and no-delete guard/redress references.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5B-FRONTEND is frontend/import/IR only. Its entry gate requires W5A admitted,
REDRESS-211 closed, V7 CRUD applied, Lock 14 owner-path routing before source
redress, and a plan that forbids provider/template deletion and provider-free
generator replacement (`restart/skinny/tranches/sk-v14/SPEC.md:724`,
`restart/skinny/tranches/sk-v14/SPEC.md:726`,
`restart/skinny/tranches/sk-v14/SPEC.md:729`).

Current topology is still the W5A legacy mesh: 8 provider modules excluding
`grammar_provider.rs` and 7 CSS template directories. The code declares seven
CSS providers plus `json_provider`; `grammar_provider` is separate
(`skinny/crates/codegen/src/lib.rs:1`, `skinny/crates/codegen/src/lib.rs:10`,
`skinny/crates/codegen/src/lib.rs:11`). Filesystem counts observed: providers
8, CSS template dirs 7, all `*_templates` dirs 8, template files 41.

The executable topology guard already encodes the W5A no-delete baseline:
provider count must equal 8 excluding `grammar_provider.rs`; CSS template dir
count must equal 7. It scans status, unstaged diff, staged diff, and parent diff
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1187`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1199`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1215`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1220`).

The exact no-add/delete/rename gate is `status == "??" || A* || D* || R*`,
applied to any path ending `_provider.rs` or containing `_templates`, except
`grammar_provider.rs` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1264`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1305`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2074`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:2093`).

Relevant REDRESS pre-blocks are REDRESS-209, REDRESS-210, and REDRESS-211.
They reject static centralization, deletion before replacement, and
provider-free generation before generic frontend closure
(`skinny/REDRESS.md:5173`, `skinny/REDRESS.md:5197`, `skinny/REDRESS.md:5221`).
V7 splits W5B-FRONTEND, W5C-GEN, and W5D-DELETE, with deletion only in W5D
(`restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:55`).

## §2 — Recommendations (named falsifiability gates)

W5B-FRONTEND should prove no provider/template deletion with these gates:

```sh
find skinny/crates/codegen/src -name '*_provider.rs' ! -name 'grammar_provider.rs' | wc -l
find skinny/crates/codegen/src -type d -name 'css_l4_*_templates' | wc -l
git diff --name-status -- skinny/crates/codegen/src
git diff --cached --name-status -- skinny/crates/codegen/src
```

Expected counts: providers = 8; CSS template dirs = 7. Diff status must contain
no `A`, `D`, `R`, or `??` for protected provider/template paths. Lock 14 unit
coverage must extend the W5A topology guard to W5B-FRONTEND owner-path routing
before frontend source edits. `cargo xtask regen-css` plus all seven
`check-css-l4-*` companions must pass without deleting or renaming
providers/templates.

## §3 — Risks (REDRESS entries to pre-block)

- Replacing `render_runtime_profile`/`RuntimeProvider` production dispatch is
  W5C-GEN scope, not W5B-FRONTEND scope
  (`restart/skinny/tranches/sk-v14/SPEC.md:758`).
- Deleting the provider mesh is W5D-DELETE scope.
- Adding a neutral frontend module without first naming it in the plan and
  adding the exact Lock 14 owner path violates SPEC §8B
  (`restart/skinny/tranches/sk-v14/SPEC.md:727`).

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
