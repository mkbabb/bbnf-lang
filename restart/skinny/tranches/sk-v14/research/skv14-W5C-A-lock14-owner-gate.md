# SK-V14 W5C-A: Lock 14 Owner Gate

Date: 2026-05-26.
Scope: W5C-GEN entry gate for owner paths and parent-diff routing.
Output: this file.

## §1 — Findings

`restart/skinny/tranches/sk-v14/SPEC.md` Section 8C requires
`SK_V14_W5C_GEN_OWNER_PATHS`, parent-diff routing for
`sk-v14-waveW5C-GEN` / `sk-v14-waveW5C-GEN-redress`, and a unit test before
generator source redress.

`skinny/crates/bbnf-bench/src/lock14_baseline.rs` currently has W5A and
W5B-FRONTEND rosters only. `sk-v14-waveW5C-GEN` appears only as a W5B rejection
case, so W5C source redress is still blocked by the Lock 14 entry gate.

## §2 — Recommendations

Add `SK_V14_W5C_GEN_OWNER_PATHS` with the W5C source surface:
`crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_provider.rs`,
`crates/codegen/src/runtime_generator.rs`, `xtask/src/main.rs`,
`xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
`crates/bbnf-bench/src/lock14_baseline.rs`.

Add parent-diff routing for `sk-v14-waveW5C-GEN` and
`sk-v14-waveW5C-GEN-redress`. The exact W5C test must admit those subjects and
reject provider/template deletion paths plus W5D subjects.

## §3 — Risks

Do not add provider or template paths to the W5C roster. W5C leaves those files
as W5D-DELETE residue. Allowing them in W5C would reopen the W5B/W5C spec
cycle and hide early deletion.

## §4 — Sources

- `restart/skinny/tranches/sk-v14/SPEC.md` Section 8C.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
