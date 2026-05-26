# SK-V14 W5B-FRONTEND A4: Lock 14 Owner Routing

Date: 2026-05-26.
Scope: read-only inspection of Lock 14 owner-path rosters, parent-diff routing, and gates.
Output: this file.

## §1 — Findings (concrete, file:line cited)

W5B-FRONTEND is blocked from touching frontend source paths until
`lock14_baseline.rs` has `SK_V14_W5B_FRONTEND_OWNER_PATHS`, subject routing for
`sk-v14-waveW5B-FRONTEND` and `sk-v14-waveW5B-FRONTEND-redress`, plus a unit
test proving only that roster is admitted (`restart/skinny/tranches/sk-v14/SPEC.md:724`,
`restart/audit/totality/astral/V7/master-plan-diff.md:55`).

Current Lock 14 SK-V14 rosters stop at W5A. `SK_V14_W5A_OWNER_PATHS` covers
`crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
`crates/codegen/src/grammar_provider.rs`, `xtask/src/{regen.rs,regen_css.rs,main.rs}`,
and `crates/bbnf-bench/src/lock14_baseline.rs`
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`). `current_lock14_owner_paths()`
only reserves and extends through W5A, so W5B dirty/staged frontend edits are
not yet admitted by the current-freeze gate
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1115`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1166`).

Parent-diff routing is subject-string based. It reads
`git log -1 --format=%s`, then routes changed frozen paths through
`validate_authorized_parent_diff` (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1329`).
Existing SK-V14 routing covers W4 and W5A, then rejects
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1600`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1611`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1622`).

The gate is live: `gate.rs` calls `lock14_baseline::validate()` before report
processing, and `validate()` includes allowlist, provider/template topology, git
freeze, backend-shape, and generic-crate neutrality checks
(`skinny/crates/bbnf-bench/src/bin/gate.rs:50`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:599`). Existing W5A topology
still rejects provider/template add/delete/rename before W5D
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1187`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1255`).

## §2 — Recommendations (named falsifiability gates)

Patch `skinny/crates/bbnf-bench/src/lock14_baseline.rs` first, before any
frontend source redress:

1. Add `SK_V14_W5B_FRONTEND_OWNER_PATHS` with exactly
   `crates/grammar/src/lib.rs`, `crates/codegen/src/lib.rs`,
   `crates/codegen/src/grammar_provider.rs`, `xtask/src/main.rs`,
   `xtask/src/regen.rs`, `xtask/src/regen_css.rs`, and
   `crates/bbnf-bench/src/lock14_baseline.rs`, matching the V7 initial roster
   (`restart/audit/totality/astral/V7/master-plan-diff.md:59`).
2. Add that roster to `current_lock14_owner_paths()` capacity and
   `extend_from_slice`.
3. Add subject-routing for `sk-v14-waveW5B-FRONTEND`,
   `sk-v14-waveW5B-FRONTEND-redress`, and lowercase `sk-v14-w5b-frontend`.
4. Add tests beside the W4/W5A parent-diff tests: admit the W5B roster under
   both required subjects, reject the same roster under W5C/other subjects, and
   reject an appended CSS provider/template path. Existing W5A tests are at
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2037` and
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:2054`.

Falsifiability gates:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo test -p bbnf-bench lock14_baseline -- --nocapture
```

Plus W5B's `regen-css` and seven `check-css-l4-*` companions
(`restart/skinny/tranches/sk-v14/SPEC.md:748`).

## §3 — Risks (REDRESS entries to pre-block)

- Do not add W5C-GEN or W5D-DELETE routing in the W5B patch.
- Do not include provider/template paths in `SK_V14_W5B_FRONTEND_OWNER_PATHS`.
- Serialize writes: W5B may touch shared `codegen`/`xtask` paths only for
  frontend lowering, then commit and admit before W5C-GEN starts.

## §4 — Sources (every external citation)

Local repository files only; no external sources used.
