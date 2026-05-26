# SK-V14 W5B.0 A: Lock 14 Owner Routing

Date: 2026-05-26.
Scope: W5B.0 LOCK14-GATE owner-path roster and parent-diff routing.
Output: this file.

## §1 — Findings

The current Lock 14 gate stops at W5A. `SK_V14_W5A_OWNER_PATHS` is the latest
SK-V14 frontend-adjacent roster and includes `crates/grammar/src/lib.rs`,
`crates/codegen/src/lib.rs`, `crates/codegen/src/grammar_provider.rs`,
`xtask/src/{main,regen,regen_css}.rs`, and
`crates/bbnf-bench/src/lock14_baseline.rs`
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1105`).

`current_lock14_owner_paths()` aggregates through W5A only
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1115`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1169`). No
`SK_V14_W5B_FRONTEND_OWNER_PATHS` roster exists.

Parent-diff routing is commit-subject driven. `validate_git_freeze()` checks
dirty, unstaged, staged, and parent diffs against `current_lock14_owner_paths()`
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1173`). The SK-V14 routing
cases currently cover W4 and W5A, then fall through to rejection
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1600`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1611`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:1622`). W5B-FRONTEND, W5C-GEN,
and W5D-DELETE subjects are absent.

## §2 — Recommendations

W5B.0 should add `SK_V14_W5B_FRONTEND_OWNER_PATHS`, extend
`current_lock14_owner_paths()`, and route parent-diff subjects for
`sk-v14-waveW5B-FRONTEND`, `sk-v14-waveW5B-FRONTEND-redress`, and lower-case
`sk-v14-w5b-frontend` variants.

The smallest owner roster for W5B.0 itself is
`crates/bbnf-bench/src/lock14_baseline.rs`, with `crates/codegen/src/grammar_provider.rs`
allowed as the neutral exception that W5B.1+ may use after the gate admits.
W5B.0 must not touch grammar, codegen, or xtask frontend implementation paths.

## §3 — Risks

The old W5B-FRONTEND plan is superseded. It combined Lock 14 routing with
import and `@ws` source work; V8 CH5 rejects that coupling
(`restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:51`,
`restart/skinny/tranches/sk-v14/research/skv14-waveW5B-FRONTEND-challenge/V2/CH5.md:18`,
`restart/audit/totality/astral/V8/hardening/V2/CH5.md:33`).

## §4 — Sources

- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/skinny/tranches/sk-v14/SPEC.md`
- `restart/audit/totality/astral/V8/CRUD-LOG.md`
- Read-only agent `019e65b7-3ba7-7a40-b1ed-41a7642238de`.
