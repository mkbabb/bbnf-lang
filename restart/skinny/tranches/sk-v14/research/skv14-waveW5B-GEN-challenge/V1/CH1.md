# SK-V14 W5B-GEN CHALLENGE V1 CH1 Correctness

Date: 2026-05-26.
Lens: CH1 Correctness.
Disposition: REVISE.

## Findings

The W5B-GEN rejection is substantively correct: the plan cites SPEC §8B,
research A-E, and the live provider route as the blocker. The falsifiability
packet needs tightening before CH1 can accept.

1. The mutation-proof gate claims no W5B-GEN source slice was attempted or
   retained, but uses `git diff --exit-code -- ...`. That does not prove staged
   changes are absent. The fold must use `git diff --exit-code HEAD -- ...` or
   add matching `git diff --cached --exit-code -- ...` checks for the same source
   and ledger paths.
2. The span-capture citation points at `grammar/css/l4/values.bbnf:67`, which is
   the comment. The actual `@{...}` capture starts at
   `grammar/css/l4/values.bbnf:69`.
3. The corrective packet has executable claims without direct citations. It must
   cite `skinny/crates/codegen/src/grammar_provider.rs:77` through
   `grammar_provider.rs:78`, `skinny/crates/codegen/src/lib.rs:180` through
   `lib.rs:185`, `lib.rs:233` through `lib.rs:244`, and
   `skinny/crates/grammar/src/lib.rs:320` through `lib.rs:327`.

## Required Fold

- Replace mutation-proof diff commands with HEAD-scoped or cached+unstaged
  checks.
- Correct `values.bbnf:67` to `values.bbnf:69`.
- Add direct source citations to the corrective packet's provider/parser claims.

## Sources

- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:56`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:60`
- `grammar/css/l4/values.bbnf:69`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:15`
- `skinny/crates/codegen/src/grammar_provider.rs:77`
- `skinny/crates/codegen/src/lib.rs:180`
- `skinny/crates/codegen/src/lib.rs:233`
- `skinny/crates/grammar/src/lib.rs:320`
