# SK-V14 W5B-GEN CHALLENGE V2 CH1 Correctness

Date: 2026-05-26.
Lens: CH1 Correctness.
Disposition: ACCEPT.

## Findings

The V2 fold resolves the V1 CH1 correctness items. The folded plan now uses
`git diff --exit-code HEAD -- ...` for the W5B-GEN source owner paths and the
RESULTS/rolling-delta ledger paths. This catches staged and unstaged mutations
against HEAD.

The span-capture citation is corrected to `grammar/css/l4/values.bbnf:69`, the
actual `@{...}` rule. The corrective packet now cites the live provider/parser
anchors directly: `grammar_provider.rs:77` through `grammar_provider.rs:78`,
`lib.rs:180` through `lib.rs:185`, `lib.rs:233` through `lib.rs:244`, and
`grammar/src/lib.rs:320` through `lib.rs:327`.

The falsifiability gates are measurable and were executed cleanly at HEAD:
source owner paths diff clean against HEAD, RESULTS/delta diff clean against
HEAD, provider-reachability grep finds the expected failing route, LOCKS count
is 16, and Pattern H count is 67.

## Sources

- `restart/skinny/tranches/sk-v14/research/skv14-waveW5B-GEN-challenge/V1/CH1.md:13`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:68`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:85`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md:87`
- `grammar/css/l4/values.bbnf:69`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md:15`
