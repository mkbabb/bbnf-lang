# BC Research Synthesis - Precepts Rollout

## Sources

- `/Users/mkbabb/Programming/precepts`
- `/Users/mkbabb/Programming/bbnf-lang`
- `/Volumes/config`
- `/Users/mkbabb/Programming/glass-ui`
- `/Users/mkbabb/Programming/speedtest`
- `/Users/mkbabb/Programming/keyframes.js`
- `/Users/mkbabb/Programming/value.js`
- `/Users/mkbabb/Programming/words`
- `/Users/mkbabb/Programming/fourier-analysis`
- `/Users/mkbabb/Programming/parse-that`
- `/Users/mkbabb/Programming/gaggle`
- `/Users/mkbabb/Programming/ffuzzy`
- `/Users/mkbabb/Programming/bbnf-lang/crates/csp-solver`

## Findings

1. `precepts` is correctly pure Markdown, but still carries the initial
   six-agent cap. BC must update the cap to 10, challenge sizing to max 5,
   and make triumvirate scope-dilation language explicit.
2. bbnf-lang, config, and speedtest duplicate shared tranche files under
   `docs/instructions/tranche/`. They should remove those files and read
   `docs/precepts/instructions/tranche/` directly.
3. glass-ui and the new consumers generally lack `docs/instructions/`.
   They need local tails, not copied shared rules.
4. `/Volumes/config/docs/LESSONS-LEARNED.md` contains reusable incidents:
   independent watchdogs, single writer per side effect, producer-consumer
   contract gates, canonical hostnames, observability as substrate, and
   parallel-agent budget discipline. These belong in shared lessons.
5. `csp-solver` is not a top-level repo under `/Users/mkbabb/Programming`.
   The active consumer is `bbnf-lang/crates/csp-solver`; it inherits through
   bbnf-lang's top-level `docs/precepts`.
6. Many consumers have user dirty work. BC must add only docs/submodule
   pointers and never broad-stage source changes.

## Accepted Plan Consequences

- Add `docs/precepts` as a submodule in every top-level consumer.
- Create or replace `docs/instructions/README.md` with repo-local tails.
- Remove duplicated tranche instruction files only in repos that already have
  those files.
- Add a crate-local csp-solver note pointing at bbnf-lang's shared precepts.
- Commit `precepts` before adding submodules so consumers point at the
  updated canon.
