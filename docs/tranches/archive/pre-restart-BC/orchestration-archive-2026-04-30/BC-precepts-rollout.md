# Tranche BC - Shared Precepts Consumer Rollout

BC wires every named consumer to the shared orchestration canon at
`docs/precepts/`, plus a small repo-local tail for commands, runtime proof,
profiling, deployment, and project invariants.

## Thesis

Agent orchestration is shared infrastructure. Rust profiling, Home Assistant
deployment, Vue visual checks, animation engine dist hygiene, color parser
benches, dictionary AI pipelines, Fourier web/runtime checks, parser
benchmarks, simulation cost controls, and fuzzy-search Rust/Python bindings
are local concerns. BC separates those concerns cleanly:

1. `precepts` owns the reusable rules: KISS, DRY, bounded parallelism,
   research, challenge, triumvirate, wave specs, scope-dilation response, and
   doc-update close.
2. Consumers reference `precepts` at `docs/precepts` as a submodule.
3. Consumers keep only `docs/instructions/README.md` local tails.
4. Existing duplicated tranche instruction paths are removed; consumers read
   `docs/precepts` directly.

## Invariants

1. **One shared rule set.** Shared orchestration rules live in `precepts`, not
   copied into consumers.
2. **Local tails only.** Consumer instruction READMEs state only project-
   specific commands, proof surfaces, local invariants, and dirty-worktree
   cautions.
3. **Ten-agent cap is explicit.** `precepts` permits up to 10 parallel agents
   when file bounds and context load justify it; challenge waves scale to
   half the research wave.
4. **Triumvirate is load-bearing.** Scope dilation, diagnostic stalls, and
   unusable first-pass returns trigger research + plan augment/synthesis +
   redress/redeployment before work resumes.
5. **No duplicate submodule for nested crates.** `crates/csp-solver` inherits
   through bbnf-lang's top-level `docs/precepts`.
6. **No user work reverted.** Dirty worktrees are respected; BC edits only
   instruction docs, submodule pointers, and tranche rollout docs.

## Wave Schedule

| Wave | Agents | Closes On | Status |
|---|---:|---|---|
| W0 | 10 research + 2 challenge | Synthesis and challenge ledger saved | complete |
| W1 | direct + bounded implementation | `precepts` updated for 10-agent/triumvirate/no-overfitting protocol | complete |
| W2 | direct + repo-sliced implementation | every consumer references `docs/precepts` and has local tail | complete |
| W3 | direct audit | status, submodules, and docs verified; FINAL written | complete |

## Critical Files

| Path | Owner | Purpose |
|---|---|---|
| `/Users/mkbabb/Programming/precepts/instructions/**` | W1 | Shared canon updates |
| `docs/tranches/BC/**` | W0/W3 | BC tranche record |
| `docs/precepts` in each repo | W2 | Shared submodule reference |
| `docs/instructions/README.md` in each repo | W2 | Project-local tail |
| `docs/instructions/tranche/*.md` where already present | W2 | Removed duplicate shared canon |
| `crates/csp-solver/docs/instructions/README.md` | W2 | Nested crate local note |

## Consumers

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

## Hard Gates

1. `precepts` docs contain no stale six-agent cap.
2. `precepts` names the lifecycle:
   research -> challenge -> plan -> wave spec -> implementation -> doc update.
3. Every top-level consumer has `docs/precepts` and `docs/instructions/README.md`.
4. Existing consumer `docs/instructions/tranche/*.md` duplicate files are
   absent.
5. `git status --short` for each repo shows only BC files plus pre-existing
   dirty work.
6. `FINAL.md` records consumers updated, misses, and follow-on work.

## Scope-Dilation Protocol

If implementation reveals a consumer needs more than a docs tail and a
submodule reference, pause the repo slice. Dispatch the triumvirate:

1. research: identify the exact extra scope and prior local process docs;
2. plan augment/synthesis: amend BC wave docs and file bounds;
3. redress/redeployment: implement the amended slice.

No consumer receives a half-migrated instruction surface.
