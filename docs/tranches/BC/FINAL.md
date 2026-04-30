# Tranche BC Final

BC moved shared agent orchestration into `precepts` and wired each named
top-level consumer to `docs/precepts`, with repo-local instruction tails kept
small and project-specific.

## Shared Rules

`/Users/mkbabb/Programming/precepts` closed at:

```text
458c2d1
```

Shared docs now state:

- hard ceiling: 10 agents per wave;
- research can use three to 10 distinct agents;
- challenge waves default to max five, with override inside the 10-agent
  ceiling;
- lifecycle:
  research -> challenge -> plan -> wave spec -> implementation -> doc update;
- triumvirate:
  research + plan augment/synthesis + redress/redeployment;
- scope dilation pauses implementation when the prompt no longer describes
  honest close scope;
- no overfitting: current consumer and evidence, otherwise delete.

## Consumers Updated

Each top-level consumer now has `docs/precepts` pinned to `458c2d1` and
`docs/instructions/README.md` as a local tail:

- bbnf-lang;
- `/Volumes/config`;
- glass-ui;
- speedtest;
- keyframes.js;
- value.js;
- words;
- fourier-analysis;
- parse-that;
- gaggle;
- ffuzzy.

`crates/csp-solver` is nested in bbnf-lang, so it received only
`crates/csp-solver/docs/instructions/README.md`; no nested precepts submodule
was created.

## Local Tail Summary

- bbnf-lang: Rust toolchain, cargo target-dir discipline, nextest, regen,
  profiling, one codegen path, csp-solver note.
- config: HA/Node-RED runtime truth, deploy/build commands, observability,
  local lessons ledger.
- glass-ui: Vue/Tailwind library rules, token-first visual system, browser
  verification, no-overfitting evidence sweep.
- speedtest: consumer-app ownership, glass-ui/keyframes workspace links,
  browser evidence, deployment caveat.
- keyframes.js: animation engine invariants, demo/browser proof, dist hygiene.
- value.js: CSS value/color parser surfaces, Vitest/build/e2e proof, release
  export caution.
- words: FastAPI/Vue product rules, uv/npm commands, ffuzzy/search/AI cost
  caveats.
- fourier-analysis: Python math package plus web/API verification surfaces.
- parse-that: TypeScript/Rust parser-combinator parity, fixtures, benchmarks.
- gaggle: AI simulation, Google Workspace credential boundaries, deterministic
  validation.
- ffuzzy: Rust/PyO3 workspace, engine/router quality gates, sibling path deps.

## Verification

- `git submodule status` showed every top-level consumer at
  `458c2d1167f4e3a327edf17fc7509da533cacf1e docs/precepts`.
- `test -e <repo>/docs/precepts/instructions/tranche/SPEC.md` and
  `test -f <repo>/docs/instructions/README.md` passed for all consumers.
- `test ! -e crates/csp-solver/docs/precepts` passed.
- bbnf/config/speedtest duplicated tranche instruction files are removed.
- Stale wording scan over `precepts` and new instruction surfaces found no
  six-agent cap, abstract framing headers, stale bbnf instruction path
  references, or keep-without-consumer language.

No build/test suites were run; this tranche changed documentation and
submodule pointers only.

## Dirty Worktree Boundaries

BC touched only exact rollout paths. Pre-existing dirty work remains in
config, speedtest, keyframes.js, value.js, words, fourier-analysis,
parse-that, gaggle, and ffuzzy. None was reverted or staged by broad add.

## Remote

`precepts` is published as private repo `mkbabb/precepts`. Consumer
`.gitmodules` point at `git@github.com:mkbabb/precepts.git`.
