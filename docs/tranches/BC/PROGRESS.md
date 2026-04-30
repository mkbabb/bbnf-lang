# Tranche BC Progress

## 2026-04-29 - W0 Research Open

Dispatched read-only research across shared `precepts`, bbnf-lang,
`/Volumes/config`, glass-ui, speedtest, keyframes.js, and local direct
research for value.js, words, fourier-analysis, parse-that, gaggle, ffuzzy,
and `crates/csp-solver`.

Harness limit admitted six concurrent explorer agents; remaining repo slices
were researched directly with file-system and manifest reads. This is within
the BC invariant: use up to 10 parallel agents when the harness permits, and
avoid fake parallelism when the harness refuses more threads.

## 2026-04-29 - W0 Challenge Open

Closed the initial research agents and dispatched a two-agent challenge wave:

- shared/bbnf/config/speedtest challenge;
- frontend/new-consumer/csp-solver challenge.

Challenge close updates `audit/challenge.md`.

## 2026-04-29 - W0 Close

Challenge dispositions saved in `audit/challenge.md`. Key narrowing:

- 10 agents is a hard ceiling, not the default target.
- Challenge waves default to five agents but may override within the
  10-agent ceiling.
- Triumvirate triggers for stalls and unclear scope dilation, not every
  absorbable reveal.
- Local tails replace only instruction surfaces; product docs remain local.

## 2026-04-29 - W1 Close

`/Users/mkbabb/Programming/precepts` updated and committed at
`458c2d1`.

Verification:

- stale six-agent wording scan in `precepts`: clean;
- `Hard ceiling: max 10 parallel agents` present;
- lifecycle line present:
  `research -> challenge -> plan -> wave spec -> implementation -> doc update`;
- triumvirate and scope-dilation language present.
- no-overfitting rule present: current consumer and evidence, otherwise delete.

## 2026-04-29 - W2 Close

Added `docs/precepts` submodule references at commit `458c2d1` and local
`docs/instructions/README.md` tails for:

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

`crates/csp-solver` received a crate-local instruction note and no nested
submodule.

Existing duplicated tranche instruction files in bbnf-lang, config, and
speedtest were removed; consumers read `docs/precepts` directly.

## 2026-04-29 - W3 Close Audit

Verification:

- every top-level consumer has `docs/precepts/instructions/tranche/SPEC.md`;
- every top-level consumer has `docs/instructions/README.md`;
- every submodule points to `458c2d1`;
- no nested `crates/csp-solver/docs/precepts`;
- old bbnf/config/speedtest tranche instruction files are absent;
- stale six-agent and old bbnf instruction path scan over new instruction
  surfaces is clean.

Remote: `precepts` is private at `git@github.com:mkbabb/precepts.git`;
consumer `.gitmodules` use that URL.
