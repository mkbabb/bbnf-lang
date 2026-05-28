# SK-V15 W1-F: Dirty Tree And Staging Risk

Date: 2026-05-28.
Scope: Current dirty tree versus likely W1 owner paths.
Output: this file.

## Findings

- HEAD is `a82196b9e` and the index was clean at research time.
- Likely W1 ledger/gate owner paths are clean except for
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs`, which carries a small
  formatting-only dirty diff.
- Clean W1-safe surfaces include:
  `restart/skinny/tranches/sk-v15/research/w1/**`,
  `skinny/RESULTS.md`, `skinny/REDRESS.md`,
  `restart/skinny/ROLLING-SOTA-DELTA.md`,
  `skinny/crates/bbnf-bench/src/report.rs`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/xtask/src/main.rs`, `skinny/xtask/src/lib.rs`,
  `skinny/xtask/src/skv15_w0.rs`, and `skinny/xtask/tests/skv15_w0.rs`.
- Dirty unrelated or protected paths include root runtime files under
  `crates/core/src/runtime/**`, historical SK-V12/SK-V13 research JSON,
  `docs/precepts`, root `xtask/src/main.rs`,
  `xtask/src/regen_simple_runtime.rs`, seven generated CSS runtime files under
  `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`,
  `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and
  `skinny/crates/bbnf-bench/src/css_l4_w8.rs`.

## Recommendations

- Use strict pathspec staging for every W1 commit. Do not use `git add -u`.
- Commit order:
  1. Research docs only under `restart/skinny/tranches/sk-v15/research/w1/**`.
  2. Plan artifact only.
  3. Redress owner paths only.
- If W1 edits `css_l4_w8.rs`, review the pre-existing formatting diff and stage
  the full file only if the intentional W1 diagnostic demotion is present and
  no unrelated semantic change is folded in.

## Risks

- W1 must not touch dirty root CSS typed-provider paths under
  `crates/core/src/runtime/css_l4/**`.
- W1 must not stage generated CSS runtime files or historical research JSON.

## Sources

- `git status --short`
- `git diff -- skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `restart/skinny/tranches/sk-v15/SPEC.md:264`
- `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:130`
- `restart/skinny/tranches/sk-v15/research/w0/skv15-W0-redress.md:117`
