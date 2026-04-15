# Instructions

## Core rules

- No workarounds, no hacks, and no `#[allow(...)]` to mask issues.
- No legacy code.
- No backward compatibility shims.
- Generated files are output of fresh regen; never hand-patch.
- One codegen path. No hidden fallback path.
- No overfitting. Fold work into existing systems instead of creating
  parallel subsystems.
- All solutions implemented must befit full generality with no special cases of grammar or structure.

## Owned crates

All crates in the dependency graph are owned and modifiable,
including external path dependencies:

- `../parse-that`
- `../pprint`
- `crates/csp-solver`

## Tranches

Every tranche lives in `docs/tranches/{LETTER}/`:

- `{LETTER}.md` is the plan.
- `PROGRESS.md` is the dated execution log and ground truth.

## Expensive commands

- Always write expensive command output to a file first.
- Then inspect with `rg`, `sed`, or `awk`.
- Do not rerun an expensive command just to slice output differently.

## Cache clearing

Clear all `.bbnf-cache` directories before bench, regen, proc-macro
expansion, or profiling work:

```bash
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
```

## Read next

- For tracked-file work: [editing.md](./editing.md)
- For bench / profile / `cargo expand`: [profiling.md](./profiling.md)
