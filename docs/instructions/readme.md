# Instructions

## Core rules

- No workarounds, no hacks, and no `#[allow(...)]`.
- No legacy code and no backward-compatibility shims.
- Generated files come from fresh regen. Do not hand-patch them.
- One codegen path. No hidden fallback path.
- No overfitting. Fold work into existing systems.
- Use `rule_kind()` dispatch, not string matching, unless the tape
  shape leaves no alternative and that recovery path is documented.

## Owned crates

Real fixes may land in:

- `../parse-that`
- `../pprint`
- `crates/csp-solver`

## Tranches

Every tranche lives in `docs/tranches/{LETTER}/`:

- `{LETTER}.md` is the plan.
- `PROGRESS.md` is the dated execution log.

## Expensive commands

- Write expensive command output to a file first.
- Inspect with `rg`, `sed`, or `awk`.
- Do not rerun an expensive command just to slice output differently.

## Cache clearing

Clear all `.bbnf-cache` directories before bench, regen, proc-macro
expansion, or profiling work:

```bash
find . -name ".bbnf-cache" -exec rm -rf {} + 2>/dev/null
```

When `bbnf-analysis` is the problem:

```bash
cargo clean -p bbnf-analysis
```

## Read next

- For tracked-file work: [editing.md](./editing.md)
- For bench / profile / `cargo expand`: [profiling.md](./profiling.md)
