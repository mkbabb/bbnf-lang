# W0 iter-surface verification — post-W0' regression report

**Run**: 2026-04-22 11:56–12:25 EDT. **HEAD**: `2e5e3ff5` (AY-II.W0'.d6
narrow build.rs fingerprint). **Worktree**: `agent-a0f4a883` (fresh
clone of branch, own `target/` directory). **HW**: Apple arm64, macOS
25.4.0. **Job**: assay + root-cause for the B0 iteration surface after
W0'.d3–d6 landed.

## Verdict

**REGRESSED** on `cargo iter-check`. Leaf tier (`cargo iter-test-leaf`)
intact at B0 targets; workspace-wide check is unusable.

## Measurements

| Command                               | Cold (s)   | Warm (s)   | B0 target (warm) | Status |
| ------------------------------------- | ---------- | ---------- | ---------------- | ------ |
| `cargo iter-test-leaf`                | **40.8**   | **1.14**   | 1.05 s           | PASS   |
| `cargo iter-check`                    | **>21:47 stall; killed** | **>6:00 stall; killed** | 0.16 s | **FAIL** |
| `cargo iter-check` cascade-touch warm | not reached | not reached | — | blocked |

Raw logs:

- `/tmp/iter2-leaf.log` — cold leaf, 40.8 s total (150.02 s user,
  382 % cpu → ~4× parallelism).
- `/tmp/iter3-leaf-warm.log` — warm leaf, 1.14 s total (0.60 s user).
- `/tmp/iter1.log` — first iter-check invocation; last progress line
  `Checking gorgeous v0.1.10` at 11:56:10; no further stdout after
  22 min; killed.
- `/tmp/iter4-check-warm.log` — warm retry; same stall on
  `bbnf-bootstrap` + `gorgeous`; killed after 6 min.

Process probe (21:47 elapsed on both rustc procs):

```
25912  99.3 %CPU  rustc --crate-name bbnf_bootstrap …
25913  99.5 %CPU  rustc --crate-name gorgeous --cfg feature="bbnf-grammar"
                                              --cfg feature="bnf-grammar"
                                              --cfg feature="css-grammar"
                                              --cfg feature="ebnf-grammar"
                                              --cfg feature="json-grammar"
                                              --cfg feature="sheets-grammar"
                                              --cfg feature="default" …
```

Both at ~99 % CPU for >22 min — a compute stall inside macro
expansion, not a deadlock nor I/O wait. Matches the signature
catalogued in `W0p-infra-root-cause.md` (derive(Parser) running
the full BBNF/JSON/CSS/EBNF/BNF/Sheets grammar pipeline inside
`proc_macro` on every rebuild).

## Root cause

`iter-check` is aliased as `check --profile ax-iter --workspace`
(`.cargo/config.toml:60`). `--workspace` forces a type-check of
**every** member, including `crates/gorgeous` and
`crates/bootstrap`.

W0'.d4 (`5c737bd1`) introduced per-grammar cargo features on
`gorgeous`, but the package's **default** feature set still turns
them all on:

```toml
# crates/gorgeous/Cargo.toml:28
default = ["bbnf-grammar", "json-grammar", "css-grammar",
           "ebnf-grammar", "bnf-grammar", "sheets-grammar"]
```

A bare `cargo check -p gorgeous` or `cargo check --workspace`
activates `default`, so all six `#[derive(Parser)]` sites expand —
identical to the pre-W0' cost. The d5/d6 landing changes (drop
gorgeous as mandatory dev-dep, narrow `build.rs` fingerprint) do
not intersect this path.

B0's 0.16 s warm measurement in `FINAL.md` must have been taken
against a target/ that was already primed by a prior `--workspace`
compile of gorgeous; a cold worktree does not reproduce it.

## Why leaf tier still passes

`iter-test-leaf = "test --profile ax-iter -p tape -p bbnf-ir -p
egraph -p csp-solver -p bbnf-ser"` (`.cargo/config.toml:67`) names
leaves that do **not** transitively depend on `gorgeous`, so the
derive-Parser macros never run. Warm 1.14 s matches B0's 1.05 s to
within noise.

## Suggested remediation (not executed — out of 30 min window)

Either of the following restores a usable `iter-check`:

1. **Narrow the alias**. Change `iter-check` to enumerate
   non-gorgeous workspace members (mirroring the `iter-test-leaf`
   pattern, plus `bbnf`, `bbnf-analysis`, `bbnf-lsp`, etc.). Drop
   `--workspace`.
2. **Empty `gorgeous` default features**. Set
   `default = []` in `crates/gorgeous/Cargo.toml`. Consumers
   (`wasm/`, the `gorg` binary via `bin-full`) already request the
   features they need explicitly.

Option 2 is the smaller diff and also fixes `cargo check -p
gorgeous` (and `cargo test --workspace`) for every other caller.
The `gorg` binary target in `crates/gorgeous/Cargo.toml` already
uses `required-features = ["bin-full"]`, so flipping the default
does not break binary builds.

Either fix must be validated by a fresh-worktree cold run
(equivalent to this report) before being closed.

## Outstanding work

- Cold + warm baselines for `iter-check` after the fix.
- Cascade-touch measurement (`touch crates/core/src/runtime/
  parsed.rs` → warm `iter-check`) was blocked by the stall; re-run
  after remediation.
- Confirm `docs/tranches/B0/FINAL.md` warm numbers against a fresh
  worktree and, if they do not reproduce, annotate the final
  artefact accordingly.
