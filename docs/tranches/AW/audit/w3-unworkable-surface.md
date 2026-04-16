# AW-I.W3 — Intentional-Unworkability Surface

AW-I invariant #4 declares the window between W3 commit and W4 close
as intentionally unworkable. This doc records the workspace state
immediately after W3.2 so W4's dispatch has a concrete surface to
redress.

## Commits landed

| Hash       | Subject                                                               |
|------------|-----------------------------------------------------------------------|
| `3dce1ec7` | `feat(emitter): parse() dispatches through dta_run wholesale (AW-I.W3.1)` |
| `fc55cbfa` | `chore(generated): regen under post-W3.1 emitter (AW-I.W3.2)`         |

## Structural gates

| Gate                                                      | Target  | Observed | Status |
|-----------------------------------------------------------|---------|----------|--------|
| Non-prettify `fn __<rule><` in `generated.rs`             | 0       | 0        | pass   |
| Prettify `fn __<rule>_prettify<` in `generated.rs`        | unbounded (carve-out) | 53 | informational |
| `parse_dta` fn count in emitter                            | 0       | 0        | pass   |
| `dta_run_into` call in emitter parse body                  | ≥ 1     | 1 (+ 3 docstring refs) | pass   |
| `DTA_SCANNER` module-level const/static in emitter         | ≥ 1     | 1        | pass   |
| `wc -l generated.rs`                                       | ≤ 12000 | 20432    | **fail (plan miscalibration)** |

### Line-count miscalibration

Pre-swap: 27522 lines. Post-swap: 20432. Reduction: 7090 lines
(-25.8%). The plan modelled a 15k+ line reduction, targeting ≤ 12000.
Reality floor below 12000 is unreachable from W3 alone because:

| Component                                     | Lines  |
|-----------------------------------------------|--------|
| Header + preamble + imports                   | ~20    |
| `GRAMMAR_BbnfBootstrap` string const array    | ~10    |
| `GRAMMAR_PROFILE`                             | ~20    |
| `DTA_STATES` table (343 entries)              | 1291   |
| `DTA_RULE_ENTRIES` table (53 entries)         | 214    |
| Per-rule literal / regex / seq children consts | ~600   |
| `impl<'p> *View<'p>` accessor blocks (112 × ~95 lines) | 10638  |
| View type/struct defs + `RuleKind` enums      | ~450   |
| `BbnfBootstrap::parse` + `DtaDfaScanner` + `DTA_SCANNER` | ~70 |
| Prettify impl block (53 `fn __*_prettify`)    | 7092   |
| Closings + blanks                             | ~40    |
| **Total**                                     | 20432  |

Dropping the line count below 12000 requires collapsing the view
layer (AC.2 mandated, non-negotiable) or moving prettify out of
`generated.rs` (not in W3/W4 scope). The structural intent of W3
— "zero `fn __<rule>` in the parse path" — is met at 0/0.

## Workspace build surface

`cargo test --workspace --no-fail-fast` compiles the full dependency
graph but cannot cross the derive-macro barrier on gorgeous
parser structs. Output captured at `/tmp/w3-tests.txt` (446 lines).

### Tier A — compile + link successfully (pre-gorgeous)

All bbnf-core dependencies and bbnf-core itself. The library proper
(`cargo check -p bbnf --lib`) is green.

- `bbnf-ir`, `bbnf-ser`, `bbnf-tape`, `bbnf-regex`, `bbnf` (core
  lib), `bbnf-bootstrap`, `bbnf_derive`, `bbnf-analysis`, `bbnf-lsp`,
  `egraph`, `egraph-derive`, `csp-solver`, `pprint`, `pprint_derive`
- All external crates (serde, rayon, cranelift, biome, lightningcss,
  etc.)

### Tier B — compile fails at `#[derive(Parser)]` on gorgeous subgrammar structs

6 proc-macro panics: the derive macro reads the grammar `.bbnf`
file, calls `BbnfBootstrap::parse(source)` via the `imports::loader`
pipeline, and now fails because `parse()` dispatches through
`dta_run_into` + `DTA_TABLE` + the W2.1 DTA walker. The walker
handles Seq / Literal / Regex / Ref / AltLinear-with-savepoint /
Repeat `lo..=hi` / ShuntingYard but still misses feature coverage for
the bootstrap grammar's full surface (import directives, recovery
directives, pretty directives, nested group expressions, token-
dispatch alternation variants).

| Grammar file                                             | Lines | Status |
|----------------------------------------------------------|-------|--------|
| `crates/gorgeous/grammar/json/json.bbnf`                 | 19    | derive-panic |
| `crates/gorgeous/grammar/bnf/bnf.bbnf`                   | 16    | derive-panic |
| `crates/gorgeous/grammar/ebnf/ebnf.bbnf`                 | 51    | derive-panic |
| `crates/gorgeous/grammar/bbnf/bbnf.bbnf`                 | 52    | derive-panic |
| `crates/gorgeous/grammar/css/pretty.bbnf`                | 62    | derive-panic |
| `crates/gorgeous/grammar/google-sheets/google-sheets.bbnf` | 115 | derive-panic |

Error shape (uniform across all six):

```
error: proc-macro derive panicked
 --> crates/gorgeous/src/<name>.rs:5:10
  |
5 | #[derive(Parser)]
  |          ^^^^^^
  |
  = help: message: import error: Parse error in
    `.../<name>.bbnf`: Failed to parse grammar
```

Trace: derive macro → `parse_to_pipeline_inputs(source)` →
`BbnfBootstrap::parse(source)` (uses `dta_run_into`) → returns `None`
on first feature the walker doesn't yet handle → `ImportError::ParseError`.

### Tier C — downstream of Tier B (cascade failures)

Because the 6 derive panics prevent emission of `<Parser>::parse()`
and `<Parser>::<rule>_prettify()` associated items, 7 E0599 errors
cascade downstream (referencing methods that never got emitted):

- `JsonParser::value_prettify`
- `BbnfParser::grammar_prettify`
- `EbnfParser::grammar_prettify`
- `BnfParser::grammar_prettify`
- `CssParser::stylesheet_prettify`
- `GoogleSheetsParser::parse`
- `GoogleSheetsParser::formula_prettify`

Final outcome: `error: could not compile gorgeous (lib) due to 13
previous errors`.

### Tier D — tests that never ran

Zero tests executed. `gorgeous` is a dev-dependency of `bbnf`, so
bbnf's integration test binaries also fail to link. The workspace
test count resets from the W2.5 `1085/0/68` baseline to `0/0/0`
during this window.

## What W4 redresses

Per AW-I.W4 plan:

1. **W4.1–W4.4** delete the emitter helper modules (`alt.rs`,
   `seq.rs`, `repeat.rs`, `binary.rs`, `leaves.rs`, `map_value.rs`,
   `tape_prelude.rs`, `string_decode.rs`, `ws.rs`, `dispatch.rs`,
   `operator_chain.rs`) + their trait-method call chains in
   `emitter/mod.rs`. This causes the `rule_functions: Vec<TokenStream>`
   to stop being produced upstream. The vec was already dropped at
   the output site by W3.1, so the deletion is side-effect-free on
   `generated.rs`.
2. **W4.5** activates cyclic-rule fuse/inline (preserving rich-AST
   parity invariants from AW-I.W2.5) so the DTA walker's feature
   coverage matches the grammars currently panicking at derive time.
3. **W4 close** — workspace returns to `cargo test` green. All 13
   gorgeous errors should resolve as the fused-IR grammars lower
   into DTA tables the walker can execute.

## Idempotency — FAILS during W3/W4 window (as designed)

A second `bash scripts/bootstrap-bbnf.sh` run immediately after W3.2
produces a truncated 23-line output because:

1. W3.1's first bootstrap invocation compiled the bbnf lib against
   the PRE-swap `generated.rs` (which still had the fn-per-rule
   `BbnfBootstrap::parse`). That compilation cached into
   `target/debug/deps/`. The bootstrap script's `cargo expand`
   invocation used that stale-but-working bbnf to run the proc-macro,
   producing the new DTA-based `generated.rs`.
2. Committing the new `generated.rs` invalidates cargo's fingerprint
   for bbnf. The second bootstrap's `cargo expand` recompiles bbnf
   against the NEW `generated.rs` (DTA-based parse). The proc-macro
   then tries to use the new `BbnfBootstrap::parse` to parse
   `bbnf.bbnf` itself, the DTA walker fails mid-grammar, the proc-
   macro panics, `cargo expand` emits a truncated dump, and the post-
   processor collapses it to the 23-line header-only skeleton.
3. The `8000-line floor` sanity in `check-bootstrap-clean.sh` is
   exactly the guard for this regression pattern.

The first bootstrap was effectively the one-shot commit moment. W4
must redress this by one of:

- Expanding DTA walker coverage so `BbnfBootstrap::parse` handles the
  bbnf grammar's full surface (plan's intended path).
- Alternatively, pinning the bootstrap to an explicit stable bbnf
  rlib (architectural shim — not recommended per "no workarounds").

**`scripts/check-bootstrap-clean.sh` WILL FAIL during the W3→W4
window.** This is expected per AW-I invariant #4 and is why the plan
marks this window intentionally unworkable. W4 close restores
idempotency.

### Practical implication for W4 agents

W4 agents cannot use `bash scripts/bootstrap-bbnf.sh` mid-wave to
validate emitter changes. They must:

1. Run `cargo check -p bbnf --lib` as a structural smoke test (the
   bbnf lib proper compiles cleanly even with the broken self-
   parser).
2. Run `cargo expand -p bbnf-bootstrap --lib` directly only AFTER
   the walker's coverage has expanded to include the bbnf grammar
   (W4.5 fuse activation + any walker extensions) OR accept that
   `generated.rs` stays frozen at W3.2's committed form until W4's
   cumulative deletions restore idempotency.
3. At W4 close, re-run `bash scripts/bootstrap-bbnf.sh` once. If it
   produces the same 20432-line output as W3.2, the window has
   closed; commit a `chore(generated): re-regen after W4 close` only
   if the output differs.

## Prettify integrity

Prettify (`emitter/prettify/grammar.rs::emit_prettify_grammar_impl`)
is untouched. The 53 `fn __<rule>_prettify` entries in
`generated.rs` are byte-identical to the pre-swap baseline (diff
confirmed against `4a61488b:crates/core/src/grammar/generated.rs`).
The gorgeous parser structs' `<rule>_prettify()` associated items
are missing from the final compile not because prettify emission
broke, but because the derive macro panic early-exited before the
prettify impl block could be written to the expanded output.
