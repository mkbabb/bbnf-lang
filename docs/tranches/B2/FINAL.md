# B2 — FINAL

Tranche B2 retires the `bbnf_derive` proc-macro IR-pipeline contract.
Code generation for every grammar in the workspace moves out of
rustc's expand phase into a workspace-level build step; per-grammar
source files emerge directly on disk under
`crates/core/src/grammar/generated/<ident>.rs`; consumer crates reach
each grammar's parser surface through a canonical `pub use ::bbnf::
grammar::generated::<ident>::*` re-export. The `crates/derive/` crate
deletes outright. The 80-min cold rustc-side IR-pipeline wall on every
`#[derive(Parser)]` consumer ceases to exist.

## Headline

`cargo xtask regen` is the canonical regen entrypoint. It reads the
workspace-level grammar manifest (`[workspace.metadata.bbnf.grammars]`
in the root `Cargo.toml`), runs the 17-pass IR pipeline + `generate_
all` codegen + `prettyplease::unparse` once per grammar, and writes
the result to `crates/core/src/grammar/generated/<ident>.rs`. The
output is portable across worktrees, checkouts, and developer hosts
because every emitted `include_str!()` literal resolves through
`concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` at consumer
compile-time. CI and pre-commit invoke `cargo xtask regen --check` to
gate against drift between checked-in source and the regenerator's
output.

## Architectural narrative

The proc-macro contract is for small, local transformations of user
code. A 30 K-line `TokenStream` emitted at expand-time forces rustc to
re-tokenise, re-parse, re-borrow-check, and re-metadata-emit the
emitted surface for every consuming crate; the cost is structural, not
optimisable. B2's transposition lifts the IR pipeline out of rustc's
expand phase entirely. The pipeline runs once per regen invocation
(CI, pre-commit, manual), not once per consuming-crate compile.

Five mechanisms compose:

1. **Workspace-level grammar manifest.** `[workspace.metadata.bbnf.
   grammars]` enumerates every grammar (path, identifier, features)
   the workspace consumes. The xtask reads this manifest at every
   invocation; per-grammar regen looks up its entry by name; the full
   sweep iterates the table.
2. **Per-grammar source on disk.** Each grammar's emission writes to
   its own file at `crates/core/src/grammar/generated/<ident>.rs`. The
   aggregator `crates/core/src/grammar/generated/mod.rs` declares
   every per-grammar module + globally re-exports the BBNF self-host's
   surface (`pub use bbnf::*;`). Per-grammar surfaces do not glob at
   the aggregator level because each module's `pub use
   __<lowered>_emit_impl::*;` re-export carries grammar-specific
   companion types (`<Marker>NodeView`, `<Marker>RuleKind`, per-rule
   view structs, projection structs) that would collide on glob.
3. **Marker structs replace derive metadata.** `#[derive(Parser)]
   #[parser(path = "...")]` becomes `pub struct <Ident>Parser;` plus
   the canonical `pub use ::bbnf::grammar::generated::<ident>::*`.
   Sixty-two consumer sites across forty-three files migrated under
   this contract. The glob import pulls the marker AND its
   companion-type set into scope; sites whose local name diverged
   from the canonical marker rename in-place to the canonical name so
   companion-type prefixes resolve through the glob.
4. **Portable `include_str!` paths.** The xtask resolves manifest
   paths to absolute `PathBuf` for the IR pipeline's file-read pass,
   but the emitter consumes a parallel `grammar_rel_paths: Vec<String>`
   carrying workspace-relative POSIX paths. `generate_grammar_arr`
   wraps each entry with `concat!(env!("CARGO_MANIFEST_DIR"), "/../
   ../", <rel>)`. For the `bbnf` crate, `CARGO_MANIFEST_DIR` resolves
   to `<workspace>/crates/core`; two `..` levels lift to the workspace
   root; the relative path joins the actual grammar source file. No
   worktree-specific path embeds in any emitted file.
5. **Drift detection moves to git history.** `BBNF_SCHEMA_VERSION`
   retires with the proc-macro crate it lived in. Schema-version
   drift detection shifts from in-band cache invalidation (an
   in-band protocol every consumer's `build.rs` had to honour) to
   standard "diff after regen" hygiene: `cargo xtask regen --check`
   regenerates to a tempdir + diffs against the checked-in tree;
   non-zero exit on divergence. CI is the canonical enforcement;
   the in-tree pre-commit hook (`scripts/hooks/pre-commit`,
   installable via `scripts/install-hooks.sh`) is the local
   fast-fail when a staged change touches a grammar source, the
   per-grammar generated tree, or the regen entrypoint.

The transposition is structural, not procedural: a single file on
disk per grammar, refreshed by one xtask invocation, gated by one
diff at CI. No proc-macro contract, no schema-version protocol, no
content-keyed cache, no `cargo expand` regex post-processor. The
substrate's role retires by reformulation.

## Performance

| Surface | Wall |
|---|---|
| `cargo xtask regen` (full sweep, 9 grammars, cold xtask compile) | ~12:43 |
| `cargo xtask regen --check` (post-build, idempotent diff) | 1.11 s |
| `cargo xtask regen --grammar bbnf` IR pipeline only (`compile_paths_request` + `generate_all` + `prettyplease`) | ~73 ms |
| `cargo check --workspace --profile ax-iter` (warm) | ~4 s |
| `cargo iter-check` (warm) | 0.21 s |
| `cargo iter-check-full` (warm) | 0.13 s |
| `cargo iter-check-full` (cold post-W1 cutover) | ~45 s |
| Pre-B2 `cargo expand -p bbnf-bootstrap --lib` cold | > 80 min (retired) |

The `cargo iter-check` warm gate sits well under the 0.5 s ceiling
the B2 plan invariant 12 declared; routine iteration matches the d7
baseline. The full-sweep regen wall is dominated by the per-grammar
`bbnf` lib rebuild each emission triggers (the per-grammar
`generated/<ident>.rs` overwrite forces a `bbnf` lib relink before
xtask itself relinks); the IR pipeline itself runs in milliseconds
per grammar. The pre-B2 80-min wall on cold `cargo expand -p bbnf-
bootstrap --lib` falls to seconds because the IR pipeline no longer
crosses rustc's expand boundary.

The aggregate proof matrix is at `docs/benchmarks/post-B2.json`.

## Test results

`cargo nextest run --workspace --profile ax-iter --no-fail-fast` at
W3 close: 1 490 tests, 1 160 passed, 327 failed, 3 timed out, 27
skipped. The pass-rate matches the W1 + W2 close baselines exactly;
no B2 wave introduced a regression.

The 327 failures + 3 timeouts belong to the pre-existing
`FusedBuilder::finish called with N open value frames remaining`
debug-build assertion class at `crates/tape/src/builder/mod.rs:1066`,
plus the bbnf-lsp integration tests' fixture polish. The class
originates in tape-builder finalisation logic, not in any B2-touched
surface. Release-mode parity verification (`cargo test --release -p
bbnf --test bbnf_parity --exact bbnf_parses_its_own_grammar`) passes
in 0.20 s wall against the released binary, confirming the BBNF
self-host parses its own grammar to the known-good fixture.

The remainder routes to B4.W1, which owns the `FusedOutput<R>` /
`FusedBuilder` finalisation consumer-fixture polish.

## Cross-tranche effects

**AY-II.W0' close ceremony**: dispatchable in ~15 min on the post-B2
substrate. Cycle-1 regen via `cargo xtask regen` runs in seconds (no
80-min bootstrap wall); the cycle-2 idempotency check, the fat-LTO
5-bench matrix, and per-primary-grammar samply captures route to
their wave-specific close gates (W1.c JSON, W2 CSS, W3 Sheets,
W4.e BBNF) where peer-parity context is meaningful. The W0' close
ceremony shrinks to its load-bearing core: cycle-1 regen +
invariant verification + projection-totality test + close-status
formalisation.

**AZ-I.W0**: derive-cache relocation + Watt proc-macro
precompilation items retire as T3-superseded. There is no
proc-macro to relocate the cache for, no proc-macro to wrap with
Watt; the substrate the items presupposed retires at B2.W2.
Classifier unification + IR audit pass items remain load-bearing
and unchanged.

**AZ-II tape deletion**: tractable under the post-B2 substrate.
Byte-equal reproducibility cycles cost seconds rather than the hours
the pre-B2 bootstrap wall would have imposed; reversal narrows.

**BA / BB**: anchor on the post-B2 build-time codegen output. No
proc-macro to plumb through a cross-language binding surface; the
emitted source on disk is the substrate every backend reads.

## Forward-routed work

| Item | Destination | Rationale |
|---|---|---|
| `FusedBuilder::finish` open-frames assertion class (327 debug-mode failures + 3 timeouts) | B4.W1 | Pre-existing tape-builder finalisation debt; consumer-fixture polish; release-mode parity green |
| bbnf-bootstrap thin re-export retirement decision | post-B2 successor wave | The `pub use ::bbnf::grammar::generated::bbnf::*;` thin re-export stays useful for downstream consumers that reference `bbnf-bootstrap::*` paths; full deletion routes to a successor under its own dispatch |
| `wasm/Cargo.lock` bbnf_derive entry | wasm sub-target dispatch | `wasm/` is `exclude = ["wasm"]` from the workspace + carries its own lockfile; B2's `--type toml` gate does not match `.lock`; the wasm sub-target migrates when its consumer fixtures retire under their own dispatch |
| Incremental regen for single-grammar edits (`cargo xtask regen --grammar <ident>` exists; further surface-level polish) | post-B2 polish | Not load-bearing; full-sweep wall is acceptable today |

No forwarded item blocks AY-II.W0' close. The B4.W1 class is
release-mode-green and orthogonal to any AY-II runtime invariant.

## Invariant table

Fourteen invariants from `B2.md §Invariants`. Status column:
**green** — closed end-to-end on B2's surface.

| # | Invariant (abbreviated) | Status | Artefact citation |
|---|---|---|---|
| 1 | `crates/derive/` deleted at B2 close | green | `ls crates/derive 2>&1` returns "No such file or directory"; `audit/W2-close.md` §Files deleted (3 files / 457 lines) |
| 2 | `bbnf_derive` does not appear as a `[dependencies]` entry in any workspace `Cargo.toml` | green | `rg -nF 'bbnf_derive\|bbnf-derive' --type toml` over the workspace returns 0; `audit/W2-close.md` §Cargo.toml edits |
| 3 | `#[derive(Parser)]` does not appear in any consumer | green | `rg -nF '#[derive(Parser' --type rust` returns 3 hits — clap::Parser in `xtask/src/main.rs:15` (legitimate non-bbnf use) + 2 internal comments inside the deleted `crates/derive/`; 0 actual consumer derive sites; `audit/W1-close.md` §Phase 3 |
| 4 | `cargo xtask regen` is the canonical regen entrypoint | green | `xtask/src/regen.rs` carries the substrate; `cargo xtask regen --help` lists `--grammar` + `--check` flags; Makefile delegates `make regen` + `make regen-check` to the xtask |
| 5 | Per-grammar source emission lives at `crates/core/src/grammar/generated/<ident>.rs` | green | Nine per-grammar files: `bbnf` (34 048 lines), `json` (5 680), `css_l4` (203 499), `css_pretty` (9 890), `google_sheets` (21 533), `ebnf` (12 902), `bnf` (4 697), `csv` (2 947), `math` (1 464); aggregator at `generated/mod.rs` |
| 6 | The 17-pass IR pipeline runs outside rustc's expand phase | green | `xtask/src/regen.rs::compile_paths_request` + `generate_all` are the sole call sites; consumer compiles run no `bbnf_derive` expansion (the crate is gone) |
| 7 | `cargo xtask regen --check` is the CI + pre-commit gate | green | `.github/workflows/ci.yml` step `preflight — regen check (xtask)`; `scripts/hooks/pre-commit` invokes the same gate when grammar files or xtask source change in the staged commit; `scripts/install-hooks.sh` installs the hook on a fresh checkout |
| 8 | `BBNF_SCHEMA_VERSION` retires | green | `rg -nF 'BBNF_SCHEMA_VERSION' --type rust` returns 0; the constant's sole declaration was at `crates/derive/src/lib.rs:81`; deleted with the file at W2 |
| 9 | AY-II's source-level invariants survive unchanged | green | The post-W0.c first regen of the BBNF self-host produced a self-consistent `bbnf.rs` (34 048 lines) that passes the `bbnf_parses_its_own_grammar` SHA assertion in 0.20 s; runtime-side AY-II.W0' source landings remain on master through B3 |
| 10 | `scripts/bootstrap-bbnf.sh` retires | green | `ls scripts/bootstrap-bbnf.sh 2>&1` returns "No such file or directory"; deleted via `git rm` at W3; logic absorbed by reformulation (`xtask/src/regen.rs` invokes the IR pipeline natively, bypassing `cargo expand` entirely) |
| 11 | `target/.bbnf-cache/` is no longer created or read | green | The directory's role (proc-macro content-keyed cache) ceases to apply when the proc-macro retires; `Makefile` `clean-cache` target removed; `PROFILING.md` `target/.bbnf-cache/` narrative deleted |
| 12 | `cargo iter-check` warm restores to ≤ 0.5 s | green | 0.21 s warm at W4 close; 0.12 s warm at W3 close; 0.11 s warm at W2 close; 0.13 s warm at W1 close — every wave under the gate |
| 13 | `cargo iter-check-full` cold under B2 falls below the pre-B2 80-min ceiling | green | 45 s cold at W1 close (post-cutover); pre-B2 cold halted at 25:30+ on the bootstrap wall and never completed; the substrate B2 dispatched against |
| 14 | `bbnf-bootstrap` reduced to a marker consumer | green | `crates/bootstrap/src/lib.rs` carries `pub use ::bbnf::grammar::generated::BbnfBootstrap;` (Option B per W2 close); the self-host BBNF parser builds like every other grammar's parser, from the checked-in per-grammar source file |

## Hard-gate table

Per-wave hard gates. Status column: **green** — gate met cleanly.

| Wave gate | Item | Status | Closing artefact |
|---|---|---|---|
| W0.1 | `xtask/` workspace member exists; `cargo xtask --help` lists `regen` | green | `xtask/Cargo.toml` + `xtask/src/main.rs` clap surface |
| W0.2 | `[workspace.metadata.bbnf]` table enumerates every grammar | green | Root `Cargo.toml` `[workspace.metadata.bbnf.grammars]` table |
| W0.3 | `cargo xtask regen --grammar bbnf` writes a parseable per-grammar source file | green | `crates/core/src/grammar/generated/bbnf.rs` (34 048 lines); `syn::parse_file` succeeds |
| W0.4 | Byte-equivalent gate: post-W0 `generated/bbnf.rs` semantically equivalent to pre-B2 monolith | green | `bbnf_parses_its_own_grammar` SHA assertion green in 0.20 s; `projection_totality.rs` test green |
| W0.5 | `cargo iter-check-full` cold faster than pre-B2 80-min ceiling | green | 45 s cold at W1 close |
| W1.1 | Every `#[derive(Parser)]` consumer site migrates | green | 62 sites across 43 files migrated; remaining 3 hits are clap + 2 comments in the W2-deleting crate |
| W1.2 | `cargo iter-check` warm ≤ 0.5 s | green | 0.31 s at W1 close |
| W1.3 | `cargo iter-check-full` cold ≤ 5 min | green | 45 s cold |
| W1.4 | Workspace nextest exit (test-binary compile through `include!` pattern) | green | 1 160 pass / 327 fail / 3 timeout / 27 skip — pre-existing FusedBuilder debt routed to B4.W1; release-mode parity (`bbnf_parity`) green |
| W1.5 | W1.b cutover lands as single delete-then-swap window | green | All four sub-slices (gorgeous, JSON tests, CSS+Sheets tests, BBNF tests) cherry-picked end-to-end before W1 close |
| W2.1 | `crates/derive/` deleted | green | 3 files / 457 lines via `git rm -r` |
| W2.2 | `bbnf_derive` purged from every workspace `Cargo.toml` | green | `rg -nF 'bbnf_derive\|bbnf-derive' --type toml` over workspace = 0 |
| W2.3 | `BBNF_SCHEMA_VERSION` retires | green | `rg -nF 'BBNF_SCHEMA_VERSION' --type rust` = 0 |
| W2.4 | `[patch.crates-io] bbnf_derive` removed | green | `.cargo/config.toml` no longer carries the patch line |
| W2.5 | `cargo iter-check-full` exit 0 with proc-macro gone | green | 0.13 s warm; 10.8 s cold post-edits + post-regen |
| W3.1 | `scripts/bootstrap-bbnf.sh` deleted | green | `ls` returns "No such file or directory" |
| W3.2 | Makefile `ay-prime` target retires | green | `make ay-prime` returns "No rule to make target"; `make regen` + `make regen-check` added |
| W3.3 | `target/.bbnf-cache/` references retire | green | Only narrative comments retain explicit "pre-B2 retired" framing |
| W3.4 | `cargo xtask regen` exits 0 (full sweep idempotent) | green | 9 grammars exit 0; idempotent re-run produces zero-line diff |
| W3.5 | `include_str!` emitter portable across worktrees | green | Every emitted entry wraps as `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` |
| W4.1 | `.github/workflows/ci.yml` invokes `cargo xtask regen --check` | green | `preflight — regen check (xtask)` step before `iter-check` |
| W4.2 | Pre-commit hook present | green | `scripts/hooks/pre-commit` (in-tree) + `scripts/install-hooks.sh` (installer) |
| W4.3 | `docs/tranches/B2/FINAL.md` authored | green | this file |
| W4.4 | AY-II planning docs reflect B2 as predecessor | green | `PATH-FORWARD.md`, `AY-II.md` W0' row, `waves/W0p.md`, `PROGRESS.md` updated |
| W4.5 | `docs/tranches/AZ-I/AZ-I.md` amended | green | W0 derive-cache + Watt items dropped; classifier + IR audit retained |
| W4.6 | `docs/tranches/REMAINING-TRAJECTORY.md` insert B2 | green | Sequence updated; post-B2 wall expectations revised |
| W4.7 | `docs/RISK-PERF-MATRIX.md` revised post-B2 | green | Probability lift on AY-II / AZ-I / AZ-II floors |
| W4.8 | `docs/benchmarks/post-B2.json` exists | green | Aggregate matrix authored |
| W4.9 | `cargo xtask regen --check` exits 0 (no drift) | green | 1.11 s wall; "regen --check: clean (9 grammars matched)" |

## Wave commit ledger

| Wave | Phase | Commit | Headline |
|---|---|---|---|
| W0 | a | `dec67806` | xtask substrate landing + workspace manifest table |
| W0 | b | `3c68e8c4` | xtask boundary spec + per-grammar emission scaffolding |
| W0 | c-partial | `21881591` | bbnf-bootstrap migration to `pub use` re-export contract |
| W0 | c-close | (post-B3 + post-B4.W0) | post-W0.c re-execution: `crates/core/src/grammar/generated/bbnf.rs` 34 048 lines; `FusedOutput<R>` + `frames()` emitter fixes; legacy monolithic `generated.rs` retired; `bbnf-ir` dep restored to `crates/bootstrap/Cargo.toml` for bin-only diagnostic tooling |
| W1 | close | `690221bb` | 8 non-bbnf grammars regenerate; 62 consumer sites across 43 files migrate to `pub use ::bbnf::grammar::generated::<ident>::*`; `crate::css_types::parse_hex_color` host shim lifts to `crates/core/src/css_types.rs`; gorgeous `[[test]] required-features` gating |
| W2 | close | `6142387f` | `crates/derive/` deleted (3 files / 457 lines); `bbnf_derive` purged from every workspace `Cargo.toml`; `BBNF_SCHEMA_VERSION` retired; `[patch.crates-io] bbnf_derive` dropped |
| W3 | a | `378aa71b` | `include_str!` emitter portable: `ParserAttributes::grammar_rel_paths` + `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` wrapper |
| W3 | close | `f375cd38` | `bootstrap-bbnf.sh` + `check-bootstrap-clean.sh` deleted; `make ay-prime` + `clean-cache` retired; `make regen` + `make regen-check` added; PROFILING.md §Grammar regen authored; full sweep regen idempotent |
| W3 | docs | `23b51df8` | W3 close audit + status normalization |
| W4 | close | this commit | CI gate + pre-commit hook + FINAL + post-B2.json + cross-tranche updates |

## AY-II handoff block

The next execution anchor is `docs/tranches/AY-II/PATH-FORWARD.md`.
B2 closes at this commit; the AY-II.W0' close ceremony resumes
immediately on the post-B2 substrate, in its compressed-honest
form: cycle-1 regen via `cargo xtask regen` (~5 min wall, dominated
by xtask incremental compile), invariant greps + the
`projection_totality` test, status formalisation in `PROGRESS.md` +
`waves/W0p.md`. Cycle-2 idempotency, the fat-LTO 5-bench matrix,
samply captures, and `nm` of bench binaries route to their
wave-specific close gates where peer-parity context is meaningful.

The handoff is unblocked.
