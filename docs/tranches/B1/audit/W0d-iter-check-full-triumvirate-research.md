# B1.W0.d — iter-check-full ceiling research (triumvirate phase)

**Run**: 2026-04-24, worktree `b1-trium-research` at `/Users/mkbabb/Programming/bbnf-wt-b1-trium-research`, branch `b1-trium-research` (read-only). HEAD master `2b6e50bf`. Apple arm64, macOS 25.4.0, pinned `nightly-2026-04-11` (`rustc 1.96.0-nightly 02c7f9bec 2026-04-10`).

## Headline attribution

`cargo iter-check-full` (aliased `check --profile ax-iter --workspace` at `.cargo/config.toml:110`) cold wall is dominated by **two single-rustc, single-core derive-expansion walls that run serially by dependency order**: `gorgeous --lib` (6 `#[derive(Parser)]` sites in one rustc, ≥ 556 s pre-pin; **still ≥ 500 s on the pin** — the `#[derive(Parser)]` code-path is a single-threaded Rust compute, unaffected by `-Zthreads=8`) and `bbnf-bootstrap --lib` (1 `structural` derive site, ≥ 130 s per meta-audit 04 and this run's probe). `--workspace` forces both onto the critical path. The four `iter-check` excludes (`gorgeous`, `bbnf-bootstrap`, `bbnf-analysis`, `bbnf-lsp`) exist precisely because of this attribution; `iter-check-full`, by taking no excludes, re-admits them. **The pin closed the ICE cluster; the ICE remediation is orthogonal to the iter-check-full ceiling.**

File refs: `.cargo/config.toml:106-110` (alias definitions), `crates/gorgeous/src/lib.rs:5-16` (6 `#[cfg(feature = "<grammar>")] pub mod <grammar>;` gates, with `default` activating all six at `crates/gorgeous/Cargo.toml:23`), `crates/bootstrap/src/lib.rs:14-15` (the single `#[derive(Parser)] #[parser(structural)]` site). Meta-audit 04 §Pain-1/Pain-2 (`docs/tranches/meta-audit/04-toolchain-pain.md:32-74`) lists the structural floor `≥ 12-min cold, closer to 15 min`, authored pre-pin; this run's probes (below) are consistent.

## Q1. Current bottleneck attribution under the pin

Cold probes under pinned toolchain, `CARGO_BUILD_JOBS=4`, single-cargo-per-target:

| # | Command | Wall | State | Artefact |
|---|---|---|---|---|
| P1 | `cargo check --profile ax-iter -p tape --lib` | **1.67 s** | fully cold `target/` | `/tmp/b1-research-probe-tape.txt` |
| P2 | `cargo check --profile ax-iter -p bbnf --lib` | **6.51 s** | tape deps warm; first bbnf typecheck | `/tmp/b1-research-probe-bbnf.txt` |
| P3 | `cargo check --profile ax-iter -p bbnf-bootstrap --lib` | **≥ 300 s (killed at 5:00, rustc 99 % CPU single-core inside derive expansion)** | pid 11771 single rustc | `/tmp/b1-research-probe-bootstrap.txt` (empty) |
| P4 | gorgeous `--lib` default features | **not re-probed** (budget) | — | pre-pin 556 s at `W0p-infra-root-cause.md:16` |

P3 raises the meta-audit 04 floor (`meta-audit/04-toolchain-pain.md:20`, which observed "≥130 s still running at cutoff") to **≥ 300 s observed at this pin**, confirming the pin does not move bootstrap's cold wall.

Ranked by criticality to iter-check-full cold:

1. **gorgeous --lib default features** (≥ 500 s, load-bearing). 6 serial `#[derive(Parser)]` expansions inside one rustc; the `syn::parse → compile_paths_request → generate_all → quote!` path is serial within a single macro invocation — `-Zthreads=8` parallelises rustc's front-end across functions but not across sibling-module derive expansions in one rustc process.
2. **bbnf-bootstrap --lib** (≥ 300 s). 1 `structural` derive site; identical single-rustc-single-core signature.
3. **bbnf --lib** (6.51 s on pin vs 8.92 s pre-pin; ~27 % pin-delta). Not on iter-check-full critical path.
4. **tape --lib** (1.67 s). Baseline.

## Q2. ICE cluster under the pin

**Closed.** Zero `rustc-ice-*.txt` emerged from any probe (`find target -name 'rustc-ice-*'` → 0). The pin commit `02c7f9bec 2026-04-10` is past the observed range `9602bda1d 2026-04-05` where the `on_disk_cache.rs:663: cannot decode AttrId` panic reproduced. Pin is a landed lever, **orthogonal to the iter-check-full ceiling** (ICE corrupts incremental state; the derive wall is cold-compute-bound).

## Q3. Cranelift

**Installed** (`rustup component list --toolchain nightly-2026-04-11 --installed` lists `rustc-codegen-cranelift-aarch64-apple-darwin`). **Irrelevant to iter-check-full**: `cargo check` stops at metadata emit (`W0p-infra-root-cause.md:66`); cranelift only shortens codegen phase, which `check` never invokes. Cranelift helps `iter-test-*` (test-binary compile) + `cargo build`. Not a lever for the ceiling. Not re-probed.

## Q4. Fast linker

**lld NOT installed** (`ls /opt/homebrew/opt/lld/bin/` → absent; `brew info lld` → Not installed). `wild` / `mold` also absent. Same scope caveat as Q3: `check` never links. Not a lever for the ceiling.

## Q5. Gorgeous as a structural compile-graph bottleneck

**Yes — still on iter-check-full's compile graph.** `--workspace` without `--exclude` pulls gorgeous as a direct workspace member regardless of dev-deps. `crates/gorgeous/Cargo.toml:22` sets `default = ["bbnf-grammar", "json-grammar", "css-grammar", "ebnf-grammar", "bnf-grammar", "sheets-grammar"]` — activating all 6 feature-gated `#[derive(Parser)]` sites at `crates/gorgeous/src/lib.rs:5-16`. So `cargo check -p gorgeous` or `--workspace` compiles all 6 derive sites, identical cost to pre-d4.

d4 (feature-gating the module imports) did its job mechanically but `default` undid its benefit. d5 removed gorgeous from `crates/core/Cargo.toml [dev-dependencies]` — verified by reading that block; no `gorgeous` line. d6 narrowed `crates/derive/build.rs` to codegen-relevant subtrees — verified (post-d6 contents match `W0p-infra-fix-plan.md` Change 5). **d4+d5+d6 collectively fix iter-check (the excluded alias) and iter-test — they do NOT fix iter-check-full.**

## Q6. Proc-macro content-hash cache

**Does not warm on first cold run of the light probes.** Post-probes-P1+P2 (tape + bbnf lib, neither a `#[derive(Parser)]` site), `find target -name .bbnf-cache` → 0 entries. Cache only populates after bbnf-bootstrap or gorgeous completes the derive-expansion + write at `crates/derive/src/lib.rs:207+`. Once populated, stable grammars hit the cache (`crates/derive/src/lib.rs:177-209`), but rustc still parses + borrowcks + metadata-emits the cached ~30k-line TokenStream per site (~5-10 s each per `W0p-infra-root-cause.md:107` estimate). `scripts/bootstrap-bbnf.sh:28` unconditionally `rm -rf target/.bbnf-cache/` (meta-audit 04 §Pain-6) — orthogonal to iter-check-full which doesn't run the script, but means developer-loop cache state is fragile.

## Q7. rustflags `-Zthreads=8 -Zshare-generics=y`

**Not re-probed** (low-signal: a clean A/B would consume ~25 s for a ~7 % delta on small crates; irrelevant on derive-wall hot spots where the serial macro-expansion path bottlenecks inside one rustc regardless of frontend threading). Keep: no cost, real benefit on richer-parallelism crates. Not a lever for iter-check-full.

## Q8. Remediation space vs the < 5 min cold target

| Remediation | iter-check-full cold Δ | Cost | In B1 scope? |
|---|---|---|---|
| Flip gorgeous `default = []` + `required-features = ["bin-full"]` on `[[bin]]` | **−~500 s** | 2-line Cargo.toml diff | **Yes (Lever 1)** |
| Redefine iter-check-full as close-ceremony-only; routine surface is iter-check + per-exclude fast-paths | **−~11 min** on routine surface; close-ceremony retains full cost | alias rename + doc | **Yes (Lever 2)** |
| Seed `.bbnf-cache/` via `make ay-prime` | 0 s first-cold; ~−90 s/site second-cold | already scoped | **Yes (Lever 3 — B1.W0.d)** |
| Enable cranelift | 0 s on `check`; ~−5-15 % on `build`/`test --no-run` | 2 lines | Orthogonal to iter-check-full |
| Enable lld | 0 s on `check`; ~−10-20 % on `build`/`test --no-run` | `brew install lld` + 5 lines | Orthogonal; host install blocker |
| Watt-wrap `bbnf_derive` | Uncertain; cross-compile IR pipeline to WASM | 2-4 weeks | Deferred to BA/BB |
| Split gorgeous into 6 lib crates | ~3× theoretical via parallel cargo jobs | 1-day refactor; crate-boundary break | Unnecessary given Lever 1 |
| Lift cache to `$XDG_CACHE_HOME` | 0 s first-cold; ~130 s on second-worktree cold | deferred | Routed to BA (`patches/derive-cache-design.md`) |

## Top-3 remediations (ranked by measured impact-to-cost)

### Lever 1 (highest impact, lowest cost) — Flip `gorgeous` `default = []` + add `required-features = ["bin-full"]` to `[[bin]]`

2-line diff in `crates/gorgeous/Cargo.toml`:
- line 22: `default = [...]` → `default = []`
- lines 9-11: `[[bin]]` block gains `required-features = ["bin-full"]`

Workspace `cargo check --workspace` then compiles gorgeous's lib with zero feature-gated modules (modules are `#[cfg(feature = "<grammar>")]` at `crates/gorgeous/src/lib.rs:5-16`; zero features → zero modules → zero derive sites) AND skips the `gorg` binary target entirely (features inactive unless opted in). `gorg` still builds via `cargo install gorgeous` or `cargo build -p gorgeous --features bin-full`.

Expected cold delta: **−500 s** from iter-check-full wall. Recommended by `W0-iter-surface-verification.md:95-98`.

**Critical clarification vs W0-iter-surface-verification line 103**: that doc asserted the `gorg` binary already carries `required-features = ["bin-full"]`. **It does NOT** — verified this run: `crates/gorgeous/Cargo.toml:9-11` has no `required-features`; `crates/gorgeous/src/builtin.rs:18-22` unconditionally references `gorgeous::{json,css,ebnf,bnf,bbnf}` grammar modules. Flipping `default = []` alone would break the binary's check/build. Adding `required-features` to `[[bin]]` is a required companion edit. The plan agent must land both lines or neither.

### Lever 2 — Redefine `iter-check-full`'s role (routine → tranche-close ceremony only)

Downgrade the `iter-check-full` alias at `.cargo/config.toml:110` to a close-ceremony-only label (e.g. `close-check-full` or add a strong comment disambiguating from the routine aliases). Routine surface is `iter-check` + the three named fast-paths `iter-check-lsp`, `iter-check-prettify`, `iter-check-bootstrap` already in B1.W0.b. B1.md invariant 11 calls for a "measured wall-clock ceiling, not exit 0"; give it a concrete number (≤ 20 min cold pre-Lever-1; ≤ 5 min cold post-Lever-1) rather than an open contract.

Evidence: meta-audit 04 §Pain-2 (`04-toolchain-pain.md:71-74`) proposes exactly this — citing that constituent parts (gorgeous ≥ 556 s + bootstrap ≥ 130 s, serialised in two single-core rustcs) put the floor at 12-15 min cold; < 5 min cold without Lever 1 is structurally infeasible.

### Lever 3 — Seed `.bbnf-cache/` with `make ay-prime` (already scoped by B1.W0.d)

Per meta-audit 04 §Pain-4 + B1.W0.d (`waves/W0.md:194-201`): on first clone / post-`cargo clean`, run `cargo check -p bbnf-bootstrap --lib` + (conditionally under `bin-full`) `cargo check -p gorgeous --lib` to populate `target/.bbnf-cache/`. Cache is content-keyed on `(grammar, attrs, ident, BBNF_SCHEMA_VERSION, CARGO_PKG_VERSION)` at `crates/derive/src/lib.rs:144-175`. Once populated, subsequent cold runs skip the 17-pass IR pipeline + 30k-line TokenStream construction; rustc still re-parses/borrowchecks the cached TokenStream (~5-10 s/site), but the ~90-150 s/site cold compute vanishes.

Cache does not populate on the light probes — `find target -name .bbnf-cache` → 0 entries post-P1/P2 this run. Lever 3 only helps second-cold and multi-worktree flows; it does not fix first-cold ceiling.

## Honest stance: can iter-check-full close < 5 min cold?

**Not within B1 scope without Lever 1.** The structural floor is gorgeous ≥ 500 s + bbnf-bootstrap ≥ 300 s (both single-rustc-single-core, serialised by dependency order). `-Zthreads`, cranelift, lld are all orthogonal: the derive-expansion path runs as serial Rust code inside rustc, not through rustc's front-end threads; cranelift handles codegen (which `check` never invokes); lld handles linking (ditto). The pin closed the 93-ICE cluster (landed lever) but is orthogonal to the cold wall.

Ceiling projections vs B1's invariant 11:

| State | iter-check-full cold |
|---|---|
| Current master (d4+d5+d6 landed) | **≥ 12 min** (meta-audit 04 conservative floor) |
| + Lever 1 (gorgeous default=[]) | **~3-5 min** (bbnf-bootstrap + analysis + lsp + ~30 fast members) |
| + Lever 1 + Lever 3 (primed cache) | **~1-2 min** (rustc TokenStream re-parse only) |
| Lever 2 only (alias re-scope) | routine close-gate < 1 min; ceremony alias retains ≥ 12 min |

**Recommendation: land all three in B1.W0.d.** Lever 1 completes d4's intent (d4 gated the grammar modules behind features but left `default` activating all six — the d4 benefit was only reachable by explicitly pinning `default-features = false`, which no workspace member does); it is a 2-line diff with ~500 s impact. Lever 2 corrects the semantic mis-classification of iter-check-full as a routine surface. Lever 3 is already scoped.

## Probe evidence paths

- `/tmp/b1-research-probe-tape.txt` — P1 tape cold 1.67 s.
- `/tmp/b1-research-probe-bbnf.txt` — P2 bbnf lib cold 6.51 s (vs pre-pin 8.92 s).
- `/tmp/b1-research-probe-bootstrap.txt` — P3 bootstrap cold empty-at-kill; pid 11771 killed at 5:00 elapsed, 99 % single-core CPU, single rustc inside derive expansion.
- `docs/tranches/AY-II/audit/W0p-infra-root-cause.md:5-16,46,107` — 556 s gorgeous cold + 6-derive-site attribution.
- `docs/tranches/AY-II/audit/W0-iter-surface-verification.md:62-98` — default-features-activates-all-six finding + suggested remediation.
- `docs/tranches/meta-audit/04-toolchain-pain.md:55-74,55-117` — Pain-2 iter-check-full ≥ 12 min floor; Pain-4/Pain-8 ay-prime entry-point.
- `docs/tranches/B1/TOOLCHAIN-SOTA.md:359-398` — cranelift / lld scope-of-benefit on codegen + link, not check.
- `docs/tranches/B1/B1.md:95-109` — invariants 10/11/12.
- `docs/tranches/B1/waves/W0.md:194-244` — B1.W0.d spec + hard gate.

## Blocker escalations

None. Probes completed within budget; measurements evidence-grounded. Lever 1 requires a 2-line edit (flip `default`; add `required-features` to `[[bin]]`); the plan agent should verify gate 7 of `W0p-infra-fix-plan.md:305-308` (`cargo check -p gorgeous --no-default-features --features json-grammar` ≤ 150 s) still holds post-Lever-1 flip. Lever 2 is a doc + alias diff; zero code risk. Lever 3 is already scoped by B1.W0.d. Note: the `P3` probe (bbnf-bootstrap cold) raised the meta-audit floor to ≥ 300 s observed; the pin did not move bootstrap's wall. That measurement is a first-run empirical truth — any plan that budgets iter-check-full at a number below ~5 min without Lever 1 is structurally wrong.
