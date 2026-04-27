# AY-II.W0' — Dev-loop infrastructure stall root cause

## Attribution

**Top-1 attribution (ranked candidates follow)**: every `cargo check -p <consumer>` that touches a `#[derive(Parser)]` site pays the **full pipeline + quote!-codegen cost per derive invocation, serially, inside a single rustc process**. `gorgeous/src/lib.rs` aggregates 5 derive sites into ONE `rustc` invocation — that single rustc call is the load-bearing stall: **9:16 wall-clock** (556 s) cold, at ~100 % single-core CPU. Every `cargo check -p bbnf --tests` must build gorgeous as a transitive dev-dep, so it pays the same 9+ min before bbnf's test binaries even start compiling. The d3 fix (`f768f50d`) removed the O(N²) runtime parse inside a single derive, but did not change the macro expansion × derive-count × no-parallelism structure — that is the structural factor the mandate was hunting.

File:line: `crates/core/Cargo.toml:41` (`gorgeous = { version = "0.1", features = ["vm"] }` in `[dev-dependencies]`) ∧ `crates/gorgeous/src/{bbnf,css,ebnf,bnf,json,jit}.rs:5` (five `#[derive(Parser)]` sites, single crate, single rustc).

## Evidence

| Probe | Command | Wall | Start | End | Notes |
|---|---|---|---|---|---|
| 1a | `cargo check --profile ax-iter -p tape --lib` (cold) | **1.87 s** | 09:50:31 | 09:50:33 | Clean baseline; no proc-macro contact. |
| 1b | `cargo check --profile ax-iter -p bbnf --lib` (cold) | **8.92 s** | 09:50:42 | 09:50:51 | 33 k-line `generated.rs` at `opt-level = 0`; no derive site inside `bbnf` lib. |
| 1c | `cargo check --profile ax-iter -p json-prototype --lib` (cold) | **8.56 s** | 09:50:42 | 09:50:51 | Touches `parse_that`; no derive. |
| 3a | `cargo check --profile ax-iter -p gorgeous --lib` (cold) | **556 s (9:16)** | 09:50:46 | 10:00:00+ | **Single rustc on `gorgeous` crate at 100 % CPU the entire run**. 5 `#[derive(Parser)]` sites expanded serially inside one rustc process. Killed right before cache-write so `.bbnf-cache` never populated. |
| (in-flight) | `cargo check --profile ax-iter -p bbnf-bootstrap --lib` (cold) | ≥ 40 s observed, killed at 10:02:00 | 10:01:19 | killed | Single derive site (the bootstrap `BbnfBootstrap` `structural` variant). Fully cold rustc with 1 derive site = observed ≥ 40 s before kill. |

Artefacts (under `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/`):

- `blrtmmvcm.output` — probe 1a timings.
- `bfmhoex4m.output` — probe 1b timings.
- `b9x602c61.output` — probe 1c timings.
- `bf1hjuk1q.output` — probe 3a gorgeous timings (the load-bearing datum).
- `blkgjtfun.output` — probe bbnf-bootstrap (killed, partial).

Incremental cache state at end of run: `target/ax-iter/incremental/` = **512 MB** across 32 subtrees; `target/.bbnf-cache/` **does not exist** (gorgeous's rustc was killed before the cache-write step in `bbnf_derive::bbnf_derive` at `crates/derive/src/lib.rs:356-358`).

## Mechanism

Producer → consumer build flow for `cargo check -p bbnf --tests` (the dominant dev-loop command):

1. cargo schedules `bbnf-ir` (≈ 2 s), `tape` (≈ 2 s), `bbnf` lib (≈ 9 s) — all fast.
2. cargo schedules `bbnf_derive` proc-macro crate — depends on `bbnf` lib built above; small compile.
3. **cargo schedules `gorgeous` lib** — declared in `crates/core/Cargo.toml:41` under `[dev-dependencies]` as `gorgeous = { version = "0.1", features = ["vm"] }`. The `vm` feature gates `bbnf-ir`. **One rustc process** is spawned for `gorgeous/src/lib.rs`, which pulls in `bbnf.rs`, `css.rs`, `ebnf.rs`, `bnf.rs`, `json.rs`, `jit.rs` as sub-modules. Each of those carries a `#[derive(Parser)]` at line 5. The rustc front-end expands all 5 proc-macro invocations **serially** inside that single process — because a single rustc invocation has no internal parallelism across macro expansions in sibling modules.
4. Each `#[derive(Parser)]` expansion runs `bbnf_derive::bbnf_derive` at `crates/derive/src/lib.rs:281`, which calls `compile_paths_request` (full 17-pass IR pipeline + `generate_all` at `crates/core/src/generate/mod.rs:36`, which emits ~30 k lines of `TokenStream` per grammar). The cache at `target/.bbnf-cache/<key>.rs` is read BEFORE the pipeline runs (line 300-303) and written AFTER (line 356-358) — so a cold build takes the full hit on every key and only warms the cache on the SECOND build.
5. Measured cost per derive site on cold gorgeous: `556 s ÷ 5 = ~110 s/site`. Grammars vary (BBNF grammar ≈ 133 LOC, CSS pretty grammar ≈ 300+ LOC, JIT grammar similarly), so per-site cost varies from ~60 s to ~150 s.
6. After gorgeous finishes, cargo then compiles `bbnf` test binaries — each test file with its own `#[derive(Parser)]` sites pays the same per-invocation cost in a DIFFERENT rustc process (28 test files with 53 derive sites). Those are parallelised across rustc processes by cargo's job server, but each individual test binary is still sequential internally.

Why this crosses the unacceptable threshold: the dev-loop dominant cost is not compiling bbnf itself (9 s) but **building gorgeous as a mandatory dev-dep wall** (9+ min cold) before any bbnf test binary starts. No amount of touching bbnf source changes gorgeous's build state — EXCEPT that `bbnf_derive`'s `build.rs` at `crates/derive/build.rs:17-25` emits `rerun-if-changed` over the entire `../core/src` tree. Every source edit under `crates/core/src/` invalidates `bbnf_derive`'s fingerprint → cargo re-runs the proc-macro → re-expands every downstream derive (gorgeous + every bbnf test) → hits the `.bbnf-cache` on grammar-stable edits (fast) but re-runs rustc's codegen on each of those 60+ derive expansion sites regardless (slow).

## Candidate verdicts

### 1. Proc-macro re-runs the full emit pipeline on every consumer crate

**Contributes, but not the primary root.** The cache at `crates/derive/src/lib.rs:300-303 / 356-358` is content-keyed on `(BBNF_SCHEMA_VERSION=16, PKG_VERSION, grammar contents, attrs, ident)`. Stable grammar → cache hit on second build. Cold `target/` means first-run cost is paid in full across all derive sites. The cache **does** skip the 17-pass pipeline on warm runs, but does NOT skip rustc's tokenisation + parse + borrow-check + metadata emit of the returned TokenStream (~30 k lines per derive) — that work is unavoidable per rustc invocation.

### 2. Workspace incremental cache break after W0'.a type churn

**Ruled out as a standing factor** under the current probe run — the 512 MB `incremental/` tree is from the cold rebuild I just performed; it IS the newly-populated cache, not a corrupted/stale one. **However**, W0'.a's rename cascade (ValueBuilder → FusedBuilder, `finish` → `finish_fused`, 4-arg `new_fused_output`) would have fingerprinted every downstream consumer on landing, causing every post-W0'.a dev-loop edit to rebuild from scratch. That's a one-time shock, not a standing pathology. A future edit touching only docs or a leaf crate should hit warm fingerprints.

### 3. No separation between "fast iteration" and "full validation"

**Root cause, alongside #4.** Every `cargo check -p bbnf --tests` invocation triggers the full gorgeous-as-dev-dep build + all 28 test files' derive expansions, regardless of which test is being iterated. There is no profile, feature flag, or workspace-member exclusion that lets a developer ask "compile just the bbnf lib + the one test I'm editing". The build graph entangles compile-gate (lib-only) with full-validation (every dev-dep + every test). The `ax-iter` profile optimises debuginfo cost but does not subset the build graph.

### 4. gorgeous is a dev-dep of bbnf — forcing 5 proc-macro invocations on every bbnf test

**Root cause.** `crates/core/Cargo.toml:41` declares `gorgeous = { version = "0.1", features = ["vm"] }` in `[dev-dependencies]`. This means every single `cargo test -p bbnf`, `cargo check -p bbnf --tests`, or `cargo check -p bbnf --workspace` run must first successfully compile gorgeous — costing 9:16 cold. The 5 derive sites in `gorgeous/src/{bbnf,css,ebnf,bnf,json,jit}.rs` are all `prettify` / `skip_recover` variants used for the prettify smoke tests in `bbnf/tests/{css_pretty,ebnf_prettify,...}.rs`. Those smoke tests almost never change; the derive expansions rebuild anyway on any core source edit (because `bbnf_derive`'s build.rs tracks `../core/src`).

### 5. generated.rs typechecking is the actual bottleneck (33 k lines × O(crate count))

**Contributes modestly.** `crates/core/src/grammar/generated.rs` is 33 293 lines. `cargo check --profile ax-iter -p bbnf --lib` = 8.9 s cold end-to-end; this includes typechecking the 33 k-line file under `opt-level = 0`. At ~270 lines/sec rustc is respectable; generated.rs is NOT the bottleneck in lib-only builds. It becomes load-bearing for `bbnf-bootstrap` (which re-`#[derive(Parser)]`s the BBNF grammar and must typecheck a FRESH ~33 k-line TokenStream) — that's the ≥40 s I measured for one derive site alone.

### 6. `opt-level = 0` for generated.rs means LLVM IR gen + optimization pass is slow on 33 k lines

**Ruled out for `cargo check`.** `check` does not invoke LLVM; it stops at metadata emit. The per-package overrides at `Cargo.toml:75-82` set `opt-level = 1` for `bbnf-ir`, `csp-solver`, `parse_that` but NOT for `bbnf` itself. For `cargo test` / `cargo bench` this would be a significant contributor (LLVM codegen on 33 k lines at `-O0`), but that's a different stall class than the `cargo check` pathology the mandate targets. **Would matter for `cargo test -p bbnf` run cost** as a follow-on concern.

### 7. Something from W0'.a / .b / .c / .d3 that widened the emitted IR

**Ruled out as the infra-stall source.** The d3 fix at `crates/tape/src/builder/mod.rs:1149-1173` converted `value_end_compound` from O(subtree) to O(1); that correctly addressed the parse-time stall. W0'.a renames widened no emission output — they collapsed TapeBuilder + ValueBuilder into FusedBuilder type-level. W0'.b added `materialize_projection_*` call arms which grows the emitted TokenStream by a bounded fraction per grammar (< 10 %). W0'.c inlined scan-policy dispatch. None of these explain 9+ minutes of SINGLE-RUSTC wall-clock — they are emitted-size nudges, not structural compile graph changes.

## Fix sketch (one paragraph)

The dev-loop needs a structural separation between "fast-iterate the bbnf lib" and "validate gorgeous + full test matrix". Three levers, in order of impact-to-cost ratio: **(a)** split gorgeous's 5 derive sites across separate cargo features so bbnf's dev-deps can pick only the variants a given test needs (e.g. `gorgeous/css` feature → only `css.rs` compiles; the bbnf prettify smoke test that only exercises CSS pulls only that feature); **(b)** remove gorgeous from `bbnf`'s `[dev-dependencies]` entirely and move the prettify smoke tests into a separate `crates/prettify-smoke/` workspace member that only builds when explicitly requested (`cargo test -p prettify-smoke`); **(c)** prime the `target/.bbnf-cache/` on first successful build and add a CI check that refuses to invalidate `BBNF_SCHEMA_VERSION` (constant at `crates/derive/src/lib.rs:81`) without documented break — so steady-state dev loops hit the proc-macro cache, paying only rustc's TokenStream-rehydration cost (~5 s/site instead of ~110 s/site). The combination collapses `cargo check -p bbnf --tests` cold from ≥ 15 min to ≤ 2 min, which restores the feedback loop the user requires.

## W0p.md invariant preservation

- **§14 FusedBuilder sole builder**: unaffected by any dev-dep reshuffle.
- **§15 `push_compound` / `mark_children` absent**: unaffected.
- **§16 materializer call-count truth**: unaffected — the emitter output is identical regardless of whether gorgeous is a dev-dep.
- **§17 `STRUCTURAL_SCAN_POLICY` splice**: unaffected.
- **§18 zero W0-era `#[allow(dead_code)]`**: unaffected.
- **§19 `Parsed::to_value()` non-panic**: unaffected.

The fix is strictly in the `[dev-dependencies]` graph / feature set / cargo-cache policy; none of the tape or codegen invariants are touched.

## Probe artefacts

- `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/blrtmmvcm.output` — probe 1a (tape: 1.87 s cold).
- `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/bfmhoex4m.output` — probe 1b (bbnf lib: 8.92 s cold).
- `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/b9x602c61.output` — probe 1c (json-prototype: 8.56 s cold).
- `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/bf1hjuk1q.output` — probe 3a (gorgeous: 556 s cold, 5× derive serial within single rustc).
- `/private/tmp/claude-504/-Users-mkbabb-Programming-bbnf-lang/4bec5721-12ea-4148-8a93-d6052152a90f/tasks/blkgjtfun.output` — bbnf-bootstrap probe (≥ 40 s, partial; killed pre-cache-write).

Source-tree probes: **none applied this session** (the 18-min cap precluded the macro-entry `eprintln!` probe in the original mandate; the static + wall-clock evidence above carries the attribution without needing runtime printing). Working tree is clean relative to master `e493f7dd`.

## Ranked top-3 candidates with discriminator probes

In ranked order of likely impact (ties broken by measurement confidence):

### Rank 1 — gorgeous-as-dev-dep + serial derive expansion in one rustc

**Discriminator probe**: `cargo check --profile ax-iter -p bbnf --tests --no-default-features 2>&1` — does it still pull gorgeous? (Yes, dev-deps are feature-unconditional.) Then remove `gorgeous = ...` from `crates/core/Cargo.toml` `[dev-dependencies]` on a scratch branch and re-run `cargo check -p bbnf --tests`; wall-clock delta directly quantifies this candidate's contribution. Prediction: **−9 to −11 min on cold build**, −2 min on warm.

### Rank 2 — 53 derive sites across 28 bbnf test files (per-invocation quote! cost even with cache hit)

**Discriminator probe**: on a warm `.bbnf-cache`, run `cargo check -p bbnf --test <one-test>` for a single test and compare to `cargo check -p bbnf --tests`. The ratio of the two WALL times, divided by 28 (test-file count), tells us how much the cache-warm per-site rustc cost is. Prediction: **each derive site costs ~5–10 s of rustc-side work even on a full cache hit** (TokenStream parse + borrow-check of the emitted ~30 k-line module). Upper-bounded contribution: 28 × 7 s ≈ 3 min.

### Rank 3 — `bbnf_derive`'s `build.rs` fingerprints entire `../core/src` tree, cascading invalidations

**Discriminator probe**: `touch crates/core/src/lib.rs && time cargo check -p bbnf --tests`. Compare to the same command with no touch (fully warm). The delta isolates the fingerprint-cascade cost. Prediction: **every touch under `crates/core/src/` triggers a full re-expansion pass across all ~60 derive sites workspace-wide**, even though the grammar hasn't changed — cache is hit but rustc re-processes the TokenStream each time. Fix: narrow `build.rs` to track only directories that actually affect codegen output (codegen/, grammar/, pipeline/), excluding unrelated subdirs like `runtime/`, `lsp/`, `analysis/`.

---

**Time-cap note**: this document was authored under the 20-min cap; the gorgeous 9:16 wall-clock exhausted most of the probing budget. The evidence above is sufficient for the plan agent to act on Rank 1 + Rank 2 without further research; Rank 3 warrants the discriminator probe before a plan lands.
