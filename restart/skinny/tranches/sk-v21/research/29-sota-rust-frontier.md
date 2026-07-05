# SOTA Rust Frontier — mid-2026, for a better-than-SOTA parser generator

Current stable is Rust 1.96.1 (2026-06-30); 1.96.0 went beta 2026-05-28 on the standard six-week
cadence. The workspace's toolchain pin, `nightly-2026-04-11` (`rust-toolchain.toml:39`), resolves
to `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)` per the pin's own probe note
(`rust-toolchain.toml:11-16`) — meaning the pin is now roughly two stable releases and one edition's
worth of compiler-internals churn behind head-of-nightly, consistent with the charter's own
"~12 weeks stale against its own 4–6-week cadence" framing. The repo is already split on edition:
every crate under `crates/` (core, bootstrap, analysis, egraph, ir, bbnf-path-ts, bbnf-path, lsp,
csp-solver, simd-scan, ser, gorgeous, egraph-derive) plus `wasm/` and `xtask/` sit on
`edition = "2024"` (e.g. `crates/core/Cargo.toml:4`), while `skinny/Cargo.toml:19` alone still
declares `edition = "2021"` with `rust-version = "1.78"` (`skinny/Cargo.toml:22`), and every
`skinny/crates/*/Cargo.toml` inherits that via `edition.workspace = true` /
`rust-version.workspace = true`. Skinny is the sole 2021 holdout the charter names it as.

## (a) Edition 2024: what to adopt, what to watch

Edition 2024 is no longer a frontier bet — it shipped with Rust 1.85 in February 2025, so by
mid-2026 it is well over a year into stable and is the edition every actively-maintained crate on
crates.io targets. Migrating skinny off 2021 is a mechanical `cargo fix --edition` pass followed by
manual cleanup of anything the fixer declines to touch (doctests, build-script-generated code,
macro-generated code) — the same caveat that applies to the already-migrated `crates/` tree, so the
skinny migration has a same-repo precedent to mirror rather than invent.

The idioms actually worth carrying forward, ranked by relevance to a codegen-heavy workspace:

1. **RPIT lifetime capture.** In 2024, `-> impl Trait` in return position now captures all lifetime
   parameters in scope by default (previously only explicit `+ 'a` bounds were captured). This
   matters directly for combinator-return-position APIs in `crates/core` and any parser-generator
   surface that returns `impl Parser<...>` — 2021-era code relying on the old non-capturing default
   to elide a lifetime will now over-capture and can trip "cannot borrow while also borrowed"
   diagnostics at call sites that used to compile; the fixer inserts `use<...>` precise-capturing
   bounds where it detects an issue, but generated code (the `crates/core/src/grammar/generated/*`
   tree) that hand-emits combinator signatures should be audited by hand rather than trusting the
   fixer against generated output — the fixer does not understand code-generator provenance.
2. **`use` resolution/prelude changes** in 2024 are the standard trap when merging a 2021 crate
   (skinny) into a workspace whose other members are already 2024: implicit imports that
   the 2021 rules allowed silently can become ambiguous or unresolved. Run the fixer per-crate,
   not workspace-wide, so failures are attributable to the exact skinny crate.
3. **`gen` becoming a reserved-ish keyword context** and other minor lexical changes are unlikely to
   bite this codebase (no crate appears to use `gen` as an identifier), but are worth a
   `grep -rn '\bgen\b'` sweep over `skinny/crates/` before the edition bump lands, since the fixer
   only rewrites what it can prove necessary and silently-still-compiling-but-wrong code is exactly
   the failure mode the charter's "no silent epsilon" discipline (`feedback_no_silent_epsilon.md`)
   warns against in the lowering context and generalizes here.

Net: edition 2024 for skinny is not a frontier risk, it is closing a known gap against the rest of
the workspace, and the migration recipe is "do what `crates/` already did."

## (b) `std::simd`/portable-simd vs `core::arch::aarch64`: no change indicated

`std::simd` (portable-simd) remains nightly-only in mid-2026 with no stabilization date in sight.
Community commentary as recent as November 2025 states plainly that portable-simd "will remain
nightly-only for the foreseeable future," citing unresolved API surface (lane-count restrictions,
mask element types, swizzle semantics) that the language team is unwilling to freeze prematurely.
The practical alternatives developers reach for on stable are `wide` and `pulp`, neither of which
this workspace depends on (`grep` for `portable-simd|packed_simd|std::simd` across `crates/`
returned nothing).

This is moot for bbnf-lang's emitted kernels because the workspace has already made the correct
choice: generated code hand-rolls `core::arch::aarch64` NEON intrinsics directly, with an
`core::arch::x86_64` branch alongside it. Concretely, `crates/core/src/grammar/generated/math.rs`
uses `core::arch::aarch64::*` at line 381 and again at 411/482, with the SSE/AVX counterpart at
lines 428 and 514; the identical pattern repeats at
`crates/core/src/grammar/generated/google_sheets.rs:3664,3688-3694,3711,3765,3797`. This is
precisely the dav1d discipline the charter invokes: hand-specialized intrinsics per target arch in
the hot emitted path, not a portable abstraction layer. `std::simd` would only ever be attractive
here as a *prototyping* convenience (write once, get both backends) before hand-specializing the
final NEON/AVX kernels — and even for that role its nightly-only status and unstable API make it a
poor fit for code meant to be emitted and read by humans auditing generated output (the
`inspect-generated-output` discipline). Recommendation: keep `core::arch` intrinsics as the
emission target for both platforms; do not adopt `std::simd` at any layer of the emitter.

## (c) Nightly-only compiler features: parallel front-end, Cranelift, `-Zshare-generics`, and the ICE picture

**Parallel front-end.** As of July 2026 the parallel rustc front end is still gated behind
`-Z threads=n` and unstable; the Rust project's own 2026H1 goals page states the 2024 target of
shipping it to stable was missed, and the April 2026 project-goals update frames current work as
"documenting and enhancing support of surrounding tools (rustc-perf, Cargo)" as a *precursor* to
beginning the stabilization process — i.e., stabilization has not started, only the groundwork for
it. This directly corroborates what the workspace already learned empirically: `.cargo/config.toml`
records that `-Zthreads=8 -Zshare-generics=y` were live in commit `416dcf76` and were measured by
"Audit α" (`docs/audit/2026-04-25-deep-audit/AUDIT-A-B1-retrospective.md`, referenced at
`.cargo/config.toml:229`) to cause a 28× warm-build regression on `cargo iter-check` (0.14s → 3.88s),
attributed to rustflags-fixed setup cost that small-crate warm checks cannot amortize
(`.cargo/config.toml:227-246`). Both flags were removed. Nothing in the current external
frontier picture reopens that verdict for the *warm-iteration* regime this workspace optimizes for
(`feedback_iter_profile_always.md`); `-Zshare-generics`'s benefit remains "strictly cold +
cross-crate-generic" per the same comment, so it only becomes worth re-measuring if/when a genuine
cold-wall benchmark (full workspace `cargo build --release` from a clean target dir) becomes a
tracked metric — which the charter's benchmark discipline (cold-only, single-invocation) actually
supports revisiting, just not under `iter-check`.

**Cranelift backend.** `rustc_codegen_cranelift` remains a dev-profile-only proposition: the Rust
project's "production-ready cranelift" 2025H2 goal reports ~20% codegen-time reduction and ~5%
total clean-build speedup on larger projects, explicitly scoped to debug builds — release/bench
workloads stay on LLVM, which is exactly how `.cargo/config.toml:284-291` already stages it (a
commented-out `[profile.ax-iter] codegen-backend = "cranelift"` block, gated on "measure before
enabling" once the pin is verified against the current ICE reproducer). This workspace's own
staging is already aligned with upstream's own scoping of the feature; there is no new external
signal to activate it beyond doing the verification run the comment already calls for.

**ICE landscape.** A 2026 empirical study of rustc-specific bugs found front-end panics account for
~38.9% of observed ICE symptoms (with roughly 14% triggered by *valid* programs), and type-system
(~25%) plus MIR-optimization (~15.8%) issues dominate the crash categories — i.e., ICEs remain a
live, non-trivial tax on any team riding nightly, not a solved problem in mid-2026. This validates
the toolchain-pin file's own framing: the 93 `rustc-ice-*.txt` files produced at master root between
2026-04-15 and 2026-04-22, all panicking at the identical `on_disk_cache.rs:663: cannot decode
AttrId` incremental-cache-decode site (`rust-toolchain.toml:8-10`), are a specific instance of the
front-end-panic class the study quantifies, not an anomaly. `rust-lang/glacier` continues to run
nightly ICE regression-tracking daily, meaning any nightly bump should be bisected against a fixed
ICE reproducer exactly as `rust-toolchain.toml:24-27` already prescribes, not merely bumped to
"whatever is newest."

**Verdict on the nightly pin.** Nothing in the mid-2026 frontier removes the need for nightly:
`-Zshare-generics`, cranelift-preview, and `rust-src`/`-Zbuild-std` are all still nightly-gated with
no stabilization imminent. The pin itself is legitimately stale relative to its own declared
4–6-week cadence (12 weeks and counting), and given stable has moved from ~1.94 to 1.96.1 across
that gap, a bump-and-bisect cycle (per the toolchain file's own upgrade procedure,
`rust-toolchain.toml:19-27`) is due — but the frontier research surfaces no new nightly feature the
project should additionally pin for; the existing feature set (parallel front-end experiment,
cranelift-preview, `-Zshare-generics`, `-Zbuild-std`) is still the complete relevant menu.

## (d) Arena/allocator SOTA and zero-copy strings

Bumpalo remains the dominant bump allocator by adoption (most-downloaded arena crate on crates.io),
with an ~11-instruction non-atomic fast path; it is explicitly not thread-safe (a single
`fetch_add` for a shared bump pointer is called out as a "significant slowdown" relative to that
fast path in community benchmarking). `blink-alloc` is the credible SOTA alternative: it offers both
a single-threaded mode and a `SyncBlinkAlloc` variant that shares one memory chunk across threads
with drop support, which bumpalo does not provide. "Oxcart" as a named, standalone crate does not
resolve on crates.io under mid-2026 search — the closest and far more relevant real-world data point
is `oxc_allocator`, the arena allocator underneath the OXC JavaScript toolchain, which allocates
every AST node into a single bump arena for the entire parse — architecturally the closest existing
analogue to what bbnf-lang's own emitted-parser arena strategy should look like, and worth a direct
source read (not just benchmark numbers) given oxc's design goal (fast JS/TS parsing) is a structural
sibling of this project's own. None of `bumpalo`, `oxcart`, or `blink-alloc` appear in any workspace
`Cargo.toml` today (grep returned no hits), so this is greenfield territory, not a migration.

For zero-copy string handling, the two live contenders are `compact_str` (24-byte inline SSO,
transparent heap fallback for longer strings) and `smol_str` (SSO plus cheap `Clone`, same
`size_of` as `String`). Community framing places `smol_str` as the SSO-first choice and `arcstr` as
the choice when minimizing `size_of` is the priority over SSO; `compact_str`'s selling point is that
construction from a `&str` need not round-trip through a heap allocation, which is the property that
actually matters for a hot parse path materializing many short identifier/token spans. Given this
project's already-stated preference for direct-to-struct construction and zero value-discarding
(`feedback_direct_to_struct.md`, `feedback_no_value_discard.md`), `compact_str`'s no-roundtrip
guarantee is the more relevant property than `smol_str`'s cheaper-clone framing, if/when the tape
or AST layer needs an owned small-string type distinct from a borrowed `&str` slice into source.

## (e) Bench and profiling tooling

The workspace has already made most of this decision correctly and inconsistently across its two
trees. `crates/core/Cargo.toml:86`, `crates/lsp/Cargo.toml:27`, and `crates/simd-scan/Cargo.toml:17`
all depend on `divan = "0.1"`, while `skinny/Cargo.toml:42` still depends on
`criterion = { version = "0.5", features = ["html_reports"] }` (consumed at
`skinny/crates/bbnf-bench/Cargo.toml:33`). This is the same 2021-vs-2024 fault line as the edition
split — skinny is the outlier holding an older-generation tool. Criterion remains the field's
"gold standard" for statistically rigorous regression detection with HTML reports, but Divan is
positioned as the lighter, faster-iterating alternative purpose-built for the kind of tight
edit-benchmark loop this project's `iter_profile_always` discipline demands; the two are not
mutually exclusive in general, but a single workspace running both is an unnecessary second harness
per the project's "single X" disciplines (`feedback_bench_sequential.md`,
`feedback_bench_single_run.md`) — folding skinny's benches onto `divan` to match `crates/` removes a
maintained-twice surface.

`tango-bench` is worth naming explicitly because its methodology is structurally suited to exactly
the "cold-parse fitness" framing the brief calls out: it runs paired benchmarks, executing two
candidate implementations *simultaneously* rather than sequentially, which the project claims makes
it far more sensitive to small regressions (a 1% change detectable in ~1 second, 9 times out of 10).
That sensitivity profile is built for A/B-ing two versions of the same kernel (e.g., a NEON rewrite
against its scalar predecessor) rather than for the standing cold-single-invocation discipline this
project already enforces (`feedback_no_warm_benches.md`, `feedback_bench_sequential.md`); paired
execution is definitionally a warm/interleaved methodology and is in tension with the project's
"cold-only" rule, so `tango-bench` reads as a tool for pre-merge kernel-comparison prototyping (in
a throwaway worktree, discarded after the comparison), never as a replacement for the standing cold
Divan/Criterion suite that gates real numbers into documentation.

For profiling, `samply` remains the correct default on macOS (already the project's standing choice
per `feedback_samply_symbols.md`): it renders into the Firefox Profiler UI and needs
`debug = true` plus an interactive (non-`--save-only`) run for symbol resolution, exactly as the
existing project memory already states. The frontier finding worth adding is the ceiling above
samply: Instruments is described in mid-2026 commentary as strictly more capable on Apple Silicon
specifically because it reaches hardware performance counters, GPU counters, and Neural Engine
events that samply's sampling model cannot see, at the cost of being macOS-only and less scriptable.
The pragmatic split reported externally — samply for rapid flamegraph iteration, a
hardware-counter tool (Instruments, or a `poop`-class cycle counter) for the final cycle-level tuning
pass once a kernel is a finalist — matches the "beat lightningcss" bar this project has set for
itself: samply narrows the search, Instruments (or its command-line hardware-counter equivalent)
verifies the final number against a real IPC/cache-miss floor, not just wall-clock.

## (f) Build ergonomics: workspace unification, sccache, cargo-hakari

`cargo-hakari` is the standard tool for workspace feature-unification: it builds a synthetic
`workspace-hack` crate holding the union of feature sets each dependency is built with across the
workspace, avoiding cargo recompiling the same dependency multiple times under different feature
combinations, with the project's own claim of up to 100× improvement on individual `check`/`build`
invocations and 1.7×+ cumulative. `cargo-rail` was surfaced as a newer alternative explicitly
avoiding the synthetic-crate model hakari uses, trading a maintenance-lane concern for a
different one; given this project's `general_infra_crates` and `no-god-modules` sensibilities, a
synthetic workspace-hack crate is at least philosophically the kind of "kitchen sink" the project's
own feedback ledger warns against in spirit, even though its purpose (feature unification, not
logic) is orthogonal to that concern — worth flagging as a design tension rather than a blocker.
Neither `hakari` nor `sccache` appear configured anywhere in the repo today (no `.config/hakari.toml`,
no `RUSTC_WRAPPER` reference found in `.cargo/config.toml`), so both remain open opportunities rather
than migrations. Given the workspace already measured that build-flag-level interventions
(`-Zthreads`, `-Zshare-generics`) regress the *warm* `iter-check` loop this project actually lives in
day to day (`.cargo/config.toml:227-246`), any hakari/sccache adoption should be benchmarked against
that same warm-iteration metric before being kept, not assumed beneficial from its cold-build
marketing numbers.

## Summary judgment

Nothing found in this pass overturns a standing decision the workspace has already made through its
own measurement discipline (the `-Zthreads`/`-Zshare-generics` reversion, the samply/divan choices,
the hand-rolled `core::arch::aarch64` kernels). The frontier work that remains genuinely open is: (1)
close the skinny edition/rust-version/benchmark-harness gap against `crates/` — a mechanical,
low-risk fold, not a research question; (2) bump-and-bisect the nightly pin against the Glacier ICE
reproducer, since it is legitimately 12 weeks stale by the project's own cadence rule and stable has
moved two point releases in that window; (3) evaluate `blink-alloc` and `oxc_allocator`'s design
(not just bumpalo) as the arena strategy for any new AST/tape allocation work, since neither is yet
committed to in this codebase; (4) treat `tango-bench`'s paired methodology as a prototyping-only
tool for kernel-vs-kernel comparisons, never as a replacement for the standing cold-only bench gate.
