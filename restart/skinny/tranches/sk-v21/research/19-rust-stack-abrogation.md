# 19 — The Rust-Stack Abrogation Census

Scope: the whole-repo Rust substrate — edition, toolchain pin, dependency
surface, and release-profile discipline — audited for a wholesale move to
current-edition, SOTA, cutting-edge footing. All version facts are static
reads of the checked-in manifests plus crates.io / docs.rs web-checks dated
2026-07-05. No compiler was invoked.

---

## 0. Headline

The single most consequential finding is not the edition split; it is that
**every nightly-only feature the toolchain pin cites as its reason to exist
is dormant.** `rust-toolchain.toml:6-8` justifies `nightly-2026-04-11` on the
parallel rustc front-end, the cranelift backend, and `-Zshare-generics`.
`.cargo/config.toml` then shows all three switched off: `-Zthreads` and
`-Zshare-generics` were removed on 2026-04-25 after a measured 28× warm
regression (`.cargo/config.toml:227-237`); the cranelift `codegen-backend`
override is commented out pending a verification that never landed
(`.cargo/config.toml:284-291`). The workspace therefore rides nightly for
zero active benefit. Current stable Rust (1.95.0 as of 2026-07-05) already
ships stable edition 2024, and every crate MSRV in the tree is below it. The
abrogation the charter asks for is thus *larger and cleaner* than a mere
edition bump: unify the whole repo on edition 2024 and drop to a **pinned
stable** toolchain, re-adopting nightly only under a measured cold-build
tradeoff.

---

## (a) Edition 2021 → 2024 migration surface

### The split and its size

The root workspace (`Cargo.toml:1-4`) and all fifteen of its member crates
are already `edition = "2024"` (e.g. `crates/core/Cargo.toml:5`,
`crates/ir/Cargo.toml:5`, `xtask/Cargo.toml:4`, `wasm/Cargo.toml:4`). The
holdout is the **skinny workspace**: `skinny/Cargo.toml:18-22` pins
`edition = "2021"` and `rust-version = "1.78"` at the `[workspace.package]`
level, inherited by all twelve member crates plus `skinny/xtask`
(`skinny/xtask/Cargo.toml:5` — `edition.workspace = true`).

Migration surface, counted from disk:

- **13 crate manifests** to re-key (the 12 members enumerated at
  `skinny/Cargo.toml:3-16` plus `skinny/xtask`). All inherit the edition
  from `[workspace.package]`, so the edition change is **one line**
  (`skinny/Cargo.toml:19`); the per-crate cost is only whatever source the
  edition lint flags.
- **166 `.rs` files, ~68,614 LOC** under `skinny/crates`.
- MSRV must rise from `1.78` to **≥ 1.85** (the hard floor for stable
  edition 2024) and realistically to **1.88** to satisfy the SOTA
  dependency bumps recommended below (simd-json 0.17 and current criterion
  both require 1.88).

### Edition-sensitive pattern scan (skinny source)

I grepped the twelve edition-2024 breaking-change classes. Results:

| Hazard class | Occurrences in `skinny/crates` | Action |
|---|---|---|
| `unsafe extern` blocks (2024 requires `unsafe extern "C" { … }`) | **3** | Wrap. `bbnf-bench/src/bin/profile_direct.rs:55`, `xctrace_probe.rs:73`, `css_canon_bench.rs:86`. The `extern "C" fn handler` sites in the checkasm tests (`bbnf-simd/tests/checkasm_parity.rs:143`, `…from_eq_set_64.rs:96`) are fn *definitions*, not blocks — unaffected. |
| `gen` reserved-keyword identifier collisions | **0** | None. |
| `static mut` references | **0** | None. |
| Unsafe-attribute wrapping (`#[unsafe(no_mangle)]`, `export_name`, `link_section`) | **0** in skinny src | None. |
| `addr_of!` / `addr_of_mut!` churn | **0** | None. |
| Match-ergonomics reservation & RPIT lifetime-capture (`impl Trait` now captures all in-scope lifetimes) | silent — not greppable | Handled by `cargo fix --edition`; the crate-set already proved out this exact migration when `crates/` moved to 2024. |

The 237 `unsafe` sites are overwhelmingly the SIMD kernels in `bbnf-simd`;
none require edition-driven changes beyond the three extern blocks above.

### Assessment

This is a **low-risk, mechanical** migration. The edition change is a
one-line workspace edit plus three `unsafe extern` wraps plus a
`cargo fix --edition` sweep for the two silent classes. The decisive
mitigant is that the sibling root workspace *already completed this exact
migration* — the in-repo playbook is proven. The only substantive coupled
decision is the MSRV bump, which is moot in practice because the toolchain
pin (1.96-nightly) already far exceeds any candidate MSRV; `rust-version`
is a declared floor, not a build constraint.

---

## (b) The toolchain pin — dormant-nightly finding, refresh, and the
##     stable question

### Current state and staleness

`rust-toolchain.toml:38-46` pins `channel = "nightly-2026-04-11"` with
components `rustfmt, clippy, rust-src, rustc-codegen-cranelift` and
`profile = "minimal"`. The header (`:14-18`) records the pin resolving to
`rustc 1.96.0-nightly (02c7f9bec 2026-04-10)`.

As of 2026-07-05, stable Rust is **1.95.0**, beta 1.96.0, nightly ~1.97.0.
The pin is **~12 weeks old** against the file's own stated cadence of "every
4-6 weeks" (`rust-toolchain.toml:20`) — roughly two-to-three cadence windows
overdue.

### The ICE that motivated the pin, and its reproducer procedure

The pin exists to make a compiler-crash class deterministic. Between
2026-04-15 and 2026-04-22 the ambient nightly produced **93
`rustc-ice-*.txt`** files at repo root, every one panicking at
`rustc_middle/src/query/on_disk_cache.rs:663: cannot decode AttrId`
(`rust-toolchain.toml:6-9`; corroborated at
`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:18-19,59-63`). The ICE was tied to
the incremental-compilation on-disk-cache decoder in the ambient
`1.96.0-nightly (9602bda1d 2026-04-05)`; the chosen pin's resolved rustc
(`02c7f9bec 2026-04-10`) is past that SHA, so the panic is expected not to
reproduce (`rust-toolchain.toml:12-18`). (No `rustc-ice-*.txt` files remain
at master root today — the pin held.)

The documented refresh procedure (`rust-toolchain.toml:20-26`,
`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:56-69,401`):

1. `rustup install nightly-YYYY-MM-DD`.
2. Bisect the ICE reproducer (TOOLCHAIN-MIGRATION §ICE reproducer). A
   nightly that still emits the AttrId ICE is rejected; bisect forward.
3. Bump `channel` in one commit, with a benchmark-parity pass
   (`make bench` against the previous pin's baselines).
4. Rotate the header comment to cite the new commit-SHA range.
5. Close gate: a `cargo iter-check-full` cold-wall under the new pin.

### The load-bearing finding: the nightly features are all dormant

The pin's stated justification (`rust-toolchain.toml:6-8`) is the parallel
front-end, cranelift, and `-Zshare-generics`. The build configuration
retracts all three:

- **`-Zthreads` (parallel front-end) and `-Zshare-generics`**: active in
  B1.W0.b, then **removed 2026-04-25** after audit-α measured a 28× warm
  `cargo iter-check` regression (0.14 s → 3.88 s) attributable entirely to
  the build-level rustflags — the small-crate warm-check working set pays
  the fixed setup cost without amortising the benefit
  (`.cargo/config.toml:227-237,270-274`).
- **cranelift codegen-backend**: the `[profile.ax-iter] codegen-backend =
  "cranelift"` override is **commented out**, gated behind a nightly-pin
  verification "that never landed" and an explicit "leave commented if
  cranelift is broken on the pinned nightly" caveat
  (`.cargo/config.toml:284-291`).
- **`rust-src` / `-Zbuild-std`**: described as an "extension" for WASM size
  experiments (`rust-toolchain.toml:29`), not wired into any active profile.

Cross-checked against the frontier: the parallel front-end remains
nightly-only and single-threaded-by-default in 2026, still requiring
`-Zthreads` to do anything; stabilization is a 2026 *goal*, not shipped
(Rust project-goals 2026, blog 2026-05-18). Cranelift and `-Zshare-generics`
likewise remain nightly. So the features still exist only on nightly — but
this workspace consumes **none of them**.

**Consequence.** The workspace is paying the full nightly tax — the 4-6 week
bisect treadmill, exposure to exactly the ICE class that cost 93 crashes,
and per-developer `rustup` drift — for zero realized benefit. Every gating
constraint (edition 2024, and the MSRVs of divan, simd-json, criterion,
cssparser) is satisfied by **current stable 1.95.0**.

### Recommendation for SK-V21

Two coherent paths; the first is the honest SOTA move given the dormancy:

- **Path A (recommended): pin stable.** Move `rust-toolchain.toml` to
  `channel = "1.95.0"` (or the then-current stable at execution). This
  eliminates the AttrId-ICE class structurally (stable's incremental cache
  is not the churning nightly surface), retires the bisect cadence, and
  keeps reproducibility via the pin. Drop the `rustc-codegen-cranelift`
  component (unused). Keep `rustfmt`, `clippy`, `rust-src`. Re-adopt nightly
  only if and when a future tranche measures a cold-build win from
  `-Zthreads`/`-Zshare-generics`/cranelift against a post-migration baseline
  — the config comment already frames this as the correct re-evaluation
  regime (`.cargo/config.toml:241-246`). Release/bench stay on LLVM
  regardless, which the config already mandates for the "beat lightningcss"
  measurement (`.cargo/config.toml:284-285`).
- **Path B: refresh nightly *and activate*.** If nightly is retained, it
  must earn its keep: bump to ~`nightly-2026-07-01` (≈1.97-nightly) per the
  five-step procedure above, and in the same wave turn on cranelift for
  `ax-iter` and re-benchmark `-Zthreads` cold, landing whichever shows a net
  win. A refreshed pin with the features still dormant is strictly worse
  than Path A.

Risk register for either path: (i) benchmark parity must be re-proven under
the new toolchain (Lock-15 numerics are toolchain-sensitive — see §(d)); (ii)
a stable move loses the option value of `-Zbuild-std` WASM experiments until
a nightly is re-pinned on demand; (iii) any transitive dep that silently
depends on a nightly feature would surface at first build (none observed in
the manifests, but only a compile confirms).

---

## (c) Full dependency census

### Skinny workspace dependencies (`skinny/Cargo.toml:39-52`)

| Crate | Pinned | Latest stable (2026-07-05) | Flag |
|---|---|---|---|
| anyhow | `1` | 1.x | current |
| blake3 | `1` | 1.x | current |
| bytemuck | `1` (derive) | 1.x | current |
| **criterion** | `0.5` (html_reports) | 0.5 (needs Rust 1.88) | **superseded in-repo by divan** — see below |
| **cssparser** | `0.34` | **0.37.0** | 3 minors behind (web-verified) |
| memchr | `2` | 2.x | current |
| libc | `0.2` | 0.2.x | current |
| mimalloc | `0.1` (no-default) | 0.1.x | current (no 0.2 exists) |
| **once_cell** | `1` | 1.x | **dead + obsoletable** — see below |
| serde | `1` (derive) | 1.x | current |
| serde_json | `1` (preserve_order, raw_value) | 1.x | current, but **in the runtime hot path** — see below |
| sha2 | `0.10` | 0.10.x | current |
| **thiserror** | `1` | **2.0.18** | **major bump available** (web-verified) |
| **toml** | `0.8` | **1.1.2** (`+spec-1.1.0`) | **major bump — toml 1.0 shipped** (web-verified) |

Skinny bench-only pins (`skinny/crates/bbnf-bench/Cargo.toml`):

| Crate | Pinned | Latest | Flag |
|---|---|---|---|
| lightningcss | `=1.0.0-alpha.71` | 1.0.0-alpha.71 | **current** (web-verified) |
| simd-json | `=0.13.11` | **0.17.0** | far behind; 0.17 needs Rust 1.88 (web-verified) |
| sonic-rs | `=0.5.8` (sort_keys) | 0.5.8 | **current** (web-verified) |

### Root workspace crate dependencies (no shared `[workspace.dependencies]`
### table — each crate pins independently; `Cargo.toml:1-4`)

Production deps of note:

| Crate | Where | Pinned | Latest | Flag |
|---|---|---|---|---|
| parse_that | core, ir, gorgeous, xtask, wasm | `0.4` | 1.0.0 cut 2026-07-03 | path-patched to `../parse-that` (`.cargo/config.toml`); nominal version stale — deferred to report 17 |
| pprint | core, wasm, xtask, gorgeous, analysis | `0.3` | path-patched to `../pprint` | sibling-repo path dep |
| rustc-hash | core, egraph, ir | `2` | 2.x | current |
| indexmap | core, analysis | `2` | 2.x | current |
| syn | core, ir, egraph-derive, bbnf-path, simd-scan, xtask | `2` (full/extra-traits) | 2.x | current |
| quote / proc-macro2 | ditto | `1` / `1` | 1.x | current |
| rmp-serde | ir | `1` | 1.x | current |
| rayon | ir | `1` | 1.x | current |
| smallvec | ir, egraph | `1.13` / `1` | 1.x | current |
| ron | ir | `0.12` | **unverified** — ron's public line historically ≤ 0.10; confirm this pin resolves |
| serde | many (derive[, rc]) | `1` | 1.x | current |
| serde_json | many | `1` | 1.x | current |
| ryu / itoa | ser | `1` / `1` | 1.x | current |
| self_cell | analysis | `1.2.2` | 1.x | current |
| ls-types | analysis, wasm | `0.0.6` | niche | verify maintained |
| tower-lsp-server | lsp | `0.23` | fork of tower-lsp | reasonably current; verify |
| tokio | lsp | `1` (full) | 1.x | current |
| wasm-bindgen / js-sys | wasm, path-ts | `0.2` / `0.3` | current | current |
| serde-wasm-bindgen | wasm, path-ts | `0.6` | current | current |
| clap | xtask | `4` (derive) | 4.x | current |
| cargo_metadata | xtask, ir(dev) | `0.18` | 0.19/0.20 line exists | mildly behind — verify |
| tempfile | xtask, core(dev), lsp(dev) | `3` | 3.x | current |
| prettyplease | xtask, core(dev) | `0.2` | 0.2.x | current |
| dhat | core (opt) | `0.3` | 0.3.x | current |

Root bench/dev competitor deps (`crates/core/Cargo.toml` dev-deps):

| Crate | Pinned | Latest | Flag |
|---|---|---|---|
| divan | `0.1` | 0.1.21 (needs 1.80) | **the SOTA bench harness** (web-verified) |
| simd-json | `0.14` | **0.17.0** | behind; note skew vs skinny's `=0.13.11` |
| sonic-rs | `0.5` | 0.5.8 | current |
| **nom** | `7` | **8.0.0** | **major bump available** (web-verified) |
| winnow | `0.7` | current line | competitor bench |
| pest / pest_grammars | `2.5` | 2.x | competitor bench |
| jiter | `0.8` | verify | competitor bench |
| serde_json_borrow | `0.9` | verify | competitor bench |
| fast-float2 | `0.2` | current | number-parse tier |
| **tree-sitter** | `0.25` | **0.26.10** | behind (web-verified) |
| tree-sitter-json | `0.24` | verify against 0.26 core | behind |
| **wasmtime** | `29` | **41–44** | **very far behind** (web-verified; dev/bench-only) |
| regex | `1` (dev only) | 1.x | current — tests only |
| iai-callgrind | `0.12` (Linux dev) | verify | CI-only |
| biome_css_* | git `cli/v1.9.4` | intentional pin | version-skew workaround documented at `crates/gorgeous/Cargo.toml` |

### Architectural findings inside the census

1. **regex-syntax is already fully excised (a win to preserve).** No crate
   in either workspace depends on `regex-syntax`. The bespoke HIR lives in
   `skinny/crates/bbnf-regex`, `skinny/crates/parse-that-regex`, and the
   root `bbnf-regex`; the only `regex` dependency anywhere is a *dev-only*
   `regex = "1"` in `crates/core/Cargo.toml` for tests. The charter's
   "regex-syntax vs our HIR" question is settled — nothing to abrogate here.

2. **criterion → divan is half-done; finish it.** The root workspace already
   standardized on divan (`crates/core/Cargo.toml`, `crates/lsp`,
   `crates/simd-scan` dev-deps), and the B1 decision ledger froze divan as
   the primary harness for cold-per-parse discipline
   (`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:27-36`, citing `no-warm-benches`
   — criterion is warm-bench-first by architecture). Yet
   `skinny/crates/bbnf-bench/Cargo.toml` still carries `criterion = "0.5"` as
   a dev-dep. Its two benches (`json_parity`, `simd_scan`) are already
   `harness = false` with hand-rolled mains, so criterion is likely a vestige.
   Abrogation: delete criterion from skinny, unify on divan. Tango was
   considered and is a niche third option — not recommended over divan.

3. **serde_json is in the skinny *runtime*, and on the number path.** For a
   generator whose thesis is to beat serde_json, the runtime delegates to it:
   `skinny/crates/runtime/src/lib.rs:265` parses JSON numbers via
   `serde_json::from_str::<serde_json::Number>(literal)`, and
   `grammars/json/view.rs:232` does the same; string serialization routes
   through `serde_json::to_string` at `grammars/json/value.rs:94` and
   `view.rs:130`. This is both a dependency smell and a latent perf ceiling —
   number parsing should run through a bespoke `fast-float2`/lexical path
   (the root bench already pulls `fast-float2 = "0.2"`), and escaping through
   the `ryu`/`itoa` primitives already vendored in `bbnf-ser`. Flag for the
   abrogation wave: sever `serde_json` from the runtime hot path; keep it
   only as a bench oracle.

4. **once_cell is dead in skinny and obsoletable everywhere.** It is declared
   at `skinny/Cargo.toml:47` but no skinny member lists it in `[dependencies]`
   and no `skinny/crates` source references it — a dead workspace-deps entry.
   Even where a `OnceCell`/`Lazy` is wanted, edition 2024 / Rust ≥ 1.85 makes
   `std::sync::LazyLock` and `OnceLock` available; once_cell should be dropped
   in favour of std.

5. **serde ecosystem is fine.** serde 1 + serde_json 1 + rmp-serde 1 +
   serde-wasm-bindgen 0.6 are all current-major; the IR-serialization use
   (`rmp-serde`, `ron`) is cold-path metadata, not runtime hot code. No
   abrogation needed beyond the runtime-number-path severance above.

6. **Version skew to reconcile.** simd-json is pinned three different ways
   (`=0.13.11` skinny bench, `0.14` root core dev) while latest is `0.17.0`;
   sonic-rs is `=0.5.8` (skinny) vs `0.5` (root). A single-source competitor
   pin table would remove the skew.

---

## (d) Release profile vs Lock 15

**Lock 15** (`restart/locks/LOCKS.md:436`) requires every bbnf-generated
runtime crate to declare `[profile.release]` with `lto = "fat"`,
`codegen-units = 1`, `panic = "abort"` (or `"unwind"` only if the grammar
carries `@error(recover = …)`), and `debug = true` (for samply symbol
resolution). The lock cites hard evidence: a `lto = "thin"` build regressed
twitter throughput **11780 → 5521 Mbps** — a near-halving. The profile-scope
clause (`restart/locks/LOCKS.md:605`) explicitly holds that the *root*
release profile is unproven and requires separate proof before Lock-15
closure.

Actual profiles:

| Profile | lto | codegen-units | panic | debug | Verdict |
|---|---|---|---|---|---|
| `skinny [profile.release]` (`skinny/Cargo.toml:78-85`) | **fat** | 1 | **abort** | true | **Lock-15 compliant** |
| `skinny [profile.bench]` (`:87-95`) | fat | 1 | abort | true | compliant |
| **`root [profile.release]`** (`Cargo.toml:80-85`) | **thin** | 1 | *(unset → unwind)* | true | **Lock-15 VIOLATION** |
| `root [profile.bench]` (`Cargo.toml:87-97`) | fat | 1 | *(unset)* | true | fat but no `panic=abort` |
| `root [profile.profiling-prep]` (`:117-121`) | inherits thin | 1 | *(unset)* | true | thin (profiling only) |
| `wasm [profile.release]` (`wasm/Cargo.toml`) | true(=fat) | — | *(unset; wasm aborts anyway)* | — | acceptable for wasm |

### Assessment and recommendation

The skinny release/bench profiles are exactly what a better-than-SOTA parser
should declare. The **root workspace release profile is the flagged gap**:
`lto = "thin"` with no `panic = "abort"` (`Cargo.toml:81-82`). Any generated
runtime or throughput-sensitive consumer built from the root workspace
inherits thin LTO — the precise configuration Lock 15 documents as halving
throughput. For SK-V21's abrogation the root release profile should become:

```
[profile.release]
lto            = "fat"
codegen-units  = 1
panic          = "abort"   # "unwind" only for @error(recover) grammars
debug          = true
strip          = false
split-debuginfo = "packed"
```

with `panic = "abort"` likewise added to `[profile.bench]`.

Two further items the profiles omit that a SOTA-claiming parser needs:

- **`target-cpu` is set nowhere.** Neither `.cargo/config.toml` nor any
  profile sets `-C target-cpu=native` (or an explicit `apple-m1`/`apple-m4`).
  Lock 15's scope clause names `target-cpu=native` host-bound rows as
  *unproven* (`restart/locks/LOCKS.md:605`). Since the benchmark discipline
  is aarch64/Apple-M-only (charter §Standing constraints), the bench profile
  should carry a host-CPU target so NEON codegen is not gated to the generic
  baseline. This is the largest untapped, zero-risk throughput lever visible
  in the profile surface.
- **`panic = "abort"` is absent from the bench profile**, so bench binaries
  carry unwinding tables the runtime thesis does not want measured.

Net: skinny profiles are correct; the abrogation work is to lift the *root*
release/bench profiles to skinny's Lock-15 shape and add an Apple-M
`target-cpu` for the aarch64 bench rows.

---

## Abrogation punch-list (for the synthesis pass)

1. Skinny `edition = "2021" → "2024"`; `rust-version "1.78" → "1.88"`; wrap
   3 `unsafe extern "C"` blocks; `cargo fix --edition` sweep. Low risk —
   the `crates/` migration is the in-repo precedent.
2. Toolchain: prefer **pinned stable 1.95.0** (all nightly features dormant);
   drop `rustc-codegen-cranelift` component; keep the pin for reproducibility.
   Fallback is a refreshed nightly *with* cranelift/`-Zthreads` re-activated
   and re-benchmarked.
3. Dependency bumps: thiserror 1 → 2; toml 0.8 → 1.x; cssparser 0.34 → 0.37;
   simd-json → 0.17 (single-source the pin); nom 7 → 8; tree-sitter 0.25 →
   0.26; wasmtime 29 → current. Delete criterion from skinny (unify on divan);
   delete dead/obsolete once_cell (use std `LazyLock`); confirm `ron = "0.12"`
   resolves. sonic-rs and lightningcss are already current.
4. Sever `serde_json` from the skinny runtime number/escape path
   (`runtime/src/lib.rs:265`, `grammars/json/{value,view}.rs`); route through
   fast-float / `bbnf-ser` primitives.
5. Lift root `[profile.release]` / `[profile.bench]` to Lock-15 shape
   (fat LTO + `panic=abort`); add an Apple-M `target-cpu` to the bench profile.
6. Preserve the regex-syntax excision (bespoke HIR) — no action, already SOTA.
