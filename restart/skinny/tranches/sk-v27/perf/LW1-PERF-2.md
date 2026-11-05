# LW1.PERF-2: `cargo +stable build` probe (RED recorded; the cure is LW2)

**Toolchain** `stable-aarch64-apple-darwin`, rustc 1.97.0 (2d8144b78 2026-07-07). Every run used `--release --keep-going` through `guarded`.
**Logs** are in `~/.dev-logs/bbnf-lockstep/rust-perf-lw1/`.

## Heads probed

| repo | tree | rev |
|---|---|---|
| bbnf-lang | `~/Programming/bbnf-lang-wt/sk-v27` (clean) | `b4e635d3e` (= `origin/master` `c9b93adb4` + plan docs) |
| parse-that | `~/Programming/parse-that-wt/sk-v27` (clean), reached by bbnf-lang through the `../parse-that` `[patch]` | `b4f63f4` (on `47e928acc`) |
| pprint | `~/Programming/pprint`, reached through the `../pprint` `[patch]` | `2b4d2d4` |

## Results

| probe | command | rc | first blocker |
|---|---|---|---|
| bbnf-lang | `cargo +stable build --release --keep-going -p bbnf -p parse_that -p pprint -p simd-scan` | **101** | `smallbox 0.8.8`: E0554 `#![feature]` on stable. It is pulled in by `parse_that`'s `smallbox = { version = "0.8", features = ["nightly"] }` (`rust/parse_that/Cargo.toml:97`). |
| parse-that | `cargo +stable build --release --keep-going --workspace` (in `rust/`) | **101** | It fails before any crate compiles. `parse-that/.cargo/config.toml` sets `build.rustflags = ["-Zthreads=8", "-Zshare-generics=y"]` (lines 10 and 16): "the option `Z` is only accepted on the nightly compiler". |

**Compiled on stable (bbnf-lang probe):** `pprint` 0.3.6, `pprint_derive`, `simd-scan`, `bbnf-regex` (parse-that/rust/regex) and `bbnf-ser`.
**Blocked:** `parse_that` and everything downstream of it (`bbnf`, `bbnf-ir`, `gorgeous`, `xtask`).

## The nightly surface the cure must remove (static census; layers behind the first blocker)

1. `parse-that/.cargo/config.toml`: `-Zthreads=8 -Zshare-generics=y`, in `build.rustflags` and per-target. bbnf-lang retired the same flags already (its `.cargo/config.toml` §Build flags retrospective).
2. `parse_that` → `smallbox` `features = ["nightly"]`.
3. `parse_that/src/lib.rs`:
   - `#![feature(portable_simd)]`, with 11 `std::simd` / `portable_simd` sites under `src/`;
   - `#![feature(fmt_helpers_for_derive)]`;
   - `#![feature(stmt_expr_attributes)]`.
4. bbnf-lang `rust-toolchain.toml` pins `nightly-2026-04-11`. Its `rustc-codegen-cranelift` component serves only the dev profile.

Item 3's `portable_simd` is a PERF matter. Under X-3 the `std::simd` sites move to `parse-that-simd` as `core::arch` kernels behind the `OnceLock` table, each with a scalar oracle. That is LW2.PERF-1, the same shape this branch gives `simd-scan` (see `LW1-PERF-1.md`).

## Already stable on this branch

The cured `simd-scan` (dispatch, `stripe`, `#[target_feature]` kernels) builds and links its tests on stable for both `aarch64-apple-darwin` and `x86_64-apple-darwin`: `cargo +stable test --no-run -p simd-scan` returned rc 0 on each (`perf2-simd-scan-cure-stable-*.log`).

## Owner of the cure

LW2.TOPO-3 owns the required `cargo +stable` CI job, and LW2.PERF-1 owns `portable_simd` → `parse-that-simd`. From LW2 on, gate LG-a demands `cargo +stable` rc 0.
