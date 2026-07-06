# PROTO-P3 — Edition-2024 + pinned-stable whole-repo build

Prototype agent P3, SK-V21 pass-1. Worktree-isolated analysis instrument; nothing here lands on
master. Host: aarch64-apple-darwin (Apple-M), Darwin 25.4.0. Date 2026-07-05.
(Written to the worktree copy — the shared-checkout path is write-locked to this agent; orchestrator
collects from `restart/skinny/tranches/sk-v21/prototypes/PROTO-P3.md`.)

## The question (falsifiable)

Does migrating skinny to edition 2024 (MSRV 1.88) and pinning stable 1.96.x build the *entire repo*
clean, and what are the cold-build / cold-bench deltas vs the `nightly-2026-04-11` pin?

## Verdict

**MIXED — split by workspace.** The skinny workspace migrates to edition 2024 and builds **fully
clean on stable 1.96.1** (check *and* release) with exactly the charter's mechanical edits (one edition
line + unsafe-extern wraps). But the **root workspace does NOT build on stable**: a transitive
dependency, `parse_that v0.4.0`, hard-enables `smallbox`'s `nightly` feature, which gates
`#![feature(strict_provenance, set_ptr_value)]` — `E0554` on any stable channel. The §3 claim that
"every nightly feature the toolchain pin cites is dormant" is true only for the *direct* `-Z`/cranelift
surface; a **live transitive nightly requirement survives in the dependency graph** and is the real
blocker to the stack abrogation.

## Toolchain reality (correction to the charter)

- Only stable **1.85.0** was pre-installed; the charter's `1.96.x` was *not* present. It is fetchable:
  `rustup check` reports `stable … 1.85.0 -> 1.96.1 (31fca3adb 2026-06-26)`. Installed `1.96.1`
  cleanly. Edition 2024 is supported since 1.85, so the migration is testable regardless.
- Pin `nightly-2026-04-11` resolves to `rustc 1.96.0-nightly (02c7f9bec 2026-04-10)` — same 1.96
  compiler generation as stable 1.96.1, so LLVM codegen is expected identical modulo the channel.

## Migration diff (skinny) — resurrectable

`skinny/Cargo.toml` `[workspace.package]`:
```diff
-edition = "2021"
+edition = "2024"
-rust-version = "1.78"
+rust-version = "1.88"
```
Four `extern "C"` blocks require the edition-2024 `unsafe extern` wrap (**charter said 3; there are 4**):
```diff
-extern "C" {
+unsafe extern "C" {
```
at `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs:86`, `.../profile_direct.rs:55`,
`.../xctrace_probe.rs:73`, and `skinny/xtask/src/bin/capacity_probe.rs:133`. (The two `extern "C" fn
handler` sites in `bbnf-simd/tests/checkasm_*.rs` are *definitions*, not blocks — no wrap needed.) No
other edition-2024 hazard exists in skinny: grep for `#[no_mangle]`, `#[export_name]`,
`#[link_section]`, `gen`, `static mut` returns nothing. `cargo fix --edition` was **unnecessary** — the
four wraps + one edition line are the complete migration.

## Build results — skinny workspace (full `--workspace --all-targets`)

Method: fresh `CARGO_TARGET_DIR` per toolchain, **sccache disabled** (`RUSTC_WRAPPER=""`) for honest
cold walls, `/usr/bin/time -l`. Both editions = 2024 (isolates the toolchain variable).

| toolchain | result | errors | warnings | real | user | loadavg (before→after) |
|---|---|---|---|---|---|---|
| stable **1.96.1** | **clean** | 0 | 10 | **15.98 s** | 49.28 s | 5.5 → 6.7 |
| nightly-2026-04-11 | clean | 0 | 10 | 28.94 s | 59.98 s | 18.1 → 23.7 |

10 warnings, identical on both, **all benign**: 9 × `patch … was not used in the crate graph` (root
`.cargo/config.toml` `[patch.crates-io]` entries skinny never consumes) + 1 × `panic setting ignored
for bench profile` (pre-existing, migration-unrelated).

**Wall delta is confounded by load** — nightly ran at loadavg ~18-23, stable at ~5-6 (fleet fan-out
contention, not a compiler property). The load-insensitive proxy is user-CPU: stable **49.3 s** vs
nightly **60.0 s** — stable does *less* compile work. **No stable compile-time penalty at check.**

Release confirmation: `cargo +1.96.1 build --release --bin css_canon_bench` (skinny `[profile.release]`
= `lto="fat"`) built **clean, exit 0**, 1m29s wall / 150 s user (built concurrently with the nightly
release build, so contended). Edition-2024 + stable holds through fat-LTO release codegen, not just
`check`.

## Build results — root workspace (the "entire repo" leg)

`cargo +1.96.1 check --workspace` at repo root **FAILS (exit 101) at 3.17 s**:
```
error[E0554]: `#![feature]` may not be used on the stable release channel
  --> smallbox-0.8.8/src/lib.rs:168
   | #![cfg_attr(feature = "nightly", feature(strict_provenance, set_ptr_value))]
error: could not compile `smallbox` (lib) due to 1 previous error
```
Provenance (`cargo tree -i smallbox -e features`): `smallbox` feat `nightly` ← `parse_that v0.4.0`
(sibling, patched in via root `.cargo/config.toml`). Exact enable site:
**`../parse-that/rust/parse_that/Cargo.toml:97`** — `smallbox = { version = "0.8", features =
["nightly"] }`. The root `crates/` graph reaches it via `bbnf (core) → parse_that`.

**Named transitive nightly dependency: `parse_that 0.4.0 → smallbox 0.8.8 [feature "nightly"]`.**

Two honesty caveats:
1. The failure is at the `smallbox` *leaf*, compiled early — `core` and the rest of `crates/` were
   **never reached**. Whether additional nightly surfaces hide deeper in the root graph is
   **unverified** (would need smallbox's feature neutralised first; out of cap).
2. Skinny builds clean on stable *precisely because it does not depend on `parse_that`* (it carries its
   own `parse-that-regex` + the `bbnf-regex` facade). This collides with **SPEC §7.3**, which deletes
   skinny's facade and routes skinny onto the canonical parse-that engine — doing so would
   **re-import this exact nightly surface into skinny**. §3 (abrogation) and §7.3 (regex-unification)
   are coupled: pinning stable requires parse-that to drop `smallbox`'s `nightly` feature first
   (the parse-that 1.0.0 cut, charter item 5, is the candidate — verify it de-nightly's smallbox
   before §3's close gate can pass).

## Cold-bench delta (nightly vs stable)

**Not completed within cap.** Both `css_canon_bench` release binaries were targeted; the stable one
built (above), the nightly one was still compiling at the 36-min write. More decisively, the runtime
capture could not be run **quiet** — loadavg sat at 15-18 throughout (fleet fan-out). Publishing any
Mbps under that load would violate the cold/loadavg-quiet discipline, so numbers are **withheld** rather
than reported dishonestly. **This is the honest sub-question P3 could not close under cap.**

Directional answer with the evidence in hand: nightly-2026-04-11 (1.96.0-nightly) and stable 1.96.1
are the **same 1.96 LLVM generation**; release profile, target-cpu, and codegen are identical across
the channel boundary, so **runtime parity within noise is the strong expectation** and there is no
mechanism for >2% divergence. The check-phase compile-parity (stable ≤ nightly on user-CPU)
corroborates the channel switch carries no cost. Clean confirmation needs the loadavg-<2 back-to-back
run the SPEC already mandates as the TW0 quiet re-capture (§2, §11 TW0).

## Answer to the success criteria

- "Zero residual compile errors (or enumerated fix list)": **skinny = zero** (4 wraps + 1 edition
  line, check + release). **Root = one enumerated blocker**: neutralise `smallbox`'s `nightly` feature
  via parse_that (`parse_that/Cargo.toml:97`) — drop the feature or bump to a parse-that release whose
  `smallbox`/`coerce` path no longer needs nightly.
- "No nightly-only dependency surfaces; any transitive nightly dependency named": **FALSIFIED for the
  whole repo** — `parse_that 0.4.0 → smallbox 0.8.8 [feature "nightly"]` is named and live. The
  "pinned stable builds the entire repo" thesis (§3 close gate) does **not** hold as-is; it holds for
  skinny only.
- "Bench parity within 2%": **not measured under cap** (load discipline unmet); directionally expected
  (same 1.96/LLVM generation) but unproven — carried to the TW0 quiet session.

## Bottom line for the SPEC

Skinny edition-2024 + stable is **mechanically trivial and clean** (5 edits; zero errors at check and
fat-LTO release; no compile-time penalty vs nightly). The stack abrogation's real gate is **not**
skinny and **not** the dormant `-Z`/cranelift surface the SPEC foregrounds — it is the
**`parse_that → smallbox nightly`** transitive requirement in the root graph, which §7.3's
regex-unification would also drag into skinny. §3's "whole repo builds on pinned stable" close gate
must add a precondition: **parse-that de-nightly'd first.**
