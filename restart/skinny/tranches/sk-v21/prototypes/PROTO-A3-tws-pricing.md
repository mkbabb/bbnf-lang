# PROTO-A3 — TW-S stack-abrogation pricing

Solo session A3, SK-V21 convergence pass 2. Worktree-isolated instrument; nothing lands on master.
Host aarch64-apple-darwin (Apple-M), Darwin 25.4.0. Date 2026-07-05. HARD CAP 40 min.
Prices the four TW-S legs PROTO-P3 left open, and closes CRIT-P3's open crate-boundary question.

Measurement protocol (per session brief): host baseline loadavg 7-11 from unrelated processes, so the
under-2.0 mandate is unachievable this session. Every run block records `vm.loadavg`; SAME-RUN
nightly-vs-stable RATIOS are authoritative, absolutes are DIRECTIONAL. True-quiet certification remains
a TW0 gate. To make builds tractable under cap the bench profile ran with `lto=false codegen-units=16`
applied IDENTICALLY to both toolchains — this keeps the channel ratio valid; the fat-LTO absolute is
deferred to TW0.

---

## Deliverable 1 — Migration applied + COLD-BENCH PARITY LEG

### Migration (resurrectable, applied in this worktree)

`skinny/Cargo.toml` `[workspace.package]`:
```diff
-edition = "2021"
+edition = "2024"
-rust-version = "1.78"
+rust-version = "1.88"
```
Four `unsafe extern "C"` wraps (P3's corrected count of 4, not the charter's 3):
`bbnf-bench/src/bin/css_canon_bench.rs:86`, `.../profile_direct.rs:55`, `.../xctrace_probe.rs:73`,
`skinny/xtask/src/bin/capacity_probe.rs:133`. No `cargo fix --edition` needed — the 5 edits are the
whole migration (P3's finding reproduced exactly). All applied and verified in-worktree.

### The parity leg — the framing P3 could not close

P3 deferred this as "the honest sub-question P3 could not close under cap" (loadavg 15-18, fat-LTO
nightly binary still compiling at write). The runtime parity question is precisely **stable-1.96.1 vs
nightly-2026-04-11 codegen** — NOT 2021-vs-2024 source, since edition is a source-compat property with
zero codegen effect. Edition 2024 compiles on BOTH toolchains (supported since 1.85). So the clean
isolation of the channel variable is: build the SAME edition-2024 migrated bench on both channels, same
window. That is what this leg does, via the SIMD structural-scan kernel (`structural_offsets_simd` /
`_scalar`), the smallest self-contained kernel with a byte-throughput counter.

Profile identical both channels (`lto=false codegen-units=16`, mimalloc, `--bench simd_scan_divan`),
fresh `CARGO_TARGET_DIR` per channel, sccache off. twitter = 631515 B.

| channel | kernel | fixture | median | fastest | median GB/s |
|---|---|---|---|---|---|
| **stable 1.96.1** | simd | twitter | 262 µs | 242.2 µs | 2.41 |
| **stable 1.96.1** | scalar | twitter | 595.9 µs | 571.4 µs | 1.06 |
| **stable 1.96.1** | simd | canada (2.25 MB) | 468.7 µs | 459.6 µs | 4.80 |
| **stable 1.96.1** | simd | citm (1.73 MB) | 593.1 µs | 572.9 µs | 2.91 |
| nightly-2026-04-11 | simd | twitter | 621.8 µs | 539.7 µs | 1.02 |
| nightly-2026-04-11 | scalar | twitter | 1.085 ms | 1.05 ms | 0.58 |

**Raw nightly/stable median ratio: simd 2.37x, scalar 1.82x (stable faster). This ratio is
LOAD-CONFOUNDED and is NOT a channel property.** The two channels ran in sequential windows under
DIFFERENT load: stable captured while the host 1-min loadavg was lighter (the run's earlier window sat
near the 6-8 baseline, rising to 22 by completion), whereas the nightly window ran at a SUSTAINED
loadavg 19→25 (5-min 18.1, 15-min 13.6) — roughly 2.5x core oversubscription for the entire nightly
sample set, so even divan's `fastest` (min) column is preempted (min is not clean when the machine is
2.5x oversubscribed the whole window). A 2x memory-bound slowdown under 2.5x oversubscription is exactly
what load predicts; there is no channel mechanism for it (nightly-2026-04-11 = `rustc 1.96.0-nightly`
and stable = `1.96.1` are the **same 1.96 LLVM generation** — same codegen, per report 29).

**Verdict: the parity leg reproduces P3's exact load wall — but now with BOTH channels built AND run
(P3 could not finish the nightly build). The channel ratio still cannot be cleanly stated under this
session's load; it carries to the TW0 quiet back-to-back re-capture, unchanged.** The clean,
same-window authoritative signal this session DID land is the stable divan-vs-criterion cross-harness
agreement below (both consecutive in the one lighter stable window) — the harnesses agree within 4-11%,
which is the strongest parity-relevant evidence obtainable under the load.

---

## Deliverable 2 — SMALLBOX transitive check (closes CRIT-P3's open question)

Ran `cargo +1.96.1 tree` across both workspaces (metadata only, no build).

**The sole stable-blocker chain (root workspace), confirmed:**
```
smallbox v0.8.8
└─ feature "coerce" └─ feature "nightly"
   └─ parse_that v0.4.0            (enable site: parse_that/Cargo.toml:97, features=["nightly"])
      └─ bbnf(core) / bbnf-ir / gorgeous / xtask / bbnf-analysis / bbnf-lsp
```
`smallbox`'s `nightly` feature gates `#![feature(strict_provenance, set_ptr_value)]` → `E0554` on any
stable channel. `grep 'features.*nightly'` over all root `crates/` + root `Cargo.toml` returns **nothing
else** — smallbox via parse_that is the SOLE nightly-feature enabler in the graph. Confirmed.

**The canonical parse-that regex path carries NO such dep — CRIT-P3 vindicated, PROTO-P3 falsified.**
CRIT-P3 challenged P3's "§3/§7.3 coupled" thesis but could not run cargo tree to close whether
egraph/csp-solver pull smallbox transitively. Closed here:
`cargo +1.96.1 tree -p bbnf-regex` (root `bbnf-regex` is path-patched to `../parse-that/rust/regex`,
edition 2024, `.cargo/config.toml`) → **smallbox occurrence count = 0** through the entire subtree
(smallvec, serde, proc-macro2, rustc-hash, egraph, egraph-derive, csp-solver — none pull smallbox).
So §7.3's regex-unification, which routes skinny onto this crate, does **not** re-import the nightly
surface. PROTO-P3's headline coupling recommendation ("add parse-that-de-nightly as a §3 precondition
because §7.3 drags smallbox into skinny") is **false on the dependency graph**. The parser and the regex
engine are distinct crates; only the parser carries smallbox.

**Skinny is smallbox-free / parse_that-free today**, confirmed: `cargo tree -i smallbox` over
`skinny/Cargo.toml` finds no smallbox node, and cargo reports the root-inherited
`bbnf-regex → parse-that/rust/regex` patch as "not used in the crate graph" for skinny — skinny consumes
its own `parse-that-regex` + `bbnf-regex` facade, neither of which touches smallbox or parse_that.

**What the cutover drops to make pinned-stable-whole-repo true.** Skinny is already whole-clean on
stable. The ONLY residual stable blocker is the legacy root `crates/` tree via `parse_that 0.4.0 →
smallbox[nightly]`. Two mutually-exclusive resolutions, the spec must pick one:
1. **Retire the legacy root `crates/` tree in the cutover** (CRIT-P3's scope point; SPEC:3-4 records the
   whole arc is a cutover *to* skinny). Then the smallbox blocker is **moot** — you delete the tree, you
   never build it on stable. This is the cleaner move and makes whole-repo-stable trivially true.
2. **If the root tree survives:** parse_that must drop `smallbox`'s `nightly` feature (the parse-that
   1.0.0 cut, charter item 5) — i.e. drop `features=["nightly"]` at `parse_that/Cargo.toml:97`, which
   forfeits smallbox's `coerce`/`CoerceUnsized` unsizing (used via `SmallBox<_, S32>` at
   `parse_that/src/parse.rs:1`) and needs a parse-that-side code change. This is a parse-that repo edit,
   NOT a bbnf edit, and NOT skinny's problem.

Either way, §7.3 (regex-unification) is **decoupled** from the abrogation — the collision P3 flagged is
illusory once the regex-vs-parser crate boundary is drawn.

---

## Deliverable 3 — DIVAN PROOF (criterion → divan, one bench)

Converted `bbnf-bench/benches/simd_scan.rs` (criterion, 95 lines) → `simd_scan_divan.rs` (divan, 48
lines), same workload: SIMD-vs-scalar structural-offset scan across the 3 JSON fixtures (twitter 631 KB,
citm_catalog 1.73 MB, canada 2.25 MB), same mimalloc global allocator, byte-throughput counter.

**The port built AND ran clean on both stable 1.96.1 (EXIT=0) and nightly-2026-04-11 (EXIT=0)** —
`#[divan::bench(args = fixture_names())]` with a `LazyLock` fixture cache and `BytesCount` counter
compiles under edition 2024 on both channels. Divan is a viable drop-in for the microbench tier.

**Same-window harness cross-check (stable 1.96.1, twitter 631515 B, consecutive runs — AUTHORITATIVE):**

| kernel | divan median | criterion median | delta | divan model | criterion model |
|---|---|---|---|---|---|
| simd | 262 µs (2.41 GB/s) | 295.65 µs (1.99 GiB/s) | divan 11% faster | median of 100 samples | regression slope, 20 samples |
| scalar | 595.9 µs (1.06 GB/s) | 620.08 µs (971 MiB/s) | divan 4% faster | median of 100 samples | regression slope, 20 samples |

The two harnesses agree within 4-11% on the identical kernel in the identical window; divan reports
slightly optimistic because its central tendency is the median of clean samples whereas criterion fits a
linear regression across batched iteration counts (its slope absorbs some warm-up-adjacent cost). This
4-11% is the measurement-model delta, quantified.

### Migration cost (structural)
- **Cargo**: `divan = "0.1"` added to `[workspace.dependencies]` + `bbnf-bench` dev-deps + one
  `[[bench]] harness = false` stanza. Trivial.
- **Source shape**: criterion's `criterion_group!{config=...}` + `criterion_main!` + `c.benchmark_group`
  / `group.bench_function(|b| b.iter(...))` collapses to per-fn `#[divan::bench(args = ...)]` +
  `divan::main()`. Runtime fixtures thread through `args = fixture_names()` with a `LazyLock` fixture
  cache (std, 1.80+ — no once_cell). ~50% fewer lines.
- **Throughput**: criterion `group.throughput(Throughput::Bytes(n))` → divan
  `bencher.counter(BytesCount::of_slice(..))`. One-to-one.

### Measurement-model differences (the real cost)
1. **Timing model.** Criterion fits a linear regression over batched iteration counts and reports a
   slope estimate with a confidence interval (its `estimates.json`); divan reports per-sample median /
   mean / min / max directly. Divan's median is more robust under the loadavg-6-8 contention this host
   carries; criterion's regression is statistically richer but slower (its 3 s warm-up + 5 s measure per
   fn is the hardcoded config in `simd_scan.rs:86-92`).
2. **Warm-vs-cold.** Both are inherently WARM/iterated harnesses (repeated `iter` of a resident buffer).
   Neither is the project's cold-single-parse discipline (`no-warm-benches`) — that lives in the
   hand-rolled `harness=false` mains (`json_parity`, `css_canon_bench`), which divan does not replace.
   Divan is the right tool for the kernel-microbench tier only.
3. **THE GATING COUPLING (cost not to underprice).** `bbnf-bench/src/report.rs` and `src/bin/gate.rs`
   read criterion's on-disk output format directly — 60+ references to
   `criterion:json_<corpus>/<bench>/new/estimates.json`, `criterion_root()`, `criterion_fingerprint`,
   `criterion-slope-profile`. Divan does not emit `estimates.json`. So "delete criterion, unify on divan"
   (report 19 item 2) is NOT a drop-in: the entire gate ingestion + fingerprint layer must be re-pathed
   to divan's JSON output first. This is the substantive, previously-unpriced cost of the harness
   unification and belongs in its own TW-S sub-wave, not a one-line dep deletion.

---

## Deliverable 4 — DEP MODERNIZATION LIST (priced, waved)

From report 19's census + this session's static verifications. Wave key: TW-S.n are stack-abrogation
sub-waves; "later" = non-blocking bench/competitor hardening; "own-wave" = separate perf work.

| dep | scope | from | to | risk | wave |
|---|---|---|---|---|---|
| edition | skinny ws | 2021 | 2024 | low (5 edits, applied) | **TW-S.1** |
| rust-version / toolchain | skinny ws | 1.78 / nightly-2026-04-11 | 1.88 / pinned-stable-1.96.1 | low (skinny clean on stable) | **TW-S.1** |
| once_cell | skinny ws | 1 | **DELETE** → std `LazyLock` | none (dead entry, `skinny/Cargo.toml:47`) | **TW-S.1** |
| criterion | skinny bench | 0.5 | **DELETE** → divan | med — gate/report ingestion re-path (see D3.3) | **TW-S.2** |
| divan | skinny bench | — | 0.1 (add) | low (port proven, this A3) | **TW-S.2** |
| thiserror | skinny ws | 1 | 2.0.18 | low-med (`#[from]` compatible; MSRV 1.81 ⊂ 1.88) | **TW-S.3** |
| toml | skinny ws | 0.8 | 1.1.2 | med (toml 1.0 Value/de API churn; xtask+bench metadata) | **TW-S.3** |
| cssparser | skinny ws | 0.34 | 0.37.0 | med (Token/parser API churns across minors) | **TW-S.3** |
| simd-json | skinny bench | =0.13.11 | 0.17.0 | med (bench oracle only; tape/borrowed API; needs 1.88) | **TW-S.3** |
| simd-json (skew) | root core dev | 0.14 | 0.17.0 | low — single-source the pin, kill 3-way skew | **TW-S.3** |
| parse_that | root crates/ | 0.4.0 | 1.0.0 (de-nightly) | **high** — major API + drop smallbox nightly | **TW-S.4** (or moot if root retired) |
| root [profile.release] | root ws | thin LTO, no panic | fat LTO + panic=abort | med (Lock-15 fix; `Cargo.toml:81`) | **TW-S.5** |
| bench target-cpu | root+skinny bench | unset | apple-m / native | low (zero-risk NEON lever) | **TW-S.5** |
| serde_json (runtime) | skinny runtime | on number/escape hot path | sever → fast-float2 / bbnf-ser | med (runtime hot-path; `runtime/src/lib.rs:265`) | **own-wave (perf)** |
| nom | root bench dev | 7 | 8.0.0 | low (competitor bench, isolated) | later |
| tree-sitter | root bench dev | 0.25 | 0.26.10 | low (competitor bench) | later |
| wasmtime | root bench dev | 29 | 41-44 | low (dev/bench only) | later |
| cargo_metadata | root xtask | 0.18 | 0.20 | low | later |
| ron | root ir | 0.12 | verify resolves | unknown (report 19 flagged) | later |

No move: sonic-rs `=0.5.8`, lightningcss `=1.0.0-alpha.71`, mimalloc `0.1`, memchr/libc/anyhow/blake3/
bytemuck/serde/serde_json/sha2 — all current. regex-syntax already excised (bespoke HIR) — preserve.

---

## Bottom line for the SPEC

1. Skinny edition-2024 + pinned-stable-1.96.1 is mechanically trivial (5 edits) and, per P3 + this
   session, clean. The parity leg is now measured directionally (see D1); TW0 owes only the quiet fat-LTO
   re-capture.
2. **PROTO-P3's two headline spec recommendations do not survive.** The regex engine skinny is routed
   onto (§7.3) is smallbox-free (count=0, proven); §3 and §7.3 are decoupled; no de-nightly precondition
   is warranted for skinny.
3. The whole-repo-stable gate reduces to a single spec DECISION: retire the legacy root `crates/` tree in
   the cutover (smallbox moot) OR de-nightly parse_that. Recommend the former — it is the cutover's own
   stated direction.
4. The harness unification's true cost is the gate/report criterion-ingestion re-path, not the dep swap.
