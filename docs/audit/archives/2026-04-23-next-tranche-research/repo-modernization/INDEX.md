# Appurtenant Repo Modernization — Master Index

**Scope**: 16 repos (4 sibling, 12 workspace-member) across the bbnf-lang
appurtenant surface. Per-repo plans live in sibling files:

- Siblings: `parse-that.md`, `pprint.md`, `gorgeous-sibling.md`,
  `csp-solver-sibling.md`.
- Workspace: `crates-core.md`, `crates-derive.md`, `crates-analysis.md`,
  `crates-ir.md`, `crates-lsp.md`, `crates-ser.md`, `crates-gorgeous.md`,
  `crates-bootstrap.md`, `crates-egraph.md`, `crates-egraph-derive.md`,
  `crates-csp-solver.md`, `crates-tape.md`, `crates-simd-scan.md`,
  `crates-json-prototype.md`.

**Inputs consumed**: `docs/tranches/meta-audit/07-appurtenant-assay.md`
(W1-B, 1297 lines); `docs/tranches/B1/TOOLCHAIN-MIGRATION.md` (W1-D, 533
lines); `docs/tranches/B1/patches/cross-repo-propagation.md` (111 lines);
`docs/tranches/meta-audit/08-abrogation-catalog.md` (W1-C, 811 lines).

---

## 1. Repo matrix

| Repo | pinned? | divan? | nextest? | config-tracked? | MSRV drop? | bench count |
|---|---|---|---|---|---|---|
| `../parse-that` | Phase A | Phase B (18) | Phase A | Phase A (new) | n/a | 18 |
| `../pprint` | Phase A | Phase B (2) | Phase A | Phase A (verify) | **YES, Phase A** | 2 |
| `../gorgeous` (sibling) | retire | retire | retire | retire | **YES, on retire** | 2 (move to workspace) |
| `../csc411/.../csp-solver` (sibling) | Phase A | Phase B (6+2) | Phase A | Phase A (new) | n/a | 6+2 morph-core |
| `crates/core` | inherit | **Phase A (19)** | inherit | n/a | n/a | 19 |
| `crates/derive` | inherit | Phase B (1 new) | inherit | n/a | n/a | 0 → 1 |
| `crates/analysis` | inherit | HOLD (deprecating) | inherit | n/a | n/a | 0 |
| `crates/ir` | inherit | Phase B (1 new) | inherit | n/a | n/a | 0 → 1 |
| `crates/lsp` | inherit | n/a | inherit | n/a | n/a | 0 |
| `crates/ser` | inherit | n/a | inherit | n/a | n/a | 0 |
| `crates/gorgeous` | inherit | Phase B (2, from sibling) | inherit | n/a | n/a | 0 → 2 |
| `crates/bootstrap` | inherit | n/a | inherit | n/a | n/a | 0 |
| `crates/egraph` | inherit | Phase B (1 new) | inherit | n/a | n/a | 0 → 1 |
| `crates/egraph-derive` | inherit | n/a | inherit | n/a | n/a | 0 |
| `crates/csp-solver` | inherit | Phase B (6 import) | inherit | n/a | n/a | 0 → 6 |
| `crates/tape` | inherit | Phase B (1) | inherit | n/a | n/a | 1 |
| `crates/simd-scan` | inherit | Phase B (1) | inherit | n/a | n/a | 1 |
| `crates/json-prototype` | inherit | Phase B (1) | inherit | n/a | n/a | 1 |

**gorgeous-mirror disposition**: RETIRE (recommended; see §6).
**csp-solver reconciliation**: workspace copy becomes authoritative for
benches (see §7).

---

## 2. Sequencing DAG

```
                     ┌─────────────────────┐
                     │  bbnf-lang B1 §1.4  │
                     │  rust-toolchain.toml│
                     │  (pin: 2026-04-11)  │
                     └──────────┬──────────┘
                                │
                ┌───────────────┼────────────────┬────────────────┐
                │               │                │                │
                ▼               ▼                ▼                ▼
        ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────────┐
        │ parse-that   │ │ pprint       │ │ csp-solver   │ │ gorgeous     │
        │ (sibling)    │ │ (sibling)    │ │ (sibling)    │ │ (RETIRE)     │
        │ Phase A pin  │ │ Phase A pin  │ │ Phase A pin  │ │ Phase A:     │
        │ config track │ │ MSRV drop    │ │ new .cargo/  │ │ disable CI   │
        └──────┬───────┘ └──────┬───────┘ └──────┬───────┘ └──────────────┘
               │                │                │
               └────────┬───────┴────────┬───────┘
                        │                │
                        ▼                ▼
            ┌──────────────────────────────────────┐
            │  bbnf-lang B1 §5-6 divan exemplar    │
            │  (crates/core compile_pipeline.rs)   │
            └────────┬─────────────────────────────┘
                     │
        ┌────────────┴──────────────┬────────────────┐
        │                           │                │
        ▼                           ▼                ▼
  ┌──────────┐              ┌─────────────┐   ┌──────────────┐
  │ crates/  │              │ crates/     │   │ crates/      │
  │ core: 19 │              │ tape,       │   │ json-        │
  │ benches  │              │ simd-scan   │   │ prototype    │
  │ (B1 §6)  │              │ (Phase B)   │   │ (Phase B)    │
  └────┬─────┘              └─────────────┘   └──────────────┘
       │
       ▼
  ┌─────────────────┐
  │ parse-that 18   │
  │ bench port      │
  │ (Phase B,       │
  │  parallelisable)│
  └────────┬────────┘
           │
           ▼
  ┌─────────────────────────────────────┐
  │  csp-solver-sibling criterion port  │
  │  + csp-solver workspace import of 6 │
  │  (Phase B, parallel workstreams)    │
  └────────┬────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────┐
  │ crates/{derive,ir,egraph} new bench  │
  │ surfaces (Phase B post-B1)           │
  └──────────────────────────────────────┘
```

**Critical gate**: the B1 pin must land before any divan migration (ICE
cluster compounds otherwise; see §B.2 of W1-B assay and the 93 ICE cluster
at `on_disk_cache.rs:663`).

**Parallel-safe streams**: once B1 pin lands, parse-that's 18-bench port +
csp-solver's migration + core's 19-bench port are three independent
workstreams; agents can own them disjointly.

---

## 3. Phase-A landing list (during bbnf-lang B1)

Required alongside B1 Step 11 cross-propagation. All items land inside B1's
~2–2.5 agent-day budget.

- **bbnf-lang (owner)**: B1 Steps 1–12.
- **`../parse-that`**: install `rust-toolchain.toml`, track
  `rust/.cargo/config.toml`, install `.config/nextest.toml`, rewrite CI.
  **~2 hours**.
- **`../pprint`**: install `rust-toolchain.toml`, **drop `rust-version =
  "1.85"` MSRV** (both pprint and pprint_derive), install
  `.config/nextest.toml`, rewrite CI, add `.cargo` alias subset. **~1.5
  hours**.
- **`../gorgeous` (sibling)**: RETIRE. Delete `src/bbnf.rs.bak`, move CI to
  `.disabled`, write tombstone README. **~40 min**.
- **`../csc411/.../csp-solver`**: install `rust-toolchain.toml`, install
  `.cargo/config.toml` with patch-table pointing back to bbnf-lang, install
  `.config/nextest.toml`. **~1 hour**.
- Workspace-member crates (12): inherit automatically; no explicit Phase-A
  action except `crates/core` (19-bench divan port is B1 Step 5–7) and
  `crates/bootstrap` (script rewrite, B1 Step 10), `crates/lsp` (release.yml
  pin update, B1 Step 9 extension).

**Phase A total (outside B1 core)**: ~5 hours sibling work.
**Phase A total (B1 core, bbnf-lang itself)**: ~2–2.5 agent-days.
**Phase A aggregate**: ~3 agent-days.

---

## 4. Phase-B landing list (post-B1, before BA)

Second-wave modernizations. Disjoint workstreams; parallelisable across
agents.

- **`../parse-that`**: port 18 benches to divan (`parse_that` 13 +
  `bbnf-regex` 4 + `regex-bootstrap` 1); rewrite `bootstrap-regex.sh`;
  rewrite `justfile`. **~8–10 hours**.
- **`../pprint`**: port 2 benches from `test::Bencher` to divan; remove
  `#![feature(test)]`; delete stale `[profile.bench]`. **~1.5 hours**.
- **`../csc411/.../csp-solver`**: port 8 criterion benches (6 csp-solver +
  2 morph-core) to divan; rewrite `scripts/bench-compare.sh`; re-enable
  Rust CI on new `rust-ci.yml`; gitignore `node_modules`. **~7–8 hours**.
- **`crates/derive`**: add `benches/expansion_cost.rs` (expansion-cost
  regression gate). **~4–6 hours**.
- **`crates/ir`**: add `benches/ir_egraph.rs` (fine-grain per-pass
  benches). **~3–4 hours**.
- **`crates/egraph`**: add `benches/egraph.rs` (saturate / add /
  congruence / extract sub-benches; synthetic Language fixtures). **~3–4
  hours**.
- **`crates/csp-solver`**: import 6 sibling benches, migrate directly to
  divan. **~4 hours**.
- **`crates/tape`**: port 1 bench (`test::Bencher` → divan); encode 6× SIMD
  gate. **~2.5 hours**.
- **`crates/simd-scan`**: dep audit (`proc-macro2`/`syn`/`quote` relocation);
  port 1 bench; document AVX-512 activation. **~2.5 hours**.
- **`crates/json-prototype`**: version-pin reconcile; port 1 bench; workspace
  `[workspace.dependencies]` for `sonic-rs`. **~1.5 hours**.
- **`crates/gorgeous`**: migrate 2 benches inbound from retired sibling.
  **~2 hours**.

**Phase B total**: ~42–48 hours (~5–6 agent-days).

---

## 5. Phase-C landing list (during/after BA)

Deferred structural work.

- **`crates/derive`**: content-keyed cache lift to `$XDG_CACHE_HOME`;
  stable symbol emission discipline (reduces AttrId churn). **~1–2 days
  each**; BA scope.
- **`crates/simd-scan`**: dedicated AVX-512 CI job. **~half-day**.
- **`crates/egraph`**: iai-callgrind instruction-count gate on
  congruence-close kernel. **~half-day**.
- **`../parse-that`**: release-profile parity gate for `bbnf-regex` vs
  `regex` crate (HIR canonicalisation). **~1.5 hours**.
- **Workspace**: bench-architecture split (`crates/bench-*` decoupling) per
  B1 §6 Deferred. **~1 week**; BA scope.
- **Watt (WASM-precompiled proc-macros)** per B1 §2.4. **2–4 weeks**; BA/BB
  scope.

**Phase C total**: ~3–4 weeks of structural work over multiple tranches.

---

## 6. gorgeous-mirror disposition — RECOMMENDED: RETIRE

**Evidence from W1-B assay**:
1. `.cargo/config.toml` `[patch.crates-io]` references 6 **STALE paths**
   (`../bbnf-lang/rust/bbnf`, `../bbnf-lang/rust/bbnf-derive`,
   `../bbnf-lang/rust/bbnf-ir`) that ceased to exist post-April 2026
   architectural consolidation.
2. CI's `sed`-rewrites target the same dead paths (`rust/bbnf`, etc.).
   **CI is broken on HEAD** unless running against a pinned older
   bbnf-lang SHA.
3. `src/bbnf.rs.bak` untracked .bak artefact suggests someone hand-patched
   the generated output — `feedback_generated_files_clean_regen` violation
   vector.
4. Biome dev-deps pinned at `=0.4.0` — contrasts with `crates/gorgeous`
   which uses `cli/v1.9.4` to dodge the 0.5.7/0.5.8 rowan skew. The
   sibling's pins are effectively broken.
5. The workspace copy at `crates/gorgeous` has been authoritative since
   April 2026; nothing in the fleet consumes the sibling.

**Recommendation**: **RETIRE**. Two layers of staleness (config paths + CI
sed) prove this mirror is effectively unused in day-to-day iteration; all
real work happens against `crates/gorgeous`. Modernising a dead mirror is
wasted work (a re-track would cost ~5 hours and the paths would drift
again on the next arch consolidation).

**Retirement actions** (~40 min): tombstone README redirect, CI `.disabled`
suffix, delete `bbnf.rs.bak`. The 2 benches (`gorgeous.rs`, `competitors.rs`)
MIGRATE INWARD to `crates/gorgeous/benches/` during Phase B.

**Counterargument (not persuasive)**: the original intent was downstream
`gorg` release independence. That intent no longer obtains — no such
release has happened, and the workspace copy handles the `bin` target
through its feature surface.

---

## 7. csp-solver sibling-vs-vendored reconciliation — RECOMMENDED: WORKSPACE AUTHORITATIVE FOR BENCHES

**Evidence from W1-B assay**:
1. Workspace copy at `crates/csp-solver` lacks ALL 6 sibling benches
   (`sudoku`, `queens`, `map_coloring`, `lattice`, `assignment`,
   `cost_finite_domain`).
2. `feedback_csp_always_optimize` flags the substrate as foundational —
   running without a bench gate is a direct contradiction.
3. Sibling uses `criterion = "0.5"` (the ONLY criterion consumer in the
   fleet); workspace copy has zero `[dev-dependencies]`.
4. Sibling CI is `.disabled`; regressions land invisibly until bbnf-lang
   pulls.
5. Sibling hosts PyO3 + WASM bindings (subcrate pattern); workspace copy
   has `py = []` no-op for symmetry.

**Recommendation**: **Workspace copy becomes authoritative for the
benches and the fleet-level bench gate**, going directly to divan (skip
criterion entirely in the workspace copy). Sibling retains its criterion
benches AS-IS until its own Phase-B migration; after its migration, both
copies run divan. Eventually the sibling becomes "standalone Python/WASM
binding host" (keeps `py` + `wasm-subcrate-pattern` role) while the
workspace copy owns the bench-gate role.

**Mechanics**: import 6 sibling bench sources into
`crates/csp-solver/benches/` during Phase B; migrate directly to divan; add
`divan` to `[dev-dependencies]`; add 6 `[[bench]]` entries. **~4 hours**.

**Why not re-vendor?** `cargo vendor`-style sync adds governance overhead
without clearing the PyO3/WASM-sibling-only concern. Duplicating benches in
both copies is cheap and aligns each copy with its role.

**Why not switch to path-patched sibling?** Sibling is inside a course-project
git root (`CSC411_HW2_ProgrammingQuestion`) — not a standalone repo.
Elevating it to authoritative requires git surgery (submodule extraction);
out of scope.

---

## 8. Divan migration count-up

Total `[[bench]]` sites across the fleet, by Phase:

| Phase | Repo | Count | Running total |
|---|---|---|---|
| Phase A | crates/core (B1 §5–6) | 19 | 19 |
| Phase A | crates/bootstrap (script rewrite, no bench) | 0 | 19 |
| Phase B | parse-that/parse_that | 13 | 32 |
| Phase B | parse-that/bbnf-regex | 4 | 36 |
| Phase B | parse-that/bootstrap | 1 | 37 |
| Phase B | pprint | 2 | 39 |
| Phase B | csp-solver (sibling) | 6 | 45 |
| Phase B | csp-solver/morph-core | 2 | 47 |
| Phase B | crates/csp-solver (import) | 6 | 53 |
| Phase B | crates/tape | 1 | 54 |
| Phase B | crates/simd-scan | 1 | 55 |
| Phase B | crates/json-prototype | 1 | 56 |
| Phase B | crates/gorgeous (from sibling) | 2 | 58 |
| Phase B | crates/derive (new) | 1 | 59 |
| Phase B | crates/ir (new) | 1 | 60 |
| Phase B | crates/egraph (new) | 1 | 61 |

**Fleet total**: **61 divan bench sites** after migration. Of these:
- **19 ported in Phase A** (all in `crates/core`, B1's exemplar + bulk port).
- **39 ported in Phase B** from existing benches (across siblings +
  workspace; framework origins: 32 bencher, 4 test::Bencher, 8 criterion).
- **3 newly authored in Phase B** (expansion_cost, ir_egraph, egraph fine-grain).

**Pre-migration total** (existing, non-divan): 19 + 32 + 4 + 8 = **63
(existing sites) minus 2 gorgeous-sibling benches that relocate, plus 3
new = 61 final**.

---

## 9. Total modernization cost

**Aggregate across Phases**:

| Phase | Scope | Agent-hours |
|---|---|---|
| Phase A | bbnf-lang B1 Steps 1–12 | ~20 (2–2.5 agent-days) |
| Phase A | sibling propagation (parse-that + pprint + gorgeous retire + csp-solver Phase-A) | ~5 |
| **Phase A total** | | **~25 hours (~3 agent-days)** |
| Phase B | sibling divan ports | ~17–19 |
| Phase B | workspace new bench surfaces (derive + ir + egraph) | ~10–14 |
| Phase B | workspace misc ports (tape + simd-scan + json-prototype + gorgeous + csp-solver) | ~14 |
| **Phase B total** | | **~42–48 hours (~5–6 agent-days)** |
| Phase C | deferred structural (BA+) | ~3–4 weeks |

**Near-term total (Phase A + B)**: ~67–73 hours (~8–9 agent-days).

---

## 10. Fleet-wide verification matrix

Phase A (post-bbnf-lang B1 land; with sibling propagation):

```bash
# 1. bbnf-lang with pinned nightly, divan exemplar, nextest-required CI.
cd /Users/mkbabb/Programming/bbnf-lang
rustc --version                                      # nightly-2026-04-11
cargo iter-check-full                                # workspace builds
cargo nextest run --workspace --profile ax-iter      # passes
cargo bench -p bbnf --bench compile_pipeline         # divan JSON

# 2. parse-that pinned; path-patch resolves.
cd ../parse-that && rustc --version
cargo iter-check
cargo nextest run --workspace

# 3. pprint pinned; MSRV dropped; CI uses nextest.
cd ../pprint && rustc --version
grep -q 'rust-version' rust/Cargo.toml && echo FAIL || echo OK
cd rust && cargo nextest run

# 4. csp-solver sibling pinned; .cargo/config.toml present.
cd /Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver
rustc --version
cargo iter-check

# 5. gorgeous sibling retired.
cd /Users/mkbabb/Programming/gorgeous
[ -f .github/workflows/ci.yml.disabled ] && echo OK
grep -q 'tombstone\|crates/gorgeous' README.md && echo OK

# 6. Cross-repo loop holds.
cd /Users/mkbabb/Programming/bbnf-lang && cargo iter-check
```

Phase B (all divan ports + new bench surfaces):

```bash
# Aggregate bench invocation — one command per repo per feedback_bench_single_run.
cd /Users/mkbabb/Programming/bbnf-lang
cargo bench-json > docs/benchmarks/post-B2-divan-bbnf-lang.json

cd ../parse-that
cargo bench-json > docs/benchmarks/post-B2-divan-parse-that.json

cd ../pprint/rust && cargo bench -- --format json > /tmp/pprint.json

cd /Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver
cargo bench-json > /tmp/csp-solver-sibling.json

# Cross-repo: every repo's bench corpus produces divan JSON; zero bencher,
# zero criterion, zero test::Bencher remaining.
cd /Users/mkbabb/Programming/bbnf-lang
rg -w 'bencher|criterion|test::Bencher' Cargo.toml crates/*/Cargo.toml
# expected: 0 hits

cd ../parse-that && rg -w 'bencher' rust/**/Cargo.toml
# expected: 0 hits

cd /Users/mkbabb/Programming/csc411/CSC411_HW2_ProgrammingQuestion/csp-solver
rg -w 'criterion' **/Cargo.toml
# expected: 0 hits
```

Phase C (post-BA, structural):

```bash
# Derive-cache lift verified via XDG_CACHE_HOME.
XDG_CACHE_HOME=/tmp/bbnf-xdg cargo iter-check
ls /tmp/bbnf-xdg/bbnf/derive/   # cache entries present

# iai-callgrind gate on congruence-close:
cargo bench -p egraph --features iai --bench egraph_callgrind

# AVX-512 CI:
RUSTFLAGS="-C target-feature=+avx512vbmi2" cargo bench -p simd-scan \
    --features avx512 --bench stage1_throughput
```

---

## Feedback-memory alignment

- **`isomorphic-api`**: parse-that's TypeScript sibling and csp-solver's
  PyO3+WASM bindings are preserved across modernization. Only
  `[dev-dependencies]` change; no public-API surface touched.
- **`wasm-subcrate-pattern`**: csp-solver sibling's `wasm/` + `wasm-morph/`
  pattern documented and preserved. bbnf-lang's `wasm/` at repo root
  (outside workspace) remains an intentional divergence.
- **`no-backward-compat`**: every modernization is a clean migration;
  bencher/criterion/test::Bencher all abrogated without fallback paths.
- **`csp-always-optimize`**: `crates/csp-solver` gains the 6-bench surface
  it lacks; `crates/egraph` gains its own stand-alone bench surface;
  foundational libraries are no longer invisible.
- **`regex-crate-isomorphic`**: `bbnf-regex` modernization is
  `[dev-dependencies]`-only; internal egraph+CSP architecture preserved.
- **`general-infra-crates`**: `crates/egraph`, `crates/csp-solver` each
  gain their own bench surface (new work in Phase B).
- **`no-workarounds`**: gorgeous-sibling retirement eliminates the CI
  `sed`-rewrite + stale path-patch workarounds that kept it nominally
  alive.
- **`bench-single-run`**: every `cargo bench-*` alias is one invocation;
  no repo loops across bench files.
- **`read-size-preflight`**: W1-B assay (1297 lines) read in deliberate
  chunks; `wc -l` executed first.
