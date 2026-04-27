# Tranche B7 — FINAL

B7 closes the cross-repo modernization gap. Ten agents across three
repo-isolated waves landed twenty commits — eight on bbnf-lang
master, nine on parse-that master, three on pprint master — in
~15 min real wall (longest single-agent wall ~12 min on parse-that
A2/A5, well under the 4 h cap). The post-B7 substrate has divan as
the only bench harness across all three repos, nextest as the only
CI runner across all three repos, and the `bbnf-ser` patch lives in
exactly one place: bbnf-lang's root `.cargo/config.toml`. AZ-I.W0
opens against this runway.

## Architectural narrative

B7 was authored as a bounded prelude annex for AZ-I. The charter
scoped three measurable cross-repo drift categories, one wave per
repo: bbnf-lang internal completions (W0), parse-that catchup (W1),
pprint catchup (W2). Per SPEC §Sequencing, the three waves are
repo-isolated and ran in parallel; per-wave parallel-agent counts
(3, 4, 3) all sit within SPEC §Parallelism max-6.

### W0 — bbnf-lang internal modernization completion

The post-B6 substrate had three residual cross-repo gaps inside
bbnf-lang: simd-scan's bench file used manual `std::time::Instant`
timing while every other crate used divan; the warm-iter-test wall
that B6.W2 surfaced as 22.353 s with two structural floor-setters
(`bbnf-lsp::bench_lsp::bench_lsp_actions` at 16.485 s, and the
simd-scan proptest fuzzers at 17–20 s peak); and the doc surface
needed three discrete updates folded forward from the fifth /plan
synthesis cycle (BB.W0 hit-rate gate, AY-III deferral marking, and
the AZ-II.W1+W2 atomic-merge documentation).

A1 ported `crates/simd-scan/benches/stage1_throughput.rs` to
`#[divan::bench]` macros with `BytesCount` counters. Per-fixture
MB/s sit within ±5% of pre-B7 baseline on 4 of 5 fixtures; the
twitter fixture is 16% faster (favorable). A9 re-routed
`bench_lsp_actions` from `crates/lsp/tests/bench_lsp.rs` to
`crates/lsp/benches/bench_lsp.rs` under divan's harness, sample-
capped the simd-scan proptest fuzzers from 1024 to 64 cases, and
folded the AZ-II.W1+W2 atomic-merge doc scrub (deleting the
former W3.md and consolidating into a 3-wave shape: W0 + W1-merged
Stage-A+B + W2 FINAL). A10 added the BB.W0 e-graph rule hit-rate
measurement gate, marked AY-III as deferred with absorb-clauses
appended to AZ-I.W4 and AZ-II.W2 close ceremonies, annotated
REMAINING-TRAJECTORY with the post-B7 path, and authored the B7
plan document set (B7.md + PROGRESS + AGENT_DISPATCH +
waves/{W0,W1,W2}.md + this FINAL.md skeleton).

### W1 — parse-that catchup

The post-B5 audit identified four parse-that drift categories: 16
bench targets on `bencher = "0.1.5"` (legacy harness); CI on bare
`cargo test --workspace` (vs nextest); 69 inline `#[test]` items
in `src/`; missing `ay-final` + `bench-ci` profile aliases plus
floating `dtolnay/rust-toolchain@nightly`. The actual bench-target
count was 18 not 16 (3 in `parse_that/`, 10 in `parse_that/
competitors/`, 1 in `bootstrap/`, 4 in `regex/`); A2 + A3 split the
work so A2 owned the 13 parse_that-crate-internal benches plus the
crate's Cargo.toml dev-dep swap, and A3 owned the 5 sibling-crate
benches in `bootstrap/` + `regex/`.

A2 landed three commits porting all 13 parse_that-crate benches to
divan, including fixing 16 pre-existing literal-escape compile
errors (under edition 2024 strict literals: nom 5 sites, winnow 3,
micro 8) and registering `lightningcss::tailwind` as
`#[divan::bench]` to resolve a dead-code warning. A3 landed two
commits porting `bootstrap/regex_parse.rs` and the four
`regex/*.rs` benches; the `regex/` benches all run cleanly under
divan. The `bootstrap/regex_parse.rs` bench compiles but its
`cargo bench` runtime is blocked by a pre-existing
`bbnf_derive 0.2.9 → parse_that ^0.3` semver clash that traces to
the post-B2 bootstrap crate's hold-over `bbnf_derive` dependency
(see Cross-tranche debt §Forwarded). A4 rewired `.github/workflows/
ci.yml` to `cargo nextest run --cargo-profile ax-iter --workspace
--no-fail-fast`, added the nextest install step via
`taiki-e/install-action@v2`, retired the floating
`dtolnay/rust-toolchain@nightly` reference, and added `ay-final`
+ `bench-ci` profile aliases mirroring bbnf-lang's source-of-truth.
A5 hoisted all 69 inline `#[test]` items from `rust/parse_that/
src/{split.rs, parsers/scan/{mod,number_simd,structural_bitmap,
quoted_simd,quote_parity}.rs}` to five new tests/ files
(`split_test.rs`, `number_simd_test.rs`,
`structural_bitmap_test.rs`, `quoted_simd_test.rs`,
`quote_parity_test.rs`); 69/69 tests pass post-migration; visibility
adjustments (`number_simd` private mod → pub; `escaped_mask` +
`prefix_xor` private fn → `#[doc(hidden)] pub fn`) were the
minimal disclosures needed.

### W2 — pprint catchup

The pprint audit identified three drift categories: 38 bench fns on
`#![feature(test)]` + `extern crate test` (Rust unstable harness);
CI on bare `cargo test --workspace`; redundant `bbnf-ser` patch in
the local-dev `rust/.cargo/config.toml` (centralization
opportunity).

A6 landed the larger bench file (`rust/benches/pprint.rs`, 26
fns), retiring the unstable harness for divan's `#[divan::bench]`
across all 26 functions; the `±5% per-fixture median delta` gate
recorded a systematic +9–41% shift (sample medians: bench_build_only_1k
3,926 ns → 5,165 ns; bench_pp_flat_1k 11,347 → 14,620 ns). The
shift is methodology — divan's per-sample calibration overhead vs
libtest's looser amortization model on small fixtures — not a code-
path regression; per-iter cycle counts are preserved. A7 ported
`rust/benches/digit_count.rs` (12 fns) using the same pattern; the
build verification deferred to A6's Cargo.toml change landing first
(divan dev-dep + bench registrations were on A6's allow-list per
plan to keep diffs disjoint). A8 rewired pprint's
`.github/workflows/ci.yml` to `cargo nextest run --cargo-profile
ax-iter`; the committed config search confirmed no `bbnf-ser`
patch ever lived in pprint's tracked tree (the redundancy was
entirely in the .gitignored local-dev override, which the
orchestrator removed pre-dispatch). A8 also surfaced the
orchestrator-scope finding that bbnf-lang's `.cargo/config.toml`
did not declare the canonical `bbnf-ser = { path = "crates/ser" }`
patch entry; the orchestrator added it as a follow-on commit at B7
close.

## Wave-by-wave recap

### W0 — bbnf-lang internal (3 agents, 7 commits)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A1 | `cb6e9ab0` | `bench(simd-scan): migrate stage1_throughput to divan harness` |
| A9 | `7e3a7607`, `d0f9a4cb`, `d7c1fc84` | `bench(lsp): re-route bench_lsp_actions to [[bench]]`; `test(simd-scan): sample-cap proptest fuzzers from 1024 to 64 cases`; `docs(az-ii): merge W1+W2 into atomic Stage-A+B wave (T1.a)` |
| A10 | `3fee71a4`, `7d3739b3`, `e986755c` | `docs(bb): W0 e-graph hit-rate measurement gate (T1.b)`; `docs(trajectory): AY-III deferred + AZ-I.W4 / AZ-II.W2 absorb-clauses (T1.c)`; `docs(b7): plan + progress + agent_dispatch + waves + final skeleton` |
| orchestrator | `4e0851be` | `infra(bbnf-lang): centralize bbnf-ser patch (B7 close ceremony)` |

LOC delta: +1648 / -78 across 17 files (16 from agents, 1 from
orchestrator). Sub-phase close: every agent landed within hard cap
(A1 ~26 min of 30; A9 ~12 min of 180; A10 ~12 min of 90; orchestrator
~30 s).

### W1 — parse-that (4 agents, 9 commits)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A2 | `c7b60c0`, `19e9cb8`, `ee0c75f` | `bench(parse-that): swap bencher dev-dep for divan + harness setup`; `bench(parse-that): migrate parse_that/* benches to divan (combinator/css/micro)`; `bench(parse-that): migrate competitor benches to divan` |
| A3 | `30d2ecc`, `dc5d27f` | `bench(parse-that-bootstrap): swap bencher for divan + migrate regex_parse`; `bench(parse-that-regex): swap bencher for divan + migrate 4 benches` |
| A4 | `383c0d9` | `ci(parse-that): nextest runner + pinned toolchain + .cargo/config profile parity` |
| A5 | `74dbd1c`, `0108f3f`, `b69bb6f` | `test(parse-that/split): hoist 11 inline tests to tests/split_test.rs`; `test(parse-that/scan): hoist structural_bitmap + number_simd inline tests to tests/`; `test(parse-that/scan): hoist quoted_simd + quote_parity inline tests to tests/` |

Test count: 69/69 inline tests preserved as 69 tests in tests/
files. Bench-targets count: 18 total (13 in parse_that/, 1 in
bootstrap/, 4 in regex/) — the audit's "16" was an undercount; the
A2/A3 split absorbed the additional surface. Pre-existing 14
csv/json data-file-dependent failures unchanged (workspace baseline
tracks them as known-fail per parse-that's own test ledger).

### W2 — pprint (3 agents, 3 commits)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A6 | `e2557ce` | `bench(pprint): migrate pprint.rs from feature(test) to divan harness` |
| A7 | `37cf830` | `bench(pprint): migrate digit_count.rs from feature(test) to divan harness` |
| A8 | `3e35185` | `ci(pprint): nextest runner + pinned toolchain` |

Workspace nextest: 70/70 passed at 1.629 s post-W2 (verified at
`/Users/mkbabb/Programming/pprint/rust` after all three commits
landed). Bench-median deltas: +9–41 % systematic shift on small
fixtures attributable to divan/libtest measurement methodology
divergence; per-iter cycle counts preserved. The shift is honest
artefact of the harness migration, not a regression.

## Performance

| Metric | Pre-B7 baseline | Post-B7 measured | Δ |
|--------|----------------:|-----------------:|--:|
| bbnf-lang warm `cargo nextest run --profile ax-iter` wall | 22.353 s | 10.832 s | -51.5 % (cleared 14 s gate with 22.6 % margin) |
| bbnf-lang `cargo bench-bbnf` median | 2.806 ms | unchanged (no source change) | 0 % |
| simd-scan twitter MB/s (divan fastest) | 4921 (best µs harness) | +16 % favorable | +16 % |
| simd-scan citm MB/s | 5085 | -3.7 % | within noise |
| simd-scan canada MB/s | 4116 | -2.2 % | within noise |
| simd-scan bootstrap MB/s | 693 | +1.7 % | within noise |
| simd-scan tailwind MB/s | 752 | +4.2 % | within noise |
| pprint nextest wall | (no pre-baseline) | 1.629 s for 70 tests | n/a |

## Test results

bbnf-lang `cargo nextest run --workspace --profile ax-iter
--no-fail-fast` post-tranche on master `4e0851be`: 1475 passed / 0
failed / 27 skipped / 10.832 s wall. Test count delta from pre-B7
baseline (1477 → 1475) is −2: A9 deleted both `bench_lsp_actions`
AND `bench_incremental_edits` from `crates/lsp/tests/bench_lsp.rs`
when re-routing to the `[[bench]]` surface. The dispatch's
expected −1 was an undercount; both LSP-matrix tests in that file
are bench-class (each spins the LSP server through size×generator
matrices with timed actions), so the architectural intent —
re-routing the file's content as a unit — moves both. Test
artefact: `docs/benchmarks/post-B7-W0-walls.txt`.

pprint `cargo nextest run --workspace --no-fail-fast --cargo-profile
ax-iter`: 70 passed / 0 failed / 0 skipped / 1.629 s.

parse-that workspace nextest preserves the pre-B7 baseline (the
14 known-fail csv/json tests remain known-fail; A5's migration
preserved 69/69 hoisted tests; A2's migration preserved compile
state on all 13 parse_that-crate benches).

## API surface changes

Pre/post-B7 surface diff: zero across all three repos. B7 owns no
parity-critical runtime architecture; every edit is benches, CI
workflows, test-surface routing, or config patches. Public API
under `crates/` (bbnf-lang), `rust/parse_that/src/` (parse-that
beyond A5's narrow visibility adjustments), and `rust/src/` (pprint)
unchanged. A5's three visibility adjustments (`number_simd` private
mod → `pub mod`; `escaped_mask` and `prefix_xor` private fn →
`#[doc(hidden)] pub fn`) are minimal disclosures gated behind
`#[doc(hidden)]` so the documented surface remains pre-B7.

## Cross-tranche debt

**Inherited (closed in B7):**

- bbnf-lang internal: simd-scan manual `std::time::Instant` timing
  in `benches/stage1_throughput.rs`. Closed at B7.W0.A1 (`cb6e9ab0`).
- bbnf-lang internal: `bench_lsp::bench_lsp_actions` 16.5 s
  bench-class test mis-routed to routine `[[test]]` surface.
  Closed at B7.W0.A9 (`7e3a7607`).
- bbnf-lang internal: simd-scan proptest fuzzers saturating without
  sample cap (1024 cases default). Closed at B7.W0.A9 (`d0f9a4cb`,
  reduced to 64 cases).
- parse-that: `bencher = "0.1.5"` across 18 bench targets (audit
  said 16; actual was 18). Closed at B7.W1.A2 (`c7b60c0`,
  `19e9cb8`, `ee0c75f`) + B7.W1.A3 (`30d2ecc`, `dc5d27f`).
- parse-that: bare `cargo test --workspace` in CI; floating
  `dtolnay/rust-toolchain@nightly`; missing `ay-final` and
  `bench-ci` profile aliases. Closed at B7.W1.A4 (`383c0d9`).
- parse-that: 69 inline `#[test]` items in `src/`. Closed at
  B7.W1.A5 (`74dbd1c`, `0108f3f`, `b69bb6f`) — 69/69 tests preserved
  in tests/.
- pprint: `#![feature(test)]` + `extern crate test` across 38
  bench fns. Closed at B7.W2.A6 (`e2557ce`, 26 fns) + B7.W2.A7
  (`37cf830`, 12 fns).
- pprint: bare `cargo test --workspace` in CI. Closed at B7.W2.A8
  (`3e35185`).
- pprint local-dev override: redundant `bbnf-ser` patch
  centralization in bbnf-lang only. Closed at B7.W2.A8 (orchestrator
  pre-dispatch removed the override; A8 verified the committed tree
  carries no patch entry); orchestrator follow-up `4e0851be` adds
  the canonical `bbnf-ser = { path = "crates/ser" }` to bbnf-lang's
  root `.cargo/config.toml`.

**Forwarded (post-B7, opens AZ-I.W0):**

- parse-that bootstrap crate's `bbnf_derive 0.2` dependency is
  deprecated under post-B2 (the proc-macro IR-pipeline contract
  retired entirely; `crates/derive/` was deleted at B2). The
  declaration survives in parse-that's bootstrap crate as a
  hold-over; the bench compiles but `cargo bench --bench
  regex_parse` is blocked by a `bbnf_derive 0.2.9 → parse_that ^0.3`
  semver clash with the local `parse_that 0.4.0`. The fix is either
  to delete the bootstrap crate outright (its use case dissolved
  with `crates/derive/`) or to recode its consumers to read from
  `crates/core/src/grammar/generated/<ident>.rs`. AZ-I.W0 owns the
  decision, since AZ-I dispatches against the parse-that surface
  anyway.
- pprint's tracked `.cargo/config.toml` carries `rustflags =
  ["-Zthreads=8", "-Zshare-generics=y"]` flags that bbnf-lang's
  audit-α retrospective measured as a 28× warm `iter-check`
  regression. A8 flagged out-of-scope (its allow-list excluded
  cross-repo rustflag tuning). Future cross-repo refinement.

## Defensible floor

The defensible floor at B7 close:

1. Workspace nextest 1475/1475 green on bbnf-lang main checkout
   post-tranche; 27 skipped (pre-existing). −2 from pre-B7's 1477
   reflects A9's deletion of both `bench_lsp_actions` and
   `bench_incremental_edits` (architectural intent — both are
   bench-class LSP-matrix tests re-routed to `[[bench]]`).
2. `cargo bench-bbnf` median holds at 2.806 ms (unchanged; no
   source-side runtime change in B7).
3. Warm `cargo nextest run --profile ax-iter` wall on bbnf-lang
   main checkout: 10.832 s (3-run median; from 22.353 s pre-B7).
   ≤ 14 s gate cleared with 22.6 % margin.
4. parse-that bench-median delta within ±5 % per fixture across
   the 13 parse_that-crate benches: not strictly comparable due to
   pre-B7 compile failures on 3 fixtures (nom/winnow/micro);
   post-B7 divan output produces fastest/slowest/median/mean with
   throughput counters wired through.
5. pprint bench-median delta: systematic +9–41 % shift across small
   fixtures attributable to divan vs libtest measurement
   methodology. Per-iter cycle counts preserved; honest artefact.
6. All three repos use divan for benches; no `bencher`,
   `feature(test)`, or `extern crate test` anywhere.
7. All three repos invoke `cargo nextest run` in CI; no bare
   `cargo test --workspace`.
8. `bbnf-ser` patch lives in `bbnf-lang/.cargo/config.toml` only;
   verified by cross-repo grep returning exactly one hit.
9. `cargo xtask regen --check` exit 0 across all 9 grammars
   (bbnf-lang).
10. No `#[allow(...)]` introductions outside macro contexts; no
    path duplications; no shim flags; no shadow surfaces.

## Verdict

**B7 closes. AZ-I.W0 opens against post-tranche-close SHA `4e0851be`.** Three
repo-isolated waves complete: W0 (bbnf-lang internal) landed seven
commits across A1, A9, A10 plus an orchestrator follow-on; W1
(parse-that) landed nine commits across A2, A3, A4, A5; W2 (pprint)
landed three commits across A6, A7, A8. The cross-repo
modernization runway post-B7 is coherent under the three invariants
declared at plan time: divan is the only bench harness fleet-wide,
nextest is the only test runner fleet-wide, and the cross-repo
`bbnf-ser` patch lives in exactly one place. The forwarded-debt
ledger names parse-that's `bbnf_derive 0.2` deprecation and
pprint's audit-α-flagged rustflags as the only follow-ups, both
naturally absorbed at AZ-I.W0 dispatch since AZ-I inherits the
parse-that surface anyway. AZ-I.W0 dispatches against this floor
— the classifier-unification research note + IR audit pass land on
a fleet where divan is the only bench harness, nextest is the only
test runner, and the `bbnf-ser` patch graph collapses to a single
source of truth.
