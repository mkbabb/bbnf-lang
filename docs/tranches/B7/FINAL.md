# Tranche B7 — FINAL

**STATUS: SKELETON.** This document is authored at plan time as a
skeleton; the orchestrator populates each TBD section at tranche
close after all ten agents have committed. Every section below
that ends in `(TBD)` is a placeholder the orchestrator fills with
the actual close-ceremony evidence.

## Architectural narrative

(TBD — orchestrator at close.)

The narrative reconstructs how B7's three repo-isolated waves
landed: what the actual cross-repo modernization measurements
showed; whether the divan port revealed any non-mechanical
divergence on parse-that or pprint; whether A9's test-surface
partition unmasked any state-leakage flake; how the warm nextest
wall measurement landed against the ≤ 14 s gate. The narrative
follows the B6/FINAL.md template — three short subsections per
wave (W0/W1/W2), each subsection telling the story of what the
plan presupposed vs what landed.

## Wave-by-wave recap

### W0 — bbnf-lang internal (TBD per agent)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A1 | (TBD) | (TBD — port stage1_throughput to divan) |
| A9 | (TBD) | (TBD — re-route bench_lsp_actions; sample-cap simd-scan fuzz; T1.a doc scrub) |
| A10 | `e3555af6`, `dcb335db`, (TBD third) | T1.b BB.W0 hit-rate gate; T1.c AY-III deferred + AZ-I.W4 + AZ-II.W2 absorb; B7 plan + PROGRESS + AGENT_DISPATCH + waves + FINAL skeleton |

(TBD — LOC delta per agent; cherry-pick conflicts encountered;
any sub-phase that closed on rationale-satisfied vs landed.)

### W1 — parse-that (TBD per agent)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A2 | (TBD) | (TBD — divan migration of 8 bench targets) |
| A3 | (TBD) | (TBD — divan migration of remaining 8 bench targets) |
| A4 | (TBD) | (TBD — CI nextest + `.cargo/config` profile parity) |
| A5 | (TBD) | (TBD — inline-`#[test]` cleanup) |

(TBD — bench-median delta per fixture; CI green confirmation;
test count pre/post.)

### W2 — pprint (TBD per agent)

| Agent | Commit(s) | One-line |
|-------|-----------|----------|
| A6 | (TBD) | (TBD — pprint.rs divan migration) |
| A7 | (TBD) | (TBD — digit_count.rs divan migration) |
| A8 | (TBD) | (TBD — CI nextest + `bbnf-ser` patch resolution) |

(TBD — bench-median delta per fixture; cross-repo patch
verification.)

## Performance

(TBD — orchestrator at close.)

| Metric | Pre-B7 baseline | Post-B7 measured | Δ |
|--------|----------------:|-----------------:|--:|
| bbnf-lang warm `cargo nextest run --profile ax-iter` wall | 22.353 s | (TBD) | (TBD) |
| bbnf-lang `cargo bench-bbnf` median | 2.806 ms | (TBD) | (TBD) |
| parse-that bench-median (combined fixtures) | (TBD pre) | (TBD post) | (TBD; ±5 % gate) |
| pprint bench-median (combined fixtures) | (TBD pre) | (TBD post) | (TBD; ±5 % gate) |

## Test results

(TBD — orchestrator at close.)

`cargo nextest run --workspace --profile ax-iter --no-fail-fast`
post-tranche on bbnf-lang main checkout: (TBD passed) /
(TBD failed) / (TBD skipped) / (TBD wall).

parse-that CI run on tranche-close commit: (TBD pass/fail; URL).

pprint CI run on tranche-close commit: (TBD pass/fail; URL).

## API surface changes

(TBD — orchestrator at close.)

B7 owns no parity-critical runtime architecture; the API surface
diff across all three repos is expected to be zero. The
orchestrator confirms by cross-repo `git diff --stat` audit that
no `pub` surface in `crates/` (bbnf-lang), `rust/parse_that/src/`
(parse-that), or `rust/src/` (pprint) changed.

## Cross-tranche debt

(TBD — orchestrator at close.)

**Inherited (closed in B7):**

- bbnf-lang internal: simd-scan manual `std::time::Instant`
  timing in `benches/stage1_throughput.rs`. (Closed at B7.W0.A1
  commit (TBD).)
- bbnf-lang internal: `bench_lsp::bench_lsp_actions` 16.5 s
  bench-class test mis-routed to routine `[[test]]` surface;
  simd-scan proptest fuzzers saturating without sample cap.
  (Closed at B7.W0.A9 commits (TBD).)
- parse-that: `bencher = "0.1.5"` across 16 bench targets; bare
  `cargo test --workspace` in CI; floating
  `dtolnay/rust-toolchain@nightly`; 69 inline `#[test]` items
  in `src/`; missing `ay-final` and `bench-ci` profile aliases.
  (Closed at B7.W1.A2 + A3 + A4 + A5 commits (TBD).)
- pprint: `#![feature(test)]` + `extern crate test` across 38
  bench fns; bare `cargo test --workspace` in CI; redundant
  `bbnf-ser` patch in `rust/.cargo/config.toml` shadowing the
  canonical bbnf-lang entry. (Closed at B7.W2.A6 + A7 + A8
  commits (TBD).)

**Forwarded (post-B7, opens AZ-I.W0):**

- parse-that bootstrap crate's `bbnf_derive 0.2` dependency is
  deprecated under post-B2 (the proc-macro IR-pipeline contract
  retired entirely; `crates/derive/` was deleted at B2.W2). The
  declaration survives in parse-that's bootstrap crate as a
  hold-over. AZ-I scope cleanup — its consumers either delete the
  bootstrap crate outright (its use case dissolved) or recode to
  read from `crates/core/src/grammar/generated/<ident>.rs`.

(TBD — vacuous-closed items, if any wave closes on rationale-
satisfied.)

## Defensible floor

(TBD — orchestrator at close.)

The defensible floor at B7 close:

1. Workspace nextest at 1477+/1477+ green; (TBD) skipped pre-
   existing on bbnf-lang main checkout.
2. `cargo bench-bbnf` median holds within 5 % of B5 baseline
   (2.806 ms): measured (TBD ms).
3. Warm `cargo nextest run --profile ax-iter` wall on bbnf-lang
   main checkout at (TBD s); ≤ 14 s gate (TBD: cleared / missed).
4. parse-that bench-median delta within ±5 % per fixture across
   all 16 targets: (TBD: cleared / missed).
5. pprint bench-median delta within ±5 % per fixture across both
   targets: (TBD: cleared / missed).
6. All three repos pass `cargo nextest run` in CI on the
   tranche-close commit.
7. All three repos use divan for benches; no legacy harness
   anywhere.
8. `bbnf-ser` patch lives in `bbnf-lang/.cargo/config.toml`
   only; cross-repo grep returns exactly one hit.
9. `cargo xtask regen --check` exit 0 across all 9 grammars at
   every wave close.
10. No `#[allow(...)]` introductions outside macro contexts; no
    path duplications; no shim flags; no shadow surfaces.

## Verdict

(TBD — orchestrator at close.)

**B7 closes. AZ-I.W0 opens against post-tranche-close SHA.**
Three repo-isolated waves complete: (TBD W0 outcome); (TBD W1
outcome); (TBD W2 outcome). The cross-repo modernization runway
post-B7 is (TBD: coherent under the three invariants / partial
under named miss). AZ-I.W0 dispatches against this floor — the
classifier-unification research note + IR audit pass land on a
fleet where divan is the only bench harness, nextest is the only
test runner, and the `bbnf-ser` patch graph collapses to a single
source of truth.
