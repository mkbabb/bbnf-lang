# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V9

Verdict: ACCEPT

Confidence: 96%

Target reviewed: `00c3485a156e78f96885fd13fcc7f47d0c8179ed`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Scope

CH2 reviewed Lock 14 grammar neutrality, no new directive/BIR/substrate/
`BackendShape`/`UnionTape`, frozen behavior surface, strict-vs-strict
discipline, and non-JSON proof after the V8 rejection. The V9 implementation
diff is confined to `skinny/crates/bbnf-bench/src/gate.rs` and
`skinny/crates/bbnf-bench/src/report.rs`; `git diff --name-status
00c3485a^..00c3485a` reports only those two files.

## Evidence

- V8's required fold was the exact hard-failure strict-admission blocker:
  current W0 rows must remain `strictness=deferred`,
  `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
  `escape_complete=yes`; hard-failure/non-admission outcomes must reject strict
  admission; and the `canada` `L / NO-GO` strict/measured/DOM repro must fail
  W0 validation
  (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:30`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:47`).
- The report-side W0 gate now freezes all W0 rows to deferred/view-boundary
  validation and rejects any W0 row relabeled as strict or measured-row
  (`skinny/crates/bbnf-bench/src/report.rs:1012`-`skinny/crates/bbnf-bench/src/report.rs:1037`).
  The focused `canada` hard-failure repro is covered by the W0 baseline test
  (`skinny/crates/bbnf-bench/src/report.rs:1954`-`skinny/crates/bbnf-bench/src/report.rs:1964`).
- The helper-level strict-admission gate now rejects every non-`GO` outcome
  before looking at strict comparator evidence
  (`skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`).
  The test enumerates `D`, `E`, `F-positive`, `F-noise`, `G`, `I`, `J`, `K`,
  `L`, `M`, `N-direct`, and `S` as strict-ineligible
  (`skinny/crates/bbnf-bench/src/gate.rs:459`-`skinny/crates/bbnf-bench/src/gate.rs:483`).
- Strict-vs-strict and sidecar boundaries remain fail-closed. Native comparator
  validation requires the expected output plane, strictness, `same-run-native`
  freshness, `sidecar_freshness=n/a`, and a Criterion source artifact
  (`skinny/crates/bbnf-bench/src/report.rs:1229`-`skinny/crates/bbnf-bench/src/report.rs:1290`);
  sidecar same-run claims still reject without a structured manifest
  (`skinny/crates/bbnf-bench/src/report.rs:1203`-`skinny/crates/bbnf-bench/src/report.rs:1208`).
- Lock 14 scope remains W0/report-gate-local. The SK-V8 SPEC forbids new
  directives, BIR variants, `BackendShape` variants, `UnionTape`, new substrate
  surfaces, parser-owned structural cursors/facts, parallel/sidecar substrate,
  and generic-crate JSON policy
  (`restart/skinny/tranches/sk-v8/SPEC.md:191`-`restart/skinny/tranches/sk-v8/SPEC.md:212`).
  W0 owner paths are `bbnf-bench`, `xtask`, `RESULTS.md`, research artifacts,
  and `REDRESS.md` only on rejection
  (`restart/skinny/tranches/sk-v8/SPEC.md:288`-`restart/skinny/tranches/sk-v8/SPEC.md:297`).
- The non-JSON proof is unchanged-output/frozen-root based, which is valid for
  W0 because V9 did not edit generic CostFacts, codegen, runtime, SIMD, or
  parser-template surfaces. SPEC explicitly permits unchanged-output audit as
  non-JSON proof for generic edits
  (`restart/skinny/tranches/sk-v8/SPEC.md:261`-`restart/skinny/tranches/sk-v8/SPEC.md:286`),
  and the Lock 14 baseline freezes grammar/runtime/IR/passes/codegen/grammar/
  SIMD/parser/product-plane roots
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:405`).
  `git diff --name-only 0bd16f6d..HEAD -- <frozen roots>` returned no paths.
- No forbidden surface was introduced. `rg` over the V9 diff for `UnionTape`,
  `union_tape`, `BackendShape`, `BIR`, `directive`,
  `StructuralAlphabet::json`, `JsonPolicy`, `json_policy`, new substrate,
  substrate surface, sidecar substrate, and parallel substrate returned no
  matches. The Lock 14 baseline also rejects forbidden names and verifies the
  five-variant `BackendShape` surface while rejecting `UnionTape`
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:365`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:367`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:491`).
- Later behavior waves remain blocked. W1-W6 require W0 admission, fresh
  research/plan artifacts, exact owner paths and row gates, pre-blocked-route
  citations, challenge acceptance where required, Lock 14 proof for generic
  edits, and the 90-minute redress cap
  (`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:113`).

## Commands

- `git status --short` was clean before writing this report.
- `git diff --name-status 00c3485a^..00c3485a` returned only
  `skinny/crates/bbnf-bench/src/gate.rs` and
  `skinny/crates/bbnf-bench/src/report.rs`.
- `git diff --shortstat 84f885a4..HEAD` returned `2 files changed, 61
  insertions(+), 64 deletions(-)`.
- `git diff --name-only 0bd16f6d..HEAD -- <frozen roots>` returned no paths.
- `git diff 84f885a4..HEAD -- skinny/crates/bbnf-bench/src/gate.rs
  skinny/crates/bbnf-bench/src/report.rs | rg -n "<forbidden-surface-regex>"`
  returned no matches.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and
  8 gate-binary W0 tests.
- `cargo test -p bbnf-bench strict -- --nocapture` passed 5 focused strict
  tests.
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed the focused
  sidecar-same-run rejection test.
- `cargo test -p bbnf-bench` passed 52 library tests, 8 gate-binary tests, and
  doc tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
  xtask gate-json --advisory --check-results` passed and validated the committed
  `RESULTS.md`.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and `cargo xtask
  check-conformance` passed; conformance reported 21 valid fixtures accepted and
  7 invalid fixtures rejected.
- `git diff --check` returned no whitespace errors.

## Blockers

None.

## Required Fold

None. CH2 accepts V9: the fold closes the strict hard-failure relabel path
without introducing generic JSON policy, grammar-name leakage into generic
crates, directive/BIR/substrate/`BackendShape`/`UnionTape` drift, behavior
surface movement, or non-JSON proof regression.
