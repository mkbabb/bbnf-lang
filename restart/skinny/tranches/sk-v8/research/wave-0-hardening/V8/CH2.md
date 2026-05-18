# CH2 GENERALITY / Lock 14 - SK-V8 W0 Hardening V8

Verdict: ACCEPT

Confidence: 94%

Target reviewed: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).
Current HEAD `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd` adds only V7
hardening docs relative to the target; I did not treat those docs as
implementation changes.

## Reviewed Surfaces

- SK-V8 dispatch and wave gating: W0-only authority at
  `restart/skinny/tranches/sk-v8/SPEC.md:35`-`restart/skinny/tranches/sk-v8/SPEC.md:38`,
  W1-W6 conditional gates at
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:113`,
  and W0-only handoff authority at
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`-`restart/skinny/tranches/sk-v8/HANDOFF.md:239`.
- Lock 14 / generality contract: no new directive/BIR/BackendShape/substrate,
  no generic JSON policy at
  `restart/skinny/tranches/sk-v8/SPEC.md:191`-`restart/skinny/tranches/sk-v8/SPEC.md:212`,
  and generic-crate/non-JSON proof rules at
  `restart/skinny/tranches/sk-v8/SPEC.md:261`-`restart/skinny/tranches/sk-v8/SPEC.md:286`.
- W0 owner/report/gate surfaces: `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/lib.rs`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`, `skinny/xtask/src/main.rs`,
  and `skinny/RESULTS.md`.
- W0 cost/run governance: the reauthorized measured W0 gate/report/Lock 14 slice
  and frozen behavior-surface condition at
  `restart/skinny/tranches/sk-v8/SPEC.md:322`-`restart/skinny/tranches/sk-v8/SPEC.md:344`,
  mirrored at
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`-`restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:61`.
- V7 convergence state: V7 consolidated as first post-V6 qualifying ACCEPT at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:18`,
  and requested this unchanged V8 re-challenge at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:47`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:52`.

## Commands / Evidence

- `git status --short` was clean before artifact creation.
- `git diff --name-status f452e837..HEAD` returned only the seven added V7
  hardening artifacts under
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/`.
- `git diff --name-only 0bd16f6d..f452e837 -- <frozen behavior roots>` returned
  no paths. The same check from `6c0bc15d..f452e837` over generic roots also
  returned no paths.
- `git diff 0bd16f6d..f452e837 -- <generic roots> | rg -n "json|Json|JSON|StructuralAlphabet::json|UnionTape|directive|BIR|BackendShape|substrate"`
  returned no matches. This is the non-JSON unchanged-output proof for W0:
  generic runtime/IR/passes/codegen/grammar/SIMD/parser roots did not move.
- `rg -n "UnionTape|union_tape|pub enum BackendShape|BackendShape|@[^\\n]*directive|new directive|BIR|substrate surface|sidecar substrate|parallel substrate|StructuralAlphabet::json|generic_json_helper|JsonPolicy|json_policy" ...`
  found expected Lock 14 validator code and negative tests only; no new
  production `UnionTape`, directive, BIR, BackendShape, or substrate surface.
- `awk` over `skinny/RESULTS.md` found `main_rows=38`,
  `manifest_rows=38`, with `parse_only` = 16 `S` plus 1 `L`,
  `direct_to_struct` = 3 `A` plus 14 `N-direct`, and
  `real_typed_struct` = 4 `A`.
- `cargo test -p bbnf-bench w0_ -- --nocapture` passed 12 report W0 tests and
  8 gate-binary W0 tests.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 7 Lock 14
  tests.
- `cargo test -p bbnf-bench` passed 52 library tests, 8 gate-binary tests, and
  doc tests.
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
  passed and validated the committed `RESULTS.md` without rewriting it.
- `cargo xtask check-json`, `cargo xtask check-real-typed`, and
  `cargo xtask check-conformance` passed; conformance reported 21 valid fixtures
  accepted and 7 invalid fixtures rejected.
- `git diff --check` returned no whitespace errors.

## Findings

- No generic JSON policy landed. The W0 diff does not touch generic runtime, IR,
  passes, codegen, grammar, SIMD, or parse-that-regex roots; the frozen behavior
  roots are explicitly guarded at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:397`.
- JSON grammar names remain confined to W0 report/gate/fixture identity. The
  report requires `grammar_id=json` / `domain=json_bench` at
  `skinny/crates/bbnf-bench/src/report.rs:322`-`skinny/crates/bbnf-bench/src/report.rs:328`,
  and row ids parse only as `json/<corpus>/<workload>/main` at
  `skinny/crates/bbnf-bench/src/report.rs:1325`-`skinny/crates/bbnf-bench/src/report.rs:1335`.
  Those checks live in `bbnf-bench`, not in generic parser/codegen/runtime code.
- `gate-json` is a same-wave telemetry consumer, not behavior authority. It
  validates the Lock 14 baseline before report construction at
  `skinny/crates/bbnf-bench/src/bin/gate.rs:37`-`skinny/crates/bbnf-bench/src/bin/gate.rs:44`,
  consumes schema plus W0 validation at
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`-`skinny/crates/bbnf-bench/src/bin/gate.rs:327`,
  and writes `RESULTS.md` only behind explicit update flags at
  `skinny/crates/bbnf-bench/src/bin/gate.rs:329`-`skinny/crates/bbnf-bench/src/bin/gate.rs:339`.
- The Lock 14 allowlist separates allowed JSON surfaces from generic surfaces:
  grammar input at `skinny/crates/bbnf-bench/src/lock14_baseline.rs:13`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:19`,
  generated JSON output at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:129`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:181`,
  per-grammar templates at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:189`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:216`,
  generic read-only surfaces at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:219`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:258`,
  and telemetry-only report/gate surfaces at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:267`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:295`.
- Directive/BIR/substrate drift remains blocked. Allowlist validation rejects
  unsupported classes, forbidden surface names, and missing paths at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:343`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:372`;
  dirty/diffed frozen roots reject at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:405`;
  BackendShape count and `UnionTape` drift reject at
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462`-`skinny/crates/bbnf-bench/src/lock14_baseline.rs:491`.
- Sidecar and strict-admission boundaries still fail closed. Strict admission
  requires strict row/comparator, measured-row UTF-8, matching output plane,
  measured validation path, and same-run native comparator freshness at
  `skinny/crates/bbnf-bench/src/gate.rs:135`-`skinny/crates/bbnf-bench/src/gate.rs:175`.
  Sidecar same-run claims without a structured manifest reject at
  `skinny/crates/bbnf-bench/src/report.rs:1235`-`skinny/crates/bbnf-bench/src/report.rs:1240`.
  Negative tests cover deferred validation, stale/sidecar freshness, and
  sidecar-same-run at
  `skinny/crates/bbnf-bench/src/gate.rs:451`-`skinny/crates/bbnf-bench/src/gate.rs:501`.
- The run-id binding is W0-scoped and exact. The W0 run id constant is
  `skinny/crates/bbnf-bench/src/report.rs:660`, validation rejects any row whose
  run id moves at
  `skinny/crates/bbnf-bench/src/report.rs:336`-`skinny/crates/bbnf-bench/src/report.rs:340`,
  and tests reject both single-row and uniform fake run-id mutations at
  `skinny/crates/bbnf-bench/src/report.rs:1976`-`skinny/crates/bbnf-bench/src/report.rs:1984`.
- Cost governance remains scoped to W0 and does not authorize behavior waves.
  W0 report fields are `none:pre-W1` in the manifest renderer at
  `skinny/crates/bbnf-bench/src/report.rs:600`-`skinny/crates/bbnf-bench/src/report.rs:605`,
  while W1 CostFacts remains a later conditional gate at
  `restart/skinny/tranches/sk-v8/SPEC.md:374`-`restart/skinny/tranches/sk-v8/SPEC.md:429`.
  W3/W4 behavior paths remain blocked on W0/W1 and fresh plan/challenge gates at
  `restart/skinny/tranches/sk-v8/SPEC.md:527`-`restart/skinny/tranches/sk-v8/SPEC.md:542`
  and `restart/skinny/tranches/sk-v8/SPEC.md:612`-`restart/skinny/tranches/sk-v8/SPEC.md:615`.
- The committed report matches the boundary: the W0 manifest starts at
  `skinny/RESULTS.md:44`, the manifest header is at `skinny/RESULTS.md:46`,
  and the report note states native Rust comparators are same-run while C++
  sidecars are historical or absent and never W0 strict anchors at
  `skinny/RESULTS.md:141`.

## Material Blockers

None for CH2 / Lock 14. I found no generic JSON policy, no grammar-name leak
beyond W0 owner/report/fixture surfaces, no directive/BIR/substrate drift, no
broken non-JSON proof, and no run-id/cost-governance path that authorizes later
behavior waves.

## Residual Risks

- W0 intentionally hard-codes JSON row ids, comparator ids, and `json_bench`
  identity inside `bbnf-bench`. This remains acceptable only while confined to
  W0 report/gate/fixture governance and must not migrate into generic crates.
- The non-JSON proof is unchanged-output based because W0 did not edit generic
  code. Any later generic CostFacts, codegen, runtime, SIMD, or parser-template
  edit still needs fresh CSS L4 / Sheets / BBNF-self proof under
  `restart/skinny/tranches/sk-v8/SPEC.md:279`-`restart/skinny/tranches/sk-v8/SPEC.md:282`.
- The Lock 14 frozen-root set is sufficient for the current W0 slice. Later
  waves must expand the allowlist/frozen-root coverage when owner paths or
  generated surfaces expand.

## Consecutive Accept Status

For CH2, this V8 ACCEPT can count as the second consecutive accepting
unchanged re-challenge after the V7 CH2 ACCEPT. For overall W0 convergence, V8
can count as the required second consecutive ACCEPT cycle only if the V8
consolidated result is also >=95% ACCEPT with no critical defect and no
unresolved REVISE, matching the V7 consolidated requirement at
`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:47`-`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:52`.
