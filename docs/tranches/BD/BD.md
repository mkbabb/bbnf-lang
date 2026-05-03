# BD — Multi-Backend Activation + Sister-Crate Publication + Cross-Backend Parity

## Gestalt

BD is the multi-backend activation tranche: hereupon BC's TS + WASM emitter scaffolds (`docs/tranches/BC/waves/W2.md:36-58`) activate into production; the sister crates frozen at BC.W5 (`docs/tranches/BC/waves/W5.md:55-77`) publish to crates.io; the worktree fixture contract closed at BC.W5 (`docs/tranches/BC/waves/W5.md:79-86`) extends to per-grammar fleet-wide CI; cross-backend parity verifies that the same JSON byte-input produces equivalent typed value across Rust + TS + WASM. The thesis is **scaffold-to-production-by-mechanism**: BC's three-step ratification (typed IR + Emitter trait + smoke test) proved Lock 5 supports TS + WASM; BD ratifies that the production paths surpass research baselines (simdjson-wasm, NAPI-RS-bound parsers) and ship to npm / crates.io.

The 13 architectural locks ratify here at BD close. Lock 5 (IR + per-backend lower) ratifies at runtime via TS + WASM activation: production parsers ship as `@bbnf-lang/runtime` (NAPI-RS native binary per-platform) and `@bbnf-lang/runtime-wasm` (wasm-bindgen + wasm-pack browser/Node bundle). Lock 11 (path-deps for incubating sister crates) ratifies at publication: `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` move from path-deps to crates.io with `cargo-semver-checks` validation. Lock 7 (`crates/path/` consolidation) ratifies at path-triplet publication: `path`, `path-core`, `path-ts` ship as a coordinated triplet — `path-ts` is the NAPI-RS cdylib, `path` is the Rust proc-macro shell, `path-core` is the deduplicated path-AST + compile logic.

Hereupon BD has no successor. The carry ledger CLOSES at BD close. There is no BE. There is no BF. There is no BG. The plan ends here. Phase 5 hardening verifies the close.

The thesis is contract-first activation begotten of the ratified scaffold. BC.W2's scaffold proved the IR contract supports TS + WASM by mechanism; BD activates the production paths against external SOTA — V8 + WASM — and ships the artefacts to npm. Final perf gates are bounded by V8 / WASM realities, not by sonic-rs's native floor: BD-G1 (`@bbnf-lang/runtime` parses twitter.json in ≤ 8 ms on M1 Pro Node 20.x) is bounded by NAPI-RS marshalling overhead; BD-G2 (`@bbnf-lang/runtime-wasm` parses twitter.json in ≤ 2.5 ms on M1 Pro Node 20.x) is bounded by simdjson-wasm's measured ~870 µs - 1.45 ms range; BD-G3 — the SOTA-relative gate — asserts that BD's TS path beats `JSON.parse` for any non-trivial transformation (typed-records-with-validation use case) by amortising the marshal cost.

## Hard gates

Every parse-throughput gate cites a specific external baseline + dataset + platform per Lock 8. Engineering gates (publication, parity, fixtures) are non-throughput; their gates are cargo / shell commands proving the artefact lands.

| ID | Gate | Anchor |
|---|---|---|
| BD-G1 | `@bbnf-lang/runtime` (NAPI-RS native) parses `twitter.json` (631 KB) in ≤ 8 ms on M1 Pro Node 20.x — bounded by NAPI-RS marshalling overhead per `docs/tranches/BD/audit/research-anchors.md:§1`; ratio cap is "≤ 18× the BC native floor of 380 µs" reflecting cross-FFI cost; hard floor is `JSON.parse` ≈ 5 ms on V8 for the same input — BD's native-bound parse must close to within 1.6× of `JSON.parse` for the typed-records use case where `JSON.parse` would require a separate validation pass | `audit/SOTA-2026-05-03.md:50-58` (sonic-rs 436 µs native baseline); V8 `JSON.parse` ~5 ms (engineering measurement); NAPI-RS marshal ~100 ns per call |
| BD-G2 | `@bbnf-lang/runtime-wasm` (wasm-bindgen + wasm-opt) parses `twitter.json` in ≤ 2.5 ms on M1 Pro Node 20.x — bounded by simdjson-wasm's measured ~870 µs - 1.45 ms range per `docs/tranches/BD/audit/research-anchors.md:§2`; ratio cap is "≤ 6.5× the BC native floor of 380 µs" reflecting WASM lacks AVX2 + SIMD128 partial coverage | simdjson-wasm benchmarks per `docs/tranches/BD/audit/research-anchors.md:§2` |
| BD-G3 | Sister crates (`egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex`) publish to crates.io with `cargo-semver-checks` clean against the BC.W5 freeze baseline; `cargo install <crate>` succeeds end-to-end for each | Lock 11; `docs/tranches/BC/waves/W5.md:55-77` (freeze baseline); `docs/tranches/BD/audit/research-anchors.md:§3` (cargo-release pipeline) |
| BD-G4 | Path triplet (`path`, `path-core`, `path-ts`) publishes coordinated: `path-core` to crates.io (Rust), `path` to crates.io (Rust proc-macro), `path-ts` to npm as `@bbnf-lang/path-ts` (NAPI-RS native + per-platform sub-packages) | Lock 7; `docs/tranches/BC/waves/W5.md:124-126` (freeze precondition) |
| BD-G5 | Cross-backend parity matrix passes: 9 grammars × ≥ 3 fixtures each = ≥ 27 parity tests; for each row, Rust + TS + WASM produce byte-equivalent serialised typed value (modulo float-repr) | Lock 5 production ratification; `docs/tranches/BD/audit/research-anchors.md:§5` |
| BD-G6 | Per-grammar test fixture infrastructure: every grammar has `crates/bbnf-parse/tests/fixtures/<grammar>/` directory with ≥ 3 canonical inputs; `cargo test -p bbnf-parse --test fixtures_<grammar>` runs cleanly; CI matrix expands fleet-wide (Rust + TS + WASM × 9 grammars) | own gate; carry-tag BC→BD.C3 closure |
| BD-G7 | Carry-ledger CLOSED: BD has no successor; BD.W6 closes the trilogy (BA + BB + BC + BD) with no forward carries; the 13-lock cross-reference verifies every lock at one of (BA, BB, BC, BD, pre-BA ceremony) | own gate; the absence-of-carry IS the gate |

## Wave summary

| Wave | Deliverable | Invariant | Closer-gate |
|---|---|---|---|
| BD.W0 | TS proc-macro shell activation: `path-ts` cdylib (NAPI-RS) compiles + binds; `LayoutSink` trait implementation produces TS source on the typed IR contract; trivial-grammar smoke test from BC.W2 graduates to "compiles + binds + runtime executes" | Lock 7 (path-ts is the NAPI-RS shell); Lock 5 (typed IR remains the input). Era V failure mode mitigated: same-wave consumer is the runtime smoke test that imports the published binary. | `cargo build -p path-ts --release` produces `.node` binary; `node -e "require('./path-ts.darwin-arm64.node').compile('test')" ` runs cleanly; trivial-grammar parse fn emit produces compileable TS. |
| BD.W1 | TS runtime emitter activation: BC.W2 scaffold's `unimplemented!()` paths fill with production TS source; full grammar emission for JSON + at-least-one cohort grammar (CSV recommended); `@bbnf-lang/runtime` npm package ships with per-platform NAPI-RS sub-packages (darwin-arm64, darwin-x64, linux-x64-gnu, linux-arm64-gnu, win32-x64-msvc); host-fn resolution table activates per-backend | Lock 5 (TS production path consumes typed IR); BD-G1 measures the throughput against `JSON.parse` for the typed-records use case. | `npm install @bbnf-lang/runtime` resolves on M1 Pro Node 20.x; `node bench-twitter.js` produces ≤ 8 ms median over 100 samples; cross-platform smoke tests pass on darwin-arm64 + linux-x64-gnu CI runners. |
| BD.W2 | WASM compilation pipeline activation: `bbnf-codegen/src/wasm/` scaffold's WAT skeletons fill with production wasm-bindgen output; `cargo build --target wasm32-unknown-unknown --release` produces `.wasm` cdylib; wasm-pack assembles the npm package; `@bbnf-lang/runtime-wasm` ships with `--target bundler` + `--target nodejs` dual-build | Lock 5 (WASM production path consumes typed IR); BD-G2 measures the throughput. | `wasm-pack build --target bundler --release` produces clean output; `wasm-opt` validation passes; `node test-wasm-twitter.js` produces ≤ 2.5 ms median over 100 samples; bundle size ≤ 250 KB gzipped. |
| BD.W3 | Sister-crate publication: `egraph-derive`, `egraph`, `csp-solver`, `bbnf-regex` publish to crates.io in dependency order via `cargo-release`; `cargo-semver-checks` validates each against BC.W5 freeze baseline; `path-core`, `path` publish to crates.io; `path-ts` publishes to npm | Lock 11 (path-deps graduate to crates.io). The publication order is derived from the path-dep DAG; cargo-release auto-computes. | `cargo install egraph-derive egraph csp-solver bbnf-regex` succeeds end-to-end; `npm install @bbnf-lang/path-ts` resolves; `cargo-semver-checks` clean for each. |
| BD.W4 | Worktree fixture fleet-wide expansion: per-grammar `crates/bbnf-parse/tests/fixtures/<grammar>/` directories materialise with ≥ 3 canonical inputs each; xtask `worktree-init` extends to materialise CI fixtures via symlinks; the per-grammar fixture pattern (path-dep workspace) replaces ad-hoc test inputs | Lock 13 (cohesive encapsulation); BC→BD.C3 carry closure. | `cargo test -p bbnf-parse --tests` runs the per-grammar fixture suites; `xtask worktree-init --fleet` materialises every grammar's fixtures cleanly. |
| BD.W5 | Cross-backend parity verification: matrix of 9 grammars × ≥ 3 fixtures × 3 backends (Rust, TS, WASM) executes; equivalence relation (canonical-JSON serialise + byte-compare modulo float-repr) holds on every cell; CI gates expand fleet-wide | Lock 5 production ratification (the IR contract supports all three backends in production, not just scaffold). | `cargo test --test parity_matrix` passes on Rust; `npm run test:parity` passes on TS; `wasmtime run parity-test.wasm` passes on WASM; CI matrix shows 9 × ≥ 3 × 3 = ≥ 81 cells green. |
| BD.W6 | BD close: PROGRESS / FINAL artefacts; carry ledger CLOSED (no successor); 13-lock cross-reference verified; final perf gates BD-G1 + BD-G2 met; sister crates published; cross-backend parity fleet-wide | All 13 locks honoured at one of (BA, BB, BC, BD, pre-BA ceremony); no forward carries. | `cargo nextest run --workspace` 100% pass; `npm test --workspaces` 100% pass; `docs/tranches/BD/FINAL.md` records the close attestation. |

## Carry-tags FROM BC

The three carries land in BD.W0, BD.W3, BD.W4 respectively. Each carry has owning BC wave + receiving BD wave + close gate.

| Tag | Owner wave | Description | Receiving BD wave | Close gate |
|---|---|---|---|---|
| BC→BD.C1 | BC.W2 | TS + WASM emitter scaffolds compile against the IR contract; produce trivial-grammar output; fail gracefully on host-fn shim sites with `unimplemented!()` (Rust) / `throw new Error(...)` (TS) / `unreachable` (WAT). BD activates: TS production emitter fills the unimplemented sites with `runtime.<host_fn>` resolution; WASM production emitter fills with indexed extern import; host-fn per-backend resolution table per `docs/tranches/BC/waves/W2.md:68-74`. | BD.W0 (TS proc-macro activation); BD.W1 (TS runtime emitter); BD.W2 (WASM pipeline) | BD.W1 closes BD-G1; BD.W2 closes BD-G2; both verify production output runtime-executes |
| BC→BD.C2 | BC.W5 | Sister crates (`egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex`) freeze with `cargo publish --dry-run` clean per `docs/tranches/BC/waves/W5.md:55-77`. BD publishes to crates.io in dep order; the BC.W5 endpoint reconciliation lands one canonical `bbnf-regex` path; BD operates against the published crate. The path triplet (`path`, `path-core`, `path-ts`) freezes per Lock 7; BD publishes the triplet coordinated (cargo-release computes order). | BD.W3 (publication wave) | BD.W3 closes BD-G3 + BD-G4; `cargo install <crate>` + `npm install @bbnf-lang/path-ts` succeed end-to-end |
| BC→BD.C3 | BC.W5 | Worktree fixture symlink contract closure: `xtask worktree-init` materialises `data/{json,css,bbnf,sheets}` + `grammar/<name>/rewrites/*.ron` for every grammar per `docs/tranches/BC/waves/W5.md:79-86`. BD extends: per-grammar `crates/bbnf-parse/tests/fixtures/<grammar>/` directories with ≥ 3 canonical inputs each; CI matrix runs Rust + TS + WASM per grammar. | BD.W4 (fixture fleet expansion); BD.W5 (cross-backend parity matrix) | BD.W4 closes BD-G6; BD.W5 closes BD-G5 |

## Carry-tags TO BE

**No successor. This is the close tranche.** Hereupon BD has no successor; the plan ends here.

| Tag | Owner wave | Description | Receiver |
|---|---|---|---|
| (none) | (none) | The 13-lock cross-reference is the closure attestation. Locks 1, 2, 3, 4, 6, 8, 9, 10, 12, 13 ratify in BA / BB / BC; locks 5, 7, 11 ratify here in BD. No carry escapes BD. | (none) |

The absence of carry-tags TO BE is the gate. BD-G7's verification: `rg -n "BD→B[A-Z]\." docs/tranches/BD/` returns zero matches.

## 13-Lock honoured

Every cell names the wave that addresses the lock; empty cells are faults. The Notes column flags weak adherence, prior-tranche closure, or final ratification.

| Lock | Wave | Notes |
|---|---|---|
| L1. Tape + columnar dead | (closed in BA.W0 + BA.W5 + BB.W1; ratified in BC.W6) | BD carries no L1 work; the substrate is rust-typed-enums, JS discriminated unions, WASM linear-memory structs — none are tape. |
| L2. Layout lowering canon | (closed in BA.W2; ratified in BC.W0 IR contract) | BD's TS + WASM emitters consume `Layout` / `LayoutSink` vocabulary; no retired terms. |
| L3. Cursor + byte-skip unified | (closed in BA.W4 + BB.W2) | BD's TS + WASM activations preserve the unified path; the eager fast path elides cursor calls in TS via per-rule emit decisions, in WASM via dead-code elimination. |
| L4. Per-domain orthogonal optimisation | (closed in BB.W3) | BD does not optimise; BD activates pre-optimised IR. |
| L5. IR + per-backend lower | W0 (TS proc-macro activation); W1 (TS runtime production); W2 (WASM pipeline production) | **Final ratification at BD close**: the typed IR supports Rust (BC.W1) + TS (BD.W1) + WASM (BD.W2) in production, not scaffold. |
| L6. xtask emits committed source | (closed in BC.W1; ratified in BD.W1 + BD.W2) | BD's TS + WASM source emit lands at xtask-controlled paths; no proc-macro façade. The exception is `path-ts` (a proc-macro shell per Lock 7); not a codegen substrate. |
| L7. `crates/path/` consolidation | W0 (path-ts activation); W3 (path triplet publication) | **Final ratification at BD close**: path-ts ships as `@bbnf-lang/path-ts` (NAPI-RS); path-core publishes to crates.io; path publishes to crates.io. The triplet is the published artefact. |
| L8. Surpass sonic-rs / simdjson / lightning-css | (closed in BC.W6 for native; bounded in BD for cross-FFI/WASM) | BD's gates are bounded by NAPI-RS marshalling + WASM-no-AVX realities; the native floor (BC) remains the SOTA-relative claim. BD-G1 + BD-G2 are engineering-bounded gates (cited research baseline), not naive SOTA-beat gates. |
| L9. Slice-borrow primary; bumpalo + owned escape hatches | (closed in BC.W4) | BD's TS path uses `Uint8Array` views over input bytes (zero-copy); BD's WASM path uses linear-memory pointer + length (zero-copy across FFI). |
| L10. Pratt + SIMD auto-detected | (closed in BB.W3) | BD does not introduce annotations; the WASM emitter respects the auto-detected Pratt + SIMD decisions, omitting SIMD scanners where wasm32 lacks AVX2. |
| L11. Path-deps for incubating sister crates | W3 (sister-crate publication) | **Final ratification at BD close**: `egraph`, `egraph-derive`, `csp-solver`, `bbnf-regex` graduate from path-deps to crates.io. `parse-that` disposition per BC.W5c gap-I (per `docs/tranches/BC/audit/W5-parse-that-disposition.md` if drafted by BC; default: permanent path-dep per surgery 33). |
| L12. ser + gorgeous archive BEFORE BA.W0 | (closed in pre-BA ceremony) | BD carries no L12 work. |
| L13. No god directories; cohesive encapsulation at every level | (closed in BC.W3 at the crate level; ratified in BD via per-grammar fixture pattern) | BD's `crates/bbnf-parse/tests/fixtures/<grammar>/` mirrors the per-grammar generated/* directory structure; the npm package layout (`@bbnf-lang/runtime-<platform>`) honours the same partition. |

## Risks + mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| BD.W0 TS proc-macro activation breaks the path-ts ABI in a way that breaks BC.W2's smoke test | Low | The smoke test is the same-wave consumer per Era V mitigation; if the activation breaks the ABI, the smoke test fails immediately; the ABI is documented at `docs/tranches/BD/audit/W0-ts-procmacro-spec.md` and pinned via `napi-derive` version. |
| BD.W1 TS runtime emitter misses BD-G1 because NAPI-RS marshalling overhead exceeds the ratio cap | Medium | The ratio cap (≤ 18× BC native floor) is bounded by NAPI-RS's documented per-call overhead (~100 ns × handful of calls per parse ≈ negligible vs the ms-scale parse); if BD-G1 misses, the root cause is parser logic, not marshalling; mitigation is samply-trace the NAPI binding boundary. |
| BD.W2 WASM compilation produces a bundle exceeding 250 KB gzipped (npm package size constraint) | Medium | wasm-opt + wasm-tools strip-debug + wasm-tools strip-custom remove debug + name sections; the BC.W2 scaffold-output baseline is ~80 KB; activated full-grammar emit is bounded by per-grammar generated LOC × ~3 (WASM expansion ratio); per-grammar bundle option (`@bbnf-lang/runtime-wasm-json`) ships JSON-only at ~120 KB. |
| BD.W3 sister-crate publication fails because cargo-semver-checks finds an unexpected break against BC.W5 freeze baseline | Low | The freeze precondition at BC.W5 includes `cargo publish --dry-run` clean per `docs/tranches/BC/waves/W5.md:114-118`; cargo-semver-checks validates the rustdoc-JSON-detectable surface; if a non-JSON-detectable break exists (field type, generic param, lifetime), BD.W3 amends the freeze docs and re-bumps the major version. |
| BD.W3 path-ts npm publication fails because the per-platform binary matrix is incomplete | Medium | The CI matrix at BD.W3 builds darwin-arm64, darwin-x64, linux-x64-gnu, linux-arm64-gnu, linux-x64-musl, win32-x64-msvc — the documented NAPI-RS Tier 1 platforms; tier 2+ platforms (FreeBSD, Android, RISC-V) defer to post-BD via consumer demand; the wave's gate cites the tier 1 set explicitly. |
| BD.W4 worktree fixture expansion conflicts with BC.W5's `xtask worktree-init` contract | Low | BD.W4 extends the contract, does not replace it; per-grammar `tests/fixtures/<grammar>/` is additive to BC.W5's `data/{json,css,bbnf,sheets}` symlinks; the xtask gains a `--fleet` flag to materialise both. |
| BD.W5 cross-backend parity fails on a CSS L4 fixture because lightningcss's color representation differs from BD's | Medium | The equivalence relation is "byte-equal canonical-JSON serialisation modulo float-repr" — any case where CSS L4's output structure intrinsically differs from lightningcss's output is the canonical bbnf representation (BC.W4 visitor surface defines the canon); the parity test is bbnf vs bbnf-across-backends, not bbnf vs lightningcss. |
| BD.W6 close discovers an empty 13-lock cell because a lock was assumed closed by a prior tranche but never explicitly named | Low | The verification at BD.W6 §2.6 walks the cross-reference table; every lock has at least one BD wave OR a prior-tranche closure note OR pre-BA ceremony note; if any lock is empty, BD.md amends to include the prior-tranche note (no fictional carries). |
| Bundle drift between TS + WASM emitters: TS production path inadvertently differs from WASM in a way the parity matrix doesn't catch | Medium | The per-grammar fixture matrix tests ≥ 3 inputs per grammar across both backends; the canonical-JSON serialisation is the boundary; cross-backend trait conformance from BC.W2 §2.6 holds across BD activation. |
| The `JSON.parse` baseline (BD-G1's hard floor) shifts if Node 22+ ships V8 engine improvements | Low | The floor is V8-version-dependent; BD-G1 cites Node 20.x explicitly; future Node versions are engineering-bounded by the same 18× BC-native ratio. The gate is platform-pinned. |

## Build/iter time gate

BD's iter-time profile: TS + WASM build paths add cross-language compile costs.

| Activity | Pre-BD wall | Post-BD wall | Notes |
|---|---:|---:|---|
| `cargo xtask regen --check` | ≤ 22 s (BC close) | ≤ 22 s | unchanged; BD does not regenerate Rust output |
| `cargo build -p path-ts --release` | n/a | ~45 s (cold) / ~5 s (incremental) | NAPI-RS cdylib compile; per-platform binary |
| `cargo build --target wasm32-unknown-unknown --release` | n/a | ~30 s (cold) / ~3 s (incremental) | wasm-bindgen compile |
| `wasm-pack build --target bundler --release` | n/a | ~50 s (full pipeline including wasm-opt) | one-time per wasm cdylib change |
| `npm install @bbnf-lang/runtime` (consumer-side) | n/a | ~3 s | NAPI-RS per-platform sub-package fetch |
| `cargo nextest run --workspace` | ~50 s (BC close) | ~65 s (BD close) | adds parity matrix tests |
| `npm test --workspaces` | n/a | ~12 s | TS + WASM tests on Node 20.x |
| Iter-loop: edit `crates/bbnf-codegen/src/ts/parse_fn.rs` + `cargo build -p bbnf-codegen --tests` | n/a | ~14 s | new iter-loop introduced |
| Iter-loop: edit `crates/bbnf-codegen/src/wasm/parse_fn.rs` + `cargo build --target wasm32-unknown-unknown` | n/a | ~10 s | new iter-loop introduced |

The BD close iter-time gate: `cargo xtask regen --check` ≤ 22 s preserved; `cargo build -p path-ts --release` ≤ 45 s cold; `wasm-pack build --target bundler --release` ≤ 50 s cold. These are the iter-loop targets for BD execution.

### Generated-LOC budget table (BC close → BD close)

BD does NOT modify the existing `crates/bbnf-parse/src/parse/generated/<g>.rs` Rust output. BD adds two new generated trees: `crates/bbnf-codegen/src/ts/generated/<g>.ts` (TS production output) and `crates/bbnf-codegen/src/wasm/generated/<g>.rs` (WASM Rust source compiling to WAT). The Rust generated tree is unchanged.

| Tree | BC close LOC | BD close LOC | Net delta | Notes |
|---|---:|---:|---:|---|
| `crates/bbnf-parse/src/parse/generated/` | ~135,167 | ~135,167 | 0% | unchanged; BD activates new backends, not new Rust emit |
| `crates/bbnf-codegen/src/ts/generated/` | 0 | ~280,000 | +new | TS source ~2× Rust LOC due to TS verbosity (function literals, type annotations); 9 grammars × 31K avg |
| `crates/bbnf-codegen/src/wasm/generated/` | 0 | ~135,000 | +new | WASM-target Rust compiles via wasm-bindgen; LOC roughly equals Rust generated |

The per-tree budgets are wave-level: BD.W1 closes the TS budget (each grammar ≤ 35K LOC); BD.W2 closes the WASM budget (each grammar ≤ 18K LOC). The gates are at `docs/tranches/BD/audit/W1-ts-emitter-spec.md` and `docs/tranches/BD/audit/W2-wasm-pipeline-spec.md`.

## Voice locks

§V1. Voice is archaic-permissive ("hereupon", "begotten", "thereof", "appurtenant", "extant"). Not corporate. Per `feedback_archaic_diction_is_voice`.

§V2. No metalanguage. Documents do NOT reference commits, conversation history, or the plan's draft history. Cite path:line. Per `feedback_no_metalanguage_docs`.

§V3. State the deliverable. State the gate. Move on.

§V4. Citations are path:line, not paraphrase. `audit/SOTA-2026-05-03.md:50-58` not "the SOTA cites sonic-rs".

§V5. Tables are liberal; markdown tables for every multi-row enumeration.

§V6. Per-X tables for every "all-X" claim. The 9-grammar parity matrix at BD.W5 is a table; the platform matrix at BD.W3 is a table; the lock-honoured cross-reference is a table.

## Wave-by-wave deliverable summary

| Wave | Primary deliverable | BD-G gates closed | Carry-tags consumed | Carry-tags produced |
|---|---|---|---|---|
| W0 | TS proc-macro shell activation: `path-ts` cdylib compiles + binds; runtime smoke test imports the published binary | partial BD-G4 (path-ts builds) | BC→BD.C1 (partial) | W0→W1, W0→W3 |
| W1 | TS runtime emitter activation: `@bbnf-lang/runtime` ships; full grammar emission for JSON + CSV cohort; host-fn resolution per-backend | BD-G1 | BC→BD.C1 (full); W0→W1 | W1→W3, W1→W5 |
| W2 | WASM compilation pipeline activation: `@bbnf-lang/runtime-wasm` ships; wasm-bindgen + wasm-pack dual-build (bundler + nodejs targets) | BD-G2 | BC→BD.C1 (full) | W2→W3, W2→W5 |
| W3 | Sister-crate publication: `egraph-derive`, `egraph`, `csp-solver`, `bbnf-regex` to crates.io; path triplet (path, path-core, path-ts) coordinated; cargo-release auto-computes order | BD-G3, BD-G4 | BC→BD.C2; W0→W3, W1→W3, W2→W3 | W3→W6 |
| W4 | Worktree fixture fleet-wide expansion: per-grammar `tests/fixtures/<g>/` directories; xtask `worktree-init --fleet` flag; CI matrix expansion | BD-G6 | BC→BD.C3 | W4→W5, W4→W6 |
| W5 | Cross-backend parity verification: 9 grammars × ≥ 3 fixtures × 3 backends = ≥ 81 cells; equivalence relation (canonical-JSON byte-equal modulo float-repr) holds | BD-G5 | W1→W5, W2→W5, W4→W5 | W5→W6 |
| W6 | BD close: PROGRESS / FINAL; carry ledger CLOSED; 13-lock cross-reference verified; all BD-G gates green; **no successor** | BD-G7 | All preceding waves | (none — terminal close) |

## SOTA anchors used in BD gates

| Anchor | Library | Dataset / Surface | Source |
|---|---|---|---|
| sonic-rs M1 Pro twitter parse 436 µs | sonic-rs (cloudwego) | twitter.json (parse-to-typed-struct) | `audit/SOTA-2026-05-03.md:50-58` |
| BC native target 380 µs | bbnf | twitter.json | `docs/tranches/BC/BC.md:15` |
| simdjson-wasm twitter ~870 µs - 1.45 ms | simdjson-wasm | twitter.json (WASM-bound) | `docs/tranches/BD/audit/research-anchors.md:§2` |
| V8 `JSON.parse` ~5 ms | V8 / Node 20.x | twitter.json (JSON.parse baseline) | engineering measurement; cited at BD-G1 |
| NAPI-RS marshal ~100 ns/call | NAPI-RS | per-FFI-transition cost | `docs/tranches/BD/audit/research-anchors.md:§1` |
| wasm-bindgen "lightweight" | wasm-bindgen | "Only pay for what you use" | `docs/tranches/BD/audit/research-anchors.md:§2` |
| sonic-rs `pointer!["a","b",1]` | sonic-rs | `src/pointer/` | `audit/SOTA-2026-05-03.md:33-42` |

Every BD perf gate names a competitor + dataset + platform per Lock 8. Engineering gates (publication, parity, fixtures) are non-throughput; they cite the cargo / npm / shell command that proves the artefact lands.

## Friction forecast for BD's exposed surfaces

Per Lane 7 (Friction-Forecast), BD's three new user-facing surfaces require named educational artefacts. Each forecast names: the API surface, the user mental model required, the point of greatest confusion, the verbatim error message the runtime should emit.

| Surface | User mental model | Point of confusion | Error message |
|---|---|---|---|
| `@bbnf-lang/runtime` install (BD.W1) | NAPI-RS native binary; per-platform sub-packages auto-resolve via `optionalDependencies`; the `darwin-arm64` machine pulls only `@bbnf-lang/runtime-darwin-arm64` | "why does my CI fail with `Cannot find module '@bbnf-lang/runtime-linux-x64-gnu'`?" — answer: the npm cache is mismatched against the OS / libc; rebuild the cache | `error: failed to load native binding for platform '<platform>-<arch>'; expected one of [<supported list>]; install with --include=optional or rebuild npm cache` |
| `@bbnf-lang/runtime-wasm` import (BD.W2) | wasm-bindgen ESM module; bundler-target supports webpack/rollup/esbuild; nodejs-target supports `require()` | "why does my Vite project show `Cannot find module './pkg/runtime_bg.wasm?init'`?" — answer: the bundler doesn't know the `.wasm` extension; configure Vite's `assetsInclude` | `error: WASM module fetch failed; ensure your bundler is configured to handle .wasm imports — see docs.bbnf-lang.org/wasm/bundler-config` |
| Cross-backend parity tooling (BD.W5) | the parity matrix runs the same input through Rust + TS + WASM; equivalence is byte-equal canonical-JSON modulo float-repr; failures cite the byte-offset of divergence | "why does my parity test fail at byte 12345 with `expected 1.5e10, got 15000000000`?" — answer: the float-repr equivalence check uses `f64::EPSILON * max(...)`; the test was using strict-equal | `error: parity divergence at byte <offset> in fixture <path>: <backend_a> emitted <value_a>; <backend_b> emitted <value_b>; epsilon ratio <ratio> exceeds 1e-15` |

## Closing posture

Hereupon BD closes the foundation arc. The BC scaffolds activate; the sister crates publish; the path triplet ships; the cross-backend parity matrix gates fleet-wide. The 13 architectural locks ratify at one of (BA, BB, BC, BD, pre-BA ceremony); BD specifically ratifies Locks 5, 7, and 11. The carry ledger CLOSES at BD close: there is no BE; there is no BF; there is no BG. Phase 5 hardening verifies the close.

The plan ends here. Hereupon BBNF ships.
