# B2 — Progress Log

Dated execution log for tranche B2, the build-time codegen
transposition that retires `bbnf_derive`'s proc-macro IR-pipeline
contract.

- `Status`: planned (waves W0→W4 sequenced per `B2.md` wave summary)
- `Current wave`: W0 (planned)
- `Next wave`: W1 (opens after W0 close)

---

## 2026-04-25 — Plan authored

B2 opens as a successor tranche to B1 with a new architectural thesis:
**code generation is build-time work that produces source on disk;
the proc-macro contract is for small, local transformations of user
code**. The 80-min cold rustc IR-pipeline expansion wall on every
`#[derive(Parser)]` consumer (a structural cost B1 surfaced but
could not fix within its prelude-annex scope) drops to zero post-B2
because the pipeline no longer runs inside rustc's expand phase.

Plan grounded in the 2026-04-25 deep audit (`docs/audit/2026-04-25-
deep-audit/AUDIT-{A,B,C,D}-*.md`):

- **AUDIT-A** identified two B1 regressions (rustflags warm regression,
  unresolvable bench-json alias) — both fixed pre-B2 at commit
  `81195656`.
- **AUDIT-B** found that AY-II.W0' close ceremony as written includes
  three theatrical steps (cycle-2 cache-cleared, fat-LTO 5-bench
  matrix, samply per primary grammar at W0' close) and one
  load-bearing step (cycle-1 regen + invariant verification).
  Compressed-honest W0' is ~15 min on the post-B2 substrate.
- **AUDIT-C** confirmed AZ-I.W0's prescribed mechanisms (derive-cache
  relocation + Watt) do NOT reduce the cold-miss expansion wall; both
  retire when B2 retires the proc-macro entirely.
- **AUDIT-D** recommended T3 (xtask + checked-in generation) over T1
  (build.rs codegen), T2 (pre-serialised IR), T4 (msgpack blob + thin
  proc-macro) on grounds of simplicity, idiomatic Rust fit, and
  elimination of the proc-macro contract entirely.

Authored in this initial state:

- `B2.md` — 14 invariants, 5-wave schedule, cross-tranche debt
  ledger, escape clause.
- `waves/W0.md` — xtask substrate + first per-grammar emission
  (BBNF self-host); 2 parallel + 1 closer.
- `waves/W1.md` — consumer cutover; 4 parallel + 1 closer; named
  delete-then-swap window.
- `waves/W2.md` — proc-macro retirement; `crates/derive/` deletes;
  `bbnf_derive` purges from every `Cargo.toml`; `BBNF_SCHEMA_VERSION`
  retires.
- `waves/W3.md` — script simplification; `bootstrap-bbnf.sh`
  retires; xtask absorbs cargo-expand + post-process; Makefile
  amendments.
- `waves/W4.md` — CI gate (`cargo xtask regen --check`) +
  pre-commit hook + FINAL.md + AY-II handoff refresh + AZ-I.W0
  amendment + REMAINING-TRAJECTORY + RISK-PERF-MATRIX revisions.
- `AGENT_DISPATCH.md` — sub-agent dispatch surface with explicit
  anti-patterns (no `ScheduleWakeup`, no `Monitor` for exit events,
  worktree target-symlink fix, single-cargo-per-target).
- `PROGRESS.md` — this file.

No execution wave has dispatched yet.

## Pre-B2 trivial fixes — 2026-04-25

Commit `81195656` (`infra(b1.followup): drop -Zthreads/-Zshare-generics
rustflags + rename json_monolithic_value bench (pre-B2)`) restored the
d7-baseline `iter-check` warm timing and resolved the `bench-json`
alias drift before B2 dispatches. AUDIT-A flagged both as regressions
shipped under B1 close that B2's measurements would otherwise have
to baseline against rather than against the d7 substrate.

The rustflags drop (`[build] rustflags`, `[target.aarch64-apple-darwin]
rustflags`, `[target.x86_64-unknown-linux-gnu] rustflags`) restores
warm `iter-check` to ≤ 0.5 s per B2.md invariant 12 — the substrate
B2 dispatches against. Linux's `link-arg=-fuse-ld=lld` retained.

The `json_monolithic_value` → `json_value` rename matches the file path
`benches/json/value.rs` and resolves the `bench-json` alias's
`--bench json_value` reference.

Two doc-comment references in `crates/core/tests/value_api_apples_to_
apples.rs` updated. Historical audit-doc references retained as factual
record.

## Forward-looking — what B2 changes for AY-II + AZ-I

Once B2 closes:

- **AY-II.W0' close ceremony**: dispatchable in ~15 min on the post-B2
  substrate (per AUDIT-B compressed-honest spec). Cycle-1 regen via
  `cargo xtask regen` (~5 min vs > 80 min via the deleted
  bootstrap-bbnf.sh path); cycle-2 idempotency defers to W4 (cache
  content-keying argument retires with the cache itself); fat-LTO
  bench matrix defers to W1 close (peer-parity context); samply
  defers to wave-specific (W1.c JSON, W2 CSS, W3 Sheets, W4.e BBNF).
- **AZ-I.W0**: derive-cache relocation + Watt items DROP (per
  AUDIT-C; B2 retired the proc-macro entirely; no IR pipeline lives
  inside rustc to cache or wrap). Classifier unification + IR audit
  items KEEP.
- **AZ-II tape deletion**: tractable under the post-B2 substrate;
  byte-equal reversal cycles cost seconds rather than hours.
- **BA / BB**: anchor on the post-B2 build-time codegen output
  (no proc-macro to plumb through).
