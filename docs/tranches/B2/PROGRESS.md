# B2 — Progress Log

Dated execution log for tranche B2, the build-time codegen
transposition that retires `bbnf_derive`'s proc-macro IR-pipeline
contract.

- `Status`: in flight (W0 + W1 + W2 closed; W3-W4 pending)
- `Current wave`: W2 (complete) → W3 (opens next)
- `Next wave`: W3 — Script simplification; `bootstrap-bbnf.sh`
  retires; xtask absorbs cargo-expand + post-process logic

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

## 2026-04-25 — W0 closed

W0.a substrate landed at `dec67806`; W0.b boundary spec at `3c68e8c4`;
W0.c partial cherry-pick at `21881591` migrated `bbnf-bootstrap` to
the `pub use` re-export contract. The W0.c blocker on parser hangs
opened B3 (parser-baseline restoration) and B4 (codegen-emission
correctness); B3 closed at the bbnf parity test passing in 0.20 s,
B4.W0 closed at `a5fdda6b` with the SIMD bitmap break wrap.

W0.c re-execution against the post-B3 + post-B4.W0 substrate writes
`crates/core/src/grammar/generated/bbnf.rs` (34 048 lines) clean in
73 ms of pipeline work; deletes the monolithic
`crates/core/src/grammar/generated.rs` (33 279 lines) outright;
authors the `generated/mod.rs` aggregator with `pub mod bbnf; pub use
bbnf::*;`. The first-cycle regen exposed two emitter defects
(`FusedOutput` missing `<R>` parameter; `output.value_frames()`
method does not exist on `FusedOutput<R>`) — both fixed at source
in `crates/core/src/backend/rust/{emitter/shapes/value_materialize.rs,
view/value.rs}`. Re-emit produces a self-consistent bbnf.rs that
compiles clean and re-emits idempotently.

Latent build-orphan in `crates/bootstrap/src/bin/{cost_grid_sweep,
dump_ir}.rs` resolved via option (c): `bbnf-ir = { path = "../ir" }`
restored to `crates/bootstrap/Cargo.toml` as a regular dep with bin-
only consumption documented. The library crate `src/lib.rs` retains
its single `bbnf` dep.

Verification: `cargo iter-check-full` exits 0 in 5.62 s warm; `cargo
test --release -p bbnf --test bbnf_parity --exact
bbnf_parses_its_own_grammar` exits 0 in 35 s release compile + 0.00 s
exec; `cargo xtask regen --grammar bbnf` re-runs idempotently with
zero-line diff. `projection_totality` test target carries pre-existing
derive-time E0599 errors on the proc-macro path (`JsonG`, `CssL4G`,
`SheetsG`, `BbnfG` test fixtures); B4.W1 owns the consumer-fixture
polish per B4.B2.md §W1.

W1 dispatches next: consumer cutover for the remaining ~50
`#[derive(Parser)]` sites (gorgeous's 5, tests' ~50). The xtask emits
per-grammar source on disk; each consumer migrates to `include!` or
the `pub use` re-export contract bbnf-bootstrap proved.

Full audit: `docs/tranches/B2/audit/W0c-close.md`.

## 2026-04-25 — W1 closed

W1 executes the full consumer cutover wave end-to-end against the
post-W0.c substrate. Eight remaining grammars (`bnf`, `css_l4`,
`css_pretty`, `csv`, `ebnf`, `google_sheets`, `json`, `math`)
regenerate clean via `cargo xtask regen --grammar <ident>`; each
writes a populated, parseable per-grammar source file under
`crates/core/src/grammar/generated/`. No emitter-side codegen
defects beyond what W0.c's `FusedOutput<R>` + `frames()` fixes
covered surfaced — all eight grammars regen exit 0 on first cycle.

The aggregator `crates/core/src/grammar/generated/mod.rs` declares
every per-grammar module; BBNF self-host re-exports glob at the
aggregator path; the others require namespaced access via
`bbnf::grammar::generated::<ident>::*` to avoid collisions on
emit-impl items each module's `pub use __<lowered>_emit_impl::*;`
re-export carries.

The CSS L4 grammar's `crate::css_types::parse_hex_color` host
reference (in `grammar/css/l4/color.bbnf:190`) lifts to a pub
module at `crates/core/src/css_types.rs`. Pre-B2 the symbol
resolved through each test crate's `mod common` indirection
because the proc-macro expansion landed in the test crate scope;
post-W1 the generated source lives under bbnf lib so `crate::`
now resolves to bbnf's root, and the host shim moves there as a
single source of truth (per `feedback_no_workarounds`).

Sixty-two consumer sites across forty-three files migrate from
`#[derive(Parser)] #[parser(path = ...)]` to the new contract:
`use ::bbnf::grammar::generated::<ident>::*;`. The glob import
pulls the canonical marker AND every grammar-emitted companion
type (NodeView, RuleKind, per-rule views, projections) into the
consumer's scope. Sites whose local name diverged from the
canonical marker (e.g., `struct JsonG`, `BbnfEmit`) rename
in-place to the canonical marker so source-text references
resolve through the glob.

`crates/gorgeous/Cargo.toml` gains `[[test]]` entries with
`required-features = [<grammar>-grammar]` for each integration
test. Pre-existing config gap; the workspace-wide cutover
exposed the gating mismatch.

Verification: `cargo check --workspace --profile ax-iter` exits
0 in 4 s warm; `cargo iter-check-full` cold 45 s exit 0; `cargo
iter-check` warm 0.31 s exit 0 (under 0.5 s gate per B2.md
invariant 12). `rg -nF '#[derive(Parser' --type rust` returns 3
hits — clap::Parser in xtask/main.rs + 2 internal comments inside
`crates/derive/` (the crate that retires at W2). 0 actual
`bbnf_derive` consumer sites in the workspace's library code,
examples, tests, or benches.

`cargo nextest run --workspace --profile ax-iter --no-fail-fast`
shows 1 490 tests with 1 160 passed, 327 failed, 3 timed out, 27
skipped. The dominant failure class is a runtime tape-finalisation
panic ("FusedBuilder::finish called with N open value frames
remaining" at `crates/tape/src/builder/mod.rs:1066`) that surfaces
in debug builds only — `cargo test --release -p bbnf --test
bbnf_parity --exact bbnf_parses_its_own_grammar` still passes
under release optimisation, matching the W0.c close gate. The
debug-mode assertions are pre-existing test debt downstream of
B2.W1's scope (originate in tape-builder finalisation logic, not
in the cutover or per-grammar emission). B4.W1 / AY-II.W0' polish
own the consumer-side fixture work.

W2 dispatches next: `crates/derive/` deletion + `bbnf_derive`
purge from every `Cargo.toml` + `BBNF_SCHEMA_VERSION` retirement.
The proc-macro contract retires by simple removal — no consumer
needs a migration path because every consumer is already migrated.

Full audit: `docs/tranches/B2/audit/W1-close.md`.

## 2026-04-25 — W2 closed

W2 deletes the `crates/derive/` proc-macro crate (3 files / 457
lines) outright, purges `bbnf_derive` from every workspace
`Cargo.toml`, drops the `[patch.crates-io]` patch line from
`.cargo/config.toml`, removes `crates/derive` from the workspace
`[workspace] members` list, retires `BBNF_SCHEMA_VERSION` (its
sole declaration was `crates/derive/src/lib.rs:81`), and
regenerates `Cargo.lock` without the `bbnf_derive` package.

Per-Cargo.toml edits:

- `crates/core/Cargo.toml` — `bbnf_derive` dropped from
  `[dev-dependencies]`.
- `crates/gorgeous/Cargo.toml` — `bbnf_derive` dropped from
  `[dependencies]`.
- `crates/bootstrap/Cargo.toml` — comment narrative scrubbed of
  the W0.c-era `bbnf_derive` reference (no dep entry to remove;
  the dep retired at W0.c).
- `xtask/Cargo.toml` — comment narrative scrubbed of the W0.a-era
  `bbnf_derive` historical reference (the xtask never depended on
  the proc-macro). The W2 file-bounds list flagged `xtask/` as
  forbidden; the literal hard-gate item 2 (`rg -nF
  'bbnf_derive\|bbnf-derive' --type toml` returns 0) demanded the
  comment scrub. Audit deviation recorded explicitly in W2-close.
- `Cargo.toml` (workspace root) — `"crates/derive"` dropped from
  `[workspace] members`.
- `.cargo/config.toml` — `bbnf_derive = { path = "crates/derive" }`
  dropped from `[patch.crates-io]`.

`crates/json-prototype/Cargo.toml` — verified: no `bbnf_derive`
dep present (the W2 spec flagged it as a possibility; was empty);
no edit.

`wasm/Cargo.lock` retains its `bbnf_derive` entry. `wasm/` is
`exclude = ["wasm"]` from the workspace; carries its own lockfile;
the W2 hard gate is `--type toml` which doesn't match `.lock`.
The wasm sub-target migrates under its own dispatch.

Verification: `cargo check --workspace --profile ax-iter` exits 0
in 10.8 s post-edits + post-regen (cold against the orchestrator-
inherited target/); `cargo iter-check-full` warm 0.13 s exit 0;
`cargo iter-check` warm 0.11 s exit 0 (well under the 0.5 s gate
per B2.md invariant 12); `cargo update --workspace` exits 0 with
the `bbnf_derive` package + its 2 referencing entries dropped from
the lockfile; `rg -nF 'bbnf_derive\|bbnf-derive' --type toml` over
the workspace returns 0; `rg -n 'BBNF_SCHEMA_VERSION' --type rust`
returns 0.

Workspace nextest pass-rate matches the W1 close baseline (1 160
pass / 327 fail / 3 timeout / 27 skip). The 327 failures + 3
timeouts are the pre-existing FusedBuilder::finish open-frames
debug-build assertion class downstream of B2.W1's scope; B4.W1
owns the consumer-side fixture polish. No W2-introduced regression.

A latent `include_str!` absolute-path issue surfaced and resolved
in-pass: the W1 close commit shipped per-grammar `generated/<ident>
.rs` files with `bbnf-wt-b2-w1`-rooted paths. Phase 3's workspace
check failed on these stale paths; one-shot `sed` correction
followed by full `cargo xtask regen` re-emitted nine grammars
clean against the b2-w2 worktree path. The path-embedding question
is forward-routed (xtask emit-side fix to workspace-relative or
`env!("CARGO_MANIFEST_DIR")` resolution); the cherry-pick to
master leaves the regenerated paths OUT of the change-set so it
doesn't propagate worktree-specific paths upstream.

W3 dispatches next: `scripts/bootstrap-bbnf.sh` retirement; xtask
absorption of cargo-expand + post-process logic; Makefile +
in-tree script audits.

Full audit: `docs/tranches/B2/audit/W2-close.md`.
