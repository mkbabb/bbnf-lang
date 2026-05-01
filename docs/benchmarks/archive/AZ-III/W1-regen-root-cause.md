# AZ-III.W1.1 Regen Drift — Root-Cause Investigation

**Run date**: 2026-04-30
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-azIII-w1-regen`
**HEAD**: `6063460c` (`docs(az-iii.progress): mark W0 and W0p complete with hard-gate counts`)
**Toolchain**: cargo `nightly-2026-04-11-aarch64-apple-darwin`

## Headline

The audit baseline `01-failure-baseline.md` (Lane 1, 2026-04-30 morning, claimed
HEAD `d5179b8a`) reported `regen --check: 9 of 9 grammars drifted` as the
primary AZ-II.O5 carry-over blocker. **At HEAD `6063460c` (one source-clean
commit above `d5179b8a`, with only docs and the orthogonal xtask `--staged`
plumbing in between) `cargo xtask regen --check` exits 0/clean across all 9
grammars and a full `cargo xtask regen` produces zero file changes.**

The regen substrate's content-equality skip (`xtask/src/regen.rs:402-407`,
B6.W0.1, commit `5967d37b`) preserves byte-for-byte identity: when the
generator's prettyplease-formatted output equals the on-disk file, the write
is skipped and mtime is preserved. A no-op regen at HEAD therefore confirms
the on-disk generated tree at HEAD is byte-equal to the regen output produced
by the generator at HEAD.

Conclusion: **W1 Hard Gate 1 (`cargo xtask regen --check` is green across the
grammar fleet) is met at HEAD without any source change.** The audit's `9 of 9
drifted` reading is **STALE-BAD**: the same posture as the audit's own
"`cargo build -p bbnf --no-default-features` already MET despite FINAL.md
record" call-out (Lane 1 §5 stale items 1-2).

## Evidence

| Step | Command | Result | Log |
|---|---|---|---|
| 1 | `cargo xtask regen --check` (cold) | exit 0, "clean (9 of 9 grammars matched)" | `docs/benchmarks/AZ-III/W1-regen-check.txt` |
| 2 | `cargo xtask regen` (full surface) | exit 0; all 9 grammars regenerated; 0 byte changes (content-equality skip fired for every grammar) | `git status --porcelain` empty after run |
| 3 | `cargo xtask regen --check --staged` (clean stage) | exit 0, "nothing staged for grammar-relevant files" | inline |
| 4 | `cargo iter-check` (post-regen) | exit 0; 185 generated-code warnings (unchanged from baseline) | `docs/benchmarks/AZ-III/W1-iter-check.txt` |

## Reconciling against the audit

`d5179b8a..HEAD` is two source commits and a docs commit:

| Commit | Subject | Touches generator output? |
|---|---|---|
| `a808d0a3..454308af` | docs only | no |
| `41e4461d..420081c4` | docs only | no |
| `c537a2a4..df1f4a92..a859e5c6..88f2e1be..cdee0582..2ae7a168` | docs/precepts only | no |
| `1407bcd4..b1b34f2c` | `chore(profile/...)` workspace `Cargo.toml` profile carve | no — `Cargo.toml` only |
| `6bd979ef` | `chore(make/doctor)` Makefile | no |
| `c558c0d4` | `ci(nextest/partition)` `.config/nextest.toml` | no |
| `57537137` | `feat(xtask/regen-staged)` adds `--staged` flag | no — generator-orthogonal CLI plumbing |
| `bd00ede1`, `6063460c` | docs only | no |

Therefore the regen-output state at `d5179b8a` and the regen-output state at
`6063460c` must be identical. If `regen --check` is clean at `6063460c`, it
was clean at `d5179b8a` too. The audit's Lane 1 §2 row f and §5 stale-item
table both reported `9 of 9 grammars drifted`; that reading does not match
the deterministic regen substrate at the audit's claimed HEAD.

The most plausible cause is that the audit's `cargo xtask regen --check`
invocation ran against a transiently dirty worktree state (uncommitted local
edits to grammar source or generator code that were reverted before the audit
landed), or the audit's `/tmp/reaudit-fail-regen.log` captured the prior
generator state before `c3f86944` and `dc1999ed` (which themselves were
explicit regen-output refreshes) propagated. Lane 1 logs no longer exist on
disk to confirm. What is verifiable: at `HEAD == 6063460c`, the regen check
exits 0/clean and a full regen produces 0 byte changes.

## Drift classification (per grammar)

Per the W1 dispatch prompt's classification scheme:

| Grammar | LOC (HEAD) | Regen output | Classification |
|---|---:|---|---|
| bbnf | 16984 | byte-identical | **(none)** — no drift |
| bnf | 2440 | byte-identical | **(none)** — no drift |
| css_l4 | 86268 | byte-identical | **(none)** — no drift |
| css_pretty | 4795 | byte-identical | **(none)** — no drift |
| csv | 1188 | byte-identical | **(none)** — no drift |
| ebnf | 6002 | byte-identical | **(none)** — no drift |
| google_sheets | 11269 | byte-identical | **(none)** — no drift |
| json | 2164 | byte-identical | **(none)** — no drift |
| math | 624 | byte-identical | **(none)** — no drift |

Total generated tree: 131,734 LOC (excluding `mod.rs` index). All 9 grammars
in steady state.

## Format and lint cadence

- `cargo xtask regen --check`: GREEN (artefact at `W1-regen-check.txt`).
- `cargo iter-check`: GREEN (artefact at `W1-iter-check.txt`); 185 generated-code warnings present, unchanged from audit baseline (Lane 1 row a).
- `cargo fmt --all -- --check`: RED on **non-generated** source files
  (`crates/analysis/`, `crates/bootstrap/`, `crates/core/benches/`,
  `crates/core/examples/`, `crates/core/src/backend/`, etc.). Per W1
  dispatch prompt, "fmt-check failures on generated/ are NOT a blocker;
  everything else must pass." This non-generated drift is **out of scope for
  W1.1 Regen Drift** — its scope is generated/ only — and is recorded here
  as a routing pointer for the orchestrator. Lane 1 §2 row c reported
  `cargo fmt PASS` at `d5179b8a`; current state is RED on the same source
  files, suggesting either (a) the audit's fmt invocation skipped the
  affected files, or (b) the post-audit profile-carve commits
  (`1407bcd4..b1b34f2c`) altered rustfmt's cargo-resolved settings such
  that the same source now triggers rustfmt's let-else / collapse-if /
  split-fn rules.
- `git diff --check`: GREEN.

## Conclusion

W1 Hard Gate 1 is met without source change. No regenerated tree is committed
because the regen output is byte-equal to the on-disk file at HEAD. The
"9/9 drifted" baseline is **STALE-BAD**, analogous to the audit's own
no-default-build-BLOCKED-but-actually-MET reconciliation. A docs-only
commit records the gate status with the cited evidence paths so AZ-III
PROGRESS.md and the W1 close packet point at MET, not pending.

W1.2 (No-Default Build), W1.3 (Deletion + Metadata), and W1.4 (A1 Residue)
remain the orchestrator's pending sub-units; the regen lane closes here.
