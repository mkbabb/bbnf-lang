# B1.W0.d — iter-check-full ceiling plan (triumvirate phase)

**Run**: 2026-04-24, worktree `b1-trium-plan` at `/Users/mkbabb/Programming/bbnf-wt-b1-trium-plan`, branch `b1-trium-plan`. Research input: `docs/tranches/B1/audit/W0d-iter-check-full-triumvirate-research.md` (master `eeca61e1`, 125 lines). The research attribution stands verified; this plan authors the edit set the redress agent executes.

## Plan summary

- Flip `crates/gorgeous/Cargo.toml` `default = [...]` → `default = []` and gate the `gorg` binary on `required-features = ["bin-full"]`. Existing feature names (`bbnf-grammar`, `json-grammar`, `css-grammar`, `ebnf-grammar`, `bnf-grammar`, `sheets-grammar`, `bin-full`) are already defined — only the `default` line flips. The `required-features` line is the new 1-line addition to `[[bin]]`.
- **Verified on this worktree**: `crates/gorgeous/src/builtin.rs` is imported **only** by `src/main.rs` (line 1: `mod builtin;`), not by `src/lib.rs`. With `required-features = ["bin-full"]` on `[[bin]]` the binary target never builds without all six grammars active, so `builtin.rs`'s unconditional grammar references are always satisfied. **Change 2 is a no-op** — this contradicts research §Lever-1 "Adding `required-features` to `[[bin]]` is a required companion edit" only in that the cfg guarding of `builtin.rs` is unnecessary; the companion `required-features` edit IS still needed. Net: Change 1 alone is load-bearing.
- Keep `.cargo/config.toml` alias surface as-is; redefine `iter-check-full`'s semantic contract in B1.md §Invariants (invariant 11) as close-ceremony-only, with an explicit ≤ 5 min cold ceiling post-Change-1-2.
- No Watt / derive-cache / cranelift / lld work this wave; research stance holds — routed out of B1 scope.

## File-level change set

### Change 1 — `crates/gorgeous/Cargo.toml`

**Target**: `/Users/mkbabb/Programming/bbnf-wt-b1-trium-plan/crates/gorgeous/Cargo.toml`.

**Current state (verified 2026-04-24)**:
- Line 9-11: `[[bin]] name = "gorg" path = "src/main.rs"` — no `required-features`.
- Line 22: `default = ["bbnf-grammar", "json-grammar", "css-grammar", "ebnf-grammar", "bnf-grammar", "sheets-grammar"]`.
- Lines 27-34: individual grammar features + `bin-full = [...]` aggregate already present (research Lever 1 prerequisites already in place).

**Diff sketch**:
```
 [[bin]]
 name = "gorg"
 path = "src/main.rs"
+required-features = ["bin-full"]
…
-default = ["bbnf-grammar", "json-grammar", "css-grammar", "ebnf-grammar", "bnf-grammar", "sheets-grammar"]
+default = []
```

**Invariant impact**: B1 invariant 11 (the iter-check-full ceiling becomes ≤ 5 min cold, not ≥ 12 min); AY-II §14-19 untouched (no tape / materializer / dispatcher / runtime edit). Workspace-level impact audited: `grep -rn "gorgeous = " crates/*/Cargo.toml` returns empty on `b1-trium-plan`, confirming no crate depends on `gorgeous` — the `default = []` flip cannot break any workspace member.

**Validation probe** (synchronous Bash on `b1-trium-redress` worktree, once):
1. `cargo check --profile ax-iter -p gorgeous --lib` (no `--features`): must compile only `lib.rs` + `PrinterConfig` surface; 6 `#[cfg(feature = "…")]`-gated modules drop out; expected ≤ ~5 s.
2. `cargo check --profile ax-iter -p gorgeous --lib --no-default-features --features json-grammar`: ≤ 150 s (AY-II W0p gate 7, Single-grammar baseline).
3. `cargo check --profile ax-iter -p gorgeous --bin gorg --features bin-full`: must build the binary + all 6 derive sites; confirms `bin-full` feature union still complete.

### Change 2 — `crates/gorgeous/src/builtin.rs` (NO-OP; verified 2026-04-24)

**Verification** (grep on `b1-trium-plan` worktree):
```
$ grep -n "mod builtin\|builtin" crates/gorgeous/src/lib.rs crates/gorgeous/src/main.rs
crates/gorgeous/src/main.rs:1:mod builtin;
crates/gorgeous/src/main.rs:154:        .or_else(|| input_path.as_deref().and_then(builtin::detect_language))
crates/gorgeous/src/main.rs:160:    match builtin::format_builtin(lang, &input, &config) {
```

`crates/gorgeous/src/lib.rs` does **not** `mod builtin;`. `builtin.rs` compiles **only as part of the `gorg` binary target**. With Change 1's new `required-features = ["bin-full"]` on `[[bin]]`, the binary target is only buildable when `bin-full` is active, which in turn activates every grammar feature — so `builtin.rs`'s unconditional `gorgeous::{json,css,ebnf,bnf,bbnf}::` references are always satisfied at the only compile site they ever appear on.

**Diff sketch**: none. No edit.

**Invariant impact**: none.

**Validation probe**: `cargo check --profile ax-iter -p gorgeous --bin gorg --features bin-full` compiles `builtin.rs` + all 6 grammar modules cleanly; `cargo check --profile ax-iter -p gorgeous --lib` does not see `builtin.rs` at all.

**Contradiction-to-research-note**: research §Lever 1 (line 82) reads "`crates/gorgeous/src/builtin.rs:18-22` unconditionally references `gorgeous::{json,css,ebnf,bnf,bbnf}` grammar modules. Flipping `default = []` alone would break the binary's check/build. Adding `required-features` to `[[bin]]` is a required companion edit." — the "required companion edit" portion holds; the implication that `builtin.rs` needs its own `#[cfg]` guarding does NOT. The binary's `required-features` alone composes all necessary feature activation.

### Change 3 — `.cargo/config.toml` alias semantics (no-op mechanically; doc-only clarification)

**Decision**: **Option (A) from dispatch.** Keep the alias surface unchanged. `iter-check-full` stays `check --profile ax-iter --workspace` and is re-classified as close-ceremony-only via B1.md invariant 11 (Change 4). Rationale:
- Post-Change-1 + Change-2, `iter-check-full` cold is ~3-5 min, within B1's "measured ceiling, not exit 0" contract.
- Adding a third alias (Option B) introduces plumbing with no wall-clock win and forces propagation through Makefile + CI.
- The alias block comments at `.cargo/config.toml:83-93` already contain the "routine vs close-gate" language; only the doc-anchor invariant in B1.md needs to sharpen.

**Diff**: none to `.cargo/config.toml`.

**Validation probe**: none.

### Change 4 — `docs/tranches/B1/B1.md` invariant 11 wording

**Target**: `/Users/mkbabb/Programming/bbnf-wt-b1-trium-plan/docs/tranches/B1/B1.md` lines 103-105.

**Current wording**:
> 11. `cargo iter-check-full` (the workspace close-gate) records a measured wall-clock ceiling in `docs/benchmarks/post-B1-W0-proof.txt`; the ceiling is an explicit number, not "exit 0".

**Proposed wording**:
> 11. `cargo iter-check-full` is the workspace **close-ceremony** gate, not a routine surface. Post-pin + d4 + d5 + d6 + d7 + the B1.W0.d `gorgeous` `default = []` flip, its measured cold-wall ceiling is **≤ 5 min** recorded as row `iter-check-full-cold-pinned` in `docs/benchmarks/post-B1-W0-proof.txt`; the number is explicit and any later run exceeding it re-opens W0.d. Routine iteration uses `cargo iter-check` (≤ 15 s cold on the excluded-crate surface per B0 measurement); `iter-check-full` is invoked at wave close, pre-dispatch proof, and CI, not in the dev loop.

**Invariant impact**: no cross-wave breakage; strengthens the contract from "explicit number" to "explicit number ≤ 5 min" with a named row.

**Validation probe**: inspection — the redress agent must, post-Change-1+2, measure `time cargo iter-check-full` cold (`rm -rf target/ax-iter/incremental`) and verify ≤ 5 min; append the row. If > 5 min, re-open W0.d and bisect.

### Change 5 — W0.d hard-gate measurement sequence (redress-agent execution recipe)

The redress agent executes, on branch `b1-trium-redress` rooted at master:

1. Land Change 1 (flip `default` + add `required-features`). Change 2 is a verified no-op — do NOT edit `builtin.rs`. Run light preflight: `cargo check --profile ax-iter -p gorgeous --lib` ≤ ~5 s (zero derive sites active under the new `default = []`).
2. Verify single-grammar gate still holds: `cargo check --profile ax-iter -p gorgeous --lib --no-default-features --features json-grammar` ≤ 150 s (AY-II W0p gate 7).
3. Verify binary still builds: `cargo check --profile ax-iter -p gorgeous --bin gorg --features bin-full`. Must compile all 6 derive sites; wall comparable to current gorgeous cold (~500 s) but OFF the workspace check path.
4. Run `rm -rf target/ax-iter/incremental && time cargo iter-check-full` cold. Expected ≤ 5 min. Append row `iter-check-full-cold-pinned` in `docs/benchmarks/post-B1-W0-proof.txt`. **Do not** `rm -rf target/.bbnf-cache/` (B1 invariant 12).
5. `ls target/rustc-ice-*.txt 2>/dev/null | wc -l` → 0 (the ICE-clean gate W1.a opens on).
6. `make ay-prime` on fresh cache → row `ay-prime-fresh` (cache count + wall).
7. Routine alias walls (`cargo iter-check`, `cargo iter-test-leaf`, `cargo iter-check-lsp`, `cargo iter-check-prettify`, `cargo iter-check-bootstrap`) → table in `docs/benchmarks/post-B1-W0-routine.txt`.
8. `cargo nextest run --workspace --cargo-profile ax-iter --no-run` + close profile dry-runs → rows `nextest-ax-iter-retry`, `nextest-close-retry`.
9. `scripts/test-tier.sh leaf --profile ax-iter` → row `scripts-test-tier-leaf`.
10. Land Change 4 (B1.md invariant 11 rewording) in the same commit or the next commit on `b1-trium-redress`.
11. Commit composes cleanly onto master via cherry-pick (file bounds disjoint from other B1.W0 waves).

## Expected wall-clock outcome

| Surface | Pre-plan (master `eeca61e1`) | Post-plan (Change 1+2) | Basis |
|---|---|---|---|
| `cargo iter-check-full` cold | **≥ 12 min** (research §Q8 meta-audit 04 floor; gorgeous ≥ 500 s + bootstrap ≥ 300 s serial) | **~3-5 min** (bootstrap ≥ 300 s dominant; gorgeous drops to ~5 s lib-only) | research Lever 1 table |
| `cargo iter-check` cold (routine) | ~15 s (B0 measurement, d7) | unchanged | gorgeous / bootstrap / analysis / lsp still excluded |
| `cargo check -p gorgeous --lib` cold (default) | ~500 s (6 derive sites) | ~5 s (zero derive sites) | research §Q1 P2 baseline for feature-light crates |
| `cargo check -p gorgeous --bin gorg --features bin-full` cold | — (implicit in `--lib` today) | ~500 s (all 6 derive sites, gated behind `required-features`) | identical to current `gorgeous --lib` with default features |

The **≤ 5 min cold** target for `iter-check-full` (invariant 11 Change 4) is achievable without additional levers; bbnf-bootstrap's ≥ 300 s wall remains the new critical-path ceiling, but it is a single-crate single-derive wall that routes to AZ-I.W0 (Watt / cache-relocation; explicitly out of B1 scope).

## Invariant impact

- **B1.md §Invariants 10**: unchanged (the four excludes and their fast-path aliases remain; the `iter-check-full` semantic shift does not touch the routine exclude list).
- **B1.md §Invariants 11**: strengthened per Change 4; ceiling becomes a named number (≤ 5 min) with an explicit row and re-open rule.
- **B1.md §Invariants 12**: honoured — no script in this plan touches `target/.bbnf-cache/`.
- **AY-II §14-19** (FusedBuilder, push_compound, materializer, STRUCTURAL_SCAN_POLICY, dead_code, Parsed::to_value): all untouched; this plan edits only `crates/gorgeous/Cargo.toml`, contingent `crates/gorgeous/src/builtin.rs`, and `docs/tranches/B1/B1.md`.
- **SPEC invariants**: untouched (no tape / backend / pipeline edit).

## Risks + rollback

1. **`gorg` binary install breakage.** Users invoking `cargo install gorgeous` on crates.io get `default = []` → binary refuses to build (no features). Mitigation: the binary's `required-features = ["bin-full"]` makes this an explicit cargo-side error (`error: target 'gorg' requires the features: bin-full`) rather than a silent failure; the error message tells the user to `cargo install gorgeous --features bin-full`. Document in `crates/gorgeous/README.md` in a follow-up doc-pass (route to W3, not W0.d). **Acceptable risk** — crates.io release of gorgeous is pre-1.0; no external consumer contract broken.
2. **Downstream workspace member broken.** Ruled out by `grep -rn "gorgeous = " crates/*/Cargo.toml` → empty; no crate depends on gorgeous on `b1-trium-plan`.
3. **`crates/gorgeous/tests/` regress.** Gate 8 of AY-II W0p fix plan (`cargo test -p gorgeous --tests` green on default features) becomes meaningless — default features are now empty. The redress agent MUST invoke `cargo test -p gorgeous --tests --features bin-full` (full-feature test surface). If gorgeous tests assume default features, add `[features] test = [...]` union or pass `--all-features` to the gate. **Validation**: probe `cargo test -p gorgeous --tests --no-run --features bin-full` during redress; adjust `crates/gorgeous/Cargo.toml` `[dev-dependencies]` / `[features]` as needed.
4. **Rollback.** If Change 1 breaks any gate above, revert `crates/gorgeous/Cargo.toml` to the pre-plan block (restore `default = [...]` line, delete the new `required-features` line); leave Change 4 (doc) in place or roll it back trivially. Recovery is a single-file revert; git reflog tracks the exact commit on `b1-trium-redress`.

## Redress-agent dispatch scope

**Allow-list** (file edits permitted):
- `crates/gorgeous/Cargo.toml` — Change 1.
- ~~`crates/gorgeous/src/builtin.rs`~~ — verified no-op; **do not touch**.
- `docs/tranches/B1/B1.md` — Change 4 (invariant 11 rewording).
- `docs/benchmarks/post-B1-W0-proof.txt` — append rows `iter-check-full-cold-pinned`, `ay-prime-fresh`, `nextest-ax-iter-retry`, `nextest-close-retry`, `scripts-test-tier-leaf`.
- `docs/benchmarks/post-B1-W0-routine.txt` — create/append routine alias table.

**Read-only** (analysis only):
- `crates/gorgeous/src/lib.rs`, `crates/gorgeous/src/main.rs`, `crates/gorgeous/tests/*`.
- `.cargo/config.toml`, `.config/nextest.toml`, `rust-toolchain.toml`.
- `docs/tranches/B1/waves/W0.md`, `docs/tranches/B1/audit/W0d-iter-check-full-triumvirate-research.md`, this plan.

**Forbidden**:
- Any edit to `crates/core/`, `crates/tape/`, `crates/derive/`, `crates/ir/`, `crates/analysis/`, `crates/bootstrap/` sources.
- Any alias edit in `.cargo/config.toml` (Change 3 is explicit no-op).
- Any Makefile edit (W0.d Makefile rewrite is the Makefile-draft copy, not this plan's scope).
- Deletion / creation of `target/.bbnf-cache/` (invariant 12).

**Hard cap**: 30 min. At 27 min (0.9N) commit, at 30 min halt.

**Cargo budget**: the 10 measurement probes in §Change 5 are the full envelope. No speculative additional `cargo check` or `cargo test` invocations. Single-cargo-per-target discipline holds throughout.
