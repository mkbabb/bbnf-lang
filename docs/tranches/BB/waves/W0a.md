# BB.W0a — Sister-Crate Path-Deps + Workspace Metadata

**Thesis** Hereupon five sister crates emigrate to path-dep status; the workspace metadata records each canonical endpoint; rank.rs + tiering.rs do NOT exist at W0a close — they land in BB.W3c with same-wave consumer per the Era V abrogation. **Closer-gate** `cargo check --workspace --profile ax-iter` green; path-deps resolve; `Cargo.toml` declares no in-tree workspace-member duplicates; `test ! -f crates/ir/src/rewrites/rank.rs && test ! -f crates/ir/src/rewrites/tiering.rs`.

## §1 Deliverable

W0a sub-wave splits the prior monolithic W0 deliverable per BB02-1 of `audit/HARDENING-PLAN-2026-05-03-02-sequencing-discipline.md:39-41`. The path-dep emigration lives at W0a; the same-wave consumer (minimal optimiser smoke pass) lives at W0b. The two sub-waves close the substrate-first/consumer-later concern by structurally landing producer + consumer in the same wave.

Five crates emigrate from in-tree workspace members to path-dep'd incubators per Lock 11 (`docs/HARDENING-PLAN-PROMPT.md:54`):
- `crates/egraph/` (12 files, ~1,800 LOC per `audit/MODULES-2026-05-03.md:136-152`)
- `crates/egraph-derive/` (343 LOC per `audit/MODULES-2026-05-03.md:158`)
- `crates/csp-solver/` (532 LOC lib.rs per `audit/MODULES-2026-05-03.md:81`)
- `parse-that/rust/regex/` → `parse-that/rust/bbnf-regex/` (renamed per the endpoint reconciliation)
- `parse-that/` (root)

The renamed `parse-that/rust/bbnf-regex/` closes the L11 omission per `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md:20`: every emigrated crate is named in workspace metadata; parse-that is no longer silently omitted.

Per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-115`, **rank and tier rewrites are NOT created in W0a**. The verification at M4 explicitly checks the absence; the W0a commit body asserts the absence. The same-wave consumer rule for rank/tier lands at BB.W3c per the amendment at lines 118-127.

The estimated edit surface: `Cargo.toml` workspace `members` array shrinks by 3 entries (egraph, egraph-derive, csp-solver — already path-deps for parse-that crates); `[workspace.dependencies]` table adds path-dep entries; `[workspace.metadata.bbnf-incubators]` block lands; `[patch.crates-io]` table adds the corresponding patches. ~50 LOC of metadata.

## §2 Milestones

| ID | Surface | Action | Gate | Exit-criteria |
|---|---|---|---|---|
| M0 | Pre-W0a baseline | Capture pre-W0a generated-LOC baseline at `docs/tranches/BB/audit/W0a-generated-baseline.md` per surgery 21 + G06-4 of `audit/HARDENING-PLAN-2026-05-03-06-generated-code-budget.md:49`. The baseline is the input to all per-wave LOC delta computation in BB. | `find crates/core/src/grammar/generated -name '*.rs' -exec wc -l {} +` matches the BB.md table | Baseline lands; W1a/W1b/W1c/W2a/W3c/W4a/W5a/W5b compute deltas. |
| M1 | `Cargo.toml` workspace structural change | Strike `crates/egraph`, `crates/egraph-derive`, `crates/csp-solver` from workspace `members`; add path-dep entries under `[workspace.dependencies]` | `cargo metadata --format-version 1 \| jq '.workspace_members \| length'` decreases by 3 | The three names disappear from `workspace_members`; `cargo check --workspace` succeeds. |
| M2 | `parse-that/rust/regex/` rename | Rename to `parse-that/rust/bbnf-regex/`; update parse-that workspace `members`; verify path-dep root path matches per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:170-175` | `test -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex && test ! -d /Users/mkbabb/Programming/parse-that/rust/regex` | Endpoint reconciliation pre-flight passes. |
| M3 | Workspace metadata block | Add `[workspace.metadata.bbnf-incubators]` block listing each path-dep with `{ path, status = "incubating", api_freeze_target = "BC.W5" }` | `rg -n 'bbnf-incubators' Cargo.toml` returns exactly one block; `cargo metadata` parses cleanly | Metadata lands; downstream consumers (BB.W0b smoke pass; BB.W3c optimiser pipeline) read each sister crate by metadata key. |
| M4 | Substrate-audit guard for rank/tier | Verify no `crates/ir/src/rewrites/rank.rs` and no `crates/ir/src/rewrites/tiering.rs` exist; the W0a commit body asserts the absence per `audit/HARDENING-PLAN-SYNTHESIS-2026-05-03.md:107-115` | `test ! -f crates/ir/src/rewrites/rank.rs && test ! -f crates/ir/src/rewrites/tiering.rs` | Two files do not exist; W3c will create them with the consumer in the same wave. |
| M5 | `[patch.crates-io]` table | Add explicit patch entries for each emigrated sister crate; the patch routes `egraph = "..."` references from crates.io fallback to path-dep | `cargo tree -p bbnf-ir` shows path-dep'd egraph, not crates.io | Patch table works; downstream cargo invocations resolve cleanly. |
| M6 | Worktree fixture preflight | Verify `xtask worktree-init` materialises `data/{json,css,bbnf,sheets}` per BA.W0 contract; W0a does not extend (BC.W5 owns the fleet-wide closure per surgery 26 + D08-3 of `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:12`) | `xtask worktree-init --check` runs cleanly | BA-installed fixture symlink contract holds. |
| M7 | Sister-crate API surface inventory | Record pre-W0a public API surface for egraph, egraph-derive, csp-solver, bbnf-regex, parse-that at `docs/tranches/BB/audit/W0a-sister-api-surface.md` | `cargo doc -p egraph -p egraph-derive -p csp-solver` produces output that matches recorded surface | API stability through path-dep relocation; BC.W5 freeze gate has the baseline. |
| M8 | Rank/tier absence artefact | Land `docs/tranches/BB/audit/W0a-rank-tier-absence.md` recording the M4 verification + the W3c same-wave consumer rule reference | `test -f docs/tranches/BB/audit/W0a-rank-tier-absence.md` | Era V abrogation gate evidence lands. |

## §3 Closer gate

```sh
cargo check --workspace --profile ax-iter                                  # green
cargo metadata --format-version 1 | jq '.workspace_members | length'        # decreases by 3 from BA close
test ! -f crates/ir/src/rewrites/rank.rs                                   # absent
test ! -f crates/ir/src/rewrites/tiering.rs                                # absent
test -d /Users/mkbabb/Programming/parse-that/rust/bbnf-regex               # endpoint chosen
test ! -d /Users/mkbabb/Programming/parse-that/rust/regex                  # old endpoint gone
rg -n 'bbnf-incubators' Cargo.toml | wc -l                                 # exactly one block
cargo nextest run -p bbnf-ir -p bbnf -p bbnf-analysis --profile ax-iter    # 100% pass for BB-owned surfaces
test -f docs/tranches/BB/audit/W0a-rank-tier-absence.md                    # absence artefact lands
test -f docs/tranches/BB/audit/W0a-sister-api-surface.md                   # API surface artefact lands
```

All ten conditions must pass; any failure halts W0b dispatch.

## §4 Invariants

§I1. **Lock 11** — five sister crates emigrate to path-dep status; simd-scan + bootstrap + analysis + lsp stay workspace-internal.

§I2. **Lock 4 precondition** — each optimiser sub-system is a path-dep boundary, ready for BB.W0b's same-wave consumer pass.

§I3. **Lock 13** — sister-crate emigration removes ~3,000 LOC from the workspace cargo-check surface.

§I4. **Era V abrogation** — rank.rs + tiering.rs are NOT created in this wave; they land in W3c with same-wave consumer. M4 explicitly verifies absence.

§I5. **L11 omission close** — `parse-that` is named in `[workspace.metadata.bbnf-incubators]`, closing the L11 omission per `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md:20`.

## §5 Risks

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| Path-dep resolution fails because relative paths break | Medium | `cargo check --workspace` errors with "no matching package found" | Use `path = "../parse-that/rust/bbnf-regex"` resolved relative to bbnf-lang's root; verify with `cargo metadata`. |
| Rename of regex → bbnf-regex breaks parse-that workspace | Medium | `cd /Users/mkbabb/Programming/parse-that && cargo check` errors | Update parse-that's workspace `members` in the same atomic commit; verify both bbnf-lang AND parse-that compile. |
| The W0a commit body fails to assert the absence of rank.rs / tiering.rs | Low | substrate_audit fails at W3c | M4 exit-criteria is a literal `test ! -f` shell predicate; the commit body includes the M4 verification command output. |

## §6 Cross-references

- **BB-G gates this wave is on the path to closing**: BB-G10 (optimiser composition output-piped); BB-G11 (generated-LOC baseline).
- **Carry-tags this wave consumes**: BA→BB.C5 — grammar-agnostic `bbnf-ir` substrate.
- **Carry-tags this wave produces**: BB→BC.C4 — sister crates as path-deps, ready for BC.W5 freeze.
- **Preceding wave dependency**: BA.W6 — BA close verified.
- **Following wave consumer**: BB.W0b — minimal optimiser smoke pass through the W0a path-deps in the SAME wave (closes BB02-1).

## §7 Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target |
|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 18 s on M1 Pro | n/a |
| `cargo nextest run -p bbnf-ir --profile ax-iter` | ≤ 35 s | 100% |
| `cargo metadata --format-version 1` | ≤ 2 s | n/a |

## §8 Verification artefacts

| Artefact | Path | Purpose |
|---|---|---|
| `W0a-generated-baseline.md` | `docs/tranches/BB/audit/` | Pre-W0a per-grammar LOC; the baseline for BB-G11 |
| `W0a-sister-api-surface.md` | same | Pre-W0a public API of each emigrated crate |
| `W0a-incubator-metadata.md` | same | The `[workspace.metadata.bbnf-incubators]` snapshot |
| `W0a-rank-tier-absence.md` | same | The M4 verification evidence |
| `W0a-lightningcss-parse-only.md` | same | Per surgery 13 + S04-7: local M1 Pro lightningcss parse-only re-measurement of bootstrap.css + tailwind.css; closes the surface-mismatch fault |

## §9 Audit lane forecast

| Lane | Anticipated challenge | W0a response |
|---|---|---|
| Lane 1 | "BB.W0 substrate-forward skeletons" | M4 verifies absence; the substrate is structurally precluded |
| Lane 2 | "BB.W0 produces sister-crate emigration; consumer is BB.W3 — multi-wave gap" | BB.W0b is the same-wave consumer; closes BB02-1 |
| Lane 4 | "CSS SOTA surface not parse-only ratified" | M0 + verification artefact `W0a-lightningcss-parse-only.md` re-measures locally per surgery 13 |
| Lane 5 | "Per-grammar leaks in supposedly-generic crates?" | The path-dep relocation is grammar-agnostic; metadata enumerates incubators by crate name |
| Lane 6 | "W0a silent on generated-code impact" | M0 captures the baseline; W0a has no generated-code delta |
| Lane 8 | "Are BB→BC.C4 carries explicit?" | `W0a-incubator-metadata.md` records each incubator's BC.W5 freeze target |
