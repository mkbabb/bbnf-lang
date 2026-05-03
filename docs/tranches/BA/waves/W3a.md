# BA.W3a — Path Crate Directory Rename

**Thesis** (the path-crate triplet's directory canon — `crates/path/`, `crates/path-ts/`, with `crates/path-core/` to follow at W3b — lands first; package names retained as `bbnf-path` and `bbnf-path-ts` in Cargo.toml; the rename on disk is a directory change, not a package-name change). **Closer-gate** (`crates/path/` and `crates/path-ts/` exist; `crates/bbnf-path*/` do not exist; `cargo check -p bbnf-path -p bbnf-path-ts` succeeds).

## §1 — Deliverable

Hereupon `crates/bbnf-path/` and `crates/bbnf-path-ts/` rename on disk per surgery #6 ("Rewrite to `crates/path`, `crates/path-core`, `crates/path-ts`; move `crates/core/src/path/` runtime into `crates/path/`"). The package names in `Cargo.toml` remain `bbnf-path` and `bbnf-path-ts` (per the BA.md §Wave summary BA.W3a row "Package names retained"); the directory rename is the canonical Lock 7 form, the package name is downstream cosmetic. Every `cargo -p` gate cites the package name (`-p bbnf-path`, `-p bbnf-path-ts`) accordingly; the workspace `members = [...]` array updates to point at `crates/path` and `crates/path-ts` directories.

The renames are mechanical: `git mv crates/bbnf-path crates/path`; `git mv crates/bbnf-path-ts crates/path-ts`. The rename preserves git-blame (per `git mv` history-tracking). The internal `Cargo.toml` files retain `name = "bbnf-path"` and `name = "bbnf-path-ts"`; only the parent directory name changes. Consumer imports `use bbnf_path::*` continue resolving (the package name maps via `crate-renamings` if needed; the default Cargo behaviour resolves `bbnf-path` regardless of directory name).

The Era V failure mode is mitigated because W3a's substrate (the renamed directories) has the same-wave consumer of `cargo check --workspace`; the rename is verified in-wave.

## §2 — Milestones

> **M0 — Workspace `members` array update**
>
> *Surface*: root `Cargo.toml`'s `[workspace] members = [...]` array (per `Cargo.toml:1-2`).
> *Action*: Replace `"crates/bbnf-path", "crates/bbnf-path-ts"` with `"crates/path", "crates/path-ts"` in the members array.
> *Gate*: `cargo metadata --format-version 1 | jq '.packages[] | .manifest_path' | rg 'crates/(bbnf-path)/'` returns 0 (the bbnf-path subdirectory is gone from members) AND `rg 'crates/(path|path-ts)/Cargo.toml'` returns ≥ 2.
> *Exit-criteria*: `cargo metadata --format-version 1 2>&1 | rg -c 'crates/bbnf-path/Cargo.toml' | tr -d '\n'` returns `0`; `cargo metadata --format-version 1 2>&1 | rg -c 'crates/path/Cargo.toml' | tr -d '\n'` returns `1`.

> **M1 — `git mv crates/bbnf-path crates/path`**
>
> *Surface*: `crates/bbnf-path/` directory (918 LOC per `audit/CENSUS-2026-05-03.md:241`).
> *Action*: `git mv crates/bbnf-path crates/path`. The internal `crates/path/Cargo.toml` retains `name = "bbnf-path"` per the BA.md §Wave summary row.
> *Gate*: `crates/bbnf-path/` does not exist; `crates/path/` exists with the same file tree.
> *Exit-criteria*: `test ! -d /Users/mkbabb/Programming/bbnf-lang/crates/bbnf-path && test -d /Users/mkbabb/Programming/bbnf-lang/crates/path && test -f /Users/mkbabb/Programming/bbnf-lang/crates/path/Cargo.toml`.

> **M2 — `git mv crates/bbnf-path-ts crates/path-ts`**
>
> *Surface*: `crates/bbnf-path-ts/` directory (1,012 LOC per `audit/CENSUS-2026-05-03.md:242`).
> *Action*: `git mv crates/bbnf-path-ts crates/path-ts`.
> *Gate*: `crates/bbnf-path-ts/` does not exist; `crates/path-ts/` exists.
> *Exit-criteria*: `test ! -d /Users/mkbabb/Programming/bbnf-lang/crates/bbnf-path-ts && test -d /Users/mkbabb/Programming/bbnf-lang/crates/path-ts`.

> **M3 — `cargo check -p bbnf-path -p bbnf-path-ts` workspace integrity**
>
> *Surface*: workspace post-rename.
> *Action*: Run `cargo check -p bbnf-path -p bbnf-path-ts`; verify both compile under their package names against the new directory paths.
> *Gate*: cargo check exits 0; no consumer's `use bbnf_path::*` import breaks.
> *Exit-criteria*: `cargo check -p bbnf-path -p bbnf-path-ts 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

## §3 — Closer gate

```
test ! -d crates/bbnf-path                            ; expect: pass
test ! -d crates/bbnf-path-ts                         ; expect: pass
test -d crates/path                                   ; expect: pass
test -d crates/path-ts                                ; expect: pass
cargo metadata --format-version 1 | jq '.packages[] | select(.name=="bbnf-path") | .manifest_path' | rg 'crates/path/Cargo.toml'
                                                      ; expect: ≥ 1
cargo check -p bbnf-path -p bbnf-path-ts              ; expect: 0 error[ lines
```

## §4 — Invariants

§I1. **Lock 7 directory canon**. Three crate directories — `path`, `path-core` (post-W3b), `path-ts`. The package names retain `bbnf-path` and `bbnf-path-ts` for downstream consumer stability; per directive surgery #6 ("State whether the package name remains `bbnf-path` or is renamed; update every `cargo -p` gate accordingly"), the choice is explicit: package names retained.

§I2. **No backward compat** (per `feedback_no_backward_compat`). Despite retaining package names, the rename is full migration on disk; no transitional symlink survives.

§I3. **System cohesion** (per `feedback_system_cohesion`). The directory rename is mechanical; no orthogonal subsystem; KISS.

§I4. **Generated LOC unchanged** (per BA-G10 + surgery G06-2). W3a does NOT regen any grammar; generated parser LOC is unchanged from W2 close.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| `git mv` does not preserve history for some files (large rename surface) | Low | `git log --follow crates/path/src/path_macro.rs` shows pre-rename history | `git mv` preserves automatically; if not, the audit's pre-rename SHA is recoverable |
| Workspace `members` array update collides with the `Cargo.lock` resolver | Low | `cargo check --workspace` post-update | The package name unchanged; resolver re-resolves on `cargo check` |
| `cargo -p bbnf-path` invocation breaks because `bbnf-path` is no longer a directory name | Low | M3 cargo check | Per §I1, the package name is preserved in `Cargo.toml`; cargo's `-p` flag operates on package name, not directory; the gate verifies |
| Consumer imports `use bbnf_path::*` break post-rename | Low | M3 + downstream `cargo check` | Package name unchanged; imports resolve |

## §6 — Cross-references

- **Closes part of Lock 7** (per BA.md §13-Lock honoured row L7): directory rename phase. W3b extracts `path-core`; W3c retires `runtime/path.rs`.
- **Carry-tags produced**: none direct to BB; W3a's outputs are consumed by W3b (which path-deps on the renamed crates) and W3c (which retires the legacy alphabet).
- **Preceding wave**: BA.W2 (god-module splits; the renamed `Layout`/`LayoutSink` types are referenced in `crates/path-core/src/lower.rs` post-W3b).
- **Following wave**: BA.W3b (path-core extraction).
- **Routed-carry**: none specific to W3a; the `bbnf-regex` endpoint reconciliation routes to BC.W5 per surgery #28.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 22 s | error count: 0 | Post-W3a directory rename gate |
| `cargo check -p bbnf-path -p bbnf-path-ts --profile ax-iter` | ≤ 12 s | error count: 0 | Per-package check post-rename |
| `find crates -maxdepth 1 -type d -name 'path*' -o -name 'bbnf-path*' \| wc -l` | < 1 s | exactly 2 | (path, path-ts; path-core arrives at W3b) |

## §8 — Verification artefacts

W3a produces no audit artefact directly; the closer-gate's filesystem checks are the verification surface.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (cargo check) verifies. Lane 06 generated-code budget: unchanged at W3a. Lane 07 friction forecast: `docs/migration/bc-core-split.md` is BC.W3-owned (per surgery #34); W3a's directory rename does not touch the migration doc.
