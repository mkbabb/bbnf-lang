# BA.W3b — Path-Core Extraction

**Thesis** (the proc-macro / cdylib mirror eliminates by extracting `compile`/`lex`/`lower`/`validate` into `crates/path-core/`; both `bbnf-path` and `bbnf-path-ts` path-dep on the new crate; ~500 LOC of mirrored logic collapses to a single body; the synthetic fixtures at `crates/path-ts/src/fixture.rs` (248 LOC) and `crates/path/src/registry.rs` (201 LOC) delete). **Closer-gate** (`crates/path-core/` exists; both `bbnf-path` and `bbnf-path-ts` path-dep on `path-core`; `wc -l crates/path/src/path_macro.rs` and `crates/path-ts/src/compile.rs` each ≤ 200 LOC).

## §1 — Deliverable

Hereupon `crates/path-core/` introduces as a non-proc-macro shared crate carrying the lex/lower/validate logic that mirrors between `crates/path/src/path_macro.rs` (639 LOC, post-W3a rename) and `crates/path-ts/src/compile.rs` (474 LOC, post-W3a rename) per `audit/CENSUS-2026-05-03.md:263`. The introduction eliminates ~500 LOC of mirrored code per CENSUS:263 ("Eliminates ~500 LOC of mirrored code"). The proc-macro's TokenStream-IO concerns and the cdylib's JS-binding concerns remain in their respective crates as thin adapter shells (≤ 200 LOC each); the merged body lives once in `path-core`.

The synthetic fixtures retire. `crates/path-ts/src/fixture.rs` (248 LOC synthetic per `audit/CENSUS-2026-05-03.md:148`) and `crates/path/src/registry.rs` (201 LOC fixture per CENSUS:147) delete; both crates' `lib.rs` consumes the production `<grammar>.registry.json` sidecar (per `audit/MODULES-2026-05-03.md:629`, the workspace already writes the sidecar). The grammar match arms at `crates/path/src/registry.rs:132-135` (hardcoded `match grammar { "json" => ..., "css_l4" => ..., "google_sheets" => ..., "bbnf" => ... }` per CENSUS:147) deletes; the registry lookup flows through the BA.W1 metadata table.

The `RegistryDescriptor` pattern (post-W1 metadata-driven schema) is the consumer of the deleted match arms; `path-core`'s `compile.rs` reads the workspace metadata's per-grammar `RegistryDescriptor` and constructs the typed-path AST without grammar-specific arms. Per surgery #15, the host-fn relocation pattern is reflected at `path-core` too: any host-fn paths flowing through `path-core` resolve via the metadata's `host_fns[].path` field.

The Era V failure mode is mitigated because W3b's substrate (`path-core`) has the same-wave consumer of both `bbnf-path` and `bbnf-path-ts`; both crates path-dep on `path-core` IN THE SAME WAVE; both crates' `cargo check` runs in-wave.

## §2 — Milestones

> **M0 — Create `crates/path-core/` skeleton**
>
> *Surface*: new `crates/path-core/` directory; new `crates/path-core/Cargo.toml`; new `crates/path-core/src/lib.rs`.
> *Action*: Create `crates/path-core/Cargo.toml` (no `proc-macro = true`, no `cdylib`; pure `rlib`). Create `crates/path-core/src/{lib.rs,compile.rs,lex.rs,lower.rs,validate.rs}` skeletons. Add `crates/path-core` to workspace `members` array in root `Cargo.toml`.
> *Gate*: `cargo check -p path-core` succeeds (empty crate compiles).
> *Exit-criteria*: `test -d /Users/mkbabb/Programming/bbnf-lang/crates/path-core && test -f /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/lib.rs && cargo check -p path-core 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

> **M1 — Move shared lex/lower/validate logic INTO `path-core`**
>
> *Surface*: `crates/path/src/path_macro.rs` (639 LOC post-W3a; per `audit/MODULES-2026-05-03.md:241`); `crates/path-ts/src/compile.rs` (474 LOC post-W3a per CENSUS:242).
> *Action*: Identify the mirrored logic between `path_macro.rs` and `compile.rs`. Move the lex/lower/validate functions to `path-core/src/{lex,lower,validate,compile}.rs`. Each function has one body in `path-core`; both crates' shells call into `path-core`. Per `audit/MODULES-2026-05-03.md:241` ("the current file mixes proc-macro IO concerns with grammar registry validation"), the W3b split refactors `path_macro.rs` into `path_macro/{lex,lower,validate,emit}.rs` (per the same module-row split recommendation), with the `lex`, `lower`, `validate` sub-modules then extracted INTO `path-core` as the shared body.
> *Gate*: the duplicate logic in `crates/path/src/path_macro.rs` and `crates/path-ts/src/compile.rs` reduces; `path-core/src/{lex,lower,validate,compile}.rs` carry the merged 500+ LOC body.
> *Exit-criteria*: `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/lex.rs /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/lower.rs /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/validate.rs /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/compile.rs | awk '$2!="total" {sum+=$1} END {print (sum >= 200)}' | tr -d '\n'` returns `1`.

> **M2 — `bbnf-path` and `bbnf-path-ts` path-dep on `path-core`**
>
> *Surface*: `crates/path/Cargo.toml` (post-W3a); `crates/path-ts/Cargo.toml` (post-W3a).
> *Action*: Add `path-core = { path = "../path-core" }` to both `[dependencies]`; rewrite the public surface of `path/src/path_macro.rs` and `path-ts/src/compile.rs` to call `path_core::compile`, `path_core::lex`, etc. The proc-macro and cdylib shells become thin TokenStream-IO and JS-binding adapters; the lex/lower/validate body lives once in `path-core`.
> *Gate*: `cargo check -p bbnf-path -p bbnf-path-ts -p path-core` succeeds; `path_macro.rs` and `path-ts/src/compile.rs` shrink to ≤ 200 LOC each.
> *Exit-criteria*: `cargo check -p bbnf-path -p bbnf-path-ts -p path-core 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`; `wc -l /Users/mkbabb/Programming/bbnf-lang/crates/path/src/path_macro.rs /Users/mkbabb/Programming/bbnf-lang/crates/path-ts/src/compile.rs | awk '$2!="total" && $1>200 {count++} END {print count+0}' | tr -d '\n'` returns `0`.

> **M3 — Delete `crates/path-ts/src/fixture.rs` (248 LOC) and `crates/path/src/registry.rs` (201 LOC) synthetic fixtures**
>
> *Surface*: `crates/path-ts/src/fixture.rs` (post-W3a per CENSUS:148); `crates/path/src/registry.rs` (post-W3a per CENSUS:147).
> *Action*: Delete both files; both crates' `lib.rs` consumes the production `<grammar>.registry.json` sidecar (per `audit/MODULES-2026-05-03.md:629`). The grammar match arms at `crates/path/src/registry.rs:132-135` (hardcoded match per CENSUS:147) delete; the registry lookup flows through the BA.W1 metadata table.
> *Gate*: both files gone; the macro and cdylib continue resolving registries via the production sidecar.
> *Exit-criteria*: `test ! -f /Users/mkbabb/Programming/bbnf-lang/crates/path-ts/src/fixture.rs && test ! -f /Users/mkbabb/Programming/bbnf-lang/crates/path/src/registry.rs && cargo nextest run -p bbnf-path -p bbnf-path-ts --profile ax-iter 2>&1 | rg 'test result: ok' | wc -l | tr -d '\n'` returns ≥ 1.

> **M4 — `path-core` consumes BA.W1 workspace metadata for `RegistryDescriptor`**
>
> *Surface*: `crates/path-core/src/compile.rs`; the BA.W1 metadata schema at `Cargo.toml` `[workspace.metadata.bbnf-strategy]`.
> *Action*: `path-core/src/compile.rs` reads the metadata via `cargo_metadata::MetadataCommand::new()`; constructs `RegistryDescriptor` per grammar; replaces hardcoded match arms (CENSUS:147) with metadata-driven lookup. Per the BA.md §Carry-tags row C5, "BB's CSP/e-graph/miner extensions reference grammars only via `&str` ident through workspace metadata"; W3b's path-core extension applies the same discipline.
> *Gate*: `crates/path-core/src/compile.rs` consumes metadata; no `match grammar { ... }` arms.
> *Exit-criteria*: `rg -nE 'match grammar\s*\{' /Users/mkbabb/Programming/bbnf-lang/crates/path-core/src/ 2>&1 | wc -l | tr -d '\n'` returns `0`.

## §3 — Closer gate

```
test -d crates/path-core                              ; expect: pass
test -f crates/path-core/src/lib.rs                   ; expect: pass
test -f crates/path-core/src/compile.rs               ; expect: pass
test ! -f crates/path-ts/src/fixture.rs               ; expect: pass
test ! -f crates/path/src/registry.rs                 ; expect: pass
wc -l crates/path/src/path_macro.rs                   ; expect: ≤ 200
wc -l crates/path-ts/src/compile.rs                   ; expect: ≤ 200
cargo check -p bbnf-path -p bbnf-path-ts -p path-core ; expect: 0 error[
cargo nextest run -p bbnf-path -p bbnf-path-ts -p path-core
                                                      ; expect: 100% pass
```

## §4 — Invariants

§I1. **Lock 7 path-core extraction**. The path-core crate is the deduplication mechanism per Lock 7 ("A `crates/path-core/` (non-proc-macro) crate may exist to share the path-AST + compile logic between the two proc-macro shells; if so, it is the only deduplication mechanism allowed").

§I2. **Lock 13** (no god directories). `crates/path-core/` has 4-6 children (`compile`, `lex`, `lower`, `validate`, `lib.rs`); each carries one cohesive concern; the directory has no >10 children mixing concerns.

§I3. **No backward compat** (per `feedback_no_backward_compat`). The synthetic fixtures delete at M3; no transitional alias survives.

§I4. **System cohesion** (per `feedback_system_cohesion`). `path-core` folds into the existing path-crate triplet topology; no orthogonal subsystem.

§I5. **Pluggable components** (per `feedback_pluggable_components`). The `RegistryDescriptor` pattern admits per-grammar registry resolution as a pluggable component; the registry itself is a pluggable surface.

§I6. **Generated LOC unchanged** (per BA-G10 + surgery G06-2). W3b does NOT regen any grammar; the registry-sidecar schema diff is recorded separately.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| `path-core` introduces a circular dep (`path-core` → `bbnf-ir` → ... → `path-core`) | Low | `cargo check --workspace` cycle detection | `path-core` depends only on `bbnf-ir` and stdlib; `bbnf-ir` does not depend on `path-core`; the dep arrow is one-directional |
| Synthetic fixture deletion breaks tests that depend on hardcoded grammar registry | Medium | M3 `cargo nextest run -p bbnf-path -p bbnf-path-ts` | Production `<grammar>.registry.json` sidecar (per MODULES:629) is the production replacement; if any test depends on the fixture-only ident, the test rewrites to use the production registry |
| `path-core` introduction breaks `pointer!` macro callers | Low | `cargo check --workspace` post-W3b | Per the BA.md §Risks row 3, `bbnf-path` and `bbnf-path-ts` continue exporting their public API; the rename on disk is a name change, not an API change |
| Mirror elimination is incomplete (some logic remains duplicated between `path_macro.rs` and `compile.rs`) | Medium | M2 LOC budget verification | If both shells are still > 200 LOC, the unmoved logic surfaces as a follow-up split; the M2 gate is the discipline |

## §6 — Cross-references

- **Closes part of Lock 7** (per BA.md §13-Lock honoured row L7): path-core extraction phase.
- **Carry-tags produced**: BA→BB.C4 (per BA.md): `path-core` crate exists; BB's `pointer!` macro reuses it without proc-macro/cdylib mirror.
- **Preceding wave**: BA.W3a (directory rename).
- **Following wave**: BA.W3c (legacy `runtime/path.rs` deletion).
- **Routed-carry**: none specific to W3b.

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check -p bbnf-path -p bbnf-path-ts -p path-core --profile ax-iter` | ≤ 14 s | error count: 0 | Post-W3b path-triplet check |
| `cargo nextest run -p bbnf-path -p bbnf-path-ts -p path-core --profile ax-iter` | ≤ 18 s | 100% | Path-triplet test cohort |
| `wc -l crates/path-core/src/compile.rs` | < 1 s | ≥ 200 | Path-core's merged body verification |
| `find crates -maxdepth 1 -type d -name 'path*' \| wc -l` | < 1 s | exactly 3 | Lock 7 surface-count gate |

## §8 — Verification artefacts

W3b's path-core extraction is recorded in the post-wave LOC budget table; no separate audit artefact.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (both crates path-dep on path-core in-wave) verifies. Lane 03 cohesion: M3 deletion gates close C03-7 (registry fixture). Lane 06 budget: registry-sidecar diff recorded; generated parser LOC unchanged.
