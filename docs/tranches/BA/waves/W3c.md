# BA.W3c — Runtime Path Relocation

**Thesis** (the legacy borrowed-alphabet duplication at `crates/core/src/runtime/path.rs` retires; the typed-path runtime at `crates/core/src/path/` is the canonical runtime executor; the four `runtime/<g>/parse_with.rs` legacy lowering passes do NOT delete in this wave — that is W4c's surgery, after the unified `parse_with` surface lands per surgery #9). **Closer-gate** (`rg -n 'use crate::runtime::path::' crates/core/src/` returns zero; `test ! -f crates/core/src/runtime/path.rs`).

## §1 — Deliverable

Hereupon `crates/core/src/runtime/path.rs` (163 LOC, the older `PathSegment<'a> { Field(&'a str), Index(usize) }` per `audit/CENSUS-2026-05-03.md:244`) deletes; consumers consume `crate::path::ir::{Path, PathSegment, IntoPathSegment}` from `crates/core/src/path/` (the typed-path runtime, 1300+ LOC per CENSUS:243). Per CENSUS:261 ("DELETE — replace with `use crate::path::ir::{Path, PathSegment, IntoPathSegment}`"), the runtime documents' path-query traits consume the typed alphabet directly.

Surgery #7 ("After BA.W3.M4 — move `crates/core/src/path/` into `crates/path/src/runtime/`. W3 close leaves `crates/core/src/path/` empty or deleted") is **rejected** per the BA.md §Wave summary BA.W3c row: `crates/core/src/path/` is the runtime-executor surface consumed by Rust-emitter generated parsers. Per `audit/MODULES-2026-05-03.md:1057-1059`, the `path/` directory in `crates/core/src/` houses the typed-path executor surface (`PathSegment`, `Path`, `TypedPath<G, T>`, `OwnedPathSegment`, `IntoPathSegment`, `PathError`, `PathSchema`, `PathCursor`, `PathExecutor`, `AscentStrategy`, `select_variant`); per `audit/MODULES-2026-05-03.md:1114`, the post-restart 9-directory layered re-org includes `path/` as one of the nine cohesive sibling concerns under `crates/core/src/`. Moving `crates/core/src/path/` to `crates/path/src/runtime/` would invert the dependency arrow: `crates/path/` (proc-macro) would house runtime executor types consumed by Rust generated parsers, which means `crates/core/` would `use crates/path/` for its runtime — but `crates/path/` is a proc-macro shell that emits TokenStreams, not a runtime crate. The runtime executor surface lives in core; the path crates consume it via path-dep.

The W3c disposition: the legacy `runtime/path.rs` (163 LOC) deletes; the typed-path runtime at `crates/core/src/path/` survives in place. The four `runtime/<g>/parse_with.rs` legacy lowering passes (~480 LOC) do NOT delete in this wave per surgery #9 — they delete in BA.W4c after the unified `parse_with` surface from BA.W4b replaces them. Per surgery #9, the deletion sequence is: W4b emits unified `parse_with` for all grammars; W4c deletes the four legacy lowering files; W3c only retires the borrowed-alphabet duplicate.

The Era V failure mode is mitigated because W3c's substrate retirement (`runtime/path.rs` deletion) has the same-wave consumer of every typed-alphabet importer that previously imported the legacy alphabet via `use crate::runtime::path::`. The typed `crate::path::ir::*` is the same-wave replacement.

## §2 — Milestones

> **M0 — Identify every `use crate::runtime::path::` consumer**
>
> *Surface*: every Rust file under `crates/core/src/` that imports `crate::runtime::path::*`.
> *Action*: Enumerate via `rg -n 'use crate::runtime::path::' crates/core/src/`. Per CENSUS:200, "the four parse_with files manually lower the typed alphabet down to the legacy alphabet"; the consumers are predictable.
> *Gate*: enumeration complete; consumer list documented in W3c sub-commit.
> *Exit-criteria*: `rg -n 'use crate::runtime::path::' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/ 2>&1 | wc -l | tr -d '\n'` produces a known count (≥ 4 from the parse_with files; possibly more from `document.rs` consumers).

> **M1 — Rewrite each consumer's import to typed alphabet**
>
> *Surface*: per the M0 enumeration; each consumer file's `use` statements.
> *Action*: For each consumer, rewrite `use crate::runtime::path::{Path, PathSegment, IntoPathSegment}` to `use crate::path::ir::{Path, PathSegment, IntoPathSegment}`. The runtime documents' path-query traits at `crates/core/src/runtime/<g>/document.rs` consume the typed alphabet directly. Per CENSUS:261 ("DELETE — replace with `use crate::path::ir::{Path, PathSegment, IntoPathSegment}`").
> *Gate*: every consumer's import rewrites; `cargo check -p bbnf` succeeds.
> *Exit-criteria*: `rg -n 'use crate::runtime::path::' /Users/mkbabb/Programming/bbnf-lang/crates/core/src/ 2>&1 | wc -l | tr -d '\n'` returns `0`; `cargo check -p bbnf 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

> **M2 — Delete `crates/core/src/runtime/path.rs`**
>
> *Surface*: `crates/core/src/runtime/path.rs` (163 LOC per `audit/CENSUS-2026-05-03.md:244` and `audit/MODULES-2026-05-03.md:926`).
> *Action*: Delete the file; remove `pub mod path;` from `crates/core/src/runtime/mod.rs`.
> *Gate*: file gone; module declaration gone.
> *Exit-criteria*: `test ! -f /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/path.rs && cargo check -p bbnf 2>&1 | rg -c 'error\[' | tr -d '\n'` returns `0`.

> **M3 — Verify `crates/core/src/path/` survives in place (NOT moved to `crates/path/src/runtime/`)**
>
> *Surface*: `crates/core/src/path/` directory.
> *Action*: Verify `crates/core/src/path/` is intact; `find crates/core/src/path -name '*.rs' | wc -l` returns ≥ 4 (cursor.rs, executor.rs, ir.rs, schema.rs, etc. per MODULES:679-685). Per the BA.md §Wave summary BA.W3c row, the runtime executor surface is core-internal; surgery #7's "move into `crates/path/src/runtime/`" is rejected.
> *Gate*: `crates/core/src/path/` exists with the typed-path runtime files; no relocation to `crates/path/`.
> *Exit-criteria*: `find /Users/mkbabb/Programming/bbnf-lang/crates/core/src/path -name '*.rs' 2>/dev/null | wc -l | tr -d '\n'` returns ≥ 4; `test ! -d /Users/mkbabb/Programming/bbnf-lang/crates/path/src/runtime`.

> **M4 — Inventory the four `runtime/<g>/parse_with.rs` files (deletion deferred to W4c)**
>
> *Surface*: `crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs`.
> *Action*: Verify the four files survive at W3c close (per surgery #9, they retire at W4c after W4b's unified `parse_with` lands). Document the deferral in the wave's commit body: "Per surgery #9, the four legacy `parse_with.rs` files retire at BA.W4c, AFTER BA.W4b's unified `parse_with` surface replaces them."
> *Gate*: the four files exist; W3c does NOT delete them.
> *Exit-criteria*: `for f in json bbnf css_l4 google_sheets; do test -f /Users/mkbabb/Programming/bbnf-lang/crates/core/src/runtime/$f/parse_with.rs || echo "MISS:$f"; done | wc -l | tr -d '\n'` returns `0`.

## §3 — Closer gate

```
rg -n 'use crate::runtime::path::' crates/core/src/   ; expect: 0 matches
test ! -f crates/core/src/runtime/path.rs              ; expect: pass
test -d crates/core/src/path                          ; expect: pass
test ! -d crates/path/src/runtime                     ; expect: pass (surgery #7 rejected)
for f in json bbnf css_l4 google_sheets; do
  test -f crates/core/src/runtime/$f/parse_with.rs
done                                                   ; expect: all pass (deletion at W4c)
cargo check -p bbnf                                    ; expect: 0 error[ lines
```

## §4 — Invariants

§I1. **Lock 7 runtime survival**. `crates/core/src/path/` is the typed-path runtime executor; surgery #7's relocation is rejected. The path-crate triplet (path, path-core, path-ts) is proc-macro / cdylib / shared-core; the runtime executor is core-internal.

§I2. **No backward compat** (per `feedback_no_backward_compat`). The legacy `LegacyPath`/`LegacySegment` types delete at M2; no transitional alias survives.

§I3. **System cohesion** (per `feedback_system_cohesion`). The runtime executor folds into the existing `crates/core/src/path/` directory; no orthogonal subsystem.

§I4. **Direct-to-struct approach** (per `feedback_direct_to_struct`). The runtime path types consume the typed alphabet directly; per `audit/CENSUS-2026-05-03.md:261`, "the runtime documents' path-query traits should consume the typed alphabet directly."

§I5. **Generated LOC unchanged** (per BA-G10 + surgery G06-2). W3c does NOT regen any grammar.

## §5 — Risks specific to this wave

| Risk | Likelihood | Detection | Mitigation |
|---|---|---|---|
| The four `runtime/<g>/parse_with.rs` files reference `crate::runtime::path::` after M2's deletion (the bridge files survive at W3c per surgery #9) | High | M1 enumeration; `cargo check -p bbnf` post-M2 | The bridge files DO reference the legacy alphabet today; after M2 deletion, the four files break their compile. The mitigation: M1 rewrites each `parse_with.rs` import from `use crate::runtime::path::` to `use crate::path::ir::` IN PLACE (the body still does the manual lowering, but the alphabet source is unified); the manual lowering body retires at W4c. |
| `crates/core/src/runtime/mod.rs` has `pub mod path;` declaration that fails compile after M2 | Low | M2 close: `cargo check -p bbnf` | M2 includes "remove `pub mod path;` from `crates/core/src/runtime/mod.rs`" in the action; the gate verifies |
| Some non-parse_with consumer imports the legacy alphabet (e.g. analysis, lsp) | Medium | M0 enumeration covers entire `crates/core/src/`; non-core crates are checked via `rg -n 'use bbnf::runtime::path::' crates/` | The non-core consumer list is enumerated at M0; each rewrites at M1 |
| `crates/core/src/path/` directory itself violates Lock 13 (god-directory) | Low | M3 verifies the count of `*.rs` files; if > 10 and mixing concerns, surface for split | Per `audit/MODULES-2026-05-03.md:679-685`, `crates/core/src/path/` has ~10 children (cursor.rs, executor.rs, ir.rs, schema.rs, type_check.rs, ascent.rs, variant_select.rs, etc.); each is a cohesive concern; not a god-directory |

## §6 — Cross-references

- **Closes part of Lock 7** (per BA.md §13-Lock honoured row L7): legacy `runtime/path.rs` retiral. The full Lock 7 close requires W3a (directory rename) + W3b (path-core extraction) + W3c (runtime path retiral); each is a separate sub-wave per directive §5.
- **Carry-tags produced**: none direct to BB; W3c's outputs are consumed by W4a/W4b (which the unified `parse_with` surface depends on).
- **Preceding wave**: BA.W3b (path-core extraction).
- **Following wave**: BA.W4a (cursor + byte-skip codegen-time elision).
- **Routed-carry**: per surgery #28, `bbnf-regex` endpoint reconciliation routes to BC.W5 (NOT BC.W4 — surgery #28 corrects the dangling carry).

## §7 — Iter-time check

| Cargo Command | Expected Duration | Pass-Rate Target | Notes |
|---|---|---|---|
| `cargo check --workspace --profile ax-iter` | ≤ 22 s | error count: 0 | Post-W3c runtime-path retiral gate |
| `rg -n 'use crate::runtime::path::' crates/core/src/` | < 1 s | 0 hits | Legacy borrowed-alphabet residue gate |
| `rg -n 'LegacyPath\|LegacySegment' crates/core/src/` | < 1 s | 0 hits OR confined to `parse_with.rs` (deletes at W4c) | Legacy alphabet purge gate |
| `cargo nextest run -p bbnf -E 'test(parse_with)' --profile ax-iter` | ≤ 15 s | 100% | parse_with test cohort (post-deletion gate; BA.W4 will introduce unified parse_with) |
| `cargo doc -p bbnf-path -p path-core -p bbnf-path-ts --no-deps` | ≤ 25 s | exit 0 | Public surface documentation gate; the three crate names visible in workspace docs |

## §8 — Verification artefacts

W3c produces no audit artefact directly; the closer-gate's filesystem checks are the verification surface.

## §9 — Audit lane forecast

Lane 02 sequencing: same-wave consumer (typed alphabet replaces legacy in-wave) verifies. Lane 03 cohesion: M2 deletion closes C03-1 partial (the parse_with bridges retire at W4c). Lane 06 budget: generated parser LOC unchanged.
