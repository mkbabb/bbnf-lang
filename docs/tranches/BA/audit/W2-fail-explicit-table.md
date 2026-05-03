# BA.W2 Fail-Explicit Table — Surgery #18 Surface

Date: 2026-05-03
Source: `audit/CENSUS-2026-05-03.md:571-581` (the fail-explicit row from the kill-list).
Surgery: surgery #18 ("Add BA.W2.M4 fail-explicit table from `audit/CENSUS-2026-05-03.md:571-581`; every fallback / asymmetry / shim row has a grep / test gate. (No row may close by `investigate later`.)").

## §1 — Per-row gate table

Each row carries: site (path:line), violation type, FATE (DELETE / FAIL-EXPLICIT / RESOLVE), grep gate (verifies the residue retired), test gate (verifies behaviour preserved or regression caught).

| Site | Violation | FATE | Grep gate | Test gate |
|---|---|---|---|---|
| `crates/core/src/lower/value_expr/simple_kinds.rs:185` | "Defensive fallback — descend through any value-layer" | FAIL-EXPLICIT | `rg -n 'Defensive fallback' crates/core/src/lower/value_expr/simple_kinds.rs` returns 0 | `cargo nextest run -p bbnf -E 'test(value_expr) + test(lower)'` 100% pass |
| `crates/core/src/backend/rust/emitter/shapes/unordered.rs:288` | "Defensive fallback: a malformed Unordered rule" | FAIL-EXPLICIT (`unreachable!()` on the malformed branch) | `rg -n 'Defensive fallback' crates/core/src/backend/rust/emitter/shapes/unordered.rs` returns 0 | `cargo nextest run -p bbnf -E 'test(unordered)'` 100% pass |
| `crates/gorgeous/src/vm.rs:217` | "shouldn't happen in practice" | (gorgeous archived pre-BA per Lock 12; row is informational only) | `test ! -d crates/gorgeous` (per pre-BA ceremony) | n/a (crate archived) |
| `crates/core/src/runtime/google_sheets/arena.rs:38, 40, 103, 153` | repeated "fallback" arena arms | FAIL-EXPLICIT (each arm proven reachable or `unreachable!()`) | `rg -n 'fallback' crates/core/src/runtime/google_sheets/arena.rs` returns 0 | `cargo nextest run -p bbnf -E 'test(google_sheets)'` 100% pass |
| `crates/core/src/runtime/css_l4/builder.rs:713` | "without a parsed unit fall through to unitless" | FAIL-EXPLICIT (per W2.M3 split — the CSS L4 builder's silent unitless fallback becomes `Err(ParseErr::Unit)`) | `rg -n 'fall through to unitless' crates/core/src/runtime/css_l4/` returns 0 | `cargo nextest run -p bbnf -E 'test(css_l4)'` 100% pass |
| `crates/core/src/grammar/host.rs:387` | wildcard `@debug` strip-prefix swallows unrecognised keywords | FAIL-EXPLICIT (the wildcard arm panics on unknown keyword) | `rg -n 'strip_prefix\("@debug"\)' crates/core/src/grammar/host.rs` returns 0 OR confirms explicit-error site | `cargo nextest run -p bbnf -E 'test(host) + test(directives)'` 100% pass |
| `crates/core/src/grammar/generated/mod.rs:35` | BBNF aggregator asymmetry (`pub use bbnf::*`) | DELETE per surgery #19 | `rg -n 'pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs` returns 0 | `cargo check --workspace` 0 errors after the deletion + namespaced consumer rewrites |
| `crates/core/src/backend/emitter.rs:96, 125, 332, 469` | `_fallback: ...` underscored unused params | DELETE (the underscore prefix admits removal) | `rg -nE '_fallback:' crates/core/src/backend/emitter.rs` returns 0 | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/rust/emitter/grammar.rs:4` | "`emit_rule_function_impl` is retained as an empty shim" | DELETE | `rg -n 'empty shim' crates/core/src/backend/rust/emitter/grammar.rs` returns 0; `rg -n 'emit_rule_function_impl' crates/core/src/backend/` returns 0 outside generated | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:281` | "Ref for now — sub-rule's emitter carries" | RESOLVE (the "for now" is a TODO; the resolution is the W2.M4 keyword/struct_direct split) | `rg -n 'for now' crates/core/src/backend/rust/emitter/shapes/keyword/` returns 0 | `cargo nextest run -p bbnf -E 'test(keyword)'` 100% pass |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/cross_shape.rs:118` | "legacy Alt-dispatch body (pre-W4 pattern preserved)" | DELETE legacy body | `rg -n 'pre-W4 pattern' crates/core/src/backend/` returns 0 | `cargo nextest run -p bbnf -E 'test(alt_dispatch)'` 100% pass |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs:85` | "the legacy `push_leaf_with_unit()` for callers that have not" | DELETE legacy callers + the wrapper | `rg -n 'push_leaf_with_unit' crates/core/src/backend/` returns 0 | `cargo nextest run -p bbnf -E 'test(keyword)'` 100% pass |
| `crates/core/src/backend/rust/emitter/shapes/array/mod.rs:35` | "legacy record stream fallback is selected by this module" | RESOLVE per W2.M4 array/mod split | `rg -n 'legacy record stream' crates/core/src/backend/` returns 0 | `cargo nextest run -p bbnf -E 'test(array)'` 100% pass |
| `crates/core/src/backend/rust/emitter/shapes/substrate.rs:70-73` | "JsonStructBuilder fallback emitted code" narrative (KEEP per CENSUS:207) | KEEP (anti-fallback narrative) | (informational only; no gate) | n/a |
| `crates/core/src/backend/rust/emitter/grammar.rs:112` | "do not emit the legacy" narrative (KEEP per CENSUS:209) | KEEP | (informational only; no gate) | n/a |
| `crates/core/src/runtime/google_sheets/value.rs:76, 92` | TODO notes carrying borrowed-span work | RESOLVE (close TODO or excise) | `rg -n 'TODO' crates/core/src/runtime/google_sheets/value.rs` returns 0 (or 1 with explicit BB receiver) | `cargo nextest run -p bbnf -E 'test(google_sheets)'` 100% pass |
| `crates/core/src/runtime/css_l4/builder.rs:992` | "Wrap-frame fall-through is structural" (KEEP per CENSUS:192) | KEEP | (informational only; no gate) | n/a |
| `crates/core/src/runtime/bbnf/arena.rs:220` | "(legacy emission paths or non-BBNF" disjunct | DELETE the legacy disjunct | `rg -n 'legacy emission paths' crates/core/src/runtime/bbnf/arena.rs` returns 0 | `cargo nextest run -p bbnf -E 'test(bbnf)'` 100% pass |
| `crates/core/src/runtime/bbnf/view.rs:206` | similar legacy disjunct | DELETE | `rg -n 'legacy emission paths' crates/core/src/runtime/bbnf/view.rs` returns 0 | `cargo nextest run -p bbnf -E 'test(bbnf)'` 100% pass |
| `crates/core/src/backend/kernels/prefix_class.rs:21-23, 42` | "legacy `emit_call` wrapper" doc residue | DELETE doc-comment narrative | `rg -n 'legacy `emit_call`' crates/core/src/backend/kernels/` returns 0 | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/kernels/charclass.rs:32` | "legacy `emit_call` wrapper" doc residue | DELETE | `rg -n 'legacy `emit_call`' crates/core/src/backend/kernels/charclass.rs` returns 0 | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/types/mod.rs:5-7` | "legacy directory alive for one file" meta-language | DELETE meta-language | `rg -n 'legacy directory' crates/core/src/backend/types/mod.rs` returns 0 | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/mod.rs:4-8` | "Every other file was a re-export shim" meta-language | DELETE | `rg -n 're-export shim' crates/core/src/backend/mod.rs` returns 0 | `cargo check --workspace` 0 errors |
| `crates/core/src/backend/ts/projection.rs:113` | "`declare function …` shim emitted at the top" | RESOLVE (TS shim emission shape; verify reachable or DELETE) | `rg -n 'declare function' crates/core/src/backend/ts/projection.rs` returns 0 OR confirms reachable site | `cargo check --workspace` 0 errors |

## §2 — Discipline gate

Per Operational Rule 2 ("No `investigate later`"), every row above carries a closure: DELETE / FAIL-EXPLICIT / RESOLVE / KEEP. No row uses "investigate". The KEEP rows are the anti-fallback narratives the audit explicitly tags KEEP per `audit/CENSUS-2026-05-03.md:190, 192, 207, 209`.

## §3 — Closer condition

W2.M4 close runs every grep gate above; expected exit 0 across the row table:

```
# Production-source fail-explicit gates
rg -n 'Defensive fallback' crates/core/src/  | wc -l        ; expect: 0
rg -n 'fall through to unitless' crates/core/src/runtime/css_l4/  ; expect: 0
rg -n 'pub use bbnf::\*' crates/core/src/grammar/generated/mod.rs ; expect: 0
rg -nE '_fallback:' crates/core/src/backend/emitter.rs       ; expect: 0
rg -n 'empty shim' crates/core/src/backend/rust/emitter/    ; expect: 0
rg -n 'for now' crates/core/src/backend/rust/emitter/shapes/keyword/ ; expect: 0
rg -n 'pre-W4 pattern' crates/core/src/backend/             ; expect: 0
rg -n 'push_leaf_with_unit' crates/core/src/backend/        ; expect: 0
rg -n 'legacy record stream' crates/core/src/backend/       ; expect: 0
rg -n 'legacy emission paths' crates/core/src/runtime/      ; expect: 0
rg -n 'legacy `emit_call`' crates/core/src/backend/kernels/ ; expect: 0
rg -n 'legacy directory' crates/core/src/backend/types/mod.rs ; expect: 0
rg -n 're-export shim' crates/core/src/backend/mod.rs       ; expect: 0
```

W2.M4 close confirms each row's gate exits 0 (or the row's KEEP rationale is documented per §1 anti-fallback narrative whitelist).
