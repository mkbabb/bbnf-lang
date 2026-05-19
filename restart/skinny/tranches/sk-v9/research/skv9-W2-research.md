# SK-V9 Wave W2 Research: Retained Class/Event Grammar Proof

Inputs: `restart/skinny/tranches/sk-v9/SPEC.md` Section 5;
`restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`;
`skinny/crates/runtime/src/tape/mod.rs`;
`skinny/crates/runtime/src/lib.rs`; `skinny/crates/runtime/Cargo.toml`;
`skinny/RESULTS.md`; `skinny/REDRESS.md` Item 92.

Status: W1 is closed by REDRESS 94. W2 is dispatchable, but the proof
CHALLENGE is mandatory before redress because W2 introduces the first
retained class/event proof surface.

## Current Source Shape

- `runtime/src/tape/mod.rs` owns `Tape<'input>`,
  `ValueRef<'doc, 'input: 'doc, K = AnyKind>`, `AnyKind`, and
  `DocumentView`. `ValueRef` currently stores `&Tape`, `cursor`, a zero-sized
  `PhantomData<fn() -> K>`, and an input-lifetime marker. Production JSON
  view code consumes it through the default and concrete marker kinds.
- `runtime/src/lib.rs` exports `pub mod tape;`, maps
  `grammars/json/mod.rs` as `generated_json`, and exposes it through
  `pub mod grammars { pub use crate::generated_json as json; }`.
- `runtime/src/grammars/json/mod.rs` is generated and currently exports only
  production JSON modules. A W2 witness must be a sibling proof module and not
  alter `generated.rs`, `parser.rs`, `scan.rs`, `value.rs`, or codegen
  templates.
- `runtime/Cargo.toml` declares `bench-counters` and `parse-attribution`; it
  does not currently declare `proof`. The SPEC names
  `#[cfg(any(test, feature = "proof"))]`, so the plan must either keep
  witnesses test-only without enabling a `proof` feature, or explicitly route a
  Cargo feature addition through CHALLENGE because `Cargo.toml` is not in the
  Section 5 owner table.
- `skinny/crates/runtime/src/grammars/` contains only the generated JSON
  directory today. The non-JSON witness must create a proof-only
  `sheets_witness` directory so Lock 14 is exercised without adding production
  grammar code.

## Proof Obligations

- Add `EventGrammar` and `AnyGrammar` as grammar-neutral types. The trait may
  expose only opaque fact/class admission, not grammar-role enums or generic
  `match grammar` arms.
- Rename the `ValueRef` marker parameter from `K = AnyKind` to
  `G: EventGrammar = AnyGrammar` while preserving current call sites through
  the default. The field layout must remain one tape reference, one cursor,
  and zero-sized markers.
- Add JSON and Sheets witnesses behind a parent `cfg` gate, plus compile-only
  proof tests with `const _: fn() = _proof_compiles::<...>;`.
- Add a negative borrow-check proof for an attempted
  `ValueRef<'static, 'static, JsonEventGrammar>` created from a non-static
  tape. Because the runtime crate has no existing compile-fail harness, the
  plan must pick either a small `rustc`-invoked fixture under runtime tests or
  an existing workspace pattern if one is found.
- Preserve `skinny/RESULTS.md` byte-for-byte. W2 has no measured row movement.
- Preserve bench reachability: `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/`
  must return zero.

## Risks For Plan/CHALLENGE

- Owner-table mismatch: enabling an explicit `proof` feature would touch
  `runtime/Cargo.toml`, outside Section 5. CHALLENGE must accept that path or
  the implementation must use `cfg(test)` only.
- Generated-module boundary: `runtime/src/grammars/json/mod.rs` is marked
  generated. W2 should avoid hand-editing it; parent gating in `runtime/src/lib.rs`
  can expose `generated_json::event_grammar_witness` only if the file can be
  included without modifying the generated module. If Rust module resolution
  requires touching `json/mod.rs`, CHALLENGE must decide whether that violates
  the no-generated-output clause.
- Compile-fail harness: adding a new dev dependency for trybuild would touch
  `runtime/Cargo.toml`. A no-dependency `rustc` fixture avoids that owner
  expansion but needs a robust test command.

## Recommended Plan Direction

Use the narrowest owner-respecting path:

1. Keep `event_grammar.rs` in the `tape` module and compile it in normal
   runtime builds so `ValueRef<G: EventGrammar = AnyGrammar>` has a default.
2. Keep witness modules and proof tests under `#[cfg(test)]` in parent modules
   unless CHALLENGE approves adding a `proof` feature to `runtime/Cargo.toml`.
3. Do not touch `skinny/RESULTS.md`, generated runtime JSON files, codegen
   templates, or benchmark crates.
4. Verify with `cargo check -p runtime`, `cargo test -p runtime event_grammar`,
   `git diff --exit-code HEAD -- skinny/RESULTS.md`, the Lock 14 `rg` audits,
   and `rg 'event_grammar|event_grammar_witness' skinny/crates/bbnf-bench/`.
