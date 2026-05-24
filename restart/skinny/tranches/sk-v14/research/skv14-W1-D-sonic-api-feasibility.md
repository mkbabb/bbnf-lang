# SK-V14 W1D: Sonic API Feasibility

Date: 2026-05-24.
Scope: sonic-rs 0.5.8 APIs usable for W1 strict comparators.
Output: this file.

## §1 — Findings

- `skinny/crates/bbnf-bench/Cargo.toml:23` pins `sonic-rs = "=0.5.8"` with `default-features = false` and `features = ["sort_keys"]`.
- `sonic-rs-0.5.8/src/lib.rs:46-49` exports `from_slice`, `Deserializer`, `Parser`, and serde APIs.
- There is no public `sonic_rs::Skipper` type in sonic-rs 0.5.8.
- `sonic-rs-0.5.8/src/serde/de.rs:863-868` implements `deserialize_ignored_any` by calling the internal parser's checked `skip_one(true)` path.
- `sonic-rs-0.5.8/src/parser.rs:1439` exposes `Parser::skip_one(checked: bool)`, but complete trailing validation is not cleanly exposed as a public parser-only API.
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:690-730` already dispatches `sonic_rs::from_slice::<Target>` per typed corpus.

## §2 — Recommendations

- Implement the parse_only strict comparator as a local Skipper-class wrapper around public sonic serde deserialization into `serde::de::IgnoredAny`, followed by `Deserializer::end()` when using the explicit deserializer form.
- Do not call it `sonic_rs::Skipper` as a public API unless a local wrapper type with truthful naming is introduced.
- Prefer explicit metadata naming such as `bbnf_bench::sonic_skipper::parse_only` or `sonic_rs_skipper` while the manifest comparator plane can remain the contractual `sonic_rs::Skipper-class`.
- Use existing `real_typed_struct::sonic_typed` for typed strict comparator rows.

## §3 — Risks

- Direct use of `Parser::skip_one(true)` risks trailing-validation gaps.
- Current direct strict comparator deserializes into `JsonDirectDigest`, not per-corpus product structs; the W1 plan must decide whether that digest is the strict direct target or whether new per-corpus direct targets are required.
- Existing typed structs generally use `#[serde(default)]`; this is strict for syntax/UTF-8/trailing input but not for unknown object fields.

## §4 — Sources

- `skinny/crates/bbnf-bench/Cargo.toml:23`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/lib.rs:46-49`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/serde/de.rs:863-868`
- `/Users/mkbabb/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/sonic-rs-0.5.8/src/parser.rs:1439`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs:690-730`
