# SK-V12 W1a Plan V3 - GrammarConfig + Lock 14 Legality Gate

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: Plan revision after CHALLENGE V2.
Gate: `G-W1a-GRAMMARCONFIG-LOCK14`.
Prior disposition: W1a CHALLENGE V2 `REVISE`.

## Selection

Select the narrow legality route, revised to close every CHALLENGE V2 blocker:

1. Add a codegen-private `GrammarProfile` / JSON-provider boundary.
2. Emit generated JSON-local config metadata consumed by generated JSON code.
3. Rename the JSON direct and typed renderers into JSON-owned codegen roots and
   delete the old generic-name renderer files.
4. Reclassify JSON `scan.rs` / `sink.rs` as JSON-owned source, not generated
   output, and own the header/provenance edit.
5. Remove the hardcoded JSON structural alphabet leak from the generic passes
   root under a narrow recognizer-only edit.
6. Add a W1a-aware Lock 14 generic-crate scan consumed by `gate-json`.
7. Prove SK-V12 JSON direct/typed guard floors with the checked-in
   `verify-skv12-json-floors.awk` command after the refreshed guard run.

W1a does not emit CSS L4, add a CSS benchmark row, compare against
lightningcss, open Sheets/BBNF-self fallback, add schema/outcome fields, or
claim any non-JSON admission. W1b remains the first legal CSS generation
surface after W1a passes.

## Exact Owner Roster

Editable source paths:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs` (new)
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs` (rename target)
- `skinny/crates/codegen/src/json_typed_direct.rs` (rename target)
- `skinny/crates/codegen/src/sink_direct.rs` (delete)
- `skinny/crates/codegen/src/typed_direct.rs` (delete)
- `skinny/crates/codegen/src/json_templates/config.rs` (new)
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/visitor.rs`
- `skinny/crates/runtime/src/grammars/json/scan.rs`
- `skinny/crates/runtime/src/grammars/json/sink.rs`
- `skinny/crates/passes/src/lib.rs`, limited to the recognizer structural
  alphabet derivation in `recognizers::derive_recognizers`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md` only if rewritten by the native JSON guard refresh
- `skinny/REDRESS.md` for REDRESS 121

Editable generated JSON outputs:

- `skinny/crates/runtime/src/grammars/json/config.rs` (new)
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/runtime/src/grammars/json/host.rs`
- `skinny/crates/runtime/src/grammars/json/mod.rs`
- `skinny/crates/runtime/src/grammars/json/parser.rs`
- `skinny/crates/runtime/src/grammars/json/value.rs`
- `skinny/crates/runtime/src/grammars/json/view.rs`
- `skinny/crates/runtime/src/grammars/json/visitor.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` only if
  `check-real-typed` requires deterministic regen

Explicitly not owned by W1a V3:

- `skinny/crates/codegen/src/json_templates/mod.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/crates/ir/src/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/grammar/src/`
- `skinny/crates/bbnf-simd/`
- CSS, Sheets, or BBNF-self runtime/generated/parser/benchmark files

If redress discovers that a path outside the owned roster is required, W1a
returns to plan before editing it.

## Generated Roster And JSON-Owned Source

The generated JSON roster after W1a is exactly:

- `config.rs`
- `generated.rs`
- `host.rs`
- `mod.rs`
- `parser.rs`
- `value.rs`
- `view.rs`
- `visitor.rs`

`scan.rs` and `sink.rs` are JSON-owned source files imported by generated
`mod.rs`. Redress removes or replaces their generated headers and stops
`json_provider` from emitting or checking them as generated outputs. A future
CSS provider must supply its own grammar-owned scan/sink implementation; it
cannot reuse these JSON files through a generic root.

`check-json` must be exact for the eight generated files it owns: expected
files byte-match, missing expected files fail, and unexpected generated-roster
files fail. It must not enforce exactness against source siblings such as
`scan.rs` and `sink.rs`.

## Code Shape

`grammar_profile.rs` may define only consumed, grammar-neutral provider metadata
and helper functions. It must not carry inert policy fields and must not contain
JSON/CSS/Sheets literals. Provider selection is a data-driven lookup: a
provider id is matched to `backend.grammar_name`, and grammar-specific policy
stays in provider-owned modules.

`json_provider.rs` owns JSON-specific grammar literals, generated file roster,
and templates. A non-JSON `backend.grammar_name` must fail before
`json_provider`, `json_sink_direct`, or `json_typed_direct` can emit output.

`json_sink_direct.rs` and `json_typed_direct.rs` are mandatory containment.
The old `sink_direct.rs` and `typed_direct.rs` files are deleted; if redress
proves deletion impossible, any retained compatibility stub must contain no
JSON policy and must be included in the generic leak scan before CHALLENGE can
accept the change.

Generated JSON `config.rs` must be imported from
`runtime/src/grammars/json/mod.rs` and consumed by generated JSON code in the
same commit. Config fields are legal only when a generated consumer exists in
`generated.rs`, `parser.rs`, `view.rs`, JSON direct output, or JSON typed
output. A profile/config field without a same-wave consumer is an orphan and
fails W1a.

## Generic Passes Root

`skinny/crates/passes/src/lib.rs` is owned only to remove the hardcoded JSON
structural alphabet leak in `recognizers::derive_recognizers`. The legal edit
is narrow: derive the recognizer alphabet from grammar literal/regex facts
already present in `GrammarIr` without a JSON punctuation superset literal.
No directive, BIR variant, `BackendShape`, public substrate API, or pass-wide
policy expansion is authorized.

The Lock 14 scan must reject production JSON structural alphabet literals in
generic roots. Tests may contain JSON tokens only if the Rust scan deliberately
excludes test-only code and the manual sanity command uses the same exclusion
model. Raw `rg` over inline tests is not a W1a gate.

## Seven-Leak Closure Matrix

Redress must close this matrix executably:

| Leak | Legal home after W1a | Same-wave consumer / proof |
|---|---|---|
| JSON structural alphabet | `json_provider` / generated JSON `config.rs` / JSON-owned `scan.rs` | Generated JSON code imports `super::config`; generic-root scan rejects structural JSON literals, including the former `passes` leak. |
| JSON value dispatch | JSON provider/templates/generated JSON modules | Provider selection rejects non-JSON before JSON output; generic-root scan rejects JSON dispatch names. |
| JSON string quote/backslash policy | JSON provider/templates/generated JSON modules / JSON-owned scan/view | Generated JSON code consumes config string policy; generic-root scan rejects JSON string helper names. |
| JSON number span policy | JSON provider/templates/generated JSON modules | Generated JSON/direct/typed code consumes config number policy; generic-root scan rejects number-policy helper names. |
| JSON object/key/member colon policy | JSON provider/templates/generated JSON modules | Generic-root scan rejects object/key policy names and colon/comma parser names. |
| JSON `OffsetFlags` meaning | generated JSON `config.rs` maps grammar bits to runtime flags | Generic roots cannot interpret `OffsetFlags::HAS_ESC` as JSON escape policy. |
| JSON sink/view/kind/callback shape | `json_sink_direct.rs`, `json_typed_direct.rs`, generated JSON modules | Generic-root scan rejects `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`, and `serde_json`. |

The executable proof is a Lock 14 scan over generic roots plus positive tests
that the same tokens remain legal in JSON-owned roots.

## Orphan Field Checks

W1a redress must add and pass named executable checks:

- `cargo test -p codegen json_config_policy_fields_are_consumed -- --nocapture`
  proves every generated JSON `config.rs` policy item used to satisfy the
  seven-leak matrix has at least one consumer in emitted JSON runtime output or
  generated JSON direct/typed output.
- `cargo test -p codegen grammar_profile_fields_are_consumed -- --nocapture`
  proves every `GrammarProfile` field is used by provider selection, generated
  file routing, or the Lock 14 gate; inert profile fields fail.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` includes negative
  fixtures for all seven forbidden generic leak classes and positive fixtures
  proving the same tokens are legal in JSON-owned roots.

## Lock 14 Consumer

Implement the W1a generic-crate scan inside
`skinny/crates/bbnf-bench/src/lock14_baseline.rs` and call it from the existing
`validate(root)` path consumed by `bbnf-bench --bin gate` and `xtask gate-json`.

Scan only generic roots:

- `crates/codegen/src/lib.rs`
- `crates/codegen/src/grammar_profile.rs`
- `crates/passes/src/lib.rs`
- shared runtime grammar roots outside per-grammar subdirectories, if any
- `crates/runtime/src/lib.rs`
- `crates/runtime/src/tape/`
- `crates/ir/src/`
- `crates/bbnf-simd/src/` only if touched

Exclude per-grammar roots, JSON-owned provider/templates/renderers, test-only
code, and `restart/` research/docs. The Rust scan and any manual sanity command
must share the same test-exclusion semantics.

The W0 frozen-root validator may be made W1a-aware only for this exact owner
delta. It must not become a broad waiver. Parent-diff or dirty-root allowances
must be path-specific to the W1a owner roster and must continue to reject
directive, BIR, `BackendShape`, public substrate, and unowned generic changes.

## Guard Floors

W1a V3 keeps the checked-in exact floor command:

```sh
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

This command parses the rendered top-level `RESULTS.md` table and enforces the
SPEC Section 0.5 direct and typed Track 1 / Track 2 floors exactly. It is not a
replacement for `gate-json`; it is an additional W1a floor proof after
`RESULTS.md` exactness is established.

## Verification

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted:

```sh
cargo test -p codegen
cargo test -p codegen json_config_policy_fields_are_consumed -- --nocapture
cargo test -p codegen grammar_profile_fields_are_consumed -- --nocapture
cargo test -p runtime
cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p bbnf-bench direct_contract -- --nocapture
cargo test -p bbnf-bench w6_typed_contract -- --nocapture
cargo test -p bbnf-bench generated_ -- --nocapture
cargo test -p bbnf-bench parity -- --nocapture
```

Native guard refresh:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard-target CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Exact floor proof from repo root:

```sh
awk -f restart/skinny/tranches/sk-v12/research/w1a/verify-skv12-json-floors.awk skinny/RESULTS.md
```

Because W1a V3 still touches JSON-producing codegen/runtime paths, PASS
requires `json_guard_state = refreshed:<run-id>:guards-pass`. A no-touch
`not_refreshed:no_behavior_drift` close is invalid for this route.

## Cost And Size Discipline

`lint-loc` is not a W1a gate because the baseline already fails unrelated
crate-level ceilings. REDRESS 121 must instead record:

- hand LOC delta;
- generated LOC delta for the eight-file generated JSON roster;
- `scan.rs` / `sink.rs` source LOC delta;
- `generated_real_typed.rs` LOC delta if it moves;
- generated module byte totals;
- `skinny/grammars/json.bbnf` byte count;
- whether the delta is O(1) metadata/config plumbing or O(N) per-grammar growth.

At the 30-minute redress cap, the agent records the current state and rejects
or re-plans instead of broadening the wave.

## Revert Protocol

If redress fails, save only the V3 slice:

```sh
git diff --binary HEAD -- \
  skinny/crates/codegen/src/lib.rs \
  skinny/crates/codegen/src/grammar_profile.rs \
  skinny/crates/codegen/src/json_provider.rs \
  skinny/crates/codegen/src/json_sink_direct.rs \
  skinny/crates/codegen/src/json_typed_direct.rs \
  skinny/crates/codegen/src/sink_direct.rs \
  skinny/crates/codegen/src/typed_direct.rs \
  skinny/crates/codegen/src/json_templates \
  skinny/crates/runtime/src/grammars/json/config.rs \
  skinny/crates/runtime/src/grammars/json/generated.rs \
  skinny/crates/runtime/src/grammars/json/host.rs \
  skinny/crates/runtime/src/grammars/json/mod.rs \
  skinny/crates/runtime/src/grammars/json/parser.rs \
  skinny/crates/runtime/src/grammars/json/scan.rs \
  skinny/crates/runtime/src/grammars/json/sink.rs \
  skinny/crates/runtime/src/grammars/json/value.rs \
  skinny/crates/runtime/src/grammars/json/view.rs \
  skinny/crates/runtime/src/grammars/json/visitor.rs \
  skinny/crates/passes/src/lib.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/RESULTS.md \
  skinny/REDRESS.md \
  > /tmp/skv12-waveW1a-rejected.patch
```

Inspect the path list before any revert. Do not use broad checkout or
`git reset --hard`.

## CHALLENGE V3 Questions

The revised CHALLENGE must adjudicate:

1. whether adding `scan.rs` / `sink.rs` to source ownership and removing them
   from generated output resolves the provenance ambiguity;
2. whether the narrow `passes/src/lib.rs` ownership is enough to remove the
   generic JSON structural alphabet leak without opening pass-wide policy;
3. whether deleting `sink_direct.rs` / `typed_direct.rs` or scan-covering stubs
   closes the renderer leak;
4. whether the named orphan-field tests are executable enough for CH1;
5. whether the Lock 14 scan and manual sanity model agree on test exclusion;
6. whether W1a still avoids CSS/SOTA/fallback claims and broad report/xtask
   plumbing.
