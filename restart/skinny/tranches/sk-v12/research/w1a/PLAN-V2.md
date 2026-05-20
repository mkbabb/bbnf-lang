# SK-V12 W1a Plan V2 - GrammarConfig + Lock 14 Legality Gate

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: Plan revision after CHALLENGE.
Gate: `G-W1a-GRAMMARCONFIG-LOCK14`.
Prior disposition: W1a CHALLENGE V1 `REVISE`.

## Selection

Select the narrow legality route, revised to answer CHALLENGE:

1. Add a codegen-private `GrammarProfile` / JSON-provider boundary.
2. Emit generated JSON-local config metadata consumed by generated JSON code.
3. Rename the JSON direct and typed renderers into JSON-owned codegen roots.
4. Add a W1a-aware Lock 14 generic-crate scan consumed by `gate-json`.
5. Prove SK-V12 JSON direct/typed guard floors with the checked-in
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
- `skinny/crates/codegen/src/sink_direct.rs` (delete or leave a non-exported
  compatibility stub only if tests require it)
- `skinny/crates/codegen/src/typed_direct.rs` (delete or leave a non-exported
  compatibility stub only if tests require it)
- `skinny/crates/codegen/src/json_templates/config.rs` (new)
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/codegen/src/json_templates/mod.rs` if the template roster
  requires it
- `skinny/crates/codegen/src/json_templates/parser.rs`
- `skinny/crates/codegen/src/json_templates/value.rs`
- `skinny/crates/codegen/src/json_templates/view.rs`
- `skinny/crates/codegen/src/json_templates/visitor.rs`
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
  `check-real-typed` requires a deterministic regen change

Explicitly not owned by W1a V2:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/crates/ir/src/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/passes/src/`
- `skinny/crates/grammar/src/`
- `skinny/crates/bbnf-simd/`
- CSS, Sheets, or BBNF-self runtime/generated/parser/benchmark files

If redress discovers that a path outside the owned roster is required, W1a
returns to plan before editing it.

## Generated Roster And Scan/Sink Ownership

W1a V2 resolves the `scan.rs` / `sink.rs` ambiguity by declaring them
JSON-owned runtime source/template files, not generated outputs. They may remain
under `skinny/crates/runtime/src/grammars/json/` and may be copied by
`json_provider` for compatibility, but W1a must remove them from the generated
roster checked by `check-json`.

The generated JSON roster after W1a is exactly:

- `config.rs`
- `generated.rs`
- `host.rs`
- `mod.rs`
- `parser.rs`
- `value.rs`
- `view.rs`
- `visitor.rs`

`scan.rs` and `sink.rs` remain JSON-owned grammar source files imported by
`mod.rs`, with their generated headers removed or replaced by a JSON-owned
source comment. This choice avoids copying 395 LOC into codegen templates and
keeps the W1a redress inside the 30-minute cap. A future CSS provider must
supply its own grammar-owned scan/sink implementation; it cannot reuse these
JSON files through a generic root.

`check-json` must become exact for the generated roster it owns: expected files
must byte-match, missing expected files fail, and unexpected generated-roster
files fail. It must not enforce exactness against source siblings such as
`scan.rs` and `sink.rs`.

## Code Shape

`grammar_profile.rs` may define only consumed, grammar-neutral metadata and
provider descriptors. It must not carry inert policy fields and must not contain
JSON/CSS/Sheets literals. Provider selection is a data-driven lookup: a
provider id is matched to `backend.grammar_name`, and grammar-specific policy
stays in provider-owned modules.

`json_provider.rs` owns JSON-specific grammar literals, file roster, and
templates. A non-JSON `backend.grammar_name` must fail before any JSON renderer
or template can emit output.

`json_sink_direct.rs` and `json_typed_direct.rs` are mandatory containment.
Generic emission code must not import or render `JsonSink`, `JsonNodeKind`,
`JsonValue`, `JsonRoot`, `JsonVisitor`, JSON literals, JSON number/string
helpers, or `serde_json`.

Generated JSON `config.rs` must be imported from `runtime/src/grammars/json/mod.rs`
and consumed by generated JSON code in the same commit. Config fields are legal
only when a generated consumer exists in `generated.rs`, `parser.rs`, `view.rs`,
or the JSON direct/typed output path. A profile/config field without a same-wave
consumer is an orphan and fails W1a.

## Seven-Leak Closure Matrix

Redress must close this matrix executably:

| Leak | Legal home after W1a | Same-wave consumer / proof |
|---|---|---|
| JSON structural alphabet | `json_provider` / generated JSON `config.rs` / JSON-owned `scan.rs` | Generated JSON code imports `super::config`; generic-root scan rejects structural JSON literals. |
| JSON value dispatch | JSON provider/templates/generated JSON modules | Provider selection rejects non-JSON before JSON output; generic-root scan rejects JSON dispatch names. |
| JSON string quote/backslash policy | JSON provider/templates/generated JSON modules / JSON-owned scan/view | Generated JSON code consumes config string policy; generic-root scan rejects JSON string helper names. |
| JSON number span policy | JSON provider/templates/generated JSON modules | Generated JSON/direct/typed code consumes config number policy; generic-root scan rejects number-policy helper names. |
| JSON object/key/member colon policy | JSON provider/templates/generated JSON modules | Generic-root scan rejects object/key policy names and colon/comma parser names. |
| JSON `OffsetFlags` meaning | generated JSON `config.rs` maps grammar bits to runtime flags | Generic roots cannot interpret `OffsetFlags::HAS_ESC` as JSON escape policy. |
| JSON sink/view/kind/callback shape | `json_sink_direct.rs`, `json_typed_direct.rs`, generated JSON modules | Generic-root scan rejects `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`, and `serde_json`. |

The executable proof is a Lock 14 scan over generic roots plus positive tests
that the same tokens remain legal in JSON-owned roots.

## Lock 14 Consumer

Implement the W1a generic-crate scan inside
`skinny/crates/bbnf-bench/src/lock14_baseline.rs` and call it from the existing
`validate(root)` path consumed by `bbnf-bench --bin gate` and `xtask gate-json`.

Scan only generic roots:

- `crates/codegen/src/lib.rs`
- `crates/codegen/src/grammar_profile.rs`
- shared runtime grammar roots outside per-grammar subdirectories, if any
- `crates/runtime/src/lib.rs`
- `crates/runtime/src/tape/`
- `crates/ir/src/`
- `crates/passes/src/`
- `crates/bbnf-simd/src/` only if touched

Exclude per-grammar roots, JSON-owned provider/templates/renderers, tests, and
`restart/` research/docs. The scan must include negative tests for the seven
forbidden leak classes and positive allow tests for JSON-owned roots.

The W0 frozen-root validator may be made W1a-aware only for this exact owner
delta. It must not become a broad waiver. Parent-diff or dirty-root allowances
must be path-specific to the W1a owner roster and must continue to reject
directive, BIR, `BackendShape`, public substrate, and unowned generic changes.

## Guard Floors

W1a V2 replaces the rejected `lint-loc` dependency with a checked-in exact floor
command:

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

Generic-root scan sanity from the repo root:

```sh
rg -n 'grammar_name == "json"|STRUCTURAL_ALPHABET_JSON|b"\{\}\[\],:\\""|JsonSink|JsonNodeKind|JsonValue|JsonRoot|JsonVisitor|OffsetFlags::HAS_ESC|match_string_at_quote_trusted_utf8|match_number_span_from_first|serde_json|ExpectedColon|ExpectedCommaOr' skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/grammar_profile.rs skinny/crates/runtime/src/tape skinny/crates/runtime/src/lib.rs skinny/crates/ir/src skinny/crates/passes/src
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

Because W1a V2 still touches JSON-producing codegen/runtime paths, PASS requires
`json_guard_state = refreshed:<run-id>:guards-pass`. A no-touch
`not_refreshed:no_behavior_drift` close is invalid for this route.

## Cost And Size Discipline

`lint-loc` is not a W1a gate because the baseline already fails unrelated
crate-level ceilings. REDRESS 121 must instead record:

- hand LOC delta;
- generated LOC delta for the eight-file generated JSON roster;
- `generated_real_typed.rs` LOC delta if it moves;
- generated module byte totals;
- `skinny/grammars/json.bbnf` byte count;
- whether the delta is O(1) metadata/config plumbing or O(N) per-grammar growth.

At the 30-minute redress cap, the agent records the current state and rejects
or re-plans instead of broadening the wave.

## Revert Protocol

If redress fails, save only the V2 slice:

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
  skinny/crates/runtime/src/grammars/json/value.rs \
  skinny/crates/runtime/src/grammars/json/view.rs \
  skinny/crates/runtime/src/grammars/json/visitor.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/RESULTS.md \
  skinny/REDRESS.md \
  > /tmp/skv12-waveW1a-rejected.patch
```

Inspect the path list before any revert. Do not use broad checkout or
`git reset --hard`.

## CHALLENGE V2 Questions

The revised CHALLENGE must adjudicate:

1. whether the exact owner and generated rosters are tight enough for redress;
2. whether declaring `scan.rs` / `sink.rs` as JSON-owned source resolves the
   provenance ambiguity without weakening Lock 14;
3. whether mandatory `json_typed_direct.rs` containment closes the typed leak;
4. whether `verify-skv12-json-floors.awk` is sufficient floor evidence when
   paired with `gate-json --check-results`;
5. whether the Lock 14 scan has an executable seven-leak matrix and orphan
   field rejection;
6. whether removing report/xtask/bin-gate/schema/outcome changes keeps W1a in
   cap without paper-closing future CSS admission.
