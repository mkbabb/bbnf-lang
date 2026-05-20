# SK-V12 W1a Plan - GrammarConfig + Lock 14 Legality Gate

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: Plan.
Gate: `G-W1a-GRAMMARCONFIG-LOCK14`.
Status: Superseded by `PLAN-V3.md`.

This V1 plan was rejected by W1a CHALLENGE as too broad and ambiguous. V2 was
then rejected by CHALLENGE V2. The binding redress plan is `PLAN-V3.md`; this
file is retained as the rejected V1 record.

## Selection

Select the narrow legality route: introduce a codegen-private
`GrammarProfile` boundary, emit generated JSON-local config metadata, and add a
W1a-aware Lock 14 generic-crate scan consumed by the existing
`bbnf-bench --bin gate` / `xtask gate-json` path.

W1a does not emit CSS L4, does not add a CSS benchmark row, does not compare to
lightningcss, and does not open Sheets or BBNF-self fallback. W1b-1/W1b-2 are
the first legal CSS generation and lightningcss admission surfaces after this
gate passes.

## Owner Paths

Editable:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs` (new)
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_sink_direct.rs` (new if renderer containment is needed)
- `skinny/crates/codegen/src/json_typed_direct.rs` (new if typed renderer containment is needed)
- `skinny/crates/codegen/src/sink_direct.rs` / `typed_direct.rs` only to move
  JSON-owned logic or leave thin wrappers
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/grammars/json/` as generated output, including
  `config.rs` if emitted
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/report.rs` only for an additive SK-V12 guard
  floor validator
- `skinny/crates/bbnf-bench/src/bin/gate.rs` and `skinny/xtask/src/main.rs`
  only if unchanged consumer plumbing cannot run the selected gate
- `skinny/RESULTS.md` only if rewritten by a fresh native JSON guard refresh
- `skinny/REDRESS.md` for Item 121 accounting

Read-only unless a blocker proves otherwise:

- `skinny/crates/ir/src/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/passes/src/`
- `skinny/crates/grammar/src/`
- `skinny/crates/bbnf-simd/`

Not authorized: CSS runtime/generated parser files, new non-JSON benchmark
bodies, report outcome/schema churn, public `runtime::tape::GrammarConfig`,
new directive/BIR/`BackendShape`, or public substrate API.

## Code Shape

Add `grammar_profile.rs` with grammar-neutral metadata structs only. It may
define profile/provider descriptors and a data-driven provider selector, but it
must not contain JSON/CSS/Sheets policy literals. The selector may compare a
provider id to `backend.grammar_name`; grammar-specific literals and policy
stay in provider-owned modules such as `json_provider`.

Replace the current JSON-only emission choke point with provider selection in
`codegen/src/lib.rs`. Keep IR lowering and `BackendShape` selection unchanged.
The provider emits runtime files and typed files from an already-lowered
program.

Move JSON direct and typed renderers behind JSON ownership if needed. Generic
emission code must not import or render `JsonSink`, `JsonNodeKind`,
`JsonValue`, `JsonRoot`, `JsonVisitor`, JSON literals, JSON number/string
helpers, or `serde_json`.

Emit a generated JSON `config.rs` and include it from JSON `mod.rs`. JSON
generated parser/view/direct code should import `super::config` for structural
alphabet, layout skip, string policy, number policy, and flag bit meaning. JSON
may map `STRING_NEEDS_DECODE` to `OffsetFlags::HAS_ESC` inside generated JSON
config; generic roots must not interpret that bit as JSON escape policy.

Add exact generated roster checking for JSON runtime output if the file set
changes. Do not make `EmittedSource::check_dir` globally exact for directories
like `crates/bbnf-bench/src` that contain source siblings.

## Seven Leaks

Redress must close or explicitly fail all seven Lock 14 leaks:

1. JSON structural alphabet hardcoding.
2. JSON value dispatch hardcoding.
3. JSON string quote/backslash escape policy in reusable templates.
4. JSON number span policy in reusable templates.
5. JSON object/key/member colon policy in reusable templates.
6. JSON `OffsetFlags` interpretation in generic code.
7. `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`, and JSON
   callback shape leaking into generic/direct renderer code.

JSON-specific names may remain only in generated JSON modules or explicitly
JSON-owned provider/template/renderer roots excluded from CSS-reusable generic
roots.

## Lock 14 Consumer

Implement `validate_generic_crate_neutrality(root)` in
`skinny/crates/bbnf-bench/src/lock14_baseline.rs` and call it from
`validate(root)`.

Scan `.rs` files in generic roots only:

- `crates/codegen/src/lib.rs`
- `crates/codegen/src/grammar_profile.rs`
- `crates/runtime/src/lib.rs`
- `crates/runtime/src/tape/`
- shared runtime grammar files outside per-grammar subdirectories
- `crates/ir/src/`
- `crates/passes/src/`
- `crates/bbnf-simd/src/` only if touched

Exclude per-grammar generated modules, JSON-owned templates/providers/renderers,
tests that assert rejection/allowance, and `restart/` documentation.

Fail on grammar parser names, literal grammar-name policy branches, JSON
structural alphabet constants, JSON string/number helpers, JSON object-key
policy names, JSON `OffsetFlags` meaning, and JSON sink/view/kind names. Add
negative tests for every forbidden class and positive tests proving the same
tokens are legal in JSON-owned roots.

Keep this as a gate precondition. Do not add outcome ids, schema fields, or
`RESULTS.md` rows for the scan.

## Gate And Evidence

`G-W1a-GRAMMARCONFIG-LOCK14` passes only when:

- the W1a generic-crate scan is consumed through `gate-json`;
- JSON generated runtime output and real typed output are byte-clean or
  regenerated with the new exact roster;
- JSON parity and conformance tests pass;
- SPEC Section 0.5 direct and typed guard floors pass exactly;
- `skinny/RESULTS.md` is byte-exact after any refresh;
- no CSS/non-JSON row is added;
- no directive, BIR variant, `BackendShape`, public substrate API, or IR edit
  lands;
- generated-size facts are recorded in REDRESS 121.

Because this selected plan touches JSON-producing codegen/runtime paths,
`json_guard_state = not_refreshed:no_behavior_drift` is not valid. Redress must
use a refreshed native JSON guard run and record
`json_guard_state = refreshed:<run-id>:guards-pass`, or reject/demote on a
measured floor miss.

## Verification

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted:

```sh
cargo test -p codegen
cargo test -p runtime
cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
cargo run -p xtask -- lint-loc
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p bbnf-bench skv12_non_json_report -- --nocapture
cargo test -p bbnf-bench --bin gate skv12_non_json_report_arg -- --nocapture
cargo test -p xtask gate_json_passthrough -- --nocapture
```

Generic-root scan sanity from repo root:

```sh
rg -n 'grammar_name == "json"|runtime emission currently supports grammar profile|STRUCTURAL_ALPHABET_JSON|b"\{\}\[\],:\\""|JsonSink|JsonNodeKind|JsonValue|JsonRoot|JsonVisitor|OffsetFlags::HAS_ESC|match_string_at_quote_trusted_utf8|match_number_span_from_first|serde_json|ExpectedColon|ExpectedCommaOr' skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/grammar_profile.rs skinny/crates/runtime/src/tape skinny/crates/runtime/src/lib.rs skinny/crates/ir/src skinny/crates/passes/src
```

Native guard refresh:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard-target CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

The guard floor check must cover the SPEC Section 0.5 direct/typed floors
named in `PLAN-P1-grammar-profile.md` and `PLAN-P2-lock14-gate.md`.

## Same-Wave Consumer

The generated JSON parser/view/direct/typed modules must import and exercise
the generated config in the same redress commit. `bbnf-bench --bin gate` must
consume the generic-crate scan through `lock14_baseline::validate` in that same
commit. An emitted metadata file without a same-wave generated consumer is an
orphan and fails W1a.

## Revert Protocol

If redress fails, save only the W1a candidate slice:

```sh
git diff --binary HEAD -- \
  skinny/crates/codegen \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/xtask/src/main.rs \
  skinny/RESULTS.md \
  skinny/REDRESS.md \
  > /tmp/skv12-waveW1a-rejected.patch
```

Inspect the patch path list. If it contains unrelated user or parallel-agent
edits, split it before any revert. Revert only W1a candidate files and
generated outputs. Do not use `git reset --hard` or broad checkout commands.

Record REDRESS 121 as PASS only if the gate passes with refreshed JSON guard
floors and no CSS row. Record REDRESS 121 as BLOCKED/REJECTED with failed
command evidence and routed remainder if any gate fails.

## CHALLENGE

CHALLENGE is mandatory. Review must specifically adjudicate:

- whether the provider selector is a legal data-driven boundary rather than a
  generic grammar-policy branch;
- whether generated config has a same-wave consumer;
- whether the Lock 14 scan avoids schema/outcome churn while still catching the
  seven leaks;
- whether JSON guard refresh and SPEC floor checks are sufficient;
- whether W1a is avoiding CSS admission and future-phase promises.
