# SK-V12 Wave W1a Plan P2: Lock 14 Gate Consumer

Date: 2026-05-20.
Wave: W1a - GrammarConfig + Lock 14 Legality Gate.
Phase: Plan.

## Inputs

- `restart/skinny/tranches/sk-v12/SPEC.md:259-275` - every generic-crate edit
  must pass Lock 14, per-grammar generated modules own grammar policy, CSS
  evidence must be executable later, and generated size must be tracked.
- `restart/skinny/tranches/sk-v12/SPEC.md:314-349` - W1a makes CSS L4 emission
  legal before CSS generation, must add generated metadata plus a Lock 14
  consumer, preserve JSON parity/floors, claim no CSS row, add no directive,
  BIR variant, `BackendShape`, or public substrate API, and save rejected
  patches at `/tmp/skv12-waveW1a-rejected.patch`.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-35` and
  `:80-106` - CSS L4 is authoritative and the later close floor is
  `lightningcss_mbps + 1`, while JSON guard floors and Lock 14 remain active.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:41-63` and `:112-115`
  - the plan selects one intervention with owner paths, falsifiability gate,
  same-wave consumer, revert protocol, and CHALLENGE posture.
- `restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md:7-40` -
  research selects codegen/runtime metadata and gate consumption only; no CSS
  row, no IR expansion, no hand CSS parser, and no public tape API.
- `restart/skinny/tranches/sk-v12/research/w1a/A1-codegen-template-leaks.md:23-69`
  - the seven Lock 14 leaks are structural alphabet, value dispatch,
  string/escape policy, number policy, object/key member policy, `OffsetFlags`
  interpretation, and sink/view/kind bindings.
- `restart/skinny/tranches/sk-v12/research/w1a/A3-lock14-gate-consumer.md:51-88`
  - the scan belongs in `lock14_baseline::validate`, not in `Outcome`,
  `SCHEMA_V3_HEADER`, `SkV12NonJsonReport`, or `RESULTS.md`.
- `restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md:161-177`
  - W1a must handle stale generated files, scan/sink provenance, W1a-aware
  Lock 14 gate use, exact guard floors, and `RESULTS.md` byte exactness.
- `restart/skinny/tranches/sk-v12/research/w1a/A6-json-guard-redress.md:122-193`
  - SK-V12 W1a uses gate label `G-W1a-GRAMMARCONFIG-LOCK14`; next REDRESS
  outcome is Item 121; no CSS row, lightningcss result, or SOTA claim is
  admitted.

Intervention: introduce a codegen-private `GrammarProfile` plus generated JSON
`config.rs` metadata and wire a generic-crate Lock 14 neutrality scan into the
existing `bbnf-bench --bin gate` / `xtask gate-json` consumer.

## Selection

Select the narrow legality route:

1. Add codegen-private grammar metadata under `skinny/crates/codegen/src/`.
   The metadata carries structural bytes, dispatch arms, layout/trivia,
   string/escape policy, number policy, flag meanings, and sink/view/kind
   bindings. It is not an IR, directive, `BackendShape`, or public runtime API
   change.
2. Emit generated JSON-local policy, preferably
   `skinny/crates/runtime/src/grammars/json/config.rs`, and update JSON module
   emission so JSON policy lives in generated JSON modules or JSON-owned
   profile/template files rather than CSS-reusable generic emitters.
3. Replace the JSON-only runtime-profile choke point with a fail-closed profile
   registry. Generic code may do data-driven lookup of `GrammarProfile::id`;
   grammar-specific literals and policy stay in per-grammar profile modules.
4. Add `validate_generic_crate_neutrality(root)` to
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs` and call it from
   `validate(root)` after W1a-aware freeze/allowlist checks and before
   `validate_backend_shape_surface`.
5. Preserve JSON behavior by regenerating/checking JSON and real typed output,
   verifying `RESULTS.md` exactness, and proving SPEC Section 0.5 JSON guard
   floors.

W1a does not emit CSS L4, does not add a CSS benchmark row, does not compare to
lightningcss, and does not open Sheets or BBNF-self fallback. W1b-1/W1b-2 are
the first legal CSS generation and lightningcss admission surfaces after W1a.

## Owner Paths

Editable for redress:

- `skinny/crates/codegen/src/lib.rs`
- `skinny/crates/codegen/src/grammar_profile.rs`
- `skinny/crates/codegen/src/grammar_profiles/mod.rs`
- `skinny/crates/codegen/src/grammar_profiles/json.rs`
- `skinny/crates/codegen/src/json_provider.rs`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/codegen/src/typed_direct.rs` only if typed output needs the
  same profile boundary
- `skinny/crates/runtime/src/grammars/json/` as generated JSON output,
  including a new `config.rs` if emitted
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` only as typed regen
  output
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/crates/bbnf-bench/src/report.rs` only for an additive SK-V12 guard
  floor validator, not schema or outcome changes
- `skinny/crates/bbnf-bench/src/bin/gate.rs` only if needed to keep the existing
  gate error/reporting path exact
- `skinny/xtask/src/main.rs` only if existing `gate-json` passthrough prevents
  running the unchanged consumer
- `skinny/REDRESS.md` for Item 121 during redress accounting
- `skinny/RESULTS.md` only if a fresh JSON guard run rewrites it through the
  existing generated report path and the follow-up check proves byte exactness

Read/verify only unless a blocker proves otherwise:

- `skinny/crates/ir/src/`
- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/lib.rs`
- `skinny/crates/passes/src/`
- `skinny/crates/grammar/src/`
- `skinny/crates/bbnf-simd/src/`

Not authorized:

- CSS runtime/generated parser files.
- New non-JSON benchmark bodies.
- New report row schemas or outcome ids.
- Public `runtime::tape::GrammarConfig`, `UnionTape`, retained cursor/vector
  substrate, directive parsing, BIR variants, or `BackendShape` variants.

## Seven Leaks To Close

The redress patch must close or explicitly fail all seven leaks named by the
audit:

1. JSON structural alphabet hardcoding.
2. JSON value dispatch hardcoding.
3. JSON string quote/backslash escape policy in reusable templates.
4. JSON number span policy in reusable templates.
5. JSON object/key/member colon policy in reusable templates.
6. JSON `OffsetFlags::HAS_ESC` / control-bit interpretation in generic code.
7. `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`, and JSON
   callback shape leaking into generic/direct renderer code.

JSON-specific names may remain only in generated JSON modules or explicitly
JSON-owned profile/template files that are excluded from CSS-reusable generic
roots.

## Exact Lock 14 Scan Policy

Implement the scan in Rust inside `lock14_baseline.rs`; do not shell out to
`rg` from the gate. The scan walks `.rs` files under these generic roots,
relative to `skinny/`:

- `crates/runtime/src/lib.rs`
- `crates/runtime/src/tape/`
- shared `crates/runtime/src/grammars/` files, excluding per-grammar
  subdirectories such as `grammars/json/`
- `crates/ir/src/`
- `crates/passes/src/`
- `crates/codegen/src/`, excluding `json_provider.rs`, `json_templates/`, and
  `grammar_profiles/json.rs`
- `crates/bbnf-simd/src/` only if W1a touches shared scanner/SIMD substrate

Fail the scan with `Err("Lock 14 generic neutrality failed: ...")` on any of
these classes in scanned generic roots:

- grammar parser/type names:
  `JsonParser`, `CssL4Parser`, `GoogleSheetsParser`, `BbnfBootstrap`;
- grammar-name policy branches:
  `grammar_name == "json"`, `grammar_name == "css"`,
  `grammar_name == "css_l4"`, `grammar_name == "sheets"`,
  `grammar_name == "google_sheets"`, `grammar_name == "bbnf_bootstrap"`, or a
  `match` arm over `grammar_name` whose body contains grammar policy;
- JSON structural alphabet literals/constants:
  `STRUCTURAL_ALPHABET_JSON`, `b"{}[],:\""`, or equivalent JSON punctuation
  class tables in generic roots;
- JSON string/escape helpers in generic roots:
  `match_string_at_quote_trusted_utf8`, `unescape_string`,
  `OffsetFlags::HAS_ESC` when used as JSON decode meaning, or JSON quote /
  backslash / control policy constants;
- JSON number helpers in generic roots:
  `match_number_span_from_first`, JSON-only `-` / digit FIRST dispatch, or
  `serde_json::Number` materialization policy;
- JSON object/key policy:
  `parse_key_colon`, `ExpectedColon`, `ExpectedCommaOr`, quoted-key plus colon
  helpers, or JSON object pair traversal policy;
- JSON sink/view/kind shape:
  `JsonSink`, `JsonNodeKind`, `JsonValue`, `JsonRoot`, `JsonVisitor`,
  `JsonObject`, `JsonArray`, or JSON direct callback names.

Allow these tokens only in per-grammar generated modules, JSON-owned profile or
template roots, tests that assert rejection/allowance, and documentation under
`restart/`. The scan must include negative fixture tests for each forbidden
class and positive tests proving the same tokens are allowed in JSON-owned
roots.

## Report And RESULTS Exactness

No report schema or outcome change is selected.

Do not edit `Outcome`, `SCHEMA_V3_HEADER`, `SCHEMA_V3_ALIGN`,
`SkV12NonJsonReport`, row field names, or markdown rendering for the Lock 14
scan. The scan is a precondition inside `lock14_baseline::validate`; it is not a
row and not an outcome.

The only justified report-path change is an additive SK-V12 guard floor
validator because current executable checks do not exactly cover the active
SPEC Section 0.5 floor table. If added, it must:

- validate existing JSON rows only;
- not change markdown output;
- not add or rename fields;
- not admit/demote outcomes by policy rewrite;
- fail with an explicit floor-miss error if Track 1 or Track 2/oracle is below
  the SK-V12 table.

`skinny/RESULTS.md` remains unchanged unless a fresh native JSON guard run
through the existing report generator rewrites it. Any rewrite must be followed
by a separate `gate-json --check-results` using the same Criterion root. A
generic-crate scan pass alone is not a reason to move `RESULTS.md`.

## JSON Parity And Floor Gate

Gate: `G-W1a-GRAMMARCONFIG-LOCK14`.

PASS requires all of:

- `validate_generic_crate_neutrality` passes through `lock14_baseline::validate`.
- JSON generated runtime output and real typed output are byte-clean or
  regenerated with the new file roster, including stale-file rejection if a
  generated `config.rs` is added.
- JSON parity unit tests pass for generated parse, direct digest, typed direct,
  and structural parity.
- SPEC Section 0.5 direct and typed guard floors pass exactly.
- `skinny/RESULTS.md` is byte-exact against the generated report after any
  refresh.
- No CSS/non-JSON row is added to `skinny/RESULTS.md`.
- No directive, BIR variant, `BackendShape`, public substrate API, or IR edit
  lands.
- Generated size facts are recorded in REDRESS 121: hand LOC, generated LOC,
  generated module bytes, `skinny/grammars/json.bbnf` size, and O(N) growth
  status.

Guard floors to enforce:

- Direct: `citm_catalog/direct_to_struct` T1 >= 18191 and T2 >= 17431;
  `apache_builds/direct_to_struct` T1 >= 11028 and T2 >= 9996;
  `marine_ik/direct_to_struct` T1 >= 8759 and T2 >= 9248;
  `unicode_basic/direct_to_struct` T1 >= 2253 and T2 >= 2182.
- Typed: `twitter/real_typed_struct` T1 >= 17385 and T2/oracle >= 15593;
  `citm_catalog/real_typed_struct` T1 >= 29928 and T2/oracle >= 17321;
  `apache_builds/real_typed_struct` T1 >= 8308 and T2/oracle >= 6754;
  `github_events/real_typed_struct` T1 >= 11633 and T2/oracle >= 12029;
  `update_center/real_typed_struct` T1 >= 11613 and T2/oracle >= 10150;
  `mesh/real_typed_struct` T1 >= 9214 and T2/oracle >= 7739;
  `marine_ik/real_typed_struct` T1 >= 11552 and T2/oracle >= 9894.

If the patch touches codegen, generated JSON, runtime generic roots, bench/gate
code, parser/scanner output, or `RESULTS.md`, no-touch proof is insufficient:
refresh JSON guards in an isolated native Criterion root and record
`json_guard_state = refreshed:<run-id>:guards-pass`. Only if no JSON-producing
path moved may REDRESS 121 record `not_refreshed:no_behavior_drift`.

## Verification Commands

Run from `/Users/mkbabb/Programming/bbnf-lang/skinny` unless noted.

```sh
git status --short
```

```sh
cargo test -p codegen
cargo test -p runtime
cargo run -p xtask -- check-json
cargo run -p xtask -- check-real-typed
cargo run -p xtask -- check-conformance
cargo run -p xtask -- lint-loc
```

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench lock14_baseline --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench skv12_non_json_report --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench w1a_non_json_report --lib -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo test -p xtask gate_json_passthrough -- --nocapture
```

```sh
RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --skv12-non-json-report ../restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CRITERION_HOME=/tmp/skv11-open-criterion-3ce75df RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

If any JSON-producing path moved, run the isolated refresh and then the exact
check:

```sh
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- bench-json --advisory
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --with-cost-facts --check-results
```

Final ownership checks:

```sh
git diff --exit-code -- crates/ir/src crates/runtime/src/tape crates/grammar/src
git diff --exit-code -- RESULTS.md
```

If `skinny/RESULTS.md` was intentionally regenerated by `bench-json`, replace
the second command with:

```sh
git diff -- RESULTS.md
CARGO_TARGET_DIR=/tmp/skv12-w1a-json-guard CRITERION_HOME=/tmp/skv12-w1a-criterion RUSTFLAGS="-C target-cpu=native" cargo run -p xtask -- gate-json --advisory --check-results
```

## Same-Wave Consumer

The same-wave consumer is the existing `bbnf-bench --bin gate` path reached by
`cargo run -p xtask -- gate-json ...`. The W1a patch must make the new
generic-crate neutrality scan execute from `lock14_baseline::validate` on the
same gate path that currently protects JSON results and SK-V12 companion
reports.

Unit tests for the scan are necessary but not sufficient. Final evidence must
show `gate-json --check-results` consumed the scan and still preserved
`RESULTS.md` exactness.

## CHALLENGE Risks

CHALLENGE is mandatory for W1a because SPEC classifies it as high risk and
generic-crate touching.

- CH1 Correctness risk: the plan can pass prose neutrality while JSON policy
  remains in reusable emitters. Mitigation: scan negative tests cover every
  leak class, and the final gate consumes the scan.
- CH2 Generality risk: a new `if grammar_name == "css"` route repeats the
  Lock 14 violation. Mitigation: only data-driven profile lookup in generic
  code; grammar literals/policy stay in per-grammar profile modules.
- CH3 Regression risk: report/schema changes could reopen REDRESS 111 or turn
  W1a into a non-JSON admission lane. Mitigation: no `Outcome`,
  `SkV12NonJsonReport`, schema, or `RESULTS.md` movement for the scan.
- CH4 Cost risk: broad template rewrites can exceed the <=360 hand LOC cap.
  Mitigation: parameterize only the JSON policy boundary and gate consumer;
  route CSS generation to W1b.
- CH5 Hidden-coupling risk: `scan.rs` and `sink.rs` are both generated output
  and template input today. Mitigation: either move their source ownership to
  codegen templates or record them explicitly as JSON-owned template inputs and
  add stale-file/file-roster checks.
- CH6 Next-wave risk: W1b could treat W1a as CSS proof. Mitigation: REDRESS
  121 and the gate label state legality only; W1b must still create the CSS L4
  generated row, strict oracle, and lightningcss comparator.

## REDRESS 121 Accounting

On PASS, append `skinny/REDRESS.md` Item 121 under
`G-W1a-GRAMMARCONFIG-LOCK14` with:

- the seven leaks resolved;
- command evidence for Lock 14 scan, JSON regen/parity, JSON guard floors,
  `RESULTS.md` exactness, and no public substrate/IR expansion;
- generated-size facts;
- `json_guard_state = refreshed:<run-id>:guards-pass` or
  `not_refreshed:no_behavior_drift`;
- explicit statement that no CSS parser row, lightningcss comparator result,
  CSS SOTA claim, SK-V12 close, Sheets fallback, or BBNF-self fallback was
  admitted.

On FAIL/BLOCKED/REJECTED, append Item 121 as the rejection/blocker with:

- exact failed command and concise output summary;
- failed evidence class: compile, Lock 14 scan, JSON parity, JSON guard floor,
  stale/no-touch proof, generated CSS metadata proof, or public surface drift;
- rejected patch path `/tmp/skv12-waveW1a-rejected.patch`;
- confirmation that only the W1a slice was reverted and unrelated user or
  parallel-agent edits were not reverted;
- routed remainder back to S-P3/W1a revision before W1b-1 can dispatch.

## Revert Protocol

Before reverting a failed implementation, save only the W1a-owned candidate
slice:

```sh
git diff --binary HEAD -- \
  skinny/crates/codegen \
  skinny/crates/runtime \
  skinny/crates/ir \
  skinny/crates/bbnf-bench \
  skinny/xtask \
  skinny/RESULTS.md \
  > /tmp/skv12-waveW1a-rejected.patch
```

Inspect the patch before reversal. If unrelated user or parallel-agent edits are
present, split them out or stop for human routing; do not use `git reset --hard`
or broad checkout commands.

Revert only W1a generic/template/config changes, generated JSON/typed output,
gate/report changes, and any generated `RESULTS.md` movement caused by the
failed candidate. Then record REDRESS 121 with exact failure evidence and the
routed remainder.
