# SK-V12 W1a CH5 Hidden Coupling

Date: 2026-05-20.
Scope: CH5 review of W1a hidden coupling in scan/sink template ownership,
generated file roster, provider selection, hooks, gate freeze, and typed direct
renderer.
Output: this file.

## Disposition

CH5 disposition: REVISE.

The W1a route is directionally legal: it keeps W1a as a legality gate, does not
admit CSS, uses a codegen-private provider/profile boundary, preserves JSON
guard treatment, and routes the Lock 14 scan through the existing gate path.
However, the plan is not yet hidden-coupling-clean because two ownership seams
remain under-specified:

1. `scan.rs` and `sink.rs` are still both checked-in generated output and
   `include_str!` template input. The plan names the risk but does not choose a
   single executable ownership rule.
2. `typed_direct.rs` remains optional in the plan even though it is a generic
   codegen root today and contains JSON object/string/number policy. If it is
   left generic, the W1a Lock 14 scan either misses a live leak or fails only
   after redress.

These are fixable plan defects, so CH5 returns REVISE rather than REJECT.

## Authority

- CHALLENGE is mandatory for W1a, and REJECT or unresolved REVISE returns to
  plan (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:113-118`;
  `restart/skinny/tranches/sk-v12/SPEC.md:250-253`).
- SPEC Section 2.1 forbids JSON grammar policy, `OffsetFlags` meaning, and
  `JsonSink` shape in generic code, and makes per-grammar generated modules own
  structural alphabets, FIRST/follow, escape policy, number policy, flag
  semantics, sink/view/kind wrappers, and output facts
  (`restart/skinny/tranches/sk-v12/SPEC.md:259-275`).
- SPEC Section 4 requires W1a to introduce `GrammarConfig` or equivalent
  generated metadata, move JSON policy out of generic code, add a Lock 14
  scan/gate consumer, preserve JSON parity and guard floors, claim no CSS parser
  row, and add no directive/BIR/`BackendShape`/public substrate API
  (`restart/skinny/tranches/sk-v12/SPEC.md:314-349`).

## Findings

### CH5-1 - REVISE: scan/sink template ownership is still ambiguous

Current source evidence:

- `codegen::emit_with_layout` emits `scan.rs` and `sink.rs` as generated JSON
  runtime files (`skinny/crates/codegen/src/lib.rs:127-135`).
- `json_provider::scan_rs()` and `json_provider::sink_rs()` read those same
  checked-in generated files through `include_str!`
  (`skinny/crates/codegen/src/json_provider.rs:56-61`).
- The files themselves carry generated/source-policy content:
  `scan.rs` has a generated header plus JSON structural bytes
  (`skinny/crates/runtime/src/grammars/json/scan.rs:1-7`), and `sink.rs` defines
  the JSON sink trait (`skinny/crates/runtime/src/grammars/json/sink.rs:1-14`).

Plan/research evidence:

- A4 correctly identifies this as a provenance problem: a hand edit to
  `scan.rs` or `sink.rs` can become the next expected regen output because those
  files are both generated output and provider template input
  (`restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md:21-24`).
- The consolidated research carries the same warning
  (`restart/skinny/tranches/sk-v12/research/w1a/CONSOLIDATED.md:20-23`).
- PLAN-P2 lists the CH5 risk but leaves the resolution as "either move" the
  source ownership or "record them explicitly" with roster checks
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:335-338`).

Judgment: REVISE. A CH5-clean plan must choose one executable ownership rule.
The preferred fix is to move `scan.rs` and `sink.rs` template inputs under
`skinny/crates/codegen/src/json_templates/` or another JSON-owned source root,
then treat `skinny/crates/runtime/src/grammars/json/scan.rs` and `sink.rs` only
as generated outputs. If the plan intentionally keeps them as template inputs,
it must stop calling them generated output and define a separate check proving
no stale generated twin can survive.

### CH5-2 - ACCEPT: generated file roster is sufficiently named, pending CH5-1

The active plan names the generated JSON output roster, including the new
`config.rs`, the existing JSON runtime modules, and the typed generated module
only if typed output changes
(`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:30-43`).
It also requires exact runtime JSON roster checking without making
`EmittedSource::check_dir` globally exact, because `check-real-typed` targets a
source directory with unrelated sibling files
(`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:140-147`).

This closes the stale-extra-file class once CH5-1 chooses real ownership for
`scan.rs` and `sink.rs`. Current code confirms why exactness must be scoped:
`EmittedSource::check_dir` checks expected files only and does not reject extra
files (`skinny/crates/codegen/src/lib.rs:55-66`), while real typed output is
checked into `crates/bbnf-bench/src` (`skinny/xtask/src/main.rs:136-153`).

### CH5-3 - ACCEPT: provider selection is legal if kept data-driven

The plan replaces the current JSON-only choke point with a provider/profile
selector. The selector may compare provider identity to `backend.grammar_name`,
but grammar-specific literals and policy stay in provider-owned modules
(`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:55-66`;
`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:46-90`).

That is a legal boundary because the current illegal shape is a literal JSON
branch in `json_provider::ensure_runtime_profile`
(`skinny/crates/codegen/src/json_provider.rs:4-12`), called by both runtime and
typed emission (`skinny/crates/codegen/src/lib.rs:102-108`, `:139-147`). A
data-driven lookup that does not embed JSON/CSS/Sheets policy in generic code is
not the same hidden coupling as `grammar_name == "json"` plus a JSON-only
renderer.

### CH5-4 - ACCEPT: hooks are not treated as the W1a authority

The local pre-commit hook runs root `cargo xtask regen --check --staged`
(`.git/hooks/pre-commit:22-24`), and CI runs root `cargo xtask regen --check`
(`.github/workflows/ci.yml:55-58`). A4 correctly says this does not replace
skinny `check-json` / `check-real-typed`
(`restart/skinny/tranches/sk-v12/research/w1a/A4-regen-json-parity.md:173-175`).

The plan does not rely on hooks as proof. It requires explicit skinny commands:
`check-json`, `check-real-typed`, `check-conformance`, Lock 14 tests, and the
native JSON guard refresh/check sequence
(`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:150-182`;
`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:252-289`).
No hook hidden coupling blocks CH5.

### CH5-5 - ACCEPT: gate freeze coupling is recognized and bounded

Current gate evidence:

- `bbnf-bench --bin gate` calls `lock14_baseline::validate(&workspace)` before
  companion report or JSON report processing
  (`skinny/crates/bbnf-bench/src/bin/gate.rs:37-54`).
- The current validator checks allowlist entries, frozen git state, and backend
  shape surface (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:342-347`).
- The frozen roots include runtime, IR, passes, codegen, grammar, SIMD, typed
  generated output, Track 2/parity/direct paths, and real typed schema
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:381-403`).
- Parent-diff authorization currently names only old `sk-v8` and `sk-v10`
  scopes, so unmodified W1a would fail after a codegen/runtime redress commit
  (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:481-525`).

Plan evidence:

- PLAN-P1 requires a W1a-aware owner-delta validation before the generic
  neutrality scan and backend shape check
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P1-grammar-profile.md:148-159`).
- PLAN-P2 requires the same gate path to consume the scan and says unit tests are
  insufficient unless `gate-json --check-results` consumes it
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:306-316`).

Judgment: ACCEPT. The plan names the freeze/gate coupling and makes owner-delta
validation part of the same gate consumer. CH5 does not require a revision here,
but the redress must implement this before using `gate-json` as final evidence.

### CH5-6 - REVISE: typed direct containment is optional but must be mandatory

Current source evidence:

- `typed_direct.rs` is a generic codegen module today
  (`skinny/crates/codegen/src/lib.rs:1-5`) and is used by typed emission
  (`skinny/crates/codegen/src/lib.rs:139-167`).
- It emits JSON parser policy directly: JSON string/number helpers
  (`skinny/crates/codegen/src/typed_direct.rs:25-29`), object braces, quoted keys,
  colon separators, comma handling, and unknown-field behavior
  (`skinny/crates/codegen/src/typed_direct.rs:79-116`), JSON string matching
  (`skinny/crates/codegen/src/typed_direct.rs:479-502`), JSON number matching
  (`skinny/crates/codegen/src/typed_direct.rs:559-568`), and JSON value skipping
  for `{`, `[`, `"`, `-`, digits, `true`, `false`, and `null`
  (`skinny/crates/codegen/src/typed_direct.rs:570-584`).

Plan/research evidence:

- A5 already flags typed direct as JSON-structured and says that if W1a touches
  typed-direct legality, it should parameterize the renderer through metadata
  rather than add IR variants
  (`restart/skinny/tranches/sk-v12/research/w1a/A5-ir-metadata-boundary.md:189-194`).
- The main plan makes `json_typed_direct.rs` conditional "if typed renderer
  containment is needed" and allows `typed_direct.rs` only to move JSON-owned
  logic or leave thin wrappers
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN.md:27-30`).
- PLAN-P2 similarly says `typed_direct.rs` is touched "only if typed output needs
  the same profile boundary"
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:83-85`).
- But PLAN-P2's own scan policy scans `crates/codegen/src/` while excluding
  `json_provider.rs`, `json_templates/`, and `grammar_profiles/json.rs`; it does
  not exclude `typed_direct.rs`
  (`restart/skinny/tranches/sk-v12/research/w1a/PLAN-P2-lock14-gate.md:136-183`).

Judgment: REVISE. Typed direct containment is not optional under CH5. Either
move/rename the renderer into a JSON-owned root and exclude that root from the
generic scan, or parameterize it with `GrammarProfile` so the generic file no
longer contains JSON policy. Leaving it as a generic JSON renderer would violate
SPEC Section 2.1 and make W1a's typed guard path a hidden JSON coupling.

## Required Plan Revisions

1. Choose one scan/sink ownership model. Preferred: move the source templates for
   JSON scanner and sink into a JSON-owned codegen template/provider root, then
   keep runtime `scan.rs` and `sink.rs` as generated output only. Update owner
   paths, generated roster, and checks accordingly.
2. Make typed direct containment mandatory. The plan must either create
   `json_typed_direct.rs` / `grammar_profiles/json.rs` ownership and exclude it
   from generic scan roots, or make `typed_direct.rs` genuinely grammar-neutral.
3. Align the generic scan root list across `PLAN.md`, PLAN-P1, and PLAN-P2. The
   final plan must state exactly whether `sink_direct.rs` and `typed_direct.rs`
   are generic scan roots or JSON-owned excluded roots.
4. Preserve the existing ACCEPTED constraints: data-driven provider selection,
   scoped exact generated roster checking, explicit skinny regen/parity commands,
   W1a-aware Lock 14 owner-delta/freeze handling, refreshed JSON guard treatment,
   no CSS row, no report outcome/schema churn, and no public substrate/IR
   expansion.

After these revisions, CH5 should be able to ACCEPT.
