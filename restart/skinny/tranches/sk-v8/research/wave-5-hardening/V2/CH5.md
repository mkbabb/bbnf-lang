# SK-V8 W5 Hardening V2 CH5 - Hidden Coupling

Date: 2026-05-18.

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e`
(`fix(sk-v8-wave5-lock14): isolate json provider boundary after V1 revise`).

Verdict: ACCEPT

Confidence: 94%.

## Findings

1. No new directive, BIR, or substrate surface is introduced. The target source
   fold is confined to `skinny/crates/codegen/src/lib.rs`,
   `skinny/crates/codegen/src/json_provider.rs`, and
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs`; there is no source diff
   under `skinny/crates/ir/src`, `skinny/crates/passes/src`, runtime generic
   surfaces, `skinny/xtask/src`, generated output, or `skinny/RESULTS.md`.
2. No `UnionTape` or `BackendShape` drift is present. `BackendShape` remains the
   five Lock 14 variants: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`,
   and `CollapsedStage`. `lock14_baseline` still enforces the five-variant
   count and rejects `UnionTape` / `union_tape` in the IR surface.
3. No sidecar/substrate relabeling is hidden in the fold. The new
   `per_grammar_provider` class names a provider boundary for the JSON runtime
   profile guard and JSON template/runtime includes; it does not add a sidecar,
   substrate, directive, or alternate parser production route.
4. No hidden Track 1 / Track 2 coupling is introduced. The target does not
   touch generated JSON runtime output, generated typed output,
   `direct_struct.rs`, direct guard source, or `skinny/RESULTS.md`. The provider
   fold changes where codegen reads JSON provider material from, not the
   generated Track 1 artifact or the independent Track 2 authority.
5. The provider boundary does not leave generic JSON policy behind in
   `codegen/src/lib.rs`. The generic codegen file now holds only a private
   module handoff (`json_provider::...`); the JSON grammar-name guard and JSON
   template/runtime path knowledge live inside `json_provider.rs`, which is
   classified as `per_grammar_provider` in Lock 14.
6. The W5 parent-diff authorization is narrow enough for CH5. It admits only
   `crates/codegen/src/lib.rs` and `crates/codegen/src/json_provider.rs` when
   the commit subject carries `sk-v8-wave5`; it does not authorize a substrate,
   BIR, generated-output, or W6 route.

## Verification/Evidence

- `git show --name-status --format=fuller 6e159f5c` shows the target commit and
  changed paths. Non-document source changes are limited to
  `skinny/crates/codegen/src/lib.rs`,
  `skinny/crates/codegen/src/json_provider.rs`, and
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`.
- `skinny/crates/codegen/src/lib.rs:102-146` delegates runtime-profile and file
  material to private `json_provider` functions; `skinny/crates/codegen/src/json_provider.rs:4-73`
  contains the JSON guard and JSON template/runtime includes.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:189-193` classifies
  `crates/codegen/src/json_provider.rs` as `per_grammar_provider`;
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:411-414` and
  `:477-485` scope the W5 parent-diff allowance to the codegen handoff and
  provider file only.
- `skinny/crates/ir/src/lib.rs:392-408` shows unchanged `BackendIr` fields and
  the five `BackendShape` variants; `lock14_baseline.rs:535-558` enforces the
  five-variant count and `UnionTape` absence.
- Bounded forbidden-symbol scan returned no matches:
  `StrictJson`, `StrictJsonTrustedUtf8`, `JsonStringMatch`, `JsonNumberMatch`,
  `skip_json`, `match_json`, `unescape_json`, `StructuralAlphabet::json`,
  `UnionTape`, `BackendShape::Union`, and `BackendShape::Json`.
- Generic provider-policy scan returned no matches outside
  `json_provider.rs` and `json_templates/**` for `grammar_name == "json"`,
  `backend.grammar_name`, `include_str!("json_templates`, and
  `runtime/src/grammars/json`.
- Provider-residency scan returned only
  `skinny/crates/codegen/src/json_provider.rs` plus existing
  `skinny/xtask/src/main.rs` generated-output tooling references.
- `rg -n "sidecar|Sidecar|substrate|Substrate"` over codegen, IR, passes, and
  `lock14_baseline.rs` returned no matches.
- `git diff 6e159f5c^ 6e159f5c --stat -- skinny/RESULTS.md
  skinny/crates/runtime/src/grammars/json skinny/crates/codegen/src/json_templates
  skinny/crates/bbnf-bench/src/generated_real_typed.rs
  skinny/crates/bbnf-bench/src/direct_struct.rs skinny/crates/ir/src
  skinny/crates/passes/src skinny/crates/parse-that-regex/src
  skinny/crates/bbnf-simd/src skinny/crates/runtime/src skinny/crates/bbnf/src
  skinny/xtask/src` returned no output.
- `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md:128-131` still
  requires another unchanged qualifying challenge cycle after V2; this CH5
  result is not W5 close and does not dispatch W6.

## Required Folds

None for CH5.

Carry-forward constraint: do not dispatch W6 from this CH5 ACCEPT.
