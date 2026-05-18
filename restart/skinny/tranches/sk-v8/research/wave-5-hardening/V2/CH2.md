# SK-V8 W5 Hardening V2 CH2 - Generality

Target: `6e159f5c70aa5b4560d874a0e446587beb8f857e`.

Verdict: ACCEPT.

Confidence: 94%.

## Findings

1. No blocking CH2 generality issue remains from the V1 fold. The live V1
   finding was that `skinny/crates/codegen/src/lib.rs` owned the JSON runtime
   profile guard and JSON template/runtime includes while Lock 14 classified
   that file as `generic_surface`. Target `6e159f5c` moves that policy into
   `skinny/crates/codegen/src/json_provider.rs` and leaves `lib.rs` with
   provider delegation only.
2. The new provider boundary is explicitly represented in Lock 14:
   `json_provider.rs` is allowlisted as `per_grammar_provider`, the class is
   admitted by `is_allowed_class`, and parent-diff authorization is limited to
   `crates/codegen/src/lib.rs` plus `crates/codegen/src/json_provider.rs`
   under a `sk-v8-wave5` subject.
3. The grammar-name/provider-residency proof is now adequate for CH2. The
   read-only generic codegen scan excluding `json_provider.rs` and
   `json_templates/**` returned no matches for `grammar_name == "json"`,
   `backend.grammar_name`, JSON template includes, or
   `runtime/src/grammars/json` includes. The broader residency scan returns
   only `json_provider.rs` and `xtask` generated-output tooling paths, plus
   codegen tests that call the JSON grammar fixture.
4. The fold does not introduce a new generic JSON policy under a neutral name.
   `lib.rs` still invokes a JSON-named provider from the current emit path, so
   the accepted claim must stay narrow: W5 isolated the current JSON provider
   boundary; it did not create or prove a full multi-grammar provider registry.
   That caveat is already compatible with the W5 plan because the required
   V1 fold was provider isolation/classification, not a redesign.
5. REDRESS 36-38 are not reopened by this target. `skinny/REDRESS.md` is
   unchanged by the target, REDRESS 85 and 86 remain the later admitted Lock 14
   neutralization records, and the focused forbidden-symbol scan returned no
   matches for the old or renamed policy surfaces listed in the W5 plan.
6. The CSS L4, Sheets, and BBNF-self posture is acceptable for CH2. The target
   edits generic `codegen/src/lib.rs`, but only to remove resident JSON policy
   and call the per-grammar provider. The W5 packet records root
   `cargo xtask regen --check` as clean across 9 of 9 grammars, including
   CSS L4, Google Sheets, and BBNF-self; read-only diffs show no generated
   output, `RESULTS.md`, runtime, passes, IR, SIMD, bbnf, or xtask drift in
   the target slice.

## Verification/Evidence

- Read: `restart/skinny/tranches/sk-v8/research/skv8-W5-plan.md`,
  `restart/skinny/tranches/sk-v8/research/skv8-W5-lock14-audit-research.md`,
  `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V1/HARDENING-W5-V1-CONSOLIDATED.md`,
  `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V1/CH2.md`,
  `restart/skinny/tranches/sk-v8/SPEC.md`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md`, `skinny/REDRESS.md`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
  `skinny/crates/codegen/src/lib.rs`, and
  `skinny/crates/codegen/src/json_provider.rs`.
- `git rev-parse HEAD` resolves to target `6e159f5c70aa5b4560d874a0e446587beb8f857e`.
- `git diff --stat 6e159f5c^ 6e159f5c -- skinny/crates/codegen/src/lib.rs skinny/crates/codegen/src/json_provider.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs`
  shows the intended three-file source fold only. `git diff --numstat` reports
  148 source insertions across those three files, inside the W5 named-cleanup
  insertion cap recorded in the plan.
- `skinny/crates/codegen/src/json_provider.rs:4-13` owns the only
  `backend.grammar_name == "json"` guard in codegen. Lines 48-73 own the
  JSON template/runtime includes.
- `skinny/crates/codegen/src/lib.rs:108-135` delegates generated runtime
  material to `json_provider`; the previous in-file JSON guard, include
  helpers, and `normalize` helper were deleted by the target diff.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:188-193`,
  `:411-414`, `:477-485`, and `:562-575` establish the
  `per_grammar_provider` class and W5-only parent-diff allowance.
- Read-only scan:
  `rg -n 'grammar_name == "json"|backend\.grammar_name|include_str!\("json_templates|runtime/src/grammars/json' skinny/crates/codegen/src -g '*.rs' --glob '!json_provider.rs' --glob '!json_templates/**'`
  returned no matches.
- Read-only scan:
  `rg -n 'emit_from_source\("json"\)|runtime/src/grammars/json' skinny/crates/codegen/src skinny/xtask/src -g '*.rs'`
  returned only `json_provider.rs` includes and `xtask` generated-output
  tooling paths.
- Read-only forbidden-symbol scan from the W5 plan returned no matches for
  `StrictJson`, `StrictJsonTrustedUtf8`, `JsonStringMatch`,
  `JsonNumberMatch`, `skip_json`, `match_json`, `unescape_json`,
  `shapes_for_json`, `nominate_json`, `materialization_for_rule`,
  `descriptor_for_rule`, `rule_by_name("json")`, `MissingEntry("json")`,
  `StructuralAlphabet::json`, `UnionTape`, `union_tape`,
  `BackendShape::Union`, or `BackendShape::Json` in the audited generic
  roots after excluding generated JSON output and JSON templates.
- I did not run cargo tests or regen locally because this assignment restricts
  writes to this single CH2 markdown file; those commands can create build
  artifacts. I relied on the target packet's recorded command evidence for the
  cargo/regen results and used only read-only scans/diffs in this review.

## Required Folds

None for CH2.

This ACCEPT is not W6 dispatch authority. W5 still needs the remaining V2
challenge packet and the second qualifying ACCEPT cycle required by the W5
plan before W5 can close.
