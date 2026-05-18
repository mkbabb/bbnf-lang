# SK-V8 W1 Research D: Lock 14 And Non-JSON Proof For CostFacts

Date: 2026-05-18.
Scope: W1 CostFacts gate binding; identify likely and avoidable generic-crate edits, scans/tests proving no JSON policy enters generic CostFacts paths, and CSS L4 / Sheets / BBNF-self proof required if W1 touches generic code.
Output: `restart/skinny/tranches/sk-v8/research/skv8-W1-d-lock14-nonjson.md`.

## Section 1 - Findings

W1 is a gate-binding wave, not a CostFacts substrate wave. The W0 V12 consolidation says W0 is closed and W1 may dispatch, while explicitly routing the residual `none:pre-W1` CostFacts sentinel replacement to W1 before behavior waves may cite route quality (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:19`, `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:65-68`, `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:82-84`). HANDOFF gives W1 a `0 parser/generated behavior LOC; <=300 CostFacts/report/gate/test LOC` budget and requires `gate-json --with-cost-facts` to become the same-wave consumer that rejects missing evidence after W1 (`restart/skinny/tranches/sk-v8/HANDOFF.md:127-137`, `restart/skinny/tranches/sk-v8/HANDOFF.md:174-181`).

The likely W1 edit slice is bench/report/gate plus skinny xtask dispatch. `SkV8Telemetry` already has `costfacts_rule_id`, `costfacts_chosen_shape`, `costfacts_rejected_alternative_ids`, `redress_entry`, and `wave_id` fields (`skinny/crates/bbnf-bench/src/report.rs:43-68`). W0 validation still requires the pre-W1 sentinels (`skinny/crates/bbnf-bench/src/report.rs:1007-1013`), and the report renderer already has one compact CostFacts manifest cell (`skinny/crates/bbnf-bench/src/report.rs:575-609`). The gate binary currently fills `none:pre-W1` in every row (`skinny/crates/bbnf-bench/src/bin/gate.rs:474-498`) and validates only `validate_sk_v8_w0()` before writing/checking `RESULTS.md` (`skinny/crates/bbnf-bench/src/bin/gate.rs:319-339`). `skinny/xtask` already recognizes `--with-cost-facts`, but today it prints a standalone `sk-v7-costfacts-v1` JSON snapshot instead of binding that evidence into the W0/W1 report gate (`skinny/xtask/src/main.rs:240-305`).

The generic CostFacts producer is already present and should be treated as avoidable unless W1 defines "evidence" more strictly than the existing schema. `ir::cost` defines grammar-neutral `CostFacts`, `BackendShape`, rejected alternatives, `EvidenceSource`, measurements, capacity policy, and the fixed backend-shape set without JSON names (`skinny/crates/ir/src/cost.rs:5-13`, `skinny/crates/ir/src/cost.rs:111-135`). `passes::compile()` already populates `LayoutFacts.cost_facts` and asserts `CostFacts.chosen` matches `backend_shape` (`skinny/crates/passes/src/lib.rs:28-55`). The recognizer pass creates one `CostFacts` per rule, one rejected alternative per non-selected shape, diagnostics for missing measurement-backed evidence, and REDRESS-backed evidence where present (`skinny/crates/passes/src/lib.rs:390-441`, `skinny/crates/passes/src/lib.rs:527-626`). The focused unit test passed in this research:

```sh
cd skinny
cargo test -p passes cost_facts -- --nocapture
# result: 1 passed; 0 failed
```

`codegen` already exposes a CostFacts snapshot without running runtime emission: `cost_facts_from_source()` parses, compiles, maps `layout_facts.cost_facts`, and returns diagnostics (`skinny/crates/codegen/src/lib.rs:222-248`). It also threads CostFacts through `LowerCtx` and selects lowerers from `cost.chosen` (`skinny/crates/codegen/src/lower/rust.rs:20-74`, `skinny/crates/codegen/src/lower/mod.rs:17-24`). W1 should not need to edit these paths unless the W1 plan requires new top-level evidence fields inside `CostFacts`.

The current snapshot proves producer availability but not W1 gate binding. This command produced parseable JSON with schema `sk-v7-costfacts-v1`, grammar `json`, 15 CostFacts entries, four rejected alternatives per entry, and diagnostic codes `BBNF-COSTFACTS-MISSING-EVIDENCE` plus `BBNF-DOMINATED-ALTERNATIVE`:

```sh
cd skinny
cargo xtask gate-json --with-cost-facts --advisory >/tmp/skv8-costfacts.json
jq -r '.schema, .grammar, (.cost_facts | length), ([.cost_facts[].rejected | length] | unique | @csv), ([.diagnostics[].code] | unique | @csv)' /tmp/skv8-costfacts.json
```

The key ambiguity for W1 is "missing evidence." Current W9-era diagnostics intentionally report rules without measurement-backed evidence while keeping them non-fatal (`skinny/REDRESS.md:2466-2506`). If W1 means "missing CostFacts entry/sentinel", no generic edit is needed: bind existing `CostFacts` into the report and fail on `none:pre-W1`. If W1 means "every chosen/rejected decision must carry explicit measurement evidence", the current snapshot will fail by design and W1 would need a generic producer/schema edit. That route is higher risk and should be split or challenged because SPEC blocks producer-only CostFacts and generic JSON policy (`restart/skinny/tranches/sk-v8/SPEC.md:396-429`).

The scoped generic CostFacts JSON-policy scans are clean except for a pre-existing JSON runtime-profile guard outside the CostFacts producer. These commands returned no matches:

```sh
cd skinny
rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' \
  crates/ir/src/cost.rs crates/codegen/src/lower -S

sed -n '390,626p' crates/passes/src/lib.rs | \
  rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' -S
```

This broader scan found one pre-existing boundary hit:

```sh
cd skinny
rg -n '(StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json|grammar_name\s*==\s*"json"|==\s*"json"|contains\("json"\)|Json[A-Za-z_]*Cost|costfacts_.*json)' \
  crates/ir/src/cost.rs crates/passes/src/lib.rs crates/codegen/src/lower crates/codegen/src/lib.rs \
  crates/bbnf-bench/src/report.rs crates/bbnf-bench/src/bin/gate.rs xtask/src/main.rs -S
# crates/codegen/src/lib.rs:170: if backend.grammar_name == "json" {
```

That `ensure_runtime_profile()` guard is a JSON runtime-emission limitation (`skinny/crates/codegen/src/lib.rs:169-177`), not a CostFacts producer branch. W1 should avoid touching it. If W1 edits `codegen`, the plan must explain why this existing JSON-only surface remains per-grammar/template bounded rather than becoming a generic policy precedent.

Lock 14 proof is not optional when generic crates move. SPEC Section 2.1 requires public API, grammar-branch, primitive/table, role/fact boundary, template/provider boundary, and non-JSON proof checks for any generic CostFacts, codegen, runtime, SIMD, or parser-template edit (`restart/skinny/tranches/sk-v8/SPEC.md:261-286`). W5 repeats the audit and blocks generic JSON public APIs, grammar-name branches, `StructuralAlphabet::json`, `skip_json`, `match_json`, `unescape_json`, `StrictJson`, renamed JSON helpers, and performance claims from cleanup (`restart/skinny/tranches/sk-v8/SPEC.md:652-700`). Section 10 blocks generic JSON policy, sidecar/permissive evidence as strict admission, telemetry-only consumers, Track 1/Track 2 coupling, and the REDRESS 36-38/85-86 Lock 14 residues (`restart/skinny/tranches/sk-v8/SPEC.md:756-801`).

The Lock 14 baseline freezes generic skinny roots and generated/runtime behavior surfaces. It names generic `ir`, `passes`, `codegen`, `grammar`, `bbnf`, SIMD, parse-that-regex, Track 2, parity, scan, materialization, and host schema paths as frozen roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:375-397`), and it rejects `BackendShape` drift or `UnionTape` in the IR surface (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:462-490`). This means W1 generic edits are not just "more tests"; they are Lock 14 events.

The non-JSON proof surface lives in the root workspace, not the skinny workspace alone. The root grammar manifest enumerates `bbnf`, `json`, `css_l4`, `google_sheets`, and other grammars, plus strategy rows for CSS L4, Sheets, and BBNF runtime builders/documents (`Cargo.toml:18-56`). The generated surface exposes `bbnf`, `css_l4`, `google_sheets`, and `json` modules (`crates/core/src/grammar/generated/mod.rs:1-35`). CSS L4 full-pipeline lower/registry proof exists in `project_types_css_l4` (`crates/core/tests/project_types_css_l4.rs:1-29`, `crates/core/tests/project_types_css_l4.rs:56-70`, `crates/core/tests/project_types_css_l4.rs:246-345`). Runtime projection proof covers JSON, CSS L4, Sheets, and BBNF in one test (`crates/core/tests/projection_totality.rs:84-224`), and typed accessor proof covers CSS L4, Sheets, BBNF, and per-grammar document-owned projections (`crates/core/tests/typed_accessor_surface.rs:560-642`, `crates/core/tests/typed_accessor_surface.rs:737-795`).

Two root non-JSON smoke tests were executed during this research and passed:

```sh
cargo test -p bbnf --test projection_totality struct_direct_documents_have_concrete_roots -- --nocapture
cargo test -p bbnf --test typed_accessor_surface struct_direct_document_projection_surface_per_grammar -- --nocapture
# result: both passed; existing generated-code warnings only
```

## Section 2 - Recommendations

Prefer a report/gate-only W1 plan:

```text
Likely owner files:
skinny/crates/bbnf-bench/src/report.rs
skinny/crates/bbnf-bench/src/bin/gate.rs
skinny/crates/bbnf-bench/src/gate.rs only for focused strict/comparator tests if needed
skinny/xtask/src/main.rs
skinny/RESULTS.md after the W1 gate refresh, if the plan admits a report update
skinny/REDRESS.md only if W1 rejects
```

Avoid editing these generic producers unless W1 explicitly changes the CostFacts schema:

```text
skinny/crates/ir/src/cost.rs
skinny/crates/passes/src/lib.rs
skinny/crates/codegen/src/lower/*
skinny/crates/codegen/src/lib.rs except as a read-only snapshot producer
skinny/crates/runtime/*
generated runtime output
root crates/core generated outputs
```

Bind W1 as a consumer, not another producer. The gate should call or reuse `codegen::cost_facts_from_source("json", include_str/grammar source)` and populate each SK-V8 row with non-sentinel CostFacts fields. The W1 report should add explicit evidence-source accounting without forcing a generic schema change: derive `StaticAnalysis` from `rationale`/`priority_fired`, `RedressBackfill` from `rejected[].evidence[].source`, and capacity-policy evidence from `capacity_policy`. If the plan requires a new manifest column for evidence source, add it to `bbnf-bench` report/gate, not to generic `ir::CostFacts`, unless a challenge accepts a generic schema amendment.

Make the W1 falsifier executable:

```sh
cd skinny
cargo xtask gate-json --with-cost-facts --advisory
# must fail if any row still has:
#   costfacts_rule_id == none:pre-W1
#   costfacts_chosen_shape == none:pre-W1
#   costfacts_rejected_alternative_ids == [none:pre-W1]
# or if the CostFacts snapshot is missing the materialized JSON rule id.
```

Keep W0 validation intact. `validate_w0_manifest_semantics()` currently rejects non-sentinel CostFacts for the `SK-V8-open` baseline (`skinny/crates/bbnf-bench/src/report.rs:1007-1013`). W1 should add a W1-specific validation mode or flag path, not silently relax W0. The same gate should continue to reject strict admission on plane/strictness/freshness/measured-path mismatch through the existing strict-admission logic (`skinny/crates/bbnf-bench/src/gate.rs:135-175`).

Use this minimum W1 no-JSON policy proof when W1 stays report/gate-only:

```sh
cd skinny
cargo test -p passes cost_facts -- --nocapture
cargo xtask gate-json --with-cost-facts --advisory >/tmp/skv8-costfacts.json
jq -e '.grammar == "json" and (.cost_facts | length) == 15 and all(.cost_facts[]; (.rejected | length) >= 4)' /tmp/skv8-costfacts.json

rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' \
  crates/ir/src/cost.rs crates/codegen/src/lower -S

sed -n '390,626p' crates/passes/src/lib.rs | \
  rg -n '\b(Json|json|JSON|serde_json|object|array|pair|field|StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json)\b' -S

rg -n '(StrictJson|skip_json|match_json|unescape_json|StructuralAlphabet::json|Json[A-Za-z_]*Cost|costfacts_.*json)' \
  crates/ir/src/cost.rs crates/passes/src/lib.rs crates/codegen/src/lower crates/codegen/src/lib.rs \
  crates/bbnf-bench/src/report.rs crates/bbnf-bench/src/bin/gate.rs xtask/src/main.rs -S

git diff --exit-code -- \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/RESULTS.md
```

If W1 touches generic `ir`, `passes`, `codegen`, runtime, SIMD, parser-template, or root compiler code, add this Lock 14 package:

```sh
# Public API scan.
rg -n 'pub (struct|enum|fn|type|trait|const).*(Json|JSON|StrictJson|skip_json|match_json|unescape_json)|StructuralAlphabet::json' \
  skinny/crates/{ir,passes,codegen,grammar,runtime,bbnf-simd}/src crates/{core,ir}/src -S

# Grammar branch / role scan. Review every hit; no generic selector may branch on JSON names or JSON roles.
rg -n 'grammar_name\s*==\s*"json"|rule_by_name\("json"\)|MissingEntry\("json"\)|shapes_for_json|nominate_json|materialization_for_rule|descriptor_for_rule|object role|array role|string role|field name|layout role' \
  skinny/crates/{ir,passes,codegen}/src crates/core/src/backend crates/ir/src -S

# Primitive/table scan.
rg -n 'JSON_STRUCTURAL|STRUCTURAL_ALPHABET_JSON|is_json_punctuation|scan_json_tail|JsonParseIndex|resolve_json_string_masks_64|StructuralAlphabet::json|skip_json|match_json|unescape_json|StrictJson' \
  skinny/crates/bbnf-simd/src skinny/crates/{ir,passes,codegen}/src crates/core/src/backend crates/ir/src -S

# Template/provider boundary and generated-output drift.
rg -n 'json_templates|grammar_name\s*==\s*"json"|generated_json|runtime/src/grammars/json|JsonParser|JsonGrammar' \
  skinny/crates/codegen/src skinny/xtask/src crates/core/src/backend/rust -S
git diff --exit-code -- \
  skinny/crates/runtime/src/grammars/json \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  crates/core/src/grammar/generated/json.rs \
  crates/core/src/grammar/generated/css_l4.rs \
  crates/core/src/grammar/generated/google_sheets.rs \
  crates/core/src/grammar/generated/bbnf.rs \
  crates/core/src/grammar/generated/*.registry.json

# Non-JSON compile/lower/run proof.
cargo test -p bbnf --test projection_totality struct_direct_documents_have_concrete_roots -- --nocapture
cargo test -p bbnf --test typed_accessor_surface struct_direct_document_projection_surface_per_grammar -- --nocapture
cargo test -p bbnf --test project_types_css_l4 audit_pass_reports_mapped_for_every_css_l4_marker -- --nocapture
cargo xtask regen --check --grammar css_l4
cargo xtask regen --check --grammar google_sheets
cargo xtask regen --check --grammar bbnf
```

If the generic edit is only in skinny and the root workspace is not linked to that crate graph, still run the root non-JSON proof as a Lock 14 guard. The absence of non-JSON grammars in `skinny/Cargo.toml` is not sufficient proof that a generic pattern is grammar-neutral; it only proves skinny has no local non-JSON consumer.

## Section 3 - Risks

Producer-only close is the main W1 risk. The current `--with-cost-facts` path prints JSON and exits successfully, but it does not update `RESULTS.md`, does not bind row-level W1 manifest fields, and does not make `gate-json` reject the pre-W1 sentinel. SPEC explicitly pre-blocks producer-only CostFacts/telemetry (`restart/skinny/tranches/sk-v8/SPEC.md:418-429`).

The `sk-v7-costfacts-v1` schema name is a governance risk. It is valid evidence that W9 created the substrate (`skinny/REDRESS.md:2466-2506`), but W1 should not claim an SK-V8 gate from a standalone SK-V7 snapshot without either renaming/wrapping the manifest or embedding the evidence into the SK-V8 report schema.

The current diagnostics create a pass/fail ambiguity. `BBNF-COSTFACTS-MISSING-EVIDENCE` exists to expose missing measurement-backed evidence without changing parser selection (`skinny/REDRESS.md:2487-2490`). If W1 treats that diagnostic as hard failure, current CostFacts cannot pass without a producer change. If W1 treats only missing entries/sentinels as hard failure, document that distinction in the W1 plan and gate tests.

Generic `codegen` already has a JSON runtime-profile guard (`skinny/crates/codegen/src/lib.rs:169-177`). W1 should not widen that surface. Any edit nearby will look like a generic JSON policy leak unless paired with the template/provider proof and zero generated-output diff.

Lock 14 residues are pre-blocked. REDRESS 36-38 record JSON-hardcoded SIMD classifiers, a JSON god module, and a fossil scanner as Lock 14 violations (`skinny/REDRESS.md:460-515`). REDRESS 85-86 record the admitted neutralization and zero-result-diff/non-JSON checks (`skinny/REDRESS.md:2397-2464`). W1 must not reopen these routes by renaming JSON policy into neutral CostFacts labels.

Non-JSON proof can exceed W1's time cap if left to the end. CSS L4 regen is the expensive path in the root workspace, and `cargo xtask regen --check` covers all nine grammars. For W1, use targeted `--grammar css_l4`, `--grammar google_sheets`, and `--grammar bbnf` when generic code changes; otherwise prefer the two runtime smoke tests plus zero-diff scans.

## Section 4 - Sources

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:11-39` - research artifact contract.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:177-186` - same-wave consumer rule.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md:190-199` - role separation.
- `restart/skinny/tranches/sk-v8/SPEC.md:103-140` - required telemetry fields.
- `restart/skinny/tranches/sk-v8/SPEC.md:261-286` - Lock 14 generic-crate proof gate.
- `restart/skinny/tranches/sk-v8/SPEC.md:374-429` - W1 CostFacts gate binding.
- `restart/skinny/tranches/sk-v8/SPEC.md:652-700` - W5 grammar-neutral Lock 14 audit.
- `restart/skinny/tranches/sk-v8/SPEC.md:756-801` - inherited pre-blocked routes.
- `restart/skinny/tranches/sk-v8/HANDOFF.md:127-146` - wave budgets and generated diff discipline.
- `restart/skinny/tranches/sk-v8/HANDOFF.md:174-191` - W1 entry and generic-edit proof requirement.
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:19-30` - W0 closure and CH dispositions.
- `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md:65-84` - W1 residual routing.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375-397` - frozen roots.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs:462-490` - BackendShape and UnionTape drift checks.
- `skinny/crates/ir/src/cost.rs:5-135` - CostFacts schema and backend-shape set.
- `skinny/crates/passes/src/lib.rs:28-55` - compile path populating layout CostFacts.
- `skinny/crates/passes/src/lib.rs:390-441` - CostFacts derivation.
- `skinny/crates/passes/src/lib.rs:527-626` - rejected alternatives, capacity, and evidence diagnostics.
- `skinny/crates/passes/src/lib.rs:1517-1560` - CostFacts unit-test assertions.
- `skinny/crates/codegen/src/lib.rs:169-248` - JSON runtime guard, default CostFacts, and CostFacts snapshot producer.
- `skinny/crates/codegen/src/lower/rust.rs:20-74` - CostFacts threaded into lowerer selection.
- `skinny/xtask/src/main.rs:240-305` - current `gate-json --with-cost-facts` standalone snapshot.
- `skinny/crates/bbnf-bench/src/report.rs:43-68` - SK-V8 telemetry CostFacts fields.
- `skinny/crates/bbnf-bench/src/report.rs:275-373` - current W0 validation.
- `skinny/crates/bbnf-bench/src/report.rs:575-609` - manifest rendering.
- `skinny/crates/bbnf-bench/src/report.rs:1007-1013` - pre-W1 sentinel validation.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:319-339` - current gate report validation/write path.
- `skinny/crates/bbnf-bench/src/bin/gate.rs:474-498` - current sentinel population.
- `skinny/crates/bbnf-bench/src/gate.rs:135-175` - strict admission fail-closed checks.
- `skinny/REDRESS.md:460-515` - REDRESS 36-38 Lock 14 residues.
- `skinny/REDRESS.md:2397-2464` - REDRESS 85-86 Lock 14 neutralization.
- `skinny/REDRESS.md:2466-2506` - REDRESS 87 CostFacts substrate projection.
- `Cargo.toml:18-56` - root grammar and strategy manifests for non-JSON proof.
- `xtask/src/regen.rs:570-650` - root regen check diffing generated Rust and registry sidecars.
- `crates/core/src/grammar/generated/mod.rs:1-35` - generated grammar module surface.
- `crates/core/tests/project_types_css_l4.rs:1-29` and `crates/core/tests/project_types_css_l4.rs:246-345` - CSS L4 compile/lower/audit proof.
- `crates/core/tests/projection_totality.rs:84-224` - JSON/CSS L4/Sheets/BBNF document projection proof.
- `crates/core/tests/typed_accessor_surface.rs:560-642` and `crates/core/tests/typed_accessor_surface.rs:737-795` - CSS L4/Sheets/BBNF typed accessor proof.
