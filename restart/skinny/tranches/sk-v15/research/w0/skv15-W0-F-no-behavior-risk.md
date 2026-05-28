# SK-V15 W0-F No-Behavior / Protected-Surface Risk

Date: 2026-05-28

Scope: research-only W0 worker F. This file is the only permitted output for this worker. No source, generated code, RESULTS, REDRESS, gate file, or unrelated doc change is authorized by this report.

## Authority Read

Current implementation authority is SK-V15 W0-W11 after G-Omega. `restart/HANDOFF.md` names SK-V15 SPEC and DISPATCH as the locked implementation contract and says the next dispatch is W0 baseline/telemetry, with no routine Alpha/Omega before W0 (`restart/HANDOFF.md:5`, `restart/HANDOFF.md:11`). The G-Omega packet states SK-V15 SPEC/DISPATCH become the W0-W11 implementation contract after authorized CRUD and that W0 should be dispatched through DISPATCH (`restart/audit/totality/astral/V9/G-OMEGA-PACKET.md:11`, `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md:29`, `restart/audit/totality/astral/V9/G-OMEGA-PACKET.md:122`).

W0 is not a behavior/provider wave. DISPATCH says every wave is research -> plan -> redress, research and plan have no source edits, and redress may edit source only for its approved slice (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:5`, `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:31`). W0 scope is baseline, RESULTS schema, telemetry carrier, gate-json parser, CSS broadcast diagnostic, and 51 JSON rows (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:119`). SPEC says W0 exit requires gate-consumed telemetry, CSS W8R non-admit diagnostic handling, and no provider deletion (`restart/skinny/tranches/sk-v15/SPEC.md:246`, `restart/skinny/tranches/sk-v15/SPEC.md:258`). P3-C defines the W0 ledger/gate budget as no production parser/codegen/runtime behavior diff and no RESULTS Mbps movement except targeted demotion/collapse (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:17`, `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:43`). P3-E says W0 may close only on baseline/gate transcript and must not create new behavioral admission (`restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:57`).

The protected runtime/generated/provider surfaces are explicitly deferred. Pattern H root runtime is W4 and has 67 required root files (`restart/HANDOFF.md:67`, `restart/skinny/tranches/sk-v15/SPEC.md:318`). CSS provider/template deletion is blocked until W5/W6-grade typed replacement proof, and CSS_GENERATED_RS/summary fact-stream are blocked before W6 (`restart/skinny/tranches/sk-v15/SPEC.md:194`, `restart/skinny/tranches/sk-v15/SPEC.md:195`, `restart/skinny/tranches/sk-v15/SPEC.md:197`). DISPATCH also requires dependency rows before delete, retire, demote, or neutralize actions (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:69`).

## Files W0 May Touch Without Behavior Or Provider Deletion

Research phase:

- `restart/skinny/tranches/sk-v15/research/w0/skv15-W0-F-no-behavior-risk.md` only.

Later W0 redress may touch only the gate/telemetry/report slice needed by an approved W0 plan:

- `skinny/crates/bbnf-bench/src/report.rs`: telemetry carrier, manifest rendering, schema validation, and W0 report row checks. Existing ownership includes `Report`, `SkV8Telemetry`, and `TelemetryRow` (`skinny/crates/bbnf-bench/src/report.rs:114`), row schema validation (`skinny/crates/bbnf-bench/src/report.rs:3387`), W0 row validation (`skinny/crates/bbnf-bench/src/report.rs:3443`), manifest validation (`skinny/crates/bbnf-bench/src/report.rs:3758`), report validation (`skinny/crates/bbnf-bench/src/report.rs:4514`), and manifest rendering (`skinny/crates/bbnf-bench/src/report.rs:5188`). This file is an allowed W0 target only for gate-consumed telemetry and rejection predicates, not for benchmark behavior.
- `skinny/crates/bbnf-bench/src/bin/gate.rs`: gate-json update/check path and retained CSS row handling. The binary already validates/render/updates reports (`skinny/crates/bbnf-bench/src/bin/gate.rs:778`) and handles companion report paths (`skinny/crates/bbnf-bench/src/bin/gate.rs:51`). W0 may use this only to consume/reject the SK-V15 telemetry schema and CSS diagnostic state.
- `skinny/xtask/src/main.rs`: gate-json front door, passthrough, and `--check-results` validation only. The wrapper calls W0 results validation before running `bbnf-bench gate` (`skinny/xtask/src/main.rs:285`), limits passthrough args (`skinny/xtask/src/main.rs:308`), and validates RESULTS/REDRESS/rolling delta for `--check-results` (`skinny/xtask/src/main.rs:400`). This is skinny gate orchestration, not root runtime regeneration.
- `skinny/RESULTS.md`: only if the W0 redress writes the SK-V15-open telemetry ledger or performs the explicitly planned CSS W8R diagnostic/demotion. SPEC requires the 51 JSON rows and CSS W8R non-admit diagnostic at W0 exit (`restart/skinny/tranches/sk-v15/SPEC.md:258`), while P3-C forbids non-target Mbps/verdict movement (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:43`).
- `skinny/REDRESS.md`: only if the W0 redress must record a failed/rejected W0 close or an approved redress ledger entry. It is not part of a successful research-only worker and must not be edited by this worker.

Conditional W0 redress files:

- `skinny/crates/bbnf-bench/src/gate.rs`: only for strict evidence validation or rejection predicates. Existing strict admission validation and metadata checks live here (`skinny/crates/bbnf-bench/src/gate.rs:136`, `skinny/crates/bbnf-bench/src/gate.rs:185`). No comparator threshold, workload, parser, or provider behavior change belongs in W0.
- `skinny/crates/bbnf-bench/src/metadata.rs`: only for telemetry source fields already consumed by the gate/report path. Row metadata and host facts are defined here (`skinny/crates/bbnf-bench/src/metadata.rs:20`, `skinny/crates/bbnf-bench/src/metadata.rs:110`). No benchmark semantics change belongs here for W0.
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`: only if a new W0 gate/report owner path needs classification. It already classifies gate/report/metadata files as bench gate schema or telemetry-only (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:453`) and tracks root scan roots (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:700`). Lock14/16 proof work is W2, so W0 must not use this to repair generic-token coverage (`restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:144`).

Forbidden W0 behavior/provider surfaces:

- `crates/core/src/runtime/**`: root runtime Pattern H files are W4, with a 67-file provenance/regen/check obligation (`restart/HANDOFF.md:67`, `restart/skinny/tranches/sk-v15/SPEC.md:318`). W0 must not edit, delete, regenerate, or normalize these files.
- `skinny/crates/runtime/src/grammars/**`: skinny generated runtime/provider output, especially CSS generated files, is W5/W6 protected. SPEC blocks CSS generated/provider retirement before typed replacement proof (`restart/skinny/tranches/sk-v15/SPEC.md:195`, `restart/skinny/tranches/sk-v15/SPEC.md:336`).
- `skinny/crates/codegen/src/**`: provider/template/codegen remediation is W3/W5/W6, not W0. SPEC places the CSS provider template neutralization in W3 and deletion proof in W6 (`restart/skinny/tranches/sk-v15/SPEC.md:300`, `restart/skinny/tranches/sk-v15/SPEC.md:336`).
- `xtask/src/main.rs` and `xtask/src/regen_simple_runtime.rs`: root runtime regeneration command surfaces write root generated runtime files (`xtask/src/main.rs:62`, `xtask/src/regen_simple_runtime.rs:71`). W0 may touch skinny `xtask`, not root `xtask`.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs` and `skinny/crates/bbnf-bench/src/generated_real_typed.rs`: CSS W8/typed bench artifacts are provider/proof surfaces for later CSS waves, not W0 telemetry schema work.

## Dirty Files That Are Unrelated Hazards

These files were already dirty and must be preserved as unrelated work. W0 redress must neither revert nor include them.

Root runtime Pattern H hazards, protected for W4:

- `crates/core/src/runtime/bbnf/arena.rs`
- `crates/core/src/runtime/bbnf/builder.rs`
- `crates/core/src/runtime/bbnf/document.rs`
- `crates/core/src/runtime/bbnf/parse_with.rs`
- `crates/core/src/runtime/bbnf/serialize.rs`
- `crates/core/src/runtime/bbnf/view.rs`
- `crates/core/src/runtime/bnf/builder.rs`
- `crates/core/src/runtime/bnf/document.rs`
- `crates/core/src/runtime/bnf/kind.rs`
- `crates/core/src/runtime/css_pretty/builder.rs`
- `crates/core/src/runtime/css_pretty/document.rs`
- `crates/core/src/runtime/css_pretty/kind.rs`
- `crates/core/src/runtime/css_pretty/view.rs`
- `crates/core/src/runtime/csv/builder.rs`
- `crates/core/src/runtime/csv/document.rs`
- `crates/core/src/runtime/csv/kind.rs`
- `crates/core/src/runtime/ebnf/builder.rs`
- `crates/core/src/runtime/ebnf/document.rs`
- `crates/core/src/runtime/ebnf/kind.rs`
- `crates/core/src/runtime/google_sheets/arena.rs`
- `crates/core/src/runtime/google_sheets/builder.rs`
- `crates/core/src/runtime/google_sheets/document/canonical.rs`
- `crates/core/src/runtime/google_sheets/document/mod.rs`
- `crates/core/src/runtime/google_sheets/document/path_query.rs`
- `crates/core/src/runtime/google_sheets/document/view.rs`
- `crates/core/src/runtime/google_sheets/parse_with.rs`
- `crates/core/src/runtime/math/builder.rs`
- `crates/core/src/runtime/math/document.rs`
- `crates/core/src/runtime/math/kind.rs`

Historical docs/research hazards outside this worker report:

- `docs/precepts`
- `restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-1-css-l4-oracle.json`
- `restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`
- `restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`
- `restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`
- `restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json`
- `restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json`
- `restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json`

Skinny CSS bench/generated/provider hazards, protected for W1/W5/W6:

- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_nested_layout/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_visual_functions/generated.rs`

Root generator hazards, not skinny W0 gate-json work:

- `xtask/src/main.rs`
- `xtask/src/regen_simple_runtime.rs`

## Exact No-Behavior Proof Commands For Later W0 Redress

Run these from `/Users/mkbabb/Programming/bbnf-lang`. They are proof commands for the later redress worker, not commands this research worker executed to mutate state.

Before W0 redress edits:

```sh
git status --porcelain=v1 > /tmp/skv15-w0-before.status
git diff --binary -- \
  crates/core/src/runtime \
  skinny/crates/runtime/src/grammars \
  skinny/crates/codegen \
  xtask/src/main.rs xtask/src/regen_simple_runtime.rs \
  skinny/crates/bbnf-bench/src/css_l4_w8.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  > /tmp/skv15-w0-protected.before.diff
```

After W0 redress edits:

```sh
git diff --binary -- \
  crates/core/src/runtime \
  skinny/crates/runtime/src/grammars \
  skinny/crates/codegen \
  xtask/src/main.rs xtask/src/regen_simple_runtime.rs \
  skinny/crates/bbnf-bench/src/css_l4_w8.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  > /tmp/skv15-w0-protected.after.diff
diff -u /tmp/skv15-w0-protected.before.diff /tmp/skv15-w0-protected.after.diff
```

Allowed-path census. The output must contain only the approved W0 redress slice, never provider/runtime/generated paths:

```sh
git diff --name-only -- \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/gate.rs \
  skinny/crates/bbnf-bench/src/metadata.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/xtask/src/main.rs \
  skinny/RESULTS.md skinny/REDRESS.md
```

Gate/report parser proof. Use `cargo run`, not a host-local cargo alias, for the consumer gate:

```sh
(cd skinny && cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --exact)
(cd skinny && cargo test -p bbnf-bench report::tests::w0_manifest_renders_required_fields -- --exact)
(cd skinny && cargo test -p bbnf-bench report::tests::schema_v3_rejects_missing_required_comparator -- --exact)
(cd skinny && cargo test -p bbnf-bench report::tests::skv13_css_comparator_report_rejects_unknown_producer_fields -- --exact)
(cd skinny && cargo test -p bbnf-bench gate::tests::rejects_non_native_comparator_id_as_strict_admission -- --exact)
(cd skinny && cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --exact)
(cd skinny && cargo run -p xtask --profile ax-iter -- gate-json --check-results)
```

No unintended RESULTS behavior movement. This may report the planned CSS W8R diagnostic/demotion only if the W0 plan names it; otherwise it must have no Mbps/verdict matches:

```sh
git diff -- skinny/RESULTS.md > /tmp/skv15-w0-results.diff
rg -n 'Track 1 Mbps|Track 2 Mbps|sonic-rs strict Mbps|cssparser_mbps|lightningcss_mbps| A \| GO | ADMITTED|NO-GO|AUDIT-' /tmp/skv15-w0-results.diff
```

Current dirty-file preservation check:

```sh
git status --porcelain=v1 > /tmp/skv15-w0-after.status
diff -u /tmp/skv15-w0-before.status /tmp/skv15-w0-after.status
```

The status diff must show only the approved W0 redress additions/edits. It must not show disappearance, cleanup, or replacement of the unrelated hazards listed above.

## Revert Boundaries For Later Redress

Do not use `git reset --hard`, `git checkout -- .`, or broad path checkout. Redress rollback must be a reverse patch over the W0-owned slice only.

Capture the W0 redress patch after edits and before rollback:

```sh
git diff --binary -- \
  skinny/crates/bbnf-bench/src/report.rs \
  skinny/crates/bbnf-bench/src/bin/gate.rs \
  skinny/crates/bbnf-bench/src/gate.rs \
  skinny/crates/bbnf-bench/src/metadata.rs \
  skinny/crates/bbnf-bench/src/lock14_baseline.rs \
  skinny/xtask/src/main.rs \
  skinny/RESULTS.md skinny/REDRESS.md \
  > /tmp/skv15-w0-redress.patch
git apply --check /tmp/skv15-w0-redress.patch
```

If W0 redress must be reverted:

```sh
git apply -R --check /tmp/skv15-w0-redress.patch
git apply -R /tmp/skv15-w0-redress.patch
git diff --binary -- \
  crates/core/src/runtime \
  skinny/crates/runtime/src/grammars \
  skinny/crates/codegen \
  xtask/src/main.rs xtask/src/regen_simple_runtime.rs \
  skinny/crates/bbnf-bench/src/css_l4_w8.rs \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  > /tmp/skv15-w0-protected.after-revert.diff
diff -u /tmp/skv15-w0-protected.before.diff /tmp/skv15-w0-protected.after-revert.diff
```

Rollback boundary:

- May reverse only W0-owned redress edits in `skinny/crates/bbnf-bench/src/report.rs`, `skinny/crates/bbnf-bench/src/bin/gate.rs`, conditional `skinny/crates/bbnf-bench/src/gate.rs`, conditional `skinny/crates/bbnf-bench/src/metadata.rs`, conditional `skinny/crates/bbnf-bench/src/lock14_baseline.rs`, `skinny/xtask/src/main.rs`, `skinny/RESULTS.md`, and conditional `skinny/REDRESS.md`.
- Must not reverse or normalize any pre-existing dirty file under `crates/core/src/runtime/**`, `skinny/crates/runtime/src/grammars/**`, `skinny/crates/codegen/**`, root `xtask/**`, `docs/precepts`, or historical SK-V12/SK-V13 research JSON.
- Must not delete, retire, demote, neutralize, or hide any provider/runtime/generated surface unless a later wave's dependency row proves its replacement or diagnostic-only status. SPEC dependency rows make CSS generated/provider deletion W5/W6 work, not W0 work (`restart/skinny/tranches/sk-v15/SPEC.md:187`, `restart/skinny/tranches/sk-v15/SPEC.md:194`).

## Bottom Line

W0 can safely change only the skinny gate/report telemetry carrier and its consumer checks, with tightly scoped RESULTS/REDRESS updates if the W0 plan requires them. It must not touch root runtime Pattern H files, skinny generated grammar/provider files, skinny codegen provider/template files, root xtask regeneration files, or the already dirty CSS bench/generated artifacts. The proof boundary is a before/after binary diff of all protected surfaces plus gate-json/test evidence that telemetry is gate-consumed and no parser/codegen/runtime behavior moved.
