# SK-V15 W0 Plan: Baseline Telemetry Lock

Date: 2026-05-28.
Scope: W0 plan-only artifact. No source, generated output, `RESULTS.md`,
`REDRESS.md`, gate, provider, or runtime files are edited by this plan.
Output: this file.

## Inputs

- W0 authority: `restart/skinny/tranches/sk-v15/SPEC.md` Section 3 and
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` W0.
- Research cohort:
  - `skv15-W0-A-results-schema.md`
  - `skv15-W0-B-gate-json.md`
  - `skv15-W0-C-css-broadcast.md`
  - `skv15-W0-D-json-51-guard.md`
  - `skv15-W0-E-telemetry-carrier.md`
  - `skv15-W0-F-no-behavior-risk.md`
- Gate schema source: `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md`.
- Current measured ledger: `skinny/RESULTS.md` and `skinny/REDRESS.md`.
- Implementation guardrail: W0 is telemetry/baseline only. It does not delete
  CSS providers, generated runtime modules, `CSS_GENERATED_RS`,
  `CssFullParseSummary`, fact-stream-only parsers, or Pattern H files.

## Intervention

W0 uses the existing W0 telemetry manifest as the carrier and upgrades it from
SK-V14 to SK-V15:

1. Keep the visible schema-v3 RESULTS table unchanged.
2. Rename the manifest heading to `## SK-V15 W0 Telemetry Manifest`.
3. Append the ten required SK-V15 fields after `Comparator evidence`:
   `measurement_row_id`, `measurement_origin`, `value_plane`,
   `css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
   `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and
   `broadcast_group_id`.
4. Populate all 51 JSON guard rows with explicit SK-V15-open telemetry and
   `broadcast_group_id=none:independent`.
5. Preserve the 24 CSS W8R rows as diagnostic broadcast evidence with shared
   `measurement_row_id=SK-V14-W8R-css-full-parse-profile-cold-8`,
   `broadcast_group_id=SK-V14-W8R-css-l4-full-parse`, non-admission
   diagnostic markers, `value_plane=full_parse_summary`, and a workload
   mismatch marker. W1 owns the full live-admit demotion/collapse ledger.
6. Extend the skinny `gate-json --check-results` path to require the SK-V15
   manifest, parse exactly 42 cells, consume every new field, reject
   missing/blank fields, reject self-exempting gate reports, reject CSS
   hidden/visible broadcast admissions, and reject CSS live admission from
   full-parse summary / fact-stream / hand-written generator provenance.
7. Extend `bbnf-bench` report rendering so generated results write the same
   42-cell manifest rather than re-emitting the obsolete SK-V14 carrier.

This is a carrier and gate-consumption change. It is not a parser behavior
change and does not create a CSS admission claim.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
  - Add SK-V15 manifest fields to the manifest model.
  - Render `SK-V15 W0 Telemetry Manifest` with 42 cells.
  - Derive JSON SK-V15-open and CSS W8R diagnostic field values from the
    existing row data.
  - Add report-side tests for the 42-column header and field rendering.
- `skinny/xtask/src/main.rs`
  - Upgrade `validate_w0_results_snapshot` from SK-V14-only validation to
    SK-V15 manifest validation.
  - Parse and validate all 42 cells.
  - Add fail-closed tests for missing fields, hidden CSS broadcast, visible CSS
    broadcast admission, self-exempting gate exclusion, and CSS plane mismatch.
- `skinny/RESULTS.md`
  - Regenerate or minimally align the manifest to the 42-cell SK-V15 carrier.
  - Preserve visible JSON rows and CSS diagnostic evidence. Do not move Track 1
    or Track 2 Mbps cells in W0.
- `restart/skinny/tranches/sk-v15/research/w0/skv15-W0-redress.md`
  - Record implementation evidence, command outputs, and W0 disposition.

Protected in W0:

- `skinny/crates/runtime/src/grammars/css_l4_*/generated.rs`
- `skinny/crates/codegen/src/runtime_generator.rs`
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- root `crates/core/src/runtime/**`

These can be read but not edited by W0 redress.

## Falsifiability Gate

W0 admits only if all checks hold:

1. `skinny/RESULTS.md` has one `## SK-V15 W0 Telemetry Manifest` and no
   `## SK-V14 W0 Telemetry Manifest`.
2. The manifest has exactly 75 rows: 51 JSON rows and 24 CSS diagnostic rows.
3. Every manifest row has exactly 42 cells and non-empty values for all ten
   SK-V15 fields.
4. JSON rows retain `A / GO / strict / measured-row`, native
   `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max`, `target_cpu=native`,
   explicit `css_comparator_workload=n/a:not-css`, and
   `broadcast_group_id=none:independent`.
5. CSS rows carry the shared W8R broadcast group and cannot be interpreted as
   independent live CSS admits under SK-V15.
6. `gate-json --check-results` rejects a fixture with any SK-V15 field removed.
7. `gate-json --check-results` rejects 24 CSS `A / GO` rows sharing one
   `measurement_row_id`.
8. `gate-json --check-results` rejects 24 CSS rows with unique
   `measurement_row_id`s but identical origin/metric/profile signatures.
9. `gate-json --check-results` rejects `gate_exclusion_report=self-exempting:*`.
10. `gate-json --check-results` rejects CSS live admission when
    `value_plane=full_parse_summary`, `value_plane=fact_stream`, or
    `generator_source` names `CSS_GENERATED_RS` / hand-written CSS tokenizer
    provenance.
11. No source, generated runtime, provider, CSS bench, `REDRESS.md`, or rolling
    delta file changes are present in the W0 redress diff.

Verification command set:

```sh
cd /Users/mkbabb/Programming/bbnf-lang/skinny
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
```

If `cargo run --profile ax-iter -p xtask -- gate-json --check-results` needs
to compile for more than 60 seconds, redirect output to one file and inspect
that file once. Do not rerun variants for filters.

## Hard Cap

Redress hard cap: 30 minutes. At 27 minutes, commit the coherent W0 redress
slice if it is admitting; at 30 minutes, stop the redress attempt, write the
redress artifact with the failed predicate, and route a REDRESS-class blocker.

W0 does not broaden into W1 demotion, W2 Lock 14 restoration, W3 codegen leak
abrogation, W4 Pattern H provenance, W5 CSS Value API, W6 retime, W7 Decision
Engine, W8-W10 SIMD/shape implementation, or W11 close.

## Revert Protocol

Revert as a single W0 redress slice if any of these occur:

- `gate-json --check-results` cannot parse the SK-V15 carrier.
- A missing SK-V15 field passes.
- A CSS W8R broadcast row can still close as an independent SK-V15 live admit.
- The patch changes parser/runtime/provider behavior.
- The patch modifies protected generated/runtime/CSS provider files.

Rollback scope is limited to:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- W0 redress artifact

Do not revert unrelated dirty files in root runtime, prior SK-V12/SK-V13
research, CSS generated runtime files, or existing bench files outside the W0
owner set.

## Same-Wave Consumer

Same-wave consumer is `cargo xtask gate-json --check-results` from the skinny
workspace, implemented through `skinny/xtask/src/main.rs`. The report renderer
in `skinny/crates/bbnf-bench/src/report.rs` is a producer; it is not sufficient
unless the xtask gate consumes the emitted carrier and negative fixtures prove
the fields are load-bearing.

## Pre-Blocked Routes

- Alias-only telemetry: old `Run id`, `Sample cost`, `Output plane`, or
  `Diagnostic nonproducer` cells do not satisfy SK-V15 exact field names.
- Producer-only telemetry: rendering ten fields without xtask parser
  validation rejects W0.
- CSS W8R admit carry-forward: 24 full-parse summary rows with the W8R tuple
  cannot close CSS admission.
- Provider deletion in W0: forbidden. W5/W6 own typed CSS replacement and
  same-workload retime.
- Warm bench baseline: forbidden. W0 can freeze current ledger and gate
  carrier; cold rerun proof remains routed to behavior waves or an isolated
  clean baseline capture.
- Dirty worktree timing: current unrelated dirty bench/generated/runtime files
  block authoritative new throughput claims. W0 therefore makes no new Mbps
  claim.

DISPOSITION: PLAN-ACCEPT. Redress is authorized only for the owner paths and
falsifiability gates above.
