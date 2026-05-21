# SK-V12 W1b-1 CH1 - Correctness / Generation / Equality

Verdict: ACCEPT.

## Evidence

- SPEC Section 6 authorizes exactly this scaffold shape: generated CSS L4 Track
  1 plus independent oracle/equality, no lightningcss throughput gate, row
  `css_l4/declaration_values/direct_to_struct/main`, output plane
  `css_l4_declaration_value_fact_stream`, generated runtime under
  `skinny/crates/runtime/src/grammars/css_l4_declaration_values/`, and strict
  equal fact-stream output with finite Mbps and generated-size telemetry
  (`SPEC.md:389-453`).
- The plan binds the same row and output plane, explicitly keeps W1b-1
  scalar-only, forbids `lightningcss`, `bbnf-simd`, and aarch64 edits, and
  assigns the SOTA admission bar to W1b-2 (`PLAN.md:8-12`). This avoids the
  main CH1 overclaim failure mode.
- Generated Track 1 is not accepted on path name alone: the plan requires a
  CSS-specific codegen profile/provider/templates surface, emitted
  `mod.rs`/`config.rs`/`parser.rs`/`generated.rs`, and a reproducibility test
  that renders those files and byte-compares against the committed runtime
  (`PLAN.md:14-25`, `PLAN.md:173-177`). That is enough to enter redress.
- The plan closes the fixture/equality surface: fixed 187-byte LF fixture with
  SHA-256 `cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374`,
  CSS-local Track 1 fact stream, `cssparser` oracle, byte equality before
  timing, retained Track 1/oracle artifacts, and first-diff artifact on failure
  (`PLAN.md:50-91`).
- Companion-gate consumption is specified for the CH1-relevant facts:
  strictness, grammar/input checksums, generated LOC/module bytes,
  validation/profile artifact, Lock 14/16 status, scalar reference status, and
  parity status, with `C/GO` scaffold outcome and no main JSON telemetry-column
  expansion (`PLAN.md:93-134`).
- The adversarial review's earlier owner-table and generated-proof concerns are
  addressed in the current plan and SPEC owner surface: the redress owner table
  now includes `skinny/Cargo.toml`, `grammar_profile.rs`, the CSS provider and
  template paths, runtime export, bench report/gate paths, fixture, report, and
  artifacts (`SPEC.md:394-417`; `PLAN.md:32-48`).

## Required Redress Preconditions

- The generated parser/token traversal must live in generated files emitted by
  the CSS-owned codegen provider. `sink.rs` may emit/normalize the canonical
  fact stream, but must not become the tokenizer, declaration parser, or oracle.
- `cargo test -p codegen
  css_l4_declaration_values_generated_runtime_reproducible -- --nocapture`
  must fail if committed runtime output diverges from freshly generated output.
- The canonical fact stream must include enough semantic shape to make equality
  meaningful for the selected fixture: schema version, input checksum/bytes,
  declaration ordinal and enclosing context, property name, important flag,
  token/value facts with offsets or equivalent source spans, and final stream
  hash. Declaration-count, token-count, pretty-print, or digest-only equality is
  insufficient.
- The oracle must remain independent: no calls to generated Track 1,
  `runtime::generated_json`, root CSS runtime, `lightningcss`,
  `parse_that_regex`, `bbnf-simd`, generated parser internals, or shared CSS
  classification/declaration traversal.
- The companion report must retain Track 1 facts, oracle facts, first-diff
  artifact on failure, profile/validation artifacts, and generated-size
  telemetry under
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/`, and `gate-json`
  must reject missing/bad values.
- W1b-1 may record only a scaffold/equality `C/GO` non-JSON row. It must not add
  `lightningcss_mbps`, CSS ADMIT language, a new outcome variant, or main
  `skinny/RESULTS.md` JSON columns.
- If source edits are attempted and any generated proof, equality, oracle
  independence, or report-consumption check fails, redress must save
  `/tmp/skv12-waveW1b-1-rejected.patch`, revert only the W1b-1 slice, and
  record `BLOCKED/FAIL` rather than substituting Sheets, BBNF-self, JSON rows,
  root CSS runtime, or a report-only close.

## Precise Blockers

None at plan time for CH1. The known risk is redress-time falsifiability:
because W1b-1 deliberately does not lower the full CSS L4 BBNF import/syntax
surface, a hand-coded parser under a generated path, a coupled oracle, or a
thin fact stream would immediately falsify `G-W1b-1-CSS-L4-ORACLE`.
