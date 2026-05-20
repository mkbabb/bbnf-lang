# SK-V12 Wave W0 Plan: Telemetry Lock Gate Surface

Inputs:

- `restart/skinny/tranches/sk-v12/SPEC.md:329` - W0 owner paths, tasks,
  `G-W0-SK-V12-OPEN`, same-wave consumers, and pre-blocked routes.
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:116` -
  `sk-v12-nonjson-generated-v1` companion report field contract.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-A1-profile-authority.md:9`
  - SK-V12-open profile authority and replay surface.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-A2-results-surface.md:9`
  - 41-row JSON opening outcome surface.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-A3-nonjson-gate.md:9` -
  existing W1a-only companion gate gap.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-A4-redress-preblocks.md:9`
  - REDRESS 111/112/113 and 119/120 pre-blocks.
- `restart/skinny/tranches/sk-v12/research/skv12-W0-A6-command-feasibility.md:9`
  - executable validation command surface and Criterion authority root.

Intervention: Admit the SK-V12-open telemetry/report lock by adding an
executable `sk-v12-nonjson-generated-v1` companion gate lane, binding the
opening profile authority into W0 evidence, and preserving the JSON row surface
unchanged.

Owner paths:

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `skinny/crates/bbnf-bench/` test-only fixtures or inline tests if required
- `restart/skinny/tranches/sk-v12/research/skv12-W0-redress.md`
- `restart/skinny/tranches/sk-v12/research/skv12-W0-nonjson-pass.json`
- `skinny/RESULTS.md` only if the opening metadata surface must be refreshed
- `skinny/REDRESS.md` only if W0 rejects

Falsifiability gate: `G-W0-SK-V12-OPEN`.

- All 41 JSON main rows keep their opening outcomes: 17 parse diagnostics, 4
  direct `A / GO`, 13 direct `N-direct / NO-GO`, and 7 typed `A / GO`.
- No behavior drift: no parser, scanner, SIMD/ASM, codegen behavior, generated
  runtime output, or benchmark body path changes.
- The SK-V12-open profile authority is named in redress evidence and validated
  through the JSON gate using `/tmp/skv11-open-criterion-3ce75df` where the
  current opening Criterion authority lives.
- The non-JSON companion report lane accepts a well-formed
  `sk-v12-nonjson-generated-v1` generated-baseline report and rejects
  malformed, coupled, stale, JSON-domain, producer-only, sub-floor, or missing
  generated Track 1 evidence.
- `gate-json` and the companion non-JSON gate consume every field emitted by the
  W0 report artifacts in the same wave.

Hard cap: 75 minutes redress, with the W0 wave wall cap remaining 90 minutes
for report/gate/test/doc overhead.

Revert protocol: If any W0 gate fails, revert the W0 gate/report/xtask and
evidence artifacts as one slice, restore the opening report surface, save the
rejected patch to `/tmp/skv12-waveW0-rejected.patch`, and add a REDRESS entry
naming the missing field, stale run id, coupled oracle, malformed gate, or
unexpected behavior drift.

Same-wave consumer:

- `xtask gate-json --advisory --check-results` consumes the JSON opening row
  surface and Criterion metadata authority.
- `xtask gate-json --with-cost-facts --check-results` consumes the cost-fact
  path.
- `xtask gate-json --skv12-non-json-report <report.json>` forwards to the gate
  binary, runs Lock 14 validation, and consumes the SK-V12 companion report.

Pre-blocked routes:

- No W0 parser, scanner, SIMD/ASM, generated runtime, codegen behavior, or
  benchmark body change.
- No JSON row movement and no JSON-only direct reopen before W1/W2 generated
  non-JSON priority resolves.
- No generated non-JSON admission by report fixture alone.
- No stale sidecar as a strict anchor.
- No reuse of the SK-V11 W1a schema as SK-V12 generated-baseline authority.
- No companion-gate bypass of generic JSON policy/Lock 14 validation.
