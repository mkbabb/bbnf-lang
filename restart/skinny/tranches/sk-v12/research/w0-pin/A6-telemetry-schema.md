# SK-V12 W0 PIN Research A6 - Telemetry Schema

Date: 2026-05-20.
Scope: read-only telemetry schema and CSS L4 pin readiness.
Verdict: PASS.

## Findings

W0 can pass without real CSS benchmark artifacts because W0 is a
telemetry/gate revalidation slice, not the CSS admission wave.

The current W0 `sk-v12-nonjson-generated-v1` validator is a companion-gate
fixture lane. It proves that report fields can be consumed by `gate-json`, but
it is not the final CSS SOTA schema and cannot admit the pin target.

W1b-2 must extend or replace that lane with a CSS L4 admission schema that
consumes:

- generated CSS L4 Track 1 throughput;
- independent oracle or Track 2 throughput;
- lightningcss command/version/Mbps/artifact;
- strict output equality artifact;
- profile and benchmark artifacts;
- Lock 14 status;
- Lock 16 status where applicable;
- JSON guard disposition.

Using the W0 companion schema as-is for CSS admission would be REVISE.

## Sources

- `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md`
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md`
- `restart/skinny/tranches/sk-v12/research/skv12-W0-redress.md`
- `restart/skinny/tranches/sk-v12/SPEC.md`
