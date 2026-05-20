# SK-V12 W0-A3: Non-JSON Gate Lane

Date: 2026-05-20.
Scope: SK-V12 W0 read-only audit of the companion non-JSON report and gate
surface.
Output: this file.

## Section 1 - Findings

The current executable non-JSON gate is the SK-V11 W1a report lane, not the
SK-V12 generated baseline lane. `bbnf-bench` defines
`sk-v11-w1a-nonjson-v1` and accepts `--w1a-non-json-report`; it does not yet
define `sk-v12-nonjson-generated-v1` or accept `--skv12-non-json-report`.

The existing W1a path rejects unknown fields and several stale/coupled oracle
shapes, but it validates a report fixture lane. It does not prove generated
Track 1 source/runtime/provenance, strict generated output equality, SK-V12
run-id freshness, or same-wave gate status for a generated non-JSON baseline.

The current gate binary returns immediately after W1a validation. That means
the Lock 14 baseline validator runs for the JSON gate path, but not for the W1a
companion gate path.

`xtask gate-json` does not pass a companion non-JSON report flag through to the
gate binary.

## Section 2 - Recommendations

W0 should land an SK-V12 companion gate surface rather than relying on the SK-V11
W1a report lane. The redress slice should add:

- a `sk-v12-nonjson-generated-v1` report schema with `deny_unknown_fields`;
- `--skv12-non-json-report <path>` in the gate binary;
- an `xtask gate-json` pass-through for the companion report flag;
- Lock 14 baseline validation before any non-JSON companion gate returns;
- negative tests for stale schema, JSON domain, missing generated evidence,
  coupled oracle/source, stale or mixed run ids, producer-only consumer class,
  missing gate status, and sub-1-Mbps baseline evidence.

## Section 3 - Risks

If W0 closes without this surface, W1 can still produce a report fixture that
looks non-JSON but does not prove generated Track 1 execution. That would repeat
the REDRESS 111/112/113 failure shape.

## Section 4 - Sources

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v12/SPEC.md` Section 0.4 and Section 3
- `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md`
