# SK-V11 S-P3 V2 CH6: Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: V2 CHALLENGE.
Lens: CH6 anti-paper-close / next-tranche impact.
Date: 2026-05-20.
Scope: verify that the committed S-P3 V2 packet cannot close by prose,
deferred telemetry, baseline self-dealing, orphan primitives, or future-wave
promises.

## Verdict

ACCEPT.

The V2 packet is dispatchable under CH6. Every wave has a named measurable gate
or an explicitly non-admitting schema/baseline gate. W1a and W1b cannot move
rows, W2 must consume the W1b baseline, telemetry must be consumed in the same
wave that emits it, and W8/W9 cannot paper-close direct or non-JSON obligations.
The only non-JSON escape is `BLOCKED`, which prevents close rather than
satisfying it.

## Checks

### Measurable Gates For Every Wave

ACCEPT. The V2 manifest names W0, W1a, W1b, W2-W9 with entry dependencies,
LOC caps, redress caps, and dispatch conditions (`SPEC.md:185-211`). P3-B binds
each wave to a measurable gate: W0 reproduces the opening authority, W1a rejects
missing non-JSON schema fields, W1b creates exactly one generated non-JSON
baseline plus independent oracle, W2 requires a CSS generated intervention at
`ceil(W1b_css_baseline_mbps * 1.01)`, W3-W7 name row floors or profiled
output-sink conditions, W8 requires every direct residual row to be admitted or
REDRESS-proven uncloseable, and W9 blocks on any unresolved row
(`p3b-wave-sequencing.md:101-113`). P3-C repeats the same gate set with revert
protocols for `G-W0` through `G-W9` (`p3c-falsifiability-gates.md:75-87`).

### W1a And W1b Cannot Paper-Close

ACCEPT. V2 splits the V1 W1 overload into two non-admitting waves. W1a is only a
non-JSON gate/report schema lane; it must reject missing non-JSON fields and
producer-only telemetry, keep JSON gate-json green, move no JSON row, and claim
no generated non-JSON baseline authority (`SPEC.md:283-320`). W1b may stand up
exactly one generated non-JSON baseline plus independent oracle, but no
intervention may admit and no JSON row may move (`SPEC.md:326-377`). P3-E
matches that boundary: W1a cannot create baseline authority or admit a row, and
W1b cannot land an intervention, claim admission, or leave W2 to invent the
first baseline (`p3e-preblocked-ledger.md:154-170`).

### W2 Consumes A Concrete Baseline

ACCEPT. W2 entry requires W1b closure plus a named generated non-JSON direct or
typed intervention, scalar oracle, independent Track 2/oracle, baseline Mbps,
target threshold, and Lock 14 proof (`SPEC.md:397-400`). Its task list states
that it consumes the W1b baseline and may not create the first measurable
non-JSON row (`SPEC.md:402-409`). Its exit gate uses one rounding rule,
`ceil(W1b_css_baseline_mbps * 1.01)`, and P3-B says W2 is REVISE before redress
if W1b cannot produce the baseline row (`p3b-wave-sequencing.md:101-107`).

### Telemetry Is Consumed Same-Wave

ACCEPT. The close condition states that telemetry is consumed by the relevant
gate in the same wave and that no producer-only field, report, or proof artifact
can close a wave (`SPEC.md:54-57`). P3-D makes this executable: any rendered
column or manifest/comparator/non-JSON field not read by `gate-json` in the same
wave rejects, and every emitted field must be consumed by `validate_schema_v3`,
`validate_sk_v8_w0`, `validate_strict_admission`, or the same-commit gate
extension (`p3d-telemetry-schema.md:195-222`). The dispatch prompt carries the
same rule for any new field or non-JSON companion report
(`DISPATCH-PROMPT.md:190-201`).

### Close Requires Direct-Row And Non-JSON Evidence

ACCEPT. The SPEC close condition requires each residual direct row to become
strict same-run `A / GO` on generated Track 1 and independent Track 2/oracle, or
to receive a per-row uncloseable REDRESS proof with measurement
(`SPEC.md:26-29`). It separately requires at least one admitted, benchmarked
non-JSON generated direct or typed parser intervention (`SPEC.md:42-44`).

W8 preserves the direct evidence boundary: all direct residual rows must be
`A / GO` or have measured REDRESS proof, W0-clamped rows need W3-W8 measured
provenance, and source movement is invalid unless CHALLENGE accepted it before
redress (`SPEC.md:708-737`). W9 then requires every residual direct row to be
admitted or backed by an uncloseable proof naming the attempted intervention,
Track 1, Track 2/oracle, comparator, floor, guard result, and routed remainder;
it also requires an admitted and benchmarked non-JSON generated intervention
unless Close escalates `BLOCKED` for grammar-generalization fixpoint
(`SPEC.md:751-764`). That `BLOCKED` path is not a close waiver; the dispatch
prompt confirms close cannot waive the non-JSON axis without a `BLOCKED` verdict
(`DISPATCH-PROMPT.md:216-222`).

### W8 Source Split Cannot Hide Future Promises

ACCEPT. V2 makes W8 docs/gate/result accounting by default and says source work
requires a W8a split plus the only remaining spare bracket slot
(`SPEC.md:193-211`). The W8 section repeats that source work is outside W8
unless split as W8a, with exactly one candidate and one row subset; W8 then
remains accounting (`SPEC.md:691-720`). P3-B blocks second-split scope creep by
stating that the 11-wave bracket has exactly one spare slot and any second split
escalates instead of silently expanding scope (`p3b-wave-sequencing.md:43-46`).
W8 and W9 pre-block paper fixpoints, future-phase promises, and G-Alpha
presentation while any W1a-W8 wave lacks admitted, rejected, or measured status
(`p3b-wave-sequencing.md:151-154`).

## Residual Notes

- W1a/W1b are intentionally not behavior gates. That is acceptable because they
  are fail-closed prerequisites and explicitly cannot admit rows.
- W2 is the first non-JSON admitting wave. If the W1b baseline is absent, W2 is
  not allowed to manufacture a same-wave baseline and claim improvement.
- W8 may escalate unresolved direct or non-JSON gates to `BLOCKED`, but that is
  a failure disposition, not close.

## File Changed

- `restart/skinny/tranches/sk-v11/research/p3/hardening/V2/CH6-anti-paper-close.md`
