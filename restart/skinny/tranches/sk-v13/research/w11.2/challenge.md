# SK-V13 W11.2 CHALLENGE - Object-Loop Scalar Direct Dispatch

Date: 2026-05-21.
Plan under review: `restart/skinny/tranches/sk-v13/research/w11.2/plan.md`.
Disposition: ACCEPT with constraints.

## CH1 Correctness

PASS. The plan reuses the existing parser arms:
`parse_string_direct`, `parse_number_object_direct`, `consume_literal_direct`,
and `sink.object_*`. This preserves strict equality and error kinds if the
redress adds explicit malformed-object tests. Constraint: redress must include
object-scalar parity and error-offset tests covering trailing comma, malformed
number, missing colon/value, and nested fallback.

## CH2 Generality / Lock 14

PASS. The change is generated JSON direct behavior, not a fixture branch or
new directive. It is JSON-specific, but W11.N is explicitly a JSON direct
residual reopen under SPEC Section 15. Constraint: update the Lock 14 owner
allowance for W11.2 and do not touch generic crates outside the owner list.

## CH3 Regression / REDRESS

PASS with hard revert. Existing direct and typed admits are guards. Redress
must run the W5-W9 + W11.1 + W11.2 gate chain; if any admitted row silently
demotes or if no primary row admits, save `/tmp/skv13-waveW11.2-rejected.patch`
and revert behavior before committing a measured rejection.

## CH4 Cost

PASS. The implementation is a local generated-loop specialization and should
be under the <=450 LOC family cap. The companion report/gate cost is acceptable
because W11.N admissions require gate-consumed per-row evidence.

## CH5 Hidden Coupling

PASS with constraint. The helper `parse_object_value_at_direct` must remain
available for fallback and non-object callers. Redress must not duplicate sink
semantics in a new ad hoc helper, and must not alter number/string parsers or
the `JsonSink` trait.

## CH6 Anti-Paper-Close

PASS with hard admission bar. This wave is not allowed to close on "reduced
dispatch" alone. At least one primary open row must clear Track 1 >
same-run sonic strict + 1 with strict equality and Track 2 independence. If the
change only improves but does not admit, the wave is a measured rejection or
movement record, not an admit.

## Accepted Redress Contract

- Gate id: `G-W11.2-JSON-DIRECT-OBJECT-SCALARS`.
- Primary rows: `twitter`, `github_events`, `update_center` direct.
- Guard rows: `gsoc-2018`, `unicode_mixed`, `unicode_basic` direct plus all
  existing JSON A/GO rows.
- Same-wave consumer: generated JSON direct sink path exercised by
  `bbnf-bench::direct_struct`.
- Forbidden: fixture/corpus branch, new parser, digest shortcut, source hook,
  SIMD, directive/BIR/BackendShape/substrate change.
