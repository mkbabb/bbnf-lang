# SK-V14 W5B.4 Research B: Proof Carry

Date: 2026-05-26.
Scope: W5B.4 executable evidence and topology carry.
Output: proof list.

## Findings

W5B.4 closes W5B-FRONTEND only if the request consumer is load-bearing while
the provider/template topology remains unchanged. The exact local proof should
exercise the new closure gate in `emit_runtime_from_request()`, then carry the
W5A/W5B.1/W5B.2/W5B.3 witnesses that could regress from the codegen change.

The provider/template count remains nonzero before W5D-DELETE. That is expected
and must be preserved in W5B.4.

## Recommendations

- Add `w5b_frontend_request_consumes_lowered_ir_before_provider_rendering`.
- Preserve `w5a_runtime_contract_consumes_source_and_metadata`,
  `w5a_json_request_matches_emit_from_source`, and
  `w5a_sheets_bbnf_fail_closed_through_runtime_contract`.
- Carry the W5B.1/W5B.2/W5B.3 exact frontend tests.
- Run `cargo xtask regen-css` and the seven exact CSS companions after the
  codegen slice, because W5B.4 closes aggregate W5B-FRONTEND.
- Verify provider/template topology remains unchanged with the W5B count gate.

## Risks

`regen-css` may expose that some live CSS L4 source file still lacks a
frontend-closure fact that the synthetic W5A source carried. If so, the right
fix is to make the closure validation reflect the actual W5B exactness table,
not to bypass the request consumer.
