# SK-V12 W1a CHALLENGE V2 - CH1 Correctness

Date: 2026-05-20.
Lens: CH1 correctness.
Disposition: REVISE.

## Findings

1. Exact roster still has a correctness gap. `PLAN-V2.md` declares `scan.rs`
   / `sink.rs` JSON-owned source, removes them from the generated roster, and
   says their generated headers should be removed or replaced, but the editable
   source roster and revert slice do not include those files. Current code
   still emits/checks them as generated outputs. Add
   `skinny/crates/runtime/src/grammars/json/scan.rs` and
   `skinny/crates/runtime/src/grammars/json/sink.rs` to the owned source/revert
   roster, or state explicitly that they are read-only and no source-comment
   header change is required. Also remove the conditional
   `json_templates/mod.rs if required` from the exact roster or make it a
   concrete owned/not-owned decision.

2. The seven-leak closure matrix is materially present and acceptable in
   shape. It names all seven leak classes, legal homes, generic-root
   rejection, JSON-owned positive allowance, and provider rejection before JSON
   rendering. Redress must still make this executable with Lock 14 negative
   tests, JSON-owned positive tests, and a non-JSON provider-selection failure
   before `json_provider`, `json_sink_direct`, or `json_typed_direct` can emit.

3. The orphan config/profile-field check is still prose, not an executable
   check. `PLAN-V2.md` says unused profile/config fields fail W1a, but the
   verification section names no unit test, snapshot check, or gate helper that
   enumerates config/profile policy fields and proves same-wave generated
   consumers. Add an explicit check that fails when a `GrammarProfile` or
   generated `config.rs` field used to satisfy the leak matrix has no consumer
   in generated JSON output, JSON direct/typed output, or the Lock 14 gate path.

4. The SK-V12 guard floor verifier is correct for CH1 when paired with
   `gate-json --check-results`. The AWK script hardcodes all SPEC Section 0.5
   direct and typed floors, reads the current `RESULTS.md` Track 1 / Track 2
   columns correctly, fails missing rows and below-floor rows, and passes the
   checked-in results.

## CH1 Disposition

REVISE, not REJECT. V2 fixes the main V1 correctness direction, including the
mandatory floor command and refreshed JSON guard requirement, but CH1 should
not accept until the scan/sink ownership mismatch and executable orphan-field
check are fixed.
