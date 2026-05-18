# SK-V8 W6 Close And Alpha Feedback

Date: 2026-05-18.

Status: closure candidate pending W6 hardening convergence. This artifact does
not dispatch SK-V9 implementation.

## Close Decision Candidate

SK-V8 is ready to close if W6 hardening accepts this packet twice in
consecutive cycles at >=95% confidence:

- W0 established the `SK-V8-open` telemetry/report gate.
- W1 admitted CostFacts and strict comparator gate binding.
- W2 admitted source/product parity only, not measured row-table expansion.
- W3 rejected/routed Tier A tape plus structural projection before source
  redress.
- W4 rejected/routed Track 2 scalar-parent folding after selected-row
  falsification.
- W5 admitted only the named Lock 14 provider-boundary cleanup.
- W6 reconciles the ledgers and routes residuals to SK-V9 or Pass Omega.

## Wave Disposition Matrix

| Wave | Final SK-V8 status | Authority | RESULTS/REDRESS effect |
|---|---|---|---|
| W0 | Closed/admitted telemetry close | `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V12/HARDENING-W0-V12-CONSOLIDATED.md` | `skinny/RESULTS.md` carries 38 `SK-V8-open` manifest rows. |
| W1 | Closed/admitted gate binding | commit `c6345e4d` and `restart/skinny/tranches/sk-v8/HANDOFF.md` W1 record | No `skinny/RESULTS.md` change. |
| W2 | Closed; source/product parity admitted, benchmark row-table admission rejected | `restart/skinny/tranches/sk-v8/research/wave-2-hardening/V5/HARDENING-W2-V5-CONSOLIDATED.md`; REDRESS 91 | `skinny/RESULTS.md` unchanged; Apache/CITM are source/product rows only. |
| W3 | Rejected/routed | `restart/skinny/tranches/sk-v8/research/wave-3-hardening/V1/HARDENING-W3-V1-CONSOLIDATED.md`; REDRESS 92 | No source, patch artifact, or row-table admission. |
| W4 | Rejected/routed | `restart/skinny/tranches/sk-v8/research/wave-4-hardening/V4/HARDENING-W4-V4-CONSOLIDATED.md`; REDRESS 93 | Source candidate reverted; `skinny/RESULTS.md` unchanged. |
| W5 | Closed/admitted named Lock 14 cleanup | `restart/skinny/tranches/sk-v8/research/wave-5-hardening/V5/HARDENING-W5-V5-CONSOLIDATED.md` | No generated output, row-table, performance, or `skinny/RESULTS.md` change. |
| W6 | Close candidate | this artifact plus W6 hardening | No source, generated output, RESULTS, or REDRESS change unless challenge finds a mismatch. |

## Ledger Reconciliation

`skinny/RESULTS.md` remains the W0 report authority:

- Manifest count: 38 `SK-V8-open` rows.
- Measured real-typed count: four `real_typed_struct A / GO` rows.
- Measured real-typed rows: `twitter`, `update_center`, `mesh`, and
  `marine_ik`.
- Overall outcome: `N-direct / NoGo`.
- Track 2 remains the independent hand-coded parser over `runtime::tape`.

`skinny/REDRESS.md` remains aligned:

- REDRESS 91 records W2 source/product parity admission plus W2 row-table
  rejection.
- REDRESS 92 records W3 rejection/routing.
- REDRESS 93 records W4 rejection/routing.

No W6 REDRESS entry is needed because W6 finds no missing rejection, mismatch,
or row/report correction. No W6 `RESULTS.md` edit is needed because no measured
row status changes in W6.

## Verification Evidence

Repository-root checks:

- `git diff --exit-code HEAD -- skinny/RESULTS.md skinny/REDRESS.md` passed
  with no output.
- Manifest counter returned `manifest_rows=38` and `real_typed_rows=4`.
- `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` exists.
- W0, W2, W3, W4, and W5 closure authority files exist.
- `cargo xtask regen --check` passed: `clean (9 of 9 grammars matched)`.

`skinny/` checks:

- `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 11/11.
- `cargo xtask check-json` passed.
- `cargo xtask check-real-typed` passed.
- `cargo xtask check-conformance` passed: 21 valid fixtures accepted and 7
  invalid fixtures rejected.

## Alpha Feedback

SK-V8 produces these downstream candidates and residuals:

- SK-V9 typed row-table candidate: Apache and CITM may become measured rows only
  in a new wave with fresh row evidence and a gate that owns run-id/metadata
  validation.
- SK-V9 structural parse candidate: before any renewed structural-heavy parse
  wave, define the retained class/event grammar and prove the retained
  `ValueRef` cursor contract over numbers, literals, container events, and
  string quote ownership.
- SK-V9 direct candidate: direct digest misses route to a direct output
  contract or control-path tranche; digest-only evidence remains guard-plane
  evidence, not product proof.
- Pass Omega residual: SC-6-L1-R1 remains unratified and unproven under Lock 1
  as written, so it routes to Pass Omega.
- Pass Omega residual: broad lock amendments, canonical path cleanup, and
  top-level surface refresh stay outside SK-V8 W6.

## Close Falsifiers

Any W6 challenger must return REVISE if it finds:

- a missing wave disposition;
- Apache/CITM presented as measured `RESULTS.md` rows;
- W3 or W4 presented as admitted source work;
- W5 presented as performance or row-table movement;
- SC-6-L1-R1 silently ratified;
- source, generated output, benchmark data, `RESULTS.md`, or `REDRESS.md`
  changed by W6 without a mismatch-specific plan;
- a repository-local document path in this close packet that does not resolve.

## Dispatch Boundary

Closing SK-V8 does not authorize SK-V9 implementation. After W6 closes, SK-V9
may be planned through Pass Alpha and the skinny pass substrate, challenged,
and presented for a new G-Alpha decision. No SK-V9 wave dispatches until that
G-Alpha is closed.
