# SK-V12 W2 CH3 - Regression / REDRESS

Disposition: REVISE.

The REDRESS differential is mostly sound. W2 avoids REDRESS 28/33 tiny-string
wiring, REDRESS 88 PMULL default-body replay, REDRESS 89 CTZ/bulk-emission
replay, x86 work, and throughput admission.

Blocking issues:

- JSON guard commands are incomplete if W2 touches JSON-producing behavior.
  The plan names `bench-json --advisory` and `gate-json --check-results` but
  omits the cost-facts check and the SK-V12 floor verifier used by the accepted
  W1a regression gate.
- Runtime scanner source-fix ownership is unresolved. W2 may test
  `scan_structurals`, but `runtime/src/grammars/json/scan.rs` is outside SPEC
  Section 5 owner paths.
- Revert protocol must save the rejected patch before any revert and limit the
  diff to W2-owned paths.

Required revision: make runtime scanner source edits fail-closed unless
ownership is expanded; make full JSON guard commands conditional on behavior
movement or explicit owner expansion; and specify
`git diff --binary HEAD -- <W2-owned paths> > /tmp/skv12-waveW2-rejected.patch`
before reverting a failed redress slice.
