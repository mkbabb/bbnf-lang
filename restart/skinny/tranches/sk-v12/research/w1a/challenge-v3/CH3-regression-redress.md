# SK-V12 W1a CHALLENGE V3 - CH3 Regression / REDRESS

Date: 2026-05-20.
Lens: CH3 regression / REDRESS.
Disposition: ACCEPT.

## Finding

`PLAN-V3.md` closes the V2 CH3 blocker. The rejected-patch slice now includes
`skinny/crates/runtime/src/grammars/json/scan.rs`,
`skinny/crates/runtime/src/grammars/json/sink.rs`, and the narrow
`skinny/crates/passes/src/lib.rs` recognizer edit, plus generated JSON outputs,
`generated_real_typed.rs` if it moves, `skinny/RESULTS.md`, and
`skinny/REDRESS.md`.

The refreshed JSON guard requirement is correct for this selected route:
because W1a V3 touches JSON-producing codegen/runtime paths, PASS requires
`json_guard_state = refreshed:<run-id>:guards-pass`; a
`not_refreshed:no_behavior_drift` close is invalid.

The exact floor verifier is correctly additive to `gate-json`:
`gate-json --advisory --check-results` establishes rendered `RESULTS.md`
exactness, the cost-facts gate is also required, and the checked-in AWK
verifier enforces SPEC Section 0.5 direct and typed Track 1 / Track 2 floors.
Read-only check output:

`SK-V12 JSON guard floors PASS`

Current `skinny/RESULTS.md` remains JSON-only; no `css_l4`, `sheets`,
`bbnf_self`, `non_json`, `lightningcss`, or CSS row text was found. W1a still
does not admit CSS, compare lightningcss, move a non-JSON row, or close SK-V12.

## REDRESS 121 Requirements

REDRESS 121 must record W1a only under `G-W1a-GRAMMARCONFIG-LOCK14`.

Required PASS evidence:

- refreshed JSON guard run id;
- `gate-json --advisory --check-results`;
- `gate-json --with-cost-facts --check-results`;
- exact AWK floor verifier output;
- generated-size and LOC facts, including `scan.rs` / `sink.rs`;
- no CSS/non-JSON row movement in `skinny/RESULTS.md`.

On failure, save the V3 rejected slice at `/tmp/skv12-waveW1a-rejected.patch`
before reverting only the candidate W1a slice. The broad `json_templates`
directory path must still be inspected/split before revert so unrelated or
not-owned `json_templates/mod.rs` movement is not reverted accidentally.

## CH3 Disposition

ACCEPT. No CH3 revision is required before W1a redress dispatch.
