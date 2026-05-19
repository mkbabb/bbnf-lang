# SK-V10 G-Alpha Presentation

Date: 2026-05-19.

Status: `G-ALPHA-SK-V10` closed by standing user directive to continue through
the full skinny plan. This artifact records the Alpha close and dispatches S-P1
Profile only. It is not an implementation dispatch.

## Authority

- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/research/alpha-hardening/V1/CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` entries 94-98

## Decision

Decision recorded: `G-ALPHA-SK-V10` closed.

Rationale:

- Alpha-A through Alpha-F exist and are folded into the SK-V10 packet.
- Alpha CHALLENGE V1 initially returned REVISE, then accepted after all defects
  were folded.
- W3 is retired by REDRESS 98 and cannot be retried under a renamed substrate
  gate.
- Parse-only stays `S / NO-GO` and cannot close SOTA while the validation path
  remains borrowed-view/deferred.
- Candidate gates now carry initial LOC budgets, hard caps, row floors,
  same-wave consumers, and REDRESS/revert disposition.

## Current Row Surface

| Family | State | SK-V10 posture |
|---|---:|---|
| `parse_only` | 17 `S / NO-GO` | diagnostic only; no SOTA close target |
| `direct_to_struct` | 3 `A / GO`, 14 `N-direct / NO-GO` | primary JSON frontier after direct-specific profile and contract |
| `real_typed_struct` | 6 `A / GO` | validated typed-product surface under current deferred/view-boundary gate |

## Goalset

| Candidate | Purpose | Admission boundary |
|---|---|---|
| Direct output/control-path contract | Diagnose and make direct rows product-contract-capable. | S-P1 direct profile plus S-P2/S-P3 contract; no digest row moves without output-plane equivalence, independent Track 2/oracle status, and same-run comparator evidence. |
| `instruments` typed product admission | Add the first bounded typed row beyond W1. | Full fixture generated/serde/sonic/Track 2 checksum parity and same-run typed comparator rows. |
| Root-type typed generalization | Unlock `github_events` and `gsoc-2018` typed roots. | Root proof may close without row movement; row movement requires same-wave measured typed comparator evidence. |
| Existing-substrate unicode/string kernels | Salvage W4 kernels without W3. | Same-host micro-prove-first, scalar reference, checkasm, current production caller, and W10b maintain floors. |
| Comparator and telemetry refresh | Create SK-V10-open report identity and freshness evidence. | Gate-only unless a same-wave behavior gate admits/rejects rows. |

## Protected Constraints

- No W3 union/event substrate retry, split, rename, or class-column variant.
- No W4 cascade through W3.
- No parse-only SOTA admission while rows remain `S / NO-GO`.
- No direct digest row relabeled as typed product proof.
- No strict-admission claim until `gate-json` consumes measured strictness and
  validation-path changes.
- No substrate/kernel candidate reaches S-P3 without same-host micro-proof.
- No generic-crate, codegen, or runtime-outside-json edit may leak JSON policy
  or lack named CSS L4 / Sheets / BBNF-self proof.

## Authority Granted

`G-ALPHA-SK-V10` authorizes S-P1 Profile only. S-P1 must profile
`direct_to_struct` first and may write research artifacts under
`restart/skinny/tranches/sk-v10/research/p1/`.

It does not authorize:

- SK-V10 source implementation;
- `SPEC.md` or `DISPATCH-PROMPT.md` before S-P3;
- row movement in `RESULTS.md`;
- any W3/W4 behavior wave before fresh S-P3 entry gates.
