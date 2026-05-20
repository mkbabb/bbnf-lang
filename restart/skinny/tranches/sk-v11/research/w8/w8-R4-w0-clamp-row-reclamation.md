# SK-V11 W8 R4 - W0 Clamp Row Reclamation

Date: 2026-05-20

Scope: `instruments/direct_to_struct`, `numbers/direct_to_struct`,
`unicode_mixed/direct_to_struct`, and any direct residual row that already
numerically passes one or both SK-V11 Section 0.4 floors.

## Verdict

No row can be admitted by W8 docs/gate-only accounting.

W8 may close the direct residual surface only by either:

1. admitting a row with W3-W8 measured provenance under the strict direct
   contract, or
2. recording a measured REDRESS uncloseable proof for rows that remain
   residual.

For the three W0-clamped rows, the W0 numbers are planning evidence only.
`instruments` numerically clears both SK-V11 seed floors, while `numbers` and
`unicode_mixed` clear Track 1 only. None of those facts is sufficient for row
movement because SPEC Section 0 item 4 and Section 12 require behavior/gate-wave
measured provenance before W0-clamped rows can admit.

## Authority Read

- SPEC Section 0 item 3 requires every residual direct row to become strict
  same-run `A / GO` on generated Track 1 plus independent Track 2/oracle, or
  receive a per-row measured uncloseable REDRESS proof.
- SPEC Section 0 item 4 explicitly clamps `instruments`, `numbers`, and
  `unicode_mixed`: positive W0 deltas are planning evidence only until a
  behavior or gate wave records measured provenance.
- SPEC Section 0.4 defines the direct floor as
  `ceil(sonic-rs strict direct Mbps / 1.10)` and names the residual row floors.
- SPEC Section 0.5 guard rows are maintain gates, not a shortcut for residual
  row admission.
- SPEC Section 12 says W8 re-evaluates remaining direct residuals under the
  strict direct contract, admits only rows that meet Section 0.4 on both tracks,
  and admits W0-clamped rows only with W3-W8 measured provenance.
- HANDOFF repeats that `instruments`, `numbers`, and `unicode_mixed` are
  W0-clamped planning rows and that W8 may dispatch only through direct
  residual fixpoint and row reclamation with W2-W7 dispositions carried forward.
- REDRESS 111-118 leave no reusable row-moving source proof: W1a admitted only
  a companion non-JSON gate/report lane; W1b rejected the generated non-JSON
  baseline; W2 is blocked; W3 and W4 rejected measured JSON direct candidates;
  W5, W6, and W7 were blocked before source redress.

## Numeric Reclamation Matrix

| Row | Track 1 | Track 2 | Floor | Numeric state | W8 docs/gate-only disposition |
|---|---:|---:|---:|---|---|
| `instruments/direct_to_struct` | 11569 | 10736 | 8969 | Both tracks clear | Not admissible from W0. Requires W3-W8 measured provenance, strict/measured-row fields, REDRESS provenance, and gate consumption. |
| `numbers/direct_to_struct` | 4479 | 2366 | 2425 | Track 1 clears; Track 2 misses by 59 | Not admissible. Also fails the both-track floor rule. Requires measured fixpoint proof or a future legal W8a source route. |
| `unicode_mixed/direct_to_struct` | 3753 | 2427 | 2588 | Track 1 clears; Track 2 misses by 161 | Not admissible. Also fails the both-track floor rule, and REDRESS 117 blocks the selected escaped-segment route. |

I found no other Section 0.4 direct residual row that already clears even one
floor in `skinny/RESULTS.md`. The near misses remain below floor on both tracks:
`mesh` misses by 114 / 23 and `random` misses by 185 / 929.

## Gate/Report Implications

The executable gate/report shape agrees with the docs:

- W0 report rows are validated as `strictness=deferred`,
  `parse_utf8=view-boundary`, `measured_validation_path=view-boundary`,
  `redress_entry=none`, and `wave_id=SK-V9-open`.
- Strict direct admission requires `strictness=strict`,
  `parse_utf8=measured-row`, `measured_validation_path=measured-row`,
  `escape_complete=yes`, digest output plane, independent Track 2, a non-gate-only
  same-wave consumer, REDRESS provenance, and a non-`SK-V9-open` wave id.
- The W0 direct signal text already distinguishes clamped rows:
  "fresh direct guard passes are not behavior evidence in W0."

Therefore a W8 docs/gate-only patch that merely relabels W0 rows would violate
both the SPEC and the validator model. It would be a W0 clamp bypass and a paper
fixpoint, both pre-blocked in SPEC Section 12.

## Required Route

W8 can produce a valid close artifact without row admission only if it records
per-row fixpoint proof for the remaining residuals: attempted candidate,
measured Track 1, measured Track 2/oracle, comparator, Section 0.4 floor, and
guard status.

If an actual row admission is desired, W8 must first split to W8a or otherwise
obtain accepted W3-W8 measured provenance for exactly the named row subset.
For this R4 scope, that means:

- `instruments`: possible only with fresh W3-W8 strict measured-row evidence,
  despite the W0 both-track pass.
- `numbers`: not possible on current W0 numbers because Track 2 misses floor.
- `unicode_mixed`: not possible on current W0 numbers because Track 2 misses
  floor and the W6 escaped-segment/source-method path is blocked by REDRESS 117.

Close recommendation: do not admit any R4 row by docs/gate only. Treat
`instruments`, `numbers`, and `unicode_mixed` as fixpoint-proof rows unless a
new accepted W8a source/measurement packet supplies strict measured provenance
and clears both floors.
