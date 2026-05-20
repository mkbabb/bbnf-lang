# SK-V11 W4 CHALLENGE V2 CH4 Cost

Date: 2026-05-20.
Lens: CH4 cost / measurement sufficiency.
Scope: W4 Plan V2 `container_tail_next_direct` for
`random/direct_to_struct`.
Output: this file.
Disposition: ACCEPT - redress authorized, row admission conditional.

## Authorities Read

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct-v2.md`.
- `restart/skinny/tranches/sk-v11/research/w4/challenge/w4-CH4-cost.md`.

## Verdict

ACCEPT for redress authorization. V2 closes the CH4 V1 planning defect by
turning the high-cost-risk route into a probe-first, fail-closed redress slice.
The cost likelihood remains low until measured, especially because CH4 V1 found
that `random/direct_to_struct` needed a large Track 2 lift from 6949 to 7878
Mbps, but that is no longer a reason to block redress outright.

The distinction is important: V2 authorizes implementation and measurement of
the D1 helper under REDRESS 115, not `RESULTS.md` movement. Row admission remains
blocked unless repeated same-host `profile_direct` probes clear `random` Track 2
above the 7878 Mbps floor with noise margin, direct guards remain above maintain
floors, typed guards are measured and hold, generated-vs-independent Track 2
digest equality passes, malformed tail fixtures reject in all four parsers, and
fresh Criterion plus `gate-json --with-cost-facts --check-results` consume the
same-wave W4 provenance.

## CH4 V1 Requirements Rechecked

| CH4 V1 requirement | V2 status |
|---|---|
| Downgrade D1 likelihood to probe-first / low confidence until Track 2 moves. | Satisfied. V2 makes row admission probe-first and forbids `RESULTS.md` movement before repeated same-host `profile_direct` evidence. |
| Require `random` Track 2 to clear the needed lift plus noise margin before admission. | Satisfied. V2 requires `random` Track 2 above the 7878 Mbps floor with noise margin before row movement. |
| Probe direct guards before binding Criterion. | Satisfied. V2 probes `citm_catalog`, `apache_builds`, `marine_ik`, and `unicode_basic` on both direct tracks and reverts on direct guard regression. |
| Explain why D1, not noise or string/whitespace/digest effects, accounts for admitted movement. | Satisfied as a redress condition. V2 requires REDRESS 115 tail-specific evidence for any admitted movement. |
| Run `regen-json` before `check-json` after `sink_direct.rs` edits. | Satisfied. V2 makes regeneration mandatory in the build/test command set. |
| Add gate/report validation for W4 floors and provenance. | Satisfied for authorization. V2 requires shared selected-row floor authority, stale-provenance rejection, direct guard floor tests, report validation, and same-wave W4 metadata. |
| Measure typed guards or justify why they are not needed. | Satisfied. V2 removes the ambiguity by requiring typed guard measurement and hold for the Section 0.5 typed rows. |

## Authorization Boundary

Redress may proceed because V2 binds the previously missing safety rails:

1. No row admission is allowed from an unprobed patch.
2. Probe failure on `random` Track 2 or any direct guard is a rejection path, not
   a paper close.
3. Criterion remains the binding admission evidence after probes, not a
   replacement for them.
4. REDRESS 115 must preserve the negative result if D1 fails to move the row or
   moves it for the wrong reason.
5. V2's revert protocol names the CH4 failure modes explicitly: probe miss,
   Criterion floor miss, direct or typed guard regression, and missing same-wave
   gate/report consumption.

This is sufficient for redress authorization even though cost risk remains high.
The plan is now falsifiable at the right boundary: implementation can test the
route, but the report cannot claim success unless the cost evidence clears the
same floors CH4 V1 demanded.

DISPOSITION: ACCEPT - redress authorized; `RESULTS.md` admission remains
conditional on the V2 probe, guard, Criterion, provenance, and REDRESS 115
evidence requirements.
