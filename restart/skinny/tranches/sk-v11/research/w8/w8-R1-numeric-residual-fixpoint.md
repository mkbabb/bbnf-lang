# SK-V11 W8 R1 Numeric Residual Fixpoint

Date: 2026-05-20.
Role: SK-V11 W8 research R1.
Scope: numeric/direct residual rows: `canada`, `mesh`, `instruments`,
`numbers`, and `gsoc-2018` only to the extent it is digit-heavy.
Artifact: read-only research disposition; no source, gate, report, RESULTS, or
REDRESS edits.

## Inputs Read

- `restart/skinny/tranches/sk-v11/SPEC.md` Section 12 and Section 0.4/0.5.
- SK-V11 W3 research, plan, challenge, and redress rejection.
- SK-V11 W4-W7 plans/challenge/redress dispositions as needed.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md` through item 118.
- SK-V11 P1 hot-leaf evidence in
  `/tmp/skv11-p1/direct-xctrace/exports/summary.json` and the folded
  `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`.

## Fixpoint Table

Direct floor is `ceil(sonic-rs strict direct Mbps / 1.10)`.

| Row | Current Track 1 / Track 2 / sonic / floor | Attempted SK-V11 candidate(s) | Outcome or exhaustion proof | Guard status | Legal W8a source candidate remains? |
|---|---:|---|---|---|---|
| `canada/direct_to_struct` | 10316 / 9819 / 11700 / 10637 | W3 `number_span_emit_slot` family only. W3 did not select `canada` for Criterion after mixed probes and selected the nearer `mesh` numeric floor. | Candidate exhaustion by measured sibling failure plus hot-leaf fit: `canada` is a number-sequence residual (`scan_digit_run` 23.7% Track 1, 21.6% Track 2; floor gaps -321/-818). The only accepted numeric source family was W3 and it missed `mesh` by >50%, so reusing it for a larger Track 2 gap would be paper-close. | No W8 source ran. Current global direct guards clear Section 0.5; `canada` is not itself a guard row. | No. A new numeric source would reopen the rejected W3 family or require a fresh CHALLENGE outside W8 R1. |
| `mesh/direct_to_struct` | 8561 / 8652 / 9542 / 8675 | W3 `number_span_emit_slot`: scalar generated JSON number-slot helper, number-slot parity tests, W3 gate/report provenance. | Measured rejection. `/tmp/skv11-w3-criterion` measured Track 1 3835, Track 2 3614, sonic 4413, serde 3191 against the 8675 floor. No row moved; patch saved and reverted. | `mesh/real_typed_struct` guard remains above Section 0.5 floors in W0 evidence: 9403 / 7897 vs 9214 / 7739. Global direct guards unchanged and clear. | No. This is the falsifying row for the only accepted numeric W3 source route. |
| `instruments/direct_to_struct` | 11569 / 10736 / 9865 / 8969 | No accepted W3 candidate selected it. W3 research marked `instruments` as W0-clamped; W5/W7 discuss string/whitespace/digest surfaces, but no accepted source proof exists. | Candidate exhaustion by clamp plus no-source proof. P1 hot leaves are tiny-string/whitespace/object dispatch, not a digit-heavy numeric residual (`tiny8` 15.7%, whitespace 15.7% Track 1; whitespace 19.7% Track 2). The row is above floor but remains `N-direct / NO-GO` under the W0 no-admission clamp until measured behavior-wave provenance exists; W3-W7 produced none for this row. | Not a Section 0.5 guard row. Current global direct and typed guard rows remain inherited and clear; no W8 source could regress them because none ran. | No. Admission by W0 numbers would violate the W0-clamp rule; no legal W8a behavior candidate is named by W3-W7 evidence. |
| `numbers/direct_to_struct` | 4479 / 2366 / 2667 / 2425 | W3 `number_span_emit_slot` was optional for `numbers`; probes showed Track 1 improvement in one probe but Track 2 regression, so the plan did not claim it. | Candidate exhaustion by selected-route rejection plus Track 2 floor failure. P1 hot leaves match W3's numeric family (`scan_digit_run` 26.8% Track 1, 27.5% Track 2), but W0 Track 2 is still -59 Mbps below floor and the row is W0-clamped. The only accepted numeric route failed on `mesh`; W5-W7 produce no reusable scalar proof. | Not a Section 0.5 guard row. Existing global guards remain unchanged and clear. | No. The only legal source family was W3 numeric, and it is measured rejected; W0-clamp bypass is pre-blocked. |
| `gsoc-2018/direct_to_struct` | 2665 / 2578 / 4110 / 3737 | No numeric candidate. P1 classifies it as `simd_movemask`/string-scan support, not `number_digit_span`; W5/W7 considered string/digest families generally but did not accept source redress. | Not digit-heavy for this R1 scope. P1 direct hot leaves are movemask 22.9%/21.6%, split-at support 12.9%/12.7%, tiny/skip-string support, with floor gaps -1072/-1159. W3 numeric evidence does not apply; W5-W7 are blocked or no-source and leave no reusable source oracle. | Not a Section 0.5 guard row. Existing global guards remain unchanged and clear. | No numeric W8a candidate. A string/SIMD candidate would be a different W8a scope and currently lacks CHALLENGE-accepted source authority. |

## Guard Summary

No W8 R1 source change ran, and W3/W4 source slices were reverted after their
measured rejects. The inherited Section 0.5 guard surface therefore remains the
current W0 evidence:

- Direct guards clear: `citm_catalog` 18563/17787 vs 18191/17431,
  `apache_builds` 11254/10189 vs 11028/9996, `marine_ik` 8938/9437 vs
  8759/9248, and `unicode_basic` 2299/2227 vs 2253/2182.
- Typed guards clear: `twitter`, `citm_catalog`, `apache_builds`,
  `github_events`, `update_center`, `mesh`, and `marine_ik` all remain above
  their Section 0.5 Track 1 and oracle floors in `skinny/RESULTS.md`.

## W8a Disposition

No legal W8a source candidate remains for this numeric/direct R1 slice. The
candidate set is exhausted as follows:

1. The only accepted numeric source route, W3 `number_span_emit_slot`, was
   measured and rejected on `mesh/direct_to_struct`.
2. `canada` and `numbers` share the same numeric hot-leaf family and have no
   stronger both-track floor-closure evidence than the rejected `mesh` route.
3. `instruments` is W0-clamped and not digit-heavy enough to justify recycling
   W3 numeric source work.
4. `gsoc-2018` is not a digit-heavy numeric row; its movemask/string-scan
   profile is outside this R1 numeric source scope.
5. W4 is a measured `random` container-tail rejection, W5 and W6 are blocked
   before source dispatch, and W7 is an accepted no-source block. None supplies
   a reusable scalar oracle, independent Track 2 mechanism, or gate-consumed
   provenance for these rows.

The legal W8 action for these rows is documentation/redress accounting, not
W8a source dispatch.
