# SK-V11 W8 R3 Unicode Residual Fixpoint

Scope: read-only accounting for W8 Section 12 direct residual fixpoint rows:
`unicode_mixed`, `unicode_escapes`, `y_string_unicode`, `distinct_values`, and
`gsoc-2018` where string/escape-heavy. No source route is selected here.

## Contract Read

SPEC Section 12 makes W8 a direct residual fixpoint and row reclamation wave.
Default surface is no new primitive; source work is outside W8 unless split as
W8a after CHALLENGE accepts exactly one final narrow candidate and row subset.
Rows may move only if both generated Track 1 and independent Track 2/oracle
meet the Section 0.4 floor. Misses require an uncloseable proof naming attempted
candidate, measured tracks, comparator, floor, and guard status.

REDRESS 116 blocks W5 before source dispatch: W5 admitted no span API and no
rejected-but-reusable scalar proof. REDRESS 117 blocks W6 before source
dispatch: W6 admitted no escaped-segment primitive, no x4 production consumer,
no source-method digest fold, and no reusable oracle. REDRESS 118 blocks W7
before source dispatch: W7 admitted no output digest/hash host-sink
optimization, no row movement, and no reusable oracle. SK-V10 REDRESS 107 is
proof-only C6 evidence for the already-consumed `unescape_string ->
unescape_four_unicode_escapes -> unescape_uxxxx_x4_neon` path. SK-V10 REDRESS
108 rejects production reuse because that exact caller already consumed x4; a
wrapper, constant, or feature re-gate is not a source delta.

## Row Fixpoint Table

Floor is `ceil(sonic-rs direct / 1.10)`. Values are the current
`skinny/RESULTS.md` direct rows.

| Row | Current Track 1 / Track 2 / sonic / floor | Attempted candidates | Measured or blocked evidence | Guard status | Legal W8a source candidate remains? |
|---|---:|---|---|---|---|
| `unicode_mixed/direct_to_struct` | 3753 / 2427 / 2846 / 2588 Mbps | W5: residual monitor. W6: selected `unicode_mixed` escaped-segment digest-fold / decoded-byte source-method plan. W7: considered only as unicode residual, not digest row. | Current row remains `N-direct / NO-GO`: Track 1 is above floor, Track 2 is 161 Mbps short and the row is W0-clamped. P1 hot leaves are full string, unescape, and escape validation on both tracks. SK-V10 REDRESS 107 found zero eligible C6 x4 slices because apparent `\u` text is escaped-backslash data, not JSON Unicode escape syntax. REDRESS 117 blocks the W6 decoded-byte source-method fold as REDRESS 54 replay; no source patch or Criterion row movement occurred. | Not admitted. W0-clamped residual; not a guard. Existing direct guards are separate rows (`citm_catalog`, `apache_builds`, `marine_ik`, `unicode_basic`). | No. The only plausible W6 source seam was blocked by REDRESS 117, x4 has zero eligible mixed input, and W8 cannot introduce source without a fresh CHALLENGE-accepted candidate. |
| `unicode_escapes/direct_to_struct` | 1345 / 1341 / 3785 / 3441 Mbps | W5: unicode residual monitor. W6: dense `\uXXXX` scout / uncloseable-proof candidate, not first selected row. SK-V10 W8/W9: existing x4 proof and rejected production reuse. W7: not digest eligible. | Both tracks need about 2.56x current throughput to reach floor. P1 hot leaves are unescape, full string, and read-hex/hex-unit. SK-V10 REDRESS 107 proved x4 only as micro-proof with `unicode_escapes` 2.636x inside the proof slice, but REDRESS 108 targeted direct Criterion still failed production floors: 5207 / 5234 / 14315 / 12527 Mbps in SK-V10 units. SK-V11 W6 carried this forward and blocked production because reusing existing `unescape_string` or wrapping x4 is not a same-wave source delta. | Not admitted. Residual row; not a guard. | No. Dense hex evidence exists only as proof/background. Same caller production reuse is pre-blocked by REDRESS 108 and W6 produced no new escaped-segment consumer. |
| `y_string_unicode/direct_to_struct` | 1983 / 1029 / 4344 / 3950 Mbps | W5: deferred to W6 unless explicitly selected. W6: narrow array-string proof / likely uncloseable monitor. SK-V10 W8/W9: x4 proof slice and rejected production reuse. W7: not digest eligible. | Track 1 needs 1967 Mbps and Track 2 needs 2921 Mbps, with Track 2 about 3.84x short of floor in W6 analysis. P1 hot leaves are hex nibble, read-hex/hex-unit, unescape, and unicode validation. SK-V10 REDRESS 107 proof slice measured only 0.943x on `y_string_unicode`; REDRESS 108 production Criterion still failed direct floors: 5096 / 3723 / 8851 / 8027 Mbps in SK-V10 units. No SK-V11 source patch or row movement occurred. | Not admitted. Residual row; not a guard. | No. The clean array-string shape remains an uncloseable monitor without a new accepted consumer and cannot reuse x4 proof as production. |
| `distinct_values/direct_to_struct` | 1750 / 1625 / 2923 / 2658 Mbps | W5: micro-proof sentinel for tiny/string/whitespace/digest, not first row gate. W7: strongest residual digest-profile antecedent, but no admissible digest row. | Both tracks are far below floor: -908 Mbps Track 1 and -1033 Mbps Track 2 versus floor. P1 hot leaves are tiny string, whitespace, fold string / hand tiny, and support cost. W7 found visible digest bucket evidence too small: even perfect visible-bucket removal does not close both tracks, and Track 2 remains limited elsewhere. REDRESS 118 records no W7 source patch, no row move, and unchanged advisory `N-direct / NoGo` surface. | Not admitted. Residual row; not a guard. | No. It is not unicode/escape-limited, and the only plausible W7 digest angle is floor-insufficient and blocked by REDRESS 118. |
| `gsoc-2018/direct_to_struct` | 2665 / 2578 / 4110 / 3737 Mbps | W5: support scout only, movemask/special-byte dominated. W7: explicitly not digest/hash eligible. No W6 unicode route selected. | Both tracks miss floor by 1072 / 1159 Mbps. P1 hot leaves are movemask, split-at/split_checked, tiny/plain string. The row is string-scan-heavy but not unicode-escape-heavy; W5 did not select it and REDRESS 116 left no reusable span proof. W7 eligibility review groups it with non-digest residuals and rejects it as not limited by output digest/hash. | Not admitted. Residual row; not a guard. | No for the W8a unicode/string-escape scope. A future movemask/string-scan route would need a new tranche/spec route, not W8a residual reclamation from W5-W7 evidence. |

## Fixpoint Conclusion

No legal W8a source candidate remains for this scope. W5/W6/W7 each ended as a
measured or challenged entry block with no source patch and no reusable scalar
oracle. The requested rows therefore stay at their current `skinny/RESULTS.md`
state: `N-direct / NO-GO` for direct residuals, with parse-only rows still
`S / NO-GO` where present. W8 can close only as docs/gate/result accounting
unless a future CHALLENGE outside this read-only artifact names a materially new
source delta, exact row subset, independent Track 2/oracle proof, same-run
comparators, guard floors, and floor-clearing measurements.
