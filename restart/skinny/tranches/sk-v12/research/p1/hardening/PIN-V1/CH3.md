# SK-V12 S-P1 PIN-V1 CH3 - Regression / REDRESS

Verdict: ACCEPT

Score: 92%

## Blocking Findings

None.

## Review Notes

The pinned S-P1 profile fold preserves REDRESS discipline on the reviewed
surface. The pin authority keeps `parse_only` diagnostic-only and forbids SOTA
admission from it (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:84`,
`:88`, `:93`). The folded P1 docs carry the same boundary: P1-A states JSON
parse rows remain `S`/`L` NO-GO diagnostics and that the close target is
generated CSS L4 Track 1 strictly greater than `lightningcss_mbps + 1`
(`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:167`);
P1-C says JSON `parse_only` is never a SOTA admission target
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:118`);
P1-F classifies all 17 parse rows as diagnostic only
(`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:201`).

Union and ASM-gen are handled as campaign-unblocked categories, not erased
preblocks. The pin requires citation of REDRESS 96/97/98, material
differential, and CHALLENGE for union reattempts
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:50`, `:56`) and
keeps ASM-gen reattempts under micro-prove plus scalar/parity/checkasm and a
same-wave consumer (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:66`,
`:69`). P1-C restates the same gate for new union/ASM-gen candidates
(`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:83`, `:87`),
P1-E records prior REDRESS entries as historical evidence
(`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:203`,
`:208`), and P1-B says no SIMD, union, or ASM-gen route is scoped from PMU row
shape alone (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:216`,
`:223`).

The historical REDRESS entries remain factual authority. REDRESS 88 rejects
PMULL prefix-XOR on the hot body (`skinny/REDRESS.md:2510`, `:2540`); REDRESS 89
rejects the CTZ bulk consumer (`skinny/REDRESS.md:2544`, `:2585`); REDRESS 90
admits only B6 canary hardening while leaving PMULL and CTZ rejected
(`skinny/REDRESS.md:2589`, `:2597`). REDRESS 96 and 97 remain measured-rejected
union implementations (`skinny/REDRESS.md:2797`, `:2848`,
`skinny/REDRESS.md:2852`, `:2906`), and REDRESS 98 remains the gate retirement
for that SK-V9 union hypothesis, not an erased fact
(`skinny/REDRESS.md:2910`, `:2950`).

JSON guards and result deltas are reported rather than hand-waved. P1-F reports
that `skinny/RESULTS.md` is unchanged from SK-V11 close, with 17 parse rows, 17
direct rows, and 7 typed rows (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:24`,
`:36`), enumerates the unchanged row surface (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:80`,
`:87`), carries every row delta explicitly (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:101`,
`:143`), and records no `skinny/RESULTS.md` or `skinny/REDRESS.md` diff from
SK-V11 close (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:173`,
`:174`). P1-B separately warns that PMU can differ from Criterion and that
`skinny/RESULTS.md` remains row-admission authority
(`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:213`, `:215`).

## Nonblocking Notes

- P1-A and P1-E still contain stale partial-capture blocker bodies below final
  fold addenda. P1-A's final addendum establishes complete parse/xctrace/hot-leaf
  authority (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:23`,
  `:37`), but older text still says hot-leaf tables are absent
  (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:137`,
  `:145`). P1-E likewise establishes final hot-leaf authority
  (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:17`,
  `:31`) while retaining older unavailable rows (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:167`,
  `:193`). The explicit "supersedes" wording and capture manifest make this
  nonblocking, but the next fold should prune or mark the old sections as
  historical-only to avoid accidental downstream citation.
- P1-D's "S-P2 search space" language for escape-heavy direct rows is
  acceptable because the same section says the artifact proposes no route and
  requires micro-proof before assuming PMU stalls
  (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:249`,
  `:255`). Downstream plans should cite P1-D only as nomination evidence, not
  as intervention scope.
- The capture manifest's pin-aware addendum is clean: it records PMU/samply/
  xctrace/hot-leaf PASS coverage, says CSS L4 remains unprofiled because no
  generated CSS L4 Track 1 or lightningcss same-plane comparator exists, and
  keeps Mode III absent (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:22`,
  `:37`). Older `/tmp/skv12-p1` manifest body remains useful as historical
  reference only.

## Required Edits If REVISE

Not applicable. This CH3 disposition is ACCEPT.
