# SK-V12 Pass Alpha Hardening V3 - CH3 Regression / REDRESS

Date: 2026-05-20.
Reviewer: CH3 regression / REDRESS.
Scope: USER PIN re-bracket for SK-V12 Pass Alpha V3.

## Disposition

PASS.

No V3 regression finding blocks G-Alpha presentation. The V2 CH3 surface remains
preserved, and the V2 CH5-required Alpha-E folds do not reopen stale REDRESS
routes.

## Findings

1. JSON guard refresh is now local to W1a as well as global. The global rule says
   any wave changing generic runtime, codegen, generated-output, benchmark,
   report, or gate paths that can produce JSON must refresh JSON guards or record
   measured REDRESS demotion; the no-refresh shortcut is limited to proving no
   JSON-producing path moved and `skinny/RESULTS.md` unchanged
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:58`). Alpha-E now repeats that rule
   inside E2/W1a itself: because W1a owns generic runtime, codegen,
   generated-output, benchmark, report, and Lock 14 paths, direct/typed JSON
   guards refresh or record measured REDRESS demotion unless the no-JSON-path-moved
   shortcut applies (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:179`).

2. Zero carried orphan is a close requirement for both ADMIT and FIXPOINT. The
   carried set is named in the user pin
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:73`) and in the
   SIMD audit as five orphans
   (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34`).
   SYNTHESIS requires the set to be admitted, removed, or inventory-demoted with
   evidence for ADMIT and FIXPOINT (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:61`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:87`), HANDOFF mirrors it
   (`restart/skinny/tranches/sk-v12/HANDOFF.md:82`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:93`), and Alpha-E E5 fail-closes on
   orphaned production bodies (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:333`).

3. REDRESS adjacency is explicit and does not re-import old category blocks.
   The user pin unblocks union and ASM-gen categories while retaining REDRESS
   96/97/98 and 88/89/90 as historical measured implementations
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`,
   `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58`). Alpha-E E4
   names the material differential from REDRESS 96/97/98 and constrains the new
   route to generated CSS-local same-tape use
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:233`).
   Alpha-E E5 names the material differential from REDRESS 88/89/90 and avoids
   PMULL default, CTZ bulk emit, and canary-as-row-movement repeats
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:288`).

4. Rejected patch coverage is sufficient for the implementation seeds. Alpha-E
   records rejected patch paths for W1a, W1b, W2, W3, and W4
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:135`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:188`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:227`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:282`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:341`).
   SYNTHESIS and Alpha-F carry the same G-Alpha seed failure paths
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:266`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:221`).

5. W0 revalidation is preserved rather than reopened. SYNTHESIS states the W0
   telemetry/gate lock at `f788eb97` is revalidated, not redone, and a drift miss
   returns to S-P3 instead of authorizing Alpha-F to rewrite W0
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:228`). HANDOFF repeats the same
   boundary (`restart/skinny/tranches/sk-v12/HANDOFF.md:97`), and Alpha-F carries
   it into the required S-P3 contract and G-Alpha seed
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:166`,
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:220`).

6. SIMD/ASM preconditions are fail-closed. The totality scout records the
   `escape_mask_64` falsifier and blocks SIMD SOTA claims until parity is
   restored (`restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md:71`,
   `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md:182`).
   SYNTHESIS and HANDOFF require `escape_mask_64` verification before new SIMD
   admission (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57`,
   `restart/skinny/tranches/sk-v12/HANDOFF.md:121`). Alpha-E E3 is
   correctness-only and blocks throughput claims (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:193`),
   while E5 requires E3 where the consumer touches string/escape scanning,
   scalar/checkasm, microbench movement, same-wave CSS consumer, JSON guard
   refresh/demotion when JSON-producing paths move, and zero orphan disposition
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:327`).

## Regression Verdict

PASS. V3 does not reopen REDRESS 88/89/90, 96/97/98, 112/113, or 114-120 without
the user-pin-required material differential and measured gate. JSON guard
refresh, local W1a demotion discipline, zero carried orphan, rejected-patch
coverage, W0 revalidation, and SIMD/ASM preconditions are present with resolving
file/line authority.
