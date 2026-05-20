# SK-V12 Pass Alpha Hardening V4 - CH3 Regression / REDRESS

Date: 2026-05-20.
Reviewer: CH3 regression / REDRESS.
Scope: V4 review after `3e5dd574` pin-aware G-Alpha fold.

## Disposition

PASS.

No V4 regression finding blocks G-Alpha presentation. The V3 CH3 pass surface
still holds, and the V3 consolidated CH6 blocker was folded in the standalone
G-Alpha presentation without reopening stale REDRESS routes.

## Sources Read

- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`
- `restart/prompts/pass-contracts/PASS-ALPHA.md`
- `restart/audit/hardening/HARDENING-CONSOLIDATED-V3.md`
- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v12/HANDOFF.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`
- `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md` tail through REDRESS 120
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
- `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md`

## Findings

1. G-Alpha consistency is restored. V3 consolidated hardening required replacing
   the stale G-Alpha presentation with a pin-aware packet carrying CSS L4 first,
   strict `> lightningcss_mbps + 1`, S-P1/S-P2/S-P3 plus W0-W5, telemetry,
   zero-carried-orphan, and category-unblocked union/ASM rules
   (`restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CONSOLIDATED.md:28`).
   The current G-Alpha file now marks status `PENDING V4 HARDENING`, explicitly
   says it is not a `G-Alpha PASS` record, limits authority to the pass sequence,
   and carries the pin-aware close contract, telemetry list, and seed table
   (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:7`,
   `:9`, `:38`, `:50`, `:88`, `:111`, `:126`).

2. JSON guard refresh is present globally and locally inside W1a. SYNTHESIS
   requires refresh or measured REDRESS demotion whenever generic runtime,
   codegen, generated-output, benchmark, report, or gate paths that can produce
   JSON move; the no-refresh shortcut requires proof that no JSON-producing path
   moved and `skinny/RESULTS.md` is unchanged
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144`). HANDOFF carries the same
   guard rule (`restart/skinny/tranches/sk-v12/HANDOFF.md:58`). Alpha-E applies it
   to W1b (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:125`)
   and, critically, to W1a/E2 itself because W1a owns generic runtime, codegen,
   generated-output, benchmark, report, and Lock 14 paths
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:179`).

3. Zero carried orphan is a close requirement for both ADMIT and FIXPOINT. The
   user pin names the five carried aarch64 orphan primitives and sets the SK-V12
   target to zero orphan kernels
   (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71`). The SIMD
   audit confirms the same five-orphan set
   (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34`,
   `:193`). SYNTHESIS, HANDOFF, Alpha-F, Alpha-D, Alpha-E/E5, and G-Alpha all
   require admission, removal, or inventory demotion with evidence before close
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:61`, `:87`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:82`, `:93`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:95`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md:191`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:333`;
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:68`).

4. REDRESS adjacency is explicit and does not reimport old category blocks.
   Alpha-C correctly keeps REDRESS 96/97/98 and 88/89/90 as historical specific
   rejects while reopening union and ASM-gen categories only with material
   differential, CHALLENGE, scalar/reference or checkasm parity, same-wave
   consumer, and measurement
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:63`,
   `:96`, `:228`, `:249`). Alpha-E's E4 and E5 candidates name the material
   differentials from those REDRESS families and forbid public substrate,
   sidecar, proof-only, dispatch-only, or orphan variants
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:236`,
   `:266`, `:293`, `:327`). SYNTHESIS, HANDOFF, Alpha-F, and G-Alpha agree that
   the categories are unblocked at category level but the specific rejected
   implementations remain evidence to cite and differentiate
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:219`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:66`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:202`;
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:79`).

5. REDRESS 112/113 and REDRESS 119 are fenced correctly. CSS L4 must be attempted
   first under the pin, but REDRESS 112/113 are superseded only for that explicit
   CSS mandate and do not authorize report-only or future-phase close
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:120`).
   REDRESS 119 remains the JSON direct residual authority: residual JSON rows are
   guard-only unless a later pass supplies fresh profile, micro-proof, and
   material evidence beyond REDRESS 114-119
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:167`,
   `:257`; `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:353`).
   The tail of REDRESS records W8/W9 as no row movement and SK-V11 as measured
   fixpoint, not direct `GO`
   (`skinny/REDRESS.md:3495`, `:3529`).

6. Rejected patch coverage is sufficient for the executable seeds. Alpha-E
   records rejected patch paths for W1b, W1a, W2, W3, and W4
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:135`,
   `:188`, `:227`, `:282`, `:341`). SYNTHESIS, HANDOFF, Alpha-F, and G-Alpha
   carry the same W0-W5 failure-action table, with W0 returning to S-P3 on drift
   and W5 routing to close/Alpha feedback rather than behavior patching
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:260`;
   `restart/skinny/tranches/sk-v12/HANDOFF.md:127`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:215`;
   `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:113`).

7. W0 revalidation is preserved and not reopened. SYNTHESIS says W0
   telemetry/gate lock at `f788eb97` is revalidated, not redone, and a
   revalidation miss returns to S-P3 rather than authorizing Alpha-F to rewrite W0
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:228`). HANDOFF and Alpha-F carry
   the same boundary (`restart/skinny/tranches/sk-v12/HANDOFF.md:97`;
   `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:166`).
   G-Alpha's seed table repeats that W0 is docs-only revalidation with S-P3 drift
   routing (`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:118`).

8. SIMD/ASM preconditions fail closed. The user pin requires the
   `escape_mask_64` NEON correctness bug to be verified and resolved before any
   new SIMD admission (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:104`).
   The totality scout records the same falsifier and Lock 16 fold requirement
   (`restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md:71`,
   `:180`). Alpha-E E3 is correctness-only and blocks E5 or any other SIMD
   admission until scalar differential, corpus parity, and CHECKASM evidence pass
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:193`,
   `:212`, `:219`). Alpha-E E5 then requires scalar/checkasm, same-host
   microbench movement, same-wave CSS consumer, strict equality, JSON guard
   refresh/demotion when JSON-producing paths move, and zero orphan disposition
   (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:318`,
   `:327`).

## Regression Verdict

PASS. V4 does not reopen REDRESS 88/89/90, 96/97/98, 112/113, or 114-120
without the user-pin-required material differential, same-wave consumer, and
measured gate. JSON guard refresh, the local W1a rule, zero carried orphan,
REDRESS adjacency, rejected patch coverage, W0 revalidation, SIMD/ASM
preconditions, and the pin-aware G-Alpha presentation are aligned.
