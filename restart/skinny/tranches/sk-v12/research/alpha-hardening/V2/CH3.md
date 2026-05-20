# SK-V12 Pass Alpha Hardening V2 - CH3 Regression / REDRESS

Date: 2026-05-20.
Lens: CH3 regression / REDRESS.
Verdict: PASS.

## Scope

Reviewed the USER PIN, Pass Alpha contract, V1 consolidated hardening,
V2-folded `SYNTHESIS.md`, `HANDOFF.md`, alpha-A/B/E/F, the REDRESS tail through
120, `skinny/RESULTS.md`, `skv12-aarch64-simd-coverage-audit.md`, and
`skv12-totality-fold-scout.md`.

No REVISE finding remains for the requested regression discipline checks. V2
preserves the pinned CSS-first route, keeps JSON direct residuals as guard /
reopen ledger state, requires zero carried SIMD orphans for both ADMIT and
FIXPOINT, binds rejected routes to REDRESS material-differential rules, and
keeps SIMD/ASM admission behind correctness, scalar/checkasm, and same-wave
consumer gates.

## File/Line Findings

### JSON guard refresh rules - PASS

- The user pin makes JSON guard floors second priority after CSS L4, and says
  demotion requires a measured gate disposition
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:84-87`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:94-95`).
- V1 explicitly required the V2 shortcut to be tightened whenever generic
  runtime, codegen, generated output, bench, report, or gate roots move
  (`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:59-61`).
- V2 folds that rule in the opening contract: a JSON-producing path move must
  refresh JSON guards or record measured REDRESS demotion; the no-refresh
  shortcut is legal only if no JSON-producing path moved and `RESULTS.md` is
  proven unchanged (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:144-148`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:58-62`).
- Alpha-E applies the same rule to row-moving CSS and SIMD waves
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:123-126`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:330-332`).

### Zero carried orphan for ADMIT / FIXPOINT - PASS

- The user pin identifies the five aarch64 orphans and sets zero orphan kernels
  as the SK-V12 close target
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78`).
- V1 required the zero-carried-orphan disposition to be added to both ADMIT and
  FIXPOINT
  (`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:56-58`).
- V2 ADMIT requires the named orphan set to be zero by admission, removal, or
  evidence-backed inventory demotion
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-65`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:81-85`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:89-98`).
- V2 FIXPOINT separately invalidates close with orphan production primitives
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:87-89`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:91-95`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:116-118`).
- Alpha-E's SIMD/ASM candidate requires the carried orphan set to be zero or
  explicitly demoted with evidence, and a row miss must leave no orphan native
  body
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:327-339`).

### REDRESS adjacency - PASS

- REDRESS 119/120 keep JSON direct residual rows as a measured fixpoint and
  route SK-V12 to generated non-JSON first
  (`skinny/REDRESS.md:3495-3527`, `skinny/REDRESS.md:3531-3553`).
- The user pin rescinds only category-level blocks for union and ASM-gen; the
  specific REDRESS 88/89/90 and 96/97/98 implementations remain historical
  measured evidence requiring citation, material differential, and CHALLENGE
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:50-56`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:66-69`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:110-120`).
- Alpha-C carries the exact adjacency rules: union attempts must not replay the
  class-column / streaming-cursor / sidecar shapes without material
  differential, ASM-gen must not replay PMULL-default or CTZ-bulk consumers
  without proof, and REDRESS 112/113 are superseded only for the explicit CSS L4
  mandate
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:71-76`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:104-107`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md:120-130`).
- V2 pre-blocks parse-only admission, Sheets/BBNF-self before CSS redress, stale
  CSS thresholds, public sidecar/substrate expansion, and only unblocks union /
  ASM-gen with REDRESS citation, material differential, CHALLENGE, proof, and
  same-wave consumer
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:206-226`).
- Alpha-E's E4 and E5 candidates name their REDRESS material differentials
  against 96/97/98 and 88/89/90 respectively
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:230-240`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:287-293`).

### Rejected patch coverage - PASS

- Prior SK-V11 rejected or blocked waves recorded patch artifacts or empty
  markers where appropriate, with no source or row movement left behind
  (`skinny/REDRESS.md:3325-3327`, `skinny/REDRESS.md:3363-3365`,
  `skinny/REDRESS.md:3390-3392`, `skinny/REDRESS.md:3424-3428`,
  `skinny/REDRESS.md:3451-3455`, `skinny/REDRESS.md:3484-3489`).
- V1 required uniform rollback and rejected-patch artifacts for E1-E5
  (`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:78-80`).
- V2's G-Alpha seed lists rejected-patch paths for W1a, W1b, W2, W3, and W4;
  W0 and W5 are docs-only / close-feedback paths with non-patch failure actions
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:265-271`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:127-137`).
- Alpha-E carries per-candidate revert rules and patch artifact paths for E1-E5
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:133-136`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:182-185`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:221-223`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:276-278`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:335-339`).

### W0 revalidation - PASS

- The carried opening JSON surface is unchanged: `skinny/RESULTS.md` records the
  overall `N-direct / NoGo` result and Track 2 independence
  (`skinny/RESULTS.md:143-146`), while REDRESS 120 verifies no `RESULTS.md` row
  movement at SK-V11 close (`skinny/REDRESS.md:3531-3538`).
- Alpha-A records zero row movement from SK-V11 close into SK-V12 and cites the
  REDRESS 120 close surface
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md:89-95`).
- V2 treats W0 telemetry/gate lock at `f788eb97` as revalidated, not redone, and
  sends drift back to S-P3 rather than allowing Alpha-F to rewrite W0
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:228-233`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:97-101`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:166-167`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:220`).

### SIMD / ASM preconditions - PASS

- The user pin unblocks ASM-gen at category level but preserves the scalar
  reference, parity/checkasm, micro-proof, and same-wave consumer requirements
  (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58-69`,
  `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:98-106`).
- The SIMD coverage audit identifies five orphans, non-compliant/proof-only
  primitive states, and the same-wave-consumer requirement
  (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34-61`,
  `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:117-136`,
  `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:193-199`).
- The totality fold scout records the Lock 16 checkasm admission gate and the
  `escape_mask_64` correctness bug as a correctness blocker
  (`restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md:66-72`,
  `restart/skinny/tranches/sk-v12/research/skv12-totality-fold-scout.md:180-199`).
- V2 requires the `escape_mask_64` issue to be verified and resolved before any
  SIMD admission, and makes missing lightningcss evidence, parse-only admission,
  or orphan SIMD primitives fail closed
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57-60`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:199-204`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:121-125`,
  `restart/skinny/tranches/sk-v12/HANDOFF.md:149-163`).
- Alpha-E sequences E3 before E5 and makes E5 a real CSS consumer only, not a
  checkasm-only or dispatch-table-only orphan
  (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:187-223`,
  `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:280-339`).

## Close

PASS. The V2-folded packet satisfies CH3 regression / REDRESS discipline under
the USER PIN. It does not reopen JSON direct residuals, parse-only SOTA
admission, REDRESS 111 report-only close, REDRESS 112/113 future-phase close, or
the specific REDRESS 88/89/90 and 96/97/98 rejected implementations without the
new material-differential gates required by the pin.
