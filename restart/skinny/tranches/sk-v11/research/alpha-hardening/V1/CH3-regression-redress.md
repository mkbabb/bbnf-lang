# SK-V11 Pass Alpha CH3 - Regression / REDRESS

Pass: Pass Alpha. Cycle: V1.
Lens: CH3 regression / REDRESS.
Date: 2026-05-19.
Scope: SK-V11 Pass Alpha V1 alpha-A..F, `SYNTHESIS.md`, `HANDOFF.md`,
and `skinny/REDRESS.md` through REDRESS 110.

## Disposition

ACCEPT-WITH-NITS.

No critical REDRESS-negative route is reopened by the Alpha V1 framing. The
packet correctly treats the SK-V9 W3 union event-model as retired, keeps
`parse_only` out of the SOTA close target, preserves the SK-V10 direct/typed
guard rows, and routes the remaining work through row-specific direct evidence,
fresh S-P1 profiling, same-host micro-proof, same-wave consumers, and
benchmarked non-JSON grammar evidence.

The nits are fold requirements before S-P3 wave scoping, not blockers for
G-Alpha:

1. Fold the W10b/PMULL/CSSC pre-block wording from Alpha-C into the main
   contract surfaces. Alpha-C explicitly preserves the PMULL/VPCLMUL
   prefix-XOR default and CSSC/CTZ bulk-emission default pre-blocks
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:112`-`116`),
   but Alpha-F's pre-block list does not name that adjacency
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:114`-`133`).
   Candidate C2 uses a CSSC first-set consumer and carries strong direct
   micro-proof, row-floor, and guard requirements
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:208`-`233`),
   so this is not a reopened CTZ bulk route. S-P3 should still copy the
   explicit REDRESS 88/89/W10b language into the SPEC pre-block list.
2. Rename or clarify Alpha-E's "measurement substrate" phrase for C1 so it is
   unambiguously gate/report infrastructure, not a new parser substrate
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:107`-`114`).
   C1 already says no behavior row moves and rejects if current JSON `A / GO`
   rows drop below carry-forward floors
   (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:150`-`159`),
   so the wording is only a CH3 precision nit.

## Regression Checks

### W3 / Parse-Plane Retirement

REDRESS 96 implemented the full class-column plus move-consumed structural
index, passed correctness checks, then failed every W3 must-improve row and
every W10b maintain row (`skinny/REDRESS.md:2797`-`2848`). REDRESS 97 removed
the allocation vector and still failed every W3/W10b row
(`skinny/REDRESS.md:2852`-`2906`). REDRESS 98 retires
`G-W3-UNION-SUBSTRATE`, states the two implementations falsified the union
substrate thesis, rejects the class-lane-only route as paper-close, and
abrogates W4's cascade-lock on W3 (`skinny/REDRESS.md:2910`-`2950`).

Alpha-C carries that result as a hard SK-V11 pre-block for retained class
column, `UnionTape`, structural index, streaming cursor, parser-owned
projection, class-lane-only fallback, W4 cascade-lock through W3, and renamed
equivalents (`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:73`-`83`).
`SYNTHESIS.md` independently blocks reopening the W3 substrate family and keeps
all 17 `parse_only` rows diagnostic, not SOTA admissions
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:49`-`54`). `HANDOFF.md` repeats
the same refusal conditions for dispatch
(`restart/skinny/tranches/sk-v11/HANDOFF.md:119`-`124`).

Verdict: ACCEPT. No candidate names W3 as a consumer, dependency, or route to
close parse-only; Alpha-E explicitly excludes W3, sidecar structural producers,
and parse-plane substrate-ceiling re-derivation
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:473`-`479`).

### W7 / W8 / W9 Lessons

REDRESS 106 rejects W7 at caller level: scalar/reference and strict checkasm
passed, but aggregate caller speedup was `0.774x` versus the required `1.08x`,
and no production caller or row change remained (`skinny/REDRESS.md:3152`-`3170`).
REDRESS 107 admits W8 only as a proof: the eligible escape caller microbench
cleared `1.268x`, but W8 moved no `RESULTS.md` row and wired no new production
behavior (`skinny/REDRESS.md:3174`-`3196`). REDRESS 108 rejects W9 because the
exact accepted W8 caller was already wired, no legitimate source delta existed,
and the targeted direct floors failed (`skinny/REDRESS.md:3200`-`3222`).

The Alpha packet preserves that distinction. Alpha-D states W8 may seed SK-V11
only through a fresh SPEC/CHALLENGE route with a real source delta, same-wave
consumer, and measured row gate
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-D-validated-invalidated.md:101`-`127`).
Candidate C4 forbids claiming the already-wired `unescape_string` path as
production integration and requires a generated direct or typed string consumer
that is not a cosmetic wrapper
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:356`-`369`).

Verdict: ACCEPT. The W7 full-string broad route and W9 "already wired"
production claim are not reopened.

### W10 / Direct Contract Lessons

REDRESS 100 makes direct row movement row-specific: a baseline
`N-direct / NO-GO` direct row may move only with digest output plane, strict
row semantics, measured-row validation, independent Track 2, REDRESS
provenance, non-gate-only consumer, and same-run native direct comparator
evidence (`skinny/REDRESS.md:2980`-`3001`). REDRESS 109 then moves exactly
`instruments/direct_to_struct`, with no parser runtime, generated direct
caller, SIMD primitive, generic crate, typed product row, or W3-adjacent
substrate change; both tracks clear the fixed W10 floor, and eleven direct
residual rows remain (`skinny/REDRESS.md:3226`-`3255`).

The SK-V11 contract follows that precedent. `SYNTHESIS.md` names the 11
remaining direct residual rows, their current Track 1/Track 2/sonic direct
numbers, seeded floors, and per-row closure/fixpoint rule
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:96`-`118`). Alpha-E repeats that
direct admission requires both generated Track 1 and independent Track
2/oracle to clear `ceil(sonic_direct / 1.10)` under one same-run strict direct
comparator
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:76`-`82`).

Verdict: ACCEPT. The W10 direct movement pattern is preserved; no direct row is
admitted by analogy.

### Guard-Row Preservation

`SYNTHESIS.md` requires the 7 typed `A / GO` rows and 6 direct `A / GO` rows
to remain admitted unless a same-wave gate records a measured REDRESS
disposition (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:43`-`48`). It lists
both guard surfaces and says existing admissions cannot be silently dropped
(`restart/skinny/tranches/sk-v11/SYNTHESIS.md:120`-`146`). Alpha-E repeats the
carry-forward guard rows and forbids using typed rows as direct proof or direct
digest rows as typed proof
(`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:98`-`105`).

Verdict: ACCEPT. Existing admitted rows are protected by maintain-floor and
measured-demotion discipline.

## Route Reopen Matrix

| Route family | CH3 result | Evidence |
|---|---|---|
| W3 union/event/class-column/streaming cursor/class-lane/substrate | Not reopened | REDRESS 98 retires the gate (`skinny/REDRESS.md:2910`-`2950`); Alpha-C and SYNTHESIS carry the pre-block (`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:73`-`83`, `restart/skinny/tranches/sk-v11/SYNTHESIS.md:52`-`54`). |
| Parse-only SOTA close | Not reopened | SK-V10 close leaves 17 `parse_only S / NO-GO` rows (`skinny/REDRESS.md:3268`-`3274`); SYNTHESIS excludes parse-only from SK-V11 close (`restart/skinny/tranches/sk-v11/SYNTHESIS.md:49`-`51`). |
| W7 broad full-string primitive | Not reopened | C2/C4/C5 require narrower caller micro-proof and row gates; C4 explicitly carries REDRESS 106/107/108 pre-blocks (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:397`-`400`). |
| W8 proof-only escape primitive | Not reopened as production | C4 requires a real generated direct/typed consumer and rejects cosmetic `unescape_string` reuse (`restart/skinny/tranches/sk-v11/research/alpha/alpha-E-candidate-shortlist.md:366`-`369`). |
| W9 already-wired production claim | Not reopened | Alpha-F blocks W9 "already wired" production claims (`restart/skinny/tranches/sk-v11/research/alpha/alpha-F-contract-draft.md:125`-`128`). |
| W10 direct row reclamation | Preserved | Remaining direct work uses same-run strict direct comparator, Track 1, independent Track 2, row floors, provenance, and gate consumption (`restart/skinny/tranches/sk-v11/HANDOFF.md:127`-`131`). |
| PMULL / CSSC CTZ default hot paths | Needs fold nit | Alpha-C preserves the pre-block (`restart/skinny/tranches/sk-v11/research/alpha/alpha-C-redress-digest.md:112`-`116`); Alpha-F/SYNTHESIS should carry it explicitly before S-P3. |

## CH3 Close

The Alpha V1 contract is regression-honest. It advances SK-V11 around the
banked direct/typed product surface, not through parse-plane repair; it treats
W3 and W7/W9 as negative authority; and it requires fresh measurement before
any source wave. The two nits above should be folded into the consolidated
hardening response or the first S-P3 SPEC draft.
